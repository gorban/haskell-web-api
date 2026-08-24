{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | Private request decoding and response interpretation for typed API
-- endpoints.  This module owns the effectful request-body boundary; route
-- family matching remains in 'HarchWeb.Api.Endpoint.Family'.
module HarchWeb.Api.Endpoint.Runtime
  ( runApiRouteEndpoint,
    runApiRouteEndpointHandler,
    runApiRouteEndpointHandlerNeverFailing,
    ApiHttpResponse (..),
    apiHttpResponseToProtocolResponse,
    apiResponseBodyToProtocolResponse,
  )
where

import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.CaseInsensitive qualified as CaseInsensitive
import Data.IORef qualified as IORef
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Type.Equality ((:~:) (Refl))
import Data.Typeable (Typeable, eqT)
import HarchWeb.Api.Endpoint.Internal
import HarchWeb.Api.HeaderName (apiHeaderNameLiteral)
import HarchWeb.Api.MediaType (ApiContentType, apiContentTypeText)
import HarchWeb.Api.Multipart (MultipartLimits, MultipartStorage, withMultipartRequestBodyWithStorage)
import HarchWeb.Api.Negotiation
import HarchWeb.Api.Request
import HarchWeb.Api.Response
import HarchWeb.Server.RequestBody
import HarchWeb.Server.Response
  ( ProtocolResponse (..),
    ProtocolResponseBody (..),
  )
import Network.HTTP.Types qualified as HttpTypes
import Network.Wai qualified as Wai

runApiRouteEndpoint :: ApiRouteEndpoint fields body domainFailure response -> Wai.Request -> IO ProtocolResponse
runApiRouteEndpoint endpoint =
  case endpoint of
    ApiRouteEndpoint _ _ fields body encoders fieldFailure handler failureResponse ->
      runApiRouteEndpointHandler fields body encoders fieldFailure handler failureResponse
    ApiRouteEndpointNeverFailing _ _ fields body encoders fieldFailure handler ->
      runApiRouteEndpointHandlerNeverFailing fields body encoders fieldFailure handler

-- | Decode one declared body and its fields before passing them to the
-- response continuation. Protocol parse failures are interpreted exactly at
-- this transport boundary rather than forwarded through endpoint handlers.
runDecodedApiRequest ::
  (Typeable response) =>
  ApiRequestData ->
  RequestCodec fields ->
  ApiRequestBody body ->
  NonEmpty (ApiResponseEncoder response) ->
  Maybe ([ApiRequestParseError] -> ApiResponse response) ->
  (fields -> body -> IO ProtocolResponse) ->
  Wai.Request ->
  IO ProtocolResponse
runDecodedApiRequest requestData fields body encoders fieldFailure onDecoded request =
  decodeBody body
  where
    decodeBody requestBody =
      case requestBody of
        ApiNoRequestBody -> decodeInitialFields (`onDecoded` ())
        ApiBufferedRequestBody missingContentTypePolicy maximumBytes decoders -> do
          decodeInitialFields (decodeBufferedBody missingContentTypePolicy maximumBytes decoders . onDecoded)
        ApiUrlEncodedFormRequestBody missingContentTypePolicy maximumBytes maximumFields ->
          decodeBufferedBody
            missingContentTypePolicy
            maximumBytes
            [urlEncodedFormBodyDecoder maximumFields]
            (\decodedForm -> decodeFormFields decodedForm (apiRequestDataWithForm decodedForm requestData))
        ApiStreamingRequestBody maximumBytes -> decodeInitialFieldsWithBody (newApiStreamingRequest maximumBytes request)
        ApiMultipartRequestBody storage limits -> decodeInitialFieldsWithBody (newApiMultipartRequest storage limits request)

    decodeInitialFieldsWithBody newBody =
      decodeInitialFields $ \decodedFields -> do
        bodyValue <- newBody
        onDecoded decodedFields bodyValue

    decodeInitialFields = decodeRequestFields requestData

    decodeFormFields decodedForm fieldsData =
      decodeRequestFields fieldsData (`onDecoded` decodedForm)

    decodeRequestFields fieldsData onDecodedFields =
      case fieldFailure of
        Nothing ->
          case runRequestCodec fields fieldsData of
            ApiRequestDecoded decodedFields -> onDecodedFields decodedFields
            _ -> pure (apiFailureProtocolResponse encoders HttpTypes.status400 "API request fields were rejected.")
        Just responseFor ->
          case runRequestCodec fields fieldsData of
            ApiRequestDecoded decodedFields -> onDecodedFields decodedFields
            ApiRequestRejected parseErrors -> pure (fieldFailureResponse responseFor (NonEmpty.toList parseErrors))
            ApiRequestCodecInvalid -> pure (apiFailureProtocolResponse encoders HttpTypes.status400 "API request fields were rejected.")

    decodeBufferedBody missingContentTypePolicy maximumBytes decoders onDecodedBody = do
      bodyResult <- readRequestBodyUpTo maximumBytes request
      case bodyResult of
        Left RequestBodyLimitExceeded -> pure (apiFailureProtocolResponse encoders HttpTypes.status413 "API request body exceeds its declared limit.")
        Right lazyBody ->
          case selectApiBodyDecoder missingContentTypePolicy decoders (contentType requestData) (LazyByteString.toStrict lazyBody) of
            ApiUnsupportedMediaType _ -> pure (apiFailureProtocolResponse encoders HttpTypes.status415 "API request body has an unsupported media type.")
            ApiMalformedBody -> pure (apiFailureProtocolResponse encoders HttpTypes.status400 "API request body is malformed.")
            ApiDecodedBody decodedBody -> onDecodedBody decodedBody

    fieldFailureResponse responseFor parseErrors =
      renderEndpointResult
        encoders
        requestData
        ((responseFor parseErrors) {apiEndpointResponseStatus = HttpTypes.status400})

-- | Takes exactly the pieces this needs rather than a full
-- 'ApiRouteEndpoint', so a context-aware route definition has no unused
-- path or method to construct.
runApiRouteEndpointHandler ::
  (Typeable response) =>
  RequestCodec fields ->
  ApiRequestBody body ->
  NonEmpty (ApiResponseEncoder response) ->
  Maybe ([ApiRequestParseError] -> ApiResponse response) ->
  (ApiEndpointRequest fields body -> IO (Either domainFailure (ApiResponse response))) ->
  (domainFailure -> ApiResponse response) ->
  Wai.Request ->
  IO ProtocolResponse
runApiRouteEndpointHandler fields body encoders fieldFailure handler failureResponse request =
  runDecodedApiRequest requestData fields body encoders fieldFailure runHandler request
  where
    requestData = apiRequestDataFromWaiRequest request

    runHandler decodedFields decodedBody = do
      handlerResult <- handler (ApiEndpointRequest decodedFields decodedBody)
      pure (renderEndpointResult encoders requestData (either failureResponse id handlerResult))

-- | The total-handler variant has no domain-failure interpreter and therefore
-- no impossible error branch for callers to construct.
runApiRouteEndpointHandlerNeverFailing ::
  (Typeable response) =>
  RequestCodec fields ->
  ApiRequestBody body ->
  NonEmpty (ApiResponseEncoder response) ->
  Maybe ([ApiRequestParseError] -> ApiResponse response) ->
  (ApiEndpointRequest fields body -> IO (ApiResponse response)) ->
  Wai.Request ->
  IO ProtocolResponse
runApiRouteEndpointHandlerNeverFailing fields body encoders fieldFailure handler request =
  runDecodedApiRequest requestData fields body encoders fieldFailure runHandler request
  where
    requestData = apiRequestDataFromWaiRequest request

    runHandler decodedFields decodedBody = do
      responseValue <- handler (ApiEndpointRequest decodedFields decodedBody)
      pure (renderEndpointResult encoders requestData responseValue)

newApiStreamingRequest :: Int -> Wai.Request -> IO ApiStreamingRequest
newApiStreamingRequest maximumBytes request =
  ApiStreamingRequest <$> newRequestBodyChunkReader maximumBytes request

newApiMultipartRequest :: MultipartStorage stored -> MultipartLimits -> Wai.Request -> IO (ApiMultipartRequest stored)
newApiMultipartRequest storage limits request = do
  consumedReference <- IORef.newIORef False
  pure
    ApiMultipartRequest
      { consumeApiMultipartRequest = \onPart -> do
          alreadyConsumed <- IORef.atomicModifyIORef' consumedReference (True,)
          if alreadyConsumed
            then pure (Left ApiMultipartRequestAlreadyConsumed)
            else do
              result <- withMultipartRequestBodyWithStorage storage limits request onPart
              pure $
                case result of
                  Left consumeError -> Left (ApiMultipartRequestFailed consumeError)
                  Right () -> Right ()
      }

contentType :: ApiRequestData -> Maybe Text
contentType = singleHeaderValue (apiHeaderNameLiteral "content-type")

acceptHeader :: ApiRequestData -> Maybe Text
acceptHeader = singleHeaderValue (apiHeaderNameLiteral "accept")

-- | RFC 9110 section 5.3 requires repeated comma-list header lines to be
-- combined before their value is interpreted.
singleHeaderValue :: ApiHeaderName -> ApiRequestData -> Maybe Text
singleHeaderValue wanted requestData =
  case [value | (name, value) <- apiRequestHeaders requestData, name == wanted] of
    [] -> Nothing
    values -> Just (Text.intercalate ", " values)

renderEndpointResult :: (Typeable response) => NonEmpty (ApiResponseEncoder response) -> ApiRequestData -> ApiResponse response -> ProtocolResponse
renderEndpointResult encoders requestData responseValue =
  case selectContentTypeRepresentation declaredContentTypes (acceptHeader requestData) of
    NoAcceptableContentTypeRepresentation -> apiFailureProtocolResponse encoders HttpTypes.status406 "API response has no acceptable representation."
    SelectedContentTypeRepresentation selectedContentType ->
      ProtocolResponse
        { protocolResponseStatus = apiEndpointResponseStatus responseValue,
          protocolResponseHeaders = endpointProtocolResponseHeaders responseValue selectedEncoder,
          protocolResponseBody = protocolResponseBodyFor (apiResponseEncoderEncode selectedEncoder (apiEndpointResponseValue responseValue)),
          protocolResponseObservabilityAttributes = apiEndpointResponseObservabilityAttributes responseValue,
          protocolResponseLogEntries = apiEndpointResponseLogEntries responseValue,
          protocolResponseDatabaseOperations = apiEndpointResponseDatabaseOperations responseValue
        }
      where
        selectedEncoder = responseEncoderFor selectedContentType encoders
  where
    declaredContentTypes = apiResponseEncoderContentType <$> encoders

responseEncoderFor :: ApiContentType -> NonEmpty (ApiResponseEncoder response) -> ApiResponseEncoder response
responseEncoderFor selectedContentType encoders =
  foldr preferEncoder (NonEmpty.head encoders) (NonEmpty.tail encoders)
  where
    preferEncoder candidate fallback
      | apiResponseEncoderContentType candidate == selectedContentType = candidate
      | otherwise = fallback

endpointResponseHeaders :: ApiResponse response -> [(ApiHeaderName, ApiHeaderValue)]
endpointResponseHeaders responseValue = addVaryAccept (apiEndpointResponseHeaders responseValue)

endpointProtocolResponseHeaders :: ApiResponse response -> ApiResponseEncoder response -> HttpTypes.ResponseHeaders
endpointProtocolResponseHeaders responseValue encoder =
  ("Content-Type", TextEncoding.encodeUtf8 (apiContentTypeText (apiResponseEncoderContentType encoder)))
    : [ (CaseInsensitive.mk (TextEncoding.encodeUtf8 (apiHeaderNameText name)), TextEncoding.encodeUtf8 (apiHeaderValueText value))
      | (name, value) <- endpointResponseHeaders responseValue
      ]

protocolResponseBodyFor :: ApiEncodedResponseBody -> ProtocolResponseBody
protocolResponseBodyFor encodedBody =
  case encodedBody of
    ApiEncodedResponseBytes bytes -> ProtocolResponseBytes bytes
    ApiEncodedResponseStream stream -> ProtocolResponseStream stream

addVaryAccept :: [(ApiHeaderName, ApiHeaderValue)] -> [(ApiHeaderName, ApiHeaderValue)]
addVaryAccept headers =
  case break ((== "vary") . apiHeaderNameText . fst) headers of
    (_, []) -> headers <> [(apiHeaderNameLiteral "Vary", apiHeaderValueLiteral "Accept")]
    (beforeVary, (name, value) : afterVary) -> beforeVary <> [(name, varyValueWithAccept value)] <> afterVary

varyValueWithAccept :: ApiHeaderValue -> ApiHeaderValue
varyValueWithAccept headerValue
  | any ((== "accept") . Text.toCaseFold . Text.strip) (Text.splitOn "," value) = headerValue
  | otherwise = apiHeaderValueAppendLiteral headerValue ", Accept"
  where
    value = apiHeaderValueText headerValue

-- | Renders a transport-boundary failure sentence through the endpoint's own
-- first declared representation when its response type can carry one
-- (checked with 'eqT', since 'apiResponseEncoderEncode' only accepts a
-- 'Text' value when @response@ genuinely is 'Text'), so a JSON-only endpoint
-- never answers 400/413/415/406 with an un-negotiated @text/plain@ body.
-- When @response@ is not 'Text', there is no representation this sentence
-- can honestly be rendered as, so the response carries no body and no
-- 'Content-Type' claim rather than a mismatched one.
apiFailureProtocolResponse :: forall response. (Typeable response) => NonEmpty (ApiResponseEncoder response) -> HttpTypes.Status -> Text -> ProtocolResponse
apiFailureProtocolResponse encoders status bodyText =
  case eqT @response @Text of
    Just Refl -> textFailureProtocolResponse (NonEmpty.head encoders) status bodyText
    Nothing -> emptyFailureProtocolResponse status

textFailureProtocolResponse :: ApiResponseEncoder Text -> HttpTypes.Status -> Text -> ProtocolResponse
textFailureProtocolResponse encoder status bodyText =
  ProtocolResponse
    { protocolResponseStatus = status,
      protocolResponseHeaders = [("Content-Type", TextEncoding.encodeUtf8 (apiContentTypeText (apiResponseEncoderContentType encoder)))],
      protocolResponseBody = protocolResponseBodyFor (apiResponseEncoderEncode encoder bodyText),
      protocolResponseObservabilityAttributes = [],
      protocolResponseLogEntries = [],
      protocolResponseDatabaseOperations = []
    }

emptyFailureProtocolResponse :: HttpTypes.Status -> ProtocolResponse
emptyFailureProtocolResponse status =
  ProtocolResponse
    { protocolResponseStatus = status,
      protocolResponseHeaders = [],
      protocolResponseBody = ProtocolResponseBytes ByteString.empty,
      protocolResponseObservabilityAttributes = [],
      protocolResponseLogEntries = [],
      protocolResponseDatabaseOperations = []
    }

data ApiHttpResponse = ApiHttpResponse
  { apiHttpResponseStatus :: HttpTypes.Status,
    apiHttpResponseHeaders :: [(Text, ApiHeaderValue)],
    apiHttpResponseBody :: Maybe ApiResponseBody
  }
  deriving (Eq, Show)

apiHttpResponseToProtocolResponse :: ApiHttpResponse -> ProtocolResponse
apiHttpResponseToProtocolResponse httpResponse =
  ProtocolResponse
    { protocolResponseStatus = apiHttpResponseStatus httpResponse,
      protocolResponseHeaders = [(CaseInsensitive.mk (TextEncoding.encodeUtf8 name), TextEncoding.encodeUtf8 (apiHeaderValueText value)) | (name, value) <- apiHttpResponseHeaders httpResponse],
      protocolResponseBody = ProtocolResponseBytes (maybe ByteString.empty apiResponseBodyBytes (apiHttpResponseBody httpResponse)),
      protocolResponseObservabilityAttributes = [],
      protocolResponseLogEntries = [],
      protocolResponseDatabaseOperations = []
    }

-- | Render one API response body through the shared protocol response
-- boundary, retaining the selected status and representation Content-Type.
apiResponseBodyToProtocolResponse :: ApiResponseBody -> ProtocolResponse
apiResponseBodyToProtocolResponse body =
  ProtocolResponse
    { protocolResponseStatus = apiResponseStatus body,
      protocolResponseHeaders =
        ("Content-Type", TextEncoding.encodeUtf8 (apiContentTypeText (apiResponseContentType body)))
          : [ (CaseInsensitive.mk (TextEncoding.encodeUtf8 name), TextEncoding.encodeUtf8 (apiHeaderValueText value))
            | (name, value) <- apiResponseHeaders body
            ],
      protocolResponseBody = ProtocolResponseBytes (apiResponseBodyBytes body),
      protocolResponseObservabilityAttributes = [],
      protocolResponseLogEntries = [],
      protocolResponseDatabaseOperations = []
    }
