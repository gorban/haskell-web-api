{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Method-aware endpoint matching and WAI dispatch.
module HarchWeb.Api.Endpoint
  ( ApiMethod (..),
    ApiPath,
    ApiEndpoint,
    ApiRouteEndpoint,
    ApiEndpointRequest (..),
    ApiRequestBody (..),
    ApiMultipartRequest,
    ApiMultipartRequestError (..),
    withApiMultipartRequest,
    ApiMatchResult (..),
    apiMethodText,
    apiEndpoint,
    apiRouteEndpoint,
    apiRouteDefinition,
    apiEndpointTarget,
    at,
    matchApiEndpoints,
    apiAllowHeaderValue,
    ApiHttpResponse (..),
    respondApiMatch,
    apiHttpResponseToProtocolResponse,
    apiResponseBodyToProtocolResponse,
    apiHttpResponseToWaiResponse,
    apiEndpointMiddleware,
  )
where

import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.CaseInsensitive qualified as CaseInsensitive
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.Encoding.Error qualified as TextEncodingError
import Data.IORef qualified as IORef
import HarchWeb qualified
import HarchWeb.Api.MediaType (ApiContentType, apiContentTypeText)
import HarchWeb.Api.Multipart
  ( MultipartConsumeError,
    MultipartLimits,
    MultipartScopedPart,
    MultipartStorage,
    withMultipartRequestBodyWithStorage,
  )
import HarchWeb.Api.Negotiation
import HarchWeb.Api.Request
import HarchWeb.Api.Response
import HarchWeb.Server.RequestBody
import HarchWeb.Server.Response
  ( ProtocolResponse (..),
    ProtocolResponseBody (..),
  )
import HarchWeb.Site (RouteDefinition (..))
import Network.HTTP.Types qualified as HttpTypes
import Network.Wai qualified as Wai
import Numeric.Natural (Natural)

-- | Methods an 'ApiEndpoint' can declare. @HEAD@ is synthesized from @GET@
-- and @OPTIONS@ from the declared method table for a matched path.
data ApiMethod
  = ApiGet
  | ApiPost
  | ApiPut
  | ApiPatch
  | ApiDelete
  deriving (Eq, Show)

apiMethodText :: ApiMethod -> Text
apiMethodText methodValue =
  case methodValue of
    ApiGet -> "GET"
    ApiPost -> "POST"
    ApiPut -> "PUT"
    ApiPatch -> "PATCH"
    ApiDelete -> "DELETE"

-- | A static, context-independent request path.
newtype ApiPath = ApiPath Text
  deriving (Eq, Show)

at :: Text -> ApiPath
at = ApiPath

data ApiEndpoint target = ApiEndpoint
  { apiEndpointTarget :: target,
    apiEndpointMethod :: ApiMethod,
    apiEndpointPath :: ApiPath
  }

apiEndpoint :: target -> ApiMethod -> ApiPath -> ApiEndpoint target
apiEndpoint = ApiEndpoint

-- | One typed endpoint declaration for use in the application's shared route
-- table. It owns field decoding, exactly one declared body consumer, domain
-- failure interpretation, and the response representation. Path matching and
-- method policy remain owned by 'HarchWeb.Routing.RouteCodec'.
data ApiRouteEndpoint fields body domainFailure response where
  ApiRouteEndpoint ::
    { apiRouteEndpointMethod :: ApiMethod,
      apiRouteEndpointFields :: RequestCodec fields,
      apiRouteEndpointBody :: ApiRequestBody body,
      apiRouteEndpointEncoders :: NonEmpty (ApiResponseEncoder response),
      apiRouteEndpointHandler :: ApiEndpointRequest fields body -> IO (Either domainFailure (ApiResponse response)),
      apiRouteEndpointFailureResponse :: domainFailure -> ApiResponse response
    } ->
    ApiRouteEndpoint fields body domainFailure response

-- | Cohesive decoded input supplied to an endpoint handler.
data ApiEndpointRequest fields body = ApiEndpointRequest
  { apiEndpointRequestFields :: fields,
    apiEndpointRequestBody :: body
  }

-- | A scoped, single-use multipart request consumer. Its callback is the
-- only place a completed file upload is visible, so an upload must be
-- promoted there to outlive the request. Calling it twice is rejected before
-- attempting a second WAI body read.
newtype ApiMultipartRequest stored = ApiMultipartRequest
  { consumeApiMultipartRequest ::
      (MultipartScopedPart stored -> IO (Either MultipartConsumeError ())) ->
      IO (Either ApiMultipartRequestError ())
  }

-- | Multipart consumption either has the parser's precise failure or was
-- already claimed by the endpoint handler.
data ApiMultipartRequestError
  = ApiMultipartRequestAlreadyConsumed
  | ApiMultipartRequestFailed MultipartConsumeError
  deriving (Eq, Show)

-- | Consume the endpoint's declared multipart body once. This keeps the
-- request-scoped storage lifecycle inside the existing multipart adapter;
-- callers map its typed result to their ordinary endpoint outcome.
withApiMultipartRequest ::
  ApiMultipartRequest stored ->
  (MultipartScopedPart stored -> IO (Either MultipartConsumeError ())) ->
  IO (Either ApiMultipartRequestError ())
withApiMultipartRequest = consumeApiMultipartRequest

-- | An endpoint either declares no body consumer, one bounded buffered
-- decoder, or a bounded URL-encoded form whose fields join the request codec.
-- A multipart body remains scoped: its callback owns each completed part, and
-- must deliberately promote any file that needs to outlive the request.
data ApiRequestBody body where
  ApiNoRequestBody :: ApiRequestBody ()
  ApiBufferedRequestBody ::
    { apiRequestBodyMissingContentTypePolicy :: MissingContentTypePolicy,
      apiRequestBodyMaximumBytes :: Int,
      apiRequestBodyDecoders :: [ApiBodyDecoder body]
    } ->
    ApiRequestBody body
  ApiUrlEncodedFormRequestBody ::
    MissingContentTypePolicy ->
    Int ->
    Natural ->
    ApiRequestBody ApiForm
  ApiMultipartRequestBody ::
    MultipartStorage stored ->
    MultipartLimits ->
    ApiRequestBody (ApiMultipartRequest stored)

apiRouteEndpoint ::
  ApiMethod ->
  RequestCodec fields ->
  ApiRequestBody body ->
  NonEmpty (ApiResponseEncoder response) ->
  (ApiEndpointRequest fields body -> IO (Either domainFailure (ApiResponse response))) ->
  (domainFailure -> ApiResponse response) ->
  ApiRouteEndpoint fields body domainFailure response
apiRouteEndpoint = ApiRouteEndpoint

-- | Convert a declaration into one entry in a 'RouteDefinition' table. The
-- server has already selected the route and method before this runs, so it
-- cannot produce a competing 404/405/HEAD/OPTIONS policy.
apiRouteDefinition :: ApiRouteEndpoint fields body domainFailure response -> RouteDefinition route context
apiRouteDefinition endpoint =
  RouteDefinition
    { routeNavigationLabel = Nothing,
      routeMethods = [toRouteMethod (apiRouteEndpointMethod endpoint)],
      routeResponse = \request _ -> runApiRouteEndpoint endpoint request
    }

runApiRouteEndpoint :: ApiRouteEndpoint fields body domainFailure response -> Wai.Request -> IO (HarchWeb.Response route context)
runApiRouteEndpoint endpoint request =
  decodeBody (apiRouteEndpointBody endpoint)
  where
    requestData = apiRequestDataFromWaiRequest request

    decodeBody requestBody =
      case requestBody of
        ApiNoRequestBody -> decodeInitialFields (`runHandler` ())
        ApiBufferedRequestBody missingContentTypePolicy maximumBytes decoders -> do
          decodeInitialFields (decodeBufferedBody missingContentTypePolicy maximumBytes decoders . runHandler)
        ApiUrlEncodedFormRequestBody missingContentTypePolicy maximumBytes maximumFields ->
          decodeBufferedBody
            missingContentTypePolicy
            maximumBytes
            [urlEncodedFormBodyDecoder maximumFields]
            (\decodedForm -> decodeFormFields decodedForm (apiRequestDataWithForm decodedForm requestData))
        ApiMultipartRequestBody storage limits ->
          decodeInitialFields $ \decodedFields -> do
            multipartRequest <- newApiMultipartRequest storage limits request
            runHandler decodedFields multipartRequest

    decodeInitialFields onDecodedFields =
      case runRequestCodec (apiRouteEndpointFields endpoint) requestData of
        ([], Just decodedFields) -> onDecodedFields decodedFields
        _ -> pure (apiFailureResponse HttpTypes.status400 "API request fields were rejected.")

    decodeFormFields decodedForm fieldsData =
      case runRequestCodec (apiRouteEndpointFields endpoint) fieldsData of
        ([], Just decodedFields) -> runHandler decodedFields decodedForm
        _ -> pure (apiFailureResponse HttpTypes.status400 "API request fields were rejected.")

    decodeBufferedBody missingContentTypePolicy maximumBytes decoders onDecodedBody = do
      bodyResult <- readRequestBodyUpTo maximumBytes request
      case bodyResult of
        Left RequestBodyLimitExceeded -> pure (apiFailureResponse HttpTypes.status413 "API request body exceeds its declared limit.")
        Right lazyBody ->
          case selectApiBodyDecoder missingContentTypePolicy decoders (contentType request) (LazyByteString.toStrict lazyBody) of
            ApiUnsupportedMediaType _ -> pure (apiFailureResponse HttpTypes.status415 "API request body has an unsupported media type.")
            ApiMalformedBody -> pure (apiFailureResponse HttpTypes.status400 "API request body is malformed.")
            ApiDecodedBody decodedBody -> onDecodedBody decodedBody

    runHandler decodedFields decodedBody = do
      handlerResult <- apiRouteEndpointHandler endpoint (ApiEndpointRequest decodedFields decodedBody)
      pure (renderEndpointResult endpoint request (either (apiRouteEndpointFailureResponse endpoint) id handlerResult))

newApiMultipartRequest :: MultipartStorage stored -> MultipartLimits -> Wai.Request -> IO (ApiMultipartRequest stored)
newApiMultipartRequest storage limits request = do
  consumedReference <- IORef.newIORef False
  pure
    ApiMultipartRequest
      { consumeApiMultipartRequest = \onPart -> do
          alreadyConsumed <- IORef.atomicModifyIORef' consumedReference (\consumed -> (True, consumed))
          if alreadyConsumed
            then pure (Left ApiMultipartRequestAlreadyConsumed)
            else do
              result <- withMultipartRequestBodyWithStorage storage limits request onPart
              pure $
                case result of
                  Left consumeError -> Left (ApiMultipartRequestFailed consumeError)
                  Right () -> Right ()
      }

contentType :: Wai.Request -> Maybe Text
contentType request =
  case [value | (name, value) <- apiRequestHeaders (apiRequestDataFromWaiRequest request), name == apiHeaderName "content-type"] of
    [value] -> Just value
    _ -> Nothing

acceptHeader :: Wai.Request -> Maybe Text
acceptHeader request =
  case [value | (name, value) <- apiRequestHeaders (apiRequestDataFromWaiRequest request), name == apiHeaderName "accept"] of
    [value] -> Just value
    _ -> Nothing

renderEndpointResult :: ApiRouteEndpoint fields body domainFailure response -> Wai.Request -> ApiResponse response -> HarchWeb.Response route context
renderEndpointResult endpoint request responseValue =
  case selectContentTypeRepresentation declaredContentTypes (acceptHeader request) of
    NoAcceptableContentTypeRepresentation -> apiFailureResponse HttpTypes.status406 "API response has no acceptable representation."
    SelectedContentTypeRepresentation selectedContentType ->
      HarchWeb.ProtocolResponseResult
        ProtocolResponse
          { protocolResponseStatus = apiEndpointResponseStatus responseValue,
            protocolResponseHeaders = endpointProtocolResponseHeaders endpoint responseValue selectedEncoder,
            protocolResponseBody = protocolResponseBodyFor (apiResponseEncoderEncode selectedEncoder (apiEndpointResponseValue responseValue)),
            protocolResponseObservabilityAttributes = [],
            protocolResponseLogEntries = []
          }
      where
        selectedEncoder = responseEncoderFor selectedContentType (apiRouteEndpointEncoders endpoint)
  where
    declaredContentTypes = apiResponseEncoderContentType <$> apiRouteEndpointEncoders endpoint

responseEncoderFor :: ApiContentType -> NonEmpty (ApiResponseEncoder response) -> ApiResponseEncoder response
responseEncoderFor selectedContentType encoders =
  foldr preferEncoder (NonEmpty.head encoders) (NonEmpty.tail encoders)
  where
    preferEncoder candidate fallback
      | apiResponseEncoderContentType candidate == selectedContentType = candidate
      | otherwise = fallback

endpointResponseHeaders :: ApiRouteEndpoint fields body domainFailure response -> ApiResponse response -> [(Text, Text)]
endpointResponseHeaders endpoint responseValue
  | length (apiRouteEndpointEncoders endpoint) > 1 = addVaryAccept (apiEndpointResponseHeaders responseValue)
  | otherwise = apiEndpointResponseHeaders responseValue

endpointProtocolResponseHeaders :: ApiRouteEndpoint fields body domainFailure response -> ApiResponse response -> ApiResponseEncoder response -> HttpTypes.ResponseHeaders
endpointProtocolResponseHeaders endpoint responseValue encoder =
  ("Content-Type", TextEncoding.encodeUtf8 (apiContentTypeText (apiResponseEncoderContentType encoder)))
    : [ (CaseInsensitive.mk (TextEncoding.encodeUtf8 name), TextEncoding.encodeUtf8 value)
      | (name, value) <- endpointResponseHeaders endpoint responseValue
      ]

protocolResponseBodyFor :: ApiEncodedResponseBody -> ProtocolResponseBody
protocolResponseBodyFor encodedBody =
  case encodedBody of
    ApiEncodedResponseBytes bytes -> ProtocolResponseBytes bytes
    ApiEncodedResponseStream stream -> ProtocolResponseStream stream

addVaryAccept :: [(Text, Text)] -> [(Text, Text)]
addVaryAccept headers =
  case break ((== "vary") . Text.toCaseFold . fst) headers of
    (_, []) -> headers <> [("Vary", "Accept")]
    (beforeVary, (name, value) : afterVary) -> beforeVary <> [(name, varyValueWithAccept value)] <> afterVary

varyValueWithAccept :: Text -> Text
varyValueWithAccept value
  | any ((== "accept") . Text.toCaseFold . Text.strip) (Text.splitOn "," value) = value
  | otherwise = value <> ", Accept"

apiFailureResponse :: HttpTypes.Status -> Text -> HarchWeb.Response route context
apiFailureResponse status bodyText =
  apiResponseBodyToResponse ((apiTextResponse bodyText) {apiResponseStatus = status})

apiResponseBodyToResponse :: ApiResponseBody -> HarchWeb.Response route context
apiResponseBodyToResponse = HarchWeb.ProtocolResponseResult . apiResponseBodyToProtocolResponse

toRouteMethod :: ApiMethod -> HarchWeb.RouteMethod
toRouteMethod apiMethod =
  case apiMethod of
    ApiGet -> HarchWeb.RouteGet
    ApiPost -> HarchWeb.RoutePost
    ApiPut -> HarchWeb.RoutePut
    ApiPatch -> HarchWeb.RoutePatch
    ApiDelete -> HarchWeb.RouteDelete

data ApiMatchResult target
  = NoApiRouteMatch
  | ApiMethodNotAllowed (NonEmpty ApiMethod)
  | ApiRouteMatched target
  | ApiRouteMatchedHead target
  | ApiRouteOptions (NonEmpty ApiMethod)
  deriving (Eq, Show)

matchApiEndpoints :: Text -> Text -> [ApiEndpoint target] -> ApiMatchResult target
matchApiEndpoints requestMethod requestPath endpoints =
  case filter (endpointAtPath requestPath) endpoints of
    [] -> NoApiRouteMatch
    firstPathMatch : remainingPathMatches ->
      matchMethod requestMethod (firstPathMatch :| remainingPathMatches)

matchMethod :: Text -> NonEmpty (ApiEndpoint target) -> ApiMatchResult target
matchMethod requestMethod pathMatches =
  case NonEmpty.filter (endpointHasMethod requestMethod) pathMatches of
    matched : _ -> ApiRouteMatched (apiEndpointTarget matched)
    [] ->
      if requestMethod == "HEAD"
        then case NonEmpty.filter (endpointHasMethod "GET") pathMatches of
          matchedGet : _ -> ApiRouteMatchedHead (apiEndpointTarget matchedGet)
          [] -> ApiMethodNotAllowed (declaredMethods pathMatches)
        else
          if requestMethod == "OPTIONS"
            then ApiRouteOptions (declaredMethods pathMatches)
            else ApiMethodNotAllowed (declaredMethods pathMatches)

endpointAtPath :: Text -> ApiEndpoint target -> Bool
endpointAtPath requestPath endpointValue =
  case apiEndpointPath endpointValue of
    ApiPath declaredPath -> declaredPath == requestPath

endpointHasMethod :: Text -> ApiEndpoint target -> Bool
endpointHasMethod requestMethod endpointValue =
  apiMethodText (apiEndpointMethod endpointValue) == requestMethod

declaredMethods :: NonEmpty (ApiEndpoint target) -> NonEmpty ApiMethod
declaredMethods (firstEndpoint :| remainingEndpoints) =
  firstMethod :| nub (filter (/= firstMethod) (map apiEndpointMethod remainingEndpoints))
  where
    firstMethod = apiEndpointMethod firstEndpoint

apiAllowHeaderValue :: NonEmpty ApiMethod -> Text
apiAllowHeaderValue declaredMethodsValue =
  Text.intercalate
    ", "
    ( map apiMethodText (NonEmpty.toList declaredMethodsValue)
        <> ["HEAD" | ApiGet `elem` declaredMethodsValue]
        <> ["OPTIONS"]
    )

data ApiHttpResponse = ApiHttpResponse
  { apiHttpResponseStatus :: HttpTypes.Status,
    apiHttpResponseHeaders :: [(Text, Text)],
    apiHttpResponseBody :: Maybe ApiResponseBody
  }
  deriving (Eq, Show)

respondApiMatch :: (target -> ApiResponseBody) -> ApiMatchResult target -> ApiHttpResponse
respondApiMatch renderTarget matchResult =
  case matchResult of
    NoApiRouteMatch -> ApiHttpResponse HttpTypes.status404 [] Nothing
    ApiMethodNotAllowed declaredMethodsValue ->
      ApiHttpResponse HttpTypes.status405 [("Allow", apiAllowHeaderValue declaredMethodsValue)] Nothing
    ApiRouteMatched target -> renderedApiResponse (renderTarget target)
    ApiRouteMatchedHead target -> (renderedApiResponse (renderTarget target)) {apiHttpResponseBody = Nothing}
    ApiRouteOptions declaredMethodsValue ->
      ApiHttpResponse HttpTypes.status204 [("Allow", apiAllowHeaderValue declaredMethodsValue)] Nothing

renderedApiResponse :: ApiResponseBody -> ApiHttpResponse
renderedApiResponse body =
  ApiHttpResponse
    { apiHttpResponseStatus = apiResponseStatus body,
      apiHttpResponseHeaders = ("Content-Type", apiContentTypeText (apiResponseContentType body)) : apiResponseHeaders body,
      apiHttpResponseBody = Just body
    }

apiHttpResponseToWaiResponse :: ApiHttpResponse -> Wai.Response
apiHttpResponseToWaiResponse httpResponse =
  Wai.responseLBS
    (apiHttpResponseStatus httpResponse)
    [(CaseInsensitive.mk (TextEncoding.encodeUtf8 name), TextEncoding.encodeUtf8 value) | (name, value) <- apiHttpResponseHeaders httpResponse]
    (maybe LazyByteString.empty (LazyByteString.fromStrict . apiResponseBodyBytes) (apiHttpResponseBody httpResponse))

-- | Convert the legacy API match result into the shared server response
-- primitive. A route-registry endpoint uses this conversion instead of the
-- compatibility WAI middleware, so framework response policy, diagnostics,
-- and observability still run once at the normal server boundary.
apiHttpResponseToProtocolResponse :: ApiHttpResponse -> ProtocolResponse
apiHttpResponseToProtocolResponse httpResponse =
  ProtocolResponse
    { protocolResponseStatus = apiHttpResponseStatus httpResponse,
      protocolResponseHeaders = [(CaseInsensitive.mk (TextEncoding.encodeUtf8 name), TextEncoding.encodeUtf8 value) | (name, value) <- apiHttpResponseHeaders httpResponse],
      protocolResponseBody = ProtocolResponseBytes (maybe ByteString.empty apiResponseBodyBytes (apiHttpResponseBody httpResponse)),
      protocolResponseObservabilityAttributes = [],
      protocolResponseLogEntries = []
    }

-- | Render one API response body through the shared protocol response
-- boundary, retaining the selected status and representation Content-Type.
apiResponseBodyToProtocolResponse :: ApiResponseBody -> ProtocolResponse
apiResponseBodyToProtocolResponse = apiHttpResponseToProtocolResponse . renderedApiResponse

-- | A WAI middleware an application opts into by wrapping its own application.
-- It owns only the paths it matches and leaves every other request unchanged.
apiEndpointMiddleware :: [ApiEndpoint target] -> (Wai.Request -> target -> IO ApiResponseBody) -> Wai.Middleware
apiEndpointMiddleware endpoints runTarget innerApplication request respond =
  case matchApiEndpoints requestMethodText requestPathText endpoints of
    NoApiRouteMatch -> innerApplication request respond
    ApiMethodNotAllowed declaredMethodsValue ->
      respond (apiHttpResponseToWaiResponse (ApiHttpResponse HttpTypes.status405 [("Allow", apiAllowHeaderValue declaredMethodsValue)] Nothing))
    ApiRouteMatched target -> do
      renderedResponse <- runRenderedTarget target
      respond (apiHttpResponseToWaiResponse renderedResponse)
    ApiRouteMatchedHead target -> do
      renderedResponse <- runRenderedTarget target
      respond (apiHttpResponseToWaiResponse (renderedResponse {apiHttpResponseBody = Nothing}))
    ApiRouteOptions declaredMethodsValue ->
      respond (apiHttpResponseToWaiResponse (ApiHttpResponse HttpTypes.status204 [("Allow", apiAllowHeaderValue declaredMethodsValue)] Nothing))
  where
    requestMethodText = TextEncoding.decodeUtf8With TextEncodingError.lenientDecode (Wai.requestMethod request)
    requestPathText = TextEncoding.decodeUtf8With TextEncodingError.lenientDecode (Wai.rawPathInfo request)
    runRenderedTarget target = renderedApiResponse <$> runTarget request target
