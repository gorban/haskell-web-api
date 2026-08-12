{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Method-aware endpoint matching and WAI dispatch.
module HarchWeb.Api.Endpoint
  ( ApiMethod (..),
    ApiPath,
    ApiRouteEndpoint,
    SomeApiRouteEndpoint (..),
    ApiEndpointRequest (..),
    ApiRequestBody (..),
    ApiMultipartRequest,
    ApiMultipartRequestError (..),
    withApiMultipartRequest,
    apiMethodText,
    apiRouteEndpoint,
    apiRouteEndpointAt,
    apiRouteEndpointPath,
    apiRouteDefinition,
    at,
    ApiEndpoint,
    ApiMatchResult (..),
    apiEndpoint,
    apiEndpointTarget,
    matchApiEndpoints,
    apiAllowHeaderValue,
    apiResponseBodyToProtocolResponse,
    apiRouteEndpointMiddleware,
    ApiHttpResponse (..),
    respondApiMatch,
    apiHttpResponseToProtocolResponse,
    apiHttpResponseToWaiResponse,
    apiEndpointMiddleware,
  )
where

import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.CaseInsensitive qualified as CaseInsensitive
import Data.IORef qualified as IORef
import Data.List (find, nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.Encoding.Error qualified as TextEncodingError
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

-- | Compatibility endpoint table for applications that still own dispatch
-- outside the typed endpoint boundary. New endpoint work should use
-- 'ApiRouteEndpoint' and 'apiRouteEndpointMiddleware'.
data ApiEndpoint target = ApiEndpoint
  { apiEndpointTarget :: target,
    apiEndpointMethod :: ApiMethod,
    apiEndpointPath :: ApiPath
  }

apiEndpoint :: target -> ApiMethod -> ApiPath -> ApiEndpoint target
apiEndpoint = ApiEndpoint

-- | One typed endpoint declaration for use in the application's shared route
-- table or an explicitly composed WAI application. It owns its path, method,
-- field decoding, exactly one declared body consumer, domain-failure
-- interpretation, and response representations.
data ApiRouteEndpoint fields body domainFailure response where
  ApiRouteEndpoint ::
    { apiRouteEndpointPath :: ApiPath,
      apiRouteEndpointMethod :: ApiMethod,
      apiRouteEndpointFields :: RequestCodec fields,
      apiRouteEndpointBody :: ApiRequestBody body,
      apiRouteEndpointEncoders :: NonEmpty (ApiResponseEncoder response),
      apiRouteEndpointHandler :: ApiEndpointRequest fields body -> IO (Either domainFailure (ApiResponse response)),
      apiRouteEndpointFailureResponse :: domainFailure -> ApiResponse response
    } ->
    ApiRouteEndpoint fields body domainFailure response

-- | An endpoint table may contain declarations with different request, body,
-- failure, and response types without exposing those existentials to a
-- handler. Each declaration remains fully typed at its definition site.
data SomeApiRouteEndpoint where
  SomeApiRouteEndpoint :: ApiRouteEndpoint fields body domainFailure response -> SomeApiRouteEndpoint

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
apiRouteEndpoint method = apiRouteEndpointAt method (at "")

-- | Construct a typed endpoint that owns a concrete path for direct WAI
-- composition. The route-table adapter can use 'apiRouteEndpoint' while the
-- application route codec remains the authoritative path owner.
apiRouteEndpointAt ::
  ApiMethod ->
  ApiPath ->
  RequestCodec fields ->
  ApiRequestBody body ->
  NonEmpty (ApiResponseEncoder response) ->
  (ApiEndpointRequest fields body -> IO (Either domainFailure (ApiResponse response))) ->
  (domainFailure -> ApiResponse response) ->
  ApiRouteEndpoint fields body domainFailure response
apiRouteEndpointAt method path = ApiRouteEndpoint path method

-- | Convert a declaration into one entry in a 'RouteDefinition' table. The
-- server has already selected the route and method before this runs, so it
-- cannot produce a competing 404/405/HEAD/OPTIONS policy.
apiRouteDefinition :: ApiRouteEndpoint fields body domainFailure response -> RouteDefinition route context
apiRouteDefinition endpoint =
  RouteDefinition
    { routeNavigationLabel = Nothing,
      routeMethods = [toRouteMethod (apiRouteEndpointMethod endpoint)],
      routeResponse = \request _ -> HarchWeb.ProtocolResponseResult <$> runApiRouteEndpoint endpoint request
    }

runApiRouteEndpoint :: ApiRouteEndpoint fields body domainFailure response -> Wai.Request -> IO ProtocolResponse
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
        _ -> pure (apiFailureProtocolResponse HttpTypes.status400 "API request fields were rejected.")

    decodeFormFields decodedForm fieldsData =
      case runRequestCodec (apiRouteEndpointFields endpoint) fieldsData of
        ([], Just decodedFields) -> runHandler decodedFields decodedForm
        _ -> pure (apiFailureProtocolResponse HttpTypes.status400 "API request fields were rejected.")

    decodeBufferedBody missingContentTypePolicy maximumBytes decoders onDecodedBody = do
      bodyResult <- readRequestBodyUpTo maximumBytes request
      case bodyResult of
        Left RequestBodyLimitExceeded -> pure (apiFailureProtocolResponse HttpTypes.status413 "API request body exceeds its declared limit.")
        Right lazyBody ->
          case selectApiBodyDecoder missingContentTypePolicy decoders (contentType request) (LazyByteString.toStrict lazyBody) of
            ApiUnsupportedMediaType _ -> pure (apiFailureProtocolResponse HttpTypes.status415 "API request body has an unsupported media type.")
            ApiMalformedBody -> pure (apiFailureProtocolResponse HttpTypes.status400 "API request body is malformed.")
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

renderEndpointResult :: ApiRouteEndpoint fields body domainFailure response -> Wai.Request -> ApiResponse response -> ProtocolResponse
renderEndpointResult endpoint request responseValue =
  case selectContentTypeRepresentation declaredContentTypes (acceptHeader request) of
    NoAcceptableContentTypeRepresentation -> apiFailureProtocolResponse HttpTypes.status406 "API response has no acceptable representation."
    SelectedContentTypeRepresentation selectedContentType ->
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

apiFailureProtocolResponse :: HttpTypes.Status -> Text -> ProtocolResponse
apiFailureProtocolResponse status bodyText =
  apiResponseBodyToProtocolResponse ((apiTextResponse bodyText) {apiResponseStatus = status})

toRouteMethod :: ApiMethod -> HarchWeb.RouteMethod
toRouteMethod apiMethod =
  case apiMethod of
    ApiGet -> HarchWeb.RouteGet
    ApiPost -> HarchWeb.RoutePost
    ApiPut -> HarchWeb.RoutePut
    ApiPatch -> HarchWeb.RoutePatch
    ApiDelete -> HarchWeb.RouteDelete

apiAllowHeaderValue :: NonEmpty ApiMethod -> Text
apiAllowHeaderValue declaredMethodsValue =
  Text.intercalate
    ", "
    ( map apiMethodText (NonEmpty.toList declaredMethodsValue)
        <> ["HEAD" | ApiGet `elem` declaredMethodsValue]
        <> ["OPTIONS"]
    )

data ApiMatchResult target
  = NoApiRouteMatch
  | ApiMethodNotAllowed (NonEmpty ApiMethod)
  | ApiRouteMatched target
  | ApiRouteMatchedHead target
  | ApiRouteOptions (NonEmpty ApiMethod)
  deriving (Eq, Show)

matchApiEndpoints :: Text -> Text -> [ApiEndpoint target] -> ApiMatchResult target
matchApiEndpoints requestMethod requestPath endpoints =
  case filter (legacyEndpointAtPath requestPath) endpoints of
    [] -> NoApiRouteMatch
    firstPathMatch : remainingPathMatches -> matchLegacyMethod requestMethod (firstPathMatch :| remainingPathMatches)

matchLegacyMethod :: Text -> NonEmpty (ApiEndpoint target) -> ApiMatchResult target
matchLegacyMethod requestMethod pathMatches =
  case NonEmpty.filter (legacyEndpointHasMethod requestMethod) pathMatches of
    matched : _ -> ApiRouteMatched (apiEndpointTarget matched)
    []
      | requestMethod == "HEAD" ->
          maybe (ApiMethodNotAllowed declared) (ApiRouteMatchedHead . apiEndpointTarget) (find (legacyEndpointHasMethod "GET") pathMatches)
      | requestMethod == "OPTIONS" -> ApiRouteOptions declared
      | otherwise -> ApiMethodNotAllowed declared
  where
    declared = legacyDeclaredMethods pathMatches

legacyEndpointAtPath :: Text -> ApiEndpoint target -> Bool
legacyEndpointAtPath requestPath endpointValue =
  case apiEndpointPath endpointValue of
    ApiPath declaredPath -> declaredPath == requestPath

legacyEndpointHasMethod :: Text -> ApiEndpoint target -> Bool
legacyEndpointHasMethod requestMethod endpointValue =
  apiMethodText (apiEndpointMethod endpointValue) == requestMethod

legacyDeclaredMethods :: NonEmpty (ApiEndpoint target) -> NonEmpty ApiMethod
legacyDeclaredMethods (firstEndpoint :| remainingEndpoints) =
  firstMethod :| nub (filter (/= firstMethod) (map apiEndpointMethod remainingEndpoints))
  where
    firstMethod = apiEndpointMethod firstEndpoint

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
    ApiMethodNotAllowed declaredMethodsValue -> ApiHttpResponse HttpTypes.status405 [("Allow", apiAllowHeaderValue declaredMethodsValue)] Nothing
    ApiRouteMatched target -> legacyRenderedApiResponse (renderTarget target)
    ApiRouteMatchedHead target -> (legacyRenderedApiResponse (renderTarget target)) {apiHttpResponseBody = Nothing}
    ApiRouteOptions declaredMethodsValue -> ApiHttpResponse HttpTypes.status204 [("Allow", apiAllowHeaderValue declaredMethodsValue)] Nothing

legacyRenderedApiResponse :: ApiResponseBody -> ApiHttpResponse
legacyRenderedApiResponse body =
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
apiResponseBodyToProtocolResponse body =
  ProtocolResponse
    { protocolResponseStatus = apiResponseStatus body,
      protocolResponseHeaders =
        ("Content-Type", TextEncoding.encodeUtf8 (apiContentTypeText (apiResponseContentType body)))
          : [ (CaseInsensitive.mk (TextEncoding.encodeUtf8 name), TextEncoding.encodeUtf8 value)
            | (name, value) <- apiResponseHeaders body
            ],
      protocolResponseBody = ProtocolResponseBytes (apiResponseBodyBytes body),
      protocolResponseObservabilityAttributes = [],
      protocolResponseLogEntries = []
    }

data ApiRouteMatch
  = TypedApiRouteNoMatch
  | TypedApiRouteMethodNotAllowed (NonEmpty ApiMethod)
  | TypedApiRouteMatched SomeApiRouteEndpoint
  | TypedApiRouteMatchedHead SomeApiRouteEndpoint
  | TypedApiRouteOptions (NonEmpty ApiMethod)

-- | Compose an endpoint table directly with a WAI application. Every matched
-- path executes the typed declaration that owns its request and response
-- contract; unrelated paths remain the responsibility of the inner app.
apiRouteEndpointMiddleware :: [SomeApiRouteEndpoint] -> Wai.Middleware
apiRouteEndpointMiddleware endpoints innerApplication request respond =
  case matchApiRouteEndpoints requestMethodText requestPathText endpoints of
    TypedApiRouteNoMatch -> innerApplication request respond
    TypedApiRouteMethodNotAllowed declaredMethodsValue ->
      respond (apiHttpResponseToWaiResponse (ApiHttpResponse HttpTypes.status405 [("Allow", apiAllowHeaderValue declaredMethodsValue)] Nothing))
    TypedApiRouteMatched (SomeApiRouteEndpoint endpoint) ->
      runApiRouteEndpoint endpoint request >>= respond . protocolResponseToWaiResponse
    TypedApiRouteMatchedHead (SomeApiRouteEndpoint endpoint) -> do
      protocolResponse <- runApiRouteEndpoint endpoint request
      respond (protocolResponseToWaiResponse (protocolResponse {protocolResponseBody = ProtocolResponseBytes ByteString.empty}))
    TypedApiRouteOptions declaredMethodsValue ->
      respond (apiHttpResponseToWaiResponse (ApiHttpResponse HttpTypes.status204 [("Allow", apiAllowHeaderValue declaredMethodsValue)] Nothing))
  where
    requestMethodText = TextEncoding.decodeUtf8With TextEncodingError.lenientDecode (Wai.requestMethod request)
    requestPathText = TextEncoding.decodeUtf8With TextEncodingError.lenientDecode (Wai.rawPathInfo request)

matchApiRouteEndpoints :: Text -> Text -> [SomeApiRouteEndpoint] -> ApiRouteMatch
matchApiRouteEndpoints requestMethod requestPath endpoints =
  case filter (endpointAtPath requestPath) endpoints of
    [] -> TypedApiRouteNoMatch
    firstPathMatch : remainingPathMatches ->
      matchApiRouteMethod requestMethod (firstPathMatch :| remainingPathMatches)

matchApiRouteMethod :: Text -> NonEmpty SomeApiRouteEndpoint -> ApiRouteMatch
matchApiRouteMethod requestMethod pathMatches =
  case find (endpointHasMethod requestMethod) pathMatchList of
    Just endpoint -> TypedApiRouteMatched endpoint
    Nothing
      | requestMethod == "HEAD" ->
          maybe (TypedApiRouteMethodNotAllowed declared) TypedApiRouteMatchedHead (find (endpointHasMethod "GET") pathMatchList)
      | requestMethod == "OPTIONS" -> TypedApiRouteOptions declared
      | otherwise -> TypedApiRouteMethodNotAllowed declared
  where
    pathMatchList = NonEmpty.toList pathMatches
    declared = declaredMethods pathMatches

endpointAtPath :: Text -> SomeApiRouteEndpoint -> Bool
endpointAtPath requestPath (SomeApiRouteEndpoint endpoint) =
  case apiRouteEndpointPath endpoint of
    ApiPath declaredPath -> declaredPath == requestPath

endpointHasMethod :: Text -> SomeApiRouteEndpoint -> Bool
endpointHasMethod requestMethod (SomeApiRouteEndpoint endpoint) =
  apiMethodText (apiRouteEndpointMethod endpoint) == requestMethod

declaredMethods :: NonEmpty SomeApiRouteEndpoint -> NonEmpty ApiMethod
declaredMethods (firstEndpoint :| remainingEndpoints) =
  firstMethod :| nub (filter (/= firstMethod) (map endpointMethod remainingEndpoints))
  where
    firstMethod = endpointMethod firstEndpoint
    endpointMethod (SomeApiRouteEndpoint endpoint) = apiRouteEndpointMethod endpoint

protocolResponseToWaiResponse :: ProtocolResponse -> Wai.Response
protocolResponseToWaiResponse protocolResponse =
  case protocolResponseBody protocolResponse of
    ProtocolResponseBytes bytes -> Wai.responseLBS (protocolResponseStatus protocolResponse) (protocolResponseHeaders protocolResponse) (LazyByteString.fromStrict bytes)
    ProtocolResponseStream stream -> Wai.responseStream (protocolResponseStatus protocolResponse) (protocolResponseHeaders protocolResponse) stream

-- | Compatibility middleware for a legacy target table. New API endpoints
-- should use 'apiRouteEndpointMiddleware', whose declarations own handlers
-- and body decoding as well as path and method matching.
apiEndpointMiddleware :: [ApiEndpoint target] -> (Wai.Request -> target -> IO ApiResponseBody) -> Wai.Middleware
apiEndpointMiddleware endpoints runTarget innerApplication request respond =
  case matchApiEndpoints requestMethodText requestPathText endpoints of
    NoApiRouteMatch -> innerApplication request respond
    ApiMethodNotAllowed declaredMethodsValue -> respond (apiHttpResponseToWaiResponse (ApiHttpResponse HttpTypes.status405 [("Allow", apiAllowHeaderValue declaredMethodsValue)] Nothing))
    ApiRouteMatched target -> runTarget request target >>= respond . apiHttpResponseToWaiResponse . legacyRenderedApiResponse
    ApiRouteMatchedHead target -> do
      renderedResponse <- legacyRenderedApiResponse <$> runTarget request target
      respond (apiHttpResponseToWaiResponse (renderedResponse {apiHttpResponseBody = Nothing}))
    ApiRouteOptions declaredMethodsValue -> respond (apiHttpResponseToWaiResponse (ApiHttpResponse HttpTypes.status204 [("Allow", apiAllowHeaderValue declaredMethodsValue)] Nothing))
  where
    requestMethodText = TextEncoding.decodeUtf8With TextEncodingError.lenientDecode (Wai.requestMethod request)
    requestPathText = TextEncoding.decodeUtf8With TextEncodingError.lenientDecode (Wai.rawPathInfo request)
