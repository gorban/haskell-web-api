{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Method-aware endpoint matching and WAI dispatch. The closed
-- route-family registry (`apiRouteEndpointFamilyCodec`/
-- `apiRouteEndpointFamilyDefinition`) is the sole supported dispatch path:
-- the legacy `ApiEndpoint`/`apiEndpointMiddleware` compatibility table and
-- the intermediate `apiRouteEndpointMiddleware` typed-WAI-middleware
-- composition were removed 2026-08-13 once every application in this
-- repository (`examples/custom-api`, `examples/multipart-upload`) had
-- migrated onto the family registry; see the AK decision record in
-- @docs/design-guidance.md@ for why that made both a genuine deletion
-- rather than a relocation. That deletion's own coverage run then surfaced
-- the @ApiRouteMatch@ sum type's @OPTIONS@\/method-not-allowed distinction
-- as dead code: @matchedApiRouteEndpointOrDie@ never actually branched on
-- it differently, so it was deleted too and folded into a direct
-- match\/HEAD-fallback\/die decision — which is why this module is now
-- 499 lines, closing the AK module-health signal by one line's margin; see
-- the same decision record for the exact numbers and why that margin is
-- worth re-checking before the next addition here.
module HarchWeb.Api.Endpoint
  ( ApiMethod (..),
    ApiPath,
    ApiRouteEndpoint,
    SomeApiRouteEndpoint (..),
    ApiEndpointRequest (..),
    ApiRequestBody (..),
    ApiStreamingRequest (..),
    RequestBodyReadFailure (..),
    ApiMultipartRequest,
    ApiMultipartRequestError (..),
    withApiMultipartRequest,
    apiMethodText,
    apiRouteEndpoint,
    apiRouteEndpointAt,
    apiRouteEndpointPath,
    apiRouteDefinition,
    apiRouteEndpointFamilyCodec,
    apiRouteEndpointFamilyDefinition,
    matchedApiRouteEndpointOrDie,
    at,
    apiResponseBodyToProtocolResponse,
    ApiHttpResponse (..),
    apiHttpResponseToProtocolResponse,
  )
where

import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.CaseInsensitive qualified as CaseInsensitive
import Data.IORef qualified as IORef
import Data.List (find, nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
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

-- | Methods an 'ApiRouteEndpoint' can declare. @HEAD@ is synthesized from
-- @GET@ and @OPTIONS@ from the declared method table for a matched path.
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

-- | One chunk pulled from a declared streaming request body, bounded the
-- same way 'HarchWeb.Server.RequestBody.readRequestBodyUpTo' bounds a
-- buffered one: each pull enforces the running-total budget instead of the
-- framework retaining the body itself, so a handler that discards each
-- chunk once it has used it keeps bounded memory regardless of body size.
-- An empty chunk marks the end of the body. Calling it after the body ends
-- keeps returning an empty chunk, matching 'Network.Wai.getRequestBodyChunk'.
newtype ApiStreamingRequest = ApiStreamingRequest
  { pullApiStreamingRequestChunk :: IO (Either RequestBodyReadFailure ByteString.ByteString)
  }

-- | An endpoint either declares no body consumer, one bounded buffered
-- decoder, a bounded URL-encoded form whose fields join the request codec,
-- a bounded incremental stream, or a scoped multipart consumer. A multipart
-- body remains scoped: its callback owns each completed part, and must
-- deliberately promote any file that needs to outlive the request.
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
  ApiStreamingRequestBody ::
    Int ->
    ApiRequestBody ApiStreamingRequest
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

-- | Adapt a heterogeneous endpoint table into one route family whose route
-- identity is a matched endpoint's declared 'ApiPath'. Combine the result
-- with 'HarchWeb.combineRouteCodecs' alongside an application's other route
-- families (its page routes, say) so the table becomes part of that one
-- closed 'HarchWeb.RouteCodec' and 'HarchWeb.Site' — the framework's closed
-- route-family registry for 'HarchWeb.Api' endpoints. Pair with
-- 'apiRouteEndpointFamilyDefinition' for the matching 'RouteDefinition'
-- selector.
apiRouteEndpointFamilyCodec :: [SomeApiRouteEndpoint] -> HarchWeb.RouteCodec ApiPath context
apiRouteEndpointFamilyCodec endpoints =
  HarchWeb.RouteCodec
    { HarchWeb.parseRoute = \context requestPath ->
        if any (endpointAtPath requestPath) endpoints
          then Just (HarchWeb.RouteRequest (ApiPath requestPath) context)
          else Nothing,
      HarchWeb.renderRoute = apiPathText . HarchWeb.requestRoute,
      HarchWeb.notFoundRequest = HarchWeb.RouteRequest (ApiPath Text.empty),
      HarchWeb.routeMethods = \(ApiPath pathText) -> apiPathRouteMethods endpoints pathText
    }

apiPathText :: ApiPath -> Text
apiPathText (ApiPath pathText) = pathText

apiPathRouteMethods :: [SomeApiRouteEndpoint] -> Text -> [HarchWeb.RouteMethod]
apiPathRouteMethods endpoints pathText =
  maybe [] (map toRouteMethod . NonEmpty.toList . declaredMethods) (NonEmpty.nonEmpty (filter (endpointAtPath pathText) endpoints))

-- | The 'RouteDefinition' for one path 'apiRouteEndpointFamilyCodec' owns.
-- Give both the same endpoint table so their notion of which endpoints live
-- at a path always agrees.
--
-- A path with no declared endpoint renders an ordinary API @404@ rather than
-- calling 'matchedApiRouteEndpointOrDie': 'apiRouteEndpointFamilyCodec's own
-- @notFoundRequest@ deliberately resolves to such a path (see its Haddock),
-- so an application using this family standalone — not combined with a
-- catch-all family via 'HarchWeb.combineRouteCodecs' whose own not-found
-- route would otherwise absorb this case — reaches this branch on every
-- ordinary unmatched request, not only on a wiring defect.
apiRouteEndpointFamilyDefinition :: [SomeApiRouteEndpoint] -> ApiPath -> RouteDefinition ApiPath context
apiRouteEndpointFamilyDefinition endpoints (ApiPath pathText) =
  RouteDefinition
    { routeNavigationLabel = Nothing,
      routeMethods = apiPathRouteMethods endpoints pathText,
      routeResponse = \request _ ->
        case filter (endpointAtPath pathText) endpoints of
          [] -> pure (HarchWeb.ProtocolResponseResult (apiHttpResponseToProtocolResponse (ApiHttpResponse HttpTypes.status404 [] Nothing)))
          _ : _ ->
            case matchedApiRouteEndpointOrDie endpoints pathText (requestMethodTextFromWai request) of
              SomeApiRouteEndpoint endpoint -> HarchWeb.ProtocolResponseResult <$> runApiRouteEndpoint endpoint request
    }

-- | The one endpoint a path and method must resolve to, once
-- 'HarchWeb.Routing' has already restricted dispatch to a request method
-- this path's own 'routeMethods' declares (or @HEAD@ alongside a declared
-- @GET@). Reaching either 'error' here means an application built this
-- 'RouteDefinition' from a different endpoint table than the one it gave
-- 'apiRouteEndpointFamilyCodec' for the same route family — a framework
-- wiring defect, not an ordinary request outcome — so it fails loudly
-- instead of silently dispatching to the wrong handler.
matchedApiRouteEndpointOrDie :: [SomeApiRouteEndpoint] -> Text -> Text -> SomeApiRouteEndpoint
matchedApiRouteEndpointOrDie endpoints requestPath requestMethod =
  case filter (endpointAtPath requestPath) endpoints of
    [] -> error ("HarchWeb.Api.Endpoint: no endpoint declared at " <> Text.unpack requestPath)
    pathEndpoints ->
      case find (endpointHasMethod requestMethod) pathEndpoints of
        Just endpoint -> endpoint
        Nothing
          | requestMethod == "HEAD" -> fromMaybe methodNotDeclared (find (endpointHasMethod "GET") pathEndpoints)
          | otherwise -> methodNotDeclared
      where
        methodNotDeclared = error ("HarchWeb.Api.Endpoint: " <> Text.unpack requestMethod <> " is not declared at " <> Text.unpack requestPath)

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
        ApiStreamingRequestBody maximumBytes -> decodeInitialFieldsWithBody (newApiStreamingRequest maximumBytes request)
        ApiMultipartRequestBody storage limits -> decodeInitialFieldsWithBody (newApiMultipartRequest storage limits request)

    decodeInitialFieldsWithBody newBody =
      decodeInitialFields $ \decodedFields -> do
        bodyValue <- newBody
        runHandler decodedFields bodyValue

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
          protocolResponseObservabilityAttributes = apiEndpointResponseObservabilityAttributes responseValue,
          protocolResponseLogEntries = apiEndpointResponseLogEntries responseValue
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

data ApiHttpResponse = ApiHttpResponse
  { apiHttpResponseStatus :: HttpTypes.Status,
    apiHttpResponseHeaders :: [(Text, Text)],
    apiHttpResponseBody :: Maybe ApiResponseBody
  }
  deriving (Eq, Show)

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

requestMethodTextFromWai :: Wai.Request -> Text
requestMethodTextFromWai request = TextEncoding.decodeUtf8With TextEncodingError.lenientDecode (Wai.requestMethod request)

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
