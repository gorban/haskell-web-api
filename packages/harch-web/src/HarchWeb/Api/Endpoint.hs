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
import HarchWeb qualified
import HarchWeb.Api.MediaType (ApiMediaType, apiContentTypeText)
import HarchWeb.Api.MediaType qualified as MediaType
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

-- | An endpoint either declares no body consumer, one bounded buffered
-- decoder, or a bounded URL-encoded form whose fields join the request codec.
-- Streaming codecs (including multipart) will use a separate constructor once
-- their storage lifecycle boundary is complete.
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
  case selectRepresentation declaredMediaTypes (acceptHeader request) of
    NoAcceptableRepresentation -> apiFailureResponse HttpTypes.status406 "API response has no acceptable representation."
    SelectedRepresentation selectedMediaType ->
      apiResponseBodyToResponse
        ApiResponseBody
          { apiResponseStatus = apiEndpointResponseStatus responseValue,
            apiResponseContentType = apiResponseEncoderContentType selectedEncoder,
            apiResponseHeaders = endpointResponseHeaders endpoint responseValue,
            apiResponseBodyBytes = apiResponseEncoderEncode selectedEncoder (apiEndpointResponseValue responseValue)
          }
      where
        selectedEncoder = responseEncoderFor selectedMediaType (apiRouteEndpointEncoders endpoint)
  where
    declaredMediaTypes = MediaType.apiContentTypeMediaType . apiResponseEncoderContentType <$> apiRouteEndpointEncoders endpoint

responseEncoderFor :: ApiMediaType -> NonEmpty (ApiResponseEncoder response) -> ApiResponseEncoder response
responseEncoderFor selectedMediaType encoders =
  foldr preferEncoder (NonEmpty.head encoders) (NonEmpty.tail encoders)
  where
    preferEncoder candidate fallback
      | MediaType.apiContentTypeMediaType (apiResponseEncoderContentType candidate) == selectedMediaType = candidate
      | otherwise = fallback

endpointResponseHeaders :: ApiRouteEndpoint fields body domainFailure response -> ApiResponse response -> [(Text, Text)]
endpointResponseHeaders endpoint responseValue
  | length (apiRouteEndpointEncoders endpoint) > 1 = addVaryAccept (apiEndpointResponseHeaders responseValue)
  | otherwise = apiEndpointResponseHeaders responseValue

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
