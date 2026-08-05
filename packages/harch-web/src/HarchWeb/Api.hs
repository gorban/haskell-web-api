{-# LANGUAGE OverloadedStrings #-}

-- | Declarative, method-aware HTTP API endpoint matching.
--
-- This is the path/method dispatch foundation for a typed 'ApiEndpoint'
-- declaration (see @TASKS.md@ item AB): matching the request path first and
-- deriving the supported-method set from every endpoint declared at that
-- path, before deciding on the method. Request/response codecs, content
-- negotiation, and streaming bodies are separate, later concerns; this
-- module only decides which declared target (if any) owns a request.
--
-- Every declared endpoint today matches one fixed, context-independent
-- path. Typed path captures are a documented future extension and are not
-- yet supported.
module HarchWeb.Api
  ( ApiMethod (..),
    ApiPath,
    ApiEndpoint,
    ApiMatchResult (..),
    apiMethodText,
    apiEndpoint,
    apiEndpointTarget,
    at,
    matchApiEndpoints,
    apiAllowHeaderValue,
    ApiRequestData (..),
    ApiRequestSource (..),
    ApiRequestParseError (..),
    ApiFieldValue,
    RequestField,
    RequestCodec,
    apiTextValue,
    parseApiField,
    queryField,
    headerField,
    requiredField,
    optionalField,
    fieldWithDefault,
    runRequestCodec,
    ApiResponseBody (..),
    apiJsonResponse,
    apiTextResponse,
    apiBytesResponse,
  )
where

import Data.Aeson (ToJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Functor.Compose (Compose (..), getCompose)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding

-- | Methods an 'ApiEndpoint' can declare. @HEAD@ is never declared directly:
-- a matched @GET@ endpoint answers a @HEAD@ request with the same target,
-- and a caller renders the response without a body. @OPTIONS@ is likewise
-- never declared; synthesize it from 'apiAllowHeaderValue' for the matched
-- path rather than maintaining a second method table.
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

-- | Declare the fixed path an endpoint matches, e.g. @at "\/api\/status"@.
at :: Text -> ApiPath
at = ApiPath

data ApiEndpoint target = ApiEndpoint
  { apiEndpointTarget :: target,
    apiEndpointMethod :: ApiMethod,
    apiEndpointPath :: ApiPath
  }

apiEndpoint :: target -> ApiMethod -> ApiPath -> ApiEndpoint target
apiEndpoint = ApiEndpoint

data ApiMatchResult target
  = -- | No declared endpoint matches this path: respond @404 Not Found@.
    NoApiRouteMatch
  | -- | The path matches, but no declared endpoint accepts this method:
    -- respond @405 Method Not Allowed@ with these methods in 'apiAllowHeaderValue'.
    ApiMethodNotAllowed (NonEmpty ApiMethod)
  | -- | A declared endpoint matches the request method exactly.
    ApiRouteMatched target
  | -- | The path matches a declared @GET@ endpoint and the request method is
    -- @HEAD@: render that target's response without a body.
    ApiRouteMatchedHead target
  deriving (Eq, Show)

-- | Match a request method and path against a declared endpoint table.
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

-- | Render the @Allow@ header value for a matched path's declared methods,
-- including the @HEAD@ and @OPTIONS@ methods synthesized from them.
apiAllowHeaderValue :: NonEmpty ApiMethod -> Text
apiAllowHeaderValue declaredMethodsValue =
  Text.intercalate
    ", "
    ( map apiMethodText (NonEmpty.toList declaredMethodsValue)
        <> ["HEAD" | ApiGet `elem` declaredMethodsValue]
        <> ["OPTIONS"]
    )

-- | The pre-parsed request data a 'RequestCodec' decodes from. Path capture
-- and body sources are documented future extensions; only query parameters
-- and headers are supported today.
data ApiRequestData = ApiRequestData
  { apiRequestQueryParameters :: [(Text, Text)],
    apiRequestHeaders :: [(Text, Text)]
  }
  deriving (Eq, Show)

-- | The declared source of an individual request field.
data ApiRequestSource
  = ApiQuerySource
  | ApiHeaderSource
  deriving (Eq, Show)

data ApiRequestParseError
  = MissingApiField ApiRequestSource Text
  | DuplicateApiField ApiRequestSource Text
  | InvalidApiField ApiRequestSource Text
  deriving (Eq, Show)

newtype ApiFieldValue value = ApiFieldValue
  { runApiFieldValue :: Text -> Maybe value
  }

-- | An unvalidated field value, kept as-is.
apiTextValue :: ApiFieldValue Text
apiTextValue = ApiFieldValue Just

parseApiField :: (Text -> Maybe value) -> ApiFieldValue value
parseApiField = ApiFieldValue

newtype RequestField value = RequestField (ApiRequestData -> ([ApiRequestParseError], Maybe value))

-- | An accumulating-validation applicative: independent field errors from
-- separate 'RequestCodec' combinators concatenate rather than short-circuit,
-- matching 'HarchWeb.Action.ActionDecoder'.
type RequestCodec value = Compose ((->) ApiRequestData) (Compose ((,) [ApiRequestParseError]) Maybe) value

requestCodec :: (ApiRequestData -> ([ApiRequestParseError], Maybe value)) -> RequestCodec value
requestCodec decode = Compose (Compose . decode)

runRequestCodec :: RequestCodec value -> ApiRequestData -> ([ApiRequestParseError], Maybe value)
runRequestCodec codec requestData = getCompose (getCompose codec requestData)

sourceFields :: ApiRequestSource -> ApiRequestData -> [(Text, Text)]
sourceFields source requestData =
  case source of
    ApiQuerySource -> apiRequestQueryParameters requestData
    ApiHeaderSource -> apiRequestHeaders requestData

requestField :: ApiRequestSource -> Text -> ApiFieldValue value -> RequestField value
requestField source fieldName valueDecoder =
  RequestField $ \requestData ->
    case [fieldValue | (name, fieldValue) <- sourceFields source requestData, name == fieldName] of
      [fieldValue] ->
        maybe
          ([InvalidApiField source fieldName], Nothing)
          (\value -> ([], Just value))
          (runApiFieldValue valueDecoder fieldValue)
      [] -> ([MissingApiField source fieldName], Nothing)
      _ -> ([DuplicateApiField source fieldName], Nothing)

-- | Declare a field sourced from the request's query parameters.
queryField :: Text -> ApiFieldValue value -> RequestField value
queryField = requestField ApiQuerySource

-- | Declare a field sourced from the request's headers.
headerField :: Text -> ApiFieldValue value -> RequestField value
headerField = requestField ApiHeaderSource

requiredField :: RequestField value -> RequestCodec value
requiredField (RequestField decode) = requestCodec decode

optionalField :: RequestField value -> RequestCodec (Maybe value)
optionalField (RequestField decode) =
  requestCodec $ \requestData ->
    case decode requestData of
      ([], Just value) -> ([], Just (Just value))
      ([MissingApiField _ _], Nothing) -> ([], Just Nothing)
      (parseErrors, _) -> (parseErrors, Nothing)

fieldWithDefault :: value -> RequestField value -> RequestCodec value
fieldWithDefault defaultValue (RequestField decode) =
  requestCodec $ \requestData ->
    case decode requestData of
      ([], Just value) -> ([], Just value)
      ([MissingApiField _ _], Nothing) -> ([], Just defaultValue)
      parseErrors -> parseErrors

-- | A rendered API response body. Content negotiation and streaming bodies
-- are documented future extensions; every response today is fully buffered.
data ApiResponseBody = ApiResponseBody
  { apiResponseContentType :: Text,
    apiResponseBodyBytes :: ByteString
  }
  deriving (Eq, Show)

apiJsonResponse :: (ToJSON value) => value -> ApiResponseBody
apiJsonResponse value =
  ApiResponseBody
    { apiResponseContentType = "application/json; charset=utf-8",
      apiResponseBodyBytes = LazyByteString.toStrict (Aeson.encode value)
    }

apiTextResponse :: Text -> ApiResponseBody
apiTextResponse bodyText =
  ApiResponseBody
    { apiResponseContentType = "text/plain; charset=utf-8",
      apiResponseBodyBytes = TextEncoding.encodeUtf8 bodyText
    }

apiBytesResponse :: Text -> ByteString -> ApiResponseBody
apiBytesResponse contentType bodyBytes =
  ApiResponseBody
    { apiResponseContentType = contentType,
      apiResponseBodyBytes = bodyBytes
    }
