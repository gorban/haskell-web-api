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
  )
where

import Data.List (nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text

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
