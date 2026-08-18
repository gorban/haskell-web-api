{-# LANGUAGE OverloadedStrings #-}

-- | Private adaptation of a heterogeneous endpoint table to the shared route
-- codec/definition boundary. This is deliberately the sole method-aware
-- endpoint dispatcher: it does not introduce WAI middleware or a competing
-- route policy.
module HarchWeb.Api.Endpoint.Family
  ( apiRouteDefinition,
    apiRouteDefinitionWithContext,
    apiRouteDefinitionWithContextWithFieldFailure,
    apiRouteDefinitionWithContextNeverFailing,
    apiRouteDefinitionWithContextNeverFailingWithFieldFailure,
    apiRouteEndpointFamilyCodec,
    apiRouteEndpointFamilyDefinition,
  )
where

import Data.List (find, nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.Encoding.Error qualified as TextEncodingError
import HarchWeb qualified
import HarchWeb.Api.Endpoint.Internal
import HarchWeb.Api.Endpoint.Runtime
import HarchWeb.Api.Request
import HarchWeb.Api.Response
import HarchWeb.Site (RouteDefinition (..))
import Network.HTTP.Types qualified as HttpTypes
import Network.Wai qualified as Wai

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

-- | Like 'apiRouteEndpoint' composed with 'apiRouteDefinition', but the
-- handler additionally receives the route's already-resolved context. This
-- calls the same runtime decoder directly rather than constructing unused
-- synthetic endpoint path and method fields.
apiRouteDefinitionWithContext ::
  ApiMethod ->
  RequestCodec fields ->
  ApiRequestBody body ->
  NonEmpty (ApiResponseEncoder response) ->
  (context -> ApiEndpointRequest fields body -> IO (Either domainFailure (ApiResponse response))) ->
  (domainFailure -> ApiResponse response) ->
  RouteDefinition route context
apiRouteDefinitionWithContext method fields body encoders contextAwareHandler failureResponse =
  RouteDefinition
    { routeNavigationLabel = Nothing,
      routeMethods = [toRouteMethod method],
      routeResponse = \request routeRequest ->
        HarchWeb.ProtocolResponseResult
          <$> runApiRouteEndpointHandler
            fields
            body
            encoders
            Nothing
            (contextAwareHandler (HarchWeb.requestContext routeRequest))
            failureResponse
            request
    }

-- | Context-aware variant that gives the declaration its accumulated typed
-- request-field failures. The runtime fixes this response's status at 400.
apiRouteDefinitionWithContextWithFieldFailure ::
  ApiMethod ->
  RequestCodec fields ->
  ApiRequestBody body ->
  NonEmpty (ApiResponseEncoder response) ->
  ([ApiRequestParseError] -> ApiResponse response) ->
  (context -> ApiEndpointRequest fields body -> IO (Either domainFailure (ApiResponse response))) ->
  (domainFailure -> ApiResponse response) ->
  RouteDefinition route context
apiRouteDefinitionWithContextWithFieldFailure method fields body encoders fieldFailure contextAwareHandler failureResponse =
  RouteDefinition
    { routeNavigationLabel = Nothing,
      routeMethods = [toRouteMethod method],
      routeResponse = \request routeRequest ->
        HarchWeb.ProtocolResponseResult
          <$> runApiRouteEndpointHandler
            fields
            body
            encoders
            (Just fieldFailure)
            (contextAwareHandler (HarchWeb.requestContext routeRequest))
            failureResponse
            request
    }

-- | The total-handler variant has no fabricated failure renderer or
-- unreachable error branch.
apiRouteDefinitionWithContextNeverFailing ::
  ApiMethod ->
  RequestCodec fields ->
  ApiRequestBody body ->
  NonEmpty (ApiResponseEncoder response) ->
  (context -> ApiEndpointRequest fields body -> IO (ApiResponse response)) ->
  RouteDefinition route context
apiRouteDefinitionWithContextNeverFailing method fields body encoders contextAwareHandler =
  RouteDefinition
    { routeNavigationLabel = Nothing,
      routeMethods = [toRouteMethod method],
      routeResponse = \request routeRequest ->
        HarchWeb.ProtocolResponseResult
          <$> runApiRouteEndpointHandlerNeverFailing
            fields
            body
            encoders
            Nothing
            (contextAwareHandler (HarchWeb.requestContext routeRequest))
            request
    }

-- | Total-handler context-aware variant that renders accumulated request-field
-- failures at the declaration boundary while retaining HTTP 400.
apiRouteDefinitionWithContextNeverFailingWithFieldFailure ::
  ApiMethod ->
  RequestCodec fields ->
  ApiRequestBody body ->
  NonEmpty (ApiResponseEncoder response) ->
  ([ApiRequestParseError] -> ApiResponse response) ->
  (context -> ApiEndpointRequest fields body -> IO (ApiResponse response)) ->
  RouteDefinition route context
apiRouteDefinitionWithContextNeverFailingWithFieldFailure method fields body encoders fieldFailure contextAwareHandler =
  RouteDefinition
    { routeNavigationLabel = Nothing,
      routeMethods = [toRouteMethod method],
      routeResponse = \request routeRequest ->
        HarchWeb.ProtocolResponseResult
          <$> runApiRouteEndpointHandlerNeverFailing
            fields
            body
            encoders
            (Just fieldFailure)
            (contextAwareHandler (HarchWeb.requestContext routeRequest))
            request
    }

-- | Adapt a heterogeneous endpoint table into one route family. Combine it
-- with the application's other route families so the shared dispatcher owns
-- every 404/405/HEAD/OPTIONS decision.
apiRouteEndpointFamilyCodec :: [SomeApiRouteEndpoint] -> HarchWeb.RouteCodec ApiPath context
apiRouteEndpointFamilyCodec endpoints =
  HarchWeb.RouteCodec
    { HarchWeb.parseRoute = \context requestPath ->
        if any (endpointAtPath requestPath) endpoints
          then Just (HarchWeb.RouteRequest (ApiPath requestPath) context)
          else Nothing,
      HarchWeb.renderRoute = apiPathText . HarchWeb.requestRoute,
      HarchWeb.notFoundRequest = HarchWeb.RouteRequest (ApiPath Text.empty),
      HarchWeb.routeMethods = \(ApiPath pathText) -> HarchWeb.routeMethodPolicy (apiPathRouteMethods endpoints pathText)
    }

apiPathText :: ApiPath -> Text
apiPathText (ApiPath pathText) = pathText

apiPathRouteMethods :: [SomeApiRouteEndpoint] -> Text -> [HarchWeb.RouteMethod]
apiPathRouteMethods endpoints pathText =
  maybe [] (map toRouteMethod . NonEmpty.toList . declaredMethods) (NonEmpty.nonEmpty (filter (endpointAtPath pathText) endpoints))

-- | The 'RouteDefinition' for one path the family codec owns. A path with no
-- declared endpoint is the family codec's ordinary not-found sentinel, so it
-- renders a 404 before the defensive matcher is considered.
apiRouteEndpointFamilyDefinition :: [SomeApiRouteEndpoint] -> ApiPath -> RouteDefinition ApiPath context
apiRouteEndpointFamilyDefinition endpoints (ApiPath pathText) =
  RouteDefinition
    { routeNavigationLabel = Nothing,
      routeMethods = apiPathRouteMethods endpoints pathText,
      routeResponse = \request _ ->
        case NonEmpty.nonEmpty (filter (endpointAtPath pathText) endpoints) of
          Nothing -> pure (HarchWeb.ProtocolResponseResult (apiHttpResponseToProtocolResponse (ApiHttpResponse HttpTypes.status404 [] Nothing)))
          Just pathEndpoints ->
            case matchedApiRouteEndpoint pathEndpoints (requestMethodTextFromWai request) of
              Nothing -> pure (HarchWeb.ProtocolResponseResult (apiHttpResponseToProtocolResponse (methodNotAllowedResponse pathEndpoints)))
              Just (SomeApiRouteEndpoint endpoint) -> HarchWeb.ProtocolResponseResult <$> runApiRouteEndpoint endpoint request
    }

-- | Resolve a method within a path that is already known to have at least one
-- declaration. The shared dispatcher normally makes 'Nothing' unreachable;
-- keeping it explicit lets the family definition remain total when embedded
-- directly or wired incorrectly.
matchedApiRouteEndpoint :: NonEmpty SomeApiRouteEndpoint -> Text -> Maybe SomeApiRouteEndpoint
matchedApiRouteEndpoint pathEndpoints requestMethod =
  case find (endpointHasMethod requestMethod) endpointList of
    Just endpoint -> Just endpoint
    Nothing
      | requestMethod == "HEAD" -> find (endpointHasMethod "GET") endpointList
      | otherwise -> Nothing
  where
    endpointList = NonEmpty.toList pathEndpoints

methodNotAllowedResponse :: NonEmpty SomeApiRouteEndpoint -> ApiHttpResponse
methodNotAllowedResponse pathEndpoints =
  ApiHttpResponse
    HttpTypes.status405
    [("Allow", apiHeaderValueLiteral (Text.intercalate ", " (map (apiMethodText . endpointMethod) (NonEmpty.toList pathEndpoints))))]
    Nothing
  where
    endpointMethod (SomeApiRouteEndpoint endpoint) = apiRouteEndpointMethod endpoint

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

toRouteMethod :: ApiMethod -> HarchWeb.RouteMethod
toRouteMethod apiMethod =
  case apiMethod of
    ApiGet -> HarchWeb.RouteGet
    ApiPost -> HarchWeb.RoutePost
    ApiPut -> HarchWeb.RoutePut
    ApiPatch -> HarchWeb.RoutePatch
    ApiDelete -> HarchWeb.RouteDelete
