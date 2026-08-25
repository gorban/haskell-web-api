{-# LANGUAGE OverloadedStrings #-}

-- | Private adaptation of a heterogeneous endpoint table to the shared route
-- codec/definition boundary. This is deliberately the sole method-aware
-- endpoint dispatcher: it does not introduce WAI middleware or a competing
-- route policy.
--
-- Decision record (PR-F3, 2026-08-24): 'ApiEndpointFamily' is the one
-- validated declaration value from which both interpreters derive. Its smart
-- constructor rejects an empty family and an exact duplicate path/method
-- declaration before either interpreter exists, rather than retaining two raw
-- lists whose divergence would require a defensive runtime repair. Different
-- methods at one path remain distinct declarations, and the direct
-- interpreter's typed 404/405 guard remains a totality boundary rather than
-- a second method dispatcher. This extends the existing 'RouteCodec' /
-- 'RouteDefinition' ownership boundary; see @docs/design-guidance.md@.
module HarchWeb.Api.Endpoint.Family
  ( apiRouteDefinition,
    apiRouteDefinitionWithContext,
    apiRouteDefinitionWithContextNeverFailing,
    ApiEndpointFamily,
    ApiEndpointFamilyError (..),
    apiEndpointFamily,
    requireApiEndpointFamily,
    apiRouteEndpointFamilyCodec,
    apiRouteEndpointFamilyDefinition,
  )
where

import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.Encoding.Error qualified as TextEncodingError
import Data.Typeable (Typeable)
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
  (Typeable response) =>
  ApiEndpointContract fields body response ->
  (context -> ApiEndpointRequest fields body -> IO (Either domainFailure (ApiResponse response))) ->
  (domainFailure -> ApiResponse response) ->
  RouteDefinition route context
apiRouteDefinitionWithContext contract contextAwareHandler failureResponse =
  RouteDefinition
    { routeNavigationLabel = Nothing,
      routeMethods = [toRouteMethod method],
      routeResponse = \request routeRequest ->
        HarchWeb.ProtocolResponseResult
          <$> runApiRouteEndpointHandler
            fields
            body
            encoders
            fieldFailure
            (contextAwareHandler (HarchWeb.requestContext routeRequest))
            failureResponse
            request
    }
  where
    ApiEndpointContract method fields body encoders failurePolicy = contract
    fieldFailure = fieldFailureRenderer failurePolicy

-- | The total-handler variant has no fabricated failure renderer or
-- unreachable error branch.
apiRouteDefinitionWithContextNeverFailing ::
  (Typeable response) =>
  ApiEndpointContract fields body response ->
  (context -> ApiEndpointRequest fields body -> IO (ApiResponse response)) ->
  RouteDefinition route context
apiRouteDefinitionWithContextNeverFailing contract contextAwareHandler =
  RouteDefinition
    { routeNavigationLabel = Nothing,
      routeMethods = [toRouteMethod method],
      routeResponse = \request routeRequest ->
        HarchWeb.ProtocolResponseResult
          <$> runApiRouteEndpointHandlerNeverFailing
            fields
            body
            encoders
            fieldFailure
            (contextAwareHandler (HarchWeb.requestContext routeRequest))
            request
    }
  where
    ApiEndpointContract method fields body encoders failurePolicy = contract
    fieldFailure = fieldFailureRenderer failurePolicy

fieldFailureRenderer :: ApiFieldFailurePolicy response -> Maybe ([ApiRequestParseError] -> ApiResponse response)
fieldFailureRenderer failurePolicy =
  case failurePolicy of
    ApiUseGenericFieldFailure -> Nothing
    ApiRenderFieldFailures renderFieldFailures -> Just renderFieldFailures

-- | A non-empty, unambiguous set of typed endpoint declarations. Construct it
-- with 'apiEndpointFamily' so a codec and definition cannot be derived from
-- different tables.
newtype ApiEndpointFamily = ApiEndpointFamily (NonEmpty SomeApiRouteEndpoint)

-- | A rejected endpoint-family declaration names the exact ambiguity rather
-- than silently selecting its first declaration.
data ApiEndpointFamilyError
  = EmptyApiEndpointFamily
  | DuplicateApiEndpointDeclaration ApiPath ApiMethod

-- | Validate an endpoint table once before deriving either route-family
-- interpreter. Every path/method pair must occur exactly once; distinct
-- methods at the same path remain valid declarations.
apiEndpointFamily :: [SomeApiRouteEndpoint] -> Either ApiEndpointFamilyError ApiEndpointFamily
apiEndpointFamily endpoints =
  case NonEmpty.nonEmpty endpoints of
    Nothing -> Left EmptyApiEndpointFamily
    Just nonEmptyEndpoints ->
      case duplicateEndpointDeclaration endpoints of
        Nothing -> Right (ApiEndpointFamily nonEmptyEndpoints)
        Just (path, method) -> Left (DuplicateApiEndpointDeclaration path method)

-- | Assert that an application-authored, static endpoint table is valid.
-- Runtime-derived tables should instead handle 'apiEndpointFamily's precise
-- error result explicitly.
requireApiEndpointFamily :: [SomeApiRouteEndpoint] -> ApiEndpointFamily
requireApiEndpointFamily endpoints =
  case apiEndpointFamily endpoints of
    Left EmptyApiEndpointFamily -> error "API endpoint family must not be empty"
    Left (DuplicateApiEndpointDeclaration path method) ->
      error
        ( Text.unpack
            ( "API endpoint family declares "
                <> apiMethodText method
                <> " more than once at "
                <> apiPathText path
            )
        )
    Right family -> family

duplicateEndpointDeclaration :: [SomeApiRouteEndpoint] -> Maybe (ApiPath, ApiMethod)
duplicateEndpointDeclaration endpoints =
  case endpoints of
    [] -> Nothing
    endpoint : remainingEndpoints ->
      case find (sameEndpointDeclaration endpoint) remainingEndpoints of
        Just _ -> Just (endpointPath endpoint, endpointMethod endpoint)
        Nothing -> duplicateEndpointDeclaration remainingEndpoints

sameEndpointDeclaration :: SomeApiRouteEndpoint -> SomeApiRouteEndpoint -> Bool
sameEndpointDeclaration firstEndpoint secondEndpoint =
  endpointPath firstEndpoint == endpointPath secondEndpoint
    && endpointMethod firstEndpoint == endpointMethod secondEndpoint

endpointPath :: SomeApiRouteEndpoint -> ApiPath
endpointPath (SomeApiRouteEndpoint endpoint) = apiRouteEndpointPath endpoint

endpointMethod :: SomeApiRouteEndpoint -> ApiMethod
endpointMethod (SomeApiRouteEndpoint endpoint) = apiRouteEndpointMethod endpoint

-- | Adapt one validated endpoint family into the shared route codec. Combine
-- it with the application's other route families so the shared dispatcher
-- owns every 404/405/HEAD/OPTIONS decision.
apiRouteEndpointFamilyCodec :: ApiEndpointFamily -> HarchWeb.RouteCodec ApiPath context
apiRouteEndpointFamilyCodec family =
  HarchWeb.RouteCodec
    { HarchWeb.parseRoute = \context requestPath ->
        if any (endpointAtPath requestPath) endpoints
          then Just (HarchWeb.RouteRequest (ApiPath requestPath) context)
          else Nothing,
      HarchWeb.renderRoute = apiPathText . HarchWeb.requestRoute,
      HarchWeb.notFoundRequest = HarchWeb.RouteRequest (ApiPath Text.empty),
      HarchWeb.routeMethods = \(ApiPath pathText) -> HarchWeb.routeMethodPolicy (apiPathRouteMethods family pathText)
    }
  where
    endpoints = endpointFamilyEndpoints family

apiPathText :: ApiPath -> Text
apiPathText (ApiPath pathText) = pathText

endpointFamilyEndpoints :: ApiEndpointFamily -> [SomeApiRouteEndpoint]
endpointFamilyEndpoints (ApiEndpointFamily endpoints) = NonEmpty.toList endpoints

apiPathRouteMethods :: ApiEndpointFamily -> Text -> [HarchWeb.RouteMethod]
apiPathRouteMethods family pathText =
  maybe [] (map toRouteMethod . NonEmpty.toList . declaredMethods) (NonEmpty.nonEmpty (filter (endpointAtPath pathText) (endpointFamilyEndpoints family)))

-- | The 'RouteDefinition' for one path the family codec owns. A path with no
-- declared endpoint is the family codec's ordinary not-found sentinel, so it
-- renders a 404 before the defensive matcher is considered.
apiRouteEndpointFamilyDefinition :: ApiEndpointFamily -> ApiPath -> RouteDefinition ApiPath context
apiRouteEndpointFamilyDefinition family (ApiPath pathText) =
  RouteDefinition
    { routeNavigationLabel = Nothing,
      routeMethods = apiPathRouteMethods family pathText,
      routeResponse = \request _ ->
        case NonEmpty.nonEmpty (filter (endpointAtPath pathText) (endpointFamilyEndpoints family)) of
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
  endpointMethod firstEndpoint :| map endpointMethod remainingEndpoints

toRouteMethod :: ApiMethod -> HarchWeb.RouteMethod
toRouteMethod apiMethod =
  case apiMethod of
    ApiGet -> HarchWeb.RouteGet
    ApiPost -> HarchWeb.RoutePost
    ApiPut -> HarchWeb.RoutePut
    ApiPatch -> HarchWeb.RoutePatch
    ApiDelete -> HarchWeb.RouteDelete
