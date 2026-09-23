{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

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
    mapApiEndpointFamily,
    apiPathText,
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
import HarchWeb.Api.Response
import HarchWeb.EndpointSecurity (EndpointMetadata (endpointRouteTemplate), routeTemplateText)
import HarchWeb.Markup (safeUrlText)
import HarchWeb.Server (unboundedRouteExecutionPolicy)
import HarchWeb.Site (RouteDefinition (..), RouteHandler (ProtocolRouteHandler))
import Network.HTTP.Types qualified as HttpTypes
import Network.Wai qualified as Wai

-- | Convert a declaration into one entry in a 'RouteDefinition' table. The
-- server has already selected the route and method before this runs, so it
-- cannot produce a competing 404/405/HEAD/OPTIONS policy.
apiRouteDefinition :: EndpointMetadata authorization -> ApiRouteEndpoint context extension fields body domainFailure response -> RouteDefinition route context authorization
apiRouteDefinition metadata endpoint =
  RouteDefinition
    { routeNavigationLabel = Nothing,
      routeMetadata = metadata,
      routeMethods = \routeRequest ->
        case apiRouteEndpointAvailability endpoint (HarchWeb.requestContext routeRequest) of
          ApiAvailable -> HarchWeb.routeMethodPolicy [toRouteMethod (apiRouteEndpointMethod endpoint)]
          ApiHidden -> HarchWeb.RouteHidden,
      routeExecutionPolicy = unboundedRouteExecutionPolicy,
      routeHandler = ProtocolRouteHandler $ \request routeRequest ->
        case apiRouteEndpointAvailability endpoint (HarchWeb.requestContext routeRequest) of
          ApiAvailable -> HarchWeb.NonPageProtocolResponse <$> runApiRouteEndpoint (HarchWeb.requestContext routeRequest) endpoint request
          ApiHidden -> pure (HarchWeb.NonPageProtocolResponse (apiHttpResponseToProtocolResponse (ApiHttpResponse HttpTypes.status404 [] Nothing)))
    }

-- | Like 'apiRouteEndpointWithContext' composed with 'apiRouteDefinition'.
-- The endpoint's declared path comes from @metadata@'s own already-validated
-- 'HarchWeb.EndpointSecurity.endpointRouteTemplate' rather than a second,
-- independently authored path: this is the same real path an application's
-- own route table already dispatches on, reused rather than duplicated, so
-- a documentation interpreter reading this declaration can never disagree
-- with where the endpoint actually lives. See the AHI-4E decision record in
-- @docs/design-guidance.md@ for why 'ApiRouteEndpoint' needed a
-- context-aware constructor family for this to be possible at all.
apiRouteDefinitionWithContext ::
  (Typeable response) =>
  ApiEndpointContract extension fields body response ->
  EndpointMetadata authorization ->
  (context -> ApiEndpointRequest fields body -> IO (Either domainFailure (ApiResponse response))) ->
  (domainFailure -> ApiResponse response) ->
  RouteDefinition route context authorization
apiRouteDefinitionWithContext contract metadata contextAwareHandler failureResponse =
  apiRouteDefinition metadata (apiRouteEndpointWithContext declaration contextAwareHandler failureResponse)
  where
    declaration = ApiRouteEndpointDeclaration (at (routeTemplateText (endpointRouteTemplate metadata))) contract

-- | The total-handler variant has no fabricated failure renderer or
-- unreachable error branch, mirroring 'apiRouteDefinitionWithContext'.
apiRouteDefinitionWithContextNeverFailing ::
  (Typeable response) =>
  ApiEndpointContract extension fields body response ->
  EndpointMetadata authorization ->
  (context -> ApiEndpointRequest fields body -> IO (ApiResponse response)) ->
  RouteDefinition route context authorization
apiRouteDefinitionWithContextNeverFailing contract metadata contextAwareHandler =
  apiRouteDefinition metadata (apiRouteEndpointWithContextNeverFailing declaration contextAwareHandler)
  where
    declaration = ApiRouteEndpointDeclaration (at (routeTemplateText (endpointRouteTemplate metadata))) contract

-- | A non-empty, unambiguous set of typed endpoint declarations. Construct it
-- with 'apiEndpointFamily' so a codec and definition cannot be derived from
-- different tables.
newtype ApiEndpointFamily context extension = ApiEndpointFamily (NonEmpty (SomeApiRouteEndpoint context extension))

-- | A rejected endpoint-family declaration names the exact ambiguity rather
-- than silently selecting its first declaration.
data ApiEndpointFamilyError
  = EmptyApiEndpointFamily
  | DuplicateApiEndpointDeclaration ApiPath ApiMethod

-- | Validate an endpoint table once before deriving either route-family
-- interpreter. Every path/method pair must occur exactly once; distinct
-- methods at the same path remain valid declarations.
apiEndpointFamily :: [SomeApiRouteEndpoint context extension] -> Either ApiEndpointFamilyError (ApiEndpointFamily context extension)
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
requireApiEndpointFamily :: [SomeApiRouteEndpoint context extension] -> ApiEndpointFamily context extension
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

-- | Project every heterogeneous declaration in a validated family into one
-- result type. This is the read-only boundary for explicit declaration
-- interpreters: an interpreter receives the exact endpoint values that feed
-- the shared runtime adapter, but cannot replace the family table or take
-- over path/method dispatch. For example, the optional OpenAPI package uses
-- this with 'withApiRouteEndpointDeclaration' to read only families the
-- application deliberately supplies.
mapApiEndpointFamily ::
  (forall fields body domainFailure response. ApiRouteEndpoint context extension fields body domainFailure response -> result) ->
  ApiEndpointFamily context extension ->
  [result]
mapApiEndpointFamily mapEndpoint (ApiEndpointFamily endpoints) =
  map mapSomeEndpoint (NonEmpty.toList endpoints)
  where
    mapSomeEndpoint (SomeApiRouteEndpoint endpoint) = mapEndpoint endpoint

duplicateEndpointDeclaration :: [SomeApiRouteEndpoint context extension] -> Maybe (ApiPath, ApiMethod)
duplicateEndpointDeclaration endpoints =
  case endpoints of
    [] -> Nothing
    endpoint : remainingEndpoints ->
      case find (sameEndpointDeclaration endpoint) remainingEndpoints of
        Just _ -> Just (endpointPath endpoint, endpointMethod endpoint)
        Nothing -> duplicateEndpointDeclaration remainingEndpoints

sameEndpointDeclaration :: SomeApiRouteEndpoint context extension -> SomeApiRouteEndpoint context extension -> Bool
sameEndpointDeclaration firstEndpoint secondEndpoint =
  endpointPath firstEndpoint == endpointPath secondEndpoint
    && endpointMethod firstEndpoint == endpointMethod secondEndpoint

endpointPath :: SomeApiRouteEndpoint context extension -> ApiPath
endpointPath (SomeApiRouteEndpoint endpoint) = apiRouteEndpointPath endpoint

endpointMethod :: SomeApiRouteEndpoint context extension -> ApiMethod
endpointMethod (SomeApiRouteEndpoint endpoint) = apiRouteEndpointMethod endpoint

-- | Adapt one validated endpoint family into the shared route codec. Combine
-- it with the application's other route families so the shared dispatcher
-- owns every 404/405/HEAD/OPTIONS decision.
apiRouteEndpointFamilyCodec :: ApiEndpointFamily context extension -> HarchWeb.RouteCodec ApiPath context
apiRouteEndpointFamilyCodec family =
  HarchWeb.RouteCodec
    { HarchWeb.parseRoute = \context location ->
        case apiPathAtLocation location of
          Just apiPath -> HarchWeb.RouteParsed (HarchWeb.RouteRequest apiPath context)
          Nothing -> HarchWeb.RouteNotMatched,
      HarchWeb.renderRoute = apiPathLocation . HarchWeb.requestRoute,
      HarchWeb.notFoundRequest = HarchWeb.RouteRequest (ApiPath Text.empty),
      HarchWeb.routeMethods = \routeRequest ->
        case HarchWeb.requestRoute routeRequest of
          ApiPath pathText -> HarchWeb.routeMethodPolicy (apiPathRouteMethods family (HarchWeb.requestContext routeRequest) pathText)
    }
  where
    endpoints = endpointFamilyEndpoints family
    apiPathAtLocation location =
      case find (endpointAtLocation location) endpoints of
        Nothing -> Nothing
        Just endpoint -> Just (endpointPath endpoint)

endpointAtLocation :: HarchWeb.RouteLocation -> SomeApiRouteEndpoint context extension -> Bool
endpointAtLocation location endpoint =
  apiPathText (endpointPath endpoint)
    == safeUrlText (HarchWeb.encodeRouteLocation (location {HarchWeb.routeQueryFields = []}))

apiPathLocation :: ApiPath -> HarchWeb.RouteLocation
apiPathLocation apiPath =
  case apiPathText apiPath of
    "" -> HarchWeb.RouteLocation [] []
    authoredPath ->
      case HarchWeb.decodeRouteLocation (HarchWeb.requestTarget (TextEncoding.encodeUtf8 authoredPath) "") of
        Left routeError -> error ("invalid authored API path: " <> show routeError)
        Right location -> location

-- | Render an authored API path for a declaration interpreter. The value
-- remains opaque for routing: this accessor gives a deliberate, read-only
-- interpreter such as the optional OpenAPI package its static declaration
-- text without exposing another way to construct route identities.
apiPathText :: ApiPath -> Text
apiPathText (ApiPath pathText) = pathText

endpointFamilyEndpoints :: ApiEndpointFamily context extension -> [SomeApiRouteEndpoint context extension]
endpointFamilyEndpoints (ApiEndpointFamily endpoints) = NonEmpty.toList endpoints

apiPathRouteMethods :: ApiEndpointFamily context extension -> context -> Text -> [HarchWeb.RouteMethod]
apiPathRouteMethods family context pathText =
  maybe [] (map toRouteMethod . NonEmpty.toList . declaredMethods) (NonEmpty.nonEmpty (filter (\endpoint -> endpointAtPath pathText endpoint && endpointIsAvailable context endpoint) (endpointFamilyEndpoints family)))

-- | The 'RouteDefinition' for one path the family codec owns. A path with no
-- declared endpoint is the family codec's ordinary not-found sentinel, so it
-- renders a 404 before the defensive matcher is considered.
apiRouteEndpointFamilyDefinition :: (ApiPath -> EndpointMetadata authorization) -> ApiEndpointFamily context extension -> ApiPath -> RouteDefinition ApiPath context authorization
apiRouteEndpointFamilyDefinition endpointMetadataForPath family apiPath@(ApiPath pathText) =
  RouteDefinition
    { routeNavigationLabel = Nothing,
      routeMetadata = endpointMetadataForPath apiPath,
      routeMethods = \routeRequest -> HarchWeb.routeMethodPolicy (apiPathRouteMethods family (HarchWeb.requestContext routeRequest) pathText),
      routeExecutionPolicy = unboundedRouteExecutionPolicy,
      routeHandler = ProtocolRouteHandler $ \request routeRequest ->
        case NonEmpty.nonEmpty (filter (\endpoint -> endpointAtPath pathText endpoint && endpointIsAvailable (HarchWeb.requestContext routeRequest) endpoint) (endpointFamilyEndpoints family)) of
          Nothing -> pure (HarchWeb.NonPageProtocolResponse (apiHttpResponseToProtocolResponse (ApiHttpResponse HttpTypes.status404 [] Nothing)))
          Just pathEndpoints ->
            case matchedApiRouteEndpoint pathEndpoints (requestMethodTextFromWai request) of
              Nothing -> pure (HarchWeb.NonPageProtocolResponse (apiHttpResponseToProtocolResponse (methodNotAllowedResponse pathEndpoints)))
              Just (SomeApiRouteEndpoint endpoint) -> HarchWeb.NonPageProtocolResponse <$> runApiRouteEndpoint (HarchWeb.requestContext routeRequest) endpoint request
    }

-- | Resolve a method within a path that is already known to have at least one
-- declaration. The shared dispatcher normally makes 'Nothing' unreachable;
-- keeping it explicit lets the family definition remain total when embedded
-- directly or wired incorrectly.
matchedApiRouteEndpoint :: NonEmpty (SomeApiRouteEndpoint context extension) -> Text -> Maybe (SomeApiRouteEndpoint context extension)
matchedApiRouteEndpoint pathEndpoints requestMethod =
  case find (endpointHasMethod requestMethod) endpointList of
    Just endpoint -> Just endpoint
    Nothing
      | requestMethod == "HEAD" -> find (endpointHasMethod "GET") endpointList
      | otherwise -> Nothing
  where
    endpointList = NonEmpty.toList pathEndpoints

methodNotAllowedResponse :: NonEmpty (SomeApiRouteEndpoint context extension) -> ApiHttpResponse
methodNotAllowedResponse pathEndpoints =
  ApiHttpResponse
    HttpTypes.status405
    [("Allow", apiHeaderValueLiteral (Text.intercalate ", " (map (apiMethodText . endpointMethod) (NonEmpty.toList pathEndpoints))))]
    Nothing

requestMethodTextFromWai :: Wai.Request -> Text
requestMethodTextFromWai request = TextEncoding.decodeUtf8With TextEncodingError.lenientDecode (Wai.requestMethod request)

endpointAtPath :: Text -> SomeApiRouteEndpoint context extension -> Bool
endpointAtPath requestPath (SomeApiRouteEndpoint endpoint) =
  case apiRouteEndpointPath endpoint of
    ApiPath declaredPath -> declaredPath == requestPath

endpointIsAvailable :: context -> SomeApiRouteEndpoint context extension -> Bool
endpointIsAvailable context (SomeApiRouteEndpoint endpoint) =
  apiRouteEndpointAvailability endpoint context == ApiAvailable

endpointHasMethod :: Text -> SomeApiRouteEndpoint context extension -> Bool
endpointHasMethod requestMethod (SomeApiRouteEndpoint endpoint) =
  apiMethodText (apiRouteEndpointMethod endpoint) == requestMethod

declaredMethods :: NonEmpty (SomeApiRouteEndpoint context extension) -> NonEmpty ApiMethod
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
