{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The @catalog.api@ application module (AHI-4E composed-domains slice):
-- a second, distinct module the composed root mounts at @/api/catalog@ while
-- @catalog.web@ keeps the HTML surface at @/catalog@. Both are transports
-- over the same 'CatalogQueries' port: this module exposes
-- @GET /api/catalog/items@ (policy 'MayReadCatalog') as typed JSON derived
-- from 'loadCatalogSummary'.
--
-- The module constructor takes the generic API extension value, so the
-- domain compiles unchanged without OpenAPI ('HarchWeb.Api.NoApiExtension')
-- while the composed root supplies the OpenAPI extension for its documented
-- assembly. The package owns only abstract policy ('CatalogPolicy'); issuer,
-- audience, scopes, and OAuth endpoints stay at the composed root.
module Catalog.Api
  ( CatalogApiAction,
    CatalogApiActionTarget,
    CatalogApiRoute (..),
    buildCatalogApiModule,
    catalogItemsApiContract,
    catalogItemsApiEndpoint,
    catalogItemsApiHandler,
    catalogItemsFamily,
  )
where

import Catalog.Domain (CatalogContext, CatalogPolicy (MayReadCatalog), CatalogQueries (loadCatalogSummary))
import Data.Aeson (encode, object, (.=))
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List.NonEmpty (NonEmpty ((:|)))
import HarchWeb
  ( AccessRequirement (RequireAuthorized),
    EndpointMetadata,
    EndpointProtocol (ApiEndpoint),
    mkEndpointMetadata,
    requiredEndpointNameOrDie,
    requiredModuleNameOrDie,
    requiredRouteTemplateOrDie,
  )
import HarchWeb.Action (emptyActionCodec)
import HarchWeb.Api
  ( ApiEndpointContract (..),
    ApiEndpointFamily,
    ApiEndpointFamilyError,
    ApiEndpointRequest,
    ApiFieldFailurePolicy (ApiUseGenericFieldFailure),
    ApiMethod (ApiGet),
    ApiRequestBody (ApiNoRequestBody),
    ApiResponse,
    ApiRouteEndpointDeclaration (..),
    SomeApiRouteEndpoint (..),
    apiContentType,
    apiEndpointFamily,
    apiResponse,
    apiRouteDefinition,
    apiRouteEndpointWithContextNeverFailing,
    at,
    bytesResponseEncoder,
    jsonMediaType,
    noRequestFields,
  )
import HarchWeb.ApplicationModule (ApplicationModule (..))
import HarchWeb.Routing
  ( RouteCodec (..),
    RouteLocation (..),
    RouteMethod (RouteGet),
    RouteParseResult (RouteNotMatched, RouteParsed),
    RouteRequest (..),
    pathSegmentText,
    routeMethodPolicy,
    routePathSegments,
  )
import HarchWeb.Site (RouteDefinition)

-- | The API module's declared routes. @CatalogItems@ is the local fragment
-- @/items@; the composed root's mount chain yields the full trusted template
-- @/api/catalog/items@.
-- | Uninhabited: the API module ships 'emptyActionCodec', so no client
-- action can ever be produced. The module mount's embedders are total
-- matches on an empty type.
data CatalogApiActionTarget

data CatalogApiAction

data CatalogApiRoute
  = CatalogItems
  deriving (Eq, Show)

-- | The contract parameterized by the generic extension: a GET with no
-- request fields or body and a JSON response, shaped exactly like web-api's
-- anonymous status contract.
catalogItemsApiContract ::
  extension () () ByteString.ByteString ->
  ApiEndpointContract extension () () ByteString.ByteString
catalogItemsApiContract =
  ApiEndpointContract
    ApiGet
    noRequestFields
    ApiNoRequestBody
    (bytesResponseEncoder (apiContentType jsonMediaType) :| [])
    ApiUseGenericFieldFailure

-- | The endpoint: derive typed JSON from the domain's summary port.
catalogItemsApiHandler ::
  extension () () ByteString.ByteString ->
  CatalogQueries ->
  CatalogContext ->
  ApiEndpointRequest () () ->
  IO (ApiResponse ByteString.ByteString)
catalogItemsApiHandler _extension queries context _endpointRequest = do
  summary <- loadCatalogSummary queries context
  pure (apiResponse (LazyByteString.toStrict (encode (object ["summary" .= summary]))))

-- | Build the @catalog.api@ module. Supplying an extension value keeps the
-- constructor generic over the documented/undocumented assembly.
buildCatalogApiModule ::
  extension () () ByteString.ByteString ->
  CatalogQueries ->
  ApplicationModule CatalogApiRoute CatalogApiActionTarget CatalogApiAction CatalogContext CatalogPolicy
buildCatalogApiModule extension queries =
  ApplicationModule
    { moduleName = requiredModuleNameOrDie "catalog.api",
      moduleOwnsRoute = \case CatalogItems -> True,
      moduleRouteMountChain = const (requiredModuleNameOrDie "catalog.api" :| []),
      moduleRouteCodec = catalogApiRouteCodec,
      moduleDeclaredRoutes = [CatalogItems],
      moduleEndpoints = catalogItemsRouteDefinition extension queries,
      moduleActionCodec = emptyActionCodec,
      moduleActionRoute = \_ _ -> Nothing,
      moduleHandleAction = \_ -> pure Nothing,
      moduleGuards = []
    }

catalogApiRouteCodec :: RouteCodec CatalogApiRoute CatalogContext
catalogApiRouteCodec =
  RouteCodec
    { parseRoute = \requestContext location ->
        case routePathSegments location of
          [segment] | pathSegmentText segment == "items" -> RouteParsed (RouteRequest CatalogItems requestContext)
          _ -> RouteNotMatched,
      renderRoute = const (RouteLocation [] []),
      notFoundRequest = RouteRequest CatalogItems,
      routeMethods = const (routeMethodPolicy [RouteGet])
    }

-- | The endpoint as a typed family member: the same value the module's
-- route definition consumes and the composed root aggregates into its
-- documented family.
-- Per docs/design-guidance.md's never-mask-a-gate-finding rule: the @$!@ on
-- 'extension' and 'queries' below is a confirmed, reproducible fix, not a guess. The tests
-- execute this endpoint's handler through the route definition (real
-- execution, asserted end to end), but 'queries' is a bare local binding
-- used as a direct argument to an already-HPC-instrumented call, the
-- documented pattern where HPC permanently leaves the occurrence unticked
-- despite real execution.
{-# ANN catalogItemsApiEndpoint ("HLint: ignore Redundant $!" :: String) #-}
catalogItemsApiEndpoint ::
  extension () () ByteString.ByteString ->
  CatalogQueries ->
  SomeApiRouteEndpoint CatalogContext extension
catalogItemsApiEndpoint extension queries =
  SomeApiRouteEndpoint
    (apiRouteEndpointWithContextNeverFailing (ApiRouteEndpointDeclaration (at "/items") (catalogItemsApiContract extension)) ((catalogItemsApiHandler $! extension) $! queries))

-- | The catalog API's endpoint family for the composed root's document.
catalogItemsFamily ::
  extension () () ByteString.ByteString ->
  CatalogQueries ->
  Either HarchWeb.Api.ApiEndpointFamilyError (HarchWeb.Api.ApiEndpointFamily CatalogContext extension)
{-# ANN catalogItemsFamily ("HLint: ignore Redundant $!" :: String) #-}
catalogItemsFamily extension queries =
  apiEndpointFamily [catalogItemsApiEndpoint extension $! queries]

catalogItemsRouteDefinition ::
  extension () () ByteString.ByteString ->
  CatalogQueries ->
  CatalogApiRoute ->
  RouteDefinition CatalogApiRoute CatalogContext CatalogPolicy
catalogItemsRouteDefinition extension queries CatalogItems =
  case catalogItemsApiEndpoint extension queries of
    SomeApiRouteEndpoint endpoint -> apiRouteDefinition catalogItemsEndpointMetadata endpoint

catalogItemsEndpointMetadata :: EndpointMetadata CatalogPolicy
catalogItemsEndpointMetadata =
  mkEndpointMetadata
    (requiredEndpointNameOrDie "catalog.items")
    (requiredRouteTemplateOrDie "/items")
    ApiEndpoint
    (RequireAuthorized MayReadCatalog)
