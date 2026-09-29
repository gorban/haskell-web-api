-- | The @catalog.api@ application module (the composed-domains slice of the
-- OpenAPI documentation and Swagger UI work):
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
    catalogApiFamily,
    catalogItemsApiContract,
    catalogItemsApiEndpoint,
    catalogItemsApiHandler,
    catalogUnlistedPreviewApiEndpoint,
  )
where

import Catalog.Domain (CatalogContext, CatalogPolicy (MayReadCatalog), CatalogQueries (loadCatalogSummary))
import Data.Aeson (encode, object, (.=))
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List.NonEmpty (NonEmpty ((:|)))
import HarchWeb
  ( AccessRequirement (AllowUnauthenticated, RequireAuthorized),
    EndpointMetadata,
    EndpointProtocol (ApiEndpoint),
    NonPageResponse (NonPageProtocolResponse),
    mkEndpointMetadata,
    requiredEndpointNameOrDie,
    requiredModuleNameOrDie,
    requiredRouteTemplateOrDie,
    unboundedRouteExecutionPolicy,
  )
import HarchWeb.Action (emptyActionCodec)
import HarchWeb.Api
  ( ApiAvailability (ApiHidden),
    ApiEndpointContract (..),
    ApiEndpointFamily,
    ApiEndpointFamilyError,
    ApiEndpointRequest,
    ApiFieldFailurePolicy (ApiUseGenericFieldFailure),
    ApiHttpResponse (..),
    ApiMethod (ApiGet),
    ApiRequestBody (ApiNoRequestBody),
    ApiResponse,
    ApiRouteEndpointDeclaration (..),
    SomeApiRouteEndpoint (..),
    apiContentType,
    apiEndpointFamily,
    apiHttpResponseToProtocolResponse,
    apiResponse,
    apiRouteDefinition,
    apiRouteEndpointWithContextNeverFailing,
    at,
    bytesResponseEncoder,
    jsonMediaType,
    noRequestFields,
    withApiEndpointAvailability,
  )
import HarchWeb.ApplicationModule (ApplicationModule (..))
import HarchWeb.Routing
  ( RouteCodec (..),
    RouteLocation (..),
    RouteMethod (RouteGet),
    RouteParseResult (RouteParsed),
    RouteRequest (..),
    pathSegmentText,
    requiredPathSegment,
    routeMethodPolicy,
    routePathSegments,
  )
import HarchWeb.Site (RouteDefinition (..), RouteHandler (..))
import Network.HTTP.Types qualified as HttpTypes

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
  | CatalogUnlistedPreview
  | -- | The API family's own not-found route (part of the OpenAPI
    -- documentation and Swagger UI work), mirroring web-api's
    -- @ApiNotFound@ convention: the module's codec parses every unmatched
    -- sub-path here so an undeclared @/api/catalog/@ path renders exactly
    -- the representation a hidden endpoint renders — the protocol's empty
    -- 404 — instead of falling through to the root HTML not-found page.
    -- That equality is what makes a hidden endpoint indistinguishable from
    -- undeclared routing at the response surface.
    CatalogApiNotFound
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
      moduleOwnsRoute = const True,
      moduleRouteMountChain = const (requiredModuleNameOrDie "catalog.api" :| []),
      moduleRouteCodec = catalogApiRouteCodec,
      moduleDeclaredRoutes = [CatalogItems, CatalogUnlistedPreview, CatalogApiNotFound],
      moduleEndpoints = catalogRouteDefinition extension queries,
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
          [segment] | pathSegmentText segment == "unlisted-preview" -> RouteParsed (RouteRequest CatalogUnlistedPreview requestContext)
          [] -> RouteParsed (RouteRequest CatalogApiNotFound requestContext)
          (_ : _) -> RouteParsed (RouteRequest CatalogApiNotFound requestContext),
      renderRoute = \routeRequest ->
        case requestRoute routeRequest of
          CatalogItems -> RouteLocation [requiredPathSegment "items"] []
          CatalogUnlistedPreview -> RouteLocation [requiredPathSegment "unlisted-preview"] []
          CatalogApiNotFound -> RouteLocation [requiredPathSegment "404"] [],
      notFoundRequest = RouteRequest CatalogApiNotFound,
      routeMethods = \routeRequest ->
        case requestRoute routeRequest of
          CatalogItems -> routeMethodPolicy [RouteGet]
          CatalogUnlistedPreview -> routeMethodPolicy []
          CatalogApiNotFound -> routeMethodPolicy []
    }

-- | The endpoint as a typed family member: the same value the module's
-- route definition consumes and the composed root aggregates into its
-- documented family.
-- Per docs/design-guidance.md's never-mask-a-gate-finding rule: the @$!@
-- below is a confirmed, reproducible fix, not a guess. This endpoint's
-- handler must never run (that is the hidden-endpoint contract, asserted by
-- the composed WAI probe with a zero-execution counter), so its handler
-- field thunk would otherwise stay unfrozen forever and HPC would leave the
-- handler-construction occurrences unticked despite the construction being
-- genuinely exercised. The strict application freezes the handler at
-- endpoint construction — the same reviewed HPC remedy as
-- 'HarchWeb.OpenApi.Document.Operation.operationForEndpoint' — and the
-- commented tests below drive that construction.
{-# ANN catalogItemsApiEndpoint ("HLint: ignore Redundant $!" :: String) #-}
catalogItemsApiEndpoint ::
  extension () () ByteString.ByteString ->
  CatalogQueries ->
  SomeApiRouteEndpoint CatalogContext extension
catalogItemsApiEndpoint extension queries =
  SomeApiRouteEndpoint
    (apiRouteEndpointWithContextNeverFailing (ApiRouteEndpointDeclaration (at "/items") (catalogItemsApiContract extension)) ((catalogItemsApiHandler $! extension) $! queries))

-- | The unlisted preview endpoint, fixed to 'ApiHidden' unconditionally
-- (the availability slice of the OpenAPI documentation and Swagger UI work).
-- A hidden endpoint is indistinguishable from
-- an undeclared route: availability gates handler execution, method
-- negotiation, and the synthesized @Allow@/@HEAD@/@OPTIONS@ answers before
-- any of them can observe the endpoint, and the OpenAPI provider prunes it
-- from the document. The example deliberately hides nothing dynamically:
-- feature-flag hiding belongs to an application's bounded context snapshot,
-- never to an endpoint.
{-# ANN catalogUnlistedPreviewApiEndpoint ("HLint: ignore Redundant $!" :: String) #-}
catalogUnlistedPreviewApiEndpoint ::
  extension () () ByteString.ByteString ->
  CatalogQueries ->
  SomeApiRouteEndpoint CatalogContext extension
catalogUnlistedPreviewApiEndpoint extension queries =
  SomeApiRouteEndpoint
    ( withApiEndpointAvailability
        ApiHidden
        (apiRouteEndpointWithContextNeverFailing (ApiRouteEndpointDeclaration (at "/unlisted-preview") (catalogItemsApiContract extension)) $! ((catalogItemsApiHandler $! extension) $! queries))
    )

-- | The catalog API's endpoint family for the composed root's document: the
-- documented items endpoint and the hidden preview endpoint, so the
-- provider's availability pruning is proven at the family boundary.
{-# ANN catalogApiFamily ("HLint: ignore Redundant $!" :: String) #-}
catalogApiFamily ::
  extension () () ByteString.ByteString ->
  CatalogQueries ->
  Either HarchWeb.Api.ApiEndpointFamilyError (HarchWeb.Api.ApiEndpointFamily CatalogContext extension)
catalogApiFamily extension queries =
  apiEndpointFamily [catalogItemsApiEndpoint extension $! queries, catalogUnlistedPreviewApiEndpoint extension $! queries]

{-# ANN catalogRouteDefinition ("HLint: ignore Redundant $!" :: String) #-}
catalogRouteDefinition ::
  extension () () ByteString.ByteString ->
  CatalogQueries ->
  CatalogApiRoute ->
  RouteDefinition CatalogApiRoute CatalogContext CatalogPolicy
catalogRouteDefinition extension queries route =
  case route of
    CatalogItems ->
      case (catalogItemsApiEndpoint $! extension) $! queries of
        SomeApiRouteEndpoint endpoint -> apiRouteDefinition catalogItemsEndpointMetadata $! endpoint
    CatalogUnlistedPreview ->
      case (catalogUnlistedPreviewApiEndpoint $! extension) $! queries of
        SomeApiRouteEndpoint endpoint -> apiRouteDefinition catalogUnlistedPreviewEndpointMetadata $! endpoint
    CatalogApiNotFound -> catalogApiNotFoundDefinition

-- | The API family's own not-found representation: exactly the protocol's
-- empty 404 that a hidden endpoint's availability gate renders
-- ('HarchWeb.Api' builds that response for an 'ApiHidden' endpoint), so a
-- hidden endpoint and an undeclared sub-path are byte-identical at the
-- response surface. Mirrors web-api's @ApiNotFound@ route definition, whose
-- handler is likewise its family's 404 representation. The method policy is
-- empty so every method renders this one representation — no @Allow@ line,
-- no 405, no handler execution.
{-# ANN catalogApiNotFoundDefinition ("HLint: ignore Redundant $!" :: String) #-}
catalogApiNotFoundDefinition :: RouteDefinition CatalogApiRoute CatalogContext CatalogPolicy
catalogApiNotFoundDefinition =
  RouteDefinition
    { routeNavigation = const Nothing,
      routeMetadata = catalogApiNotFoundEndpointMetadata,
      routeMethods = const (routeMethodPolicy []),
      routeExecutionPolicy = unboundedRouteExecutionPolicy,
      routeHandler =
        ProtocolRouteHandler
          ( \_ _ ->
              pure
                ( NonPageProtocolResponse
                    (apiHttpResponseToProtocolResponse ((ApiHttpResponse HttpTypes.status404 $! []) $! Nothing))
                )
          )
    }

catalogItemsEndpointMetadata :: EndpointMetadata CatalogPolicy
catalogItemsEndpointMetadata =
  mkEndpointMetadata
    (requiredEndpointNameOrDie "catalog.items")
    (requiredRouteTemplateOrDie "/items")
    ApiEndpoint
    (RequireAuthorized MayReadCatalog)

-- The @$!@ forms here are the same confirmed HPC unticked-occurrence remedy
-- as above (bare constructor/local references as direct arguments), pinned
-- by the module test's metadata and not-found-definition assertions.
{-# ANN catalogUnlistedPreviewEndpointMetadata ("HLint: ignore Redundant $!" :: String) #-}
catalogUnlistedPreviewEndpointMetadata :: EndpointMetadata CatalogPolicy
catalogUnlistedPreviewEndpointMetadata =
  ( mkEndpointMetadata
      (requiredEndpointNameOrDie "catalog.unlisted-preview")
      (requiredRouteTemplateOrDie "/unlisted-preview")
      $! ApiEndpoint
  )
    $! RequireAuthorized MayReadCatalog

{-# ANN catalogApiNotFoundEndpointMetadata ("HLint: ignore Redundant $!" :: String) #-}
catalogApiNotFoundEndpointMetadata :: EndpointMetadata CatalogPolicy
catalogApiNotFoundEndpointMetadata =
  ( mkEndpointMetadata
      (requiredEndpointNameOrDie "catalog.not-found")
      (requiredRouteTemplateOrDie "/404")
      $! ApiEndpoint
  )
    $! AllowUnauthenticated
