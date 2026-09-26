{-# LANGUAGE OverloadedStrings #-}

-- | The composed root's one merged OpenAPI document (AHI-4E): both domain
-- API families aggregated through the exact mounts that install them for
-- runtime routing, with each operation's security derived from the root's
-- real authorization projection and scope mapping. The packages own only
-- abstract policy; issuer, audience, and scheme names stay here.
module App.Composed.Document
  ( requireOpenApiExtension,
    composedCatalogItemsExtension,
    composedOrdersSubmitExtension,
    composedOpenApiDocumentDetails,
    composedOpenApiSecuritySchemes,
    composedEndpointMetadataForPath,
    composedAuthorizationScopes,
    composedCatalogMountedFamily,
    composedOrdersMountedFamily,
    composedOpenApiDocumentProvider,
  )
where

import App.Composed.Model (ComposedContext, RootAuthorization (..))
import App.Composed.Mounts (catalogApiRootMount, ordersApiRootMount)
import Catalog.Api (catalogItemsApiEndpoint)
import Catalog.Domain (CatalogQueries)
import Data.ByteString qualified as ByteString
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import HarchWeb
  ( AccessRequirement (..),
    EndpointMetadata,
    EndpointProtocol (ApiEndpoint),
    mkEndpointMetadata,
    requiredEndpointNameOrDie,
    requiredRouteTemplateOrDie,
    withAuthenticationProfile,
  )
import HarchWeb.Api (ApiForm, ApiPath, apiPathText, hoistApiEndpointFamily, requireApiEndpointFamily)
import HarchWeb.ApplicationModule (ContextProjection (..), mountedContext, mountedRoutes)
import HarchWeb.EndpointSecurity (AuthenticationProfileName, requiredAuthenticationProfileNameOrDie)
import HarchWeb.OpenApi
  ( OpenApiDocumentDetails (..),
    OpenApiDocumentFailure,
    OpenApiDocumentProvider,
    OpenApiExtension,
    OpenApiExtensionError,
    OpenApiMountedFamily,
    OpenApiSecurityScheme,
    mkCachedOpenApiDocumentProvider,
    mkOpenApiExtension,
    mkOpenApiHttpBearerSecurityScheme,
    openApiMountedFamily,
  )
import Orders.Api (SubmitOrderCommand, ordersApiEndpoint)
import Orders.Domain (OrdersCommands)

composedOpenApiDocumentDetails :: OpenApiDocumentDetails
composedOpenApiDocumentDetails =
  OpenApiDocumentDetails
    { openApiDocumentTitle = "Composed Domains API",
      openApiDocumentVersion = "0.1.0.0"
    }

-- | The composed root's documented security scheme: one bearer scheme for
-- the API-audience tokens (the web-audience account cookie is deliberately
-- not a documented API credential).
composedOpenApiSecuritySchemes :: Map AuthenticationProfileName OpenApiSecurityScheme
composedOpenApiSecuritySchemes =
  Map.fromList
    [ ( requiredAuthenticationProfileNameOrDie "composed-api-bearer",
        mkOpenApiHttpBearerSecurityScheme (Just "JWT")
      )
    ]

-- | The one documented bearer profile: the metadata links every API
-- operation to it so the security mapping resolves exactly one scheme
-- (there is no independent docs security override).
composedApiProfile :: EndpointMetadata RootAuthorization -> EndpointMetadata RootAuthorization
composedApiProfile = withAuthenticationProfile (requiredAuthenticationProfileNameOrDie "composed-api-bearer")

-- | Each documented operation's real metadata: the root-composed template
-- and the root's authorization projection, re-projected by the family-local
-- declaration path 'openApiMountedFamily' supplies (mirroring web-api's
-- resolver), so the documented values are exactly what the runtime route
-- definitions carry after their mount projection.
-- Per docs/design-guidance.md's never-mask-a-gate-finding rule: the @$!@
-- forms in this module are confirmed, reproducible fixes for the documented
-- HPC pattern where directly passed bindings and literals stay unticked
-- despite real execution (proved end to end by the composed WAI tests and
-- the direct error-rail test).
{-# ANN composedEndpointMetadataForPath ("HLint: ignore Redundant $!" :: String) #-}

{-# ANN composedCatalogMountedFamily ("HLint: ignore Redundant $!" :: String) #-}

{-# ANN composedOrdersMountedFamily ("HLint: ignore Redundant $!" :: String) #-}

{-# ANN composedCatalogItemsExtension ("HLint: ignore Redundant $!" :: String) #-}

{-# ANN composedOrdersSubmitExtension ("HLint: ignore Redundant $!" :: String) #-}

{-# ANN composedOpenApiDocumentProvider ("HLint: ignore Redundant $!" :: String) #-}

composedEndpointMetadataForPath :: ApiPath -> EndpointMetadata RootAuthorization
composedEndpointMetadataForPath apiPath =
  case apiPathText apiPath of
    "/items" ->
      composedApiProfile
        ((((mkEndpointMetadata $! requiredEndpointNameOrDie "root.catalog.api.catalog.items") $! requiredRouteTemplateOrDie "/api/catalog/items") $! ApiEndpoint) $! RequireAuthorized RootMayReadCatalog)
    "/" ->
      composedApiProfile
        ((((mkEndpointMetadata $! requiredEndpointNameOrDie "root.orders.api.orders.submit") $! requiredRouteTemplateOrDie "/api/orders") $! ApiEndpoint) $! RequireAuthorized RootMaySubmitOrders)
    _ ->
      -- An authored-composition defect: the document may only ask about
      -- paths its mounted families actually declare.
      error ("composed-domains documentation asked for an undocumented path: " <> show (apiPathText apiPath))

-- | The composed root's authored documentation extensions for the two
-- documented operations (AHI-4E): one combined document with separate
-- Catalog/Orders tags. The domain packages see only the generic extension
-- parameter.
composedCatalogItemsExtension :: OpenApiExtension () () ByteString.ByteString
composedCatalogItemsExtension =
  requireOpenApiExtension
    (((((mkOpenApiExtension $! Just "List the catalog summary.") $! Nothing) $! ["Catalog"]) $! False) $! [])

composedOrdersSubmitExtension :: OpenApiExtension SubmitOrderCommand ApiForm ByteString.ByteString
composedOrdersSubmitExtension =
  requireOpenApiExtension
    (((((mkOpenApiExtension $! Just "Submit one order.") $! Nothing) $! ["Orders"]) $! False) $! [])

-- | Unwrap one statically authored documentation extension; a failure here
-- is a composition-time defect in authored literals.
requireOpenApiExtension :: Either OpenApiExtensionError (OpenApiExtension fields body response) -> OpenApiExtension fields body response
requireOpenApiExtension = either (error . ("composed-domains authored an invalid OpenAPI extension: " <>) . show) id

composedAuthorizationScopes :: RootAuthorization -> [Text]
composedAuthorizationScopes authorization =
  case authorization of
    RootMayReadCatalog -> ["catalog:read"]
    RootMayRefreshCatalog -> ["catalog:write"]
    RootMayReadOrders -> ["orders:read"]
    RootMaySubmitOrders -> ["orders:write"]

composedCatalogMountedFamily :: CatalogQueries -> OpenApiMountedFamily ComposedContext
composedCatalogMountedFamily composedCatalogQueriesValue =
  let mount = catalogApiRootMount
      queries = composedCatalogQueriesValue
      extension = composedCatalogItemsExtension
   in case mountedContext $! mount of
        ContextProjection projectCatalogContext ->
          let project = projectCatalogContext
           in ((((openApiMountedFamily $! mountedRoutes mount) $! ((hoistApiEndpointFamily $! project) $! requireApiEndpointFamily [(catalogItemsApiEndpoint $! extension) $! queries])) $! composedEndpointMetadataForPath) $! composedAuthorizationScopes)

composedOrdersMountedFamily :: OrdersCommands -> OpenApiMountedFamily ComposedContext
composedOrdersMountedFamily composedOrdersCommandsValue =
  let mount = ordersApiRootMount
      commands = composedOrdersCommandsValue
      extension = composedOrdersSubmitExtension
   in case mountedContext $! mount of
        ContextProjection projectOrdersContext ->
          let project = projectOrdersContext
           in ((((openApiMountedFamily $! mountedRoutes mount) $! ((hoistApiEndpointFamily $! project) $! requireApiEndpointFamily [(ordersApiEndpoint $! extension) $! commands])) $! composedEndpointMetadataForPath) $! composedAuthorizationScopes)

composedOpenApiDocumentProvider ::
  ComposedContext -> CatalogQueries -> OrdersCommands -> Either OpenApiDocumentFailure (OpenApiDocumentProvider ComposedContext)
composedOpenApiDocumentProvider composedDefaultContext composedCatalogQueriesValue composedOrdersCommandsValue =
  mkCachedOpenApiDocumentProvider
    composedOpenApiDocumentDetails
    composedOpenApiSecuritySchemes
    composedDefaultContext
    [ composedCatalogMountedFamily $! composedCatalogQueriesValue,
      composedOrdersMountedFamily $! composedOrdersCommandsValue
    ]
