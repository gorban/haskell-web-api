{-# LANGUAGE OverloadedStrings #-}

-- | Interpret web-api's existing endpoint declarations as one cached
-- OpenAPI document. Endpoint handlers and their typed contracts remain owned
-- by 'WebApi.Api.Endpoints'; this module owns only family mounting,
-- documentation metadata, security projection, and the document route.
--
-- The mount value comes from 'WebApi.Api.Mount' because endpoint declarations
-- also use that same path fact to derive family-local operation paths. This
-- keeps one route prefix without making declarations depend on their
-- documentation interpreter.
module WebApi.Api.OpenApiDocs
  ( webApiApiMountPrefix,
    webApiApiMountPrefixText,
    webApiOpenApiSecuritySchemes,
    webApiOpenApiEndpointMetadataForPath,
    webApiApiRouteMount,
    appAuthorizationScopes,
    webApiOpenApiDocumentDetails,
    webApiOpenApiDocumentProvider,
    webApiOpenApiMountedFamily,
    requireWebApiOpenApiDocumentProvider,
    docsOpenApiSpecRouteDefinition,
  )
where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import HarchWeb.Api (ApiPath, apiPathText, requireApiEndpointFamily)
import HarchWeb.ApplicationModule (RouteMount (..))
import HarchWeb.Authentication (ScopeRequirement (RequireAllScopes, RequireAnyScope), oauth2ScopeText)
import HarchWeb.EndpointSecurity (AuthenticationProfileName, EndpointMetadata (endpointRouteTemplate), routeTemplateText)
import HarchWeb.OpenApi
  ( OpenApiDocumentDetails (..),
    OpenApiDocumentFailure,
    OpenApiDocumentProvider,
    OpenApiMountedFamily,
    OpenApiSecurityScheme,
    mkCachedOpenApiDocumentProvider,
    mkOpenApiHttpBearerSecurityScheme,
    openApiDocumentRouteDefinition,
    openApiMountedFamily,
    renderOpenApiDocumentFailure,
  )
import HarchWeb.SecurityEvent (requiredModuleNameOrDie)
import HarchWeb.Site (RouteDefinition)
import WebApi.Account (AccountProfileStore)
import WebApi.Api.Endpoints (meApiEndpoint, secondApiEndpoint, statusApiEndpoint, tokenApiEndpoint)
import WebApi.Api.Mount (webApiApiMountPrefix, webApiApiMountPrefixText)
import WebApi.ApiClientToken (ApiClientTokenEnvironment)
import WebApi.Database (PageRepository)
import WebApi.Route
  ( AppAuthorization,
    AppRequestContext,
    AppRoute (DocsOpenApiSpecRoute, MeApiRoute, SecondApiRoute, StatusApiRoute, TokenApiRoute),
    accountAuthenticationProfileName,
    defaultRequestContext,
    endpointMetadata,
    resourceAuthenticationProfileName,
  )

-- | The closed set of routes whose endpoints are documented. Keeping route
-- identities next to their metadata interpreter means a newly documented
-- endpoint cannot be silently dropped from the served document.
webApiDocumentedRoutes :: [AppRoute]
webApiDocumentedRoutes = [StatusApiRoute, SecondApiRoute, MeApiRoute, TokenApiRoute]

-- | The structural mount web-api's documented family is recorded under. The
-- prism is the identity on this closed route type: web-api composes no child
-- module at runtime, so the prefix records the existing paths without
-- installing a second dispatcher.
webApiApiRouteMount :: RouteMount AppRoute AppRoute
webApiApiRouteMount =
  RouteMount
    { routeMountName = requiredModuleNameOrDie "web-api",
      routeMountPrefix = webApiApiMountPrefix,
      embedChildRoute = id,
      projectChildRoute = Just
    }

-- | Resolve a family-local declaration path back to the real endpoint
-- metadata used by runtime dispatch. An unknown family path is an authored
-- table defect and fails document construction at startup.
webApiOpenApiEndpointMetadataForPath :: ApiPath -> EndpointMetadata AppAuthorization
webApiOpenApiEndpointMetadataForPath apiPath =
  case [ metadata
       | route <- webApiDocumentedRoutes,
         let metadata = endpointMetadata route,
         routeTemplateText (endpointRouteTemplate metadata) == webApiApiMountPrefixText <> apiPathText apiPath
       ] of
    metadata : _ -> metadata
    [] ->
      error
        ( "web-api documents no API endpoint at family path "
            <> Text.unpack (webApiApiMountPrefixText <> apiPathText apiPath)
        )

-- | Project a real authorization value into the OpenAPI scope names it
-- demands. All-versus-any enforcement stays in the runtime guard.
appAuthorizationScopes :: AppAuthorization -> [Text.Text]
appAuthorizationScopes scopeRequirement =
  case scopeRequirement of
    RequireAllScopes scopes -> oauth2ScopeText <$> NonEmpty.toList scopes
    RequireAnyScope scopes -> oauth2ScopeText <$> NonEmpty.toList scopes

-- | Map the exact authentication profiles used by route dispatch to their
-- documented HTTP bearer scheme. The account profile's cookie-or-bearer
-- union remains an application transport detail that one OpenAPI scheme
-- cannot express.
webApiOpenApiSecuritySchemes :: Map AuthenticationProfileName OpenApiSecurityScheme
webApiOpenApiSecuritySchemes =
  Map.fromList
    [ (accountAuthenticationProfileName, jwtBearerScheme),
      (resourceAuthenticationProfileName, jwtBearerScheme)
    ]
  where
    jwtBearerScheme = mkOpenApiHttpBearerSecurityScheme (Just "JWT")

-- | Document identity follows the shipped @haskell-web-api@ package version.
webApiOpenApiDocumentDetails :: OpenApiDocumentDetails
webApiOpenApiDocumentDetails =
  OpenApiDocumentDetails
    { openApiDocumentTitle = "Harch Web API",
      openApiDocumentVersion = "0.1.2.0"
    }

-- | Build the startup-cached provider from one availability snapshot; document
-- construction and encoding happen once, before the server accepts requests.
webApiOpenApiDocumentProvider :: PageRepository -> AccountProfileStore -> ApiClientTokenEnvironment -> Either OpenApiDocumentFailure (OpenApiDocumentProvider AppRequestContext)
webApiOpenApiDocumentProvider pageRepository profileStore tokenEnvironment =
  mkCachedOpenApiDocumentProvider
    webApiOpenApiDocumentDetails
    webApiOpenApiSecuritySchemes
    defaultRequestContext
    [webApiOpenApiMountedFamily pageRepository profileStore tokenEnvironment]

-- | Aggregate the exact endpoint values used by runtime route definitions.
-- Security metadata is resolved from the same route declarations and
-- authorization values that dispatch enforces.
webApiOpenApiMountedFamily :: PageRepository -> AccountProfileStore -> ApiClientTokenEnvironment -> OpenApiMountedFamily AppRequestContext
webApiOpenApiMountedFamily pageRepository profileStore tokenEnvironment =
  openApiMountedFamily
    webApiApiRouteMount
    ( requireApiEndpointFamily
        [ statusApiEndpoint,
          secondApiEndpoint pageRepository,
          meApiEndpoint profileStore,
          tokenApiEndpoint tokenEnvironment
        ]
    )
    webApiOpenApiEndpointMetadataForPath
    appAuthorizationScopes

-- | Name a typed construction failure at application startup, rather than
-- allowing the first request to discover an invalid authored document.
requireWebApiOpenApiDocumentProvider :: Either OpenApiDocumentFailure (OpenApiDocumentProvider context) -> OpenApiDocumentProvider context
requireWebApiOpenApiDocumentProvider =
  either (error . Text.unpack . renderOpenApiDocumentFailure) id

-- | The typed @GET /docs/openapi.json@ route over the startup-validated
-- provider. Its access policy still comes from this route's own metadata.
docsOpenApiSpecRouteDefinition :: OpenApiDocumentProvider AppRequestContext -> RouteDefinition AppRoute AppRequestContext AppAuthorization
docsOpenApiSpecRouteDefinition =
  openApiDocumentRouteDefinition (endpointMetadata DocsOpenApiSpecRoute)
