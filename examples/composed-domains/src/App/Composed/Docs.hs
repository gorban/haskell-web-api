{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The composed root's documentation surface (AHI-4E): the typed
-- specification route at @/docs/openapi.json@ serving the one merged
-- document, and the Swagger page at @/docs@ - both ordinary typed routes
-- mounted at the root outside the locale wrapper, exactly like the API
-- subtree, so the documented templates stay locale-free.
module App.Composed.Docs
  ( buildDocsModule,
  )
where

import App.Composed.Model (ComposedContext, DocsAction, DocsActionTarget, DocsRoute (..), RootAuthorization)
import Data.List.NonEmpty (NonEmpty ((:|)))
import HarchWeb
  ( AccessRequirement (AllowUnauthenticated),
    EndpointMetadata,
    EndpointProtocol (ApiEndpoint, HtmlEndpoint),
    Page (..),
    RouteMethod (RouteGet),
    RouteRequest (..),
    mkEndpointMetadata,
    requiredEndpointNameOrDie,
    requiredModuleNameOrDie,
    requiredPathSegment,
    requiredRouteTemplateOrDie,
    routeMethodPolicy,
  )
import HarchWeb.Action (emptyActionCodec)
import HarchWeb.ApplicationModule (ApplicationModule (..))
import HarchWeb.OpenApi
  ( OpenApiDocumentProvider,
    openApiDocumentRouteDefinition,
  )
import HarchWeb.OpenApi.Swagger (defaultSwaggerUiProps, swaggerUiPage, swaggerUiStylesheet)
import HarchWeb.Routing
  ( RouteCodec (..),
    RouteLocation (..),
    RouteParseResult (RouteNotMatched, RouteParsed),
    pathSegmentText,
    routePathSegments,
  )
import HarchWeb.Site (RouteDefinition)
import HarchWeb.Site qualified as Site

buildDocsModule ::
  OpenApiDocumentProvider ComposedContext ->
  ApplicationModule DocsRoute DocsActionTarget DocsAction ComposedContext RootAuthorization
buildDocsModule provider =
  ApplicationModule
    { moduleName = requiredModuleNameOrDie "docs",
      moduleOwnsRoute = const True,
      moduleRouteMountChain = const (requiredModuleNameOrDie "docs" :| []),
      moduleRouteCodec = docsRouteCodec,
      moduleDeclaredRoutes = [DocsSpec, DocsUi],
      moduleEndpoints = \case
        DocsSpec -> openApiDocumentRouteDefinition docsSpecMetadata provider
        DocsUi -> docsUiRouteDefinition,
      moduleActionCodec = emptyActionCodec,
      moduleActionRoute = \_ _ -> Nothing,
      moduleHandleAction = \_ -> pure Nothing,
      moduleGuards = []
    }

docsRouteCodec :: RouteCodec DocsRoute ComposedContext
docsRouteCodec =
  RouteCodec
    { parseRoute = \requestContext location ->
        case routePathSegments location of
          [] -> RouteParsed (RouteRequest DocsUi requestContext)
          [segment] | pathSegmentText segment == "openapi.json" -> RouteParsed (RouteRequest DocsSpec requestContext)
          _ -> RouteNotMatched,
      renderRoute = \request ->
        case requestRoute request of
          DocsUi -> RouteLocation [] []
          DocsSpec -> RouteLocation (pure (requiredPathSegment "openapi.json")) [],
      notFoundRequest = RouteRequest DocsUi,
      routeMethods = const (routeMethodPolicy [RouteGet])
    }

docsSpecMetadata :: EndpointMetadata RootAuthorization
docsSpecMetadata =
  mkEndpointMetadata
    (requiredEndpointNameOrDie "docs.openapi-spec")
    (requiredRouteTemplateOrDie "/openapi.json")
    ApiEndpoint
    AllowUnauthenticated

docsUiMetadata :: EndpointMetadata RootAuthorization
docsUiMetadata =
  mkEndpointMetadata
    (requiredEndpointNameOrDie "docs.swagger")
    (requiredRouteTemplateOrDie "/")
    HtmlEndpoint
    AllowUnauthenticated

-- | The Swagger page as an ordinary typed application surface: complete SSR
-- with the script-free fallback and the enhancement mount, its own scoped
-- stylesheet carried on the page.
docsUiRouteDefinition :: RouteDefinition DocsRoute ComposedContext RootAuthorization
docsUiRouteDefinition =
  Site.pageRoute
    docsUiMetadata
    Nothing
    ( \_security request ->
        let props = defaultSwaggerUiProps (requestRoute request) (requestContext request)
         in pure
              ( (swaggerUiPage props)
                  { pageStylesheets = [swaggerUiStylesheet props],
                    pageBootstrapHooks = ["composed-docs"]
                  }
              )
    )
