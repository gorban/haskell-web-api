{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Anonymous public routes and typed static-asset ownership for the composed
-- root.  Filesystem delivery remains in HarchWeb's single asset interpreter;
-- this module only declares root-owned routing and endpoint metadata.
module App.Composed.Public
  ( buildPublicModule,
  )
where

import App.Composed.Model
import Data.List.NonEmpty qualified as NonEmpty
import HarchWeb.Action (emptyActionCodec)
import HarchWeb.ApplicationModule (ApplicationModule (..))
import HarchWeb.Document (Page (..))
import HarchWeb.EndpointMetadata
  ( AccessRequirement (AllowUnauthenticated),
    EndpointProtocol (AssetEndpoint, HtmlEndpoint),
    mkEndpointMetadata,
    requiredEndpointNameOrDie,
    requiredRouteTemplateOrDie,
  )
import HarchWeb.Markup (element, headingOneTag, text)
import HarchWeb.Routing
  ( RouteCodec (..),
    RouteLocation (..),
    RouteMethodPolicy (RouteHidden),
    RouteRequest (..),
    mapRouteParseResult,
    pathSegmentText,
    requiredPathSegment,
  )
import HarchWeb.Routing qualified as Routing
import HarchWeb.SecurityEvent (requiredModuleNameOrDie)
import HarchWeb.Server (Response (..), unboundedRouteExecutionPolicy)
import HarchWeb.Site (RouteDefinition (..))
import HarchWeb.StaticAssets (StaticAssetsConfig)
import HarchWeb.StaticAssets.Route
  ( StaticAssetRoute (..),
    staticAssetRouteCodec,
    staticAssetRouteResponse,
  )

buildPublicModule :: StaticAssetsConfig -> ApplicationModule LocalizedRoute RootActionTarget RootAction ComposedContext RootAuthorization
buildPublicModule staticAssetsConfig =
  ApplicationModule
    { moduleName = requiredModuleNameOrDie "root.public",
      moduleOwnsRoute = \case
        Public _ -> True
        _ -> False,
      moduleRouteMountChain = const (requiredModuleNameOrDie "root.public" NonEmpty.:| [requiredModuleNameOrDie "public"]),
      moduleRouteCodec = publicRouteCodec staticAssetsConfig,
      moduleDeclaredRoutes =
        [ Public PublicLogin,
          Public (PublicAsset (StaticAssetRoute [requiredPathSegment "public", requiredPathSegment "assets", requiredPathSegment "app.css"])),
          Public PublicNotFound
        ],
      moduleEndpoints = publicRouteDefinition staticAssetsConfig,
      moduleActionCodec = emptyActionCodec,
      moduleHandleAction = const (pure Nothing),
      moduleGuards = []
    }

publicRouteCodec :: StaticAssetsConfig -> RouteCodec LocalizedRoute ComposedContext
publicRouteCodec staticAssetsConfig =
  RouteCodec
    { parseRoute = \requestContext location ->
        case routePathSegments location of
          [publicSegment, loginSegment]
            | pathSegmentText publicSegment == "public", pathSegmentText loginSegment == "login" -> Routing.RouteParsed (RouteRequest (Public PublicLogin) requestContext)
          _ -> mapAssetRoute requestContext location,
      renderRoute = \routeRequest ->
        case requestRoute routeRequest of
          Public PublicLogin -> RouteLocation [requiredPathSegment "public", requiredPathSegment "login"] []
          Public (PublicAsset assetRoute) -> RouteLocation (staticAssetPathSegments assetRoute) []
          _ -> error "attempted to render a non-public route through the public module",
      notFoundRequest = RouteRequest (Public PublicNotFound),
      routeMethods = \case
        Public PublicLogin -> Routing.routeMethodPolicy [Routing.RouteGet]
        Public (PublicAsset _) -> Routing.routeMethodPolicy [Routing.RouteGet]
        Public PublicNotFound -> RouteHidden
        _ -> RouteHidden
    }
  where
    staticCodec = staticAssetRouteCodec staticAssetsConfig
    mapAssetRoute requestContext location =
      mapRouteParseResult (Public . PublicAsset) (parseRoute staticCodec requestContext location)

publicRouteDefinition :: StaticAssetsConfig -> LocalizedRoute -> RouteDefinition LocalizedRoute ComposedContext RootAuthorization
publicRouteDefinition staticAssetsConfig routeValue =
  case routeValue of
    Public PublicLogin ->
      RouteDefinition
        { routeNavigationLabel = Just "Login",
          routeMetadata = mkEndpointMetadata (requiredEndpointNameOrDie "root.public.login") (requiredRouteTemplateOrDie "/public/login") HtmlEndpoint AllowUnauthenticated,
          routeMethods = [Routing.RouteGet],
          routeExecutionPolicy = unboundedRouteExecutionPolicy,
          routeResponse = \_ request ->
            pure
              ( PageResponse
                  Page
                    { pageTitle = "Login",
                      pageRoute = Public PublicLogin,
                      pageContext = requestContext request,
                      pageBody = element headingOneTag [] [text "Login"],
                      pageBootstrapHooks = []
                    }
              )
        }
    Public (PublicAsset assetRoute) ->
      RouteDefinition
        { routeNavigationLabel = Nothing,
          routeMetadata = mkEndpointMetadata (requiredEndpointNameOrDie "root.public.assets") (requiredRouteTemplateOrDie "/public/assets/*") AssetEndpoint AllowUnauthenticated,
          routeMethods = [Routing.RouteGet],
          routeExecutionPolicy = unboundedRouteExecutionPolicy,
          routeResponse = \request _ ->
            ProtocolResponseResult <$> staticAssetRouteResponse staticAssetsConfig request assetRoute
        }
    Public PublicNotFound ->
      RouteDefinition
        { routeNavigationLabel = Nothing,
          routeMetadata = mkEndpointMetadata (requiredEndpointNameOrDie "root.public.not-found") (requiredRouteTemplateOrDie "/public/404") HtmlEndpoint AllowUnauthenticated,
          routeMethods = [],
          routeExecutionPolicy = unboundedRouteExecutionPolicy,
          routeResponse = \_ request ->
            pure
              ( PageResponse
                  Page
                    { pageTitle = "Not Found",
                      pageRoute = Public PublicNotFound,
                      pageContext = requestContext request,
                      pageBody = element headingOneTag [] [text "Not Found"],
                      pageBootstrapHooks = []
                    }
              )
        }
    _ -> error "attempted to select a non-public route through the public module"
