{-# LANGUAGE OverloadedStrings #-}

module HarchWeb.Site
  ( Site (..),
    SiteRoute (..),
    buildSiteApplication,
    pageSiteRoute,
    simpleSite,
  )
where

import Data.List (find)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import HarchWeb
  ( Application (..),
    NavigationItem (..),
    Page,
    PageShell (..),
    RequestPolicyConfig (..),
    Response (..),
    RouteCodec,
    RouteRequest,
    StaticAssetRoot,
    StaticAssetsConfig (..),
    StrictTransportSecurityConfig,
    buildPageShell,
    defaultCorsPolicyConfig,
    defaultResponseSecurityHeadersConfig,
  )
import HarchWeb qualified
import HarchWeb.Observability qualified as Observability
import Network.Wai qualified as Wai

data SiteRoute route context = SiteRoute
  { siteRouteValue :: route,
    siteRouteNavigationLabel :: Maybe Text,
    siteRouteResponse :: RouteRequest route context -> IO (Response route context)
  }

data Site route context = Site
  { siteName :: Text,
    siteDefaultRequestContext :: context,
    siteRequestContextFromRequest :: Wai.Request -> context -> context,
    siteStaticAssets :: StaticAssetsConfig,
    siteRequestPolicy :: RequestPolicyConfig,
    siteRouteCodec :: RouteCodec route context,
    siteRoutes :: [SiteRoute route context],
    sitePageShell :: Page route context -> PageShell route context,
    siteReportRequestObservability :: Observability.RequestObservability -> IO (),
    siteReportConnectionObservability :: Observability.ConnectionObservability -> IO (),
    siteReportApplicationLog :: Text -> IO ()
  }

simpleSite ::
  Text ->
  context ->
  RouteCodec route context ->
  (Page route context -> PageShell route context) ->
  [SiteRoute route context] ->
  Site route context
simpleSite name defaultContext codec shellBuilder routeDefinitions =
  Site
    { siteName = name,
      siteDefaultRequestContext = defaultContext,
      siteRequestContextFromRequest = \_ requestContext -> requestContext,
      siteStaticAssets = emptyStaticAssetsConfig,
      siteRequestPolicy = defaultSiteRequestPolicy,
      siteRouteCodec = codec,
      siteRoutes = routeDefinitions,
      sitePageShell = shellBuilder,
      siteReportRequestObservability = \requestObservability ->
        Observability.forceRequestObservability requestObservability `seq` pure (),
      siteReportConnectionObservability = \connectionObservability ->
        Observability.forceConnectionObservability connectionObservability `seq` pure (),
      siteReportApplicationLog = \logEntry ->
        logEntry `seq` pure ()
    }

pageSiteRoute ::
  route ->
  Maybe Text ->
  (RouteRequest route context -> IO (Page route context)) ->
  SiteRoute route context
pageSiteRoute route navigationLabel renderPage =
  SiteRoute
    { siteRouteValue = route,
      siteRouteNavigationLabel = navigationLabel,
      siteRouteResponse = fmap PageResponse . renderPage
    }

buildSiteApplication :: (Eq route, Show route) => Site route context -> Application route context
buildSiteApplication site =
  HarchWeb.application
    Application
      { appName = siteName site,
        defaultRequestContext = siteDefaultRequestContext site,
        requestContextFromRequest = siteRequestContextFromRequest site,
        applicationStaticAssets = siteStaticAssets site,
        applicationRequestPolicy = siteRequestPolicy site,
        routeCodec = siteRouteCodec site,
        renderResponse = renderSiteResponse site,
        pageShell = renderSitePageShell site,
        reportRequestObservability = siteReportRequestObservability site,
        reportConnectionObservability = siteReportConnectionObservability site,
        reportApplicationLog = siteReportApplicationLog site
      }

renderSiteResponse :: (Eq route, Show route) => Site route context -> RouteRequest route context -> IO (Response route context)
renderSiteResponse site routeRequest =
  case resolveSiteRoute site (HarchWeb.requestRoute routeRequest) of
    Just routeDefinition ->
      siteRouteResponse routeDefinition routeRequest
    Nothing ->
      ioError
        ( userError
            ( "No site route configured for matched route: "
                <> show (HarchWeb.requestRoute routeRequest)
            )
        )

renderSitePageShell :: (Eq route) => Site route context -> Page route context -> Text
renderSitePageShell site page =
  buildPageShell
    (siteRouteCodec site)
    ( addRouteNavigation
        (siteNavigationItems site)
        (sitePageShell site page)
    )
    page

resolveSiteRoute :: (Eq route) => Site route context -> route -> Maybe (SiteRoute route context)
resolveSiteRoute site routeValue =
  find (\routeDefinition -> siteRouteValue routeDefinition == routeValue) (siteRoutes site)

siteNavigationItems :: Site route context -> [NavigationItem route]
siteNavigationItems site =
  mapMaybe
    ( \routeDefinition ->
        fmap
          ( \navigationLabel ->
              NavigationItem
                { navigationLabel = navigationLabel,
                  navigationRoute = siteRouteValue routeDefinition
                }
          )
          (siteRouteNavigationLabel routeDefinition)
    )
    (siteRoutes site)

addRouteNavigation :: [NavigationItem route] -> PageShell route context -> PageShell route context
addRouteNavigation generatedNavigation shell =
  shell
    { shellNavigationItems =
        generatedNavigation <> shellNavigationItems shell
    }

emptyStaticAssetsConfig :: StaticAssetsConfig
emptyStaticAssetsConfig =
  StaticAssetsConfig
    { staticAssetRoots = [] :: [StaticAssetRoot],
      staticAssetContentTypes = HarchWeb.defaultStaticAssetContentTypes,
      staticCacheControlSeconds = Nothing
    }

defaultSiteRequestPolicy :: RequestPolicyConfig
defaultSiteRequestPolicy =
  RequestPolicyConfig
    { redirectHttpToHttps = False,
      httpsRedirectPort = Nothing,
      strictTransportSecurity = Nothing :: Maybe StrictTransportSecurityConfig,
      trustForwardedHeaders = False,
      corsPolicy = defaultCorsPolicyConfig,
      responseSecurityHeaders = defaultResponseSecurityHeadersConfig
    }
