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
    ClientActionRequest,
    ClientActionResponse,
    Document,
    NavigationItem (..),
    NavigationRuntime,
    Page,
    PageShell (..),
    RequestMiddleware,
    RequestPolicyConfig (..),
    Response (..),
    RouteCodec,
    RouteRequest,
    RuntimeDescriptor (..),
    StaticAssetRoot,
    StaticAssetsConfig (..),
    StrictTransportSecurityConfig,
    buildPageShell,
    defaultCaptureKernel,
    defaultCorsPolicyConfig,
    defaultNavigationRuntime,
    defaultResponseSecurityHeadersConfig,
    navigationRuntimeScriptSource,
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
    siteNavigationRuntime :: Maybe NavigationRuntime,
    siteNavigationRuntimePathPrefix :: context -> Text,
    siteRequestPolicy :: RequestPolicyConfig,
    siteRequestMiddleware :: [RequestMiddleware context],
    siteRouteCodec :: RouteCodec route context,
    siteRoutes :: [SiteRoute route context],
    siteHandleClientAction :: ClientActionRequest context -> IO (Maybe ClientActionResponse),
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
      siteNavigationRuntime = Just defaultNavigationRuntime,
      siteNavigationRuntimePathPrefix = const "",
      siteRequestPolicy = defaultSiteRequestPolicy,
      siteRequestMiddleware = [],
      siteRouteCodec = codec,
      siteRoutes = routeDefinitions,
      siteHandleClientAction = const (pure Nothing),
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
        applicationNavigationRuntime = siteNavigationRuntime site,
        applicationStaticAssets = siteStaticAssets site,
        applicationRequestPolicy = siteRequestPolicy site,
        applicationRequestMiddleware = siteRequestMiddleware site,
        routeCodec = siteRouteCodec site,
        renderResponse = renderSiteResponse site,
        handleClientAction = siteHandleClientAction site,
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

renderSitePageShell :: (Eq route) => Site route context -> Page route context -> Document route
renderSitePageShell site page =
  buildPageShell
    (siteRouteCodec site)
    ( addFrameworkShellConventions
        site
        page
        ( addRouteNavigation
            (siteNavigationItems site)
            (sitePageShell site page)
        )
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

addFrameworkShellConventions :: Site route context -> Page route context -> PageShell route context -> PageShell route context
addFrameworkShellConventions site page shell =
  shell
    { shellNavigationAttributes =
        ensureAttribute "data-navigation-region" "primary" (shellNavigationAttributes shell),
      shellMainAttributes =
        ensureAttribute "data-navigation-content" "true" (shellMainAttributes shell),
      shellRuntimeDescriptors =
        maybe
          (shellRuntimeDescriptors shell)
          ( \runtime ->
              prependUnique
                defaultCaptureKernel
                ( appendUnique
                    DeferredModule
                      { runtimeDescriptorName = "harch-navigation",
                        runtimeDescriptorSource = navigationRuntimeScriptSource (siteNavigationRuntimePathPrefix site (HarchWeb.pageContext page)) runtime
                      }
                    (shellRuntimeDescriptors shell)
                )
          )
          (siteNavigationRuntime site)
    }

ensureAttribute :: Text -> Text -> [HarchWeb.HtmlAttribute] -> [HarchWeb.HtmlAttribute]
ensureAttribute name value attributes =
  if any ((== name) . HarchWeb.attributeName) attributes
    then attributes
    else
      attributes
        <> [ HarchWeb.HtmlAttribute
               { HarchWeb.attributeName = name,
                 HarchWeb.attributeValue = value
               }
           ]

appendUnique :: (Eq a) => a -> [a] -> [a]
appendUnique value values =
  if value `elem` values
    then values
    else values <> [value]

prependUnique :: (Eq a) => a -> [a] -> [a]
prependUnique value values =
  if value `elem` values
    then values
    else value : values

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
