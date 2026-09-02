{-# LANGUAGE OverloadedStrings #-}

-- | Declarative site composition.
--
-- FQ8 makes the six stable route-table and shell declaration inputs one
-- 'SimpleSiteConfiguration'. Dynamic policy, middleware, action, and
-- reporter customizations stay on 'Site' itself. The default disabled
-- reporters remain ordinary no-op observer policy, not strictness or
-- coverage-only callbacks.
module HarchWeb.Site
  ( RouteDefinition (..),
    Site (..),
    SimpleSiteConfiguration (..),
    apiOnlySite,
    buildSiteApplication,
    pageRoute,
    simpleSite,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import HarchWeb
  ( Application (..),
    ApplicationSecurity,
    ClientActionDecodeResult,
    ClientActionPayload,
    ClientActionRequest,
    ClientActionResponse,
    Document,
    EndpointMetadata,
    ForwardedHeaderTrust (..),
    NavigationItem (..),
    NavigationRuntime,
    Page,
    PageShell (..),
    PathPrefix,
    RequestMiddleware,
    RequestPolicyConfig (..),
    Response (..),
    RouteCodec,
    RouteExecutionPolicy,
    RouteRequest,
    RuntimeAsset,
    RuntimeDescriptor (..),
    StaticAssetRoot,
    StaticAssetsConfig (..),
    StrictTransportSecurityConfig,
    buildPageShell,
    defaultCaptureKernel,
    defaultCorsPolicyConfig,
    defaultNavigationRuntime,
    defaultResponseSecurityHeadersConfig,
    emptyPathPrefix,
    literalElementId,
    navigationRuntimeScriptSource,
    unboundedRequestHeadLimits,
    unboundedRouteExecutionPolicy,
    warpDefaultRequestTransportLimits,
  )
import HarchWeb qualified
import HarchWeb.Document qualified as Document
import HarchWeb.Observability qualified as Observability
import HarchWeb.Session (CsrfToken, generateCsrfToken)
import Network.Wai qualified as Wai

data RouteDefinition route context authorization = RouteDefinition
  { routeNavigationLabel :: Maybe Text,
    -- | Required typed endpoint metadata. Ordinary page/API/action builders
    -- supply authenticated metadata; a public endpoint must name
    -- 'AllowUnauthenticated' explicitly.
    routeMetadata :: EndpointMetadata authorization,
    -- | The methods this route owns. 'buildSiteApplication' installs this
    -- declaration into the shared route codec, making the site table the
    -- authoritative source for ordinary page and protocol dispatch alike.
    routeMethods :: [HarchWeb.RouteMethod],
    -- | Optional additional admission for this route after the shared
    -- dispatcher has selected its path and method. It cannot alter listener,
    -- request-head, or application-wide policy; see 'RouteExecutionPolicy'.
    routeExecutionPolicy :: RouteExecutionPolicy,
    routeResponse :: Wai.Request -> RouteRequest route context -> IO (Response route context)
  }

data Site route action context authorization = Site
  { siteName :: Text,
    siteDefaultRequestContext :: context,
    siteRequestContextFromRequest :: Wai.Request -> context -> context,
    siteStaticAssets :: StaticAssetsConfig,
    siteNavigationRuntime :: Maybe NavigationRuntime,
    -- | Replaceable behavior modules paired with the shell descriptors that
    -- load them. Declaration order is ownership order for duplicate paths.
    siteRuntimeAssets :: [RuntimeAsset],
    siteNavigationRuntimePathPrefix :: context -> PathPrefix,
    siteRequestPolicy :: RequestPolicyConfig,
    siteRequestMiddleware :: [RequestMiddleware context],
    siteSecurity :: ApplicationSecurity route context authorization,
    siteSecurityEventRoot :: Maybe (HarchWeb.SecurityEventRoot context),
    siteRouteModuleChain :: Maybe (route -> NonEmpty HarchWeb.ModuleName),
    siteAttachRouteObservation :: route -> EndpointMetadata authorization -> context -> context,
    siteRouteCodec :: RouteCodec route context,
    siteNavigationRoutes :: [route],
    siteRouteDefinition :: route -> RouteDefinition route context authorization,
    siteClientActionEndpointMetadata :: Text -> Text -> context -> Maybe (EndpointMetadata authorization),
    siteDecodeClientAction :: ClientActionPayload context -> ClientActionDecodeResult action,
    sitePageCsrfToken :: Page route context -> IO CsrfToken,
    siteAuthorizeClientActionCsrf :: ClientActionRequest action context -> CsrfToken -> IO Bool,
    siteHandleClientAction :: ClientActionRequest action context -> IO (Maybe ClientActionResponse),
    sitePageShell :: Page route context -> PageShell route context,
    siteReportRequestObservability :: Observability.RequestObservability -> IO (),
    siteReportConnectionObservability :: Observability.ConnectionObservability -> IO (),
    siteReportApplicationLog :: Text -> IO ()
  }

-- | The stable declaration inputs for 'simpleSite'.  Request middleware,
-- assets, policies, client actions, and reporters remain deliberate
-- subsequent 'Site' customizations; grouping only the route-table and shell
-- declaration prevents those six values being transposed at every simple
-- composition root.
data SimpleSiteConfiguration route context authorization = SimpleSiteConfiguration
  { simpleSiteName :: Text,
    simpleSiteDefaultRequestContext :: context,
    simpleSiteRouteCodec :: RouteCodec route context,
    simpleSiteSecurity :: ApplicationSecurity route context authorization,
    simpleSitePageShell :: Page route context -> PageShell route context,
    simpleSiteNavigationRoutes :: [route],
    simpleSiteRouteDefinition :: route -> RouteDefinition route context authorization
  }

simpleSite ::
  SimpleSiteConfiguration route context authorization ->
  Site route action context authorization
simpleSite configuration =
  Site
    { siteName = simpleSiteName configuration,
      siteDefaultRequestContext = simpleSiteDefaultRequestContext configuration,
      siteRequestContextFromRequest = \_ requestContext -> requestContext,
      siteStaticAssets = emptyStaticAssetsConfig,
      siteNavigationRuntime = Just defaultNavigationRuntime,
      siteRuntimeAssets = [],
      siteNavigationRuntimePathPrefix = const emptyPathPrefix,
      siteRequestPolicy = defaultSiteRequestPolicy,
      siteRequestMiddleware = [],
      siteSecurity = simpleSiteSecurity configuration,
      siteSecurityEventRoot = Nothing,
      siteRouteModuleChain = Nothing,
      siteAttachRouteObservation = \_ _ requestContext -> requestContext,
      siteRouteCodec = simpleSiteRouteCodec configuration,
      siteNavigationRoutes = simpleSiteNavigationRoutes configuration,
      siteRouteDefinition = simpleSiteRouteDefinition configuration,
      siteClientActionEndpointMetadata = \_ _ _ -> Nothing,
      siteDecodeClientAction = const HarchWeb.UnrecognizedClientAction,
      sitePageCsrfToken = const generateCsrfToken,
      siteAuthorizeClientActionCsrf = \_ _ -> pure True,
      siteHandleClientAction = const (pure Nothing),
      sitePageShell = simpleSitePageShell configuration,
      siteReportRequestObservability = const (pure ()),
      siteReportConnectionObservability = const (pure ()),
      siteReportApplicationLog = const (pure ())
    }

-- | Compose a site whose route table contains protocol endpoints only. It
-- extends the ordinary 'Site' boundary rather than adding a second
-- dispatcher: the supplied codec and definition remain the complete
-- method/path table. API-only sites have no navigation runtime or navigation
-- routes. If a future route accidentally renders a page, the internal shell
-- still renders a minimal complete SSR document instead of failing.
apiOnlySite ::
  Text ->
  context ->
  RouteCodec route context ->
  ApplicationSecurity route context authorization ->
  (route -> RouteDefinition route context authorization) ->
  Site route action context authorization
apiOnlySite name defaultContext codec siteSecurityValue routeDefinition =
  ( simpleSite
      SimpleSiteConfiguration
        { simpleSiteName = name,
          simpleSiteDefaultRequestContext = defaultContext,
          simpleSiteRouteCodec = codec,
          simpleSiteSecurity = siteSecurityValue,
          simpleSitePageShell = const apiOnlyFallbackPageShell,
          simpleSiteNavigationRoutes = [],
          simpleSiteRouteDefinition = routeDefinition
        }
  )
    { siteNavigationRuntime = Nothing
    }

apiOnlyFallbackPageShell :: PageShell route context
apiOnlyFallbackPageShell =
  PageShell
    { shellBodyAttributes = [],
      shellNavigationAttributes = [],
      shellNavigationItems = [],
      shellMainId = literalElementId "main",
      shellMainAttributes = [],
      shellNavigationLifecycle = Nothing,
      shellStylesheets = [],
      shellRuntimeDescriptors = []
    }

pageRoute ::
  EndpointMetadata authorization ->
  Maybe Text ->
  (RouteRequest route context -> IO (Page route context)) ->
  RouteDefinition route context authorization
pageRoute metadata navigationLabel renderPage =
  RouteDefinition
    { routeNavigationLabel = navigationLabel,
      routeMetadata = metadata,
      routeMethods = [HarchWeb.RouteGet],
      routeExecutionPolicy = unboundedRouteExecutionPolicy,
      routeResponse = \_ -> fmap PageResponse . renderPage
    }

buildSiteApplication :: (Eq route) => Site route action context authorization -> Application route action context authorization
buildSiteApplication site =
  HarchWeb.application
    Application
      { appName = siteName site,
        defaultRequestContext = siteDefaultRequestContext site,
        requestContextFromRequest = siteRequestContextFromRequest site,
        applicationNavigationRuntime = siteNavigationRuntime site,
        applicationRuntimeAssets = siteRuntimeAssets site,
        applicationStaticAssets = siteStaticAssets site,
        applicationRequestPolicy = siteRequestPolicy site,
        applicationRequestMiddleware = siteRequestMiddleware site,
        routeCodec = (siteRouteCodec site) {HarchWeb.routeMethods = HarchWeb.routeMethodPolicy . routeMethods . siteRouteDefinition site},
        applicationSecurity = siteSecurity site,
        applicationSecurityEventRoot = siteSecurityEventRoot site,
        applicationRouteModuleChain = siteRouteModuleChain site,
        applicationAttachRouteObservation = siteAttachRouteObservation site,
        routeEndpointMetadata = routeMetadata . siteRouteDefinition site,
        clientActionEndpointMetadata = siteClientActionEndpointMetadata site,
        HarchWeb.routeExecutionPolicy = routeDefinitionExecutionPolicy . siteRouteDefinition site,
        renderRequestResponse = renderSiteResponse site,
        decodeClientAction = siteDecodeClientAction site,
        pageCsrfToken = sitePageCsrfToken site,
        authorizeClientActionCsrf = siteAuthorizeClientActionCsrf site,
        handleClientAction = siteHandleClientAction site,
        pageShell = renderSitePageShell site,
        reportRequestObservability = siteReportRequestObservability site,
        reportConnectionObservability = siteReportConnectionObservability site,
        reportApplicationLog = siteReportApplicationLog site
      }

renderSiteResponse :: Site route action context authorization -> Wai.Request -> RouteRequest route context -> IO (Response route context)
renderSiteResponse site request routeRequest =
  routeResponse
    (siteRouteDefinition site (HarchWeb.requestRoute routeRequest))
    request
    routeRequest

routeDefinitionExecutionPolicy :: RouteDefinition route context authorization -> RouteExecutionPolicy
routeDefinitionExecutionPolicy RouteDefinition {routeExecutionPolicy = executionPolicy} = executionPolicy

renderSitePageShell :: (Eq route) => Site route action context authorization -> Page route context -> Document route
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

siteNavigationItems :: Site route action context authorization -> [NavigationItem route]
siteNavigationItems site =
  mapMaybe
    ( \routeValue ->
        fmap
          ( \navigationLabel ->
              NavigationItem
                { navigationLabel = navigationLabel,
                  navigationRoute = routeValue
                }
          )
          (routeNavigationLabel (siteRouteDefinition site routeValue))
    )
    (siteNavigationRoutes site)

addRouteNavigation :: [NavigationItem route] -> PageShell route context -> PageShell route context
addRouteNavigation generatedNavigation shell =
  shell
    { shellNavigationItems =
        generatedNavigation <> shellNavigationItems shell
    }

addFrameworkShellConventions ::
  Site route action context authorization ->
  Page route context ->
  PageShell route context ->
  PageShell route context
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
  if any ((== name) . Document.attributeName) attributes
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
      httpsRedirectAuthority = Nothing,
      strictTransportSecurity = Nothing :: Maybe StrictTransportSecurityConfig,
      forwardedHeaderTrust = NeverTrustForwarded,
      requestHeadLimits = unboundedRequestHeadLimits,
      requestTransportLimits = warpDefaultRequestTransportLimits,
      requestConcurrencyLimit = Nothing,
      corsPolicy = defaultCorsPolicyConfig,
      responseSecurityHeaders = defaultResponseSecurityHeadersConfig
    }
