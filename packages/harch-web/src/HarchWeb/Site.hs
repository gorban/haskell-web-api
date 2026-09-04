{-# LANGUAGE OverloadedStrings #-}

-- | Declarative site composition.
--
-- FQ8 makes the stable route-table, shell, and CSRF declaration inputs one
-- 'SimpleSiteConfiguration'. Dynamic policy, middleware, action, and
-- reporter customizations stay on 'Site' itself. The default disabled
-- reporters remain ordinary no-op observer policy, not strictness or
-- coverage-only callbacks.
module HarchWeb.Site
  ( RouteDefinition (..),
    RouteHandler (..),
    Site (..),
    SimpleSiteConfiguration (..),
    apiOnlySite,
    buildSiteApplication,
    pageRoute,
    routeResponse,
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
    NonPageResponse,
    Page,
    PageResult (..),
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
import HarchWeb.Csrf (CsrfPagePreparationFailure (..), CsrfProtection, PageSecurity, csrfProtectionUnavailable, preparePageSecurity)
import HarchWeb.Document qualified as Document
import HarchWeb.Observability qualified as Observability
import HarchWeb.Server.ClientAction (csrfCookieFromRequest)
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai

-- | A matched route is either an SSR page or a protocol endpoint.  Only the
-- page alternative receives the security state that must be authored into
-- its markup; protocol endpoints cannot accidentally receive a nonce/token
-- which has no valid meaning for them.
data RouteHandler route context
  = PageRouteHandler (PageSecurity -> RouteRequest route context -> IO (PageResult route context))
  | ProtocolRouteHandler (Wai.Request -> RouteRequest route context -> IO (NonPageResponse route context))

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
    routeHandler :: RouteHandler route context
  }

-- | Transitional direct invocation helper.  Protocol tests and adapters can
-- exercise their handler without inventing page state.  A page must instead
-- travel through 'buildSiteApplication', which constructs its security value
-- before the page handler runs.
routeResponse :: RouteDefinition route context authorization -> Wai.Request -> RouteRequest route context -> IO (Response route context)
routeResponse routeDefinition request routeRequest =
  case routeHandler routeDefinition of
    ProtocolRouteHandler renderProtocol -> HarchWeb.nonPageResponse <$> renderProtocol request routeRequest
    PageRouteHandler _ -> error "direct page response execution requires pre-render PageSecurity"

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
    siteClientActionRoute :: Text -> Text -> context -> Maybe route,
    siteDecodeClientAction :: ClientActionPayload context -> ClientActionDecodeResult action,
    siteCsrfProtection :: CsrfProtection context,
    siteHandleClientAction :: ClientActionRequest action context -> IO (Maybe (ClientActionResponse route context)),
    sitePageShell :: Page route context -> PageShell route context,
    siteReportRequestObservability :: Observability.RequestObservability -> IO (),
    siteReportConnectionObservability :: Observability.ConnectionObservability -> IO (),
    siteReportApplicationLog :: Text -> IO ()
  }

-- | The stable declaration inputs for 'simpleSite'.  Request middleware,
-- assets, policies, client actions, and reporters remain deliberate
-- subsequent 'Site' customizations; grouping only the route-table and shell
-- declaration prevents those values being transposed at every simple
-- composition root. CSRF is deliberately required here: a page-capable site
-- must select an authority rather than inherit a verifier which accepts every
-- token. This extends the existing site composition boundary instead of
-- creating a second CSRF dispatcher; see the AHI-4C decision record in
-- @docs/design-guidance.md@.
data SimpleSiteConfiguration route context authorization = SimpleSiteConfiguration
  { simpleSiteName :: Text,
    simpleSiteDefaultRequestContext :: context,
    simpleSiteRouteCodec :: RouteCodec route context,
    simpleSiteSecurity :: ApplicationSecurity route context authorization,
    simpleSiteCsrfProtection :: CsrfProtection context,
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
      siteClientActionRoute = \_ _ _ -> Nothing,
      siteDecodeClientAction = const HarchWeb.UnrecognizedClientAction,
      siteCsrfProtection = simpleSiteCsrfProtection configuration,
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
          simpleSiteCsrfProtection = csrfProtectionUnavailable,
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
  (PageSecurity -> RouteRequest route context -> IO (Page route context)) ->
  RouteDefinition route context authorization
pageRoute metadata navigationLabel renderPage =
  RouteDefinition
    { routeNavigationLabel = navigationLabel,
      routeMetadata = metadata,
      routeMethods = [HarchWeb.RouteGet],
      routeExecutionPolicy = unboundedRouteExecutionPolicy,
      routeHandler = PageRouteHandler $ \pageSecurity routeRequest ->
        RenderedPage <$> renderPage pageSecurity routeRequest
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
        clientActionRoute = siteClientActionRoute site,
        HarchWeb.routeExecutionPolicy = routeDefinitionExecutionPolicy . siteRouteDefinition site,
        renderRequestResponse = renderSiteResponse site,
        decodeClientAction = siteDecodeClientAction site,
        csrfProtection = siteCsrfProtection site,
        handleClientAction = siteHandleClientAction site,
        pageShell = renderSitePageShell site,
        reportRequestObservability = siteReportRequestObservability site,
        reportConnectionObservability = siteReportConnectionObservability site,
        reportApplicationLog = siteReportApplicationLog site
      }

renderSiteResponse :: Site route action context authorization -> Wai.Request -> RouteRequest route context -> IO (Response route context)
renderSiteResponse site request routeRequest =
  case routeHandler (siteRouteDefinition site (HarchWeb.requestRoute routeRequest)) of
    PageRouteHandler renderPage -> do
      preparedPageSecurity <- preparePageSecurity (siteCsrfProtection site) (csrfCookieFromRequest request) (HarchWeb.requestContext routeRequest)
      case preparedPageSecurity of
        Left CsrfPageProtectionUnavailable -> pure csrfUnavailableResponse
        Right pageSecurity -> do
          pageResult <- renderPage pageSecurity routeRequest
          pure $
            case pageResult of
              RenderedPage page -> PageResponse pageSecurity page
              RenderedPageWithMetadata responseBodyValue page -> PageResponseWithMetadata pageSecurity responseBodyValue page
    ProtocolRouteHandler renderProtocol -> HarchWeb.nonPageResponse <$> renderProtocol request routeRequest

csrfUnavailableResponse :: Response route context
csrfUnavailableResponse =
  BodyResponse
    HarchWeb.ResponseBody
      { HarchWeb.responseStatus = Http.status503,
        HarchWeb.responseContentType = "text/plain; charset=utf-8",
        HarchWeb.responseBody = "CSRF protection is unavailable.",
        HarchWeb.responseObservabilityAttributes = [],
        HarchWeb.responseLogEntries = [],
        HarchWeb.responseDatabaseOperations = []
      }

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
