{-# LANGUAGE OverloadedStrings #-}

module App.App
  ( buildApplication,
    twoPageServerConfig,
    twoPageSite,
    TwoPageAction (..),
  )
where

import App.Components.Layout (twoPageShell)
import App.Components.SubscriptionEmailField (subscriptionEmailId)
import App.CustomPages.Preview (previewPageDefinition)
import App.Pages.Generated (pageRouteDefinition)
import App.Pages.Home (nativeSubscriptionResultPage, subscriptionResultRegion)
import App.Pages.Route.Generated (PageRoute (..))
import App.Routes
  ( ApiRoute (..),
    CustomRoute (..),
    TwoPageAction (..),
    TwoPageRoute (..),
    routeCodec,
    twoPageActionEndpointMetadata,
    twoPageActions,
    twoPageEndpointMetadata,
  )
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text qualified as Text
import HarchWeb
  ( ActionNavigation (NavigateInternal, StayOnCurrentRoute),
    Application,
    ApplicationSecurity (AuthenticationDisabled),
    ClientActionRequest (..),
    ClientActionResponse (..),
    CsrfProtection,
    CsrfVerification (..),
    EndpointProtocol (ApiEndpoint, HtmlEndpoint),
    ForwardedHeaderTrust (..),
    HistoryMode (PushHistory),
    ListenerConfig (..),
    ListenerScheme (..),
    NonPageResponse (..),
    ObservabilityConfig (..),
    RegionPatch,
    RequestBodyReadFailure (..),
    RequestPolicyConfig (..),
    ResponseBody (..),
    RouteMethod (..),
    RouteRequest (..),
    ServerConfig (..),
    ServerSentEvent (..),
    StaticAssetRoot (..),
    StaticAssetsConfig (..),
    defaultCorsPolicyConfig,
    defaultResponseSecurityHeadersConfig,
    defaultStaticAssetContentTypes,
    eventStreamResponse,
    nonPageInternalRedirectResponse,
    parseClientActionFields,
    readRequestBodyUpTo,
    replaceRegion,
    serverSentEventSourceFromList,
    unboundedRequestHeadLimits,
    unboundedRouteExecutionPolicy,
    validateActionCsrfTransport,
    verifyCsrfToken,
    warpDefaultRequestTransportLimits,
  )
import HarchWeb.Action (decodeAction)
import HarchWeb.Site
  ( RouteDefinition (..),
    RouteHandler (ProtocolRouteHandler),
    SimpleSiteConfiguration (..),
    Site (..),
    buildSiteApplication,
    simpleSite,
  )
import HarchWeb.Site qualified as Site
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai

-- | Compose the example with an application-selected CSRF authority. The
-- example deliberately does not compile a signing secret into its library;
-- its executable supplies an ephemeral development authority and a real
-- deployment can inject a configured rotating keyring instead.
buildApplication :: CsrfProtection () -> Application TwoPageRoute TwoPageAction () ()
buildApplication csrfProtection = buildSiteApplication (twoPageSite csrfProtection)

twoPageSite :: CsrfProtection () -> Site TwoPageRoute TwoPageAction () ()
twoPageSite csrfProtection =
  ( simpleSite
      SimpleSiteConfiguration
        { simpleSiteName = "two-pages-example",
          simpleSiteDefaultRequestContext = (),
          simpleSiteRouteCodec = routeCodec,
          simpleSiteSecurity = AuthenticationDisabled [],
          simpleSiteCsrfProtection = csrfProtection,
          simpleSitePageShell = twoPageShell,
          simpleSiteNavigationRoutes = [Page HomePage, Page SecondPage, Page LiveDataPage],
          simpleSiteRouteDefinition = routeDefinition csrfProtection
        }
  )
    { siteStaticAssets = twoPageStaticAssets,
      siteRequestPolicy = twoPageRequestPolicy,
      siteDecodeClientAction = decodeAction twoPageActions,
      siteClientActionEndpointMetadata = twoPageActionEndpointMetadata,
      siteHandleClientAction = twoPageClientAction
    }

routeDefinition :: CsrfProtection () -> TwoPageRoute -> RouteDefinition TwoPageRoute () ()
routeDefinition csrfProtection route =
  case route of
    Page PageNotFound -> (pageRouteDefinition PageNotFound) {routeMethods = []}
    Page page -> pageRouteDefinition page
    Api LiveDataEvents -> liveDataEventsRouteDefinition
    Custom (PreviewPage previewSlug) -> previewPageDefinition previewSlug
    Custom NativeSubscriptionFallback ->
      nativeSubscriptionFallbackRouteDefinition csrfProtection
    Custom NativeSubscriptionResult ->
      Site.pageRoute (twoPageEndpointMetadata HtmlEndpoint (Custom NativeSubscriptionResult)) Nothing nativeSubscriptionResultPage

liveDataEventsRouteDefinition :: RouteDefinition TwoPageRoute () ()
liveDataEventsRouteDefinition =
  RouteDefinition
    { routeNavigationLabel = Nothing,
      routeMetadata = twoPageEndpointMetadata ApiEndpoint (Api LiveDataEvents),
      routeMethods = [RouteGet],
      routeExecutionPolicy = unboundedRouteExecutionPolicy,
      routeHandler = ProtocolRouteHandler $ \_ _ -> do
        eventSource <-
          serverSentEventSourceFromList
            [ServerSentEvent (Just "update") (Just "example-1") "The live update arrived."]
        pure (eventStreamResponse eventSource)
    }

twoPageClientAction :: ClientActionRequest TwoPageAction () -> IO (Maybe (ClientActionResponse TwoPageRoute ()))
twoPageClientAction actionRequest =
  pure $
    case clientAction actionRequest of
      SubscribeAction emailAddress ->
        Just
          ( case emailAddress of
              value
                | validSubscriptionEmail value ->
                    ClientActionResponse
                      { clientActionStatus = Http.status200,
                        clientActionPatches = subscriptionPatch "status" "Thanks. Your subscription request is ready.",
                        clientActionFocusId = Nothing,
                        clientActionNavigation = NavigateInternal PushHistory (RouteRequest (Custom NativeSubscriptionResult) ()),
                        clientActionHeaders = [],
                        clientActionObservabilityAttributes = [],
                        clientActionLogEntries = []
                      }
              _ ->
                ClientActionResponse
                  { clientActionStatus = Http.status422,
                    clientActionPatches = subscriptionPatch "alert" "Enter a valid email address.",
                    clientActionFocusId = Just subscriptionEmailId,
                    clientActionNavigation = StayOnCurrentRoute,
                    clientActionHeaders = [],
                    clientActionObservabilityAttributes = [],
                    clientActionLogEntries = []
                  }
          )

nativeSubscriptionFallbackRouteDefinition :: CsrfProtection () -> RouteDefinition TwoPageRoute () ()
nativeSubscriptionFallbackRouteDefinition csrfProtection =
  RouteDefinition
    { routeNavigationLabel = Nothing,
      routeMetadata = twoPageEndpointMetadata ApiEndpoint (Custom NativeSubscriptionFallback),
      routeMethods = [RoutePost],
      routeExecutionPolicy = unboundedRouteExecutionPolicy,
      routeHandler = ProtocolRouteHandler (nativeSubscriptionFallbackHandler csrfProtection)
    }

nativeSubscriptionFallbackHandler :: CsrfProtection () -> Wai.Request -> RouteRequest TwoPageRoute () -> IO (NonPageResponse TwoPageRoute ())
nativeSubscriptionFallbackHandler csrfProtection request routeRequest = do
  requestBodyResult <- readRequestBodyUpTo nativeFallbackBodyBytes request
  case requestBodyResult of
    Left RequestBodyLimitExceeded -> pure nativeFallbackBodyTooLarge
    Right requestBody
      | nativeFallbackFieldCountExceedsLimit requestBody -> pure nativeFallbackTooManyFields
      | otherwise ->
          case parseClientActionFields requestBody of
            Left _ -> pure nativeFallbackCsrfRejected
            Right formFields ->
              case validateActionCsrfTransport request formFields of
                Left _ -> pure nativeFallbackCsrfRejected
                Right csrfToken -> do
                  verification <- verifyCsrfToken csrfProtection (requestContext routeRequest) csrfToken
                  pure $
                    case verification of
                      CsrfVerified ->
                        case lookup "email" formFields of
                          Just emailAddress
                            | validSubscriptionEmail emailAddress ->
                                nonPageInternalRedirectResponse Http.status303 (RouteRequest (Custom NativeSubscriptionResult) (requestContext routeRequest))
                          _ -> nativeFallbackInvalidEmail
                      CsrfRejected -> nativeFallbackCsrfRejected
                      CsrfVerificationUnavailable -> nativeFallbackCsrfUnavailable

nativeFallbackBodyBytes :: Int
nativeFallbackBodyBytes = 8192

-- | This native form only needs its CSRF field and ordinary form inputs.
-- Keep its decoded query representation bounded independently of the body
-- budget so a small body containing many empty fields cannot amplify into a
-- large list before CSRF validation.
nativeFallbackFieldCountLimit :: Int
nativeFallbackFieldCountLimit = 32

nativeFallbackBodyTooLarge :: NonPageResponse TwoPageRoute ()
nativeFallbackBodyTooLarge =
  NonPageBodyResponse
    ResponseBody
      { responseStatus = Http.status413,
        responseContentType = "text/plain; charset=utf-8",
        responseBody = "Native fallback request body is too large.",
        responseObservabilityAttributes = [],
        responseLogEntries = [],
        responseDatabaseOperations = []
      }

nativeFallbackTooManyFields :: NonPageResponse TwoPageRoute ()
nativeFallbackTooManyFields =
  NonPageBodyResponse
    ResponseBody
      { responseStatus = Http.status413,
        responseContentType = "text/plain; charset=utf-8",
        responseBody = "Native fallback request has too many form fields.",
        responseObservabilityAttributes = [],
        responseLogEntries = [],
        responseDatabaseOperations = []
      }

nativeFallbackCsrfRejected :: NonPageResponse TwoPageRoute ()
nativeFallbackCsrfRejected =
  NonPageBodyResponse
    ResponseBody
      { responseStatus = Http.status403,
        responseContentType = "text/plain; charset=utf-8",
        responseBody = "Native fallback CSRF validation failed.",
        responseObservabilityAttributes = [],
        responseLogEntries = [],
        responseDatabaseOperations = []
      }

nativeFallbackCsrfUnavailable :: NonPageResponse TwoPageRoute ()
nativeFallbackCsrfUnavailable =
  NonPageBodyResponse
    ResponseBody
      { responseStatus = Http.status503,
        responseContentType = "text/plain; charset=utf-8",
        responseBody = "Native fallback CSRF protection is unavailable.",
        responseObservabilityAttributes = [],
        responseLogEntries = [],
        responseDatabaseOperations = []
      }

nativeFallbackInvalidEmail :: NonPageResponse TwoPageRoute ()
nativeFallbackInvalidEmail =
  NonPageBodyResponse
    ResponseBody
      { responseStatus = Http.status422,
        responseContentType = "text/plain; charset=utf-8",
        responseBody = "Enter a valid email address.",
        responseObservabilityAttributes = [],
        responseLogEntries = [],
        responseDatabaseOperations = []
      }

nativeFallbackFieldCountExceedsLimit :: LazyByteString.ByteString -> Bool
nativeFallbackFieldCountExceedsLimit requestBody =
  nativeFallbackFieldCount requestBody > nativeFallbackFieldCountLimit

nativeFallbackFieldCount :: LazyByteString.ByteString -> Int
nativeFallbackFieldCount requestBody =
  if LazyByteString.null requestBody
    then 0
    else 1 + sum (map (ByteString.count 38) (LazyByteString.toChunks requestBody))

validSubscriptionEmail :: Text.Text -> Bool
validSubscriptionEmail value =
  "@" `Text.isInfixOf` value && "." `Text.isInfixOf` value

subscriptionPatch :: Text.Text -> Text.Text -> [RegionPatch]
subscriptionPatch liveRole message =
  [replaceRegion (subscriptionResultRegion liveRole message)]

twoPageServerConfig :: ServerConfig
twoPageServerConfig =
  ServerConfig
    { listenerConfigs =
        [ ListenerConfig
            { listenerHost = "127.0.0.1",
              listenerPort = 8080,
              listenerScheme = Http,
              listenerTls = Nothing,
              listenerAcme = Nothing
            }
        ],
      staticAssets = twoPageStaticAssets,
      requestPolicy = twoPageRequestPolicy,
      observability =
        ObservabilityConfig
          { tracingExporter = Nothing,
            metricsExporter = Nothing
          }
    }

twoPageStaticAssets :: StaticAssetsConfig
twoPageStaticAssets =
  StaticAssetsConfig
    { staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = "/assets", staticDirectory = "public"}],
      staticAssetContentTypes = defaultStaticAssetContentTypes,
      staticCacheControlSeconds = Nothing
    }

twoPageRequestPolicy :: RequestPolicyConfig
twoPageRequestPolicy =
  RequestPolicyConfig
    { redirectHttpToHttps = False,
      httpsRedirectPort = Nothing,
      httpsRedirectAuthority = Nothing,
      strictTransportSecurity = Nothing,
      forwardedHeaderTrust = NeverTrustForwarded,
      requestHeadLimits = unboundedRequestHeadLimits,
      requestTransportLimits = warpDefaultRequestTransportLimits,
      requestConcurrencyLimit = Nothing,
      corsPolicy = defaultCorsPolicyConfig,
      responseSecurityHeaders = defaultResponseSecurityHeadersConfig
    }
