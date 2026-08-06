{-# LANGUAGE OverloadedStrings #-}

module App.App
  ( buildApplication,
    buildNativeUploadMiddleware,
    twoPageServerConfig,
    twoPageSite,
    TwoPageAction (..),
  )
where

import App.Components.Layout (twoPageShell)
import App.CustomPages.Preview (previewPageDefinition)
import App.NativeUpload qualified as NativeUpload
import App.Pages.Generated (pageRouteDefinition)
import App.Pages.Home (nativeSubscriptionFallbackPage, subscriptionResultRegion)
import App.Pages.Route.Generated (PageRoute (..))
import App.Routes
  ( ApiRoute (..),
    CustomRoute (..),
    TwoPageAction (..),
    TwoPageRoute (..),
    routeCodec,
    twoPageActions,
  )
import Control.Monad (join)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text qualified as Text
import HarchWeb
  ( Application,
    ClientActionRequest (..),
    ClientActionResponse (..),
    ListenerConfig (..),
    ListenerScheme (..),
    MiddlewareResult (..),
    ObservabilityConfig (..),
    RegionPatch,
    RequestMiddleware (..),
    RequestPolicyConfig (..),
    ResponseBody (..),
    ServerConfig (..),
    ServerSentEvent (..),
    StaticAssetRoot (..),
    StaticAssetsConfig (..),
    defaultCorsPolicyConfig,
    defaultResponseSecurityHeadersConfig,
    defaultStaticAssetContentTypes,
    eventStreamResponse,
    replaceRegion,
    serverSentEventSourceFromList,
  )
import HarchWeb.Action (decodeAction)
import HarchWeb.Api qualified as Api
import HarchWeb.Site
  ( RouteDefinition (..),
    Site (..),
    buildSiteApplication,
    simpleSite,
  )
import HarchWeb.Site qualified as Site
import Network.HTTP.Types.URI qualified as HttpUri
import Network.Wai qualified as Wai

buildApplication :: Application TwoPageRoute TwoPageAction ()
buildApplication = buildSiteApplication twoPageSite

-- | The native file-upload form's dispatch, composed in front of
-- 'buildApplication' via 'HarchWeb.runServerWithWaiMiddleware' (production,
-- see @app/Main.hs@) or 'HarchWeb.withLocalTestServerForApplication' (tests,
-- see @test/E2E/AppSpec.hs@) rather than through 'buildApplication' itself:
-- see "App.NativeUpload" for why. Each call creates its own CSRF state, so
-- callers should build this once per running application, not per request.
buildNativeUploadMiddleware :: IO Wai.Middleware
buildNativeUploadMiddleware =
  Api.apiEndpointMiddleware NativeUpload.nativeUploadEndpoints . NativeUpload.handleNativeUpload
    <$> NativeUpload.newNativeUploadState

twoPageSite :: Site TwoPageRoute TwoPageAction ()
twoPageSite =
  ( simpleSite
      "two-pages-example"
      ()
      routeCodec
      twoPageShell
      [Page HomePage, Page SecondPage, Page LiveDataPage]
      routeDefinition
  )
    { siteStaticAssets = twoPageStaticAssets,
      siteRequestPolicy = twoPageRequestPolicy,
      siteRequestMiddleware = [RequestMiddleware nativeFallbackCsrfMiddleware],
      siteDecodeClientAction = decodeAction twoPageActions,
      siteHandleClientAction = twoPageClientAction
    }

routeDefinition :: TwoPageRoute -> RouteDefinition TwoPageRoute ()
routeDefinition route =
  case route of
    Page page -> pageRouteDefinition page
    Api LiveDataEvents -> liveDataEventsRouteDefinition
    Custom (PreviewPage previewSlug) -> previewPageDefinition previewSlug
    Custom NativeSubscriptionFallback -> Site.pageRoute Nothing nativeSubscriptionFallbackPage

liveDataEventsRouteDefinition :: RouteDefinition TwoPageRoute ()
liveDataEventsRouteDefinition =
  RouteDefinition
    { routeNavigationLabel = Nothing,
      routeResponse = \_ -> do
        eventSource <-
          serverSentEventSourceFromList
            [ServerSentEvent (Just "update") (Just "example-1") "The live update arrived."]
        pure (eventStreamResponse eventSource)
    }

twoPageClientAction :: ClientActionRequest TwoPageAction () -> IO (Maybe ClientActionResponse)
twoPageClientAction actionRequest =
  pure $
    case clientAction actionRequest of
      SubscribeAction emailAddress ->
        Just
          ( case emailAddress of
              value
                | "@" `Text.isInfixOf` value,
                  "." `Text.isInfixOf` value ->
                    ClientActionResponse
                      { clientActionStatus = 200,
                        clientActionPatches = subscriptionPatch "status" "Thanks. Your subscription request is ready.",
                        clientActionFocusId = Nothing,
                        clientActionHeaders = [],
                        clientActionObservabilityAttributes = [],
                        clientActionLogEntries = []
                      }
              _ ->
                ClientActionResponse
                  { clientActionStatus = 422,
                    clientActionPatches = subscriptionPatch "alert" "Enter a valid email address.",
                    clientActionFocusId = Just "subscription-email",
                    clientActionHeaders = [],
                    clientActionObservabilityAttributes = [],
                    clientActionLogEntries = []
                  }
          )

nativeFallbackCsrfMiddleware :: Wai.Request -> () -> IO (MiddlewareResult ())
nativeFallbackCsrfMiddleware request requestContext
  | Wai.requestMethod request == "POST",
    Wai.rawPathInfo request == "/native-subscribe" = do
      requestBody <- Wai.strictRequestBody request
      pure
        ( case (nativeFallbackCsrfToken request, nativeFallbackSubmittedToken requestBody) of
            (Just cookieToken, Just submittedToken)
              | cookieToken == submittedToken -> ContinueMiddleware requestContext
            _ ->
              HaltMiddleware
                requestContext
                ResponseBody
                  { responseStatus = 403,
                    responseContentType = "text/plain; charset=utf-8",
                    responseBody = "Native fallback CSRF validation failed.",
                    responseObservabilityAttributes = [],
                    responseLogEntries = []
                  }
        )
  | otherwise = pure (ContinueMiddleware requestContext)

nativeFallbackCsrfToken :: Wai.Request -> Maybe ByteString.ByteString
nativeFallbackCsrfToken request =
  lookup "harch-native-fallback-csrf" (requestCookies request)

nativeFallbackSubmittedToken :: LazyByteString.ByteString -> Maybe ByteString.ByteString
nativeFallbackSubmittedToken requestBody =
  join (lookup "_harch_csrf" (HttpUri.parseQuery (LazyByteString.toStrict requestBody)))

requestCookies :: Wai.Request -> [(ByteString.ByteString, ByteString.ByteString)]
requestCookies request =
  maybe [] (map parseCookie . ByteString.split 59) (lookup "Cookie" (Wai.requestHeaders request))
  where
    parseCookie cookie =
      let (cookieName, valueWithSeparator) = ByteString.break (== 61) (ByteString.dropWhile (== 32) cookie)
       in (cookieName, ByteString.drop 1 valueWithSeparator)

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
      strictTransportSecurity = Nothing,
      trustForwardedHeaders = False,
      corsPolicy = defaultCorsPolicyConfig,
      responseSecurityHeaders = defaultResponseSecurityHeadersConfig
    }
