{-# LANGUAGE OverloadedStrings #-}

module App.App
  ( buildApplication,
    twoPageServerConfig,
    twoPageSite,
  )
where

import App.Components.Layout (twoPageShell)
import App.Pages.Home (homePage)
import App.Pages.LiveData (liveDataPage)
import App.Pages.NotFound (notFoundPage)
import App.Pages.Second (secondPage)
import App.Routes (TwoPageRoute (..), routeCodec)
import Data.Text qualified as Text
import HarchWeb
  ( Application,
    ClientActionRequest (..),
    ClientActionResponse (..),
    ListenerConfig (..),
    ListenerScheme (..),
    ObservabilityConfig (..),
    RegionPatch,
    RequestPolicyConfig (..),
    ServerConfig (..),
    ServerSentEvent (..),
    StaticAssetRoot (..),
    StaticAssetsConfig (..),
    defaultCorsPolicyConfig,
    defaultResponseSecurityHeadersConfig,
    defaultStaticAssetContentTypes,
    eventStreamResponse,
    mkElementId,
    mkRegionId,
    paragraphTag,
    region,
    replaceRegion,
    role,
    serverSentEventSourceFromList,
    text,
  )
import HarchWeb.Site
  ( Site (..),
    SiteRoute (..),
    buildSiteApplication,
    pageSiteRoute,
    simpleSite,
  )

buildApplication :: Application TwoPageRoute ()
buildApplication = buildSiteApplication twoPageSite

twoPageSite :: Site TwoPageRoute ()
twoPageSite =
  ( simpleSite
      "two-pages-example"
      ()
      routeCodec
      twoPageShell
      [ pageSiteRoute HomeRoute (Just "Home") homePage,
        pageSiteRoute SecondRoute (Just "Second") secondPage,
        pageSiteRoute LiveDataRoute (Just "Live updates") liveDataPage,
        liveDataEventsRoute,
        pageSiteRoute NotFoundRoute Nothing notFoundPage
      ]
  )
    { siteStaticAssets = twoPageStaticAssets,
      siteRequestPolicy = twoPageRequestPolicy,
      siteHandleClientAction = twoPageClientAction
    }

liveDataEventsRoute :: SiteRoute TwoPageRoute ()
liveDataEventsRoute =
  SiteRoute
    { siteRouteValue = LiveDataEventsRoute,
      siteRouteNavigationLabel = Nothing,
      siteRouteResponse = \_ -> do
        eventSource <-
          serverSentEventSourceFromList
            [ServerSentEvent (Just "update") (Just "example-1") "The live update arrived."]
        pure (eventStreamResponse eventSource)
    }

twoPageClientAction :: ClientActionRequest () -> IO (Maybe ClientActionResponse)
twoPageClientAction actionRequest =
  pure $
    case (clientActionMethod actionRequest, clientActionPath actionRequest) of
      ("POST", "/actions/subscribe") ->
        Just
          ( case lookup "email" (clientActionFields actionRequest) of
              Just emailAddress
                | "@" `Text.isInfixOf` emailAddress,
                  "." `Text.isInfixOf` emailAddress ->
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
      _ -> Nothing

subscriptionPatch :: Text.Text -> Text.Text -> [RegionPatch]
subscriptionPatch liveRole message =
  maybe []
    (\identifier -> [replaceRegion (region (mkRegionId identifier) paragraphTag [role liveRole] [text message])])
    (mkElementId "subscription-result")

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
