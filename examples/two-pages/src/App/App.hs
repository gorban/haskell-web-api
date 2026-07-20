{-# LANGUAGE OverloadedStrings #-}

module App.App
  ( buildApplication,
    twoPageServerConfig,
    twoPageSite,
  )
where

import App.Components.Layout (twoPageShell)
import App.Pages.Home (homePage)
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
    RegionPatch (..),
    RequestPolicyConfig (..),
    ServerConfig (..),
    StaticAssetRoot (..),
    StaticAssetsConfig (..),
    defaultCorsPolicyConfig,
    defaultResponseSecurityHeadersConfig,
    defaultStaticAssetContentTypes,
  )
import HarchWeb.Site
  ( Site (..),
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
        pageSiteRoute NotFoundRoute Nothing notFoundPage
      ]
  )
    { siteStaticAssets = twoPageStaticAssets,
      siteRequestPolicy = twoPageRequestPolicy,
      siteHandleClientAction = twoPageClientAction
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
                        clientActionPatches = [RegionPatch "subscription-result" "<p id=\"subscription-result\" data-harch-region=\"true\" role=\"status\">Thanks. Your subscription request is ready.</p>"],
                        clientActionFocusId = Nothing,
                        clientActionHeaders = []
                      }
              _ ->
                ClientActionResponse
                  { clientActionStatus = 422,
                    clientActionPatches = [RegionPatch "subscription-result" "<p id=\"subscription-result\" data-harch-region=\"true\" role=\"alert\">Enter a valid email address.</p>"],
                    clientActionFocusId = Just "subscription-email",
                    clientActionHeaders = []
                  }
          )
      _ -> Nothing

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
