{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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
import qualified Data.Text as Text
import HarchWeb
  ( Application,
    Response (..),
    ResponseBody (..),
    ListenerConfig (..),
    ListenerScheme (..),
    ObservabilityConfig (..),
    RequestPolicyConfig (..),
    ServerConfig (..),
    StaticAssetsConfig (..),
    defaultCorsPolicyConfig,
    defaultResponseSecurityHeadersConfig,
    defaultStaticAssetContentTypes,
  )
import HarchWeb.Site
  ( Site (..),
    SiteRoute (..),
    buildSiteApplication,
    pageSiteRoute,
    simpleSite,
  )
import qualified Language.Haskell.TH.Syntax as TH

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
        navigationScriptRoute,
        pageSiteRoute NotFoundRoute Nothing notFoundPage
      ]
  )
    { siteStaticAssets = twoPageStaticAssets,
      siteRequestPolicy = twoPageRequestPolicy
    }

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
    { staticAssetRoots = [],
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

navigationScriptRoute :: SiteRoute TwoPageRoute ()
navigationScriptRoute =
  SiteRoute
    { siteRouteValue = NavigationScriptRoute,
      siteRouteNavigationLabel = Nothing,
      siteRouteResponse =
        \_ ->
          pure
            ( BodyResponse
                ResponseBody
                  { responseStatus = 200,
                    responseContentType = "application/javascript; charset=utf-8",
                    responseBody = navigationScriptBody,
                    responseObservabilityAttributes = [],
                    responseLogEntries = []
                  }
            )
    }

navigationScriptBody :: Text.Text
navigationScriptBody =
  Text.pack
    $( do
         navigationPath <- TH.makeRelativeToProject "public/navigation.js"
         TH.addDependentFile navigationPath
         navigationSource <- TH.runIO (readFile navigationPath)
         TH.lift navigationSource
     )
