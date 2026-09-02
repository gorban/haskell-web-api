{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import App.Composed
import Catalog.Domain
import HarchWeb
  ( ListenerConfig (..),
    ListenerScheme (Http),
    ObservabilityConfig (..),
    ServerConfig (..),
    runServer,
  )
import HarchWeb.Site qualified as Site
import Orders.Domain
import System.IO (stdout)

main :: IO ()
main =
  runServer stdout (serverConfig site) (Site.buildSiteApplication site)
  where
    site = buildComposedSite defaultComposedStaticAssets defaultLocalePolicy catalogQueries catalogCommands ordersQueries ordersCommands
    catalogQueries = CatalogQueries (const (pure "Catalog"))
    catalogCommands = CatalogCommands (const (pure "refreshed"))
    ordersQueries = OrdersQueries (const (pure "Orders"))
    ordersCommands = OrdersCommands (const (pure (OrderId "order-1")))

serverConfig :: Site.Site RootRoute RootAction ComposedContext RootAuthorization -> ServerConfig
serverConfig site =
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
      staticAssets = Site.siteStaticAssets site,
      requestPolicy = Site.siteRequestPolicy site,
      observability = ObservabilityConfig {tracingExporter = Nothing, metricsExporter = Nothing}
    }
