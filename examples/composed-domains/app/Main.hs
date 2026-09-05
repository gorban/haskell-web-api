{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import App.Composed
import Catalog.Domain
import Data.List.NonEmpty (NonEmpty (..))
import HarchWeb
  ( ListenerConfig (..),
    ListenerScheme (Http),
    ObservabilityConfig (..),
    ServerConfig (..),
    runServer,
  )
import HarchWeb qualified
import HarchWeb.Site qualified as Site
import HarchWeb.Time (currentUnixTimeNanoseconds)
import Orders.Domain
import System.IO (stdout)

main :: IO ()
main = do
  signingKey <- HarchWeb.generateCsrfSigningKey
  case HarchWeb.mkCsrfKeyId "composed-domains-development-v1" of
    Nothing -> ioError (userError "invalid composed-domains development CSRF key identifier")
    Just keyId ->
      case HarchWeb.mkSignedCsrfKeyring keyId ((keyId, signingKey) :| []) of
        Nothing -> ioError (userError "invalid composed-domains development CSRF key ring")
        Just keyring -> do
          let site =
                buildComposedSite
                  defaultComposedStaticAssets
                  defaultLocalePolicy
                  (HarchWeb.signedCsrfProtection keyring HarchWeb.defaultSignedCsrfPolicy currentUnixTimeNanoseconds (const (pure HarchWeb.AnonymousCsrfBinding)))
                  catalogQueries
                  catalogCommands
                  ordersQueries
                  ordersCommands
              catalogQueries = CatalogQueries (const (pure "Catalog"))
              catalogCommands = CatalogCommands (const (pure "refreshed"))
              ordersQueries = OrdersQueries (const (pure "Orders"))
              ordersCommands = OrdersCommands (const (pure (OrderId "order-1")))
          runServer stdout (serverConfig site) (Site.buildSiteApplication site)

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
