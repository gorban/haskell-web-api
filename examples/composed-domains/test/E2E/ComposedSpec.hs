{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# E2E_SPEC #-}

import App.Composed
import Catalog.Domain
import Data.List.NonEmpty (NonEmpty (..))
import HarchWeb
import HarchWeb.RequestContext (RequestContext (..), RequestIdentity (..))
import HarchWeb.Site qualified as Site
import Orders.Domain

spec =
  describe "composed-domains real-browser behavior" $ do
    it "keeps localized public and mounted-domain navigation SSR-complete and enhanced" $
      withBrowserAndServer $ \browser server -> do
        let loginUrl = localServerBaseUrl server <> "/es/public/login"
            catalogUrl = localServerBaseUrl server <> "/es/catalog"
            ordersUrl = localServerBaseUrl server <> "/es/orders"
        ( runBrowserScenario browser do
            visit loginUrl
            assertAll
              ((,) <$> textContent (byRole Link `named` "Catalog") <*> textContent (byRole Link `named` "Orders"))
              (\(catalogLabel, ordersLabel) -> (catalogLabel `shouldBe` "Catalog") :| [ordersLabel `shouldBe` "Orders"])
            click (byRole Link `named` "Catalog")
            assertAll
              ((,,) <$> currentUrl <*> textContent (byRole Heading `named` "es catalog") <*> browserMetrics)
              ( \(url, heading, metrics) ->
                  (url `shouldBe` catalogUrl)
                    :| [ heading `shouldBe` "es catalog",
                         $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 1, hardNavigationCount = 0}|])
                       ]
              )
            reload
            assertAll
              ((,) <$> currentUrl <*> textContent (byRole Heading `named` "es catalog"))
              (\(url, heading) -> (url `shouldBe` catalogUrl) :| [heading `shouldBe` "es catalog"])
            click (byRole Link `named` "Orders")
            assertAll
              ((,) <$> currentUrl <*> textContent (byRole Heading `named` "es orders"))
              (\(url, heading) -> (url `shouldBe` ordersUrl) :| [heading `shouldBe` "es orders"])
          )
          `shouldReturn` Right ()

    it "keeps default-locale mounted navigation SSR-complete across reload and enhancement" $
      withBrowserAndServer $ \browser server -> do
        let catalogUrl = localServerBaseUrl server <> "/catalog"
            ordersUrl = localServerBaseUrl server <> "/en/orders"
        ( runBrowserScenario browser do
            visit catalogUrl
            assertAll
              ((,) <$> currentUrl <*> textContent (byRole Heading `named` "en catalog"))
              (\(url, heading) -> (url `shouldBe` catalogUrl) :| [heading `shouldBe` "en catalog"])
            reload
            assertAll
              (textContent (byRole Heading `named` "en catalog"))
              (\heading -> (heading `shouldBe` "en catalog") :| [])
            click (byRole Link `named` "Orders")
            assertAll
              ((,,) <$> currentUrl <*> textContent (byRole Heading `named` "en orders") <*> browserMetrics)
              ( \(url, heading, metrics) ->
                  (url `shouldBe` ordersUrl)
                    :| [ heading `shouldBe` "en orders",
                         $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 1, hardNavigationCount = 1}|])
                       ]
              )
          )
          `shouldReturn` Right ()

    it "keeps public and mounted-domain navigation usable when scripts are disabled" $
      withBrowserAndServer $ \browser server -> do
        let loginUrl = localServerBaseUrl server <> "/public/login"
            spanishLoginUrl = localServerBaseUrl server <> "/es/public/login"
        ( runBrowserScenario browser do
            visitWithoutScripts loginUrl
            assertAll
              ((,) <$> textContent (byRole Heading `named` "Login") <*> textContent (byRole Link `named` "Catalog"))
              (\(heading, catalogLabel) -> (heading `shouldBe` "Login") :| [catalogLabel `shouldBe` "Catalog"])
            click (byRole Link `named` "Catalog")
            assertAll
              (textContent (byRole Heading `named` "en catalog"))
              (\heading -> (heading `shouldBe` "en catalog") :| [])
            click (byRole Link `named` "Orders")
            assertAll
              (textContent (byRole Heading `named` "en orders"))
              (\heading -> (heading `shouldBe` "en orders") :| [])
            visitWithoutScripts spanishLoginUrl
            click (byRole Link `named` "Catalog")
            assertAll
              (textContent (byRole Heading `named` "es catalog"))
              (\heading -> (heading `shouldBe` "es catalog") :| [])
            click (byRole Link `named` "Orders")
            assertAll
              (textContent (byRole Heading `named` "es orders"))
              (\heading -> (heading `shouldBe` "es orders") :| [])
          )
          `shouldReturn` Right ()

withBrowserAndServer :: (BrowserConfig -> LocalTestServer -> IO a) -> IO a
withBrowserAndServer action = do
  loadedConfig <- loadPlaywrightBrowserConfig
  browser <-
    case loadedConfig of
      Left loadError -> expectationFailure loadError >> fail "unreachable"
      Right config -> pure config
  withLocalTestServer composedBrowserApplication (action browser)

composedBrowserApplication :: Application RootRoute RootAction ComposedContext RootAuthorization
composedBrowserApplication =
  Site.buildSiteApplication (buildComposedSiteWithSecurity defaultComposedStaticAssets defaultLocalePolicy browserSecurity catalogQueries catalogCommands ordersQueries ordersCommands)

browserSecurity :: ApplicationSecurity RootRoute ComposedContext RootAuthorization
browserSecurity =
  AuthenticationEnabled [] (AuthenticationGuard (pure . ContinueEndpoint . authenticatedContext . endpointRouteRequest)) []

authenticatedContext :: RouteRequest RootRoute ComposedContext -> ComposedContext
authenticatedContext request =
  (requestContext request)
    { requestIdentity = AuthenticatedIdentity (RootPrincipal (Just (locale "es")) ["catalog.read", "orders.read"])
    }

catalogQueries :: CatalogQueries
catalogQueries = CatalogQueries (\domainContext -> pure (catalogLocaleCode domainContext <> " catalog"))

catalogCommands :: CatalogCommands
catalogCommands = CatalogCommands (const (pure "refreshed"))

ordersQueries :: OrdersQueries
ordersQueries = OrdersQueries (\domainContext -> pure (ordersLocaleCode domainContext <> " orders"))

ordersCommands :: OrdersCommands
ordersCommands = OrdersCommands (\domainContext -> pure (OrderId ("order-" <> ordersLocaleCode domainContext)))
