{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# E2E_SPEC #-}

import qualified HarchWeb
import System.IO.Temp (withSystemTempDirectory)
import WebApi (buildApp)
import WebApi.Config (AppConfig (..), StaticAssetRoot (..), StaticAssetsConfig (..), defaultAppConfig, defaultStaticAssetContentTypes)

spec =
  describe "stacked application real-browser smoke coverage" $ do
    it "serves complete SSR and enhances same-origin navigation" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildApp appConfig) $ \server -> do
          let homeUrl = HarchWeb.localServerBaseUrl server <> "/"
              secondUrl = HarchWeb.localServerBaseUrl server <> "/second"
          runBrowserScenario
            browser
            ( do
                visit homeUrl
                assertText (byRole Heading) (`shouldBe` "Home")
                click (byRole Link `named` "Browse the second page")
                assertUrl (`shouldBe` secondUrl)
                assertText (byRole Heading) (`shouldBe` "Second")
                assertMetrics $ \metrics ->
                  $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 1, hardNavigationCount = 0}|])
            )
            `shouldReturn` Right ()

    it "keeps direct second-page loads and script-disabled navigation usable" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildApp appConfig) $ \server -> do
          let homeUrl = HarchWeb.localServerBaseUrl server <> "/"
              secondUrl = HarchWeb.localServerBaseUrl server <> "/second"
          runBrowserScenario
            browser
            ( do
                visit secondUrl
                assertText (byRole Heading) (`shouldBe` "Second")
                visitWithoutScripts homeUrl
                click (byRole Link `named` "Browse the second page")
                assertUrl (`shouldBe` secondUrl)
                assertText (byRole Heading) (`shouldBe` "Second")
                assertMetrics $ \metrics ->
                  $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 0, hardNavigationCount = 1}|])
            )
            `shouldReturn` Right ()

    it "serves Spanish SSR content and preserves its typed locale while scripts are disabled" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildApp appConfig) $ \server -> do
          let spanishHomeUrl = HarchWeb.localServerBaseUrl server <> "/es"
              spanishSecondUrl = HarchWeb.localServerBaseUrl server <> "/es/second"
          runBrowserScenario
            browser
            ( do
                visitWithoutScripts spanishHomeUrl
                assertText (byText "Inicio renderizado en el servidor con datos de desarrollo preconfigurados.") (`shouldBe` "Inicio renderizado en el servidor con datos de desarrollo preconfigurados.")
                click (byRole Link `named` "Ver la segunda página")
                assertUrl (`shouldBe` spanishSecondUrl)
                assertText (byRole Link `named` "Volver al inicio") (`shouldBe` "Volver al inicio")
            )
            `shouldReturn` Right ()

    it "serves the app-home spaces placeholder through SSR and enhanced navigation" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildApp appConfig) $ \server -> do
          let homeUrl = HarchWeb.localServerBaseUrl server <> "/"
              spacesUrl = HarchWeb.localServerBaseUrl server <> "/spaces"
              spanishSpacesUrl = HarchWeb.localServerBaseUrl server <> "/es/spaces"
          runBrowserScenario
            browser
            ( do
                visit homeUrl
                click (byRole Link `named` "Spaces")
                assertUrl (`shouldBe` spacesUrl)
                assertText (byRole Heading) (`shouldBe` "Site under construction")
                assertMetrics $ \metrics ->
                  $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 1, hardNavigationCount = 0}|])
                visitWithoutScripts spanishSpacesUrl
                assertText (byRole Heading) (`shouldBe` "Sitio en construcción")
                assertText (byText "Sigan este espacio.") (`shouldBe` "Sigan este espacio.")
            )
            `shouldReturn` Right ()

withBrowserApp :: (BrowserConfig -> AppConfig -> IO a) -> IO a
withBrowserApp action = do
  loadedConfig <- loadPlaywrightBrowserConfig
  browser <-
    case loadedConfig of
      Left loadError -> expectationFailure loadError >> fail "unreachable"
      Right config -> pure config
  withSystemTempDirectory "web-api-e2e-assets" $ \assetDirectory ->
    action
      browser
      defaultAppConfig
        { staticAssets =
            StaticAssetsConfig
              { staticAssetRoots =
                  [ StaticAssetRoot
                      { staticUrlPrefix = "/assets",
                        staticDirectory = assetDirectory
                      }
                  ],
                staticAssetContentTypes = defaultStaticAssetContentTypes,
                staticCacheControlSeconds = Nothing
              }
        }
