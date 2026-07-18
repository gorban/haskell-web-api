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
