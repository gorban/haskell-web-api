{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# E2E_SPEC #-}

import HarchWeb qualified
import System.IO.Temp (withSystemTempDirectory)
import WebApi (buildApp)
import WebApi.Config (AppConfig (..), StaticAssetRoot (..), StaticAssetsConfig (..), defaultAppConfig, defaultStaticAssetContentTypes)

spec =
  describe "stacked application real-browser smoke coverage" $ do
    it "redirects the root route to the complete Spaces SSR document" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildApp appConfig) $ \server -> do
          let homeUrl = HarchWeb.localServerBaseUrl server <> "/"
          runBrowserScenario
            browser
            ( do
                visit homeUrl
                assertUrl (`shouldBe` (HarchWeb.localServerBaseUrl server <> "/spaces"))
                assertText (byRole Heading) (`shouldBe` "Site under construction")
            )
            `shouldReturn` Right ()

    it "keeps direct second-page loads and script-disabled root redirects usable" $
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
                assertUrl (`shouldBe` (HarchWeb.localServerBaseUrl server <> "/spaces"))
                assertText (byRole Heading) (`shouldBe` "Site under construction")
            )
            `shouldReturn` Right ()

    it "redirects Spanish roots to localized Spaces SSR content while scripts are disabled" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildApp appConfig) $ \server -> do
          let spanishHomeUrl = HarchWeb.localServerBaseUrl server <> "/es"
          runBrowserScenario
            browser
            ( do
                visitWithoutScripts spanishHomeUrl
                assertUrl (`shouldBe` (HarchWeb.localServerBaseUrl server <> "/es/spaces"))
                assertText (byRole Heading) (`shouldBe` "Sitio en construcción")
            )
            `shouldReturn` Right ()

    it "serves the app-home spaces placeholder through SSR and enhanced navigation" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildApp appConfig) $ \server -> do
          let homeUrl = HarchWeb.localServerBaseUrl server <> "/"
              secondUrl = HarchWeb.localServerBaseUrl server <> "/second"
              spacesUrl = HarchWeb.localServerBaseUrl server <> "/spaces"
              spanishSpacesUrl = HarchWeb.localServerBaseUrl server <> "/es/spaces"
          runBrowserScenario
            browser
            ( do
                visit homeUrl
                assertUrl (`shouldBe` spacesUrl)
                assertText (byRole Heading) (`shouldBe` "Site under construction")
                visit secondUrl
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

    it "serves the app-home profile landing through SSR and enhanced navigation" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildApp appConfig) $ \server -> do
          let secondUrl = HarchWeb.localServerBaseUrl server <> "/second"
              profileUrl = HarchWeb.localServerBaseUrl server <> "/profile"
              spanishProfileUrl = HarchWeb.localServerBaseUrl server <> "/es/profile"
          runBrowserScenario
            browser
            ( do
                visit secondUrl
                click (byRole Link `named` "Profile")
                assertUrl (`shouldBe` profileUrl)
                assertText (byRole Heading) (`shouldBe` "Profile")
                assertText (byText "Sign in to view and manage your profile.") (`shouldBe` "Sign in to view and manage your profile.")
                assertMetrics $ \metrics ->
                  $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 1, hardNavigationCount = 0}|])
                visitWithoutScripts profileUrl
                assertText (byRole Heading) (`shouldBe` "Profile")
                assertText (within (css "#app-main") (byText "Create account")) (`shouldBe` "Create account")
                visitWithoutScripts spanishProfileUrl
                assertText (byRole Heading) (`shouldBe` "Perfil")
                assertText (byText "Inicia sesión para ver y administrar tu perfil.") (`shouldBe` "Inicia sesión para ver y administrar tu perfil.")
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
