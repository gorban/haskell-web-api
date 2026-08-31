{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Data.Text qualified as Text
import HarchWeb qualified
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import TestCore.Wai (performWaiRequest, readResponseBody, waiRequest)
import Unit.WebApi.TestSupport hiding (databaseConfig)
import WebApi (buildApp)
import WebApi.App.Enhancements (pageEnhancementHooks)
import WebApi.App.Shell (buildAppPageShell, buildAppPageShellConfig)
import WebApi.Config (AppConfig (..), StaticAssetRoot (..), StaticAssetsConfig (..), defaultAppConfig, defaultStaticAssetContentTypes)
import WebApi.Page (renderPage)
import WebApi.PageShell qualified as LegacyPageShell
import WebApi.Route (AppRoute (..), defaultRequestContext, routeMetadata)

spec = do
  describe "page shell integration" $ do
    it "keeps every page route's path, title, and enhancements in one metadata table" $
      map (metadataFields . routeMetadata) [HomeRoute, SecondRoute, SpacesRoute, RegistrationRoute, EmailVerificationRoute, MfaEnrollmentRoute, LoginRoute, LogoutRoute, ProfileRoute, NotFoundRoute, StatusApiRoute]
        `shouldBe` [ (Nothing, "", "Home", []),
                     (Just "second", "/second", "Second", ["second-page"]),
                     (Just "spaces", "/spaces", "Spaces", []),
                     (Just "register", "/register", "Create account", []),
                     (Just "verify", "/verify", "Verify email", []),
                     (Just "mfa", "/mfa", "Set up authenticator", []),
                     (Just "login", "/login", "Sign in", []),
                     (Just "logout", "/logout", "Sign out", []),
                     (Just "profile", "/profile", "Profile", []),
                     (Just "404", "/404", "Not Found", []),
                     (Nothing, "/api/404", "Not Found", [])
                   ]

    it "keeps client-only enhancement hooks in the app seam instead of page rendering" $ do
      pageEnhancementHooks HomeRoute `shouldBe` []
      pageEnhancementHooks SecondRoute `shouldBe` ["second-page"]
      pageEnhancementHooks SpacesRoute `shouldBe` []
      pageEnhancementHooks StatusApiRoute `shouldBe` []
      pageEnhancementHooks NotFoundRoute `shouldBe` []

    it "marks the active navigation item for each routed page" $ do
      homeShell <- renderedShell defaultAppConfig HomeRoute
      secondShell <- renderedShell defaultAppConfig SecondRoute
      spacesShell <- renderedShell defaultAppConfig SpacesRoute
      notFoundShell <- renderedShell defaultAppConfig NotFoundRoute
      Text.isInfixOf "<a href=\"/\" data-page-link=\"true\" aria-current=\"page\">Home</a><a href=\"/second\" data-page-link=\"true\">Second</a><a href=\"/spaces\" data-page-link=\"true\">Spaces</a>" homeShell `shouldBe` True
      Text.isInfixOf "<a href=\"/\" data-page-link=\"true\">Home</a><a href=\"/second\" data-page-link=\"true\" aria-current=\"page\">Second</a><a href=\"/spaces\" data-page-link=\"true\">Spaces</a>" secondShell `shouldBe` True
      Text.isInfixOf "<a href=\"/spaces\" data-page-link=\"true\" aria-current=\"page\">Spaces</a>" spacesShell `shouldBe` True
      Text.isInfixOf "aria-current=\"page\"" notFoundShell `shouldBe` False

    it "emits deterministic navigation hooks and script references when assets are configured" $ do
      let rootMountedConfig =
            navigationAppConfig
              { staticAssets =
                  StaticAssetsConfig
                    { staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = "/", staticDirectory = "public"}],
                      staticAssetContentTypes = defaultStaticAssetContentTypes,
                      staticCacheControlSeconds = Nothing
                    }
              }
      homeShellWithoutAssets <- renderedShell defaultAppConfig HomeRoute
      homeShell <- renderedShell navigationAppConfig HomeRoute
      secondShell <- renderedShell navigationAppConfig SecondRoute
      rootMountedShell <- renderedShell rootMountedConfig HomeRoute
      Text.isInfixOf "<script type=\"module\" src=\"/assets/navigation.js\" defer></script>" homeShellWithoutAssets `shouldBe` True
      Text.isInfixOf "<script type=\"module\" src=\"/assets/navigation.js\" defer></script>" homeShell `shouldBe` True
      Text.isInfixOf "<script type=\"module\" src=\"/assets/navigation.js\" defer></script>" rootMountedShell `shouldBe` True
      Text.isInfixOf "<nav data-navigation-region=\"primary\" class=\"harch-app-shell-navigation\">" homeShell `shouldBe` True
      Text.isInfixOf "<main id=\"app-main\" data-navigation-content=\"true\" class=\"harch-app-shell-main\">" homeShell `shouldBe` True
      Text.isInfixOf "data-bootstrap-hooks" homeShell `shouldBe` False
      Text.isInfixOf "<main id=\"app-main\" data-navigation-content=\"true\" class=\"harch-app-shell-main\" data-bootstrap-hooks=\"second-page\">" secondShell `shouldBe` True

    it "renders navigation and script hrefs under the forwarded request path prefix" $ do
      prefixedShell <- renderedShellForRequest navigationAppConfig prefixedSecondRequest
      Text.isInfixOf "<a href=\"/app\" data-page-link=\"true\">Home</a><a href=\"/app/second\" data-page-link=\"true\" aria-current=\"page\">Second</a>" prefixedShell `shouldBe` True
      Text.isInfixOf "<script type=\"module\" src=\"/app/assets/navigation.js\" defer></script>" prefixedShell `shouldBe` True
      Text.isInfixOf "<link rel=\"stylesheet\" href=\"/app/assets/styles/app.css\">" prefixedShell `shouldBe` True

    it "serves the bundled navigation asset through configured static roots" $ do
      response <- performWaiRequest (HarchWeb.toWaiApplication (buildApp navigationAppConfig)) (waiRequest ["assets", "navigation.js"])
      Wai.responseStatus response `shouldBe` Http.status200
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just "application/javascript; charset=utf-8"
      responseBody <- readResponseBody response
      Text.isInfixOf "data-page-link" responseBody `shouldBe` True
      Text.isInfixOf "popstate" responseBody `shouldBe` True

    it "serves bundled style, font, and resource assets through configured static roots" $ do
      stylesheetResponse <- performWaiRequest (HarchWeb.toWaiApplication (buildApp navigationAppConfig)) (waiRequest ["assets", "styles", "app.css"])
      Wai.responseStatus stylesheetResponse `shouldBe` Http.status200
      lookup Http.hContentType (Wai.responseHeaders stylesheetResponse) `shouldBe` Just "text/css; charset=utf-8"
      stylesheetBody <- readResponseBody stylesheetResponse
      Text.isInfixOf "font-family: system-ui, sans-serif;" stylesheetBody `shouldBe` True
      Text.isInfixOf ":where(a, button, input, select):focus-visible" stylesheetBody `shouldBe` True
      Text.isInfixOf ".harch-page-frame-root" stylesheetBody `shouldBe` True

      fontStylesheetResponse <- performWaiRequest (HarchWeb.toWaiApplication (buildApp navigationAppConfig)) (waiRequest ["assets", "fonts", "font-faces.css"])
      Wai.responseStatus fontStylesheetResponse `shouldBe` Http.status200
      lookup Http.hContentType (Wai.responseHeaders fontStylesheetResponse) `shouldBe` Just "text/css; charset=utf-8"
      fontStylesheetBody <- readResponseBody fontStylesheetResponse
      Text.isInfixOf "@font-face" fontStylesheetBody `shouldBe` True

      faviconResponse <- performWaiRequest (HarchWeb.toWaiApplication (buildApp navigationAppConfig)) (waiRequest ["assets", "resources", "favicon.svg"])
      Wai.responseStatus faviconResponse `shouldBe` Http.status200
      lookup Http.hContentType (Wai.responseHeaders faviconResponse) `shouldBe` Just "image/svg+xml"
      faviconBody <- readResponseBody faviconResponse
      Text.isInfixOf "<svg" faviconBody `shouldBe` True

    it "keeps shell output identical for repeated renders of the same page input" $ do
      let application = buildApp defaultAppConfig
      page <- renderPage defaultAppConfig spanishSecondRequest
      HarchWeb.pageShell application page `shouldBe` HarchWeb.pageShell application page

    it "keeps the legacy page-shell shim aligned with the app shell seam" $ do
      renderedPage <- renderPage defaultAppConfig secondRequest
      HarchWeb.renderDocumentForTests (LegacyPageShell.buildAppPageShell defaultAppConfig renderedPage)
        `shouldBe` HarchWeb.renderDocumentForTests (buildAppPageShell defaultAppConfig renderedPage)
      navigationRenderedPage <- renderPage navigationAppConfig secondRequest
      HarchWeb.renderDocumentForTests (LegacyPageShell.buildAppPageShell navigationAppConfig navigationRenderedPage)
        `shouldBe` HarchWeb.renderDocumentForTests (buildAppPageShell navigationAppConfig navigationRenderedPage)

    it "keeps the shell configuration seam aligned with the rendered shell entry point" $ do
      let shellConfig = buildAppPageShellConfig navigationAppConfig defaultRequestContext
      HarchWeb.shellNavigationItems shellConfig `shouldBe` []
      HarchWeb.shellRuntimeDescriptors shellConfig `shouldBe` []

    it "keeps not-found pages inside the shared shell" $ do
      notFoundShell <- renderedShell defaultAppConfig NotFoundRoute
      Text.isInfixOf "<title>web-api: Not Found</title>" notFoundShell `shouldBe` True
      Text.isInfixOf "data-page=\"not-found\"" notFoundShell `shouldBe` True
