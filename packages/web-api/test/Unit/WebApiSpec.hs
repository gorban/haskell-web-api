{-# SPEC #-}

import Data.Text (Text)
import qualified Data.Text as Text
import qualified HarchWeb
import WebApi (AppConfig (..), AppRoute (..), buildApp, defaultAppConfig, matchRoute, renderRoute, run)

pureApplication :: HarchWeb.Application AppRoute
pureApplication = buildApp defaultAppConfig

pureRouteMatcher :: Text -> AppRoute
pureRouteMatcher = matchRoute

spec = do
  describe "buildApp" $ do
    it "constructs the application description against the HarchWeb facade" $ do
      HarchWeb.appName pureApplication `shouldBe` Text.pack "web-api"
      pureRouteMatcher (Text.pack "/") `shouldBe` HomeRoute
      pureRouteMatcher (Text.pack "/missing") `shouldBe` NotFoundRoute

    it "stores pure page rendering and shell functions" $ do
      let homePage = HarchWeb.renderPage pureApplication HomeRoute
      homePage `shouldBe` renderRoute defaultAppConfig HomeRoute
      HarchWeb.pageShell pureApplication homePage
        `shouldBe` Text.pack "<html><head><title>web-api: Home</title></head><body data-app=\"web-api\"><main><h1>Home</h1></main></body></html>"
      HarchWeb.notFoundPage pureApplication `shouldBe` renderRoute defaultAppConfig NotFoundRoute

  describe "renderRoute" $ do
    it "keeps config, routes, and pages serializable and deterministic for tests" $ do
      let config = AppConfig {appTitlePrefix = Text.pack "test-app"}
      show config `shouldBe` "AppConfig {appTitlePrefix = \"test-app\"}"
      show (renderRoute config SecondRoute)
        `shouldBe` "Page {pageTitle = \"test-app: Second\", pageRoute = SecondRoute, pageBody = \"<h1>Second</h1>\"}"

  describe "run" $
    it "keeps IO at the composition-root startup seam" $
      run `shouldReturn` ()
