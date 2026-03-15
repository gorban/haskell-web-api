{-# SPEC #-}

import Data.Text (Text)
import qualified Data.Text as Text
import qualified HarchWeb
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import WebApi (AppConfig (..), AppRoute (..), buildApp, defaultAppConfig, matchRoute, renderRoute, run)

pureApplication :: HarchWeb.Application AppRoute
pureApplication = buildApp defaultAppConfig

pureRouteMatcher :: Text -> AppRoute
pureRouteMatcher = matchRoute

renderedShell :: AppConfig -> AppRoute -> Text
renderedShell config route =
  let application = buildApp config
      page = HarchWeb.renderPage application route
   in HarchWeb.pageShell application page

spec = do
  describe "matchRoute" $ do
    it "matches the home path" $
      pureRouteMatcher (Text.pack "/") `shouldBe` HomeRoute

    it "matches the second page path" $
      pureRouteMatcher (Text.pack "/second") `shouldBe` SecondRoute

    it "falls back to the stable not-found route for unknown paths" $
      pureRouteMatcher (Text.pack "/missing") `shouldBe` NotFoundRoute

  describe "renderRoute" $ do
    it "selects the expected home page model" $
      renderRoute defaultAppConfig HomeRoute
        `shouldBe` HarchWeb.Page
          { HarchWeb.pageTitle = Text.pack "web-api: Home",
            HarchWeb.pageRoute = HomeRoute,
            HarchWeb.pageBody = Text.pack "<h1>Home</h1>"
          }

    it "selects a distinct second page model" $
      renderRoute defaultAppConfig SecondRoute
        `shouldBe` HarchWeb.Page
          { HarchWeb.pageTitle = Text.pack "web-api: Second",
            HarchWeb.pageRoute = SecondRoute,
            HarchWeb.pageBody = Text.pack "<h1>Second</h1>"
          }

    it "selects a stable not-found page model" $
      renderRoute defaultAppConfig NotFoundRoute
        `shouldBe` HarchWeb.Page
          { HarchWeb.pageTitle = Text.pack "web-api: Not Found",
            HarchWeb.pageRoute = NotFoundRoute,
            HarchWeb.pageBody = Text.pack "<h1>Not Found</h1>"
          }

    it "keeps shared layout data consistent across all routes" $ do
      let config = AppConfig {appTitlePrefix = Text.pack "test-app"}
      renderedShell config HomeRoute
        `shouldBe` Text.pack "<html><head><title>test-app: Home</title></head><body data-app=\"test-app\"><main><h1>Home</h1></main></body></html>"
      renderedShell config SecondRoute
        `shouldBe` Text.pack "<html><head><title>test-app: Second</title></head><body data-app=\"test-app\"><main><h1>Second</h1></main></body></html>"
      renderedShell config NotFoundRoute
        `shouldBe` Text.pack "<html><head><title>test-app: Not Found</title></head><body data-app=\"test-app\"><main><h1>Not Found</h1></main></body></html>"

    it "keeps config, routes, and pages serializable and deterministic for tests" $ do
      let config = AppConfig {appTitlePrefix = Text.pack "test-app"}
      show config `shouldBe` "AppConfig {appTitlePrefix = \"test-app\"}"
      show (renderRoute config SecondRoute)
        `shouldBe` "Page {pageTitle = \"test-app: Second\", pageRoute = SecondRoute, pageBody = \"<h1>Second</h1>\"}"
      renderRoute config SecondRoute `shouldBe` renderRoute config SecondRoute

  describe "buildApp" $ do
    it "constructs the application description against the HarchWeb facade" $
      HarchWeb.appName pureApplication `shouldBe` Text.pack "web-api"

    it "stores the same route codec behavior used by direct route tests" $ do
      let codec = HarchWeb.routeCodec pureApplication
      HarchWeb.parseRoute codec (Text.pack "/") `shouldBe` Just HomeRoute
      HarchWeb.parseRoute codec (Text.pack "/second") `shouldBe` Just SecondRoute
      HarchWeb.parseRoute codec (Text.pack "/missing") `shouldBe` Nothing
      HarchWeb.renderRoute codec HomeRoute `shouldBe` Text.pack "/"
      HarchWeb.renderRoute codec SecondRoute `shouldBe` Text.pack "/second"
      HarchWeb.renderRoute codec NotFoundRoute `shouldBe` Text.pack "/404"
      HarchWeb.notFoundRoute codec `shouldBe` NotFoundRoute

    it "stores the same page rendering behavior used by direct page tests" $ do
      HarchWeb.renderPage pureApplication HomeRoute `shouldBe` renderRoute defaultAppConfig HomeRoute
      HarchWeb.renderPage pureApplication SecondRoute `shouldBe` renderRoute defaultAppConfig SecondRoute
      HarchWeb.notFoundPage pureApplication `shouldBe` renderRoute defaultAppConfig NotFoundRoute

    it "is structurally complete enough to render supported and not-found shells" $ do
      let homePage = HarchWeb.renderPage pureApplication HomeRoute
          secondPage = HarchWeb.renderPage pureApplication SecondRoute
          notFoundPage = HarchWeb.notFoundPage pureApplication
      HarchWeb.pageShell pureApplication homePage
        `shouldBe` Text.pack "<html><head><title>web-api: Home</title></head><body data-app=\"web-api\"><main><h1>Home</h1></main></body></html>"
      HarchWeb.pageShell pureApplication secondPage
        `shouldBe` Text.pack "<html><head><title>web-api: Second</title></head><body data-app=\"web-api\"><main><h1>Second</h1></main></body></html>"
      HarchWeb.pageShell pureApplication notFoundPage
        `shouldBe` Text.pack "<html><head><title>web-api: Not Found</title></head><body data-app=\"web-api\"><main><h1>Not Found</h1></main></body></html>"

  describe "run" $
    it "writes startup output to the supplied handle for isolated tests" $
      withSystemTempFile "web-api-output.txt" $ \outputPath outputHandle -> do
        run outputHandle
        hClose outputHandle
        readFile outputPath `shouldReturn` "HTTP Server listening at http://localhost:5001\n"
