{-# SPEC #-}

import Data.Text (Text)
import qualified Data.Text as Text
import HarchWeb
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)

newtype TestContext = TestContext
  { requestLanguage :: Text
  }
  deriving (Eq, Show)

data TestRoute
  = KnownRoute
  | MissingRoute
  deriving (Eq, Show)

defaultContext :: TestContext
defaultContext = TestContext {requestLanguage = Text.pack "en"}

spanishContext :: TestContext
spanishContext = TestContext {requestLanguage = Text.pack "es"}

sampleCodec :: RouteCodec TestRoute TestContext
sampleCodec =
  RouteCodec
    { parseRoute = parseSampleRoute,
      renderRoute = renderSampleRoute,
      notFoundRequest = \routeContext -> RouteRequest {requestRoute = MissingRoute, requestContext = routeContext}
    }

parseSampleRoute :: TestContext -> Text -> Maybe (RouteRequest TestRoute TestContext)
parseSampleRoute routeContext path
  | path == Text.pack "/known" =
      Just RouteRequest {requestRoute = KnownRoute, requestContext = routeContext}
  | path == Text.pack "/es/known" =
      Just RouteRequest {requestRoute = KnownRoute, requestContext = spanishContext}
  | otherwise = Nothing

renderSampleRoute :: RouteRequest TestRoute TestContext -> Text
renderSampleRoute request =
  case (requestLanguage (requestContext request), requestRoute request) of
    (language, KnownRoute)
      | language == Text.pack "es" -> Text.pack "/es/known"
      | otherwise -> Text.pack "/known"
    (_, MissingRoute) -> Text.pack "/404"

samplePage :: RouteRequest TestRoute TestContext -> Page TestRoute TestContext
samplePage request =
  Page
    { pageTitle = Text.pack "Known",
      pageRoute = requestRoute request,
      pageContext = requestContext request,
      pageBody = Text.pack "<h1>Known</h1>"
    }

sampleShell :: PageShell TestRoute TestContext
sampleShell =
  PageShell
    { shellBodyAttributes =
        [ HtmlAttribute
            { attributeName = Text.pack "data-app",
              attributeValue = Text.pack "sample"
            }
        ],
      shellNavigationItems =
        [ NavigationItem
            { navigationLabel = Text.pack "Known",
              navigationRoute = KnownRoute
            },
          NavigationItem
            { navigationLabel = Text.pack "Missing",
              navigationRoute = MissingRoute
            }
        ],
      shellMainId = Text.pack "app-main"
    }

sampleApplication :: Application TestRoute TestContext
sampleApplication =
  Application
    { appName = Text.pack "sample",
      routeCodec = sampleCodec,
      renderResponse = renderSampleResponse,
      pageShell = buildPageShell sampleCodec sampleShell
    }

renderSampleResponse :: RouteRequest TestRoute TestContext -> Response TestRoute TestContext
renderSampleResponse request =
  case requestRoute request of
    KnownRoute -> PageResponse (samplePage request)
    MissingRoute ->
      BodyResponse
        ResponseBody
          { responseStatus = 404,
            responseContentType = Text.pack "application/json",
            responseBody = Text.pack "{\"error\":\"missing\"}"
          }

spec = do
  describe "public record coverage" $ do
    it "reads every exported selector from the public request, page, shell, and document records" $ do
      let request = RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext}
          attribute = HtmlAttribute {attributeName = Text.pack "data-app", attributeValue = Text.pack "sample"}
          page = Page {pageTitle = Text.pack "Known", pageRoute = KnownRoute, pageContext = defaultContext, pageBody = Text.pack "<h1>Known</h1>"}
          navigationItem = NavigationItem {navigationLabel = Text.pack "Known", navigationRoute = KnownRoute}
          resolvedNavigationItem = ResolvedNavigationItem {navigationLabel = Text.pack "Known", navigationRoute = KnownRoute, navigationHref = Text.pack "/known", navigationIsActive = True}
          document = Document {documentTitle = Text.pack "Known", documentBodyAttributes = [attribute], documentNavigation = [resolvedNavigationItem], documentMainId = Text.pack "app-main", documentMainContent = Text.pack "<h1>Known</h1>"}
          shell = PageShell {shellBodyAttributes = [attribute], shellNavigationItems = [navigationItem], shellMainId = Text.pack "app-main"}
          responseBodyValue = ResponseBody {responseStatus = 404, responseContentType = Text.pack "application/json", responseBody = Text.pack "{\"error\":\"missing\"}"}
          NavigationItem {navigationLabel = navigationItemLabel, navigationRoute = navigationItemRoute} = navigationItem
          ResolvedNavigationItem {navigationLabel = resolvedNavigationItemLabel, navigationRoute = resolvedNavigationItemRoute, navigationHref = resolvedNavigationItemHref, navigationIsActive = resolvedNavigationItemIsActive} = resolvedNavigationItem

      requestRoute request `shouldBe` KnownRoute
      requestContext request `shouldBe` defaultContext
      attributeName attribute `shouldBe` Text.pack "data-app"
      attributeValue attribute `shouldBe` Text.pack "sample"
      pageTitle page `shouldBe` Text.pack "Known"
      pageRoute page `shouldBe` KnownRoute
      pageContext page `shouldBe` defaultContext
      pageBody page `shouldBe` Text.pack "<h1>Known</h1>"
      navigationItemLabel `shouldBe` Text.pack "Known"
      navigationItemRoute `shouldBe` KnownRoute
      resolvedNavigationItemLabel `shouldBe` Text.pack "Known"
      resolvedNavigationItemRoute `shouldBe` KnownRoute
      resolvedNavigationItemHref `shouldBe` Text.pack "/known"
      resolvedNavigationItemIsActive `shouldBe` True
      documentTitle document `shouldBe` Text.pack "Known"
      documentBodyAttributes document `shouldBe` [attribute]
      documentNavigation document `shouldBe` [resolvedNavigationItem]
      documentMainId document `shouldBe` Text.pack "app-main"
      documentMainContent document `shouldBe` Text.pack "<h1>Known</h1>"
      shellBodyAttributes shell `shouldBe` [attribute]
      shellNavigationItems shell `shouldBe` [navigationItem]
      shellMainId shell `shouldBe` Text.pack "app-main"
      responseStatus responseBodyValue `shouldBe` 404
      responseContentType responseBodyValue `shouldBe` Text.pack "application/json"
      responseBody responseBodyValue `shouldBe` Text.pack "{\"error\":\"missing\"}"

    it "exercises derived Eq and Show instances for public HarchWeb records and responses" $ do
      let request = RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext}
          otherRequest = RouteRequest {requestRoute = MissingRoute, requestContext = defaultContext}
          page = Page {pageTitle = Text.pack "Known", pageRoute = KnownRoute, pageContext = defaultContext, pageBody = Text.pack "<h1>Known</h1>"}
          otherPage = Page {pageTitle = Text.pack "Missing", pageRoute = MissingRoute, pageContext = defaultContext, pageBody = Text.pack "<h1>Missing</h1>"}
          attribute = HtmlAttribute {attributeName = Text.pack "data-app", attributeValue = Text.pack "sample"}
          otherAttribute = HtmlAttribute {attributeName = Text.pack "lang", attributeValue = Text.pack "en"}
          navigationItem = NavigationItem {navigationLabel = Text.pack "Known", navigationRoute = KnownRoute}
          otherNavigationItem = NavigationItem {navigationLabel = Text.pack "Missing", navigationRoute = MissingRoute}
          resolvedNavigationItem = ResolvedNavigationItem {navigationLabel = Text.pack "Known", navigationRoute = KnownRoute, navigationHref = Text.pack "/known", navigationIsActive = True}
          otherResolvedNavigationItem = ResolvedNavigationItem {navigationLabel = Text.pack "Missing", navigationRoute = MissingRoute, navigationHref = Text.pack "/404", navigationIsActive = False}
          document = Document {documentTitle = Text.pack "Known", documentBodyAttributes = [attribute], documentNavigation = [resolvedNavigationItem], documentMainId = Text.pack "app-main", documentMainContent = Text.pack "<h1>Known</h1>"}
          otherDocument = Document {documentTitle = Text.pack "Missing", documentBodyAttributes = [otherAttribute], documentNavigation = [otherResolvedNavigationItem], documentMainId = Text.pack "other-main", documentMainContent = Text.pack "<h1>Missing</h1>"}
          shell = PageShell {shellBodyAttributes = [attribute], shellNavigationItems = [navigationItem], shellMainId = Text.pack "app-main"}
          otherShell = PageShell {shellBodyAttributes = [otherAttribute], shellNavigationItems = [otherNavigationItem], shellMainId = Text.pack "other-main"}
          body = ResponseBody {responseStatus = 404, responseContentType = Text.pack "application/json", responseBody = Text.pack "{\"error\":\"missing\"}"}
          otherBody = ResponseBody {responseStatus = 200, responseContentType = Text.pack "text/html", responseBody = Text.pack "<h1>OK</h1>"}
          pageResponse :: Response TestRoute TestContext
          pageResponse = PageResponse page
          otherPageResponse :: Response TestRoute TestContext
          otherPageResponse = PageResponse otherPage
          bodyResponseValue :: Response TestRoute TestContext
          bodyResponseValue = BodyResponse body
          otherBodyResponseValue :: Response TestRoute TestContext
          otherBodyResponseValue = BodyResponse otherBody

      (request == request) `shouldBe` True
      (request /= otherRequest) `shouldBe` True
      show request `shouldBe` "RouteRequest {requestRoute = KnownRoute, requestContext = TestContext {requestLanguage = \"en\"}}"
      show [request] `shouldBe` "[RouteRequest {requestRoute = KnownRoute, requestContext = TestContext {requestLanguage = \"en\"}}]"
      (page == page) `shouldBe` True
      (page /= otherPage) `shouldBe` True
      show page `shouldBe` "Page {pageTitle = \"Known\", pageRoute = KnownRoute, pageContext = TestContext {requestLanguage = \"en\"}, pageBody = \"<h1>Known</h1>\"}"
      show [page] `shouldBe` "[Page {pageTitle = \"Known\", pageRoute = KnownRoute, pageContext = TestContext {requestLanguage = \"en\"}, pageBody = \"<h1>Known</h1>\"}]"
      (attribute == attribute) `shouldBe` True
      (attribute /= otherAttribute) `shouldBe` True
      show attribute `shouldBe` "HtmlAttribute {attributeName = \"data-app\", attributeValue = \"sample\"}"
      (navigationItem == navigationItem) `shouldBe` True
      (navigationItem /= otherNavigationItem) `shouldBe` True
      show navigationItem `shouldBe` "NavigationItem {navigationLabel = \"Known\", navigationRoute = KnownRoute}"
      (resolvedNavigationItem == resolvedNavigationItem) `shouldBe` True
      (resolvedNavigationItem /= otherResolvedNavigationItem) `shouldBe` True
      show resolvedNavigationItem `shouldBe` "ResolvedNavigationItem {navigationLabel = \"Known\", navigationRoute = KnownRoute, navigationHref = \"/known\", navigationIsActive = True}"
      (document == document) `shouldBe` True
      (document /= otherDocument) `shouldBe` True
      show document `shouldBe` "Document {documentTitle = \"Known\", documentBodyAttributes = [HtmlAttribute {attributeName = \"data-app\", attributeValue = \"sample\"}], documentNavigation = [ResolvedNavigationItem {navigationLabel = \"Known\", navigationRoute = KnownRoute, navigationHref = \"/known\", navigationIsActive = True}], documentMainId = \"app-main\", documentMainContent = \"<h1>Known</h1>\"}"
      show [document] `shouldBe` "[Document {documentTitle = \"Known\", documentBodyAttributes = [HtmlAttribute {attributeName = \"data-app\", attributeValue = \"sample\"}], documentNavigation = [ResolvedNavigationItem {navigationLabel = \"Known\", navigationRoute = KnownRoute, navigationHref = \"/known\", navigationIsActive = True}], documentMainId = \"app-main\", documentMainContent = \"<h1>Known</h1>\"}]"
      (shell == shell) `shouldBe` True
      (shell /= otherShell) `shouldBe` True
      show shell `shouldBe` "PageShell {shellBodyAttributes = [HtmlAttribute {attributeName = \"data-app\", attributeValue = \"sample\"}], shellNavigationItems = [NavigationItem {navigationLabel = \"Known\", navigationRoute = KnownRoute}], shellMainId = \"app-main\"}"
      show [shell] `shouldBe` "[PageShell {shellBodyAttributes = [HtmlAttribute {attributeName = \"data-app\", attributeValue = \"sample\"}], shellNavigationItems = [NavigationItem {navigationLabel = \"Known\", navigationRoute = KnownRoute}], shellMainId = \"app-main\"}]"
      (body == body) `shouldBe` True
      (body /= otherBody) `shouldBe` True
      show body `shouldBe` "ResponseBody {responseStatus = 404, responseContentType = \"application/json\", responseBody = \"{\\\"error\\\":\\\"missing\\\"}\"}"
      show [body] `shouldBe` "[ResponseBody {responseStatus = 404, responseContentType = \"application/json\", responseBody = \"{\\\"error\\\":\\\"missing\\\"}\"}]"
      (pageResponse == pageResponse) `shouldBe` True
      (pageResponse /= otherPageResponse) `shouldBe` True
      show pageResponse `shouldBe` "PageResponse (Page {pageTitle = \"Known\", pageRoute = KnownRoute, pageContext = TestContext {requestLanguage = \"en\"}, pageBody = \"<h1>Known</h1>\"})"
      (bodyResponseValue == bodyResponseValue) `shouldBe` True
      (bodyResponseValue /= otherBodyResponseValue) `shouldBe` True
      show bodyResponseValue `shouldBe` "BodyResponse (ResponseBody {responseStatus = 404, responseContentType = \"application/json\", responseBody = \"{\\\"error\\\":\\\"missing\\\"}\"})"
      show [pageResponse, bodyResponseValue] `shouldBe` "[PageResponse (Page {pageTitle = \"Known\", pageRoute = KnownRoute, pageContext = TestContext {requestLanguage = \"en\"}, pageBody = \"<h1>Known</h1>\"}),BodyResponse (ResponseBody {responseStatus = 404, responseContentType = \"application/json\", responseBody = \"{\\\"error\\\":\\\"missing\\\"}\"})]"

    it "reads the Application fields directly without relying on higher-level helpers" $ do
      let request = RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext}
          codec = routeCodec sampleApplication

      appName sampleApplication `shouldBe` Text.pack "sample"
      parseRoute codec defaultContext (Text.pack "/known") `shouldBe` Just request
      renderRoute codec request `shouldBe` Text.pack "/known"
      notFoundRequest codec defaultContext `shouldBe` RouteRequest {requestRoute = MissingRoute, requestContext = defaultContext}
      renderResponse sampleApplication request `shouldBe` PageResponse (samplePage request)
      pageShell sampleApplication (samplePage request)
        `shouldBe` Text.pack "<html><head><title>Known</title></head><body data-app=\"sample\"><nav><a href=\"/known\" aria-current=\"page\">Known</a><a href=\"/404\">Missing</a></nav><main id=\"app-main\"><h1>Known</h1></main></body></html>"

  describe "application" $ do
    it "preserves the supplied application description" $
      appName (application sampleApplication) `shouldBe` Text.pack "sample"

    it "can render non-page responses for future API routes" $
      renderResponse sampleApplication (RouteRequest {requestRoute = MissingRoute, requestContext = defaultContext})
        `shouldBe` BodyResponse ResponseBody {responseStatus = 404, responseContentType = Text.pack "application/json", responseBody = Text.pack "{\"error\":\"missing\"}"}

  describe "matchRoute" $ do
    it "returns parsed routes for supported paths" $
      matchRoute sampleCodec defaultContext (Text.pack "/known")
        `shouldBe` RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext}

    it "can derive route context from the matched path" $
      matchRoute sampleCodec defaultContext (Text.pack "/es/known")
        `shouldBe` RouteRequest {requestRoute = KnownRoute, requestContext = spanishContext}

    it "falls back to the stable not-found route for unsupported paths" $
      matchRoute sampleCodec defaultContext (Text.pack "/missing")
        `shouldBe` RouteRequest {requestRoute = MissingRoute, requestContext = defaultContext}

  describe "renderRoute" $
    it "can include route context in generated paths" $ do
      renderRoute sampleCodec (RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext})
        `shouldBe` Text.pack "/known"
      renderRoute sampleCodec (RouteRequest {requestRoute = KnownRoute, requestContext = spanishContext})
        `shouldBe` Text.pack "/es/known"

  describe "routeHref" $
    it "reuses route rendering for app-provided navigation targets" $ do
      routeHref sampleCodec defaultContext KnownRoute `shouldBe` Text.pack "/known"
      routeHref sampleCodec spanishContext KnownRoute `shouldBe` Text.pack "/es/known"

  describe "buildNavigation" $
    it "resolves hrefs and active state from the current page context" $
      buildNavigation sampleCodec (samplePage (RouteRequest {requestRoute = KnownRoute, requestContext = spanishContext})) (shellNavigationItems sampleShell)
        `shouldBe` [ ResolvedNavigationItem
                       { navigationLabel = Text.pack "Known",
                         navigationRoute = KnownRoute,
                         navigationHref = Text.pack "/es/known",
                         navigationIsActive = True
                       },
                     ResolvedNavigationItem
                       { navigationLabel = Text.pack "Missing",
                         navigationRoute = MissingRoute,
                         navigationHref = Text.pack "/404",
                         navigationIsActive = False
                       }
                   ]

  describe "buildDocument" $
    it "preserves the generic shell contract separately from app-specific page content" $
      buildDocument sampleCodec sampleShell (samplePage (RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext}))
        `shouldBe` Document
          { documentTitle = Text.pack "Known",
            documentBodyAttributes =
              [ HtmlAttribute
                  { attributeName = Text.pack "data-app",
                    attributeValue = Text.pack "sample"
                  }
              ],
            documentNavigation =
              [ ResolvedNavigationItem
                  { navigationLabel = Text.pack "Known",
                    navigationRoute = KnownRoute,
                    navigationHref = Text.pack "/known",
                    navigationIsActive = True
                  },
                ResolvedNavigationItem
                  { navigationLabel = Text.pack "Missing",
                    navigationRoute = MissingRoute,
                    navigationHref = Text.pack "/404",
                    navigationIsActive = False
                  }
              ],
            documentMainId = Text.pack "app-main",
            documentMainContent = Text.pack "<h1>Known</h1>"
          }

  describe "buildPageShell" $
    it "renders the shared HTML document for the supplied page and shell options" $
      buildPageShell sampleCodec sampleShell (samplePage (RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext}))
        `shouldBe` Text.pack "<html><head><title>Known</title></head><body data-app=\"sample\"><nav><a href=\"/known\" aria-current=\"page\">Known</a><a href=\"/404\">Missing</a></nav><main id=\"app-main\"><h1>Known</h1></main></body></html>"

  describe "runServer" $
    it "writes the stub startup message to the supplied handle" $
      withSystemTempFile "harch-web-output.txt" $ \outputPath outputHandle -> do
        runServer outputHandle () sampleApplication
        hClose outputHandle
        readFile outputPath `shouldReturn` "HTTP Server listening at http://localhost:5001\n"
