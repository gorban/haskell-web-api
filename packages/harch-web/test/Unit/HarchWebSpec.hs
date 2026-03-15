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

sampleApplication :: Application TestRoute TestContext
sampleApplication =
  Application
    { appName = Text.pack "sample",
      routeCodec = sampleCodec,
      renderResponse = renderSampleResponse,
      pageShell = \page -> Text.concat [Text.pack "<main>", pageBody page, Text.pack "</main>"]
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

  describe "runServer" $
    it "writes the stub startup message to the supplied handle" $
      withSystemTempFile "harch-web-output.txt" $ \outputPath outputHandle -> do
        runServer outputHandle () sampleApplication
        hClose outputHandle
        readFile outputPath `shouldReturn` "HTTP Server listening at http://localhost:5001\n"
