{-# SPEC #-}

import Data.Text (Text)
import qualified Data.Text as Text
import HarchWeb

data TestRoute
  = KnownRoute
  | MissingRoute
  deriving (Eq, Show)

sampleCodec :: RouteCodec TestRoute
sampleCodec =
  RouteCodec
    { parseRoute = parseSampleRoute
    , renderRoute = renderSampleRoute
    , notFoundRoute = MissingRoute
    }

parseSampleRoute :: Text -> Maybe TestRoute
parseSampleRoute path
  | path == Text.pack "/known" = Just KnownRoute
  | otherwise = Nothing

renderSampleRoute :: TestRoute -> Text
renderSampleRoute route =
  case route of
    KnownRoute -> Text.pack "/known"
    MissingRoute -> Text.pack "/404"

samplePage :: Page TestRoute
samplePage =
  Page
    { pageTitle = Text.pack "Known"
    , pageRoute = KnownRoute
    , pageBody = Text.pack "<h1>Known</h1>"
    }

sampleApplication :: Application TestRoute
sampleApplication =
  Application
    { appName = Text.pack "sample"
    , routeCodec = sampleCodec
    , renderPage = const samplePage
    , notFoundPage = samplePage {pageRoute = MissingRoute, pageTitle = Text.pack "Missing"}
    , pageShell = \page -> Text.concat [Text.pack "<main>", pageBody page, Text.pack "</main>"]
    }

spec = do
  describe "application" $
    it "preserves the supplied application description" $
      appName (application sampleApplication) `shouldBe` Text.pack "sample"

  describe "matchRoute" $ do
    it "returns parsed routes for supported paths" $
      matchRoute sampleCodec (Text.pack "/known") `shouldBe` KnownRoute

    it "falls back to the stable not-found route for unsupported paths" $
      matchRoute sampleCodec (Text.pack "/missing") `shouldBe` MissingRoute

  describe "runServer" $
    it "completes for the stub server boundary" $
      runServer () sampleApplication `shouldReturn` ()