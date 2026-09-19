{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# SPEC #-}

import Control.Monad (forM_)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as Text
import HarchWeb qualified
import System.Directory (doesDirectoryExist)
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Test.Hspec.Runner qualified as Hspec
import TestSupport.BrowserApp (withBrowserApp, withBrowserServer)
import WebApi.App (buildAppWithDatabaseAndAccountWorkflow, unavailableAccountWorkflow)
import WebApi.Config (AppConfig (..), StaticAssetRoot (..), StaticAssetsConfig (..))
import WebApi.Database (PageRepository (..))

spec = describe "shared browser fixture lifetime" $ do
  it "keeps the same server usable after request and assertion failures and releases both scopes" $ do
    assetDirectories <- newIORef []
    serverUrls <- newIORef []
    failedRequestResponse <- newIORef Nothing
    let recordApp action = withBrowserApp $ \environment@(_, config) -> do
          writeIORef assetDirectories (map staticDirectory (staticAssetRoots (staticAssets config)))
          action environment
        brokenPageApplication config =
          buildAppWithDatabaseAndAccountWorkflow config (PageRepository (\_ -> ioError (userError "intentional page load failure"))) unavailableAccountWorkflow
        recordServer action = withBrowserServer brokenPageApplication $ \subject@(_, server) -> do
          modifyIORef' serverUrls (HarchWeb.localServerBaseUrl server :)
          action subject
    summary <- runFixtureSpec $ aroundAll recordApp $ aroundAllWith recordServer $ do
      it "reports the first scenario failure" $ \(_, server) -> do
        response <- fetchStatus (HarchWeb.localServerBaseUrl server <> "/second")
        writeIORef failedRequestResponse (Just response)
        expectationFailure "intentional scenario assertion failure"
      it "serves the next scenario on the same listener" $ \(_, server) -> do
        readIORef serverUrls `shouldReturn` [HarchWeb.localServerBaseUrl server]
        fetchStatus (HarchWeb.localServerBaseUrl server <> "/assets/styles/app.css") `shouldReturn` (ExitSuccess, "200")
    directories <- readIORef assetDirectories
    urls <- readIORef serverUrls
    baseUrl <- $([|urls|] `shouldMatch` [p|[baseUrl]|])
    expectAll $
      (Hspec.summaryExamples summary `shouldBe` 2)
        :| [ Hspec.summaryFailures summary `shouldBe` 1,
             readIORef failedRequestResponse `shouldReturn` Just (ExitSuccess, "500"),
             length directories `shouldBe` 1,
             forM_ directories (\directory -> doesDirectoryExist directory `shouldReturn` False),
             fetchStatus (baseUrl <> "/assets/styles/app.css") >>= (\(exitCode, _) -> exitCode `shouldSatisfy` (/= ExitSuccess))
           ]

  it "releases shared assets when application construction fails before any example runs" $ do
    assetDirectories <- newIORef []
    exampleRan <- newIORef False
    let recordApp action = withBrowserApp $ \environment@(_, config) -> do
          writeIORef assetDirectories (map staticDirectory (staticAssetRoots (staticAssets config)))
          action environment
    summary <- runFixtureSpec $
      aroundAll recordApp $
        aroundAllWith (withBrowserServer (\_ -> error "intentional application construction failure")) $
          it "must not run without a server" $
            \_ -> writeIORef exampleRan True
    directories <- readIORef assetDirectories
    expectAll $
      (Hspec.summaryFailures summary `shouldBe` 1)
        :| [ readIORef exampleRan `shouldReturn` False,
             length directories `shouldBe` 1,
             forM_ directories (\directory -> doesDirectoryExist directory `shouldReturn` False)
           ]

-- The child suite deliberately fails. Run it without inheriting the outer CLI
-- filters or emitting its expected failures into the parent test report.
runFixtureSpec :: Spec -> IO Hspec.Summary
runFixtureSpec fixtureSpec = do
  let config = Hspec.defaultConfig {Hspec.configFormat = Just (\_ -> pure (\_ -> pure ()))}
  (evaluatedConfig, forest) <- Hspec.evalSpec config fixtureSpec
  Hspec.toSummary <$> Hspec.runSpecForest forest evaluatedConfig

fetchStatus :: Text.Text -> IO (ExitCode, String)
fetchStatus url = do
  (exitCode, status, _) <-
    readProcessWithExitCode
      "curl"
      ["--silent", "--show-error", "--noproxy", "*", "--max-time", "2", "--output", "/dev/null", "--write-out", "%{http_code}", Text.unpack url]
      ""
  pure (exitCode, status)
