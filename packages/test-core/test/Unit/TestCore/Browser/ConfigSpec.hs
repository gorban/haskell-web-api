{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Exception (finally)
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as Text
import System.Directory (withCurrentDirectory)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.IO.Temp (withSystemTempDirectory)
import TestCore.Browser

spec = do
  describe "browser configuration" $ do
    it "uses the bundled Playwright runner defaults" $
      expectAll
        ( (browserRunnerCommand defaultPlaywrightBrowserConfig `shouldBe` "node")
            :| [ browserRunnerArguments defaultPlaywrightBrowserConfig `shouldBe` ["packages/test-core/playwright-runner/runner.cjs"],
                 browserHeadless defaultPlaywrightBrowserConfig `shouldBe` True,
                 browserPauseOnFailure defaultPlaywrightBrowserConfig `shouldBe` False,
                 browserTimeoutMilliseconds defaultPlaywrightBrowserConfig `shouldBe` 10000,
                 browserProtocolTimeoutMilliseconds defaultPlaywrightBrowserConfig `shouldBe` 30000,
                 browserArtifactDirectory defaultPlaywrightBrowserConfig `shouldBe` "test-results/playwright"
               ]
        )

    it "parses runner, timeout, artifact, and browser overrides" $
      parseBrowserConfig
        [ ("TEST_CORE_BROWSER_RUNNER", "custom-node"),
          ("TEST_CORE_BROWSER_RUNNER_ARGUMENTS", "runner.cjs, --debug"),
          ("TEST_CORE_BROWSER_HEADLESS", "false"),
          ("TEST_CORE_BROWSER_PAUSE_ON_FAILURE", "yes"),
          ("TEST_CORE_BROWSER_TIMEOUT_MILLISECONDS", "2500"),
          ("TEST_CORE_BROWSER_PROTOCOL_TIMEOUT_MILLISECONDS", "7500"),
          ("TEST_CORE_BROWSER_ARTIFACT_DIRECTORY", "artifacts/browser")
        ]
        `shouldBe` Right
          BrowserConfig
            { browserRunnerCommand = "custom-node",
              browserRunnerArguments = ["runner.cjs", "--debug"],
              browserHeadless = False,
              browserPauseOnFailure = True,
              browserTimeoutMilliseconds = 2500,
              browserProtocolTimeoutMilliseconds = 7500,
              browserArtifactDirectory = "artifacts/browser"
            }

    it "accepts the supported boolean aliases and rejects invalid overrides" $ do
      expectAll
        ( (parseBrowserConfig [] `shouldBe` Right defaultPlaywrightBrowserConfig)
            :| [ fmap browserHeadless (parseBrowserConfig [("TEST_CORE_BROWSER_HEADLESS", "true")]) `shouldBe` Right True,
                 fmap browserHeadless (parseBrowserConfig [("TEST_CORE_BROWSER_HEADLESS", "1")]) `shouldBe` Right True,
                 fmap browserHeadless (parseBrowserConfig [("TEST_CORE_BROWSER_HEADLESS", "0")]) `shouldBe` Right False,
                 fmap browserPauseOnFailure (parseBrowserConfig [("TEST_CORE_BROWSER_PAUSE_ON_FAILURE", "no")]) `shouldBe` Right False,
                 browserRunnerArguments <$> parseBrowserConfig [("TEST_CORE_BROWSER_RUNNER_ARGUMENTS", " , ")] `shouldBe` Right [],
                 parseBrowserConfig [("TEST_CORE_BROWSER_HEADLESS", "maybe")] `shouldBe` Left "Invalid boolean for TEST_CORE_BROWSER_HEADLESS: maybe",
                 parseBrowserConfig [("TEST_CORE_BROWSER_PAUSE_ON_FAILURE", "sometimes")] `shouldBe` Left "Invalid boolean for TEST_CORE_BROWSER_PAUSE_ON_FAILURE: sometimes",
                 parseBrowserConfig [("TEST_CORE_BROWSER_TIMEOUT_MILLISECONDS", "0")] `shouldBe` Left "Invalid positive integer for TEST_CORE_BROWSER_TIMEOUT_MILLISECONDS: 0",
                 parseBrowserConfig [("TEST_CORE_BROWSER_TIMEOUT_MILLISECONDS", "later")] `shouldBe` Left "Invalid positive integer for TEST_CORE_BROWSER_TIMEOUT_MILLISECONDS: later",
                 parseBrowserConfig [("TEST_CORE_BROWSER_PROTOCOL_TIMEOUT_MILLISECONDS", "0")] `shouldBe` Left "Invalid positive integer for TEST_CORE_BROWSER_PROTOCOL_TIMEOUT_MILLISECONDS: 0",
                 parseBrowserConfig [("TEST_CORE_BROWSER_PROTOCOL_TIMEOUT_MILLISECONDS", "later")] `shouldBe` Left "Invalid positive integer for TEST_CORE_BROWSER_PROTOCOL_TIMEOUT_MILLISECONDS: later"
               ]
        )

    it "resolves the bundled runner and applies environment overrides"
      $ withEnvironment
        [ ("TEST_CORE_BROWSER_HEADLESS", Just "false"),
          ("TEST_CORE_BROWSER_TIMEOUT_MILLISECONDS", Just "3210"),
          ("TEST_CORE_BROWSER_PROTOCOL_TIMEOUT_MILLISECONDS", Just "6543")
        ]
      $ do
        loaded <- loadPlaywrightBrowserConfig
        config <-
          case loaded of
            Left loadError -> expectationFailure loadError >> fail "unreachable"
            Right loadedConfig -> pure loadedConfig
        expectAll
          ( (browserRunnerCommand config `shouldBe` "node")
              :| [ browserRunnerArguments config `shouldSatisfy` \case
                     [runnerArgument] -> "runner.cjs" `Text.isSuffixOf` Text.pack runnerArgument
                     _ -> False,
                   browserHeadless config `shouldBe` False,
                   browserTimeoutMilliseconds config `shouldBe` 3210,
                   browserProtocolTimeoutMilliseconds config `shouldBe` 6543
                 ]
          )

    it "reports a missing bundled runner when no repository ancestor contains it" $
      withSystemTempDirectory "browser-config" $ \tempDirectory ->
        withCurrentDirectory tempDirectory $ do
          result <- loadPlaywrightBrowserConfig
          result `shouldSatisfy` \case
            Left message -> "Could not find bundled Playwright runner" `Text.isInfixOf` Text.pack message
            Right _ -> False
  where
    withEnvironment overrides action = do
      originalValues <- traverse capture overrides
      apply overrides
      action `finally` restore originalValues
    capture (name, _) = do
      originalValue <- lookupEnv name
      pure (name, originalValue)
    apply = traverse_ setValue
    restore = traverse_ setValue
    setValue (name, maybeValue) =
      case maybeValue of
        Just value -> setEnv name value
        Nothing -> unsetEnv name
