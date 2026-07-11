{-# LANGUAGE TemplateHaskell #-}

{-# SPEC #-}

import Control.Exception (finally)
import Data.Foldable (traverse_)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import TestCore.Browser

spec = do
  describe "parseBrowserConfig" $ do
    it "uses deterministic defaults when no overrides are present" $
      parseBrowserConfig []
        `shouldBe` Right defaultBrowserConfig

    it "applies environment overrides for runner and browser behavior" $
      parseBrowserConfig
        [ ("TEST_CORE_BROWSER_RUNNER", "node"),
          ("TEST_CORE_BROWSER_RUNNER_ARGUMENTS", "playwright-runner.js, --browser, chromium"),
          ("TEST_CORE_BROWSER_HEADLESS", "false"),
          ("TEST_CORE_BROWSER_KEEP_OPEN_ON_FAILURE", "yes")
        ]
        `shouldBe` Right
          defaultBrowserConfig
            { browserRunnerCommand = "node",
              browserRunnerArguments = ["playwright-runner.js", "--browser", "chromium"],
              browserHeadless = False,
              browserKeepOpenOnFailure = True
            }

    it "accepts numeric boolean overrides" $
      parseBrowserConfig
        [ ("TEST_CORE_BROWSER_HEADLESS", "1"),
          ("TEST_CORE_BROWSER_KEEP_OPEN_ON_FAILURE", "0")
        ]
        `shouldBe` Right
          defaultBrowserConfig
            { browserHeadless = True,
              browserKeepOpenOnFailure = False
            }

    it "accepts no as a false-like keep-open override" $
      parseBrowserConfig [("TEST_CORE_BROWSER_KEEP_OPEN_ON_FAILURE", "no")]
        `shouldBe` Right
          defaultBrowserConfig
            { browserKeepOpenOnFailure = False
            }

    it "treats blank runner arguments as an empty list" $
      parseBrowserConfig [("TEST_CORE_BROWSER_RUNNER_ARGUMENTS", " , ")]
        `shouldBe` Right
          defaultBrowserConfig
            { browserRunnerArguments = []
            }

    it "rejects invalid headless values explicitly" $
      parseBrowserConfig [("TEST_CORE_BROWSER_HEADLESS", "maybe")]
        `shouldBe` Left "Invalid boolean for TEST_CORE_BROWSER_HEADLESS: maybe"

    it "rejects invalid keep-open values explicitly" $
      parseBrowserConfig [("TEST_CORE_BROWSER_KEEP_OPEN_ON_FAILURE", "sometimes")]
        `shouldBe` Left "Invalid boolean for TEST_CORE_BROWSER_KEEP_OPEN_ON_FAILURE: sometimes"

  describe "loadBrowserConfig"
    $ it "reads the same environment overrides through IO"
    $ withEnvironment
      [ ("TEST_CORE_BROWSER_RUNNER", Just "node"),
        ("TEST_CORE_BROWSER_RUNNER_ARGUMENTS", Just "playwright-runner.js, --headed"),
        ("TEST_CORE_BROWSER_HEADLESS", Just "false"),
        ("TEST_CORE_BROWSER_KEEP_OPEN_ON_FAILURE", Just "true")
      ]
    $ do
      loadBrowserConfig
        `shouldReturn` Right
          defaultBrowserConfig
            { browserRunnerCommand = "node",
              browserRunnerArguments = ["playwright-runner.js", "--headed"],
              browserHeadless = False,
              browserKeepOpenOnFailure = True
            }

  describe "renderBrowserRequest" $ do
    it "renders config flags and all supported browser actions predictably" $
      renderBrowserRequest
        defaultBrowserConfig
          { browserHeadless = False,
            browserKeepOpenOnFailure = True
          }
        [ VisitUrl "http://localhost:8080/",
          VisitUrlWithoutScripts "http://localhost:8080/no-js",
          ReloadPage,
          ClickLinkWithText "Browse the second page",
          NavigateHistoryBack,
          NavigateHistoryForward,
          AssertTextEquals "[data-page-title=\"true\"]" "Second",
          AssertNavigationMetricEquals EnhancedFetchCount 1,
          AssertNavigationMetricEquals HardNavigationCount 0
        ]
        `shouldBe` unlines
          [ "headless\tfalse",
            "keep-open-on-failure\ttrue",
            "action\tvisit-url\thttp://localhost:8080/",
            "action\tvisit-url-without-scripts\thttp://localhost:8080/no-js",
            "action\treload-page",
            "action\tclick-link-with-text\tBrowse the second page",
            "action\thistory-back",
            "action\thistory-forward",
            "action\tassert-text-equals\t[data-page-title=\"true\"]\tSecond",
            "action\tassert-navigation-metric-equals\tenhanced-fetch-count\t1",
            "action\tassert-navigation-metric-equals\thard-navigation-count\t0"
          ]

    it "covers selectors and derived instances for the public browser types" $ do
      let browserConfig =
            BrowserConfig
              { browserRunnerCommand = "node",
                browserRunnerArguments = ["runner.js"],
                browserHeadless = False,
                browserKeepOpenOnFailure = True
              }
          navigationMetric = HardNavigationCount
          browserAction = AssertNavigationMetricEquals navigationMetric 0
          browserError = BrowserRunnerProcessError (ExitFailure 3) "stdout" "stderr"
      browserRunnerCommand browserConfig `shouldBe` "node"
      browserRunnerArguments browserConfig `shouldBe` ["runner.js"]
      browserHeadless browserConfig `shouldBe` False
      browserKeepOpenOnFailure browserConfig `shouldBe` True
      navigationMetric `shouldBe` HardNavigationCount
      show navigationMetric `shouldBe` "HardNavigationCount"
      browserAction `shouldBe` AssertNavigationMetricEquals HardNavigationCount 0
      show browserAction `shouldBe` "AssertNavigationMetricEquals HardNavigationCount 0"
      show browserConfig `shouldBe` "BrowserConfig {browserRunnerCommand = \"node\", browserRunnerArguments = [\"runner.js\"], browserHeadless = False, browserKeepOpenOnFailure = True}"
      show browserError `shouldBe` "BrowserRunnerProcessError (ExitFailure 3) \"stdout\" \"stderr\""

    it "covers the remaining equality and show branches for browser actions and errors" $ do
      let visitAction = VisitUrl "http://localhost:8080/"
          noScriptVisitAction = VisitUrlWithoutScripts "http://localhost:8080/no-js"
          reloadAction = ReloadPage
          backAction = NavigateHistoryBack
          forwardAction = NavigateHistoryForward
          assertAction = AssertTextEquals "[data-page-title=\"true\"]" "Second"
          metricAction = AssertNavigationMetricEquals EnhancedFetchCount 1
          launchError = BrowserRunnerLaunchError "missing-browser-runner"
          protocolError = BrowserRunnerProtocolError "unexpected response"
          assertionError = BrowserAssertionFailed "Expected the second page to load"
      defaultBrowserConfig `shouldBe` defaultBrowserConfig
      visitAction `shouldBe` VisitUrl "http://localhost:8080/"
      noScriptVisitAction `shouldBe` VisitUrlWithoutScripts "http://localhost:8080/no-js"
      reloadAction `shouldBe` ReloadPage
      backAction `shouldBe` NavigateHistoryBack
      forwardAction `shouldBe` NavigateHistoryForward
      assertAction `shouldBe` AssertTextEquals "[data-page-title=\"true\"]" "Second"
      metricAction `shouldBe` AssertNavigationMetricEquals EnhancedFetchCount 1
      launchError `shouldBe` BrowserRunnerLaunchError "missing-browser-runner"
      protocolError `shouldBe` BrowserRunnerProtocolError "unexpected response"
      assertionError `shouldBe` BrowserAssertionFailed "Expected the second page to load"
      show visitAction `shouldBe` "VisitUrl \"http://localhost:8080/\""
      show noScriptVisitAction `shouldBe` "VisitUrlWithoutScripts \"http://localhost:8080/no-js\""
      show reloadAction `shouldBe` "ReloadPage"
      show backAction `shouldBe` "NavigateHistoryBack"
      show forwardAction `shouldBe` "NavigateHistoryForward"
      show assertAction `shouldBe` "AssertTextEquals \"[data-page-title=\\\"true\\\"]\" \"Second\""
      show metricAction `shouldBe` "AssertNavigationMetricEquals EnhancedFetchCount 1"
      show launchError `shouldBe` "BrowserRunnerLaunchError \"missing-browser-runner\""
      show protocolError `shouldBe` "BrowserRunnerProtocolError \"unexpected response\""
      show assertionError `shouldBe` "BrowserAssertionFailed \"Expected the second page to load\""

    it "covers the derived /= and showList methods for browser types" $ do
      let otherBrowserConfig =
            defaultBrowserConfig
              { browserRunnerCommand = "node"
              }
          visitAction = VisitUrl "http://localhost:8080/"
          noScriptVisitAction = VisitUrlWithoutScripts "http://localhost:8080/no-js"
          reloadAction = ReloadPage
          clickAction = ClickLinkWithText "Browse the second page"
          backAction = NavigateHistoryBack
          forwardAction = NavigateHistoryForward
          metricAction = AssertNavigationMetricEquals EnhancedFetchCount 1
          launchError = BrowserRunnerLaunchError "missing-browser-runner"
          protocolError = BrowserRunnerProtocolError "unexpected response"
      defaultBrowserConfig /= otherBrowserConfig `shouldBe` True
      EnhancedFetchCount /= HardNavigationCount `shouldBe` True
      visitAction /= clickAction `shouldBe` True
      noScriptVisitAction /= visitAction `shouldBe` True
      reloadAction /= visitAction `shouldBe` True
      backAction /= forwardAction `shouldBe` True
      metricAction /= backAction `shouldBe` True
      launchError /= protocolError `shouldBe` True
      show [defaultBrowserConfig, otherBrowserConfig]
        `shouldBe` "[BrowserConfig {browserRunnerCommand = \"playwright-e2e-runner\", browserRunnerArguments = [], browserHeadless = True, browserKeepOpenOnFailure = False},BrowserConfig {browserRunnerCommand = \"node\", browserRunnerArguments = [], browserHeadless = True, browserKeepOpenOnFailure = False}]"
      show [EnhancedFetchCount, HardNavigationCount]
        `shouldBe` "[EnhancedFetchCount,HardNavigationCount]"
      show [visitAction, noScriptVisitAction, reloadAction, clickAction, backAction, forwardAction, metricAction]
        `shouldBe` "[VisitUrl \"http://localhost:8080/\",VisitUrlWithoutScripts \"http://localhost:8080/no-js\",ReloadPage,ClickLinkWithText \"Browse the second page\",NavigateHistoryBack,NavigateHistoryForward,AssertNavigationMetricEquals EnhancedFetchCount 1]"
      show [launchError, protocolError]
        `shouldBe` "[BrowserRunnerLaunchError \"missing-browser-runner\",BrowserRunnerProtocolError \"unexpected response\"]"

  describe "runBrowserScript" $ do
    it "runs the configured browser runner and accepts an ok response" $
      withFakeBrowserScript $ \scriptPath -> do
        let browserConfig =
              defaultBrowserConfig
                { browserRunnerCommand = "node",
                  browserRunnerArguments = [scriptPath, "ok"],
                  browserHeadless = False
                }
        runBrowserScript browserConfig [VisitUrl "http://localhost:8080/"]
          `shouldReturn` Right ()

    it "surfaces explicit assertion failures from the runner protocol" $
      withFakeBrowserScript $ \scriptPath -> do
        let browserConfig =
              defaultBrowserConfig
                { browserRunnerCommand = "node",
                  browserRunnerArguments = [scriptPath, "assertion-failure"]
                }
        runBrowserScript browserConfig [ClickLinkWithText "Browse the second page"]
          `shouldReturn` Left (BrowserAssertionFailed "Expected the second page to load")

    it "surfaces malformed runner responses explicitly" $
      withFakeBrowserScript $ \scriptPath -> do
        let browserConfig =
              defaultBrowserConfig
                { browserRunnerCommand = "node",
                  browserRunnerArguments = [scriptPath, "invalid-response"]
                }
        runBrowserScript browserConfig [VisitUrl "http://localhost:8080/"]
          `shouldReturn` Left (BrowserRunnerProtocolError "Unexpected browser runner response: maybe")

    it "surfaces empty runner responses explicitly" $
      withFakeBrowserScript $ \scriptPath -> do
        let browserConfig =
              defaultBrowserConfig
                { browserRunnerCommand = "node",
                  browserRunnerArguments = [scriptPath, "empty-response"]
                }
        runBrowserScript browserConfig [VisitUrl "http://localhost:8080/"]
          `shouldReturn` Left (BrowserRunnerProtocolError "browser runner returned an empty response")

    it "surfaces missing response files explicitly" $
      withFakeBrowserScript $ \scriptPath -> do
        let browserConfig =
              defaultBrowserConfig
                { browserRunnerCommand = "node",
                  browserRunnerArguments = [scriptPath, "missing-response"]
                }
        runBrowserScript browserConfig [VisitUrl "http://localhost:8080/"]
          `shouldReturn` Left (BrowserRunnerProtocolError "browser runner completed without writing a response file")

    it "surfaces non-zero runner exits with stdout and stderr" $
      withFakeBrowserScript $ \scriptPath -> do
        let browserConfig =
              defaultBrowserConfig
                { browserRunnerCommand = "node",
                  browserRunnerArguments = [scriptPath, "process-failure"]
                }
        runBrowserScript browserConfig [VisitUrl "http://localhost:8080/"]
          `shouldReturn` Left (BrowserRunnerProcessError (ExitFailure 4) "runner stdout\n" "runner stderr\n")

    it "surfaces missing runner executables as launch errors" $ do
      result <- runBrowserScript defaultBrowserConfig {browserRunnerCommand = "missing-browser-runner"} [VisitUrl "http://localhost:8080/"]
      errorMessage <- $([|result|] `shouldMatch` [p|Left (BrowserRunnerLaunchError errorMessage)|])
      errorMessage `shouldContain'` "missing-browser-runner"
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
    withFakeBrowserScript action =
      withSystemTempDirectory "browser-runner" $ \tempDirectory -> do
        let scriptPath = tempDirectory </> "fake-browser-runner.js"
        writeFile scriptPath fakeBrowserRunnerSource
        action scriptPath
    fakeBrowserRunnerSource =
      unlines
        [ "const fs = require('fs');",
          "const [mode, requestPath, responsePath] = process.argv.slice(2);",
          "const request = fs.readFileSync(requestPath, 'utf8');",
          "if (!request.includes('headless\\t') || !request.includes('action\\t')) {",
          "  fs.writeFileSync(responsePath, 'error\\tMissing request content\\n');",
          "  process.exit(0);",
          "}",
          "switch (mode) {",
          "  case 'ok':",
          "    fs.writeFileSync(responsePath, 'ok\\n');",
          "    process.exit(0);",
          "  case 'assertion-failure':",
          "    fs.writeFileSync(responsePath, 'error\\tExpected the second page to load\\n');",
          "    process.exit(0);",
          "  case 'invalid-response':",
          "    fs.writeFileSync(responsePath, 'maybe\\n');",
          "    process.exit(0);",
          "  case 'empty-response':",
          "    fs.writeFileSync(responsePath, '');",
          "    process.exit(0);",
          "  case 'missing-response':",
          "    process.exit(0);",
          "  case 'process-failure':",
          "    process.stdout.write('runner stdout\\n');",
          "    process.stderr.write('runner stderr\\n');",
          "    process.exit(4);",
          "  default:",
          "    fs.writeFileSync(responsePath, 'error\\tUnknown fake mode\\n');",
          "    process.exit(0);",
          "}"
        ]
