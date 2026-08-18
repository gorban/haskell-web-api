{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (AsyncException (ThreadKilled), SomeException, finally, fromException, try)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as AesonEncoding
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Foldable (traverse_)
import Data.IORef (newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as Text
import System.Directory (doesFileExist, withCurrentDirectory)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import TestCore.Browser

data FieldState = FieldState
  { fieldStateValue :: Text.Text,
    fieldStateFocused :: Bool
  }
  deriving (Eq, Show)

spec = do
  describe "browser configuration" $ do
    it "uses the bundled Playwright runner defaults" $
      expectAll
        ( (browserRunnerCommand defaultPlaywrightBrowserConfig `shouldBe` "node")
            :| [ browserRunnerArguments defaultPlaywrightBrowserConfig `shouldBe` ["packages/test-core/playwright-runner/runner.cjs"],
                 browserHeadless defaultPlaywrightBrowserConfig `shouldBe` True,
                 browserPauseOnFailure defaultPlaywrightBrowserConfig `shouldBe` False,
                 browserTimeoutMilliseconds defaultPlaywrightBrowserConfig `shouldBe` 10000,
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
          ("TEST_CORE_BROWSER_ARTIFACT_DIRECTORY", "artifacts/browser")
        ]
        `shouldBe` Right
          BrowserConfig
            { browserRunnerCommand = "custom-node",
              browserRunnerArguments = ["runner.cjs", "--debug"],
              browserHeadless = False,
              browserPauseOnFailure = True,
              browserTimeoutMilliseconds = 2500,
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
                 parseBrowserConfig [("TEST_CORE_BROWSER_TIMEOUT_MILLISECONDS", "later")] `shouldBe` Left "Invalid positive integer for TEST_CORE_BROWSER_TIMEOUT_MILLISECONDS: later"
               ]
        )

    it "resolves the bundled runner and applies environment overrides"
      $ withEnvironment
        [ ("TEST_CORE_BROWSER_HEADLESS", Just "false"),
          ("TEST_CORE_BROWSER_TIMEOUT_MILLISECONDS", Just "3210")
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
                   browserTimeoutMilliseconds config `shouldBe` 3210
                 ]
          )

    it "reports a missing bundled runner when no repository ancestor contains it" $
      withSystemTempDirectory "browser-config" $ \tempDirectory ->
        withCurrentDirectory tempDirectory $ do
          result <- loadPlaywrightBrowserConfig
          result `shouldSatisfy` \case
            Left message -> "Could not find bundled Playwright runner" `Text.isInfixOf` Text.pack message
            Right _ -> False

  describe "runBrowserScenario" $ do
    it "terminates the runner when initialization is interrupted asynchronously" $
      withCancellableFakeRunner $ \config enteredPath -> do
        completion <- newEmptyMVar
        workerThread <-
          forkIO $ do
            result <- try (runBrowserScenario config (pure ())) :: IO (Either SomeException (Either BrowserRunnerError ()))
            putMVar completion result
        let interruptWorker = killThread workerThread
        ( do
            awaitFile enteredPath `shouldReturn` Just ()
            interruptWorker
            completionResult <- timeout 1000000 (takeMVar completion)
            completionResult `shouldSatisfy` interruptedByThreadKilled
          )
          `finally` interruptWorker

    it "uses semantic locators and batches only composed observations" $
      withFakeRunner "normal" $ \config -> do
        let emailField = byLabel "Email address"
            fieldState = FieldState <$> inputValue emailField <*> isFocused emailField
        runBrowserScenario
          config
          ( do
              visit "http://localhost/"
              setCookie "http://localhost/" "session" "opaque-session"
              click (byRole Link `named` "Continue")
              scriptResult <- runPageScript "true"
              fill emailField "person@example.com"
              setInputFiles (byLabel "Attachment") "test-fixtures/attachment.txt"
              submit (byRole Form `named` "Registration")
              blockRequestsMatching "**/enhancements.js"
              releaseRequestsMatching "**/enhancements.js"
              blockRequestsMatching "**/failure.js"
              failBlockedRequestsMatching "**/failure.js"
              assertAll
                ((,,,,) <$> fieldState <*> attributeValue emailField "aria-busy" <*> isVisible (within (byRole Navigation) (byRole Link `named` "Home")) <*> currentUrl <*> browserMetrics)
                ( \(actualFieldState, busyAttribute, homeLinkVisible, url, metrics) ->
                    (actualFieldState `shouldBe` FieldState "person@example.com" True)
                      :| [ busyAttribute `shouldBe` Just "false",
                           homeLinkVisible `shouldBe` True,
                           url `shouldBe` "http://localhost/",
                           metrics `shouldBe` BrowserMetrics 1 0 1
                         ]
                )
              historyBack
              historyForward
              reload
              visitWithoutScripts "http://localhost/no-js"
              pure scriptResult
          )
          `shouldReturn` Right (Aeson.Bool True)

    it "encodes every semantic locator and role while exercising both applicative APIs" $
      withFakeRunner "normal" $ \config -> do
        roleReference <- newIORef Textbox
        runtimeRole <- readIORef roleReference
        let roles = [Button, Checkbox, Form, Heading, Link, List, ListItem, Navigation, Radio, Status, Textbox]
            locators =
              [ byRole Button,
                byLabel "Label" `named` "Named label",
                byText "Text",
                byPlaceholder "Placeholder",
                byAltText "Alternative",
                byTitle "Title",
                byTestId "identifier",
                css "h1",
                containingText (byRole ListItem) "Product",
                within (byRole Navigation) (byRole Link)
              ]
            combinedObservation =
              (\email focused visible url -> (email, focused, visible, url))
                <$> keepLeftObservation (inputValue (byLabel "Email")) (pure ())
                <*> keepRightObservation (pure ()) (isFocused (byLabel "Email"))
                <*> combineObservation (&&) (isVisible (byRole Form)) (isVisible (byRole Heading))
                <*> currentUrl
            appliedScenario = applyScenario (mapScenario (+) (pure 1)) (pure 2)
            scenarioValue = combineScenario (+) appliedScenario (pure 0) :: BrowserScenario Int
        expectAll
          ( (Aeson.toJSONList locators `shouldSatisfy` (not . null . show))
              :| [ show runtimeRole `shouldBe` "Textbox",
                   shows runtimeRole "" `shouldBe` "Textbox",
                   showList [runtimeRole] "" `shouldBe` "[Textbox]",
                   Aeson.omitField (byRole Button) `shouldBe` False,
                   LazyByteString.length (AesonEncoding.encodingToLazyByteString (Aeson.toEncoding (byRole Button))) `shouldSatisfy` (> 0),
                   LazyByteString.length (AesonEncoding.encodingToLazyByteString (Aeson.toEncodingList locators)) `shouldSatisfy` (> 0)
                 ]
          )
        runBrowserScenario
          config
          ( do
              traverse_ (click . byRole) roles
              traverse_ click locators
              applicativeValue <- keepLeftScenario (keepLeftScenario scenarioValue (pure ())) (keepRightScenario (pure ()) (pure ()))
              monadicValue <- bindScenario (scenarioReturn applicativeValue) pure
              assertEventually (pure ("constant" :: Text.Text)) (`shouldBe` "constant")
              assertEventually combinedObservation (`shouldBe` ("person@example.com", True, True, "http://localhost/"))
              pure monadicValue
          )
          `shouldReturn` Right 3

    it "retries Hspec callback failures against fresh observations" $
      withFakeRunner "retry" $ \config ->
        runBrowserScenario
          config
          ( do
              assertText (byRole Heading) (`shouldBe` "Home")
              assertValue (css "input[name=email]") (`shouldBe` "person@example.com")
              assertFocused (byTestId "email") (`shouldBe` True)
          )
          `shouldReturn` Right ()

    it "retains the individual observation assertion helpers" $
      withFakeRunner "normal" $ \config -> do
        attributeResult <- runBrowserScenario config (assertAttribute (byLabel "Email address") "aria-busy" (`shouldBe` Just "false"))
        visibilityResult <- runBrowserScenario config (assertVisible (within (byRole Navigation) (byRole Link `named` "Home")) (`shouldBe` True))
        urlResult <- runBrowserScenario config (assertUrl (`shouldBe` "http://localhost/"))
        metricsResult <- runBrowserScenario config (assertMetrics (`shouldBe` BrowserMetrics 1 0 1))
        expectAll
          ( (attributeResult `shouldBe` Right ())
              :| [ visibilityResult `shouldBe` Right (),
                   urlResult `shouldBe` Right (),
                   metricsResult `shouldBe` Right ()
                 ]
          )

    it "times out with the last callback failure instead of sleeping indefinitely" $
      withFakeRunner "never-match" $ \config -> do
        result <- runBrowserScenario config (assertText (byRole Heading) (`shouldBe` "Home"))
        result `shouldSatisfy` \case
          Left (BrowserAssertionFailed message _) -> "timed out after 250ms" `Text.isInfixOf` Text.pack message
          _ -> False

    it "uses the first failure when a slow initial callback already exceeds the timeout" $
      withFakeRunner "never-match-artifacts" $ \config -> do
        let shortConfig = config {browserTimeoutMilliseconds = 1}
        result <-
          runBrowserScenario shortConfig $
            assertText (byRole Heading) $ \actual -> do
              threadDelay 5000
              actual `shouldBe` "Home"
        result `shouldSatisfy` \case
          Left (BrowserAssertionFailed message artifacts) ->
            "timed out after 1ms" `Text.isInfixOf` Text.pack message
              && artifacts == ["test-results/failure/assertion-trace.zip"]
          _ -> False

    it "does not retry unexpected callback exceptions" $
      withFakeRunner "normal" $ \config -> do
        result <- runBrowserScenario config (assertText (byRole Heading) (\_ -> ioError (userError "callback exploded")))
        result `shouldSatisfy` \case
          Left (BrowserRunnerProtocolError message) -> "callback exploded" `Text.isInfixOf` Text.pack message
          _ -> False

    it "reports command errors and retained artifact paths" $
      withFakeRunner "command-error" $ \config ->
        runBrowserScenario config (click (byText "Missing"))
          `shouldReturn` Left (BrowserCommandFailed 2 "missing element" ["test-results/failure/trace.zip"])

    it "reports malformed protocol responses" $
      withFakeRunner "malformed" $ \config -> do
        result <- runBrowserScenario config (visit "http://localhost/")
        result `shouldSatisfy` \case
          Left (BrowserRunnerProtocolError message) -> "not enough input" `Text.isInfixOf` Text.pack message || "invalid" `Text.isInfixOf` Text.toLower (Text.pack message)
          _ -> False

    it "validates correlation, version, status, and required response fields" $ do
      let protocolFailure mode expected =
            withFakeRunner mode $ \config -> do
              result <- runBrowserScenario config (visit "http://localhost/")
              result `shouldSatisfy` \case
                Left (BrowserRunnerProtocolError message) -> expected `Text.isInfixOf` Text.pack message
                _ -> False
      protocolFailure "wrong-protocol" "Unsupported browser protocol version"
      protocolFailure "wrong-id" "Expected command response"
      protocolFailure "unknown-status" "Unknown browser response status"
      protocolFailure "missing-fields" "key"
      protocolFailure "response-array" "browser command response"

    it "validates observation result shape, count, and leaf types" $ do
      let observationFailure mode observation expected =
            withFakeRunner mode $ \config -> do
              result <- runBrowserScenario config (assertEventually observation (const (pure ())))
              result `shouldSatisfy` \case
                Left (BrowserRunnerProtocolError message) -> expected `Text.isInfixOf` Text.toLower (Text.pack message)
                _ -> False
      observationFailure "observe-not-array" (textContent (byRole Heading)) "array"
      observationFailure "observe-missing" (textContent (byRole Heading)) "omitted"
      observationFailure "observe-extra" (textContent (byRole Heading)) "unexpected observation values"
      observationFailure "observe-bad-type" (textContent (byRole Heading)) "text"
      observationFailure "metrics-invalid" browserMetrics "enhancednavigationfetchcount"
      observationFailure "observe-no-value" (textContent (byRole Heading)) "array"

    it "keeps scenario errors when cleanup also fails and otherwise reports cleanup failures" $ do
      withFakeRunner "scenario-and-finish-error" $ \config ->
        runBrowserScenario config (visit "http://localhost/")
          `shouldReturn` Left (BrowserCommandFailed 2 "scenario failed" [])
      withFakeRunner "finish-error" $ \config ->
        runBrowserScenario config (pure ())
          `shouldReturn` Left (BrowserCommandFailed 2 "finish failed" [])
      withFakeRunner "finish-invalid" $ \config -> do
        result <- runBrowserScenario config (pure ())
        result `shouldSatisfy` \case
          Left (BrowserRunnerProtocolError message) -> "finish response" `Text.isInfixOf` Text.pack message
          _ -> False
      withFakeRunner "exit-failure" $ \config ->
        runBrowserScenario config (pure ())
          `shouldReturn` Left (BrowserRunnerProcessError (ExitFailure 3) "" "browser runner exited unsuccessfully")

    it "handles initialization failures, closed adapters, null values, and omitted optional fields" $ do
      withFakeRunner "init-error" $ \config ->
        runBrowserScenario config (pure ())
          `shouldReturn` Left (BrowserCommandFailed 1 "initialization failed" [])
      withFakeRunner "closed" $ \config -> do
        result <- runBrowserScenario config (visit "http://localhost/")
        result `shouldSatisfy` \case
          Left (BrowserRunnerProtocolError _) -> True
          _ -> False
      withFakeRunner "no-value" $ \config ->
        runBrowserScenario config (visit "http://localhost/") `shouldReturn` Right ()
      withFakeRunner "command-error-no-artifacts" $ \config ->
        runBrowserScenario config (click (byText "Missing"))
          `shouldReturn` Left (BrowserCommandFailed 2 "missing element" [])
      withFakeRunner "finish-no-artifacts" $ \config ->
        runBrowserScenario config (pure ()) `shouldReturn` Right ()
      withFakeRunner "scenario-error-finish-no-artifacts" $ \config ->
        runBrowserScenario config (click (byText "Missing"))
          `shouldReturn` Left (BrowserCommandFailed 2 "missing element" [])

    it "surfaces missing runner executables" $ do
      result <- runBrowserScenario defaultPlaywrightBrowserConfig {browserRunnerCommand = "missing-browser-runner"} (pure ())
      result `shouldSatisfy` \case
        Left (BrowserRunnerLaunchError message) -> "missing-browser-runner" `Text.isInfixOf` Text.pack message
        _ -> False

    it "covers public metric, config, and error instances" $ do
      let metrics = BrowserMetrics 1 2 3
          otherMetrics = BrowserMetrics 9 8 7
          config = defaultPlaywrightBrowserConfig
          processError = BrowserRunnerProcessError (ExitFailure 4) "out" "err"
          assertionError = BrowserAssertionFailed "failed" ["trace.zip"]
          errors =
            [ BrowserRunnerLaunchError "launch",
              processError,
              BrowserRunnerProtocolError "protocol",
              BrowserCommandFailed 4 "command" ["trace.zip"],
              assertionError
            ]
      expectAll
        ( (enhancedNavigationFetchCount metrics `shouldBe` 1)
            :| [ hardNavigationCount metrics `shouldBe` 2,
                 mutationRequestCount metrics `shouldBe` 3,
                 metrics `shouldNotBe` otherMetrics,
                 show [metrics] `shouldContain'` "BrowserMetrics",
                 (Aeson.eitherDecode "[{\"enhancedNavigationFetchCount\":1,\"hardNavigationCount\":2,\"mutationRequestCount\":3}]" :: Either String [BrowserMetrics]) `shouldBe` Right [metrics],
                 (Aeson.eitherDecode "null" :: Either String BrowserMetrics) `shouldSatisfy` \case
                   Left message -> "BrowserMetrics" `Text.isInfixOf` Text.pack message
                   Right _ -> False,
                 show metrics `shouldBe` "BrowserMetrics {enhancedNavigationFetchCount = 1, hardNavigationCount = 2, mutationRequestCount = 3}",
                 (Aeson.omittedField :: Maybe BrowserMetrics) `shouldBe` Nothing,
                 config `shouldNotBe` config {browserHeadless = False},
                 show config `shouldContain'` "BrowserConfig",
                 show [config] `shouldContain'` "BrowserConfig",
                 processError `shouldBe` BrowserRunnerProcessError (ExitFailure 4) "out" "err",
                 assertionError `shouldBe` BrowserAssertionFailed "failed" ["trace.zip"],
                 errors `shouldBe` errors,
                 BrowserRunnerLaunchError "one" `shouldNotBe` BrowserRunnerLaunchError "two",
                 BrowserRunnerProcessError ExitSuccess "out" "err" `shouldNotBe` processError,
                 BrowserRunnerProtocolError "one" `shouldNotBe` BrowserRunnerProtocolError "two",
                 BrowserCommandFailed 1 "one" [] `shouldNotBe` BrowserCommandFailed 2 "two" [],
                 BrowserAssertionFailed "one" [] `shouldNotBe` BrowserAssertionFailed "two" [],
                 show errors `shouldContain'` "BrowserRunnerLaunchError"
               ]
        )
  where
    combineObservation :: (a -> b -> c) -> BrowserObservation a -> BrowserObservation b -> BrowserObservation c
    combineObservation = liftA2
    keepLeftObservation :: BrowserObservation a -> BrowserObservation b -> BrowserObservation a
    keepLeftObservation = (<*)
    keepRightObservation :: BrowserObservation a -> BrowserObservation b -> BrowserObservation b
    keepRightObservation = (*>)
    combineScenario :: (a -> b -> c) -> BrowserScenario a -> BrowserScenario b -> BrowserScenario c
    combineScenario = liftA2
    applyScenario :: BrowserScenario (a -> b) -> BrowserScenario a -> BrowserScenario b
    applyScenario = (<*>)
    mapScenario :: (a -> b) -> BrowserScenario a -> BrowserScenario b
    mapScenario = fmap
    keepLeftScenario :: BrowserScenario a -> BrowserScenario b -> BrowserScenario a
    keepLeftScenario = (<*)
    keepRightScenario :: BrowserScenario a -> BrowserScenario b -> BrowserScenario b
    keepRightScenario = (*>)
    scenarioReturn :: a -> BrowserScenario a
    scenarioReturn = return
    bindScenario :: BrowserScenario a -> (a -> BrowserScenario b) -> BrowserScenario b
    bindScenario = (>>=)

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

    withFakeRunner mode action =
      withSystemTempDirectory "browser-runner" $ \tempDirectory -> do
        let runnerPath = tempDirectory </> "runner.js"
            config =
              defaultPlaywrightBrowserConfig
                { browserRunnerArguments = [runnerPath, mode],
                  browserTimeoutMilliseconds = 250
                }
        writeFile runnerPath fakeRunnerSource
        action config

    withCancellableFakeRunner action =
      withSystemTempDirectory "browser-runner-cancellation" $ \tempDirectory -> do
        let runnerPath = tempDirectory </> "runner.js"
            enteredPath = tempDirectory </> "entered"
            config =
              defaultPlaywrightBrowserConfig
                { browserRunnerArguments = [runnerPath, "hang-initialize", enteredPath],
                  browserTimeoutMilliseconds = 250
                }
        writeFile runnerPath fakeRunnerSource
        action config enteredPath

    awaitFile path =
      -- The clean coverage build can leave the hosted runner briefly starved
      -- while Node is scheduled.  This is only a startup synchronization
      -- bound; once the runner records the initialize command the test still
      -- interrupts the worker immediately.  Keep the bound comfortably above
      -- the observed CI scheduling delay so it cannot race the test's
      -- cancellation assertion.
      timeout 60000000 (waitForFile path)

    waitForFile path = do
      exists <- doesFileExist path
      if exists
        then pure ()
        -- Leave time for the worker thread and its Node child to run.  A
        -- millisecond-scale filesystem polling loop can starve that handshake
        -- under a clean, CPU-contended coverage build.
        else threadDelay 100000 >> waitForFile path

    interruptedByThreadKilled result =
      case result of
        Just (Left exception) ->
          case fromException exception of
            Just ThreadKilled -> True
            _ -> False
        _ -> False

    fakeRunnerSource =
      unlines
        [ "const fs = require('node:fs');",
          "const readline = require('node:readline');",
          "const mode = process.argv[2];",
          "const enteredPath = process.argv[3];",
          "let textAttempts = 0;",
          "const lines = readline.createInterface({ input: process.stdin, crlfDelay: Infinity });",
          "function reply(id, status, value, message, artifacts = []) {",
          "  process.stdout.write(JSON.stringify({ protocol: 1, id, status, value, message, artifacts }) + '\\n');",
          "}",
          "function rawReply(value) { process.stdout.write(JSON.stringify(value) + '\\n'); }",
          "if (mode === 'hang-initialize') {",
          "  // Keep the child alive if stdin closes: the Haskell cleanup must",
          "  // terminate it for the interrupted worker to finish.",
          "  setInterval(() => {}, 60000);",
          "}",
          "(async () => {",
          "  for await (const line of lines) {",
          "    const request = JSON.parse(line);",
          "    if (mode === 'hang-initialize' && request.command === 'initialize') { fs.writeFileSync(enteredPath, 'entered'); continue; }",
          "    if (mode === 'init-error' && request.command === 'initialize') { reply(request.id, 'error', null, 'initialization failed'); continue; }",
          "    if (mode === 'malformed' && request.command === 'visit') { process.stdout.write('{invalid\\n'); continue; }",
          "    if (mode === 'wrong-protocol' && request.command === 'visit') { rawReply({ protocol: 2, id: request.id, status: 'ok', value: null }); continue; }",
          "    if (mode === 'wrong-id' && request.command === 'visit') { rawReply({ protocol: 1, id: request.id + 1, status: 'ok', value: null }); continue; }",
          "    if (mode === 'unknown-status' && request.command === 'visit') { rawReply({ protocol: 1, id: request.id, status: 'mystery', value: null }); continue; }",
          "    if (mode === 'missing-fields' && request.command === 'visit') { rawReply({ protocol: 1, id: request.id }); continue; }",
          "    if (mode === 'response-array' && request.command === 'visit') { rawReply([]); continue; }",
          "    if (mode === 'closed' && request.command === 'visit') { process.exit(0); }",
          "    if (mode === 'no-value' && request.command === 'visit') { rawReply({ protocol: 1, id: request.id, status: 'ok' }); continue; }",
          "    if (mode === 'scenario-and-finish-error' && request.command === 'visit') { reply(request.id, 'error', null, 'scenario failed'); continue; }",
          "    if (mode === 'command-error' && request.command === 'click') { reply(request.id, 'error', null, 'missing element'); continue; }",
          "    if (mode === 'scenario-error-finish-no-artifacts' && request.command === 'click') { reply(request.id, 'error', null, 'missing element'); continue; }",
          "    if (mode === 'command-error-no-artifacts' && request.command === 'click') { rawReply({ protocol: 1, id: request.id, status: 'error', message: 'missing element' }); continue; }",
          "    if (request.command === 'observeMany') {",
          "      if (mode === 'observe-not-array') { reply(request.id, 'ok', { value: 'Home' }); continue; }",
          "      if (mode === 'observe-no-value') { rawReply({ protocol: 1, id: request.id, status: 'ok' }); continue; }",
          "      if (mode === 'observe-missing') { reply(request.id, 'ok', []); continue; }",
          "      if (mode === 'observe-extra') { reply(request.id, 'ok', ['Home', 'extra']); continue; }",
          "      if (mode === 'observe-bad-type') { reply(request.id, 'ok', [123]); continue; }",
          "      if (mode === 'metrics-invalid') { reply(request.id, 'ok', [{ invalid: true }]); continue; }",
          "      const values = request.observations.map((observation) => {",
          "        switch (observation.kind) {",
          "          case 'textContent': return mode === 'never-match' || mode === 'never-match-artifacts' || (mode === 'retry' && textAttempts++ === 0) ? 'Loading' : 'Home';",
          "          case 'inputValue': return 'person@example.com';",
          "          case 'attributeValue': return 'false';",
          "          case 'focused': case 'visible': return true;",
          "          case 'currentUrl': return 'http://localhost/';",
          "          case 'browserMetrics': return { enhancedNavigationFetchCount: 1, hardNavigationCount: 0, mutationRequestCount: 1 };",
          "          default: return null;",
          "        }",
          "      });",
          "      reply(request.id, 'ok', values);",
          "      continue;",
          "    }",
          "    if (request.command === 'runPageScript') { reply(request.id, 'ok', request.source === 'true'); continue; }",
          "    if (request.command === 'finish') {",
          "      if (mode === 'scenario-and-finish-error' || mode === 'finish-error') { reply(request.id, 'error', null, 'finish failed'); return; }",
          "      if (mode === 'finish-invalid') { reply(request.id, 'ok', 'invalid finish value'); return; }",
          "      if (mode === 'finish-no-artifacts' || mode === 'scenario-error-finish-no-artifacts') { reply(request.id, 'ok', {}); return; }",
          "      const artifacts = mode === 'command-error' ? ['test-results/failure/trace.zip'] : mode === 'never-match-artifacts' ? ['test-results/failure/assertion-trace.zip'] : [];",
          "      reply(request.id, 'ok', { artifacts });",
          "      if (mode === 'exit-failure') process.exitCode = 3;",
          "      return;",
          "    }",
          "    reply(request.id, 'ok', null);",
          "  }",
          "})();"
        ]
