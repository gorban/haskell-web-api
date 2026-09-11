{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# SPEC #-}

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (AsyncException (ThreadKilled), SomeException, displayException, finally, fromException, try)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as AesonEncoding
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Foldable (traverse_)
import Data.IORef (newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as Text
import System.Directory (doesFileExist)
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

spec =
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

    it "bounds an unresponsive browser-runner command and cleans up its process" $
      withFakeRunner "unresponsive" $ \config -> do
        result <- runBrowserScenario config (pure ())
        result `shouldBe` Left (BrowserRunnerProtocolError "browser command initialize timed out after 1000ms")

    it "bounds a runner that answers finish but retains an event-loop handle" $
      withFakeRunner "hang-after-finish" $ \config -> do
        result <- timeout 1000000 (runBrowserScenario config (pure ()))
        result
          `shouldBe` Just (Left (BrowserRunnerProtocolError "browser runner did not exit after finish within 250ms"))

    it "keeps protocol transport bounded separately from browser operations" $
      withFakeRunner "delayed-observe" $ \config ->
        runBrowserScenario
          config
            { browserTimeoutMilliseconds = 1,
              browserProtocolTimeoutMilliseconds = 1000
            }
          (assertText (byRole Heading) (`shouldBe` "Home"))
          `shouldReturn` Right ()

    it "uses semantic locators and batches only composed observations" $
      withFakeRunner "normal" $ \config -> do
        let emailField = byLabel "Email address"
            fieldState = FieldState <$> inputValue emailField <*> isFocused emailField
        runBrowserScenario
          config
          ( do
              emulateMobileViewport 320 480
              visit "http://localhost/"
              setCookie "http://localhost/" "session" "opaque-session"
              setViewportSize 320 480
              click (byRole Link `named` "Continue")
              press emailField "Enter"
              paste emailField "pasted@example.com"
              scriptResult <- runPageScript "true"
              fill emailField "person@example.com"
              setInputFiles (byLabel "Attachment") "test-fixtures/attachment.txt"
              submit (byRole Form `named` "Registration")
              blockRequestsMatching "**/enhancements.js"
              waitForBlockedRequestsMatching "**/enhancements.js"
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

    it "batches heterogeneous observed assertions in one browser snapshot" $
      withFakeRunner "aggregate-only" $ \config ->
        runBrowserSpec config $
          assertAllObserved do
            currentUrl `satisfies` (== "http://localhost/")
            $([|Just <$> textContent (byRole Heading)|] `matchesPattern` [p|Just heading@"Home"|])
            $([|browserMetrics|] `matchesPattern` [p|BrowserMetrics {enhancedNavigationFetchCount = 1, hardNavigationCount = 0}|])

    it "matches absent snapshots immediately in one observation attempt" $
      withFakeRunner "snapshot-missing-once" $ \config ->
        runBrowserSpec config $ assertAllObserved $ $([|observeElement (css "#missing")|] `matchesPattern` [p|Nothing|])

    it "retries absent and present snapshots until the requested pattern matches" $ do
      withFakeRunner "snapshot-appears" $ \config ->
        runBrowserSpec config $ assertAllObserved do
          $([|observeElement (byRole Heading)|] `matchesPattern` [p|Just ElementSnapshot {elementText = "Home", elementVisible = True}|])
          $([|observeElement (byLabel "Email")|] `matchesPattern` [p|Just ElementSnapshot {elementValue = Just "person@example.com", elementFocused = True}|])
      withFakeRunner "snapshot-disappears" $ \config ->
        runBrowserSpec config $ assertAllObserved do
          observeElement (byRole Heading) `shouldEqual` Nothing
          observeElement (byLabel "Email") `shouldEqual` Nothing

    it "includes Nothing and the expected snapshot pattern in failure diagnostics" $
      withFakeRunner "snapshot-missing" $ \config -> do
        result <-
          runBrowserScenario config $
            assertAllObserved $
              $([|observeElement (byRole Heading)|] `matchesPattern` [p|Just ElementSnapshot {elementText = "Profile"}|])
        result `shouldSatisfy` \case
          Left (BrowserAssertionFailed message _) ->
            all (`Text.isInfixOf` Text.pack message) ["Nothing", "failed to match pattern", "Just", "ElementSnapshot", "Profile"]
          _ -> False

    it "rejects an empty observed assertion block" $
      withFakeRunner "normal" $ \config -> do
        result <- runBrowserScenario config (assertAllObserved (pure ()))
        result `shouldBe` Left (BrowserRunnerProtocolError "empty observed assertion block")

    it "keeps aggregate observed assertion failures in declaration order" $
      withFakeRunner "normal" $ \config -> do
        result <-
          runBrowserScenario config $ assertAllObserved do
            $([|currentUrl|] `matchesPattern` [p|"https://wrong.example/"|])
            byRole Heading `shouldHaveText` "Wrong heading"
        result `shouldSatisfy` \case
          Left (BrowserAssertionFailed message _) ->
            let rendered = Text.pack message
                (firstFailure, laterFailures) = Text.breakOn "Wrong heading" rendered
             in "https://wrong.example/" `Text.isInfixOf` firstFailure
                  && "failed to match pattern" `Text.isInfixOf` firstFailure
                  && "http://localhost/" `Text.isInfixOf` firstFailure
                  && not (Text.null laterFailures)
          _ -> False

    it "retries a failed aggregate block against fresh snapshots" $
      withFakeRunner "retry" $ \config ->
        runBrowserSpec config $
          assertAllObserved do
            byRole Heading `shouldHaveText` "Home"
            $([|inputValue (css "input[name=email]")|] `matchesPattern` [p|"person@example.com"|])

    it "does not retry an unexpected aggregate matcher exception" $
      withFakeRunner "normal" $ \config -> do
        result <-
          runBrowserScenario config $
            assertAllObserved $
              textContent (byRole Heading) `matches` (\_ -> ioError (userError "aggregate callback exploded"))
        result `shouldSatisfy` \case
          Left (BrowserRunnerProtocolError message) -> "aggregate callback exploded" `Text.isInfixOf` Text.pack message
          _ -> False

    it "renders runner failures at the Hspec boundary" $
      withFakeRunner "command-error" $ \config -> do
        failed <- try (runBrowserSpec config (click (byText "Missing"))) :: IO (Either SomeException ())
        failed `shouldSatisfy` \case
          Left exception -> "BrowserCommandFailed" `Text.isInfixOf` Text.pack (displayException exception)
          Right () -> False

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
      observationFailure "observe-null-text" (textContent (byRole Heading)) "null"
      observationFailure "observe-bad-type" (observeElement (byRole Heading)) "elementsnapshot"
      observationFailure "snapshot-null-text" (observeElement (byRole Heading)) "null"
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

    it "decodes complete element snapshots and distinguishes each property" $ do
      let snapshot = ElementSnapshot "Heading" Nothing True False
          encoded = "{\"elementText\":\"Heading\",\"elementValue\":null,\"elementVisible\":true,\"elementFocused\":false}"
      expectAll
        ( ((Aeson.eitherDecode encoded :: Either String ElementSnapshot) `shouldBe` Right snapshot)
            :| [ elementText snapshot `shouldBe` "Heading",
                 elementValue snapshot `shouldBe` Nothing,
                 elementVisible snapshot `shouldBe` True,
                 elementFocused snapshot `shouldBe` False,
                 snapshot `shouldNotBe` snapshot {elementText = "Other"},
                 snapshot `shouldNotBe` snapshot {elementValue = Just ""},
                 snapshot `shouldNotBe` snapshot {elementVisible = False},
                 snapshot `shouldNotBe` snapshot {elementFocused = True},
                 show [snapshot] `shouldContain'` "elementValue = Nothing",
                 (Aeson.eitherDecode "{}" :: Either String ElementSnapshot) `shouldSatisfy` \case
                   Left message -> "elementText" `Text.isInfixOf` Text.pack message
                   Right _ -> False
               ]
        )

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

    withFakeRunner mode action =
      withSystemTempDirectory "browser-runner" $ \tempDirectory -> do
        let runnerPath = tempDirectory </> "runner.js"
            config =
              defaultPlaywrightBrowserConfig
                { browserRunnerArguments = [runnerPath, mode],
                  browserTimeoutMilliseconds = 250,
                  browserProtocolTimeoutMilliseconds = 250
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
                  browserTimeoutMilliseconds = 250,
                  browserProtocolTimeoutMilliseconds = 250
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
          "let snapshotAttempts = 0;",
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
          "if (mode === 'unresponsive') { setInterval(() => {}, 60000); }",
          "if (mode === 'hang-after-finish') { setInterval(() => {}, 60000); }",
          "(async () => {",
          "  for await (const line of lines) {",
          "    const request = JSON.parse(line);",
          "    if (mode === 'hang-initialize' && request.command === 'initialize') { fs.writeFileSync(enteredPath, 'entered'); continue; }",
          "    if (mode === 'unresponsive' && request.command === 'initialize') { continue; }",
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
          "      if (mode === 'delayed-observe') { await new Promise((resolve) => setTimeout(resolve, 500)); }",
          "      if (mode === 'aggregate-only' && request.observations.length !== 3) { reply(request.id, 'error', null, 'expected one three-observation snapshot'); continue; }",
          "      if (mode === 'observe-not-array') { reply(request.id, 'ok', { value: 'Home' }); continue; }",
          "      if (mode === 'observe-no-value') { rawReply({ protocol: 1, id: request.id, status: 'ok' }); continue; }",
          "      if (mode === 'observe-missing') { reply(request.id, 'ok', []); continue; }",
          "      if (mode === 'observe-extra') { reply(request.id, 'ok', ['Home', 'extra']); continue; }",
          "      if (mode === 'observe-null-text') { reply(request.id, 'ok', [null]); continue; }",
          "      if (mode === 'observe-bad-type') { reply(request.id, 'ok', [123]); continue; }",
          "      if (mode === 'metrics-invalid') { reply(request.id, 'ok', [{ invalid: true }]); continue; }",
          "      snapshotAttempts++;",
          "      if (mode === 'snapshot-missing-once' && snapshotAttempts > 1) { reply(request.id, 'error', null, 'unexpected retry'); continue; }",
          "      const snapshotMissing = mode.startsWith('snapshot-missing') || (mode === 'snapshot-appears' && snapshotAttempts === 1) || (mode === 'snapshot-disappears' && snapshotAttempts > 1);",
          "      const values = request.observations.map((observation) => {",
          "        switch (observation.kind) {",
          "          case 'textContent': return mode === 'never-match' || mode === 'never-match-artifacts' || (mode === 'retry' && textAttempts++ === 0) ? 'Loading' : 'Home';",
          "          case 'elementSnapshot': return snapshotMissing ? null : { elementText: mode === 'snapshot-null-text' ? null : 'Home', elementValue: 'person@example.com', elementVisible: true, elementFocused: true };",
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
