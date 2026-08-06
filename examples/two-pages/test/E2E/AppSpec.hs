{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# E2E_SPEC #-}

import App.App (buildApplication, buildNativeUploadMiddleware)
import Data.List.NonEmpty (NonEmpty (..))
import HarchWeb (LocalTestServer (..), toWaiApplication, withLocalTestServer, withLocalTestServerForApplication)

spec =
  describe "two-page real-browser behavior" $ do
    it "enhances navigation without replacing complete SSR documents" $
      withBrowserAndServer $ \browser server -> do
        let homeUrl = localServerBaseUrl server <> "/"
            secondUrl = localServerBaseUrl server <> "/second"
        ( runBrowserScenario browser $ do
            visit homeUrl
            assertAll
              ((,,) <$> textContent (byRole Heading `named` "Home") <*> attributeValue (css "link[rel='stylesheet']") "href" <*> attributeValue (css "section[data-page='home']") "class")
              ( \(heading, stylesheetHref, homeClass) ->
                  (heading `shouldBe` "Home")
                    :| [ stylesheetHref `shouldBe` Just "/assets/two-pages.css",
                         homeClass `shouldBe` Just "harch-home-root"
                       ]
              )
            click (byRole Link `named` "Go to the second page")
            assertAll
              ((,,) <$> currentUrl <*> textContent (byRole Heading `named` "Second") <*> browserMetrics)
              ( \(url, heading, metrics) ->
                  (url `shouldBe` secondUrl)
                    :| [ heading `shouldBe` "Second",
                         $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 1, hardNavigationCount = 0}|])
                       ]
              )
          )
          `shouldReturn` Right ()

    it "uses the enhanced path for Back and Forward" $
      withBrowserAndServer $ \browser server -> do
        let homeUrl = localServerBaseUrl server <> "/"
            secondUrl = localServerBaseUrl server <> "/second"
        ( runBrowserScenario browser $ do
            visit homeUrl
            click (byRole Link `named` "Go to the second page")
            assertUrl (`shouldBe` secondUrl)
            historyBack
            assertAll
              ((,) <$> currentUrl <*> textContent (byRole Heading `named` "Home"))
              (\(url, heading) -> (url `shouldBe` homeUrl) :| [heading `shouldBe` "Home"])
            historyForward
            assertAll
              ((,) <$> currentUrl <*> textContent (byRole Heading `named` "Second"))
              (\(url, heading) -> (url `shouldBe` secondUrl) :| [heading `shouldBe` "Second"])
          )
          `shouldReturn` Right ()

    it "captures a submitted control before the deferred module loads, then patches its SSR region" $
      withBrowserAndServer $ \browser server -> do
        let homeUrl = localServerBaseUrl server <> "/"
            subscriptionForm = byRole Form `named` "Subscription"
            emailField = byLabel "Email address"
        ( runBrowserScenario browser $ do
            blockRequestsMatching "**/assets/navigation.js"
            visit homeUrl
            fill emailField "ada@example"
            submit subscriptionForm
            assertAll
              ((,,) <$> currentUrl <*> inputValue emailField <*> browserMetrics)
              ( \(url, email, metrics) ->
                  (url `shouldBe` homeUrl)
                    :| [ email `shouldBe` "ada@example",
                         $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 0, hardNavigationCount = 0, mutationRequestCount = 0}|])
                       ]
              )
            releaseRequestsMatching "**/assets/navigation.js"
            assertAll
              ((,,) <$> textContent (css "#subscription-result") <*> isFocused emailField <*> inputValue emailField)
              ( \(result, focused, email) ->
                  (result `shouldBe` "Enter a valid email address.")
                    :| [focused `shouldBe` True, email `shouldBe` "ada@example"]
              )
            fill emailField "ada@example.com"
            submit subscriptionForm
            assertAll
              ((,) <$> textContent (css "#subscription-result") <*> browserMetrics)
              ( \(result, metrics) ->
                  (result `shouldBe` "Thanks. Your subscription request is ready.")
                    :| [$([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 0, hardNavigationCount = 0, mutationRequestCount = 2}|])]
              )
          )
          `shouldReturn` Right ()

    it "does not perform a native submission for the default exclusive client action when scripts are disabled" $
      withBrowserAndServer $ \browser server -> do
        let homeUrl = localServerBaseUrl server <> "/"
            subscriptionForm = byRole Form `named` "Subscription"
            emailField = byLabel "Email address"
        ( runBrowserScenario browser $ do
            visitWithoutScripts homeUrl
            fill emailField "ada@example.com"
            submit subscriptionForm
            assertAll
              ((,,) <$> currentUrl <*> inputValue emailField <*> browserMetrics)
              ( \(url, email, metrics) ->
                  (url `shouldBe` homeUrl)
                    :| [ email `shouldBe` "ada@example.com",
                         $([|metrics|] `shouldMatch` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 0}|])
                       ]
              )
          )
          `shouldReturn` Right ()

    it "uses an explicitly authored CSRF-protected native fallback when scripts are disabled" $
      withBrowserAndServer $ \browser server -> do
        let homeUrl = localServerBaseUrl server <> "/"
            fallbackForm = byRole Form `named` "Native fallback subscription"
            fallbackEmail = byLabel "Native fallback email address"
        ( runBrowserScenario browser $ do
            visitWithoutScripts homeUrl
            setCookie homeUrl "harch-native-fallback-csrf" "two-pages-native-fallback"
            visitWithoutScripts homeUrl
            fill fallbackEmail "native@example.com"
            submit fallbackForm
            assertAll
              ((,) <$> textContent (byRole Heading `named` "Subscription received") <*> browserMetrics)
              ( \(heading, metrics) ->
                  (heading `shouldBe` "Subscription received")
                    :| [$([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 0, hardNavigationCount = 1, mutationRequestCount = 0}|])]
              )
          )
          `shouldReturn` Right ()

    it "rejects the native fallback when its CSRF cookie is absent" $
      withBrowserAndServer $ \browser server -> do
        let homeUrl = localServerBaseUrl server <> "/"
            fallbackForm = byRole Form `named` "Native fallback subscription"
        ( runBrowserScenario browser $ do
            visitWithoutScripts homeUrl
            submit fallbackForm
            assertAll
              ((,) <$> textContent (css "body") <*> browserMetrics)
              ( \(body, metrics) ->
                  (body `shouldBe` "Native fallback CSRF validation failed.")
                    :| [$([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 0, hardNavigationCount = 1, mutationRequestCount = 0}|])]
              )
          )
          `shouldReturn` Right ()

    it "keeps a permanently blocked action visibly recoverable until the user cancels it" $
      withBrowserAndServer $ \browser server -> do
        let homeUrl = localServerBaseUrl server <> "/"
            subscriptionForm = byRole Form `named` "Subscription"
            emailField = byLabel "Email address"
            actionStatus = within subscriptionForm (css "[data-harch-action-status]")
        ( runBrowserScenario browser $ do
            blockRequestsMatching "**/assets/navigation.js"
            visit homeUrl
            fill emailField "ada@example.com"
            submit subscriptionForm
            assertEventually (textContent actionStatus) (`shouldBe` "Still waiting for this action to be handled.")
            click (byRole Button `named` "Cancel action")
            assertAll
              ((,) <$> textContent actionStatus <*> browserMetrics)
              ( \(status, metrics) ->
                  (status `shouldBe` "Action cancelled.")
                    :| [$([|metrics|] `shouldMatch` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 0}|])]
              )
            releaseRequestsMatching "**/assets/navigation.js"
            assertAll
              ((,) <$> textContent actionStatus <*> browserMetrics)
              ( \(status, metrics) ->
                  (status `shouldBe` "Action cancelled.")
                    :| [$([|metrics|] `shouldMatch` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 0}|])]
              )
          )
          `shouldReturn` Right ()

    it "lets a handler arrive after the liveness threshold without replaying a cancellation" $
      withBrowserAndServer $ \browser server -> do
        let homeUrl = localServerBaseUrl server <> "/"
            subscriptionForm = byRole Form `named` "Subscription"
            emailField = byLabel "Email address"
            actionStatus = within subscriptionForm (css "[data-harch-action-status]")
        ( runBrowserScenario browser $ do
            blockRequestsMatching "**/assets/navigation.js"
            visit homeUrl
            fill emailField "ada@example.com"
            submit subscriptionForm
            assertEventually (textContent actionStatus) (`shouldBe` "Still waiting for this action to be handled.")
            releaseRequestsMatching "**/assets/navigation.js"
            assertAll
              ((,) <$> textContent (css "#subscription-result") <*> browserMetrics)
              ( \(result, metrics) ->
                  (result `shouldBe` "Thanks. Your subscription request is ready.")
                    :| [$([|metrics|] `shouldMatch` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 1}|])]
              )
          )
          `shouldReturn` Right ()

    it "shows immediate recoverable outcomes for throwing and rejected handlers" $
      withBrowserAndServer $ \browser server -> do
        let homeUrl = localServerBaseUrl server <> "/"
            subscriptionForm = byRole Form `named` "Subscription"
            emailField = byLabel "Email address"
            actionStatus = within subscriptionForm (css "[data-harch-action-status]")
            handler =
              "window.__harchCaptureKernel.register(window.__harchCaptureKernel.eventTypes.Submit, (capturedAction) => { const email = capturedAction.fields.find(([name]) => name === 'email')?.[1]; if (email === 'throw@example.com') { throw new Error('test failure'); } return Promise.reject(new Error('test rejection')); });"
        ( runBrowserScenario browser $ do
            blockRequestsMatching "**/assets/navigation.js"
            visit homeUrl
            _ <- runPageScript handler
            fill emailField "throw@example.com"
            submit subscriptionForm
            assertText actionStatus (`shouldBe` "This action needs your attention.")
            fill emailField "reject@example.com"
            submit subscriptionForm
            assertText actionStatus (`shouldBe` "This action needs your attention.")
            assertAttribute (within subscriptionForm (css "[data-harch-action-retry]")) "hidden" (`shouldBe` Just "")
          )
          `shouldReturn` Right ()

    it "retries only declared safe handlers and preserves the idempotency identity for mutation retries" $
      withBrowserAndServer $ \browser server -> do
        let homeUrl = localServerBaseUrl server <> "/"
            subscriptionForm = byRole Form `named` "Subscription"
            emailField = byLabel "Email address"
            actionStatus = within subscriptionForm (css "[data-harch-action-status]")
            retryButton = within subscriptionForm (byRole Button `named` "Retry action")
            handlerSafeRetry =
              "let attempts = 0; document.querySelector('form[data-harch-action=\"true\"]').dataset.harchActionCapabilities = 'handler-safe-retry'; window.__harchCaptureKernel.register(window.__harchCaptureKernel.eventTypes.Submit, (capturedAction, settlement) => { attempts += 1; document.body.dataset.harchRetryEvidence = String(attempts) + ':' + capturedAction.fields.find(([name]) => name === 'email')?.[1]; if (attempts === 1) { return Promise.reject(new Error('recoverable')); } settlement.completed(); });"
            idempotentRetry =
              "let attempts = 0; const form = document.querySelector('form[data-harch-action=\"true\"]'); form.dataset.harchActionCapabilities = 'idempotent-mutation-retry'; form.dataset.harchActionIdempotencyKey = 'mutation-1'; window.__harchCaptureKernel.register(window.__harchCaptureKernel.eventTypes.Submit, (capturedAction, settlement) => { attempts += 1; document.body.dataset.harchIdempotencyEvidence = String(attempts) + ':' + capturedAction.idempotencyKey; if (attempts === 1) { return Promise.reject(new Error('recoverable')); } settlement.completed(); });"
        ( runBrowserScenario browser $ do
            blockRequestsMatching "**/assets/navigation.js"
            visit homeUrl
            _ <- runPageScript handlerSafeRetry
            fill emailField "safe@example.com"
            submit subscriptionForm
            assertText actionStatus (`shouldBe` "This action needs your attention.")
            click retryButton
            assertAll
              ((,) <$> textContent actionStatus <*> attributeValue (css "body") "data-harch-retry-evidence")
              ( \(status, evidence) ->
                  (status `shouldBe` "Completed.")
                    :| [evidence `shouldBe` Just "2:safe@example.com"]
              )
            _ <- runPageScript idempotentRetry
            fill emailField "idempotent@example.com"
            submit subscriptionForm
            assertText actionStatus (`shouldBe` "This action needs your attention.")
            click retryButton
            assertAll
              ((,) <$> textContent actionStatus <*> attributeValue (css "body") "data-harch-idempotency-evidence")
              ( \(status, evidence) ->
                  (status `shouldBe` "Completed.")
                    :| [evidence `shouldBe` Just "2:mutation-1"]
              )
          )
          `shouldReturn` Right ()

    it "keeps an unsettled claim local, rejects stale settlement, and warns only opted-in unresolved actions" $
      withBrowserAndServer $ \browser server -> do
        let homeUrl = localServerBaseUrl server <> "/"
            subscriptionForm = byRole Form `named` "Subscription"
            emailField = byLabel "Email address"
            actionStatus = within subscriptionForm (css "[data-harch-action-status]")
            handler =
              "window.__harchCaptureKernel.register(window.__harchCaptureKernel.eventTypes.Submit, (_capturedAction, settlement) => { window.__harchTestSettlement = settlement; });"
        ( runBrowserScenario browser $ do
            blockRequestsMatching "**/assets/navigation.js"
            visit homeUrl
            _ <- runPageScript "document.querySelector('form[data-harch-action=\"true\"]').dataset.harchActionCapabilities = 'conditional-leave-confirmation';"
            _ <- runPageScript handler
            fill emailField "ada@example.com"
            submit subscriptionForm
            assertEventually (textContent actionStatus) (`shouldBe` "Still waiting for this action to be handled.")
            assertAttribute subscriptionForm "aria-busy" (`shouldBe` Just "true")
            _ <- runPageScript "const event = new Event('beforeunload', { cancelable: true }); window.dispatchEvent(event); document.body.dataset.harchBeforeUnload = String(event.defaultPrevented);"
            assertAttribute (css "body") "data-harch-before-unload" (`shouldBe` Just "true")
            click (byRole Button `named` "Cancel action")
            _ <- runPageScript "document.body.dataset.harchStaleSettlement = String(window.__harchTestSettlement.completed()); const event = new Event('beforeunload', { cancelable: true }); window.dispatchEvent(event); document.body.dataset.harchBeforeUnload = String(event.defaultPrevented);"
            assertAll
              ((,,) <$> textContent actionStatus <*> attributeValue (css "body") "data-harch-stale-settlement" <*> attributeValue (css "body") "data-harch-before-unload")
              ( \(status, staleSettlement, beforeUnload) ->
                  (status `shouldBe` "Action cancelled.")
                    :| [ staleSettlement `shouldBe` Just "false",
                         beforeUnload `shouldBe` Just "false"
                       ]
              )
          )
          `shouldReturn` Right ()

    it "keeps multiple pending controls and their input snapshots independent" $
      withBrowserAndServer $ \browser server -> do
        let homeUrl = localServerBaseUrl server <> "/"
            firstForm = byRole Form `named` "Subscription"
            secondForm = byRole Form `named` "Second subscription"
            firstEmail = within firstForm (byLabel "Email address")
            secondEmail = within secondForm (byLabel "Second email address")
            firstStatus = within firstForm (css "[data-harch-action-status]")
            secondStatus = within secondForm (css "[data-harch-action-status]")
            addSecondControl =
              "const first = document.querySelector('form[data-harch-action=\"true\"]'); const second = first.cloneNode(true); second.setAttribute('aria-label', 'Second subscription'); const label = second.querySelector('label'); const input = second.querySelector('input[name=\"email\"]'); label.htmlFor = 'second-subscription-email'; label.textContent = 'Second email address'; input.id = 'second-subscription-email'; first.after(second);"
            handler =
              "window.__harchCaptureKernel.register(window.__harchCaptureKernel.eventTypes.Submit, () => {});"
        ( runBrowserScenario browser $ do
            blockRequestsMatching "**/assets/navigation.js"
            visit homeUrl
            _ <- runPageScript addSecondControl
            _ <- runPageScript handler
            fill firstEmail "first@example.com"
            fill secondEmail "second@example.com"
            submit firstForm
            submit secondForm
            assertAll
              ((,,,,) <$> textContent firstStatus <*> textContent secondStatus <*> inputValue firstEmail <*> inputValue secondEmail <*> browserMetrics)
              ( \(firstState, secondState, firstValue, secondValue, metrics) ->
                  (firstState `shouldBe` "Still waiting for this action to be handled.")
                    :| [ secondState `shouldBe` "Still waiting for this action to be handled.",
                         firstValue `shouldBe` "first@example.com",
                         secondValue `shouldBe` "second@example.com",
                         $([|metrics|] `shouldMatch` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 0}|])
                       ]
              )
          )
          `shouldReturn` Right ()

    it "reports a deferred-script failure locally after an action has been captured" $
      withBrowserAndServer $ \browser server -> do
        let homeUrl = localServerBaseUrl server <> "/"
            subscriptionForm = byRole Form `named` "Subscription"
            emailField = byLabel "Email address"
            actionStatus = within subscriptionForm (css "[data-harch-action-status]")
        ( runBrowserScenario browser $ do
            blockRequestsMatching "**/assets/navigation.js"
            visit homeUrl
            fill emailField "ada@example.com"
            submit subscriptionForm
            failBlockedRequestsMatching "**/assets/navigation.js"
            assertAll
              ((,) <$> textContent actionStatus <*> browserMetrics)
              ( \(status, metrics) ->
                  (status `shouldBe` "This action needs your attention.")
                    :| [$([|metrics|] `shouldMatch` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 0}|])]
              )
          )
          `shouldReturn` Right ()

    it "keeps reload and script-disabled navigation fully server rendered" $
      withBrowserAndServer $ \browser server -> do
        let homeUrl = localServerBaseUrl server <> "/"
            secondUrl = localServerBaseUrl server <> "/second"
        ( runBrowserScenario browser $ do
            visit secondUrl
            reload
            assertText (byRole Heading `named` "Second") (`shouldBe` "Second")
            visitWithoutScripts homeUrl
            click (byRole Link `named` "Go to the second page")
            assertAll
              ((,,) <$> currentUrl <*> textContent (byRole Heading `named` "Second") <*> browserMetrics)
              ( \(url, heading, metrics) ->
                  (url `shouldBe` secondUrl)
                    :| [ heading `shouldBe` "Second",
                         $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 0, hardNavigationCount = 1}|])
                       ]
              )
          )
          `shouldReturn` Right ()

    it "preserves the server-rendered live status until the optional EventSource module updates it" $
      withBrowserAndServer $ \browser server -> do
        let liveDataUrl = localServerBaseUrl server <> "/live-data"
        ( runBrowserScenario browser $ do
            visitWithoutScripts liveDataUrl
            assertAll
              ((,) <$> textContent (byRole Heading `named` "Live updates") <*> textContent (css "#live-data-status"))
              ( \(heading, status) ->
                  (heading `shouldBe` "Live updates")
                    :| [status `shouldBe` "Waiting for an update."]
              )
            visit liveDataUrl
            assertText (css "#live-data-status") (`shouldBe` "The live update arrived.")
          )
          `shouldReturn` Right ()

    it "submits a native multipart upload as a hard navigation, never through the capture kernel" $
      withBrowserAndNativeUploadServer $ \browser server ->
        withTempFile "native-upload-e2e" [] "attachment.txt" $ \(_tempRoot, filePath) -> do
          writeFile filePath "e2e file contents"
          let uploadUrl = localServerBaseUrl server <> "/native-upload"
          ( runBrowserScenario browser $ do
              visit uploadUrl
              setInputFiles (css "#native-upload-file") filePath
              submit (byRole Form `named` "Upload a file")
              assertAll
                ((,) <$> textContent (byRole Heading `named` "Upload received") <*> browserMetrics)
                ( \(heading, metrics) ->
                    (heading `shouldBe` "Upload received")
                      :| [$([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 0, hardNavigationCount = 1, mutationRequestCount = 0}|])]
                )
            )
            `shouldReturn` Right ()

    it "completes the same native upload flow with scripts disabled" $
      withBrowserAndNativeUploadServer $ \browser server ->
        withTempFile "native-upload-e2e-no-js" [] "attachment.txt" $ \(_tempRoot, filePath) -> do
          writeFile filePath "e2e file contents, no scripts"
          let uploadUrl = localServerBaseUrl server <> "/native-upload"
          ( runBrowserScenario browser $ do
              visitWithoutScripts uploadUrl
              setInputFiles (css "#native-upload-file") filePath
              submit (byRole Form `named` "Upload a file")
              assertText (byRole Heading `named` "Upload received") (`shouldBe` "Upload received")
            )
            `shouldReturn` Right ()

withBrowserAndServer :: (BrowserConfig -> LocalTestServer -> IO a) -> IO a
withBrowserAndServer action = do
  loadedConfig <- loadPlaywrightBrowserConfig
  browser <-
    case loadedConfig of
      Left loadError -> expectationFailure loadError >> fail "unreachable"
      Right config -> pure config
  withLocalTestServer buildApplication (action browser)

withBrowserAndNativeUploadServer :: (BrowserConfig -> LocalTestServer -> IO a) -> IO a
withBrowserAndNativeUploadServer action = do
  loadedConfig <- loadPlaywrightBrowserConfig
  browser <-
    case loadedConfig of
      Left loadError -> expectationFailure loadError >> fail "unreachable"
      Right config -> pure config
  nativeUploadMiddleware <- buildNativeUploadMiddleware
  withLocalTestServerForApplication (nativeUploadMiddleware (toWaiApplication buildApplication)) (action browser)
