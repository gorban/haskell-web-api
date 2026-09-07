{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# E2E_SPEC #-}

import App.App qualified as App
import App.Routes (TwoPageRoute)
import HarchWeb (LocalTestServer (..), withLocalTestServer)
import HarchWeb qualified
import HarchWeb.Csrf (generateCsrfToken)

testCsrfProtection :: HarchWeb.CsrfProtection ()
testCsrfProtection =
  HarchWeb.CsrfProtection
    { HarchWeb.issueCsrfToken = const ((`HarchWeb.CsrfTokenIssued` HarchWeb.defaultCsrfCookieMaxAgeSeconds) <$> generateCsrfToken),
      HarchWeb.verifyCsrfToken = \_ _ -> pure HarchWeb.CsrfVerified
    }

buildApplication :: HarchWeb.Application TwoPageRoute App.TwoPageAction () ()
buildApplication = App.buildApplication testCsrfProtection

spec =
  describe "two-page real-browser behavior" $ do
    it "enhances navigation without replacing complete SSR documents" $
      withBrowserAndServer $ \browser server -> do
        let homeUrl = localServerBaseUrl server <> "/"
            secondUrl = localServerBaseUrl server <> "/second"
        runBrowserSpec browser do
          visit homeUrl
          assertAllObserved do
            textContent (byRole Heading `named` "Home") `matches` (`shouldBe` "Home")
            attributeValue (css "link[href='/assets/two-pages.css']") "href" `matches` (`shouldBe` Just "/assets/two-pages.css")
            attributeValue (css "section[data-page='home']") "class" `matches` (`shouldBe` Just "harch-home-root")
          click (byRole Link `named` "Go to the second page")
          assertAllObserved do
            currentUrl `matches` (`shouldBe` secondUrl)
            textContent (byRole Heading `named` "Second") `matches` (`shouldBe` "Second")
            browserMetrics `matches` \metrics ->
              $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 1, hardNavigationCount = 0}|])

    it "uses the enhanced path for Back and Forward" $
      withBrowserAndServer $ \browser server -> do
        let homeUrl = localServerBaseUrl server <> "/"
            secondUrl = localServerBaseUrl server <> "/second"
        runBrowserSpec browser do
          visit homeUrl
          click (byRole Link `named` "Go to the second page")
          assertAllObserved do
            currentUrl `matches` (`shouldBe` secondUrl)
          historyBack
          assertAllObserved do
            currentUrl `matches` (`shouldBe` homeUrl)
            textContent (byRole Heading `named` "Home") `matches` (`shouldBe` "Home")
          historyForward
          assertAllObserved do
            currentUrl `matches` (`shouldBe` secondUrl)
            textContent (byRole Heading `named` "Second") `matches` (`shouldBe` "Second")

    it "captures a submitted control before the deferred module loads, then settles its patch before navigating" $
      withBrowserAndServer $ \browser server -> do
        let homeUrl = localServerBaseUrl server <> "/"
            subscriptionForm = byRole Form `named` "Subscription"
            emailField = byLabel "Email address"
        runBrowserSpec browser do
          blockRequestsMatching "**/assets/navigation.js"
          visit homeUrl
          fill emailField "ada@example"
          submit subscriptionForm
          assertAllObserved do
            currentUrl `matches` (`shouldBe` homeUrl)
            inputValue emailField `matches` (`shouldBe` "ada@example")
            browserMetrics `matches` \metrics ->
              $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 0, hardNavigationCount = 0, mutationRequestCount = 0}|])
          releaseRequestsMatching "**/assets/navigation.js"
          assertAllObserved do
            textContent (css "#subscription-result") `matches` (`shouldBe` "Enter a valid email address.")
            isFocused emailField `satisfies` id
            inputValue emailField `matches` (`shouldBe` "ada@example")
          fill emailField "ada@example.com"
          submit subscriptionForm
          assertAllObserved do
            textContent (byRole Heading `named` "Subscription received") `matches` (`shouldBe` "Subscription received")
            browserMetrics `matches` \metrics ->
              $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 1, hardNavigationCount = 0, mutationRequestCount = 2}|])

    it "does not perform a native submission for the default exclusive client action when scripts are disabled" $
      withBrowserAndServer $ \browser server -> do
        let homeUrl = localServerBaseUrl server <> "/"
            subscriptionForm = byRole Form `named` "Subscription"
            emailField = byLabel "Email address"
        runBrowserSpec browser do
          visitWithoutScripts homeUrl
          fill emailField "ada@example.com"
          submit subscriptionForm
          assertAllObserved do
            currentUrl `matches` (`shouldBe` homeUrl)
            inputValue emailField `matches` (`shouldBe` "ada@example.com")
            browserMetrics `matches` \metrics ->
              $([|metrics|] `shouldMatch` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 0}|])

    it "uses an explicitly authored CSRF-protected native fallback when scripts are disabled" $
      withBrowserAndServer $ \browser server -> do
        let homeUrl = localServerBaseUrl server <> "/"
            fallbackForm = byRole Form `named` "Native fallback subscription"
            fallbackEmail = byLabel "Native fallback email address"
        runBrowserSpec browser do
          visitWithoutScripts homeUrl
          fill fallbackEmail "native@example.com"
          submit fallbackForm
          assertAllObserved do
            textContent (byRole Heading `named` "Subscription received") `matches` (`shouldBe` "Subscription received")
            browserMetrics `matches` \metrics ->
              $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 0, hardNavigationCount = 2, mutationRequestCount = 0}|])

    it "keeps a permanently blocked action visibly recoverable until the user cancels it" $
      withBrowserAndServer $ \browser server -> do
        let homeUrl = localServerBaseUrl server <> "/"
            subscriptionForm = byRole Form `named` "Subscription"
            emailField = byLabel "Email address"
            actionStatus = within subscriptionForm (css "[data-harch-action-status]")
        runBrowserSpec browser do
          blockRequestsMatching "**/assets/navigation.js"
          visit homeUrl
          fill emailField "ada@example.com"
          submit subscriptionForm
          assertAllObserved do
            textContent actionStatus `matches` (`shouldBe` "Still waiting for this action to be handled.")
          click (byRole Button `named` "Cancel action")
          assertAllObserved do
            textContent actionStatus `matches` (`shouldBe` "Action cancelled.")
            browserMetrics `matches` \metrics ->
              $([|metrics|] `shouldMatch` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 0}|])
          releaseRequestsMatching "**/assets/navigation.js"
          assertAllObserved do
            textContent actionStatus `matches` (`shouldBe` "Action cancelled.")
            browserMetrics `matches` \metrics ->
              $([|metrics|] `shouldMatch` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 0}|])

    it "lets a handler arrive after the liveness threshold without replaying a cancellation" $
      withBrowserAndServer $ \browser server -> do
        let homeUrl = localServerBaseUrl server <> "/"
            subscriptionForm = byRole Form `named` "Subscription"
            emailField = byLabel "Email address"
            actionStatus = within subscriptionForm (css "[data-harch-action-status]")
        runBrowserSpec browser do
          blockRequestsMatching "**/assets/navigation.js"
          visit homeUrl
          fill emailField "ada@example.com"
          submit subscriptionForm
          assertAllObserved do
            textContent actionStatus `matches` (`shouldBe` "Still waiting for this action to be handled.")
          releaseRequestsMatching "**/assets/navigation.js"
          assertAllObserved do
            textContent (byRole Heading `named` "Subscription received") `matches` (`shouldBe` "Subscription received")
            browserMetrics `matches` \metrics ->
              $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 1, hardNavigationCount = 0, mutationRequestCount = 1}|])

    it "does not present a delayed response after its action is cancelled" $
      withBrowserAndServer $ \browser server -> do
        let homeUrl = localServerBaseUrl server <> "/"
            subscriptionForm = byRole Form `named` "Subscription"
            emailField = byLabel "Email address"
            actionStatus = within subscriptionForm (css "[data-harch-action-status]")
            actionResponseMarker =
              "const originalFetch = window.fetch.bind(window); window.fetch = async (...arguments_) => { const response = await originalFetch(...arguments_); if (!String(arguments_[0]).includes('/actions/subscribe')) { return response; } const decode = response.json.bind(response); response.json = async () => { const value = await decode(); document.body.dataset.harchActionResponseDecoded = 'true'; return value; }; return response; };"
        runBrowserSpec browser do
          blockRequestsMatching "**/actions/subscribe"
          visit homeUrl
          _ <- runPageScript actionResponseMarker
          fill emailField "cancelled@example.com"
          submit subscriptionForm
          waitForBlockedRequestsMatching "**/actions/subscribe"
          click (byRole Button `named` "Cancel action")
          releaseRequestsMatching "**/actions/subscribe"
          assertAllObserved do
            attributeValue (css "body") "data-harch-action-response-decoded" `matches` (`shouldBe` Just "true")
          assertAllObserved do
            currentUrl `matches` (`shouldBe` homeUrl)
            textContent actionStatus `matches` (`shouldBe` "Action cancelled.")
            browserMetrics `matches` \metrics ->
              $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 0, hardNavigationCount = 0, mutationRequestCount = 1}|])

    it "does not let a delayed action replace a page selected by navigation" $
      withBrowserAndServer $ \browser server -> do
        let secondUrl = localServerBaseUrl server <> "/second"
            subscriptionForm = byRole Form `named` "Subscription"
            emailField = byLabel "Email address"
            actionResponseMarker =
              "const originalFetch = window.fetch.bind(window); window.fetch = async (...arguments_) => { const response = await originalFetch(...arguments_); if (!String(arguments_[0]).includes('/actions/subscribe')) { return response; } const decode = response.json.bind(response); response.json = async () => { const value = await decode(); document.body.dataset.harchActionResponseDecoded = 'true'; return value; }; return response; };"
        runBrowserSpec browser do
          blockRequestsMatching "**/actions/subscribe"
          visit (localServerBaseUrl server <> "/")
          _ <- runPageScript actionResponseMarker
          fill emailField "navigation@example.com"
          submit subscriptionForm
          waitForBlockedRequestsMatching "**/actions/subscribe"
          click (byRole Link `named` "Go to the second page")
          assertAllObserved do
            currentUrl `matches` (`shouldBe` secondUrl)
          releaseRequestsMatching "**/actions/subscribe"
          assertAllObserved do
            attributeValue (css "body") "data-harch-action-response-decoded" `matches` (`shouldBe` Just "true")
          assertAllObserved do
            currentUrl `matches` (`shouldBe` secondUrl)
            textContent (byRole Heading `named` "Second") `matches` (`shouldBe` "Second")
            browserMetrics `matches` \metrics ->
              $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 1, hardNavigationCount = 0, mutationRequestCount = 1}|])

    it "supersedes an earlier delayed submission from the same control" $
      withBrowserAndServer $ \browser server -> do
        let subscriptionForm = byRole Form `named` "Subscription"
            emailField = byLabel "Email address"
        runBrowserSpec browser do
          blockRequestsMatching "**/actions/subscribe"
          visit (localServerBaseUrl server <> "/")
          fill emailField "first@example.com"
          submit subscriptionForm
          waitForBlockedRequestsMatching "**/actions/subscribe"
          fill emailField "second@example.com"
          submit subscriptionForm
          assertAllObserved do
            (mutationRequestCount <$> browserMetrics) `matches` (`shouldBe` 2)
          releaseRequestsMatching "**/actions/subscribe"
          assertAllObserved do
            textContent (byRole Heading `named` "Subscription received") `matches` (`shouldBe` "Subscription received")
            browserMetrics `matches` \metrics ->
              $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 1, hardNavigationCount = 0, mutationRequestCount = 2}|])

    it "shows immediate recoverable outcomes for throwing and rejected handlers" $
      withBrowserAndServer $ \browser server -> do
        let homeUrl = localServerBaseUrl server <> "/"
            subscriptionForm = byRole Form `named` "Subscription"
            emailField = byLabel "Email address"
            actionStatus = within subscriptionForm (css "[data-harch-action-status]")
            handler =
              "window.__harchCaptureKernel.register(window.__harchCaptureKernel.eventTypes.Submit, (capturedAction) => { const email = capturedAction.fields.find(([name]) => name === 'email')?.[1]; if (email === 'throw@example.com') { throw new Error('test failure'); } return Promise.reject(new Error('test rejection')); });"
        runBrowserSpec browser do
          blockRequestsMatching "**/assets/navigation.js"
          visit homeUrl
          _ <- runPageScript handler
          fill emailField "throw@example.com"
          submit subscriptionForm
          assertAllObserved do
            textContent actionStatus `matches` (`shouldBe` "This action needs your attention.")
          fill emailField "reject@example.com"
          submit subscriptionForm
          assertAllObserved do
            textContent actionStatus `matches` (`shouldBe` "This action needs your attention.")
            attributeValue (within subscriptionForm (css "[data-harch-action-retry]")) "hidden" `matches` (`shouldBe` Just "")

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
        runBrowserSpec browser do
          blockRequestsMatching "**/assets/navigation.js"
          visit homeUrl
          _ <- runPageScript handlerSafeRetry
          fill emailField "safe@example.com"
          submit subscriptionForm
          assertAllObserved do
            textContent actionStatus `matches` (`shouldBe` "This action needs your attention.")
          click retryButton
          assertAllObserved do
            textContent actionStatus `matches` (`shouldBe` "Completed.")
            attributeValue (css "body") "data-harch-retry-evidence" `matches` (`shouldBe` Just "2:safe@example.com")
          _ <- runPageScript idempotentRetry
          fill emailField "idempotent@example.com"
          submit subscriptionForm
          assertAllObserved do
            textContent actionStatus `matches` (`shouldBe` "This action needs your attention.")
          click retryButton
          assertAllObserved do
            textContent actionStatus `matches` (`shouldBe` "Completed.")
            attributeValue (css "body") "data-harch-idempotency-evidence" `matches` (`shouldBe` Just "2:mutation-1")

    it "keeps an unsettled claim local, rejects stale settlement, and warns only opted-in unresolved actions" $
      withBrowserAndServer $ \browser server -> do
        let homeUrl = localServerBaseUrl server <> "/"
            subscriptionForm = byRole Form `named` "Subscription"
            emailField = byLabel "Email address"
            actionStatus = within subscriptionForm (css "[data-harch-action-status]")
            handler =
              "window.__harchCaptureKernel.register(window.__harchCaptureKernel.eventTypes.Submit, (_capturedAction, settlement) => { window.__harchTestSettlement = settlement; });"
        runBrowserSpec browser do
          blockRequestsMatching "**/assets/navigation.js"
          visit homeUrl
          _ <- runPageScript "document.querySelector('form[data-harch-action=\"true\"]').dataset.harchActionCapabilities = 'conditional-leave-confirmation';"
          _ <- runPageScript handler
          fill emailField "ada@example.com"
          submit subscriptionForm
          assertAllObserved do
            textContent actionStatus `matches` (`shouldBe` "Still waiting for this action to be handled.")
            attributeValue subscriptionForm "aria-busy" `matches` (`shouldBe` Just "true")
          _ <- runPageScript "const event = new Event('beforeunload', { cancelable: true }); window.dispatchEvent(event); document.body.dataset.harchBeforeUnload = String(event.defaultPrevented);"
          assertAllObserved do
            attributeValue (css "body") "data-harch-before-unload" `matches` (`shouldBe` Just "true")
          click (byRole Button `named` "Cancel action")
          _ <- runPageScript "document.body.dataset.harchStaleSettlement = String(window.__harchTestSettlement.completed()); const event = new Event('beforeunload', { cancelable: true }); window.dispatchEvent(event); document.body.dataset.harchBeforeUnload = String(event.defaultPrevented);"
          assertAllObserved do
            textContent actionStatus `matches` (`shouldBe` "Action cancelled.")
            attributeValue (css "body") "data-harch-stale-settlement" `matches` (`shouldBe` Just "false")
            attributeValue (css "body") "data-harch-before-unload" `matches` (`shouldBe` Just "false")

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
        runBrowserSpec browser do
          blockRequestsMatching "**/assets/navigation.js"
          visit homeUrl
          _ <- runPageScript addSecondControl
          _ <- runPageScript handler
          fill firstEmail "first@example.com"
          fill secondEmail "second@example.com"
          submit firstForm
          submit secondForm
          assertAllObserved do
            textContent firstStatus `matches` (`shouldBe` "Still waiting for this action to be handled.")
            textContent secondStatus `matches` (`shouldBe` "Still waiting for this action to be handled.")
            inputValue firstEmail `matches` (`shouldBe` "first@example.com")
            inputValue secondEmail `matches` (`shouldBe` "second@example.com")
            browserMetrics `matches` \metrics ->
              $([|metrics|] `shouldMatch` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 0}|])

    it "reports a deferred-script failure locally after an action has been captured" $
      withBrowserAndServer $ \browser server -> do
        let homeUrl = localServerBaseUrl server <> "/"
            subscriptionForm = byRole Form `named` "Subscription"
            emailField = byLabel "Email address"
            actionStatus = within subscriptionForm (css "[data-harch-action-status]")
        runBrowserSpec browser do
          blockRequestsMatching "**/assets/navigation.js"
          visit homeUrl
          fill emailField "ada@example.com"
          submit subscriptionForm
          failBlockedRequestsMatching "**/assets/navigation.js"
          assertAllObserved do
            textContent actionStatus `matches` (`shouldBe` "This action needs your attention.")
            browserMetrics `matches` \metrics ->
              $([|metrics|] `shouldMatch` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 0}|])

    it "keeps reload and script-disabled navigation fully server rendered" $
      withBrowserAndServer $ \browser server -> do
        let homeUrl = localServerBaseUrl server <> "/"
            secondUrl = localServerBaseUrl server <> "/second"
        runBrowserSpec browser do
          visit secondUrl
          reload
          assertAllObserved do
            textContent (byRole Heading `named` "Second") `matches` (`shouldBe` "Second")
          visitWithoutScripts homeUrl
          click (byRole Link `named` "Go to the second page")
          assertAllObserved do
            currentUrl `matches` (`shouldBe` secondUrl)
            textContent (byRole Heading `named` "Second") `matches` (`shouldBe` "Second")
            browserMetrics `matches` \metrics ->
              $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 0, hardNavigationCount = 1}|])

    it "preserves the server-rendered live status until the optional EventSource module updates it" $
      withBrowserAndServer $ \browser server -> do
        let liveDataUrl = localServerBaseUrl server <> "/live-data"
        runBrowserSpec browser do
          visitWithoutScripts liveDataUrl
          assertAllObserved do
            textContent (byRole Heading `named` "Live updates") `matches` (`shouldBe` "Live updates")
            textContent (css "#live-data-status") `matches` (`shouldBe` "Waiting for an update.")
          visit liveDataUrl
          assertAllObserved do
            textContent (css "#live-data-status") `matches` (`shouldBe` "The live update arrived.")

    it "reconciles declared page enhancements across enhanced navigation and history" $
      withBrowserAndServer $ \browser server -> do
        let homeUrl = localServerBaseUrl server <> "/"
            liveDataUrl = localServerBaseUrl server <> "/live-data"
        runBrowserSpec browser do
          visit homeUrl
          assertAllObserved do
            textContent (css "[data-home-enhancement-status]") `matches` (`shouldBe` "The page-scoped home enhancement is ready.")
          click (byRole Link `named` "See live updates")
          assertAllObserved do
            textContent (css "#live-data-status") `matches` (`shouldBe` "The live update arrived.")
          click (byRole Link `named` "Home")
          assertAllObserved do
            textContent (css "[data-home-enhancement-status]") `matches` (`shouldBe` "The page-scoped home enhancement is ready.")
          historyBack
          assertAllObserved do
            currentUrl `matches` (`shouldBe` liveDataUrl)
            textContent (css "#live-data-status") `matches` (`shouldBe` "The live update arrived.")
            browserMetrics `matches` \metrics ->
              $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 3, hardNavigationCount = 0}|])

    it "supersedes a pending page enhancement before it can mount stale behavior" $
      withBrowserAndServer $ \browser server -> do
        let homeUrl = localServerBaseUrl server <> "/"
            secondUrl = localServerBaseUrl server <> "/second"
        runBrowserSpec browser do
          blockRequestsMatching "**/assets/live-data.js"
          visit homeUrl
          click (byRole Link `named` "See live updates")
          waitForBlockedRequestsMatching "**/assets/live-data.js"
          click (byRole Link `named` "Go to the second page")
          failBlockedRequestsMatching "**/assets/live-data.js"
          assertAllObserved do
            currentUrl `matches` (`shouldBe` secondUrl)
            textContent (byRole Heading `named` "Second") `matches` (`shouldBe` "Second")
            browserMetrics `matches` \metrics ->
              $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 2, hardNavigationCount = 0}|])

    it "uses a native SSR navigation when a declared page enhancement fails to load" $
      withBrowserAndServer $ \browser server -> do
        let homeUrl = localServerBaseUrl server <> "/"
            liveDataUrl = localServerBaseUrl server <> "/live-data"
        runBrowserSpec browser do
          blockRequestsMatching "**/assets/live-data.js"
          visit homeUrl
          click (byRole Link `named` "See live updates")
          waitForBlockedRequestsMatching "**/assets/live-data.js"
          failBlockedRequestsMatching "**/assets/live-data.js"
          assertAllObserved do
            currentUrl `matches` (`shouldBe` liveDataUrl)
            textContent (byRole Heading `named` "Live updates") `matches` (`shouldBe` "Live updates")
            browserMetrics `matches` \metrics ->
              $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 1, hardNavigationCount = 1}|])

withBrowserAndServer :: (BrowserConfig -> LocalTestServer -> IO a) -> IO a
withBrowserAndServer action = do
  loadedConfig <- loadPlaywrightBrowserConfig
  browser <-
    case loadedConfig of
      Left loadError -> expectationFailure loadError >> fail "unreachable"
      Right config -> pure config
  withLocalTestServer buildApplication (action browser)
