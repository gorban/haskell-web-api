{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# E2E_SPEC #-}

import App.App qualified as App
import App.Routes (TwoPageRoute)
import Data.Text qualified as Text
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

-- | One application serves independent browser sessions concurrently. Hspec
-- owns the shared bracket; each runBrowserSpec still owns its client state.
spec =
  beforeAll requirePlaywrightBrowserConfig $
    aroundAllWith withBrowserAndServer $
      parallel $
        describe "two-page real-browser behavior" $ do
          it "enhances navigation without replacing complete SSR documents" $ \(browser, server) -> do
            let homeUrl = localServerBaseUrl server <> "/"
                secondUrl = localServerBaseUrl server <> "/second"
            runBrowserSpec browser do
              visit homeUrl
              assertAllObserved do
                textContent (byRole Heading `named` "Home") `shouldEqual` "Home"
                attributeValue (css "link[href='/assets/two-pages.css']") "href" `shouldEqual` Just "/assets/two-pages.css"
                attributeValue (css "section[data-page='home']") "class" `shouldEqual` Just "harch-home-root"
              click (byRole Link `named` "Go to the second page")
              assertAllObserved do
                currentUrl `shouldEqual` secondUrl
                textContent (byRole Heading `named` "Second") `shouldEqual` "Second"
                $([|browserMetrics|] `matchesPattern` [p|BrowserMetrics {enhancedNavigationFetchCount = 1, hardNavigationCount = 0}|])

          it "uses the enhanced path for Back and Forward" $ \(browser, server) -> do
            let homeUrl = localServerBaseUrl server <> "/"
                secondUrl = localServerBaseUrl server <> "/second"
            runBrowserSpec browser do
              visit homeUrl
              click (byRole Link `named` "Go to the second page")
              assertAllObserved do
                currentUrl `shouldEqual` secondUrl
              historyBack
              assertAllObserved do
                currentUrl `shouldEqual` homeUrl
                textContent (byRole Heading `named` "Home") `shouldEqual` "Home"
              historyForward
              assertAllObserved do
                currentUrl `shouldEqual` secondUrl
                textContent (byRole Heading `named` "Second") `shouldEqual` "Second"

          it "captures a submitted control before the deferred module loads, then settles its patch before navigating" $ \(browser, server) -> do
            let homeUrl = localServerBaseUrl server <> "/"
                subscriptionForm = byRole Form `named` "Subscription"
                emailField = byLabel "Email address"
            runBrowserSpec browser do
              blockRequestsMatching "**/assets/navigation.js"
              visit homeUrl
              fill emailField "ada@example"
              submit subscriptionForm
              assertAllObserved do
                currentUrl `shouldEqual` homeUrl
                inputValue emailField `shouldEqual` "ada@example"
                $([|browserMetrics|] `matchesPattern` [p|BrowserMetrics {enhancedNavigationFetchCount = 0, hardNavigationCount = 0, mutationRequestCount = 0}|])
              releaseRequestsMatching "**/assets/navigation.js"
              assertAllObserved do
                textContent (css "#subscription-result") `shouldEqual` "Enter a valid email address."
                isFocused emailField `satisfies` id
                inputValue emailField `shouldEqual` "ada@example"
              fill emailField "ada@example.com"
              submit subscriptionForm
              assertAllObserved do
                textContent (byRole Heading `named` "Subscription received") `shouldEqual` "Subscription received"
                $([|browserMetrics|] `matchesPattern` [p|BrowserMetrics {enhancedNavigationFetchCount = 1, hardNavigationCount = 0, mutationRequestCount = 2}|])

          it "clears the authenticated document through the typed failure page when controlled storage cleanup fails" $ \(browser, server) -> do
            let homeUrl = localServerBaseUrl server <> "/"
                failureUrlPrefix = localServerBaseUrl server <> "/client-action-failure/storage-cleanup-failed/"
                subscriptionForm = byRole Form `named` "Subscription"
                emailField = byLabel "Email address"
                injectStorageCleanupFailure =
                  "const originalFetch = window.fetch.bind(window); window.fetch = async (...arguments_) => { const response = await originalFetch(...arguments_); if (!String(arguments_[0]).includes('/actions/subscribe')) { return response; } const decode = response.json.bind(response); response.json = async () => ({ ...(await decode()), storageCleanup: [{ storage: 'local', key: 'controlled-test-key' }] }); return response; }; Storage.prototype.removeItem = function () { throw new Error('controlled storage cleanup failure'); };"
            runBrowserSpec browser do
              visit homeUrl
              _ <- runPageScript injectStorageCleanupFailure
              fill emailField "ada@example.com"
              submit subscriptionForm
              assertAllObserved do
                currentUrl `satisfies` Text.isPrefixOf failureUrlPrefix
                textContent (byRole Heading `named` "Request could not be completed") `shouldEqual` "Request could not be completed"
                textContent (css "body") `satisfies` (not . Text.isInfixOf "This page is fully server-rendered on direct load and reload.")

          it "does not perform a native submission for the default exclusive client action when scripts are disabled" $ \(browser, server) -> do
            let homeUrl = localServerBaseUrl server <> "/"
                subscriptionForm = byRole Form `named` "Subscription"
                emailField = byLabel "Email address"
            runBrowserSpec browser do
              visitWithoutScripts homeUrl
              fill emailField "ada@example.com"
              submit subscriptionForm
              assertAllObserved do
                currentUrl `shouldEqual` homeUrl
                inputValue emailField `shouldEqual` "ada@example.com"
                $([|browserMetrics|] `matchesPattern` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 0}|])

          it "uses an explicitly authored CSRF-protected native fallback when scripts are disabled" $ \(browser, server) -> do
            let homeUrl = localServerBaseUrl server <> "/"
                fallbackForm = byRole Form `named` "Native fallback subscription"
                fallbackEmail = byLabel "Native fallback email address"
            runBrowserSpec browser do
              visitWithoutScripts homeUrl
              fill fallbackEmail "native@example.com"
              submit fallbackForm
              assertAllObserved do
                textContent (byRole Heading `named` "Subscription received") `shouldEqual` "Subscription received"
                $([|browserMetrics|] `matchesPattern` [p|BrowserMetrics {enhancedNavigationFetchCount = 0, hardNavigationCount = 2, mutationRequestCount = 0}|])

          it "keeps a permanently blocked action visibly recoverable until the user cancels it" $ \(browser, server) -> do
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
                textContent actionStatus `shouldEqual` "Still waiting for this action to be handled."
              click (byRole Button `named` "Cancel action")
              assertAllObserved do
                textContent actionStatus `shouldEqual` "Action cancelled."
                $([|browserMetrics|] `matchesPattern` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 0}|])
              releaseRequestsMatching "**/assets/navigation.js"
              assertAllObserved do
                textContent actionStatus `shouldEqual` "Action cancelled."
                $([|browserMetrics|] `matchesPattern` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 0}|])

          it "lets a handler arrive after the liveness threshold without replaying a cancellation" $ \(browser, server) -> do
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
                textContent actionStatus `shouldEqual` "Still waiting for this action to be handled."
              releaseRequestsMatching "**/assets/navigation.js"
              assertAllObserved do
                textContent (byRole Heading `named` "Subscription received") `shouldEqual` "Subscription received"
                $([|browserMetrics|] `matchesPattern` [p|BrowserMetrics {enhancedNavigationFetchCount = 1, hardNavigationCount = 0, mutationRequestCount = 1}|])

          it "does not present a delayed response after its action is cancelled" $ \(browser, server) -> do
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
                attributeValue (css "body") "data-harch-action-response-decoded" `shouldEqual` Just "true"
              assertAllObserved do
                currentUrl `shouldEqual` homeUrl
                textContent actionStatus `shouldEqual` "Action cancelled."
                $([|browserMetrics|] `matchesPattern` [p|BrowserMetrics {enhancedNavigationFetchCount = 0, hardNavigationCount = 0, mutationRequestCount = 1}|])

          it "does not let a delayed action replace a page selected by navigation" $ \(browser, server) -> do
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
                currentUrl `shouldEqual` secondUrl
              releaseRequestsMatching "**/actions/subscribe"
              assertAllObserved do
                attributeValue (css "body") "data-harch-action-response-decoded" `shouldEqual` Just "true"
              assertAllObserved do
                currentUrl `shouldEqual` secondUrl
                textContent (byRole Heading `named` "Second") `shouldEqual` "Second"
                $([|browserMetrics|] `matchesPattern` [p|BrowserMetrics {enhancedNavigationFetchCount = 1, hardNavigationCount = 0, mutationRequestCount = 1}|])

          it "supersedes an earlier delayed submission from the same control" $ \(browser, server) -> do
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
              waitForBlockedRequestCountMatching "**/actions/subscribe" 2
              assertAllObserved do
                (mutationRequestCount <$> browserMetrics) `shouldEqual` 2
              releaseRequestsMatching "**/actions/subscribe"
              assertAllObserved do
                textContent (byRole Heading `named` "Subscription received") `shouldEqual` "Subscription received"
                $([|browserMetrics|] `matchesPattern` [p|BrowserMetrics {enhancedNavigationFetchCount = 1, hardNavigationCount = 0, mutationRequestCount = 2}|])

          it "shows immediate recoverable outcomes for throwing and rejected handlers" $ \(browser, server) -> do
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
                textContent actionStatus `shouldEqual` "This action needs your attention."
              fill emailField "reject@example.com"
              submit subscriptionForm
              assertAllObserved do
                textContent actionStatus `shouldEqual` "This action needs your attention."
                attributeValue (within subscriptionForm (css "[data-harch-action-retry]")) "hidden" `shouldEqual` Just ""

          it "retries only declared safe handlers and preserves the idempotency identity for mutation retries" $ \(browser, server) -> do
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
                textContent actionStatus `shouldEqual` "This action needs your attention."
              click retryButton
              assertAllObserved do
                textContent actionStatus `shouldEqual` "Completed."
                attributeValue (css "body") "data-harch-retry-evidence" `shouldEqual` Just "2:safe@example.com"
              _ <- runPageScript idempotentRetry
              fill emailField "idempotent@example.com"
              submit subscriptionForm
              assertAllObserved do
                textContent actionStatus `shouldEqual` "This action needs your attention."
              click retryButton
              assertAllObserved do
                textContent actionStatus `shouldEqual` "Completed."
                attributeValue (css "body") "data-harch-idempotency-evidence" `shouldEqual` Just "2:mutation-1"

          it "keeps an unsettled claim local, rejects stale settlement, and warns only opted-in unresolved actions" $ \(browser, server) -> do
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
                textContent actionStatus `shouldEqual` "Still waiting for this action to be handled."
                attributeValue subscriptionForm "aria-busy" `shouldEqual` Just "true"
              _ <- runPageScript "const event = new Event('beforeunload', { cancelable: true }); window.dispatchEvent(event); document.body.dataset.harchBeforeUnload = String(event.defaultPrevented);"
              assertAllObserved do
                attributeValue (css "body") "data-harch-before-unload" `shouldEqual` Just "true"
              click (byRole Button `named` "Cancel action")
              _ <- runPageScript "document.body.dataset.harchStaleSettlement = String(window.__harchTestSettlement.completed()); const event = new Event('beforeunload', { cancelable: true }); window.dispatchEvent(event); document.body.dataset.harchBeforeUnload = String(event.defaultPrevented);"
              assertAllObserved do
                textContent actionStatus `shouldEqual` "Action cancelled."
                attributeValue (css "body") "data-harch-stale-settlement" `shouldEqual` Just "false"
                attributeValue (css "body") "data-harch-before-unload" `shouldEqual` Just "false"

          it "keeps multiple pending controls and their input snapshots independent" $ \(browser, server) -> do
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
                textContent firstStatus `shouldEqual` "Still waiting for this action to be handled."
                textContent secondStatus `shouldEqual` "Still waiting for this action to be handled."
                inputValue firstEmail `shouldEqual` "first@example.com"
                inputValue secondEmail `shouldEqual` "second@example.com"
                $([|browserMetrics|] `matchesPattern` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 0}|])

          it "reports a deferred-script failure locally after an action has been captured" $ \(browser, server) -> do
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
                textContent actionStatus `shouldEqual` "This action needs your attention."
                $([|browserMetrics|] `matchesPattern` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 0}|])

          it "keeps reload and script-disabled navigation fully server rendered" $ \(browser, server) -> do
            let homeUrl = localServerBaseUrl server <> "/"
                secondUrl = localServerBaseUrl server <> "/second"
            runBrowserSpec browser do
              visit secondUrl
              reload
              assertAllObserved do
                textContent (byRole Heading `named` "Second") `shouldEqual` "Second"
              visitWithoutScripts homeUrl
              click (byRole Link `named` "Go to the second page")
              assertAllObserved do
                currentUrl `shouldEqual` secondUrl
                textContent (byRole Heading `named` "Second") `shouldEqual` "Second"
                $([|browserMetrics|] `matchesPattern` [p|BrowserMetrics {enhancedNavigationFetchCount = 0, hardNavigationCount = 1}|])

          it "preserves the server-rendered live status until the optional EventSource module updates it" $ \(browser, server) -> do
            let liveDataUrl = localServerBaseUrl server <> "/live-data"
            runBrowserSpec browser do
              visitWithoutScripts liveDataUrl
              assertAllObserved do
                textContent (byRole Heading `named` "Live updates") `shouldEqual` "Live updates"
                textContent (css "#live-data-status") `shouldEqual` "Waiting for an update."
              visit liveDataUrl
              assertAllObserved do
                textContent (css "#live-data-status") `shouldEqual` "The live update arrived."

          it "reconciles declared page enhancements across enhanced navigation and history" $ \(browser, server) -> do
            let homeUrl = localServerBaseUrl server <> "/"
                liveDataUrl = localServerBaseUrl server <> "/live-data"
            runBrowserSpec browser do
              visit homeUrl
              assertAllObserved do
                textContent (css "[data-home-enhancement-status]") `shouldEqual` "The page-scoped home enhancement is ready."
              click (byRole Link `named` "See live updates")
              assertAllObserved do
                textContent (css "#live-data-status") `shouldEqual` "The live update arrived."
              click (byRole Link `named` "Home")
              assertAllObserved do
                textContent (css "[data-home-enhancement-status]") `shouldEqual` "The page-scoped home enhancement is ready."
              historyBack
              assertAllObserved do
                currentUrl `shouldEqual` liveDataUrl
                textContent (css "#live-data-status") `shouldEqual` "The live update arrived."
                $([|browserMetrics|] `matchesPattern` [p|BrowserMetrics {enhancedNavigationFetchCount = 3, hardNavigationCount = 0}|])

          it "supersedes a pending page enhancement before it can mount stale behavior" $ \(browser, server) -> do
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
                currentUrl `shouldEqual` secondUrl
                textContent (byRole Heading `named` "Second") `shouldEqual` "Second"
                $([|browserMetrics|] `matchesPattern` [p|BrowserMetrics {enhancedNavigationFetchCount = 2, hardNavigationCount = 0}|])

          it "uses a native SSR navigation when a declared page enhancement fails to load" $ \(browser, server) -> do
            let homeUrl = localServerBaseUrl server <> "/"
                liveDataUrl = localServerBaseUrl server <> "/live-data"
            runBrowserSpec browser do
              blockRequestsMatching "**/assets/live-data.js"
              visit homeUrl
              click (byRole Link `named` "See live updates")
              waitForBlockedRequestsMatching "**/assets/live-data.js"
              failBlockedRequestsMatching "**/assets/live-data.js"
              assertAllObserved do
                currentUrl `shouldEqual` liveDataUrl
                textContent (byRole Heading `named` "Live updates") `shouldEqual` "Live updates"
                $([|browserMetrics|] `matchesPattern` [p|BrowserMetrics {enhancedNavigationFetchCount = 1, hardNavigationCount = 1}|])

withBrowserAndServer :: ((BrowserConfig, LocalTestServer) -> IO a) -> BrowserConfig -> IO a
withBrowserAndServer action browser =
  withLocalTestServer buildApplication (\server -> action (browser, server))
