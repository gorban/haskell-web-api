{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# E2E_SPEC #-}

import App.Composed
import Catalog.Domain
import Crypto.Error (maybeCryptoError)
import Data.ByteString qualified as ByteString
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word64)
import HarchWeb
import HarchWeb.Csrf qualified as Csrf
import HarchWeb.LoginProtection (defaultLoginProtectionPolicy)
import HarchWeb.RequestContext (RequestContext (..), RequestIdentity (..))
import HarchWeb.Secret (encryptSecretWithNonce, mkEncryptionNonce, mkSecretEncryptionKey, mkSecretPlaintext)
import HarchWeb.Session (OpaqueSession (..), mkSessionId)
import HarchWeb.Site qualified as Site
import HarchWeb.Time (UnixTimeNanoseconds, unixTimeNanoseconds, unixTimeSeconds)
import HarchWeb.Totp (mkTotpSecret, renderTotpSecret, totpCode, totpCodeText)
import Orders.Domain

-- | Immutable public routing shares a server. Admission examples consume TOTP
-- counters and mutate session stores, so aroundWith gives each its own state.
-- Configuration is shared; every scenario retains a fresh browser session.
spec =
  beforeAll requirePlaywrightBrowserConfig $
    describe "composed-domains real-browser behavior" $ do
      aroundAllWith withBrowserAndServer $
        parallel $
          describe "public navigation" $ do
            it "keeps root authentication while disabled admission removes its form and guard" $ \(browser, server) -> do
              let admissionUrl = localServerBaseUrl server <> "/public/admission"
                  catalogUrl = localServerBaseUrl server <> "/catalog"
              runBrowserSpec browser do
                visit admissionUrl
                assertAllObserved do
                  byRole Heading `shouldHaveText` "Admission"
                  byText "Admission is not enabled." `shouldHaveText` "Admission is not enabled."
                  isVisible (byRole Form `named` "Admission") `shouldEqual` False
                visit catalogUrl
                assertAllObserved do
                  currentUrl `shouldEqual` catalogUrl
                  byRole Heading `named` "en catalog" `shouldHaveText` "en catalog"

            it "keeps localized public and mounted-domain navigation SSR-complete and enhanced" $ \(browser, server) -> do
              let loginUrl = localServerBaseUrl server <> "/es/public/login"
                  catalogUrl = localServerBaseUrl server <> "/es/catalog"
                  ordersUrl = localServerBaseUrl server <> "/es/orders"
              runBrowserSpec browser do
                visit loginUrl
                assertAllObserved do
                  byRole Link `named` "Catalog" `shouldHaveText` "Catalog"
                  byRole Link `named` "Orders" `shouldHaveText` "Orders"
                click (byRole Link `named` "Catalog")
                assertAllObserved do
                  currentUrl `shouldEqual` catalogUrl
                  byRole Heading `named` "es catalog" `shouldHaveText` "es catalog"
                  $([|browserMetrics|] `matchesPattern` [p|BrowserMetrics {enhancedNavigationFetchCount = 1, hardNavigationCount = 0}|])
                reload
                assertAllObserved do
                  currentUrl `shouldEqual` catalogUrl
                  byRole Heading `named` "es catalog" `shouldHaveText` "es catalog"
                click (byRole Link `named` "Orders")
                assertAllObserved do
                  currentUrl `shouldEqual` ordersUrl
                  byRole Heading `named` "es orders" `shouldHaveText` "es orders"

            it "keeps default-locale mounted navigation SSR-complete across reload and enhancement" $ \(browser, server) -> do
              let catalogUrl = localServerBaseUrl server <> "/catalog"
                  ordersUrl = localServerBaseUrl server <> "/en/orders"
              runBrowserSpec browser do
                visit catalogUrl
                assertAllObserved do
                  currentUrl `shouldEqual` catalogUrl
                  byRole Heading `named` "en catalog" `shouldHaveText` "en catalog"
                reload
                assertAllObserved $ byRole Heading `named` "en catalog" `shouldHaveText` "en catalog"
                click (byRole Link `named` "Orders")
                assertAllObserved do
                  currentUrl `shouldEqual` ordersUrl
                  byRole Heading `named` "en orders" `shouldHaveText` "en orders"
                  $([|browserMetrics|] `matchesPattern` [p|BrowserMetrics {enhancedNavigationFetchCount = 1, hardNavigationCount = 1}|])

            it "keeps public and mounted-domain navigation usable when scripts are disabled" $ \(browser, server) -> do
              let loginUrl = localServerBaseUrl server <> "/public/login"
                  spanishLoginUrl = localServerBaseUrl server <> "/es/public/login"
              runBrowserSpec browser do
                visitWithoutScripts loginUrl
                assertAllObserved do
                  byRole Heading `named` "Login" `shouldHaveText` "Login"
                  byRole Link `named` "Catalog" `shouldHaveText` "Catalog"
                click (byRole Link `named` "Catalog")
                assertAllObserved $ byRole Heading `named` "en catalog" `shouldHaveText` "en catalog"
                click (byRole Link `named` "Orders")
                assertAllObserved $ byRole Heading `named` "en orders" `shouldHaveText` "en orders"
                visitWithoutScripts spanishLoginUrl
                click (byRole Link `named` "Catalog")
                assertAllObserved $ byRole Heading `named` "es catalog" `shouldHaveText` "es catalog"
                click (byRole Link `named` "Orders")
                assertAllObserved $ byRole Heading `named` "es orders" `shouldHaveText` "es orders"

      aroundWith (withAdmissionBrowserAndServer defaultAdmissionBrowserFixture) $
        parallel $
          describe "admission sessions and replay" $ do
            it "returns a direct protected Catalog navigation through the typed admission target" $ \(browser, server) -> do
              let catalogUrl = localServerBaseUrl server <> "/en/catalog"
                  admissionUrl = localServerBaseUrl server <> "/en/public/admission?return=catalog"
              runBrowserSpec browser do
                visit catalogUrl
                assertAllObserved do
                  currentUrl `shouldEqual` admissionUrl
                  byRole Heading `named` "Admission" `shouldHaveText` "Admission"
                fill (byLabel "Admission name") "support_operator"
                fill (byLabel "One-time code") browserAdmissionCode
                submit (byRole Form `named` "Admission")
                assertAllObserved do
                  currentUrl `shouldEqual` catalogUrl
                  byRole Heading `named` "en catalog" `shouldHaveText` "en catalog"
                  $([|browserMetrics|] `matchesPattern` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 1}|])

            it "submits admission through the enhanced action and replaces credential history" $ \(browser, server) -> do
              let admissionUrl = localServerBaseUrl server <> "/public/admission"
                  loginUrl = localServerBaseUrl server <> "/en/public/login"
              runBrowserSpec browser do
                visit admissionUrl
                fill (byLabel "Admission name") "support_operator"
                fill (byLabel "One-time code") browserAdmissionCode
                submit (byRole Form `named` "Admission")
                assertAllObserved do
                  currentUrl `shouldEqual` loginUrl
                  byRole Heading `named` "Login" `shouldHaveText` "Login"
                  $([|browserMetrics|] `matchesPattern` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 1}|])

            it "rejects a mismatched admission CSRF submission without issuing admission or navigating" $ \(browser, server) -> do
              let admissionUrl = localServerBaseUrl server <> "/public/admission"
              runBrowserSpec browser do
                visit admissionUrl
                fill (byLabel "Admission name") "support_operator"
                fill (byLabel "One-time code") browserAdmissionCode
                _ <- runPageScript "document.body.dataset.harchCsrfToken = 'not-the-rendered-admission-token'; true"
                submit (byRole Form `named` "Admission")
                assertAllObserved do
                  currentUrl `shouldEqual` admissionUrl
                  css "[data-harch-action-status]" `shouldHaveText` "This action needs your attention."
                  $([|browserMetrics|] `matchesPattern` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 1}|])

            it "rejects an older admission document after another tab rotates shared security" $ \(browser, server) -> do
              let admissionUrl = localServerBaseUrl server <> "/public/admission"
                  loginUrl = localServerBaseUrl server <> "/en/public/login"
                  loginField = byLabel "Admission name"
                  codeField = byLabel "One-time code"
              runBrowserSpec browser do
                visit admissionUrl
                fill loginField "support_operator"
                fill codeField browserAdmissionCode
                withSharedCookieDocument do
                  visit admissionUrl
                  fill loginField "support_operator"
                  fill codeField browserAdmissionCode
                  submit (byRole Form `named` "Admission")
                  assertAllObserved do
                    currentUrl `shouldEqual` loginUrl
                    byRole Heading `named` "Login" `shouldHaveText` "Login"
                submit (byRole Form `named` "Admission")
                assertAllObserved do
                  currentUrl `shouldEqual` admissionUrl
                  css "[data-harch-action-status]" `shouldHaveText` "This action needs your attention."
                  inputValue loginField `shouldEqual` "support_operator"
                  inputValue codeField `shouldEqual` browserAdmissionCode
                  $([|browserMetrics|] `matchesPattern` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 1}|])

            it "keeps an invalid admission TOTP draft editable until its corrected submission succeeds" $ \(browser, server) -> do
              let admissionUrl = localServerBaseUrl server <> "/public/admission"
                  loginUrl = localServerBaseUrl server <> "/en/public/login"
                  loginField = byLabel "Admission name"
                  codeField = byLabel "One-time code"
              runBrowserSpec browser do
                visit admissionUrl
                fill loginField "support_operator"
                fill codeField "000000"
                submit (byRole Form `named` "Admission")
                assertAllObserved do
                  currentUrl `shouldEqual` admissionUrl
                  css "[data-harch-action-status]" `shouldHaveText` "This action needs your attention."
                  inputValue loginField `shouldEqual` "support_operator"
                  inputValue codeField `shouldEqual` "000000"
                  $([|browserMetrics|] `matchesPattern` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 1}|])
                fill codeField browserAdmissionCode
                submit (byRole Form `named` "Admission")
                assertAllObserved do
                  currentUrl `shouldEqual` loginUrl
                  byRole Heading `named` "Login" `shouldHaveText` "Login"
                  $([|browserMetrics|] `matchesPattern` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 2}|])

            it "rejects a replayed admission TOTP without navigating or clearing the new draft" $ \(browser, server) -> do
              let admissionUrl = localServerBaseUrl server <> "/public/admission"
                  loginUrl = localServerBaseUrl server <> "/en/public/login"
                  loginField = byLabel "Admission name"
                  codeField = byLabel "One-time code"
              runBrowserSpec browser do
                visit admissionUrl
                fill loginField "support_operator"
                fill codeField browserAdmissionCode
                submit (byRole Form `named` "Admission")
                assertAllObserved $ currentUrl `shouldEqual` loginUrl
                visit admissionUrl
                fill loginField "support_operator"
                fill codeField browserAdmissionCode
                submit (byRole Form `named` "Admission")
                assertAllObserved do
                  currentUrl `shouldEqual` admissionUrl
                  css "[data-harch-action-status]" `shouldHaveText` "This action needs your attention."
                  inputValue loginField `shouldEqual` "support_operator"
                  inputValue codeField `shouldEqual` browserAdmissionCode

            it "keeps an unknown admission principal indistinguishable while preserving its draft" $ \(browser, server) -> do
              let admissionUrl = localServerBaseUrl server <> "/public/admission"
                  loginField = byLabel "Admission name"
                  codeField = byLabel "One-time code"
              runBrowserSpec browser do
                visit admissionUrl
                fill loginField "unknown_operator"
                fill codeField browserAdmissionCode
                submit (byRole Form `named` "Admission")
                assertAllObserved do
                  currentUrl `shouldEqual` admissionUrl
                  css "[data-harch-action-status]" `shouldHaveText` "This action needs your attention."
                  inputValue loginField `shouldEqual` "unknown_operator"
                  inputValue codeField `shouldEqual` browserAdmissionCode
                  $([|browserMetrics|] `matchesPattern` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 1}|])

            it "redirects a browser-deliverable revoked admission cookie to a fresh challenge" $ \(browser, server) -> do
              let admissionUrl = localServerBaseUrl server <> "/en/public/admission"
                  loginUrl = localServerBaseUrl server <> "/en/public/login"
              runBrowserSpec browser do
                setCookie loginUrl "__Host-composed-admission" expiredAdmissionBrowserSessionValue
                visit loginUrl
                assertAllObserved $ currentUrl `shouldEqual` admissionUrl

            it "submits the same admission workflow through its CSRF-protected native fallback" $ \(browser, server) -> do
              let admissionUrl = localServerBaseUrl server <> "/public/admission"
                  loginUrl = localServerBaseUrl server <> "/en/public/login"
              runBrowserSpec browser do
                visitWithoutScripts admissionUrl
                fill (byLabel "Admission name") "support_operator"
                fill (byLabel "One-time code") browserAdmissionCode
                submit (byRole Form `named` "Admission")
                assertAllObserved do
                  currentUrl `shouldEqual` loginUrl
                  byRole Heading `named` "Login" `shouldHaveText` "Login"

      aroundWith (withAdmissionBrowserAndServer defaultAdmissionBrowserFixture {admissionFixtureAttempts = throttledAdmissionAttemptStore}) $
        parallel $
          describe "throttled admission" $
            it "keeps a throttled admission attempt recoverable without navigating or clearing its draft" $ \(browser, server) -> do
              let admissionUrl = localServerBaseUrl server <> "/public/admission"
                  loginField = byLabel "Admission name"
                  codeField = byLabel "One-time code"
              runBrowserSpec browser do
                visit admissionUrl
                fill loginField "support_operator"
                fill codeField browserAdmissionCode
                submit (byRole Form `named` "Admission")
                assertAllObserved do
                  currentUrl `shouldEqual` admissionUrl
                  css "[data-harch-action-status]" `shouldHaveText` "This action needs your attention."
                  inputValue loginField `shouldEqual` "support_operator"
                  inputValue codeField `shouldEqual` browserAdmissionCode
                  $([|browserMetrics|] `matchesPattern` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 1}|])

      aroundWith (withAdmissionBrowserAndServer defaultAdmissionBrowserFixture {admissionFixtureAttempts = unavailableAdmissionAttemptStore}) $
        parallel $
          describe "unavailable admission attempt storage" $
            it "keeps an attempt-store outage recoverable without issuing a credential or navigating" $ \(browser, server) -> do
              let admissionUrl = localServerBaseUrl server <> "/public/admission"
                  loginField = byLabel "Admission name"
                  codeField = byLabel "One-time code"
              runBrowserSpec browser do
                visit admissionUrl
                fill loginField "support_operator"
                fill codeField browserAdmissionCode
                submit (byRole Form `named` "Admission")
                assertAllObserved do
                  currentUrl `shouldEqual` admissionUrl
                  css "[data-harch-action-status]" `shouldHaveText` "This action needs your attention."
                  inputValue loginField `shouldEqual` "support_operator"
                  inputValue codeField `shouldEqual` browserAdmissionCode
                  $([|browserMetrics|] `matchesPattern` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 1}|])

      aroundWith (withAdmissionBrowserAndServer defaultAdmissionBrowserFixture {admissionFixtureSessions = [expiredAdmissionBrowserSession]}) $
        parallel $
          describe "expired admission sessions" $
            it "redirects a browser-deliverable expired admission cookie to a fresh challenge" $ \(browser, server) -> do
              let admissionUrl = localServerBaseUrl server <> "/en/public/admission"
                  loginUrl = localServerBaseUrl server <> "/en/public/login"
              runBrowserSpec browser do
                setCookie loginUrl "__Host-composed-admission" expiredAdmissionBrowserSessionValue
                visit loginUrl
                assertAllObserved $ currentUrl `shouldEqual` admissionUrl

      aroundWith (withAdmissionBrowserAndServer defaultAdmissionBrowserFixture {admissionFixtureSessionStore = BrowserAdmissionSessionStoreUnavailable}) $
        parallel $
          describe "unavailable admission sessions" $
            it "returns the typed unavailable response instead of treating a durable credential as absent" $ \(browser, server) -> do
              let loginUrl = localServerBaseUrl server <> "/en/public/login"
              runBrowserSpec browser do
                setCookie loginUrl "__Host-composed-admission" expiredAdmissionBrowserSessionValue
                visit loginUrl
                assertAllObserved do
                  currentUrl `shouldEqual` loginUrl
                  textContent (css "body") `shouldEqual` "Admission is temporarily unavailable."

      aroundWith (withAdmissionBrowserAndServer defaultAdmissionBrowserFixture {admissionFixtureSessionStore = BrowserAdmissionSessionStoreWriteUnavailable}) $
        parallel $
          describe "unavailable admission session writes" $
            it "keeps a successful proof recoverable without issuing a credential or navigating" $ \(browser, server) -> do
              let admissionUrl = localServerBaseUrl server <> "/public/admission"
                  loginField = byLabel "Admission name"
                  codeField = byLabel "One-time code"
              runBrowserSpec browser do
                visit admissionUrl
                fill loginField "support_operator"
                fill codeField browserAdmissionCode
                submit (byRole Form `named` "Admission")
                assertAllObserved do
                  currentUrl `shouldEqual` admissionUrl
                  css "[data-harch-action-status]" `shouldHaveText` "This action needs your attention."
                  inputValue loginField `shouldEqual` "support_operator"
                  inputValue codeField `shouldEqual` browserAdmissionCode
                  $([|browserMetrics|] `matchesPattern` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 1}|])

      aroundWith (withAdmissionBrowserAndServer defaultAdmissionBrowserFixture {admissionFixtureSessionStore = BrowserAdmissionSessionStoreCapacityExceeded}) $
        parallel $
          describe "full admission session storage" $
            it "keeps a successful proof recoverable when durable session capacity is exhausted" $ \(browser, server) -> do
              let admissionUrl = localServerBaseUrl server <> "/public/admission"
                  loginField = byLabel "Admission name"
                  codeField = byLabel "One-time code"
              runBrowserSpec browser do
                visit admissionUrl
                fill loginField "support_operator"
                fill codeField browserAdmissionCode
                submit (byRole Form `named` "Admission")
                assertAllObserved do
                  currentUrl `shouldEqual` admissionUrl
                  css "[data-harch-action-status]" `shouldHaveText` "This action needs your attention."
                  inputValue loginField `shouldEqual` "support_operator"
                  inputValue codeField `shouldEqual` browserAdmissionCode
                  $([|browserMetrics|] `matchesPattern` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 1}|])

      aroundWith (withAdmissionBrowserAndServer defaultAdmissionBrowserFixture {admissionFixtureCredentials = BrowserAdmissionCredentialStoreUnavailable}) $
        parallel $
          describe "unavailable admission credentials" $
            it "keeps an unavailable admission credential store recoverable without navigating or clearing its draft" $ \(browser, server) -> do
              let admissionUrl = localServerBaseUrl server <> "/public/admission"
                  loginField = byLabel "Admission name"
                  codeField = byLabel "One-time code"
              runBrowserSpec browser do
                visit admissionUrl
                fill loginField "support_operator"
                fill codeField browserAdmissionCode
                submit (byRole Form `named` "Admission")
                assertAllObserved do
                  currentUrl `shouldEqual` admissionUrl
                  css "[data-harch-action-status]" `shouldHaveText` "This action needs your attention."
                  inputValue loginField `shouldEqual` "support_operator"
                  inputValue codeField `shouldEqual` browserAdmissionCode
                  $([|browserMetrics|] `matchesPattern` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 1}|])

      aroundWith (withAdmissionBrowserAndServer defaultAdmissionBrowserFixture {admissionFixtureCredentials = BrowserAdmissionCredentialStoreCorrupt}) $
        parallel $
          describe "corrupt admission credentials" $
            it "keeps corrupt encrypted admission credentials recoverable without navigating or clearing its draft" $ \(browser, server) -> do
              let admissionUrl = localServerBaseUrl server <> "/public/admission"
                  loginField = byLabel "Admission name"
                  codeField = byLabel "One-time code"
              runBrowserSpec browser do
                visit admissionUrl
                fill loginField "support_operator"
                fill codeField browserAdmissionCode
                submit (byRole Form `named` "Admission")
                assertAllObserved do
                  currentUrl `shouldEqual` admissionUrl
                  css "[data-harch-action-status]" `shouldHaveText` "This action needs your attention."
                  inputValue loginField `shouldEqual` "support_operator"
                  inputValue codeField `shouldEqual` browserAdmissionCode
                  $([|browserMetrics|] `matchesPattern` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 1}|])

      aroundWith withSynchronizerAdmissionBrowserAndServer $
        parallel $
          describe "durable synchronizer admission CSRF" $ do
            it "submits admission through the same guarded action transport" $ \(browser, server, csrfFixture) -> do
              let admissionUrl = localServerBaseUrl server <> "/public/admission"
                  loginUrl = localServerBaseUrl server <> "/en/public/login"
              runBrowserSpec browser do
                visit admissionUrl
                fill (byLabel "Admission name") "support_operator"
                fill (byLabel "One-time code") browserAdmissionCode
                submit (byRole Form `named` "Admission")
                assertAllObserved do
                  currentUrl `shouldEqual` loginUrl
                  byRole Heading `named` "Login" `shouldHaveText` "Login"
                  $([|browserMetrics|] `matchesPattern` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 1}|])
              readIORef (synchronizerBrowserVerificationCount csrfFixture) `shouldReturn` 1

            it "rejects a synchronizer token revoked after SSR without navigating or consuming the draft" $ \(browser, server, csrfFixture) -> do
              let admissionUrl = localServerBaseUrl server <> "/public/admission"
                  loginField = byLabel "Admission name"
                  codeField = byLabel "One-time code"
              runBrowserSpec browser do
                visit admissionUrl
                liftScenarioIO $ writeIORef (synchronizerBrowserTokens csrfFixture) []
                fill loginField "support_operator"
                fill codeField browserAdmissionCode
                submit (byRole Form `named` "Admission")
                assertAllObserved do
                  currentUrl `shouldEqual` admissionUrl
                  css "[data-harch-action-status]" `shouldHaveText` "This action needs your attention."
                  inputValue loginField `shouldEqual` "support_operator"
                  inputValue codeField `shouldEqual` browserAdmissionCode
                  $([|browserMetrics|] `matchesPattern` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 1}|])
              readIORef (synchronizerBrowserVerificationCount csrfFixture) `shouldReturn` 1

withBrowserAndServer :: ((BrowserConfig, LocalTestServer) -> IO a) -> BrowserConfig -> IO a
withBrowserAndServer action browser =
  withLocalTestServer composedBrowserApplication (\server -> action (browser, server))

-- | One owned input record replaces the cascading fixture wrappers. Each call
-- constructs fresh session and replay stores even when the inputs are shared.
data AdmissionBrowserFixture = AdmissionBrowserFixture
  { admissionFixtureAttempts :: AdmissionAttemptStore,
    admissionFixtureSessions :: [OpaqueSession AdmissionPrincipalId],
    admissionFixtureSessionStore :: AdmissionSessionStoreState,
    admissionFixtureCredentials :: AdmissionCredentialStoreState
  }

defaultAdmissionBrowserFixture :: AdmissionBrowserFixture
defaultAdmissionBrowserFixture = AdmissionBrowserFixture permissiveAdmissionAttemptStore [] BrowserAdmissionSessionStoreAvailable AdmissionCredentialStoreAvailable

withAdmissionBrowserAndServer :: AdmissionBrowserFixture -> ((BrowserConfig, LocalTestServer) -> IO a) -> BrowserConfig -> IO a
withAdmissionBrowserAndServer fixture action browser = do
  admissionApplication <- admissionBrowserApplication fixture
  withLocalTestServer admissionApplication (\server -> action (browser, server))

-- | Browser-level proof uses a durable-shaped store rather than the default
-- signed backend for these two cases.  The fixture keeps only token digests,
-- bindings, and expiry; removing its record after SSR models an immediate
-- server-side revocation.  It exercises Harch's one cookie/field transport
-- and the composed application's selected backend without putting a test
-- database or a second action path into the browser runtime.
data SynchronizerBrowserCsrfFixture = SynchronizerBrowserCsrfFixture
  { synchronizerBrowserTokens :: IORef [(SynchronizerTokenDigest, Csrf.CsrfBindingDigest, UnixTimeNanoseconds)],
    synchronizerBrowserVerificationCount :: IORef Int
  }

withSynchronizerAdmissionBrowserAndServer :: ((BrowserConfig, LocalTestServer, SynchronizerBrowserCsrfFixture) -> IO a) -> BrowserConfig -> IO a
withSynchronizerAdmissionBrowserAndServer action browser = do
  (csrfProtection, csrfFixture) <- newSynchronizerBrowserCsrfFixture
  admissionApplication <- admissionBrowserApplicationWithCsrf defaultAdmissionBrowserFixture csrfProtection
  withLocalTestServer admissionApplication (\server -> action (browser, server, csrfFixture))

newSynchronizerBrowserCsrfFixture :: IO (Csrf.CsrfProtection ComposedContext, SynchronizerBrowserCsrfFixture)
newSynchronizerBrowserCsrfFixture = do
  tokens <- newIORef []
  verificationCount <- newIORef 0
  let fixture = SynchronizerBrowserCsrfFixture tokens verificationCount
      store =
        SynchronizerTokenStore
          { saveSynchronizerToken = \_ tokenDigest bindingDigest _ expiresAt ->
              atomicModifyIORef' tokens $ \stored ->
                if any (\(storedDigest, _, _) -> storedDigest == tokenDigest) stored
                  then (stored, Right False)
                  else ((tokenDigest, bindingDigest, expiresAt) : stored, Right True),
            verifySynchronizerToken = \tokenDigest bindingDigest now -> do
              modifyIORef' verificationCount (+ 1)
              stored <- readIORef tokens
              pure (Right (any (\(storedDigest, storedBinding, expiresAt) -> storedDigest == tokenDigest && storedBinding == bindingDigest && expiresAt > now) stored)),
            cleanupSynchronizerTokens = \now -> do
              modifyIORef' tokens (filter (\(_, _, expiresAt) -> expiresAt > now))
              pure (Right ())
          }
  pure (synchronizerCsrfProtection store (pure (unixTimeNanoseconds 123456000000000)) resolveAdmissionCsrfBinding, fixture)

composedBrowserApplication :: Application RootRoute RootAction ComposedContext RootAuthorization
composedBrowserApplication =
  Site.buildSiteApplication $
    case buildComposedSiteWithAdmissionSecurityDependencies (browserDependencies browserCsrfProtection) AdmissionDisabled browserSecurity of
      Left admissionError -> error ("expected disabled admission browser site: " <> show admissionError)
      Right site -> site

data AdmissionCredentialStoreState
  = AdmissionCredentialStoreAvailable
  | BrowserAdmissionCredentialStoreUnavailable
  | BrowserAdmissionCredentialStoreCorrupt

data AdmissionSessionStoreState
  = BrowserAdmissionSessionStoreAvailable
  | BrowserAdmissionSessionStoreUnavailable
  | BrowserAdmissionSessionStoreWriteUnavailable
  | BrowserAdmissionSessionStoreCapacityExceeded

admissionBrowserApplication :: AdmissionBrowserFixture -> IO (Application RootRoute RootAction ComposedContext RootAuthorization)
admissionBrowserApplication fixture = admissionBrowserApplicationWithCsrf fixture admissionBrowserCsrfProtection

admissionBrowserApplicationWithCsrf :: AdmissionBrowserFixture -> Csrf.CsrfProtection ComposedContext -> IO (Application RootRoute RootAction ComposedContext RootAuthorization)
admissionBrowserApplicationWithCsrf AdmissionBrowserFixture {admissionFixtureAttempts = attemptStore, admissionFixtureSessions = storedSessions, admissionFixtureSessionStore = sessionStoreState, admissionFixtureCredentials = credentialState} csrfProtection = do
  sessions <- newIORef storedSessions
  usedCounters <- newIORef ([] :: [Word64])
  let loginName = requiredBrowser "admission login" (mkAdmissionLoginName "support_operator")
      principalId = requiredBrowser "admission principal" (mkAdmissionPrincipalId "browser-operator")
      encryptionKey = requiredBrowser "admission encryption key" (mkSecretEncryptionKey "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
      secret = requiredBrowser "admission TOTP secret" (mkTotpSecret "JBSWY3DPEHPK3PXPJBSWY3DPEHPK3PXP")
      now = unixTimeNanoseconds 123456000000000
      encryptedSecret =
        requiredBrowser
          "encrypted admission TOTP secret"
          ( mkEncryptedAdmissionTotpSecret
              =<< maybeCryptoError
                ( encryptSecretWithNonce
                    encryptionKey
                    (requiredBrowser "admission encryption nonce" (mkEncryptionNonce (ByteString.replicate 12 6)))
                    (mkSecretPlaintext (TextEncoding.encodeUtf8 (renderTotpSecret secret)))
                )
          )
      credential = StoredAdmissionCredential principalId encryptedSecret Nothing
      corruptCredential =
        StoredAdmissionCredential
          principalId
          (requiredBrowser "corrupt encrypted admission TOTP secret" (mkEncryptedAdmissionTotpSecret "not-an-encrypted-envelope"))
          Nothing
      sessionStore =
        AdmissionSessionStore
          { saveAdmissionSession = \session ->
              case sessionStoreState of
                BrowserAdmissionSessionStoreWriteUnavailable -> pure (Left AdmissionSessionStoreUnavailable)
                BrowserAdmissionSessionStoreCapacityExceeded -> pure (Right False)
                _ -> atomicModifyIORef' sessions (\saved -> (session : filter ((/= sessionId session) . sessionId) saved, Right True)),
            loadAdmissionSession = \requestedSessionId ->
              case sessionStoreState of
                BrowserAdmissionSessionStoreAvailable -> do
                  saved <- readIORef sessions
                  pure (Right (find ((== requestedSessionId) . mkAdmissionSessionId . sessionId) saved))
                BrowserAdmissionSessionStoreUnavailable -> pure (Left AdmissionSessionStoreUnavailable)
                BrowserAdmissionSessionStoreWriteUnavailable -> do
                  saved <- readIORef sessions
                  pure (Right (find ((== requestedSessionId) . mkAdmissionSessionId . sessionId) saved))
                BrowserAdmissionSessionStoreCapacityExceeded -> do
                  saved <- readIORef sessions
                  pure (Right (find ((== requestedSessionId) . mkAdmissionSessionId . sessionId) saved)),
            invalidateAdmissionSession = \requestedSessionId _ ->
              atomicModifyIORef' sessions (\saved -> (filter ((/= requestedSessionId) . mkAdmissionSessionId . sessionId) saved, Right True))
          }
      credentialStore =
        case credentialState of
          AdmissionCredentialStoreAvailable ->
            AdmissionCredentialStore
              { findAdmissionCredential = \receivedLogin -> pure (Right (if receivedLogin == loginName then Just credential else Nothing)),
                markAdmissionTotpCounterUsed = \_ counter ->
                  atomicModifyIORef' usedCounters (\used -> if counter `elem` used then (used, Right False) else (counter : used, Right True))
              }
          BrowserAdmissionCredentialStoreUnavailable ->
            AdmissionCredentialStore
              { findAdmissionCredential = \_ -> pure (Left AdmissionCredentialStoreUnavailable),
                markAdmissionTotpCounterUsed = \_ _ -> pure (Left AdmissionCredentialStoreUnavailable)
              }
          BrowserAdmissionCredentialStoreCorrupt ->
            AdmissionCredentialStore
              { findAdmissionCredential = \receivedLogin -> pure (Right (if receivedLogin == loginName then Just corruptCredential else Nothing)),
                markAdmissionTotpCounterUsed = \_ _ -> pure (Left AdmissionCredentialStoreCorrupt)
              }
      proofConfig =
        AdmissionProofConfig
          { admissionProofCredentials = credentialStore,
            admissionProofAttempts = attemptStore,
            admissionProofPolicy = defaultLoginProtectionPolicy,
            admissionProofEncryptionKey = encryptionKey,
            admissionProofReadClock = pure (Right now)
          }
  sessionConfig <-
    case mkAdmissionConfig defaultAdmissionSessionCookiePolicy sessionStore (pure (Right now)) of
      Left _ -> expectationFailure "expected browser admission session configuration" >> fail "unreachable"
      Right config -> pure config
  case buildComposedSiteWithAdmissionSecurityDependencies (browserDependencies csrfProtection) (AdmissionEnabled sessionConfig proofConfig) browserSecurity of
    Left _ -> expectationFailure "expected admission-enabled browser site" >> fail "unreachable"
    Right site -> pure (Site.buildSiteApplication site)

permissiveAdmissionAttemptStore :: AdmissionAttemptStore
permissiveAdmissionAttemptStore =
  AdmissionAttemptStore
    { reserveAdmissionAttempt = \_ _ -> pure (Right (AdmissionAttemptReserved (AdmissionAttemptReservation "browser-reservation"))),
      settleAdmissionAttempt = \_ _ -> pure (Right ()),
      cancelAdmissionAttempt = \_ -> pure (Right ())
    }

throttledAdmissionAttemptStore :: AdmissionAttemptStore
throttledAdmissionAttemptStore =
  AdmissionAttemptStore
    { reserveAdmissionAttempt = \_ _ -> pure (Right (AdmissionAttemptThrottled (unixTimeNanoseconds 123456000000001))),
      settleAdmissionAttempt = \_ _ -> pure (Right ()),
      cancelAdmissionAttempt = \_ -> pure (Right ())
    }

unavailableAdmissionAttemptStore :: AdmissionAttemptStore
unavailableAdmissionAttemptStore =
  AdmissionAttemptStore
    { reserveAdmissionAttempt = \_ _ -> pure (Left AdmissionAttemptStoreUnavailable),
      settleAdmissionAttempt = \_ _ -> pure (Right ()),
      cancelAdmissionAttempt = \_ -> pure (Right ())
    }

expiredAdmissionBrowserSessionValue :: Text
expiredAdmissionBrowserSessionValue = "0123456789abcdef0123456789abcdef"

expiredAdmissionBrowserSession :: OpaqueSession AdmissionPrincipalId
expiredAdmissionBrowserSession =
  OpaqueSession
    (requiredBrowser "expired admission session ID" (mkSessionId expiredAdmissionBrowserSessionValue))
    (requiredBrowser "expired admission principal" (mkAdmissionPrincipalId "browser-operator"))
    (unixTimeNanoseconds 123456000000000)
    (unixTimeNanoseconds 123456000000000)

browserAdmissionCode :: Text
browserAdmissionCode =
  totpCodeText
    (totpCode (unixTimeSeconds 123456) (requiredBrowser "browser admission TOTP secret" (mkTotpSecret "JBSWY3DPEHPK3PXPJBSWY3DPEHPK3PXP")))

browserDependencies :: Csrf.CsrfProtection ComposedContext -> ComposedSiteDependencies
browserDependencies csrfProtection =
  ComposedSiteDependencies
    { composedStaticAssets = defaultComposedStaticAssets,
      composedLocalePolicy = defaultLocalePolicy,
      composedCsrfProtection = csrfProtection,
      composedDomainCapabilities = ComposedDomainCapabilities catalogQueries catalogCommands ordersQueries ordersCommands
    }

browserCsrfProtection :: Csrf.CsrfProtection ComposedContext
browserCsrfProtection =
  Csrf.signedCsrfProtection
    Csrf.SignedCsrfDependencies
      { Csrf.signedCsrfDependenciesKeyring = keyring,
        Csrf.signedCsrfDependenciesPolicy = Csrf.defaultSignedCsrfPolicy,
        Csrf.signedCsrfDependenciesCurrentTime = pure 1000000000,
        Csrf.signedCsrfDependenciesResolveBinding = const (pure Csrf.AnonymousCsrfBinding)
      }
  where
    keyId = requiredCsrf "browser CSRF key id" (Csrf.mkCsrfKeyId "composed-browser-v1")
    signingKey = requiredCsrf "browser CSRF signing key" (Csrf.mkCsrfSigningKey "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
    keyring = requiredCsrf "browser CSRF keyring" (Csrf.mkSignedCsrfKeyring keyId ((keyId, signingKey) :| []))

admissionBrowserCsrfProtection :: Csrf.CsrfProtection ComposedContext
admissionBrowserCsrfProtection =
  Csrf.signedCsrfProtection
    Csrf.SignedCsrfDependencies
      { Csrf.signedCsrfDependenciesKeyring = keyring,
        Csrf.signedCsrfDependenciesPolicy = Csrf.defaultSignedCsrfPolicy,
        Csrf.signedCsrfDependenciesCurrentTime = pure 1000000000,
        Csrf.signedCsrfDependenciesResolveBinding = resolveAdmissionCsrfBinding
      }
  where
    keyId = requiredCsrf "admission browser CSRF key id" (Csrf.mkCsrfKeyId "composed-browser-v1")
    signingKey = requiredCsrf "admission browser CSRF signing key" (Csrf.mkCsrfSigningKey "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
    keyring = requiredCsrf "admission browser CSRF keyring" (Csrf.mkSignedCsrfKeyring keyId ((keyId, signingKey) :| []))

requiredCsrf :: String -> Maybe value -> value
requiredCsrf label = fromMaybe (error ("expected " <> label))

requiredBrowser :: String -> Maybe value -> value
requiredBrowser label = fromMaybe (error ("expected " <> label))

browserSecurity :: ApplicationSecurity RootRoute ComposedContext RootAuthorization
browserSecurity =
  AuthenticationEnabled [] (AuthenticationGuard (pure . ContinueEndpoint . authenticatedContext . endpointRouteRequest)) []

authenticatedContext :: RouteRequest RootRoute ComposedContext -> ComposedContext
authenticatedContext request =
  (requestContext request)
    { requestIdentity = AuthenticatedIdentity (RootPrincipal (Just (locale "es")) ["catalog.read", "orders.read"])
    }

catalogQueries :: CatalogQueries
catalogQueries = CatalogQueries (\domainContext -> pure (catalogLocaleCode domainContext <> " catalog"))

catalogCommands :: CatalogCommands
catalogCommands = CatalogCommands (const (pure "refreshed"))

ordersQueries :: OrdersQueries
ordersQueries = OrdersQueries (\domainContext -> pure (ordersLocaleCode domainContext <> " orders"))

ordersCommands :: OrdersCommands
ordersCommands = OrdersCommands (\domainContext -> pure (OrderId ("order-" <> ordersLocaleCode domainContext)))
