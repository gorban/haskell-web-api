{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# E2E_SPEC #-}

import Control.Monad (when)
import Crypto.Error qualified as Crypto
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as ByteString
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb qualified
import HarchWeb.Account qualified as Account
import HarchWeb.Email qualified as Email
import HarchWeb.Password qualified as Password
import HarchWeb.Secret qualified as Secret
import HarchWeb.Session qualified as Session
import HarchWeb.Time qualified as Time
import HarchWeb.Totp qualified as Totp
import Network.HTTP.Types qualified as Http
import System.Directory (copyFile, createDirectory, doesFileExist, getCurrentDirectory)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import TestSupport.AccountJwt (withTestAccountJwtFixture)
import WebApi.Account (AccountProfile (..), AccountProfileStore (..), AccountStore (..), CreatePendingAccountOutcome (..), VerificationResendAdmission (..), VerificationResendClaim (..), VerificationResendClaimSettlement (..))
import WebApi.AccountJwt (AccountJwtRuntime, accountJwtAuthenticationPipeline, accountJwtIssuerFromRuntime, loadAccountJwtRuntime)
import WebApi.AccountJwt qualified as AccountJwt
import WebApi.AccountPages (AccountAction)
import WebApi.AccountPrincipal (mkAccountPrincipal)
import WebApi.App (buildApp, buildAppWithDatabaseAndAccountWorkflow, buildAppWithDatabaseAndAccountWorkflowAndSecurity, unavailableAccountWorkflow)
import WebApi.AppEffect (AccountWorkflow (..))
import WebApi.Config (AppConfig (..), AppEnvironmentConfig (..), StaticAssetRoot (..), StaticAssetsConfig (..), defaultAppConfig, defaultStaticAssetContentTypes, totpEncryptionKey)
import WebApi.Database (defaultPageRepository)
import WebApi.Login (AccountCredential (..), AccountCredentialStore (..), LoginAttemptAdmission (..), LoginAttemptReservation (..), LoginAttemptStore (..))
import WebApi.Mfa (MfaStore (..), StoredTotpEnrollment (..))
import WebApi.Route (AppRoute (LoginRoute))
import WebApi.Route qualified
import WebApi.Session (AccountSessionStore (..), MfaEnrollmentSessionStore (..), mfaEnrollmentSessionCookiePolicy)

spec =
  describe "stacked application real-browser smoke coverage" $ do
    it "redirects the root route to the complete Spaces SSR document" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildApp appConfig) $ \server -> do
          let homeUrl = HarchWeb.localServerBaseUrl server <> "/"
          runBrowserSpec browser do
            visit homeUrl
            assertAllObserved do
              currentUrl `matches` (`shouldBe` (HarchWeb.localServerBaseUrl server <> "/spaces"))
              textContent (byRole Heading) `matches` (`shouldBe` "Site under construction")

    it "keeps direct second-page loads and script-disabled root redirects usable" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildApp appConfig) $ \server -> do
          let homeUrl = HarchWeb.localServerBaseUrl server <> "/"
              secondUrl = HarchWeb.localServerBaseUrl server <> "/second"
          runBrowserSpec browser do
            visit secondUrl
            assertAllObserved do
              textContent (byRole Heading) `matches` (`shouldBe` "Second")
            visitWithoutScripts homeUrl
            assertAllObserved do
              currentUrl `matches` (`shouldBe` (HarchWeb.localServerBaseUrl server <> "/spaces"))
              textContent (byRole Heading) `matches` (`shouldBe` "Site under construction")

    it "redirects Spanish roots to localized Spaces SSR content while scripts are disabled" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildApp appConfig) $ \server -> do
          let spanishHomeUrl = HarchWeb.localServerBaseUrl server <> "/es"
          runBrowserSpec browser do
            visitWithoutScripts spanishHomeUrl
            assertAllObserved do
              currentUrl `matches` (`shouldBe` (HarchWeb.localServerBaseUrl server <> "/es/spaces"))
              textContent (byRole Heading) `matches` (`shouldBe` "Sitio en construcción")
              attributeValue (css "html") "lang" `matches` (`shouldBe` Just "es")

    it "serves the app-home spaces placeholder through SSR and enhanced navigation" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildApp appConfig) $ \server -> do
          let homeUrl = HarchWeb.localServerBaseUrl server <> "/"
              secondUrl = HarchWeb.localServerBaseUrl server <> "/second"
              spacesUrl = HarchWeb.localServerBaseUrl server <> "/spaces"
              spanishSpacesUrl = HarchWeb.localServerBaseUrl server <> "/es/spaces"
          runBrowserSpec browser do
            visit homeUrl
            assertAllObserved do
              currentUrl `matches` (`shouldBe` spacesUrl)
              textContent (byRole Heading) `matches` (`shouldBe` "Site under construction")
            visit secondUrl
            click (byRole Link `named` "Spaces")
            assertAllObserved do
              currentUrl `matches` (`shouldBe` spacesUrl)
              textContent (byRole Heading) `matches` (`shouldBe` "Site under construction")
              browserMetrics `matches` \metrics ->
                $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 1, hardNavigationCount = 0}|])
            visitWithoutScripts spanishSpacesUrl
            assertAllObserved do
              textContent (byRole Heading) `matches` (`shouldBe` "Sitio en construcción")
              textContent (byText "Sigan este espacio.") `matches` (`shouldBe` "Sigan este espacio.")

    it "serves the app-home profile landing through SSR and enhanced navigation" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (challengedBrowserApp appConfig) $ \server -> do
          let secondUrl = HarchWeb.localServerBaseUrl server <> "/second"
              loginUrl = HarchWeb.localServerBaseUrl server <> "/login"
              profileUrl = HarchWeb.localServerBaseUrl server <> "/profile"
              spanishProfileUrl = HarchWeb.localServerBaseUrl server <> "/es/profile"
          runBrowserSpec browser do
            visit secondUrl
            _ <-
              runPageScript
                "const link = document.querySelector('nav a'); link.focus(); const style = getComputedStyle(link); link.dataset.testFocusVisibleStyle = String(link.matches(':focus-visible') && style.outlineStyle !== 'none' && parseFloat(style.outlineWidth) > 0); true"
            assertAllObserved do
              attributeValue (byRole Link `named` "Home") "data-test-focus-visible-style" `matches` (`shouldBe` Just "true")
            click (byRole Link `named` "Profile")
            assertAllObserved do
              currentUrl `matches` (`shouldBe` loginUrl)
              textContent (byRole Heading) `matches` (`shouldBe` "Sign in")
              browserMetrics `matches` \metrics ->
                $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 1, hardNavigationCount = 0}|])
            visitWithoutScripts profileUrl
            assertAllObserved do
              textContent (byRole Heading) `matches` (`shouldBe` "Sign in")
            visitWithoutScripts spanishProfileUrl
            assertAllObserved do
              textContent (byRole Heading) `matches` (`shouldBe` "Iniciar sesion")

    it "opens the language picker accessibly and navigates its typed choices" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildApp appConfig) $ \server -> do
          let baseUrl = HarchWeb.localServerBaseUrl server
              secondUrl = baseUrl <> "/second"
              spanishLanguageUrl = baseUrl <> "/es/language"
              languageTrigger = byRole Link `named` "Language"
              englishChoice = byRole Link `named` "English"
              spanishChoice = byRole Link `named` "Spanish"
              closeControl = byRole Button `named` "Close language picker"
          runBrowserSpec browser do
            visit secondUrl
            assertAllObserved do
              attributeValue (css "html") "lang" `matches` (`shouldBe` Just "en")
            reload
            assertAllObserved do
              attributeValue (css "html") "lang" `matches` (`shouldBe` Just "en")
            click languageTrigger
            assertAllObserved do
              attributeValue (css "#language-dialog") "open" `matches` (`shouldBe` Just "")
              isFocused englishChoice `satisfies` id
            _ <-
              runPageScript
                "const dialog = document.querySelector('#language-dialog'); document.querySelector('nav a').focus(); dialog.dataset.testBackgroundContained = String(dialog.contains(document.activeElement)); true"
            assertAllObserved do
              attributeValue (css "#language-dialog") "data-test-background-contained" `matches` (`shouldBe` Just "true")
            press englishChoice "Tab"
            assertAllObserved do
              isFocused spanishChoice `satisfies` id
            press spanishChoice "Tab"
            assertAllObserved do
              isFocused closeControl `satisfies` id
            press closeControl "Tab"
            assertAllObserved do
              isFocused englishChoice `satisfies` id
            press (css "#language-dialog") "Escape"
            assertAllObserved do
              isFocused languageTrigger `satisfies` id
            click languageTrigger
            click spanishChoice
            assertAllObserved do
              currentUrl `matches` (`shouldBe` spanishLanguageUrl)
              textContent (byRole Heading `named` "Elige un idioma") `matches` (`shouldBe` "Elige un idioma")
              textContent (byRole Status) `matches` (`shouldBe` "web-api: Language")
              browserMetrics `matches` \metrics ->
                $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 1, hardNavigationCount = 1}|])
              attributeValue (css "html") "lang" `matches` (`shouldBe` Just "es")
            assertAllObserved do
              attributeValue (css "#language-dialog") "open" `matches` (`shouldBe` Nothing)
            historyBack
            assertAllObserved do
              attributeValue (css "html") "lang" `matches` (`shouldBe` Just "en")
            historyForward
            assertAllObserved do
              attributeValue (css "html") "lang" `matches` (`shouldBe` Just "es")

    it "keeps language selection and dialog startup failure complete without enhanced behavior" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (challengedBrowserApp appConfig) $ \server -> do
          let baseUrl = HarchWeb.localServerBaseUrl server
              secondUrl = baseUrl <> "/second"
              languageUrl = baseUrl <> "/language"
              spanishLanguageUrl = baseUrl <> "/es/language"
          runBrowserSpec browser do
            blockRequestsMatching "**/assets/dialog.js"
            visit secondUrl
            click (byRole Link `named` "Language")
            failBlockedRequestsMatching "**/assets/dialog.js"
            assertAllObserved do
              currentUrl `matches` (`shouldBe` languageUrl)
              browserMetrics `matches` \metrics ->
                $([|metrics|] `shouldMatch` [p|BrowserMetrics {hardNavigationCount = 1}|])
            visitWithoutScripts secondUrl
            press (byRole Link `named` "Language") "Enter"
            assertAllObserved do
              currentUrl `matches` (`shouldBe` languageUrl)
              textContent (byRole Heading) `matches` (`shouldBe` "Choose a language")
            press (byRole Link `named` "Spanish") "Enter"
            assertAllObserved do
              currentUrl `matches` (`shouldBe` spanishLanguageUrl)
              textContent (byRole Heading) `matches` (`shouldBe` "Elige un idioma")

    it "keeps the Help FAB usable in a desktop narrow-width, layout-zoomed viewport and absent at its destination" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildApp appConfig) $ \server -> do
          let baseUrl = HarchWeb.localServerBaseUrl server
              secondUrl = baseUrl <> "/second"
              helpUrl = baseUrl <> "/help"
              helpFab = byRole Link `named` "Help and support"
          runBrowserSpec browser do
            setViewportSize 320 480
            visit secondUrl
            _ <-
              runPageScript
                "document.documentElement.style.zoom = '2'; const fab = document.querySelector('[data-help-fab]'); fab.focus(); const box = fab.getBoundingClientRect(); const overlaps = [...document.querySelectorAll('#app-main a, #app-main button, #app-main input, #app-main select')].filter((control) => control !== fab && !control.closest('dialog')).some((control) => { const other = control.getBoundingClientRect(); return box.left < other.right && box.right > other.left && box.top < other.bottom && box.bottom > other.top; }); fab.dataset.testGeometry = String(box.width >= 44 && box.height >= 44 && box.right <= window.innerWidth && box.bottom <= window.innerHeight && !overlaps && getComputedStyle(fab).outlineStyle !== 'none'); true"
            assertAllObserved do
              attributeValue helpFab "data-test-geometry" `matches` (`shouldBe` Just "true")
            press helpFab "Enter"
            assertAllObserved do
              currentUrl `matches` (`shouldBe` helpUrl)
              textContent (byRole Heading `named` "Help and support") `matches` (`shouldBe` "Help and support")
              browserMetrics `matches` \metrics ->
                $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 1, hardNavigationCount = 0}|])
            _ <- runPageScript "document.body.dataset.testNoHelpFab = String(!document.querySelector('[data-help-fab]')); true"
            assertAllObserved do
              attributeValue (css "body") "data-test-no-help-fab" `matches` (`shouldBe` Just "true")
            visitWithoutScripts secondUrl
            press helpFab "Enter"
            assertAllObserved do
              currentUrl `matches` (`shouldBe` helpUrl)
              textContent (byRole Heading) `matches` (`shouldBe` "Help and support")

    it "uses the responsive document viewport on mobile for Help FAB navigation and history" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildApp appConfig) $ \server -> do
          let baseUrl = HarchWeb.localServerBaseUrl server
              secondUrl = baseUrl <> "/second"
              helpUrl = baseUrl <> "/help"
              helpFab = byRole Link `named` "Help and support"
          runBrowserSpec browser do
            emulateMobileViewport 320 480
            visit secondUrl
            _ <-
              runPageScript
                "const viewport = document.querySelector('meta[name=viewport]'); const fab = document.querySelector('[data-help-fab]'); const box = fab.getBoundingClientRect(); const policy = viewport && viewport.content === 'width=device-width, initial-scale=1'; document.body.dataset.testMobileViewport = String(policy && window.innerWidth === 320 && document.documentElement.scrollWidth <= window.innerWidth && box.width >= 44 && box.height >= 44 && box.right <= window.innerWidth && box.bottom <= window.innerHeight); true"
            assertAllObserved do
              attributeValue (css "body") "data-test-mobile-viewport" `matches` (`shouldBe` Just "true")
            press helpFab "Enter"
            assertAllObserved do
              currentUrl `matches` (`shouldBe` helpUrl)
              textContent (byRole Heading `named` "Help and support") `matches` (`shouldBe` "Help and support")
            historyBack
            assertAllObserved do
              currentUrl `matches` (`shouldBe` secondUrl)
              textContent (byRole Heading `named` "Second") `matches` (`shouldBe` "Second")
            _ <-
              runPageScript
                "const viewport = document.querySelector('meta[name=viewport]'); const fab = document.querySelector('[data-help-fab]'); const box = fab.getBoundingClientRect(); document.body.dataset.testMobileHistoryViewport = String(viewport && viewport.content === 'width=device-width, initial-scale=1' && document.documentElement.scrollWidth <= window.innerWidth && box.width >= 44 && box.height >= 44); true"
            assertAllObserved do
              attributeValue (css "body") "data-test-mobile-history-viewport" `matches` (`shouldBe` Just "true")

    it "focuses and announces one lifecycle for keyboard navigation, history, and final redirected URLs" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildApp appConfig) $ \server -> do
          let baseUrl = HarchWeb.localServerBaseUrl server
              secondUrl = baseUrl <> "/second"
              spacesUrl = baseUrl <> "/spaces"
              mainContent = css "#app-main"
              routeStatus = css "[data-navigation-route-status]"
          runBrowserSpec browser do
            setViewportSize 320 480
            visit secondUrl
            assertAllObserved do
              textContent routeStatus `matches` (`shouldBe` "")
            _ <-
              runPageScript
                "window.__ahi8HistoryLength = history.length; const status = document.querySelector('[data-navigation-route-status]'); let count = 0; status.dataset.testMutationCount = '0'; new MutationObserver((records) => { count += records.filter((record) => record.type === 'childList' || record.type === 'characterData').length; status.dataset.testMutationCount = String(count); }).observe(status, { childList: true, characterData: true, subtree: true }); document.documentElement.style.zoom = '2'; true"
            press (byRole Link `named` "Spaces") "Enter"
            assertAllObserved do
              currentUrl `matches` (`shouldBe` spacesUrl)
              isFocused mainContent `satisfies` id
            _ <-
              runPageScript
                "const main = document.querySelector('#app-main'); const box = main.getBoundingClientRect(); const sampleX = Math.min(window.innerWidth - 1, Math.max(0, box.left + 1)); const sampleY = Math.min(window.innerHeight - 1, Math.max(0, box.top + 1)); const topElement = document.elementFromPoint(sampleX, sampleY); const style = getComputedStyle(main); main.dataset.testFocusUnobscured = String(document.activeElement === main && box.top >= 0 && box.top < window.innerHeight && (topElement === main || main.contains(topElement)) && style.outlineStyle !== 'none' && parseFloat(style.outlineWidth) > 0); true"
            assertAllObserved do
              currentUrl `matches` (`shouldBe` spacesUrl)
              textContent (css "title") `matches` (`shouldBe` "web-api: Spaces")
              textContent (byRole Heading) `matches` (`shouldBe` "Site under construction")
              textContent routeStatus `matches` (`shouldBe` "web-api: Spaces")
              isFocused mainContent `satisfies` id
              attributeValue routeStatus "data-test-mutation-count" `matches` (`shouldBe` Just "1")
              attributeValue mainContent "data-test-focus-unobscured" `matches` (`shouldBe` Just "true")
            historyBack
            assertAllObserved do
              currentUrl `matches` (`shouldBe` secondUrl)
              textContent (css "title") `matches` (`shouldBe` "web-api: Second")
              textContent routeStatus `matches` (`shouldBe` "web-api: Second")
              attributeValue routeStatus "data-test-mutation-count" `matches` (`shouldBe` Just "2")
              isFocused mainContent `satisfies` id
            historyForward
            assertAllObserved do
              currentUrl `matches` (`shouldBe` spacesUrl)
              textContent routeStatus `matches` (`shouldBe` "web-api: Spaces")
              attributeValue routeStatus "data-test-mutation-count" `matches` (`shouldBe` Just "3")
              attributeValue (byRole Link `named` "Spaces") "aria-current" `matches` (`shouldBe` Just "page")
            _ <- runPageScript "document.querySelector('#app-main').dataset.testHistoryStable = String(history.length === window.__ahi8HistoryLength + 1); true"
            assertAllObserved do
              attributeValue mainContent "data-test-history-stable" `matches` (`shouldBe` Just "true")
            visit secondUrl
            press (byRole Link `named` "Home") "Enter"
            assertAllObserved do
              currentUrl `matches` (`shouldBe` spacesUrl)
              textContent (css "title") `matches` (`shouldBe` "web-api: Spaces")
              textContent routeStatus `matches` (`shouldBe` "web-api: Spaces")
              isFocused mainContent `satisfies` id
              browserMetrics `matches` \metrics ->
                $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 1, hardNavigationCount = 0}|])

    it "keeps only the newest overlapping enhanced navigation lifecycle" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (challengedBrowserApp appConfig) $ \server -> do
          let baseUrl = HarchWeb.localServerBaseUrl server
              spacesUrl = baseUrl <> "/spaces"
              loginUrl = baseUrl <> "/login"
              routeStatus = css "[data-navigation-route-status]"
          runBrowserSpec browser do
            visit spacesUrl
            blockRequestsMatching "**/second"
            _ <-
              runPageScript
                "const status = document.querySelector('[data-navigation-route-status]'); let count = 0; status.dataset.testMutationCount = '0'; new MutationObserver((records) => { count += records.filter((record) => record.type === 'childList' || record.type === 'characterData').length; status.dataset.testMutationCount = String(count); }).observe(status, { childList: true, characterData: true, subtree: true }); true"
            press (byRole Link `named` "Second") "Enter"
            waitForBlockedRequestsMatching "**/second"
            press (byRole Link `named` "Profile") "Enter"
            releaseRequestsMatching "**/second"
            assertAllObserved do
              currentUrl `matches` (`shouldBe` loginUrl)
              textContent (byRole Heading) `matches` (`shouldBe` "Sign in")
              textContent routeStatus `matches` (`shouldBe` "web-api: Sign in")
              attributeValue routeStatus "data-test-mutation-count" `matches` (`shouldBe` Just "1")
              browserMetrics `matches` \metrics ->
                $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 2, hardNavigationCount = 0}|])
              isFocused (css "#app-main") `satisfies` id

    it "falls back natively for failed, incompatible, and unsafe final responses without announcing success" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildApp appConfig) $ \server -> do
          let baseUrl = HarchWeb.localServerBaseUrl server
              secondUrl = baseUrl <> "/second"
              spacesUrl = baseUrl <> "/spaces"
              routeStatus = byRole Status
              assertNativeFallback = assertAllObserved do
                currentUrl `matches` (`shouldBe` secondUrl)
                textContent (byRole Heading) `matches` (`shouldBe` "Second")
                textContent routeStatus `matches` (`shouldBe` "")
                browserMetrics `matches` \metrics ->
                  $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 1, hardNavigationCount = 1}|])
          runBrowserSpec browser do
            visit spacesUrl
            blockRequestsMatching "**/second"
            press (byRole Link `named` "Second") "Enter"
            failBlockedRequestsMatching "**/second"
            assertNativeFallback
            visit spacesUrl
            _ <-
              runPageScript
                "const originalFetch = window.fetch.bind(window); window.fetch = async (...arguments_) => { const response = await originalFetch(...arguments_); return { ok: response.ok, url: response.url, text: async () => '<!DOCTYPE html><html><head><title>Incompatible</title></head><body><main>Missing lifecycle markers</main></body></html>' }; }; true"
            press (byRole Link `named` "Second") "Enter"
            assertNativeFallback
            visit spacesUrl
            _ <-
              runPageScript
                "const originalFetch = window.fetch.bind(window); window.fetch = async (...arguments_) => { const response = await originalFetch(...arguments_); return { ok: response.ok, url: 'https://outside.example/redirect', text: () => response.text() }; }; true"
            press (byRole Link `named` "Second") "Enter"
            assertNativeFallback
            visit spacesUrl
            _ <-
              runPageScript
                "const originalFetch = window.fetch.bind(window); window.fetch = async (...arguments_) => { const response = await originalFetch(...arguments_); return { ok: response.ok, url: '://malformed', text: () => response.text() }; }; true"
            press (byRole Link `named` "Second") "Enter"
            assertNativeFallback

    it "keeps delayed-runtime and scripts-disabled keyboard navigation native, including the skip link" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildApp appConfig) $ \server -> do
          let baseUrl = HarchWeb.localServerBaseUrl server
              secondUrl = baseUrl <> "/second"
              spacesUrl = baseUrl <> "/spaces"
              mainContent = css "#app-main"
          runBrowserSpec browser do
            blockRequestsMatching "**/assets/navigation.js"
            visit secondUrl
            press (byRole Link `named` "Spaces") "Enter"
            assertAllObserved do
              currentUrl `matches` (`shouldBe` spacesUrl)
              textContent (byRole Status) `matches` (`shouldBe` "")
              browserMetrics `matches` \metrics ->
                $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 0, hardNavigationCount = 1}|])
            releaseRequestsMatching "**/assets/navigation.js"
            visitWithoutScripts secondUrl
            press (css "body") "Tab"
            assertAllObserved do
              isFocused (byRole Link `named` "Skip to main content") `satisfies` id
            press (byRole Link `named` "Skip to main content") "Enter"
            assertAllObserved do
              isFocused mainContent `satisfies` id
            press (byRole Link `named` "Spaces") "Enter"
            assertAllObserved do
              currentUrl `matches` (`shouldBe` spacesUrl)
              textContent (byRole Heading) `matches` (`shouldBe` "Site under construction")
              browserMetrics `matches` \metrics ->
                $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 0, hardNavigationCount = 1}|])

    it "preserves Spanish registration input until the delayed runtime sends its localized patch" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildAppWithDatabaseAndAccountWorkflow appConfig defaultPageRepository localizedRegistrationWorkflow) $ \server -> do
          let registrationUrl = HarchWeb.localServerBaseUrl server <> "/es/register"
              usernameField = byLabel "Nombre de usuario"
              emailField = byLabel "Direccion de correo"
              passwordField = byLabel "Contrasena"
          runBrowserSpec browser do
            blockRequestsMatching "**/assets/navigation.js"
            visit registrationUrl
            assertAllObserved do
              textContent (byRole Heading) `matches` (`shouldBe` "Crea tu cuenta")
            fill usernameField "person_01"
            _ <-
              runPageScript
                "const field = document.querySelector('#registration-email'); field.value = 'person@example.test'; field.dispatchEvent(new InputEvent('input', { bubbles: true, inputType: 'insertReplacementText', data: 'person@example.test' })); true"
            paste passwordField "correct horse battery staple"
            click (byRole Button `named` "Crear cuenta")
            assertAllObserved do
              currentUrl `matches` (`shouldBe` registrationUrl)
              inputValue usernameField `matches` (`shouldBe` "person_01")
              inputValue emailField `matches` (`shouldBe` "person@example.test")
              inputValue passwordField `matches` (`shouldBe` "correct horse battery staple")
              browserMetrics `matches` \metrics ->
                $([|metrics|] `shouldMatch` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 0}|])
            releaseRequestsMatching "**/assets/navigation.js"
            assertAllObserved do
              browserMetrics `matches` \metrics ->
                $([|metrics|] `shouldMatch` [p|BrowserMetrics {mutationRequestCount = 1}|])
              textContent (byText "Si esa direccion puede registrarse, revisa su bandeja de entrada para obtener un enlace de verificacion.") `matches` (`shouldBe` "Si esa direccion puede registrarse, revisa su bandeja de entrada para obtener un enlace de verificacion.")
              inputValue passwordField `matches` (`shouldBe` "")

    it "accepts pasted and autofill-compatible login values, clears secrets, and keeps focus visible when narrow and zoomed" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildApp appConfig) $ \server -> do
          let loginUrl = HarchWeb.localServerBaseUrl server <> "/login"
              identifierField = byLabel "Email address or username"
              passwordField = byLabel "Password"
              proofField = byLabel "Verification method"
              authenticatorField = byLabel "Authenticator code"
              recoveryField = byLabel "Recovery code"
          runBrowserSpec browser do
            setViewportSize 320 480
            visit loginUrl
            _ <-
              runPageScript
                "const field = document.querySelector('#login-identifier'); field.value = 'not an identifier!'; field.dispatchEvent(new InputEvent('input', { bubbles: true, inputType: 'insertReplacementText', data: 'not an identifier!' })); true"
            paste passwordField "short"
            paste authenticatorField "1"
            press identifierField "Tab"
            assertAllObserved do
              isFocused passwordField `satisfies` id
            press passwordField "Tab"
            assertAllObserved do
              isFocused proofField `satisfies` id
            press proofField "Tab"
            assertAllObserved do
              isFocused authenticatorField `satisfies` id
            click (byRole Button `named` "Sign in")
            assertAllObserved do
              isFocused (css "#login-error-summary") `satisfies` id
              inputValue identifierField `matches` (`shouldBe` "not an identifier!")
              inputValue passwordField `matches` (`shouldBe` "")
              inputValue authenticatorField `matches` (`shouldBe` "")
              browserMetrics `matches` \metrics ->
                $([|metrics|] `shouldMatch` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 1}|])
            _ <-
              runPageScript
                "const proof = document.querySelector('#login-proof'); proof.value = 'recovery'; proof.dispatchEvent(new Event('change', { bubbles: true })); const identifier = document.querySelector('#login-identifier'); identifier.value = 'person@example.test'; identifier.dispatchEvent(new InputEvent('input', { bubbles: true, inputType: 'insertReplacementText', data: 'person@example.test' })); document.documentElement.style.zoom = '2'; true"
            paste passwordField "correct horse battery staple"
            paste recoveryField "pasted-recovery"
            _ <-
              runPageScript
                "const field = document.querySelector('#login-recovery-code'); field.focus(); field.scrollIntoView({ block: 'nearest' }); const box = field.getBoundingClientRect(); field.dataset.testFocusVisible = String(field === document.activeElement && box.top >= 0 && box.bottom <= window.innerHeight); field.dataset.testFocusVisible"
            assertAllObserved do
              attributeValue recoveryField "data-test-focus-visible" `matches` (`shouldBe` Just "true")
            click (byRole Button `named` "Sign in")
            assertAllObserved do
              inputValue identifierField `matches` (`shouldBe` "person@example.test")
              inputValue passwordField `matches` (`shouldBe` "")
              inputValue authenticatorField `matches` (`shouldBe` "")
              inputValue recoveryField `matches` (`shouldBe` "")

    it "keeps client-only authentication forms semantically complete without scripts" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildApp appConfig) $ \server -> do
          let baseUrl = HarchWeb.localServerBaseUrl server
          runBrowserSpec browser do
            visitWithoutScripts (baseUrl <> "/register")
            assertAllObserved do
              attributeValue (css "#registration-region form") "method" `matches` (`shouldBe` Just "dialog")
              inputValue (byLabel "Password") `matches` (`shouldBe` "")
            press (byLabel "Username") "Tab"
            assertAllObserved do
              isFocused (byLabel "Email address") `satisfies` id
            press (byLabel "Email address") "Tab"
            assertAllObserved do
              isFocused (byLabel "Display name (optional)") `satisfies` id
            press (byLabel "Display name (optional)") "Tab"
            assertAllObserved do
              isFocused (byLabel "Password") `satisfies` id
            visitWithoutScripts (baseUrl <> "/login")
            assertAllObserved do
              attributeValue (css "#login-region form") "method" `matches` (`shouldBe` Just "dialog")
              textContent (byText "Choose Authenticator code above, then enter or paste its six-digit code.") `matches` (`shouldBe` "Choose Authenticator code above, then enter or paste its six-digit code.")
            visitWithoutScripts (baseUrl <> "/verify?token=delivered-token")
            assertAllObserved do
              attributeValue (css "#verification-region form") "method" `matches` (`shouldBe` Just "dialog")
              inputValue (byLabel "Verification token") `matches` (`shouldBe` "delivered-token")
            press (byLabel "Verification token") "Tab"
            assertAllObserved do
              isFocused (byRole Button `named` "Verify email") `satisfies` id
            visitWithoutScripts (baseUrl <> "/mfa")
            assertAllObserved do
              attributeValue (css "#mfa-enrollment-region form") "method" `matches` (`shouldBe` Just "dialog")
              textContent (byRole Button `named` "Start authenticator enrollment") `matches` (`shouldBe` "Start authenticator enrollment")

    it "keeps MFA confirmation keyboard- and paste-usable after its server patch" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildAppWithDatabaseAndAccountWorkflow appConfig defaultPageRepository mfaEnrollmentBrowserWorkflow) $ \server -> do
          let mfaUrl = HarchWeb.localServerBaseUrl server <> "/mfa"
              codeField = byLabel "Authenticator code"
          runBrowserSpec browser do
            setCookie mfaUrl mfaEnrollmentCookieName sessionToken
            visit mfaUrl
            csrfToken <- documentCsrfToken
            setCookie mfaUrl "__Host-harch-csrf" csrfToken
            click (byRole Button `named` "Start authenticator enrollment")
            assertAllObserved do
              isFocused codeField `satisfies` id
            press codeField "Tab"
            assertAllObserved do
              isFocused (byRole Button `named` "Confirm authenticator") `satisfies` id
            paste codeField "123"
            click (byRole Button `named` "Confirm authenticator")
            assertAllObserved do
              inputValue codeField `matches` (`shouldBe` "")
              browserMetrics `matches` \metrics ->
                $([|metrics|] `shouldMatch` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 2}|])
              isFocused codeField `satisfies` id

    it "focuses a multi-error registration summary and follows its field link by keyboard" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildAppWithDatabaseAndAccountWorkflow appConfig defaultPageRepository localizedRegistrationWorkflow) $ \server -> do
          let registrationUrl = HarchWeb.localServerBaseUrl server <> "/register"
              oversizedEmail = Text.replicate 245 "a" <> "@example.test"
              usernameField = byLabel "Username"
              emailField = byLabel "Email address"
              passwordField = byLabel "Password"
              usernameErrorLink = byRole Link `named` "Use a username with 3 to 20 letters, numbers, underscores, or hyphens."
          runBrowserSpec browser do
            visit registrationUrl
            fill usernameField "no!"
            fill emailField oversizedEmail
            fill passwordField "correct horse battery staple"
            click (byRole Button `named` "Create account")
            assertAllObserved do
              isFocused (css "#registration-error-summary") `satisfies` id
              textContent (byRole Heading `named` "Fix the following problems") `matches` (`shouldBe` "Fix the following problems")
              inputValue usernameField `matches` (`shouldBe` "no!")
              inputValue emailField `matches` (`shouldBe` oversizedEmail)
              inputValue passwordField `matches` (`shouldBe` "")
              attributeValue passwordField "aria-describedby" `matches` (`shouldBe` Just "registration-password-hint")
            press usernameErrorLink "Enter"
            assertAllObserved do
              isFocused usernameField `satisfies` id

    it "resends a pending-profile verification email through the immediate capture path" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildAppWithDatabaseAndAccountWorkflowAndSecurity appConfig defaultPageRepository pendingProfileWorkflow pendingProfileE2eSecurity) $ \server -> do
          let profileUrl = HarchWeb.localServerBaseUrl server <> "/profile"
          runBrowserSpec browser do
            setCookie profileUrl sessionCookieName sessionToken
            visit profileUrl
            csrfToken <- documentCsrfToken
            setCookie profileUrl "__Host-harch-csrf" csrfToken
            assertAllObserved do
              textContent (byRole Heading) `matches` (`shouldBe` "Profile")
              textContent (byText "person@example.test") `matches` (`shouldBe` "person@example.test")
            click (byRole Button `named` "Resend verification email")
            assertAllObserved do
              textContent (byText "Check your inbox for a verification link.") `matches` (`shouldBe` "Check your inbox for a verification link.")
              browserMetrics `matches` \metrics ->
                $([|metrics|] `shouldMatch` [p|BrowserMetrics {mutationRequestCount = 1}|])

    it "does not retain a CSRF-rejected action in the durable account fixture" $
      withTestAccountJwtFixture $ \environmentConfig _ -> do
        runtime <- requiredAccountJwtRuntime environmentConfig
        initialNow <- Time.currentUnixTimeNanoseconds
        initialSessionId <- Session.generateSessionId
        let initialSession = Session.OpaqueSession initialSessionId pendingProfileAccountId initialNow (initialNow + 86400000000000)
            issuer = accountJwtIssuerFromRuntime runtime
        initialJwt <- issueInitialSessionJwt issuer initialSession
        sessionsReference <- newIORef [initialSession]
        profileLoadsReference <- newIORef (0 :: Int)
        deliveryCountReference <- newIORef (0 :: Int)
        workflow <- reauthenticationProfileWorkflow ReauthenticationKeepsSessions environmentConfig issuer sessionsReference profileLoadsReference deliveryCountReference
        let security = accountJwtSecurity runtime (accountWorkflowSessionStore workflow)
        withBrowserApp $ \browser appConfig ->
          HarchWeb.withLocalTestServer (buildAppWithDatabaseAndAccountWorkflowAndSecurity appConfig defaultPageRepository workflow security) $ \server -> do
            let profileUrl = Text.replace "127.0.0.1" "localhost" (HarchWeb.localServerBaseUrl server) <> "/profile"
                reauthenticationDialog = css "#reauthentication-dialog"
            runBrowserSpec browser do
              setCookie profileUrl sessionCookieName (TextEncoding.decodeUtf8 (HarchWeb.encodedJwtBytes initialJwt))
              visit profileUrl
              _ <- runPageScript "document.body.dataset.harchCsrfToken = 'not-the-rendered-page-token'"
              click (byRole Button `named` "Resend verification email")
              assertAllObserved do
                attributeValue reauthenticationDialog "open" `matches` (`shouldBe` Nothing)
                textContent (css "[data-profile-resend] [data-harch-action-status]") `matches` (`shouldBe` "Completed.")
                browserMetrics `matches` \metrics ->
                  $([|metrics|] `shouldMatch` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 1}|])
        readIORef profileLoadsReference `shouldReturn` 1
        readIORef deliveryCountReference `shouldReturn` 0

    it "expires a retained profile action without leaving the reauthentication dialog open" $
      withTestAccountJwtFixture $ \environmentConfig _ -> do
        runtime <- requiredAccountJwtRuntime environmentConfig
        initialNow <- Time.currentUnixTimeNanoseconds
        initialSessionId <- Session.generateSessionId
        let initialSession = Session.OpaqueSession initialSessionId pendingProfileAccountId initialNow (initialNow + 86400000000000)
            issuer = accountJwtIssuerFromRuntime runtime
        initialJwt <- issueInitialSessionJwt issuer initialSession
        sessionsReference <- newIORef [initialSession]
        profileLoadsReference <- newIORef (0 :: Int)
        deliveryCountReference <- newIORef (0 :: Int)
        workflow <- reauthenticationProfileWorkflow ReauthenticationExpiresInitialSession environmentConfig issuer sessionsReference profileLoadsReference deliveryCountReference
        let security = accountJwtSecurity runtime (accountWorkflowSessionStore workflow)
        withBrowserApp $ \browser appConfig ->
          HarchWeb.withLocalTestServer (buildAppWithDatabaseAndAccountWorkflowAndSecurity appConfig defaultPageRepository workflow security) $ \server -> do
            let profileUrl = Text.replace "127.0.0.1" "localhost" (HarchWeb.localServerBaseUrl server) <> "/profile"
                profileSubmit = byRole Button `named` "Resend verification email"
                reauthenticationDialog = css "#reauthentication-dialog"
            runBrowserSpec browser do
              setCookie profileUrl sessionCookieName (TextEncoding.decodeUtf8 (HarchWeb.encodedJwtBytes initialJwt))
              visit profileUrl
              _ <- runPageScript "document.querySelector('[data-profile-resend] form').dataset.harchActionRetentionMs = '1000'"
              click profileSubmit
              assertAllObserved do
                attributeValue reauthenticationDialog "open" `matches` (`shouldBe` Nothing)
                textContent (css "[data-profile-resend] [data-harch-action-status]") `matches` (`shouldBe` "This action needs your attention.")
                isFocused profileSubmit `satisfies` id
                browserMetrics `matches` \metrics ->
                  $([|metrics|] `shouldMatch` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 1}|])
        readIORef deliveryCountReference `shouldReturn` 0

    it "recovers one retained profile action after its signed durable session expires" $
      withTestAccountJwtFixture $ \environmentConfig _ -> do
        runtime <- requiredAccountJwtRuntime environmentConfig
        initialNow <- Time.currentUnixTimeNanoseconds
        initialSessionId <- Session.generateSessionId
        let initialSession =
              Session.OpaqueSession
                { Session.sessionId = initialSessionId,
                  Session.sessionPrincipal = pendingProfileAccountId,
                  Session.sessionIssuedAtNanoseconds = initialNow,
                  Session.sessionExpiresAtNanoseconds = initialNow + 86400000000000
                }
            expiredInitialSession = initialSession {Session.sessionExpiresAtNanoseconds = initialNow}
            issuer = accountJwtIssuerFromRuntime runtime
        initialJwt <- issueInitialSessionJwt issuer initialSession
        sessionsReference <- newIORef [initialSession]
        profileLoadsReference <- newIORef (0 :: Int)
        deliveryCountReference <- newIORef (0 :: Int)
        workflow <- reauthenticationProfileWorkflow ReauthenticationExpiresInitialSession environmentConfig issuer sessionsReference profileLoadsReference deliveryCountReference
        let security = accountJwtSecurity runtime (accountWorkflowSessionStore workflow)
        withBrowserApp $ \browser appConfig ->
          HarchWeb.withLocalTestServer (buildAppWithDatabaseAndAccountWorkflowAndSecurity appConfig defaultPageRepository workflow security) $ \server -> do
            let profileUrl = Text.replace "127.0.0.1" "localhost" (HarchWeb.localServerBaseUrl server) <> "/profile"
                profileSubmit = byRole Button `named` "Resend verification email"
                reauthenticationDialog = css "#reauthentication-dialog"
                identifierField = byLabel "Email address or username"
                passwordField = byLabel "Password"
                authenticatorCodeField = byLabel "Authenticator code"
                retryOriginalAction = byRole Button `named` "Retry original action"
            runBrowserSpec browser do
              setCookie profileUrl sessionCookieName (TextEncoding.decodeUtf8 (HarchWeb.encodedJwtBytes initialJwt))
              visit profileUrl
              assertAllObserved do
                textContent (byRole Heading `named` "Profile") `matches` (`shouldBe` "Profile")
              click profileSubmit
              assertAllObserved do
                attributeValue reauthenticationDialog "open" `matches` (`shouldBe` Just "")
                inputValue identifierField `matches` (`shouldBe` "")
                browserMetrics `matches` \metrics ->
                  $([|metrics|] `shouldMatch` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 1}|])
              fill identifierField "person@example.test"
              fill passwordField "incorrect password"
              fill authenticatorCodeField reauthenticationTotpCode
              click (byRole Button `named` "Sign in")
              assertAllObserved do
                attributeValue reauthenticationDialog "open" `matches` (`shouldBe` Just "")
                inputValue passwordField `matches` (`shouldBe` "")
                browserMetrics `matches` \metrics ->
                  $([|metrics|] `shouldMatch` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 2}|])
              press reauthenticationDialog "Escape"
              assertAllObserved do
                attributeValue reauthenticationDialog "open" `matches` (`shouldBe` Nothing)
                isFocused profileSubmit `satisfies` id
                browserMetrics `matches` \metrics ->
                  $([|metrics|] `shouldMatch` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 2}|])
              click profileSubmit
              assertAllObserved do
                attributeValue reauthenticationDialog "open" `matches` (`shouldBe` Just "")
                browserMetrics `matches` \metrics ->
                  $([|metrics|] `shouldMatch` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 3}|])
              fill identifierField "person@example.test"
              fill passwordField "correct horse battery staple"
              fill authenticatorCodeField reauthenticationTotpCode
              click (byRole Button `named` "Sign in")
              assertAllObserved do
                textContent (css "[data-web-api-reauthentication-status]") `matches` (`shouldBe` "Signed in. Confirm to retry the original action.")
                attributeValue retryOriginalAction "hidden" `matches` (`shouldBe` Nothing)
                browserMetrics `matches` \metrics ->
                  $([|metrics|] `shouldMatch` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 4}|])
              click retryOriginalAction
              assertAllObserved do
                textContent (byText "Check your inbox for a verification link.") `matches` (`shouldBe` "Check your inbox for a verification link.")
                attributeValue reauthenticationDialog "open" `matches` (`shouldBe` Nothing)
                browserMetrics `matches` \metrics ->
                  $([|metrics|] `shouldMatch` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 5}|])
        deliveryCount <- readIORef deliveryCountReference
        deliveryCount `shouldBe` 1
        sessions <- readIORef sessionsReference
        find ((== initialSessionId) . Session.sessionId) sessions `shouldBe` Just expiredInitialSession

    it "keeps one retained profile action available through corrected password and MFA failures" $
      withTestAccountJwtFixture $ \environmentConfig _ -> do
        runtime <- requiredAccountJwtRuntime environmentConfig
        initialNow <- Time.currentUnixTimeNanoseconds
        initialSessionId <- Session.generateSessionId
        let initialSession =
              Session.OpaqueSession
                { Session.sessionId = initialSessionId,
                  Session.sessionPrincipal = pendingProfileAccountId,
                  Session.sessionIssuedAtNanoseconds = initialNow,
                  Session.sessionExpiresAtNanoseconds = initialNow + 86400000000000
                }
            issuer = accountJwtIssuerFromRuntime runtime
        initialJwt <- issueInitialSessionJwt issuer initialSession
        sessionsReference <- newIORef [initialSession]
        profileLoadsReference <- newIORef (0 :: Int)
        deliveryCountReference <- newIORef (0 :: Int)
        workflow <- reauthenticationProfileWorkflow ReauthenticationExpiresInitialSession environmentConfig issuer sessionsReference profileLoadsReference deliveryCountReference
        let security = accountJwtSecurity runtime (accountWorkflowSessionStore workflow)
        withBrowserApp $ \browser appConfig ->
          HarchWeb.withLocalTestServer (buildAppWithDatabaseAndAccountWorkflowAndSecurity appConfig defaultPageRepository workflow security) $ \server -> do
            let profileUrl = Text.replace "127.0.0.1" "localhost" (HarchWeb.localServerBaseUrl server) <> "/profile"
                profileSubmit = byRole Button `named` "Resend verification email"
                reauthenticationDialog = css "#reauthentication-dialog"
                identifierField = byLabel "Email address or username"
                passwordField = byLabel "Password"
                authenticatorCodeField = byLabel "Authenticator code"
                retryOriginalAction = css "[data-web-api-reauthentication-retry]"
                loginForm = css "#login-region form"
            runBrowserSpec browser do
              setCookie profileUrl sessionCookieName (TextEncoding.decodeUtf8 (HarchWeb.encodedJwtBytes initialJwt))
              visit profileUrl
              click profileSubmit
              fill identifierField "person@example.test"
              fill passwordField "incorrect password"
              fill authenticatorCodeField reauthenticationTotpCode
              click (byRole Button `named` "Sign in")
              assertAllObserved do
                attributeValue reauthenticationDialog "open" `matches` (`shouldBe` Just "")
                attributeValue loginForm "aria-busy" `matches` (`shouldBe` Nothing)
                attributeValue retryOriginalAction "hidden" `matches` (`shouldBe` Just "")
                inputValue passwordField `matches` (`shouldBe` "")
                inputValue authenticatorCodeField `matches` (`shouldBe` "")
                browserMetrics `matches` \metrics ->
                  $([|metrics|] `shouldMatch` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 2}|])
              fill identifierField "person@example.test"
              fill passwordField "correct horse battery staple"
              fill authenticatorCodeField "000000"
              click (byRole Button `named` "Sign in")
              assertAllObserved do
                attributeValue reauthenticationDialog "open" `matches` (`shouldBe` Just "")
                attributeValue loginForm "aria-busy" `matches` (`shouldBe` Nothing)
                attributeValue retryOriginalAction "hidden" `matches` (`shouldBe` Just "")
                inputValue passwordField `matches` (`shouldBe` "")
                inputValue authenticatorCodeField `matches` (`shouldBe` "")
                browserMetrics `matches` \metrics ->
                  $([|metrics|] `shouldMatch` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 3}|])
              fill identifierField "person@example.test"
              fill passwordField "correct horse battery staple"
              fill authenticatorCodeField reauthenticationTotpCode
              click (byRole Button `named` "Sign in")
              assertAllObserved do
                textContent (css "[data-web-api-reauthentication-status]") `matches` (`shouldBe` "Signed in. Confirm to retry the original action.")
                attributeValue retryOriginalAction "hidden" `matches` (`shouldBe` Nothing)
                browserMetrics `matches` \metrics ->
                  $([|metrics|] `shouldMatch` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 4}|])
              click retryOriginalAction
              assertAllObserved do
                textContent (byText "Check your inbox for a verification link.") `matches` (`shouldBe` "Check your inbox for a verification link.")
                attributeValue reauthenticationDialog "open" `matches` (`shouldBe` Nothing)
                browserMetrics `matches` \metrics ->
                  $([|metrics|] `shouldMatch` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 5}|])
        readIORef deliveryCountReference `shouldReturn` 1

    it "does not open a second reauthentication dialog when replay is rejected again" $
      withTestAccountJwtFixture $ \environmentConfig _ -> do
        runtime <- requiredAccountJwtRuntime environmentConfig
        initialNow <- Time.currentUnixTimeNanoseconds
        initialSessionId <- Session.generateSessionId
        let initialSession = Session.OpaqueSession initialSessionId pendingProfileAccountId initialNow (initialNow + 86400000000000)
            issuer = accountJwtIssuerFromRuntime runtime
        initialJwt <- issueInitialSessionJwt issuer initialSession
        sessionsReference <- newIORef [initialSession]
        profileLoadsReference <- newIORef (0 :: Int)
        deliveryCountReference <- newIORef (0 :: Int)
        workflow <- reauthenticationProfileWorkflow ReauthenticationExpiresIssuedSessions environmentConfig issuer sessionsReference profileLoadsReference deliveryCountReference
        let security = accountJwtSecurity runtime (accountWorkflowSessionStore workflow)
        withBrowserApp $ \browser appConfig ->
          HarchWeb.withLocalTestServer (buildAppWithDatabaseAndAccountWorkflowAndSecurity appConfig defaultPageRepository workflow security) $ \server -> do
            let profileUrl = Text.replace "127.0.0.1" "localhost" (HarchWeb.localServerBaseUrl server) <> "/profile"
                profileSubmit = byRole Button `named` "Resend verification email"
                reauthenticationDialog = css "#reauthentication-dialog"
                identifierField = byLabel "Email address or username"
                passwordField = byLabel "Password"
                authenticatorCodeField = byLabel "Authenticator code"
                retryOriginalAction = byRole Button `named` "Retry original action"
            runBrowserSpec browser do
              setCookie profileUrl sessionCookieName (TextEncoding.decodeUtf8 (HarchWeb.encodedJwtBytes initialJwt))
              visit profileUrl
              click profileSubmit
              fill identifierField "person@example.test"
              fill passwordField "correct horse battery staple"
              fill authenticatorCodeField reauthenticationTotpCode
              click (byRole Button `named` "Sign in")
              assertAllObserved do
                textContent (css "[data-web-api-reauthentication-status]") `matches` (`shouldBe` "Signed in. Confirm to retry the original action.")
              click retryOriginalAction
              assertAllObserved do
                attributeValue reauthenticationDialog "open" `matches` (`shouldBe` Nothing)
                textContent (css "[data-profile-resend] [data-harch-action-status]") `matches` (`shouldBe` "This action needs your attention.")
                browserMetrics `matches` \metrics ->
                  $([|metrics|] `shouldMatch` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 3}|])
        readIORef deliveryCountReference `shouldReturn` 0

withBrowserApp :: (BrowserConfig -> AppConfig -> IO a) -> IO a
withBrowserApp action = do
  loadedConfig <- loadPlaywrightBrowserConfig
  browser <-
    case loadedConfig of
      Left loadError -> expectationFailure loadError >> fail "unreachable"
      Right config -> pure config
  withSystemTempDirectory "web-api-e2e-assets" $ \assetDirectory ->
    do
      let stylesDirectory = assetDirectory </> "styles"
      createDirectory stylesDirectory
      sourceStylesheet <- findSourceStylesheet
      copyFile sourceStylesheet (stylesDirectory </> "app.css")
      action
        browser
        defaultAppConfig
          { staticAssets =
              StaticAssetsConfig
                { staticAssetRoots =
                    [ StaticAssetRoot
                        { staticUrlPrefix = "/assets",
                          staticDirectory = assetDirectory
                        }
                    ],
                  staticAssetContentTypes = defaultStaticAssetContentTypes,
                  staticCacheControlSeconds = Nothing
                }
          }

challengedBrowserApp :: AppConfig -> HarchWeb.Application AppRoute AccountAction WebApi.Route.AppRequestContext ()
challengedBrowserApp appConfig =
  buildAppWithDatabaseAndAccountWorkflowAndSecurity
    appConfig
    defaultPageRepository
    unavailableAccountWorkflow
    unauthenticatedProfileChallengeSecurity

unauthenticatedProfileChallengeSecurity :: HarchWeb.ApplicationSecurity AppRoute WebApi.Route.AppRequestContext ()
unauthenticatedProfileChallengeSecurity =
  HarchWeb.AuthenticationEnabled
    []
    ( HarchWeb.AuthenticationGuard
        ( \endpointRequest ->
            let routeRequest = HarchWeb.endpointRouteRequest endpointRequest
             in pure $
                  case HarchWeb.endpointAccess (HarchWeb.endpointMetadata endpointRequest) of
                    HarchWeb.AllowUnauthenticated -> HarchWeb.ContinueEndpoint (HarchWeb.requestContext routeRequest)
                    _ -> HarchWeb.HaltEndpoint (HarchWeb.nonPageInternalRedirectResponse Http.status303 (HarchWeb.RouteRequest LoginRoute (HarchWeb.requestContext routeRequest)))
        )
    )
    []

documentCsrfToken :: BrowserScenario Text
documentCsrfToken = do
  value <- runPageScript "document.body.dataset.harchCsrfToken"
  case Aeson.fromJSON value of
    Aeson.Success token -> pure token
    Aeson.Error _ -> error "browser fixture expected a CSRF token string"

-- | Cabal can execute this suite from either the package directory, the
-- workspace root, or a build directory.  Locate the checked-in stylesheet
-- relative to an ancestor rather than making the browser fixture depend on
-- the runner's working directory.
findSourceStylesheet :: IO FilePath
findSourceStylesheet = getCurrentDirectory >>= searchFrom
  where
    searchFrom directory = do
      let candidates =
            [ directory </> "public/styles/app.css",
              directory </> "packages/web-api/public/styles/app.css"
            ]
      existing <- firstExisting candidates
      case existing of
        Just stylesheet -> pure stylesheet
        Nothing ->
          let parent = takeDirectory directory
           in if parent == directory
                then ioError (userError "could not locate packages/web-api/public/styles/app.css")
                else searchFrom parent

    firstExisting paths =
      case paths of
        [] -> pure Nothing
        path : remaining -> do
          exists <- doesFileExist path
          if exists
            then pure (Just path)
            else firstExisting remaining

pendingProfileWorkflow :: AccountWorkflow
pendingProfileWorkflow =
  unavailableAccountWorkflow
    { accountWorkflowStore =
        AccountStore
          { createPendingAccount = \_ _ -> error "unexpected account creation",
            completePendingRegistrationDelivery = \_ -> pure (Right True),
            releasePendingRegistrationDelivery = \_ -> pure (Right True),
            reserveVerificationResend = \_ verification _ -> pure (Right (VerificationResendReserved (VerificationResendClaim (Account.storedVerificationAccountId verification) (Account.storedVerificationTokenDigest verification)))),
            completeVerificationResend = \_ _ -> pure (Right VerificationResendClaimSettled),
            releaseVerificationResend = \_ -> pure (Right VerificationResendClaimSettled),
            replaceEmailVerification = \_ -> pure (Right True),
            findEmailVerification = \_ -> error "unexpected verification lookup",
            consumeEmailVerification = \_ _ -> error "unexpected verification consumption"
          },
      accountWorkflowEmailDelivery = Email.EmailDelivery (\_ -> pure ()),
      accountWorkflowClock = pure 100,
      accountWorkflowSessionStore =
        AccountSessionStore
          { saveAccountSession = \_ -> error "unexpected session save",
            loadAccountSession = \receivedSessionId ->
              pure (Right (if receivedSessionId == pendingProfileSessionId then Just pendingProfileSession else Nothing)),
            invalidateAccountSession = \_ _ -> error "unexpected session invalidation"
          },
      accountWorkflowProfileStore =
        AccountProfileStore
          { findAccountProfile = \receivedAccountId ->
              pure (Right (if receivedAccountId == pendingProfileAccountId then Just pendingProfile else Nothing))
          },
      accountWorkflowVerificationUrl = \_ _ -> "https://account.example.test/verify"
    }

-- The account-JWT unit/integration tests prove proof verification and durable
-- revocation. This browser fixture starts after that admission boundary with a
-- known principal so it can exercise the protected profile action's capture
-- and patch behavior without putting test signing keys in browser state.
pendingProfileE2eSecurity :: HarchWeb.ApplicationSecurity AppRoute WebApi.Route.AppRequestContext ()
pendingProfileE2eSecurity =
  HarchWeb.AuthenticationEnabled
    []
    ( HarchWeb.AuthenticationGuard
        ( \endpointRequest ->
            pure
              ( HarchWeb.ContinueEndpoint
                  ( (HarchWeb.requestContext (HarchWeb.endpointRouteRequest endpointRequest))
                      { WebApi.Route.requestAccountPrincipal =
                          Just
                            ( mkAccountPrincipal
                                pendingProfileAccountId
                                pendingProfileSessionId
                                (Session.sessionExpiresAtNanoseconds pendingProfileSession)
                            )
                      }
                  )
              )
        )
    )
    []

-- | This fixture is deliberately stateful at the durable-session boundary:
-- the first authenticated profile render uses a real signed JWT and active
-- session, then the profile repository marks that same server-side session
-- expired.  The action therefore proves a deliverable JWT is insufficient
-- after durable expiry, while the modal login issues a fresh signed session
-- through the ordinary account workflow.
data ReauthenticationSessionExpiry
  = ReauthenticationKeepsSessions
  | ReauthenticationExpiresInitialSession
  | ReauthenticationExpiresIssuedSessions

reauthenticationProfileWorkflow :: ReauthenticationSessionExpiry -> AppEnvironmentConfig -> AccountJwt.AccountJwtIssuer -> IORef [Session.OpaqueSession Account.AccountId] -> IORef Int -> IORef Int -> IO AccountWorkflow
reauthenticationProfileWorkflow sessionExpiry environmentConfig issuer sessionsReference profileLoadsReference deliveryCountReference = do
  let password = Password.mkPassword "correct horse battery staple"
      passwordHash = fromMaybe (error "expected deterministic test password hash") (Password.hashPasswordWithSalt Password.defaultPasswordHashingPolicy (ByteString.replicate 16 8) password)
      credential = AccountCredential pendingProfileAccountId passwordHash True
      totpSecret = fromMaybe (error "expected deterministic test TOTP secret") (Totp.mkTotpSecret reauthenticationTotpSecret)
      encryptedTotpSecret = requiredEncryptedTotpSecret (Secret.encryptSecretWithNonce (totpEncryptionKey environmentConfig) (requiredEncryptionNonce (ByteString.replicate 12 8)) (Secret.mkSecretPlaintext (TextEncoding.encodeUtf8 (Totp.renderTotpSecret totpSecret))))
      enrollment = StoredTotpEnrollment encryptedTotpSecret (Just 1) Nothing
      sessionStore =
        AccountSessionStore
          { saveAccountSession = \session -> modifyIORef' sessionsReference (sessionForFixture session :) >> pure (Right True),
            loadAccountSession = \receivedSessionId -> do
              sessions <- readIORef sessionsReference
              pure (Right (find ((== receivedSessionId) . Session.sessionId) sessions)),
            invalidateAccountSession = \_ _ -> pure (Right True)
          }
      profileStore =
        AccountProfileStore
          { findAccountProfile = \receivedAccountId -> do
              firstProfileLoad <- atomicModifyIORef' profileLoadsReference (\count -> (count + 1, count == 0))
              when (firstProfileLoad && expiresInitialProfileSession sessionExpiry) (expireInitialProfileSession sessionsReference)
              pure (Right (if receivedAccountId == pendingProfileAccountId then Just pendingProfile else Nothing))
          }
      mfaStore =
        MfaStore
          { saveUnconfirmedTotpEnrollment = \_ _ _ -> error "unexpected enrollment save",
            loadTotpEnrollment = \_ -> pure (Right (Just enrollment)),
            confirmTotpEnrollment = \_ _ _ -> error "unexpected enrollment confirmation",
            loadUnusedRecoveryCodeHashes = \_ -> pure (Right []),
            consumeRecoveryCodeHash = \_ _ _ -> pure (Right True),
            markTotpCodeUsed = \_ _ -> pure (Right True)
          }
      credentialStore =
        AccountCredentialStore
          { findAccountCredentialByEmail = \_ -> pure (Right (Just credential)),
            findAccountCredentialByUsername = \_ -> pure (Right (Just credential)),
            replacePasswordHashIfCurrent = \_ _ _ -> pure (Right False)
          }
      permissiveAttemptStore =
        LoginAttemptStore
          { reserveLoginAttempt = \_ _ -> pure (Right (LoginAttemptReserved (LoginAttemptReservation "reauthentication-login"))),
            settleLoginAttempt = \_ _ -> pure (Right ()),
            cancelLoginAttempt = \_ -> pure (Right ())
          }
      sessionForFixture session =
        case sessionExpiry of
          ReauthenticationKeepsSessions -> session
          ReauthenticationExpiresInitialSession -> session
          ReauthenticationExpiresIssuedSessions -> session {Session.sessionExpiresAtNanoseconds = Session.sessionIssuedAtNanoseconds session}
  pure
    unavailableAccountWorkflow
      { accountWorkflowStore = pendingProfileAccountStore,
        accountWorkflowEmailDelivery = Email.EmailDelivery (\_ -> modifyIORef' deliveryCountReference (+ 1)),
        accountWorkflowClock = Time.currentUnixTimeNanoseconds,
        accountWorkflowMfaStore = mfaStore,
        accountWorkflowCredentialStore = credentialStore,
        accountWorkflowLoginAttemptStore = permissiveAttemptStore,
        accountWorkflowSessionStore = sessionStore,
        accountWorkflowProfileStore = profileStore,
        accountWorkflowTotpEncryptionKey = totpEncryptionKey environmentConfig,
        accountWorkflowJwtIssuer = issuer,
        accountWorkflowTotpClock = const 123456,
        accountWorkflowVerificationUrl = \_ _ -> "https://account.example.test/verify"
      }

expiresInitialProfileSession :: ReauthenticationSessionExpiry -> Bool
expiresInitialProfileSession sessionExpiry =
  case sessionExpiry of
    ReauthenticationKeepsSessions -> False
    ReauthenticationExpiresInitialSession -> True
    ReauthenticationExpiresIssuedSessions -> True

pendingProfileAccountStore :: AccountStore
pendingProfileAccountStore =
  AccountStore
    { createPendingAccount = \_ _ -> error "unexpected account creation",
      completePendingRegistrationDelivery = \_ -> pure (Right True),
      releasePendingRegistrationDelivery = \_ -> pure (Right True),
      reserveVerificationResend = \_ verification _ -> pure (Right (VerificationResendReserved (VerificationResendClaim (Account.storedVerificationAccountId verification) (Account.storedVerificationTokenDigest verification)))),
      completeVerificationResend = \_ _ -> pure (Right VerificationResendClaimSettled),
      releaseVerificationResend = \_ -> pure (Right VerificationResendClaimSettled),
      replaceEmailVerification = \_ -> pure (Right True),
      findEmailVerification = \_ -> error "unexpected verification lookup",
      consumeEmailVerification = \_ _ -> error "unexpected verification consumption"
    }

expireInitialProfileSession :: IORef [Session.OpaqueSession Account.AccountId] -> IO ()
expireInitialProfileSession sessionsReference =
  atomicModifyIORef' sessionsReference $ \sessions ->
    ( map expireSession sessions,
      ()
    )
  where
    expireSession session =
      session
        { Session.sessionExpiresAtNanoseconds = Session.sessionIssuedAtNanoseconds session
        }

accountJwtSecurity :: AccountJwtRuntime -> AccountSessionStore -> HarchWeb.ApplicationSecurity AppRoute WebApi.Route.AppRequestContext ()
accountJwtSecurity runtime sessionStore =
  HarchWeb.AuthenticationEnabled
    []
    (HarchWeb.authenticationGuardFromPipeline (accountJwtAuthenticationPipeline sessionStore Time.currentUnixTimeNanoseconds runtime))
    []

requiredAccountJwtRuntime :: AppEnvironmentConfig -> IO AccountJwtRuntime
requiredAccountJwtRuntime environmentConfig = do
  loaded <- loadAccountJwtRuntime (accountJwtConfiguration environmentConfig)
  case loaded of
    Right runtime -> pure runtime
    Left _ -> expectationFailure "expected test account-JWT runtime" >> error "unreachable"

issueInitialSessionJwt :: AccountJwt.AccountJwtIssuer -> Session.OpaqueSession Account.AccountId -> IO HarchWeb.EncodedJwt
issueInitialSessionJwt issuer session = do
  issued <- AccountJwt.issueAccountSessionJwt issuer session
  case issued of
    Right encodedJwt -> pure encodedJwt
    Left _ -> expectationFailure "expected a signed test account session JWT" >> error "unreachable"

requiredEncryptionNonce :: ByteString.ByteString -> Secret.EncryptionNonce
requiredEncryptionNonce value =
  fromMaybe (error "expected test encryption nonce") (Secret.mkEncryptionNonce value)

requiredEncryptedTotpSecret :: Crypto.CryptoFailable Text -> Text
requiredEncryptedTotpSecret encrypted =
  case encrypted of
    Crypto.CryptoPassed value -> value
    Crypto.CryptoFailed _ -> error "expected encrypted test TOTP secret"

reauthenticationTotpSecret :: Text
reauthenticationTotpSecret = "JBSWY3DPEHPK3PXPJBSWY3DPEHPK3PXP"

reauthenticationTotpCode :: Text
reauthenticationTotpCode =
  let secret = fromMaybe (error "expected deterministic test TOTP secret") (Totp.mkTotpSecret reauthenticationTotpSecret)
   in Totp.totpCodeText (Totp.totpCode 123456 secret)

localizedRegistrationWorkflow :: AccountWorkflow
localizedRegistrationWorkflow =
  unavailableAccountWorkflow
    { accountWorkflowStore =
        (accountWorkflowStore unavailableAccountWorkflow)
          { createPendingAccount = \_ _ -> pure (Right PendingAccountEmailTaken),
            completePendingRegistrationDelivery = \_ -> pure (Right True),
            releasePendingRegistrationDelivery = \_ -> pure (Right True)
          },
      accountWorkflowPasswordHasher = \_ _ -> pure (Just (Password.PasswordHash "test-password-hash"))
    }

mfaEnrollmentBrowserWorkflow :: AccountWorkflow
mfaEnrollmentBrowserWorkflow =
  unavailableAccountWorkflow
    { accountWorkflowClock = pure 100,
      accountWorkflowMfaEnrollmentSessionStore =
        MfaEnrollmentSessionStore
          { saveMfaEnrollmentSession = \_ -> error "unexpected MFA-enrollment session save",
            loadMfaEnrollmentSession = \receivedSessionId -> pure (Right (if receivedSessionId == pendingProfileSessionId then Just mfaEnrollmentBrowserSession else Nothing)),
            invalidateMfaEnrollmentSession = \_ _ -> error "unexpected MFA-enrollment session invalidation"
          },
      accountWorkflowMfaStore =
        MfaStore
          { saveUnconfirmedTotpEnrollment = \_ _ _ -> pure (Right True),
            loadTotpEnrollment = \_ -> error "invalid browser code must not load the enrollment",
            confirmTotpEnrollment = \_ _ _ -> error "invalid browser code must not confirm the enrollment",
            loadUnusedRecoveryCodeHashes = \_ -> error "unexpected recovery-code load",
            consumeRecoveryCodeHash = \_ _ _ -> error "unexpected recovery-code consumption",
            markTotpCodeUsed = \_ _ -> error "unexpected TOTP replay write"
          }
    }

mfaEnrollmentBrowserSession :: Session.OpaqueSession Account.AccountId
mfaEnrollmentBrowserSession =
  Session.OpaqueSession
    { Session.sessionId = pendingProfileSessionId,
      Session.sessionPrincipal = pendingProfileAccountId,
      Session.sessionIssuedAtNanoseconds = 0,
      Session.sessionExpiresAtNanoseconds = 1000
    }

pendingProfile :: AccountProfile
pendingProfile = AccountProfile pendingProfileAccountId pendingProfileEmail Nothing Nothing False

pendingProfileSession :: Session.OpaqueSession Account.AccountId
pendingProfileSession =
  Session.OpaqueSession
    { Session.sessionId = pendingProfileSessionId,
      Session.sessionPrincipal = pendingProfileAccountId,
      Session.sessionIssuedAtNanoseconds = 0,
      Session.sessionExpiresAtNanoseconds = 200
    }

pendingProfileAccountId :: Account.AccountId
pendingProfileAccountId = requiredAccountId "account_01"

pendingProfileEmail :: Email.EmailAddress
pendingProfileEmail = requiredEmailAddress "person@example.test"

pendingProfileSessionId :: Session.SessionId
pendingProfileSessionId = requiredSessionId sessionToken

sessionCookieName :: Text
sessionCookieName = Session.sessionCookieNameText (Session.sessionCookieName Session.defaultSessionCookiePolicy)

mfaEnrollmentCookieName :: Text
mfaEnrollmentCookieName = Session.sessionCookieNameText (Session.sessionCookieName mfaEnrollmentSessionCookiePolicy)

sessionToken :: Text
sessionToken = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_"

requiredAccountId :: Text -> Account.AccountId
requiredAccountId value =
  case Account.mkAccountId value of
    Just accountId -> accountId
    Nothing -> error "expected a valid account id"

requiredEmailAddress :: Text -> Email.EmailAddress
requiredEmailAddress value =
  case Email.mkEmailAddress value of
    Just emailAddress -> emailAddress
    Nothing -> error "expected a valid email address"

requiredSessionId :: Text -> Session.SessionId
requiredSessionId value =
  case Session.mkSessionId value of
    Just sessionIdValue -> sessionIdValue
    Nothing -> error "expected a valid session id"
