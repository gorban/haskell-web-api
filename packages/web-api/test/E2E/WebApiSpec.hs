{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# E2E_SPEC #-}

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb qualified
import HarchWeb.Account qualified as Account
import HarchWeb.Email qualified as Email
import HarchWeb.Password qualified as Password
import HarchWeb.Session qualified as Session
import System.Directory (copyFile, createDirectory, doesFileExist, getCurrentDirectory)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import WebApi.Account (AccountProfile (..), AccountProfileStore (..), AccountStore (..), CreatePendingAccountOutcome (..), VerificationResendAdmission (..), VerificationResendClaim (..), VerificationResendClaimSettlement (..))
import WebApi.App (buildApp, buildAppWithDatabaseAndAccountWorkflow, unavailableAccountWorkflow)
import WebApi.AppEffect (AccountWorkflow (..))
import WebApi.Config (AppConfig (..), StaticAssetRoot (..), StaticAssetsConfig (..), defaultAppConfig, defaultStaticAssetContentTypes)
import WebApi.Database (defaultPageRepository)
import WebApi.Mfa (MfaStore (..))
import WebApi.Session (AccountSessionStore (..), MfaEnrollmentSessionStore (..), mfaEnrollmentSessionCookiePolicy)

spec =
  describe "stacked application real-browser smoke coverage" $ do
    it "redirects the root route to the complete Spaces SSR document" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildApp appConfig) $ \server -> do
          let homeUrl = HarchWeb.localServerBaseUrl server <> "/"
          runBrowserScenario
            browser
            ( do
                visit homeUrl
                assertAll
                  ((,) <$> currentUrl <*> textContent (byRole Heading))
                  (\(url, heading) -> (url `shouldBe` (HarchWeb.localServerBaseUrl server <> "/spaces")) :| [heading `shouldBe` "Site under construction"])
            )
            `shouldReturn` Right ()

    it "keeps direct second-page loads and script-disabled root redirects usable" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildApp appConfig) $ \server -> do
          let homeUrl = HarchWeb.localServerBaseUrl server <> "/"
              secondUrl = HarchWeb.localServerBaseUrl server <> "/second"
          runBrowserScenario
            browser
            ( do
                visit secondUrl
                assertText (byRole Heading) (`shouldBe` "Second")
                visitWithoutScripts homeUrl
                assertAll
                  ((,) <$> currentUrl <*> textContent (byRole Heading))
                  (\(url, heading) -> (url `shouldBe` (HarchWeb.localServerBaseUrl server <> "/spaces")) :| [heading `shouldBe` "Site under construction"])
            )
            `shouldReturn` Right ()

    it "redirects Spanish roots to localized Spaces SSR content while scripts are disabled" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildApp appConfig) $ \server -> do
          let spanishHomeUrl = HarchWeb.localServerBaseUrl server <> "/es"
          runBrowserScenario
            browser
            ( do
                visitWithoutScripts spanishHomeUrl
                assertAll
                  ((,) <$> currentUrl <*> textContent (byRole Heading))
                  (\(url, heading) -> (url `shouldBe` (HarchWeb.localServerBaseUrl server <> "/es/spaces")) :| [heading `shouldBe` "Sitio en construcción"])
            )
            `shouldReturn` Right ()

    it "serves the app-home spaces placeholder through SSR and enhanced navigation" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildApp appConfig) $ \server -> do
          let homeUrl = HarchWeb.localServerBaseUrl server <> "/"
              secondUrl = HarchWeb.localServerBaseUrl server <> "/second"
              spacesUrl = HarchWeb.localServerBaseUrl server <> "/spaces"
              spanishSpacesUrl = HarchWeb.localServerBaseUrl server <> "/es/spaces"
          runBrowserScenario
            browser
            ( do
                visit homeUrl
                assertAll
                  ((,) <$> currentUrl <*> textContent (byRole Heading))
                  (\(url, heading) -> (url `shouldBe` spacesUrl) :| [heading `shouldBe` "Site under construction"])
                visit secondUrl
                click (byRole Link `named` "Spaces")
                assertAll
                  ((,,) <$> currentUrl <*> textContent (byRole Heading) <*> browserMetrics)
                  ( \(url, heading, metrics) ->
                      (url `shouldBe` spacesUrl)
                        :| [ heading `shouldBe` "Site under construction",
                             $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 1, hardNavigationCount = 0}|])
                           ]
                  )
                visitWithoutScripts spanishSpacesUrl
                assertAll
                  ((,) <$> textContent (byRole Heading) <*> textContent (byText "Sigan este espacio."))
                  (\(heading, body) -> (heading `shouldBe` "Sitio en construcción") :| [body `shouldBe` "Sigan este espacio."])
            )
            `shouldReturn` Right ()

    it "serves the app-home profile landing through SSR and enhanced navigation" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildApp appConfig) $ \server -> do
          let secondUrl = HarchWeb.localServerBaseUrl server <> "/second"
              profileUrl = HarchWeb.localServerBaseUrl server <> "/profile"
              spanishProfileUrl = HarchWeb.localServerBaseUrl server <> "/es/profile"
          runBrowserScenario
            browser
            ( do
                visit secondUrl
                _ <-
                  runPageScript
                    "const link = document.querySelector('nav a'); link.focus(); const style = getComputedStyle(link); link.dataset.testFocusVisibleStyle = String(link.matches(':focus-visible') && style.outlineStyle !== 'none' && parseFloat(style.outlineWidth) > 0); true"
                assertAttribute (byRole Link `named` "Home") "data-test-focus-visible-style" (`shouldBe` Just "true")
                click (byRole Link `named` "Profile")
                assertAll
                  ((,,,) <$> currentUrl <*> textContent (byRole Heading) <*> textContent (byText "Sign in to view and manage your profile.") <*> browserMetrics)
                  ( \(url, heading, body, metrics) ->
                      (url `shouldBe` profileUrl)
                        :| [ heading `shouldBe` "Profile",
                             body `shouldBe` "Sign in to view and manage your profile.",
                             $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 1, hardNavigationCount = 0}|])
                           ]
                  )
                visitWithoutScripts profileUrl
                assertAll
                  ((,) <$> textContent (byRole Heading) <*> textContent (within (css "#app-main") (byText "Create account")))
                  (\(heading, createAccount) -> (heading `shouldBe` "Profile") :| [createAccount `shouldBe` "Create account"])
                visitWithoutScripts spanishProfileUrl
                assertAll
                  ((,) <$> textContent (byRole Heading) <*> textContent (byText "Inicia sesión para ver y administrar tu perfil."))
                  (\(heading, body) -> (heading `shouldBe` "Perfil") :| [body `shouldBe` "Inicia sesión para ver y administrar tu perfil."])
            )
            `shouldReturn` Right ()

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
          runBrowserScenario
            browser
            ( do
                visit secondUrl
                click languageTrigger
                assertAttribute (css "#language-dialog") "open" (`shouldBe` Just "")
                assertFocused englishChoice (`shouldBe` True)
                _ <-
                  runPageScript
                    "const dialog = document.querySelector('#language-dialog'); document.querySelector('nav a').focus(); dialog.dataset.testBackgroundContained = String(dialog.contains(document.activeElement)); true"
                assertAttribute (css "#language-dialog") "data-test-background-contained" (`shouldBe` Just "true")
                press englishChoice "Tab"
                assertFocused spanishChoice (`shouldBe` True)
                press spanishChoice "Tab"
                assertFocused closeControl (`shouldBe` True)
                press closeControl "Tab"
                assertFocused englishChoice (`shouldBe` True)
                press (css "#language-dialog") "Escape"
                assertFocused languageTrigger (`shouldBe` True)
                click languageTrigger
                click spanishChoice
                assertAll
                  ((,,,) <$> currentUrl <*> textContent (byRole Heading `named` "Elige un idioma") <*> textContent (byRole Status) <*> browserMetrics)
                  ( \(url, heading, announcement, metrics) ->
                      (url `shouldBe` spanishLanguageUrl)
                        :| [ heading `shouldBe` "Elige un idioma",
                             announcement `shouldBe` "web-api: Language",
                             $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 1, hardNavigationCount = 0}|])
                           ]
                  )
                assertAttribute (css "#language-dialog") "open" (`shouldBe` Nothing)
            )
            `shouldReturn` Right ()

    it "keeps language selection and dialog startup failure complete without enhanced behavior" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildApp appConfig) $ \server -> do
          let baseUrl = HarchWeb.localServerBaseUrl server
              secondUrl = baseUrl <> "/second"
              languageUrl = baseUrl <> "/language"
              spanishLanguageUrl = baseUrl <> "/es/language"
          runBrowserScenario
            browser
            ( do
                blockRequestsMatching "**/assets/dialog.js"
                visit secondUrl
                click (byRole Link `named` "Language")
                failBlockedRequestsMatching "**/assets/dialog.js"
                assertAll
                  ((,) <$> currentUrl <*> browserMetrics)
                  ( \(url, metrics) ->
                      (url `shouldBe` languageUrl)
                        :| [$([|metrics|] `shouldMatch` [p|BrowserMetrics {hardNavigationCount = 1}|])]
                  )
                visitWithoutScripts secondUrl
                press (byRole Link `named` "Language") "Enter"
                assertAll
                  ((,) <$> currentUrl <*> textContent (byRole Heading))
                  (\(url, heading) -> (url `shouldBe` languageUrl) :| [heading `shouldBe` "Choose a language"])
                press (byRole Link `named` "Spanish") "Enter"
                assertAll
                  ((,) <$> currentUrl <*> textContent (byRole Heading))
                  (\(url, heading) -> (url `shouldBe` spanishLanguageUrl) :| [heading `shouldBe` "Elige un idioma"])
            )
            `shouldReturn` Right ()

    it "keeps the Help FAB usable, unobstructive, and absent at its destination" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildApp appConfig) $ \server -> do
          let baseUrl = HarchWeb.localServerBaseUrl server
              secondUrl = baseUrl <> "/second"
              helpUrl = baseUrl <> "/help"
              helpFab = byRole Link `named` "Help and support"
          runBrowserScenario
            browser
            ( do
                setViewportSize 320 480
                visit secondUrl
                _ <-
                  runPageScript
                    "document.documentElement.style.zoom = '2'; const fab = document.querySelector('[data-help-fab]'); fab.focus(); const box = fab.getBoundingClientRect(); const overlaps = [...document.querySelectorAll('#app-main a, #app-main button, #app-main input, #app-main select')].filter((control) => control !== fab && !control.closest('dialog')).some((control) => { const other = control.getBoundingClientRect(); return box.left < other.right && box.right > other.left && box.top < other.bottom && box.bottom > other.top; }); fab.dataset.testGeometry = String(box.width >= 44 && box.height >= 44 && box.right <= window.innerWidth && box.bottom <= window.innerHeight && !overlaps && getComputedStyle(fab).outlineStyle !== 'none'); true"
                assertAttribute helpFab "data-test-geometry" (`shouldBe` Just "true")
                press helpFab "Enter"
                assertAll
                  ((,,) <$> currentUrl <*> textContent (byRole Heading `named` "Help and support") <*> browserMetrics)
                  ( \(url, heading, metrics) ->
                      (url `shouldBe` helpUrl)
                        :| [ heading `shouldBe` "Help and support",
                             $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 1, hardNavigationCount = 0}|])
                           ]
                  )
                _ <- runPageScript "document.body.dataset.testNoHelpFab = String(!document.querySelector('[data-help-fab]')); true"
                assertAttribute (css "body") "data-test-no-help-fab" (`shouldBe` Just "true")
                visitWithoutScripts secondUrl
                press helpFab "Enter"
                assertAll
                  ((,) <$> currentUrl <*> textContent (byRole Heading))
                  (\(url, heading) -> (url `shouldBe` helpUrl) :| [heading `shouldBe` "Help and support"])
            )
            `shouldReturn` Right ()

    it "focuses and announces one lifecycle for keyboard navigation, history, and final redirected URLs" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildApp appConfig) $ \server -> do
          let baseUrl = HarchWeb.localServerBaseUrl server
              secondUrl = baseUrl <> "/second"
              spacesUrl = baseUrl <> "/spaces"
              mainContent = css "#app-main"
              routeStatus = byRole Status
          runBrowserScenario
            browser
            ( do
                setViewportSize 320 480
                visit secondUrl
                assertText routeStatus (`shouldBe` "")
                _ <-
                  runPageScript
                    "window.__ahi8HistoryLength = history.length; const status = document.querySelector('[data-navigation-route-status]'); let count = 0; status.dataset.testMutationCount = '0'; new MutationObserver((records) => { count += records.filter((record) => record.type === 'childList' || record.type === 'characterData').length; status.dataset.testMutationCount = String(count); }).observe(status, { childList: true, characterData: true, subtree: true }); document.documentElement.style.zoom = '2'; true"
                press (byRole Link `named` "Spaces") "Enter"
                assertAll
                  ((,) <$> currentUrl <*> isFocused mainContent)
                  (\(url, mainFocused) -> (url `shouldBe` spacesUrl) :| [mainFocused `shouldBe` True])
                _ <-
                  runPageScript
                    "const main = document.querySelector('#app-main'); const box = main.getBoundingClientRect(); const sampleX = Math.min(window.innerWidth - 1, Math.max(0, box.left + 1)); const sampleY = Math.min(window.innerHeight - 1, Math.max(0, box.top + 1)); const topElement = document.elementFromPoint(sampleX, sampleY); const style = getComputedStyle(main); main.dataset.testFocusUnobscured = String(document.activeElement === main && box.top >= 0 && box.top < window.innerHeight && (topElement === main || main.contains(topElement)) && style.outlineStyle !== 'none' && parseFloat(style.outlineWidth) > 0); true"
                assertAll
                  ( (,,,,,,)
                      <$> currentUrl
                      <*> textContent (css "title")
                      <*> textContent (byRole Heading)
                      <*> textContent routeStatus
                      <*> isFocused mainContent
                      <*> attributeValue routeStatus "data-test-mutation-count"
                      <*> attributeValue mainContent "data-test-focus-unobscured"
                  )
                  ( \(url, title, heading, announcement, mainFocused, mutationCount, unobscured) ->
                      (url `shouldBe` spacesUrl)
                        :| [ title `shouldBe` "web-api: Spaces",
                             heading `shouldBe` "Site under construction",
                             announcement `shouldBe` "web-api: Spaces",
                             mainFocused `shouldBe` True,
                             mutationCount `shouldBe` Just "1",
                             unobscured `shouldBe` Just "true"
                           ]
                  )
                historyBack
                assertAll
                  ((,,,) <$> currentUrl <*> textContent (css "title") <*> textContent routeStatus <*> attributeValue routeStatus "data-test-mutation-count")
                  ( \(url, title, announcement, mutationCount) ->
                      (url `shouldBe` secondUrl)
                        :| [ title `shouldBe` "web-api: Second",
                             announcement `shouldBe` "web-api: Second",
                             mutationCount `shouldBe` Just "2"
                           ]
                  )
                assertFocused mainContent (`shouldBe` True)
                historyForward
                assertAll
                  ((,,,) <$> currentUrl <*> textContent routeStatus <*> attributeValue routeStatus "data-test-mutation-count" <*> attributeValue (byRole Link `named` "Spaces") "aria-current")
                  ( \(url, announcement, mutationCount, activeRoute) ->
                      (url `shouldBe` spacesUrl)
                        :| [ announcement `shouldBe` "web-api: Spaces",
                             mutationCount `shouldBe` Just "3",
                             activeRoute `shouldBe` Just "page"
                           ]
                  )
                _ <- runPageScript "document.querySelector('#app-main').dataset.testHistoryStable = String(history.length === window.__ahi8HistoryLength + 1); true"
                assertAttribute mainContent "data-test-history-stable" (`shouldBe` Just "true")
                visit secondUrl
                press (byRole Link `named` "Home") "Enter"
                assertAll
                  ((,,,,) <$> currentUrl <*> textContent (css "title") <*> textContent routeStatus <*> isFocused mainContent <*> browserMetrics)
                  ( \(url, title, announcement, mainFocused, metrics) ->
                      (url `shouldBe` spacesUrl)
                        :| [ title `shouldBe` "web-api: Spaces",
                             announcement `shouldBe` "web-api: Spaces",
                             mainFocused `shouldBe` True,
                             $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 1, hardNavigationCount = 0}|])
                           ]
                  )
            )
            `shouldReturn` Right ()

    it "keeps only the newest overlapping enhanced navigation lifecycle" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildApp appConfig) $ \server -> do
          let baseUrl = HarchWeb.localServerBaseUrl server
              spacesUrl = baseUrl <> "/spaces"
              profileUrl = baseUrl <> "/profile"
              routeStatus = byRole Status
          runBrowserScenario
            browser
            ( do
                visit spacesUrl
                blockRequestsMatching "**/second"
                _ <-
                  runPageScript
                    "const status = document.querySelector('[data-navigation-route-status]'); let count = 0; status.dataset.testMutationCount = '0'; new MutationObserver((records) => { count += records.filter((record) => record.type === 'childList' || record.type === 'characterData').length; status.dataset.testMutationCount = String(count); }).observe(status, { childList: true, characterData: true, subtree: true }); true"
                press (byRole Link `named` "Second") "Enter"
                press (byRole Link `named` "Profile") "Enter"
                releaseRequestsMatching "**/second"
                assertAll
                  ((,,,,) <$> currentUrl <*> textContent (byRole Heading) <*> textContent routeStatus <*> attributeValue routeStatus "data-test-mutation-count" <*> browserMetrics)
                  ( \(url, heading, announcement, mutationCount, metrics) ->
                      (url `shouldBe` profileUrl)
                        :| [ heading `shouldBe` "Profile",
                             announcement `shouldBe` "web-api: Profile",
                             mutationCount `shouldBe` Just "1",
                             $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 2, hardNavigationCount = 0}|])
                           ]
                  )
                assertFocused (css "#app-main") (`shouldBe` True)
            )
            `shouldReturn` Right ()

    it "falls back natively for failed, incompatible, and unsafe final responses without announcing success" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildApp appConfig) $ \server -> do
          let baseUrl = HarchWeb.localServerBaseUrl server
              secondUrl = baseUrl <> "/second"
              spacesUrl = baseUrl <> "/spaces"
              routeStatus = byRole Status
              assertNativeFallback =
                assertAll
                  ((,,,) <$> currentUrl <*> textContent (byRole Heading) <*> textContent routeStatus <*> browserMetrics)
                  ( \(url, heading, announcement, metrics) ->
                      (url `shouldBe` secondUrl)
                        :| [ heading `shouldBe` "Second",
                             announcement `shouldBe` "",
                             $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 1, hardNavigationCount = 1}|])
                           ]
                  )
          runBrowserScenario
            browser
            ( do
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
            )
            `shouldReturn` Right ()

    it "keeps delayed-runtime and scripts-disabled keyboard navigation native, including the skip link" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildApp appConfig) $ \server -> do
          let baseUrl = HarchWeb.localServerBaseUrl server
              secondUrl = baseUrl <> "/second"
              spacesUrl = baseUrl <> "/spaces"
              mainContent = css "#app-main"
          runBrowserScenario
            browser
            ( do
                blockRequestsMatching "**/assets/navigation.js"
                visit secondUrl
                press (byRole Link `named` "Spaces") "Enter"
                assertAll
                  ((,,) <$> currentUrl <*> textContent (byRole Status) <*> browserMetrics)
                  ( \(url, announcement, metrics) ->
                      (url `shouldBe` spacesUrl)
                        :| [ announcement `shouldBe` "",
                             $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 0, hardNavigationCount = 1}|])
                           ]
                  )
                releaseRequestsMatching "**/assets/navigation.js"
                visitWithoutScripts secondUrl
                press (css "body") "Tab"
                assertFocused (byRole Link `named` "Skip to main content") (`shouldBe` True)
                press (byRole Link `named` "Skip to main content") "Enter"
                assertFocused mainContent (`shouldBe` True)
                press (byRole Link `named` "Spaces") "Enter"
                assertAll
                  ((,,) <$> currentUrl <*> textContent (byRole Heading) <*> browserMetrics)
                  ( \(url, heading, metrics) ->
                      (url `shouldBe` spacesUrl)
                        :| [ heading `shouldBe` "Site under construction",
                             $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 0, hardNavigationCount = 1}|])
                           ]
                  )
            )
            `shouldReturn` Right ()

    it "preserves Spanish registration input until the delayed runtime sends its localized patch" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildAppWithDatabaseAndAccountWorkflow appConfig defaultPageRepository localizedRegistrationWorkflow) $ \server -> do
          let registrationUrl = HarchWeb.localServerBaseUrl server <> "/es/register"
              usernameField = byLabel "Nombre de usuario"
              emailField = byLabel "Direccion de correo"
              passwordField = byLabel "Contrasena"
          runBrowserScenario
            browser
            ( do
                blockRequestsMatching "**/assets/navigation.js"
                visit registrationUrl
                assertText (byRole Heading) (`shouldBe` "Crea tu cuenta")
                fill usernameField "person_01"
                _ <-
                  runPageScript
                    "const field = document.querySelector('#registration-email'); field.value = 'person@example.test'; field.dispatchEvent(new InputEvent('input', { bubbles: true, inputType: 'insertReplacementText', data: 'person@example.test' })); true"
                paste passwordField "correct horse battery staple"
                click (byRole Button `named` "Crear cuenta")
                assertAll
                  ( (,,,,)
                      <$> currentUrl
                      <*> inputValue usernameField
                      <*> inputValue emailField
                      <*> inputValue passwordField
                      <*> browserMetrics
                  )
                  ( \(url, username, email, password, metrics) ->
                      (url `shouldBe` registrationUrl)
                        :| [ username `shouldBe` "person_01",
                             email `shouldBe` "person@example.test",
                             password `shouldBe` "correct horse battery staple",
                             $( [|metrics|]
                                  `shouldMatch` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 0}|]
                              )
                           ]
                  )
                releaseRequestsMatching "**/assets/navigation.js"
                assertAll
                  ((,,) <$> browserMetrics <*> textContent (byText "Si esa direccion puede registrarse, revisa su bandeja de entrada para obtener un enlace de verificacion.") <*> inputValue passwordField)
                  ( \(metrics, message, password) ->
                      ($([|metrics|] `shouldMatch` [p|BrowserMetrics {mutationRequestCount = 1}|]))
                        :| [ message `shouldBe` "Si esa direccion puede registrarse, revisa su bandeja de entrada para obtener un enlace de verificacion.",
                             password `shouldBe` ""
                           ]
                  )
            )
            `shouldReturn` Right ()

    it "accepts pasted and autofill-compatible login values, clears secrets, and keeps focus visible when narrow and zoomed" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildApp appConfig) $ \server -> do
          let loginUrl = HarchWeb.localServerBaseUrl server <> "/login"
              identifierField = byLabel "Email address or username"
              passwordField = byLabel "Password"
              proofField = byLabel "Verification method"
              authenticatorField = byLabel "Authenticator code"
              recoveryField = byLabel "Recovery code"
          runBrowserScenario
            browser
            ( do
                setViewportSize 320 480
                visit loginUrl
                _ <-
                  runPageScript
                    "const field = document.querySelector('#login-identifier'); field.value = 'not an identifier!'; field.dispatchEvent(new InputEvent('input', { bubbles: true, inputType: 'insertReplacementText', data: 'not an identifier!' })); true"
                paste passwordField "short"
                paste authenticatorField "1"
                press identifierField "Tab"
                assertFocused passwordField (`shouldBe` True)
                press passwordField "Tab"
                assertFocused proofField (`shouldBe` True)
                press proofField "Tab"
                assertFocused authenticatorField (`shouldBe` True)
                click (byRole Button `named` "Sign in")
                assertFocused (css "#login-error-summary") (`shouldBe` True)
                assertAll
                  ((,,,) <$> inputValue identifierField <*> inputValue passwordField <*> inputValue authenticatorField <*> browserMetrics)
                  ( \(identifier, password, authenticator, metrics) ->
                      (identifier `shouldBe` "not an identifier!")
                        :| [ password `shouldBe` "",
                             authenticator `shouldBe` "",
                             $([|metrics|] `shouldMatch` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 1}|])
                           ]
                  )
                _ <-
                  runPageScript
                    "const proof = document.querySelector('#login-proof'); proof.value = 'recovery'; proof.dispatchEvent(new Event('change', { bubbles: true })); const identifier = document.querySelector('#login-identifier'); identifier.value = 'person@example.test'; identifier.dispatchEvent(new InputEvent('input', { bubbles: true, inputType: 'insertReplacementText', data: 'person@example.test' })); document.documentElement.style.zoom = '2'; true"
                paste passwordField "correct horse battery staple"
                paste recoveryField "pasted-recovery"
                _ <-
                  runPageScript
                    "const field = document.querySelector('#login-recovery-code'); field.focus(); field.scrollIntoView({ block: 'nearest' }); const box = field.getBoundingClientRect(); field.dataset.testFocusVisible = String(field === document.activeElement && box.top >= 0 && box.bottom <= window.innerHeight); field.dataset.testFocusVisible"
                assertAttribute recoveryField "data-test-focus-visible" (`shouldBe` Just "true")
                click (byRole Button `named` "Sign in")
                assertAll
                  ((,,,) <$> inputValue identifierField <*> inputValue passwordField <*> inputValue authenticatorField <*> inputValue recoveryField)
                  ( \(identifier, password, authenticator, recovery) ->
                      (identifier `shouldBe` "person@example.test")
                        :| [password `shouldBe` "", authenticator `shouldBe` "", recovery `shouldBe` ""]
                  )
            )
            `shouldReturn` Right ()

    it "keeps client-only authentication forms semantically complete without scripts" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildApp appConfig) $ \server -> do
          let baseUrl = HarchWeb.localServerBaseUrl server
          runBrowserScenario
            browser
            ( do
                visitWithoutScripts (baseUrl <> "/register")
                assertAttribute (css "#registration-region form") "method" (`shouldBe` Just "dialog")
                assertValue (byLabel "Password") (`shouldBe` "")
                press (byLabel "Username") "Tab"
                assertFocused (byLabel "Email address") (`shouldBe` True)
                press (byLabel "Email address") "Tab"
                assertFocused (byLabel "Display name (optional)") (`shouldBe` True)
                press (byLabel "Display name (optional)") "Tab"
                assertFocused (byLabel "Password") (`shouldBe` True)
                visitWithoutScripts (baseUrl <> "/login")
                assertAttribute (css "#login-region form") "method" (`shouldBe` Just "dialog")
                assertText (byText "Choose Authenticator code above, then enter or paste its six-digit code.") (`shouldBe` "Choose Authenticator code above, then enter or paste its six-digit code.")
                visitWithoutScripts (baseUrl <> "/verify?token=delivered-token")
                assertAttribute (css "#verification-region form") "method" (`shouldBe` Just "dialog")
                assertValue (byLabel "Verification token") (`shouldBe` "delivered-token")
                press (byLabel "Verification token") "Tab"
                assertFocused (byRole Button `named` "Verify email") (`shouldBe` True)
                visitWithoutScripts (baseUrl <> "/mfa")
                assertAttribute (css "#mfa-enrollment-region form") "method" (`shouldBe` Just "dialog")
                assertText (byRole Button `named` "Start authenticator enrollment") (`shouldBe` "Start authenticator enrollment")
            )
            `shouldReturn` Right ()

    it "keeps MFA confirmation keyboard- and paste-usable after its server patch" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildAppWithDatabaseAndAccountWorkflow appConfig defaultPageRepository mfaEnrollmentBrowserWorkflow) $ \server -> do
          let mfaUrl = HarchWeb.localServerBaseUrl server <> "/mfa"
              codeField = byLabel "Authenticator code"
          runBrowserScenario
            browser
            ( do
                setCookie mfaUrl mfaEnrollmentCookieName sessionToken
                visit mfaUrl
                click (byRole Button `named` "Start authenticator enrollment")
                assertFocused codeField (`shouldBe` True)
                press codeField "Tab"
                assertFocused (byRole Button `named` "Confirm authenticator") (`shouldBe` True)
                paste codeField "123"
                click (byRole Button `named` "Confirm authenticator")
                assertAll
                  ((,) <$> inputValue codeField <*> browserMetrics)
                  ( \(code, metrics) ->
                      (code `shouldBe` "")
                        :| [$([|metrics|] `shouldMatch` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 2}|])]
                  )
                assertFocused codeField (`shouldBe` True)
            )
            `shouldReturn` Right ()

    it "focuses a multi-error registration summary and follows its field link by keyboard" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildAppWithDatabaseAndAccountWorkflow appConfig defaultPageRepository localizedRegistrationWorkflow) $ \server -> do
          let registrationUrl = HarchWeb.localServerBaseUrl server <> "/register"
              oversizedEmail = Text.replicate 245 "a" <> "@example.test"
              usernameField = byLabel "Username"
              emailField = byLabel "Email address"
              passwordField = byLabel "Password"
              usernameErrorLink = byRole Link `named` "Use a username with 3 to 20 letters, numbers, underscores, or hyphens."
          runBrowserScenario
            browser
            ( do
                visit registrationUrl
                fill usernameField "no!"
                fill emailField oversizedEmail
                fill passwordField "correct horse battery staple"
                click (byRole Button `named` "Create account")
                assertFocused (css "#registration-error-summary") (`shouldBe` True)
                assertText (byRole Heading `named` "Fix the following problems") (`shouldBe` "Fix the following problems")
                assertAll
                  ((,,,) <$> inputValue usernameField <*> inputValue emailField <*> inputValue passwordField <*> attributeValue passwordField "aria-describedby")
                  ( \(username, email, password, describedBy) ->
                      (username `shouldBe` "no!")
                        :| [ email `shouldBe` oversizedEmail,
                             password `shouldBe` "",
                             describedBy `shouldBe` Just "registration-password-hint"
                           ]
                  )
                press usernameErrorLink "Enter"
                assertFocused usernameField (`shouldBe` True)
            )
            `shouldReturn` Right ()

    it "resends a pending-profile verification email through the immediate capture path" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildAppWithDatabaseAndAccountWorkflow appConfig defaultPageRepository pendingProfileWorkflow) $ \server -> do
          let profileUrl = HarchWeb.localServerBaseUrl server <> "/profile"
          runBrowserScenario
            browser
            ( do
                setCookie profileUrl sessionCookieName sessionToken
                visit profileUrl
                assertAll
                  ((,) <$> textContent (byRole Heading) <*> textContent (byText "person@example.test"))
                  (\(heading, email) -> (heading `shouldBe` "Profile") :| [email `shouldBe` "person@example.test"])
                click (byRole Button `named` "Resend verification email")
                assertAll
                  ((,) <$> textContent (byText "Check your inbox for a verification link.") <*> browserMetrics)
                  ( \(message, metrics) ->
                      (message `shouldBe` "Check your inbox for a verification link.")
                        :| [$([|metrics|] `shouldMatch` [p|BrowserMetrics {mutationRequestCount = 1}|])]
                  )
            )
            `shouldReturn` Right ()

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
      Session.sessionCsrfToken = requiredCsrfToken "abcdefghijklmnopqrstuvwxyz0123456789-_",
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
      Session.sessionCsrfToken = requiredCsrfToken "abcdefghijklmnopqrstuvwxyz0123456789-_",
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

requiredCsrfToken :: Text -> Session.CsrfToken
requiredCsrfToken value =
  case Session.mkCsrfToken value of
    Just csrfToken -> csrfToken
    Nothing -> error "expected a valid CSRF token"
