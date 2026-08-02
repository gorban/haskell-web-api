{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# E2E_SPEC #-}

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import HarchWeb qualified
import HarchWeb.Account qualified as Account
import HarchWeb.Email qualified as Email
import HarchWeb.Password qualified as Password
import HarchWeb.Session qualified as Session
import System.IO.Temp (withSystemTempDirectory)
import WebApi.Account (AccountProfile (..), AccountProfileStore (..), AccountStore (..))
import WebApi.App (buildApp, buildAppWithDatabaseAndAccountWorkflow, unavailableAccountWorkflow)
import WebApi.AppEffect (AccountWorkflow (..))
import WebApi.Config (AppConfig (..), StaticAssetRoot (..), StaticAssetsConfig (..), defaultAppConfig, defaultStaticAssetContentTypes)
import WebApi.Database (defaultPageRepository)
import WebApi.Session (AccountSessionStore (..))

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

    it "keeps Spanish registration SSR and its immediate failure patch localized" $
      withBrowserApp $ \browser appConfig ->
        HarchWeb.withLocalTestServer (buildAppWithDatabaseAndAccountWorkflow appConfig defaultPageRepository localizedRegistrationWorkflow) $ \server -> do
          let registrationUrl = HarchWeb.localServerBaseUrl server <> "/es/register"
          runBrowserScenario
            browser
            ( do
                visit registrationUrl
                assertText (byRole Heading) (`shouldBe` "Crea tu cuenta")
                fill (byLabel "Nombre de usuario") "person_01"
                fill (byLabel "Direccion de correo") "person@example.test"
                fill (byLabel "Contrasena") "correct horse battery staple"
                click (byRole Button `named` "Crear cuenta")
                assertAll
                  ((,) <$> browserMetrics <*> textContent (byText "Si esa direccion puede registrarse, revisa su bandeja de entrada para obtener un enlace de verificacion."))
                  ( \(metrics, message) ->
                      ($([|metrics|] `shouldMatch` [p|BrowserMetrics {mutationRequestCount = 1}|]))
                        :| [message `shouldBe` "Si esa direccion puede registrarse, revisa su bandeja de entrada para obtener un enlace de verificacion."]
                  )
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

pendingProfileWorkflow :: AccountWorkflow
pendingProfileWorkflow =
  unavailableAccountWorkflow
    { accountWorkflowStore =
        AccountStore
          { createPendingAccount = \_ -> error "unexpected account creation",
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
            invalidateAccountSession = \_ -> error "unexpected session invalidation"
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
          { createPendingAccount = \_ -> pure (Right False)
          },
      accountWorkflowPasswordHasher = \_ _ -> pure (Just (Password.PasswordHash "test-password-hash"))
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
