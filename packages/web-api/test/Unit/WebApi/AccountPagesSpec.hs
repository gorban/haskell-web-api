{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Exception (ErrorCall (..), IOException, displayException, evaluate, try)
import Control.Monad (forM_)
import Data.ByteString qualified as ByteString
import Data.Foldable (toList)
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, isNothing)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb qualified
import HarchWeb.Account (AccountId, emailVerificationTokenText, mkAccountId, storedVerificationTokenDigest)
import HarchWeb.Account qualified as Account
import HarchWeb.Action qualified as Action
import HarchWeb.Email (EmailAddress, EmailDelivery (..), mkEmailAddress)
import HarchWeb.Email qualified as Email
import HarchWeb.LoginProtection qualified as LoginProtection
import HarchWeb.Observability qualified as Observability
import HarchWeb.Password qualified as Password
import HarchWeb.RecoveryCode qualified as RecoveryCode
import HarchWeb.Secret qualified as Secret
import HarchWeb.Session (OpaqueSession (..), SessionId, mkCsrfToken, mkSessionId)
import HarchWeb.Session qualified as Session
import HarchWeb.Time (UnixTimeNanoseconds, unixTimeSecondsFromNanoseconds)
import HarchWeb.Totp qualified as Totp
import HarchWeb.Username qualified as Username
import Network.HTTP.Types qualified as Http
import Unit.WebApi.TestSupport hiding (accountId, databaseConfig, emailAddress, opaqueSession, sessionIdValue, testSessionId)
import WebApi.Account (AccountProfile (..), AccountProfileStore (..), AccountStore (..), AccountStoreError (..), CreatePendingAccountOutcome (..), PendingAccount (..))
import WebApi.AccountPages (AccountAction, AccountActionTarget (..), AccountWorkflow (..), LoginForm (..), MfaEnrollmentForm (..), PendingProfileForm (..), RegistrationForm (..), VerificationForm (..), accountActions, authorizeAccountActionCsrf, emptyRegistrationForm, handleAccountAction, mfaEnrollmentFailureDiagnostics, pageCsrfTokenForAccountPage, renderLoginPage, renderLoginRegion, renderLogoutPage, renderLogoutRegion, renderMfaEnrollmentPage, renderMfaEnrollmentRegion, renderPendingProfileRegion, renderRegistrationPage, renderRegistrationRegion, renderVerificationPage, renderVerificationRegion)
import WebApi.AccountPages.Actions.Contract (AccountAction (LogoutAccount), buildActionCodecOrDie)
import WebApi.App (buildRuntimeAppWithDatabaseBuilder, unavailableAccountWorkflow)
import WebApi.App.Enhancements (pageEnhancementHooks)
import WebApi.AppEffect qualified as AppEffect
import WebApi.Config (AppEnvironmentConfig (..), defaultAppConfig, defaultAppEnvironmentConfig)
import WebApi.Database (defaultPageRepository)
import WebApi.Login (AccountCredential (..), AccountCredentialStore (..), AccountCredentialStoreError (..), LoginAttemptAdmission (..), LoginAttemptReservation (..), LoginAttemptStore (..), LoginAttemptStoreError (..), LoginIdentifier (..), PasswordLoginResult (..), beginPasswordLoginWithIdentifier)
import WebApi.Mfa (MfaStore (..), MfaStoreError (..), StoredTotpEnrollment (..))
import WebApi.MfaEnrollment (MfaEnrollmentError (..))
import WebApi.Page (AppPageModel (..), CallToAction (..), ProfilePageModel (..), SignedOutProfilePageDetails (..), buildPageModelFromRouteData, renderPageFromRouteData)
import WebApi.Route (AppLocale (..), AppRequestContext (..), AppRoute (..), defaultRequestContext, renderRoutePath)
import WebApi.RouteData (RouteDataResult (..), RouteDataSelection (..), selectRouteData, selectRouteDataSelectionWithDatabase)
import WebApi.Session (AccountSessionStore (..), AccountSessionStoreError (..), MfaEnrollmentSessionStore (..), MfaEnrollmentSessionStoreError (..))

existingSpec :: SpecWith ()
existingSpec = do
  describe "WebApi.AccountPages" $ do
    it "binds protected actions to the matching live account or MFA session token" $ do
      let activeWorkflow =
            unavailableAccountWorkflow
              { accountWorkflowClock = pure 150,
                accountWorkflowSessionStore = existingSessionStore (Right (Just activeSession)),
                accountWorkflowMfaEnrollmentSessionStore = enrollmentSessionStoreFor existingAccountId
              }
          expiredWorkflow =
            activeWorkflow
              { accountWorkflowSessionStore = existingSessionStore (Right (Just (opaqueSession 150)))
              }
          revokedWorkflow =
            activeWorkflow
              { accountWorkflowSessionStore = existingSessionStore (Right Nothing)
              }
          unavailableWorkflow =
            activeWorkflow
              { accountWorkflowSessionStore = existingSessionStore (Left AccountSessionStoreUnavailable)
              }
          expiredMfaWorkflow =
            activeWorkflow
              { accountWorkflowMfaEnrollmentSessionStore =
                  MfaEnrollmentSessionStore
                    { saveMfaEnrollmentSession = \_ -> pure (error "unexpected MFA-enrollment session save"),
                      loadMfaEnrollmentSession = \_ ->
                        pure
                          ( Right
                              ( Just
                                  Session.OpaqueSession
                                    { Session.sessionId = enrollmentSessionIdValue,
                                      Session.sessionPrincipal = existingAccountId,
                                      Session.sessionCsrfToken = enrollmentCsrfTokenValue,
                                      Session.sessionIssuedAtNanoseconds = 0,
                                      Session.sessionExpiresAtNanoseconds = 150
                                    }
                              )
                          ),
                      invalidateMfaEnrollmentSession = \_ _ -> pure (error "unexpected MFA-enrollment session invalidation")
                    }
              }
          unavailableMfaWorkflow =
            activeWorkflow
              { accountWorkflowMfaEnrollmentSessionStore =
                  MfaEnrollmentSessionStore
                    { saveMfaEnrollmentSession = \_ -> pure (error "unexpected MFA-enrollment session save"),
                      loadMfaEnrollmentSession = \_ -> pure (Left MfaEnrollmentSessionStoreUnavailable),
                      invalidateMfaEnrollmentSession = \_ _ -> pure (error "unexpected MFA-enrollment session invalidation")
                    }
              }
          profileActionRequest = typedAccountActionRequest "POST" "/profile" [("intent", "resend-verification")] sessionRequestContext
          logoutRequest = typedAccountActionRequest "POST" "/logout" [] sessionRequestContext
          mfaRequest =
            typedAccountActionRequest
              "POST"
              "/mfa"
              [("intent", "start")]
              (defaultRequestContext {WebApi.Route.requestMfaEnrollmentSessionId = Just enrollmentSessionIdValue})
          anonymousRequest = typedAccountActionRequest "POST" "/register" [] defaultRequestContext
          verificationRequest = typedAccountActionRequest "POST" "/verify" [] defaultRequestContext
          loginRequest = typedAccountActionRequest "POST" "/login" [] defaultRequestContext
          anonymousProfileRequest = typedAccountActionRequest "POST" "/profile" [("intent", "resend-verification")] defaultRequestContext
          anonymousMfaRequest = typedAccountActionRequest "POST" "/mfa" [("intent", "start")] defaultRequestContext
          sessionToken = Session.sessionCsrfToken activeSession
          wrongToken = fromMaybe (error "expected a valid wrong CSRF token") (Session.mkCsrfToken "0123456789abcdefghijklmnopqrstuvwxyz-_ABCDE")
          accountPage route requestContext =
            HarchWeb.Page
              { HarchWeb.pageTitle = "",
                HarchWeb.pageRoute = route,
                HarchWeb.pageContext = requestContext,
                HarchWeb.pageBody = HarchWeb.text "",
                HarchWeb.pageBootstrapHooks = []
              }
      authorizeAccountActionCsrf activeWorkflow profileActionRequest sessionToken `shouldReturn` True
      authorizeAccountActionCsrf activeWorkflow profileActionRequest wrongToken `shouldReturn` False
      authorizeAccountActionCsrf expiredWorkflow profileActionRequest sessionToken `shouldReturn` False
      authorizeAccountActionCsrf revokedWorkflow profileActionRequest sessionToken `shouldReturn` False
      authorizeAccountActionCsrf unavailableWorkflow profileActionRequest sessionToken `shouldReturn` False
      authorizeAccountActionCsrf activeWorkflow anonymousProfileRequest sessionToken `shouldReturn` False
      authorizeAccountActionCsrf activeWorkflow logoutRequest sessionToken `shouldReturn` True
      authorizeAccountActionCsrf activeWorkflow mfaRequest enrollmentCsrfTokenValue `shouldReturn` True
      authorizeAccountActionCsrf activeWorkflow mfaRequest wrongToken `shouldReturn` False
      authorizeAccountActionCsrf expiredMfaWorkflow mfaRequest enrollmentCsrfTokenValue `shouldReturn` False
      authorizeAccountActionCsrf unavailableMfaWorkflow mfaRequest enrollmentCsrfTokenValue `shouldReturn` False
      authorizeAccountActionCsrf activeWorkflow anonymousMfaRequest enrollmentCsrfTokenValue `shouldReturn` False
      authorizeAccountActionCsrf activeWorkflow anonymousRequest wrongToken `shouldReturn` True
      authorizeAccountActionCsrf activeWorkflow verificationRequest wrongToken `shouldReturn` True
      authorizeAccountActionCsrf activeWorkflow loginRequest wrongToken `shouldReturn` True
      pageCsrfTokenForAccountPage activeWorkflow (accountPage ProfileRoute sessionRequestContext) `shouldReturn` sessionToken
      pageCsrfTokenForAccountPage activeWorkflow (accountPage LogoutRoute sessionRequestContext) `shouldReturn` sessionToken
      pageCsrfTokenForAccountPage activeWorkflow (accountPage MfaEnrollmentRoute (defaultRequestContext {WebApi.Route.requestMfaEnrollmentSessionId = Just enrollmentSessionIdValue})) `shouldReturn` enrollmentCsrfTokenValue

    it "resends pending-profile verification through a localized client-action patch" $ do
      let actionRequest requestContext fields =
            fromMaybe
              (error "expected a recognized profile action fixture")
              ( do
                  action <-
                    case Action.decodeAction
                      accountActions
                      HarchWeb.ClientActionPayload
                        { HarchWeb.clientActionMethod = "POST",
                          HarchWeb.clientActionPath = profileActionPath requestContext,
                          HarchWeb.clientActionFields = fields,
                          HarchWeb.clientActionCsrfToken = Nothing,
                          HarchWeb.clientActionIdempotencyKey = Nothing,
                          HarchWeb.clientActionPayloadContext = requestContext
                        } of
                      HarchWeb.DecodedClientAction decodedAction -> Just decodedAction
                      _ -> Nothing
                  pure
                    HarchWeb.ClientActionRequest
                      { HarchWeb.clientAction = action,
                        HarchWeb.clientActionRequestIdempotencyKey = Nothing,
                        HarchWeb.clientActionContext = requestContext
                      }
              )
          profileActionPath requestContext =
            case WebApi.Route.requestLocale requestContext of
              WebApi.Route.English -> "/profile"
              WebApi.Route.Spanish -> "/es/profile"
          store replacementResult =
            AccountStore
              { createPendingAccount = \_ -> error "unexpected account creation",
                replaceEmailVerification = \verification -> storedVerificationTokenDigest verification `seq` pure replacementResult,
                findEmailVerification = \_ -> error "unexpected verification lookup",
                consumeEmailVerification = \_ _ -> error "unexpected verification consumption"
              }
          workflow replacementResult emailDelivery sessionResult loadedProfile now =
            unavailableAccountWorkflow
              { accountWorkflowStore = store replacementResult,
                accountWorkflowEmailDelivery = emailDelivery,
                accountWorkflowClock = pure now,
                accountWorkflowSessionStore = existingSessionStore sessionResult,
                accountWorkflowProfileStore = profileStore loadedProfile,
                accountWorkflowVerificationUrl = \requestContext token ->
                  case WebApi.Route.requestLocale requestContext of
                    WebApi.Route.English -> "https://account.example.test/verify?token=" <> emailVerificationTokenText token
                    WebApi.Route.Spanish -> "https://account.example.test/es/verify?token=" <> emailVerificationTokenText token
              }
          delivery = EmailDelivery (\message -> Text.length (Email.emailMessageBody message) `seq` pure ())
          pendingWorkflow = workflow (Right True) delivery (Right (Just activeSession)) (Right (Just pendingProfile)) 150
          expect workflowValue request expectedStatus expectedText = do
            actionResult <- handleAccountAction workflowValue request
            case actionResult of
              Nothing -> expectationFailure "expected a profile client-action response"
              Just response ->
                expectAll
                  ( (Http.statusCode (HarchWeb.clientActionStatus response) `shouldBe` expectedStatus)
                      :| [ HarchWeb.clientActionPatches response `shouldSatisfy` any ((== "profile-region") . HarchWeb.regionPatchId),
                           HarchWeb.clientActionPatches response `shouldSatisfy` any (Text.isInfixOf expectedText . HarchWeb.regionPatchHtml),
                           length (show response) `shouldSatisfy` (> 0)
                         ]
                  )
      expect pendingWorkflow (actionRequest sessionRequestContext [("intent", "resend-verification")]) 202 "Check your inbox"
      expect pendingWorkflow (actionRequest spanishSessionRequestContext [("intent", "resend-verification")]) 202 "Revisa tu bandeja"
      expect pendingWorkflow (actionRequest sessionRequestContext []) 422 "Choose a profile action"
      expect pendingWorkflow (actionRequest defaultRequestContext [("intent", "resend-verification")]) 403 "Sign in before"
      expect pendingWorkflow (actionRequest spanishSessionRequestContext []) 422 "Elige una accion de perfil"
      expect pendingWorkflow (actionRequest (defaultRequestContext {WebApi.Route.requestLocale = WebApi.Route.Spanish, WebApi.Route.requestLocaleIsExplicit = True}) [("intent", "resend-verification")]) 403 "Inicia sesion antes"
      expect (workflow (Right True) delivery (Right (Just activeSession)) (Right (Just verifiedProfile)) 150) (actionRequest sessionRequestContext [("intent", "resend-verification")]) 409 "already verified"
      expect (workflow (Right True) delivery (Right (Just activeSession)) (Right (Just verifiedProfile)) 150) (actionRequest spanishSessionRequestContext [("intent", "resend-verification")]) 409 "Tu direccion de correo ya esta verificada"
      expect (workflow (Right False) delivery (Right (Just activeSession)) (Right (Just pendingProfile)) 150) (actionRequest sessionRequestContext [("intent", "resend-verification")]) 409 "profile state changed"
      expect (workflow (Right False) delivery (Right (Just activeSession)) (Right (Just pendingProfile)) 150) (actionRequest spanishSessionRequestContext [("intent", "resend-verification")]) 409 "El estado de tu perfil ha cambiado"
      expect (workflow (Left (AccountStoreUnavailable "database unavailable")) delivery (Right (Just activeSession)) (Right (Just pendingProfile)) 150) (actionRequest sessionRequestContext [("intent", "resend-verification")]) 503 "temporarily unavailable"
      expect (workflow (Left (AccountStoreUnavailable "database unavailable")) delivery (Right (Just activeSession)) (Right (Just pendingProfile)) 150) (actionRequest spanishSessionRequestContext [("intent", "resend-verification")]) 503 "Tu perfil no esta disponible"
      expect (workflow (Right True) (EmailDelivery (\_ -> ioError (userError "SMTP unavailable"))) (Right (Just activeSession)) (Right (Just pendingProfile)) 150) (actionRequest sessionRequestContext [("intent", "resend-verification")]) 502 "could not send"
      expect (workflow (Right True) (EmailDelivery (\_ -> ioError (userError "SMTP unavailable"))) (Right (Just activeSession)) (Right (Just pendingProfile)) 150) (actionRequest spanishSessionRequestContext [("intent", "resend-verification")]) 502 "No pudimos enviar"
      expect (workflow (Right True) delivery (Left AccountSessionStoreUnavailable) (Right (Just pendingProfile)) 150) (actionRequest sessionRequestContext [("intent", "resend-verification")]) 503 "temporarily unavailable"
      expect (workflow (Right True) delivery (Left AccountSessionStoreUnavailable) (Right (Just pendingProfile)) 150) (actionRequest spanishSessionRequestContext [("intent", "resend-verification")]) 503 "Tu perfil no esta disponible"
      expect (workflow (Right True) delivery (Right (Just activeSession)) (Left (AccountStoreUnavailable "database unavailable")) 150) (actionRequest sessionRequestContext [("intent", "resend-verification")]) 503 "temporarily unavailable"
      expect (workflow (Right True) delivery (Right (Just activeSession)) (Left (AccountStoreUnavailable "database unavailable")) 150) (actionRequest spanishSessionRequestContext [("intent", "resend-verification")]) 503 "Tu perfil no esta disponible"
      expect (workflow (Right True) delivery (Right (Just (opaqueSession maxBound))) (Right (Just pendingProfile)) (maxBound - 1)) (actionRequest sessionRequestContext [("intent", "resend-verification")]) 503 "temporarily unavailable"
      expect (workflow (Right True) delivery (Right (Just (opaqueSession maxBound))) (Right (Just pendingProfile)) (maxBound - 1)) (actionRequest spanishSessionRequestContext [("intent", "resend-verification")]) 503 "Tu perfil no esta disponible"

    it "keeps PendingProfileForm comparable and its rendered region free of a false error flag" $
      expectAll
        ( ( ( PendingProfileForm "person@example.test" Nothing False "Resend verification email"
                == PendingProfileForm "person@example.test" Nothing False "Resend verification email"
            )
              `shouldBe` True
          )
            :| [ ( PendingProfileForm "person@example.test" Nothing False "Resend verification email"
                     /= PendingProfileForm "person@example.test" (Just "Updated") False "Resend verification email"
                 )
                   `shouldBe` True,
                 ( PendingProfileForm "person@example.test" Nothing False "Resend verification email"
                     /= PendingProfileForm "person@example.test" Nothing True "Resend verification email"
                 )
                   `shouldBe` True,
                 (PendingProfileForm "person@example.test" Nothing False "Resend verification email" /= PendingProfileForm "person@example.test" Nothing False "Send again")
                   `shouldBe` True,
                 renderPendingProfileRegion defaultRequestContext UpdateProfileTarget (PendingProfileForm "person@example.test" (Just "Updated") False "Resend verification email")
                   `shouldSatisfy` (not . Text.isInfixOf "data-message-error=\"true\"")
               ]
        )

existingSessionStore :: Either AccountSessionStoreError (Maybe (OpaqueSession AccountId)) -> AccountSessionStore
existingSessionStore result =
  AccountSessionStore
    { saveAccountSession = \_ -> pure (Right True),
      loadAccountSession = \sessionIdValue -> sessionIdValue `seq` pure result,
      invalidateAccountSession = \_ _ -> pure (Right False)
    }

profileStore :: Either AccountStoreError (Maybe AccountProfile) -> AccountProfileStore
profileStore result = AccountProfileStore (\accountIdValue -> accountIdValue `seq` pure result)

sessionRequestContext :: AppRequestContext
sessionRequestContext = defaultRequestContext {WebApi.Route.requestSessionId = Just testSessionId}

spanishSessionRequestContext :: AppRequestContext
spanishSessionRequestContext = sessionRequestContext {WebApi.Route.requestLocale = WebApi.Route.Spanish, WebApi.Route.requestLocaleIsExplicit = True}

activeSession :: OpaqueSession AccountId
activeSession = opaqueSession 200

opaqueSession :: UnixTimeNanoseconds -> OpaqueSession AccountId
opaqueSession expiresAtNanoseconds =
  case mkCsrfToken "abcdefghijklmnopqrstuvwxyz0123456789-_" of
    Just csrfToken ->
      OpaqueSession
        { sessionId = testSessionId,
          sessionPrincipal = existingAccountId,
          sessionCsrfToken = csrfToken,
          sessionIssuedAtNanoseconds = 100,
          sessionExpiresAtNanoseconds = expiresAtNanoseconds
        }
    Nothing -> error "expected a valid CSRF token"

pendingProfile :: AccountProfile
pendingProfile = AccountProfile existingAccountId existingEmailAddress (Username.mkUsername "person_01") (Just "Person Example") False

verifiedProfile :: AccountProfile
verifiedProfile = AccountProfile existingAccountId existingEmailAddress (Username.mkUsername "person_01") (Just "Person Example") True

existingAccountId :: AccountId
existingAccountId =
  case mkAccountId "account_01" of
    Just value -> value
    Nothing -> error "expected a valid account id"

existingEmailAddress :: EmailAddress
existingEmailAddress =
  case mkEmailAddress "person@example.test" of
    Just value -> value
    Nothing -> error "expected a valid email address"

testSessionId :: SessionId
testSessionId =
  case mkSessionId "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_" of
    Just value -> value
    Nothing -> error "expected a valid session id"

spec = do
  existingSpec
  describe "WebApi.AccountPages" $ do
    it "keeps account routes fully server-rendered and query-aware" $ do
      let registrationRequest = HarchWeb.RouteRequest RegistrationRoute defaultRequestContext
          verificationRequest = HarchWeb.RouteRequest EmailVerificationRoute (defaultRequestContext {requestQueryParameters = [("token", "prefilled-token")]})
          mfaRequest = HarchWeb.RouteRequest MfaEnrollmentRoute (defaultRequestContext {requestQueryParameters = [("account", "account_01")]})
          loginRequest = HarchWeb.RouteRequest LoginRoute defaultRequestContext
          logoutRequest = HarchWeb.RouteRequest LogoutRoute defaultRequestContext
          profileRequestValue = HarchWeb.RouteRequest ProfileRoute defaultRequestContext
      selectRouteData registrationRequest `shouldReturn` RegistrationRouteDataResult
      selectRouteData verificationRequest `shouldReturn` EmailVerificationRouteDataResult
      selectRouteData mfaRequest `shouldReturn` MfaEnrollmentRouteDataResult
      selectRouteData loginRequest `shouldReturn` LoginRouteDataResult
      selectRouteData logoutRequest `shouldReturn` LogoutRouteDataResult
      selectRouteData profileRequestValue `shouldReturn` ProfileRouteDataResult
      selectRouteDataSelectionWithDatabase defaultPageRepository registrationRequest
        `shouldReturn` RouteDataSelection RegistrationRouteDataResult []
      selectRouteDataSelectionWithDatabase defaultPageRepository verificationRequest
        `shouldReturn` RouteDataSelection EmailVerificationRouteDataResult []
      selectRouteDataSelectionWithDatabase defaultPageRepository mfaRequest
        `shouldReturn` RouteDataSelection MfaEnrollmentRouteDataResult []
      selectRouteDataSelectionWithDatabase defaultPageRepository loginRequest
        `shouldReturn` RouteDataSelection LoginRouteDataResult []
      selectRouteDataSelectionWithDatabase defaultPageRepository logoutRequest
        `shouldReturn` RouteDataSelection LogoutRouteDataResult []
      selectRouteDataSelectionWithDatabase defaultPageRepository profileRequestValue
        `shouldReturn` RouteDataSelection ProfileRouteDataResult []
      buildPageModelFromRouteData registrationRequest RegistrationRouteDataResult
        `shouldBe` RegistrationPage RegisterAccountTarget emptyRegistrationForm
      buildPageModelFromRouteData verificationRequest EmailVerificationRouteDataResult
        `shouldBe` EmailVerificationPage VerifyEmailTarget (VerificationForm "prefilled-token" Nothing False)
      buildPageModelFromRouteData (HarchWeb.RouteRequest EmailVerificationRoute defaultRequestContext) EmailVerificationRouteDataResult
        `shouldBe` EmailVerificationPage VerifyEmailTarget (VerificationForm Text.empty Nothing False)
      buildPageModelFromRouteData mfaRequest MfaEnrollmentRouteDataResult
        `shouldBe` MfaEnrollmentPage EnrollMfaTarget (MfaEnrollmentForm Nothing [] Nothing False)
      buildPageModelFromRouteData (HarchWeb.RouteRequest MfaEnrollmentRoute defaultRequestContext) MfaEnrollmentRouteDataResult
        `shouldBe` MfaEnrollmentPage EnrollMfaTarget (MfaEnrollmentForm Nothing [] Nothing False)
      buildPageModelFromRouteData loginRequest LoginRouteDataResult
        `shouldBe` LoginPage LoginAccountTarget (LoginForm Text.empty Nothing False)
      buildPageModelFromRouteData logoutRequest LogoutRouteDataResult
        `shouldBe` LogoutPage LogoutAccountTarget
      buildPageModelFromRouteData profileRequestValue ProfileRouteDataResult
        `shouldBe` ProfilePage
          ( SignedOutProfilePage
              SignedOutProfilePageDetails
                { signedOutProfileHeading = "Profile",
                  signedOutProfileSummary = "Sign in to view and manage your profile.",
                  signedOutProfileSignInAction = CallToAction "Sign in" LoginRoute "/login",
                  signedOutProfileRegistrationAction = CallToAction "Create account" RegistrationRoute "/register"
                }
          )
      let spanishProfileRequest = HarchWeb.RouteRequest ProfileRoute spanishRequestContext
          spanishProfileModel =
            SignedOutProfilePage
              SignedOutProfilePageDetails
                { signedOutProfileHeading = "Perfil",
                  signedOutProfileSummary = "Inicia sesión para ver y administrar tu perfil.",
                  signedOutProfileSignInAction = CallToAction "Iniciar sesión" LoginRoute "/es/login",
                  signedOutProfileRegistrationAction = CallToAction "Crear cuenta" RegistrationRoute "/es/register"
                }
      buildPageModelFromRouteData spanishProfileRequest ProfileRouteDataResult
        `shouldBe` ProfilePage spanishProfileModel
      let spanishProfileModelCopy =
            SignedOutProfilePage
              SignedOutProfilePageDetails
                { signedOutProfileHeading = "Perfil",
                  signedOutProfileSummary = "Inicia sesión para ver y administrar tu perfil.",
                  signedOutProfileSignInAction = CallToAction "Iniciar sesión" LoginRoute "/es/login",
                  signedOutProfileRegistrationAction = CallToAction "Crear cuenta" RegistrationRoute "/es/register"
                }
      assertSameProfilePageModel spanishProfileModel spanishProfileModelCopy
      show (ProfilePage spanishProfileModel)
        `shouldSatisfy` (Text.isPrefixOf "ProfilePage (SignedOutProfilePage (SignedOutProfilePageDetails" . Text.pack)
      show spanishProfileModel
        `shouldSatisfy` (Text.isPrefixOf "SignedOutProfilePage (SignedOutProfilePageDetails {signedOutProfileHeading = \"Perfil\"" . Text.pack)
      show [spanishProfileModel]
        `shouldSatisfy` (Text.isPrefixOf "[SignedOutProfilePage (SignedOutProfilePageDetails {signedOutProfileHeading = \"Perfil\"" . Text.pack)
      renderPageFromRouteData defaultAppConfig verificationRequest EmailVerificationRouteDataResult
        `shouldSatisfy` \page ->
          HarchWeb.pageTitle page == "web-api: Verify email"
            && "data-page=\"email-verification\"" `Text.isInfixOf` HarchWeb.renderHtml (HarchWeb.pageBody page)
            && "value=\"prefilled-token\"" `Text.isInfixOf` HarchWeb.renderHtml (HarchWeb.pageBody page)
      renderPageFromRouteData defaultAppConfig registrationRequest RegistrationRouteDataResult
        `shouldSatisfy` \page ->
          HarchWeb.pageTitle page == "web-api: Create account"
            && "data-page=\"registration\"" `Text.isInfixOf` HarchWeb.renderHtml (HarchWeb.pageBody page)
      renderPageFromRouteData defaultAppConfig mfaRequest MfaEnrollmentRouteDataResult
        `shouldSatisfy` \page ->
          HarchWeb.pageTitle page == "web-api: Set up authenticator"
            && "data-page=\"mfa-enrollment\"" `Text.isInfixOf` HarchWeb.renderHtml (HarchWeb.pageBody page)
      renderPageFromRouteData defaultAppConfig loginRequest LoginRouteDataResult
        `shouldSatisfy` \page ->
          HarchWeb.pageTitle page == "web-api: Sign in"
            && "data-page=\"login\"" `Text.isInfixOf` HarchWeb.renderHtml (HarchWeb.pageBody page)
      renderPageFromRouteData defaultAppConfig logoutRequest LogoutRouteDataResult
        `shouldSatisfy` \page ->
          HarchWeb.pageTitle page == "web-api: Sign out"
            && "data-page=\"logout\"" `Text.isInfixOf` HarchWeb.renderHtml (HarchWeb.pageBody page)
      renderPageFromRouteData defaultAppConfig profileRequestValue ProfileRouteDataResult
        `shouldSatisfy` \page ->
          HarchWeb.pageTitle page == "web-api: Profile"
            && "data-page=\"profile\"" `Text.isInfixOf` HarchWeb.renderHtml (HarchWeb.pageBody page)
            && "href=\"/login\"" `Text.isInfixOf` HarchWeb.renderHtml (HarchWeb.pageBody page)
            && "href=\"/register\"" `Text.isInfixOf` HarchWeb.renderHtml (HarchWeb.pageBody page)
      HarchWeb.renderResponse pureApplication registrationRequest
        >>= \case
          HarchWeb.PageResponse page -> HarchWeb.renderHtml (HarchWeb.pageBody page) `shouldSatisfy` Text.isInfixOf "data-page=\"registration\""
          _ -> expectationFailure "expected a registration page response"
      HarchWeb.renderResponse pureApplication verificationRequest
        >>= \case
          HarchWeb.PageResponse page -> HarchWeb.renderHtml (HarchWeb.pageBody page) `shouldSatisfy` Text.isInfixOf "data-page=\"email-verification\""
          _ -> expectationFailure "expected an email-verification page response"
      HarchWeb.renderResponse pureApplication mfaRequest
        >>= \case
          HarchWeb.PageResponse page -> HarchWeb.renderHtml (HarchWeb.pageBody page) `shouldSatisfy` Text.isInfixOf "data-page=\"mfa-enrollment\""
          _ -> expectationFailure "expected an MFA-enrollment page response"
      HarchWeb.renderResponse pureApplication loginRequest
        >>= \case
          HarchWeb.PageResponse page -> HarchWeb.renderHtml (HarchWeb.pageBody page) `shouldSatisfy` Text.isInfixOf "data-page=\"login\""
          _ -> expectationFailure "expected a login page response"
      HarchWeb.renderResponse pureApplication logoutRequest
        >>= \case
          HarchWeb.PageResponse page -> HarchWeb.renderHtml (HarchWeb.pageBody page) `shouldSatisfy` Text.isInfixOf "data-page=\"logout\""
          _ -> expectationFailure "expected a logout page response"
      HarchWeb.renderResponse pureApplication profileRequestValue
        >>= \case
          HarchWeb.PageResponse page -> HarchWeb.renderHtml (HarchWeb.pageBody page) `shouldSatisfy` Text.isInfixOf "data-page=\"profile\""
          _ -> expectationFailure "expected a profile page response"
      let runtimeApplication = buildRuntimeAppWithDatabaseBuilder defaultAppConfig (const defaultPageRepository) defaultAppEnvironmentConfig
      HarchWeb.handleClientAction
        runtimeApplication
        (typedAccountActionRequest "POST" "/register" [("username", "person_01"), ("email", "person@example.test"), ("password", "correct horse battery staple")] defaultRequestContext)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 503 (Just "registration-email") "temporarily unavailable")

    it "renders complete SSR registration and verification forms with escaped values" $ do
      if emptyRegistrationForm == RegistrationForm Text.empty Text.empty Text.empty Nothing False then pure () else expectationFailure "expected empty registration form"
      if RegistrationForm "person_01" "person@example.test" "Person Example" Nothing False /= RegistrationForm "other_01" "other@example.test" "Other Example" Nothing False then pure () else expectationFailure "registration forms must compare identity values"
      if VerificationForm "token" Nothing False /= VerificationForm "token" (Just "error") True then pure () else expectationFailure "verification forms must compare their state"
      if MfaEnrollmentForm Nothing [] Nothing False /= MfaEnrollmentForm Nothing [] (Just "error") True then pure () else expectationFailure "MFA forms must compare their state"
      if LoginForm "person@example.test" Nothing False /= LoginForm "other@example.test" Nothing False then pure () else expectationFailure "login forms must compare their email values"
      let registrationForm = RegistrationForm "person_01" "person@example.test" "Person Example" Nothing False
          verificationForm = VerificationForm "token" (Just "ready") False
          pendingProfileForm = PendingProfileForm "person@example.test" (Just "ready") False "Resend verification email"
          mfaEnrollmentForm = MfaEnrollmentForm (Just "SECRET&VALUE") ["RECOVERY-CODE"] (Just "Ready") False
          loginForm = LoginForm "person@example.test" Nothing False
      show registrationForm
        `shouldBe` "RegistrationForm {registrationFormUsername = \"person_01\", registrationFormEmail = \"person@example.test\", registrationFormDisplayName = \"Person Example\", registrationFormMessage = Nothing, registrationFormIsError = False}"
      show [registrationForm]
        `shouldBe` "[RegistrationForm {registrationFormUsername = \"person_01\", registrationFormEmail = \"person@example.test\", registrationFormDisplayName = \"Person Example\", registrationFormMessage = Nothing, registrationFormIsError = False}]"
      show verificationForm
        `shouldBe` "VerificationForm {verificationFormToken = \"token\", verificationFormMessage = Just \"ready\", verificationFormIsError = False}"
      show [verificationForm]
        `shouldBe` "[VerificationForm {verificationFormToken = \"token\", verificationFormMessage = Just \"ready\", verificationFormIsError = False}]"
      show pendingProfileForm
        `shouldBe` "PendingProfileForm {pendingProfileFormEmail = \"person@example.test\", pendingProfileFormMessage = Just \"ready\", pendingProfileFormIsError = False, pendingProfileFormResendLabel = \"Resend verification email\"}"
      show [pendingProfileForm]
        `shouldBe` "[PendingProfileForm {pendingProfileFormEmail = \"person@example.test\", pendingProfileFormMessage = Just \"ready\", pendingProfileFormIsError = False, pendingProfileFormResendLabel = \"Resend verification email\"}]"
      let printedMfaEnrollment = Text.pack (show mfaEnrollmentForm)
      printedMfaEnrollment
        `shouldBe` "MfaEnrollmentForm {mfaEnrollmentFormSecret = <redacted>, mfaEnrollmentFormRecoveryCodes = <redacted>, mfaEnrollmentFormMessage = Just \"Ready\", mfaEnrollmentFormIsError = False}"
      showsPrec 11 mfaEnrollmentForm ""
        `shouldBe` "(MfaEnrollmentForm {mfaEnrollmentFormSecret = <redacted>, mfaEnrollmentFormRecoveryCodes = <redacted>, mfaEnrollmentFormMessage = Just \"Ready\", mfaEnrollmentFormIsError = False})"
      show [mfaEnrollmentForm]
        `shouldBe` "[MfaEnrollmentForm {mfaEnrollmentFormSecret = <redacted>, mfaEnrollmentFormRecoveryCodes = <redacted>, mfaEnrollmentFormMessage = Just \"Ready\", mfaEnrollmentFormIsError = False}]"
      show loginForm
        `shouldBe` "LoginForm {loginFormEmail = \"person@example.test\", loginFormMessage = Nothing, loginFormIsError = False}"
      show [loginForm]
        `shouldBe` "[LoginForm {loginFormEmail = \"person@example.test\", loginFormMessage = Nothing, loginFormIsError = False}]"
      show (RegistrationPage RegisterAccountTarget registrationForm)
        `shouldBe` "RegistrationPage RegisterAccountTarget (RegistrationForm {registrationFormUsername = \"person_01\", registrationFormEmail = \"person@example.test\", registrationFormDisplayName = \"Person Example\", registrationFormMessage = Nothing, registrationFormIsError = False})"
      show (EmailVerificationPage VerifyEmailTarget verificationForm)
        `shouldBe` "EmailVerificationPage VerifyEmailTarget (VerificationForm {verificationFormToken = \"token\", verificationFormMessage = Just \"ready\", verificationFormIsError = False})"
      let printedMfaEnrollmentPage = Text.pack (show (MfaEnrollmentPage EnrollMfaTarget mfaEnrollmentForm))
      printedMfaEnrollmentPage
        `shouldBe` "MfaEnrollmentPage EnrollMfaTarget (MfaEnrollmentForm {mfaEnrollmentFormSecret = <redacted>, mfaEnrollmentFormRecoveryCodes = <redacted>, mfaEnrollmentFormMessage = Just \"Ready\", mfaEnrollmentFormIsError = False})"
      printedMfaEnrollment `shouldSatisfy` (not . Text.isInfixOf "SECRET&VALUE")
      printedMfaEnrollment `shouldSatisfy` (not . Text.isInfixOf "RECOVERY-CODE")
      printedMfaEnrollmentPage `shouldSatisfy` (not . Text.isInfixOf "SECRET&VALUE")
      printedMfaEnrollmentPage `shouldSatisfy` (not . Text.isInfixOf "RECOVERY-CODE")
      show (LoginPage LoginAccountTarget loginForm)
        `shouldBe` "LoginPage LoginAccountTarget (LoginForm {loginFormEmail = \"person@example.test\", loginFormMessage = Nothing, loginFormIsError = False})"
      show (LogoutPage LogoutAccountTarget) `shouldBe` "LogoutPage LogoutAccountTarget"
      renderRegistrationPage (defaultRequestContext {requestLocale = Spanish}) Spanish (RegistrationForm "person_01\" onclick=\"bad" "person@example.test\" onclick=\"bad" "Person & Example" (Just "Ready <now>") False)
        `shouldSatisfy` \html ->
          "Nombre de usuario" `Text.isInfixOf` html
            && "Nombre para mostrar (opcional)" `Text.isInfixOf` html
            && "person_01&quot; onclick=&quot;bad" `Text.isInfixOf` html
            && "person@example.test&quot; onclick=&quot;bad" `Text.isInfixOf` html
            && "Person &amp; Example" `Text.isInfixOf` html
            && "Ready &lt;now&gt;" `Text.isInfixOf` html
      renderRegistrationRegion defaultRequestContext English (RegistrationForm Text.empty Text.empty Text.empty (Just "No") True)
        `shouldSatisfy` Text.isInfixOf "data-error-state=\"true\""
      renderVerificationPage defaultRequestContext English (VerificationForm "token&value" Nothing False)
        `shouldSatisfy` \html ->
          "<section data-page=\"email-verification\">" `Text.isPrefixOf` html
            && "value=\"token&amp;value\"" `Text.isInfixOf` html
      renderVerificationPage (defaultRequestContext {requestLocale = Spanish}) Spanish (VerificationForm Text.empty Nothing False)
        `shouldSatisfy` Text.isInfixOf "Verifica tu direccion de correo"
      renderVerificationRegion defaultRequestContext English (VerificationForm Text.empty Nothing False)
        `shouldSatisfy` (not . Text.isInfixOf "data-account-message")
      renderRegistrationRegion defaultRequestContext English (RegistrationForm "'>&" "'>&" "'>&" Nothing False)
        `shouldSatisfy` \html -> "&#39;&gt;&amp;" `Text.isInfixOf` html
      let spanishMfaPage = renderMfaEnrollmentPage (defaultRequestContext {requestLocale = Spanish}) Spanish (MfaEnrollmentForm (Just "SECRET&VALUE") ["CODE-ONE"] (Just "Ready <now>") False)
      spanishMfaPage
        `shouldSatisfy` \html -> "data-harch-control" `Text.isInfixOf` html && "SECRET&amp;VALUE" `Text.isInfixOf` html && "Ready &lt;now&gt;" `Text.isInfixOf` html && "action=\"/es/mfa\"" `Text.isInfixOf` html
      mapM_
        (\label -> spanishMfaPage `shouldSatisfy` Text.isInfixOf label)
        [ "Configura tu autenticador",
          "Iniciar registro del autenticador",
          "Codigo del autenticador",
          "Confirmar autenticador",
          "Codigos de recuperacion",
          "Guarda estos codigos. No se mostraran de nuevo."
        ]
      renderMfaEnrollmentRegion defaultRequestContext English (MfaEnrollmentForm Nothing ["CODE-ONE"] Nothing False)
        `shouldSatisfy` Text.isInfixOf "data-recovery-codes=\"true\""
      let spanishLoginPage = renderLoginPage (defaultRequestContext {requestLocale = Spanish}) Spanish (LoginForm "person@example.test\" onclick=\"bad" (Just "Ready <now>") False)
      spanishLoginPage
        `shouldSatisfy` \html -> "data-page=\"login\"" `Text.isInfixOf` html && "action=\"/es/login\"" `Text.isInfixOf` html && "autocomplete=\"username\"" `Text.isInfixOf` html && "person@example.test&quot; onclick=&quot;bad" `Text.isInfixOf` html && "Ready &lt;now&gt;" `Text.isInfixOf` html
      mapM_
        (\label -> spanishLoginPage `shouldSatisfy` Text.isInfixOf label)
        [ "Iniciar sesion",
          "Direccion de correo o nombre de usuario",
          "Contrasena",
          "Metodo de verificacion",
          "Codigo del autenticador",
          "Codigo de recuperacion",
          "Codigo de verificacion"
        ]
      renderLoginRegion defaultRequestContext English (LoginForm Text.empty Nothing False)
        `shouldSatisfy` (not . Text.isInfixOf "data-account-message")
      renderLogoutPage defaultRequestContext English `shouldSatisfy` Text.isInfixOf "data-harch-control"
      Text.length (renderLogoutPage defaultRequestContext English) `shouldSatisfy` (> 0)
      let spanishLogoutPage = renderLogoutPage (defaultRequestContext {requestLocale = Spanish}) Spanish
      spanishLogoutPage `shouldSatisfy` Text.isInfixOf "Cerrar sesion"
      spanishLogoutPage `shouldSatisfy` Text.isInfixOf ">Cerrar sesion</button>"
      renderLogoutRegion defaultRequestContext English (Just "Signed <out>") True
        `shouldSatisfy` \html -> "data-error-state=\"true\"" `Text.isInfixOf` html && "Signed &lt;out&gt;" `Text.isInfixOf` html
      pageEnhancementHooks RegistrationRoute `shouldBe` []
      pageEnhancementHooks EmailVerificationRoute `shouldBe` []
      pageEnhancementHooks MfaEnrollmentRoute `shouldBe` []
      pageEnhancementHooks LoginRoute `shouldBe` []
      pageEnhancementHooks LogoutRoute `shouldBe` []

    it "captures registration actions before deferred behavior and patches the localized region" $ do
      deliveredMessagesReference <- newIORef []
      let accountId = requiredAccountId "account_01"
          emailAddress = requiredEmailAddress "person@example.test"
          token = requiredVerificationToken (Text.replicate 43 "a")
          storedVerification = Account.mkStoredEmailVerification accountId emailAddress 500 token
          createdStore =
            AccountStore
              { createPendingAccount = \pendingAccount ->
                  do
                    pendingAccountUsername pendingAccount `shouldBe` Just (fromMaybe (error "expected username") (Username.mkUsername "person_01"))
                    pendingAccountDisplayName pendingAccount `shouldSatisfy` (`elem` [Nothing, Just "Person Example"])
                    pendingAccountEmail pendingAccount `shouldBe` emailAddress
                    pure (Right PendingAccountCreated),
                replaceEmailVerification = \_ -> error "unexpected verification replacement",
                findEmailVerification = \_ -> pure (Right (Just storedVerification)),
                consumeEmailVerification = \_ _ -> pure (Right (Just accountId))
              }
          workflow =
            AccountWorkflow
              { accountWorkflowStore = createdStore,
                accountWorkflowEmailDelivery = Email.EmailDelivery (\message -> modifyIORef' deliveredMessagesReference (<> [message])),
                accountWorkflowPasswordHasher = Password.hashPassword,
                accountWorkflowPasswordWorkGate = accountWorkflowPasswordWorkGate unavailableAccountWorkflow,
                accountWorkflowClock = pure 100,
                accountWorkflowMfaStore = accountWorkflowMfaStore unavailableAccountWorkflow,
                accountWorkflowCredentialStore = accountWorkflowCredentialStore unavailableAccountWorkflow,
                accountWorkflowLoginAttemptStore = accountWorkflowLoginAttemptStore unavailableAccountWorkflow,
                accountWorkflowSessionStore = accountWorkflowSessionStore unavailableAccountWorkflow,
                accountWorkflowMfaEnrollmentSessionStore = accountWorkflowMfaEnrollmentSessionStore unavailableAccountWorkflow,
                accountWorkflowProfileStore = accountWorkflowProfileStore unavailableAccountWorkflow,
                accountWorkflowTotpEncryptionKey = accountWorkflowTotpEncryptionKey unavailableAccountWorkflow,
                accountWorkflowTotpClock = const 0,
                accountWorkflowVerificationUrl = \requestContext verificationToken ->
                  "https://account.example.test"
                    <> renderRoutePath
                      HarchWeb.RouteRequest
                        { HarchWeb.requestRoute = EmailVerificationRoute,
                          HarchWeb.requestContext = requestContext
                        }
                    <> "?token="
                    <> Account.emailVerificationTokenText verificationToken
              }
          request method path fields locale = typedAccountActionRequest method path fields (defaultRequestContext {requestLocale = locale})
          rawAction method path fields =
            HarchWeb.ClientActionPayload
              { HarchWeb.clientActionMethod = method,
                HarchWeb.clientActionPath = path,
                HarchWeb.clientActionFields = fields,
                HarchWeb.clientActionCsrfToken = Nothing,
                HarchWeb.clientActionIdempotencyKey = Nothing,
                HarchWeb.clientActionPayloadContext = defaultRequestContext
              }
      case Action.decodeAction accountActions (rawAction "GET" "/register" []) of
        HarchWeb.MethodNotAllowedClientAction _ -> pure ()
        _ -> expectationFailure "expected declared path with an unsupported method to be rejected"
      case Action.decodeAction accountActions (rawAction "POST" "/missing" []) of
        HarchWeb.UnrecognizedClientAction -> pure ()
        _ -> expectationFailure "expected unknown action path to be unrecognized"
      case Action.decodeAction accountActions (rawAction "POST" "/register" []) of
        HarchWeb.DecodedClientAction _ -> pure ()
        _ -> expectationFailure "expected registration action to decode"
      let assertDuplicateField path fieldName =
            case Action.decodeAction accountActions (rawAction "POST" path [(fieldName, "first"), (fieldName, "second")]) of
              HarchWeb.MalformedClientAction (Action.DuplicateActionField duplicateFieldName :| []) -> duplicateFieldName `shouldBe` fieldName
              _ -> expectationFailure "expected duplicate account action fields to be rejected"
      expectAll
        ( assertDuplicateField "/register" "username"
            :| [ assertDuplicateField "/register" "email",
                 assertDuplicateField "/register" "displayName",
                 assertDuplicateField "/register" "password",
                 assertDuplicateField "/verify" "token",
                 assertDuplicateField "/mfa" "intent",
                 assertDuplicateField "/mfa" "code",
                 assertDuplicateField "/login" "email",
                 assertDuplicateField "/login" "username",
                 assertDuplicateField "/login" "password",
                 assertDuplicateField "/login" "proof",
                 assertDuplicateField "/login" "code",
                 assertDuplicateField "/profile" "intent"
               ]
        )
      case Action.decodeAction accountActions (rawAction "POST" "/login" [("email", "first@example.test"), ("email", "second@example.test")]) of
        HarchWeb.MalformedClientAction _ -> pure ()
        _ -> expectationFailure "expected duplicate action fields to be malformed"
      case Action.decodeAction accountActions (rawAction "POST" "/register" [("username", "person_01"), ("email", "person@example.test"), ("displayName", "Person Example"), ("password", "correct horse battery staple")]) of
        HarchWeb.DecodedClientAction _ -> pure ()
        _ -> expectationFailure "expected a fully populated registration submission to decode"
      case Action.decodeAction accountActions (rawAction "POST" "/mfa" [("intent", "confirm"), ("code", "123456")]) of
        HarchWeb.DecodedClientAction _ -> pure ()
        _ -> expectationFailure "expected a fully populated MFA enrollment submission to decode"
      case Action.decodeAction accountActions (rawAction "POST" "/login" [("email", "person@example.test"), ("username", "person_01"), ("password", "correct horse battery staple"), ("proof", "123456"), ("code", "recovery-code")]) of
        HarchWeb.DecodedClientAction _ -> pure ()
        _ -> expectationFailure "expected a fully populated login submission to decode"
      let assertSingleFieldDecodes path fieldName fieldValue =
            case Action.decodeAction accountActions (rawAction "POST" path [(fieldName, fieldValue)]) of
              HarchWeb.DecodedClientAction _ -> pure ()
              _ -> expectationFailure ("expected a single " <> Text.unpack fieldName <> " value to decode")
      expectAll
        ( assertSingleFieldDecodes "/register" "username" "person_01"
            :| [ assertSingleFieldDecodes "/register" "email" "person@example.test",
                 assertSingleFieldDecodes "/register" "password" "correct horse battery staple",
                 assertSingleFieldDecodes "/mfa" "intent" "confirm",
                 assertSingleFieldDecodes "/login" "email" "person@example.test",
                 assertSingleFieldDecodes "/login" "password" "correct horse battery staple",
                 assertSingleFieldDecodes "/login" "proof" "123456",
                 assertSingleFieldDecodes "/login" "code" "recovery-code"
               ]
        )
      invalidMfaResult <- handleAccountAction workflow (request "POST" "/mfa" [("intent", "start")] English)
      invalidMfaResult `shouldSatisfy` actionHasStatusAndFocus 403 Nothing "This enrollment link is invalid or has expired"
      spanishInvalidMfaResult <- handleAccountAction workflow (request "POST" "/es/mfa" [("intent", "start")] Spanish)
      spanishInvalidMfaResult `shouldSatisfy` \case
        Just response -> Http.statusCode (HarchWeb.clientActionStatus response) == 403 && any (Text.isInfixOf "action=\"/es/mfa\"" . HarchWeb.regionPatchHtml) (HarchWeb.clientActionPatches response)
        Nothing -> False
      invalidUsernameResult <- handleAccountAction workflow (request "POST" "/register" [("username", "no!"), ("email", "person@example.test"), ("password", "correct horse battery staple")] English)
      invalidUsernameResult `shouldSatisfy` actionHasStatusAndFocus 422 (Just "registration-username") "Use a username"
      spanishInvalidUsernameResult <- handleAccountAction workflow (request "POST" "/es/register" [("username", "no!"), ("email", "person@example.test"), ("password", "correct horse battery staple")] Spanish)
      spanishInvalidUsernameResult `shouldSatisfy` actionHasStatusAndFocus 422 (Just "registration-username") "Usa un nombre de usuario"
      invalidEmailResult <- handleAccountAction workflow (request "POST" "/register" [("username", "person_01"), ("email", "not-an-email"), ("password", "correct horse battery staple")] English)
      invalidEmailResult `shouldSatisfy` actionHasStatusAndFocus 422 (Just "registration-email") "Enter a valid email address."
      spanishInvalidEmailResult <- handleAccountAction workflow (request "POST" "/es/register" [("username", "person_01"), ("email", "not-an-email"), ("password", "correct horse battery staple")] Spanish)
      spanishInvalidEmailResult `shouldSatisfy` actionHasStatusAndFocus 422 (Just "registration-email") "Introduce una direccion"
      invalidPasswordResult <- handleAccountAction workflow (request "POST" "/register" [("username", "person_01"), ("email", "person@example.test"), ("password", "short")] English)
      invalidPasswordResult `shouldSatisfy` actionHasStatusAndFocus 422 (Just "registration-password") "Use a password with at least 12 characters."
      spanishInvalidPasswordResult <- handleAccountAction workflow (request "POST" "/es/register" [("username", "person_01"), ("email", "person@example.test"), ("password", "short")] Spanish)
      spanishInvalidPasswordResult `shouldSatisfy` actionHasStatusAndFocus 422 (Just "registration-password") "Usa una contrasena"
      emptyDisplayNameResult <- handleAccountAction workflow (request "POST" "/register" [("username", "person_01"), ("email", "person@example.test"), ("displayName", ""), ("password", "correct horse battery staple")] English)
      emptyDisplayNameResult `shouldSatisfy` actionHasStatusAndFocus 202 Nothing "If that address can register, check its inbox"
      createdResult <- handleAccountAction workflow (request "POST" "/es/register" [("username", "person_01"), ("email", "person@example.test"), ("displayName", "Person Example"), ("password", "correct horse battery staple")] Spanish)
      createdResult `shouldSatisfy` actionHasStatusAndFocus 202 Nothing "Si esa direccion puede registrarse, revisa su bandeja de entrada"
      deliveredMessages <- readIORef deliveredMessagesReference
      deliveredMessages `shouldSatisfy` \case
        [_, message] -> "https://account.example.test/es/verify?token=" `Text.isInfixOf` Email.emailMessageBody message
        _ -> False
      unconfiguredAction <-
        HarchWeb.handleClientAction
          pureApplication
          (typedAccountActionRequest "POST" "/register" [("username", "person_01"), ("email", "person@example.test"), ("password", "correct horse battery staple")] defaultRequestContext)
      unconfiguredAction `shouldSatisfy` actionHasStatusAndFocus 503 (Just "registration-email") "temporarily unavailable"
      let unconfiguredStore = accountWorkflowStore unavailableAccountWorkflow
      assertAccountStoreError
        (createPendingAccount unconfiguredStore (error "the unavailable store must ignore pending-account input"))
        (isUnavailable "account persistence is not configured")
      assertAccountStoreError
        (replaceEmailVerification unconfiguredStore (error "the unavailable store must ignore verification input"))
        (isUnavailable "account persistence is not configured")
      assertAccountStoreError
        (findEmailVerification unconfiguredStore (Account.emailVerificationTokenDigest token))
        (isUnavailable "account persistence is not configured")
      assertAccountStoreError
        (consumeEmailVerification unconfiguredStore (Account.emailVerificationTokenDigest token) 0)
        (isUnavailable "account persistence is not configured")
      let unconfiguredMfaStore = accountWorkflowMfaStore unavailableAccountWorkflow
          assertMfaUnavailable action = do
            result <- action
            case result of
              Left (MfaStoreUnavailable "MFA persistence is not configured") -> pure ()
              _ -> expectationFailure "expected unavailable MFA persistence"
      assertMfaUnavailable (loadTotpEnrollment unconfiguredMfaStore accountId)
      assertMfaUnavailable (saveUnconfirmedTotpEnrollment unconfiguredMfaStore accountId "secret" 0)
      assertMfaUnavailable (confirmTotpEnrollment unconfiguredMfaStore accountId ("hash" :| []) 0)
      assertMfaUnavailable (loadUnusedRecoveryCodeHashes unconfiguredMfaStore accountId)
      assertMfaUnavailable (consumeRecoveryCodeHash unconfiguredMfaStore accountId "hash" 0)
      assertMfaUnavailable (markTotpCodeUsed unconfiguredMfaStore accountId 0)
      let unconfiguredCredentialStore = accountWorkflowCredentialStore unavailableAccountWorkflow
      findAccountCredentialByEmail unconfiguredCredentialStore (requiredEmailAddress "person@example.test")
        >>= \case
          Left (AccountCredentialStoreUnavailable "account credentials are not configured") -> pure ()
          _ -> expectationFailure "expected unavailable account credentials"
      findAccountCredentialByUsername unconfiguredCredentialStore (fromMaybe (error "expected valid username") (Username.mkUsername "person_01"))
        >>= \case
          Left (AccountCredentialStoreUnavailable "account credentials are not configured") -> pure ()
          _ -> expectationFailure "expected unavailable account credentials"
      let unconfiguredLoginAttemptStore = accountWorkflowLoginAttemptStore unavailableAccountWorkflow
          assertLoginAttemptsUnavailable :: IO (Either LoginAttemptStoreError value) -> Expectation
          assertLoginAttemptsUnavailable action =
            action >>= \case
              Left (LoginAttemptStoreUnavailable "login-attempt persistence is not configured") -> pure ()
              _ -> expectationFailure "expected unavailable login-attempt persistence"
      assertLoginAttemptsUnavailable (reserveLoginAttempt unconfiguredLoginAttemptStore "key" LoginProtection.defaultLoginProtectionPolicy 0)
      assertLoginAttemptsUnavailable (settleLoginAttempt unconfiguredLoginAttemptStore (LoginAttemptReservation "reservation") True)
      assertLoginAttemptsUnavailable (cancelLoginAttempt unconfiguredLoginAttemptStore (LoginAttemptReservation "reservation"))
      let unconfiguredSessionStore = accountWorkflowSessionStore unavailableAccountWorkflow
          assertSessionUnavailable :: IO (Either AccountSessionStoreError value) -> Expectation
          assertSessionUnavailable action =
            action >>= \case
              Left AccountSessionStoreUnavailable -> pure ()
              _ -> expectationFailure "expected unavailable account sessions"
      assertSessionUnavailable (saveAccountSession unconfiguredSessionStore (error "unavailable session store must ignore input"))
      assertSessionUnavailable (loadAccountSession unconfiguredSessionStore (error "unavailable session store must ignore input"))
      assertSessionUnavailable (invalidateAccountSession unconfiguredSessionStore (error "unavailable session store must ignore input") (error "unavailable session store must ignore input"))
      let unconfiguredMfaEnrollmentSessionStore = accountWorkflowMfaEnrollmentSessionStore unavailableAccountWorkflow
          assertMfaEnrollmentSessionUnavailable :: IO (Either MfaEnrollmentSessionStoreError value) -> Expectation
          assertMfaEnrollmentSessionUnavailable action =
            action >>= \case
              Left MfaEnrollmentSessionStoreUnavailable -> pure ()
              _ -> expectationFailure "expected unavailable MFA-enrollment sessions"
      assertMfaEnrollmentSessionUnavailable (saveMfaEnrollmentSession unconfiguredMfaEnrollmentSessionStore (error "unavailable MFA-enrollment session store must ignore input"))
      assertMfaEnrollmentSessionUnavailable (loadMfaEnrollmentSession unconfiguredMfaEnrollmentSessionStore (error "unavailable MFA-enrollment session store must ignore input"))
      assertMfaEnrollmentSessionUnavailable (invalidateMfaEnrollmentSession unconfiguredMfaEnrollmentSessionStore (error "unavailable MFA-enrollment session store must ignore input") (error "unavailable MFA-enrollment session store must ignore input"))
      findAccountProfile (accountWorkflowProfileStore unavailableAccountWorkflow) accountId
        >>= \case
          Left (AccountStoreUnavailable "account profiles are not configured") -> pure ()
          _ -> expectationFailure "expected unavailable account profiles"
      accountWorkflowPasswordHasher unavailableAccountWorkflow `seq` pure ()
      accountWorkflowTotpEncryptionKey unavailableAccountWorkflow `seq` pure ()
      accountWorkflowTotpClock unavailableAccountWorkflow 0 `shouldBe` 0
      unavailableDelivery <-
        try (Email.deliverEmail (accountWorkflowEmailDelivery unavailableAccountWorkflow) (error "the unavailable delivery must ignore messages")) :: IO (Either IOException ())
      unavailableDelivery `shouldSatisfy` \case Left errorMessage -> "email delivery is not configured" `isInfixOf` displayException errorMessage; Right _ -> False
      accountWorkflowVerificationUrl unavailableAccountWorkflow defaultRequestContext token `shouldBe` "https://invalid.example.test/verify"

    it "raises the codec-construction error for a duplicate endpoint declaration" $ do
      let duplicateEndpoints :: [Action.ActionEndpoint AccountActionTarget () AccountAction]
          duplicateEndpoints =
            [ Action.action RegisterAccountTarget (Action.postAt "/dup" (const "/dup")) (pure LogoutAccount),
              Action.action LoginAccountTarget (Action.postAt "/dup" (const "/dup")) (pure LogoutAccount)
            ]
      evaluate (buildActionCodecOrDie duplicateEndpoints `seq` ())
        `shouldThrow` \case
          ErrorCall message -> "DuplicateActionEndpoint" `isInfixOf` message

    it "derives comparable, printable representations for every account action target" $ do
      let targets =
            [ RegisterAccountTarget,
              VerifyEmailTarget,
              EnrollMfaTarget,
              LoginAccountTarget,
              UpdateProfileTarget,
              LogoutAccountTarget
            ]
      expectAll
        ( (sum [fromEnum (left == right) | left <- targets, right <- targets] `shouldBe` length targets)
            :| [ sum [fromEnum (left /= right) | left <- targets, right <- targets] `shouldBe` length targets * (length targets - 1),
                 sum [length (show targetValue) + length (showList [targetValue] "") | targetValue <- targets] `shouldSatisfy` (> 0)
               ]
        )

    it "returns opaque registration failures and accepts, rejects, or expires verification actions" $ do
      let accountId = requiredAccountId "account_01"
          emailAddress = requiredEmailAddress "person@example.test"
          token = requiredVerificationToken (Text.replicate 43 "a")
          storedVerification = Account.mkStoredEmailVerification accountId emailAddress 500 token
          request path fields = typedAccountActionRequest "POST" path fields defaultRequestContext
          workflowFor accountStore now emailDelivery =
            AccountWorkflow
              { accountWorkflowStore = accountStore,
                accountWorkflowEmailDelivery = emailDelivery,
                accountWorkflowPasswordHasher = Password.hashPassword,
                accountWorkflowPasswordWorkGate = accountWorkflowPasswordWorkGate unavailableAccountWorkflow,
                accountWorkflowClock = pure now,
                accountWorkflowMfaStore = accountWorkflowMfaStore unavailableAccountWorkflow,
                accountWorkflowCredentialStore = accountWorkflowCredentialStore unavailableAccountWorkflow,
                accountWorkflowLoginAttemptStore = accountWorkflowLoginAttemptStore unavailableAccountWorkflow,
                accountWorkflowSessionStore = accountWorkflowSessionStore unavailableAccountWorkflow,
                accountWorkflowMfaEnrollmentSessionStore = accountWorkflowMfaEnrollmentSessionStore unavailableAccountWorkflow,
                accountWorkflowProfileStore = accountWorkflowProfileStore unavailableAccountWorkflow,
                accountWorkflowTotpEncryptionKey = accountWorkflowTotpEncryptionKey unavailableAccountWorkflow,
                accountWorkflowTotpClock = const 0,
                accountWorkflowVerificationUrl = \_ verificationToken -> "https://account.example.test/verify?token=" <> Account.emailVerificationTokenText verificationToken
              }
          store createResult lookupResult consumeResult =
            AccountStore
              { createPendingAccount = \_ -> pure createResult,
                replaceEmailVerification = \_ -> error "unexpected verification replacement",
                findEmailVerification = \_ -> pure lookupResult,
                consumeEmailVerification = \_ _ -> pure consumeResult
              }
          validRegistration = [("username", "person_01"), ("email", "person@example.test"), ("password", "correct horse battery staple")]
          validToken = [("token", Account.emailVerificationTokenText token)]
          delivery = Email.EmailDelivery (\message -> Email.emailMessageSubject message `shouldBe` "Verify your email address")
          spanishAction path fields = typedAccountActionRequest "POST" ("/es" <> path) fields (defaultRequestContext {requestLocale = Spanish})
      alreadyRegistered <- handleAccountAction (workflowFor (store (Right PendingAccountEmailTaken) (Right Nothing) (Right Nothing)) 100 delivery) (request "/register" validRegistration)
      alreadyRegistered `shouldSatisfy` actionHasStatusAndFocus 202 Nothing "If that address can register"
      spanishAlreadyRegistered <- handleAccountAction (workflowFor (store (Right PendingAccountEmailTaken) (Right Nothing) (Right Nothing)) 100 delivery) (spanishAction "/register" validRegistration)
      spanishAlreadyRegistered `shouldSatisfy` actionHasStatusAndFocus 202 Nothing "Si esa direccion"
      createdEnglish <- handleAccountAction (workflowFor (store (Right PendingAccountCreated) (Right Nothing) (Right Nothing)) 100 delivery) (request "/register" validRegistration)
      createdEnglish `shouldSatisfy` actionHasStatusAndFocus 202 Nothing "If that address can register"
      -- Byte-identical responses for the already-registered and newly-created
      -- outcomes: the hedged "if that address can register" wording only
      -- protects against enumeration if both branches answer identically.
      alreadyRegistered `shouldBe` createdEnglish
      usernameTaken <- handleAccountAction (workflowFor (store (Right PendingAccountUsernameTaken) (Right Nothing) (Right Nothing)) 100 delivery) (request "/register" validRegistration)
      usernameTaken `shouldSatisfy` actionHasStatusAndFocus 422 (Just "registration-username") "That username is already taken"
      spanishUsernameTaken <- handleAccountAction (workflowFor (store (Right PendingAccountUsernameTaken) (Right Nothing) (Right Nothing)) 100 delivery) (spanishAction "/register" validRegistration)
      spanishUsernameTaken `shouldSatisfy` actionHasStatusAndFocus 422 (Just "registration-username") "Ese nombre de usuario ya esta en uso"
      unavailableRegistration <- handleAccountAction (workflowFor (store (Left (AccountStoreUnavailable "down")) (Right Nothing) (Right Nothing)) 100 delivery) (request "/register" validRegistration)
      unavailableRegistration `shouldSatisfy` actionHasStatusAndFocus 503 (Just "registration-email") "temporarily unavailable"
      spanishUnavailableRegistration <- handleAccountAction (workflowFor (store (Left (AccountStoreUnavailable "down")) (Right Nothing) (Right Nothing)) 100 delivery) (spanishAction "/register" validRegistration)
      spanishUnavailableRegistration `shouldSatisfy` actionHasStatusAndFocus 503 (Just "registration-email") "no esta disponible"
      corruptRegistration <- handleAccountAction (workflowFor (store (Left (AccountStoreCorruptData "bad")) (Right Nothing) (Right Nothing)) 100 delivery) (request "/register" validRegistration)
      corruptRegistration `shouldSatisfy` actionHasStatusAndFocus 503 (Just "registration-email") "temporarily unavailable"
      corruptRegistration
        `shouldSatisfy` maybe
          False
          ( \response ->
              not (any (Text.isInfixOf "bad" . HarchWeb.regionPatchHtml) (HarchWeb.clientActionPatches response))
                && any (Text.isInfixOf "bad") (HarchWeb.clientActionLogEntries response)
                && any (\attribute -> Observability.attributeName attribute == "app.failure.code" && Observability.attributeValue attribute == Observability.TextAttribute "account.registration.store") (HarchWeb.clientActionObservabilityAttributes response)
          )
      deliveryFailure <- handleAccountAction (workflowFor (store (Right PendingAccountCreated) (Right Nothing) (Right Nothing)) 100 (Email.EmailDelivery (\_ -> ioError (userError "mail down")))) (request "/register" validRegistration)
      deliveryFailure `shouldSatisfy` actionHasStatusAndFocus 502 (Just "registration-email") "could not send"
      spanishDeliveryFailure <- handleAccountAction (workflowFor (store (Right PendingAccountCreated) (Right Nothing) (Right Nothing)) 100 (Email.EmailDelivery (\_ -> ioError (userError "mail down")))) (spanishAction "/register" validRegistration)
      spanishDeliveryFailure `shouldSatisfy` actionHasStatusAndFocus 502 (Just "registration-email") "No pudimos enviar"
      passwordHashingFailure <-
        handleAccountAction
          ( (workflowFor (store (Right PendingAccountCreated) (Right Nothing) (Right Nothing)) 100 delivery)
              { accountWorkflowPasswordHasher = \_ _ -> pure Nothing
              }
          )
          (request "/register" validRegistration)
      passwordHashingFailure `shouldSatisfy` actionHasStatusAndFocus 503 (Just "registration-email") "temporarily unavailable"
      spanishPasswordHashingFailure <-
        handleAccountAction
          ( (workflowFor (store (Right PendingAccountCreated) (Right Nothing) (Right Nothing)) 100 delivery)
              { accountWorkflowPasswordHasher = \_ _ -> pure Nothing
              }
          )
          (spanishAction "/register" validRegistration)
      spanishPasswordHashingFailure `shouldSatisfy` actionHasStatusAndFocus 503 (Just "registration-email") "no esta disponible"
      exhaustedRegistrationBudget <- Password.newPasswordWorkGate (fromMaybe (error "expected a positive password-work budget") (Password.mkPasswordWorkBudget 8))
      registrationBudgetExhausted <-
        handleAccountAction
          ( (workflowFor (store (Right PendingAccountCreated) (Right Nothing) (Right Nothing)) 100 delivery)
              { accountWorkflowPasswordWorkGate = exhaustedRegistrationBudget
              }
          )
          (request "/register" validRegistration)
      registrationBudgetExhausted `shouldSatisfy` actionHasStatusAndFocus 503 (Just "registration-email") "temporarily unavailable"
      spanishRegistrationBudgetExhausted <-
        handleAccountAction
          ( (workflowFor (store (Right PendingAccountCreated) (Right Nothing) (Right Nothing)) 100 delivery)
              { accountWorkflowPasswordWorkGate = exhaustedRegistrationBudget
              }
          )
          (spanishAction "/register" validRegistration)
      spanishRegistrationBudgetExhausted `shouldSatisfy` actionHasStatusAndFocus 503 (Just "registration-email") "no esta disponible"
      clockOverflow <- handleAccountAction (workflowFor (store (Right PendingAccountCreated) (Right Nothing) (Right Nothing)) maxBound delivery) (request "/register" validRegistration)
      clockOverflow `shouldSatisfy` actionHasStatusAndFocus 503 (Just "registration-email") "temporarily unavailable"
      spanishClockOverflow <- handleAccountAction (workflowFor (store (Right PendingAccountCreated) (Right Nothing) (Right Nothing)) maxBound delivery) (spanishAction "/register" validRegistration)
      spanishClockOverflow `shouldSatisfy` actionHasStatusAndFocus 503 (Just "registration-email") "no esta disponible"
      invalidVerification <- handleAccountAction (workflowFor (store (Right PendingAccountCreated) (Right Nothing) (Right Nothing)) 100 delivery) (request "/verify" [("token", "invalid")])
      invalidVerification `shouldSatisfy` actionHasStatusAndFocus 422 (Just "verification-token") "link is invalid"
      spanishInvalidVerification <- handleAccountAction (workflowFor (store (Right PendingAccountCreated) (Right Nothing) (Right Nothing)) 100 delivery) (spanishAction "/verify" [("token", "invalid")])
      spanishInvalidVerification `shouldSatisfy` actionHasStatusAndFocus 422 (Just "verification-token") "enlace de verificacion no es valido"
      missingVerification <- handleAccountAction (workflowFor (store (Right PendingAccountCreated) (Right Nothing) (Right Nothing)) 100 delivery) (request "/verify" [])
      missingVerification `shouldSatisfy` actionHasStatusAndFocus 422 (Just "verification-token") "link is invalid"
      acceptedVerification <- handleAccountAction (workflowFor (store (Right PendingAccountCreated) (Right (Just storedVerification)) (Right (Just accountId))) 499 delivery) (request "/verify" validToken)
      acceptedVerification `shouldSatisfy` actionHasStatusAndFocus 200 Nothing "email address is verified"
      acceptedVerificationSessionReference <- newIORef Nothing
      let workingEnrollmentSessionStore =
            MfaEnrollmentSessionStore
              { saveMfaEnrollmentSession = \session -> writeIORef acceptedVerificationSessionReference (Just session) >> pure (Right True),
                loadMfaEnrollmentSession = \_ -> pure (error "unexpected MFA-enrollment session load"),
                invalidateMfaEnrollmentSession = \_ _ -> pure (error "unexpected MFA-enrollment session invalidation")
              }
      acceptedVerificationWithSession <-
        handleAccountAction
          (workflowFor (store (Right PendingAccountCreated) (Right (Just storedVerification)) (Right (Just accountId))) 499 delivery)
            { accountWorkflowMfaEnrollmentSessionStore = workingEnrollmentSessionStore
            }
          (request "/verify" validToken)
      case acceptedVerificationWithSession of
        Just response -> do
          forceShowValue response `shouldBe` True
          Http.statusCode (HarchWeb.clientActionStatus response) `shouldBe` 200
          HarchWeb.clientActionHeaders response `shouldSatisfy` any ((== "Set-Cookie") . fst)
        Nothing -> expectationFailure "expected a verification action response"
      savedEnrollmentSession <- readIORef acceptedVerificationSessionReference
      case savedEnrollmentSession of
        Just session -> Session.sessionPrincipal session `shouldBe` accountId
        Nothing -> expectationFailure "expected an MFA-enrollment session to be saved after verification"
      spanishAcceptedVerification <- handleAccountAction (workflowFor (store (Right PendingAccountCreated) (Right (Just storedVerification)) (Right (Just accountId))) 499 delivery) (spanishAction "/verify" validToken)
      spanishAcceptedVerification `shouldSatisfy` actionHasStatusAndFocus 200 Nothing "direccion de correo esta verificada"
      expiredVerification <- handleAccountAction (workflowFor (store (Right PendingAccountCreated) (Right (Just storedVerification)) (Right Nothing)) 500 delivery) (request "/verify" validToken)
      expiredVerification `shouldSatisfy` actionHasStatusAndFocus 422 (Just "verification-token") "has expired"
      spanishExpiredVerification <- handleAccountAction (workflowFor (store (Right PendingAccountCreated) (Right (Just storedVerification)) (Right Nothing)) 500 delivery) (spanishAction "/verify" validToken)
      spanishExpiredVerification `shouldSatisfy` actionHasStatusAndFocus 422 (Just "verification-token") "ha caducado"
      rejectedVerification <- handleAccountAction (workflowFor (store (Right PendingAccountCreated) (Right Nothing) (Right Nothing)) 499 delivery) (request "/verify" validToken)
      rejectedVerification `shouldSatisfy` actionHasStatusAndFocus 422 (Just "verification-token") "invalid or has already been used"
      spanishRejectedVerification <- handleAccountAction (workflowFor (store (Right PendingAccountCreated) (Right Nothing) (Right Nothing)) 499 delivery) (spanishAction "/verify" validToken)
      spanishRejectedVerification `shouldSatisfy` actionHasStatusAndFocus 422 (Just "verification-token") "no es valido o ya se ha utilizado"
      unavailableVerification <- handleAccountAction (workflowFor (store (Right PendingAccountCreated) (Left (AccountStoreUnavailable "down")) (Right Nothing)) 499 delivery) (request "/verify" validToken)
      unavailableVerification `shouldSatisfy` actionHasStatusAndFocus 503 (Just "verification-token") "temporarily unavailable"
      spanishUnavailableVerification <- handleAccountAction (workflowFor (store (Right PendingAccountCreated) (Left (AccountStoreUnavailable "down")) (Right Nothing)) 499 delivery) (spanishAction "/verify" validToken)
      spanishUnavailableVerification `shouldSatisfy` actionHasStatusAndFocus 503 (Just "verification-token") "no esta disponible"

    it "issues a cookie only after password and TOTP verification, and revokes it on logout" $ do
      savedSessionsReference <- newIORef []
      invalidatedSessionsReference <- newIORef []
      -- Advances on every read, so logout's invalidation timestamp is
      -- provably a fresh clock reading rather than one copied from the
      -- session's own issued-at time.
      clockReference <- newIORef (123456000000000 :: UnixTimeNanoseconds)
      let accountId = requiredAccountId "account_01"
          emailAddress = requiredEmailAddress "person@example.test"
          password = Password.mkPassword "correct horse battery staple"
          passwordHash = fromMaybe (error "expected test password hash") (Password.hashPasswordWithSalt Password.defaultPasswordHashingPolicy (ByteString.replicate 16 7) password)
          totpSecret = fromMaybe (error "expected TOTP secret") (Totp.mkTotpSecret "JBSWY3DPEHPK3PXPJBSWY3DPEHPK3PXP")
          encryptedTotpSecret = requiredSecretEnvelope (Secret.encryptSecretWithNonce (totpEncryptionKey defaultAppEnvironmentConfig) (requiredSecretNonce (ByteString.replicate 12 7)) (Secret.mkSecretPlaintext (TextEncoding.encodeUtf8 (Totp.renderTotpSecret totpSecret))))
          credentialStore = AccountCredentialStore (\email -> (email `shouldBe` emailAddress) >> pure (Right (Just (AccountCredential accountId passwordHash True)))) (\_ -> pure (error "unexpected username credential lookup"))
          mfaStore =
            MfaStore
              { saveUnconfirmedTotpEnrollment = \_ _ _ -> pure (error "unexpected enrollment save"),
                loadTotpEnrollment = \account -> (account `shouldBe` accountId) >> pure (Right (Just (StoredTotpEnrollment encryptedTotpSecret (Just 1) Nothing))),
                confirmTotpEnrollment = \_ _ _ -> pure (error "unexpected enrollment confirmation"),
                loadUnusedRecoveryCodeHashes = \_ -> pure (Right []),
                consumeRecoveryCodeHash = \_ _ _ -> pure (error "unexpected recovery-code consumption"),
                markTotpCodeUsed = \_ _ -> pure (Right True)
              }
          sessionStore =
            AccountSessionStore
              { saveAccountSession = \session -> modifyIORef' savedSessionsReference (<> [session]) >> pure (Right True),
                loadAccountSession = \_ -> pure (Right Nothing),
                invalidateAccountSession = \session invalidatedAt -> modifyIORef' invalidatedSessionsReference (<> [(session, invalidatedAt)]) >> pure (Right True)
              }
          workflow =
            unavailableAccountWorkflow
              { accountWorkflowCredentialStore = credentialStore,
                accountWorkflowMfaStore = mfaStore,
                accountWorkflowSessionStore = sessionStore,
                accountWorkflowLoginAttemptStore = permissiveLoginAttemptStore,
                accountWorkflowTotpEncryptionKey = totpEncryptionKey defaultAppEnvironmentConfig,
                accountWorkflowClock = atomicModifyIORef' clockReference (\value -> (value + 1, value)),
                accountWorkflowTotpClock = unixTimeSecondsFromNanoseconds
              }
          loginRequest fields = typedAccountActionRequest "POST" "/login" fields defaultRequestContext
          loginFields = [("email", "person@example.test"), ("password", "correct horse battery staple"), ("proof", "totp"), ("code", Totp.totpCodeText (Totp.totpCode 123456 totpSecret))]
      invalidEmail <- handleAccountAction workflow (loginRequest [("email", "not an identifier!")])
      invalidEmail `shouldSatisfy` actionHasStatusAndFocus 422 (Just "login-email") "valid email address"
      loginResult <- handleAccountAction workflow (loginRequest loginFields)
      case loginResult of
        Nothing -> expectationFailure "expected login action response"
        Just response -> do
          forceShowValue response `shouldBe` True
          Http.statusCode (HarchWeb.clientActionStatus response) `shouldBe` 200
          HarchWeb.clientActionFocusId response `shouldBe` Nothing
          HarchWeb.clientActionHeaders response `shouldSatisfy` any ((== "Set-Cookie") . fst)
      savedSessions <- readIORef savedSessionsReference
      length savedSessions `shouldBe` 1
      loggedInSession <-
        case savedSessions of
          [session] -> pure session
          _ -> expectationFailure "expected exactly one saved session" >> pure (error "unreachable")
      Session.sessionPrincipal loggedInSession `shouldBe` accountId
      Session.sessionIssuedAtNanoseconds loggedInSession `shouldBe` 123456000000000
      let logoutRequest = typedAccountActionRequest "POST" "/logout" [] (defaultRequestContext {requestSessionId = Just (Session.sessionId loggedInSession)})
      logoutResult <- handleAccountAction workflow logoutRequest
      case logoutResult of
        Nothing -> expectationFailure "expected logout action response"
        Just response -> do
          forceShowValue response `shouldBe` True
          Http.statusCode (HarchWeb.clientActionStatus response) `shouldBe` 200
          HarchWeb.clientActionHeaders response `shouldSatisfy` any (Text.isInfixOf "Max-Age=0" . TextEncoding.decodeUtf8 . snd)
      invalidatedSessions <- readIORef invalidatedSessionsReference
      case invalidatedSessions of
        [(invalidatedSessionId, invalidatedAt)] ->
          expectAll
            ( (invalidatedSessionId `shouldBe` Session.sessionId loggedInSession)
                -- clockReference strictly advances, so a value read during
                -- logout (after login already completed) can never equal
                -- one read during login: this proves the invalidation
                -- timestamp is a fresh clock reading, not the session's own
                -- issued-at time copied across.
                :| [invalidatedAt `shouldNotBe` Session.sessionIssuedAtNanoseconds loggedInSession]
            )
        _ -> expectationFailure "expected exactly one invalidated session"

    it "validates every login state and keeps logout revocation failures visible" $ do
      let accountId = requiredAccountId "account_02"
          password = Password.mkPassword "correct horse battery staple"
          passwordHash = fromMaybe (error "expected test password hash") (Password.hashPasswordWithSalt Password.defaultPasswordHashingPolicy (ByteString.replicate 16 8) password)
          confirmedCredential = AccountCredential accountId passwordHash True
          unverifiedCredential = AccountCredential accountId passwordHash False
          totpSecret = fromMaybe (error "expected TOTP secret") (Totp.mkTotpSecret "JBSWY3DPEHPK3PXPJBSWY3DPEHPK3PXP")
          encryptedTotpSecret = requiredSecretEnvelope (Secret.encryptSecretWithNonce (totpEncryptionKey defaultAppEnvironmentConfig) (requiredSecretNonce (ByteString.replicate 12 8)) (Secret.mkSecretPlaintext (TextEncoding.encodeUtf8 (Totp.renderTotpSecret totpSecret))))
          confirmedEnrollment = StoredTotpEnrollment encryptedTotpSecret (Just 1) Nothing
          loginRequest requestContext fields = typedAccountActionRequest "POST" "/login" fields requestContext
          spanishLoginRequest fields = typedAccountActionRequest "POST" "/es/login" fields spanishRequestContext
          logoutRequest = typedAccountActionRequest "POST" "/logout" []
          workflowFor credentialResult enrollmentResult sessionSaveResult invalidationResult =
            unavailableAccountWorkflow
              { accountWorkflowCredentialStore = AccountCredentialStore (\_ -> pure credentialResult) (\receivedUsername -> receivedUsername `seq` pure credentialResult),
                accountWorkflowMfaStore =
                  MfaStore
                    { saveUnconfirmedTotpEnrollment = \_ _ _ -> pure (error "unexpected enrollment save"),
                      loadTotpEnrollment = \_ -> pure enrollmentResult,
                      confirmTotpEnrollment = \_ _ _ -> pure (error "unexpected enrollment confirmation"),
                      loadUnusedRecoveryCodeHashes = \_ -> pure (Right []),
                      consumeRecoveryCodeHash = \_ _ _ -> pure (error "unexpected recovery-code consumption"),
                      markTotpCodeUsed = \_ _ -> pure (Right True)
                    },
                accountWorkflowSessionStore =
                  AccountSessionStore
                    { saveAccountSession = \_ -> pure sessionSaveResult,
                      loadAccountSession = \_ -> pure (Right Nothing),
                      invalidateAccountSession = \_ _ -> pure invalidationResult
                    },
                accountWorkflowLoginAttemptStore = permissiveLoginAttemptStore,
                accountWorkflowTotpEncryptionKey = totpEncryptionKey defaultAppEnvironmentConfig,
                accountWorkflowClock = pure 500,
                accountWorkflowTotpClock = const 123456
              }
          validCode = Totp.totpCodeText (Totp.totpCode 123456 totpSecret)
          invalidCode = Text.take 5 validCode <> if Text.drop 5 validCode == "0" then "1" else "0"
          validFields = [("email", "person@example.test"), ("password", "correct horse battery staple"), ("proof", "totp"), ("code", validCode)]
          usernameFields = [("email", ""), ("username", "person_01"), ("password", "correct horse battery staple"), ("proof", "totp"), ("code", validCode)]
          uppercaseUsernameFields = [("email", ""), ("username", "Person_01"), ("password", "correct horse battery staple"), ("proof", "totp"), ("code", validCode)]
          emailUsernameFields = [("email", "person_01"), ("password", "correct horse battery staple"), ("proof", "totp"), ("code", validCode)]
          validWorkflow = workflowFor (Right (Just confirmedCredential)) (Right (Just confirmedEnrollment)) (Right True) (Right True)
          recoveryCode = fromMaybe (error "expected a valid recovery code") (RecoveryCode.mkRecoveryCode "0123456789ABCDEF0123")
          recoveryHash = fromMaybe (error "expected a recovery-code hash") (RecoveryCode.hashRecoveryCodeWithSalt testPasswordHashingPolicy "0123456789abcdef" recoveryCode)
          recoveryMfaStore =
            (accountWorkflowMfaStore validWorkflow)
              { loadUnusedRecoveryCodeHashes = \receivedAccountId -> do
                  receivedAccountId `shouldBe` accountId
                  pure (Right [RecoveryCode.recoveryCodeHashText recoveryHash]),
                consumeRecoveryCodeHash = \receivedAccountId receivedHash receivedNow -> do
                  receivedAccountId `shouldBe` accountId
                  receivedHash `shouldBe` RecoveryCode.recoveryCodeHashText recoveryHash
                  receivedNow `shouldBe` 500
                  pure (Right True)
              }
          recoveryWorkflow = validWorkflow {accountWorkflowMfaStore = recoveryMfaStore}
          recoveryFields = [("email", "person@example.test"), ("password", "correct horse battery staple"), ("proof", "recovery"), ("code", RecoveryCode.recoveryCodeText recoveryCode)]
          unavailableSession = workflowFor (Right (Just confirmedCredential)) (Right (Just confirmedEnrollment)) (Left AccountSessionStoreUnavailable) (Right True)
          mfaAttemptKey = "mfa:" <> Account.accountIdText accountId
          exhaustedMfaThrottleStore =
            LoginAttemptStore
              { reserveLoginAttempt = \key _ _ -> pure (Right (if key == mfaAttemptKey then LoginAttemptThrottled 1000 else LoginAttemptReserved (LoginAttemptReservation key))),
                settleLoginAttempt = \_ _ -> pure (Right ()),
                cancelLoginAttempt = \_ -> pure (Right ())
              }
          postCheckWriteFailure = LoginAttemptStoreUnavailable "post-check attempt write failed"
          postCheckWriteFailureStore =
            LoginAttemptStore
              { reserveLoginAttempt = \key _ _ -> pure (Right (LoginAttemptReserved (LoginAttemptReservation key))),
                settleLoginAttempt = \(LoginAttemptReservation key) _ -> if key == mfaAttemptKey then pure (Left postCheckWriteFailure) else pure (Right ()),
                cancelLoginAttempt = \_ -> pure (Right ())
              }
      canonicalUsernameKeysReference <- newIORef []
      let canonicalUsernameWorkflow =
            validWorkflow
              { accountWorkflowLoginAttemptStore =
                  LoginAttemptStore
                    { reserveLoginAttempt = \key _ _ -> modifyIORef' canonicalUsernameKeysReference (key :) >> pure (Right (LoginAttemptReserved (LoginAttemptReservation key))),
                      settleLoginAttempt = \_ _ -> pure (Right ()),
                      cancelLoginAttempt = \_ -> pure (Right ())
                    }
              }
          exhaustedTotpWorkflow =
            validWorkflow
              { accountWorkflowLoginAttemptStore = exhaustedMfaThrottleStore,
                accountWorkflowMfaStore =
                  (accountWorkflowMfaStore validWorkflow)
                    { markTotpCodeUsed = \_ _ -> error "exhausted TOTP proof must not reach the counter update"
                    }
              }
          postCheckWriteFailureWorkflow =
            (validWorkflow {accountWorkflowLoginAttemptStore = postCheckWriteFailureStore})
              { accountWorkflowSessionStore =
                  (accountWorkflowSessionStore validWorkflow)
                    { saveAccountSession = \_ -> error "a failed second-factor settlement must not issue a session"
                    }
              }
      admissionContextReference <- newIORef Nothing
      let policyCapturingWorkflow =
            validWorkflow
              { accountWorkflowLoginAttemptStore =
                  LoginAttemptStore
                    { reserveLoginAttempt = \_ policy now -> writeIORef admissionContextReference (Just (policy, now)) >> pure (Right (LoginAttemptReserved (LoginAttemptReservation "captured"))),
                      settleLoginAttempt = \_ _ -> pure (Right ()),
                      cancelLoginAttempt = \_ -> pure (Right ())
                    }
              }
      handleAccountAction policyCapturingWorkflow (loginRequest defaultRequestContext validFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 200 Nothing "You are signed in")
      readIORef admissionContextReference `shouldReturn` Just (LoginProtection.defaultLoginProtectionPolicy, 500)
      handleAccountAction validWorkflow (loginRequest defaultRequestContext [("email", "not an identifier!")])
        >>= (`shouldSatisfy` actionHasStatusAndFocus 422 (Just "login-email") "valid email address")
      handleAccountAction validWorkflow (spanishLoginRequest [("email", "not an identifier!")])
        >>= (`shouldSatisfy` actionHasStatusAndFocus 422 (Just "login-email") "nombre de usuario valido")
      handleAccountAction validWorkflow (loginRequest defaultRequestContext [("email", "person@example.test"), ("password", "short"), ("proof", "totp"), ("code", validCode)])
        >>= (`shouldSatisfy` actionHasStatusAndFocus 422 (Just "login-password") "Enter your password")
      handleAccountAction validWorkflow (spanishLoginRequest [("email", "person@example.test"), ("password", "short"), ("proof", "totp"), ("code", validCode)])
        >>= (`shouldSatisfy` actionHasStatusAndFocus 422 (Just "login-password") "Introduce tu contrasena")
      handleAccountAction validWorkflow (loginRequest defaultRequestContext [("email", "person@example.test"), ("password", "correct horse battery staple"), ("proof", "unknown"), ("code", validCode)])
        >>= (`shouldSatisfy` actionHasStatusAndFocus 422 (Just "login-code") "Enter a valid authenticator")
      handleAccountAction validWorkflow (spanishLoginRequest [("email", "person@example.test"), ("password", "correct horse battery staple"), ("proof", "unknown"), ("code", validCode)])
        >>= (`shouldSatisfy` actionHasStatusAndFocus 422 (Just "login-code") "Introduce un codigo")
      handleAccountAction (workflowFor (Right (Just unverifiedCredential)) (Right Nothing) (Right True) (Right True)) (loginRequest defaultRequestContext validFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 403 Nothing "Verify your email address")
      handleAccountAction (workflowFor (Right (Just unverifiedCredential)) (Right Nothing) (Right True) (Right True)) (spanishLoginRequest validFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 403 Nothing "Verifica tu direccion")
      handleAccountAction (workflowFor (Right (Just confirmedCredential)) (Right Nothing) (Right True) (Right True)) (loginRequest defaultRequestContext validFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 403 Nothing "Enroll your authenticator")
      handleAccountAction (workflowFor (Right (Just confirmedCredential)) (Right Nothing) (Right True) (Right True)) (spanishLoginRequest validFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 403 Nothing "Registra tu autenticador")
      loginEnrollmentSessionReference <- newIORef Nothing
      let workingLoginEnrollmentSessionStore =
            MfaEnrollmentSessionStore
              { saveMfaEnrollmentSession = \session -> writeIORef loginEnrollmentSessionReference (Just session) >> pure (Right True),
                loadMfaEnrollmentSession = \_ -> pure (error "unexpected MFA-enrollment session load"),
                invalidateMfaEnrollmentSession = \_ _ -> pure (error "unexpected MFA-enrollment session invalidation")
              }
      loginEnrollmentRequiredWithSession <-
        handleAccountAction
          (workflowFor (Right (Just confirmedCredential)) (Right Nothing) (Right True) (Right True))
            { accountWorkflowMfaEnrollmentSessionStore = workingLoginEnrollmentSessionStore
            }
          (loginRequest defaultRequestContext validFields)
      case loginEnrollmentRequiredWithSession of
        Just response -> do
          forceShowValue response `shouldBe` True
          Http.statusCode (HarchWeb.clientActionStatus response) `shouldBe` 403
          HarchWeb.clientActionHeaders response `shouldSatisfy` any ((== "Set-Cookie") . fst)
        Nothing -> expectationFailure "expected a login action response"
      savedLoginEnrollmentSession <- readIORef loginEnrollmentSessionReference
      case savedLoginEnrollmentSession of
        Just session -> Session.sessionPrincipal session `shouldBe` accountId
        Nothing -> expectationFailure "expected an MFA-enrollment session to be saved after a password-only login"
      handleAccountAction validWorkflow (loginRequest defaultRequestContext [("email", "person@example.test"), ("password", "correct horse battery staple"), ("proof", "totp"), ("code", invalidCode)])
        >>= (`shouldSatisfy` actionHasStatusAndFocus 422 (Just "login-code") "Sign-in was rejected")
      handleAccountAction validWorkflow (spanishLoginRequest [("email", "person@example.test"), ("password", "correct horse battery staple"), ("proof", "totp"), ("code", invalidCode)])
        >>= (`shouldSatisfy` actionHasStatusAndFocus 422 (Just "login-code") "inicio de sesion fue rechazado")
      handleAccountAction validWorkflow (loginRequest defaultRequestContext [("email", "person@example.test"), ("password", "correct horse battery staple"), ("proof", "recovery"), ("code", "0123456789ABCDEF0123")])
        >>= (`shouldSatisfy` actionHasStatusAndFocus 422 (Just "login-code") "Sign-in was rejected")
      handleAccountAction recoveryWorkflow (loginRequest defaultRequestContext recoveryFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 200 Nothing "You are signed in")
      handleAccountAction canonicalUsernameWorkflow (loginRequest defaultRequestContext usernameFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 200 Nothing "You are signed in")
      handleAccountAction canonicalUsernameWorkflow (loginRequest defaultRequestContext uppercaseUsernameFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 200 Nothing "You are signed in")
      canonicalUsernameKeys <- readIORef canonicalUsernameKeysReference
      filter (Text.isPrefixOf "username:") canonicalUsernameKeys `shouldBe` ["username:person_01", "username:person_01"]
      handleAccountAction validWorkflow (loginRequest defaultRequestContext emailUsernameFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 200 Nothing "You are signed in")
      handleAccountAction exhaustedTotpWorkflow (loginRequest defaultRequestContext validFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 429 (Just "login-email") "Too many sign-in attempts")
      handleAccountAction postCheckWriteFailureWorkflow (loginRequest defaultRequestContext validFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 503 (Just "login-email") "temporarily unavailable")
      usernameLoginResult <-
        beginPasswordLoginWithIdentifier
          (accountWorkflowCredentialStore validWorkflow)
          (accountWorkflowMfaStore validWorkflow)
          (permissiveLoginThrottleContext 500)
          (accountWorkflowPasswordWorkGate validWorkflow)
          (LoginUsername (fromMaybe (error "expected valid username") (Username.mkUsername "person_01")))
          password
      case usernameLoginResult of
        PasswordLoginMfaRequired receivedAccountId -> receivedAccountId `shouldBe` accountId
        _ -> expectationFailure "expected MFA to be required for a valid username login"
      handleAccountAction (workflowFor (Left (AccountCredentialStoreUnavailable "down")) (Right Nothing) (Right True) (Right True)) (loginRequest defaultRequestContext validFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 503 (Just "login-email") "temporarily unavailable")
      handleAccountAction (workflowFor (Left (AccountCredentialStoreCorruptData "bad credential")) (Right Nothing) (Right True) (Right True)) (loginRequest defaultRequestContext validFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 503 (Just "login-email") "temporarily unavailable")
      handleAccountAction (workflowFor (Left (AccountCredentialStoreUnavailable "down")) (Right Nothing) (Right True) (Right True)) (spanishLoginRequest validFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 503 (Just "login-email") "no esta disponible")
      handleAccountAction (workflowFor (Right (Just confirmedCredential)) (Left (MfaStoreUnavailable "down")) (Right True) (Right True)) (loginRequest defaultRequestContext validFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 503 (Just "login-code") "temporarily unavailable")
      handleAccountAction (workflowFor (Right (Just confirmedCredential)) (Right (Just (StoredTotpEnrollment "not-encrypted" (Just 1) Nothing))) (Right True) (Right True)) (loginRequest defaultRequestContext validFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 503 (Just "login-code") "temporarily unavailable")
      let failingLoginAttemptStore =
            LoginAttemptStore
              { reserveLoginAttempt = \_ _ _ -> pure (Left (LoginAttemptStoreUnavailable "attempt store down")),
                settleLoginAttempt = \_ _ -> pure (Left (LoginAttemptStoreUnavailable "attempt store down")),
                cancelLoginAttempt = \_ -> pure (Left (LoginAttemptStoreUnavailable "attempt store down"))
              }
          corruptLoginAttemptStore =
            LoginAttemptStore
              { reserveLoginAttempt = \_ _ _ -> pure (Left (LoginAttemptStoreCorruptData "attempt store corrupt")),
                settleLoginAttempt = \_ _ -> pure (Left (LoginAttemptStoreCorruptData "attempt store corrupt")),
                cancelLoginAttempt = \_ -> pure (Left (LoginAttemptStoreCorruptData "attempt store corrupt"))
              }
          throttledLoginAttemptStore =
            LoginAttemptStore
              { reserveLoginAttempt = \_ _ _ -> pure (Right (LoginAttemptThrottled 1000)),
                settleLoginAttempt = \_ _ -> error "unexpected throttle settlement while already throttled",
                cancelLoginAttempt = \_ -> error "unexpected throttle cancellation while already throttled"
              }
      handleAccountAction validWorkflow {accountWorkflowLoginAttemptStore = failingLoginAttemptStore} (loginRequest defaultRequestContext validFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 503 (Just "login-email") "temporarily unavailable")
      handleAccountAction validWorkflow {accountWorkflowLoginAttemptStore = corruptLoginAttemptStore} (loginRequest defaultRequestContext validFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 503 (Just "login-email") "temporarily unavailable")
      handleAccountAction validWorkflow {accountWorkflowLoginAttemptStore = throttledLoginAttemptStore} (loginRequest defaultRequestContext validFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 429 (Just "login-email") "Too many sign-in attempts")
      handleAccountAction validWorkflow {accountWorkflowLoginAttemptStore = throttledLoginAttemptStore} (spanishLoginRequest validFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 429 (Just "login-email") "Demasiados intentos")
      handleAccountAction unavailableSession (loginRequest defaultRequestContext validFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 503 (Just "login-email") "temporarily unavailable")
      handleAccountAction (workflowFor (Right (Just confirmedCredential)) (Right (Just confirmedEnrollment)) (Left AccountSessionStoreCorruptData) (Right True)) (loginRequest defaultRequestContext validFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 503 (Just "login-email") "temporarily unavailable")
      handleAccountAction unavailableSession (spanishLoginRequest validFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 503 (Just "login-email") "no esta disponible")
      handleAccountAction validWorkflow (spanishLoginRequest validFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 200 Nothing "Has iniciado sesion")
      exhaustedLoginBudget <- Password.newPasswordWorkGate (fromMaybe (error "expected a positive password-work budget") (Password.mkPasswordWorkBudget 8))
      handleAccountAction validWorkflow {accountWorkflowPasswordWorkGate = exhaustedLoginBudget} (loginRequest defaultRequestContext validFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 503 (Just "login-email") "temporarily unavailable")
      handleAccountAction validWorkflow {accountWorkflowPasswordWorkGate = exhaustedLoginBudget} (spanishLoginRequest validFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 503 (Just "login-email") "no esta disponible")
      handleAccountAction validWorkflow (logoutRequest defaultRequestContext)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 200 Nothing "You are signed out")
      handleAccountAction validWorkflow (typedAccountActionRequest "POST" "/es/logout" [] spanishRequestContext)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 200 Nothing "Has cerrado sesion")
      let sessionId = fromMaybe (error "expected valid session id") (Session.mkSessionId "0123456789ABCDEF0123456789ABCDEF0123456789ABC")
          sessionContext = defaultRequestContext {requestSessionId = Just sessionId}
      handleAccountAction (workflowFor (Right Nothing) (Right Nothing) (Right True) (Left AccountSessionStoreUnavailable)) (logoutRequest sessionContext)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 503 Nothing "Sign-out is temporarily unavailable")
      handleAccountAction (workflowFor (Right Nothing) (Right Nothing) (Right True) (Left AccountSessionStoreCorruptData)) (logoutRequest sessionContext)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 503 Nothing "Sign-out is temporarily unavailable")
      handleAccountAction
        (workflowFor (Right Nothing) (Right Nothing) (Right True) (Left AccountSessionStoreUnavailable))
        (typedAccountActionRequest "POST" "/es/logout" [] (spanishRequestContext {requestSessionId = Just sessionId}))
        >>= (`shouldSatisfy` actionHasStatusAndFocus 503 Nothing "no esta disponible")
      logoutSuccess <- handleAccountAction validWorkflow (logoutRequest sessionContext)
      case logoutSuccess of
        Just response -> do
          forceShowValue response `shouldBe` True
          HarchWeb.clientActionHeaders response `shouldSatisfy` any ((== "Set-Cookie") . fst)
        Nothing -> expectationFailure "expected a logout action response"
      spanishLogoutSuccess <- handleAccountAction validWorkflow (typedAccountActionRequest "POST" "/es/logout" [] (spanishRequestContext {requestSessionId = Just sessionId}))
      spanishLogoutSuccess `shouldSatisfy` actionHasStatusAndFocus 200 Nothing "Has cerrado sesion"

    it "captures a complete authenticator enrollment and returns recovery codes in one patch" $ do
      encryptedSecretReference <- newIORef Nothing
      confirmationHashesReference <- newIORef []
      let accountId = requiredAccountId "account_01"
          mfaStore =
            MfaStore
              { saveUnconfirmedTotpEnrollment = \receivedAccountId encryptedSecret receivedNow -> do
                  receivedAccountId `shouldBe` accountId
                  receivedNow `shouldBe` 123456000000000
                  writeIORef encryptedSecretReference (Just encryptedSecret)
                  pure (Right True),
                loadTotpEnrollment = \receivedAccountId -> do
                  receivedAccountId `shouldBe` accountId
                  fmap (Right . fmap (\secretValue -> StoredTotpEnrollment secretValue Nothing Nothing)) (readIORef encryptedSecretReference),
                confirmTotpEnrollment = \receivedAccountId hashes receivedNow -> do
                  receivedAccountId `shouldBe` accountId
                  receivedNow `shouldBe` 123456000000000
                  writeIORef confirmationHashesReference (toList hashes)
                  pure (Right True),
                loadUnusedRecoveryCodeHashes = \_ -> pure (error "unexpected recovery-code lookup"),
                consumeRecoveryCodeHash = \_ _ _ -> pure (error "unexpected recovery-code consumption"),
                markTotpCodeUsed = \_ _ -> pure (error "unexpected TOTP counter update")
              }
          workflow =
            unavailableAccountWorkflow
              { accountWorkflowMfaStore = mfaStore,
                accountWorkflowMfaEnrollmentSessionStore = enrollmentSessionStoreFor accountId,
                accountWorkflowTotpEncryptionKey = totpEncryptionKey defaultAppEnvironmentConfig,
                accountWorkflowClock = pure 123456000000000,
                accountWorkflowTotpClock = unixTimeSecondsFromNanoseconds
              }
          request path actionContext fields = typedAccountActionRequest "POST" path fields (actionContext {requestMfaEnrollmentSessionId = Just enrollmentSessionIdValue})
      started <- handleAccountAction workflow (request "/mfa" defaultRequestContext [("intent", "start")])
      started `shouldSatisfy` \case
        Just response -> Http.statusCode (HarchWeb.clientActionStatus response) == 200 && HarchWeb.clientActionFocusId response == Just "mfa-code"
        Nothing -> False
      case started of
        Just response -> forceShowValue response `shouldBe` True
        Nothing -> expectationFailure "expected an enrollment-start action response"
      secret <-
        case started of
          Just response ->
            case HarchWeb.clientActionPatches response of
              [patch] ->
                let html = HarchWeb.regionPatchHtml patch
                 in case Text.stripPrefix "<code>" (snd (Text.breakOn "<code>" html)) of
                      Just secretWithSuffix -> pure (Text.takeWhile (/= '<') secretWithSuffix)
                      Nothing -> expectationFailure "expected an enrollment secret" >> pure Text.empty
              _ -> expectationFailure "expected one enrollment patch" >> pure Text.empty
          Nothing -> expectationFailure "expected enrollment action" >> pure Text.empty
      totpSecret <- maybe (expectationFailure "expected a valid enrollment secret" >> pure (error "unreachable")) pure (Totp.mkTotpSecret secret)
      confirmed <- handleAccountAction workflow (request "/mfa" defaultRequestContext [("intent", "confirm"), ("code", Totp.totpCodeText (Totp.totpCode 123456 totpSecret))])
      confirmed `shouldSatisfy` \case
        Just response -> Http.statusCode (HarchWeb.clientActionStatus response) == 200 && any (Text.isInfixOf "data-recovery-codes=\"true\"" . HarchWeb.regionPatchHtml) (HarchWeb.clientActionPatches response)
        Nothing -> False
      case confirmed of
        Just response -> forceShowValue response `shouldBe` True
        Nothing -> expectationFailure "expected an enrollment-confirm action response"
      confirmationHashes <- readIORef confirmationHashesReference
      length confirmationHashes `shouldBe` 8
      spanishStarted <- handleAccountAction workflow (request "/es/mfa" (defaultRequestContext {requestLocale = Spanish}) [("intent", "start")])
      spanishStarted `shouldSatisfy` \case
        Just response -> Http.statusCode (HarchWeb.clientActionStatus response) == 200 && HarchWeb.clientActionFocusId response == Just "mfa-code" && any (Text.isInfixOf "Agrega este secreto" . HarchWeb.regionPatchHtml) (HarchWeb.clientActionPatches response)
        Nothing -> False
      spanishSecret <-
        case spanishStarted of
          Just response ->
            case HarchWeb.clientActionPatches response of
              [patch] ->
                let html = HarchWeb.regionPatchHtml patch
                 in case Text.stripPrefix "<code>" (snd (Text.breakOn "<code>" html)) of
                      Just secretWithSuffix -> pure (Text.takeWhile (/= '<') secretWithSuffix)
                      Nothing -> expectationFailure "expected a Spanish enrollment secret" >> pure Text.empty
              _ -> expectationFailure "expected one Spanish enrollment patch" >> pure Text.empty
          Nothing -> expectationFailure "expected a Spanish enrollment action" >> pure Text.empty
      spanishTotpSecret <- maybe (expectationFailure "expected a valid Spanish enrollment secret" >> pure (error "unreachable")) pure (Totp.mkTotpSecret spanishSecret)
      spanishConfirmed <- handleAccountAction workflow (request "/es/mfa" (defaultRequestContext {requestLocale = Spanish}) [("intent", "confirm"), ("code", Totp.totpCodeText (Totp.totpCode 123456 spanishTotpSecret))])
      spanishConfirmed `shouldSatisfy` \case
        Just response -> Http.statusCode (HarchWeb.clientActionStatus response) == 200 && isNothing (HarchWeb.clientActionFocusId response) && any (Text.isInfixOf "Autenticador registrado" . HarchWeb.regionPatchHtml) (HarchWeb.clientActionPatches response)
        Nothing -> False

    it "returns every MFA enrollment action error as a localized region patch" $ do
      let accountId = requiredAccountId "account_01"
          request fields = typedAccountActionRequest "POST" "/mfa" fields (defaultRequestContext {requestMfaEnrollmentSessionId = Just enrollmentSessionIdValue})
          spanishRequest fields = typedAccountActionRequest "POST" "/es/mfa" fields (defaultRequestContext {requestLocale = Spanish, requestMfaEnrollmentSessionId = Just enrollmentSessionIdValue})
          workflowFor mfaStore =
            unavailableAccountWorkflow
              { accountWorkflowMfaStore = mfaStore,
                accountWorkflowMfaEnrollmentSessionStore = enrollmentSessionStoreFor accountId,
                accountWorkflowTotpEncryptionKey = totpEncryptionKey defaultAppEnvironmentConfig,
                accountWorkflowClock = pure 500,
                accountWorkflowTotpClock = const 123456
              }
          mfaStoreFor saveResult loadResult confirmationResult =
            MfaStore
              { saveUnconfirmedTotpEnrollment = \_ _ _ -> pure saveResult,
                loadTotpEnrollment = \_ -> pure loadResult,
                confirmTotpEnrollment = \_ _ _ -> pure confirmationResult,
                loadUnusedRecoveryCodeHashes = \_ -> error "unexpected recovery-code lookup",
                consumeRecoveryCodeHash = \_ _ _ -> error "unexpected recovery-code consumption",
                markTotpCodeUsed = \_ _ -> error "unexpected TOTP counter update"
              }
          validTotpSecret = fromMaybe (error "expected TOTP secret") (Totp.mkTotpSecret "JBSWY3DPEHPK3PXPJBSWY3DPEHPK3PXP")
          encryptedTotpSecret =
            requiredSecretEnvelope
              (Secret.encryptSecretWithNonce (totpEncryptionKey defaultAppEnvironmentConfig) (requiredSecretNonce (ByteString.replicate 12 3)) (Secret.mkSecretPlaintext (TextEncoding.encodeUtf8 (Totp.renderTotpSecret validTotpSecret))))
          expect mfaStore fields status focusId message = do
            actionResult <- handleAccountAction (workflowFor mfaStore) (request fields)
            actionResult `shouldSatisfy` actionHasStatusAndFocus status focusId message
          expectSpanish mfaStore fields status focusId message = do
            actionResult <- handleAccountAction (workflowFor mfaStore) (spanishRequest fields)
            actionResult `shouldSatisfy` actionHasStatusAndFocus status focusId message
      expect (mfaStoreFor (Right False) (Right Nothing) (Right False)) [("intent", "start")] 422 Nothing "Verify your email address"
      expect (mfaStoreFor (Left (MfaStoreUnavailable "down")) (Right Nothing) (Right False)) [("intent", "start")] 503 Nothing "temporarily unavailable"
      expect (mfaStoreFor (Right True) (Right Nothing) (Right False)) [("intent", "confirm")] 422 (Just "mfa-code") "Enter a six-digit authenticator code"
      expect (mfaStoreFor (Right True) (Right Nothing) (Right False)) [("intent", "confirm"), ("code", "123456")] 422 (Just "mfa-code") "Start a new authenticator enrollment"
      expect (mfaStoreFor (Right True) (Left (MfaStoreCorruptData "bad enrollment")) (Right False)) [("intent", "confirm"), ("code", "123456")] 503 (Just "mfa-code") "temporarily unavailable"
      expect (mfaStoreFor (Right True) (Right (Just (StoredTotpEnrollment "not-an-envelope" Nothing Nothing))) (Right False)) [("intent", "confirm"), ("code", "123456")] 503 (Just "mfa-code") "temporarily unavailable"
      expect (mfaStoreFor (Right True) (Right (Just (StoredTotpEnrollment "not-an-envelope" (Just 100) Nothing))) (Right False)) [("intent", "confirm"), ("code", "123456")] 422 (Just "mfa-code") "That enrollment can no longer be confirmed"
      expect (mfaStoreFor (Right True) (Right Nothing) (Right False)) [("intent", "other")] 422 Nothing "Choose an enrollment action"
      expectSpanish (mfaStoreFor (Right False) (Right Nothing) (Right False)) [("intent", "start")] 422 Nothing "Verifica tu direccion de correo"
      expectSpanish (mfaStoreFor (Left (MfaStoreUnavailable "down")) (Right Nothing) (Right False)) [("intent", "start")] 503 Nothing "no esta disponible temporalmente"
      expectSpanish (mfaStoreFor (Right True) (Right Nothing) (Right False)) [("intent", "confirm")] 422 (Just "mfa-code") "Introduce un codigo de autenticador"
      expectSpanish (mfaStoreFor (Right True) (Right Nothing) (Right False)) [("intent", "confirm"), ("code", "123456")] 422 (Just "mfa-code") "Inicia un nuevo registro"
      expectSpanish (mfaStoreFor (Right True) (Left (MfaStoreCorruptData "bad enrollment")) (Right False)) [("intent", "confirm"), ("code", "123456")] 503 (Just "mfa-code") "no esta disponible temporalmente"
      expectSpanish (mfaStoreFor (Right True) (Right (Just (StoredTotpEnrollment "not-an-envelope" Nothing Nothing))) (Right False)) [("intent", "confirm"), ("code", "123456")] 503 (Just "mfa-code") "no esta disponible temporalmente"
      expectSpanish (mfaStoreFor (Right True) (Right (Just (StoredTotpEnrollment "not-an-envelope" (Just 100) Nothing))) (Right False)) [("intent", "confirm"), ("code", "123456")] 422 (Just "mfa-code") "Ese registro ya no se puede confirmar"
      expectSpanish (mfaStoreFor (Right True) (Right Nothing) (Right False)) [("intent", "other")] 422 Nothing "Elige una accion de registro"
      expect (mfaStoreFor (Right True) (Right (Just (StoredTotpEnrollment encryptedTotpSecret Nothing Nothing))) (Right False)) [("intent", "confirm"), ("code", "000000")] 422 (Just "mfa-code") "That authenticator code is invalid"
      expectSpanish (mfaStoreFor (Right True) (Right (Just (StoredTotpEnrollment encryptedTotpSecret Nothing Nothing))) (Right False)) [("intent", "confirm"), ("code", "000000")] 422 (Just "mfa-code") "Ese codigo de autenticador no es valido"
      let unusedMfaStore = mfaStoreFor (Right True) (Right Nothing) (Right False)
          withSessionStore sessionStore = (workflowFor unusedMfaStore) {accountWorkflowMfaEnrollmentSessionStore = sessionStore}
          unavailableSessionStore =
            MfaEnrollmentSessionStore
              { saveMfaEnrollmentSession = \_ -> pure (error "unexpected MFA-enrollment session save"),
                loadMfaEnrollmentSession = \_ -> pure (Left MfaEnrollmentSessionStoreUnavailable),
                invalidateMfaEnrollmentSession = \_ _ -> pure (error "unexpected MFA-enrollment session invalidation")
              }
          corruptSessionStore =
            MfaEnrollmentSessionStore
              { saveMfaEnrollmentSession = \_ -> pure (error "unexpected MFA-enrollment session save"),
                loadMfaEnrollmentSession = \_ -> pure (Left MfaEnrollmentSessionStoreCorruptData),
                invalidateMfaEnrollmentSession = \_ _ -> pure (error "unexpected MFA-enrollment session invalidation")
              }
          missingSessionStore =
            MfaEnrollmentSessionStore
              { saveMfaEnrollmentSession = \_ -> pure (error "unexpected MFA-enrollment session save"),
                loadMfaEnrollmentSession = \_ -> pure (Right Nothing),
                invalidateMfaEnrollmentSession = \_ _ -> pure (error "unexpected MFA-enrollment session invalidation")
              }
          expiredSessionStore =
            MfaEnrollmentSessionStore
              { saveMfaEnrollmentSession = \_ -> pure (error "unexpected MFA-enrollment session save"),
                loadMfaEnrollmentSession = \_ ->
                  pure
                    ( Right
                        ( Just
                            Session.OpaqueSession
                              { Session.sessionId = enrollmentSessionIdValue,
                                Session.sessionPrincipal = accountId,
                                Session.sessionCsrfToken = enrollmentCsrfTokenValue,
                                Session.sessionIssuedAtNanoseconds = 0,
                                Session.sessionExpiresAtNanoseconds = 500
                              }
                        )
                    ),
                invalidateMfaEnrollmentSession = \_ _ -> pure (error "unexpected MFA-enrollment session invalidation")
              }
      handleAccountAction (withSessionStore unavailableSessionStore) (request [("intent", "start")])
        >>= (`shouldSatisfy` actionHasStatusAndFocus 403 Nothing "invalid or has expired")
      handleAccountAction (withSessionStore corruptSessionStore) (request [("intent", "start")])
        >>= (`shouldSatisfy` actionHasStatusAndFocus 403 Nothing "invalid or has expired")
      handleAccountAction (withSessionStore missingSessionStore) (request [("intent", "start")])
        >>= (`shouldSatisfy` actionHasStatusAndFocus 403 Nothing "invalid or has expired")
      handleAccountAction (withSessionStore expiredSessionStore) (request [("intent", "start")])
        >>= (`shouldSatisfy` actionHasStatusAndFocus 403 Nothing "invalid or has expired")
      forM_
        [ (MfaEnrollmentRecoveryCodeHashingFailed, "RecoveryCodeHashingError", "recovery-code hashing failed"),
          (MfaEnrollmentEncryptionFailed, "TotpEncryptionError", "TOTP secret encryption failed")
        ]
        $ \(failureValue, expectedType, expectedDetail) ->
          case mfaEnrollmentFailureDiagnostics AppEffect.MfaEnrollmentConfirmFailure failureValue of
            Nothing -> expectationFailure "expected infrastructure diagnostics for the MFA failure"
            Just diagnostics -> do
              AppEffect.failureCode diagnostics `shouldBe` AppEffect.MfaEnrollmentConfirmFailure
              AppEffect.failureType diagnostics `shouldBe` expectedType
              AppEffect.failureLogEntries diagnostics `shouldSatisfy` any (Text.isInfixOf expectedDetail)
