{-# LANGUAGE OverloadedStrings #-}

module WebApi.AccountPages.Actions.Workflows
  ( handleRegistrationSubmission,
    handleVerificationSubmission,
    handleMfaEnrollmentSubmission,
    handleLoginSubmission,
    handleLogout,
    handleProfileSubmission,
    mfaEnrollmentFailureDiagnostics,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Crypto.Error (maybeCryptoError)
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb qualified
import HarchWeb.Account qualified as Account
import HarchWeb.Email qualified as Email
import HarchWeb.LoginProtection qualified as LoginProtection
import HarchWeb.Password qualified as Password
import HarchWeb.RecoveryCode qualified as RecoveryCode
import HarchWeb.Secret (encryptSecret)
import HarchWeb.Session
  ( OpaqueSession (..),
    SessionId,
    defaultSessionCookiePolicy,
    renderSessionCookie,
    sessionCookieMaxAgeSeconds,
    sessionId,
  )
import HarchWeb.Time (UnixTimeNanoseconds)
import HarchWeb.Totp qualified as Totp
import HarchWeb.Username qualified as Username
import Network.HTTP.Types qualified as Http
import WebApi.AccountPages.Actions.Common
import WebApi.AccountPages.Actions.Contract
import WebApi.AccountPages.Actions.Profile qualified as Profile
import WebApi.AccountPages.Actions.Registration qualified as Registration
import WebApi.AccountPages.FieldIds
  ( loginCodeId,
    loginEmailId,
    loginPasswordId,
    mfaCodeId,
  )
import WebApi.AccountPages.Forms
import WebApi.AppEffect
  ( AccountWorkflow (..),
    AppFailure (..),
    AppM,
    FailureCode (..),
    FailureDiagnostics,
    throwAppFailure,
  )
import WebApi.Localization (AppMessage (..))
import WebApi.Login
  ( LoginIdentifier (..),
    LoginThrottleContext (..),
    MfaLoginProof (..),
    PasswordLoginEnvironment (..),
    PasswordMfaLoginResult (..),
    SecondFactorContext (..),
    completePasswordLoginWithIdentifier,
    defaultPasswordRehasher,
  )
import WebApi.MfaEnrollment
  ( MfaConfirmationEnvironment (..),
    MfaEnrollmentConfirmation (..),
    MfaEnrollmentEnvironment (..),
    MfaEnrollmentError (..),
    MfaEnrollmentStart (..),
    confirmMfaEnrollment,
    startMfaEnrollment,
  )
import WebApi.Route
  ( AppRequestContext (..),
  )
import WebApi.Session
  ( AccountSessionStoreError,
    MfaEnrollmentSessionStore (..),
    MfaEnrollmentSessionStoreError,
    invalidateAccountSession,
    issueAccountSession,
    mfaEnrollmentSessionCookiePolicy,
  )

handleRegistrationSubmission :: AccountActionRequest -> RegistrationSubmission -> AccountActionWorkflow
handleRegistrationSubmission actionRequest submission =
  Registration.handleRegistrationWorkflow
    Registration.RegistrationWorkflowInput
      { Registration.registrationWorkflowRequest = actionRequest,
        Registration.registrationWorkflowSubmission = submission
      }

handleVerificationSubmission :: AccountActionRequest -> VerificationSubmission -> AccountActionWorkflow
handleVerificationSubmission actionRequest submission =
  Registration.handleVerificationWorkflow
    Registration.VerificationWorkflowInput
      { Registration.verificationWorkflowRequest = actionRequest,
        Registration.verificationWorkflowSubmission = submission
      }

-- | Decision record (AM, 2026-08-14): MFA enrollment previously trusted a
-- client-supplied @account@ form field with no session check at all — see
-- TASKS.md's AM entry for the full vulnerability. The fix binds enrollment
-- to a session principal, per that entry's own instruction, but no session
-- existed at either legitimate handoff point (right after email
-- verification, or after a correct password with enrollment still
-- required) — 'WebApi.Profile.loadProfile'/'WebApi.Session.AccountSessionStore'
-- are login sessions, and issuing one before MFA is confirmed would let a
-- password alone grant everything a completed login grants (Profile access
-- and any future protected resource), silently narrowing what "signed in"
-- means. Per this document's missing-capability protocol, option 1 (a
-- small, general primitive squarely within 'WebApi.Session'\'s existing
-- ownership) is 'WebApi.Session.MfaEnrollmentSessionStore': its own table,
-- store, and 10-minute cookie, granting only enrollment and nothing else.
-- 'handleMfaEnrollmentSubmission' now trusts only that session's principal;
-- the submitted @account@ field is gone entirely (deleted from
-- 'MfaEnrollmentSubmission', 'MfaEnrollmentForm', and the hidden form
-- input), closing the "any 128-bit id" guessing surface named in AM's own
-- text. AN (a confirmed enrollment silently destroyed by simply restarting
-- it) was fixed separately and stays a needed guard even under this
-- session-bound caller: it is what stops the account's own legitimate
-- enrollment session from clobbering an authenticator it already confirmed
-- in an earlier session.
handleMfaEnrollmentSubmission :: AccountActionRequest -> MfaEnrollmentSubmission -> AccountActionWorkflow
handleMfaEnrollmentSubmission actionRequest submission = do
  case requestMfaEnrollmentSessionId (HarchWeb.clientActionContext actionRequest) of
    Nothing -> pure (invalidEnrollmentSessionResponse actionRequest)
    Just enrollmentSessionId -> do
      (now, loadedSession) <- loadMfaEnrollmentSessionNow enrollmentSessionId
      case loadedSession of
        Left storeError -> throwClientActionFailure (invalidEnrollmentSessionResponse actionRequest) MfaEnrollmentSessionFailure "MfaEnrollmentSessionStoreError" (mfaEnrollmentSessionStoreErrorMessage storeError)
        Right Nothing -> pure (invalidEnrollmentSessionResponse actionRequest)
        Right (Just opaqueSession)
          | sessionExpiresAtNanoseconds opaqueSession <= now -> pure (invalidEnrollmentSessionResponse actionRequest)
          | otherwise ->
              let accountId = sessionPrincipal opaqueSession
               in case mfaEnrollmentIntentValue submission of
                    "start" -> startMfaAction actionRequest accountId
                    "confirm" -> confirmMfaAction actionRequest accountId (mfaEnrollmentCodeValue submission)
                    _ -> pure (mfaEnrollmentResponse (accountActionResponseContext actionRequest Http.status422 Nothing []) (MfaEnrollmentForm Nothing [] (Just (localized actionRequest ChooseEnrollmentAction)) True))

invalidEnrollmentSessionResponse :: AccountActionRequest -> HarchWeb.ClientActionResponse
invalidEnrollmentSessionResponse actionRequest =
  mfaEnrollmentResponse
    (accountActionResponseContext actionRequest Http.status403 Nothing [])
    (MfaEnrollmentForm Nothing [] (Just (localized actionRequest EnrollmentLinkInvalid)) True)

loadMfaEnrollmentSessionNow :: SessionId -> AppM publicFailure (UnixTimeNanoseconds, Either MfaEnrollmentSessionStoreError (Maybe (OpaqueSession Account.AccountId)))
loadMfaEnrollmentSessionNow enrollmentSessionId = do
  workflow <- accountWorkflow
  liftIO $ do
    now <- accountWorkflowClock workflow
    loadedSession <- loadMfaEnrollmentSession (accountWorkflowMfaEnrollmentSessionStore workflow) enrollmentSessionId
    pure (now, loadedSession)

startMfaEnrollmentNow :: Account.AccountId -> AppM publicFailure (Either MfaEnrollmentError MfaEnrollmentStart)
startMfaEnrollmentNow accountId = do
  workflow <- accountWorkflow
  liftIO $ do
    now <- accountWorkflowClock workflow
    startMfaEnrollment
      MfaEnrollmentEnvironment
        { mfaEnrollmentGenerateSecret = Totp.generateTotpSecret,
          mfaEnrollmentEncryptSecret = \encryptionKey plaintext -> maybeCryptoError <$> encryptSecret encryptionKey plaintext,
          mfaEnrollmentStore = accountWorkflowMfaStore workflow,
          mfaEnrollmentEncryptionKey = accountWorkflowTotpEncryptionKey workflow,
          mfaEnrollmentNowNanoseconds = now
        }
      accountId

startMfaAction :: AccountActionRequest -> Account.AccountId -> AccountActionWorkflow
startMfaAction actionRequest accountId = do
  started <- startMfaEnrollmentNow accountId
  case started of
    Right (MfaEnrollmentStart secret) -> pure (mfaEnrollmentResponse (accountActionResponseContext actionRequest Http.status200 (Just mfaCodeId) noHeaders) (MfaEnrollmentForm (Just (Totp.renderTotpSecret secret)) [] (Just (localized actionRequest AddAuthenticatorSecret)) False))
    Left errorValue -> interpretMfaFailure actionRequest MfaEnrollmentStartFailure Nothing errorValue

confirmMfaAction :: AccountActionRequest -> Account.AccountId -> Text -> AccountActionWorkflow
confirmMfaAction actionRequest accountId codeValue =
  case Totp.mkTotpCode codeValue of
    Nothing -> pure (mfaEnrollmentResponse (accountActionResponseContext actionRequest Http.status422 (Just mfaCodeId) []) (MfaEnrollmentForm Nothing [] (Just (localized actionRequest EnterAuthenticatorCode)) True))
    Just code -> do
      confirmed <- confirmMfaEnrollmentNow accountId code
      case confirmed of
        Right (MfaEnrollmentConfirmation recoveryCodes) -> pure (mfaEnrollmentResponse (accountActionResponseContext actionRequest Http.status200 Nothing noHeaders) (MfaEnrollmentForm Nothing (map RecoveryCode.recoveryCodeText (toList recoveryCodes)) (Just (localized actionRequest AuthenticatorEnrolled)) False))
        Left errorValue -> interpretMfaFailure actionRequest MfaEnrollmentConfirmFailure (Just mfaCodeId) errorValue

confirmMfaEnrollmentNow :: Account.AccountId -> Totp.TotpCode -> AppM publicFailure (Either MfaEnrollmentError MfaEnrollmentConfirmation)
confirmMfaEnrollmentNow accountId code = do
  workflow <- accountWorkflow
  liftIO $ do
    now <- accountWorkflowClock workflow
    confirmMfaEnrollment
      MfaConfirmationEnvironment
        { mfaConfirmationGenerateCode = RecoveryCode.generateRecoveryCode,
          mfaConfirmationHashCode = RecoveryCode.hashRecoveryCode Password.defaultPasswordHashingPolicy,
          mfaConfirmationStore = accountWorkflowMfaStore workflow,
          mfaConfirmationEncryptionKey = accountWorkflowTotpEncryptionKey workflow,
          mfaConfirmationNowNanoseconds = now,
          mfaConfirmationNowSeconds = accountWorkflowTotpClock workflow now
        }
      accountId
      code

interpretMfaFailure ::
  AccountActionRequest ->
  FailureCode ->
  Maybe HarchWeb.ElementId ->
  MfaEnrollmentError ->
  AccountActionWorkflow
interpretMfaFailure actionRequest failureCodeValue focusId errorValue =
  let response status =
        mfaEnrollmentResponse
          (accountActionResponseContext actionRequest status focusId [])
          (MfaEnrollmentForm Nothing [] (Just (mfaErrorMessage actionRequest errorValue)) True)
   in case mfaEnrollmentFailureDiagnostics failureCodeValue errorValue of
        Nothing -> pure (response Http.status422)
        Just diagnostics -> throwAppFailure AppFailure {appFailurePublic = response Http.status503, appFailureDiagnostics = diagnostics}

mfaEnrollmentFailureDiagnostics :: FailureCode -> MfaEnrollmentError -> Maybe FailureDiagnostics
mfaEnrollmentFailureDiagnostics failureCodeValue errorValue =
  case errorValue of
    MfaEnrollmentStoreError storeError -> Just (failureDiagnostics "MfaStoreError" (mfaStoreErrorMessage storeError))
    MfaEnrollmentCorruptSecret -> Just (failureDiagnostics "CorruptTotpEnrollment" "stored TOTP secret could not be decoded")
    MfaEnrollmentRecoveryCodeHashingFailed -> Just (failureDiagnostics "RecoveryCodeHashingError" "recovery-code hashing failed")
    MfaEnrollmentEncryptionFailed -> Just (failureDiagnostics "TotpEncryptionError" "TOTP secret encryption failed")
    _ -> Nothing
  where
    failureDiagnostics = buildFailureDiagnostics failureCodeValue

handleLoginSubmission :: AccountActionRequest -> LoginSubmission -> AccountActionWorkflow
handleLoginSubmission actionRequest submission =
  case parseLoginForm actionRequest submission of
    Left response -> pure response
    Right (emailValue, passwordValue, identifier, proof) -> do
      (nowNanoseconds, loginResult) <- completePasswordLoginNow identifier passwordValue proof
      interpretLoginResult actionRequest emailValue nowNanoseconds loginResult

completePasswordLoginNow :: LoginIdentifier -> Text -> MfaLoginProof -> AppM publicFailure (UnixTimeNanoseconds, PasswordMfaLoginResult)
completePasswordLoginNow identifier passwordValue proof = do
  workflow <- accountWorkflow
  liftIO $ do
    nowNanoseconds <- accountWorkflowClock workflow
    loginResult <-
      completePasswordLoginWithIdentifier
        SecondFactorContext
          { secondFactorPasswordLoginEnvironment =
              PasswordLoginEnvironment
                { passwordLoginCredentialStore = accountWorkflowCredentialStore workflow,
                  passwordLoginMfaStore = accountWorkflowMfaStore workflow,
                  passwordLoginThrottle =
                    LoginThrottleContext
                      { loginThrottleStore = accountWorkflowLoginAttemptStore workflow,
                        loginThrottlePolicy = LoginProtection.defaultLoginProtectionPolicy,
                        loginThrottleNow = nowNanoseconds
                      },
                  passwordLoginWorkGate = accountWorkflowPasswordWorkGate workflow,
                  passwordLoginRehasher = defaultPasswordRehasher
                },
            secondFactorEncryptionKey = accountWorkflowTotpEncryptionKey workflow,
            secondFactorNowNanoseconds = nowNanoseconds,
            secondFactorNowSeconds = accountWorkflowTotpClock workflow nowNanoseconds,
            secondFactorProof = proof
          }
        identifier
        (Password.mkPassword passwordValue)
    pure (nowNanoseconds, loginResult)

parseLoginForm ::
  AccountActionRequest ->
  LoginSubmission ->
  Either HarchWeb.ClientActionResponse (Text, Text, LoginIdentifier, MfaLoginProof)
parseLoginForm actionRequest submission =
  let emailValue = loginEmailValue submission
      usernameValue = loginUsernameValue submission
      passwordValue = loginPasswordValue submission
      loginForm message = LoginForm emailValue (Just message)
      maybeIdentifier =
        (LoginEmailAddress <$> Email.mkEmailAddress emailValue)
          <|> (LoginUsername <$> Username.mkUsername emailValue)
          <|> (LoginUsername <$> Username.mkUsername usernameValue)
   in case (maybeIdentifier, validPassword passwordValue, loginProof submission) of
        (Nothing, _, _) -> Left (loginResponse (accountActionResponseContext actionRequest Http.status422 (Just loginEmailId) []) (loginForm (localized actionRequest EnterValidEmailOrUsername) True))
        (_, False, _) -> Left (loginResponse (accountActionResponseContext actionRequest Http.status422 (Just loginPasswordId) []) (loginForm (localized actionRequest EnterPassword) True))
        (_, _, Nothing) -> Left (loginResponse (accountActionResponseContext actionRequest Http.status422 (Just loginCodeId) []) (loginForm (localized actionRequest EnterAuthenticatorOrRecoveryCode) True))
        (Just identifier, True, Just proof) -> Right (if Text.null emailValue then usernameValue else emailValue, passwordValue, identifier, proof)

interpretLoginResult ::
  AccountActionRequest ->
  Text ->
  UnixTimeNanoseconds ->
  PasswordMfaLoginResult ->
  AccountActionWorkflow
interpretLoginResult actionRequest emailValue nowNanoseconds loginResult =
  let loginForm message = LoginForm emailValue (Just message)
      response status message isError focusId headers = loginResponse (accountActionResponseContext actionRequest status focusId headers) (loginForm message isError)
      unavailable focusId = response Http.status503 (localized actionRequest SignInUnavailable) True focusId []
   in case loginResult of
        PasswordMfaLoginAccepted accountId -> issueLoginSession actionRequest emailValue nowNanoseconds accountId
        PasswordMfaLoginEmailVerificationRequired _ -> pure (response Http.status403 (localized actionRequest VerifyEmailBeforeSignIn) True Nothing [])
        PasswordMfaLoginEnrollmentRequired accountId -> issueLoginEnrollmentSession actionRequest emailValue nowNanoseconds accountId
        PasswordMfaLoginRejected -> pure (response Http.status422 (localized actionRequest SignInRejected) True (Just loginCodeId) [])
        PasswordMfaLoginThrottled _retryAfterNanoseconds -> pure (response Http.status429 (localized actionRequest SignInThrottled) True (Just loginEmailId) [])
        PasswordMfaLoginCredentialStoreError storeError -> throwClientActionFailure (unavailable (Just loginEmailId)) LoginCredentialStoreFailure "AccountCredentialStoreError" (credentialStoreErrorMessage storeError)
        PasswordMfaLoginMfaStoreError storeError -> throwClientActionFailure (unavailable (Just loginCodeId)) LoginMfaStoreFailure "MfaStoreError" (mfaStoreErrorMessage storeError)
        PasswordMfaLoginAttemptStoreError storeError -> throwClientActionFailure (unavailable (Just loginEmailId)) LoginAttemptStoreFailure "LoginAttemptStoreError" (loginAttemptStoreErrorMessage storeError)
        PasswordMfaLoginPasswordWorkBudgetExhausted -> throwClientActionFailure (unavailable (Just loginEmailId)) LoginPasswordWorkBudgetFailure "PasswordWorkBudgetExhausted" "password work budget is exhausted"
        PasswordMfaLoginCorruptEnrollment -> throwClientActionFailure (unavailable (Just loginCodeId)) LoginCorruptEnrollmentFailure "CorruptTotpEnrollment" "stored MFA enrollment could not be decoded"

issueLoginSession :: AccountActionRequest -> Text -> UnixTimeNanoseconds -> Account.AccountId -> AccountActionWorkflow
issueLoginSession actionRequest emailValue nowNanoseconds accountId = do
  issuedSession <- issueAccountSessionNow accountId nowNanoseconds
  let form message = LoginForm emailValue (Just message)
  case issuedSession of
    Left storeError -> throwClientActionFailure (loginResponse (accountActionResponseContext actionRequest Http.status503 (Just loginEmailId) []) (form (localized actionRequest SignInUnavailable) True)) LoginSessionFailure "AccountSessionStoreError" (sessionStoreErrorMessage storeError)
    Right opaqueSession -> pure (loginResponse (accountActionResponseContext actionRequest Http.status200 Nothing [("Set-Cookie", TextEncoding.encodeUtf8 (renderSessionCookie defaultSessionCookiePolicy (sessionId opaqueSession)))]) (form (localized actionRequest SignedIn) False))

issueAccountSessionNow :: Account.AccountId -> UnixTimeNanoseconds -> AppM publicFailure (Either AccountSessionStoreError (OpaqueSession Account.AccountId))
issueAccountSessionNow accountId nowNanoseconds = do
  workflow <- accountWorkflow
  liftIO (issueAccountSession (accountWorkflowSessionStore workflow) accountId nowNanoseconds)

-- | A correct password already proves account ownership even though MFA
-- enrollment is still outstanding, so this is the second legitimate place
-- (with email verification, above) to grant an 'issueMfaEnrollmentSession'
-- instead of a dead-end rejection with no path forward — see the AM
-- decision record on 'handleMfaEnrollmentSubmission' for why this session
-- is deliberately not the same 'issueAccountSession' full login grants.
issueLoginEnrollmentSession :: AccountActionRequest -> Text -> UnixTimeNanoseconds -> Account.AccountId -> AccountActionWorkflow
issueLoginEnrollmentSession actionRequest emailValue nowNanoseconds accountId = do
  let form message = LoginForm emailValue (Just message)
      response headers = loginResponse (accountActionResponseContext actionRequest Http.status403 Nothing headers) (form (localized actionRequest EnrollAuthenticatorBeforeSignIn) True)
  issued <- issueMfaEnrollmentSessionNow accountId nowNanoseconds
  case issued of
    Right opaqueSession -> pure (response [("Set-Cookie", TextEncoding.encodeUtf8 (renderSessionCookie mfaEnrollmentSessionCookiePolicy (sessionId opaqueSession)))])
    Left storeError -> throwClientActionFailure (response []) MfaEnrollmentSessionFailure "MfaEnrollmentSessionStoreError" (mfaEnrollmentSessionStoreErrorMessage storeError)

handleLogout :: AccountActionRequest -> AccountActionWorkflow
handleLogout actionRequest =
  case requestSessionId (HarchWeb.clientActionContext actionRequest) of
    Nothing -> pure (logoutResponse (accountActionResponseContext actionRequest Http.status200 Nothing []) (Just (localized actionRequest SignedOut, False)))
    Just sessionToken -> do
      invalidated <- invalidateAccountSessionNow sessionToken
      case invalidated of
        Left storeError -> throwClientActionFailure (logoutResponse (accountActionResponseContext actionRequest Http.status503 Nothing []) (Just (localized actionRequest SignOutUnavailable, True))) LogoutSessionFailure "AccountSessionStoreError" (sessionStoreErrorMessage storeError)
        Right _ -> pure (logoutResponse (accountActionResponseContext actionRequest Http.status200 Nothing [("Set-Cookie", TextEncoding.encodeUtf8 (renderSessionCookie (defaultSessionCookiePolicy {sessionCookieMaxAgeSeconds = 0}) sessionToken))]) (Just (localized actionRequest SignedOut, False)))

invalidateAccountSessionNow :: SessionId -> AppM publicFailure (Either AccountSessionStoreError Bool)
invalidateAccountSessionNow sessionToken = do
  workflow <- accountWorkflow
  liftIO $ do
    now <- accountWorkflowClock workflow
    invalidateAccountSession (accountWorkflowSessionStore workflow) sessionToken now

handleProfileSubmission :: AccountActionRequest -> ProfileSubmission -> AccountActionWorkflow
handleProfileSubmission actionRequest submission =
  Profile.handleProfileWorkflow
    Profile.ProfileWorkflowInput
      { Profile.profileWorkflowRequest = actionRequest,
        Profile.profileWorkflowSubmission = submission
      }

loginProof :: LoginSubmission -> Maybe MfaLoginProof
loginProof submission =
  case loginProofValue submission of
    "totp" -> TotpLoginProof <$> Totp.mkTotpCode (loginCodeValue submission)
    "recovery" -> RecoveryCodeLoginProof <$> RecoveryCode.mkRecoveryCode (loginCodeValue submission)
    _ -> Nothing
