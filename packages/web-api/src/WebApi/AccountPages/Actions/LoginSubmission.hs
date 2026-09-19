{-# LANGUAGE OverloadedStrings #-}

-- | Password/MFA login-submission interpretation.
--
-- Decision record (AHI-5-WF2, 2026-09-19): keep the public action façade and
-- its 'AccountActionWorkflow'/'AppM' failure rail, but give the complete
-- account-login submission concern one internal owner.  Its explicit input
-- keeps request context and user fields together while this module preserves
-- the public denial, unknown-identifier no-row guarantee, trusted request and
-- route attribution, and bounded best-effort audit signal.
module WebApi.AccountPages.Actions.LoginSubmission
  ( LoginWorkflowInput (..),
    handleLoginWorkflow,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb qualified
import HarchWeb.Account qualified as Account
import HarchWeb.Email qualified as Email
import HarchWeb.LoginProtection qualified as LoginProtection
import HarchWeb.Password qualified as Password
import HarchWeb.RecoveryCode qualified as RecoveryCode
import HarchWeb.Session (renderSessionCookie, sessionId)
import HarchWeb.Time (UnixTimeNanoseconds)
import HarchWeb.Totp qualified as Totp
import HarchWeb.Username qualified as Username
import Network.HTTP.Types qualified as Http
import WebApi.AccountPages.Actions.Common
import WebApi.AccountPages.Actions.Contract (LoginSubmission (..))
import WebApi.AccountPages.Actions.Login qualified as AcceptedLogin
import WebApi.AccountPages.FieldIds
  ( loginAuthenticatorCodeId,
    loginIdentifierId,
    loginPasswordId,
    loginProofId,
    loginRecoveryCodeId,
    loginSummaryId,
  )
import WebApi.AccountPages.Forms
import WebApi.AccountPages.Validation (Validation, invalid, valid, validate3, validationResult)
import WebApi.ActivityAudit
  ( AccountActivity (..),
    AccountAuditEvent (AuthenticationRejected),
    ActivityAuditStore (..),
    ActivityAuditStoreError (..),
    AuditAuthenticationStage (..),
    auditRouteObservationFromTrusted,
  )
import WebApi.AppEffect
  ( AccountWorkflow (..),
    AppM,
    FailureCode (..),
  )
import WebApi.Localization (AppMessage (..))
import WebApi.Login
  ( LoginIdentifier (..),
    LoginStage (..),
    LoginThrottleContext (..),
    MfaLoginProof (..),
    PasswordLoginEnvironment (..),
    PasswordMfaLoginResult (..),
    SecondFactorContext (..),
    completePasswordLoginWithIdentifier,
    defaultPasswordRehasher,
  )
import WebApi.Route (requestClientAddress, requestCorrelationId, requestRouteObservation)
import WebApi.Session (mfaEnrollmentSessionCookiePolicy)

data LoginWorkflowInput = LoginWorkflowInput
  { loginWorkflowRequest :: AccountActionRequest,
    loginWorkflowSubmission :: LoginSubmission
  }

handleLoginWorkflow :: LoginWorkflowInput -> AccountActionWorkflow
handleLoginWorkflow input =
  handleLoginSubmission (loginWorkflowRequest input) (loginWorkflowSubmission input)

handleLoginSubmission :: AccountActionRequest -> LoginSubmission -> AccountActionWorkflow
handleLoginSubmission actionRequest submission =
  case parseLoginForm actionRequest submission of
    Left response -> pure response
    Right (identifierValue, proofChoice, passwordValue, identifier, proof) -> do
      (nowNanoseconds, loginResult) <- completePasswordLoginNow actionRequest identifier passwordValue proof
      interpretLoginResult actionRequest identifierValue proofChoice nowNanoseconds loginResult

completePasswordLoginNow :: AccountActionRequest -> LoginIdentifier -> Text -> MfaLoginProof -> AppM publicFailure (UnixTimeNanoseconds, PasswordMfaLoginResult)
completePasswordLoginNow actionRequest identifier passwordValue proof = do
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
                        loginThrottleClientAddress = requestClientAddress (HarchWeb.clientActionContext actionRequest),
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
  Either AccountActionResponse (Text, LoginProofChoice, Text, LoginIdentifier, MfaLoginProof)
parseLoginForm actionRequest submission =
  let identifierValue = loginIdentifierValue submission
      proofChoice = loginProofChoiceValue submission
      parsed =
        validate3
          (\identifier password (selectedChoice, proof) -> (identifierValue, selectedChoice, password, identifier, proof))
          (validateLoginIdentifier identifierValue)
          (validateLoginPassword (loginPasswordValue submission))
          (validateLoginProof submission)
   in case validationResult parsed of
        Left errors ->
          Left
            ( loginResponse
                (accountActionResponseContext actionRequest Http.status422 (Just (loginValidationFocus errors)) [])
                (LoginForm identifierValue proofChoice (FormRejected errors))
            )
        Right validLogin -> Right validLogin

validateLoginIdentifier :: Text -> Validation LoginValidationError LoginIdentifier
validateLoginIdentifier identifierValue =
  case (LoginEmailAddress <$> Email.mkEmailAddress identifierValue) <|> (LoginUsername <$> Username.mkUsername identifierValue) of
    Nothing -> invalid LoginIdentifierInvalid
    Just identifier -> valid identifier

validateLoginPassword :: Text -> Validation LoginValidationError Text
validateLoginPassword passwordValue =
  if validPassword passwordValue then valid passwordValue else invalid LoginPasswordMissing

validateLoginProof :: LoginSubmission -> Validation LoginValidationError (LoginProofChoice, MfaLoginProof)
validateLoginProof submission =
  case loginProofChoiceValue submission of
    Nothing -> invalid LoginProofMissing
    Just LoginAuthenticatorProof ->
      case Totp.mkTotpCode (loginTotpCodeValue submission) of
        Nothing -> invalid LoginAuthenticatorCodeInvalid
        Just code -> valid (LoginAuthenticatorProof, TotpLoginProof code)
    Just LoginRecoveryProof ->
      case RecoveryCode.mkRecoveryCode (loginRecoveryCodeValue submission) of
        Nothing -> invalid LoginRecoveryCodeInvalid
        Just code -> valid (LoginRecoveryProof, RecoveryCodeLoginProof code)

loginValidationFocus :: NonEmpty LoginValidationError -> HarchWeb.ElementId
loginValidationFocus (single :| []) = loginValidationErrorId single
loginValidationFocus _ = loginSummaryId

loginValidationErrorId :: LoginValidationError -> HarchWeb.ElementId
loginValidationErrorId validationError =
  case validationError of
    LoginIdentifierInvalid -> loginIdentifierId
    LoginPasswordMissing -> loginPasswordId
    LoginProofMissing -> loginProofId
    LoginAuthenticatorCodeInvalid -> loginAuthenticatorCodeId
    LoginRecoveryCodeInvalid -> loginRecoveryCodeId

interpretLoginResult ::
  AccountActionRequest ->
  Text ->
  LoginProofChoice ->
  UnixTimeNanoseconds ->
  PasswordMfaLoginResult ->
  AccountActionWorkflow
interpretLoginResult actionRequest identifierValue proofChoice nowNanoseconds loginResult =
  let loginForm message statusKind = LoginForm identifierValue (Just proofChoice) (FormStatusMessage (FormStatus message statusKind))
      response status message statusKind focusId headers = loginResponse (accountActionResponseContext actionRequest status focusId headers) (loginForm message statusKind)
      unavailable focusId = response Http.status503 (localized actionRequest SignInUnavailable) FormStatusFailure focusId []
      proofFocus = loginProofFocusId proofChoice
   in case loginResult of
        PasswordMfaLoginAccepted accountId ->
          AcceptedLogin.handleAcceptedLogin
            AcceptedLogin.AcceptedLoginInput
              { AcceptedLogin.acceptedLoginRequest = actionRequest,
                AcceptedLogin.acceptedLoginIdentifier = identifierValue,
                AcceptedLogin.acceptedLoginProof = proofChoice,
                AcceptedLogin.acceptedLoginNowNanoseconds = nowNanoseconds,
                AcceptedLogin.acceptedLoginAccountId = accountId
              }
        PasswordMfaLoginEmailVerificationRequired _ -> pure (response Http.status403 (localized actionRequest VerifyEmailBeforeSignIn) FormStatusFailure Nothing [])
        PasswordMfaLoginEnrollmentRequired accountId -> issueLoginEnrollmentSession actionRequest identifierValue proofChoice nowNanoseconds accountId
        PasswordMfaLoginRejected -> pure (response Http.status422 (localized actionRequest SignInRejected) FormStatusFailure (Just proofFocus) [])
        PasswordMfaLoginKnownAccountRejected accountId loginStage ->
          appendKnownLoginRejection
            actionRequest
            accountId
            loginStage
            (response Http.status422 (localized actionRequest SignInRejected) FormStatusFailure (Just proofFocus) [])
        PasswordMfaLoginThrottled _retryAfterNanoseconds -> pure (response Http.status429 (localized actionRequest SignInThrottled) FormStatusFailure (Just loginIdentifierId) [])
        PasswordMfaLoginCredentialStoreError storeError -> throwClientActionFailure (unavailable (Just loginIdentifierId)) LoginCredentialStoreFailure "AccountCredentialStoreError" (credentialStoreErrorMessage storeError)
        PasswordMfaLoginMfaStoreError storeError -> throwClientActionFailure (unavailable (Just proofFocus)) LoginMfaStoreFailure "MfaStoreError" (mfaStoreErrorMessage storeError)
        PasswordMfaLoginAttemptStoreError storeError -> throwClientActionFailure (unavailable (Just loginIdentifierId)) LoginAttemptStoreFailure "LoginAttemptStoreError" (loginAttemptStoreErrorMessage storeError)
        PasswordMfaLoginPasswordWorkBudgetExhausted -> throwClientActionFailure (unavailable (Just loginIdentifierId)) LoginPasswordWorkBudgetFailure "PasswordWorkBudgetExhausted" "password work budget is exhausted"
        PasswordMfaLoginCorruptEnrollment -> throwClientActionFailure (unavailable (Just proofFocus)) LoginCorruptEnrollmentFailure "CorruptTotpEnrollment" "stored MFA enrollment could not be decoded"

loginProofFocusId :: LoginProofChoice -> HarchWeb.ElementId
loginProofFocusId proofChoice =
  case proofChoice of
    LoginAuthenticatorProof -> loginAuthenticatorCodeId
    LoginRecoveryProof -> loginRecoveryCodeId

appendKnownLoginRejection :: AccountActionRequest -> Account.AccountId -> LoginStage -> AccountActionResponse -> AccountActionWorkflow
appendKnownLoginRejection actionRequest accountId loginStage deniedResponse =
  case knownLoginRejectionActivity actionRequest accountId loginStage of
    Left activityError -> pure (attachBestEffortAuditFailure KnownAuthenticationRejectionAudit activityError deniedResponse)
    Right activity -> do
      workflow <- accountWorkflow
      appendResult <- liftIO (appendAccountActivity (accountWorkflowActivityAuditStore workflow) activity)
      pure (either (\storeError -> attachBestEffortAuditFailure KnownAuthenticationRejectionAudit storeError deniedResponse) (const deniedResponse) appendResult)

knownLoginRejectionActivity :: AccountActionRequest -> Account.AccountId -> LoginStage -> Either ActivityAuditStoreError AccountActivity
knownLoginRejectionActivity actionRequest accountId loginStage = do
  requestId <- maybe (Left ActivityAuditCorruptResult) Right (requestCorrelationId context)
  route <- traverse (either (const (Left ActivityAuditCorruptResult)) Right . auditRouteObservationFromTrusted) (requestRouteObservation context)
  pure
    AccountActivity
      { activitySubject = accountId,
        activityRequestId = requestId,
        activityEvent = AuthenticationRejected (auditAuthenticationStage loginStage),
        activityRoute = route
      }
  where
    context = HarchWeb.clientActionContext actionRequest

auditAuthenticationStage :: LoginStage -> AuditAuthenticationStage
auditAuthenticationStage loginStage =
  case loginStage of
    PasswordLoginStage -> PasswordAuthenticationStage
    SecondFactorLoginStage -> SecondFactorAuthenticationStage

-- | A correct password already proves account ownership even though MFA
-- enrollment is still outstanding, so this is the second legitimate place
-- (with email verification, above) to grant an 'issueMfaEnrollmentSession'
-- instead of a dead-end rejection with no path forward — see the AM
-- decision record on 'handleMfaEnrollmentSubmission' for why this session
-- is deliberately not the same 'issueAccountSession' full login grants.
setCookieHeader :: Text -> Http.Header
setCookieHeader cookie = ("Set-Cookie", TextEncoding.encodeUtf8 cookie)

issueLoginEnrollmentSession :: AccountActionRequest -> Text -> LoginProofChoice -> UnixTimeNanoseconds -> Account.AccountId -> AccountActionWorkflow
issueLoginEnrollmentSession actionRequest identifierValue proofChoice nowNanoseconds accountId = do
  let form message = LoginForm identifierValue (Just proofChoice) (FormStatusMessage (FormStatus message FormStatusFailure))
      response headers = loginResponse (accountActionResponseContext actionRequest Http.status403 Nothing headers) (form (localized actionRequest EnrollAuthenticatorBeforeSignIn))
  issued <- issueMfaEnrollmentSessionNow accountId nowNanoseconds
  case issued of
    Right opaqueSession -> pure (response [HarchWeb.csrfClearCookieHeader, setCookieHeader (renderSessionCookie mfaEnrollmentSessionCookiePolicy (sessionId opaqueSession))])
    Left storeError -> throwClientActionFailure (response []) MfaEnrollmentSessionFailure "MfaEnrollmentSessionStoreError" (mfaEnrollmentSessionStoreErrorMessage storeError)
