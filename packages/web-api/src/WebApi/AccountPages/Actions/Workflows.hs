{-# LANGUAGE OverloadedStrings #-}

-- | Account-action workflows.
--
-- Decision record (AHI-5, 2026-09-11): retain known-account rejection
-- provenance in the existing application login-result algebra and append its
-- closed audit event at this action interpreter. The password/MFA workflow
-- remains the sole owner of whether the account is known; this interpreter
-- neither performs another lookup nor changes the public denial. A failed
-- best-effort append adds only a bounded operational signal and private log
-- entry, preserving the already-denied outcome. Unknown identifiers retain
-- no account subject and produce no audit row. This extends existing login
-- and action boundaries rather than adding an audit middleware or a second
-- authentication workflow.
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
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb qualified
import HarchWeb.Account qualified as Account
import HarchWeb.Email qualified as Email
import HarchWeb.LoginProtection qualified as LoginProtection
import HarchWeb.Observability qualified as Observability
import HarchWeb.Password qualified as Password
import HarchWeb.RecoveryCode qualified as RecoveryCode
import HarchWeb.Secret (encryptSecret)
import HarchWeb.Session
  ( OpaqueSession (..),
    SessionId,
    renderSessionCookie,
    sessionId,
  )
import HarchWeb.Time (UnixTimeNanoseconds)
import HarchWeb.Totp qualified as Totp
import HarchWeb.Username qualified as Username
import Network.HTTP.Types qualified as Http
import WebApi.AccountJwt (AccountJwtIssuer (..))
import WebApi.AccountPages.Actions.Common
import WebApi.AccountPages.Actions.Contract
import WebApi.AccountPages.Actions.Login qualified as Login
import WebApi.AccountPages.Actions.Profile qualified as Profile
import WebApi.AccountPages.Actions.Registration qualified as Registration
import WebApi.AccountPages.FieldIds
  ( loginAuthenticatorCodeId,
    loginIdentifierId,
    loginPasswordId,
    loginProofId,
    loginRecoveryCodeId,
    loginSummaryId,
    mfaCodeId,
  )
import WebApi.AccountPages.Forms
import WebApi.AccountPages.Validation (Validation, invalid, valid, validate3, validationResult)
import WebApi.AccountPrincipal (accountPrincipalAccountId, accountPrincipalSessionId)
import WebApi.ActivityAudit
  ( AccountActivity (..),
    AccountAuditEvent (AccountSessionEnded, AuthenticationRejected),
    ActivityAuditStore (..),
    ActivityAuditStoreError (..),
    AuditAuthenticationStage (..),
    AuditSessionEndReason (ExplicitLogout),
    auditRouteObservationFromTrusted,
  )
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
    LoginStage (..),
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
import WebApi.Route (AppRequestContext (..))
import WebApi.Session
  ( AccountSessionStoreError,
    MfaEnrollmentSessionStore (..),
    MfaEnrollmentSessionStoreError,
    invalidateAccountSession,
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
-- required) — 'WebApi.Profile.loadProfileForPrincipal'/'WebApi.Session.AccountSessionStore'
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
handleMfaEnrollmentSubmission actionRequest submission =
  case requestMfaEnrollmentSessionId (HarchWeb.clientActionContext actionRequest) of
    Nothing -> pure (invalidEnrollmentSessionResponse actionRequest)
    Just enrollmentSessionId -> do
      (now, loadedSession) <- loadMfaEnrollmentSessionNow enrollmentSessionId
      case loadedSession of
        Left storeError -> throwClientActionFailure (invalidEnrollmentSessionResponse actionRequest) MfaEnrollmentSessionFailure "MfaEnrollmentSessionStoreError" (mfaEnrollmentSessionStoreErrorMessage storeError)
        Right Nothing -> pure (invalidEnrollmentSessionResponse actionRequest)
        Right (Just opaqueSession) ->
          if sessionExpiresAtNanoseconds opaqueSession <= now
            then pure (invalidEnrollmentSessionResponse actionRequest)
            else
              let accountId = sessionPrincipal opaqueSession
               in case mfaEnrollmentIntentValue submission of
                    "start" -> startMfaAction actionRequest accountId
                    "confirm" -> confirmMfaAction actionRequest accountId (mfaEnrollmentCodeValue submission)
                    _ -> pure (mfaEnrollmentResponse (accountActionResponseContext actionRequest Http.status422 Nothing []) (MfaEnrollmentForm Nothing [] False (Just (localized actionRequest ChooseEnrollmentAction)) True))

invalidEnrollmentSessionResponse :: AccountActionRequest -> AccountActionResponse
invalidEnrollmentSessionResponse actionRequest =
  mfaEnrollmentResponse
    (accountActionResponseContext actionRequest Http.status403 Nothing [])
    (MfaEnrollmentForm Nothing [] False (Just (localized actionRequest EnrollmentLinkInvalid)) True)

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
    Right (MfaEnrollmentStart secret) -> pure (mfaEnrollmentResponse (accountActionResponseContext actionRequest Http.status200 (Just mfaCodeId) noHeaders) (MfaEnrollmentForm (Just (Totp.renderTotpSecret secret)) [] True (Just (localized actionRequest AddAuthenticatorSecret)) False))
    Left errorValue -> interpretMfaFailure actionRequest MfaEnrollmentStartFailure Nothing errorValue

confirmMfaAction :: AccountActionRequest -> Account.AccountId -> Text -> AccountActionWorkflow
confirmMfaAction actionRequest accountId codeValue =
  case Totp.mkTotpCode codeValue of
    Nothing -> pure (mfaEnrollmentResponse (accountActionResponseContext actionRequest Http.status422 (Just mfaCodeId) []) (MfaEnrollmentForm Nothing [] True (Just (localized actionRequest EnterAuthenticatorCode)) True))
    Just code -> do
      confirmed <- confirmMfaEnrollmentNow accountId code
      case confirmed of
        Right (MfaEnrollmentConfirmation recoveryCodes) -> pure (mfaEnrollmentResponse (accountActionResponseContext actionRequest Http.status200 Nothing noHeaders) (MfaEnrollmentForm Nothing (map RecoveryCode.recoveryCodeText (toList recoveryCodes)) False (Just (localized actionRequest AuthenticatorEnrolled)) False))
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
          (MfaEnrollmentForm Nothing [] (isJust focusId) (Just (mfaErrorMessage actionRequest errorValue)) True)
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
          Login.handleAcceptedLogin
            Login.AcceptedLoginInput
              { Login.acceptedLoginRequest = actionRequest,
                Login.acceptedLoginIdentifier = identifierValue,
                Login.acceptedLoginProof = proofChoice,
                Login.acceptedLoginNowNanoseconds = nowNanoseconds,
                Login.acceptedLoginAccountId = accountId
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
issueLoginEnrollmentSession :: AccountActionRequest -> Text -> LoginProofChoice -> UnixTimeNanoseconds -> Account.AccountId -> AccountActionWorkflow
issueLoginEnrollmentSession actionRequest identifierValue proofChoice nowNanoseconds accountId = do
  let form message = LoginForm identifierValue (Just proofChoice) (FormStatusMessage (FormStatus message FormStatusFailure))
      response headers = loginResponse (accountActionResponseContext actionRequest Http.status403 Nothing headers) (form (localized actionRequest EnrollAuthenticatorBeforeSignIn))
  issued <- issueMfaEnrollmentSessionNow accountId nowNanoseconds
  case issued of
    Right opaqueSession -> pure (response [HarchWeb.csrfClearCookieHeader, setCookieHeader (renderSessionCookie mfaEnrollmentSessionCookiePolicy (sessionId opaqueSession))])
    Left storeError -> throwClientActionFailure (response []) MfaEnrollmentSessionFailure "MfaEnrollmentSessionStoreError" (mfaEnrollmentSessionStoreErrorMessage storeError)

handleLogout :: AccountActionRequest -> AccountActionWorkflow
handleLogout actionRequest =
  case requestAccountPrincipal (HarchWeb.clientActionContext actionRequest) of
    Nothing -> do
      cookiePolicy <- accountJwtCookiePolicyNow
      pure (logoutResponse (accountActionResponseContext actionRequest Http.status200 Nothing [HarchWeb.csrfClearCookieHeader, setCookieHeader (HarchWeb.clearAuthenticationCookie cookiePolicy)]) (Just (localized actionRequest SignedOut, False)))
    Just principal -> do
      let sessionToken = accountPrincipalSessionId principal
      invalidated <- invalidateAccountSessionNow sessionToken
      case invalidated of
        Left storeError -> throwClientActionFailure (logoutResponse (accountActionResponseContext actionRequest Http.status503 Nothing []) (Just (localized actionRequest SignOutUnavailable, True))) LogoutSessionFailure "AccountSessionStoreError" (sessionStoreErrorMessage storeError)
        Right sessionEnded -> do
          auditFailure <-
            if sessionEnded
              then appendExplicitLogoutAudit actionRequest (accountPrincipalAccountId principal)
              else pure Nothing
          logoutSuccessResponse actionRequest auditFailure

-- | AHI-5 deliberately gives explicit logout a different durability contract
-- from login. Login's session and audit event commit together because no new
-- credential may be issued without its required audit evidence. Logout first
-- revokes the existing durable session; after that committed security change,
-- an unavailable/capacity/corrupt audit append becomes a low-cardinality
-- operational signal, never a reason to leave browser credentials in place.
-- A @False@ invalidation is an already-ended or raced session, not a second
-- logout event. This small action-specific rail is clearer than a misleading
-- "atomic logout" store or an unbounded retry/outbox mechanism.
appendExplicitLogoutAudit :: AccountActionRequest -> Account.AccountId -> AppM publicFailure (Maybe ActivityAuditStoreError)
appendExplicitLogoutAudit actionRequest accountId =
  case explicitLogoutActivity actionRequest accountId of
    Left activityError -> pure (Just activityError)
    Right activity -> do
      workflow <- accountWorkflow
      appendResult <- liftIO (appendAccountActivity (accountWorkflowActivityAuditStore workflow) activity)
      pure (either Just (const Nothing) appendResult)

explicitLogoutActivity :: AccountActionRequest -> Account.AccountId -> Either ActivityAuditStoreError AccountActivity
explicitLogoutActivity actionRequest accountId = do
  requestId <- maybe (Left ActivityAuditCorruptResult) Right (requestCorrelationId context)
  route <- traverse (either (const (Left ActivityAuditCorruptResult)) Right . auditRouteObservationFromTrusted) (requestRouteObservation context)
  pure
    AccountActivity
      { activitySubject = accountId,
        activityRequestId = requestId,
        activityEvent = AccountSessionEnded ExplicitLogout,
        activityRoute = route
      }
  where
    context = HarchWeb.clientActionContext actionRequest

logoutSuccessResponse :: AccountActionRequest -> Maybe ActivityAuditStoreError -> AccountActionWorkflow
logoutSuccessResponse actionRequest auditFailure = do
  cookiePolicy <- accountJwtCookiePolicyNow
  let response =
        logoutResponse
          (accountActionResponseContext actionRequest Http.status200 Nothing [HarchWeb.csrfClearCookieHeader, setCookieHeader (HarchWeb.clearAuthenticationCookie cookiePolicy)])
          (Just (localized actionRequest SignedOut, False))
  pure (maybe response (`attachLogoutAuditFailure` response) auditFailure)

attachLogoutAuditFailure :: ActivityAuditStoreError -> AccountActionResponse -> AccountActionResponse
attachLogoutAuditFailure = attachBestEffortAuditFailure LogoutAuditAppend

data BestEffortAuditOperation
  = LogoutAuditAppend
  | KnownAuthenticationRejectionAudit

attachBestEffortAuditFailure :: BestEffortAuditOperation -> ActivityAuditStoreError -> AccountActionResponse -> AccountActionResponse
attachBestEffortAuditFailure operation storeError response =
  response
    { HarchWeb.clientActionObservabilityAttributes =
        HarchWeb.clientActionObservabilityAttributes response
          <> [ Observability.ObservabilityAttribute (bestEffortAuditSignalName operation) (Observability.TextAttribute "true"),
               Observability.ObservabilityAttribute "account.audit.operation" (Observability.TextAttribute (bestEffortAuditOperationName operation)),
               Observability.ObservabilityAttribute "account.audit.failure-kind" (Observability.TextAttribute (auditFailureKind storeError))
             ]
          <> capacityExceededSignal storeError,
      HarchWeb.clientActionLogEntries =
        HarchWeb.clientActionLogEntries response
          <> ["[" <> bestEffortAuditLogName operation <> "] audit.operation=" <> bestEffortAuditOperationName operation <> " audit.failure-kind=" <> auditFailureKind storeError]
          <> capacityExceededLog storeError
    }

bestEffortAuditSignalName :: BestEffortAuditOperation -> Text
bestEffortAuditSignalName operation =
  case operation of
    LogoutAuditAppend -> "app.operational.signal.account.logout.audit-append-failed"
    KnownAuthenticationRejectionAudit -> "app.operational.signal.account.authentication-rejection.audit-append-failed"

bestEffortAuditLogName :: BestEffortAuditOperation -> Text
bestEffortAuditLogName operation =
  case operation of
    LogoutAuditAppend -> "account.logout.audit-append-failed"
    KnownAuthenticationRejectionAudit -> "account.authentication-rejection.audit-append-failed"

bestEffortAuditOperationName :: BestEffortAuditOperation -> Text
bestEffortAuditOperationName operation =
  case operation of
    LogoutAuditAppend -> "append"
    KnownAuthenticationRejectionAudit -> "authentication-rejection"

auditFailureKind :: ActivityAuditStoreError -> Text
auditFailureKind storeError =
  case storeError of
    ActivityAuditUnavailable -> "unavailable"
    ActivityAuditCapacityExceeded -> "capacity-exhausted"
    ActivityAuditCorruptResult -> "corrupt-result"

capacityExceededSignal :: ActivityAuditStoreError -> [Observability.ObservabilityAttribute]
capacityExceededSignal storeError =
  case storeError of
    ActivityAuditCapacityExceeded -> [Observability.ObservabilityAttribute "app.operational.signal.audit_capacity_exceeded" (Observability.TextAttribute "true")]
    ActivityAuditUnavailable -> []
    ActivityAuditCorruptResult -> []

capacityExceededLog :: ActivityAuditStoreError -> [Text]
capacityExceededLog storeError =
  case storeError of
    ActivityAuditCapacityExceeded -> ["[audit_capacity_exceeded] audit.operation=append"]
    ActivityAuditUnavailable -> []
    ActivityAuditCorruptResult -> []

setCookieHeader :: Text -> Http.Header
setCookieHeader cookie = ("Set-Cookie", TextEncoding.encodeUtf8 cookie)

accountJwtCookiePolicyNow :: AppM publicFailure HarchWeb.AuthenticationCookiePolicy
accountJwtCookiePolicyNow = accountJwtCookie . accountWorkflowJwtIssuer <$> accountWorkflow

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
