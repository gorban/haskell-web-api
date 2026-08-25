{-# LANGUAGE LambdaCase #-}
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
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb qualified
import HarchWeb.Account qualified as Account
import HarchWeb.Email qualified as Email
import HarchWeb.LoginProtection qualified as LoginProtection
import HarchWeb.Observability qualified as Observability
import HarchWeb.Password qualified as Password
import HarchWeb.RecoveryCode qualified as RecoveryCode
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
import WebApi.Account
  ( AccountProfile (..),
    AccountStoreError,
    RegistrationEnvironment (..),
    RegistrationError (..),
    RegistrationRequest (..),
    RegistrationResult (..),
    ResendVerificationError (..),
    VerificationDeliveryFailure (..),
    confirmEmailVerificationAt,
    defaultPendingRegistrationStoragePolicy,
    registerAccount,
    resendEmailVerificationAt,
  )
import WebApi.AccountPages.Actions.Common
import WebApi.AccountPages.Actions.Contract
import WebApi.AccountPages.Forms
import WebApi.AppEffect
  ( AccountWorkflow (..),
    AppFailure (..),
    AppM,
    FailureCode (..),
    FailureDiagnostics,
    throwAppFailure,
  )
import WebApi.Login
  ( LoginIdentifier (..),
    LoginThrottleContext (..),
    MfaLoginProof (..),
    PasswordMfaLoginResult (..),
    SecondFactorContext (..),
    completePasswordLoginWithIdentifier,
  )
import WebApi.MfaEnrollment
  ( MfaEnrollmentConfirmation (..),
    MfaEnrollmentError (..),
    MfaEnrollmentStart (..),
    confirmMfaEnrollment,
    startMfaEnrollment,
  )
import WebApi.Profile
  ( ProfileLoadError,
    ProfileState (..),
    loadProfile,
  )
import WebApi.Route
  ( AppRequestContext (..),
  )
import WebApi.Session
  ( AccountSessionStore (..),
    AccountSessionStoreError,
    MfaEnrollmentSessionStore (..),
    MfaEnrollmentSessionStoreError,
    issueAccountSession,
    issueMfaEnrollmentSession,
    mfaEnrollmentSessionCookiePolicy,
  )

type AccountActionRequest = HarchWeb.ClientActionRequest AccountAction AppRequestContext

type AccountActionWorkflow = AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse

type ParsedRegistration = (Text, Text, Text, Text, Username.Username, Email.EmailAddress)

handleRegistrationSubmission :: AccountActionRequest -> RegistrationSubmission -> AccountActionWorkflow
handleRegistrationSubmission actionRequest submission =
  case parseRegistrationForm actionRequest submission of
    Left response -> pure response
    Right registration -> do
      registrationResult <- registerAccountNow actionRequest registration
      let (usernameValue, emailValue, displayNameValue, _, _, _) = registration
      interpretRegistrationResult actionRequest usernameValue emailValue displayNameValue registrationResult

-- | Registration is one application operation: it obtains one clock value
-- and invokes the account service with dependencies selected for this
-- request. This keeps the workflow from splitting that operation across
-- generic IO lifts.
registerAccountNow ::
  AccountActionRequest ->
  ParsedRegistration ->
  AppM publicFailure (Either RegistrationError RegistrationResult)
registerAccountNow actionRequest (_, _, displayNameValue, passwordValue, username, emailAddress) = do
  workflow <- accountWorkflow
  liftIO $ do
    now <- accountWorkflowClock workflow
    registerAccount
      RegistrationEnvironment
        { registrationPasswordHasher = accountWorkflowPasswordHasher workflow,
          registrationHashingPolicy = Password.defaultPasswordHashingPolicy,
          registrationPasswordWorkGate = accountWorkflowPasswordWorkGate workflow,
          registrationStoragePolicy = defaultPendingRegistrationStoragePolicy,
          registrationDeliveryTimeout = accountWorkflowRegistrationDeliveryTimeout workflow,
          registrationStore = accountWorkflowStore workflow,
          registrationDelivery = accountWorkflowEmailDelivery workflow,
          registrationLocale = emailLocale (requestLocale (HarchWeb.clientActionContext actionRequest)),
          registrationVerificationUrl = accountWorkflowVerificationUrl workflow (HarchWeb.clientActionContext actionRequest),
          registrationNow = now,
          registrationLifetime = emailVerificationLifetimeNanoseconds
        }
      RegistrationRequest
        { registrationEmail = emailAddress,
          registrationPassword = Password.mkPassword passwordValue,
          registrationUsername = Just username,
          registrationDisplayName = nonEmptyText displayNameValue
        }

parseRegistrationForm ::
  AccountActionRequest ->
  RegistrationSubmission ->
  Either HarchWeb.ClientActionResponse ParsedRegistration
parseRegistrationForm actionRequest submission =
  let usernameValue = registrationUsernameValue submission
      emailValue = registrationEmailValue submission
      displayNameValue = registrationDisplayNameValue submission
      passwordValue = registrationPasswordValue submission
      path = HarchWeb.clientActionContext actionRequest
      form = RegistrationForm usernameValue emailValue displayNameValue
   in case (Username.mkUsername usernameValue, Email.mkEmailAddress emailValue, validPassword passwordValue) of
        (Nothing, _, _) -> Left (registrationResponse (actionLocale actionRequest) path Http.status422 (form (Just (localized actionRequest "Use a username with 3 to 20 letters, numbers, underscores, or hyphens." "Usa un nombre de usuario de 3 a 20 letras, numeros, guiones bajos o guiones.")) True) (Just "registration-username"))
        (_, Nothing, _) -> Left (registrationResponse (actionLocale actionRequest) path Http.status422 (form (Just (localized actionRequest "Enter a valid email address." "Introduce una direccion de correo valida.")) True) (Just "registration-email"))
        (_, _, False) -> Left (registrationResponse (actionLocale actionRequest) path Http.status422 (form (Just (localized actionRequest "Use a password with at least 12 characters." "Usa una contrasena de al menos 12 caracteres.")) True) (Just "registration-password"))
        (Just username, Just emailAddress, True) -> Right (usernameValue, emailValue, displayNameValue, passwordValue, username, emailAddress)

interpretRegistrationResult ::
  AccountActionRequest ->
  Text ->
  Text ->
  Text ->
  Either RegistrationError RegistrationResult ->
  AccountActionWorkflow
interpretRegistrationResult actionRequest usernameValue emailValue displayNameValue = \case
  Right registrationResult -> pure (registrationResultResponse registrationResult)
  Left registrationError -> throwRegistrationFailure registrationError
  where
    path = HarchWeb.clientActionContext actionRequest
    response status message isError = registrationResponse (actionLocale actionRequest) path status (RegistrationForm usernameValue emailValue displayNameValue (Just message) isError)
    registrationSuccess stage =
      registrationLifecycleResponse
        stage
        (response Http.status202 (localized actionRequest "If that address can register, check its inbox for a verification link." "Si esa direccion puede registrarse, revisa su bandeja de entrada para obtener un enlace de verificacion.") False Nothing)
    unavailableRegistration = response Http.status503 (localized actionRequest "Registration is temporarily unavailable." "El registro no esta disponible temporalmente.") True (Just "registration-email")
    deliveryFailureResponse = response Http.status502 (localized actionRequest "We could not send the verification email. Try again shortly." "No pudimos enviar el correo de verificacion. Intentalo de nuevo en breve.") True (Just "registration-email")

    registrationResultResponse = \case
      -- A taken username is a recoverable, user-correctable input (the
      -- applicant can simply pick another one), unlike a taken email
      -- address — reporting it plainly restores the recovery path BA's
      -- own note found missing, and does not reopen the address-enumeration
      -- concern the branch below exists to close: usernames, unlike email
      -- addresses, are not privacy-sensitive to confirm as taken.
      RegistrationUsernameTaken -> response Http.status422 (localized actionRequest "That username is already taken. Please choose another." "Ese nombre de usuario ya esta en uso. Elige otro.") True (Just "registration-username")
      -- Both remaining outcomes share this exact branch (not merely the
      -- same wording) so a registered-address probe cannot be
      -- distinguished from a genuine registration by response bytes: the
      -- hedged "if that address can register" phrasing is meaningless if
      -- the other outcome answers differently.
      RegistrationCreated _ -> registrationSuccess "created"
      RegistrationRetried _ -> registrationSuccess "retried"
      RegistrationAlreadyRegistered -> registrationSuccess "already-registered"

    throwRegistrationFailure = \case
      RegistrationDeliveryFailed VerificationDeliveryTimedOut -> throwClientActionFailure deliveryFailureResponse RegistrationDeliveryTimeoutFailure "EmailDeliveryTimeout" "registration verification delivery timed out"
      RegistrationDeliveryFailed (VerificationDeliveryTransportFailed detail) -> throwClientActionFailure deliveryFailureResponse RegistrationDeliveryFailure "EmailDeliveryError" detail
      RegistrationStoreError storeError -> throwClientActionFailure unavailableRegistration RegistrationStoreFailure "AccountStoreError" (accountStoreErrorDetail storeError)
      RegistrationPasswordHashingFailed -> throwClientActionFailure unavailableRegistration RegistrationPasswordHashFailure "PasswordHashingError" "password hashing failed"
      RegistrationPasswordWorkBudgetExhausted -> throwClientActionFailure unavailableRegistration RegistrationPasswordWorkBudgetFailure "PasswordWorkBudgetExhausted" "password work budget is exhausted"
      RegistrationStorageExhausted -> throwClientActionFailure unavailableRegistration RegistrationStorageCapacityFailure "PendingRegistrationStorageExhausted" "pending registration storage is at capacity"
      RegistrationDeliveryClaimLost -> throwClientActionFailure unavailableRegistration RegistrationDeliveryClaimFailure "PendingRegistrationDeliveryClaimLost" "registration delivery claim was replaced before completion"
      RegistrationClockOverflow -> throwClientActionFailure unavailableRegistration RegistrationClockFailure "ClockOverflow" "verification expiry overflowed"

registrationLifecycleResponse :: Text -> HarchWeb.ClientActionResponse -> HarchWeb.ClientActionResponse
registrationLifecycleResponse stage response =
  response
    { HarchWeb.clientActionObservabilityAttributes =
        [Observability.ObservabilityAttribute "account.registration.stage" (Observability.TextAttribute stage)],
      HarchWeb.clientActionLogEntries = ["INFO [account.registration] stage=" <> stage]
    }

handleVerificationSubmission :: AccountActionRequest -> VerificationSubmission -> AccountActionWorkflow
handleVerificationSubmission actionRequest submission =
  let tokenValue = verificationTokenValue submission
      path = HarchWeb.clientActionContext actionRequest
   in case Account.mkEmailVerificationToken tokenValue of
        Nothing -> pure (verificationResponse (actionLocale actionRequest) path Http.status422 (VerificationForm tokenValue (Just (localized actionRequest "The verification link is invalid." "El enlace de verificacion no es valido.")) True) (Just "verification-token") [])
        Just token -> do
          (now, confirmationResult) <- confirmEmailVerificationNow token
          case confirmationResult of
            Right (Account.EmailVerificationAccepted accountId _) -> issueVerificationEnrollmentSession actionRequest now accountId
            Right Account.EmailVerificationExpired -> pure (verificationResponse (actionLocale actionRequest) path Http.status422 (VerificationForm tokenValue (Just (localized actionRequest "That verification link has expired." "Ese enlace de verificacion ha caducado.")) True) (Just "verification-token") [])
            Right Account.EmailVerificationRejected -> pure (verificationResponse (actionLocale actionRequest) path Http.status422 (VerificationForm tokenValue (Just (localized actionRequest "That verification link is invalid or has already been used." "Ese enlace de verificacion no es valido o ya se ha utilizado.")) True) (Just "verification-token") [])
            Left storeError -> throwClientActionFailure (verificationResponse (actionLocale actionRequest) path Http.status503 (VerificationForm tokenValue (Just (localized actionRequest "Verification is temporarily unavailable." "La verificacion no esta disponible temporalmente.")) True) (Just "verification-token") []) VerificationStoreFailure "AccountStoreError" (accountStoreErrorDetail storeError)

-- | Verification confirmation reads the clock and store as one operation so
-- the accepted result and subsequent enrollment session share one time.
confirmEmailVerificationNow :: Account.EmailVerificationToken -> AppM publicFailure (UnixTimeNanoseconds, Either AccountStoreError Account.EmailVerificationValidation)
confirmEmailVerificationNow token = do
  workflow <- accountWorkflow
  liftIO $ do
    now <- accountWorkflowClock workflow
    confirmationResult <- confirmEmailVerificationAt (accountWorkflowStore workflow) now token
    pure (now, confirmationResult)

issueMfaEnrollmentSessionNow :: Account.AccountId -> UnixTimeNanoseconds -> AppM publicFailure (Either MfaEnrollmentSessionStoreError (OpaqueSession Account.AccountId))
issueMfaEnrollmentSessionNow accountId now = do
  workflow <- accountWorkflow
  liftIO (issueMfaEnrollmentSession (accountWorkflowMfaEnrollmentSessionStore workflow) accountId now)

-- | Email verification just proved ownership of the account, so this is the
-- one legitimate place to grant enrollment access — see the AM decision
-- record below for why that access is a distinct, short-lived session
-- rather than the ordinary login session or a client-supplied account id.
issueVerificationEnrollmentSession :: AccountActionRequest -> UnixTimeNanoseconds -> Account.AccountId -> AccountActionWorkflow
issueVerificationEnrollmentSession actionRequest now accountId = do
  let path = HarchWeb.clientActionContext actionRequest
      successResponse = verificationResponse (actionLocale actionRequest) path Http.status200 (VerificationForm Text.empty (Just (localized actionRequest "Your email address is verified. Enroll your authenticator next." "Tu direccion de correo esta verificada. A continuacion, registra tu autenticador.")) False) Nothing
  issued <- issueMfaEnrollmentSessionNow accountId now
  case issued of
    Right opaqueSession -> pure (successResponse [("Set-Cookie", TextEncoding.encodeUtf8 (renderSessionCookie mfaEnrollmentSessionCookiePolicy (sessionId opaqueSession)))])
    Left storeError -> throwClientActionFailure (successResponse []) MfaEnrollmentSessionFailure "MfaEnrollmentSessionStoreError" (mfaEnrollmentSessionStoreErrorMessage storeError)

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
  let path = HarchWeb.clientActionContext actionRequest
  case requestMfaEnrollmentSessionId path of
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
                    "start" -> startMfaAction actionRequest path accountId
                    "confirm" -> confirmMfaAction actionRequest path accountId (mfaEnrollmentCodeValue submission)
                    _ -> pure (mfaEnrollmentResponse (actionLocale actionRequest) path Http.status422 (MfaEnrollmentForm Nothing [] (Just (localized actionRequest "Choose an enrollment action." "Elige una accion de registro.")) True) Nothing [])

invalidEnrollmentSessionResponse :: AccountActionRequest -> HarchWeb.ClientActionResponse
invalidEnrollmentSessionResponse actionRequest =
  let path = HarchWeb.clientActionContext actionRequest
   in mfaEnrollmentResponse (actionLocale actionRequest) path Http.status403 (MfaEnrollmentForm Nothing [] (Just (localized actionRequest "This enrollment link is invalid or has expired. Sign in again to continue." "Este enlace de registro no es valido o ha caducado. Inicia sesion de nuevo para continuar.")) True) Nothing []

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
    startMfaEnrollment (accountWorkflowMfaStore workflow) (accountWorkflowTotpEncryptionKey workflow) accountId now

startMfaAction :: AccountActionRequest -> AppRequestContext -> Account.AccountId -> AccountActionWorkflow
startMfaAction actionRequest path accountId = do
  started <- startMfaEnrollmentNow accountId
  case started of
    Right (MfaEnrollmentStart secret) -> pure (mfaEnrollmentResponse (actionLocale actionRequest) path Http.status200 (MfaEnrollmentForm (Just (Totp.renderTotpSecret secret)) [] (Just (localized actionRequest "Add this secret to your authenticator, then enter its six-digit code." "Agrega este secreto a tu autenticador y luego introduce su codigo de seis digitos.")) False) (Just "mfa-code") noHeaders)
    Left errorValue -> interpretMfaFailure actionRequest path MfaEnrollmentStartFailure Nothing errorValue

confirmMfaAction :: AccountActionRequest -> AppRequestContext -> Account.AccountId -> Text -> AccountActionWorkflow
confirmMfaAction actionRequest path accountId codeValue =
  case Totp.mkTotpCode codeValue of
    Nothing -> pure (mfaEnrollmentResponse (actionLocale actionRequest) path Http.status422 (MfaEnrollmentForm Nothing [] (Just (localized actionRequest "Enter a six-digit authenticator code." "Introduce un codigo de autenticador de seis digitos.")) True) (Just "mfa-code") [])
    Just code -> do
      confirmed <- confirmMfaEnrollmentNow accountId code
      case confirmed of
        Right (MfaEnrollmentConfirmation recoveryCodes) -> pure (mfaEnrollmentResponse (actionLocale actionRequest) path Http.status200 (MfaEnrollmentForm Nothing (map RecoveryCode.recoveryCodeText (toList recoveryCodes)) (Just (localized actionRequest "Authenticator enrolled. Save these recovery codes now." "Autenticador registrado. Guarda estos codigos de recuperacion ahora.")) False) Nothing noHeaders)
        Left errorValue -> interpretMfaFailure actionRequest path MfaEnrollmentConfirmFailure (Just "mfa-code") errorValue

confirmMfaEnrollmentNow :: Account.AccountId -> Totp.TotpCode -> AppM publicFailure (Either MfaEnrollmentError MfaEnrollmentConfirmation)
confirmMfaEnrollmentNow accountId code = do
  workflow <- accountWorkflow
  liftIO $ do
    now <- accountWorkflowClock workflow
    confirmMfaEnrollment Password.defaultPasswordHashingPolicy (accountWorkflowMfaStore workflow) (accountWorkflowTotpEncryptionKey workflow) now (accountWorkflowTotpClock workflow now) accountId code

interpretMfaFailure ::
  AccountActionRequest ->
  AppRequestContext ->
  FailureCode ->
  Maybe Text ->
  MfaEnrollmentError ->
  AccountActionWorkflow
interpretMfaFailure actionRequest path failureCodeValue focusId errorValue =
  let response status = mfaEnrollmentResponse (actionLocale actionRequest) path status (MfaEnrollmentForm Nothing [] (Just (mfaErrorMessage actionRequest errorValue)) True) focusId []
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
        (accountWorkflowCredentialStore workflow)
        SecondFactorContext
          { secondFactorMfaStore = accountWorkflowMfaStore workflow,
            secondFactorEncryptionKey = accountWorkflowTotpEncryptionKey workflow,
            secondFactorNowNanoseconds = nowNanoseconds,
            secondFactorNowSeconds = accountWorkflowTotpClock workflow nowNanoseconds,
            secondFactorProof = proof,
            secondFactorThrottle =
              LoginThrottleContext
                { loginThrottleStore = accountWorkflowLoginAttemptStore workflow,
                  loginThrottlePolicy = LoginProtection.defaultLoginProtectionPolicy,
                  loginThrottleNow = nowNanoseconds
                },
            secondFactorPasswordWorkGate = accountWorkflowPasswordWorkGate workflow
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
      path = HarchWeb.clientActionContext actionRequest
      loginForm message = LoginForm emailValue (Just message)
      maybeIdentifier =
        (LoginEmailAddress <$> Email.mkEmailAddress emailValue)
          <|> (LoginUsername <$> Username.mkUsername emailValue)
          <|> (LoginUsername <$> Username.mkUsername usernameValue)
   in case (maybeIdentifier, validPassword passwordValue, loginProof submission) of
        (Nothing, _, _) -> Left (loginResponse (actionLocale actionRequest) path Http.status422 (loginForm (localized actionRequest "Enter a valid email address or username." "Introduce una direccion de correo o un nombre de usuario valido.") True) (Just "login-email") [])
        (_, False, _) -> Left (loginResponse (actionLocale actionRequest) path Http.status422 (loginForm (localized actionRequest "Enter your password." "Introduce tu contrasena.") True) (Just "login-password") [])
        (_, _, Nothing) -> Left (loginResponse (actionLocale actionRequest) path Http.status422 (loginForm (localized actionRequest "Enter a valid authenticator or recovery code." "Introduce un codigo de autenticador o recuperacion valido.") True) (Just "login-code") [])
        (Just identifier, True, Just proof) -> Right (if Text.null emailValue then usernameValue else emailValue, passwordValue, identifier, proof)

interpretLoginResult ::
  AccountActionRequest ->
  Text ->
  UnixTimeNanoseconds ->
  PasswordMfaLoginResult ->
  AccountActionWorkflow
interpretLoginResult actionRequest emailValue nowNanoseconds loginResult =
  let path = HarchWeb.clientActionContext actionRequest
      loginForm message = LoginForm emailValue (Just message)
      response status message isError = loginResponse (actionLocale actionRequest) path status (loginForm message isError)
      unavailable focusId = response Http.status503 (localized actionRequest "Sign-in is temporarily unavailable." "El inicio de sesion no esta disponible temporalmente.") True focusId []
   in case loginResult of
        PasswordMfaLoginAccepted accountId -> issueLoginSession actionRequest emailValue nowNanoseconds accountId
        PasswordMfaLoginEmailVerificationRequired _ -> pure (response Http.status403 (localized actionRequest "Verify your email address before signing in." "Verifica tu direccion de correo antes de iniciar sesion.") True Nothing [])
        PasswordMfaLoginEnrollmentRequired accountId -> issueLoginEnrollmentSession actionRequest emailValue nowNanoseconds accountId
        PasswordMfaLoginRejected -> pure (response Http.status422 (localized actionRequest "Sign-in was rejected." "El inicio de sesion fue rechazado.") True (Just "login-code") [])
        PasswordMfaLoginThrottled _retryAfterNanoseconds -> pure (response Http.status429 (localized actionRequest "Too many sign-in attempts. Try again later." "Demasiados intentos de inicio de sesion. Intentalo de nuevo mas tarde.") True (Just "login-email") [])
        PasswordMfaLoginCredentialStoreError storeError -> throwClientActionFailure (unavailable (Just "login-email")) LoginCredentialStoreFailure "AccountCredentialStoreError" (credentialStoreErrorMessage storeError)
        PasswordMfaLoginMfaStoreError storeError -> throwClientActionFailure (unavailable (Just "login-code")) LoginMfaStoreFailure "MfaStoreError" (mfaStoreErrorMessage storeError)
        PasswordMfaLoginAttemptStoreError storeError -> throwClientActionFailure (unavailable (Just "login-email")) LoginAttemptStoreFailure "LoginAttemptStoreError" (loginAttemptStoreErrorMessage storeError)
        PasswordMfaLoginPasswordWorkBudgetExhausted -> throwClientActionFailure (unavailable (Just "login-email")) LoginPasswordWorkBudgetFailure "PasswordWorkBudgetExhausted" "password work budget is exhausted"
        PasswordMfaLoginCorruptEnrollment -> throwClientActionFailure (unavailable (Just "login-code")) LoginCorruptEnrollmentFailure "CorruptTotpEnrollment" "stored MFA enrollment could not be decoded"

issueLoginSession :: AccountActionRequest -> Text -> UnixTimeNanoseconds -> Account.AccountId -> AccountActionWorkflow
issueLoginSession actionRequest emailValue nowNanoseconds accountId = do
  issuedSession <- issueAccountSessionNow accountId nowNanoseconds
  let path = HarchWeb.clientActionContext actionRequest
      form message = LoginForm emailValue (Just message)
  case issuedSession of
    Left storeError -> throwClientActionFailure (loginResponse (actionLocale actionRequest) path Http.status503 (form (localized actionRequest "Sign-in is temporarily unavailable." "El inicio de sesion no esta disponible temporalmente.") True) (Just "login-email") []) LoginSessionFailure "AccountSessionStoreError" (sessionStoreErrorMessage storeError)
    Right opaqueSession -> pure (loginResponse (actionLocale actionRequest) path Http.status200 (form (localized actionRequest "You are signed in." "Has iniciado sesion.") False) Nothing [("Set-Cookie", TextEncoding.encodeUtf8 (renderSessionCookie defaultSessionCookiePolicy (sessionId opaqueSession)))])

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
  let path = HarchWeb.clientActionContext actionRequest
      form message = LoginForm emailValue (Just message)
      response = loginResponse (actionLocale actionRequest) path Http.status403 (form (localized actionRequest "Enroll your authenticator before signing in." "Registra tu autenticador antes de iniciar sesion.") True) Nothing
  issued <- issueMfaEnrollmentSessionNow accountId nowNanoseconds
  case issued of
    Right opaqueSession -> pure (response [("Set-Cookie", TextEncoding.encodeUtf8 (renderSessionCookie mfaEnrollmentSessionCookiePolicy (sessionId opaqueSession)))])
    Left storeError -> throwClientActionFailure (response []) MfaEnrollmentSessionFailure "MfaEnrollmentSessionStoreError" (mfaEnrollmentSessionStoreErrorMessage storeError)

handleLogout :: AccountActionRequest -> AccountActionWorkflow
handleLogout actionRequest =
  let path = HarchWeb.clientActionContext actionRequest
   in case requestSessionId (HarchWeb.clientActionContext actionRequest) of
        Nothing -> pure (logoutResponse (actionLocale actionRequest) path Http.status200 (Just (localized actionRequest "You are signed out." "Has cerrado sesion.")) False [])
        Just sessionToken -> do
          invalidated <- invalidateAccountSessionNow sessionToken
          case invalidated of
            Left storeError -> throwClientActionFailure (logoutResponse (actionLocale actionRequest) path Http.status503 (Just (localized actionRequest "Sign-out is temporarily unavailable." "El cierre de sesion no esta disponible temporalmente.")) True []) LogoutSessionFailure "AccountSessionStoreError" (sessionStoreErrorMessage storeError)
            Right _ -> pure (logoutResponse (actionLocale actionRequest) path Http.status200 (Just (localized actionRequest "You are signed out." "Has cerrado sesion.")) False [("Set-Cookie", TextEncoding.encodeUtf8 (renderSessionCookie (defaultSessionCookiePolicy {sessionCookieMaxAgeSeconds = 0}) sessionToken))])

invalidateAccountSessionNow :: SessionId -> AppM publicFailure (Either AccountSessionStoreError Bool)
invalidateAccountSessionNow sessionToken = do
  workflow <- accountWorkflow
  liftIO $ do
    now <- accountWorkflowClock workflow
    invalidateAccountSession (accountWorkflowSessionStore workflow) sessionToken now

handleProfileSubmission :: AccountActionRequest -> ProfileSubmission -> AccountActionWorkflow
handleProfileSubmission actionRequest submission = do
  (now, loadedProfile) <- loadProfileNow (requestSessionId (HarchWeb.clientActionContext actionRequest))
  case loadedProfile of
    Left loadError -> throwClientActionFailure (profileResponse actionRequest Http.status503 (PendingProfileForm Text.empty (Just (localized actionRequest "Your profile is temporarily unavailable." "Tu perfil no esta disponible temporalmente.")) True (resendLabel actionRequest))) ProfileLoadFailure (profileLoadErrorType loadError) (profileLoadErrorDetail loadError)
    Right ProfileUnauthenticated -> pure (profileResponse actionRequest Http.status403 (PendingProfileForm Text.empty (Just (localized actionRequest "Sign in before requesting another verification email." "Inicia sesion antes de solicitar otro correo de verificacion.")) True (resendLabel actionRequest)))
    Right (ProfileAuthenticated profile) -> pure (profileResponse actionRequest Http.status409 (PendingProfileForm (Email.emailAddressText (accountProfileEmail profile)) (Just (localized actionRequest "Your email address is already verified." "Tu direccion de correo ya esta verificada.")) True (resendLabel actionRequest)))
    Right (ProfilePending profile) -> handlePendingProfile actionRequest submission now profile

loadProfileNow :: Maybe SessionId -> AppM publicFailure (UnixTimeNanoseconds, Either ProfileLoadError ProfileState)
loadProfileNow maybeSessionId = do
  workflow <- accountWorkflow
  liftIO $ do
    now <- accountWorkflowClock workflow
    loadedProfile <- loadProfile (accountWorkflowSessionStore workflow) (accountWorkflowProfileStore workflow) now maybeSessionId
    pure (now, loadedProfile)

handlePendingProfile ::
  AccountActionRequest ->
  ProfileSubmission ->
  UnixTimeNanoseconds ->
  AccountProfile ->
  AccountActionWorkflow
handlePendingProfile actionRequest submission now profile =
  case profileIntentValue submission of
    "resend-verification" -> do
      resendResult <- resendEmailVerificationNow actionRequest now profile
      interpretProfileResendResult actionRequest profile resendResult
    _ -> pure (profileResponse actionRequest Http.status422 (pendingProfileForm actionRequest profile (Just (localized actionRequest "Choose a profile action." "Elige una accion de perfil.")) True))

resendEmailVerificationNow :: AccountActionRequest -> UnixTimeNanoseconds -> AccountProfile -> AppM publicFailure (Either ResendVerificationError ())
resendEmailVerificationNow actionRequest now profile = do
  workflow <- accountWorkflow
  liftIO $
    resendEmailVerificationAt
      (accountWorkflowStore workflow)
      (accountWorkflowRegistrationDeliveryTimeout workflow)
      (accountWorkflowEmailDelivery workflow)
      (emailLocale (requestLocale (HarchWeb.clientActionContext actionRequest)))
      (accountWorkflowVerificationUrl workflow (HarchWeb.clientActionContext actionRequest))
      now
      emailVerificationLifetimeNanoseconds
      profile

interpretProfileResendResult ::
  AccountActionRequest ->
  AccountProfile ->
  Either ResendVerificationError () ->
  AccountActionWorkflow
interpretProfileResendResult actionRequest profile resendResult =
  let form message = pendingProfileForm actionRequest profile (Just message)
   in case resendResult of
        Right () -> pure (profileResponse actionRequest Http.status202 (form (localized actionRequest "Check your inbox for a verification link." "Revisa tu bandeja de entrada para obtener un enlace de verificacion.") False))
        Left ResendVerificationNoLongerPending -> pure (profileResponse actionRequest Http.status409 (form (localized actionRequest "Your profile state changed. Reload the page before trying again." "El estado de tu perfil ha cambiado. Recarga la pagina antes de intentarlo de nuevo.") True))
        Left (ResendVerificationDeliveryFailed detail) -> throwClientActionFailure (profileResponse actionRequest Http.status502 (form (localized actionRequest "We could not send the verification email. Try again shortly." "No pudimos enviar el correo de verificacion. Intentalo de nuevo en breve.") True)) ProfileResendDeliveryFailure "EmailDeliveryError" detail
        Left (ResendVerificationStoreError storeError) -> throwClientActionFailure (profileResponse actionRequest Http.status503 (form (localized actionRequest "Your profile is temporarily unavailable." "Tu perfil no esta disponible temporalmente.") True)) ProfileResendStoreFailure "AccountStoreError" (accountStoreErrorDetail storeError)
        Left ResendVerificationClockOverflow -> throwClientActionFailure (profileResponse actionRequest Http.status503 (form (localized actionRequest "Your profile is temporarily unavailable." "Tu perfil no esta disponible temporalmente.") True)) ProfileResendClockFailure "ClockOverflow" "verification expiry overflowed"

loginProof :: LoginSubmission -> Maybe MfaLoginProof
loginProof submission =
  case loginProofValue submission of
    "totp" -> TotpLoginProof <$> Totp.mkTotpCode (loginCodeValue submission)
    "recovery" -> RecoveryCodeLoginProof <$> RecoveryCode.mkRecoveryCode (loginCodeValue submission)
    _ -> Nothing
