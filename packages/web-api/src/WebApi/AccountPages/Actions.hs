{-# LANGUAGE OverloadedStrings #-}

module WebApi.AccountPages.Actions
  ( handleAccountAction,
    mfaEnrollmentFailureDiagnostics,
  )
where

import Control.Applicative ((<|>))
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word64)
import HarchWeb qualified
import HarchWeb.Account qualified as Account
import HarchWeb.Email qualified as Email
import HarchWeb.Observability qualified as Observability
import HarchWeb.Password qualified as Password
import HarchWeb.RecoveryCode qualified as RecoveryCode
import HarchWeb.Session
  ( defaultSessionCookiePolicy,
    renderSessionCookie,
    sessionCookieMaxAgeSeconds,
    sessionId,
  )
import HarchWeb.Totp qualified as Totp
import HarchWeb.Username qualified as Username
import Network.HTTP.Types qualified as Http
import WebApi.Account
  ( AccountProfile (..),
    AccountStoreError (..),
    RegistrationError (..),
    RegistrationResult (..),
    ResendVerificationError (..),
    confirmEmailVerificationAt,
    registerAccountWithIdentityAtWithPasswordHasher,
    resendEmailVerificationAt,
  )
import WebApi.AccountPages.Forms
import WebApi.AccountPages.Rendering
import WebApi.AppEffect
  ( AccountWorkflow (..),
    AppFailure (..),
    AppM,
    AppServices (..),
    FailureDiagnostics (..),
    askAppServices,
    liftAppIO,
    runAppM,
    throwAppFailure,
  )
import WebApi.Login
  ( AccountCredentialStoreError (..),
    LoginIdentifier (..),
    MfaLoginProof (..),
    PasswordMfaLoginResult (..),
    completePasswordLoginWithIdentifier,
  )
import WebApi.Mfa (MfaStoreError (..))
import WebApi.MfaEnrollment
  ( MfaEnrollmentConfirmation (..),
    MfaEnrollmentError (..),
    MfaEnrollmentStart (..),
    confirmMfaEnrollment,
    startMfaEnrollment,
  )
import WebApi.Profile
  ( ProfileLoadError (..),
    ProfileState (..),
    loadProfile,
  )
import WebApi.Route
  ( AppLocale (..),
    AppRequestContext (..),
    AppRoute (..),
    renderRoutePath,
  )
import WebApi.Session
  ( AccountSessionStore (..),
    AccountSessionStoreError (..),
    issueAccountSession,
  )

handleAccountAction :: AccountWorkflow -> HarchWeb.ClientActionRequest AppRequestContext -> IO (Maybe HarchWeb.ClientActionResponse)
handleAccountAction workflow actionRequest =
  traverse runSelectedAccountAction (accountActionCodec actionRequest)
  where
    runSelectedAccountAction action =
      either attachClientActionFailure id <$> runAppM (AppServices workflow) (runAccountAction action)

data AccountAction
  = RegistrationAction (HarchWeb.ClientActionRequest AppRequestContext) RegistrationSubmission
  | VerificationAction (HarchWeb.ClientActionRequest AppRequestContext) VerificationSubmission
  | MfaEnrollmentAction (HarchWeb.ClientActionRequest AppRequestContext) MfaEnrollmentSubmission
  | LoginAction (HarchWeb.ClientActionRequest AppRequestContext) LoginSubmission
  | ProfileAction (HarchWeb.ClientActionRequest AppRequestContext) ProfileSubmission
  | LogoutAction (HarchWeb.ClientActionRequest AppRequestContext)

data RegistrationSubmission = RegistrationSubmission
  { registrationUsernameValue :: Text,
    registrationEmailValue :: Text,
    registrationDisplayNameValue :: Text,
    registrationPasswordValue :: Text
  }

newtype VerificationSubmission = VerificationSubmission
  { verificationTokenValue :: Text
  }

data MfaEnrollmentSubmission = MfaEnrollmentSubmission
  { mfaEnrollmentAccountValue :: Text,
    mfaEnrollmentIntentValue :: Text,
    mfaEnrollmentCodeValue :: Text
  }

data LoginSubmission = LoginSubmission
  { loginEmailValue :: Text,
    loginUsernameValue :: Text,
    loginPasswordValue :: Text,
    loginProofValue :: Text,
    loginCodeValue :: Text
  }

newtype ProfileSubmission = ProfileSubmission
  { profileIntentValue :: Text
  }

accountActionCodec :: HarchWeb.ClientActionRequest AppRequestContext -> Maybe AccountAction
accountActionCodec actionRequest =
  if HarchWeb.clientActionMethod actionRequest /= "POST"
    then Nothing
    else case HarchWeb.clientActionPath actionRequest of
      path | path == accountRoutePath actionRequest RegistrationRoute -> Just (RegistrationAction actionRequest (registrationSubmission actionRequest))
      path | path == accountRoutePath actionRequest EmailVerificationRoute -> Just (VerificationAction actionRequest (verificationSubmission actionRequest))
      path | path == accountRoutePath actionRequest MfaEnrollmentRoute -> Just (MfaEnrollmentAction actionRequest (mfaEnrollmentSubmission actionRequest))
      path | path == accountRoutePath actionRequest LoginRoute -> Just (LoginAction actionRequest (loginSubmission actionRequest))
      path | path == accountRoutePath actionRequest ProfileRoute -> Just (ProfileAction actionRequest (profileSubmission actionRequest))
      path | path == accountRoutePath actionRequest LogoutRoute -> Just (LogoutAction actionRequest)
      _ -> Nothing

runAccountAction :: AccountAction -> AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse
runAccountAction accountAction =
  case accountAction of
    RegistrationAction actionRequest submission -> handleRegistration actionRequest submission
    VerificationAction actionRequest submission -> handleVerification actionRequest submission
    MfaEnrollmentAction actionRequest submission -> handleMfaEnrollment actionRequest submission
    LoginAction actionRequest submission -> handleLogin actionRequest submission
    ProfileAction actionRequest submission -> handleProfile actionRequest submission
    LogoutAction actionRequest -> handleLogout actionRequest

registrationSubmission :: HarchWeb.ClientActionRequest context -> RegistrationSubmission
registrationSubmission actionRequest =
  RegistrationSubmission
    { registrationUsernameValue = submittedField actionRequest "username",
      registrationEmailValue = submittedField actionRequest "email",
      registrationDisplayNameValue = submittedField actionRequest "displayName",
      registrationPasswordValue = submittedField actionRequest "password"
    }

verificationSubmission :: HarchWeb.ClientActionRequest context -> VerificationSubmission
verificationSubmission actionRequest = VerificationSubmission {verificationTokenValue = submittedField actionRequest "token"}

mfaEnrollmentSubmission :: HarchWeb.ClientActionRequest context -> MfaEnrollmentSubmission
mfaEnrollmentSubmission actionRequest =
  MfaEnrollmentSubmission
    { mfaEnrollmentAccountValue = submittedField actionRequest "account",
      mfaEnrollmentIntentValue = submittedField actionRequest "intent",
      mfaEnrollmentCodeValue = submittedField actionRequest "code"
    }

loginSubmission :: HarchWeb.ClientActionRequest context -> LoginSubmission
loginSubmission actionRequest =
  LoginSubmission
    { loginEmailValue = submittedField actionRequest "email",
      loginUsernameValue = submittedField actionRequest "username",
      loginPasswordValue = submittedField actionRequest "password",
      loginProofValue = submittedField actionRequest "proof",
      loginCodeValue = submittedField actionRequest "code"
    }

profileSubmission :: HarchWeb.ClientActionRequest context -> ProfileSubmission
profileSubmission actionRequest = ProfileSubmission {profileIntentValue = submittedField actionRequest "intent"}

accountRoutePath :: HarchWeb.ClientActionRequest AppRequestContext -> AppRoute -> Text
accountRoutePath actionRequest route =
  renderRoutePath
    HarchWeb.RouteRequest
      { HarchWeb.requestRoute = route,
        HarchWeb.requestContext = HarchWeb.clientActionContext actionRequest
      }

accountWorkflow :: AppM publicFailure AccountWorkflow
accountWorkflow = appAccountWorkflow <$> askAppServices

handleRegistration :: HarchWeb.ClientActionRequest AppRequestContext -> RegistrationSubmission -> AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse
handleRegistration actionRequest submission =
  case parseRegistrationForm actionRequest submission of
    Left response -> pure response
    Right (usernameValue, emailValue, displayNameValue, passwordValue, username, emailAddress) -> do
      workflow <- accountWorkflow
      now <- liftAppIO (accountWorkflowClock workflow)
      registrationResult <-
        liftAppIO $
          registerAccountWithIdentityAtWithPasswordHasher
            (accountWorkflowPasswordHasher workflow)
            Password.defaultPasswordHashingPolicy
            (accountWorkflowStore workflow)
            (accountWorkflowEmailDelivery workflow)
            (emailLocale (requestLocale (HarchWeb.clientActionContext actionRequest)))
            (accountWorkflowVerificationUrl workflow (HarchWeb.clientActionContext actionRequest))
            now
            emailVerificationLifetimeNanoseconds
            (Just username)
            (nonEmptyText displayNameValue)
            emailAddress
            (Password.mkPassword passwordValue)
      interpretRegistrationResult actionRequest usernameValue emailValue displayNameValue registrationResult

parseRegistrationForm :: HarchWeb.ClientActionRequest AppRequestContext -> RegistrationSubmission -> Either HarchWeb.ClientActionResponse (Text, Text, Text, Text, Username.Username, Email.EmailAddress)
parseRegistrationForm actionRequest submission =
  let usernameValue = registrationUsernameValue submission
      emailValue = registrationEmailValue submission
      displayNameValue = registrationDisplayNameValue submission
      passwordValue = registrationPasswordValue submission
      path = accountRoutePath actionRequest RegistrationRoute
      form = RegistrationForm usernameValue emailValue displayNameValue
   in case (Username.mkUsername usernameValue, Email.mkEmailAddress emailValue, validPassword passwordValue) of
        (Nothing, _, _) -> Left (registrationResponse (actionLocale actionRequest) path 422 (form (Just (localized actionRequest "Use a username with 3 to 20 letters, numbers, underscores, or hyphens." "Usa un nombre de usuario de 3 a 20 letras, numeros, guiones bajos o guiones.")) True) (Just "registration-username"))
        (_, Nothing, _) -> Left (registrationResponse (actionLocale actionRequest) path 422 (form (Just (localized actionRequest "Enter a valid email address." "Introduce una direccion de correo valida.")) True) (Just "registration-email"))
        (_, _, False) -> Left (registrationResponse (actionLocale actionRequest) path 422 (form (Just (localized actionRequest "Use a password with at least 12 characters." "Usa una contrasena de al menos 12 caracteres.")) True) (Just "registration-password"))
        (Just username, Just emailAddress, True) -> Right (usernameValue, emailValue, displayNameValue, passwordValue, username, emailAddress)

interpretRegistrationResult :: HarchWeb.ClientActionRequest AppRequestContext -> Text -> Text -> Text -> Either RegistrationError RegistrationResult -> AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse
interpretRegistrationResult actionRequest usernameValue emailValue displayNameValue registrationResult =
  let path = accountRoutePath actionRequest RegistrationRoute
      response status message isError = registrationResponse (actionLocale actionRequest) path status (RegistrationForm usernameValue emailValue displayNameValue (Just message) isError)
   in case registrationResult of
        Right RegistrationAlreadyRegistered -> pure (response 202 (localized actionRequest "If that address can register, check its inbox for a verification link." "Si esa direccion puede registrarse, revisa su bandeja de entrada para obtener un enlace de verificacion.") False Nothing)
        Right (RegistrationCreated _) -> pure (response 202 (localized actionRequest "Check your inbox for a verification link." "Revisa tu bandeja de entrada para obtener un enlace de verificacion.") False Nothing)
        Left (RegistrationDeliveryFailed detail) -> throwClientActionFailure (response 502 (localized actionRequest "We could not send the verification email. Try again shortly." "No pudimos enviar el correo de verificacion. Intentalo de nuevo en breve.") True (Just "registration-email")) "account.registration.delivery" "EmailDeliveryError" detail
        Left (RegistrationStoreError storeError) -> throwClientActionFailure (response 503 (localized actionRequest "Registration is temporarily unavailable." "El registro no esta disponible temporalmente.") True (Just "registration-email")) "account.registration.store" "AccountStoreError" (accountStoreErrorDetail storeError)
        Left RegistrationPasswordHashingFailed -> throwClientActionFailure (response 503 (localized actionRequest "Registration is temporarily unavailable." "El registro no esta disponible temporalmente.") True (Just "registration-email")) "account.registration.password-hash" "PasswordHashingError" "password hashing failed"
        Left RegistrationClockOverflow -> throwClientActionFailure (response 503 (localized actionRequest "Registration is temporarily unavailable." "El registro no esta disponible temporalmente.") True (Just "registration-email")) "account.registration.clock" "ClockOverflow" "verification expiry overflowed"

handleVerification :: HarchWeb.ClientActionRequest AppRequestContext -> VerificationSubmission -> AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse
handleVerification actionRequest submission =
  let tokenValue = verificationTokenValue submission
      path = accountRoutePath actionRequest EmailVerificationRoute
   in case Account.mkEmailVerificationToken tokenValue of
        Nothing -> pure (verificationResponse (actionLocale actionRequest) path 422 (VerificationForm tokenValue (Just (localized actionRequest "The verification link is invalid." "El enlace de verificacion no es valido.")) True) (Just "verification-token"))
        Just token -> do
          workflow <- accountWorkflow
          now <- liftAppIO (accountWorkflowClock workflow)
          confirmationResult <- liftAppIO (confirmEmailVerificationAt (accountWorkflowStore workflow) now token)
          case confirmationResult of
            Right (Account.EmailVerificationAccepted _ _) -> pure (verificationResponse (actionLocale actionRequest) path 200 (VerificationForm Text.empty (Just (localized actionRequest "Your email address is verified. Enroll your authenticator next." "Tu direccion de correo esta verificada. A continuacion, registra tu autenticador.")) False) Nothing)
            Right Account.EmailVerificationExpired -> pure (verificationResponse (actionLocale actionRequest) path 422 (VerificationForm tokenValue (Just (localized actionRequest "That verification link has expired." "Ese enlace de verificacion ha caducado.")) True) (Just "verification-token"))
            Right Account.EmailVerificationRejected -> pure (verificationResponse (actionLocale actionRequest) path 422 (VerificationForm tokenValue (Just (localized actionRequest "That verification link is invalid or has already been used." "Ese enlace de verificacion no es valido o ya se ha utilizado.")) True) (Just "verification-token"))
            Left storeError -> throwClientActionFailure (verificationResponse (actionLocale actionRequest) path 503 (VerificationForm tokenValue (Just (localized actionRequest "Verification is temporarily unavailable." "La verificacion no esta disponible temporalmente.")) True) (Just "verification-token")) "account.verification.store" "AccountStoreError" (accountStoreErrorDetail storeError)

handleMfaEnrollment :: HarchWeb.ClientActionRequest AppRequestContext -> MfaEnrollmentSubmission -> AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse
handleMfaEnrollment actionRequest submission =
  let accountValue = mfaEnrollmentAccountValue submission
      path = accountRoutePath actionRequest MfaEnrollmentRoute
   in case Account.mkAccountId accountValue of
        Nothing -> pure (mfaEnrollmentResponse (actionLocale actionRequest) path 422 (MfaEnrollmentForm accountValue Nothing [] (Just (localized actionRequest "The enrollment link is invalid." "El enlace de registro no es valido.")) True) (Just "mfa-account"))
        Just accountId ->
          case mfaEnrollmentIntentValue submission of
            "start" -> startMfaAction actionRequest path accountId
            "confirm" -> confirmMfaAction actionRequest path accountId (mfaEnrollmentCodeValue submission)
            _ -> pure (mfaEnrollmentResponse (actionLocale actionRequest) path 422 (MfaEnrollmentForm (Account.accountIdText accountId) Nothing [] (Just (localized actionRequest "Choose an enrollment action." "Elige una accion de registro.")) True) (Just "mfa-account"))

startMfaAction :: HarchWeb.ClientActionRequest AppRequestContext -> Text -> Account.AccountId -> AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse
startMfaAction actionRequest path accountId = do
  workflow <- accountWorkflow
  now <- liftAppIO (accountWorkflowClock workflow)
  started <- liftAppIO (startMfaEnrollment (accountWorkflowMfaStore workflow) (accountWorkflowTotpEncryptionKey workflow) accountId now)
  case started of
    Right (MfaEnrollmentStart secret) -> pure (mfaEnrollmentResponse (actionLocale actionRequest) path 200 (MfaEnrollmentForm (Account.accountIdText accountId) (Just (Totp.renderTotpSecret secret)) [] (Just (localized actionRequest "Add this secret to your authenticator, then enter its six-digit code." "Agrega este secreto a tu autenticador y luego introduce su codigo de seis digitos.")) False) (Just "mfa-code"))
    Left errorValue -> interpretMfaFailure actionRequest path accountId "start" "mfa-account" errorValue

confirmMfaAction :: HarchWeb.ClientActionRequest AppRequestContext -> Text -> Account.AccountId -> Text -> AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse
confirmMfaAction actionRequest path accountId codeValue =
  case Totp.mkTotpCode codeValue of
    Nothing -> pure (mfaEnrollmentResponse (actionLocale actionRequest) path 422 (MfaEnrollmentForm (Account.accountIdText accountId) Nothing [] (Just (localized actionRequest "Enter a six-digit authenticator code." "Introduce un codigo de autenticador de seis digitos.")) True) (Just "mfa-code"))
    Just code -> do
      workflow <- accountWorkflow
      nowNanoseconds <- liftAppIO (accountWorkflowClock workflow)
      nowSeconds <- liftAppIO (accountWorkflowTotpClock workflow)
      confirmed <- liftAppIO (confirmMfaEnrollment Password.defaultPasswordHashingPolicy (accountWorkflowMfaStore workflow) (accountWorkflowTotpEncryptionKey workflow) accountId nowNanoseconds nowSeconds code)
      case confirmed of
        Right (MfaEnrollmentConfirmation recoveryCodes) -> pure (mfaEnrollmentResponse (actionLocale actionRequest) path 200 (MfaEnrollmentForm (Account.accountIdText accountId) Nothing (map RecoveryCode.recoveryCodeText (toList recoveryCodes)) (Just (localized actionRequest "Authenticator enrolled. Save these recovery codes now." "Autenticador registrado. Guarda estos codigos de recuperacion ahora.")) False) Nothing)
        Left errorValue -> interpretMfaFailure actionRequest path accountId "confirm" "mfa-code" errorValue

interpretMfaFailure :: HarchWeb.ClientActionRequest AppRequestContext -> Text -> Account.AccountId -> Text -> Text -> MfaEnrollmentError -> AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse
interpretMfaFailure actionRequest path accountId operation focusId errorValue =
  let response status = mfaEnrollmentResponse (actionLocale actionRequest) path status (MfaEnrollmentForm (Account.accountIdText accountId) Nothing [] (Just (mfaErrorMessage actionRequest errorValue)) True) (Just focusId)
   in case mfaEnrollmentFailureDiagnostics operation errorValue of
        Nothing -> pure (response 422)
        Just diagnostics -> throwAppFailure AppFailure {appFailurePublic = response 503, appFailureDiagnostics = diagnostics}

mfaEnrollmentFailureDiagnostics :: Text -> MfaEnrollmentError -> Maybe FailureDiagnostics
mfaEnrollmentFailureDiagnostics operation errorValue =
  case errorValue of
    MfaEnrollmentStoreError storeError -> Just (failureDiagnostics "MfaStoreError" (mfaStoreErrorMessage storeError))
    MfaEnrollmentCorruptSecret -> Just (failureDiagnostics "CorruptTotpEnrollment" "stored TOTP secret could not be decoded")
    MfaEnrollmentRecoveryCodeHashingFailed -> Just (failureDiagnostics "RecoveryCodeHashingError" "recovery-code hashing failed")
    MfaEnrollmentEncryptionFailed -> Just (failureDiagnostics "TotpEncryptionError" "TOTP secret encryption failed")
    _ -> Nothing
  where
    failureDiagnostics = buildFailureDiagnostics ("account.mfa." <> operation)

handleLogin :: HarchWeb.ClientActionRequest AppRequestContext -> LoginSubmission -> AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse
handleLogin actionRequest submission =
  case parseLoginForm actionRequest submission of
    Left response -> pure response
    Right (emailValue, passwordValue, identifier, proof) -> do
      workflow <- accountWorkflow
      nowNanoseconds <- liftAppIO (accountWorkflowClock workflow)
      nowSeconds <- liftAppIO (accountWorkflowTotpClock workflow)
      loginResult <- liftAppIO (completePasswordLoginWithIdentifier (accountWorkflowCredentialStore workflow) (accountWorkflowMfaStore workflow) (accountWorkflowTotpEncryptionKey workflow) nowNanoseconds nowSeconds identifier (Password.mkPassword passwordValue) proof)
      interpretLoginResult actionRequest emailValue nowNanoseconds loginResult

parseLoginForm :: HarchWeb.ClientActionRequest AppRequestContext -> LoginSubmission -> Either HarchWeb.ClientActionResponse (Text, Text, LoginIdentifier, MfaLoginProof)
parseLoginForm actionRequest submission =
  let emailValue = loginEmailValue submission
      usernameValue = loginUsernameValue submission
      passwordValue = loginPasswordValue submission
      path = accountRoutePath actionRequest LoginRoute
      loginForm message = LoginForm emailValue (Just message)
      maybeIdentifier =
        (LoginEmailAddress <$> Email.mkEmailAddress emailValue)
          <|> (LoginUsername <$> Username.mkUsername emailValue)
          <|> (LoginUsername <$> Username.mkUsername usernameValue)
   in case (maybeIdentifier, validPassword passwordValue, loginProof submission) of
        (Nothing, _, _) -> Left (loginResponse (actionLocale actionRequest) path 422 (loginForm (localized actionRequest "Enter a valid email address or username." "Introduce una direccion de correo o un nombre de usuario valido.") True) (Just "login-email") [])
        (_, False, _) -> Left (loginResponse (actionLocale actionRequest) path 422 (loginForm (localized actionRequest "Enter your password." "Introduce tu contrasena.") True) (Just "login-password") [])
        (_, _, Nothing) -> Left (loginResponse (actionLocale actionRequest) path 422 (loginForm (localized actionRequest "Enter a valid authenticator or recovery code." "Introduce un codigo de autenticador o recuperacion valido.") True) (Just "login-code") [])
        (Just identifier, True, Just proof) -> Right (if Text.null emailValue then usernameValue else emailValue, passwordValue, identifier, proof)

interpretLoginResult :: HarchWeb.ClientActionRequest AppRequestContext -> Text -> Word64 -> PasswordMfaLoginResult -> AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse
interpretLoginResult actionRequest emailValue nowNanoseconds loginResult =
  let path = accountRoutePath actionRequest LoginRoute
      loginForm message = LoginForm emailValue (Just message)
      response status message isError = loginResponse (actionLocale actionRequest) path status (loginForm message isError)
      unavailable focusId = response 503 (localized actionRequest "Sign-in is temporarily unavailable." "El inicio de sesion no esta disponible temporalmente.") True focusId []
   in case loginResult of
        PasswordMfaLoginAccepted accountId -> issueLoginSession actionRequest emailValue nowNanoseconds accountId
        PasswordMfaLoginEmailVerificationRequired _ -> pure (response 403 (localized actionRequest "Verify your email address before signing in." "Verifica tu direccion de correo antes de iniciar sesion.") True Nothing [])
        PasswordMfaLoginEnrollmentRequired _ -> pure (response 403 (localized actionRequest "Enroll your authenticator before signing in." "Registra tu autenticador antes de iniciar sesion.") True Nothing [])
        PasswordMfaLoginRejected -> pure (response 422 (localized actionRequest "Sign-in was rejected." "El inicio de sesion fue rechazado.") True (Just "login-code") [])
        PasswordMfaLoginCredentialStoreError storeError -> throwClientActionFailure (unavailable (Just "login-email")) "account.login.credential-store" "AccountCredentialStoreError" (credentialStoreErrorMessage storeError)
        PasswordMfaLoginMfaStoreError storeError -> throwClientActionFailure (unavailable (Just "login-code")) "account.login.mfa-store" "MfaStoreError" (mfaStoreErrorMessage storeError)
        PasswordMfaLoginCorruptEnrollment -> throwClientActionFailure (unavailable (Just "login-code")) "account.login.corrupt-enrollment" "CorruptTotpEnrollment" "stored MFA enrollment could not be decoded"

issueLoginSession :: HarchWeb.ClientActionRequest AppRequestContext -> Text -> Word64 -> Account.AccountId -> AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse
issueLoginSession actionRequest emailValue nowNanoseconds accountId = do
  workflow <- accountWorkflow
  issuedSession <- liftAppIO (issueAccountSession (accountWorkflowSessionStore workflow) accountId nowNanoseconds)
  let path = accountRoutePath actionRequest LoginRoute
      form message = LoginForm emailValue (Just message)
  case issuedSession of
    Left storeError -> throwClientActionFailure (loginResponse (actionLocale actionRequest) path 503 (form (localized actionRequest "Sign-in is temporarily unavailable." "El inicio de sesion no esta disponible temporalmente.") True) (Just "login-email") []) "account.login.session" "AccountSessionStoreError" (sessionStoreErrorMessage storeError)
    Right opaqueSession -> pure (loginResponse (actionLocale actionRequest) path 200 (form (localized actionRequest "You are signed in." "Has iniciado sesion.") False) Nothing [("Set-Cookie", TextEncoding.encodeUtf8 (renderSessionCookie defaultSessionCookiePolicy (sessionId opaqueSession)))])

handleLogout :: HarchWeb.ClientActionRequest AppRequestContext -> AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse
handleLogout actionRequest =
  let path = accountRoutePath actionRequest LogoutRoute
   in case requestSessionId (HarchWeb.clientActionContext actionRequest) of
        Nothing -> pure (logoutResponse (actionLocale actionRequest) path 200 (Just (localized actionRequest "You are signed out." "Has cerrado sesion.")) False [])
        Just sessionToken -> do
          workflow <- accountWorkflow
          invalidated <- liftAppIO (invalidateAccountSession (accountWorkflowSessionStore workflow) sessionToken)
          case invalidated of
            Left storeError -> throwClientActionFailure (logoutResponse (actionLocale actionRequest) path 503 (Just (localized actionRequest "Sign-out is temporarily unavailable." "El cierre de sesion no esta disponible temporalmente.")) True []) "account.logout.session" "AccountSessionStoreError" (sessionStoreErrorMessage storeError)
            Right _ -> pure (logoutResponse (actionLocale actionRequest) path 200 (Just (localized actionRequest "You are signed out." "Has cerrado sesion.")) False [("Set-Cookie", TextEncoding.encodeUtf8 (renderSessionCookie (defaultSessionCookiePolicy {sessionCookieMaxAgeSeconds = 0}) sessionToken))])

handleProfile :: HarchWeb.ClientActionRequest AppRequestContext -> ProfileSubmission -> AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse
handleProfile actionRequest submission = do
  workflow <- accountWorkflow
  now <- liftAppIO (accountWorkflowClock workflow)
  loadedProfile <- liftAppIO (loadProfile (accountWorkflowSessionStore workflow) (accountWorkflowProfileStore workflow) now (requestSessionId (HarchWeb.clientActionContext actionRequest)))
  case loadedProfile of
    Left loadError -> throwClientActionFailure (profileResponse actionRequest 503 (PendingProfileForm Text.empty (Just (localized actionRequest "Your profile is temporarily unavailable." "Tu perfil no esta disponible temporalmente.")) True (resendLabel actionRequest))) "account.profile.load" (profileLoadErrorType loadError) (profileLoadErrorDetail loadError)
    Right ProfileUnauthenticated -> pure (profileResponse actionRequest 403 (PendingProfileForm Text.empty (Just (localized actionRequest "Sign in before requesting another verification email." "Inicia sesion antes de solicitar otro correo de verificacion.")) True (resendLabel actionRequest)))
    Right (ProfileAuthenticated profile) -> pure (profileResponse actionRequest 409 (PendingProfileForm (Email.emailAddressText (accountProfileEmail profile)) (Just (localized actionRequest "Your email address is already verified." "Tu direccion de correo ya esta verificada.")) True (resendLabel actionRequest)))
    Right (ProfilePending profile) -> handlePendingProfile actionRequest submission workflow now profile

handlePendingProfile :: HarchWeb.ClientActionRequest AppRequestContext -> ProfileSubmission -> AccountWorkflow -> Word64 -> AccountProfile -> AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse
handlePendingProfile actionRequest submission workflow now profile =
  case profileIntentValue submission of
    "resend-verification" -> do
      resendResult <-
        liftAppIO $
          resendEmailVerificationAt
            (accountWorkflowStore workflow)
            (accountWorkflowEmailDelivery workflow)
            (emailLocale (requestLocale (HarchWeb.clientActionContext actionRequest)))
            (accountWorkflowVerificationUrl workflow (HarchWeb.clientActionContext actionRequest))
            now
            emailVerificationLifetimeNanoseconds
            profile
      interpretProfileResendResult actionRequest profile resendResult
    _ -> pure (profileResponse actionRequest 422 (pendingProfileForm actionRequest profile (Just (localized actionRequest "Choose a profile action." "Elige una accion de perfil.")) True))

interpretProfileResendResult :: HarchWeb.ClientActionRequest AppRequestContext -> AccountProfile -> Either ResendVerificationError () -> AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse
interpretProfileResendResult actionRequest profile resendResult =
  let form message = pendingProfileForm actionRequest profile (Just message)
   in case resendResult of
        Right () -> pure (profileResponse actionRequest 202 (form (localized actionRequest "Check your inbox for a verification link." "Revisa tu bandeja de entrada para obtener un enlace de verificacion.") False))
        Left ResendVerificationNoLongerPending -> pure (profileResponse actionRequest 409 (form (localized actionRequest "Your profile state changed. Reload the page before trying again." "El estado de tu perfil ha cambiado. Recarga la pagina antes de intentarlo de nuevo.") True))
        Left (ResendVerificationDeliveryFailed detail) -> throwClientActionFailure (profileResponse actionRequest 502 (form (localized actionRequest "We could not send the verification email. Try again shortly." "No pudimos enviar el correo de verificacion. Intentalo de nuevo en breve.") True)) "account.profile.resend.delivery" "EmailDeliveryError" detail
        Left (ResendVerificationStoreError storeError) -> throwClientActionFailure (profileResponse actionRequest 503 (form (localized actionRequest "Your profile is temporarily unavailable." "Tu perfil no esta disponible temporalmente.") True)) "account.profile.resend.store" "AccountStoreError" (accountStoreErrorDetail storeError)
        Left ResendVerificationClockOverflow -> throwClientActionFailure (profileResponse actionRequest 503 (form (localized actionRequest "Your profile is temporarily unavailable." "Tu perfil no esta disponible temporalmente.") True)) "account.profile.resend.clock" "ClockOverflow" "verification expiry overflowed"

pendingProfileForm :: HarchWeb.ClientActionRequest AppRequestContext -> AccountProfile -> Maybe Text -> Bool -> PendingProfileForm
pendingProfileForm actionRequest profile message isError =
  PendingProfileForm
    { pendingProfileFormEmail = Email.emailAddressText (accountProfileEmail profile),
      pendingProfileFormMessage = message,
      pendingProfileFormIsError = isError,
      pendingProfileFormResendLabel = resendLabel actionRequest
    }

resendLabel :: HarchWeb.ClientActionRequest AppRequestContext -> Text
resendLabel actionRequest = localized actionRequest "Resend verification email" "Reenviar correo de verificacion"

profileLoadErrorType :: ProfileLoadError -> Text
profileLoadErrorType loadError =
  case loadError of
    ProfileSessionStoreError _ -> "AccountSessionStoreError"
    ProfileAccountStoreError _ -> "AccountStoreError"

profileLoadErrorDetail :: ProfileLoadError -> Text
profileLoadErrorDetail loadError =
  case loadError of
    ProfileSessionStoreError storeError -> sessionStoreErrorMessage storeError
    ProfileAccountStoreError storeError -> accountStoreErrorDetail storeError

submittedField :: HarchWeb.ClientActionRequest context -> Text -> Text
submittedField actionRequest name =
  case [value | (fieldName, value) <- HarchWeb.clientActionFields actionRequest, fieldName == name] of
    [value] -> value
    _ -> Text.empty

localized :: HarchWeb.ClientActionRequest AppRequestContext -> Text -> Text -> Text
localized actionRequest english spanish =
  case actionLocale actionRequest of
    English -> english
    Spanish -> spanish

actionLocale :: HarchWeb.ClientActionRequest AppRequestContext -> AppLocale
actionLocale = requestLocale . HarchWeb.clientActionContext

loginProof :: LoginSubmission -> Maybe MfaLoginProof
loginProof submission =
  case loginProofValue submission of
    "totp" -> TotpLoginProof <$> Totp.mkTotpCode (loginCodeValue submission)
    "recovery" -> RecoveryCodeLoginProof <$> RecoveryCode.mkRecoveryCode (loginCodeValue submission)
    _ -> Nothing

mfaErrorMessage :: HarchWeb.ClientActionRequest AppRequestContext -> MfaEnrollmentError -> Text
mfaErrorMessage actionRequest errorValue =
  case errorValue of
    MfaEnrollmentAccountIsNotEligible -> localized actionRequest "Verify your email address before enrolling an authenticator." "Verifica tu direccion de correo antes de registrar un autenticador."
    MfaEnrollmentInvalidCode -> localized actionRequest "That authenticator code is invalid." "Ese codigo de autenticador no es valido."
    MfaEnrollmentNotFound -> localized actionRequest "Start a new authenticator enrollment." "Inicia un nuevo registro de autenticador."
    MfaEnrollmentConfirmationRejected -> localized actionRequest "That enrollment can no longer be confirmed." "Ese registro ya no se puede confirmar."
    _ -> localized actionRequest "Authenticator enrollment is temporarily unavailable." "El registro del autenticador no esta disponible temporalmente."

throwClientActionFailure :: HarchWeb.ClientActionResponse -> Text -> Text -> Text -> AppM HarchWeb.ClientActionResponse value
throwClientActionFailure publicResponse code typeName detail =
  throwAppFailure
    AppFailure
      { appFailurePublic = publicResponse,
        appFailureDiagnostics = buildFailureDiagnostics code typeName detail
      }

buildFailureDiagnostics :: Text -> Text -> Text -> FailureDiagnostics
buildFailureDiagnostics code typeName detail =
  FailureDiagnostics
    { failureCode = code,
      failureType = typeName,
      failureLogEntries = ["ERROR [" <> code <> "] " <> detail]
    }

attachClientActionFailure :: AppFailure HarchWeb.ClientActionResponse -> HarchWeb.ClientActionResponse
attachClientActionFailure failure =
  let publicResponse = appFailurePublic failure
      diagnostics = appFailureDiagnostics failure
   in publicResponse
        { HarchWeb.clientActionObservabilityAttributes =
            HarchWeb.clientActionObservabilityAttributes publicResponse
              <> [ Observability.ObservabilityAttribute "error.type" (Observability.TextAttribute (failureType diagnostics)),
                   Observability.ObservabilityAttribute "app.failure.code" (Observability.TextAttribute (failureCode diagnostics))
                 ],
          HarchWeb.clientActionLogEntries = HarchWeb.clientActionLogEntries publicResponse <> failureLogEntries diagnostics
        }

credentialStoreErrorMessage :: AccountCredentialStoreError -> Text
credentialStoreErrorMessage storeError =
  case storeError of
    AccountCredentialStoreUnavailable detail -> detail
    AccountCredentialStoreCorruptData detail -> detail

accountStoreErrorDetail :: AccountStoreError -> Text
accountStoreErrorDetail storeError =
  case storeError of
    AccountStoreUnavailable detail -> detail
    AccountStoreCorruptData detail -> detail

mfaStoreErrorMessage :: MfaStoreError -> Text
mfaStoreErrorMessage storeError =
  case storeError of
    MfaStoreUnavailable detail -> detail
    MfaStoreCorruptData detail -> detail

sessionStoreErrorMessage :: AccountSessionStoreError -> Text
sessionStoreErrorMessage storeError =
  case storeError of
    AccountSessionStoreUnavailable -> "account session store unavailable"
    AccountSessionStoreCorruptData -> "account session store returned corrupt data"

registrationResponse :: AppLocale -> Text -> Int -> RegistrationForm -> Maybe Text -> HarchWeb.ClientActionResponse
registrationResponse locale registrationPath status form focusId =
  HarchWeb.ClientActionResponse
    { HarchWeb.clientActionStatus = status,
      HarchWeb.clientActionPatches = replaceRegionPatch (registrationRegion locale registrationPath form),
      HarchWeb.clientActionFocusId = focusId,
      HarchWeb.clientActionHeaders = [],
      HarchWeb.clientActionObservabilityAttributes = [],
      HarchWeb.clientActionLogEntries = []
    }

verificationResponse :: AppLocale -> Text -> Int -> VerificationForm -> Maybe Text -> HarchWeb.ClientActionResponse
verificationResponse locale verificationPath status form focusId =
  HarchWeb.ClientActionResponse
    { HarchWeb.clientActionStatus = status,
      HarchWeb.clientActionPatches = replaceRegionPatch (verificationRegion locale verificationPath form),
      HarchWeb.clientActionFocusId = focusId,
      HarchWeb.clientActionHeaders = [],
      HarchWeb.clientActionObservabilityAttributes = [],
      HarchWeb.clientActionLogEntries = []
    }

mfaEnrollmentResponse :: AppLocale -> Text -> Int -> MfaEnrollmentForm -> Maybe Text -> HarchWeb.ClientActionResponse
mfaEnrollmentResponse locale mfaEnrollmentPath status form focusId =
  HarchWeb.ClientActionResponse
    { HarchWeb.clientActionStatus = status,
      HarchWeb.clientActionPatches = replaceRegionPatch (mfaEnrollmentRegion locale mfaEnrollmentPath form),
      HarchWeb.clientActionFocusId = focusId,
      HarchWeb.clientActionHeaders = [],
      HarchWeb.clientActionObservabilityAttributes = [],
      HarchWeb.clientActionLogEntries = []
    }

loginResponse :: AppLocale -> Text -> Int -> LoginForm -> Maybe Text -> Http.ResponseHeaders -> HarchWeb.ClientActionResponse
loginResponse locale loginPath status form focusId headers =
  HarchWeb.ClientActionResponse
    { HarchWeb.clientActionStatus = status,
      HarchWeb.clientActionPatches = replaceRegionPatch (loginRegion locale loginPath form),
      HarchWeb.clientActionFocusId = focusId,
      HarchWeb.clientActionHeaders = headers,
      HarchWeb.clientActionObservabilityAttributes = [],
      HarchWeb.clientActionLogEntries = []
    }

logoutResponse :: AppLocale -> Text -> Int -> Maybe Text -> Bool -> Http.ResponseHeaders -> HarchWeb.ClientActionResponse
logoutResponse locale logoutPath status message isError headers =
  HarchWeb.ClientActionResponse
    { HarchWeb.clientActionStatus = status,
      HarchWeb.clientActionPatches = replaceRegionPatch (logoutRegion locale logoutPath ((,isError) <$> message)),
      HarchWeb.clientActionFocusId = Nothing,
      HarchWeb.clientActionHeaders = headers,
      HarchWeb.clientActionObservabilityAttributes = [],
      HarchWeb.clientActionLogEntries = []
    }

profileResponse :: HarchWeb.ClientActionRequest AppRequestContext -> Int -> PendingProfileForm -> HarchWeb.ClientActionResponse
profileResponse actionRequest status form =
  HarchWeb.ClientActionResponse
    { HarchWeb.clientActionStatus = status,
      HarchWeb.clientActionPatches = replaceRegionPatch (pendingProfileRegion (accountRoutePath actionRequest ProfileRoute) form),
      HarchWeb.clientActionFocusId = Nothing,
      HarchWeb.clientActionHeaders = [],
      HarchWeb.clientActionObservabilityAttributes = [],
      HarchWeb.clientActionLogEntries = []
    }

emailVerificationLifetimeNanoseconds :: Word64
emailVerificationLifetimeNanoseconds = 24 * 60 * 60 * 1000000000

emailLocale :: AppLocale -> Email.EmailLocale
emailLocale locale =
  case locale of
    English -> Email.EmailEnglish
    Spanish -> Email.EmailSpanish

validPassword :: Text -> Bool
validPassword password = Text.length password >= 12

nonEmptyText :: Text -> Maybe Text
nonEmptyText "" = Nothing
nonEmptyText value = Just value
