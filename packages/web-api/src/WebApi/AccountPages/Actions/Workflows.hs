{-# LANGUAGE OverloadedStrings #-}

module WebApi.AccountPages.Actions.Workflows
  ( handleRegistration,
    handleVerification,
    handleMfaEnrollment,
    handleLogin,
    handleLogout,
    handleProfile,
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
import WebApi.Account
  ( AccountProfile (..),
    RegistrationError (..),
    RegistrationResult (..),
    ResendVerificationError (..),
    confirmEmailVerificationAt,
    registerAccountWithIdentityAtWithPasswordHasher,
    resendEmailVerificationAt,
  )
import WebApi.AccountPages.Actions.Common
import WebApi.AccountPages.Forms
import WebApi.AppEffect
  ( AccountWorkflow (..),
    AppFailure (..),
    AppM,
    FailureCode (..),
    FailureDiagnostics,
    liftAppIO,
    throwAppFailure,
  )
import WebApi.Login
  ( LoginIdentifier (..),
    MfaLoginProof (..),
    PasswordMfaLoginResult (..),
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
  ( ProfileState (..),
    loadProfile,
  )
import WebApi.Route
  ( AppRequestContext (..),
    AppRoute (..),
  )
import WebApi.Session
  ( AccountSessionStore (..),
    issueAccountSession,
  )

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

handleRegistration :: HarchWeb.ClientActionRequest AppRequestContext -> AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse
handleRegistration actionRequest = handleRegistrationSubmission actionRequest (registrationSubmission actionRequest)

handleVerification :: HarchWeb.ClientActionRequest AppRequestContext -> AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse
handleVerification actionRequest = handleVerificationSubmission actionRequest (verificationSubmission actionRequest)

handleMfaEnrollment :: HarchWeb.ClientActionRequest AppRequestContext -> AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse
handleMfaEnrollment actionRequest = handleMfaEnrollmentSubmission actionRequest (mfaEnrollmentSubmission actionRequest)

handleLogin :: HarchWeb.ClientActionRequest AppRequestContext -> AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse
handleLogin actionRequest = handleLoginSubmission actionRequest (loginSubmission actionRequest)

handleProfile :: HarchWeb.ClientActionRequest AppRequestContext -> AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse
handleProfile actionRequest = handleProfileSubmission actionRequest (profileSubmission actionRequest)

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

handleRegistrationSubmission :: HarchWeb.ClientActionRequest AppRequestContext -> RegistrationSubmission -> AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse
handleRegistrationSubmission actionRequest submission =
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
        Left (RegistrationDeliveryFailed detail) -> throwClientActionFailure (response 502 (localized actionRequest "We could not send the verification email. Try again shortly." "No pudimos enviar el correo de verificacion. Intentalo de nuevo en breve.") True (Just "registration-email")) RegistrationDeliveryFailure "EmailDeliveryError" detail
        Left (RegistrationStoreError storeError) -> throwClientActionFailure (response 503 (localized actionRequest "Registration is temporarily unavailable." "El registro no esta disponible temporalmente.") True (Just "registration-email")) RegistrationStoreFailure "AccountStoreError" (accountStoreErrorDetail storeError)
        Left RegistrationPasswordHashingFailed -> throwClientActionFailure (response 503 (localized actionRequest "Registration is temporarily unavailable." "El registro no esta disponible temporalmente.") True (Just "registration-email")) RegistrationPasswordHashFailure "PasswordHashingError" "password hashing failed"
        Left RegistrationClockOverflow -> throwClientActionFailure (response 503 (localized actionRequest "Registration is temporarily unavailable." "El registro no esta disponible temporalmente.") True (Just "registration-email")) RegistrationClockFailure "ClockOverflow" "verification expiry overflowed"

handleVerificationSubmission :: HarchWeb.ClientActionRequest AppRequestContext -> VerificationSubmission -> AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse
handleVerificationSubmission actionRequest submission =
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
            Left storeError -> throwClientActionFailure (verificationResponse (actionLocale actionRequest) path 503 (VerificationForm tokenValue (Just (localized actionRequest "Verification is temporarily unavailable." "La verificacion no esta disponible temporalmente.")) True) (Just "verification-token")) VerificationStoreFailure "AccountStoreError" (accountStoreErrorDetail storeError)

handleMfaEnrollmentSubmission :: HarchWeb.ClientActionRequest AppRequestContext -> MfaEnrollmentSubmission -> AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse
handleMfaEnrollmentSubmission actionRequest submission =
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
    Left errorValue -> interpretMfaFailure actionRequest path accountId MfaEnrollmentStartFailure "mfa-account" errorValue

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
        Left errorValue -> interpretMfaFailure actionRequest path accountId MfaEnrollmentConfirmFailure "mfa-code" errorValue

interpretMfaFailure :: HarchWeb.ClientActionRequest AppRequestContext -> Text -> Account.AccountId -> FailureCode -> Text -> MfaEnrollmentError -> AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse
interpretMfaFailure actionRequest path accountId failureCodeValue focusId errorValue =
  let response status = mfaEnrollmentResponse (actionLocale actionRequest) path status (MfaEnrollmentForm (Account.accountIdText accountId) Nothing [] (Just (mfaErrorMessage actionRequest errorValue)) True) (Just focusId)
   in case mfaEnrollmentFailureDiagnostics failureCodeValue errorValue of
        Nothing -> pure (response 422)
        Just diagnostics -> throwAppFailure AppFailure {appFailurePublic = response 503, appFailureDiagnostics = diagnostics}

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

handleLoginSubmission :: HarchWeb.ClientActionRequest AppRequestContext -> LoginSubmission -> AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse
handleLoginSubmission actionRequest submission =
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
        PasswordMfaLoginCredentialStoreError storeError -> throwClientActionFailure (unavailable (Just "login-email")) LoginCredentialStoreFailure "AccountCredentialStoreError" (credentialStoreErrorMessage storeError)
        PasswordMfaLoginMfaStoreError storeError -> throwClientActionFailure (unavailable (Just "login-code")) LoginMfaStoreFailure "MfaStoreError" (mfaStoreErrorMessage storeError)
        PasswordMfaLoginCorruptEnrollment -> throwClientActionFailure (unavailable (Just "login-code")) LoginCorruptEnrollmentFailure "CorruptTotpEnrollment" "stored MFA enrollment could not be decoded"

issueLoginSession :: HarchWeb.ClientActionRequest AppRequestContext -> Text -> Word64 -> Account.AccountId -> AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse
issueLoginSession actionRequest emailValue nowNanoseconds accountId = do
  workflow <- accountWorkflow
  issuedSession <- liftAppIO (issueAccountSession (accountWorkflowSessionStore workflow) accountId nowNanoseconds)
  let path = accountRoutePath actionRequest LoginRoute
      form message = LoginForm emailValue (Just message)
  case issuedSession of
    Left storeError -> throwClientActionFailure (loginResponse (actionLocale actionRequest) path 503 (form (localized actionRequest "Sign-in is temporarily unavailable." "El inicio de sesion no esta disponible temporalmente.") True) (Just "login-email") []) LoginSessionFailure "AccountSessionStoreError" (sessionStoreErrorMessage storeError)
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
            Left storeError -> throwClientActionFailure (logoutResponse (actionLocale actionRequest) path 503 (Just (localized actionRequest "Sign-out is temporarily unavailable." "El cierre de sesion no esta disponible temporalmente.")) True []) LogoutSessionFailure "AccountSessionStoreError" (sessionStoreErrorMessage storeError)
            Right _ -> pure (logoutResponse (actionLocale actionRequest) path 200 (Just (localized actionRequest "You are signed out." "Has cerrado sesion.")) False [("Set-Cookie", TextEncoding.encodeUtf8 (renderSessionCookie (defaultSessionCookiePolicy {sessionCookieMaxAgeSeconds = 0}) sessionToken))])

handleProfileSubmission :: HarchWeb.ClientActionRequest AppRequestContext -> ProfileSubmission -> AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse
handleProfileSubmission actionRequest submission = do
  workflow <- accountWorkflow
  now <- liftAppIO (accountWorkflowClock workflow)
  loadedProfile <- liftAppIO (loadProfile (accountWorkflowSessionStore workflow) (accountWorkflowProfileStore workflow) now (requestSessionId (HarchWeb.clientActionContext actionRequest)))
  case loadedProfile of
    Left loadError -> throwClientActionFailure (profileResponse actionRequest 503 (PendingProfileForm Text.empty (Just (localized actionRequest "Your profile is temporarily unavailable." "Tu perfil no esta disponible temporalmente.")) True (resendLabel actionRequest))) ProfileLoadFailure (profileLoadErrorType loadError) (profileLoadErrorDetail loadError)
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
        Left (ResendVerificationDeliveryFailed detail) -> throwClientActionFailure (profileResponse actionRequest 502 (form (localized actionRequest "We could not send the verification email. Try again shortly." "No pudimos enviar el correo de verificacion. Intentalo de nuevo en breve.") True)) ProfileResendDeliveryFailure "EmailDeliveryError" detail
        Left (ResendVerificationStoreError storeError) -> throwClientActionFailure (profileResponse actionRequest 503 (form (localized actionRequest "Your profile is temporarily unavailable." "Tu perfil no esta disponible temporalmente.") True)) ProfileResendStoreFailure "AccountStoreError" (accountStoreErrorDetail storeError)
        Left ResendVerificationClockOverflow -> throwClientActionFailure (profileResponse actionRequest 503 (form (localized actionRequest "Your profile is temporarily unavailable." "Tu perfil no esta disponible temporalmente.") True)) ProfileResendClockFailure "ClockOverflow" "verification expiry overflowed"

loginProof :: LoginSubmission -> Maybe MfaLoginProof
loginProof submission =
  case loginProofValue submission of
    "totp" -> TotpLoginProof <$> Totp.mkTotpCode (loginCodeValue submission)
    "recovery" -> RecoveryCodeLoginProof <$> RecoveryCode.mkRecoveryCode (loginCodeValue submission)
    _ -> Nothing
