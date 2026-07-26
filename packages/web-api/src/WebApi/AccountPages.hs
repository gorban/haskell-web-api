{-# LANGUAGE OverloadedStrings #-}

module WebApi.AccountPages
  ( AccountWorkflow (..),
    RegistrationForm (..),
    VerificationForm (..),
    PendingProfileForm (..),
    MfaEnrollmentForm (..),
    LoginForm (..),
    emptyRegistrationForm,
    handleAccountAction,
    mfaEnrollmentFailureDiagnostics,
    renderRegistrationPage,
    renderRegistrationRegion,
    renderVerificationPage,
    renderVerificationRegion,
    renderPendingProfileRegion,
    renderMfaEnrollmentPage,
    renderMfaEnrollmentRegion,
    renderLoginPage,
    renderLoginRegion,
    renderLogoutPage,
    renderLogoutRegion,
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

data RegistrationForm = RegistrationForm
  { registrationFormUsername :: Text,
    registrationFormEmail :: Text,
    registrationFormDisplayName :: Text,
    registrationFormMessage :: Maybe Text,
    registrationFormIsError :: Bool
  }
  deriving (Eq)

data VerificationForm = VerificationForm
  { verificationFormToken :: Text,
    verificationFormMessage :: Maybe Text,
    verificationFormIsError :: Bool
  }
  deriving (Eq)

data PendingProfileForm = PendingProfileForm
  { pendingProfileFormEmail :: Text,
    pendingProfileFormMessage :: Maybe Text,
    pendingProfileFormIsError :: Bool,
    pendingProfileFormResendLabel :: Text
  }
  deriving (Eq)

data MfaEnrollmentForm = MfaEnrollmentForm
  { mfaEnrollmentFormAccountId :: Text,
    mfaEnrollmentFormSecret :: Maybe Text,
    mfaEnrollmentFormRecoveryCodes :: [Text],
    mfaEnrollmentFormMessage :: Maybe Text,
    mfaEnrollmentFormIsError :: Bool
  }
  deriving (Eq)

data LoginForm = LoginForm
  { loginFormEmail :: Text,
    loginFormMessage :: Maybe Text,
    loginFormIsError :: Bool
  }
  deriving (Eq)

data AccountPageCopy = AccountPageCopy
  { accountRegistrationHeading :: Text,
    accountUsernameLabel :: Text,
    accountEmailLabel :: Text,
    accountDisplayNameLabel :: Text,
    accountRegistrationPasswordLabel :: Text,
    accountCreateAccountLabel :: Text,
    accountVerificationHeading :: Text,
    accountVerificationTokenLabel :: Text,
    accountVerifyEmailLabel :: Text,
    accountMfaEnrollmentHeading :: Text,
    accountStartMfaEnrollmentLabel :: Text,
    accountConfirmMfaEnrollmentLabel :: Text,
    accountLoginHeading :: Text,
    accountLoginIdentifierLabel :: Text,
    accountLoginPasswordLabel :: Text,
    accountVerificationMethodLabel :: Text,
    accountAuthenticatorCodeLabel :: Text,
    accountRecoveryCodeLabel :: Text,
    accountVerificationCodeLabel :: Text,
    accountSignInLabel :: Text,
    accountLogoutHeading :: Text,
    accountSignOutLabel :: Text,
    accountRecoveryCodesHeading :: Text,
    accountRecoveryCodesInstruction :: Text
  }

accountPageCopy :: AppLocale -> AccountPageCopy
accountPageCopy locale =
  case locale of
    English ->
      AccountPageCopy
        { accountRegistrationHeading = "Create your account",
          accountUsernameLabel = "Username",
          accountEmailLabel = "Email address",
          accountDisplayNameLabel = "Display name (optional)",
          accountRegistrationPasswordLabel = "Password",
          accountCreateAccountLabel = "Create account",
          accountVerificationHeading = "Verify your email address",
          accountVerificationTokenLabel = "Verification token",
          accountVerifyEmailLabel = "Verify email",
          accountMfaEnrollmentHeading = "Set up your authenticator",
          accountStartMfaEnrollmentLabel = "Start authenticator enrollment",
          accountConfirmMfaEnrollmentLabel = "Confirm authenticator",
          accountLoginHeading = "Sign in",
          accountLoginIdentifierLabel = "Email address or username",
          accountLoginPasswordLabel = "Password",
          accountVerificationMethodLabel = "Verification method",
          accountAuthenticatorCodeLabel = "Authenticator code",
          accountRecoveryCodeLabel = "Recovery code",
          accountVerificationCodeLabel = "Verification code",
          accountSignInLabel = "Sign in",
          accountLogoutHeading = "Sign out",
          accountSignOutLabel = "Sign out",
          accountRecoveryCodesHeading = "Recovery codes",
          accountRecoveryCodesInstruction = "Save these codes. They will not be shown again."
        }
    Spanish ->
      AccountPageCopy
        { accountRegistrationHeading = "Crea tu cuenta",
          accountUsernameLabel = "Nombre de usuario",
          accountEmailLabel = "Direccion de correo",
          accountDisplayNameLabel = "Nombre para mostrar (opcional)",
          accountRegistrationPasswordLabel = "Contrasena",
          accountCreateAccountLabel = "Crear cuenta",
          accountVerificationHeading = "Verifica tu direccion de correo",
          accountVerificationTokenLabel = "Token de verificacion",
          accountVerifyEmailLabel = "Verificar correo",
          accountMfaEnrollmentHeading = "Configura tu autenticador",
          accountStartMfaEnrollmentLabel = "Iniciar registro del autenticador",
          accountConfirmMfaEnrollmentLabel = "Confirmar autenticador",
          accountLoginHeading = "Iniciar sesion",
          accountLoginIdentifierLabel = "Direccion de correo o nombre de usuario",
          accountLoginPasswordLabel = "Contrasena",
          accountVerificationMethodLabel = "Metodo de verificacion",
          accountAuthenticatorCodeLabel = "Codigo del autenticador",
          accountRecoveryCodeLabel = "Codigo de recuperacion",
          accountVerificationCodeLabel = "Codigo de verificacion",
          accountSignInLabel = "Iniciar sesion",
          accountLogoutHeading = "Cerrar sesion",
          accountSignOutLabel = "Cerrar sesion",
          accountRecoveryCodesHeading = "Codigos de recuperacion",
          accountRecoveryCodesInstruction = "Guarda estos codigos. No se mostraran de nuevo."
        }

emptyRegistrationForm :: RegistrationForm
emptyRegistrationForm = RegistrationForm Text.empty Text.empty Text.empty Nothing False

handleAccountAction :: AccountWorkflow -> HarchWeb.ClientActionRequest AppRequestContext -> IO (Maybe HarchWeb.ClientActionResponse)
handleAccountAction workflow actionRequest =
  case selectedAccountAction actionRequest of
    Nothing -> pure Nothing
    Just action -> do
      result <- runAppM (AppServices workflow) action
      pure (Just (either attachClientActionFailure id result))

selectedAccountAction :: HarchWeb.ClientActionRequest AppRequestContext -> Maybe (AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse)
selectedAccountAction actionRequest =
  if HarchWeb.clientActionMethod actionRequest /= "POST"
    then Nothing
    else
      lookup
        (HarchWeb.clientActionPath actionRequest)
        [ (accountRoutePath actionRequest RegistrationRoute, handleRegistration actionRequest),
          (accountRoutePath actionRequest EmailVerificationRoute, handleVerification actionRequest),
          (accountRoutePath actionRequest MfaEnrollmentRoute, handleMfaEnrollment actionRequest),
          (accountRoutePath actionRequest LoginRoute, handleLogin actionRequest),
          (accountRoutePath actionRequest ProfileRoute, handleProfile actionRequest),
          (accountRoutePath actionRequest LogoutRoute, handleLogout actionRequest)
        ]

accountRoutePath :: HarchWeb.ClientActionRequest AppRequestContext -> AppRoute -> Text
accountRoutePath actionRequest route =
  renderRoutePath
    HarchWeb.RouteRequest
      { HarchWeb.requestRoute = route,
        HarchWeb.requestContext = HarchWeb.clientActionContext actionRequest
      }

accountWorkflow :: AppM publicFailure AccountWorkflow
accountWorkflow = appAccountWorkflow <$> askAppServices

handleRegistration :: HarchWeb.ClientActionRequest AppRequestContext -> AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse
handleRegistration actionRequest =
  case parseRegistrationForm actionRequest of
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

parseRegistrationForm :: HarchWeb.ClientActionRequest AppRequestContext -> Either HarchWeb.ClientActionResponse (Text, Text, Text, Text, Username.Username, Email.EmailAddress)
parseRegistrationForm actionRequest =
  let usernameValue = actionField actionRequest "username"
      emailValue = actionField actionRequest "email"
      displayNameValue = actionField actionRequest "displayName"
      passwordValue = actionField actionRequest "password"
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

handleVerification :: HarchWeb.ClientActionRequest AppRequestContext -> AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse
handleVerification actionRequest =
  let tokenValue = actionField actionRequest "token"
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

handleMfaEnrollment :: HarchWeb.ClientActionRequest AppRequestContext -> AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse
handleMfaEnrollment actionRequest =
  let accountValue = actionField actionRequest "account"
      path = accountRoutePath actionRequest MfaEnrollmentRoute
   in case Account.mkAccountId accountValue of
        Nothing -> pure (mfaEnrollmentResponse (actionLocale actionRequest) path 422 (MfaEnrollmentForm accountValue Nothing [] (Just (localized actionRequest "The enrollment link is invalid." "El enlace de registro no es valido.")) True) (Just "mfa-account"))
        Just accountId ->
          case actionField actionRequest "intent" of
            "start" -> startMfaAction actionRequest path accountId
            "confirm" -> confirmMfaAction actionRequest path accountId
            _ -> pure (mfaEnrollmentResponse (actionLocale actionRequest) path 422 (MfaEnrollmentForm (Account.accountIdText accountId) Nothing [] (Just (localized actionRequest "Choose an enrollment action." "Elige una accion de registro.")) True) (Just "mfa-account"))

startMfaAction :: HarchWeb.ClientActionRequest AppRequestContext -> Text -> Account.AccountId -> AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse
startMfaAction actionRequest path accountId = do
  workflow <- accountWorkflow
  now <- liftAppIO (accountWorkflowClock workflow)
  started <- liftAppIO (startMfaEnrollment (accountWorkflowMfaStore workflow) (accountWorkflowTotpEncryptionKey workflow) accountId now)
  case started of
    Right (MfaEnrollmentStart secret) -> pure (mfaEnrollmentResponse (actionLocale actionRequest) path 200 (MfaEnrollmentForm (Account.accountIdText accountId) (Just (Totp.renderTotpSecret secret)) [] (Just (localized actionRequest "Add this secret to your authenticator, then enter its six-digit code." "Agrega este secreto a tu autenticador y luego introduce su codigo de seis digitos.")) False) (Just "mfa-code"))
    Left errorValue -> interpretMfaFailure actionRequest path accountId "start" "mfa-account" errorValue

confirmMfaAction :: HarchWeb.ClientActionRequest AppRequestContext -> Text -> Account.AccountId -> AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse
confirmMfaAction actionRequest path accountId =
  case Totp.mkTotpCode (actionField actionRequest "code") of
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

handleLogin :: HarchWeb.ClientActionRequest AppRequestContext -> AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse
handleLogin actionRequest =
  case parseLoginForm actionRequest of
    Left response -> pure response
    Right (emailValue, passwordValue, identifier, proof) -> do
      workflow <- accountWorkflow
      nowNanoseconds <- liftAppIO (accountWorkflowClock workflow)
      nowSeconds <- liftAppIO (accountWorkflowTotpClock workflow)
      loginResult <- liftAppIO (completePasswordLoginWithIdentifier (accountWorkflowCredentialStore workflow) (accountWorkflowMfaStore workflow) (accountWorkflowTotpEncryptionKey workflow) nowNanoseconds nowSeconds identifier (Password.mkPassword passwordValue) proof)
      interpretLoginResult actionRequest emailValue nowNanoseconds loginResult

parseLoginForm :: HarchWeb.ClientActionRequest AppRequestContext -> Either HarchWeb.ClientActionResponse (Text, Text, LoginIdentifier, MfaLoginProof)
parseLoginForm actionRequest =
  let emailValue = actionField actionRequest "email"
      usernameValue = actionField actionRequest "username"
      passwordValue = actionField actionRequest "password"
      path = accountRoutePath actionRequest LoginRoute
      loginForm message = LoginForm emailValue (Just message)
      maybeIdentifier =
        (LoginEmailAddress <$> Email.mkEmailAddress emailValue)
          <|> (LoginUsername <$> Username.mkUsername emailValue)
          <|> (LoginUsername <$> Username.mkUsername usernameValue)
   in case (maybeIdentifier, validPassword passwordValue, loginProof actionRequest) of
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

handleProfile :: HarchWeb.ClientActionRequest AppRequestContext -> AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse
handleProfile actionRequest = do
  workflow <- accountWorkflow
  now <- liftAppIO (accountWorkflowClock workflow)
  loadedProfile <- liftAppIO (loadProfile (accountWorkflowSessionStore workflow) (accountWorkflowProfileStore workflow) now (requestSessionId (HarchWeb.clientActionContext actionRequest)))
  case loadedProfile of
    Left loadError -> throwClientActionFailure (profileResponse actionRequest 503 (PendingProfileForm Text.empty (Just (localized actionRequest "Your profile is temporarily unavailable." "Tu perfil no esta disponible temporalmente.")) True (resendLabel actionRequest))) "account.profile.load" (profileLoadErrorType loadError) (profileLoadErrorDetail loadError)
    Right ProfileUnauthenticated -> pure (profileResponse actionRequest 403 (PendingProfileForm Text.empty (Just (localized actionRequest "Sign in before requesting another verification email." "Inicia sesion antes de solicitar otro correo de verificacion.")) True (resendLabel actionRequest)))
    Right (ProfileAuthenticated profile) -> pure (profileResponse actionRequest 409 (PendingProfileForm (Email.emailAddressText (accountProfileEmail profile)) (Just (localized actionRequest "Your email address is already verified." "Tu direccion de correo ya esta verificada.")) True (resendLabel actionRequest)))
    Right (ProfilePending profile) -> handlePendingProfile actionRequest workflow now profile

handlePendingProfile :: HarchWeb.ClientActionRequest AppRequestContext -> AccountWorkflow -> Word64 -> AccountProfile -> AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse
handlePendingProfile actionRequest workflow now profile =
  case actionField actionRequest "intent" of
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

actionField :: HarchWeb.ClientActionRequest context -> Text -> Text
actionField actionRequest name =
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

loginProof :: HarchWeb.ClientActionRequest context -> Maybe MfaLoginProof
loginProof actionRequest =
  case actionField actionRequest "proof" of
    "totp" -> TotpLoginProof <$> Totp.mkTotpCode (actionField actionRequest "code")
    "recovery" -> RecoveryCodeLoginProof <$> RecoveryCode.mkRecoveryCode (actionField actionRequest "code")
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
      HarchWeb.clientActionPatches = [HarchWeb.RegionPatch "registration-region" (renderRegistrationRegion locale registrationPath form)],
      HarchWeb.clientActionFocusId = focusId,
      HarchWeb.clientActionHeaders = [],
      HarchWeb.clientActionObservabilityAttributes = [],
      HarchWeb.clientActionLogEntries = []
    }

verificationResponse :: AppLocale -> Text -> Int -> VerificationForm -> Maybe Text -> HarchWeb.ClientActionResponse
verificationResponse locale verificationPath status form focusId =
  HarchWeb.ClientActionResponse
    { HarchWeb.clientActionStatus = status,
      HarchWeb.clientActionPatches = [HarchWeb.RegionPatch "verification-region" (renderVerificationRegion locale verificationPath form)],
      HarchWeb.clientActionFocusId = focusId,
      HarchWeb.clientActionHeaders = [],
      HarchWeb.clientActionObservabilityAttributes = [],
      HarchWeb.clientActionLogEntries = []
    }

mfaEnrollmentResponse :: AppLocale -> Text -> Int -> MfaEnrollmentForm -> Maybe Text -> HarchWeb.ClientActionResponse
mfaEnrollmentResponse locale mfaEnrollmentPath status form focusId =
  HarchWeb.ClientActionResponse
    { HarchWeb.clientActionStatus = status,
      HarchWeb.clientActionPatches = [HarchWeb.RegionPatch "mfa-enrollment-region" (renderMfaEnrollmentRegion locale mfaEnrollmentPath form)],
      HarchWeb.clientActionFocusId = focusId,
      HarchWeb.clientActionHeaders = [],
      HarchWeb.clientActionObservabilityAttributes = [],
      HarchWeb.clientActionLogEntries = []
    }

loginResponse :: AppLocale -> Text -> Int -> LoginForm -> Maybe Text -> Http.ResponseHeaders -> HarchWeb.ClientActionResponse
loginResponse locale loginPath status form focusId headers =
  HarchWeb.ClientActionResponse
    { HarchWeb.clientActionStatus = status,
      HarchWeb.clientActionPatches = [HarchWeb.RegionPatch "login-region" (renderLoginRegion locale loginPath form)],
      HarchWeb.clientActionFocusId = focusId,
      HarchWeb.clientActionHeaders = headers,
      HarchWeb.clientActionObservabilityAttributes = [],
      HarchWeb.clientActionLogEntries = []
    }

logoutResponse :: AppLocale -> Text -> Int -> Maybe Text -> Bool -> Http.ResponseHeaders -> HarchWeb.ClientActionResponse
logoutResponse locale logoutPath status message isError headers =
  HarchWeb.ClientActionResponse
    { HarchWeb.clientActionStatus = status,
      HarchWeb.clientActionPatches = [HarchWeb.RegionPatch "logout-region" (renderLogoutRegion locale logoutPath message isError)],
      HarchWeb.clientActionFocusId = Nothing,
      HarchWeb.clientActionHeaders = headers,
      HarchWeb.clientActionObservabilityAttributes = [],
      HarchWeb.clientActionLogEntries = []
    }

profileResponse :: HarchWeb.ClientActionRequest AppRequestContext -> Int -> PendingProfileForm -> HarchWeb.ClientActionResponse
profileResponse actionRequest status form =
  HarchWeb.ClientActionResponse
    { HarchWeb.clientActionStatus = status,
      HarchWeb.clientActionPatches = [HarchWeb.RegionPatch "profile-region" (renderPendingProfileRegion (accountRoutePath actionRequest ProfileRoute) form)],
      HarchWeb.clientActionFocusId = Nothing,
      HarchWeb.clientActionHeaders = [],
      HarchWeb.clientActionObservabilityAttributes = [],
      HarchWeb.clientActionLogEntries = []
    }

renderMfaEnrollmentPage :: AppLocale -> Text -> MfaEnrollmentForm -> Text
renderMfaEnrollmentPage locale mfaEnrollmentPath form =
  let copy = accountPageCopy locale
   in Text.concat
        [ "<section data-page=\"mfa-enrollment\"><h1 data-page-title=\"true\">",
          accountMfaEnrollmentHeading copy,
          "</h1>",
          renderMfaEnrollmentRegion locale mfaEnrollmentPath form,
          "</section>"
        ]

renderMfaEnrollmentRegion :: AppLocale -> Text -> MfaEnrollmentForm -> Text
renderMfaEnrollmentRegion locale mfaEnrollmentPath form =
  let copy = accountPageCopy locale
   in Text.concat
        [ "<section id=\"mfa-enrollment-region\" aria-live=\"polite\">",
          renderMessage (mfaEnrollmentFormMessage form) (mfaEnrollmentFormIsError form),
          renderEnrollmentSecret (mfaEnrollmentFormSecret form),
          renderRecoveryCodes copy (mfaEnrollmentFormRecoveryCodes form),
          "<form data-harch-action=\"true\" data-harch-control action=\"",
          escapeHtml mfaEnrollmentPath,
          "\" method=\"post\"><input id=\"mfa-account\" name=\"account\" type=\"hidden\" value=\"",
          escapeHtml (mfaEnrollmentFormAccountId form),
          "\"><input name=\"intent\" type=\"hidden\" value=\"start\"><button type=\"submit\">",
          accountStartMfaEnrollmentLabel copy,
          "</button></form>",
          renderConfirmationForm locale mfaEnrollmentPath form,
          "</section>"
        ]

renderLoginPage :: AppLocale -> Text -> LoginForm -> Text
renderLoginPage locale loginPath form =
  let copy = accountPageCopy locale
   in Text.concat
        [ "<section data-page=\"login\"><h1 data-page-title=\"true\">",
          accountLoginHeading copy,
          "</h1>",
          renderLoginRegion locale loginPath form,
          "</section>"
        ]

renderLoginRegion :: AppLocale -> Text -> LoginForm -> Text
renderLoginRegion locale loginPath form =
  let copy = accountPageCopy locale
   in Text.concat
        [ "<section id=\"login-region\" aria-live=\"polite\">",
          renderMessage (loginFormMessage form) (loginFormIsError form),
          "<form data-harch-action=\"true\" data-harch-control action=\"",
          escapeHtml loginPath,
          "\" method=\"post\"><label for=\"login-email\">",
          accountLoginIdentifierLabel copy,
          "</label><input id=\"login-email\" name=\"email\" type=\"text\" autocomplete=\"username\" required value=\"",
          escapeHtml (loginFormEmail form),
          "\"><label for=\"login-password\">",
          accountLoginPasswordLabel copy,
          "</label><input id=\"login-password\" name=\"password\" type=\"password\" autocomplete=\"current-password\" required><label for=\"login-proof\">",
          accountVerificationMethodLabel copy,
          "</label><select id=\"login-proof\" name=\"proof\"><option value=\"totp\">",
          accountAuthenticatorCodeLabel copy,
          "</option><option value=\"recovery\">",
          accountRecoveryCodeLabel copy,
          "</option></select><label for=\"login-code\">",
          accountVerificationCodeLabel copy,
          "</label><input id=\"login-code\" name=\"code\" autocomplete=\"one-time-code\" required><button type=\"submit\">",
          accountSignInLabel copy,
          "</button></form></section>"
        ]

renderPendingProfileRegion :: Text -> PendingProfileForm -> Text
renderPendingProfileRegion profilePath form =
  Text.concat
    [ "<section id=\"profile-region\" aria-live=\"polite\">",
      renderMessage (pendingProfileFormMessage form) (pendingProfileFormIsError form),
      "<p data-profile-email=\"true\">",
      escapeHtml (pendingProfileFormEmail form),
      "</p><form data-profile-resend=\"true\" data-harch-action=\"true\" data-harch-control action=\"",
      escapeHtml profilePath,
      "\" method=\"post\"><input name=\"intent\" type=\"hidden\" value=\"resend-verification\"><button type=\"submit\">",
      escapeHtml (pendingProfileFormResendLabel form),
      "</button></form></section>"
    ]

renderLogoutPage :: AppLocale -> Text -> Text
renderLogoutPage locale logoutPath =
  let copy = accountPageCopy locale
   in Text.concat
        [ "<section data-page=\"logout\"><h1 data-page-title=\"true\">",
          accountLogoutHeading copy,
          "</h1>",
          renderLogoutRegionWithMessage locale logoutPath Nothing,
          "</section>"
        ]

renderLogoutRegion :: AppLocale -> Text -> Maybe Text -> Bool -> Text
renderLogoutRegion locale logoutPath message isError =
  renderLogoutRegionWithMessage locale logoutPath ((,isError) <$> message)

renderLogoutRegionWithMessage :: AppLocale -> Text -> Maybe (Text, Bool) -> Text
renderLogoutRegionWithMessage locale logoutPath messageState =
  let copy = accountPageCopy locale
   in Text.concat
        [ "<section id=\"logout-region\" aria-live=\"polite\">",
          maybe Text.empty renderLogoutMessage messageState,
          "<form data-harch-action=\"true\" data-harch-control action=\"",
          escapeHtml logoutPath,
          "\" method=\"post\"><button type=\"submit\">",
          accountSignOutLabel copy,
          "</button></form></section>"
        ]
  where
    renderLogoutMessage (message, isError) = renderMessage (Just message) isError

renderEnrollmentSecret :: Maybe Text -> Text
renderEnrollmentSecret maybeSecret =
  case maybeSecret of
    Nothing -> Text.empty
    Just secret -> Text.concat ["<p data-totp-secret=\"true\"><code>", escapeHtml secret, "</code></p>"]

renderRecoveryCodes :: AccountPageCopy -> [Text] -> Text
renderRecoveryCodes copy recoveryCodes =
  case recoveryCodes of
    [] -> Text.empty
    _ -> Text.concat ["<section data-recovery-codes=\"true\"><h2>", accountRecoveryCodesHeading copy, "</h2><p>", accountRecoveryCodesInstruction copy, "</p><ul>", Text.concat (map (\code -> Text.concat ["<li><code>", escapeHtml code, "</code></li>"]) recoveryCodes), "</ul></section>"]

renderConfirmationForm :: AppLocale -> Text -> MfaEnrollmentForm -> Text
renderConfirmationForm locale mfaEnrollmentPath form =
  case mfaEnrollmentFormSecret form of
    Nothing -> Text.empty
    Just _ ->
      let copy = accountPageCopy locale
       in Text.concat
            [ "<form data-harch-action=\"true\" data-harch-control action=\"",
              escapeHtml mfaEnrollmentPath,
              "\" method=\"post\"><input name=\"account\" type=\"hidden\" value=\"",
              escapeHtml (mfaEnrollmentFormAccountId form),
              "\"><input name=\"intent\" type=\"hidden\" value=\"confirm\"><label for=\"mfa-code\">",
              accountAuthenticatorCodeLabel copy,
              "</label><input id=\"mfa-code\" name=\"code\" inputmode=\"numeric\" autocomplete=\"one-time-code\" required><button type=\"submit\">",
              accountConfirmMfaEnrollmentLabel copy,
              "</button></form>"
            ]

renderRegistrationPage :: AppLocale -> Text -> RegistrationForm -> Text
renderRegistrationPage locale registrationPath form =
  let copy = accountPageCopy locale
   in Text.concat
        [ "<section data-page=\"registration\"><h1 data-page-title=\"true\">",
          accountRegistrationHeading copy,
          "</h1>",
          renderRegistrationRegion locale registrationPath form,
          "</section>"
        ]

renderRegistrationRegion :: AppLocale -> Text -> RegistrationForm -> Text
renderRegistrationRegion locale registrationPath form =
  let copy = accountPageCopy locale
   in Text.concat
        [ "<section id=\"registration-region\" aria-live=\"polite\">",
          renderMessage (registrationFormMessage form) (registrationFormIsError form),
          "<form data-harch-action=\"true\" data-harch-control action=\"",
          escapeHtml registrationPath,
          "\" method=\"post\"><label for=\"registration-username\">",
          accountUsernameLabel copy,
          "</label><input id=\"registration-username\" name=\"username\" autocomplete=\"username\" minlength=\"3\" maxlength=\"20\" required value=\"",
          escapeHtml (registrationFormUsername form),
          "\"><label for=\"registration-email\">",
          accountEmailLabel copy,
          "</label><input id=\"registration-email\" name=\"email\" type=\"email\" autocomplete=\"email\" required value=\"",
          escapeHtml (registrationFormEmail form),
          "\"><label for=\"registration-display-name\">",
          accountDisplayNameLabel copy,
          "</label><input id=\"registration-display-name\" name=\"displayName\" autocomplete=\"name\" value=\"",
          escapeHtml (registrationFormDisplayName form),
          "\"><label for=\"registration-password\">",
          accountRegistrationPasswordLabel copy,
          "</label><input id=\"registration-password\" name=\"password\" type=\"password\" autocomplete=\"new-password\" minlength=\"12\" required><button type=\"submit\">",
          accountCreateAccountLabel copy,
          "</button></form></section>"
        ]

renderVerificationPage :: AppLocale -> Text -> VerificationForm -> Text
renderVerificationPage locale verificationPath form =
  let copy = accountPageCopy locale
   in Text.concat
        [ "<section data-page=\"email-verification\"><h1 data-page-title=\"true\">",
          accountVerificationHeading copy,
          "</h1>",
          renderVerificationRegion locale verificationPath form,
          "</section>"
        ]

renderVerificationRegion :: AppLocale -> Text -> VerificationForm -> Text
renderVerificationRegion locale verificationPath form =
  let copy = accountPageCopy locale
   in Text.concat
        [ "<section id=\"verification-region\" aria-live=\"polite\">",
          renderMessage (verificationFormMessage form) (verificationFormIsError form),
          "<form data-harch-action=\"true\" data-harch-control action=\"",
          escapeHtml verificationPath,
          "\" method=\"post\"><label for=\"verification-token\">",
          accountVerificationTokenLabel copy,
          "</label><input id=\"verification-token\" name=\"token\" autocomplete=\"one-time-code\" required value=\"",
          escapeHtml (verificationFormToken form),
          "\"><button type=\"submit\">",
          accountVerifyEmailLabel copy,
          "</button></form></section>"
        ]

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

renderMessage :: Maybe Text -> Bool -> Text
renderMessage maybeMessage isError =
  case isError of
    False -> renderMessageWithState maybeMessage Text.empty
    True -> renderMessageWithState maybeMessage " data-error-state=\"true\""

renderMessageWithState :: Maybe Text -> Text -> Text
renderMessageWithState maybeMessage stateAttribute =
  case maybeMessage of
    Nothing -> Text.empty
    Just message ->
      Text.concat
        [ "<p data-account-message=\"true\"",
          stateAttribute,
          ">",
          escapeHtml message,
          "</p>"
        ]

escapeHtml :: Text -> Text
escapeHtml =
  Text.concatMap
    ( \character ->
        case character of
          '&' -> "&amp;"
          '<' -> "&lt;"
          '>' -> "&gt;"
          '\"' -> "&quot;"
          '\'' -> "&#39;"
          _ -> Text.singleton character
    )
