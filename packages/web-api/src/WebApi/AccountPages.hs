{-# LANGUAGE OverloadedStrings #-}

module WebApi.AccountPages
  ( AccountWorkflow (..),
    RegistrationForm (..),
    VerificationForm (..),
    MfaEnrollmentForm (..),
    LoginForm (..),
    emptyRegistrationForm,
    handleAccountAction,
    renderRegistrationPage,
    renderRegistrationRegion,
    renderVerificationPage,
    renderVerificationRegion,
    renderMfaEnrollmentPage,
    renderMfaEnrollmentRegion,
    renderLoginPage,
    renderLoginRegion,
    renderLogoutPage,
    renderLogoutRegion,
  )
where

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
import HarchWeb.Secret (SecretEncryptionKey)
import HarchWeb.Session
  ( defaultSessionCookiePolicy,
    renderSessionCookie,
    sessionCookieMaxAgeSeconds,
    sessionId,
  )
import HarchWeb.Totp qualified as Totp
import Network.HTTP.Types qualified as Http
import WebApi.Account
  ( AccountStore,
    AccountStoreError (..),
    RegistrationError (..),
    RegistrationResult (..),
    confirmEmailVerificationAt,
    registerAccountAt,
  )
import WebApi.Login
  ( AccountCredentialStore,
    MfaLoginProof (..),
    PasswordMfaLoginResult (..),
    completePasswordLogin,
  )
import WebApi.Mfa (MfaStore)
import WebApi.MfaEnrollment
  ( MfaEnrollmentConfirmation (..),
    MfaEnrollmentError (..),
    MfaEnrollmentStart (..),
    confirmMfaEnrollment,
    startMfaEnrollment,
  )
import WebApi.Route
  ( AppLocale (..),
    AppRequestContext (..),
    AppRoute (..),
    renderRoutePath,
  )
import WebApi.Session
  ( AccountSessionStore (..),
    issueAccountSession,
  )

data AccountWorkflow = AccountWorkflow
  { accountWorkflowStore :: AccountStore,
    accountWorkflowEmailDelivery :: Email.EmailDelivery,
    accountWorkflowClock :: IO Word64,
    accountWorkflowMfaStore :: MfaStore,
    accountWorkflowCredentialStore :: AccountCredentialStore,
    accountWorkflowSessionStore :: AccountSessionStore,
    accountWorkflowTotpEncryptionKey :: SecretEncryptionKey,
    accountWorkflowTotpClock :: IO Word64,
    accountWorkflowVerificationUrl :: AppRequestContext -> Account.EmailVerificationToken -> Text
  }

data RegistrationForm = RegistrationForm
  { registrationFormEmail :: Text,
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

emptyRegistrationForm :: RegistrationForm
emptyRegistrationForm = RegistrationForm Text.empty Nothing False

handleAccountAction :: AccountWorkflow -> HarchWeb.ClientActionRequest AppRequestContext -> IO (Maybe HarchWeb.ClientActionResponse)
handleAccountAction workflow actionRequest =
  case HarchWeb.clientActionMethod actionRequest of
    "POST" ->
      case HarchWeb.clientActionPath actionRequest of
        path
          | path == registrationPath -> Just <$> handleRegistration
          | path == verificationPath -> Just <$> handleVerification
          | path == mfaEnrollmentPath -> Just <$> handleMfaEnrollment
          | path == loginPath -> Just <$> handleLogin
          | path == logoutPath -> Just <$> handleLogout
        _ -> pure Nothing
    _ -> pure Nothing
  where
    actionContext = HarchWeb.clientActionContext actionRequest
    registrationPath = renderRoutePath (routeRequest RegistrationRoute)
    verificationPath = renderRoutePath (routeRequest EmailVerificationRoute)
    mfaEnrollmentPath = renderRoutePath (routeRequest MfaEnrollmentRoute)
    loginPath = renderRoutePath (routeRequest LoginRoute)
    logoutPath = renderRoutePath (routeRequest LogoutRoute)
    routeRequest route = HarchWeb.RouteRequest {HarchWeb.requestRoute = route, HarchWeb.requestContext = actionContext}

    handleRegistration = do
      let emailValue = actionField "email"
          passwordValue = actionField "password"
      case (Email.mkEmailAddress emailValue, validPassword passwordValue) of
        (Nothing, _) ->
          pure (registrationResponse registrationPath 422 (RegistrationForm emailValue (Just (localized "Enter a valid email address." "Introduce una direccion de correo valida.")) True) (Just "registration-email"))
        (_, False) ->
          pure (registrationResponse registrationPath 422 (RegistrationForm emailValue (Just (localized "Use a password with at least 12 characters." "Usa una contrasena de al menos 12 caracteres.")) True) (Just "registration-password"))
        (Just emailAddress, True) -> do
          now <- accountWorkflowClock workflow
          registrationResult <-
            registerAccountAt
              Password.defaultPasswordHashingPolicy
              (accountWorkflowStore workflow)
              (accountWorkflowEmailDelivery workflow)
              (emailLocale (requestLocale actionContext))
              (accountWorkflowVerificationUrl workflow actionContext)
              now
              emailVerificationLifetimeNanoseconds
              emailAddress
              (Password.mkPassword passwordValue)
          pure $
            case registrationResult of
              Right RegistrationAlreadyRegistered -> registrationResponse registrationPath 202 (RegistrationForm emailValue (Just (localized "If that address can register, check its inbox for a verification link." "Si esa direccion puede registrarse, revisa su bandeja de entrada para obtener un enlace de verificacion.")) False) Nothing
              Right (RegistrationCreated _) -> registrationResponse registrationPath 202 (RegistrationForm emailValue (Just (localized "Check your inbox for a verification link." "Revisa tu bandeja de entrada para obtener un enlace de verificacion.")) False) Nothing
              Left (RegistrationStoreError storeError) -> registrationResponse registrationPath 503 (RegistrationForm emailValue (Just (storeErrorMessage storeError)) True) (Just "registration-email")
              Left (RegistrationDeliveryFailed _) -> registrationResponse registrationPath 502 (RegistrationForm emailValue (Just (localized "We could not send the verification email. Try again shortly." "No pudimos enviar el correo de verificacion. Intentalo de nuevo en breve.")) True) (Just "registration-email")
              Left _ -> registrationResponse registrationPath 503 (RegistrationForm emailValue (Just (localized "Registration is temporarily unavailable." "El registro no esta disponible temporalmente.")) True) (Just "registration-email")

    handleVerification = do
      let tokenValue = actionField "token"
      case Account.mkEmailVerificationToken tokenValue of
        Nothing -> pure (verificationResponse verificationPath 422 (VerificationForm tokenValue (Just (localized "The verification link is invalid." "El enlace de verificacion no es valido.")) True) (Just "verification-token"))
        Just token -> do
          now <- accountWorkflowClock workflow
          confirmationResult <- confirmEmailVerificationAt (accountWorkflowStore workflow) now token
          pure $
            case confirmationResult of
              Right (Account.EmailVerificationAccepted _ _) -> verificationResponse verificationPath 200 (VerificationForm Text.empty (Just (localized "Your email address is verified. Enroll your authenticator next." "Tu direccion de correo esta verificada. A continuacion, registra tu autenticador.")) False) Nothing
              Right Account.EmailVerificationExpired -> verificationResponse verificationPath 422 (VerificationForm tokenValue (Just (localized "That verification link has expired." "Ese enlace de verificacion ha caducado.")) True) (Just "verification-token")
              Right Account.EmailVerificationRejected -> verificationResponse verificationPath 422 (VerificationForm tokenValue (Just (localized "That verification link is invalid or has already been used." "Ese enlace de verificacion no es valido o ya se ha utilizado.")) True) (Just "verification-token")
              Left storeError -> verificationResponse verificationPath 503 (VerificationForm tokenValue (Just (storeErrorMessage storeError)) True) (Just "verification-token")

    handleMfaEnrollment =
      case Account.mkAccountId (actionField "account") of
        Nothing -> pure (mfaEnrollmentResponse mfaEnrollmentPath 422 (mfaForm (actionField "account") Nothing [] (Just (localized "The enrollment link is invalid." "El enlace de registro no es valido.")) True) (Just "mfa-account"))
        Just accountId ->
          case actionField "intent" of
            "start" -> do
              now <- accountWorkflowClock workflow
              started <- startMfaEnrollment (accountWorkflowMfaStore workflow) (accountWorkflowTotpEncryptionKey workflow) accountId now
              pure $ case started of
                Right (MfaEnrollmentStart secret) -> mfaEnrollmentResponse mfaEnrollmentPath 200 (mfaForm (Account.accountIdText accountId) (Just (Totp.renderTotpSecret secret)) [] (Just (localized "Add this secret to your authenticator, then enter its six-digit code." "Agrega este secreto a tu autenticador y luego introduce su codigo de seis digitos.")) False) (Just "mfa-code")
                Left errorValue -> mfaEnrollmentResponse mfaEnrollmentPath 422 (mfaForm (Account.accountIdText accountId) Nothing [] (Just (mfaErrorMessage errorValue)) True) (Just "mfa-account")
            "confirm" ->
              case Totp.mkTotpCode (actionField "code") of
                Nothing -> pure (mfaEnrollmentResponse mfaEnrollmentPath 422 (mfaForm (Account.accountIdText accountId) Nothing [] (Just (localized "Enter a six-digit authenticator code." "Introduce un codigo de autenticador de seis digitos.")) True) (Just "mfa-code"))
                Just code -> do
                  nowNanoseconds <- accountWorkflowClock workflow
                  nowSeconds <- accountWorkflowTotpClock workflow
                  confirmed <- confirmMfaEnrollment Password.defaultPasswordHashingPolicy (accountWorkflowMfaStore workflow) (accountWorkflowTotpEncryptionKey workflow) accountId nowNanoseconds nowSeconds code
                  pure $ case confirmed of
                    Right (MfaEnrollmentConfirmation recoveryCodes) -> mfaEnrollmentResponse mfaEnrollmentPath 200 (mfaForm (Account.accountIdText accountId) Nothing (map RecoveryCode.recoveryCodeText (toList recoveryCodes)) (Just (localized "Authenticator enrolled. Save these recovery codes now." "Autenticador registrado. Guarda estos codigos de recuperacion ahora.")) False) Nothing
                    Left errorValue -> mfaEnrollmentResponse mfaEnrollmentPath 422 (mfaForm (Account.accountIdText accountId) Nothing [] (Just (mfaErrorMessage errorValue)) True) (Just "mfa-code")
            _ -> pure (mfaEnrollmentResponse mfaEnrollmentPath 422 (mfaForm (Account.accountIdText accountId) Nothing [] (Just (localized "Choose an enrollment action." "Elige una accion de registro.")) True) (Just "mfa-account"))

    handleLogin =
      let emailValue = actionField "email"
          passwordValue = actionField "password"
          loginForm message = LoginForm emailValue (Just message)
       in case (Email.mkEmailAddress emailValue, validPassword passwordValue, loginProof) of
            (Nothing, _, _) -> pure (loginResponse loginPath 422 (loginForm (localized "Enter a valid email address." "Introduce una direccion de correo valida.") True) (Just "login-email") [])
            (_, False, _) -> pure (loginResponse loginPath 422 (loginForm (localized "Enter your password." "Introduce tu contrasena.") True) (Just "login-password") [])
            (_, _, Nothing) -> pure (loginResponse loginPath 422 (loginForm (localized "Enter a valid authenticator or recovery code." "Introduce un codigo de autenticador o recuperacion valido.") True) (Just "login-code") [])
            (Just emailAddress, True, Just proof) -> do
              nowNanoseconds <- accountWorkflowClock workflow
              nowSeconds <- accountWorkflowTotpClock workflow
              loginResult <- completePasswordLogin (accountWorkflowCredentialStore workflow) (accountWorkflowMfaStore workflow) (accountWorkflowTotpEncryptionKey workflow) nowNanoseconds nowSeconds emailAddress (Password.mkPassword passwordValue) proof
              case loginResult of
                PasswordMfaLoginAccepted accountId -> do
                  issuedSession <- issueAccountSession (accountWorkflowSessionStore workflow) accountId nowNanoseconds
                  pure $ case issuedSession of
                    Left _ -> loginResponse loginPath 503 (loginForm (localized "Sign-in is temporarily unavailable." "El inicio de sesion no esta disponible temporalmente.") True) (Just "login-email") []
                    Right opaqueSession -> loginResponse loginPath 200 (loginForm (localized "You are signed in." "Has iniciado sesion.") False) Nothing [("Set-Cookie", TextEncoding.encodeUtf8 (renderSessionCookie defaultSessionCookiePolicy (sessionId opaqueSession)))]
                PasswordMfaLoginEmailVerificationRequired _ -> pure (loginResponse loginPath 403 (loginForm (localized "Verify your email address before signing in." "Verifica tu direccion de correo antes de iniciar sesion.") True) Nothing [])
                PasswordMfaLoginEnrollmentRequired _ -> pure (loginResponse loginPath 403 (loginForm (localized "Enroll your authenticator before signing in." "Registra tu autenticador antes de iniciar sesion.") True) Nothing [])
                PasswordMfaLoginRejected -> pure (loginResponse loginPath 422 (loginForm (localized "Sign-in was rejected." "El inicio de sesion fue rechazado.") True) (Just "login-code") [])
                PasswordMfaLoginCredentialStoreError _ -> pure (loginResponse loginPath 503 (loginForm (localized "Sign-in is temporarily unavailable." "El inicio de sesion no esta disponible temporalmente.") True) (Just "login-email") [])
                PasswordMfaLoginMfaStoreError _ -> pure (loginResponse loginPath 503 (loginForm (localized "Sign-in is temporarily unavailable." "El inicio de sesion no esta disponible temporalmente.") True) (Just "login-code") [])
                PasswordMfaLoginCorruptEnrollment -> pure (loginResponse loginPath 503 (loginForm (localized "Sign-in is temporarily unavailable." "El inicio de sesion no esta disponible temporalmente.") True) (Just "login-code") [])

    handleLogout =
      case requestSessionId actionContext of
        Nothing -> pure (logoutResponse logoutPath 200 (Just (localized "You are signed out." "Has cerrado sesion.")) False [])
        Just sessionId -> do
          invalidated <- invalidateAccountSession (accountWorkflowSessionStore workflow) sessionId
          pure $ case invalidated of
            Left _ -> logoutResponse logoutPath 503 (Just (localized "Sign-out is temporarily unavailable." "El cierre de sesion no esta disponible temporalmente.")) True []
            Right _ -> logoutResponse logoutPath 200 (Just (localized "You are signed out." "Has cerrado sesion.")) False [("Set-Cookie", TextEncoding.encodeUtf8 (renderSessionCookie (defaultSessionCookiePolicy {sessionCookieMaxAgeSeconds = 0}) sessionId))]

    actionField name =
      case [value | (fieldName, value) <- HarchWeb.clientActionFields actionRequest, fieldName == name] of
        [value] -> value
        _ -> Text.empty

    localized english spanish =
      case requestLocale actionContext of
        English -> english
        Spanish -> spanish

    mfaForm = MfaEnrollmentForm

    loginProof =
      case actionField "proof" of
        "totp" -> TotpLoginProof <$> Totp.mkTotpCode (actionField "code")
        "recovery" -> RecoveryCodeLoginProof <$> RecoveryCode.mkRecoveryCode (actionField "code")
        _ -> Nothing

    mfaErrorMessage errorValue =
      case errorValue of
        MfaEnrollmentAccountIsNotEligible -> localized "Verify your email address before enrolling an authenticator." "Verifica tu direccion de correo antes de registrar un autenticador."
        MfaEnrollmentInvalidCode -> localized "That authenticator code is invalid." "Ese codigo de autenticador no es valido."
        MfaEnrollmentNotFound -> localized "Start a new authenticator enrollment." "Inicia un nuevo registro de autenticador."
        MfaEnrollmentConfirmationRejected -> localized "That enrollment can no longer be confirmed." "Ese registro ya no se puede confirmar."
        _ -> localized "Authenticator enrollment is temporarily unavailable." "El registro del autenticador no esta disponible temporalmente."

registrationResponse :: Text -> Int -> RegistrationForm -> Maybe Text -> HarchWeb.ClientActionResponse
registrationResponse registrationPath status form focusId =
  HarchWeb.ClientActionResponse
    { HarchWeb.clientActionStatus = status,
      HarchWeb.clientActionPatches = [HarchWeb.RegionPatch "registration-region" (renderRegistrationRegion registrationPath form)],
      HarchWeb.clientActionFocusId = focusId,
      HarchWeb.clientActionHeaders = []
    }

verificationResponse :: Text -> Int -> VerificationForm -> Maybe Text -> HarchWeb.ClientActionResponse
verificationResponse verificationPath status form focusId =
  HarchWeb.ClientActionResponse
    { HarchWeb.clientActionStatus = status,
      HarchWeb.clientActionPatches = [HarchWeb.RegionPatch "verification-region" (renderVerificationRegion verificationPath form)],
      HarchWeb.clientActionFocusId = focusId,
      HarchWeb.clientActionHeaders = []
    }

mfaEnrollmentResponse :: Text -> Int -> MfaEnrollmentForm -> Maybe Text -> HarchWeb.ClientActionResponse
mfaEnrollmentResponse mfaEnrollmentPath status form focusId =
  HarchWeb.ClientActionResponse
    { HarchWeb.clientActionStatus = status,
      HarchWeb.clientActionPatches = [HarchWeb.RegionPatch "mfa-enrollment-region" (renderMfaEnrollmentRegion mfaEnrollmentPath form)],
      HarchWeb.clientActionFocusId = focusId,
      HarchWeb.clientActionHeaders = []
    }

loginResponse :: Text -> Int -> LoginForm -> Maybe Text -> Http.ResponseHeaders -> HarchWeb.ClientActionResponse
loginResponse loginPath status form focusId headers =
  HarchWeb.ClientActionResponse
    { HarchWeb.clientActionStatus = status,
      HarchWeb.clientActionPatches = [HarchWeb.RegionPatch "login-region" (renderLoginRegion loginPath form)],
      HarchWeb.clientActionFocusId = focusId,
      HarchWeb.clientActionHeaders = headers
    }

logoutResponse :: Text -> Int -> Maybe Text -> Bool -> Http.ResponseHeaders -> HarchWeb.ClientActionResponse
logoutResponse logoutPath status message isError headers =
  HarchWeb.ClientActionResponse
    { HarchWeb.clientActionStatus = status,
      HarchWeb.clientActionPatches = [HarchWeb.RegionPatch "logout-region" (renderLogoutRegion logoutPath message isError)],
      HarchWeb.clientActionFocusId = Nothing,
      HarchWeb.clientActionHeaders = headers
    }

renderMfaEnrollmentPage :: Text -> MfaEnrollmentForm -> Text
renderMfaEnrollmentPage mfaEnrollmentPath form =
  Text.concat
    [ "<section data-page=\"mfa-enrollment\"><h1 data-page-title=\"true\">Set up your authenticator</h1>",
      renderMfaEnrollmentRegion mfaEnrollmentPath form,
      "</section>"
    ]

renderMfaEnrollmentRegion :: Text -> MfaEnrollmentForm -> Text
renderMfaEnrollmentRegion mfaEnrollmentPath form =
  Text.concat
    [ "<section id=\"mfa-enrollment-region\" aria-live=\"polite\">",
      renderMessage (mfaEnrollmentFormMessage form) (mfaEnrollmentFormIsError form),
      renderEnrollmentSecret (mfaEnrollmentFormSecret form),
      renderRecoveryCodes (mfaEnrollmentFormRecoveryCodes form),
      "<form data-harch-action=\"true\" data-harch-control action=\"",
      escapeHtml mfaEnrollmentPath,
      "\" method=\"post\"><input id=\"mfa-account\" name=\"account\" type=\"hidden\" value=\"",
      escapeHtml (mfaEnrollmentFormAccountId form),
      "\"><input name=\"intent\" type=\"hidden\" value=\"start\"><button type=\"submit\">Start authenticator enrollment</button></form>",
      renderConfirmationForm mfaEnrollmentPath form,
      "</section>"
    ]

renderLoginPage :: Text -> LoginForm -> Text
renderLoginPage loginPath form =
  Text.concat
    [ "<section data-page=\"login\"><h1 data-page-title=\"true\">Sign in</h1>",
      renderLoginRegion loginPath form,
      "</section>"
    ]

renderLoginRegion :: Text -> LoginForm -> Text
renderLoginRegion loginPath form =
  Text.concat
    [ "<section id=\"login-region\" aria-live=\"polite\">",
      renderMessage (loginFormMessage form) (loginFormIsError form),
      "<form data-harch-action=\"true\" data-harch-control action=\"",
      escapeHtml loginPath,
      "\" method=\"post\"><label for=\"login-email\">Email address</label><input id=\"login-email\" name=\"email\" type=\"email\" autocomplete=\"email\" required value=\"",
      escapeHtml (loginFormEmail form),
      "\"><label for=\"login-password\">Password</label><input id=\"login-password\" name=\"password\" type=\"password\" autocomplete=\"current-password\" required><label for=\"login-proof\">Verification method</label><select id=\"login-proof\" name=\"proof\"><option value=\"totp\">Authenticator code</option><option value=\"recovery\">Recovery code</option></select><label for=\"login-code\">Verification code</label><input id=\"login-code\" name=\"code\" autocomplete=\"one-time-code\" required><button type=\"submit\">Sign in</button></form></section>"
    ]

renderLogoutPage :: Text -> Text
renderLogoutPage logoutPath =
  Text.concat
    [ "<section data-page=\"logout\"><h1 data-page-title=\"true\">Sign out</h1>",
      renderLogoutRegion logoutPath Nothing False,
      "</section>"
    ]

renderLogoutRegion :: Text -> Maybe Text -> Bool -> Text
renderLogoutRegion logoutPath message isError =
  Text.concat
    [ "<section id=\"logout-region\" aria-live=\"polite\">",
      renderMessage message isError,
      "<form data-harch-action=\"true\" data-harch-control action=\"",
      escapeHtml logoutPath,
      "\" method=\"post\"><button type=\"submit\">Sign out</button></form></section>"
    ]

renderEnrollmentSecret :: Maybe Text -> Text
renderEnrollmentSecret maybeSecret =
  case maybeSecret of
    Nothing -> Text.empty
    Just secret -> Text.concat ["<p data-totp-secret=\"true\"><code>", escapeHtml secret, "</code></p>"]

renderRecoveryCodes :: [Text] -> Text
renderRecoveryCodes recoveryCodes =
  case recoveryCodes of
    [] -> Text.empty
    _ -> Text.concat ["<section data-recovery-codes=\"true\"><h2>Recovery codes</h2><p>Save these codes. They will not be shown again.</p><ul>", Text.concat (map (\code -> Text.concat ["<li><code>", escapeHtml code, "</code></li>"]) recoveryCodes), "</ul></section>"]

renderConfirmationForm :: Text -> MfaEnrollmentForm -> Text
renderConfirmationForm mfaEnrollmentPath form =
  case mfaEnrollmentFormSecret form of
    Nothing -> Text.empty
    Just _ ->
      Text.concat
        [ "<form data-harch-action=\"true\" data-harch-control action=\"",
          escapeHtml mfaEnrollmentPath,
          "\" method=\"post\"><input name=\"account\" type=\"hidden\" value=\"",
          escapeHtml (mfaEnrollmentFormAccountId form),
          "\"><input name=\"intent\" type=\"hidden\" value=\"confirm\"><label for=\"mfa-code\">Authenticator code</label><input id=\"mfa-code\" name=\"code\" inputmode=\"numeric\" autocomplete=\"one-time-code\" required><button type=\"submit\">Confirm authenticator</button></form>"
        ]

renderRegistrationPage :: Text -> RegistrationForm -> Text
renderRegistrationPage registrationPath form =
  Text.concat
    [ "<section data-page=\"registration\"><h1 data-page-title=\"true\">Create your account</h1>",
      renderRegistrationRegion registrationPath form,
      "</section>"
    ]

renderRegistrationRegion :: Text -> RegistrationForm -> Text
renderRegistrationRegion registrationPath form =
  Text.concat
    [ "<section id=\"registration-region\" aria-live=\"polite\">",
      renderMessage (registrationFormMessage form) (registrationFormIsError form),
      "<form data-harch-action=\"true\" data-harch-control action=\"",
      escapeHtml registrationPath,
      "\" method=\"post\"><label for=\"registration-email\">Email address</label><input id=\"registration-email\" name=\"email\" type=\"email\" autocomplete=\"email\" required value=\"",
      escapeHtml (registrationFormEmail form),
      "\"><label for=\"registration-password\">Password</label><input id=\"registration-password\" name=\"password\" type=\"password\" autocomplete=\"new-password\" minlength=\"12\" required><button type=\"submit\">Create account</button></form></section>"
    ]

renderVerificationPage :: Text -> VerificationForm -> Text
renderVerificationPage verificationPath form =
  Text.concat
    [ "<section data-page=\"email-verification\"><h1 data-page-title=\"true\">Verify your email address</h1>",
      renderVerificationRegion verificationPath form,
      "</section>"
    ]

renderVerificationRegion :: Text -> VerificationForm -> Text
renderVerificationRegion verificationPath form =
  Text.concat
    [ "<section id=\"verification-region\" aria-live=\"polite\">",
      renderMessage (verificationFormMessage form) (verificationFormIsError form),
      "<form data-harch-action=\"true\" data-harch-control action=\"",
      escapeHtml verificationPath,
      "\" method=\"post\"><label for=\"verification-token\">Verification token</label><input id=\"verification-token\" name=\"token\" autocomplete=\"one-time-code\" required value=\"",
      escapeHtml (verificationFormToken form),
      "\"><button type=\"submit\">Verify email</button></form></section>"
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

storeErrorMessage :: AccountStoreError -> Text
storeErrorMessage storeError =
  case storeError of
    AccountStoreUnavailable _ -> "The account service is temporarily unavailable."
    AccountStoreCorruptData _ -> "The account service returned invalid data."

renderMessage :: Maybe Text -> Bool -> Text
renderMessage maybeMessage isError =
  case maybeMessage of
    Nothing -> Text.empty
    Just message ->
      Text.concat
        [ "<p data-account-message=\"true\"",
          if isError then " data-error-state=\"true\"" else "",
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
