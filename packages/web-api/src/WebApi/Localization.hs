{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The reference application's closed catalog. Adding a message is an enum
-- constructor plus its templates here; no action handler supplies positional
-- per-language text. HarchWeb's empty default catalog is intentionally not a
-- fallback because every application key must be covered explicitly.
module WebApi.Localization
  ( AppMessage (..),
    localizedMessage,
    localizedMessageForLocale,
  )
where

import Data.Text (Text)
import HarchWeb qualified
import HarchWeb.Localization.Quasi (message)
import WebApi.Route (AppLocale (..))

data AppMessage
  = AddAuthenticatorSecret
  | AuthenticatorEnrolled
  | AuthenticatorEnrollmentUnavailable
  | CheckVerificationInbox
  | ChooseProfileAction
  | ChooseEnrollmentAction
  | EnrollAuthenticatorBeforeSignIn
  | EnterAuthenticatorCode
  | EnterAuthenticatorOrRecoveryCode
  | EnterValidEmailAddress
  | EnterValidEmailOrUsername
  | EnterPassword
  | RegistrationVerificationInbox
  | RegistrationUnavailable
  | ResendVerificationEmail
  | SignInBeforeResend
  | SignInUnavailable
  | SignInRejected
  | SignOutUnavailable
  | StartAuthenticatorEnrollment
  | AuthenticatorCodeInvalid
  | EnrollmentConfirmationUnavailable
  | UsernameTaken
  | VerificationLinkExpired
  | VerificationLinkUsed
  | VerificationLinkInvalid
  | EnrollmentLinkInvalid
  | SignInThrottled
  | PasswordTooShort
  | UsernameInvalid
  | VerificationUnavailable
  | VerifyEmailBeforeEnrollment
  | VerifyEmailBeforeSignIn
  | VerificationDeliveryFailed
  | SignedIn
  | SignedOut
  | EmailAlreadyVerified
  | EmailVerifiedEnrollAuthenticator
  | ProfileUnavailable
  | ProfileStateChanged
  | SiteUnderConstruction
  | FollowThisSpace
  | NotFound
  | NotFoundSummary
  | ReturnHome
  | Profile
  | SignedOutProfileSummary
  | SignIn
  | CreateAccount
  | VerifyEmailBeforeContinuing
  | SignOut
  | AuthenticatedProfileSummary
  | UnavailableProfileSummary
  | Second
  | SecondPageUnavailable
  | SecondPageLoadFailed
  deriving (Bounded, Enum, Eq, Show)

localizedMessage :: AppLocale -> AppMessage -> Text
localizedMessage appLocale = localizedMessageForLocale (localeFor appLocale)

-- | Render a catalog key for an arbitrary locale. Application routes use the
-- closed 'AppLocale' through 'localizedMessage'; integrations that carry a
-- raw locale receive a safe, non-empty fallback instead of a process crash
-- when this two-locale catalog has no matching entry.
localizedMessageForLocale :: HarchWeb.Locale -> AppMessage -> Text
localizedMessageForLocale requestedLocale messageKey =
  case HarchWeb.renderLocalizedMessage appLocalizer messageKey requestedLocale (HarchWeb.messageArguments []) of
    Right rendered -> rendered
    Left _ -> "Message unavailable."

appLocalizer :: HarchWeb.Localizer AppMessage
appLocalizer =
  HarchWeb.localizer $ \messageKey requestedLocale ->
    case HarchWeb.localeText requestedLocale of
      "en" -> Just (messageTemplateFor messageKey English)
      "es" -> Just (messageTemplateFor messageKey Spanish)
      _ -> Nothing

localeFor :: AppLocale -> HarchWeb.Locale
localeFor appLocale =
  HarchWeb.locale
    ( case appLocale of
        English -> "en"
        Spanish -> "es"
    )

messageTemplateFor :: AppMessage -> AppLocale -> HarchWeb.MessageTemplate
messageTemplateFor messageKey appLocale =
  case (messageKey, appLocale) of
    (AddAuthenticatorSecret, English) -> [message|Add this secret to your authenticator, then enter its six-digit code.|]
    (AddAuthenticatorSecret, Spanish) -> [message|Agrega este secreto a tu autenticador y luego introduce su codigo de seis digitos.|]
    (AuthenticatorEnrolled, English) -> [message|Authenticator enrolled. Save these recovery codes now.|]
    (AuthenticatorEnrolled, Spanish) -> [message|Autenticador registrado. Guarda estos codigos de recuperacion ahora.|]
    (AuthenticatorEnrollmentUnavailable, English) -> [message|Authenticator enrollment is temporarily unavailable.|]
    (AuthenticatorEnrollmentUnavailable, Spanish) -> [message|El registro del autenticador no esta disponible temporalmente.|]
    (CheckVerificationInbox, English) -> [message|Check your inbox for a verification link.|]
    (CheckVerificationInbox, Spanish) -> [message|Revisa tu bandeja de entrada para obtener un enlace de verificacion.|]
    (ChooseProfileAction, English) -> [message|Choose a profile action.|]
    (ChooseProfileAction, Spanish) -> [message|Elige una accion de perfil.|]
    (ChooseEnrollmentAction, English) -> [message|Choose an enrollment action.|]
    (ChooseEnrollmentAction, Spanish) -> [message|Elige una accion de registro.|]
    (EnrollAuthenticatorBeforeSignIn, English) -> [message|Enroll your authenticator before signing in.|]
    (EnrollAuthenticatorBeforeSignIn, Spanish) -> [message|Registra tu autenticador antes de iniciar sesion.|]
    (EnterAuthenticatorCode, English) -> [message|Enter a six-digit authenticator code.|]
    (EnterAuthenticatorCode, Spanish) -> [message|Introduce un codigo de autenticador de seis digitos.|]
    (EnterAuthenticatorOrRecoveryCode, English) -> [message|Enter a valid authenticator or recovery code.|]
    (EnterAuthenticatorOrRecoveryCode, Spanish) -> [message|Introduce un codigo de autenticador o recuperacion valido.|]
    (EnterValidEmailAddress, English) -> [message|Enter a valid email address.|]
    (EnterValidEmailAddress, Spanish) -> [message|Introduce una direccion de correo valida.|]
    (EnterValidEmailOrUsername, English) -> [message|Enter a valid email address or username.|]
    (EnterValidEmailOrUsername, Spanish) -> [message|Introduce una direccion de correo o un nombre de usuario valido.|]
    (EnterPassword, English) -> [message|Enter your password.|]
    (EnterPassword, Spanish) -> [message|Introduce tu contrasena.|]
    (RegistrationVerificationInbox, English) -> [message|If that address can register, check its inbox for a verification link.|]
    (RegistrationVerificationInbox, Spanish) -> [message|Si esa direccion puede registrarse, revisa su bandeja de entrada para obtener un enlace de verificacion.|]
    (RegistrationUnavailable, English) -> [message|Registration is temporarily unavailable.|]
    (RegistrationUnavailable, Spanish) -> [message|El registro no esta disponible temporalmente.|]
    (ResendVerificationEmail, English) -> [message|Resend verification email|]
    (ResendVerificationEmail, Spanish) -> [message|Reenviar correo de verificacion|]
    (SignInBeforeResend, English) -> [message|Sign in before requesting another verification email.|]
    (SignInBeforeResend, Spanish) -> [message|Inicia sesion antes de solicitar otro correo de verificacion.|]
    (SignInUnavailable, English) -> [message|Sign-in is temporarily unavailable.|]
    (SignInUnavailable, Spanish) -> [message|El inicio de sesion no esta disponible temporalmente.|]
    (SignInRejected, English) -> [message|Sign-in was rejected.|]
    (SignInRejected, Spanish) -> [message|El inicio de sesion fue rechazado.|]
    (SignOutUnavailable, English) -> [message|Sign-out is temporarily unavailable.|]
    (SignOutUnavailable, Spanish) -> [message|El cierre de sesion no esta disponible temporalmente.|]
    (StartAuthenticatorEnrollment, English) -> [message|Start a new authenticator enrollment.|]
    (StartAuthenticatorEnrollment, Spanish) -> [message|Inicia un nuevo registro de autenticador.|]
    (AuthenticatorCodeInvalid, English) -> [message|That authenticator code is invalid.|]
    (AuthenticatorCodeInvalid, Spanish) -> [message|Ese codigo de autenticador no es valido.|]
    (EnrollmentConfirmationUnavailable, English) -> [message|That enrollment can no longer be confirmed.|]
    (EnrollmentConfirmationUnavailable, Spanish) -> [message|Ese registro ya no se puede confirmar.|]
    (UsernameTaken, English) -> [message|That username is already taken. Please choose another.|]
    (UsernameTaken, Spanish) -> [message|Ese nombre de usuario ya esta en uso. Elige otro.|]
    (VerificationLinkExpired, English) -> [message|That verification link has expired.|]
    (VerificationLinkExpired, Spanish) -> [message|Ese enlace de verificacion ha caducado.|]
    (VerificationLinkUsed, English) -> [message|That verification link is invalid or has already been used.|]
    (VerificationLinkUsed, Spanish) -> [message|El enlace de verificacion no es valido o ya se ha utilizado.|]
    (VerificationLinkInvalid, English) -> [message|The verification link is invalid.|]
    (VerificationLinkInvalid, Spanish) -> [message|El enlace de verificacion no es valido.|]
    (EnrollmentLinkInvalid, English) -> [message|This enrollment link is invalid or has expired. Sign in again to continue.|]
    (EnrollmentLinkInvalid, Spanish) -> [message|Este enlace de registro no es valido o ha caducado. Inicia sesion de nuevo para continuar.|]
    (SignInThrottled, English) -> [message|Too many sign-in attempts. Try again later.|]
    (SignInThrottled, Spanish) -> [message|Demasiados intentos de inicio de sesion. Intentalo de nuevo mas tarde.|]
    (PasswordTooShort, English) -> [message|Use a password with at least 12 characters.|]
    (PasswordTooShort, Spanish) -> [message|Usa una contrasena de al menos 12 caracteres.|]
    (UsernameInvalid, English) -> [message|Use a username with 3 to 20 letters, numbers, underscores, or hyphens.|]
    (UsernameInvalid, Spanish) -> [message|Usa un nombre de usuario de 3 a 20 letras, numeros, guiones bajos o guiones.|]
    (VerificationUnavailable, English) -> [message|Verification is temporarily unavailable.|]
    (VerificationUnavailable, Spanish) -> [message|La verificacion no esta disponible temporalmente.|]
    (VerifyEmailBeforeEnrollment, English) -> [message|Verify your email address before enrolling an authenticator.|]
    (VerifyEmailBeforeEnrollment, Spanish) -> [message|Verifica tu direccion de correo antes de registrar un autenticador.|]
    (VerifyEmailBeforeSignIn, English) -> [message|Verify your email address before signing in.|]
    (VerifyEmailBeforeSignIn, Spanish) -> [message|Verifica tu direccion de correo antes de iniciar sesion.|]
    (VerificationDeliveryFailed, English) -> [message|We could not send the verification email. Try again shortly.|]
    (VerificationDeliveryFailed, Spanish) -> [message|No pudimos enviar el correo de verificacion. Intentalo de nuevo en breve.|]
    (SignedIn, English) -> [message|You are signed in.|]
    (SignedIn, Spanish) -> [message|Has iniciado sesion.|]
    (SignedOut, English) -> [message|You are signed out.|]
    (SignedOut, Spanish) -> [message|Has cerrado sesion.|]
    (EmailAlreadyVerified, English) -> [message|Your email address is already verified.|]
    (EmailAlreadyVerified, Spanish) -> [message|Tu direccion de correo ya esta verificada.|]
    (EmailVerifiedEnrollAuthenticator, English) -> [message|Your email address is verified. Enroll your authenticator next.|]
    (EmailVerifiedEnrollAuthenticator, Spanish) -> [message|Tu direccion de correo esta verificada. A continuacion, registra tu autenticador.|]
    (ProfileUnavailable, English) -> [message|Your profile is temporarily unavailable.|]
    (ProfileUnavailable, Spanish) -> [message|Tu perfil no esta disponible temporalmente.|]
    (ProfileStateChanged, English) -> [message|Your profile state changed. Reload the page before trying again.|]
    (ProfileStateChanged, Spanish) -> [message|El estado de tu perfil ha cambiado. Recarga la pagina antes de intentarlo de nuevo.|]
    (SiteUnderConstruction, English) -> [message|Site under construction|]
    (SiteUnderConstruction, Spanish) -> [message|Sitio en construcción|]
    (FollowThisSpace, English) -> [message|Follow this space.|]
    (FollowThisSpace, Spanish) -> [message|Sigan este espacio.|]
    (NotFound, English) -> [message|Not Found|]
    (NotFound, Spanish) -> [message|No encontrado|]
    (NotFoundSummary, English) -> [message|The requested page could not be found.|]
    (NotFoundSummary, Spanish) -> [message|No se pudo encontrar la pagina solicitada.|]
    (ReturnHome, English) -> [message|Return home|]
    (ReturnHome, Spanish) -> [message|Volver al inicio|]
    (Profile, English) -> [message|Profile|]
    (Profile, Spanish) -> [message|Perfil|]
    (SignedOutProfileSummary, English) -> [message|Sign in to view and manage your profile.|]
    (SignedOutProfileSummary, Spanish) -> [message|Inicia sesión para ver y administrar tu perfil.|]
    (SignIn, English) -> [message|Sign in|]
    (SignIn, Spanish) -> [message|Iniciar sesión|]
    (CreateAccount, English) -> [message|Create account|]
    (CreateAccount, Spanish) -> [message|Crear cuenta|]
    (VerifyEmailBeforeContinuing, English) -> [message|Verify your email address before continuing.|]
    (VerifyEmailBeforeContinuing, Spanish) -> [message|Verifica tu dirección de correo antes de continuar.|]
    (SignOut, English) -> [message|Sign out|]
    (SignOut, Spanish) -> [message|Cerrar sesión|]
    (AuthenticatedProfileSummary, English) -> [message|You are signed in.|]
    (AuthenticatedProfileSummary, Spanish) -> [message|Has iniciado sesión.|]
    (UnavailableProfileSummary, English) -> [message|Your profile is temporarily unavailable.|]
    (UnavailableProfileSummary, Spanish) -> [message|Tu perfil no está disponible temporalmente.|]
    (Second, English) -> [message|Second|]
    (Second, Spanish) -> [message|Segunda|]
    (SecondPageUnavailable, English) -> [message|Second page content is temporarily unavailable.|]
    (SecondPageUnavailable, Spanish) -> [message|El contenido de la segunda pagina no esta disponible temporalmente.|]
    (SecondPageLoadFailed, English) -> [message|Could not load second page data.|]
    (SecondPageLoadFailed, Spanish) -> [message|No se pudieron cargar los datos de la segunda pagina.|]
