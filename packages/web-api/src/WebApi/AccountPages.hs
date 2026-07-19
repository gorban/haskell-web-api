{-# LANGUAGE OverloadedStrings #-}

module WebApi.AccountPages
  ( AccountWorkflow (..),
    RegistrationForm (..),
    VerificationForm (..),
    emptyRegistrationForm,
    handleAccountAction,
    renderRegistrationPage,
    renderRegistrationRegion,
    renderVerificationPage,
    renderVerificationRegion,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import HarchWeb qualified
import HarchWeb.Account qualified as Account
import HarchWeb.Email qualified as Email
import HarchWeb.Password qualified as Password
import WebApi.Account
  ( AccountStore,
    AccountStoreError (..),
    RegistrationError (..),
    RegistrationResult (..),
    confirmEmailVerificationAt,
    registerAccountAt,
  )
import WebApi.Route
  ( AppLocale (..),
    AppRequestContext (..),
    AppRoute (..),
    renderRoutePath,
  )

data AccountWorkflow = AccountWorkflow
  { accountWorkflowStore :: AccountStore,
    accountWorkflowEmailDelivery :: Email.EmailDelivery,
    accountWorkflowClock :: IO Word64,
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
        _ -> pure Nothing
    _ -> pure Nothing
  where
    actionContext = HarchWeb.clientActionContext actionRequest
    registrationPath = renderRoutePath (routeRequest RegistrationRoute)
    verificationPath = renderRoutePath (routeRequest EmailVerificationRoute)
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

    actionField name =
      case [value | (fieldName, value) <- HarchWeb.clientActionFields actionRequest, fieldName == name] of
        [value] -> value
        _ -> Text.empty

    localized english spanish =
      case requestLocale actionContext of
        English -> english
        Spanish -> spanish

registrationResponse :: Text -> Int -> RegistrationForm -> Maybe Text -> HarchWeb.ClientActionResponse
registrationResponse registrationPath status form focusId =
  HarchWeb.ClientActionResponse
    { HarchWeb.clientActionStatus = status,
      HarchWeb.clientActionPatches = [HarchWeb.RegionPatch "registration-region" (renderRegistrationRegion registrationPath form)],
      HarchWeb.clientActionFocusId = focusId
    }

verificationResponse :: Text -> Int -> VerificationForm -> Maybe Text -> HarchWeb.ClientActionResponse
verificationResponse verificationPath status form focusId =
  HarchWeb.ClientActionResponse
    { HarchWeb.clientActionStatus = status,
      HarchWeb.clientActionPatches = [HarchWeb.RegionPatch "verification-region" (renderVerificationRegion verificationPath form)],
      HarchWeb.clientActionFocusId = focusId
    }

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
