{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module WebApi.AccountPages.Rendering
  ( loginRegion,
    logoutRegion,
    mfaEnrollmentRegion,
    pendingProfileRegion,
    registrationRegion,
    renderLoginPage,
    renderLoginPageHtml,
    renderLoginRegion,
    renderLogoutPage,
    renderLogoutPageHtml,
    renderLogoutRegion,
    renderMfaEnrollmentPage,
    renderMfaEnrollmentPageHtml,
    renderMfaEnrollmentRegion,
    renderPendingProfileRegion,
    renderPendingProfileRegionHtml,
    renderRegistrationPage,
    renderRegistrationPageHtml,
    renderRegistrationRegion,
    renderVerificationPage,
    renderVerificationPageHtml,
    renderVerificationRegion,
    replaceRegionPatch,
    verificationRegion,
  )
where

import Data.Text (Text)
import HarchWeb qualified
import WebApi.AccountPages.Forms
import WebApi.Route (AppLocale (..))

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

renderMfaEnrollmentPage :: AppLocale -> Text -> MfaEnrollmentForm -> Text
renderMfaEnrollmentPage locale path form = HarchWeb.renderHtml (renderMfaEnrollmentPageHtml locale path form)

renderMfaEnrollmentPageHtml :: AppLocale -> Text -> MfaEnrollmentForm -> HarchWeb.Html
renderMfaEnrollmentPageHtml locale path form = pageSection "mfa-enrollment" (accountMfaEnrollmentHeading (accountPageCopy locale)) (renderMfaEnrollmentRegionHtml locale path form)

renderMfaEnrollmentRegion :: AppLocale -> Text -> MfaEnrollmentForm -> Text
renderMfaEnrollmentRegion locale path form = HarchWeb.renderHtml (renderMfaEnrollmentRegionHtml locale path form)

renderMfaEnrollmentRegionHtml :: AppLocale -> Text -> MfaEnrollmentForm -> HarchWeb.Html
renderMfaEnrollmentRegionHtml locale path form = HarchWeb.regionHtml (mfaEnrollmentRegion locale path form)

mfaEnrollmentRegion :: AppLocale -> Text -> MfaEnrollmentForm -> HarchWeb.Region
mfaEnrollmentRegion locale path form =
  let copy = accountPageCopy locale
   in accountRegion
        "mfa-enrollment-region"
        [ renderMessage (mfaEnrollmentFormMessage form) (mfaEnrollmentFormIsError form),
          renderEnrollmentSecret (mfaEnrollmentFormSecret form),
          renderRecoveryCodes copy (mfaEnrollmentFormRecoveryCodes form),
          actionForm
            path
            [ hiddenInput "account" (mfaEnrollmentFormAccountId form),
              hiddenInput "intent" "start",
              submitButton (accountStartMfaEnrollmentLabel copy)
            ],
          renderConfirmationForm locale path form
        ]

renderLoginPage :: AppLocale -> Text -> LoginForm -> Text
renderLoginPage locale path form = HarchWeb.renderHtml (renderLoginPageHtml locale path form)

renderLoginPageHtml :: AppLocale -> Text -> LoginForm -> HarchWeb.Html
renderLoginPageHtml locale path form = pageSection "login" (accountLoginHeading (accountPageCopy locale)) (renderLoginRegionHtml locale path form)

renderLoginRegion :: AppLocale -> Text -> LoginForm -> Text
renderLoginRegion locale path form = HarchWeb.renderHtml (renderLoginRegionHtml locale path form)

renderLoginRegionHtml :: AppLocale -> Text -> LoginForm -> HarchWeb.Html
renderLoginRegionHtml locale path form = HarchWeb.regionHtml (loginRegion locale path form)

loginRegion :: AppLocale -> Text -> LoginForm -> HarchWeb.Region
loginRegion locale path form =
  let copy = accountPageCopy locale
   in accountRegion
        "login-region"
        [ renderMessage (loginFormMessage form) (loginFormIsError form),
          actionForm
            path
            [ labelWithFor "login-email" (accountLoginIdentifierLabel copy),
              inputWithId "login-email" [HarchWeb.name "email", HarchWeb.inputType "text", HarchWeb.autocomplete "username", HarchWeb.required, HarchWeb.value (loginFormEmail form)],
              labelWithFor "login-password" (accountLoginPasswordLabel copy),
              inputWithId "login-password" [HarchWeb.name "password", HarchWeb.inputType "password", HarchWeb.autocomplete "current-password", HarchWeb.required],
              labelWithFor "login-proof" (accountVerificationMethodLabel copy),
              elementWithId HarchWeb.selectTag "login-proof" [HarchWeb.name "proof"] [HarchWeb.element HarchWeb.optionTag [HarchWeb.value "totp"] [HarchWeb.text (accountAuthenticatorCodeLabel copy)], HarchWeb.element HarchWeb.optionTag [HarchWeb.value "recovery"] [HarchWeb.text (accountRecoveryCodeLabel copy)]],
              labelWithFor "login-code" (accountVerificationCodeLabel copy),
              inputWithId "login-code" [HarchWeb.name "code", HarchWeb.autocomplete "one-time-code", HarchWeb.required],
              submitButton (accountSignInLabel copy)
            ]
        ]

renderPendingProfileRegion :: Text -> PendingProfileForm -> Text
renderPendingProfileRegion path form = HarchWeb.renderHtml (renderPendingProfileRegionHtml path form)

renderPendingProfileRegionHtml :: Text -> PendingProfileForm -> HarchWeb.Html
renderPendingProfileRegionHtml path form = HarchWeb.regionHtml (pendingProfileRegion path form)

pendingProfileRegion :: Text -> PendingProfileForm -> HarchWeb.Region
pendingProfileRegion path form =
  accountRegion
    "profile-region"
    [ renderMessage (pendingProfileFormMessage form) (pendingProfileFormIsError form),
      HarchWeb.element HarchWeb.paragraphTag [HarchWeb.dataAttribute "profile-email" "true"] [HarchWeb.text (pendingProfileFormEmail form)],
      HarchWeb.element
        HarchWeb.formTag
        [ HarchWeb.dataAttribute "profile-resend" "true",
          HarchWeb.dataAttribute "harch-action" "true",
          HarchWeb.dataFlag "harch-control",
          HarchWeb.formAction path,
          HarchWeb.method "post"
        ]
        [hiddenInput "intent" "resend-verification", submitButton (pendingProfileFormResendLabel form)]
    ]

renderLogoutPage :: AppLocale -> Text -> Text
renderLogoutPage locale path = HarchWeb.renderHtml (renderLogoutPageHtml locale path)

renderLogoutPageHtml :: AppLocale -> Text -> HarchWeb.Html
renderLogoutPageHtml locale path = pageSection "logout" (accountLogoutHeading (accountPageCopy locale)) (renderLogoutRegionWithMessage locale path Nothing)

renderLogoutRegion :: AppLocale -> Text -> Maybe Text -> Bool -> Text
renderLogoutRegion locale path message isError = HarchWeb.renderHtml (renderLogoutRegionWithMessage locale path ((,isError) <$> message))

renderLogoutRegionWithMessage :: AppLocale -> Text -> Maybe (Text, Bool) -> HarchWeb.Html
renderLogoutRegionWithMessage locale path message = HarchWeb.regionHtml (logoutRegion locale path message)

logoutRegion :: AppLocale -> Text -> Maybe (Text, Bool) -> HarchWeb.Region
logoutRegion locale path message = accountRegion "logout-region" [maybe (HarchWeb.fragment []) (uncurry (renderMessage . Just)) message, actionForm path [submitButton (accountSignOutLabel (accountPageCopy locale))]]

renderRegistrationPage :: AppLocale -> Text -> RegistrationForm -> Text
renderRegistrationPage locale path form = HarchWeb.renderHtml (renderRegistrationPageHtml locale path form)

renderRegistrationPageHtml :: AppLocale -> Text -> RegistrationForm -> HarchWeb.Html
renderRegistrationPageHtml locale path form = pageSection "registration" (accountRegistrationHeading (accountPageCopy locale)) (renderRegistrationRegionHtml locale path form)

renderRegistrationRegion :: AppLocale -> Text -> RegistrationForm -> Text
renderRegistrationRegion locale path form = HarchWeb.renderHtml (renderRegistrationRegionHtml locale path form)

renderRegistrationRegionHtml :: AppLocale -> Text -> RegistrationForm -> HarchWeb.Html
renderRegistrationRegionHtml locale path form = HarchWeb.regionHtml (registrationRegion locale path form)

registrationRegion :: AppLocale -> Text -> RegistrationForm -> HarchWeb.Region
registrationRegion locale path form =
  let copy = accountPageCopy locale
   in accountRegion
        "registration-region"
        [ renderMessage (registrationFormMessage form) (registrationFormIsError form),
          actionForm
            path
            [ labelWithFor "registration-username" (accountUsernameLabel copy),
              inputWithId "registration-username" [HarchWeb.name "username", HarchWeb.autocomplete "username", HarchWeb.minLength "3", HarchWeb.maxLength "20", HarchWeb.required, HarchWeb.value (registrationFormUsername form)],
              labelWithFor "registration-email" (accountEmailLabel copy),
              inputWithId "registration-email" [HarchWeb.name "email", HarchWeb.inputType "email", HarchWeb.autocomplete "email", HarchWeb.required, HarchWeb.value (registrationFormEmail form)],
              labelWithFor "registration-display-name" (accountDisplayNameLabel copy),
              inputWithId "registration-display-name" [HarchWeb.name "displayName", HarchWeb.autocomplete "name", HarchWeb.value (registrationFormDisplayName form)],
              labelWithFor "registration-password" (accountRegistrationPasswordLabel copy),
              inputWithId "registration-password" [HarchWeb.name "password", HarchWeb.inputType "password", HarchWeb.autocomplete "new-password", HarchWeb.minLength "12", HarchWeb.required],
              submitButton (accountCreateAccountLabel copy)
            ]
        ]

renderVerificationPage :: AppLocale -> Text -> VerificationForm -> Text
renderVerificationPage locale path form = HarchWeb.renderHtml (renderVerificationPageHtml locale path form)

renderVerificationPageHtml :: AppLocale -> Text -> VerificationForm -> HarchWeb.Html
renderVerificationPageHtml locale path form = pageSection "email-verification" (accountVerificationHeading (accountPageCopy locale)) (renderVerificationRegionHtml locale path form)

renderVerificationRegion :: AppLocale -> Text -> VerificationForm -> Text
renderVerificationRegion locale path form = HarchWeb.renderHtml (renderVerificationRegionHtml locale path form)

renderVerificationRegionHtml :: AppLocale -> Text -> VerificationForm -> HarchWeb.Html
renderVerificationRegionHtml locale path form = HarchWeb.regionHtml (verificationRegion locale path form)

verificationRegion :: AppLocale -> Text -> VerificationForm -> HarchWeb.Region
verificationRegion locale path form =
  let copy = accountPageCopy locale
   in accountRegion
        "verification-region"
        [ renderMessage (verificationFormMessage form) (verificationFormIsError form),
          actionForm path [labelWithFor "verification-token" (accountVerificationTokenLabel copy), inputWithId "verification-token" [HarchWeb.name "token", HarchWeb.autocomplete "one-time-code", HarchWeb.required, HarchWeb.value (verificationFormToken form)], submitButton (accountVerifyEmailLabel copy)]
        ]

pageSection :: Text -> Text -> HarchWeb.Html -> HarchWeb.Html
pageSection page heading content = HarchWeb.element HarchWeb.sectionTag [HarchWeb.dataAttribute "page" page] [HarchWeb.element HarchWeb.headingOneTag [HarchWeb.dataAttribute "page-title" "true"] [HarchWeb.text heading], content]

actionForm :: Text -> [HarchWeb.Html] -> HarchWeb.Html
actionForm path = HarchWeb.element HarchWeb.formTag [HarchWeb.dataAttribute "harch-action" "true", HarchWeb.dataFlag "harch-control", HarchWeb.formAction path, HarchWeb.method "post"]

accountRegion :: Text -> [HarchWeb.Html] -> HarchWeb.Region
accountRegion identifier = HarchWeb.region (HarchWeb.mkRegionId (HarchWeb.literalElementId identifier)) HarchWeb.sectionTag [HarchWeb.ariaLive "polite"]

replaceRegionPatch :: HarchWeb.Region -> [HarchWeb.RegionPatch]
replaceRegionPatch = pure . HarchWeb.replaceRegion

hiddenInput :: Text -> Text -> HarchWeb.Html
hiddenInput name value = HarchWeb.voidElement HarchWeb.inputTag [HarchWeb.name name, HarchWeb.inputType "hidden", HarchWeb.value value]

inputWithId :: Text -> [HarchWeb.Attribute] -> HarchWeb.Html
inputWithId identifier attributes = HarchWeb.voidElement HarchWeb.inputTag (HarchWeb.elementId (HarchWeb.literalElementId identifier) : attributes)

elementWithId :: HarchWeb.Tag -> Text -> [HarchWeb.Attribute] -> [HarchWeb.Html] -> HarchWeb.Html
elementWithId tag identifier attributes = HarchWeb.element tag (HarchWeb.elementId (HarchWeb.literalElementId identifier) : attributes)

labelWithFor :: Text -> Text -> HarchWeb.Html
labelWithFor identifier label = HarchWeb.element HarchWeb.labelTag [HarchWeb.labelFor (HarchWeb.literalElementId identifier)] [HarchWeb.text label]

submitButton :: Text -> HarchWeb.Html
submitButton label = HarchWeb.element HarchWeb.buttonTag [HarchWeb.inputType "submit"] [HarchWeb.text label]

renderMessage :: Maybe Text -> Bool -> HarchWeb.Html
renderMessage maybeMessage isError = maybe (HarchWeb.fragment []) (HarchWeb.element HarchWeb.paragraphTag attributes . pure . HarchWeb.text) maybeMessage
  where
    attributes = HarchWeb.dataAttribute "account-message" "true" : [HarchWeb.dataAttribute "error-state" "true" | isError]

renderEnrollmentSecret :: Maybe Text -> HarchWeb.Html
renderEnrollmentSecret = maybe (HarchWeb.fragment []) (HarchWeb.element HarchWeb.paragraphTag [HarchWeb.dataAttribute "totp-secret" "true"] . pure . HarchWeb.element HarchWeb.codeTag [] . pure . HarchWeb.text)

renderRecoveryCodes :: AccountPageCopy -> [Text] -> HarchWeb.Html
renderRecoveryCodes copy = \case
  [] -> HarchWeb.fragment []
  codes ->
    HarchWeb.element
      HarchWeb.sectionTag
      [HarchWeb.dataAttribute "recovery-codes" "true"]
      [ HarchWeb.element HarchWeb.headingTwoTag [] [HarchWeb.text (accountRecoveryCodesHeading copy)],
        HarchWeb.element HarchWeb.paragraphTag [] [HarchWeb.text (accountRecoveryCodesInstruction copy)],
        HarchWeb.element
          HarchWeb.listTag
          []
          [ HarchWeb.element HarchWeb.listItemTag [] [HarchWeb.element HarchWeb.codeTag [] [HarchWeb.text code]]
          | code <- codes
          ]
      ]

renderConfirmationForm :: AppLocale -> Text -> MfaEnrollmentForm -> HarchWeb.Html
renderConfirmationForm locale path form = maybe (HarchWeb.fragment []) (const (actionForm path [hiddenInput "account" (mfaEnrollmentFormAccountId form), hiddenInput "intent" "confirm", labelWithFor "mfa-code" (accountAuthenticatorCodeLabel (accountPageCopy locale)), inputWithId "mfa-code" [HarchWeb.name "code", HarchWeb.inputMode "numeric", HarchWeb.autocomplete "one-time-code", HarchWeb.required], submitButton (accountConfirmMfaEnrollmentLabel (accountPageCopy locale))])) (mfaEnrollmentFormSecret form)
