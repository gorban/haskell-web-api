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

import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import HarchWeb qualified
import HarchWeb.Controls qualified as Controls
import WebApi.AccountPages.Actions.Contract (AccountActionTarget (..), accountActions)
import WebApi.AccountPages.FieldIds
import WebApi.AccountPages.Forms
import WebApi.Components.PageFrame
  ( PageFrameProps (..),
    PageKind (..),
    pageFrame,
  )
import WebApi.Localization (AppMessage (..), localizedMessage)
import WebApi.Route (AppLocale (..), AppRequestContext)

data AccountPageCopy = AccountPageCopy
  { accountRegistrationHeading :: Text,
    accountUsernameLabel :: Text,
    accountEmailLabel :: Text,
    accountDisplayNameLabel :: Text,
    accountRegistrationPasswordLabel :: Text,
    accountRegistrationPasswordHint :: Text,
    accountRegistrationErrorHeading :: Text,
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
          accountRegistrationPasswordHint = "Use at least 12 characters.",
          accountRegistrationErrorHeading = "Fix the following problems",
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
          accountRegistrationPasswordHint = "Usa al menos 12 caracteres.",
          accountRegistrationErrorHeading = "Corrige los siguientes problemas",
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

renderMfaEnrollmentPage :: AppRequestContext -> AppLocale -> MfaEnrollmentForm -> Text
renderMfaEnrollmentPage context locale form = HarchWeb.renderHtml (renderMfaEnrollmentPageHtml context locale form)

renderMfaEnrollmentPageHtml :: AppRequestContext -> AppLocale -> MfaEnrollmentForm -> HarchWeb.Html
renderMfaEnrollmentPageHtml context locale form = pageSection MfaEnrollmentPageFrame (accountMfaEnrollmentHeading (accountPageCopy locale)) (renderMfaEnrollmentRegionHtml context locale form)

renderMfaEnrollmentRegion :: AppRequestContext -> AppLocale -> MfaEnrollmentForm -> Text
renderMfaEnrollmentRegion context locale form = HarchWeb.renderHtml (renderMfaEnrollmentRegionHtml context locale form)

renderMfaEnrollmentRegionHtml :: AppRequestContext -> AppLocale -> MfaEnrollmentForm -> HarchWeb.Html
renderMfaEnrollmentRegionHtml context locale form = HarchWeb.regionHtml (mfaEnrollmentRegion context locale form)

mfaEnrollmentRegion :: AppRequestContext -> AppLocale -> MfaEnrollmentForm -> HarchWeb.Region
mfaEnrollmentRegion context locale form =
  let copy = accountPageCopy locale
   in accountRegion
        "mfa-enrollment-region"
        [ renderMessage (mfaEnrollmentFormMessage form) (mfaEnrollmentFormIsError form),
          renderEnrollmentSecret (mfaEnrollmentFormSecret form),
          renderRecoveryCodes copy (mfaEnrollmentFormRecoveryCodes form),
          actionForm
            context
            EnrollMfaTarget
            [ hiddenInput "intent" "start",
              submitButton (accountStartMfaEnrollmentLabel copy)
            ],
          renderConfirmationForm context locale form
        ]

renderLoginPage :: AppRequestContext -> AppLocale -> LoginForm -> Text
renderLoginPage context locale form = HarchWeb.renderHtml (renderLoginPageHtml context locale form)

renderLoginPageHtml :: AppRequestContext -> AppLocale -> LoginForm -> HarchWeb.Html
renderLoginPageHtml context locale form = pageSection LoginPageFrame (accountLoginHeading (accountPageCopy locale)) (renderLoginRegionHtml context locale form)

renderLoginRegion :: AppRequestContext -> AppLocale -> LoginForm -> Text
renderLoginRegion context locale form = HarchWeb.renderHtml (renderLoginRegionHtml context locale form)

renderLoginRegionHtml :: AppRequestContext -> AppLocale -> LoginForm -> HarchWeb.Html
renderLoginRegionHtml context locale form = HarchWeb.regionHtml (loginRegion context locale form)

loginRegion :: AppRequestContext -> AppLocale -> LoginForm -> HarchWeb.Region
loginRegion context locale form =
  let copy = accountPageCopy locale
   in accountRegion
        "login-region"
        [ renderMessage (loginFormMessage form) (loginFormIsError form),
          actionForm
            context
            LoginAccountTarget
            [ accessibleInput loginEmailId (accountLoginIdentifierLabel copy) Nothing Controls.FieldValid [HarchWeb.name "email", HarchWeb.inputType "text", HarchWeb.autocomplete "username", HarchWeb.required, HarchWeb.value (loginFormEmail form)],
              accessibleInput loginPasswordId (accountLoginPasswordLabel copy) Nothing Controls.FieldValid [HarchWeb.name "password", HarchWeb.inputType "password", HarchWeb.autocomplete "current-password", HarchWeb.required],
              accessibleSelect loginProofId (accountVerificationMethodLabel copy) Controls.FieldValid [HarchWeb.name "proof"] [HarchWeb.element HarchWeb.optionTag [HarchWeb.value "totp"] [HarchWeb.text (accountAuthenticatorCodeLabel copy)], HarchWeb.element HarchWeb.optionTag [HarchWeb.value "recovery"] [HarchWeb.text (accountRecoveryCodeLabel copy)]],
              accessibleInput loginCodeId (accountVerificationCodeLabel copy) Nothing Controls.FieldValid [HarchWeb.name "code", HarchWeb.autocomplete "one-time-code", HarchWeb.required],
              submitButton (accountSignInLabel copy)
            ]
        ]

renderPendingProfileRegion :: AppRequestContext -> AccountActionTarget -> PendingProfileForm -> Text
renderPendingProfileRegion context target form = HarchWeb.renderHtml (renderPendingProfileRegionHtml context target form)

renderPendingProfileRegionHtml :: AppRequestContext -> AccountActionTarget -> PendingProfileForm -> HarchWeb.Html
renderPendingProfileRegionHtml context target form = HarchWeb.regionHtml (pendingProfileRegion context target form)

pendingProfileRegion :: AppRequestContext -> AccountActionTarget -> PendingProfileForm -> HarchWeb.Region
pendingProfileRegion context target form =
  accountRegion
    "profile-region"
    [ renderMessage (pendingProfileFormMessage form) (pendingProfileFormIsError form),
      HarchWeb.element HarchWeb.paragraphTag [HarchWeb.dataAttribute "profile-email" "true"] [HarchWeb.text (pendingProfileFormEmail form)],
      HarchWeb.element
        HarchWeb.divTag
        [HarchWeb.dataAttribute "profile-resend" "true"]
        [ Controls.renderActionForm
            ( Controls.actionForm
                accountActions
                context
                target
                Controls.defaultActionFormAttributes
                [hiddenInput "intent" "resend-verification", submitButton (pendingProfileFormResendLabel form)]
            )
        ]
    ]

renderLogoutPage :: AppRequestContext -> AppLocale -> Text
renderLogoutPage context locale = HarchWeb.renderHtml (renderLogoutPageHtml context locale)

renderLogoutPageHtml :: AppRequestContext -> AppLocale -> HarchWeb.Html
renderLogoutPageHtml context locale = pageSection LogoutPageFrame (accountLogoutHeading (accountPageCopy locale)) (renderLogoutRegionWithMessage context locale Nothing)

renderLogoutRegion :: AppRequestContext -> AppLocale -> Maybe Text -> Bool -> Text
renderLogoutRegion context locale message isError = HarchWeb.renderHtml (renderLogoutRegionWithMessage context locale ((,isError) <$> message))

renderLogoutRegionWithMessage :: AppRequestContext -> AppLocale -> Maybe (Text, Bool) -> HarchWeb.Html
renderLogoutRegionWithMessage context locale message = HarchWeb.regionHtml (logoutRegion context locale message)

logoutRegion :: AppRequestContext -> AppLocale -> Maybe (Text, Bool) -> HarchWeb.Region
logoutRegion context locale message = accountRegion "logout-region" [maybe (HarchWeb.fragment []) (uncurry (renderMessage . Just)) message, actionForm context LogoutAccountTarget [submitButton (accountSignOutLabel (accountPageCopy locale))]]

renderRegistrationPage :: AppRequestContext -> AppLocale -> RegistrationForm -> Text
renderRegistrationPage context locale form = HarchWeb.renderHtml (renderRegistrationPageHtml context locale form)

renderRegistrationPageHtml :: AppRequestContext -> AppLocale -> RegistrationForm -> HarchWeb.Html
renderRegistrationPageHtml context locale form = pageSection RegistrationPageFrame (accountRegistrationHeading (accountPageCopy locale)) (renderRegistrationRegionHtml context locale form)

renderRegistrationRegion :: AppRequestContext -> AppLocale -> RegistrationForm -> Text
renderRegistrationRegion context locale form = HarchWeb.renderHtml (renderRegistrationRegionHtml context locale form)

renderRegistrationRegionHtml :: AppRequestContext -> AppLocale -> RegistrationForm -> HarchWeb.Html
renderRegistrationRegionHtml context locale form = HarchWeb.regionHtml (registrationRegion context locale form)

registrationRegion :: AppRequestContext -> AppLocale -> RegistrationForm -> HarchWeb.Region
registrationRegion context locale form =
  let copy = accountPageCopy locale
      errors = registrationErrors form
   in accountRegion
        "registration-region"
        [ renderRegistrationFeedback locale copy (registrationFormFeedback form),
          actionForm
            context
            RegisterAccountTarget
            [ accessibleInput registrationUsernameId (accountUsernameLabel copy) Nothing (registrationValidity locale registrationUsernameId errors) [HarchWeb.name "username", HarchWeb.autocomplete "username", HarchWeb.minLength "3", HarchWeb.maxLength "20", HarchWeb.required, HarchWeb.value (registrationFormUsername form)],
              accessibleInput registrationEmailId (accountEmailLabel copy) Nothing (registrationValidity locale registrationEmailId errors) [HarchWeb.name "email", HarchWeb.inputType "email", HarchWeb.autocomplete "email", HarchWeb.required, HarchWeb.value (registrationFormEmail form)],
              accessibleInput registrationDisplayNameId (accountDisplayNameLabel copy) Nothing Controls.FieldValid [HarchWeb.name "displayName", HarchWeb.autocomplete "name", HarchWeb.value (registrationFormDisplayName form)],
              accessibleInput registrationPasswordId (accountRegistrationPasswordLabel copy) (Just (Controls.DescribedContent registrationPasswordHintId (HarchWeb.text (accountRegistrationPasswordHint copy)))) (registrationValidity locale registrationPasswordId errors) [HarchWeb.name "password", HarchWeb.inputType "password", HarchWeb.autocomplete "new-password", HarchWeb.minLength "12", HarchWeb.required],
              submitButton (accountCreateAccountLabel copy)
            ]
        ]

renderVerificationPage :: AppRequestContext -> AppLocale -> VerificationForm -> Text
renderVerificationPage context locale form = HarchWeb.renderHtml (renderVerificationPageHtml context locale form)

renderVerificationPageHtml :: AppRequestContext -> AppLocale -> VerificationForm -> HarchWeb.Html
renderVerificationPageHtml context locale form = pageSection EmailVerificationPageFrame (accountVerificationHeading (accountPageCopy locale)) (renderVerificationRegionHtml context locale form)

renderVerificationRegion :: AppRequestContext -> AppLocale -> VerificationForm -> Text
renderVerificationRegion context locale form = HarchWeb.renderHtml (renderVerificationRegionHtml context locale form)

renderVerificationRegionHtml :: AppRequestContext -> AppLocale -> VerificationForm -> HarchWeb.Html
renderVerificationRegionHtml context locale form = HarchWeb.regionHtml (verificationRegion context locale form)

verificationRegion :: AppRequestContext -> AppLocale -> VerificationForm -> HarchWeb.Region
verificationRegion context locale form =
  let copy = accountPageCopy locale
   in accountRegion
        "verification-region"
        [ renderMessage (verificationFormMessage form) (verificationFormIsError form),
          actionForm context VerifyEmailTarget [accessibleInput verificationTokenId (accountVerificationTokenLabel copy) Nothing Controls.FieldValid [HarchWeb.name "token", HarchWeb.autocomplete "one-time-code", HarchWeb.required, HarchWeb.value (verificationFormToken form)], submitButton (accountVerifyEmailLabel copy)]
        ]

pageSection :: PageKind -> Text -> HarchWeb.Html -> HarchWeb.Html
pageSection pageKind heading content =
  pageFrame
    PageFrameProps
      { pageFrameKind = pageKind,
        pageFrameHeading = heading,
        pageFrameSummary = Nothing,
        pageFrameContent = [content]
      }

actionForm :: AppRequestContext -> AccountActionTarget -> [HarchWeb.Html] -> HarchWeb.Html
actionForm context target = Controls.renderActionForm . Controls.actionForm accountActions context target Controls.defaultActionFormAttributes

accountRegion :: Text -> [HarchWeb.Html] -> HarchWeb.Region
accountRegion identifier = HarchWeb.region (HarchWeb.mkRegionId (HarchWeb.literalElementId identifier)) HarchWeb.sectionTag []

replaceRegionPatch :: HarchWeb.Region -> [HarchWeb.RegionPatch]
replaceRegionPatch = pure . HarchWeb.replaceRegion

registrationErrors :: RegistrationForm -> [RegistrationValidationError]
registrationErrors form =
  case registrationFormFeedback form of
    FormRejected errors -> NonEmpty.toList errors
    FormReady -> []
    FormStatusMessage _ -> []

renderRegistrationFeedback :: AppLocale -> AccountPageCopy -> FormFeedback RegistrationValidationError -> HarchWeb.Html
renderRegistrationFeedback locale copy feedback =
  case feedback of
    FormReady -> HarchWeb.fragment []
    FormStatusMessage status -> renderFormStatus status
    FormRejected errors ->
      Controls.errorSummary
        Controls.ErrorSummary
          { Controls.errorSummaryId = registrationSummaryId,
            Controls.errorSummaryHeading = HarchWeb.text (accountRegistrationErrorHeading copy),
            Controls.errorSummaryItems = fmap (registrationErrorLink locale) errors
          }

registrationErrorLink :: AppLocale -> RegistrationValidationError -> Controls.FieldErrorLink
registrationErrorLink locale validationError =
  let (controlId, _, message) = registrationErrorDetails locale validationError
   in Controls.FieldErrorLink controlId (HarchWeb.text message)

registrationValidity :: AppLocale -> HarchWeb.ElementId -> [RegistrationValidationError] -> Controls.FieldValidity
registrationValidity locale controlId validationErrors =
  case [details | validationError <- validationErrors, let details@(targetId, _, _) = registrationErrorDetails locale validationError, targetId == controlId] of
    [] -> Controls.FieldValid
    (_, errorId, message) : _ -> Controls.FieldInvalid (Controls.DescribedContent errorId (HarchWeb.text message))

registrationErrorDetails :: AppLocale -> RegistrationValidationError -> (HarchWeb.ElementId, HarchWeb.ElementId, Text)
registrationErrorDetails locale validationError =
  case validationError of
    RegistrationUsernameInvalid -> (registrationUsernameId, registrationUsernameErrorId, localizedMessage locale UsernameInvalid)
    RegistrationEmailInvalid -> (registrationEmailId, registrationEmailErrorId, localizedMessage locale EnterValidEmailAddress)
    RegistrationPasswordTooShort -> (registrationPasswordId, registrationPasswordErrorId, localizedMessage locale PasswordTooShort)
    RegistrationUsernameUnavailable -> (registrationUsernameId, registrationUsernameErrorId, localizedMessage locale UsernameTaken)

accessibleInput :: HarchWeb.ElementId -> Text -> Maybe Controls.DescribedContent -> Controls.FieldValidity -> [HarchWeb.Attribute] -> HarchWeb.Html
accessibleInput controlId label hint validity controlAttributes =
  Controls.accessibleField
    Controls.AccessibleFieldProps
      { Controls.accessibleFieldControlId = controlId,
        Controls.accessibleFieldLabel = HarchWeb.text label,
        Controls.accessibleFieldHint = hint,
        Controls.accessibleFieldValidity = validity
      }
    (\derived -> HarchWeb.voidElement HarchWeb.inputTag (Controls.fieldControlIdAttribute derived : Controls.fieldControlRelationshipAttributes derived <> controlAttributes))

accessibleSelect :: HarchWeb.ElementId -> Text -> Controls.FieldValidity -> [HarchWeb.Attribute] -> [HarchWeb.Html] -> HarchWeb.Html
accessibleSelect controlId label validity controlAttributes children =
  Controls.accessibleField
    Controls.AccessibleFieldProps
      { Controls.accessibleFieldControlId = controlId,
        Controls.accessibleFieldLabel = HarchWeb.text label,
        Controls.accessibleFieldHint = Nothing,
        Controls.accessibleFieldValidity = validity
      }
    (\derived -> HarchWeb.element HarchWeb.selectTag (Controls.fieldControlIdAttribute derived : Controls.fieldControlRelationshipAttributes derived <> controlAttributes) children)

hiddenInput :: Text -> Text -> HarchWeb.Html
hiddenInput name value = HarchWeb.voidElement HarchWeb.inputTag [HarchWeb.name name, HarchWeb.inputType "hidden", HarchWeb.value value]

inputWithId :: Text -> [HarchWeb.Attribute] -> HarchWeb.Html
inputWithId identifier attributes = HarchWeb.voidElement HarchWeb.inputTag (HarchWeb.elementId (HarchWeb.literalElementId identifier) : attributes)

labelWithFor :: Text -> Text -> HarchWeb.Html
labelWithFor identifier label = HarchWeb.element HarchWeb.labelTag [HarchWeb.labelFor (HarchWeb.literalElementId identifier)] [HarchWeb.text label]

submitButton :: Text -> HarchWeb.Html
submitButton label = HarchWeb.element HarchWeb.buttonTag [HarchWeb.inputType "submit"] [HarchWeb.text label]

renderMessage :: Maybe Text -> Bool -> HarchWeb.Html
renderMessage maybeMessage isError = maybe (HarchWeb.fragment []) (HarchWeb.element HarchWeb.paragraphTag attributes . pure . HarchWeb.text) maybeMessage
  where
    attributes =
      HarchWeb.dataAttribute "account-message" "true"
        : if isError
          then [HarchWeb.dataAttribute "error-state" "true", HarchWeb.role "alert"]
          else [HarchWeb.role "status", HarchWeb.ariaLive "polite"]

renderFormStatus :: FormStatus -> HarchWeb.Html
renderFormStatus status =
  renderMessage
    (Just (formStatusMessage status))
    (case formStatusKind status of FormStatusSuccess -> False; FormStatusFailure -> True)

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

renderConfirmationForm :: AppRequestContext -> AppLocale -> MfaEnrollmentForm -> HarchWeb.Html
renderConfirmationForm context locale form = maybe (HarchWeb.fragment []) (const (actionForm context EnrollMfaTarget [hiddenInput "intent" "confirm", labelWithFor "mfa-code" (accountAuthenticatorCodeLabel (accountPageCopy locale)), inputWithId "mfa-code" [HarchWeb.inputMode "numeric", HarchWeb.autocomplete "one-time-code", HarchWeb.name "code", HarchWeb.required], submitButton (accountConfirmMfaEnrollmentLabel (accountPageCopy locale))])) (mfaEnrollmentFormSecret form)
