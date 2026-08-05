{-# LANGUAGE OverloadedStrings #-}

module WebApi.AccountPages.Actions.Common
  ( accountWorkflow,
    pendingProfileForm,
    resendLabel,
    profileLoadErrorType,
    profileLoadErrorDetail,
    localized,
    actionLocale,
    mfaErrorMessage,
    throwClientActionFailure,
    buildFailureDiagnostics,
    attachClientActionFailure,
    credentialStoreErrorMessage,
    accountStoreErrorDetail,
    mfaStoreErrorMessage,
    sessionStoreErrorMessage,
    registrationResponse,
    verificationResponse,
    mfaEnrollmentResponse,
    loginResponse,
    logoutResponse,
    profileResponse,
    emailVerificationLifetimeNanoseconds,
    emailLocale,
    validPassword,
    nonEmptyText,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import HarchWeb qualified
import HarchWeb.Email qualified as Email
import HarchWeb.Observability qualified as Observability
import Network.HTTP.Types qualified as Http
import WebApi.Account (AccountProfile (..), AccountStoreError (..))
import WebApi.AccountPages.Actions.Contract (AccountAction)
import WebApi.AccountPages.Forms
import WebApi.AccountPages.Rendering
import WebApi.AppEffect
  ( AccountWorkflow (..),
    AppFailure (..),
    AppM,
    AppServices (..),
    FailureCode,
    FailureDiagnostics (..),
    askAppServices,
    renderFailureCode,
    throwAppFailure,
  )
import WebApi.Login (AccountCredentialStoreError (..))
import WebApi.Mfa (MfaStoreError (..))
import WebApi.MfaEnrollment (MfaEnrollmentError (..))
import WebApi.Profile (ProfileLoadError (..))
import WebApi.Route (AppLocale (..), AppRequestContext (..))
import WebApi.Session (AccountSessionStoreError (..))

type AccountActionRequest = HarchWeb.ClientActionRequest AccountAction AppRequestContext

accountWorkflow :: AppM publicFailure AccountWorkflow
accountWorkflow = appAccountWorkflow <$> askAppServices

pendingProfileForm :: AccountActionRequest -> AccountProfile -> Maybe Text -> Bool -> PendingProfileForm
pendingProfileForm actionRequest profile message isError =
  PendingProfileForm
    { pendingProfileFormEmail = Email.emailAddressText (accountProfileEmail profile),
      pendingProfileFormMessage = message,
      pendingProfileFormIsError = isError,
      pendingProfileFormResendLabel = resendLabel actionRequest
    }

resendLabel :: AccountActionRequest -> Text
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

localized :: AccountActionRequest -> Text -> Text -> Text
localized actionRequest english spanish =
  case actionLocale actionRequest of
    English -> english
    Spanish -> spanish

actionLocale :: AccountActionRequest -> AppLocale
actionLocale = requestLocale . HarchWeb.clientActionContext

mfaErrorMessage :: AccountActionRequest -> MfaEnrollmentError -> Text
mfaErrorMessage actionRequest errorValue =
  case errorValue of
    MfaEnrollmentAccountIsNotEligible -> localized actionRequest "Verify your email address before enrolling an authenticator." "Verifica tu direccion de correo antes de registrar un autenticador."
    MfaEnrollmentInvalidCode -> localized actionRequest "That authenticator code is invalid." "Ese codigo de autenticador no es valido."
    MfaEnrollmentNotFound -> localized actionRequest "Start a new authenticator enrollment." "Inicia un nuevo registro de autenticador."
    MfaEnrollmentConfirmationRejected -> localized actionRequest "That enrollment can no longer be confirmed." "Ese registro ya no se puede confirmar."
    _ -> localized actionRequest "Authenticator enrollment is temporarily unavailable." "El registro del autenticador no esta disponible temporalmente."

throwClientActionFailure :: HarchWeb.ClientActionResponse -> FailureCode -> Text -> Text -> AppM HarchWeb.ClientActionResponse value
throwClientActionFailure publicResponse code typeName detail =
  throwAppFailure
    AppFailure
      { appFailurePublic = publicResponse,
        appFailureDiagnostics = buildFailureDiagnostics code typeName detail
      }

buildFailureDiagnostics :: FailureCode -> Text -> Text -> FailureDiagnostics
buildFailureDiagnostics code typeName detail =
  FailureDiagnostics
    { failureCode = code,
      failureType = typeName,
      failureLogEntries = ["ERROR [" <> renderFailureCode code <> "] " <> detail]
    }

attachClientActionFailure :: AppFailure HarchWeb.ClientActionResponse -> HarchWeb.ClientActionResponse
attachClientActionFailure failure =
  let publicResponse = appFailurePublic failure
      diagnostics = appFailureDiagnostics failure
   in publicResponse
        { HarchWeb.clientActionObservabilityAttributes =
            HarchWeb.clientActionObservabilityAttributes publicResponse
              <> [ Observability.ObservabilityAttribute "error.type" (Observability.TextAttribute (failureType diagnostics)),
                   Observability.ObservabilityAttribute "app.failure.code" (Observability.TextAttribute (renderFailureCode (failureCode diagnostics)))
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

registrationResponse :: AppLocale -> AppRequestContext -> Int -> RegistrationForm -> Maybe Text -> HarchWeb.ClientActionResponse
registrationResponse locale requestContext status form focusId =
  HarchWeb.ClientActionResponse
    { HarchWeb.clientActionStatus = status,
      HarchWeb.clientActionPatches = replaceRegionPatch (registrationRegion requestContext locale form),
      HarchWeb.clientActionFocusId = focusId,
      HarchWeb.clientActionHeaders = [],
      HarchWeb.clientActionObservabilityAttributes = [],
      HarchWeb.clientActionLogEntries = []
    }

verificationResponse :: AppLocale -> AppRequestContext -> Int -> VerificationForm -> Maybe Text -> HarchWeb.ClientActionResponse
verificationResponse locale requestContext status form focusId =
  HarchWeb.ClientActionResponse
    { HarchWeb.clientActionStatus = status,
      HarchWeb.clientActionPatches = replaceRegionPatch (verificationRegion requestContext locale form),
      HarchWeb.clientActionFocusId = focusId,
      HarchWeb.clientActionHeaders = [],
      HarchWeb.clientActionObservabilityAttributes = [],
      HarchWeb.clientActionLogEntries = []
    }

mfaEnrollmentResponse :: AppLocale -> AppRequestContext -> Int -> MfaEnrollmentForm -> Maybe Text -> HarchWeb.ClientActionResponse
mfaEnrollmentResponse locale requestContext status form focusId =
  HarchWeb.ClientActionResponse
    { HarchWeb.clientActionStatus = status,
      HarchWeb.clientActionPatches = replaceRegionPatch (mfaEnrollmentRegion requestContext locale form),
      HarchWeb.clientActionFocusId = focusId,
      HarchWeb.clientActionHeaders = [],
      HarchWeb.clientActionObservabilityAttributes = [],
      HarchWeb.clientActionLogEntries = []
    }

loginResponse :: AppLocale -> AppRequestContext -> Int -> LoginForm -> Maybe Text -> Http.ResponseHeaders -> HarchWeb.ClientActionResponse
loginResponse locale requestContext status form focusId headers =
  HarchWeb.ClientActionResponse
    { HarchWeb.clientActionStatus = status,
      HarchWeb.clientActionPatches = replaceRegionPatch (loginRegion requestContext locale form),
      HarchWeb.clientActionFocusId = focusId,
      HarchWeb.clientActionHeaders = headers,
      HarchWeb.clientActionObservabilityAttributes = [],
      HarchWeb.clientActionLogEntries = []
    }

logoutResponse :: AppLocale -> AppRequestContext -> Int -> Maybe Text -> Bool -> Http.ResponseHeaders -> HarchWeb.ClientActionResponse
logoutResponse locale requestContext status message isError headers =
  HarchWeb.ClientActionResponse
    { HarchWeb.clientActionStatus = status,
      HarchWeb.clientActionPatches = replaceRegionPatch (logoutRegion requestContext locale ((,isError) <$> message)),
      HarchWeb.clientActionFocusId = Nothing,
      HarchWeb.clientActionHeaders = headers,
      HarchWeb.clientActionObservabilityAttributes = [],
      HarchWeb.clientActionLogEntries = []
    }

profileResponse :: AccountActionRequest -> Int -> PendingProfileForm -> HarchWeb.ClientActionResponse
profileResponse actionRequest status form =
  HarchWeb.ClientActionResponse
    { HarchWeb.clientActionStatus = status,
      HarchWeb.clientActionPatches = replaceRegionPatch (pendingProfileRegion (HarchWeb.clientActionContext actionRequest) form),
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
