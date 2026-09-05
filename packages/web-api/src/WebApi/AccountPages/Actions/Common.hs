{-# LANGUAGE OverloadedStrings #-}

-- | Shared account action workflow support.
--
-- Decision (FQ6, 2026-08-29): region response metadata is captured in one
-- internal context before rendering. Existing action functions still expose
-- their route values where cookie parsing needs them; the response-building
-- path does not independently assemble status, focus, headers, locale, and
-- request context.
module WebApi.AccountPages.Actions.Common
  ( AccountActionRequest,
    AccountActionResponse,
    AccountActionWorkflow,
    accountWorkflow,
    AccountActionResponseContext,
    accountActionResponseContext,
    issueMfaEnrollmentSessionNow,
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
    loginAttemptStoreErrorMessage,
    sessionStoreErrorMessage,
    mfaEnrollmentSessionStoreErrorMessage,
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
    noHeaders,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import HarchWeb qualified
import HarchWeb.Account qualified as Account
import HarchWeb.Email qualified as Email
import HarchWeb.Observability qualified as Observability
import HarchWeb.Session (OpaqueSession)
import HarchWeb.Time (UnixTimeNanoseconds)
import Network.HTTP.Types qualified as Http
import WebApi.Account (AccountProfile (..), AccountStoreError (..))
import WebApi.AccountPages.Actions.Contract (AccountAction, AccountActionTarget (UpdateProfileTarget))
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
import WebApi.Localization
import WebApi.Login (AccountCredentialStoreError (..), LoginAttemptStoreError (..))
import WebApi.Mfa (MfaStoreError (..))
import WebApi.MfaEnrollment (MfaEnrollmentError (..))
import WebApi.Profile (ProfileLoadError (..))
import WebApi.Route (AppLocale (..), AppRequestContext (..), AppRoute)
import WebApi.Session
  ( AccountSessionStoreError (..),
    MfaEnrollmentSessionStoreError (..),
    issueMfaEnrollmentSession,
  )

type AccountActionRequest = HarchWeb.ClientActionRequest AccountAction AppRequestContext

type AccountActionResponse = HarchWeb.ClientActionResponse AppRoute AppRequestContext

-- | The account action effect exposes one public client-action response on
-- both its success and failure rails. Focused workflow modules use this
-- shared boundary instead of inventing per-action effect stacks.
type AccountActionWorkflow = AppM AccountActionResponse AccountActionResponse

accountWorkflow :: AppM publicFailure AccountWorkflow
accountWorkflow = appAccountWorkflow <$> askAppServices

-- | Issue the already-narrow MFA enrollment capability after a workflow has
-- independently established an account principal. Registration verification
-- and password login are the two legitimate callers; response rendering stays
-- with their respective focused modules.
issueMfaEnrollmentSessionNow :: Account.AccountId -> UnixTimeNanoseconds -> AppM publicFailure (Either MfaEnrollmentSessionStoreError (OpaqueSession Account.AccountId))
issueMfaEnrollmentSessionNow accountId now = do
  workflow <- accountWorkflow
  liftIO (issueMfaEnrollmentSession (accountWorkflowMfaEnrollmentSessionStore workflow) accountId now)

pendingProfileForm :: AccountActionRequest -> AccountProfile -> Maybe Text -> Bool -> PendingProfileForm
pendingProfileForm actionRequest profile message isError =
  PendingProfileForm
    { pendingProfileFormEmail = Email.emailAddressText (accountProfileEmail profile),
      pendingProfileFormMessage = message,
      pendingProfileFormIsError = isError,
      pendingProfileFormResendLabel = resendLabel actionRequest
    }

resendLabel :: AccountActionRequest -> Text
resendLabel actionRequest = localized actionRequest ResendVerificationEmail

profileLoadErrorType :: Text
profileLoadErrorType = "AccountStoreError"

profileLoadErrorDetail :: ProfileLoadError -> Text
profileLoadErrorDetail loadError =
  case loadError of
    ProfileAccountStoreError storeError -> accountStoreErrorDetail storeError

localized :: AccountActionRequest -> AppMessage -> Text
localized actionRequest = localizedMessage (actionLocale actionRequest)

actionLocale :: AccountActionRequest -> AppLocale
actionLocale = requestLocale . HarchWeb.clientActionContext

mfaErrorMessage :: AccountActionRequest -> MfaEnrollmentError -> Text
mfaErrorMessage actionRequest errorValue =
  case errorValue of
    MfaEnrollmentAccountIsNotEligible -> localized actionRequest VerifyEmailBeforeEnrollment
    MfaEnrollmentInvalidCode -> localized actionRequest AuthenticatorCodeInvalid
    MfaEnrollmentNotFound -> localized actionRequest StartAuthenticatorEnrollment
    MfaEnrollmentConfirmationRejected -> localized actionRequest EnrollmentConfirmationUnavailable
    _ -> localized actionRequest AuthenticatorEnrollmentUnavailable

throwClientActionFailure :: AccountActionResponse -> FailureCode -> Text -> Text -> AppM AccountActionResponse value
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

attachClientActionFailure :: AppFailure AccountActionResponse -> AccountActionResponse
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

loginAttemptStoreErrorMessage :: LoginAttemptStoreError -> Text
loginAttemptStoreErrorMessage storeError =
  case storeError of
    LoginAttemptStoreUnavailable detail -> detail
    LoginAttemptStoreCorruptData detail -> detail

sessionStoreErrorMessage :: AccountSessionStoreError -> Text
sessionStoreErrorMessage storeError =
  case storeError of
    AccountSessionStoreUnavailable -> "account session store unavailable"
    AccountSessionStoreCorruptData -> "account session store returned corrupt data"

mfaEnrollmentSessionStoreErrorMessage :: MfaEnrollmentSessionStoreError -> Text
mfaEnrollmentSessionStoreErrorMessage storeError =
  case storeError of
    MfaEnrollmentSessionStoreUnavailable -> "MFA enrollment session store unavailable"
    MfaEnrollmentSessionStoreCorruptData -> "MFA enrollment session store returned corrupt data"

-- | Response metadata is captured once before rendering. The existing action
-- boundary still carries route context for cookie parsing, but each renderer
-- receives one cohesive value instead of manually assembling a response.
data AccountActionResponseContext = AccountActionResponseContext
  { accountActionResponseLocale :: AppLocale,
    accountActionResponseRequestContext :: AppRequestContext,
    accountActionResponseStatus :: Http.Status,
    accountActionResponseFocusId :: Maybe HarchWeb.ElementId,
    accountActionResponseHeaders :: Http.ResponseHeaders
  }

-- | Derive response metadata from the one action request before a workflow
-- chooses its form/body. This keeps locale and route context coupled to the
-- request that supplied them instead of passing transposable copies to every
-- renderer.
accountActionResponseContext :: AccountActionRequest -> Http.Status -> Maybe HarchWeb.ElementId -> Http.ResponseHeaders -> AccountActionResponseContext
accountActionResponseContext actionRequest status focusId headers =
  AccountActionResponseContext
    { accountActionResponseLocale = actionLocale actionRequest,
      accountActionResponseRequestContext = HarchWeb.clientActionContext actionRequest,
      accountActionResponseStatus = status,
      accountActionResponseFocusId = focusId,
      accountActionResponseHeaders = headers
    }

registrationResponse :: AccountActionResponseContext -> RegistrationForm -> AccountActionResponse
registrationResponse responseContext form =
  HarchWeb.ClientActionResponse
    { HarchWeb.clientActionStatus = status,
      HarchWeb.clientActionPatches = replaceRegionPatch (registrationRegion requestContext locale form),
      HarchWeb.clientActionFocusId = focusId,
      HarchWeb.clientActionNavigation = HarchWeb.StayOnCurrentRoute,
      HarchWeb.clientActionHeaders = headers,
      HarchWeb.clientActionObservabilityAttributes = [],
      HarchWeb.clientActionLogEntries = []
    }
  where
    locale = accountActionResponseLocale responseContext
    requestContext = accountActionResponseRequestContext responseContext
    status = accountActionResponseStatus responseContext
    focusId = accountActionResponseFocusId responseContext
    headers = accountActionResponseHeaders responseContext

verificationResponse :: AccountActionResponseContext -> VerificationForm -> AccountActionResponse
verificationResponse responseContext form =
  HarchWeb.ClientActionResponse
    { HarchWeb.clientActionStatus = status,
      HarchWeb.clientActionPatches = replaceRegionPatch (verificationRegion requestContext locale form),
      HarchWeb.clientActionFocusId = focusId,
      HarchWeb.clientActionNavigation = HarchWeb.StayOnCurrentRoute,
      HarchWeb.clientActionHeaders = headers,
      HarchWeb.clientActionObservabilityAttributes = [],
      HarchWeb.clientActionLogEntries = []
    }
  where
    locale = accountActionResponseLocale responseContext
    requestContext = accountActionResponseRequestContext responseContext
    status = accountActionResponseStatus responseContext
    focusId = accountActionResponseFocusId responseContext
    headers = accountActionResponseHeaders responseContext

mfaEnrollmentResponse :: AccountActionResponseContext -> MfaEnrollmentForm -> AccountActionResponse
mfaEnrollmentResponse responseContext form =
  HarchWeb.ClientActionResponse
    { HarchWeb.clientActionStatus = status,
      HarchWeb.clientActionPatches = replaceRegionPatch (mfaEnrollmentRegion requestContext locale form),
      HarchWeb.clientActionFocusId = focusId,
      HarchWeb.clientActionNavigation = HarchWeb.StayOnCurrentRoute,
      HarchWeb.clientActionHeaders = headers,
      HarchWeb.clientActionObservabilityAttributes = [],
      HarchWeb.clientActionLogEntries = []
    }
  where
    locale = accountActionResponseLocale responseContext
    requestContext = accountActionResponseRequestContext responseContext
    status = accountActionResponseStatus responseContext
    focusId = accountActionResponseFocusId responseContext
    headers = accountActionResponseHeaders responseContext

loginResponse :: AccountActionResponseContext -> LoginForm -> AccountActionResponse
loginResponse responseContext form =
  HarchWeb.ClientActionResponse
    { HarchWeb.clientActionStatus = status,
      HarchWeb.clientActionPatches = replaceRegionPatch (loginRegion requestContext locale form),
      HarchWeb.clientActionFocusId = focusId,
      HarchWeb.clientActionNavigation = HarchWeb.StayOnCurrentRoute,
      HarchWeb.clientActionHeaders = headers,
      HarchWeb.clientActionObservabilityAttributes = [],
      HarchWeb.clientActionLogEntries = []
    }
  where
    locale = accountActionResponseLocale responseContext
    requestContext = accountActionResponseRequestContext responseContext
    status = accountActionResponseStatus responseContext
    focusId = accountActionResponseFocusId responseContext
    headers = accountActionResponseHeaders responseContext

logoutResponse :: AccountActionResponseContext -> Maybe (Text, Bool) -> AccountActionResponse
logoutResponse responseContext message =
  HarchWeb.ClientActionResponse
    { HarchWeb.clientActionStatus = status,
      HarchWeb.clientActionPatches = replaceRegionPatch (logoutRegion requestContext locale message),
      HarchWeb.clientActionFocusId = accountActionResponseFocusId responseContext,
      HarchWeb.clientActionNavigation = HarchWeb.StayOnCurrentRoute,
      HarchWeb.clientActionHeaders = headers,
      HarchWeb.clientActionObservabilityAttributes = [],
      HarchWeb.clientActionLogEntries = []
    }
  where
    locale = accountActionResponseLocale responseContext
    requestContext = accountActionResponseRequestContext responseContext
    status = accountActionResponseStatus responseContext
    headers = accountActionResponseHeaders responseContext

profileResponse :: AccountActionRequest -> Http.Status -> PendingProfileForm -> AccountActionResponse
profileResponse actionRequest status form =
  HarchWeb.ClientActionResponse
    { HarchWeb.clientActionStatus = status,
      HarchWeb.clientActionPatches = replaceRegionPatch (pendingProfileRegion (HarchWeb.clientActionContext actionRequest) UpdateProfileTarget form),
      HarchWeb.clientActionFocusId = Nothing,
      HarchWeb.clientActionNavigation = HarchWeb.StayOnCurrentRoute,
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

-- | A single named binding for "no extra response headers," used at every
-- call site that would otherwise repeat an empty-list literal of this exact
-- type. Repeating the literal risks the CSE-sharing HPC gap this codebase
-- has already hit more than once (see the AC decision record in
-- docs/design-guidance.md): GHC can common up textually identical literals
-- into one CAF, silently leaving every call site but one permanently
-- unticked even though each is genuinely reached.
noHeaders :: Http.ResponseHeaders
noHeaders = []
