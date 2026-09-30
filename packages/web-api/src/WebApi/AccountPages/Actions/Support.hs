-- | Application dependencies and pure helpers shared by account actions.
--
-- Decision (AHI-5-WA-MH, 2026-09-29): these helpers stay at the application
-- action boundary; they do not create another dispatcher or effect stack.
-- The public 'WebApi.AccountPages.Actions.Common' module re-exports them,
-- while response interpretation and private failure reporting have separate
-- owners.
module WebApi.AccountPages.Actions.Support
  ( accountWorkflow,
    issueMfaEnrollmentSessionNow,
    pendingProfileForm,
    resendLabel,
    localized,
    actionLocale,
    mfaErrorMessage,
    emailVerificationLifetimeNanoseconds,
    emailLocale,
    validPassword,
    nonEmptyText,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import HarchWeb qualified
import HarchWeb.Account qualified as Account
import HarchWeb.Email qualified as Email
import HarchWeb.Session (OpaqueSession)
import HarchWeb.Time (UnixTimeNanoseconds)
import WebApi.Account (AccountProfile (..))
import WebApi.AccountPages.Actions.Types (AccountActionRequest)
import WebApi.AccountPages.Forms (PendingProfileForm (..))
import WebApi.AppEffect
  ( AccountWorkflow (..),
    AppM,
    AppServices (..),
    askAppServices,
  )
import WebApi.Localization
import WebApi.MfaEnrollment (MfaEnrollmentError (..))
import WebApi.Route (AppLocale (..), AppRequestContext (..))
import WebApi.Session
  ( MfaEnrollmentSessionStoreError,
    issueMfaEnrollmentSession,
  )

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
