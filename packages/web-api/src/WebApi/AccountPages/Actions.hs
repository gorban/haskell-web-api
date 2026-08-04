module WebApi.AccountPages.Actions
  ( AccountAction,
    AccountActionTarget (..),
    accountActions,
    handleAccountAction,
    mfaEnrollmentFailureDiagnostics,
  )
where

import HarchWeb qualified
import WebApi.AccountPages.Actions.Common
  ( attachClientActionFailure,
  )
import WebApi.AccountPages.Actions.Contract
import WebApi.AccountPages.Actions.Workflows
  ( handleLoginSubmission,
    handleLogout,
    handleMfaEnrollmentSubmission,
    handleProfileSubmission,
    handleRegistrationSubmission,
    handleVerificationSubmission,
    mfaEnrollmentFailureDiagnostics,
  )
import WebApi.AppEffect
  ( AccountWorkflow,
    AppM,
    AppServices (..),
    runAppM,
  )
import WebApi.Route (AppRequestContext)

type AccountActionRequest = HarchWeb.ClientActionRequest AccountAction AppRequestContext

type AccountActionWorkflow = AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse

handleAccountAction :: AccountWorkflow -> AccountActionRequest -> IO (Maybe HarchWeb.ClientActionResponse)
handleAccountAction workflow actionRequest =
  Just <$> runSelectedAccountAction (accountActionCodec actionRequest)
  where
    runSelectedAccountAction selectedAction =
      either attachClientActionFailure id <$> runAppM (AppServices workflow) selectedAction

accountActionCodec :: AccountActionRequest -> AccountActionWorkflow
accountActionCodec actionRequest =
  case HarchWeb.clientAction actionRequest of
    RegisterAccount submission -> handleRegistrationSubmission actionRequest submission
    VerifyEmail submission -> handleVerificationSubmission actionRequest submission
    EnrollMfa submission -> handleMfaEnrollmentSubmission actionRequest submission
    LoginAccount submission -> handleLoginSubmission actionRequest submission
    UpdateProfile submission -> handleProfileSubmission actionRequest submission
    LogoutAccount -> handleLogout actionRequest
