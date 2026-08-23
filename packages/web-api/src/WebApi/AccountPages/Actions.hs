-- | Account action orchestration and CSRF policy.
--
-- Decision record (CZ, 2026-08-23): session-bound CSRF policy belongs at the
-- existing typed action boundary, after framework transport validation and
-- before a workflow can run. Ordinary account and MFA enrollment sessions are
-- distinct capabilities, so page token issuance receives the rendered route
-- and selects only that route's live session; anonymous pages receive a fresh
-- framework token. Store failures, revocation, expiry, and missing sessions
-- fail closed without exposing storage details.
module WebApi.AccountPages.Actions
  ( AccountAction,
    AccountActionTarget (..),
    accountActions,
    authorizeAccountActionCsrf,
    handleAccountAction,
    mfaEnrollmentFailureDiagnostics,
    pageCsrfTokenForAccountPage,
  )
where

import HarchWeb qualified
import HarchWeb.Session qualified as Session
import HarchWeb.Time (UnixTimeNanoseconds)
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
  ( AccountWorkflow (..),
    AppM,
    AppServices (..),
    runAppM,
  )
import WebApi.Route (AppRequestContext (..), AppRoute (..))
import WebApi.Session
  ( AccountSessionStore (..),
    MfaEnrollmentSessionStore (..),
  )

type AccountActionRequest = HarchWeb.ClientActionRequest AccountAction AppRequestContext

type AccountActionWorkflow = AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse

-- | Selects the only session capability that may authorize the rendered page.
-- A bad or missing cookie/store record gets a fresh anonymous transport token;
-- the authorization hook below still rejects any protected submission.
pageCsrfTokenForAccountPage :: AccountWorkflow -> HarchWeb.Page AppRoute AppRequestContext -> IO Session.CsrfToken
pageCsrfTokenForAccountPage workflow page = do
  now <- accountWorkflowClock workflow
  maybeToken <-
    case HarchWeb.pageRoute page of
      ProfileRoute -> accountSessionCsrfToken workflow now (requestSessionId (HarchWeb.pageContext page))
      LogoutRoute -> accountSessionCsrfToken workflow now (requestSessionId (HarchWeb.pageContext page))
      MfaEnrollmentRoute -> mfaEnrollmentSessionCsrfToken workflow now (requestMfaEnrollmentSessionId (HarchWeb.pageContext page))
      _ -> pure Nothing
  maybe Session.generateCsrfToken pure maybeToken

-- | Binds protected decoded actions to their current, non-expired session.
-- Register, verification, and login legitimately begin without a session;
-- every other account mutation requires the dedicated capability it uses.
authorizeAccountActionCsrf :: AccountWorkflow -> AccountActionRequest -> Session.CsrfToken -> IO Bool
authorizeAccountActionCsrf workflow actionRequest suppliedToken =
  case HarchWeb.clientAction actionRequest of
    RegisterAccount _ -> pure True
    VerifyEmail _ -> pure True
    LoginAccount _ -> pure True
    UpdateProfile _ -> authorizeAccountSession
    LogoutAccount -> authorizeAccountSession
    EnrollMfa _ -> authorizeMfaEnrollmentSession
  where
    requestContext = HarchWeb.clientActionContext actionRequest
    authorizeAccountSession = do
      now <- accountWorkflowClock workflow
      matchesCsrfToken suppliedToken <$> accountSessionCsrfToken workflow now (requestSessionId requestContext)
    authorizeMfaEnrollmentSession = do
      now <- accountWorkflowClock workflow
      matchesCsrfToken suppliedToken <$> mfaEnrollmentSessionCsrfToken workflow now (requestMfaEnrollmentSessionId requestContext)

accountSessionCsrfToken :: AccountWorkflow -> UnixTimeNanoseconds -> Maybe Session.SessionId -> IO (Maybe Session.CsrfToken)
accountSessionCsrfToken workflow now maybeSessionId =
  case maybeSessionId of
    Nothing -> pure Nothing
    Just sessionId -> do
      loaded <- loadAccountSession (accountWorkflowSessionStore workflow) sessionId
      pure (storedSessionCsrfToken now loaded)

mfaEnrollmentSessionCsrfToken :: AccountWorkflow -> UnixTimeNanoseconds -> Maybe Session.SessionId -> IO (Maybe Session.CsrfToken)
mfaEnrollmentSessionCsrfToken workflow now maybeSessionId =
  case maybeSessionId of
    Nothing -> pure Nothing
    Just sessionId -> do
      loaded <- loadMfaEnrollmentSession (accountWorkflowMfaEnrollmentSessionStore workflow) sessionId
      pure (storedSessionCsrfToken now loaded)

storedSessionCsrfToken :: UnixTimeNanoseconds -> Either storeError (Maybe (Session.OpaqueSession principal)) -> Maybe Session.CsrfToken
storedSessionCsrfToken now loaded =
  case loaded of
    Right sessionValue ->
      case Session.validateSession now sessionValue of
        Session.ActiveSession activeSession -> Just (Session.sessionCsrfToken activeSession)
        Session.MissingSession -> Nothing
        Session.ExpiredSession -> Nothing
    Left _ -> Nothing

matchesCsrfToken :: Session.CsrfToken -> Maybe Session.CsrfToken -> Bool
matchesCsrfToken suppliedToken =
  maybe False (`Session.validateCsrfToken` suppliedToken)

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
