{-# LANGUAGE OverloadedStrings #-}

-- | Account action orchestration and CSRF policy.
--
-- Decision record (AHI-4C, 2026-09-03): session-bound CSRF belongs at Harch's
-- existing typed action boundary, after its mandatory transport validation and
-- before any workflow can run.  This application supplies one signed backend,
-- whose binding is the complete canonical set of live account and MFA
-- enrollment grants carried by the request.  The binding is immediately
-- hashed by Harch; session and account identifiers never enter the token or
-- diagnostics.  Store failure is unavailable, never anonymous; revocation,
-- expiry, or a changed grant set reject a previously issued token.
module WebApi.AccountPages.Actions
  ( AccountAction,
    AccountActionTarget (..),
    accountCsrfProtection,
    accountActionEndpointMetadata,
    accountActions,
    handleAccountAction,
    mfaEnrollmentFailureDiagnostics,
  )
where

import Data.ByteString qualified as ByteString
import Data.List (sortOn)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb qualified
import HarchWeb.Account (AccountId, accountIdText)
import HarchWeb.Session (OpaqueSession (..), SessionId, sessionIdText)
import HarchWeb.Session qualified as Session
import HarchWeb.Time (UnixTimeNanoseconds, unixTimeNanosecondsValue)
import WebApi.AccountPages.Actions.Common
  ( AccountActionResponse,
    attachClientActionFailure,
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
import WebApi.AccountPrincipal
  ( AccountPrincipal,
    accountPrincipalAccountId,
    accountPrincipalSessionExpiresAtNanoseconds,
    accountPrincipalSessionId,
  )
import WebApi.AppEffect
  ( AccountWorkflow (..),
    AppM,
    AppServices (..),
    runAppM,
  )
import WebApi.Route (AppRequestContext (..))
import WebApi.Session
  ( MfaEnrollmentSessionStore (..),
  )

type AccountActionRequest = HarchWeb.ClientActionRequest AccountAction AppRequestContext

type AccountActionWorkflow = AppM AccountActionResponse AccountActionResponse

-- | The application-selected signed backend. Harch still owns exact-one
-- cookie/form extraction and constant-work double-submit equality; this
-- resolver only turns durable grants into its opaque binding rail.
accountCsrfProtection :: AccountWorkflow -> HarchWeb.CsrfProtection AppRequestContext
accountCsrfProtection workflow =
  HarchWeb.signedCsrfProtection
    (accountWorkflowCsrfSigningKeyring workflow)
    HarchWeb.defaultSignedCsrfPolicy
    (accountWorkflowClock workflow)
    (resolveCsrfBinding workflow)

resolveCsrfBinding :: AccountWorkflow -> AppRequestContext -> IO HarchWeb.CsrfBindingResolution
resolveCsrfBinding workflow requestContext = do
  now <- accountWorkflowClock workflow
  let accountGrant = activeAccountGrant (requestAccountPrincipal requestContext)
  enrollmentGrant <- activeEnrollmentGrant workflow now (requestMfaEnrollmentSessionId requestContext)
  case enrollmentGrant of
    Left () -> pure HarchWeb.CsrfBindingUnavailable
    Right maybeEnrollmentGrant ->
      case accountGrant <> maybeEnrollmentGrant of
        [] -> pure HarchWeb.AnonymousCsrfBinding
        activeGrants ->
          pure
            ( HarchWeb.BoundCsrfBinding
                (HarchWeb.csrfBindingFromCanonicalBytes (renderGrantBinding activeGrants))
                (minimum (map csrfGrantExpiry activeGrants))
            )

data CsrfGrant = CsrfGrant
  { csrfGrantDomain :: ByteString.ByteString,
    csrfGrantSessionId :: SessionId,
    csrfGrantAccountId :: AccountId,
    csrfGrantExpiry :: UnixTimeNanoseconds
  }

activeAccountGrant :: Maybe AccountPrincipal -> [CsrfGrant]
activeAccountGrant maybePrincipal =
  case maybePrincipal of
    Nothing -> []
    Just principal ->
      [ CsrfGrant
          { csrfGrantDomain = "account-session",
            csrfGrantSessionId = accountPrincipalSessionId principal,
            csrfGrantAccountId = accountPrincipalAccountId principal,
            csrfGrantExpiry = accountPrincipalSessionExpiresAtNanoseconds principal
          }
      ]

activeEnrollmentGrant :: AccountWorkflow -> UnixTimeNanoseconds -> Maybe SessionId -> IO (Either () [CsrfGrant])
activeEnrollmentGrant workflow now maybeSessionId =
  case maybeSessionId of
    Nothing -> pure (Right [])
    Just sessionIdValue ->
      activeGrant
        now
        "mfa-enrollment-session"
        (loadMfaEnrollmentSession (accountWorkflowMfaEnrollmentSessionStore workflow) sessionIdValue)

activeGrant :: UnixTimeNanoseconds -> ByteString.ByteString -> IO (Either storeError (Maybe (OpaqueSession AccountId))) -> IO (Either () [CsrfGrant])
activeGrant now grantDomain loadSession = do
  loadedSession <- loadSession
  pure
    ( case loadedSession of
        Left _ -> Left ()
        Right maybeSession ->
          case Session.validateSession now maybeSession of
            Session.ActiveSession sessionValue ->
              Right
                [ CsrfGrant
                    { csrfGrantDomain = grantDomain,
                      csrfGrantSessionId = sessionId sessionValue,
                      csrfGrantAccountId = sessionPrincipal sessionValue,
                      csrfGrantExpiry = sessionExpiresAtNanoseconds sessionValue
                    }
                ]
            Session.MissingSession -> Right []
            Session.ExpiredSession -> Right []
    )

renderGrantBinding :: [CsrfGrant] -> ByteString.ByteString
renderGrantBinding activeGrants =
  ByteString.intercalate
    "\NUL"
    ( "web-api-csrf-grants-v1"
        : concatMap renderGrant (sortOn csrfGrantDomain activeGrants)
    )
  where
    renderGrant grant =
      [ csrfGrantDomain grant,
        TextEncoding.encodeUtf8 (sessionIdText (csrfGrantSessionId grant)),
        TextEncoding.encodeUtf8 (accountIdText (csrfGrantAccountId grant)),
        TextEncoding.encodeUtf8 (Text.pack (show (unixTimeNanosecondsValue (csrfGrantExpiry grant))))
      ]

handleAccountAction :: AccountWorkflow -> AccountActionRequest -> IO (Maybe AccountActionResponse)
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
