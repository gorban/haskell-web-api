{-# LANGUAGE OverloadedStrings #-}

-- | The accepted-login durable-operation boundary.
--
-- Decision (AHI-5-WF, 2026-09-09): keep parsing and interpretation of every
-- password/MFA outcome in the public 'Workflows' façade, but move the one
-- cohesive accepted path here. It prepares the opaque session, produces the
-- application JWT, derives the trusted audit activity, and invokes the
-- application-owned atomic session/audit port on the existing 'AppM' failure
-- rail. A second generic login monad or a post-commit audit logger would
-- either duplicate that rail or break the no-cookie-before-atomic-commit
-- invariant. This internal boundary instead receives one explicit input
-- record and produces one explicit committed-output record; its caller alone
-- renders the ordinary client-action response.
module WebApi.AccountPages.Actions.Login
  ( AcceptedLoginInput (..),
    handleAcceptedLogin,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb qualified
import HarchWeb.Account qualified as Account
import HarchWeb.Session (OpaqueSession (..))
import HarchWeb.Time (UnixTimeNanoseconds)
import Network.HTTP.Types qualified as Http
import WebApi.AccountJwt (AccountJwtIssueError (..), AccountJwtIssuer (..))
import WebApi.AccountPages.Actions.Common
import WebApi.AccountPages.FieldIds (loginIdentifierId)
import WebApi.AccountPages.Forms (FormFeedback (FormStatusMessage), FormStatus (..), FormStatusKind (FormStatusFailure, FormStatusSuccess), LoginForm (..), LoginProofChoice (..))
import WebApi.AccountSessionAudit (AccountSessionAuditStore (..), AccountSessionAuditStoreError (..))
import WebApi.ActivityAudit
  ( AccountActivity (..),
    AccountAuditEvent (AccountSessionIssued),
    AuditAuthenticationMethod (..),
    auditRouteObservationFromTrusted,
  )
import WebApi.AppEffect (AccountWorkflow (..), AppM, FailureCode (LoginJwtIssueFailure, LoginSessionFailure))
import WebApi.Localization (AppMessage (SignInUnavailable, SignedIn))
import WebApi.Route (AppRoute (ProfileRoute), requestCorrelationId, requestRouteObservation)
import WebApi.Session (prepareAccountSession)

data AcceptedLoginInput = AcceptedLoginInput
  { acceptedLoginRequest :: AccountActionRequest,
    acceptedLoginIdentifier :: Text,
    acceptedLoginProof :: LoginProofChoice,
    acceptedLoginNowNanoseconds :: UnixTimeNanoseconds,
    acceptedLoginAccountId :: Account.AccountId
  }

-- | Values that exist only after the atomic session/audit operation has
-- committed. Keeping the rendered cookie here makes the response boundary
-- unable to attach it before durable success.
data AcceptedLoginOutput = AcceptedLoginOutput
  { acceptedLoginSession :: OpaqueSession Account.AccountId,
    acceptedLoginCookieHeader :: Http.Header
  }

handleAcceptedLogin :: AcceptedLoginInput -> AccountActionWorkflow
handleAcceptedLogin input = renderAcceptedLogin input <$> commitAcceptedLogin input

commitAcceptedLogin :: AcceptedLoginInput -> AppM AccountActionResponse AcceptedLoginOutput
commitAcceptedLogin input = do
  opaqueSession <- prepareAcceptedSession input
  (jwt, cookiePolicy) <- issueAcceptedSessionJwt input opaqueSession
  renderedCookie <-
    maybe
      (throwLoginFailure input LoginJwtIssueFailure "AccountJwtCookieError" "issued account JWT cannot be rendered as a cookie")
      pure
      (HarchWeb.renderAuthenticationCookie cookiePolicy jwt)
  activity <-
    either
      (throwLoginFailure input LoginSessionFailure "AccountSessionAuditStoreError" . sessionAuditStoreErrorMessage)
      pure
      (loginSessionActivity input (sessionPrincipal opaqueSession))
  workflow <- accountWorkflow
  persisted <- liftIO (saveAccountSessionWithAudit (accountWorkflowSessionAuditStore workflow) opaqueSession activity)
  case persisted of
    Left storeError ->
      case requiredAuditFailure storeError of
        Just auditFailure -> throwRequiredAuditFailure (unavailableLoginResponse input) LoginSessionFailure AccountSessionIssueAudit auditFailure
        Nothing -> throwLoginFailure input LoginSessionFailure "AccountSessionAuditStoreError" (sessionAuditStoreErrorMessage storeError)
    Right False -> throwLoginFailure input LoginSessionFailure "AccountSessionAuditStoreError" "account session identifier collision"
    Right True ->
      pure
        AcceptedLoginOutput
          { acceptedLoginSession = opaqueSession,
            acceptedLoginCookieHeader = setCookieHeader renderedCookie
          }

prepareAcceptedSession :: AcceptedLoginInput -> AppM AccountActionResponse (OpaqueSession Account.AccountId)
prepareAcceptedSession input = do
  prepared <- liftIO (prepareAccountSession (acceptedLoginAccountId input) (acceptedLoginNowNanoseconds input))
  either
    (throwLoginFailure input LoginSessionFailure "AccountSessionStoreError" . sessionStoreErrorMessage)
    pure
    prepared

issueAcceptedSessionJwt :: AcceptedLoginInput -> OpaqueSession Account.AccountId -> AppM AccountActionResponse (HarchWeb.EncodedJwt, HarchWeb.AuthenticationCookiePolicy)
issueAcceptedSessionJwt input opaqueSession = do
  workflow <- accountWorkflow
  let jwtIssuer = accountWorkflowJwtIssuer workflow
  issued <- liftIO (issueAccountSessionJwt jwtIssuer opaqueSession)
  either
    (throwLoginFailure input LoginJwtIssueFailure "AccountJwtIssueError" . accountJwtIssueErrorMessage)
    (pure . (,accountJwtCookie jwtIssuer))
    issued

renderAcceptedLogin :: AcceptedLoginInput -> AcceptedLoginOutput -> AccountActionResponse
renderAcceptedLogin input output =
  ( loginResponse
      (accountActionResponseContext (acceptedLoginRequest input) Http.status200 Nothing [HarchWeb.csrfClearCookieHeader, acceptedLoginCookieHeader output])
      (LoginForm (acceptedLoginIdentifier input) (Just (acceptedLoginProof input)) (FormStatusMessage (FormStatus (localized (acceptedLoginRequest input) SignedIn) FormStatusSuccess)))
  )
    { HarchWeb.clientActionNavigation =
        HarchWeb.NavigateInternal
          HarchWeb.ReplaceHistory
          (HarchWeb.RouteRequest ProfileRoute (HarchWeb.clientActionContext (acceptedLoginRequest input)))
    }

throwLoginFailure :: AcceptedLoginInput -> FailureCode -> Text -> Text -> AppM AccountActionResponse value
throwLoginFailure input = throwClientActionFailure (unavailableLoginResponse input)

unavailableLoginResponse :: AcceptedLoginInput -> AccountActionResponse
unavailableLoginResponse input =
  loginResponse
    (accountActionResponseContext (acceptedLoginRequest input) Http.status503 (Just loginIdentifierId) [])
    (LoginForm (acceptedLoginIdentifier input) (Just (acceptedLoginProof input)) (FormStatusMessage (FormStatus (localized (acceptedLoginRequest input) SignInUnavailable) FormStatusFailure)))

loginSessionActivity :: AcceptedLoginInput -> Account.AccountId -> Either AccountSessionAuditStoreError AccountActivity
loginSessionActivity input accountId = do
  requestId <- maybe (Left AccountSessionAuditStoreCorruptData) Right (requestCorrelationId context)
  route <- traverse (either (const (Left AccountSessionAuditStoreCorruptData)) Right . auditRouteObservationFromTrusted) (requestRouteObservation context)
  pure
    AccountActivity
      { activitySubject = accountId,
        activityRequestId = requestId,
        activityEvent = AccountSessionIssued (auditMethod (acceptedLoginProof input)),
        activityRoute = route
      }
  where
    context = HarchWeb.clientActionContext (acceptedLoginRequest input)

auditMethod :: LoginProofChoice -> AuditAuthenticationMethod
auditMethod proofChoice =
  case proofChoice of
    LoginAuthenticatorProof -> TotpAuthenticationMethod
    LoginRecoveryProof -> RecoveryCodeAuthenticationMethod

setCookieHeader :: Text -> Http.Header
setCookieHeader cookie = ("Set-Cookie", TextEncoding.encodeUtf8 cookie)

accountJwtIssueErrorMessage :: AccountJwtIssueError -> Text
accountJwtIssueErrorMessage issueError =
  case issueError of
    AccountJwtIssueFailed -> "account JWT issuance is unavailable"

sessionAuditStoreErrorMessage :: AccountSessionAuditStoreError -> Text
sessionAuditStoreErrorMessage storeError =
  case storeError of
    AccountSessionAuditStoreUnavailable -> "account session audit store unavailable"
    AccountSessionAuditCapacityExceeded -> "account audit partition capacity is exhausted"
    AccountSessionAuditStoreCorruptData -> "account session audit store returned corrupt data"

requiredAuditFailure :: AccountSessionAuditStoreError -> Maybe RequiredAuditFailure
requiredAuditFailure storeError =
  case storeError of
    AccountSessionAuditStoreUnavailable -> Just RequiredAuditUnavailable
    AccountSessionAuditCapacityExceeded -> Just RequiredAuditCapacityExceeded
    AccountSessionAuditStoreCorruptData -> Just RequiredAuditCorruptResult
