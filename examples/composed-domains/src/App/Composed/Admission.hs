{-# LANGUAGE OverloadedStrings #-}

-- | Application-owned pre-auth admission policy for the composed example.
--
-- This module is deliberately the small composition facade. Credential/TOTP
-- proof and reservation ownership live in 'App.Composed.Admission.Proof';
-- durable session issuance and validation live in
-- 'App.Composed.Admission.Session'. Keeping this public rail here prevents
-- transports from choosing different proof or session interpretations.
--
-- Decision (AHI-4C-AMH, 2026-09-04): split the former combined admission
-- workflow by its two stable capabilities without widening the application
-- surface. Harch still owns the generic cancellation-safe reservation handoff
-- and the endpoint dispatcher; this application owns credential encryption,
-- admission-specific stores, session lifecycle, and policy.
module App.Composed.Admission
  ( AdmissionCompositionError (..),
    AdmissionConfig (..),
    AdmissionConfigError (..),
    AdmissionCredentialStore (..),
    AdmissionCredentialStoreError (..),
    AdmissionAttemptAdmission (..),
    AdmissionAttemptBudget (..),
    AdmissionAttemptBudgets,
    AdmissionPrincipalKey (..),
    AdmissionAttemptReservation (..),
    AdmissionAttemptScope (..),
    AdmissionAttemptStore (..),
    AdmissionAttemptStoreError (..),
    AdmissionProofConfig (..),
    AdmissionProofClockError (..),
    AdmissionProofResult (..),
    AdmissionSessionStore (..),
    AdmissionSessionStoreError (..),
    AdmissionSessionClockError (..),
    AdmissionSessionIssueError (..),
    AdmissionPolicy (..),
    AdmissionRequirement (..),
    StoredAdmissionCredential (..),
    AdmissionSubmissionResult (..),
    admissionGuard,
    admissionRequirement,
    admissionAttemptScopeStorageKey,
    admissionAttemptBudgetsToList,
    applyAdmissionPolicy,
    completeAdmissionProof,
    defaultAdmissionSessionCookiePolicy,
    issueAdmissionSession,
    mkAdmissionConfig,
    resolveAdmissionCsrfBinding,
    submitAdmission,
  )
where

import App.Composed.Admission.Proof
  ( AdmissionAttemptAdmission (..),
    AdmissionAttemptBudget (..),
    AdmissionAttemptBudgets,
    AdmissionAttemptReservation (..),
    AdmissionAttemptScope (..),
    AdmissionAttemptStore (..),
    AdmissionAttemptStoreError (..),
    AdmissionCredentialStore (..),
    AdmissionCredentialStoreError (..),
    AdmissionPrincipalKey (..),
    AdmissionProofClockError (..),
    AdmissionProofConfig (..),
    AdmissionProofResult (..),
    StoredAdmissionCredential (..),
    admissionAttemptBudgetsToList,
    admissionAttemptScopeStorageKey,
    completeAdmissionProof,
  )
import App.Composed.Admission.Session
  ( AdmissionConfig (..),
    AdmissionConfigError (..),
    AdmissionGuardFailure (..),
    AdmissionSessionClockError (..),
    AdmissionSessionIssueError (..),
    AdmissionSessionStore (..),
    AdmissionSessionStoreError (..),
    defaultAdmissionSessionCookiePolicy,
    establishAdmissionPrincipal,
    issueAdmissionSession,
    mkAdmissionConfig,
    resolveAdmissionCsrfBinding,
  )
import App.Composed.Admission.Types
import App.Composed.Model
import Control.Monad.Except (runExceptT)
import HarchWeb.EndpointSecurity
  ( ApplicationSecurity (..),
    EndpointDispatchKind (EndpointClientAction),
    EndpointGuard (..),
    EndpointGuardResult (..),
    EndpointRequest (..),
  )
import HarchWeb.RequestContext (RequestContext (..))
import HarchWeb.Routing (RouteRequest (..))
import HarchWeb.Security (ClientAddress)
import HarchWeb.Server
  ( ActionNavigation (NavigateInternal),
    ClientActionResponse (..),
    HistoryMode (ReplaceHistory),
    NonPageResponse (..),
    ResponseBody (..),
    nonPageInternalRedirectResponse,
  )
import HarchWeb.Session (OpaqueSession)
import HarchWeb.Totp (TotpCode)
import Network.HTTP.Types qualified as Http

data AdmissionRequirement
  = RequireAdmission
  | AllowWithoutAdmission
  deriving (Eq, Show)

-- | The sole shared result for enhanced and native admission submission. A
-- proof acceptance becomes a grant only after the durable session write.
data AdmissionSubmissionResult
  = AdmissionSubmissionAccepted (OpaqueSession AdmissionPrincipalId)
  | AdmissionSubmissionRejected
  | AdmissionSubmissionUnavailable
  deriving (Eq)

instance Show AdmissionSubmissionResult where
  show result =
    case result of
      AdmissionSubmissionAccepted _ -> "AdmissionSubmissionAccepted <redacted>"
      AdmissionSubmissionRejected -> "AdmissionSubmissionRejected"
      AdmissionSubmissionUnavailable -> "AdmissionSubmissionUnavailable"

submitAdmission :: AdmissionConfig -> AdmissionProofConfig -> ClientAddress -> AdmissionLoginName -> TotpCode -> IO AdmissionSubmissionResult
submitAdmission sessionConfig proofConfig clientAddress loginName suppliedCode = do
  proofResult <- completeAdmissionProof proofConfig clientAddress loginName suppliedCode
  case proofResult of
    AdmissionProofAccepted principalId -> do
      issuedSession <- issueAdmissionSession sessionConfig principalId
      pure $
        case issuedSession of
          Right session -> AdmissionSubmissionAccepted session
          Left _ -> AdmissionSubmissionUnavailable
    AdmissionProofRejected -> pure AdmissionSubmissionRejected
    AdmissionProofReplayed -> pure AdmissionSubmissionRejected
    AdmissionProofThrottled -> pure AdmissionSubmissionRejected
    AdmissionProofUnavailable -> pure AdmissionSubmissionUnavailable

data AdmissionPolicy
  = AdmissionDisabled
  | AdmissionEnabled AdmissionConfig AdmissionProofConfig

instance Show AdmissionPolicy where
  show policy =
    case policy of
      AdmissionDisabled -> "AdmissionDisabled"
      AdmissionEnabled _ _ -> "AdmissionEnabled <redacted>"

data AdmissionCompositionError
  = AdmissionRequiresConfiguredAuthentication
  deriving (Eq, Show)

applyAdmissionPolicy :: AdmissionPolicy -> ApplicationSecurity RootRoute ComposedContext RootAuthorization -> Either AdmissionCompositionError (ApplicationSecurity RootRoute ComposedContext RootAuthorization)
applyAdmissionPolicy policy applicationSecurity =
  case policy of
    AdmissionDisabled -> Right applicationSecurity
    AdmissionEnabled config _ ->
      case applicationSecurity of
        AuthenticationDisabled _ -> Left AdmissionRequiresConfiguredAuthentication
        AuthenticationEnabled beforeGuards accountAuthentication afterGuards ->
          Right (AuthenticationEnabled (admissionGuard config : beforeGuards) accountAuthentication afterGuards)

admissionRequirement :: RootRoute -> AdmissionRequirement
admissionRequirement (Localized _ localRoute) =
  case localRoute of
    Public PublicAdmission -> AllowWithoutAdmission
    Public PublicAdmissionNativeFallback -> AllowWithoutAdmission
    Public (PublicAsset _) -> AllowWithoutAdmission
    Public PublicNotFound -> AllowWithoutAdmission
    Public PublicLogin -> RequireAdmission
    Catalog _ -> RequireAdmission
    Orders _ -> RequireAdmission

admissionGuard :: AdmissionConfig -> EndpointGuard RootRoute ComposedContext RootAuthorization
admissionGuard config = EndpointGuard $ \endpointRequest ->
  case admissionRequirement (requestRoute (endpointRouteRequest endpointRequest)) of
    AllowWithoutAdmission -> pure (ContinueEndpoint (requestContext (endpointRouteRequest endpointRequest)))
    RequireAdmission -> do
      admissionResult <- runExceptT (establishAdmissionPrincipal config endpointRequest)
      pure $
        case admissionResult of
          Right principal ->
            ContinueEndpoint
              ( (requestContext (endpointRouteRequest endpointRequest))
                  { requestLocal = AdmissionEstablished principal
                  }
              )
          Left AdmissionUnavailable -> HaltEndpoint admissionUnavailableResponse
          Left AdmissionNotEstablished -> HaltEndpoint (admissionChallenge endpointRequest)

admissionChallenge :: EndpointRequest RootRoute ComposedContext RootAuthorization -> NonPageResponse RootRoute ComposedContext
admissionChallenge endpointRequest =
  case endpointDispatchKind endpointRequest of
    EndpointClientAction ->
      NonPageClientActionBodyResponse
        ClientActionResponse
          { clientActionStatus = Http.status401,
            clientActionPatches = [],
            clientActionFocusId = Nothing,
            clientActionNavigation = NavigateInternal ReplaceHistory admissionRoute,
            clientActionHeaders = [],
            clientActionObservabilityAttributes = [],
            clientActionLogEntries = []
          }
    _ -> nonPageInternalRedirectResponse Http.status303 admissionRoute
  where
    routeRequest = endpointRouteRequest endpointRequest
    admissionRoute =
      RouteRequest
        { requestRoute =
            case requestRoute routeRequest of
              Localized selectedLocale _ -> Localized selectedLocale (Public PublicAdmission),
          requestContext = requestContext routeRequest
        }

admissionUnavailableResponse :: NonPageResponse RootRoute ComposedContext
admissionUnavailableResponse =
  NonPageBodyResponse
    ResponseBody
      { responseStatus = Http.status503,
        responseContentType = "text/plain; charset=utf-8",
        responseBody = "Admission is temporarily unavailable.",
        responseObservabilityAttributes = [],
        responseLogEntries = [],
        responseDatabaseOperations = []
      }
