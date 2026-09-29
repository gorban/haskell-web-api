-- | Private account action failure and audit-diagnostic interpretation.
--
-- Decision (AHI-5-WA-MH, 2026-09-29): required and best-effort audit
-- outcomes stay with the existing application action-failure boundary. This
-- collaborator owns stable failure attributes and private log entries;
-- Harch telemetry remains outside application audit policy.
module WebApi.AccountPages.Actions.Failure
  ( profileLoadErrorType,
    profileLoadErrorDetail,
    RequiredAuditOperation (..),
    BestEffortAuditOperation (..),
    attachBestEffortAuditFailure,
    RequiredAuditFailure (..),
    throwClientActionFailure,
    throwRequiredAuditFailure,
    buildFailureDiagnostics,
    attachClientActionFailure,
    credentialStoreErrorMessage,
    accountStoreErrorDetail,
    mfaStoreErrorMessage,
    loginAttemptStoreErrorMessage,
    sessionStoreErrorMessage,
    mfaEnrollmentSessionStoreErrorMessage,
  )
where

import Data.Text (Text)
import HarchWeb qualified
import HarchWeb.Observability qualified as Observability
import WebApi.Account (AccountStoreError (..))
import WebApi.AccountPages.Actions.Types (AccountActionResponse)
import WebApi.ActivityAudit (ActivityAuditStoreError (..))
import WebApi.AppEffect
  ( AppFailure (..),
    AppM,
    FailureCode,
    FailureDiagnostics (..),
    renderFailureCode,
    throwAppFailure,
  )
import WebApi.Login (AccountCredentialStoreError (..), LoginAttemptStoreError (..))
import WebApi.Mfa (MfaStoreError (..))
import WebApi.Profile (ProfileLoadError (..))
import WebApi.Session
  ( AccountSessionStoreError (..),
    MfaEnrollmentSessionStoreError (..),
  )

profileLoadErrorType :: Text
profileLoadErrorType = "AccountStoreError"

profileLoadErrorDetail :: ProfileLoadError -> Text
profileLoadErrorDetail loadError =
  case loadError of
    ProfileAccountStoreError storeError -> accountStoreErrorDetail storeError

throwClientActionFailure :: AccountActionResponse -> FailureCode -> Text -> Text -> AppM AccountActionResponse value
throwClientActionFailure publicResponse code typeName detail =
  throwAppFailure
    AppFailure
      { appFailurePublic = publicResponse,
        appFailureDiagnostics = buildFailureDiagnostics code typeName detail
      }

-- | Decision (README ownership review, 2026-09-11): required-audit reporting
-- extends the existing application action-failure interpreter. That
-- boundary already owns private diagnostics and low-cardinality attributes;
-- a Harch telemetry API would invert ownership, while a post-commit logger
-- would weaken the atomic contract. 'RequiredAuditFailure' keeps unrelated
-- account-store failures from being mislabeled. Explicit logout remains on
-- its best-effort path.
throwRequiredAuditFailure :: AccountActionResponse -> FailureCode -> RequiredAuditOperation -> RequiredAuditFailure -> AppM AccountActionResponse value
throwRequiredAuditFailure publicResponse code operation auditFailure =
  throwClientActionFailure
    (attachRequiredAuditFailure operation auditFailure publicResponse)
    code
    "RequiredAuditFailure"
    ("required audit append failed: " <> requiredAuditFailureKind auditFailure)

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
    AccountStoreRequiredAuditUnavailable -> "required audit append failed: unavailable"
    AccountStoreRequiredAuditCapacityExceeded -> "required audit append failed: capacity-exhausted"
    AccountStoreRequiredAuditCorruptResult -> "required audit append failed: corrupt-result"

data BestEffortAuditOperation
  = LogoutAuditAppend
  | KnownAuthenticationRejectionAudit

attachBestEffortAuditFailure :: BestEffortAuditOperation -> ActivityAuditStoreError -> AccountActionResponse -> AccountActionResponse
attachBestEffortAuditFailure operation storeError response =
  response
    { HarchWeb.clientActionObservabilityAttributes =
        HarchWeb.clientActionObservabilityAttributes response
          <> [ Observability.ObservabilityAttribute (bestEffortAuditSignalName operation) (Observability.TextAttribute "true"),
               Observability.ObservabilityAttribute "account.audit.operation" (Observability.TextAttribute (bestEffortAuditOperationName operation)),
               Observability.ObservabilityAttribute "account.audit.failure-kind" (Observability.TextAttribute (auditFailureKind storeError))
             ]
          <> capacityExceededSignal storeError,
      HarchWeb.clientActionLogEntries =
        HarchWeb.clientActionLogEntries response
          <> ["[" <> bestEffortAuditLogName operation <> "] audit.operation=" <> bestEffortAuditOperationName operation <> " audit.failure-kind=" <> auditFailureKind storeError]
          <> capacityExceededLog storeError
    }

bestEffortAuditSignalName :: BestEffortAuditOperation -> Text
bestEffortAuditSignalName operation =
  case operation of
    LogoutAuditAppend -> "app.operational.signal.account.logout.audit-append-failed"
    KnownAuthenticationRejectionAudit -> "app.operational.signal.account.authentication-rejection.audit-append-failed"

bestEffortAuditLogName :: BestEffortAuditOperation -> Text
bestEffortAuditLogName operation =
  case operation of
    LogoutAuditAppend -> "account.logout.audit-append-failed"
    KnownAuthenticationRejectionAudit -> "account.authentication-rejection.audit-append-failed"

bestEffortAuditOperationName :: BestEffortAuditOperation -> Text
bestEffortAuditOperationName operation =
  case operation of
    LogoutAuditAppend -> "append"
    KnownAuthenticationRejectionAudit -> "authentication-rejection"

auditFailureKind :: ActivityAuditStoreError -> Text
auditFailureKind storeError =
  case storeError of
    ActivityAuditUnavailable -> "unavailable"
    ActivityAuditCapacityExceeded -> "capacity-exhausted"
    ActivityAuditCorruptResult -> "corrupt-result"

capacityExceededSignal :: ActivityAuditStoreError -> [Observability.ObservabilityAttribute]
capacityExceededSignal storeError =
  case storeError of
    ActivityAuditCapacityExceeded -> [Observability.ObservabilityAttribute "app.operational.signal.audit_capacity_exceeded" (Observability.TextAttribute "true")]
    ActivityAuditUnavailable -> []
    ActivityAuditCorruptResult -> []

capacityExceededLog :: ActivityAuditStoreError -> [Text]
capacityExceededLog storeError =
  case storeError of
    ActivityAuditCapacityExceeded -> ["[audit_capacity_exceeded] audit.operation=append"]
    ActivityAuditUnavailable -> []
    ActivityAuditCorruptResult -> []

data RequiredAuditOperation
  = AccountSessionIssueAudit
  | PendingRegistrationDeliveryAudit
  | VerificationResendDeliveryAudit
  | MfaEnrollmentConfirmationAudit

-- | Closed application-only classification for a required audit append. The
-- generic account lifecycle carries the matching cases in 'AccountStoreError'
-- so delivery workflows retain their provenance without textual sentinels.
data RequiredAuditFailure
  = RequiredAuditUnavailable
  | RequiredAuditCapacityExceeded
  | RequiredAuditCorruptResult

requiredAuditOperationName :: RequiredAuditOperation -> Text
requiredAuditOperationName operation =
  case operation of
    AccountSessionIssueAudit -> "account-session-issue"
    PendingRegistrationDeliveryAudit -> "pending-registration-delivery"
    VerificationResendDeliveryAudit -> "verification-resend-delivery"
    MfaEnrollmentConfirmationAudit -> "mfa-enrollment-confirmation"

requiredAuditFailureKind :: RequiredAuditFailure -> Text
requiredAuditFailureKind auditFailure =
  case auditFailure of
    RequiredAuditUnavailable -> "unavailable"
    RequiredAuditCapacityExceeded -> "capacity-exhausted"
    RequiredAuditCorruptResult -> "corrupt-result"

attachRequiredAuditFailure :: RequiredAuditOperation -> RequiredAuditFailure -> AccountActionResponse -> AccountActionResponse
attachRequiredAuditFailure operation auditFailure response =
  response
    { HarchWeb.clientActionObservabilityAttributes =
        HarchWeb.clientActionObservabilityAttributes response
          <> [ Observability.ObservabilityAttribute "app.operational.signal.account.audit.required-append-failed" (Observability.TextAttribute "true"),
               Observability.ObservabilityAttribute "account.audit.operation" (Observability.TextAttribute (requiredAuditOperationName operation)),
               Observability.ObservabilityAttribute "account.audit.failure-kind" (Observability.TextAttribute (requiredAuditFailureKind auditFailure))
             ]
          <> requiredAuditCapacityExceededSignal auditFailure,
      HarchWeb.clientActionLogEntries =
        HarchWeb.clientActionLogEntries response
          <> ["[account.audit.required-append-failed] audit.operation=" <> requiredAuditOperationName operation <> " audit.failure-kind=" <> requiredAuditFailureKind auditFailure]
          <> requiredAuditCapacityExceededLog auditFailure
    }

requiredAuditCapacityExceededSignal :: RequiredAuditFailure -> [Observability.ObservabilityAttribute]
requiredAuditCapacityExceededSignal auditFailure =
  case auditFailure of
    RequiredAuditCapacityExceeded -> [Observability.ObservabilityAttribute "app.operational.signal.audit_capacity_exceeded" (Observability.TextAttribute "true")]
    RequiredAuditUnavailable -> []
    RequiredAuditCorruptResult -> []

requiredAuditCapacityExceededLog :: RequiredAuditFailure -> [Text]
requiredAuditCapacityExceededLog auditFailure =
  case auditFailure of
    RequiredAuditCapacityExceeded -> ["[audit_capacity_exceeded] audit.operation=append"]
    RequiredAuditUnavailable -> []
    RequiredAuditCorruptResult -> []

mfaStoreErrorMessage :: MfaStoreError -> Text
mfaStoreErrorMessage storeError =
  case storeError of
    MfaStoreUnavailable detail -> detail
    MfaStoreCorruptData detail -> detail
    MfaStoreAuditAppendFailed ActivityAuditUnavailable -> "required audit append failed: unavailable"
    MfaStoreAuditAppendFailed ActivityAuditCapacityExceeded -> "required audit append failed: capacity-exhausted"
    MfaStoreAuditAppendFailed ActivityAuditCorruptResult -> "required audit append failed: corrupt-result"

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
