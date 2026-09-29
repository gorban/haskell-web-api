-- | Public re-export boundary for shared account action support.
--
-- Decision (AHI-5-WA-MH, 2026-09-29): retain this existing exposed module
-- and its export list because it is the package's current account-action
-- support API. Its effect helpers, failure/audit interpretation, and
-- region-patch response construction now live with their respective owners
-- in private collaborators. This preserves caller imports without making a
-- second action dispatcher or keeping implementation responsibilities
-- coupled to reduce a metric.
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

import WebApi.AccountPages.Actions.Failure
  ( BestEffortAuditOperation (..),
    RequiredAuditFailure (..),
    RequiredAuditOperation (..),
    accountStoreErrorDetail,
    attachBestEffortAuditFailure,
    attachClientActionFailure,
    buildFailureDiagnostics,
    credentialStoreErrorMessage,
    loginAttemptStoreErrorMessage,
    mfaEnrollmentSessionStoreErrorMessage,
    mfaStoreErrorMessage,
    profileLoadErrorDetail,
    profileLoadErrorType,
    sessionStoreErrorMessage,
    throwClientActionFailure,
    throwRequiredAuditFailure,
  )
import WebApi.AccountPages.Actions.Response
  ( AccountActionResponseContext,
    accountActionResponseContext,
    loginResponse,
    logoutResponse,
    mfaEnrollmentResponse,
    noHeaders,
    profileResponse,
    registrationResponse,
    verificationResponse,
  )
import WebApi.AccountPages.Actions.Support
  ( accountWorkflow,
    actionLocale,
    emailLocale,
    emailVerificationLifetimeNanoseconds,
    issueMfaEnrollmentSessionNow,
    localized,
    mfaErrorMessage,
    nonEmptyText,
    pendingProfileForm,
    resendLabel,
    validPassword,
  )
import WebApi.AccountPages.Actions.Types
  ( AccountActionRequest,
    AccountActionResponse,
    AccountActionWorkflow,
  )
