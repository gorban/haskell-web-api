module WebApi.AccountPages
  ( AccountAction,
    AccountActionTarget (..),
    AccountWorkflow (..),
    FormFeedback (..),
    FormStatus (..),
    FormStatusKind (..),
    LoginForm (..),
    MfaEnrollmentForm (..),
    PendingProfileForm (..),
    RegistrationForm (..),
    RegistrationValidationError (..),
    VerificationForm (..),
    accountActions,
    authorizeAccountActionCsrf,
    emptyRegistrationForm,
    initialPendingProfileForm,
    handleAccountAction,
    mfaEnrollmentFailureDiagnostics,
    pageCsrfTokenForAccountPage,
    renderLoginPage,
    renderLoginPageHtml,
    renderLoginRegion,
    renderLogoutPage,
    renderLogoutPageHtml,
    renderLogoutRegion,
    renderMfaEnrollmentPage,
    renderMfaEnrollmentPageHtml,
    renderMfaEnrollmentRegion,
    renderPendingProfileRegion,
    renderPendingProfileRegionHtml,
    renderRegistrationPage,
    renderRegistrationPageHtml,
    renderRegistrationRegion,
    renderVerificationPage,
    renderVerificationPageHtml,
    renderVerificationRegion,
  )
where

import WebApi.AccountPages.Actions
import WebApi.AccountPages.Forms
import WebApi.AccountPages.Rendering
import WebApi.AppEffect (AccountWorkflow (..))
