module WebApi.AccountPages
  ( AccountAction,
    AccountWorkflow (..),
    LoginForm (..),
    MfaEnrollmentForm (..),
    PendingProfileForm (..),
    RegistrationForm (..),
    VerificationForm (..),
    decodeAccountAction,
    emptyRegistrationForm,
    handleAccountAction,
    mfaEnrollmentFailureDiagnostics,
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
