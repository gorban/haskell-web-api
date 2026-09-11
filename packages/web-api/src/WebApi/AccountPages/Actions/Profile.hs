{-# LANGUAGE OverloadedStrings #-}

-- | Pending-profile verification-resend action orchestration.
--
-- The named input makes it clear that the request context and submitted
-- profile intent remain on the action boundary; the module returns through
-- the existing 'AccountActionWorkflow' rail and never creates a second
-- response interpreter.
module WebApi.AccountPages.Actions.Profile
  ( ProfileWorkflowInput (..),
    handleProfileWorkflow,
  )
where

import Control.Monad.IO.Class (liftIO)
import HarchWeb qualified
import HarchWeb.Email qualified as Email
import HarchWeb.Time (UnixTimeNanoseconds)
import Network.HTTP.Types qualified as Http
import WebApi.Account
  ( AccountProfile (..),
    AccountStore (..),
    AccountStoreError (..),
    EmailVerificationEnvironment (..),
    ResendVerificationError (..),
    ResendVerificationResult,
    VerificationDeliveryEnvironment (..),
    VerificationResendClaim (..),
    resendEmailVerificationAt,
  )
import WebApi.AccountPages.Actions.Common
import WebApi.AccountPages.Actions.Contract (ProfileSubmission (..))
import WebApi.AccountPages.Forms (PendingProfileForm (..))
import WebApi.AccountPrincipal (AccountPrincipal)
import WebApi.ActivityAudit
  ( AccountActivity (..),
    AccountAuditEvent (VerificationResendDelivered),
    auditRouteObservationFromTrusted,
  )
import WebApi.AppEffect
  ( AccountWorkflow (..),
    AppM,
    FailureCode (..),
  )
import WebApi.Localization (AppMessage (..))
import WebApi.Profile
  ( ProfileLoadError,
    ProfileState (..),
    loadProfileForPrincipal,
  )
import WebApi.Route (AppRequestContext (..))
import WebApi.VerificationResendAudit
  ( VerificationResendAuditStore (..),
    VerificationResendAuditStoreError (..),
  )

-- | All request-owned inputs required to process a profile submission.
data ProfileWorkflowInput = ProfileWorkflowInput
  { profileWorkflowRequest :: AccountActionRequest,
    profileWorkflowSubmission :: ProfileSubmission
  }

handleProfileWorkflow :: ProfileWorkflowInput -> AccountActionWorkflow
handleProfileWorkflow input = do
  (now, loadedProfile) <- loadProfileNow (requestAccountPrincipal (HarchWeb.clientActionContext actionRequest))
  case loadedProfile of
    Left loadError -> throwClientActionFailure (profileResponse actionRequest Http.status503 (PendingProfileForm mempty (Just (localized actionRequest ProfileUnavailable)) True (resendLabel actionRequest))) ProfileLoadFailure profileLoadErrorType (profileLoadErrorDetail loadError)
    Right ProfileUnauthenticated -> pure (profileResponse actionRequest Http.status403 (PendingProfileForm mempty (Just (localized actionRequest SignInBeforeResend)) True (resendLabel actionRequest)))
    Right (ProfileAuthenticated profile) -> pure (profileResponse actionRequest Http.status409 (PendingProfileForm (Email.emailAddressText (accountProfileEmail profile)) (Just (localized actionRequest EmailAlreadyVerified)) True (resendLabel actionRequest)))
    Right (ProfilePending profile) -> handlePendingProfile actionRequest submission now profile
  where
    actionRequest = profileWorkflowRequest input
    submission = profileWorkflowSubmission input

loadProfileNow :: Maybe AccountPrincipal -> AppM publicFailure (UnixTimeNanoseconds, Either ProfileLoadError ProfileState)
loadProfileNow maybePrincipal = do
  workflow <- accountWorkflow
  liftIO $ do
    now <- accountWorkflowClock workflow
    loadedProfile <- loadProfileForPrincipal (accountWorkflowProfileStore workflow) maybePrincipal
    pure (now, loadedProfile)

handlePendingProfile ::
  AccountActionRequest ->
  ProfileSubmission ->
  UnixTimeNanoseconds ->
  AccountProfile ->
  AccountActionWorkflow
handlePendingProfile actionRequest submission now profile =
  case profileIntentValue submission of
    "resend-verification" -> do
      resendResult <- resendEmailVerificationNow actionRequest now profile
      interpretProfileResendResult actionRequest profile resendResult
    _ -> pure (profileResponse actionRequest Http.status422 (pendingProfileForm actionRequest profile (Just (localized actionRequest ChooseProfileAction)) True))

resendEmailVerificationNow :: AccountActionRequest -> UnixTimeNanoseconds -> AccountProfile -> AppM publicFailure (Either ResendVerificationError ResendVerificationResult)
resendEmailVerificationNow actionRequest now profile@AccountProfile {} = do
  workflow <- accountWorkflow
  liftIO $
    resendEmailVerificationAt
      EmailVerificationEnvironment
        { verificationStore = verificationResendAuditStore actionRequest workflow,
          verificationDeliveryEnvironment =
            VerificationDeliveryEnvironment
              { verificationDeliveryTimeout = accountWorkflowRegistrationDeliveryTimeout workflow,
                verificationDelivery = accountWorkflowEmailDelivery workflow,
                verificationLocale = emailLocale (requestLocale (HarchWeb.clientActionContext actionRequest)),
                verificationUrl = accountWorkflowVerificationUrl workflow (HarchWeb.clientActionContext actionRequest)
              },
          verificationNow = now,
          verificationLifetime = emailVerificationLifetimeNanoseconds
        }
      profile

-- | Keep the storage-neutral resend workflow as the owner of reservation,
-- release, and lost-claim semantics.  Only its successful post-SMTP
-- promotion is replaced here, where trusted request attribution can form the
-- required closed operator activity.  A typed audit failure maps to the
-- workflow's existing unavailable store rail, leaving the durable claim for
-- a later retry instead of completing it without the event.
verificationResendAuditStore :: AccountActionRequest -> AccountWorkflow -> AccountStore
verificationResendAuditStore actionRequest workflow =
  (accountWorkflowStore workflow)
    { completeVerificationResend = completeWithAudit
    }
  where
    completeWithAudit claim now =
      case verificationResendDeliveryActivity actionRequest claim of
        Left activityError -> pure (Left (verificationResendAuditAsAccountStoreError activityError))
        Right activity -> do
          settled <- completeVerificationResendWithAudit (accountWorkflowVerificationResendAuditStore workflow) claim now activity
          pure (either (Left . verificationResendAuditAsAccountStoreError) Right settled)

verificationResendDeliveryActivity :: AccountActionRequest -> VerificationResendClaim -> Either VerificationResendAuditStoreError AccountActivity
verificationResendDeliveryActivity actionRequest claim = do
  requestId <- maybe (Left VerificationResendAuditStoreCorruptData) Right (requestCorrelationId context)
  route <- traverse (either (const (Left VerificationResendAuditStoreCorruptData)) Right . auditRouteObservationFromTrusted) (requestRouteObservation context)
  pure
    AccountActivity
      { activitySubject = verificationResendClaimAccountId claim,
        activityRequestId = requestId,
        activityEvent = VerificationResendDelivered,
        activityRoute = route
      }
  where
    context = HarchWeb.clientActionContext actionRequest

verificationResendAuditAsAccountStoreError :: VerificationResendAuditStoreError -> AccountStoreError
verificationResendAuditAsAccountStoreError auditError =
  case auditError of
    VerificationResendAuditStoreUnavailable -> AccountStoreUnavailable "verification resend audit store unavailable"
    VerificationResendAuditCapacityExceeded -> AccountStoreUnavailable "account audit partition capacity is exhausted"
    VerificationResendAuditStoreCorruptData -> AccountStoreCorruptData "verification resend audit store returned corrupt data"

interpretProfileResendResult ::
  AccountActionRequest ->
  AccountProfile ->
  Either ResendVerificationError ResendVerificationResult ->
  AccountActionWorkflow
interpretProfileResendResult actionRequest profile resendResult =
  let form message = pendingProfileForm actionRequest profile (Just message)
   in case resendResult of
        Right _ -> pure (profileResponse actionRequest Http.status202 (form (localized actionRequest CheckVerificationInbox) False))
        Left (ResendVerificationDeliveryFailed _) -> throwClientActionFailure (profileResponse actionRequest Http.status502 (form (localized actionRequest VerificationDeliveryFailed) True)) ProfileResendDeliveryFailure "EmailDeliveryError" "verification delivery failed"
        Left (ResendVerificationStoreError storeError) -> throwClientActionFailure (profileResponse actionRequest Http.status503 (form (localized actionRequest ProfileUnavailable) True)) ProfileResendStoreFailure "AccountStoreError" (accountStoreErrorDetail storeError)
        Left ResendVerificationClockOverflow -> throwClientActionFailure (profileResponse actionRequest Http.status503 (form (localized actionRequest ProfileUnavailable) True)) ProfileResendClockFailure "ClockOverflow" "verification expiry overflowed"
