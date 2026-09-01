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
import HarchWeb.Session (SessionId)
import HarchWeb.Time (UnixTimeNanoseconds)
import Network.HTTP.Types qualified as Http
import WebApi.Account
  ( AccountProfile (..),
    EmailVerificationEnvironment (..),
    ResendVerificationError (..),
    ResendVerificationResult,
    VerificationDeliveryEnvironment (..),
    resendEmailVerificationAt,
  )
import WebApi.AccountPages.Actions.Common
import WebApi.AccountPages.Actions.Contract (ProfileSubmission (..))
import WebApi.AccountPages.Forms (PendingProfileForm (..))
import WebApi.AppEffect
  ( AccountWorkflow (..),
    AppM,
    FailureCode (..),
  )
import WebApi.Localization (AppMessage (..))
import WebApi.Profile
  ( ProfileLoadError,
    ProfileState (..),
    loadProfile,
  )
import WebApi.Route (AppRequestContext (..))

-- | All request-owned inputs required to process a profile submission.
data ProfileWorkflowInput = ProfileWorkflowInput
  { profileWorkflowRequest :: AccountActionRequest,
    profileWorkflowSubmission :: ProfileSubmission
  }

handleProfileWorkflow :: ProfileWorkflowInput -> AccountActionWorkflow
handleProfileWorkflow input = do
  (now, loadedProfile) <- loadProfileNow (requestSessionId (HarchWeb.clientActionContext actionRequest))
  case loadedProfile of
    Left loadError -> throwClientActionFailure (profileResponse actionRequest Http.status503 (PendingProfileForm mempty (Just (localized actionRequest ProfileUnavailable)) True (resendLabel actionRequest))) ProfileLoadFailure (profileLoadErrorType loadError) (profileLoadErrorDetail loadError)
    Right ProfileUnauthenticated -> pure (profileResponse actionRequest Http.status403 (PendingProfileForm mempty (Just (localized actionRequest SignInBeforeResend)) True (resendLabel actionRequest)))
    Right (ProfileAuthenticated profile) -> pure (profileResponse actionRequest Http.status409 (PendingProfileForm (Email.emailAddressText (accountProfileEmail profile)) (Just (localized actionRequest EmailAlreadyVerified)) True (resendLabel actionRequest)))
    Right (ProfilePending profile) -> handlePendingProfile actionRequest submission now profile
  where
    actionRequest = profileWorkflowRequest input
    submission = profileWorkflowSubmission input

loadProfileNow :: Maybe SessionId -> AppM publicFailure (UnixTimeNanoseconds, Either ProfileLoadError ProfileState)
loadProfileNow maybeSessionId = do
  workflow <- accountWorkflow
  liftIO $ do
    now <- accountWorkflowClock workflow
    loadedProfile <- loadProfile (accountWorkflowSessionStore workflow) (accountWorkflowProfileStore workflow) now maybeSessionId
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
        { verificationStore = accountWorkflowStore workflow,
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
