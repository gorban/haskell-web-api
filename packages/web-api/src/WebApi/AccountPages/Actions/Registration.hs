{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Registration and email-verification action orchestration.
--
-- The named inputs below make the two independent action boundaries explicit:
-- a registration submission may create or retry a pending registration, while
-- a verification submission may grant only the short-lived MFA-enrollment
-- capability.  Both retain the outer 'AccountActionWorkflow' rail so their
-- public response and private diagnostics stay owned by the existing action
-- interpreter.
module WebApi.AccountPages.Actions.Registration
  ( RegistrationWorkflowInput (..),
    VerificationWorkflowInput (..),
    handleRegistrationWorkflow,
    handleVerificationWorkflow,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb qualified
import HarchWeb.Account qualified as Account
import HarchWeb.Email qualified as Email
import HarchWeb.Observability qualified as Observability
import HarchWeb.Password qualified as Password
import HarchWeb.Session
  ( renderSessionCookie,
    sessionId,
  )
import HarchWeb.Time (UnixTimeNanoseconds)
import HarchWeb.Username qualified as Username
import Network.HTTP.Types qualified as Http
import WebApi.Account
  ( AccountStoreError,
    EmailVerificationEnvironment (..),
    RegistrationEnvironment (..),
    RegistrationError (..),
    RegistrationRequest (..),
    RegistrationResult (..),
    VerificationDeliveryFailure (..),
    confirmEmailVerificationAt,
    defaultPendingRegistrationStoragePolicy,
    registerAccount,
  )
import WebApi.AccountPages.Actions.Common
import WebApi.AccountPages.Actions.Contract
  ( RegistrationSubmission (..),
    VerificationSubmission (..),
  )
import WebApi.AccountPages.Forms
import WebApi.AppEffect
  ( AccountWorkflow (..),
    AppM,
    FailureCode (..),
  )
import WebApi.Localization (AppMessage (..))
import WebApi.Route (AppRequestContext (..))
import WebApi.Session (mfaEnrollmentSessionCookiePolicy)

-- | All request-owned inputs required to process a registration submission.
-- Keeping this as a record makes the action's preserved fields and parsed
-- domain values visible at the module boundary.
data RegistrationWorkflowInput = RegistrationWorkflowInput
  { registrationWorkflowRequest :: AccountActionRequest,
    registrationWorkflowSubmission :: RegistrationSubmission
  }

-- | All request-owned inputs required to consume an email-verification link.
data VerificationWorkflowInput = VerificationWorkflowInput
  { verificationWorkflowRequest :: AccountActionRequest,
    verificationWorkflowSubmission :: VerificationSubmission
  }

type ParsedRegistration = (Text, Text, Text, Text, Username.Username, Email.EmailAddress)

handleRegistrationWorkflow :: RegistrationWorkflowInput -> AccountActionWorkflow
handleRegistrationWorkflow input =
  case parseRegistrationForm actionRequest submission of
    Left response -> pure response
    Right registration -> do
      registrationResult <- registerAccountNow actionRequest registration
      let (usernameValue, emailValue, displayNameValue, _, _, _) = registration
      interpretRegistrationResult actionRequest usernameValue emailValue displayNameValue registrationResult
  where
    actionRequest = registrationWorkflowRequest input
    submission = registrationWorkflowSubmission input

-- | Registration is one application operation: it obtains one clock value
-- and invokes the account service with dependencies selected for this
-- request. This keeps the workflow from splitting that operation across
-- generic IO lifts.
registerAccountNow ::
  AccountActionRequest ->
  ParsedRegistration ->
  AppM publicFailure (Either RegistrationError RegistrationResult)
registerAccountNow actionRequest (_, _, displayNameValue, passwordValue, username, emailAddress) = do
  workflow <- accountWorkflow
  liftIO $ do
    now <- accountWorkflowClock workflow
    registerAccount
      RegistrationEnvironment
        { registrationPasswordHasher = accountWorkflowPasswordHasher workflow,
          registrationHashingPolicy = Password.defaultPasswordHashingPolicy,
          registrationPasswordWorkGate = accountWorkflowPasswordWorkGate workflow,
          registrationStoragePolicy = defaultPendingRegistrationStoragePolicy,
          registrationVerificationEnvironment =
            EmailVerificationEnvironment
              { verificationStore = accountWorkflowStore workflow,
                verificationDeliveryTimeout = accountWorkflowRegistrationDeliveryTimeout workflow,
                verificationDelivery = accountWorkflowEmailDelivery workflow,
                verificationLocale = emailLocale (requestLocale (HarchWeb.clientActionContext actionRequest)),
                verificationUrl = accountWorkflowVerificationUrl workflow (HarchWeb.clientActionContext actionRequest),
                verificationNow = now,
                verificationLifetime = emailVerificationLifetimeNanoseconds
              }
        }
      RegistrationRequest
        { registrationEmail = emailAddress,
          registrationPassword = Password.mkPassword passwordValue,
          registrationUsername = Just username,
          registrationDisplayName = nonEmptyText displayNameValue
        }

parseRegistrationForm ::
  AccountActionRequest ->
  RegistrationSubmission ->
  Either HarchWeb.ClientActionResponse ParsedRegistration
parseRegistrationForm actionRequest submission =
  let usernameValue = registrationUsernameValue submission
      emailValue = registrationEmailValue submission
      displayNameValue = registrationDisplayNameValue submission
      passwordValue = registrationPasswordValue submission
      path = HarchWeb.clientActionContext actionRequest
      form = RegistrationForm usernameValue emailValue displayNameValue
   in case (Username.mkUsername usernameValue, Email.mkEmailAddress emailValue, validPassword passwordValue) of
        (Nothing, _, _) -> Left (registrationResponse (actionLocale actionRequest) path Http.status422 (form (Just (localized actionRequest UsernameInvalid)) True) (Just "registration-username"))
        (_, Nothing, _) -> Left (registrationResponse (actionLocale actionRequest) path Http.status422 (form (Just (localized actionRequest EnterValidEmailAddress)) True) (Just "registration-email"))
        (_, _, False) -> Left (registrationResponse (actionLocale actionRequest) path Http.status422 (form (Just (localized actionRequest PasswordTooShort)) True) (Just "registration-password"))
        (Just username, Just emailAddress, True) -> Right (usernameValue, emailValue, displayNameValue, passwordValue, username, emailAddress)

interpretRegistrationResult ::
  AccountActionRequest ->
  Text ->
  Text ->
  Text ->
  Either RegistrationError RegistrationResult ->
  AccountActionWorkflow
interpretRegistrationResult actionRequest usernameValue emailValue displayNameValue = \case
  Right registrationResult -> pure (registrationResultResponse registrationResult)
  Left registrationError -> throwRegistrationFailure registrationError
  where
    path = HarchWeb.clientActionContext actionRequest
    response status message isError = registrationResponse (actionLocale actionRequest) path status (RegistrationForm usernameValue emailValue displayNameValue (Just message) isError)
    registrationSuccess stage =
      registrationLifecycleResponse
        stage
        (response Http.status202 (localized actionRequest RegistrationVerificationInbox) False Nothing)
    unavailableRegistration = response Http.status503 (localized actionRequest RegistrationUnavailable) True (Just "registration-email")
    deliveryFailureResponse = response Http.status502 (localized actionRequest VerificationDeliveryFailed) True (Just "registration-email")

    registrationResultResponse = \case
      -- A taken username is a recoverable, user-correctable input (the
      -- applicant can simply pick another one), unlike a taken email
      -- address — reporting it plainly restores the recovery path BA's
      -- own note found missing, and does not reopen the address-enumeration
      -- concern the branch below exists to close: usernames, unlike email
      -- addresses, are not privacy-sensitive to confirm as taken.
      RegistrationUsernameTaken -> response Http.status422 (localized actionRequest UsernameTaken) True (Just "registration-username")
      -- Both remaining outcomes share this exact branch (not merely the
      -- same wording) so a registered-address probe cannot be
      -- distinguished from a genuine registration by response bytes: the
      -- hedged "if that address can register" phrasing is meaningless if
      -- the other outcome answers differently.
      RegistrationCreated _ -> registrationSuccess "created"
      RegistrationRetried _ -> registrationSuccess "retried"
      RegistrationAlreadyRegistered -> registrationSuccess "already-registered"

    throwRegistrationFailure = \case
      RegistrationDeliveryFailed VerificationDeliveryTimedOut -> throwClientActionFailure deliveryFailureResponse RegistrationDeliveryTimeoutFailure "EmailDeliveryTimeout" "registration verification delivery timed out"
      RegistrationDeliveryFailed (VerificationDeliveryTransportFailed detail) -> throwClientActionFailure deliveryFailureResponse RegistrationDeliveryFailure "EmailDeliveryError" detail
      RegistrationStoreError storeError -> throwClientActionFailure unavailableRegistration RegistrationStoreFailure "AccountStoreError" (accountStoreErrorDetail storeError)
      RegistrationPasswordHashingFailed -> throwClientActionFailure unavailableRegistration RegistrationPasswordHashFailure "PasswordHashingError" "password hashing failed"
      RegistrationPasswordWorkBudgetExhausted -> throwClientActionFailure unavailableRegistration RegistrationPasswordWorkBudgetFailure "PasswordWorkBudgetExhausted" "password work budget is exhausted"
      RegistrationStorageExhausted -> throwClientActionFailure unavailableRegistration RegistrationStorageCapacityFailure "PendingRegistrationStorageExhausted" "pending registration storage is at capacity"
      RegistrationDeliveryClaimLost -> throwClientActionFailure unavailableRegistration RegistrationDeliveryClaimFailure "PendingRegistrationDeliveryClaimLost" "registration delivery claim was replaced before completion"
      RegistrationClockOverflow -> throwClientActionFailure unavailableRegistration RegistrationClockFailure "ClockOverflow" "verification expiry overflowed"

registrationLifecycleResponse :: Text -> HarchWeb.ClientActionResponse -> HarchWeb.ClientActionResponse
registrationLifecycleResponse stage response =
  response
    { HarchWeb.clientActionObservabilityAttributes =
        [Observability.ObservabilityAttribute "account.registration.stage" (Observability.TextAttribute stage)],
      HarchWeb.clientActionLogEntries = ["INFO [account.registration] stage=" <> stage]
    }

handleVerificationWorkflow :: VerificationWorkflowInput -> AccountActionWorkflow
handleVerificationWorkflow input =
  let tokenValue = verificationTokenValue submission
      path = HarchWeb.clientActionContext actionRequest
   in case Account.mkEmailVerificationToken tokenValue of
        Nothing -> pure (verificationResponse (actionLocale actionRequest) path Http.status422 (VerificationForm tokenValue (Just (localized actionRequest VerificationLinkInvalid)) True) (Just "verification-token") [])
        Just token -> do
          (now, confirmationResult) <- confirmEmailVerificationNow token
          case confirmationResult of
            Right (Account.EmailVerificationAccepted accountId _) -> issueVerificationEnrollmentSession actionRequest now accountId
            Right Account.EmailVerificationExpired -> pure (verificationResponse (actionLocale actionRequest) path Http.status422 (VerificationForm tokenValue (Just (localized actionRequest VerificationLinkExpired)) True) (Just "verification-token") [])
            Right Account.EmailVerificationRejected -> pure (verificationResponse (actionLocale actionRequest) path Http.status422 (VerificationForm tokenValue (Just (localized actionRequest VerificationLinkUsed)) True) (Just "verification-token") [])
            Left storeError -> throwClientActionFailure (verificationResponse (actionLocale actionRequest) path Http.status503 (VerificationForm tokenValue (Just (localized actionRequest VerificationUnavailable)) True) (Just "verification-token") []) VerificationStoreFailure "AccountStoreError" (accountStoreErrorDetail storeError)
  where
    actionRequest = verificationWorkflowRequest input
    submission = verificationWorkflowSubmission input

-- | Verification confirmation reads the clock and store as one operation so
-- the accepted result and subsequent enrollment session share one time.
confirmEmailVerificationNow :: Account.EmailVerificationToken -> AppM publicFailure (UnixTimeNanoseconds, Either AccountStoreError Account.EmailVerificationValidation)
confirmEmailVerificationNow token = do
  workflow <- accountWorkflow
  liftIO $ do
    now <- accountWorkflowClock workflow
    confirmationResult <- confirmEmailVerificationAt (accountWorkflowStore workflow) now token
    pure (now, confirmationResult)

-- | Email verification just proved ownership of the account, so this is the
-- one legitimate place to grant enrollment access — see the AM decision
-- record on 'WebApi.AccountPages.Actions.Workflows.handleMfaEnrollmentSubmission'
-- for why that access is a distinct, short-lived session rather than the
-- ordinary login session or a client-supplied account id.
issueVerificationEnrollmentSession :: AccountActionRequest -> UnixTimeNanoseconds -> Account.AccountId -> AccountActionWorkflow
issueVerificationEnrollmentSession actionRequest now accountId = do
  let path = HarchWeb.clientActionContext actionRequest
      successResponse = verificationResponse (actionLocale actionRequest) path Http.status200 (VerificationForm Text.empty (Just (localized actionRequest EmailVerifiedEnrollAuthenticator)) False) Nothing
  issued <- issueMfaEnrollmentSessionNow accountId now
  case issued of
    Right opaqueSession -> pure (successResponse [("Set-Cookie", TextEncoding.encodeUtf8 (renderSessionCookie mfaEnrollmentSessionCookiePolicy (sessionId opaqueSession)))])
    Left storeError -> throwClientActionFailure (successResponse []) MfaEnrollmentSessionFailure "MfaEnrollmentSessionStoreError" (mfaEnrollmentSessionStoreErrorMessage storeError)
