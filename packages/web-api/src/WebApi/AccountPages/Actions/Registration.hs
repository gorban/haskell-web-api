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
import Data.List.NonEmpty (NonEmpty (..))
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
    VerificationDeliveryEnvironment (..),
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
import WebApi.AccountPages.FieldIds
  ( registrationEmailId,
    registrationPasswordId,
    registrationSummaryId,
    registrationUsernameId,
    verificationTokenId,
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

data RegistrationValidation value
  = RegistrationValidationFailure (NonEmpty RegistrationValidationError)
  | RegistrationValidationSuccess value

-- Registration has one local independent-input validation workflow rather
-- than a stable general-purpose capability stack.  These named pure
-- combinators retain applicative accumulation semantics while keeping that
-- narrow ownership visible: every rejected field is collected, and effects
-- begin only after all inputs have succeeded.
mapRegistrationValidation :: (input -> output) -> RegistrationValidation input -> RegistrationValidation output
mapRegistrationValidation transform validation =
  case validation of
    RegistrationValidationFailure validationErrors -> RegistrationValidationFailure validationErrors
    RegistrationValidationSuccess value -> RegistrationValidationSuccess (transform value)

applyRegistrationValidation :: RegistrationValidation (input -> output) -> RegistrationValidation input -> RegistrationValidation output
applyRegistrationValidation functionValidation valueValidation =
  case (functionValidation, valueValidation) of
    (RegistrationValidationSuccess transform, RegistrationValidationSuccess value) -> RegistrationValidationSuccess (transform value)
    (RegistrationValidationFailure leftErrors, RegistrationValidationFailure rightErrors) -> RegistrationValidationFailure (leftErrors <> rightErrors)
    (RegistrationValidationFailure validationErrors, RegistrationValidationSuccess _) -> RegistrationValidationFailure validationErrors
    (RegistrationValidationSuccess _, RegistrationValidationFailure validationErrors) -> RegistrationValidationFailure validationErrors

succeedRegistrationValidation :: value -> RegistrationValidation value
succeedRegistrationValidation = RegistrationValidationSuccess

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
      form = RegistrationForm usernameValue emailValue displayNameValue
      parsed =
        applyRegistrationValidation
          ( applyRegistrationValidation
              ( applyRegistrationValidation
                  (mapRegistrationValidation (,,,) (requireValue RegistrationUsernameInvalid (Username.mkUsername usernameValue)))
                  (requireValue RegistrationEmailInvalid (Email.mkEmailAddress emailValue))
              )
              (requirePassword passwordValue)
          )
          (succeedRegistrationValidation displayNameValue)
   in case parsed of
        RegistrationValidationFailure errors ->
          Left
            ( registrationResponse
                (accountActionResponseContext actionRequest Http.status422 (Just (registrationErrorFocus errors)) [])
                (form (FormRejected errors))
            )
        RegistrationValidationSuccess (username, emailAddress, password, displayName) -> Right (usernameValue, emailValue, displayName, password, username, emailAddress)

requireValue :: RegistrationValidationError -> Maybe value -> RegistrationValidation value
requireValue validationError = maybe (RegistrationValidationFailure (validationError :| [])) RegistrationValidationSuccess

requirePassword :: Text -> RegistrationValidation Text
requirePassword password =
  case validPassword password of
    True -> RegistrationValidationSuccess password
    False -> RegistrationValidationFailure (RegistrationPasswordTooShort :| [])

registrationErrorFocus :: NonEmpty RegistrationValidationError -> HarchWeb.ElementId
registrationErrorFocus (registrationError :| []) = registrationErrorControlId registrationError
registrationErrorFocus (_ :| _ : _) = registrationSummaryId

registrationErrorControlId :: RegistrationValidationError -> HarchWeb.ElementId
registrationErrorControlId validationError =
  case validationError of
    RegistrationUsernameInvalid -> registrationUsernameId
    RegistrationEmailInvalid -> registrationEmailId
    RegistrationPasswordTooShort -> registrationPasswordId
    RegistrationUsernameUnavailable -> registrationUsernameId

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
    response status feedback focusId =
      registrationResponse
        (accountActionResponseContext actionRequest status focusId [])
        (RegistrationForm usernameValue emailValue displayNameValue feedback)
    statusFeedback message kind = FormStatusMessage (FormStatus message kind)
    registrationSuccess stage =
      registrationLifecycleResponse
        stage
        (response Http.status202 (statusFeedback (localized actionRequest RegistrationVerificationInbox) FormStatusSuccess) Nothing)
    unavailableRegistration = response Http.status503 (statusFeedback (localized actionRequest RegistrationUnavailable) FormStatusFailure) (Just registrationEmailId)
    deliveryFailureResponse = response Http.status502 (statusFeedback (localized actionRequest VerificationDeliveryFailed) FormStatusFailure) (Just registrationEmailId)

    registrationResultResponse = \case
      -- A taken username is a recoverable, user-correctable input (the
      -- applicant can simply pick another one), unlike a taken email
      -- address — reporting it plainly restores the recovery path BA's
      -- own note found missing, and does not reopen the address-enumeration
      -- concern the branch below exists to close: usernames, unlike email
      -- addresses, are not privacy-sensitive to confirm as taken.
      RegistrationUsernameTaken ->
        response
          Http.status422
          (FormRejected (RegistrationUsernameUnavailable :| []))
          (Just (registrationErrorControlId RegistrationUsernameUnavailable))
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
      RegistrationDeliveryFailed VerificationDeliveryTransportFailed -> throwClientActionFailure deliveryFailureResponse RegistrationDeliveryFailure "EmailDeliveryError" "registration verification delivery transport failed"
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
   in case Account.mkEmailVerificationToken tokenValue of
        Nothing -> pure (verificationResponse (accountActionResponseContext actionRequest Http.status422 (Just verificationTokenId) []) (VerificationForm tokenValue (Just (localized actionRequest VerificationLinkInvalid)) True))
        Just token -> do
          (now, confirmationResult) <- confirmEmailVerificationNow token
          case confirmationResult of
            Right (Account.EmailVerificationAccepted accountId _) -> issueVerificationEnrollmentSession actionRequest now accountId
            Right Account.EmailVerificationExpired -> pure (verificationResponse (accountActionResponseContext actionRequest Http.status422 (Just verificationTokenId) []) (VerificationForm tokenValue (Just (localized actionRequest VerificationLinkExpired)) True))
            Right Account.EmailVerificationRejected -> pure (verificationResponse (accountActionResponseContext actionRequest Http.status422 (Just verificationTokenId) []) (VerificationForm tokenValue (Just (localized actionRequest VerificationLinkUsed)) True))
            Left storeError -> throwClientActionFailure (verificationResponse (accountActionResponseContext actionRequest Http.status503 (Just verificationTokenId) []) (VerificationForm tokenValue (Just (localized actionRequest VerificationUnavailable)) True)) VerificationStoreFailure "AccountStoreError" (accountStoreErrorDetail storeError)
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
  let successResponse headers = verificationResponse (accountActionResponseContext actionRequest Http.status200 Nothing headers) (VerificationForm Text.empty (Just (localized actionRequest EmailVerifiedEnrollAuthenticator)) False)
  issued <- issueMfaEnrollmentSessionNow accountId now
  case issued of
    Right opaqueSession -> pure (successResponse [("Set-Cookie", TextEncoding.encodeUtf8 (renderSessionCookie mfaEnrollmentSessionCookiePolicy (sessionId opaqueSession)))])
    Left storeError -> throwClientActionFailure (successResponse []) MfaEnrollmentSessionFailure "MfaEnrollmentSessionStoreError" (mfaEnrollmentSessionStoreErrorMessage storeError)
