{-# LANGUAGE OverloadedStrings #-}

-- | Account registration and verification workflows.
--
-- Decision (FQ6, 2026-08-29): verification persistence/expiry stays in
-- 'EmailVerificationEnvironment', while mail transport, locale, URL, and
-- timeout form 'VerificationDeliveryEnvironment'. Registration and resend
-- therefore share one explicit delivery capability without conflating it
-- with credential hashing or account storage.
--
-- Decision (AHI-2, 2026-09-01): resend is a staged claim owned by
-- 'AccountStore'.  The existing delivered verification remains usable until
-- SMTP succeeds and the store atomically promotes the candidate token.  This
-- is deliberately a narrow lifecycle capability, rather than a generic
-- budget abstraction: account verification owns the pending-state check, its
-- token, and the bounded delivery history.  Claim values contain only an
-- account id and digest, never a row id or raw token.
module WebApi.Account
  ( AccountStore (..),
    AccountStoreError (..),
    AccountProfile (..),
    AccountProfileStore (..),
    CreatePendingAccountOutcome (..),
    PendingRegistrationClaim (..),
    PendingRegistrationDeliveryStage (..),
    PendingRegistrationStoragePolicy,
    VerificationResendPolicy,
    RegistrationDeliveryTimeout,
    PendingAccount (..),
    EmailVerificationEnvironment (..),
    VerificationDeliveryEnvironment (..),
    RegistrationEnvironment (..),
    RegistrationRequest (..),
    ResendVerificationError (..),
    ResendVerificationResult (..),
    VerificationResendAdmission (..),
    VerificationResendClaim (..),
    VerificationResendClaimSettlement (..),
    VerificationResendSuppression (..),
    RegistrationError (..),
    RegistrationResult (..),
    VerificationDeliveryFailure (..),
    confirmEmailVerificationAt,
    defaultPendingRegistrationStoragePolicy,
    defaultRegistrationDeliveryTimeout,
    defaultVerificationResendPolicy,
    mkPendingRegistrationStoragePolicy,
    mkRegistrationDeliveryTimeout,
    mkVerificationResendPolicy,
    pendingRegistrationClaimLeaseNanoseconds,
    pendingRegistrationMaximumAccounts,
    verificationResendClaimLeaseNanoseconds,
    verificationResendMaximumDeliveries,
    verificationResendMaximumRecords,
    verificationResendWindowNanoseconds,
    registerAccount,
    resendEmailVerificationAt,
  )
where

import Control.Exception (AsyncException, SomeException, fromException, mask, onException, throwIO, try)
import Control.Monad (void)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Core.Control.Error (fromMaybeError, guardError, liftEitherWith, liftMaybeWith)
import Data.Text (Text)
import Data.Word (Word64)
import HarchWeb.Account
  ( AccountId,
    EmailVerificationToken,
    EmailVerificationTokenDigest,
    EmailVerificationValidation (..),
    StoredEmailVerification,
    emailVerificationTokenDigest,
    generateAccountId,
    generateEmailVerificationToken,
    mkStoredEmailVerification,
    storedVerificationTokenDigest,
    validateEmailVerificationToken,
  )
import HarchWeb.Email
  ( EmailAddress,
    EmailDelivery (..),
    EmailLocale,
    verificationEmail,
  )
import HarchWeb.Password
  ( Password,
    PasswordHash,
    PasswordHashingPolicy,
    PasswordWorkGate,
    passwordHashMemoryKibibytes,
    withPasswordWork,
  )
import HarchWeb.Time (UnixTimeNanoseconds, addUnixTimeNanoseconds)
import HarchWeb.Username (Username)
import System.Timeout (timeout)

data AccountStoreError
  = AccountStoreUnavailable Text
  | AccountStoreCorruptData Text
  deriving (Eq, Show)

-- | The safe account data required by authenticated page surfaces. Password
-- hashes, verification tokens, and other credentials never cross this seam.
data AccountProfile = AccountProfile
  { accountProfileId :: AccountId,
    accountProfileEmail :: EmailAddress,
    accountProfileUsername :: Maybe Username,
    accountProfileDisplayName :: Maybe Text,
    accountProfileEmailVerified :: Bool
  }

newtype AccountProfileStore = AccountProfileStore
  { findAccountProfile :: AccountId -> IO (Either AccountStoreError (Maybe AccountProfile))
  }

data PendingAccount = PendingAccount
  { pendingAccountId :: AccountId,
    pendingAccountEmail :: EmailAddress,
    pendingAccountUsername :: Maybe Username,
    pendingAccountDisplayName :: Maybe Text,
    pendingAccountPasswordHash :: PasswordHash,
    pendingAccountVerification :: StoredEmailVerification,
    pendingAccountCreatedAtNanoseconds :: UnixTimeNanoseconds
  }

data AccountStore = AccountStore
  { createPendingAccount :: PendingRegistrationStoragePolicy -> PendingAccount -> IO (Either AccountStoreError CreatePendingAccountOutcome),
    completePendingRegistrationDelivery :: PendingRegistrationClaim -> IO (Either AccountStoreError Bool),
    releasePendingRegistrationDelivery :: PendingRegistrationClaim -> IO (Either AccountStoreError Bool),
    reserveVerificationResend :: VerificationResendPolicy -> StoredEmailVerification -> UnixTimeNanoseconds -> IO (Either AccountStoreError VerificationResendAdmission),
    completeVerificationResend :: VerificationResendClaim -> UnixTimeNanoseconds -> IO (Either AccountStoreError VerificationResendClaimSettlement),
    releaseVerificationResend :: VerificationResendClaim -> IO (Either AccountStoreError VerificationResendClaimSettlement),
    replaceEmailVerification :: StoredEmailVerification -> IO (Either AccountStoreError Bool),
    findEmailVerification :: EmailVerificationTokenDigest -> IO (Either AccountStoreError (Maybe StoredEmailVerification)),
    consumeEmailVerification :: EmailVerificationTokenDigest -> UnixTimeNanoseconds -> IO (Either AccountStoreError (Maybe AccountId))
  }

-- | Which of the two independent unique constraints a pending-account
-- insert can collide with. Kept distinct from a plain 'Bool' so a taken
-- username (a recoverable, user-correctable input) is never conflated
-- with a taken email address (deliberately reported identically to a
-- successful registration — see BA's decision record).
data CreatePendingAccountOutcome
  = PendingAccountCreated
  | PendingAccountDeliveryClaimed PendingRegistrationClaim
  | PendingAccountEmailTaken
  | PendingAccountUsernameTaken
  | PendingAccountStorageExhausted

-- | The durable identity of one currently claimed registration delivery.  It
-- contains only the account id and token digest, never the raw token; the
-- caller retains the latter only long enough to hand it to the mail transport.
data PendingRegistrationClaim = PendingRegistrationClaim
  { pendingRegistrationClaimAccountId :: AccountId,
    pendingRegistrationClaimTokenDigest :: EmailVerificationTokenDigest,
    pendingRegistrationClaimStage :: PendingRegistrationDeliveryStage
  }

-- | The only observable lifecycle stages for a registration delivery.  These
-- stable values are safe for telemetry because they never identify an email,
-- account, or verification token.
data PendingRegistrationDeliveryStage
  = PendingRegistrationCreated
  | PendingRegistrationRetried

-- | Application-owned bounds for unauthenticated pending registrations.  A
-- positive capacity prevents attacker-chosen addresses from owning unlimited
-- rows; a positive claim lease makes a process that stops during SMTP delivery
-- recoverable by a later identical registration.  Expired verification rows
-- are removed during the same staging transaction.
data PendingRegistrationStoragePolicy = PendingRegistrationStoragePolicy
  { pendingRegistrationMaximumAccounts :: Word64,
    pendingRegistrationClaimLeaseNanoseconds :: Word64
  }

mkPendingRegistrationStoragePolicy :: Word64 -> Word64 -> Maybe PendingRegistrationStoragePolicy
mkPendingRegistrationStoragePolicy maximumAccounts claimLeaseNanoseconds =
  case (maximumAccounts, claimLeaseNanoseconds) of
    (0, _) -> Nothing
    (_, 0) -> Nothing
    _ -> Just (PendingRegistrationStoragePolicy maximumAccounts claimLeaseNanoseconds)

-- | Reference-application policy: a maximum of 100,000 unverified accounts
-- and a five-minute abandoned-delivery lease.  Expired verification tokens
-- are the retention boundary, so the staging transaction removes their
-- unverified accounts before it evaluates this capacity.
defaultPendingRegistrationStoragePolicy :: PendingRegistrationStoragePolicy
defaultPendingRegistrationStoragePolicy = PendingRegistrationStoragePolicy 100000 (5 * 60 * 1000000000)

-- | Bounded resend policy.  Delivery records use a rolling window while a
-- short claim lease makes a cancelled or failed process recoverable.  The
-- record limit applies to both claims and delivery history, preventing a
-- wide account-id keyspace from becoming unbounded storage.
data VerificationResendPolicy = VerificationResendPolicy
  { verificationResendMaximumDeliveries :: Word64,
    verificationResendWindowNanoseconds :: Word64,
    verificationResendClaimLeaseNanoseconds :: Word64,
    verificationResendMaximumRecords :: Word64
  }

mkVerificationResendPolicy :: Word64 -> Word64 -> Word64 -> Word64 -> Maybe VerificationResendPolicy
mkVerificationResendPolicy maximumDeliveries windowNanoseconds claimLeaseNanoseconds maximumRecords =
  case (maximumDeliveries, windowNanoseconds, claimLeaseNanoseconds, maximumRecords) of
    (0, _, _, _) -> Nothing
    (_, 0, _, _) -> Nothing
    (_, _, 0, _) -> Nothing
    (_, _, _, 0) -> Nothing
    _ -> Just (VerificationResendPolicy maximumDeliveries windowNanoseconds claimLeaseNanoseconds maximumRecords)

-- | Reference policy: at most three accepted resend deliveries per hour, a
-- five-minute abandoned-claim lease, and 100,000 retained lifecycle rows.
defaultVerificationResendPolicy :: VerificationResendPolicy
defaultVerificationResendPolicy = VerificationResendPolicy 3 (60 * 60 * 1000000000) (5 * 60 * 1000000000) 100000

-- | A positive, account-workflow-owned deadline for one SMTP delivery.  The
-- SMTP transport can otherwise wait indefinitely for DNS, connect, or a peer
-- response, retaining a registration delivery claim forever instead of
-- returning the recoverable failure its caller expects.
newtype RegistrationDeliveryTimeout = RegistrationDeliveryTimeout Int

mkRegistrationDeliveryTimeout :: Int -> Maybe RegistrationDeliveryTimeout
mkRegistrationDeliveryTimeout microseconds =
  case compare microseconds 0 of
    GT -> Just (RegistrationDeliveryTimeout microseconds)
    EQ -> Nothing
    LT -> Nothing

defaultRegistrationDeliveryTimeout :: RegistrationDeliveryTimeout
defaultRegistrationDeliveryTimeout = RegistrationDeliveryTimeout (10 * 1000000)

data RegistrationError
  = RegistrationStoreError AccountStoreError
  | RegistrationPasswordHashingFailed
  | RegistrationPasswordWorkBudgetExhausted
  | RegistrationStorageExhausted
  | RegistrationDeliveryClaimLost
  | RegistrationDeliveryFailed VerificationDeliveryFailure
  | RegistrationClockOverflow

data RegistrationResult
  = RegistrationCreated AccountId
  | RegistrationRetried AccountId
  | RegistrationAlreadyRegistered
  | RegistrationUsernameTaken

data ResendVerificationError
  = ResendVerificationStoreError AccountStoreError
  | ResendVerificationDeliveryFailed VerificationDeliveryFailure
  | ResendVerificationClockOverflow

-- | The workflow's expected, non-public result states.  Page actions map
-- delivery and suppression to the same generic acceptance response, while
-- retaining this closed classification for private low-cardinality telemetry.
-- 'ResendVerificationError' deliberately has no 'Show' instance: an
-- application boundary must select its safe classification instead of
-- rendering an operational error value.
data ResendVerificationResult
  = ResendVerificationDelivered
  | ResendVerificationSuppressed VerificationResendSuppression

data VerificationResendSuppression
  = VerificationResendThrottled
  | VerificationResendNoLongerPending

-- | A candidate has been durably claimed or has an ordinary suppression.
data VerificationResendAdmission
  = VerificationResendReserved VerificationResendClaim
  | VerificationResendAdmissionSuppressed VerificationResendSuppression

-- | The opaque-equivalent ownership handle returned by the store.  Its
-- fields are durable identities only; no caller can use it to recover a raw
-- token or address a database row outside the account lifecycle.
data VerificationResendClaim = VerificationResendClaim
  { verificationResendClaimAccountId :: AccountId,
    verificationResendClaimTokenDigest :: EmailVerificationTokenDigest
  }

data VerificationResendClaimSettlement
  = VerificationResendClaimSettled
  | VerificationResendClaimLost

-- | Private failure classification for a verification-email attempt.  A
-- timeout is a distinct operational outcome, rather than a matching message
-- string, so application telemetry can alert on exhausted SMTP deadlines
-- without recording the recipient, token, or SMTP conversation.
-- The type deliberately has no 'Show' instance for the same boundary
-- discipline.
data VerificationDeliveryFailure
  = VerificationDeliveryTimedOut
  | -- | An email adapter threw an unexpected exception.  The adapter is an
    -- application-supplied boundary, so its exception text is untrusted and
    -- must not enter action diagnostics or production logs.
    VerificationDeliveryTransportFailed

-- | What is being registered: the applicant-supplied identity and
-- credential. Grouping these stops a positional call site from, for
-- example, swapping the optional display name for the required email.
data RegistrationRequest = RegistrationRequest
  { registrationEmail :: EmailAddress,
    registrationPassword :: Password,
    registrationUsername :: Maybe Username,
    registrationDisplayName :: Maybe Text
  }

-- | The shared dependencies and clock for creating or replacing an email
-- verification token. Persistence and the clock/expiry belong to this
-- lifecycle, while delivery itself is a reusable capability record.
data EmailVerificationEnvironment = EmailVerificationEnvironment
  { verificationStore :: AccountStore,
    verificationDeliveryEnvironment :: VerificationDeliveryEnvironment,
    verificationNow :: UnixTimeNanoseconds,
    verificationLifetime :: Word64
  }

-- | All stable inputs to rendering and delivering a verification message.
-- Keeping them together prevents registration and resend from accidentally
-- pairing a URL or locale with a different mail transport policy.
data VerificationDeliveryEnvironment = VerificationDeliveryEnvironment
  { verificationDeliveryTimeout :: RegistrationDeliveryTimeout,
    verificationDelivery :: EmailDelivery,
    verificationLocale :: EmailLocale,
    verificationUrl :: EmailVerificationToken -> Text
  }

-- | The dependencies specific to creating a pending registration.
-- Password hashing and storage policy remain separate from the reusable
-- verification lifecycle in 'EmailVerificationEnvironment', so callers name
-- both roles instead of conflating registration-only work with a resend.
data RegistrationEnvironment = RegistrationEnvironment
  { registrationPasswordHasher :: PasswordHashingPolicy -> Password -> IO (Maybe PasswordHash),
    registrationHashingPolicy :: PasswordHashingPolicy,
    registrationPasswordWorkGate :: PasswordWorkGate,
    registrationStoragePolicy :: PendingRegistrationStoragePolicy,
    registrationVerificationEnvironment :: EmailVerificationEnvironment
  }

registerAccount :: RegistrationEnvironment -> RegistrationRequest -> IO (Either RegistrationError RegistrationResult)
registerAccount environment request =
  runExceptT $ do
    expiresAt <- fromMaybeError RegistrationClockOverflow (addNanoseconds now verificationLifetimeNanoseconds)
    maybeInputs <-
      liftIO $
        withPasswordWork
          passwordWorkGate
          (passwordHashMemoryKibibytes passwordHashingPolicy)
          (generateRegistrationInputs passwordHasher passwordHashingPolicy password)
    (passwordHash, accountId, token) <-
      case maybeInputs of
        Nothing -> throwError RegistrationPasswordWorkBudgetExhausted
        Just inputs -> liftMaybeWith RegistrationPasswordHashingFailed (pure inputs)
    let pendingAccount =
          PendingAccount
            { pendingAccountId = accountId,
              pendingAccountEmail = emailAddress,
              pendingAccountUsername = registrationUsername request,
              pendingAccountDisplayName = registrationDisplayName request,
              pendingAccountPasswordHash = passwordHash,
              pendingAccountVerification = mkStoredEmailVerification accountId emailAddress expiresAt token,
              pendingAccountCreatedAtNanoseconds = now
            }
    outcome <- liftAccountStore RegistrationStoreError (createPendingAccount accountStore storagePolicy pendingAccount)
    case outcome of
      PendingAccountCreated -> do
        let claim = PendingRegistrationClaim accountId (storedVerificationTokenDigest (pendingAccountVerification pendingAccount)) PendingRegistrationCreated
        deliverRegistrationVerification claim token
        pure (RegistrationCreated accountId)
      PendingAccountDeliveryClaimed claim -> do
        deliverRegistrationVerification claim token
        pure $
          case pendingRegistrationClaimStage claim of
            PendingRegistrationCreated -> RegistrationCreated (pendingRegistrationClaimAccountId claim)
            PendingRegistrationRetried -> RegistrationRetried (pendingRegistrationClaimAccountId claim)
      PendingAccountEmailTaken -> pure RegistrationAlreadyRegistered
      PendingAccountUsernameTaken -> pure RegistrationUsernameTaken
      PendingAccountStorageExhausted -> throwError RegistrationStorageExhausted
  where
    passwordHasher = registrationPasswordHasher environment
    passwordHashingPolicy = registrationHashingPolicy environment
    passwordWorkGate = registrationPasswordWorkGate environment
    storagePolicy = registrationStoragePolicy environment
    verificationEnvironment = registrationVerificationEnvironment environment
    deliveryEnvironment = verificationDeliveryEnvironment verificationEnvironment
    accountStore = verificationStore verificationEnvironment
    now = verificationNow verificationEnvironment
    verificationLifetimeNanoseconds = verificationLifetime verificationEnvironment
    emailAddress = registrationEmail request
    password = registrationPassword request

    -- \| PR-S6 (2026-08-24): a registration is staged before SMTP, then its
    -- delivery claim is settled only after the transport succeeds.  A failed
    -- send releases the matching claim for a later identical registration;
    -- an abandoned claim becomes retryable after the storage policy lease.
    -- This extends 'AccountStore' rather than introducing an independent
    -- outbox because that store already owns pending-account and verification
    -- token persistence.  The application supplies the bounded capacity and
    -- lease policy; see @docs/design-guidance.md@.
    deliverRegistrationVerification claim token = do
      deliveryResult <- liftIO (deliverVerificationMessage deliveryEnvironment emailAddress token)
      case deliveryResult of
        Left deliveryFailure -> do
          _ <- liftAccountStore RegistrationStoreError (releasePendingRegistrationDelivery accountStore claim)
          throwError (RegistrationDeliveryFailed deliveryFailure)
        Right () -> do
          completed <- liftAccountStore RegistrationStoreError (completePendingRegistrationDelivery accountStore claim)
          guardError RegistrationDeliveryClaimLost completed

confirmEmailVerificationAt :: AccountStore -> UnixTimeNanoseconds -> EmailVerificationToken -> IO (Either AccountStoreError EmailVerificationValidation)
confirmEmailVerificationAt accountStore now token =
  runExceptT $ do
    maybeStoredVerification <- liftAccountStore id (findEmailVerification accountStore (emailVerificationTokenDigest token))
    maybe (pure EmailVerificationRejected) confirmStoredEmailVerification maybeStoredVerification
  where
    confirmStoredEmailVerification storedVerification =
      case validateEmailVerificationToken now token storedVerification of
        acceptedVerification@(EmailVerificationAccepted accountId _) -> do
          maybeConsumedAccountId <- liftAccountStore id (consumeEmailVerification accountStore (emailVerificationTokenDigest token) now)
          maybe (pure EmailVerificationRejected) (confirmConsumedAccount acceptedVerification accountId) maybeConsumedAccountId
        validationResult -> pure validationResult

    confirmConsumedAccount acceptedVerification accountId consumedAccountId = do
      guardError (AccountStoreCorruptData "email verification was consumed for a different account") (consumedAccountId == accountId)
      pure acceptedVerification

resendEmailVerificationAt ::
  EmailVerificationEnvironment ->
  AccountProfile ->
  IO (Either ResendVerificationError ResendVerificationResult)
resendEmailVerificationAt verificationEnvironment profile@AccountProfile {} =
  mask $ \restore ->
    runExceptT $ do
      expiresAt <- fromMaybeError ResendVerificationClockOverflow (addNanoseconds now verificationLifetimeNanoseconds)
      token <- liftIO generateEmailVerificationToken
      let verification = mkStoredEmailVerification (accountProfileId profile) (accountProfileEmail profile) expiresAt token
      admission <- liftAccountStore ResendVerificationStoreError (restore (reserveVerificationResend accountStore defaultVerificationResendPolicy verification now))
      case admission of
        VerificationResendAdmissionSuppressed suppression -> pure (ResendVerificationSuppressed suppression)
        VerificationResendReserved claim -> do
          deliveryResult <-
            liftIO $
              restore (deliverVerificationMessage deliveryEnvironment (accountProfileEmail profile) token)
                `onException` discardClaim claim
          case deliveryResult of
            Left deliveryFailure -> do
              settled <- liftAccountStore ResendVerificationStoreError (releaseVerificationResend accountStore claim)
              case settled of
                VerificationResendClaimSettled -> throwError (ResendVerificationDeliveryFailed deliveryFailure)
                VerificationResendClaimLost -> throwError (ResendVerificationStoreError (AccountStoreCorruptData "verification resend claim was lost while releasing delivery"))
            Right () -> do
              settled <-
                liftAccountStore ResendVerificationStoreError $
                  restore (completeVerificationResend accountStore claim now)
                    `onException` discardClaim claim
              pure $
                case settled of
                  VerificationResendClaimSettled -> ResendVerificationDelivered
                  VerificationResendClaimLost -> ResendVerificationSuppressed VerificationResendNoLongerPending
  where
    accountStore = verificationStore verificationEnvironment
    deliveryEnvironment = verificationDeliveryEnvironment verificationEnvironment
    now = verificationNow verificationEnvironment
    verificationLifetimeNanoseconds = verificationLifetime verificationEnvironment
    discardClaim claim@VerificationResendClaim {} =
      void (releaseVerificationResend accountStore claim)

liftAccountStore ::
  (AccountStoreError -> error) ->
  IO (Either AccountStoreError value) ->
  ExceptT error IO value
liftAccountStore = liftEitherWith

generateRegistrationInputs ::
  (PasswordHashingPolicy -> Password -> IO (Maybe PasswordHash)) ->
  PasswordHashingPolicy ->
  Password ->
  IO (Maybe (PasswordHash, AccountId, EmailVerificationToken))
generateRegistrationInputs passwordHasher passwordHashingPolicy password =
  traverse generateIdentifiers =<< passwordHasher passwordHashingPolicy password
  where
    generateIdentifiers passwordHash =
      (,,) passwordHash <$> generateAccountId <*> generateEmailVerificationToken

deliverVerificationMessage ::
  VerificationDeliveryEnvironment ->
  EmailAddress ->
  EmailVerificationToken ->
  IO (Either VerificationDeliveryFailure ())
deliverVerificationMessage deliveryEnvironment emailAddress token = do
  deliveryResult <- try (timeout microseconds (deliverEmail emailDelivery (verificationEmail locale emailAddress (renderVerificationUrl token)))) :: IO (Either SomeException (Maybe ()))
  case deliveryResult of
    Left exception ->
      case fromException exception :: Maybe AsyncException of
        Just asyncException -> throwIO asyncException
        Nothing -> pure (Left VerificationDeliveryTransportFailed)
    Right Nothing -> pure (Left VerificationDeliveryTimedOut)
    Right (Just ()) -> pure (Right ())
  where
    RegistrationDeliveryTimeout microseconds = verificationDeliveryTimeout deliveryEnvironment
    emailDelivery = verificationDelivery deliveryEnvironment
    locale = verificationLocale deliveryEnvironment
    renderVerificationUrl = verificationUrl deliveryEnvironment

addNanoseconds :: UnixTimeNanoseconds -> Word64 -> Maybe UnixTimeNanoseconds
addNanoseconds = addUnixTimeNanoseconds
