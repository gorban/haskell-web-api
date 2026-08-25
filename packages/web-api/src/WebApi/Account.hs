{-# LANGUAGE OverloadedStrings #-}

module WebApi.Account
  ( AccountStore (..),
    AccountStoreError (..),
    AccountProfile (..),
    AccountProfileStore (..),
    CreatePendingAccountOutcome (..),
    PendingRegistrationClaim (..),
    PendingRegistrationDeliveryStage (..),
    PendingRegistrationStoragePolicy,
    RegistrationDeliveryTimeout,
    PendingAccount (..),
    EmailVerificationEnvironment (..),
    RegistrationEnvironment (..),
    RegistrationRequest (..),
    ResendVerificationError (..),
    RegistrationError (..),
    RegistrationResult (..),
    VerificationDeliveryFailure (..),
    confirmEmailVerificationAt,
    defaultPendingRegistrationStoragePolicy,
    defaultRegistrationDeliveryTimeout,
    mkPendingRegistrationStoragePolicy,
    mkRegistrationDeliveryTimeout,
    pendingRegistrationClaimLeaseNanoseconds,
    pendingRegistrationMaximumAccounts,
    registerAccount,
    resendEmailVerificationAt,
  )
where

import Control.Exception (SomeException, displayException, try)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Core.Control.Error (fromMaybeError, guardError, liftEitherWith, liftMaybeWith)
import Data.Text (Text)
import Data.Text qualified as Text
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
mkPendingRegistrationStoragePolicy maximumAccounts claimLeaseNanoseconds
  | maximumAccounts == 0 || claimLeaseNanoseconds == 0 = Nothing
  | otherwise = Just (PendingRegistrationStoragePolicy maximumAccounts claimLeaseNanoseconds)

-- | Reference-application policy: a maximum of 100,000 unverified accounts
-- and a five-minute abandoned-delivery lease.  Expired verification tokens
-- are the retention boundary, so the staging transaction removes their
-- unverified accounts before it evaluates this capacity.
defaultPendingRegistrationStoragePolicy :: PendingRegistrationStoragePolicy
defaultPendingRegistrationStoragePolicy = PendingRegistrationStoragePolicy 100000 (5 * 60 * 1000000000)

-- | A positive, account-workflow-owned deadline for one SMTP delivery.  The
-- SMTP transport can otherwise wait indefinitely for DNS, connect, or a peer
-- response, retaining a registration delivery claim forever instead of
-- returning the recoverable failure its caller expects.
newtype RegistrationDeliveryTimeout = RegistrationDeliveryTimeout Int

mkRegistrationDeliveryTimeout :: Int -> Maybe RegistrationDeliveryTimeout
mkRegistrationDeliveryTimeout microseconds
  | microseconds <= 0 = Nothing
  | otherwise = Just (RegistrationDeliveryTimeout microseconds)

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
  | ResendVerificationDeliveryFailed Text
  | ResendVerificationClockOverflow
  | ResendVerificationNoLongerPending
  deriving (Eq, Show)

-- | Private failure classification for a verification-email attempt.  A
-- timeout is a distinct operational outcome, rather than a matching message
-- string, so application telemetry can alert on exhausted SMTP deadlines
-- without recording the recipient, token, or SMTP conversation.
data VerificationDeliveryFailure
  = VerificationDeliveryTimedOut
  | VerificationDeliveryTransportFailed Text

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
-- verification token.  Registration and a later resend have the same
-- persistence, delivery, locale, URL, and expiry concerns, so this is one
-- reusable context rather than forcing the resend path to thread them as
-- seven positional inputs.
data EmailVerificationEnvironment = EmailVerificationEnvironment
  { verificationStore :: AccountStore,
    verificationDeliveryTimeout :: RegistrationDeliveryTimeout,
    verificationDelivery :: EmailDelivery,
    verificationLocale :: EmailLocale,
    verificationUrl :: EmailVerificationToken -> Text,
    verificationNow :: UnixTimeNanoseconds,
    verificationLifetime :: Word64
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
    deliveryTimeout = verificationDeliveryTimeout verificationEnvironment
    accountStore = verificationStore verificationEnvironment
    emailDelivery = verificationDelivery verificationEnvironment
    locale = verificationLocale verificationEnvironment
    renderVerificationUrl = verificationUrl verificationEnvironment
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
      deliveryResult <- liftIO (deliverVerificationMessage deliveryTimeout emailDelivery locale emailAddress renderVerificationUrl token)
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
  IO (Either ResendVerificationError ())
resendEmailVerificationAt verificationEnvironment profile =
  runExceptT $ do
    guardError ResendVerificationNoLongerPending (not (accountProfileEmailVerified profile))
    expiresAt <- fromMaybeError ResendVerificationClockOverflow (addNanoseconds now verificationLifetimeNanoseconds)
    token <- liftIO generateEmailVerificationToken
    let verification = mkStoredEmailVerification (accountProfileId profile) (accountProfileEmail profile) expiresAt token
    replaced <- liftAccountStore ResendVerificationStoreError (replaceEmailVerification accountStore verification)
    guardError ResendVerificationNoLongerPending replaced
    deliverVerificationEmail (ResendVerificationDeliveryFailed . renderVerificationDeliveryFailure) deliveryTimeout emailDelivery locale (accountProfileEmail profile) renderVerificationUrl token
  where
    accountStore = verificationStore verificationEnvironment
    deliveryTimeout = verificationDeliveryTimeout verificationEnvironment
    emailDelivery = verificationDelivery verificationEnvironment
    locale = verificationLocale verificationEnvironment
    renderVerificationUrl = verificationUrl verificationEnvironment
    now = verificationNow verificationEnvironment
    verificationLifetimeNanoseconds = verificationLifetime verificationEnvironment

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

deliverVerificationEmail ::
  (VerificationDeliveryFailure -> error) ->
  RegistrationDeliveryTimeout ->
  EmailDelivery ->
  EmailLocale ->
  EmailAddress ->
  (EmailVerificationToken -> Text) ->
  EmailVerificationToken ->
  ExceptT error IO ()
deliverVerificationEmail toError deliveryTimeout emailDelivery locale emailAddress renderVerificationUrl token =
  either (throwError . toError) pure
    =<< liftIO (deliverVerificationMessage deliveryTimeout emailDelivery locale emailAddress renderVerificationUrl token)

deliverVerificationMessage ::
  RegistrationDeliveryTimeout ->
  EmailDelivery ->
  EmailLocale ->
  EmailAddress ->
  (EmailVerificationToken -> Text) ->
  EmailVerificationToken ->
  IO (Either VerificationDeliveryFailure ())
deliverVerificationMessage (RegistrationDeliveryTimeout microseconds) emailDelivery locale emailAddress renderVerificationUrl token = do
  deliveryResult <- try (timeout microseconds (deliverEmail emailDelivery (verificationEmail locale emailAddress (renderVerificationUrl token)))) :: IO (Either SomeException (Maybe ()))
  pure $
    case deliveryResult of
      Left deliveryFailure -> Left (VerificationDeliveryTransportFailed (Text.pack (displayException deliveryFailure)))
      Right Nothing -> Left VerificationDeliveryTimedOut
      Right (Just ()) -> Right ()

renderVerificationDeliveryFailure :: VerificationDeliveryFailure -> Text
renderVerificationDeliveryFailure deliveryFailure =
  case deliveryFailure of
    VerificationDeliveryTimedOut -> "email delivery timed out"
    VerificationDeliveryTransportFailed detail -> detail

addNanoseconds :: UnixTimeNanoseconds -> Word64 -> Maybe UnixTimeNanoseconds
addNanoseconds = addUnixTimeNanoseconds
