{-# LANGUAGE OverloadedStrings #-}

module WebApi.Account
  ( AccountStore (..),
    AccountStoreError (..),
    AccountProfile (..),
    AccountProfileStore (..),
    CreatePendingAccountOutcome (..),
    PendingAccount (..),
    RegistrationEnvironment (..),
    RegistrationRequest (..),
    ResendVerificationError (..),
    RegistrationError (..),
    RegistrationResult (..),
    confirmEmailVerificationAt,
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
  { createPendingAccount :: PendingAccount -> IO (Either AccountStoreError CreatePendingAccountOutcome),
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
  | PendingAccountEmailTaken
  | PendingAccountUsernameTaken

data RegistrationError
  = RegistrationStoreError AccountStoreError
  | RegistrationPasswordHashingFailed
  | RegistrationPasswordWorkBudgetExhausted
  | RegistrationDeliveryFailed Text
  | RegistrationClockOverflow

data RegistrationResult
  = RegistrationCreated AccountId
  | RegistrationAlreadyRegistered
  | RegistrationUsernameTaken

data ResendVerificationError
  = ResendVerificationStoreError AccountStoreError
  | ResendVerificationDeliveryFailed Text
  | ResendVerificationClockOverflow
  | ResendVerificationNoLongerPending
  deriving (Eq, Show)

-- | What is being registered: the applicant-supplied identity and
-- credential. Grouping these stops a positional call site from, for
-- example, swapping the optional display name for the required email.
data RegistrationRequest = RegistrationRequest
  { registrationEmail :: EmailAddress,
    registrationPassword :: Password,
    registrationUsername :: Maybe Username,
    registrationDisplayName :: Maybe Text
  }

-- | The dependencies and clock a registration attempt runs against.
-- Grouping these lets one call construct a single value instead of
-- threading eight independent, same-shaped-in-places arguments; a caller
-- that wants only @HarchWeb.Password.hashPassword@ can build this once and
-- reuse it, replacing the old default-hasher convenience functions.
data RegistrationEnvironment = RegistrationEnvironment
  { registrationPasswordHasher :: PasswordHashingPolicy -> Password -> IO (Maybe PasswordHash),
    registrationHashingPolicy :: PasswordHashingPolicy,
    registrationPasswordWorkGate :: PasswordWorkGate,
    registrationStore :: AccountStore,
    registrationDelivery :: EmailDelivery,
    registrationLocale :: EmailLocale,
    registrationVerificationUrl :: EmailVerificationToken -> Text,
    registrationNow :: UnixTimeNanoseconds,
    registrationLifetime :: Word64
  }

registerAccount :: RegistrationEnvironment -> RegistrationRequest -> IO (Either RegistrationError RegistrationResult)
registerAccount environment request =
  runExceptT $ do
    expiresAt <- fromMaybeError RegistrationClockOverflow (addNanoseconds now verificationLifetime)
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
    outcome <- liftAccountStore RegistrationStoreError (createPendingAccount accountStore pendingAccount)
    case outcome of
      PendingAccountCreated -> do
        deliverVerificationEmail RegistrationDeliveryFailed emailDelivery locale emailAddress renderVerificationUrl token
        pure (RegistrationCreated accountId)
      PendingAccountEmailTaken -> pure RegistrationAlreadyRegistered
      PendingAccountUsernameTaken -> pure RegistrationUsernameTaken
  where
    passwordHasher = registrationPasswordHasher environment
    passwordHashingPolicy = registrationHashingPolicy environment
    passwordWorkGate = registrationPasswordWorkGate environment
    accountStore = registrationStore environment
    emailDelivery = registrationDelivery environment
    locale = registrationLocale environment
    renderVerificationUrl = registrationVerificationUrl environment
    now = registrationNow environment
    verificationLifetime = registrationLifetime environment
    emailAddress = registrationEmail request
    password = registrationPassword request

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
  AccountStore ->
  EmailDelivery ->
  EmailLocale ->
  (EmailVerificationToken -> Text) ->
  UnixTimeNanoseconds ->
  Word64 ->
  AccountProfile ->
  IO (Either ResendVerificationError ())
resendEmailVerificationAt accountStore emailDelivery locale renderVerificationUrl now verificationLifetime profile =
  runExceptT $ do
    guardError ResendVerificationNoLongerPending (not (accountProfileEmailVerified profile))
    expiresAt <- fromMaybeError ResendVerificationClockOverflow (addNanoseconds now verificationLifetime)
    token <- liftIO generateEmailVerificationToken
    let verification = mkStoredEmailVerification (accountProfileId profile) (accountProfileEmail profile) expiresAt token
    replaced <- liftAccountStore ResendVerificationStoreError (replaceEmailVerification accountStore verification)
    guardError ResendVerificationNoLongerPending replaced
    deliverVerificationEmail ResendVerificationDeliveryFailed emailDelivery locale (accountProfileEmail profile) renderVerificationUrl token

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
  (Text -> error) ->
  EmailDelivery ->
  EmailLocale ->
  EmailAddress ->
  (EmailVerificationToken -> Text) ->
  EmailVerificationToken ->
  ExceptT error IO ()
deliverVerificationEmail toError emailDelivery locale emailAddress renderVerificationUrl token =
  either (throwError . toError . Text.pack . displayException) pure
    =<< liftIO (try (deliverEmail emailDelivery (verificationEmail locale emailAddress (renderVerificationUrl token))) :: IO (Either SomeException ()))

addNanoseconds :: UnixTimeNanoseconds -> Word64 -> Maybe UnixTimeNanoseconds
addNanoseconds = addUnixTimeNanoseconds
