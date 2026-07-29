{-# LANGUAGE OverloadedStrings #-}

module WebApi.Account
  ( AccountStore (..),
    AccountStoreError (..),
    AccountProfile (..),
    AccountProfileStore (..),
    PendingAccount (..),
    ResendVerificationError (..),
    RegistrationError (..),
    RegistrationResult (..),
    confirmEmailVerificationAt,
    registerAccountAt,
    registerAccountAtWithPasswordHasher,
    registerAccountWithIdentityAt,
    registerAccountWithIdentityAtWithPasswordHasher,
    resendEmailVerificationAt,
  )
where

import Control.Exception (SomeException, displayException, try)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT, throwError, withExceptT)
import Control.Monad.IO.Class (liftIO)
import Core.Control.Error (fromMaybeError, guardError)
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
    hashPassword,
  )
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
    pendingAccountCreatedAtNanoseconds :: Word64
  }

data AccountStore = AccountStore
  { createPendingAccount :: PendingAccount -> IO (Either AccountStoreError Bool),
    replaceEmailVerification :: StoredEmailVerification -> IO (Either AccountStoreError Bool),
    findEmailVerification :: EmailVerificationTokenDigest -> IO (Either AccountStoreError (Maybe StoredEmailVerification)),
    consumeEmailVerification :: EmailVerificationTokenDigest -> Word64 -> IO (Either AccountStoreError (Maybe AccountId))
  }

data RegistrationError
  = RegistrationStoreError AccountStoreError
  | RegistrationPasswordHashingFailed
  | RegistrationDeliveryFailed Text
  | RegistrationClockOverflow

data RegistrationResult
  = RegistrationCreated AccountId
  | RegistrationAlreadyRegistered

data ResendVerificationError
  = ResendVerificationStoreError AccountStoreError
  | ResendVerificationDeliveryFailed Text
  | ResendVerificationClockOverflow
  | ResendVerificationNoLongerPending
  deriving (Eq, Show)

registerAccountAt ::
  PasswordHashingPolicy ->
  AccountStore ->
  EmailDelivery ->
  EmailLocale ->
  (EmailVerificationToken -> Text) ->
  Word64 ->
  Word64 ->
  EmailAddress ->
  Password ->
  IO (Either RegistrationError RegistrationResult)
registerAccountAt =
  registerAccountAtWithPasswordHasher hashPassword

registerAccountAtWithPasswordHasher ::
  (PasswordHashingPolicy -> Password -> IO (Maybe PasswordHash)) ->
  PasswordHashingPolicy ->
  AccountStore ->
  EmailDelivery ->
  EmailLocale ->
  (EmailVerificationToken -> Text) ->
  Word64 ->
  Word64 ->
  EmailAddress ->
  Password ->
  IO (Either RegistrationError RegistrationResult)
registerAccountAtWithPasswordHasher passwordHasher passwordHashingPolicy accountStore emailDelivery locale renderVerificationUrl now verificationLifetime =
  registerAccountWithIdentityAtWithPasswordHasher passwordHasher passwordHashingPolicy accountStore emailDelivery locale renderVerificationUrl now verificationLifetime Nothing Nothing

registerAccountWithIdentityAt ::
  PasswordHashingPolicy ->
  AccountStore ->
  EmailDelivery ->
  EmailLocale ->
  (EmailVerificationToken -> Text) ->
  Word64 ->
  Word64 ->
  Maybe Username ->
  Maybe Text ->
  EmailAddress ->
  Password ->
  IO (Either RegistrationError RegistrationResult)
registerAccountWithIdentityAt =
  registerAccountWithIdentityAtWithPasswordHasher hashPassword

registerAccountWithIdentityAtWithPasswordHasher ::
  (PasswordHashingPolicy -> Password -> IO (Maybe PasswordHash)) ->
  PasswordHashingPolicy ->
  AccountStore ->
  EmailDelivery ->
  EmailLocale ->
  (EmailVerificationToken -> Text) ->
  Word64 ->
  Word64 ->
  Maybe Username ->
  Maybe Text ->
  EmailAddress ->
  Password ->
  IO (Either RegistrationError RegistrationResult)
registerAccountWithIdentityAtWithPasswordHasher passwordHasher passwordHashingPolicy accountStore emailDelivery locale renderVerificationUrl now verificationLifetime maybeUsername maybeDisplayName emailAddress password =
  runExceptT $ do
    expiresAt <- fromMaybeError RegistrationClockOverflow (addNanoseconds now verificationLifetime)
    passwordHash <-
      liftIO (passwordHasher passwordHashingPolicy password)
        >>= fromMaybeError RegistrationPasswordHashingFailed
    accountId <- liftIO generateAccountId
    token <- liftIO generateEmailVerificationToken
    let pendingAccount =
          PendingAccount
            { pendingAccountId = accountId,
              pendingAccountEmail = emailAddress,
              pendingAccountUsername = maybeUsername,
              pendingAccountDisplayName = maybeDisplayName,
              pendingAccountPasswordHash = passwordHash,
              pendingAccountVerification = mkStoredEmailVerification accountId emailAddress expiresAt token,
              pendingAccountCreatedAtNanoseconds = now
            }
    created <- liftAccountStore RegistrationStoreError (createPendingAccount accountStore pendingAccount)
    if created
      then do
        deliverVerificationEmail RegistrationDeliveryFailed emailDelivery locale emailAddress renderVerificationUrl token
        pure (RegistrationCreated accountId)
      else pure RegistrationAlreadyRegistered

confirmEmailVerificationAt :: AccountStore -> Word64 -> EmailVerificationToken -> IO (Either AccountStoreError EmailVerificationValidation)
confirmEmailVerificationAt accountStore now token =
  runExceptT $ do
    maybeStoredVerification <- liftAccountStore id (findEmailVerification accountStore (emailVerificationTokenDigest token))
    case maybeStoredVerification of
      Nothing -> pure EmailVerificationRejected
      Just storedVerification -> confirmStoredEmailVerification storedVerification
  where
    confirmStoredEmailVerification storedVerification =
      case validateEmailVerificationToken now token storedVerification of
        acceptedVerification@(EmailVerificationAccepted accountId _) -> do
          maybeConsumedAccountId <- liftAccountStore id (consumeEmailVerification accountStore (emailVerificationTokenDigest token) now)
          case maybeConsumedAccountId of
            Nothing -> pure EmailVerificationRejected
            Just consumedAccountId -> do
              guardError (AccountStoreCorruptData "email verification was consumed for a different account") (consumedAccountId == accountId)
              pure acceptedVerification
        validationResult -> pure validationResult

resendEmailVerificationAt ::
  AccountStore ->
  EmailDelivery ->
  EmailLocale ->
  (EmailVerificationToken -> Text) ->
  Word64 ->
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
liftAccountStore toError = withExceptT toError . ExceptT

deliverVerificationEmail ::
  (Text -> error) ->
  EmailDelivery ->
  EmailLocale ->
  EmailAddress ->
  (EmailVerificationToken -> Text) ->
  EmailVerificationToken ->
  ExceptT error IO ()
deliverVerificationEmail toError emailDelivery locale emailAddress renderVerificationUrl token =
  liftIO (try (deliverEmail emailDelivery (verificationEmail locale emailAddress (renderVerificationUrl token))) :: IO (Either SomeException ()))
    >>= either (throwError . toError . Text.pack . displayException) pure

addNanoseconds :: Word64 -> Word64 -> Maybe Word64
addNanoseconds now duration =
  let result = now + duration
   in if result < now then Nothing else Just result
