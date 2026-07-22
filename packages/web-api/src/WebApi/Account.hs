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
    resendEmailVerificationAt,
  )
where

import Control.Exception (SomeException, displayException, try)
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

data AccountStoreError
  = AccountStoreUnavailable Text
  | AccountStoreCorruptData Text
  deriving (Eq, Show)

-- | The safe account data required by authenticated page surfaces. Password
-- hashes, verification tokens, and other credentials never cross this seam.
data AccountProfile = AccountProfile
  { accountProfileId :: AccountId,
    accountProfileEmail :: EmailAddress,
    accountProfileEmailVerified :: Bool
  }

newtype AccountProfileStore = AccountProfileStore
  { findAccountProfile :: AccountId -> IO (Either AccountStoreError (Maybe AccountProfile))
  }

data PendingAccount = PendingAccount
  { pendingAccountId :: AccountId,
    pendingAccountEmail :: EmailAddress,
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
registerAccountAtWithPasswordHasher passwordHasher passwordHashingPolicy accountStore emailDelivery locale renderVerificationUrl now verificationLifetime emailAddress password =
  case addNanoseconds now verificationLifetime of
    Nothing -> pure (Left RegistrationClockOverflow)
    Just expiresAt -> do
      maybePasswordHash <- passwordHasher passwordHashingPolicy password
      case maybePasswordHash of
        Nothing -> pure (Left RegistrationPasswordHashingFailed)
        Just passwordHash -> do
          accountId <- generateAccountId
          token <- generateEmailVerificationToken
          let pendingAccount =
                PendingAccount
                  { pendingAccountId = accountId,
                    pendingAccountEmail = emailAddress,
                    pendingAccountPasswordHash = passwordHash,
                    pendingAccountVerification = mkStoredEmailVerification accountId emailAddress expiresAt token,
                    pendingAccountCreatedAtNanoseconds = now
                  }
          creationResult <- createPendingAccount accountStore pendingAccount
          case creationResult of
            Left storeError -> pure (Left (RegistrationStoreError storeError))
            Right False -> pure (Right RegistrationAlreadyRegistered)
            Right True -> do
              deliveryResult <-
                try (deliverEmail emailDelivery (verificationEmail locale emailAddress (renderVerificationUrl token))) :: IO (Either SomeException ())
              pure $
                case deliveryResult of
                  Left deliveryError -> Left (RegistrationDeliveryFailed (Text.pack (displayException deliveryError)))
                  Right () -> Right (RegistrationCreated accountId)

confirmEmailVerificationAt :: AccountStore -> Word64 -> EmailVerificationToken -> IO (Either AccountStoreError EmailVerificationValidation)
confirmEmailVerificationAt accountStore now token = do
  storedResult <- findEmailVerification accountStore (emailVerificationTokenDigest token)
  case storedResult of
    Left storeError -> pure (Left storeError)
    Right Nothing -> pure (Right EmailVerificationRejected)
    Right (Just storedVerification) ->
      case validateEmailVerificationToken now token storedVerification of
        EmailVerificationAccepted accountId emailAddress -> do
          consumptionResult <- consumeEmailVerification accountStore (emailVerificationTokenDigest token) now
          pure $
            case consumptionResult of
              Left storeError -> Left storeError
              Right Nothing -> Right EmailVerificationRejected
              Right (Just consumedAccountId) ->
                if consumedAccountId == accountId
                  then Right (EmailVerificationAccepted accountId emailAddress)
                  else Left (AccountStoreCorruptData "email verification was consumed for a different account")
        validationResult -> pure (Right validationResult)

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
  if accountProfileEmailVerified profile
    then pure (Left ResendVerificationNoLongerPending)
    else case addNanoseconds now verificationLifetime of
      Nothing -> pure (Left ResendVerificationClockOverflow)
      Just expiresAt -> do
        token <- generateEmailVerificationToken
        let verification = mkStoredEmailVerification (accountProfileId profile) (accountProfileEmail profile) expiresAt token
        replacementResult <- replaceEmailVerification accountStore verification
        case replacementResult of
          Left storeError -> pure (Left (ResendVerificationStoreError storeError))
          Right False -> pure (Left ResendVerificationNoLongerPending)
          Right True -> do
            deliveryResult <-
              try (deliverEmail emailDelivery (verificationEmail locale (accountProfileEmail profile) (renderVerificationUrl token))) :: IO (Either SomeException ())
            pure $
              case deliveryResult of
                Left deliveryError -> Left (ResendVerificationDeliveryFailed (Text.pack (displayException deliveryError)))
                Right () -> Right ()

addNanoseconds :: Word64 -> Word64 -> Maybe Word64
addNanoseconds now duration =
  let result = now + duration
   in if result < now then Nothing else Just result
