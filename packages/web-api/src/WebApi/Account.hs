{-# LANGUAGE OverloadedStrings #-}

module WebApi.Account
  ( AccountStore (..),
    AccountStoreError (..),
    PendingAccount (..),
    RegistrationError (..),
    RegistrationResult (..),
    confirmEmailVerificationAt,
    registerAccountAt,
    registerAccountAtWithPasswordHasher,
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

data PendingAccount = PendingAccount
  { pendingAccountId :: AccountId,
    pendingAccountEmail :: EmailAddress,
    pendingAccountPasswordHash :: PasswordHash,
    pendingAccountVerification :: StoredEmailVerification,
    pendingAccountCreatedAtNanoseconds :: Word64
  }

data AccountStore = AccountStore
  { createPendingAccount :: PendingAccount -> IO (Either AccountStoreError Bool),
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

addNanoseconds :: Word64 -> Word64 -> Maybe Word64
addNanoseconds now duration =
  let result = now + duration
   in if result < now then Nothing else Just result
