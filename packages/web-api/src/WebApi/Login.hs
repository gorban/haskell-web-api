module WebApi.Login
  ( AccountCredential (..),
    AccountCredentialStore (..),
    AccountCredentialStoreError (..),
    PasswordLoginResult (..),
    beginPasswordLogin,
  )
where

import Data.Text (Text)
import HarchWeb.Account (AccountId)
import HarchWeb.Email (EmailAddress)
import HarchWeb.Password (Password, PasswordHash, verifyPassword)
import WebApi.Mfa (MfaStore (..), MfaStoreError, StoredTotpEnrollment (..))

data AccountCredential = AccountCredential
  { accountCredentialId :: AccountId,
    accountCredentialPasswordHash :: PasswordHash,
    accountCredentialEmailVerified :: Bool
  }

data AccountCredentialStoreError
  = AccountCredentialStoreUnavailable Text
  | AccountCredentialStoreCorruptData Text
  deriving (Eq)

newtype AccountCredentialStore = AccountCredentialStore
  { findAccountCredentialByEmail :: EmailAddress -> IO (Either AccountCredentialStoreError (Maybe AccountCredential))
  }

data PasswordLoginResult
  = PasswordLoginRejected
  | PasswordLoginEmailVerificationRequired AccountId
  | PasswordLoginMfaEnrollmentRequired AccountId
  | PasswordLoginMfaRequired AccountId
  | PasswordLoginCredentialStoreError AccountCredentialStoreError
  | PasswordLoginMfaStoreError MfaStoreError
  deriving (Eq)

-- | Validates the password first, then requires a confirmed authenticator.
-- This function intentionally never creates a session: completing the second
-- factor is required before the application may authenticate the account.
beginPasswordLogin :: AccountCredentialStore -> MfaStore -> EmailAddress -> Password -> IO PasswordLoginResult
beginPasswordLogin credentialStore mfaStore emailAddress password = do
  credentialResult <- findAccountCredentialByEmail credentialStore emailAddress
  case credentialResult of
    Left storeError -> pure (PasswordLoginCredentialStoreError storeError)
    Right Nothing -> pure PasswordLoginRejected
    Right (Just credential)
      | not (verifyPassword password (accountCredentialPasswordHash credential)) -> pure PasswordLoginRejected
      | not (accountCredentialEmailVerified credential) -> pure (PasswordLoginEmailVerificationRequired (accountCredentialId credential))
      | otherwise -> do
          enrollmentResult <- loadTotpEnrollment mfaStore (accountCredentialId credential)
          pure $
            case enrollmentResult of
              Left storeError -> PasswordLoginMfaStoreError storeError
              Right Nothing -> PasswordLoginMfaEnrollmentRequired (accountCredentialId credential)
              Right (Just StoredTotpEnrollment {storedTotpConfirmedAtNanoseconds = Nothing}) -> PasswordLoginMfaEnrollmentRequired (accountCredentialId credential)
              Right (Just StoredTotpEnrollment {storedTotpConfirmedAtNanoseconds = Just _}) -> PasswordLoginMfaRequired (accountCredentialId credential)
