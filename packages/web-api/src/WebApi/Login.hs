module WebApi.Login
  ( AccountCredential (..),
    AccountCredentialStore (..),
    AccountCredentialStoreError (..),
    MfaLoginProof (..),
    PasswordMfaLoginResult (..),
    PasswordLoginResult (..),
    beginPasswordLogin,
    completePasswordLogin,
  )
where

import Data.List (find)
import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word64)
import HarchWeb.Account (AccountId)
import HarchWeb.Email (EmailAddress)
import HarchWeb.Password (Password, PasswordHash, verifyPassword)
import HarchWeb.RecoveryCode (RecoveryCode, readRecoveryCodeHash, recoveryCodeHashText, verifyRecoveryCode)
import HarchWeb.Secret (SecretEncryptionKey, decryptSecret)
import HarchWeb.Totp (TotpCode, TotpSecret, mkTotpSecret, validateTotpCode)
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

data MfaLoginProof
  = TotpLoginProof TotpCode
  | RecoveryCodeLoginProof RecoveryCode
  deriving (Eq)

data PasswordMfaLoginResult
  = PasswordMfaLoginRejected
  | PasswordMfaLoginEmailVerificationRequired AccountId
  | PasswordMfaLoginEnrollmentRequired AccountId
  | PasswordMfaLoginAccepted AccountId
  | PasswordMfaLoginCredentialStoreError AccountCredentialStoreError
  | PasswordMfaLoginMfaStoreError MfaStoreError
  | PasswordMfaLoginCorruptEnrollment
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

-- | Performs password validation before examining the supplied second factor.
-- A recovery code is marked used by its stored hash only after its Argon2id
-- verification succeeds, so its cleartext representation never reaches storage.
completePasswordLogin ::
  AccountCredentialStore ->
  MfaStore ->
  SecretEncryptionKey ->
  Word64 ->
  Word64 ->
  EmailAddress ->
  Password ->
  MfaLoginProof ->
  IO PasswordMfaLoginResult
completePasswordLogin credentialStore mfaStore encryptionKey nowNanoseconds nowSeconds emailAddress password proof = do
  passwordResult <- beginPasswordLogin credentialStore mfaStore emailAddress password
  case passwordResult of
    PasswordLoginRejected -> pure PasswordMfaLoginRejected
    PasswordLoginEmailVerificationRequired accountId -> pure (PasswordMfaLoginEmailVerificationRequired accountId)
    PasswordLoginMfaEnrollmentRequired accountId -> pure (PasswordMfaLoginEnrollmentRequired accountId)
    PasswordLoginCredentialStoreError storeError -> pure (PasswordMfaLoginCredentialStoreError storeError)
    PasswordLoginMfaStoreError storeError -> pure (PasswordMfaLoginMfaStoreError storeError)
    PasswordLoginMfaRequired accountId -> completeConfirmedEnrollment accountId
  where
    completeConfirmedEnrollment accountId = do
      enrollmentResult <- loadTotpEnrollment mfaStore accountId
      case enrollmentResult of
        Left storeError -> pure (PasswordMfaLoginMfaStoreError storeError)
        Right Nothing -> pure (PasswordMfaLoginEnrollmentRequired accountId)
        Right (Just StoredTotpEnrollment {storedTotpConfirmedAtNanoseconds = Nothing}) -> pure (PasswordMfaLoginEnrollmentRequired accountId)
        Right (Just StoredTotpEnrollment {storedTotpEncryptedSecret}) ->
          case proof of
            TotpLoginProof suppliedCode ->
              pure $
                case decodeTotpSecret encryptionKey storedTotpEncryptedSecret of
                  Nothing -> PasswordMfaLoginCorruptEnrollment
                  Just secret ->
                    if validateTotpCode nowSeconds secret suppliedCode
                      then PasswordMfaLoginAccepted accountId
                      else PasswordMfaLoginRejected
            RecoveryCodeLoginProof suppliedCode -> completeRecoveryCode accountId suppliedCode

    completeRecoveryCode accountId suppliedCode = do
      recoveryHashResult <- loadUnusedRecoveryCodeHashes mfaStore accountId
      case recoveryHashResult of
        Left storeError -> pure (PasswordMfaLoginMfaStoreError storeError)
        Right recoveryHashValues ->
          case traverse readRecoveryCodeHash recoveryHashValues of
            Nothing -> pure PasswordMfaLoginCorruptEnrollment
            Just recoveryHashes ->
              case find (verifyRecoveryCode suppliedCode) recoveryHashes of
                Nothing -> pure PasswordMfaLoginRejected
                Just matchingHash -> do
                  consumptionResult <- consumeRecoveryCodeHash mfaStore accountId (recoveryCodeHashText matchingHash) nowNanoseconds
                  pure $
                    case consumptionResult of
                      Left storeError -> PasswordMfaLoginMfaStoreError storeError
                      Right True -> PasswordMfaLoginAccepted accountId
                      Right False -> PasswordMfaLoginRejected

decodeTotpSecret :: SecretEncryptionKey -> Text -> Maybe TotpSecret
decodeTotpSecret encryptionKey encryptedSecret = do
  plaintext <- decryptSecret encryptionKey encryptedSecret
  renderedSecret <- either (const Nothing) Just (TextEncoding.decodeUtf8' plaintext)
  mkTotpSecret renderedSecret
