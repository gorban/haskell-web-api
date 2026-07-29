module WebApi.Login
  ( AccountCredential (..),
    AccountCredentialStore (..),
    AccountCredentialStoreError (..),
    LoginIdentifier (..),
    MfaLoginProof (..),
    PasswordMfaLoginResult (..),
    PasswordLoginResult (..),
    beginPasswordLogin,
    beginPasswordLoginWithIdentifier,
    completePasswordLogin,
    completePasswordLoginWithIdentifier,
  )
where

import Control.Monad.Except (ExceptT, runExceptT)
import Core.Control.Error (fromMaybeError, liftEitherWith)
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
import HarchWeb.Username (Username)
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

data AccountCredentialStore = AccountCredentialStore
  { findAccountCredentialByEmail :: EmailAddress -> IO (Either AccountCredentialStoreError (Maybe AccountCredential)),
    findAccountCredentialByUsername :: Username -> IO (Either AccountCredentialStoreError (Maybe AccountCredential))
  }

data LoginIdentifier
  = LoginEmailAddress EmailAddress
  | LoginUsername Username

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

data LoginInfrastructureError
  = LoginMfaStoreError MfaStoreError
  | LoginCorruptEnrollment

data SecondFactorContext = SecondFactorContext
  { secondFactorMfaStore :: MfaStore,
    secondFactorEncryptionKey :: SecretEncryptionKey,
    secondFactorNowNanoseconds :: Word64,
    secondFactorNowSeconds :: Word64,
    secondFactorProof :: MfaLoginProof
  }

-- | Validates the password first, then requires a confirmed authenticator.
-- This function intentionally never creates a session: completing the second
-- factor is required before the application may authenticate the account.
beginPasswordLogin :: AccountCredentialStore -> MfaStore -> EmailAddress -> Password -> IO PasswordLoginResult
beginPasswordLogin credentialStore mfaStore emailAddress =
  beginPasswordLoginWithIdentifier credentialStore mfaStore (LoginEmailAddress emailAddress)

beginPasswordLoginWithIdentifier :: AccountCredentialStore -> MfaStore -> LoginIdentifier -> Password -> IO PasswordLoginResult
beginPasswordLoginWithIdentifier credentialStore mfaStore identifier password = do
  credentialResult <-
    case identifier of
      LoginEmailAddress emailAddress -> findAccountCredentialByEmail credentialStore emailAddress
      LoginUsername username -> findAccountCredentialByUsername credentialStore username
  case credentialResult of
    Left storeError -> pure (PasswordLoginCredentialStoreError storeError)
    Right Nothing -> pure PasswordLoginRejected
    Right (Just credential)
      | not (verifyPassword password (accountCredentialPasswordHash credential)) -> pure PasswordLoginRejected
      | not (accountCredentialEmailVerified credential) -> pure (PasswordLoginEmailVerificationRequired (accountCredentialId credential))
    Right (Just credential) -> do
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
  completePasswordLoginWithIdentifier credentialStore mfaStore encryptionKey nowNanoseconds nowSeconds (LoginEmailAddress emailAddress) password proof

completePasswordLoginWithIdentifier ::
  AccountCredentialStore ->
  MfaStore ->
  SecretEncryptionKey ->
  Word64 ->
  Word64 ->
  LoginIdentifier ->
  Password ->
  MfaLoginProof ->
  IO PasswordMfaLoginResult
completePasswordLoginWithIdentifier credentialStore mfaStore encryptionKey nowNanoseconds nowSeconds identifier password proof = do
  passwordResult <- beginPasswordLoginWithIdentifier credentialStore mfaStore identifier password
  continuePasswordLogin
    SecondFactorContext
      { secondFactorMfaStore = mfaStore,
        secondFactorEncryptionKey = encryptionKey,
        secondFactorNowNanoseconds = nowNanoseconds,
        secondFactorNowSeconds = nowSeconds,
        secondFactorProof = proof
      }
    passwordResult

continuePasswordLogin :: SecondFactorContext -> PasswordLoginResult -> IO PasswordMfaLoginResult
continuePasswordLogin context passwordResult =
  case passwordResult of
    PasswordLoginRejected -> pure PasswordMfaLoginRejected
    PasswordLoginEmailVerificationRequired accountId -> pure (PasswordMfaLoginEmailVerificationRequired accountId)
    PasswordLoginMfaEnrollmentRequired accountId -> pure (PasswordMfaLoginEnrollmentRequired accountId)
    PasswordLoginCredentialStoreError storeError -> pure (PasswordMfaLoginCredentialStoreError storeError)
    PasswordLoginMfaStoreError storeError -> pure (PasswordMfaLoginMfaStoreError storeError)
    PasswordLoginMfaRequired accountId -> completeConfirmedEnrollment context accountId

completeConfirmedEnrollment :: SecondFactorContext -> AccountId -> IO PasswordMfaLoginResult
completeConfirmedEnrollment context accountId = do
  enrollmentResult <- runExceptT (liftMfaStore (loadTotpEnrollment (secondFactorMfaStore context) accountId))
  either
    (pure . infrastructureFailureResult)
    (maybe (pure (PasswordMfaLoginEnrollmentRequired accountId)) (completeStoredEnrollment context accountId))
    enrollmentResult

completeStoredEnrollment :: SecondFactorContext -> AccountId -> StoredTotpEnrollment -> IO PasswordMfaLoginResult
completeStoredEnrollment context accountId enrollment =
  case storedTotpConfirmedAtNanoseconds enrollment of
    Nothing -> pure (PasswordMfaLoginEnrollmentRequired accountId)
    Just _ -> verifyProof context accountId (storedTotpEncryptedSecret enrollment)

verifyProof :: SecondFactorContext -> AccountId -> Text -> IO PasswordMfaLoginResult
verifyProof context accountId encryptedSecret =
  case secondFactorProof context of
    TotpLoginProof suppliedCode ->
      pure (verifyTotpProof context accountId encryptedSecret suppliedCode)
    RecoveryCodeLoginProof suppliedCode ->
      completeRecoveryCode context accountId suppliedCode

verifyTotpProof :: SecondFactorContext -> AccountId -> Text -> TotpCode -> PasswordMfaLoginResult
verifyTotpProof context accountId encryptedSecret suppliedCode =
  case decodeTotpSecret (secondFactorEncryptionKey context) encryptedSecret of
    Nothing -> PasswordMfaLoginCorruptEnrollment
    Just secret ->
      if validateTotpCode (secondFactorNowSeconds context) 1 secret suppliedCode
        then PasswordMfaLoginAccepted accountId
        else PasswordMfaLoginRejected

completeRecoveryCode :: SecondFactorContext -> AccountId -> RecoveryCode -> IO PasswordMfaLoginResult
completeRecoveryCode context accountId suppliedCode = do
  recoveryResult <- runExceptT $ do
    recoveryHashValues <- liftMfaStore (loadUnusedRecoveryCodeHashes (secondFactorMfaStore context) accountId)
    recoveryHashes <-
      fromMaybeError LoginCorruptEnrollment (traverse readRecoveryCodeHash recoveryHashValues)
    case find (verifyRecoveryCode suppliedCode) recoveryHashes of
      Nothing -> pure False
      Just matchingHash ->
        liftMfaStore
          (consumeRecoveryCodeHash (secondFactorMfaStore context) accountId (recoveryCodeHashText matchingHash) (secondFactorNowNanoseconds context))
  pure $
    either
      infrastructureFailureResult
      (\accepted -> if accepted then PasswordMfaLoginAccepted accountId else PasswordMfaLoginRejected)
      recoveryResult

liftMfaStore :: IO (Either MfaStoreError value) -> ExceptT LoginInfrastructureError IO value
liftMfaStore = liftEitherWith LoginMfaStoreError

infrastructureFailureResult :: LoginInfrastructureError -> PasswordMfaLoginResult
infrastructureFailureResult infrastructureError =
  case infrastructureError of
    LoginMfaStoreError storeError -> PasswordMfaLoginMfaStoreError storeError
    LoginCorruptEnrollment -> PasswordMfaLoginCorruptEnrollment

decodeTotpSecret :: SecretEncryptionKey -> Text -> Maybe TotpSecret
decodeTotpSecret encryptionKey encryptedSecret = do
  plaintext <- decryptSecret encryptionKey encryptedSecret
  renderedSecret <- either (const Nothing) Just (TextEncoding.decodeUtf8' plaintext)
  mkTotpSecret renderedSecret
