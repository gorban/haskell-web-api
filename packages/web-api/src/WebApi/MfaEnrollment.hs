module WebApi.MfaEnrollment
  ( MfaEnrollmentConfirmation (..),
    MfaEnrollmentError (..),
    MfaEnrollmentStart (..),
    confirmMfaEnrollment,
    confirmMfaEnrollmentWith,
    startMfaEnrollment,
    startMfaEnrollmentWith,
  )
where

import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word64)
import HarchWeb.Account (AccountId)
import HarchWeb.Password (PasswordHashingPolicy)
import HarchWeb.RecoveryCode
  ( RecoveryCode,
    RecoveryCodeHash,
    generateRecoveryCode,
    hashRecoveryCode,
    recoveryCodeHashText,
  )
import HarchWeb.Secret (SecretEncryptionKey, decryptSecret, encryptSecret)
import HarchWeb.Totp
  ( TotpCode,
    TotpSecret,
    generateTotpSecret,
    mkTotpSecret,
    renderTotpSecret,
    validateTotpCode,
  )
import WebApi.Mfa
  ( MfaStore (..),
    MfaStoreError,
    StoredTotpEnrollment (..),
  )

data MfaEnrollmentError
  = MfaEnrollmentStoreError MfaStoreError
  | MfaEnrollmentAccountIsNotEligible
  | MfaEnrollmentNotFound
  | MfaEnrollmentCorruptSecret
  | MfaEnrollmentInvalidCode
  | MfaEnrollmentConfirmationRejected
  | MfaEnrollmentRecoveryCodeHashingFailed
  | MfaEnrollmentEncryptionFailed
  deriving (Eq)

newtype MfaEnrollmentStart = MfaEnrollmentStart
  { mfaEnrollmentStartSecret :: TotpSecret
  }
  deriving (Eq)

newtype MfaEnrollmentConfirmation = MfaEnrollmentConfirmation
  { mfaEnrollmentRecoveryCodes :: NonEmpty RecoveryCode
  }
  deriving (Eq)

startMfaEnrollment :: MfaStore -> SecretEncryptionKey -> AccountId -> Word64 -> IO (Either MfaEnrollmentError MfaEnrollmentStart)
startMfaEnrollment =
  startMfaEnrollmentWith generateTotpSecret encryptSecret

startMfaEnrollmentWith ::
  IO TotpSecret ->
  (SecretEncryptionKey -> ByteString.ByteString -> IO (Maybe Text)) ->
  MfaStore ->
  SecretEncryptionKey ->
  AccountId ->
  Word64 ->
  IO (Either MfaEnrollmentError MfaEnrollmentStart)
startMfaEnrollmentWith generateSecret encrypt mfaStore encryptionKey accountId now = do
  secret <- generateSecret
  encryptedSecret <- encrypt encryptionKey (TextEncoding.encodeUtf8 (renderTotpSecret secret))
  case encryptedSecret of
    Nothing -> pure (Left MfaEnrollmentEncryptionFailed)
    Just encryptedSecretValue -> do
      saveResult <- saveUnconfirmedTotpEnrollment mfaStore accountId encryptedSecretValue now
      pure $
        case saveResult of
          Left storeError -> Left (MfaEnrollmentStoreError storeError)
          Right False -> Left MfaEnrollmentAccountIsNotEligible
          Right True -> Right (MfaEnrollmentStart secret)

confirmMfaEnrollment ::
  PasswordHashingPolicy ->
  MfaStore ->
  SecretEncryptionKey ->
  AccountId ->
  Word64 ->
  Word64 ->
  TotpCode ->
  IO (Either MfaEnrollmentError MfaEnrollmentConfirmation)
confirmMfaEnrollment passwordHashingPolicy =
  confirmMfaEnrollmentWith generateRecoveryCode (hashRecoveryCode passwordHashingPolicy)

confirmMfaEnrollmentWith ::
  IO RecoveryCode ->
  (RecoveryCode -> IO (Maybe RecoveryCodeHash)) ->
  MfaStore ->
  SecretEncryptionKey ->
  AccountId ->
  Word64 ->
  Word64 ->
  TotpCode ->
  IO (Either MfaEnrollmentError MfaEnrollmentConfirmation)
confirmMfaEnrollmentWith generateCode hashCode mfaStore encryptionKey accountId nowNanoseconds nowSeconds suppliedCode = do
  enrollmentResult <- loadTotpEnrollment mfaStore accountId
  case enrollmentResult of
    Left storeError -> pure (Left (MfaEnrollmentStoreError storeError))
    Right Nothing -> pure (Left MfaEnrollmentNotFound)
    Right (Just StoredTotpEnrollment {storedTotpConfirmedAtNanoseconds = Just _}) -> pure (Left MfaEnrollmentConfirmationRejected)
    Right (Just StoredTotpEnrollment {storedTotpEncryptedSecret, storedTotpConfirmedAtNanoseconds = Nothing}) ->
      case decodeSecret encryptionKey storedTotpEncryptedSecret of
        Nothing -> pure (Left MfaEnrollmentCorruptSecret)
        Just secret ->
          if not (validateTotpCode nowSeconds 1 secret suppliedCode)
            then pure (Left MfaEnrollmentInvalidCode)
            else do
              recoveryCodes <- generateRecoveryCodes generateCode
              recoveryCodeHashes <- traverse hashCode recoveryCodes
              case traverse (fmap recoveryCodeHashText) recoveryCodeHashes of
                Nothing -> pure (Left MfaEnrollmentRecoveryCodeHashingFailed)
                Just recoveryCodeHashesText -> do
                  confirmationResult <- confirmTotpEnrollment mfaStore accountId recoveryCodeHashesText nowNanoseconds
                  pure $
                    case confirmationResult of
                      Left storeError -> Left (MfaEnrollmentStoreError storeError)
                      Right False -> Left MfaEnrollmentConfirmationRejected
                      Right True -> Right (MfaEnrollmentConfirmation recoveryCodes)

decodeSecret :: SecretEncryptionKey -> Text -> Maybe TotpSecret
decodeSecret encryptionKey encryptedSecret = do
  plaintext <- decryptSecret encryptionKey encryptedSecret
  renderedSecret <- either (const Nothing) Just (TextEncoding.decodeUtf8' plaintext)
  mkTotpSecret renderedSecret

generateRecoveryCodes :: IO RecoveryCode -> IO (NonEmpty RecoveryCode)
generateRecoveryCodes generateCode = do
  firstCode <- generateCode
  remainingCodes <- traverse (const generateCode) [1 .. recoveryCodeCount - 1]
  pure (firstCode :| remainingCodes)

recoveryCodeCount :: Int
recoveryCodeCount = 8
