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

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Core.Control.Error (fromMaybeError, guardError, liftEitherWith, liftMaybeWith)
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
import HarchWeb.Secret (SecretEncryptionKey, decryptSecretText, encryptSecret)
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
startMfaEnrollmentWith generateSecret encrypt mfaStore encryptionKey accountId now =
  runExceptT $ do
    (secret, encryptedSecret) <-
      liftMaybeWith MfaEnrollmentEncryptionFailed
        (generateEncryptedSecret generateSecret encrypt encryptionKey)
    saved <- liftMfaStore (saveUnconfirmedTotpEnrollment mfaStore accountId encryptedSecret now)
    guardError MfaEnrollmentAccountIsNotEligible saved
    pure (MfaEnrollmentStart secret)

generateEncryptedSecret ::
  IO TotpSecret ->
  (SecretEncryptionKey -> ByteString.ByteString -> IO (Maybe Text)) ->
  SecretEncryptionKey ->
  IO (Maybe (TotpSecret, Text))
generateEncryptedSecret generateSecret encrypt encryptionKey = do
  secret <- generateSecret
  encryptedSecret <-
    encrypt encryptionKey (TextEncoding.encodeUtf8 (renderTotpSecret secret))
  pure (fmap (pairTotpSecret secret) encryptedSecret)

pairTotpSecret :: TotpSecret -> Text -> (TotpSecret, Text)
pairTotpSecret = (,)

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
confirmMfaEnrollmentWith generateCode hashCode mfaStore encryptionKey accountId nowNanoseconds nowSeconds suppliedCode =
  runExceptT $ do
    enrollment <-
      liftMfaStore (loadTotpEnrollment mfaStore accountId)
        >>= fromMaybeError MfaEnrollmentNotFound
    encryptedSecret <- requireUnconfirmedEnrollment enrollment
    secret <- fromMaybeError MfaEnrollmentCorruptSecret (decodeSecret encryptionKey encryptedSecret)
    guardError MfaEnrollmentInvalidCode (validateTotpCode nowSeconds 1 secret suppliedCode)
    recoveryCodes <- liftIO (generateRecoveryCodes generateCode)
    recoveryCodeHashes <- traverse hashGeneratedRecoveryCode recoveryCodes
    confirmed <- liftMfaStore (confirmTotpEnrollment mfaStore accountId recoveryCodeHashes nowNanoseconds)
    guardError MfaEnrollmentConfirmationRejected confirmed
    pure (MfaEnrollmentConfirmation recoveryCodes)
  where
    hashGeneratedRecoveryCode recoveryCode =
      recoveryCodeHashText
        <$> liftMaybeWith MfaEnrollmentRecoveryCodeHashingFailed (hashCode recoveryCode)

liftMfaStore :: IO (Either MfaStoreError value) -> ExceptT MfaEnrollmentError IO value
liftMfaStore = liftEitherWith MfaEnrollmentStoreError

requireUnconfirmedEnrollment :: StoredTotpEnrollment -> ExceptT MfaEnrollmentError IO Text
requireUnconfirmedEnrollment enrollment =
  case storedTotpConfirmedAtNanoseconds enrollment of
    Just _ -> throwError MfaEnrollmentConfirmationRejected
    Nothing -> pure (storedTotpEncryptedSecret enrollment)

decodeSecret :: SecretEncryptionKey -> Text -> Maybe TotpSecret
decodeSecret encryptionKey encryptedSecret =
  mkTotpSecret =<< decryptSecretText encryptionKey encryptedSecret

generateRecoveryCodes :: IO RecoveryCode -> IO (NonEmpty RecoveryCode)
generateRecoveryCodes generateCode = do
  firstCode <- generateCode
  remainingCodes <- traverse (const generateCode) [1 .. recoveryCodeCount - 1]
  pure (firstCode :| remainingCodes)

recoveryCodeCount :: Int
recoveryCodeCount = 8
