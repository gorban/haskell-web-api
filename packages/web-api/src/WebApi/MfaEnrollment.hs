-- | MFA enrollment workflows.
--
-- Decision (FQ6, 2026-08-29): start and confirmation accept explicit
-- environment records. This retains one effect interpretation per workflow,
-- makes production cryptographic dependencies visible at construction, and
-- gives deterministic tests the same path rather than a parallel protocol.
module WebApi.MfaEnrollment
  ( MfaConfirmationEnvironment (..),
    MfaEnrollmentEnvironment (..),
    MfaEnrollmentConfirmation (..),
    MfaEnrollmentError (..),
    MfaEnrollmentStart (..),
    confirmMfaEnrollment,
    confirmMfaEnrollmentWith,
    startMfaEnrollment,
  )
where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Core.Control.Error (fromMaybeError, guardError, liftEitherWith, liftMaybeWith)
import Crypto.Error (maybeCryptoError)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Account (AccountId)
import HarchWeb.RecoveryCode
  ( RecoveryCode,
    RecoveryCodeHash,
    recoveryCodeHashText,
  )
import HarchWeb.Secret (SecretEncryptionKey, decryptSecretText)
import HarchWeb.Time (UnixTimeNanoseconds, UnixTimeSeconds)
import HarchWeb.Totp
  ( TotpCode,
    TotpSecret,
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

-- | Invariant dependencies of a start attempt. Production callers use
-- 'generateTotpSecret' and 'encryptSecret'; tests supply the same record with
-- deterministic effects, rather than a second positional API.
data MfaEnrollmentEnvironment = MfaEnrollmentEnvironment
  { mfaEnrollmentGenerateSecret :: IO TotpSecret,
    mfaEnrollmentEncryptSecret :: SecretEncryptionKey -> ByteString.ByteString -> IO (Maybe Text),
    mfaEnrollmentStore :: MfaStore,
    mfaEnrollmentEncryptionKey :: SecretEncryptionKey,
    mfaEnrollmentNowNanoseconds :: UnixTimeNanoseconds
  }

startMfaEnrollment :: MfaEnrollmentEnvironment -> AccountId -> IO (Either MfaEnrollmentError MfaEnrollmentStart)
startMfaEnrollment environment accountId =
  runExceptT $ do
    (secret, encryptedSecret) <-
      liftMaybeWith
        MfaEnrollmentEncryptionFailed
        (generateEncryptedSecret generateSecret encrypt encryptionKey)
    saved <- liftMfaStore (saveUnconfirmedTotpEnrollment mfaStore accountId encryptedSecret now)
    guardError MfaEnrollmentAccountIsNotEligible saved
    pure (MfaEnrollmentStart secret)
  where
    generateSecret = mfaEnrollmentGenerateSecret environment
    encrypt = mfaEnrollmentEncryptSecret environment
    mfaStore = mfaEnrollmentStore environment
    encryptionKey = mfaEnrollmentEncryptionKey environment
    now = mfaEnrollmentNowNanoseconds environment

generateEncryptedSecret ::
  IO TotpSecret ->
  (SecretEncryptionKey -> ByteString.ByteString -> IO (Maybe Text)) ->
  SecretEncryptionKey ->
  IO (Maybe (TotpSecret, Text))
generateEncryptedSecret generateSecret encrypt encryptionKey = do
  secret <- generateSecret
  encryptedSecret <-
    encrypt encryptionKey (TextEncoding.encodeUtf8 (renderTotpSecret secret))
  pure (pairTotpSecret secret <$> encryptedSecret)

pairTotpSecret :: TotpSecret -> Text -> (TotpSecret, Text)
pairTotpSecret = (,)

-- | The dependencies one enrollment confirmation runs against: how to
-- generate and hash recovery codes, where enrollments live, the TOTP
-- decryption key, and the clock. Grouping these leaves only the two inputs
-- that vary per confirmation attempt, the account and the supplied code, as
-- positional arguments.
data MfaConfirmationEnvironment = MfaConfirmationEnvironment
  { mfaConfirmationGenerateCode :: IO RecoveryCode,
    mfaConfirmationHashCode :: RecoveryCode -> IO (Maybe RecoveryCodeHash),
    mfaConfirmationStore :: MfaStore,
    mfaConfirmationEncryptionKey :: SecretEncryptionKey,
    mfaConfirmationNowNanoseconds :: UnixTimeNanoseconds,
    mfaConfirmationNowSeconds :: UnixTimeSeconds
  }

confirmMfaEnrollment ::
  MfaConfirmationEnvironment ->
  AccountId ->
  TotpCode ->
  IO (Either MfaEnrollmentError MfaEnrollmentConfirmation)
confirmMfaEnrollment environment accountId suppliedCode =
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
    generateCode = mfaConfirmationGenerateCode environment
    hashCode = mfaConfirmationHashCode environment
    mfaStore = mfaConfirmationStore environment
    encryptionKey = mfaConfirmationEncryptionKey environment
    nowNanoseconds = mfaConfirmationNowNanoseconds environment
    nowSeconds = mfaConfirmationNowSeconds environment
    hashGeneratedRecoveryCode recoveryCode =
      recoveryCodeHashText
        <$> liftMaybeWith MfaEnrollmentRecoveryCodeHashingFailed (hashCode recoveryCode)

-- | Compatibility spelling for adapters that already carry a confirmation
-- environment. It has the same record-first shape as 'confirmMfaEnrollment'.
confirmMfaEnrollmentWith ::
  MfaConfirmationEnvironment ->
  AccountId ->
  TotpCode ->
  IO (Either MfaEnrollmentError MfaEnrollmentConfirmation)
confirmMfaEnrollmentWith = confirmMfaEnrollment

liftMfaStore :: IO (Either MfaStoreError value) -> ExceptT MfaEnrollmentError IO value
liftMfaStore = liftEitherWith MfaEnrollmentStoreError

requireUnconfirmedEnrollment :: StoredTotpEnrollment -> ExceptT MfaEnrollmentError IO Text
requireUnconfirmedEnrollment enrollment =
  case storedTotpConfirmedAtNanoseconds enrollment of
    Just _ -> throwError MfaEnrollmentConfirmationRejected
    Nothing -> pure (storedTotpEncryptedSecret enrollment)

decodeSecret :: SecretEncryptionKey -> Text -> Maybe TotpSecret
decodeSecret encryptionKey encryptedSecret =
  maybeCryptoError (decryptSecretText encryptionKey encryptedSecret) >>= either (const Nothing) mkTotpSecret

generateRecoveryCodes :: IO RecoveryCode -> IO (NonEmpty RecoveryCode)
generateRecoveryCodes generateCode = do
  firstCode <- generateCode
  remainingCodes <- traverse (const generateCode) [1 .. recoveryCodeCount - 1]
  pure (firstCode :| remainingCodes)

recoveryCodeCount :: Int
recoveryCodeCount = 8
