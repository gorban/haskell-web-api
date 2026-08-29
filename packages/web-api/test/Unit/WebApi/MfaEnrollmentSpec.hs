{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Monad (unless)
import Crypto.Error (CryptoFailable, maybeCryptoError)
import Data.ByteString qualified as ByteString
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Account (AccountId, mkAccountId)
import HarchWeb.Password (defaultPasswordHashingPolicy)
import HarchWeb.RecoveryCode (RecoveryCode, RecoveryCodeHash, generateRecoveryCode, hashRecoveryCode, hashRecoveryCodeWithSalt, mkRecoveryCode)
import HarchWeb.Secret (EncryptionNonce, SecretEncryptionKey, encryptSecret, encryptSecretWithNonce, mkEncryptionNonce, mkSecretEncryptionKey, mkSecretPlaintext)
import HarchWeb.Totp (TotpCode, TotpSecret, generateTotpSecret, mkTotpCode, mkTotpSecret, renderTotpSecret, totpCode)
import WebApi.Mfa (MfaStore (..), MfaStoreError (..), StoredTotpEnrollment (..))
import WebApi.MfaEnrollment (MfaConfirmationEnvironment (..), MfaEnrollmentConfirmation (..), MfaEnrollmentEnvironment (..), MfaEnrollmentError (..), MfaEnrollmentStart (..), confirmMfaEnrollment, confirmMfaEnrollmentWith, startMfaEnrollment)

spec = do
  describe "TOTP enrollment workflow" $ do
    it "uses secure production generators and hashers for enrollment confirmation" $ do
      encryptedSecretReference <- newIORef Nothing
      let store =
            MfaStore
              { saveUnconfirmedTotpEnrollment = \_ encryptedSecret _ -> do
                  writeIORef encryptedSecretReference (Just encryptedSecret)
                  pure (Right True),
                loadTotpEnrollment = \_ -> do
                  encryptedSecret <- readIORef encryptedSecretReference
                  pure (Right (fmap (\secretValue -> StoredTotpEnrollment secretValue Nothing Nothing) encryptedSecret)),
                confirmTotpEnrollment = \receivedAccountId hashes _ ->
                  pure (Right (receivedAccountId == accountId && length hashes == 8 && not (any Text.null hashes))),
                loadUnusedRecoveryCodeHashes = \_ -> pure (error "unexpected recovery-code lookup"),
                consumeRecoveryCodeHash = \_ _ _ -> pure (error "unexpected recovery-code consumption"),
                markTotpCodeUsed = \_ _ -> pure (error "unexpected TOTP counter update")
              }
      started <- startMfaEnrollment (productionStartEnvironment store 100) accountId
      case started of
        Left _ -> expectationFailure "expected production enrollment start to succeed"
        Right (MfaEnrollmentStart secret) -> do
          (MfaEnrollmentStart secret == MfaEnrollmentStart secret) `shouldBe` True
          confirmation <- confirmMfaEnrollment (productionConfirmationEnvironment store) accountId (totpCode 123456 secret)
          case confirmation of
            Left _ -> expectationFailure "expected production enrollment confirmation to succeed"
            Right confirmed -> do
              let recoveryCodes = mfaEnrollmentRecoveryCodes confirmed
              length recoveryCodes `shouldBe` 8
              (confirmed == MfaEnrollmentConfirmation recoveryCodes) `shouldBe` True

    it "encrypts and stores a freshly generated secret only for a verified account" $ do
      savedValuesReference <- newIORef []
      let secret = requiredTotpSecret "JBSWY3DPEHPK3PXPJBSWY3DPEHPK3PXP"
          store =
            MfaStore
              { saveUnconfirmedTotpEnrollment = \savedAccountId encryptedSecret now -> do
                  modifyIORef' savedValuesReference ((savedAccountId, encryptedSecret, now) :)
                  pure (Right True),
                loadTotpEnrollment = \_ -> pure (error "unexpected load"),
                confirmTotpEnrollment = \_ _ _ -> pure (error "unexpected confirmation"),
                loadUnusedRecoveryCodeHashes = \_ -> pure (error "unexpected recovery-code lookup"),
                consumeRecoveryCodeHash = \_ _ _ -> pure (error "unexpected recovery-code consumption"),
                markTotpCodeUsed = \_ _ -> pure (error "unexpected TOTP counter update")
              }
      startMfaEnrollmentWith (pure secret) (\_ plaintext -> if plaintext == "JBSWY3DPEHPK3PXPJBSWY3DPEHPK3PXP" then pure (Just "encrypted-secret") else pure Nothing) store encryptionKey accountId 100
        `shouldReturnEqual` Right (MfaEnrollmentStart secret)
      savedValues <- readIORef savedValuesReference
      savedValues `shouldBe` [(accountId, "encrypted-secret", 100)]

    it "preserves enrollment start encryption, eligibility, and store failures" $ do
      let secret = requiredTotpSecret "JBSWY3DPEHPK3PXPJBSWY3DPEHPK3PXP"
          startWith result = startMfaEnrollmentWith (pure secret) (\_ _ -> pure (Just "encrypted-secret")) (storeWithSave result) encryptionKey accountId 100
      startMfaEnrollmentWith (pure secret) (\_ _ -> pure Nothing) (storeWithSave (Right True)) encryptionKey accountId 100
        `shouldReturnEqual` Left MfaEnrollmentEncryptionFailed
      startWith (Right False) `shouldReturnEqual` Left MfaEnrollmentAccountIsNotEligible
      startWith (Left (MfaStoreUnavailable "unavailable")) `shouldReturnEqual` Left (MfaEnrollmentStoreError (MfaStoreUnavailable "unavailable"))

    it "validates an encrypted secret before atomically confirming it with recovery-code hashes" $ do
      confirmationCallsReference <- newIORef []
      let secret = requiredTotpSecret "JBSWY3DPEHPK3PXPJBSWY3DPEHPK3PXP"
          encryptedSecret = requiredEncryptedSecret secret
          recoveryCodes = requiredRecoveryCode "0123456789ABCDEF0123" :| replicate 7 (requiredRecoveryCode "0123456789ABCDEF0123")
          store =
            MfaStore
              { saveUnconfirmedTotpEnrollment = \_ _ _ -> pure (error "unexpected save"),
                loadTotpEnrollment = \receivedAccountId ->
                  if receivedAccountId == accountId
                    then pure (Right (Just (StoredTotpEnrollment encryptedSecret Nothing Nothing)))
                    else pure (error "unexpected account"),
                confirmTotpEnrollment = \receivedAccountId hashes now -> do
                  modifyIORef' confirmationCallsReference ((receivedAccountId, hashes, now) :)
                  pure (Right True),
                loadUnusedRecoveryCodeHashes = \_ -> pure (error "unexpected recovery-code lookup"),
                consumeRecoveryCodeHash = \_ _ _ -> pure (error "unexpected recovery-code consumption"),
                markTotpCodeUsed = \_ _ -> pure (error "unexpected TOTP counter update")
              }
          hashCode recoveryCode = pure (hashRecoveryCodeWithSalt defaultPasswordHashingPolicy "0123456789abcdef" recoveryCode)
      confirmation <- confirmMfaEnrollmentWith (confirmationEnvironment (nextFrom recoveryCodes) hashCode store) accountId (totpCode 123456 secret)
      if confirmation == Right (MfaEnrollmentConfirmation recoveryCodes)
        then pure ()
        else expectationFailure "expected the encrypted enrollment to be confirmed"
      confirmationCalls <- readIORef confirmationCallsReference
      case confirmationCalls of
        [(receivedAccountId, hashes, now)] -> do
          expectAll
            ( (receivedAccountId `shouldBe` accountId)
                :| [length hashes `shouldBe` 8, now `shouldBe` 500]
            )
        _ -> expectationFailure "expected one confirmation call"

    it "rejects missing, corrupt, already-confirmed, invalid-code, hash, and store outcomes" $ do
      hashCallsReference <- newIORef (0 :: Int)
      let secret = requiredTotpSecret "JBSWY3DPEHPK3PXPJBSWY3DPEHPK3PXP"
          encryptedSecret = requiredEncryptedSecret secret
          suppliedCode = totpCode 123456 secret
          validStore enrollment confirmation =
            MfaStore
              { saveUnconfirmedTotpEnrollment = \_ _ _ -> pure (error "unexpected save"),
                loadTotpEnrollment = \_ -> pure enrollment,
                confirmTotpEnrollment = \_ _ _ -> pure confirmation,
                loadUnusedRecoveryCodeHashes = \_ -> pure (error "unexpected recovery-code lookup"),
                consumeRecoveryCodeHash = \_ _ _ -> pure (error "unexpected recovery-code consumption"),
                markTotpCodeUsed = \_ _ -> pure (error "unexpected TOTP counter update")
              }
          oneCode = requiredRecoveryCode "0123456789ABCDEF0123"
          successfulHash recoveryCode = pure (hashRecoveryCodeWithSalt defaultPasswordHashingPolicy "0123456789abcdef" recoveryCode)
          failingHash _ = modifyIORef' hashCallsReference (+ 1) >> pure Nothing
          confirmWith store generatedCode hashing = confirmMfaEnrollmentWith (confirmationEnvironment generatedCode hashing store) accountId suppliedCode
      confirmWith (validStore (Left (MfaStoreUnavailable "unavailable")) (Right True)) (pure oneCode) successfulHash
        `shouldReturnEqual` Left (MfaEnrollmentStoreError (MfaStoreUnavailable "unavailable"))
      confirmWith (validStore (Right Nothing) (Right True)) (pure oneCode) successfulHash
        `shouldReturnEqual` Left MfaEnrollmentNotFound
      confirmWith (validStore (Right (Just (StoredTotpEnrollment "not-an-envelope" Nothing Nothing))) (Right True)) (pure oneCode) successfulHash
        `shouldReturnEqual` Left MfaEnrollmentCorruptSecret
      confirmWith (validStore (Right (Just (StoredTotpEnrollment (requiredEncryptedPlaintext (ByteString.pack [255])) Nothing Nothing))) (Right True)) (pure oneCode) successfulHash
        `shouldReturnEqual` Left MfaEnrollmentCorruptSecret
      confirmWith (validStore (Right (Just (StoredTotpEnrollment (requiredEncryptedPlaintext "not-a-valid-totp-secret") Nothing Nothing))) (Right True)) (pure oneCode) successfulHash
        `shouldReturnEqual` Left MfaEnrollmentCorruptSecret
      confirmWith (validStore (Right (Just (StoredTotpEnrollment encryptedSecret (Just 1) Nothing))) (Right True)) (pure oneCode) successfulHash
        `shouldReturnEqual` Left MfaEnrollmentConfirmationRejected
      confirmMfaEnrollmentWith (confirmationEnvironment (pure oneCode) successfulHash (validStore (Right (Just (StoredTotpEnrollment encryptedSecret Nothing Nothing))) (Right True))) accountId (requiredTotpCode "000000")
        `shouldReturnEqual` Left MfaEnrollmentInvalidCode
      confirmWith (validStore (Right (Just (StoredTotpEnrollment encryptedSecret Nothing Nothing))) (Right True)) (pure oneCode) failingHash
        `shouldReturnEqual` Left MfaEnrollmentRecoveryCodeHashingFailed
      readIORef hashCallsReference `shouldReturn` 1
      confirmWith (validStore (Right (Just (StoredTotpEnrollment encryptedSecret Nothing Nothing))) (Left (MfaStoreCorruptData "bad confirmation"))) (pure oneCode) successfulHash
        `shouldReturnEqual` Left (MfaEnrollmentStoreError (MfaStoreCorruptData "bad confirmation"))
      confirmWith (validStore (Right (Just (StoredTotpEnrollment encryptedSecret Nothing Nothing))) (Right False)) (pure oneCode) successfulHash
        `shouldReturnEqual` Left MfaEnrollmentConfirmationRejected

    it "keeps enrollment results comparable without rendering their secrets" $ do
      let secret = requiredTotpSecret "JBSWY3DPEHPK3PXPJBSWY3DPEHPK3PXP"
          recoveryCode = requiredRecoveryCode "0123456789ABCDEF0123"
      expectAll
        ( (sameMfaEnrollmentError MfaEnrollmentInvalidCode MfaEnrollmentInvalidCode `shouldBe` True)
            :| [ sameMfaEnrollmentError MfaEnrollmentInvalidCode MfaEnrollmentNotFound `shouldBe` False,
                 MfaEnrollmentInvalidCode /= MfaEnrollmentNotFound `shouldBe` True,
                 sameMfaEnrollmentStart (MfaEnrollmentStart secret) (MfaEnrollmentStart secret) `shouldBe` True,
                 MfaEnrollmentStart secret /= MfaEnrollmentStart (requiredTotpSecret "KRUGS4ZANFZSAYJAON2HE2LOM4XXXXXX") `shouldBe` True,
                 sameMfaEnrollmentConfirmation (MfaEnrollmentConfirmation (recoveryCode :| [])) (MfaEnrollmentConfirmation (recoveryCode :| [])) `shouldBe` True,
                 MfaEnrollmentConfirmation (recoveryCode :| []) /= MfaEnrollmentConfirmation (requiredRecoveryCode "ABCDEF0123456789ABCD" :| []) `shouldBe` True
               ]
        )

shouldReturnEqual :: (Eq value) => IO value -> value -> Expectation
shouldReturnEqual action expected = do
  actual <- action
  unless (actual == expected) (expectationFailure "unexpected result")

storeWithSave :: Either MfaStoreError Bool -> MfaStore
storeWithSave result =
  MfaStore
    { saveUnconfirmedTotpEnrollment = \_ _ _ -> pure result,
      loadTotpEnrollment = \_ -> pure (error "unexpected load"),
      confirmTotpEnrollment = \_ _ _ -> pure (error "unexpected confirmation"),
      loadUnusedRecoveryCodeHashes = \_ -> pure (error "unexpected recovery-code lookup"),
      consumeRecoveryCodeHash = \_ _ _ -> pure (error "unexpected recovery-code consumption"),
      markTotpCodeUsed = \_ _ -> pure (error "unexpected TOTP counter update")
    }

nextFrom :: NonEmpty RecoveryCode -> IO RecoveryCode
nextFrom (firstCode :| _) = pure firstCode

sameMfaEnrollmentError :: MfaEnrollmentError -> MfaEnrollmentError -> Bool
{-# NOINLINE sameMfaEnrollmentError #-}
sameMfaEnrollmentError = (==)

sameMfaEnrollmentStart :: MfaEnrollmentStart -> MfaEnrollmentStart -> Bool
{-# NOINLINE sameMfaEnrollmentStart #-}
sameMfaEnrollmentStart = (==)

sameMfaEnrollmentConfirmation :: MfaEnrollmentConfirmation -> MfaEnrollmentConfirmation -> Bool
{-# NOINLINE sameMfaEnrollmentConfirmation #-}
sameMfaEnrollmentConfirmation = (==)

requiredTotpSecret :: Text.Text -> TotpSecret
requiredTotpSecret value =
  case mkTotpSecret value of
    Just secret -> secret
    Nothing -> error "expected a valid TOTP secret"

requiredTotpCode :: Text.Text -> TotpCode
requiredTotpCode value =
  case mkTotpCode value of
    Just code -> code
    Nothing -> error "expected a valid TOTP code"

requiredRecoveryCode :: Text.Text -> RecoveryCode
requiredRecoveryCode value =
  case mkRecoveryCode value of
    Just code -> code
    Nothing -> error "expected a valid recovery code"

requiredEncryptedSecret :: TotpSecret -> Text.Text
requiredEncryptedSecret secret =
  requiredEncryptedPlaintext (TextEncoding.encodeUtf8 (renderTotpSecret secret))

requiredEncryptedPlaintext :: ByteString.ByteString -> Text.Text
requiredEncryptedPlaintext plaintext =
  requiredCrypto (encryptSecretWithNonce encryptionKey (requiredEncryptionNonce "123456789012") (mkSecretPlaintext plaintext))

requiredCrypto :: CryptoFailable value -> value
requiredCrypto = fromMaybe (error "expected cryptographic operation to succeed") . maybeCryptoError

requiredEncryptionNonce :: ByteString.ByteString -> EncryptionNonce
requiredEncryptionNonce value =
  case mkEncryptionNonce value of
    Just nonce -> nonce
    Nothing -> error "expected a valid encryption nonce"

encryptionKey :: SecretEncryptionKey
encryptionKey =
  case mkSecretEncryptionKey "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" of
    Just key -> key
    Nothing -> error "expected a valid secret encryption key"

confirmationEnvironment :: IO RecoveryCode -> (RecoveryCode -> IO (Maybe RecoveryCodeHash)) -> MfaStore -> MfaConfirmationEnvironment
confirmationEnvironment generateCode hashCode store =
  MfaConfirmationEnvironment
    { mfaConfirmationGenerateCode = generateCode,
      mfaConfirmationHashCode = hashCode,
      mfaConfirmationStore = store,
      mfaConfirmationEncryptionKey = encryptionKey,
      mfaConfirmationNowNanoseconds = 500,
      mfaConfirmationNowSeconds = 123456
    }

productionStartEnvironment :: MfaStore -> Integer -> MfaEnrollmentEnvironment
productionStartEnvironment store now =
  MfaEnrollmentEnvironment
    { mfaEnrollmentGenerateSecret = generateTotpSecret,
      mfaEnrollmentEncryptSecret = \key plaintext -> maybeCryptoError <$> encryptSecret key plaintext,
      mfaEnrollmentStore = store,
      mfaEnrollmentEncryptionKey = encryptionKey,
      mfaEnrollmentNowNanoseconds = fromIntegral now
    }

productionConfirmationEnvironment :: MfaStore -> MfaConfirmationEnvironment
productionConfirmationEnvironment store =
  MfaConfirmationEnvironment
    { mfaConfirmationGenerateCode = generateRecoveryCode,
      mfaConfirmationHashCode = hashRecoveryCode defaultPasswordHashingPolicy,
      mfaConfirmationStore = store,
      mfaConfirmationEncryptionKey = encryptionKey,
      mfaConfirmationNowNanoseconds = 500,
      mfaConfirmationNowSeconds = 123456
    }

accountId :: AccountId
accountId =
  case mkAccountId "account_01" of
    Just value -> value
    Nothing -> error "expected a valid account id"

startMfaEnrollmentWith :: IO TotpSecret -> (SecretEncryptionKey -> ByteString.ByteString -> IO (Maybe Text.Text)) -> MfaStore -> SecretEncryptionKey -> AccountId -> Integer -> IO (Either MfaEnrollmentError MfaEnrollmentStart)
startMfaEnrollmentWith generateSecret encrypt store key enrollmentAccountId now =
  startMfaEnrollment
    MfaEnrollmentEnvironment
      { mfaEnrollmentGenerateSecret = generateSecret,
        mfaEnrollmentEncryptSecret = encrypt,
        mfaEnrollmentStore = store,
        mfaEnrollmentEncryptionKey = key,
        mfaEnrollmentNowNanoseconds = fromIntegral now
      }
    enrollmentAccountId
