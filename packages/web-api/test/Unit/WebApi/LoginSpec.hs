{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (AsyncException (ThreadKilled), ErrorCall (..), SomeException, evaluate, throwTo, try)
import Crypto.Error (CryptoFailable, maybeCryptoError)
import Data.ByteString qualified as ByteString
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Account (accountIdText)
import HarchWeb.LoginProtection (defaultLoginProtectionPolicy, loginProtectionLockoutNanoseconds, loginProtectionWindowNanoseconds)
import HarchWeb.Password (PasswordHash (..), argon2Iterations, argon2MemoryKib, argon2Parallelism, defaultPasswordHashingPolicy, hashPasswordWithSalt, mkPassword, mkPasswordHashingPolicy, mkPasswordWorkBudget, newPasswordWorkGate, passwordHashText, withPasswordWork)
import HarchWeb.RecoveryCode (hashRecoveryCodeWithSalt, mkRecoveryCode, recoveryCodeHashText)
import HarchWeb.Secret (SecretEncryptionKey, encryptSecretWithNonce, mkEncryptionNonce, mkSecretEncryptionKey, mkSecretPlaintext)
import HarchWeb.Time (unixTimeNanoseconds)
import HarchWeb.Totp (mkTotpCode, mkTotpSecret, renderTotpSecret, totpCode)
import HarchWeb.Username (mkUsername)
import Unit.WebApi.TestSupport (accountId, emailAddress, required, shouldReturnEqual, testPasswordWorkGate)
import WebApi.Login
import WebApi.Mfa (MfaStore (..), MfaStoreError (..), StoredTotpEnrollment (..))

spec = do
  describe "password-first MFA login" $ do
    it "requires a confirmed authenticator only after a correct verified password" $ do
      beginPasswordLogin (credentialStore (Right (Just verifiedCredential))) confirmedMfaStore permissiveThrottle testPasswordWorkGate emailAddress (mkPassword "correct horse battery staple")
        `shouldReturnEqual` PasswordLoginMfaRequired accountId
      beginPasswordLogin (credentialStore (Right (Just verifiedCredential))) unexpectedMfaStore permissiveThrottle testPasswordWorkGate emailAddress (mkPassword "incorrect password")
        `shouldReturnEqual` PasswordLoginRejected
      beginPasswordLogin (credentialStore (Right Nothing)) unexpectedMfaStore permissiveThrottle testPasswordWorkGate emailAddress (mkPassword "correct horse battery staple")
        `shouldReturnEqual` PasswordLoginRejected

    it "directs verified accounts without a confirmed enrollment back to MFA setup" $ do
      beginPasswordLogin (credentialStore (Right (Just verifiedCredential))) (mfaStore (Right Nothing)) permissiveThrottle testPasswordWorkGate emailAddress (mkPassword "correct horse battery staple")
        `shouldReturnEqual` PasswordLoginMfaEnrollmentRequired accountId
      beginPasswordLogin (credentialStore (Right (Just verifiedCredential))) (mfaStore (Right (Just (StoredTotpEnrollment "encrypted" Nothing Nothing)))) permissiveThrottle testPasswordWorkGate emailAddress (mkPassword "correct horse battery staple")
        `shouldReturnEqual` PasswordLoginMfaEnrollmentRequired accountId
      beginPasswordLogin (credentialStore (Right (Just unverifiedCredential))) unexpectedMfaStore permissiveThrottle testPasswordWorkGate emailAddress (mkPassword "correct horse battery staple")
        `shouldReturnEqual` PasswordLoginEmailVerificationRequired accountId

    it "preserves credential and MFA persistence failures" $ do
      beginPasswordLogin (credentialStore (Left (AccountCredentialStoreUnavailable "database unavailable"))) unexpectedMfaStore permissiveThrottle testPasswordWorkGate emailAddress (mkPassword "correct horse battery staple")
        `shouldReturnEqual` PasswordLoginCredentialStoreError (AccountCredentialStoreUnavailable "database unavailable")
      beginPasswordLogin (credentialStore (Right (Just verifiedCredential))) (mfaStore (Left (MfaStoreCorruptData "bad enrollment"))) permissiveThrottle testPasswordWorkGate emailAddress (mkPassword "correct horse battery staple")
        `shouldReturnEqual` PasswordLoginMfaStoreError (MfaStoreCorruptData "bad enrollment")

    it "reports exhausted password work without running or charging the credential check" $ do
      passwordWorkGate <- newPasswordWorkGate (required "password-work budget" (mkPasswordWorkBudget 8))
      beginPasswordLogin (credentialStore (Right Nothing)) unexpectedMfaStore permissiveThrottle passwordWorkGate emailAddress (mkPassword "correct horse battery staple")
        `shouldReturnEqual` PasswordLoginPasswordWorkBudgetExhausted

    it "keeps unknown-account verification on the fixed timing-defense path" $ do
      beginPasswordLogin (credentialStore (Right Nothing)) unexpectedMfaStore permissiveThrottle testPasswordWorkGate emailAddress (mkPassword "login-existence-oracle-defense")
        `shouldReturnEqual` PasswordLoginRejected

    it "rejects an externally malformed stored password hash without native password work" $ do
      beginPasswordLogin (credentialStore (Right (Just (AccountCredential accountId (PasswordHash "malformed") True)))) unexpectedMfaStore permissiveThrottle testPasswordWorkGate emailAddress (mkPassword "correct horse battery staple")
        `shouldReturnEqual` PasswordLoginRejected

    it "upgrades a verified weaker hash once, without making a failed best-effort update a login failure" $ do
      let password = mkPassword "correct horse battery staple"
          legacyPolicy = required "legacy hashing policy" (mkPasswordHashingPolicy (argon2Iterations 1) (argon2MemoryKib 8) (argon2Parallelism 1))
          legacyHash = required "legacy password hash" (hashPasswordWithSalt legacyPolicy "0123456789abcdef" password)
          legacyCredential = AccountCredential accountId legacyHash True
      storedHashReference <- newIORef legacyHash
      replacementCallsReference <- newIORef (0 :: Int)
      let upgradingStore =
            AccountCredentialStore
              ( \_ -> do
                  currentHash <- readIORef storedHashReference
                  pure (Right (Just (AccountCredential accountId currentHash True)))
              )
              (\_ -> error "unexpected username lookup")
              ( \receivedAccountId expectedHash replacementHash -> do
                  currentHash <- readIORef storedHashReference
                  if receivedAccountId == accountId && passwordHashText currentHash == passwordHashText expectedHash
                    then do
                      modifyIORef' replacementCallsReference (+ 1)
                      writeIORef storedHashReference replacementHash
                      pure (Right True)
                    else pure (Right False)
              )
      beginPasswordLogin upgradingStore confirmedMfaStore permissiveThrottle testPasswordWorkGate emailAddress password
        `shouldReturnEqual` PasswordLoginMfaRequired accountId
      beginPasswordLogin upgradingStore confirmedMfaStore permissiveThrottle testPasswordWorkGate emailAddress password
        `shouldReturnEqual` PasswordLoginMfaRequired accountId
      readIORef replacementCallsReference `shouldReturn` 1
      upgradedHash <- readIORef storedHashReference
      passwordHashText upgradedHash `shouldNotBe` passwordHashText legacyHash

      failedUpdateCallsReference <- newIORef (0 :: Int)
      let failedUpdateStore =
            (credentialStore (Right (Just legacyCredential)))
              { replacePasswordHashIfCurrent = \_ _ _ -> modifyIORef' failedUpdateCallsReference (+ 1) >> pure (Left (AccountCredentialStoreUnavailable "database unavailable"))
              }
      beginPasswordLogin failedUpdateStore confirmedMfaStore permissiveThrottle testPasswordWorkGate emailAddress password
        `shouldReturnEqual` PasswordLoginMfaRequired accountId
      readIORef failedUpdateCallsReference `shouldReturn` 1

      failedHashCallsReference <- newIORef (0 :: Int)
      let failedHashStore =
            (credentialStore (Right (Just legacyCredential)))
              { replacePasswordHashIfCurrent = \_ _ _ -> modifyIORef' failedHashCallsReference (+ 1) >> pure (Right True)
              }
          failedRehasher = PasswordRehasher defaultPasswordHashingPolicy (\_ _ -> pure Nothing)
      beginPasswordLoginWithIdentifierAndRehasher failedRehasher failedHashStore confirmedMfaStore permissiveThrottle testPasswordWorkGate (LoginEmailAddress emailAddress) password
        `shouldReturnEqual` PasswordLoginMfaRequired accountId
      readIORef failedHashCallsReference `shouldReturn` 0

      gate <- newPasswordWorkGate (required "rehash-admission budget" (mkPasswordWorkBudget 65544))
      holderStarted <- newEmptyMVar
      holderRelease <- newEmptyMVar
      holderFinished <- newEmptyMVar
      _ <- forkIO (withPasswordWork gate 65536 (putMVar holderStarted () >> takeMVar holderRelease) >>= putMVar holderFinished)
      takeMVar holderStarted
      unavailableRehashCallsReference <- newIORef (0 :: Int)
      let unavailableRehashStore =
            (credentialStore (Right (Just legacyCredential)))
              { replacePasswordHashIfCurrent = \_ _ _ -> modifyIORef' unavailableRehashCallsReference (+ 1) >> pure (Right True)
              }
      beginPasswordLogin unavailableRehashStore confirmedMfaStore permissiveThrottle gate emailAddress password
        `shouldReturnEqual` PasswordLoginMfaRequired accountId
      readIORef unavailableRehashCallsReference `shouldReturn` 0
      putMVar holderRelease ()
      takeMVar holderFinished `shouldReturn` Just ()

    it "accepts a validated TOTP only after validating the password" $ do
      let secret = required "TOTP secret" (mkTotpSecret "JBSWY3DPEHPK3PXPJBSWY3DPEHPK3PXP")
          encryptedSecret = requiredCrypto (encryptSecretWithNonce encryptionKey (required "encryption nonce" (mkEncryptionNonce (ByteString.replicate 12 7))) (mkSecretPlaintext (TextEncoding.encodeUtf8 (renderTotpSecret secret))))
          confirmedStore = mfaStore (Right (Just (StoredTotpEnrollment encryptedSecret (Just 100) Nothing)))
          validProof = TotpLoginProof (totpCode 123456 secret)
      completePasswordLogin (credentialStore (Right (Just verifiedCredential))) (secondFactorContextFor confirmedStore validProof) emailAddress (mkPassword "correct horse battery staple")
        `shouldReturnEqual` PasswordMfaLoginAccepted accountId
      completePasswordLogin (credentialStore (Right (Just verifiedCredential))) (secondFactorContextFor confirmedStore (TotpLoginProof (totpCode (123456 - 30) secret))) emailAddress (mkPassword "correct horse battery staple")
        `shouldReturnEqual` PasswordMfaLoginAccepted accountId
      completePasswordLogin (credentialStore (Right (Just verifiedCredential))) (secondFactorContextFor confirmedStore (TotpLoginProof (totpCode (123456 + 30) secret))) emailAddress (mkPassword "correct horse battery staple")
        `shouldReturnEqual` PasswordMfaLoginAccepted accountId
      completePasswordLogin (credentialStore (Right (Just verifiedCredential))) (secondFactorContextFor confirmedStore (TotpLoginProof (totpCode (123456 + 60) secret))) emailAddress (mkPassword "correct horse battery staple")
        `shouldReturnEqual` PasswordMfaLoginRejected
      completePasswordLogin (credentialStore (Right (Just verifiedCredential))) (secondFactorContextFor confirmedStore validProof) emailAddress (mkPassword "incorrect password")
        `shouldReturnEqual` PasswordMfaLoginRejected
      completePasswordLogin (credentialStore (Right (Just verifiedCredential))) (secondFactorContextFor confirmedStore (TotpLoginProof (required "invalid TOTP code" (mkTotpCode "000000")))) emailAddress (mkPassword "correct horse battery staple")
        `shouldReturnEqual` PasswordMfaLoginRejected

    it "rejects a replayed TOTP code without ever consulting the store again, and closes the race with an atomic mark" $ do
      let secret = required "TOTP secret" (mkTotpSecret "JBSWY3DPEHPK3PXPJBSWY3DPEHPK3PXP")
          encryptedSecret = requiredCrypto (encryptSecretWithNonce encryptionKey (required "encryption nonce" (mkEncryptionNonce (ByteString.replicate 12 7))) (mkSecretPlaintext (TextEncoding.encodeUtf8 (renderTotpSecret secret))))
          validProof = TotpLoginProof (totpCode 123456 secret)
          -- The counter 'totpCode 123456 secret' matches at nowSeconds=123456
          -- (see 'secondFactorContextFor') is @123456 \`div\` 30@.
          matchedCounter = 123456 `div` 30
          storeWithLastUsed lastUsed =
            (mfaStore (Right (Just (StoredTotpEnrollment encryptedSecret (Just 100) lastUsed))))
              { markTotpCodeUsed = \_ _ -> error "unexpected TOTP counter update while already replayed"
              }
      completePasswordLogin (credentialStore (Right (Just verifiedCredential))) (secondFactorContextFor (storeWithLastUsed (Just matchedCounter)) validProof) emailAddress (mkPassword "correct horse battery staple")
        `shouldReturnEqual` PasswordMfaLoginRejected
      completePasswordLogin (credentialStore (Right (Just verifiedCredential))) (secondFactorContextFor (storeWithLastUsed (Just (matchedCounter + 1))) validProof) emailAddress (mkPassword "correct horse battery staple")
        `shouldReturnEqual` PasswordMfaLoginRejected
      markCallsReference <- newIORef []
      let acceptingStore =
            (mfaStore (Right (Just (StoredTotpEnrollment encryptedSecret (Just 100) (Just (matchedCounter - 1))))))
              { markTotpCodeUsed = \markedAccountId markedCounter -> do
                  modifyIORef' markCallsReference ((markedAccountId, markedCounter) :)
                  pure (Right True)
              }
      completePasswordLogin (credentialStore (Right (Just verifiedCredential))) (secondFactorContextFor acceptingStore validProof) emailAddress (mkPassword "correct horse battery staple")
        `shouldReturnEqual` PasswordMfaLoginAccepted accountId
      readIORef markCallsReference `shouldReturn` [(accountId, matchedCounter)]
      let racedStore =
            (mfaStore (Right (Just (StoredTotpEnrollment encryptedSecret (Just 100) (Just (matchedCounter - 1))))))
              { markTotpCodeUsed = \_ _ -> pure (Right False)
              }
      completePasswordLogin (credentialStore (Right (Just verifiedCredential))) (secondFactorContextFor racedStore validProof) emailAddress (mkPassword "correct horse battery staple")
        `shouldReturnEqual` PasswordMfaLoginRejected
      let unavailableMarkStore =
            (mfaStore (Right (Just (StoredTotpEnrollment encryptedSecret (Just 100) (Just (matchedCounter - 1))))))
              { markTotpCodeUsed = \_ _ -> pure (Left (MfaStoreUnavailable "counter store down"))
              }
      completePasswordLogin (credentialStore (Right (Just verifiedCredential))) (secondFactorContextFor unavailableMarkStore validProof) emailAddress (mkPassword "correct horse battery staple")
        `shouldReturnEqual` PasswordMfaLoginMfaStoreError (MfaStoreUnavailable "counter store down")

    it "consumes a matched recovery-code hash atomically without exposing a reusable code" $ do
      consumedHashReference <- newIORef Nothing
      let recoveryCode = required "recovery code" (mkRecoveryCode "0123456789ABCDEF0123")
          recoveryCodeHash = required "recovery-code hash" (hashRecoveryCodeWithSalt defaultPasswordHashingPolicy "0123456789abcdef" recoveryCode)
          confirmedStore =
            MfaStore
              { saveUnconfirmedTotpEnrollment = \_ _ _ -> error "unexpected enrollment save",
                loadTotpEnrollment = \_ -> pure (Right (Just (StoredTotpEnrollment "not-needed-for-recovery-code" (Just 100) Nothing))),
                confirmTotpEnrollment = \_ _ _ -> error "unexpected enrollment confirmation",
                loadUnusedRecoveryCodeHashes = \receivedAccountId -> receivedAccountId `seq` pure (Right [recoveryCodeHashText recoveryCodeHash]),
                consumeRecoveryCodeHash = \receivedAccountId receivedHash receivedNow -> do
                  expectAll
                    ( (receivedAccountId `shouldBe` accountId)
                        :| [receivedHash `shouldBe` recoveryCodeHashText recoveryCodeHash, receivedNow `shouldBe` 500]
                    )
                  writeIORef consumedHashReference (Just receivedHash)
                  pure (Right True),
                markTotpCodeUsed = \_ _ -> error "unexpected TOTP counter update"
              }
      completePasswordLogin (credentialStore (Right (Just verifiedCredential))) (secondFactorContextFor confirmedStore (RecoveryCodeLoginProof recoveryCode)) emailAddress (mkPassword "correct horse battery staple")
        `shouldReturnEqual` PasswordMfaLoginAccepted accountId
      readIORef consumedHashReference `shouldReturn` Just (recoveryCodeHashText recoveryCodeHash)

    it "bounds every recovery-code check and continues past a nonmatching verifier" $ do
      let recoveryCode = required "recovery code" (mkRecoveryCode "0123456789ABCDEF0123")
          otherRecoveryCode = required "other recovery code" (mkRecoveryCode "0123456789ABCDEF0124")
          recoveryCodeHash = required "recovery-code hash" (hashRecoveryCodeWithSalt defaultPasswordHashingPolicy "0123456789abcdef" recoveryCode)
          otherRecoveryCodeHash = required "other recovery-code hash" (hashRecoveryCodeWithSalt defaultPasswordHashingPolicy "0123456789abcdef" otherRecoveryCode)
          enrollment = StoredTotpEnrollment "not-needed-for-recovery-code" (Just 100) Nothing
          recoveryStore hashes consumeResult =
            (mfaStore (Right (Just enrollment)))
              { loadUnusedRecoveryCodeHashes = \_ -> pure (Right hashes),
                consumeRecoveryCodeHash = \_ _ _ -> pure consumeResult
              }
          complete gate store =
            completePasswordLogin
              (credentialStore (Right (Just verifiedCredential)))
              ((secondFactorContextFor store (RecoveryCodeLoginProof recoveryCode)) {secondFactorPasswordWorkGate = gate})
              emailAddress
              (mkPassword "correct horse battery staple")
          completeContext secondFactorContext =
            completePasswordLogin
              (credentialStore (Right (Just verifiedCredential)))
              secondFactorContext
              emailAddress
              (mkPassword "correct horse battery staple")
      complete testPasswordWorkGate (recoveryStore [recoveryCodeHashText otherRecoveryCodeHash] (Right True))
        `shouldReturnEqual` PasswordMfaLoginRejected
      consumptionAttempted <- newIORef False
      settledOutcome <- newIORef Nothing
      let racedRecoveryStore =
            (recoveryStore [recoveryCodeHashText recoveryCodeHash] (Right False))
              { consumeRecoveryCodeHash = \_ _ _ -> writeIORef consumptionAttempted True >> pure (Right False)
              }
          settlingThrottle =
            permissiveThrottle
              { loginThrottleStore =
                  (loginThrottleStore permissiveThrottle)
                    { settleLoginAttempt = \_ outcome -> outcome `seq` writeIORef settledOutcome (Just outcome) >> pure (Right ())
                    }
              }
          racedContext =
            (secondFactorContextFor racedRecoveryStore (RecoveryCodeLoginProof recoveryCode))
              { secondFactorPasswordWorkGate = testPasswordWorkGate,
                secondFactorThrottle = settlingThrottle
              }
      completeContext racedContext
        `shouldReturnEqual` PasswordMfaLoginRejected
      readIORef consumptionAttempted `shouldReturn` True
      readIORef settledOutcome `shouldReturn` Just False

      passwordWorkGate <- newPasswordWorkGate (required "password-work budget" (mkPasswordWorkBudget 65536))
      holderStarted <- newEmptyMVar
      holderRelease <- newEmptyMVar
      holderFinished <- newEmptyMVar
      enrollmentLoads <- newIORef (0 :: Int)
      let blockingStore =
            (recoveryStore [recoveryCodeHashText recoveryCodeHash] (Right True))
              { loadTotpEnrollment = \_ -> do
                  isFirstLoad <- atomicModifyIORef' enrollmentLoads (\loads -> (loads + 1, loads == 0))
                  if isFirstLoad
                    then do
                      _ <-
                        forkIO $ do
                          held <- withPasswordWork passwordWorkGate 65536 (putMVar holderStarted () >> takeMVar holderRelease)
                          putMVar holderFinished held
                      takeMVar holderStarted
                      pure (Right (Just enrollment))
                    else pure (Right (Just enrollment))
              }
      complete passwordWorkGate blockingStore `shouldReturnEqual` PasswordMfaLoginPasswordWorkBudgetExhausted
      putMVar holderRelease ()
      takeMVar holderFinished `shouldReturn` Just ()

    it "rejects unavailable, raced, and corrupt second-factor records" $ do
      let secret = required "TOTP secret" (mkTotpSecret "JBSWY3DPEHPK3PXPJBSWY3DPEHPK3PXP")
          validProof = TotpLoginProof (totpCode 123456 secret)
          completeWith store proof = completePasswordLogin (credentialStore (Right (Just verifiedCredential))) (secondFactorContextFor store proof) emailAddress (mkPassword "correct horse battery staple")
          enrollmentStore enrollment =
            MfaStore
              { saveUnconfirmedTotpEnrollment = \_ _ _ -> error "unexpected enrollment save",
                loadTotpEnrollment = \_ -> pure enrollment,
                confirmTotpEnrollment = \_ _ _ -> error "unexpected enrollment confirmation",
                loadUnusedRecoveryCodeHashes = \_ -> pure (Right []),
                consumeRecoveryCodeHash = \_ _ _ -> error "unexpected recovery-code consumption",
                markTotpCodeUsed = \_ _ -> error "unexpected TOTP counter update"
              }
      completeWith (enrollmentStore (Right Nothing)) validProof `shouldReturnEqual` PasswordMfaLoginEnrollmentRequired accountId
      completeWith (enrollmentStore (Right (Just (StoredTotpEnrollment "not-an-envelope" (Just 100) Nothing)))) validProof `shouldReturnEqual` PasswordMfaLoginCorruptEnrollment
      completeWith (enrollmentStore (Left (MfaStoreUnavailable "database unavailable"))) validProof `shouldReturnEqual` PasswordMfaLoginMfaStoreError (MfaStoreUnavailable "database unavailable")
      completeWith (enrollmentStore (Right (Just (StoredTotpEnrollment "not-needed-for-recovery-code" Nothing Nothing)))) validProof `shouldReturnEqual` PasswordMfaLoginEnrollmentRequired accountId
      let recoveryCode = required "recovery code" (mkRecoveryCode "0123456789ABCDEF0123")
          recoveryCodeHash = required "recovery-code hash" (hashRecoveryCodeWithSalt defaultPasswordHashingPolicy "0123456789abcdef" recoveryCode)
          racedStore =
            (enrollmentStore (Right (Just (StoredTotpEnrollment "not-needed-for-recovery-code" (Just 100) Nothing))))
              { loadUnusedRecoveryCodeHashes = \_ -> pure (Right [recoveryCodeHashText recoveryCodeHash]),
                consumeRecoveryCodeHash = \_ _ _ -> pure (Right False)
              }
      completeWith racedStore (RecoveryCodeLoginProof recoveryCode) `shouldReturnEqual` PasswordMfaLoginRejected

    it "preserves every password and second-factor state without authenticating early" $ do
      let secret = required "TOTP secret" (mkTotpSecret "JBSWY3DPEHPK3PXPJBSWY3DPEHPK3PXP")
          encryptedSecret = requiredCrypto (encryptSecretWithNonce encryptionKey (required "encryption nonce" (mkEncryptionNonce (ByteString.replicate 12 8))) (mkSecretPlaintext (TextEncoding.encodeUtf8 (renderTotpSecret secret))))
          confirmedEnrollment = StoredTotpEnrollment encryptedSecret (Just 100) Nothing
          validProof = TotpLoginProof (totpCode 123456 secret)
          completeWith credentialStoreValue mfaStoreValue = completePasswordLogin credentialStoreValue (secondFactorContextFor mfaStoreValue validProof) emailAddress (mkPassword "correct horse battery staple")
      completeWith (credentialStore (Right (Just unverifiedCredential))) unexpectedMfaStore `shouldReturnEqual` PasswordMfaLoginEmailVerificationRequired accountId
      completeWith (credentialStore (Left (AccountCredentialStoreCorruptData "bad credential"))) unexpectedMfaStore `shouldReturnEqual` PasswordMfaLoginCredentialStoreError (AccountCredentialStoreCorruptData "bad credential")
      completeWith (credentialStore (Right (Just verifiedCredential))) (mfaStore (Right Nothing)) `shouldReturnEqual` PasswordMfaLoginEnrollmentRequired accountId
      secondLookupStore <- storeWithLookups [Right (Just confirmedEnrollment), Left (MfaStoreCorruptData "second lookup failed")]
      completeWith (credentialStore (Right (Just verifiedCredential))) secondLookupStore `shouldReturnEqual` PasswordMfaLoginMfaStoreError (MfaStoreCorruptData "second lookup failed")
      noEnrollmentStore <- storeWithLookups [Right (Just confirmedEnrollment), Right Nothing]
      completeWith (credentialStore (Right (Just verifiedCredential))) noEnrollmentStore `shouldReturnEqual` PasswordMfaLoginEnrollmentRequired accountId
      pendingEnrollmentStore <- storeWithLookups [Right (Just confirmedEnrollment), Right (Just (StoredTotpEnrollment encryptedSecret Nothing Nothing))]
      completeWith (credentialStore (Right (Just verifiedCredential))) pendingEnrollmentStore `shouldReturnEqual` PasswordMfaLoginEnrollmentRequired accountId
      let invalidUtf8Envelope = requiredCrypto (encryptSecretWithNonce encryptionKey (required "encryption nonce" (mkEncryptionNonce (ByteString.replicate 12 9))) (mkSecretPlaintext (ByteString.pack [255])))
      completeWith (credentialStore (Right (Just verifiedCredential))) (mfaStore (Right (Just (StoredTotpEnrollment invalidUtf8Envelope (Just 100) Nothing)))) `shouldReturnEqual` PasswordMfaLoginCorruptEnrollment

    it "rejects malformed, missing, and unavailable recovery-code state" $ do
      let recoveryCode = required "recovery code" (mkRecoveryCode "0123456789ABCDEF0123")
          confirmedEnrollment = StoredTotpEnrollment "recovery-code-login" (Just 100) Nothing
          completeWith store = completePasswordLogin (credentialStore (Right (Just verifiedCredential))) (secondFactorContextFor store (RecoveryCodeLoginProof recoveryCode)) emailAddress (mkPassword "correct horse battery staple")
          storeFor recoveryResult consumptionResult =
            MfaStore
              { saveUnconfirmedTotpEnrollment = \_ _ _ -> error "unexpected enrollment save",
                loadTotpEnrollment = \_ -> pure (Right (Just confirmedEnrollment)),
                confirmTotpEnrollment = \_ _ _ -> error "unexpected enrollment confirmation",
                loadUnusedRecoveryCodeHashes = \_ -> pure recoveryResult,
                consumeRecoveryCodeHash = \_ _ _ -> pure consumptionResult,
                markTotpCodeUsed = \_ _ -> error "unexpected TOTP counter update"
              }
      completeWith (storeFor (Left (MfaStoreUnavailable "recovery lookup failed")) (Right True)) `shouldReturnEqual` PasswordMfaLoginMfaStoreError (MfaStoreUnavailable "recovery lookup failed")
      completeWith (storeFor (Right ["not-a-password-hash"]) (Right True)) `shouldReturnEqual` PasswordMfaLoginCorruptEnrollment
      completeWith (storeFor (Right []) (Right True)) `shouldReturnEqual` PasswordMfaLoginRejected
      let recoveryCodeHash = required "recovery-code hash" (hashRecoveryCodeWithSalt defaultPasswordHashingPolicy "0123456789abcdef" recoveryCode)
      completeWith (storeFor (Right [recoveryCodeHashText recoveryCodeHash]) (Left (MfaStoreCorruptData "recovery consumption failed"))) `shouldReturnEqual` PasswordMfaLoginMfaStoreError (MfaStoreCorruptData "recovery consumption failed")

    it "keeps login outcomes and credential-store errors comparable" $ do
      let totpProof = TotpLoginProof (required "TOTP code" (mkTotpCode "123456"))
          recoveryProof = RecoveryCodeLoginProof (required "recovery code" (mkRecoveryCode "0123456789ABCDEF0123"))
      expectAll
        ( (AccountCredentialStoreUnavailable "unavailable" /= AccountCredentialStoreUnavailable "other" `shouldBe` True)
            :| [ AccountCredentialStoreUnavailable "unavailable" /= AccountCredentialStoreCorruptData "unavailable" `shouldBe` True,
                 PasswordLoginRejected /= PasswordLoginMfaRequired accountId `shouldBe` True,
                 PasswordLoginEmailVerificationRequired accountId /= PasswordLoginMfaEnrollmentRequired accountId `shouldBe` True,
                 PasswordLoginCredentialStoreError (AccountCredentialStoreUnavailable "unavailable") /= PasswordLoginMfaStoreError (MfaStoreUnavailable "unavailable") `shouldBe` True,
                 LoginAttemptReservation "reservation" == LoginAttemptReservation "reservation" `shouldBe` True,
                 LoginAttemptReservation "reservation" /= LoginAttemptReservation "other" `shouldBe` True,
                 LoginAttemptReserved (LoginAttemptReservation "reservation") == LoginAttemptReserved (LoginAttemptReservation "reservation") `shouldBe` True,
                 LoginAttemptReserved (LoginAttemptReservation "reservation") /= LoginAttemptThrottled 500 `shouldBe` True,
                 LoginAttemptThrottled 500 == LoginAttemptThrottled 500 `shouldBe` True,
                 totpProof /= recoveryProof `shouldBe` True,
                 PasswordMfaLoginRejected /= PasswordMfaLoginEmailVerificationRequired accountId `shouldBe` True,
                 PasswordMfaLoginEnrollmentRequired accountId /= PasswordMfaLoginAccepted accountId `shouldBe` True,
                 PasswordMfaLoginCredentialStoreError (AccountCredentialStoreUnavailable "unavailable") /= PasswordMfaLoginMfaStoreError (MfaStoreUnavailable "unavailable") `shouldBe` True,
                 PasswordMfaLoginCorruptEnrollment /= PasswordMfaLoginRejected `shouldBe` True,
                 PasswordLoginThrottled 100 /= PasswordLoginThrottled 200 `shouldBe` True,
                 PasswordLoginThrottled 100 /= PasswordLoginRejected `shouldBe` True,
                 PasswordLoginAttemptStoreError (LoginAttemptStoreUnavailable "x") /= PasswordLoginAttemptStoreError (LoginAttemptStoreCorruptData "x") `shouldBe` True,
                 PasswordMfaLoginThrottled 100 /= PasswordMfaLoginThrottled 200 `shouldBe` True,
                 PasswordMfaLoginAttemptStoreError (LoginAttemptStoreUnavailable "x") /= PasswordMfaLoginAttemptStoreError (LoginAttemptStoreCorruptData "x") `shouldBe` True
               ]
        )

  describe "login throttling" $ do
    it "raises the existence-oracle dummy-hash construction error for an invalid input" $ do
      evaluate (requiredPasswordHashOrDie "test failure" Nothing `seq` ())
        `shouldThrow` \case
          ErrorCall message -> "test failure" `Text.isInfixOf` Text.pack message

    it "throttles a password step and an already-verified recovery-code step alike, without touching the credential, MFA, or attempt-recording stores" $ do
      let expectedLockoutEnd = 450 + fromIntegral (loginProtectionLockoutNanoseconds defaultLoginProtectionPolicy)
          throttledStoreFor matchingKey =
            LoginThrottleContext
              { loginThrottleStore =
                  LoginAttemptStore
                    { reserveLoginAttempt = \requestedKey _ _ ->
                        pure (Right (if requestedKey == matchingKey then LoginAttemptThrottled expectedLockoutEnd else LoginAttemptReserved (LoginAttemptReservation "permitted"))),
                      settleLoginAttempt = \_ _ -> pure (Right ()),
                      cancelLoginAttempt = \_ -> pure (Right ())
                    },
                loginThrottlePolicy = defaultLoginProtectionPolicy,
                loginThrottleNow = 500
              }
          unexpectedCredentialStore = AccountCredentialStore (\_ -> error "unexpected credential lookup while throttled") (\_ -> error "unexpected credential lookup while throttled") (\_ _ _ -> error "unexpected password-hash replacement while throttled")
      beginPasswordLogin unexpectedCredentialStore unexpectedMfaStore (throttledStoreFor "email:person@example.test") testPasswordWorkGate emailAddress (mkPassword "irrelevant")
        `shouldReturnEqual` PasswordLoginThrottled expectedLockoutEnd
      completePasswordLogin unexpectedCredentialStore ((secondFactorContextFor unexpectedMfaStore (TotpLoginProof (required "TOTP code" (mkTotpCode "123456")))) {secondFactorThrottle = throttledStoreFor "email:person@example.test"}) emailAddress (mkPassword "irrelevant")
        `shouldReturnEqual` PasswordMfaLoginThrottled expectedLockoutEnd
      let recoveryContext =
            -- 'confirmedMfaStore' lets the password step's and the second-factor
            -- step's own 'loadTotpEnrollment' lookups succeed (both are required
            -- before 'completeRecoveryCode' can even run), while its recovery-code
            -- methods still error out — proving the throttle short-circuits before
            -- the KDF-heavy recovery-code hash comparison this test is guarding.
            (secondFactorContextFor confirmedMfaStore (RecoveryCodeLoginProof (required "recovery code" (mkRecoveryCode "0123456789ABCDEF0123"))))
              { secondFactorThrottle = throttledStoreFor ("mfa:" <> accountIdText accountId)
              }
      completePasswordLogin (credentialStore (Right (Just verifiedCredential))) recoveryContext emailAddress (mkPassword "correct horse battery staple")
        `shouldReturnEqual` PasswordMfaLoginThrottled expectedLockoutEnd

    it "records an identical failed attempt for an unknown identifier and for a known identifier with a wrong password, so both count the same toward the throttle" $ do
      let recordingThrottle recordedReference =
            LoginThrottleContext
              { loginThrottleStore =
                  LoginAttemptStore
                    { reserveLoginAttempt = \key _ _ -> pure (Right (LoginAttemptReserved (LoginAttemptReservation key))),
                      settleLoginAttempt = \reservation succeeded -> modifyIORef' recordedReference ((showReservation reservation, succeeded) :) >> pure (Right ()),
                      cancelLoginAttempt = \_ -> pure (Right ())
                    },
                loginThrottlePolicy = defaultLoginProtectionPolicy,
                loginThrottleNow = 500
              }
      unknownReference <- newIORef []
      beginPasswordLogin (credentialStore (Right Nothing)) unexpectedMfaStore (recordingThrottle unknownReference) testPasswordWorkGate emailAddress (mkPassword "whatever")
        `shouldReturnEqual` PasswordLoginRejected
      unknownRecorded <- readIORef unknownReference
      knownReference <- newIORef []
      beginPasswordLogin (credentialStore (Right (Just verifiedCredential))) unexpectedMfaStore (recordingThrottle knownReference) testPasswordWorkGate emailAddress (mkPassword "incorrect password")
        `shouldReturnEqual` PasswordLoginRejected
      knownRecorded <- readIORef knownReference
      expectAll
        ( (unknownRecorded `shouldBe` [("email:person@example.test", False)])
            :| [knownRecorded `shouldBe` [("email:person@example.test", False)]]
        )

    it "records a successful attempt whenever the password check itself succeeds, even though a further step is still required" $ do
      let recordFor mfaStoreValue credential = do
            recordedReference <- newIORef []
            let throttle =
                  LoginThrottleContext
                    { loginThrottleStore =
                        LoginAttemptStore
                          { reserveLoginAttempt = \key _ _ -> pure (Right (LoginAttemptReserved (LoginAttemptReservation key))),
                            settleLoginAttempt = \reservation succeeded -> modifyIORef' recordedReference ((showReservation reservation, succeeded) :) >> pure (Right ()),
                            cancelLoginAttempt = \_ -> pure (Right ())
                          },
                      loginThrottlePolicy = defaultLoginProtectionPolicy,
                      loginThrottleNow = 500
                    }
            result <- beginPasswordLogin (credentialStore (Right (Just credential))) mfaStoreValue throttle testPasswordWorkGate emailAddress (mkPassword "correct horse battery staple")
            recorded <- readIORef recordedReference
            pure (result, recorded)
      -- 'verifiedCredential' reaches 'loadTotpEnrollment' (needs 'confirmedMfaStore'
      -- to answer it); 'unverifiedCredential' is rejected on its unverified email
      -- before any MFA lookup, so 'unexpectedMfaStore' proves that lookup never runs.
      (mfaRequiredResult, mfaRequiredRecorded) <- recordFor confirmedMfaStore verifiedCredential
      (unverifiedResult, unverifiedRecorded) <- recordFor unexpectedMfaStore unverifiedCredential
      (mfaEnrollmentResult, mfaEnrollmentRecorded) <- recordFor (mfaStore (Right Nothing)) verifiedCredential
      (mfaStoreErrorResult, mfaStoreErrorRecorded) <- recordFor (mfaStore (Left (MfaStoreUnavailable "MFA store down"))) verifiedCredential
      expectAll
        ( (mfaRequiredResult == PasswordLoginMfaRequired accountId `shouldBe` True)
            :| [ mfaRequiredRecorded `shouldBe` [("email:person@example.test", True)],
                 unverifiedResult == PasswordLoginEmailVerificationRequired accountId `shouldBe` True,
                 unverifiedRecorded `shouldBe` [("email:person@example.test", True)],
                 mfaEnrollmentResult == PasswordLoginMfaEnrollmentRequired accountId `shouldBe` True,
                 mfaEnrollmentRecorded `shouldBe` [("email:person@example.test", True)],
                 mfaStoreErrorResult == PasswordLoginMfaStoreError (MfaStoreUnavailable "MFA store down") `shouldBe` True,
                 mfaStoreErrorRecorded `shouldBe` [("email:person@example.test", True)]
               ]
        )

    it "propagates login-attempt store failures from the throttle check without querying the credential store" $ do
      let failingThrottle =
            LoginThrottleContext
              { loginThrottleStore =
                  LoginAttemptStore
                    { reserveLoginAttempt = \_ _ _ -> pure (Left (LoginAttemptStoreUnavailable "database unavailable")),
                      settleLoginAttempt = \_ _ -> error "unexpected throttle settlement after a failed admission",
                      cancelLoginAttempt = \_ -> error "unexpected throttle cancellation after a failed admission"
                    },
                loginThrottlePolicy = defaultLoginProtectionPolicy,
                loginThrottleNow = 500
              }
          unexpectedCredentialStore = AccountCredentialStore (\_ -> error "unexpected credential lookup") (\_ -> error "unexpected credential lookup") (\_ _ _ -> error "unexpected password-hash replacement")
      beginPasswordLogin unexpectedCredentialStore unexpectedMfaStore failingThrottle testPasswordWorkGate emailAddress (mkPassword "irrelevant")
        `shouldReturnEqual` PasswordLoginAttemptStoreError (LoginAttemptStoreUnavailable "database unavailable")
      completePasswordLogin unexpectedCredentialStore ((secondFactorContextFor unexpectedMfaStore (TotpLoginProof (required "TOTP code" (mkTotpCode "123456")))) {secondFactorThrottle = failingThrottle}) emailAddress (mkPassword "irrelevant")
        `shouldReturnEqual` PasswordMfaLoginAttemptStoreError (LoginAttemptStoreUnavailable "database unavailable")

    it "cancels an unsettled reservation on typed credential failure and returns cleanup or settlement errors" $ do
      cancelledReservationsReference <- newIORef []
      let reservation = LoginAttemptReservation "password-reservation"
          cancellationThrottle cancellationResult =
            LoginThrottleContext
              { loginThrottleStore =
                  LoginAttemptStore
                    { reserveLoginAttempt = \_ _ _ -> pure (Right (LoginAttemptReserved reservation)),
                      settleLoginAttempt = \_ _ -> error "unexpected settlement for a typed credential-store failure",
                      cancelLoginAttempt = \receivedReservation -> modifyIORef' cancelledReservationsReference (receivedReservation :) >> pure cancellationResult
                    },
                loginThrottlePolicy = defaultLoginProtectionPolicy,
                loginThrottleNow = 500
              }
          typedCredentialFailure = credentialStore (Left (AccountCredentialStoreUnavailable "credential store down"))
      beginPasswordLogin typedCredentialFailure unexpectedMfaStore (cancellationThrottle (Right ())) testPasswordWorkGate emailAddress (mkPassword "irrelevant")
        `shouldReturnEqual` PasswordLoginCredentialStoreError (AccountCredentialStoreUnavailable "credential store down")
      (map showReservation <$> readIORef cancelledReservationsReference) `shouldReturn` ["password-reservation"]
      beginPasswordLogin typedCredentialFailure unexpectedMfaStore (cancellationThrottle (Left (LoginAttemptStoreUnavailable "cleanup failed"))) testPasswordWorkGate emailAddress (mkPassword "irrelevant")
        `shouldReturnEqual` PasswordLoginAttemptStoreError (LoginAttemptStoreUnavailable "cleanup failed")
      let settlementFailureThrottle =
            LoginThrottleContext
              { loginThrottleStore =
                  LoginAttemptStore
                    { reserveLoginAttempt = \_ _ _ -> pure (Right (LoginAttemptReserved reservation)),
                      settleLoginAttempt = \_ _ -> pure (Left (LoginAttemptStoreUnavailable "settlement failed")),
                      cancelLoginAttempt = \receivedReservation -> modifyIORef' cancelledReservationsReference (receivedReservation :) >> pure (Right ())
                    },
                loginThrottlePolicy = defaultLoginProtectionPolicy,
                loginThrottleNow = 500
              }
      beginPasswordLogin (credentialStore (Right Nothing)) unexpectedMfaStore settlementFailureThrottle testPasswordWorkGate emailAddress (mkPassword "irrelevant")
        `shouldReturnEqual` PasswordLoginAttemptStoreError (LoginAttemptStoreUnavailable "settlement failed")
      (map showReservation <$> readIORef cancelledReservationsReference)
        `shouldReturn` ["password-reservation", "password-reservation", "password-reservation"]

    it "cancels a reserved password attempt when asynchronous cancellation interrupts credential work" $ do
      cancelledReservationsReference <- newIORef []
      credentialLookupStarted <- newEmptyMVar
      blockedCredentialResult <- newEmptyMVar
      workerResult <- newEmptyMVar
      let cancellationThrottle =
            LoginThrottleContext
              { loginThrottleStore =
                  LoginAttemptStore
                    { reserveLoginAttempt = \key _ _ -> pure (Right (LoginAttemptReserved (LoginAttemptReservation key))),
                      settleLoginAttempt = \_ _ -> error "unexpected settlement after asynchronous cancellation",
                      cancelLoginAttempt = \reservation -> modifyIORef' cancelledReservationsReference (showReservation reservation :) >> pure (Right ())
                    },
                loginThrottlePolicy = defaultLoginProtectionPolicy,
                loginThrottleNow = 500
              }
          blockedCredentialStore =
            AccountCredentialStore
              (\_ -> putMVar credentialLookupStarted () >> takeMVar blockedCredentialResult)
              (\_ -> error "unexpected username lookup")
              (\_ _ _ -> error "unexpected password-hash replacement")
      worker <- forkIO $ do
        result <- (try (beginPasswordLogin blockedCredentialStore unexpectedMfaStore cancellationThrottle testPasswordWorkGate emailAddress (mkPassword "irrelevant")) :: IO (Either SomeException PasswordLoginResult))
        putMVar workerResult result
      takeMVar credentialLookupStarted
      throwTo worker ThreadKilled
      cancellationResult <- takeMVar workerResult
      case cancellationResult of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected asynchronous cancellation"
      readIORef cancelledReservationsReference `shouldReturn` ["email:person@example.test"]

    it "keys case variants of a username identifier with the same canonical throttle identity, distinct from email" $ do
      keyReference <- newIORef []
      let capturingThrottle =
            LoginThrottleContext
              { loginThrottleStore =
                  LoginAttemptStore
                    { reserveLoginAttempt = \key _ _ -> modifyIORef' keyReference (key :) >> pure (Right (LoginAttemptReserved (LoginAttemptReservation key))),
                      settleLoginAttempt = \_ _ -> pure (Right ()),
                      cancelLoginAttempt = \_ -> pure (Right ())
                    },
                loginThrottlePolicy = defaultLoginProtectionPolicy,
                loginThrottleNow = 500
              }
          username = required "username" (mkUsername "Person_01")
          lowercaseUsername = required "lowercase username" (mkUsername "person_01")
      beginPasswordLoginWithIdentifier (credentialStore (Right Nothing)) unexpectedMfaStore capturingThrottle testPasswordWorkGate (LoginUsername username) (mkPassword "whatever")
        `shouldReturnEqual` PasswordLoginRejected
      beginPasswordLoginWithIdentifier (credentialStore (Right Nothing)) unexpectedMfaStore capturingThrottle testPasswordWorkGate (LoginUsername lowercaseUsername) (mkPassword "whatever")
        `shouldReturnEqual` PasswordLoginRejected
      capturedKeys <- readIORef keyReference
      capturedKeys `shouldBe` ["username:person_01", "username:person_01"]

    it "passes the current time and policy to admission before credential work" $ do
      admissionReference <- newIORef Nothing
      let capturingThrottleAt now =
            LoginThrottleContext
              { loginThrottleStore =
                  LoginAttemptStore
                    { reserveLoginAttempt = \_ policy admittedNow -> writeIORef admissionReference (Just (policy, admittedNow)) >> pure (Right (LoginAttemptReserved (LoginAttemptReservation "captured"))),
                      settleLoginAttempt = \_ _ -> pure (Right ()),
                      cancelLoginAttempt = \_ -> pure (Right ())
                    },
                loginThrottlePolicy = defaultLoginProtectionPolicy,
                loginThrottleNow = now
              }
      beginPasswordLogin (credentialStore (Right Nothing)) unexpectedMfaStore (capturingThrottleAt 500) testPasswordWorkGate emailAddress (mkPassword "whatever")
        `shouldReturnEqual` PasswordLoginRejected
      readIORef admissionReference `shouldReturn` Just (defaultLoginProtectionPolicy, 500)
      let largeNow = unixTimeNanoseconds (loginProtectionWindowNanoseconds defaultLoginProtectionPolicy + 500)
      beginPasswordLogin (credentialStore (Right Nothing)) unexpectedMfaStore (capturingThrottleAt largeNow) testPasswordWorkGate emailAddress (mkPassword "whatever")
        `shouldReturnEqual` PasswordLoginRejected
      readIORef admissionReference `shouldReturn` Just (defaultLoginProtectionPolicy, largeNow)

    it "propagates a login-attempt store failure from the recovery-code step's own throttle check, distinct from an already-throttled recovery attempt" $ do
      let recoveryKey = "mfa:" <> accountIdText accountId
          recoveryFailingThrottle =
            LoginThrottleContext
              { loginThrottleStore =
                  LoginAttemptStore
                    { reserveLoginAttempt = \requestedKey _ _ -> pure (if requestedKey == recoveryKey then Left (LoginAttemptStoreUnavailable "recovery throttle store down") else Right (LoginAttemptReserved (LoginAttemptReservation requestedKey))),
                      settleLoginAttempt = \_ _ -> pure (Right ()),
                      cancelLoginAttempt = \_ -> pure (Right ())
                    },
                loginThrottlePolicy = defaultLoginProtectionPolicy,
                loginThrottleNow = 500
              }
          -- 'confirmedMfaStore' lets both 'loadTotpEnrollment' lookups succeed
          -- (password step, then second-factor step) while its recovery-code
          -- methods still error out, proving the throttle store's own failure
          -- is what stops 'completeRecoveryCode' before any hash comparison.
          recoveryContext =
            (secondFactorContextFor confirmedMfaStore (RecoveryCodeLoginProof (required "recovery code" (mkRecoveryCode "0123456789ABCDEF0123"))))
              { secondFactorThrottle = recoveryFailingThrottle
              }
      completePasswordLogin (credentialStore (Right (Just verifiedCredential))) recoveryContext emailAddress (mkPassword "correct horse battery staple")
        `shouldReturnEqual` PasswordMfaLoginAttemptStoreError (LoginAttemptStoreUnavailable "recovery throttle store down")

    it "gates exhausted TOTP attempts before decrypting or marking the proof" $ do
      let expectedLockoutEnd = 450 + fromIntegral (loginProtectionLockoutNanoseconds defaultLoginProtectionPolicy)
          mfaKey = "mfa:" <> accountIdText accountId
          exhaustedTotpThrottle =
            LoginThrottleContext
              { loginThrottleStore =
                  LoginAttemptStore
                    { reserveLoginAttempt = \key _ _ -> pure (Right (if key == mfaKey then LoginAttemptThrottled expectedLockoutEnd else LoginAttemptReserved (LoginAttemptReservation key))),
                      settleLoginAttempt = \_ _ -> pure (Right ()),
                      cancelLoginAttempt = \_ -> pure (Right ())
                    },
                loginThrottlePolicy = defaultLoginProtectionPolicy,
                loginThrottleNow = 500
              }
          totpContext =
            (secondFactorContextFor confirmedMfaStore (TotpLoginProof (required "TOTP code" (mkTotpCode "123456"))))
              { secondFactorThrottle = exhaustedTotpThrottle
              }
      completePasswordLogin (credentialStore (Right (Just verifiedCredential))) totpContext emailAddress (mkPassword "correct horse battery staple")
        `shouldReturnEqual` PasswordMfaLoginThrottled expectedLockoutEnd

    it "propagates an attempt-store failure from the TOTP throttle check before validating the proof" $ do
      let mfaKey = "mfa:" <> accountIdText accountId
          failingTotpThrottle =
            LoginThrottleContext
              { loginThrottleStore =
                  LoginAttemptStore
                    { reserveLoginAttempt = \key _ _ -> pure (if key == mfaKey then Left (LoginAttemptStoreUnavailable "TOTP throttle store down") else Right (LoginAttemptReserved (LoginAttemptReservation key))),
                      settleLoginAttempt = \_ _ -> pure (Right ()),
                      cancelLoginAttempt = \_ -> pure (Right ())
                    },
                loginThrottlePolicy = defaultLoginProtectionPolicy,
                loginThrottleNow = 500
              }
          totpContext =
            (secondFactorContextFor confirmedMfaStore (TotpLoginProof (required "TOTP code" (mkTotpCode "123456"))))
              { secondFactorThrottle = failingTotpThrottle
              }
      completePasswordLogin (credentialStore (Right (Just verifiedCredential))) totpContext emailAddress (mkPassword "correct horse battery staple")
        `shouldReturnEqual` PasswordMfaLoginAttemptStoreError (LoginAttemptStoreUnavailable "TOTP throttle store down")

    it "settles rejected and accepted TOTP proofs in the shared MFA history" $ do
      recordedAttemptsReference <- newIORef []
      let secret = required "TOTP secret" (mkTotpSecret "JBSWY3DPEHPK3PXPJBSWY3DPEHPK3PXP")
          encryptedSecret = requiredCrypto (encryptSecretWithNonce encryptionKey (required "encryption nonce" (mkEncryptionNonce (ByteString.replicate 12 5))) (mkSecretPlaintext (TextEncoding.encodeUtf8 (renderTotpSecret secret))))
          recordingThrottle =
            LoginThrottleContext
              { loginThrottleStore =
                  LoginAttemptStore
                    { reserveLoginAttempt = \key _ _ -> pure (Right (LoginAttemptReserved (LoginAttemptReservation key))),
                      settleLoginAttempt = \reservation succeeded -> modifyIORef' recordedAttemptsReference ((showReservation reservation, succeeded) :) >> pure (Right ()),
                      cancelLoginAttempt = \_ -> pure (Right ())
                    },
                loginThrottlePolicy = defaultLoginProtectionPolicy,
                loginThrottleNow = 500
              }
          totpStore =
            (mfaStore (Right (Just (StoredTotpEnrollment encryptedSecret (Just 100) Nothing))))
              { markTotpCodeUsed = \_ _ -> pure (Right True)
              }
          matchedCounter = 123456 `div` 30
          replayedTotpStore =
            (mfaStore (Right (Just (StoredTotpEnrollment encryptedSecret (Just 100) (Just matchedCounter)))))
              { markTotpCodeUsed = \_ _ -> error "replayed TOTP proof must not reach the counter update"
              }
          racedTotpStore =
            totpStore
              { markTotpCodeUsed = \_ _ -> pure (Right False)
              }
          complete store proof =
            completePasswordLogin
              (credentialStore (Right (Just verifiedCredential)))
              ((secondFactorContextFor store proof) {secondFactorThrottle = recordingThrottle})
              emailAddress
              (mkPassword "correct horse battery staple")
      complete totpStore (TotpLoginProof (required "invalid TOTP code" (mkTotpCode "000000")))
        `shouldReturnEqual` PasswordMfaLoginRejected
      complete totpStore (TotpLoginProof (totpCode 123456 secret))
        `shouldReturnEqual` PasswordMfaLoginAccepted accountId
      complete replayedTotpStore (TotpLoginProof (totpCode 123456 secret))
        `shouldReturnEqual` PasswordMfaLoginRejected
      complete racedTotpStore (TotpLoginProof (totpCode 123456 secret))
        `shouldReturnEqual` PasswordMfaLoginRejected
      recordedAttempts <- readIORef recordedAttemptsReference
      filter ((== "mfa:" <> accountIdText accountId) . fst) recordedAttempts
        `shouldBe` [ ("mfa:" <> accountIdText accountId, False),
                     ("mfa:" <> accountIdText accountId, False),
                     ("mfa:" <> accountIdText accountId, True),
                     ("mfa:" <> accountIdText accountId, False)
                   ]

    it "settles rejected and accepted recovery proofs in the shared MFA history" $ do
      recordedAttemptsReference <- newIORef []
      let recoveryCode = required "recovery code" (mkRecoveryCode "0123456789ABCDEF0123")
          recoveryCodeHash = required "recovery-code hash" (hashRecoveryCodeWithSalt defaultPasswordHashingPolicy "0123456789abcdef" recoveryCode)
          recordingThrottle =
            LoginThrottleContext
              { loginThrottleStore =
                  LoginAttemptStore
                    { reserveLoginAttempt = \key _ _ -> pure (Right (LoginAttemptReserved (LoginAttemptReservation key))),
                      settleLoginAttempt = \reservation succeeded -> modifyIORef' recordedAttemptsReference ((showReservation reservation, succeeded) :) >> pure (Right ()),
                      cancelLoginAttempt = \_ -> pure (Right ())
                    },
                loginThrottlePolicy = defaultLoginProtectionPolicy,
                loginThrottleNow = 500
              }
          recoveryStore hashes =
            (mfaStore (Right (Just (StoredTotpEnrollment "recovery-code-login" (Just 100) Nothing))))
              { loadUnusedRecoveryCodeHashes = \_ -> pure (Right hashes),
                consumeRecoveryCodeHash = \_ _ _ -> pure (Right True)
              }
          complete store =
            completePasswordLogin
              (credentialStore (Right (Just verifiedCredential)))
              ((secondFactorContextFor store (RecoveryCodeLoginProof recoveryCode)) {secondFactorThrottle = recordingThrottle})
              emailAddress
              (mkPassword "correct horse battery staple")
      complete (recoveryStore [recoveryCodeHashText recoveryCodeHash])
        `shouldReturnEqual` PasswordMfaLoginAccepted accountId
      complete (recoveryStore [])
        `shouldReturnEqual` PasswordMfaLoginRejected
      recordedAttempts <- readIORef recordedAttemptsReference
      filter ((== "mfa:" <> accountIdText accountId) . fst) recordedAttempts
        `shouldBe` [ ("mfa:" <> accountIdText accountId, False),
                     ("mfa:" <> accountIdText accountId, True)
                   ]

    it "fails closed when a required password, TOTP, or recovery settlement write fails" $ do
      let secret = required "TOTP secret" (mkTotpSecret "JBSWY3DPEHPK3PXPJBSWY3DPEHPK3PXP")
          encryptedSecret = requiredCrypto (encryptSecretWithNonce encryptionKey (required "encryption nonce" (mkEncryptionNonce (ByteString.replicate 12 6))) (mkSecretPlaintext (TextEncoding.encodeUtf8 (renderTotpSecret secret))))
          recoveryCode = required "recovery code" (mkRecoveryCode "0123456789ABCDEF0123")
          recoveryCodeHash = required "recovery-code hash" (hashRecoveryCodeWithSalt defaultPasswordHashingPolicy "0123456789abcdef" recoveryCode)
          settlementFailure = LoginAttemptStoreUnavailable "attempt settlement failed"
          failingThrottle =
            LoginThrottleContext
              { loginThrottleStore =
                  LoginAttemptStore
                    { reserveLoginAttempt = \key _ _ -> pure (Right (LoginAttemptReserved (LoginAttemptReservation key))),
                      settleLoginAttempt = \reservation _ ->
                        if showReservation reservation == "email:person@example.test"
                          then pure (Left settlementFailure)
                          else pure (Right ()),
                      cancelLoginAttempt = \_ -> pure (Right ())
                    },
                loginThrottlePolicy = defaultLoginProtectionPolicy,
                loginThrottleNow = 500
              }
          secondFactorSettlementFailure =
            failingThrottle
              { loginThrottleStore =
                  LoginAttemptStore
                    { reserveLoginAttempt = \key _ _ -> pure (Right (LoginAttemptReserved (LoginAttemptReservation key))),
                      settleLoginAttempt = \reservation _ ->
                        if showReservation reservation == "mfa:" <> accountIdText accountId
                          then pure (Left settlementFailure)
                          else pure (Right ()),
                      cancelLoginAttempt = \_ -> pure (Right ())
                    }
              }
          totpStore =
            (mfaStore (Right (Just (StoredTotpEnrollment encryptedSecret (Just 100) Nothing))))
              { markTotpCodeUsed = \_ _ -> pure (Right True)
              }
          recoveryStore =
            (mfaStore (Right (Just (StoredTotpEnrollment "not-needed-for-recovery-code" (Just 100) Nothing))))
              { loadUnusedRecoveryCodeHashes = \_ -> pure (Right [recoveryCodeHashText recoveryCodeHash]),
                consumeRecoveryCodeHash = \_ _ _ -> pure (Right True)
              }
          password = mkPassword "correct horse battery staple"
      beginPasswordLogin (credentialStore (Right Nothing)) unexpectedMfaStore failingThrottle testPasswordWorkGate emailAddress password
        `shouldReturnEqual` PasswordLoginAttemptStoreError settlementFailure
      completePasswordLogin (credentialStore (Right (Just verifiedCredential))) ((secondFactorContextFor totpStore (TotpLoginProof (totpCode 123456 secret))) {secondFactorThrottle = secondFactorSettlementFailure}) emailAddress password
        `shouldReturnEqual` PasswordMfaLoginAttemptStoreError settlementFailure
      completePasswordLogin (credentialStore (Right (Just verifiedCredential))) ((secondFactorContextFor recoveryStore (RecoveryCodeLoginProof recoveryCode)) {secondFactorThrottle = secondFactorSettlementFailure}) emailAddress password
        `shouldReturnEqual` PasswordMfaLoginAttemptStoreError settlementFailure

credentialStore :: Either AccountCredentialStoreError (Maybe AccountCredential) -> AccountCredentialStore
credentialStore result = AccountCredentialStore (\requestedEmail -> requestedEmail `seq` pure result) (\requestedUsername -> requestedUsername `seq` pure result) (\_ _ _ -> pure (Right False))

mfaStore :: Either MfaStoreError (Maybe StoredTotpEnrollment) -> MfaStore
mfaStore result =
  MfaStore
    { saveUnconfirmedTotpEnrollment = \_ _ _ -> error "unexpected enrollment save",
      loadTotpEnrollment = \requestedAccountId -> requestedAccountId `seq` pure result,
      confirmTotpEnrollment = \_ _ _ -> error "unexpected enrollment confirmation",
      loadUnusedRecoveryCodeHashes = \_ -> error "unexpected recovery-code lookup",
      consumeRecoveryCodeHash = \_ _ _ -> error "unexpected recovery-code consumption",
      markTotpCodeUsed = \_ _ -> pure (Right True)
    }

unexpectedMfaStore :: MfaStore
unexpectedMfaStore = mfaStore (error "MFA lookup should not occur")

passwordHash :: PasswordHash
passwordHash = required "password hash" (hashPasswordWithSalt defaultPasswordHashingPolicy "0123456789abcdef" (mkPassword "correct horse battery staple"))

verifiedCredential :: AccountCredential
verifiedCredential = AccountCredential accountId passwordHash True

unverifiedCredential :: AccountCredential
unverifiedCredential = AccountCredential accountId passwordHash False

confirmedMfaStore :: MfaStore
confirmedMfaStore = mfaStore (Right (Just (StoredTotpEnrollment "encrypted" (Just 100) Nothing)))

storeWithLookups :: [Either MfaStoreError (Maybe StoredTotpEnrollment)] -> IO MfaStore
storeWithLookups lookupResults = do
  resultsReference <- newIORef lookupResults
  pure
    MfaStore
      { saveUnconfirmedTotpEnrollment = \_ _ _ -> error "unexpected enrollment save",
        loadTotpEnrollment = \_ -> do
          currentResults <- readIORef resultsReference
          case currentResults of
            [] -> error "unexpected extra enrollment lookup"
            result : remainingResults -> modifyIORef' resultsReference (const remainingResults) >> pure result,
        confirmTotpEnrollment = \_ _ _ -> error "unexpected enrollment confirmation",
        loadUnusedRecoveryCodeHashes = \_ -> error "unexpected recovery-code lookup",
        consumeRecoveryCodeHash = \_ _ _ -> error "unexpected recovery-code consumption",
        markTotpCodeUsed = \_ _ -> error "unexpected TOTP counter update"
      }

encryptionKey :: SecretEncryptionKey
encryptionKey = required "encryption key" (mkSecretEncryptionKey "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")

secondFactorContextFor :: MfaStore -> MfaLoginProof -> SecondFactorContext
secondFactorContextFor mfaStoreValue proof =
  SecondFactorContext
    { secondFactorMfaStore = mfaStoreValue,
      secondFactorEncryptionKey = encryptionKey,
      secondFactorNowNanoseconds = 500,
      secondFactorNowSeconds = 123456,
      secondFactorProof = proof,
      secondFactorThrottle = permissiveThrottle,
      secondFactorPasswordWorkGate = testPasswordWorkGate
    }

-- | Never denies an attempt and always settles or cancels successfully. Used
-- by every test in this module that is not itself about throttling.
permissiveThrottle :: LoginThrottleContext
permissiveThrottle =
  LoginThrottleContext
    { loginThrottleStore =
        LoginAttemptStore
          { reserveLoginAttempt = \_ _ _ -> pure (Right (LoginAttemptReserved (LoginAttemptReservation "permissive"))),
            settleLoginAttempt = \_ _ -> pure (Right ()),
            cancelLoginAttempt = \_ -> pure (Right ())
          },
      loginThrottlePolicy = defaultLoginProtectionPolicy,
      loginThrottleNow = 500
    }

requiredCrypto :: CryptoFailable value -> value
requiredCrypto = fromMaybe (error "expected cryptographic operation to succeed") . maybeCryptoError

showReservation :: LoginAttemptReservation -> Text.Text
showReservation (LoginAttemptReservation value) = value
