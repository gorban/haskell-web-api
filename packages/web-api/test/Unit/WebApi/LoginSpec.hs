{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Unit.WebApi.LoginSpec (spec) where

import Control.Exception (ErrorCall (..), evaluate)
import Control.Monad (unless)
import Crypto.Error (CryptoFailable, maybeCryptoError)
import Data.ByteString qualified as ByteString
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Account (AccountId, accountIdText, mkAccountId)
import HarchWeb.Email (EmailAddress, mkEmailAddress)
import HarchWeb.LoginProtection (LoginAttempt (..), defaultLoginProtectionPolicy, loginProtectionLockoutNanoseconds, loginProtectionWindowNanoseconds)
import HarchWeb.Password (PasswordHash, defaultPasswordHashingPolicy, hashPasswordWithSalt, mkPassword, passwordHashText)
import HarchWeb.RecoveryCode (hashRecoveryCodeWithSalt, mkRecoveryCode, recoveryCodeHashText)
import HarchWeb.Secret (SecretEncryptionKey, encryptSecretWithNonce, mkEncryptionNonce, mkSecretEncryptionKey, mkSecretPlaintext)
import HarchWeb.Totp (mkTotpCode, mkTotpSecret, renderTotpSecret, totpCode)
import HarchWeb.Username (mkUsername)
import Test.Hspec
import TestCore.CustomAssertions (expectAll)
import TestSupport.RealPostgres (defaultMigrationPostgresConfig, defaultRealPostgresConfig, ensureDefaultPostgresAvailable)
import WebApi.Config (DatabaseConfig (..))
import WebApi.Login
import WebApi.Mfa (MfaStore (..), MfaStoreError (..), StoredTotpEnrollment (..))
import WebApi.Postgres.Testing (buildRuntimePostgresAccountCredentialStore, buildRuntimePostgresAccountCredentialStoreWithRunner, runPostgresMigrationsForRuntime)

spec :: Spec
spec = do
  describe "password-first MFA login" $ do
    it "requires a confirmed authenticator only after a correct verified password" $ do
      beginPasswordLogin (credentialStore (Right (Just verifiedCredential))) confirmedMfaStore permissiveThrottle emailAddress (mkPassword "correct horse battery staple")
        `shouldReturnEqual` PasswordLoginMfaRequired accountId
      beginPasswordLogin (credentialStore (Right (Just verifiedCredential))) unexpectedMfaStore permissiveThrottle emailAddress (mkPassword "incorrect password")
        `shouldReturnEqual` PasswordLoginRejected
      beginPasswordLogin (credentialStore (Right Nothing)) unexpectedMfaStore permissiveThrottle emailAddress (mkPassword "correct horse battery staple")
        `shouldReturnEqual` PasswordLoginRejected

    it "directs verified accounts without a confirmed enrollment back to MFA setup" $ do
      beginPasswordLogin (credentialStore (Right (Just verifiedCredential))) (mfaStore (Right Nothing)) permissiveThrottle emailAddress (mkPassword "correct horse battery staple")
        `shouldReturnEqual` PasswordLoginMfaEnrollmentRequired accountId
      beginPasswordLogin (credentialStore (Right (Just verifiedCredential))) (mfaStore (Right (Just (StoredTotpEnrollment "encrypted" Nothing Nothing)))) permissiveThrottle emailAddress (mkPassword "correct horse battery staple")
        `shouldReturnEqual` PasswordLoginMfaEnrollmentRequired accountId
      beginPasswordLogin (credentialStore (Right (Just unverifiedCredential))) unexpectedMfaStore permissiveThrottle emailAddress (mkPassword "correct horse battery staple")
        `shouldReturnEqual` PasswordLoginEmailVerificationRequired accountId

    it "preserves credential and MFA persistence failures" $ do
      beginPasswordLogin (credentialStore (Left (AccountCredentialStoreUnavailable "database unavailable"))) unexpectedMfaStore permissiveThrottle emailAddress (mkPassword "correct horse battery staple")
        `shouldReturnEqual` PasswordLoginCredentialStoreError (AccountCredentialStoreUnavailable "database unavailable")
      beginPasswordLogin (credentialStore (Right (Just verifiedCredential))) (mfaStore (Left (MfaStoreCorruptData "bad enrollment"))) permissiveThrottle emailAddress (mkPassword "correct horse battery staple")
        `shouldReturnEqual` PasswordLoginMfaStoreError (MfaStoreCorruptData "bad enrollment")

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
        ( (AccountCredentialStoreUnavailable "unavailable" == AccountCredentialStoreUnavailable "unavailable" `shouldBe` True)
            :| [ AccountCredentialStoreUnavailable "unavailable" /= AccountCredentialStoreUnavailable "other" `shouldBe` True,
                 AccountCredentialStoreUnavailable "unavailable" /= AccountCredentialStoreCorruptData "unavailable" `shouldBe` True,
                 PasswordLoginRejected == PasswordLoginRejected `shouldBe` True,
                 PasswordLoginRejected /= PasswordLoginMfaRequired accountId `shouldBe` True,
                 PasswordLoginEmailVerificationRequired accountId /= PasswordLoginMfaEnrollmentRequired accountId `shouldBe` True,
                 PasswordLoginCredentialStoreError (AccountCredentialStoreUnavailable "unavailable") /= PasswordLoginMfaStoreError (MfaStoreUnavailable "unavailable") `shouldBe` True,
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
      let recentFailures = [LoginAttempt failureTime False | failureTime <- [100, 200, 300, 400, 450]]
          expectedLockoutEnd = 450 + loginProtectionLockoutNanoseconds defaultLoginProtectionPolicy
          throttledStoreFor matchingKey =
            LoginThrottleContext
              { loginThrottleStore =
                  LoginAttemptStore
                    { recordLoginAttempt = \key attempt ->
                        if key == matchingKey
                          then error "unexpected throttle record for an already-throttled key"
                          else key `seq` attempt `seq` pure (Right ()),
                      loadRecentLoginAttempts = \requestedKey _ ->
                        pure (if requestedKey == matchingKey then Right recentFailures else Right [])
                    },
                loginThrottlePolicy = defaultLoginProtectionPolicy,
                loginThrottleNow = 500
              }
          unexpectedCredentialStore = AccountCredentialStore (\_ -> error "unexpected credential lookup while throttled") (\_ -> error "unexpected credential lookup while throttled")
      beginPasswordLogin unexpectedCredentialStore unexpectedMfaStore (throttledStoreFor "email:person@example.test") emailAddress (mkPassword "irrelevant")
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
              { secondFactorThrottle = throttledStoreFor ("recovery:" <> accountIdText accountId)
              }
      completePasswordLogin (credentialStore (Right (Just verifiedCredential))) recoveryContext emailAddress (mkPassword "correct horse battery staple")
        `shouldReturnEqual` PasswordMfaLoginThrottled expectedLockoutEnd

    it "records an identical failed attempt for an unknown identifier and for a known identifier with a wrong password, so both count the same toward the throttle" $ do
      let recordingThrottle recordedReference =
            LoginThrottleContext
              { loginThrottleStore =
                  LoginAttemptStore
                    { recordLoginAttempt = \key attempt -> modifyIORef' recordedReference ((key, attempt) :) >> pure (Right ()),
                      loadRecentLoginAttempts = \_ _ -> pure (Right [])
                    },
                loginThrottlePolicy = defaultLoginProtectionPolicy,
                loginThrottleNow = 500
              }
      unknownReference <- newIORef []
      beginPasswordLogin (credentialStore (Right Nothing)) unexpectedMfaStore (recordingThrottle unknownReference) emailAddress (mkPassword "whatever")
        `shouldReturnEqual` PasswordLoginRejected
      unknownRecorded <- readIORef unknownReference
      knownReference <- newIORef []
      beginPasswordLogin (credentialStore (Right (Just verifiedCredential))) unexpectedMfaStore (recordingThrottle knownReference) emailAddress (mkPassword "incorrect password")
        `shouldReturnEqual` PasswordLoginRejected
      knownRecorded <- readIORef knownReference
      expectAll
        ( (unknownRecorded `shouldBe` [("email:person@example.test", LoginAttempt 500 False)])
            :| [knownRecorded `shouldBe` [("email:person@example.test", LoginAttempt 500 False)]]
        )

    it "records a successful attempt whenever the password check itself succeeds, even though a further step is still required" $ do
      let recordFor mfaStoreValue credential = do
            recordedReference <- newIORef []
            let throttle =
                  LoginThrottleContext
                    { loginThrottleStore =
                        LoginAttemptStore
                          { recordLoginAttempt = \key attempt -> modifyIORef' recordedReference ((key, attempt) :) >> pure (Right ()),
                            loadRecentLoginAttempts = \_ _ -> pure (Right [])
                          },
                      loginThrottlePolicy = defaultLoginProtectionPolicy,
                      loginThrottleNow = 500
                    }
            result <- beginPasswordLogin (credentialStore (Right (Just credential))) mfaStoreValue throttle emailAddress (mkPassword "correct horse battery staple")
            recorded <- readIORef recordedReference
            pure (result, recorded)
      -- 'verifiedCredential' reaches 'loadTotpEnrollment' (needs 'confirmedMfaStore'
      -- to answer it); 'unverifiedCredential' is rejected on its unverified email
      -- before any MFA lookup, so 'unexpectedMfaStore' proves that lookup never runs.
      (mfaRequiredResult, mfaRequiredRecorded) <- recordFor confirmedMfaStore verifiedCredential
      (unverifiedResult, unverifiedRecorded) <- recordFor unexpectedMfaStore unverifiedCredential
      expectAll
        ( (mfaRequiredResult == PasswordLoginMfaRequired accountId `shouldBe` True)
            :| [ mfaRequiredRecorded `shouldBe` [("email:person@example.test", LoginAttempt 500 True)],
                 unverifiedResult == PasswordLoginEmailVerificationRequired accountId `shouldBe` True,
                 unverifiedRecorded `shouldBe` [("email:person@example.test", LoginAttempt 500 True)]
               ]
        )

    it "propagates login-attempt store failures from the throttle check without querying the credential store" $ do
      let failingThrottle =
            LoginThrottleContext
              { loginThrottleStore =
                  LoginAttemptStore
                    { recordLoginAttempt = \_ _ -> error "unexpected throttle record after a failed throttle check",
                      loadRecentLoginAttempts = \_ _ -> pure (Left (LoginAttemptStoreUnavailable "database unavailable"))
                    },
                loginThrottlePolicy = defaultLoginProtectionPolicy,
                loginThrottleNow = 500
              }
          unexpectedCredentialStore = AccountCredentialStore (\_ -> error "unexpected credential lookup") (\_ -> error "unexpected credential lookup")
      beginPasswordLogin unexpectedCredentialStore unexpectedMfaStore failingThrottle emailAddress (mkPassword "irrelevant")
        `shouldReturnEqual` PasswordLoginAttemptStoreError (LoginAttemptStoreUnavailable "database unavailable")
      completePasswordLogin unexpectedCredentialStore ((secondFactorContextFor unexpectedMfaStore (TotpLoginProof (required "TOTP code" (mkTotpCode "123456")))) {secondFactorThrottle = failingThrottle}) emailAddress (mkPassword "irrelevant")
        `shouldReturnEqual` PasswordMfaLoginAttemptStoreError (LoginAttemptStoreUnavailable "database unavailable")

    it "keys the throttle by \"username:\" for a username identifier, distinct from the \"email:\" key an email identifier uses" $ do
      keyReference <- newIORef []
      let capturingThrottle =
            LoginThrottleContext
              { loginThrottleStore =
                  LoginAttemptStore
                    { recordLoginAttempt = \key _ -> modifyIORef' keyReference (key :) >> pure (Right ()),
                      loadRecentLoginAttempts = \key _ -> modifyIORef' keyReference (key :) >> pure (Right [])
                    },
                loginThrottlePolicy = defaultLoginProtectionPolicy,
                loginThrottleNow = 500
              }
          username = required "username" (mkUsername "person_01")
      beginPasswordLoginWithIdentifier (credentialStore (Right Nothing)) unexpectedMfaStore capturingThrottle (LoginUsername username) (mkPassword "whatever")
        `shouldReturnEqual` PasswordLoginRejected
      capturedKeys <- readIORef keyReference
      capturedKeys `shouldBe` ["username:person_01", "username:person_01"]

    it "clamps the throttle window's lower bound to zero instead of underflowing when the current time is earlier than the policy window" $ do
      sinceReference <- newIORef Nothing
      let capturingThrottleAt now =
            LoginThrottleContext
              { loginThrottleStore =
                  LoginAttemptStore
                    { recordLoginAttempt = \_ _ -> pure (Right ()),
                      loadRecentLoginAttempts = \_ since -> writeIORef sinceReference (Just since) >> pure (Right [])
                    },
                loginThrottlePolicy = defaultLoginProtectionPolicy,
                loginThrottleNow = now
              }
      beginPasswordLogin (credentialStore (Right Nothing)) unexpectedMfaStore (capturingThrottleAt 500) emailAddress (mkPassword "whatever")
        `shouldReturnEqual` PasswordLoginRejected
      readIORef sinceReference `shouldReturn` Just 0
      let largeNow = loginProtectionWindowNanoseconds defaultLoginProtectionPolicy + 500
      beginPasswordLogin (credentialStore (Right Nothing)) unexpectedMfaStore (capturingThrottleAt largeNow) emailAddress (mkPassword "whatever")
        `shouldReturnEqual` PasswordLoginRejected
      readIORef sinceReference `shouldReturn` Just 500

    it "propagates a login-attempt store failure from the recovery-code step's own throttle check, distinct from an already-throttled recovery attempt" $ do
      let recoveryKey = "recovery:" <> accountIdText accountId
          recoveryFailingThrottle =
            LoginThrottleContext
              { loginThrottleStore =
                  LoginAttemptStore
                    { recordLoginAttempt = \key attempt ->
                        if key == recoveryKey
                          then error "unexpected throttle record after a failed recovery throttle check"
                          else key `seq` attempt `seq` pure (Right ()),
                      loadRecentLoginAttempts = \requestedKey _ ->
                        pure (if requestedKey == recoveryKey then Left (LoginAttemptStoreUnavailable "recovery throttle store down") else Right [])
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

  describe "runtime PostgreSQL credential lookup" $ do
    it "uses an email parameter and decodes verified credentials" $ do
      let store = buildRuntimePostgresAccountCredentialStoreWithRunner runner databaseConfig
          runner _ query parameters = do
            expectAll
              ( (query `shouldBe` "SELECT account_id, password_hash, COALESCE(email_verified_at_nanoseconds::TEXT, '') FROM web_api.accounts WHERE email_normalized = $1;")
                  :| [parameters `shouldBe` ["person@example.test"]]
              )
            pure (Right [["account_01", encodedPasswordHash, "500"]])
      findAccountCredentialByEmail store emailAddress `shouldSatisfyEqual` \case
        Right (Just credential) ->
          accountCredentialId credential == accountId
            && passwordHashText (accountCredentialPasswordHash credential) == encodedPasswordHash
            && accountCredentialEmailVerified credential
        _ -> False

    it "uses a case-insensitive username parameter" $ do
      let username = required "username" (mkUsername "person_01")
          store = buildRuntimePostgresAccountCredentialStoreWithRunner runner databaseConfig
          runner _ query parameters = do
            expectAll
              ( (query `shouldBe` "SELECT account_id, password_hash, COALESCE(email_verified_at_nanoseconds::TEXT, '') FROM web_api.accounts WHERE lower(username) = lower($1);")
                  :| [parameters `shouldBe` ["person_01"]]
              )
            pure (Right [["account_01", encodedPasswordHash, "500"]])
      findAccountCredentialByUsername store username `shouldSatisfyEqual` \case
        Right (Just credential) -> accountCredentialId credential == accountId
        _ -> False

    it "maps absent, malformed, and unavailable credential results to typed outcomes" $ do
      let storeFor result = buildRuntimePostgresAccountCredentialStoreWithRunner (\_ _ _ -> pure result) databaseConfig
      findAccountCredentialByEmail (storeFor (Right [])) emailAddress `shouldSatisfyEqual` \case
        Right Nothing -> True
        _ -> False
      findAccountCredentialByEmail (storeFor (Right [["account_01", encodedPasswordHash, ""]])) emailAddress `shouldSatisfyEqual` \case
        Right (Just credential) -> not (accountCredentialEmailVerified credential)
        _ -> False
      findAccountCredentialByEmail (storeFor (Left "connection failed")) emailAddress
        `shouldSatisfyEqual` \case
          Left (AccountCredentialStoreUnavailable "connection failed") -> True
          _ -> False
      findAccountCredentialByEmail (storeFor (Right [["invalid id", encodedPasswordHash, "500"]])) emailAddress
        `shouldSatisfyEqual` \case
          Left (AccountCredentialStoreCorruptData "account credential lookup has an invalid account id") -> True
          _ -> False
      findAccountCredentialByEmail (storeFor (Right [["account_01", "invalid", "500"]])) emailAddress
        `shouldSatisfyEqual` \case
          Left (AccountCredentialStoreCorruptData "account credential lookup has an invalid password hash") -> True
          _ -> False
      findAccountCredentialByEmail (storeFor (Right [["account_01"]])) emailAddress
        `shouldSatisfyEqual` \case
          Left (AccountCredentialStoreCorruptData "unexpected account credential lookup result: [[\"account_01\"]]") -> True
          _ -> False

    it "executes the native libpq credential adapter against a migrated PostgreSQL database" $ do
      ensureDefaultPostgresAvailable
      runPostgresMigrationsForRuntime defaultMigrationPostgresConfig defaultRealPostgresConfig
        `shouldReturn` Right ()
      let store = buildRuntimePostgresAccountCredentialStore defaultRealPostgresConfig
      findAccountCredentialByEmail store (required "unknown email address" (mkEmailAddress "missing-login-credential@example.test"))
        `shouldSatisfyEqual` \case
          Right Nothing -> True
          _ -> False

credentialStore :: Either AccountCredentialStoreError (Maybe AccountCredential) -> AccountCredentialStore
credentialStore result = AccountCredentialStore (\requestedEmail -> requestedEmail `seq` pure result) (\requestedUsername -> requestedUsername `seq` pure result)

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

accountId :: AccountId
accountId = required "account id" (mkAccountId "account_01")

emailAddress :: EmailAddress
emailAddress = required "email address" (mkEmailAddress "person@example.test")

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
      secondFactorThrottle = permissiveThrottle
    }

-- | Never denies an attempt: records nothing, always reports no prior
-- attempts, so 'evaluateLoginAttempt' always returns 'LoginPermitted'. Used
-- by every test in this module that is not itself about throttling.
permissiveThrottle :: LoginThrottleContext
permissiveThrottle =
  LoginThrottleContext
    { loginThrottleStore =
        LoginAttemptStore
          { recordLoginAttempt = \_ _ -> pure (Right ()),
            loadRecentLoginAttempts = \_ _ -> pure (Right [])
          },
      loginThrottlePolicy = defaultLoginProtectionPolicy,
      loginThrottleNow = 500
    }

required :: String -> Maybe value -> value
required label = fromMaybe (error ("expected " <> label))

requiredCrypto :: CryptoFailable value -> value
requiredCrypto = fromMaybe (error "expected cryptographic operation to succeed") . maybeCryptoError

encodedPasswordHash :: Text.Text
encodedPasswordHash = passwordHashText passwordHash

databaseConfig :: DatabaseConfig
databaseConfig =
  DatabaseConfig
    { databaseHost = "127.0.0.1",
      databasePort = 5432,
      databaseName = "web_api_test",
      databaseUser = "web_api_runtime",
      databasePassword = "password"
    }

shouldReturnEqual :: (Eq value) => IO value -> value -> Expectation
shouldReturnEqual action expected = do
  actual <- action
  unless (actual == expected) (expectationFailure "unexpected result")

shouldSatisfyEqual :: IO value -> (value -> Bool) -> Expectation
shouldSatisfyEqual action predicate = do
  actual <- action
  unless (predicate actual) (expectationFailure "unexpected result")
