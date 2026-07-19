{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Unit.WebApi.LoginSpec (spec) where

import Control.Monad (unless)
import Data.ByteString qualified as ByteString
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Account (AccountId, mkAccountId)
import HarchWeb.Email (EmailAddress, mkEmailAddress)
import HarchWeb.Password (PasswordHash, defaultPasswordHashingPolicy, hashPasswordWithSalt, mkPassword, passwordHashText)
import HarchWeb.RecoveryCode (hashRecoveryCodeWithSalt, mkRecoveryCode, recoveryCodeHashText)
import HarchWeb.Secret (SecretEncryptionKey, encryptSecretWithNonce, mkSecretEncryptionKey)
import HarchWeb.Totp (mkTotpCode, mkTotpSecret, renderTotpSecret, totpCode)
import Test.Hspec
import TestSupport.RealPostgres (defaultMigrationPostgresConfig, defaultRealPostgresConfig, ensureDefaultPostgresAvailable)
import WebApi.Config (DatabaseConfig (..))
import WebApi.Login
import WebApi.Mfa (MfaStore (..), MfaStoreError (..), StoredTotpEnrollment (..))
import WebApi.Postgres (buildRuntimePostgresAccountCredentialStore, buildRuntimePostgresAccountCredentialStoreWithRunner, runPostgresMigrationsForRuntime)

spec :: Spec
spec = do
  describe "password-first MFA login" $ do
    it "requires a confirmed authenticator only after a correct verified password" $ do
      beginPasswordLogin (credentialStore (Right (Just verifiedCredential))) confirmedMfaStore emailAddress (mkPassword "correct horse battery staple")
        `shouldReturnEqual` PasswordLoginMfaRequired accountId
      beginPasswordLogin (credentialStore (Right (Just verifiedCredential))) unexpectedMfaStore emailAddress (mkPassword "incorrect password")
        `shouldReturnEqual` PasswordLoginRejected
      beginPasswordLogin (credentialStore (Right Nothing)) unexpectedMfaStore emailAddress (mkPassword "correct horse battery staple")
        `shouldReturnEqual` PasswordLoginRejected

    it "directs verified accounts without a confirmed enrollment back to MFA setup" $ do
      beginPasswordLogin (credentialStore (Right (Just verifiedCredential))) (mfaStore (Right Nothing)) emailAddress (mkPassword "correct horse battery staple")
        `shouldReturnEqual` PasswordLoginMfaEnrollmentRequired accountId
      beginPasswordLogin (credentialStore (Right (Just verifiedCredential))) (mfaStore (Right (Just (StoredTotpEnrollment "encrypted" Nothing)))) emailAddress (mkPassword "correct horse battery staple")
        `shouldReturnEqual` PasswordLoginMfaEnrollmentRequired accountId
      beginPasswordLogin (credentialStore (Right (Just unverifiedCredential))) unexpectedMfaStore emailAddress (mkPassword "correct horse battery staple")
        `shouldReturnEqual` PasswordLoginEmailVerificationRequired accountId

    it "preserves credential and MFA persistence failures" $ do
      beginPasswordLogin (credentialStore (Left (AccountCredentialStoreUnavailable "database unavailable"))) unexpectedMfaStore emailAddress (mkPassword "correct horse battery staple")
        `shouldReturnEqual` PasswordLoginCredentialStoreError (AccountCredentialStoreUnavailable "database unavailable")
      beginPasswordLogin (credentialStore (Right (Just verifiedCredential))) (mfaStore (Left (MfaStoreCorruptData "bad enrollment"))) emailAddress (mkPassword "correct horse battery staple")
        `shouldReturnEqual` PasswordLoginMfaStoreError (MfaStoreCorruptData "bad enrollment")

    it "accepts a validated TOTP only after validating the password" $ do
      let secret = required "TOTP secret" (mkTotpSecret "JBSWY3DPEHPK3PXPJBSWY3DPEHPK3PXP")
          encryptedSecret = required "encrypted TOTP secret" (encryptSecretWithNonce encryptionKey (ByteString.replicate 12 7) (TextEncoding.encodeUtf8 (renderTotpSecret secret)))
          confirmedStore = mfaStore (Right (Just (StoredTotpEnrollment encryptedSecret (Just 100))))
          validProof = TotpLoginProof (totpCode 123456 secret)
      completePasswordLogin (credentialStore (Right (Just verifiedCredential))) confirmedStore encryptionKey 500 123456 emailAddress (mkPassword "correct horse battery staple") validProof
        `shouldReturnEqual` PasswordMfaLoginAccepted accountId
      completePasswordLogin (credentialStore (Right (Just verifiedCredential))) confirmedStore encryptionKey 500 123456 emailAddress (mkPassword "incorrect password") validProof
        `shouldReturnEqual` PasswordMfaLoginRejected
      completePasswordLogin (credentialStore (Right (Just verifiedCredential))) confirmedStore encryptionKey 500 123456 emailAddress (mkPassword "correct horse battery staple") (TotpLoginProof (required "invalid TOTP code" (mkTotpCode "000000")))
        `shouldReturnEqual` PasswordMfaLoginRejected

    it "consumes a matched recovery-code hash atomically without exposing a reusable code" $ do
      consumedHashReference <- newIORef Nothing
      let recoveryCode = required "recovery code" (mkRecoveryCode "0123456789ABCDEF0123")
          recoveryCodeHash = required "recovery-code hash" (hashRecoveryCodeWithSalt defaultPasswordHashingPolicy "0123456789abcdef" recoveryCode)
          confirmedStore =
            MfaStore
              { saveUnconfirmedTotpEnrollment = \_ _ _ -> error "unexpected enrollment save",
                loadTotpEnrollment = \_ -> pure (Right (Just (StoredTotpEnrollment "not-needed-for-recovery-code" (Just 100)))),
                confirmTotpEnrollment = \_ _ _ -> error "unexpected enrollment confirmation",
                loadUnusedRecoveryCodeHashes = \receivedAccountId -> receivedAccountId `seq` pure (Right [recoveryCodeHashText recoveryCodeHash]),
                consumeRecoveryCodeHash = \receivedAccountId receivedHash receivedNow -> do
                  receivedAccountId `shouldBe` accountId
                  receivedHash `shouldBe` recoveryCodeHashText recoveryCodeHash
                  receivedNow `shouldBe` 500
                  writeIORef consumedHashReference (Just receivedHash)
                  pure (Right True)
              }
      completePasswordLogin (credentialStore (Right (Just verifiedCredential))) confirmedStore encryptionKey 500 123456 emailAddress (mkPassword "correct horse battery staple") (RecoveryCodeLoginProof recoveryCode)
        `shouldReturnEqual` PasswordMfaLoginAccepted accountId
      readIORef consumedHashReference `shouldReturn` Just (recoveryCodeHashText recoveryCodeHash)

    it "rejects unavailable, raced, and corrupt second-factor records" $ do
      let secret = required "TOTP secret" (mkTotpSecret "JBSWY3DPEHPK3PXPJBSWY3DPEHPK3PXP")
          validProof = TotpLoginProof (totpCode 123456 secret)
          completeWith store = completePasswordLogin (credentialStore (Right (Just verifiedCredential))) store encryptionKey 500 123456 emailAddress (mkPassword "correct horse battery staple")
          enrollmentStore enrollment =
            MfaStore
              { saveUnconfirmedTotpEnrollment = \_ _ _ -> error "unexpected enrollment save",
                loadTotpEnrollment = \_ -> pure enrollment,
                confirmTotpEnrollment = \_ _ _ -> error "unexpected enrollment confirmation",
                loadUnusedRecoveryCodeHashes = \_ -> pure (Right []),
                consumeRecoveryCodeHash = \_ _ _ -> error "unexpected recovery-code consumption"
              }
      completeWith (enrollmentStore (Right Nothing)) validProof `shouldReturnEqual` PasswordMfaLoginEnrollmentRequired accountId
      completeWith (enrollmentStore (Right (Just (StoredTotpEnrollment "not-an-envelope" (Just 100))))) validProof `shouldReturnEqual` PasswordMfaLoginCorruptEnrollment
      completeWith (enrollmentStore (Left (MfaStoreUnavailable "database unavailable"))) validProof `shouldReturnEqual` PasswordMfaLoginMfaStoreError (MfaStoreUnavailable "database unavailable")
      completeWith (enrollmentStore (Right (Just (StoredTotpEnrollment "not-needed-for-recovery-code" Nothing)))) validProof `shouldReturnEqual` PasswordMfaLoginEnrollmentRequired accountId
      let recoveryCode = required "recovery code" (mkRecoveryCode "0123456789ABCDEF0123")
          recoveryCodeHash = required "recovery-code hash" (hashRecoveryCodeWithSalt defaultPasswordHashingPolicy "0123456789abcdef" recoveryCode)
          racedStore =
            (enrollmentStore (Right (Just (StoredTotpEnrollment "not-needed-for-recovery-code" (Just 100)))))
              { loadUnusedRecoveryCodeHashes = \_ -> pure (Right [recoveryCodeHashText recoveryCodeHash]),
                consumeRecoveryCodeHash = \_ _ _ -> pure (Right False)
              }
      completeWith racedStore (RecoveryCodeLoginProof recoveryCode) `shouldReturnEqual` PasswordMfaLoginRejected

    it "preserves every password and second-factor state without authenticating early" $ do
      let secret = required "TOTP secret" (mkTotpSecret "JBSWY3DPEHPK3PXPJBSWY3DPEHPK3PXP")
          encryptedSecret = required "encrypted TOTP secret" (encryptSecretWithNonce encryptionKey (ByteString.replicate 12 8) (TextEncoding.encodeUtf8 (renderTotpSecret secret)))
          confirmedEnrollment = StoredTotpEnrollment encryptedSecret (Just 100)
          validProof = TotpLoginProof (totpCode 123456 secret)
          completeWith credentialStoreValue mfaStoreValue = completePasswordLogin credentialStoreValue mfaStoreValue encryptionKey 500 123456 emailAddress (mkPassword "correct horse battery staple") validProof
      completeWith (credentialStore (Right (Just unverifiedCredential))) unexpectedMfaStore `shouldReturnEqual` PasswordMfaLoginEmailVerificationRequired accountId
      completeWith (credentialStore (Left (AccountCredentialStoreCorruptData "bad credential"))) unexpectedMfaStore `shouldReturnEqual` PasswordMfaLoginCredentialStoreError (AccountCredentialStoreCorruptData "bad credential")
      completeWith (credentialStore (Right (Just verifiedCredential))) (mfaStore (Right Nothing)) `shouldReturnEqual` PasswordMfaLoginEnrollmentRequired accountId
      secondLookupStore <- storeWithLookups [Right (Just confirmedEnrollment), Left (MfaStoreCorruptData "second lookup failed")]
      completeWith (credentialStore (Right (Just verifiedCredential))) secondLookupStore `shouldReturnEqual` PasswordMfaLoginMfaStoreError (MfaStoreCorruptData "second lookup failed")
      noEnrollmentStore <- storeWithLookups [Right (Just confirmedEnrollment), Right Nothing]
      completeWith (credentialStore (Right (Just verifiedCredential))) noEnrollmentStore `shouldReturnEqual` PasswordMfaLoginEnrollmentRequired accountId
      pendingEnrollmentStore <- storeWithLookups [Right (Just confirmedEnrollment), Right (Just (StoredTotpEnrollment encryptedSecret Nothing))]
      completeWith (credentialStore (Right (Just verifiedCredential))) pendingEnrollmentStore `shouldReturnEqual` PasswordMfaLoginEnrollmentRequired accountId
      let invalidUtf8Envelope = required "encrypted invalid UTF-8" (encryptSecretWithNonce encryptionKey (ByteString.replicate 12 9) (ByteString.pack [255]))
      completeWith (credentialStore (Right (Just verifiedCredential))) (mfaStore (Right (Just (StoredTotpEnrollment invalidUtf8Envelope (Just 100))))) `shouldReturnEqual` PasswordMfaLoginCorruptEnrollment

    it "rejects malformed, missing, and unavailable recovery-code state" $ do
      let recoveryCode = required "recovery code" (mkRecoveryCode "0123456789ABCDEF0123")
          confirmedEnrollment = StoredTotpEnrollment "recovery-code-login" (Just 100)
          completeWith store = completePasswordLogin (credentialStore (Right (Just verifiedCredential))) store encryptionKey 500 123456 emailAddress (mkPassword "correct horse battery staple") (RecoveryCodeLoginProof recoveryCode)
          storeFor recoveryResult consumptionResult =
            MfaStore
              { saveUnconfirmedTotpEnrollment = \_ _ _ -> error "unexpected enrollment save",
                loadTotpEnrollment = \_ -> pure (Right (Just confirmedEnrollment)),
                confirmTotpEnrollment = \_ _ _ -> error "unexpected enrollment confirmation",
                loadUnusedRecoveryCodeHashes = \_ -> pure recoveryResult,
                consumeRecoveryCodeHash = \_ _ _ -> pure consumptionResult
              }
      completeWith (storeFor (Left (MfaStoreUnavailable "recovery lookup failed")) (Right True)) `shouldReturnEqual` PasswordMfaLoginMfaStoreError (MfaStoreUnavailable "recovery lookup failed")
      completeWith (storeFor (Right ["not-a-password-hash"]) (Right True)) `shouldReturnEqual` PasswordMfaLoginCorruptEnrollment
      completeWith (storeFor (Right []) (Right True)) `shouldReturnEqual` PasswordMfaLoginRejected
      let recoveryCodeHash = required "recovery-code hash" (hashRecoveryCodeWithSalt defaultPasswordHashingPolicy "0123456789abcdef" recoveryCode)
      completeWith (storeFor (Right [recoveryCodeHashText recoveryCodeHash]) (Left (MfaStoreCorruptData "recovery consumption failed"))) `shouldReturnEqual` PasswordMfaLoginMfaStoreError (MfaStoreCorruptData "recovery consumption failed")

    it "keeps login outcomes and credential-store errors comparable" $ do
      AccountCredentialStoreUnavailable "unavailable" == AccountCredentialStoreUnavailable "unavailable" `shouldBe` True
      AccountCredentialStoreUnavailable "unavailable" /= AccountCredentialStoreUnavailable "other" `shouldBe` True
      AccountCredentialStoreUnavailable "unavailable" /= AccountCredentialStoreCorruptData "unavailable" `shouldBe` True
      PasswordLoginRejected == PasswordLoginRejected `shouldBe` True
      PasswordLoginRejected /= PasswordLoginMfaRequired accountId `shouldBe` True
      PasswordLoginEmailVerificationRequired accountId /= PasswordLoginMfaEnrollmentRequired accountId `shouldBe` True
      PasswordLoginCredentialStoreError (AccountCredentialStoreUnavailable "unavailable") /= PasswordLoginMfaStoreError (MfaStoreUnavailable "unavailable") `shouldBe` True
      let totpProof = TotpLoginProof (required "TOTP code" (mkTotpCode "123456"))
          recoveryProof = RecoveryCodeLoginProof (required "recovery code" (mkRecoveryCode "0123456789ABCDEF0123"))
      totpProof /= recoveryProof `shouldBe` True
      PasswordMfaLoginRejected /= PasswordMfaLoginEmailVerificationRequired accountId `shouldBe` True
      PasswordMfaLoginEnrollmentRequired accountId /= PasswordMfaLoginAccepted accountId `shouldBe` True
      PasswordMfaLoginCredentialStoreError (AccountCredentialStoreUnavailable "unavailable") /= PasswordMfaLoginMfaStoreError (MfaStoreUnavailable "unavailable") `shouldBe` True
      PasswordMfaLoginCorruptEnrollment /= PasswordMfaLoginRejected `shouldBe` True

  describe "runtime PostgreSQL credential lookup" $ do
    it "uses an email parameter and decodes verified credentials" $ do
      let store = buildRuntimePostgresAccountCredentialStoreWithRunner runner databaseConfig
          runner _ query parameters = do
            query `shouldBe` "SELECT account_id, password_hash, COALESCE(email_verified_at_nanoseconds::TEXT, '') FROM web_api.accounts WHERE email_normalized = $1;"
            parameters `shouldBe` ["person@example.test"]
            pure (Right [["account_01", encodedPasswordHash, "500"]])
      findAccountCredentialByEmail store emailAddress `shouldSatisfyEqual` \case
        Right (Just credential) ->
          accountCredentialId credential == accountId
            && passwordHashText (accountCredentialPasswordHash credential) == encodedPasswordHash
            && accountCredentialEmailVerified credential
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
credentialStore result = AccountCredentialStore (\requestedEmail -> requestedEmail `seq` pure result)

mfaStore :: Either MfaStoreError (Maybe StoredTotpEnrollment) -> MfaStore
mfaStore result =
  MfaStore
    { saveUnconfirmedTotpEnrollment = \_ _ _ -> error "unexpected enrollment save",
      loadTotpEnrollment = \requestedAccountId -> requestedAccountId `seq` pure result,
      confirmTotpEnrollment = \_ _ _ -> error "unexpected enrollment confirmation",
      loadUnusedRecoveryCodeHashes = \_ -> error "unexpected recovery-code lookup",
      consumeRecoveryCodeHash = \_ _ _ -> error "unexpected recovery-code consumption"
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
confirmedMfaStore = mfaStore (Right (Just (StoredTotpEnrollment "encrypted" (Just 100))))

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
        consumeRecoveryCodeHash = \_ _ _ -> error "unexpected recovery-code consumption"
      }

encryptionKey :: SecretEncryptionKey
encryptionKey = required "encryption key" (mkSecretEncryptionKey "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")

required :: String -> Maybe value -> value
required label = fromMaybe (error ("expected " <> label))

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
