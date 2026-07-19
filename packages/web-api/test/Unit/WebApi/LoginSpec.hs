{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Unit.WebApi.LoginSpec (spec) where

import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import HarchWeb.Account (AccountId, mkAccountId)
import HarchWeb.Email (EmailAddress, mkEmailAddress)
import HarchWeb.Password (PasswordHash, defaultPasswordHashingPolicy, hashPasswordWithSalt, mkPassword, passwordHashText)
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

    it "keeps login outcomes and credential-store errors comparable" $ do
      AccountCredentialStoreUnavailable "unavailable" == AccountCredentialStoreUnavailable "unavailable" `shouldBe` True
      AccountCredentialStoreUnavailable "unavailable" /= AccountCredentialStoreUnavailable "other" `shouldBe` True
      AccountCredentialStoreUnavailable "unavailable" /= AccountCredentialStoreCorruptData "unavailable" `shouldBe` True
      PasswordLoginRejected == PasswordLoginRejected `shouldBe` True
      PasswordLoginRejected /= PasswordLoginMfaRequired accountId `shouldBe` True
      PasswordLoginEmailVerificationRequired accountId /= PasswordLoginMfaEnrollmentRequired accountId `shouldBe` True
      PasswordLoginCredentialStoreError (AccountCredentialStoreUnavailable "unavailable") /= PasswordLoginMfaStoreError (MfaStoreUnavailable "unavailable") `shouldBe` True

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
      confirmTotpEnrollment = \_ _ _ -> error "unexpected enrollment confirmation"
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
