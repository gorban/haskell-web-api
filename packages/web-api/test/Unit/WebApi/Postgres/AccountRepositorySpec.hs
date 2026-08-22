{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Unit.WebApi.Postgres.AccountRepositorySpec (spec) where

import Control.Monad (unless)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as Text
import HarchWeb.Email (mkEmailAddress)
import HarchWeb.Password (defaultPasswordHashingPolicy, hashPasswordWithSalt, mkPassword, passwordHashText)
import HarchWeb.Username (mkUsername)
import Test.Hspec
import TestCore.CustomAssertions (expectAll)
import TestSupport.RealPostgres (defaultMigrationPostgresConfig, defaultRealPostgresConfig, ensureDefaultPostgresAvailable)
import Unit.WebApi.TestSupport (accountId, databaseConfig, emailAddress, required)
import WebApi.Config (DatabaseConfig (..))
import WebApi.Login
import WebApi.Postgres.Testing (buildRuntimePostgresAccountCredentialStore, buildRuntimePostgresAccountCredentialStoreWithRunner, newPostgresPool, runPostgresMigrationsForRuntime)

spec :: Spec
spec = do
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
      pool <- newPostgresPool (databasePoolCapacity defaultRealPostgresConfig) defaultRealPostgresConfig
      let store = buildRuntimePostgresAccountCredentialStore pool
      findAccountCredentialByEmail store (required "unknown email address" (mkEmailAddress "missing-login-credential@example.test"))
        `shouldSatisfyEqual` \case
          Right Nothing -> True
          _ -> False

encodedPasswordHash :: Text.Text
encodedPasswordHash = passwordHashText (required "password hash" (hashPasswordWithSalt defaultPasswordHashingPolicy "0123456789abcdef" (mkPassword "correct horse battery staple")))

shouldSatisfyEqual :: IO value -> (value -> Bool) -> Expectation
shouldSatisfyEqual action predicate = do
  actual <- action
  unless (predicate actual) (expectationFailure "unexpected result")
