{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Unit.WebApi.Postgres.LoginAttemptRepositorySpec (spec) where

import Control.Monad (unless)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text qualified as Text
import HarchWeb.Account (accountIdText, generateAccountId)
import HarchWeb.LoginProtection (LoginAttempt (..))
import Test.Hspec
import TestSupport.RealPostgres (defaultMigrationPostgresConfig, defaultRealPostgresConfig, ensureDefaultPostgresAvailable)
import WebApi.Config (DatabaseConfig (..))
import WebApi.Login (LoginAttemptStore (..), LoginAttemptStoreError (..))
import WebApi.Postgres.Testing (buildRuntimePostgresLoginAttemptStore, buildRuntimePostgresLoginAttemptStoreWithRunner, newPostgresPool, runPostgresMigrationsForRuntime)

spec :: Spec
spec = do
  describe "runtime PostgreSQL login-attempt persistence" $ do
    it "uses bound parameters to record and load recent login attempts" $ do
      queriesReference <- newIORef []
      let runner _databaseConfig query parameters = do
            modifyIORef' queriesReference ((query, parameters) :)
            pure $
              if "INSERT INTO web_api.login_attempts" `Text.isInfixOf` query
                then Right [["ignored"]]
                else
                  if "SELECT attempted_at_nanoseconds, succeeded" `Text.isInfixOf` query
                    then Right [["500", "true"], ["600", "false"]]
                    else Left "unexpected query"
          store = buildRuntimePostgresLoginAttemptStoreWithRunner runner databaseConfig
      recordLoginAttempt store "email:person@example.test" (LoginAttempt 500 True) `shouldReturnEqual` Right ()
      loadRecentLoginAttempts store "email:person@example.test" 100
        `shouldReturnEqual` Right [LoginAttempt 500 True, LoginAttempt 600 False]
      recordedQueries <- reverse <$> readIORef queriesReference
      recordedQueries
        `shouldBe` [ ( "INSERT INTO web_api.login_attempts (attempt_key, attempted_at_nanoseconds, succeeded) VALUES ($1, $2, $3) RETURNING attempt_key;",
                       ["email:person@example.test", "500", "true"]
                     ),
                     ( "SELECT attempted_at_nanoseconds, succeeded FROM web_api.login_attempts WHERE attempt_key = $1 AND attempted_at_nanoseconds >= $2 ORDER BY attempted_at_nanoseconds ASC;",
                       ["email:person@example.test", "100"]
                     )
                   ]

    it "preserves unavailable and corrupt database outcomes" $ do
      let unavailableStore = buildRuntimePostgresLoginAttemptStoreWithRunner (\_ _ _ -> pure (Left "database unavailable")) databaseConfig
          malformedTimestampStore = buildRuntimePostgresLoginAttemptStoreWithRunner (\_ _ _ -> pure (Right [["not-a-timestamp", "true"]])) databaseConfig
          malformedSucceededStore = buildRuntimePostgresLoginAttemptStoreWithRunner (\_ _ _ -> pure (Right [["500", "not-a-bool"]])) databaseConfig
          shortRowStore = buildRuntimePostgresLoginAttemptStoreWithRunner (\_ _ _ -> pure (Right [["500"]])) databaseConfig
      recordLoginAttempt unavailableStore "key" (LoginAttempt 500 True) `shouldReturnEqual` Left (LoginAttemptStoreUnavailable "database unavailable")
      loadRecentLoginAttempts unavailableStore "key" 0 `shouldReturnEqual` Left (LoginAttemptStoreUnavailable "database unavailable")
      loadRecentLoginAttempts malformedTimestampStore "key" 0
        `shouldReturnEqual` Left (LoginAttemptStoreCorruptData "unexpected login-attempt lookup result: [[\"not-a-timestamp\",\"true\"]]")
      loadRecentLoginAttempts malformedSucceededStore "key" 0
        `shouldReturnEqual` Left (LoginAttemptStoreCorruptData "unexpected login-attempt lookup result: [[\"500\",\"not-a-bool\"]]")
      loadRecentLoginAttempts shortRowStore "key" 0
        `shouldReturnEqual` Left (LoginAttemptStoreCorruptData "unexpected login-attempt lookup result: [[\"500\"]]")

    it "keeps login-attempt errors comparable" $ do
      (LoginAttemptStoreUnavailable "same" == LoginAttemptStoreUnavailable "same") `shouldBe` True
      (LoginAttemptStoreUnavailable "same" /= LoginAttemptStoreCorruptData "same") `shouldBe` True

    it "executes the native libpq login-attempt adapter against a migrated PostgreSQL database" $ do
      ensureDefaultPostgresAvailable
      runPostgresMigrationsForRuntime defaultMigrationPostgresConfig defaultRealPostgresConfig
        `shouldReturn` Right ()
      attemptKey <- accountIdText <$> generateAccountId
      pool <- newPostgresPool (databasePoolCapacity defaultRealPostgresConfig) defaultRealPostgresConfig
      let store = buildRuntimePostgresLoginAttemptStore pool
      loadRecentLoginAttempts store attemptKey 0 `shouldReturnEqual` Right []
      recordLoginAttempt store attemptKey (LoginAttempt 500 False) `shouldReturnEqual` Right ()
      recordLoginAttempt store attemptKey (LoginAttempt 600 True) `shouldReturnEqual` Right ()
      loadRecentLoginAttempts store attemptKey 500
        `shouldReturnEqual` Right [LoginAttempt 500 False, LoginAttempt 600 True]
      loadRecentLoginAttempts store attemptKey 550 `shouldReturnEqual` Right [LoginAttempt 600 True]

shouldReturnEqual :: (Eq value) => IO value -> value -> Expectation
shouldReturnEqual action expected = do
  actual <- action
  unless (actual == expected) (expectationFailure "unexpected result")

databaseConfig :: DatabaseConfig
databaseConfig =
  DatabaseConfig
    { databaseHost = "127.0.0.1",
      databasePort = 5432,
      databaseName = "web_api_test",
      databaseUser = "web_api_runtime",
      databasePassword = "password",
      databaseConnectTimeoutSeconds = 10,
      databasePoolCapacity = 10
    }
