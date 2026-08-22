{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Unit.WebApi.Postgres.SessionRepositorySpec (spec) where

import Control.Exception (evaluate)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text qualified as Text
import HarchWeb.Session (generateSessionId)
import Test.Hspec
import TestSupport.RealPostgres (defaultMigrationPostgresConfig, defaultRealPostgresConfig, ensureDefaultPostgresAvailable)
import Unit.WebApi.TestSupport (csrfTokenValue, databaseConfig, opaqueSession, sessionIdValue, shouldReturnEqual, testSessionId)
import WebApi.Config (DatabaseConfig (..))
import WebApi.Postgres.Testing (buildRuntimePostgresAccountSessionStore, buildRuntimePostgresAccountSessionStoreWithRunner, newPostgresPool, runPostgresMigrationsForRuntime)
import WebApi.Session
  ( AccountSessionStore (..),
    AccountSessionStoreError (..),
  )

spec :: Spec
spec = do
  describe "runtime PostgreSQL account-session persistence" $ do
    it "uses bound parameters to save, load, and invalidate an opaque session" $ do
      queriesReference <- newIORef []
      let runner runnerDatabaseConfig query parameters = do
            _ <- evaluate (databaseHost runnerDatabaseConfig)
            modifyIORef' queriesReference ((query, parameters) :)
            pure $
              if "INSERT INTO web_api.account_sessions" `Text.isInfixOf` query
                then Right [[sessionIdValue]]
                else
                  if "SELECT account_id, csrf_token" `Text.isInfixOf` query
                    then Right [["account_01", csrfTokenValue, "100", "200"]]
                    else
                      if "UPDATE web_api.account_sessions" `Text.isInfixOf` query
                        then Right [[sessionIdValue]]
                        else Left "unexpected query"
          store = buildRuntimePostgresAccountSessionStoreWithRunner runner databaseConfig
      saveAccountSession store opaqueSession `shouldReturnEqual` Right True
      loadAccountSession store testSessionId `shouldReturnEqual` Right (Just opaqueSession)
      invalidateAccountSession store testSessionId 300 `shouldReturnEqual` Right True
      recordedQueries <- reverse <$> readIORef queriesReference
      recordedQueries
        `shouldBe` [ ( "INSERT INTO web_api.account_sessions (session_id, account_id, csrf_token, issued_at_nanoseconds, expires_at_nanoseconds) VALUES ($1, $2, $3, $4, $5) ON CONFLICT (session_id) DO NOTHING RETURNING session_id;",
                       [sessionIdValue, "account_01", csrfTokenValue, "100", "200"]
                     ),
                     ( "SELECT account_id, csrf_token, issued_at_nanoseconds::TEXT, expires_at_nanoseconds::TEXT FROM web_api.account_sessions WHERE session_id = $1 AND invalidated_at_nanoseconds IS NULL;",
                       [sessionIdValue]
                     ),
                     ( "UPDATE web_api.account_sessions SET invalidated_at_nanoseconds = $2 WHERE session_id = $1 AND invalidated_at_nanoseconds IS NULL RETURNING session_id;",
                       [sessionIdValue, "300"]
                     )
                   ]

    it "preserves unavailable, declined, and corrupt database outcomes" $ do
      let unavailableStore = buildRuntimePostgresAccountSessionStoreWithRunner (\_ _ _ -> pure (Left "database unavailable")) databaseConfig
          declinedStore = buildRuntimePostgresAccountSessionStoreWithRunner (\_ _ _ -> pure (Right [])) databaseConfig
          malformedStore = buildRuntimePostgresAccountSessionStoreWithRunner (\_ _ _ -> pure (Right [["account_01", csrfTokenValue, "not-a-time", "200"]])) databaseConfig
          wrongSessionStore = buildRuntimePostgresAccountSessionStoreWithRunner (\_ _ _ -> pure (Right [["other-session"]])) databaseConfig
      saveAccountSession unavailableStore opaqueSession `shouldReturnEqual` Left AccountSessionStoreUnavailable
      saveAccountSession declinedStore opaqueSession `shouldReturnEqual` Right False
      saveAccountSession wrongSessionStore opaqueSession `shouldReturnEqual` Left AccountSessionStoreCorruptData
      loadAccountSession unavailableStore testSessionId `shouldReturnEqual` Left AccountSessionStoreUnavailable
      loadAccountSession declinedStore testSessionId `shouldReturnEqual` Right Nothing
      loadAccountSession malformedStore testSessionId `shouldReturnEqual` Left AccountSessionStoreCorruptData
      loadAccountSession (buildRuntimePostgresAccountSessionStoreWithRunner (\_ _ _ -> pure (Right [["invalid account", csrfTokenValue, "100", "200"]])) databaseConfig) testSessionId
        `shouldReturnEqual` Left AccountSessionStoreCorruptData
      loadAccountSession (buildRuntimePostgresAccountSessionStoreWithRunner (\_ _ _ -> pure (Right [["account_01", "invalid-csrf", "100", "200"]])) databaseConfig) testSessionId
        `shouldReturnEqual` Left AccountSessionStoreCorruptData
      loadAccountSession (buildRuntimePostgresAccountSessionStoreWithRunner (\_ _ _ -> pure (Right [["account_01", csrfTokenValue, "100"]])) databaseConfig) testSessionId
        `shouldReturnEqual` Left AccountSessionStoreCorruptData
      invalidateAccountSession unavailableStore testSessionId 300 `shouldReturnEqual` Left AccountSessionStoreUnavailable
      invalidateAccountSession declinedStore testSessionId 300 `shouldReturnEqual` Right False
      invalidateAccountSession wrongSessionStore testSessionId 300 `shouldReturnEqual` Left AccountSessionStoreCorruptData
      loadAccountSession (buildRuntimePostgresAccountSessionStoreWithRunner (\runnerDatabaseConfig _ _ -> evaluate (databaseHost runnerDatabaseConfig) >> pure (Right [["wrong", "shape"]])) databaseConfig) testSessionId
        `shouldReturnEqual` Left AccountSessionStoreCorruptData

    it "covers invalidation-specific persistence outcomes" $ do
      let store result =
            buildRuntimePostgresAccountSessionStoreWithRunner
              ( \runnerDatabaseConfig query _ -> do
                  _ <- evaluate (databaseHost runnerDatabaseConfig)
                  if "UPDATE web_api.account_sessions" `Text.isInfixOf` query
                    then pure result
                    else pure (Left "unexpected query")
              )
              databaseConfig
      invalidateAccountSession (store (Left "database unavailable")) testSessionId 300 `shouldReturnEqual` Left AccountSessionStoreUnavailable
      invalidateAccountSession (store (Right [])) testSessionId 300 `shouldReturnEqual` Right False
      invalidateAccountSession (store (Right [["other-session"]])) testSessionId 300 `shouldReturnEqual` Left AccountSessionStoreCorruptData

    it "executes the native libpq session adapter against a migrated PostgreSQL database" $ do
      ensureDefaultPostgresAvailable
      runPostgresMigrationsForRuntime defaultMigrationPostgresConfig defaultRealPostgresConfig `shouldReturn` Right ()
      unknownSessionId <- generateSessionId
      pool <- newPostgresPool (databasePoolCapacity defaultRealPostgresConfig) defaultRealPostgresConfig
      loadAccountSession (buildRuntimePostgresAccountSessionStore pool) unknownSessionId `shouldReturnEqual` Right Nothing

    it "keeps the account-session errors comparable without exposing persistence details" $ do
      AccountSessionStoreUnavailable /= AccountSessionStoreCorruptData `shouldBe` True
