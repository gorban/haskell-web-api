{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Exception (evaluate)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text qualified as Text
import HarchWeb.Session (generateSessionId)
import TestSupport.RealPostgres (defaultMigrationPostgresConfig, defaultRealPostgresConfig, ensureDefaultPostgresAvailable)
import Unit.WebApi.TestSupport (databaseConfig, opaqueSession, sessionIdValue, shouldReturnEqual, testSessionId)
import WebApi.Config (DatabaseConfig (..))
import WebApi.Postgres.Testing (buildRuntimePostgresMfaEnrollmentSessionStore, buildRuntimePostgresMfaEnrollmentSessionStoreWithRunner, newPostgresPool, runPostgresMigrationsForRuntime)
import WebApi.Session (MfaEnrollmentSessionStore (..), MfaEnrollmentSessionStoreError (..))

spec = do
  describe "runtime PostgreSQL MFA-enrollment-session persistence" $ do
    it "uses bound parameters to save, load, and invalidate an opaque session" $ do
      queriesReference <- newIORef []
      let runner runnerDatabaseConfig query parameters = do
            _ <- evaluate (databaseHost runnerDatabaseConfig)
            modifyIORef' queriesReference ((query, parameters) :)
            pure $
              if "INSERT INTO web_api.mfa_enrollment_sessions" `Text.isInfixOf` query
                then Right [[sessionIdValue]]
                else
                  if "SELECT account_id, issued_at_nanoseconds" `Text.isInfixOf` query
                    then Right [["account_01", "100", "200"]]
                    else
                      if "UPDATE web_api.mfa_enrollment_sessions" `Text.isInfixOf` query
                        then Right [[sessionIdValue]]
                        else Left "unexpected query"
          store = buildRuntimePostgresMfaEnrollmentSessionStoreWithRunner runner databaseConfig
      saveMfaEnrollmentSession store opaqueSession `shouldReturnEqual` Right True
      loadMfaEnrollmentSession store testSessionId `shouldReturnEqual` Right (Just opaqueSession)
      invalidateMfaEnrollmentSession store testSessionId 300 `shouldReturnEqual` Right True
      recordedQueries <- reverse <$> readIORef queriesReference
      recordedQueries
        `shouldBe` [ ( "INSERT INTO web_api.mfa_enrollment_sessions (session_id, account_id, issued_at_nanoseconds, expires_at_nanoseconds) VALUES ($1, $2, $3, $4) ON CONFLICT (session_id) DO NOTHING RETURNING session_id;",
                       [sessionIdValue, "account_01", "100", "200"]
                     ),
                     ( "SELECT account_id, issued_at_nanoseconds::TEXT, expires_at_nanoseconds::TEXT FROM web_api.mfa_enrollment_sessions WHERE session_id = $1 AND invalidated_at_nanoseconds IS NULL;",
                       [sessionIdValue]
                     ),
                     ( "UPDATE web_api.mfa_enrollment_sessions SET invalidated_at_nanoseconds = $2 WHERE session_id = $1 AND invalidated_at_nanoseconds IS NULL RETURNING session_id;",
                       [sessionIdValue, "300"]
                     )
                   ]

    it "preserves unavailable, declined, and corrupt database outcomes" $ do
      let unavailableStore = buildRuntimePostgresMfaEnrollmentSessionStoreWithRunner (\_ _ _ -> pure (Left "database unavailable")) databaseConfig
          declinedStore = buildRuntimePostgresMfaEnrollmentSessionStoreWithRunner (\_ _ _ -> pure (Right [])) databaseConfig
          malformedStore = buildRuntimePostgresMfaEnrollmentSessionStoreWithRunner (\_ _ _ -> pure (Right [["account_01", "not-a-time", "200"]])) databaseConfig
          wrongSessionStore = buildRuntimePostgresMfaEnrollmentSessionStoreWithRunner (\_ _ _ -> pure (Right [["other-session"]])) databaseConfig
      saveMfaEnrollmentSession unavailableStore opaqueSession `shouldReturnEqual` Left MfaEnrollmentSessionStoreUnavailable
      saveMfaEnrollmentSession declinedStore opaqueSession `shouldReturnEqual` Right False
      saveMfaEnrollmentSession wrongSessionStore opaqueSession `shouldReturnEqual` Left MfaEnrollmentSessionStoreCorruptData
      loadMfaEnrollmentSession unavailableStore testSessionId `shouldReturnEqual` Left MfaEnrollmentSessionStoreUnavailable
      loadMfaEnrollmentSession declinedStore testSessionId `shouldReturnEqual` Right Nothing
      loadMfaEnrollmentSession malformedStore testSessionId `shouldReturnEqual` Left MfaEnrollmentSessionStoreCorruptData
      loadMfaEnrollmentSession (buildRuntimePostgresMfaEnrollmentSessionStoreWithRunner (\_ _ _ -> pure (Right [["invalid account", "100", "200"]])) databaseConfig) testSessionId
        `shouldReturnEqual` Left MfaEnrollmentSessionStoreCorruptData
      loadMfaEnrollmentSession (buildRuntimePostgresMfaEnrollmentSessionStoreWithRunner (\_ _ _ -> pure (Right [["account_01", "invalid-time", "200"]])) databaseConfig) testSessionId
        `shouldReturnEqual` Left MfaEnrollmentSessionStoreCorruptData
      loadMfaEnrollmentSession (buildRuntimePostgresMfaEnrollmentSessionStoreWithRunner (\_ _ _ -> pure (Right [["account_01", "100"]])) databaseConfig) testSessionId
        `shouldReturnEqual` Left MfaEnrollmentSessionStoreCorruptData
      invalidateMfaEnrollmentSession unavailableStore testSessionId 300 `shouldReturnEqual` Left MfaEnrollmentSessionStoreUnavailable
      invalidateMfaEnrollmentSession declinedStore testSessionId 300 `shouldReturnEqual` Right False
      invalidateMfaEnrollmentSession wrongSessionStore testSessionId 300 `shouldReturnEqual` Left MfaEnrollmentSessionStoreCorruptData
      loadMfaEnrollmentSession (buildRuntimePostgresMfaEnrollmentSessionStoreWithRunner (\runnerDatabaseConfig _ _ -> evaluate (databaseHost runnerDatabaseConfig) >> pure (Right [["wrong", "shape"]])) databaseConfig) testSessionId
        `shouldReturnEqual` Left MfaEnrollmentSessionStoreCorruptData

    it "covers invalidation-specific persistence outcomes" $ do
      let store result =
            buildRuntimePostgresMfaEnrollmentSessionStoreWithRunner
              ( \runnerDatabaseConfig query _ -> do
                  _ <- evaluate (databaseHost runnerDatabaseConfig)
                  if "UPDATE web_api.mfa_enrollment_sessions" `Text.isInfixOf` query
                    then pure result
                    else pure (Left "unexpected query")
              )
              databaseConfig
      invalidateMfaEnrollmentSession (store (Left "database unavailable")) testSessionId 300 `shouldReturnEqual` Left MfaEnrollmentSessionStoreUnavailable
      invalidateMfaEnrollmentSession (store (Right [])) testSessionId 300 `shouldReturnEqual` Right False
      invalidateMfaEnrollmentSession (store (Right [["other-session"]])) testSessionId 300 `shouldReturnEqual` Left MfaEnrollmentSessionStoreCorruptData

    it "executes the native libpq MFA-enrollment-session adapter against a migrated PostgreSQL database" $ do
      ensureDefaultPostgresAvailable
      runPostgresMigrationsForRuntime defaultMigrationPostgresConfig defaultRealPostgresConfig `shouldReturn` Right ()
      unknownSessionId <- generateSessionId
      pool <- newPostgresPool (databasePoolCapacity defaultRealPostgresConfig) defaultRealPostgresConfig
      loadMfaEnrollmentSession (buildRuntimePostgresMfaEnrollmentSessionStore pool) unknownSessionId `shouldReturnEqual` Right Nothing

    it "keeps the MFA-enrollment-session errors comparable without exposing persistence details" $ do
      MfaEnrollmentSessionStoreUnavailable /= MfaEnrollmentSessionStoreCorruptData `shouldBe` True
