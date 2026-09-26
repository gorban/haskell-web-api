{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Exception (evaluate)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe)
import HarchWeb.Account (mkAccountId)
import HarchWeb.RequestId (mkRequestId)
import TestSupport.RealPostgres (defaultMigrationPostgresConfig, defaultRealPostgresConfig, ensureDefaultPostgresAvailable)
import Unit.WebApi.TestSupport (databaseConfig, opaqueSession, sessionIdValue, shouldReturnEqual)
import WebApi.AccountSessionAudit (AccountSessionAuditStore (..), AccountSessionAuditStoreError (..))
import WebApi.ActivityAudit (AccountActivity (..), AccountAuditEvent (AccountSessionIssued), AuditAuthenticationMethod (PasswordAuthenticationMethod))
import WebApi.Config (DatabaseConfig (..))
import WebApi.Postgres.Testing (buildRuntimePostgresAccountSessionAuditStore, buildRuntimePostgresAccountSessionAuditStoreWithRunner, newPostgresPool, runPostgresMigrationsForRuntime, runRuntimeParameterizedRowsQuery)

spec = describe "runtime PostgreSQL atomic account-session audit persistence" $ do
  it "uses one nullable bound call for the session and required audit activity" $ do
    queriesReference <- newIORef []
    let runner runnerDatabaseConfig query parameters = do
          _ <- evaluate (databaseHost runnerDatabaseConfig)
          modifyIORef' queriesReference (<> [(query, parameters)])
          pure (Right [[sessionIdValue]])
        store = buildRuntimePostgresAccountSessionAuditStoreWithRunner runner databaseConfig
    saveAccountSessionWithAudit store opaqueSession sampleActivity >>= expectSaved
    readIORef queriesReference
      `shouldReturnEqual` [ ( "SELECT session_id FROM account_audit.issue_account_session_with_activity($1, $2, $3::BIGINT, $4::BIGINT, $5, $6, $7, $8::SMALLINT, $9, $10, $11, $12, $13);",
                              [ Just sessionIdValue,
                                Just "account_01",
                                Just "100",
                                Just "200",
                                Just "account_01",
                                Just "550e8400-e29b-41d4-a716-446655440000",
                                Just "account-session-issued",
                                Just "1",
                                Just "password",
                                Nothing,
                                Nothing,
                                Nothing,
                                Nothing
                              ]
                            )
                          ]

  it "keeps unavailable, capacity, collision, and corrupt results distinct" $ do
    let unavailableStore = buildRuntimePostgresAccountSessionAuditStoreWithRunner (\_ _ _ -> pure (Left "database unavailable")) databaseConfig
        capacityStore = buildRuntimePostgresAccountSessionAuditStoreWithRunner (\_ _ _ -> pure (Left "account audit partition capacity is exhausted")) databaseConfig
        collisionStore = buildRuntimePostgresAccountSessionAuditStoreWithRunner (\_ _ _ -> pure (Right [])) databaseConfig
        corruptStore = buildRuntimePostgresAccountSessionAuditStoreWithRunner (\_ _ _ -> pure (Right [["another-session"]])) databaseConfig
    saveAccountSessionWithAudit unavailableStore opaqueSession sampleActivity >>= expectUnavailable
    saveAccountSessionWithAudit capacityStore opaqueSession sampleActivity >>= expectCapacityExceeded
    saveAccountSessionWithAudit collisionStore opaqueSession sampleActivity >>= expectCollision
    saveAccountSessionWithAudit corruptStore opaqueSession sampleActivity >>= expectCorruptData

  it "uses the runtime role to commit the session and audit event through the controlled operation" $ do
    ensureDefaultPostgresAvailable
    runPostgresMigrationsForRuntime defaultMigrationPostgresConfig defaultRealPostgresConfig `shouldReturnEqual` Right ()
    _ <- runRuntimeParameterizedRowsQuery defaultMigrationPostgresConfig "DELETE FROM web_api.accounts WHERE account_id = $1;" ["account_01"]
    _ <- runRuntimeParameterizedRowsQuery defaultMigrationPostgresConfig "INSERT INTO web_api.accounts (account_id, email_normalized, password_hash, created_at_nanoseconds) VALUES ($1, $2, 'test-hash', 1);" ["account_01", "account-session-audit@example.test"]
    pool <- newPostgresPool (databasePoolCapacity defaultRealPostgresConfig) defaultRealPostgresConfig
    saveAccountSessionWithAudit (buildRuntimePostgresAccountSessionAuditStore pool) opaqueSession sampleActivity >>= expectSaved

sampleActivity :: AccountActivity
sampleActivity =
  AccountActivity
    { activitySubject = requiredMaybe "account id" (mkAccountId "account_01"),
      activityRequestId = requiredMaybe "request id" (mkRequestId "550e8400-e29b-41d4-a716-446655440000"),
      activityEvent = AccountSessionIssued PasswordAuthenticationMethod,
      activityRoute = Nothing
    }

requiredMaybe :: String -> Maybe value -> value
requiredMaybe label = fromMaybe (error ("expected valid " <> label))

expectUnavailable :: Either AccountSessionAuditStoreError Bool -> Expectation
expectUnavailable = \case
  Left AccountSessionAuditStoreUnavailable -> pure ()
  _ -> expectationFailure "expected unavailable account-session audit storage"

expectCapacityExceeded :: Either AccountSessionAuditStoreError Bool -> Expectation
expectCapacityExceeded = \case
  Left AccountSessionAuditCapacityExceeded -> pure ()
  _ -> expectationFailure "expected exhausted account-audit partition capacity"

expectCorruptData :: Either AccountSessionAuditStoreError Bool -> Expectation
expectCorruptData = \case
  Left AccountSessionAuditStoreCorruptData -> pure ()
  _ -> expectationFailure "expected corrupt account-session audit storage data"

expectSaved :: Either AccountSessionAuditStoreError Bool -> Expectation
expectSaved = \case
  Right True -> pure ()
  _ -> expectationFailure "expected the account session and activity to be saved"

expectCollision :: Either AccountSessionAuditStoreError Bool -> Expectation
expectCollision = \case
  Right False -> pure ()
  _ -> expectationFailure "expected an account-session identifier collision"
