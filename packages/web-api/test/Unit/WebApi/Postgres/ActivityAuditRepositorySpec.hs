{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Exception (evaluate)
import Control.Monad (void)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe)
import HarchWeb.Account (mkAccountId)
import HarchWeb.RequestId (mkRequestId)
import TestSupport.RealPostgres (defaultMigrationPostgresConfig, defaultRealPostgresConfig, ensureDefaultPostgresAvailable)
import WebApi.ActivityAudit
import WebApi.Config (DatabaseConfig (..))
import WebApi.Postgres.Testing (buildRuntimePostgresActivityAuditStore, buildRuntimePostgresActivityAuditStoreWithRunner, newPostgresPool, runPostgresMigrationsForRuntime)

spec = describe "runtime PostgreSQL account-audit persistence" $ do
  it "uses one nullable bound call to the controlled append function" $ do
    recordedQueriesReference <- newIORef []
    let runner runnerDatabaseConfig query parameters = do
          _ <- evaluate (databaseHost runnerDatabaseConfig)
          modifyIORef' recordedQueriesReference (<> [(query, parameters)])
          pure (Right [["42"]])
        store = buildRuntimePostgresActivityAuditStoreWithRunner runner defaultRealPostgresConfig
    appendAccountActivity store sampleActivity >>= expectAppended
    recordedQueries <- readIORef recordedQueriesReference
    recordedQueries
      `shouldBe` [ ( "SELECT activity_id::TEXT FROM account_audit.append_activity($1, $2, $3, $4::SMALLINT, $5, $6, $7, $8, $9);",
                     [ Just "account_audit_test",
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

  it "keeps database failure, capacity exhaustion, and malformed function output on explicit rails" $ do
    let unavailableStore = buildRuntimePostgresActivityAuditStoreWithRunner (\_ _ _ -> pure (Left "database unavailable")) defaultRealPostgresConfig
        capacityStore = buildRuntimePostgresActivityAuditStoreWithRunner (\_ _ _ -> pure (Left "ERROR: account audit partition capacity is exhausted")) defaultRealPostgresConfig
        malformedStore = buildRuntimePostgresActivityAuditStoreWithRunner (\_ _ _ -> pure (Right [["not-an-activity-id"]])) defaultRealPostgresConfig
        wrongShapeStore = buildRuntimePostgresActivityAuditStoreWithRunner (\_ _ _ -> pure (Right [["42", "unexpected"]])) defaultRealPostgresConfig
    appendAccountActivity unavailableStore sampleActivity >>= expectError ActivityAuditUnavailable
    appendAccountActivity capacityStore sampleActivity >>= expectError ActivityAuditCapacityExceeded
    appendAccountActivity malformedStore sampleActivity >>= expectError ActivityAuditCorruptResult
    appendAccountActivity wrongShapeStore sampleActivity >>= expectError ActivityAuditCorruptResult

  it "executes the native pooled adapter with SQL NULL route values" $ do
    ensureDefaultPostgresAvailable
    runPostgresMigrationsForRuntime defaultMigrationPostgresConfig defaultRealPostgresConfig `shouldReturn` Right ()
    pool <- newPostgresPool (databasePoolCapacity defaultRealPostgresConfig) defaultRealPostgresConfig
    appendAccountActivity (buildRuntimePostgresActivityAuditStore pool) sampleActivity >>= expectAppended

sampleActivity :: AccountActivity
sampleActivity =
  AccountActivity
    { activitySubject = requiredMaybe "account id" (mkAccountId "account_audit_test"),
      activityRequestId = requiredMaybe "request id" (mkRequestId "550e8400-e29b-41d4-a716-446655440000"),
      activityEvent = AccountSessionIssued PasswordAuthenticationMethod,
      activityRoute = Nothing
    }

expectAppended :: Either ActivityAuditStoreError activityId -> Expectation
expectAppended appendResult =
  case appendResult of
    Left ActivityAuditUnavailable -> expectationFailure "expected a committed audit activity"
    Left ActivityAuditCapacityExceeded -> expectationFailure "expected a committed audit activity"
    Left ActivityAuditCorruptResult -> expectationFailure "expected a committed audit activity"
    Right activityId -> void (evaluate activityId)

expectError :: ActivityAuditStoreError -> Either ActivityAuditStoreError activityId -> Expectation
expectError expectedError actualResult =
  case (expectedError, actualResult) of
    (ActivityAuditUnavailable, Left ActivityAuditUnavailable) -> pure ()
    (ActivityAuditCapacityExceeded, Left ActivityAuditCapacityExceeded) -> pure ()
    (ActivityAuditCorruptResult, Left ActivityAuditCorruptResult) -> pure ()
    _ -> expectationFailure "expected the matching account-audit storage error"

requiredMaybe :: String -> Maybe value -> value
requiredMaybe label = fromMaybe (error ("expected valid " <> label))
