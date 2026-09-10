{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Exception (evaluate)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Account (emailVerificationTokenDigest, emailVerificationTokenDigestText, mkAccountId, mkEmailVerificationToken)
import HarchWeb.Account qualified as Account
import HarchWeb.RequestId (mkRequestId)
import TestSupport.RealPostgres (defaultMigrationPostgresConfig, defaultRealPostgresConfig, ensureDefaultPostgresAvailable)
import Unit.WebApi.TestSupport (databaseConfig, shouldReturnEqual)
import WebApi.Account (PendingRegistrationClaim (..), PendingRegistrationDeliveryStage (PendingRegistrationCreated))
import WebApi.ActivityAudit (AccountActivity (..), AccountAuditEvent (PendingRegistrationDelivered), AuditRegistrationDeliveryStage (RegistrationCreated))
import WebApi.Config (DatabaseConfig (..))
import WebApi.PendingRegistrationAudit (PendingRegistrationAuditStore (..), PendingRegistrationAuditStoreError (..))
import WebApi.Postgres.Testing (buildRuntimePostgresPendingRegistrationAuditStore, buildRuntimePostgresPendingRegistrationAuditStoreWithRunner, newPostgresPool, runPostgresMigrationsForRuntime, runRuntimeParameterizedRowsQuery)

spec = describe "runtime PostgreSQL atomic pending-registration delivery audit persistence" $ do
  it "uses one nullable bound call for the claim settlement and required audit activity" $ do
    queriesReference <- newIORef []
    let runner runnerDatabaseConfig query parameters = do
          _ <- evaluate (databaseHost runnerDatabaseConfig)
          modifyIORef' queriesReference (<> [(query, parameters)])
          pure (Right [["account_01"]])
        store = buildRuntimePostgresPendingRegistrationAuditStoreWithRunner runner databaseConfig
    completePendingRegistrationDeliveryWithAudit store sampleClaim sampleActivity >>= expectCompleted
    readIORef queriesReference
      `shouldReturnEqual` [ ( "SELECT account_id FROM account_audit.complete_pending_registration_delivery_with_activity($1, $2, $3, $4, $5, $6::SMALLINT, $7, $8, $9, $10, $11);",
                              [ Just "account_01",
                                Just sampleClaimTokenDigestText,
                                Just "account_01",
                                Just "550e8400-e29b-41d4-a716-446655440000",
                                Just "pending-registration-delivered",
                                Just "1",
                                Just "created",
                                Nothing,
                                Nothing,
                                Nothing,
                                Nothing
                              ]
                            )
                          ]

  it "keeps unavailable, capacity, lost-claim, and corrupt results distinct" $ do
    let unavailableStore = buildRuntimePostgresPendingRegistrationAuditStoreWithRunner (\_ _ _ -> pure (Left "database unavailable")) databaseConfig
        capacityStore = buildRuntimePostgresPendingRegistrationAuditStoreWithRunner (\_ _ _ -> pure (Left "account audit partition capacity is exhausted")) databaseConfig
        lostClaimStore = buildRuntimePostgresPendingRegistrationAuditStoreWithRunner (\_ _ _ -> pure (Right [])) databaseConfig
        corruptStore = buildRuntimePostgresPendingRegistrationAuditStoreWithRunner (\_ _ _ -> pure (Right [["another-account"]])) databaseConfig
    completePendingRegistrationDeliveryWithAudit unavailableStore sampleClaim sampleActivity >>= expectUnavailable
    completePendingRegistrationDeliveryWithAudit capacityStore sampleClaim sampleActivity >>= expectCapacityExceeded
    completePendingRegistrationDeliveryWithAudit lostClaimStore sampleClaim sampleActivity >>= expectLostClaim
    completePendingRegistrationDeliveryWithAudit corruptStore sampleClaim sampleActivity >>= expectCorruptData

  it "uses the runtime role to commit the claimed delivery and audit event through the controlled operation" $ do
    ensureDefaultPostgresAvailable
    runPostgresMigrationsForRuntime defaultMigrationPostgresConfig defaultRealPostgresConfig `shouldReturnEqual` Right ()
    _ <- runRuntimeParameterizedRowsQuery defaultMigrationPostgresConfig "DELETE FROM web_api.accounts WHERE account_id = $1;" ["account_01"]
    _ <- runRuntimeParameterizedRowsQuery defaultMigrationPostgresConfig "INSERT INTO web_api.accounts (account_id, email_normalized, password_hash, created_at_nanoseconds) VALUES ($1, $2, 'test-hash', 1);" ["account_01", "pending-registration-audit@example.test"]
    _ <- runRuntimeParameterizedRowsQuery defaultMigrationPostgresConfig "INSERT INTO web_api.email_verifications (token_digest, account_id, email_normalized, expires_at_nanoseconds, delivery_state, delivery_claimed_at_nanoseconds) VALUES ($1, $2, $3, 2, 'claimed', 1);" [sampleClaimTokenDigestText, "account_01", "pending-registration-audit@example.test"]
    pool <- newPostgresPool (databasePoolCapacity defaultRealPostgresConfig) defaultRealPostgresConfig
    completePendingRegistrationDeliveryWithAudit (buildRuntimePostgresPendingRegistrationAuditStore pool) sampleClaim sampleActivity >>= expectCompleted

sampleClaim :: PendingRegistrationClaim
sampleClaim =
  PendingRegistrationClaim
    { pendingRegistrationClaimAccountId = requiredMaybe "account id" (mkAccountId "account_01"),
      pendingRegistrationClaimTokenDigest = sampleClaimTokenDigest,
      pendingRegistrationClaimStage = PendingRegistrationCreated
    }

sampleClaimTokenDigest :: Account.EmailVerificationTokenDigest
sampleClaimTokenDigest = emailVerificationTokenDigest sampleClaimToken

sampleClaimTokenDigestText :: Text
sampleClaimTokenDigestText = emailVerificationTokenDigestText sampleClaimTokenDigest

sampleClaimToken :: Account.EmailVerificationToken
sampleClaimToken = requiredMaybe "verification token" (mkEmailVerificationToken (Text.replicate 43 "a"))

sampleActivity :: AccountActivity
sampleActivity =
  AccountActivity
    { activitySubject = requiredMaybe "account id" (mkAccountId "account_01"),
      activityRequestId = requiredMaybe "request id" (mkRequestId "550e8400-e29b-41d4-a716-446655440000"),
      activityEvent = PendingRegistrationDelivered RegistrationCreated,
      activityRoute = Nothing
    }

requiredMaybe :: String -> Maybe value -> value
requiredMaybe label = fromMaybe (error ("expected valid " <> label))

expectUnavailable :: Either PendingRegistrationAuditStoreError Bool -> Expectation
expectUnavailable = \case
  Left PendingRegistrationAuditStoreUnavailable -> pure ()
  _ -> expectationFailure "expected unavailable pending-registration audit storage"

expectCapacityExceeded :: Either PendingRegistrationAuditStoreError Bool -> Expectation
expectCapacityExceeded = \case
  Left PendingRegistrationAuditCapacityExceeded -> pure ()
  _ -> expectationFailure "expected exhausted account-audit partition capacity"

expectCorruptData :: Either PendingRegistrationAuditStoreError Bool -> Expectation
expectCorruptData = \case
  Left PendingRegistrationAuditStoreCorruptData -> pure ()
  _ -> expectationFailure "expected corrupt pending-registration audit storage data"

expectCompleted :: Either PendingRegistrationAuditStoreError Bool -> Expectation
expectCompleted = \case
  Right True -> pure ()
  Right False -> expectationFailure "expected the pending registration delivery and activity to be saved, but the claim was lost"
  Left PendingRegistrationAuditStoreUnavailable -> expectationFailure "expected the pending registration delivery and activity to be saved, but storage was unavailable"
  Left PendingRegistrationAuditCapacityExceeded -> expectationFailure "expected the pending registration delivery and activity to be saved, but audit capacity was exhausted"
  Left PendingRegistrationAuditStoreCorruptData -> expectationFailure "expected the pending registration delivery and activity to be saved, but PostgreSQL returned corrupt data"

expectLostClaim :: Either PendingRegistrationAuditStoreError Bool -> Expectation
expectLostClaim = \case
  Right False -> pure ()
  _ -> expectationFailure "expected a pending-registration delivery claim collision"
