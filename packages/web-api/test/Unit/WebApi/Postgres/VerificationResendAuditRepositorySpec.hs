{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Exception (evaluate)
import Data.Either (isRight)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Account (emailVerificationTokenDigest, emailVerificationTokenDigestText, mkAccountId, mkEmailVerificationToken)
import HarchWeb.Account qualified as Account
import HarchWeb.RequestId (mkRequestId)
import HarchWeb.Time (unixTimeNanoseconds)
import TestSupport.RealPostgres (defaultMigrationPostgresConfig, defaultRealPostgresConfig, ensureDefaultPostgresAvailable)
import Unit.WebApi.TestSupport (databaseConfig, shouldReturnEqual)
import WebApi.Account (VerificationResendClaim (..), VerificationResendClaimSettlement (..))
import WebApi.ActivityAudit (AccountActivity (..), AccountAuditEvent (VerificationResendDelivered))
import WebApi.Config (DatabaseConfig (..))
import WebApi.Postgres.Testing (buildRuntimePostgresVerificationResendAuditStore, buildRuntimePostgresVerificationResendAuditStoreWithRunner, newPostgresPool, runPostgresMigrationsForRuntime, runRuntimeParameterizedRowsQuery)
import WebApi.VerificationResendAudit (VerificationResendAuditStore (..), VerificationResendAuditStoreError (..))

spec = describe "runtime PostgreSQL atomic verification-resend audit persistence" $ do
  it "uses one nullable bound call for resend settlement and required audit activity" $ do
    queriesReference <- newIORef []
    let runner runnerDatabaseConfig query parameters = do
          _ <- evaluate (databaseHost runnerDatabaseConfig)
          modifyIORef' queriesReference (<> [(query, parameters)])
          pure (Right [["settled", "account_01"]])
        store = buildRuntimePostgresVerificationResendAuditStoreWithRunner runner databaseConfig
    completeVerificationResendWithAudit store sampleClaim (unixTimeNanoseconds 500) sampleActivity >>= expectSettled
    readIORef queriesReference
      `shouldReturnEqual` [ ( "SELECT outcome, value FROM account_audit.complete_verification_resend_with_activity($1, $2, $3, $4, $5, $6, $7::SMALLINT, $8, $9, $10, $11, $12);",
                              [ Just "account_01",
                                Just sampleClaimTokenDigestText,
                                Just "500",
                                Just "account_01",
                                Just "ca139e12-69b8-4daa-8eb7-3dfc80b265a4",
                                Just "verification-resend-delivered",
                                Just "1",
                                Nothing,
                                Nothing,
                                Nothing,
                                Nothing,
                                Nothing
                              ]
                            )
                          ]

  it "keeps unavailable, capacity, lost-claim, and corrupt results distinct" $ do
    let unavailableStore = buildRuntimePostgresVerificationResendAuditStoreWithRunner (\_ _ _ -> pure (Left "database unavailable")) databaseConfig
        capacityStore = buildRuntimePostgresVerificationResendAuditStoreWithRunner (\_ _ _ -> pure (Left "account audit partition capacity is exhausted")) databaseConfig
        lostClaimStore = buildRuntimePostgresVerificationResendAuditStoreWithRunner (\_ _ _ -> pure (Right [["lost", ""]])) databaseConfig
        corruptStore = buildRuntimePostgresVerificationResendAuditStoreWithRunner (\_ _ _ -> pure (Right [["settled", "another-account"]])) databaseConfig
        settle store = completeVerificationResendWithAudit store sampleClaim (unixTimeNanoseconds 500) sampleActivity
    settle unavailableStore >>= expectUnavailable
    settle capacityStore >>= expectCapacityExceeded
    settle lostClaimStore >>= expectLostClaim
    settle corruptStore >>= expectCorruptData

  it "uses the runtime role to commit the resend promotion, delivery record, and audit event through the controlled operation" $ do
    ensureDefaultPostgresAvailable
    runPostgresMigrationsForRuntime defaultMigrationPostgresConfig defaultRealPostgresConfig `shouldReturnEqual` Right ()
    removedPriorActivity <- runRuntimeParameterizedRowsQuery defaultMigrationPostgresConfig "DELETE FROM account_audit.activity WHERE account_id = $1 AND request_id = $2 AND event_code = 'verification-resend-delivered' RETURNING activity_id::TEXT;" ["account_01", "ca139e12-69b8-4daa-8eb7-3dfc80b265a4"]
    removedPriorActivity `shouldSatisfy` isRight
    deletedAccounts <- runRuntimeParameterizedRowsQuery defaultMigrationPostgresConfig "DELETE FROM web_api.accounts WHERE account_id = $1 RETURNING account_id;" ["account_01"]
    deletedAccounts `shouldSatisfy` isRight
    runRuntimeParameterizedRowsQuery defaultMigrationPostgresConfig "INSERT INTO web_api.accounts (account_id, email_normalized, password_hash, created_at_nanoseconds) VALUES ($1, $2, 'test-hash', 1) RETURNING account_id;" ["account_01", "verification-resend-audit@example.test"]
      `shouldReturnEqual` Right [["account_01"]]
    runRuntimeParameterizedRowsQuery defaultMigrationPostgresConfig "INSERT INTO web_api.verification_resend_claims (account_id, token_digest, email_normalized, expires_at_nanoseconds, claimed_at_nanoseconds) VALUES ($1, $2, $3, 700, 1) RETURNING account_id;" ["account_01", sampleClaimTokenDigestText, "verification-resend-audit@example.test"]
      `shouldReturnEqual` Right [["account_01"]]
    pool <- newPostgresPool (databasePoolCapacity defaultRealPostgresConfig) defaultRealPostgresConfig
    completeVerificationResendWithAudit (buildRuntimePostgresVerificationResendAuditStore pool) sampleClaim (unixTimeNanoseconds 500) sampleActivity >>= expectSettled
    persistedRows <-
      runRuntimeParameterizedRowsQuery defaultMigrationPostgresConfig "SELECT (SELECT token_digest FROM web_api.email_verifications WHERE account_id = $1) || '|' || (SELECT count(*)::TEXT FROM web_api.verification_resend_claims WHERE account_id = $1) || '|' || (SELECT count(*)::TEXT FROM web_api.verification_resend_deliveries WHERE account_id = $1) || '|' || (SELECT count(*)::TEXT FROM account_audit.activity WHERE account_id = $1 AND event_code = 'verification-resend-delivered');" ["account_01"]
    persistedRows `shouldBe` Right [[sampleClaimTokenDigestText <> "|0|1|1"]]

sampleClaim :: VerificationResendClaim
sampleClaim =
  VerificationResendClaim
    { verificationResendClaimAccountId = requiredMaybe "account id" (mkAccountId "account_01"),
      verificationResendClaimTokenDigest = sampleClaimTokenDigest
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
      activityRequestId = requiredMaybe "request id" (mkRequestId "ca139e12-69b8-4daa-8eb7-3dfc80b265a4"),
      activityEvent = VerificationResendDelivered,
      activityRoute = Nothing
    }

requiredMaybe :: String -> Maybe value -> value
requiredMaybe label = fromMaybe (error ("expected valid " <> label))

expectUnavailable :: Either VerificationResendAuditStoreError VerificationResendClaimSettlement -> Expectation
expectUnavailable = \case
  Left VerificationResendAuditStoreUnavailable -> pure ()
  _ -> expectationFailure "expected unavailable verification-resend audit storage"

expectCapacityExceeded :: Either VerificationResendAuditStoreError VerificationResendClaimSettlement -> Expectation
expectCapacityExceeded = \case
  Left VerificationResendAuditCapacityExceeded -> pure ()
  _ -> expectationFailure "expected exhausted account-audit partition capacity"

expectCorruptData :: Either VerificationResendAuditStoreError VerificationResendClaimSettlement -> Expectation
expectCorruptData = \case
  Left VerificationResendAuditStoreCorruptData -> pure ()
  _ -> expectationFailure "expected corrupt verification-resend audit storage data"

expectSettled :: Either VerificationResendAuditStoreError VerificationResendClaimSettlement -> Expectation
expectSettled = \case
  Right VerificationResendClaimSettled -> pure ()
  Right VerificationResendClaimLost -> expectationFailure "expected verification resend and activity to be saved, but the claim was lost"
  Left VerificationResendAuditStoreUnavailable -> expectationFailure "expected verification resend and activity to be saved, but storage was unavailable"
  Left VerificationResendAuditCapacityExceeded -> expectationFailure "expected verification resend and activity to be saved, but audit capacity was exhausted"
  Left VerificationResendAuditStoreCorruptData -> expectationFailure "expected verification resend and activity to be saved, but PostgreSQL returned corrupt data"

expectLostClaim :: Either VerificationResendAuditStoreError VerificationResendClaimSettlement -> Expectation
expectLostClaim = \case
  Right VerificationResendClaimLost -> pure ()
  _ -> expectationFailure "expected a verification-resend claim collision"
