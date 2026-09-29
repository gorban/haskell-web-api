{-# LANGUAGE ImportQualifiedPost #-}

{-# SPEC #-}

import Data.Either (isLeft)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import HarchWeb.Account (accountIdText, generateAccountId)
import TestSupport.RealPostgres (defaultMigrationPostgresConfig, defaultRealPostgresConfig, ensureDefaultPostgresAvailable)
import Unit.WebApi.TestSupport (accountId, databaseConfig, shouldReturnEqual, testRequestId)
import WebApi.ActivityAudit (ActivityAuditStoreError (..))
import WebApi.Config (DatabaseConfig (..))
import WebApi.Mfa (MfaConfirmationAuditContext (..), MfaStore (..), MfaStoreError (..), StoredTotpEnrollment (..))
import WebApi.Postgres.Testing (buildRuntimePostgresMfaStore, buildRuntimePostgresMfaStoreWithRunner, newPostgresPool, runPostgresMigrationsForRuntime, runRuntimeNullableParameterizedRowsQuery, runRuntimeParameterizedRowsQuery)

spec =
  describe "runtime PostgreSQL MFA persistence" $ do
    it "uses bound parameters to enroll, load, confirm, and consume recovery-code hashes" $ do
      queriesReference <- newIORef []
      let runner _databaseConfig query parameters = do
            modifyIORef' queriesReference ((query, parameters) :)
            pure $
              if "INSERT INTO web_api.account_totp" `Text.isInfixOf` query
                then Right [["account_01"]]
                else
                  if "SELECT convert_from(encrypted_secret" `Text.isInfixOf` query
                    then Right [["encrypted-envelope", "500", ""]]
                    else
                      if "confirm_mfa_enrollment_with_activity" `Text.isInfixOf` query
                        then Right [["account_01"]]
                        else
                          if "SELECT code_hash FROM web_api.account_recovery_codes" `Text.isInfixOf` query
                            then Right [["hash-one"], ["hash-two"]]
                            else
                              if "UPDATE web_api.account_recovery_codes SET used_at_nanoseconds" `Text.isInfixOf` query
                                || "UPDATE web_api.account_totp SET last_used_totp_counter" `Text.isInfixOf` query
                                then Right [["account_01"]]
                                else Left "unexpected query"
          store = buildRuntimePostgresMfaStoreWithRunner runner databaseConfig
          recoveryHashes = "hash-one" :| ["hash-two", "hash-three", "hash-four", "hash-five", "hash-six", "hash-seven", "hash-eight"]
      saveUnconfirmedTotpEnrollment store accountId "encrypted-envelope" 100 `shouldReturnEqual` Right True
      loadTotpEnrollment store accountId
        `shouldReturnEqual` Right (Just (StoredTotpEnrollment "encrypted-envelope" (Just 500) Nothing))
      confirmTotpEnrollment store accountId recoveryHashes 500 testMfaAuditContext `shouldReturnEqual` Right True
      loadUnusedRecoveryCodeHashes store accountId `shouldReturnEqual` Right ["hash-one", "hash-two"]
      consumeRecoveryCodeHash store accountId "hash-one" 600 `shouldReturnEqual` Right True
      markTotpCodeUsed store accountId 700 `shouldReturnEqual` Right True
      recordedQueries <- reverse <$> readIORef queriesReference
      recordedQueries
        `shouldBe` [ ( "INSERT INTO web_api.account_totp (account_id, encrypted_secret, created_at_nanoseconds) SELECT $1, convert_to($2, 'UTF8'), $3 WHERE EXISTS (SELECT 1 FROM web_api.accounts WHERE account_id = $1 AND email_verified_at_nanoseconds IS NOT NULL) AND NOT EXISTS (SELECT 1 FROM web_api.account_totp WHERE account_id = $1 AND confirmed_at_nanoseconds IS NOT NULL) ON CONFLICT (account_id) DO UPDATE SET encrypted_secret = EXCLUDED.encrypted_secret, confirmed_at_nanoseconds = NULL, created_at_nanoseconds = EXCLUDED.created_at_nanoseconds, last_used_totp_counter = NULL RETURNING account_id;",
                       [Just "account_01", Just "encrypted-envelope", Just "100"]
                     ),
                     ( "SELECT convert_from(encrypted_secret, 'UTF8'), COALESCE(confirmed_at_nanoseconds::TEXT, ''), COALESCE(last_used_totp_counter::TEXT, '') FROM web_api.account_totp WHERE account_id = $1;",
                       [Just "account_01"]
                     ),
                     ( "SELECT account_id FROM account_audit.confirm_mfa_enrollment_with_activity($1, $2::BIGINT, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15);",
                       [ Just "account_01",
                         Just "500",
                         Just "550e8400-e29b-41d4-a716-446655440000",
                         Nothing,
                         Nothing,
                         Nothing,
                         Nothing,
                         Just "hash-one",
                         Just "hash-two",
                         Just "hash-three",
                         Just "hash-four",
                         Just "hash-five",
                         Just "hash-six",
                         Just "hash-seven",
                         Just "hash-eight"
                       ]
                     ),
                     ( "SELECT code_hash FROM web_api.account_recovery_codes WHERE account_id = $1 AND used_at_nanoseconds IS NULL ORDER BY code_hash ASC;",
                       [Just "account_01"]
                     ),
                     ( "UPDATE web_api.account_recovery_codes SET used_at_nanoseconds = $3 WHERE account_id = $1 AND code_hash = $2 AND used_at_nanoseconds IS NULL RETURNING account_id;",
                       [Just "account_01", Just "hash-one", Just "600"]
                     ),
                     ( "UPDATE web_api.account_totp SET last_used_totp_counter = $2 WHERE account_id = $1 AND (last_used_totp_counter IS NULL OR last_used_totp_counter < $2) RETURNING account_id;",
                       [Just "account_01", Just "700"]
                     )
                   ]

    it "preserves unavailable, declined, and corrupt database outcomes" $ do
      let unavailableStore = buildRuntimePostgresMfaStoreWithRunner (\_ _ _ -> pure (Left "database unavailable")) databaseConfig
          declinedStore = buildRuntimePostgresMfaStoreWithRunner (\_ _ _ -> pure (Right [])) databaseConfig
          malformedStore = buildRuntimePostgresMfaStoreWithRunner (\_ _ _ -> pure (Right [["account_01", "not-a-timestamp", ""]])) databaseConfig
          wrongAccountStore = buildRuntimePostgresMfaStoreWithRunner (\_ _ _ -> pure (Right [["other-account"]])) databaseConfig
          capacityStore = buildRuntimePostgresMfaStoreWithRunner (\_ _ _ -> pure (Left "ERROR: account audit partition capacity is exhausted")) databaseConfig
      saveUnconfirmedTotpEnrollment unavailableStore accountId "encrypted-envelope" 100 `shouldReturnEqual` Left (MfaStoreUnavailable "database unavailable")
      saveUnconfirmedTotpEnrollment declinedStore accountId "encrypted-envelope" 100 `shouldReturnEqual` Right False
      saveUnconfirmedTotpEnrollment malformedStore accountId "encrypted-envelope" 100 `shouldReturnEqual` Left (MfaStoreCorruptData "unexpected TOTP enrollment result: row-count=1, column-counts=[3]")
      saveUnconfirmedTotpEnrollment wrongAccountStore accountId "encrypted-envelope" 100 `shouldReturnEqual` Left (MfaStoreCorruptData "unexpected TOTP enrollment result: row-count=1, column-counts=[1]")
      loadTotpEnrollment unavailableStore accountId `shouldReturnEqual` Left (MfaStoreUnavailable "database unavailable")
      loadTotpEnrollment declinedStore accountId `shouldReturnEqual` Right Nothing
      loadTotpEnrollment (buildRuntimePostgresMfaStoreWithRunner (\_ _ _ -> pure (Right [["encrypted-envelope", "", ""]])) databaseConfig) accountId
        `shouldReturnEqual` Right (Just (StoredTotpEnrollment "encrypted-envelope" Nothing Nothing))
      loadTotpEnrollment (buildRuntimePostgresMfaStoreWithRunner (\_ _ _ -> pure (Right [["encrypted-envelope", "500", "42"]])) databaseConfig) accountId
        `shouldReturnEqual` Right (Just (StoredTotpEnrollment "encrypted-envelope" (Just 500) (Just 42)))
      loadTotpEnrollment (buildRuntimePostgresMfaStoreWithRunner (\_ _ _ -> pure (Right [["encrypted-envelope", "500", "not-a-counter"]])) databaseConfig) accountId
        `shouldReturnEqual` Left (MfaStoreCorruptData "TOTP enrollment has an invalid last-used counter")
      loadTotpEnrollment (buildRuntimePostgresMfaStoreWithRunner (\_ _ _ -> pure (Right [["encrypted-envelope"]])) databaseConfig) accountId
        `shouldReturnEqual` Left (MfaStoreCorruptData "unexpected TOTP enrollment lookup result: row-count=1, column-counts=[1]")
      confirmTotpEnrollment declinedStore accountId ("hash" :| []) 500 testMfaAuditContext `shouldReturnEqual` Right False
      confirmTotpEnrollment unavailableStore accountId ("hash" :| []) 500 testMfaAuditContext `shouldReturnEqual` Left (MfaStoreAuditAppendFailed ActivityAuditUnavailable)
      confirmTotpEnrollment capacityStore accountId ("hash" :| []) 500 testMfaAuditContext `shouldReturnEqual` Left (MfaStoreAuditAppendFailed ActivityAuditCapacityExceeded)
      confirmTotpEnrollment malformedStore accountId ("hash" :| []) 500 testMfaAuditContext `shouldReturnEqual` Left (MfaStoreCorruptData "unexpected TOTP confirmation result: row-count=1, column-counts=[3]")
      confirmTotpEnrollment wrongAccountStore accountId ("hash" :| []) 500 testMfaAuditContext `shouldReturnEqual` Left (MfaStoreCorruptData "unexpected TOTP confirmation result: row-count=1, column-counts=[1]")
      loadTotpEnrollment malformedStore accountId `shouldReturnEqual` Left (MfaStoreCorruptData "TOTP enrollment has an invalid confirmation timestamp")
      loadUnusedRecoveryCodeHashes unavailableStore accountId `shouldReturnEqual` Left (MfaStoreUnavailable "database unavailable")
      loadUnusedRecoveryCodeHashes (buildRuntimePostgresMfaStoreWithRunner (\_ _ _ -> pure (Right [["hash"], ["wrong", "row"]])) databaseConfig) accountId
        `shouldReturnEqual` Left (MfaStoreCorruptData "unexpected recovery-code lookup result: row-count=2, column-counts=[1,2]")
      consumeRecoveryCodeHash declinedStore accountId "hash" 500 `shouldReturnEqual` Right False
      consumeRecoveryCodeHash unavailableStore accountId "hash" 500 `shouldReturnEqual` Left (MfaStoreUnavailable "database unavailable")
      consumeRecoveryCodeHash malformedStore accountId "hash" 500 `shouldReturnEqual` Left (MfaStoreCorruptData "unexpected recovery-code consumption result: row-count=1, column-counts=[3]")
      consumeRecoveryCodeHash wrongAccountStore accountId "hash" 500 `shouldReturnEqual` Left (MfaStoreCorruptData "unexpected recovery-code consumption result: row-count=1, column-counts=[1]")
      markTotpCodeUsed declinedStore accountId 700 `shouldReturnEqual` Right False
      markTotpCodeUsed unavailableStore accountId 700 `shouldReturnEqual` Left (MfaStoreUnavailable "database unavailable")
      markTotpCodeUsed malformedStore accountId 700 `shouldReturnEqual` Left (MfaStoreCorruptData "unexpected TOTP counter update result: row-count=1, column-counts=[3]")
      markTotpCodeUsed wrongAccountStore accountId 700 `shouldReturnEqual` Left (MfaStoreCorruptData "unexpected TOTP counter update result: row-count=1, column-counts=[1]")

    it "keeps secret-bearing values non-renderable while exposing stable equality and errors" $ do
      let pendingEnrollment = StoredTotpEnrollment "encrypted-envelope" Nothing Nothing
          confirmedEnrollment = StoredTotpEnrollment "other-envelope" (Just 500) Nothing
          unavailableError = MfaStoreUnavailable "database unavailable"
      expectAll
        ( (pendingEnrollment /= confirmedEnrollment `shouldBe` True)
            :| [ unavailableError /= MfaStoreCorruptData "database unavailable" `shouldBe` True,
                 ActivityAuditCapacityExceeded /= ActivityAuditUnavailable `shouldBe` True
               ]
        )

    it "executes the native libpq MFA adapter against a migrated PostgreSQL database" $ do
      ensureDefaultPostgresAvailable
      runPostgresMigrationsForRuntime defaultMigrationPostgresConfig defaultRealPostgresConfig
        `shouldReturn` Right ()
      unknownAccountId <- generateAccountId
      pool <- newPostgresPool (databasePoolCapacity defaultRealPostgresConfig) defaultRealPostgresConfig
      let store = buildRuntimePostgresMfaStore pool
      saveUnconfirmedTotpEnrollment store unknownAccountId "encrypted-envelope" 100 `shouldReturnEqual` Right False
      loadTotpEnrollment store unknownAccountId `shouldReturnEqual` Right Nothing
      confirmTotpEnrollment store unknownAccountId ("hash" :| replicate 7 "hash") 500 testMfaAuditContext `shouldReturnEqual` Right False
      markTotpCodeUsed store unknownAccountId 700 `shouldReturnEqual` Right False
      loadUnusedRecoveryCodeHashes store unknownAccountId `shouldReturnEqual` Right []
      consumeRecoveryCodeHash store unknownAccountId "hash" 500 `shouldReturnEqual` Right False

    it "commits MFA confirmation and its audit event together" $ do
      ensureDefaultPostgresAvailable
      migrationResult <- runPostgresMigrationsForRuntime defaultMigrationPostgresConfig defaultRealPostgresConfig
      migrationResult `shouldBe` Right ()
      account <- generateAccountId
      let accountText = accountIdText account
          hashes = fmap (\index -> accountText <> "-recovery-" <> Text.pack (show index)) ((1 :: Int) :| [2 .. 8])
          confirmationQuery = "SELECT account_id FROM account_audit.confirm_mfa_enrollment_with_activity($1, $2::BIGINT, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15);"
          invalidRequestParameters =
            [Just accountText, Just "700", Just "malformed-request-id", Nothing, Nothing, Nothing, Nothing]
              <> fmap Just (NonEmpty.toList hashes)
      _ <-
        runRuntimeParameterizedRowsQuery
          defaultMigrationPostgresConfig
          "INSERT INTO web_api.accounts (account_id, email_normalized, password_hash, email_verified_at_nanoseconds, created_at_nanoseconds) VALUES ($1, $2, 'test-hash', 1, 1);"
          [accountText, accountText <> "@example.test"]
      pool <- newPostgresPool (databasePoolCapacity defaultRealPostgresConfig) defaultRealPostgresConfig
      let store = buildRuntimePostgresMfaStore pool
      savedEnrollment <- saveUnconfirmedTotpEnrollment store account "encrypted-envelope" 600
      expectMfaStoreSuccess "expected an unconfirmed authenticator to be stored" savedEnrollment
      invalidAuditAppend <- runRuntimeNullableParameterizedRowsQuery defaultRealPostgresConfig confirmationQuery invalidRequestParameters
      invalidAuditAppend `shouldSatisfy` isLeft
      afterRejectedAppend <-
        runRuntimeParameterizedRowsQuery
          defaultMigrationPostgresConfig
          "SELECT COALESCE(totp.confirmed_at_nanoseconds::TEXT, ''), (SELECT count(*)::TEXT FROM web_api.account_recovery_codes codes WHERE codes.account_id = totp.account_id), (SELECT count(*)::TEXT FROM account_audit.activity activity WHERE activity.account_id = totp.account_id AND activity.event_code = 'mfa-enrolled') FROM web_api.account_totp totp WHERE totp.account_id = $1;"
          [accountText]
      afterRejectedAppend `shouldBe` Right [["", "0", "0"]]
      confirmationResult <- confirmTotpEnrollment store account hashes 700 testMfaAuditContext
      expectMfaStoreSuccess "expected TOTP, recovery codes, and audit row to commit" confirmationResult
      committedState <-
        runRuntimeParameterizedRowsQuery
          defaultMigrationPostgresConfig
          "SELECT COALESCE(totp.confirmed_at_nanoseconds::TEXT, ''), (SELECT count(*)::TEXT FROM web_api.account_recovery_codes codes WHERE codes.account_id = totp.account_id), (SELECT count(*)::TEXT FROM account_audit.activity activity WHERE activity.account_id = totp.account_id AND activity.request_id = $2 AND activity.event_code = 'mfa-enrolled') FROM web_api.account_totp totp WHERE totp.account_id = $1;"
          [accountText, "550e8400-e29b-41d4-a716-446655440000"]
      committedState `shouldBe` Right [["700", "8", "1"]]

testMfaAuditContext :: MfaConfirmationAuditContext
testMfaAuditContext = MfaConfirmationAuditContext testRequestId Nothing

expectMfaStoreSuccess :: String -> Either MfaStoreError Bool -> Expectation
expectMfaStoreSuccess label storeResult =
  case storeResult of
    Right True -> pure ()
    Right False -> expectationFailure (label <> ": operation declined")
    Left (MfaStoreUnavailable _) -> expectationFailure (label <> ": persistence unavailable")
    Left (MfaStoreCorruptData _) -> expectationFailure (label <> ": persistence returned corrupt data")
    Left (MfaStoreAuditAppendFailed ActivityAuditUnavailable) -> expectationFailure (label <> ": audit append unavailable")
    Left (MfaStoreAuditAppendFailed ActivityAuditCapacityExceeded) -> expectationFailure (label <> ": audit capacity exhausted")
    Left (MfaStoreAuditAppendFailed ActivityAuditCorruptResult) -> expectationFailure (label <> ": audit append result was corrupt")
