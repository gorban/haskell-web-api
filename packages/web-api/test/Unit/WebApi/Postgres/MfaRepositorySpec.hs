{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as Text
import HarchWeb.Account (generateAccountId)
import TestSupport.RealPostgres (defaultMigrationPostgresConfig, defaultRealPostgresConfig, ensureDefaultPostgresAvailable)
import Unit.WebApi.TestSupport (accountId, databaseConfig, shouldReturnEqual)
import WebApi.Config (DatabaseConfig (..))
import WebApi.Mfa (MfaStore (..), MfaStoreError (..), StoredTotpEnrollment (..))
import WebApi.Postgres.Testing (buildRuntimePostgresMfaStore, buildRuntimePostgresMfaStoreWithRunner, newPostgresPool, runPostgresMigrationsForRuntime)

spec = do
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
                      if "WITH confirmed AS" `Text.isInfixOf` query
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
          recoveryHashes = "hash-one" :| ["hash-two"]
      saveUnconfirmedTotpEnrollment store accountId "encrypted-envelope" 100 `shouldReturnEqual` Right True
      loadTotpEnrollment store accountId
        `shouldReturnEqual` Right (Just (StoredTotpEnrollment "encrypted-envelope" (Just 500) Nothing))
      confirmTotpEnrollment store accountId recoveryHashes 500 `shouldReturnEqual` Right True
      loadUnusedRecoveryCodeHashes store accountId `shouldReturnEqual` Right ["hash-one", "hash-two"]
      consumeRecoveryCodeHash store accountId "hash-one" 600 `shouldReturnEqual` Right True
      markTotpCodeUsed store accountId 700 `shouldReturnEqual` Right True
      recordedQueries <- reverse <$> readIORef queriesReference
      recordedQueries
        `shouldBe` [ ( "INSERT INTO web_api.account_totp (account_id, encrypted_secret, created_at_nanoseconds) SELECT $1, convert_to($2, 'UTF8'), $3 WHERE EXISTS (SELECT 1 FROM web_api.accounts WHERE account_id = $1 AND email_verified_at_nanoseconds IS NOT NULL) AND NOT EXISTS (SELECT 1 FROM web_api.account_totp WHERE account_id = $1 AND confirmed_at_nanoseconds IS NOT NULL) ON CONFLICT (account_id) DO UPDATE SET encrypted_secret = EXCLUDED.encrypted_secret, confirmed_at_nanoseconds = NULL, created_at_nanoseconds = EXCLUDED.created_at_nanoseconds, last_used_totp_counter = NULL RETURNING account_id;",
                       ["account_01", "encrypted-envelope", "100"]
                     ),
                     ( "SELECT convert_from(encrypted_secret, 'UTF8'), COALESCE(confirmed_at_nanoseconds::TEXT, ''), COALESCE(last_used_totp_counter::TEXT, '') FROM web_api.account_totp WHERE account_id = $1;",
                       ["account_01"]
                     ),
                     ( "WITH confirmed AS (UPDATE web_api.account_totp SET confirmed_at_nanoseconds = $2 WHERE account_id = $1 AND confirmed_at_nanoseconds IS NULL RETURNING account_id), removed_codes AS (DELETE FROM web_api.account_recovery_codes WHERE account_id IN (SELECT account_id FROM confirmed)), issued_codes AS (INSERT INTO web_api.account_recovery_codes (account_id, code_hash, created_at_nanoseconds) SELECT confirmed.account_id, recovery_codes.code_hash, $2 FROM confirmed CROSS JOIN (VALUES ($3), ($4)) AS recovery_codes(code_hash)) SELECT account_id FROM confirmed;",
                       ["account_01", "500", "hash-one", "hash-two"]
                     ),
                     ( "SELECT code_hash FROM web_api.account_recovery_codes WHERE account_id = $1 AND used_at_nanoseconds IS NULL ORDER BY code_hash ASC;",
                       ["account_01"]
                     ),
                     ( "UPDATE web_api.account_recovery_codes SET used_at_nanoseconds = $3 WHERE account_id = $1 AND code_hash = $2 AND used_at_nanoseconds IS NULL RETURNING account_id;",
                       ["account_01", "hash-one", "600"]
                     ),
                     ( "UPDATE web_api.account_totp SET last_used_totp_counter = $2 WHERE account_id = $1 AND (last_used_totp_counter IS NULL OR last_used_totp_counter < $2) RETURNING account_id;",
                       ["account_01", "700"]
                     )
                   ]

    it "preserves unavailable, declined, and corrupt database outcomes" $ do
      let unavailableStore = buildRuntimePostgresMfaStoreWithRunner (\_ _ _ -> pure (Left "database unavailable")) databaseConfig
          declinedStore = buildRuntimePostgresMfaStoreWithRunner (\_ _ _ -> pure (Right [])) databaseConfig
          malformedStore = buildRuntimePostgresMfaStoreWithRunner (\_ _ _ -> pure (Right [["account_01", "not-a-timestamp", ""]])) databaseConfig
          wrongAccountStore = buildRuntimePostgresMfaStoreWithRunner (\_ _ _ -> pure (Right [["other-account"]])) databaseConfig
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
      loadTotpEnrollment (buildRuntimePostgresMfaStoreWithRunner (\_ _ _ -> pure (Right [["encrypted-envelope"]])) databaseConfig) accountId
        `shouldReturnEqual` Left (MfaStoreCorruptData "unexpected TOTP enrollment lookup result: row-count=1, column-counts=[1]")
      confirmTotpEnrollment declinedStore accountId ("hash" :| []) 500 `shouldReturnEqual` Right False
      confirmTotpEnrollment unavailableStore accountId ("hash" :| []) 500 `shouldReturnEqual` Left (MfaStoreUnavailable "database unavailable")
      confirmTotpEnrollment malformedStore accountId ("hash" :| []) 500 `shouldReturnEqual` Left (MfaStoreCorruptData "unexpected TOTP confirmation result: row-count=1, column-counts=[3]")
      confirmTotpEnrollment wrongAccountStore accountId ("hash" :| []) 500 `shouldReturnEqual` Left (MfaStoreCorruptData "unexpected TOTP confirmation result: row-count=1, column-counts=[1]")
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
            :| [unavailableError /= MfaStoreCorruptData "database unavailable" `shouldBe` True]
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
      confirmTotpEnrollment store unknownAccountId ("hash" :| []) 500 `shouldReturnEqual` Right False
      markTotpCodeUsed store unknownAccountId 700 `shouldReturnEqual` Right False
      loadUnusedRecoveryCodeHashes store unknownAccountId `shouldReturnEqual` Right []
      consumeRecoveryCodeHash store unknownAccountId "hash" 500 `shouldReturnEqual` Right False
