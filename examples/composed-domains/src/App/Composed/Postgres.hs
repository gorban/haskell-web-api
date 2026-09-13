{-# LANGUAGE OverloadedStrings #-}

-- | Immutable PostgreSQL changes owned by the composed admission example.
--
-- The schema is intentionally separate from @web_api@: admission credentials,
-- opaque admission sessions, and synchronizer records must not be usable as
-- account/MFA/session state.  The shared runner supplies transaction, digest,
-- and ledger guarantees; this module owns only this application's schema.
module App.Composed.Postgres
  ( ComposedDatabaseConnectionString (..),
    composedDatabaseChanges,
    runComposedDatabaseChanges,
    runComposedDatabaseChangesWithExecutor,
  )
where

import Data.ByteString (ByteString)
import Data.List.NonEmpty qualified as NonEmpty
import Postgres.DatabaseChange
  ( DatabaseChange (..),
    DatabaseChangeConnectionString (..),
    DatabaseChangeError,
    DatabaseChangeExecutor,
    DatabaseChangeId (..),
    DatabaseChangeLedger (..),
    runDatabaseChanges,
    runDatabaseChangesWithExecutor,
  )

newtype ComposedDatabaseConnectionString = ComposedDatabaseConnectionString ByteString

-- | Apply the composed schema and its own digest ledger.  Provisioning and
-- runtime grants are deployment-owned follow-up statements, not mutable
-- application migration state.
runComposedDatabaseChanges :: ComposedDatabaseConnectionString -> IO (Either DatabaseChangeError ())
runComposedDatabaseChanges (ComposedDatabaseConnectionString connectionString) =
  runDatabaseChanges
    (DatabaseChangeConnectionString connectionString)
    composedDatabaseLedger
    composedDatabaseChanges
    []

runComposedDatabaseChangesWithExecutor :: DatabaseChangeExecutor -> IO (Either DatabaseChangeError ())
runComposedDatabaseChangesWithExecutor executor =
  runDatabaseChangesWithExecutor executor composedDatabaseLedger composedDatabaseChanges []

composedDatabaseLedger :: DatabaseChangeLedger
composedDatabaseLedger =
  DatabaseChangeLedger
    { databaseChangeLedgerSchema = "composed",
      databaseChangeLedgerTable = "database_changes",
      databaseChangeLegacyTable = Nothing,
      databaseChangeLedgerLockId = 817230044
    }

composedDatabaseChanges :: [DatabaseChange]
composedDatabaseChanges =
  [ DatabaseChange
      { databaseChangeId = DatabaseChangeId "admission-and-synchronizer-v1",
        databaseChangeStatements =
          NonEmpty.fromList
            [ "CREATE SCHEMA IF NOT EXISTS composed;",
              "CREATE TABLE IF NOT EXISTS composed.admission_credentials (admission_principal_id TEXT PRIMARY KEY, admission_login_name TEXT NOT NULL UNIQUE, encrypted_totp_secret TEXT NOT NULL, last_used_totp_counter BIGINT);",
              "CREATE TABLE IF NOT EXISTS composed.admission_sessions (session_id TEXT PRIMARY KEY, admission_principal_id TEXT NOT NULL REFERENCES composed.admission_credentials (admission_principal_id) ON DELETE RESTRICT, issued_at_nanoseconds BIGINT NOT NULL, expires_at_nanoseconds BIGINT NOT NULL, invalidated_at_nanoseconds BIGINT);",
              "CREATE INDEX IF NOT EXISTS admission_sessions_active_expiry_idx ON composed.admission_sessions (expires_at_nanoseconds) WHERE invalidated_at_nanoseconds IS NULL;",
              "CREATE TABLE IF NOT EXISTS composed.csrf_synchronizer_tokens (token_digest TEXT PRIMARY KEY, binding_digest TEXT NOT NULL, issued_at_nanoseconds BIGINT NOT NULL, expires_at_nanoseconds BIGINT NOT NULL, revoked_at_nanoseconds BIGINT);",
              "CREATE INDEX IF NOT EXISTS csrf_synchronizer_binding_expiry_idx ON composed.csrf_synchronizer_tokens (binding_digest, expires_at_nanoseconds) WHERE revoked_at_nanoseconds IS NULL;"
            ]
      },
    DatabaseChange
      { databaseChangeId = DatabaseChangeId "admission-attempt-groups-v2",
        databaseChangeStatements =
          NonEmpty.fromList
            [ "CREATE TABLE IF NOT EXISTS composed.admission_attempt_groups (attempt_group_id BIGSERIAL PRIMARY KEY, attempted_at_nanoseconds BIGINT NOT NULL, succeeded BOOLEAN NOT NULL DEFAULT false, settled BOOLEAN NOT NULL DEFAULT false);",
              "CREATE TABLE IF NOT EXISTS composed.admission_attempts (attempt_group_id BIGINT NOT NULL REFERENCES composed.admission_attempt_groups (attempt_group_id) ON DELETE CASCADE, attempt_key TEXT NOT NULL, PRIMARY KEY (attempt_group_id, attempt_key));",
              "CREATE INDEX IF NOT EXISTS admission_attempt_groups_time_idx ON composed.admission_attempt_groups (attempted_at_nanoseconds);",
              "CREATE INDEX IF NOT EXISTS admission_attempts_key_idx ON composed.admission_attempts (attempt_key);",
              "CREATE OR REPLACE FUNCTION composed.reserve_admission_attempt_group(p_budgets JSONB, p_retention_since BIGINT, p_now BIGINT, p_storage_max BIGINT) RETURNS TABLE(outcome TEXT, value TEXT) LANGUAGE plpgsql VOLATILE AS $$ DECLARE budget JSONB; budget_key TEXT; budget_max BIGINT; budget_window BIGINT; budget_lockout BIGINT; latest_failure BIGINT; failure_count BIGINT; lockout_ends_at BIGINT := 0; stored_count BIGINT; reserved_group_id BIGINT; budget_count BIGINT; BEGIN IF jsonb_typeof(p_budgets) <> 'array' OR p_retention_since < 0 OR p_now < 0 OR p_storage_max < 1 THEN RETURN QUERY SELECT 'invalid-budget'::TEXT, ''::TEXT; RETURN; END IF; SELECT jsonb_array_length(p_budgets) INTO budget_count; IF budget_count < 1 OR budget_count > 4 OR (SELECT count(DISTINCT entry->>'key') FROM jsonb_array_elements(p_budgets) entry) <> budget_count THEN RETURN QUERY SELECT 'invalid-budget'::TEXT, ''::TEXT; RETURN; END IF; PERFORM pg_advisory_xact_lock(hashtextextended('composed.admission_attempt_groups.capacity', 0)); DELETE FROM composed.admission_attempt_groups WHERE attempted_at_nanoseconds < p_retention_since; FOR budget IN SELECT entry FROM jsonb_array_elements(p_budgets) entry ORDER BY entry->>'key' LOOP budget_key := budget->>'key'; IF budget_key IS NULL OR char_length(budget_key) < 1 OR char_length(budget_key) > 260 OR (budget->>'maximum') !~ '^[1-9][0-9]{0,17}$' OR (budget->>'window') !~ '^[1-9][0-9]{0,17}$' OR (budget->>'lockout') !~ '^[1-9][0-9]{0,17}$' THEN RETURN QUERY SELECT 'invalid-budget'::TEXT, ''::TEXT; RETURN; END IF; budget_max := (budget->>'maximum')::BIGINT; budget_window := (budget->>'window')::BIGINT; budget_lockout := (budget->>'lockout')::BIGINT; PERFORM pg_advisory_xact_lock(hashtextextended('composed.admission_attempt.scope.' || budget_key, 0)); SELECT max(group_row.attempted_at_nanoseconds), count(*) INTO latest_failure, failure_count FROM composed.admission_attempts attempt JOIN composed.admission_attempt_groups group_row ON group_row.attempt_group_id = attempt.attempt_group_id WHERE attempt.attempt_key = budget_key AND group_row.succeeded = false AND group_row.attempted_at_nanoseconds >= GREATEST(0, p_now - budget_window) AND group_row.attempted_at_nanoseconds <= p_now; IF failure_count >= budget_max AND latest_failure + budget_lockout > p_now THEN lockout_ends_at := GREATEST(lockout_ends_at, latest_failure + budget_lockout); END IF; END LOOP; IF lockout_ends_at > 0 THEN RETURN QUERY SELECT 'throttled'::TEXT, lockout_ends_at::TEXT; RETURN; END IF; SELECT count(*) INTO stored_count FROM composed.admission_attempt_groups; IF stored_count >= p_storage_max THEN RETURN QUERY SELECT 'storage-exhausted'::TEXT, ''::TEXT; RETURN; END IF; INSERT INTO composed.admission_attempt_groups (attempted_at_nanoseconds, succeeded, settled) VALUES (p_now, false, false) RETURNING attempt_group_id INTO reserved_group_id; FOR budget IN SELECT entry FROM jsonb_array_elements(p_budgets) entry ORDER BY entry->>'key' LOOP INSERT INTO composed.admission_attempts (attempt_group_id, attempt_key) VALUES (reserved_group_id, budget->>'key'); END LOOP; RETURN QUERY SELECT 'reserved'::TEXT, reserved_group_id::TEXT; END $$;"
            ]
      }
  ]
