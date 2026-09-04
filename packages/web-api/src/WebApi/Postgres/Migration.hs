{-# LANGUAGE OverloadedStrings #-}

-- | Database-change migration runs on one short-lived, owner-credential
-- libpq connection for the complete transaction.  This deliberately differs
-- from the application's shared runtime pool: a migration is exclusive,
-- privileged setup work and must retain its advisory lock until commit or
-- rollback.  Recorded structural migrations run once; configured ownership
-- and runtime-role privileges reconcile on every run in that same transaction,
-- so a changed runtime identity is not hidden by a schema version.  See the
-- AX decision record in @docs/design-guidance.md@.
module WebApi.Postgres.Migration
  ( migrationStatementsFor,
    runPostgresMigrations,
    runPostgresMigrationsForRuntime,
    runPostgresSeed,
    runPostgresSeedWithRunner,
    seedStatements,
  )
where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import Postgres.DatabaseChange
  ( DatabaseChange (..),
    DatabaseChangeConnectionString (..),
    DatabaseChangeError (..),
    DatabaseChangeId (..),
    DatabaseChangeLedger (..),
    runDatabaseChanges,
  )
import WebApi.Config (DatabaseConfig (..))
import WebApi.Postgres.Pool (runtimeConnectionString)
import WebApi.Postgres.Runtime
  ( PostgresCommand,
    PostgresCommandResult,
    PostgresRunnerError (..),
    runPostgresCommand,
    runStatements,
  )

runPostgresMigrations :: DatabaseConfig -> IO (Either PostgresRunnerError ())
runPostgresMigrations databaseConfig =
  runPostgresMigrationsForRuntime databaseConfig databaseConfig

runPostgresMigrationsForRuntime :: DatabaseConfig -> DatabaseConfig -> IO (Either PostgresRunnerError ())
runPostgresMigrationsForRuntime migrationDatabaseConfig runtimeDatabaseConfig =
  mapDatabaseChangeFailure
    <$> runDatabaseChanges
      (DatabaseChangeConnectionString (runtimeConnectionString migrationDatabaseConfig))
      webApiDatabaseChangeLedger
      webApiDatabaseChanges
      (migrationReconciliationStatementsFor migrationDatabaseConfig runtimeDatabaseConfig)

mapDatabaseChangeFailure :: Either DatabaseChangeError () -> Either PostgresRunnerError ()
mapDatabaseChangeFailure = either (Left . PostgresMigrationFailed . renderDatabaseChangeFailure) Right

renderDatabaseChangeFailure :: DatabaseChangeError -> Text
renderDatabaseChangeFailure failure =
  case failure of
    DatabaseChangeInvalidId -> "PostgreSQL database change has an invalid ID"
    DatabaseChangeInvalidLedger -> "PostgreSQL database change ledger is invalid"
    DatabaseChangeDuplicateId _ -> "PostgreSQL database change plan has a duplicate ID"
    DatabaseChangeExecutionFailed -> "PostgreSQL database change command failed"
    DatabaseChangeCommandReturnedRows -> "PostgreSQL database change command returned rows"
    DatabaseChangeQueryReturnedNoRows -> "PostgreSQL database change query returned no rows"
    DatabaseChangeMalformedLedgerRow -> "PostgreSQL database change ledger row is malformed"
    DatabaseChangeUnknownRecordedId _ -> "Unknown PostgreSQL database change ID"
    DatabaseChangeDigestMismatch _ -> "PostgreSQL database change digest mismatch"
    DatabaseChangeOutOfOrder _ -> "PostgreSQL database changes are out of order"

webApiDatabaseChangeLedger :: DatabaseChangeLedger
webApiDatabaseChangeLedger =
  DatabaseChangeLedger
    { databaseChangeLedgerSchema = "web_api",
      databaseChangeLedgerTable = "database_changes",
      databaseChangeLegacyTable = Just "schema_migrations",
      databaseChangeLedgerLockId = 782476311
    }

webApiDatabaseChanges :: [DatabaseChange]
webApiDatabaseChanges =
  [ change "initial-schema" migrationStatementsFor,
    change "epoch-security-time-v1" epochSecurityTimeMigrationStatements,
    change "login-attempt-reservations-v1" loginAttemptReservationMigrationStatements,
    change "login-attempt-reservation-function-v1" loginAttemptReservationFunctionMigrationStatements,
    change "login-attempt-storage-bound-v1" loginAttemptStorageBoundMigrationStatements,
    change "pending-registration-lifecycle-v1" pendingRegistrationLifecycleMigrationStatements,
    change "verification-resend-lifecycle-v1" verificationResendLifecycleMigrationStatements,
    change "keyed-login-attempt-groups-v1" keyedLoginAttemptGroupMigrationStatements,
    change "remove-session-csrf-v1" removeSessionCsrfMigrationStatements
  ]
  where
    change changeId statements =
      DatabaseChange
        { databaseChangeId = DatabaseChangeId changeId,
          databaseChangeStatements = NonEmpty.fromList statements
        }

runPostgresSeed :: DatabaseConfig -> IO (Either PostgresRunnerError ())
runPostgresSeed = runPostgresSeedWithRunner runPostgresCommand

runPostgresSeedWithRunner :: (PostgresCommand -> IO PostgresCommandResult) -> DatabaseConfig -> IO (Either PostgresRunnerError ())
runPostgresSeedWithRunner runCommand databaseConfig = runStatements runCommand databaseConfig seedStatements

appSchemaName :: Text
appSchemaName = "web_api"

migrationStatementsFor :: [Text]
migrationStatementsFor =
  versionedSchemaStatements
  where
    versionedSchemaStatements =
      [ "CREATE SCHEMA IF NOT EXISTS " <> appSchemaName <> ";",
        "CREATE TABLE IF NOT EXISTS " <> qualifiedTableName "page_content" <> " (route_slug TEXT NOT NULL, locale TEXT NOT NULL, summary TEXT NOT NULL, PRIMARY KEY (route_slug, locale));",
        "CREATE TABLE IF NOT EXISTS " <> qualifiedTableName "page_highlights" <> " (route_slug TEXT NOT NULL, locale TEXT NOT NULL, position INTEGER NOT NULL, highlight TEXT NOT NULL, PRIMARY KEY (route_slug, locale, position));",
        "CREATE TABLE IF NOT EXISTS " <> qualifiedTableName "accounts" <> " (account_id TEXT PRIMARY KEY, email_normalized TEXT NOT NULL UNIQUE, username TEXT, display_name TEXT, password_hash TEXT NOT NULL, email_verified_at_nanoseconds BIGINT, created_at_nanoseconds BIGINT NOT NULL);",
        "ALTER TABLE " <> qualifiedTableName "accounts" <> " ADD COLUMN IF NOT EXISTS username TEXT;",
        "ALTER TABLE " <> qualifiedTableName "accounts" <> " ADD COLUMN IF NOT EXISTS display_name TEXT;",
        "CREATE UNIQUE INDEX IF NOT EXISTS accounts_username_lower_unique ON " <> qualifiedTableName "accounts" <> " (lower(username)) WHERE username IS NOT NULL;",
        "CREATE TABLE IF NOT EXISTS " <> qualifiedTableName "email_verifications" <> " (token_digest TEXT PRIMARY KEY, account_id TEXT NOT NULL REFERENCES " <> qualifiedTableName "accounts" <> " (account_id) ON DELETE CASCADE, email_normalized TEXT NOT NULL, expires_at_nanoseconds BIGINT NOT NULL);",
        "CREATE TABLE IF NOT EXISTS " <> qualifiedTableName "account_totp" <> " (account_id TEXT PRIMARY KEY REFERENCES " <> qualifiedTableName "accounts" <> " (account_id) ON DELETE CASCADE, encrypted_secret BYTEA NOT NULL, confirmed_at_nanoseconds BIGINT, created_at_nanoseconds BIGINT NOT NULL, last_used_totp_counter BIGINT);",
        "ALTER TABLE " <> qualifiedTableName "account_totp" <> " ADD COLUMN IF NOT EXISTS last_used_totp_counter BIGINT;",
        "CREATE TABLE IF NOT EXISTS " <> qualifiedTableName "account_recovery_codes" <> " (account_id TEXT NOT NULL REFERENCES " <> qualifiedTableName "accounts" <> " (account_id) ON DELETE CASCADE, code_hash TEXT NOT NULL UNIQUE, created_at_nanoseconds BIGINT NOT NULL, used_at_nanoseconds BIGINT, PRIMARY KEY (account_id, code_hash));",
        "CREATE TABLE IF NOT EXISTS " <> qualifiedTableName "account_sessions" <> " (session_id TEXT PRIMARY KEY, account_id TEXT NOT NULL REFERENCES " <> qualifiedTableName "accounts" <> " (account_id) ON DELETE CASCADE, csrf_token TEXT NOT NULL, issued_at_nanoseconds BIGINT NOT NULL, expires_at_nanoseconds BIGINT NOT NULL, invalidated_at_nanoseconds BIGINT);",
        "CREATE TABLE IF NOT EXISTS " <> qualifiedTableName "mfa_enrollment_sessions" <> " (session_id TEXT PRIMARY KEY, account_id TEXT NOT NULL REFERENCES " <> qualifiedTableName "accounts" <> " (account_id) ON DELETE CASCADE, csrf_token TEXT NOT NULL, issued_at_nanoseconds BIGINT NOT NULL, expires_at_nanoseconds BIGINT NOT NULL, invalidated_at_nanoseconds BIGINT);",
        -- Not a foreign key to accounts: an attempt key can name an unknown
        -- identifier (the existence-oracle and brute-force surface this
        -- table exists to throttle), so it must be recordable whether or
        -- not any matching account exists.
        "CREATE TABLE IF NOT EXISTS " <> qualifiedTableName "login_attempts" <> " (attempt_id BIGSERIAL PRIMARY KEY, attempt_key TEXT NOT NULL, attempted_at_nanoseconds BIGINT NOT NULL, succeeded TEXT NOT NULL, settled BOOLEAN NOT NULL DEFAULT true);",
        "CREATE INDEX IF NOT EXISTS login_attempts_key_time ON " <> qualifiedTableName "login_attempts" <> " (attempt_key, attempted_at_nanoseconds);"
      ]

-- | Old releases persisted a reboot-relative monotonic reading in every one
-- of these columns.  Its origin cannot be recovered as a Unix epoch, so this
-- migration deliberately revokes bearer credentials and verification tokens,
-- drops stale throttle history, and assigns the one transaction's epoch time
-- to non-authorizing historical markers whose state must remain meaningful
-- (verified, confirmed, or used).  It never pretends that the old numbers
-- themselves were epoch timestamps; see the PR-S1 decision in
-- @docs/design-guidance.md@.
epochSecurityTimeMigrationStatements :: [Text]
epochSecurityTimeMigrationStatements =
  [ "DELETE FROM web_api.account_sessions;",
    "DELETE FROM web_api.mfa_enrollment_sessions;",
    "DELETE FROM web_api.email_verifications;",
    "DELETE FROM web_api.login_attempts;",
    "UPDATE web_api.accounts SET created_at_nanoseconds = " <> migrationEpochNanoseconds <> ", email_verified_at_nanoseconds = CASE WHEN email_verified_at_nanoseconds IS NULL THEN NULL ELSE " <> migrationEpochNanoseconds <> " END;",
    "UPDATE web_api.account_totp SET created_at_nanoseconds = " <> migrationEpochNanoseconds <> ", confirmed_at_nanoseconds = CASE WHEN confirmed_at_nanoseconds IS NULL THEN NULL ELSE " <> migrationEpochNanoseconds <> " END;",
    "UPDATE web_api.account_recovery_codes SET created_at_nanoseconds = " <> migrationEpochNanoseconds <> ", used_at_nanoseconds = CASE WHEN used_at_nanoseconds IS NULL THEN NULL ELSE " <> migrationEpochNanoseconds <> " END;"
  ]

loginAttemptReservationMigrationStatements :: [Text]
loginAttemptReservationMigrationStatements =
  [ "ALTER TABLE web_api.login_attempts ADD COLUMN IF NOT EXISTS attempt_id BIGSERIAL;",
    "ALTER TABLE web_api.login_attempts ADD COLUMN IF NOT EXISTS settled BOOLEAN NOT NULL DEFAULT true;",
    "CREATE UNIQUE INDEX IF NOT EXISTS login_attempts_attempt_id_unique ON web_api.login_attempts (attempt_id);"
  ]

loginAttemptReservationFunctionMigrationStatements :: [Text]
loginAttemptReservationFunctionMigrationStatements =
  [ "CREATE OR REPLACE FUNCTION web_api.reserve_login_attempt(p_key TEXT, p_since BIGINT, p_now BIGINT, p_max BIGINT, p_lockout BIGINT) RETURNS TABLE(outcome TEXT, value TEXT) LANGUAGE plpgsql VOLATILE AS $$ DECLARE latest_failure BIGINT; failure_count BIGINT; BEGIN PERFORM pg_advisory_xact_lock(hashtextextended(p_key, 0)); SELECT max(attempted_at_nanoseconds), count(*) INTO latest_failure, failure_count FROM web_api.login_attempts WHERE attempt_key = p_key AND succeeded = 'false' AND attempted_at_nanoseconds >= p_since AND attempted_at_nanoseconds <= p_now; IF failure_count >= p_max AND latest_failure + p_lockout > p_now THEN RETURN QUERY SELECT 'throttled'::TEXT, (latest_failure + p_lockout)::TEXT; ELSE RETURN QUERY INSERT INTO web_api.login_attempts (attempt_key, attempted_at_nanoseconds, succeeded, settled) VALUES (p_key, p_now, 'false', false) RETURNING 'reserved'::TEXT, attempt_id::TEXT; END IF; END $$;"
  ]

loginAttemptStorageBoundMigrationStatements :: [Text]
loginAttemptStorageBoundMigrationStatements =
  [ "DELETE FROM web_api.login_attempts WHERE succeeded = 'true';",
    "ALTER TABLE web_api.login_attempts ADD CONSTRAINT login_attempts_key_length CHECK (char_length(attempt_key) <= 260) NOT VALID;",
    "CREATE INDEX IF NOT EXISTS login_attempts_time ON web_api.login_attempts (attempted_at_nanoseconds);",
    "DROP FUNCTION IF EXISTS web_api.reserve_login_attempt(TEXT, BIGINT, BIGINT, BIGINT, BIGINT);",
    "CREATE FUNCTION web_api.reserve_login_attempt(p_key TEXT, p_since BIGINT, p_retention_since BIGINT, p_now BIGINT, p_max BIGINT, p_lockout BIGINT, p_storage_max BIGINT) RETURNS TABLE(outcome TEXT, value TEXT) LANGUAGE plpgsql VOLATILE AS $$ DECLARE latest_failure BIGINT; failure_count BIGINT; stored_count BIGINT; BEGIN IF char_length(p_key) > 260 THEN RETURN QUERY SELECT 'key-too-long'::TEXT, ''::TEXT; RETURN; END IF; PERFORM pg_advisory_xact_lock(hashtextextended('web_api.login_attempts.capacity', 0)); DELETE FROM web_api.login_attempts WHERE attempted_at_nanoseconds < p_retention_since; PERFORM pg_advisory_xact_lock(hashtextextended(p_key, 0)); SELECT max(attempted_at_nanoseconds), count(*) INTO latest_failure, failure_count FROM web_api.login_attempts WHERE attempt_key = p_key AND succeeded = 'false' AND attempted_at_nanoseconds >= p_since AND attempted_at_nanoseconds <= p_now; IF failure_count >= p_max AND latest_failure + p_lockout > p_now THEN RETURN QUERY SELECT 'throttled'::TEXT, (latest_failure + p_lockout)::TEXT; RETURN; END IF; SELECT count(*) INTO stored_count FROM web_api.login_attempts; IF stored_count >= p_storage_max THEN RETURN QUERY SELECT 'storage-exhausted'::TEXT, ''::TEXT; RETURN; END IF; RETURN QUERY INSERT INTO web_api.login_attempts (attempt_key, attempted_at_nanoseconds, succeeded, settled) VALUES (p_key, p_now, 'false', false) RETURNING 'reserved'::TEXT, attempt_id::TEXT; END $$;"
  ]

-- | PR-S6 (2026-08-24): pending registration is one bounded, retryable
-- database lifecycle.  The function serializes capacity, email, and username
-- decisions; it deletes expired unverified accounts before enforcing the
-- application-supplied cap, leases one delivery attempt, and lets a later
-- identical request reclaim an abandoned or failed delivery.  This extends
-- the existing account/verification store rather than adding a parallel
-- outbox: the same transaction owns both the pending account and its token.
pendingRegistrationLifecycleMigrationStatements :: [Text]
pendingRegistrationLifecycleMigrationStatements =
  [ "ALTER TABLE web_api.email_verifications ADD COLUMN IF NOT EXISTS delivery_state TEXT NOT NULL DEFAULT 'awaiting';",
    "ALTER TABLE web_api.email_verifications ADD COLUMN IF NOT EXISTS delivery_claimed_at_nanoseconds BIGINT;",
    "ALTER TABLE web_api.email_verifications DROP CONSTRAINT IF EXISTS email_verifications_delivery_state_check;",
    "ALTER TABLE web_api.email_verifications ADD CONSTRAINT email_verifications_delivery_state_check CHECK (delivery_state IN ('awaiting', 'claimed', 'delivered'));",
    "CREATE UNIQUE INDEX IF NOT EXISTS email_verifications_account_unique ON web_api.email_verifications (account_id);",
    "CREATE OR REPLACE FUNCTION web_api.stage_pending_registration(p_account_id TEXT, p_email TEXT, p_password_hash TEXT, p_token_digest TEXT, p_expires_at BIGINT, p_now BIGINT, p_username TEXT, p_display_name TEXT, p_maximum_accounts BIGINT, p_claim_recover_before BIGINT) RETURNS TABLE(outcome TEXT, value TEXT) LANGUAGE plpgsql VOLATILE AS $$ DECLARE existing_account_id TEXT; existing_verified_at BIGINT; existing_delivery_state TEXT; existing_delivery_claimed_at BIGINT; pending_count BIGINT; BEGIN PERFORM pg_advisory_xact_lock(hashtextextended('web_api.pending_registration.capacity', 0)); DELETE FROM web_api.accounts pending_account WHERE pending_account.email_verified_at_nanoseconds IS NULL AND NOT EXISTS (SELECT 1 FROM web_api.email_verifications verification WHERE verification.account_id = pending_account.account_id AND verification.expires_at_nanoseconds > p_now); IF p_username <> '' THEN PERFORM pg_advisory_xact_lock(hashtextextended('web_api.pending_registration.username.' || lower(p_username), 0)); IF EXISTS (SELECT 1 FROM web_api.accounts WHERE username IS NOT NULL AND lower(username) = lower(p_username)) THEN RETURN QUERY SELECT 'username-taken'::TEXT, ''::TEXT; RETURN; END IF; END IF; PERFORM pg_advisory_xact_lock(hashtextextended('web_api.pending_registration.email.' || p_email, 0)); SELECT account_id, email_verified_at_nanoseconds INTO existing_account_id, existing_verified_at FROM web_api.accounts WHERE email_normalized = p_email; IF FOUND THEN IF existing_verified_at IS NOT NULL THEN RETURN QUERY SELECT 'email-taken'::TEXT, ''::TEXT; RETURN; END IF; SELECT delivery_state, delivery_claimed_at_nanoseconds INTO existing_delivery_state, existing_delivery_claimed_at FROM web_api.email_verifications WHERE account_id = existing_account_id; IF existing_delivery_state = 'delivered' OR (existing_delivery_state = 'claimed' AND existing_delivery_claimed_at > p_claim_recover_before) THEN RETURN QUERY SELECT 'email-taken'::TEXT, ''::TEXT; RETURN; END IF; DELETE FROM web_api.email_verifications WHERE account_id = existing_account_id; INSERT INTO web_api.email_verifications (token_digest, account_id, email_normalized, expires_at_nanoseconds, delivery_state, delivery_claimed_at_nanoseconds) VALUES (p_token_digest, existing_account_id, p_email, p_expires_at, 'claimed', p_now); RETURN QUERY SELECT 'retried'::TEXT, existing_account_id; RETURN; END IF; SELECT count(*) INTO pending_count FROM web_api.accounts WHERE email_verified_at_nanoseconds IS NULL; IF pending_count >= p_maximum_accounts THEN RETURN QUERY SELECT 'storage-exhausted'::TEXT, ''::TEXT; RETURN; END IF; INSERT INTO web_api.accounts (account_id, email_normalized, password_hash, created_at_nanoseconds, username, display_name) VALUES (p_account_id, p_email, p_password_hash, p_now, NULLIF(p_username, ''), NULLIF(p_display_name, '')); INSERT INTO web_api.email_verifications (token_digest, account_id, email_normalized, expires_at_nanoseconds, delivery_state, delivery_claimed_at_nanoseconds) VALUES (p_token_digest, p_account_id, p_email, p_expires_at, 'claimed', p_now); RETURN QUERY SELECT 'created'::TEXT, p_account_id; END $$;"
  ]

-- | AHI-2 keeps the delivered token in @email_verifications@ until a staged
-- resend has actually been handed to SMTP.  The functions own their short
-- claim and bounded history transactionally, so callers never address raw
-- rows and a failed or cancelled send cannot invalidate the old token.
verificationResendLifecycleMigrationStatements :: [Text]
verificationResendLifecycleMigrationStatements =
  [ "CREATE TABLE IF NOT EXISTS web_api.verification_resend_claims (account_id TEXT PRIMARY KEY REFERENCES web_api.accounts (account_id) ON DELETE CASCADE, token_digest TEXT NOT NULL UNIQUE, email_normalized TEXT NOT NULL, expires_at_nanoseconds BIGINT NOT NULL, claimed_at_nanoseconds BIGINT NOT NULL);",
    "CREATE TABLE IF NOT EXISTS web_api.verification_resend_deliveries (delivery_id BIGSERIAL PRIMARY KEY, account_id TEXT NOT NULL REFERENCES web_api.accounts (account_id) ON DELETE CASCADE, delivered_at_nanoseconds BIGINT NOT NULL);",
    "CREATE INDEX IF NOT EXISTS verification_resend_deliveries_account_time ON web_api.verification_resend_deliveries (account_id, delivered_at_nanoseconds);",
    "CREATE INDEX IF NOT EXISTS verification_resend_deliveries_time ON web_api.verification_resend_deliveries (delivered_at_nanoseconds);",
    "CREATE FUNCTION web_api.reserve_verification_resend(p_account_id TEXT, p_token_digest TEXT, p_email TEXT, p_expires_at BIGINT, p_now BIGINT, p_window_since BIGINT, p_claim_recover_before BIGINT, p_max_deliveries BIGINT, p_storage_max BIGINT) RETURNS TABLE(outcome TEXT, value TEXT) LANGUAGE plpgsql VOLATILE AS $$ DECLARE account_verified_at BIGINT; delivery_count BIGINT; stored_count BIGINT; BEGIN PERFORM pg_advisory_xact_lock(hashtextextended('web_api.verification_resend.capacity', 0)); DELETE FROM web_api.verification_resend_claims WHERE claimed_at_nanoseconds <= p_claim_recover_before; DELETE FROM web_api.verification_resend_deliveries WHERE delivered_at_nanoseconds <= p_window_since; PERFORM pg_advisory_xact_lock(hashtextextended('web_api.verification_resend.account.' || p_account_id, 0)); SELECT email_verified_at_nanoseconds INTO account_verified_at FROM web_api.accounts WHERE account_id = p_account_id FOR UPDATE; IF NOT FOUND OR account_verified_at IS NOT NULL THEN RETURN QUERY SELECT 'no-longer-pending'::TEXT, ''::TEXT; RETURN; END IF; IF EXISTS (SELECT 1 FROM web_api.verification_resend_claims WHERE account_id = p_account_id) THEN RETURN QUERY SELECT 'throttled'::TEXT, ''::TEXT; RETURN; END IF; SELECT count(*) INTO delivery_count FROM web_api.verification_resend_deliveries WHERE account_id = p_account_id AND delivered_at_nanoseconds > p_window_since AND delivered_at_nanoseconds <= p_now; IF delivery_count >= p_max_deliveries THEN RETURN QUERY SELECT 'throttled'::TEXT, ''::TEXT; RETURN; END IF; SELECT (SELECT count(*) FROM web_api.verification_resend_claims) + (SELECT count(*) FROM web_api.verification_resend_deliveries) INTO stored_count; IF stored_count >= p_storage_max THEN RETURN QUERY SELECT 'storage-exhausted'::TEXT, ''::TEXT; RETURN; END IF; INSERT INTO web_api.verification_resend_claims (account_id, token_digest, email_normalized, expires_at_nanoseconds, claimed_at_nanoseconds) VALUES (p_account_id, p_token_digest, p_email, p_expires_at, p_now); RETURN QUERY SELECT 'reserved'::TEXT, p_account_id; END $$;",
    "CREATE FUNCTION web_api.complete_verification_resend(p_account_id TEXT, p_token_digest TEXT, p_now BIGINT) RETURNS TABLE(outcome TEXT, value TEXT) LANGUAGE plpgsql VOLATILE AS $$ DECLARE candidate_email TEXT; candidate_expires BIGINT; account_verified_at BIGINT; BEGIN PERFORM pg_advisory_xact_lock(hashtextextended('web_api.verification_resend.account.' || p_account_id, 0)); SELECT email_verified_at_nanoseconds INTO account_verified_at FROM web_api.accounts WHERE account_id = p_account_id FOR UPDATE; DELETE FROM web_api.verification_resend_claims WHERE account_id = p_account_id AND token_digest = p_token_digest RETURNING email_normalized, expires_at_nanoseconds INTO candidate_email, candidate_expires; IF NOT FOUND OR account_verified_at IS NOT NULL THEN RETURN QUERY SELECT 'lost'::TEXT, ''::TEXT; RETURN; END IF; DELETE FROM web_api.email_verifications WHERE account_id = p_account_id; INSERT INTO web_api.email_verifications (token_digest, account_id, email_normalized, expires_at_nanoseconds, delivery_state, delivery_claimed_at_nanoseconds) VALUES (p_token_digest, p_account_id, candidate_email, candidate_expires, 'delivered', NULL); INSERT INTO web_api.verification_resend_deliveries (account_id, delivered_at_nanoseconds) VALUES (p_account_id, p_now); RETURN QUERY SELECT 'settled'::TEXT, p_account_id; END $$;",
    "CREATE FUNCTION web_api.release_verification_resend(p_account_id TEXT, p_token_digest TEXT) RETURNS TABLE(outcome TEXT, value TEXT) LANGUAGE plpgsql VOLATILE AS $$ BEGIN DELETE FROM web_api.verification_resend_claims WHERE account_id = p_account_id AND token_digest = p_token_digest; IF FOUND THEN RETURN QUERY SELECT 'settled'::TEXT, p_account_id; ELSE RETURN QUERY SELECT 'lost'::TEXT, ''::TEXT; END IF; END $$;"
  ]

-- | AHI-3 makes one logical authentication attempt own every principal and
-- trusted-peer scope together.  Groups, rather than child rows, remain the
-- globally bounded unit because each normal attempt now has two children.
-- The database revalidates the closed JSON transport before locking scopes in
-- lexical order, so a malformed adapter request cannot create a partial
-- reservation or deadlock a concurrent caller.
keyedLoginAttemptGroupMigrationStatements :: [Text]
keyedLoginAttemptGroupMigrationStatements =
  [ "DELETE FROM web_api.login_attempts;",
    "DROP FUNCTION IF EXISTS web_api.reserve_login_attempt(TEXT, BIGINT, BIGINT, BIGINT, BIGINT, BIGINT, BIGINT);",
    "CREATE TABLE IF NOT EXISTS web_api.login_attempt_groups (attempt_group_id BIGSERIAL PRIMARY KEY, attempted_at_nanoseconds BIGINT NOT NULL, succeeded TEXT NOT NULL DEFAULT 'false', settled BOOLEAN NOT NULL DEFAULT false);",
    "ALTER TABLE web_api.login_attempts ADD COLUMN IF NOT EXISTS attempt_group_id BIGINT REFERENCES web_api.login_attempt_groups (attempt_group_id) ON DELETE CASCADE;",
    "ALTER TABLE web_api.login_attempts ALTER COLUMN attempt_group_id SET NOT NULL;",
    "CREATE INDEX IF NOT EXISTS login_attempt_groups_time ON web_api.login_attempt_groups (attempted_at_nanoseconds);",
    "CREATE INDEX IF NOT EXISTS login_attempts_group ON web_api.login_attempts (attempt_group_id);",
    "CREATE OR REPLACE FUNCTION web_api.reserve_login_attempt_group(p_budgets JSONB, p_retention_since BIGINT, p_now BIGINT, p_storage_max BIGINT) RETURNS TABLE(outcome TEXT, value TEXT) LANGUAGE plpgsql VOLATILE AS $$ DECLARE budget JSONB; budget_key TEXT; budget_max BIGINT; budget_window BIGINT; budget_lockout BIGINT; latest_failure BIGINT; failure_count BIGINT; lockout_ends_at BIGINT := 0; stored_count BIGINT; reserved_group_id BIGINT; budget_count BIGINT; BEGIN IF jsonb_typeof(p_budgets) <> 'array' THEN RETURN QUERY SELECT 'invalid-budget'::TEXT, ''::TEXT; RETURN; END IF; SELECT jsonb_array_length(p_budgets) INTO budget_count; IF budget_count < 1 OR budget_count > 4 OR (SELECT count(DISTINCT entry->>'key') FROM jsonb_array_elements(p_budgets) entry) <> budget_count THEN RETURN QUERY SELECT 'invalid-budget'::TEXT, ''::TEXT; RETURN; END IF; PERFORM pg_advisory_xact_lock(hashtextextended('web_api.login_attempt_groups.capacity', 0)); DELETE FROM web_api.login_attempt_groups WHERE attempted_at_nanoseconds < p_retention_since; FOR budget IN SELECT entry FROM jsonb_array_elements(p_budgets) entry ORDER BY entry->>'key' LOOP budget_key := budget->>'key'; IF budget_key IS NULL OR char_length(budget_key) < 1 OR char_length(budget_key) > 260 OR (budget->>'maximum') !~ '^[1-9][0-9]{0,17}$' OR (budget->>'window') !~ '^[1-9][0-9]{0,17}$' OR (budget->>'lockout') !~ '^[1-9][0-9]{0,17}$' THEN RETURN QUERY SELECT 'invalid-budget'::TEXT, ''::TEXT; RETURN; END IF; budget_max := (budget->>'maximum')::BIGINT; budget_window := (budget->>'window')::BIGINT; budget_lockout := (budget->>'lockout')::BIGINT; PERFORM pg_advisory_xact_lock(hashtextextended('web_api.login_attempt.scope.' || budget_key, 0)); SELECT max(group_row.attempted_at_nanoseconds), count(*) INTO latest_failure, failure_count FROM web_api.login_attempts attempt JOIN web_api.login_attempt_groups group_row ON group_row.attempt_group_id = attempt.attempt_group_id WHERE attempt.attempt_key = budget_key AND group_row.succeeded = 'false' AND group_row.attempted_at_nanoseconds >= GREATEST(0, p_now - budget_window) AND group_row.attempted_at_nanoseconds <= p_now; IF failure_count >= budget_max AND latest_failure + budget_lockout > p_now THEN lockout_ends_at := GREATEST(lockout_ends_at, latest_failure + budget_lockout); END IF; END LOOP; IF lockout_ends_at > 0 THEN RETURN QUERY SELECT 'throttled'::TEXT, lockout_ends_at::TEXT; RETURN; END IF; SELECT count(*) INTO stored_count FROM web_api.login_attempt_groups; IF stored_count >= p_storage_max THEN RETURN QUERY SELECT 'storage-exhausted'::TEXT, ''::TEXT; RETURN; END IF; INSERT INTO web_api.login_attempt_groups (attempted_at_nanoseconds, succeeded, settled) VALUES (p_now, 'false', false) RETURNING attempt_group_id INTO reserved_group_id; FOR budget IN SELECT entry FROM jsonb_array_elements(p_budgets) entry ORDER BY entry->>'key' LOOP INSERT INTO web_api.login_attempts (attempt_key, attempted_at_nanoseconds, succeeded, settled, attempt_group_id) VALUES (budget->>'key', p_now, 'false', false, reserved_group_id); END LOOP; RETURN QUERY SELECT 'reserved'::TEXT, reserved_group_id::TEXT; END $$;"
  ]

removeSessionCsrfMigrationStatements :: [Text]
removeSessionCsrfMigrationStatements =
  [ "ALTER TABLE web_api.account_sessions DROP COLUMN IF EXISTS csrf_token;",
    "ALTER TABLE web_api.mfa_enrollment_sessions DROP COLUMN IF EXISTS csrf_token;"
  ]

migrationEpochNanoseconds :: Text
migrationEpochNanoseconds = "floor(EXTRACT(EPOCH FROM CURRENT_TIMESTAMP) * 1000000000)::BIGINT"

-- | Ownership and runtime-role grants depend on the currently configured
-- identities, unlike the durable schema history above.  Reconcile them on
-- every run, still inside AX's one locked transaction, so a runtime-password
-- rotation or changed runtime role takes effect after the initial version is
-- recorded rather than being hidden by it.
migrationReconciliationStatementsFor :: DatabaseConfig -> DatabaseConfig -> [Text]
migrationReconciliationStatementsFor migrationDatabaseConfig runtimeDatabaseConfig =
  ownershipStatements <> privilegeStatements
  where
    migrationOwner = sqlIdentifier (databaseUser migrationDatabaseConfig)
    runtimeOwner = sqlIdentifier (databaseUser runtimeDatabaseConfig)
    ownershipStatements =
      [ "ALTER SCHEMA " <> appSchemaName <> " OWNER TO " <> migrationOwner <> ";",
        "ALTER DATABASE " <> sqlIdentifier (databaseName migrationDatabaseConfig) <> " OWNER TO " <> migrationOwner <> ";"
      ]
        <> fmap (\tableName -> "ALTER TABLE " <> qualifiedTableName tableName <> " OWNER TO " <> migrationOwner <> ";") applicationTableNames
    privilegeStatements =
      case databaseUser migrationDatabaseConfig == databaseUser runtimeDatabaseConfig of
        True -> []
        False ->
          [ ensureRuntimeRoleStatement runtimeDatabaseConfig,
            "REVOKE ALL ON DATABASE " <> sqlIdentifier (databaseName runtimeDatabaseConfig) <> " FROM PUBLIC;",
            "REVOKE ALL ON DATABASE " <> sqlIdentifier (databaseName runtimeDatabaseConfig) <> " FROM " <> runtimeOwner <> ";",
            "GRANT CONNECT ON DATABASE " <> sqlIdentifier (databaseName runtimeDatabaseConfig) <> " TO " <> runtimeOwner <> ";",
            "REVOKE ALL ON SCHEMA public FROM PUBLIC;",
            "REVOKE ALL ON SCHEMA public FROM " <> runtimeOwner <> ";",
            "REVOKE ALL ON SCHEMA " <> appSchemaName <> " FROM PUBLIC;",
            "GRANT USAGE ON SCHEMA " <> appSchemaName <> " TO " <> runtimeOwner <> ";"
          ]
            <> tablePrivileges runtimeOwner

applicationTableNames :: [Text]
applicationTableNames =
  [ "page_content",
    "page_highlights",
    "accounts",
    "email_verifications",
    "account_totp",
    "account_recovery_codes",
    "account_sessions",
    "mfa_enrollment_sessions",
    "login_attempts",
    "login_attempt_groups",
    "verification_resend_claims",
    "verification_resend_deliveries"
  ]

tablePrivileges :: Text -> [Text]
tablePrivileges runtimeOwner =
  let revoke tableName = "REVOKE ALL ON TABLE " <> qualifiedTableName tableName <> " FROM PUBLIC;"
      readOnly tableName = "GRANT SELECT ON TABLE " <> qualifiedTableName tableName <> " TO " <> runtimeOwner <> ";"
      readWrite tableName = "GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE " <> qualifiedTableName tableName <> " TO " <> runtimeOwner <> ";"
   in [revoke "page_content", revoke "page_highlights", revoke "accounts", revoke "email_verifications", revoke "account_totp", revoke "account_recovery_codes", revoke "account_sessions", revoke "mfa_enrollment_sessions", revoke "login_attempts", revoke "login_attempt_groups", revoke "verification_resend_claims", revoke "verification_resend_deliveries", readOnly "page_content", readOnly "page_highlights", readWrite "accounts", readWrite "email_verifications", readWrite "account_totp", readWrite "account_recovery_codes", readWrite "account_sessions", readWrite "mfa_enrollment_sessions", readWrite "login_attempts", readWrite "login_attempt_groups", readWrite "verification_resend_claims", readWrite "verification_resend_deliveries", "GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA " <> appSchemaName <> " TO " <> runtimeOwner <> ";", "REVOKE ALL ON FUNCTION web_api.reserve_login_attempt_group(JSONB, BIGINT, BIGINT, BIGINT) FROM PUBLIC;", "GRANT EXECUTE ON FUNCTION web_api.reserve_login_attempt_group(JSONB, BIGINT, BIGINT, BIGINT) TO " <> runtimeOwner <> ";", "REVOKE ALL ON FUNCTION web_api.stage_pending_registration(TEXT, TEXT, TEXT, TEXT, BIGINT, BIGINT, TEXT, TEXT, BIGINT, BIGINT) FROM PUBLIC;", "GRANT EXECUTE ON FUNCTION web_api.stage_pending_registration(TEXT, TEXT, TEXT, TEXT, BIGINT, BIGINT, TEXT, TEXT, BIGINT, BIGINT) TO " <> runtimeOwner <> ";", "REVOKE ALL ON FUNCTION web_api.reserve_verification_resend(TEXT, TEXT, TEXT, BIGINT, BIGINT, BIGINT, BIGINT, BIGINT, BIGINT) FROM PUBLIC;", "GRANT EXECUTE ON FUNCTION web_api.reserve_verification_resend(TEXT, TEXT, TEXT, BIGINT, BIGINT, BIGINT, BIGINT, BIGINT, BIGINT) TO " <> runtimeOwner <> ";", "REVOKE ALL ON FUNCTION web_api.complete_verification_resend(TEXT, TEXT, BIGINT) FROM PUBLIC;", "GRANT EXECUTE ON FUNCTION web_api.complete_verification_resend(TEXT, TEXT, BIGINT) TO " <> runtimeOwner <> ";", "REVOKE ALL ON FUNCTION web_api.release_verification_resend(TEXT, TEXT) FROM PUBLIC;", "GRANT EXECUTE ON FUNCTION web_api.release_verification_resend(TEXT, TEXT) TO " <> runtimeOwner <> ";"]

seedStatements :: [Text]
seedStatements =
  [ "DELETE FROM " <> qualifiedTableName "page_highlights" <> ";",
    "DELETE FROM " <> qualifiedTableName "page_content" <> ";",
    "INSERT INTO " <> qualifiedTableName "page_content" <> " (route_slug, locale, summary) VALUES ('second', 'en', 'Second page content with stubbed data ready for future loaders.'), ('second', 'es', 'Contenido de la segunda pagina con datos de ejemplo listos para futuros cargadores.');"
  ]

qualifiedTableName :: Text -> Text
qualifiedTableName tableName = appSchemaName <> "." <> tableName

ensureRuntimeRoleStatement :: DatabaseConfig -> Text
ensureRuntimeRoleStatement runtimeDatabaseConfig =
  "DO $$ BEGIN IF EXISTS (SELECT 1 FROM pg_catalog.pg_roles WHERE rolname = " <> sqlLiteral (databaseUser runtimeDatabaseConfig) <> ") THEN EXECUTE " <> sqlLiteral alterRoleCommand <> "; ELSE EXECUTE " <> sqlLiteral createRoleCommand <> "; END IF; END $$;"
  where
    createRoleCommand = "CREATE ROLE " <> sqlIdentifier (databaseUser runtimeDatabaseConfig) <> " WITH LOGIN PASSWORD " <> sqlLiteral (databasePassword runtimeDatabaseConfig) <> " NOSUPERUSER NOCREATEDB NOCREATEROLE NOREPLICATION INHERIT"
    alterRoleCommand = "ALTER ROLE " <> sqlIdentifier (databaseUser runtimeDatabaseConfig) <> " WITH LOGIN PASSWORD " <> sqlLiteral (databasePassword runtimeDatabaseConfig) <> " NOSUPERUSER NOCREATEDB NOCREATEROLE NOREPLICATION INHERIT"

sqlIdentifier :: Text -> Text
sqlIdentifier value = "\"" <> Text.replace "\"" "\"\"" value <> "\""

sqlLiteral :: Text -> Text
sqlLiteral value = "'" <> Text.replace "'" "''" value <> "'"
