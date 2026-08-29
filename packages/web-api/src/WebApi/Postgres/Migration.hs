{-# LANGUAGE OverloadedStrings #-}

-- | Versioned schema migration runs own one short-lived, owner-credential
-- libpq connection for the complete transaction.  This deliberately differs
-- from the application's shared runtime pool: a migration is exclusive,
-- privileged setup work and must retain its advisory lock until commit or
-- rollback.  Recorded structural migrations run once; configured ownership
-- and runtime-role privileges reconcile on every run in that same transaction,
-- so a changed runtime identity is not hidden by a schema version.  See the
-- AX decision record in @docs/design-guidance.md@.
module WebApi.Postgres.Migration
  ( migrationStatementsFor,
    PostgresMigrationExecutor (..),
    PostgresMigrationResult (..),
    runPostgresMigrations,
    runPostgresMigrationsForRuntime,
    runPostgresMigrationsWithExecutor,
    runPostgresSeed,
    runPostgresSeedWithRunner,
    seedStatements,
  )
where

import Control.Exception (bracket)
import Control.Monad (unless, void)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Core.Control.Error (liftEitherWith)
import Data.Bifunctor (first)
import Data.ByteString qualified as ByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Database.PostgreSQL.LibPQ qualified as LibPQ
import WebApi.Config (DatabaseConfig (..))
import WebApi.Postgres.Pool (runtimeConnectionString)
import WebApi.Postgres.Runtime
  ( PostgresCommand,
    PostgresCommandResult,
    PostgresRunnerError (..),
    runPostgresCommand,
    runStatements,
  )

-- | A connection-scoped SQL executor.  It is deliberately migration-specific:
-- tests can record transaction and version-table operations without preserving
-- the old one-process-per-statement @psql@ fixture contract.  Its result is on
-- the same typed rail as the transaction: an adapter can report a malformed
-- wire value without throwing or inventing a schema version.
newtype PostgresMigrationExecutor = PostgresMigrationExecutor
  { executeMigrationSql :: Text -> IO (Either PostgresRunnerError (Maybe PostgresMigrationResult))
  }

-- | The two successful shapes the migration protocol accepts.  Commands and
-- queries are intentionally distinct, so a test or libpq interpreter cannot
-- accidentally treat an empty command result as an applied-version query.
-- Query cells stay as raw optional bytes until the transaction's typed
-- decoding boundary, where NULL and invalid UTF-8 become explicit failures.
data PostgresMigrationResult
  = PostgresMigrationCommandSucceeded
  | PostgresMigrationRows [Maybe ByteString.ByteString]

-- | One ordered, durable schema change.  The version is recorded only after
-- every statement in this migration has succeeded, while the surrounding
-- transaction makes that record and its schema changes atomic.
data PostgresMigration = PostgresMigration
  { postgresMigrationVersion :: Text,
    postgresMigrationStatements :: [Text]
  }

runPostgresMigrations :: DatabaseConfig -> IO (Either PostgresRunnerError ())
runPostgresMigrations databaseConfig =
  runPostgresMigrationsForRuntime databaseConfig databaseConfig

runPostgresMigrationsForRuntime :: DatabaseConfig -> DatabaseConfig -> IO (Either PostgresRunnerError ())
runPostgresMigrationsForRuntime migrationDatabaseConfig runtimeDatabaseConfig =
  bracket
    (LibPQ.connectdb (runtimeConnectionString migrationDatabaseConfig))
    LibPQ.finish
    (\connection -> runPostgresMigrationsWithExecutor (libpqMigrationExecutor connection) migrationDatabaseConfig runtimeDatabaseConfig)

runPostgresMigrationsWithExecutor :: PostgresMigrationExecutor -> DatabaseConfig -> DatabaseConfig -> IO (Either PostgresRunnerError ())
runPostgresMigrationsWithExecutor executor migrationDatabaseConfig runtimeDatabaseConfig = do
  begun <- runExceptT (executeMigration executor "BEGIN;")
  case begun of
    Left failure -> pure (Left failure)
    Right () -> do
      result <-
        runExceptT $ do
          runMigrationTransaction executor migrationDatabaseConfig runtimeDatabaseConfig
          executeMigration executor "COMMIT;"
      case result of
        Right () -> pure (Right ())
        Left failure -> rollbackAfterFailure executor failure

rollbackAfterFailure :: PostgresMigrationExecutor -> PostgresRunnerError -> IO (Either PostgresRunnerError ())
rollbackAfterFailure executor failure = do
  void (runExceptT (executeMigration executor "ROLLBACK;"))
  pure (Left failure)

runMigrationTransaction :: PostgresMigrationExecutor -> DatabaseConfig -> DatabaseConfig -> ExceptT PostgresRunnerError IO ()
runMigrationTransaction executor migrationDatabaseConfig runtimeDatabaseConfig = do
  executeMigrationQuery executor "SELECT pg_advisory_xact_lock(782476311);"
  runMigrationStatements executor migrationBootstrapStatements
  appliedVersions <- executeMigrationRows executor "SELECT version FROM web_api.schema_migrations ORDER BY version ASC;"
  runPendingMigrations executor appliedVersions migrations
  runMigrationStatements executor (migrationReconciliationStatementsFor migrationDatabaseConfig runtimeDatabaseConfig)

runPendingMigrations :: PostgresMigrationExecutor -> [Text] -> [PostgresMigration] -> ExceptT PostgresRunnerError IO ()
runPendingMigrations executor appliedVersions migrationPlan =
  case unknownAppliedVersions appliedVersions migrationPlan of
    unknownVersion : _ -> throwError (PostgresMigrationFailed ("Unknown PostgreSQL schema migration version: " <> unknownVersion))
    [] -> mapM_ runMigration migrationPlan
  where
    runMigration migration =
      unless (postgresMigrationVersion migration `elem` appliedVersions) $ do
        runMigrationStatements executor (postgresMigrationStatements migration)
        executeMigration
          executor
          ("INSERT INTO web_api.schema_migrations (version) VALUES (" <> sqlLiteral (postgresMigrationVersion migration) <> ");")

unknownAppliedVersions :: [Text] -> [PostgresMigration] -> [Text]
unknownAppliedVersions appliedVersions migrationPlan =
  filter (`notElem` map postgresMigrationVersion migrationPlan) appliedVersions

runMigrationStatements :: PostgresMigrationExecutor -> [Text] -> ExceptT PostgresRunnerError IO ()
runMigrationStatements executor = mapM_ (executeMigration executor)

executeMigration :: PostgresMigrationExecutor -> Text -> ExceptT PostgresRunnerError IO ()
executeMigration executor sql = do
  result <- runMigrationSql executor sql
  case result of
    Nothing -> throwError (PostgresMigrationFailed "PostgreSQL migration command failed")
    Just PostgresMigrationCommandSucceeded -> pure ()
    Just (PostgresMigrationRows _) -> throwError (PostgresMigrationFailed "PostgreSQL migration command returned rows")

executeMigrationRows :: PostgresMigrationExecutor -> Text -> ExceptT PostgresRunnerError IO [Text]
executeMigrationRows executor sql = do
  result <- runMigrationSql executor sql
  case result of
    Nothing -> throwError (PostgresMigrationFailed "PostgreSQL migration command failed")
    Just PostgresMigrationCommandSucceeded -> throwError (PostgresMigrationFailed "PostgreSQL migration version query returned no rows")
    Just (PostgresMigrationRows rows) -> liftEitherWith id (pure (traverse decodeMigrationVersion rows))

executeMigrationQuery :: PostgresMigrationExecutor -> Text -> ExceptT PostgresRunnerError IO ()
executeMigrationQuery executor sql = do
  void (executeMigrationRows executor sql)

runMigrationSql :: PostgresMigrationExecutor -> Text -> ExceptT PostgresRunnerError IO (Maybe PostgresMigrationResult)
runMigrationSql executor sql =
  liftEitherWith id (executeMigrationSql executor sql)

migrationBootstrapStatements :: [Text]
migrationBootstrapStatements =
  [ "CREATE SCHEMA IF NOT EXISTS web_api;",
    "CREATE TABLE IF NOT EXISTS web_api.schema_migrations (version TEXT PRIMARY KEY, applied_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP);"
  ]

migrations :: [PostgresMigration]
migrations =
  [ PostgresMigration
      { postgresMigrationVersion = "initial-schema",
        postgresMigrationStatements = migrationStatementsFor
      },
    PostgresMigration
      { postgresMigrationVersion = "epoch-security-time-v1",
        postgresMigrationStatements = epochSecurityTimeMigrationStatements
      },
    PostgresMigration
      { postgresMigrationVersion = "login-attempt-reservations-v1",
        postgresMigrationStatements = loginAttemptReservationMigrationStatements
      },
    PostgresMigration
      { postgresMigrationVersion = "login-attempt-reservation-function-v1",
        postgresMigrationStatements = loginAttemptReservationFunctionMigrationStatements
      },
    PostgresMigration
      { postgresMigrationVersion = "login-attempt-storage-bound-v1",
        postgresMigrationStatements = loginAttemptStorageBoundMigrationStatements
      },
    PostgresMigration
      { postgresMigrationVersion = "pending-registration-lifecycle-v1",
        postgresMigrationStatements = pendingRegistrationLifecycleMigrationStatements
      }
  ]

libpqMigrationExecutor :: LibPQ.Connection -> PostgresMigrationExecutor
libpqMigrationExecutor connection =
  PostgresMigrationExecutor (runLibpqMigrationSql connection)

runLibpqMigrationSql :: LibPQ.Connection -> Text -> IO (Either PostgresRunnerError (Maybe PostgresMigrationResult))
runLibpqMigrationSql connection sql = do
  maybeResult <- LibPQ.exec connection (TextEncoding.encodeUtf8 sql)
  case maybeResult of
    Nothing -> pure (Right Nothing)
    Just result -> do
      status <- LibPQ.resultStatus result
      if status == LibPQ.CommandOk
        then pure (Right (Just PostgresMigrationCommandSucceeded))
        else
          if status == LibPQ.TuplesOk
            then fmap (Right . Just . PostgresMigrationRows) (readMigrationRows result)
            else pure (Right Nothing)

readMigrationRows :: LibPQ.Result -> IO [Maybe ByteString.ByteString]
readMigrationRows result = do
  rowCount <- LibPQ.ntuples result
  traverse (\rowIndex -> LibPQ.getvalue result rowIndex 0) [0 .. rowCount - 1]

decodeMigrationVersion :: Maybe ByteString.ByteString -> Either PostgresRunnerError Text
decodeMigrationVersion maybeValue =
  case maybeValue of
    Nothing -> Left (PostgresMigrationFailed "PostgreSQL migration version row was NULL")
    Just value ->
      first
        (const (PostgresMigrationFailed "PostgreSQL migration version row contained invalid UTF-8"))
        (TextEncoding.decodeUtf8' value)

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
    "login_attempts"
  ]

tablePrivileges :: Text -> [Text]
tablePrivileges runtimeOwner =
  let revoke tableName = "REVOKE ALL ON TABLE " <> qualifiedTableName tableName <> " FROM PUBLIC;"
      readOnly tableName = "GRANT SELECT ON TABLE " <> qualifiedTableName tableName <> " TO " <> runtimeOwner <> ";"
      readWrite tableName = "GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE " <> qualifiedTableName tableName <> " TO " <> runtimeOwner <> ";"
   in [revoke "page_content", revoke "page_highlights", revoke "accounts", revoke "email_verifications", revoke "account_totp", revoke "account_recovery_codes", revoke "account_sessions", revoke "mfa_enrollment_sessions", revoke "login_attempts", readOnly "page_content", readOnly "page_highlights", readWrite "accounts", readWrite "email_verifications", readWrite "account_totp", readWrite "account_recovery_codes", readWrite "account_sessions", readWrite "mfa_enrollment_sessions", readWrite "login_attempts", "GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA " <> appSchemaName <> " TO " <> runtimeOwner <> ";", "REVOKE ALL ON FUNCTION web_api.reserve_login_attempt(TEXT, BIGINT, BIGINT, BIGINT, BIGINT, BIGINT, BIGINT) FROM PUBLIC;", "GRANT EXECUTE ON FUNCTION web_api.reserve_login_attempt(TEXT, BIGINT, BIGINT, BIGINT, BIGINT, BIGINT, BIGINT) TO " <> runtimeOwner <> ";", "REVOKE ALL ON FUNCTION web_api.stage_pending_registration(TEXT, TEXT, TEXT, TEXT, BIGINT, BIGINT, TEXT, TEXT, BIGINT, BIGINT) FROM PUBLIC;", "GRANT EXECUTE ON FUNCTION web_api.stage_pending_registration(TEXT, TEXT, TEXT, TEXT, BIGINT, BIGINT, TEXT, TEXT, BIGINT, BIGINT) TO " <> runtimeOwner <> ";"]

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
