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
import Control.Monad (void)
import Data.Maybe (fromJust)
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
-- the old one-process-per-statement @psql@ fixture contract.
newtype PostgresMigrationExecutor = PostgresMigrationExecutor
  { executeMigrationSql :: Text -> IO (Maybe PostgresMigrationResult)
  }

-- | The two successful shapes the migration protocol accepts.  Commands and
-- queries are intentionally distinct, so a test or libpq interpreter cannot
-- accidentally treat an empty command result as an applied-version query.
data PostgresMigrationResult
  = PostgresMigrationCommandSucceeded
  | PostgresMigrationRows [Text]

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
  begun <- executeMigration executor "BEGIN;"
  case begun of
    Left failure -> pure (Left failure)
    Right () -> do
      result <- runMigrationTransaction executor migrationDatabaseConfig runtimeDatabaseConfig
      case result of
        Right () -> do
          commitResult <- executeMigration executor "COMMIT;"
          case commitResult of
            Right () -> pure (Right ())
            Left failure -> rollbackAfterFailure executor failure
        Left failure -> do
          rollbackAfterFailure executor failure

rollbackAfterFailure :: PostgresMigrationExecutor -> PostgresRunnerError -> IO (Either PostgresRunnerError ())
rollbackAfterFailure executor failure = do
  void (executeMigration executor "ROLLBACK;")
  pure (Left failure)

runMigrationTransaction :: PostgresMigrationExecutor -> DatabaseConfig -> DatabaseConfig -> IO (Either PostgresRunnerError ())
runMigrationTransaction executor migrationDatabaseConfig runtimeDatabaseConfig = do
  lockResult <- executeMigrationQuery executor "SELECT pg_advisory_xact_lock(782476311);"
  case lockResult of
    Left failure -> pure (Left failure)
    Right () -> do
      bootstrapResult <- runMigrationStatements executor migrationBootstrapStatements
      case bootstrapResult of
        Left failure -> pure (Left failure)
        Right () -> do
          versionsResult <- executeMigrationRows executor "SELECT version FROM web_api.schema_migrations ORDER BY version ASC;"
          case versionsResult of
            Left failure -> pure (Left failure)
            Right appliedVersions -> do
              pendingResult <- runPendingMigrations executor appliedVersions migrations
              case pendingResult of
                Left failure -> pure (Left failure)
                Right () -> runMigrationStatements executor (migrationReconciliationStatementsFor migrationDatabaseConfig runtimeDatabaseConfig)

runPendingMigrations :: PostgresMigrationExecutor -> [Text] -> [PostgresMigration] -> IO (Either PostgresRunnerError ())
runPendingMigrations executor appliedVersions migrationPlan =
  case unknownAppliedVersions appliedVersions migrationPlan of
    unknownVersion : _ -> pure (Left (PostgresMigrationFailed ("Unknown PostgreSQL schema migration version: " <> unknownVersion)))
    [] -> go migrationPlan
  where
    go remainingMigrations =
      case remainingMigrations of
        [] -> pure (Right ())
        migration : rest ->
          case postgresMigrationVersion migration `elem` appliedVersions of
            True -> go rest
            False -> do
              statementsResult <- runMigrationStatements executor (postgresMigrationStatements migration)
              case statementsResult of
                Left failure -> pure (Left failure)
                Right () -> do
                  recordResult <-
                    executeMigration
                      executor
                      ("INSERT INTO web_api.schema_migrations (version) VALUES (" <> sqlLiteral (postgresMigrationVersion migration) <> ");")
                  case recordResult of
                    Left failure -> pure (Left failure)
                    Right () -> go rest

unknownAppliedVersions :: [Text] -> [PostgresMigration] -> [Text]
unknownAppliedVersions appliedVersions migrationPlan =
  filter (`notElem` map postgresMigrationVersion migrationPlan) appliedVersions

runMigrationStatements :: PostgresMigrationExecutor -> [Text] -> IO (Either PostgresRunnerError ())
runMigrationStatements executor = go
  where
    go statements =
      case statements of
        [] -> pure (Right ())
        statement : remainingStatements -> do
          statementResult <- executeMigration executor statement
          case statementResult of
            Left failure -> pure (Left failure)
            Right () -> go remainingStatements

executeMigration :: PostgresMigrationExecutor -> Text -> IO (Either PostgresRunnerError ())
executeMigration executor sql = do
  result <- executeMigrationSql executor sql
  pure $
    case result of
      Nothing -> Left (PostgresMigrationFailed "PostgreSQL migration command failed")
      Just PostgresMigrationCommandSucceeded -> Right ()
      Just (PostgresMigrationRows _) -> Left (PostgresMigrationFailed "PostgreSQL migration command returned rows")

executeMigrationRows :: PostgresMigrationExecutor -> Text -> IO (Either PostgresRunnerError [Text])
executeMigrationRows executor sql = do
  result <- executeMigrationSql executor sql
  pure $
    case result of
      Nothing -> Left (PostgresMigrationFailed "PostgreSQL migration command failed")
      Just PostgresMigrationCommandSucceeded -> Left (PostgresMigrationFailed "PostgreSQL migration version query returned no rows")
      Just (PostgresMigrationRows rows) -> Right rows

executeMigrationQuery :: PostgresMigrationExecutor -> Text -> IO (Either PostgresRunnerError ())
executeMigrationQuery executor sql = void <$> executeMigrationRows executor sql

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
      }
  ]

libpqMigrationExecutor :: LibPQ.Connection -> PostgresMigrationExecutor
libpqMigrationExecutor connection =
  PostgresMigrationExecutor (runLibpqMigrationSql connection)

runLibpqMigrationSql :: LibPQ.Connection -> Text -> IO (Maybe PostgresMigrationResult)
runLibpqMigrationSql connection sql = do
  maybeResult <- LibPQ.exec connection (TextEncoding.encodeUtf8 sql)
  case maybeResult of
    Nothing -> pure Nothing
    Just result -> do
      status <- LibPQ.resultStatus result
      if status == LibPQ.CommandOk
        then pure (Just PostgresMigrationCommandSucceeded)
        else
          if status == LibPQ.TuplesOk
            then fmap (Just . PostgresMigrationRows) (readMigrationRows result)
            else pure Nothing

readMigrationRows :: LibPQ.Result -> IO [Text]
readMigrationRows result = do
  rowCount <- LibPQ.ntuples result
  values <- traverse (\rowIndex -> LibPQ.getvalue result rowIndex 0) [0 .. rowCount - 1]
  -- @schema_migrations.version@ is the primary key created above, hence SQL
  -- cannot return NULL here.  Keeping that invariant at the database boundary
  -- avoids turning an impossible wire value into a fake migration version.
  pure (fmap (TextEncoding.decodeUtf8 . fromJust) values)

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
        "CREATE TABLE IF NOT EXISTS " <> qualifiedTableName "login_attempts" <> " (attempt_key TEXT NOT NULL, attempted_at_nanoseconds BIGINT NOT NULL, succeeded TEXT NOT NULL);",
        "CREATE INDEX IF NOT EXISTS login_attempts_key_time ON " <> qualifiedTableName "login_attempts" <> " (attempt_key, attempted_at_nanoseconds);"
      ]

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
   in [revoke "page_content", revoke "page_highlights", revoke "accounts", revoke "email_verifications", revoke "account_totp", revoke "account_recovery_codes", revoke "account_sessions", revoke "mfa_enrollment_sessions", revoke "login_attempts", readOnly "page_content", readOnly "page_highlights", readWrite "accounts", readWrite "email_verifications", readWrite "account_totp", readWrite "account_recovery_codes", readWrite "account_sessions", readWrite "mfa_enrollment_sessions", readWrite "login_attempts"]

seedStatements :: [Text]
seedStatements =
  [ "DELETE FROM " <> qualifiedTableName "page_highlights" <> ";",
    "DELETE FROM " <> qualifiedTableName "page_content" <> ";",
    "INSERT INTO " <> qualifiedTableName "page_content" <> " (route_slug, locale, summary) VALUES ('home', 'en', 'Server-rendered home page with stubbed content.'), ('home', 'es', 'Inicio renderizado en el servidor con datos de desarrollo preconfigurados.'), ('second', 'en', 'Second page content with stubbed data ready for future loaders.'), ('second', 'es', 'Contenido de la segunda pagina con datos de ejemplo listos para futuros cargadores.');"
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
