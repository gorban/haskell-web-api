{-# LANGUAGE OverloadedStrings #-}

module WebApi.Postgres.Migration
  ( migrationStatementsFor,
    runPostgresMigrations,
    runPostgresMigrationsForRuntime,
    runPostgresMigrationsWithRunner,
    runPostgresMigrationsWithRunnerForRuntime,
    runPostgresSeed,
    runPostgresSeedWithRunner,
    seedStatements,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import WebApi.Config (DatabaseConfig (..))
import WebApi.Postgres.Runtime
  ( PostgresCommand,
    PostgresCommandResult,
    PostgresRunnerError,
    runPostgresCommand,
    runStatements,
  )

runPostgresMigrations :: DatabaseConfig -> IO (Either PostgresRunnerError ())
runPostgresMigrations = runPostgresMigrationsWithRunner runPostgresCommand

runPostgresMigrationsWithRunner :: (PostgresCommand -> IO PostgresCommandResult) -> DatabaseConfig -> IO (Either PostgresRunnerError ())
runPostgresMigrationsWithRunner runCommand databaseConfig =
  runPostgresMigrationsWithRunnerForRuntime runCommand databaseConfig databaseConfig

runPostgresMigrationsForRuntime :: DatabaseConfig -> DatabaseConfig -> IO (Either PostgresRunnerError ())
runPostgresMigrationsForRuntime = runPostgresMigrationsWithRunnerForRuntime runPostgresCommand

runPostgresMigrationsWithRunnerForRuntime :: (PostgresCommand -> IO PostgresCommandResult) -> DatabaseConfig -> DatabaseConfig -> IO (Either PostgresRunnerError ())
runPostgresMigrationsWithRunnerForRuntime runCommand migrationDatabaseConfig runtimeDatabaseConfig =
  runStatements runCommand migrationDatabaseConfig (migrationStatementsFor migrationDatabaseConfig runtimeDatabaseConfig)

runPostgresSeed :: DatabaseConfig -> IO (Either PostgresRunnerError ())
runPostgresSeed = runPostgresSeedWithRunner runPostgresCommand

runPostgresSeedWithRunner :: (PostgresCommand -> IO PostgresCommandResult) -> DatabaseConfig -> IO (Either PostgresRunnerError ())
runPostgresSeedWithRunner runCommand databaseConfig = runStatements runCommand databaseConfig seedStatements

appSchemaName :: Text
appSchemaName = "web_api"

migrationStatementsFor :: DatabaseConfig -> DatabaseConfig -> [Text]
migrationStatementsFor migrationDatabaseConfig runtimeDatabaseConfig =
  baseSchemaStatements <> privilegeStatements
  where
    migrationOwner = sqlIdentifier (databaseUser migrationDatabaseConfig)
    runtimeOwner = sqlIdentifier (databaseUser runtimeDatabaseConfig)
    baseSchemaStatements =
      [ "CREATE SCHEMA IF NOT EXISTS " <> appSchemaName <> ";",
        "ALTER SCHEMA " <> appSchemaName <> " OWNER TO " <> migrationOwner <> ";",
        "ALTER DATABASE " <> sqlIdentifier (databaseName migrationDatabaseConfig) <> " OWNER TO " <> migrationOwner <> ";",
        "CREATE TABLE IF NOT EXISTS " <> qualifiedTableName "page_content" <> " (route_slug TEXT NOT NULL, locale TEXT NOT NULL, summary TEXT NOT NULL, PRIMARY KEY (route_slug, locale));",
        "CREATE TABLE IF NOT EXISTS " <> qualifiedTableName "page_highlights" <> " (route_slug TEXT NOT NULL, locale TEXT NOT NULL, position INTEGER NOT NULL, highlight TEXT NOT NULL, PRIMARY KEY (route_slug, locale, position));",
        "CREATE TABLE IF NOT EXISTS " <> qualifiedTableName "accounts" <> " (account_id TEXT PRIMARY KEY, email_normalized TEXT NOT NULL UNIQUE, username TEXT, display_name TEXT, password_hash TEXT NOT NULL, email_verified_at_nanoseconds BIGINT, created_at_nanoseconds BIGINT NOT NULL);",
        "ALTER TABLE " <> qualifiedTableName "accounts" <> " ADD COLUMN IF NOT EXISTS username TEXT;",
        "ALTER TABLE " <> qualifiedTableName "accounts" <> " ADD COLUMN IF NOT EXISTS display_name TEXT;",
        "CREATE UNIQUE INDEX IF NOT EXISTS accounts_username_lower_unique ON " <> qualifiedTableName "accounts" <> " (lower(username)) WHERE username IS NOT NULL;",
        "CREATE TABLE IF NOT EXISTS " <> qualifiedTableName "email_verifications" <> " (token_digest TEXT PRIMARY KEY, account_id TEXT NOT NULL REFERENCES " <> qualifiedTableName "accounts" <> " (account_id) ON DELETE CASCADE, email_normalized TEXT NOT NULL, expires_at_nanoseconds BIGINT NOT NULL);",
        "CREATE TABLE IF NOT EXISTS " <> qualifiedTableName "account_totp" <> " (account_id TEXT PRIMARY KEY REFERENCES " <> qualifiedTableName "accounts" <> " (account_id) ON DELETE CASCADE, encrypted_secret BYTEA NOT NULL, confirmed_at_nanoseconds BIGINT, created_at_nanoseconds BIGINT NOT NULL);",
        "CREATE TABLE IF NOT EXISTS " <> qualifiedTableName "account_recovery_codes" <> " (account_id TEXT NOT NULL REFERENCES " <> qualifiedTableName "accounts" <> " (account_id) ON DELETE CASCADE, code_hash TEXT NOT NULL UNIQUE, created_at_nanoseconds BIGINT NOT NULL, used_at_nanoseconds BIGINT, PRIMARY KEY (account_id, code_hash));",
        "CREATE TABLE IF NOT EXISTS " <> qualifiedTableName "account_sessions" <> " (session_id TEXT PRIMARY KEY, account_id TEXT NOT NULL REFERENCES " <> qualifiedTableName "accounts" <> " (account_id) ON DELETE CASCADE, csrf_token TEXT NOT NULL, issued_at_nanoseconds BIGINT NOT NULL, expires_at_nanoseconds BIGINT NOT NULL, invalidated_at_nanoseconds BIGINT);",
        "ALTER TABLE " <> qualifiedTableName "page_content" <> " OWNER TO " <> migrationOwner <> ";",
        "ALTER TABLE " <> qualifiedTableName "page_highlights" <> " OWNER TO " <> migrationOwner <> ";",
        "ALTER TABLE " <> qualifiedTableName "accounts" <> " OWNER TO " <> migrationOwner <> ";",
        "ALTER TABLE " <> qualifiedTableName "email_verifications" <> " OWNER TO " <> migrationOwner <> ";",
        "ALTER TABLE " <> qualifiedTableName "account_totp" <> " OWNER TO " <> migrationOwner <> ";",
        "ALTER TABLE " <> qualifiedTableName "account_recovery_codes" <> " OWNER TO " <> migrationOwner <> ";",
        "ALTER TABLE " <> qualifiedTableName "account_sessions" <> " OWNER TO " <> migrationOwner <> ";"
      ]
    privilegeStatements
      | databaseUser migrationDatabaseConfig == databaseUser runtimeDatabaseConfig = []
      | otherwise =
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

tablePrivileges :: Text -> [Text]
tablePrivileges runtimeOwner =
  let revoke tableName = "REVOKE ALL ON TABLE " <> qualifiedTableName tableName <> " FROM PUBLIC;"
      readOnly tableName = "GRANT SELECT ON TABLE " <> qualifiedTableName tableName <> " TO " <> runtimeOwner <> ";"
      readWrite tableName = "GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE " <> qualifiedTableName tableName <> " TO " <> runtimeOwner <> ";"
   in [revoke "page_content", revoke "page_highlights", revoke "accounts", revoke "email_verifications", revoke "account_totp", revoke "account_recovery_codes", revoke "account_sessions", readOnly "page_content", readOnly "page_highlights", readWrite "accounts", readWrite "email_verifications", readWrite "account_totp", readWrite "account_recovery_codes", readWrite "account_sessions"]

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
