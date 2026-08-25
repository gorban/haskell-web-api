{-# LANGUAGE OverloadedStrings #-}

module WebApi.DatabaseSetup
  ( DatabaseSetupCommand (..),
    DatabaseSetupError (..),
    loadDatabaseSetupConfig,
    parseDatabaseSetupCommand,
    parseDatabaseSetupConfig,
    renderDatabaseSetupError,
    runDatabaseSetupArgs,
    runDatabaseSetupArgsWith,
    runDatabaseSetupCommand,
    runDatabaseSetupCommandWith,
  )
where

import Control.Monad.Except (runExceptT)
import Core.Config (ConfigParseError (..), parsePositiveInt)
import Core.Control.Error (liftEitherWith)
import Data.Bifunctor (bimap)
import Data.Text (Text)
import Data.Text qualified as Text
import System.Environment (getEnvironment)
import System.IO (Handle, hPutStrLn)
import WebApi.Config (DatabaseConfig (..), committedEnvDefaults, parseRuntimeDatabaseConfig, singletonDatabasePoolCapacity)
import WebApi.Postgres.Migration
  ( runPostgresMigrationsForRuntime,
    runPostgresSeed,
  )
import WebApi.Postgres.Runtime
  ( PostgresRunnerError,
    renderRunnerError,
  )

data DatabaseSetupCommand
  = MigrateDatabase
  | SeedDatabase
  | MigrateAndSeedDatabase
  deriving (Eq, Show)

data DatabaseSetupError
  = InvalidDatabaseSetupCommand [String]
  | DatabaseSetupConfigLoadError ConfigParseError
  | DatabaseSetupRuntimeConfigLoadError ConfigParseError
  | DatabaseSetupMigrationError PostgresRunnerError
  | DatabaseSetupSeedError PostgresRunnerError
  deriving (Eq, Show)

loadDatabaseSetupConfig :: IO (Either ConfigParseError DatabaseConfig)
loadDatabaseSetupConfig =
  fmap
    ( parseDatabaseSetupConfig
        . map (bimap Text.pack Text.pack)
    )
    getEnvironment

loadRuntimeDatabaseConfig :: IO (Either ConfigParseError DatabaseConfig)
loadRuntimeDatabaseConfig =
  fmap
    ( parseRuntimeDatabaseConfig committedEnvDefaults []
        . map (bimap Text.pack Text.pack)
    )
    getEnvironment

parseDatabaseSetupConfig :: [(Text, Text)] -> Either ConfigParseError DatabaseConfig
parseDatabaseSetupConfig environmentEntries =
  DatabaseConfig
    <$> requiredConfigValue "WEB_API_MIGRATION_DATABASE_HOST"
    <*> (parsePositiveInt "WEB_API_MIGRATION_DATABASE_PORT" =<< requiredConfigValue "WEB_API_MIGRATION_DATABASE_PORT")
    <*> requiredConfigValue "WEB_API_MIGRATION_DATABASE_NAME"
    <*> requiredConfigValue "WEB_API_MIGRATION_DATABASE_USER"
    <*> requiredConfigValue "WEB_API_MIGRATION_DATABASE_PASSWORD"
    -- Not sourced from the environment: one migration transaction has no
    -- concurrent request thread to starve, but it does use the shared libpq
    -- conninfo encoder.  Keep the bounded, committed default rather than
    -- introducing a second migration-only timeout knob.
    <*> pure migrationDatabaseConnectTimeoutSeconds
    -- Migrations own one short-lived connection rather than the application's
    -- runtime pool, so this required record field is inert on this path.
    <*> pure singletonDatabasePoolCapacity
  where
    requiredConfigValue key =
      case lookup key environmentEntries of
        Just value -> Right value
        Nothing -> Left (MissingConfigValue key)

migrationDatabaseConnectTimeoutSeconds :: Int
migrationDatabaseConnectTimeoutSeconds = 10

parseDatabaseSetupCommand :: [String] -> Either DatabaseSetupError DatabaseSetupCommand
parseDatabaseSetupCommand arguments =
  case arguments of
    ["migrate"] -> Right MigrateDatabase
    ["seed"] -> Right SeedDatabase
    ["migrate-and-seed"] -> Right MigrateAndSeedDatabase
    _ -> Left (InvalidDatabaseSetupCommand arguments)

renderDatabaseSetupError :: DatabaseSetupError -> String
renderDatabaseSetupError setupError =
  case setupError of
    InvalidDatabaseSetupCommand arguments ->
      "Unsupported database setup command: "
        <> unwords arguments
        <> "\nExpected one of: migrate, seed, migrate-and-seed"
    DatabaseSetupConfigLoadError loadError ->
      "Failed to load database setup config: " <> show loadError
    DatabaseSetupRuntimeConfigLoadError loadError ->
      "Failed to load runtime database config: " <> show loadError
    DatabaseSetupMigrationError runnerError ->
      "Failed to apply database migrations: " <> Text.unpack (renderRunnerError runnerError)
    DatabaseSetupSeedError runnerError ->
      "Failed to apply database seed data: " <> Text.unpack (renderRunnerError runnerError)

runDatabaseSetupArgs :: Handle -> [String] -> IO ()
runDatabaseSetupArgs =
  runDatabaseSetupArgsWith loadDatabaseSetupConfig loadRuntimeDatabaseConfig runPostgresMigrationsForRuntime runPostgresSeed

runDatabaseSetupArgsWith ::
  IO (Either ConfigParseError DatabaseConfig) ->
  IO (Either ConfigParseError DatabaseConfig) ->
  (DatabaseConfig -> DatabaseConfig -> IO (Either PostgresRunnerError ())) ->
  (DatabaseConfig -> IO (Either PostgresRunnerError ())) ->
  Handle ->
  [String] ->
  IO ()
runDatabaseSetupArgsWith loadMigrationConfig loadRuntimeConfig runMigrations runSeed outputHandle arguments =
  either throwDatabaseSetupError runParsedCommand (parseDatabaseSetupCommand arguments)
  where
    throwDatabaseSetupError =
      ioError . userError . renderDatabaseSetupError
    runParsedCommand setupCommand =
      runDatabaseSetupCommandWith loadMigrationConfig loadRuntimeConfig runMigrations runSeed setupCommand
        >>= either throwDatabaseSetupError (const (hPutStrLn outputHandle (successMessage setupCommand)))

runDatabaseSetupCommand :: DatabaseSetupCommand -> IO (Either DatabaseSetupError ())
runDatabaseSetupCommand =
  runDatabaseSetupCommandWith loadDatabaseSetupConfig loadRuntimeDatabaseConfig runPostgresMigrationsForRuntime runPostgresSeed

runDatabaseSetupCommandWith ::
  IO (Either ConfigParseError DatabaseConfig) ->
  IO (Either ConfigParseError DatabaseConfig) ->
  (DatabaseConfig -> DatabaseConfig -> IO (Either PostgresRunnerError ())) ->
  (DatabaseConfig -> IO (Either PostgresRunnerError ())) ->
  DatabaseSetupCommand ->
  IO (Either DatabaseSetupError ())
runDatabaseSetupCommandWith loadMigrationConfig loadRuntimeConfig runMigrations runSeed setupCommand =
  runExceptT $ do
    migrationDatabaseConfig <- liftEitherWith DatabaseSetupConfigLoadError loadMigrationConfig
    runtimeDatabaseConfig <- liftEitherWith DatabaseSetupRuntimeConfigLoadError loadRuntimeConfig
    runCommandWithConfig migrationDatabaseConfig runtimeDatabaseConfig
  where
    runCommandWithConfig migrationDatabaseConfig runtimeDatabaseConfig =
      case setupCommand of
        MigrateDatabase ->
          liftEitherWith DatabaseSetupMigrationError (runMigrations migrationDatabaseConfig runtimeDatabaseConfig)
        SeedDatabase ->
          liftEitherWith DatabaseSetupSeedError (runSeed migrationDatabaseConfig)
        MigrateAndSeedDatabase ->
          liftEitherWith DatabaseSetupMigrationError (runMigrations migrationDatabaseConfig runtimeDatabaseConfig)
            >> liftEitherWith DatabaseSetupSeedError (runSeed migrationDatabaseConfig)

successMessage :: DatabaseSetupCommand -> String
successMessage setupCommand =
  case setupCommand of
    MigrateDatabase -> "Applied database migrations."
    SeedDatabase -> "Applied database seed data."
    MigrateAndSeedDatabase -> "Applied database migrations and seed data."
