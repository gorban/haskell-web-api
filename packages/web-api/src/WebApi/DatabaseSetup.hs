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

import Core.Config (ConfigParseError (..), parsePositiveInt)
import Data.Bifunctor (bimap)
import Data.Text (Text)
import Data.Text qualified as Text
import System.Environment (getEnvironment)
import System.IO (Handle, hPutStrLn)
import WebApi.Config (DatabaseConfig (..))
import WebApi.Postgres
  ( PostgresRunnerError,
    runPostgresMigrations,
    runPostgresSeed,
  )

data DatabaseSetupCommand
  = MigrateDatabase
  | SeedDatabase
  | MigrateAndSeedDatabase
  deriving (Eq, Show)

data DatabaseSetupError
  = InvalidDatabaseSetupCommand [String]
  | DatabaseSetupConfigLoadError ConfigParseError
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

parseDatabaseSetupConfig :: [(Text, Text)] -> Either ConfigParseError DatabaseConfig
parseDatabaseSetupConfig environmentEntries =
  DatabaseConfig
    <$> requiredConfigValue "WEB_API_MIGRATION_DATABASE_HOST"
    <*> (parsePositiveInt "WEB_API_MIGRATION_DATABASE_PORT" =<< requiredConfigValue "WEB_API_MIGRATION_DATABASE_PORT")
    <*> requiredConfigValue "WEB_API_MIGRATION_DATABASE_NAME"
    <*> requiredConfigValue "WEB_API_MIGRATION_DATABASE_USER"
    <*> requiredConfigValue "WEB_API_MIGRATION_DATABASE_PASSWORD"
  where
    requiredConfigValue key =
      case lookup key environmentEntries of
        Just value -> Right value
        Nothing -> Left (MissingConfigValue key)

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
    DatabaseSetupMigrationError runnerError ->
      "Failed to apply database migrations: " <> show runnerError
    DatabaseSetupSeedError runnerError ->
      "Failed to apply database seed data: " <> show runnerError

runDatabaseSetupArgs :: Handle -> [String] -> IO ()
runDatabaseSetupArgs =
  runDatabaseSetupArgsWith loadDatabaseSetupConfig runPostgresMigrations runPostgresSeed

runDatabaseSetupArgsWith ::
  IO (Either ConfigParseError DatabaseConfig) ->
  (DatabaseConfig -> IO (Either PostgresRunnerError ())) ->
  (DatabaseConfig -> IO (Either PostgresRunnerError ())) ->
  Handle ->
  [String] ->
  IO ()
runDatabaseSetupArgsWith loadEnvironmentConfig runMigrations runSeed outputHandle arguments =
  case parseDatabaseSetupCommand arguments of
    Left setupError -> ioError (userError (renderDatabaseSetupError setupError))
    Right setupCommand -> do
      setupResult <- runDatabaseSetupCommandWith loadEnvironmentConfig runMigrations runSeed setupCommand
      case setupResult of
        Left setupError -> ioError (userError (renderDatabaseSetupError setupError))
        Right () -> hPutStrLn outputHandle (successMessage setupCommand)

runDatabaseSetupCommand :: DatabaseSetupCommand -> IO (Either DatabaseSetupError ())
runDatabaseSetupCommand =
  runDatabaseSetupCommandWith loadDatabaseSetupConfig runPostgresMigrations runPostgresSeed

runDatabaseSetupCommandWith ::
  IO (Either ConfigParseError DatabaseConfig) ->
  (DatabaseConfig -> IO (Either PostgresRunnerError ())) ->
  (DatabaseConfig -> IO (Either PostgresRunnerError ())) ->
  DatabaseSetupCommand ->
  IO (Either DatabaseSetupError ())
runDatabaseSetupCommandWith loadEnvironmentConfig runMigrations runSeed setupCommand = do
  databaseConfigResult <- loadEnvironmentConfig
  case databaseConfigResult of
    Left loadError -> pure (Left (DatabaseSetupConfigLoadError loadError))
    Right databaseRuntimeConfig ->
      runCommandWithConfig databaseRuntimeConfig
  where
    runCommandWithConfig databaseRuntimeConfig =
      case setupCommand of
        MigrateDatabase ->
          fmap (either (Left . DatabaseSetupMigrationError) Right) (runMigrations databaseRuntimeConfig)
        SeedDatabase ->
          fmap (either (Left . DatabaseSetupSeedError) Right) (runSeed databaseRuntimeConfig)
        MigrateAndSeedDatabase -> do
          migrationsResult <- runMigrations databaseRuntimeConfig
          case migrationsResult of
            Left migrationError -> pure (Left (DatabaseSetupMigrationError migrationError))
            Right () ->
              fmap (either (Left . DatabaseSetupSeedError) Right) (runSeed databaseRuntimeConfig)

successMessage :: DatabaseSetupCommand -> String
successMessage setupCommand =
  case setupCommand of
    MigrateDatabase -> "Applied database migrations."
    SeedDatabase -> "Applied database seed data."
    MigrateAndSeedDatabase -> "Applied database migrations and seed data."
