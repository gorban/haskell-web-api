{-# LANGUAGE OverloadedStrings #-}

-- | Database migration and seeding command execution.
--
-- FQ9 makes loaders and runners one injected 'DatabaseSetupDependencies'
-- record. Parsed commands and the output handle remain explicit operation
-- inputs, preserving their command-specific ordering and error boundary.
module WebApi.DatabaseSetup
  ( DatabaseSetupCommand (..),
    DatabaseSetupDependencies (..),
    DatabaseSetupError (..),
    loadAccountAuditSchedulerConfig,
    loadDatabaseSetupConfig,
    parseAccountAuditSchedulerConfig,
    parseDatabaseSetupCommand,
    parseDatabaseSetupConfig,
    renderDatabaseSetupError,
    runDatabaseSetupArgs,
    runDatabaseSetupArgsWith,
    runDatabaseSetupCommand,
    runDatabaseSetupCommandWith,
  )
where

import Control.Monad (void)
import Control.Monad.Except (runExceptT)
import Core.Config (ConfigParseError (..), parsePositiveInt)
import Core.Control.Error (liftEitherWith)
import Data.Bifunctor (bimap)
import Data.Text (Text)
import Data.Text qualified as Text
import System.Environment (getEnvironment)
import System.IO (Handle, hPutStrLn)
import WebApi.Config (DatabaseConfig (..), committedEnvDefaults, parseDatabaseTransportSecurity, parseRuntimeDatabaseConfig, singletonDatabasePoolCapacity)
import WebApi.Postgres.ActivityAuditScheduler
  ( accountAuditSchedulerRoleName,
    bootstrapAccountAuditScheduler,
  )
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
  | DatabaseSetupAuditSchedulerConfigLoadError ConfigParseError
  | DatabaseSetupMigrationError PostgresRunnerError
  | DatabaseSetupAuditSchedulerError PostgresRunnerError
  | DatabaseSetupSeedError PostgresRunnerError
  deriving (Eq, Show)

data DatabaseSetupDependencies = DatabaseSetupDependencies
  { databaseSetupLoadMigrationConfig :: IO (Either ConfigParseError DatabaseConfig),
    databaseSetupLoadRuntimeConfig :: IO (Either ConfigParseError DatabaseConfig),
    databaseSetupLoadAuditSchedulerConfig :: IO (Either ConfigParseError DatabaseConfig),
    databaseSetupRunMigrations :: DatabaseConfig -> DatabaseConfig -> IO (Either PostgresRunnerError ()),
    databaseSetupInstallAuditScheduler :: DatabaseConfig -> DatabaseConfig -> IO (Either PostgresRunnerError ()),
    databaseSetupRunSeed :: DatabaseConfig -> IO (Either PostgresRunnerError ())
  }

loadDatabaseSetupConfig :: IO (Either ConfigParseError DatabaseConfig)
loadDatabaseSetupConfig =
  fmap
    ( parseDatabaseSetupConfig
        . map (bimap Text.pack Text.pack)
    )
    getEnvironment

-- | The example pg_cron adapter always authenticates directly as the fixed,
-- least-privileged scheduler role. It has separate connection configuration so
-- deployments can keep scheduler and provisioning credentials distinct.
loadAccountAuditSchedulerConfig :: IO (Either ConfigParseError DatabaseConfig)
loadAccountAuditSchedulerConfig =
  fmap
    ( parseAccountAuditSchedulerConfig
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
parseDatabaseSetupConfig =
  parseDatabaseConfig "WEB_API_MIGRATION_DATABASE"

parseAccountAuditSchedulerConfig :: [(Text, Text)] -> Either ConfigParseError DatabaseConfig
parseAccountAuditSchedulerConfig environmentEntries = do
  databaseConfig <- parseDatabaseConfig "WEB_API_AUDIT_SCHEDULER_DATABASE" environmentEntries
  if databaseUser databaseConfig == accountAuditSchedulerRoleName
    then Right databaseConfig
    else Left (InvalidConfigValue "WEB_API_AUDIT_SCHEDULER_DATABASE_USER" (databaseUser databaseConfig))

parseDatabaseConfig :: Text -> [(Text, Text)] -> Either ConfigParseError DatabaseConfig
parseDatabaseConfig prefix environmentEntries =
  DatabaseConfig
    <$> requiredConfigValue "HOST"
    <*> (parsePositiveInt (configKey "PORT") =<< requiredConfigValue "PORT")
    <*> requiredConfigValue "NAME"
    <*> requiredConfigValue "USER"
    <*> requiredConfigValue "PASSWORD"
    -- Not sourced from the environment: one migration transaction has no
    -- concurrent request thread to starve, but it does use the shared libpq
    -- conninfo encoder.  Keep the bounded, committed default rather than
    -- introducing a second migration-only timeout knob.
    <*> pure migrationDatabaseConnectTimeoutSeconds
    -- Migrations own one short-lived connection rather than the application's
    -- runtime pool, so this required record field is inert on this path.
    <*> pure singletonDatabasePoolCapacity
    <*> parseDatabaseTransportSecurity
      (configKey "SSL_MODE")
      (configKey "SSL_ROOT_CERT")
      (lookup (configKey "SSL_MODE") environmentEntries)
      (lookup (configKey "SSL_ROOT_CERT") environmentEntries)
  where
    configKey suffix = prefix <> "_" <> suffix
    requiredConfigValue suffix =
      case lookup (configKey suffix) environmentEntries of
        Just value -> Right value
        Nothing -> Left (MissingConfigValue (configKey suffix))

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
    DatabaseSetupAuditSchedulerConfigLoadError loadError ->
      "Failed to load account-audit scheduler config: " <> show loadError
    DatabaseSetupMigrationError runnerError ->
      "Failed to apply database migrations: " <> Text.unpack (renderRunnerError runnerError)
    DatabaseSetupAuditSchedulerError runnerError ->
      "Failed to install account-audit scheduler: " <> Text.unpack (renderRunnerError runnerError)
    DatabaseSetupSeedError runnerError ->
      "Failed to apply database seed data: " <> Text.unpack (renderRunnerError runnerError)

runDatabaseSetupArgs :: Handle -> [String] -> IO ()
runDatabaseSetupArgs =
  runDatabaseSetupArgsWith defaultDatabaseSetupDependencies

runDatabaseSetupArgsWith :: DatabaseSetupDependencies -> Handle -> [String] -> IO ()
runDatabaseSetupArgsWith dependencies outputHandle arguments =
  either throwDatabaseSetupError runParsedCommand (parseDatabaseSetupCommand arguments)
  where
    throwDatabaseSetupError =
      ioError . userError . renderDatabaseSetupError
    runParsedCommand setupCommand =
      runDatabaseSetupCommandWith dependencies setupCommand
        >>= either throwDatabaseSetupError (const (hPutStrLn outputHandle (successMessage setupCommand)))

runDatabaseSetupCommand :: DatabaseSetupCommand -> IO (Either DatabaseSetupError ())
runDatabaseSetupCommand =
  runDatabaseSetupCommandWith defaultDatabaseSetupDependencies

runDatabaseSetupCommandWith :: DatabaseSetupDependencies -> DatabaseSetupCommand -> IO (Either DatabaseSetupError ())
runDatabaseSetupCommandWith dependencies setupCommand =
  runExceptT $
    case setupCommand of
      SeedDatabase -> do
        migrationDatabaseConfig <- liftEitherWith DatabaseSetupConfigLoadError (databaseSetupLoadMigrationConfig dependencies)
        liftEitherWith DatabaseSetupSeedError (databaseSetupRunSeed dependencies migrationDatabaseConfig)
      MigrateDatabase -> void runMigrationsAndInstallScheduler
      MigrateAndSeedDatabase -> do
        migrationDatabaseConfig <- runMigrationsAndInstallScheduler
        liftEitherWith DatabaseSetupSeedError (databaseSetupRunSeed dependencies migrationDatabaseConfig)
  where
    runMigrationsAndInstallScheduler = do
      migrationDatabaseConfig <- liftEitherWith DatabaseSetupConfigLoadError (databaseSetupLoadMigrationConfig dependencies)
      runtimeDatabaseConfig <- liftEitherWith DatabaseSetupRuntimeConfigLoadError (databaseSetupLoadRuntimeConfig dependencies)
      auditSchedulerDatabaseConfig <- liftEitherWith DatabaseSetupAuditSchedulerConfigLoadError (databaseSetupLoadAuditSchedulerConfig dependencies)
      liftEitherWith DatabaseSetupMigrationError (databaseSetupRunMigrations dependencies migrationDatabaseConfig runtimeDatabaseConfig)
      liftEitherWith DatabaseSetupAuditSchedulerError (databaseSetupInstallAuditScheduler dependencies migrationDatabaseConfig auditSchedulerDatabaseConfig)
      pure migrationDatabaseConfig

defaultDatabaseSetupDependencies :: DatabaseSetupDependencies
defaultDatabaseSetupDependencies =
  DatabaseSetupDependencies
    { databaseSetupLoadMigrationConfig = loadDatabaseSetupConfig,
      databaseSetupLoadRuntimeConfig = loadRuntimeDatabaseConfig,
      databaseSetupLoadAuditSchedulerConfig = loadAccountAuditSchedulerConfig,
      databaseSetupRunMigrations = runPostgresMigrationsForRuntime,
      databaseSetupInstallAuditScheduler = bootstrapAccountAuditScheduler,
      databaseSetupRunSeed = runPostgresSeed
    }

successMessage :: DatabaseSetupCommand -> String
successMessage setupCommand =
  case setupCommand of
    MigrateDatabase -> "Applied database migrations."
    SeedDatabase -> "Applied database seed data."
    MigrateAndSeedDatabase -> "Applied database migrations and seed data."
