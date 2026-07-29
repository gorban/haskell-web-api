{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module WebApi.SetupConfig
  ( AppSetupConfig (..),
    AppSetupConfigLoadError (..),
    SetupAutostartConfig (..),
    committedSetupDefaults,
    defaultAppSetupConfig,
    defaultSetupAutostartConfig,
    loadAppSetupConfig,
    loadAppSetupConfigWithFiles,
    parseAppSetupConfig,
  )
where

import Core.Config
  ( ConfigOverridesFileError (..),
    ConfigParseError (..),
    loadConfigOverridesFile,
    lookupConfigValue,
    parseBoolean,
  )
import Data.Bifunctor (bimap, first)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import System.Environment (getEnvironment)
import WebApi.Config
  ( AppConfig,
    AppEnvironmentConfig,
    DatabaseConfig,
    committedEnvDefaults,
    committedRuntimeDefaults,
    defaultAppConfig,
    defaultAppEnvironmentConfig,
    parseAppEnvironmentConfig,
    parseRuntimeAppConfig,
  )
import WebApi.DatabaseSetup (parseDatabaseSetupConfig)

data SetupAutostartConfig = SetupAutostartConfig
  { setupAutostartDatabase :: Bool,
    setupAutostartJaeger :: Bool
  }
  deriving (Eq, Show)

data AppSetupConfig = AppSetupConfig
  { setupEnvironmentConfig :: AppEnvironmentConfig,
    setupAppConfig :: AppConfig,
    setupMigrationDatabaseConfig :: Maybe DatabaseConfig,
    setupAutostartConfig :: SetupAutostartConfig
  }
  deriving (Eq, Show)

data AppSetupConfigLoadError
  = AppSetupOverridesFileError FilePath ConfigOverridesFileError
  | AppSetupConfigParseError ConfigParseError
  deriving (Eq, Show)

committedSetupDefaults :: [(Text, Text)]
committedSetupDefaults =
  [ ("SETUP_AUTOSTART_DATABASE", "true"),
    ("SETUP_AUTOSTART_JAEGER", "false")
  ]

defaultSetupAutostartConfig :: SetupAutostartConfig
defaultSetupAutostartConfig =
  SetupAutostartConfig
    { setupAutostartDatabase = True,
      setupAutostartJaeger = False
    }

defaultAppSetupConfig :: AppSetupConfig
defaultAppSetupConfig =
  AppSetupConfig
    { setupEnvironmentConfig = defaultAppEnvironmentConfig,
      setupAppConfig = defaultAppConfig,
      setupMigrationDatabaseConfig = Nothing,
      setupAutostartConfig = defaultSetupAutostartConfig
    }

loadAppSetupConfig :: IO (Either AppSetupConfigLoadError AppSetupConfig)
loadAppSetupConfig =
  loadAppSetupConfigWithFiles ".env" ".env.local"

loadAppSetupConfigWithFiles :: FilePath -> FilePath -> IO (Either AppSetupConfigLoadError AppSetupConfig)
loadAppSetupConfigWithFiles committedDefaultsPath localOverridesPath = do
  committedDefaultsResult <- loadOverridesFile committedDefaultsPath
  localOverridesResult <- loadOverridesFile localOverridesPath
  environmentOverrides <- loadEnvironmentOverrides
  pure $ do
    committedDefaults <- committedDefaultsResult
    localOverrides <- localOverridesResult
    first AppSetupConfigParseError $
      parseAppSetupConfig
        (committedEnvDefaults <> committedRuntimeDefaults <> committedSetupDefaults)
        committedDefaults
        (localOverrides <> environmentOverrides)
  where
    loadOverridesFile overridesPath =
      fmap
        (first (AppSetupOverridesFileError overridesPath))
        (loadConfigOverridesFile overridesPath)

loadEnvironmentOverrides :: IO [(Text, Text)]
loadEnvironmentOverrides =
  fmap
    (map (bimap Text.pack Text.pack))
    getEnvironment

parseAppSetupConfig :: [(Text, Text)] -> [(Text, Text)] -> [(Text, Text)] -> Either ConfigParseError AppSetupConfig
parseAppSetupConfig committedDefaults localOverrides environmentOverrides = do
  parsedEnvironmentConfig <- parseAppEnvironmentConfig committedDefaults localOverrides environmentOverrides
  parsedRuntimeConfig <- parseRuntimeAppConfig committedDefaults localOverrides environmentOverrides
  parsedMigrationDatabaseConfig <- parseOptionalMigrationDatabaseConfig committedDefaults localOverrides environmentOverrides
  parsedAutostartConfig <- parseSetupAutostartConfig committedDefaults localOverrides environmentOverrides
  pure
    AppSetupConfig
      { setupEnvironmentConfig = parsedEnvironmentConfig,
        setupAppConfig = parsedRuntimeConfig,
        setupMigrationDatabaseConfig = parsedMigrationDatabaseConfig,
        setupAutostartConfig = parsedAutostartConfig
      }

parseOptionalMigrationDatabaseConfig :: [(Text, Text)] -> [(Text, Text)] -> [(Text, Text)] -> Either ConfigParseError (Maybe DatabaseConfig)
parseOptionalMigrationDatabaseConfig committedDefaults localOverrides environmentOverrides =
  case migrationEntries of
    [] -> Right Nothing
    _ -> Just <$> parseDatabaseSetupConfig migrationEntries
  where
    migrationEntries =
      mapMaybe lookupMigrationValue migrationConfigKeys

    lookupMigrationValue key =
      fmap
        (key,)
        (lookupConfigValue key committedDefaults localOverrides environmentOverrides)

    migrationConfigKeys =
      [ "WEB_API_MIGRATION_DATABASE_HOST",
        "WEB_API_MIGRATION_DATABASE_PORT",
        "WEB_API_MIGRATION_DATABASE_NAME",
        "WEB_API_MIGRATION_DATABASE_USER",
        "WEB_API_MIGRATION_DATABASE_PASSWORD"
      ]

parseSetupAutostartConfig :: [(Text, Text)] -> [(Text, Text)] -> [(Text, Text)] -> Either ConfigParseError SetupAutostartConfig
parseSetupAutostartConfig committedDefaults localOverrides environmentOverrides =
  SetupAutostartConfig
    <$> optionalBoolean "SETUP_AUTOSTART_DATABASE" True
    <*> optionalBoolean "SETUP_AUTOSTART_JAEGER" False
  where
    optionalBoolean key fallback =
      maybe
        (Right fallback)
        (parseBoolean key)
        (lookupConfigValue key committedDefaults localOverrides environmentOverrides)
