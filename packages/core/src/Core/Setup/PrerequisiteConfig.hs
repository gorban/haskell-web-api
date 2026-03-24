{-# LANGUAGE OverloadedStrings #-}

module Core.Setup.PrerequisiteConfig
  ( SetupPrerequisiteConfig (..),
    SetupPrerequisiteConfigLoadError (..),
    committedPrerequisiteDefaults,
    defaultSetupPrerequisiteConfig,
    loadSetupPrerequisiteConfig,
    loadSetupPrerequisiteConfigWithFiles,
    parseSetupPrerequisiteConfig,
  )
where

import Core.Config
  ( ConfigOverridesFileError (..),
    ConfigParseError (..),
    loadConfigOverridesFile,
    lookupConfigValue,
    parseBoolean,
    parsePositiveInt,
  )
import Core.Setup.Prerequisite (TcpEndpoint (..))
import Data.Text (Text)

data SetupPrerequisiteConfig = SetupPrerequisiteConfig
  { setupDatabaseEndpoint :: TcpEndpoint,
    setupDatabaseName :: Text,
    setupDatabaseUser :: Text,
    setupDatabasePassword :: Text,
    setupTracingEndpoint :: Maybe Text,
    setupAutostartDatabase :: Bool,
    setupAutostartJaeger :: Bool
  }
  deriving (Eq, Show)

data SetupPrerequisiteConfigLoadError
  = SetupPrerequisiteOverridesFileError FilePath ConfigOverridesFileError
  | SetupPrerequisiteConfigParseError ConfigParseError
  deriving (Eq, Show)

committedPrerequisiteDefaults :: [(Text, Text)]
committedPrerequisiteDefaults =
  [ ("DATABASE_HOST", "127.0.0.1"),
    ("DATABASE_PORT", "5432"),
    ("DATABASE_NAME", "web_api_dev"),
    ("DATABASE_USER", "web_api"),
    ("DATABASE_PASSWORD", "web_api"),
    ("SETUP_AUTOSTART_DATABASE", "true"),
    ("SETUP_AUTOSTART_JAEGER", "false")
  ]

defaultSetupPrerequisiteConfig :: SetupPrerequisiteConfig
defaultSetupPrerequisiteConfig =
  SetupPrerequisiteConfig
    { setupDatabaseEndpoint =
        TcpEndpoint
          { tcpEndpointHost = "127.0.0.1",
            tcpEndpointPort = 5432
          },
      setupDatabaseName = "web_api_dev",
      setupDatabaseUser = "web_api",
      setupDatabasePassword = "web_api",
      setupTracingEndpoint = Nothing,
      setupAutostartDatabase = True,
      setupAutostartJaeger = False
    }

loadSetupPrerequisiteConfig :: IO (Either SetupPrerequisiteConfigLoadError SetupPrerequisiteConfig)
loadSetupPrerequisiteConfig =
  loadSetupPrerequisiteConfigWithFiles ".env" ".env.local"

loadSetupPrerequisiteConfigWithFiles :: FilePath -> FilePath -> IO (Either SetupPrerequisiteConfigLoadError SetupPrerequisiteConfig)
loadSetupPrerequisiteConfigWithFiles committedDefaultsPath localOverridesPath = do
  committedDefaultsResult <- loadOverridesFile committedDefaultsPath
  localOverridesResult <- loadOverridesFile localOverridesPath
  pure $ do
    committedDefaults <- committedDefaultsResult
    localOverrides <- localOverridesResult
    case parseSetupPrerequisiteConfig committedPrerequisiteDefaults committedDefaults localOverrides of
      Left parseError -> Left (SetupPrerequisiteConfigParseError parseError)
      Right setupConfig -> Right setupConfig
  where
    loadOverridesFile overridesPath =
      fmap
        ( either
            (Left . SetupPrerequisiteOverridesFileError overridesPath)
            Right
        )
        (loadConfigOverridesFile overridesPath)

parseSetupPrerequisiteConfig :: [(Text, Text)] -> [(Text, Text)] -> [(Text, Text)] -> Either ConfigParseError SetupPrerequisiteConfig
parseSetupPrerequisiteConfig committedDefaults localOverrides environmentOverrides =
  SetupPrerequisiteConfig
    <$> parseDatabaseEndpoint
    <*> requiredConfigValue "DATABASE_NAME"
    <*> requiredConfigValue "DATABASE_USER"
    <*> requiredConfigValue "DATABASE_PASSWORD"
    <*> pure (lookupOptionalValue "OTLP_TRACING_ENDPOINT")
    <*> optionalBoolean "SETUP_AUTOSTART_DATABASE" True
    <*> optionalBoolean "SETUP_AUTOSTART_JAEGER" False
  where
    parseDatabaseEndpoint =
      TcpEndpoint
        <$> requiredConfigValue "DATABASE_HOST"
        <*> (parsePositiveInt "DATABASE_PORT" =<< requiredConfigValue "DATABASE_PORT")

    requiredConfigValue key =
      case lookupOptionalValue key of
        Just value -> Right value
        Nothing -> Left (MissingConfigValue key)

    lookupOptionalValue key =
      lookupConfigValue key committedDefaults localOverrides environmentOverrides

    optionalBoolean key fallback =
      maybe
        (Right fallback)
        (parseBoolean key)
        (lookupOptionalValue key)
