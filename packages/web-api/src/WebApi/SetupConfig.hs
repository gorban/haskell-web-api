{-# LANGUAGE OverloadedStrings #-}

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
import Data.Text (Text)
import WebApi.Config
  ( AppConfig,
    committedRuntimeDefaults,
    defaultAppConfig,
    parseRuntimeAppConfig,
  )

data SetupAutostartConfig = SetupAutostartConfig
  { setupAutostartDatabase :: Bool,
    setupAutostartJaeger :: Bool
  }
  deriving (Eq, Show)

data AppSetupConfig = AppSetupConfig
  { setupAppConfig :: AppConfig,
    setupAutostartConfig :: SetupAutostartConfig
  }
  deriving (Eq, Show)

data AppSetupConfigLoadError
  = AppSetupOverridesFileError FilePath ConfigOverridesFileError
  | AppSetupConfigParseError ConfigParseError
  deriving (Eq, Show)

committedSetupDefaults :: [(Text, Text)]
committedSetupDefaults =
  [ ("SETUP_AUTOSTART_DATABASE", "false"),
    ("SETUP_AUTOSTART_JAEGER", "false")
  ]

defaultSetupAutostartConfig :: SetupAutostartConfig
defaultSetupAutostartConfig =
  SetupAutostartConfig
    { setupAutostartDatabase = False,
      setupAutostartJaeger = False
    }

defaultAppSetupConfig :: AppSetupConfig
defaultAppSetupConfig =
  AppSetupConfig
    { setupAppConfig = defaultAppConfig,
      setupAutostartConfig = defaultSetupAutostartConfig
    }

loadAppSetupConfig :: IO (Either AppSetupConfigLoadError AppSetupConfig)
loadAppSetupConfig =
  loadAppSetupConfigWithFiles ".env" ".env.local"

loadAppSetupConfigWithFiles :: FilePath -> FilePath -> IO (Either AppSetupConfigLoadError AppSetupConfig)
loadAppSetupConfigWithFiles committedDefaultsPath localOverridesPath = do
  committedDefaultsResult <- loadOverridesFile committedDefaultsPath
  localOverridesResult <- loadOverridesFile localOverridesPath
  pure $ do
    committedDefaults <- committedDefaultsResult
    localOverrides <- localOverridesResult
    case parseAppSetupConfig
      (committedRuntimeDefaults <> committedSetupDefaults)
      committedDefaults
      localOverrides of
      Left parseError -> Left (AppSetupConfigParseError parseError)
      Right setupConfig -> Right setupConfig
  where
    loadOverridesFile overridesPath =
      fmap
        ( either
            (Left . AppSetupOverridesFileError overridesPath)
            Right
        )
        (loadConfigOverridesFile overridesPath)

parseAppSetupConfig :: [(Text, Text)] -> [(Text, Text)] -> [(Text, Text)] -> Either ConfigParseError AppSetupConfig
parseAppSetupConfig committedDefaults localOverrides environmentOverrides = do
  parsedRuntimeConfig <- parseRuntimeAppConfig committedDefaults localOverrides environmentOverrides
  parsedAutostartConfig <- parseSetupAutostartConfig committedDefaults localOverrides environmentOverrides
  pure
    AppSetupConfig
      { setupAppConfig = parsedRuntimeConfig,
        setupAutostartConfig = parsedAutostartConfig
      }

parseSetupAutostartConfig :: [(Text, Text)] -> [(Text, Text)] -> [(Text, Text)] -> Either ConfigParseError SetupAutostartConfig
parseSetupAutostartConfig committedDefaults localOverrides environmentOverrides =
  SetupAutostartConfig
    <$> optionalBoolean "SETUP_AUTOSTART_DATABASE" False
    <*> optionalBoolean "SETUP_AUTOSTART_JAEGER" False
  where
    optionalBoolean key fallback =
      maybe
        (Right fallback)
        (parseBoolean key)
        (lookupConfigValue key committedDefaults localOverrides environmentOverrides)
