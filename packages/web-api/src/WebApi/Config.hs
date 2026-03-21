{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module WebApi.Config
  ( AcmeChallengeBackend (..),
    AcmeConfig (..),
    AppConfig (..),
    AppEnvironmentConfig (..),
    AppMode (..),
    CertbotConfig (..),
    ConfigParseError (..),
    DatabaseConfig (..),
    ListenerConfig (..),
    ListenerScheme (..),
    ObservabilityConfig (..),
    OtlpExporter (..),
    StaticAssetsConfig (..),
    StaticAssetRoot (..),
    TlsCertificateSource (..),
    TlsConfig (..),
    committedEnvDefaults,
    committedRuntimeDefaults,
    defaultAppConfig,
    defaultAppEnvironmentConfig,
    parseAppEnvironmentConfig,
    parseRuntimeAppConfig,
  )
where

import Core.Config
  ( ConfigParseError (..),
    declaredIndices,
    indexedConfigKey,
    lookupConfigValue,
    parseDelimitedTexts,
    parseDelimitedTextsUnsafe,
    parseHeadersUnsafe,
    parseNonNegativeInt,
    parsePositiveInt,
  )
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb
  ( AcmeChallengeBackend (..),
    AcmeConfig (..),
    CertbotConfig (..),
    HasServerConfig (..),
    ListenerConfig (..),
    ListenerScheme (..),
    ObservabilityConfig (..),
    OtlpExporter (..),
    ServerConfig (..),
    StaticAssetRoot (..),
    StaticAssetsConfig (..),
    TlsCertificateSource (..),
    TlsConfig (..),
  )

data AppMode
  = Development
  | Test
  | Production
  deriving (Eq, Show)

data DatabaseConfig = DatabaseConfig
  { databaseHost :: Text,
    databasePort :: Int,
    databaseName :: Text,
    databaseUser :: Text,
    databasePassword :: Text
  }
  deriving (Eq, Show)

data AppEnvironmentConfig = AppEnvironmentConfig
  { appMode :: AppMode,
    databaseConfig :: DatabaseConfig
  }
  deriving (Eq, Show)

data AppConfig = AppConfig
  { appTitlePrefix :: Text,
    listenerConfigs :: [ListenerConfig],
    staticAssets :: StaticAssetsConfig,
    observability :: ObservabilityConfig
  }
  deriving (Eq, Show)

instance HasServerConfig AppConfig where
  toServerConfig AppConfig {listenerConfigs = appListeners, staticAssets = appStaticAssets, observability = appObservability} =
    ServerConfig
      { listenerConfigs = appListeners,
        staticAssets = appStaticAssets,
        observability = appObservability
      }

committedEnvDefaults :: [(Text, Text)]
committedEnvDefaults =
  [ ("APP_MODE", "development"),
    ("DATABASE_HOST", "127.0.0.1"),
    ("DATABASE_PORT", "5432"),
    ("DATABASE_NAME", "web_api_dev"),
    ("DATABASE_USER", "web_api"),
    ("DATABASE_PASSWORD", "web_api")
  ]

committedRuntimeDefaults :: [(Text, Text)]
committedRuntimeDefaults =
  [ ("APP_TITLE_PREFIX", "web-api"),
    ("LISTENER_0_HOST", "127.0.0.1"),
    ("LISTENER_0_PORT", "5001"),
    ("LISTENER_0_SCHEME", "http")
  ]

defaultAppEnvironmentConfig :: AppEnvironmentConfig
defaultAppEnvironmentConfig =
  AppEnvironmentConfig
    { appMode = Development,
      databaseConfig =
        DatabaseConfig
          { databaseHost = "127.0.0.1",
            databasePort = 5432,
            databaseName = "web_api_dev",
            databaseUser = "web_api",
            databasePassword = "web_api"
          }
    }

defaultAppConfig :: AppConfig
defaultAppConfig =
  AppConfig
    { appTitlePrefix = "web-api",
      listenerConfigs =
        [ ListenerConfig
            { listenerHost = "127.0.0.1",
              listenerPort = 5001,
              listenerScheme = Http,
              listenerTls = Nothing
            }
        ],
      staticAssets =
        StaticAssetsConfig
          { staticAssetRoots = [],
            staticCacheControlSeconds = Nothing
          },
      observability =
        ObservabilityConfig
          { tracingExporter = Nothing,
            metricsExporter = Nothing
          }
    }

parseAppEnvironmentConfig :: [(Text, Text)] -> [(Text, Text)] -> [(Text, Text)] -> Either ConfigParseError AppEnvironmentConfig
parseAppEnvironmentConfig committedDefaults localOverrides environmentOverrides = do
  parsedMode <- parseMode =<< requiredConfigValue "APP_MODE"
  parsedDatabaseHost <- requiredConfigValue "DATABASE_HOST"
  parsedDatabasePort <- parsePort =<< requiredConfigValue "DATABASE_PORT"
  parsedDatabaseName <- requiredConfigValue "DATABASE_NAME"
  parsedDatabaseUser <- requiredConfigValue "DATABASE_USER"
  parsedDatabasePassword <- requiredConfigValue "DATABASE_PASSWORD"
  pure
    AppEnvironmentConfig
      { appMode = parsedMode,
        databaseConfig =
          DatabaseConfig
            { databaseHost = parsedDatabaseHost,
              databasePort = parsedDatabasePort,
              databaseName = parsedDatabaseName,
              databaseUser = parsedDatabaseUser,
              databasePassword = parsedDatabasePassword
            }
      }
  where
    requiredConfigValue key =
      case lookupConfigValue key committedDefaults localOverrides environmentOverrides of
        Just value -> Right value
        Nothing -> Left (MissingConfigValue key)

parseMode :: Text -> Either ConfigParseError AppMode
parseMode value =
  maybe
    (Left (InvalidConfigValue "APP_MODE" value))
    Right
    ( lookup
        value
        [ ("development", Development),
          ("test", Test),
          ("production", Production)
        ]
    )

parsePort :: Text -> Either ConfigParseError Int
parsePort = parsePositiveInt "DATABASE_PORT"

parseRuntimeAppConfig :: [(Text, Text)] -> [(Text, Text)] -> [(Text, Text)] -> Either ConfigParseError AppConfig
parseRuntimeAppConfig committedDefaults localOverrides environmentOverrides = do
  parsedTitlePrefix <- requiredConfigValue "APP_TITLE_PREFIX"
  parsedListeners <- parseListenerConfigs
  parsedStaticAssets <- parseStaticAssetsConfig
  parsedObservability <- parseObservabilityConfig
  pure
    AppConfig
      { appTitlePrefix = parsedTitlePrefix,
        listenerConfigs = parsedListeners,
        staticAssets = parsedStaticAssets,
        observability = parsedObservability
      }
  where
    allConfigEntries = committedDefaults <> localOverrides <> environmentOverrides

    requiredConfigValue key =
      case lookupConfigValue key committedDefaults localOverrides environmentOverrides of
        Just value -> Right value
        Nothing -> Left (MissingConfigValue key)

    optionalConfigValue key =
      lookupConfigValue key committedDefaults localOverrides environmentOverrides

    parseListenerConfigs =
      case declaredIndices "LISTENER_" allConfigEntries of
        [] -> Left (MissingConfigValue "LISTENER_0_HOST")
        listenerIndices -> traverse parseListenerConfig listenerIndices

    parseListenerConfig listenerIndex = do
      parsedHost <- requiredIndexedConfigValue "LISTENER" listenerIndex "HOST"
      parsedPort <-
        parsePositiveInt (indexedConfigKey "LISTENER" listenerIndex "PORT")
          =<< requiredIndexedConfigValue "LISTENER" listenerIndex "PORT"
      parsedScheme <-
        parseListenerScheme
          (indexedConfigKey "LISTENER" listenerIndex "SCHEME")
          =<< requiredIndexedConfigValue "LISTENER" listenerIndex "SCHEME"
      parsedTls <- parseListenerTlsConfig listenerIndex parsedScheme
      pure
        ListenerConfig
          { listenerHost = parsedHost,
            listenerPort = parsedPort,
            listenerScheme = parsedScheme,
            listenerTls = parsedTls
          }

    parseListenerTlsConfig _ Http = Right Nothing
    parseListenerTlsConfig listenerIndex Https = do
      tlsSource <- requiredIndexedConfigValue "LISTENER" listenerIndex "TLS_SOURCE"
      parsedCertificateSource <- parseTlsCertificateSource listenerIndex tlsSource
      pure (Just (TlsConfig {certificateSource = parsedCertificateSource}))

    parseTlsCertificateSource listenerIndex tlsSource =
      case Text.unpack tlsSource of
        "manual" ->
          ManualCertificateFiles
            <$> requiredIndexedFilePathValue "LISTENER" listenerIndex "TLS_CERTIFICATE_FILE"
            <*> requiredIndexedFilePathValue "LISTENER" listenerIndex "TLS_PRIVATE_KEY_FILE"
        "acme" -> parseAcmeCertificateSource listenerIndex
        _ ->
          Left
            ( InvalidConfigValue
                (indexedConfigKey "LISTENER" listenerIndex "TLS_SOURCE")
                tlsSource
            )

    parseAcmeCertificateSource listenerIndex =
      AcmeCertificateSource
        <$> ( AcmeConfig
                <$> requiredIndexedConfigValue "LISTENER" listenerIndex "ACME_DIRECTORY_URL"
                <*> ( parseDelimitedTexts
                        (indexedConfigKey "LISTENER" listenerIndex "ACME_CONTACT_EMAILS")
                        =<< requiredIndexedConfigValue "LISTENER" listenerIndex "ACME_CONTACT_EMAILS"
                    )
                <*> parseAcmeChallengeBackend listenerIndex
            )

    parseAcmeChallengeBackend listenerIndex = do
      backendValue <- requiredIndexedConfigValue "LISTENER" listenerIndex "ACME_CHALLENGE_BACKEND"
      if backendValue == "in-process-http01"
        then Right InProcessHttp01
        else
          if backendValue == "certbot-http01"
            then
              CertbotHttp01
                <$> ( CertbotConfig
                        <$> requiredIndexedFilePathValue "LISTENER" listenerIndex "ACME_CERTBOT_EXECUTABLE"
                        <*> pure
                          ( maybe
                              []
                              (parseDelimitedTextsUnsafe ",")
                              (optionalIndexedConfigValue "LISTENER" listenerIndex "ACME_CERTBOT_ARGUMENTS")
                          )
                    )
            else
              Left
                ( InvalidConfigValue
                    (indexedConfigKey "LISTENER" listenerIndex "ACME_CHALLENGE_BACKEND")
                    backendValue
                )

    parseStaticAssetsConfig =
      StaticAssetsConfig
        <$> traverse parseStaticAssetRoot (declaredIndices "STATIC_ASSET_ROOT_" allConfigEntries)
        <*> traverse
          (parseNonNegativeInt "STATIC_CACHE_CONTROL_SECONDS")
          (optionalConfigValue "STATIC_CACHE_CONTROL_SECONDS")

    parseStaticAssetRoot staticRootIndex =
      StaticAssetRoot
        <$> requiredIndexedConfigValue "STATIC_ASSET_ROOT" staticRootIndex "URL_PREFIX"
        <*> requiredIndexedFilePathValue "STATIC_ASSET_ROOT" staticRootIndex "DIRECTORY"

    parseObservabilityConfig =
      ObservabilityConfig
        <$> parseOptionalOtlpExporter "OTLP_TRACING"
        <*> parseOptionalOtlpExporter "OTLP_METRICS"

    parseOptionalOtlpExporter exporterPrefix =
      case optionalConfigValue (exporterPrefix <> "_ENDPOINT") of
        Just endpoint ->
          Right
            ( Just
                OtlpExporter
                  { otlpEndpoint = endpoint,
                    otlpHeaders =
                      maybe
                        []
                        (parseHeadersUnsafe . Text.strip)
                        (optionalConfigValue (exporterPrefix <> "_HEADERS"))
                  }
            )
        Nothing ->
          case optionalConfigValue (exporterPrefix <> "_HEADERS") of
            Just _ -> Left (MissingConfigValue (exporterPrefix <> "_ENDPOINT"))
            Nothing -> Right Nothing

    requiredIndexedConfigValue prefix configIndex suffix =
      requiredConfigValue (indexedConfigKey prefix configIndex suffix)

    optionalIndexedConfigValue prefix configIndex suffix =
      optionalConfigValue (indexedConfigKey prefix configIndex suffix)

    requiredIndexedFilePathValue prefix configIndex suffix =
      Text.unpack <$> requiredIndexedConfigValue prefix configIndex suffix

parseListenerScheme :: Text -> Text -> Either ConfigParseError ListenerScheme
parseListenerScheme key value =
  maybe
    (Left (InvalidConfigValue key value))
    Right
    ( lookup
        value
        [ ("http", Http),
          ("https", Https)
        ]
    )
