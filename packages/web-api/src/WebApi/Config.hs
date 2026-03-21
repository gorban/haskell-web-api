{-# LANGUAGE DuplicateRecordFields #-}

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
  [ (Text.pack "APP_MODE", Text.pack "development"),
    (Text.pack "DATABASE_HOST", Text.pack "127.0.0.1"),
    (Text.pack "DATABASE_PORT", Text.pack "5432"),
    (Text.pack "DATABASE_NAME", Text.pack "web_api_dev"),
    (Text.pack "DATABASE_USER", Text.pack "web_api"),
    (Text.pack "DATABASE_PASSWORD", Text.pack "web_api")
  ]

committedRuntimeDefaults :: [(Text, Text)]
committedRuntimeDefaults =
  [ (Text.pack "APP_TITLE_PREFIX", Text.pack "web-api"),
    (Text.pack "LISTENER_0_HOST", Text.pack "127.0.0.1"),
    (Text.pack "LISTENER_0_PORT", Text.pack "5001"),
    (Text.pack "LISTENER_0_SCHEME", Text.pack "http")
  ]

defaultAppEnvironmentConfig :: AppEnvironmentConfig
defaultAppEnvironmentConfig =
  AppEnvironmentConfig
    { appMode = Development,
      databaseConfig =
        DatabaseConfig
          { databaseHost = Text.pack "127.0.0.1",
            databasePort = 5432,
            databaseName = Text.pack "web_api_dev",
            databaseUser = Text.pack "web_api",
            databasePassword = Text.pack "web_api"
          }
    }

defaultAppConfig :: AppConfig
defaultAppConfig =
  AppConfig
    { appTitlePrefix = Text.pack "web-api",
      listenerConfigs =
        [ ListenerConfig
            { listenerHost = Text.pack "127.0.0.1",
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
  parsedMode <- parseMode =<< requiredConfigValue (Text.pack "APP_MODE")
  parsedDatabaseHost <- requiredConfigValue (Text.pack "DATABASE_HOST")
  parsedDatabasePort <- parsePort =<< requiredConfigValue (Text.pack "DATABASE_PORT")
  parsedDatabaseName <- requiredConfigValue (Text.pack "DATABASE_NAME")
  parsedDatabaseUser <- requiredConfigValue (Text.pack "DATABASE_USER")
  parsedDatabasePassword <- requiredConfigValue (Text.pack "DATABASE_PASSWORD")
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
    (Left (InvalidConfigValue (Text.pack "APP_MODE") value))
    Right
    ( lookup
        value
        [ (Text.pack "development", Development),
          (Text.pack "test", Test),
          (Text.pack "production", Production)
        ]
    )

parsePort :: Text -> Either ConfigParseError Int
parsePort = parsePositiveInt (Text.pack "DATABASE_PORT")

parseRuntimeAppConfig :: [(Text, Text)] -> [(Text, Text)] -> [(Text, Text)] -> Either ConfigParseError AppConfig
parseRuntimeAppConfig committedDefaults localOverrides environmentOverrides = do
  parsedTitlePrefix <- requiredConfigValue (Text.pack "APP_TITLE_PREFIX")
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
      case declaredIndices (Text.pack "LISTENER_") allConfigEntries of
        [] -> Left (MissingConfigValue (Text.pack "LISTENER_0_HOST"))
        listenerIndices -> traverse parseListenerConfig listenerIndices

    parseListenerConfig listenerIndex = do
      parsedHost <- requiredIndexedConfigValue (Text.pack "LISTENER") listenerIndex (Text.pack "HOST")
      parsedPort <-
        parsePositiveInt (indexedConfigKey (Text.pack "LISTENER") listenerIndex (Text.pack "PORT"))
          =<< requiredIndexedConfigValue (Text.pack "LISTENER") listenerIndex (Text.pack "PORT")
      parsedScheme <-
        parseListenerScheme
          (indexedConfigKey (Text.pack "LISTENER") listenerIndex (Text.pack "SCHEME"))
          =<< requiredIndexedConfigValue (Text.pack "LISTENER") listenerIndex (Text.pack "SCHEME")
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
      tlsSource <- requiredIndexedConfigValue (Text.pack "LISTENER") listenerIndex (Text.pack "TLS_SOURCE")
      parsedCertificateSource <- parseTlsCertificateSource listenerIndex tlsSource
      pure (Just (TlsConfig {certificateSource = parsedCertificateSource}))

    parseTlsCertificateSource listenerIndex tlsSource =
      case Text.unpack tlsSource of
        "manual" ->
          ManualCertificateFiles
            <$> requiredIndexedFilePathValue (Text.pack "LISTENER") listenerIndex (Text.pack "TLS_CERTIFICATE_FILE")
            <*> requiredIndexedFilePathValue (Text.pack "LISTENER") listenerIndex (Text.pack "TLS_PRIVATE_KEY_FILE")
        "acme" -> parseAcmeCertificateSource listenerIndex
        _ ->
          Left
            ( InvalidConfigValue
                (indexedConfigKey (Text.pack "LISTENER") listenerIndex (Text.pack "TLS_SOURCE"))
                tlsSource
            )

    parseAcmeCertificateSource listenerIndex =
      AcmeCertificateSource
        <$> ( AcmeConfig
                <$> requiredIndexedConfigValue (Text.pack "LISTENER") listenerIndex (Text.pack "ACME_DIRECTORY_URL")
                <*> ( parseDelimitedTexts
                        (indexedConfigKey (Text.pack "LISTENER") listenerIndex (Text.pack "ACME_CONTACT_EMAILS"))
                        =<< requiredIndexedConfigValue (Text.pack "LISTENER") listenerIndex (Text.pack "ACME_CONTACT_EMAILS")
                    )
                <*> parseAcmeChallengeBackend listenerIndex
            )

    parseAcmeChallengeBackend listenerIndex = do
      backendValue <- requiredIndexedConfigValue (Text.pack "LISTENER") listenerIndex (Text.pack "ACME_CHALLENGE_BACKEND")
      if backendValue == Text.pack "in-process-http01"
        then Right InProcessHttp01
        else
          if backendValue == Text.pack "certbot-http01"
            then
              CertbotHttp01
                <$> ( CertbotConfig
                        <$> requiredIndexedFilePathValue (Text.pack "LISTENER") listenerIndex (Text.pack "ACME_CERTBOT_EXECUTABLE")
                        <*> pure
                          ( maybe
                              []
                              (parseDelimitedTextsUnsafe (Text.pack ","))
                              (optionalIndexedConfigValue (Text.pack "LISTENER") listenerIndex (Text.pack "ACME_CERTBOT_ARGUMENTS"))
                          )
                    )
            else
              Left
                ( InvalidConfigValue
                    (indexedConfigKey (Text.pack "LISTENER") listenerIndex (Text.pack "ACME_CHALLENGE_BACKEND"))
                    backendValue
                )

    parseStaticAssetsConfig =
      StaticAssetsConfig
        <$> traverse parseStaticAssetRoot (declaredIndices (Text.pack "STATIC_ASSET_ROOT_") allConfigEntries)
        <*> traverse
          (parseNonNegativeInt (Text.pack "STATIC_CACHE_CONTROL_SECONDS"))
          (optionalConfigValue (Text.pack "STATIC_CACHE_CONTROL_SECONDS"))

    parseStaticAssetRoot staticRootIndex =
      StaticAssetRoot
        <$> requiredIndexedConfigValue (Text.pack "STATIC_ASSET_ROOT") staticRootIndex (Text.pack "URL_PREFIX")
        <*> requiredIndexedFilePathValue (Text.pack "STATIC_ASSET_ROOT") staticRootIndex (Text.pack "DIRECTORY")

    parseObservabilityConfig =
      ObservabilityConfig
        <$> parseOptionalOtlpExporter (Text.pack "OTLP_TRACING")
        <*> parseOptionalOtlpExporter (Text.pack "OTLP_METRICS")

    parseOptionalOtlpExporter exporterPrefix =
      case optionalConfigValue (exporterPrefix <> Text.pack "_ENDPOINT") of
        Just endpoint ->
          Right
            ( Just
                OtlpExporter
                  { otlpEndpoint = endpoint,
                    otlpHeaders =
                      maybe
                        []
                        (parseHeadersUnsafe . Text.strip)
                        (optionalConfigValue (exporterPrefix <> Text.pack "_HEADERS"))
                  }
            )
        Nothing ->
          case optionalConfigValue (exporterPrefix <> Text.pack "_HEADERS") of
            Just _ -> Left (MissingConfigValue (exporterPrefix <> Text.pack "_ENDPOINT"))
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
        [ (Text.pack "http", Http),
          (Text.pack "https", Https)
        ]
    )
