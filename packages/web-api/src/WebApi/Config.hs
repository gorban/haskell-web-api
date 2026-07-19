{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module WebApi.Config
  ( AcmeConfig (..),
    AppConfig (..),
    AppEnvironmentConfig (..),
    AppEnvironmentConfigLoadError (..),
    AppStartupConfig (..),
    AppStartupConfigLoadError (..),
    AppMode (..),
    CertbotConfig (..),
    ConfigOverridesFileError (..),
    ConfigParseError (..),
    CorsPolicyConfig (..),
    DatabaseConfig (..),
    ListenerConfig (..),
    ListenerScheme (..),
    ObservabilityConfig (..),
    OtlpExporter (..),
    RequestPolicyConfig (..),
    ResponseSecurityHeadersConfig (..),
    SmtpDeliveryConfig (..),
    StaticAssetsConfig (..),
    StaticAssetRoot (..),
    StrictTransportSecurityConfig (..),
    TlsCertificateSource (..),
    TlsStartupMode (..),
    TlsConfig (..),
    committedEnvDefaults,
    committedRuntimeDefaults,
    defaultAppConfig,
    defaultAppEnvironmentConfig,
    defaultAppStartupConfig,
    defaultContentSecurityPolicy,
    defaultCorsPolicyConfig,
    defaultResponseSecurityHeadersConfig,
    defaultStaticAssetContentTypes,
    loadAppEnvironmentConfig,
    loadAppEnvironmentConfigWithFiles,
    loadAppStartupConfig,
    loadAppStartupConfigWithFiles,
    parseAppEnvironmentConfig,
    parseAppStartupConfig,
    parseRuntimeAppConfig,
  )
where

import Control.Applicative ((<|>))
import Core.Config
  ( ConfigOverridesFileError (..),
    ConfigParseError (..),
    declaredIndices,
    indexedConfigKey,
    loadConfigOverridesFile,
    lookupConfigValue,
    parseBoolean,
    parseDelimitedTexts,
    parseDelimitedTextsUnsafe,
    parseHeadersUnsafe,
    parseNonNegativeInt,
    parsePositiveInt,
  )
import Data.Bifunctor (bimap)
import Data.List (nub)
import Data.Maybe (fromJust, fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb
  ( AcmeConfig (..),
    CertbotConfig (..),
    CorsPolicyConfig (..),
    HasServerConfig (..),
    ListenerConfig (..),
    ListenerScheme (..),
    ObservabilityConfig (..),
    OtlpExporter (..),
    RequestPolicyConfig (..),
    ResponseSecurityHeadersConfig (..),
    ServerConfig (..),
    StaticAssetRoot (..),
    StaticAssetsConfig (..),
    StrictTransportSecurityConfig (..),
    TlsCertificateSource (..),
    TlsConfig (..),
    TlsStartupMode (..),
    certbotOptionValues,
    defaultContentSecurityPolicy,
    defaultCorsPolicyConfig,
    defaultResponseSecurityHeadersConfig,
    defaultStaticAssetContentTypes,
    firstCertbotDomain,
  )
import HarchWeb.Secret (SecretEncryptionKey, mkSecretEncryptionKey)
import System.Environment (getEnvironment)

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
    databaseConfig :: DatabaseConfig,
    smtpDeliveryConfig :: SmtpDeliveryConfig,
    publicBaseUrl :: Text,
    totpEncryptionKey :: SecretEncryptionKey
  }
  deriving (Eq)

instance Show AppEnvironmentConfig where
  show AppEnvironmentConfig {appMode, databaseConfig, smtpDeliveryConfig, publicBaseUrl} =
    "AppEnvironmentConfig {appMode = "
      <> show appMode
      <> ", databaseConfig = "
      <> show databaseConfig
      <> ", smtpDeliveryConfig = "
      <> renderSmtpDeliveryConfig smtpDeliveryConfig
      <> ", publicBaseUrl = "
      <> show publicBaseUrl
      <> ", totpEncryptionKey = <redacted>"
      <> "}"

data SmtpDeliveryConfig = SmtpDeliveryConfig
  { smtpDeliveryHost :: Text,
    smtpDeliveryPort :: Int,
    smtpDeliveryHeloName :: Text,
    smtpDeliverySender :: Text,
    smtpDeliveryUsername :: Text,
    smtpDeliveryPassword :: Text
  }
  deriving (Eq)

renderSmtpDeliveryConfig :: SmtpDeliveryConfig -> String
renderSmtpDeliveryConfig SmtpDeliveryConfig {smtpDeliveryHost, smtpDeliveryPort, smtpDeliveryHeloName, smtpDeliverySender, smtpDeliveryUsername, smtpDeliveryPassword} =
  "SmtpDeliveryConfig {smtpDeliveryHost = "
    <> show smtpDeliveryHost
    <> ", smtpDeliveryPort = "
    <> show smtpDeliveryPort
    <> ", smtpDeliveryHeloName = "
    <> show smtpDeliveryHeloName
    <> ", smtpDeliverySender = "
    <> show smtpDeliverySender
    <> ", smtpDeliveryUsername = "
    <> show smtpDeliveryUsername
    <> ", smtpDeliveryPassword = "
    <> show smtpDeliveryPassword
    <> "}"

data AppEnvironmentConfigLoadError
  = AppEnvironmentOverridesFileError FilePath ConfigOverridesFileError
  | AppEnvironmentConfigParseError ConfigParseError
  deriving (Eq, Show)

data AppConfig = AppConfig
  { appTitlePrefix :: Text,
    listenerConfigs :: [ListenerConfig],
    staticAssets :: StaticAssetsConfig,
    requestPolicy :: RequestPolicyConfig,
    observability :: ObservabilityConfig
  }
  deriving (Eq, Show)

data AppStartupConfig = AppStartupConfig
  { startupEnvironmentConfig :: AppEnvironmentConfig,
    startupAppConfig :: AppConfig
  }
  deriving (Eq, Show)

data AppStartupConfigLoadError
  = AppStartupOverridesFileError FilePath ConfigOverridesFileError
  | AppStartupConfigParseError ConfigParseError
  deriving (Eq, Show)

instance HasServerConfig AppConfig where
  toServerConfig AppConfig {listenerConfigs = appListeners, staticAssets = appStaticAssets, requestPolicy = appRequestPolicy, observability = appObservability} =
    ServerConfig
      { listenerConfigs = appListeners,
        staticAssets = appStaticAssets,
        requestPolicy = appRequestPolicy,
        observability = appObservability
      }

committedEnvDefaults :: [(Text, Text)]
committedEnvDefaults =
  [ ("APP_MODE", "development"),
    ("DATABASE_HOST", "127.0.0.1"),
    ("DATABASE_PORT", "5432"),
    ("DATABASE_NAME", "web_api_dev"),
    ("DATABASE_USER", "web_api_runtime"),
    ("DATABASE_PASSWORD", "web_api"),
    ("SMTP_HOST", "127.0.0.1"),
    ("SMTP_PORT", "5025"),
    ("SMTP_HELO_NAME", "localhost"),
    ("SMTP_USER", "test@localhost"),
    ("SMTP_PASSWORD", "password"),
    ("EMAIL_FROM", "noreply@localhost"),
    ("PUBLIC_BASE_URL", "http://127.0.0.1:5001"),
    ("TOTP_ENCRYPTION_KEY", "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
  ]

committedRuntimeDefaults :: [(Text, Text)]
committedRuntimeDefaults =
  [ ("APP_TITLE_PREFIX", "web-api"),
    ("LISTENER_0_HOST", "127.0.0.1"),
    ("LISTENER_0_PORT", "5001"),
    ("LISTENER_0_SCHEME", "http")
  ]

defaultLocalTracingEndpoint :: Text
defaultLocalTracingEndpoint = "http://127.0.0.1:4318/v1/traces"

defaultAcmeDirectoryUrl :: Text
defaultAcmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory"

defaultCertbotExecutable :: FilePath
defaultCertbotExecutable = "certbot"

defaultCertificateDirectoryRoot :: FilePath
defaultCertificateDirectoryRoot = ".tls"

defaultAppEnvironmentConfig :: AppEnvironmentConfig
defaultAppEnvironmentConfig =
  AppEnvironmentConfig
    { appMode = Development,
      databaseConfig =
        DatabaseConfig
          { databaseHost = "127.0.0.1",
            databasePort = 5432,
            databaseName = "web_api_dev",
            databaseUser = "web_api_runtime",
            databasePassword = "web_api"
          },
      smtpDeliveryConfig =
        SmtpDeliveryConfig
          { smtpDeliveryHost = "127.0.0.1",
            smtpDeliveryPort = 5025,
            smtpDeliveryHeloName = "localhost",
            smtpDeliverySender = "noreply@localhost",
            smtpDeliveryUsername = "test@localhost",
            smtpDeliveryPassword = "password"
          },
      publicBaseUrl = "http://127.0.0.1:5001",
      totpEncryptionKey = defaultTotpEncryptionKey
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
              listenerTls = Nothing,
              listenerAcme = Nothing
            }
        ],
      staticAssets =
        StaticAssetsConfig
          { staticAssetRoots = [],
            staticAssetContentTypes = defaultStaticAssetContentTypes,
            staticCacheControlSeconds = Nothing
          },
      requestPolicy =
        RequestPolicyConfig
          { redirectHttpToHttps = False,
            httpsRedirectPort = Nothing,
            strictTransportSecurity = Nothing,
            trustForwardedHeaders = False,
            corsPolicy = defaultCorsPolicyConfig,
            responseSecurityHeaders = defaultResponseSecurityHeadersConfig
          },
      observability =
        ObservabilityConfig
          { tracingExporter = Nothing,
            metricsExporter = Nothing
          }
    }

defaultAppStartupConfig :: AppStartupConfig
defaultAppStartupConfig =
  AppStartupConfig
    { startupEnvironmentConfig = defaultAppEnvironmentConfig,
      startupAppConfig = defaultAppConfig
    }

loadAppEnvironmentConfig :: IO (Either AppEnvironmentConfigLoadError AppEnvironmentConfig)
loadAppEnvironmentConfig =
  loadAppEnvironmentConfigWithFiles ".env" ".env.local"

loadAppEnvironmentConfigWithFiles :: FilePath -> FilePath -> IO (Either AppEnvironmentConfigLoadError AppEnvironmentConfig)
loadAppEnvironmentConfigWithFiles committedDefaultsPath localOverridesPath = do
  committedDefaultsResult <- loadOverridesFile committedDefaultsPath
  localOverridesResult <- loadOverridesFile localOverridesPath
  environmentOverrides <- loadEnvironmentOverrides
  pure $ do
    committedDefaults <- committedDefaultsResult
    localOverrides <- localOverridesResult
    case parseAppEnvironmentConfig committedEnvDefaults committedDefaults (localOverrides <> environmentOverrides) of
      Left parseError -> Left (AppEnvironmentConfigParseError parseError)
      Right environmentConfig -> Right environmentConfig
  where
    loadOverridesFile overridesPath =
      fmap
        ( either
            (Left . AppEnvironmentOverridesFileError overridesPath)
            Right
        )
        (loadConfigOverridesFile overridesPath)

loadEnvironmentOverrides :: IO [(Text, Text)]
loadEnvironmentOverrides =
  fmap
    (map (bimap Text.pack Text.pack))
    getEnvironment

loadAppStartupConfig :: IO (Either AppStartupConfigLoadError AppStartupConfig)
loadAppStartupConfig =
  loadAppStartupConfigWithFiles ".env" ".env.local"

loadAppStartupConfigWithFiles :: FilePath -> FilePath -> IO (Either AppStartupConfigLoadError AppStartupConfig)
loadAppStartupConfigWithFiles committedDefaultsPath localOverridesPath = do
  committedDefaultsResult <- loadOverridesFile committedDefaultsPath
  localOverridesResult <- loadOverridesFile localOverridesPath
  environmentOverrides <- loadEnvironmentOverrides
  pure $ do
    committedDefaults <- committedDefaultsResult
    localOverrides <- localOverridesResult
    case parseAppStartupConfig (committedEnvDefaults <> committedRuntimeDefaults) committedDefaults (localOverrides <> environmentOverrides) of
      Left parseError -> Left (AppStartupConfigParseError parseError)
      Right startupConfig -> Right startupConfig
  where
    loadOverridesFile overridesPath =
      fmap
        ( either
            (Left . AppStartupOverridesFileError overridesPath)
            Right
        )
        (loadConfigOverridesFile overridesPath)

parseAppEnvironmentConfig :: [(Text, Text)] -> [(Text, Text)] -> [(Text, Text)] -> Either ConfigParseError AppEnvironmentConfig
parseAppEnvironmentConfig committedDefaults localOverrides environmentOverrides = do
  parsedMode <- parseMode =<< requiredConfigValue "APP_MODE"
  parsedDatabaseHost <- requiredConfigValue "DATABASE_HOST"
  parsedDatabasePort <- parsePort =<< requiredConfigValue "DATABASE_PORT"
  parsedDatabaseName <- requiredConfigValue "DATABASE_NAME"
  parsedDatabaseUser <- requiredConfigValue "DATABASE_USER"
  parsedDatabasePassword <- requiredConfigValue "DATABASE_PASSWORD"
  parsedSmtpHost <- requiredConfigValue "SMTP_HOST"
  parsedSmtpPort <- parseSmtpPort =<< requiredConfigValue "SMTP_PORT"
  parsedSmtpHeloName <- requiredConfigValue "SMTP_HELO_NAME"
  parsedSmtpSender <- requiredConfigValue "EMAIL_FROM"
  parsedSmtpUsername <- requiredConfigValue "SMTP_USER"
  parsedSmtpPassword <- requiredConfigValue "SMTP_PASSWORD"
  parsedPublicBaseUrl <- requiredConfigValue "PUBLIC_BASE_URL"
  parsedTotpEncryptionKey <- parseTotpEncryptionKey =<< requiredConfigValue "TOTP_ENCRYPTION_KEY"
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
            },
        smtpDeliveryConfig =
          SmtpDeliveryConfig
            { smtpDeliveryHost = parsedSmtpHost,
              smtpDeliveryPort = parsedSmtpPort,
              smtpDeliveryHeloName = parsedSmtpHeloName,
              smtpDeliverySender = parsedSmtpSender,
              smtpDeliveryUsername = parsedSmtpUsername,
              smtpDeliveryPassword = parsedSmtpPassword
            },
        publicBaseUrl = parsedPublicBaseUrl,
        totpEncryptionKey = parsedTotpEncryptionKey
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

parseSmtpPort :: Text -> Either ConfigParseError Int
parseSmtpPort value = do
  parsedPort <- parsePositiveInt "SMTP_PORT" value
  if parsedPort <= 65535
    then Right parsedPort
    else Left (InvalidConfigValue "SMTP_PORT" value)

parseTotpEncryptionKey :: Text -> Either ConfigParseError SecretEncryptionKey
parseTotpEncryptionKey value =
  maybe
    (Left (InvalidConfigValue "TOTP_ENCRYPTION_KEY" value))
    Right
    (mkSecretEncryptionKey value)

defaultTotpEncryptionKey :: SecretEncryptionKey
defaultTotpEncryptionKey = fromJust (mkSecretEncryptionKey "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")

parseAppStartupConfig :: [(Text, Text)] -> [(Text, Text)] -> [(Text, Text)] -> Either ConfigParseError AppStartupConfig
parseAppStartupConfig committedDefaults localOverrides environmentOverrides =
  AppStartupConfig
    <$> parseAppEnvironmentConfig committedDefaults localOverrides environmentOverrides
    <*> parseRuntimeAppConfig committedDefaults localOverrides environmentOverrides

parseRuntimeAppConfig :: [(Text, Text)] -> [(Text, Text)] -> [(Text, Text)] -> Either ConfigParseError AppConfig
parseRuntimeAppConfig committedDefaults localOverrides environmentOverrides = do
  parsedTitlePrefix <- requiredConfigValue "APP_TITLE_PREFIX"
  parsedListeners <- parseListenerConfigs
  parsedStaticAssets <- parseStaticAssetsConfig
  parsedRequestPolicy <- parseRequestPolicyConfig parsedListeners
  parsedObservability <- parseObservabilityConfig
  pure
    AppConfig
      { appTitlePrefix = parsedTitlePrefix,
        listenerConfigs = parsedListeners,
        staticAssets = parsedStaticAssets,
        requestPolicy = parsedRequestPolicy,
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
      parsedAcme <- parseListenerAcmeConfig listenerIndex parsedScheme parsedPort
      parsedTls <- parseListenerTlsConfig listenerIndex parsedScheme
      pure
        ListenerConfig
          { listenerHost = parsedHost,
            listenerPort = parsedPort,
            listenerScheme = parsedScheme,
            listenerTls = parsedTls,
            listenerAcme = parsedAcme
          }

    parseListenerAcmeConfig listenerIndex Http parsedPort =
      if listenerHasAcmeConfig listenerIndex
        then Just <$> parseAcmeConfig listenerIndex parsedPort
        else Right Nothing
    parseListenerAcmeConfig _ Https _ =
      Right Nothing

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
        "shared" ->
          parseSharedCertificateSource listenerIndex (AwaitCertificateFiles <$> parseSharedTlsWaitTimeout listenerIndex)
        "shared-wait" ->
          parseSharedCertificateSource listenerIndex (AwaitCertificateFiles <$> parseSharedTlsWaitTimeout listenerIndex)
        "shared-fail-fast" ->
          parseSharedCertificateSource listenerIndex (parseSharedTlsFailFastMode listenerIndex)
        "acme" -> parseAcmeCertificateSource listenerIndex
        _ ->
          Left
            ( InvalidConfigValue
                (indexedConfigKey "LISTENER" listenerIndex "TLS_SOURCE")
                tlsSource
            )

    parseSharedCertificateSource listenerIndex parseStartupMode =
      SharedCertificateFiles
        <$> resolveSharedCertificateDirectory listenerIndex
        <*> parseStartupMode

    parseSharedTlsWaitTimeout listenerIndex =
      traverse
        (parseNonNegativeInt (indexedConfigKey "LISTENER" listenerIndex "TLS_SHARED_WAIT_SECONDS"))
        (optionalIndexedConfigValue "LISTENER" listenerIndex "TLS_SHARED_WAIT_SECONDS")

    parseSharedTlsFailFastMode listenerIndex =
      case optionalIndexedConfigValue "LISTENER" listenerIndex "TLS_SHARED_WAIT_SECONDS" of
        Nothing -> Right RequireCertificateFiles
        Just value ->
          Left
            ( InvalidConfigValue
                (indexedConfigKey "LISTENER" listenerIndex "TLS_SHARED_WAIT_SECONDS")
                value
            )

    parseAcmeCertificateSource listenerIndex =
      AcmeCertificateSource <$> parseAcmeConfig listenerIndex 80

    parseAcmeConfig listenerIndex parsedPort =
      do
        () <- rejectRemovedCertbotConfig listenerIndex
        let parsedDirectoryUrl =
              fromMaybe
                defaultAcmeDirectoryUrl
                (optionalIndexedConfigValue "LISTENER" listenerIndex "ACME_DIRECTORY_URL")
        parsedContactEmails <-
          parseDelimitedTexts
            (indexedConfigKey "LISTENER" listenerIndex "ACME_CONTACT_EMAILS")
            =<< requiredIndexedConfigValue "LISTENER" listenerIndex "ACME_CONTACT_EMAILS"
        parsedDomains <- parseConfiguredAcmeDomains listenerIndex
        parsedCertbotConfig <- parseAcmeCertbotConfig listenerIndex
        resolvedCertificateDirectory <-
          resolveAcmeCertificateDirectory listenerIndex parsedDomains parsedCertbotConfig
        pure
          AcmeConfig
            { acmeDirectoryUrl = parsedDirectoryUrl,
              acmeContactEmails = parsedContactEmails,
              acmeDomains = parsedDomains,
              acmeHttp01Port = parsedPort,
              acmeCertificateDirectory = Just resolvedCertificateDirectory,
              acmeCertbotConfig = parsedCertbotConfig
            }

    parseConfiguredAcmeDomains listenerIndex =
      maybe
        (Right [])
        (parseDelimitedTexts (indexedConfigKey "LISTENER" listenerIndex "ACME_DOMAINS"))
        (optionalIndexedConfigValue "LISTENER" listenerIndex "ACME_DOMAINS")

    resolveSharedCertificateDirectory listenerIndex =
      case optionalIndexedConfigValue "LISTENER" listenerIndex "TLS_CERTIFICATE_DIRECTORY" of
        Just directory ->
          Right (Text.unpack directory)
        Nothing -> do
          resolvedAcmeDirectories <- traverse resolveConfiguredAcmeCertificateDirectory acmeListenerIndices
          case nub resolvedAcmeDirectories of
            [sharedDirectory] ->
              Right sharedDirectory
            _ ->
              Left
                (MissingConfigValue (indexedConfigKey "LISTENER" listenerIndex "TLS_CERTIFICATE_DIRECTORY"))

    resolveConfiguredAcmeCertificateDirectory listenerIndex = do
      parsedDomains <- parseConfiguredAcmeDomains listenerIndex
      parsedCertbotConfig <- parseAcmeCertbotConfig listenerIndex
      resolveAcmeCertificateDirectory listenerIndex parsedDomains parsedCertbotConfig

    resolveAcmeCertificateDirectory listenerIndex parsedDomains parsedCertbotConfig =
      pure $
        maybe
          ( defaultCertificateDirectoryPath
              (defaultAcmeCertificateIdentifier listenerIndex parsedDomains parsedCertbotConfig)
          )
          Text.unpack
          (optionalIndexedConfigValue "LISTENER" listenerIndex "ACME_CERTIFICATE_DIRECTORY")

    defaultAcmeCertificateIdentifier listenerIndex parsedDomains parsedCertbotConfig =
      fromMaybe
        (Text.pack ("listener-" <> show listenerIndex))
        ( listToMaybe (certbotOptionValues "--cert-name" (certbotArguments parsedCertbotConfig))
            <|> firstCertbotDomain (certbotArguments parsedCertbotConfig)
            <|> listToMaybe parsedDomains
        )

    defaultCertificateDirectoryPath certificateIdentifier =
      defaultCertificateDirectoryRoot <> "/" <> Text.unpack certificateIdentifier

    acmeListenerIndices =
      filter
        listenerHasAcmeRuntime
        (declaredIndices "LISTENER_" allConfigEntries)

    listenerHasAcmeRuntime listenerIndex =
      optionalIndexedConfigValue "LISTENER" listenerIndex "TLS_SOURCE" == Just "acme"
        || listenerHasAcmeConfig listenerIndex

    listenerHasAcmeConfig listenerIndex =
      any
        (Text.isPrefixOf (Text.pack ("LISTENER_" <> show listenerIndex <> "_ACME_")) . fst)
        allConfigEntries

    rejectRemovedCertbotConfig listenerIndex =
      case optionalIndexedConfigValue "LISTENER" listenerIndex "ACME_CHALLENGE_BACKEND" of
        Nothing -> Right ()
        Just backendValue ->
          Left
            ( InvalidConfigValue
                (indexedConfigKey "LISTENER" listenerIndex "ACME_CHALLENGE_BACKEND")
                backendValue
            )

    parseAcmeCertbotConfig listenerIndex =
      pure $
        CertbotConfig
          { certbotExecutable =
              maybe
                defaultCertbotExecutable
                Text.unpack
                (optionalIndexedConfigValue "LISTENER" listenerIndex "ACME_CERTBOT_EXECUTABLE"),
            certbotArguments =
              maybe
                []
                (parseDelimitedTextsUnsafe ",")
                (optionalIndexedConfigValue "LISTENER" listenerIndex "ACME_CERTBOT_ARGUMENTS")
          }

    parseStaticAssetsConfig =
      StaticAssetsConfig
        <$> traverse parseStaticAssetRoot (declaredIndices "STATIC_ASSET_ROOT_" allConfigEntries)
        <*> parseStaticAssetContentTypes
        <*> traverse
          (parseNonNegativeInt "STATIC_CACHE_CONTROL_SECONDS")
          (optionalConfigValue "STATIC_CACHE_CONTROL_SECONDS")

    parseRequestPolicyConfig parsedListeners =
      RequestPolicyConfig
        <$> parseRedirectHttpToHttps parsedListeners
        <*> pure (defaultHttpsRedirectPort parsedListeners)
        <*> parseOptionalStrictTransportSecurity
        <*> parseOptionalBoolWithDefault "TRUST_FORWARDED_HEADERS" False
        <*> parseCorsPolicyConfig
        <*> parseResponseSecurityHeadersConfig

    parseRedirectHttpToHttps parsedListeners =
      case optionalConfigValue "REDIRECT_HTTP_TO_HTTPS" of
        Nothing -> Right (defaultRedirectHttpToHttps parsedListeners)
        Just "true" -> Right True
        Just "false" -> Right False
        Just value -> Left (InvalidConfigValue "REDIRECT_HTTP_TO_HTTPS" value)

    defaultRedirectHttpToHttps parsedListeners =
      any ((== Http) . listenerScheme) parsedListeners
        && any ((== Https) . listenerScheme) parsedListeners

    defaultHttpsRedirectPort parsedListeners =
      if any ((== Http) . listenerScheme) parsedListeners
        then case nub [listenerPort listener | listener <- parsedListeners, listenerScheme listener == Https] of
          [redirectPort] -> Just redirectPort
          _ -> Nothing
        else Nothing

    parseOptionalStrictTransportSecurity =
      case optionalConfigValue "HSTS_MAX_AGE_SECONDS" of
        Nothing ->
          if any (isJust . optionalConfigValue) ["HSTS_INCLUDE_SUBDOMAINS", "HSTS_PRELOAD"]
            then Left (MissingConfigValue "HSTS_MAX_AGE_SECONDS")
            else Right Nothing
        Just maxAgeValue ->
          Just
            <$> ( StrictTransportSecurityConfig
                    <$> parseNonNegativeInt "HSTS_MAX_AGE_SECONDS" maxAgeValue
                    <*> parseOptionalBool "HSTS_INCLUDE_SUBDOMAINS"
                    <*> parseOptionalBool "HSTS_PRELOAD"
                )

    parseOptionalBool key =
      case optionalConfigValue key of
        Nothing -> Right False
        Just "true" -> Right True
        Just "false" -> Right False
        Just value -> Left (InvalidConfigValue key value)

    parseCorsPolicyConfig =
      CorsPolicyConfig
        <$> parseOptionalDelimitedTextList "CORS_ALLOWED_ORIGINS" (corsAllowedOrigins defaultCorsPolicyConfig)
        <*> parseOptionalDelimitedTextList "CORS_ALLOWED_METHODS" (corsAllowedMethods defaultCorsPolicyConfig)
        <*> parseOptionalDelimitedTextList "CORS_ALLOWED_HEADERS" (corsAllowedHeaders defaultCorsPolicyConfig)
        <*> traverse
          (parseNonNegativeInt "CORS_MAX_AGE_SECONDS")
          (optionalConfigValue "CORS_MAX_AGE_SECONDS")

    parseResponseSecurityHeadersConfig =
      ResponseSecurityHeadersConfig
        <$> parseOptionalTextHeader "CONTENT_SECURITY_POLICY" (contentSecurityPolicy defaultResponseSecurityHeadersConfig)
        <*> parseOptionalBoolWithDefault "X_CONTENT_TYPE_OPTIONS_NOSNIFF" (contentTypeOptionsNoSniff defaultResponseSecurityHeadersConfig)
        <*> parseOptionalTextHeader "X_XSS_PROTECTION" (xssProtection defaultResponseSecurityHeadersConfig)
        <*> parseOptionalTextHeader "REFERRER_POLICY" (referrerPolicy defaultResponseSecurityHeadersConfig)
        <*> parseOptionalTextHeader "PERMISSIONS_POLICY" (permissionsPolicy defaultResponseSecurityHeadersConfig)
        <*> parseOptionalTextHeader "X_FRAME_OPTIONS" (frameOptions defaultResponseSecurityHeadersConfig)

    parseOptionalDelimitedTextList key defaultValues =
      case optionalConfigValue key of
        Nothing -> Right defaultValues
        Just value -> parseDelimitedTexts key value

    parseOptionalTextHeader key defaultValue =
      case optionalConfigValue key of
        Nothing -> Right defaultValue
        Just value ->
          if Text.null value
            then Left (InvalidConfigValue key value)
            else Right (Just value)

    parseOptionalBoolWithDefault key defaultValue =
      case optionalConfigValue key of
        Nothing -> Right defaultValue
        Just value -> parseBoolean key value

    parseStaticAssetRoot staticRootIndex =
      StaticAssetRoot
        <$> requiredIndexedConfigValue "STATIC_ASSET_ROOT" staticRootIndex "URL_PREFIX"
        <*> requiredIndexedFilePathValue "STATIC_ASSET_ROOT" staticRootIndex "DIRECTORY"

    parseStaticAssetContentTypes =
      case declaredIndices "STATIC_ASSET_CONTENT_TYPE_" allConfigEntries of
        [] -> Right defaultStaticAssetContentTypes
        contentTypeIndices -> traverse parseStaticAssetContentType contentTypeIndices

    parseStaticAssetContentType contentTypeIndex =
      (,)
        <$> parseStaticAssetExtension contentTypeIndex
        <*> parseStaticAssetMimeType contentTypeIndex

    parseStaticAssetExtension contentTypeIndex = do
      let extensionKey = indexedConfigKey "STATIC_ASSET_CONTENT_TYPE" contentTypeIndex "EXTENSION"
      extension <- requiredConfigValue extensionKey
      if Text.null extension || Text.isPrefixOf "." extension
        then Right extension
        else Left (InvalidConfigValue extensionKey extension)

    parseStaticAssetMimeType contentTypeIndex = do
      let mimeTypeKey = indexedConfigKey "STATIC_ASSET_CONTENT_TYPE" contentTypeIndex "MIME_TYPE"
      mimeType <- requiredConfigValue mimeTypeKey
      if Text.null mimeType
        then Left (InvalidConfigValue mimeTypeKey mimeType)
        else Right mimeType

    parseObservabilityConfig =
      ObservabilityConfig
        <$> parseOptionalTracingExporter
        <*> parseOptionalOtlpExporter "OTLP_METRICS"

    parseOptionalTracingExporter =
      case optionalConfigValue "OTLP_TRACING_ENABLED" of
        Just tracingEnabledValue -> do
          tracingEnabled <- parseBoolean "OTLP_TRACING_ENABLED" tracingEnabledValue
          if tracingEnabled
            then parseOptionalOtlpExporterWithDefault "OTLP_TRACING" (Just defaultLocalTracingEndpoint)
            else Right Nothing
        Nothing ->
          parseOptionalOtlpExporterWithDefault "OTLP_TRACING" Nothing

    parseOptionalOtlpExporter exporterPrefix =
      parseOptionalOtlpExporterWithDefault exporterPrefix Nothing

    parseOptionalOtlpExporterWithDefault exporterPrefix defaultEndpoint =
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
          case defaultEndpoint of
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
