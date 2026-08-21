{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module WebApi.Config.Internal
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
    ForwardedHeaderTrust (..),
    ListenerConfig (..),
    ListenerScheme (..),
    ManualTlsCertificateFiles (..),
    ObservabilityConfig (..),
    OtlpExporter (..),
    RequestPolicyConfig (..),
    ResponseSecurityHeadersConfig (..),
    SharedTlsCertificateFiles (..),
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
import Control.Monad (filterM)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Core.Config
  ( ConfigLayers (..),
    ConfigOverridesFileError (..),
    ConfigParseError (..),
    declaredIndices,
    indexedConfigKey,
    loadConfigOverridesFile,
    lookupConfigValue,
    parseBoolean,
    parseDelimitedTexts,
    parseDelimitedTextsUnsafe,
    parseHeaders,
    parseNonNegativeInt,
    parsePositiveInt,
  )
import Data.Bifunctor (bimap, first)
import Data.ByteString qualified as ByteString
import Data.Foldable (traverse_)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromJust, fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb
  ( AcmeConfig (..),
    CertbotConfig (..),
    CorsPolicyConfig (..),
    ForwardedHeaderTrust (..),
    HasServerConfig (..),
    ListenerConfig (..),
    ListenerScheme (..),
    ManualTlsCertificateFiles (..),
    ObservabilityConfig (..),
    OtlpExporter (..),
    RequestByteLimit,
    RequestConcurrencyLimit,
    RequestHeadLimits (..),
    RequestHeaderCountLimit,
    RequestItemCountLimit,
    RequestPolicyConfig (..),
    RequestTimeoutSeconds,
    RequestTransportLimits (..),
    ResponseSecurityHeadersConfig (..),
    ServerConfig (..),
    SharedTlsCertificateFiles (..),
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
    mkRequestConcurrencyLimit,
    mkRequestHeaderCountLimit,
    parseCidrBlock,
    requestByteLimit,
    requestItemCountLimit,
    requestTimeoutSeconds,
    unboundedRequestHeadLimits,
    warpDefaultRequestTransportLimits,
  )
import HarchWeb.Secret (SecretEncryptionKey, mkSecretEncryptionKey)
import System.Environment (getEnvironment)
import Text.Read (readMaybe)

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
    databasePassword :: Text,
    databaseConnectTimeoutSeconds :: Int,
    databasePoolCapacity :: Int
  }
  deriving (Eq)

-- | Redacted: a derived 'Show' would print 'databasePassword' in the clear,
-- and this value is reachable from ordinary diagnostics ('AppEnvironmentConfig'\'s
-- own 'Show', an uncaught 'error', a failing @shouldBe@).
instance Show DatabaseConfig where
  show config =
    "DatabaseConfig {databaseHost = "
      <> show (databaseHost config)
      <> ", databasePort = "
      <> show (databasePort config)
      <> ", databaseName = "
      <> show (databaseName config)
      <> ", databaseUser = "
      <> show (databaseUser config)
      <> ", databasePassword = <redacted>, databaseConnectTimeoutSeconds = "
      <> show (databaseConnectTimeoutSeconds config)
      <> ", databasePoolCapacity = "
      <> show (databasePoolCapacity config)
      <> "}"

data AppEnvironmentConfig = AppEnvironmentConfig
  { appMode :: AppMode,
    databaseConfig :: DatabaseConfig,
    smtpDeliveryConfig :: SmtpDeliveryConfig,
    publicBaseUrl :: Text,
    totpEncryptionKey :: SecretEncryptionKey
  }
  deriving (Eq)

instance Show AppEnvironmentConfig where
  show
    AppEnvironmentConfig
      { appMode,
        databaseConfig,
        smtpDeliveryConfig,
        publicBaseUrl
      } =
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
renderSmtpDeliveryConfig
  SmtpDeliveryConfig
    { smtpDeliveryHost,
      smtpDeliveryPort,
      smtpDeliveryHeloName,
      smtpDeliverySender,
      smtpDeliveryUsername
    } =
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
      <> ", smtpDeliveryPassword = <redacted>}"

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
  toServerConfig
    AppConfig
      { listenerConfigs = appListeners,
        staticAssets = appStaticAssets,
        requestPolicy = appRequestPolicy,
        observability = appObservability
      } =
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
    ("DATABASE_CONNECT_TIMEOUT_SECONDS", "10"),
    ("DATABASE_POOL_CAPACITY", "10"),
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

-- | libpq's own @connect_timeout@ default is to wait indefinitely, which
-- lets a wedged database server pin a request thread forever and, with a
-- concurrency limit configured, eventually starve every other request
-- behind it. Ten seconds bounds that wait without being so short that a
-- momentarily slow server causes spurious failures.
defaultDatabaseConnectTimeoutSeconds :: Int
defaultDatabaseConnectTimeoutSeconds = 10

-- | Bounds how many live libpq connections 'WebApi.Postgres.Pool' opens
-- against this database at once. Ten matches Warp's own comfortable
-- concurrency range for a single small deployment; a busier deployment
-- raises this via 'databasePoolCapacityKey' rather than by editing code.
defaultDatabasePoolCapacity :: Int
defaultDatabasePoolCapacity = 10

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
            databasePassword = "web_api",
            databaseConnectTimeoutSeconds = defaultDatabaseConnectTimeoutSeconds,
            databasePoolCapacity = defaultDatabasePoolCapacity
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
            httpsRedirectAuthority = Nothing,
            strictTransportSecurity = Nothing,
            forwardedHeaderTrust = NeverTrustForwarded,
            requestHeadLimits = unboundedRequestHeadLimits,
            requestTransportLimits = warpDefaultRequestTransportLimits,
            requestConcurrencyLimit = Nothing,
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
    first AppEnvironmentConfigParseError $
      parseAppEnvironmentConfig committedEnvDefaults committedDefaults (localOverrides <> environmentOverrides)
  where
    loadOverridesFile overridesPath =
      fmap
        (first (AppEnvironmentOverridesFileError overridesPath))
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
    first AppStartupConfigParseError $
      parseAppStartupConfig (committedEnvDefaults <> committedRuntimeDefaults) committedDefaults (localOverrides <> environmentOverrides)
  where
    loadOverridesFile overridesPath =
      fmap
        (first (AppStartupOverridesFileError overridesPath))
        (loadConfigOverridesFile overridesPath)

parseAppEnvironmentConfig :: [(Text, Text)] -> [(Text, Text)] -> [(Text, Text)] -> Either ConfigParseError AppEnvironmentConfig
parseAppEnvironmentConfig committedDefaults localOverrides environmentOverrides = do
  parsedMode <- parseMode =<< requiredConfigValue "APP_MODE"
  parsedDatabaseHost <- requiredConfigValue "DATABASE_HOST"
  parsedDatabasePort <- parsePort =<< requiredConfigValue "DATABASE_PORT"
  parsedDatabaseName <- requiredConfigValue "DATABASE_NAME"
  parsedDatabaseUser <- requiredConfigValue "DATABASE_USER"
  parsedDatabasePassword <- requiredConfigValue "DATABASE_PASSWORD"
  parsedDatabaseConnectTimeoutSeconds <- parseConnectTimeout =<< requiredConfigValue databaseConnectTimeoutSecondsKey
  parsedDatabasePoolCapacity <- parsePoolCapacity =<< requiredConfigValue databasePoolCapacityKey
  parsedSmtpHost <- requiredConfigValue "SMTP_HOST"
  parsedSmtpPort <- parseSmtpPort =<< requiredConfigValue "SMTP_PORT"
  parsedSmtpHeloName <- requiredConfigValue "SMTP_HELO_NAME"
  parsedSmtpSender <- requiredConfigValue "EMAIL_FROM"
  parsedSmtpUsername <- requiredConfigValue "SMTP_USER"
  parsedSmtpPassword <- requiredConfigValue "SMTP_PASSWORD"
  parsedPublicBaseUrl <- requiredConfigValue "PUBLIC_BASE_URL"
  parsedTotpEncryptionKey <- parseTotpEncryptionKey =<< requiredConfigValue "TOTP_ENCRYPTION_KEY"
  () <- validateProductionTotpEncryptionKey parsedMode parsedTotpEncryptionKey
  pure
    AppEnvironmentConfig
      { appMode = parsedMode,
        databaseConfig =
          DatabaseConfig
            { databaseHost = parsedDatabaseHost,
              databasePort = parsedDatabasePort,
              databaseName = parsedDatabaseName,
              databaseUser = parsedDatabaseUser,
              databasePassword = parsedDatabasePassword,
              databaseConnectTimeoutSeconds = parsedDatabaseConnectTimeoutSeconds,
              databasePoolCapacity = parsedDatabasePoolCapacity
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
      case lookupConfigValue key configLayers of
        Just value -> Right value
        Nothing -> Left (MissingConfigValue key)

    configLayers =
      ConfigLayers
        { configLayerCommittedDefaults = committedDefaults,
          configLayerLocalOverrides = localOverrides,
          configLayerEnvironmentOverrides = environmentOverrides
        }

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

-- | The @DATABASE_CONNECT_TIMEOUT_SECONDS@ env var name, named once instead
-- of written at both this module's uses (the required-value lookup in
-- 'parseAppEnvironmentConfig' and 'parseConnectTimeout''s own error label).
-- Per @docs/design-guidance.md@'s never-mask-a-gate-finding rule: naming the
-- shared literal once is the preferred fix and is applied here, but it does
-- not fully close the HPC gap by itself, confirmed directly rather than
-- assumed — 'databaseConnectTimeoutSecondsKey' is a trivial 'Text' literal
-- CAF, and GHC's @-O2@ optimizer inlines it back to a bare literal at
-- 'parseConnectTimeout''s reference, reproducing the same CSE-sharing gap
-- the naming was meant to remove. The @$!@ below is the last-resort fix for
-- that one remaining reference.
databaseConnectTimeoutSecondsKey :: Text
databaseConnectTimeoutSecondsKey = "DATABASE_CONNECT_TIMEOUT_SECONDS"

-- | Non-negative, not positive: libpq treats a @connect_timeout@ below 2
-- seconds (including 0) as "wait indefinitely", which is a legitimate,
-- explicit opt-out of the bound this field otherwise provides.
{-# ANN parseConnectTimeout ("HLint: ignore Redundant $!" :: String) #-}
parseConnectTimeout :: Text -> Either ConfigParseError Int
parseConnectTimeout = parseNonNegativeInt $! databaseConnectTimeoutSecondsKey

-- | The @DATABASE_POOL_CAPACITY@ env var name, mirroring
-- 'databaseConnectTimeoutSecondsKey' above.
databasePoolCapacityKey :: Text
databasePoolCapacityKey = "DATABASE_POOL_CAPACITY"

-- | Positive, not non-negative: a zero-capacity pool can never hand out a
-- connection, so every database-backed request would block forever.
--
-- Naming 'databasePoolCapacityKey' once instead of writing the env var name
-- at both this module's uses does not fully close the coverage gap by
-- itself, confirmed directly rather than assumed: it is a trivial 'Text'
-- literal CAF, and GHC's @-O2@ optimizer inlines it back to a bare literal
-- at this reference, reproducing the same CSE-sharing gap the naming was
-- meant to remove. The @$!@ below forces that one remaining reference.
-- Confirmed directly (not assumed by analogy to this module's other
-- @$!@-forced key lookups) that HLint's @--language=ImportQualifiedPost@
-- invocation does not flag this line, so no ignore pragma is added.
parsePoolCapacity :: Text -> Either ConfigParseError Int
parsePoolCapacity = parsePositiveInt $! databasePoolCapacityKey

parseSmtpPort :: Text -> Either ConfigParseError Int
parseSmtpPort value = do
  parsedPort <- parsePositiveInt "SMTP_PORT" value
  if parsedPort <= 65535
    then Right parsedPort
    else Left (InvalidConfigValue "SMTP_PORT" value)

parseTotpEncryptionKey :: Text -> Either ConfigParseError SecretEncryptionKey
parseTotpEncryptionKey value =
  maybe
    (Left (InvalidConfigValue "TOTP_ENCRYPTION_KEY" "<redacted>"))
    Right
    (mkSecretEncryptionKey value)

defaultTotpEncryptionKey :: SecretEncryptionKey
defaultTotpEncryptionKey = fromJust (mkSecretEncryptionKey "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")

validateProductionTotpEncryptionKey :: AppMode -> SecretEncryptionKey -> Either ConfigParseError ()
validateProductionTotpEncryptionKey appMode encryptionKey =
  if appMode == Production && encryptionKey == defaultTotpEncryptionKey
    then Left (InvalidConfigValue "TOTP_ENCRYPTION_KEY" "development-default")
    else Right ()

parseAppStartupConfig :: [(Text, Text)] -> [(Text, Text)] -> [(Text, Text)] -> Either ConfigParseError AppStartupConfig
parseAppStartupConfig committedDefaults localOverrides environmentOverrides =
  AppStartupConfig
    <$> parseAppEnvironmentConfig committedDefaults localOverrides environmentOverrides
    <*> parseRuntimeAppConfig committedDefaults localOverrides environmentOverrides

parseRuntimeAppConfig :: [(Text, Text)] -> [(Text, Text)] -> [(Text, Text)] -> Either ConfigParseError AppConfig
parseRuntimeAppConfig committedDefaults localOverrides environmentOverrides = do
  runReaderT
    parseAppConfig
    ConfigSources
      { configCommittedDefaults = committedDefaults,
        configLocalOverrides = localOverrides,
        configEnvironmentOverrides = environmentOverrides
      }

data ConfigSources = ConfigSources
  { configCommittedDefaults :: [(Text, Text)],
    configLocalOverrides :: [(Text, Text)],
    configEnvironmentOverrides :: [(Text, Text)]
  }

type ConfigParser = ReaderT ConfigSources (Either ConfigParseError)

liftEitherP :: Either ConfigParseError value -> ConfigParser value
liftEitherP = either throwError pure

parseAppConfig :: ConfigParser AppConfig
parseAppConfig = do
  parsedTitlePrefix <- requiredConfigValueP "APP_TITLE_PREFIX"
  parsedListeners <- parseListenerConfigsP
  parsedStaticAssets <- parseStaticAssetsConfigP
  parsedRequestPolicy <- parseRequestPolicyConfigP parsedListeners
  parsedObservability <- parseObservabilityConfigP
  pure
    AppConfig
      { appTitlePrefix = parsedTitlePrefix,
        listenerConfigs = parsedListeners,
        staticAssets = parsedStaticAssets,
        requestPolicy = parsedRequestPolicy,
        observability = parsedObservability
      }

allConfigEntriesP :: ConfigParser [(Text, Text)]
allConfigEntriesP = asks (\sources -> configCommittedDefaults sources <> configLocalOverrides sources <> configEnvironmentOverrides sources)

optionalConfigValueP :: Text -> ConfigParser (Maybe Text)
optionalConfigValueP key =
  asks (lookupConfigValue key . configLayersFromSources)

configLayersFromSources :: ConfigSources -> ConfigLayers
configLayersFromSources sources =
  ConfigLayers
    { configLayerCommittedDefaults = configCommittedDefaults sources,
      configLayerLocalOverrides = configLocalOverrides sources,
      configLayerEnvironmentOverrides = configEnvironmentOverrides sources
    }

requiredConfigValueP :: Text -> ConfigParser Text
requiredConfigValueP key = do
  maybeValue <- optionalConfigValueP key
  liftEitherP (maybe (Left (MissingConfigValue key)) Right maybeValue)

parseListenerConfigsP :: ConfigParser [ListenerConfig]
parseListenerConfigsP = do
  entries <- allConfigEntriesP
  case declaredIndices "LISTENER_" entries of
    [] -> throwError (MissingConfigValue "LISTENER_0_HOST")
    listenerIndices -> traverse parseListenerConfigP listenerIndices

parseListenerConfigP :: Int -> ConfigParser ListenerConfig
parseListenerConfigP listenerIndex = do
  parsedHost <- requiredIndexedConfigValueP "LISTENER" listenerIndex "HOST"
  parsedPort <- parseRequiredIndexedP listenerIndex "PORT" parsePositiveInt
  parsedScheme <- parseRequiredIndexedP listenerIndex "SCHEME" parseListenerScheme
  parsedAcme <- parseListenerAcmeConfigP listenerIndex parsedScheme parsedPort
  parsedTls <- parseListenerTlsConfigP listenerIndex parsedScheme
  pure ListenerConfig {listenerHost = parsedHost, listenerPort = parsedPort, listenerScheme = parsedScheme, listenerTls = parsedTls, listenerAcme = parsedAcme}

parseRequiredIndexedP :: Int -> Text -> (Text -> Text -> Either ConfigParseError value) -> ConfigParser value
parseRequiredIndexedP listenerIndex suffix parser = do
  let key = indexedConfigKey "LISTENER" listenerIndex suffix
  value <- requiredConfigValueP key
  liftEitherP (parser key value)

parseListenerAcmeConfigP :: Int -> ListenerScheme -> Int -> ConfigParser (Maybe AcmeConfig)
parseListenerAcmeConfigP listenerIndex Http parsedPort = do
  hasAcmeConfig <- listenerHasAcmeConfigP listenerIndex
  if hasAcmeConfig then Just <$> parseAcmeConfigP listenerIndex parsedPort else pure Nothing
parseListenerAcmeConfigP _ Https _ = pure Nothing

parseListenerTlsConfigP :: Int -> ListenerScheme -> ConfigParser (Maybe TlsConfig)
parseListenerTlsConfigP _ Http = pure Nothing
parseListenerTlsConfigP listenerIndex Https = do
  tlsSource <- requiredIndexedConfigValueP "LISTENER" listenerIndex "TLS_SOURCE"
  Just . TlsConfig <$> parseTlsCertificateSourceP listenerIndex tlsSource

parseTlsCertificateSourceP :: Int -> Text -> ConfigParser TlsCertificateSource
parseTlsCertificateSourceP listenerIndex tlsSource =
  case tlsSource of
    "manual" -> ManualCertificateFiles <$> (ManualTlsCertificateFiles <$> requiredIndexedFilePathValueP "LISTENER" listenerIndex "TLS_CERTIFICATE_FILE" <*> requiredIndexedFilePathValueP "LISTENER" listenerIndex "TLS_PRIVATE_KEY_FILE")
    "shared" -> parseSharedCertificateSourceP listenerIndex (AwaitCertificateFiles <$> parseSharedTlsWaitTimeoutP listenerIndex)
    "shared-wait" -> parseSharedCertificateSourceP listenerIndex (AwaitCertificateFiles <$> parseSharedTlsWaitTimeoutP listenerIndex)
    "shared-fail-fast" -> parseSharedCertificateSourceP listenerIndex (parseSharedTlsFailFastModeP listenerIndex)
    "acme" -> AcmeCertificateSource <$> parseAcmeConfigP listenerIndex 80
    _ -> throwError (InvalidConfigValue (indexedConfigKey "LISTENER" listenerIndex "TLS_SOURCE") tlsSource)

parseSharedCertificateSourceP :: Int -> ConfigParser TlsStartupMode -> ConfigParser TlsCertificateSource
parseSharedCertificateSourceP listenerIndex parseStartupMode = SharedCertificateFiles <$> (SharedTlsCertificateFiles <$> resolveSharedCertificateDirectoryP listenerIndex <*> parseStartupMode)

parseSharedTlsWaitTimeoutP :: Int -> ConfigParser (Maybe Int)
parseSharedTlsWaitTimeoutP listenerIndex = do
  let key = indexedConfigKey "LISTENER" listenerIndex "TLS_SHARED_WAIT_SECONDS"
  maybeValue <- optionalConfigValueP key
  liftEitherP (traverse (parseNonNegativeInt key) maybeValue)

parseSharedTlsFailFastModeP :: Int -> ConfigParser TlsStartupMode
parseSharedTlsFailFastModeP listenerIndex = do
  let key = indexedConfigKey "LISTENER" listenerIndex "TLS_SHARED_WAIT_SECONDS"
  maybeValue <- optionalConfigValueP key
  liftEitherP (maybe (Right RequireCertificateFiles) (Left . InvalidConfigValue key) maybeValue)

parseAcmeConfigP :: Int -> Int -> ConfigParser AcmeConfig
parseAcmeConfigP listenerIndex parsedPort = do
  rejectRemovedCertbotConfigP listenerIndex
  parsedDirectoryUrl <- fromMaybe defaultAcmeDirectoryUrl <$> optionalIndexedConfigValueP "LISTENER" listenerIndex "ACME_DIRECTORY_URL"
  parsedContactEmails <- parseRequiredIndexedP listenerIndex "ACME_CONTACT_EMAILS" parseDelimitedTexts
  parsedDomains <- parseConfiguredAcmeDomainsP listenerIndex
  parsedCertbotConfig <- parseAcmeCertbotConfigP listenerIndex
  resolvedCertificateDirectory <- resolveAcmeCertificateDirectoryP listenerIndex parsedDomains parsedCertbotConfig
  pure AcmeConfig {acmeDirectoryUrl = parsedDirectoryUrl, acmeContactEmails = parsedContactEmails, acmeDomains = parsedDomains, acmeHttp01Port = parsedPort, acmeCertificateDirectory = Just resolvedCertificateDirectory, acmeCertbotConfig = parsedCertbotConfig}

parseConfiguredAcmeDomainsP :: Int -> ConfigParser [Text]
parseConfiguredAcmeDomainsP listenerIndex = do
  let key = indexedConfigKey "LISTENER" listenerIndex "ACME_DOMAINS"
  maybeValue <- optionalConfigValueP key
  liftEitherP (maybe (Right []) (parseDelimitedTexts key) maybeValue)

resolveSharedCertificateDirectoryP :: Int -> ConfigParser FilePath
resolveSharedCertificateDirectoryP listenerIndex = do
  maybeDirectory <- optionalIndexedConfigValueP "LISTENER" listenerIndex "TLS_CERTIFICATE_DIRECTORY"
  case maybeDirectory of
    Just directory -> pure (Text.unpack directory)
    Nothing -> do
      listenerIndices <- acmeListenerIndicesP
      resolvedDirectories <- traverse resolveConfiguredAcmeCertificateDirectoryP listenerIndices
      liftEitherP $ case nub resolvedDirectories of
        [sharedDirectory] -> Right sharedDirectory
        _ -> Left (MissingConfigValue (indexedConfigKey "LISTENER" listenerIndex "TLS_CERTIFICATE_DIRECTORY"))

resolveConfiguredAcmeCertificateDirectoryP :: Int -> ConfigParser FilePath
resolveConfiguredAcmeCertificateDirectoryP listenerIndex = do
  parsedDomains <- parseConfiguredAcmeDomainsP listenerIndex
  parsedCertbotConfig <- parseAcmeCertbotConfigP listenerIndex
  resolveAcmeCertificateDirectoryP listenerIndex parsedDomains parsedCertbotConfig

resolveAcmeCertificateDirectoryP :: Int -> [Text] -> CertbotConfig -> ConfigParser FilePath
resolveAcmeCertificateDirectoryP listenerIndex parsedDomains parsedCertbotConfig = do
  configuredDirectory <- optionalIndexedConfigValueP "LISTENER" listenerIndex "ACME_CERTIFICATE_DIRECTORY"
  pure $ maybe (defaultCertificateDirectoryPath (defaultAcmeCertificateIdentifier listenerIndex parsedDomains parsedCertbotConfig)) Text.unpack configuredDirectory

defaultAcmeCertificateIdentifier :: Int -> [Text] -> CertbotConfig -> Text
defaultAcmeCertificateIdentifier listenerIndex parsedDomains parsedCertbotConfig =
  fromMaybe (Text.pack ("listener-" <> show listenerIndex)) (listToMaybe (certbotOptionValues "--cert-name" arguments) <|> firstCertbotDomain arguments <|> listToMaybe parsedDomains)
  where
    arguments = certbotArguments parsedCertbotConfig

defaultCertificateDirectoryPath :: Text -> FilePath
defaultCertificateDirectoryPath certificateIdentifier = defaultCertificateDirectoryRoot <> "/" <> Text.unpack certificateIdentifier

acmeListenerIndicesP :: ConfigParser [Int]
acmeListenerIndicesP = allConfigEntriesP >>= filterM listenerHasAcmeRuntimeP . declaredIndices "LISTENER_"

listenerHasAcmeRuntimeP :: Int -> ConfigParser Bool
listenerHasAcmeRuntimeP listenerIndex = do
  tlsSource <- optionalIndexedConfigValueP "LISTENER" listenerIndex "TLS_SOURCE"
  hasAcmeConfig <- listenerHasAcmeConfigP listenerIndex
  pure (tlsSource == Just "acme" || hasAcmeConfig)

listenerHasAcmeConfigP :: Int -> ConfigParser Bool
listenerHasAcmeConfigP listenerIndex =
  any (Text.isPrefixOf (Text.pack ("LISTENER_" <> show listenerIndex <> "_ACME_")) . fst) <$> allConfigEntriesP

rejectRemovedCertbotConfigP :: Int -> ConfigParser ()
rejectRemovedCertbotConfigP listenerIndex =
  optionalConfigValueP key >>= traverse_ (throwError . InvalidConfigValue key)
  where
    key = indexedConfigKey "LISTENER" listenerIndex "ACME_CHALLENGE_BACKEND"

parseAcmeCertbotConfigP :: Int -> ConfigParser CertbotConfig
parseAcmeCertbotConfigP listenerIndex =
  (CertbotConfig . maybe defaultCertbotExecutable Text.unpack <$> optionalIndexedConfigValueP "LISTENER" listenerIndex "ACME_CERTBOT_EXECUTABLE")
    <*> (maybe [] (parseDelimitedTextsUnsafe ",") <$> optionalIndexedConfigValueP "LISTENER" listenerIndex "ACME_CERTBOT_ARGUMENTS")

parseStaticAssetsConfigP :: ConfigParser StaticAssetsConfig
parseStaticAssetsConfigP = do
  entries <- allConfigEntriesP
  StaticAssetsConfig
    <$> traverse parseStaticAssetRootP (declaredIndices "STATIC_ASSET_ROOT_" entries)
    <*> parseStaticAssetContentTypesP
    <*> (optionalConfigValueP "STATIC_CACHE_CONTROL_SECONDS" >>= liftEitherP . traverse (parseNonNegativeInt "STATIC_CACHE_CONTROL_SECONDS"))

parseStaticAssetRootP :: Int -> ConfigParser StaticAssetRoot
parseStaticAssetRootP staticRootIndex = StaticAssetRoot <$> requiredIndexedConfigValueP "STATIC_ASSET_ROOT" staticRootIndex "URL_PREFIX" <*> requiredIndexedFilePathValueP "STATIC_ASSET_ROOT" staticRootIndex "DIRECTORY"

parseStaticAssetContentTypesP :: ConfigParser [(Text, Text)]
parseStaticAssetContentTypesP = do
  entries <- allConfigEntriesP
  case declaredIndices "STATIC_ASSET_CONTENT_TYPE_" entries of
    [] -> pure defaultStaticAssetContentTypes
    contentTypeIndices -> traverse parseStaticAssetContentTypeP contentTypeIndices

parseStaticAssetContentTypeP :: Int -> ConfigParser (Text, Text)
parseStaticAssetContentTypeP contentTypeIndex = (,) <$> parseStaticAssetExtensionP contentTypeIndex <*> parseStaticAssetMimeTypeP contentTypeIndex

parseStaticAssetExtensionP :: Int -> ConfigParser Text
parseStaticAssetExtensionP contentTypeIndex = do
  let key = indexedConfigKey "STATIC_ASSET_CONTENT_TYPE" contentTypeIndex "EXTENSION"
  extension <- requiredConfigValueP key
  liftEitherP $ if Text.null extension || Text.isPrefixOf "." extension then Right extension else Left (InvalidConfigValue key extension)

parseStaticAssetMimeTypeP :: Int -> ConfigParser Text
parseStaticAssetMimeTypeP contentTypeIndex = do
  let key = indexedConfigKey "STATIC_ASSET_CONTENT_TYPE" contentTypeIndex "MIME_TYPE"
  mimeType <- requiredConfigValueP key
  liftEitherP $ if Text.null mimeType then Left (InvalidConfigValue key mimeType) else Right mimeType

parseRequestPolicyConfigP :: [ListenerConfig] -> ConfigParser RequestPolicyConfig
parseRequestPolicyConfigP parsedListeners =
  RequestPolicyConfig
    <$> parseRedirectHttpToHttpsP parsedListeners
    <*> pure (defaultHttpsRedirectPort parsedListeners)
    <*> pure (defaultHttpsRedirectAuthority parsedListeners)
    <*> parseOptionalStrictTransportSecurityP
    <*> parseForwardedHeaderTrustP
    <*> parseRequestHeadLimitsP
    <*> parseRequestTransportLimitsP
    <*> parseOptionalRequestConcurrencyLimitP "REQUEST_MAX_CONCURRENT"
    <*> parseCorsPolicyConfigP
    <*> parseResponseSecurityHeadersConfigP

-- | These deployment limits are opt-in.  An absent setting remains unbounded
-- so upgrading does not silently change an established application's traffic
-- contract; deployments select values appropriate to their proxy and memory
-- budget.
parseRequestHeadLimitsP :: ConfigParser RequestHeadLimits
parseRequestHeadLimitsP =
  RequestHeadLimits
    <$> parseOptionalRequestByteLimitP "REQUEST_TARGET_MAX_BYTES"
    <*> parseOptionalRequestByteLimitP "REQUEST_HEADER_MAX_BYTES"
    <*> parseOptionalRequestHeaderCountLimitP "REQUEST_HEADER_MAX_COUNT"
    <*> parseOptionalRequestByteLimitP "REQUEST_HEADER_VALUE_MAX_BYTES"
    <*> parseOptionalRequestItemCountLimitP "REQUEST_PATH_SEGMENT_MAX_COUNT"
    <*> parseOptionalRequestByteLimitP "REQUEST_PATH_SEGMENT_MAX_BYTES"
    <*> parseOptionalRequestItemCountLimitP "REQUEST_QUERY_FIELD_MAX_COUNT"
    <*> parseOptionalRequestByteLimitP "REQUEST_QUERY_FIELD_MAX_BYTES"

parseRequestTransportLimitsP :: ConfigParser RequestTransportLimits
parseRequestTransportLimitsP =
  RequestTransportLimits
    <$> parseOptionalRequestTimeoutSecondsP "REQUEST_NETWORK_TIMEOUT_SECONDS"
    <*> parseOptionalRequestByteLimitP "REQUEST_SLOWLORIS_MAX_BYTES"

-- | Parse an optional integer config value directly into the smart
-- constructor that owns its bound, rather than duplicating that bound in an
-- intermediate 'parseNonNegativeInt'\/'parsePositiveInt' check first: the
-- two checks agreeing today made the constructor's own rejection dead code,
-- so a future change to either bound without the other would have silently
-- downgraded a configured limit to \"no limit\" instead of failing startup.
parseOptionalBoundedIntP :: Text -> (Int -> Maybe limit) -> ConfigParser (Maybe limit)
parseOptionalBoundedIntP key construct = do
  maybeValue <- optionalConfigValueP key
  case maybeValue of
    Nothing -> pure Nothing
    Just rawValue -> do
      parsedInt <- liftEitherP (maybe (Left (InvalidConfigValue key rawValue)) Right (readMaybe (Text.unpack rawValue)))
      liftEitherP (maybe (Left (InvalidConfigValue key rawValue)) (Right . Just) (construct parsedInt))

parseOptionalRequestByteLimitP :: Text -> ConfigParser (Maybe RequestByteLimit)
parseOptionalRequestByteLimitP key = parseOptionalBoundedIntP key requestByteLimit

parseOptionalRequestTimeoutSecondsP :: Text -> ConfigParser (Maybe RequestTimeoutSeconds)
parseOptionalRequestTimeoutSecondsP key = parseOptionalBoundedIntP key requestTimeoutSeconds

parseOptionalRequestConcurrencyLimitP :: Text -> ConfigParser (Maybe RequestConcurrencyLimit)
parseOptionalRequestConcurrencyLimitP key = parseOptionalBoundedIntP key mkRequestConcurrencyLimit

parseOptionalRequestHeaderCountLimitP :: Text -> ConfigParser (Maybe RequestHeaderCountLimit)
parseOptionalRequestHeaderCountLimitP key = parseOptionalBoundedIntP key mkRequestHeaderCountLimit

parseOptionalRequestItemCountLimitP :: Text -> ConfigParser (Maybe RequestItemCountLimit)
parseOptionalRequestItemCountLimitP key = parseOptionalBoundedIntP key requestItemCountLimit

-- | Accepts the same case-insensitive @true\/false\/1\/0\/yes\/no@ forms as
-- every other boolean config knob (via 'parseOptionalBoolWithDefaultP'),
-- rather than only the exact lowercase @"true"@\/@"false"@ this previously
-- hand-matched.
parseRedirectHttpToHttpsP :: [ListenerConfig] -> ConfigParser Bool
parseRedirectHttpToHttpsP parsedListeners =
  parseOptionalBoolWithDefaultP "REDIRECT_HTTP_TO_HTTPS" (defaultRedirectHttpToHttps parsedListeners)

defaultRedirectHttpToHttps :: [ListenerConfig] -> Bool
defaultRedirectHttpToHttps parsedListeners = any ((== Http) . listenerScheme) parsedListeners && any ((== Https) . listenerScheme) parsedListeners

defaultHttpsRedirectPort :: [ListenerConfig] -> Maybe Int
defaultHttpsRedirectPort parsedListeners =
  if any ((== Http) . listenerScheme) parsedListeners
    then case nub [listenerPort listener | listener <- parsedListeners, listenerScheme listener == Https] of
      [redirectPort] -> Just redirectPort
      _ -> Nothing
    else Nothing

-- | A configuration-time best-effort default for 'HarchWeb.httpsRedirectAuthority':
-- the host of this app's own HTTPS listener, when exactly one distinct host
-- is declared. This only covers a deployment that terminates TLS itself; a
-- deployment behind a TLS-offloading proxy declares no HTTPS listener at
-- all, so 'WebApi.App.buildRuntimeApp' overrides this with the host parsed
-- from @PUBLIC_BASE_URL@, which is required in every deployment shape.
defaultHttpsRedirectAuthority :: [ListenerConfig] -> Maybe ByteString.ByteString
defaultHttpsRedirectAuthority parsedListeners =
  if any ((== Http) . listenerScheme) parsedListeners
    then case nub [listenerHost listener | listener <- parsedListeners, listenerScheme listener == Https] of
      [singleHttpsHost] -> Just (TextEncoding.encodeUtf8 singleHttpsHost)
      _ -> Nothing
    else Nothing

-- | Trust is a property of the peer, not a global flag: a bare on\/off
-- toggle let any client spoof its own @X-Forwarded-*@ headers regardless
-- of who actually connected. @TRUSTED_FORWARDED_PROXIES@ names the CIDR
-- blocks (comma-separated, e.g. @10.0.0.0\/8,172.16.0.0\/12@) whose peers
-- this deployment's own reverse proxy connects from; absent or empty means
-- 'NeverTrustForwarded'. See the DE decision record in
-- @docs/design-guidance.md@.
parseForwardedHeaderTrustP :: ConfigParser ForwardedHeaderTrust
parseForwardedHeaderTrustP = do
  cidrTexts <- parseOptionalDelimitedTextListP "TRUSTED_FORWARDED_PROXIES" []
  case cidrTexts of
    [] -> pure NeverTrustForwarded
    firstCidrText : remainingCidrTexts ->
      TrustForwardedFrom <$> liftEitherP (traverse parseCidrBlockConfigValue (firstCidrText :| remainingCidrTexts))
  where
    parseCidrBlockConfigValue cidrText =
      maybe (Left (InvalidConfigValue "TRUSTED_FORWARDED_PROXIES" cidrText)) Right (parseCidrBlock cidrText)

parseOptionalStrictTransportSecurityP :: ConfigParser (Maybe StrictTransportSecurityConfig)
parseOptionalStrictTransportSecurityP = do
  maybeMaxAge <- optionalConfigValueP "HSTS_MAX_AGE_SECONDS"
  case maybeMaxAge of
    Just maxAgeValue -> Just <$> (StrictTransportSecurityConfig <$> liftEitherP (parseNonNegativeInt "HSTS_MAX_AGE_SECONDS" maxAgeValue) <*> parseOptionalBoolP "HSTS_INCLUDE_SUBDOMAINS" <*> parseOptionalBoolP "HSTS_PRELOAD")
    Nothing -> do
      hasDependentSetting <- any isJust <$> traverse optionalConfigValueP ["HSTS_INCLUDE_SUBDOMAINS", "HSTS_PRELOAD"]
      if hasDependentSetting then throwError (MissingConfigValue "HSTS_MAX_AGE_SECONDS") else pure Nothing

parseOptionalBoolP :: Text -> ConfigParser Bool
parseOptionalBoolP key = optionalConfigValueP key >>= maybe (pure False) (liftEitherP . parseBoolean key)

parseOptionalBoolWithDefaultP :: Text -> Bool -> ConfigParser Bool
parseOptionalBoolWithDefaultP key defaultValue = optionalConfigValueP key >>= maybe (pure defaultValue) (liftEitherP . parseBoolean key)

parseCorsPolicyConfigP :: ConfigParser CorsPolicyConfig
parseCorsPolicyConfigP =
  CorsPolicyConfig
    <$> parseOptionalDelimitedTextListP "CORS_ALLOWED_ORIGINS" (corsAllowedOrigins defaultCorsPolicyConfig)
    <*> parseOptionalDelimitedTextListP "CORS_ALLOWED_METHODS" (corsAllowedMethods defaultCorsPolicyConfig)
    <*> parseOptionalDelimitedTextListP "CORS_ALLOWED_HEADERS" (corsAllowedHeaders defaultCorsPolicyConfig)
    <*> (optionalConfigValueP "CORS_MAX_AGE_SECONDS" >>= liftEitherP . traverse (parseNonNegativeInt "CORS_MAX_AGE_SECONDS"))

parseOptionalDelimitedTextListP :: Text -> [Text] -> ConfigParser [Text]
parseOptionalDelimitedTextListP key defaultValues = optionalConfigValueP key >>= maybe (pure defaultValues) (liftEitherP . parseDelimitedTexts key)

parseResponseSecurityHeadersConfigP :: ConfigParser ResponseSecurityHeadersConfig
parseResponseSecurityHeadersConfigP =
  ResponseSecurityHeadersConfig
    <$> parseOptionalTextHeaderP "CONTENT_SECURITY_POLICY" (contentSecurityPolicy defaultResponseSecurityHeadersConfig)
    <*> parseOptionalBoolWithDefaultP "X_CONTENT_TYPE_OPTIONS_NOSNIFF" (contentTypeOptionsNoSniff defaultResponseSecurityHeadersConfig)
    <*> parseOptionalTextHeaderP "X_XSS_PROTECTION" (xssProtection defaultResponseSecurityHeadersConfig)
    <*> parseOptionalTextHeaderP "REFERRER_POLICY" (referrerPolicy defaultResponseSecurityHeadersConfig)
    <*> parseOptionalTextHeaderP "PERMISSIONS_POLICY" (permissionsPolicy defaultResponseSecurityHeadersConfig)
    <*> parseOptionalTextHeaderP "X_FRAME_OPTIONS" (frameOptions defaultResponseSecurityHeadersConfig)

parseOptionalTextHeaderP :: Text -> Maybe Text -> ConfigParser (Maybe Text)
parseOptionalTextHeaderP key defaultValue = do
  maybeValue <- optionalConfigValueP key
  liftEitherP $ case maybeValue of
    Nothing -> Right defaultValue
    Just value | Text.null value -> Left (InvalidConfigValue key value)
    Just value -> Right (Just value)

parseObservabilityConfigP :: ConfigParser ObservabilityConfig
parseObservabilityConfigP = ObservabilityConfig <$> parseOptionalTracingExporterP <*> parseOptionalOtlpExporterP "OTLP_METRICS"

parseOptionalTracingExporterP :: ConfigParser (Maybe OtlpExporter)
parseOptionalTracingExporterP = do
  maybeEnabled <- optionalConfigValueP "OTLP_TRACING_ENABLED"
  case maybeEnabled of
    Nothing -> parseOptionalOtlpExporterWithDefaultP "OTLP_TRACING" Nothing
    Just enabledValue -> do
      enabled <- liftEitherP (parseBoolean "OTLP_TRACING_ENABLED" enabledValue)
      if enabled then parseOptionalOtlpExporterWithDefaultP "OTLP_TRACING" (Just defaultLocalTracingEndpoint) else pure Nothing

parseOptionalOtlpExporterP :: Text -> ConfigParser (Maybe OtlpExporter)
parseOptionalOtlpExporterP exporterPrefix = parseOptionalOtlpExporterWithDefaultP exporterPrefix Nothing

parseOptionalOtlpExporterWithDefaultP :: Text -> Maybe Text -> ConfigParser (Maybe OtlpExporter)
parseOptionalOtlpExporterWithDefaultP exporterPrefix defaultEndpoint = do
  configuredEndpoint <- optionalConfigValueP (exporterPrefix <> "_ENDPOINT")
  configuredHeaders <- optionalConfigValueP (exporterPrefix <> "_HEADERS")
  case configuredEndpoint <|> defaultEndpoint of
    Just endpoint -> do
      headers <-
        traverse
          (liftEitherP . parseHeaders (exporterPrefix <> "_HEADERS"))
          configuredHeaders
      pure
        ( Just
            OtlpExporter
              { otlpEndpoint = endpoint,
                otlpHeaders = fromMaybe [] headers
              }
        )
    Nothing -> liftEitherP $ case configuredHeaders of
      Just _ -> Left (MissingConfigValue (exporterPrefix <> "_ENDPOINT"))
      Nothing -> Right Nothing

requiredIndexedConfigValueP :: Text -> Int -> Text -> ConfigParser Text
requiredIndexedConfigValueP prefix configIndex suffix = requiredConfigValueP (indexedConfigKey prefix configIndex suffix)

optionalIndexedConfigValueP :: Text -> Int -> Text -> ConfigParser (Maybe Text)
optionalIndexedConfigValueP prefix configIndex suffix = optionalConfigValueP (indexedConfigKey prefix configIndex suffix)

requiredIndexedFilePathValueP :: Text -> Int -> Text -> ConfigParser FilePath
requiredIndexedFilePathValueP prefix configIndex suffix = Text.unpack <$> requiredIndexedConfigValueP prefix configIndex suffix

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
