module Core.Config
  ( ConfigOverridesFileError (..),
    ConfigParseError (..),
    RuntimeAppConfigLoadError (..),
    committedRuntimeDefaults,
    defaultAppConfig,
    loadConfigOverridesFile,
    loadRuntimeAppConfig,
    parseConfigOverridesFile,
    parseRuntimeAppConfig,
  )
where

import Control.Applicative ((<|>))
import Data.Bifunctor (first)
import Data.Char (isDigit)
import Data.List (nub, sort)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import HarchWeb
  ( AcmeChallengeBackend (..),
    AcmeConfig (..),
    AppConfig (..),
    CertbotConfig (..),
    ListenerConfig (..),
    ListenerScheme (..),
    ObservabilityConfig (..),
    OtlpExporter (..),
    StaticAssetRoot (..),
    StaticAssetsConfig (..),
    TlsCertificateSource (..),
    TlsConfig (..),
  )
import System.Directory (doesFileExist)
import Text.Read (readMaybe)

data ConfigParseError
  = MissingConfigValue Text
  | InvalidConfigValue Text Text
  deriving (Eq, Show)

data ConfigOverridesFileError
  = InvalidConfigOverridesLine Int Text
  deriving (Eq, Show)

data RuntimeAppConfigLoadError
  = InvalidConfigOverridesFile ConfigOverridesFileError
  | InvalidRuntimeConfig ConfigParseError
  deriving (Eq, Show)

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

committedRuntimeDefaults :: [(Text, Text)]
committedRuntimeDefaults =
  [ (Text.pack "APP_TITLE_PREFIX", Text.pack "web-api"),
    (Text.pack "LISTENER_0_HOST", Text.pack "127.0.0.1"),
    (Text.pack "LISTENER_0_PORT", Text.pack "5001"),
    (Text.pack "LISTENER_0_SCHEME", Text.pack "http")
  ]

loadConfigOverridesFile :: FilePath -> IO (Either ConfigOverridesFileError [(Text, Text)])
loadConfigOverridesFile overridesPath = do
  overridesFileExists <- doesFileExist overridesPath
  if overridesFileExists
    then parseConfigOverridesFile <$> TextIO.readFile overridesPath
    else pure (Right [])

loadRuntimeAppConfig :: FilePath -> [(Text, Text)] -> IO (Either RuntimeAppConfigLoadError AppConfig)
loadRuntimeAppConfig overridesPath environmentOverrides = do
  overridesResult <- loadConfigOverridesFile overridesPath
  pure $
    case overridesResult of
      Left overridesError -> Left (InvalidConfigOverridesFile overridesError)
      Right localOverrides ->
        first InvalidRuntimeConfig $
          parseRuntimeAppConfig committedRuntimeDefaults localOverrides environmentOverrides

parseConfigOverridesFile :: Text -> Either ConfigOverridesFileError [(Text, Text)]
parseConfigOverridesFile =
  fmap concat
    . traverse parseLine
    . zip [1 :: Int ..]
    . Text.lines
  where
    parseLine (lineNumber, rawLine) =
      let strippedLine = Text.strip rawLine
       in if Text.null strippedLine || Text.isPrefixOf (Text.pack "#") strippedLine
            then Right []
            else
              let (rawKey, rawValueWithSeparator) = Text.breakOn (Text.pack "=") strippedLine
                  strippedKey = Text.strip rawKey
               in if Text.null rawValueWithSeparator || Text.null strippedKey
                    then Left (InvalidConfigOverridesLine lineNumber rawLine)
                    else Right [(strippedKey, Text.strip (Text.drop 1 rawValueWithSeparator))]

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

parsePositiveInt :: Text -> Text -> Either ConfigParseError Int
parsePositiveInt key value =
  case readMaybe (Text.unpack value) of
    Just parsedInt
      | parsedInt > 0 -> Right parsedInt
    _ -> Left (InvalidConfigValue key value)

parseNonNegativeInt :: Text -> Text -> Either ConfigParseError Int
parseNonNegativeInt key value =
  case readMaybe (Text.unpack value) of
    Just parsedInt
      | parsedInt >= 0 -> Right parsedInt
    _ -> Left (InvalidConfigValue key value)

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

parseDelimitedTexts :: Text -> Text -> Either ConfigParseError [Text]
parseDelimitedTexts key value =
  case parseDelimitedTextsUnsafe (Text.pack ",") value of
    [] -> Left (InvalidConfigValue key value)
    parsedValues -> Right parsedValues

parseDelimitedTextsUnsafe :: Text -> Text -> [Text]
parseDelimitedTextsUnsafe delimiter =
  filter (not . Text.null)
    . map Text.strip
    . Text.splitOn delimiter

parseHeadersUnsafe :: Text -> [(Text, Text)]
parseHeadersUnsafe value =
  mapMaybe parseHeaderPair (parseDelimitedTextsUnsafe (Text.pack ";") value)
  where
    parseHeaderPair headerEntry =
      let (headerName, headerValueWithSeparator) = Text.breakOn (Text.pack "=") headerEntry
       in if Text.null headerName || Text.null headerValueWithSeparator
            then Nothing
            else Just (Text.strip headerName, Text.strip (Text.drop 1 headerValueWithSeparator))

declaredIndices :: Text -> [(Text, Text)] -> [Int]
declaredIndices entryPrefix =
  sort . nub . mapMaybe (extractIndexedKey entryPrefix . fst)

extractIndexedKey :: Text -> Text -> Maybe Int
extractIndexedKey entryPrefix entryKey =
  if Text.isPrefixOf entryPrefix entryKey
    then
      let indexedSuffix = Text.drop (Text.length entryPrefix) entryKey
          (indexDigits, remainder) = Text.span isDigit indexedSuffix
       in if Text.null indexDigits || not (Text.isPrefixOf (Text.pack "_") remainder)
            then Nothing
            else readMaybe (Text.unpack indexDigits)
    else Nothing

indexedConfigKey :: Text -> Int -> Text -> Text
indexedConfigKey prefix configIndex suffix =
  prefix <> Text.pack "_" <> Text.pack (show configIndex) <> Text.pack "_" <> suffix

lookupConfigValue :: Text -> [(Text, Text)] -> [(Text, Text)] -> [(Text, Text)] -> Maybe Text
lookupConfigValue key committedDefaults localOverrides environmentOverrides =
  lookupInLayer environmentOverrides
    `orElse` lookupInLayer localOverrides
    `orElse` lookupInLayer committedDefaults
  where
    lookupInLayer = lookup key . reverse

orElse :: Maybe value -> Maybe value -> Maybe value
orElse maybeValue fallbackValue =
  maybeValue <|> fallbackValue
