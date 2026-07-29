{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Private configuration and startup-plan vocabulary shared by the server
-- runtime and the ACME implementation. The supported public surface is
-- re-exported by 'HarchWeb.Server' and the framework facade.
module HarchWeb.Server.Config
  ( AcmeBindPlan (..),
    AcmeConfig (..),
    CertbotConfig (..),
    HasServerConfig (..),
    HttpBindPlan (..),
    ListenerConfig (..),
    ListenerEndpoint (..),
    ListenerScheme (..),
    ListenerStartupError (..),
    ManualTlsBindPlan (..),
    ServerConfig (..),
    ServerStartupPlan (..),
    ObservabilityConfig (..),
    ObservabilityStartupPlan (..),
    OtlpExporter (..),
    OtlpExporterStartup (..),
    TelemetrySignal (..),
    TlsCertificateSource (..),
    TlsConfig (..),
    TlsCredentialSourceKind (..),
    TlsStartupMode (..),
    planServerStartup,
    sharedCertificatePaths,
  )
where

import Data.Maybe (maybeToList)
import Data.Text (Text)
import HarchWeb.Observability
  ( ObservabilityConfig (..),
    ObservabilityStartupPlan (..),
    OtlpExporter (..),
    OtlpExporterStartup (..),
    TelemetrySignal (..),
  )
import HarchWeb.Security (RequestPolicyConfig)
import HarchWeb.StaticAssets (StaticAssetsConfig)
import System.FilePath ((</>))

data ListenerScheme
  = Http
  | Https
  deriving (Eq, Show)

data CertbotConfig = CertbotConfig
  { certbotExecutable :: FilePath,
    certbotArguments :: [Text]
  }
  deriving (Eq, Show)

data AcmeConfig = AcmeConfig
  { acmeDirectoryUrl :: Text,
    acmeContactEmails :: [Text],
    acmeDomains :: [Text],
    acmeHttp01Port :: Int,
    acmeCertificateDirectory :: Maybe FilePath,
    acmeCertbotConfig :: CertbotConfig
  }
  deriving (Eq, Show)

data TlsCertificateSource
  = ManualCertificateFiles
      { certificateFile :: FilePath,
        privateKeyFile :: FilePath
      }
  | SharedCertificateFiles
      { certificateDirectory :: FilePath,
        sharedCertificateStartupMode :: TlsStartupMode
      }
  | AcmeCertificateSource AcmeConfig
  deriving (Eq, Show)

data TlsStartupMode
  = RequireCertificateFiles
  | AwaitCertificateFiles
      { certificateWaitTimeoutSeconds :: Maybe Int
      }
  deriving (Eq, Show)

data TlsCredentialSourceKind
  = ManualTlsCredentials
  | SharedTlsCredentials
  deriving (Eq, Show)

newtype TlsConfig = TlsConfig
  { certificateSource :: TlsCertificateSource
  }
  deriving (Eq, Show)

data ListenerConfig = ListenerConfig
  { listenerHost :: Text,
    listenerPort :: Int,
    listenerScheme :: ListenerScheme,
    listenerTls :: Maybe TlsConfig,
    listenerAcme :: Maybe AcmeConfig
  }
  deriving (Eq)

instance Show ListenerConfig where
  showsPrec precedence listenerConfig =
    showParen (precedence > 10) $
      showString "ListenerConfig {listenerHost = "
        . shows (listenerHost listenerConfig)
        . showString ", listenerPort = "
        . shows (listenerPort listenerConfig)
        . showString ", listenerScheme = "
        . shows (listenerScheme listenerConfig)
        . showString ", listenerTls = "
        . shows (listenerTls listenerConfig)
        . maybe id (\acmeConfig -> showString ", listenerAcme = " . shows acmeConfig) (listenerAcme listenerConfig)
        . showString "}"

data ServerConfig = ServerConfig
  { listenerConfigs :: [ListenerConfig],
    staticAssets :: StaticAssetsConfig,
    requestPolicy :: RequestPolicyConfig,
    observability :: ObservabilityConfig
  }
  deriving (Eq, Show)

class HasServerConfig config where
  toServerConfig :: config -> ServerConfig

instance HasServerConfig ServerConfig where
  toServerConfig = id

data ListenerEndpoint = ListenerEndpoint
  { endpointHost :: Text,
    endpointPort :: Int
  }
  deriving (Eq, Show)

newtype HttpBindPlan = HttpBindPlan
  { httpEndpoints :: [ListenerEndpoint]
  }
  deriving (Eq, Show)

data ManualTlsBindPlan = ManualTlsBindPlan
  { tlsEndpoint :: ListenerEndpoint,
    tlsCertificateFile :: FilePath,
    tlsPrivateKeyFile :: FilePath,
    tlsCredentialSourceKind :: TlsCredentialSourceKind,
    tlsStartupMode :: TlsStartupMode
  }
  deriving (Eq, Show)

data AcmeBindPlan = AcmeBindPlan
  { acmeEndpoint :: ListenerEndpoint,
    acmeTlsEndpoint :: Maybe ListenerEndpoint,
    acmeListenerConfig :: AcmeConfig
  }
  deriving (Eq, Show)

data ServerStartupPlan = ServerStartupPlan
  { httpBindPlan :: HttpBindPlan,
    manualTlsBindPlans :: [ManualTlsBindPlan],
    acmeBindPlans :: [AcmeBindPlan]
  }
  deriving (Eq, Show)

data ListenerStartupError
  = DuplicateListenerEndpoint ListenerEndpoint
  | InvalidListenerTlsConfiguration ListenerConfig
  | InvalidListenerAcmeConfiguration ListenerConfig
  deriving (Eq, Show)

sharedCertificatePaths :: FilePath -> (FilePath, FilePath)
sharedCertificatePaths certificateDirectory =
  (certificateDirectory </> "fullchain.pem", certificateDirectory </> "privkey.pem")

-- | Validate listener declarations and describe the runtime listeners they
-- require. This is configuration planning, so it stays independent of socket
-- and application execution.
planServerStartup :: (HasServerConfig config) => config -> Either ListenerStartupError ServerStartupPlan
planServerStartup config = do
  plannedListeners <- concat <$> traverse classifyListener (listenerConfigs (toServerConfig config))
  case firstDuplicate (concatMap plannedBindEndpoints plannedListeners) of
    Just duplicateEndpoint -> Left (DuplicateListenerEndpoint duplicateEndpoint)
    Nothing ->
      Right
        ServerStartupPlan
          { httpBindPlan =
              HttpBindPlan
                { httpEndpoints =
                    [ endpoint
                    | PlannedHttp endpoint <- plannedListeners
                    ]
                },
            manualTlsBindPlans =
              [ manualTlsBindPlan
              | PlannedManualTls manualTlsBindPlan <- plannedListeners
              ],
            acmeBindPlans =
              [ acmeBindPlan
              | PlannedAcme acmeBindPlan <- plannedListeners
              ]
          }

data PlannedListener
  = PlannedHttp ListenerEndpoint
  | PlannedManualTls ManualTlsBindPlan
  | PlannedAcme AcmeBindPlan

classifyListener :: ListenerConfig -> Either ListenerStartupError [PlannedListener]
classifyListener listenerConfig =
  case (listenerScheme listenerConfig, listenerTls listenerConfig, listenerAcme listenerConfig) of
    (Http, Nothing, Nothing) ->
      Right [PlannedHttp (listenerEndpoint listenerConfig)]
    (Http, Nothing, Just acmeConfig) ->
      Right
        [ PlannedHttp (listenerEndpoint listenerConfig),
          PlannedAcme
            AcmeBindPlan
              { acmeEndpoint = listenerEndpoint listenerConfig,
                acmeTlsEndpoint = Nothing,
                acmeListenerConfig = acmeConfig
              }
        ]
    (Http, Just _, _) ->
      Left (InvalidListenerTlsConfiguration listenerConfig)
    (Https, _, Just _) ->
      Left (InvalidListenerAcmeConfiguration listenerConfig)
    (Https, Nothing, Nothing) ->
      Left (InvalidListenerTlsConfiguration listenerConfig)
    (Https, Just TlsConfig {certificateSource = ManualCertificateFiles {certificateFile = certificatePath, privateKeyFile = privateKeyPath}}, Nothing) ->
      Right
        [ PlannedManualTls
            ManualTlsBindPlan
              { tlsEndpoint = listenerEndpoint listenerConfig,
                tlsCertificateFile = certificatePath,
                tlsPrivateKeyFile = privateKeyPath,
                tlsCredentialSourceKind = ManualTlsCredentials,
                tlsStartupMode = RequireCertificateFiles
              }
        ]
    (Https, Just TlsConfig {certificateSource = SharedCertificateFiles {certificateDirectory = sharedDirectory, sharedCertificateStartupMode = startupMode}}, Nothing) ->
      let (certificatePath, privateKeyPath) = sharedCertificatePaths sharedDirectory
       in Right
            [ PlannedManualTls
                ManualTlsBindPlan
                  { tlsEndpoint = listenerEndpoint listenerConfig,
                    tlsCertificateFile = certificatePath,
                    tlsPrivateKeyFile = privateKeyPath,
                    tlsCredentialSourceKind = SharedTlsCredentials,
                    tlsStartupMode = startupMode
                  }
            ]
    (Https, Just TlsConfig {certificateSource = AcmeCertificateSource acmeConfig}, Nothing) ->
      Right
        [ PlannedAcme
            AcmeBindPlan
              { acmeEndpoint = listenerEndpoint listenerConfig,
                acmeTlsEndpoint = Just (listenerEndpoint listenerConfig),
                acmeListenerConfig = acmeConfig
              }
        ]

plannedBindEndpoints :: PlannedListener -> [ListenerEndpoint]
plannedBindEndpoints plannedListener =
  case plannedListener of
    PlannedHttp endpoint -> [endpoint]
    PlannedManualTls manualTlsBindPlan -> [tlsEndpoint manualTlsBindPlan]
    PlannedAcme acmeBindPlan -> maybeToList (acmeTlsEndpoint acmeBindPlan)

listenerEndpoint :: ListenerConfig -> ListenerEndpoint
listenerEndpoint listenerConfig =
  ListenerEndpoint
    { endpointHost = listenerHost listenerConfig,
      endpointPort = listenerPort listenerConfig
    }

firstDuplicate :: (Eq value) => [value] -> Maybe value
firstDuplicate values =
  case values of
    [] -> Nothing
    value : remainingValues ->
      if value `elem` remainingValues
        then Just value
        else firstDuplicate remainingValues
