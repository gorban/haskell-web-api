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
    sharedCertificatePaths,
  )
where

import Data.Text (Text)
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

data OtlpExporter = OtlpExporter
  { otlpEndpoint :: Text,
    otlpHeaders :: [(Text, Text)]
  }
  deriving (Eq, Show)

data ObservabilityConfig = ObservabilityConfig
  { tracingExporter :: Maybe OtlpExporter,
    metricsExporter :: Maybe OtlpExporter
  }
  deriving (Eq, Show)

data TelemetrySignal
  = TracingSignal
  | MetricsSignal
  deriving (Eq, Show)

data OtlpExporterStartup = OtlpExporterStartup
  { startupSignal :: TelemetrySignal,
    startupEndpoint :: Text,
    startupHeaders :: [(Text, Text)]
  }
  deriving (Eq, Show)

newtype ObservabilityStartupPlan = ObservabilityStartupPlan
  { startupExporters :: [OtlpExporterStartup]
  }
  deriving (Eq, Show)

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
