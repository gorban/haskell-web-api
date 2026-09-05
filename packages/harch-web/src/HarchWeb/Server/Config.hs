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
    ManualTlsCertificateFiles (..),
    ServerConfig (..),
    ServerStartupPlan (..),
    ObservabilityConfig (..),
    ObservabilityStartupPlan (..),
    OtlpExporter (..),
    OtlpExporterStartup (..),
    SharedTlsCertificateFiles (..),
    TlsCipherSuite (..),
    TelemetrySignal (..),
    TlsCertificateSource (..),
    TlsConfig (..),
    TlsCredentialSourceKind (..),
    TlsPolicy (..),
    TlsProtocolVersion (..),
    TlsStartupMode (..),
    defaultTlsPolicy,
    planServerStartup,
    sharedCertificatePaths,
    tlsCipherSuiteValue,
    tlsCipherSuiteFromIdentifier,
    tlsPolicySupports,
    tlsProtocolVersionValue,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
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
import Network.TLS qualified as TLS
import Network.TLS.Extra.Cipher qualified as TLSExtra
import System.FilePath ((</>))

data ListenerScheme
  = Http
  | Https
  deriving (Eq, Show)

data CertbotConfig = CertbotConfig
  { certbotExecutable :: FilePath,
    certbotArguments :: [Text]
  }
  deriving (Eq)

-- | Certbot's configured argv is intentionally not rendered: a Certbot
-- plugin can accept a credential-looking value and process arguments are a
-- disclosure surface. Runtime environment configuration therefore no longer
-- accepts arbitrary Certbot arguments; the derived HTTP-01 invocation is the
-- supported built-in path. DNS or other custom authentication belongs in an
-- operator-controlled executable wrapper that obtains its own credentials
-- from a root-owned file or managed environment, never from this framework's
-- configuration (PR-SEC5, 2026-08-28).
instance Show CertbotConfig where
  showsPrec precedence certbotConfig =
    showParen
      (precedence > 10)
      ( showString "CertbotConfig {certbotExecutable = "
          . showString (show (certbotExecutable certbotConfig))
          . showString ", certbotArguments = <redacted: "
          . shows (length (certbotArguments certbotConfig))
          . showString ">}"
      )

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
  = ManualCertificateFiles ManualTlsCertificateFiles
  | SharedCertificateFiles SharedTlsCertificateFiles
  | AcmeCertificateSource AcmeConfig
  deriving (Eq, Show)

data ManualTlsCertificateFiles = ManualTlsCertificateFiles
  { certificateFile :: FilePath,
    privateKeyFile :: FilePath
  }
  deriving (Eq, Show)

data SharedTlsCertificateFiles = SharedTlsCertificateFiles
  { certificateDirectory :: FilePath,
    sharedCertificateStartupMode :: TlsStartupMode
  }
  deriving (Eq, Show)

-- | 'AwaitCertificateFiles' is plain positional, not a named record: its
-- one field has zero external accessor use (confirmed by grep, not
-- assumed), so wrapping it in its own nested record purely to satisfy
-- '-Wpartial-fields' would add a type with no reader beyond this
-- declaration. 'RequireCertificateFiles' has no fields to be partial about.
data TlsStartupMode
  = RequireCertificateFiles
  | AwaitCertificateFiles (Maybe Int)
  deriving (Eq, Show)

data TlsCredentialSourceKind
  = ManualTlsCredentials
  | SharedTlsCredentials
  deriving (Eq, Show)

-- | A closed TLS protocol identifier accepted by listener configuration.
--
-- TLS 1.0 and 1.1 exist only as explicit compatibility choices.  The default
-- policy is TLS 1.2/1.3; callers selecting older protocols must also select a
-- cipher suite that can negotiate with each selected version.
data TlsProtocolVersion
  = Tls10
  | Tls11
  | Tls12
  | Tls13

-- | The installed @tls@ package's supported strong cipher-suite inventory.
--
-- Constructor names deliberately describe the protocol suite rather than a
-- library implementation detail.  Parsing happens at the application config
-- boundary; transport code receives only this closed value, so a misspelled
-- or newly unsupported environment value cannot silently widen policy.
data TlsCipherSuite
  = TlsEcdheEcdsaAes256GcmSha384
  | TlsEcdheEcdsaChacha20Poly1305Sha256
  | TlsEcdheEcdsaAes256CcmSha256
  | TlsEcdheEcdsaAes128GcmSha256
  | TlsEcdheEcdsaAes128CcmSha256
  | TlsEcdheRsaAes256GcmSha384
  | TlsEcdheRsaChacha20Poly1305Sha256
  | TlsEcdheRsaAes128GcmSha256
  | TlsDheRsaAes256GcmSha384
  | TlsDheRsaChacha20Poly1305Sha256
  | TlsDheRsaAes256CcmSha256
  | TlsDheRsaAes128GcmSha256
  | TlsDheRsaAes128CcmSha256
  | TlsEcdheEcdsaAes256CbcSha384
  | TlsEcdheRsaAes256CbcSha384
  | TlsDheRsaAes256CbcSha256
  | TlsEcdheEcdsaAes256CbcSha
  | TlsEcdheRsaAes256CbcSha
  | TlsDheRsaAes256CbcSha
  | TlsRsaAes256GcmSha384
  | TlsRsaAes256CcmSha256
  | TlsRsaAes256CbcSha256
  | TlsRsaAes256CbcSha
  | Tls13Aes256GcmSha384
  | Tls13Chacha20Poly1305Sha256
  | Tls13Aes128GcmSha256
  | Tls13Aes128CcmSha256

-- | The TLS protocol and cipher policy carried from a listener declaration to
-- its runtime.  This extends the existing TLS configuration/bind-plan boundary
-- instead of introducing a second transport-policy path (DT, 2026-08-26).
data TlsPolicy = TlsPolicy
  { tlsAllowedVersions :: NonEmpty TlsProtocolVersion,
    tlsCipherSuites :: NonEmpty TlsCipherSuite
  }

-- | These instances describe the closed configuration vocabulary directly
-- rather than delegating its observable equality and diagnostic rendering to
-- compiler-generated defaults.  In particular, every cipher's identity is
-- its installed @tls@ cipher ID, and the rendered form remains the stable
-- constructor spelling already used by configuration diagnostics.  This is a
-- total representation of the values that the parser and transport can
-- produce; focused configuration tests exercise equality, inequality, and
-- rendering for every constructor.
instance Eq TlsProtocolVersion where
  left == right = tlsProtocolVersionValue left == tlsProtocolVersionValue right
  left /= right = not (left == right)

instance Show TlsProtocolVersion where
  showsPrec _ tlsProtocolVersion =
    showString $
      case tlsProtocolVersion of
        Tls10 -> "Tls10"
        Tls11 -> "Tls11"
        Tls12 -> "Tls12"
        Tls13 -> "Tls13"

instance Eq TlsCipherSuite where
  left == right = TLS.cipherID (tlsCipherSuiteValue left) == TLS.cipherID (tlsCipherSuiteValue right)
  left /= right = not (left == right)

instance Show TlsCipherSuite where
  showsPrec _ tlsCipherSuite =
    showString $
      case tlsCipherSuite of
        TlsEcdheEcdsaAes256GcmSha384 -> "TlsEcdheEcdsaAes256GcmSha384"
        TlsEcdheEcdsaChacha20Poly1305Sha256 -> "TlsEcdheEcdsaChacha20Poly1305Sha256"
        TlsEcdheEcdsaAes256CcmSha256 -> "TlsEcdheEcdsaAes256CcmSha256"
        TlsEcdheEcdsaAes128GcmSha256 -> "TlsEcdheEcdsaAes128GcmSha256"
        TlsEcdheEcdsaAes128CcmSha256 -> "TlsEcdheEcdsaAes128CcmSha256"
        TlsEcdheRsaAes256GcmSha384 -> "TlsEcdheRsaAes256GcmSha384"
        TlsEcdheRsaChacha20Poly1305Sha256 -> "TlsEcdheRsaChacha20Poly1305Sha256"
        TlsEcdheRsaAes128GcmSha256 -> "TlsEcdheRsaAes128GcmSha256"
        TlsDheRsaAes256GcmSha384 -> "TlsDheRsaAes256GcmSha384"
        TlsDheRsaChacha20Poly1305Sha256 -> "TlsDheRsaChacha20Poly1305Sha256"
        TlsDheRsaAes256CcmSha256 -> "TlsDheRsaAes256CcmSha256"
        TlsDheRsaAes128GcmSha256 -> "TlsDheRsaAes128GcmSha256"
        TlsDheRsaAes128CcmSha256 -> "TlsDheRsaAes128CcmSha256"
        TlsEcdheEcdsaAes256CbcSha384 -> "TlsEcdheEcdsaAes256CbcSha384"
        TlsEcdheRsaAes256CbcSha384 -> "TlsEcdheRsaAes256CbcSha384"
        TlsDheRsaAes256CbcSha256 -> "TlsDheRsaAes256CbcSha256"
        TlsEcdheEcdsaAes256CbcSha -> "TlsEcdheEcdsaAes256CbcSha"
        TlsEcdheRsaAes256CbcSha -> "TlsEcdheRsaAes256CbcSha"
        TlsDheRsaAes256CbcSha -> "TlsDheRsaAes256CbcSha"
        TlsRsaAes256GcmSha384 -> "TlsRsaAes256GcmSha384"
        TlsRsaAes256CcmSha256 -> "TlsRsaAes256CcmSha256"
        TlsRsaAes256CbcSha256 -> "TlsRsaAes256CbcSha256"
        TlsRsaAes256CbcSha -> "TlsRsaAes256CbcSha"
        Tls13Aes256GcmSha384 -> "Tls13Aes256GcmSha384"
        Tls13Chacha20Poly1305Sha256 -> "Tls13Chacha20Poly1305Sha256"
        Tls13Aes128GcmSha256 -> "Tls13Aes128GcmSha256"
        Tls13Aes128CcmSha256 -> "Tls13Aes128CcmSha256"

instance Eq TlsPolicy where
  left == right =
    tlsAllowedVersions left == tlsAllowedVersions right
      && tlsCipherSuites left == tlsCipherSuites right
  left /= right = not (left == right)

instance Show TlsPolicy where
  showsPrec precedence tlsPolicy =
    showParen (precedence > 10) $
      showString "TlsPolicy {tlsAllowedVersions = "
        . shows (tlsAllowedVersions tlsPolicy)
        . showString ", tlsCipherSuites = "
        . shows (tlsCipherSuites tlsPolicy)
        . showString "}"

-- | Modern web-server defaults: TLS 1.2/1.3 and only AEAD/PFS suites.
defaultTlsPolicy :: TlsPolicy
defaultTlsPolicy =
  TlsPolicy
    { tlsAllowedVersions = Tls12 :| [Tls13],
      tlsCipherSuites =
        TlsEcdheEcdsaAes256GcmSha384
          :| [ TlsEcdheEcdsaChacha20Poly1305Sha256,
               TlsEcdheEcdsaAes128GcmSha256,
               TlsEcdheRsaAes256GcmSha384,
               TlsEcdheRsaChacha20Poly1305Sha256,
               TlsEcdheRsaAes128GcmSha256,
               Tls13Aes256GcmSha384,
               Tls13Chacha20Poly1305Sha256,
               Tls13Aes128GcmSha256
             ]
    }

tlsProtocolVersionValue :: TlsProtocolVersion -> TLS.Version
tlsProtocolVersionValue tlsProtocolVersion =
  case tlsProtocolVersion of
    Tls10 -> TLS.TLS10
    Tls11 -> TLS.TLS11
    Tls12 -> TLS.TLS12
    Tls13 -> TLS.TLS13

tlsCipherSuiteValue :: TlsCipherSuite -> TLS.Cipher
tlsCipherSuiteValue tlsCipherSuite =
  case tlsCipherSuite of
    TlsEcdheEcdsaAes256GcmSha384 -> TLSExtra.cipher_ECDHE_ECDSA_AES256GCM_SHA384
    TlsEcdheEcdsaChacha20Poly1305Sha256 -> TLSExtra.cipher_ECDHE_ECDSA_CHACHA20POLY1305_SHA256
    TlsEcdheEcdsaAes256CcmSha256 -> TLSExtra.cipher_ECDHE_ECDSA_AES256CCM_SHA256
    TlsEcdheEcdsaAes128GcmSha256 -> TLSExtra.cipher_ECDHE_ECDSA_AES128GCM_SHA256
    TlsEcdheEcdsaAes128CcmSha256 -> TLSExtra.cipher_ECDHE_ECDSA_AES128CCM_SHA256
    TlsEcdheRsaAes256GcmSha384 -> TLSExtra.cipher_ECDHE_RSA_AES256GCM_SHA384
    TlsEcdheRsaChacha20Poly1305Sha256 -> TLSExtra.cipher_ECDHE_RSA_CHACHA20POLY1305_SHA256
    TlsEcdheRsaAes128GcmSha256 -> TLSExtra.cipher_ECDHE_RSA_AES128GCM_SHA256
    TlsDheRsaAes256GcmSha384 -> TLSExtra.cipher_DHE_RSA_AES256GCM_SHA384
    TlsDheRsaChacha20Poly1305Sha256 -> TLSExtra.cipher_DHE_RSA_CHACHA20POLY1305_SHA256
    TlsDheRsaAes256CcmSha256 -> TLSExtra.cipher_DHE_RSA_AES256CCM_SHA256
    TlsDheRsaAes128GcmSha256 -> TLSExtra.cipher_DHE_RSA_AES128GCM_SHA256
    TlsDheRsaAes128CcmSha256 -> TLSExtra.cipher_DHE_RSA_AES128CCM_SHA256
    TlsEcdheEcdsaAes256CbcSha384 -> TLSExtra.cipher_ECDHE_ECDSA_AES256CBC_SHA384
    TlsEcdheRsaAes256CbcSha384 -> TLSExtra.cipher_ECDHE_RSA_AES256CBC_SHA384
    TlsDheRsaAes256CbcSha256 -> TLSExtra.cipher_DHE_RSA_AES256_SHA256
    TlsEcdheEcdsaAes256CbcSha -> TLSExtra.cipher_ECDHE_ECDSA_AES256CBC_SHA
    TlsEcdheRsaAes256CbcSha -> TLSExtra.cipher_ECDHE_RSA_AES256CBC_SHA
    TlsDheRsaAes256CbcSha -> TLSExtra.cipher_DHE_RSA_AES256_SHA1
    TlsRsaAes256GcmSha384 -> TLSExtra.cipher_AES256GCM_SHA384
    TlsRsaAes256CcmSha256 -> TLSExtra.cipher_AES256CCM_SHA256
    TlsRsaAes256CbcSha256 -> TLSExtra.cipher_AES256_SHA256
    TlsRsaAes256CbcSha -> TLSExtra.cipher_AES256_SHA1
    Tls13Aes256GcmSha384 -> TLSExtra.cipher_TLS13_AES256GCM_SHA384
    Tls13Chacha20Poly1305Sha256 -> TLSExtra.cipher_TLS13_CHACHA20POLY1305_SHA256
    Tls13Aes128GcmSha256 -> TLSExtra.cipher_TLS13_AES128GCM_SHA256
    Tls13Aes128CcmSha256 -> TLSExtra.cipher_TLS13_AES128CCM_SHA256

-- | Parse the IANA spelling used by @LISTENER_<n>_TLS_CIPHER_SUITES@.
tlsCipherSuiteFromIdentifier :: Text -> Maybe TlsCipherSuite
tlsCipherSuiteFromIdentifier identifier =
  lookup identifier tlsCipherSuiteIdentifiers

tlsCipherSuiteIdentifiers :: [(Text, TlsCipherSuite)]
tlsCipherSuiteIdentifiers =
  [ ("TLS_ECDHE_ECDSA_WITH_AES_256_GCM_SHA384", TlsEcdheEcdsaAes256GcmSha384),
    ("TLS_ECDHE_ECDSA_WITH_CHACHA20_POLY1305_SHA256", TlsEcdheEcdsaChacha20Poly1305Sha256),
    ("TLS_ECDHE_ECDSA_WITH_AES_256_CCM", TlsEcdheEcdsaAes256CcmSha256),
    ("TLS_ECDHE_ECDSA_WITH_AES_128_GCM_SHA256", TlsEcdheEcdsaAes128GcmSha256),
    ("TLS_ECDHE_ECDSA_WITH_AES_128_CCM", TlsEcdheEcdsaAes128CcmSha256),
    ("TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384", TlsEcdheRsaAes256GcmSha384),
    ("TLS_ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256", TlsEcdheRsaChacha20Poly1305Sha256),
    ("TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256", TlsEcdheRsaAes128GcmSha256),
    ("TLS_DHE_RSA_WITH_AES_256_GCM_SHA384", TlsDheRsaAes256GcmSha384),
    ("TLS_DHE_RSA_WITH_CHACHA20_POLY1305_SHA256", TlsDheRsaChacha20Poly1305Sha256),
    ("TLS_DHE_RSA_WITH_AES_256_CCM", TlsDheRsaAes256CcmSha256),
    ("TLS_DHE_RSA_WITH_AES_128_GCM_SHA256", TlsDheRsaAes128GcmSha256),
    ("TLS_DHE_RSA_WITH_AES_128_CCM", TlsDheRsaAes128CcmSha256),
    ("TLS_ECDHE_ECDSA_WITH_AES_256_CBC_SHA384", TlsEcdheEcdsaAes256CbcSha384),
    ("TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA384", TlsEcdheRsaAes256CbcSha384),
    ("TLS_DHE_RSA_WITH_AES_256_CBC_SHA256", TlsDheRsaAes256CbcSha256),
    ("TLS_ECDHE_ECDSA_WITH_AES_256_CBC_SHA", TlsEcdheEcdsaAes256CbcSha),
    ("TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA", TlsEcdheRsaAes256CbcSha),
    ("TLS_DHE_RSA_WITH_AES_256_CBC_SHA", TlsDheRsaAes256CbcSha),
    ("TLS_RSA_WITH_AES_256_GCM_SHA384", TlsRsaAes256GcmSha384),
    ("TLS_RSA_WITH_AES_256_CCM", TlsRsaAes256CcmSha256),
    ("TLS_RSA_WITH_AES_256_CBC_SHA256", TlsRsaAes256CbcSha256),
    ("TLS_RSA_WITH_AES_256_CBC_SHA", TlsRsaAes256CbcSha),
    ("TLS_AES_256_GCM_SHA384", Tls13Aes256GcmSha384),
    ("TLS_CHACHA20_POLY1305_SHA256", Tls13Chacha20Poly1305Sha256),
    ("TLS_AES_128_GCM_SHA256", Tls13Aes128GcmSha256),
    ("TLS_AES_128_CCM_SHA256", Tls13Aes128CcmSha256)
  ]

-- | A configured policy is valid only when every allowed protocol can select
-- at least one configured cipher suite.  This keeps incompatible override
-- values out of the runtime handshake path.
tlsPolicySupports :: TlsPolicy -> Bool
tlsPolicySupports tlsPolicy =
  all
    (\tlsVersion -> any (TLS.cipherAllowedForVersion (tlsProtocolVersionValue tlsVersion) . tlsCipherSuiteValue) (tlsCipherSuites tlsPolicy))
    (tlsAllowedVersions tlsPolicy)

data TlsConfig = TlsConfig
  { certificateSource :: TlsCertificateSource,
    tlsPolicy :: TlsPolicy
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
    tlsStartupMode :: TlsStartupMode,
    tlsBindPolicy :: TlsPolicy
  }
  deriving (Eq, Show)

data AcmeBindPlan = AcmeBindPlan
  { acmeEndpoint :: ListenerEndpoint,
    acmeTlsEndpoint :: Maybe ListenerEndpoint,
    acmeListenerConfig :: AcmeConfig,
    acmeTlsPolicy :: TlsPolicy
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
                acmeListenerConfig = acmeConfig,
                acmeTlsPolicy = defaultTlsPolicy
              }
        ]
    (Http, Just _, _) ->
      Left (InvalidListenerTlsConfiguration listenerConfig)
    (Https, _, Just _) ->
      Left (InvalidListenerAcmeConfiguration listenerConfig)
    (Https, Nothing, Nothing) ->
      Left (InvalidListenerTlsConfiguration listenerConfig)
    (Https, Just TlsConfig {certificateSource = ManualCertificateFiles ManualTlsCertificateFiles {certificateFile = certificatePath, privateKeyFile = privateKeyPath}, tlsPolicy = configuredTlsPolicy}, Nothing) ->
      Right
        [ PlannedManualTls
            ManualTlsBindPlan
              { tlsEndpoint = listenerEndpoint listenerConfig,
                tlsCertificateFile = certificatePath,
                tlsPrivateKeyFile = privateKeyPath,
                tlsCredentialSourceKind = ManualTlsCredentials,
                tlsStartupMode = RequireCertificateFiles,
                tlsBindPolicy = configuredTlsPolicy
              }
        ]
    (Https, Just TlsConfig {certificateSource = SharedCertificateFiles SharedTlsCertificateFiles {certificateDirectory = sharedDirectory, sharedCertificateStartupMode = startupMode}, tlsPolicy = configuredTlsPolicy}, Nothing) ->
      let (certificatePath, privateKeyPath) = sharedCertificatePaths sharedDirectory
       in Right
            [ PlannedManualTls
                ManualTlsBindPlan
                  { tlsEndpoint = listenerEndpoint listenerConfig,
                    tlsCertificateFile = certificatePath,
                    tlsPrivateKeyFile = privateKeyPath,
                    tlsCredentialSourceKind = SharedTlsCredentials,
                    tlsStartupMode = startupMode,
                    tlsBindPolicy = configuredTlsPolicy
                  }
            ]
    (Https, Just TlsConfig {certificateSource = AcmeCertificateSource acmeConfig, tlsPolicy = configuredTlsPolicy}, Nothing) ->
      Right
        [ PlannedAcme
            AcmeBindPlan
              { acmeEndpoint = listenerEndpoint listenerConfig,
                acmeTlsEndpoint = Just (listenerEndpoint listenerConfig),
                acmeListenerConfig = acmeConfig,
                acmeTlsPolicy = configuredTlsPolicy
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
