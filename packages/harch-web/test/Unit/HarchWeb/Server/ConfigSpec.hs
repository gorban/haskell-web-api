{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Concurrent ()
import Control.Exception ()
import Control.Monad (forM_)
import Data.ByteString qualified as ByteString ()
import Data.ByteString.Builder qualified as Builder ()
import Data.ByteString.Char8 qualified as ByteStringChar8 ()
import Data.ByteString.Lazy qualified as LazyByteString ()
import Data.Char ()
import Data.Either ()
import Data.Functor.Compose ()
import Data.IORef ()
import Data.List ()
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe ()
import Data.Text ()
import Data.Text qualified as Text ()
import Data.Text.Encoding qualified as TextEncoding ()
import HarchWeb (AcmeBindPlan (AcmeBindPlan, acmeEndpoint, acmeListenerConfig, acmeTlsEndpoint, acmeTlsPolicy), AcmeConfig (AcmeConfig, acmeCertbotConfig, acmeCertificateDirectory, acmeContactEmails, acmeDirectoryUrl, acmeDomains, acmeHttp01Port), CertbotConfig (CertbotConfig, certbotArguments, certbotExecutable), CorsPolicyConfig (corsAllowedOrigins, corsMaxAgeSeconds), ForwardedHeaderTrust (NeverTrustForwarded), HasServerConfig (toServerConfig), HttpBindPlan (HttpBindPlan, httpEndpoints), ListenerConfig (ListenerConfig, listenerAcme, listenerHost, listenerPort, listenerScheme, listenerTls), ListenerEndpoint (ListenerEndpoint, endpointHost, endpointPort), ListenerScheme (Http, Https), ListenerStartupError (DuplicateListenerEndpoint, InvalidListenerAcmeConfiguration, InvalidListenerTlsConfiguration), ManualTlsBindPlan (ManualTlsBindPlan, tlsBindPolicy, tlsCertificateFile, tlsCredentialSourceKind, tlsEndpoint, tlsPrivateKeyFile, tlsStartupMode), ManualTlsCertificateFiles (ManualTlsCertificateFiles, certificateFile, privateKeyFile), ObservabilityConfig (ObservabilityConfig, metricsExporter, tracingExporter), ObservabilityStartupPlan (ObservabilityStartupPlan, startupExporters), OtlpExporter (OtlpExporter, otlpEndpoint, otlpHeaders), OtlpExporterStartup (OtlpExporterStartup, startupEndpoint, startupHeaders, startupSignal), RequestPolicyConfig (RequestPolicyConfig, corsPolicy, forwardedHeaderTrust, httpsRedirectAuthority, httpsRedirectPort, redirectHttpToHttps, requestConcurrencyLimit, requestHeadLimits, requestTransportLimits, responseSecurityHeaders, strictTransportSecurity), ResponseSecurityHeadersConfig (contentSecurityPolicy, frameOptions), ServerConfig (ServerConfig, listenerConfigs, observability, requestPolicy, staticAssets), ServerStartupPlan (ServerStartupPlan, acmeBindPlans, httpBindPlan, manualTlsBindPlans), SharedTlsCertificateFiles (SharedTlsCertificateFiles, certificateDirectory, sharedCertificateStartupMode), StaticAssetRoot (StaticAssetRoot, staticDirectory, staticUrlPrefix), StaticAssetsConfig (StaticAssetsConfig, staticAssetContentTypes, staticAssetRoots, staticCacheControlSeconds), StrictTransportSecurityConfig (StrictTransportSecurityConfig, strictTransportSecurityIncludeSubDomains, strictTransportSecurityMaxAgeSeconds, strictTransportSecurityPreload), TelemetrySignal (MetricsSignal, TracingSignal), TlsCertificateSource (AcmeCertificateSource, ManualCertificateFiles, SharedCertificateFiles), TlsCipherSuite (..), TlsConfig (TlsConfig, certificateSource, tlsPolicy), TlsCredentialSourceKind (ManualTlsCredentials, SharedTlsCredentials), TlsPolicy (TlsPolicy, tlsAllowedVersions, tlsCipherSuites), TlsProtocolVersion (..), TlsStartupMode (AwaitCertificateFiles, RequireCertificateFiles), defaultCorsPolicyConfig, defaultResponseSecurityHeadersConfig, defaultStaticAssetContentTypes, defaultTlsPolicy, planServerStartup, tlsCipherSuiteFromIdentifier, tlsCipherSuiteValue, tlsPolicySupports, tlsProtocolVersionValue, unboundedRequestHeadLimits, warpDefaultRequestTransportLimits)
import HarchWeb.Action qualified as Action ()
import HarchWeb.Database qualified as Database ()
import HarchWeb.Markup.Unsafe qualified as MarkupUnsafe ()
import HarchWeb.Observability qualified as Observability ()
import HarchWeb.Security qualified as Security ()
import Network.HTTP.Client qualified as HttpClient ()
import Network.HTTP.Types qualified as Http ()
import Network.Socket qualified as Socket ()
import Network.Socket.ByteString qualified as SocketByteString ()
import Network.TLS qualified as TLS
import Network.Wai qualified as Wai ()
import Network.Wai.Handler.Warp qualified as Warp ()
import System.Directory ()
import System.Environment ()
import System.Exit ()
import System.FilePath ()
import System.IO ()
import System.IO.Error ()
import System.IO.Temp ()
import System.Posix.Signals ()
import System.Process ()
import TestCore.CustomAssertions ()
import TestCore.Wai ()
import Text.Read ()
import Unit.HarchWeb.TestSupport (certbotHttp01Backend, serverConfigWithListeners)

spec = do
  describe "TLS policy vocabulary" $ do
    it "resolves every documented cipher identifier to an installed cipher usable by a supported protocol" $ do
      let supportedCipherSuites =
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
          supportedVersions = [Tls10, Tls11, Tls12, Tls13]
      forM_ supportedCipherSuites $ \(identifier, cipherSuite) -> do
        tlsCipherSuiteFromIdentifier identifier `shouldBe` Just cipherSuite
        any (\tlsVersion -> TLS.cipherAllowedForVersion (tlsProtocolVersionValue tlsVersion) (tlsCipherSuiteValue cipherSuite)) supportedVersions `shouldBe` True
      show supportedVersions `shouldBe` "[Tls10,Tls11,Tls12,Tls13]"
      show Tls10 `shouldBe` "Tls10"
      show (map snd supportedCipherSuites) `shouldContain` "Tls13Aes128CcmSha256"
      show Tls13Aes128CcmSha256 `shouldBe` "Tls13Aes128CcmSha256"
      Tls10 `shouldNotBe` Tls11
      TlsEcdheRsaAes256CbcSha `shouldNotBe` Tls13Aes256GcmSha384
      tlsPolicySupports (TlsPolicy {tlsAllowedVersions = Tls10 :| [], tlsCipherSuites = TlsEcdheRsaAes256CbcSha :| []}) `shouldBe` True
      tlsPolicySupports (TlsPolicy {tlsAllowedVersions = Tls10 :| [], tlsCipherSuites = Tls13Aes256GcmSha384 :| []}) `shouldBe` False
      let legacyTlsPolicy = TlsPolicy {tlsAllowedVersions = Tls10 :| [], tlsCipherSuites = TlsEcdheRsaAes256CbcSha :| []}
      legacyTlsPolicy `shouldNotBe` TlsPolicy {tlsAllowedVersions = Tls13 :| [], tlsCipherSuites = Tls13Aes256GcmSha384 :| []}
      show legacyTlsPolicy `shouldContain` "TlsEcdheRsaAes256CbcSha"
      show [legacyTlsPolicy] `shouldContain` "TlsEcdheRsaAes256CbcSha"

  describe "shared config coverage" $ do
    it "reads exported selectors from the shared server config records" $ do
      let certbotConfig = CertbotConfig {certbotExecutable = "certbot", certbotArguments = ["certonly", "--webroot"]}
          challengeBackend = certbotConfig
          acmeConfig =
            AcmeConfig
              { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                acmeContactEmails = ["ops@example.com"],
                acmeDomains = ["example.com", "www.example.com"],
                acmeHttp01Port = 80,
                acmeCertificateDirectory = Nothing,
                acmeCertbotConfig = challengeBackend
              }
          sharedCertificateSource =
            SharedCertificateFiles
              SharedTlsCertificateFiles
                { certificateDirectory = "/var/lib/harch-web/shared-certs",
                  sharedCertificateStartupMode = AwaitCertificateFiles Nothing
                }
          tlsSource = AcmeCertificateSource acmeConfig
          tlsConfig = TlsConfig {certificateSource = tlsSource, tlsPolicy = defaultTlsPolicy}
          staticRoot = StaticAssetRoot {staticUrlPrefix = "/assets", staticDirectory = "public"}
          tracingConfig =
            OtlpExporter
              { otlpEndpoint = "http://collector:4318/v1/traces",
                otlpHeaders = [("authorization", "Bearer token")]
              }
          observabilityConfig =
            ObservabilityConfig
              { tracingExporter = Just tracingConfig,
                metricsExporter = Nothing
              }
          observabilityStartup =
            OtlpExporterStartup
              { startupSignal = TracingSignal,
                startupEndpoint = "http://collector:4318/v1/traces",
                startupHeaders = [("authorization", "Bearer token")]
              }
          observabilityPlan =
            ObservabilityStartupPlan
              { startupExporters = [observabilityStartup]
              }
          strictTransportSecurityConfig =
            StrictTransportSecurityConfig
              { strictTransportSecurityMaxAgeSeconds = 31536000,
                strictTransportSecurityIncludeSubDomains = True,
                strictTransportSecurityPreload = True
              }
          requestPolicyConfig =
            RequestPolicyConfig
              { redirectHttpToHttps = True,
                httpsRedirectPort = Just 5443,
                httpsRedirectAuthority = Just "app.example.com",
                strictTransportSecurity = Just strictTransportSecurityConfig,
                forwardedHeaderTrust = NeverTrustForwarded,
                requestHeadLimits = unboundedRequestHeadLimits,
                requestTransportLimits = warpDefaultRequestTransportLimits,
                requestConcurrencyLimit = Nothing,
                corsPolicy = defaultCorsPolicyConfig,
                responseSecurityHeaders = defaultResponseSecurityHeadersConfig
              }
          serverConfig =
            ServerConfig
              { listenerConfigs =
                  [ ListenerConfig
                      { listenerHost = "127.0.0.1",
                        listenerPort = 5001,
                        listenerScheme = Https,
                        listenerTls = Just tlsConfig,
                        listenerAcme = Nothing
                      }
                  ],
                staticAssets =
                  StaticAssetsConfig
                    { staticAssetRoots = [staticRoot],
                      staticAssetContentTypes = defaultStaticAssetContentTypes,
                      staticCacheControlSeconds = Just 3600
                    },
                requestPolicy = requestPolicyConfig,
                observability = observabilityConfig
              }
          listenerConfig =
            case listenerConfigs serverConfig of
              [singleListenerConfig] -> singleListenerConfig
              _ -> error "expected exactly one listener config"
      certbotExecutable certbotConfig `shouldBe` "certbot"
      certbotArguments certbotConfig `shouldBe` ["certonly", "--webroot"]
      acmeDirectoryUrl acmeConfig `shouldBe` "https://acme-v02.api.letsencrypt.org/directory"
      acmeContactEmails acmeConfig `shouldBe` ["ops@example.com"]
      acmeDomains acmeConfig `shouldBe` ["example.com", "www.example.com"]
      acmeCertificateDirectory acmeConfig `shouldBe` Nothing
      acmeCertbotConfig acmeConfig `shouldBe` challengeBackend
      case sharedCertificateSource of
        SharedCertificateFiles SharedTlsCertificateFiles {certificateDirectory = sharedDirectory, sharedCertificateStartupMode = startupMode} -> do
          sharedDirectory `shouldBe` "/var/lib/harch-web/shared-certs"
          startupMode `shouldBe` AwaitCertificateFiles Nothing
        _ ->
          expectationFailure "expected shared certificate files"
      certificateSource tlsConfig `shouldBe` tlsSource
      listenerHost listenerConfig `shouldBe` "127.0.0.1"
      listenerPort listenerConfig `shouldBe` 5001
      listenerScheme listenerConfig `shouldBe` Https
      listenerTls listenerConfig `shouldBe` Just tlsConfig
      staticUrlPrefix staticRoot `shouldBe` "/assets"
      staticDirectory staticRoot `shouldBe` "public"
      staticAssetRoots (staticAssets serverConfig) `shouldBe` [staticRoot]
      staticCacheControlSeconds (staticAssets serverConfig) `shouldBe` Just 3600
      redirectHttpToHttps requestPolicyConfig `shouldBe` True
      httpsRedirectPort requestPolicyConfig `shouldBe` Just 5443
      httpsRedirectAuthority requestPolicyConfig `shouldBe` Just "app.example.com"
      strictTransportSecurity requestPolicyConfig `shouldBe` Just strictTransportSecurityConfig
      corsPolicy requestPolicyConfig `shouldBe` defaultCorsPolicyConfig
      responseSecurityHeaders requestPolicyConfig `shouldBe` defaultResponseSecurityHeadersConfig
      strictTransportSecurityMaxAgeSeconds strictTransportSecurityConfig `shouldBe` 31536000
      strictTransportSecurityIncludeSubDomains strictTransportSecurityConfig `shouldBe` True
      strictTransportSecurityPreload strictTransportSecurityConfig `shouldBe` True
      requestPolicy serverConfig `shouldBe` requestPolicyConfig
      otlpEndpoint tracingConfig `shouldBe` "http://collector:4318/v1/traces"
      otlpHeaders tracingConfig `shouldBe` [("authorization", "Bearer token")]
      tracingExporter observabilityConfig `shouldBe` Just tracingConfig
      metricsExporter observabilityConfig `shouldBe` Nothing
      observability serverConfig `shouldBe` observabilityConfig
      startupSignal observabilityStartup `shouldBe` TracingSignal
      startupEndpoint observabilityStartup `shouldBe` "http://collector:4318/v1/traces"
      startupHeaders observabilityStartup `shouldBe` [("authorization", "Bearer token")]
      startupExporters observabilityPlan `shouldBe` [observabilityStartup]
      toServerConfig serverConfig `shouldBe` serverConfig

    it "covers derived Eq and Show instances for the shared server config types" $ do
      let shouldBeParenthesized rendered = do
            case rendered of
              '(' : rest ->
                case reverse rest of
                  ')' : _ -> pure ()
                  _ -> expectationFailure "expected parenthesized rendering"
              _ -> expectationFailure "expected parenthesized rendering"
          certbotConfig = CertbotConfig {certbotExecutable = "certbot", certbotArguments = ["certonly", "--webroot"]}
          otherCertbotConfig = CertbotConfig {certbotExecutable = "certbot", certbotArguments = ["renew"]}
          strictTransportSecurityConfig =
            StrictTransportSecurityConfig
              { strictTransportSecurityMaxAgeSeconds = 31536000,
                strictTransportSecurityIncludeSubDomains = True,
                strictTransportSecurityPreload = True
              }
          otherStrictTransportSecurityConfig =
            StrictTransportSecurityConfig
              { strictTransportSecurityMaxAgeSeconds = 60,
                strictTransportSecurityIncludeSubDomains = False,
                strictTransportSecurityPreload = False
              }
          corsPolicyConfig =
            defaultCorsPolicyConfig
              { corsAllowedOrigins = ["https://client.example.com"],
                corsMaxAgeSeconds = Just 600
              }
          otherCorsPolicyConfig =
            defaultCorsPolicyConfig
              { corsAllowedOrigins = ["https://admin.example.com"]
              }
          responseSecurityHeadersConfig =
            defaultResponseSecurityHeadersConfig
              { frameOptions = Just "SAMEORIGIN"
              }
          otherResponseSecurityHeadersConfig =
            defaultResponseSecurityHeadersConfig
              { contentSecurityPolicy = Just "default-src 'none'"
              }
          requestPolicyConfig =
            RequestPolicyConfig
              { redirectHttpToHttps = True,
                httpsRedirectPort = Just 5443,
                httpsRedirectAuthority = Just "app.example.com",
                strictTransportSecurity = Just strictTransportSecurityConfig,
                forwardedHeaderTrust = NeverTrustForwarded,
                requestHeadLimits = unboundedRequestHeadLimits,
                requestTransportLimits = warpDefaultRequestTransportLimits,
                requestConcurrencyLimit = Nothing,
                corsPolicy = corsPolicyConfig,
                responseSecurityHeaders = responseSecurityHeadersConfig
              }
          otherRequestPolicyConfig =
            RequestPolicyConfig
              { redirectHttpToHttps = False,
                httpsRedirectPort = Nothing,
                httpsRedirectAuthority = Just "other.example.com",
                strictTransportSecurity = Just otherStrictTransportSecurityConfig,
                forwardedHeaderTrust = NeverTrustForwarded,
                requestHeadLimits = unboundedRequestHeadLimits,
                requestTransportLimits = warpDefaultRequestTransportLimits,
                requestConcurrencyLimit = Nothing,
                corsPolicy =
                  defaultCorsPolicyConfig
                    { corsAllowedOrigins = ["https://app.example.com"]
                    },
                responseSecurityHeaders =
                  defaultResponseSecurityHeadersConfig
                    { frameOptions = Just "SAMEORIGIN"
                    }
              }
          acmeConfig =
            AcmeConfig
              { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                acmeContactEmails = ["ops@example.com"],
                acmeDomains = ["example.com", "www.example.com"],
                acmeHttp01Port = 80,
                acmeCertificateDirectory = Nothing,
                acmeCertbotConfig = certbotConfig
              }
          otherAcmeConfig =
            AcmeConfig
              { acmeDirectoryUrl = "https://acme-staging-v02.api.letsencrypt.org/directory",
                acmeContactEmails = ["ops@example.com"],
                acmeDomains = ["staging.example.com"],
                acmeHttp01Port = 80,
                acmeCertificateDirectory = Just "/var/lib/harch-web/staging-certs",
                acmeCertbotConfig = otherCertbotConfig
              }
          manualCertificateSource = ManualCertificateFiles ManualTlsCertificateFiles {certificateFile = "cert.pem", privateKeyFile = "key.pem"}
          sharedCertificateSource =
            SharedCertificateFiles
              SharedTlsCertificateFiles
                { certificateDirectory = "/var/lib/harch-web/shared-certs",
                  sharedCertificateStartupMode = AwaitCertificateFiles Nothing
                }
          acmeCertificateSource = AcmeCertificateSource acmeConfig
          tlsConfig = TlsConfig {certificateSource = acmeCertificateSource, tlsPolicy = defaultTlsPolicy}
          listenerConfig =
            ListenerConfig
              { listenerHost = "127.0.0.1",
                listenerPort = 5001,
                listenerScheme = Https,
                listenerTls = Just tlsConfig,
                listenerAcme = Nothing
              }
          otherListenerConfig =
            ListenerConfig
              { listenerHost = "0.0.0.0",
                listenerPort = 5443,
                listenerScheme = Https,
                listenerTls = Just (TlsConfig {certificateSource = manualCertificateSource, tlsPolicy = defaultTlsPolicy}),
                listenerAcme = Nothing
              }
          httpAcmeListenerConfig =
            ListenerConfig
              { listenerHost = "0.0.0.0",
                listenerPort = 80,
                listenerScheme = Http,
                listenerTls = Nothing,
                listenerAcme = Just otherAcmeConfig
              }
          staticRoot = StaticAssetRoot {staticUrlPrefix = "/assets", staticDirectory = "public"}
          staticAssetsConfig =
            StaticAssetsConfig
              { staticAssetRoots = [staticRoot],
                staticAssetContentTypes = defaultStaticAssetContentTypes,
                staticCacheControlSeconds = Just 3600
              }
          tracingConfig =
            OtlpExporter
              { otlpEndpoint = "http://collector:4318/v1/traces",
                otlpHeaders = [("authorization", "Bearer token")]
              }
          otherTracingConfig = OtlpExporter {otlpEndpoint = "http://other-collector:4318/v1/traces", otlpHeaders = []}
          observabilityConfig = ObservabilityConfig {tracingExporter = Just tracingConfig, metricsExporter = Nothing}
          exporterStartup =
            OtlpExporterStartup
              { startupSignal = TracingSignal,
                startupEndpoint = "http://collector:4318/v1/traces",
                startupHeaders = [("authorization", "Bearer token")]
              }
          otherExporterStartup =
            OtlpExporterStartup
              { startupSignal = MetricsSignal,
                startupEndpoint = "http://collector:4318/v1/metrics",
                startupHeaders = []
              }
          observabilityPlan = ObservabilityStartupPlan {startupExporters = [exporterStartup]}
          serverConfig =
            ServerConfig
              { listenerConfigs = [listenerConfig],
                staticAssets = staticAssetsConfig,
                requestPolicy = requestPolicyConfig,
                observability = observabilityConfig
              }
      Http `shouldNotBe` Https
      certbotConfig `shouldNotBe` otherCertbotConfig
      strictTransportSecurityConfig `shouldNotBe` otherStrictTransportSecurityConfig
      corsPolicyConfig `shouldNotBe` otherCorsPolicyConfig
      responseSecurityHeadersConfig `shouldNotBe` otherResponseSecurityHeadersConfig
      requestPolicyConfig `shouldNotBe` otherRequestPolicyConfig
      acmeConfig `shouldNotBe` otherAcmeConfig
      manualCertificateSource `shouldNotBe` acmeCertificateSource
      sharedCertificateSource `shouldNotBe` manualCertificateSource
      sharedCertificateSource `shouldNotBe` acmeCertificateSource
      acmeCertificateSource `shouldNotBe` AcmeCertificateSource otherAcmeConfig
      tlsConfig `shouldNotBe` TlsConfig {certificateSource = manualCertificateSource, tlsPolicy = defaultTlsPolicy}
      tlsConfig `shouldNotBe` TlsConfig {certificateSource = sharedCertificateSource, tlsPolicy = defaultTlsPolicy}
      listenerConfig `shouldNotBe` otherListenerConfig
      httpAcmeListenerConfig `shouldNotBe` listenerConfig
      staticRoot `shouldNotBe` StaticAssetRoot {staticUrlPrefix = "/static", staticDirectory = "public"}
      staticAssetsConfig
        `shouldNotBe` StaticAssetsConfig
          { staticAssetRoots = [],
            staticAssetContentTypes = defaultStaticAssetContentTypes,
            staticCacheControlSeconds = Nothing
          }
      tracingConfig `shouldNotBe` otherTracingConfig
      observabilityConfig `shouldNotBe` ObservabilityConfig {tracingExporter = Nothing, metricsExporter = Nothing}
      ManualTlsCredentials `shouldNotBe` SharedTlsCredentials
      AwaitCertificateFiles Nothing `shouldNotBe` RequireCertificateFiles
      TracingSignal `shouldNotBe` MetricsSignal
      exporterStartup `shouldNotBe` otherExporterStartup
      observabilityPlan `shouldNotBe` ObservabilityStartupPlan {startupExporters = []}
      serverConfig `shouldNotBe` serverConfig {listenerConfigs = [otherListenerConfig]}
      show Http `shouldBe` "Http"
      show Https `shouldBe` "Https"
      show certbotConfig `shouldBe` "CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = <redacted: 2>}"
      show strictTransportSecurityConfig `shouldBe` "StrictTransportSecurityConfig {strictTransportSecurityMaxAgeSeconds = 31536000, strictTransportSecurityIncludeSubDomains = True, strictTransportSecurityPreload = True}"
      show corsPolicyConfig `shouldBe` "CorsPolicyConfig {corsAllowedOrigins = [\"https://client.example.com\"], corsAllowedMethods = [\"GET\",\"HEAD\",\"OPTIONS\"], corsAllowedHeaders = [\"Content-Type\",\"X-Requested-With\"], corsMaxAgeSeconds = Just 600}"
      show responseSecurityHeadersConfig `shouldContain` "ResponseSecurityHeadersConfig {contentSecurityPolicy = Just \"default-src 'self'; base-uri 'self'; object-src 'none'; frame-ancestors 'none'; form-action 'self'; script-src 'self'; style-src 'self'; img-src 'self' data:; font-src 'self'; connect-src 'self'\""
      show requestPolicyConfig `shouldContain` "corsPolicy = CorsPolicyConfig {corsAllowedOrigins = [\"https://client.example.com\"], corsAllowedMethods = [\"GET\",\"HEAD\",\"OPTIONS\"], corsAllowedHeaders = [\"Content-Type\",\"X-Requested-With\"], corsMaxAgeSeconds = Just 600}"
      show requestPolicyConfig `shouldContain` "responseSecurityHeaders = ResponseSecurityHeadersConfig {contentSecurityPolicy = Just \"default-src 'self'; base-uri 'self'; object-src 'none'; frame-ancestors 'none'; form-action 'self'; script-src 'self'; style-src 'self'; img-src 'self' data:; font-src 'self'; connect-src 'self'\""
      show certbotConfig `shouldBe` "CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = <redacted: 2>}"
      show acmeConfig `shouldContain` "certbotArguments = <redacted: 2>"
      show manualCertificateSource `shouldBe` "ManualCertificateFiles (ManualTlsCertificateFiles {certificateFile = \"cert.pem\", privateKeyFile = \"key.pem\"})"
      show sharedCertificateSource `shouldBe` "SharedCertificateFiles (SharedTlsCertificateFiles {certificateDirectory = \"/var/lib/harch-web/shared-certs\", sharedCertificateStartupMode = AwaitCertificateFiles Nothing})"
      -- 'deriving (Eq, Show)' on 'ManualTlsCertificateFiles'/'SharedTlsCertificateFiles'
      -- themselves is only ever reached indirectly above, through the outer
      -- 'TlsCertificateSource' constructor's own derived instances; HPC does
      -- not credit those two declarations from that alone, confirmed
      -- directly by the coverage gate rather than assumed, so both are
      -- exercised here too, directly and on their own. Same-value,
      -- different-construction (not 'x == x') per this codebase's own
      -- derived-instance coverage lesson.
      ManualTlsCertificateFiles {certificateFile = "cert.pem", privateKeyFile = "key.pem"}
        `shouldBe` ManualTlsCertificateFiles {certificateFile = "cert.pem", privateKeyFile = "key.pem"}
      SharedTlsCertificateFiles {certificateDirectory = "/var/lib/harch-web/shared-certs", sharedCertificateStartupMode = AwaitCertificateFiles Nothing}
        `shouldBe` SharedTlsCertificateFiles {certificateDirectory = "/var/lib/harch-web/shared-certs", sharedCertificateStartupMode = AwaitCertificateFiles Nothing}
      -- 'deriving (Eq)' writes only '=='; the unoverridden '/=' default
      -- method HPC boxes separately (this codebase's own established
      -- derived-instance lesson), so a genuine inequality is exercised too.
      ManualTlsCertificateFiles {certificateFile = "cert.pem", privateKeyFile = "key.pem"}
        `shouldNotBe` ManualTlsCertificateFiles {certificateFile = "other.pem", privateKeyFile = "key.pem"}
      SharedTlsCertificateFiles {certificateDirectory = "/var/lib/harch-web/shared-certs", sharedCertificateStartupMode = AwaitCertificateFiles Nothing}
        `shouldNotBe` SharedTlsCertificateFiles {certificateDirectory = "/var/lib/harch-web/other-certs", sharedCertificateStartupMode = AwaitCertificateFiles Nothing}
      shouldBeParenthesized (showsPrec 11 (ManualTlsCertificateFiles {certificateFile = "cert.pem", privateKeyFile = "key.pem"}) "")
      shouldBeParenthesized (showsPrec 11 (SharedTlsCertificateFiles {certificateDirectory = "/var/lib/harch-web/shared-certs", sharedCertificateStartupMode = AwaitCertificateFiles Nothing}) "")
      show (ManualTlsCertificateFiles {certificateFile = "cert.pem", privateKeyFile = "key.pem"})
        `shouldBe` "ManualTlsCertificateFiles {certificateFile = \"cert.pem\", privateKeyFile = \"key.pem\"}"
      show (SharedTlsCertificateFiles {certificateDirectory = "/var/lib/harch-web/shared-certs", sharedCertificateStartupMode = AwaitCertificateFiles Nothing})
        `shouldBe` "SharedTlsCertificateFiles {certificateDirectory = \"/var/lib/harch-web/shared-certs\", sharedCertificateStartupMode = AwaitCertificateFiles Nothing}"
      -- Derived 'Show' also writes a 'showList' method (used to render a
      -- list of these values), a third box distinct from 'show'/'showsPrec'.
      show [ManualTlsCertificateFiles {certificateFile = "cert.pem", privateKeyFile = "key.pem"}]
        `shouldBe` "[ManualTlsCertificateFiles {certificateFile = \"cert.pem\", privateKeyFile = \"key.pem\"}]"
      show [SharedTlsCertificateFiles {certificateDirectory = "/var/lib/harch-web/shared-certs", sharedCertificateStartupMode = AwaitCertificateFiles Nothing}]
        `shouldBe` "[SharedTlsCertificateFiles {certificateDirectory = \"/var/lib/harch-web/shared-certs\", sharedCertificateStartupMode = AwaitCertificateFiles Nothing}]"
      show acmeCertificateSource `shouldContain` "certbotArguments = <redacted: 2>"
      show (TlsConfig {certificateSource = manualCertificateSource, tlsPolicy = defaultTlsPolicy}) `shouldBe` "TlsConfig {certificateSource = ManualCertificateFiles (ManualTlsCertificateFiles {certificateFile = \"cert.pem\", privateKeyFile = \"key.pem\"}), tlsPolicy = TlsPolicy {tlsAllowedVersions = Tls12 :| [Tls13], tlsCipherSuites = TlsEcdheEcdsaAes256GcmSha384 :| [TlsEcdheEcdsaChacha20Poly1305Sha256,TlsEcdheEcdsaAes128GcmSha256,TlsEcdheRsaAes256GcmSha384,TlsEcdheRsaChacha20Poly1305Sha256,TlsEcdheRsaAes128GcmSha256,Tls13Aes256GcmSha384,Tls13Chacha20Poly1305Sha256,Tls13Aes128GcmSha256]}}"
      show listenerConfig `shouldContain` "certbotArguments = <redacted: 2>"
      show httpAcmeListenerConfig `shouldContain` "certbotArguments = <redacted: 1>"
      show staticRoot `shouldBe` "StaticAssetRoot {staticUrlPrefix = \"/assets\", staticDirectory = \"public\"}"
      show staticAssetsConfig
        `shouldBe` ( "StaticAssetsConfig {staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = \"/assets\", staticDirectory = \"public\"}], staticAssetContentTypes = "
                       <> show defaultStaticAssetContentTypes
                       <> ", staticCacheControlSeconds = Just 3600}"
                   )
      show tracingConfig `shouldBe` "OtlpExporter {otlpEndpoint = <configured>, otlpHeaders = <redacted: 1>}"
      show observabilityConfig `shouldContain` "otlpHeaders = <redacted: 1>"
      show ManualTlsCredentials `shouldBe` "ManualTlsCredentials"
      show (AwaitCertificateFiles (Just 15)) `shouldBe` "AwaitCertificateFiles (Just 15)"
      show TracingSignal `shouldBe` "TracingSignal"
      show exporterStartup `shouldBe` "OtlpExporterStartup {startupSignal = TracingSignal, startupEndpoint = <configured>, startupHeaders = <redacted: 1>}"
      show observabilityPlan `shouldContain` "startupHeaders = <redacted: 1>"
      show serverConfig `shouldContain` ("staticAssetContentTypes = " <> show defaultStaticAssetContentTypes)
      shouldBeParenthesized (showsPrec 11 certbotConfig "")
      shouldBeParenthesized (showsPrec 11 strictTransportSecurityConfig "")
      shouldBeParenthesized (showsPrec 11 corsPolicyConfig "")
      shouldBeParenthesized (showsPrec 11 responseSecurityHeadersConfig "")
      shouldBeParenthesized (showsPrec 11 requestPolicyConfig "")
      shouldBeParenthesized (showsPrec 11 certbotConfig "")
      shouldBeParenthesized (showsPrec 11 acmeConfig "")
      shouldBeParenthesized (showsPrec 11 manualCertificateSource "")
      shouldBeParenthesized (showsPrec 11 sharedCertificateSource "")
      shouldBeParenthesized (showsPrec 11 acmeCertificateSource "")
      shouldBeParenthesized (showsPrec 11 tlsConfig "")
      shouldBeParenthesized (showsPrec 11 listenerConfig "")
      shouldBeParenthesized (showsPrec 11 httpAcmeListenerConfig "")
      shouldBeParenthesized (showsPrec 11 staticRoot "")
      shouldBeParenthesized (showsPrec 11 staticAssetsConfig "")
      shouldBeParenthesized (showsPrec 11 tracingConfig "")
      shouldBeParenthesized (showsPrec 11 observabilityConfig "")
      shouldBeParenthesized (showsPrec 11 exporterStartup "")
      shouldBeParenthesized (showsPrec 11 observabilityPlan "")
      shouldBeParenthesized (showsPrec 11 serverConfig "")
      show [Http, Https] `shouldBe` "[Http,Https]"
      show [ManualTlsCredentials, SharedTlsCredentials] `shouldBe` "[ManualTlsCredentials,SharedTlsCredentials]"
      show [RequireCertificateFiles, AwaitCertificateFiles Nothing] `shouldBe` "[RequireCertificateFiles,AwaitCertificateFiles Nothing]"
      show [certbotConfig] `shouldBe` "[CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = <redacted: 2>}]"
      show [strictTransportSecurityConfig] `shouldBe` "[StrictTransportSecurityConfig {strictTransportSecurityMaxAgeSeconds = 31536000, strictTransportSecurityIncludeSubDomains = True, strictTransportSecurityPreload = True}]"
      show [corsPolicyConfig] `shouldBe` "[CorsPolicyConfig {corsAllowedOrigins = [\"https://client.example.com\"], corsAllowedMethods = [\"GET\",\"HEAD\",\"OPTIONS\"], corsAllowedHeaders = [\"Content-Type\",\"X-Requested-With\"], corsMaxAgeSeconds = Just 600}]"
      show [responseSecurityHeadersConfig] `shouldContain` "[ResponseSecurityHeadersConfig {contentSecurityPolicy = Just \"default-src 'self'; base-uri 'self'; object-src 'none'; frame-ancestors 'none'; form-action 'self'; script-src 'self'; style-src 'self'; img-src 'self' data:; font-src 'self'; connect-src 'self'\""
      show [requestPolicyConfig] `shouldContain` "RequestPolicyConfig {redirectHttpToHttps = True, httpsRedirectPort = Just 5443, httpsRedirectAuthority = Just \"app.example.com\", strictTransportSecurity = Just (StrictTransportSecurityConfig {strictTransportSecurityMaxAgeSeconds = 31536000, strictTransportSecurityIncludeSubDomains = True, strictTransportSecurityPreload = True}), forwardedHeaderTrust = NeverTrustForwarded"
      show [certbotConfig] `shouldBe` "[CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = <redacted: 2>}]"
      show [acmeConfig] `shouldContain` "certbotArguments = <redacted: 2>"
      show [manualCertificateSource, sharedCertificateSource, acmeCertificateSource] `shouldContain` "certbotArguments = <redacted: 2>"
      show [tlsConfig] `shouldContain` "certbotArguments = <redacted: 2>"
      show [listenerConfig] `shouldContain` "certbotArguments = <redacted: 2>"
      show [httpAcmeListenerConfig] `shouldContain` "certbotArguments = <redacted: 1>"
      show [staticRoot] `shouldBe` "[StaticAssetRoot {staticUrlPrefix = \"/assets\", staticDirectory = \"public\"}]"
      show [staticAssetsConfig]
        `shouldBe` ( "[StaticAssetsConfig {staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = \"/assets\", staticDirectory = \"public\"}], staticAssetContentTypes = "
                       <> show defaultStaticAssetContentTypes
                       <> ", staticCacheControlSeconds = Just 3600}]"
                   )
      show [tracingConfig] `shouldBe` "[OtlpExporter {otlpEndpoint = <configured>, otlpHeaders = <redacted: 1>}]"
      show [observabilityConfig] `shouldContain` "otlpHeaders = <redacted: 1>"
      show [TracingSignal, MetricsSignal] `shouldBe` "[TracingSignal,MetricsSignal]"
      show [exporterStartup] `shouldBe` "[OtlpExporterStartup {startupSignal = TracingSignal, startupEndpoint = <configured>, startupHeaders = <redacted: 1>}]"
      show [observabilityPlan] `shouldContain` "startupHeaders = <redacted: 1>"
      show [serverConfig] `shouldContain` ("staticAssetContentTypes = " <> show defaultStaticAssetContentTypes)

  describe "planServerStartup" $ do
    it "redacts Certbot argv from direct and nested diagnostics" $ do
      let secretArgument = "certbot-argv-sentinel" :: String
          certbotConfig = CertbotConfig {certbotExecutable = "certbot", certbotArguments = ["certbot-argv-sentinel"]}
          acmeConfig =
            AcmeConfig
              { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                acmeContactEmails = ["ops@example.com"],
                acmeDomains = ["example.com"],
                acmeHttp01Port = 80,
                acmeCertificateDirectory = Nothing,
                acmeCertbotConfig = certbotConfig
              }
      expectAll
        ( (show certbotConfig `shouldNotContain` secretArgument)
            :| [ show [certbotConfig] `shouldNotContain` secretArgument,
                 show acmeConfig `shouldNotContain` secretArgument,
                 show acmeConfig `shouldContain` "certbotArguments = <redacted: 1>"
               ]
        )

    it "groups HTTP listeners into the expected bind plan" $ do
      let firstEndpoint = ListenerEndpoint {endpointHost = "127.0.0.1", endpointPort = 5001}
          secondEndpoint = ListenerEndpoint {endpointHost = "0.0.0.0", endpointPort = 5002}
          firstListener = ListenerConfig {listenerHost = endpointHost firstEndpoint, listenerPort = endpointPort firstEndpoint, listenerScheme = Http, listenerTls = Nothing, listenerAcme = Nothing}
          secondListener = ListenerConfig {listenerHost = endpointHost secondEndpoint, listenerPort = endpointPort secondEndpoint, listenerScheme = Http, listenerTls = Nothing, listenerAcme = Nothing}
          httpBindPlan = HttpBindPlan {httpEndpoints = [firstEndpoint, secondEndpoint]}
          startupPlan =
            ServerStartupPlan
              { httpBindPlan = httpBindPlan,
                manualTlsBindPlans = [],
                acmeBindPlans = []
              }
      planServerStartup (serverConfigWithListeners [firstListener, secondListener]) `shouldBe` Right startupPlan
      firstEndpoint `shouldNotBe` secondEndpoint
      httpBindPlan `shouldNotBe` HttpBindPlan {httpEndpoints = [firstEndpoint]}
      startupPlan `shouldNotBe` startupPlan {httpBindPlan = HttpBindPlan {httpEndpoints = [firstEndpoint]}}
      show firstEndpoint `shouldBe` "ListenerEndpoint {endpointHost = \"127.0.0.1\", endpointPort = 5001}"
      show [firstEndpoint, secondEndpoint] `shouldBe` "[ListenerEndpoint {endpointHost = \"127.0.0.1\", endpointPort = 5001},ListenerEndpoint {endpointHost = \"0.0.0.0\", endpointPort = 5002}]"
      show httpBindPlan `shouldBe` "HttpBindPlan {httpEndpoints = [ListenerEndpoint {endpointHost = \"127.0.0.1\", endpointPort = 5001},ListenerEndpoint {endpointHost = \"0.0.0.0\", endpointPort = 5002}]}"
      show [httpBindPlan] `shouldBe` "[HttpBindPlan {httpEndpoints = [ListenerEndpoint {endpointHost = \"127.0.0.1\", endpointPort = 5001},ListenerEndpoint {endpointHost = \"0.0.0.0\", endpointPort = 5002}]}]"
      show startupPlan `shouldBe` "ServerStartupPlan {httpBindPlan = HttpBindPlan {httpEndpoints = [ListenerEndpoint {endpointHost = \"127.0.0.1\", endpointPort = 5001},ListenerEndpoint {endpointHost = \"0.0.0.0\", endpointPort = 5002}]}, manualTlsBindPlans = [], acmeBindPlans = []}"
      show [startupPlan] `shouldBe` "[ServerStartupPlan {httpBindPlan = HttpBindPlan {httpEndpoints = [ListenerEndpoint {endpointHost = \"127.0.0.1\", endpointPort = 5001},ListenerEndpoint {endpointHost = \"0.0.0.0\", endpointPort = 5002}]}, manualTlsBindPlans = [], acmeBindPlans = []}]"

    it "gives HTTP-01 ACME plans the same default TLS policy" $ do
      let endpoint = ListenerEndpoint {endpointHost = "0.0.0.0", endpointPort = 80}
          acmeConfig =
            AcmeConfig
              { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                acmeContactEmails = ["ops@example.com"],
                acmeDomains = ["example.com"],
                acmeHttp01Port = 80,
                acmeCertificateDirectory = Nothing,
                acmeCertbotConfig = CertbotConfig {certbotExecutable = "certbot", certbotArguments = ["certonly", "--webroot"]}
              }
          listener =
            ListenerConfig
              { listenerHost = endpointHost endpoint,
                listenerPort = endpointPort endpoint,
                listenerScheme = Http,
                listenerTls = Nothing,
                listenerAcme = Just acmeConfig
              }
      planServerStartup (serverConfigWithListeners [listener])
        `shouldBe` Right
          ServerStartupPlan
            { httpBindPlan = HttpBindPlan {httpEndpoints = [endpoint]},
              manualTlsBindPlans = [],
              acmeBindPlans = [AcmeBindPlan {acmeEndpoint = endpoint, acmeTlsEndpoint = Nothing, acmeListenerConfig = acmeConfig, acmeTlsPolicy = defaultTlsPolicy}]
            }

    it "translates manual certificate files into TLS startup parameters" $ do
      let endpoint = ListenerEndpoint {endpointHost = "0.0.0.0", endpointPort = 5443}
          certificateSource = ManualCertificateFiles ManualTlsCertificateFiles {certificateFile = "cert.pem", privateKeyFile = "key.pem"}
          listener =
            ListenerConfig
              { listenerHost = endpointHost endpoint,
                listenerPort = endpointPort endpoint,
                listenerScheme = Https,
                listenerTls = Just (TlsConfig {certificateSource = certificateSource, tlsPolicy = defaultTlsPolicy}),
                listenerAcme = Nothing
              }
          manualPlan =
            ManualTlsBindPlan
              { tlsEndpoint = endpoint,
                tlsCertificateFile = "cert.pem",
                tlsPrivateKeyFile = "key.pem",
                tlsCredentialSourceKind = ManualTlsCredentials,
                tlsStartupMode = RequireCertificateFiles,
                tlsBindPolicy = defaultTlsPolicy
              }
      planServerStartup (serverConfigWithListeners [listener])
        `shouldBe` Right
          ServerStartupPlan
            { httpBindPlan = HttpBindPlan {httpEndpoints = []},
              manualTlsBindPlans = [manualPlan],
              acmeBindPlans = []
            }
      manualPlan `shouldNotBe` manualPlan {tlsCertificateFile = "other.pem"}
      show manualPlan `shouldBe` "ManualTlsBindPlan {tlsEndpoint = ListenerEndpoint {endpointHost = \"0.0.0.0\", endpointPort = 5443}, tlsCertificateFile = \"cert.pem\", tlsPrivateKeyFile = \"key.pem\", tlsCredentialSourceKind = ManualTlsCredentials, tlsStartupMode = RequireCertificateFiles, tlsBindPolicy = TlsPolicy {tlsAllowedVersions = Tls12 :| [Tls13], tlsCipherSuites = TlsEcdheEcdsaAes256GcmSha384 :| [TlsEcdheEcdsaChacha20Poly1305Sha256,TlsEcdheEcdsaAes128GcmSha256,TlsEcdheRsaAes256GcmSha384,TlsEcdheRsaChacha20Poly1305Sha256,TlsEcdheRsaAes128GcmSha256,Tls13Aes256GcmSha384,Tls13Chacha20Poly1305Sha256,Tls13Aes128GcmSha256]}}"
      show [manualPlan] `shouldBe` "[ManualTlsBindPlan {tlsEndpoint = ListenerEndpoint {endpointHost = \"0.0.0.0\", endpointPort = 5443}, tlsCertificateFile = \"cert.pem\", tlsPrivateKeyFile = \"key.pem\", tlsCredentialSourceKind = ManualTlsCredentials, tlsStartupMode = RequireCertificateFiles, tlsBindPolicy = TlsPolicy {tlsAllowedVersions = Tls12 :| [Tls13], tlsCipherSuites = TlsEcdheEcdsaAes256GcmSha384 :| [TlsEcdheEcdsaChacha20Poly1305Sha256,TlsEcdheEcdsaAes128GcmSha256,TlsEcdheRsaAes256GcmSha384,TlsEcdheRsaChacha20Poly1305Sha256,TlsEcdheRsaAes128GcmSha256,Tls13Aes256GcmSha384,Tls13Chacha20Poly1305Sha256,Tls13Aes128GcmSha256]}}]"

    it "translates shared certificate directories into TLS startup parameters" $ do
      let endpoint = ListenerEndpoint {endpointHost = "0.0.0.0", endpointPort = 5444}
          certificateSource =
            SharedCertificateFiles
              SharedTlsCertificateFiles
                { certificateDirectory = "/var/lib/harch-web/shared-certs",
                  sharedCertificateStartupMode = AwaitCertificateFiles Nothing
                }
          listener =
            ListenerConfig
              { listenerHost = endpointHost endpoint,
                listenerPort = endpointPort endpoint,
                listenerScheme = Https,
                listenerTls = Just (TlsConfig {certificateSource = certificateSource, tlsPolicy = defaultTlsPolicy}),
                listenerAcme = Nothing
              }
          manualPlan =
            ManualTlsBindPlan
              { tlsEndpoint = endpoint,
                tlsCertificateFile = "/var/lib/harch-web/shared-certs/fullchain.pem",
                tlsPrivateKeyFile = "/var/lib/harch-web/shared-certs/privkey.pem",
                tlsCredentialSourceKind = SharedTlsCredentials,
                tlsStartupMode = AwaitCertificateFiles Nothing,
                tlsBindPolicy = defaultTlsPolicy
              }
      planServerStartup (serverConfigWithListeners [listener])
        `shouldBe` Right
          ServerStartupPlan
            { httpBindPlan = HttpBindPlan {httpEndpoints = []},
              manualTlsBindPlans = [manualPlan],
              acmeBindPlans = []
            }
      manualPlan `shouldNotBe` manualPlan {tlsPrivateKeyFile = "other-privkey.pem"}
      show manualPlan `shouldBe` "ManualTlsBindPlan {tlsEndpoint = ListenerEndpoint {endpointHost = \"0.0.0.0\", endpointPort = 5444}, tlsCertificateFile = \"/var/lib/harch-web/shared-certs/fullchain.pem\", tlsPrivateKeyFile = \"/var/lib/harch-web/shared-certs/privkey.pem\", tlsCredentialSourceKind = SharedTlsCredentials, tlsStartupMode = AwaitCertificateFiles Nothing, tlsBindPolicy = TlsPolicy {tlsAllowedVersions = Tls12 :| [Tls13], tlsCipherSuites = TlsEcdheEcdsaAes256GcmSha384 :| [TlsEcdheEcdsaChacha20Poly1305Sha256,TlsEcdheEcdsaAes128GcmSha256,TlsEcdheRsaAes256GcmSha384,TlsEcdheRsaChacha20Poly1305Sha256,TlsEcdheRsaAes128GcmSha256,Tls13Aes256GcmSha384,Tls13Chacha20Poly1305Sha256,Tls13Aes128GcmSha256]}}"
      show [manualPlan] `shouldBe` "[ManualTlsBindPlan {tlsEndpoint = ListenerEndpoint {endpointHost = \"0.0.0.0\", endpointPort = 5444}, tlsCertificateFile = \"/var/lib/harch-web/shared-certs/fullchain.pem\", tlsPrivateKeyFile = \"/var/lib/harch-web/shared-certs/privkey.pem\", tlsCredentialSourceKind = SharedTlsCredentials, tlsStartupMode = AwaitCertificateFiles Nothing, tlsBindPolicy = TlsPolicy {tlsAllowedVersions = Tls12 :| [Tls13], tlsCipherSuites = TlsEcdheEcdsaAes256GcmSha384 :| [TlsEcdheEcdsaChacha20Poly1305Sha256,TlsEcdheEcdsaAes128GcmSha256,TlsEcdheRsaAes256GcmSha384,TlsEcdheRsaChacha20Poly1305Sha256,TlsEcdheRsaAes128GcmSha256,Tls13Aes256GcmSha384,Tls13Chacha20Poly1305Sha256,Tls13Aes128GcmSha256]}}]"

    it "translates fail-fast shared certificate directories into immediate TLS startup parameters" $ do
      let endpoint = ListenerEndpoint {endpointHost = "0.0.0.0", endpointPort = 5445}
          certificateSource =
            SharedCertificateFiles
              SharedTlsCertificateFiles
                { certificateDirectory = "/var/lib/harch-web/preprovisioned-certs",
                  sharedCertificateStartupMode = RequireCertificateFiles
                }
          listener =
            ListenerConfig
              { listenerHost = endpointHost endpoint,
                listenerPort = endpointPort endpoint,
                listenerScheme = Https,
                listenerTls = Just (TlsConfig {certificateSource = certificateSource, tlsPolicy = defaultTlsPolicy}),
                listenerAcme = Nothing
              }
      planServerStartup (serverConfigWithListeners [listener])
        `shouldBe` Right
          ServerStartupPlan
            { httpBindPlan = HttpBindPlan {httpEndpoints = []},
              manualTlsBindPlans =
                [ ManualTlsBindPlan
                    { tlsEndpoint = endpoint,
                      tlsCertificateFile = "/var/lib/harch-web/preprovisioned-certs/fullchain.pem",
                      tlsPrivateKeyFile = "/var/lib/harch-web/preprovisioned-certs/privkey.pem",
                      tlsCredentialSourceKind = SharedTlsCredentials,
                      tlsStartupMode = RequireCertificateFiles,
                      tlsBindPolicy = defaultTlsPolicy
                    }
                ],
              acmeBindPlans = []
            }

    it "translates ACME-backed HTTPS listeners into certificate-management plans" $ do
      let httpEndpoint = ListenerEndpoint {endpointHost = "127.0.0.1", endpointPort = 5001}
          endpoint = ListenerEndpoint {endpointHost = "0.0.0.0", endpointPort = 5444}
          httpListener =
            ListenerConfig
              { listenerHost = endpointHost httpEndpoint,
                listenerPort = endpointPort httpEndpoint,
                listenerScheme = Http,
                listenerTls = Nothing,
                listenerAcme = Nothing
              }
          acmeConfig =
            AcmeConfig
              { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                acmeContactEmails = ["ops@example.com"],
                acmeDomains = ["example.com", "www.example.com"],
                acmeHttp01Port = 80,
                acmeCertificateDirectory = Nothing,
                acmeCertbotConfig =
                  CertbotConfig
                    { certbotExecutable = "certbot",
                      certbotArguments = ["certonly", "--webroot"]
                    }
              }
          listener =
            ListenerConfig
              { listenerHost = endpointHost endpoint,
                listenerPort = endpointPort endpoint,
                listenerScheme = Https,
                listenerTls = Just (TlsConfig {certificateSource = AcmeCertificateSource acmeConfig, tlsPolicy = defaultTlsPolicy}),
                listenerAcme = Nothing
              }
          acmePlan =
            AcmeBindPlan
              { acmeEndpoint = endpoint,
                acmeTlsEndpoint = Just endpoint,
                acmeListenerConfig = acmeConfig,
                acmeTlsPolicy = defaultTlsPolicy
              }
      planServerStartup (serverConfigWithListeners [httpListener, listener])
        `shouldBe` Right
          ServerStartupPlan
            { httpBindPlan = HttpBindPlan {httpEndpoints = [httpEndpoint]},
              manualTlsBindPlans = [],
              acmeBindPlans = [acmePlan]
            }
      acmePlan `shouldNotBe` acmePlan {acmeEndpoint = ListenerEndpoint {endpointHost = "127.0.0.1", endpointPort = 5444}}
      show acmePlan `shouldContain` "certbotArguments = <redacted: 2>"
      show [acmePlan] `shouldContain` "certbotArguments = <redacted: 2>"

    it "rejects listeners whose TLS mode does not match their scheme" $ do
      let httpTlsListener =
            ListenerConfig
              { listenerHost = "127.0.0.1",
                listenerPort = 5001,
                listenerScheme = Http,
                listenerTls = Just (TlsConfig {certificateSource = ManualCertificateFiles ManualTlsCertificateFiles {certificateFile = "cert.pem", privateKeyFile = "key.pem"}, tlsPolicy = defaultTlsPolicy}),
                listenerAcme = Nothing
              }
          httpsWithoutTls =
            ListenerConfig
              { listenerHost = "127.0.0.1",
                listenerPort = 5443,
                listenerScheme = Https,
                listenerTls = Nothing,
                listenerAcme = Nothing
              }
      planServerStartup (serverConfigWithListeners [httpTlsListener])
        `shouldBe` Left (InvalidListenerTlsConfiguration httpTlsListener)
      planServerStartup (serverConfigWithListeners [httpsWithoutTls])
        `shouldBe` Left (InvalidListenerTlsConfiguration httpsWithoutTls)
      InvalidListenerTlsConfiguration httpTlsListener `shouldNotBe` InvalidListenerTlsConfiguration httpsWithoutTls
      show (InvalidListenerTlsConfiguration httpTlsListener)
        `shouldBe` "InvalidListenerTlsConfiguration (ListenerConfig {listenerHost = \"127.0.0.1\", listenerPort = 5001, listenerScheme = Http, listenerTls = Just (TlsConfig {certificateSource = ManualCertificateFiles (ManualTlsCertificateFiles {certificateFile = \"cert.pem\", privateKeyFile = \"key.pem\"}), tlsPolicy = TlsPolicy {tlsAllowedVersions = Tls12 :| [Tls13], tlsCipherSuites = TlsEcdheEcdsaAes256GcmSha384 :| [TlsEcdheEcdsaChacha20Poly1305Sha256,TlsEcdheEcdsaAes128GcmSha256,TlsEcdheRsaAes256GcmSha384,TlsEcdheRsaChacha20Poly1305Sha256,TlsEcdheRsaAes128GcmSha256,Tls13Aes256GcmSha384,Tls13Chacha20Poly1305Sha256,Tls13Aes128GcmSha256]}})})"
      show [InvalidListenerTlsConfiguration httpsWithoutTls]
        `shouldBe` "[InvalidListenerTlsConfiguration (ListenerConfig {listenerHost = \"127.0.0.1\", listenerPort = 5443, listenerScheme = Https, listenerTls = Nothing})]"

    it "rejects ACME producer config attached to HTTPS listeners" $ do
      let httpsAcmeProducerListener =
            ListenerConfig
              { listenerHost = "127.0.0.1",
                listenerPort = 5443,
                listenerScheme = Https,
                listenerTls = Nothing,
                listenerAcme =
                  Just
                    AcmeConfig
                      { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                        acmeContactEmails = ["ops@example.com"],
                        acmeDomains = ["example.com"],
                        acmeHttp01Port = 80,
                        acmeCertificateDirectory = Just ".tls/example.com",
                        acmeCertbotConfig = certbotHttp01Backend []
                      }
              }
      planServerStartup (serverConfigWithListeners [httpsAcmeProducerListener])
        `shouldBe` Left (InvalidListenerAcmeConfiguration httpsAcmeProducerListener)

    it "rejects invalid mixed listener configurations before startup" $ do
      let httpListener =
            ListenerConfig
              { listenerHost = "0.0.0.0",
                listenerPort = 5001,
                listenerScheme = Http,
                listenerTls = Nothing,
                listenerAcme = Nothing
              }
          httpsListener =
            ListenerConfig
              { listenerHost = "0.0.0.0",
                listenerPort = 5001,
                listenerScheme = Https,
                listenerTls =
                  Just
                    TlsConfig
                      { certificateSource = ManualCertificateFiles ManualTlsCertificateFiles {certificateFile = "cert.pem", privateKeyFile = "key.pem"},
                        tlsPolicy = defaultTlsPolicy
                      },
                listenerAcme = Nothing
              }
          duplicateEndpoint = ListenerEndpoint {endpointHost = "0.0.0.0", endpointPort = 5001}
      planServerStartup (serverConfigWithListeners [httpListener, httpsListener])
        `shouldBe` Left (DuplicateListenerEndpoint duplicateEndpoint)
      DuplicateListenerEndpoint duplicateEndpoint `shouldBe` DuplicateListenerEndpoint duplicateEndpoint
      DuplicateListenerEndpoint duplicateEndpoint `shouldNotBe` DuplicateListenerEndpoint ListenerEndpoint {endpointHost = "127.0.0.1", endpointPort = 5001}
      show (DuplicateListenerEndpoint duplicateEndpoint)
        `shouldBe` "DuplicateListenerEndpoint (ListenerEndpoint {endpointHost = \"0.0.0.0\", endpointPort = 5001})"
      show [DuplicateListenerEndpoint duplicateEndpoint]
        `shouldBe` "[DuplicateListenerEndpoint (ListenerEndpoint {endpointHost = \"0.0.0.0\", endpointPort = 5001})]"
