{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Data.Text qualified as Text
import HarchWeb qualified
import WebApi.AccountPages (AccountActionTarget (..), MfaEnrollmentForm (..), VerificationForm (..), emptyLoginForm, emptyRegistrationForm)
import WebApi.Config (AcmeConfig (..), AppConfig (..), CertbotConfig (..), ListenerConfig (..), ListenerScheme (..), ManualTlsCertificateFiles (..), ObservabilityConfig (..), OtlpExporter (..), SharedTlsCertificateFiles (..), StaticAssetRoot (..), StaticAssetsConfig (..), TlsCertificateSource (..), TlsConfig (..), TlsStartupMode (..), defaultAppConfig, defaultStaticAssetContentTypes, defaultTlsPolicy)
import WebApi.Page (AppPageModel (..), CallToAction (..), NotFoundPageModel (..), ProfilePageModel (..), SecondPageModel (..), SpacesPageModel (..), UnavailableProfilePageDetails (..))
import WebApi.Route (ApiRoute (..), AppLocale (..), AppRequestContext (..), AppRoute (..), RouteSelectionError (..), defaultRequestContext)
import WebApi.Route qualified

requestIdFixture :: HarchWeb.RequestId
requestIdFixture =
  case HarchWeb.mkRequestId "550e8400-e29b-41d4-a716-446655440000" of
    Nothing -> error "test request identifier must be canonical UUIDv4"
    Just requestId -> requestId

spec = do
  describe "config model values" $ do
    it "can represent manual, shared, and certbot-backed ACME certificates plus exporter endpoints" $ do
      let certbotConfig =
            CertbotConfig
              { certbotExecutable = "certbot",
                certbotArguments = ["certonly", "--webroot"]
              }
          sharedCertificateSource =
            SharedCertificateFiles
              SharedTlsCertificateFiles
                { certificateDirectory = "/var/lib/web-api/shared-certs",
                  sharedCertificateStartupMode = AwaitCertificateFiles Nothing
                }
          tlsSource =
            AcmeCertificateSource
              AcmeConfig
                { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                  acmeContactEmails = ["ops@example.com"],
                  acmeDomains = ["example.com", "www.example.com"],
                  acmeHttp01Port = 80,
                  acmeCertificateDirectory = Nothing,
                  acmeCertbotConfig = certbotConfig
                }
          exporter =
            OtlpExporter
              { otlpEndpoint = "http://otel-collector:4318",
                otlpHeaders = [("x-api-key", "secret")]
              }
      TlsConfig {certificateSource = ManualCertificateFiles ManualTlsCertificateFiles {certificateFile = "cert.pem", privateKeyFile = "key.pem"}, tlsPolicy = defaultTlsPolicy}
        `shouldBe` TlsConfig {certificateSource = ManualCertificateFiles ManualTlsCertificateFiles {certificateFile = "cert.pem", privateKeyFile = "key.pem"}, tlsPolicy = defaultTlsPolicy}
      show sharedCertificateSource
        `shouldBe` "SharedCertificateFiles (SharedTlsCertificateFiles {certificateDirectory = \"/var/lib/web-api/shared-certs\", sharedCertificateStartupMode = AwaitCertificateFiles Nothing})"
      show tlsSource `shouldContain` "certbotArguments = <redacted: 2>"
      show exporter
        `shouldBe` "OtlpExporter {otlpEndpoint = <configured>, otlpHeaders = <redacted: 1>}"

    it "reads exported selectors from the remaining public config and page-model types" $ do
      let manualCertificateSource =
            ManualCertificateFiles
              ManualTlsCertificateFiles
                { certificateFile = "cert.pem",
                  privateKeyFile = "key.pem"
                }
          inProcessAcmeConfig =
            AcmeConfig
              { acmeDirectoryUrl = "https://acme-staging-v02.api.letsencrypt.org/directory",
                acmeContactEmails = ["ops@example.com", "alerts@example.com"],
                acmeDomains = ["example.com", "www.example.com"],
                acmeHttp01Port = 80,
                acmeCertificateDirectory = Nothing,
                acmeCertbotConfig =
                  CertbotConfig
                    { certbotExecutable = "certbot",
                      certbotArguments = []
                    }
              }
          sharedCertificateSource =
            SharedCertificateFiles
              SharedTlsCertificateFiles
                { certificateDirectory = "/var/lib/web-api/shared-certs",
                  sharedCertificateStartupMode = AwaitCertificateFiles Nothing
                }
          tlsConfig = TlsConfig {certificateSource = manualCertificateSource, tlsPolicy = defaultTlsPolicy}
          listenerConfig =
            ListenerConfig
              { listenerHost = "0.0.0.0",
                listenerPort = 5443,
                listenerScheme = Https,
                listenerTls = Just tlsConfig,
                listenerAcme = Nothing
              }
          staticRoot =
            StaticAssetRoot
              { staticUrlPrefix = "/assets",
                staticDirectory = "public"
              }
          staticConfig =
            StaticAssetsConfig
              { staticAssetRoots = [staticRoot],
                staticAssetContentTypes = defaultStaticAssetContentTypes,
                staticCacheControlSeconds = Just 3600
              }
          exporter =
            OtlpExporter
              { otlpEndpoint = "http://otel-collector:4318",
                otlpHeaders = [("authorization", "Bearer token")]
              }
          observabilityConfig =
            ObservabilityConfig
              { tracingExporter = Just exporter,
                metricsExporter = Just exporter
              }
          appConfig =
            AppConfig
              { appTitlePrefix = "test-app",
                listenerConfigs = [listenerConfig],
                staticAssets = staticConfig,
                requestPolicy = requestPolicy defaultAppConfig,
                observability = observabilityConfig
              }
          requestContext =
            AppRequestContext
              { requestLocale = Spanish,
                requestLocaleIsExplicit = False,
                requestCorrelationId = Just requestIdFixture,
                requestClientAddress = requestClientAddress defaultRequestContext,
                requestPathPrefix = requestPathPrefix defaultRequestContext,
                requestQueryParameters = [],
                requestAccountPrincipal = Nothing,
                requestMfaEnrollmentSessionId = Nothing
              }
          callToAction =
            CallToAction
              { callToActionLabel = "Return home",
                callToActionRoute = HomeRoute,
                callToActionHref = "/es"
              }
          notFoundPageModel =
            NotFoundPageModel
              { notFoundHeading = "Not Found",
                notFoundSummary = "The requested page could not be found.",
                notFoundPrimaryAction = callToAction
              }
          secondPageModel =
            SecondPageModel
              { secondHeading = "Second",
                secondSummary = "Second page content with stubbed data ready for future loaders.",
                secondHighlights = ["Fast SSR", "Progressive enhancement"],
                secondErrorMessage = Nothing,
                secondPrimaryAction = callToAction
              }
      case manualCertificateSource of
        ManualCertificateFiles source -> do
          certificateFile source `shouldBe` "cert.pem"
          privateKeyFile source `shouldBe` "key.pem"
        AcmeCertificateSource _ -> expectationFailure "expected manual certificate files"
        SharedCertificateFiles _ -> expectationFailure "expected manual certificate files"
      acmeDirectoryUrl inProcessAcmeConfig `shouldBe` "https://acme-staging-v02.api.letsencrypt.org/directory"
      acmeContactEmails inProcessAcmeConfig `shouldBe` ["ops@example.com", "alerts@example.com"]
      acmeDomains inProcessAcmeConfig `shouldBe` ["example.com", "www.example.com"]
      acmeHttp01Port inProcessAcmeConfig `shouldBe` 80
      acmeCertificateDirectory inProcessAcmeConfig `shouldBe` Nothing
      acmeCertbotConfig inProcessAcmeConfig
        `shouldBe` CertbotConfig
          { certbotExecutable = "certbot",
            certbotArguments = []
          }
      case sharedCertificateSource of
        SharedCertificateFiles SharedTlsCertificateFiles {certificateDirectory = sharedDirectory, sharedCertificateStartupMode = startupMode} -> do
          sharedDirectory `shouldBe` "/var/lib/web-api/shared-certs"
          startupMode `shouldBe` AwaitCertificateFiles Nothing
        _ ->
          expectationFailure "expected shared certificate files"
      certificateSource tlsConfig `shouldBe` manualCertificateSource
      listenerHost listenerConfig `shouldBe` "0.0.0.0"
      listenerPort listenerConfig `shouldBe` 5443
      listenerScheme listenerConfig `shouldBe` Https
      listenerTls listenerConfig `shouldBe` Just tlsConfig
      staticUrlPrefix staticRoot `shouldBe` "/assets"
      staticDirectory staticRoot `shouldBe` "public"
      staticAssetRoots staticConfig `shouldBe` [staticRoot]
      staticCacheControlSeconds staticConfig `shouldBe` Just 3600
      otlpEndpoint exporter `shouldBe` "http://otel-collector:4318"
      otlpHeaders exporter `shouldBe` [("authorization", "Bearer token")]
      tracingExporter observabilityConfig `shouldBe` Just exporter
      metricsExporter observabilityConfig `shouldBe` Just exporter
      appTitlePrefix appConfig `shouldBe` "test-app"
      listenerConfigs appConfig `shouldBe` [listenerConfig]
      staticAssets appConfig `shouldBe` staticConfig
      observability appConfig `shouldBe` observabilityConfig
      requestLocale requestContext `shouldBe` Spanish
      requestCorrelationId requestContext `shouldBe` Just requestIdFixture
      callToActionLabel callToAction `shouldBe` "Return home"
      callToActionRoute callToAction `shouldBe` HomeRoute
      callToActionHref callToAction `shouldBe` "/es"
      secondHeading secondPageModel `shouldBe` "Second"
      secondSummary secondPageModel `shouldBe` "Second page content with stubbed data ready for future loaders."
      secondHighlights secondPageModel `shouldBe` ["Fast SSR", "Progressive enhancement"]
      secondPrimaryAction secondPageModel `shouldBe` callToAction
      notFoundHeading notFoundPageModel `shouldBe` "Not Found"
      notFoundSummary notFoundPageModel `shouldBe` "The requested page could not be found."
      notFoundPrimaryAction notFoundPageModel `shouldBe` callToAction

    it "directly exercises the remaining derived eq and show instances" $ do
      let certbotConfig =
            CertbotConfig
              { certbotExecutable = "certbot",
                certbotArguments = ["certonly", "--webroot"]
              }
          manualCertificateSource =
            ManualCertificateFiles
              ManualTlsCertificateFiles
                { certificateFile = "cert.pem",
                  privateKeyFile = "key.pem"
                }
          acmeCertificateSource =
            AcmeCertificateSource
              AcmeConfig
                { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                  acmeContactEmails = ["ops@example.com"],
                  acmeDomains = ["example.com", "www.example.com"],
                  acmeHttp01Port = 80,
                  acmeCertificateDirectory = Nothing,
                  acmeCertbotConfig = certbotConfig
                }
          sharedCertificateSource =
            SharedCertificateFiles
              SharedTlsCertificateFiles
                { certificateDirectory = "/var/lib/web-api/shared-certs",
                  sharedCertificateStartupMode = AwaitCertificateFiles Nothing
                }
          staticRoot =
            StaticAssetRoot
              { staticUrlPrefix = "/assets",
                staticDirectory = "public"
              }
          secondPageModel =
            SecondPageModel
              { secondHeading = "Second",
                secondSummary = "Second page content with stubbed data ready for future loaders.",
                secondHighlights = ["Fast SSR"],
                secondErrorMessage = Nothing,
                secondPrimaryAction =
                  CallToAction
                    { callToActionLabel = "Return home",
                      callToActionRoute = HomeRoute,
                      callToActionHref = "/"
                    }
              }
          notFoundPageModel =
            NotFoundPageModel
              { notFoundHeading = "Not Found",
                notFoundSummary = "The requested page could not be found.",
                notFoundPrimaryAction =
                  CallToAction
                    { callToActionLabel = "Return home",
                      callToActionRoute = HomeRoute,
                      callToActionHref = "/"
                    }
              }
      show certbotConfig
        `shouldBe` "CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = <redacted: 2>}"
      show certbotConfig
        `shouldBe` "CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = <redacted: 2>}"
      show
        AcmeConfig
          { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
            acmeContactEmails = ["ops@example.com"],
            acmeDomains = ["example.com", "www.example.com"],
            acmeHttp01Port = 80,
            acmeCertificateDirectory = Nothing,
            acmeCertbotConfig = certbotConfig
          }
        `shouldContain` "certbotArguments = <redacted: 2>"
      show acmeCertificateSource
        `shouldContain` "certbotArguments = <redacted: 2>"
      show (TlsConfig {certificateSource = manualCertificateSource, tlsPolicy = defaultTlsPolicy})
        `shouldBe` "TlsConfig {certificateSource = ManualCertificateFiles (ManualTlsCertificateFiles {certificateFile = \"cert.pem\", privateKeyFile = \"key.pem\"}), tlsPolicy = TlsPolicy {tlsAllowedVersions = Tls12 :| [Tls13], tlsCipherSuites = TlsEcdheEcdsaAes256GcmSha384 :| [TlsEcdheEcdsaChacha20Poly1305Sha256,TlsEcdheEcdsaAes128GcmSha256,TlsEcdheRsaAes256GcmSha384,TlsEcdheRsaChacha20Poly1305Sha256,TlsEcdheRsaAes128GcmSha256,Tls13Aes256GcmSha384,Tls13Chacha20Poly1305Sha256,Tls13Aes128GcmSha256]}}"
      show sharedCertificateSource
        `shouldBe` "SharedCertificateFiles (SharedTlsCertificateFiles {certificateDirectory = \"/var/lib/web-api/shared-certs\", sharedCertificateStartupMode = AwaitCertificateFiles Nothing})"
      show manualCertificateSource
        `shouldBe` "ManualCertificateFiles (ManualTlsCertificateFiles {certificateFile = \"cert.pem\", privateKeyFile = \"key.pem\"})"
      show (ListenerConfig {listenerHost = "127.0.0.1", listenerPort = 5001, listenerScheme = Http, listenerTls = Nothing, listenerAcme = Nothing})
        `shouldBe` "ListenerConfig {listenerHost = \"127.0.0.1\", listenerPort = 5001, listenerScheme = Http, listenerTls = Nothing}"
      show staticRoot `shouldBe` "StaticAssetRoot {staticUrlPrefix = \"/assets\", staticDirectory = \"public\"}"
      show
        ( StaticAssetsConfig
            { staticAssetRoots = [staticRoot],
              staticAssetContentTypes = defaultStaticAssetContentTypes,
              staticCacheControlSeconds = Just 3600
            }
        )
        `shouldBe` ( "StaticAssetsConfig {staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = \"/assets\", staticDirectory = \"public\"}], staticAssetContentTypes = "
                       <> show defaultStaticAssetContentTypes
                       <> ", staticCacheControlSeconds = Just 3600}"
                   )
      show
        ( ObservabilityConfig
            { tracingExporter =
                Just
                  OtlpExporter
                    { otlpEndpoint = "http://otel-collector:4318",
                      otlpHeaders = [("x-api-key", "secret")]
                    },
              metricsExporter = Nothing
            }
        )
        `shouldContain` "otlpHeaders = <redacted: 1>"
      show
        ( AppRequestContext
            { requestLocale = Spanish,
              requestLocaleIsExplicit = False,
              requestCorrelationId = Just requestIdFixture,
              requestClientAddress = requestClientAddress defaultRequestContext,
              requestPathPrefix = requestPathPrefix defaultRequestContext,
              requestQueryParameters = [],
              requestAccountPrincipal = Nothing,
              requestMfaEnrollmentSessionId = Nothing
            }
        )
        `shouldBe` "AppRequestContext {requestLocale = Spanish, requestLocaleIsExplicit = False, requestCorrelationId = Just (RequestId \"550e8400-e29b-41d4-a716-446655440000\"), requestClientAddress = ClientAddress <redacted>, requestPathPrefix = PathPrefix \"\", requestQueryParameters = [], requestAccountPrincipal = Nothing, requestMfaEnrollmentSessionId = Nothing}"
      show
        ( CallToAction
            { callToActionLabel = "Return home",
              callToActionRoute = HomeRoute,
              callToActionHref = "/"
            }
        )
        `shouldBe` "CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = SafeUrl \"/\"}"
      show English `shouldBe` "English"
      show Spanish `shouldBe` "Spanish"
      show WebApi.Route.HomePage `shouldBe` "HomePage"
      show WebApi.Route.StatusApi `shouldBe` "StatusApi"
      show (UnsupportedLocalePrefix "de") `shouldBe` "UnsupportedLocalePrefix \"de\""
      show (UnsupportedPath "/missing") `shouldBe` "UnsupportedPath \"/missing\""
      show secondPageModel
        `shouldBe` "SecondPageModel {secondHeading = \"Second\", secondSummary = \"Second page content with stubbed data ready for future loaders.\", secondHighlights = [\"Fast SSR\"], secondErrorMessage = Nothing, secondPrimaryAction = CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = SafeUrl \"/\"}}"
      show (SecondPage secondPageModel)
        `shouldBe` "SecondPage (SecondPageModel {secondHeading = \"Second\", secondSummary = \"Second page content with stubbed data ready for future loaders.\", secondHighlights = [\"Fast SSR\"], secondErrorMessage = Nothing, secondPrimaryAction = CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = SafeUrl \"/\"}})"
      show notFoundPageModel
        `shouldBe` "NotFoundPageModel {notFoundHeading = \"Not Found\", notFoundSummary = \"The requested page could not be found.\", notFoundPrimaryAction = CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = SafeUrl \"/\"}}"
      show (NotFoundPage notFoundPageModel)
        `shouldBe` "NotFoundPage (NotFoundPageModel {notFoundHeading = \"Not Found\", notFoundSummary = \"The requested page could not be found.\", notFoundPrimaryAction = CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = SafeUrl \"/\"}})"
      show
        ( AppConfig
            { appTitlePrefix = "test-app",
              listenerConfigs = [ListenerConfig {listenerHost = "127.0.0.1", listenerPort = 5001, listenerScheme = Http, listenerTls = Nothing, listenerAcme = Nothing}],
              staticAssets =
                StaticAssetsConfig
                  { staticAssetRoots = [staticRoot],
                    staticAssetContentTypes = defaultStaticAssetContentTypes,
                    staticCacheControlSeconds = Just 3600
                  },
              requestPolicy = requestPolicy defaultAppConfig,
              observability = ObservabilityConfig {tracingExporter = Nothing, metricsExporter = Nothing}
            }
        )
        `shouldContain` ("staticAssetContentTypes = " <> show defaultStaticAssetContentTypes)

    it "covers direct equality branches across the remaining public config and page types" $ do
      let certbotConfig =
            CertbotConfig
              { certbotExecutable = "certbot",
                certbotArguments = ["certonly", "--webroot"]
              }
          otherCertbotConfig =
            CertbotConfig
              { certbotExecutable = "certbot",
                certbotArguments = ["renew"]
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
                acmeCertificateDirectory = Just "/var/lib/web-api/staging-certs",
                acmeCertbotConfig = otherCertbotConfig
              }
          manualCertificateSource =
            ManualCertificateFiles
              ManualTlsCertificateFiles
                { certificateFile = "cert.pem",
                  privateKeyFile = "key.pem"
                }
          acmeCertificateSource = AcmeCertificateSource acmeConfig
          tlsConfig = TlsConfig {certificateSource = manualCertificateSource, tlsPolicy = defaultTlsPolicy}
          listenerConfig =
            ListenerConfig
              { listenerHost = "127.0.0.1",
                listenerPort = 5001,
                listenerScheme = Http,
                listenerTls = Nothing,
                listenerAcme = Nothing
              }
          secureListenerConfig =
            ListenerConfig
              { listenerHost = "0.0.0.0",
                listenerPort = 5443,
                listenerScheme = Https,
                listenerTls = Just tlsConfig,
                listenerAcme = Nothing
              }
          staticRoot =
            StaticAssetRoot
              { staticUrlPrefix = "/assets",
                staticDirectory = "public"
              }
          staticAssetsConfig =
            StaticAssetsConfig
              { staticAssetRoots = [staticRoot],
                staticAssetContentTypes = defaultStaticAssetContentTypes,
                staticCacheControlSeconds = Just 3600
              }
          exporter =
            OtlpExporter
              { otlpEndpoint = "http://otel-collector:4318",
                otlpHeaders = [("authorization", "Bearer token")]
              }
          observabilityConfig =
            ObservabilityConfig
              { tracingExporter = Just exporter,
                metricsExporter = Nothing
              }
          appConfig =
            AppConfig
              { appTitlePrefix = "test-app",
                listenerConfigs = [listenerConfig, secureListenerConfig],
                staticAssets = staticAssetsConfig,
                requestPolicy = requestPolicy defaultAppConfig,
                observability = observabilityConfig
              }
          requestContext =
            AppRequestContext
              { requestLocale = Spanish,
                requestLocaleIsExplicit = False,
                requestCorrelationId = Just requestIdFixture,
                requestClientAddress = requestClientAddress defaultRequestContext,
                requestPathPrefix = requestPathPrefix defaultRequestContext,
                requestQueryParameters = [],
                requestAccountPrincipal = Nothing,
                requestMfaEnrollmentSessionId = Nothing
              }
          callToAction =
            CallToAction
              { callToActionLabel = "Return home",
                callToActionRoute = HomeRoute,
                callToActionHref = "/"
              }
          secondPageModel =
            SecondPageModel
              { secondHeading = "Second",
                secondSummary = "Second page content with stubbed data ready for future loaders.",
                secondHighlights = ["Fast SSR"],
                secondErrorMessage = Nothing,
                secondPrimaryAction = callToAction
              }
          spacesPageModel =
            SpacesPageModel
              { spacesHeading = "Site under construction",
                spacesSummary = "Follow this space."
              }
          notFoundPageModel =
            NotFoundPageModel
              { notFoundHeading = "Not Found",
                notFoundSummary = "The requested page could not be found.",
                notFoundPrimaryAction = callToAction
              }
      certbotExecutable certbotConfig `shouldBe` "certbot"
      certbotArguments certbotConfig `shouldBe` ["certonly", "--webroot"]
      certbotConfig `shouldNotBe` otherCertbotConfig
      acmeConfig `shouldNotBe` otherAcmeConfig
      manualCertificateSource `shouldNotBe` acmeCertificateSource
      acmeCertificateSource `shouldNotBe` AcmeCertificateSource otherAcmeConfig
      tlsConfig `shouldNotBe` TlsConfig {certificateSource = acmeCertificateSource, tlsPolicy = defaultTlsPolicy}
      listenerConfig `shouldNotBe` secureListenerConfig
      staticRoot `shouldNotBe` StaticAssetRoot {staticUrlPrefix = "/static", staticDirectory = "public"}
      staticAssetsConfig
        `shouldNotBe` StaticAssetsConfig
          { staticAssetRoots = [],
            staticAssetContentTypes = defaultStaticAssetContentTypes,
            staticCacheControlSeconds = Nothing
          }
      exporter `shouldNotBe` OtlpExporter {otlpEndpoint = "http://other-collector:4318", otlpHeaders = []}
      observabilityConfig `shouldNotBe` ObservabilityConfig {tracingExporter = Nothing, metricsExporter = Nothing}
      appConfig `shouldNotBe` appConfig {listenerConfigs = [listenerConfig]}
      English `shouldNotBe` Spanish
      requestContext `shouldNotBe` defaultRequestContext
      callToAction `shouldNotBe` callToAction {callToActionHref = "/es"}
      secondPageModel `shouldNotBe` secondPageModel {secondHighlights = ["Different"]}
      spacesPageModel `shouldNotBe` spacesPageModel {spacesSummary = "Different"}
      notFoundPageModel `shouldNotBe` notFoundPageModel {notFoundSummary = "Missing"}
      SecondPage secondPageModel `shouldNotBe` NotFoundPage notFoundPageModel
      SpacesPage spacesPageModel `shouldNotBe` SecondPage secondPageModel
      RegistrationPage RegisterAccountTarget emptyRegistrationForm `shouldNotBe` SecondPage secondPageModel
      EmailVerificationPage VerifyEmailTarget (VerificationForm Text.empty Nothing False) `shouldNotBe` SecondPage secondPageModel
      MfaEnrollmentPage EnrollMfaTarget (MfaEnrollmentForm Nothing [] False Nothing False) `shouldNotBe` SecondPage secondPageModel
      LoginPage LoginAccountTarget emptyLoginForm `shouldNotBe` SecondPage secondPageModel
      LogoutPage LogoutAccountTarget `shouldNotBe` SecondPage secondPageModel
      ProfilePage (UnavailableProfilePage (UnavailableProfilePageDetails "Profile" "Unavailable" callToAction)) `shouldNotBe` SecondPage secondPageModel
      NotFoundPage notFoundPageModel `shouldNotBe` SecondPage secondPageModel
      UnsupportedLocalePrefix "de" `shouldNotBe` UnsupportedPath "/de"
      Page WebApi.Route.HomePage `shouldNotBe` Api WebApi.Route.StatusApi
      HomeRoute `shouldNotBe` SecondRoute
      SecondRoute `shouldNotBe` NotFoundRoute

    it "covers high-precedence show rendering for the remaining public types" $ do
      let shouldBeParenthesized rendered = do
            case rendered of
              '(' : rest ->
                case reverse rest of
                  ')' : _ -> pure ()
                  _ -> expectationFailure "expected parenthesized rendering"
              _ -> expectationFailure "expected parenthesized rendering"
          certbotConfig =
            CertbotConfig
              { certbotExecutable = "certbot",
                certbotArguments = ["certonly", "--webroot"]
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
          manualCertificateSource =
            ManualCertificateFiles
              ManualTlsCertificateFiles
                { certificateFile = "cert.pem",
                  privateKeyFile = "key.pem"
                }
          acmeCertificateSource = AcmeCertificateSource acmeConfig
          tlsConfig = TlsConfig {certificateSource = acmeCertificateSource, tlsPolicy = defaultTlsPolicy}
          listenerConfig =
            ListenerConfig
              { listenerHost = "0.0.0.0",
                listenerPort = 5443,
                listenerScheme = Https,
                listenerTls = Just tlsConfig,
                listenerAcme = Nothing
              }
          staticRoot =
            StaticAssetRoot
              { staticUrlPrefix = "/assets",
                staticDirectory = "public"
              }
          staticAssetsConfig =
            StaticAssetsConfig
              { staticAssetRoots = [staticRoot],
                staticAssetContentTypes = defaultStaticAssetContentTypes,
                staticCacheControlSeconds = Just 3600
              }
          exporter =
            OtlpExporter
              { otlpEndpoint = "http://otel-collector:4318",
                otlpHeaders = [("authorization", "Bearer token")]
              }
          observabilityConfig =
            ObservabilityConfig
              { tracingExporter = Just exporter,
                metricsExporter = Just exporter
              }
          appConfig =
            AppConfig
              { appTitlePrefix = "test-app",
                listenerConfigs = [listenerConfig],
                staticAssets = staticAssetsConfig,
                requestPolicy = requestPolicy defaultAppConfig,
                observability = observabilityConfig
              }
          requestContext =
            AppRequestContext
              { requestLocale = Spanish,
                requestLocaleIsExplicit = False,
                requestCorrelationId = Just requestIdFixture,
                requestClientAddress = requestClientAddress defaultRequestContext,
                requestPathPrefix = requestPathPrefix defaultRequestContext,
                requestQueryParameters = [],
                requestAccountPrincipal = Nothing,
                requestMfaEnrollmentSessionId = Nothing
              }
          callToAction =
            CallToAction
              { callToActionLabel = "Return home",
                callToActionRoute = HomeRoute,
                callToActionHref = "/"
              }
          secondPageModel =
            SecondPageModel
              { secondHeading = "Second",
                secondSummary = "Second page content with stubbed data ready for future loaders.",
                secondHighlights = ["Fast SSR"],
                secondErrorMessage = Nothing,
                secondPrimaryAction = callToAction
              }
          spacesPageModel =
            SpacesPageModel
              { spacesHeading = "Site under construction",
                spacesSummary = "Follow this space."
              }
          notFoundPageModel =
            NotFoundPageModel
              { notFoundHeading = "Not Found",
                notFoundSummary = "The requested page could not be found.",
                notFoundPrimaryAction = callToAction
              }
      show Http `shouldBe` "Http"
      show Https `shouldBe` "Https"
      show HomeRoute `shouldBe` "HomeRoute"
      show SecondRoute `shouldBe` "SecondRoute"
      show SpacesRoute `shouldBe` "SpacesRoute"
      show StatusApiRoute `shouldBe` "StatusApiRoute"
      show NotFoundRoute `shouldBe` "NotFoundRoute"
      show spacesPageModel
        `shouldBe` "SpacesPageModel {spacesHeading = \"Site under construction\", spacesSummary = \"Follow this space.\"}"
      show [spacesPageModel]
        `shouldBe` "[SpacesPageModel {spacesHeading = \"Site under construction\", spacesSummary = \"Follow this space.\"}]"
      shouldBeParenthesized (showsPrec 11 certbotConfig "")
      shouldBeParenthesized (showsPrec 11 certbotConfig "")
      shouldBeParenthesized (showsPrec 11 acmeConfig "")
      shouldBeParenthesized (showsPrec 11 manualCertificateSource "")
      shouldBeParenthesized (showsPrec 11 acmeCertificateSource "")
      shouldBeParenthesized (showsPrec 11 tlsConfig "")
      shouldBeParenthesized (showsPrec 11 listenerConfig "")
      shouldBeParenthesized (showsPrec 11 staticRoot "")
      shouldBeParenthesized (showsPrec 11 staticAssetsConfig "")
      shouldBeParenthesized (showsPrec 11 exporter "")
      shouldBeParenthesized (showsPrec 11 observabilityConfig "")
      shouldBeParenthesized (showsPrec 11 appConfig "")
      shouldBeParenthesized (showsPrec 11 requestContext "")
      shouldBeParenthesized (showsPrec 11 callToAction "")
      shouldBeParenthesized (showsPrec 11 secondPageModel "")
      shouldBeParenthesized (showsPrec 11 spacesPageModel "")
      shouldBeParenthesized (showsPrec 11 notFoundPageModel "")
      shouldBeParenthesized (showsPrec 11 (SecondPage secondPageModel) "")
      shouldBeParenthesized (showsPrec 11 (SpacesPage spacesPageModel) "")
      shouldBeParenthesized (showsPrec 11 (NotFoundPage notFoundPageModel) "")
      shouldBeParenthesized (showsPrec 11 (UnsupportedLocalePrefix "de") "")
      shouldBeParenthesized (showsPrec 11 (UnsupportedPath "/missing") "")

    it "covers derived list-show rendering for the remaining public types" $ do
      let certbotConfig =
            CertbotConfig
              { certbotExecutable = "certbot",
                certbotArguments = ["certonly", "--webroot"]
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
          manualCertificateSource =
            ManualCertificateFiles
              ManualTlsCertificateFiles
                { certificateFile = "cert.pem",
                  privateKeyFile = "key.pem"
                }
          acmeCertificateSource = AcmeCertificateSource acmeConfig
          tlsConfig = TlsConfig {certificateSource = acmeCertificateSource, tlsPolicy = defaultTlsPolicy}
          listenerConfig =
            ListenerConfig
              { listenerHost = "0.0.0.0",
                listenerPort = 5443,
                listenerScheme = Https,
                listenerTls = Just tlsConfig,
                listenerAcme = Nothing
              }
          staticRoot =
            StaticAssetRoot
              { staticUrlPrefix = "/assets",
                staticDirectory = "public"
              }
          staticAssetsConfig =
            StaticAssetsConfig
              { staticAssetRoots = [staticRoot],
                staticAssetContentTypes = defaultStaticAssetContentTypes,
                staticCacheControlSeconds = Just 3600
              }
          exporter =
            OtlpExporter
              { otlpEndpoint = "http://otel-collector:4318",
                otlpHeaders = [("authorization", "Bearer token")]
              }
          observabilityConfig =
            ObservabilityConfig
              { tracingExporter = Just exporter,
                metricsExporter = Just exporter
              }
          appConfig =
            AppConfig
              { appTitlePrefix = "test-app",
                listenerConfigs = [listenerConfig],
                staticAssets = staticAssetsConfig,
                requestPolicy = requestPolicy defaultAppConfig,
                observability = observabilityConfig
              }
          requestContext =
            AppRequestContext
              { requestLocale = Spanish,
                requestLocaleIsExplicit = False,
                requestCorrelationId = Just requestIdFixture,
                requestClientAddress = requestClientAddress defaultRequestContext,
                requestPathPrefix = requestPathPrefix defaultRequestContext,
                requestQueryParameters = [],
                requestAccountPrincipal = Nothing,
                requestMfaEnrollmentSessionId = Nothing
              }
          callToAction =
            CallToAction
              { callToActionLabel = "Return home",
                callToActionRoute = HomeRoute,
                callToActionHref = "/"
              }
          secondPageModel =
            SecondPageModel
              { secondHeading = "Second",
                secondSummary = "Second page content with stubbed data ready for future loaders.",
                secondHighlights = ["Fast SSR"],
                secondErrorMessage = Nothing,
                secondPrimaryAction = callToAction
              }
          notFoundPageModel =
            NotFoundPageModel
              { notFoundHeading = "Not Found",
                notFoundSummary = "The requested page could not be found.",
                notFoundPrimaryAction = callToAction
              }
      Http `shouldNotBe` Https
      show [Http, Https] `shouldBe` "[Http,Https]"
      show [certbotConfig] `shouldBe` "[CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = <redacted: 2>}]"
      show [certbotConfig]
        `shouldBe` "[CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = <redacted: 2>}]"
      show [acmeConfig]
        `shouldContain` "certbotArguments = <redacted: 2>"
      show [manualCertificateSource, acmeCertificateSource]
        `shouldContain` "certbotArguments = <redacted: 2>"
      show [tlsConfig]
        `shouldContain` "certbotArguments = <redacted: 2>"
      show [listenerConfig]
        `shouldContain` "certbotArguments = <redacted: 2>"
      show [staticRoot] `shouldBe` "[StaticAssetRoot {staticUrlPrefix = \"/assets\", staticDirectory = \"public\"}]"
      show [staticAssetsConfig]
        `shouldBe` ( "[StaticAssetsConfig {staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = \"/assets\", staticDirectory = \"public\"}], staticAssetContentTypes = "
                       <> show defaultStaticAssetContentTypes
                       <> ", staticCacheControlSeconds = Just 3600}]"
                   )
      show [exporter]
        `shouldBe` "[OtlpExporter {otlpEndpoint = <configured>, otlpHeaders = <redacted: 1>}]"
      show [observabilityConfig]
        `shouldContain` "otlpHeaders = <redacted: 1>"
      show [appConfig]
        `shouldContain` ("staticAssetContentTypes = " <> show defaultStaticAssetContentTypes)
      show [English, Spanish] `shouldBe` "[English,Spanish]"
      show [Page WebApi.Route.HomePage, Api WebApi.Route.StatusApi]
        `shouldBe` "[HomeRoute,StatusApiRoute]"
      show [requestContext]
        `shouldBe` "[AppRequestContext {requestLocale = Spanish, requestLocaleIsExplicit = False, requestCorrelationId = Just (RequestId \"550e8400-e29b-41d4-a716-446655440000\"), requestClientAddress = ClientAddress <redacted>, requestPathPrefix = PathPrefix \"\", requestQueryParameters = [], requestAccountPrincipal = Nothing, requestMfaEnrollmentSessionId = Nothing}]"

      show [callToAction]
        `shouldBe` "[CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = SafeUrl \"/\"}]"
      show [secondPageModel]
        `shouldBe` "[SecondPageModel {secondHeading = \"Second\", secondSummary = \"Second page content with stubbed data ready for future loaders.\", secondHighlights = [\"Fast SSR\"], secondErrorMessage = Nothing, secondPrimaryAction = CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = SafeUrl \"/\"}}]"
      show [notFoundPageModel]
        `shouldBe` "[NotFoundPageModel {notFoundHeading = \"Not Found\", notFoundSummary = \"The requested page could not be found.\", notFoundPrimaryAction = CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = SafeUrl \"/\"}}]"
      show [SecondPage secondPageModel, NotFoundPage notFoundPageModel]
        `shouldBe` "[SecondPage (SecondPageModel {secondHeading = \"Second\", secondSummary = \"Second page content with stubbed data ready for future loaders.\", secondHighlights = [\"Fast SSR\"], secondErrorMessage = Nothing, secondPrimaryAction = CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = SafeUrl \"/\"}}),NotFoundPage (NotFoundPageModel {notFoundHeading = \"Not Found\", notFoundSummary = \"The requested page could not be found.\", notFoundPrimaryAction = CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = SafeUrl \"/\"}})]"
      show [UnsupportedLocalePrefix "de", UnsupportedPath "/missing"]
        `shouldBe` "[UnsupportedLocalePrefix \"de\",UnsupportedPath \"/missing\"]"
      show [HomeRoute, SecondRoute, RegistrationRoute, EmailVerificationRoute, MfaEnrollmentRoute, LoginRoute, LogoutRoute, ProfileRoute, StatusApiRoute, NotFoundRoute] `shouldBe` "[HomeRoute,SecondRoute,RegistrationRoute,EmailVerificationRoute,MfaEnrollmentRoute,LoginRoute,LogoutRoute,ProfileRoute,StatusApiRoute,NotFoundRoute]"
