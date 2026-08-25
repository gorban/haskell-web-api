{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Exception (IOException, bracket, displayException, try)
import Control.Monad (forM_)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isNothing)
import Data.Text qualified as Text
import HarchWeb qualified
import HarchWeb.DevSmtp qualified as DevSmtp
import HarchWeb.Email qualified as Email
import HarchWeb.Observability qualified as Observability
import System.Environment (lookupEnv)
import System.IO.Temp (withSystemTempDirectory)
import Unit.WebApi.TestSupport hiding (databaseConfig)
import WebApi.AccountPages (AccountWorkflow (..))
import WebApi.App (buildRuntimeAccountWorkflow, buildRuntimeApp, runtimeRequestObservabilityReporter)
import WebApi.Config (AcmeConfig (..), AppConfig (..), AppEnvironmentConfig (..), AppEnvironmentConfigLoadError (..), AppMode (..), AppStartupConfig (..), AppStartupConfigLoadError (..), CertbotConfig (..), CorsPolicyConfig (..), DatabaseConfig (..), ForwardedHeaderTrust (..), ListenerConfig (..), ListenerScheme (..), ManualTlsCertificateFiles (..), ObservabilityConfig (..), OtlpExporter (..), RequestPolicyConfig (..), ResponseSecurityHeadersConfig (..), SharedTlsCertificateFiles (..), SmtpDeliveryConfig (..), StaticAssetRoot (..), StaticAssetsConfig (..), StrictTransportSecurityConfig (..), TlsCertificateSource (..), TlsConfig (..), TlsStartupMode (..), committedEnvDefaults, committedRuntimeDefaults, databasePoolCapacityValue, defaultAppConfig, defaultAppEnvironmentConfig, defaultAppStartupConfig, defaultCorsPolicyConfig, defaultResponseSecurityHeadersConfig, defaultStaticAssetContentTypes, loadAppEnvironmentConfig, loadAppEnvironmentConfigWithFiles, loadAppStartupConfig, loadAppStartupConfigWithFiles, mkDatabasePoolCapacity, parseAppEnvironmentConfig, parseAppStartupConfig, parseRuntimeAppConfig)
import WebApi.Postgres.Testing (newPostgresPool)
import WebApi.Route (AppLocale (..), AppRequestContext (..), AppRoute (..), defaultRequestContext)

-- Kept in the test only: production configuration must never acquire these
-- repository-known fixtures by omission.
developmentEnvironmentSecrets :: [(Text.Text, Text.Text)]
developmentEnvironmentSecrets =
  [ ("DATABASE_PASSWORD", "web_api"),
    ("SMTP_PASSWORD", "password"),
    ("TOTP_ENCRYPTION_KEY", "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
  ]

spec = do
  describe "DatabasePoolCapacity" $ do
    it "accepts only positive capacities" $ do
      fmap databasePoolCapacityValue (mkDatabasePoolCapacity 1) `shouldBe` Just 1
      fmap databasePoolCapacityValue (mkDatabasePoolCapacity 0) `shouldBe` Nothing
      fmap databasePoolCapacityValue (mkDatabasePoolCapacity (-1)) `shouldBe` Nothing

  describe "defaultAppConfig" $ do
    it "reserves structured listener, static asset, and observability settings" $ do
      defaultAppConfig
        `shouldBe` AppConfig
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
                  requestHeadLimits = HarchWeb.unboundedRequestHeadLimits,
                  requestTransportLimits = HarchWeb.warpDefaultRequestTransportLimits,
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
      let serverConfig = HarchWeb.toServerConfig defaultAppConfig
      HarchWeb.listenerConfigs serverConfig `shouldBe` listenerConfigs defaultAppConfig
      HarchWeb.staticAssets serverConfig `shouldBe` staticAssets defaultAppConfig
      HarchWeb.requestPolicy serverConfig `shouldBe` requestPolicy defaultAppConfig
      HarchWeb.observability serverConfig `shouldBe` observability defaultAppConfig

  describe "parseRuntimeAppConfig" $ do
    it "parses committed runtime defaults into the expected app config" $
      parseRuntimeAppConfig committedRuntimeDefaults [] []
        `shouldBe` Right defaultAppConfig

    it "fails when no listeners are configured" $
      parseRuntimeAppConfig
        [("APP_TITLE_PREFIX", "runtime-test")]
        []
        []
        `shouldBe` Left (MissingConfigValue "LISTENER_0_HOST")

    it "parses multiple listeners in deterministic index order" $ do
      let committedDefaults =
            [ ("APP_TITLE_PREFIX", "runtime-test"),
              ("LISTENER_2_SCHEME", "http"),
              ("LISTENER_1_PORT", "5002"),
              ("LISTENER_2_PORT", "5003"),
              ("LISTENER_1_HOST", "127.0.0.2"),
              ("LISTENER_2_HOST", "127.0.0.3"),
              ("LISTENER_1_SCHEME", "http")
            ]
      parseRuntimeAppConfig committedDefaults [] []
        `shouldBe` Right
          AppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = "127.0.0.2",
                      listenerPort = 5002,
                      listenerScheme = Http,
                      listenerTls = Nothing,
                      listenerAcme = Nothing
                    },
                  ListenerConfig
                    { listenerHost = "127.0.0.3",
                      listenerPort = 5003,
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
              requestPolicy = requestPolicy defaultAppConfig,
              observability =
                ObservabilityConfig
                  { tracingExporter = Nothing,
                    metricsExporter = Nothing
                  }
            }

    it "requires HTTPS listeners to specify a TLS source" $
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "0.0.0.0"),
          ("LISTENER_0_PORT", "5443"),
          ("LISTENER_0_SCHEME", "https")
        ]
        []
        []
        `shouldBe` Left (MissingConfigValue "LISTENER_0_TLS_SOURCE")

    it "defaults redirects on and records the HTTPS port when one HTTP and one manual HTTPS listener are configured" $
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5001"),
          ("LISTENER_0_SCHEME", "http"),
          ("LISTENER_1_HOST", "127.0.0.1"),
          ("LISTENER_1_PORT", "5443"),
          ("LISTENER_1_SCHEME", "https"),
          ("LISTENER_1_TLS_SOURCE", "manual"),
          ("LISTENER_1_TLS_CERTIFICATE_FILE", "cert.pem"),
          ("LISTENER_1_TLS_PRIVATE_KEY_FILE", "key.pem")
        ]
        []
        []
        `shouldBe` Right
          defaultAppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5001,
                      listenerScheme = Http,
                      listenerTls = Nothing,
                      listenerAcme = Nothing
                    },
                  ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5443,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                ManualCertificateFiles
                                  ManualTlsCertificateFiles
                                    { certificateFile = "cert.pem",
                                      privateKeyFile = "key.pem"
                                    }
                            },
                      listenerAcme = Nothing
                    }
                ],
              requestPolicy =
                RequestPolicyConfig
                  { redirectHttpToHttps = True,
                    httpsRedirectPort = Just 5443,
                    httpsRedirectAuthority = Just "127.0.0.1",
                    strictTransportSecurity = Nothing,
                    forwardedHeaderTrust = NeverTrustForwarded,
                    requestHeadLimits = HarchWeb.unboundedRequestHeadLimits,
                    requestTransportLimits = HarchWeb.warpDefaultRequestTransportLimits,
                    requestConcurrencyLimit = Nothing,
                    corsPolicy = defaultCorsPolicyConfig,
                    responseSecurityHeaders = defaultResponseSecurityHeadersConfig
                  }
            }

    it "parses shared HTTPS directories and ACME certificate publishing directories" $
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5443"),
          ("LISTENER_0_SCHEME", "https"),
          ("LISTENER_0_TLS_SOURCE", "shared"),
          ("LISTENER_0_TLS_CERTIFICATE_DIRECTORY", "/var/lib/web-api/shared-certs"),
          ("LISTENER_1_HOST", "127.0.0.1"),
          ("LISTENER_1_PORT", "5444"),
          ("LISTENER_1_SCHEME", "https"),
          ("LISTENER_1_TLS_SOURCE", "acme"),
          ("LISTENER_1_ACME_DIRECTORY_URL", "https://acme-staging-v02.api.letsencrypt.org/directory"),
          ("LISTENER_1_ACME_CONTACT_EMAILS", "ops@example.com"),
          ("LISTENER_1_ACME_DOMAINS", "example.com,www.example.com"),
          ("LISTENER_1_ACME_CERTIFICATE_DIRECTORY", "/var/lib/web-api/shared-certs")
        ]
        []
        []
        `shouldBe` Right
          defaultAppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5443,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                SharedCertificateFiles
                                  SharedTlsCertificateFiles
                                    { certificateDirectory = "/var/lib/web-api/shared-certs",
                                      sharedCertificateStartupMode = AwaitCertificateFiles Nothing
                                    }
                            },
                      listenerAcme = Nothing
                    },
                  ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5444,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                AcmeCertificateSource
                                  AcmeConfig
                                    { acmeDirectoryUrl = "https://acme-staging-v02.api.letsencrypt.org/directory",
                                      acmeContactEmails = ["ops@example.com"],
                                      acmeDomains = ["example.com", "www.example.com"],
                                      acmeHttp01Port = 80,
                                      acmeCertificateDirectory = Just "/var/lib/web-api/shared-certs",
                                      acmeCertbotConfig =
                                        CertbotConfig
                                          { certbotExecutable = "certbot",
                                            certbotArguments = []
                                          }
                                    }
                            },
                      listenerAcme = Nothing
                    }
                ],
              requestPolicy = requestPolicy defaultAppConfig
            }

    it "defaults ACME publish directories and shared TLS reuse directories to .tls paths" $ do
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5443"),
          ("LISTENER_0_SCHEME", "https"),
          ("LISTENER_0_TLS_SOURCE", "shared"),
          ("LISTENER_1_HOST", "127.0.0.1"),
          ("LISTENER_1_PORT", "5444"),
          ("LISTENER_1_SCHEME", "https"),
          ("LISTENER_1_TLS_SOURCE", "acme"),
          ("LISTENER_1_ACME_DIRECTORY_URL", "https://acme-staging-v02.api.letsencrypt.org/directory"),
          ("LISTENER_1_ACME_CONTACT_EMAILS", "ops@example.com"),
          ("LISTENER_1_ACME_DOMAINS", "example.com,www.example.com")
        ]
        []
        []
        `shouldBe` Right
          defaultAppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5443,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                SharedCertificateFiles
                                  SharedTlsCertificateFiles
                                    { certificateDirectory = ".tls/example.com",
                                      sharedCertificateStartupMode = AwaitCertificateFiles Nothing
                                    }
                            },
                      listenerAcme = Nothing
                    },
                  ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5444,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                AcmeCertificateSource
                                  AcmeConfig
                                    { acmeDirectoryUrl = "https://acme-staging-v02.api.letsencrypt.org/directory",
                                      acmeContactEmails = ["ops@example.com"],
                                      acmeDomains = ["example.com", "www.example.com"],
                                      acmeHttp01Port = 80,
                                      acmeCertificateDirectory = Just ".tls/example.com",
                                      acmeCertbotConfig =
                                        CertbotConfig
                                          { certbotExecutable = "certbot",
                                            certbotArguments = []
                                          }
                                    }
                            },
                      listenerAcme = Nothing
                    }
                ],
              requestPolicy = requestPolicy defaultAppConfig
            }
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5443"),
          ("LISTENER_0_SCHEME", "https"),
          ("LISTENER_0_TLS_SOURCE", "acme"),
          ("LISTENER_0_ACME_DIRECTORY_URL", "https://acme-v02.api.letsencrypt.org/directory"),
          ("LISTENER_0_ACME_CONTACT_EMAILS", "ops@example.com"),
          ("LISTENER_0_ACME_CERTBOT_ARGUMENTS", "certonly,--webroot,--cert-name,prod/example")
        ]
        []
        []
        `shouldBe` Right
          defaultAppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5443,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                AcmeCertificateSource
                                  AcmeConfig
                                    { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                                      acmeContactEmails = ["ops@example.com"],
                                      acmeDomains = [],
                                      acmeHttp01Port = 80,
                                      acmeCertificateDirectory = Just ".tls/prod/example",
                                      acmeCertbotConfig =
                                        CertbotConfig
                                          { certbotExecutable = "certbot",
                                            certbotArguments = ["certonly", "--webroot", "--cert-name", "prod/example"]
                                          }
                                    }
                            },
                      listenerAcme = Nothing
                    }
                ],
              requestPolicy = requestPolicy defaultAppConfig
            }
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5443"),
          ("LISTENER_0_SCHEME", "https"),
          ("LISTENER_0_TLS_SOURCE", "acme"),
          ("LISTENER_0_ACME_DIRECTORY_URL", "https://acme-v02.api.letsencrypt.org/directory"),
          ("LISTENER_0_ACME_CONTACT_EMAILS", "ops@example.com"),
          ("LISTENER_0_ACME_DOMAINS", "one.example.com"),
          ("LISTENER_1_HOST", "127.0.0.1"),
          ("LISTENER_1_PORT", "5444"),
          ("LISTENER_1_SCHEME", "https"),
          ("LISTENER_1_TLS_SOURCE", "acme"),
          ("LISTENER_1_ACME_DIRECTORY_URL", "https://acme-v02.api.letsencrypt.org/directory"),
          ("LISTENER_1_ACME_CONTACT_EMAILS", "ops@example.com"),
          ("LISTENER_1_ACME_DOMAINS", "two.example.com"),
          ("LISTENER_2_HOST", "127.0.0.1"),
          ("LISTENER_2_PORT", "5445"),
          ("LISTENER_2_SCHEME", "https"),
          ("LISTENER_2_TLS_SOURCE", "shared")
        ]
        []
        []
        `shouldBe` Left (MissingConfigValue "LISTENER_2_TLS_CERTIFICATE_DIRECTORY")

    it "parses explicit shared TLS wait and fail-fast startup modes" $ do
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5443"),
          ("LISTENER_0_SCHEME", "https"),
          ("LISTENER_0_TLS_SOURCE", "shared-wait"),
          ("LISTENER_0_TLS_CERTIFICATE_DIRECTORY", "/var/lib/web-api/shared-certs"),
          ("LISTENER_0_TLS_SHARED_WAIT_SECONDS", "15"),
          ("LISTENER_1_HOST", "127.0.0.1"),
          ("LISTENER_1_PORT", "5444"),
          ("LISTENER_1_SCHEME", "https"),
          ("LISTENER_1_TLS_SOURCE", "shared-fail-fast"),
          ("LISTENER_1_TLS_CERTIFICATE_DIRECTORY", "/var/lib/web-api/preprovisioned-certs")
        ]
        []
        []
        `shouldBe` Right
          defaultAppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5443,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                SharedCertificateFiles
                                  SharedTlsCertificateFiles
                                    { certificateDirectory = "/var/lib/web-api/shared-certs",
                                      sharedCertificateStartupMode = AwaitCertificateFiles (Just 15)
                                    }
                            },
                      listenerAcme = Nothing
                    },
                  ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5444,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                SharedCertificateFiles
                                  SharedTlsCertificateFiles
                                    { certificateDirectory = "/var/lib/web-api/preprovisioned-certs",
                                      sharedCertificateStartupMode = RequireCertificateFiles
                                    }
                            },
                      listenerAcme = Nothing
                    }
                ],
              requestPolicy = requestPolicy defaultAppConfig
            }
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5443"),
          ("LISTENER_0_SCHEME", "https"),
          ("LISTENER_0_TLS_SOURCE", "shared-fail-fast"),
          ("LISTENER_0_TLS_CERTIFICATE_DIRECTORY", "/var/lib/web-api/shared-certs"),
          ("LISTENER_0_TLS_SHARED_WAIT_SECONDS", "15")
        ]
        []
        []
        `shouldBe` Left (InvalidConfigValue "LISTENER_0_TLS_SHARED_WAIT_SECONDS" "15")
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5443"),
          ("LISTENER_0_SCHEME", "https"),
          ("LISTENER_0_TLS_SOURCE", "shared-wait"),
          ("LISTENER_0_TLS_CERTIFICATE_DIRECTORY", "/var/lib/web-api/shared-certs"),
          ("LISTENER_0_TLS_SHARED_WAIT_SECONDS", "-1")
        ]
        []
        []
        `shouldBe` Left (InvalidConfigValue "LISTENER_0_TLS_SHARED_WAIT_SECONDS" "-1")

    it "defaults production ACME directory URLs and redirects on for HTTP ACME producers plus shared HTTPS listener plans" $
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "8080"),
          ("LISTENER_0_SCHEME", "http"),
          ("LISTENER_0_ACME_CONTACT_EMAILS", "ops@example.com"),
          ("LISTENER_0_ACME_DOMAINS", "example.com,www.example.com"),
          ("LISTENER_1_HOST", "127.0.0.1"),
          ("LISTENER_1_PORT", "5443"),
          ("LISTENER_1_SCHEME", "https"),
          ("LISTENER_1_TLS_SOURCE", "shared-wait"),
          ("LISTENER_1_TLS_SHARED_WAIT_SECONDS", "120")
        ]
        []
        []
        `shouldBe` Right
          defaultAppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 8080,
                      listenerScheme = Http,
                      listenerTls = Nothing,
                      listenerAcme =
                        Just
                          AcmeConfig
                            { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                              acmeContactEmails = ["ops@example.com"],
                              acmeDomains = ["example.com", "www.example.com"],
                              acmeHttp01Port = 8080,
                              acmeCertificateDirectory = Just ".tls/example.com",
                              acmeCertbotConfig =
                                CertbotConfig
                                  { certbotExecutable = "certbot",
                                    certbotArguments = []
                                  }
                            }
                    },
                  ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5443,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                SharedCertificateFiles
                                  SharedTlsCertificateFiles
                                    { certificateDirectory = ".tls/example.com",
                                      sharedCertificateStartupMode = AwaitCertificateFiles (Just 120)
                                    }
                            },
                      listenerAcme = Nothing
                    }
                ],
              requestPolicy =
                RequestPolicyConfig
                  { redirectHttpToHttps = True,
                    httpsRedirectPort = Just 5443,
                    httpsRedirectAuthority = Just "127.0.0.1",
                    strictTransportSecurity = Nothing,
                    forwardedHeaderTrust = NeverTrustForwarded,
                    requestHeadLimits = HarchWeb.unboundedRequestHeadLimits,
                    requestTransportLimits = HarchWeb.warpDefaultRequestTransportLimits,
                    requestConcurrencyLimit = Nothing,
                    corsPolicy = defaultCorsPolicyConfig,
                    responseSecurityHeaders = defaultResponseSecurityHeadersConfig
                  }
            }

    it "parses manual and ACME-backed HTTPS listeners distinctly" $ do
      let committedDefaults =
            [ ("APP_TITLE_PREFIX", "runtime-test"),
              ("LISTENER_BAD_HOST", "ignored-host"),
              ("LISTENER_0_HOST", "0.0.0.0"),
              ("LISTENER_0_PORT", "5443"),
              ("LISTENER_0_SCHEME", "https"),
              ("LISTENER_0_TLS_SOURCE", "manual"),
              ("LISTENER_0_TLS_CERTIFICATE_FILE", "cert.pem"),
              ("LISTENER_0_TLS_PRIVATE_KEY_FILE", "key.pem"),
              ("LISTENER_1_HOST", "0.0.0.0"),
              ("LISTENER_1_PORT", "5444"),
              ("LISTENER_1_SCHEME", "https"),
              ("LISTENER_1_TLS_SOURCE", "acme"),
              ("LISTENER_1_ACME_DIRECTORY_URL", "https://acme-staging-v02.api.letsencrypt.org/directory"),
              ("LISTENER_1_ACME_CONTACT_EMAILS", "ops@example.com,alerts@example.com"),
              ("LISTENER_1_ACME_DOMAINS", "example.com,www.example.com"),
              ("LISTENER_2_HOST", "0.0.0.0"),
              ("LISTENER_2_PORT", "5445"),
              ("LISTENER_2_SCHEME", "https"),
              ("LISTENER_2_TLS_SOURCE", "acme"),
              ("LISTENER_2_ACME_DIRECTORY_URL", "https://acme-v02.api.letsencrypt.org/directory"),
              ("LISTENER_2_ACME_CONTACT_EMAILS", "ops@example.com"),
              ("LISTENER_2_ACME_DOMAINS", "example.com"),
              ("LISTENER_2_ACME_CERTBOT_ARGUMENTS", "certonly,--webroot,--agree-tos")
            ]
      parseRuntimeAppConfig committedDefaults [] []
        `shouldBe` Right
          AppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = "0.0.0.0",
                      listenerPort = 5443,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                ManualCertificateFiles
                                  ManualTlsCertificateFiles
                                    { certificateFile = "cert.pem",
                                      privateKeyFile = "key.pem"
                                    }
                            },
                      listenerAcme = Nothing
                    },
                  ListenerConfig
                    { listenerHost = "0.0.0.0",
                      listenerPort = 5444,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                AcmeCertificateSource
                                  AcmeConfig
                                    { acmeDirectoryUrl = "https://acme-staging-v02.api.letsencrypt.org/directory",
                                      acmeContactEmails = ["ops@example.com", "alerts@example.com"],
                                      acmeDomains = ["example.com", "www.example.com"],
                                      acmeHttp01Port = 80,
                                      acmeCertificateDirectory = Just ".tls/example.com",
                                      acmeCertbotConfig =
                                        CertbotConfig
                                          { certbotExecutable = "certbot",
                                            certbotArguments = []
                                          }
                                    }
                            },
                      listenerAcme = Nothing
                    },
                  ListenerConfig
                    { listenerHost = "0.0.0.0",
                      listenerPort = 5445,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                AcmeCertificateSource
                                  AcmeConfig
                                    { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                                      acmeContactEmails = ["ops@example.com"],
                                      acmeDomains = ["example.com"],
                                      acmeHttp01Port = 80,
                                      acmeCertificateDirectory = Just ".tls/example.com",
                                      acmeCertbotConfig =
                                        CertbotConfig
                                          { certbotExecutable = "certbot",
                                            certbotArguments = ["certonly", "--webroot", "--agree-tos"]
                                          }
                                    }
                            },
                      listenerAcme = Nothing
                    }
                ],
              staticAssets =
                StaticAssetsConfig
                  { staticAssetRoots = [],
                    staticAssetContentTypes = defaultStaticAssetContentTypes,
                    staticCacheControlSeconds = Nothing
                  },
              requestPolicy = requestPolicy defaultAppConfig,
              observability =
                ObservabilityConfig
                  { tracingExporter = Nothing,
                    metricsExporter = Nothing
                  }
            }

    it "rejects invalid listener scheme and TLS source values" $ do
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "0.0.0.0"),
          ("LISTENER_0_PORT", "5443"),
          ("LISTENER_0_SCHEME", "tcp")
        ]
        []
        []
        `shouldBe` Left (InvalidConfigValue "LISTENER_0_SCHEME" "tcp")
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "0.0.0.0"),
          ("LISTENER_0_PORT", "5443"),
          ("LISTENER_0_SCHEME", "https"),
          ("LISTENER_0_TLS_SOURCE", "vault")
        ]
        []
        []
        `shouldBe` Left (InvalidConfigValue "LISTENER_0_TLS_SOURCE" "vault")

    it "parses static asset roots and cache policy into the expected config" $ do
      let committedDefaults =
            [ ("APP_TITLE_PREFIX", "runtime-test"),
              ("LISTENER_0_HOST", "127.0.0.1"),
              ("LISTENER_0_PORT", "5001"),
              ("LISTENER_0_SCHEME", "http"),
              ("STATIC_ASSET_ROOT_2_DIRECTORY", "vendor/public"),
              ("STATIC_ASSET_ROOT_1_URL_PREFIX", "/assets"),
              ("STATIC_ASSET_ROOT_2_URL_PREFIX", "/vendor"),
              ("STATIC_ASSET_ROOT_1_DIRECTORY", "public"),
              ("STATIC_CACHE_CONTROL_SECONDS", "3600")
            ]
      parseRuntimeAppConfig committedDefaults [] []
        `shouldBe` Right
          AppConfig
            { appTitlePrefix = "runtime-test",
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
                  { staticAssetRoots =
                      [ StaticAssetRoot
                          { staticUrlPrefix = "/assets",
                            staticDirectory = "public"
                          },
                        StaticAssetRoot
                          { staticUrlPrefix = "/vendor",
                            staticDirectory = "vendor/public"
                          }
                      ],
                    staticAssetContentTypes = defaultStaticAssetContentTypes,
                    staticCacheControlSeconds = Just 3600
                  },
              requestPolicy = requestPolicy defaultAppConfig,
              observability =
                ObservabilityConfig
                  { tracingExporter = Nothing,
                    metricsExporter = Nothing
                  }
            }

    it "parses numbered static asset content type entries including extensionless opt-in" $ do
      let committedDefaults =
            [ ("APP_TITLE_PREFIX", "runtime-test"),
              ("LISTENER_0_HOST", "127.0.0.1"),
              ("LISTENER_0_PORT", "5001"),
              ("LISTENER_0_SCHEME", "http"),
              ("STATIC_ASSET_CONTENT_TYPE_1_EXTENSION", ".wasm"),
              ("STATIC_ASSET_CONTENT_TYPE_1_MIME_TYPE", "application/wasm"),
              ("STATIC_ASSET_CONTENT_TYPE_2_EXTENSION", ""),
              ("STATIC_ASSET_CONTENT_TYPE_2_MIME_TYPE", "application/octet-stream")
            ]
      fmap (staticAssetContentTypes . staticAssets) (parseRuntimeAppConfig committedDefaults [] [])
        `shouldBe` Right
          [ (".wasm", "application/wasm"),
            ("", "application/octet-stream")
          ]

    it "parses redirect and HSTS request policy values for TLS-offload deployments" $
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [ ("REDIRECT_HTTP_TO_HTTPS", "true"),
          ("HSTS_MAX_AGE_SECONDS", "31536000"),
          ("HSTS_INCLUDE_SUBDOMAINS", "true"),
          ("HSTS_PRELOAD", "true")
        ]
        `shouldBe` Right
          defaultAppConfig
            { requestPolicy =
                RequestPolicyConfig
                  { redirectHttpToHttps = True,
                    httpsRedirectPort = Nothing,
                    httpsRedirectAuthority = Nothing,
                    strictTransportSecurity =
                      Just
                        StrictTransportSecurityConfig
                          { strictTransportSecurityMaxAgeSeconds = 31536000,
                            strictTransportSecurityIncludeSubDomains = True,
                            strictTransportSecurityPreload = True
                          },
                    forwardedHeaderTrust = NeverTrustForwarded,
                    requestHeadLimits = HarchWeb.unboundedRequestHeadLimits,
                    requestTransportLimits = HarchWeb.warpDefaultRequestTransportLimits,
                    requestConcurrencyLimit = Nothing,
                    corsPolicy = defaultCorsPolicyConfig,
                    responseSecurityHeaders = defaultResponseSecurityHeadersConfig
                  }
            }

    it "parses explicit false redirect and HSTS flags without changing the default policy shape" $
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [ ("REDIRECT_HTTP_TO_HTTPS", "false"),
          ("HSTS_MAX_AGE_SECONDS", "86400"),
          ("HSTS_INCLUDE_SUBDOMAINS", "false"),
          ("HSTS_PRELOAD", "false")
        ]
        `shouldBe` Right
          defaultAppConfig
            { requestPolicy =
                RequestPolicyConfig
                  { redirectHttpToHttps = False,
                    httpsRedirectPort = Nothing,
                    httpsRedirectAuthority = Nothing,
                    strictTransportSecurity =
                      Just
                        StrictTransportSecurityConfig
                          { strictTransportSecurityMaxAgeSeconds = 86400,
                            strictTransportSecurityIncludeSubDomains = False,
                            strictTransportSecurityPreload = False
                          },
                    forwardedHeaderTrust = NeverTrustForwarded,
                    requestHeadLimits = HarchWeb.unboundedRequestHeadLimits,
                    requestTransportLimits = HarchWeb.warpDefaultRequestTransportLimits,
                    requestConcurrencyLimit = Nothing,
                    corsPolicy = defaultCorsPolicyConfig,
                    responseSecurityHeaders = defaultResponseSecurityHeadersConfig
                  }
            }

    it "defaults optional HSTS booleans to false when only max-age is configured" $
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [("HSTS_MAX_AGE_SECONDS", "86400")]
        `shouldBe` Right
          defaultAppConfig
            { requestPolicy =
                RequestPolicyConfig
                  { redirectHttpToHttps = False,
                    httpsRedirectPort = Nothing,
                    httpsRedirectAuthority = Nothing,
                    strictTransportSecurity =
                      Just
                        StrictTransportSecurityConfig
                          { strictTransportSecurityMaxAgeSeconds = 86400,
                            strictTransportSecurityIncludeSubDomains = False,
                            strictTransportSecurityPreload = False
                          },
                    forwardedHeaderTrust = NeverTrustForwarded,
                    requestHeadLimits = HarchWeb.unboundedRequestHeadLimits,
                    requestTransportLimits = HarchWeb.warpDefaultRequestTransportLimits,
                    requestConcurrencyLimit = Nothing,
                    corsPolicy = defaultCorsPolicyConfig,
                    responseSecurityHeaders = defaultResponseSecurityHeadersConfig
                  }
            }

    it "parses a single trusted forwarded-proxy CIDR block" $
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [("TRUSTED_FORWARDED_PROXIES", "10.0.0.0/8")]
        `shouldBe` Right
          defaultAppConfig
            { requestPolicy =
                (requestPolicy defaultAppConfig)
                  { forwardedHeaderTrust =
                      TrustForwardedFrom (requiredTestCidrBlock "10.0.0.0/8" :| [])
                  }
            }

    it "parses multiple comma-separated trusted forwarded-proxy CIDR blocks" $
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [("TRUSTED_FORWARDED_PROXIES", "10.0.0.0/8, 172.16.0.0/12")]
        `shouldBe` Right
          defaultAppConfig
            { requestPolicy =
                (requestPolicy defaultAppConfig)
                  { forwardedHeaderTrust =
                      TrustForwardedFrom (requiredTestCidrBlock "10.0.0.0/8" :| [requiredTestCidrBlock "172.16.0.0/12"])
                  }
            }

    it "rejects a malformed trusted forwarded-proxy CIDR block" $
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [("TRUSTED_FORWARDED_PROXIES", "10.0.0.0/8, not-a-cidr")]
        `shouldBe` Left (InvalidConfigValue "TRUSTED_FORWARDED_PROXIES" "not-a-cidr")

    it "parses opt-in request-head resource limits without changing defaults" $
      fmap
        requestPolicy
        ( parseRuntimeAppConfig
            committedRuntimeDefaults
            []
            [ ("REQUEST_TARGET_MAX_BYTES", "2048"),
              ("REQUEST_HEADER_MAX_BYTES", "8192"),
              ("REQUEST_HEADER_MAX_COUNT", "32"),
              ("REQUEST_HEADER_VALUE_MAX_BYTES", "1024"),
              ("REQUEST_COOKIE_MAX_COUNT", "4"),
              ("REQUEST_COOKIE_NAME_MAX_BYTES", "64"),
              ("REQUEST_COOKIE_VALUE_MAX_BYTES", "512"),
              ("REQUEST_PATH_SEGMENT_MAX_COUNT", "8"),
              ("REQUEST_PATH_SEGMENT_MAX_BYTES", "128"),
              ("REQUEST_QUERY_FIELD_MAX_COUNT", "16"),
              ("REQUEST_QUERY_FIELD_MAX_BYTES", "256"),
              ("REQUEST_NETWORK_TIMEOUT_SECONDS", "12"),
              ("REQUEST_SLOWLORIS_MAX_BYTES", "512"),
              ("REQUEST_MAX_CONCURRENT", "64")
            ]
        )
        `shouldBe` Right
          (requestPolicy defaultAppConfig)
            { requestHeadLimits =
                HarchWeb.RequestHeadLimits
                  { HarchWeb.requestTargetByteLimit = HarchWeb.requestByteLimit 2048,
                    HarchWeb.requestHeaderByteLimit = HarchWeb.requestByteLimit 8192,
                    HarchWeb.requestHeaderCountLimit = HarchWeb.mkRequestHeaderCountLimit 32,
                    HarchWeb.requestHeaderValueByteLimit = HarchWeb.requestByteLimit 1024,
                    HarchWeb.requestCookieCountLimit = HarchWeb.requestItemCountLimit 4,
                    HarchWeb.requestCookieNameByteLimit = HarchWeb.requestByteLimit 64,
                    HarchWeb.requestCookieValueByteLimit = HarchWeb.requestByteLimit 512,
                    HarchWeb.requestPathSegmentCountLimit = HarchWeb.requestItemCountLimit 8,
                    HarchWeb.requestPathSegmentByteLimit = HarchWeb.requestByteLimit 128,
                    HarchWeb.requestQueryFieldCountLimit = HarchWeb.requestItemCountLimit 16,
                    HarchWeb.requestQueryFieldByteLimit = HarchWeb.requestByteLimit 256
                  },
              requestTransportLimits =
                HarchWeb.RequestTransportLimits
                  { HarchWeb.requestNetworkTimeout = HarchWeb.requestTimeoutSeconds 12,
                    HarchWeb.requestSlowlorisByteLimit = HarchWeb.requestByteLimit 512
                  },
              requestConcurrencyLimit = HarchWeb.mkRequestConcurrencyLimit 64
            }

    it "names each invalid request-limit setting in its configuration error" $
      expectAll
        ( ( parseRuntimeAppConfig committedRuntimeDefaults [] [("REQUEST_TARGET_MAX_BYTES", "not-a-number")]
              `shouldBe` Left (InvalidConfigValue "REQUEST_TARGET_MAX_BYTES" "not-a-number")
          )
            :| [ parseRuntimeAppConfig committedRuntimeDefaults [] [("REQUEST_NETWORK_TIMEOUT_SECONDS", "not-a-number")]
                   `shouldBe` Left (InvalidConfigValue "REQUEST_NETWORK_TIMEOUT_SECONDS" "not-a-number"),
                 parseRuntimeAppConfig committedRuntimeDefaults [] [("REQUEST_COOKIE_MAX_COUNT", "not-a-number")]
                   `shouldBe` Left (InvalidConfigValue "REQUEST_COOKIE_MAX_COUNT" "not-a-number"),
                 parseRuntimeAppConfig committedRuntimeDefaults [] [("REQUEST_HEADER_MAX_COUNT", "not-a-number")]
                   `shouldBe` Left (InvalidConfigValue "REQUEST_HEADER_MAX_COUNT" "not-a-number"),
                 parseRuntimeAppConfig committedRuntimeDefaults [] [("REQUEST_PATH_SEGMENT_MAX_COUNT", "not-a-number")]
                   `shouldBe` Left (InvalidConfigValue "REQUEST_PATH_SEGMENT_MAX_COUNT" "not-a-number"),
                 parseRuntimeAppConfig committedRuntimeDefaults [] [("REQUEST_MAX_CONCURRENT", "not-a-number")]
                   `shouldBe` Left (InvalidConfigValue "REQUEST_MAX_CONCURRENT" "not-a-number"),
                 parseRuntimeAppConfig committedRuntimeDefaults [] [("REQUEST_MAX_CONCURRENT", "0")]
                   `shouldBe` Left (InvalidConfigValue "REQUEST_MAX_CONCURRENT" "0"),
                 parseRuntimeAppConfig committedRuntimeDefaults [] [("REQUEST_TARGET_MAX_BYTES", "-1")]
                   `shouldBe` Left (InvalidConfigValue "REQUEST_TARGET_MAX_BYTES" "-1"),
                 parseRuntimeAppConfig committedRuntimeDefaults [] [("REQUEST_NETWORK_TIMEOUT_SECONDS", "-1")]
                   `shouldBe` Left (InvalidConfigValue "REQUEST_NETWORK_TIMEOUT_SECONDS" "-1"),
                 parseRuntimeAppConfig committedRuntimeDefaults [] [("REQUEST_HEADER_MAX_COUNT", "-1")]
                   `shouldBe` Left (InvalidConfigValue "REQUEST_HEADER_MAX_COUNT" "-1"),
                 parseRuntimeAppConfig committedRuntimeDefaults [] [("REQUEST_COOKIE_VALUE_MAX_BYTES", "-1")]
                   `shouldBe` Left (InvalidConfigValue "REQUEST_COOKIE_VALUE_MAX_BYTES" "-1"),
                 parseRuntimeAppConfig committedRuntimeDefaults [] [("REQUEST_PATH_SEGMENT_MAX_COUNT", "-1")]
                   `shouldBe` Left (InvalidConfigValue "REQUEST_PATH_SEGMENT_MAX_COUNT" "-1")
               ]
        )

    it "parses CORS and response security policy overrides" $
      fmap
        requestPolicy
        ( parseRuntimeAppConfig
            committedRuntimeDefaults
            []
            [ ("CORS_ALLOWED_ORIGINS", "https://app.example.com, https://admin.example.com"),
              ("CORS_ALLOWED_METHODS", "GET, HEAD"),
              ("CORS_ALLOWED_HEADERS", "Content-Type, X-Requested-With"),
              ("CORS_MAX_AGE_SECONDS", "600"),
              ("CONTENT_SECURITY_POLICY", "default-src 'self'; connect-src 'self' https://collector.example.com"),
              ("X_CONTENT_TYPE_OPTIONS_NOSNIFF", "false"),
              ("X_XSS_PROTECTION", "0"),
              ("REFERRER_POLICY", "no-referrer"),
              ("PERMISSIONS_POLICY", "camera=()"),
              ("X_FRAME_OPTIONS", "SAMEORIGIN")
            ]
        )
        `shouldBe` Right
          ( (requestPolicy defaultAppConfig)
              { corsPolicy =
                  CorsPolicyConfig
                    { corsAllowedOrigins = ["https://app.example.com", "https://admin.example.com"],
                      corsAllowedMethods = ["GET", "HEAD"],
                      corsAllowedHeaders = ["Content-Type", "X-Requested-With"],
                      corsMaxAgeSeconds = Just 600
                    },
                responseSecurityHeaders =
                  ResponseSecurityHeadersConfig
                    { contentSecurityPolicy = Just "default-src 'self'; connect-src 'self' https://collector.example.com",
                      contentTypeOptionsNoSniff = False,
                      xssProtection = Just "0",
                      referrerPolicy = Just "no-referrer",
                      permissionsPolicy = Just "camera=()",
                      frameOptions = Just "SAMEORIGIN"
                    }
              }
          )

    it "lets REDIRECT_HTTP_TO_HTTPS=false disable the listener-aware default for dual listeners" $
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5001"),
          ("LISTENER_0_SCHEME", "http"),
          ("LISTENER_1_HOST", "127.0.0.1"),
          ("LISTENER_1_PORT", "5443"),
          ("LISTENER_1_SCHEME", "https"),
          ("LISTENER_1_TLS_SOURCE", "manual"),
          ("LISTENER_1_TLS_CERTIFICATE_FILE", "cert.pem"),
          ("LISTENER_1_TLS_PRIVATE_KEY_FILE", "key.pem")
        ]
        []
        [("REDIRECT_HTTP_TO_HTTPS", "false")]
        `shouldBe` Right
          defaultAppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5001,
                      listenerScheme = Http,
                      listenerTls = Nothing,
                      listenerAcme = Nothing
                    },
                  ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5443,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                ManualCertificateFiles
                                  ManualTlsCertificateFiles
                                    { certificateFile = "cert.pem",
                                      privateKeyFile = "key.pem"
                                    }
                            },
                      listenerAcme = Nothing
                    }
                ],
              requestPolicy =
                RequestPolicyConfig
                  { redirectHttpToHttps = False,
                    httpsRedirectPort = Just 5443,
                    httpsRedirectAuthority = Just "127.0.0.1",
                    strictTransportSecurity = Nothing,
                    forwardedHeaderTrust = NeverTrustForwarded,
                    requestHeadLimits = HarchWeb.unboundedRequestHeadLimits,
                    requestTransportLimits = HarchWeb.warpDefaultRequestTransportLimits,
                    requestConcurrencyLimit = Nothing,
                    corsPolicy = defaultCorsPolicyConfig,
                    responseSecurityHeaders = defaultResponseSecurityHeadersConfig
                  }
            }

    it "keeps redirects on but leaves the redirect port implicit when multiple HTTPS ports exist" $
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5001"),
          ("LISTENER_0_SCHEME", "http"),
          ("LISTENER_1_HOST", "127.0.0.1"),
          ("LISTENER_1_PORT", "443"),
          ("LISTENER_1_SCHEME", "https"),
          ("LISTENER_1_TLS_SOURCE", "manual"),
          ("LISTENER_1_TLS_CERTIFICATE_FILE", "https-443-cert.pem"),
          ("LISTENER_1_TLS_PRIVATE_KEY_FILE", "https-443-key.pem"),
          ("LISTENER_2_HOST", "127.0.0.1"),
          ("LISTENER_2_PORT", "5443"),
          ("LISTENER_2_SCHEME", "https"),
          ("LISTENER_2_TLS_SOURCE", "manual"),
          ("LISTENER_2_TLS_CERTIFICATE_FILE", "https-5443-cert.pem"),
          ("LISTENER_2_TLS_PRIVATE_KEY_FILE", "https-5443-key.pem")
        ]
        []
        []
        `shouldBe` Right
          defaultAppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5001,
                      listenerScheme = Http,
                      listenerTls = Nothing,
                      listenerAcme = Nothing
                    },
                  ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 443,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                ManualCertificateFiles
                                  ManualTlsCertificateFiles
                                    { certificateFile = "https-443-cert.pem",
                                      privateKeyFile = "https-443-key.pem"
                                    }
                            },
                      listenerAcme = Nothing
                    },
                  ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5443,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                ManualCertificateFiles
                                  ManualTlsCertificateFiles
                                    { certificateFile = "https-5443-cert.pem",
                                      privateKeyFile = "https-5443-key.pem"
                                    }
                            },
                      listenerAcme = Nothing
                    }
                ],
              requestPolicy =
                RequestPolicyConfig
                  { redirectHttpToHttps = True,
                    httpsRedirectPort = Nothing,
                    httpsRedirectAuthority = Just "127.0.0.1",
                    strictTransportSecurity = Nothing,
                    forwardedHeaderTrust = NeverTrustForwarded,
                    requestHeadLimits = HarchWeb.unboundedRequestHeadLimits,
                    requestTransportLimits = HarchWeb.warpDefaultRequestTransportLimits,
                    requestConcurrencyLimit = Nothing,
                    corsPolicy = defaultCorsPolicyConfig,
                    responseSecurityHeaders = defaultResponseSecurityHeadersConfig
                  }
            }

    it "parses tracing and metrics exporters independently while preserving header order" $ do
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [ ("OTLP_TRACING_ENDPOINT", "http://collector:4318/v1/traces"),
          ("OTLP_TRACING_HEADERS", "authorization=Bearer token;x-api-key=secret")
        ]
        `shouldBe` Right
          defaultAppConfig
            { observability =
                ObservabilityConfig
                  { tracingExporter =
                      Just
                        OtlpExporter
                          { otlpEndpoint = "http://collector:4318/v1/traces",
                            otlpHeaders =
                              [ ("authorization", "Bearer token"),
                                ("x-api-key", "secret")
                              ]
                          },
                    metricsExporter = Nothing
                  }
            }
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [("OTLP_TRACING_ENDPOINT", "http://collector:4318/v1/traces")]
        `shouldBe` Right
          defaultAppConfig
            { observability =
                ObservabilityConfig
                  { tracingExporter =
                      Just
                        OtlpExporter
                          { otlpEndpoint = "http://collector:4318/v1/traces",
                            otlpHeaders = []
                          },
                    metricsExporter = Nothing
                  }
            }
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [ ("OTLP_TRACING_ENABLED", "true"),
          ("OTLP_TRACING_HEADERS", "authorization=Bearer token")
        ]
        `shouldBe` Right
          defaultAppConfig
            { observability =
                ObservabilityConfig
                  { tracingExporter =
                      Just
                        OtlpExporter
                          { otlpEndpoint = "http://127.0.0.1:4318/v1/traces",
                            otlpHeaders = [("authorization", "Bearer token")]
                          },
                    metricsExporter = Nothing
                  }
            }
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [("OTLP_TRACING_ENABLED", "true")]
        `shouldBe` Right
          defaultAppConfig
            { observability =
                ObservabilityConfig
                  { tracingExporter =
                      Just
                        OtlpExporter
                          { otlpEndpoint = "http://127.0.0.1:4318/v1/traces",
                            otlpHeaders = []
                          },
                    metricsExporter = Nothing
                  }
            }
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [ ("OTLP_TRACING_ENABLED", "false"),
          ("OTLP_TRACING_ENDPOINT", "http://collector:4318/v1/traces"),
          ("OTLP_TRACING_HEADERS", "authorization=Bearer token")
        ]
        `shouldBe` Right
          defaultAppConfig
            { observability =
                ObservabilityConfig
                  { tracingExporter = Nothing,
                    metricsExporter = Nothing
                  }
            }
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [ ("OTLP_METRICS_ENDPOINT", "http://collector:4318/v1/metrics"),
          ("OTLP_METRICS_HEADERS", "x-scope=metrics;broken-entry")
        ]
        `shouldBe` Left
          ( InvalidConfigValue
              "OTLP_METRICS_HEADERS"
              "x-scope=metrics;broken-entry"
          )

    -- The three Right-case its below were split out of what was one
    -- "fails invalid runtime values with explicit errors" it: each
    -- asserts a full AppConfig, a different act from the Left-error
    -- table that follows, and was only ever a fail-fast do-block
    -- statement rather than its own named case.
    it "parses an explicit ACME certbot executable override" $
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5001"),
          ("LISTENER_0_SCHEME", "https"),
          ("LISTENER_0_TLS_SOURCE", "acme"),
          ("LISTENER_0_ACME_CONTACT_EMAILS", "ops@example.com"),
          ("LISTENER_0_ACME_DOMAINS", "example.com,www.example.com"),
          ("LISTENER_0_ACME_CERTBOT_EXECUTABLE", "certbot")
        ]
        []
        []
        `shouldBe` Right
          AppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5001,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                AcmeCertificateSource
                                  AcmeConfig
                                    { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                                      acmeContactEmails = ["ops@example.com"],
                                      acmeDomains = ["example.com", "www.example.com"],
                                      acmeHttp01Port = 80,
                                      acmeCertificateDirectory = Just ".tls/example.com",
                                      acmeCertbotConfig =
                                        CertbotConfig
                                          { certbotExecutable = "certbot",
                                            certbotArguments = []
                                          }
                                    }
                            },
                      listenerAcme = Nothing
                    }
                ],
              staticAssets =
                StaticAssetsConfig
                  { staticAssetRoots = [],
                    staticAssetContentTypes = defaultStaticAssetContentTypes,
                    staticCacheControlSeconds = Nothing
                  },
              requestPolicy = requestPolicy defaultAppConfig,
              observability =
                ObservabilityConfig
                  { tracingExporter = Nothing,
                    metricsExporter = Nothing
                  }
            }

    it "defaults the ACME certbot executable when not overridden" $
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5001"),
          ("LISTENER_0_SCHEME", "https"),
          ("LISTENER_0_TLS_SOURCE", "acme"),
          ("LISTENER_0_ACME_CONTACT_EMAILS", "ops@example.com"),
          ("LISTENER_0_ACME_DOMAINS", "example.com,www.example.com")
        ]
        []
        []
        `shouldBe` Right
          AppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5001,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                AcmeCertificateSource
                                  AcmeConfig
                                    { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                                      acmeContactEmails = ["ops@example.com"],
                                      acmeDomains = ["example.com", "www.example.com"],
                                      acmeHttp01Port = 80,
                                      acmeCertificateDirectory = Just ".tls/example.com",
                                      acmeCertbotConfig =
                                        CertbotConfig
                                          { certbotExecutable = "certbot",
                                            certbotArguments = []
                                          }
                                    }
                            },
                      listenerAcme = Nothing
                    }
                ],
              staticAssets =
                StaticAssetsConfig
                  { staticAssetRoots = [],
                    staticAssetContentTypes = defaultStaticAssetContentTypes,
                    staticCacheControlSeconds = Nothing
                  },
              requestPolicy = requestPolicy defaultAppConfig,
              observability =
                ObservabilityConfig
                  { tracingExporter = Nothing,
                    metricsExporter = Nothing
                  }
            }

    it "defaults the ACME certificate directory to a listener-indexed path when no domains are configured" $
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5001"),
          ("LISTENER_0_SCHEME", "https"),
          ("LISTENER_0_TLS_SOURCE", "acme"),
          ("LISTENER_0_ACME_CONTACT_EMAILS", "ops@example.com")
        ]
        []
        []
        `shouldBe` Right
          AppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5001,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                AcmeCertificateSource
                                  AcmeConfig
                                    { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                                      acmeContactEmails = ["ops@example.com"],
                                      acmeDomains = [],
                                      acmeHttp01Port = 80,
                                      acmeCertificateDirectory = Just ".tls/listener-0",
                                      acmeCertbotConfig =
                                        CertbotConfig
                                          { certbotExecutable = "certbot",
                                            certbotArguments = []
                                          }
                                    }
                            },
                      listenerAcme = Nothing
                    }
                ],
              staticAssets =
                StaticAssetsConfig
                  { staticAssetRoots = [],
                    staticAssetContentTypes = defaultStaticAssetContentTypes,
                    staticCacheControlSeconds = Nothing
                  },
              requestPolicy = requestPolicy defaultAppConfig,
              observability =
                ObservabilityConfig
                  { tracingExporter = Nothing,
                    metricsExporter = Nothing
                  }
            }

    -- Tabled per docs/design-guidance.md's CN decision record: one act
    -- (parseRuntimeAppConfig against three env-pair lists), one
    -- comparison against a Left ConfigParseError, differing only in the
    -- env pairs and the expected error. Extracted from what was one
    -- 284-line "fails invalid runtime values with explicit errors" it
    -- whose fail-fast do-block statements silently stopped reporting
    -- after the first failing row; each row is now independently
    -- reported.
    [ ("rejects a zero listener port", [("APP_TITLE_PREFIX", "runtime-test"), ("LISTENER_0_HOST", "127.0.0.1"), ("LISTENER_0_PORT", "0"), ("LISTENER_0_SCHEME", "http")], [], [], InvalidConfigValue "LISTENER_0_PORT" "0"),
      ("rejects an ACME listener with an empty ACME_CONTACT_EMAILS value", [("APP_TITLE_PREFIX", "runtime-test"), ("LISTENER_0_HOST", "127.0.0.1"), ("LISTENER_0_PORT", "5001"), ("LISTENER_0_SCHEME", "https"), ("LISTENER_0_TLS_SOURCE", "acme"), ("LISTENER_0_ACME_DIRECTORY_URL", "https://acme-v02.api.letsencrypt.org/directory"), ("LISTENER_0_ACME_CONTACT_EMAILS", ""), ("LISTENER_0_ACME_DOMAINS", "")], [], [], InvalidConfigValue "LISTENER_0_ACME_CONTACT_EMAILS" ""),
      ("rejects an ACME listener with an empty ACME_DOMAINS value", [("APP_TITLE_PREFIX", "runtime-test"), ("LISTENER_0_HOST", "127.0.0.1"), ("LISTENER_0_PORT", "5001"), ("LISTENER_0_SCHEME", "https"), ("LISTENER_0_TLS_SOURCE", "acme"), ("LISTENER_0_ACME_DIRECTORY_URL", "https://acme-v02.api.letsencrypt.org/directory"), ("LISTENER_0_ACME_CONTACT_EMAILS", "ops@example.com"), ("LISTENER_0_ACME_DOMAINS", "")], [], [], InvalidConfigValue "LISTENER_0_ACME_DOMAINS" ""),
      ("rejects an unrecognized ACME_CHALLENGE_BACKEND value", [("APP_TITLE_PREFIX", "runtime-test"), ("LISTENER_0_HOST", "127.0.0.1"), ("LISTENER_0_PORT", "5001"), ("LISTENER_0_SCHEME", "https"), ("LISTENER_0_TLS_SOURCE", "acme"), ("LISTENER_0_ACME_DIRECTORY_URL", "https://acme-v02.api.letsencrypt.org/directory"), ("LISTENER_0_ACME_CONTACT_EMAILS", "ops@example.com"), ("LISTENER_0_ACME_CHALLENGE_BACKEND", "shell-script")], [], [], InvalidConfigValue "LISTENER_0_ACME_CHALLENGE_BACKEND" "shell-script"),
      ("rejects a negative STATIC_CACHE_CONTROL_SECONDS value", [("APP_TITLE_PREFIX", "runtime-test"), ("LISTENER_0_HOST", "127.0.0.1"), ("LISTENER_0_PORT", "5001"), ("LISTENER_0_SCHEME", "http"), ("STATIC_CACHE_CONTROL_SECONDS", "-1")], [], [], InvalidConfigValue "STATIC_CACHE_CONTROL_SECONDS" "-1"),
      ("rejects a static asset content type extension without a leading dot", committedRuntimeDefaults, [], [("STATIC_ASSET_CONTENT_TYPE_1_EXTENSION", "wasm"), ("STATIC_ASSET_CONTENT_TYPE_1_MIME_TYPE", "application/wasm")], InvalidConfigValue "STATIC_ASSET_CONTENT_TYPE_1_EXTENSION" "wasm"),
      ("rejects a static asset content type with an empty MIME type", committedRuntimeDefaults, [], [("STATIC_ASSET_CONTENT_TYPE_1_EXTENSION", ".wasm"), ("STATIC_ASSET_CONTENT_TYPE_1_MIME_TYPE", "")], InvalidConfigValue "STATIC_ASSET_CONTENT_TYPE_1_MIME_TYPE" ""),
      ("requires OTLP_TRACING_ENDPOINT when OTLP tracing headers are configured", committedRuntimeDefaults, [], [("OTLP_TRACING_HEADERS", "authorization=Bearer token")], MissingConfigValue "OTLP_TRACING_ENDPOINT"),
      ("rejects a non-boolean OTLP_TRACING_ENABLED value", committedRuntimeDefaults, [], [("OTLP_TRACING_ENABLED", "maybe")], InvalidConfigValue "OTLP_TRACING_ENABLED" "maybe"),
      ("rejects a non-boolean REDIRECT_HTTP_TO_HTTPS value", committedRuntimeDefaults, [], [("REDIRECT_HTTP_TO_HTTPS", "maybe")], InvalidConfigValue "REDIRECT_HTTP_TO_HTTPS" "maybe"),
      ("requires HSTS_MAX_AGE_SECONDS when HSTS_INCLUDE_SUBDOMAINS is set", committedRuntimeDefaults, [], [("HSTS_INCLUDE_SUBDOMAINS", "true")], MissingConfigValue "HSTS_MAX_AGE_SECONDS"),
      ("rejects a non-boolean HSTS_PRELOAD value", committedRuntimeDefaults, [], [("HSTS_MAX_AGE_SECONDS", "31536000"), ("HSTS_PRELOAD", "sometimes")], InvalidConfigValue "HSTS_PRELOAD" "sometimes"),
      ("rejects a negative HSTS_MAX_AGE_SECONDS value", committedRuntimeDefaults, [], [("HSTS_MAX_AGE_SECONDS", "-1")], InvalidConfigValue "HSTS_MAX_AGE_SECONDS" "-1"),
      ("rejects a CORS_ALLOWED_ORIGINS value with only blank entries", committedRuntimeDefaults, [], [("CORS_ALLOWED_ORIGINS", " , ")], InvalidConfigValue "CORS_ALLOWED_ORIGINS" " , "),
      ("rejects a negative CORS_MAX_AGE_SECONDS value", committedRuntimeDefaults, [], [("CORS_MAX_AGE_SECONDS", "-1")], InvalidConfigValue "CORS_MAX_AGE_SECONDS" "-1"),
      ("rejects an empty CONTENT_SECURITY_POLICY value", committedRuntimeDefaults, [], [("CONTENT_SECURITY_POLICY", "")], InvalidConfigValue "CONTENT_SECURITY_POLICY" ""),
      ("rejects a non-boolean X_CONTENT_TYPE_OPTIONS_NOSNIFF value", committedRuntimeDefaults, [], [("X_CONTENT_TYPE_OPTIONS_NOSNIFF", "maybe")], InvalidConfigValue "X_CONTENT_TYPE_OPTIONS_NOSNIFF" "maybe")
      ]
      `forM_` \(label, envPairs, listenerPairs, otherPairs, expectedError) ->
        it label $
          parseRuntimeAppConfig envPairs listenerPairs otherPairs `shouldBe` Left expectedError

  describe "defaultAppEnvironmentConfig" $ do
    it "keeps committed .env defaults free of credentials and encryption keys" $ do
      committedEnvDefaults
        `shouldBe` [ ("APP_MODE", "development"),
                     ("DATABASE_HOST", "127.0.0.1"),
                     ("DATABASE_PORT", "5432"),
                     ("DATABASE_NAME", "web_api_dev"),
                     ("DATABASE_USER", "web_api_runtime"),
                     ("DATABASE_CONNECT_TIMEOUT_SECONDS", "10"),
                     ("DATABASE_POOL_CAPACITY", "10"),
                     ("SMTP_HOST", "127.0.0.1"),
                     ("SMTP_PORT", "5025"),
                     ("SMTP_HELO_NAME", "localhost"),
                     ("SMTP_USER", "test@localhost"),
                     ("EMAIL_FROM", "noreply@localhost"),
                     ("PUBLIC_BASE_URL", "http://127.0.0.1:5001")
                   ]
      smtpDeliveryHost (smtpDeliveryConfig defaultAppEnvironmentConfig) `shouldBe` "127.0.0.1"
      smtpDeliveryPort (smtpDeliveryConfig defaultAppEnvironmentConfig) `shouldBe` 5025
      publicBaseUrl defaultAppEnvironmentConfig `shouldBe` "http://127.0.0.1:5001"
      runtimeMarker <- lookupEnv "PATH"
      let dynamicSmtpPort = if isNothing runtimeMarker then 2526 else 2527
      smtpDeliveryConfig defaultAppEnvironmentConfig
        /= (smtpDeliveryConfig defaultAppEnvironmentConfig)
          { smtpDeliveryPort = dynamicSmtpPort
          }
          `shouldBe` True
      let dynamicSmtpConfig =
            (smtpDeliveryConfig defaultAppEnvironmentConfig)
              { smtpDeliveryPort = dynamicSmtpPort
              }
          dynamicEnvironmentConfig = defaultAppEnvironmentConfig {smtpDeliveryConfig = dynamicSmtpConfig}
      show dynamicEnvironmentConfig
        `shouldBe` ( "AppEnvironmentConfig {appMode = Development, databaseConfig = DatabaseConfig {databaseHost = \"127.0.0.1\", databasePort = 5432, databaseName = \"web_api_dev\", databaseUser = \"web_api_runtime\", databasePassword = <redacted>, databaseConnectTimeoutSeconds = 10, databasePoolCapacity = 10}, smtpDeliveryConfig = SmtpDeliveryConfig {smtpDeliveryHost = \"127.0.0.1\", smtpDeliveryPort = "
                       <> show dynamicSmtpPort
                       <> ", smtpDeliveryHeloName = \"localhost\", smtpDeliverySender = \"noreply@localhost\", smtpDeliveryUsername = \"test@localhost\", smtpDeliveryPassword = <redacted>}, publicBaseUrl = \"http://127.0.0.1:5001\", totpEncryptionKey = <redacted>}"
                   )
      show dynamicEnvironmentConfig `shouldNotContain` "databasePassword = \""
      show dynamicEnvironmentConfig `shouldNotContain` "smtpDeliveryPassword = \""

    it "builds localized verification URLs and delivers through the native loopback SMTP server" $
      bracket DevSmtp.startDevSmtpServer DevSmtp.stopDevSmtpServer $ \server -> do
        let environmentConfig =
              defaultAppEnvironmentConfig
                { publicBaseUrl = "https://accounts.example.test/",
                  smtpDeliveryConfig =
                    (smtpDeliveryConfig defaultAppEnvironmentConfig)
                      { smtpDeliveryPort = fromIntegral (DevSmtp.devSmtpPort server)
                      }
                }
            baseUrlWithoutTrailingSlash = "https://accounts.example.test:" <> Text.pack (show (DevSmtp.devSmtpPort server))
            token = requiredVerificationToken (Text.replicate 43 "a")
            recipient = requiredEmailAddress "person@example.test"
        pool <- newPostgresPool (databasePoolCapacity (databaseConfig environmentConfig)) (databaseConfig environmentConfig)
        let workflow = buildRuntimeAccountWorkflow pool environmentConfig
            untrimmedWorkflow = buildRuntimeAccountWorkflow pool (environmentConfig {publicBaseUrl = baseUrlWithoutTrailingSlash})
        accountWorkflowVerificationUrl workflow (defaultRequestContext {requestLocale = Spanish}) token
          `shouldBe` "https://accounts.example.test/es/verify?token=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        Text.unpack (accountWorkflowVerificationUrl workflow defaultRequestContext token)
          `shouldBe` "https://accounts.example.test/verify?token=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        accountWorkflowVerificationUrl untrimmedWorkflow defaultRequestContext token
          `shouldBe` baseUrlWithoutTrailingSlash
          <> "/verify?token=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        accountWorkflowStore workflow `seq` pure ()
        accountWorkflowPasswordHasher workflow `seq` pure ()
        accountWorkflowPasswordWorkGate workflow `seq` pure ()
        accountWorkflowRegistrationDeliveryTimeout workflow `seq` pure ()
        accountWorkflowMfaStore workflow `seq` pure ()
        accountWorkflowCredentialStore workflow `seq` pure ()
        accountWorkflowLoginAttemptStore workflow `seq` pure ()
        accountWorkflowSessionStore workflow `seq` pure ()
        accountWorkflowMfaEnrollmentSessionStore workflow `seq` pure ()
        accountWorkflowProfileStore workflow `seq` pure ()
        accountWorkflowTotpEncryptionKey workflow `seq` pure ()
        accountWorkflowClock workflow >>= (`shouldSatisfy` (> 0))
        accountWorkflowTotpClock workflow 100000000000 `shouldSatisfy` (> 0)
        case Email.mkEmailMessage (Email.EmailMessageInput recipient "Verification test" "Hello") of
          Nothing -> expectationFailure "expected a valid SMTP test message"
          Just message -> Email.deliverEmail (accountWorkflowEmailDelivery workflow) message
        awaitDevSmtpEmail server "person@example.test"
          >>= \case
            Just received ->
              "Subject: Verification test"
                `ByteString.isInfixOf` DevSmtp.devSmtpRawMessage received
                `shouldBe` True
            Nothing -> expectationFailure "expected the loopback SMTP server to receive the message"
        let runtimeApplication = buildRuntimeApp pool defaultAppConfig environmentConfig
        HarchWeb.renderResponse
          runtimeApplication
          (HarchWeb.RouteRequest StatusApiRoute defaultRequestContext)
          >>= (`shouldSatisfy` \case HarchWeb.ProtocolResponseResult _ -> True; _ -> False)
        let productionRuntimeApplication = buildRuntimeApp pool defaultAppConfig (environmentConfig {appMode = Production})
        HarchWeb.renderResponse
          productionRuntimeApplication
          (HarchWeb.RouteRequest StatusApiRoute defaultRequestContext)
          >>= (`shouldSatisfy` \case HarchWeb.ProtocolResponseResult _ -> True; _ -> False)
        let sampleRequestObservability = Observability.buildRequestObservability Observability.RequestIdentity {Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "GET", Observability.requestIdentityScheme = "http", Observability.requestIdentityPath = "/api/status", Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/api/status"} 200 Observability.BodyResponseKind []
        runtimeRequestObservabilityReporter Production defaultAppConfig sampleRequestObservability
        runtimeRequestObservabilityReporter Development defaultAppConfig sampleRequestObservability
        HarchWeb.reportConnectionObservability
          runtimeApplication
          (Observability.buildConnectionObservability "CONNECTION runtime-account-workflow-test" [])

    it "rejects invalid SMTP runtime delivery configurations" $ do
      pool <- newPostgresPool (databasePoolCapacity (databaseConfig defaultAppEnvironmentConfig)) (databaseConfig defaultAppEnvironmentConfig)
      let recipient = requiredEmailAddress "person@example.test"
          invalidSenderWorkflow =
            buildRuntimeAccountWorkflow
              pool
              defaultAppEnvironmentConfig
                { smtpDeliveryConfig = (smtpDeliveryConfig defaultAppEnvironmentConfig) {smtpDeliverySender = "not-an-email"}
                }
          invalidHeloWorkflow =
            buildRuntimeAccountWorkflow
              pool
              defaultAppEnvironmentConfig
                { smtpDeliveryConfig = (smtpDeliveryConfig defaultAppEnvironmentConfig) {smtpDeliveryHeloName = "bad\nhelo"}
                }
      case Email.mkEmailMessage (Email.EmailMessageInput recipient "Verification test" "Hello") of
        Nothing -> expectationFailure "expected a valid SMTP test message"
        Just message ->
          forM_ [invalidSenderWorkflow, invalidHeloWorkflow] $ \invalidWorkflow ->
            (try (Email.deliverEmail (accountWorkflowEmailDelivery invalidWorkflow) message) :: IO (Either IOException ()))
              >>= \case
                Left errorValue -> displayException errorValue `shouldContain` "SMTP delivery configuration is invalid"
                Right () -> expectationFailure "invalid SMTP configuration unexpectedly delivered email"

    it "covers the new app/database config selectors and derived instances" $ do
      let productionDatabaseConfig =
            DatabaseConfig
              { databaseHost = "db.internal",
                databasePort = 6543,
                databaseName = "web_api_prod",
                databaseUser = "web_api_app",
                databasePassword = "super-secret",
                databaseConnectTimeoutSeconds = 10,
                databasePoolCapacity = requiredDatabasePoolCapacity 10
              }
          productionEnvironmentConfig =
            defaultAppEnvironmentConfig
              { appMode = Production,
                databaseConfig = productionDatabaseConfig
              }
      appMode productionEnvironmentConfig `shouldBe` Production
      databaseConfig productionEnvironmentConfig `shouldBe` productionDatabaseConfig
      databaseHost productionDatabaseConfig `shouldBe` "db.internal"
      databasePort productionDatabaseConfig `shouldBe` 6543
      databaseName productionDatabaseConfig `shouldBe` "web_api_prod"
      databaseUser productionDatabaseConfig `shouldBe` "web_api_app"
      databasePassword productionDatabaseConfig `shouldBe` "super-secret"
      Development `shouldNotBe` Test
      Test `shouldNotBe` Production
      productionDatabaseConfig `shouldBe` productionDatabaseConfig
      productionDatabaseConfig
        `shouldNotBe` productionDatabaseConfig
          { databasePassword = "different-secret"
          }
      productionEnvironmentConfig `shouldBe` productionEnvironmentConfig
      productionEnvironmentConfig
        `shouldNotBe` productionEnvironmentConfig
          { appMode = Test
          }
      MissingConfigValue "DATABASE_PASSWORD"
        `shouldNotBe` InvalidConfigValue "DATABASE_PASSWORD" "missing"
      show Development `shouldBe` "Development"
      show Test `shouldBe` "Test"
      show Production `shouldBe` "Production"
      show [Development, Test, Production] `shouldBe` "[Development,Test,Production]"
      show productionDatabaseConfig
        `shouldBe` "DatabaseConfig {databaseHost = \"db.internal\", databasePort = 6543, databaseName = \"web_api_prod\", databaseUser = \"web_api_app\", databasePassword = <redacted>, databaseConnectTimeoutSeconds = 10, databasePoolCapacity = 10}"
      show [productionDatabaseConfig]
        `shouldBe` "[DatabaseConfig {databaseHost = \"db.internal\", databasePort = 6543, databaseName = \"web_api_prod\", databaseUser = \"web_api_app\", databasePassword = <redacted>, databaseConnectTimeoutSeconds = 10, databasePoolCapacity = 10}]"
      show productionDatabaseConfig `shouldNotContain` "super-secret"
      show productionEnvironmentConfig
        `shouldContain` "smtpDeliveryConfig = SmtpDeliveryConfig {smtpDeliveryHost = \"127.0.0.1\", smtpDeliveryPort = 5025, smtpDeliveryHeloName = \"localhost\", smtpDeliverySender = \"noreply@localhost\", smtpDeliveryUsername = \"test@localhost\", smtpDeliveryPassword = <redacted>}, publicBaseUrl = \"http://127.0.0.1:5001\", totpEncryptionKey = <redacted>}"
      show [productionEnvironmentConfig]
        `shouldContain` "smtpDeliveryConfig = SmtpDeliveryConfig {smtpDeliveryHost = \"127.0.0.1\", smtpDeliveryPort = 5025, smtpDeliveryHeloName = \"localhost\", smtpDeliverySender = \"noreply@localhost\", smtpDeliveryUsername = \"test@localhost\", smtpDeliveryPassword = <redacted>}, publicBaseUrl = \"http://127.0.0.1:5001\", totpEncryptionKey = <redacted>}]"
      show productionEnvironmentConfig `shouldNotContain` "super-secret"
      show (MissingConfigValue "DATABASE_PASSWORD") `shouldBe` "MissingConfigValue \"DATABASE_PASSWORD\""
      show (InvalidConfigValue "APP_MODE" "staging") `shouldBe` "InvalidConfigValue \"APP_MODE\" \"staging\""
      show [MissingConfigValue "DATABASE_PASSWORD", InvalidConfigValue "APP_MODE" "staging"]
        `shouldBe` "[MissingConfigValue \"DATABASE_PASSWORD\",InvalidConfigValue \"APP_MODE\" \"staging\"]"

  describe "parseAppEnvironmentConfig" $ do
    it "requires every credential and encryption key outside compiled defaults" $ do
      parseAppEnvironmentConfig committedEnvDefaults [] []
        `shouldBe` Left (MissingConfigValue "DATABASE_PASSWORD")
      parseAppEnvironmentConfig committedEnvDefaults [("DATABASE_PASSWORD", "local_password")] []
        `shouldBe` Left (MissingConfigValue "SMTP_PASSWORD")
      parseAppEnvironmentConfig committedEnvDefaults [("DATABASE_PASSWORD", "local_password"), ("SMTP_PASSWORD", "local_password")] []
        `shouldBe` Left (MissingConfigValue "TOTP_ENCRYPTION_KEY")

    it "parses explicit local development credentials into the expected config" $
      parseAppEnvironmentConfig committedEnvDefaults developmentEnvironmentSecrets []
        `shouldBe` Right defaultAppEnvironmentConfig

    it "lets .env.local override committed .env defaults" $ do
      let localOverrides =
            [ ("APP_MODE", "test"),
              ("DATABASE_HOST", "localhost"),
              ("DATABASE_PORT", "6432"),
              ("DATABASE_NAME", "web_api_local"),
              ("DATABASE_USER", "local_user"),
              ("DATABASE_PASSWORD", "local_password")
            ]
      parseAppEnvironmentConfig committedEnvDefaults (developmentEnvironmentSecrets <> localOverrides) []
        `shouldBe` Right
          defaultAppEnvironmentConfig
            { appMode = Test,
              databaseConfig =
                DatabaseConfig
                  { databaseHost = "localhost",
                    databasePort = 6432,
                    databaseName = "web_api_local",
                    databaseUser = "local_user",
                    databasePassword = "local_password",
                    databaseConnectTimeoutSeconds = 10,
                    databasePoolCapacity = requiredDatabasePoolCapacity 10
                  }
            }

    it "lets environment variables override .env.local values" $ do
      let localOverrides =
            [ ("APP_MODE", "production"),
              ("DATABASE_HOST", "localhost"),
              ("DATABASE_PORT", "6432"),
              ("DATABASE_NAME", "web_api_local"),
              ("DATABASE_USER", "local_user"),
              ("DATABASE_PASSWORD", "local_password")
            ]
          environmentOverrides =
            [ ("APP_MODE", "test"),
              ("DATABASE_PORT", "7432"),
              ("DATABASE_PASSWORD", "runtime_password")
            ]
      parseAppEnvironmentConfig committedEnvDefaults (developmentEnvironmentSecrets <> localOverrides) environmentOverrides
        `shouldBe` Right
          defaultAppEnvironmentConfig
            { appMode = Test,
              databaseConfig =
                DatabaseConfig
                  { databaseHost = "localhost",
                    databasePort = 7432,
                    databaseName = "web_api_local",
                    databaseUser = "local_user",
                    databasePassword = "runtime_password",
                    databaseConnectTimeoutSeconds = 10,
                    databasePoolCapacity = requiredDatabasePoolCapacity 10
                  }
            }

    it "fails missing required values with explicit errors" $
      parseAppEnvironmentConfig
        [ ("APP_MODE", "development"),
          ("DATABASE_HOST", "127.0.0.1"),
          ("DATABASE_PORT", "5432"),
          ("DATABASE_NAME", "web_api_dev"),
          ("DATABASE_USER", "web_api_runtime")
        ]
        []
        []
        `shouldBe` Left (MissingConfigValue "DATABASE_PASSWORD")

    it "fails invalid port or mode values with precise errors" $ do
      parseAppEnvironmentConfig (committedEnvDefaults <> developmentEnvironmentSecrets) [] [("APP_MODE", "staging")]
        `shouldBe` Left (InvalidConfigValue "APP_MODE" "staging")
      parseAppEnvironmentConfig (committedEnvDefaults <> developmentEnvironmentSecrets) [] [("DATABASE_PORT", "0")]
        `shouldBe` Left (InvalidConfigValue "DATABASE_PORT" "0")
      parseAppEnvironmentConfig (committedEnvDefaults <> developmentEnvironmentSecrets) [] [("SMTP_PORT", "65536")]
        `shouldBe` Left (InvalidConfigValue "SMTP_PORT" "65536")
      parseAppEnvironmentConfig (committedEnvDefaults <> developmentEnvironmentSecrets) [] [("SMTP_PORT", "not-a-port")]
        `shouldBe` Left (InvalidConfigValue "SMTP_PORT" "not-a-port")
      parseAppEnvironmentConfig (committedEnvDefaults <> developmentEnvironmentSecrets) [] [("TOTP_ENCRYPTION_KEY", "not-a-key")]
        `shouldBe` Left (InvalidConfigValue "TOTP_ENCRYPTION_KEY" "<redacted>")
      parseAppEnvironmentConfig (committedEnvDefaults <> developmentEnvironmentSecrets) [("APP_MODE", "production")] []
        `shouldBe` Left (InvalidConfigValue "TOTP_ENCRYPTION_KEY" "development-default")
      parseAppEnvironmentConfig
        (committedEnvDefaults <> developmentEnvironmentSecrets)
        [ ("APP_MODE", "production"),
          ("TOTP_ENCRYPTION_KEY", "QkJCQkJCQkJCQkJCQkJCQkJCQkJCQkJCQkJCQkJCQkI")
        ]
        []
        `shouldBe` Right defaultAppEnvironmentConfig {appMode = Production, totpEncryptionKey = productionTotpEncryptionKey}

  describe "loadAppEnvironmentConfigWithFiles" $ do
    it "loads the documented .env then .env.local layers" $
      withSystemTempDirectory "app-environment-config" $ \tempDirectory ->
        withClearedAppEnvironment $ do
          let envPath = tempDirectory <> "/.env"
              envLocalPath = tempDirectory <> "/.env.local"
          writeFile envPath "APP_MODE=production\nDATABASE_HOST=db.shared\nDATABASE_PORT=6432\nDATABASE_NAME=shared_db\nDATABASE_USER=shared_user\nDATABASE_PASSWORD=shared_password\n"
          writeFile envLocalPath "APP_MODE=test\nDATABASE_PORT=7432\nDATABASE_PASSWORD=local_password\nSMTP_PASSWORD=password\nTOTP_ENCRYPTION_KEY=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\n"
          loadAppEnvironmentConfigWithFiles envPath envLocalPath
            `shouldReturn` Right
              defaultAppEnvironmentConfig
                { appMode = Test,
                  databaseConfig =
                    DatabaseConfig
                      { databaseHost = "db.shared",
                        databasePort = 7432,
                        databaseName = "shared_db",
                        databaseUser = "shared_user",
                        databasePassword = "local_password",
                        databaseConnectTimeoutSeconds = 10,
                        databasePoolCapacity = requiredDatabasePoolCapacity 10
                      }
                }

    it "lets process environment override .env.local values" $
      withSystemTempDirectory "app-environment-config-env" $ \tempDirectory ->
        withClearedAppEnvironment $
          withTemporaryEnvironment "APP_MODE" (Just "production") $
            withTemporaryEnvironment "DATABASE_PORT" (Just "8432") $
              withTemporaryEnvironment "DATABASE_PASSWORD" (Just "runtime_password") $ do
                let envPath = tempDirectory <> "/.env"
                    envLocalPath = tempDirectory <> "/.env.local"
                writeFile envPath "APP_MODE=development\nDATABASE_HOST=db.shared\nDATABASE_PORT=6432\nDATABASE_NAME=shared_db\nDATABASE_USER=shared_user\nDATABASE_PASSWORD=shared_password\n"
                writeFile envLocalPath "APP_MODE=test\nDATABASE_PORT=7432\nDATABASE_PASSWORD=local_password\nSMTP_PASSWORD=password\nTOTP_ENCRYPTION_KEY=QkJCQkJCQkJCQkJCQkJCQkJCQkJCQkJCQkJCQkJCQkI\n"
                loadAppEnvironmentConfigWithFiles envPath envLocalPath
                  `shouldReturn` Right
                    defaultAppEnvironmentConfig
                      { appMode = Production,
                        totpEncryptionKey = productionTotpEncryptionKey,
                        databaseConfig =
                          DatabaseConfig
                            { databaseHost = "db.shared",
                              databasePort = 8432,
                              databaseName = "shared_db",
                              databaseUser = "shared_user",
                              databasePassword = "runtime_password",
                              databaseConnectTimeoutSeconds = 10,
                              databasePoolCapacity = requiredDatabasePoolCapacity 10
                            }
                      }

    it "reports invalid override files with the failing path" $
      withSystemTempDirectory "app-environment-config-error" $ \tempDirectory ->
        withClearedAppEnvironment $ do
          let envPath = tempDirectory <> "/.env"
              envLocalPath = tempDirectory <> "/.env.local"
          writeFile envPath "DATABASE_HOST\n"
          loadAppEnvironmentConfigWithFiles envPath envLocalPath
            `shouldReturn` Left
              (AppEnvironmentOverridesFileError envPath (InvalidConfigOverridesLine 1 "DATABASE_HOST"))

    it "reports unreadable override files with the failing path" $
      withSystemTempDirectory "app-environment-config-unreadable" $ \tempDirectory ->
        withClearedAppEnvironment $ do
          let envPath = tempDirectory <> "/.env"
              envLocalPath = tempDirectory <> "/.env.local"
          writeFile envPath "APP_MODE=production\nDATABASE_HOST=db.shared\nDATABASE_PORT=6432\nDATABASE_NAME=shared_db\nDATABASE_USER=shared_user\nDATABASE_PASSWORD=shared_password\n"
          withUnreadableFile envLocalPath "APP_MODE=test\n" $ do
            result <- loadAppEnvironmentConfigWithFiles envPath envLocalPath
            result `shouldSatisfy` \case
              Left
                (AppEnvironmentOverridesFileError failingPath (UnreadableConfigOverridesFile errorMessage))
                  | failingPath == envLocalPath ->
                      not (Text.null errorMessage)
              _ -> False

    it "reports parse errors after both files load successfully" $
      withSystemTempDirectory "app-environment-config-parse-error" $ \tempDirectory ->
        withClearedAppEnvironment $ do
          let envPath = tempDirectory <> "/.env"
              envLocalPath = tempDirectory <> "/.env.local"
          writeFile envPath "DATABASE_PORT=0\n"
          loadAppEnvironmentConfigWithFiles envPath envLocalPath
            `shouldReturn` Left
              (AppEnvironmentConfigParseError (InvalidConfigValue "DATABASE_PORT" "0"))

  describe "loadAppEnvironmentConfig" $
    it "loads the default .env file names from the current directory" $
      withSystemTempDirectory "app-environment-config-current-directory" $ \tempDirectory ->
        withClearedAppEnvironment $ do
          writeFile (tempDirectory <> "/.env") "APP_MODE=production\nDATABASE_HOST=db.shared\nDATABASE_PORT=6432\nDATABASE_NAME=shared_db\nDATABASE_USER=shared_user\nDATABASE_PASSWORD=shared_password\n"
          writeFile (tempDirectory <> "/.env.local") "APP_MODE=test\nDATABASE_PASSWORD=local_password\nSMTP_PASSWORD=password\nTOTP_ENCRYPTION_KEY=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\n"
          withCurrentDirectory tempDirectory $
            loadAppEnvironmentConfig
              `shouldReturn` Right
                defaultAppEnvironmentConfig
                  { appMode = Test,
                    databaseConfig =
                      DatabaseConfig
                        { databaseHost = "db.shared",
                          databasePort = 6432,
                          databaseName = "shared_db",
                          databaseUser = "shared_user",
                          databasePassword = "local_password",
                          databaseConnectTimeoutSeconds = 10,
                          databasePoolCapacity = requiredDatabasePoolCapacity 10
                        }
                  }

  describe "AppEnvironmentConfigLoadError" $
    it "keeps load-error equality and rendering deterministic" $ do
      let fileLoadError = AppEnvironmentOverridesFileError ".env" (InvalidConfigOverridesLine 1 "BROKEN")
          parseLoadError = AppEnvironmentConfigParseError (InvalidConfigValue "DATABASE_PORT" "0")
      fileLoadError `shouldBe` fileLoadError
      fileLoadError `shouldNotBe` parseLoadError
      show fileLoadError
        `shouldBe` "AppEnvironmentOverridesFileError \".env\" (InvalidConfigOverridesLine 1 \"BROKEN\")"
      show parseLoadError
        `shouldBe` "AppEnvironmentConfigParseError (InvalidConfigValue \"DATABASE_PORT\" \"0\")"
      show [fileLoadError, parseLoadError]
        `shouldBe` "[AppEnvironmentOverridesFileError \".env\" (InvalidConfigOverridesLine 1 \"BROKEN\"),AppEnvironmentConfigParseError (InvalidConfigValue \"DATABASE_PORT\" \"0\")]"

  describe "parseAppStartupConfig" $
    it "requires explicit secrets before parsing environment and runtime defaults" $ do
      defaultAppStartupConfig
        `shouldBe` AppStartupConfig
          { startupEnvironmentConfig = defaultAppEnvironmentConfig,
            startupAppConfig = defaultAppConfig
          }
      parseAppStartupConfig (committedEnvDefaults <> committedRuntimeDefaults) [] []
        `shouldBe` Left (MissingConfigValue "DATABASE_PASSWORD")
      parseAppStartupConfig (committedEnvDefaults <> committedRuntimeDefaults) developmentEnvironmentSecrets []
        `shouldBe` Right defaultAppStartupConfig

  describe "loadAppStartupConfigWithFiles" $ do
    it "loads the documented .env then .env.local layers for runtime startup" $
      withSystemTempDirectory "app-startup-config" $ \tempDirectory ->
        withClearedAppEnvironment $
          withClearedRuntimeEnvironment $ do
            let envPath = tempDirectory <> "/.env"
                envLocalPath = tempDirectory <> "/.env.local"
            writeFile envPath "APP_MODE=production\nDATABASE_HOST=db.shared\nDATABASE_PORT=6432\nTOTP_ENCRYPTION_KEY=QkJCQkJCQkJCQkJCQkJCQkJCQkJCQkJCQkJCQkJCQkI\nAPP_TITLE_PREFIX=web-api-shared\nLISTENER_0_PORT=5443\n"
            writeFile envLocalPath "DATABASE_PASSWORD=local_password\nSMTP_PASSWORD=password\nAPP_TITLE_PREFIX=web-api-local\nLISTENER_0_PORT=7443\n"
            loadAppStartupConfigWithFiles envPath envLocalPath
              `shouldReturn` Right
                AppStartupConfig
                  { startupEnvironmentConfig =
                      defaultAppEnvironmentConfig
                        { appMode = Production,
                          totpEncryptionKey = productionTotpEncryptionKey,
                          databaseConfig =
                            DatabaseConfig
                              { databaseHost = "db.shared",
                                databasePort = 6432,
                                databaseName = "web_api_dev",
                                databaseUser = "web_api_runtime",
                                databasePassword = "local_password",
                                databaseConnectTimeoutSeconds = 10,
                                databasePoolCapacity = requiredDatabasePoolCapacity 10
                              }
                        },
                    startupAppConfig =
                      defaultAppConfig
                        { appTitlePrefix = "web-api-local",
                          listenerConfigs =
                            [ ListenerConfig
                                { listenerHost = "127.0.0.1",
                                  listenerPort = 7443,
                                  listenerScheme = Http,
                                  listenerTls = Nothing,
                                  listenerAcme = Nothing
                                }
                            ]
                        }
                  }

    it "lets process environment override .env.local values for runtime startup" $
      withSystemTempDirectory "app-startup-config-env" $ \tempDirectory ->
        withClearedAppEnvironment $
          withClearedRuntimeEnvironment $
            withTemporaryEnvironment "APP_TITLE_PREFIX" (Just "web-api-runtime") $
              withTemporaryEnvironment "LISTENER_0_HOST" (Just "0.0.0.0") $
                withTemporaryEnvironment "LISTENER_0_PORT" (Just "80") $ do
                  let envPath = tempDirectory <> "/.env"
                      envLocalPath = tempDirectory <> "/.env.local"
                  writeFile envPath "APP_MODE=production\nDATABASE_HOST=db.shared\nDATABASE_PORT=6432\nTOTP_ENCRYPTION_KEY=QkJCQkJCQkJCQkJCQkJCQkJCQkJCQkJCQkJCQkJCQkI\nAPP_TITLE_PREFIX=web-api-shared\nLISTENER_0_HOST=127.0.0.1\nLISTENER_0_PORT=5443\n"
                  writeFile envLocalPath "DATABASE_PASSWORD=local_password\nSMTP_PASSWORD=password\nAPP_TITLE_PREFIX=web-api-local\nLISTENER_0_PORT=7443\n"
                  loadAppStartupConfigWithFiles envPath envLocalPath
                    `shouldReturn` Right
                      AppStartupConfig
                        { startupEnvironmentConfig =
                            defaultAppEnvironmentConfig
                              { appMode = Production,
                                totpEncryptionKey = productionTotpEncryptionKey,
                                databaseConfig =
                                  DatabaseConfig
                                    { databaseHost = "db.shared",
                                      databasePort = 6432,
                                      databaseName = "web_api_dev",
                                      databaseUser = "web_api_runtime",
                                      databasePassword = "local_password",
                                      databaseConnectTimeoutSeconds = 10,
                                      databasePoolCapacity = requiredDatabasePoolCapacity 10
                                    }
                              },
                          startupAppConfig =
                            defaultAppConfig
                              { appTitlePrefix = "web-api-runtime",
                                listenerConfigs =
                                  [ ListenerConfig
                                      { listenerHost = "0.0.0.0",
                                        listenerPort = 80,
                                        listenerScheme = Http,
                                        listenerTls = Nothing,
                                        listenerAcme = Nothing
                                      }
                                  ]
                              }
                        }

    it "reports invalid override files or parse failures with explicit errors" $
      withSystemTempDirectory "app-startup-config-errors" $ \tempDirectory ->
        withClearedAppEnvironment $
          withClearedRuntimeEnvironment $ do
            let brokenEnvPath = tempDirectory <> "/broken.env"
                envLocalPath = tempDirectory <> "/.env.local"
                invalidEnvPath = tempDirectory <> "/invalid.env"
            writeFile brokenEnvPath "APP_TITLE_PREFIX\n"
            loadAppStartupConfigWithFiles brokenEnvPath envLocalPath
              `shouldReturn` Left
                (AppStartupOverridesFileError brokenEnvPath (InvalidConfigOverridesLine 1 "APP_TITLE_PREFIX"))
            writeFile envLocalPath "DATABASE_PASSWORD=local_password\nSMTP_PASSWORD=local_password\nTOTP_ENCRYPTION_KEY=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\n"
            writeFile invalidEnvPath "LISTENER_0_PORT=0\n"
            loadAppStartupConfigWithFiles invalidEnvPath envLocalPath
              `shouldReturn` Left
                (AppStartupConfigParseError (InvalidConfigValue "LISTENER_0_PORT" "0"))

    it "reports unreadable override files with the failing path" $
      withSystemTempDirectory "app-startup-config-unreadable" $ \tempDirectory ->
        withClearedAppEnvironment $
          withClearedRuntimeEnvironment $ do
            let envPath = tempDirectory <> "/.env"
                envLocalPath = tempDirectory <> "/.env.local"
            writeFile envPath "APP_MODE=production\nDATABASE_HOST=db.shared\nDATABASE_PORT=6432\nAPP_TITLE_PREFIX=web-api-shared\nLISTENER_0_PORT=5443\n"
            withUnreadableFile envLocalPath "DATABASE_PASSWORD=local_password\nAPP_TITLE_PREFIX=web-api-local\n" $ do
              result <- loadAppStartupConfigWithFiles envPath envLocalPath
              result `shouldSatisfy` \case
                Left
                  (AppStartupOverridesFileError failingPath (UnreadableConfigOverridesFile errorMessage))
                    | failingPath == envLocalPath ->
                        not (Text.null errorMessage)
                _ -> False

  describe "loadAppStartupConfig" $
    it "loads the default .env file names for runtime startup from the current directory" $
      withSystemTempDirectory "app-startup-config-current-directory" $ \tempDirectory ->
        withClearedAppEnvironment $
          withClearedRuntimeEnvironment $ do
            writeFile (tempDirectory <> "/.env") "APP_MODE=production\nAPP_TITLE_PREFIX=web-api-shared\n"
            writeFile (tempDirectory <> "/.env.local") "APP_MODE=test\nDATABASE_PASSWORD=web_api\nSMTP_PASSWORD=password\nTOTP_ENCRYPTION_KEY=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\nLISTENER_0_PORT=6001\n"
            withCurrentDirectory tempDirectory $
              loadAppStartupConfig
                `shouldReturn` Right
                  defaultAppStartupConfig
                    { startupEnvironmentConfig =
                        defaultAppEnvironmentConfig
                          { appMode = Test
                          },
                      startupAppConfig =
                        defaultAppConfig
                          { appTitlePrefix = "web-api-shared",
                            listenerConfigs =
                              [ ListenerConfig
                                  { listenerHost = "127.0.0.1",
                                    listenerPort = 6001,
                                    listenerScheme = Http,
                                    listenerTls = Nothing,
                                    listenerAcme = Nothing
                                  }
                              ]
                          }
                    }

  describe "AppStartupConfig and AppStartupConfigLoadError" $
    it "keep equality and rendering deterministic" $ do
      let startupConfig =
            AppStartupConfig
              { startupEnvironmentConfig = defaultAppEnvironmentConfig {appMode = Test},
                startupAppConfig = defaultAppConfig {appTitlePrefix = "web-api-test"}
              }
          differentStartupConfig =
            AppStartupConfig
              { startupEnvironmentConfig = defaultAppEnvironmentConfig,
                startupAppConfig = defaultAppConfig
              }
          fileLoadError = AppStartupOverridesFileError ".env" (InvalidConfigOverridesLine 1 "BROKEN")
          parseLoadError = AppStartupConfigParseError (InvalidConfigValue "LISTENER_0_PORT" "0")
      startupConfig `shouldBe` startupConfig
      startupConfig `shouldNotBe` differentStartupConfig
      show startupConfig
        `shouldContain` ("staticAssetContentTypes = " <> show defaultStaticAssetContentTypes)
      show [startupConfig]
        `shouldContain` ("staticAssetContentTypes = " <> show defaultStaticAssetContentTypes)
      fileLoadError `shouldBe` fileLoadError
      fileLoadError `shouldNotBe` parseLoadError
      show fileLoadError
        `shouldBe` "AppStartupOverridesFileError \".env\" (InvalidConfigOverridesLine 1 \"BROKEN\")"
      show parseLoadError
        `shouldBe` "AppStartupConfigParseError (InvalidConfigValue \"LISTENER_0_PORT\" \"0\")"
      show [fileLoadError, parseLoadError]
        `shouldBe` "[AppStartupOverridesFileError \".env\" (InvalidConfigOverridesLine 1 \"BROKEN\"),AppStartupConfigParseError (InvalidConfigValue \"LISTENER_0_PORT\" \"0\")]"
