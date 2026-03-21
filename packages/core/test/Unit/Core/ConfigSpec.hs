{-# SPEC #-}

import qualified Core.Config as CoreConfig
import qualified Data.Text as Text
import HarchWeb
import System.Directory (removeFile)
import System.IO (hClose, hPutStr)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)

spec = do
  describe "defaultAppConfig" $ do
    it "keeps the committed runtime defaults aligned with the shared HarchWeb config" $
      CoreConfig.defaultAppConfig
        `shouldBe` AppConfig
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

  describe "parseConfigOverridesFile" $ do
    it "parses key value lines while ignoring blank lines and comments" $
      CoreConfig.parseConfigOverridesFile
        ( Text.unlines
            [ Text.pack "# override file",
              Text.pack " APP_TITLE_PREFIX = custom-app ",
              Text.empty,
              Text.pack "LISTENER_0_PORT=6001"
            ]
        )
        `shouldBe` Right
          [ (Text.pack "APP_TITLE_PREFIX", Text.pack "custom-app"),
            (Text.pack "LISTENER_0_PORT", Text.pack "6001")
          ]

    it "rejects malformed override lines with the original line content" $ do
      CoreConfig.parseConfigOverridesFile
        ( Text.unlines
            [ Text.pack "APP_TITLE_PREFIX=custom-app",
              Text.pack "BROKEN_LINE"
            ]
        )
        `shouldBe` Left (CoreConfig.InvalidConfigOverridesLine 2 (Text.pack "BROKEN_LINE"))
      CoreConfig.parseConfigOverridesFile (Text.pack "   =value")
        `shouldBe` Left (CoreConfig.InvalidConfigOverridesLine 1 (Text.pack "   =value"))

  describe "loadConfigOverridesFile" $ do
    it "returns no overrides when the file does not exist" $
      withSystemTempDirectory "core-config" $ \tempDirectory -> do
        CoreConfig.loadConfigOverridesFile (tempDirectory <> "/missing.overrides")
          `shouldReturn` Right []

    it "loads override entries from disk" $
      withSystemTempFile "runtime.overrides" $ \overridesPath overridesHandle -> do
        hPutStr overridesHandle "APP_TITLE_PREFIX=loaded-from-file\nLISTENER_0_PORT=5100\n"
        hClose overridesHandle
        CoreConfig.loadConfigOverridesFile overridesPath
          `shouldReturn` Right
            [ (Text.pack "APP_TITLE_PREFIX", Text.pack "loaded-from-file"),
              (Text.pack "LISTENER_0_PORT", Text.pack "5100")
            ]

  describe "loadRuntimeAppConfig" $ do
    it "merges committed defaults, file overrides, and environment overrides into the shared HarchWeb config" $
      withSystemTempFile "runtime.overrides" $ \overridesPath overridesHandle -> do
        hPutStr overridesHandle "APP_TITLE_PREFIX=file-title\nLISTENER_0_PORT=5100\n"
        hClose overridesHandle
        CoreConfig.loadRuntimeAppConfig overridesPath [(Text.pack "LISTENER_0_PORT", Text.pack "6100")]
          `shouldReturn` Right
            AppConfig
              { appTitlePrefix = Text.pack "file-title",
                listenerConfigs =
                  [ ListenerConfig
                      { listenerHost = Text.pack "127.0.0.1",
                        listenerPort = 6100,
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

    it "surfaces override file parsing and runtime parsing failures" $ do
      withSystemTempFile "runtime.overrides" $ \overridesPath overridesHandle -> do
        hPutStr overridesHandle "BROKEN_LINE\n"
        hClose overridesHandle
        CoreConfig.loadRuntimeAppConfig overridesPath []
          `shouldReturn` Left
            (CoreConfig.InvalidConfigOverridesFile (CoreConfig.InvalidConfigOverridesLine 1 (Text.pack "BROKEN_LINE")))
      withSystemTempFile "runtime.overrides" $ \overridesPath overridesHandle -> do
        hPutStr overridesHandle "APP_TITLE_PREFIX=file-title\nLISTENER_0_PORT=0\n"
        hClose overridesHandle
        CoreConfig.loadRuntimeAppConfig overridesPath []
          `shouldReturn` Left
            (CoreConfig.InvalidRuntimeConfig (CoreConfig.InvalidConfigValue (Text.pack "LISTENER_0_PORT") (Text.pack "0")))

    it "keeps committed defaults aligned with the loaded runtime config when no overrides are present" $
      withSystemTempFile "runtime.overrides" $ \overridesPath overridesHandle -> do
        hClose overridesHandle
        removeFile overridesPath
        CoreConfig.loadRuntimeAppConfig overridesPath []
          `shouldReturn` Right CoreConfig.defaultAppConfig

  describe "parseRuntimeAppConfig" $ do
    it "parses committed runtime defaults into the expected app config" $
      CoreConfig.parseRuntimeAppConfig CoreConfig.committedRuntimeDefaults [] []
        `shouldBe` Right CoreConfig.defaultAppConfig

    it "fails when no listeners are configured" $
      CoreConfig.parseRuntimeAppConfig
        [(Text.pack "APP_TITLE_PREFIX", Text.pack "runtime-test")]
        []
        []
        `shouldBe` Left (CoreConfig.MissingConfigValue (Text.pack "LISTENER_0_HOST"))

    it "parses multiple listeners in deterministic index order" $ do
      let committedDefaults =
            [ (Text.pack "APP_TITLE_PREFIX", Text.pack "runtime-test"),
              (Text.pack "LISTENER_2_SCHEME", Text.pack "http"),
              (Text.pack "LISTENER_1_PORT", Text.pack "5002"),
              (Text.pack "LISTENER_2_PORT", Text.pack "5003"),
              (Text.pack "LISTENER_1_HOST", Text.pack "127.0.0.2"),
              (Text.pack "LISTENER_2_HOST", Text.pack "127.0.0.3"),
              (Text.pack "LISTENER_1_SCHEME", Text.pack "http")
            ]
      CoreConfig.parseRuntimeAppConfig committedDefaults [] []
        `shouldBe` Right
          AppConfig
            { appTitlePrefix = Text.pack "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = Text.pack "127.0.0.2",
                      listenerPort = 5002,
                      listenerScheme = Http,
                      listenerTls = Nothing
                    },
                  ListenerConfig
                    { listenerHost = Text.pack "127.0.0.3",
                      listenerPort = 5003,
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

    it "requires HTTPS listeners to specify a TLS source" $
      CoreConfig.parseRuntimeAppConfig
        [ (Text.pack "APP_TITLE_PREFIX", Text.pack "runtime-test"),
          (Text.pack "LISTENER_0_HOST", Text.pack "0.0.0.0"),
          (Text.pack "LISTENER_0_PORT", Text.pack "5443"),
          (Text.pack "LISTENER_0_SCHEME", Text.pack "https")
        ]
        []
        []
        `shouldBe` Left (CoreConfig.MissingConfigValue (Text.pack "LISTENER_0_TLS_SOURCE"))

    it "parses manual and ACME-backed HTTPS listeners distinctly" $ do
      let committedDefaults =
            [ (Text.pack "APP_TITLE_PREFIX", Text.pack "runtime-test"),
              (Text.pack "LISTENER_BAD_HOST", Text.pack "ignored-host"),
              (Text.pack "LISTENER_0_HOST", Text.pack "0.0.0.0"),
              (Text.pack "LISTENER_0_PORT", Text.pack "5443"),
              (Text.pack "LISTENER_0_SCHEME", Text.pack "https"),
              (Text.pack "LISTENER_0_TLS_SOURCE", Text.pack "manual"),
              (Text.pack "LISTENER_0_TLS_CERTIFICATE_FILE", Text.pack "cert.pem"),
              (Text.pack "LISTENER_0_TLS_PRIVATE_KEY_FILE", Text.pack "key.pem"),
              (Text.pack "LISTENER_1_HOST", Text.pack "0.0.0.0"),
              (Text.pack "LISTENER_1_PORT", Text.pack "5444"),
              (Text.pack "LISTENER_1_SCHEME", Text.pack "https"),
              (Text.pack "LISTENER_1_TLS_SOURCE", Text.pack "acme"),
              (Text.pack "LISTENER_1_ACME_DIRECTORY_URL", Text.pack "https://acme-staging-v02.api.letsencrypt.org/directory"),
              (Text.pack "LISTENER_1_ACME_CONTACT_EMAILS", Text.pack "ops@example.com,alerts@example.com"),
              (Text.pack "LISTENER_1_ACME_CHALLENGE_BACKEND", Text.pack "in-process-http01"),
              (Text.pack "LISTENER_2_HOST", Text.pack "0.0.0.0"),
              (Text.pack "LISTENER_2_PORT", Text.pack "5445"),
              (Text.pack "LISTENER_2_SCHEME", Text.pack "https"),
              (Text.pack "LISTENER_2_TLS_SOURCE", Text.pack "acme"),
              (Text.pack "LISTENER_2_ACME_DIRECTORY_URL", Text.pack "https://acme-v02.api.letsencrypt.org/directory"),
              (Text.pack "LISTENER_2_ACME_CONTACT_EMAILS", Text.pack "ops@example.com"),
              (Text.pack "LISTENER_2_ACME_CHALLENGE_BACKEND", Text.pack "certbot-http01"),
              (Text.pack "LISTENER_2_ACME_CERTBOT_EXECUTABLE", Text.pack "certbot"),
              (Text.pack "LISTENER_2_ACME_CERTBOT_ARGUMENTS", Text.pack "certonly,--webroot,--agree-tos")
            ]
      CoreConfig.parseRuntimeAppConfig committedDefaults [] []
        `shouldBe` Right
          AppConfig
            { appTitlePrefix = Text.pack "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = Text.pack "0.0.0.0",
                      listenerPort = 5443,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                ManualCertificateFiles
                                  { certificateFile = "cert.pem",
                                    privateKeyFile = "key.pem"
                                  }
                            }
                    },
                  ListenerConfig
                    { listenerHost = Text.pack "0.0.0.0",
                      listenerPort = 5444,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                AcmeCertificateSource
                                  AcmeConfig
                                    { acmeDirectoryUrl = Text.pack "https://acme-staging-v02.api.letsencrypt.org/directory",
                                      acmeContactEmails = [Text.pack "ops@example.com", Text.pack "alerts@example.com"],
                                      acmeChallengeBackend = InProcessHttp01
                                    }
                            }
                    },
                  ListenerConfig
                    { listenerHost = Text.pack "0.0.0.0",
                      listenerPort = 5445,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                AcmeCertificateSource
                                  AcmeConfig
                                    { acmeDirectoryUrl = Text.pack "https://acme-v02.api.letsencrypt.org/directory",
                                      acmeContactEmails = [Text.pack "ops@example.com"],
                                      acmeChallengeBackend =
                                        CertbotHttp01
                                          CertbotConfig
                                            { certbotExecutable = "certbot",
                                              certbotArguments = [Text.pack "certonly", Text.pack "--webroot", Text.pack "--agree-tos"]
                                            }
                                    }
                            }
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

    it "rejects invalid listener scheme and TLS source values" $ do
      CoreConfig.parseRuntimeAppConfig
        [ (Text.pack "APP_TITLE_PREFIX", Text.pack "runtime-test"),
          (Text.pack "LISTENER_0_HOST", Text.pack "0.0.0.0"),
          (Text.pack "LISTENER_0_PORT", Text.pack "5443"),
          (Text.pack "LISTENER_0_SCHEME", Text.pack "tcp")
        ]
        []
        []
        `shouldBe` Left (CoreConfig.InvalidConfigValue (Text.pack "LISTENER_0_SCHEME") (Text.pack "tcp"))
      CoreConfig.parseRuntimeAppConfig
        [ (Text.pack "APP_TITLE_PREFIX", Text.pack "runtime-test"),
          (Text.pack "LISTENER_0_HOST", Text.pack "0.0.0.0"),
          (Text.pack "LISTENER_0_PORT", Text.pack "5443"),
          (Text.pack "LISTENER_0_SCHEME", Text.pack "https"),
          (Text.pack "LISTENER_0_TLS_SOURCE", Text.pack "vault")
        ]
        []
        []
        `shouldBe` Left (CoreConfig.InvalidConfigValue (Text.pack "LISTENER_0_TLS_SOURCE") (Text.pack "vault"))

    it "parses static asset roots and cache policy into the expected config" $ do
      let committedDefaults =
            [ (Text.pack "APP_TITLE_PREFIX", Text.pack "runtime-test"),
              (Text.pack "LISTENER_0_HOST", Text.pack "127.0.0.1"),
              (Text.pack "LISTENER_0_PORT", Text.pack "5001"),
              (Text.pack "LISTENER_0_SCHEME", Text.pack "http"),
              (Text.pack "STATIC_ASSET_ROOT_2_DIRECTORY", Text.pack "vendor/public"),
              (Text.pack "STATIC_ASSET_ROOT_1_URL_PREFIX", Text.pack "/assets"),
              (Text.pack "STATIC_ASSET_ROOT_2_URL_PREFIX", Text.pack "/vendor"),
              (Text.pack "STATIC_ASSET_ROOT_1_DIRECTORY", Text.pack "public"),
              (Text.pack "STATIC_CACHE_CONTROL_SECONDS", Text.pack "3600")
            ]
      CoreConfig.parseRuntimeAppConfig committedDefaults [] []
        `shouldBe` Right
          AppConfig
            { appTitlePrefix = Text.pack "runtime-test",
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
                  { staticAssetRoots =
                      [ StaticAssetRoot
                          { staticUrlPrefix = Text.pack "/assets",
                            staticDirectory = "public"
                          },
                        StaticAssetRoot
                          { staticUrlPrefix = Text.pack "/vendor",
                            staticDirectory = "vendor/public"
                          }
                      ],
                    staticCacheControlSeconds = Just 3600
                  },
              observability =
                ObservabilityConfig
                  { tracingExporter = Nothing,
                    metricsExporter = Nothing
                  }
            }

    it "parses tracing and metrics exporters independently while preserving header order" $ do
      CoreConfig.parseRuntimeAppConfig
        CoreConfig.committedRuntimeDefaults
        []
        [ (Text.pack "OTLP_TRACING_ENDPOINT", Text.pack "http://collector:4318/v1/traces"),
          (Text.pack "OTLP_TRACING_HEADERS", Text.pack "authorization=Bearer token;x-api-key=secret")
        ]
        `shouldBe` Right
          CoreConfig.defaultAppConfig
            { observability =
                ObservabilityConfig
                  { tracingExporter =
                      Just
                        OtlpExporter
                          { otlpEndpoint = Text.pack "http://collector:4318/v1/traces",
                            otlpHeaders =
                              [ (Text.pack "authorization", Text.pack "Bearer token"),
                                (Text.pack "x-api-key", Text.pack "secret")
                              ]
                          },
                    metricsExporter = Nothing
                  }
            }
      CoreConfig.parseRuntimeAppConfig
        CoreConfig.committedRuntimeDefaults
        []
        [(Text.pack "OTLP_TRACING_ENDPOINT", Text.pack "http://collector:4318/v1/traces")]
        `shouldBe` Right
          CoreConfig.defaultAppConfig
            { observability =
                ObservabilityConfig
                  { tracingExporter =
                      Just
                        OtlpExporter
                          { otlpEndpoint = Text.pack "http://collector:4318/v1/traces",
                            otlpHeaders = []
                          },
                    metricsExporter = Nothing
                  }
            }
      CoreConfig.parseRuntimeAppConfig
        CoreConfig.committedRuntimeDefaults
        []
        [ (Text.pack "OTLP_METRICS_ENDPOINT", Text.pack "http://collector:4318/v1/metrics"),
          (Text.pack "OTLP_METRICS_HEADERS", Text.pack "x-scope=metrics;broken-entry")
        ]
        `shouldBe` Right
          CoreConfig.defaultAppConfig
            { observability =
                ObservabilityConfig
                  { tracingExporter = Nothing,
                    metricsExporter =
                      Just
                        OtlpExporter
                          { otlpEndpoint = Text.pack "http://collector:4318/v1/metrics",
                            otlpHeaders = [(Text.pack "x-scope", Text.pack "metrics")]
                          }
                  }
            }

    it "fails invalid runtime values with explicit errors" $ do
      CoreConfig.parseRuntimeAppConfig
        [ (Text.pack "APP_TITLE_PREFIX", Text.pack "runtime-test"),
          (Text.pack "LISTENER_0_HOST", Text.pack "127.0.0.1"),
          (Text.pack "LISTENER_0_PORT", Text.pack "0"),
          (Text.pack "LISTENER_0_SCHEME", Text.pack "http")
        ]
        []
        []
        `shouldBe` Left (CoreConfig.InvalidConfigValue (Text.pack "LISTENER_0_PORT") (Text.pack "0"))
      CoreConfig.parseRuntimeAppConfig
        [ (Text.pack "APP_TITLE_PREFIX", Text.pack "runtime-test"),
          (Text.pack "LISTENER_0_HOST", Text.pack "127.0.0.1"),
          (Text.pack "LISTENER_0_PORT", Text.pack "5001"),
          (Text.pack "LISTENER_0_SCHEME", Text.pack "https"),
          (Text.pack "LISTENER_0_TLS_SOURCE", Text.pack "acme"),
          (Text.pack "LISTENER_0_ACME_DIRECTORY_URL", Text.pack "https://acme-v02.api.letsencrypt.org/directory"),
          (Text.pack "LISTENER_0_ACME_CONTACT_EMAILS", Text.pack ""),
          (Text.pack "LISTENER_0_ACME_CHALLENGE_BACKEND", Text.pack "shell-script")
        ]
        []
        []
        `shouldBe` Left (CoreConfig.InvalidConfigValue (Text.pack "LISTENER_0_ACME_CONTACT_EMAILS") (Text.pack ""))
      CoreConfig.parseRuntimeAppConfig
        [ (Text.pack "APP_TITLE_PREFIX", Text.pack "runtime-test"),
          (Text.pack "LISTENER_0_HOST", Text.pack "127.0.0.1"),
          (Text.pack "LISTENER_0_PORT", Text.pack "5001"),
          (Text.pack "LISTENER_0_SCHEME", Text.pack "https"),
          (Text.pack "LISTENER_0_TLS_SOURCE", Text.pack "acme"),
          (Text.pack "LISTENER_0_ACME_DIRECTORY_URL", Text.pack "https://acme-v02.api.letsencrypt.org/directory"),
          (Text.pack "LISTENER_0_ACME_CONTACT_EMAILS", Text.pack "ops@example.com"),
          (Text.pack "LISTENER_0_ACME_CHALLENGE_BACKEND", Text.pack "shell-script")
        ]
        []
        []
        `shouldBe` Left (CoreConfig.InvalidConfigValue (Text.pack "LISTENER_0_ACME_CHALLENGE_BACKEND") (Text.pack "shell-script"))
      CoreConfig.parseRuntimeAppConfig
        [ (Text.pack "APP_TITLE_PREFIX", Text.pack "runtime-test"),
          (Text.pack "LISTENER_0_HOST", Text.pack "127.0.0.1"),
          (Text.pack "LISTENER_0_PORT", Text.pack "5001"),
          (Text.pack "LISTENER_0_SCHEME", Text.pack "https"),
          (Text.pack "LISTENER_0_TLS_SOURCE", Text.pack "acme"),
          (Text.pack "LISTENER_0_ACME_DIRECTORY_URL", Text.pack "https://acme-v02.api.letsencrypt.org/directory"),
          (Text.pack "LISTENER_0_ACME_CONTACT_EMAILS", Text.pack "ops@example.com"),
          (Text.pack "LISTENER_0_ACME_CHALLENGE_BACKEND", Text.pack "certbot-http01"),
          (Text.pack "LISTENER_0_ACME_CERTBOT_EXECUTABLE", Text.pack "certbot")
        ]
        []
        []
        `shouldBe` Right
          AppConfig
            { appTitlePrefix = Text.pack "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = Text.pack "127.0.0.1",
                      listenerPort = 5001,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                AcmeCertificateSource
                                  AcmeConfig
                                    { acmeDirectoryUrl = Text.pack "https://acme-v02.api.letsencrypt.org/directory",
                                      acmeContactEmails = [Text.pack "ops@example.com"],
                                      acmeChallengeBackend =
                                        CertbotHttp01
                                          CertbotConfig
                                            { certbotExecutable = "certbot",
                                              certbotArguments = []
                                            }
                                    }
                            }
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
      CoreConfig.parseRuntimeAppConfig
        [ (Text.pack "APP_TITLE_PREFIX", Text.pack "runtime-test"),
          (Text.pack "LISTENER_0_HOST", Text.pack "127.0.0.1"),
          (Text.pack "LISTENER_0_PORT", Text.pack "5001"),
          (Text.pack "LISTENER_0_SCHEME", Text.pack "http"),
          (Text.pack "STATIC_CACHE_CONTROL_SECONDS", Text.pack "-1")
        ]
        []
        []
        `shouldBe` Left (CoreConfig.InvalidConfigValue (Text.pack "STATIC_CACHE_CONTROL_SECONDS") (Text.pack "-1"))
      CoreConfig.parseRuntimeAppConfig
        CoreConfig.committedRuntimeDefaults
        []
        [(Text.pack "OTLP_TRACING_HEADERS", Text.pack "authorization=Bearer token")]
        `shouldBe` Left (CoreConfig.MissingConfigValue (Text.pack "OTLP_TRACING_ENDPOINT"))

  describe "config error instances" $ do
    it "preserves derived Eq and Show coverage for the core-owned error types" $ do
      let missingConfigValue = CoreConfig.MissingConfigValue (Text.pack "APP_TITLE_PREFIX")
          otherMissingConfigValue = CoreConfig.MissingConfigValue (Text.pack "LISTENER_0_HOST")
          invalidConfigValue = CoreConfig.InvalidConfigValue (Text.pack "LISTENER_0_PORT") (Text.pack "0")
          otherInvalidConfigValue = CoreConfig.InvalidConfigValue (Text.pack "LISTENER_0_PORT") (Text.pack "1")
          invalidOverridesLine = CoreConfig.InvalidConfigOverridesLine 3 (Text.pack "BROKEN_LINE")
          otherInvalidOverridesLine = CoreConfig.InvalidConfigOverridesLine 4 (Text.pack "OTHER_LINE")
          invalidOverridesFile = CoreConfig.InvalidConfigOverridesFile invalidOverridesLine
          otherInvalidOverridesFile = CoreConfig.InvalidConfigOverridesFile otherInvalidOverridesLine
          invalidRuntimeConfig = CoreConfig.InvalidRuntimeConfig invalidConfigValue
          otherInvalidRuntimeConfig = CoreConfig.InvalidRuntimeConfig otherInvalidConfigValue
      missingConfigValue `shouldBe` missingConfigValue
      missingConfigValue `shouldNotBe` otherMissingConfigValue
      invalidConfigValue `shouldBe` invalidConfigValue
      invalidConfigValue `shouldNotBe` otherInvalidConfigValue
      invalidOverridesLine `shouldBe` invalidOverridesLine
      invalidOverridesLine `shouldNotBe` otherInvalidOverridesLine
      invalidOverridesFile `shouldBe` invalidOverridesFile
      invalidOverridesFile `shouldNotBe` otherInvalidOverridesFile
      invalidRuntimeConfig `shouldBe` invalidRuntimeConfig
      invalidRuntimeConfig `shouldNotBe` otherInvalidRuntimeConfig
      show missingConfigValue `shouldBe` "MissingConfigValue \"APP_TITLE_PREFIX\""
      show invalidConfigValue `shouldBe` "InvalidConfigValue \"LISTENER_0_PORT\" \"0\""
      show invalidOverridesLine `shouldBe` "InvalidConfigOverridesLine 3 \"BROKEN_LINE\""
      show invalidOverridesFile `shouldBe` "InvalidConfigOverridesFile (InvalidConfigOverridesLine 3 \"BROKEN_LINE\")"
      show invalidRuntimeConfig `shouldBe` "InvalidRuntimeConfig (InvalidConfigValue \"LISTENER_0_PORT\" \"0\")"
      show [missingConfigValue, otherMissingConfigValue] `shouldBe` "[MissingConfigValue \"APP_TITLE_PREFIX\",MissingConfigValue \"LISTENER_0_HOST\"]"
      show [invalidOverridesLine] `shouldBe` "[InvalidConfigOverridesLine 3 \"BROKEN_LINE\"]"
      show [invalidOverridesFile] `shouldBe` "[InvalidConfigOverridesFile (InvalidConfigOverridesLine 3 \"BROKEN_LINE\")]"
      show [invalidRuntimeConfig] `shouldBe` "[InvalidRuntimeConfig (InvalidConfigValue \"LISTENER_0_PORT\" \"0\")]"
