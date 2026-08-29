{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Data.Text qualified as Text
import System.IO.Temp (withSystemTempDirectory)
import Unit.WebApi.TestSupport hiding (databaseConfig)
import WebApi.Config (AppConfig (..), AppEnvironmentConfig (..), AppMode (..), DatabaseConfig (..), DatabaseTransportSecurity (..), ObservabilityConfig (..), OtlpExporter (..), committedEnvDefaults, committedRuntimeDefaults, defaultAppConfig, defaultAppEnvironmentConfig, defaultStaticAssetContentTypes)
import WebApi.SetupConfig (AppSetupConfig (..), AppSetupConfigLoadError (..), SetupAutostartConfig (..), committedSetupDefaults, defaultAppSetupConfig, defaultSetupAutostartConfig, loadAppSetupConfig, loadAppSetupConfigWithFiles, parseAppSetupConfig)

testRuntimeSecrets :: [(Text.Text, Text.Text)]
testRuntimeSecrets =
  [ ("DATABASE_PASSWORD", "web_api"),
    ("SMTP_PASSWORD", "password"),
    ("TOTP_ENCRYPTION_KEY", "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
  ]

testSetupDefaults :: [(Text.Text, Text.Text)]
testSetupDefaults = committedEnvDefaults <> committedRuntimeDefaults <> committedSetupDefaults <> testRuntimeSecrets

spec = do
  describe "parseAppSetupConfig" $ do
    it "parses committed runtime and setup defaults into the expected setup config" $ do
      committedSetupDefaults
        `shouldBe` [ ("SETUP_AUTOSTART_DATABASE", "true"),
                     ("SETUP_AUTOSTART_JAEGER", "false")
                   ]
      defaultSetupAutostartConfig
        `shouldBe` SetupAutostartConfig
          { setupAutostartDatabase = True,
            setupAutostartJaeger = False
          }
      defaultAppSetupConfig
        `shouldBe` AppSetupConfig
          { setupEnvironmentConfig = defaultAppEnvironmentConfig,
            setupAppConfig = defaultAppConfig,
            setupMigrationDatabaseConfig = Nothing,
            setupAutostartConfig = defaultSetupAutostartConfig
          }
      parseAppSetupConfig testSetupDefaults [] []
        `shouldBe` Right defaultAppSetupConfig
      parseAppSetupConfig testSetupDefaults [] []
        `shouldBe` Right defaultAppSetupConfig
      -- Setup defaults stay independently optional: runtime credentials must be
      -- explicit, but setup-autostart values retain their documented fallback.
      parseAppSetupConfig (committedEnvDefaults <> committedRuntimeDefaults <> testRuntimeSecrets) [] []
        `shouldBe` Right defaultAppSetupConfig
      parseAppSetupConfig (committedEnvDefaults <> committedRuntimeDefaults) [] []
        `shouldBe` Left (MissingConfigValue "DATABASE_PASSWORD")

    it "lets setup booleans follow the same layered precedence as runtime config" $
      parseAppSetupConfig
        testSetupDefaults
        [ ("APP_TITLE_PREFIX", "setup-local"),
          ("SETUP_AUTOSTART_DATABASE", "yes")
        ]
        [("SETUP_AUTOSTART_JAEGER", "1")]
        `shouldBe` Right
          AppSetupConfig
            { setupEnvironmentConfig = defaultAppEnvironmentConfig,
              setupAppConfig =
                defaultAppConfig
                  { appTitlePrefix = "setup-local"
                  },
              setupMigrationDatabaseConfig = Nothing,
              setupAutostartConfig =
                SetupAutostartConfig
                  { setupAutostartDatabase = True,
                    setupAutostartJaeger = True
                  }
            }

    it "lets OTLP_TRACING_ENABLED use the default local endpoint while still flowing into setup config" $
      parseAppSetupConfig
        testSetupDefaults
        []
        [ ("OTLP_TRACING_ENABLED", "true"),
          ("OTLP_TRACING_HEADERS", "authorization=Bearer token")
        ]
        `shouldBe` Right
          AppSetupConfig
            { setupEnvironmentConfig = defaultAppEnvironmentConfig,
              setupAppConfig =
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
                  },
              setupMigrationDatabaseConfig = Nothing,
              setupAutostartConfig = defaultSetupAutostartConfig
            }

    it "parses optional migration-owner credentials separately from the runtime database config" $
      parseAppSetupConfig
        testSetupDefaults
        [ ("DATABASE_USER", "web_api_runtime"),
          ("WEB_API_MIGRATION_DATABASE_HOST", "127.0.0.1"),
          ("WEB_API_MIGRATION_DATABASE_PORT", "5432"),
          ("WEB_API_MIGRATION_DATABASE_NAME", "web_api_dev"),
          ("WEB_API_MIGRATION_DATABASE_USER", "web_api_owner")
        ]
        [("WEB_API_MIGRATION_DATABASE_PASSWORD", "owner-secret")]
        `shouldBe` Right
          AppSetupConfig
            { setupEnvironmentConfig =
                defaultAppEnvironmentConfig
                  { databaseConfig =
                      DatabaseConfig
                        { databaseHost = "127.0.0.1",
                          databasePort = 5432,
                          databaseName = "web_api_dev",
                          databaseUser = "web_api_runtime",
                          databasePassword = "web_api",
                          databaseConnectTimeoutSeconds = 10,
                          databasePoolCapacity = requiredDatabasePoolCapacity 10,
                          databaseTransportSecurity = DatabaseTransportLibpqDefault
                        }
                  },
              setupAppConfig = defaultAppConfig,
              setupMigrationDatabaseConfig =
                Just
                  DatabaseConfig
                    { databaseHost = "127.0.0.1",
                      databasePort = 5432,
                      databaseName = "web_api_dev",
                      databaseUser = "web_api_owner",
                      databasePassword = "owner-secret",
                      databaseConnectTimeoutSeconds = 10,
                      databasePoolCapacity = requiredDatabasePoolCapacity 1,
                      databaseTransportSecurity = DatabaseTransportLibpqDefault
                    },
              setupAutostartConfig = defaultSetupAutostartConfig
            }

    it "fails invalid runtime, setup, or partial migration config values explicitly" $ do
      parseAppSetupConfig
        testSetupDefaults
        []
        [("LISTENER_0_PORT", "0")]
        `shouldBe` Left (InvalidConfigValue "LISTENER_0_PORT" "0")
      parseAppSetupConfig
        testSetupDefaults
        []
        [("SETUP_AUTOSTART_DATABASE", "sometimes")]
        `shouldBe` Left (InvalidConfigValue "SETUP_AUTOSTART_DATABASE" "sometimes")
      parseAppSetupConfig
        testSetupDefaults
        []
        [("WEB_API_MIGRATION_DATABASE_HOST", "127.0.0.1")]
        `shouldBe` Left (MissingConfigValue "WEB_API_MIGRATION_DATABASE_PORT")
      parseAppSetupConfig
        testSetupDefaults
        []
        [ ("WEB_API_MIGRATION_DATABASE_HOST", "127.0.0.1"),
          ("WEB_API_MIGRATION_DATABASE_PORT", "0"),
          ("WEB_API_MIGRATION_DATABASE_NAME", "web_api_dev"),
          ("WEB_API_MIGRATION_DATABASE_USER", "web_api_owner"),
          ("WEB_API_MIGRATION_DATABASE_PASSWORD", "owner-secret")
        ]
        `shouldBe` Left (InvalidConfigValue "WEB_API_MIGRATION_DATABASE_PORT" "0")

  describe "loadAppSetupConfigWithFiles" $ do
    it "loads the documented .env then .env.local layers for setup config" $
      withSystemTempDirectory "app-setup-config" $ \tempDirectory ->
        withClearedAppEnvironment $
          withClearedRuntimeEnvironment $
            withClearedSetupEnvironment $ do
              let envPath = tempDirectory <> "/.env"
                  envLocalPath = tempDirectory <> "/.env.local"
              writeFile envPath "APP_TITLE_PREFIX=web-api-shared\nSETUP_AUTOSTART_DATABASE=true\n"
              writeFile envLocalPath "DATABASE_PASSWORD=web_api\nSMTP_PASSWORD=password\nTOTP_ENCRYPTION_KEY=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\nAPP_TITLE_PREFIX=web-api-local\nSETUP_AUTOSTART_JAEGER=yes\n"
              loadAppSetupConfigWithFiles envPath envLocalPath
                `shouldReturn` Right
                  AppSetupConfig
                    { setupEnvironmentConfig = defaultAppEnvironmentConfig,
                      setupAppConfig =
                        defaultAppConfig
                          { appTitlePrefix = "web-api-local"
                          },
                      setupMigrationDatabaseConfig = Nothing,
                      setupAutostartConfig =
                        SetupAutostartConfig
                          { setupAutostartDatabase = True,
                            setupAutostartJaeger = True
                          }
                    }

    it "lets process environment override .env.local values for setup config" $
      withSystemTempDirectory "app-setup-config-env" $ \tempDirectory ->
        withClearedAppEnvironment $
          withClearedRuntimeEnvironment $
            withClearedSetupEnvironment $
              withTemporaryEnvironment "APP_TITLE_PREFIX" (Just "web-api-runtime") $
                withTemporaryEnvironment "SETUP_AUTOSTART_DATABASE" (Just "false") $
                  withTemporaryEnvironment "SETUP_AUTOSTART_JAEGER" (Just "true") $ do
                    let envPath = tempDirectory <> "/.env"
                        envLocalPath = tempDirectory <> "/.env.local"
                    writeFile envPath "APP_TITLE_PREFIX=web-api-shared\nSETUP_AUTOSTART_DATABASE=true\n"
                    writeFile envLocalPath "DATABASE_PASSWORD=web_api\nSMTP_PASSWORD=password\nTOTP_ENCRYPTION_KEY=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\nAPP_TITLE_PREFIX=web-api-local\nSETUP_AUTOSTART_JAEGER=no\n"
                    loadAppSetupConfigWithFiles envPath envLocalPath
                      `shouldReturn` Right
                        AppSetupConfig
                          { setupEnvironmentConfig = defaultAppEnvironmentConfig,
                            setupAppConfig =
                              defaultAppConfig
                                { appTitlePrefix = "web-api-runtime"
                                },
                            setupMigrationDatabaseConfig = Nothing,
                            setupAutostartConfig =
                              SetupAutostartConfig
                                { setupAutostartDatabase = False,
                                  setupAutostartJaeger = True
                                }
                          }

    it "loads optional migration-owner credentials from the same file layers without replacing runtime credentials" $
      withSystemTempDirectory "app-setup-config-migration" $ \tempDirectory ->
        withClearedAppEnvironment $
          withClearedRuntimeEnvironment $
            withClearedSetupEnvironment $ do
              let envPath = tempDirectory <> "/.env"
                  envLocalPath = tempDirectory <> "/.env.local"
              writeFile
                envPath
                ( unlines
                    [ "DATABASE_USER=web_api_runtime",
                      "WEB_API_MIGRATION_DATABASE_HOST=127.0.0.1",
                      "WEB_API_MIGRATION_DATABASE_PORT=5432",
                      "WEB_API_MIGRATION_DATABASE_NAME=web_api_dev",
                      "WEB_API_MIGRATION_DATABASE_USER=web_api_owner"
                    ]
                )
              writeFile envLocalPath "DATABASE_PASSWORD=web_api\nSMTP_PASSWORD=password\nTOTP_ENCRYPTION_KEY=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\nWEB_API_MIGRATION_DATABASE_PASSWORD=owner-secret\n"
              loadAppSetupConfigWithFiles envPath envLocalPath
                `shouldReturn` Right
                  AppSetupConfig
                    { setupEnvironmentConfig =
                        defaultAppEnvironmentConfig
                          { databaseConfig =
                              DatabaseConfig
                                { databaseHost = "127.0.0.1",
                                  databasePort = 5432,
                                  databaseName = "web_api_dev",
                                  databaseUser = "web_api_runtime",
                                  databasePassword = "web_api",
                                  databaseConnectTimeoutSeconds = 10,
                                  databasePoolCapacity = requiredDatabasePoolCapacity 10,
                                  databaseTransportSecurity = DatabaseTransportLibpqDefault
                                }
                          },
                      setupAppConfig = defaultAppConfig,
                      setupMigrationDatabaseConfig =
                        Just
                          DatabaseConfig
                            { databaseHost = "127.0.0.1",
                              databasePort = 5432,
                              databaseName = "web_api_dev",
                              databaseUser = "web_api_owner",
                              databasePassword = "owner-secret",
                              databaseConnectTimeoutSeconds = 10,
                              databasePoolCapacity = requiredDatabasePoolCapacity 1,
                              databaseTransportSecurity = DatabaseTransportLibpqDefault
                            },
                      setupAutostartConfig = defaultSetupAutostartConfig
                    }

    it "reports invalid override files or parse failures with explicit errors" $
      withSystemTempDirectory "app-setup-config-errors" $ \tempDirectory ->
        withClearedAppEnvironment $
          withClearedRuntimeEnvironment $
            withClearedSetupEnvironment $ do
              let brokenEnvPath = tempDirectory <> "/broken.env"
                  envLocalPath = tempDirectory <> "/.env.local"
                  invalidEnvPath = tempDirectory <> "/invalid.env"
              writeFile brokenEnvPath "SETUP_AUTOSTART_DATABASE\n"
              loadAppSetupConfigWithFiles brokenEnvPath envLocalPath
                `shouldReturn` Left
                  (AppSetupOverridesFileError brokenEnvPath (InvalidConfigOverridesLine 1 "SETUP_AUTOSTART_DATABASE"))
              writeFile envLocalPath "DATABASE_PASSWORD=web_api\nSMTP_PASSWORD=password\nTOTP_ENCRYPTION_KEY=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\n"
              writeFile invalidEnvPath "SETUP_AUTOSTART_JAEGER=maybe\n"
              loadAppSetupConfigWithFiles invalidEnvPath envLocalPath
                `shouldReturn` Left
                  (AppSetupConfigParseError (InvalidConfigValue "SETUP_AUTOSTART_JAEGER" "maybe"))

    it "reports unreadable override files with the failing path" $
      withSystemTempDirectory "app-setup-config-unreadable" $ \tempDirectory ->
        withClearedAppEnvironment $
          withClearedRuntimeEnvironment $
            withClearedSetupEnvironment $ do
              let envPath = tempDirectory <> "/.env"
                  envLocalPath = tempDirectory <> "/.env.local"
              writeFile envPath "APP_TITLE_PREFIX=web-api-shared\nSETUP_AUTOSTART_DATABASE=true\n"
              withUnreadableFile envLocalPath "APP_TITLE_PREFIX=web-api-local\nSETUP_AUTOSTART_JAEGER=yes\n" $ do
                result <- loadAppSetupConfigWithFiles envPath envLocalPath
                result `shouldSatisfy` \case
                  Left
                    (AppSetupOverridesFileError failingPath (UnreadableConfigOverridesFile errorMessage))
                      | failingPath == envLocalPath ->
                          not (Text.null errorMessage)
                  _ -> False

  describe "loadAppSetupConfig" $
    it "loads the default .env file names for setup config from the current directory" $
      withSystemTempDirectory "app-setup-config-current-directory" $ \tempDirectory ->
        withClearedAppEnvironment $
          withClearedRuntimeEnvironment $
            withClearedSetupEnvironment $ do
              writeFile (tempDirectory <> "/.env") "SETUP_AUTOSTART_DATABASE=true\n"
              writeFile (tempDirectory <> "/.env.local") "DATABASE_PASSWORD=web_api\nSMTP_PASSWORD=password\nTOTP_ENCRYPTION_KEY=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\nAPP_TITLE_PREFIX=web-api-dev\nSETUP_AUTOSTART_JAEGER=true\n"
              withCurrentDirectory tempDirectory $
                loadAppSetupConfig
                  `shouldReturn` Right
                    AppSetupConfig
                      { setupEnvironmentConfig = defaultAppEnvironmentConfig,
                        setupAppConfig =
                          defaultAppConfig
                            { appTitlePrefix = "web-api-dev"
                            },
                        setupMigrationDatabaseConfig = Nothing,
                        setupAutostartConfig =
                          SetupAutostartConfig
                            { setupAutostartDatabase = True,
                              setupAutostartJaeger = True
                            }
                      }

  describe "AppSetupConfig and AppSetupConfigLoadError" $
    it "keep selectors, equality, and rendering deterministic" $ do
      let setupConfig =
            AppSetupConfig
              { setupEnvironmentConfig = defaultAppEnvironmentConfig {appMode = Test},
                setupAppConfig = defaultAppConfig {appTitlePrefix = "setup-app"},
                setupMigrationDatabaseConfig =
                  Just
                    DatabaseConfig
                      { databaseHost = "127.0.0.1",
                        databasePort = 5432,
                        databaseName = "web_api_dev",
                        databaseUser = "web_api_owner",
                        databasePassword = "owner-secret",
                        databaseConnectTimeoutSeconds = 10,
                        databasePoolCapacity = requiredDatabasePoolCapacity 10,
                        databaseTransportSecurity = DatabaseTransportLibpqDefault
                      },
                setupAutostartConfig =
                  SetupAutostartConfig
                    { setupAutostartDatabase = True,
                      setupAutostartJaeger = False
                    }
              }
          fileLoadError = AppSetupOverridesFileError ".env" (InvalidConfigOverridesLine 1 "BROKEN")
          parseLoadError = AppSetupConfigParseError (InvalidConfigValue "SETUP_AUTOSTART_DATABASE" "maybe")
      setupEnvironmentConfig setupConfig `shouldBe` defaultAppEnvironmentConfig {appMode = Test}
      setupAppConfig setupConfig `shouldBe` defaultAppConfig {appTitlePrefix = "setup-app"}
      setupMigrationDatabaseConfig setupConfig
        `shouldBe` Just
          DatabaseConfig
            { databaseHost = "127.0.0.1",
              databasePort = 5432,
              databaseName = "web_api_dev",
              databaseUser = "web_api_owner",
              databasePassword = "owner-secret",
              databaseConnectTimeoutSeconds = 10,
              databasePoolCapacity = requiredDatabasePoolCapacity 10,
              databaseTransportSecurity = DatabaseTransportLibpqDefault
            }
      setupAutostartConfig setupConfig
        `shouldBe` SetupAutostartConfig
          { setupAutostartDatabase = True,
            setupAutostartJaeger = False
          }
      setupAutostartDatabase (setupAutostartConfig setupConfig) `shouldBe` True
      setupAutostartJaeger (setupAutostartConfig setupConfig) `shouldBe` False
      defaultSetupAutostartConfig
        `shouldNotBe` SetupAutostartConfig
          { setupAutostartDatabase = False,
            setupAutostartJaeger = False
          }
      show defaultSetupAutostartConfig
        `shouldBe` "SetupAutostartConfig {setupAutostartDatabase = True, setupAutostartJaeger = False}"
      showsPrec 11 defaultSetupAutostartConfig ""
        `shouldBe` "(SetupAutostartConfig {setupAutostartDatabase = True, setupAutostartJaeger = False})"
      show [defaultSetupAutostartConfig]
        `shouldBe` "[SetupAutostartConfig {setupAutostartDatabase = True, setupAutostartJaeger = False}]"
      setupConfig
        `shouldNotBe` setupConfig
          { setupAutostartConfig =
              SetupAutostartConfig
                { setupAutostartDatabase = False,
                  setupAutostartJaeger = False
                }
          }
      show setupConfig
        `shouldContain` ("staticAssetContentTypes = " <> show defaultStaticAssetContentTypes)
      showsPrec 11 setupConfig ""
        `shouldContain` ("staticAssetContentTypes = " <> show defaultStaticAssetContentTypes)
      show [setupConfig]
        `shouldContain` ("staticAssetContentTypes = " <> show defaultStaticAssetContentTypes)
      fileLoadError `shouldNotBe` parseLoadError
      show fileLoadError
        `shouldBe` "AppSetupOverridesFileError \".env\" (InvalidConfigOverridesLine 1 \"BROKEN\")"
      show parseLoadError
        `shouldBe` "AppSetupConfigParseError (InvalidConfigValue \"SETUP_AUTOSTART_DATABASE\" \"maybe\")"
      show [fileLoadError, parseLoadError]
        `shouldBe` "[AppSetupOverridesFileError \".env\" (InvalidConfigOverridesLine 1 \"BROKEN\"),AppSetupConfigParseError (InvalidConfigValue \"SETUP_AUTOSTART_DATABASE\" \"maybe\")]"
