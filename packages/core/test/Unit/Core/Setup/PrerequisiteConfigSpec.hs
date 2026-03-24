{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Exception (finally)
import qualified Core.Config as CoreConfig
import qualified Core.Setup.Prerequisite as Prerequisite
import qualified Core.Setup.PrerequisiteConfig as PrerequisiteConfig
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.IO.Temp (withSystemTempDirectory)

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory directory action = do
  previousDirectory <- getCurrentDirectory
  setCurrentDirectory directory
  action `finally` setCurrentDirectory previousDirectory

spec = do
  describe "defaultSetupPrerequisiteConfig" $ do
    it "matches the committed defaults and keeps selectors, equality, and rendering deterministic" $ do
      let setupConfig = PrerequisiteConfig.defaultSetupPrerequisiteConfig
      PrerequisiteConfig.setupDatabaseEndpoint setupConfig
        `shouldBe` Prerequisite.TcpEndpoint
          { Prerequisite.tcpEndpointHost = "127.0.0.1",
            Prerequisite.tcpEndpointPort = 5432
          }
      PrerequisiteConfig.setupTracingEndpoint setupConfig `shouldBe` Nothing
      PrerequisiteConfig.setupAutostartDatabase setupConfig `shouldBe` True
      PrerequisiteConfig.setupAutostartJaeger setupConfig `shouldBe` False
      setupConfig
        `shouldBe` PrerequisiteConfig.SetupPrerequisiteConfig
          { PrerequisiteConfig.setupDatabaseEndpoint =
              Prerequisite.TcpEndpoint
                { Prerequisite.tcpEndpointHost = "127.0.0.1",
                  Prerequisite.tcpEndpointPort = 5432
                },
            PrerequisiteConfig.setupTracingEndpoint = Nothing,
            PrerequisiteConfig.setupAutostartDatabase = True,
            PrerequisiteConfig.setupAutostartJaeger = False
          }
      setupConfig
        `shouldNotBe` setupConfig {PrerequisiteConfig.setupAutostartJaeger = True}
      show setupConfig
        `shouldBe` "SetupPrerequisiteConfig {setupDatabaseEndpoint = TcpEndpoint {tcpEndpointHost = \"127.0.0.1\", tcpEndpointPort = 5432}, setupTracingEndpoint = Nothing, setupAutostartDatabase = True, setupAutostartJaeger = False}"
      show [setupConfig]
        `shouldBe` "[SetupPrerequisiteConfig {setupDatabaseEndpoint = TcpEndpoint {tcpEndpointHost = \"127.0.0.1\", tcpEndpointPort = 5432}, setupTracingEndpoint = Nothing, setupAutostartDatabase = True, setupAutostartJaeger = False}]"

  describe "parseSetupPrerequisiteConfig" $ do
    it "parses the built-in defaults when file layers are empty" $
      PrerequisiteConfig.parseSetupPrerequisiteConfig
        PrerequisiteConfig.committedPrerequisiteDefaults
        []
        []
        `shouldBe` Right PrerequisiteConfig.defaultSetupPrerequisiteConfig

    it "falls back to the default autostart values when setup flags are absent in every layer" $
      PrerequisiteConfig.parseSetupPrerequisiteConfig
        [ ("DATABASE_HOST", "db.internal"),
          ("DATABASE_PORT", "6543")
        ]
        []
        []
        `shouldBe` Right
          PrerequisiteConfig.SetupPrerequisiteConfig
            { PrerequisiteConfig.setupDatabaseEndpoint =
                Prerequisite.TcpEndpoint
                  { Prerequisite.tcpEndpointHost = "db.internal",
                    Prerequisite.tcpEndpointPort = 6543
                  },
              PrerequisiteConfig.setupTracingEndpoint = Nothing,
              PrerequisiteConfig.setupAutostartDatabase = True,
              PrerequisiteConfig.setupAutostartJaeger = False
            }

    it "lets later file layers override earlier values while keeping tracing optional" $
      PrerequisiteConfig.parseSetupPrerequisiteConfig
        PrerequisiteConfig.committedPrerequisiteDefaults
        [ ("DATABASE_HOST", "db.internal"),
          ("DATABASE_PORT", "6543"),
          ("OTLP_TRACING_ENDPOINT", "http://collector:4318/v1/traces"),
          ("SETUP_AUTOSTART_DATABASE", "false")
        ]
        [ ("DATABASE_HOST", "db.local"),
          ("DATABASE_PORT", "7654"),
          ("OTLP_TRACING_ENDPOINT", "https://collector.example/v1/traces"),
          ("SETUP_AUTOSTART_DATABASE", "true"),
          ("SETUP_AUTOSTART_JAEGER", "true")
        ]
        `shouldBe` Right
          PrerequisiteConfig.SetupPrerequisiteConfig
            { PrerequisiteConfig.setupDatabaseEndpoint =
                Prerequisite.TcpEndpoint
                  { Prerequisite.tcpEndpointHost = "db.local",
                    Prerequisite.tcpEndpointPort = 7654
                  },
              PrerequisiteConfig.setupTracingEndpoint = Just "https://collector.example/v1/traces",
              PrerequisiteConfig.setupAutostartDatabase = True,
              PrerequisiteConfig.setupAutostartJaeger = True
            }

    it "fails missing required values and invalid port or boolean entries explicitly" $ do
      PrerequisiteConfig.parseSetupPrerequisiteConfig [] [] []
        `shouldBe` Left (CoreConfig.MissingConfigValue "DATABASE_HOST")
      PrerequisiteConfig.parseSetupPrerequisiteConfig
        []
        [("DATABASE_HOST", "db.internal"), ("DATABASE_PORT", "nope")]
        []
        `shouldBe` Left (CoreConfig.InvalidConfigValue "DATABASE_PORT" "nope")
      PrerequisiteConfig.parseSetupPrerequisiteConfig
        PrerequisiteConfig.committedPrerequisiteDefaults
        []
        [("SETUP_AUTOSTART_DATABASE", "sometimes")]
        `shouldBe` Left (CoreConfig.InvalidConfigValue "SETUP_AUTOSTART_DATABASE" "sometimes")
      PrerequisiteConfig.parseSetupPrerequisiteConfig
        PrerequisiteConfig.committedPrerequisiteDefaults
        []
        [("SETUP_AUTOSTART_JAEGER", "later")]
        `shouldBe` Left (CoreConfig.InvalidConfigValue "SETUP_AUTOSTART_JAEGER" "later")

  describe "loadSetupPrerequisiteConfigWithFiles" $ do
    it "loads .env then .env.local with the documented precedence" $
      withSystemTempDirectory "setup-prerequisite-config" $ \tempDirectory -> do
        writeFile
          (tempDirectory <> "/.env")
          ( unlines
              [ "DATABASE_HOST=db.internal",
                "DATABASE_PORT=6543",
                "OTLP_TRACING_ENDPOINT=http://collector:4318/v1/traces",
                "SETUP_AUTOSTART_DATABASE=false"
              ]
          )
        writeFile
          (tempDirectory <> "/.env.local")
          ( unlines
              [ "DATABASE_PORT=7654",
                "SETUP_AUTOSTART_JAEGER=true"
              ]
          )
        PrerequisiteConfig.loadSetupPrerequisiteConfigWithFiles
          (tempDirectory <> "/.env")
          (tempDirectory <> "/.env.local")
          `shouldReturn` Right
            PrerequisiteConfig.SetupPrerequisiteConfig
              { PrerequisiteConfig.setupDatabaseEndpoint =
                  Prerequisite.TcpEndpoint
                    { Prerequisite.tcpEndpointHost = "db.internal",
                      Prerequisite.tcpEndpointPort = 7654
                    },
                PrerequisiteConfig.setupTracingEndpoint = Just "http://collector:4318/v1/traces",
                PrerequisiteConfig.setupAutostartDatabase = False,
                PrerequisiteConfig.setupAutostartJaeger = True
              }

    it "reports invalid override files and parse failures with explicit paths and errors" $
      withSystemTempDirectory "setup-prerequisite-config-errors" $ \tempDirectory -> do
        writeFile (tempDirectory <> "/broken.env") "BROKEN\n"
        PrerequisiteConfig.loadSetupPrerequisiteConfigWithFiles
          (tempDirectory <> "/broken.env")
          (tempDirectory <> "/missing.env.local")
          `shouldReturn` Left
            ( PrerequisiteConfig.SetupPrerequisiteOverridesFileError
                (tempDirectory <> "/broken.env")
                (CoreConfig.InvalidConfigOverridesLine 1 "BROKEN")
            )
        writeFile (tempDirectory <> "/valid.env") "DATABASE_HOST=db.internal\nDATABASE_PORT=6543\n"
        writeFile (tempDirectory <> "/invalid.env.local") "SETUP_AUTOSTART_JAEGER=later\n"
        PrerequisiteConfig.loadSetupPrerequisiteConfigWithFiles
          (tempDirectory <> "/valid.env")
          (tempDirectory <> "/invalid.env.local")
          `shouldReturn` Left
            ( PrerequisiteConfig.SetupPrerequisiteConfigParseError
                (CoreConfig.InvalidConfigValue "SETUP_AUTOSTART_JAEGER" "later")
            )

  describe "loadSetupPrerequisiteConfig" $ do
    it "loads the default .env filenames from the current directory" $
      withSystemTempDirectory "setup-prerequisite-default-files" $ \tempDirectory -> do
        writeFile
          (tempDirectory <> "/.env")
          ( unlines
              [ "DATABASE_HOST=db.internal",
                "DATABASE_PORT=6543"
              ]
          )
        withCurrentDirectory tempDirectory $
          PrerequisiteConfig.loadSetupPrerequisiteConfig
            `shouldReturn` Right
              PrerequisiteConfig.SetupPrerequisiteConfig
                { PrerequisiteConfig.setupDatabaseEndpoint =
                    Prerequisite.TcpEndpoint
                      { Prerequisite.tcpEndpointHost = "db.internal",
                        Prerequisite.tcpEndpointPort = 6543
                      },
                  PrerequisiteConfig.setupTracingEndpoint = Nothing,
                  PrerequisiteConfig.setupAutostartDatabase = True,
                  PrerequisiteConfig.setupAutostartJaeger = False
                }

  describe "SetupPrerequisiteConfigLoadError" $ do
    it "keeps load error equality and rendering deterministic" $ do
      let fileLoadError =
            PrerequisiteConfig.SetupPrerequisiteOverridesFileError
              ".env"
              (CoreConfig.InvalidConfigOverridesLine 1 "BROKEN")
          parseLoadError =
            PrerequisiteConfig.SetupPrerequisiteConfigParseError
              (CoreConfig.InvalidConfigValue "SETUP_AUTOSTART_DATABASE" "maybe")
      fileLoadError `shouldBe` fileLoadError
      fileLoadError `shouldNotBe` parseLoadError
      show fileLoadError
        `shouldBe` "SetupPrerequisiteOverridesFileError \".env\" (InvalidConfigOverridesLine 1 \"BROKEN\")"
      show parseLoadError
        `shouldBe` "SetupPrerequisiteConfigParseError (InvalidConfigValue \"SETUP_AUTOSTART_DATABASE\" \"maybe\")"
      show [fileLoadError, parseLoadError]
        `shouldBe` "[SetupPrerequisiteOverridesFileError \".env\" (InvalidConfigOverridesLine 1 \"BROKEN\"),SetupPrerequisiteConfigParseError (InvalidConfigValue \"SETUP_AUTOSTART_DATABASE\" \"maybe\")]"
