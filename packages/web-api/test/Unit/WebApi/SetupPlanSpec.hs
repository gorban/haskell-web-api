{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Core.Setup.PrerequisiteConfig qualified as PrerequisiteConfig
import Unit.WebApi.TestSupport (requiredDatabasePoolCapacity)
import WebApi.Config (AppConfig (..), AppEnvironmentConfig (..), DatabaseConfig (..), DatabaseTransportSecurity (..), ObservabilityConfig (..), OtlpExporter (..), defaultAppConfig, defaultAppEnvironmentConfig)
import WebApi.SetupConfig (AppSetupConfig (..), SetupAutostartConfig (..), defaultAppSetupConfig, defaultSetupAutostartConfig)
import WebApi.SetupPlan (AppPrerequisitePlan (..), ContainerAutostartPlan (..), ContainerRuntime (..), DatabasePrerequisitePlan (..), TcpEndpoint (..), TracingPrerequisitePlan (..), defaultContainerAutostartPlan, planAppPrerequisites, toSetupPrerequisiteConfig)

spec = do
  describe "planAppPrerequisites" $ do
    it "preserves runtime database identity fields in the shared setup prerequisite config" $ do
      let setupConfig =
            defaultAppSetupConfig
              { setupEnvironmentConfig =
                  defaultAppEnvironmentConfig
                    { databaseConfig =
                        DatabaseConfig
                          { databaseHost = "db.internal",
                            databasePort = 6543,
                            databaseName = "web_api_build",
                            databaseUser = "web_api_runtime",
                            databasePassword = "secret",
                            databaseConnectTimeoutSeconds = 10,
                            databasePoolCapacity = requiredDatabasePoolCapacity 10,
                            databaseTransportSecurity = DatabaseTransportLibpqDefault
                          }
                    }
              }
          prerequisiteConfig = toSetupPrerequisiteConfig setupConfig
      PrerequisiteConfig.setupDatabaseEndpoint prerequisiteConfig
        `shouldBe` TcpEndpoint
          { tcpEndpointHost = "db.internal",
            tcpEndpointPort = 6543
          }
      PrerequisiteConfig.setupDatabaseName prerequisiteConfig `shouldBe` "web_api_build"
      PrerequisiteConfig.setupDatabaseUser prerequisiteConfig `shouldBe` "web_api_runtime"
      PrerequisiteConfig.setupDatabasePassword prerequisiteConfig `shouldBe` "secret"

    it "always plans the configured database reachability check and skips disabled autostarts" $ do
      let setupConfig =
            defaultAppSetupConfig
              { setupEnvironmentConfig =
                  defaultAppEnvironmentConfig
                    { databaseConfig =
                        DatabaseConfig
                          { databaseHost = "db.internal",
                            databasePort = 6543,
                            databaseName = "web_api_build",
                            databaseUser = "web_api_runtime",
                            databasePassword = "secret",
                            databaseConnectTimeoutSeconds = 10,
                            databasePoolCapacity = requiredDatabasePoolCapacity 10,
                            databaseTransportSecurity = DatabaseTransportLibpqDefault
                          }
                    },
                setupAutostartConfig =
                  defaultSetupAutostartConfig
                    { setupAutostartDatabase = False
                    }
              }
      planAppPrerequisites setupConfig
        `shouldBe` AppPrerequisitePlan
          { databasePrerequisitePlan =
              DatabasePrerequisitePlan
                { databaseCheckEndpoint =
                    TcpEndpoint
                      { tcpEndpointHost = "db.internal",
                        tcpEndpointPort = 6543
                      },
                  databaseAutostartPlan = Nothing
                },
            tracingPrerequisitePlan = Nothing
          }

    it "still plans tracing reachability when tracing is configured but Jaeger autostart stays disabled" $ do
      let tracing =
            OtlpExporter
              { otlpEndpoint = "http://127.0.0.1:4318",
                otlpHeaders = []
              }
          setupConfig =
            defaultAppSetupConfig
              { setupAppConfig =
                  defaultAppConfig
                    { observability =
                        ObservabilityConfig
                          { tracingExporter = Just tracing,
                            metricsExporter = Nothing
                          }
                    },
                setupAutostartConfig =
                  defaultSetupAutostartConfig
                    { setupAutostartDatabase = True
                    }
              }
      planAppPrerequisites setupConfig
        `shouldBe` AppPrerequisitePlan
          { databasePrerequisitePlan =
              DatabasePrerequisitePlan
                { databaseCheckEndpoint =
                    TcpEndpoint
                      { tcpEndpointHost = "127.0.0.1",
                        tcpEndpointPort = 5432
                      },
                  databaseAutostartPlan = Just defaultContainerAutostartPlan
                },
            tracingPrerequisitePlan =
              Just
                TracingPrerequisitePlan
                  { tracingCheckEndpoint = "http://127.0.0.1:4318",
                    tracingAutostartPlan = Nothing
                  }
          }

    it "plans podman-then-docker autostart for database and tracing when enabled" $ do
      let tracing =
            OtlpExporter
              { otlpEndpoint = "http://127.0.0.1:4318",
                otlpHeaders = [("authorization", "Bearer token")]
              }
          setupConfig =
            defaultAppSetupConfig
              { setupAppConfig =
                  defaultAppConfig
                    { observability =
                        ObservabilityConfig
                          { tracingExporter = Just tracing,
                            metricsExporter = Nothing
                          }
                    },
                setupAutostartConfig =
                  SetupAutostartConfig
                    { setupAutostartDatabase = True,
                      setupAutostartJaeger = True
                    }
              }
      planAppPrerequisites setupConfig
        `shouldBe` AppPrerequisitePlan
          { databasePrerequisitePlan =
              DatabasePrerequisitePlan
                { databaseCheckEndpoint =
                    TcpEndpoint
                      { tcpEndpointHost = "127.0.0.1",
                        tcpEndpointPort = 5432
                      },
                  databaseAutostartPlan = Just defaultContainerAutostartPlan
                },
            tracingPrerequisitePlan =
              Just
                TracingPrerequisitePlan
                  { tracingCheckEndpoint = "http://127.0.0.1:4318",
                    tracingAutostartPlan = Just defaultContainerAutostartPlan
                  }
          }

    it "keeps planner model selectors, equality, and rendering deterministic" $ do
      let databaseEndpoint =
            TcpEndpoint
              { tcpEndpointHost = "db.internal",
                tcpEndpointPort = 6543
              }
          databasePlan =
            DatabasePrerequisitePlan
              { databaseCheckEndpoint = databaseEndpoint,
                databaseAutostartPlan = Just defaultContainerAutostartPlan
              }
          tracingPlan =
            TracingPrerequisitePlan
              { tracingCheckEndpoint = "http://127.0.0.1:4318",
                tracingAutostartPlan = Nothing
              }
          appPlan =
            AppPrerequisitePlan
              { databasePrerequisitePlan = databasePlan,
                tracingPrerequisitePlan = Just tracingPlan
              }
      PodmanRuntime `shouldNotBe` DockerRuntime
      show PodmanRuntime `shouldBe` "PodmanRuntime"
      show [PodmanRuntime, DockerRuntime] `shouldBe` "[PodmanRuntime,DockerRuntime]"
      autostartRuntimes defaultContainerAutostartPlan
        `shouldBe` [PodmanRuntime, DockerRuntime]
      defaultContainerAutostartPlan
        `shouldNotBe` ContainerAutostartPlan {autostartRuntimes = [DockerRuntime]}
      show defaultContainerAutostartPlan
        `shouldBe` "ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]}"
      databaseEndpoint
        `shouldNotBe` TcpEndpoint
          { tcpEndpointHost = "db.other",
            tcpEndpointPort = 6543
          }
      show databaseEndpoint
        `shouldBe` "TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543}"
      showsPrec 11 databaseEndpoint ""
        `shouldBe` "(TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543})"
      show [databaseEndpoint]
        `shouldBe` "[TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543}]"
      databaseCheckEndpoint databasePlan
        `shouldBe` TcpEndpoint
          { tcpEndpointHost = "db.internal",
            tcpEndpointPort = 6543
          }
      databasePlan
        `shouldNotBe` databasePlan
          { databaseAutostartPlan = Nothing
          }
      show databasePlan
        `shouldBe` "DatabasePrerequisitePlan {databaseCheckEndpoint = TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543}, databaseAutostartPlan = Just (ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]})}"
      showsPrec 11 databasePlan ""
        `shouldBe` "(DatabasePrerequisitePlan {databaseCheckEndpoint = TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543}, databaseAutostartPlan = Just (ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]})})"
      databaseAutostartPlan databasePlan `shouldBe` Just defaultContainerAutostartPlan
      tracingPlan
        `shouldNotBe` tracingPlan
          { tracingCheckEndpoint = "http://127.0.0.1:9999"
          }
      tracingCheckEndpoint tracingPlan `shouldBe` "http://127.0.0.1:4318"
      tracingAutostartPlan tracingPlan `shouldBe` Nothing
      show tracingPlan
        `shouldBe` "TracingPrerequisitePlan {tracingCheckEndpoint = \"http://127.0.0.1:4318\", tracingAutostartPlan = Nothing}"
      showsPrec 11 tracingPlan ""
        `shouldBe` "(TracingPrerequisitePlan {tracingCheckEndpoint = \"http://127.0.0.1:4318\", tracingAutostartPlan = Nothing})"
      databasePrerequisitePlan appPlan `shouldBe` databasePlan
      tracingPrerequisitePlan appPlan `shouldBe` Just tracingPlan
      appPlan
        `shouldNotBe` appPlan
          { tracingPrerequisitePlan = Nothing
          }
      show appPlan
        `shouldBe` "AppPrerequisitePlan {databasePrerequisitePlan = DatabasePrerequisitePlan {databaseCheckEndpoint = TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543}, databaseAutostartPlan = Just (ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]})}, tracingPrerequisitePlan = Just (TracingPrerequisitePlan {tracingCheckEndpoint = \"http://127.0.0.1:4318\", tracingAutostartPlan = Nothing})}"
      showsPrec 11 appPlan ""
        `shouldBe` "(AppPrerequisitePlan {databasePrerequisitePlan = DatabasePrerequisitePlan {databaseCheckEndpoint = TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543}, databaseAutostartPlan = Just (ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]})}, tracingPrerequisitePlan = Just (TracingPrerequisitePlan {tracingCheckEndpoint = \"http://127.0.0.1:4318\", tracingAutostartPlan = Nothing})})"
      show [defaultContainerAutostartPlan]
        `shouldBe` "[ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]}]"
      show [databasePlan]
        `shouldBe` "[DatabasePrerequisitePlan {databaseCheckEndpoint = TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543}, databaseAutostartPlan = Just (ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]})}]"
      show [tracingPlan]
        `shouldBe` "[TracingPrerequisitePlan {tracingCheckEndpoint = \"http://127.0.0.1:4318\", tracingAutostartPlan = Nothing}]"
      show [appPlan]
        `shouldBe` "[AppPrerequisitePlan {databasePrerequisitePlan = DatabasePrerequisitePlan {databaseCheckEndpoint = TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543}, databaseAutostartPlan = Just (ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]})}, tracingPrerequisitePlan = Just (TracingPrerequisitePlan {tracingCheckEndpoint = \"http://127.0.0.1:4318\", tracingAutostartPlan = Nothing})}]"
