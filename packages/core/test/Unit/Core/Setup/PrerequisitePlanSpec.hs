{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Core.Setup.Prerequisite qualified as Prerequisite
import Core.Setup.PrerequisiteConfig qualified as PrerequisiteConfig
import Core.Setup.PrerequisitePlan qualified as PrerequisitePlan

spec = do
  describe "planSetupPrerequisites" $ do
    it "always plans the configured database reachability check and skips disabled autostarts" $
      PrerequisitePlan.planSetupPrerequisites
        PrerequisiteConfig.SetupPrerequisiteConfig
          { PrerequisiteConfig.setupDatabaseEndpoint =
              Prerequisite.TcpEndpoint
                { Prerequisite.tcpEndpointHost = "db.internal",
                  Prerequisite.tcpEndpointPort = 6543
                },
            PrerequisiteConfig.setupDatabaseName = "web_api_build",
            PrerequisiteConfig.setupDatabaseUser = "web_api_runtime",
            PrerequisiteConfig.setupDatabasePassword = "secret",
            PrerequisiteConfig.setupTracingEndpoint = Nothing,
            PrerequisiteConfig.setupAutostartDatabase = False,
            PrerequisiteConfig.setupAutostartJaeger = False
          }
        `shouldBe` PrerequisitePlan.AppPrerequisitePlan
          { PrerequisitePlan.databasePrerequisitePlan =
              PrerequisitePlan.DatabasePrerequisitePlan
                { PrerequisitePlan.databaseCheckEndpoint =
                    Prerequisite.TcpEndpoint
                      { Prerequisite.tcpEndpointHost = "db.internal",
                        Prerequisite.tcpEndpointPort = 6543
                      },
                  PrerequisitePlan.databaseAutostartPlan = Nothing
                },
            PrerequisitePlan.tracingPrerequisitePlan = Nothing
          }

    it "still plans tracing reachability when tracing is configured but Jaeger autostart stays disabled" $
      PrerequisitePlan.planSetupPrerequisites
        PrerequisiteConfig.defaultSetupPrerequisiteConfig
          { PrerequisiteConfig.setupTracingEndpoint = Just "http://127.0.0.1:4318"
          }
        `shouldBe` PrerequisitePlan.AppPrerequisitePlan
          { PrerequisitePlan.databasePrerequisitePlan =
              PrerequisitePlan.DatabasePrerequisitePlan
                { PrerequisitePlan.databaseCheckEndpoint =
                    Prerequisite.TcpEndpoint
                      { Prerequisite.tcpEndpointHost = "127.0.0.1",
                        Prerequisite.tcpEndpointPort = 5432
                      },
                  PrerequisitePlan.databaseAutostartPlan = Just PrerequisitePlan.defaultContainerAutostartPlan
                },
            PrerequisitePlan.tracingPrerequisitePlan =
              Just
                PrerequisitePlan.TracingPrerequisitePlan
                  { PrerequisitePlan.tracingCheckEndpoint = "http://127.0.0.1:4318",
                    PrerequisitePlan.tracingAutostartPlan = Nothing
                  }
          }

    it "plans podman-then-docker autostart for database and tracing when enabled" $
      PrerequisitePlan.planSetupPrerequisites
        PrerequisiteConfig.defaultSetupPrerequisiteConfig
          { PrerequisiteConfig.setupTracingEndpoint = Just "http://127.0.0.1:4318",
            PrerequisiteConfig.setupAutostartJaeger = True
          }
        `shouldBe` PrerequisitePlan.AppPrerequisitePlan
          { PrerequisitePlan.databasePrerequisitePlan =
              PrerequisitePlan.DatabasePrerequisitePlan
                { PrerequisitePlan.databaseCheckEndpoint =
                    Prerequisite.TcpEndpoint
                      { Prerequisite.tcpEndpointHost = "127.0.0.1",
                        Prerequisite.tcpEndpointPort = 5432
                      },
                  PrerequisitePlan.databaseAutostartPlan = Just PrerequisitePlan.defaultContainerAutostartPlan
                },
            PrerequisitePlan.tracingPrerequisitePlan =
              Just
                PrerequisitePlan.TracingPrerequisitePlan
                  { PrerequisitePlan.tracingCheckEndpoint = "http://127.0.0.1:4318",
                    PrerequisitePlan.tracingAutostartPlan = Just PrerequisitePlan.defaultContainerAutostartPlan
                  }
          }

  describe "defaultContainerAutostartPlan and prerequisite plan rendering" $
    it "keep selectors, equality, and rendering deterministic" $ do
      let databaseEndpoint =
            Prerequisite.TcpEndpoint
              { Prerequisite.tcpEndpointHost = "db.internal",
                Prerequisite.tcpEndpointPort = 6543
              }
          databasePlan =
            PrerequisitePlan.DatabasePrerequisitePlan
              { PrerequisitePlan.databaseCheckEndpoint = databaseEndpoint,
                PrerequisitePlan.databaseAutostartPlan = Just PrerequisitePlan.defaultContainerAutostartPlan
              }
          tracingPlan =
            PrerequisitePlan.TracingPrerequisitePlan
              { PrerequisitePlan.tracingCheckEndpoint = "http://127.0.0.1:4318",
                PrerequisitePlan.tracingAutostartPlan = Nothing
              }
          appPlan =
            PrerequisitePlan.AppPrerequisitePlan
              { PrerequisitePlan.databasePrerequisitePlan = databasePlan,
                PrerequisitePlan.tracingPrerequisitePlan = Just tracingPlan
              }
          otherDatabasePlan =
            databasePlan
              { PrerequisitePlan.databaseAutostartPlan = Nothing
              }
          otherTracingPlan =
            tracingPlan
              { PrerequisitePlan.tracingAutostartPlan = Just PrerequisitePlan.defaultContainerAutostartPlan
              }
      PrerequisitePlan.autostartRuntimes PrerequisitePlan.defaultContainerAutostartPlan
        `shouldBe` [PrerequisitePlan.PodmanRuntime, PrerequisitePlan.DockerRuntime]
      PrerequisitePlan.PodmanRuntime `shouldBe` PrerequisitePlan.PodmanRuntime
      PrerequisitePlan.PodmanRuntime `shouldNotBe` PrerequisitePlan.DockerRuntime
      PrerequisitePlan.defaultContainerAutostartPlan `shouldBe` PrerequisitePlan.defaultContainerAutostartPlan
      PrerequisitePlan.defaultContainerAutostartPlan
        `shouldNotBe` PrerequisitePlan.ContainerAutostartPlan
          { PrerequisitePlan.autostartRuntimes = [PrerequisitePlan.DockerRuntime]
          }
      databasePlan `shouldBe` databasePlan
      databasePlan `shouldNotBe` otherDatabasePlan
      tracingPlan `shouldBe` tracingPlan
      tracingPlan `shouldNotBe` otherTracingPlan
      appPlan `shouldBe` appPlan
      appPlan
        `shouldNotBe` PrerequisitePlan.AppPrerequisitePlan
          { PrerequisitePlan.databasePrerequisitePlan = databasePlan,
            PrerequisitePlan.tracingPrerequisitePlan = Nothing
          }
      show PrerequisitePlan.PodmanRuntime `shouldBe` "PodmanRuntime"
      showsPrec 11 PrerequisitePlan.PodmanRuntime ""
        `shouldBe` "PodmanRuntime"
      show [PrerequisitePlan.PodmanRuntime] `shouldBe` "[PodmanRuntime]"
      show PrerequisitePlan.defaultContainerAutostartPlan
        `shouldBe` "ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]}"
      show databasePlan
        `shouldBe` "DatabasePrerequisitePlan {databaseCheckEndpoint = TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543}, databaseAutostartPlan = Just (ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]})}"
      show tracingPlan
        `shouldBe` "TracingPrerequisitePlan {tracingCheckEndpoint = \"http://127.0.0.1:4318\", tracingAutostartPlan = Nothing}"
      show appPlan
        `shouldBe` "AppPrerequisitePlan {databasePrerequisitePlan = DatabasePrerequisitePlan {databaseCheckEndpoint = TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543}, databaseAutostartPlan = Just (ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]})}, tracingPrerequisitePlan = Just (TracingPrerequisitePlan {tracingCheckEndpoint = \"http://127.0.0.1:4318\", tracingAutostartPlan = Nothing})}"
      show [PrerequisitePlan.defaultContainerAutostartPlan]
        `shouldBe` "[ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]}]"
      show [databasePlan]
        `shouldBe` "[DatabasePrerequisitePlan {databaseCheckEndpoint = TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543}, databaseAutostartPlan = Just (ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]})}]"
      show [tracingPlan]
        `shouldBe` "[TracingPrerequisitePlan {tracingCheckEndpoint = \"http://127.0.0.1:4318\", tracingAutostartPlan = Nothing}]"
      show [appPlan]
        `shouldBe` "[AppPrerequisitePlan {databasePrerequisitePlan = DatabasePrerequisitePlan {databaseCheckEndpoint = TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543}, databaseAutostartPlan = Just (ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]})}, tracingPrerequisitePlan = Just (TracingPrerequisitePlan {tracingCheckEndpoint = \"http://127.0.0.1:4318\", tracingAutostartPlan = Nothing})}]"
