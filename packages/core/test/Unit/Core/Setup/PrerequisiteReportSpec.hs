{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Exception (finally)
import Control.Monad (forM_)
import Core.Config qualified as CoreConfig
import Core.Setup.DatabaseAutostart qualified as DatabaseAutostart
import Core.Setup.Prerequisite qualified as Prerequisite
import Core.Setup.PrerequisiteConfig qualified as PrerequisiteConfig
import Core.Setup.PrerequisitePlan qualified as PrerequisitePlan
import Core.Setup.PrerequisiteReport qualified as PrerequisiteReport
import Core.Setup.TracingAutostart qualified as TracingAutostart
import Data.Text qualified as Text
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Network.Socket (Family (AF_INET), SockAddr (SockAddrInet), SocketType (Stream), bind, close, defaultProtocol, getSocketName, listen, socket, tupleToHostAddress)
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.IO (hClose, hFlush, stdout)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import System.Process (callProcess)

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory directory action = do
  previousDirectory <- getCurrentDirectory
  setCurrentDirectory directory
  action `finally` setCurrentDirectory previousDirectory

withListeningTcpEndpoint :: (Prerequisite.TcpEndpoint -> IO a) -> IO a
withListeningTcpEndpoint action = do
  listeningSocket <- socket AF_INET Stream defaultProtocol
  bind listeningSocket (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  listen listeningSocket 1
  socketAddress <- getSocketName listeningSocket
  case socketAddress of
    SockAddrInet port _ ->
      action
        Prerequisite.TcpEndpoint
          { Prerequisite.tcpEndpointHost = "127.0.0.1",
            Prerequisite.tcpEndpointPort = fromIntegral port
          }
        `finally` close listeningSocket
    _ -> close listeningSocket >> error "expected IPv4 listening socket"

withUnusedTcpEndpoint :: (Prerequisite.TcpEndpoint -> IO a) -> IO a
withUnusedTcpEndpoint action = do
  reservedSocket <- socket AF_INET Stream defaultProtocol
  bind reservedSocket (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  socketAddress <- getSocketName reservedSocket
  case socketAddress of
    SockAddrInet port _ -> do
      close reservedSocket
      action
        Prerequisite.TcpEndpoint
          { Prerequisite.tcpEndpointHost = "127.0.0.1",
            Prerequisite.tcpEndpointPort = fromIntegral port
          }
    _ -> close reservedSocket >> error "expected IPv4 reserved socket"

captureStdout :: IO () -> IO String
captureStdout action =
  withSystemTempFile "setup-prerequisite-stdout.txt" $ \outputPath outputHandle -> do
    originalStdout <- hDuplicate stdout
    hDuplicateTo outputHandle stdout
    action
      `finally` do
        hFlush stdout
        hDuplicateTo originalStdout stdout
        hClose originalStdout
        hClose outputHandle
    readFile outputPath

withPathScripts :: [(FilePath, String)] -> IO a -> IO a
withPathScripts scripts action =
  withSystemTempDirectory "setup-prerequisite-bin" $ \tempDirectory -> do
    forM_ scripts $ \(scriptName, scriptBody) -> do
      let scriptPath = tempDirectory <> "/" <> scriptName
      writeFile scriptPath scriptBody
      callProcess "chmod" ["+x", scriptPath]
    originalPath <- lookupEnv "PATH"
    let updatedPath =
          maybe
            tempDirectory
            (\existingPath -> tempDirectory <> ":" <> existingPath)
            originalPath
    setEnv "PATH" updatedPath
    action
      `finally` maybe
        (unsetEnv "PATH")
        (setEnv "PATH")
        originalPath

unusedTracingAutostart :: PrerequisitePlan.TracingPrerequisitePlan -> IO TracingAutostart.TracingAutostartResult
unusedTracingAutostart _ =
  expectationFailure "tracing autostart should not run"
    >> pure (TracingAutostart.TracingAutostartSkipped "tracing autostart should not run")

spec = do
  describe "checkSetupPrerequisitesWith" $ do
    it "reports explicit load failures without attempting checks" $ do
      let loadError =
            PrerequisiteConfig.SetupPrerequisiteConfigParseError
              (CoreConfig.InvalidConfigValue "DATABASE_PORT" "nope")
      PrerequisiteReport.checkSetupPrerequisitesWith
        (pure (Left loadError))
        (\_ -> expectationFailure "database check should not run" >> pure False)
        (\_ -> expectationFailure "tracing check should not run" >> pure (Right False))
        `shouldReturn` Left loadError

    it "reports reachable database prerequisites and skips absent tracing config" $
      PrerequisiteReport.checkSetupPrerequisitesWith
        (pure (Right PrerequisiteConfig.defaultSetupPrerequisiteConfig))
        ( \endpoint -> do
            endpoint
              `shouldBe` Prerequisite.TcpEndpoint
                { Prerequisite.tcpEndpointHost = "127.0.0.1",
                  Prerequisite.tcpEndpointPort = 5432
                }
            pure True
        )
        (\_ -> expectationFailure "tracing check should not run" >> pure (Right False))
        `shouldReturn` Right
          PrerequisiteReport.SetupPrerequisiteReport
            { PrerequisiteReport.databasePrerequisiteStatus =
                PrerequisiteReport.DatabasePrerequisiteReachable
                  Prerequisite.TcpEndpoint
                    { Prerequisite.tcpEndpointHost = "127.0.0.1",
                      Prerequisite.tcpEndpointPort = 5432
                    },
              PrerequisiteReport.tracingPrerequisiteStatus = Nothing
            }

    it "reports unreachable database and tracing prerequisites when both checks fail" $
      PrerequisiteReport.checkSetupPrerequisitesWith
        ( pure
            ( Right
                PrerequisiteConfig.defaultSetupPrerequisiteConfig
                  { PrerequisiteConfig.setupTracingEndpoint = Just "http://collector:4318/v1/traces",
                    PrerequisiteConfig.setupAutostartJaeger = True
                  }
            )
        )
        (\_ -> pure False)
        ( \endpoint -> do
            endpoint `shouldBe` "http://collector:4318/v1/traces"
            pure (Right False)
        )
        `shouldReturn` Right
          PrerequisiteReport.SetupPrerequisiteReport
            { PrerequisiteReport.databasePrerequisiteStatus =
                PrerequisiteReport.DatabasePrerequisiteUnreachable
                  PrerequisitePlan.DatabasePrerequisitePlan
                    { PrerequisitePlan.databaseCheckEndpoint =
                        Prerequisite.TcpEndpoint
                          { Prerequisite.tcpEndpointHost = "127.0.0.1",
                            Prerequisite.tcpEndpointPort = 5432
                          },
                      PrerequisitePlan.databaseAutostartPlan = Just PrerequisitePlan.defaultContainerAutostartPlan
                    },
              PrerequisiteReport.tracingPrerequisiteStatus =
                Just
                  ( PrerequisiteReport.TracingPrerequisiteUnreachable
                      PrerequisitePlan.TracingPrerequisitePlan
                        { PrerequisitePlan.tracingCheckEndpoint = "http://collector:4318/v1/traces",
                          PrerequisitePlan.tracingAutostartPlan = Just PrerequisitePlan.defaultContainerAutostartPlan
                        }
                  )
            }

    it "surfaces invalid tracing endpoint parse errors explicitly" $
      PrerequisiteReport.checkSetupPrerequisitesWith
        ( pure
            ( Right
                PrerequisiteConfig.defaultSetupPrerequisiteConfig
                  { PrerequisiteConfig.setupTracingEndpoint = Just "collector:4318/v1/traces"
                  }
            )
        )
        (\_ -> pure True)
        (\_ -> pure (Left (Prerequisite.InvalidTracingEndpointFormat "collector:4318/v1/traces")))
        `shouldReturn` Right
          PrerequisiteReport.SetupPrerequisiteReport
            { PrerequisiteReport.databasePrerequisiteStatus =
                PrerequisiteReport.DatabasePrerequisiteReachable
                  Prerequisite.TcpEndpoint
                    { Prerequisite.tcpEndpointHost = "127.0.0.1",
                      Prerequisite.tcpEndpointPort = 5432
                    },
              PrerequisiteReport.tracingPrerequisiteStatus =
                Just
                  ( PrerequisiteReport.TracingPrerequisiteInvalidEndpoint
                      "collector:4318/v1/traces"
                      (Prerequisite.InvalidTracingEndpointFormat "collector:4318/v1/traces")
                  )
            }

  describe "renderSetupPrerequisiteReport" $ do
    it "renders load failures, reachability outcomes, and autostart hints deterministically" $ do
      let loadError =
            PrerequisiteConfig.SetupPrerequisiteConfigParseError
              (CoreConfig.InvalidConfigValue "DATABASE_PORT" "nope")
          databasePlan =
            PrerequisitePlan.DatabasePrerequisitePlan
              { PrerequisitePlan.databaseCheckEndpoint =
                  Prerequisite.TcpEndpoint
                    { Prerequisite.tcpEndpointHost = "db.internal",
                      Prerequisite.tcpEndpointPort = 6543
                    },
                PrerequisitePlan.databaseAutostartPlan = Just PrerequisitePlan.defaultContainerAutostartPlan
              }
          tracingPlan =
            PrerequisitePlan.TracingPrerequisitePlan
              { PrerequisitePlan.tracingCheckEndpoint = "http://collector:4318/v1/traces",
                PrerequisitePlan.tracingAutostartPlan = Just PrerequisitePlan.defaultContainerAutostartPlan
              }
      PrerequisiteReport.renderSetupPrerequisiteReport
        (Left loadError)
        `shouldBe` ["Setup: Failed to load prerequisite config: SetupPrerequisiteConfigParseError (InvalidConfigValue \"DATABASE_PORT\" \"nope\")"]
      PrerequisiteReport.renderSetupPrerequisiteReport
        ( Right
            PrerequisiteReport.SetupPrerequisiteReport
              { PrerequisiteReport.databasePrerequisiteStatus =
                  PrerequisiteReport.DatabasePrerequisiteReachable
                    Prerequisite.TcpEndpoint
                      { Prerequisite.tcpEndpointHost = "db.internal",
                        Prerequisite.tcpEndpointPort = 6543
                      },
                PrerequisiteReport.tracingPrerequisiteStatus = Nothing
              }
        )
        `shouldBe` ["Setup: Database prerequisite reachable at db.internal:6543."]
      PrerequisiteReport.renderSetupPrerequisiteReport
        ( Right
            PrerequisiteReport.SetupPrerequisiteReport
              { PrerequisiteReport.databasePrerequisiteStatus =
                  PrerequisiteReport.DatabasePrerequisiteUnreachable
                    databasePlan
                      { PrerequisitePlan.databaseAutostartPlan = Nothing
                      },
                PrerequisiteReport.tracingPrerequisiteStatus =
                  Just
                    ( PrerequisiteReport.TracingPrerequisiteUnreachable
                        tracingPlan
                    )
              }
        )
        `shouldBe` [ "Setup: Database prerequisite unreachable at db.internal:6543.",
                     "Setup: Tracing prerequisite unreachable at http://collector:4318/v1/traces. Configured autostart runtimes: podman, docker."
                   ]
      PrerequisiteReport.renderSetupPrerequisiteReport
        ( Right
            PrerequisiteReport.SetupPrerequisiteReport
              { PrerequisiteReport.databasePrerequisiteStatus =
                  PrerequisiteReport.DatabasePrerequisiteAutostarted
                    databasePlan
                    PrerequisitePlan.PodmanRuntime,
                PrerequisiteReport.tracingPrerequisiteStatus = Nothing
              }
        )
        `shouldBe` [ "Setup: Database prerequisite unreachable at db.internal:6543. Configured autostart runtimes: podman, docker.",
                     "Setup: Started local PostgreSQL container via podman."
                   ]
      PrerequisiteReport.renderSetupPrerequisiteReport
        ( Right
            PrerequisiteReport.SetupPrerequisiteReport
              { PrerequisiteReport.databasePrerequisiteStatus =
                  PrerequisiteReport.DatabasePrerequisiteAutostartSkipped
                    databasePlan
                    "automatic database autostart only supports DATABASE_HOST values 127.0.0.1 or 0.0.0.0, but got db.internal",
                PrerequisiteReport.tracingPrerequisiteStatus = Nothing
              }
        )
        `shouldBe` [ "Setup: Database prerequisite unreachable at db.internal:6543. Configured autostart runtimes: podman, docker.",
                     "Setup: Skipping database autostart: automatic database autostart only supports DATABASE_HOST values 127.0.0.1 or 0.0.0.0, but got db.internal."
                   ]
      PrerequisiteReport.renderSetupPrerequisiteReport
        ( Right
            PrerequisiteReport.SetupPrerequisiteReport
              { PrerequisiteReport.databasePrerequisiteStatus =
                  PrerequisiteReport.DatabasePrerequisiteAutostartFailed
                    databasePlan
                    [ DatabaseAutostart.ContainerRuntimeFailure
                        { DatabaseAutostart.failedContainerRuntime = PrerequisitePlan.PodmanRuntime,
                          DatabaseAutostart.containerRuntimeFailureMessage = "podman missing"
                        },
                      DatabaseAutostart.ContainerRuntimeFailure
                        { DatabaseAutostart.failedContainerRuntime = PrerequisitePlan.DockerRuntime,
                          DatabaseAutostart.containerRuntimeFailureMessage = "docker missing"
                        }
                    ],
                PrerequisiteReport.tracingPrerequisiteStatus = Nothing
              }
        )
        `shouldBe` [ "Setup: Database prerequisite unreachable at db.internal:6543. Configured autostart runtimes: podman, docker.",
                     "Setup: Database autostart via podman failed: podman missing.",
                     "Setup: Database autostart via docker failed: docker missing.",
                     "Setup: Continuing without database autostart."
                   ]
      PrerequisiteReport.renderSetupPrerequisiteReport
        ( Right
            PrerequisiteReport.SetupPrerequisiteReport
              { PrerequisiteReport.databasePrerequisiteStatus =
                  PrerequisiteReport.DatabasePrerequisiteReachable
                    Prerequisite.TcpEndpoint
                      { Prerequisite.tcpEndpointHost = "127.0.0.1",
                        Prerequisite.tcpEndpointPort = 5432
                      },
                PrerequisiteReport.tracingPrerequisiteStatus =
                  Just
                    ( PrerequisiteReport.TracingPrerequisiteAutostarted
                        tracingPlan
                        PrerequisitePlan.PodmanRuntime
                    )
              }
        )
        `shouldBe` [ "Setup: Database prerequisite reachable at 127.0.0.1:5432.",
                     "Setup: Tracing prerequisite unreachable at http://collector:4318/v1/traces. Configured autostart runtimes: podman, docker.",
                     "Setup: Started Jaeger container via podman."
                   ]
      PrerequisiteReport.renderSetupPrerequisiteReport
        ( Right
            PrerequisiteReport.SetupPrerequisiteReport
              { PrerequisiteReport.databasePrerequisiteStatus =
                  PrerequisiteReport.DatabasePrerequisiteReachable
                    Prerequisite.TcpEndpoint
                      { Prerequisite.tcpEndpointHost = "127.0.0.1",
                        Prerequisite.tcpEndpointPort = 5432
                      },
                PrerequisiteReport.tracingPrerequisiteStatus =
                  Just
                    ( PrerequisiteReport.TracingPrerequisiteAutostartSkipped
                        tracingPlan
                        "unsupported host"
                    )
              }
        )
        `shouldBe` [ "Setup: Database prerequisite reachable at 127.0.0.1:5432.",
                     "Setup: Tracing prerequisite unreachable at http://collector:4318/v1/traces. Configured autostart runtimes: podman, docker.",
                     "Setup: Skipping tracing autostart: unsupported host."
                   ]
      PrerequisiteReport.renderSetupPrerequisiteReport
        ( Right
            PrerequisiteReport.SetupPrerequisiteReport
              { PrerequisiteReport.databasePrerequisiteStatus =
                  PrerequisiteReport.DatabasePrerequisiteReachable
                    Prerequisite.TcpEndpoint
                      { Prerequisite.tcpEndpointHost = "127.0.0.1",
                        Prerequisite.tcpEndpointPort = 5432
                      },
                PrerequisiteReport.tracingPrerequisiteStatus =
                  Just
                    ( PrerequisiteReport.TracingPrerequisiteAutostartFailed
                        tracingPlan
                        [ DatabaseAutostart.ContainerRuntimeFailure
                            { DatabaseAutostart.failedContainerRuntime = PrerequisitePlan.PodmanRuntime,
                              DatabaseAutostart.containerRuntimeFailureMessage = "podman missing"
                            },
                          DatabaseAutostart.ContainerRuntimeFailure
                            { DatabaseAutostart.failedContainerRuntime = PrerequisitePlan.DockerRuntime,
                              DatabaseAutostart.containerRuntimeFailureMessage = "docker missing"
                            }
                        ]
                    )
              }
        )
        `shouldBe` [ "Setup: Database prerequisite reachable at 127.0.0.1:5432.",
                     "Setup: Tracing prerequisite unreachable at http://collector:4318/v1/traces. Configured autostart runtimes: podman, docker.",
                     "Setup: Tracing autostart via podman failed: podman missing.",
                     "Setup: Tracing autostart via docker failed: docker missing.",
                     "Setup: Continuing without tracing autostart."
                   ]
      PrerequisiteReport.renderSetupPrerequisiteReport
        ( Right
            PrerequisiteReport.SetupPrerequisiteReport
              { PrerequisiteReport.databasePrerequisiteStatus =
                  PrerequisiteReport.DatabasePrerequisiteReachable
                    Prerequisite.TcpEndpoint
                      { Prerequisite.tcpEndpointHost = "127.0.0.1",
                        Prerequisite.tcpEndpointPort = 5432
                      },
                PrerequisiteReport.tracingPrerequisiteStatus =
                  Just
                    ( PrerequisiteReport.TracingPrerequisiteInvalidEndpoint
                        "collector:4318/v1/traces"
                        (Prerequisite.InvalidTracingEndpointFormat "collector:4318/v1/traces")
                    )
              }
        )
        `shouldBe` [ "Setup: Database prerequisite reachable at 127.0.0.1:5432.",
                     "Setup: Tracing prerequisite endpoint collector:4318/v1/traces is invalid: InvalidTracingEndpointFormat \"collector:4318/v1/traces\"."
                   ]

  describe "reportSetupPrerequisitesWith" $ do
    it "writes load failures to the supplied handle without attempting checks or autostart" $
      withSystemTempFile "setup-prerequisite-report.txt" $ \outputPath outputHandle -> do
        let loadError =
              PrerequisiteConfig.SetupPrerequisiteConfigParseError
                (CoreConfig.InvalidConfigValue "DATABASE_PORT" "bad")
        PrerequisiteReport.reportSetupPrerequisitesWith
          (pure (Left loadError))
          (\_ -> expectationFailure "database check should not run" >> pure False)
          (\_ -> expectationFailure "tracing check should not run" >> pure (Right False))
          (\_ _ -> expectationFailure "database autostart should not run" >> pure (DatabaseAutostart.DatabaseAutostartSkipped "database autostart should not run"))
          unusedTracingAutostart
          outputHandle
        hClose outputHandle
        readFile outputPath
          `shouldReturn` unlines
            [ "Setup: Failed to load prerequisite config: SetupPrerequisiteConfigParseError (InvalidConfigValue \"DATABASE_PORT\" \"bad\")"
            ]

    it "writes the rendered report to the supplied handle after applying database autostart" $
      withSystemTempFile "setup-prerequisite-report.txt" $ \outputPath outputHandle -> do
        PrerequisiteReport.reportSetupPrerequisitesWith
          ( pure
              ( Right
                  PrerequisiteConfig.defaultSetupPrerequisiteConfig
                    { PrerequisiteConfig.setupTracingEndpoint = Just "http://collector:4318/v1/traces"
                    }
              )
          )
          (\_ -> pure False)
          (\_ -> pure (Right True))
          ( \setupConfig databasePlan -> do
              PrerequisiteConfig.setupDatabaseName setupConfig `shouldBe` "web_api_dev"
              PrerequisitePlan.databaseCheckEndpoint databasePlan
                `shouldBe` Prerequisite.TcpEndpoint
                  { Prerequisite.tcpEndpointHost = "127.0.0.1",
                    Prerequisite.tcpEndpointPort = 5432
                  }
              pure (DatabaseAutostart.DatabaseAutostartSucceeded PrerequisitePlan.PodmanRuntime)
          )
          unusedTracingAutostart
          outputHandle
        hClose outputHandle
        readFile outputPath
          `shouldReturn` unlines
            [ "Setup: Database prerequisite unreachable at 127.0.0.1:5432. Configured autostart runtimes: podman, docker.",
              "Setup: Started local PostgreSQL container via podman.",
              "Setup: Tracing prerequisite reachable at http://collector:4318/v1/traces."
            ]

    it "leaves unreachable database reports unchanged when autostart is disabled in setup config" $
      withSystemTempFile "setup-prerequisite-report.txt" $ \outputPath outputHandle -> do
        PrerequisiteReport.reportSetupPrerequisitesWith
          ( pure
              ( Right
                  PrerequisiteConfig.defaultSetupPrerequisiteConfig
                    { PrerequisiteConfig.setupAutostartDatabase = False
                    }
              )
          )
          (\_ -> pure False)
          (\_ -> pure (Right False))
          (\_ _ -> expectationFailure "database autostart should not run" >> pure (DatabaseAutostart.DatabaseAutostartSkipped "database autostart should not run"))
          unusedTracingAutostart
          outputHandle
        hClose outputHandle
        readFile outputPath
          `shouldReturn` unlines
            [ "Setup: Database prerequisite unreachable at 127.0.0.1:5432."
            ]

    it "writes skipped database autostart outcomes to the supplied handle" $
      withSystemTempFile "setup-prerequisite-report.txt" $ \outputPath outputHandle -> do
        PrerequisiteReport.reportSetupPrerequisitesWith
          (pure (Right PrerequisiteConfig.defaultSetupPrerequisiteConfig))
          (\_ -> pure False)
          (\_ -> pure (Right False))
          (\_ _ -> pure (DatabaseAutostart.DatabaseAutostartSkipped "unsupported host"))
          unusedTracingAutostart
          outputHandle
        hClose outputHandle
        readFile outputPath
          `shouldReturn` unlines
            [ "Setup: Database prerequisite unreachable at 127.0.0.1:5432. Configured autostart runtimes: podman, docker.",
              "Setup: Skipping database autostart: unsupported host."
            ]

    it "writes failed database autostart outcomes to the supplied handle" $
      withSystemTempFile "setup-prerequisite-report.txt" $ \outputPath outputHandle -> do
        PrerequisiteReport.reportSetupPrerequisitesWith
          (pure (Right PrerequisiteConfig.defaultSetupPrerequisiteConfig))
          (\_ -> pure False)
          (\_ -> pure (Right False))
          ( \_ _ ->
              pure $
                DatabaseAutostart.DatabaseAutostartFailed
                  [ DatabaseAutostart.ContainerRuntimeFailure
                      { DatabaseAutostart.failedContainerRuntime = PrerequisitePlan.PodmanRuntime,
                        DatabaseAutostart.containerRuntimeFailureMessage = "podman missing"
                      }
                  ]
          )
          unusedTracingAutostart
          outputHandle
        hClose outputHandle
        readFile outputPath
          `shouldReturn` unlines
            [ "Setup: Database prerequisite unreachable at 127.0.0.1:5432. Configured autostart runtimes: podman, docker.",
              "Setup: Database autostart via podman failed: podman missing.",
              "Setup: Continuing without database autostart."
            ]

    it "writes successful tracing autostart outcomes to the supplied handle" $
      withSystemTempFile "setup-prerequisite-report.txt" $ \outputPath outputHandle -> do
        PrerequisiteReport.reportSetupPrerequisitesWith
          ( pure
              ( Right
                  PrerequisiteConfig.defaultSetupPrerequisiteConfig
                    { PrerequisiteConfig.setupTracingEndpoint = Just "http://collector:4318/v1/traces",
                      PrerequisiteConfig.setupAutostartJaeger = True
                    }
              )
          )
          (\_ -> pure True)
          (\_ -> pure (Right False))
          (\_ _ -> expectationFailure "database autostart should not run" >> pure (DatabaseAutostart.DatabaseAutostartSkipped "database autostart should not run"))
          ( \tracingPlan -> do
              PrerequisitePlan.tracingCheckEndpoint tracingPlan
                `shouldBe` "http://collector:4318/v1/traces"
              pure (TracingAutostart.TracingAutostartSucceeded PrerequisitePlan.PodmanRuntime)
          )
          outputHandle
        hClose outputHandle
        readFile outputPath
          `shouldReturn` unlines
            [ "Setup: Database prerequisite reachable at 127.0.0.1:5432.",
              "Setup: Tracing prerequisite unreachable at http://collector:4318/v1/traces. Configured autostart runtimes: podman, docker.",
              "Setup: Started Jaeger container via podman."
            ]

    it "writes failed tracing autostart outcomes to the supplied handle" $
      withSystemTempFile "setup-prerequisite-report.txt" $ \outputPath outputHandle -> do
        PrerequisiteReport.reportSetupPrerequisitesWith
          ( pure
              ( Right
                  PrerequisiteConfig.defaultSetupPrerequisiteConfig
                    { PrerequisiteConfig.setupTracingEndpoint = Just "http://collector:4318/v1/traces",
                      PrerequisiteConfig.setupAutostartJaeger = True
                    }
              )
          )
          (\_ -> pure True)
          (\_ -> pure (Right False))
          (\_ _ -> expectationFailure "database autostart should not run" >> pure (DatabaseAutostart.DatabaseAutostartSkipped "database autostart should not run"))
          ( \_ ->
              pure $
                TracingAutostart.TracingAutostartFailed
                  [ DatabaseAutostart.ContainerRuntimeFailure
                      { DatabaseAutostart.failedContainerRuntime = PrerequisitePlan.PodmanRuntime,
                        DatabaseAutostart.containerRuntimeFailureMessage = "podman missing"
                      }
                  ]
          )
          outputHandle
        hClose outputHandle
        readFile outputPath
          `shouldReturn` unlines
            [ "Setup: Database prerequisite reachable at 127.0.0.1:5432.",
              "Setup: Tracing prerequisite unreachable at http://collector:4318/v1/traces. Configured autostart runtimes: podman, docker.",
              "Setup: Tracing autostart via podman failed: podman missing.",
              "Setup: Continuing without tracing autostart."
            ]

    it "writes skipped tracing autostart outcomes to the supplied handle" $
      withSystemTempFile "setup-prerequisite-report.txt" $ \outputPath outputHandle -> do
        PrerequisiteReport.reportSetupPrerequisitesWith
          ( pure
              ( Right
                  PrerequisiteConfig.defaultSetupPrerequisiteConfig
                    { PrerequisiteConfig.setupTracingEndpoint = Just "http://collector:4318/v1/traces",
                      PrerequisiteConfig.setupAutostartJaeger = True
                    }
              )
          )
          (\_ -> pure True)
          (\_ -> pure (Right False))
          (\_ _ -> expectationFailure "database autostart should not run" >> pure (DatabaseAutostart.DatabaseAutostartSkipped "database autostart should not run"))
          (\_ -> pure (TracingAutostart.TracingAutostartSkipped "unsupported host"))
          outputHandle
        hClose outputHandle
        readFile outputPath
          `shouldReturn` unlines
            [ "Setup: Database prerequisite reachable at 127.0.0.1:5432.",
              "Setup: Tracing prerequisite unreachable at http://collector:4318/v1/traces. Configured autostart runtimes: podman, docker.",
              "Setup: Skipping tracing autostart: unsupported host."
            ]

    it "leaves unreachable tracing reports unchanged when tracing autostart is disabled in setup config" $
      withSystemTempFile "setup-prerequisite-report.txt" $ \outputPath outputHandle -> do
        PrerequisiteReport.reportSetupPrerequisitesWith
          ( pure
              ( Right
                  PrerequisiteConfig.defaultSetupPrerequisiteConfig
                    { PrerequisiteConfig.setupTracingEndpoint = Just "http://collector:4318/v1/traces",
                      PrerequisiteConfig.setupAutostartJaeger = False
                    }
              )
          )
          (\_ -> pure True)
          (\_ -> pure (Right False))
          (\_ _ -> expectationFailure "database autostart should not run" >> pure (DatabaseAutostart.DatabaseAutostartSkipped "database autostart should not run"))
          unusedTracingAutostart
          outputHandle
        hClose outputHandle
        readFile outputPath
          `shouldReturn` unlines
            [ "Setup: Database prerequisite reachable at 127.0.0.1:5432.",
              "Setup: Tracing prerequisite unreachable at http://collector:4318/v1/traces."
            ]

    it "leaves tracing reports unchanged when tracing is reachable or invalid" $
      withSystemTempFile "setup-prerequisite-report.txt" $ \outputPath outputHandle -> do
        PrerequisiteReport.reportSetupPrerequisitesWith
          ( pure
              ( Right
                  PrerequisiteConfig.defaultSetupPrerequisiteConfig
                    { PrerequisiteConfig.setupTracingEndpoint = Just "collector:4318/v1/traces",
                      PrerequisiteConfig.setupAutostartJaeger = True
                    }
              )
          )
          (\_ -> pure True)
          (\_ -> pure (Left (Prerequisite.InvalidTracingEndpointFormat "collector:4318/v1/traces")))
          (\_ _ -> expectationFailure "database autostart should not run" >> pure (DatabaseAutostart.DatabaseAutostartSkipped "database autostart should not run"))
          unusedTracingAutostart
          outputHandle
        hClose outputHandle
        readFile outputPath
          `shouldReturn` unlines
            [ "Setup: Database prerequisite reachable at 127.0.0.1:5432.",
              "Setup: Tracing prerequisite endpoint collector:4318/v1/traces is invalid: InvalidTracingEndpointFormat \"collector:4318/v1/traces\"."
            ]

    it "returns the rendered report after writing it to the supplied handle" $
      withSystemTempFile "setup-prerequisite-report.txt" $ \outputPath outputHandle -> do
        reportedPrerequisites <-
          PrerequisiteReport.reportSetupPrerequisitesWithResult
            (pure (Right PrerequisiteConfig.defaultSetupPrerequisiteConfig))
            (\_ -> pure False)
            (\_ -> pure (Right False))
            (\_ _ -> pure (DatabaseAutostart.DatabaseAutostartSucceeded PrerequisitePlan.PodmanRuntime))
            unusedTracingAutostart
            outputHandle
        hClose outputHandle
        reportedPrerequisites
          `shouldBe` Right
            PrerequisiteReport.SetupPrerequisiteReport
              { PrerequisiteReport.databasePrerequisiteStatus =
                  PrerequisiteReport.DatabasePrerequisiteAutostarted
                    PrerequisitePlan.DatabasePrerequisitePlan
                      { PrerequisitePlan.databaseCheckEndpoint =
                          Prerequisite.TcpEndpoint
                            { Prerequisite.tcpEndpointHost = "127.0.0.1",
                              Prerequisite.tcpEndpointPort = 5432
                            },
                        PrerequisitePlan.databaseAutostartPlan = Just PrerequisitePlan.defaultContainerAutostartPlan
                      }
                    PrerequisitePlan.PodmanRuntime,
                PrerequisiteReport.tracingPrerequisiteStatus = Nothing
              }
        readFile outputPath
          `shouldReturn` unlines
            [ "Setup: Database prerequisite unreachable at 127.0.0.1:5432. Configured autostart runtimes: podman, docker.",
              "Setup: Started local PostgreSQL container via podman."
            ]

  describe "checkSetupPrerequisites, reportSetupPrerequisitesAndReturn, and reportSetupPrerequisites" $ do
    it "uses the default loader and real reachability checks from the current directory" $
      withSystemTempDirectory "setup-prerequisite-real-check" $ \tempDirectory ->
        withListeningTcpEndpoint $ \tcpEndpoint -> do
          writeFile
            (tempDirectory <> "/.env")
            ( unlines
                [ "DATABASE_HOST=" <> Text.unpack (Prerequisite.tcpEndpointHost tcpEndpoint),
                  "DATABASE_PORT=" <> show (Prerequisite.tcpEndpointPort tcpEndpoint),
                  "OTLP_TRACING_ENDPOINT=http://"
                    <> Text.unpack (Prerequisite.tcpEndpointHost tcpEndpoint)
                    <> ":"
                    <> show (Prerequisite.tcpEndpointPort tcpEndpoint)
                    <> "/v1/traces"
                ]
            )
          withCurrentDirectory tempDirectory $
            PrerequisiteReport.checkSetupPrerequisites
              `shouldReturn` Right
                PrerequisiteReport.SetupPrerequisiteReport
                  { PrerequisiteReport.databasePrerequisiteStatus =
                      PrerequisiteReport.DatabasePrerequisiteReachable tcpEndpoint,
                    PrerequisiteReport.tracingPrerequisiteStatus =
                      Just
                        ( PrerequisiteReport.TracingPrerequisiteReachable
                            ( "http://"
                                <> Prerequisite.tcpEndpointHost tcpEndpoint
                                <> ":"
                                <> Text.pack (show (Prerequisite.tcpEndpointPort tcpEndpoint))
                                <> "/v1/traces"
                            )
                        )
                  }

    it "writes the default report to stdout" $
      withSystemTempDirectory "setup-prerequisite-stdout" $ \tempDirectory ->
        withListeningTcpEndpoint $ \tcpEndpoint -> do
          writeFile
            (tempDirectory <> "/.env")
            ( unlines
                [ "DATABASE_HOST=" <> Text.unpack (Prerequisite.tcpEndpointHost tcpEndpoint),
                  "DATABASE_PORT=" <> show (Prerequisite.tcpEndpointPort tcpEndpoint),
                  "OTLP_TRACING_ENDPOINT=http://"
                    <> Text.unpack (Prerequisite.tcpEndpointHost tcpEndpoint)
                    <> ":"
                    <> show (Prerequisite.tcpEndpointPort tcpEndpoint)
                    <> "/v1/traces"
                ]
            )
          output <-
            withCurrentDirectory tempDirectory $
              captureStdout PrerequisiteReport.reportSetupPrerequisites
          output
            `shouldBe` unlines
              [ "Setup: Database prerequisite reachable at "
                  <> Text.unpack (Prerequisite.tcpEndpointHost tcpEndpoint)
                  <> ":"
                  <> show (Prerequisite.tcpEndpointPort tcpEndpoint)
                  <> ".",
                "Setup: Tracing prerequisite reachable at http://"
                  <> Text.unpack (Prerequisite.tcpEndpointHost tcpEndpoint)
                  <> ":"
                  <> show (Prerequisite.tcpEndpointPort tcpEndpoint)
                  <> "/v1/traces."
              ]

    it "returns the default reported prerequisites after writing them to stdout" $
      withSystemTempDirectory "setup-prerequisite-stdout-return" $ \tempDirectory ->
        withListeningTcpEndpoint $ \tcpEndpoint -> do
          writeFile
            (tempDirectory <> "/.env")
            ( unlines
                [ "DATABASE_HOST=" <> Text.unpack (Prerequisite.tcpEndpointHost tcpEndpoint),
                  "DATABASE_PORT=" <> show (Prerequisite.tcpEndpointPort tcpEndpoint)
                ]
            )
          withCurrentDirectory tempDirectory $
            PrerequisiteReport.reportSetupPrerequisitesAndReturn
              `shouldReturn` Right
                PrerequisiteReport.SetupPrerequisiteReport
                  { PrerequisiteReport.databasePrerequisiteStatus =
                      PrerequisiteReport.DatabasePrerequisiteReachable tcpEndpoint,
                    PrerequisiteReport.tracingPrerequisiteStatus = Nothing
                  }

    it "uses the real database autostart runner when the default report finds an unreachable local database" $
      withSystemTempDirectory "setup-prerequisite-autostart" $ \tempDirectory ->
        withUnusedTcpEndpoint $ \tcpEndpoint ->
          withPathScripts
            [("podman", "#!/bin/sh\nexit 0\n")]
            $ do
              writeFile
                (tempDirectory <> "/.env")
                ( unlines
                    [ "DATABASE_HOST=" <> Text.unpack (Prerequisite.tcpEndpointHost tcpEndpoint),
                      "DATABASE_PORT=" <> show (Prerequisite.tcpEndpointPort tcpEndpoint)
                    ]
                )
              output <-
                withCurrentDirectory tempDirectory $
                  captureStdout PrerequisiteReport.reportSetupPrerequisites
              output
                `shouldBe` unlines
                  [ "Setup: Database prerequisite unreachable at "
                      <> Text.unpack (Prerequisite.tcpEndpointHost tcpEndpoint)
                      <> ":"
                      <> show (Prerequisite.tcpEndpointPort tcpEndpoint)
                      <> ". Configured autostart runtimes: podman, docker.",
                    "Setup: Started local PostgreSQL container via podman."
                  ]

    it "uses the real tracing autostart runner when the default report finds an unreachable local tracing endpoint" $
      withSystemTempDirectory "setup-prerequisite-tracing-autostart" $ \tempDirectory ->
        withListeningTcpEndpoint $ \databaseEndpoint ->
          withUnusedTcpEndpoint $ \tracingEndpoint ->
            withPathScripts
              [("podman", "#!/bin/sh\nexit 0\n")]
              $ do
                writeFile
                  (tempDirectory <> "/.env")
                  ( unlines
                      [ "DATABASE_HOST=" <> Text.unpack (Prerequisite.tcpEndpointHost databaseEndpoint),
                        "DATABASE_PORT=" <> show (Prerequisite.tcpEndpointPort databaseEndpoint),
                        "OTLP_TRACING_ENDPOINT=http://"
                          <> Text.unpack (Prerequisite.tcpEndpointHost tracingEndpoint)
                          <> ":"
                          <> show (Prerequisite.tcpEndpointPort tracingEndpoint)
                          <> "/v1/traces",
                        "SETUP_AUTOSTART_JAEGER=true"
                      ]
                  )
                output <-
                  withCurrentDirectory tempDirectory $
                    captureStdout PrerequisiteReport.reportSetupPrerequisites
                output
                  `shouldBe` unlines
                    [ "Setup: Database prerequisite reachable at "
                        <> Text.unpack (Prerequisite.tcpEndpointHost databaseEndpoint)
                        <> ":"
                        <> show (Prerequisite.tcpEndpointPort databaseEndpoint)
                        <> ".",
                      "Setup: Tracing prerequisite unreachable at http://"
                        <> Text.unpack (Prerequisite.tcpEndpointHost tracingEndpoint)
                        <> ":"
                        <> show (Prerequisite.tcpEndpointPort tracingEndpoint)
                        <> "/v1/traces. Configured autostart runtimes: podman, docker.",
                      "Setup: Started Jaeger container via podman."
                    ]

  describe "prerequisite report records" $
    it "keep selectors, equality, and rendering deterministic" $ do
      let databaseReachableStatus =
            PrerequisiteReport.DatabasePrerequisiteReachable
              Prerequisite.TcpEndpoint
                { Prerequisite.tcpEndpointHost = "127.0.0.1",
                  Prerequisite.tcpEndpointPort = 5432
                }
          databaseStatus =
            PrerequisiteReport.DatabasePrerequisiteUnreachable
              PrerequisitePlan.DatabasePrerequisitePlan
                { PrerequisitePlan.databaseCheckEndpoint =
                    Prerequisite.TcpEndpoint
                      { Prerequisite.tcpEndpointHost = "db.internal",
                        Prerequisite.tcpEndpointPort = 6543
                      },
                  PrerequisitePlan.databaseAutostartPlan = Just PrerequisitePlan.defaultContainerAutostartPlan
                }
          databaseAutostartedStatus =
            PrerequisiteReport.DatabasePrerequisiteAutostarted
              ( PrerequisitePlan.DatabasePrerequisitePlan
                  { PrerequisitePlan.databaseCheckEndpoint =
                      Prerequisite.TcpEndpoint
                        { Prerequisite.tcpEndpointHost = "db.internal",
                          Prerequisite.tcpEndpointPort = 6543
                        },
                    PrerequisitePlan.databaseAutostartPlan = Just PrerequisitePlan.defaultContainerAutostartPlan
                  }
              )
              PrerequisitePlan.PodmanRuntime
          databaseAutostartSkippedStatus =
            PrerequisiteReport.DatabasePrerequisiteAutostartSkipped
              ( PrerequisitePlan.DatabasePrerequisitePlan
                  { PrerequisitePlan.databaseCheckEndpoint =
                      Prerequisite.TcpEndpoint
                        { Prerequisite.tcpEndpointHost = "db.internal",
                          Prerequisite.tcpEndpointPort = 6543
                        },
                    PrerequisitePlan.databaseAutostartPlan = Just PrerequisitePlan.defaultContainerAutostartPlan
                  }
              )
              "automatic database autostart only supports DATABASE_HOST values 127.0.0.1 or 0.0.0.0, but got db.internal"
          databaseAutostartFailedStatus =
            PrerequisiteReport.DatabasePrerequisiteAutostartFailed
              ( PrerequisitePlan.DatabasePrerequisitePlan
                  { PrerequisitePlan.databaseCheckEndpoint =
                      Prerequisite.TcpEndpoint
                        { Prerequisite.tcpEndpointHost = "db.internal",
                          Prerequisite.tcpEndpointPort = 6543
                        },
                    PrerequisitePlan.databaseAutostartPlan = Just PrerequisitePlan.defaultContainerAutostartPlan
                  }
              )
              [ DatabaseAutostart.ContainerRuntimeFailure
                  { DatabaseAutostart.failedContainerRuntime = PrerequisitePlan.PodmanRuntime,
                    DatabaseAutostart.containerRuntimeFailureMessage = "podman missing"
                  }
              ]
          tracingReachableStatus =
            PrerequisiteReport.TracingPrerequisiteReachable
              "http://127.0.0.1:4318/v1/traces"
          tracingUnreachableStatus =
            PrerequisiteReport.TracingPrerequisiteUnreachable
              PrerequisitePlan.TracingPrerequisitePlan
                { PrerequisitePlan.tracingCheckEndpoint = "http://127.0.0.1:4318/v1/traces",
                  PrerequisitePlan.tracingAutostartPlan = Nothing
                }
          tracingAutostartedStatus =
            PrerequisiteReport.TracingPrerequisiteAutostarted
              ( PrerequisitePlan.TracingPrerequisitePlan
                  { PrerequisitePlan.tracingCheckEndpoint = "http://127.0.0.1:4318/v1/traces",
                    PrerequisitePlan.tracingAutostartPlan = Just PrerequisitePlan.defaultContainerAutostartPlan
                  }
              )
              PrerequisitePlan.PodmanRuntime
          tracingAutostartSkippedStatus =
            PrerequisiteReport.TracingPrerequisiteAutostartSkipped
              ( PrerequisitePlan.TracingPrerequisitePlan
                  { PrerequisitePlan.tracingCheckEndpoint = "http://127.0.0.1:4318/v1/traces",
                    PrerequisitePlan.tracingAutostartPlan = Just PrerequisitePlan.defaultContainerAutostartPlan
                  }
              )
              "unsupported host"
          tracingAutostartFailedStatus =
            PrerequisiteReport.TracingPrerequisiteAutostartFailed
              ( PrerequisitePlan.TracingPrerequisitePlan
                  { PrerequisitePlan.tracingCheckEndpoint = "http://127.0.0.1:4318/v1/traces",
                    PrerequisitePlan.tracingAutostartPlan = Just PrerequisitePlan.defaultContainerAutostartPlan
                  }
              )
              [ DatabaseAutostart.ContainerRuntimeFailure
                  { DatabaseAutostart.failedContainerRuntime = PrerequisitePlan.PodmanRuntime,
                    DatabaseAutostart.containerRuntimeFailureMessage = "podman missing"
                  }
              ]
          tracingStatus =
            PrerequisiteReport.TracingPrerequisiteInvalidEndpoint
              "collector:4318/v1/traces"
              (Prerequisite.InvalidTracingEndpointFormat "collector:4318/v1/traces")
          report =
            PrerequisiteReport.SetupPrerequisiteReport
              { PrerequisiteReport.databasePrerequisiteStatus = databaseStatus,
                PrerequisiteReport.tracingPrerequisiteStatus = Just tracingStatus
              }
      PrerequisiteReport.databasePrerequisiteStatus report `shouldBe` databaseStatus
      PrerequisiteReport.tracingPrerequisiteStatus report `shouldBe` Just tracingStatus
      tracingStatus
        `shouldNotBe` PrerequisiteReport.TracingPrerequisiteReachable "http://collector:4318/v1/traces"
      report
        `shouldNotBe` report
          { PrerequisiteReport.tracingPrerequisiteStatus = Nothing
          }
      databaseStatus
        `shouldNotBe` PrerequisiteReport.DatabasePrerequisiteReachable
          Prerequisite.TcpEndpoint
            { Prerequisite.tcpEndpointHost = "db.internal",
              Prerequisite.tcpEndpointPort = 6543
            }
      show databaseReachableStatus
        `shouldBe` "DatabasePrerequisiteReachable (TcpEndpoint {tcpEndpointHost = \"127.0.0.1\", tcpEndpointPort = 5432})"
      showsPrec 11 databaseReachableStatus ""
        `shouldBe` "(DatabasePrerequisiteReachable (TcpEndpoint {tcpEndpointHost = \"127.0.0.1\", tcpEndpointPort = 5432}))"
      show [databaseReachableStatus]
        `shouldBe` "[DatabasePrerequisiteReachable (TcpEndpoint {tcpEndpointHost = \"127.0.0.1\", tcpEndpointPort = 5432})]"
      showList [databaseReachableStatus] ""
        `shouldBe` "[DatabasePrerequisiteReachable (TcpEndpoint {tcpEndpointHost = \"127.0.0.1\", tcpEndpointPort = 5432})]"
      show databaseStatus
        `shouldBe` "DatabasePrerequisiteUnreachable (DatabasePrerequisitePlan {databaseCheckEndpoint = TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543}, databaseAutostartPlan = Just (ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]})})"
      showsPrec 11 databaseStatus ""
        `shouldBe` "(DatabasePrerequisiteUnreachable (DatabasePrerequisitePlan {databaseCheckEndpoint = TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543}, databaseAutostartPlan = Just (ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]})}))"
      show [databaseStatus]
        `shouldBe` "[DatabasePrerequisiteUnreachable (DatabasePrerequisitePlan {databaseCheckEndpoint = TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543}, databaseAutostartPlan = Just (ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]})})]"
      showList [databaseStatus] ""
        `shouldBe` "[DatabasePrerequisiteUnreachable (DatabasePrerequisitePlan {databaseCheckEndpoint = TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543}, databaseAutostartPlan = Just (ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]})})]"
      show databaseAutostartedStatus
        `shouldBe` "DatabasePrerequisiteAutostarted (DatabasePrerequisitePlan {databaseCheckEndpoint = TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543}, databaseAutostartPlan = Just (ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]})}) PodmanRuntime"
      showsPrec 11 databaseAutostartedStatus ""
        `shouldBe` "(DatabasePrerequisiteAutostarted (DatabasePrerequisitePlan {databaseCheckEndpoint = TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543}, databaseAutostartPlan = Just (ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]})}) PodmanRuntime)"
      show databaseAutostartSkippedStatus
        `shouldBe` "DatabasePrerequisiteAutostartSkipped (DatabasePrerequisitePlan {databaseCheckEndpoint = TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543}, databaseAutostartPlan = Just (ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]})}) \"automatic database autostart only supports DATABASE_HOST values 127.0.0.1 or 0.0.0.0, but got db.internal\""
      show databaseAutostartFailedStatus
        `shouldBe` "DatabasePrerequisiteAutostartFailed (DatabasePrerequisitePlan {databaseCheckEndpoint = TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543}, databaseAutostartPlan = Just (ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]})}) [ContainerRuntimeFailure {failedContainerRuntime = PodmanRuntime, containerRuntimeFailureMessage = \"podman missing\"}]"
      showsPrec 11 databaseAutostartFailedStatus ""
        `shouldBe` "(DatabasePrerequisiteAutostartFailed (DatabasePrerequisitePlan {databaseCheckEndpoint = TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543}, databaseAutostartPlan = Just (ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]})}) [ContainerRuntimeFailure {failedContainerRuntime = PodmanRuntime, containerRuntimeFailureMessage = \"podman missing\"}])"
      show tracingReachableStatus
        `shouldBe` "TracingPrerequisiteReachable \"http://127.0.0.1:4318/v1/traces\""
      showsPrec 11 tracingReachableStatus ""
        `shouldBe` "(TracingPrerequisiteReachable \"http://127.0.0.1:4318/v1/traces\")"
      show [tracingReachableStatus]
        `shouldBe` "[TracingPrerequisiteReachable \"http://127.0.0.1:4318/v1/traces\"]"
      showList [tracingReachableStatus] ""
        `shouldBe` "[TracingPrerequisiteReachable \"http://127.0.0.1:4318/v1/traces\"]"
      show tracingUnreachableStatus
        `shouldBe` "TracingPrerequisiteUnreachable (TracingPrerequisitePlan {tracingCheckEndpoint = \"http://127.0.0.1:4318/v1/traces\", tracingAutostartPlan = Nothing})"
      showsPrec 11 tracingUnreachableStatus ""
        `shouldBe` "(TracingPrerequisiteUnreachable (TracingPrerequisitePlan {tracingCheckEndpoint = \"http://127.0.0.1:4318/v1/traces\", tracingAutostartPlan = Nothing}))"
      show tracingAutostartedStatus
        `shouldBe` "TracingPrerequisiteAutostarted (TracingPrerequisitePlan {tracingCheckEndpoint = \"http://127.0.0.1:4318/v1/traces\", tracingAutostartPlan = Just (ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]})}) PodmanRuntime"
      show tracingAutostartSkippedStatus
        `shouldBe` "TracingPrerequisiteAutostartSkipped (TracingPrerequisitePlan {tracingCheckEndpoint = \"http://127.0.0.1:4318/v1/traces\", tracingAutostartPlan = Just (ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]})}) \"unsupported host\""
      show tracingAutostartFailedStatus
        `shouldBe` "TracingPrerequisiteAutostartFailed (TracingPrerequisitePlan {tracingCheckEndpoint = \"http://127.0.0.1:4318/v1/traces\", tracingAutostartPlan = Just (ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]})}) [ContainerRuntimeFailure {failedContainerRuntime = PodmanRuntime, containerRuntimeFailureMessage = \"podman missing\"}]"
      show [tracingUnreachableStatus]
        `shouldBe` "[TracingPrerequisiteUnreachable (TracingPrerequisitePlan {tracingCheckEndpoint = \"http://127.0.0.1:4318/v1/traces\", tracingAutostartPlan = Nothing})]"
      showList [tracingUnreachableStatus] ""
        `shouldBe` "[TracingPrerequisiteUnreachable (TracingPrerequisitePlan {tracingCheckEndpoint = \"http://127.0.0.1:4318/v1/traces\", tracingAutostartPlan = Nothing})]"
      show tracingStatus
        `shouldBe` "TracingPrerequisiteInvalidEndpoint \"collector:4318/v1/traces\" (InvalidTracingEndpointFormat \"collector:4318/v1/traces\")"
      showsPrec 11 tracingStatus ""
        `shouldBe` "(TracingPrerequisiteInvalidEndpoint \"collector:4318/v1/traces\" (InvalidTracingEndpointFormat \"collector:4318/v1/traces\"))"
      show [tracingStatus]
        `shouldBe` "[TracingPrerequisiteInvalidEndpoint \"collector:4318/v1/traces\" (InvalidTracingEndpointFormat \"collector:4318/v1/traces\")]"
      showList [tracingStatus] ""
        `shouldBe` "[TracingPrerequisiteInvalidEndpoint \"collector:4318/v1/traces\" (InvalidTracingEndpointFormat \"collector:4318/v1/traces\")]"
      show report
        `shouldBe` "SetupPrerequisiteReport {databasePrerequisiteStatus = DatabasePrerequisiteUnreachable (DatabasePrerequisitePlan {databaseCheckEndpoint = TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543}, databaseAutostartPlan = Just (ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]})}), tracingPrerequisiteStatus = Just (TracingPrerequisiteInvalidEndpoint \"collector:4318/v1/traces\" (InvalidTracingEndpointFormat \"collector:4318/v1/traces\"))}"
      show [report]
        `shouldBe` "[SetupPrerequisiteReport {databasePrerequisiteStatus = DatabasePrerequisiteUnreachable (DatabasePrerequisitePlan {databaseCheckEndpoint = TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543}, databaseAutostartPlan = Just (ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]})}), tracingPrerequisiteStatus = Just (TracingPrerequisiteInvalidEndpoint \"collector:4318/v1/traces\" (InvalidTracingEndpointFormat \"collector:4318/v1/traces\"))}]"
