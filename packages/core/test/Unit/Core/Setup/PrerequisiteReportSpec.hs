{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Exception (finally)
import qualified Core.Config as CoreConfig
import qualified Core.Setup.Prerequisite as Prerequisite
import qualified Core.Setup.PrerequisiteConfig as PrerequisiteConfig
import qualified Core.Setup.PrerequisitePlan as PrerequisitePlan
import qualified Core.Setup.PrerequisiteReport as PrerequisiteReport
import qualified Data.Text as Text
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Network.Socket (Family (AF_INET), SockAddr (SockAddrInet), SocketType (Stream), bind, close, defaultProtocol, getSocketName, listen, socket, tupleToHostAddress)
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.IO (hClose, hFlush, stdout)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)

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
                    PrerequisitePlan.DatabasePrerequisitePlan
                      { PrerequisitePlan.databaseCheckEndpoint =
                          Prerequisite.TcpEndpoint
                            { Prerequisite.tcpEndpointHost = "db.internal",
                              Prerequisite.tcpEndpointPort = 6543
                            },
                        PrerequisitePlan.databaseAutostartPlan = Nothing
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
        )
        `shouldBe` [ "Setup: Database prerequisite unreachable at db.internal:6543.",
                     "Setup: Tracing prerequisite unreachable at http://collector:4318/v1/traces. Configured autostart runtimes: podman, docker."
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

  describe "reportSetupPrerequisitesWith" $
    it "writes the rendered report to the supplied handle" $
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
          outputHandle
        hClose outputHandle
        readFile outputPath
          `shouldReturn` unlines
            [ "Setup: Database prerequisite unreachable at 127.0.0.1:5432. Configured autostart runtimes: podman, docker.",
              "Setup: Tracing prerequisite reachable at http://collector:4318/v1/traces."
            ]

  describe "checkSetupPrerequisites and reportSetupPrerequisites" $ do
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
          tracingReachableStatus =
            PrerequisiteReport.TracingPrerequisiteReachable
              "http://127.0.0.1:4318/v1/traces"
          tracingUnreachableStatus =
            PrerequisiteReport.TracingPrerequisiteUnreachable
              PrerequisitePlan.TracingPrerequisitePlan
                { PrerequisitePlan.tracingCheckEndpoint = "http://127.0.0.1:4318/v1/traces",
                  PrerequisitePlan.tracingAutostartPlan = Nothing
                }
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
      databaseStatus `shouldBe` databaseStatus
      tracingStatus `shouldBe` tracingStatus
      report `shouldBe` report
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
