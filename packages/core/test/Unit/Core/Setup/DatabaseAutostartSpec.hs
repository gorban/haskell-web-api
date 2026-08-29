{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Exception (finally)
import Control.Monad (forM_)
import Core.Setup.DatabaseAutostart qualified as DatabaseAutostart
import Core.Setup.Prerequisite qualified as Prerequisite
import Core.Setup.PrerequisiteConfig qualified as PrerequisiteConfig
import Core.Setup.PrerequisitePlan qualified as PrerequisitePlan
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as Text
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)
import Unit.Core.Setup.TestSupport (withEmptyPath)

withPathScripts :: [(FilePath, String)] -> IO a -> IO a
withPathScripts scripts action =
  withSystemTempDirectory "database-autostart-bin" $ \tempDirectory -> do
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

singleRuntimePlan :: PrerequisitePlan.ContainerRuntime -> PrerequisitePlan.DatabasePrerequisitePlan
singleRuntimePlan containerRuntime =
  PrerequisitePlan.DatabasePrerequisitePlan
    { PrerequisitePlan.databaseCheckEndpoint =
        PrerequisiteConfig.setupDatabaseEndpoint PrerequisiteConfig.defaultSetupPrerequisiteConfig,
      PrerequisitePlan.databaseAutostartPlan =
        Just
          PrerequisitePlan.ContainerAutostartPlan
            { PrerequisitePlan.autostartRuntimes = [containerRuntime]
            }
    }

spec = do
  describe "attemptDatabaseAutostartWith" $ do
    it "skips autostart when the setup plan leaves database autostart disabled" $
      DatabaseAutostart.attemptDatabaseAutostartWith
        (\_ _ -> expectationFailure "container runtime should not be called" >> pure (Right ()))
        PrerequisiteConfig.defaultSetupPrerequisiteConfig
        PrerequisitePlan.DatabasePrerequisitePlan
          { PrerequisitePlan.databaseCheckEndpoint =
              Prerequisite.TcpEndpoint
                { Prerequisite.tcpEndpointHost = "127.0.0.1",
                  Prerequisite.tcpEndpointPort = 5432
                },
            PrerequisitePlan.databaseAutostartPlan = Nothing
          }
        `shouldReturn` DatabaseAutostart.DatabaseAutostartSkipped
          "database autostart is disabled for this setup plan"

    it "tries podman first with the documented postgres container arguments and stops on success" $ do
      runtimeCalls <- newIORef []
      DatabaseAutostart.attemptDatabaseAutostartWith
        ( \runtime arguments -> do
            modifyIORef' runtimeCalls (++ [(runtime, arguments)])
            pure (Right ())
        )
        PrerequisiteConfig.defaultSetupPrerequisiteConfig
        PrerequisitePlan.DatabasePrerequisitePlan
          { PrerequisitePlan.databaseCheckEndpoint =
              PrerequisiteConfig.setupDatabaseEndpoint PrerequisiteConfig.defaultSetupPrerequisiteConfig,
            PrerequisitePlan.databaseAutostartPlan = Just PrerequisitePlan.defaultContainerAutostartPlan
          }
        `shouldReturn` DatabaseAutostart.DatabaseAutostartSucceeded PrerequisitePlan.PodmanRuntime
      readIORef runtimeCalls
        `shouldReturn` [ ( PrerequisitePlan.PodmanRuntime,
                           [ "run",
                             "--name",
                             "web-api-postgres",
                             "-e",
                             "POSTGRES_USER=web_api_owner",
                             "-e",
                             "POSTGRES_PASSWORD=web_api_owner",
                             "-e",
                             "POSTGRES_DB=web_api_dev",
                             "-p",
                             "127.0.0.1:5432:5432",
                             "-d",
                             "docker.io/library/postgres:17"
                           ]
                         )
                       ]

    it "falls back to docker when podman fails first" $ do
      runtimeCalls <- newIORef []
      DatabaseAutostart.attemptDatabaseAutostartWith
        ( \runtime arguments -> do
            modifyIORef' runtimeCalls (++ [(runtime, arguments)])
            pure $
              if runtime == PrerequisitePlan.PodmanRuntime
                then Left "podman missing"
                else Right ()
        )
        PrerequisiteConfig.defaultSetupPrerequisiteConfig
        PrerequisitePlan.DatabasePrerequisitePlan
          { PrerequisitePlan.databaseCheckEndpoint =
              PrerequisiteConfig.setupDatabaseEndpoint PrerequisiteConfig.defaultSetupPrerequisiteConfig,
            PrerequisitePlan.databaseAutostartPlan = Just PrerequisitePlan.defaultContainerAutostartPlan
          }
        `shouldReturn` DatabaseAutostart.DatabaseAutostartSucceeded PrerequisitePlan.DockerRuntime
      (map fst <$> readIORef runtimeCalls)
        `shouldReturn` [PrerequisitePlan.PodmanRuntime, PrerequisitePlan.DockerRuntime]

    it "reports every failed runtime when podman and docker both fail" $
      DatabaseAutostart.attemptDatabaseAutostartWith
        ( \runtime _ ->
            pure $
              Left $
                if runtime == PrerequisitePlan.PodmanRuntime
                  then "podman failed"
                  else "docker failed"
        )
        PrerequisiteConfig.defaultSetupPrerequisiteConfig
        PrerequisitePlan.DatabasePrerequisitePlan
          { PrerequisitePlan.databaseCheckEndpoint =
              PrerequisiteConfig.setupDatabaseEndpoint PrerequisiteConfig.defaultSetupPrerequisiteConfig,
            PrerequisitePlan.databaseAutostartPlan = Just PrerequisitePlan.defaultContainerAutostartPlan
          }
        `shouldReturn` DatabaseAutostart.DatabaseAutostartFailed
          [ DatabaseAutostart.ContainerRuntimeFailure
              { DatabaseAutostart.failedContainerRuntime = PrerequisitePlan.PodmanRuntime,
                DatabaseAutostart.containerRuntimeFailureMessage = "podman failed"
              },
            DatabaseAutostart.ContainerRuntimeFailure
              { DatabaseAutostart.failedContainerRuntime = PrerequisitePlan.DockerRuntime,
                DatabaseAutostart.containerRuntimeFailureMessage = "docker failed"
              }
          ]

    it "skips database autostart for non-local database hosts before attempting any runtime" $
      DatabaseAutostart.attemptDatabaseAutostartWith
        (\_ _ -> expectationFailure "container runtime should not be called" >> pure (Right ()))
        PrerequisiteConfig.defaultSetupPrerequisiteConfig
          { PrerequisiteConfig.setupDatabaseEndpoint =
              Prerequisite.TcpEndpoint
                { Prerequisite.tcpEndpointHost = "db.internal",
                  Prerequisite.tcpEndpointPort = 6543
                }
          }
        PrerequisitePlan.DatabasePrerequisitePlan
          { PrerequisitePlan.databaseCheckEndpoint =
              Prerequisite.TcpEndpoint
                { Prerequisite.tcpEndpointHost = "db.internal",
                  Prerequisite.tcpEndpointPort = 6543
                },
            PrerequisitePlan.databaseAutostartPlan = Just PrerequisitePlan.defaultContainerAutostartPlan
          }
        `shouldReturn` DatabaseAutostart.DatabaseAutostartSkipped
          "automatic database autostart only supports DATABASE_HOST values 127.0.0.1 or 0.0.0.0, but got db.internal"

  describe "attemptDatabaseAutostart" $ do
    it "uses the real container runner with podman from PATH and records the launch arguments" $
      withSystemTempDirectory "database-autostart-args" $ \tempDirectory -> do
        let argumentsPath = tempDirectory <> "/podman-args.txt"
        withPathScripts
          [ ( "podman",
              "#!/bin/sh\nprintf '%s\\n' \"$@\" > \"" <> argumentsPath <> "\"\nexit 0\n"
            )
          ]
          $ do
            DatabaseAutostart.attemptDatabaseAutostart
              PrerequisiteConfig.defaultSetupPrerequisiteConfig
              (singleRuntimePlan PrerequisitePlan.PodmanRuntime)
              `shouldReturn` DatabaseAutostart.DatabaseAutostartSucceeded PrerequisitePlan.PodmanRuntime
            readFile argumentsPath
              `shouldReturn` unlines
                [ "run",
                  "--name",
                  "web-api-postgres",
                  "-e",
                  "POSTGRES_USER=web_api_owner",
                  "-e",
                  "POSTGRES_PASSWORD=web_api_owner",
                  "-e",
                  "POSTGRES_DB=web_api_dev",
                  "-p",
                  "127.0.0.1:5432:5432",
                  "-d",
                  "docker.io/library/postgres:17"
                ]

    it "surfaces real runner stderr failures from docker"
      $ withPathScripts
        [ ("docker", "#!/bin/sh\nprintf 'docker stderr' >&2\nexit 7\n")
        ]
      $ do
        DatabaseAutostart.attemptDatabaseAutostart
          PrerequisiteConfig.defaultSetupPrerequisiteConfig
          (singleRuntimePlan PrerequisitePlan.DockerRuntime)
          `shouldReturn` DatabaseAutostart.DatabaseAutostartFailed
            [ DatabaseAutostart.ContainerRuntimeFailure
                { DatabaseAutostart.failedContainerRuntime = PrerequisitePlan.DockerRuntime,
                  DatabaseAutostart.containerRuntimeFailureMessage = "docker stderr"
                }
            ]

    it "uses the fallback exit-code message when the real runner exits silently"
      $ withPathScripts
        [ ("docker", "#!/bin/sh\nexit 7\n")
        ]
      $ do
        DatabaseAutostart.attemptDatabaseAutostart
          PrerequisiteConfig.defaultSetupPrerequisiteConfig
          (singleRuntimePlan PrerequisitePlan.DockerRuntime)
          `shouldReturn` DatabaseAutostart.DatabaseAutostartFailed
            [ DatabaseAutostart.ContainerRuntimeFailure
                { DatabaseAutostart.failedContainerRuntime = PrerequisitePlan.DockerRuntime,
                  DatabaseAutostart.containerRuntimeFailureMessage = "command failed with exit code 7"
                }
            ]

    it "surfaces missing runtime executables from the real runner explicitly" $
      withEmptyPath $
        do
          autostartResult <-
            DatabaseAutostart.attemptDatabaseAutostart
              PrerequisiteConfig.defaultSetupPrerequisiteConfig
              (singleRuntimePlan PrerequisitePlan.DockerRuntime)
          case autostartResult of
            DatabaseAutostart.DatabaseAutostartFailed
              [ DatabaseAutostart.ContainerRuntimeFailure
                  { DatabaseAutostart.failedContainerRuntime = PrerequisitePlan.DockerRuntime,
                    DatabaseAutostart.containerRuntimeFailureMessage = failureMessage
                  }
                ] -> do
                failureMessage `shouldSatisfy` (not . Text.null)
                failureMessage `shouldSatisfy` Text.isInfixOf "docker"
            _ ->
              expectationFailure ("unexpected autostart result: " <> show autostartResult)

  describe "database autostart records" $
    it "keep equality and rendering deterministic" $ do
      let failedRuntime =
            DatabaseAutostart.ContainerRuntimeFailure
              { DatabaseAutostart.failedContainerRuntime = PrerequisitePlan.PodmanRuntime,
                DatabaseAutostart.containerRuntimeFailureMessage = "podman failed"
              }
          skippedResult =
            DatabaseAutostart.DatabaseAutostartSkipped
              "automatic database autostart only supports DATABASE_HOST values 127.0.0.1 or 0.0.0.0, but got db.internal"
          succeededResult =
            DatabaseAutostart.DatabaseAutostartSucceeded PrerequisitePlan.DockerRuntime
          failedResult =
            DatabaseAutostart.DatabaseAutostartFailed [failedRuntime]
      expectAll
        ( (failedRuntime `shouldNotBe` failedRuntime {DatabaseAutostart.containerRuntimeFailureMessage = "docker failed"})
            :| [ skippedResult `shouldNotBe` succeededResult,
                 show failedRuntime `shouldBe` "ContainerRuntimeFailure {failedContainerRuntime = PodmanRuntime, containerRuntimeFailureMessage = \"podman failed\"}",
                 show skippedResult `shouldBe` "DatabaseAutostartSkipped \"automatic database autostart only supports DATABASE_HOST values 127.0.0.1 or 0.0.0.0, but got db.internal\"",
                 show succeededResult `shouldBe` "DatabaseAutostartSucceeded DockerRuntime",
                 show failedResult `shouldBe` "DatabaseAutostartFailed [ContainerRuntimeFailure {failedContainerRuntime = PodmanRuntime, containerRuntimeFailureMessage = \"podman failed\"}]",
                 show [failedRuntime] `shouldBe` "[ContainerRuntimeFailure {failedContainerRuntime = PodmanRuntime, containerRuntimeFailureMessage = \"podman failed\"}]",
                 show [skippedResult, succeededResult, failedResult] `shouldBe` "[DatabaseAutostartSkipped \"automatic database autostart only supports DATABASE_HOST values 127.0.0.1 or 0.0.0.0, but got db.internal\",DatabaseAutostartSucceeded DockerRuntime,DatabaseAutostartFailed [ContainerRuntimeFailure {failedContainerRuntime = PodmanRuntime, containerRuntimeFailureMessage = \"podman failed\"}]]"
               ]
        )
