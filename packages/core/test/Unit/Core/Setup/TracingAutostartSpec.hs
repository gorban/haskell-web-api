{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Exception (finally)
import Control.Monad (forM_)
import Core.Setup.DatabaseAutostart qualified as DatabaseAutostart
import Core.Setup.PrerequisitePlan qualified as PrerequisitePlan
import Core.Setup.TracingAutostart qualified as TracingAutostart
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as Text
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)

withPathScripts :: [(FilePath, String)] -> IO a -> IO a
withPathScripts scripts action =
  withSystemTempDirectory "tracing-autostart-bin" $ \tempDirectory -> do
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

withIsolatedPathScripts :: [(FilePath, String)] -> IO a -> IO a
withIsolatedPathScripts scripts action =
  withSystemTempDirectory "tracing-autostart-bin" $ \tempDirectory -> do
    forM_ scripts $ \(scriptName, scriptBody) -> do
      let scriptPath = tempDirectory <> "/" <> scriptName
      writeFile scriptPath scriptBody
      callProcess "chmod" ["+x", scriptPath]
    originalPath <- lookupEnv "PATH"
    setEnv "PATH" tempDirectory
    action
      `finally` maybe
        (unsetEnv "PATH")
        (setEnv "PATH")
        originalPath

singleRuntimePlan :: PrerequisitePlan.ContainerRuntime -> PrerequisitePlan.TracingPrerequisitePlan
singleRuntimePlan containerRuntime =
  PrerequisitePlan.TracingPrerequisitePlan
    { PrerequisitePlan.tracingCheckEndpoint = "http://127.0.0.1:4318/v1/traces",
      PrerequisitePlan.tracingAutostartPlan =
        Just
          PrerequisitePlan.ContainerAutostartPlan
            { PrerequisitePlan.autostartRuntimes = [containerRuntime]
            }
    }

defaultTracingPlan :: PrerequisitePlan.TracingPrerequisitePlan
defaultTracingPlan =
  PrerequisitePlan.TracingPrerequisitePlan
    { PrerequisitePlan.tracingCheckEndpoint = "http://127.0.0.1:4318/v1/traces",
      PrerequisitePlan.tracingAutostartPlan = Just PrerequisitePlan.defaultContainerAutostartPlan
    }

spec = do
  describe "attemptTracingAutostartWith" $ do
    it "skips autostart when the setup plan leaves tracing autostart disabled" $
      TracingAutostart.attemptTracingAutostartWith
        (\_ _ -> expectationFailure "container runtime should not be called" >> pure (Right ()))
        PrerequisitePlan.TracingPrerequisitePlan
          { PrerequisitePlan.tracingCheckEndpoint = "http://127.0.0.1:4318/v1/traces",
            PrerequisitePlan.tracingAutostartPlan = Nothing
          }
        `shouldReturn` TracingAutostart.TracingAutostartSkipped
          "tracing autostart is disabled for this setup plan"

    it "tries podman first with the documented Jaeger container arguments and stops on success" $ do
      runtimeCalls <- newIORef []
      TracingAutostart.attemptTracingAutostartWith
        ( \runtime arguments -> do
            modifyIORef' runtimeCalls (++ [(runtime, arguments)])
            pure (Right ())
        )
        defaultTracingPlan
        `shouldReturn` TracingAutostart.TracingAutostartSucceeded PrerequisitePlan.PodmanRuntime
      readIORef runtimeCalls
        `shouldReturn` [ ( PrerequisitePlan.PodmanRuntime,
                           [ "run",
                             "--name",
                             "web-api-jaeger",
                             "-e",
                             "COLLECTOR_OTLP_ENABLED=true",
                             "-p",
                             "127.0.0.1:16686:16686",
                             "-p",
                             "127.0.0.1:4318:4318",
                             "-d",
                             "docker.io/jaegertracing/all-in-one"
                           ]
                         )
                       ]

    it "falls back to docker when podman fails first" $ do
      runtimeCalls <- newIORef []
      TracingAutostart.attemptTracingAutostartWith
        ( \runtime arguments -> do
            modifyIORef' runtimeCalls (++ [(runtime, arguments)])
            pure $
              if runtime == PrerequisitePlan.PodmanRuntime
                then Left "podman missing"
                else Right ()
        )
        defaultTracingPlan
        `shouldReturn` TracingAutostart.TracingAutostartSucceeded PrerequisitePlan.DockerRuntime
      (map fst <$> readIORef runtimeCalls)
        `shouldReturn` [PrerequisitePlan.PodmanRuntime, PrerequisitePlan.DockerRuntime]

    it "reports every failed runtime when podman and docker both fail" $
      TracingAutostart.attemptTracingAutostartWith
        ( \runtime _ ->
            pure $
              Left $
                if runtime == PrerequisitePlan.PodmanRuntime
                  then "podman failed"
                  else "docker failed"
        )
        defaultTracingPlan
        `shouldReturn` TracingAutostart.TracingAutostartFailed
          [ DatabaseAutostart.ContainerRuntimeFailure
              { DatabaseAutostart.failedContainerRuntime = PrerequisitePlan.PodmanRuntime,
                DatabaseAutostart.containerRuntimeFailureMessage = "podman failed"
              },
            DatabaseAutostart.ContainerRuntimeFailure
              { DatabaseAutostart.failedContainerRuntime = PrerequisitePlan.DockerRuntime,
                DatabaseAutostart.containerRuntimeFailureMessage = "docker failed"
              }
          ]

    it "supports 0.0.0.0 OTLP bindings when Jaeger autostart succeeds" $ do
      runtimeCalls <- newIORef []
      TracingAutostart.attemptTracingAutostartWith
        ( \runtime arguments -> do
            modifyIORef' runtimeCalls (++ [(runtime, arguments)])
            pure (Right ())
        )
        defaultTracingPlan
          { PrerequisitePlan.tracingCheckEndpoint = "http://0.0.0.0:55681/v1/traces"
          }
        `shouldReturn` TracingAutostart.TracingAutostartSucceeded PrerequisitePlan.PodmanRuntime
      readIORef runtimeCalls
        `shouldReturn` [ ( PrerequisitePlan.PodmanRuntime,
                           [ "run",
                             "--name",
                             "web-api-jaeger",
                             "-e",
                             "COLLECTOR_OTLP_ENABLED=true",
                             "-p",
                             "0.0.0.0:16686:16686",
                             "-p",
                             "0.0.0.0:55681:4318",
                             "-d",
                             "docker.io/jaegertracing/all-in-one"
                           ]
                         )
                       ]

    it "skips Jaeger autostart for malformed http OTLP endpoints before attempting any runtime" $
      TracingAutostart.attemptTracingAutostartWith
        (\_ _ -> expectationFailure "container runtime should not be called" >> pure (Right ()))
        defaultTracingPlan
          { PrerequisitePlan.tracingCheckEndpoint = "http://127.0.0.1:bad/v1/traces"
          }
        `shouldReturn` TracingAutostart.TracingAutostartSkipped
          "automatic Jaeger autostart requires a valid OTLP_TRACING_ENDPOINT, but got http://127.0.0.1:bad/v1/traces: InvalidTracingEndpointPort \"bad\""

    it "skips Jaeger autostart for https OTLP endpoints before attempting any runtime" $
      TracingAutostart.attemptTracingAutostartWith
        (\_ _ -> expectationFailure "container runtime should not be called" >> pure (Right ()))
        defaultTracingPlan
          { PrerequisitePlan.tracingCheckEndpoint = "https://127.0.0.1:4318/v1/traces"
          }
        `shouldReturn` TracingAutostart.TracingAutostartSkipped
          "automatic Jaeger autostart only supports http:// OTLP_TRACING_ENDPOINT values, but got https://127.0.0.1:4318/v1/traces"

    it "skips Jaeger autostart for non-local tracing hosts before attempting any runtime" $
      TracingAutostart.attemptTracingAutostartWith
        (\_ _ -> expectationFailure "container runtime should not be called" >> pure (Right ()))
        defaultTracingPlan
          { PrerequisitePlan.tracingCheckEndpoint = "http://collector.internal:4318/v1/traces"
          }
        `shouldReturn` TracingAutostart.TracingAutostartSkipped
          "automatic Jaeger autostart only supports OTLP_TRACING_ENDPOINT hosts 127.0.0.1 or 0.0.0.0, but got collector.internal"

  describe "attemptTracingAutostart" $ do
    it "uses the real container runner with podman from PATH and records the launch arguments" $
      withSystemTempDirectory "tracing-autostart-args" $ \tempDirectory -> do
        let argumentsPath = tempDirectory <> "/podman-args.txt"
        withPathScripts
          [ ( "podman",
              "#!/bin/sh\nprintf '%s\\n' \"$@\" > \"" <> argumentsPath <> "\"\nexit 0\n"
            )
          ]
          $ do
            TracingAutostart.attemptTracingAutostart
              (singleRuntimePlan PrerequisitePlan.PodmanRuntime)
              `shouldReturn` TracingAutostart.TracingAutostartSucceeded PrerequisitePlan.PodmanRuntime
            readFile argumentsPath
              `shouldReturn` unlines
                [ "run",
                  "--name",
                  "web-api-jaeger",
                  "-e",
                  "COLLECTOR_OTLP_ENABLED=true",
                  "-p",
                  "127.0.0.1:16686:16686",
                  "-p",
                  "127.0.0.1:4318:4318",
                  "-d",
                  "docker.io/jaegertracing/all-in-one"
                ]

    it "surfaces real runner stderr failures from docker"
      $ withPathScripts
        [("docker", "#!/bin/sh\nprintf 'docker stderr' >&2\nexit 7\n")]
      $ do
        TracingAutostart.attemptTracingAutostart
          (singleRuntimePlan PrerequisitePlan.DockerRuntime)
          `shouldReturn` TracingAutostart.TracingAutostartFailed
            [ DatabaseAutostart.ContainerRuntimeFailure
                { DatabaseAutostart.failedContainerRuntime = PrerequisitePlan.DockerRuntime,
                  DatabaseAutostart.containerRuntimeFailureMessage = "docker stderr"
                }
            ]

    it "uses the fallback exit-code message when the real runner exits silently"
      $ withPathScripts
        [("docker", "#!/bin/sh\nexit 7\n")]
      $ do
        TracingAutostart.attemptTracingAutostart
          (singleRuntimePlan PrerequisitePlan.DockerRuntime)
          `shouldReturn` TracingAutostart.TracingAutostartFailed
            [ DatabaseAutostart.ContainerRuntimeFailure
                { DatabaseAutostart.failedContainerRuntime = PrerequisitePlan.DockerRuntime,
                  DatabaseAutostart.containerRuntimeFailureMessage = "command failed with exit code 7"
                }
            ]

    it "surfaces missing runtime executables from the real runner explicitly" $
      withIsolatedPathScripts [] $
        do
          autostartResult <-
            TracingAutostart.attemptTracingAutostart
              (singleRuntimePlan PrerequisitePlan.DockerRuntime)
          case autostartResult of
            TracingAutostart.TracingAutostartFailed
              [ DatabaseAutostart.ContainerRuntimeFailure
                  { DatabaseAutostart.failedContainerRuntime = PrerequisitePlan.DockerRuntime,
                    DatabaseAutostart.containerRuntimeFailureMessage = failureMessage
                  }
                ] -> do
                failureMessage `shouldSatisfy` (not . Text.null)
                failureMessage `shouldSatisfy` Text.isInfixOf "docker"
            _ ->
              expectationFailure ("unexpected autostart result: " <> show autostartResult)

  describe "tracing autostart records" $
    it "keep equality and rendering deterministic" $ do
      let failedRuntime =
            DatabaseAutostart.ContainerRuntimeFailure
              { DatabaseAutostart.failedContainerRuntime = PrerequisitePlan.PodmanRuntime,
                DatabaseAutostart.containerRuntimeFailureMessage = "podman failed"
              }
          skippedResult =
            TracingAutostart.TracingAutostartSkipped
              "automatic Jaeger autostart only supports OTLP_TRACING_ENDPOINT hosts 127.0.0.1 or 0.0.0.0, but got collector.internal"
          succeededResult =
            TracingAutostart.TracingAutostartSucceeded PrerequisitePlan.DockerRuntime
          failedResult =
            TracingAutostart.TracingAutostartFailed [failedRuntime]
      expectAll
        ( (failedRuntime `shouldBe` failedRuntime)
            :| [ failedRuntime `shouldNotBe` failedRuntime {DatabaseAutostart.containerRuntimeFailureMessage = "docker failed"},
                 skippedResult `shouldBe` skippedResult,
                 skippedResult `shouldNotBe` succeededResult,
                 succeededResult `shouldBe` succeededResult,
                 failedResult `shouldBe` failedResult,
                 show skippedResult `shouldBe` "TracingAutostartSkipped \"automatic Jaeger autostart only supports OTLP_TRACING_ENDPOINT hosts 127.0.0.1 or 0.0.0.0, but got collector.internal\"",
                 show succeededResult `shouldBe` "TracingAutostartSucceeded DockerRuntime",
                 show failedResult `shouldBe` "TracingAutostartFailed [ContainerRuntimeFailure {failedContainerRuntime = PodmanRuntime, containerRuntimeFailureMessage = \"podman failed\"}]",
                 show [skippedResult, succeededResult, failedResult] `shouldBe` "[TracingAutostartSkipped \"automatic Jaeger autostart only supports OTLP_TRACING_ENDPOINT hosts 127.0.0.1 or 0.0.0.0, but got collector.internal\",TracingAutostartSucceeded DockerRuntime,TracingAutostartFailed [ContainerRuntimeFailure {failedContainerRuntime = PodmanRuntime, containerRuntimeFailureMessage = \"podman failed\"}]]"
               ]
        )
