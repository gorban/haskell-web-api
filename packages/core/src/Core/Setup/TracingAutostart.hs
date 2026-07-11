{-# LANGUAGE OverloadedStrings #-}

module Core.Setup.TracingAutostart
  ( TracingAutostartResult (..),
    attemptTracingAutostart,
    attemptTracingAutostartWith,
  )
where

import Control.Exception (IOException, try)
import Core.Setup.DatabaseAutostart (ContainerRuntimeFailure (..))
import Core.Setup.Prerequisite
  ( TcpEndpoint (..),
    parseTracingEndpoint,
  )
import Core.Setup.PrerequisitePlan
  ( ContainerRuntime (..),
    TracingPrerequisitePlan (..),
    autostartRuntimes,
  )
import Data.Text (Text)
import Data.Text qualified as Text
import System.Exit (ExitCode (..))
import System.Process (proc, readCreateProcessWithExitCode)

data TracingAutostartResult
  = TracingAutostartSkipped Text
  | TracingAutostartSucceeded ContainerRuntime
  | TracingAutostartFailed [ContainerRuntimeFailure]
  deriving (Eq, Show)

attemptTracingAutostart ::
  TracingPrerequisitePlan ->
  IO TracingAutostartResult
attemptTracingAutostart =
  attemptTracingAutostartWith runContainerRuntimeCommand

attemptTracingAutostartWith ::
  (ContainerRuntime -> [String] -> IO (Either Text ())) ->
  TracingPrerequisitePlan ->
  IO TracingAutostartResult
attemptTracingAutostartWith runCommand tracingPlan =
  case tracingAutostartPlan tracingPlan of
    Nothing ->
      pure (TracingAutostartSkipped "tracing autostart is disabled for this setup plan")
    Just autostartPlan ->
      case tracingAutostartArguments tracingPlan of
        Left skipReason ->
          pure (TracingAutostartSkipped skipReason)
        Right commandArguments ->
          let tryRuntimes failures [] =
                pure (TracingAutostartFailed (reverse failures))
              tryRuntimes failures (runtime : remainingRuntimes) = do
                launchResult <- runCommand runtime commandArguments
                case launchResult of
                  Right () ->
                    pure (TracingAutostartSucceeded runtime)
                  Left failureMessage ->
                    tryRuntimes
                      (ContainerRuntimeFailure runtime failureMessage : failures)
                      remainingRuntimes
           in tryRuntimes [] (autostartRuntimes autostartPlan)

tracingAutostartArguments :: TracingPrerequisitePlan -> Either Text [String]
tracingAutostartArguments tracingPlan = do
  otlpEndpoint <- parseSupportedTracingEndpoint (tracingCheckEndpoint tracingPlan)
  let tracingHost = tcpEndpointHost otlpEndpoint
  let uiPortBinding = renderPortBinding tracingHost 16686 16686
      otlpPortBinding = renderPortBinding tracingHost (tcpEndpointPort otlpEndpoint) 4318
  pure
    [ "run",
      "--name",
      "web-api-jaeger",
      "-e",
      "COLLECTOR_OTLP_ENABLED=true",
      "-p",
      Text.unpack uiPortBinding,
      "-p",
      Text.unpack otlpPortBinding,
      "-d",
      "docker.io/jaegertracing/all-in-one"
    ]

parseSupportedTracingEndpoint :: Text -> Either Text TcpEndpoint
parseSupportedTracingEndpoint endpoint =
  if not (Text.isPrefixOf "http://" endpoint)
    then
      Left
        ( "automatic Jaeger autostart only supports http:// OTLP_TRACING_ENDPOINT values, but got "
            <> endpoint
        )
    else case parseTracingEndpoint endpoint of
      Left parseError ->
        Left
          ( "automatic Jaeger autostart requires a valid OTLP_TRACING_ENDPOINT, but got "
              <> endpoint
              <> ": "
              <> Text.pack (show parseError)
          )
      Right tcpEndpoint ->
        case tcpEndpointHost tcpEndpoint of
          "127.0.0.1" ->
            Right tcpEndpoint
          "0.0.0.0" ->
            Right tcpEndpoint
          unsupportedHost ->
            Left
              ( "automatic Jaeger autostart only supports OTLP_TRACING_ENDPOINT hosts 127.0.0.1 or 0.0.0.0, but got "
                  <> unsupportedHost
              )

renderPortBinding :: Text -> Int -> Int -> Text
renderPortBinding host hostPort containerPort =
  host
    <> ":"
    <> Text.pack (show hostPort)
    <> ":"
    <> Text.pack (show containerPort)

runContainerRuntimeCommand :: ContainerRuntime -> [String] -> IO (Either Text ())
runContainerRuntimeCommand runtime commandArguments = do
  let executable = renderContainerRuntimeExecutable runtime
  processResult <-
    try (readCreateProcessWithExitCode (proc executable commandArguments) "") ::
      IO (Either IOException (ExitCode, String, String))
  pure $
    case processResult of
      Left processError ->
        Left (Text.pack (show processError))
      Right (ExitSuccess, _, _) ->
        Right ()
      Right (ExitFailure exitCode, stdoutText, stderrText) ->
        Left (renderCommandFailure exitCode stdoutText stderrText)

renderContainerRuntimeExecutable :: ContainerRuntime -> String
renderContainerRuntimeExecutable containerRuntime =
  case containerRuntime of
    PodmanRuntime -> "podman"
    DockerRuntime -> "docker"

renderCommandFailure :: Int -> String -> String -> Text
renderCommandFailure exitCode stdoutText stderrText =
  let failureMessage = firstNonEmptyText [Text.pack stderrText, Text.pack stdoutText]
   in if Text.null failureMessage
        then "command failed with exit code " <> Text.pack (show exitCode)
        else failureMessage

firstNonEmptyText :: [Text] -> Text
firstNonEmptyText textValues =
  case filter (not . Text.null) (map Text.strip textValues) of
    message : _ -> message
    [] -> ""
