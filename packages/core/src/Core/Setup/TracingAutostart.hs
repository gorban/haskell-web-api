{-# LANGUAGE OverloadedStrings #-}

module Core.Setup.TracingAutostart
  ( TracingAutostartResult (..),
    attemptTracingAutostart,
    attemptTracingAutostartWith,
  )
where

import Core.Setup.ContainerRuntime
  ( ContainerAutostartOutcomes (..),
    ContainerRuntimeFailure (..),
    attemptContainerAutostart,
    runContainerRuntimeCommand,
  )
import Core.Setup.Prerequisite
  ( TcpEndpoint (..),
    parseTracingEndpoint,
  )
import Core.Setup.PrerequisitePlan
  ( ContainerRuntime (..),
    TracingPrerequisitePlan (..),
  )
import Data.Text (Text)
import Data.Text qualified as Text

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
  attemptContainerAutostart
    runCommand
    (tracingAutostartPlan tracingPlan)
    "tracing autostart is disabled for this setup plan"
    (tracingAutostartArguments tracingPlan)
    ContainerAutostartOutcomes
      { containerAutostartSkipped = TracingAutostartSkipped,
        containerAutostartSucceeded = TracingAutostartSucceeded,
        containerAutostartFailed = TracingAutostartFailed
      }

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
