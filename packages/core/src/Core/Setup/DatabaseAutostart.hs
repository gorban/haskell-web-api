{-# LANGUAGE OverloadedStrings #-}

module Core.Setup.DatabaseAutostart
  ( ContainerRuntimeFailure (..),
    DatabaseAutostartResult (..),
    attemptDatabaseAutostart,
    attemptDatabaseAutostartWith,
  )
where

import Control.Exception (IOException, try)
import Core.Setup.Prerequisite (TcpEndpoint (..))
import Core.Setup.PrerequisiteConfig (SetupPrerequisiteConfig (..))
import Core.Setup.PrerequisitePlan
  ( ContainerRuntime (..),
    DatabasePrerequisitePlan (..),
    autostartRuntimes,
  )
import Data.Text (Text)
import Data.Text qualified as Text
import System.Exit (ExitCode (..))
import System.Process (proc, readCreateProcessWithExitCode)

data ContainerRuntimeFailure = ContainerRuntimeFailure
  { failedContainerRuntime :: ContainerRuntime,
    containerRuntimeFailureMessage :: Text
  }
  deriving (Eq, Show)

data DatabaseAutostartResult
  = DatabaseAutostartSkipped Text
  | DatabaseAutostartSucceeded ContainerRuntime
  | DatabaseAutostartFailed [ContainerRuntimeFailure]
  deriving (Eq, Show)

attemptDatabaseAutostart ::
  SetupPrerequisiteConfig ->
  DatabasePrerequisitePlan ->
  IO DatabaseAutostartResult
attemptDatabaseAutostart =
  attemptDatabaseAutostartWith runContainerRuntimeCommand

attemptDatabaseAutostartWith ::
  (ContainerRuntime -> [String] -> IO (Either Text ())) ->
  SetupPrerequisiteConfig ->
  DatabasePrerequisitePlan ->
  IO DatabaseAutostartResult
attemptDatabaseAutostartWith runCommand setupConfig databasePlan =
  case databaseAutostartPlan databasePlan of
    Nothing ->
      pure (DatabaseAutostartSkipped "database autostart is disabled for this setup plan")
    Just autostartPlan ->
      case databaseAutostartArguments setupConfig of
        Left skipReason ->
          pure (DatabaseAutostartSkipped skipReason)
        Right commandArguments ->
          let tryRuntimes failures [] =
                pure (DatabaseAutostartFailed (reverse failures))
              tryRuntimes failures (runtime : remainingRuntimes) = do
                launchResult <- runCommand runtime commandArguments
                case launchResult of
                  Right () ->
                    pure (DatabaseAutostartSucceeded runtime)
                  Left failureMessage ->
                    tryRuntimes
                      (ContainerRuntimeFailure runtime failureMessage : failures)
                      remainingRuntimes
           in tryRuntimes [] (autostartRuntimes autostartPlan)

databaseAutostartArguments :: SetupPrerequisiteConfig -> Either Text [String]
databaseAutostartArguments setupConfig = do
  portBinding <- renderPortBinding (setupDatabaseEndpoint setupConfig)
  pure
    [ "run",
      "--name",
      "web-api-postgres",
      "-e",
      "POSTGRES_USER=" <> Text.unpack (setupDatabaseUser setupConfig),
      "-e",
      "POSTGRES_PASSWORD=" <> Text.unpack (setupDatabasePassword setupConfig),
      "-e",
      "POSTGRES_DB=" <> Text.unpack (setupDatabaseName setupConfig),
      "-p",
      Text.unpack portBinding,
      "-d",
      "docker.io/library/postgres:17"
    ]

renderPortBinding :: TcpEndpoint -> Either Text Text
renderPortBinding endpoint =
  if tcpEndpointHost endpoint `elem` ["127.0.0.1", "0.0.0.0"]
    then
      Right $
        tcpEndpointHost endpoint
          <> ":"
          <> Text.pack (show (tcpEndpointPort endpoint))
          <> ":5432"
    else
      Left $
        "automatic database autostart only supports DATABASE_HOST values 127.0.0.1 or 0.0.0.0, but got "
          <> tcpEndpointHost endpoint

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
