{-# LANGUAGE OverloadedStrings #-}

module Core.Setup.DatabaseAutostart
  ( ContainerRuntimeFailure (..),
    DatabaseAutostartResult (..),
    attemptDatabaseAutostart,
    attemptDatabaseAutostartWith,
  )
where

import Core.Setup.ContainerRuntime
  ( ContainerRuntimeFailure (..),
    runContainerRuntimeCommand,
    tryContainerRuntimes,
  )
import Core.Setup.Prerequisite (TcpEndpoint (..))
import Core.Setup.PrerequisiteConfig (SetupPrerequisiteConfig (..))
import Core.Setup.PrerequisitePlan
  ( ContainerRuntime (..),
    DatabasePrerequisitePlan (..),
    autostartRuntimes,
  )
import Data.Text (Text)
import Data.Text qualified as Text

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
          either DatabaseAutostartFailed DatabaseAutostartSucceeded
            <$> tryContainerRuntimes
              (autostartRuntimes autostartPlan)
              (`runCommand` commandArguments)

databaseAutostartArguments :: SetupPrerequisiteConfig -> Either Text [String]
databaseAutostartArguments setupConfig = do
  portBinding <- renderPortBinding (setupDatabaseEndpoint setupConfig)
  pure
    [ "run",
      "--name",
      "web-api-postgres",
      "-e",
      "POSTGRES_USER=web_api_owner",
      "-e",
      "POSTGRES_PASSWORD=web_api_owner",
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
