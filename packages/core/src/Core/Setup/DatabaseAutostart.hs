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
    attemptContainerAutostart,
    runContainerRuntimeCommand,
  )
import Core.Setup.Prerequisite (TcpEndpoint (..))
import Core.Setup.PrerequisiteConfig (SetupPrerequisiteConfig (..))
import Core.Setup.PrerequisitePlan
  ( ContainerRuntime (..),
    DatabasePrerequisitePlan (..),
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
  attemptContainerAutostart
    runCommand
    (databaseAutostartPlan databasePlan)
    "database autostart is disabled for this setup plan"
    (databaseAutostartArguments setupConfig)
    DatabaseAutostartSkipped
    DatabaseAutostartSucceeded
    DatabaseAutostartFailed

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
