{-# LANGUAGE OverloadedStrings #-}

module Core.Setup.PrerequisiteReport
  ( DatabasePrerequisiteStatus (..),
    SetupPrerequisiteReport (..),
    TracingPrerequisiteStatus (..),
    checkSetupPrerequisites,
    checkSetupPrerequisitesWith,
    renderSetupPrerequisiteReport,
    reportSetupPrerequisites,
    reportSetupPrerequisitesWith,
  )
where

import Core.Setup.Prerequisite
  ( TcpEndpoint (..),
    TracingEndpointParseError,
    checkTcpEndpointReachable,
    checkTracingEndpointReachable,
  )
import Core.Setup.PrerequisiteConfig
  ( SetupPrerequisiteConfigLoadError,
    loadSetupPrerequisiteConfig,
  )
import Core.Setup.PrerequisiteConfig qualified as PrerequisiteConfig
import Core.Setup.PrerequisitePlan
  ( AppPrerequisitePlan (..),
    ContainerAutostartPlan (..),
    ContainerRuntime (..),
    DatabasePrerequisitePlan (..),
    TracingPrerequisitePlan (..),
    planSetupPrerequisites,
  )
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import System.IO (Handle, stdout)
import Text.Show (showListWith)

data DatabasePrerequisiteStatus
  = DatabasePrerequisiteReachable TcpEndpoint
  | DatabasePrerequisiteUnreachable DatabasePrerequisitePlan
  deriving (Eq)

data TracingPrerequisiteStatus
  = TracingPrerequisiteReachable Text
  | TracingPrerequisiteUnreachable TracingPrerequisitePlan
  | TracingPrerequisiteInvalidEndpoint Text TracingEndpointParseError
  deriving (Eq)

instance Show DatabasePrerequisiteStatus where
  showsPrec depth databaseStatus =
    showParen (depth > 10) $
      case databaseStatus of
        DatabasePrerequisiteReachable endpoint ->
          showString "DatabasePrerequisiteReachable "
            . showsPrec 11 endpoint
        DatabasePrerequisiteUnreachable databasePlan ->
          showString "DatabasePrerequisiteUnreachable "
            . showsPrec 11 databasePlan

  showList = showListWith shows

instance Show TracingPrerequisiteStatus where
  showsPrec depth tracingStatus =
    showParen (depth > 10) $
      case tracingStatus of
        TracingPrerequisiteReachable endpoint ->
          showString "TracingPrerequisiteReachable "
            . shows endpoint
        TracingPrerequisiteUnreachable tracingPlan ->
          showString "TracingPrerequisiteUnreachable "
            . showsPrec 11 tracingPlan
        TracingPrerequisiteInvalidEndpoint endpoint parseError ->
          showString "TracingPrerequisiteInvalidEndpoint "
            . shows endpoint
            . showChar ' '
            . showsPrec 11 parseError

data SetupPrerequisiteReport = SetupPrerequisiteReport
  { databasePrerequisiteStatus :: DatabasePrerequisiteStatus,
    tracingPrerequisiteStatus :: Maybe TracingPrerequisiteStatus
  }
  deriving (Eq, Show)

checkSetupPrerequisites ::
  IO (Either SetupPrerequisiteConfigLoadError SetupPrerequisiteReport)
checkSetupPrerequisites =
  checkSetupPrerequisitesWith
    loadSetupPrerequisiteConfig
    checkTcpEndpointReachable
    checkTracingEndpointReachable

checkSetupPrerequisitesWith ::
  IO (Either SetupPrerequisiteConfigLoadError PrerequisiteConfig.SetupPrerequisiteConfig) ->
  (TcpEndpoint -> IO Bool) ->
  (Text -> IO (Either TracingEndpointParseError Bool)) ->
  IO (Either SetupPrerequisiteConfigLoadError SetupPrerequisiteReport)
checkSetupPrerequisitesWith loadConfig checkDatabase checkTracing = do
  loadedConfig <- loadConfig
  case loadedConfig of
    Left loadError -> pure (Left loadError)
    Right setupConfig -> do
      let prerequisitePlan = planSetupPrerequisites setupConfig
          databasePlan = databasePrerequisitePlan prerequisitePlan
      databaseReachable <- checkDatabase (databaseCheckEndpoint databasePlan)
      tracingStatus <- traverse (checkTracingPlan checkTracing) (tracingPrerequisitePlan prerequisitePlan)
      pure $
        Right
          SetupPrerequisiteReport
            { databasePrerequisiteStatus =
                if databaseReachable
                  then DatabasePrerequisiteReachable (databaseCheckEndpoint databasePlan)
                  else DatabasePrerequisiteUnreachable databasePlan,
              tracingPrerequisiteStatus = tracingStatus
            }

checkTracingPlan ::
  (Text -> IO (Either TracingEndpointParseError Bool)) ->
  TracingPrerequisitePlan ->
  IO TracingPrerequisiteStatus
checkTracingPlan checkTracing tracingPlan = do
  tracingReachable <- checkTracing (tracingCheckEndpoint tracingPlan)
  pure $
    case tracingReachable of
      Left parseError ->
        TracingPrerequisiteInvalidEndpoint (tracingCheckEndpoint tracingPlan) parseError
      Right True ->
        TracingPrerequisiteReachable (tracingCheckEndpoint tracingPlan)
      Right False ->
        TracingPrerequisiteUnreachable tracingPlan

renderSetupPrerequisiteReport ::
  Either SetupPrerequisiteConfigLoadError SetupPrerequisiteReport ->
  [Text]
renderSetupPrerequisiteReport prerequisiteReport =
  case prerequisiteReport of
    Left loadError ->
      ["Setup: Failed to load prerequisite config: " <> Text.pack (show loadError)]
    Right report ->
      renderDatabaseStatus (databasePrerequisiteStatus report)
        <> maybe [] renderTracingStatus (tracingPrerequisiteStatus report)

renderDatabaseStatus :: DatabasePrerequisiteStatus -> [Text]
renderDatabaseStatus databaseStatus =
  case databaseStatus of
    DatabasePrerequisiteReachable endpoint ->
      ["Setup: Database prerequisite reachable at " <> renderTcpEndpoint endpoint <> "."]
    DatabasePrerequisiteUnreachable databasePlan ->
      [ "Setup: Database prerequisite unreachable at "
          <> renderTcpEndpoint (databaseCheckEndpoint databasePlan)
          <> renderAutostartSuffix (databaseAutostartPlan databasePlan)
      ]

renderTracingStatus :: TracingPrerequisiteStatus -> [Text]
renderTracingStatus tracingStatus =
  case tracingStatus of
    TracingPrerequisiteReachable endpoint ->
      ["Setup: Tracing prerequisite reachable at " <> endpoint <> "."]
    TracingPrerequisiteUnreachable tracingPlan ->
      [ "Setup: Tracing prerequisite unreachable at "
          <> tracingCheckEndpoint tracingPlan
          <> renderAutostartSuffix (tracingAutostartPlan tracingPlan)
      ]
    TracingPrerequisiteInvalidEndpoint endpoint parseError ->
      [ "Setup: Tracing prerequisite endpoint "
          <> endpoint
          <> " is invalid: "
          <> Text.pack (show parseError)
          <> "."
      ]

renderTcpEndpoint :: TcpEndpoint -> Text
renderTcpEndpoint endpoint =
  tcpEndpointHost endpoint <> ":" <> Text.pack (show (tcpEndpointPort endpoint))

renderAutostartSuffix :: Maybe ContainerAutostartPlan -> Text
renderAutostartSuffix maybeAutostartPlan =
  case maybeAutostartPlan of
    Nothing -> "."
    Just autostartPlan ->
      ". Configured autostart runtimes: "
        <> Text.intercalate ", " (map renderContainerRuntime (autostartRuntimes autostartPlan))
        <> "."

renderContainerRuntime :: ContainerRuntime -> Text
renderContainerRuntime containerRuntime =
  case containerRuntime of
    PodmanRuntime -> "podman"
    DockerRuntime -> "docker"

reportSetupPrerequisites :: IO ()
reportSetupPrerequisites =
  reportSetupPrerequisitesWith
    loadSetupPrerequisiteConfig
    checkTcpEndpointReachable
    checkTracingEndpointReachable
    stdout

reportSetupPrerequisitesWith ::
  IO (Either SetupPrerequisiteConfigLoadError PrerequisiteConfig.SetupPrerequisiteConfig) ->
  (TcpEndpoint -> IO Bool) ->
  (Text -> IO (Either TracingEndpointParseError Bool)) ->
  Handle ->
  IO ()
reportSetupPrerequisitesWith loadConfig checkDatabase checkTracing outputHandle = do
  prerequisiteReport <- checkSetupPrerequisitesWith loadConfig checkDatabase checkTracing
  mapM_ (TextIO.hPutStrLn outputHandle) (renderSetupPrerequisiteReport prerequisiteReport)
