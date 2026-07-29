{-# LANGUAGE OverloadedStrings #-}

module Core.Setup.PrerequisiteReport
  ( DatabasePrerequisiteStatus (..),
    SetupPrerequisiteReport (..),
    TracingPrerequisiteStatus (..),
    checkSetupPrerequisites,
    checkSetupPrerequisitesWith,
    renderSetupPrerequisiteReport,
    reportSetupPrerequisitesAndReturn,
    reportSetupPrerequisites,
    reportSetupPrerequisitesWithResult,
    reportSetupPrerequisitesWith,
  )
where

import Control.Monad (void)
import Core.Setup.DatabaseAutostart
  ( ContainerRuntimeFailure (..),
    DatabaseAutostartResult (..),
    attemptDatabaseAutostart,
  )
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
import Core.Setup.TracingAutostart
  ( TracingAutostartResult (..),
    attemptTracingAutostart,
  )
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import System.IO (Handle, stdout)
import Text.Show (showListWith)

data DatabasePrerequisiteStatus
  = DatabasePrerequisiteReachable TcpEndpoint
  | DatabasePrerequisiteUnreachable DatabasePrerequisitePlan
  | DatabasePrerequisiteAutostarted DatabasePrerequisitePlan ContainerRuntime
  | DatabasePrerequisiteAutostartSkipped DatabasePrerequisitePlan Text
  | DatabasePrerequisiteAutostartFailed DatabasePrerequisitePlan [ContainerRuntimeFailure]
  deriving (Eq)

data TracingPrerequisiteStatus
  = TracingPrerequisiteReachable Text
  | TracingPrerequisiteUnreachable TracingPrerequisitePlan
  | TracingPrerequisiteAutostarted TracingPrerequisitePlan ContainerRuntime
  | TracingPrerequisiteAutostartSkipped TracingPrerequisitePlan Text
  | TracingPrerequisiteAutostartFailed TracingPrerequisitePlan [ContainerRuntimeFailure]
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
        DatabasePrerequisiteAutostarted databasePlan containerRuntime ->
          showString "DatabasePrerequisiteAutostarted "
            . showsPrec 11 databasePlan
            . showChar ' '
            . shows containerRuntime
        DatabasePrerequisiteAutostartSkipped databasePlan reason ->
          showString "DatabasePrerequisiteAutostartSkipped "
            . showsPrec 11 databasePlan
            . showChar ' '
            . shows reason
        DatabasePrerequisiteAutostartFailed databasePlan runtimeFailures ->
          showString "DatabasePrerequisiteAutostartFailed "
            . showsPrec 11 databasePlan
            . showChar ' '
            . shows runtimeFailures

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
        TracingPrerequisiteAutostarted tracingPlan containerRuntime ->
          showString "TracingPrerequisiteAutostarted "
            . showsPrec 11 tracingPlan
            . showChar ' '
            . shows containerRuntime
        TracingPrerequisiteAutostartSkipped tracingPlan reason ->
          showString "TracingPrerequisiteAutostartSkipped "
            . showsPrec 11 tracingPlan
            . showChar ' '
            . shows reason
        TracingPrerequisiteAutostartFailed tracingPlan runtimeFailures ->
          showString "TracingPrerequisiteAutostartFailed "
            . showsPrec 11 tracingPlan
            . showChar ' '
            . shows runtimeFailures
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
checkSetupPrerequisitesWith loadConfig checkDatabase checkTracing =
  loadConfig >>= either (pure . Left) (checkLoadedSetupConfig checkDatabase checkTracing)

checkLoadedSetupConfig ::
  (TcpEndpoint -> IO Bool) ->
  (Text -> IO (Either TracingEndpointParseError Bool)) ->
  PrerequisiteConfig.SetupPrerequisiteConfig ->
  IO (Either SetupPrerequisiteConfigLoadError SetupPrerequisiteReport)
checkLoadedSetupConfig checkDatabase checkTracing setupConfig = do
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
    DatabasePrerequisiteAutostarted databasePlan containerRuntime ->
      [ "Setup: Database prerequisite unreachable at "
          <> renderTcpEndpoint (databaseCheckEndpoint databasePlan)
          <> renderAutostartSuffix (databaseAutostartPlan databasePlan),
        "Setup: Started local PostgreSQL container via "
          <> renderContainerRuntime containerRuntime
          <> "."
      ]
    DatabasePrerequisiteAutostartSkipped databasePlan reason ->
      [ "Setup: Database prerequisite unreachable at "
          <> renderTcpEndpoint (databaseCheckEndpoint databasePlan)
          <> renderAutostartSuffix (databaseAutostartPlan databasePlan),
        "Setup: Skipping database autostart: " <> reason <> "."
      ]
    DatabasePrerequisiteAutostartFailed databasePlan runtimeFailures ->
      [ "Setup: Database prerequisite unreachable at "
          <> renderTcpEndpoint (databaseCheckEndpoint databasePlan)
          <> renderAutostartSuffix (databaseAutostartPlan databasePlan)
      ]
        <> map (renderContainerRuntimeFailure "Database") runtimeFailures
        <> ["Setup: Continuing without database autostart."]

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
    TracingPrerequisiteAutostarted tracingPlan containerRuntime ->
      [ "Setup: Tracing prerequisite unreachable at "
          <> tracingCheckEndpoint tracingPlan
          <> renderAutostartSuffix (tracingAutostartPlan tracingPlan),
        "Setup: Started Jaeger container via "
          <> renderContainerRuntime containerRuntime
          <> "."
      ]
    TracingPrerequisiteAutostartSkipped tracingPlan reason ->
      [ "Setup: Tracing prerequisite unreachable at "
          <> tracingCheckEndpoint tracingPlan
          <> renderAutostartSuffix (tracingAutostartPlan tracingPlan),
        "Setup: Skipping tracing autostart: " <> reason <> "."
      ]
    TracingPrerequisiteAutostartFailed tracingPlan runtimeFailures ->
      [ "Setup: Tracing prerequisite unreachable at "
          <> tracingCheckEndpoint tracingPlan
          <> renderAutostartSuffix (tracingAutostartPlan tracingPlan)
      ]
        <> map (renderContainerRuntimeFailure "Tracing") runtimeFailures
        <> ["Setup: Continuing without tracing autostart."]
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

renderContainerRuntimeFailure :: Text -> ContainerRuntimeFailure -> Text
renderContainerRuntimeFailure subject runtimeFailure =
  "Setup: "
    <> subject
    <> " autostart via "
    <> renderContainerRuntime (failedContainerRuntime runtimeFailure)
    <> " failed: "
    <> containerRuntimeFailureMessage runtimeFailure
    <> "."

reportSetupPrerequisitesAndReturn :: IO (Either SetupPrerequisiteConfigLoadError SetupPrerequisiteReport)
reportSetupPrerequisitesAndReturn =
  reportSetupPrerequisitesWithResult
    loadSetupPrerequisiteConfig
    checkTcpEndpointReachable
    checkTracingEndpointReachable
    attemptDatabaseAutostart
    attemptTracingAutostart
    stdout

reportSetupPrerequisites :: IO ()
reportSetupPrerequisites =
  void reportSetupPrerequisitesAndReturn

reportSetupPrerequisitesWithResult ::
  IO (Either SetupPrerequisiteConfigLoadError PrerequisiteConfig.SetupPrerequisiteConfig) ->
  (TcpEndpoint -> IO Bool) ->
  (Text -> IO (Either TracingEndpointParseError Bool)) ->
  (PrerequisiteConfig.SetupPrerequisiteConfig -> DatabasePrerequisitePlan -> IO DatabaseAutostartResult) ->
  (TracingPrerequisitePlan -> IO TracingAutostartResult) ->
  Handle ->
  IO (Either SetupPrerequisiteConfigLoadError SetupPrerequisiteReport)
reportSetupPrerequisitesWithResult loadConfig checkDatabase checkTracing attemptDatabase attemptTracing outputHandle = do
  loadedConfig <- loadConfig
  prerequisiteReport <-
    case loadedConfig of
      Left loadError ->
        pure (Left loadError)
      Right setupConfig -> do
        Right report <- checkSetupPrerequisitesWith (pure (Right setupConfig)) checkDatabase checkTracing
        reportWithDatabase <- applyDatabaseAutostart attemptDatabase setupConfig report
        Right <$> applyTracingAutostart attemptTracing reportWithDatabase
  mapM_ (TextIO.hPutStrLn outputHandle) (renderSetupPrerequisiteReport prerequisiteReport)
  pure prerequisiteReport

reportSetupPrerequisitesWith ::
  IO (Either SetupPrerequisiteConfigLoadError PrerequisiteConfig.SetupPrerequisiteConfig) ->
  (TcpEndpoint -> IO Bool) ->
  (Text -> IO (Either TracingEndpointParseError Bool)) ->
  (PrerequisiteConfig.SetupPrerequisiteConfig -> DatabasePrerequisitePlan -> IO DatabaseAutostartResult) ->
  (TracingPrerequisitePlan -> IO TracingAutostartResult) ->
  Handle ->
  IO ()
reportSetupPrerequisitesWith loadConfig checkDatabase checkTracing attemptDatabase attemptTracing outputHandle =
  void (reportSetupPrerequisitesWithResult loadConfig checkDatabase checkTracing attemptDatabase attemptTracing outputHandle)

applyDatabaseAutostart ::
  (PrerequisiteConfig.SetupPrerequisiteConfig -> DatabasePrerequisitePlan -> IO DatabaseAutostartResult) ->
  PrerequisiteConfig.SetupPrerequisiteConfig ->
  SetupPrerequisiteReport ->
  IO SetupPrerequisiteReport
applyDatabaseAutostart attemptDatabase setupConfig prerequisiteReport =
  case databasePrerequisiteStatus prerequisiteReport of
    DatabasePrerequisiteUnreachable databasePlan ->
      case databaseAutostartPlan databasePlan of
        Nothing ->
          pure prerequisiteReport
        Just _ -> do
          autostartResult <- attemptDatabase setupConfig databasePlan
          pure
            prerequisiteReport
              { databasePrerequisiteStatus =
                  case autostartResult of
                    DatabaseAutostartSkipped reason ->
                      DatabasePrerequisiteAutostartSkipped databasePlan reason
                    DatabaseAutostartSucceeded containerRuntime ->
                      DatabasePrerequisiteAutostarted databasePlan containerRuntime
                    DatabaseAutostartFailed runtimeFailures ->
                      DatabasePrerequisiteAutostartFailed databasePlan runtimeFailures
              }
    _ ->
      pure prerequisiteReport

applyTracingAutostart ::
  (TracingPrerequisitePlan -> IO TracingAutostartResult) ->
  SetupPrerequisiteReport ->
  IO SetupPrerequisiteReport
applyTracingAutostart attemptTracing prerequisiteReport =
  case tracingPrerequisiteStatus prerequisiteReport of
    Just (TracingPrerequisiteUnreachable tracingPlan) ->
      case tracingAutostartPlan tracingPlan of
        Nothing ->
          pure prerequisiteReport
        Just _ -> do
          autostartResult <- attemptTracing tracingPlan
          pure
            prerequisiteReport
              { tracingPrerequisiteStatus =
                  Just $
                    case autostartResult of
                      TracingAutostartSkipped reason ->
                        TracingPrerequisiteAutostartSkipped tracingPlan reason
                      TracingAutostartSucceeded containerRuntime ->
                        TracingPrerequisiteAutostarted tracingPlan containerRuntime
                      TracingAutostartFailed runtimeFailures ->
                        TracingPrerequisiteAutostartFailed tracingPlan runtimeFailures
              }
    _ ->
      pure prerequisiteReport
