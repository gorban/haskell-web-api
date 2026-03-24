module WebApi.SetupPlan
  ( AppPrerequisitePlan (..),
    ContainerAutostartPlan (..),
    ContainerRuntime (..),
    DatabasePrerequisitePlan (..),
    TcpEndpoint (..),
    TracingPrerequisitePlan (..),
    TracingEndpointParseError (..),
    checkTcpEndpointReachable,
    checkTcpEndpointReachableWithTimeout,
    checkTracingEndpointReachable,
    defaultContainerAutostartPlan,
    parseTracingEndpoint,
    planAppPrerequisites,
  )
where

import Core.Setup.Prerequisite
  ( TcpEndpoint (..),
    TracingEndpointParseError (..),
    checkTcpEndpointReachable,
    checkTcpEndpointReachableWithTimeout,
    checkTracingEndpointReachable,
    parseTracingEndpoint,
  )
import Data.Text (Text)
import WebApi.Config (AppConfig (..), AppEnvironmentConfig (..), DatabaseConfig (..), ObservabilityConfig (..), OtlpExporter (..))
import WebApi.SetupConfig (AppSetupConfig (..), SetupAutostartConfig (..))

data ContainerRuntime
  = PodmanRuntime
  | DockerRuntime
  deriving (Eq, Show)

newtype ContainerAutostartPlan = ContainerAutostartPlan
  { autostartRuntimes :: [ContainerRuntime]
  }
  deriving (Eq, Show)

data DatabasePrerequisitePlan = DatabasePrerequisitePlan
  { databaseCheckEndpoint :: TcpEndpoint,
    databaseAutostartPlan :: Maybe ContainerAutostartPlan
  }
  deriving (Eq, Show)

data TracingPrerequisitePlan = TracingPrerequisitePlan
  { tracingCheckEndpoint :: Text,
    tracingAutostartPlan :: Maybe ContainerAutostartPlan
  }
  deriving (Eq, Show)

data AppPrerequisitePlan = AppPrerequisitePlan
  { databasePrerequisitePlan :: DatabasePrerequisitePlan,
    tracingPrerequisitePlan :: Maybe TracingPrerequisitePlan
  }
  deriving (Eq, Show)

defaultContainerAutostartPlan :: ContainerAutostartPlan
defaultContainerAutostartPlan =
  ContainerAutostartPlan
    { autostartRuntimes = [PodmanRuntime, DockerRuntime]
    }

planAppPrerequisites :: AppSetupConfig -> AppPrerequisitePlan
planAppPrerequisites setupConfig =
  AppPrerequisitePlan
    { databasePrerequisitePlan =
        DatabasePrerequisitePlan
          { databaseCheckEndpoint =
              TcpEndpoint
                { tcpEndpointHost = databaseHost runtimeDatabaseConfig,
                  tcpEndpointPort = databasePort runtimeDatabaseConfig
                },
            databaseAutostartPlan =
              if setupAutostartDatabase autostartConfig
                then Just defaultContainerAutostartPlan
                else Nothing
          },
      tracingPrerequisitePlan =
        case tracingExporter (observability (setupAppConfig setupConfig)) of
          Nothing -> Nothing
          Just exporter ->
            Just
              TracingPrerequisitePlan
                { tracingCheckEndpoint = otlpEndpoint exporter,
                  tracingAutostartPlan =
                    if setupAutostartJaeger autostartConfig
                      then Just defaultContainerAutostartPlan
                      else Nothing
                }
    }
  where
    runtimeDatabaseConfig = databaseConfig (setupEnvironmentConfig setupConfig)
    autostartConfig = setupAutostartConfig setupConfig
