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
import Core.Setup.PrerequisiteConfig qualified as PrerequisiteConfig
import Core.Setup.PrerequisitePlan
  ( AppPrerequisitePlan (..),
    ContainerAutostartPlan (..),
    ContainerRuntime (..),
    DatabasePrerequisitePlan (..),
    TracingPrerequisitePlan (..),
    defaultContainerAutostartPlan,
    planSetupPrerequisites,
  )
import WebApi.Config (AppConfig (..), AppEnvironmentConfig (..), DatabaseConfig (..), ObservabilityConfig (..), OtlpExporter (..))
import WebApi.SetupConfig (AppSetupConfig (..), SetupAutostartConfig (..))

planAppPrerequisites :: AppSetupConfig -> AppPrerequisitePlan
planAppPrerequisites setupConfig =
  planSetupPrerequisites $
    PrerequisiteConfig.SetupPrerequisiteConfig
      { PrerequisiteConfig.setupDatabaseEndpoint =
          TcpEndpoint
            { tcpEndpointHost = databaseHost runtimeDatabaseConfig,
              tcpEndpointPort = databasePort runtimeDatabaseConfig
            },
        PrerequisiteConfig.setupTracingEndpoint =
          otlpEndpoint <$> tracingExporter (observability (setupAppConfig setupConfig)),
        PrerequisiteConfig.setupAutostartDatabase = setupAutostartDatabase autostartConfig,
        PrerequisiteConfig.setupAutostartJaeger = setupAutostartJaeger autostartConfig
      }
  where
    runtimeDatabaseConfig = databaseConfig (setupEnvironmentConfig setupConfig)
    autostartConfig = setupAutostartConfig setupConfig
