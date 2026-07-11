module Core.Setup.PrerequisitePlan
  ( AppPrerequisitePlan (..),
    ContainerAutostartPlan (..),
    ContainerRuntime (..),
    DatabasePrerequisitePlan (..),
    TracingPrerequisitePlan (..),
    defaultContainerAutostartPlan,
    planSetupPrerequisites,
  )
where

import Core.Setup.Prerequisite (TcpEndpoint)
import Core.Setup.PrerequisiteConfig (SetupPrerequisiteConfig (..))
import Data.Text (Text)

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

planSetupPrerequisites :: SetupPrerequisiteConfig -> AppPrerequisitePlan
planSetupPrerequisites setupConfig =
  AppPrerequisitePlan
    { databasePrerequisitePlan =
        DatabasePrerequisitePlan
          { databaseCheckEndpoint = setupDatabaseEndpoint setupConfig,
            databaseAutostartPlan =
              if setupAutostartDatabase setupConfig
                then Just defaultContainerAutostartPlan
                else Nothing
          },
      tracingPrerequisitePlan =
        case setupTracingEndpoint setupConfig of
          Nothing -> Nothing
          Just tracingEndpoint ->
            Just
              TracingPrerequisitePlan
                { tracingCheckEndpoint = tracingEndpoint,
                  tracingAutostartPlan =
                    if setupAutostartJaeger setupConfig
                      then Just defaultContainerAutostartPlan
                      else Nothing
                }
    }
