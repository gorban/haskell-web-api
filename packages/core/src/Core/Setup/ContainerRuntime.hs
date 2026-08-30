{-# LANGUAGE OverloadedStrings #-}

-- | Shared container-runtime fallback for setup prerequisites.
--
-- FQ9 groups the three stable outcome constructors for a single caller's
-- domain result. The plan, command arguments, and disabled explanation remain
-- explicit because they vary for each autostart attempt.
module Core.Setup.ContainerRuntime
  ( ContainerRuntimeFailure (..),
    ContainerAutostartOutcomes (..),
    attemptContainerAutostart,
    runContainerRuntimeCommand,
    tryContainerRuntimes,
  )
where

import Control.Exception (IOException, try)
import Core.Setup.PrerequisitePlan (ContainerAutostartPlan (..), ContainerRuntime (..))
import Data.Text (Text)
import Data.Text qualified as Text
import System.Exit (ExitCode (..))
import System.Process (proc, readCreateProcessWithExitCode)

data ContainerRuntimeFailure = ContainerRuntimeFailure
  { failedContainerRuntime :: ContainerRuntime,
    containerRuntimeFailureMessage :: Text
  }
  deriving (Eq, Show)

data ContainerAutostartOutcomes result = ContainerAutostartOutcomes
  { containerAutostartSkipped :: Text -> result,
    containerAutostartSucceeded :: ContainerRuntime -> result,
    containerAutostartFailed :: [ContainerRuntimeFailure] -> result
  }

attemptContainerAutostart ::
  (ContainerRuntime -> [String] -> IO (Either Text ())) ->
  Maybe ContainerAutostartPlan ->
  Text ->
  Either Text [String] ->
  ContainerAutostartOutcomes result ->
  IO result
attemptContainerAutostart runCommand maybeAutostartPlan disabledMessage commandArguments outcomes =
  case maybeAutostartPlan of
    Nothing -> pure (containerAutostartSkipped outcomes disabledMessage)
    Just autostartPlan ->
      either (pure . containerAutostartSkipped outcomes) runWithArguments commandArguments
      where
        runWithArguments arguments =
          either (containerAutostartFailed outcomes) (containerAutostartSucceeded outcomes)
            <$> tryContainerRuntimes
              (autostartRuntimes autostartPlan)
              (`runCommand` arguments)

runContainerRuntimeCommand :: ContainerRuntime -> [String] -> IO (Either Text ())
runContainerRuntimeCommand runtime commandArguments =
  renderProcessResult
    <$> ( try
            (readCreateProcessWithExitCode (proc executable commandArguments) "") ::
            IO (Either IOException (ExitCode, String, String))
        )
  where
    executable = renderContainerRuntimeExecutable runtime

tryContainerRuntimes ::
  [ContainerRuntime] ->
  (ContainerRuntime -> IO (Either Text ())) ->
  IO (Either [ContainerRuntimeFailure] ContainerRuntime)
tryContainerRuntimes runtimes runCommand =
  tryRemainingRuntimes [] runtimes
  where
    tryRemainingRuntimes failures [] =
      pure (Left (reverse failures))
    tryRemainingRuntimes failures (runtime : remainingRuntimes) =
      runCommand runtime
        >>= either
          ( \failureMessage ->
              tryRemainingRuntimes
                (ContainerRuntimeFailure runtime failureMessage : failures)
                remainingRuntimes
          )
          (\() -> pure (Right runtime))

renderProcessResult :: Either IOException (ExitCode, String, String) -> Either Text ()
renderProcessResult =
  either (Left . Text.pack . show) renderExitResult

renderExitResult :: (ExitCode, String, String) -> Either Text ()
renderExitResult (ExitSuccess, _, _) =
  Right ()
renderExitResult (ExitFailure exitCode, stdoutText, stderrText) =
  Left (renderCommandFailure exitCode stdoutText stderrText)

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

renderContainerRuntimeExecutable :: ContainerRuntime -> String
renderContainerRuntimeExecutable containerRuntime =
  case containerRuntime of
    PodmanRuntime -> "podman"
    DockerRuntime -> "docker"
