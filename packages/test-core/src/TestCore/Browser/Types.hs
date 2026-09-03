{-# LANGUAGE OverloadedStrings #-}

module TestCore.Browser.Types
  ( BrowserConfig (..),
    BrowserMetrics (..),
    BrowserRunnerError (..),
  )
where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import System.Exit (ExitCode)

data BrowserConfig = BrowserConfig
  { browserRunnerCommand :: FilePath,
    browserRunnerArguments :: [String],
    browserHeadless :: Bool,
    browserPauseOnFailure :: Bool,
    -- | Playwright operation and assertion bound, passed to the runner.
    browserTimeoutMilliseconds :: Int,
    -- | Upper bound for a single request/response exchange with the runner.
    -- This is intentionally independent of 'browserTimeoutMilliseconds': a
    -- responsive runner can be delayed briefly by host scheduling, while a
    -- missing response must still terminate the scenario.
    browserProtocolTimeoutMilliseconds :: Int,
    browserArtifactDirectory :: FilePath
  }
  deriving (Eq, Show)

data BrowserMetrics = BrowserMetrics
  { enhancedNavigationFetchCount :: Int,
    hardNavigationCount :: Int,
    mutationRequestCount :: Int
  }
  deriving (Eq, Show)

instance FromJSON BrowserMetrics where
  parseJSON = withObject "BrowserMetrics" $ \value ->
    BrowserMetrics
      <$> value .: "enhancedNavigationFetchCount"
      <*> value .: "hardNavigationCount"
      <*> value .: "mutationRequestCount"

data BrowserRunnerError
  = BrowserRunnerLaunchError String
  | BrowserRunnerProcessError ExitCode String String
  | BrowserRunnerProtocolError String
  | BrowserCommandFailed Int String [FilePath]
  | BrowserAssertionFailed String [FilePath]
  deriving (Eq, Show)
