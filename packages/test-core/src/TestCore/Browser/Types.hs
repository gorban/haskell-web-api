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
    browserTimeoutMilliseconds :: Int,
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
