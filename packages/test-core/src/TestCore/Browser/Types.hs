{-# LANGUAGE OverloadedStrings #-}

module TestCore.Browser.Types
  ( BrowserConfig (..),
    BrowserMetrics (..),
    BrowserRunnerError (..),
    ElementSnapshot (..),
  )
where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Data.Text (Text)
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

-- | Client-side operations observed by the browser runner.  Enhanced-navigation
-- and mutation counts are calls to @window.fetch@ made by the page, rather than
-- HTTP transport legs: a redirect or an intercepted-and-aborted request must
-- not turn one client action into multiple metric events.  Hard navigations
-- remain browser transport events because they do not call @window.fetch@.
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

-- | Properties read together from one element. Text is exact descendant text;
-- visibility follows the Chromium Playwright visibility contract, not opacity
-- or viewport intersection. Absence belongs to the surrounding Maybe, not text.
-- elementValue is present for inputs, textareas and selects; other elements
-- have no form-control value. An empty control value is Just empty Text.
data ElementSnapshot = ElementSnapshot
  { elementText :: Text,
    elementValue :: Maybe Text,
    elementVisible :: Bool,
    elementFocused :: Bool
  }
  deriving (Eq, Show)

instance FromJSON ElementSnapshot where
  parseJSON = withObject "ElementSnapshot" $ \value ->
    ElementSnapshot
      <$> value .: "elementText"
      <*> value .: "elementValue"
      <*> value .: "elementVisible"
      <*> value .: "elementFocused"

data BrowserRunnerError
  = BrowserRunnerLaunchError String
  | BrowserRunnerProcessError ExitCode String String
  | BrowserRunnerProtocolError String
  | BrowserCommandFailed Int String [FilePath]
  | BrowserAssertionFailed String [FilePath]
  deriving (Eq, Show)
