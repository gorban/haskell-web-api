{-# LANGUAGE OverloadedStrings #-}

module TestCore.Browser.Config
  ( defaultPlaywrightBrowserConfig,
    loadPlaywrightBrowserConfig,
    parseBrowserConfig,
  )
where

import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Environment (getEnvironment)
import System.FilePath (takeDirectory, (</>))
import TestCore.Browser.Types (BrowserConfig (..))
import Text.Read (readMaybe)

defaultPlaywrightBrowserConfig :: BrowserConfig
defaultPlaywrightBrowserConfig =
  BrowserConfig
    { browserRunnerCommand = "node",
      browserRunnerArguments = [playwrightRunnerRelativePath],
      browserHeadless = True,
      browserPauseOnFailure = False,
      browserTimeoutMilliseconds = 10000,
      browserArtifactDirectory = "test-results" </> "playwright"
    }

playwrightRunnerRelativePath :: FilePath
playwrightRunnerRelativePath = "packages" </> "test-core" </> "playwright-runner" </> "runner.cjs"

loadPlaywrightBrowserConfig :: IO (Either String BrowserConfig)
loadPlaywrightBrowserConfig = do
  workingDirectory <- getCurrentDirectory
  resolvedRunner <- findRunner workingDirectory
  environment <- getEnvironment
  pure $ do
    runnerPath <- resolvedRunner
    parseBrowserConfigWithDefault
      defaultPlaywrightBrowserConfig {browserRunnerArguments = [runnerPath]}
      environment
  where
    findRunner directory = do
      let candidate = directory </> playwrightRunnerRelativePath
          parent = takeDirectory directory
      candidateExists <- doesFileExist candidate
      if candidateExists
        then pure (Right candidate)
        else
          if parent == directory
            then pure (Left ("Could not find bundled Playwright runner: " <> playwrightRunnerRelativePath))
            else findRunner parent

parseBrowserConfig :: [(String, String)] -> Either String BrowserConfig
parseBrowserConfig = parseBrowserConfigWithDefault defaultPlaywrightBrowserConfig

parseBrowserConfigWithDefault :: BrowserConfig -> [(String, String)] -> Either String BrowserConfig
parseBrowserConfigWithDefault baseConfig environment = do
  headless <- parseOptionalBoolean "TEST_CORE_BROWSER_HEADLESS" (browserHeadless baseConfig) (lookup "TEST_CORE_BROWSER_HEADLESS" environment)
  pauseOnFailure <- parseOptionalBoolean "TEST_CORE_BROWSER_PAUSE_ON_FAILURE" (browserPauseOnFailure baseConfig) (lookup "TEST_CORE_BROWSER_PAUSE_ON_FAILURE" environment)
  timeoutMilliseconds <- parseOptionalPositiveInt "TEST_CORE_BROWSER_TIMEOUT_MILLISECONDS" (browserTimeoutMilliseconds baseConfig) (lookup "TEST_CORE_BROWSER_TIMEOUT_MILLISECONDS" environment)
  pure
    baseConfig
      { browserRunnerCommand = fromMaybe (browserRunnerCommand baseConfig) (lookup "TEST_CORE_BROWSER_RUNNER" environment),
        browserRunnerArguments = maybe (browserRunnerArguments baseConfig) splitRunnerArguments (lookup "TEST_CORE_BROWSER_RUNNER_ARGUMENTS" environment),
        browserHeadless = headless,
        browserPauseOnFailure = pauseOnFailure,
        browserTimeoutMilliseconds = timeoutMilliseconds,
        browserArtifactDirectory = fromMaybe (browserArtifactDirectory baseConfig) (lookup "TEST_CORE_BROWSER_ARTIFACT_DIRECTORY" environment)
      }

parseOptionalBoolean :: String -> Bool -> Maybe String -> Either String Bool
parseOptionalBoolean _ fallback Nothing = Right fallback
parseOptionalBoolean variableName _ (Just value) =
  maybe
    (Left ("Invalid boolean for " <> variableName <> ": " <> value))
    Right
    (lookup (map toLower value) booleanValues)

booleanValues :: [(String, Bool)]
booleanValues =
  [ ("true", True),
    ("false", False),
    ("1", True),
    ("0", False),
    ("yes", True),
    ("no", False)
  ]

parseOptionalPositiveInt :: String -> Int -> Maybe String -> Either String Int
parseOptionalPositiveInt _ fallback Nothing = Right fallback
parseOptionalPositiveInt variableName _ (Just value) =
  case readMaybe value of
    Just parsed | parsed > 0 -> Right parsed
    _ -> Left ("Invalid positive integer for " <> variableName <> ": " <> value)

splitRunnerArguments :: String -> [String]
splitRunnerArguments value =
  filter (not . null) (map (Text.unpack . Text.strip) (Text.splitOn "," (Text.pack value)))
