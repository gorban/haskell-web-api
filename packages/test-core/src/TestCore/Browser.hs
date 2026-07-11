-- |
-- Module: TestCore.Browser
--
-- Haskell owns the browser-spec DSL and request/response protocol; browser automation itself lives behind an
-- external runner process. The intended long-term Playwright integration point is that runner boundary, using
-- the official Node client behind it rather than reimplementing Playwright's remote protocol in Haskell.
module TestCore.Browser
  ( BrowserAction (..),
    BrowserConfig (..),
    NavigationMetric (..),
    BrowserRunnerError (..),
    defaultBrowserConfig,
    loadBrowserConfig,
    parseBrowserConfig,
    renderBrowserRequest,
    runBrowserScript,
  )
where

import Control.Exception (IOException, displayException, try)
import Core.Config qualified as CoreConfig
import Core.System.Temp (withTempFile)
import Data.Char (toLower)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import System.Directory (doesFileExist)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process (proc, readCreateProcessWithExitCode)

data BrowserConfig = BrowserConfig
  { browserRunnerCommand :: FilePath,
    browserRunnerArguments :: [String],
    browserHeadless :: Bool,
    browserKeepOpenOnFailure :: Bool
  }
  deriving (Eq, Show)

data NavigationMetric
  = EnhancedFetchCount
  | HardNavigationCount
  deriving (Eq, Show)

data BrowserAction
  = VisitUrl String
  | VisitUrlWithoutScripts String
  | ReloadPage
  | ClickLinkWithText String
  | NavigateHistoryBack
  | NavigateHistoryForward
  | AssertTextEquals String String
  | AssertNavigationMetricEquals NavigationMetric Int
  deriving (Eq, Show)

data BrowserRunnerError
  = BrowserRunnerLaunchError String
  | BrowserRunnerProcessError ExitCode String String
  | BrowserRunnerProtocolError String
  | BrowserAssertionFailed String
  deriving (Eq, Show)

defaultBrowserConfig :: BrowserConfig
defaultBrowserConfig =
  BrowserConfig
    { browserRunnerCommand = "playwright-e2e-runner",
      browserRunnerArguments = [],
      browserHeadless = True,
      browserKeepOpenOnFailure = False
    }

loadBrowserConfig :: IO (Either String BrowserConfig)
loadBrowserConfig =
  fmap parseBrowserConfig getEnvironment

parseBrowserConfig :: [(String, String)] -> Either String BrowserConfig
parseBrowserConfig environment = do
  browserHeadless' <-
    parseOptionalBoolean
      "TEST_CORE_BROWSER_HEADLESS"
      True
      (lookup "TEST_CORE_BROWSER_HEADLESS" environment)
  browserKeepOpenOnFailure' <-
    parseOptionalBoolean
      "TEST_CORE_BROWSER_KEEP_OPEN_ON_FAILURE"
      False
      (lookup "TEST_CORE_BROWSER_KEEP_OPEN_ON_FAILURE" environment)
  pure
    defaultBrowserConfig
      { browserRunnerCommand = fromMaybe (browserRunnerCommand defaultBrowserConfig) (lookup "TEST_CORE_BROWSER_RUNNER" environment),
        browserRunnerArguments =
          maybe
            (browserRunnerArguments defaultBrowserConfig)
            parseRunnerArguments
            (lookup "TEST_CORE_BROWSER_RUNNER_ARGUMENTS" environment),
        browserHeadless = browserHeadless',
        browserKeepOpenOnFailure = browserKeepOpenOnFailure'
      }

runBrowserScript :: BrowserConfig -> [BrowserAction] -> IO (Either BrowserRunnerError ())
runBrowserScript browserConfig actions =
  withTempFile "browser-runner" [] "request.txt" $ \(tempRoot, requestPath) -> do
    let responsePath = tempRoot </> "response.txt"
        requestBody = renderBrowserRequest browserConfig actions
        createProcess =
          proc
            (browserRunnerCommand browserConfig)
            (browserRunnerArguments browserConfig ++ [requestPath, responsePath])
    writeFile requestPath requestBody
    processResult <- try (readCreateProcessWithExitCode createProcess "") :: IO (Either IOException (ExitCode, String, String))
    case processResult of
      Left ioException ->
        pure $
          Left $
            BrowserRunnerLaunchError (displayException ioException)
      Right (exitCode, stdoutText, stderrText) ->
        if isSuccessExitCode exitCode
          then do
            responseExists <- doesFileExist responsePath
            if responseExists
              then fmap parseBrowserResponse (readFile responsePath)
              else pure (Left (BrowserRunnerProtocolError "browser runner completed without writing a response file"))
          else
            pure $
              Left $
                BrowserRunnerProcessError exitCode stdoutText stderrText

renderBrowserRequest :: BrowserConfig -> [BrowserAction] -> String
renderBrowserRequest browserConfig actions =
  unlines $
    [ "headless\t" ++ renderBoolean (browserHeadless browserConfig),
      "keep-open-on-failure\t" ++ renderBoolean (browserKeepOpenOnFailure browserConfig)
    ]
      ++ map renderBrowserAction actions

parseOptionalBoolean :: String -> Bool -> Maybe String -> Either String Bool
parseOptionalBoolean _ fallback Nothing = Right fallback
parseOptionalBoolean variableName _ (Just value) =
  case map toLower value of
    "true" -> Right True
    "false" -> Right False
    "1" -> Right True
    "0" -> Right False
    "yes" -> Right True
    "no" -> Right False
    _ -> Left ("Invalid boolean for " ++ variableName ++ ": " ++ value)

renderBoolean :: Bool -> String
renderBoolean booleanValue =
  case booleanValue of
    True -> "true"
    False -> "false"

renderBrowserAction :: BrowserAction -> String
renderBrowserAction browserAction =
  case browserAction of
    VisitUrl url -> "action\tvisit-url\t" ++ url
    VisitUrlWithoutScripts url -> "action\tvisit-url-without-scripts\t" ++ url
    ReloadPage -> "action\treload-page"
    ClickLinkWithText linkText -> "action\tclick-link-with-text\t" ++ linkText
    NavigateHistoryBack -> "action\thistory-back"
    NavigateHistoryForward -> "action\thistory-forward"
    AssertTextEquals selector expectedText -> "action\tassert-text-equals\t" ++ selector ++ "\t" ++ expectedText
    AssertNavigationMetricEquals navigationMetric expectedCount ->
      "action\tassert-navigation-metric-equals\t"
        ++ renderNavigationMetric navigationMetric
        ++ "\t"
        ++ show expectedCount

renderNavigationMetric :: NavigationMetric -> String
renderNavigationMetric navigationMetric =
  case navigationMetric of
    EnhancedFetchCount -> "enhanced-fetch-count"
    HardNavigationCount -> "hard-navigation-count"

parseRunnerArguments :: String -> [String]
parseRunnerArguments =
  map Text.unpack
    . CoreConfig.parseDelimitedTextsUnsafe (Text.pack ",")
    . Text.pack

parseBrowserResponse :: String -> Either BrowserRunnerError ()
parseBrowserResponse responseBody =
  case lines responseBody of
    [] -> Left (BrowserRunnerProtocolError "browser runner returned an empty response")
    firstLine : _
      | firstLine == "ok" -> Right ()
    firstLine : _ ->
      case stripPrefix "error\t" firstLine of
        Just message -> Left (BrowserAssertionFailed message)
        Nothing -> Left (BrowserRunnerProtocolError ("Unexpected browser runner response: " ++ firstLine))

isSuccessExitCode :: ExitCode -> Bool
isSuccessExitCode exitCode =
  case exitCode of
    ExitSuccess -> True
    ExitFailure _ -> False
