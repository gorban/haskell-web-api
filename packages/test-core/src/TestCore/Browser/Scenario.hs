{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Haskell-authored browser scenarios. The Node process remains a thin
-- Playwright adapter; scenario control flow and assertions stay here.
module TestCore.Browser.Scenario
  ( BrowserScenario,
    assertAll,
    assertAttribute,
    assertEventually,
    assertFocused,
    assertMetrics,
    assertText,
    assertUrl,
    assertValue,
    assertVisible,
    blockRequestsMatching,
    click,
    failBlockedRequestsMatching,
    runPageScript,
    fill,
    historyBack,
    historyForward,
    releaseRequestsMatching,
    reload,
    runBrowserScenario,
    setCookie,
    submit,
    visit,
    visitWithoutScripts,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, displayException, fromException, throwIO, try)
import Control.Monad (void)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Aeson (Value, (.=))
import Data.Aeson.Types (Pair)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Clock (getMonotonicTimeNSec)
import Test.HUnit.Lang (HUnitFailure)
import Test.Hspec (Expectation)
import TestCore.Browser.Model.Internal
  ( BrowserObservation,
    CompiledObservation (..),
    Locator,
    attributeValue,
    browserMetrics,
    compileObservation,
    currentUrl,
    fromJsonResult,
    inputValue,
    isFocused,
    isVisible,
    textContent,
  )
import TestCore.Browser.Protocol (BrowserSession, sendCommand, sessionConfig, withBrowserSession)
import TestCore.Browser.Types (BrowserConfig (browserTimeoutMilliseconds), BrowserMetrics, BrowserRunnerError (..))
import TestCore.CustomAssertions (expectAll)

newtype BrowserScenario a = BrowserScenario
  { unBrowserScenario :: ReaderT BrowserSession (ExceptT BrowserRunnerError IO) a
  }
  deriving newtype (Functor, Applicative, Monad)

askSession :: BrowserScenario BrowserSession
askSession = BrowserScenario ask

liftScenarioIO :: IO a -> BrowserScenario a
liftScenarioIO = BrowserScenario . liftIO

throwScenarioError :: BrowserRunnerError -> BrowserScenario a
throwScenarioError = BrowserScenario . throwError

runBrowserScenario :: BrowserConfig -> BrowserScenario a -> IO (Either BrowserRunnerError a)
runBrowserScenario config scenario =
  withBrowserSession config (runExceptT . runReaderT (unBrowserScenario scenario))

command :: Text -> [Pair] -> BrowserScenario Value
command commandName fields = do
  session <- askSession
  result <- liftScenarioIO (sendCommand session commandName fields)
  either throwScenarioError pure result

simpleCommand :: Text -> [Pair] -> BrowserScenario ()
simpleCommand commandName fields = void (command commandName fields)

visit :: Text -> BrowserScenario ()
visit url = simpleCommand "visit" ["url" .= url]

visitWithoutScripts :: Text -> BrowserScenario ()
visitWithoutScripts url = simpleCommand "visitWithoutScripts" ["url" .= url]

-- | Seed a same-origin browser cookie before visiting the supplied URL. This is
-- intentionally scoped to the URL rather than exposing arbitrary browser
-- context state to application scenarios.
setCookie :: Text -> Text -> Text -> BrowserScenario ()
setCookie url name value =
  simpleCommand "setCookie" ["url" .= url, "name" .= name, "value" .= value]

reload :: BrowserScenario ()
reload = simpleCommand "reload" []

click :: Locator -> BrowserScenario ()
click locator = simpleCommand "click" ["locator" .= locator]

-- | Evaluate a test-owned expression in the current page. This is deliberately
-- limited to E2E fixtures that need to control browser-only failure modes which
-- ordinary user interactions cannot induce deterministically.
runPageScript :: Text -> BrowserScenario Value
runPageScript source = command "runPageScript" ["source" .= source]

fill :: Locator -> Text -> BrowserScenario ()
fill locator value = simpleCommand "fill" ["locator" .= locator, "value" .= value]

submit :: Locator -> BrowserScenario ()
submit locator = simpleCommand "submit" ["locator" .= locator]

historyBack :: BrowserScenario ()
historyBack = simpleCommand "historyBack" []

historyForward :: BrowserScenario ()
historyForward = simpleCommand "historyForward" []

blockRequestsMatching :: Text -> BrowserScenario ()
blockRequestsMatching patternText = simpleCommand "blockRequestsMatching" ["pattern" .= patternText]

releaseRequestsMatching :: Text -> BrowserScenario ()
releaseRequestsMatching patternText = simpleCommand "releaseRequestsMatching" ["pattern" .= patternText]

-- | Fail requests that were deliberately held with 'blockRequestsMatching'.
-- This lets an E2E fixture prove the browser's real script-error path after a
-- control has already been captured.
failBlockedRequestsMatching :: Text -> BrowserScenario ()
failBlockedRequestsMatching patternText = simpleCommand "failBlockedRequestsMatching" ["pattern" .= patternText]

observe :: BrowserObservation a -> BrowserScenario a
observe observation = do
  let compiled = compileObservation observation
  response <- command "observeMany" ["observations" .= compiledRequests compiled]
  responseValues <-
    case fromJsonResult response of
      Left decodeError -> throwScenarioError (BrowserRunnerProtocolError decodeError)
      Right values -> pure values
  case decodeCompiledValues compiled responseValues of
    Left decodeError -> throwScenarioError (BrowserRunnerProtocolError decodeError)
    Right (result, []) -> pure result
    Right (_, remaining) -> throwScenarioError (BrowserRunnerProtocolError ("browser runner returned " <> show (length remaining) <> " unexpected observation values"))

assertEventually :: BrowserObservation a -> (a -> Expectation) -> BrowserScenario ()
assertEventually observation expectation = do
  session <- askSession
  startedAt <- liftScenarioIO getMonotonicTimeNSec
  retryUntil session startedAt Nothing
  where
    retryUntil session startedAt lastFailure = do
      observedValue <- observe observation
      assertionAttempt <- liftScenarioIO (try (expectation observedValue) :: IO (Either SomeException ()))
      case assertionAttempt of
        Right () -> pure ()
        Left assertionException ->
          case fromException assertionException :: Maybe HUnitFailure of
            Nothing -> liftScenarioIO (throwIO assertionException)
            Just _ -> do
              now <- liftScenarioIO getMonotonicTimeNSec
              let elapsedMilliseconds = fromIntegral ((now - startedAt) `div` 1000000)
                  timeoutMilliseconds = browserTimeoutMilliseconds (sessionConfig session)
                  failureMessage = displayException assertionException
              if elapsedMilliseconds >= timeoutMilliseconds
                then
                  throwScenarioError
                    ( BrowserAssertionFailed
                        ( fromMaybe failureMessage lastFailure
                            <> " (timed out after "
                            <> show timeoutMilliseconds
                            <> "ms; last failure: "
                            <> failureMessage
                            <> ")"
                        )
                        []
                    )
                else do
                  liftScenarioIO (threadDelay 25000)
                  retryUntil session startedAt (Just failureMessage)

-- | Retry one composed observation, then report every independent expectation
-- against that observation. Keep browser actions and dependent checks outside
-- this helper so they remain fail-fast.
assertAll :: BrowserObservation a -> (a -> NonEmpty Expectation) -> BrowserScenario ()
assertAll observation expectations =
  assertEventually observation (expectAll . expectations)

assertText :: Locator -> (Text -> Expectation) -> BrowserScenario ()
assertText locator = assertEventually (textContent locator)

assertValue :: Locator -> (Text -> Expectation) -> BrowserScenario ()
assertValue locator = assertEventually (inputValue locator)

assertAttribute :: Locator -> Text -> (Maybe Text -> Expectation) -> BrowserScenario ()
assertAttribute locator attributeName = assertEventually (attributeValue locator attributeName)

assertFocused :: Locator -> (Bool -> Expectation) -> BrowserScenario ()
assertFocused locator = assertEventually (isFocused locator)

assertVisible :: Locator -> (Bool -> Expectation) -> BrowserScenario ()
assertVisible locator = assertEventually (isVisible locator)

assertUrl :: (Text -> Expectation) -> BrowserScenario ()
assertUrl = assertEventually currentUrl

assertMetrics :: (BrowserMetrics -> Expectation) -> BrowserScenario ()
assertMetrics = assertEventually browserMetrics
