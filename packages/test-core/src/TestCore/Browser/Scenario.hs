{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Haskell-authored browser scenarios. The Node process remains a thin
-- Playwright adapter; scenario control flow and assertions stay here.
module TestCore.Browser.Scenario
  ( BrowserScenario,
    BrowserAssertionBlock,
    assertAll,
    assertAllObserved,
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
    emulateMobileViewport,
    failBlockedRequestsMatching,
    runPageScript,
    fill,
    historyBack,
    historyForward,
    paste,
    press,
    releaseRequestsMatching,
    reload,
    runBrowserScenario,
    runBrowserSpec,
    satisfies,
    matches,
    setCookie,
    setInputFiles,
    setViewportSize,
    submit,
    visit,
    visitWithoutScripts,
    waitForBlockedRequestCountMatching,
    waitForBlockedRequestsMatching,
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
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Clock (getMonotonicTimeNSec)
import Test.HUnit.Lang (HUnitFailure)
import Test.Hspec (Expectation, expectationFailure, shouldSatisfy)
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

-- | Interpret a scenario once at the Hspec boundary.  Scenario code keeps the
-- typed runner-error rail; ordinary E2E specs need not repeatedly assert a
-- successful @Right ()@ result.
runBrowserSpec :: BrowserConfig -> BrowserScenario () -> Expectation
runBrowserSpec config scenario = do
  result <- runBrowserScenario config scenario
  either (expectationFailure . show) pure result

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

-- | Resize the real browser viewport for responsive and focus-visibility
-- checks. Dimensions must be positive; the runner validates them again at
-- the protocol boundary.
setViewportSize :: Int -> Int -> BrowserScenario ()
setViewportSize width height = simpleCommand "setViewportSize" ["width" .= width, "height" .= height]

-- | Recreate the browser context as a touch-capable mobile device before a
-- visit. This exercises a document's viewport policy without exposing raw
-- Playwright context options to application scenarios.
emulateMobileViewport :: Int -> Int -> BrowserScenario ()
emulateMobileViewport width height = simpleCommand "emulateMobileViewport" ["width" .= width, "height" .= height]

reload :: BrowserScenario ()
reload = simpleCommand "reload" []

click :: Locator -> BrowserScenario ()
click locator = simpleCommand "click" ["locator" .= locator]

-- | Press a real keyboard key while the locator is focused.  This belongs at
-- the browser-scenario boundary so accessibility tests do not replace user
-- input with page-script event simulation.
press :: Locator -> Text -> BrowserScenario ()
press locator key = simpleCommand "press" ["locator" .= locator, "key" .= key]

-- | Paste through the browser clipboard and a real platform keyboard shortcut.
-- This deliberately lives beside 'press' so acceptance tests can prove that
-- application event capture accepts clipboard input rather than simulating an
-- @input@ event with page script.
paste :: Locator -> Text -> BrowserScenario ()
paste locator value = simpleCommand "paste" ["locator" .= locator, "value" .= value]

-- | Evaluate a test-owned expression in the current page. This is deliberately
-- limited to E2E fixtures that need to control browser-only failure modes which
-- ordinary user interactions cannot induce deterministically.
runPageScript :: Text -> BrowserScenario Value
runPageScript source = command "runPageScript" ["source" .= source]

fill :: Locator -> Text -> BrowserScenario ()
fill locator value = simpleCommand "fill" ["locator" .= locator, "value" .= value]

-- | Attach a real file already on disk to a file input, e.g. via a scenario
-- fixture written with 'System.IO.Temp.withSystemTempFile'. There is no
-- in-memory-buffer variant: sharing a real path keeps this test-only
-- protocol from needing to move file bytes through the JSON command
-- channel.
setInputFiles :: Locator -> FilePath -> BrowserScenario ()
setInputFiles locator filePath = simpleCommand "setInputFiles" ["locator" .= locator, "filePath" .= filePath]

submit :: Locator -> BrowserScenario ()
submit locator = simpleCommand "submit" ["locator" .= locator]

historyBack :: BrowserScenario ()
historyBack = simpleCommand "historyBack" []

historyForward :: BrowserScenario ()
historyForward = simpleCommand "historyForward" []

blockRequestsMatching :: Text -> BrowserScenario ()
blockRequestsMatching patternText = simpleCommand "blockRequestsMatching" ["pattern" .= patternText]

-- | Wait until a request matching a deliberate blocker is actually pending.
-- This makes a subsequent release or failure deterministic without exposing
-- Playwright routing state to scenarios.
waitForBlockedRequestsMatching :: Text -> BrowserScenario ()
waitForBlockedRequestsMatching patternText = simpleCommand "waitForBlockedRequestsMatching" ["pattern" .= patternText]

-- | Wait until at least the requested number of requests have reached a
-- deliberate blocker.  Use this before releasing a blocker shared by
-- concurrent requests, so a request that has been dispatched but has not yet
-- reached Playwright routing cannot be stranded behind a retired handler.
-- The runner rejects non-positive counts at the JSON boundary.
waitForBlockedRequestCountMatching :: Text -> Int -> BrowserScenario ()
waitForBlockedRequestCountMatching patternText expectedCount =
  simpleCommand "waitForBlockedRequestCountMatching" ["pattern" .= patternText, "count" .= expectedCount]

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

-- | Retry one semantically cohesive observation, then report every independent
-- expectation against that observation. This compatibility helper remains for
-- callers whose observation is intentionally one positional product; new
-- heterogeneous browser assertions should use 'assertAllObserved' instead.
-- Keep browser actions and dependent checks outside either helper so they
-- remain fail-fast.
assertAll :: BrowserObservation a -> (a -> NonEmpty Expectation) -> BrowserScenario ()
assertAll observation expectations =
  assertEventually observation (expectAll . expectations)

-- | An opaque, heterogeneous set of independent observations.  It gathers
-- one browser snapshot, rather than encoding unrelated observations as a
-- positional tuple.  The result is intentionally only useful for authoring
-- the block: dependent browser actions belong outside it, where their
-- fail-fast ordering remains explicit.
newtype BrowserAssertionBlock a = BrowserAssertionBlock ([ObservedAssertion], a)

data ObservedAssertion = forall value. ObservedAssertion (BrowserObservation value) (value -> Expectation)

instance Functor BrowserAssertionBlock where
  fmap transform (BrowserAssertionBlock (assertions, value)) = BrowserAssertionBlock (assertions, transform value)

instance Applicative BrowserAssertionBlock where
  pure value = BrowserAssertionBlock ([], value)
  BrowserAssertionBlock (left, transform) <*> BrowserAssertionBlock (right, value) = BrowserAssertionBlock (left <> right, transform value)

instance Monad BrowserAssertionBlock where
  BrowserAssertionBlock (assertions, value) >>= next =
    let BrowserAssertionBlock (successor, result) = next value
     in BrowserAssertionBlock (assertions <> successor, result)

infix 1 `satisfies`

-- | Add one predicate assertion to an aggregate browser snapshot.
satisfies :: (Show value) => BrowserObservation value -> (value -> Bool) -> BrowserAssertionBlock ()
satisfies observation predicate = observation `matches` (`shouldSatisfy` predicate)

infix 1 `matches`

-- | Add one arbitrary Hspec expectation to an aggregate browser snapshot.
matches :: BrowserObservation value -> (value -> Expectation) -> BrowserAssertionBlock ()
matches observation expectation = BrowserAssertionBlock ([ObservedAssertion observation expectation], ())

-- | Retry one heterogeneous snapshot and report all independent matchers in
-- source declaration order.  Empty blocks are a construction error because
-- they would otherwise create a misleading successful no-op assertion.
assertAllObserved :: BrowserAssertionBlock () -> BrowserScenario ()
assertAllObserved (BrowserAssertionBlock (assertions, _)) =
  case assertions of
    [] -> throwScenarioError (BrowserRunnerProtocolError "empty observed assertion block")
    _ -> do
      session <- askSession
      startedAt <- liftScenarioIO getMonotonicTimeNSec
      retryObserved session startedAt Nothing
  where
    retryObserved session startedAt lastFailure = do
      attempts <- observeAssertions assertions
      case attempts of
        [] -> throwScenarioError (BrowserRunnerProtocolError "observed assertion block produced no expectations")
        firstAttempt : remainingAttempts -> do
          assertionAttempt <- liftScenarioIO (try (expectAll (firstAttempt :| remainingAttempts)) :: IO (Either SomeException ()))
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
                    then throwScenarioError (BrowserAssertionFailed (fromMaybe failureMessage lastFailure <> " (timed out after " <> show timeoutMilliseconds <> "ms; last failure: " <> failureMessage <> ")") [])
                    else liftScenarioIO (threadDelay 25000) >> retryObserved session startedAt (Just failureMessage)

    observeAssertions observedAssertions = do
      let compiledAssertions = map compileAssertion observedAssertions
      response <- command "observeMany" ["observations" .= concatMap compiledAssertionRequests compiledAssertions]
      responseValues <-
        case fromJsonResult response of
          Left decodeError -> throwScenarioError (BrowserRunnerProtocolError decodeError)
          Right values -> pure values
      case decodeAssertions compiledAssertions responseValues of
        Left decodeError -> throwScenarioError (BrowserRunnerProtocolError decodeError)
        Right (expectations, []) -> pure expectations
        Right (_, remaining) -> throwScenarioError (BrowserRunnerProtocolError ("browser runner returned " <> show (length remaining) <> " unexpected observation values"))

    compileAssertion (ObservedAssertion observation expectation) =
      let compiled = compileObservation observation
       in CompiledAssertion
            (compiledRequests compiled)
            ( \values -> do
                (observedValue, remaining) <- decodeCompiledValues compiled values
                pure (expectation observedValue, remaining)
            )

    decodeAssertions [] values = Right ([], values)
    decodeAssertions (compiled : remaining) values = do
      (expectation, afterCompiled) <- compiledAssertionDecode compiled values
      (otherExpectations, afterAll) <- decodeAssertions remaining afterCompiled
      pure (expectation : otherExpectations, afterAll)

data CompiledAssertion = CompiledAssertion
  { compiledAssertionRequests :: [Value],
    compiledAssertionDecode :: [Value] -> Either String (Expectation, [Value])
  }

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
