{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | A Haskell-authored, real-browser scenario API. The Node process is only a
-- thin Playwright adapter; scenario control flow and assertions stay here.
module TestCore.Browser
  ( AriaRole (..),
    BrowserConfig (..),
    BrowserMetrics (..),
    BrowserObservation,
    BrowserRunnerError (..),
    BrowserScenario,
    Locator,
    assertAll,
    assertAttribute,
    assertEventually,
    assertFocused,
    assertMetrics,
    assertText,
    assertUrl,
    assertValue,
    assertVisible,
    attributeValue,
    blockRequestsMatching,
    browserMetrics,
    byAltText,
    byLabel,
    byPlaceholder,
    byRole,
    byTestId,
    byText,
    byTitle,
    click,
    containingText,
    css,
    currentUrl,
    defaultPlaywrightBrowserConfig,
    fill,
    historyBack,
    historyForward,
    inputValue,
    isFocused,
    isVisible,
    loadPlaywrightBrowserConfig,
    named,
    parseBrowserConfig,
    releaseRequestsMatching,
    reload,
    runBrowserScenario,
    setCookie,
    submit,
    textContent,
    visit,
    visitWithoutScripts,
    within,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, SomeException, displayException, fromException, throwIO, try)
import Control.Monad (void)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Aeson (FromJSON (parseJSON), Result (..), ToJSON (toJSON), Value (..), eitherDecodeStrict', encode, object, withObject, (.:), (.:?), (.=))
import Data.Aeson qualified
import Data.Aeson.Encoding qualified as AesonEncoding
import Data.Aeson.Types (Pair, parseEither)
import Data.ByteString.Lazy.Char8 qualified as LazyByteStringChar8
import Data.Char (toLower)
import Data.Functor ((<&>))
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.IO qualified as TextIO
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, (</>))
import System.IO (BufferMode (LineBuffering), Handle, hClose, hFlush, hSetBuffering)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), createProcess, proc, terminateProcess, waitForProcess)
import Test.HUnit.Lang (HUnitFailure)
import Test.Hspec (Expectation)
import TestCore.CustomAssertions (expectAll)
import Text.Read (readMaybe)

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

data AriaRole
  = Button
  | Checkbox
  | Form
  | Heading
  | Link
  | List
  | ListItem
  | Navigation
  | Radio
  | Status
  | Textbox

data Locator
  = RoleLocator AriaRole (Maybe Text)
  | LabelLocator Text
  | TextLocator Text
  | PlaceholderLocator Text
  | AltTextLocator Text
  | TitleLocator Text
  | TestIdLocator Text
  | CssLocator Text
  | WithinLocator Locator Locator
  | ContainingTextLocator Locator Text

instance ToJSON Locator where
  toJSON locator =
    case locator of
      RoleLocator role accessibleName ->
        object
          [ "kind" .= ("role" :: Text),
            "role" .= renderAriaRole role,
            "name" .= accessibleName
          ]
      LabelLocator labelText -> textLocatorJson "label" labelText
      TextLocator visibleText -> textLocatorJson "text" visibleText
      PlaceholderLocator placeholderText -> textLocatorJson "placeholder" placeholderText
      AltTextLocator alternativeText -> textLocatorJson "altText" alternativeText
      TitleLocator titleText -> textLocatorJson "title" titleText
      TestIdLocator testId -> textLocatorJson "testId" testId
      CssLocator selector -> textLocatorJson "css" selector
      WithinLocator parent child ->
        object
          [ "kind" .= ("within" :: Text),
            "parent" .= parent,
            "child" .= child
          ]
      ContainingTextLocator target containedText ->
        object
          [ "kind" .= ("containingText" :: Text),
            "locator" .= target,
            "text" .= containedText
          ]
    where
      textLocatorJson kind value = object ["kind" .= (kind :: Text), "text" .= value]

  toEncoding = AesonEncoding.value . toJSON
  toEncodingList = AesonEncoding.value . Data.Aeson.toJSONList

renderAriaRole :: AriaRole -> Text
renderAriaRole role =
  case role of
    Button -> "button"
    Checkbox -> "checkbox"
    Form -> "form"
    Heading -> "heading"
    Link -> "link"
    List -> "list"
    ListItem -> "listitem"
    Navigation -> "navigation"
    Radio -> "radio"
    Status -> "status"
    Textbox -> "textbox"

byRole :: AriaRole -> Locator
byRole role = RoleLocator role Nothing

named :: Locator -> Text -> Locator
named locator accessibleName =
  case locator of
    RoleLocator role _ -> RoleLocator role (Just accessibleName)
    _ -> ContainingTextLocator locator accessibleName

byLabel :: Text -> Locator
byLabel = LabelLocator

byText :: Text -> Locator
byText = TextLocator

byPlaceholder :: Text -> Locator
byPlaceholder = PlaceholderLocator

byAltText :: Text -> Locator
byAltText = AltTextLocator

byTitle :: Text -> Locator
byTitle = TitleLocator

byTestId :: Text -> Locator
byTestId = TestIdLocator

css :: Text -> Locator
css = CssLocator

within :: Locator -> Locator -> Locator
within = WithinLocator

containingText :: Locator -> Text -> Locator
containingText = ContainingTextLocator

data ObservationLeaf a where
  TextContentObservation :: Locator -> ObservationLeaf Text
  InputValueObservation :: Locator -> ObservationLeaf Text
  AttributeValueObservation :: Locator -> Text -> ObservationLeaf (Maybe Text)
  FocusedObservation :: Locator -> ObservationLeaf Bool
  VisibleObservation :: Locator -> ObservationLeaf Bool
  CurrentUrlObservation :: ObservationLeaf Text
  BrowserMetricsObservation :: ObservationLeaf BrowserMetrics

data BrowserObservation a where
  PureObservation :: a -> BrowserObservation a
  MapObservation :: (a -> b) -> BrowserObservation a -> BrowserObservation b
  ApplyObservation :: BrowserObservation (a -> b) -> BrowserObservation a -> BrowserObservation b
  LeafObservation :: (FromJSON a) => ObservationLeaf a -> BrowserObservation a

instance Functor BrowserObservation where
  fmap = MapObservation

instance Applicative BrowserObservation where
  pure = PureObservation
  (<*>) = ApplyObservation

textContent :: Locator -> BrowserObservation Text
textContent = LeafObservation . TextContentObservation

inputValue :: Locator -> BrowserObservation Text
inputValue = LeafObservation . InputValueObservation

attributeValue :: Locator -> Text -> BrowserObservation (Maybe Text)
attributeValue locator attributeName = LeafObservation (AttributeValueObservation locator attributeName)

isFocused :: Locator -> BrowserObservation Bool
isFocused = LeafObservation . FocusedObservation

isVisible :: Locator -> BrowserObservation Bool
isVisible = LeafObservation . VisibleObservation

currentUrl :: BrowserObservation Text
currentUrl = LeafObservation CurrentUrlObservation

browserMetrics :: BrowserObservation BrowserMetrics
browserMetrics = LeafObservation BrowserMetricsObservation

data CompiledObservation a = CompiledObservation
  { compiledRequests :: [Value],
    decodeCompiledValues :: [Value] -> Either String (a, [Value])
  }

compileObservation :: BrowserObservation a -> CompiledObservation a
compileObservation observation =
  case observation of
    PureObservation value -> CompiledObservation [] (\remaining -> Right (value, remaining))
    MapObservation transform child ->
      let compiledChild = compileObservation child
       in CompiledObservation
            (compiledRequests compiledChild)
            ( \values -> do
                (childValue, remaining) <- decodeCompiledValues compiledChild values
                pure (transform childValue, remaining)
            )
    ApplyObservation functionObservation valueObservation ->
      let compiledFunction = compileObservation functionObservation
          compiledValue = compileObservation valueObservation
       in CompiledObservation
            (compiledRequests compiledFunction <> compiledRequests compiledValue)
            ( \values -> do
                (functionValue, afterFunction) <- decodeCompiledValues compiledFunction values
                (argumentValue, remaining) <- decodeCompiledValues compiledValue afterFunction
                pure (functionValue argumentValue, remaining)
            )
    LeafObservation leaf ->
      CompiledObservation
        [observationLeafJson leaf]
        ( \case
            [] -> Left "browser runner omitted an observation value"
            value : remaining ->
              case fromJsonResult value of
                Left decodeError -> Left decodeError
                Right decodedValue -> Right (decodedValue, remaining)
        )

observationLeafJson :: ObservationLeaf a -> Value
observationLeafJson leaf =
  case leaf of
    TextContentObservation locator -> locatedObservation "textContent" locator []
    InputValueObservation locator -> locatedObservation "inputValue" locator []
    AttributeValueObservation locator attributeName -> locatedObservation "attributeValue" locator ["attribute" .= attributeName]
    FocusedObservation locator -> locatedObservation "focused" locator []
    VisibleObservation locator -> locatedObservation "visible" locator []
    CurrentUrlObservation -> object ["kind" .= ("currentUrl" :: Text)]
    BrowserMetricsObservation -> object ["kind" .= ("browserMetrics" :: Text)]
  where
    locatedObservation kind locator fields =
      object (["kind" .= (kind :: Text), "locator" .= locator] <> fields)

fromJsonResult :: (FromJSON a) => Value -> Either String a
fromJsonResult value =
  case Data.Aeson.fromJSON value of
    Error message -> Left message
    Success decodedValue -> Right decodedValue

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

data BrowserSession = BrowserSession
  { sessionInput :: Handle,
    sessionOutput :: Handle,
    sessionProcess :: ProcessHandle,
    sessionNextCommandId :: IORef Int,
    sessionConfig :: BrowserConfig
  }

data BrowserRunnerError
  = BrowserRunnerLaunchError String
  | BrowserRunnerProcessError ExitCode String String
  | BrowserRunnerProtocolError String
  | BrowserCommandFailed Int String [FilePath]
  | BrowserAssertionFailed String [FilePath]
  deriving (Eq, Show)

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

runBrowserScenario :: BrowserConfig -> BrowserScenario a -> IO (Either BrowserRunnerError a)
runBrowserScenario config scenario =
  launchBrowserSession config >>= either (pure . Left) (runInitializedScenario config scenario)

runInitializedScenario :: BrowserConfig -> BrowserScenario a -> BrowserSession -> IO (Either BrowserRunnerError a)
runInitializedScenario config scenario session = do
  initialized <- sendCommand session "initialize" (configurationFields config)
  either (closeFailedSession session) (const (runInitializedBrowserScenario scenario session)) initialized

runInitializedBrowserScenario :: BrowserScenario a -> BrowserSession -> IO (Either BrowserRunnerError a)
runInitializedBrowserScenario scenario session = do
  scenarioAttempt <- tryAny (runExceptT (runReaderT (unBrowserScenario scenario) session))
  let scenarioResult =
        case scenarioAttempt of
          Left unexpectedException -> Left (BrowserRunnerProtocolError (displayException unexpectedException))
          Right result -> result
  finishResult <- finishSession session scenarioResult
  exitCode <- waitForProcess (sessionProcess session)
  pure (mergeSessionResults exitCode scenarioResult finishResult)

launchBrowserSession :: BrowserConfig -> IO (Either BrowserRunnerError BrowserSession)
launchBrowserSession config = do
  let processSpec =
        (proc (browserRunnerCommand config) (browserRunnerArguments config))
          { std_in = CreatePipe,
            std_out = CreatePipe,
            std_err = Inherit
          }
  processAttempt <- try (createProcess processSpec) :: IO (Either IOException (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle))
  case processAttempt of
    Left ioException -> pure (Left (BrowserRunnerLaunchError (displayException ioException)))
    Right (maybeInputHandle, maybeOutputHandle, _, processHandle) -> do
      -- CreatePipe guarantees these handles. Keeping that invariant here avoids
      -- exposing an impossible public protocol failure branch.
      let inputHandle = fromJust maybeInputHandle
          outputHandle = fromJust maybeOutputHandle
      hSetBuffering inputHandle LineBuffering
      hSetBuffering outputHandle LineBuffering
      commandIdReference <- newIORef 0
      pure
        ( Right
            BrowserSession
              { sessionInput = inputHandle,
                sessionOutput = outputHandle,
                sessionProcess = processHandle,
                sessionNextCommandId = commandIdReference,
                sessionConfig = config
              }
        )

configurationFields :: BrowserConfig -> [Pair]
configurationFields config =
  [ "headless" .= browserHeadless config,
    "pauseOnFailure" .= browserPauseOnFailure config,
    "timeoutMilliseconds" .= browserTimeoutMilliseconds config,
    "artifactDirectory" .= browserArtifactDirectory config
  ]

finishSession :: BrowserSession -> Either BrowserRunnerError a -> IO (Either BrowserRunnerError [FilePath])
finishSession session scenarioResult = do
  let failureMessage = either (Just . show) (const Nothing) scenarioResult
  finishResponse <- sendCommand session "finish" ["failure" .= failureMessage]
  safeClose (sessionInput session)
  safeClose (sessionOutput session)
  pure $ do
    responseValue <- finishResponse
    mapLeft
      BrowserRunnerProtocolError
      ( parseEither
          (withObject "finish response" $ \value -> (value .:? "artifacts") <&> fromMaybe [])
          responseValue
      )

closeFailedSession :: BrowserSession -> BrowserRunnerError -> IO (Either BrowserRunnerError a)
closeFailedSession session initializationError = do
  terminateProcess (sessionProcess session)
  safeClose (sessionInput session)
  safeClose (sessionOutput session)
  _ <- waitForProcess (sessionProcess session)
  pure (Left initializationError)

safeClose :: Handle -> IO ()
safeClose handle = void (try (hClose handle) :: IO (Either IOException ()))

mergeSessionResults :: ExitCode -> Either BrowserRunnerError a -> Either BrowserRunnerError [FilePath] -> Either BrowserRunnerError a
mergeSessionResults exitCode scenarioResult finishResult =
  case (scenarioResult, finishResult, exitCode) of
    (Left scenarioError, Right artifacts, _) -> Left (attachArtifacts artifacts scenarioError)
    (Left scenarioError, Left _, _) -> Left scenarioError
    (Right _, Left finishError, _) -> Left finishError
    (Right value, Right _, ExitSuccess) -> Right value
    (Right _, Right _, failedExitCode) -> Left (BrowserRunnerProcessError failedExitCode "" "browser runner exited unsuccessfully")

attachArtifacts :: [FilePath] -> BrowserRunnerError -> BrowserRunnerError
attachArtifacts artifacts browserError =
  case browserError of
    BrowserCommandFailed commandId message _ -> BrowserCommandFailed commandId message artifacts
    BrowserAssertionFailed message _ -> BrowserAssertionFailed message artifacts
    _ -> browserError

sendCommand :: BrowserSession -> Text -> [Pair] -> IO (Either BrowserRunnerError Value)
sendCommand session commandName fields = do
  commandId <- atomicModifyIORef' (sessionNextCommandId session) (\current -> let next = current + 1 in (next, next))
  let request = object (["protocol" .= (1 :: Int), "id" .= commandId, "command" .= commandName] <> fields)
  writeAttempt <- try $ do
    LazyByteStringChar8.hPutStrLn (sessionInput session) (encode request)
    hFlush (sessionInput session)
    TextIO.hGetLine (sessionOutput session)
  case writeAttempt of
    Left ioException -> pure (Left (BrowserRunnerProtocolError (displayException (ioException :: IOException))))
    Right responseLine ->
      pure $ do
        responseValue <- mapLeft BrowserRunnerProtocolError (eitherDecodeStrict' (TextEncoding.encodeUtf8 responseLine))
        parseCommandResponse commandId responseValue

parseCommandResponse :: Int -> Value -> Either BrowserRunnerError Value
parseCommandResponse expectedCommandId responseValue =
  case parseEither
    ( withObject "browser command response" $ \value -> do
        protocol <- value .: "protocol"
        responseId <- value .: "id"
        status <- value .: "status"
        if protocol /= (1 :: Int)
          then pure (Left (BrowserRunnerProtocolError ("Unsupported browser protocol version: " <> show protocol)))
          else
            if responseId /= expectedCommandId
              then pure (Left (BrowserRunnerProtocolError ("Expected command response " <> show expectedCommandId <> ", received " <> show responseId)))
              else case (status :: Text) of
                "ok" -> Right . fromMaybe Null <$> value .:? "value"
                "error" -> do
                  message <- value .: "message"
                  artifacts <- fromMaybe [] <$> value .:? "artifacts"
                  pure (Left (BrowserCommandFailed responseId message artifacts))
                _ -> pure (Left (BrowserRunnerProtocolError ("Unknown browser response status: " <> Text.unpack status)))
    )
    responseValue of
    Left parseError -> Left (BrowserRunnerProtocolError parseError)
    Right parsedResponse -> parsedResponse

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft transform result =
  case result of
    Left value -> Left (transform value)
    Right value -> Right value

tryAny :: IO a -> IO (Either SomeException a)
tryAny = try

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
