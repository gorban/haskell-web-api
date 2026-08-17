{-# LANGUAGE OverloadedStrings #-}

module TestCore.Browser.Protocol
  ( BrowserSession,
    sendCommand,
    sessionConfig,
    withBrowserSession,
  )
where

import Control.Exception (IOException, SomeException, bracket, displayException, finally, mask, try)
import Control.Monad (void)
import Data.Aeson (Value (Null), eitherDecodeStrict', encode, object, withObject, (.:), (.:?), (.=))
import Data.Aeson.Types (Pair, parseEither)
import Data.ByteString.Lazy.Char8 qualified as LazyByteStringChar8
import Data.Functor ((<&>))
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.IO qualified as TextIO
import System.Exit (ExitCode (..))
import System.IO (BufferMode (LineBuffering), Handle, hClose, hFlush, hSetBuffering)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (..), createProcess, proc, terminateProcess, waitForProcess)
import TestCore.Browser.Types (BrowserConfig (..), BrowserRunnerError (..))

data BrowserSession = BrowserSession
  { sessionInput :: Handle,
    sessionOutput :: Handle,
    sessionProcess :: ProcessHandle,
    sessionNextCommandId :: IORef Int,
    sessionConfig :: BrowserConfig
  }

withBrowserSession :: BrowserConfig -> (BrowserSession -> IO (Either BrowserRunnerError a)) -> IO (Either BrowserRunnerError a)
withBrowserSession config action =
  mask $ \restore -> do
    launchResult <- launchBrowserSession config
    case launchResult of
      Left launchError -> pure (Left launchError)
      Right session ->
        bracket (pure session) releaseSession (restore . initializeBrowserSession action)

initializeBrowserSession :: (BrowserSession -> IO (Either BrowserRunnerError a)) -> BrowserSession -> IO (Either BrowserRunnerError a)
initializeBrowserSession action session = do
  initialized <- sendCommand session "initialize" (configurationFields (sessionConfig session))
  either (pure . Left) (const (runBrowserSession action session)) initialized

runBrowserSession :: (BrowserSession -> IO (Either BrowserRunnerError a)) -> BrowserSession -> IO (Either BrowserRunnerError a)
runBrowserSession action session = do
  scenarioAttempt <- tryAny (action session)
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

releaseSession :: BrowserSession -> IO ()
releaseSession session =
  safeTerminate (sessionProcess session)
    `finally` do
      safeClose (sessionInput session)
      safeClose (sessionOutput session)
      safeWait (sessionProcess session)

safeTerminate :: ProcessHandle -> IO ()
safeTerminate processHandle =
  void (try (terminateProcess processHandle) :: IO (Either IOException ()))

safeWait :: ProcessHandle -> IO ()
safeWait processHandle =
  void (try (waitForProcess processHandle) :: IO (Either IOException ExitCode))

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
