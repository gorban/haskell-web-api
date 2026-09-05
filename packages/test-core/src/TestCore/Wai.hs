{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCore.Wai
  ( performWaiRequest,
    readResponseBody,
    readResponseBodyWithFlushCount,
    waiRequest,
    nextRequestBodyChunk,
    requiredWaiResponseOrDie,
  )
where

import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, tryTakeMVar)
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LazyByteString
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Network.Wai qualified as Wai
import Network.Wai.Internal qualified as WaiInternal

-- | Runs a built WAI 'Wai.Application' against a request and captures the
-- single response it produces, for tests that exercise an application
-- end-to-end without a real HTTP listener.
performWaiRequest :: IO Wai.Application -> Wai.Request -> IO Wai.Response
performWaiRequest buildWebApplication request = do
  webApplication <- buildWebApplication
  responseSlot <- newEmptyMVar
  responseReceipt <- webApplication request (recordResponse responseSlot)
  case responseReceipt of
    WaiInternal.ResponseReceived -> do
      maybeResponse <- tryTakeMVar responseSlot
      pure (requiredWaiResponseOrDie maybeResponse)

recordResponse :: MVar Wai.Response -> Wai.Response -> IO WaiInternal.ResponseReceived
recordResponse responseSlot response = do
  putMVar responseSlot response
  pure WaiInternal.ResponseReceived

-- | The successful-response projection 'performWaiRequest' relies on: every
-- 'Wai.Application' calls its respond callback exactly once, so a 'Nothing'
-- here means the application under test never responded. Exported and
-- tested directly (with a deliberately absent response) since the real
-- 'performWaiRequest' call site can never legitimately reach this branch.
requiredWaiResponseOrDie :: Maybe Wai.Response -> Wai.Response
requiredWaiResponseOrDie = fromMaybe (error "expected WAI application to produce a response")

-- | Drains a 'Wai.Response's streaming body into 'Text' for assertions.
readResponseBody :: Wai.Response -> IO Text
readResponseBody = fmap fst . readResponseBodyWithFlushCount

-- | Drains a response while also reporting how often its stream asked to
-- flush. This preserves an observable part of WAI's streaming contract rather
-- than treating its flush callback as a fake no-op.
readResponseBodyWithFlushCount :: Wai.Response -> IO (Text, Int)
readResponseBodyWithFlushCount response = do
  let (_, _, withStreamingBody) = Wai.responseToStream response
  chunksReference <- newIORef []
  flushCountReference <- newIORef 0
  withStreamingBody $ \streamingBody ->
    streamingBody
      (\builder -> modifyIORef' chunksReference (<> [Builder.toLazyByteString builder]))
      (modifyIORef' flushCountReference (+ 1))
  chunks <- readIORef chunksReference
  flushCount <- readIORef flushCountReference
  pure (TextEncoding.decodeUtf8 (LazyByteString.toStrict (mconcat chunks)), flushCount)

-- | Builds a minimal 'Wai.Request' for the given path segments, for tests
-- that only care about routing on the request path.
waiRequest :: [Text] -> Wai.Request
waiRequest segments =
  Wai.defaultRequest
    { Wai.rawPathInfo = TextEncoding.encodeUtf8 renderedPath,
      Wai.pathInfo = segments
    }
  where
    renderedPath =
      case segments of
        [] -> "/"
        _ -> "/" <> Text.intercalate "/" segments

-- | Pops the next chunk off a queued request-body chunk list, for building a
-- 'Wai.requestBody' reader in tests that feed a request body incrementally.
nextRequestBodyChunk :: IORef [ByteString.ByteString] -> IO ByteString.ByteString
nextRequestBodyChunk chunksReference =
  atomicModifyIORef' chunksReference $ \case
    [] -> ([], ByteString.empty)
    chunk : remainingChunks -> (remainingChunks, chunk)
