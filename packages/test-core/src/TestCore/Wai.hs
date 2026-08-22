{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCore.Wai
  ( performWaiRequest,
    readResponseBody,
    waiRequest,
    nextRequestBodyChunk,
    requiredWaiResponseOrDie,
  )
where

import Data.ByteString qualified as ByteString
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LazyByteString
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Network.Wai qualified as Wai
import Network.Wai.Internal qualified as WaiInternal

-- | Runs a built WAI 'Wai.Application' against a request and captures the
-- single response it produces, for tests that exercise an application
-- end-to-end without a real HTTP listener.

-- $!-forced at 'Nothing'/'request'/'WaiInternal.ResponseReceived': these are
-- bare literal/variable arguments to an already-instrumented call, which
-- this project's documented HPC CSE-sharing artifact otherwise leaves
-- permanently unticked (see docs/design-guidance.md's
-- never-mask-a-gate-finding section).

{-# ANN performWaiRequest ("HLint: ignore Redundant $!" :: String) #-}
performWaiRequest :: IO Wai.Application -> Wai.Request -> IO Wai.Response
performWaiRequest buildWebApplication request = do
  webApplication <- buildWebApplication
  responseReference <- newIORef $! Nothing
  _ <- (webApplication $! request) (\response -> writeIORef responseReference (Just response) >> (pure $! WaiInternal.ResponseReceived))
  maybeResponse <- readIORef responseReference
  pure (requiredWaiResponseOrDie maybeResponse)

-- | The successful-response projection 'performWaiRequest' relies on: every
-- 'Wai.Application' calls its respond callback exactly once, so a 'Nothing'
-- here means the application under test never responded. Exported and
-- tested directly (with a deliberately absent response) since the real
-- 'performWaiRequest' call site can never legitimately reach this branch.
requiredWaiResponseOrDie :: Maybe Wai.Response -> Wai.Response
requiredWaiResponseOrDie = fromMaybe (error "expected WAI application to produce a response")

{-# ANN readResponseBody ("HLint: ignore Redundant $!" :: String) #-}

-- | Drains a 'Wai.Response's streaming body into 'Text' for assertions.
readResponseBody :: Wai.Response -> IO Text
readResponseBody response = do
  let (_, _, withStreamingBody) = Wai.responseToStream response
  chunksReference <- newIORef []
  withStreamingBody $ \streamingBody ->
    streamingBody
      (\builder -> modifyIORef' chunksReference (<> [Builder.toLazyByteString builder]))
      (pure $! ())
  chunks <- readIORef chunksReference
  pure (TextEncoding.decodeUtf8 (LazyByteString.toStrict (mconcat chunks)))

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
