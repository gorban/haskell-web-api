{-# LANGUAGE DerivingStrategies #-}

-- | Bounded consumption of a WAI request body.
--
-- Body readers are explicit because different endpoint contracts need
-- different budgets.  This helper enforces the supplied budget while chunks
-- arrive, so a chunked request cannot bypass a check based only on
-- @Content-Length@.
module HarchWeb.Server.RequestBody
  ( RequestBodyReadFailure (..),
    readRequestBodyUpTo,
    newRequestBodyChunkReader,
  )
where

import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteStringChar8
import Data.ByteString.Lazy qualified as LazyByteString
import Data.IORef qualified as IORef
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai

-- | The only expected failure of a bounded body reader.  It carries no
-- request data and can therefore be safely mapped to a stable 413 response
-- and low-cardinality telemetry.
data RequestBodyReadFailure = RequestBodyLimitExceeded
  deriving stock (Eq, Show)

-- | Read at most the given number of bytes, rejecting as soon as a chunk
-- would exceed the budget.  A negative bound rejects every non-empty body;
-- public configuration is represented by non-negative newtypes, while this
-- small trusted helper remains convenient for fixed endpoint budgets.
readRequestBodyUpTo :: Int -> Wai.Request -> IO (Either RequestBodyReadFailure LazyByteString.ByteString)
readRequestBodyUpTo maximumBytes request
  | declaredContentLengthExceeds maximumBytes request = pure (Left RequestBodyLimitExceeded)
  | otherwise = go 0 []
  where
    go byteCount chunks = do
      chunk <- Wai.getRequestBodyChunk request
      let nextByteCount = byteCount + ByteString.length chunk
      if nextByteCount > maximumBytes
        then pure (Left RequestBodyLimitExceeded)
        else
          if ByteString.null chunk
            then pure (Right (LazyByteString.fromChunks (reverse chunks)))
            else go nextByteCount (chunk : chunks)

-- | Build a bounded, incremental chunk reader instead of buffering the whole
-- body first: each pull enforces the same running-total budget
-- 'readRequestBodyUpTo' enforces over a single call, so a caller that never
-- retains more than the current chunk keeps its own memory use bounded
-- regardless of body size. An empty chunk marks the end of the body,
-- matching 'Wai.getRequestBodyChunk'. The declared @Content-Length@ is
-- checked once up front, the same as the buffered reader, so an oversized
-- declared body is rejected before any chunk is pulled.
newRequestBodyChunkReader :: Int -> Wai.Request -> IO (IO (Either RequestBodyReadFailure ByteString.ByteString))
newRequestBodyChunkReader maximumBytes request
  | declaredContentLengthExceeds maximumBytes request = pure (pure (Left RequestBodyLimitExceeded))
  | otherwise = do
      byteCountReference <- IORef.newIORef 0
      pure (pullChunk byteCountReference)
  where
    pullChunk byteCountReference = do
      chunk <- Wai.getRequestBodyChunk request
      if ByteString.null chunk
        then pure (Right chunk)
        else do
          byteCount <- IORef.atomicModifyIORef' byteCountReference (\prior -> let next = prior + ByteString.length chunk in (next, next))
          if byteCount > maximumBytes
            then pure (Left RequestBodyLimitExceeded)
            else pure (Right chunk)

declaredContentLengthExceeds :: Int -> Wai.Request -> Bool
declaredContentLengthExceeds maximumBytes request =
  maybe False (> maximumBytes) (lookup Http.hContentLength (Wai.requestHeaders request) >>= parseContentLength)

parseContentLength :: ByteString.ByteString -> Maybe Int
parseContentLength contentLength = do
  (bodyBytes, remaining) <- ByteStringChar8.readInt contentLength
  if bodyBytes >= 0 && ByteString.null remaining then Just bodyBytes else Nothing
