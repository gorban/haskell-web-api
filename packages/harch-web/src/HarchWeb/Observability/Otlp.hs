{-# LANGUAGE OverloadedStrings #-}

-- | Private OTLP transport and identifier generation.
--
-- The public observability API exposes typed spans and exporter configuration;
-- these implementation details deliberately remain behind that boundary.
module HarchWeb.Observability.Otlp
  ( currentUnixTimeNSec,
    nextOtlpSpanIdentifiers,
    nextOtlpSpanId,
    sendOtlpTraceRequest,
  )
where

import Control.Monad (unless)
import Data.Bits (shiftR, xor)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import HarchWeb.Observability (OtlpExporter (..))
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Client.TLS qualified as HttpClientTls
import Network.HTTP.Types qualified as Http
import System.IO.Unsafe (unsafePerformIO)

sendOtlpTraceRequest :: OtlpExporter -> LazyByteString.ByteString -> IO ()
sendOtlpTraceRequest exporter requestBody = do
  baseRequest <- HttpClient.parseRequest (Text.unpack (otlpEndpoint exporter))
  response <-
    HttpClient.httpLbs
      baseRequest
        { HttpClient.method = "POST",
          HttpClient.requestHeaders =
            (Http.hContentType, "application/json")
              : map otlpHeader (otlpHeaders exporter),
          HttpClient.requestBody = HttpClient.RequestBodyLBS requestBody
        }
      otlpHttpManager
  let statusCode = Http.statusCode (HttpClient.responseStatus response)
  unless (statusCode >= 200 && statusCode < 300) $
    ioError . userError $
      "OTLP trace export failed with status "
        <> show statusCode
        <> ".\nbody:\n"
        <> renderResponseBody response

currentUnixTimeNSec :: IO Word64
currentUnixTimeNSec =
  floor . (* 1000000000) <$> getPOSIXTime

nextOtlpSpanIdentifiers :: IO (Text, Text)
nextOtlpSpanIdentifiers = do
  requestSeed <- atomicModifyIORef' otlpSpanSeed (\seed -> let nextSeed = seed + 1 in (nextSeed, nextSeed))
  monotonicTime <- getMonotonicTimeNSec
  let traceIdBytes = word64Bytes monotonicTime <> word64Bytes requestSeed
      spanIdBytes = word64Bytes (monotonicTime `xor` (requestSeed + 0x9e3779b97f4a7c15))
  pure (otlpIdHexText traceIdBytes, otlpIdHexText spanIdBytes)

nextOtlpSpanId :: IO Text
nextOtlpSpanId = snd <$> nextOtlpSpanIdentifiers

renderResponseBody :: HttpClient.Response LazyByteString.ByteString -> String
renderResponseBody =
  Text.unpack . TextEncoding.decodeUtf8 . LazyByteString.toStrict . HttpClient.responseBody

otlpHeader :: (Text, Text) -> Http.Header
otlpHeader (headerName, headerValue) =
  (fromString (Text.unpack headerName), TextEncoding.encodeUtf8 headerValue)

otlpIdHexText :: ByteString.ByteString -> Text
otlpIdHexText =
  Text.concatMap renderHexByte . TextEncoding.decodeLatin1
  where
    renderHexByte byte =
      let byteValue = fromEnum byte
          highNibble = byteValue `div` 16
          lowNibble = byteValue `mod` 16
       in Text.pack [hexDigit highNibble, hexDigit lowNibble]

    hexDigit nibble =
      "0123456789abcdef" !! nibble

word64Bytes :: Word64 -> ByteString.ByteString
word64Bytes word =
  ByteString.pack
    [ fromIntegral (word `shiftR` 56),
      fromIntegral (word `shiftR` 48),
      fromIntegral (word `shiftR` 40),
      fromIntegral (word `shiftR` 32),
      fromIntegral (word `shiftR` 24),
      fromIntegral (word `shiftR` 16),
      fromIntegral (word `shiftR` 8),
      fromIntegral word
    ]

otlpHttpManager :: HttpClient.Manager
{-# NOINLINE otlpHttpManager #-}
otlpHttpManager =
  unsafePerformIO (HttpClient.newManager HttpClientTls.tlsManagerSettings)

otlpSpanSeed :: IORef Word64
{-# NOINLINE otlpSpanSeed #-}
otlpSpanSeed =
  unsafePerformIO (newIORef 0)
