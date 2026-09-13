{-# LANGUAGE OverloadedStrings #-}

-- | Private OTLP transport and identifier generation.
--
-- The public observability API exposes typed spans and exporter configuration;
-- these implementation details deliberately remain behind that boundary.
module HarchWeb.Observability.Otlp
  ( currentUnixTimeNSec,
    newOtlpHttpManager,
    nextOtlpSpanIdentifiers,
    nextOtlpSpanId,
    OtlpExportFailure (..),
    renderOtlpExportFailure,
    sendOtlpTraceRequest,
  )
where

import Control.Exception (try)
import Crypto.Random.Entropy (getEntropy)
import Data.ByteString qualified as ByteString
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as LazyByteString
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word64)
import HarchWeb.Observability.Types (OtlpExporter (..))
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Client.TLS qualified as HttpClientTls
import Network.HTTP.Types qualified as Http

-- | A stable, payload-free OTLP export failure.
--
-- Decision (PR-SEC8, 2026-08-30): this is constructed at the existing OTLP
-- transport boundary, rather than asking each application log reporter to
-- redact @http-client@ exceptions. Those exceptions can render configured
-- request headers and endpoint queries, so neither the raw request nor its
-- exception text may cross this boundary. The closed categories retain the
-- operator-useful distinction between invalid configuration, transport, and
-- a rejecting collector without retaining a URL, header, TLS payload, or
-- response body.
data OtlpExportFailure
  = OtlpInvalidEndpoint
  | OtlpTransportFailure
  | OtlpCollectorRejectedStatus Int

-- | Renders the stable, payload-free OTLP failure category for a private
-- operator diagnostic.
renderOtlpExportFailure :: OtlpExportFailure -> Text.Text
renderOtlpExportFailure = \case
  OtlpInvalidEndpoint -> "OTLP endpoint is invalid"
  OtlpTransportFailure -> "OTLP transport failed"
  OtlpCollectorRejectedStatus statusCode ->
    "OTLP collector rejected export with status " <> Text.pack (show statusCode)

-- | Decision (BZ, 2026-08-21, per @docs/design-guidance.md@'s explicit-props
-- rule): the connection-reusing HTTP manager is a caller-owned prop, not a
-- process-global CAF — two applications (or two parallel test suites) in
-- one process would otherwise unavoidably share one manager with no way to
-- substitute their own. See @docs/design-guidance.md@'s
-- \"Follow-up decision — BZ\" for the full record. A caller constructs one
-- via 'newOtlpHttpManager' once and passes it to every 'sendOtlpTraceRequest'
-- call, the same shape 'HarchWeb.Gmail.runGmailHttpRequest' already uses.
newOtlpHttpManager :: IO HttpClient.Manager
newOtlpHttpManager = HttpClient.newManager HttpClientTls.tlsManagerSettings

-- | Posts one OTLP trace payload. A collector response is provider-controlled,
-- so a rejecting status becomes a stable status-only result and its body is
-- discarded before it can reach the application's log reporter. Request
-- parsing and transport exceptions are similarly converted to
-- 'OtlpExportFailure' before an application can log their secret-bearing
-- @http-client@ request representation.
sendOtlpTraceRequest :: HttpClient.Manager -> OtlpExporter -> LazyByteString.ByteString -> IO (Either OtlpExportFailure ())
sendOtlpTraceRequest manager exporter requestBody = do
  requestResult <-
    try (postOtlpTraceRequest manager exporter requestBody) :: IO (Either HttpClient.HttpException Int)
  case requestResult of
    Left requestException -> pure (Left (otlpHttpExceptionFailure requestException))
    Right statusCode ->
      pure $
        if statusCode >= 200 && statusCode < 300
          then Right ()
          else Left (OtlpCollectorRejectedStatus statusCode)

postOtlpTraceRequest :: HttpClient.Manager -> OtlpExporter -> LazyByteString.ByteString -> IO Int
postOtlpTraceRequest manager exporter requestBody = do
  baseRequest <- HttpClient.parseRequest (Text.unpack (otlpEndpoint exporter))
  response <-
    HttpClient.httpNoBody
      baseRequest
        { HttpClient.method = "POST",
          HttpClient.requestHeaders =
            (Http.hContentType, "application/json")
              : map otlpHeader (otlpHeaders exporter),
          HttpClient.requestBody = HttpClient.RequestBodyLBS requestBody
        }
      manager
  pure (Http.statusCode (HttpClient.responseStatus response))

otlpHttpExceptionFailure :: HttpClient.HttpException -> OtlpExportFailure
otlpHttpExceptionFailure = \case
  HttpClient.InvalidUrlException {} -> OtlpInvalidEndpoint
  HttpClient.HttpExceptionRequest {} -> OtlpTransportFailure

currentUnixTimeNSec :: IO Word64
currentUnixTimeNSec =
  floor . (* 1000000000) <$> getPOSIXTime

-- | Decision record (AV): trace and span ids must be unpredictable per the
-- W3C Trace Context spec, not derived from a boot-relative clock reading
-- plus a per-process counter — that scheme let two replicas started at
-- similar uptimes mint identical trace ids and silently merge unrelated
-- traces, and let anyone holding a trace id recompute its span id.
-- 'getEntropy' (the same CSPRNG source already used for session ids,
-- secrets, and TOTP keys elsewhere in this module's package) replaces both
-- halves with independently random bytes.
nextOtlpSpanIdentifiers :: IO (Text, Text)
nextOtlpSpanIdentifiers = do
  traceIdBytes <- getEntropy 16
  spanIdBytes <- getEntropy 8
  pure (otlpIdHexText traceIdBytes, otlpIdHexText spanIdBytes)

nextOtlpSpanId :: IO Text
nextOtlpSpanId = snd <$> nextOtlpSpanIdentifiers

otlpHeader :: (Text, Text) -> Http.Header
otlpHeader (headerName, headerValue) =
  (fromString (Text.unpack headerName), TextEncoding.encodeUtf8 headerValue)

otlpIdHexText :: ByteString.ByteString -> Text
otlpIdHexText =
  TextEncoding.decodeLatin1 . Base16.encode
