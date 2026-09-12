{-# LANGUAGE OverloadedStrings #-}

module WebApi.App.Observability
  ( otlpExportFailureMessage,
    requestObservabilityLogContext,
    runOtlpExportAction,
    runtimeApplicationLogReporter,
    runtimeConnectionObservabilityReporter,
    runtimeRequestObservabilityReporter,
    runtimeRequestObservabilityReporterWithLog,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (TBQueue, isFullTBQueue, newTBQueueIO, readTBQueue, writeTBQueue)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, forever, unless)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import HarchWeb qualified
import HarchWeb.Observability qualified as Observability
import Network.HTTP.Client qualified as HttpClient
import Numeric.Natural (Natural)
import System.IO (hFlush, stderr)
import System.IO.Unsafe (unsafePerformIO)
import WebApi.Config (AppConfig, AppMode (..), observability)

runtimeRequestObservabilityReporter :: AppMode -> AppConfig -> Observability.RequestObservability -> IO ()
runtimeRequestObservabilityReporter =
  runtimeRequestObservabilityReporterWithLog runtimeApplicationLogReporter

-- | Report a runtime request through the ordinary OTLP queue while retaining
-- the application's selected diagnostic sink for an asynchronous export
-- failure.  The queue item carries that sink because its worker is detached
-- from the request and cannot recover it safely from ambient process state.
-- Production uses 'runtimeApplicationLogReporter'; this explicit dependency
-- also lets the real WAI/request/collector path prove its correlation join.
runtimeRequestObservabilityReporterWithLog ::
  (Text.Text -> IO ()) ->
  AppMode ->
  AppConfig ->
  Observability.RequestObservability ->
  IO ()
runtimeRequestObservabilityReporterWithLog reportLog mode config requestObservability =
  case requestObservabilityLogContext requestObservability of
    requestLogContextValue ->
      runtimeObservabilityReporter
        RuntimeObservabilityDependencies
          { runtimeObservabilityReportLog = reportLog,
            runtimeObservabilityMode = mode,
            runtimeObservabilityConfig = config
          }
        requestLogContextValue
        "request observability"
        (HarchWeb.exportRequestObservabilityToOtlp otlpHttpManager "web-api")
        requestObservability

runtimeConnectionObservabilityReporter :: AppMode -> AppConfig -> Observability.ConnectionObservability -> IO ()
runtimeConnectionObservabilityReporter mode config =
  runtimeObservabilityReporter
    RuntimeObservabilityDependencies
      { runtimeObservabilityReportLog = runtimeApplicationLogReporter,
        runtimeObservabilityMode = mode,
        runtimeObservabilityConfig = config
      }
    Nothing
    "connection observability"
    (HarchWeb.exportConnectionObservabilityToOtlp otlpHttpManager "web-api")

-- | The unstructured "TRACE " stderr dump is a local-debugging convenience,
-- not a private structured log: it carries client.address, user_agent, and
-- other per-request PII with no level or config gate. Kept for Development
-- and Test (where CI/local debugging value outweighs the exposure), but
-- suppressed in Production, where it would otherwise print PII for every
-- real request forever with no way to turn it off.
data RuntimeObservabilityDependencies = RuntimeObservabilityDependencies
  { runtimeObservabilityReportLog :: Text.Text -> IO (),
    runtimeObservabilityMode :: AppMode,
    runtimeObservabilityConfig :: AppConfig
  }

runtimeObservabilityReporter ::
  (Show observabilityValue) =>
  RuntimeObservabilityDependencies ->
  Maybe Text.Text ->
  Text.Text ->
  (HarchWeb.OtlpExporter -> observabilityValue -> IO (Either HarchWeb.OtlpExportFailure ())) ->
  observabilityValue ->
  IO ()
runtimeObservabilityReporter dependencies requestLogContextValue observabilityKind exportObservability observabilityValue = do
  -- 'unless's own no-op branch (not a local @pure ()@) is deliberate: a
  -- bare @()@ literal here is a lazy value nothing downstream forces, the
  -- same "genuinely never scrutinized" HPC gap this codebase has hit
  -- before (see the AC decision record in docs/design-guidance.md).
  -- Delegating the no-op to 'Control.Monad.unless' keeps that triviality
  -- inside @base@, outside this project's own coverage boundary, rather
  -- than adding a forced tick for a value with nothing to assert about.
  unless (runtimeObservabilityMode dependencies == Production) (TextIO.hPutStrLn stderr ("TRACE " <> Text.pack (show observabilityValue)))
  forM_ (maybe [] pure (HarchWeb.tracingExporter (observability (runtimeObservabilityConfig dependencies)))) $ \exporter ->
    enqueueOtlpExport
      OtlpExportWork
        { otlpExportReportLog = runtimeObservabilityReportLog dependencies,
          otlpExportRequestLogContext = requestLogContextValue,
          otlpExportKind = observabilityKind,
          otlpExportAction = exportObservability exporter observabilityValue
        }

-- | Decision record (AU, updated BZ 2026-08-21): the request-handling thread
-- must never block on network I/O to the OTLP collector, so
-- 'runtimeObservabilityReporter' hands each export off to this bounded queue
-- instead of awaiting 'exportObservability' itself. A background worker —
-- started once, lazily, via 'unsafePerformIO' \/ 'NOINLINE' — drains the
-- queue and performs the actual blocking POST off the request path. A full
-- queue drops the export and counts it rather than blocking the caller: a
-- slow or hung collector degrades trace completeness, never response
-- latency. This is deliberately an application-layer fix rather than a
-- framework one (per the framework-capability-gap protocol in
-- @docs/design-guidance.md@): 'web-api' is this tree's only caller of
-- @HarchWeb.exportRequestObservabilityToOtlp@\/@exportConnectionObservabilityToOtlp@
-- today, so there is no shared boundary yet to extend. If a second
-- application adopts OTLP export, promote this queue into
-- @HarchWeb.Observability@ instead of duplicating it there. As of BZ,
-- 'otlpHttpManager' below follows this exact same reasoning: it used to be
-- @HarchWeb.Observability.Otlp@'s own global, but a framework module owning
-- ambient mutable state means two applications (or two parallel test
-- suites) in one process unavoidably share it with no way to substitute
-- their own — so ownership moved here, the one real caller, the same place
-- this queue already lives, rather than becoming a second framework-owned
-- global. See @docs/design-guidance.md@'s "Follow-up decision — BZ" for
-- the full record.
data OtlpExportWork = OtlpExportWork
  { otlpExportReportLog :: Text.Text -> IO (),
    otlpExportRequestLogContext :: Maybe Text.Text,
    otlpExportKind :: Text.Text,
    otlpExportAction :: IO (Either HarchWeb.OtlpExportFailure ())
  }

enqueueOtlpExport :: OtlpExportWork -> IO ()
enqueueOtlpExport exportWork = do
  enqueued <- atomically $ do
    full <- isFullTBQueue otlpExportQueue
    unless full (writeTBQueue otlpExportQueue exportWork)
    pure (not full)
  unless enqueued $ do
    droppedTotal <- atomicModifyIORef' otlpExportDroppedCount (\count -> (count + 1, count + 1))
    otlpExportReportLog
      exportWork
      ( withRequestLogContext
          (otlpExportRequestLogContext exportWork)
          (otlpExportQueueFullMessage (otlpExportKind exportWork) droppedTotal)
      )

otlpExportQueueCapacity :: Natural
otlpExportQueueCapacity = 256

otlpExportQueue :: TBQueue OtlpExportWork
{-# NOINLINE otlpExportQueue #-}
otlpExportQueue =
  unsafePerformIO $ do
    queue <- newTBQueueIO otlpExportQueueCapacity
    _ <- forkIO (otlpExportWorker queue)
    pure queue

otlpExportDroppedCount :: IORef Int
{-# NOINLINE otlpExportDroppedCount #-}
otlpExportDroppedCount =
  unsafePerformIO (newIORef 0)

otlpHttpManager :: HttpClient.Manager
{-# NOINLINE otlpHttpManager #-}
otlpHttpManager =
  unsafePerformIO HarchWeb.newOtlpHttpManager

otlpExportWorker :: TBQueue OtlpExportWork -> IO ()
otlpExportWorker queue = forever $ do
  exportWork <- atomically (readTBQueue queue)
  runOtlpExportAction
    (otlpExportReportLog exportWork)
    (otlpExportRequestLogContext exportWork)
    (otlpExportKind exportWork)
    (otlpExportAction exportWork)

-- | Run an OTLP action off the request path, turning both the closed adapter
-- failure result and any unexpected I/O exception into a payload-free log
-- message.  The logger is an explicit dependency so the recovery boundary can
-- be tested without redirecting process-wide stderr.
runOtlpExportAction ::
  (Text.Text -> IO ()) ->
  Maybe Text.Text ->
  Text.Text ->
  IO (Either HarchWeb.OtlpExportFailure ()) ->
  IO ()
runOtlpExportAction reportLog requestLogContextValue observabilityKind exportAction = do
  exportResult <- try exportAction :: IO (Either SomeException (Either HarchWeb.OtlpExportFailure ()))
  case exportResult of
    Left _ ->
      reportLog (withRequestLogContext requestLogContextValue (unexpectedExportFailureMessage observabilityKind))
    Right (Left otlpFailure) ->
      reportLog (withRequestLogContext requestLogContextValue (otlpExportFailureMessage observabilityKind otlpFailure))
    Right (Right ()) -> hFlush stderr

-- | Derive the one authoritative, parseable log field from a framework request
-- observation. Ambiguous or absent attributes deliberately yield no field: an
-- application-provided diagnostic must not be able to choose a request ID.
requestObservabilityLogContext :: Observability.RequestObservability -> Maybe Text.Text
requestObservabilityLogContext requestObservability =
  case [ requestId
       | Observability.ObservabilityAttribute
           { Observability.attributeName = "harch.request.id",
             Observability.attributeValue = Observability.TextAttribute requestId
           } <-
           Observability.requestSpanAttributes (Observability.observabilityRequestSpan requestObservability)
       ] of
    [requestId] -> Just ("request.id=" <> requestId)
    _ -> Nothing

withRequestLogContext :: Maybe Text.Text -> Text.Text -> Text.Text
withRequestLogContext requestLogContextValue message =
  maybe message (<> " " <> message) requestLogContextValue

otlpExportQueueFullMessage :: Text.Text -> Int -> Text.Text
otlpExportQueueFullMessage observabilityKind droppedTotal =
  "Dropped "
    <> observabilityKind
    <> " OTLP export because the export queue is full ("
    <> Text.pack (show droppedTotal)
    <> " dropped total)"

unexpectedExportFailureMessage :: Text.Text -> Text.Text
unexpectedExportFailureMessage observabilityKind =
  "Failed to export "
    <> observabilityKind
    <> " to OTLP: unexpected exporter failure"

-- | The final production log text for a typed OTLP adapter failure. The
-- adapter owns classification before this formatter runs, so the message
-- cannot retain a configured endpoint, header, request, or response payload.
otlpExportFailureMessage :: Text.Text -> HarchWeb.OtlpExportFailure -> Text.Text
otlpExportFailureMessage observabilityKind otlpFailure =
  "Failed to export "
    <> observabilityKind
    <> " to OTLP: "
    <> HarchWeb.renderOtlpExportFailure otlpFailure

runtimeApplicationLogReporter :: Text.Text -> IO ()
runtimeApplicationLogReporter =
  TextIO.hPutStrLn stderr . ("ERROR " <>)
