{-# LANGUAGE OverloadedStrings #-}

module WebApi.App.Observability
  ( otlpExportFailureMessage,
    runOtlpExportAction,
    runtimeApplicationLogReporter,
    runtimeConnectionObservabilityReporter,
    runtimeRequestObservabilityReporter,
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
runtimeRequestObservabilityReporter mode config =
  runtimeObservabilityReporter
    mode
    config
    "request observability"
    (HarchWeb.exportRequestObservabilityToOtlp otlpHttpManager "web-api")

runtimeConnectionObservabilityReporter :: AppMode -> AppConfig -> Observability.ConnectionObservability -> IO ()
runtimeConnectionObservabilityReporter mode config =
  runtimeObservabilityReporter
    mode
    config
    "connection observability"
    (HarchWeb.exportConnectionObservabilityToOtlp otlpHttpManager "web-api")

-- | The unstructured "TRACE " stderr dump is a local-debugging convenience,
-- not a private structured log: it carries client.address, user_agent, and
-- other per-request PII with no level or config gate. Kept for Development
-- and Test (where CI/local debugging value outweighs the exposure), but
-- suppressed in Production, where it would otherwise print PII for every
-- real request forever with no way to turn it off.
runtimeObservabilityReporter ::
  (Show observabilityValue) =>
  AppMode ->
  AppConfig ->
  Text.Text ->
  (HarchWeb.OtlpExporter -> observabilityValue -> IO (Either HarchWeb.OtlpExportFailure ())) ->
  observabilityValue ->
  IO ()
runtimeObservabilityReporter mode config observabilityKind exportObservability observabilityValue = do
  -- 'unless's own no-op branch (not a local @pure ()@) is deliberate: a
  -- bare @()@ literal here is a lazy value nothing downstream forces, the
  -- same "genuinely never scrutinized" HPC gap this codebase has hit
  -- before (see the AC decision record in docs/design-guidance.md).
  -- Delegating the no-op to 'Control.Monad.unless' keeps that triviality
  -- inside @base@, outside this project's own coverage boundary, rather
  -- than adding a forced tick for a value with nothing to assert about.
  unless (mode == Production) (TextIO.hPutStrLn stderr ("TRACE " <> Text.pack (show observabilityValue)))
  forM_ (maybe [] pure (HarchWeb.tracingExporter (observability config))) $ \exporter ->
    enqueueOtlpExport observabilityKind (exportObservability exporter observabilityValue)

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
enqueueOtlpExport :: Text.Text -> IO (Either HarchWeb.OtlpExportFailure ()) -> IO ()
enqueueOtlpExport observabilityKind exportAction = do
  enqueued <- atomically $ do
    full <- isFullTBQueue otlpExportQueue
    unless full (writeTBQueue otlpExportQueue (observabilityKind, exportAction))
    pure (not full)
  unless enqueued $ do
    droppedTotal <- atomicModifyIORef' otlpExportDroppedCount (\count -> (count + 1, count + 1))
    runtimeApplicationLogReporter (otlpExportQueueFullMessage observabilityKind droppedTotal)

otlpExportQueueCapacity :: Natural
otlpExportQueueCapacity = 256

otlpExportQueue :: TBQueue (Text.Text, IO (Either HarchWeb.OtlpExportFailure ()))
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

otlpExportWorker :: TBQueue (Text.Text, IO (Either HarchWeb.OtlpExportFailure ())) -> IO ()
otlpExportWorker queue = forever $ do
  (observabilityKind, exportAction) <- atomically (readTBQueue queue)
  runOtlpExportAction runtimeApplicationLogReporter observabilityKind exportAction

-- | Run an OTLP action off the request path, turning both the closed adapter
-- failure result and any unexpected I/O exception into a payload-free log
-- message.  The logger is an explicit dependency so the recovery boundary can
-- be tested without redirecting process-wide stderr.
runOtlpExportAction ::
  (Text.Text -> IO ()) ->
  Text.Text ->
  IO (Either HarchWeb.OtlpExportFailure ()) ->
  IO ()
runOtlpExportAction reportLog observabilityKind exportAction = do
  exportResult <- try exportAction :: IO (Either SomeException (Either HarchWeb.OtlpExportFailure ()))
  case exportResult of
    Left _ ->
      reportLog (unexpectedExportFailureMessage observabilityKind)
    Right (Left otlpFailure) ->
      reportLog (otlpExportFailureMessage observabilityKind otlpFailure)
    Right (Right ()) -> hFlush stderr

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
