{-# LANGUAGE OverloadedStrings #-}

-- | Runtime-owned OTLP reporting installed by 'HarchWeb.Server.runServer'.
--
-- Decision (configured OTLP runtime export, 2026-09-14): extend the existing
-- 'Application' reporters at server startup.  The application remains the
-- source of its normal reports and diagnostic sink; this adapter calls those
-- reporters first, then best-effort exports the framework's already-safe span
-- projection through one startup-owned HTTP manager.  It does not add WAI
-- middleware or another dispatcher.  Export, timeout, and logger failures are
-- swallowed after the HTTP response path and render only stable diagnostics.
-- The manager is retained only by the running-server continuation and the
-- installed @http-client@ version finalizes it once that scope is unreachable;
-- its deprecated explicit closer is deliberately not used.  Metrics are
-- rejected before listeners start because this runtime has no complete metrics
-- wire encoding yet.
module HarchWeb.Server.RuntimeObservability
  ( withRuntimeObservability,
  )
where

import Control.Exception (SomeAsyncException, SomeException, fromException, tryJust)
import Data.Text qualified as Text
import HarchWeb.Observability qualified as Observability
import HarchWeb.Server.Application (Application (..))
import System.Timeout (timeout)

withRuntimeObservability ::
  Application route action context authorization ->
  Observability.ObservabilityConfig ->
  (Application route action context authorization -> IO result) ->
  IO result
withRuntimeObservability webApplication config continue =
  case Observability.metricsExporter config of
    Just _ -> ioError (userError "Unsupported observability configuration: OTLP metrics export is not implemented.")
    Nothing ->
      case Observability.tracingExporter config of
        Nothing -> continue webApplication
        Just exporter -> do
          manager <- Observability.newOtlpHttpManager
          continue
            webApplication
              { reportRequestObservability = \requestObservability -> do
                  reportRequestObservability webApplication requestObservability
                  exportSafely webApplication "request observability" (Observability.exportRequestObservabilityToOtlp manager (appName webApplication) exporter requestObservability),
                reportConnectionObservability = \connectionObservability -> do
                  reportConnectionObservability webApplication connectionObservability
                  exportSafely webApplication "connection observability" (Observability.exportConnectionObservabilityToOtlp manager (appName webApplication) exporter connectionObservability)
              }

exportTimeoutMicroseconds :: Int
exportTimeoutMicroseconds = 2000000

reportDiagnosticSafely :: IO () -> IO ()
reportDiagnosticSafely report = do
  _ <- trySynchronous report
  pure ()

exportSafely ::
  Application route action context authorization ->
  Text.Text ->
  IO (Either Observability.OtlpExportFailure ()) ->
  IO ()
exportSafely webApplication signal exportAction = do
  exportResult <- trySynchronous (timeout exportTimeoutMicroseconds exportAction)
  let diagnostic =
        case exportResult of
          Left _ -> "Failed to export " <> signal <> " to OTLP: unexpected exporter failure"
          Right Nothing -> "Failed to export " <> signal <> " to OTLP: export timed out"
          Right (Just (Left failure)) -> "Failed to export " <> signal <> " to OTLP: " <> Observability.renderOtlpExportFailure failure
          Right (Just (Right ())) -> ""
  if Text.null diagnostic
    then pure ()
    else reportDiagnosticSafely (reportApplicationLog webApplication diagnostic)

trySynchronous :: IO value -> IO (Either SomeException value)
trySynchronous = tryJust synchronousException

synchronousException :: SomeException -> Maybe SomeException
synchronousException exception =
  case fromException exception :: Maybe SomeAsyncException of
    Just _ -> Nothing
    Nothing -> Just exception
