{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module WebApi.App
  ( buildAppWithDatabase,
    buildApp,
    buildRuntimeApp,
    buildRuntimeAppWithDatabaseBuilder,
    run,
    runWithConfig,
  )
where

import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Monad (forM_)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import HarchWeb qualified
import HarchWeb.Observability qualified as Observability
import System.Directory (doesFileExist)
import System.IO (Handle, hFlush, stderr)
import WebApi.App.Shell (buildAppPageShell)
import WebApi.Config
  ( AppConfig (..),
    AppEnvironmentConfig (..),
    AppStartupConfig (..),
    DatabaseConfig,
    ListenerConfig (..),
    ListenerScheme (..),
    loadAppStartupConfig,
  )
import WebApi.Database (DatabaseEffect, defaultDatabaseEffect)
import WebApi.Postgres (buildPostgresDatabaseEffect)
import WebApi.Response (selectResponseWithDatabase)
import WebApi.Route
  ( AppRequestContext,
    AppRoute (..),
    defaultRequestContext,
    requestContextFromWaiRequest,
    routeCodec,
  )

buildAppWithDatabase :: AppConfig -> DatabaseEffect -> HarchWeb.Application AppRoute AppRequestContext
buildAppWithDatabase config databaseEffect =
  buildAppWithDatabaseAndReporters config databaseEffect ignoreRequestObservability ignoreApplicationLog

buildAppWithDatabaseAndReporters ::
  AppConfig ->
  DatabaseEffect ->
  (Observability.RequestObservability -> IO ()) ->
  (Text.Text -> IO ()) ->
  HarchWeb.Application AppRoute AppRequestContext
buildAppWithDatabaseAndReporters config databaseEffect requestObservabilityReporter applicationLogReporter =
  config `seq`
    HarchWeb.application
      HarchWeb.Application
        { HarchWeb.appName = "web-api",
          HarchWeb.defaultRequestContext = defaultRequestContext,
          HarchWeb.requestContextFromRequest = requestContextFromWaiRequest,
          HarchWeb.applicationStaticAssets = staticAssets config,
          HarchWeb.applicationRequestPolicy = requestPolicy config,
          HarchWeb.routeCodec = routeCodec,
          HarchWeb.renderResponse = selectResponseWithDatabase config databaseEffect,
          HarchWeb.pageShell = buildAppPageShell config,
          HarchWeb.reportRequestObservability = requestObservabilityReporter,
          HarchWeb.reportApplicationLog = applicationLogReporter
        }

buildApp :: AppConfig -> HarchWeb.Application AppRoute AppRequestContext
buildApp config =
  buildAppWithDatabase config defaultDatabaseEffect

buildRuntimeApp :: AppConfig -> AppEnvironmentConfig -> HarchWeb.Application AppRoute AppRequestContext
buildRuntimeApp config =
  buildRuntimeAppWithDatabaseBuilder config buildPostgresDatabaseEffect

buildRuntimeAppWithDatabaseBuilder ::
  AppConfig ->
  (DatabaseConfig -> DatabaseEffect) ->
  AppEnvironmentConfig ->
  HarchWeb.Application AppRoute AppRequestContext
buildRuntimeAppWithDatabaseBuilder config buildDatabaseEffect environmentConfig =
  let databaseEffect = buildDatabaseEffect (databaseConfig environmentConfig)
   in databaseEffect `seq`
        buildAppWithDatabaseAndReporters
          config
          databaseEffect
          (runtimeRequestObservabilityReporter config)
          runtimeApplicationLogReporter

runWithConfig :: Handle -> AppConfig -> AppEnvironmentConfig -> IO ()
runWithConfig outputHandle appConfig !environmentConfig = do
  announceParsedListenerConfigs outputHandle appConfig
  HarchWeb.runServer outputHandle appConfig (buildRuntimeApp appConfig environmentConfig)

run :: Handle -> IO ()
run outputHandle = do
  configFileStatuses <- loadDefaultStartupConfigFileStatuses
  startupConfigResult <- loadAppStartupConfig
  either
    (\loadError -> ioError (userError ("Failed to load app startup config: " <> show loadError)))
    ( \AppStartupConfig {startupEnvironmentConfig = environmentConfig, startupAppConfig = appConfig} ->
        announceConfigFileStatuses outputHandle configFileStatuses
          >> runWithConfig outputHandle appConfig environmentConfig
    )
    startupConfigResult

loadDefaultStartupConfigFileStatuses :: IO [(FilePath, Bool)]
loadDefaultStartupConfigFileStatuses =
  traverse
    (\filePath -> (filePath,) <$> doesFileExist filePath)
    [".env", ".env.local"]

announceConfigFileStatuses :: Handle -> [(FilePath, Bool)] -> IO ()
announceConfigFileStatuses outputHandle configFileStatuses = do
  mapM_ (TextIO.hPutStrLn outputHandle . renderConfigFileStatus) configFileStatuses
  hFlush outputHandle
  where
    renderConfigFileStatus (filePath, fileExists) =
      if fileExists
        then "Loaded config file: ./" <> Text.pack filePath
        else "Config file missing: ./" <> Text.pack filePath

announceParsedListenerConfigs :: Handle -> AppConfig -> IO ()
announceParsedListenerConfigs outputHandle appConfig = do
  mapM_ (TextIO.hPutStrLn outputHandle . renderParsedListenerConfig) (listenerConfigs appConfig)
  hFlush outputHandle
  where
    renderParsedListenerConfig listenerConfig =
      "Parsed listener config: "
        <> listenerUrlPrefix (listenerScheme listenerConfig)
        <> listenerHost listenerConfig
        <> ":"
        <> Text.pack (show (listenerPort listenerConfig))

listenerUrlPrefix :: ListenerScheme -> Text.Text
listenerUrlPrefix listenerScheme =
  case listenerScheme of
    Http -> "http://"
    Https -> "https://"

runtimeRequestObservabilityReporter :: AppConfig -> Observability.RequestObservability -> IO ()
runtimeRequestObservabilityReporter config requestObservability = do
  TextIO.hPutStrLn stderr ("TRACE " <> Text.pack (show requestObservability))
  forM_ (maybe [] pure (HarchWeb.tracingExporter (observability config))) $ \exporter -> do
    exportResult <-
      try
        (HarchWeb.exportRequestObservabilityToOtlp "web-api" exporter requestObservability) ::
        IO (Either SomeException ())
    case exportResult of
      Left exportError ->
        runtimeApplicationLogReporter
          ("Failed to export request observability to OTLP: " <> Text.pack (displayException exportError))
      Right () ->
        hFlush stderr

runtimeApplicationLogReporter :: Text.Text -> IO ()
runtimeApplicationLogReporter =
  TextIO.hPutStrLn stderr . ("ERROR " <>)

ignoreRequestObservability :: Observability.RequestObservability -> IO ()
ignoreRequestObservability requestObservability =
  let ignored = mempty :: ()
   in Observability.forceRequestObservability requestObservability `seq` ignored `seq` evaluate ignored

ignoreApplicationLog :: Text.Text -> IO ()
ignoreApplicationLog logEntry =
  let ignored = mempty :: ()
   in Text.length logEntry `seq` ignored `seq` evaluate ignored
