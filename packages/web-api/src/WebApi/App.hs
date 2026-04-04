{-# LANGUAGE OverloadedStrings #-}

module WebApi.App
  ( buildAppWithDatabase,
    buildApp,
    buildRuntimeApp,
    buildRuntimeAppWithDatabaseBuilder,
    run,
    runWithEnvironmentConfig,
  )
where

import Control.Exception (evaluate)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import HarchWeb qualified
import HarchWeb.Observability qualified as Observability
import System.IO (Handle, stderr)
import WebApi.App.Shell (buildAppPageShell)
import WebApi.Config
  ( AppConfig (..),
    AppEnvironmentConfig (..),
    DatabaseConfig,
    defaultAppConfig,
    loadAppEnvironmentConfig,
  )
import WebApi.Database (DatabaseEffect, defaultDatabaseEffect)
import WebApi.Postgres (buildPostgresDatabaseEffect)
import WebApi.Response (selectResponseWithDatabase)
import WebApi.Route
  ( AppRequestContext,
    AppRoute (..),
    defaultRequestContext,
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
          HarchWeb.applicationStaticAssets = staticAssets config,
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
          runtimeRequestObservabilityReporter
          runtimeApplicationLogReporter

runWithEnvironmentConfig :: Handle -> AppEnvironmentConfig -> IO ()
runWithEnvironmentConfig outputHandle =
  HarchWeb.runServer outputHandle defaultAppConfig . buildRuntimeApp defaultAppConfig

run :: Handle -> IO ()
run outputHandle = do
  environmentConfigResult <- loadAppEnvironmentConfig
  either
    (\loadError -> ioError (userError ("Failed to load app environment config: " <> show loadError)))
    (runWithEnvironmentConfig outputHandle)
    environmentConfigResult

runtimeRequestObservabilityReporter :: Observability.RequestObservability -> IO ()
runtimeRequestObservabilityReporter =
  TextIO.hPutStrLn stderr . ("TRACE " <>) . Text.pack . show

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
