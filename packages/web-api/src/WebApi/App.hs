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

import HarchWeb qualified
import System.IO (Handle)
import WebApi.Config
  ( AppConfig (..),
    AppEnvironmentConfig (..),
    DatabaseConfig,
    defaultAppConfig,
    loadAppEnvironmentConfig,
  )
import WebApi.Database (DatabaseEffect, defaultDatabaseEffect)
import WebApi.PageShell (buildAppPageShell)
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
  config `seq`
    HarchWeb.application
      HarchWeb.Application
        { HarchWeb.appName = "web-api",
          HarchWeb.defaultRequestContext = defaultRequestContext,
          HarchWeb.applicationStaticAssets = staticAssets config,
          HarchWeb.routeCodec = routeCodec,
          HarchWeb.renderResponse = selectResponseWithDatabase config databaseEffect,
          HarchWeb.pageShell = buildAppPageShell config
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
   in databaseEffect `seq` buildAppWithDatabase config databaseEffect

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
