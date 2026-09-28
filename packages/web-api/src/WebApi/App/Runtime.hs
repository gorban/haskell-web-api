{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Runtime-server and startup configuration for the web-api reference
-- application. 'WebApi.App' owns the typed site composition; this module
-- supplies its concrete PostgreSQL, JWT, observability, and listener runtime.
--
-- Keeping the server lifecycle here leaves route definitions and application
-- security assembly in one place, while configuration loading, resource
-- acquisition, and listener startup remain owned by the runtime boundary.
module WebApi.App.Runtime
  ( buildRuntimeAppWithAccountJwt,
    buildRuntimeAppWithDatabaseBuilder,
    run,
    runWithConfig,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (bracket)
import Data.ByteString qualified as ByteString
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.IO qualified as TextIO
import HarchWeb qualified
import HarchWeb.OpenApi.Swagger (swaggerUiAssetsRoot)
import System.Directory (doesFileExist)
import System.IO (Handle, hFlush)
import WebApi.AccountJwt (AccountJwtLoadError, AccountJwtRuntime, loadAccountJwtRuntime)
import WebApi.AccountPages (AccountAction)
import WebApi.App
  ( RuntimeApplicationReporters (..),
    buildAppWithDatabaseAndReporters,
    buildAppWithDatabaseAndReportersAndSecurity,
    runtimeAuthenticationProfiles,
  )
import WebApi.App.AccountWorkflow (buildRuntimeAccountWorkflowWithJwtRuntime, unavailableAccountWorkflow)
import WebApi.App.Observability
  ( runtimeApplicationLogReporter,
    runtimeConnectionObservabilityReporter,
    runtimeRequestObservabilityReporter,
  )
import WebApi.Config
  ( AppConfig (..),
    AppEnvironmentConfig (..),
    AppStartupConfig (..),
    AppStartupConfigLoadError,
    DatabaseConfig,
    ListenerConfig (..),
    ListenerScheme (..),
    databasePoolCapacity,
    loadAppStartupConfig,
  )
import WebApi.Database (PageRepository)
import WebApi.Postgres.Pool (PostgresPool, closePostgresPool, newPostgresPool)
import WebApi.Postgres.Runtime (buildRuntimePostgresPageRepository)
import WebApi.Route (AppAuthorization, AppRequestContext, AppRoute)

-- | The production server path supplies the immutable startup-validated JWT
-- runtime. Test applications can keep using the explicit composition
-- functions in 'WebApi.App' without loading key files as a side effect.
buildRuntimeAppWithAccountJwt ::
  PostgresPool ->
  AppConfig ->
  AppEnvironmentConfig ->
  AccountJwtRuntime ->
  HarchWeb.Application AppRoute AccountAction AppRequestContext AppAuthorization
buildRuntimeAppWithAccountJwt pool config environmentConfig jwtRuntime =
  buildAppWithDatabaseAndReportersAndSecurity
    (withPublicBaseUrlRedirectAuthority environmentConfig config)
    (buildRuntimePostgresPageRepository pool)
    accountWorkflow
    (runtimeApplicationReporters environmentConfig config)
    (runtimeAuthenticationProfiles accountWorkflow jwtRuntime)
  where
    -- Construct this record at startup so its validated JWT runtime cannot
    -- remain deferred until the first successful login.
    !accountWorkflow = buildRuntimeAccountWorkflowWithJwtRuntime pool environmentConfig (Just jwtRuntime)

-- | Build a runtime application around an adapter-selected page repository.
-- This is the config-driven counterpart to 'buildRuntimeAppWithAccountJwt'
-- used by runtime and configuration tests.
buildRuntimeAppWithDatabaseBuilder ::
  AppConfig ->
  (DatabaseConfig -> PageRepository) ->
  AppEnvironmentConfig ->
  HarchWeb.Application AppRoute AccountAction AppRequestContext AppAuthorization
buildRuntimeAppWithDatabaseBuilder config buildPageRepository environmentConfig =
  let pageRepository = buildPageRepository (databaseConfig environmentConfig)
   in buildAppWithDatabaseAndReporters
        (withPublicBaseUrlRedirectAuthority environmentConfig config)
        pageRepository
        unavailableAccountWorkflow
        (runtimeApplicationReporters environmentConfig config)

runtimeApplicationReporters :: AppEnvironmentConfig -> AppConfig -> RuntimeApplicationReporters
runtimeApplicationReporters environmentConfig config =
  RuntimeApplicationReporters
    { runtimeApplicationRequestObservabilityReporter = runtimeRequestObservabilityReporter (appMode environmentConfig) config,
      runtimeApplicationConnectionObservabilityReporter = runtimeConnectionObservabilityReporter (appMode environmentConfig) config,
      runtimeApplicationReporterLog = runtimeApplicationLogReporter
    }

-- | Use the configured public origin for HTTPS-upgrade redirects so the
-- response never echoes an untrusted request @Host@ value.
withPublicBaseUrlRedirectAuthority :: AppEnvironmentConfig -> AppConfig -> AppConfig
withPublicBaseUrlRedirectAuthority !environmentConfig config =
  config
    { requestPolicy =
        (requestPolicy config)
          { HarchWeb.httpsRedirectAuthority =
              authorityFromPublicBaseUrl (publicBaseUrl environmentConfig)
                <|> HarchWeb.httpsRedirectAuthority (requestPolicy config)
          }
    }

authorityFromPublicBaseUrl :: Text.Text -> Maybe ByteString.ByteString
authorityFromPublicBaseUrl baseUrl =
  case Text.stripPrefix "https://" baseUrl <|> Text.stripPrefix "http://" baseUrl of
    Nothing -> Nothing
    Just afterScheme ->
      let authority = Text.takeWhile (\character -> character /= '/' && character /= '?' && character /= '#') afterScheme
          host = Text.takeWhile (/= ':') authority
       in if Text.null host then Nothing else Just (TextEncoding.encodeUtf8 host)

runWithConfig :: Handle -> AppConfig -> AppEnvironmentConfig -> IO ()
runWithConfig outputHandle appConfig !environmentConfig = do
  jwtRuntimeResult <- loadAccountJwtRuntime (accountJwtConfiguration environmentConfig)
  jwtRuntime <- either throwAccountJwtLoadError pure jwtRuntimeResult
  let runtimeDatabaseConfig = databaseConfig environmentConfig
  bracket
    (newPostgresPool (databasePoolCapacity runtimeDatabaseConfig) runtimeDatabaseConfig)
    closePostgresPool
    ( \pool -> do
        announceParsedListenerConfigs outputHandle appConfig
        HarchWeb.runServer outputHandle appConfig (buildRuntimeAppWithAccountJwt pool appConfig environmentConfig jwtRuntime)
    )

throwAccountJwtLoadError :: AccountJwtLoadError -> IO value
throwAccountJwtLoadError loadError =
  ioError (userError ("Failed to load account JWT configuration: " <> show loadError))

run :: Handle -> IO ()
run outputHandle = do
  configFileStatuses <- loadDefaultStartupConfigFileStatuses
  either throwStartupLoadError (runLoadedStartupConfig outputHandle configFileStatuses) =<< loadAppStartupConfig

throwStartupLoadError :: AppStartupConfigLoadError -> IO value
throwStartupLoadError loadError =
  ioError (userError ("Failed to load app startup config: " <> show loadError))

runLoadedStartupConfig :: Handle -> [(FilePath, Bool)] -> AppStartupConfig -> IO ()
runLoadedStartupConfig
  outputHandle
  configFileStatuses
  AppStartupConfig
    { startupEnvironmentConfig = environmentConfig,
      startupAppConfig = appConfig
    } = do
    announceConfigFileStatuses outputHandle configFileStatuses
    swaggerAssetsRoot <- swaggerUiAssetsRoot "/docs/assets"
    let appConfigWithDocs =
          appConfig
            { staticAssets =
                (staticAssets appConfig)
                  { HarchWeb.staticAssetRoots = HarchWeb.staticAssetRoots (staticAssets appConfig) <> [swaggerAssetsRoot]
                  }
            }
    runWithConfig outputHandle appConfigWithDocs environmentConfig

loadDefaultStartupConfigFileStatuses :: IO [(FilePath, Bool)]
loadDefaultStartupConfigFileStatuses =
  traverse
    ( \filePath -> do
        fileExists <- doesFileExist filePath
        pure (filePath, fileExists)
    )
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
