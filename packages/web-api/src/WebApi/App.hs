{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Web-api application composition.
--
-- FQ9 groups the three reporters runtime setup always supplies together in
-- 'RuntimeApplicationReporters'; page, account, policy, and route values
-- remain explicit because they vary per application composition.
-- FQ12 moves account-workflow construction into its own private collaborator:
-- runtime and unavailable workflows must share one process-wide password-work
-- gate, while this module remains the explicit application/site composition
-- boundary.
module WebApi.App
  ( buildAppWithDatabase,
    buildAppWithDatabaseAndAccountWorkflow,
    buildApp,
    buildRuntimeAccountWorkflow,
    buildRuntimeApp,
    buildRuntimeAppWithDatabaseBuilder,
    otlpExportFailureMessage,
    run,
    runWithConfig,
    runtimeRequestObservabilityReporter,
    unavailableAccountWorkflow,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (bracket)
import Data.ByteString qualified as ByteString
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.IO qualified as TextIO
import HarchWeb qualified
import HarchWeb.Action (decodeAction)
import HarchWeb.Observability qualified as Observability
import HarchWeb.Site qualified as Site
import System.Directory (doesFileExist)
import System.IO (Handle, hFlush)
import WebApi.AccountPages (AccountAction, accountActions, authorizeAccountActionCsrf, handleAccountAction, pageCsrfTokenForAccountPage)
import WebApi.Api.Endpoints (secondApiRouteDefinition, statusApiRouteDefinition)
import WebApi.App.AccountWorkflow (buildRuntimeAccountWorkflow, unavailableAccountWorkflow)
import WebApi.App.Observability
  ( otlpExportFailureMessage,
    runtimeApplicationLogReporter,
    runtimeConnectionObservabilityReporter,
    runtimeRequestObservabilityReporter,
  )
import WebApi.App.Shell (appRuntimeAssets, buildAppPageShellConfig)
import WebApi.AppEffect (AccountWorkflow (..))
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
import WebApi.Database (PageRepository, defaultPageRepository)
import WebApi.Postgres.Pool (PostgresPool, closePostgresPool, newPostgresPool)
import WebApi.Postgres.Runtime (buildRuntimePostgresPageRepository)
import WebApi.Response (selectResponseWithDatabaseAndAccountWorkflow)
import WebApi.Route
  ( AppRequestContext (..),
    AppRoute (..),
    defaultRequestContext,
    requestContextFromWaiRequest,
    routeCodec,
  )

buildAppWithDatabase ::
  AppConfig ->
  PageRepository ->
  HarchWeb.Application AppRoute AccountAction AppRequestContext
buildAppWithDatabase config pageRepository =
  buildAppWithDatabaseAndAccountWorkflow config pageRepository unavailableAccountWorkflow

buildAppWithDatabaseAndAccountWorkflow ::
  AppConfig ->
  PageRepository ->
  AccountWorkflow ->
  HarchWeb.Application AppRoute AccountAction AppRequestContext
buildAppWithDatabaseAndAccountWorkflow config pageRepository accountWorkflow =
  buildAppWithDatabaseAndOptionalReporters config pageRepository accountWorkflow Nothing

buildAppWithDatabaseAndReporters ::
  AppConfig ->
  PageRepository ->
  AccountWorkflow ->
  RuntimeApplicationReporters ->
  HarchWeb.Application AppRoute AccountAction AppRequestContext
buildAppWithDatabaseAndReporters config pageRepository !accountWorkflow reporters =
  buildAppWithDatabaseAndOptionalReporters
    config
    pageRepository
    accountWorkflow
    (Just reporters)

data RuntimeApplicationReporters = RuntimeApplicationReporters
  { runtimeApplicationRequestObservabilityReporter :: Observability.RequestObservability -> IO (),
    runtimeApplicationConnectionObservabilityReporter :: Observability.ConnectionObservability -> IO (),
    runtimeApplicationReporterLog :: Text.Text -> IO ()
  }

-- | The ordinary application leaves observability on the framework's default
-- disabled policy. Runtime setup supplies all three concrete reporters
-- together, so no local fake callbacks are needed to bridge the two modes.
buildAppWithDatabaseAndOptionalReporters ::
  AppConfig ->
  PageRepository ->
  AccountWorkflow ->
  Maybe RuntimeApplicationReporters ->
  HarchWeb.Application AppRoute AccountAction AppRequestContext
buildAppWithDatabaseAndOptionalReporters config pageRepository !accountWorkflow maybeReporters =
  Site.buildSiteApplication
    ( configureReporters
        ( ( Site.simpleSite
              Site.SimpleSiteConfiguration
                { Site.simpleSiteName = "web-api",
                  Site.simpleSiteDefaultRequestContext = defaultRequestContext,
                  Site.simpleSiteRouteCodec = routeCodec,
                  Site.simpleSitePageShell = buildAppPageShellConfig config . HarchWeb.pageContext,
                  Site.simpleSiteNavigationRoutes = appNavigationRoutes,
                  Site.simpleSiteRouteDefinition = buildAppRouteDefinition config pageRepository accountWorkflow
                }
          )
            { Site.siteRequestContextFromRequest =
                requestContextFromWaiRequest (requestPolicy config),
              Site.siteStaticAssets = staticAssets config,
              Site.siteRuntimeAssets = appRuntimeAssets,
              Site.siteNavigationRuntimePathPrefix = requestPathPrefix,
              Site.siteRequestPolicy = requestPolicy config,
              Site.siteDecodeClientAction = decodeAction accountActions,
              Site.sitePageCsrfToken = pageCsrfTokenForAccountPage accountWorkflow,
              Site.siteAuthorizeClientActionCsrf = authorizeAccountActionCsrf accountWorkflow,
              Site.siteHandleClientAction = handleAccountAction accountWorkflow
            }
        )
    )
  where
    configureReporters site =
      case maybeReporters of
        Nothing -> site
        Just reporters ->
          site
            { Site.siteReportRequestObservability = runtimeApplicationRequestObservabilityReporter reporters,
              Site.siteReportConnectionObservability = runtimeApplicationConnectionObservabilityReporter reporters,
              Site.siteReportApplicationLog = runtimeApplicationReporterLog reporters
            }

buildApp :: AppConfig -> HarchWeb.Application AppRoute AccountAction AppRequestContext
buildApp config =
  buildAppWithDatabase config defaultPageRepository

appNavigationRoutes :: [AppRoute]
appNavigationRoutes =
  [HomeRoute, SecondRoute, SpacesRoute, RegistrationRoute, LoginRoute, ProfileRoute]

buildAppRouteDefinition ::
  AppConfig ->
  PageRepository ->
  AccountWorkflow ->
  AppRoute ->
  Site.RouteDefinition AppRoute AppRequestContext
buildAppRouteDefinition config pageRepository accountWorkflow route =
  case route of
    StatusApiRoute -> statusApiRouteDefinition
    SecondApiRoute -> secondApiRouteDefinition pageRepository
    _ ->
      Site.RouteDefinition
        { Site.routeNavigationLabel = routeNavigationLabel route,
          Site.routeMethods = HarchWeb.routeMethodPolicyMethods (HarchWeb.routeMethods routeCodec route),
          Site.routeExecutionPolicy = HarchWeb.unboundedRouteExecutionPolicy,
          Site.routeResponse =
            \_ -> selectResponseWithDatabaseAndAccountWorkflow config pageRepository accountWorkflow
        }

routeNavigationLabel :: AppRoute -> Maybe Text.Text
routeNavigationLabel route = lookup route navigationLabels
  where
    navigationLabels =
      [ (HomeRoute, "Home"),
        (SecondRoute, "Second"),
        (SpacesRoute, "Spaces"),
        (RegistrationRoute, "Create account"),
        (LoginRoute, "Sign in"),
        (ProfileRoute, "Profile")
      ]

buildRuntimeApp ::
  PostgresPool ->
  AppConfig ->
  AppEnvironmentConfig ->
  HarchWeb.Application AppRoute AccountAction AppRequestContext
buildRuntimeApp pool config environmentConfig =
  buildAppWithDatabaseAndReporters
    (withPublicBaseUrlRedirectAuthority environmentConfig config)
    (buildRuntimePostgresPageRepository pool)
    (buildRuntimeAccountWorkflow pool environmentConfig)
    (runtimeApplicationReporters environmentConfig config)

buildRuntimeAppWithDatabaseBuilder ::
  AppConfig ->
  (DatabaseConfig -> PageRepository) ->
  AppEnvironmentConfig ->
  HarchWeb.Application AppRoute AccountAction AppRequestContext
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

-- | The HTTPS-upgrade redirect must never echo a client-supplied @Host@
-- header into its target (see 'HarchWeb.httpsRedirectAuthority'). Every
-- web-api deployment already declares a canonical @PUBLIC_BASE_URL@ (used
-- for email links), including a TLS-offloading deployment whose own
-- listeners are HTTP-only and so cannot supply
-- 'WebApi.Config.defaultHttpsRedirectAuthority''s listener-derived guess.
-- Prefer the host parsed from that required setting, falling back to the
-- config-derived guess only if @PUBLIC_BASE_URL@ is malformed.
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
  let runtimeDatabaseConfig = databaseConfig environmentConfig
  bracket
    (newPostgresPool (databasePoolCapacity runtimeDatabaseConfig) runtimeDatabaseConfig)
    closePostgresPool
    ( \pool -> do
        announceParsedListenerConfigs outputHandle appConfig
        HarchWeb.runServer outputHandle appConfig (buildRuntimeApp pool appConfig environmentConfig)
    )

run :: Handle -> IO ()
run outputHandle = do
  configFileStatuses <- loadDefaultStartupConfigFileStatuses
  either throwStartupLoadError (runLoadedStartupConfig outputHandle configFileStatuses) =<< loadAppStartupConfig

throwStartupLoadError :: AppStartupConfigLoadError -> IO ()
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
    runWithConfig outputHandle appConfig environmentConfig

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
