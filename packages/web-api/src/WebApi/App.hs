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
--
-- AHI-5 extends that boundary through 'Site.siteAttachRouteObservation': the
-- root attaches declared endpoint facts only after typed route selection,
-- rather than deriving audit attribution from a URL or action input.  This is
-- the trusted-context handoff consumed by the application-owned atomic
-- account-session/audit operation.  The generic session port remains
-- available for ordinary session lifecycle operations; login uses the
-- narrower operation so it cannot commit the session without its required
-- audit activity. Other selected audit-producing mutations remain AHI-5
-- follow-up work.
--
-- Decision record (AHI-4D slice 2, 2026-09-13): production composition uses
-- the root-owned public/account profile registry. Only protected account page
-- and action declarations select the cookie-or-bearer JWT guard. The action
-- CSRF selector receives that resolved declaration and its established source
-- fact, so only bearer-only account requests omit CSRF; cookie and dual-source
-- requests retain it. This extends the existing post-match/action lifecycle
-- rather than adding a token-specific middleware or route matcher.
--
-- Decision record (AHI-4D slice 5, 2026-09-17): every 'HarchWeb.Application'
-- and 'HarchWeb.ApplicationSecurity' signature here now carries
-- 'WebApi.Route.AppAuthorization' instead of @()@, ahead of the combined
-- account-or-API-client-bearer profile that will first construct
-- 'HarchWeb.RequireAuthorized'; see the widening decision record in
-- @docs\/design-guidance.md@. That same commit named this module as now
-- marginally over this document's module-health line\/import threshold
-- (501 lines, 27 imports); no split is done here, see that record for the
-- named follow-up.
--
-- Decision record (AHI-4D slice 5, 2026-09-17): 'runtimeAuthenticationProfiles'
-- registers a third profile, 'WebApi.Route.resourceAuthenticationProfileName',
-- built from 'WebApi.ResourceAuthentication.resourceAuthenticationPipeline'
-- and reusing this module's already-wired account session store/clock plus
-- the durable API-client store already owned by
-- 'WebApi.AppEffect.accountWorkflowApiClientTokenEnvironment' (the same store
-- 'WebApi.Api.Endpoints.tokenApiRouteDefinition' issues bearer tokens
-- against). It secures @GET \/api\/second@; see the full decision record in
-- @docs\/design-guidance.md@. The API-client store argument here stayed
-- unforced under HPC until 'Unit.WebApi.AppSpec' replayed a real minted
-- bearer token against @GET \/api\/second@ on the composed runtime
-- application; see that document's coverage-gap finding for the same
-- decision record.
module WebApi.App
  ( buildAppWithDatabase,
    buildAppWithDatabaseAndAccountWorkflow,
    buildAppWithDatabaseAndAccountWorkflowAndSecurity,
    runtimeAuthenticationProfiles,
    buildApp,
    buildRuntimeAccountWorkflow,
    buildRuntimeAccountWorkflowWithJwt,
    buildRuntimeAccountWorkflowWithJwtRuntime,
    buildRuntimeAppWithAccountJwt,
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
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.IO qualified as TextIO
import HarchWeb qualified
import HarchWeb.Action (decodeAction)
import HarchWeb.Observability qualified as Observability
import HarchWeb.Site qualified as Site
import Network.HTTP.Types qualified as Http
import System.Directory (doesFileExist)
import System.IO (Handle, hFlush)
import WebApi.AccountJwt (AccountJwtLoadError, AccountJwtRuntime, accountJwtAuthenticationPipeline, loadAccountJwtRuntime)
import WebApi.AccountPages (AccountAction, accountActionEndpointMetadata, accountActionRoute, accountActions, accountCsrfProtection, handleAccountAction)
import WebApi.Api.Endpoints (meApiRouteDefinition, secondApiRouteDefinition, statusApiRouteDefinition, tokenApiRouteDefinition)
import WebApi.ApiClientToken qualified as ApiClientToken
import WebApi.App.AccountWorkflow (buildRuntimeAccountWorkflow, buildRuntimeAccountWorkflowWithJwt, buildRuntimeAccountWorkflowWithJwtRuntime, unavailableAccountWorkflow)
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
import WebApi.ResourceAuthentication qualified as ResourceAuthentication
import WebApi.Response (apiNotFoundResponse, renderLocale, selectResponseWithDatabaseAndAccountWorkflow, todoLocation)
import WebApi.Route
  ( AppAuthorization,
    AppRequestContext (..),
    AppRoute (..),
    RequestAuthenticationTransport (..),
    accountAuthenticationProfileName,
    defaultRequestContext,
    endpointMetadata,
    requestContextFromWaiRequest,
    resourceAuthenticationProfileName,
    routeCodec,
  )

buildAppWithDatabase ::
  AppConfig ->
  PageRepository ->
  HarchWeb.Application AppRoute AccountAction AppRequestContext AppAuthorization
buildAppWithDatabase config pageRepository =
  buildAppWithDatabaseAndAccountWorkflow config pageRepository unavailableAccountWorkflow

buildAppWithDatabaseAndAccountWorkflow ::
  AppConfig ->
  PageRepository ->
  AccountWorkflow ->
  HarchWeb.Application AppRoute AccountAction AppRequestContext AppAuthorization
buildAppWithDatabaseAndAccountWorkflow config pageRepository accountWorkflow =
  buildAppWithDatabaseAndOptionalReporters config pageRepository accountWorkflow Nothing

-- | Compose a supplied application workflow with an explicit endpoint-security
-- policy. This is the pluggable assembly point for embedders and test
-- applications; the production server uses 'buildRuntimeAppWithAccountJwt'
-- so it can only start with startup-validated key material and durable
-- principal establishment.
buildAppWithDatabaseAndAccountWorkflowAndSecurity ::
  AppConfig ->
  PageRepository ->
  AccountWorkflow ->
  HarchWeb.ApplicationSecurity AppRoute AppRequestContext AppAuthorization ->
  HarchWeb.Application AppRoute AccountAction AppRequestContext AppAuthorization
buildAppWithDatabaseAndAccountWorkflowAndSecurity config pageRepository accountWorkflow =
  buildAppWithDatabaseAndOptionalReportersAndSecurity
    config
    pageRepository
    accountWorkflow
    Nothing

buildAppWithDatabaseAndReporters ::
  AppConfig ->
  PageRepository ->
  AccountWorkflow ->
  RuntimeApplicationReporters ->
  HarchWeb.Application AppRoute AccountAction AppRequestContext AppAuthorization
buildAppWithDatabaseAndReporters config pageRepository !accountWorkflow reporters =
  buildAppWithDatabaseAndOptionalReporters
    config
    pageRepository
    accountWorkflow
    (Just reporters)

buildAppWithDatabaseAndReportersAndSecurity ::
  AppConfig ->
  PageRepository ->
  AccountWorkflow ->
  RuntimeApplicationReporters ->
  HarchWeb.ApplicationSecurity AppRoute AppRequestContext AppAuthorization ->
  HarchWeb.Application AppRoute AccountAction AppRequestContext AppAuthorization
buildAppWithDatabaseAndReportersAndSecurity config pageRepository !accountWorkflow reporters =
  buildAppWithDatabaseAndOptionalReportersAndSecurity
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
  HarchWeb.Application AppRoute AccountAction AppRequestContext AppAuthorization
buildAppWithDatabaseAndOptionalReporters config pageRepository !accountWorkflow maybeReporters =
  buildAppWithDatabaseAndOptionalReportersAndSecurity
    config
    pageRepository
    accountWorkflow
    maybeReporters
    (HarchWeb.AuthenticationDisabled [])

buildAppWithDatabaseAndOptionalReportersAndSecurity ::
  AppConfig ->
  PageRepository ->
  AccountWorkflow ->
  Maybe RuntimeApplicationReporters ->
  HarchWeb.ApplicationSecurity AppRoute AppRequestContext AppAuthorization ->
  HarchWeb.Application AppRoute AccountAction AppRequestContext AppAuthorization
buildAppWithDatabaseAndOptionalReportersAndSecurity config pageRepository !accountWorkflow maybeReporters applicationSecurity =
  ( Site.buildSiteApplication
      ( configureReporters
          ( ( Site.simpleSite
                Site.SimpleSiteConfiguration
                  { Site.simpleSiteName = "web-api",
                    Site.simpleSiteDefaultRequestContext = defaultRequestContext,
                    Site.simpleSiteRouteCodec = routeCodec,
                    Site.simpleSiteSecurity = applicationSecurity,
                    Site.simpleSiteCsrfProtection = accountCsrfProtection accountWorkflow,
                    Site.simpleSitePageShell = buildAppPageShellConfig config . HarchWeb.pageContext,
                    Site.simpleSiteNavigationRoutes = appNavigationRoutes,
                    Site.simpleSiteRouteDefinition = buildAppRouteDefinition config pageRepository accountWorkflow
                  }
            )
              { Site.siteRequestContextFromRequest =
                  requestContextFromWaiRequest (requestPolicy config),
                -- Decision (AHI-5, 2026-09-08): reuse Site's existing
                -- post-match attribution boundary.  The root derives audit
                -- route facts from declared metadata and locale, never a URL
                -- or client-submitted value.
                Site.siteAttachRouteObservation = \_ metadata requestContext ->
                  requestContext
                    { requestRouteObservation =
                        Just
                          ( HarchWeb.rootRouteObservation
                              (HarchWeb.requiredModuleNameOrDie "web-api")
                              (appRequestLocale (requestLocale requestContext))
                              (HarchWeb.endpointName metadata)
                              (HarchWeb.endpointRouteTemplate metadata)
                          )
                    },
                -- Public web traffic never inherits a caller-supplied request
                -- correlation ID. A production service adapter must establish
                -- service identity and its separate propagation capability.
                Site.siteRequestIdIngress = HarchWeb.freshRequestIdIngress,
                Site.siteStaticAssets = staticAssets config,
                Site.siteRuntimeAssets = appRuntimeAssets,
                Site.siteNavigationRuntimePathPrefix = requestPathPrefix,
                Site.siteRequestPolicy = requestPolicy config,
                Site.siteDecodeClientAction = decodeAction accountActions,
                Site.siteClientActionEndpointMetadata = accountActionEndpointMetadata,
                Site.siteClientActionRoute = accountActionRoute,
                Site.siteHandleClientAction = fmap (fmap HarchWeb.ClientActionSucceeded) . handleAccountAction accountWorkflow
              }
          )
      )
  )
    { HarchWeb.clientActionCsrfRequirement = clientActionCsrfRequirementFor
    }
  where
    appRequestLocale = HarchWeb.locale . renderLocale

    -- A cookie (including the same JWT also supplied as bearer) remains an
    -- ambient browser credential. Only the source-aware post-match guard can
    -- establish the bearer-only state that omits the CSRF transport check.
    clientActionCsrfRequirementFor selectedMetadata requestContext =
      case (selectedMetadata >>= HarchWeb.endpointAuthenticationProfile, requestAuthenticationTransport requestContext) of
        (Just profileName, AccountJwtFromBearer)
          | profileName == accountAuthenticationProfileName -> HarchWeb.ClientActionCsrfNotRequired
        _ -> HarchWeb.ClientActionCsrfRequired

    configureReporters site =
      case maybeReporters of
        Nothing -> site
        Just reporters ->
          site
            { Site.siteReportRequestObservability = runtimeApplicationRequestObservabilityReporter reporters,
              Site.siteReportConnectionObservability = runtimeApplicationConnectionObservabilityReporter reporters,
              Site.siteReportApplicationLog = runtimeApplicationReporterLog reporters
            }

buildApp :: AppConfig -> HarchWeb.Application AppRoute AccountAction AppRequestContext AppAuthorization
buildApp config =
  buildAppWithDatabase config defaultPageRepository

appNavigationRoutes :: [AppRoute]
appNavigationRoutes =
  [HomeRoute, SecondRoute, TodoRoute, RegistrationRoute, LoginRoute, ProfileRoute]

buildAppRouteDefinition ::
  AppConfig ->
  PageRepository ->
  AccountWorkflow ->
  AppRoute ->
  Site.RouteDefinition AppRoute AppRequestContext AppAuthorization
buildAppRouteDefinition config pageRepository accountWorkflow route =
  case route of
    StatusApiRoute -> statusApiRouteDefinition
    SecondApiRoute -> secondApiRouteDefinition pageRepository
    MeApiRoute -> meApiRouteDefinition (accountWorkflowProfileStore accountWorkflow)
    TokenApiRoute -> tokenApiRouteDefinition (accountWorkflowApiClientTokenEnvironment accountWorkflow)
    HomeRoute ->
      protocolRouteDefinition route $
        \routeRequest -> pure (HarchWeb.nonPageRedirectResponse Http.status302 (todoLocation routeRequest))
    ApiNotFoundRoute ->
      protocolRouteDefinition route $
        \_ ->
          pure (HarchWeb.NonPageBodyResponse apiNotFoundResponse)
    _ ->
      Site.RouteDefinition
        { Site.routeNavigationLabel = routeNavigationLabel route,
          Site.routeMetadata = endpointMetadata route,
          Site.routeMethods = HarchWeb.routeMethodPolicyMethods (HarchWeb.routeMethods routeCodec (HarchWeb.RouteRequest route defaultRequestContext)),
          Site.routeExecutionPolicy = HarchWeb.unboundedRouteExecutionPolicy,
          Site.routeHandler = Site.PageRouteHandler $
            \_ -> selectResponseWithDatabaseAndAccountWorkflow config pageRepository accountWorkflow
        }

protocolRouteDefinition :: AppRoute -> (HarchWeb.RouteRequest AppRoute AppRequestContext -> IO (HarchWeb.NonPageResponse AppRoute AppRequestContext)) -> Site.RouteDefinition AppRoute AppRequestContext AppAuthorization
protocolRouteDefinition route renderProtocol =
  Site.RouteDefinition
    { Site.routeNavigationLabel = routeNavigationLabel route,
      Site.routeMetadata = endpointMetadata route,
      Site.routeMethods = HarchWeb.routeMethodPolicyMethods (HarchWeb.routeMethods routeCodec (HarchWeb.RouteRequest route defaultRequestContext)),
      Site.routeExecutionPolicy = HarchWeb.unboundedRouteExecutionPolicy,
      Site.routeHandler = Site.ProtocolRouteHandler (const renderProtocol)
    }

routeNavigationLabel :: AppRoute -> Maybe Text.Text
routeNavigationLabel route = lookup route navigationLabels
  where
    navigationLabels =
      [ (HomeRoute, "Home"),
        (SecondRoute, "Second"),
        (TodoRoute, "TODO"),
        (RegistrationRoute, "Create account"),
        (LoginRoute, "Sign in"),
        (ProfileRoute, "Profile")
      ]

-- | The runnable server path supplies the immutable startup-validated JWT
-- runtime. Keeping the legacy three-argument builder available lets storage
-- and observability tests assemble an application whose login issuer is
-- deliberately unavailable, rather than loading key files as a test side
-- effect.
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
    -- The selected issuer is a strict field of 'AccountWorkflow': construct
    -- the record now so application startup cannot defer that validated
    -- security dependency until the first successful login.
    !accountWorkflow = buildRuntimeAccountWorkflowWithJwtRuntime pool environmentConfig (Just jwtRuntime)

-- | The root keeps public operation as its explicit default and gives only
-- account declarations the JWT guard. The declaration names are statically validated, distinct literals; no request
-- can select a credential parser by a path or header value.
--
-- Exported alongside 'buildAppWithDatabaseAndAccountWorkflowAndSecurity' so a
-- test can compose a real, guard-enabled application whose durable stores
-- are otherwise unavailable-by-construction test doubles: only pairing a
-- genuinely deployed 'ApplicationSecurity' with a broken store reaches a
-- handler's own failure-response argument through the real dispatcher, the
-- same requirement 'WebApi.Api.Endpoints.tokenApiFailureResponse's and
-- 'WebApi.Api.Endpoints.meApiFailureResponse's own coverage already needed.
runtimeAuthenticationProfiles :: AccountWorkflow -> AccountJwtRuntime -> HarchWeb.ApplicationSecurity AppRoute AppRequestContext AppAuthorization
runtimeAuthenticationProfiles accountWorkflow jwtRuntime =
  HarchWeb.AuthenticationProfiles
    []
    ( HarchWeb.mkAuthenticationProfile publicAuthenticationProfileName Nothing
        :| [ HarchWeb.mkAuthenticationProfile
               accountAuthenticationProfileName
               ( Just
                   ( HarchWeb.authenticationGuardFromPipeline
                       ( accountJwtAuthenticationPipeline
                           (accountWorkflowSessionStore accountWorkflow)
                           (accountWorkflowClock accountWorkflow)
                           jwtRuntime
                       )
                   )
               ),
             HarchWeb.mkAuthenticationProfile
               resourceAuthenticationProfileName
               ( Just
                   ( HarchWeb.authenticationGuardFromPipeline
                       ( ResourceAuthentication.resourceAuthenticationPipeline
                           (accountWorkflowSessionStore accountWorkflow)
                           (accountWorkflowClock accountWorkflow)
                           jwtRuntime
                           (ApiClientToken.apiClientTokenStore (accountWorkflowApiClientTokenEnvironment accountWorkflow))
                       )
                   )
               )
           ]
    )
    publicAuthenticationProfileName
    []

publicAuthenticationProfileName :: HarchWeb.AuthenticationProfileName
publicAuthenticationProfileName = HarchWeb.requiredAuthenticationProfileNameOrDie "public"

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
