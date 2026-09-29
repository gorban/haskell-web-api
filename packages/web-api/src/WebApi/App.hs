{-# LANGUAGE BangPatterns #-}

-- | Compose web-api's typed Site, page routes, actions, security profiles,
-- and application-owned request context. Runtime resource acquisition and
-- listener startup live in 'WebApi.App.Runtime'; this module remains the
-- single owner of application composition.
--
-- Decision record (AHI-4E module health, 2026-09-28): split runtime/config
-- startup into 'WebApi.App.Runtime', which depends on this composition
-- boundary. Route declarations, request policy, account workflow, and the
-- security registry remain assembled here; the runtime module adds concrete
-- PostgreSQL/JWT/reporter dependencies without duplicating those tables or
-- adding a second dispatcher.
-- The Swagger page has no site-navigation label, so its typed page definition
-- records 'Nothing' directly. 'buildAppRouteDefinition' exposes this exact
-- route metadata as a typed composition seam, so the no-navigation contract
-- can be tested without adding the page to the site's link list.
--
-- The root attaches declared route facts only after typed route selection
-- through 'Site.siteAttachRouteObservation'. The existing admission rail also
-- chooses the account or resource JWT profile after matching; client-action
-- CSRF remains required for cookie and dual-source requests and is omitted
-- only for the established bearer-only account source.
module WebApi.App
  ( buildAppWithDatabase,
    buildAppWithDatabaseAndAccountWorkflow,
    buildAppWithDatabaseAndAccountWorkflowAndSecurity,
    buildAppWithDatabaseAndReporters,
    buildAppWithDatabaseAndReportersAndSecurity,
    buildAppRouteDefinition,
    RuntimeApplicationReporters (..),
    runtimeAuthenticationProfiles,
    buildApp,
    buildRuntimeAccountWorkflow,
    buildRuntimeAccountWorkflowWithJwt,
    buildRuntimeAccountWorkflowWithJwtRuntime,
    otlpExportFailureMessage,
    runtimeRequestObservabilityReporter,
    unavailableAccountWorkflow,
  )
where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text qualified as Text
import HarchWeb qualified
import HarchWeb.Action (decodeAction)
import HarchWeb.Observability qualified as Observability
import HarchWeb.OpenApi (OpenApiDocumentProvider)
import HarchWeb.Site qualified as Site
import Network.HTTP.Types qualified as Http
import WebApi.AccountJwt (AccountJwtRuntime, accountJwtAuthenticationPipeline)
import WebApi.AccountPages (AccountAction, accountActionEndpointMetadata, accountActionRoute, accountActions, accountCsrfProtection, handleAccountAction)
import WebApi.Api.Endpoints (meApiRouteDefinition, secondApiRouteDefinition, statusApiRouteDefinition, tokenApiRouteDefinition)
import WebApi.Api.OpenApiDocs (docsOpenApiSpecRouteDefinition, requireWebApiOpenApiDocumentProvider, webApiOpenApiDocumentProvider)
import WebApi.ApiClientToken qualified as ApiClientToken
import WebApi.App.AccountWorkflow (buildRuntimeAccountWorkflow, buildRuntimeAccountWorkflowWithJwt, buildRuntimeAccountWorkflowWithJwtRuntime, unavailableAccountWorkflow)
import WebApi.App.Observability
  ( otlpExportFailureMessage,
    runtimeRequestObservabilityReporter,
  )
import WebApi.App.Shell (appPageShellForPage, appRuntimeAssets)
import WebApi.AppEffect (AccountWorkflow (..))
import WebApi.Config
  ( AppConfig (..),
  )
import WebApi.Database (PageRepository, defaultPageRepository)
import WebApi.DocsSwagger (docsSwaggerPage)
import WebApi.PageModule (PageDefinitionContext (..))
import WebApi.Pages.Generated qualified as PagesGenerated
import WebApi.ResourceAuthentication qualified as ResourceAuthentication
import WebApi.Response (apiNotFoundResponse, renderLocale, selectResponseWithAccountWorkflow, todoLocation)
import WebApi.Route
  ( AppAuthorization,
    AppRequestContext (..),
    AppRoute (..),
    RequestAuthenticationTransport (..),
    accountAuthenticationProfileName,
    appNavigationRoutes,
    appRouteMethods,
    defaultRequestContext,
    endpointMetadata,
    requestContextFromWaiRequest,
    resourceAuthenticationProfileName,
    routeCodec,
    routeNavigationDeclaration,
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
-- applications; the production server uses
-- 'WebApi.App.Runtime.buildRuntimeAppWithAccountJwt'
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
                    Site.simpleSitePageShell = appPageShellForPage config,
                    Site.simpleSiteNavigationRoutes = appNavigationRoutes,
                    Site.simpleSiteRouteDefinition = buildAppRouteDefinition config pageRepository accountWorkflow docsOpenApiDocumentProvider
                  }
            )
              { Site.siteRequestContextFromRequest =
                  requestContextFromWaiRequest (requestPolicy config),
                -- Decision (durable activity audit, 2026-09-08): reuse Site's existing
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

    -- OpenAPI documentation and Swagger UI: resolve the startup-cached OpenAPI provider exactly once, with
    -- the same eager-binding discipline
    -- 'WebApi.App.Runtime.buildRuntimeAppWithAccountJwt'
    -- already applies to @!accountWorkflow@. A typed document-construction
    -- failure (an invalid title, a duplicate operation, an unresolvable
    -- security profile) therefore surfaces while the framework forces this
    -- application value to build its request dispatcher — application
    -- startup failure — instead of a first-request crash or a stale served
    -- document. The prepared bytes are immutable, so every later
    -- @/docs/openapi.json@ request serves exactly what startup validated.
    !docsOpenApiDocumentProvider =
      requireWebApiOpenApiDocumentProvider
        ( webApiOpenApiDocumentProvider
            pageRepository
            (accountWorkflowProfileStore accountWorkflow)
            (accountWorkflowApiClientTokenEnvironment accountWorkflow)
        )

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

-- | Build the exact typed Site route definition for one route with its
-- application dependencies supplied explicitly. Exposing this composition
-- seam lets embedders inspect or reuse declaration metadata without building
-- a second route table.
buildAppRouteDefinition ::
  AppConfig ->
  PageRepository ->
  AccountWorkflow ->
  OpenApiDocumentProvider AppRequestContext ->
  AppRoute ->
  Site.RouteDefinition AppRoute AppRequestContext AppAuthorization
buildAppRouteDefinition config pageRepository accountWorkflow docsOpenApiDocumentProvider route =
  case route of
    StatusApiRoute -> statusApiRouteDefinition
    SecondApiRoute -> secondApiRouteDefinition pageRepository
    MeApiRoute -> meApiRouteDefinition (accountWorkflowProfileStore accountWorkflow)
    TokenApiRoute -> tokenApiRouteDefinition (accountWorkflowApiClientTokenEnvironment accountWorkflow)
    DocsOpenApiSpecRoute -> docsOpenApiSpecRouteDefinition docsOpenApiDocumentProvider
    HomeRoute ->
      protocolRouteDefinition route $
        \routeRequest -> pure (HarchWeb.nonPageRedirectResponse Http.status302 (todoLocation routeRequest))
    ApiNotFoundRoute ->
      protocolRouteDefinition route $
        \_ ->
          pure (HarchWeb.NonPageBodyResponse apiNotFoundResponse)
    -- The generated page family carries its own composed definition
    -- (see 'WebApi.PageModule'): title, hooks, scoped styles, load rail, and
    -- body all arrive from the page module.
    GeneratedPages generatedPage ->
      PagesGenerated.pageRouteDefinition
        PageDefinitionContext
          { pageDefinitionConfig = config,
            pageDefinitionPageRepository = pageRepository
          }
        generatedPage
    -- OpenAPI documentation and Swagger UI: the docs page is an ordinary typed page route; its
    -- SSR, stylesheet, and enhancement descriptor all arrive from the
    -- typed Swagger surface in 'WebApi.DocsSwagger'.
    DocsSwaggerRoute ->
      Site.pageRoute
        (endpointMetadata route)
        Nothing
        (\_security request -> pure (docsSwaggerPage request))
    _ ->
      Site.RouteDefinition
        { Site.routeNavigation = routeNavigationDeclaration route,
          Site.routeMetadata = endpointMetadata route,
          Site.routeMethods = const (HarchWeb.routeMethodPolicy (appRouteMethods route)),
          Site.routeExecutionPolicy = HarchWeb.unboundedRouteExecutionPolicy,
          Site.routeHandler = Site.PageRouteHandler $
            \_ -> selectResponseWithAccountWorkflow config accountWorkflow
        }

protocolRouteDefinition :: AppRoute -> (HarchWeb.RouteRequest AppRoute AppRequestContext -> IO (HarchWeb.NonPageResponse AppRoute AppRequestContext)) -> Site.RouteDefinition AppRoute AppRequestContext AppAuthorization
protocolRouteDefinition route renderProtocol =
  Site.RouteDefinition
    { Site.routeNavigation = routeNavigationDeclaration route,
      Site.routeMetadata = endpointMetadata route,
      Site.routeMethods = const (HarchWeb.routeMethodPolicy (appRouteMethods route)),
      Site.routeExecutionPolicy = HarchWeb.unboundedRouteExecutionPolicy,
      Site.routeHandler = Site.ProtocolRouteHandler (const renderProtocol)
    }

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
