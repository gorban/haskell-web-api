{-# LANGUAGE OverloadedStrings #-}

-- | The thin server-owning composition root for independently packaged
-- Catalog and Orders modules.  Closed root values, domain mounts, public
-- routes, and locale adaptation are private modules with their own stable
-- ownership; this module keeps deployment security, site installation, and
-- trusted route-observation attachment together.
--
-- Decision record (AHI-4B-MH, 2026-09-03): split the former monolithic root
-- by ownership rather than by incidental helpers.  The extracted modules do
-- not become a service locator or a second routing architecture: domains
-- still cannot import the root, route/action algebras remain closed, and this
-- root remains the only site/security/observation owner.
--
-- Decision record (PR-F4, 2026-09-05): the root's durable deployment
-- collaborators travel in 'ComposedSiteDependencies', with independently
-- packaged domain query/command capabilities nested in
-- 'ComposedDomainCapabilities'.  This replaces positional assembly lists
-- without creating ambient application state.  Per-request WAI and typed route
-- values stay explicit at their protocol boundary; the native admission
-- fallback likewise groups only its installed stable capabilities.
module App.Composed
  ( ComposedContext,
    AdmissionPrincipal,
    AdmissionReturnTarget (..),
    AdmissionPrincipalId,
    AdmissionSessionId,
    AdmissionSetupCommand (..),
    AdmissionSetupError (..),
    AdmissionCompositionError (..),
    AdmissionConfig,
    AdmissionConfigError (..),
    AdmissionAttemptAdmission (..),
    AdmissionAttemptBudget (..),
    AdmissionAttemptBudgets,
    AdmissionPrincipalKey (..),
    AdmissionAttemptReservation (..),
    AdmissionAttemptScope (..),
    AdmissionAttemptStore (..),
    AdmissionAttemptStoreError (..),
    AdmissionAttemptStoragePolicy,
    AdmissionCredentialStore (..),
    AdmissionCredentialStoreError (..),
    AdmissionLoginName,
    AdmissionPolicy (..),
    AdmissionProofConfig (..),
    AdmissionProofClockError (..),
    AdmissionProofResult (..),
    AdmissionRequirement (..),
    AdmissionSessionStore (..),
    AdmissionSessionStoreError (..),
    AdmissionSessionClockError (..),
    AdmissionSessionIssueError (..),
    EncryptedAdmissionTotpSecret,
    LocalePolicy (..),
    LocaleResolutionInput (..),
    LocalizedRoute (..),
    PublicRoute (..),
    RootAction (..),
    RootActionTarget (..),
    RootAuthorization (..),
    RootClient (..),
    RootLocal (..),
    RootPrincipal (..),
    RootRoute (..),
    StoredAdmissionCredential (..),
    ComposedDatabaseConnectionString (..),
    ComposedDatabaseRuntime,
    ComposedDeploymentConfig,
    composedDeploymentAdmissionEncryptionKey,
    composedDeploymentDatabase,
    ComposedDomainCapabilities (..),
    ComposedSiteDependencies (..),
    SynchronizerTokenDigest,
    SynchronizerTokenCapacityPolicy (..),
    SynchronizerTokenStore (..),
    SynchronizerTokenStoreError (..),
    SynchronizerStoragePolicy,
    buildComposedModuleWithDependencies,
    buildPostgresAdmissionSessionStoreWithRunner,
    buildPostgresAdmissionCredentialStoreWithRunner,
    provisionPostgresAdmissionCredentialWithRunner,
    buildPostgresAdmissionAttemptStoreWithRunner,
    closeComposedDatabaseRuntime,
    defaultAdmissionAttemptStoragePolicy,
    buildComposedSiteWithDependencies,
    buildComposedSiteWithAdmissionSecurityDependencies,
    buildComposedSiteWithSecurityDependencies,
    buildPublicModule,
    catalogModuleMount,
    defaultComposedStaticAssets,
    defaultComposedContext,
    defaultAdmissionSessionCookiePolicy,
    completeAdmissionProof,
    admissionAttemptScopeStorageKey,
    admissionAttemptBudgetsToList,
    encryptAdmissionTotpSecret,
    admissionReturnTargetRoute,
    admissionLoginNameText,
    admissionPrincipalId,
    admissionPrincipalIdText,
    admissionPrincipalSessionExpiresAt,
    admissionPrincipalSessionId,
    encryptedAdmissionTotpSecretText,
    issueAdmissionSession,
    defaultLocalePolicy,
    localizeApplicationModule,
    ordersModuleMount,
    mkAdmissionConfig,
    mkAdmissionReturnTarget,
    mkAdmissionAttemptStoragePolicy,
    mkAdmissionLoginName,
    mkEncryptedAdmissionTotpSecret,
    mkAdmissionPrincipal,
    mkAdmissionPrincipalId,
    mkAdmissionSessionId,
    unAdmissionSessionId,
    resolveAdmissionCsrfBinding,
    resolveLocale,
    composedDatabaseChanges,
    runComposedDatabaseChanges,
    runComposedDatabaseChangesWithExecutor,
    newComposedDatabaseRuntime,
    parseComposedDeploymentConfig,
    runComposedDatabaseQuery,
    synchronizerCsrfProtection,
    buildPostgresSynchronizerTokenStoreWithRunner,
    defaultSynchronizerStoragePolicy,
    mkSynchronizerStoragePolicy,
    parseAdmissionSetupCommand,
    renderAdmissionSetupError,
  )
where

import App.Composed.Admission
  ( AdmissionAttemptAdmission (..),
    AdmissionAttemptBudget (..),
    AdmissionAttemptBudgets,
    AdmissionAttemptReservation (..),
    AdmissionAttemptScope (..),
    AdmissionAttemptStore (..),
    AdmissionAttemptStoreError (..),
    AdmissionCompositionError (..),
    AdmissionConfig,
    AdmissionConfigError (..),
    AdmissionCredentialStore (..),
    AdmissionCredentialStoreError (..),
    AdmissionPolicy (..),
    AdmissionPrincipalKey (..),
    AdmissionProofClockError (..),
    AdmissionProofConfig (..),
    AdmissionProofResult (..),
    AdmissionRequirement (..),
    AdmissionSessionClockError (..),
    AdmissionSessionIssueError (..),
    AdmissionSessionStore (..),
    AdmissionSessionStoreError (..),
    StoredAdmissionCredential (..),
    admissionAttemptBudgetsToList,
    admissionAttemptScopeStorageKey,
    applyAdmissionPolicy,
    completeAdmissionProof,
    defaultAdmissionSessionCookiePolicy,
    issueAdmissionSession,
    mkAdmissionConfig,
    resolveAdmissionCsrfBinding,
  )
import App.Composed.Admission.Types
  ( AdmissionLoginName,
    AdmissionPrincipalId,
    AdmissionSessionId,
    EncryptedAdmissionTotpSecret,
    admissionLoginNameText,
    admissionPrincipalId,
    admissionPrincipalIdText,
    admissionPrincipalSessionExpiresAt,
    admissionPrincipalSessionId,
    encryptedAdmissionTotpSecretText,
    mkAdmissionLoginName,
    mkAdmissionPrincipal,
    mkAdmissionPrincipalId,
    mkAdmissionSessionId,
    mkEncryptedAdmissionTotpSecret,
    unAdmissionSessionId,
  )
import App.Composed.AdmissionSetup
  ( AdmissionSetupCommand (..),
    AdmissionSetupError (..),
    encryptAdmissionTotpSecret,
    parseAdmissionSetupCommand,
    renderAdmissionSetupError,
  )
import App.Composed.CsrfSynchronizer
  ( SynchronizerTokenCapacityPolicy (..),
    SynchronizerTokenDigest,
    SynchronizerTokenStore (..),
    SynchronizerTokenStoreError (..),
    synchronizerCsrfProtection,
  )
import App.Composed.DeploymentConfig
  ( ComposedDeploymentConfig,
    composedDeploymentAdmissionEncryptionKey,
    composedDeploymentDatabase,
    parseComposedDeploymentConfig,
  )
import App.Composed.Localized (localizeApplicationModule, requestContextFromWai)
import App.Composed.Model
import App.Composed.Mounts (catalogApiModuleMount, catalogModuleMount, ordersApiModuleMount, ordersModuleMount)
import App.Composed.Postgres
  ( ComposedDatabaseConnectionString (..),
    composedDatabaseChanges,
    runComposedDatabaseChanges,
    runComposedDatabaseChangesWithExecutor,
  )
import App.Composed.Postgres.AdmissionAttemptStore
  ( AdmissionAttemptStoragePolicy,
    buildPostgresAdmissionAttemptStoreWithRunner,
    defaultAdmissionAttemptStoragePolicy,
    mkAdmissionAttemptStoragePolicy,
  )
import App.Composed.Postgres.AdmissionCredentialStore
  ( buildPostgresAdmissionCredentialStoreWithRunner,
    provisionPostgresAdmissionCredentialWithRunner,
  )
import App.Composed.Postgres.AdmissionSessionStore (buildPostgresAdmissionSessionStoreWithRunner)
import App.Composed.Postgres.Runtime
  ( ComposedDatabaseRuntime,
    closeComposedDatabaseRuntime,
    newComposedDatabaseRuntime,
    runComposedDatabaseQuery,
  )
import App.Composed.Postgres.SynchronizerStore
  ( SynchronizerStoragePolicy,
    buildPostgresSynchronizerTokenStoreWithRunner,
    defaultSynchronizerStoragePolicy,
    mkSynchronizerStoragePolicy,
  )
import App.Composed.Public (buildPublicModule, buildPublicModuleWithAdmissionWorkflow)
import Catalog.Api (buildCatalogApiModule)
import Catalog.Domain (CatalogCommands, CatalogQueries, CatalogRoute (CatalogIndex), buildCatalogModule)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import HarchWeb.Api (ApiForm)
import HarchWeb.ApplicationModule
  ( ApplicationModule (..),
    applicationModuleSite,
    combineApplicationModules,
    mountApplicationModule,
    requiredModuleConfiguration,
  )
import HarchWeb.Csrf (CsrfProtection)
import HarchWeb.Document
  ( NavigationItem (..),
    Page (..),
    PageShell (..),
    defaultNavigationRuntime,
  )
import HarchWeb.EndpointMetadata (EndpointMetadata (..))
import HarchWeb.EndpointSecurity (ApplicationSecurity (AuthenticationDisabled))
import HarchWeb.Markup (literalElementId)
import HarchWeb.OpenApi (OpenApiExtension, OpenApiExtensionError, mkOpenApiExtension)
import HarchWeb.RequestContext (CoreRequestContext (..), RequestContext (..))
import HarchWeb.SecurityEvent (RouteObservation (..))
import HarchWeb.Site (Site)
import HarchWeb.Site qualified as Site
import HarchWeb.StaticAssets (StaticAssetsConfig)
import Orders.Api (SubmitOrderCommand, buildOrdersApiModule)
import Orders.Domain (OrdersCommands, OrdersQueries, OrdersRoute (OrdersIndex), buildOrdersModule)

-- | The Catalog and Orders capabilities installed by this composed root.
-- They are grouped because they are selected once at assembly and then mounted
-- together, while each domain remains independently packaged.
data ComposedDomainCapabilities = ComposedDomainCapabilities
  { composedCatalogQueries :: CatalogQueries,
    composedCatalogCommands :: CatalogCommands,
    composedOrdersQueries :: OrdersQueries,
    composedOrdersCommands :: OrdersCommands
  }

-- | Stable root assembly dependencies.  Request and route values deliberately
-- do not belong here: they are created per invocation and remain explicit at
-- their respective boundary.
data ComposedSiteDependencies = ComposedSiteDependencies
  { composedStaticAssets :: StaticAssetsConfig,
    composedLocalePolicy :: LocalePolicy,
    composedCsrfProtection :: CsrfProtection ComposedContext,
    composedDomainCapabilities :: ComposedDomainCapabilities
  }

buildComposedSiteWithDependencies :: ComposedSiteDependencies -> Site RootRoute RootAction ComposedContext RootAuthorization
buildComposedSiteWithDependencies dependencies =
  buildComposedSiteWithAdmissionWorkflow dependencies Nothing (AuthenticationDisabled [])

-- | Compose the application-owned admission policy before the root account
-- authentication guard.  A caller cannot enable admission while selecting a
-- public-only security configuration: 'applyAdmissionPolicy' returns the
-- explicit assembly error instead of silently weakening the route matrix.
buildComposedSiteWithAdmissionSecurityDependencies :: ComposedSiteDependencies -> AdmissionPolicy -> ApplicationSecurity RootRoute ComposedContext RootAuthorization -> Either AdmissionCompositionError (Site RootRoute RootAction ComposedContext RootAuthorization)
buildComposedSiteWithAdmissionSecurityDependencies dependencies admissionPolicy rootSecurity = do
  securedRoot <- applyAdmissionPolicy admissionPolicy rootSecurity
  pure (buildComposedSiteWithAdmissionWorkflow dependencies (admissionWorkflow admissionPolicy) securedRoot)

-- | The root chooses deployment security explicitly.  The runnable example
-- stays public-only until AHI-4C supplies login; tests may supply a bounded
-- authenticated policy without pretending it is a deployment credential.
buildComposedSiteWithSecurityDependencies :: ComposedSiteDependencies -> ApplicationSecurity RootRoute ComposedContext RootAuthorization -> Site RootRoute RootAction ComposedContext RootAuthorization
buildComposedSiteWithSecurityDependencies dependencies =
  buildComposedSiteWithAdmissionWorkflow dependencies Nothing

buildComposedSiteWithAdmissionWorkflow :: ComposedSiteDependencies -> Maybe (AdmissionConfig, AdmissionProofConfig) -> ApplicationSecurity RootRoute ComposedContext RootAuthorization -> Site RootRoute RootAction ComposedContext RootAuthorization
buildComposedSiteWithAdmissionWorkflow dependencies maybeAdmissionWorkflow rootSecurity =
  initialSite
    { Site.siteRequestContextFromRequest = requestContextFromWai (composedLocalePolicy dependencies) (Site.siteRequestPolicy initialSite),
      Site.siteCsrfProtection = composedCsrfProtection dependencies,
      Site.siteNavigationRuntime = Just defaultNavigationRuntime,
      Site.sitePageShell = composedPageShell,
      Site.siteAttachRouteObservation = \routeValue metadata requestContext ->
        requestContext
          { requestCore =
              (requestCore requestContext)
                { requestRouteObservation =
                    Just
                      RouteObservation
                        { observedEndpointName = endpointName metadata,
                          observedMountChain = moduleRouteMountChain rootModule routeValue,
                          observedRouteTemplate = endpointRouteTemplate metadata,
                          observedLocale = requestLocale (requestCore requestContext)
                        }
                }
          }
    }
  where
    initialSite =
      applicationModuleSite
        "composed-domains"
        defaultComposedContext
        rootSecurity
        rootModule
    rootModule = buildComposedModuleWithAdmissionWorkflow dependencies maybeAdmissionWorkflow

composedPageShell :: Page RootRoute ComposedContext -> PageShell RootRoute ComposedContext
composedPageShell page =
  PageShell
    { shellDocumentLanguage = selectedLocale,
      shellBodyAttributes = [],
      shellNavigationAttributes = [],
      shellNavigationItems =
        [ NavigationItem "Sign in" (Localized selectedLocale (Public PublicLogin)),
          NavigationItem "Catalog" (Localized selectedLocale (Catalog CatalogIndex)),
          NavigationItem "Orders" (Localized selectedLocale (Orders OrdersIndex))
        ],
      shellMainId = literalElementId "main",
      shellMainAttributes = [],
      shellNavigationLifecycle = Nothing,
      shellStylesheets = [],
      shellRuntimeDescriptors = []
    }
  where
    selectedLocale = requestLocale (requestCore (pageContext page))

buildComposedModuleWithDependencies :: ComposedSiteDependencies -> ApplicationModule RootRoute RootActionTarget RootAction ComposedContext RootAuthorization
buildComposedModuleWithDependencies dependencies =
  buildComposedModuleWithPublicModule dependencies (buildPublicModule (composedStaticAssets dependencies))

buildComposedModuleWithAdmissionWorkflow :: ComposedSiteDependencies -> Maybe (AdmissionConfig, AdmissionProofConfig) -> ApplicationModule RootRoute RootActionTarget RootAction ComposedContext RootAuthorization
buildComposedModuleWithAdmissionWorkflow dependencies maybeAdmissionWorkflow =
  buildComposedModuleWithPublicModule dependencies publicModule
  where
    publicModule = buildPublicModuleWithAdmissionWorkflow (composedStaticAssets dependencies) (composedCsrfProtection dependencies) maybeAdmissionWorkflow

buildComposedModuleWithPublicModule :: ComposedSiteDependencies -> ApplicationModule LocalizedRoute RootActionTarget RootAction ComposedContext RootAuthorization -> ApplicationModule RootRoute RootActionTarget RootAction ComposedContext RootAuthorization
buildComposedModuleWithPublicModule dependencies publicModule =
  requiredModuleConfiguration (localizeApplicationModule (composedLocalePolicy dependencies) localizedModule)
  where
    domainCapabilities = composedDomainCapabilities dependencies
    catalogModule = requiredModuleConfiguration (mountApplicationModule catalogModuleMount (buildCatalogModule (composedCatalogQueries domainCapabilities) (composedCatalogCommands domainCapabilities)))
    ordersModule = requiredModuleConfiguration (mountApplicationModule ordersModuleMount (buildOrdersModule (composedOrdersQueries domainCapabilities) (composedOrdersCommands domainCapabilities)))
    -- The domain API modules take the documentation extension at assembly:
    -- the composed root is the one documented surface (AHI-4E), while the
    -- domain packages themselves compile unchanged with 'NoApiExtension'.
    catalogApiModule = requiredModuleConfiguration (mountApplicationModule catalogApiModuleMount (buildCatalogApiModule composedCatalogItemsExtension (composedCatalogQueries domainCapabilities)))
    ordersApiModule = requiredModuleConfiguration (mountApplicationModule ordersApiModuleMount (buildOrdersApiModule composedOrdersSubmitExtension (composedOrdersCommands domainCapabilities)))
    localizedModule = requiredModuleConfiguration (combineApplicationModules (publicModule :| [catalogModule, ordersModule, catalogApiModule, ordersApiModule]))

-- | The composed root's authored documentation extensions for the two
-- documented operations (AHI-4E): one combined document, separate
-- Catalog/Orders tags, and the ordinary scope requirements.
-- | Unwrap one statically authored documentation extension; a failure here
-- is a composition-time defect in authored literals, exactly like the
-- framework's required-or-die boundaries.
requireOpenApiExtension :: Either OpenApiExtensionError (OpenApiExtension fields body response) -> OpenApiExtension fields body response
requireOpenApiExtension = either (error . ("composed-domains authored an invalid OpenAPI extension: " <>) . show) id

composedCatalogItemsExtension :: OpenApiExtension () () ByteString.ByteString
composedCatalogItemsExtension =
  requireOpenApiExtension
    (mkOpenApiExtension (Just "List the catalog summary.") Nothing ["Catalog"] False [])

composedOrdersSubmitExtension :: OpenApiExtension SubmitOrderCommand ApiForm ByteString.ByteString
composedOrdersSubmitExtension =
  requireOpenApiExtension
    (mkOpenApiExtension (Just "Submit one order.") Nothing ["Orders"] False [])

admissionWorkflow :: AdmissionPolicy -> Maybe (AdmissionConfig, AdmissionProofConfig)
admissionWorkflow policy =
  case policy of
    AdmissionDisabled -> Nothing
    AdmissionEnabled admissionConfig admissionProofConfig -> Just (admissionConfig, admissionProofConfig)
