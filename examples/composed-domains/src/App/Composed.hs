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
module App.Composed
  ( ComposedContext,
    AdmissionPrincipal,
    AdmissionReturnTarget (..),
    AdmissionPrincipalId,
    AdmissionSessionId,
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
    SynchronizerTokenDigest,
    SynchronizerTokenStore (..),
    SynchronizerTokenStoreError (..),
    SynchronizerStoragePolicy,
    buildComposedModule,
    buildPostgresAdmissionSessionStoreWithRunner,
    buildPostgresAdmissionCredentialStoreWithRunner,
    buildPostgresAdmissionAttemptStoreWithRunner,
    defaultAdmissionAttemptStoragePolicy,
    buildComposedSite,
    buildComposedSiteWithAdmissionSecurity,
    buildComposedSiteWithSecurity,
    buildPublicModule,
    catalogModuleMount,
    defaultComposedStaticAssets,
    defaultComposedContext,
    defaultAdmissionSessionCookiePolicy,
    completeAdmissionProof,
    admissionAttemptScopeStorageKey,
    admissionAttemptBudgetsToList,
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
    synchronizerCsrfProtection,
    buildPostgresSynchronizerTokenStoreWithRunner,
    defaultSynchronizerStoragePolicy,
    mkSynchronizerStoragePolicy,
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
import App.Composed.CsrfSynchronizer
  ( SynchronizerTokenDigest,
    SynchronizerTokenStore (..),
    SynchronizerTokenStoreError (..),
    synchronizerCsrfProtection,
  )
import App.Composed.Localized (localizeApplicationModule, requestContextFromWai)
import App.Composed.Model
import App.Composed.Mounts (catalogModuleMount, ordersModuleMount)
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
import App.Composed.Postgres.AdmissionCredentialStore (buildPostgresAdmissionCredentialStoreWithRunner)
import App.Composed.Postgres.AdmissionSessionStore (buildPostgresAdmissionSessionStoreWithRunner)
import App.Composed.Postgres.SynchronizerStore
  ( SynchronizerStoragePolicy,
    buildPostgresSynchronizerTokenStoreWithRunner,
    defaultSynchronizerStoragePolicy,
    mkSynchronizerStoragePolicy,
  )
import App.Composed.Public (buildPublicModule, buildPublicModuleWithAdmissionWorkflow)
import Catalog.Domain (CatalogCommands, CatalogQueries, CatalogRoute (CatalogIndex), buildCatalogModule)
import Data.List.NonEmpty (NonEmpty (..))
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
import HarchWeb.RequestContext (CoreRequestContext (..), RequestContext (..))
import HarchWeb.SecurityEvent (RouteObservation (..))
import HarchWeb.Site (Site)
import HarchWeb.Site qualified as Site
import HarchWeb.StaticAssets (StaticAssetsConfig)
import Orders.Domain (OrdersCommands, OrdersQueries, OrdersRoute (OrdersIndex), buildOrdersModule)

buildComposedSite :: StaticAssetsConfig -> LocalePolicy -> CsrfProtection ComposedContext -> CatalogQueries -> CatalogCommands -> OrdersQueries -> OrdersCommands -> Site RootRoute RootAction ComposedContext RootAuthorization
buildComposedSite staticAssetsConfig localePolicy csrfProtection =
  buildComposedSiteWithAdmissionWorkflow staticAssetsConfig localePolicy csrfProtection Nothing (AuthenticationDisabled [])

-- | Compose the application-owned admission policy before the root account
-- authentication guard.  A caller cannot enable admission while selecting a
-- public-only security configuration: 'applyAdmissionPolicy' returns the
-- explicit assembly error instead of silently weakening the route matrix.
buildComposedSiteWithAdmissionSecurity :: StaticAssetsConfig -> LocalePolicy -> CsrfProtection ComposedContext -> AdmissionPolicy -> ApplicationSecurity RootRoute ComposedContext RootAuthorization -> CatalogQueries -> CatalogCommands -> OrdersQueries -> OrdersCommands -> Either AdmissionCompositionError (Site RootRoute RootAction ComposedContext RootAuthorization)
buildComposedSiteWithAdmissionSecurity staticAssetsConfig localePolicy csrfProtection admissionPolicy rootSecurity catalogQueries catalogCommands ordersQueries ordersCommands = do
  securedRoot <- applyAdmissionPolicy admissionPolicy rootSecurity
  pure (buildComposedSiteWithAdmissionWorkflow staticAssetsConfig localePolicy csrfProtection (admissionWorkflow admissionPolicy) securedRoot catalogQueries catalogCommands ordersQueries ordersCommands)

-- | The root chooses deployment security explicitly.  The runnable example
-- stays public-only until AHI-4C supplies login; tests may supply a bounded
-- authenticated policy without pretending it is a deployment credential.
buildComposedSiteWithSecurity :: StaticAssetsConfig -> LocalePolicy -> CsrfProtection ComposedContext -> ApplicationSecurity RootRoute ComposedContext RootAuthorization -> CatalogQueries -> CatalogCommands -> OrdersQueries -> OrdersCommands -> Site RootRoute RootAction ComposedContext RootAuthorization
buildComposedSiteWithSecurity staticAssetsConfig localePolicy csrfProtection =
  buildComposedSiteWithAdmissionWorkflow staticAssetsConfig localePolicy csrfProtection Nothing

buildComposedSiteWithAdmissionWorkflow :: StaticAssetsConfig -> LocalePolicy -> CsrfProtection ComposedContext -> Maybe (AdmissionConfig, AdmissionProofConfig) -> ApplicationSecurity RootRoute ComposedContext RootAuthorization -> CatalogQueries -> CatalogCommands -> OrdersQueries -> OrdersCommands -> Site RootRoute RootAction ComposedContext RootAuthorization
buildComposedSiteWithAdmissionWorkflow staticAssetsConfig localePolicy csrfProtection maybeAdmissionWorkflow rootSecurity catalogQueries catalogCommands ordersQueries ordersCommands =
  initialSite
    { Site.siteRequestContextFromRequest = requestContextFromWai localePolicy (Site.siteRequestPolicy initialSite),
      Site.siteCsrfProtection = csrfProtection,
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
    rootModule = buildComposedModuleWithAdmissionWorkflow staticAssetsConfig localePolicy csrfProtection maybeAdmissionWorkflow catalogQueries catalogCommands ordersQueries ordersCommands

composedPageShell :: Page RootRoute ComposedContext -> PageShell RootRoute ComposedContext
composedPageShell page =
  PageShell
    { shellBodyAttributes = [],
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

buildComposedModule :: StaticAssetsConfig -> LocalePolicy -> CatalogQueries -> CatalogCommands -> OrdersQueries -> OrdersCommands -> ApplicationModule RootRoute RootActionTarget RootAction ComposedContext RootAuthorization
buildComposedModule staticAssetsConfig localePolicy =
  buildComposedModuleWithPublicModule localePolicy (buildPublicModule staticAssetsConfig)

buildComposedModuleWithAdmissionWorkflow :: StaticAssetsConfig -> LocalePolicy -> CsrfProtection ComposedContext -> Maybe (AdmissionConfig, AdmissionProofConfig) -> CatalogQueries -> CatalogCommands -> OrdersQueries -> OrdersCommands -> ApplicationModule RootRoute RootActionTarget RootAction ComposedContext RootAuthorization
buildComposedModuleWithAdmissionWorkflow staticAssetsConfig localePolicy csrfProtection maybeAdmissionWorkflow =
  buildComposedModuleWithPublicModule localePolicy publicModule
  where
    publicModule = buildPublicModuleWithAdmissionWorkflow staticAssetsConfig csrfProtection maybeAdmissionWorkflow

buildComposedModuleWithPublicModule :: LocalePolicy -> ApplicationModule LocalizedRoute RootActionTarget RootAction ComposedContext RootAuthorization -> CatalogQueries -> CatalogCommands -> OrdersQueries -> OrdersCommands -> ApplicationModule RootRoute RootActionTarget RootAction ComposedContext RootAuthorization
buildComposedModuleWithPublicModule localePolicy publicModule catalogQueries catalogCommands ordersQueries ordersCommands =
  requiredModuleConfiguration (localizeApplicationModule localePolicy localizedModule)
  where
    catalogModule = requiredModuleConfiguration (mountApplicationModule catalogModuleMount (buildCatalogModule catalogQueries catalogCommands))
    ordersModule = requiredModuleConfiguration (mountApplicationModule ordersModuleMount (buildOrdersModule ordersQueries ordersCommands))
    localizedModule = requiredModuleConfiguration (combineApplicationModules (publicModule :| [catalogModule, ordersModule]))

admissionWorkflow :: AdmissionPolicy -> Maybe (AdmissionConfig, AdmissionProofConfig)
admissionWorkflow policy =
  case policy of
    AdmissionDisabled -> Nothing
    AdmissionEnabled admissionConfig admissionProofConfig -> Just (admissionConfig, admissionProofConfig)
