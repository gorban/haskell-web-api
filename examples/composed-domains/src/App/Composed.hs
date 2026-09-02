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
    buildComposedModule,
    buildComposedSite,
    buildComposedSiteWithSecurity,
    buildPublicModule,
    catalogModuleMount,
    defaultComposedStaticAssets,
    defaultComposedContext,
    defaultLocalePolicy,
    localizeApplicationModule,
    ordersModuleMount,
    resolveLocale,
  )
where

import App.Composed.Localized (localizeApplicationModule, requestContextFromWai)
import App.Composed.Model
import App.Composed.Mounts (catalogModuleMount, ordersModuleMount)
import App.Composed.Public (buildPublicModule)
import Catalog.Domain (CatalogCommands, CatalogQueries, CatalogRoute (CatalogIndex), buildCatalogModule)
import Data.List.NonEmpty (NonEmpty (..))
import HarchWeb.ApplicationModule
  ( ApplicationModule (..),
    applicationModuleSite,
    combineApplicationModules,
    mountApplicationModule,
    requiredModuleConfiguration,
  )
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

buildComposedSite :: StaticAssetsConfig -> LocalePolicy -> CatalogQueries -> CatalogCommands -> OrdersQueries -> OrdersCommands -> Site RootRoute RootAction ComposedContext RootAuthorization
buildComposedSite staticAssetsConfig localePolicy =
  buildComposedSiteWithSecurity staticAssetsConfig localePolicy (AuthenticationDisabled [])

-- | The root chooses deployment security explicitly.  The runnable example
-- stays public-only until AHI-4C supplies login; tests may supply a bounded
-- authenticated policy without pretending it is a deployment credential.
buildComposedSiteWithSecurity :: StaticAssetsConfig -> LocalePolicy -> ApplicationSecurity RootRoute ComposedContext RootAuthorization -> CatalogQueries -> CatalogCommands -> OrdersQueries -> OrdersCommands -> Site RootRoute RootAction ComposedContext RootAuthorization
buildComposedSiteWithSecurity staticAssetsConfig localePolicy rootSecurity catalogQueries catalogCommands ordersQueries ordersCommands =
  ( applicationModuleSite
      "composed-domains"
      defaultComposedContext
      rootSecurity
      rootModule
  )
    { Site.siteRequestContextFromRequest = requestContextFromWai localePolicy,
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
    rootModule = buildComposedModule staticAssetsConfig localePolicy catalogQueries catalogCommands ordersQueries ordersCommands

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
buildComposedModule staticAssetsConfig localePolicy catalogQueries catalogCommands ordersQueries ordersCommands =
  requiredModuleConfiguration (localizeApplicationModule localePolicy localizedModule)
  where
    catalogModule = requiredModuleConfiguration (mountApplicationModule catalogModuleMount (buildCatalogModule catalogQueries catalogCommands))
    ordersModule = requiredModuleConfiguration (mountApplicationModule ordersModuleMount (buildOrdersModule ordersQueries ordersCommands))
    localizedModule = requiredModuleConfiguration (combineApplicationModules (buildPublicModule staticAssetsConfig :| [catalogModule, ordersModule]))
