{-# LANGUAGE OverloadedStrings #-}

-- | Core application-module contract and the root-owned adapter to 'Site'.
--
-- This private module owns the immutable module declaration, its trusted
-- module-chain contract, and the one final adaptation into the server-owning
-- 'Site'.  Mount and sibling-composition implementation live separately so
-- the public facade can stay stable without turning one module into a second
-- routing interpreter.
module HarchWeb.ApplicationModule.Core
  ( ApplicationModule (..),
    applicationModuleSite,
    inheritApplicationModuleGuards,
    installApplicationModule,
    requiredModuleConfiguration,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Text qualified as Text
import HarchWeb.Action (ActionCodec, actionEndpointMetadata, decodeAction)
import HarchWeb.EndpointSecurity
  ( ApplicationSecurity (..),
    EndpointGuard,
  )
import HarchWeb.Routing (RouteCodec)
import HarchWeb.SecurityEvent (ModuleName)
import HarchWeb.Server.Response (ClientActionRequest (..), ClientActionResponse)
import HarchWeb.Site (RouteDefinition, Site (..))
import HarchWeb.Site qualified as Site

-- | A reusable application module. It deliberately omits listener/runtime,
-- root authentication, request-head policy, stores, and reporters: those are
-- root-owned deployment capabilities, not ambient child dependencies. A
-- domain package receives its own explicit services when it constructs this
-- value.
data ApplicationModule route actionTarget action context authorization = ApplicationModule
  { moduleName :: ModuleName,
    -- | A module declares exactly the parent-route values it can render and
    -- execute. Composition uses this predicate to retain the root's one
    -- route table; a child never claims a sibling's constructor.
    moduleOwnsRoute :: route -> Bool,
    -- | Construction-owned module chain for every route this module can
    -- select.  It is consumed only by the root security-event attachment;
    -- handlers and guards receive no way to construct or alter observations.
    moduleRouteMountChain :: route -> NonEmpty ModuleName,
    moduleRouteCodec :: RouteCodec route context,
    -- | One representative route value for every endpoint declaration this
    -- module owns.  Startup uses the actual 'moduleEndpoints' table at these
    -- values to validate mounted endpoint names/templates before serving.
    -- Dynamic route values need only one representative per declaration.
    moduleDeclaredRoutes :: [route],
    moduleEndpoints :: route -> RouteDefinition route context authorization,
    moduleActionCodec :: ActionCodec actionTarget context authorization action,
    moduleHandleAction :: ClientActionRequest action context -> IO (Maybe ClientActionResponse),
    moduleGuards :: [EndpointGuard route context authorization]
  }

-- | Append module guards to the root's existing post-match security pipeline.
-- A child never receives an authentication configuration: public-root guards
-- stay in their declared order, and enabled-root guards always run before a
-- mounted module's restrictions.  Sibling module guards are supplied in root
-- composition order by the composition implementation.
inheritApplicationModuleGuards ::
  ApplicationSecurity route context authorization ->
  ApplicationModule route actionTarget action context authorization ->
  ApplicationSecurity route context authorization
inheritApplicationModuleGuards rootSecurity applicationModule =
  case rootSecurity of
    AuthenticationDisabled rootGuards -> AuthenticationDisabled (rootGuards <> moduleGuards applicationModule)
    AuthenticationEnabled beforeGuards authentication afterGuards ->
      AuthenticationEnabled beforeGuards authentication (afterGuards <> moduleGuards applicationModule)

-- | Install one already-composed module in the existing server-owning root.
-- The root keeps its deployment, request, shell, and reporting policy; only
-- the route/action declarations and the post-root guard tail come from the
-- module.  This is the final adaptation to 'Site', not a mounted WAI app or
-- parallel 404/405/action dispatcher.
installApplicationModule ::
  ApplicationModule route actionTarget action context authorization ->
  Site route action context authorization ->
  Site route action context authorization
installApplicationModule applicationModule site =
  attachApplicationModuleFeatures
    applicationModule
    ( site
        { siteRouteCodec = moduleRouteCodec applicationModule,
          siteRouteDefinition = moduleEndpoints applicationModule
        }
    )

-- | Create the minimal server-owning 'Site' for one already-composed module.
--
-- This is the direct root adapter when a composition root has no independent
-- route table to preserve. It avoids constructing placeholder route fields
-- only for 'installApplicationModule' to replace them. The caller still owns
-- request adaptation, shell, runtime, assets, observability, and deployment
-- security; the module contributes the one route/action dispatcher and its
-- post-root guard tail.
applicationModuleSite ::
  Text.Text ->
  context ->
  ApplicationSecurity route context authorization ->
  ApplicationModule route actionTarget action context authorization ->
  Site route action context authorization
applicationModuleSite siteName defaultContext rootSecurity applicationModule =
  attachApplicationModuleFeatures
    applicationModule
    ( Site.apiOnlySite
        siteName
        defaultContext
        (moduleRouteCodec applicationModule)
        rootSecurity
        (moduleEndpoints applicationModule)
    )

attachApplicationModuleFeatures ::
  ApplicationModule route actionTarget action context authorization ->
  Site route action context authorization ->
  Site route action context authorization
attachApplicationModuleFeatures applicationModule site =
  site
    { siteClientActionEndpointMetadata = \methodValue pathValue requestContext ->
        actionEndpointMetadata (moduleActionCodec applicationModule) requestContext methodValue pathValue,
      siteDecodeClientAction = decodeAction (moduleActionCodec applicationModule),
      siteHandleClientAction = moduleHandleAction applicationModule,
      siteRouteModuleChain = Just (moduleRouteMountChain applicationModule),
      siteSecurity = inheritApplicationModuleGuards (siteSecurity site) applicationModule
    }

-- | Resolve a declaration-time construction result. A composition root uses
-- this only for literals and fixed module declarations it owns; failure is an
-- authored configuration defect, not a request outcome. The structured
-- construction error supplies the relevant module/endpoint identity, so the
-- success path does not carry an unrelated diagnostic label.
requiredModuleConfiguration :: (Show errorValue) => Either errorValue value -> value
requiredModuleConfiguration =
  either (error . ("invalid application-module declaration: " <>) . show) id
