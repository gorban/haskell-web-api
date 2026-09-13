-- | Typed application-module route composition.
--
-- AHI-4B keeps a reusable module below the server-owning 'Application': this
-- facade composes only route ownership and one-way request-context projection.
-- It deliberately does not introduce a mounted WAI application or a second
-- method/action dispatcher. A root combines the returned codec with its other
-- route families through 'combineRouteCodecs', so 404, 405, HEAD, and OPTIONS
-- remain decisions of one shared route boundary.
--
-- The internal split is by stable ownership: the core owns the immutable
-- module declaration and final root adapter, mounts own typed adaptation and
-- declaration validation, and composition owns sibling combination.  It is an
-- implementation-only health refactor; this public surface and its capability
-- boundaries remain unchanged.
--
-- Decision record (AHI-4B): a root may resolve an 'Either' with
-- 'requiredModuleConfiguration' only when both its inputs and the declaration
-- are fixed, construction-owned values.  That turns an impossible deployed
-- configuration into an immediate startup defect; it must not be used to hide
-- a request, adapter, or other runtime failure rail.
module HarchWeb.ApplicationModule
  ( ActionMount (..),
    AuthorizationProjection (..),
    ContextProjection (..),
    ApplicationModule (..),
    applicationModuleSite,
    installApplicationModule,
    inheritApplicationModuleGuards,
    ModuleMount (..),
    ModuleMountError (..),
    ModuleCompositionError (..),
    RouteMount (..),
    combineApplicationModules,
    mountApplicationModule,
    mountActionCodec,
    mountRouteCodec,
    requiredModuleConfiguration,
  )
where

import HarchWeb.ApplicationModule.Composition
  ( ModuleCompositionError (..),
    combineApplicationModules,
  )
import HarchWeb.ApplicationModule.Core
  ( ApplicationModule (..),
    applicationModuleSite,
    inheritApplicationModuleGuards,
    installApplicationModule,
    requiredModuleConfiguration,
  )
import HarchWeb.ApplicationModule.Mount
  ( ActionMount (..),
    AuthorizationProjection (..),
    ModuleMount (..),
    ModuleMountError (..),
    RouteMount (..),
    mountActionCodec,
    mountApplicationModule,
    mountRouteCodec,
  )
import HarchWeb.RequestContext (ContextProjection (..))
