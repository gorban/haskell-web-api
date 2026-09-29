-- | Application-owned page construction over the framework's existing
-- 'HarchWeb.Site.RouteDefinition' and request/response types.
--
-- The page adapter receives the one route request and opaque 'PageSecurity'
-- value prepared by Site, then interprets its page-local load outcome once
-- into the existing 'HarchWeb.PageResult'. It extends the existing dispatch
-- boundary rather than creating another router or response interpreter.
-- 'PageDefinitionContext' is the generator's application assembly input;
-- individual page definitions extract only the capabilities they own.
-- Dynamic pages use this adapter for one typed load and pure outcome mapping;
-- static pages construct a 'RouteDefinition' directly instead of inventing a
-- loader or failure rail.
module WebApi.PageModule
  ( PageDefinitionContext (..),
    PageRequest (..),
    PageModule (..),
    pageModuleDefinition,
  )
where

import HarchWeb
  ( EndpointMetadata,
    PageResult,
    RouteMethodPolicy,
    RouteRequest,
    unboundedRouteExecutionPolicy,
  )
import HarchWeb.Csrf (PageSecurity)
import HarchWeb.Site
  ( RouteDefinition (..),
    RouteHandler (PageRouteHandler),
    RouteNavigation,
  )
import WebApi.Config (AppConfig)
import WebApi.Database (PageRepository)
import WebApi.Route
  ( AppAuthorization,
    AppRequestContext,
    AppRoute,
  )

-- | Dependencies passed to generated page definitions at application
-- assembly. 'AppConfig' remains configuration-only; page modules select their
-- own repository or other cohesive capabilities from this record.
data PageDefinitionContext = PageDefinitionContext
  { pageDefinitionConfig :: AppConfig,
    pageDefinitionPageRepository :: PageRepository
  }

-- | The request facts a page needs after route selection and before loading.
-- 'PageSecurity' is the exact opaque value prepared once by Site; pages pass
-- it to existing controls and do not reconstruct or retain it.
data PageRequest = PageRequest
  { pageRequestRoute :: RouteRequest AppRoute AppRequestContext,
    pageRequestSecurity :: PageSecurity
  }

-- | One page's existing route declaration, load operation, and pure outcome
-- interpretation. Load outcomes stay specific to each page.
data PageModule dependencies outcome = PageModule
  { pageModuleEndpointMetadata :: EndpointMetadata AppAuthorization,
    pageModuleNavigation :: AppRequestContext -> Maybe RouteNavigation,
    pageModuleMethods :: RouteMethodPolicy,
    pageModuleLoad :: dependencies -> PageRequest -> IO outcome,
    pageModuleRespond :: PageRequest -> outcome -> PageResult AppRoute AppRequestContext
  }

-- | Compose one page adapter into the application's existing route table.
-- Admission metadata, method policy, and execution policy continue through
-- the shared Site dispatcher; the page receives the request and its already
-- prepared security value only after dispatch has selected it.
pageModuleDefinition ::
  PageModule dependencies outcome ->
  dependencies ->
  RouteDefinition AppRoute AppRequestContext AppAuthorization
pageModuleDefinition pageModule dependencies =
  RouteDefinition
    { routeNavigation = pageModuleNavigation pageModule,
      routeMetadata = pageModuleEndpointMetadata pageModule,
      routeMethods = const (pageModuleMethods pageModule),
      routeExecutionPolicy = unboundedRouteExecutionPolicy,
      routeHandler =
        PageRouteHandler $ \pageSecurity routeRequest -> do
          let pageRequest = PageRequest routeRequest pageSecurity
          outcome <- pageModuleLoad pageModule dependencies pageRequest
          pure (pageModuleRespond pageModule pageRequest outcome)
    }
