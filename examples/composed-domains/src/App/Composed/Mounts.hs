{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Explicit Catalog and Orders adaptations into the composed root's closed
-- algebra.  The declarations live beside the root model, not in either domain
-- package, so domains remain independently buildable and cannot import the
-- application composition root.
module App.Composed.Mounts
  ( catalogModuleMount,
    ordersModuleMount,
  )
where

import App.Composed.Model
import Catalog.Domain
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (listToMaybe)
import HarchWeb.ApplicationModule
  ( ActionMount (..),
    AuthorizationProjection (..),
    ModuleMount (..),
    RouteMount (..),
  )
import HarchWeb.Localization (localeText)
import HarchWeb.RequestContext (ContextProjection (..), RequestContext (..), requestCore, requestIdentity, requestLocale)
import HarchWeb.Routing (requiredPathSegment)
import HarchWeb.SecurityEvent (requiredModuleNameOrDie)
import Orders.Domain

catalogModuleMount :: ModuleMount LocalizedRoute RootActionTarget RootAction ComposedContext RootAuthorization CatalogRoute CatalogActionTarget CatalogAction CatalogContext CatalogPolicy
catalogModuleMount =
  ModuleMount
    { mountedRoutes =
        RouteMount
          { routeMountName = requiredModuleNameOrDie "root.catalog",
            routeMountPrefix = requiredPathSegment "catalog" :| [],
            embedChildRoute = Catalog,
            projectChildRoute = \case
              Catalog route -> Just route
              _ -> Nothing
          },
      mountedActions =
        ActionMount
          { embedChildActionTarget = CatalogActionTarget,
            projectChildActionTarget = \case
              CatalogActionTarget target -> Just target
              _ -> Nothing,
            embedChildAction = CatalogAction,
            projectChildAction = \case
              CatalogAction childAction -> Just childAction
              _ -> Nothing
          },
      mountedContext = ContextProjection catalogContext,
      mountedAuthorization = AuthorizationProjection catalogAuthorization
    }

ordersModuleMount :: ModuleMount LocalizedRoute RootActionTarget RootAction ComposedContext RootAuthorization OrdersRoute OrdersActionTarget OrdersAction OrdersContext OrdersPolicy
ordersModuleMount =
  ModuleMount
    { mountedRoutes =
        RouteMount
          { routeMountName = requiredModuleNameOrDie "root.orders",
            routeMountPrefix = requiredPathSegment "orders" :| [],
            embedChildRoute = Orders,
            projectChildRoute = \case
              Orders route -> Just route
              _ -> Nothing
          },
      mountedActions =
        ActionMount
          { embedChildActionTarget = OrdersActionTarget,
            projectChildActionTarget = \case
              OrdersActionTarget target -> Just target
              _ -> Nothing,
            embedChildAction = OrdersAction,
            projectChildAction = \case
              OrdersAction childAction -> Just childAction
              _ -> Nothing
          },
      mountedContext = ContextProjection ordersContext,
      mountedAuthorization = AuthorizationProjection ordersAuthorization
    }

catalogContext :: ComposedContext -> CatalogContext
catalogContext requestContext =
  CatalogContext
    { catalogLocaleCode = localeText (requestLocale (requestCore requestContext)),
      catalogViewerScope = listToMaybe (principalScopes (requestIdentity requestContext))
    }

ordersContext :: ComposedContext -> OrdersContext
ordersContext requestContext =
  OrdersContext
    { ordersLocaleCode = localeText (requestLocale (requestCore requestContext)),
      ordersCustomerScope = listToMaybe (principalScopes (requestIdentity requestContext))
    }

catalogAuthorization :: CatalogPolicy -> RootAuthorization
catalogAuthorization policy =
  case policy of
    MayReadCatalog -> RootMayReadCatalog
    MayRefreshCatalog -> RootMayRefreshCatalog

ordersAuthorization :: OrdersPolicy -> RootAuthorization
ordersAuthorization policy =
  case policy of
    MayReadOrders -> RootMayReadOrders
    MaySubmitOrders -> RootMaySubmitOrders
