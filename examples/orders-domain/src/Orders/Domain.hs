{-# LANGUAGE OverloadedStrings #-}

-- | An independently packaged Orders module.  Its read model and command
-- adapter are explicit constructor inputs; neither needs a composition root
-- route, authentication implementation, or server-owned capability.
module Orders.Domain
  ( OrderId (..),
    OrdersAction (..),
    OrdersActionTarget (..),
    OrdersCommands (..),
    OrdersContext (..),
    OrdersPolicy (..),
    OrdersQueries (..),
    OrdersRoute (..),
    buildOrdersModule,
  )
where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import HarchWeb.Action (ActionCodec, post, singleActionCodecWithMetadata)
import HarchWeb.ApplicationModule (ApplicationModule (..))
import HarchWeb.Document (Page (..))
import HarchWeb.EndpointMetadata
  ( AccessRequirement (RequireAuthorized),
    EndpointProtocol (ActionEndpoint, HtmlEndpoint),
    mkEndpointMetadata,
    requiredEndpointNameOrDie,
    requiredRouteTemplateOrDie,
  )
import HarchWeb.Markup (element, headingOneTag, text)
import HarchWeb.Routing
  ( RouteCodec (..),
    RouteLocation (..),
    RouteMethod (RouteGet),
    RouteParseResult (..),
    RouteRequest (..),
    routeMethodPolicy,
  )
import HarchWeb.SecurityEvent (requiredModuleNameOrDie)
import HarchWeb.Server
  ( ClientActionRequest (..),
    ClientActionResponse (..),
    Response (PageResponse),
    unboundedRouteExecutionPolicy,
  )
import HarchWeb.Site (RouteDefinition (..))
import Network.HTTP.Types qualified as Http

newtype OrderId = OrderId Text
  deriving (Eq, Show)

data OrdersRoute = OrdersIndex
  deriving (Eq, Show)

data OrdersActionTarget = SubmitOrderTarget
  deriving (Eq, Show)

data OrdersAction = SubmitOrder
  deriving (Eq, Show)

data OrdersPolicy
  = MayReadOrders
  | MaySubmitOrders
  deriving (Eq, Show)

data OrdersContext = OrdersContext
  { ordersLocaleCode :: Text,
    ordersCustomerScope :: Maybe Text
  }
  deriving (Eq, Show)

newtype OrdersQueries = OrdersQueries
  { loadOrdersSummary :: OrdersContext -> IO Text
  }

newtype OrdersCommands = OrdersCommands
  { submitOrder :: OrdersContext -> IO OrderId
  }

buildOrdersModule :: OrdersQueries -> OrdersCommands -> ApplicationModule OrdersRoute OrdersActionTarget OrdersAction OrdersContext OrdersPolicy
buildOrdersModule queries commands =
  ApplicationModule
    { moduleName = requiredModuleNameOrDie "orders",
      moduleOwnsRoute = const True,
      moduleRouteMountChain = const (requiredModuleNameOrDie "orders" NonEmpty.:| []),
      moduleRouteCodec = ordersRouteCodec,
      moduleDeclaredRoutes = [OrdersIndex],
      moduleEndpoints = ordersRouteDefinition queries,
      moduleActionCodec = ordersActionCodec,
      moduleHandleAction = ordersActionHandler commands,
      moduleGuards = []
    }

ordersRouteCodec :: RouteCodec OrdersRoute OrdersContext
ordersRouteCodec =
  RouteCodec
    { parseRoute = \requestContext location ->
        case routePathSegments location of
          [] -> RouteParsed (RouteRequest OrdersIndex requestContext)
          _ -> RouteNotMatched,
      renderRoute = const (RouteLocation [] []),
      notFoundRequest = RouteRequest OrdersIndex,
      routeMethods = const (routeMethodPolicy [RouteGet])
    }

ordersRouteDefinition :: OrdersQueries -> OrdersRoute -> RouteDefinition OrdersRoute OrdersContext OrdersPolicy
ordersRouteDefinition queries OrdersIndex =
  RouteDefinition
    { routeNavigationLabel = Just "Orders",
      routeMetadata =
        mkEndpointMetadata
          (requiredEndpointNameOrDie "orders.index")
          (requiredRouteTemplateOrDie "/")
          HtmlEndpoint
          (RequireAuthorized MayReadOrders),
      routeMethods = [RouteGet],
      routeExecutionPolicy = unboundedRouteExecutionPolicy,
      routeResponse = \_ request -> do
        summary <- loadOrdersSummary queries (requestContext request)
        pure
          ( PageResponse
              Page
                { pageTitle = "Orders",
                  pageRoute = OrdersIndex,
                  pageContext = requestContext request,
                  pageBody = element headingOneTag [] [text summary],
                  pageBootstrapHooks = []
                }
          )
    }

ordersActionCodec :: ActionCodec OrdersActionTarget OrdersContext OrdersPolicy OrdersAction
ordersActionCodec =
  singleActionCodecWithMetadata
    SubmitOrderTarget
    (post "/actions/submit")
    ( mkEndpointMetadata
        (requiredEndpointNameOrDie "orders.submit")
        (requiredRouteTemplateOrDie "/actions/submit")
        ActionEndpoint
        (RequireAuthorized MaySubmitOrders)
    )
    (pure SubmitOrder)

ordersActionHandler :: OrdersCommands -> ClientActionRequest OrdersAction OrdersContext -> IO (Maybe ClientActionResponse)
ordersActionHandler commands actionRequest =
  case clientAction actionRequest of
    SubmitOrder -> do
      _ <- submitOrder commands (clientActionContext actionRequest)
      pure
        ( Just
            ClientActionResponse
              { clientActionStatus = Http.status202,
                clientActionPatches = [],
                clientActionFocusId = Nothing,
                clientActionHeaders = [],
                clientActionObservabilityAttributes = [],
                clientActionLogEntries = []
              }
        )
