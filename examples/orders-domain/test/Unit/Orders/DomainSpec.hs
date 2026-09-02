{-# LANGUAGE OverloadedStrings #-}

module Unit.Orders.DomainSpec (spec) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import HarchWeb.Action qualified as Action
import HarchWeb.ApplicationModule (ApplicationModule (..))
import HarchWeb.Document (Page (..))
import HarchWeb.EndpointMetadata (AccessRequirement (RequireAuthorized), EndpointProtocol (ActionEndpoint, HtmlEndpoint), endpointAccess, endpointName, endpointNameText, endpointProtocol, endpointRouteTemplate, routeTemplateText)
import HarchWeb.Routing (RouteCodec (..), RouteLocation (..), RouteMethod (RouteGet), RouteParseResult (..), RouteRequest (..), requiredPathSegment, routeMethodPolicy)
import HarchWeb.Routing qualified as Routing
import HarchWeb.Server (ClientActionRequest (..), ClientActionResponse (..), Response (PageResponse), unboundedRouteExecutionPolicy)
import HarchWeb.Site (RouteDefinition (..))
import HarchWeb.Site qualified as Site
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import Orders.Domain
import Test.Hspec

spec :: Spec
spec = describe "Unit.Orders.Domain" $ do
  it "builds and executes its local query and command without root dependencies" $ do
    let ordersContext = OrdersContext "en" (Just "orders.write")
        queries = OrdersQueries (\domainContext -> pure (ordersLocaleCode domainContext <> " summary"))
        commands = OrdersCommands (\domainContext -> (domainContext `shouldBe` ordersContext) >> pure (OrderId ("order-" <> ordersLocaleCode domainContext)))
        moduleValue = buildOrdersModule queries commands
    checkDerived (OrderId "order-42")
    checkDerived OrdersIndex
    checkDerived SubmitOrderTarget
    checkDerived SubmitOrder
    checkDerived MayReadOrders
    checkDerived MaySubmitOrders
    ordersContext `shouldBe` OrdersContext "en" (Just "orders.write")
    checkDerived ordersContext
    ordersLocaleCode ordersContext `shouldBe` "en"
    ordersCustomerScope ordersContext `shouldBe` Just "orders.write"
    loadOrdersSummary queries ordersContext `shouldReturn` "en summary"
    submitOrder commands ordersContext `shouldReturn` OrderId "order-en"
    [MayReadOrders, MaySubmitOrders] `shouldBe` [MayReadOrders, MaySubmitOrders]
    moduleOwnsRoute moduleValue OrdersIndex `shouldBe` True
    show (moduleName moduleValue) `shouldBe` "ModuleName \"orders\""
    moduleRouteMountChain moduleValue OrdersIndex `shouldBe` moduleName moduleValue :| []
    moduleDeclaredRoutes moduleValue `shouldBe` [OrdersIndex]
    case moduleGuards moduleValue of
      [] -> pure ()
      _ -> expectationFailure "orders module should not install root-owned guards"
    parseRoute (moduleRouteCodec moduleValue) ordersContext (RouteLocation [] [])
      `shouldBe` RouteParsed (RouteRequest OrdersIndex ordersContext)
    parseRoute (moduleRouteCodec moduleValue) ordersContext (RouteLocation [requiredPathSegment "not-orders"] [])
      `shouldBe` RouteNotMatched
    renderRoute (moduleRouteCodec moduleValue) (RouteRequest OrdersIndex ordersContext)
      `shouldBe` RouteLocation [] []
    notFoundRequest (moduleRouteCodec moduleValue) ordersContext
      `shouldBe` RouteRequest OrdersIndex ordersContext
    Routing.routeMethods (moduleRouteCodec moduleValue) OrdersIndex `shouldBe` routeMethodPolicy [RouteGet]
    Action.decodeAction
      (moduleActionCodec moduleValue)
      Action.ClientActionPayload
        { Action.clientActionMethod = "POST",
          Action.clientActionPath = "/actions/submit",
          Action.clientActionFields = [],
          Action.clientActionCsrfToken = Nothing,
          Action.clientActionIdempotencyKey = Nothing,
          Action.clientActionPayloadContext = ordersContext
        }
      `shouldBe` Action.DecodedClientAction SubmitOrder
    Action.actionPath (moduleActionCodec moduleValue) ordersContext SubmitOrderTarget
      `shouldBe` Just "/actions/submit"
    Action.staticActionPath (moduleActionCodec moduleValue) SubmitOrderTarget
      `shouldBe` Just "/actions/submit"
    let definition = moduleEndpoints moduleValue OrdersIndex
    routeNavigationLabel definition `shouldBe` Just "Orders"
    endpointProtocol (routeMetadata definition) `shouldBe` HtmlEndpoint
    endpointAccess (routeMetadata definition) `shouldBe` RequireAuthorized MayReadOrders
    endpointNameText (endpointName (routeMetadata definition)) `shouldBe` "orders.index"
    routeTemplateText (endpointRouteTemplate (routeMetadata definition)) `shouldBe` "/"
    Site.routeMethods definition `shouldBe` [RouteGet]
    routeExecutionPolicy definition `shouldBe` unboundedRouteExecutionPolicy
    response <- routeResponse definition Wai.defaultRequest (RouteRequest OrdersIndex ordersContext)
    case response of
      PageResponse page -> do
        pageTitle page `shouldBe` "Orders"
        pageRoute page `shouldBe` OrdersIndex
        pageContext page `shouldBe` ordersContext
        pageBootstrapHooks page `shouldBe` []
        show (pageBody page) `shouldBe` "\"<h1>en summary</h1>\""
      _ -> expectationFailure "expected orders page"
    actionResult <- moduleHandleAction moduleValue (ClientActionRequest SubmitOrder Nothing ordersContext)
    case actionResult of
      Nothing -> expectationFailure "orders action must produce a response"
      Just actionResponse -> do
        clientActionStatus actionResponse `shouldBe` Http.status202
        clientActionPatches actionResponse `shouldBe` []
        clientActionFocusId actionResponse `shouldBe` Nothing
        clientActionHeaders actionResponse `shouldBe` []
        clientActionObservabilityAttributes actionResponse `shouldBe` []
        clientActionLogEntries actionResponse `shouldBe` []
    case Action.declaredActionEndpointMetadata (moduleActionCodec moduleValue) of
      [actionMetadata] -> do
        endpointNameText (endpointName actionMetadata) `shouldBe` "orders.submit"
        routeTemplateText (endpointRouteTemplate actionMetadata) `shouldBe` "/actions/submit"
        endpointProtocol actionMetadata `shouldBe` ActionEndpoint
        endpointAccess actionMetadata `shouldBe` RequireAuthorized MaySubmitOrders
      _ -> expectationFailure "orders module must declare exactly one action endpoint"

checkDerived :: (Eq value, Show value) => value -> Expectation
checkDerived value = do
  value == value `shouldBe` True
  value /= value `shouldBe` False
  shows value "" `shouldBe` show value
  showsPrec 11 value "" `shouldSatisfy` (not . null)
  showList [value] "" `shouldSatisfy` (not . null)
