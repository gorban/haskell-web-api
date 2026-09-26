{-# LANGUAGE OverloadedStrings #-}

module Unit.Orders.ApiSpec (spec) where

import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty ((:|)))
import HarchWeb.Action qualified as Action
import HarchWeb.Api
  ( ApiEndpointContract (..),
    ApiFieldFailurePolicy (ApiRenderFieldFailures, ApiUseGenericFieldFailure),
    ApiMethod (ApiPost),
    ApiRequestBody (ApiUrlEncodedFormRequestBody),
    ApiRequestData (..),
    ApiRequestDecodeResult (..),
    ApiRequestParseError (..),
    ApiRequestSource (ApiFormSource),
    ApiResponse (..),
    ApiRouteEndpointDeclaration (..),
    MissingContentTypePolicy (RejectMissingContentType),
    NoApiExtension (..),
    SomeApiRouteEndpoint (..),
    apiContentType,
    apiPathText,
    apiRequestBodyByteLimitValue,
    apiResponseEncoderContentType,
    jsonMediaType,
    mapApiEndpointFamily,
    runRequestCodec,
    withApiRouteEndpointDeclaration,
  )
import HarchWeb.ApplicationModule (ApplicationModule (..))
import HarchWeb.EndpointMetadata (AccessRequirement (RequireAuthorized), EndpointProtocol (ApiEndpoint), endpointAccess, endpointName, endpointNameText, endpointProtocol, endpointRouteTemplate, routeTemplateText)
import HarchWeb.Routing (RouteCodec (..), RouteLocation (..), RouteMethod (RoutePost), RouteParseResult (..), RouteRequest (..), requiredPathSegment, routeMethodPolicy)
import HarchWeb.Routing qualified as Routing
import HarchWeb.Server (NonPageResponse (..), ProtocolResponse (..), ProtocolResponseBody (..))
import HarchWeb.Site (RouteDefinition (..), RouteHandler (..))
import HarchWeb.Site qualified as Site
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import Orders.Api
import Orders.Domain (OrderId (..), OrdersCommands (..), OrdersContext (..), OrdersPolicy (MaySubmitOrders))
import Test.Hspec

spec :: Spec
spec = describe "Unit.Orders.Api" $ do
  it "declares its typed POST form contract and order-id JSON response" $ do
    let contract = ordersApiContract NoApiExtension
        commands = OrdersCommands (\ordersContextValue -> pure (OrderId (ordersLocaleCode ordersContextValue <> "-order")))
        ordersContext = OrdersContext "en" (Just "orders.write")
    case apiEndpointContractMethod contract of
      ApiPost -> pure ()
      _ -> expectationFailure "orders submit must be a POST"
    case apiEndpointContractBody contract of
      ApiUrlEncodedFormRequestBody RejectMissingContentType bodyByteLimit fieldLimit -> do
        apiRequestBodyByteLimitValue bodyByteLimit `shouldBe` 2048
        fieldLimit `shouldBe` 4
      _ -> expectationFailure "orders submit must take a url-encoded form body"
    case apiEndpointContractFieldFailurePolicy contract of
      ApiUseGenericFieldFailure -> pure ()
      ApiRenderFieldFailures _ -> expectationFailure "the contract must use the generic field failure"
    case apiEndpointContractEncoders contract of
      responseEncoder :| _ ->
        apiResponseEncoderContentType responseEncoder `shouldBe` apiContentType jsonMediaType
    response <- ordersApiHandler NoApiExtension commands ordersContext (error "orders submit reads only its decoded fields")
    apiEndpointResponseStatus response `shouldBe` Http.status202
    apiEndpointResponseValue response `shouldBe` "{\"orderId\":\"en-order\"}"

  it "validates its typed item form with total field decoding" $ do
    let fields = apiEndpointContractFields (ordersApiContract NoApiExtension)
    case runRequestCodec fields (ApiRequestData [] [] [] [("item", "widget")]) of
      ApiRequestDecoded command -> submitOrderItem command `shouldBe` "widget"
      _ -> expectationFailure "a non-empty item must decode"
    case runRequestCodec fields (ApiRequestData [] [] [] []) of
      ApiRequestRejected errors -> errors `shouldBe` (MissingApiField ApiFormSource "item" :| [])
      _ -> expectationFailure "a missing item must be rejected"
    case runRequestCodec fields (ApiRequestData [] [] [] [("item", "")]) of
      ApiRequestRejected errors -> errors `shouldBe` (InvalidApiField ApiFormSource "item" :| [])
      _ -> expectationFailure "an empty item must be rejected"
    checkDerived OrdersSubmit
    checkDerived (SubmitOrderCommand "widget")

  it "builds its module and executes the submit endpoint through the route definition" $ do
    let commands = OrdersCommands (\ordersContextValue -> pure (OrderId (ordersLocaleCode ordersContextValue <> "-order")))
        moduleValue = buildOrdersApiModule NoApiExtension commands
        ordersContext = OrdersContext "en" Nothing
    show (moduleName moduleValue) `shouldBe` "ModuleName \"orders.api\""
    moduleOwnsRoute moduleValue OrdersSubmit `shouldBe` True
    moduleRouteMountChain moduleValue OrdersSubmit `shouldBe` moduleName moduleValue :| []
    moduleDeclaredRoutes moduleValue `shouldBe` [OrdersSubmit]
    case moduleGuards moduleValue of
      [] -> pure ()
      _ -> expectationFailure "orders api module must not install guards"
    case Action.declaredActionEndpointMetadata (moduleActionCodec moduleValue) of
      [] -> pure ()
      _ -> expectationFailure "orders api module must declare no action endpoints"
    parseRoute (moduleRouteCodec moduleValue) ordersContext (RouteLocation [] [])
      `shouldBe` RouteParsed (RouteRequest OrdersSubmit ordersContext)
    parseRoute (moduleRouteCodec moduleValue) ordersContext (RouteLocation [requiredPathSegment "items"] [])
      `shouldBe` RouteNotMatched
    parseRoute (moduleRouteCodec moduleValue) ordersContext (RouteLocation [requiredPathSegment "submit"] [])
      `shouldBe` RouteNotMatched
    renderRoute (moduleRouteCodec moduleValue) (RouteRequest OrdersSubmit ordersContext)
      `shouldBe` RouteLocation [] []
    notFoundRequest (moduleRouteCodec moduleValue) ordersContext
      `shouldBe` RouteRequest OrdersSubmit ordersContext
    Routing.routeMethods (moduleRouteCodec moduleValue) (RouteRequest OrdersSubmit ordersContext) `shouldBe` routeMethodPolicy [RoutePost]
    -- The API module's action algebra is uninhabited, so both projections are
    -- constant on values that cannot exist; the tests supply bottom stand-ins
    -- to pin that constant behavior.
    moduleActionRoute moduleValue ordersContext (error "OrdersApiActionTarget is uninhabited") `shouldBe` Nothing
    actionResult <- moduleHandleAction moduleValue (error "OrdersApiAction cannot be requested")
    case actionResult of
      Nothing -> pure ()
      Just _ -> expectationFailure "orders api module must not handle actions"
    let definition = moduleEndpoints moduleValue OrdersSubmit
    routeNavigationLabel definition `shouldBe` Nothing
    endpointNameText (endpointName (routeMetadata definition)) `shouldBe` "orders.submit"
    routeTemplateText (endpointRouteTemplate (routeMetadata definition)) `shouldBe` "/"
    endpointProtocol (routeMetadata definition) `shouldBe` ApiEndpoint
    endpointAccess (routeMetadata definition) `shouldBe` RequireAuthorized MaySubmitOrders
    Site.routeMethods definition (RouteRequest OrdersSubmit ordersContext) `shouldBe` routeMethodPolicy [RoutePost]
    case routeHandler definition of
      ProtocolRouteHandler renderProtocol -> do
        formChunks <- newIORef ["item=widget"]
        let formRequest =
              Wai.setRequestBodyChunks
                ( do
                    chunks <- readIORef formChunks
                    case chunks of
                      [] -> pure ""
                      chunk : remaining -> writeIORef formChunks remaining >> pure chunk
                )
                ( Wai.defaultRequest
                    { Wai.requestMethod = "POST",
                      Wai.requestHeaders = [("Content-Type", "application/x-www-form-urlencoded")]
                    }
                )
        nonPage <- renderProtocol formRequest (RouteRequest OrdersSubmit ordersContext)
        case nonPage of
          NonPageProtocolResponse protocolResponse -> do
            protocolResponseStatus protocolResponse `shouldBe` Http.status202
            case protocolResponseBody protocolResponse of
              ProtocolResponseBytes bodyBytes -> bodyBytes `shouldBe` "{\"orderId\":\"en-order\"}"
              _ -> expectationFailure "expected strict protocol bytes"
          _ -> expectationFailure "expected a protocol response"
      PageRouteHandler _ -> expectationFailure "orders submit is a protocol endpoint"

  it "exposes its endpoint and documented family over the submit port" $ do
    let commands = OrdersCommands (\_ -> pure (OrderId "order-7"))
    case ordersApiEndpoint NoApiExtension commands of
      SomeApiRouteEndpoint _ -> pure ()
    case ordersFamily NoApiExtension commands of
      Left _ -> expectationFailure "orders family must validate"
      Right family ->
        mapApiEndpointFamily
          ( \endpoint ->
              withApiRouteEndpointDeclaration endpoint $ \declaration ->
                ( apiPathText (apiRouteEndpointDeclarationPath declaration),
                  apiEndpointContractMethod (apiRouteEndpointDeclarationContract declaration)
                )
          )
          family
          `shouldBe` [("/", ApiPost)]

checkDerived :: (Eq value, Show value) => value -> Expectation
checkDerived value = do
  value == value `shouldBe` True
  value /= value `shouldBe` False
  shows value "" `shouldBe` show value
  showsPrec 11 value "" `shouldSatisfy` (not . null)
  showList [value] "" `shouldSatisfy` (not . null)
