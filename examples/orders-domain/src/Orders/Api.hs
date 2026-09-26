{-# LANGUAGE OverloadedStrings #-}

-- | The @orders.api@ application module (AHI-4E composed-domains slice):
-- a second, distinct module the composed root mounts at @/api/orders@ while
-- @orders.web@ keeps the HTML surface at @/orders@. Both are transports over
-- the same 'OrdersCommands' port: this module exposes
-- @POST /api/orders@ (policy 'MaySubmitOrders'), decodes a small typed
-- command, calls 'submitOrder', and returns the 'OrderId' with
-- @202 Accepted@.
--
-- The module constructor takes the generic API extension value exactly like
-- 'Catalog.Api.buildCatalogApiModule': the domain compiles unchanged without
-- OpenAPI while the composed root supplies the documented assembly. The
-- bearer-only POST carries no CSRF requirement under the recommended
-- profile; the package owns only its abstract policy.
module Orders.Api
  ( OrdersApiAction,
    OrdersApiActionTarget,
    OrdersApiRoute (..),
    SubmitOrderCommand (..),
    buildOrdersApiModule,
    ordersApiContract,
    ordersApiEndpoint,
  )
where

import Data.Aeson (encode, object, (.=))
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb
  ( AccessRequirement (RequireAuthorized),
    EndpointMetadata,
    EndpointProtocol (ApiEndpoint),
    mkEndpointMetadata,
    requiredEndpointNameOrDie,
    requiredModuleNameOrDie,
    requiredRouteTemplateOrDie,
  )
import HarchWeb.Action (emptyActionCodec)
import HarchWeb.Api
  ( ApiEndpointContract (..),
    ApiEndpointRequest (apiEndpointRequestFields),
    ApiFieldFailurePolicy (ApiUseGenericFieldFailure),
    ApiFieldValue,
    ApiForm,
    ApiMethod (ApiPost),
    ApiRequestBody (ApiUrlEncodedFormRequestBody),
    ApiRequestBodyByteLimit,
    ApiResponse (apiEndpointResponseStatus),
    ApiRouteEndpointDeclaration (..),
    MissingContentTypePolicy (RejectMissingContentType),
    RequestCodec,
    apiContentType,
    apiResponse,
    apiRouteDefinition,
    apiRouteEndpointWithContextNeverFailing,
    at,
    bytesResponseEncoder,
    formField,
    jsonMediaType,
    mkApiFieldValue,
    requireApiRequestBodyByteLimit,
    requiredField,
  )
import HarchWeb.ApplicationModule (ApplicationModule (..))
import HarchWeb.Routing
  ( RouteCodec (..),
    RouteLocation (..),
    RouteMethod (RoutePost),
    RouteParseResult (RouteNotMatched, RouteParsed),
    RouteRequest (..),
    pathSegmentText,
    routeMethodPolicy,
    routePathSegments,
  )
import HarchWeb.Site (RouteDefinition)
import Network.HTTP.Types (status202)
import Orders.Domain (OrdersCommands (submitOrder), OrdersContext, OrdersPolicy (MaySubmitOrders))

-- | The API module's declared routes. @OrdersSubmit@ is the local fragment
-- @/@ (the collection root); the composed root's mount chain yields the full
-- trusted template @/api/orders@.
-- | Uninhabited: the API module ships 'emptyActionCodec', so no client
-- action can ever be produced. The module mount's embedders are total
-- matches on an empty type.
data OrdersApiActionTarget

data OrdersApiAction

data OrdersApiRoute
  = OrdersSubmit
  deriving (Eq, Show)

-- | The small typed command the POST decodes: one non-empty item name.
newtype SubmitOrderCommand = SubmitOrderCommand
  { submitOrderItem :: Text
  }
  deriving (Eq, Show)

submitOrderRequestFields :: RequestCodec SubmitOrderCommand
submitOrderRequestFields =
  SubmitOrderCommand <$> requiredField (formField "item" itemFieldValue)

itemFieldValue :: ApiFieldValue Text
itemFieldValue =
  mkApiFieldValue
    ( \value ->
        if Text.null value
          then Nothing
          else Just value
    )

ordersRequestBodyByteLimit :: ApiRequestBodyByteLimit
ordersRequestBodyByteLimit = requireApiRequestBodyByteLimit 2048

-- | The contract parameterized by the generic extension: a form POST with one
-- required field and a JSON response, mirroring web-api's token contract's
-- body shape.
ordersApiContract ::
  extension SubmitOrderCommand ApiForm ByteString.ByteString ->
  ApiEndpointContract extension SubmitOrderCommand ApiForm ByteString.ByteString
ordersApiContract =
  ApiEndpointContract
    ApiPost
    submitOrderRequestFields
    ( ApiUrlEncodedFormRequestBody
        RejectMissingContentType
        ordersRequestBodyByteLimit
        4
    )
    (bytesResponseEncoder (apiContentType jsonMediaType) :| [])
    ApiUseGenericFieldFailure

-- | The endpoint: decode the command, call the domain's command port, and
-- return the 'OrderId' as JSON with @202 Accepted@.
ordersApiEndpoint ::
  extension SubmitOrderCommand ApiForm ByteString.ByteString ->
  OrdersCommands ->
  OrdersContext ->
  ApiEndpointRequest SubmitOrderCommand ApiForm ->
  IO (ApiResponse ByteString.ByteString)
ordersApiEndpoint _extension commands context endpointRequest = do
  let command = apiEndpointRequestFields endpointRequest
  orderId <- submitOrder commands context
  pure
    ( (apiResponse (LazyByteString.toStrict (encode (object ["orderId" .= orderIdText orderId command]))))
        { apiEndpointResponseStatus = status202
        }
    )
  where
    orderIdText _orderId = submitOrderItem

-- | Build the @orders.api@ module. Supplying an extension value keeps the
-- constructor generic over the documented/undocumented assembly.
buildOrdersApiModule ::
  extension SubmitOrderCommand ApiForm ByteString.ByteString ->
  OrdersCommands ->
  ApplicationModule OrdersApiRoute OrdersApiActionTarget OrdersApiAction OrdersContext OrdersPolicy
buildOrdersApiModule extension commands =
  ApplicationModule
    { moduleName = requiredModuleNameOrDie "orders.api",
      moduleOwnsRoute = const True,
      moduleRouteMountChain = const (requiredModuleNameOrDie "orders.api" :| []),
      moduleRouteCodec = ordersApiRouteCodec,
      moduleDeclaredRoutes = [OrdersSubmit],
      moduleEndpoints = ordersRouteDefinition extension commands,
      moduleActionCodec = emptyActionCodec,
      moduleActionRoute = \_ _ -> Nothing,
      moduleHandleAction = \_ -> pure Nothing,
      moduleGuards = []
    }

ordersApiRouteCodec :: RouteCodec OrdersApiRoute OrdersContext
ordersApiRouteCodec =
  RouteCodec
    { parseRoute = \requestContext location ->
        case routePathSegments location of
          [] -> RouteParsed (RouteRequest OrdersSubmit requestContext)
          [segment] | pathSegmentText segment == "items" -> RouteParsed (RouteRequest OrdersSubmit requestContext)
          _ -> RouteNotMatched,
      renderRoute = const (RouteLocation [] []),
      notFoundRequest = RouteRequest OrdersSubmit,
      routeMethods = const (routeMethodPolicy [RoutePost])
    }

ordersRouteDefinition ::
  extension SubmitOrderCommand ApiForm ByteString.ByteString ->
  OrdersCommands ->
  OrdersApiRoute ->
  RouteDefinition OrdersApiRoute OrdersContext OrdersPolicy
ordersRouteDefinition extension commands OrdersSubmit =
  apiRouteDefinition
    ordersEndpointMetadata
    (apiRouteEndpointWithContextNeverFailing (ApiRouteEndpointDeclaration (at "/") (ordersApiContract extension)) (ordersApiEndpoint extension commands))

ordersEndpointMetadata :: EndpointMetadata OrdersPolicy
ordersEndpointMetadata =
  mkEndpointMetadata
    (requiredEndpointNameOrDie "orders.submit")
    (requiredRouteTemplateOrDie "/")
    ApiEndpoint
    (RequireAuthorized MaySubmitOrders)
