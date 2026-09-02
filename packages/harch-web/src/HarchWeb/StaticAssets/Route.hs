-- | Typed route ownership for the existing static-asset delivery interpreter.
-- The codec operates on decoded locations; the one filesystem implementation
-- remains in 'HarchWeb.Server.StaticAssets'.
module HarchWeb.StaticAssets.Route
  ( StaticAssetRoute (..),
    staticAssetRouteCodec,
    staticAssetRouteDefinition,
    staticAssetRouteResponse,
  )
where

import HarchWeb.EndpointMetadata (EndpointMetadata)
import HarchWeb.Routing
  ( PathSegment,
    RouteCodec (..),
    RouteLocation (..),
    RouteMethod (RouteGet),
    RouteParseResult (..),
    RouteRequest (..),
    routeMethodPolicy,
  )
import HarchWeb.Server
  ( ProtocolResponse (..),
    ProtocolResponseBody (ProtocolResponseWai),
    Response (ProtocolResponseResult),
    unboundedRouteExecutionPolicy,
  )
import HarchWeb.Server.StaticAssets
  ( serveStaticAssetPathResponse,
    staticAssetRouteOwnsPath,
  )
import HarchWeb.Site (RouteDefinition (..))
import HarchWeb.StaticAssets (StaticAssetsConfig)
import Network.Wai qualified as Wai

-- | A selected static request retains only decoded path segments. Query fields
-- are deliberately not filesystem selectors.
newtype StaticAssetRoute = StaticAssetRoute
  { staticAssetPathSegments :: [PathSegment]
  }
  deriving (Eq, Show)

staticAssetRouteCodec :: StaticAssetsConfig -> RouteCodec StaticAssetRoute context
staticAssetRouteCodec staticAssetsConfig =
  RouteCodec
    { parseRoute = \requestContext location ->
        if staticAssetRouteOwnsPath staticAssetsConfig (routePathSegments location)
          then RouteParsed (RouteRequest (StaticAssetRoute (routePathSegments location)) requestContext)
          else RouteNotMatched,
      renderRoute = \routeRequest -> RouteLocation (staticAssetPathSegments (requestRoute routeRequest)) [],
      notFoundRequest = RouteRequest (StaticAssetRoute []),
      routeMethods = const (routeMethodPolicy [RouteGet])
    }

staticAssetRouteDefinition :: StaticAssetsConfig -> EndpointMetadata authorization -> RouteDefinition StaticAssetRoute context authorization
staticAssetRouteDefinition staticAssetsConfig metadata =
  RouteDefinition
    { routeNavigationLabel = Nothing,
      routeMetadata = metadata,
      routeMethods = [RouteGet],
      routeExecutionPolicy = unboundedRouteExecutionPolicy,
      routeResponse = \request routeRequest ->
        ProtocolResponseResult <$> staticAssetRouteResponse staticAssetsConfig request (requestRoute routeRequest)
    }

-- | Execute the existing static interpreter for a route that has already
-- passed typed static-route ownership. The response does not depend on the
-- enclosing page context, so a composed route can adapt it without inventing
-- a child request context merely to satisfy a page-route-shaped callback.
staticAssetRouteResponse :: StaticAssetsConfig -> Wai.Request -> StaticAssetRoute -> IO ProtocolResponse
staticAssetRouteResponse staticAssetsConfig request assetRoute = do
  served <- serveStaticAssetPathResponse staticAssetsConfig request (staticAssetPathSegments assetRoute)
  case served of
    Nothing -> error "selected static route was not owned by its configured static assets"
    Just (_, waiResponse) ->
      pure
        ProtocolResponse
          { protocolResponseStatus = Wai.responseStatus waiResponse,
            protocolResponseHeaders = [],
            protocolResponseBody = ProtocolResponseWai waiResponse,
            protocolResponseObservabilityAttributes = [],
            protocolResponseLogEntries = [],
            protocolResponseDatabaseOperations = []
          }
