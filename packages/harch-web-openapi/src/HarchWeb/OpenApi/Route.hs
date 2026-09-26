{-# LANGUAGE OverloadedStrings #-}

-- | The ordinary typed route adapter for a prepared OpenAPI document.
--
-- Decision record (AHI-4E, 2026-09-23): adapt an application-selected
-- 'OpenApiDocumentProvider' through the existing 'RouteDefinition' and
-- 'ProtocolRouteHandler' boundary.  That boundary already owns route
-- selection, method negotiation, endpoint security, response-security
-- headers, observability, and final WAI rendering; this module supplies no
-- second documentation dispatcher.  Applications keep ownership of the
-- route constructor, mount, endpoint metadata, and authorization policy.
--
-- A provider failure produces a deliberately detail-free 503.  The provider
-- error is useful to the application that constructs a dynamic provider, but
-- is not safe public diagnostic text.  The adapter serves encoded bytes only;
-- the later Swagger page and asset slices remain separate work.
module HarchWeb.OpenApi.Route
  ( openApiDocumentRouteDefinition,
  )
where

import Data.ByteString.Lazy qualified as LazyByteString
import HarchWeb.EndpointMetadata (EndpointMetadata)
import HarchWeb.OpenApi.Provider
  ( OpenApiDocumentProvider,
    PreparedOpenApiDocument,
    prepareOpenApiDocument,
    preparedOpenApiDocumentBytes,
  )
import HarchWeb.Routing (RouteMethod (RouteGet), RouteRequest (..), routeMethodPolicy)
import HarchWeb.Server
  ( NonPageResponse (NonPageProtocolResponse),
    ProtocolResponse (..),
    ProtocolResponseBody (ProtocolResponseBytes),
    unboundedRouteExecutionPolicy,
  )
import HarchWeb.Site (RouteDefinition (..), RouteHandler (ProtocolRouteHandler))
import Network.HTTP.Types qualified as Http

-- | Declare one GET documentation-specification endpoint.  The surrounding
-- application route codec continues to decide which path owns this
-- definition.  A GET declaration also receives the framework's established
-- HEAD and OPTIONS behavior through the shared dispatcher.
--
-- Per @docs/design-guidance.md@'s never-mask-a-gate-finding rule: the @$!@
-- below is a confirmed, reproducible fix, not a guess. Both 'RouteSpec'
-- tests genuinely call this handler, but 'requestContext' applied to
-- 'request' is a bare record-accessor result used as a direct argument to
-- the already-HPC-instrumented 'prepareOpenApiDocument' call, the documented
-- pattern where HPC permanently leaves the occurrence unticked despite real
-- execution. There is no duplicate expression here to name away, and
-- 'requestContext request' is otherwise unread by the handler, so no
-- assertion on it can force the tick a different way.
{-# ANN openApiDocumentRouteDefinition ("HLint: ignore Redundant $!" :: String) #-}
openApiDocumentRouteDefinition :: EndpointMetadata authorization -> OpenApiDocumentProvider context -> RouteDefinition route context authorization
openApiDocumentRouteDefinition metadata provider =
  RouteDefinition
    { routeNavigationLabel = Nothing,
      routeMetadata = metadata,
      routeMethods = const (routeMethodPolicy [RouteGet]),
      routeExecutionPolicy = unboundedRouteExecutionPolicy,
      routeHandler = ProtocolRouteHandler $ \_ request -> do
        preparedDocument <- prepareOpenApiDocument provider $! requestContext request
        pure (documentResponse preparedDocument)
    }

documentResponse :: Either failure PreparedOpenApiDocument -> NonPageResponse route context
documentResponse preparedDocument =
  NonPageProtocolResponse $
    case preparedDocument of
      Right document ->
        ProtocolResponse
          { protocolResponseStatus = Http.status200,
            protocolResponseHeaders = [(Http.hContentType, "application/vnd.oai.openapi+json;version=3.0")],
            protocolResponseBody = ProtocolResponseBytes (LazyByteString.toStrict (preparedOpenApiDocumentBytes document)),
            protocolResponseObservabilityAttributes = [],
            protocolResponseLogEntries = [],
            protocolResponseDatabaseOperations = []
          }
      Left _ ->
        ProtocolResponse
          { protocolResponseStatus = Http.status503,
            protocolResponseHeaders = [(Http.hContentType, "text/plain; charset=utf-8"), ("Cache-Control", "no-store")],
            protocolResponseBody = ProtocolResponseBytes "OpenAPI document unavailable\n",
            protocolResponseObservabilityAttributes = [],
            protocolResponseLogEntries = [],
            protocolResponseDatabaseOperations = []
          }
