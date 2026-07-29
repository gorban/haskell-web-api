-- | Typed application configuration and request middleware execution.
module HarchWeb.Server.Application
  ( Application (..),
    application,
    middlewareResultContext,
    runRequestMiddlewarePipeline,
  )
where

import Data.Text (Text)
import HarchWeb.Document (Document, NavigationRuntime, Page)
import HarchWeb.Observability qualified as Observability
import HarchWeb.Routing (RouteCodec, RouteRequest)
import HarchWeb.Security (RequestPolicyConfig)
import HarchWeb.Server.Response
  ( ClientActionRequest,
    ClientActionResponse,
    MiddlewareResult (..),
    RequestMiddleware (..),
    Response,
  )
import HarchWeb.StaticAssets (StaticAssetsConfig)
import Network.Wai qualified as Wai

data Application route context = Application
  { appName :: Text,
    defaultRequestContext :: context,
    requestContextFromRequest :: Wai.Request -> context -> context,
    applicationNavigationRuntime :: Maybe NavigationRuntime,
    applicationStaticAssets :: StaticAssetsConfig,
    applicationRequestPolicy :: RequestPolicyConfig,
    applicationRequestMiddleware :: [RequestMiddleware context],
    routeCodec :: RouteCodec route context,
    renderResponse :: RouteRequest route context -> IO (Response route context),
    handleClientAction :: ClientActionRequest context -> IO (Maybe ClientActionResponse),
    pageShell :: Page route context -> Document route,
    reportRequestObservability :: Observability.RequestObservability -> IO (),
    reportConnectionObservability :: Observability.ConnectionObservability -> IO (),
    reportApplicationLog :: Text -> IO ()
  }

application :: Application route context -> Application route context
application = id

middlewareResultContext :: MiddlewareResult context -> context
middlewareResultContext middlewareResult =
  case middlewareResult of
    ContinueMiddleware requestContext -> requestContext
    HaltMiddleware requestContext _ -> requestContext

-- | Run middleware in declaration order. The first middleware sees the
-- request first; a halt short-circuits the remaining middleware.
runRequestMiddlewarePipeline :: [RequestMiddleware context] -> Wai.Request -> context -> IO (MiddlewareResult context)
runRequestMiddlewarePipeline middleware request = go middleware
  where
    go [] requestContext = pure (ContinueMiddleware requestContext)
    go (RequestMiddleware runMiddleware : remainingMiddleware) requestContext = do
      result <- runMiddleware request requestContext
      case result of
        ContinueMiddleware nextRequestContext -> go remainingMiddleware nextRequestContext
        HaltMiddleware haltedRequestContext responseBodyValue -> pure (HaltMiddleware haltedRequestContext responseBodyValue)
