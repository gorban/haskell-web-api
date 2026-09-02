-- | Typed application configuration and request middleware execution.
--
-- Decision record (CZ, 2026-08-23): CSRF issuance and authorization extend
-- this existing typed application/action boundary rather than adding a second
-- action dispatcher.  Response rendering already owns page cookies and the
-- capture kernel, while request execution already owns typed action decoding
-- immediately before a handler runs.  The framework therefore owns strict
-- host-cookie parsing, token syntax, and constant-work double-submit
-- comparison; applications supply the page-context token and decide whether a
-- decoded action requires that token to match a live application session.
-- This keeps anonymous actions available without a session, binds privileged
-- actions to their existing application-owned session store, and preserves
-- one route/action interpreter.
module HarchWeb.Server.Application
  ( Application (..),
    RouteExecutionPolicy (..),
    application,
    renderResponse,
    middlewareResultContext,
    unboundedRouteExecutionPolicy,
    runRequestMiddlewarePipeline,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import HarchWeb.Document (Document, NavigationRuntime, Page, RuntimeAsset)
import HarchWeb.EndpointSecurity (ApplicationSecurity, EndpointMetadata)
import HarchWeb.Observability qualified as Observability
import HarchWeb.Routing (RouteCodec, RouteRequest)
import HarchWeb.Security (RequestConcurrencyLimit, RequestPolicyConfig)
import HarchWeb.SecurityEvent (ModuleName, SecurityEventRoot)
import HarchWeb.Server.Response
  ( ClientActionDecodeResult,
    ClientActionPayload,
    ClientActionRequest,
    ClientActionResponse,
    MiddlewareResult (..),
    RequestMiddleware (..),
    Response,
  )
import HarchWeb.Session (CsrfToken)
import HarchWeb.StaticAssets (StaticAssetsConfig)
import Network.Wai qualified as Wai

-- | The only request policy a route can safely tighten after route/method
-- selection. Listener transport controls, request-head budgets, and the
-- application-wide admission gate have already acted by this point, so they
-- deliberately do not appear here. A bounded route gate is additional to
-- the global gate and remains held for the selected response lifetime. See
-- the EN decision record in @docs/design-guidance.md@.
newtype RouteExecutionPolicy = RouteExecutionPolicy
  { routeExecutionConcurrencyLimit :: Maybe RequestConcurrencyLimit
  }
  deriving (Eq, Show)

-- | Preserve the established default: a route has no additional execution
-- admission budget unless its declaration opts in.
unboundedRouteExecutionPolicy :: RouteExecutionPolicy
unboundedRouteExecutionPolicy = RouteExecutionPolicy {routeExecutionConcurrencyLimit = Nothing}

data Application route action context authorization = Application
  { appName :: Text,
    defaultRequestContext :: context,
    requestContextFromRequest :: Wai.Request -> context -> context,
    applicationNavigationRuntime :: Maybe NavigationRuntime,
    -- | Ordered, application-selected behavior adapters served by the
    -- framework's early response boundary. The first asset owning a request
    -- path wins, so applications can replace a default by declaration rather
    -- than changing the request executor.
    applicationRuntimeAssets :: [RuntimeAsset],
    applicationStaticAssets :: StaticAssetsConfig,
    applicationRequestPolicy :: RequestPolicyConfig,
    applicationRequestMiddleware :: [RequestMiddleware context],
    routeCodec :: RouteCodec route context,
    -- | The root's explicit authentication choice.  There is no implicit
    -- disabled default: an application either selects public operation or
    -- supplies the configured authentication guard pipeline.
    applicationSecurity :: ApplicationSecurity route context authorization,
    -- | Optional root-owned security-event attachment. When present, the
    -- shared matcher builds route attribution from declared metadata before
    -- any guard runs; guards can emit only a typed event body through it.
    applicationSecurityEventRoot :: Maybe (SecurityEventRoot context),
    -- | A construction-owned chain for the selected route.  It is optional
    -- so applications which do not configure security events need not invent
    -- module identity; when present it is consumed only by the root matcher.
    applicationRouteModuleChain :: Maybe (route -> NonEmpty ModuleName),
    -- | Root-owned post-match context enrichment.  Composition roots that use
    -- 'RequestContext' install the declared route observation here; the
    -- shared executor applies it before any endpoint guard or handler.
    applicationAttachRouteObservation :: route -> EndpointMetadata authorization -> context -> context,
    -- | Metadata for the typed route selected by the one route codec.
    routeEndpointMetadata :: route -> EndpointMetadata authorization,
    -- | Selects declared client-action metadata by its already captured
    -- method/path and route-resolved context.  The request executor consults
    -- this before it reads an action body, so an action POST has an admission
    -- policy independent of the page GET at the same path.
    clientActionEndpointMetadata :: Text -> Text -> context -> Maybe (EndpointMetadata authorization),
    -- | Selects the route-local policy only after the shared dispatcher has
    -- matched a route and method. It is not a second request-policy parser.
    routeExecutionPolicy :: route -> RouteExecutionPolicy,
    renderRequestResponse :: Wai.Request -> RouteRequest route context -> IO (Response route context),
    decodeClientAction :: ClientActionPayload context -> ClientActionDecodeResult action,
    -- | Supplies the token rendered in a page response's strict host cookie.
    -- The complete page is deliberate: application policy can select the
    -- appropriate live session when distinct page routes use distinct session
    -- capabilities.
    pageCsrfToken :: Page route context -> IO CsrfToken,
    -- | Authorizes a successfully decoded action against its typed CSRF token
    -- before the handler starts. Framework transport validation has already
    -- established a strict host-cookie double-submit match; applications use
    -- this hook to bind protected actions to their live session state.
    authorizeClientActionCsrf :: ClientActionRequest action context -> CsrfToken -> IO Bool,
    handleClientAction :: ClientActionRequest action context -> IO (Maybe ClientActionResponse),
    pageShell :: Page route context -> Document route,
    reportRequestObservability :: Observability.RequestObservability -> IO (),
    reportConnectionObservability :: Observability.ConnectionObservability -> IO (),
    reportApplicationLog :: Text -> IO ()
  }

application :: Application route action context authorization -> Application route action context authorization
application = id

-- | Render a route directly with an empty request. The server uses
-- 'renderRequestResponse' so an endpoint route can own the real request's
-- decoding and body consumption without a second WAI dispatcher.
renderResponse :: Application route action context authorization -> RouteRequest route context -> IO (Response route context)
renderResponse webApplication = renderRequestResponse webApplication Wai.defaultRequest

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
