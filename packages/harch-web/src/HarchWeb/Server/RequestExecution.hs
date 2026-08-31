{-# LANGUAGE OverloadedStrings #-}

-- | Typed request execution and the public WAI adapter.
--
-- FQ8 keeps WAI request, response, and route-dispatch values explicit at
-- their changing execution stages.  'RoutedRequestExecution' owns the
-- stable accepted-request dependencies, while 'RequestExecutionTimingState'
-- captures only timings that have already happened.  That preserves the
-- deliberate @seq@ timing boundaries without allowing independently passed
-- timestamps or reporting dependencies to be transposed.
-- FQ11 keeps the client-action protocol interpreter in its own internal
-- module: decoding, bounded body intake, CSRF/origin checks, authorization,
-- and handler invocation form one protocol lifecycle, while route selection,
-- timing, and final response reporting stay here.
module HarchWeb.Server.RequestExecution
  ( concurrencyLimitedMiddleware,
    navigationRuntimeResponse,
    runtimeAssetResponse,
    reportEarlyRequestObservability,
    runEarlyRequestStages,
    toWaiApplication,
  )
where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as ByteString
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.Encoding.Error qualified as TextEncodingError
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import HarchWeb.Document (NavigationRuntime, RuntimeAsset)
import HarchWeb.Document qualified as Document
import HarchWeb.Routing
  ( RouteDispatch (..),
    RouteMethod,
    RouteRequest (..),
    matchRouteMethod,
    routeAllowHeaderValue,
  )
import HarchWeb.Routing qualified as Routing
import HarchWeb.Security
  ( RequestHeadLimitFailure (..),
    RequestPolicyConfig (..),
    applyRequestPathPrefix,
    corsPreflightResponse,
    externalRequestPath,
    httpsRedirectResponse,
    mkUrlPath,
    requestPathPrefix,
    requestPolicyResponseHeaders,
    requestRedirectLocation,
    urlPathText,
    validateRequestHead,
    waiRequestPath,
    waiRequestRouteTarget,
  )
import HarchWeb.Server.Application
import HarchWeb.Server.ClientAction
import HarchWeb.Server.ClientAction.Runtime (clientActionResponse)
import HarchWeb.Server.RequestAdmission
  ( RouteConcurrencyGateCache,
    concurrencyLimitedMiddleware,
    newRouteConcurrencyGateCache,
    routeConcurrencyMiddleware,
  )
import HarchWeb.Server.RequestObservability
  ( RequestExecutionTimings (..),
    RequestObservabilityContext,
    reportEarlyRequestObservability,
    reportRoutedResponseObservability,
    requestObservabilityContext,
  )
import HarchWeb.Server.Response
import HarchWeb.Server.ResponseRendering
import HarchWeb.Server.StaticAssets (serveStaticAssetResponse)
import HarchWeb.Session (CsrfToken)
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai

navigationRuntimeResponse :: NavigationRuntime -> Text -> Maybe ResponseBody
navigationRuntimeResponse runtime requestPath =
  if requestPath == Document.navigationRuntimePath runtime
    then
      Just
        ResponseBody
          { responseStatus = Http.status200,
            responseContentType = "application/javascript; charset=utf-8",
            responseBody = Document.navigationRuntimeScript runtime,
            responseObservabilityAttributes = [],
            responseLogEntries = [],
            responseDatabaseOperations = []
          }
    else Nothing

-- | Interpret one application-selected runtime asset at the existing early
-- response boundary. The caller preserves declaration order and serves only
-- the first path match; this is adapter selection, not a second static-file
-- or dialog-specific request pipeline.
runtimeAssetResponse :: RuntimeAsset -> Text -> Maybe ResponseBody
runtimeAssetResponse runtimeAsset requestPath =
  if requestPath == Document.runtimeAssetPath runtimeAsset
    then
      Just
        ResponseBody
          { responseStatus = Http.status200,
            responseContentType = "application/javascript; charset=utf-8",
            responseBody = Document.runtimeAssetScript runtimeAsset,
            responseObservabilityAttributes = [],
            responseLogEntries = [],
            responseDatabaseOperations = []
          }
    else Nothing

-- | Handle framework-owned responses before routing or application middleware.
-- Each response receives the request policy headers exactly once.
runEarlyRequestStages ::
  Application route action context ->
  Wai.Request ->
  Text ->
  Http.ResponseHeaders ->
  ExceptT (Text, Wai.Response) IO ()
runEarlyRequestStages webApplication request requestPath policyResponseHeaders = do
  let requestPolicyConfig = applicationRequestPolicy webApplication
      earlyResponse path = throwError . (path,) . applyResponseHeaders policyResponseHeaders
  for_ (corsPreflightResponse requestPolicyConfig request) $
    earlyResponse (externalRequestPath requestPolicyConfig request)
  for_ (requestRedirectLocation requestPolicyConfig request) $ \redirectLocation ->
    earlyResponse (externalRequestPath requestPolicyConfig request) (httpsRedirectResponse redirectLocation)
  for_ (applicationNavigationRuntime webApplication >>= (`navigationRuntimeResponse` requestPath)) $
    earlyResponse requestPath . toWaiBodyResponse []
  for_
    ( listToMaybe
        (mapMaybe (`runtimeAssetResponse` requestPath) (applicationRuntimeAssets webApplication))
    )
    $ earlyResponse requestPath . toWaiBodyResponse []
  maybeStaticResponse <- liftIO (serveStaticAssetResponse (applicationStaticAssets webApplication) request requestPath)
  for_ maybeStaticResponse $ \(staticRoutePath, staticResponse) ->
    earlyResponse
      ( urlPathText
          (applyRequestPathPrefix (requestPathPrefix requestPolicyConfig request) (mkUrlPath staticRoutePath))
      )
      staticResponse

-- | Inputs that stay fixed after the framework has accepted a request for
-- routing. Grouping them keeps the lifecycle helpers focused on their
-- changing request state rather than plumbing the same environment through
-- every stage.
data RoutedRequestExecution route action context = RoutedRequestExecution
  { routedRequestApplication :: Application route action context,
    routedRequestRouteGateCache :: RouteConcurrencyGateCache route,
    routedRequestWaiRequest :: Wai.Request,
    routedRequestRespond :: Wai.Response -> IO Wai.ResponseReceived,
    routedRequestPolicyConfig :: RequestPolicyConfig,
    routedRequestPath :: Text
  }

-- | Timing values stable after policy, middleware, and route matching have
-- completed.  Rendering times are deliberately absent until the response is
-- forced at the final rendering boundary.
data RequestExecutionTimingState = RequestExecutionTimingState
  { requestTimingStartedAt :: Word64,
    requestTimingPolicyEvaluatedAt :: Word64,
    requestTimingMiddleware :: [(Text, Word64, Word64)],
    requestTimingRouteMatchingStartedAt :: Word64,
    requestTimingRouteMatchedAt :: Word64
  }

routedRequestObservabilityContext :: RoutedRequestExecution route action context -> RequestObservabilityContext route action context
routedRequestObservabilityContext routedRequestExecution =
  requestObservabilityContext
    (routedRequestApplication routedRequestExecution)
    (routedRequestWaiRequest routedRequestExecution)
    (routedRequestPolicyConfig routedRequestExecution)

-- | Adapt a typed application to WAI. Framework-owned early responses,
-- middleware, route dispatch, and finalization all converge here. The
-- returned application unconditionally honors 'requestConcurrencyLimit'
-- from the application's own 'RequestPolicyConfig' (a 'Nothing' limit is
-- the framework's established unbounded default): every caller reaches
-- this same admission gate by construction, whether it composes this
-- adapter through 'HarchWeb.Server.Runtime'/'HarchWeb.Server.LocalTest' or
-- builds its own 'Wai.Application' from it directly. Called once per
-- running server, since the gate's in-flight counter is allocated here and
-- must be shared across every request that server handles, not
-- reallocated per request.
toWaiApplication :: (Eq route) => Application route action context -> IO Wai.Application
toWaiApplication webApplication = do
  gateMiddleware <- concurrencyLimitedMiddleware (requestConcurrencyLimit (applicationRequestPolicy webApplication)) id
  routeGateCache <- newRouteConcurrencyGateCache
  pure (gateMiddleware (headLimitedWaiApplication routeGateCache webApplication))

headLimitedWaiApplication :: (Eq route) => RouteConcurrencyGateCache route -> Application route action context -> Wai.Application
headLimitedWaiApplication routeGateCache webApplication request respond =
  case validateRequestHead (requestHeadLimits (applicationRequestPolicy webApplication)) request of
    Left limitFailure -> respond (requestHeadLimitResponse limitFailure)
    Right () -> toValidatedWaiApplication routeGateCache webApplication request respond

-- | Only valid, budgeted request heads reach the ordinary request pipeline.
-- This keeps malformed target bytes and oversized metadata out of route
-- parsing, application middleware, logs, and observability extraction.
toValidatedWaiApplication :: (Eq route) => RouteConcurrencyGateCache route -> Application route action context -> Wai.Application
toValidatedWaiApplication routeGateCache webApplication request respond = do
  requestStartedAt <- getMonotonicTimeNSec
  let requestPolicyConfig = applicationRequestPolicy webApplication
      policyResponseHeaders = requestPolicyResponseHeaders requestPolicyConfig request
      requestPath = waiRequestPath requestPolicyConfig request
  policyEvaluatedAt <- policyResponseHeaders `seq` getMonotonicTimeNSec
  earlyResult <- runExceptT (runEarlyRequestStages webApplication request requestPath policyResponseHeaders)
  let respondEarlyRequest (earlyResponsePath, earlyResponseValue) = do
        responseReportedAt <- earlyResponseValue `seq` getMonotonicTimeNSec
        responseReceived <- respond earlyResponseValue
        reportEarlyRequestObservability
          (requestObservabilityContext webApplication request requestPolicyConfig)
          requestStartedAt
          responseReportedAt
          earlyResponsePath
          earlyResponseValue
        pure responseReceived

      handleRoutedRequestAfterEarlyStages =
        handleRoutedRequest
          RoutedRequestExecution
            { routedRequestApplication = webApplication,
              routedRequestRouteGateCache = routeGateCache,
              routedRequestWaiRequest = request,
              routedRequestRespond = respond,
              routedRequestPolicyConfig = requestPolicyConfig,
              routedRequestPath = requestPath
            }
          requestStartedAt
          policyEvaluatedAt
  either respondEarlyRequest (const handleRoutedRequestAfterEarlyStages) earlyResult

requestHeadLimitResponse :: RequestHeadLimitFailure -> Wai.Response
requestHeadLimitResponse limitFailure =
  Wai.responseLBS
    status
    [(Http.hContentType, "text/plain; charset=utf-8")]
    "Request metadata was rejected."
  where
    status =
      case limitFailure of
        InvalidRequestTargetEncoding -> Http.status400
        RequestTargetTooLarge -> Http.status414
        TooManyRequestHeaders -> Http.status431
        RequestHeadersTooLarge -> Http.status431
        RequestHeaderValueTooLarge -> Http.status431
        TooManyRequestCookies -> Http.status431
        RequestCookieNameTooLarge -> Http.status431
        RequestCookieValueTooLarge -> Http.status431
        TooManyPathSegments -> Http.status414
        RequestPathSegmentTooLarge -> Http.status414
        TooManyQueryFields -> Http.status414
        RequestQueryFieldTooLarge -> Http.status414

handleRoutedRequest ::
  (Eq route) =>
  RoutedRequestExecution route action context ->
  Word64 ->
  Word64 ->
  IO Wai.ResponseReceived
handleRoutedRequest routedRequestExecution requestStartedAt policyEvaluatedAt = do
  let webApplication = routedRequestApplication routedRequestExecution
      request = routedRequestWaiRequest routedRequestExecution
      requestPolicyConfig = routedRequestPolicyConfig routedRequestExecution
  middlewareStartedAt <- getMonotonicTimeNSec
  middlewareResult <- runRequestMiddlewarePipeline (applicationRequestMiddleware webApplication) request (requestContextFromRequest webApplication request (defaultRequestContext webApplication))
  middlewareCompletedAt <- middlewareResult `seq` getMonotonicTimeNSec
  let requestContext = middlewareResultContext middlewareResult
      middlewareTiming = middlewareTimingEntry webApplication middlewareStartedAt middlewareCompletedAt
  routeMatchingStartedAt <- getMonotonicTimeNSec
  let decodedRequestMethod = requestMethodText request
      routeDispatch =
        matchRouteMethod
          (routeCodec webApplication)
          requestContext
          (Routing.requestMethod decodedRequestMethod)
          (Routing.requestPath (waiRequestRouteTarget requestPolicyConfig request))
  routeMatchedAt <- routeDispatch `seq` getMonotonicTimeNSec
  let timingState =
        RequestExecutionTimingState
          { requestTimingStartedAt = requestStartedAt,
            requestTimingPolicyEvaluatedAt = policyEvaluatedAt,
            requestTimingMiddleware = middlewareTiming,
            requestTimingRouteMatchingStartedAt = routeMatchingStartedAt,
            requestTimingRouteMatchedAt = routeMatchedAt
          }
  routeMiddleware <- routeAdmissionMiddleware routedRequestExecution routeDispatch
  routeMiddleware
    ( \admittedRequest admittedRespond ->
        continueRoutedRequest
          (routedRequestExecution {routedRequestWaiRequest = admittedRequest, routedRequestRespond = admittedRespond})
          timingState
          decodedRequestMethod
          routeDispatch
          middlewareResult
    )
    request
    (routedRequestRespond routedRequestExecution)

-- | A route-local gate is available only for an already path-matched route.
-- `RouteNotFound` deliberately has no route declaration whose policy it could
-- claim; all other dispatch outcomes retain the selected route so their
-- `HEAD`, `OPTIONS`, and 405 responses cannot bypass that route's gate.
routeAdmissionMiddleware :: (Eq route) => RoutedRequestExecution route action context -> RouteDispatch route context -> IO Wai.Middleware
routeAdmissionMiddleware routedRequestExecution routeDispatch =
  case routeDispatch of
    RouteNotFound _ -> pure id
    _ ->
      routeConcurrencyMiddleware
        (routedRequestRouteGateCache routedRequestExecution)
        selectedRoute
        (routeExecutionConcurrencyLimit (routeExecutionPolicy (routedRequestApplication routedRequestExecution) selectedRoute))
  where
    selectedRoute = requestRoute (routeDispatchRequest routeDispatch)

continueRoutedRequest ::
  (Eq route) =>
  RoutedRequestExecution route action context ->
  RequestExecutionTimingState ->
  Text ->
  RouteDispatch route context ->
  MiddlewareResult context ->
  IO Wai.ResponseReceived
continueRoutedRequest routedRequestExecution timingState decodedRequestMethod routeDispatch middlewareResult = do
  let webApplication = routedRequestApplication routedRequestExecution
  renderStartedAt <- getMonotonicTimeNSec
  response <- dispatchRoutedRequest routedRequestExecution decodedRequestMethod routeDispatch middlewareResult
  responseRenderedAt <- response `seq` getMonotonicTimeNSec
  pageSecurity <- responsePageSecurity webApplication response
  let executionTimings =
        RequestExecutionTimings
          { requestExecutionStartedAt = requestTimingStartedAt timingState,
            requestPolicyEvaluatedAt = requestTimingPolicyEvaluatedAt timingState,
            requestMiddlewareTimings = requestTimingMiddleware timingState,
            requestRouteMatchingStartedAt = requestTimingRouteMatchingStartedAt timingState,
            requestRouteMatchedAt = requestTimingRouteMatchedAt timingState,
            requestRenderingStartedAt = renderStartedAt,
            requestResponseRenderedAt = responseRenderedAt
          }
  finalizeRoutedResponse routedRequestExecution executionTimings routeDispatch pageSecurity response

routeDispatchRequest :: RouteDispatch route context -> RouteRequest route context
routeDispatchRequest routeDispatch =
  case routeDispatch of
    RouteNotFound routeRequest -> routeRequest
    RouteMethodNotAllowed routeRequest _ -> routeRequest
    RouteMatched routeRequest -> routeRequest
    RouteMatchedHead routeRequest -> routeRequest
    RouteOptions routeRequest _ -> routeRequest

isHeadDispatch :: RouteDispatch route context -> Bool
isHeadDispatch routeDispatch =
  case routeDispatch of
    RouteMatchedHead _ -> True
    _ -> False

middlewareTimingEntry :: Application route action context -> Word64 -> Word64 -> [(Text, Word64, Word64)]
middlewareTimingEntry webApplication startedAt completedAt =
  case applicationRequestMiddleware webApplication of
    [] -> []
    _ -> [("middleware", startedAt, completedAt)]

-- | Decision record (DR): route-method dispatch remains the authority for a
-- route's synthesized @HEAD@ and @OPTIONS@ responses. Client-action endpoints
-- are a distinct declared protocol table, so ordinary action methods may not
-- appear in the page route table; however, a client-action header can never
-- turn 'RouteMatchedHead' or 'RouteOptions' into a state-changing action.
dispatchRoutedRequest ::
  RoutedRequestExecution route action context ->
  Text ->
  RouteDispatch route context ->
  MiddlewareResult context ->
  IO (Response route context)
dispatchRoutedRequest _ _ _ (HaltMiddleware _ responseBody) = pure (BodyResponse responseBody)
dispatchRoutedRequest
  routedRequestExecution
  decodedRequestMethod
  routeDispatch
  (ContinueMiddleware _) =
    let webApplication = routedRequestApplication routedRequestExecution
        request = routedRequestWaiRequest routedRequestExecution
        RouteRequest {requestContext = routedRequestContext} = routeDispatchRequest routeDispatch
     in case routeRenderDispatch routeDispatch of
          Left declaredMethods -> routeOptionsResponse declaredMethods
          Right renderDispatch@RenderMatchedHead {} -> renderRouteDispatch webApplication request renderDispatch
          Right renderDispatch
            | isClientActionRequest request ->
                clientActionResponse webApplication request decodedRequestMethod (routedRequestPath routedRequestExecution) routedRequestContext
            | otherwise -> renderRouteDispatch webApplication request renderDispatch

data RouteRenderDispatch route context
  = RenderNotFound (RouteRequest route context)
  | RenderMethodNotAllowed (NonEmpty RouteMethod)
  | RenderMatched (RouteRequest route context)
  | RenderMatchedHead (RouteRequest route context)

routeRenderDispatch :: RouteDispatch route context -> Either (NonEmpty RouteMethod) (RouteRenderDispatch route context)
routeRenderDispatch routeDispatch =
  case routeDispatch of
    RouteNotFound routeRequest -> Right (RenderNotFound routeRequest)
    RouteMethodNotAllowed _ declaredMethods -> Right (RenderMethodNotAllowed declaredMethods)
    RouteMatched routeRequest -> Right (RenderMatched routeRequest)
    RouteMatchedHead routeRequest -> Right (RenderMatchedHead routeRequest)
    RouteOptions _ declaredMethods -> Left declaredMethods

renderRouteDispatch :: Application route action context -> Wai.Request -> RouteRenderDispatch route context -> IO (Response route context)
renderRouteDispatch webApplication request renderDispatch =
  case renderDispatch of
    RenderNotFound routeRequest -> renderRequestResponse webApplication request routeRequest
    RenderMethodNotAllowed declaredMethods ->
      pure
        ( ProtocolResponseResult
            ProtocolResponse
              { protocolResponseStatus = Http.status405,
                protocolResponseHeaders = [(Http.hAllow, TextEncoding.encodeUtf8 (routeAllowHeaderValue declaredMethods))],
                protocolResponseBody = ProtocolResponseBytes ByteString.empty,
                protocolResponseObservabilityAttributes = [],
                protocolResponseLogEntries = [],
                protocolResponseDatabaseOperations = []
              }
        )
    RenderMatched routeRequest -> renderRequestResponse webApplication request routeRequest
    RenderMatchedHead routeRequest -> renderRequestResponse webApplication request routeRequest

routeOptionsResponse :: NonEmpty RouteMethod -> IO (Response route context)
routeOptionsResponse declaredMethods =
  pure
    ( ProtocolResponseResult
        ProtocolResponse
          { protocolResponseStatus = Http.status204,
            protocolResponseHeaders = [(Http.hAllow, TextEncoding.encodeUtf8 (routeAllowHeaderValue declaredMethods))],
            protocolResponseBody = ProtocolResponseBytes ByteString.empty,
            protocolResponseObservabilityAttributes = [],
            protocolResponseLogEntries = [],
            protocolResponseDatabaseOperations = []
          }
    )

-- | Decision record (AU): 'respond' now runs before 'reportRequestObservability'
-- and 'reportApplicationLog', not after. Previously an app-supplied reporter
-- (web-api's OTLP exporter, in particular) sat on the response path — a slow
-- or hung collector added its latency to every user response before a byte
-- was sent. Both this function and 'respondEarlyRequest' in
-- 'toValidatedWaiApplication' now hand the WAI response to Warp first and
-- report observability afterward, so no caller-supplied reporter can ever
-- delay a response again, regardless of how slow it is. Timing fields
-- ('responseReportedAt' and friends) are still captured immediately after
-- the response value is forced, before 'respond' runs, so recorded
-- durations reflect render time, not reporter time. This is a small,
-- general framework fix (every 'Application', not just web-api, benefits);
-- see 'docs/design-guidance.md' for the full framework-capability-gap
-- protocol this follows. It does not by itself make an app's reporter
-- non-blocking — a reporter that itself blocks (e.g. on a synchronous
-- network call) still occupies this request's handling thread after
-- 'respond' returns; decoupling that is the caller's responsibility (see
-- web-api's bounded-queue 'WebApi.App' exporter, added alongside this fix).
finalizeRoutedResponse ::
  (Eq route) =>
  RoutedRequestExecution route action context ->
  RequestExecutionTimings ->
  RouteDispatch route context ->
  Maybe (Document.RuntimeNonce, CsrfToken) ->
  Response route context ->
  IO Wai.ResponseReceived
finalizeRoutedResponse routedRequestExecution executionTimings routeDispatch pageSecurity response = do
  let webApplication = routedRequestApplication routedRequestExecution
      request = routedRequestWaiRequest routedRequestExecution
      respond = routedRequestRespond routedRequestExecution
      requestPolicyConfig = routedRequestPolicyConfig routedRequestExecution
      routeRequest = routeDispatchRequest routeDispatch
  responseReceived <-
    respond
      ( omitResponseBodyWhen
          (isHeadDispatch routeDispatch)
          (applyResponseHeaders (responsePolicyHeaders requestPolicyConfig request (fst <$> pageSecurity)) (toWaiResponse [] pageSecurity webApplication response))
      )
  reportRoutedResponseObservability
    (routedRequestObservabilityContext routedRequestExecution)
    (routedRequestPath routedRequestExecution)
    executionTimings
    routeRequest
    response
  pure responseReceived

omitResponseBodyWhen :: Bool -> Wai.Response -> Wai.Response
omitResponseBodyWhen omitResponseBody waiResponse =
  if omitResponseBody
    then Wai.responseStream (Wai.responseStatus waiResponse) (Wai.responseHeaders waiResponse) (\_write flush -> flush)
    else waiResponse

requestMethodText :: Wai.Request -> Text
requestMethodText = TextEncoding.decodeUtf8With TextEncodingError.lenientDecode . Wai.requestMethod
