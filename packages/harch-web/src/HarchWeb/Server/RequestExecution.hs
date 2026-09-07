{-# LANGUAGE LambdaCase #-}
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
    applyRequestIdResponseHeader,
    navigationRuntimeResponse,
    runtimeAssetResponse,
    reportEarlyRequestObservability,
    runEarlyRequestStages,
    toWaiApplication,
  )
where

import Control.Monad.Except (runExceptT)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.Encoding.Error qualified as TextEncodingError
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import HarchWeb.Csrf (PageSecurity, pageSecurityRuntimeNonce)
import HarchWeb.RequestId (RequestId, RequestIdIngressResult (..), requestIdText, resolveRequestIdIngress)
import HarchWeb.Routing
  ( RouteDispatch (..),
    RouteLocation,
    RouteMethod,
    RouteRequest (..),
    decodeRouteLocation,
    matchRouteMethod,
    routeAllowHeaderValue,
  )
import HarchWeb.Routing qualified as Routing
import HarchWeb.Security (RequestPolicyConfig (..), requestPolicyResponseHeaders, validateRequestHead, waiRequestPath, waiRequestRouteTarget)
import HarchWeb.Server.Application
import HarchWeb.Server.ClientAction
import HarchWeb.Server.ClientAction.Runtime (clientActionResponse)
import HarchWeb.Server.EarlyStages (navigationRuntimeResponse, requestHeadLimitResponse, routeLocationDecodeResponse, runEarlyRequestStages, runtimeAssetResponse)
import HarchWeb.Server.PostMatch (PostMatchGuardResult (..), runPostMatchGuards)
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
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai

-- | Inputs that stay fixed after the framework has accepted a request for
-- routing. Grouping them keeps the lifecycle helpers focused on their
-- changing request state rather than plumbing the same environment through
-- every stage.
data RoutedRequestExecution route action context authorization = RoutedRequestExecution
  { routedRequestApplication :: Application route action context authorization,
    routedRequestRouteGateCache :: RouteConcurrencyGateCache,
    routedRequestWaiRequest :: Wai.Request,
    routedRequestRespond :: Wai.Response -> IO Wai.ResponseReceived,
    routedRequestPolicyConfig :: RequestPolicyConfig,
    routedRequestId :: RequestId,
    routedRequestPath :: Text,
    routedRequestLocation :: RouteLocation
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

routedRequestObservabilityContext :: RoutedRequestExecution route action context authorization -> RequestObservabilityContext route action context authorization
routedRequestObservabilityContext routedRequestExecution =
  requestObservabilityContext
    (routedRequestApplication routedRequestExecution)
    (routedRequestId routedRequestExecution)
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
toWaiApplication :: (Eq route) => Application route action context authorization -> IO Wai.Application
toWaiApplication webApplication = do
  gateMiddleware <- concurrencyLimitedMiddleware (requestConcurrencyLimit (applicationRequestPolicy webApplication)) id
  routeGateCache <- newRouteConcurrencyGateCache
  pure (requestIdentifiedWaiApplication gateMiddleware routeGateCache webApplication)

-- | Mint the request correlation identifier before request-head validation so
-- even framework rejections use the same response-header path as a routed
-- request. The generated value is then fixed for the rest of this request.
requestIdentifiedWaiApplication :: (Eq route) => Wai.Middleware -> RouteConcurrencyGateCache -> Application route action context authorization -> Wai.Application
requestIdentifiedWaiApplication gateMiddleware routeGateCache webApplication request respond = do
  (requestId, ingressResult) <- resolveRequestIdIngress (applicationRequestIdIngress webApplication) request
  reportRejectedInheritance webApplication requestId ingressResult
  let respondWithRequestId = respond . applyRequestIdResponseHeader requestId
  gateMiddleware (headLimitedWaiApplication routeGateCache webApplication requestId) request respondWithRequestId

-- | This private, value-free diagnostic lets an operator investigate a
-- misconfigured trusted caller without making arbitrary header text available
-- to logs. Public callers do not produce it because their header is ignored.
reportRejectedInheritance :: Application route action context authorization -> RequestId -> RequestIdIngressResult -> IO ()
reportRejectedInheritance webApplication requestId = \case
  RejectedInheritedRequestId ->
    reportApplicationLog webApplication ("request.id=" <> requestIdText requestId <> " harch.request_id.inheritance=invalid")
  FreshRequestId -> pure ()
  InheritedRequestId -> pure ()

headLimitedWaiApplication :: (Eq route) => RouteConcurrencyGateCache -> Application route action context authorization -> RequestId -> Wai.Application
headLimitedWaiApplication routeGateCache webApplication requestId request respond =
  case validateRequestHead (requestHeadLimits (applicationRequestPolicy webApplication)) request of
    Left limitFailure -> respond (requestHeadLimitResponse requestId limitFailure)
    Right () -> toValidatedWaiApplication routeGateCache webApplication requestId request respond

-- | Only valid, budgeted request heads reach the ordinary request pipeline.
-- This keeps malformed target bytes and oversized metadata out of route
-- parsing, application middleware, logs, and observability extraction.
toValidatedWaiApplication :: (Eq route) => RouteConcurrencyGateCache -> Application route action context authorization -> RequestId -> Wai.Application
toValidatedWaiApplication routeGateCache webApplication requestId request respond = do
  let requestPolicyConfig = applicationRequestPolicy webApplication
  case decodeRouteLocation (waiRequestRouteTarget requestPolicyConfig request) of
    Left _ -> respond (routeLocationDecodeResponse requestId)
    Right routeLocation -> do
      requestStartedAt <- getMonotonicTimeNSec
      let policyResponseHeaders = requestPolicyResponseHeaders requestPolicyConfig request
          requestPath = waiRequestPath requestPolicyConfig request
      policyEvaluatedAt <- policyResponseHeaders `seq` getMonotonicTimeNSec
      earlyResult <- runExceptT (runEarlyRequestStages webApplication request requestPath policyResponseHeaders)
      let respondEarlyRequest (earlyResponsePath, earlyResponseValue) = do
            responseReportedAt <- earlyResponseValue `seq` getMonotonicTimeNSec
            responseReceived <- respond earlyResponseValue
            reportEarlyRequestObservability
              (requestObservabilityContext webApplication requestId request requestPolicyConfig)
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
                  routedRequestId = requestId,
                  routedRequestPath = requestPath,
                  routedRequestLocation = routeLocation
                }
              requestStartedAt
              policyEvaluatedAt
      either respondEarlyRequest (const handleRoutedRequestAfterEarlyStages) earlyResult

-- | The WAI boundary owns replacement rather than addition so an application
-- response cannot shadow the framework's correlation value with a conflicting
-- or client-derived header.
applyRequestIdResponseHeader :: RequestId -> Wai.Response -> Wai.Response
applyRequestIdResponseHeader requestId =
  Wai.mapResponseHeaders $ \headers ->
    ("X-Request-ID", TextEncoding.encodeUtf8 (requestIdText requestId))
      : filter ((/= "X-Request-ID") . fst) headers

handleRoutedRequest ::
  (Eq route) =>
  RoutedRequestExecution route action context authorization ->
  Word64 ->
  Word64 ->
  IO Wai.ResponseReceived
handleRoutedRequest routedRequestExecution requestStartedAt policyEvaluatedAt = do
  let webApplication = routedRequestApplication routedRequestExecution
      request = routedRequestWaiRequest routedRequestExecution
  middlewareStartedAt <- getMonotonicTimeNSec
  middlewareResult <- runRequestMiddlewarePipeline (applicationRequestMiddleware webApplication) request (requestContextFromRequest webApplication request (routedRequestId routedRequestExecution) (defaultRequestContext webApplication))
  middlewareCompletedAt <- middlewareResult `seq` getMonotonicTimeNSec
  let requestContext = middlewareResultContext middlewareResult
      middlewareTiming = middlewareTimingEntry webApplication middlewareStartedAt middlewareCompletedAt
  routeMatchingStartedAt <- getMonotonicTimeNSec
  let decodedRequestMethod = requestMethodText request
      routeDispatchResult =
        matchRouteMethod
          (routeCodec webApplication)
          requestContext
          (Routing.requestMethod decodedRequestMethod)
          (routedRequestLocation routedRequestExecution)
  routeMatchedAt <- routeDispatchResult `seq` getMonotonicTimeNSec
  let timingState =
        RequestExecutionTimingState
          { requestTimingStartedAt = requestStartedAt,
            requestTimingPolicyEvaluatedAt = policyEvaluatedAt,
            requestTimingMiddleware = middlewareTiming,
            requestTimingRouteMatchingStartedAt = routeMatchingStartedAt,
            requestTimingRouteMatchedAt = routeMatchedAt
          }
  case routeDispatchResult of
    Left _ -> respondRouteLocationDecodeFailure routedRequestExecution
    Right routeDispatch -> do
      guardResult <- runPostMatchGuards webApplication request (routedRequestPath routedRequestExecution) routeDispatch middlewareResult
      case guardResult of
        HaltPostMatch guardedResponse ->
          continueRoutedResponse routedRequestExecution timingState routeDispatch (nonPageResponse guardedResponse)
        ContinuePostMatch guardedContext selectedAdmissionRoute -> do
          let guardedDispatch = setRouteDispatchContext guardedContext routeDispatch
          routeMiddleware <- routeAdmissionMiddleware routedRequestExecution selectedAdmissionRoute
          routeMiddleware
            ( \admittedRequest admittedRespond ->
                continueRoutedRequest
                  (routedRequestExecution {routedRequestWaiRequest = admittedRequest, routedRequestRespond = admittedRespond})
                  timingState
                  decodedRequestMethod
                  guardedDispatch
            )
            request
            (routedRequestRespond routedRequestExecution)

respondRouteLocationDecodeFailure :: RoutedRequestExecution route action context authorization -> IO Wai.ResponseReceived
respondRouteLocationDecodeFailure routedRequestExecution =
  routedRequestRespond routedRequestExecution (routeLocationDecodeResponse (routedRequestId routedRequestExecution))

-- | Admit the declaration selected exactly once by post-match execution. An
-- ordinary unmatched route and an unknown client action have no declaration;
-- a declared action uses its action owner even if its URL is not in the page
-- codec. This preserves the route authority for 405, HEAD and OPTIONS while
-- preventing page/action URL collisions from selecting another gate.
routeAdmissionMiddleware :: RoutedRequestExecution route action context authorization -> Maybe route -> IO Wai.Middleware
routeAdmissionMiddleware routedRequestExecution = \case
  Nothing -> pure id
  Just selectedRoute ->
    routeConcurrencyMiddleware
      (routedRequestRouteGateCache routedRequestExecution)
      (routeExecutionIdentity (routedRequestApplication routedRequestExecution) selectedRoute)
      (routeExecutionConcurrencyLimit (routeExecutionPolicy (routedRequestApplication routedRequestExecution) selectedRoute))

continueRoutedRequest ::
  (Eq route) =>
  RoutedRequestExecution route action context authorization ->
  RequestExecutionTimingState ->
  Text ->
  RouteDispatch route context ->
  IO Wai.ResponseReceived
continueRoutedRequest routedRequestExecution timingState decodedRequestMethod routeDispatch = do
  renderStartedAt <- getMonotonicTimeNSec
  response <- dispatchRoutedRequest routedRequestExecution decodedRequestMethod routeDispatch
  continueRoutedResponseAt routedRequestExecution timingState routeDispatch renderStartedAt response

continueRoutedResponse ::
  (Eq route) =>
  RoutedRequestExecution route action context authorization ->
  RequestExecutionTimingState ->
  RouteDispatch route context ->
  Response route context ->
  IO Wai.ResponseReceived
continueRoutedResponse routedRequestExecution timingState routeDispatch response = do
  renderStartedAt <- getMonotonicTimeNSec
  continueRoutedResponseAt routedRequestExecution timingState routeDispatch renderStartedAt response

continueRoutedResponseAt ::
  (Eq route) =>
  RoutedRequestExecution route action context authorization ->
  RequestExecutionTimingState ->
  RouteDispatch route context ->
  Word64 ->
  Response route context ->
  IO Wai.ResponseReceived
continueRoutedResponseAt routedRequestExecution timingState routeDispatch renderStartedAt response = do
  responseRenderedAt <- response `seq` getMonotonicTimeNSec
  let pageSecurity = responsePageSecurity response
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

setRouteDispatchContext :: context -> RouteDispatch route context -> RouteDispatch route context
setRouteDispatchContext requestContext routeDispatch =
  case routeDispatch of
    RouteNotFound routeRequest -> RouteNotFound (setRouteRequestContext requestContext routeRequest)
    RouteMethodNotAllowed routeRequest methods -> RouteMethodNotAllowed (setRouteRequestContext requestContext routeRequest) methods
    RouteMatched routeRequest -> RouteMatched (setRouteRequestContext requestContext routeRequest)
    RouteMatchedHead routeRequest -> RouteMatchedHead (setRouteRequestContext requestContext routeRequest)
    RouteOptions routeRequest methods -> RouteOptions (setRouteRequestContext requestContext routeRequest) methods

setRouteRequestContext :: context -> RouteRequest route context -> RouteRequest route context
setRouteRequestContext requestContext routeRequest = routeRequest {requestContext = requestContext}

isHeadDispatch :: RouteDispatch route context -> Bool
isHeadDispatch routeDispatch =
  case routeDispatch of
    RouteMatchedHead _ -> True
    _ -> False

middlewareTimingEntry :: Application route action context authorization -> Word64 -> Word64 -> [(Text, Word64, Word64)]
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
  RoutedRequestExecution route action context authorization ->
  Text ->
  RouteDispatch route context ->
  IO (Response route context)
dispatchRoutedRequest
  routedRequestExecution
  decodedRequestMethod
  routeDispatch =
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

renderRouteDispatch :: Application route action context authorization -> Wai.Request -> RouteRenderDispatch route context -> IO (Response route context)
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
  RoutedRequestExecution route action context authorization ->
  RequestExecutionTimings ->
  RouteDispatch route context ->
  Maybe PageSecurity ->
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
          (applyResponseHeaders (responsePolicyHeaders requestPolicyConfig request (pageSecurityRuntimeNonce <$> pageSecurity)) (toWaiResponse [] pageSecurity webApplication response))
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
