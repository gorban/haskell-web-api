{-# LANGUAGE OverloadedStrings #-}

-- | Typed request execution and the public WAI adapter.
module HarchWeb.Server.RequestExecution
  ( concurrencyLimitedMiddleware,
    navigationRuntimeResponse,
    reportEarlyRequestObservability,
    runEarlyRequestStages,
    toWaiApplication,
  )
where

import Control.Exception (finally)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Foldable (for_)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.Encoding.Error qualified as TextEncodingError
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import HarchWeb.Document (NavigationRuntime)
import HarchWeb.Document qualified as Document
import HarchWeb.Observability qualified as Observability
import HarchWeb.Routing
  ( RouteCodec (..),
    RouteDispatch (..),
    RouteRequest (..),
    matchRouteMethod,
    renderRoute,
    routeAllowHeaderValue,
  )
import HarchWeb.Security
  ( RequestConcurrencyLimit,
    RequestHeadLimitFailure (..),
    RequestPolicyConfig (..),
    applyRequestPathPrefix,
    corsPreflightResponse,
    externalRequestPath,
    httpsRedirectResponse,
    prependRequestLogContext,
    requestConcurrencyLimitValue,
    requestContextObservabilityAttributes,
    requestLogContextFields,
    requestPathPrefix,
    requestPolicyResponseHeaders,
    requestRedirectLocation,
    requestScheme,
    requestTraceContext,
    validateRequestHead,
    waiRequestPath,
    waiRequestRouteTarget,
  )
import HarchWeb.Server.Application
import HarchWeb.Server.ClientAction
import HarchWeb.Server.RequestBody (RequestBodyReadFailure (..), readRequestBodyUpTo)
import HarchWeb.Server.Response
import HarchWeb.Server.ResponseRendering
import HarchWeb.Server.StaticAssets (serveStaticAssetResponse)
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai

navigationRuntimeResponse :: NavigationRuntime -> Text -> Maybe ResponseBody
navigationRuntimeResponse runtime requestPath =
  if requestPath == Document.navigationRuntimePath runtime
    then
      Just
        ResponseBody
          { responseStatus = 200,
            responseContentType = "application/javascript; charset=utf-8",
            responseBody = Document.navigationRuntimeScript runtime,
            responseObservabilityAttributes = [],
            responseLogEntries = []
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
  maybeStaticResponse <- liftIO (serveStaticAssetResponse (applicationStaticAssets webApplication) requestPath)
  for_ maybeStaticResponse $ \(staticRoutePath, staticResponse) ->
    earlyResponse
      (applyRequestPathPrefix (requestPathPrefix requestPolicyConfig request) staticRoutePath)
      staticResponse

data RequestExecutionTimings = RequestExecutionTimings
  { requestExecutionStartedAt :: Word64,
    requestPolicyEvaluatedAt :: Word64,
    requestMiddlewareTimings :: [(Text, Word64, Word64)],
    requestRouteMatchingStartedAt :: Word64,
    requestRouteMatchedAt :: Word64,
    requestRenderingStartedAt :: Word64,
    requestResponseRenderedAt :: Word64
  }

-- | Inputs that stay fixed after the framework has accepted a request for
-- routing. Grouping them keeps the lifecycle helpers focused on their
-- changing request state rather than plumbing the same environment through
-- every stage.
data RoutedRequestExecution route action context = RoutedRequestExecution
  { routedRequestApplication :: Application route action context,
    routedRequestWaiRequest :: Wai.Request,
    routedRequestRespond :: Wai.Response -> IO Wai.ResponseReceived,
    routedRequestPolicyConfig :: RequestPolicyConfig,
    routedRequestPath :: Text
  }

-- | Adapt a typed application to WAI. Framework-owned early responses,
-- middleware, route dispatch, and finalization all converge here.
toWaiApplication :: (Eq route) => Application route action context -> Wai.Application
toWaiApplication webApplication request respond =
  case validateRequestHead (requestHeadLimits (applicationRequestPolicy webApplication)) request of
    Left limitFailure -> respond (requestHeadLimitResponse limitFailure)
    Right () -> toValidatedWaiApplication webApplication request respond

-- | Compose an opt-in concurrent-in-flight-request gate in front of a
-- caller-supplied middleware. 'Nothing' preserves the framework's
-- established unbounded behaviour: the runtime forks a worker per accepted
-- connection with no admission control of its own, matching Warp 3.4.12's
-- own lack of a concurrent-request or connection-count setting. Every
-- caller that renders a real listener — 'HarchWeb.Server.Runtime' and
-- 'HarchWeb.Server.LocalTest' alike — builds this gate from the same
-- 'RequestPolicyConfig' field, so a real-socket test against a local
-- listener observes the same admission behaviour a deployed runtime would.
concurrencyLimitedMiddleware :: Maybe RequestConcurrencyLimit -> Wai.Middleware -> IO Wai.Middleware
concurrencyLimitedMiddleware maybeLimit waiMiddleware =
  case maybeLimit of
    Nothing -> pure waiMiddleware
    Just limit -> do
      gate <- newRequestConcurrencyGate limit
      pure (concurrencyGateMiddleware gate . waiMiddleware)

data RequestConcurrencyGate = RequestConcurrencyGate
  { concurrencyGateLimit :: Int,
    concurrencyGateInFlight :: IORef Int
  }

newRequestConcurrencyGate :: RequestConcurrencyLimit -> IO RequestConcurrencyGate
newRequestConcurrencyGate limit =
  RequestConcurrencyGate (requestConcurrencyLimitValue limit) <$> newIORef 0

-- | Admit at most the configured number of requests at once, across every
-- listener sharing this gate. Admission is a non-blocking, immediate
-- accept-or-reject rather than a queue: an exceeded gate returns a stable
-- '503' before route parsing, middleware, observability, or body reads,
-- rather than making a caller wait for a slot. A slot is held for the
-- request's whole lifetime, including a streamed response, and always
-- released — on ordinary completion or any exception.
concurrencyGateMiddleware :: RequestConcurrencyGate -> Wai.Middleware
concurrencyGateMiddleware gate app request respond = do
  admitted <- acquireConcurrencySlot gate
  if admitted
    then app request respond `finally` releaseConcurrencySlot gate
    else respond concurrencyLimitResponse

acquireConcurrencySlot :: RequestConcurrencyGate -> IO Bool
acquireConcurrencySlot gate =
  atomicModifyIORef' (concurrencyGateInFlight gate) $ \inFlight ->
    if inFlight < concurrencyGateLimit gate
      then (inFlight + 1, True)
      else (inFlight, False)

releaseConcurrencySlot :: RequestConcurrencyGate -> IO ()
releaseConcurrencySlot gate =
  atomicModifyIORef' (concurrencyGateInFlight gate) (\inFlight -> (inFlight - 1, ()))

concurrencyLimitResponse :: Wai.Response
concurrencyLimitResponse =
  Wai.responseLBS
    Http.status503
    [(Http.hContentType, "text/plain; charset=utf-8")]
    "Too many concurrent requests."

-- | Only valid, budgeted request heads reach the ordinary request pipeline.
-- This keeps malformed target bytes and oversized metadata out of route
-- parsing, application middleware, logs, and observability extraction.
toValidatedWaiApplication :: (Eq route) => Application route action context -> Wai.Application
toValidatedWaiApplication webApplication request respond = do
  requestStartedAt <- getMonotonicTimeNSec
  let requestPolicyConfig = applicationRequestPolicy webApplication
      policyResponseHeaders = requestPolicyResponseHeaders requestPolicyConfig request
      requestPath = waiRequestPath requestPolicyConfig request
  policyEvaluatedAt <- policyResponseHeaders `seq` getMonotonicTimeNSec
  earlyResult <- runExceptT (runEarlyRequestStages webApplication request requestPath policyResponseHeaders)
  let respondEarlyRequest (earlyResponsePath, earlyResponseValue) = do
        responseReportedAt <- earlyResponseValue `seq` getMonotonicTimeNSec
        responseReceived <- respond earlyResponseValue
        reportEarlyRequestObservability webApplication request requestStartedAt responseReportedAt earlyResponsePath earlyResponseValue
        pure responseReceived

      handleRoutedRequestAfterEarlyStages =
        handleRoutedRequest
          RoutedRequestExecution
            { routedRequestApplication = webApplication,
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
  let routeDispatch =
        matchRouteMethod
          (routeCodec webApplication)
          requestContext
          (requestMethodText request)
          (waiRequestRouteTarget requestPolicyConfig request)
      routeRequest = routeDispatchRequest routeDispatch
  routeMatchedAt <- routeDispatch `seq` getMonotonicTimeNSec
  renderStartedAt <- getMonotonicTimeNSec
  response <- dispatchRoutedRequest routedRequestExecution routeDispatch middlewareResult
  responseRenderedAt <- response `seq` getMonotonicTimeNSec
  runtimeNonce <- responseRuntimeNonce response
  let executionTimings =
        RequestExecutionTimings
          { requestExecutionStartedAt = requestStartedAt,
            requestPolicyEvaluatedAt = policyEvaluatedAt,
            requestMiddlewareTimings = middlewareTiming,
            requestRouteMatchingStartedAt = routeMatchingStartedAt,
            requestRouteMatchedAt = routeMatchedAt,
            requestRenderingStartedAt = renderStartedAt,
            requestResponseRenderedAt = responseRenderedAt
          }
  finalizeRoutedResponse routedRequestExecution executionTimings routeRequest (isHeadDispatch routeDispatch) runtimeNonce response

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

dispatchRoutedRequest ::
  RoutedRequestExecution route action context ->
  RouteDispatch route context ->
  MiddlewareResult context ->
  IO (Response route context)
dispatchRoutedRequest _ _ (HaltMiddleware _ responseBody) = pure (BodyResponse responseBody)
dispatchRoutedRequest
  routedRequestExecution
  routeDispatch
  (ContinueMiddleware _) =
    let webApplication = routedRequestApplication routedRequestExecution
        request = routedRequestWaiRequest routedRequestExecution
        requestPath = routedRequestPath routedRequestExecution
        requestPolicyConfig = routedRequestPolicyConfig routedRequestExecution
        RouteRequest {requestContext = routedRequestContext} = routeDispatchRequest routeDispatch
     in if isClientActionRequest request
          then do
            let expectedOrigin =
                  (\host -> requestScheme requestPolicyConfig request <> "://" <> host)
                    <$> (lookup "Host" (Wai.requestHeaders request) >>= either (const Nothing) Just . TextEncoding.decodeUtf8')
            case validateClientActionRequest expectedOrigin request of
              Left protocolError -> pure (BodyResponse (clientActionProtocolErrorResponse protocolError))
              Right () -> do
                actionBody <- readClientActionBody request
                case actionBody >>= parseClientActionFields of
                  Left protocolError -> pure (BodyResponse (clientActionProtocolErrorResponse protocolError))
                  Right actionFields -> do
                    case validateClientActionCsrf request actionFields of
                      Left protocolError -> pure (BodyResponse (clientActionProtocolErrorResponse protocolError))
                      Right () -> do
                        let actionPayload =
                              ClientActionPayload
                                { clientActionMethod = requestMethodText request,
                                  clientActionPath = requestPath,
                                  clientActionFields = actionFields,
                                  clientActionCsrfToken = lookup "_harch_csrf" actionFields,
                                  clientActionIdempotencyKey = requestIdempotencyKey request,
                                  clientActionPayloadContext = routedRequestContext
                                }
                        case decodeClientAction webApplication actionPayload of
                          UnrecognizedClientAction ->
                            pure
                              (BodyResponse (clientActionProtocolErrorResponse ClientActionNotFound))
                          MethodNotAllowedClientAction allowedMethods ->
                            pure
                              (ClientActionBodyResponse (clientActionMethodNotAllowedResponse allowedMethods))
                          MalformedClientAction _ ->
                            pure
                              (BodyResponse (clientActionProtocolErrorResponse ClientActionPayloadMalformed))
                          DecodedClientAction action -> do
                            maybeActionResponse <-
                              handleClientAction
                                webApplication
                                ClientActionRequest
                                  { clientAction = action,
                                    clientActionRequestIdempotencyKey = requestIdempotencyKey request,
                                    clientActionContext = routedRequestContext
                                  }
                            pure
                              ( maybe
                                  (BodyResponse (clientActionProtocolErrorResponse ClientActionNotFound))
                                  ClientActionBodyResponse
                                  maybeActionResponse
                              )
          else renderRouteDispatch webApplication request routeDispatch

renderRouteDispatch :: Application route action context -> Wai.Request -> RouteDispatch route context -> IO (Response route context)
renderRouteDispatch webApplication request routeDispatch =
  case routeDispatch of
    RouteNotFound routeRequest -> renderRequestResponse webApplication request routeRequest
    RouteMethodNotAllowed _ declaredMethods ->
      pure
        ( ProtocolResponseResult
            ProtocolResponse
              { protocolResponseStatus = Http.status405,
                protocolResponseHeaders = [(Http.hAllow, TextEncoding.encodeUtf8 (routeAllowHeaderValue declaredMethods))],
                protocolResponseBody = ProtocolResponseBytes ByteString.empty,
                protocolResponseObservabilityAttributes = [],
                protocolResponseLogEntries = []
              }
        )
    RouteMatched routeRequest -> renderRequestResponse webApplication request routeRequest
    RouteMatchedHead routeRequest -> renderRequestResponse webApplication request routeRequest
    RouteOptions _ declaredMethods ->
      pure
        ( ProtocolResponseResult
            ProtocolResponse
              { protocolResponseStatus = Http.status204,
                protocolResponseHeaders = [(Http.hAllow, TextEncoding.encodeUtf8 (routeAllowHeaderValue declaredMethods))],
                protocolResponseBody = ProtocolResponseBytes ByteString.empty,
                protocolResponseObservabilityAttributes = [],
                protocolResponseLogEntries = []
              }
        )

requestIdempotencyKey :: Wai.Request -> Maybe ClientActionIdempotencyKey
requestIdempotencyKey request =
  lookup "Idempotency-Key" (Wai.requestHeaders request)
    >>= either (const Nothing) Just . TextEncoding.decodeUtf8'

readClientActionBody :: Wai.Request -> IO (Either ClientActionProtocolError LazyByteString.ByteString)
readClientActionBody request = do
  result <- readRequestBodyUpTo maxClientActionBodyBytes request
  pure $
    case result of
      Left RequestBodyLimitExceeded -> Left ClientActionBodyTooLarge
      Right requestBody -> Right requestBody

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
  RouteRequest route context ->
  Bool ->
  Document.RuntimeNonce ->
  Response route context ->
  IO Wai.ResponseReceived
finalizeRoutedResponse routedRequestExecution executionTimings routeRequest omitResponseBody runtimeNonce response = do
  let webApplication = routedRequestApplication routedRequestExecution
      request = routedRequestWaiRequest routedRequestExecution
      respond = routedRequestRespond routedRequestExecution
      requestPolicyConfig = routedRequestPolicyConfig routedRequestExecution
  let requestLogFields = requestLogContextFields requestPolicyConfig request
      diagnosticValues = responseDiagnostics response
      contextualizedLogs = map (prependRequestLogContext requestLogFields) (diagnosticLogEntries diagnosticValues)
      observabilityValue = buildRoutedRequestObservability routedRequestExecution executionTimings routeRequest response diagnosticValues
  responseReceived <-
    respond
      ( omitResponseBodyWhen
          omitResponseBody
          (applyResponseHeaders (responsePolicyHeaders requestPolicyConfig request runtimeNonce response) (toWaiResponse [] runtimeNonce webApplication response))
      )
  Observability.forceRequestObservability observabilityValue `seq`
    reportRequestObservability webApplication observabilityValue
      >> mapM_ (reportApplicationLog webApplication) contextualizedLogs
  pure responseReceived

omitResponseBodyWhen :: Bool -> Wai.Response -> Wai.Response
omitResponseBodyWhen omitResponseBody waiResponse =
  if omitResponseBody
    then Wai.responseStream (Wai.responseStatus waiResponse) (Wai.responseHeaders waiResponse) (\_write flush -> flush)
    else waiResponse

buildRoutedRequestObservability ::
  (Eq route) =>
  RoutedRequestExecution route action context ->
  RequestExecutionTimings ->
  RouteRequest route context ->
  Response route context ->
  ResponseDiagnostics ->
  Observability.RequestObservability
buildRoutedRequestObservability routedRequestExecution executionTimings routeRequest response diagnosticValues =
  let webApplication = routedRequestApplication routedRequestExecution
      request = routedRequestWaiRequest routedRequestExecution
      requestPolicyConfig = routedRequestPolicyConfig routedRequestExecution
      requestPath = routedRequestPath routedRequestExecution
   in maybe id Observability.withRequestTraceContext (requestTraceContext request) $
        Observability.buildRequestObservability
          (requestMethodText request)
          (requestScheme requestPolicyConfig request)
          requestPath
          (renderRoute (routeCodec webApplication) routeRequest)
          (responseStatusCode webApplication response)
          (responseKind response)
          ( requestContextObservabilityAttributes requestPolicyConfig request
              <> diagnosticObservabilityAttributes diagnosticValues
              <> requestTimingObservabilityAttributes
                (requestExecutionStartedAt executionTimings)
                (requestResponseRenderedAt executionTimings)
                ( [("request-policy", requestExecutionStartedAt executionTimings, requestPolicyEvaluatedAt executionTimings)]
                    <> requestMiddlewareTimings executionTimings
                    <> [ ("route-match", requestRouteMatchingStartedAt executionTimings, requestRouteMatchedAt executionTimings),
                         ("render-response", requestRenderingStartedAt executionTimings, requestResponseRenderedAt executionTimings)
                       ]
                )
          )

requestTimingObservabilityAttributes :: Word64 -> Word64 -> [(Text, Word64, Word64)] -> [Observability.ObservabilityAttribute]
requestTimingObservabilityAttributes requestStartedAt requestCompletedAt phaseTimings =
  intObservabilityAttribute "harch.request.start_monotonic_ns" (fromIntegral requestStartedAt)
    : intObservabilityAttribute "harch.request.duration_ns" (nanosecondsBetween requestStartedAt requestCompletedAt)
    : concatMap phaseTimingAttributes phaseTimings
  where
    phaseTimingAttributes (phaseName, phaseStartedAt, phaseEndedAt) =
      [ intObservabilityAttribute ("harch.phase." <> phaseName <> ".start_offset_ns") (nanosecondsBetween requestStartedAt phaseStartedAt),
        intObservabilityAttribute ("harch.phase." <> phaseName <> ".duration_ns") (nanosecondsBetween phaseStartedAt phaseEndedAt)
      ]

nanosecondsBetween :: Word64 -> Word64 -> Int
nanosecondsBetween start end = fromIntegral (end - min start end)

intObservabilityAttribute :: Text -> Int -> Observability.ObservabilityAttribute
intObservabilityAttribute name value =
  Observability.ObservabilityAttribute
    { Observability.attributeName = name,
      Observability.attributeValue = Observability.IntAttribute value
    }

requestMethodText :: Wai.Request -> Text
requestMethodText = TextEncoding.decodeUtf8With TextEncodingError.lenientDecode . Wai.requestMethod

reportEarlyRequestObservability ::
  (Eq route) =>
  Application route action context ->
  Wai.Request ->
  Word64 ->
  Word64 ->
  Text ->
  Wai.Response ->
  IO ()
reportEarlyRequestObservability webApplication request requestStartedAt requestCompletedAt routePath response =
  let requestPolicyConfig = applicationRequestPolicy webApplication
      requestObservability =
        maybe id Observability.withRequestTraceContext (requestTraceContext request) $
          Observability.buildRequestObservability
            (requestMethodText request)
            (requestScheme requestPolicyConfig request)
            (waiRequestPath requestPolicyConfig request)
            routePath
            (Http.statusCode (Wai.responseStatus response))
            Observability.BodyResponseKind
            (requestContextObservabilityAttributes requestPolicyConfig request <> requestTimingObservabilityAttributes requestStartedAt requestCompletedAt [])
   in Observability.forceRequestObservability requestObservability `seq`
        reportRequestObservability webApplication requestObservability
