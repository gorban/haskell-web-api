{-# LANGUAGE OverloadedStrings #-}

-- | Private construction and reporting of request observability.
--
-- Request execution owns routing, admission, and response delivery; this
-- module owns the derived telemetry and application-log values that follow
-- delivery. Keeping that boundary separate ensures all routed and early
-- response paths retain one exact encoding of request identity, timing,
-- response diagnostics, and trace context without regrowing the execution
-- facade around observability plumbing.
--
-- FQ8 keeps the per-response route, timing, and response values explicit,
-- while 'RequestObservabilityContext' owns the stable application, WAI
-- request, and resolved policy that every reporter needs.  The force at this
-- observability boundary remains deliberate: it makes exceptions from pure
-- telemetry construction occur where the framework can associate them with
-- the report, rather than moving evaluation into request delivery.
module HarchWeb.Server.RequestObservability
  ( RequestExecutionTimings (..),
    RequestObservabilityContext,
    requestObservabilityContext,
    reportEarlyRequestObservability,
    reportRoutedResponseObservability,
  )
where

import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.Encoding.Error qualified as TextEncodingError
import Data.Word (Word64)
import HarchWeb.Markup (safeUrlText)
import HarchWeb.Observability qualified as Observability
import HarchWeb.Routing (RouteRequest, encodeRouteLocation, renderRoute)
import HarchWeb.Security
  ( RequestPolicyConfig,
    prependRequestLogContext,
    requestContextObservabilityAttributes,
    requestLogContextFields,
    requestScheme,
    requestTraceContext,
    waiRequestPath,
  )
import HarchWeb.Server.Application
import HarchWeb.Server.Response
import HarchWeb.Server.ResponseRendering (responseDiagnostics, responseKind, responseStatusCode)
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai

data RequestExecutionTimings = RequestExecutionTimings
  { requestExecutionStartedAt :: Word64,
    requestPolicyEvaluatedAt :: Word64,
    requestMiddlewareTimings :: [(Text, Word64, Word64)],
    requestRouteMatchingStartedAt :: Word64,
    requestRouteMatchedAt :: Word64,
    requestRenderingStartedAt :: Word64,
    requestResponseRenderedAt :: Word64
  }

-- | Values fixed for every observability result emitted by one WAI request.
-- Response-specific path, timing, route, and diagnostic values intentionally
-- remain arguments to the reporting operations.
data RequestObservabilityContext route action context authorization = RequestObservabilityContext
  { requestObservabilityApplication :: Application route action context authorization,
    requestObservabilityWaiRequest :: Wai.Request,
    requestObservabilityPolicyConfig :: RequestPolicyConfig
  }

requestObservabilityContext :: Application route action context authorization -> Wai.Request -> RequestPolicyConfig -> RequestObservabilityContext route action context authorization
requestObservabilityContext = RequestObservabilityContext

reportRoutedResponseObservability ::
  (Eq route) =>
  RequestObservabilityContext route action context authorization ->
  Text ->
  RequestExecutionTimings ->
  RouteRequest route context ->
  Response route context ->
  IO ()
reportRoutedResponseObservability observabilityContext requestPath executionTimings routeRequest response = do
  let diagnosticValues = responseDiagnostics response
      webApplication = requestObservabilityApplication observabilityContext
      request = requestObservabilityWaiRequest observabilityContext
      requestPolicyConfig = requestObservabilityPolicyConfig observabilityContext
      requestLogFields = requestLogContextFields requestPolicyConfig request
      contextualizedLogs = map (prependRequestLogContext requestLogFields) (diagnosticLogEntries diagnosticValues)
      observabilityValue = buildRoutedRequestObservability observabilityContext requestPath executionTimings routeRequest response diagnosticValues
  Observability.forceRequestObservability observabilityValue `seq`
    reportRequestObservability webApplication observabilityValue
      >> mapM_ (reportApplicationLog webApplication) contextualizedLogs

buildRoutedRequestObservability ::
  (Eq route) =>
  RequestObservabilityContext route action context authorization ->
  Text ->
  RequestExecutionTimings ->
  RouteRequest route context ->
  Response route context ->
  ResponseDiagnostics ->
  Observability.RequestObservability
buildRoutedRequestObservability observabilityContext requestPath executionTimings routeRequest response diagnosticValues =
  Observability.withDatabaseOperations (diagnosticDatabaseOperations diagnosticValues) $
    maybe id Observability.withRequestTraceContext (requestTraceContext request) $
      Observability.buildRequestObservability
        Observability.RequestIdentity
          { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel (requestMethodText request),
            Observability.requestIdentityScheme = requestScheme requestPolicyConfig request,
            Observability.requestIdentityPath = requestPath,
            Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath (safeUrlText (encodeRouteLocation (renderRoute (routeCodec webApplication) routeRequest)))
          }
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
  where
    webApplication = requestObservabilityApplication observabilityContext
    request = requestObservabilityWaiRequest observabilityContext
    requestPolicyConfig = requestObservabilityPolicyConfig observabilityContext

reportEarlyRequestObservability ::
  RequestObservabilityContext route action context authorization ->
  Word64 ->
  Word64 ->
  Text ->
  Wai.Response ->
  IO ()
reportEarlyRequestObservability observabilityContext requestStartedAt requestCompletedAt routePath response =
  let webApplication = requestObservabilityApplication observabilityContext
      request = requestObservabilityWaiRequest observabilityContext
      requestPolicyConfig = requestObservabilityPolicyConfig observabilityContext
      requestObservability =
        maybe id Observability.withRequestTraceContext (requestTraceContext request) $
          Observability.buildRequestObservability
            Observability.RequestIdentity
              { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel (requestMethodText request),
                Observability.requestIdentityScheme = requestScheme requestPolicyConfig request,
                Observability.requestIdentityPath = waiRequestPath requestPolicyConfig request,
                Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath routePath
              }
            (Http.statusCode (Wai.responseStatus response))
            Observability.BodyResponseKind
            (requestContextObservabilityAttributes requestPolicyConfig request <> requestTimingObservabilityAttributes requestStartedAt requestCompletedAt [])
   in Observability.forceRequestObservability requestObservability `seq`
        reportRequestObservability webApplication requestObservability

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
