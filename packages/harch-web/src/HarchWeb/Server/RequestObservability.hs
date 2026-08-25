{-# LANGUAGE OverloadedStrings #-}

-- | Private construction and reporting of request observability.
--
-- Request execution owns routing, admission, and response delivery; this
-- module owns the derived telemetry and application-log values that follow
-- delivery. Keeping that boundary separate ensures all routed and early
-- response paths retain one exact encoding of request identity, timing,
-- response diagnostics, and trace context without regrowing the execution
-- facade around observability plumbing.
module HarchWeb.Server.RequestObservability
  ( RequestExecutionTimings (..),
    reportEarlyRequestObservability,
    reportRoutedResponseObservability,
  )
where

import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.Encoding.Error qualified as TextEncodingError
import Data.Word (Word64)
import HarchWeb.Observability qualified as Observability
import HarchWeb.Routing (RouteRequest, renderRoute)
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

reportRoutedResponseObservability ::
  (Eq route) =>
  Application route action context ->
  Wai.Request ->
  RequestPolicyConfig ->
  Text ->
  RequestExecutionTimings ->
  RouteRequest route context ->
  Response route context ->
  IO ()
reportRoutedResponseObservability webApplication request requestPolicyConfig requestPath executionTimings routeRequest response = do
  let diagnosticValues = responseDiagnostics response
      requestLogFields = requestLogContextFields requestPolicyConfig request
      contextualizedLogs = map (prependRequestLogContext requestLogFields) (diagnosticLogEntries diagnosticValues)
      observabilityValue = buildRoutedRequestObservability webApplication request requestPolicyConfig requestPath executionTimings routeRequest response diagnosticValues
  Observability.forceRequestObservability observabilityValue `seq`
    reportRequestObservability webApplication observabilityValue
      >> mapM_ (reportApplicationLog webApplication) contextualizedLogs

buildRoutedRequestObservability ::
  (Eq route) =>
  Application route action context ->
  Wai.Request ->
  RequestPolicyConfig ->
  Text ->
  RequestExecutionTimings ->
  RouteRequest route context ->
  Response route context ->
  ResponseDiagnostics ->
  Observability.RequestObservability
buildRoutedRequestObservability webApplication request requestPolicyConfig requestPath executionTimings routeRequest response diagnosticValues =
  Observability.withDatabaseOperations (diagnosticDatabaseOperations diagnosticValues) $
    maybe id Observability.withRequestTraceContext (requestTraceContext request) $
      Observability.buildRequestObservability
        Observability.RequestIdentity
          { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel (requestMethodText request),
            Observability.requestIdentityScheme = requestScheme requestPolicyConfig request,
            Observability.requestIdentityPath = requestPath,
            Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath (renderRoute (routeCodec webApplication) routeRequest)
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

reportEarlyRequestObservability ::
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
