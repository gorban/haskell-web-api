{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module HarchWeb
  ( module HarchWeb.Acme,
    module HarchWeb.Document,
    ConnectionObservability (..),
    HttpServerMetrics (..),
    ObservabilityAttribute (ObservabilityAttribute),
    ObservabilityAttributeValue (..),
    RequestTraceContext (..),
    RequestObservability (..),
    RequestSpan (..),
    ResponseKind (..),
    buildConnectionObservability,
    buildRequestObservability,
    forceConnectionObservability,
    forceRequestObservability,
    requestObservabilityAttributes,
    requestSpanName,
    withRequestTraceContext,
    module HarchWeb.Routing,
    module HarchWeb.Security,
    module HarchWeb.Server,
    module HarchWeb.StaticAssets,
    LocalTestServer (..),
    ObservabilityStartupPlan (..),
    ReloadingTlsCredentials,
    exportConnectionObservabilityToOtlp,
    exportRequestObservabilityToOtlp,
    navigationRuntimeScriptSource,
    planObservabilityStartup,
    reloadTlsCredentialsIfChanged,
    loadReloadingTlsCredentials,
    loadTlsCredentialSnapshotOrThrowWithLoader,
    startManualTlsRuntimeServerWithStarter,
    runServer,
    startWarpRuntimeServerOnSocket,
    withLocalTestServer,
  )
where

import Data.ByteString.Lazy qualified as LazyByteString
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Word (Word64)
import HarchWeb.Acme
import HarchWeb.Document
  ( Document (..),
    HtmlAttribute (..),
    LiveRegion (..),
    NavigationItem (..),
    NavigationRuntime (..),
    Page (..),
    PageShell (..),
    ResolvedNavigationItem (..),
    RuntimeDescriptor (..),
    RuntimeNonce (..),
    buildDocument,
    buildNavigation,
    buildPageShell,
    defaultCaptureKernel,
    defaultCaptureKernelScript,
    defaultNavigationRuntime,
    defaultNavigationRuntimeScript,
    generateRuntimeNonce,
    liveRegionAttributes,
    renderDocument,
    renderDocumentWithNonce,
  )
import HarchWeb.Observability
import HarchWeb.Observability qualified as Observability
import HarchWeb.Observability.Otlp qualified as Otlp
import HarchWeb.Observability.Otlp.Wire qualified as OtlpWire
import HarchWeb.Routing (RouteCodec (..), RouteRequest (..), matchRoute, routeHref)
import HarchWeb.Security
import HarchWeb.Server
import HarchWeb.Server.LocalTest (LocalTestServer (..), withLocalTestServer)
import HarchWeb.Server.Runtime (runServer)
import HarchWeb.Server.Transport
  ( ReloadingTlsCredentials,
    loadReloadingTlsCredentials,
    loadTlsCredentialSnapshotOrThrowWithLoader,
    reloadTlsCredentialsIfChanged,
    startManualTlsRuntimeServerWithStarter,
    startWarpRuntimeServerOnSocket,
  )
import HarchWeb.StaticAssets
  ( AssetPath (..),
    CssClass (..),
    CssScope (..),
    StaticAssetRoot (..),
    StaticAssetsConfig (..),
    Stylesheet (..),
    cssClassText,
    cssScope,
    defaultStaticAssetContentTypes,
    staticAssetHref,
    staticAssetHrefWithPrefix,
    stylesheet,
  )

navigationRuntimeScriptSource :: Text -> NavigationRuntime -> Text
navigationRuntimeScriptSource pathPrefix runtime =
  applyRequestPathPrefix pathPrefix (navigationRuntimePath runtime)

textObservabilityAttribute :: Text -> Text -> Observability.ObservabilityAttribute
textObservabilityAttribute name value =
  Observability.ObservabilityAttribute
    { Observability.attributeName = name,
      Observability.attributeValue = Observability.TextAttribute value
    }

intObservabilityAttribute :: Text -> Int -> Observability.ObservabilityAttribute
intObservabilityAttribute name value =
  Observability.ObservabilityAttribute
    { Observability.attributeName = name,
      Observability.attributeValue = Observability.IntAttribute value
    }

exportRequestObservabilityToOtlp ::
  Text ->
  OtlpExporter ->
  Observability.RequestObservability ->
  IO ()
exportRequestObservabilityToOtlp serviceName exporter requestObservability = do
  (generatedTraceId, spanId) <- Otlp.nextOtlpSpanIdentifiers
  let childSpans =
        requestRuntimePhaseChildSpans requestObservability
          <> requestDatabaseChildSpans requestObservability
  childSpanIds <- mapM (const Otlp.nextOtlpSpanId) childSpans
  endTimeUnixNano <- Otlp.currentUnixTimeNSec
  let rootDurationNanoseconds =
        fromMaybe
          (requestFallbackDurationNanoseconds childSpans)
          (requestDurationNanoseconds requestObservability)
      startTimeUnixNano = nonNegativeStartTime endTimeUnixNano rootDurationNanoseconds
      traceId =
        maybe
          generatedTraceId
          Observability.traceContextTraceId
          (Observability.observabilityTraceContext requestObservability)
      parentSpanId =
        Observability.traceContextParentSpanId
          <$> Observability.observabilityTraceContext requestObservability
      traceState =
        Observability.traceContextState
          =<< Observability.observabilityTraceContext requestObservability
      timedChildSpans =
        zipWith
          (timedOtlpChildSpan startTimeUnixNano rootDurationNanoseconds)
          childSpanIds
          childSpans
      rootSpan =
        withoutDatabaseOperationAttributes
          (Observability.observabilityRequestSpan requestObservability)
  let requestBody =
        OtlpWire.otlpTraceBodyFromSpan
          serviceName
          traceId
          spanId
          parentSpanId
          traceState
          startTimeUnixNano
          endTimeUnixNano
          rootSpan
          "SPAN_KIND_SERVER"
          (otlpRequestSpanStatusFields requestObservability)
          timedChildSpans
  Otlp.sendOtlpTraceRequest exporter requestBody

exportConnectionObservabilityToOtlp ::
  Text ->
  OtlpExporter ->
  Observability.ConnectionObservability ->
  IO ()
exportConnectionObservabilityToOtlp serviceName exporter connectionObservability = do
  (traceId, spanId) <- Otlp.nextOtlpSpanIdentifiers
  endTimeUnixNano <- Otlp.currentUnixTimeNSec
  let startTimeUnixNano = nonNegativeStartTime endTimeUnixNano connectionFallbackDurationNanoseconds
  let requestBody =
        OtlpWire.otlpTraceBodyFromSpan
          serviceName
          traceId
          spanId
          Nothing
          Nothing
          startTimeUnixNano
          endTimeUnixNano
          (Observability.observabilityConnectionSpan connectionObservability)
          "SPAN_KIND_INTERNAL"
          OtlpWire.otlpErrorStatusFields
          []
  Otlp.sendOtlpTraceRequest exporter requestBody

minimumOtlpSpanDurationNanoseconds :: Word64
minimumOtlpSpanDurationNanoseconds = 1000

requestFallbackDurationNanoseconds :: [(Text, Observability.RequestSpan)] -> Word64
requestFallbackDurationNanoseconds childSpans =
  minimumOtlpSpanDurationNanoseconds * fromIntegral (max 1 (length childSpans + 1))

connectionFallbackDurationNanoseconds :: Word64
connectionFallbackDurationNanoseconds = minimumOtlpSpanDurationNanoseconds

nonNegativeStartTime :: Word64 -> Word64 -> Word64
nonNegativeStartTime endTimeUnixNano durationNanos =
  endTimeUnixNano - min endTimeUnixNano durationNanos

timedOtlpChildSpan :: Word64 -> Word64 -> Text -> (Text, Observability.RequestSpan) -> (Text, Text, Word64, Word64, Observability.RequestSpan)
timedOtlpChildSpan rootStartTimeUnixNano rootDurationNanoseconds childSpanId (childSpanKind, childSpan) =
  ( childSpanId,
    childSpanKind,
    childStartTimeUnixNano,
    childStartTimeUnixNano + childDurationNanoseconds,
    childSpan
  )
  where
    childStartOffsetNanoseconds =
      fromMaybe 0 (requestSpanIntAttribute "harch.span.start_offset_ns" childSpan)
    childDurationNanoseconds =
      fromMaybe rootDurationNanoseconds (requestSpanIntAttribute "harch.span.duration_ns" childSpan)
    childStartTimeUnixNano =
      rootStartTimeUnixNano + min rootDurationNanoseconds childStartOffsetNanoseconds

requestDurationNanoseconds :: Observability.RequestObservability -> Maybe Word64
requestDurationNanoseconds requestObservability =
  requestSpanIntAttribute "harch.request.duration_ns" (Observability.observabilityRequestSpan requestObservability)

requestSpanIntAttribute :: Text -> Observability.RequestSpan -> Maybe Word64
requestSpanIntAttribute attributeName requestSpan =
  listToMaybe
    [ fromIntegral attributeValue
    | Observability.ObservabilityAttribute
        { Observability.attributeName = currentName,
          Observability.attributeValue = Observability.IntAttribute attributeValue
        } <-
        Observability.requestSpanAttributes requestSpan,
      currentName == attributeName,
      attributeValue >= 0
    ]

requestRuntimePhaseChildSpans :: Observability.RequestObservability -> [(Text, Observability.RequestSpan)]
requestRuntimePhaseChildSpans requestObservability =
  mapMaybe
    (\(displayName, phaseName, copiedAttributeNames) -> runtimePhaseChildSpan displayName phaseName copiedAttributeNames)
    [ ("HarchWeb request policy", "request-policy", ["http.request.method", "url.scheme"]),
      ("HarchWeb route match", "route-match", ["url.path", "http.route"]),
      ("HarchWeb render response", "render-response", ["http.response.status_code", "harch.response.kind"])
    ]
  where
    rootAttributes =
      Observability.requestSpanAttributes
        (Observability.observabilityRequestSpan requestObservability)

    runtimePhaseChildSpan displayName phaseName copiedAttributeNames =
      case phaseTimingAttributes phaseName of
        [] -> Nothing
        timingAttributes ->
          Just
            ( "SPAN_KIND_INTERNAL",
              Observability.RequestSpan
                { Observability.requestSpanDisplayName = displayName,
                  Observability.requestSpanAttributes =
                    [textObservabilityAttribute "harch.span.phase" phaseName]
                      <> timingAttributes
                      <> concatMap (`attributesNamed` rootAttributes) copiedAttributeNames
                }
            )

    phaseTimingAttributes phaseName =
      renamedIntAttribute
        "harch.span.start_offset_ns"
        ("harch.phase." <> phaseName <> ".start_offset_ns")
        <> renamedIntAttribute
          "harch.span.duration_ns"
          ("harch.phase." <> phaseName <> ".duration_ns")

    renamedIntAttribute childName rootName =
      [ Observability.ObservabilityAttribute
          { Observability.attributeName = childName,
            Observability.attributeValue = Observability.IntAttribute attributeValue
          }
      | Observability.ObservabilityAttribute
          { Observability.attributeName = currentName,
            Observability.attributeValue = Observability.IntAttribute attributeValue
          } <-
          rootAttributes,
        currentName == rootName
      ]

attributesNamed :: Text -> [Observability.ObservabilityAttribute] -> [Observability.ObservabilityAttribute]
attributesNamed expectedName attributes =
  [ attribute
  | attribute <- attributes,
    Observability.attributeName attribute == expectedName
  ]

requestDatabaseChildSpans :: Observability.RequestObservability -> [(Text, Observability.RequestSpan)]
requestDatabaseChildSpans requestObservability =
  databaseChildSpansFromAttributes requestStartMonotonicNanoseconds rootAttributes
  where
    rootSpan = Observability.observabilityRequestSpan requestObservability
    rootAttributes = Observability.requestSpanAttributes rootSpan
    requestStartMonotonicNanoseconds =
      requestSpanIntAttribute "harch.request.start_monotonic_ns" rootSpan

withoutDatabaseOperationAttributes :: Observability.RequestSpan -> Observability.RequestSpan
withoutDatabaseOperationAttributes requestSpan =
  requestSpan
    { Observability.requestSpanAttributes =
        filter
          (not . isDatabaseOperationAttribute)
          (Observability.requestSpanAttributes requestSpan)
    }

isDatabaseOperationAttribute :: Observability.ObservabilityAttribute -> Bool
isDatabaseOperationAttribute attribute =
  Observability.attributeName attribute
    `elem` [ "db.system",
             "db.operation.name",
             "db.query.template",
             "db.operation.start_monotonic_ns",
             "db.operation.duration_ns"
           ]

databaseChildSpansFromAttributes :: Maybe Word64 -> [Observability.ObservabilityAttribute] -> [(Text, Observability.RequestSpan)]
databaseChildSpansFromAttributes requestStartMonotonicNanoseconds =
  go
  where
    go currentAttributes =
      case currentAttributes of
        [] -> []
        systemAttribute@Observability.ObservabilityAttribute {Observability.attributeName = "db.system", Observability.attributeValue = Observability.TextAttribute _}
          : operationAttribute@Observability.ObservabilityAttribute {Observability.attributeName = "db.operation.name", Observability.attributeValue = Observability.TextAttribute operationName}
          : queryAttribute@Observability.ObservabilityAttribute {Observability.attributeName = "db.query.template", Observability.attributeValue = Observability.TextAttribute _}
          : startedAtAttribute@Observability.ObservabilityAttribute {Observability.attributeName = "db.operation.start_monotonic_ns", Observability.attributeValue = Observability.IntAttribute _}
          : durationAttribute@Observability.ObservabilityAttribute {Observability.attributeName = "db.operation.duration_ns", Observability.attributeValue = Observability.IntAttribute _}
          : remainingAttributes ->
            ( "SPAN_KIND_CLIENT",
              databaseOperationChildSpan
                requestStartMonotonicNanoseconds
                operationName
                systemAttribute
                operationAttribute
                queryAttribute
                [startedAtAttribute, durationAttribute]
            )
              : go remainingAttributes
        systemAttribute@Observability.ObservabilityAttribute {Observability.attributeName = "db.system", Observability.attributeValue = Observability.TextAttribute _}
          : operationAttribute@Observability.ObservabilityAttribute {Observability.attributeName = "db.operation.name", Observability.attributeValue = Observability.TextAttribute operationName}
          : queryAttribute@Observability.ObservabilityAttribute {Observability.attributeName = "db.query.template", Observability.attributeValue = Observability.TextAttribute _}
          : remainingAttributes ->
            ( "SPAN_KIND_CLIENT",
              databaseOperationChildSpan requestStartMonotonicNanoseconds operationName systemAttribute operationAttribute queryAttribute []
            )
              : go remainingAttributes
        _ : remainingAttributes ->
          go remainingAttributes

databaseOperationChildSpan ::
  Maybe Word64 ->
  Text ->
  Observability.ObservabilityAttribute ->
  Observability.ObservabilityAttribute ->
  Observability.ObservabilityAttribute ->
  [Observability.ObservabilityAttribute] ->
  Observability.RequestSpan
databaseOperationChildSpan requestStartMonotonicNanoseconds operationName systemAttribute operationAttribute queryAttribute timingAttributes =
  Observability.RequestSpan
    { Observability.requestSpanDisplayName =
        "DB " <> operationName,
      Observability.requestSpanAttributes =
        [systemAttribute, operationAttribute, queryAttribute]
          <> databaseOperationTimingAttributes requestStartMonotonicNanoseconds timingAttributes
    }

databaseOperationTimingAttributes :: Maybe Word64 -> [Observability.ObservabilityAttribute] -> [Observability.ObservabilityAttribute]
databaseOperationTimingAttributes requestStartMonotonicNanoseconds timingAttributes =
  case (requestStartMonotonicNanoseconds, attributeIntValue "db.operation.start_monotonic_ns" timingAttributes, attributeIntValue "db.operation.duration_ns" timingAttributes) of
    (Just requestStartedAt, Just operationStartedAt, Just operationDuration) ->
      [ intObservabilityAttribute
          "harch.span.start_offset_ns"
          (fromIntegral (operationStartedAt - min requestStartedAt operationStartedAt)),
        intObservabilityAttribute
          "harch.span.duration_ns"
          (fromIntegral operationDuration)
      ]
    _ -> []

attributeIntValue :: Text -> [Observability.ObservabilityAttribute] -> Maybe Word64
attributeIntValue expectedName attributes =
  listToMaybe
    [ fromIntegral attributeValue
    | Observability.ObservabilityAttribute
        { Observability.attributeName = currentName,
          Observability.attributeValue = Observability.IntAttribute attributeValue
        } <-
        attributes,
      currentName == expectedName,
      attributeValue >= 0
    ]

otlpRequestSpanStatusFields :: Observability.RequestObservability -> [(Text, LazyByteString.ByteString)]
otlpRequestSpanStatusFields requestObservability =
  case requestObservabilityStatusCode requestObservability of
    Just statusCode
      | statusCode >= 500 ->
          OtlpWire.otlpErrorStatusFields
    _ -> []

requestObservabilityStatusCode :: Observability.RequestObservability -> Maybe Int
requestObservabilityStatusCode requestObservability =
  listToMaybe
    [ statusCode
    | Observability.ObservabilityAttribute
        { Observability.attributeName = "http.response.status_code",
          Observability.attributeValue = Observability.IntAttribute statusCode
        } <-
        Observability.requestSpanAttributes
          (Observability.observabilityRequestSpan requestObservability)
    ]
