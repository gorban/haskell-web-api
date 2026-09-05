{-# LANGUAGE OverloadedStrings #-}

-- | Private OTLP span projection and export workflows.
module HarchWeb.Observability.Otlp.Export
  ( exportConnectionObservabilityToOtlp,
    exportRequestObservabilityToOtlp,
  )
where

import Data.ByteString.Lazy qualified as LazyByteString
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Word (Word64)
import HarchWeb.Database qualified as Database
import HarchWeb.Observability.Otlp qualified as Otlp
import HarchWeb.Observability.Otlp.Wire qualified as OtlpWire
import HarchWeb.Observability.Types qualified as Observability
import Network.HTTP.Client qualified as HttpClient

exportRequestObservabilityToOtlp ::
  HttpClient.Manager ->
  Text ->
  Observability.OtlpExporter ->
  Observability.RequestObservability ->
  IO (Either Otlp.OtlpExportFailure ())
exportRequestObservabilityToOtlp manager serviceName exporter requestObservability = do
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
      rootSpan = Observability.observabilityRequestSpan requestObservability
  let requestBody =
        OtlpWire.otlpTraceBodyFromSpan
          OtlpWire.OtlpTraceIdentity {OtlpWire.otlpTraceServiceName = serviceName, OtlpWire.otlpTraceId = traceId, OtlpWire.otlpTraceState = traceState}
          OtlpWire.OtlpSpanIdentity {OtlpWire.otlpSpanId = spanId, OtlpWire.otlpSpanParentId = parentSpanId, OtlpWire.otlpSpanKindValue = OtlpWire.OtlpServerSpan}
          OtlpWire.OtlpSpanTiming {OtlpWire.otlpSpanStartTimeUnixNano = startTimeUnixNano, OtlpWire.otlpSpanEndTimeUnixNano = endTimeUnixNano}
          OtlpWire.OtlpRootSpanContent {OtlpWire.otlpRootRequestSpan = rootSpan, OtlpWire.otlpRootStatusFields = otlpRequestSpanStatusFields requestObservability}
          timedChildSpans
  Otlp.sendOtlpTraceRequest manager exporter requestBody

exportConnectionObservabilityToOtlp ::
  HttpClient.Manager ->
  Text ->
  Observability.OtlpExporter ->
  Observability.ConnectionObservability ->
  IO (Either Otlp.OtlpExportFailure ())
exportConnectionObservabilityToOtlp manager serviceName exporter connectionObservability = do
  (traceId, spanId) <- Otlp.nextOtlpSpanIdentifiers
  endTimeUnixNano <- Otlp.currentUnixTimeNSec
  let startTimeUnixNano = nonNegativeStartTime endTimeUnixNano connectionFallbackDurationNanoseconds
  let requestBody =
        OtlpWire.otlpTraceBodyFromSpan
          OtlpWire.OtlpTraceIdentity {OtlpWire.otlpTraceServiceName = serviceName, OtlpWire.otlpTraceId = traceId, OtlpWire.otlpTraceState = Nothing}
          OtlpWire.OtlpSpanIdentity {OtlpWire.otlpSpanId = spanId, OtlpWire.otlpSpanParentId = Nothing, OtlpWire.otlpSpanKindValue = OtlpWire.OtlpInternalSpan}
          OtlpWire.OtlpSpanTiming {OtlpWire.otlpSpanStartTimeUnixNano = startTimeUnixNano, OtlpWire.otlpSpanEndTimeUnixNano = endTimeUnixNano}
          OtlpWire.OtlpRootSpanContent
            { OtlpWire.otlpRootRequestSpan = Observability.observabilityConnectionSpan connectionObservability,
              OtlpWire.otlpRootStatusFields = OtlpWire.otlpErrorStatusFields
            }
          []
  Otlp.sendOtlpTraceRequest manager exporter requestBody

minimumOtlpSpanDurationNanoseconds :: Word64
minimumOtlpSpanDurationNanoseconds = 1000

requestFallbackDurationNanoseconds :: [(OtlpWire.OtlpSpanKind, Observability.RequestSpan)] -> Word64
requestFallbackDurationNanoseconds childSpans =
  minimumOtlpSpanDurationNanoseconds * fromIntegral (max 1 (length childSpans + 1))

connectionFallbackDurationNanoseconds :: Word64
connectionFallbackDurationNanoseconds = minimumOtlpSpanDurationNanoseconds

nonNegativeStartTime :: Word64 -> Word64 -> Word64
nonNegativeStartTime endTimeUnixNano durationNanos =
  endTimeUnixNano - min endTimeUnixNano durationNanos

timedOtlpChildSpan :: Word64 -> Word64 -> Text -> (OtlpWire.OtlpSpanKind, Observability.RequestSpan) -> OtlpWire.OtlpChildSpan
timedOtlpChildSpan rootStartTimeUnixNano rootDurationNanoseconds childSpanId (childSpanKind, childSpan) =
  OtlpWire.OtlpChildSpan
    { OtlpWire.otlpChildSpanId = childSpanId,
      OtlpWire.otlpChildSpanKind = childSpanKind,
      OtlpWire.otlpChildTiming =
        OtlpWire.OtlpSpanTiming
          { OtlpWire.otlpSpanStartTimeUnixNano = childStartTimeUnixNano,
            OtlpWire.otlpSpanEndTimeUnixNano = childStartTimeUnixNano + childDurationNanoseconds
          },
      OtlpWire.otlpChildRequestSpan = childSpan
    }
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

requestRuntimePhaseChildSpans :: Observability.RequestObservability -> [(OtlpWire.OtlpSpanKind, Observability.RequestSpan)]
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
            ( OtlpWire.OtlpInternalSpan,
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

requestDatabaseChildSpans :: Observability.RequestObservability -> [(OtlpWire.OtlpSpanKind, Observability.RequestSpan)]
requestDatabaseChildSpans requestObservability =
  [ (OtlpWire.OtlpClientSpan, databaseOperationChildSpan requestStartMonotonicNanoseconds databaseOperation)
  | databaseOperation <- Observability.observabilityDatabaseOperations requestObservability
  ]
  where
    requestStartMonotonicNanoseconds =
      requestSpanIntAttribute
        "harch.request.start_monotonic_ns"
        (Observability.observabilityRequestSpan requestObservability)

databaseOperationChildSpan :: Maybe Word64 -> Database.DatabaseOperation -> Observability.RequestSpan
databaseOperationChildSpan requestStartMonotonicNanoseconds databaseOperation =
  Observability.RequestSpan
    { Observability.requestSpanDisplayName = "DB " <> Database.databaseOperationName databaseOperation,
      Observability.requestSpanAttributes =
        [ textObservabilityAttribute "db.system" (Database.databaseOperationSystem databaseOperation),
          textObservabilityAttribute "db.operation.name" (Database.databaseOperationName databaseOperation),
          textObservabilityAttribute "db.query.template" (Database.databaseQueryTemplate databaseOperation)
        ]
          <> databaseOperationTimingAttributes requestStartMonotonicNanoseconds databaseOperation
    }

databaseOperationTimingAttributes :: Maybe Word64 -> Database.DatabaseOperation -> [Observability.ObservabilityAttribute]
databaseOperationTimingAttributes requestStartMonotonicNanoseconds databaseOperation =
  case (requestStartMonotonicNanoseconds, Database.databaseOperationStartedAtNanoseconds databaseOperation, Database.databaseOperationEndedAtNanoseconds databaseOperation) of
    (Just requestStartedAt, Just operationStartedAt, Just operationEndedAt) ->
      [ intObservabilityAttribute
          "harch.span.start_offset_ns"
          (fromIntegral (operationStartedAt - min requestStartedAt operationStartedAt)),
        intObservabilityAttribute
          "harch.span.duration_ns"
          (fromIntegral (operationEndedAt - min operationStartedAt operationEndedAt))
      ]
    _ -> []

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
