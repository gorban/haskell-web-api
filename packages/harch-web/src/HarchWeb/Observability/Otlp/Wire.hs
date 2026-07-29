{-# LANGUAGE OverloadedStrings #-}

-- | Private OTLP JSON wire representation.
module HarchWeb.Observability.Otlp.Wire
  ( otlpErrorStatusFields,
    otlpTraceBodyFromSpan,
  )
where

import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import HarchWeb.Acme.Json (jsonArrayBytes, jsonObjectBytes, jsonStringBytes)
import HarchWeb.Observability qualified as Observability

otlpTraceBodyFromSpan ::
  Text ->
  Text ->
  Text ->
  Maybe Text ->
  Maybe Text ->
  Word64 ->
  Word64 ->
  Observability.RequestSpan ->
  Text ->
  [(Text, LazyByteString.ByteString)] ->
  [(Text, Text, Word64, Word64, Observability.RequestSpan)] ->
  LazyByteString.ByteString
otlpTraceBodyFromSpan serviceName traceId spanId maybeParentSpanId maybeTraceState startTimeUnixNano endTimeUnixNano requestSpan rootSpanKind statusFields childSpans =
  jsonObjectBytes
    [ ( "resourceSpans",
        jsonArrayBytes
          [ jsonObjectBytes
              [ ("resource", otlpResourceObject serviceName),
                ( "scopeSpans",
                  jsonArrayBytes
                    [ jsonObjectBytes
                        [ ( "scope",
                            jsonObjectBytes
                              [("name", jsonStringBytes "harch-web")]
                          ),
                          ( "spans",
                            jsonArrayBytes
                              ( otlpSpanObject
                                  traceId
                                  spanId
                                  maybeParentSpanId
                                  maybeTraceState
                                  rootSpanKind
                                  startTimeUnixNano
                                  endTimeUnixNano
                                  requestSpan
                                  statusFields
                                  : [ otlpSpanObject
                                        traceId
                                        childSpanId
                                        (Just spanId)
                                        maybeTraceState
                                        childSpanKind
                                        childStartTimeUnixNano
                                        childEndTimeUnixNano
                                        childSpan
                                        []
                                    | (childSpanId, childSpanKind, childStartTimeUnixNano, childEndTimeUnixNano, childSpan) <- childSpans
                                    ]
                              )
                          )
                        ]
                    ]
                )
              ]
          ]
      )
    ]

otlpResourceObject :: Text -> LazyByteString.ByteString
otlpResourceObject serviceName =
  jsonObjectBytes
    [ ( "attributes",
        jsonArrayBytes
          [ otlpAttribute
              Observability.ObservabilityAttribute
                { Observability.attributeName = "service.name",
                  Observability.attributeValue = Observability.TextAttribute serviceName
                },
            otlpAttribute
              Observability.ObservabilityAttribute
                { Observability.attributeName = "telemetry.sdk.language",
                  Observability.attributeValue = Observability.TextAttribute "haskell"
                },
            otlpAttribute
              Observability.ObservabilityAttribute
                { Observability.attributeName = "telemetry.sdk.name",
                  Observability.attributeValue = Observability.TextAttribute "harch-web"
                }
          ]
      )
    ]

otlpSpanObject ::
  Text ->
  Text ->
  Maybe Text ->
  Maybe Text ->
  Text ->
  Word64 ->
  Word64 ->
  Observability.RequestSpan ->
  [(Text, LazyByteString.ByteString)] ->
  LazyByteString.ByteString
otlpSpanObject traceId spanId maybeParentSpanId maybeTraceState spanKind startTimeUnixNano endTimeUnixNano requestSpan statusFields =
  jsonObjectBytes
    ( [ ("traceId", jsonStringBytes traceId),
        ("spanId", jsonStringBytes spanId),
        ("name", jsonStringBytes (Observability.requestSpanDisplayName requestSpan)),
        ("kind", jsonStringBytes spanKind),
        ("startTimeUnixNano", jsonStringBytes (Text.pack (show startTimeUnixNano))),
        ("endTimeUnixNano", jsonStringBytes (Text.pack (show endTimeUnixNano))),
        ( "attributes",
          jsonArrayBytes
            ( map otlpAttribute $
                filter shouldExportOtlpAttribute (Observability.requestSpanAttributes requestSpan)
            )
        )
      ]
        ++ maybe [] (\parentSpanId -> [("parentSpanId", jsonStringBytes parentSpanId)]) maybeParentSpanId
        ++ maybe [] (\traceState -> [("traceState", jsonStringBytes traceState)]) maybeTraceState
        ++ statusFields
    )

otlpErrorStatusFields :: [(Text, LazyByteString.ByteString)]
otlpErrorStatusFields =
  [ ( "status",
      jsonObjectBytes
        [("code", jsonStringBytes "STATUS_CODE_ERROR")]
    )
  ]

otlpAttribute :: Observability.ObservabilityAttribute -> LazyByteString.ByteString
otlpAttribute attribute =
  jsonObjectBytes
    [ ("key", jsonStringBytes (Observability.attributeName attribute)),
      ("value", otlpAttributeValue (Observability.attributeValue attribute))
    ]

shouldExportOtlpAttribute :: Observability.ObservabilityAttribute -> Bool
shouldExportOtlpAttribute attribute =
  not (isInternalTimingAttributeName (Observability.attributeName attribute))

isInternalTimingAttributeName :: Text -> Bool
isInternalTimingAttributeName attributeName =
  attributeName
    `elem` [ "harch.request.start_monotonic_ns",
             "harch.request.duration_ns",
             "harch.span.start_offset_ns",
             "harch.span.duration_ns",
             "db.operation.start_monotonic_ns",
             "db.operation.duration_ns"
           ]
    || ("harch.phase." `Text.isPrefixOf` attributeName && ".start_offset_ns" `Text.isSuffixOf` attributeName)
    || ("harch.phase." `Text.isPrefixOf` attributeName && ".duration_ns" `Text.isSuffixOf` attributeName)

otlpAttributeValue :: Observability.ObservabilityAttributeValue -> LazyByteString.ByteString
otlpAttributeValue attributeValue =
  jsonObjectBytes
    [ case attributeValue of
        Observability.TextAttribute textValue ->
          ("stringValue", jsonStringBytes textValue)
        Observability.IntAttribute intValue ->
          ("intValue", jsonStringBytes (Text.pack (show intValue)))
    ]
