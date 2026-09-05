{-# LANGUAGE OverloadedStrings #-}

-- | Private OTLP JSON wire representation.
--
-- Decision (CF, 2026-08-19): group this module's OTLP-specific recurring
-- argument clusters into named records rather than threading them
-- positionally, matching the same pattern applied to ACME's protocol
-- context in @HarchWeb.Acme.Protocol.Types@. 'OtlpTraceIdentity' is the
-- @(service name, trace id, trace state)@ that every span in one trace
-- shares. 'OtlpSpanIdentity' and 'OtlpSpanTiming' are each individual
-- span's identity within the trace tree and its time range, used for both
-- the root span and every child. 'OtlpRootSpanContent' and 'OtlpChildSpan'
-- separate what only the root carries (arbitrary status fields) from what
-- only a child carries (its own identity and timing, since children always
-- inherit the root's parent linkage and never carry status fields). This
-- took 'otlpTraceBodyFromSpan' from 11 positional parameters to 5, and the
-- private 'otlpSpanObject' helper from 9 to 5.
module HarchWeb.Observability.Otlp.Wire
  ( OtlpChildSpan (..),
    OtlpRootSpanContent (..),
    OtlpSpanIdentity (..),
    OtlpSpanKind (..),
    OtlpSpanTiming (..),
    OtlpTraceIdentity (..),
    otlpErrorStatusFields,
    otlpTraceBodyFromSpan,
  )
where

import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import HarchWeb.Acme.Json (jsonArrayBytes, jsonObjectBytes, jsonStringBytes)
import HarchWeb.Observability.Types qualified as Observability

-- | Closed OTLP protocol vocabulary. This is private to the wire adapter, so
-- framework sources cannot interchange arbitrary protocol strings.
data OtlpSpanKind
  = OtlpServerSpan
  | OtlpInternalSpan
  | OtlpClientSpan

otlpSpanKindText :: OtlpSpanKind -> Text
otlpSpanKindText spanKind =
  case spanKind of
    OtlpServerSpan -> "SPAN_KIND_SERVER"
    OtlpInternalSpan -> "SPAN_KIND_INTERNAL"
    OtlpClientSpan -> "SPAN_KIND_CLIENT"

-- | What every span in one trace shares: which service produced it, which
-- trace it belongs to, and the (optional) upstream trace state to
-- propagate.
data OtlpTraceIdentity = OtlpTraceIdentity
  { otlpTraceServiceName :: Text,
    otlpTraceId :: Text,
    otlpTraceState :: Maybe Text
  }

-- | One span's identity within its trace's span tree: its own id, its
-- parent's id (absent only for a trace's root span with no upstream
-- parent), and its OTLP kind.
data OtlpSpanIdentity = OtlpSpanIdentity
  { otlpSpanId :: Text,
    otlpSpanParentId :: Maybe Text,
    otlpSpanKindValue :: OtlpSpanKind
  }

-- | One span's time range.
data OtlpSpanTiming = OtlpSpanTiming
  { otlpSpanStartTimeUnixNano :: Word64,
    otlpSpanEndTimeUnixNano :: Word64
  }

-- | What only a trace's root span carries beyond identity and timing: the
-- application span it was projected from, and any OTLP status fields (for
-- example an error status). Children never carry status fields of their
-- own.
data OtlpRootSpanContent = OtlpRootSpanContent
  { otlpRootRequestSpan :: Observability.RequestSpan,
    otlpRootStatusFields :: [(Text, LazyByteString.ByteString)]
  }

-- | One child span. Its parent linkage is always the trace's root span, so
-- only what varies per child is exposed here.
data OtlpChildSpan = OtlpChildSpan
  { otlpChildSpanId :: Text,
    otlpChildSpanKind :: OtlpSpanKind,
    otlpChildTiming :: OtlpSpanTiming,
    otlpChildRequestSpan :: Observability.RequestSpan
  }

otlpTraceBodyFromSpan ::
  OtlpTraceIdentity ->
  OtlpSpanIdentity ->
  OtlpSpanTiming ->
  OtlpRootSpanContent ->
  [OtlpChildSpan] ->
  LazyByteString.ByteString
otlpTraceBodyFromSpan traceIdentity rootIdentity rootTiming rootContent childSpans =
  jsonObjectBytes
    [ ( "resourceSpans",
        jsonArrayBytes
          [ jsonObjectBytes
              [ ("resource", otlpResourceObject (otlpTraceServiceName traceIdentity)),
                ( "scopeSpans",
                  jsonArrayBytes
                    [ jsonObjectBytes
                        [ ( "scope",
                            jsonObjectBytes
                              [("name", jsonStringBytes "harch-web")]
                          ),
                          ( "spans",
                            jsonArrayBytes
                              ( otlpSpanObject traceIdentity rootIdentity rootTiming (otlpRootRequestSpan rootContent) (otlpRootStatusFields rootContent)
                                  : [ otlpSpanObject
                                        traceIdentity
                                        OtlpSpanIdentity
                                          { otlpSpanId = otlpChildSpanId childSpan,
                                            otlpSpanParentId = Just (otlpSpanId rootIdentity),
                                            otlpSpanKindValue = otlpChildSpanKind childSpan
                                          }
                                        (otlpChildTiming childSpan)
                                        (otlpChildRequestSpan childSpan)
                                        []
                                    | childSpan <- childSpans
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
  OtlpTraceIdentity ->
  OtlpSpanIdentity ->
  OtlpSpanTiming ->
  Observability.RequestSpan ->
  [(Text, LazyByteString.ByteString)] ->
  LazyByteString.ByteString
otlpSpanObject traceIdentity spanIdentity timing requestSpan statusFields =
  jsonObjectBytes
    ( [ ("traceId", jsonStringBytes (otlpTraceId traceIdentity)),
        ("spanId", jsonStringBytes (otlpSpanId spanIdentity)),
        ("name", jsonStringBytes (Observability.requestSpanDisplayName requestSpan)),
        ("kind", jsonStringBytes (otlpSpanKindText (otlpSpanKindValue spanIdentity))),
        ("startTimeUnixNano", jsonStringBytes (Text.pack (show (otlpSpanStartTimeUnixNano timing)))),
        ("endTimeUnixNano", jsonStringBytes (Text.pack (show (otlpSpanEndTimeUnixNano timing)))),
        ( "attributes",
          jsonArrayBytes
            ( map otlpAttribute $
                filter shouldExportOtlpAttribute (Observability.requestSpanAttributes requestSpan)
            )
        )
      ]
        ++ maybe [] (\parentSpanId -> [("parentSpanId", jsonStringBytes parentSpanId)]) (otlpSpanParentId spanIdentity)
        ++ maybe [] (\traceState -> [("traceState", jsonStringBytes traceState)]) (otlpTraceState traceIdentity)
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
