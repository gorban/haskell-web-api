{-# LANGUAGE OverloadedStrings #-}

module HarchWeb.Observability
  ( ConnectionObservability (..),
    HttpServerMetrics (..),
    ObservabilityConfig (..),
    ObservabilityAttribute (..),
    ObservabilityAttributeValue (..),
    ObservabilityStartupPlan (..),
    OtlpExporter (..),
    OtlpExporterStartup (..),
    RequestTraceContext (..),
    RequestObservability (..),
    RequestSpan (..),
    ResponseKind (..),
    TelemetrySignal (..),
    buildConnectionObservability,
    buildRequestObservability,
    forceConnectionObservability,
    forceRequestObservability,
    planObservabilityStartup,
    requestObservabilityAttributes,
    requestSpanName,
    withRequestTraceContext,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text

data ObservabilityAttributeValue
  = TextAttribute Text
  | IntAttribute Int
  deriving (Eq, Show)

data ObservabilityAttribute = ObservabilityAttribute
  { attributeName :: Text,
    attributeValue :: ObservabilityAttributeValue
  }
  deriving (Eq, Show)

data ResponseKind
  = PageResponseKind
  | BodyResponseKind
  deriving (Eq, Show)

data OtlpExporter = OtlpExporter
  { otlpEndpoint :: Text,
    otlpHeaders :: [(Text, Text)]
  }
  deriving (Eq, Show)

data ObservabilityConfig = ObservabilityConfig
  { tracingExporter :: Maybe OtlpExporter,
    metricsExporter :: Maybe OtlpExporter
  }
  deriving (Eq, Show)

data TelemetrySignal
  = TracingSignal
  | MetricsSignal
  deriving (Eq, Show)

data OtlpExporterStartup = OtlpExporterStartup
  { startupSignal :: TelemetrySignal,
    startupEndpoint :: Text,
    startupHeaders :: [(Text, Text)]
  }
  deriving (Eq, Show)

newtype ObservabilityStartupPlan = ObservabilityStartupPlan
  { startupExporters :: [OtlpExporterStartup]
  }
  deriving (Eq, Show)

-- | Convert configured exporters into the stable startup actions that the
-- server runtime performs. This is pure so applications can validate and
-- describe observability setup without starting any exporters.
planObservabilityStartup :: ObservabilityConfig -> ObservabilityStartupPlan
planObservabilityStartup observabilityConfig =
  ObservabilityStartupPlan
    { startupExporters =
        maybe [] (pure . buildStartup TracingSignal) (tracingExporter observabilityConfig)
          ++ maybe [] (pure . buildStartup MetricsSignal) (metricsExporter observabilityConfig)
    }
  where
    buildStartup signal exporter =
      OtlpExporterStartup
        { startupSignal = signal,
          startupEndpoint = otlpEndpoint exporter,
          startupHeaders = otlpHeaders exporter
        }

data RequestSpan = RequestSpan
  { requestSpanDisplayName :: Text,
    requestSpanAttributes :: [ObservabilityAttribute]
  }
  deriving (Eq, Show)

data HttpServerMetrics = HttpServerMetrics
  { requestDurationMetricName :: Text,
    activeRequestsMetricName :: Text,
    httpServerMetricAttributes :: [ObservabilityAttribute]
  }
  deriving (Eq, Show)

data RequestTraceContext = RequestTraceContext
  { traceContextTraceId :: Text,
    traceContextParentSpanId :: Text,
    traceContextState :: Maybe Text
  }
  deriving (Eq, Show)

data RequestObservability = RequestObservability
  { observabilityRequestSpan :: RequestSpan,
    observabilityHttpServerMetrics :: HttpServerMetrics,
    observabilityTraceContext :: Maybe RequestTraceContext
  }
  deriving (Eq, Show)

newtype ConnectionObservability = ConnectionObservability
  { observabilityConnectionSpan :: RequestSpan
  }
  deriving (Eq, Show)

requestSpanName :: Text -> Text -> Text
requestSpanName method routePath =
  Text.concat [method, " ", requestSpanOperationName routePath]

requestObservabilityAttributes ::
  Text ->
  Text ->
  Text ->
  Text ->
  Int ->
  ResponseKind ->
  [ObservabilityAttribute] ->
  [ObservabilityAttribute]
requestObservabilityAttributes method scheme requestPath routePath statusCode responseKind extraAttributes =
  commonAttributes ++ extraAttributes
  where
    commonAttributes =
      [ ObservabilityAttribute
          { attributeName = "http.request.method",
            attributeValue = TextAttribute method
          },
        ObservabilityAttribute
          { attributeName = "url.scheme",
            attributeValue = TextAttribute scheme
          },
        ObservabilityAttribute
          { attributeName = "url.path",
            attributeValue = TextAttribute requestPath
          },
        ObservabilityAttribute
          { attributeName = "http.route",
            attributeValue = TextAttribute routePath
          },
        ObservabilityAttribute
          { attributeName = "http.response.status_code",
            attributeValue = IntAttribute statusCode
          },
        ObservabilityAttribute
          { attributeName = "harch.response.kind",
            attributeValue = TextAttribute (responseKindText responseKind)
          }
      ]

buildRequestObservability ::
  Text ->
  Text ->
  Text ->
  Text ->
  Int ->
  ResponseKind ->
  [ObservabilityAttribute] ->
  RequestObservability
buildRequestObservability method scheme requestPath routePath statusCode responseKind extraAttributes =
  let attributes = requestObservabilityAttributes method scheme requestPath routePath statusCode responseKind extraAttributes
   in RequestObservability
        { observabilityRequestSpan =
            RequestSpan
              { requestSpanDisplayName = requestSpanName method routePath,
                requestSpanAttributes = attributes
              },
          observabilityHttpServerMetrics =
            HttpServerMetrics
              { requestDurationMetricName = "http.server.request.duration",
                activeRequestsMetricName = "http.server.active_requests",
                httpServerMetricAttributes = attributes
              },
          observabilityTraceContext = Nothing
        }

withRequestTraceContext :: RequestTraceContext -> RequestObservability -> RequestObservability
withRequestTraceContext traceContext requestObservability =
  requestObservability
    { observabilityTraceContext = Just traceContext
    }

buildConnectionObservability :: Text -> [ObservabilityAttribute] -> ConnectionObservability
buildConnectionObservability displayName attributes =
  ConnectionObservability
    { observabilityConnectionSpan =
        RequestSpan
          { requestSpanDisplayName = displayName,
            requestSpanAttributes = attributes
          }
    }

forceRequestObservability :: RequestObservability -> ()
forceRequestObservability requestObservability =
  forceRequestSpan (observabilityRequestSpan requestObservability) `seq`
    forceHttpServerMetrics (observabilityHttpServerMetrics requestObservability) `seq`
      forceTraceContext (observabilityTraceContext requestObservability)

forceConnectionObservability :: ConnectionObservability -> ()
forceConnectionObservability =
  forceRequestSpan . observabilityConnectionSpan

forceTraceContext :: Maybe RequestTraceContext -> ()
forceTraceContext maybeTraceContext =
  case maybeTraceContext of
    Nothing -> ()
    Just traceContext ->
      Text.length (traceContextTraceId traceContext) `seq`
        Text.length (traceContextParentSpanId traceContext) `seq`
          maybe () (\traceState -> Text.length traceState `seq` ()) (traceContextState traceContext)

forceRequestSpan :: RequestSpan -> ()
forceRequestSpan requestSpan =
  Text.length (requestSpanDisplayName requestSpan) `seq`
    forceAttributes (requestSpanAttributes requestSpan)

forceHttpServerMetrics :: HttpServerMetrics -> ()
forceHttpServerMetrics httpServerMetrics =
  Text.length (requestDurationMetricName httpServerMetrics) `seq`
    Text.length (activeRequestsMetricName httpServerMetrics) `seq`
      forceAttributes (httpServerMetricAttributes httpServerMetrics)

forceAttributes :: [ObservabilityAttribute] -> ()
forceAttributes attributes =
  case attributes of
    [] -> ()
    attribute : remainingAttributes ->
      forceAttribute attribute `seq` forceAttributes remainingAttributes

forceAttribute :: ObservabilityAttribute -> ()
forceAttribute attribute =
  Text.length (attributeName attribute) `seq`
    forceAttributeValue (attributeValue attribute)

forceAttributeValue :: ObservabilityAttributeValue -> ()
forceAttributeValue attributeValue =
  case attributeValue of
    TextAttribute textValue -> Text.length textValue `seq` ()
    IntAttribute intValue -> intValue `seq` ()

responseKindText :: ResponseKind -> Text
responseKindText responseKind =
  case responseKind of
    PageResponseKind -> "page"
    BodyResponseKind -> "body"

requestSpanOperationName :: Text -> Text
requestSpanOperationName routePath =
  if isNotFoundRoutePath routePath
    then "not-found"
    else routePath

isNotFoundRoutePath :: Text -> Bool
isNotFoundRoutePath routePath =
  case filter (not . Text.null) (Text.splitOn "/" routePath) of
    [] -> False
    segments -> last segments == "404"
