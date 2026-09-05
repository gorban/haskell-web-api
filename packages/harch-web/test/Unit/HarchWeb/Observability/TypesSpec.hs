{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Concurrent ()
import Control.Exception ()
import Control.Monad ()
import Data.ByteString qualified as ByteString ()
import Data.ByteString.Builder qualified as Builder ()
import Data.ByteString.Char8 qualified as ByteStringChar8 ()
import Data.ByteString.Lazy qualified as LazyByteString ()
import Data.Char ()
import Data.Either ()
import Data.Functor.Compose ()
import Data.IORef ()
import Data.List ()
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe ()
import Data.Text ()
import Data.Text qualified as Text ()
import Data.Text.Encoding qualified as TextEncoding ()
import HarchWeb (ObservabilityConfig (ObservabilityConfig, metricsExporter, tracingExporter), ObservabilityStartupPlan (ObservabilityStartupPlan, startupExporters), OtlpExporter (OtlpExporter, otlpEndpoint, otlpHeaders), OtlpExporterStartup (OtlpExporterStartup, startupEndpoint, startupHeaders, startupSignal), TelemetrySignal (MetricsSignal, TracingSignal), planObservabilityStartup)
import HarchWeb.Action qualified as Action ()
import HarchWeb.Database qualified as Database (DatabaseOperation (DatabaseOperation, databaseOperationEndedAtNanoseconds, databaseOperationName, databaseOperationStartedAtNanoseconds, databaseOperationSystem, databaseQueryTemplate))
import HarchWeb.Markup.Unsafe qualified as MarkupUnsafe ()
import HarchWeb.Observability qualified as Observability (ConnectionObservability (ConnectionObservability, observabilityConnectionSpan), HttpServerMetrics (HttpServerMetrics, activeRequestsMetricName, httpServerMetricAttributes, requestDurationMetricName), ObservabilityAttribute (ObservabilityAttribute, attributeName, attributeValue), ObservabilityAttributeValue (IntAttribute, TextAttribute), RequestIdentity (RequestIdentity, requestIdentityMethod, requestIdentityPath, requestIdentityRoutePath, requestIdentityScheme), RequestObservability (RequestObservability, observabilityDatabaseOperations, observabilityHttpServerMetrics, observabilityRequestSpan, observabilityTraceContext), RequestSpan (RequestSpan, requestSpanAttributes, requestSpanDisplayName), RequestTraceContext (RequestTraceContext, traceContextParentSpanId, traceContextState, traceContextTraceId), ResponseKind (BodyResponseKind, PageResponseKind), buildConnectionObservability, buildRequestObservability, forceRequestObservability, mkSpanMethodLabel, mkSpanRoutePath, requestObservabilityAttributes, requestSpanName, withDatabaseOperations, withRequestTraceContext)
import HarchWeb.Security qualified as Security ()
import Network.HTTP.Client qualified as HttpClient ()
import Network.HTTP.Types qualified as Http ()
import Network.Socket qualified as Socket ()
import Network.Socket.ByteString qualified as SocketByteString ()
import Network.Wai qualified as Wai ()
import Network.Wai.Handler.Warp qualified as Warp ()
import System.Directory ()
import System.Environment ()
import System.Exit ()
import System.FilePath ()
import System.IO ()
import System.IO.Error ()
import System.IO.Temp ()
import System.Posix.Signals ()
import System.Process ()
import TestCore.CustomAssertions ()
import TestCore.Wai ()
import Text.Read ()
import Unit.HarchWeb.TestSupport ()

existingSpec :: Spec
existingSpec = do
  describe "public record coverage" $ do
    it "reads exported selectors from the observability helper records" $ do
      let pageKindAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "harch.response.kind",
                Observability.attributeValue = Observability.TextAttribute "page"
              }
          localeAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "app.locale",
                Observability.attributeValue = Observability.TextAttribute "fr"
              }
          statusAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "http.response.status_code",
                Observability.attributeValue = Observability.IntAttribute 200
              }
          requestSpan =
            Observability.RequestSpan
              { Observability.requestSpanDisplayName = "GET /second",
                Observability.requestSpanAttributes = [pageKindAttribute, localeAttribute]
              }
          httpServerMetrics =
            Observability.HttpServerMetrics
              { Observability.requestDurationMetricName = "http.server.request.duration",
                Observability.activeRequestsMetricName = "http.server.active_requests",
                Observability.httpServerMetricAttributes = [statusAttribute]
              }
          requestObservability =
            Observability.RequestObservability
              { Observability.observabilityRequestSpan = requestSpan,
                Observability.observabilityHttpServerMetrics = httpServerMetrics,
                Observability.observabilityTraceContext = Nothing,
                Observability.observabilityDatabaseOperations = []
              }
          connectionObservability =
            Observability.ConnectionObservability
              { Observability.observabilityConnectionSpan = requestSpan
              }
          traceContextWithoutState =
            Observability.RequestTraceContext
              { Observability.traceContextTraceId = "4bf92f3577b34da6a3ce929d0e0e4736",
                Observability.traceContextParentSpanId = "00f067aa0ba902b7",
                Observability.traceContextState = Nothing
              }
          databaseOperation =
            Database.DatabaseOperation
              { Database.databaseOperationSystem = "postgresql",
                Database.databaseOperationName = "load-profile",
                Database.databaseQueryTemplate = "SELECT profile FROM account WHERE id = ?;",
                Database.databaseOperationStartedAtNanoseconds = Nothing,
                Database.databaseOperationEndedAtNanoseconds = Nothing
              }
          observabilityWithDatabaseOperation =
            Observability.withDatabaseOperations [databaseOperation] requestObservability
      expectAll
        ( (Observability.attributeName pageKindAttribute `shouldBe` "harch.response.kind")
            :| [ Observability.attributeValue pageKindAttribute `shouldBe` Observability.TextAttribute "page",
                 Observability.attributeName localeAttribute `shouldBe` "app.locale",
                 Observability.attributeValue localeAttribute `shouldBe` Observability.TextAttribute "fr",
                 Observability.attributeName statusAttribute `shouldBe` "http.response.status_code",
                 Observability.attributeValue statusAttribute `shouldBe` Observability.IntAttribute 200,
                 Observability.requestSpanDisplayName requestSpan `shouldBe` "GET /second",
                 Observability.requestSpanAttributes requestSpan `shouldBe` [pageKindAttribute, localeAttribute],
                 Observability.requestDurationMetricName httpServerMetrics `shouldBe` "http.server.request.duration",
                 Observability.activeRequestsMetricName httpServerMetrics `shouldBe` "http.server.active_requests",
                 Observability.httpServerMetricAttributes httpServerMetrics `shouldBe` [statusAttribute],
                 Observability.observabilityRequestSpan requestObservability `shouldBe` requestSpan,
                 Observability.observabilityHttpServerMetrics requestObservability `shouldBe` httpServerMetrics,
                 Observability.traceContextTraceId traceContextWithoutState `shouldBe` "4bf92f3577b34da6a3ce929d0e0e4736",
                 Observability.traceContextParentSpanId traceContextWithoutState `shouldBe` "00f067aa0ba902b7",
                 Observability.traceContextState traceContextWithoutState `shouldBe` Nothing,
                 Observability.observabilityConnectionSpan connectionObservability `shouldBe` requestSpan,
                 Observability.observabilityDatabaseOperations observabilityWithDatabaseOperation `shouldBe` [databaseOperation],
                 Observability.forceRequestObservability observabilityWithDatabaseOperation `shouldBe` ()
               ]
        )

    it "covers derived Eq and Show instances for the observability helper types" $ do
      let pageKindAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "harch.response.kind",
                Observability.attributeValue = Observability.TextAttribute "page"
              }
          statusAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "http.response.status_code",
                Observability.attributeValue = Observability.IntAttribute 200
              }
          requestSpan =
            Observability.RequestSpan
              { Observability.requestSpanDisplayName = "GET /",
                Observability.requestSpanAttributes = [pageKindAttribute]
              }
          httpServerMetrics =
            Observability.HttpServerMetrics
              { Observability.requestDurationMetricName = "http.server.request.duration",
                Observability.activeRequestsMetricName = "http.server.active_requests",
                Observability.httpServerMetricAttributes = [statusAttribute]
              }
          requestObservability =
            Observability.RequestObservability
              { Observability.observabilityRequestSpan = requestSpan,
                Observability.observabilityHttpServerMetrics = httpServerMetrics,
                Observability.observabilityTraceContext = Nothing,
                Observability.observabilityDatabaseOperations = []
              }
          traceContext =
            Observability.RequestTraceContext
              { Observability.traceContextTraceId = "4bf92f3577b34da6a3ce929d0e0e4736",
                Observability.traceContextParentSpanId = "00f067aa0ba902b7",
                Observability.traceContextState = Just "vendor=value"
              }
          connectionObservability =
            Observability.ConnectionObservability
              { Observability.observabilityConnectionSpan = requestSpan
              }
      expectAll
        ( (Observability.TextAttribute "page" `shouldBe` Observability.TextAttribute "page")
            :| [ Observability.TextAttribute "page" `shouldNotBe` Observability.TextAttribute "body",
                 Observability.IntAttribute 200 `shouldBe` Observability.IntAttribute 200,
                 Observability.IntAttribute 200 `shouldNotBe` Observability.IntAttribute 404,
                 Observability.PageResponseKind `shouldNotBe` Observability.BodyResponseKind,
                 pageKindAttribute `shouldNotBe` statusAttribute,
                 requestSpan `shouldNotBe` requestSpan {Observability.requestSpanDisplayName = "GET /second"},
                 httpServerMetrics `shouldNotBe` httpServerMetrics {Observability.activeRequestsMetricName = "other.metric"},
                 requestObservability `shouldNotBe` requestObservability {Observability.observabilityRequestSpan = requestSpan {Observability.requestSpanDisplayName = "POST /"}},
                 traceContext `shouldNotBe` traceContext {Observability.traceContextState = Nothing},
                 Observability.withRequestTraceContext traceContext requestObservability `shouldBe` requestObservability {Observability.observabilityTraceContext = Just traceContext},
                 connectionObservability `shouldNotBe` connectionObservability {Observability.observabilityConnectionSpan = requestSpan {Observability.requestSpanDisplayName = "POST /"}}
               ]
        )
      expectAll
        ( (show (Observability.TextAttribute "page") `shouldBe` "TextAttribute \"page\"")
            :| [ show [Observability.IntAttribute 200] `shouldBe` "[IntAttribute 200]",
                 show Observability.PageResponseKind `shouldBe` "PageResponseKind",
                 show [Observability.BodyResponseKind] `shouldBe` "[BodyResponseKind]",
                 show traceContext `shouldBe` "RequestTraceContext {traceContextTraceId = \"4bf92f3577b34da6a3ce929d0e0e4736\", traceContextParentSpanId = \"00f067aa0ba902b7\", traceContextState = Just \"vendor=value\"}",
                 show [traceContext {Observability.traceContextState = Nothing}] `shouldBe` "[RequestTraceContext {traceContextTraceId = \"4bf92f3577b34da6a3ce929d0e0e4736\", traceContextParentSpanId = \"00f067aa0ba902b7\", traceContextState = Nothing}]",
                 show pageKindAttribute `shouldBe` "ObservabilityAttribute {attributeName = \"harch.response.kind\", attributeValue = TextAttribute \"page\"}",
                 show requestSpan `shouldBe` "RequestSpan {requestSpanDisplayName = \"GET /\", requestSpanAttributes = [ObservabilityAttribute {attributeName = \"harch.response.kind\", attributeValue = TextAttribute \"page\"}]}",
                 show [requestSpan] `shouldBe` "[RequestSpan {requestSpanDisplayName = \"GET /\", requestSpanAttributes = [ObservabilityAttribute {attributeName = \"harch.response.kind\", attributeValue = TextAttribute \"page\"}]}]",
                 show httpServerMetrics `shouldBe` "HttpServerMetrics {requestDurationMetricName = \"http.server.request.duration\", activeRequestsMetricName = \"http.server.active_requests\", httpServerMetricAttributes = [ObservabilityAttribute {attributeName = \"http.response.status_code\", attributeValue = IntAttribute 200}]}",
                 show [httpServerMetrics] `shouldBe` "[HttpServerMetrics {requestDurationMetricName = \"http.server.request.duration\", activeRequestsMetricName = \"http.server.active_requests\", httpServerMetricAttributes = [ObservabilityAttribute {attributeName = \"http.response.status_code\", attributeValue = IntAttribute 200}]}]",
                 show requestObservability `shouldBe` "RequestObservability {observabilityRequestSpan = RequestSpan {requestSpanDisplayName = \"GET /\", requestSpanAttributes = [ObservabilityAttribute {attributeName = \"harch.response.kind\", attributeValue = TextAttribute \"page\"}]}, observabilityHttpServerMetrics = HttpServerMetrics {requestDurationMetricName = \"http.server.request.duration\", activeRequestsMetricName = \"http.server.active_requests\", httpServerMetricAttributes = [ObservabilityAttribute {attributeName = \"http.response.status_code\", attributeValue = IntAttribute 200}]}, observabilityTraceContext = Nothing, observabilityDatabaseOperations = []}",
                 show [requestObservability] `shouldBe` "[RequestObservability {observabilityRequestSpan = RequestSpan {requestSpanDisplayName = \"GET /\", requestSpanAttributes = [ObservabilityAttribute {attributeName = \"harch.response.kind\", attributeValue = TextAttribute \"page\"}]}, observabilityHttpServerMetrics = HttpServerMetrics {requestDurationMetricName = \"http.server.request.duration\", activeRequestsMetricName = \"http.server.active_requests\", httpServerMetricAttributes = [ObservabilityAttribute {attributeName = \"http.response.status_code\", attributeValue = IntAttribute 200}]}, observabilityTraceContext = Nothing, observabilityDatabaseOperations = []}]",
                 show connectionObservability `shouldBe` "ConnectionObservability {observabilityConnectionSpan = RequestSpan {requestSpanDisplayName = \"GET /\", requestSpanAttributes = [ObservabilityAttribute {attributeName = \"harch.response.kind\", attributeValue = TextAttribute \"page\"}]}}",
                 show [connectionObservability] `shouldBe` "[ConnectionObservability {observabilityConnectionSpan = RequestSpan {requestSpanDisplayName = \"GET /\", requestSpanAttributes = [ObservabilityAttribute {attributeName = \"harch.response.kind\", attributeValue = TextAttribute \"page\"}]}}]"
               ]
        )

    it "forces optional trace context state when present or absent" $ do
      let requestObservability =
            Observability.buildRequestObservability
              Observability.RequestIdentity
                { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "GET",
                  Observability.requestIdentityScheme = "http",
                  Observability.requestIdentityPath = "/health",
                  Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/health"
                }
              200
              Observability.BodyResponseKind
              []
          traceContextWithState =
            Observability.RequestTraceContext
              { Observability.traceContextTraceId = "4bf92f3577b34da6a3ce929d0e0e4736",
                Observability.traceContextParentSpanId = "00f067aa0ba902b7",
                Observability.traceContextState = Just "vendor=value"
              }
          traceContextWithoutState =
            Observability.RequestTraceContext
              { Observability.traceContextTraceId = "4bf92f3577b34da6a3ce929d0e0e4736",
                Observability.traceContextParentSpanId = "00f067aa0ba902b7",
                Observability.traceContextState = Nothing
              }
      expectAll
        ( ( Observability.forceRequestObservability
              (Observability.withRequestTraceContext traceContextWithState requestObservability)
              `shouldBe` ()
          )
            :| [ Observability.forceRequestObservability
                   (Observability.withRequestTraceContext traceContextWithoutState requestObservability)
                   `shouldBe` ()
               ]
        )

  describe "requestSpanName" $ do
    it "uses the request method with the canonical route path" $
      Observability.requestSpanName (Observability.mkSpanMethodLabel "GET") (Observability.mkSpanRoutePath "/fr/second")
        `shouldBe` "GET /fr/second"

    it "uses a stable not-found operation name instead of a route-looking synthetic path" $
      Observability.requestSpanName (Observability.mkSpanMethodLabel "GET") (Observability.mkSpanRoutePath "/fr/404")
        `shouldBe` "GET not-found"

  describe "requestObservabilityAttributes" $
    it "builds stable common attributes for page responses and preserves extra attributes" $
      let extraAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "app.locale",
                Observability.attributeValue = Observability.TextAttribute "fr"
              }
       in Observability.requestObservabilityAttributes
            Observability.RequestIdentity
              { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "GET",
                Observability.requestIdentityScheme = "http",
                Observability.requestIdentityPath = "/fr/missing",
                Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/fr/404"
              }
            404
            Observability.PageResponseKind
            [extraAttribute]
            `shouldBe` [ Observability.ObservabilityAttribute
                           { Observability.attributeName = "http.request.method",
                             Observability.attributeValue = Observability.TextAttribute "GET"
                           },
                         Observability.ObservabilityAttribute
                           { Observability.attributeName = "url.scheme",
                             Observability.attributeValue = Observability.TextAttribute "http"
                           },
                         Observability.ObservabilityAttribute
                           { Observability.attributeName = "url.path",
                             Observability.attributeValue = Observability.TextAttribute "/fr/missing"
                           },
                         Observability.ObservabilityAttribute
                           { Observability.attributeName = "http.route",
                             Observability.attributeValue = Observability.TextAttribute "/fr/404"
                           },
                         Observability.ObservabilityAttribute
                           { Observability.attributeName = "http.response.status_code",
                             Observability.attributeValue = Observability.IntAttribute 404
                           },
                         Observability.ObservabilityAttribute
                           { Observability.attributeName = "harch.response.kind",
                             Observability.attributeValue = Observability.TextAttribute "page"
                           },
                         extraAttribute
                       ]

  describe "buildRequestObservability" $
    it "uses stable span and HTTP metric names for body responses" $
      Observability.buildRequestObservability
        Observability.RequestIdentity
          { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "POST",
            Observability.requestIdentityScheme = "https",
            Observability.requestIdentityPath = "/api/status",
            Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/api/status"
          }
        200
        Observability.BodyResponseKind
        [ Observability.ObservabilityAttribute
            { Observability.attributeName = "app.surface",
              Observability.attributeValue = Observability.TextAttribute "api"
            }
        ]
        `shouldBe` Observability.RequestObservability
          { Observability.observabilityRequestSpan =
              Observability.RequestSpan
                { Observability.requestSpanDisplayName = "POST /api/status",
                  Observability.requestSpanAttributes =
                    [ Observability.ObservabilityAttribute
                        { Observability.attributeName = "http.request.method",
                          Observability.attributeValue = Observability.TextAttribute "POST"
                        },
                      Observability.ObservabilityAttribute
                        { Observability.attributeName = "url.scheme",
                          Observability.attributeValue = Observability.TextAttribute "https"
                        },
                      Observability.ObservabilityAttribute
                        { Observability.attributeName = "url.path",
                          Observability.attributeValue = Observability.TextAttribute "/api/status"
                        },
                      Observability.ObservabilityAttribute
                        { Observability.attributeName = "http.route",
                          Observability.attributeValue = Observability.TextAttribute "/api/status"
                        },
                      Observability.ObservabilityAttribute
                        { Observability.attributeName = "http.response.status_code",
                          Observability.attributeValue = Observability.IntAttribute 200
                        },
                      Observability.ObservabilityAttribute
                        { Observability.attributeName = "harch.response.kind",
                          Observability.attributeValue = Observability.TextAttribute "body"
                        },
                      Observability.ObservabilityAttribute
                        { Observability.attributeName = "app.surface",
                          Observability.attributeValue = Observability.TextAttribute "api"
                        }
                    ]
                },
            Observability.observabilityHttpServerMetrics =
              Observability.HttpServerMetrics
                { Observability.requestDurationMetricName = "http.server.request.duration",
                  Observability.activeRequestsMetricName = "http.server.active_requests",
                  Observability.httpServerMetricAttributes =
                    [ Observability.ObservabilityAttribute
                        { Observability.attributeName = "http.request.method",
                          Observability.attributeValue = Observability.TextAttribute "POST"
                        },
                      Observability.ObservabilityAttribute
                        { Observability.attributeName = "url.scheme",
                          Observability.attributeValue = Observability.TextAttribute "https"
                        },
                      Observability.ObservabilityAttribute
                        { Observability.attributeName = "http.route",
                          Observability.attributeValue = Observability.TextAttribute "/api/status"
                        },
                      Observability.ObservabilityAttribute
                        { Observability.attributeName = "http.response.status_code",
                          Observability.attributeValue = Observability.IntAttribute 200
                        },
                      Observability.ObservabilityAttribute
                        { Observability.attributeName = "harch.response.kind",
                          Observability.attributeValue = Observability.TextAttribute "body"
                        }
                    ]
                },
            Observability.observabilityTraceContext = Nothing,
            Observability.observabilityDatabaseOperations = []
          }

  describe "buildConnectionObservability" $
    it "keeps the supplied display name and attributes without synthesizing request metrics" $
      Observability.buildConnectionObservability
        "CONNECTION insecure-connection-denied"
        [ Observability.ObservabilityAttribute
            { Observability.attributeName = "network.peer.address",
              Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
            }
        ]
        `shouldBe` Observability.ConnectionObservability
          { Observability.observabilityConnectionSpan =
              Observability.RequestSpan
                { Observability.requestSpanDisplayName = "CONNECTION insecure-connection-denied",
                  Observability.requestSpanAttributes =
                    [ Observability.ObservabilityAttribute
                        { Observability.attributeName = "network.peer.address",
                          Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
                        }
                    ]
                }
          }

  describe "buildRequestObservability" $
    it "groups unmatched request spans under a stable not-found operation name while retaining the concrete URL path attribute" $
      Observability.buildRequestObservability
        Observability.RequestIdentity
          { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "GET",
            Observability.requestIdentityScheme = "http",
            Observability.requestIdentityPath = "/favicon.ico",
            Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/404"
          }
        404
        Observability.PageResponseKind
        []
        `shouldBe` Observability.RequestObservability
          { Observability.observabilityRequestSpan =
              Observability.RequestSpan
                { Observability.requestSpanDisplayName = "GET not-found",
                  Observability.requestSpanAttributes =
                    [ Observability.ObservabilityAttribute
                        { Observability.attributeName = "http.request.method",
                          Observability.attributeValue = Observability.TextAttribute "GET"
                        },
                      Observability.ObservabilityAttribute
                        { Observability.attributeName = "url.scheme",
                          Observability.attributeValue = Observability.TextAttribute "http"
                        },
                      Observability.ObservabilityAttribute
                        { Observability.attributeName = "url.path",
                          Observability.attributeValue = Observability.TextAttribute "/favicon.ico"
                        },
                      Observability.ObservabilityAttribute
                        { Observability.attributeName = "http.route",
                          Observability.attributeValue = Observability.TextAttribute "/404"
                        },
                      Observability.ObservabilityAttribute
                        { Observability.attributeName = "http.response.status_code",
                          Observability.attributeValue = Observability.IntAttribute 404
                        },
                      Observability.ObservabilityAttribute
                        { Observability.attributeName = "harch.response.kind",
                          Observability.attributeValue = Observability.TextAttribute "page"
                        }
                    ]
                },
            Observability.observabilityHttpServerMetrics =
              Observability.HttpServerMetrics
                { Observability.requestDurationMetricName = "http.server.request.duration",
                  Observability.activeRequestsMetricName = "http.server.active_requests",
                  Observability.httpServerMetricAttributes =
                    [ Observability.ObservabilityAttribute
                        { Observability.attributeName = "http.request.method",
                          Observability.attributeValue = Observability.TextAttribute "GET"
                        },
                      Observability.ObservabilityAttribute
                        { Observability.attributeName = "url.scheme",
                          Observability.attributeValue = Observability.TextAttribute "http"
                        },
                      Observability.ObservabilityAttribute
                        { Observability.attributeName = "http.route",
                          Observability.attributeValue = Observability.TextAttribute "/404"
                        },
                      Observability.ObservabilityAttribute
                        { Observability.attributeName = "http.response.status_code",
                          Observability.attributeValue = Observability.IntAttribute 404
                        },
                      Observability.ObservabilityAttribute
                        { Observability.attributeName = "harch.response.kind",
                          Observability.attributeValue = Observability.TextAttribute "page"
                        }
                    ]
                },
            Observability.observabilityTraceContext = Nothing,
            Observability.observabilityDatabaseOperations = []
          }

movedSpec :: Spec
movedSpec = do
  describe "planObservabilityStartup" $ do
    it "produces no exporter startup actions when tracing and metrics are disabled" $
      planObservabilityStartup ObservabilityConfig {tracingExporter = Nothing, metricsExporter = Nothing}
        `shouldBe` ObservabilityStartupPlan {startupExporters = []}

    it "translates OTLP tracing and metrics exporters into deterministic startup parameters" $
      planObservabilityStartup
        ObservabilityConfig
          { tracingExporter =
              Just
                OtlpExporter
                  { otlpEndpoint = "http://collector:4318/v1/traces",
                    otlpHeaders = [("authorization", "Bearer tracing")]
                  },
            metricsExporter =
              Just
                OtlpExporter
                  { otlpEndpoint = "http://collector:4318/v1/metrics",
                    otlpHeaders = [("x-scope", "metrics")]
                  }
          }
        `shouldBe` ObservabilityStartupPlan
          { startupExporters =
              [ OtlpExporterStartup
                  { startupSignal = TracingSignal,
                    startupEndpoint = "http://collector:4318/v1/traces",
                    startupHeaders = [("authorization", "Bearer tracing")]
                  },
                OtlpExporterStartup
                  { startupSignal = MetricsSignal,
                    startupEndpoint = "http://collector:4318/v1/metrics",
                    startupHeaders = [("x-scope", "metrics")]
                  }
              ]
          }

    it "redacts exporter credentials through configuration and startup-plan diagnostics" $ do
      let secretHeader = "otlp-header-sentinel" :: String
          exporter =
            OtlpExporter
              { otlpEndpoint = "https://collector.example/v1/traces",
                otlpHeaders = [("authorization", "otlp-header-sentinel")]
              }
          observabilityConfig = ObservabilityConfig {tracingExporter = Just exporter, metricsExporter = Nothing}
          startupPlan = planObservabilityStartup observabilityConfig
      expectAll
        ( (show exporter `shouldNotContain` secretHeader)
            :| [ show exporter `shouldNotContain` "collector.example",
                 show observabilityConfig `shouldNotContain` secretHeader,
                 show startupPlan `shouldNotContain` secretHeader,
                 show startupPlan `shouldContain` "startupHeaders = <redacted: 1>"
               ]
        )

spec = do
  existingSpec
  movedSpec
