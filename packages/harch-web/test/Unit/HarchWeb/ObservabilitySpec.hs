{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWeb.ObservabilitySpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import HarchWeb.Observability qualified as Observability
import Test.Hspec
import TestCore.CustomAssertions (expectAll)

spec :: Spec
spec = do
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
                Observability.observabilityTraceContext = Nothing
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
                 Observability.observabilityConnectionSpan connectionObservability `shouldBe` requestSpan
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
                Observability.observabilityTraceContext = Nothing
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
                 Observability.PageResponseKind `shouldBe` Observability.PageResponseKind,
                 Observability.PageResponseKind `shouldNotBe` Observability.BodyResponseKind,
                 pageKindAttribute `shouldBe` pageKindAttribute,
                 pageKindAttribute `shouldNotBe` statusAttribute,
                 requestSpan `shouldBe` requestSpan,
                 requestSpan `shouldNotBe` requestSpan {Observability.requestSpanDisplayName = "GET /second"},
                 httpServerMetrics `shouldBe` httpServerMetrics,
                 httpServerMetrics `shouldNotBe` httpServerMetrics {Observability.activeRequestsMetricName = "other.metric"},
                 requestObservability `shouldBe` requestObservability,
                 requestObservability `shouldNotBe` requestObservability {Observability.observabilityRequestSpan = requestSpan {Observability.requestSpanDisplayName = "POST /"}},
                 traceContext `shouldBe` traceContext,
                 traceContext `shouldNotBe` traceContext {Observability.traceContextState = Nothing},
                 Observability.withRequestTraceContext traceContext requestObservability `shouldBe` requestObservability {Observability.observabilityTraceContext = Just traceContext},
                 connectionObservability `shouldBe` connectionObservability,
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
                 show requestObservability `shouldBe` "RequestObservability {observabilityRequestSpan = RequestSpan {requestSpanDisplayName = \"GET /\", requestSpanAttributes = [ObservabilityAttribute {attributeName = \"harch.response.kind\", attributeValue = TextAttribute \"page\"}]}, observabilityHttpServerMetrics = HttpServerMetrics {requestDurationMetricName = \"http.server.request.duration\", activeRequestsMetricName = \"http.server.active_requests\", httpServerMetricAttributes = [ObservabilityAttribute {attributeName = \"http.response.status_code\", attributeValue = IntAttribute 200}]}, observabilityTraceContext = Nothing}",
                 show [requestObservability] `shouldBe` "[RequestObservability {observabilityRequestSpan = RequestSpan {requestSpanDisplayName = \"GET /\", requestSpanAttributes = [ObservabilityAttribute {attributeName = \"harch.response.kind\", attributeValue = TextAttribute \"page\"}]}, observabilityHttpServerMetrics = HttpServerMetrics {requestDurationMetricName = \"http.server.request.duration\", activeRequestsMetricName = \"http.server.active_requests\", httpServerMetricAttributes = [ObservabilityAttribute {attributeName = \"http.response.status_code\", attributeValue = IntAttribute 200}]}, observabilityTraceContext = Nothing}]",
                 show connectionObservability `shouldBe` "ConnectionObservability {observabilityConnectionSpan = RequestSpan {requestSpanDisplayName = \"GET /\", requestSpanAttributes = [ObservabilityAttribute {attributeName = \"harch.response.kind\", attributeValue = TextAttribute \"page\"}]}}",
                 show [connectionObservability] `shouldBe` "[ConnectionObservability {observabilityConnectionSpan = RequestSpan {requestSpanDisplayName = \"GET /\", requestSpanAttributes = [ObservabilityAttribute {attributeName = \"harch.response.kind\", attributeValue = TextAttribute \"page\"}]}}]"
               ]
        )

    it "forces optional trace context state when present or absent" $ do
      let requestObservability =
            Observability.buildRequestObservability
              "GET"
              "http"
              "/health"
              "/health"
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
      Observability.requestSpanName "GET" "/fr/second" `shouldBe` "GET /fr/second"

    it "uses a stable not-found operation name instead of a route-looking synthetic path" $
      Observability.requestSpanName "GET" "/fr/404" `shouldBe` "GET not-found"

  describe "requestObservabilityAttributes" $
    it "builds stable common attributes for page responses and preserves extra attributes" $
      let extraAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "app.locale",
                Observability.attributeValue = Observability.TextAttribute "fr"
              }
       in Observability.requestObservabilityAttributes
            "GET"
            "http"
            "/fr/missing"
            "/fr/404"
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
        "POST"
        "https"
        "/api/status"
        "/api/status"
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
            Observability.observabilityTraceContext = Nothing
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
        "GET"
        "http"
        "/favicon.ico"
        "/404"
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
            Observability.observabilityTraceContext = Nothing
          }
