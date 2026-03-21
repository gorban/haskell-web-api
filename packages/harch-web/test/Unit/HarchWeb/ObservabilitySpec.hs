{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWeb.ObservabilitySpec (spec) where

import qualified HarchWeb.Observability as Observability
import Test.Hspec

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
                Observability.observabilityHttpServerMetrics = httpServerMetrics
              }
      Observability.attributeName pageKindAttribute `shouldBe` "harch.response.kind"
      Observability.attributeValue pageKindAttribute `shouldBe` Observability.TextAttribute "page"
      Observability.attributeName localeAttribute `shouldBe` "app.locale"
      Observability.attributeValue localeAttribute `shouldBe` Observability.TextAttribute "fr"
      Observability.attributeName statusAttribute `shouldBe` "http.response.status_code"
      Observability.attributeValue statusAttribute `shouldBe` Observability.IntAttribute 200
      Observability.requestSpanDisplayName requestSpan `shouldBe` "GET /second"
      Observability.requestSpanAttributes requestSpan `shouldBe` [pageKindAttribute, localeAttribute]
      Observability.requestDurationMetricName httpServerMetrics `shouldBe` "http.server.request.duration"
      Observability.activeRequestsMetricName httpServerMetrics `shouldBe` "http.server.active_requests"
      Observability.httpServerMetricAttributes httpServerMetrics `shouldBe` [statusAttribute]
      Observability.observabilityRequestSpan requestObservability `shouldBe` requestSpan
      Observability.observabilityHttpServerMetrics requestObservability `shouldBe` httpServerMetrics

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
                Observability.observabilityHttpServerMetrics = httpServerMetrics
              }
      Observability.TextAttribute "page" `shouldBe` Observability.TextAttribute "page"
      Observability.TextAttribute "page" `shouldNotBe` Observability.TextAttribute "body"
      Observability.IntAttribute 200 `shouldBe` Observability.IntAttribute 200
      Observability.IntAttribute 200 `shouldNotBe` Observability.IntAttribute 404
      Observability.PageResponseKind `shouldBe` Observability.PageResponseKind
      Observability.PageResponseKind `shouldNotBe` Observability.BodyResponseKind
      pageKindAttribute `shouldBe` pageKindAttribute
      pageKindAttribute `shouldNotBe` statusAttribute
      requestSpan `shouldBe` requestSpan
      requestSpan `shouldNotBe` requestSpan {Observability.requestSpanDisplayName = "GET /second"}
      httpServerMetrics `shouldBe` httpServerMetrics
      httpServerMetrics `shouldNotBe` httpServerMetrics {Observability.activeRequestsMetricName = "other.metric"}
      requestObservability `shouldBe` requestObservability
      requestObservability `shouldNotBe` requestObservability {Observability.observabilityRequestSpan = requestSpan {Observability.requestSpanDisplayName = "POST /"}}
      show (Observability.TextAttribute "page") `shouldBe` "TextAttribute \"page\""
      show [Observability.IntAttribute 200] `shouldBe` "[IntAttribute 200]"
      show Observability.PageResponseKind `shouldBe` "PageResponseKind"
      show [Observability.BodyResponseKind] `shouldBe` "[BodyResponseKind]"
      show pageKindAttribute `shouldBe` "ObservabilityAttribute {attributeName = \"harch.response.kind\", attributeValue = TextAttribute \"page\"}"
      show requestSpan `shouldBe` "RequestSpan {requestSpanDisplayName = \"GET /\", requestSpanAttributes = [ObservabilityAttribute {attributeName = \"harch.response.kind\", attributeValue = TextAttribute \"page\"}]}"
      show [requestSpan] `shouldBe` "[RequestSpan {requestSpanDisplayName = \"GET /\", requestSpanAttributes = [ObservabilityAttribute {attributeName = \"harch.response.kind\", attributeValue = TextAttribute \"page\"}]}]"
      show httpServerMetrics `shouldBe` "HttpServerMetrics {requestDurationMetricName = \"http.server.request.duration\", activeRequestsMetricName = \"http.server.active_requests\", httpServerMetricAttributes = [ObservabilityAttribute {attributeName = \"http.response.status_code\", attributeValue = IntAttribute 200}]}"
      show [httpServerMetrics] `shouldBe` "[HttpServerMetrics {requestDurationMetricName = \"http.server.request.duration\", activeRequestsMetricName = \"http.server.active_requests\", httpServerMetricAttributes = [ObservabilityAttribute {attributeName = \"http.response.status_code\", attributeValue = IntAttribute 200}]}]"
      show requestObservability `shouldBe` "RequestObservability {observabilityRequestSpan = RequestSpan {requestSpanDisplayName = \"GET /\", requestSpanAttributes = [ObservabilityAttribute {attributeName = \"harch.response.kind\", attributeValue = TextAttribute \"page\"}]}, observabilityHttpServerMetrics = HttpServerMetrics {requestDurationMetricName = \"http.server.request.duration\", activeRequestsMetricName = \"http.server.active_requests\", httpServerMetricAttributes = [ObservabilityAttribute {attributeName = \"http.response.status_code\", attributeValue = IntAttribute 200}]}}"
      show [requestObservability] `shouldBe` "[RequestObservability {observabilityRequestSpan = RequestSpan {requestSpanDisplayName = \"GET /\", requestSpanAttributes = [ObservabilityAttribute {attributeName = \"harch.response.kind\", attributeValue = TextAttribute \"page\"}]}, observabilityHttpServerMetrics = HttpServerMetrics {requestDurationMetricName = \"http.server.request.duration\", activeRequestsMetricName = \"http.server.active_requests\", httpServerMetricAttributes = [ObservabilityAttribute {attributeName = \"http.response.status_code\", attributeValue = IntAttribute 200}]}}]"

  describe "requestSpanName" $
    it "uses the request method with the canonical route path" $
      Observability.requestSpanName "GET" "/fr/second" `shouldBe` "GET /fr/second"

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
                }
          }
