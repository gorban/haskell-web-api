{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Concurrent (readMVar)
import Control.Exception (try)
import Control.Monad ()
import Data.ByteString qualified as ByteString ()
import Data.ByteString.Builder qualified as Builder ()
import Data.ByteString.Char8 qualified as ByteStringChar8 ()
import Data.ByteString.Lazy qualified as LazyByteString (toStrict)
import Data.Char (isHexDigit)
import Data.Either ()
import Data.Functor.Compose ()
import Data.IORef ()
import Data.List ()
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe ()
import Data.Text ()
import Data.Text qualified as Text (all, count, isInfixOf, length)
import Data.Text.Encoding qualified as TextEncoding (decodeUtf8)
import HarchWeb (OtlpExporter (OtlpExporter, otlpEndpoint, otlpHeaders), exportConnectionObservabilityToOtlp, exportRequestObservabilityToOtlp)
import HarchWeb.Action qualified as Action ()
import HarchWeb.Database qualified as Database (DatabaseOperation (DatabaseOperation))
import HarchWeb.Markup.Unsafe qualified as MarkupUnsafe ()
import HarchWeb.Observability qualified as Observability (ObservabilityAttribute (ObservabilityAttribute, attributeName, attributeValue), ObservabilityAttributeValue (IntAttribute, TextAttribute), RequestIdentity (RequestIdentity, requestIdentityMethod, requestIdentityPath, requestIdentityRoutePath, requestIdentityScheme), RequestTraceContext (RequestTraceContext, traceContextParentSpanId, traceContextState, traceContextTraceId), ResponseKind (BodyResponseKind, PageResponseKind), buildConnectionObservability, buildRequestObservability, mkSpanMethodLabel, mkSpanRoutePath, withDatabaseOperations, withRequestTraceContext)
import HarchWeb.Security qualified as Security ()
import Network.HTTP.Client qualified as HttpClient ()
import Network.HTTP.Types qualified as Http (hContentType, ok200, serviceUnavailable503)
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
import Unit.HarchWeb.TestSupport (CapturedCollectorRequest (CapturedCollectorRequest, capturedCollectorBody, capturedCollectorHeaders, capturedCollectorMethod, capturedCollectorPath), expectPlausibleEpochNanoTimestamps, extractQuotedJsonField, extractQuotedJsonIntegerFields, withOtlpCollector)

spec = do
  describe "exportRequestObservabilityToOtlp" $ do
    it "posts OTLP trace payloads with request attributes, resource attributes, and custom headers" $
      withOtlpCollector Http.ok200 "{}" $ \manager collectorUrl capturedRequestReference -> do
        exportRequestObservabilityToOtlp
          manager
          "sample-app"
          OtlpExporter
            { otlpEndpoint = collectorUrl,
              otlpHeaders = [("authorization", "Bearer sample-token")]
            }
          ( Observability.withDatabaseOperations
              [ Database.DatabaseOperation "postgresql" "load-second-page-summary" "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;" (Just 3000000) (Just 4250000),
                Database.DatabaseOperation "postgresql" "load-home-page-summary" "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;" Nothing Nothing,
                Database.DatabaseOperation "postgresql" "load-health-check" "SELECT 1;" Nothing Nothing
              ]
              ( Observability.buildRequestObservability
                  Observability.RequestIdentity
                    { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "GET",
                      Observability.requestIdentityScheme = "https",
                      Observability.requestIdentityPath = "/known",
                      Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/known"
                    }
                  503
                  Observability.PageResponseKind
                  [ Observability.ObservabilityAttribute
                      { Observability.attributeName = "exception.type",
                        Observability.attributeValue = Observability.TextAttribute "ExampleFailure"
                      },
                    Observability.ObservabilityAttribute
                      { Observability.attributeName = "db.system",
                        Observability.attributeValue = Observability.TextAttribute "postgresql"
                      },
                    Observability.ObservabilityAttribute
                      { Observability.attributeName = "db.operation.name",
                        Observability.attributeValue = Observability.TextAttribute "load-second-page-summary"
                      },
                    Observability.ObservabilityAttribute
                      { Observability.attributeName = "db.query.template",
                        Observability.attributeValue = Observability.TextAttribute "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;"
                      },
                    Observability.ObservabilityAttribute
                      { Observability.attributeName = "db.operation.start_monotonic_ns",
                        Observability.attributeValue = Observability.IntAttribute 3000000
                      },
                    Observability.ObservabilityAttribute
                      { Observability.attributeName = "db.operation.duration_ns",
                        Observability.attributeValue = Observability.IntAttribute 1250000
                      },
                    Observability.ObservabilityAttribute
                      { Observability.attributeName = "db.system",
                        Observability.attributeValue = Observability.TextAttribute "postgresql"
                      },
                    Observability.ObservabilityAttribute
                      { Observability.attributeName = "db.operation.name",
                        Observability.attributeValue = Observability.TextAttribute "load-home-page-summary"
                      },
                    Observability.ObservabilityAttribute
                      { Observability.attributeName = "db.query.template",
                        Observability.attributeValue = Observability.TextAttribute "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;"
                      },
                    Observability.ObservabilityAttribute
                      { Observability.attributeName = "db.operation.start_monotonic_ns",
                        Observability.attributeValue = Observability.IntAttribute (-1)
                      },
                    Observability.ObservabilityAttribute
                      { Observability.attributeName = "db.operation.duration_ns",
                        Observability.attributeValue = Observability.IntAttribute (-1)
                      },
                    Observability.ObservabilityAttribute
                      { Observability.attributeName = "db.system",
                        Observability.attributeValue = Observability.TextAttribute "postgresql"
                      },
                    Observability.ObservabilityAttribute
                      { Observability.attributeName = "db.operation.name",
                        Observability.attributeValue = Observability.TextAttribute "load-health-check"
                      },
                    Observability.ObservabilityAttribute
                      { Observability.attributeName = "db.query.template",
                        Observability.attributeValue = Observability.TextAttribute "SELECT 1;"
                      },
                    Observability.ObservabilityAttribute
                      { Observability.attributeName = "harch.request.start_monotonic_ns",
                        Observability.attributeValue = Observability.IntAttribute 1000000
                      },
                    Observability.ObservabilityAttribute
                      { Observability.attributeName = "harch.request.duration_ns",
                        Observability.attributeValue = Observability.IntAttribute 5000000
                      },
                    Observability.ObservabilityAttribute
                      { Observability.attributeName = "harch.phase.request-policy.start_offset_ns",
                        Observability.attributeValue = Observability.IntAttribute 0
                      },
                    Observability.ObservabilityAttribute
                      { Observability.attributeName = "harch.phase.request-policy.duration_ns",
                        Observability.attributeValue = Observability.IntAttribute 250000
                      },
                    Observability.ObservabilityAttribute
                      { Observability.attributeName = "harch.phase.route-match.start_offset_ns",
                        Observability.attributeValue = Observability.IntAttribute 500000
                      },
                    Observability.ObservabilityAttribute
                      { Observability.attributeName = "harch.phase.route-match.duration_ns",
                        Observability.attributeValue = Observability.IntAttribute 750000
                      },
                    Observability.ObservabilityAttribute
                      { Observability.attributeName = "harch.phase.render-response.start_offset_ns",
                        Observability.attributeValue = Observability.IntAttribute 1500000
                      },
                    Observability.ObservabilityAttribute
                      { Observability.attributeName = "harch.phase.render-response.duration_ns",
                        Observability.attributeValue = Observability.IntAttribute 3000000
                      }
                  ]
              )
          )
        CapturedCollectorRequest
          { capturedCollectorMethod = requestMethod,
            capturedCollectorPath = requestPath,
            capturedCollectorHeaders = requestHeaders,
            capturedCollectorBody = requestBody
          } <-
          readMVar capturedRequestReference
        let requestBodyText = TextEncoding.decodeUtf8 (LazyByteString.toStrict requestBody)
        expectAll
          ( (requestMethod `shouldBe` "POST")
              :| [ requestPath `shouldBe` "/v1/traces",
                   lookup Http.hContentType requestHeaders `shouldBe` Just "application/json",
                   lookup "authorization" requestHeaders `shouldBe` Just "Bearer sample-token",
                   requestBodyText `shouldSatisfy` Text.isInfixOf "\"service.name\"",
                   requestBodyText `shouldSatisfy` Text.isInfixOf "\"sample-app\"",
                   requestBodyText `shouldSatisfy` Text.isInfixOf "\"telemetry.sdk.language\"",
                   requestBodyText `shouldSatisfy` Text.isInfixOf "\"name\":\"GET /known\"",
                   requestBodyText `shouldSatisfy` Text.isInfixOf "\"kind\":\"SPAN_KIND_SERVER\"",
                   requestBodyText `shouldSatisfy` Text.isInfixOf "\"kind\":\"SPAN_KIND_INTERNAL\"",
                   requestBodyText `shouldSatisfy` Text.isInfixOf "\"kind\":\"SPAN_KIND_CLIENT\"",
                   requestBodyText `shouldSatisfy` Text.isInfixOf "\"parentSpanId\"",
                   requestBodyText `shouldSatisfy` Text.isInfixOf "\"name\":\"HarchWeb request policy\"",
                   requestBodyText `shouldSatisfy` Text.isInfixOf "\"name\":\"HarchWeb route match\"",
                   requestBodyText `shouldSatisfy` Text.isInfixOf "\"name\":\"HarchWeb render response\"",
                   requestBodyText `shouldSatisfy` Text.isInfixOf "\"name\":\"DB load-second-page-summary\"",
                   requestBodyText `shouldSatisfy` Text.isInfixOf "\"name\":\"DB load-home-page-summary\"",
                   requestBodyText `shouldSatisfy` Text.isInfixOf "\"name\":\"DB load-health-check\"",
                   requestBodyText `shouldSatisfy` Text.isInfixOf "\"harch.span.phase\"",
                   requestBodyText `shouldSatisfy` Text.isInfixOf "\"request-policy\"",
                   requestBodyText `shouldSatisfy` Text.isInfixOf "\"route-match\"",
                   requestBodyText `shouldSatisfy` Text.isInfixOf "\"render-response\"",
                   requestBodyText `shouldSatisfy` Text.isInfixOf "\"exception.type\"",
                   requestBodyText `shouldSatisfy` Text.isInfixOf "\"db.operation.name\"",
                   requestBodyText `shouldSatisfy` Text.isInfixOf "\"db.query.template\"",
                   requestBodyText `shouldSatisfy` Text.isInfixOf "\"STATUS_CODE_ERROR\"",
                   requestBodyText `shouldNotSatisfy` Text.isInfixOf "\"harch.request.start_monotonic_ns\"",
                   requestBodyText `shouldNotSatisfy` Text.isInfixOf "\"harch.request.duration_ns\"",
                   requestBodyText `shouldNotSatisfy` Text.isInfixOf "\"harch.phase.request-policy.start_offset_ns\"",
                   requestBodyText `shouldNotSatisfy` Text.isInfixOf "\"harch.phase.request-policy.duration_ns\"",
                   requestBodyText `shouldNotSatisfy` Text.isInfixOf "\"harch.phase.route-match.start_offset_ns\"",
                   requestBodyText `shouldNotSatisfy` Text.isInfixOf "\"harch.phase.route-match.duration_ns\"",
                   requestBodyText `shouldNotSatisfy` Text.isInfixOf "\"harch.phase.render-response.start_offset_ns\"",
                   requestBodyText `shouldNotSatisfy` Text.isInfixOf "\"harch.phase.render-response.duration_ns\"",
                   requestBodyText `shouldNotSatisfy` Text.isInfixOf "\"harch.span.start_offset_ns\"",
                   requestBodyText `shouldNotSatisfy` Text.isInfixOf "\"harch.span.duration_ns\"",
                   requestBodyText `shouldNotSatisfy` Text.isInfixOf "\"db.operation.start_monotonic_ns\"",
                   requestBodyText `shouldNotSatisfy` Text.isInfixOf "\"db.operation.duration_ns\"",
                   Text.count "\"name\":\"GET /known\"" requestBodyText `shouldBe` 1,
                   Text.count "\"name\":\"HarchWeb request policy\"" requestBodyText `shouldBe` 1,
                   Text.count "\"name\":\"HarchWeb route match\"" requestBodyText `shouldBe` 1,
                   Text.count "\"name\":\"HarchWeb render response\"" requestBodyText `shouldBe` 1,
                   Text.count "\"name\":\"DB load-second-page-summary\"" requestBodyText `shouldBe` 1,
                   Text.count "\"name\":\"DB load-home-page-summary\"" requestBodyText `shouldBe` 1,
                   Text.count "\"name\":\"DB load-health-check\"" requestBodyText `shouldBe` 1,
                   Text.count "\"key\":\"db.system\"" requestBodyText `shouldBe` 6,
                   Text.count "\"key\":\"db.operation.name\"" requestBodyText `shouldBe` 6,
                   Text.count "\"key\":\"db.query.template\"" requestBodyText `shouldBe` 6,
                   Text.count "\"kind\":\"SPAN_KIND_SERVER\"" requestBodyText `shouldBe` 1,
                   Text.count "\"kind\":\"SPAN_KIND_INTERNAL\"" requestBodyText `shouldBe` 3,
                   Text.count "\"kind\":\"SPAN_KIND_CLIENT\"" requestBodyText `shouldBe` 3,
                   extractQuotedJsonField "traceId" requestBodyText
                     `shouldSatisfy` maybe False (\traceId -> Text.length traceId == 32 && Text.all isHexDigit traceId),
                   extractQuotedJsonField "spanId" requestBodyText
                     `shouldSatisfy` maybe False (\spanId -> Text.length spanId == 16 && Text.all isHexDigit spanId)
                 ]
          )
        expectPlausibleEpochNanoTimestamps requestBodyText
        let startTimes = extractQuotedJsonIntegerFields "startTimeUnixNano" requestBodyText
            endTimes = extractQuotedJsonIntegerFields "endTimeUnixNano" requestBodyText
            durations = zipWith (-) endTimes startTimes
        case (startTimes, endTimes, durations) of
          ( [ rootStart,
              requestPolicyStart,
              routeMatchStart,
              renderResponseStart,
              secondPageDbStart,
              homePageDbStart,
              healthCheckDbStart
              ],
            [ rootEnd,
              requestPolicyEnd,
              routeMatchEnd,
              renderResponseEnd,
              secondPageDbEnd,
              homePageDbEnd,
              healthCheckDbEnd
              ],
            [ rootDuration,
              requestPolicyDuration,
              routeMatchDuration,
              renderResponseDuration,
              secondPageDbDuration,
              homePageDbDuration,
              healthCheckDbDuration
              ]
            ) -> do
              expectAll
                ( (rootDuration `shouldBe` 5000000)
                    :| [ rootEnd - rootStart `shouldBe` rootDuration,
                         requestPolicyStart - rootStart `shouldBe` 0,
                         requestPolicyDuration `shouldBe` 250000,
                         requestPolicyEnd `shouldBe` requestPolicyStart + requestPolicyDuration,
                         routeMatchStart - rootStart `shouldBe` 500000,
                         routeMatchDuration `shouldBe` 750000,
                         routeMatchEnd `shouldBe` routeMatchStart + routeMatchDuration,
                         renderResponseStart - rootStart `shouldBe` 1500000,
                         renderResponseDuration `shouldBe` 3000000,
                         renderResponseEnd `shouldBe` renderResponseStart + renderResponseDuration,
                         secondPageDbStart - rootStart `shouldBe` 2000000,
                         secondPageDbDuration `shouldBe` 1250000,
                         secondPageDbEnd `shouldBe` secondPageDbStart + secondPageDbDuration,
                         homePageDbStart `shouldBe` rootStart,
                         homePageDbDuration `shouldBe` rootDuration,
                         homePageDbEnd `shouldBe` rootEnd,
                         healthCheckDbStart `shouldBe` rootStart,
                         healthCheckDbDuration `shouldBe` rootDuration,
                         healthCheckDbEnd `shouldBe` rootEnd,
                         requestPolicyEnd `shouldSatisfy` (<= rootEnd),
                         routeMatchEnd `shouldSatisfy` (<= rootEnd),
                         renderResponseEnd `shouldSatisfy` (<= rootEnd),
                         secondPageDbEnd `shouldSatisfy` (<= rootEnd),
                         homePageDbEnd `shouldSatisfy` (<= rootEnd),
                         healthCheckDbEnd `shouldSatisfy` (<= rootEnd)
                       ]
                )
          _ ->
            expectationFailure "expected rooted OTLP timing data for one request span, three phase spans, and three DB spans"

    it "omits runtime phase child spans when request timing has only a measured root duration" $
      withOtlpCollector Http.ok200 "{}" $ \manager collectorUrl capturedRequestReference -> do
        exportRequestObservabilityToOtlp
          manager
          "sample-app"
          OtlpExporter
            { otlpEndpoint = collectorUrl,
              otlpHeaders = []
            }
          ( Observability.buildRequestObservability
              Observability.RequestIdentity
                { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "GET",
                  Observability.requestIdentityScheme = "http",
                  Observability.requestIdentityPath = "/assets/app.js",
                  Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/assets/*"
                }
              200
              Observability.BodyResponseKind
              [ Observability.ObservabilityAttribute
                  { Observability.attributeName = "harch.request.start_monotonic_ns",
                    Observability.attributeValue = Observability.IntAttribute 1000000
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "harch.request.duration_ns",
                    Observability.attributeValue = Observability.IntAttribute 5000000
                  }
              ]
          )
        CapturedCollectorRequest {capturedCollectorBody = requestBody} <-
          readMVar capturedRequestReference
        let requestBodyText = TextEncoding.decodeUtf8 (LazyByteString.toStrict requestBody)
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"name\":\"GET /assets/*\""
        requestBodyText `shouldNotSatisfy` Text.isInfixOf "\"name\":\"HarchWeb request policy\""
        requestBodyText `shouldNotSatisfy` Text.isInfixOf "\"name\":\"HarchWeb route match\""
        requestBodyText `shouldNotSatisfy` Text.isInfixOf "\"name\":\"HarchWeb render response\""
        requestBodyText `shouldNotSatisfy` Text.isInfixOf "\"harch.span.phase\""
        requestBodyText `shouldNotSatisfy` Text.isInfixOf "\"harch.request.start_monotonic_ns\""
        requestBodyText `shouldNotSatisfy` Text.isInfixOf "\"harch.request.duration_ns\""
        Text.count "\"name\":\"GET /assets/*\"" requestBodyText `shouldBe` 1

    it "reuses incoming W3C trace context for OTLP request exports" $
      withOtlpCollector Http.ok200 "{}" $ \manager collectorUrl capturedRequestReference -> do
        exportRequestObservabilityToOtlp
          manager
          "sample-app"
          OtlpExporter
            { otlpEndpoint = collectorUrl,
              otlpHeaders = []
            }
          ( Observability.withRequestTraceContext
              Observability.RequestTraceContext
                { Observability.traceContextTraceId = "4bf92f3577b34da6a3ce929d0e0e4736",
                  Observability.traceContextParentSpanId = "00f067aa0ba902b7",
                  Observability.traceContextState = Just "vendor=value"
                }
              ( Observability.buildRequestObservability
                  Observability.RequestIdentity
                    { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "GET",
                      Observability.requestIdentityScheme = "http",
                      Observability.requestIdentityPath = "/known",
                      Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/known"
                    }
                  200
                  Observability.BodyResponseKind
                  [ Observability.ObservabilityAttribute
                      { Observability.attributeName = "harch.request.start_monotonic_ns",
                        Observability.attributeValue = Observability.IntAttribute 1000000
                      },
                    Observability.ObservabilityAttribute
                      { Observability.attributeName = "harch.request.duration_ns",
                        Observability.attributeValue = Observability.IntAttribute 5000000
                      }
                  ]
              )
          )
        CapturedCollectorRequest {capturedCollectorBody = requestBody} <-
          readMVar capturedRequestReference
        let requestBodyText = TextEncoding.decodeUtf8 (LazyByteString.toStrict requestBody)
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"traceId\":\"4bf92f3577b34da6a3ce929d0e0e4736\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"parentSpanId\":\"00f067aa0ba902b7\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"traceState\":\"vendor=value\""
        extractQuotedJsonField "traceId" requestBodyText `shouldBe` Just "4bf92f3577b34da6a3ce929d0e0e4736"
        Text.count "\"parentSpanId\":\"00f067aa0ba902b7\"" requestBodyText `shouldBe` 1

    it "uses an intentional fallback duration when direct request exports lack runtime timing metadata" $
      withOtlpCollector Http.ok200 "{}" $ \manager collectorUrl capturedRequestReference -> do
        exportRequestObservabilityToOtlp
          manager
          "sample-app"
          OtlpExporter
            { otlpEndpoint = collectorUrl,
              otlpHeaders = []
            }
          ( Observability.withDatabaseOperations
              [Database.DatabaseOperation "postgresql" "ping-database" "SELECT 1;" Nothing Nothing]
              ( Observability.buildRequestObservability
                  Observability.RequestIdentity
                    { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "GET",
                      Observability.requestIdentityScheme = "http",
                      Observability.requestIdentityPath = "/health",
                      Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/health"
                    }
                  200
                  Observability.BodyResponseKind
                  []
              )
          )
        CapturedCollectorRequest {capturedCollectorBody = requestBody} <-
          readMVar capturedRequestReference
        let requestBodyText = TextEncoding.decodeUtf8 (LazyByteString.toStrict requestBody)
            startTimes = extractQuotedJsonIntegerFields "startTimeUnixNano" requestBodyText
            endTimes = extractQuotedJsonIntegerFields "endTimeUnixNano" requestBodyText
            durations = zipWith (-) endTimes startTimes
        expectAll
          ( (requestBodyText `shouldSatisfy` Text.isInfixOf "\"name\":\"GET /health\"")
              :| [ requestBodyText `shouldSatisfy` Text.isInfixOf "\"name\":\"DB ping-database\"",
                   Text.count "\"kind\":\"SPAN_KIND_SERVER\"" requestBodyText `shouldBe` 1,
                   Text.count "\"kind\":\"SPAN_KIND_CLIENT\"" requestBodyText `shouldBe` 1,
                   startTimes `shouldSatisfy` ((== 2) . length),
                   endTimes `shouldSatisfy` ((== 2) . length),
                   durations `shouldBe` [2000, 2000]
                 ]
          )
        case startTimes of
          [rootStart, childStart] -> rootStart `shouldBe` childStart
          _ -> expectationFailure "expected root and child OTLP spans"

    it "fails explicitly when the collector rejects the export request" $
      withOtlpCollector Http.serviceUnavailable503 "{\"error\":\"collector unavailable\"}" $ \manager collectorUrl capturedRequestReference -> do
        exportResult <-
          try
            ( exportRequestObservabilityToOtlp
                manager
                "sample-app"
                OtlpExporter
                  { otlpEndpoint = collectorUrl,
                    otlpHeaders = []
                  }
                ( Observability.buildRequestObservability
                    Observability.RequestIdentity
                      { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "GET",
                        Observability.requestIdentityScheme = "http",
                        Observability.requestIdentityPath = "/",
                        Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/"
                      }
                    200
                    Observability.BodyResponseKind
                    [ Observability.ObservabilityAttribute
                        { Observability.attributeName = "harch.request.duration_ns",
                          Observability.attributeValue = Observability.IntAttribute (-1)
                        }
                    ]
                )
            ) ::
            IO (Either IOError ())
        _ <- readMVar capturedRequestReference
        case exportResult of
          Left exportError -> do
            show exportError `shouldContain` "OTLP trace export failed with status 503"
            show exportError `shouldContain` "collector unavailable"
          Right () ->
            expectationFailure "expected OTLP export to fail when the collector returns a non-2xx status"

  describe "exportConnectionObservabilityToOtlp" $ do
    it "posts OTLP trace payloads for connection-level observability" $
      withOtlpCollector Http.ok200 "{}" $ \manager collectorUrl capturedRequestReference -> do
        exportConnectionObservabilityToOtlp
          manager
          "sample-app"
          OtlpExporter
            { otlpEndpoint = collectorUrl,
              otlpHeaders = [("authorization", "Bearer sample-token")]
            }
          ( Observability.buildConnectionObservability
              "CONNECTION insecure-connection-denied"
              [ Observability.ObservabilityAttribute
                  { Observability.attributeName = "network.peer.address",
                    Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "exception.type",
                    Observability.attributeValue = Observability.TextAttribute "InsecureConnectionDenied"
                  }
              ]
          )
        CapturedCollectorRequest
          { capturedCollectorMethod = requestMethod,
            capturedCollectorPath = requestPath,
            capturedCollectorHeaders = requestHeaders,
            capturedCollectorBody = requestBody
          } <-
          readMVar capturedRequestReference
        let requestBodyText = TextEncoding.decodeUtf8 (LazyByteString.toStrict requestBody)
        let startTimes = extractQuotedJsonIntegerFields "startTimeUnixNano" requestBodyText
            endTimes = extractQuotedJsonIntegerFields "endTimeUnixNano" requestBodyText
        expectAll
          ( (requestMethod `shouldBe` "POST")
              :| [ requestPath `shouldBe` "/v1/traces",
                   lookup Http.hContentType requestHeaders `shouldBe` Just "application/json",
                   lookup "authorization" requestHeaders `shouldBe` Just "Bearer sample-token",
                   requestBodyText `shouldSatisfy` Text.isInfixOf "\"name\":\"CONNECTION insecure-connection-denied\"",
                   requestBodyText `shouldSatisfy` Text.isInfixOf "\"kind\":\"SPAN_KIND_INTERNAL\"",
                   requestBodyText `shouldNotSatisfy` Text.isInfixOf "\"kind\":\"SPAN_KIND_SERVER\"",
                   requestBodyText `shouldSatisfy` Text.isInfixOf "\"network.peer.address\"",
                   requestBodyText `shouldSatisfy` Text.isInfixOf "\"InsecureConnectionDenied\"",
                   requestBodyText `shouldSatisfy` Text.isInfixOf "\"STATUS_CODE_ERROR\"",
                   expectPlausibleEpochNanoTimestamps requestBodyText,
                   startTimes `shouldSatisfy` ((== 1) . length),
                   endTimes `shouldSatisfy` ((== 1) . length),
                   zipWith (-) endTimes startTimes `shouldBe` [1000]
                 ]
          )

    it "posts OTLP trace payloads for prematurely closed connection observability" $
      withOtlpCollector Http.ok200 "{}" $ \manager collectorUrl capturedRequestReference -> do
        exportConnectionObservabilityToOtlp
          manager
          "sample-app"
          OtlpExporter
            { otlpEndpoint = collectorUrl,
              otlpHeaders = []
            }
          ( Observability.buildConnectionObservability
              "CONNECTION client-closed-connection-prematurely"
              [ Observability.ObservabilityAttribute
                  { Observability.attributeName = "network.peer.address",
                    Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "exception.type",
                    Observability.attributeValue = Observability.TextAttribute "ClientClosedConnectionPrematurely"
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "harch.connection.event",
                    Observability.attributeValue = Observability.TextAttribute "client-closed-connection-prematurely"
                  }
              ]
          )
        CapturedCollectorRequest
          { capturedCollectorMethod = requestMethod,
            capturedCollectorPath = requestPath,
            capturedCollectorHeaders = requestHeaders,
            capturedCollectorBody = requestBody
          } <-
          readMVar capturedRequestReference
        let requestBodyText = TextEncoding.decodeUtf8 (LazyByteString.toStrict requestBody)
            startTimes = extractQuotedJsonIntegerFields "startTimeUnixNano" requestBodyText
            endTimes = extractQuotedJsonIntegerFields "endTimeUnixNano" requestBodyText
        expectAll
          ( (requestMethod `shouldBe` "POST")
              :| [ requestPath `shouldBe` "/v1/traces",
                   lookup Http.hContentType requestHeaders `shouldBe` Just "application/json",
                   requestBodyText `shouldSatisfy` Text.isInfixOf "\"name\":\"CONNECTION client-closed-connection-prematurely\"",
                   requestBodyText `shouldSatisfy` Text.isInfixOf "\"kind\":\"SPAN_KIND_INTERNAL\"",
                   requestBodyText `shouldNotSatisfy` Text.isInfixOf "\"kind\":\"SPAN_KIND_SERVER\"",
                   requestBodyText `shouldSatisfy` Text.isInfixOf "\"network.peer.address\"",
                   requestBodyText `shouldSatisfy` Text.isInfixOf "\"ClientClosedConnectionPrematurely\"",
                   requestBodyText `shouldSatisfy` Text.isInfixOf "\"harch.connection.event\"",
                   requestBodyText `shouldSatisfy` Text.isInfixOf "\"client-closed-connection-prematurely\"",
                   requestBodyText `shouldSatisfy` Text.isInfixOf "\"STATUS_CODE_ERROR\"",
                   expectPlausibleEpochNanoTimestamps requestBodyText,
                   startTimes `shouldSatisfy` ((== 1) . length),
                   endTimes `shouldSatisfy` ((== 1) . length),
                   zipWith (-) endTimes startTimes `shouldBe` [1000]
                 ]
          )
