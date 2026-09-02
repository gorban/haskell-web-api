{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Concurrent ()
import Control.Exception ()
import Control.Monad ()
import Data.ByteString qualified as ByteString (replicate)
import Data.ByteString.Builder qualified as Builder ()
import Data.ByteString.Char8 qualified as ByteStringChar8 ()
import Data.ByteString.Lazy qualified as LazyByteString ()
import Data.Char ()
import Data.Either ()
import Data.Functor.Compose ()
import Data.IORef (newIORef, readIORef)
import Data.List ()
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text ()
import Data.Text qualified as Text ()
import Data.Text.Encoding qualified as TextEncoding ()
import HarchWeb (Application (applicationRequestMiddleware, renderRequestResponse), MiddlewareResult (ContinueMiddleware), RequestBodyReadFailure (RequestBodyLimitExceeded), RequestHeadLimitFailure (InvalidRequestTargetEncoding, RequestCookieNameTooLarge, RequestCookieValueTooLarge, RequestHeaderValueTooLarge, RequestHeadersTooLarge, RequestPathSegmentTooLarge, RequestQueryFieldTooLarge, RequestTargetTooLarge, TooManyPathSegments, TooManyQueryFields, TooManyRequestCookies, TooManyRequestHeaders), RequestHeadLimits (requestCookieCountLimit, requestCookieNameByteLimit, requestCookieValueByteLimit, requestHeaderByteLimit, requestHeaderCountLimit, requestHeaderValueByteLimit, requestPathSegmentByteLimit, requestPathSegmentCountLimit, requestQueryFieldByteLimit, requestQueryFieldCountLimit, requestTargetByteLimit), RequestMiddleware (RequestMiddleware), RequestPolicyConfig (requestHeadLimits), RequestTransportLimits (requestNetworkTimeout, requestSlowlorisByteLimit), RouteRequest (RouteRequest), mkRequestConcurrencyLimit, mkRequestHeaderCountLimit, newRequestBodyChunkReader, readRequestBodyUpTo, requestByteLimit, requestConcurrencyLimitValue, requestItemCountLimit, requestTimeoutSeconds, requestTimeoutSecondsValue, toWaiApplication, unboundedRequestHeadLimits, validateRequestHead, warpDefaultRequestTransportLimits)
import HarchWeb.Action qualified as Action ()
import HarchWeb.Database qualified as Database ()
import HarchWeb.Markup.Unsafe qualified as MarkupUnsafe ()
import HarchWeb.Observability qualified as Observability ()
import HarchWeb.Routing (RouteDecodeError (InvalidRouteTargetEncoding), decodeRouteLocation)
import HarchWeb.Security qualified as Security (RequestContextField (requestContextFieldName), requestContextFields, waiRequestPath, waiRequestRouteTarget)
import Network.HTTP.Client qualified as HttpClient ()
import Network.HTTP.Types qualified as Http (hContentLength, hContentType, status400, status414, status431)
import Network.Socket qualified as Socket ()
import Network.Socket.ByteString qualified as SocketByteString ()
import Network.Wai qualified as Wai (Request (rawPathInfo, rawQueryString, requestHeaders), defaultRequest, responseHeaders, responseStatus, setRequestBodyChunks)
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
import TestCore.Wai (nextRequestBodyChunk, performWaiRequest, readResponseBody)
import Text.Read ()
import Unit.HarchWeb.TestSupport (TestRoute (DataRoute), defaultContext, defaultRequestPolicy, emptyStaticAssets, renderSampleResponse, sampleApplicationWithConfig)

spec = do
  describe "request-head limits" $ do
    it "keeps the default policy deliberately unbounded" $
      validateRequestHead
        unboundedRequestHeadLimits
        Wai.defaultRequest
          { Wai.rawPathInfo = ByteString.replicate 8192 97,
            Wai.requestHeaders = [("X-Large", ByteString.replicate 8192 98)]
          }
        `shouldBe` Right ()

    it "classifies configured request-target, header, and encoding failures without retaining input" $ do
      let limits =
            unboundedRequestHeadLimits
              { requestTargetByteLimit = requestByteLimit 4,
                requestHeaderByteLimit = requestByteLimit 12,
                requestHeaderCountLimit = mkRequestHeaderCountLimit 1,
                requestHeaderValueByteLimit = requestByteLimit 3
              }
          requestFor requestPath headers =
            Wai.defaultRequest
              { Wai.rawPathInfo = requestPath,
                Wai.requestHeaders = headers
              }
          pathLimits =
            unboundedRequestHeadLimits
              { requestPathSegmentCountLimit = requestItemCountLimit 1,
                requestPathSegmentByteLimit = requestByteLimit 2
              }
          queryLimits =
            unboundedRequestHeadLimits
              { requestQueryFieldCountLimit = requestItemCountLimit 1,
                requestQueryFieldByteLimit = requestByteLimit 3
              }
          emptyQueryLimits = unboundedRequestHeadLimits {requestQueryFieldCountLimit = requestItemCountLimit 0}
          cookieCountLimits = unboundedRequestHeadLimits {requestCookieCountLimit = requestItemCountLimit 1}
          cookieNameLimits = unboundedRequestHeadLimits {requestCookieNameByteLimit = requestByteLimit 3}
          cookieValueLimits = unboundedRequestHeadLimits {requestCookieValueByteLimit = requestByteLimit 3}
      expectAll
        ( (validateRequestHead limits (requestFor "/long" []) `shouldBe` Left RequestTargetTooLarge)
            :| [ validateRequestHead limits (requestFor "\255" []) `shouldBe` Left InvalidRequestTargetEncoding,
                 validateRequestHead limits (requestFor "/ok" [("A", "1"), ("B", "2")]) `shouldBe` Left TooManyRequestHeaders,
                 validateRequestHead limits (requestFor "/ok" [("A", "1234")]) `shouldBe` Left RequestHeaderValueTooLarge,
                 validateRequestHead
                   (limits {requestHeaderValueByteLimit = Nothing, requestHeaderCountLimit = Nothing})
                   (requestFor "/ok" [("Header", "1234567")])
                   `shouldBe` Left RequestHeadersTooLarge,
                 validateRequestHead pathLimits (requestFor "/one/two" []) `shouldBe` Left TooManyPathSegments,
                 validateRequestHead pathLimits (requestFor "/long" []) `shouldBe` Left RequestPathSegmentTooLarge,
                 validateRequestHead (pathLimits {requestPathSegmentByteLimit = Nothing, requestPathSegmentCountLimit = requestItemCountLimit 0}) (requestFor "one" []) `shouldBe` Left TooManyPathSegments,
                 validateRequestHead queryLimits ((requestFor "/ok" []) {Wai.rawQueryString = "?one&two"}) `shouldBe` Left TooManyQueryFields,
                 validateRequestHead queryLimits ((requestFor "/ok" []) {Wai.rawQueryString = "?long"}) `shouldBe` Left RequestQueryFieldTooLarge,
                 validateRequestHead cookieCountLimits (requestFor "/ok" [("Cookie", "one=1; two=2")]) `shouldBe` Left TooManyRequestCookies,
                 validateRequestHead cookieNameLimits (requestFor "/ok" [("Cookie", "name=1")]) `shouldBe` Left RequestCookieNameTooLarge,
                 validateRequestHead cookieValueLimits (requestFor "/ok" [("Cookie", "a=long")]) `shouldBe` Left RequestCookieValueTooLarge,
                 validateRequestHead cookieCountLimits (requestFor "/ok" [("Cookie", "malformed; bad name=ignored; Empty=")]) `shouldBe` Right (),
                 validateRequestHead cookieCountLimits (requestFor "/ok" [("X-Ignored", "one=1"), ("Cookie", "!#$%&'*+-.0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ^_`abcdefghijklmnopqrstuvwxyz|~=value")]) `shouldBe` Right (),
                 validateRequestHead
                   (unboundedRequestHeadLimits {requestHeaderValueByteLimit = requestByteLimit 3, requestCookieValueByteLimit = requestByteLimit 100})
                   (requestFor "/ok" [("Cookie", "a=long")])
                   `shouldBe` Left RequestHeaderValueTooLarge,
                 validateRequestHead emptyQueryLimits ((requestFor "/ok" []) {Wai.rawQueryString = "?"}) `shouldBe` Right ()
               ]
        )
    it "keeps request-budget constructors total and their failure values inspectable" $ do
      byteLimitInput <- newIORef 8
      differentByteLimitInput <- newIORef 9
      headerCountLimitInput <- newIORef 2
      differentHeaderCountLimitInput <- newIORef 3
      itemCountLimitInput <- newIORef 4
      differentItemCountLimitInput <- newIORef 5
      timeoutSecondsInput <- newIORef 12
      failureInput <- newIORef RequestHeadersTooLarge
      byteLimit <- requestByteLimit <$> readIORef byteLimitInput
      differentByteLimit <- requestByteLimit <$> readIORef differentByteLimitInput
      headerCountLimit <- mkRequestHeaderCountLimit <$> readIORef headerCountLimitInput
      differentHeaderCountLimit <- mkRequestHeaderCountLimit <$> readIORef differentHeaderCountLimitInput
      itemCountLimit <- requestItemCountLimit <$> readIORef itemCountLimitInput
      differentItemCountLimit <- requestItemCountLimit <$> readIORef differentItemCountLimitInput
      timeoutSeconds <- requestTimeoutSeconds <$> readIORef timeoutSecondsInput
      failure <- readIORef failureInput
      let byteLimitValue = fromMaybe (error "expected request byte limit") byteLimit
          differentByteLimitValue = fromMaybe (error "expected distinct request byte limit") differentByteLimit
          headerCountLimitValue = fromMaybe (error "expected request header count limit") headerCountLimit
          differentHeaderCountLimitValue = fromMaybe (error "expected distinct request header count limit") differentHeaderCountLimit
          itemCountLimitValue = fromMaybe (error "expected request item count limit") itemCountLimit
          differentItemCountLimitValue = fromMaybe (error "expected distinct request item count limit") differentItemCountLimit
          timeoutSecondsValue = fromMaybe (error "expected request timeout") timeoutSeconds
          concurrencyLimitValue = fromMaybe (error "expected request concurrency limit") (mkRequestConcurrencyLimit 4)
          boundedHeadLimits =
            unboundedRequestHeadLimits
              { requestTargetByteLimit = byteLimit,
                requestHeaderCountLimit = headerCountLimit,
                requestCookieCountLimit = itemCountLimit,
                requestCookieNameByteLimit = byteLimit,
                requestCookieValueByteLimit = differentByteLimit
              }
          differentBoundedHeadLimits =
            unboundedRequestHeadLimits
              { requestTargetByteLimit = differentByteLimit,
                requestHeaderCountLimit = differentHeaderCountLimit,
                requestCookieCountLimit = differentItemCountLimit,
                requestCookieNameByteLimit = differentByteLimit,
                requestCookieValueByteLimit = byteLimit
              }
          transportLimits =
            warpDefaultRequestTransportLimits
              { requestNetworkTimeout = timeoutSeconds,
                requestSlowlorisByteLimit = byteLimit
              }
          differentTransportLimits =
            transportLimits
              { requestNetworkTimeout = requestTimeoutSeconds 13
              }
      expectAll
        ( (requestByteLimit (-1) `shouldBe` Nothing)
            :| [ mkRequestHeaderCountLimit (-1) `shouldBe` Nothing,
                 requestItemCountLimit (-1) `shouldBe` Nothing,
                 requestTimeoutSeconds (-1) `shouldBe` Nothing,
                 byteLimit `shouldNotBe` differentByteLimit,
                 headerCountLimit `shouldNotBe` differentHeaderCountLimit,
                 show byteLimit `shouldBe` "Just (RequestByteLimit 8)",
                 show headerCountLimit `shouldBe` "Just (RequestHeaderCountLimit 2)",
                 show boundedHeadLimits
                   `shouldBe` "RequestHeadLimits {requestTargetByteLimit = Just (RequestByteLimit 8), requestHeaderByteLimit = Nothing, requestHeaderCountLimit = Just (RequestHeaderCountLimit 2), requestHeaderValueByteLimit = Nothing, requestCookieCountLimit = Just (RequestItemCountLimit 4), requestCookieNameByteLimit = Just (RequestByteLimit 8), requestCookieValueByteLimit = Just (RequestByteLimit 9), requestPathSegmentCountLimit = Nothing, requestPathSegmentByteLimit = Nothing, requestQueryFieldCountLimit = Nothing, requestQueryFieldByteLimit = Nothing}",
                 boundedHeadLimits `shouldNotBe` differentBoundedHeadLimits,
                 failure `shouldNotBe` RequestTargetTooLarge,
                 show failure `shouldBe` "RequestHeadersTooLarge",
                 RequestCookieNameTooLarge `shouldNotBe` RequestCookieValueTooLarge,
                 show TooManyRequestCookies `shouldBe` "TooManyRequestCookies",
                 byteLimitValue /= differentByteLimitValue `shouldBe` True,
                 show byteLimitValue `shouldBe` "RequestByteLimit 8",
                 show [byteLimitValue] `shouldBe` "[RequestByteLimit 8]",
                 headerCountLimitValue /= differentHeaderCountLimitValue `shouldBe` True,
                 show headerCountLimitValue `shouldBe` "RequestHeaderCountLimit 2",
                 show [headerCountLimitValue] `shouldBe` "[RequestHeaderCountLimit 2]",
                 itemCountLimitValue /= differentItemCountLimitValue `shouldBe` True,
                 show itemCountLimitValue `shouldBe` "RequestItemCountLimit 4",
                 show [itemCountLimitValue] `shouldBe` "[RequestItemCountLimit 4]",
                 timeoutSecondsValue /= fromMaybe (error "expected distinct request timeout") (requestTimeoutSeconds 13) `shouldBe` True,
                 requestTimeoutSecondsValue timeoutSecondsValue `shouldBe` 12,
                 show timeoutSecondsValue `shouldBe` "RequestTimeoutSeconds 12",
                 show [timeoutSecondsValue] `shouldBe` "[RequestTimeoutSeconds 12]",
                 transportLimits /= differentTransportLimits `shouldBe` True,
                 show transportLimits `shouldBe` "RequestTransportLimits {requestNetworkTimeout = Just (RequestTimeoutSeconds 12), requestSlowlorisByteLimit = Just (RequestByteLimit 8)}",
                 show [transportLimits] `shouldBe` "[RequestTransportLimits {requestNetworkTimeout = Just (RequestTimeoutSeconds 12), requestSlowlorisByteLimit = Just (RequestByteLimit 8)}]",
                 length (show boundedHeadLimits)
                   + length (showList [boundedHeadLimits] "")
                     `shouldSatisfy` (> 0),
                 length (show failure)
                   + length (showList [failure] "")
                     `shouldSatisfy` (> 0),
                 mkRequestConcurrencyLimit 0 `shouldBe` Nothing,
                 mkRequestConcurrencyLimit (-1) `shouldBe` Nothing,
                 requestConcurrencyLimitValue <$> mkRequestConcurrencyLimit 4 `shouldBe` Just 4,
                 (concurrencyLimitValue /= fromMaybe (error "expected distinct concurrency limit") (mkRequestConcurrencyLimit 5))
                   `shouldBe` True,
                 length (show concurrencyLimitValue)
                   + length (showList [concurrencyLimitValue] "")
                     `shouldSatisfy` (> 0)
               ]
        )

    it "keeps request-text helpers total for invalid UTF-8" $
      expectAll
        ( ( Security.waiRequestPath
              defaultRequestPolicy
              (Wai.defaultRequest {Wai.rawPathInfo = "\255"})
              `shouldBe` ""
          )
            :| [ decodeRouteLocation
                   ( Security.waiRequestRouteTarget
                       defaultRequestPolicy
                       (Wai.defaultRequest {Wai.rawPathInfo = "/known", Wai.rawQueryString = "?\255"})
                   )
                   `shouldBe` Left InvalidRouteTargetEncoding,
                 map
                   Security.requestContextFieldName
                   ( Security.requestContextFields
                       defaultRequestPolicy
                       (Wai.defaultRequest {Wai.requestHeaders = [("User-Agent", "\255")]})
                   )
                   `shouldNotContain` ["user_agent.original"],
                 Security.waiRequestPath
                   defaultRequestPolicy
                   (Wai.defaultRequest {Wai.rawPathInfo = "/known"})
                   `shouldBe` "/known"
               ]
        )

    it "bounds request bodies while chunks arrive" $ do
      successfulChunks <- newIORef ["ab", "c"]
      oversizedChunks <- newIORef ["ab", "cd"]
      let requestFrom chunksReference =
            Wai.setRequestBodyChunks (nextRequestBodyChunk chunksReference) Wai.defaultRequest
      successfulResult <- readRequestBodyUpTo 3 (requestFrom successfulChunks)
      oversizedResult <- readRequestBodyUpTo 3 (requestFrom oversizedChunks)
      declaredOversizedResult <-
        readRequestBodyUpTo
          3
          (Wai.defaultRequest {Wai.requestHeaders = [(Http.hContentLength, "4")]})
      malformedDeclaredLengthResult <-
        readRequestBodyUpTo
          3
          (Wai.defaultRequest {Wai.requestHeaders = [(Http.hContentLength, "4bytes")]})
      negativeLimitResult <- readRequestBodyUpTo (-1) Wai.defaultRequest
      expectAll
        ( (successfulResult `shouldBe` Right "abc")
            :| [ oversizedResult `shouldBe` Left RequestBodyLimitExceeded,
                 declaredOversizedResult `shouldBe` Left RequestBodyLimitExceeded,
                 malformedDeclaredLengthResult `shouldBe` Right "",
                 negativeLimitResult `shouldBe` Left RequestBodyLimitExceeded,
                 -- 'RequestBodyLimitExceeded' is the complete error domain: there is
                 -- no distinct inhabitant with which a caller can compare it. These
                 -- assertions therefore document its observable equality contract,
                 -- including the complementary default implementation of '(/=').
                 RequestBodyLimitExceeded == RequestBodyLimitExceeded `shouldBe` True,
                 RequestBodyLimitExceeded /= RequestBodyLimitExceeded `shouldBe` False,
                 length (show RequestBodyLimitExceeded) `shouldSatisfy` (> 0),
                 length (showList [RequestBodyLimitExceeded] "") `shouldSatisfy` (> 0)
               ]
        )

    it "pulls request body chunks incrementally within a bound" $ do
      chunksReference <- newIORef ["ab", "c"]
      pullChunk <- newRequestBodyChunkReader 3 (Wai.setRequestBodyChunks (nextRequestBodyChunk chunksReference) Wai.defaultRequest)
      firstChunk <- pullChunk
      secondChunk <- pullChunk
      endChunk <- pullChunk
      repeatedEndChunk <- pullChunk
      expectAll
        ( (firstChunk `shouldBe` Right "ab")
            :| [ secondChunk `shouldBe` Right "c",
                 endChunk `shouldBe` Right "",
                 repeatedEndChunk `shouldBe` Right ""
               ]
        )

    it "rejects a streamed chunk that would push the running total over the bound, retaining only the prior chunk" $ do
      chunksReference <- newIORef ["ab", "cd"]
      pullChunk <- newRequestBodyChunkReader 3 (Wai.setRequestBodyChunks (nextRequestBodyChunk chunksReference) Wai.defaultRequest)
      firstChunk <- pullChunk
      secondChunk <- pullChunk
      expectAll
        ( (firstChunk `shouldBe` Right "ab")
            :| [secondChunk `shouldBe` Left RequestBodyLimitExceeded]
        )

    it "rejects every pull once the declared Content-Length exceeds the bound, before reading" $ do
      pullChunk <- newRequestBodyChunkReader 3 (Wai.defaultRequest {Wai.requestHeaders = [(Http.hContentLength, "4")]})
      firstResult <- pullChunk
      secondResult <- pullChunk
      expectAll
        ( (firstResult `shouldBe` Left RequestBodyLimitExceeded)
            :| [secondResult `shouldBe` Left RequestBodyLimitExceeded]
        )

    it "treats a malformed declared Content-Length as unavailable rather than rejecting early" $ do
      pullChunk <- newRequestBodyChunkReader 3 (Wai.defaultRequest {Wai.requestHeaders = [(Http.hContentLength, "4bytes")]})
      result <- pullChunk
      result `shouldBe` Right ""

    it "rejects every non-empty streamed chunk against a negative bound" $ do
      chunksReference <- newIORef ["a"]
      pullChunk <- newRequestBodyChunkReader (-1) (Wai.setRequestBodyChunks (nextRequestBodyChunk chunksReference) Wai.defaultRequest)
      result <- pullChunk
      result `shouldBe` Left RequestBodyLimitExceeded

    it "rejects a configured request head before application routing or middleware" $ do
      let limits = unboundedRequestHeadLimits {requestTargetByteLimit = requestByteLimit 4}
          limitedApplication =
            (sampleApplicationWithConfig emptyStaticAssets (defaultRequestPolicy {requestHeadLimits = limits}))
              { applicationRequestMiddleware = [RequestMiddleware (\_ _ -> expectationFailure "request-head gate should run first" >> pure (ContinueMiddleware defaultContext))],
                renderRequestResponse = \_ _ -> expectationFailure "request-head gate should run first" >> pure (renderSampleResponse (RouteRequest DataRoute defaultContext))
              }
      response <- performWaiRequest (toWaiApplication limitedApplication) (Wai.defaultRequest {Wai.rawPathInfo = "/long"})
      Wai.responseStatus response `shouldBe` Http.status414

    it "maps every rejected request-head budget to its public HTTP status" $ do
      let applicationFor limits = sampleApplicationWithConfig emptyStaticAssets (defaultRequestPolicy {requestHeadLimits = limits})
          requestFor rawPath headers rawQuery = Wai.defaultRequest {Wai.rawPathInfo = rawPath, Wai.requestHeaders = headers, Wai.rawQueryString = rawQuery}
          cases =
            [ (unboundedRequestHeadLimits {requestTargetByteLimit = requestByteLimit 4}, requestFor "/long" [] "", Http.status414),
              (unboundedRequestHeadLimits, requestFor "\255" [] "", Http.status400),
              (unboundedRequestHeadLimits {requestHeaderCountLimit = mkRequestHeaderCountLimit 1}, requestFor "/ok" [("A", "1"), ("B", "2")] "", Http.status431),
              (unboundedRequestHeadLimits {requestHeaderByteLimit = requestByteLimit 4}, requestFor "/ok" [("Header", "value")] "", Http.status431),
              (unboundedRequestHeadLimits {requestHeaderValueByteLimit = requestByteLimit 3}, requestFor "/ok" [("A", "1234")] "", Http.status431),
              (unboundedRequestHeadLimits {requestCookieCountLimit = requestItemCountLimit 1}, requestFor "/ok" [("Cookie", "one=1; two=2")] "", Http.status431),
              (unboundedRequestHeadLimits {requestCookieNameByteLimit = requestByteLimit 3}, requestFor "/ok" [("Cookie", "name=1")] "", Http.status431),
              (unboundedRequestHeadLimits {requestCookieValueByteLimit = requestByteLimit 3}, requestFor "/ok" [("Cookie", "a=long")] "", Http.status431),
              (unboundedRequestHeadLimits {requestPathSegmentCountLimit = requestItemCountLimit 1}, requestFor "/one/two" [] "", Http.status414),
              (unboundedRequestHeadLimits {requestPathSegmentByteLimit = requestByteLimit 2}, requestFor "/long" [] "", Http.status414),
              (unboundedRequestHeadLimits {requestQueryFieldCountLimit = requestItemCountLimit 1}, requestFor "/ok" [] "?one&two", Http.status414),
              (unboundedRequestHeadLimits {requestQueryFieldByteLimit = requestByteLimit 3}, requestFor "/ok" [] "?long", Http.status414)
            ]
      responses <- traverse (\(limits, request, _) -> performWaiRequest (toWaiApplication (applicationFor limits)) request) cases
      responseBodies <- traverse readResponseBody responses
      expectAll
        ( (map Wai.responseStatus responses `shouldBe` map (\(_, _, expectedStatus) -> expectedStatus) cases)
            :| ( map (\response -> lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just "text/plain; charset=utf-8") responses
                   <> map (`shouldBe` "Request metadata was rejected.") responseBodies
               )
        )
