{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Concurrent (forkIO, myThreadId, newEmptyMVar, putMVar, readMVar, threadDelay, throwTo)
import Control.Exception (AsyncException (ThreadKilled), Exception (displayException), SomeException, try)
import Control.Monad ()
import Data.ByteString qualified as ByteString (ByteString, empty)
import Data.ByteString.Builder qualified as Builder ()
import Data.ByteString.Char8 qualified as ByteStringChar8 (concat, isInfixOf, pack)
import Data.ByteString.Lazy qualified as LazyByteString ()
import Data.Char ()
import Data.Either ()
import Data.Functor.Compose ()
import Data.IORef (atomicModifyIORef', newIORef)
import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe ()
import Data.Text ()
import Data.Text qualified as Text (isInfixOf, pack)
import Data.Text.Encoding qualified as TextEncoding ()
import HarchWeb (Application (renderRequestResponse, routeExecutionPolicy), LocalTestServer (localServerBaseUrl, localServerHost, localServerPort), RequestHeadLimits (requestCookieCountLimit, requestHeaderByteLimit, requestHeaderCountLimit), RequestPolicyConfig (requestConcurrencyLimit, requestHeadLimits, requestTransportLimits), RequestTransportLimits (requestNetworkTimeout, requestSlowlorisByteLimit), RouteExecutionPolicy (RouteExecutionPolicy), RouteRequest (requestRoute), ServerSentEventSource (ServerSentEventSource), StaticAssetRoot (StaticAssetRoot, staticDirectory, staticUrlPrefix), StaticAssetsConfig (StaticAssetsConfig, staticAssetContentTypes, staticAssetRoots, staticCacheControlSeconds), defaultStaticAssetContentTypes, eventStreamResponse, mkRequestConcurrencyLimit, mkRequestHeaderCountLimit, nonPageResponse, requestByteLimit, requestItemCountLimit, requestTimeoutSeconds, toWaiApplication, unboundedRequestHeadLimits, unboundedRouteExecutionPolicy, warpDefaultRequestTransportLimits, withLocalTestServer, withLocalTestServerForApplication)
import HarchWeb.Action qualified as Action ()
import HarchWeb.Database qualified as Database ()
import HarchWeb.Markup.Unsafe qualified as MarkupUnsafe ()
import HarchWeb.Observability qualified as Observability ()
import HarchWeb.Security qualified as Security ()
import Network.HTTP.Client qualified as HttpClient ()
import Network.HTTP.Types qualified as Http (status200)
import Network.Socket qualified as Socket (Family (AF_INET), SockAddr (SockAddrInet), SocketType (Stream), close, connect, defaultProtocol, socket, tupleToHostAddress)
import Network.Socket.ByteString qualified as SocketByteString (recv, sendAll)
import Network.Wai qualified as Wai (Request (rawPathInfo), responseLBS)
import Network.Wai.Handler.Warp qualified as Warp ()
import System.Directory (createDirectoryIfMissing)
import System.Environment ()
import System.Exit ()
import System.FilePath ((</>))
import System.IO ()
import System.IO.Error ()
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Signals ()
import System.Process ()
import TestCore.CustomAssertions ()
import TestCore.Wai ()
import Text.Read ()
import Unit.HarchWeb.TestSupport (TestRoute (DataRoute, EventStreamRoute, KnownRoute), defaultRequestPolicy, emptyStaticAssets, readLocalTestServerResponse, readRawLoopbackHttpResponse, sampleApplication, sampleApplicationWithConfig, sampleApplicationWithStaticAssets, waitUntilIORefEquals)

spec = do
  describe "withLocalTestServer" $ do
    it "serves the rendered application over a real loopback HTTP listener" $
      withLocalTestServer sampleApplication $ \localTestServer -> do
        localServerHost localTestServer `shouldBe` "127.0.0.1"
        localServerPort localTestServer `shouldSatisfy` (> 0)
        localServerBaseUrl localTestServer `shouldBe` Text.pack ("http://127.0.0.1:" <> show (localServerPort localTestServer))
        responseText <- readLocalTestServerResponse localTestServer "/known"
        Text.isInfixOf "<h1>Known</h1>" responseText `shouldBe` True
        Text.isInfixOf "<nav data-navigation-region=\"primary\">" responseText `shouldBe` True

    it "serves static assets through the same loopback HTTP listener" $
      withSystemTempDirectory "harch-web-local-static" $ \tempDirectory -> do
        let assetConfig =
              StaticAssetsConfig
                { staticAssetRoots =
                    [ StaticAssetRoot
                        { staticUrlPrefix = "/assets",
                          staticDirectory = tempDirectory
                        }
                    ],
                  staticAssetContentTypes = defaultStaticAssetContentTypes,
                  staticCacheControlSeconds = Just 60
                }
            staticApplication = sampleApplicationWithStaticAssets assetConfig
            assetDirectory = tempDirectory </> "styles"
            assetPath = assetDirectory </> "site.css"
        createDirectoryIfMissing True assetDirectory
        writeFile assetPath "body { color: red; }"
        withLocalTestServer staticApplication $ \localTestServer -> do
          responseText <- readLocalTestServerResponse localTestServer "/assets/styles/site.css"
          Text.isInfixOf "body { color: red; }" responseText `shouldBe` True

    it "rejects an oversized header block at the real Warp listener" $ do
      let limitedApplication =
            sampleApplicationWithConfig
              emptyStaticAssets
              ( defaultRequestPolicy
                  { requestHeadLimits = unboundedRequestHeadLimits {requestHeaderByteLimit = requestByteLimit 64},
                    requestTransportLimits =
                      warpDefaultRequestTransportLimits
                        { requestNetworkTimeout = requestTimeoutSeconds 1,
                          requestSlowlorisByteLimit = requestByteLimit 1
                        }
                  }
              )
      withLocalTestServer limitedApplication $ \localTestServer -> do
        responseBytes <-
          readRawLoopbackHttpResponse
            (localServerPort localTestServer)
            "GET /known HTTP/1.1\r\nHost: 127.0.0.1\r\nX-Oversized: 012345678901234567890123456789012345678901234567890123456789\r\n\r\n"
        -- Warp rejects an over-limit wire head before WAI request construction
        -- with its stable parser-level 400. The WAI gate supplies 431 for the
        -- count and individual-value limits it can inspect.
        responseBytes `shouldSatisfy` ByteStringChar8.isInfixOf "400 Bad Request"

    it "lets Warp's always-on total-header-length cap admit many small headers, leaving the WAI count gate to reject them with 431" $ do
      let limitedApplication =
            sampleApplicationWithConfig
              emptyStaticAssets
              (defaultRequestPolicy {requestHeadLimits = unboundedRequestHeadLimits {requestHeaderCountLimit = mkRequestHeaderCountLimit 3}})
          fiveSmallHeaders = ByteStringChar8.concat ["X-Small" <> ByteStringChar8.pack (show headerIndex) <> ": v\r\n" | headerIndex <- [1 .. 5 :: Int]]
      withLocalTestServer limitedApplication $ \localTestServer -> do
        responseBytes <-
          readRawLoopbackHttpResponse
            (localServerPort localTestServer)
            ("GET /known HTTP/1.1\r\nHost: 127.0.0.1\r\n" <> fiveSmallHeaders <> "\r\n")
        -- Five short headers stay far under Warp's always-on 50 KiB total
        -- header allocation cap (Warp 3.4.12 has no header-count or
        -- per-value setting; see docs/runtime-configuration.md), so Warp
        -- itself constructs the WAI request; only the count-aware WAI gate
        -- can reject it, with 431.
        responseBytes `shouldSatisfy` ByteStringChar8.isInfixOf "431"

    it "rejects a cookie count across repeated Cookie fields at the real listener" $ do
      let limitedApplication =
            sampleApplicationWithConfig
              emptyStaticAssets
              (defaultRequestPolicy {requestHeadLimits = unboundedRequestHeadLimits {requestCookieCountLimit = requestItemCountLimit 1}})
      withLocalTestServer limitedApplication $ \localTestServer -> do
        responseBytes <-
          readRawLoopbackHttpResponse
            (localServerPort localTestServer)
            "GET /known HTTP/1.1\r\nHost: 127.0.0.1\r\nCookie: first=1\r\ncOoKiE: second=2\r\n\r\n"
        responseBytes `shouldSatisfy` ByteStringChar8.isInfixOf "431"

    it "closes an incomplete request that makes no configured network progress" $ do
      let limitedApplication =
            sampleApplicationWithConfig
              emptyStaticAssets
              ( defaultRequestPolicy
                  { requestTransportLimits =
                      warpDefaultRequestTransportLimits
                        { requestNetworkTimeout = requestTimeoutSeconds 1,
                          requestSlowlorisByteLimit = requestByteLimit 2
                        }
                  }
              )
      withLocalTestServer limitedApplication $ \localTestServer -> do
        clientSocket <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
        socketResult <- try $ do
          Socket.connect clientSocket (Socket.SockAddrInet (fromIntegral (localServerPort localTestServer)) (Socket.tupleToHostAddress (127, 0, 0, 1)))
          SocketByteString.sendAll clientSocket "G"
          threadDelay 2500000
          SocketByteString.recv clientSocket 1
        Socket.close clientSocket
        socketResult `shouldBe` (Right ByteString.empty :: Either IOError ByteString.ByteString)

    it "admits at most the configured concurrent requests, returns 503 for the rest, and releases the slot when a request finishes" $ do
      releaseSignal <- newEmptyMVar
      admittedCount <- newIORef (0 :: Int)
      let baseApplication = sampleApplicationWithConfig emptyStaticAssets (defaultRequestPolicy {requestConcurrencyLimit = mkRequestConcurrencyLimit 1})
          slowApplication =
            baseApplication
              { renderRequestResponse = \request routeRequest ->
                  case requestRoute routeRequest of
                    KnownRoute -> do
                      atomicModifyIORef' admittedCount (\count -> (count + 1, ()))
                      readMVar releaseSignal
                      renderRequestResponse baseApplication request routeRequest
                    _ -> renderRequestResponse baseApplication request routeRequest
              }
      withLocalTestServer slowApplication $ \localTestServer -> do
        firstResponseSignal <- newEmptyMVar
        _ <-
          forkIO $
            readRawLoopbackHttpResponse (localServerPort localTestServer) "GET /known HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n"
              >>= putMVar firstResponseSignal
        waitUntilIORefEquals admittedCount 1
        secondResponseBytes <- readRawLoopbackHttpResponse (localServerPort localTestServer) "GET /known HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n"
        putMVar releaseSignal ()
        firstResponseBytes <- readMVar firstResponseSignal
        thirdResponseBytes <- readRawLoopbackHttpResponse (localServerPort localTestServer) "GET /known HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n"
        expectAll
          ( (secondResponseBytes `shouldSatisfy` ByteStringChar8.isInfixOf "503")
              :| [ firstResponseBytes `shouldSatisfy` ByteStringChar8.isInfixOf "200",
                   thirdResponseBytes `shouldSatisfy` ByteStringChar8.isInfixOf "200"
                 ]
          )

    it "applies a route-local admission budget at the real listener without constraining another route" $ do
      releaseSignal <- newEmptyMVar
      admittedCount <- newIORef (0 :: Int)
      let baseApplication = sampleApplicationWithConfig emptyStaticAssets defaultRequestPolicy
          limitedApplication =
            baseApplication
              { routeExecutionPolicy =
                  \case
                    KnownRoute -> RouteExecutionPolicy (mkRequestConcurrencyLimit 1)
                    DataRoute -> RouteExecutionPolicy (mkRequestConcurrencyLimit 1)
                    _ -> unboundedRouteExecutionPolicy,
                renderRequestResponse = \request routeRequest ->
                  case requestRoute routeRequest of
                    KnownRoute -> do
                      atomicModifyIORef' admittedCount (\count -> (count + 1, ()))
                      readMVar releaseSignal
                      renderRequestResponse baseApplication request routeRequest
                    _ -> renderRequestResponse baseApplication request routeRequest
              }
      withLocalTestServer limitedApplication $ \localTestServer -> do
        firstResponseSignal <- newEmptyMVar
        _ <-
          forkIO $
            readRawLoopbackHttpResponse (localServerPort localTestServer) "GET /known HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n"
              >>= putMVar firstResponseSignal
        waitUntilIORefEquals admittedCount 1
        blockedResponseBytes <- readRawLoopbackHttpResponse (localServerPort localTestServer) "GET /known HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n"
        otherRouteResponseBytes <- readRawLoopbackHttpResponse (localServerPort localTestServer) "GET /data HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n"
        putMVar releaseSignal ()
        firstResponseBytes <- readMVar firstResponseSignal
        releasedRouteResponseBytes <- readRawLoopbackHttpResponse (localServerPort localTestServer) "GET /known HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n"
        expectAll
          ( (blockedResponseBytes `shouldSatisfy` ByteStringChar8.isInfixOf "503")
              :| [ otherRouteResponseBytes `shouldSatisfy` ByteStringChar8.isInfixOf "202",
                   firstResponseBytes `shouldSatisfy` ByteStringChar8.isInfixOf "200",
                   releasedRouteResponseBytes `shouldSatisfy` ByteStringChar8.isInfixOf "200"
                 ]
          )

    it "holds a route-local admission slot until its real listener event stream completes" $ do
      releaseSignal <- newEmptyMVar
      streamStartedSignal <- newEmptyMVar
      let baseApplication = sampleApplicationWithConfig emptyStaticAssets defaultRequestPolicy
          eventSource =
            ServerSentEventSource $ do
              putMVar streamStartedSignal ()
              readMVar releaseSignal
              pure Nothing
          limitedApplication =
            baseApplication
              { routeExecutionPolicy =
                  \case
                    EventStreamRoute -> RouteExecutionPolicy (mkRequestConcurrencyLimit 1)
                    _ -> unboundedRouteExecutionPolicy,
                renderRequestResponse = \request routeRequest ->
                  case requestRoute routeRequest of
                    EventStreamRoute -> pure (nonPageResponse (eventStreamResponse eventSource))
                    _ -> renderRequestResponse baseApplication request routeRequest
              }
      withLocalTestServer limitedApplication $ \localTestServer -> do
        firstResponseSignal <- newEmptyMVar
        _ <-
          forkIO $
            readRawLoopbackHttpResponse (localServerPort localTestServer) "GET /events HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n"
              >>= putMVar firstResponseSignal
        readMVar streamStartedSignal
        blockedResponseBytes <- readRawLoopbackHttpResponse (localServerPort localTestServer) "GET /events HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n"
        putMVar releaseSignal ()
        firstResponseBytes <- readMVar firstResponseSignal
        expectAll
          ( (blockedResponseBytes `shouldSatisfy` ByteStringChar8.isInfixOf "503")
              :| [firstResponseBytes `shouldSatisfy` ByteStringChar8.isInfixOf "200"]
          )

    it "releases an admitted slot when an asynchronous exception cancels its handler" $ do
      admittedCount <- newIORef (0 :: Int)
      let baseApplication = sampleApplicationWithConfig emptyStaticAssets (defaultRequestPolicy {requestConcurrencyLimit = mkRequestConcurrencyLimit 1})
          interruptedApplication =
            baseApplication
              { renderRequestResponse = \request routeRequest ->
                  case requestRoute routeRequest of
                    KnownRoute -> do
                      requestNumber <- atomicModifyIORef' admittedCount (\count -> let next = count + 1 in (next, next))
                      if requestNumber == 1
                        then do
                          requestThread <- myThreadId
                          _ <- forkIO (throwTo requestThread ThreadKilled)
                          threadDelay 1000000
                          renderRequestResponse baseApplication request routeRequest
                        else renderRequestResponse baseApplication request routeRequest
                    _ -> renderRequestResponse baseApplication request routeRequest
              }
      withLocalTestServer interruptedApplication $ \localTestServer -> do
        firstResponseSignal <- newEmptyMVar
        _ <-
          forkIO $
            try (readRawLoopbackHttpResponse (localServerPort localTestServer) "GET /known HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n")
              >>= putMVar firstResponseSignal
        waitUntilIORefEquals admittedCount 1
        _ <- readMVar firstResponseSignal :: IO (Either SomeException ByteString.ByteString)
        secondResponseBytes <- readRawLoopbackHttpResponse (localServerPort localTestServer) "GET /known HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n"
        secondResponseBytes `shouldSatisfy` ByteStringChar8.isInfixOf "200"

  describe "withLocalTestServerForApplication" $ do
    it "serves an already-built Wai.Application over a real loopback HTTP listener" $ do
      gatedApplication <- toWaiApplication sampleApplication
      let markedWaiApplication request respond =
            if Wai.rawPathInfo request == "/middleware-marker"
              then respond (Wai.responseLBS Http.status200 [] "handled by middleware")
              else gatedApplication request respond
      withLocalTestServerForApplication markedWaiApplication $ \localTestServer -> do
        localServerHost localTestServer `shouldBe` "127.0.0.1"
        knownResponseText <- readLocalTestServerResponse localTestServer "/known"
        markerResponseText <- readLocalTestServerResponse localTestServer "/middleware-marker"
        expectAll
          ( (Text.isInfixOf "<h1>Known</h1>" knownResponseText `shouldBe` True)
              :| [Text.isInfixOf "handled by middleware" markerResponseText `shouldBe` True]
          )

  describe "withLocalTestServer startup cleanup" $ do
    it "closes its loopback listener when Warp rejects the transport settings" $ do
      let startupFailure = "synthetic local-server transport startup failure"
          failingApplication =
            sampleApplicationWithConfig
              emptyStaticAssets
              ( defaultRequestPolicy
                  { requestTransportLimits =
                      warpDefaultRequestTransportLimits
                        { requestNetworkTimeout = Just (error startupFailure)
                        }
                  }
              )
      withLocalTestServer failingApplication (\_ -> expectationFailure "unexpected local server startup")
        `shouldThrow` (\exception -> startupFailure `isInfixOf` displayException (exception :: SomeException))
      recoveredResponse <- withLocalTestServer sampleApplication (`readLocalTestServerResponse` "/known")
      recoveredResponse `shouldSatisfy` Text.isInfixOf "<h1>Known</h1>"
