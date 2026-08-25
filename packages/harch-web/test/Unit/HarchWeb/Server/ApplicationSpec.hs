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
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.List ()
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe ()
import Data.Text (Text)
import Data.Text qualified as Text ()
import Data.Text.Encoding qualified as TextEncoding ()
import HarchWeb (Application (appName, renderRequestResponse), MiddlewareResult (ContinueMiddleware, HaltMiddleware), RequestMiddleware (RequestMiddleware, runRequestMiddleware), Response (BodyResponse), ResponseBody (ResponseBody, responseBody, responseContentType, responseDatabaseOperations, responseLogEntries, responseObservabilityAttributes, responseStatus), RouteExecutionPolicy (RouteExecutionPolicy, routeExecutionConcurrencyLimit), RouteRequest (RouteRequest, requestContext, requestRoute), application, mkRequestConcurrencyLimit, renderResponse, runRequestMiddlewarePipeline, unboundedRouteExecutionPolicy)
import HarchWeb.Action qualified as Action ()
import HarchWeb.Database qualified as Database ()
import HarchWeb.Markup.Unsafe qualified as MarkupUnsafe ()
import HarchWeb.Observability qualified as Observability ()
import HarchWeb.Security qualified as Security ()
import Network.HTTP.Client qualified as HttpClient ()
import Network.HTTP.Types qualified as Http (status200, status202, status401)
import Network.Socket qualified as Socket ()
import Network.Socket.ByteString qualified as SocketByteString ()
import Network.Wai qualified as Wai (Request (pathInfo, rawPathInfo), defaultRequest)
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
import Unit.HarchWeb.TestSupport (TestContext (requestLanguage), TestRoute (DataRoute), defaultContext, sampleApplication, spanishContext)

spec = do
  describe "application" $ do
    it "preserves the supplied application description" $
      appName (application sampleApplication) `shouldBe` "sample"

    it "can render non-page responses for future API routes" $
      renderResponse sampleApplication (RouteRequest {requestRoute = DataRoute, requestContext = defaultContext})
        `shouldReturn` BodyResponse ResponseBody {responseStatus = Http.status202, responseContentType = "application/json", responseBody = "{\"route\":\"data\"}", responseObservabilityAttributes = [], responseLogEntries = [], responseDatabaseOperations = []}

    it "renders a direct route with WAI's empty request rather than ambient transport state" $ do
      observedRequest <- newIORef Nothing
      let applicationWithRequestProbe =
            sampleApplication
              { renderRequestResponse = \request _ -> do
                  writeIORef observedRequest (Just request)
                  pure (BodyResponse (ResponseBody Http.status200 "text/plain" "probe" [] [] []))
              }
      renderResponse applicationWithRequestProbe (RouteRequest {requestRoute = DataRoute, requestContext = defaultContext})
        `shouldReturn` BodyResponse (ResponseBody Http.status200 "text/plain" "probe" [] [] [])
      capturedRequest <- readIORef observedRequest
      fmap Wai.rawPathInfo capturedRequest `shouldBe` Just ""

    it "keeps route execution policy values explicit and unbounded by default" $ do
      let boundedPolicy = RouteExecutionPolicy (mkRequestConcurrencyLimit 1)
          differentPolicy = RouteExecutionPolicy (mkRequestConcurrencyLimit 2)
      expectAll
        ( (routeExecutionConcurrencyLimit unboundedRouteExecutionPolicy `shouldBe` Nothing)
            :| [ boundedPolicy == boundedPolicy `shouldBe` True,
                 boundedPolicy /= differentPolicy `shouldBe` True,
                 show boundedPolicy `shouldBe` "RouteExecutionPolicy {routeExecutionConcurrencyLimit = Just (RequestConcurrencyLimit 1)}",
                 show [boundedPolicy] `shouldBe` "[RouteExecutionPolicy {routeExecutionConcurrencyLimit = Just (RequestConcurrencyLimit 1)}]"
               ]
        )

  describe "runRequestMiddlewarePipeline" $ do
    it "runs in declaration order, carries context forward, and stops after a halt" $ do
      visitedMiddleware <- newIORef ([] :: [Text])
      let responseBodyValue = ResponseBody {responseStatus = Http.status401, responseContentType = "text/plain; charset=utf-8", responseBody = "Sign in required", responseObservabilityAttributes = [], responseLogEntries = [], responseDatabaseOperations = []}
          continuedResult = ContinueMiddleware spanishContext
          haltedResult = HaltMiddleware spanishContext responseBodyValue
          enrichMiddleware =
            RequestMiddleware $ \request requestContext -> do
              Wai.pathInfo request `shouldBe` []
              modifyIORef' visitedMiddleware (<> ["enrich"])
              pure (ContinueMiddleware requestContext {requestLanguage = "es"})
          haltMiddleware =
            RequestMiddleware $ \_ requestContext -> do
              modifyIORef' visitedMiddleware (<> ["halt"])
              pure (HaltMiddleware requestContext responseBodyValue)
          skippedMiddleware =
            RequestMiddleware $ \_ requestContext -> do
              modifyIORef' visitedMiddleware (<> ["skipped"])
              pure (ContinueMiddleware requestContext)
      runRequestMiddleware enrichMiddleware Wai.defaultRequest defaultContext
        `shouldReturn` continuedResult
      runRequestMiddlewarePipeline [] Wai.defaultRequest defaultContext
        `shouldReturn` ContinueMiddleware defaultContext
      runRequestMiddlewarePipeline [enrichMiddleware, haltMiddleware, skippedMiddleware] Wai.defaultRequest defaultContext
        `shouldReturn` haltedResult
      readIORef visitedMiddleware `shouldReturn` ["enrich", "enrich", "halt"]
      continuedResult == haltedResult `shouldBe` False
      continuedResult /= haltedResult `shouldBe` True
      show continuedResult `shouldBe` "ContinueMiddleware (TestContext {requestLanguage = \"es\", testContextPathPrefix = \"\"})"
      show haltedResult `shouldBe` "HaltMiddleware (TestContext {requestLanguage = \"es\", testContextPathPrefix = \"\"}) (ResponseBody {responseStatus = Status {statusCode = 401, statusMessage = \"Unauthorized\"}, responseContentType = \"text/plain; charset=utf-8\", responseBody = \"Sign in required\", responseObservabilityAttributes = [], responseLogEntries = [], responseDatabaseOperations = []})"
      show [continuedResult, haltedResult] `shouldBe` "[ContinueMiddleware (TestContext {requestLanguage = \"es\", testContextPathPrefix = \"\"}),HaltMiddleware (TestContext {requestLanguage = \"es\", testContextPathPrefix = \"\"}) (ResponseBody {responseStatus = Status {statusCode = 401, statusMessage = \"Unauthorized\"}, responseContentType = \"text/plain; charset=utf-8\", responseBody = \"Sign in required\", responseObservabilityAttributes = [], responseLogEntries = [], responseDatabaseOperations = []})]"
      showList [continuedResult, haltedResult] "" `shouldBe` "[ContinueMiddleware (TestContext {requestLanguage = \"es\", testContextPathPrefix = \"\"}),HaltMiddleware (TestContext {requestLanguage = \"es\", testContextPathPrefix = \"\"}) (ResponseBody {responseStatus = Status {statusCode = 401, statusMessage = \"Unauthorized\"}, responseContentType = \"text/plain; charset=utf-8\", responseBody = \"Sign in required\", responseObservabilityAttributes = [], responseLogEntries = [], responseDatabaseOperations = []})]"
