{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# SPEC #-}

import Control.Exception (ErrorCall, evaluate, try)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.IORef (newIORef)
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import TestCore.Wai (nextRequestBodyChunk, performWaiRequest, readResponseBody, readResponseBodyWithFlushCount, requiredWaiResponseOrDie, waiRequest)

spec = do
  describe "waiRequest" $ do
    it "renders the root path for no segments" $
      Wai.rawPathInfo (waiRequest []) `shouldBe` "/"

    it "joins segments into a slash-separated path and keeps pathInfo intact" $ do
      let request = waiRequest ["assets", "app.js"]
      Wai.rawPathInfo request `shouldBe` "/assets/app.js"
      Wai.pathInfo request `shouldBe` ["assets", "app.js"]

  describe "performWaiRequest and readResponseBody" $ do
    it "runs a built application against a request and reads back its response body" $ do
      response <- performWaiRequest (pure staticTextApplication) (waiRequest ["ping"])
      Wai.responseStatus response `shouldBe` Http.status200
      body <- readResponseBody response
      body `shouldBe` "/ping"

    it "reads a response whose streaming body flushes between chunks" $ do
      response <- performWaiRequest (pure flushingApplication) (waiRequest [])
      (body, flushCount) <- readResponseBodyWithFlushCount response
      body `shouldBe` "first-second"
      flushCount `shouldBe` 1

  describe "requiredWaiResponseOrDie" $ do
    it "unwraps a present response" $
      Wai.responseStatus (requiredWaiResponseOrDie (Just (Wai.responseLBS Http.status200 [] "ok")))
        `shouldBe` Http.status200

    it "errors when the application under test never responded" $ do
      result <- try (evaluate (requiredWaiResponseOrDie Nothing))
      case result of
        Left (exception :: ErrorCall) -> show exception `shouldContain'` "expected WAI application to produce a response"
        Right _ -> expectationFailure "expected requiredWaiResponseOrDie Nothing to throw"

  describe "nextRequestBodyChunk" $
    it "pops queued chunks in order and then reports exhaustion as empty" $ do
      chunksReference <- newIORef ["first", "second"]
      firstChunk <- nextRequestBodyChunk chunksReference
      secondChunk <- nextRequestBodyChunk chunksReference
      exhaustedChunk <- nextRequestBodyChunk chunksReference
      firstChunk `shouldBe` "first"
      secondChunk `shouldBe` "second"
      exhaustedChunk `shouldBe` ""

staticTextApplication :: Wai.Application
staticTextApplication request respond =
  respond (Wai.responseLBS Http.status200 [] (LazyByteString.fromStrict (Wai.rawPathInfo request)))

flushingApplication :: Wai.Application
flushingApplication _request respond =
  respond
    ( Wai.responseStream Http.status200 [] $ \sendChunk flush -> do
        sendChunk "first-"
        flush
        sendChunk "second"
    )
