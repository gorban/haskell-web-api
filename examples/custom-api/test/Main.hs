{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import App.Api.Declarative
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LazyByteString
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import HarchWeb.Site qualified as Site
import Network.HTTP.Types qualified as HttpTypes
import Network.Wai qualified as Wai
import Network.Wai.Internal qualified as WaiInternal
import Test.Hspec

performWaiRequest :: Wai.Application -> Wai.Request -> IO Wai.Response
performWaiRequest webApplication request = do
  responseReference <- newIORef Nothing
  _ <- webApplication request (\response -> writeIORef responseReference (Just response) >> pure WaiInternal.ResponseReceived)
  maybeResponse <- readIORef responseReference
  pure (fromMaybe (error "expected WAI application to produce a response") maybeResponse)

readResponseBody :: Wai.Response -> IO ByteString.ByteString
readResponseBody response = do
  let (_, _, withStreamingBody) = Wai.responseToStream response
  chunksReference <- newIORef []
  withStreamingBody $ \streamingBody ->
    streamingBody
      (\builder -> atomicModifyIORef' chunksReference (\chunks -> (chunks <> [Builder.toLazyByteString builder], ())))
      (pure ())
  chunks <- readIORef chunksReference
  pure (LazyByteString.toStrict (LazyByteString.concat chunks))

jsonRequest :: HttpTypes.Method -> ByteString.ByteString -> ByteString.ByteString -> IO Wai.Request
jsonRequest requestMethod requestPath bodyBytes = jsonRequestChunks requestMethod requestPath [bodyBytes]

jsonRequestChunks :: HttpTypes.Method -> ByteString.ByteString -> [ByteString.ByteString] -> IO Wai.Request
jsonRequestChunks requestMethod requestPath bodyChunks = do
  bodyRef <- newIORef bodyChunks
  let readChunk = atomicModifyIORef' bodyRef (\case [] -> ([], ByteString.empty); chunk : rest -> (rest, chunk))
  pure
    ( Wai.setRequestBodyChunks
        readChunk
        Wai.defaultRequest
          { Wai.requestMethod = requestMethod,
            Wai.rawPathInfo = requestPath,
            Wai.requestHeaders = [(HttpTypes.hContentType, "application/json")]
          }
    )

main :: IO ()
main = do
  application <- declarativeApiApplication
  hspec $ mainSpec application

mainSpec :: Wai.Application -> Spec
mainSpec application = describe "Unit.App.Api.Declarative" $ do
  it "exposes an API-only site composition root with its declared name and empty context" $ do
    Site.siteName declarativeApiSite `shouldBe` "custom-api"
    Site.siteDefaultRequestContext declarativeApiSite `shouldBe` ()

  describe "GET /api/greeting" $ do
    it "renders JSON by default" $ do
      response <- performWaiRequest application Wai.defaultRequest {Wai.requestMethod = "GET", Wai.rawPathInfo = "/api/greeting"}
      body <- readResponseBody response
      (Aeson.decodeStrict body :: Maybe Aeson.Value) `shouldBe` Just (Aeson.object ["greetingText" Aeson..= ("Hello, World!" :: Text)])

    it "renders the application-defined text/x-greeting media type when preferred by Accept" $ do
      response <-
        performWaiRequest
          application
          Wai.defaultRequest
            { Wai.requestMethod = "GET",
              Wai.rawPathInfo = "/api/greeting",
              Wai.requestHeaders = [(HttpTypes.hAccept, "text/x-greeting")]
            }
      body <- readResponseBody response
      body `shouldBe` "GREETING Hello, World!"

    it "leniently decodes an invalid UTF-8 Accept header instead of treating it as absent, so it matches no declared representation" $ do
      response <-
        performWaiRequest
          application
          Wai.defaultRequest
            { Wai.requestMethod = "GET",
              Wai.rawPathInfo = "/api/greeting",
              Wai.requestHeaders = [(HttpTypes.hAccept, "\255")]
            }
      Wai.responseStatus response `shouldBe` HttpTypes.status406

    it "rejects an explicit Accept header that excludes every declared representation" $ do
      response <-
        performWaiRequest
          application
          Wai.defaultRequest
            { Wai.requestMethod = "GET",
              Wai.rawPathInfo = "/api/greeting",
              Wai.requestHeaders = [(HttpTypes.hAccept, "text/plain")]
            }
      Wai.responseStatus response `shouldBe` HttpTypes.status406

  describe "POST /api/greeting" $ do
    it "decodes a JSON body and greets the requested name" $ do
      request <- jsonRequest "POST" "/api/greeting" "{\"requestedName\":\"Ada\"}"
      response <- performWaiRequest application request
      body <- readResponseBody response
      (Aeson.decodeStrict body :: Maybe Aeson.Value) `shouldBe` Just (Aeson.object ["greetingText" Aeson..= ("Hello, Ada!" :: Text)])

    it "reports an unsupported media type without a Content-Type header" $ do
      response <-
        performWaiRequest
          application
          Wai.defaultRequest {Wai.requestMethod = "POST", Wai.rawPathInfo = "/api/greeting"}
      Wai.responseStatus response `shouldBe` HttpTypes.status415

    it "reports an unsupported media type for invalid UTF-8 Content-Type" $ do
      request <- jsonRequest "POST" "/api/greeting" "{\"requestedName\":\"Ada\"}"
      response <- performWaiRequest application (request {Wai.requestHeaders = [(HttpTypes.hContentType, "\255")]})
      Wai.responseStatus response `shouldBe` HttpTypes.status415

    it "reports a malformed body for invalid JSON" $ do
      request <- jsonRequest "POST" "/api/greeting" "not json"
      response <- performWaiRequest application request
      Wai.responseStatus response `shouldBe` HttpTypes.status400

    it "reports an oversized body without decoding it" $ do
      request <- jsonRequest "POST" "/api/greeting" (ByteString.replicate 20000 65)
      response <- performWaiRequest application request
      Wai.responseStatus response `shouldBe` HttpTypes.status413

    it "rejects a chunked body when the next chunk exceeds its byte budget" $ do
      request <- jsonRequestChunks "POST" "/api/greeting" [ByteString.replicate (16 * 1024) 65, "B"]
      response <- performWaiRequest application request
      Wai.responseStatus response `shouldBe` HttpTypes.status413

  describe "POST /api/avatar" $ do
    let boundaryToken = "EXAMPLE-BOUNDARY" :: ByteString.ByteString
        multipartBody =
          "--"
            <> boundaryToken
            <> "\r\nContent-Disposition: form-data; name=\"avatar\"; filename=\"a.png\"\r\n\r\nfile bytes\r\n--"
            <> boundaryToken
            <> "--\r\n"

    it "reports how many parts a valid multipart upload contains" $ do
      request <- jsonRequest "POST" "/api/avatar" multipartBody
      let requestWithBoundary = request {Wai.requestHeaders = [(HttpTypes.hContentType, "multipart/form-data; boundary=" <> boundaryToken)]}
      response <- performWaiRequest application requestWithBoundary
      body <- readResponseBody response
      body `shouldBe` "1 part(s) received"

    it "accepts a quoted boundary parameter" $ do
      request <- jsonRequest "POST" "/api/avatar" multipartBody
      let requestWithBoundary =
            request
              { Wai.requestHeaders = [(HttpTypes.hContentType, "multipart/form-data; boundary=\"" <> boundaryToken <> "\"")]
              }
      response <- performWaiRequest application requestWithBoundary
      body <- readResponseBody response
      body `shouldBe` "1 part(s) received"

    it "reports a missing boundary as an invalid multipart body" $ do
      request <- jsonRequest "POST" "/api/avatar" multipartBody
      let requestWithoutBoundary = request {Wai.requestHeaders = [(HttpTypes.hContentType, "multipart/form-data")]}
      response <- performWaiRequest application requestWithoutBoundary
      body <- readResponseBody response
      body `shouldBe` "invalid multipart body"

    it "reports an invalid multipart body" $ do
      request <- jsonRequest "POST" "/api/avatar" "not multipart"
      let requestWithBoundary = request {Wai.requestHeaders = [(HttpTypes.hContentType, "multipart/form-data; boundary=" <> boundaryToken)]}
      response <- performWaiRequest application requestWithBoundary
      body <- readResponseBody response
      body `shouldBe` "invalid multipart body"

  describe "a path no declared endpoint owns" $ do
    it "renders the shared endpoint table's own 404, since the table is the sole path/method authority" $ do
      response <- performWaiRequest application Wai.defaultRequest {Wai.requestMethod = "GET", Wai.rawPathInfo = "/unrelated"}
      Wai.responseStatus response `shouldBe` HttpTypes.status404

  describe "DELETE /api/greeting" $
    it "reports 405 with the declared methods in Allow" $ do
      response <- performWaiRequest application Wai.defaultRequest {Wai.requestMethod = "DELETE", Wai.rawPathInfo = "/api/greeting"}
      body <- readResponseBody response
      Wai.responseStatus response `shouldBe` HttpTypes.status405
      lookup "Allow" (Wai.responseHeaders response) `shouldBe` Just "GET, POST, HEAD, OPTIONS"
      body `shouldBe` ""
