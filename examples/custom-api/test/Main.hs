{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import App.Api.Declarative
import Control.Exception (ErrorCall, displayException, evaluate, try)
import Control.Monad (forM_)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LazyByteString
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb qualified
import HarchWeb.Api (ApiBodyDecoder (apiBodyDecoderParse))
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

routeLocationForTest :: Text -> HarchWeb.RouteLocation
routeLocationForTest target =
  case HarchWeb.decodeRouteLocation (HarchWeb.requestTarget (TextEncoding.encodeUtf8 path) (TextEncoding.encodeUtf8 query)) of
    Left routeError -> error ("invalid test route target: " <> show routeError)
    Right location -> location
  where
    (path, query) = Text.breakOn "?" target

main :: IO ()
main = do
  application <- declarativeApiApplication
  hspec $ mainSpec application

mainSpec :: Wai.Application -> Spec
mainSpec application = describe "Unit.App.Api.Declarative" $ do
  it "exposes an API-only site composition root with its declared name and empty context" $ do
    Site.siteName declarativeApiSite `shouldBe` "custom-api"
    Site.siteDefaultRequestContext declarativeApiSite `shouldBe` ()

  it "uses an explicit anonymous endpoint declaration for the public API family" $ do
    routeRequest <-
      case HarchWeb.parseRoute (Site.siteRouteCodec declarativeApiSite) () (routeLocationForTest "/api/greeting") of
        HarchWeb.RouteParsed matchedRoute -> pure matchedRoute
        HarchWeb.RouteNotMatched -> expectationFailure "expected the declared greeting route" >> error "unreachable"
        HarchWeb.RouteMalformed routeError -> expectationFailure ("route was malformed: " <> show routeError) >> error "unreachable"
    let endpointMetadata = Site.routeMetadata (Site.siteRouteDefinition declarativeApiSite (HarchWeb.requestRoute routeRequest))
    HarchWeb.endpointNameText (HarchWeb.endpointName endpointMetadata) `shouldBe` "custom-api.endpoint"
    HarchWeb.routeTemplateText (HarchWeb.endpointRouteTemplate endpointMetadata) `shouldBe` "/api/{endpoint}"
    HarchWeb.endpointProtocol endpointMetadata `shouldBe` HarchWeb.ApiEndpoint
    HarchWeb.endpointAccess endpointMetadata `shouldBe` HarchWeb.AllowUnauthenticated

  it "rejects invalid authored endpoint metadata during startup construction" $ do
    endpointNameFailure <- try (evaluate (requiredEndpointName "invalid/name")) :: IO (Either ErrorCall HarchWeb.EndpointName)
    routeTemplateFailure <- try (evaluate (requiredRouteTemplate "not-a-route")) :: IO (Either ErrorCall HarchWeb.RouteTemplate)
    either displayException (const "unexpectedly accepted endpoint name") endpointNameFailure `shouldBe` "invalid custom-api endpoint name: InvalidEndpointName"
    either displayException (const "unexpectedly accepted route template") routeTemplateFailure `shouldBe` "invalid custom-api route template: InvalidRouteTemplate"

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
    it "retains its custom decoder's malformed-object diagnostic" $
      case apiBodyDecoderParse greetingRequestBodyDecoder "\"not an object\"" of
        Left parseError -> parseError `shouldSatisfy` Text.isInfixOf "GreetingRequest"
        Right _ -> expectationFailure "unexpectedly decoded malformed JSON"

    it "decodes a JSON body and greets the requested name" $ do
      request <- jsonRequest "POST" "/api/greeting" "{\"requestedName\":\"Ada\"}"
      response <- performWaiRequest application request
      body <- readResponseBody response
      (Aeson.decodeStrict body :: Maybe Aeson.Value) `shouldBe` Just (Aeson.object ["greetingText" Aeson..= ("Hello, Ada!" :: Text)])

    -- Tabled per docs/design-guidance.md's CN decision record: one act
    -- (build a request, perform it, check only its status code),
    -- differing only in how the request is built and the expected
    -- status. The body-decoding it above stays separate: it asserts on
    -- the decoded response body, not just the status.
    [ ("reports an unsupported media type without a Content-Type header", pure Wai.defaultRequest {Wai.requestMethod = "POST", Wai.rawPathInfo = "/api/greeting"}, HttpTypes.status415),
      ("reports an unsupported media type for invalid UTF-8 Content-Type", (\request -> request {Wai.requestHeaders = [(HttpTypes.hContentType, "\255")]}) <$> jsonRequest "POST" "/api/greeting" "{\"requestedName\":\"Ada\"}", HttpTypes.status415),
      ("reports a malformed body for invalid JSON", jsonRequest "POST" "/api/greeting" "not json", HttpTypes.status400),
      ("reports an oversized body without decoding it", jsonRequest "POST" "/api/greeting" (ByteString.replicate 20000 65), HttpTypes.status413),
      ("rejects a chunked body when the next chunk exceeds its byte budget", jsonRequestChunks "POST" "/api/greeting" [ByteString.replicate (16 * 1024) 65, "B"], HttpTypes.status413)
      ]
      `forM_` \(label, buildRequest, expectedStatus) ->
        it label $ do
          request <- buildRequest
          response <- performWaiRequest application request
          Wai.responseStatus response `shouldBe` expectedStatus

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
