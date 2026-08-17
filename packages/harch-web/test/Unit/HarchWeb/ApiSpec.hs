{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWeb.ApiSpec (spec) where

import Control.Exception (ErrorCall (..), evaluate)
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LazyByteString
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb qualified
import HarchWeb.Api
import HarchWeb.Api.Multipart
  ( MultipartConsumeError (..),
    MultipartScopedPart (..),
    defaultMultipartLimits,
    inMemoryMultipartStorage,
  )
import HarchWeb.Observability qualified as Observability
import HarchWeb.Routing (RouteRequest (..))
import HarchWeb.Server (ProtocolResponse (..), ProtocolResponseBody (..), Response (..))
import HarchWeb.Site (RouteDefinition (..))
import Network.HTTP.Types qualified as HttpTypes
import Network.Wai qualified as Wai
import Test.Hspec
import TestCore.CustomAssertions (expectAll)

testEndpointTable :: [SomeApiRouteEndpoint]
testEndpointTable =
  [ SomeApiRouteEndpoint (testEndpoint ApiGet (at "/api/status") "ReadStatus"),
    SomeApiRouteEndpoint (testEndpoint ApiPost (at "/api/status") "WriteStatus"),
    SomeApiRouteEndpoint (testEndpoint ApiGet (at "/api/second") "ReadSecond"),
    SomeApiRouteEndpoint streamEndpoint
  ]

testEndpoint :: ApiMethod -> ApiPath -> Text -> ApiRouteEndpoint () () () Text
testEndpoint method path responseText =
  apiRouteEndpointAt
    method
    path
    (pure ())
    ApiNoRequestBody
    (textResponseEncoder :| [])
    (const (pure (Right (apiResponse responseText))))
    (const (apiResponse "unreachable"))

streamEndpoint :: ApiRouteEndpoint () () () ()
streamEndpoint =
  apiRouteEndpointAt
    ApiGet
    (at "/api/stream")
    (pure ())
    ApiNoRequestBody
    (streamingResponseEncoder plainTextContentType streamResponse :| [])
    (const (pure (Right (apiResponse ()))))
    (const (apiResponse ()))
  where
    streamResponse _ write flush = write (Builder.byteString "streamed") >> flush

testMediaType :: Text -> ApiMediaType
testMediaType value = fromMaybe (error "expected test media type to be valid") (apiMediaType value)

testHeaderValue :: Text -> ApiHeaderValue
testHeaderValue value = fromMaybe (error "expected test header value to be valid") (apiHeaderValue value)

requestWithBody :: HttpTypes.RequestHeaders -> [ByteString.ByteString] -> IO Wai.Request
requestWithBody headers chunks = do
  chunksReference <- newIORef chunks
  pure
    ( Wai.setRequestBodyChunks
        (atomicModifyIORef' chunksReference takeNextChunk)
        (Wai.defaultRequest {Wai.requestHeaders = headers})
    )

takeNextChunk :: [ByteString.ByteString] -> ([ByteString.ByteString], ByteString.ByteString)
takeNextChunk remainingChunks =
  case remainingChunks of
    [] -> ([], "")
    nextChunk : laterChunks -> (laterChunks, nextChunk)

apiRouteResponseStatus :: Response route context -> HttpTypes.Status
apiRouteResponseStatus response =
  case response of
    ProtocolResponseResult protocolResponse -> protocolResponseStatus protocolResponse
    _ -> error "expected API route to render a protocol response"

apiRouteResponseBody :: Response route context -> ByteString.ByteString
apiRouteResponseBody response =
  case response of
    ProtocolResponseResult protocolResponse ->
      case protocolResponseBody protocolResponse of
        ProtocolResponseBytes bodyBytes -> bodyBytes
        ProtocolResponseStream _ -> error "expected API route to render strict protocol bytes"
    _ -> error "expected API route to render a protocol response"

apiRouteResponseHeaders :: Response route context -> HttpTypes.ResponseHeaders
apiRouteResponseHeaders response =
  protocolResponseHeaders (apiRouteProtocolResponse response)

apiRouteProtocolResponse :: Response route context -> ProtocolResponse
apiRouteProtocolResponse response =
  case response of
    ProtocolResponseResult protocolResponse -> protocolResponse
    _ -> error "expected API route to render a protocol response"

apiRouteResponseStream :: Response route context -> Wai.StreamingBody
apiRouteResponseStream response =
  case response of
    ProtocolResponseResult protocolResponse ->
      case protocolResponseBody protocolResponse of
        ProtocolResponseBytes _ -> error "expected API route to render a protocol stream"
        ProtocolResponseStream stream -> stream
    _ -> error "expected API route to render a protocol response"

strictEncodedResponseBytes :: ApiEncodedResponseBody -> ByteString.ByteString
strictEncodedResponseBytes encodedResponse =
  case encodedResponse of
    ApiEncodedResponseBytes bodyBytes -> bodyBytes
    ApiEncodedResponseStream _ -> error "expected a strict encoded response"

runApiRoute :: ApiRouteEndpoint fields body domainFailure response -> Wai.Request -> IO (Response () ())
runApiRoute endpoint request =
  routeResponse (apiRouteDefinition endpoint) request (RouteRequest () ())

runApiRouteEndpointGroup :: [SomeApiRouteEndpoint] -> ApiPath -> Wai.Request -> IO (Response ApiPath ())
runApiRouteEndpointGroup endpoints declaredPath request =
  routeResponse (apiRouteEndpointFamilyDefinition endpoints declaredPath) request (RouteRequest declaredPath ())

-- | Pull every chunk from a streaming request body one at a time,
-- concatenating them, until the body ends or a pull reports the running
-- total exceeded its declared budget.
pullAllStreamingChunks :: ApiStreamingRequest -> ByteString.ByteString -> IO (Either () (ApiResponse Text))
pullAllStreamingChunks streamingRequest accumulated = do
  chunkResult <- pullApiStreamingRequestChunk streamingRequest
  case chunkResult of
    Left RequestBodyLimitExceeded -> pure (Left ())
    Right chunk
      | ByteString.null chunk -> pure (Right (apiResponse (TextEncoding.decodeUtf8 accumulated)))
      | otherwise -> pullAllStreamingChunks streamingRequest (accumulated <> chunk)

spec :: Spec
spec =
  describe "HarchWeb.Api" $ do
    describe "apiRouteEndpointFamilyCodec and apiRouteEndpointFamilyDefinition" $ do
      it "parses a declared path into its ApiPath route identity" $
        HarchWeb.parseRoute (apiRouteEndpointFamilyCodec testEndpointTable) () "/api/status"
          `shouldBe` Just (RouteRequest (at "/api/status") ())

      it "reports no match for an undeclared path" $
        HarchWeb.parseRoute (apiRouteEndpointFamilyCodec testEndpointTable) () "/api/unknown" `shouldBe` Nothing

      it "renders the route identity back to its declared path" $
        HarchWeb.renderRoute (apiRouteEndpointFamilyCodec testEndpointTable) (RouteRequest (at "/api/status") ())
          `shouldBe` "/api/status"

      it "falls back to an empty path for the family's own not-found request" $
        HarchWeb.requestRoute (HarchWeb.notFoundRequest (apiRouteEndpointFamilyCodec testEndpointTable) ())
          `shouldBe` at ""

      it "renders the family's own not-found route as an ordinary 404 with no headers or body, instead of raising, when used standalone with no catch-all family" $ do
        notFoundResponse <-
          runApiRouteEndpointGroup
            testEndpointTable
            (HarchWeb.requestRoute (HarchWeb.notFoundRequest (apiRouteEndpointFamilyCodec testEndpointTable) ()))
            (Wai.defaultRequest {Wai.requestMethod = "GET", Wai.rawPathInfo = "/api/unknown"})
        expectAll
          ( (apiRouteResponseStatus notFoundResponse `shouldBe` HttpTypes.status404)
              :| [ apiRouteResponseHeaders notFoundResponse `shouldBe` [],
                   apiRouteResponseBody notFoundResponse `shouldBe` ""
                 ]
          )

      it "reports every declared method at a path, deduplicated" $
        HarchWeb.routeMethods (apiRouteEndpointFamilyCodec testEndpointTable) (at "/api/status")
          `shouldBe` HarchWeb.routeMethodPolicy [HarchWeb.RouteGet, HarchWeb.RoutePost]

      it "reports no methods for a path with no declared endpoint" $
        HarchWeb.routeMethods (apiRouteEndpointFamilyCodec testEndpointTable) (at "/api/unknown") `shouldBe` HarchWeb.RouteHidden

      it "agrees with the codec's routeMethods so the shared dispatcher and the definition never diverge" $
        HarchWeb.routeMethodPolicy (routeMethods (apiRouteEndpointFamilyDefinition testEndpointTable (at "/api/status")))
          `shouldBe` HarchWeb.routeMethods (apiRouteEndpointFamilyCodec testEndpointTable) (at "/api/status")

      it "keeps the definition's navigation label unset like the single-endpoint adapter" $
        routeNavigationLabel (apiRouteEndpointFamilyDefinition testEndpointTable (at "/api/status")) `shouldBe` Nothing

      it "runs the one endpoint matching the request's real method" $ do
        getResponse <- runApiRouteEndpointGroup testEndpointTable (at "/api/status") (Wai.defaultRequest {Wai.requestMethod = "GET", Wai.rawPathInfo = "/api/status"})
        postResponse <- runApiRouteEndpointGroup testEndpointTable (at "/api/status") (Wai.defaultRequest {Wai.requestMethod = "POST", Wai.rawPathInfo = "/api/status"})
        expectAll
          ( (apiRouteResponseBody getResponse `shouldBe` "ReadStatus")
              :| [apiRouteResponseBody postResponse `shouldBe` "WriteStatus"]
          )

      it "resolves HEAD to the declared GET endpoint's handler, same as the shared dispatcher's HEAD synthesis" $ do
        headResponse <- runApiRouteEndpointGroup testEndpointTable (at "/api/status") (Wai.defaultRequest {Wai.requestMethod = "HEAD", Wai.rawPathInfo = "/api/status"})
        apiRouteResponseBody headResponse `shouldBe` "ReadStatus"

      it "raises immediately when no endpoint is declared at the given path" $
        evaluate (matchedApiRouteEndpointOrDie testEndpointTable "/api/missing" "GET" `seq` ())
          `shouldThrow` \case
            ErrorCall message -> "no endpoint declared at /api/missing" `isInfixOf` message

      it "raises immediately when the given method is not declared at the given path" $
        evaluate (matchedApiRouteEndpointOrDie testEndpointTable "/api/status" "DELETE" `seq` ())
          `shouldThrow` \case
            ErrorCall message -> "DELETE is not declared at /api/status" `isInfixOf` message

      it "raises immediately when HEAD is requested for a path with no declared GET" $
        let postOnlyTable = [SomeApiRouteEndpoint (testEndpoint ApiPost (at "/api/post-only") "WriteOnly")]
         in runApiRouteEndpointGroup postOnlyTable (at "/api/post-only") (Wai.defaultRequest {Wai.requestMethod = "HEAD", Wai.rawPathInfo = "/api/post-only"})
              `shouldThrow` \case
                ErrorCall message -> "HEAD is not declared at /api/post-only" `isInfixOf` message

      it "leniently decodes a malformed, non-UTF-8 request method before failing to match it to a declared endpoint" $
        runApiRouteEndpointGroup testEndpointTable (at "/api/status") (Wai.defaultRequest {Wai.requestMethod = "\xFF", Wai.rawPathInfo = "/api/status"})
          `shouldThrow` \case
            ErrorCall message -> "is not declared at /api/status" `isInfixOf` message

      it "retains comparable, printable endpoint values" $
        let methods = [ApiGet, ApiPost, ApiPut, ApiPatch, ApiDelete]
            paths = [at "/x", at "/y"]
            httpResponses = [ApiHttpResponse HttpTypes.status200 [] Nothing, ApiHttpResponse HttpTypes.status404 [] Nothing]
         in expectAll
              ( (sum [fromEnum (left == right) | left <- methods, right <- methods] `shouldBe` length methods)
                  :| [ sum [fromEnum (left /= right) | left <- methods, right <- methods] `shouldBe` length methods * (length methods - 1),
                       sum [length (show methodValue) + length (showList [methodValue] "") | methodValue <- methods] `shouldSatisfy` (> 0),
                       sum [fromEnum (left == right) | left <- paths, right <- paths] `shouldBe` length paths,
                       sum [fromEnum (left /= right) | left <- paths, right <- paths] `shouldBe` length paths * (length paths - 1),
                       sum [length (show pathValue) + length (showList [pathValue] "") | pathValue <- paths] `shouldSatisfy` (> 0),
                       sum [fromEnum (left == right) | left <- httpResponses, right <- httpResponses] `shouldBe` length httpResponses,
                       sum [fromEnum (left /= right) | left <- httpResponses, right <- httpResponses] `shouldBe` length httpResponses * (length httpResponses - 1),
                       sum [length (show responseValue) + length (showList [responseValue] "") | responseValue <- httpResponses] `shouldSatisfy` (> 0)
                     ]
              )

      it "participates in one shared dispatch authority when combined with another route family" $ do
        let pageCodec :: HarchWeb.RouteCodec Text ()
            pageCodec =
              HarchWeb.RouteCodec
                { HarchWeb.parseRoute = \requestContextValue path -> if path == "/home" then Just (RouteRequest "home" requestContextValue) else Nothing,
                  HarchWeb.renderRoute = const "/home",
                  HarchWeb.notFoundRequest = RouteRequest "not-found",
                  HarchWeb.routeMethods = const (HarchWeb.routeMethodPolicy [HarchWeb.RouteGet])
                }
            combined = HarchWeb.combineRouteCodecs (apiRouteEndpointFamilyCodec testEndpointTable) pageCodec
        expectAll
          ( ( HarchWeb.matchRouteMethod combined () (HarchWeb.requestMethod "GET") (HarchWeb.requestPath "/home")
                `shouldBe` HarchWeb.RouteMatched (RouteRequest (HarchWeb.RouteFamilyB "home") ())
            )
              :| [ HarchWeb.matchRouteMethod combined () (HarchWeb.requestMethod "GET") (HarchWeb.requestPath "/api/status")
                     `shouldBe` HarchWeb.RouteMatched (RouteRequest (HarchWeb.RouteFamilyA (at "/api/status")) ()),
                   HarchWeb.matchRouteMethod combined () (HarchWeb.requestMethod "DELETE") (HarchWeb.requestPath "/api/status")
                     `shouldBe` HarchWeb.RouteMethodNotAllowed (RouteRequest (HarchWeb.RouteFamilyA (at "/api/status")) ()) (HarchWeb.RouteGet :| [HarchWeb.RoutePost]),
                   HarchWeb.matchRouteMethod combined () (HarchWeb.requestMethod "GET") (HarchWeb.requestPath "/missing") `shouldBe` HarchWeb.RouteNotFound (RouteRequest (HarchWeb.RouteFamilyB "not-found") ())
                 ]
          )

    it "keeps a route-table endpoint's compatibility path explicit" $
      apiRouteEndpointPath
        ( apiRouteEndpoint
            ApiGet
            (pure ())
            ApiNoRequestBody
            (textResponseEncoder :| [])
            (const (pure (Right (apiResponse "unused"))))
            (const (apiResponse "unreachable"))
        )
        `shouldBe` at ""

    describe "apiHeaderValue" $ do
      it "accepts a value without control characters or surrounding whitespace" $
        let acceptedValue = testHeaderValue "no-store"
         in expectAll
              ( (acceptedValue `shouldBe` testHeaderValue "no-store")
                  :| [ acceptedValue `shouldNotBe` testHeaderValue "must-revalidate",
                       apiHeaderValueText acceptedValue `shouldBe` "no-store",
                       length (show acceptedValue) + length (showList [acceptedValue] "") `shouldSatisfy` (> 0)
                     ]
              )

      it "rejects CR, LF, NUL, and surrounding whitespace" $
        expectAll
          ( (apiHeaderValue "bad\rvalue" `shouldBe` Nothing)
              :| [ apiHeaderValue "bad\nvalue" `shouldBe` Nothing,
                   apiHeaderValue "bad\NULvalue" `shouldBe` Nothing,
                   apiHeaderValue " padded" `shouldBe` Nothing,
                   apiHeaderValue "padded " `shouldBe` Nothing
                 ]
          )

    describe "apiResponseBodyToProtocolResponse" $ do
      it "converts status, headers, and body into the server protocol response" $
        apiResponseBodyToProtocolResponse
          ((apiTextResponse "hello") {apiResponseStatus = HttpTypes.status201, apiResponseHeaders = [("X-Example", testHeaderValue "present")]})
          `shouldBe` ProtocolResponse
            { protocolResponseStatus = HttpTypes.status201,
              protocolResponseHeaders = [("Content-Type", "text/plain; charset=utf-8"), ("X-Example", "present")],
              protocolResponseBody = ProtocolResponseBytes "hello",
              protocolResponseObservabilityAttributes = [],
              protocolResponseLogEntries = []
            }

    it "renders every declared method to its RFC 9110 token" $
      expectAll
        ( (apiMethodText ApiGet `shouldBe` "GET")
            :| [ apiMethodText ApiPost `shouldBe` "POST",
                 apiMethodText ApiPut `shouldBe` "PUT",
                 apiMethodText ApiPatch `shouldBe` "PATCH",
                 apiMethodText ApiDelete `shouldBe` "DELETE"
               ]
        )

    describe "apiRequestDataFromWaiRequest" $ do
      it "extracts query parameters and headers from a WAI request" $
        let request =
              Wai.defaultRequest
                { Wai.queryString = [("q", Just "hello")],
                  Wai.requestHeaders = [("X-Custom", "value")]
                }
         in apiRequestDataFromWaiRequest request
              `shouldBe` ApiRequestData
                { apiRequestQueryParameters = [("q", "hello")],
                  apiRequestHeaders = [(apiHeaderName "x-custom", "value")],
                  apiRequestCookies = [],
                  apiRequestFormFields = []
                }

      it "decodes a flag-style query parameter with no value as empty rather than dropping it" $
        let request = Wai.defaultRequest {Wai.queryString = [("flag", Nothing)]}
         in apiRequestDataFromWaiRequest request
              `shouldBe` ApiRequestData {apiRequestQueryParameters = [("flag", "")], apiRequestHeaders = [], apiRequestCookies = [], apiRequestFormFields = []}

      it "decodes invalid UTF-8 in a query value leniently rather than failing" $
        let request = Wai.defaultRequest {Wai.queryString = [("q", Just "bad\xFF")]}
         in apiRequestDataFromWaiRequest request
              `shouldBe` ApiRequestData {apiRequestQueryParameters = [("q", "bad\65533")], apiRequestHeaders = [], apiRequestCookies = [], apiRequestFormFields = []}

      it "extracts case-sensitive cookie pairs from every Cookie header" $
        let request =
              Wai.defaultRequest
                { Wai.requestHeaders =
                    [ ("Cookie", "session=first; theme=dark"),
                      ("cOoKiE", "session=second; malformed; bad name=ignored; Empty=")
                    ]
                }
         in apiRequestDataFromWaiRequest request
              `shouldBe` ApiRequestData
                { apiRequestQueryParameters = [],
                  apiRequestHeaders = [(apiHeaderName "cookie", "session=first; theme=dark"), (apiHeaderName "cookie", "session=second; malformed; bad name=ignored; Empty=")],
                  apiRequestCookies = [("session", "first"), ("theme", "dark"), ("session", "second"), ("Empty", "")],
                  apiRequestFormFields = []
                }

    describe "apiRouteDefinition" $ do
      let requiredQuery = requiredField (queryField "q" apiTextValue)
          successfulEndpoint =
            apiRouteEndpoint
              ApiPost
              requiredQuery
              ApiNoRequestBody
              (textResponseEncoder :| [])
              ( \endpointRequest ->
                  case apiEndpointRequestBody endpointRequest of
                    () -> pure (Right (apiResponse (apiEndpointRequestFields endpointRequest)))
              )
              (\() -> apiResponse "unreachable")
          domainFailureEndpoint =
            apiRouteEndpoint
              ApiGet
              (pure ())
              ApiNoRequestBody
              (textResponseEncoder :| [])
              (const (pure (Left ())))
              (\() -> (apiResponse "domain failure") {apiEndpointResponseStatus = HttpTypes.status422})

      it "declares its one method in the shared route table" $
        expectAll
          ( (routeNavigationLabel (apiRouteDefinition successfulEndpoint) `shouldBe` Nothing)
              :| [ routeMethods (apiRouteDefinition successfulEndpoint) `shouldBe` [HarchWeb.RoutePost],
                   routeMethods (apiRouteDefinition domainFailureEndpoint) `shouldBe` [HarchWeb.RouteGet],
                   routeMethods (apiRouteDefinition (apiRouteEndpoint ApiPut (pure ()) ApiNoRequestBody (textResponseEncoder :| []) (const (pure (Right (apiResponse "")))) (\() -> apiResponse ""))) `shouldBe` [HarchWeb.RoutePut],
                   routeMethods (apiRouteDefinition (apiRouteEndpoint ApiPatch (pure ()) ApiNoRequestBody (textResponseEncoder :| []) (const (pure (Right (apiResponse "")))) (\() -> apiResponse ""))) `shouldBe` [HarchWeb.RoutePatch],
                   routeMethods (apiRouteDefinition (apiRouteEndpoint ApiDelete (pure ()) ApiNoRequestBody (textResponseEncoder :| []) (const (pure (Right (apiResponse "")))) (\() -> apiResponse ""))) `shouldBe` [HarchWeb.RouteDelete]
                 ]
          )

      it "rejects invalid fields before the handler and does not consume a body" $ do
        chunksReference <- newIORef ["not consumed"]
        let request = Wai.setRequestBodyChunks (atomicModifyIORef' chunksReference takeNextChunk) Wai.defaultRequest
        response <- runApiRoute successfulEndpoint request
        remainingChunks <- readIORef chunksReference
        expectAll
          ( (apiRouteResponseStatus response `shouldBe` HttpTypes.status400)
              :| [ apiRouteResponseBody response `shouldBe` "API request fields were rejected.",
                   remainingChunks `shouldBe` ["not consumed"]
                 ]
          )

      it "runs a no-body endpoint after typed field decoding" $ do
        response <- runApiRoute successfulEndpoint (Wai.defaultRequest {Wai.queryString = [("q", Just "accepted")]})
        expectAll
          ( (apiRouteResponseStatus response `shouldBe` HttpTypes.status200)
              :| [ apiRouteResponseBody response `shouldBe` "accepted",
                   lookup "Content-Type" (apiRouteResponseHeaders response) `shouldBe` Just "text/plain; charset=utf-8",
                   -- A single-encoder endpoint's response still depends on
                   -- Accept (an unsatisfiable one would 406 instead), so
                   -- Vary: Accept is present even with only one declared
                   -- representation.
                   lookup "Vary" (apiRouteResponseHeaders response) `shouldBe` Just "Accept"
                 ]
          )

      it "interprets expected domain failures at the endpoint boundary" $ do
        response <- runApiRoute domainFailureEndpoint Wai.defaultRequest
        expectAll
          ( (apiRouteResponseStatus response `shouldBe` HttpTypes.status422)
              :| [apiRouteResponseBody response `shouldBe` "domain failure"]
          )

      it "maps bounded buffered-body failures without invoking the handler" $ do
        let bufferedEndpoint =
              apiRouteEndpoint
                ApiPost
                (pure ())
                (ApiBufferedRequestBody RejectMissingContentType 4 [textBodyDecoder])
                (textResponseEncoder :| [])
                (pure . Right . apiResponse . apiEndpointRequestBody)
                (\() -> apiResponse "unreachable")
        oversizedRequest <- requestWithBody [("Content-Type", "text/plain")] ["123", "45"]
        unsupportedRequest <- requestWithBody [("Content-Type", "application/json")] ["ok"]
        malformedRequest <- requestWithBody [("Content-Type", "text/plain")] ["bad\xFF"]
        acceptedRequest <- requestWithBody [("Content-Type", "text/plain")] ["ok"]
        oversizedResponse <- runApiRoute bufferedEndpoint oversizedRequest
        unsupportedResponse <- runApiRoute bufferedEndpoint unsupportedRequest
        malformedResponse <- runApiRoute bufferedEndpoint malformedRequest
        acceptedResponse <- runApiRoute bufferedEndpoint acceptedRequest
        expectAll
          ( (apiRouteResponseStatus oversizedResponse `shouldBe` HttpTypes.status413)
              :| [ apiRouteResponseStatus unsupportedResponse `shouldBe` HttpTypes.status415,
                   apiRouteResponseStatus malformedResponse `shouldBe` HttpTypes.status400,
                   apiRouteResponseStatus acceptedResponse `shouldBe` HttpTypes.status200,
                   apiRouteResponseBody acceptedResponse `shouldBe` "ok",
                   apiRouteResponseBody oversizedResponse `shouldBe` "API request body exceeds its declared limit.",
                   apiRouteResponseBody unsupportedResponse `shouldBe` "API request body has an unsupported media type.",
                   apiRouteResponseBody malformedResponse `shouldBe` "API request body is malformed."
                 ]
          )

      it "combines repeated Content-Type header lines per RFC 9110 instead of silently treating the request as having none" $ do
        -- Content-Type is not itself a list-valued field (unlike Accept), so
        -- RFC 9110's combine-with-commas rule turns two declared lines —
        -- even two identical, individually acceptable ones — into a value
        -- no single declared media type can match. That is the point: the
        -- previous behavior silently treated two lines as "Content-Type
        -- absent", which combined with an AssumeMediaType policy meant the
        -- body was parsed as an assumed type instead of the server noticing
        -- the client sent something malformed. 415 here is a strictly safer
        -- outcome than a silent 200 that guessed.
        let assumedEndpoint =
              apiRouteEndpoint
                ApiPost
                (pure ())
                (ApiBufferedRequestBody (AssumeMediaType plainTextMediaType) 4 [textBodyDecoder])
                (textResponseEncoder :| [])
                (pure . Right . apiResponse . apiEndpointRequestBody)
                (\() -> apiResponse "unreachable")
        duplicateHeaderRequest <- requestWithBody [("Content-Type", "text/plain"), ("Content-Type", "text/plain")] ["ok"]
        singleHeaderRequest <- requestWithBody [("Content-Type", "text/plain")] ["ok"]
        duplicateResponse <- runApiRoute assumedEndpoint duplicateHeaderRequest
        singleResponse <- runApiRoute assumedEndpoint singleHeaderRequest
        expectAll
          ( (apiRouteResponseStatus duplicateResponse `shouldBe` HttpTypes.status415)
              :| [ apiRouteResponseStatus singleResponse `shouldBe` HttpTypes.status200,
                   apiRouteResponseBody singleResponse `shouldBe` "ok"
                 ]
          )

      it "combines repeated Accept header lines per RFC 9110 instead of falling back to the first declared representation" $ do
        let negotiatedEndpoint =
              apiRouteEndpoint
                ApiGet
                (pure ())
                ApiNoRequestBody
                (textResponseEncoder :| [jsonResponseEncoder])
                (\_ -> pure (Right (apiResponse "hello")))
                (\() -> apiResponse "unreachable")
        let duplicateAcceptRequest = Wai.defaultRequest {Wai.requestHeaders = [("Accept", "application/json"), ("Accept", "application/json")]}
        response <- runApiRoute negotiatedEndpoint duplicateAcceptRequest
        expectAll
          ( (apiRouteResponseStatus response `shouldBe` HttpTypes.status200)
              :| [ apiRouteResponseBody response `shouldBe` "\"hello\"",
                   lookup "Content-Type" (apiRouteResponseHeaders response) `shouldBe` Just "application/json; charset=utf-8"
                 ]
          )

      it "uses an explicitly assumed media type only when the endpoint opts in" $ do
        let assumedContentTypeEndpoint =
              apiRouteEndpoint
                ApiPost
                (pure ())
                (ApiBufferedRequestBody (AssumeMediaType plainTextMediaType) 4 [textBodyDecoder])
                (textResponseEncoder :| [])
                (pure . Right . apiResponse . apiEndpointRequestBody)
                (\() -> apiResponse "unreachable")
        request <- requestWithBody [] ["ok"]
        response <- runApiRoute assumedContentTypeEndpoint request
        expectAll
          ( (apiRouteResponseStatus response `shouldBe` HttpTypes.status200)
              :| [apiRouteResponseBody response `shouldBe` "ok"]
          )

      it "decodes one bounded URL-encoded form body before applying its declared form fields" $ do
        handlerCalls <- newIORef (0 :: Int)
        let formEndpoint =
              apiRouteEndpoint
                ApiPost
                ( (,)
                    <$> requiredField (queryField "source" apiTextValue)
                    <*> requiredField (formField "name" apiTextValue)
                )
                (ApiUrlEncodedFormRequestBody RejectMissingContentType 64 2)
                (textResponseEncoder :| [])
                ( \endpointRequest -> do
                    atomicModifyIORef' handlerCalls (\callCount -> (callCount + 1, ()))
                    let source = fst (apiEndpointRequestFields endpointRequest)
                        name = snd (apiEndpointRequestFields endpointRequest)
                        decodedForm = apiEndpointRequestBody endpointRequest
                    case apiFormFields decodedForm of
                      [("name", decodedName)] -> pure (Right (apiResponse (source <> ":" <> name <> ":" <> decodedName)))
                      _ -> pure (Left ())
                )
                (\() -> apiResponse "unreachable")
        acceptedRequest <-
          requestWithBody
            [("Content-Type", "Application/X-Www-Form-Urlencoded; charset=utf-8")]
            ["name=Ada+Lovelace"]
        let withQuery = acceptedRequest {Wai.queryString = [("source", Just "native-form")]}
        malformedRequest <- requestWithBody [("Content-Type", "application/x-www-form-urlencoded")] ["name=%ZZ"]
        duplicateRequest <- requestWithBody [("Content-Type", "application/x-www-form-urlencoded")] ["name=first&name=second"]
        acceptedResponse <- runApiRoute formEndpoint withQuery
        malformedResponse <- runApiRoute formEndpoint malformedRequest
        duplicateResponse <- runApiRoute formEndpoint duplicateRequest
        calls <- readIORef handlerCalls
        expectAll
          ( (apiRouteResponseStatus acceptedResponse `shouldBe` HttpTypes.status200)
              :| [ apiRouteResponseBody acceptedResponse `shouldBe` "native-form:Ada Lovelace:Ada Lovelace",
                   apiRouteResponseStatus malformedResponse `shouldBe` HttpTypes.status400,
                   apiRouteResponseStatus duplicateResponse `shouldBe` HttpTypes.status400,
                   apiRouteResponseBody malformedResponse `shouldBe` "API request body is malformed.",
                   apiRouteResponseBody duplicateResponse `shouldBe` "API request fields were rejected.",
                   calls `shouldBe` 1
                 ]
          )

      it "maps missing, oversized, ambiguous, and field-invalid form requests before the handler" $ do
        handlerCalls <- newIORef (0 :: Int)
        let formEndpoint =
              apiRouteEndpoint
                ApiPost
                (requiredField (formField "name" apiTextValue))
                (ApiUrlEncodedFormRequestBody RejectMissingContentType 64 1)
                (textResponseEncoder :| [])
                ( \endpointRequest -> do
                    atomicModifyIORef' handlerCalls (\callCount -> (callCount + 1, ()))
                    pure (Right (apiResponse (apiEndpointRequestFields endpointRequest)))
                )
                (\() -> apiResponse "unreachable")
        missingContentTypeRequest <- requestWithBody [] ["name"]
        oversizedRequest <- requestWithBody [("Content-Type", "application/x-www-form-urlencoded")] [ByteString.replicate 65 120]
        ambiguousContentTypeRequest <-
          requestWithBody
            [ ("Content-Type", "application/x-www-form-urlencoded"),
              ("content-type", "application/x-www-form-urlencoded")
            ]
            ["name"]
        invalidFieldRequest <- requestWithBody [("Content-Type", "application/x-www-form-urlencoded")] ["other=x"]
        missingContentTypeResponse <- runApiRoute formEndpoint missingContentTypeRequest
        oversizedResponse <- runApiRoute formEndpoint oversizedRequest
        ambiguousContentTypeResponse <- runApiRoute formEndpoint ambiguousContentTypeRequest
        invalidFieldResponse <- runApiRoute formEndpoint invalidFieldRequest
        calls <- readIORef handlerCalls
        expectAll
          ( (apiRouteResponseStatus missingContentTypeResponse `shouldBe` HttpTypes.status415)
              :| [ apiRouteResponseStatus oversizedResponse `shouldBe` HttpTypes.status413,
                   apiRouteResponseStatus ambiguousContentTypeResponse `shouldBe` HttpTypes.status415,
                   apiRouteResponseStatus invalidFieldResponse `shouldBe` HttpTypes.status400,
                   calls `shouldBe` 0,
                   apiRouteResponseBody missingContentTypeResponse `shouldBe` "API request body has an unsupported media type.",
                   apiRouteResponseBody oversizedResponse `shouldBe` "API request body exceeds its declared limit.",
                   apiRouteResponseBody ambiguousContentTypeResponse `shouldBe` "API request body has an unsupported media type.",
                   apiRouteResponseBody invalidFieldResponse `shouldBe` "API request fields were rejected."
                 ]
          )

      it "negotiates declared response encoders and adds Vary: Accept" $ do
        let negotiatedEndpoint =
              apiRouteEndpoint
                ApiGet
                (pure ())
                ApiNoRequestBody
                (jsonResponseEncoder :| [textResponseEncoder])
                ( \_ ->
                    pure
                      ( Right
                          ( (apiResponse "hello")
                              { apiEndpointResponseHeaders = [("Cache-Control", testHeaderValue "no-store")]
                              }
                          )
                      )
                )
                (\() -> apiResponse "unreachable")
        textResponse <- runApiRoute negotiatedEndpoint (Wai.defaultRequest {Wai.requestHeaders = [("Accept", "text/plain")]})
        jsonResponse <- runApiRoute negotiatedEndpoint (Wai.defaultRequest {Wai.requestHeaders = [("Accept", "application/json")]})
        unacceptableResponse <- runApiRoute negotiatedEndpoint (Wai.defaultRequest {Wai.requestHeaders = [("Accept", "application/xml")]})
        expectAll
          ( (apiRouteResponseStatus textResponse `shouldBe` HttpTypes.status200)
              :| [ apiRouteResponseBody textResponse `shouldBe` "hello",
                   lookup "Content-Type" (apiRouteResponseHeaders textResponse) `shouldBe` Just "text/plain; charset=utf-8",
                   lookup "Vary" (apiRouteResponseHeaders textResponse) `shouldBe` Just "Accept",
                   lookup "Cache-Control" (apiRouteResponseHeaders textResponse) `shouldBe` Just "no-store",
                   apiRouteResponseBody jsonResponse `shouldBe` "\"hello\"",
                   lookup "Content-Type" (apiRouteResponseHeaders jsonResponse) `shouldBe` Just "application/json; charset=utf-8",
                   apiRouteResponseStatus unacceptableResponse `shouldBe` HttpTypes.status406,
                   apiRouteResponseBody unacceptableResponse `shouldBe` "API response has no acceptable representation."
                 ]
          )

      it "carries a handler's observability attributes and log entries onto the rendered protocol response, never the body" $ do
        let diagnosticEndpoint =
              apiRouteEndpoint
                ApiGet
                (pure ())
                ApiNoRequestBody
                (textResponseEncoder :| [])
                ( \_ ->
                    pure
                      ( Right
                          ( (apiResponse "hello")
                              { apiEndpointResponseObservabilityAttributes =
                                  [Observability.ObservabilityAttribute "app.failure.code" (Observability.TextAttribute "example")],
                                apiEndpointResponseLogEntries = ["diagnostic detail that must never reach the response body"]
                              }
                          )
                      )
                )
                (\() -> apiResponse "unreachable")
        response <- runApiRoute diagnosticEndpoint Wai.defaultRequest
        expectAll
          ( ( protocolResponseObservabilityAttributes (apiRouteProtocolResponse response)
                `shouldBe` [Observability.ObservabilityAttribute "app.failure.code" (Observability.TextAttribute "example")]
            )
              :| [ protocolResponseLogEntries (apiRouteProtocolResponse response) `shouldBe` ["diagnostic detail that must never reach the response body"],
                   apiRouteResponseBody response `shouldBe` "hello"
                 ]
          )

      it "merges Accept into an application Vary header without changing its spelling" $ do
        let varyEndpoint =
              apiRouteEndpoint
                ApiGet
                (pure ())
                ApiNoRequestBody
                (textResponseEncoder :| [jsonResponseEncoder])
                ( \_ ->
                    pure
                      ( Right
                          ( (apiResponse "hello")
                              { apiEndpointResponseHeaders = [("X-Trace", testHeaderValue "present"), ("vArY", testHeaderValue "Origin"), ("Cache-Control", testHeaderValue "no-store")]
                              }
                          )
                      )
                )
                (\() -> apiResponse "unreachable")
            alreadyVaryEndpoint =
              apiRouteEndpoint
                ApiGet
                (pure ())
                ApiNoRequestBody
                (textResponseEncoder :| [jsonResponseEncoder])
                (\_ -> pure (Right ((apiResponse "hello") {apiEndpointResponseHeaders = [("Vary", testHeaderValue "Accept")]})))
                (\() -> apiResponse "unreachable")
        response <- runApiRoute varyEndpoint Wai.defaultRequest
        alreadyVaryResponse <- runApiRoute alreadyVaryEndpoint Wai.defaultRequest
        expectAll
          ( (lookup "vArY" (apiRouteResponseHeaders response) `shouldBe` Just "Origin, Accept")
              :| [ lookup "X-Trace" (apiRouteResponseHeaders response) `shouldBe` Just "present",
                   lookup "Cache-Control" (apiRouteResponseHeaders response) `shouldBe` Just "no-store",
                   lookup "Vary" (apiRouteResponseHeaders alreadyVaryResponse) `shouldBe` Just "Accept"
                 ]
          )

      it "selects the encoder whose declared Content-Type satisfies Accept parameters" $ do
        let plainMediaType = testMediaType "text/plain"
            parameterAwareEndpoint =
              apiRouteEndpoint
                ApiGet
                (pure ())
                ApiNoRequestBody
                ( bytesResponseEncoder (apiContentType plainMediaType)
                    :| [bytesResponseEncoder (apiUtf8ContentType plainMediaType)]
                )
                (\_ -> pure (Right (apiResponse "parameter-aware")))
                (\() -> apiResponse "unreachable")
        utf8Response <- runApiRoute parameterAwareEndpoint (Wai.defaultRequest {Wai.requestHeaders = [("Accept", "text/plain; charset=UTF-8")]})
        unacceptableResponse <- runApiRoute parameterAwareEndpoint (Wai.defaultRequest {Wai.requestHeaders = [("Accept", "text/plain; charset=us-ascii")]})
        expectAll
          ( (lookup "Content-Type" (apiRouteResponseHeaders utf8Response) `shouldBe` Just "text/plain; charset=utf-8")
              :| [ apiRouteResponseStatus unacceptableResponse `shouldBe` HttpTypes.status406,
                   apiRouteResponseBody unacceptableResponse `shouldBe` "API response has no acceptable representation."
                 ]
          )

      it "keeps a selected streaming encoder request-scoped until it is rendered" $ do
        executionReference <- newIORef False
        flushReference <- newIORef (0 :: Int)
        chunksReference <- newIORef []
        let streamBody write flush = do
              write (Builder.byteString "first")
              flush
              atomicModifyIORef' flushReference (\flushes -> (flushes + 1, ()))
              write (Builder.byteString "second")
              writeIORef executionReference True
            streamingEndpoint =
              apiRouteEndpoint
                ApiGet
                (pure ())
                ApiNoRequestBody
                (streamingResponseEncoder plainTextContentType id :| [])
                (\_ -> pure (Right (apiResponse streamBody)))
                (\() -> apiResponse streamBody)
        response <- runApiRoute streamingEndpoint Wai.defaultRequest
        executedBeforeRendering <- readIORef executionReference
        apiRouteResponseStream
          response
          (\builder -> atomicModifyIORef' chunksReference (\chunks -> (chunks <> [Builder.toLazyByteString builder], ())))
          (pure ())
        chunks <- readIORef chunksReference
        flushes <- readIORef flushReference
        executedAfterRendering <- readIORef executionReference
        expectAll
          ( (executedBeforeRendering `shouldBe` False)
              :| [ LazyByteString.toStrict (LazyByteString.concat chunks) `shouldBe` "firstsecond",
                   flushes `shouldBe` 1,
                   executedAfterRendering `shouldBe` True,
                   lookup "Content-Type" (apiRouteResponseHeaders response) `shouldBe` Just "text/plain; charset=utf-8",
                   protocolResponseObservabilityAttributes (apiRouteProtocolResponse response) `shouldBe` [],
                   protocolResponseLogEntries (apiRouteProtocolResponse response) `shouldBe` []
                 ]
          )

      it "gives a multipart endpoint one scoped body consumer" $ do
        consumedFieldsReference <- newIORef []
        let requestBody =
              "--endpoint-boundary\r\nContent-Disposition: form-data; name=\"title\"\r\n\r\nQuarterly report\r\n--endpoint-boundary\r\nContent-Disposition: form-data; name=\"attachment\"; filename=\"report.txt\"\r\n\r\ncontents\r\n--endpoint-boundary--\r\n"
            multipartEndpoint =
              apiRouteEndpoint
                ApiPost
                (requiredField (queryField "mode" apiTextValue))
                (ApiMultipartRequestBody inMemoryMultipartStorage defaultMultipartLimits)
                (textResponseEncoder :| [])
                ( \endpointRequest -> do
                    firstConsumption <-
                      withApiMultipartRequest (apiEndpointRequestBody endpointRequest) $ \part -> do
                        case part of
                          MultipartScopedFieldPart fieldName fieldValue ->
                            atomicModifyIORef' consumedFieldsReference (\fields -> (fields <> [(fieldName, fieldValue)], ()))
                          MultipartScopedFilePart {} -> pure ()
                        pure (Right ())
                    secondConsumption <- withApiMultipartRequest (apiEndpointRequestBody endpointRequest) (const (pure (Right ())))
                    pure $
                      case (apiEndpointRequestFields endpointRequest, firstConsumption, secondConsumption) of
                        ("multipart", Right (), Left ApiMultipartRequestAlreadyConsumed) -> Right (apiResponse "multipart consumed")
                        _ -> Left ()
                )
                (const (apiResponse "multipart endpoint failed"))
        request <- requestWithBody [("Content-Type", "multipart/form-data; boundary=endpoint-boundary")] [requestBody]
        response <- runApiRoute multipartEndpoint (request {Wai.queryString = [("mode", Just "multipart")]})
        consumedFields <- readIORef consumedFieldsReference
        expectAll
          ( (apiRouteResponseStatus response `shouldBe` HttpTypes.status200)
              :| [ apiRouteResponseBody response `shouldBe` "multipart consumed",
                   consumedFields `shouldBe` [("title", "Quarterly report")]
                 ]
          )

      it "leaves a multipart parser failure typed for the endpoint handler" $ do
        let multipartEndpoint =
              apiRouteEndpoint
                ApiPost
                (pure ())
                (ApiMultipartRequestBody inMemoryMultipartStorage defaultMultipartLimits)
                (textResponseEncoder :| [])
                ( \endpointRequest -> do
                    consumption <- withApiMultipartRequest (apiEndpointRequestBody endpointRequest) (const (pure (Right ())))
                    pure $
                      case consumption of
                        Left (ApiMultipartRequestFailed MultipartInvalidContentType) ->
                          Right ((apiResponse "multipart media type rejected") {apiEndpointResponseStatus = HttpTypes.status415})
                        _ -> Left ()
                )
                (const (apiResponse "multipart endpoint failed"))
        response <- runApiRoute multipartEndpoint Wai.defaultRequest
        expectAll
          ( (apiRouteResponseStatus response `shouldBe` HttpTypes.status415)
              :| [apiRouteResponseBody response `shouldBe` "multipart media type rejected"]
          )

      it "derives comparable, printable multipart request errors" $ do
        errorReference <- newIORef ApiMultipartRequestAlreadyConsumed
        alreadyConsumed <- readIORef errorReference
        writeIORef errorReference (ApiMultipartRequestFailed MultipartInvalidContentType)
        invalidContentType <- readIORef errorReference
        expectAll
          ( (alreadyConsumed `shouldBe` ApiMultipartRequestAlreadyConsumed)
              :| [ invalidContentType `shouldBe` ApiMultipartRequestFailed MultipartInvalidContentType,
                   alreadyConsumed `shouldNotBe` invalidContentType,
                   show alreadyConsumed `shouldBe` "ApiMultipartRequestAlreadyConsumed",
                   show invalidContentType `shouldBe` "ApiMultipartRequestFailed MultipartInvalidContentType",
                   showList [alreadyConsumed, invalidContentType] "" `shouldBe` "[ApiMultipartRequestAlreadyConsumed,ApiMultipartRequestFailed MultipartInvalidContentType]"
                 ]
          )

      it "gives a streaming endpoint one chunk-at-a-time consumer instead of a buffered body" $ do
        chunksReference <- newIORef ["ab", "cd", "e"]
        let streamingEndpoint =
              apiRouteEndpoint
                ApiPost
                (pure ())
                (ApiStreamingRequestBody 5)
                (textResponseEncoder :| [])
                (\endpointRequest -> pullAllStreamingChunks (apiEndpointRequestBody endpointRequest) "")
                (const ((apiResponse "stream too large") {apiEndpointResponseStatus = HttpTypes.status413}))
            request = Wai.setRequestBodyChunks (atomicModifyIORef' chunksReference takeNextChunk) Wai.defaultRequest
        response <- runApiRoute streamingEndpoint request
        expectAll
          ( (apiRouteResponseStatus response `shouldBe` HttpTypes.status200)
              :| [apiRouteResponseBody response `shouldBe` "abcde"]
          )

      it "leaves a streamed chunk exceeding the declared budget typed for the endpoint handler" $ do
        chunksReference <- newIORef ["ab", "cd", "ef"]
        let streamingEndpoint =
              apiRouteEndpoint
                ApiPost
                (pure ())
                (ApiStreamingRequestBody 5)
                (textResponseEncoder :| [])
                (\endpointRequest -> pullAllStreamingChunks (apiEndpointRequestBody endpointRequest) "")
                (const ((apiResponse "stream too large") {apiEndpointResponseStatus = HttpTypes.status413}))
            request = Wai.setRequestBodyChunks (atomicModifyIORef' chunksReference takeNextChunk) Wai.defaultRequest
        response <- runApiRoute streamingEndpoint request
        expectAll
          ( (apiRouteResponseStatus response `shouldBe` HttpTypes.status413)
              :| [apiRouteResponseBody response `shouldBe` "stream too large"]
          )

      it "rejects a streamed body whose declared Content-Length already exceeds the budget, before any pull" $ do
        let streamingEndpoint =
              apiRouteEndpoint
                ApiPost
                (pure ())
                (ApiStreamingRequestBody 5)
                (textResponseEncoder :| [])
                (\endpointRequest -> pullAllStreamingChunks (apiEndpointRequestBody endpointRequest) "")
                (const ((apiResponse "stream too large") {apiEndpointResponseStatus = HttpTypes.status413}))
        response <- runApiRoute streamingEndpoint (Wai.defaultRequest {Wai.requestHeaders = [(HttpTypes.hContentLength, "6")]})
        expectAll
          ( (apiRouteResponseStatus response `shouldBe` HttpTypes.status413)
              :| [apiRouteResponseBody response `shouldBe` "stream too large"]
          )

    describe "apiRouteDefinitionWithContext" $ do
      let contextAwareEndpointDefinition =
            apiRouteDefinitionWithContext
              ApiGet
              (pure ())
              ApiNoRequestBody
              (textResponseEncoder :| [])
              (\contextValue _endpointRequest -> pure (Right (apiResponse ("context:" <> contextValue))))
              (\() -> apiResponse "unreachable")
          failingContextAwareEndpointDefinition =
            apiRouteDefinitionWithContext
              ApiGet
              (pure ())
              ApiNoRequestBody
              (textResponseEncoder :| [])
              (const (const (pure (Left ()))))
              (\() -> (apiResponse "context-aware domain failure") {apiEndpointResponseStatus = HttpTypes.status422})
          runWithContext contextValue request =
            routeResponse contextAwareEndpointDefinition request (RouteRequest () contextValue)

      it "declares its endpoint's own method and no navigation label, unaffected by context" $
        expectAll
          ( (routeMethods contextAwareEndpointDefinition `shouldBe` [HarchWeb.RouteGet])
              :| [routeNavigationLabel contextAwareEndpointDefinition `shouldBe` Nothing]
          )

      it "passes the route's own resolved context to the handler instead of the template endpoint's" $ do
        firstResponse <- runWithContext "first" Wai.defaultRequest
        secondResponse <- runWithContext "second" Wai.defaultRequest
        expectAll
          ( (apiRouteResponseStatus firstResponse `shouldBe` HttpTypes.status200)
              :| [ apiRouteResponseBody firstResponse `shouldBe` "context:first",
                   apiRouteResponseBody secondResponse `shouldBe` "context:second"
                 ]
          )

      it "interprets an expected domain failure at the endpoint boundary just like apiRouteDefinition" $ do
        response <- routeResponse failingContextAwareEndpointDefinition Wai.defaultRequest (RouteRequest () ())
        expectAll
          ( (apiRouteResponseStatus response `shouldBe` HttpTypes.status422)
              :| [apiRouteResponseBody response `shouldBe` "context-aware domain failure"]
          )

    describe "apiRouteDefinitionWithContextNeverFailing" $ do
      let neverFailingContextAwareEndpointDefinition =
            apiRouteDefinitionWithContextNeverFailing
              ApiPost
              (requiredField (queryField "greeting" apiTextValue))
              (ApiBufferedRequestBody (AssumeMediaType plainTextMediaType) 64 [textBodyDecoder])
              (textResponseEncoder :| [])
              ( \contextValue endpointRequest ->
                  pure
                    ( apiResponse
                        (contextValue <> ":" <> apiEndpointRequestFields endpointRequest <> ":" <> apiEndpointRequestBody endpointRequest)
                    )
              )
          runWithContext contextValue request =
            routeResponse neverFailingContextAwareEndpointDefinition request (RouteRequest () contextValue)

      it "declares its endpoint's own method and no navigation label, unaffected by context" $
        expectAll
          ( (routeMethods neverFailingContextAwareEndpointDefinition `shouldBe` [HarchWeb.RoutePost])
              :| [routeNavigationLabel neverFailingContextAwareEndpointDefinition `shouldBe` Nothing]
          )

      it "passes the route's own resolved context and decoded fields and body straight to the handler's response, with no domain failure to interpret" $ do
        firstRequest <- requestWithBody [] ["world"]
        secondRequest <- requestWithBody [] ["there"]
        firstResponse <- runWithContext "first" firstRequest {Wai.queryString = [("greeting", Just "hello")]}
        secondResponse <- runWithContext "second" secondRequest {Wai.queryString = [("greeting", Just "hi")]}
        expectAll
          ( (apiRouteResponseStatus firstResponse `shouldBe` HttpTypes.status200)
              :| [ apiRouteResponseBody firstResponse `shouldBe` "first:hello:world",
                   apiRouteResponseBody secondResponse `shouldBe` "second:hi:there"
                 ]
          )

    describe "apiHttpResponseToProtocolResponse" $ do
      it "preserves a matched response's status, headers, and strict protocol bytes" $
        apiHttpResponseToProtocolResponse
          (ApiHttpResponse HttpTypes.status201 [("Content-Type", testHeaderValue "application/example"), ("X-Example", testHeaderValue "present")] (Just (apiBytesResponse (apiContentType (testMediaType "application/example")) "\NUL\SOH\STX")))
          `shouldBe` ProtocolResponse
            { protocolResponseStatus = HttpTypes.status201,
              protocolResponseHeaders = [("Content-Type", "application/example"), ("X-Example", "present")],
              protocolResponseBody = ProtocolResponseBytes "\NUL\SOH\STX",
              protocolResponseObservabilityAttributes = [],
              protocolResponseLogEntries = []
            }

      it "uses an empty strict body for a protocol result without an API body" $
        case protocolResponseBody (apiHttpResponseToProtocolResponse (ApiHttpResponse HttpTypes.status405 [("Allow", testHeaderValue "GET, HEAD, OPTIONS")] Nothing)) of
          ProtocolResponseBytes bodyBytes -> bodyBytes `shouldBe` ""
          ProtocolResponseStream _ -> expectationFailure "expected a strict protocol body"

    describe "RequestCodec" $ do
      let sampleRequestData =
            ApiRequestData
              { apiRequestQueryParameters = [("q", "hello"), ("dup", "one"), ("dup", "two")],
                apiRequestHeaders = [(apiHeaderName "X-Token", "secret"), (apiHeaderName "X-Bad", "")],
                apiRequestCookies = [("session", "opaque"), ("repeat", "first"), ("repeat", "second")],
                apiRequestFormFields = [("name", "Ada"), ("repeat-form", "first"), ("repeat-form", "second")]
              }

      it "decodes a required, present field from its declared source" $
        runRequestCodec (requiredField (queryField "q" apiTextValue)) sampleRequestData
          `shouldBe` ([], Just "hello")

      it "decodes a required header field" $
        runRequestCodec (requiredField (headerField (apiHeaderName "X-Token") apiTextValue)) sampleRequestData
          `shouldBe` ([], Just "secret")

      it "decodes a required case-sensitive cookie field" $
        expectAll
          ( (runRequestCodec (requiredField (cookieField "session" apiTextValue)) sampleRequestData `shouldBe` ([], Just "opaque"))
              :| [ runRequestCodec (requiredField (cookieField "SESSION" apiTextValue)) sampleRequestData
                     `shouldBe` ([MissingApiField ApiCookieSource "SESSION"], Nothing),
                   runRequestCodec (requiredField (cookieField "repeat" apiTextValue)) sampleRequestData
                     `shouldBe` ([DuplicateApiField ApiCookieSource "repeat"], Nothing)
                 ]
          )

      it "decodes a required form field and retains duplicate-form rejection" $
        expectAll
          ( (runRequestCodec (requiredField (formField "name" apiTextValue)) sampleRequestData `shouldBe` ([], Just "Ada"))
              :| [ runRequestCodec (requiredField (formField "repeat-form" apiTextValue)) sampleRequestData
                     `shouldBe` ([DuplicateApiField ApiFormSource "repeat-form"], Nothing),
                   runRequestCodec (requiredField (formField "missing-form" apiTextValue)) sampleRequestData
                     `shouldBe` ([MissingApiField ApiFormSource "missing-form"], Nothing)
                 ]
          )

      it "adds decoded form fields without discarding the original request sources" $
        case apiBodyDecoderParse (urlEncodedFormBodyDecoder 1) "name=Ada" of
          Left parseError -> expectationFailure (Text.unpack parseError)
          Right decodedForm ->
            runRequestCodec
              ( (,)
                  <$> requiredField (queryField "q" apiTextValue)
                  <*> requiredField (formField "name" apiTextValue)
              )
              (apiRequestDataWithForm decodedForm sampleRequestData)
              `shouldBe` ([], Just ("hello", "Ada"))

      it "canonicalizes header names for equality and diagnostics" $
        let declaredName = apiHeaderName "X-Token"
         in expectAll
              ( (declaredName `shouldBe` apiHeaderName "x-token")
                  :| [ declaredName `shouldNotBe` apiHeaderName "x-other",
                       apiHeaderNameText declaredName `shouldBe` "x-token",
                       apiHeaderName "\8490" `shouldNotBe` apiHeaderName "k",
                       apiHeaderNameText (apiHeaderName "\8490") `shouldBe` "\8490",
                       length (show declaredName) + length (showList [declaredName] "") `shouldSatisfy` (> 0)
                     ]
              )

      it "matches declared and extracted header names case-insensitively" $
        let request = Wai.defaultRequest {Wai.requestHeaders = [("X-Token", "secret")]}
         in runRequestCodec
              (requiredField (headerField (apiHeaderName "x-TOKEN") apiTextValue))
              (apiRequestDataFromWaiRequest request)
              `shouldBe` ([], Just "secret")

      it "reports a missing required field" $
        runRequestCodec (requiredField (queryField "missing" apiTextValue)) sampleRequestData
          `shouldBe` ([MissingApiField ApiQuerySource "missing"], Nothing)

      it "reports a duplicate field" $
        runRequestCodec (requiredField (queryField "dup" apiTextValue)) sampleRequestData
          `shouldBe` ([DuplicateApiField ApiQuerySource "dup"], Nothing)

      it "reports an invalid field that fails its value parser" $
        runRequestCodec
          (requiredField (queryField "q" (parseApiField (const Nothing :: Text -> Maybe Text))))
          sampleRequestData
          `shouldBe` ([InvalidApiField ApiQuerySource "q"], Nothing)

      it "resolves a present optional field to Just" $
        runRequestCodec (optionalField (queryField "q" apiTextValue)) sampleRequestData
          `shouldBe` ([], Just (Just "hello"))

      it "resolves a missing optional field to Nothing without an error" $
        runRequestCodec (optionalField (queryField "missing" apiTextValue)) sampleRequestData
          `shouldBe` ([], Just Nothing)

      it "keeps a duplicate optional field an error rather than silently defaulting" $
        runRequestCodec (optionalField (queryField "dup" apiTextValue)) sampleRequestData
          `shouldBe` ([DuplicateApiField ApiQuerySource "dup"], Nothing)

      it "resolves a present field-with-default to its value" $
        runRequestCodec (fieldWithDefault "fallback" (queryField "q" apiTextValue)) sampleRequestData
          `shouldBe` ([], Just "hello")

      it "resolves a missing field-with-default to its default" $
        runRequestCodec (fieldWithDefault "fallback" (queryField "missing" apiTextValue)) sampleRequestData
          `shouldBe` ([], Just "fallback")

      it "keeps a duplicate field-with-default an error rather than silently defaulting" $
        runRequestCodec (fieldWithDefault "fallback" (queryField "dup" apiTextValue)) sampleRequestData
          `shouldBe` ([DuplicateApiField ApiQuerySource "dup"], Nothing)

      it "accumulates independent errors from multiple required fields" $
        runRequestCodec
          ( (,)
              <$> requiredField (queryField "missing" apiTextValue)
              <*> requiredField (headerField (apiHeaderName "X-Missing") apiTextValue)
          )
          sampleRequestData
          `shouldBe` ( [MissingApiField ApiQuerySource "missing", MissingApiField ApiHeaderSource "x-missing"],
                       Nothing
                     )

      it "derives comparable, printable representations for request codec types" $
        let sources = [ApiQuerySource, ApiHeaderSource, ApiCookieSource, ApiFormSource]
            parseErrors =
              [ MissingApiField ApiQuerySource "q",
                DuplicateApiField ApiHeaderSource "h",
                InvalidApiField ApiQuerySource "q",
                MissingApiField ApiCookieSource "session",
                DuplicateApiField ApiFormSource "name"
              ]
         in expectAll
              ( (sum [fromEnum (left == right) | left <- sources, right <- sources] `shouldBe` length sources)
                  :| [ sum [fromEnum (left /= right) | left <- sources, right <- sources] `shouldBe` length sources * (length sources - 1),
                       sum [length (show sourceValue) + length (showList [sourceValue] "") | sourceValue <- sources] `shouldSatisfy` (> 0),
                       sum [fromEnum (left == right) | left <- parseErrors, right <- parseErrors] `shouldBe` length parseErrors,
                       sum [fromEnum (left /= right) | left <- parseErrors, right <- parseErrors] `shouldBe` length parseErrors * (length parseErrors - 1),
                       sum [length (show parseError) + length (showList [parseError] "") | parseError <- parseErrors] `shouldSatisfy` (> 0),
                       (sampleRequestData == sampleRequestData) `shouldBe` True,
                       (sampleRequestData /= sampleRequestData {apiRequestQueryParameters = []}) `shouldBe` True,
                       length (show sampleRequestData) + length (showList [sampleRequestData] "") `shouldSatisfy` (> 0)
                     ]
              )

    describe "request body decoding" $ do
      let jsonDecoder = jsonBodyDecoder :: ApiBodyDecoder Int

      it "validates and normalizes an application-declared media type" $
        expectAll
          ( (apiMediaType " Application/JSON " `shouldBe` Just (testMediaType "application/json"))
              :| [ apiMediaTypeText (testMediaType "application/json") `shouldBe` "application/json",
                   apiMediaTypeText jsonMediaType `shouldBe` "application/json",
                   apiMediaTypeText plainTextMediaType `shouldBe` "text/plain",
                   apiMediaTypeText htmlMediaType `shouldBe` "text/html",
                   testMediaType "application/json" == testMediaType "application/json" `shouldBe` True,
                   testMediaType "application/json" /= testMediaType "text/plain" `shouldBe` True,
                   show (testMediaType "application/json") `shouldSatisfy` (not . null),
                   showList [testMediaType "application/json", testMediaType "text/plain"] "" `shouldSatisfy` (not . null),
                   apiMediaType "not-a-media-type" `shouldBe` Nothing,
                   apiMediaType "text" `shouldBe` Nothing
                 ]
          )

      it "decodes a JSON body when Content-Type matches" $
        selectApiBodyDecoder RejectMissingContentType [jsonDecoder] (Just "application/json") "42"
          `shouldBe` ApiDecodedBody 42

      it "decodes a JSON body when Content-Type includes parameters" $
        selectApiBodyDecoder RejectMissingContentType [jsonDecoder] (Just "application/json; charset=utf-8") "7"
          `shouldBe` ApiDecodedBody 7

      it "matches Content-Type case-insensitively" $
        selectApiBodyDecoder RejectMissingContentType [jsonDecoder] (Just "APPLICATION/JSON") "1"
          `shouldBe` ApiDecodedBody 1

      it "reports unsupported media type for an undeclared Content-Type" $
        selectApiBodyDecoder RejectMissingContentType [jsonDecoder] (Just "text/plain") "3"
          `shouldBe` ApiUnsupportedMediaType [testMediaType "application/json"]

      it "reports unsupported media type for a malformed Content-Type header" $
        selectApiBodyDecoder RejectMissingContentType [jsonDecoder] (Just "garbage") "3"
          `shouldBe` ApiUnsupportedMediaType [testMediaType "application/json"]

      it "rejects a missing Content-Type when the policy requires one" $
        selectApiBodyDecoder RejectMissingContentType [jsonDecoder] Nothing "42"
          `shouldBe` ApiUnsupportedMediaType [testMediaType "application/json"]

      it "assumes a declared media type when Content-Type is missing and the policy allows it" $
        selectApiBodyDecoder (AssumeMediaType (testMediaType "application/json")) [jsonDecoder] Nothing "42"
          `shouldBe` ApiDecodedBody 42

      it "reports a malformed body when the selected decoder rejects the syntax" $
        selectApiBodyDecoder RejectMissingContentType [jsonDecoder] (Just "application/json") "not json"
          `shouldBe` ApiMalformedBody

      it "carries a non-empty error message when the JSON decoder itself rejects a body" $
        apiBodyDecoderParse jsonDecoder "not json" `shouldSatisfy` \case
          Left errorMessage -> not (Text.null errorMessage)
          Right (_ :: Int) -> False

      it "decodes a strict-UTF-8 text/plain body" $
        selectApiBodyDecoder RejectMissingContentType [textBodyDecoder] (Just "text/plain") "hello"
          `shouldBe` ApiDecodedBody "hello"

      it "reports a malformed body for invalid UTF-8 in a text/plain body" $
        selectApiBodyDecoder RejectMissingContentType [textBodyDecoder] (Just "text/plain") "bad\xFF"
          `shouldBe` ApiMalformedBody

      it "carries a fixed error message when the text decoder itself rejects invalid UTF-8" $
        apiBodyDecoderParse textBodyDecoder "bad\xFF" `shouldBe` Left "invalid UTF-8 body"

      it "passes a body through unparsed for a declared bytes media type" $
        selectApiBodyDecoder RejectMissingContentType [bytesBodyDecoder (testMediaType "application/octet-stream")] (Just "application/octet-stream") "\1\2\3"
          `shouldBe` ApiDecodedBody "\1\2\3"

      it "decodes a bounded URL-encoded form with strict UTF-8 fields" $
        let formDecoder = urlEncodedFormBodyDecoder 2
            parsedForm = apiBodyDecoderParse formDecoder "name=Ada+Lovelace&empty"
         in expectAll
              ( (apiBodyDecoderMediaType formDecoder `shouldBe` urlEncodedFormMediaType)
                  :| [ fmap apiFormFields parsedForm `shouldBe` Right [("name", "Ada Lovelace"), ("empty", "")],
                       apiBodyDecoderParse formDecoder "one=1&two=2&three=3" `shouldBe` Left "form contains more fields than declared",
                       apiBodyDecoderParse formDecoder "name=%ZZ" `shouldBe` Left "form contains invalid percent encoding",
                       apiBodyDecoderParse formDecoder "name=%" `shouldBe` Left "form contains invalid percent encoding",
                       apiBodyDecoderParse formDecoder "name=%A" `shouldBe` Left "form contains invalid percent encoding",
                       fmap apiFormFields (apiBodyDecoderParse formDecoder "digit=%41&lower=%4a")
                         `shouldBe` Right [("digit", "A"), ("lower", "J")],
                       apiBodyDecoderParse formDecoder "name=bad\xFF" `shouldBe` Left "form contains invalid UTF-8"
                     ]
              )

      it "validates a runtime-supplied percent escape before parsing the form" $ do
        encodedBodyReference <- newIORef "name=%41"
        encodedBody <- readIORef encodedBodyReference
        fmap apiFormFields (apiBodyDecoderParse (urlEncodedFormBodyDecoder 1) encodedBody)
          `shouldBe` Right [("name", "A")]

      it "keeps decoded forms comparable and printable without changing their field order" $
        let formDecoder = urlEncodedFormBodyDecoder 2
            orderedForm = apiBodyDecoderParse formDecoder "first=1&second=2"
            reversedForm = apiBodyDecoderParse formDecoder "second=2&first=1"
            changedLastValueForm = apiBodyDecoderParse formDecoder "first=1&second=changed"
         in expectAll
              ( (orderedForm `shouldBe` apiBodyDecoderParse formDecoder "first=1&second=2")
                  :| [ orderedForm /= reversedForm `shouldBe` True,
                       orderedForm /= changedLastValueForm `shouldBe` True,
                       fmap show orderedForm `shouldSatisfy` \case
                         Right renderedForm -> not (null renderedForm)
                         Left _parseError -> False,
                       fmap (\decodedForm -> showList [decodedForm] "") orderedForm `shouldSatisfy` \case
                         Right renderedForms -> not (null renderedForms)
                         Left _parseError -> False
                     ]
              )

      it "compares decoded forms directly rather than only through their decoder result" $
        case (apiBodyDecoderParse (urlEncodedFormBodyDecoder 1) "name=Ada", apiBodyDecoderParse (urlEncodedFormBodyDecoder 1) "name=Grace") of
          (Right adaForm, Right graceForm) -> adaForm /= graceForm `shouldBe` True
          _ -> expectationFailure "expected both bounded forms to decode"

      it "accepts an empty URL-encoded form" $
        fmap apiFormFields (apiBodyDecoderParse (urlEncodedFormBodyDecoder 0) "") `shouldBe` Right []

      it "runs a typed form codec only after the form decoder succeeds" $
        let formDecoder = urlEncodedFormBodyDecoder 2
         in case apiBodyDecoderParse formDecoder "name=Ada" of
              Left _parseError -> expectationFailure "expected the form body to decode"
              Right decodedForm ->
                expectAll
                  ( (runApiFormCodec (requiredField (formField "name" apiTextValue)) decodedForm `shouldBe` ([], Just "Ada"))
                      :| [ runApiFormCodec (optionalField (queryField "q" apiTextValue)) decodedForm
                             `shouldBe` ([], Just Nothing),
                           runApiFormCodec (optionalField (headerField (apiHeaderName "X-Test") apiTextValue)) decodedForm
                             `shouldBe` ([], Just Nothing),
                           runApiFormCodec (optionalField (cookieField "session" apiTextValue)) decodedForm
                             `shouldBe` ([], Just Nothing)
                         ]
                  )

      it "derives comparable, printable representations for MissingContentTypePolicy and ApiBodyOutcome" $
        let policies = [RejectMissingContentType, AssumeMediaType (testMediaType "application/json")]
            outcomes = [ApiUnsupportedMediaType [testMediaType "application/json"], ApiMalformedBody, ApiDecodedBody (1 :: Int)]
         in expectAll
              ( (sum [fromEnum (left == right) | left <- policies, right <- policies] `shouldBe` length policies)
                  :| [ sum [fromEnum (left /= right) | left <- policies, right <- policies]
                         `shouldBe` length policies * (length policies - 1),
                       sum [length (show p) + length (showList [p] "") | p <- policies] `shouldSatisfy` (> 0),
                       sum [fromEnum (left == right) | left <- outcomes, right <- outcomes] `shouldBe` length outcomes,
                       sum [fromEnum (left /= right) | left <- outcomes, right <- outcomes]
                         `shouldBe` length outcomes * (length outcomes - 1),
                       sum [length (show o) + length (showList [o] "") | o <- outcomes] `shouldSatisfy` (> 0)
                     ]
              )

    describe "ResponseCodec" $ do
      it "renders a JSON response with its content type" $
        expectAll
          ( (apiContentTypeText (apiResponseContentType (apiJsonResponse (42 :: Int))) `shouldBe` "application/json; charset=utf-8")
              :| [apiResponseBodyBytes (apiJsonResponse (42 :: Int)) `shouldBe` "42"]
          )

      it "renders a text response with its content type" $
        expectAll
          ( (apiContentTypeText (apiResponseContentType (apiTextResponse "hello")) `shouldBe` "text/plain; charset=utf-8")
              :| [apiResponseBodyBytes (apiTextResponse "hello") `shouldBe` "hello"]
          )

      it "renders a bytes response with an explicit content type" $
        expectAll
          ( (apiContentTypeText (apiResponseContentType (apiBytesResponse (apiContentType (testMediaType "image/svg+xml")) "<svg/>")) `shouldBe` "image/svg+xml")
              :| [apiResponseBodyBytes (apiBytesResponse (apiContentType (testMediaType "image/svg+xml")) "<svg/>") `shouldBe` "<svg/>"]
          )

      it "keeps typed response data separate from its declared pure encoders" $
        let svgContentType = apiContentType (testMediaType "image/svg+xml")
            responseValue = apiResponse "hello"
         in expectAll
              ( (apiEndpointResponseStatus responseValue `shouldBe` HttpTypes.status200)
                  :| [ apiEndpointResponseHeaders responseValue `shouldBe` [],
                       apiEndpointResponseObservabilityAttributes responseValue `shouldBe` [],
                       apiEndpointResponseLogEntries responseValue `shouldBe` [],
                       apiEndpointResponseValue responseValue `shouldBe` ("hello" :: Text),
                       strictEncodedResponseBytes (apiResponseEncoderEncode jsonResponseEncoder ("hello" :: Text)) `shouldBe` "\"hello\"",
                       strictEncodedResponseBytes (apiResponseEncoderEncode textResponseEncoder "hello") `shouldBe` "hello",
                       strictEncodedResponseBytes (apiResponseEncoderEncode (bytesResponseEncoder svgContentType) "<svg/>") `shouldBe` "<svg/>",
                       apiContentTypeMediaType svgContentType `shouldBe` testMediaType "image/svg+xml",
                       apiContentType (testMediaType "application/json") /= jsonContentType `shouldBe` True,
                       length (show (apiContentType (testMediaType "application/json"))) `shouldSatisfy` (> 0),
                       length (show jsonContentType) `shouldSatisfy` (> 0),
                       length (showList [apiContentType (testMediaType "application/json"), jsonContentType] "") `shouldSatisfy` (> 0),
                       apiResponseEncoderContentType (bytesResponseEncoder svgContentType) `shouldBe` svgContentType
                     ]
              )

      it "defaults every built-in response body to status 200" $
        expectAll
          ( (apiResponseStatus (apiJsonResponse (42 :: Int)) `shouldBe` HttpTypes.status200)
              :| [ apiResponseStatus (apiTextResponse "hello") `shouldBe` HttpTypes.status200,
                   apiResponseStatus (apiBytesResponse (apiContentType (testMediaType "image/svg+xml")) "<svg/>") `shouldBe` HttpTypes.status200,
                   apiResponseHeaders (apiTextResponse "hello") `shouldBe` []
                 ]
          )

      it "lets a caller override the status with a record update, e.g. 422 for a semantically invalid request" $
        apiResponseStatus (apiTextResponse "invalid") {apiResponseStatus = HttpTypes.status422} `shouldBe` HttpTypes.status422

      it "derives comparable, printable representations for response bodies" $
        let bodies = [apiJsonResponse (1 :: Int), apiTextResponse "x", apiBytesResponse (apiContentType (testMediaType "image/svg+xml")) "<svg/>"]
         in expectAll
              ( (sum [fromEnum (left == right) | left <- bodies, right <- bodies] `shouldBe` length bodies)
                  :| [ sum [fromEnum (left /= right) | left <- bodies, right <- bodies] `shouldBe` length bodies * (length bodies - 1),
                       sum [length (show bodyValue) + length (showList [bodyValue] "") | bodyValue <- bodies] `shouldSatisfy` (> 0)
                     ]
              )

    describe "Content negotiation" $ do
      let jsonAndText = testMediaType "application/json" :| [testMediaType "text/plain"]

      it "selects the first declared representation when Accept is absent" $
        selectRepresentation jsonAndText Nothing `shouldBe` SelectedRepresentation (testMediaType "application/json")

      it "selects an exact match" $
        selectRepresentation jsonAndText (Just "text/plain") `shouldBe` SelectedRepresentation (testMediaType "text/plain")

      it "prefers the higher client quality between two acceptable representations" $
        selectRepresentation jsonAndText (Just "application/json;q=0.2, text/plain;q=0.8")
          `shouldBe` SelectedRepresentation (testMediaType "text/plain")

      it "breaks a quality tie with server declaration order" $
        selectRepresentation jsonAndText (Just "application/json;q=0.5, text/plain;q=0.5")
          `shouldBe` SelectedRepresentation (testMediaType "application/json")

      it "matches a type wildcard" $
        selectRepresentation jsonAndText (Just "text/*") `shouldBe` SelectedRepresentation (testMediaType "text/plain")

      it "matches the full wildcard" $
        selectRepresentation jsonAndText (Just "*/*") `shouldBe` SelectedRepresentation (testMediaType "application/json")

      it "does not claim a bare media type satisfies an Accept media parameter" $
        selectRepresentation jsonAndText (Just "text/plain; charset=utf-8")
          `shouldBe` NoAcceptableRepresentation

      it "matches an Accept media parameter against a declared response Content-Type" $
        let plainMediaType = testMediaType "text/plain"
            textContentTypes = apiContentType plainMediaType :| [apiUtf8ContentType plainMediaType]
         in selectContentTypeRepresentation textContentTypes (Just "text/plain; charset=\"UTF-8\"")
              `shouldBe` SelectedContentTypeRepresentation (apiUtf8ContentType plainMediaType)

      it "lets a more parameterized Accept range override an otherwise identical range" $
        let plainMediaType = testMediaType "text/plain"
            textContentTypes = apiContentType plainMediaType :| [apiUtf8ContentType plainMediaType]
         in selectContentTypeRepresentation textContentTypes (Just "text/plain;q=0.1, text/plain;charset=utf-8;q=0.9")
              `shouldBe` SelectedContentTypeRepresentation (apiUtf8ContentType plainMediaType)

      it "matches parameterized wildcard ranges against declared Content-Types" $
        let plainMediaType = testMediaType "text/plain"
            textContentTypes = apiContentType plainMediaType :| [apiUtf8ContentType plainMediaType]
            expected = SelectedContentTypeRepresentation (apiUtf8ContentType plainMediaType)
         in expectAll
              ( (selectContentTypeRepresentation textContentTypes (Just "*/*;charset=utf-8;q=0.1, text/plain;charset=utf-8;q=0.9") `shouldBe` expected)
                  :| [ selectContentTypeRepresentation textContentTypes (Just "text/*;charset=utf-8;q=0.1, text/plain;charset=utf-8;q=0.9") `shouldBe` expected,
                       selectContentTypeRepresentation textContentTypes (Just "*/*;charset=utf-8, */*;charset=utf-8;charset=utf-8") `shouldBe` expected,
                       selectContentTypeRepresentation textContentTypes (Just "text/*;charset=utf-8, text/*;charset=utf-8;charset=utf-8") `shouldBe` expected
                     ]
              )

      it "does not let an Accept extension after q constrain Content-Type matching" $
        let plainMediaType = testMediaType "text/plain"
            textContentTypes = apiContentType plainMediaType :| [apiUtf8ContentType plainMediaType]
         in selectContentTypeRepresentation textContentTypes (Just "text/plain; q=0.5; charset=us-ascii")
              `shouldBe` SelectedContentTypeRepresentation (apiContentType plainMediaType)

      it "lets a more specific range's q=0 exclude a representation despite a permissive wildcard" $
        selectRepresentation jsonAndText (Just "*/*;q=1, application/json;q=0")
          `shouldBe` SelectedRepresentation (testMediaType "text/plain")

      it "returns 406 when every declared representation is excluded" $
        selectRepresentation jsonAndText (Just "text/html, application/xml")
          `shouldBe` NoAcceptableRepresentation

      it "returns 406 when the only match is explicitly q=0" $
        selectRepresentation jsonAndText (Just "*/*;q=0") `shouldBe` NoAcceptableRepresentation

      it "keeps the less specific match when a later range in the header is no more specific" $
        selectRepresentation jsonAndText (Just "application/json, */*")
          `shouldBe` SelectedRepresentation (testMediaType "application/json")

      it "lets a type wildcard's specificity win over its own higher quality against a more specific, lower-quality match" $
        selectRepresentation jsonAndText (Just "*/*;q=0.1, text/*;q=0.9, text/plain;q=0.5")
          `shouldBe` SelectedRepresentation (testMediaType "text/plain")

      it "only accepts validated declared representations" $
        apiMediaType "not-a-media-type" `shouldBe` Nothing

      it "drops an Accept parameter that has no '=' rather than failing the whole entry" $
        parseAcceptHeader "text/plain;malformed, application/json"
          `shouldBe` [AcceptedRange "text" "plain" [] 1.0, AcceptedRange "application" "json" [] 1.0]

      it "is case-insensitive for the declared media type" $
        selectRepresentation jsonAndText (Just "APPLICATION/JSON")
          `shouldBe` SelectedRepresentation (testMediaType "application/json")

      it "parses quality, whitespace, and multiple ranges from a header" $
        expectAll
          ( (map acceptedRangeQuality (parseAcceptHeader "text/plain; q=0.5, application/json") `shouldBe` [0.5, 1.0])
              :| [ map (\r -> (acceptedRangeType r, acceptedRangeSubtype r)) (parseAcceptHeader " text/plain , application/json ")
                     `shouldBe` [("text", "plain"), ("application", "json")]
                 ]
          )

      it "retains normalized media parameters before q and ignores extensions after it" $
        parseAcceptHeader "text/plain; charset=\"UTF-8\";q=0.5;level=1"
          `shouldBe` [AcceptedRange "text" "plain" [("charset", "utf-8")] 0.5]

      it "keeps quoted commas, semicolons, and escaped quotes inside one Accept parameter" $
        parseAcceptHeader "text/plain; note=\"first, second; \\\"quoted\\\"\";q=0.2, application/json"
          `shouldBe` [ AcceptedRange "text" "plain" [("note", "first, second; \\\"quoted\\\"")] 0.2,
                       AcceptedRange "application" "json" [] 1.0
                     ]

      it "drops a malformed quality value and malformed media range" $
        expectAll
          ( (parseAcceptHeader "text/plain;q=nope" `shouldBe` [])
              :| [parseAcceptHeader "not-a-media-type, text/plain" `shouldBe` [AcceptedRange "text" "plain" [] 1.0]]
          )

      it "accepts only RFC-bounded quality values with at most three decimal places" $
        expectAll
          ( (map acceptedRangeQuality (parseAcceptHeader "text/plain;q=0, application/json;q=0.125, image/svg+xml;q=1.000") `shouldBe` [0.0, 0.125, 1.0])
              :| [ parseAcceptHeader "text/plain;q=1.001" `shouldBe` [],
                   parseAcceptHeader "text/plain;q=0.1234" `shouldBe` [],
                   parseAcceptHeader "text/plain;q=2" `shouldBe` [],
                   parseAcceptHeader "text/plain;q=0.5suffix" `shouldBe` []
                 ]
          )

      it "derives comparable, printable representations for negotiation types" $
        let plainMediaType = testMediaType "text/plain"
            ranges = parseAcceptHeader "text/plain;q=0.5;level=1, application/json"
            results = [NoAcceptableRepresentation, SelectedRepresentation (testMediaType "application/json")]
            contentTypeResults = [NoAcceptableContentTypeRepresentation, SelectedContentTypeRepresentation (apiUtf8ContentType plainMediaType)]
         in expectAll
              ( (sum [fromEnum (left == right) | left <- ranges, right <- ranges] `shouldBe` length ranges)
                  :| [ sum [fromEnum (left /= right) | left <- ranges, right <- ranges] `shouldBe` length ranges * (length ranges - 1),
                       sum [length (show rangeValue) + length (showList [rangeValue] "") | rangeValue <- ranges] `shouldSatisfy` (> 0),
                       sum [fromEnum (left == right) | left <- results, right <- results] `shouldBe` length results,
                       sum [fromEnum (left /= right) | left <- results, right <- results] `shouldBe` length results * (length results - 1),
                       sum [length (show resultValue) + length (showList [resultValue] "") | resultValue <- results] `shouldSatisfy` (> 0),
                       sum [fromEnum (left == right) | left <- contentTypeResults, right <- contentTypeResults] `shouldBe` length contentTypeResults,
                       sum [fromEnum (left /= right) | left <- contentTypeResults, right <- contentTypeResults] `shouldBe` length contentTypeResults * (length contentTypeResults - 1),
                       sum [length (show resultValue) + length (showList [resultValue] "") | resultValue <- contentTypeResults] `shouldSatisfy` (> 0)
                     ]
              )
