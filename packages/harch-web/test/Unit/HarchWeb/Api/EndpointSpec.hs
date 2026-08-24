{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Data.ByteString qualified as ByteString
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LazyByteString
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb qualified
import HarchWeb.Api
import HarchWeb.Api.Multipart (MultipartConsumeError (..), MultipartScopedPart (..), defaultMultipartLimits, inMemoryMultipartStorage)
import HarchWeb.Observability qualified as Observability
import HarchWeb.Routing (RouteRequest (..))
import HarchWeb.Server (ProtocolResponse (..), ProtocolResponseBody (..), Response (..))
import HarchWeb.Site (RouteDefinition (..))
import Network.HTTP.Types qualified as HttpTypes
import Network.Wai qualified as Wai

testEndpointTable :: [SomeApiRouteEndpoint]
testEndpointTable =
  [ SomeApiRouteEndpoint (testEndpoint ApiGet (at "/api/status") "ReadStatus"),
    SomeApiRouteEndpoint (testEndpoint ApiPost (at "/api/status") "WriteStatus"),
    SomeApiRouteEndpoint (testEndpoint ApiGet (at "/api/second") "ReadSecond"),
    SomeApiRouteEndpoint neverFailingEndpoint,
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

neverFailingEndpoint :: ApiRouteEndpoint () () domainFailure Text
neverFailingEndpoint =
  apiRouteEndpointAtNeverFailing
    ApiGet
    (at "/api/total")
    noRequestFields
    ApiNoRequestBody
    (textResponseEncoder :| [])
    (const (pure (apiResponse "Total")))

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

testHeaderValue :: Text -> ApiHeaderValue
testHeaderValue value = fromMaybe (error "expected test header value to be valid") (apiHeaderValue value)

testHeaderName :: Text -> ApiHeaderName
testHeaderName value = fromMaybe (error "expected test header name to be valid") (apiHeaderName value)

testMediaType :: Text -> ApiMediaType
testMediaType value = fromMaybe (error "expected test media type to be valid") (apiMediaType value)

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

spec =
  describe "HarchWeb.Api.Endpoint" $ do
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
              :| [ apiRouteResponseBody postResponse `shouldBe` "WriteStatus",
                   protocolResponseDatabaseOperations (apiRouteProtocolResponse getResponse) `shouldBe` []
                 ]
          )

      it "runs a never-failing endpoint without inventing a domain-failure renderer" $ do
        response <- runApiRouteEndpointGroup testEndpointTable (at "/api/total") (Wai.defaultRequest {Wai.requestMethod = "GET", Wai.rawPathInfo = "/api/total"})
        apiRouteResponseBody response `shouldBe` "Total"

      it "resolves HEAD to the declared GET endpoint's handler, same as the shared dispatcher's HEAD synthesis" $ do
        headResponse <- runApiRouteEndpointGroup testEndpointTable (at "/api/status") (Wai.defaultRequest {Wai.requestMethod = "HEAD", Wai.rawPathInfo = "/api/status"})
        apiRouteResponseBody headResponse `shouldBe` "ReadStatus"

      it "renders stable 405 responses for a method outside the route definition's declared policy" $ do
        deleteResponse <- runApiRouteEndpointGroup testEndpointTable (at "/api/status") (Wai.defaultRequest {Wai.requestMethod = "DELETE", Wai.rawPathInfo = "/api/status"})
        let postOnlyTable = [SomeApiRouteEndpoint (testEndpoint ApiPost (at "/api/post-only") "WriteOnly")]
        headResponse <- runApiRouteEndpointGroup postOnlyTable (at "/api/post-only") (Wai.defaultRequest {Wai.requestMethod = "HEAD", Wai.rawPathInfo = "/api/post-only"})
        malformedMethodResponse <- runApiRouteEndpointGroup testEndpointTable (at "/api/status") (Wai.defaultRequest {Wai.requestMethod = "\xFF", Wai.rawPathInfo = "/api/status"})
        expectAll
          ( (apiRouteResponseStatus deleteResponse `shouldBe` HttpTypes.status405)
              :| [ apiRouteResponseStatus headResponse `shouldBe` HttpTypes.status405,
                   apiRouteResponseStatus malformedMethodResponse `shouldBe` HttpTypes.status405,
                   apiRouteResponseHeaders deleteResponse `shouldBe` [("Allow", "GET, POST")],
                   apiRouteResponseBody deleteResponse `shouldBe` ""
                 ]
          )

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

    describe "apiResponseBodyToProtocolResponse" $ do
      it "converts status, headers, and body into the server protocol response" $
        apiResponseBodyToProtocolResponse
          ((apiTextResponse "hello") {apiResponseStatus = HttpTypes.status201, apiResponseHeaders = [("X-Example", testHeaderValue "present")]})
          `shouldBe` ProtocolResponse
            { protocolResponseStatus = HttpTypes.status201,
              protocolResponseHeaders = [("Content-Type", "text/plain; charset=utf-8"), ("X-Example", "present")],
              protocolResponseBody = ProtocolResponseBytes "hello",
              protocolResponseObservabilityAttributes = [],
              protocolResponseLogEntries = [],
              protocolResponseDatabaseOperations = []
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

    describe "apiRouteDefinition" $ do
      let requiredQuery = requiredField (queryField "q" apiTextValue)
          successfulEndpoint =
            apiRouteEndpointWithFieldFailure
              ApiPost
              requiredQuery
              ApiNoRequestBody
              (textResponseEncoder :| [])
              (apiResponse . Text.pack . show)
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
              :| [ apiRouteResponseBody response `shouldBe` "[MissingApiField ApiQuerySource \"q\"]",
                   remainingChunks `shouldBe` ["not consumed"]
                 ]
          )

      it "gives a field-failure renderer every accumulated error in declaration order" $ do
        let accumulatingEndpoint =
              apiRouteEndpointWithFieldFailure
                ApiGet
                ( (,)
                    <$> requiredField (queryField "query" apiTextValue)
                    <*> requiredField (headerField (testHeaderName "x-token") apiTextValue)
                )
                ApiNoRequestBody
                (textResponseEncoder :| [])
                (apiResponse . Text.pack . show)
                (const (pure (Right (apiResponse "unreachable"))))
                (\() -> apiResponse "unreachable")
        response <- runApiRoute accumulatingEndpoint Wai.defaultRequest
        expectAll
          ( (apiRouteEndpointPath accumulatingEndpoint `shouldBe` at "")
              :| [ apiRouteResponseStatus response `shouldBe` HttpTypes.status400,
                   apiRouteResponseBody response
                     `shouldBe` "[MissingApiField ApiQuerySource \"query\",MissingApiField ApiHeaderSource \"x-token\"]"
                 ]
          )

      it "lets a total handler declaration render typed field failures" $ do
        let totalEndpoint =
              apiRouteEndpointAtNeverFailingWithFieldFailure
                ApiGet
                (at "/api/total-field-failure")
                (requiredField (queryField "query" apiTextValue))
                ApiNoRequestBody
                (textResponseEncoder :| [])
                (apiResponse . Text.pack . show)
                (const (pure (apiResponse "unreachable")))
        response <- runApiRoute totalEndpoint Wai.defaultRequest
        expectAll
          ( (apiRouteEndpointPath totalEndpoint `shouldBe` at "/api/total-field-failure")
              :| [ routeMethods (apiRouteDefinition totalEndpoint) `shouldBe` [HarchWeb.RouteGet],
                   apiRouteResponseStatus response `shouldBe` HttpTypes.status400,
                   apiRouteResponseBody response `shouldBe` "[MissingApiField ApiQuerySource \"query\"]"
                 ]
          )

      it "retains the generic field response for the legacy total constructor" $ do
        let legacyEndpoint =
              apiRouteEndpointAtNeverFailing
                ApiGet
                (at "/api/legacy-total-field-failure")
                (requiredField (queryField "query" apiTextValue))
                ApiNoRequestBody
                (textResponseEncoder :| [])
                (const (pure (apiResponse "unreachable")))
        response <- runApiRoute legacyEndpoint Wai.defaultRequest
        expectAll
          ( (apiRouteResponseStatus response `shouldBe` HttpTypes.status400)
              :| [apiRouteResponseBody response `shouldBe` "API request fields were rejected."]
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
                              { apiEndpointResponseHeaders = [(testHeaderName "Cache-Control", testHeaderValue "no-store")]
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
                   apiRouteResponseBody unacceptableResponse `shouldBe` "\"API response has no acceptable representation.\"",
                   lookup "Content-Type" (apiRouteResponseHeaders unacceptableResponse) `shouldBe` Just "application/json; charset=utf-8",
                   protocolResponseObservabilityAttributes (apiRouteProtocolResponse unacceptableResponse) `shouldBe` [],
                   protocolResponseLogEntries (apiRouteProtocolResponse unacceptableResponse) `shouldBe` [],
                   protocolResponseDatabaseOperations (apiRouteProtocolResponse unacceptableResponse) `shouldBe` []
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
                              { apiEndpointResponseHeaders = [(testHeaderName "X-Trace", testHeaderValue "present"), (testHeaderName "vArY", testHeaderValue "Origin"), (testHeaderName "Cache-Control", testHeaderValue "no-store")]
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
                (\_ -> pure (Right ((apiResponse "hello") {apiEndpointResponseHeaders = [(testHeaderName "Vary", testHeaderValue "Accept")]})))
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
                   apiRouteResponseBody unacceptableResponse `shouldBe` "",
                   lookup "Content-Type" (apiRouteResponseHeaders unacceptableResponse) `shouldBe` Nothing,
                   protocolResponseObservabilityAttributes (apiRouteProtocolResponse unacceptableResponse) `shouldBe` [],
                   protocolResponseLogEntries (apiRouteProtocolResponse unacceptableResponse) `shouldBe` [],
                   protocolResponseDatabaseOperations (apiRouteProtocolResponse unacceptableResponse) `shouldBe` []
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
              (requiredField (queryField "query" apiTextValue))
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
        response <- routeResponse failingContextAwareEndpointDefinition (Wai.defaultRequest {Wai.queryString = [("query", Just "present")]}) (RouteRequest () ())
        expectAll
          ( (apiRouteResponseStatus response `shouldBe` HttpTypes.status422)
              :| [apiRouteResponseBody response `shouldBe` "context-aware domain failure"]
          )

      it "retains the generic field response for a legacy context route" $ do
        response <- routeResponse failingContextAwareEndpointDefinition Wai.defaultRequest (RouteRequest () ())
        expectAll
          ( (apiRouteResponseStatus response `shouldBe` HttpTypes.status400)
              :| [apiRouteResponseBody response `shouldBe` "API request fields were rejected."]
          )

      it "lets a context-aware declaration render its typed field failures" $ do
        let fieldFailureDefinition =
              apiRouteDefinitionWithContextWithFieldFailure
                ApiGet
                (requiredField (queryField "query" apiTextValue))
                ApiNoRequestBody
                (textResponseEncoder :| [])
                (apiResponse . Text.pack . show)
                ( \contextValue endpointRequest ->
                    case apiEndpointRequestFields endpointRequest of
                      "domain" -> pure (Left ())
                      fieldValue -> pure (Right (apiResponse (contextValue <> ":" <> fieldValue)))
                )
                (\() -> apiResponse "context failure")
        response <- routeResponse fieldFailureDefinition Wai.defaultRequest (RouteRequest () "context")
        acceptedResponse <- routeResponse fieldFailureDefinition (Wai.defaultRequest {Wai.queryString = [("query", Just "accepted")]}) (RouteRequest () "context")
        domainFailureResponse <- routeResponse fieldFailureDefinition (Wai.defaultRequest {Wai.queryString = [("query", Just "domain")]}) (RouteRequest () "context")
        expectAll
          ( (routeNavigationLabel fieldFailureDefinition `shouldBe` Nothing)
              :| [ routeMethods fieldFailureDefinition `shouldBe` [HarchWeb.RouteGet],
                   apiRouteResponseStatus response `shouldBe` HttpTypes.status400,
                   apiRouteResponseBody response `shouldBe` "[MissingApiField ApiQuerySource \"query\"]",
                   apiRouteResponseBody acceptedResponse `shouldBe` "context:accepted",
                   apiRouteResponseBody domainFailureResponse `shouldBe` "context failure"
                 ]
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

      it "retains the generic field response for a legacy total context route" $ do
        response <- runWithContext "context" Wai.defaultRequest
        expectAll
          ( (apiRouteResponseStatus response `shouldBe` HttpTypes.status400)
              :| [apiRouteResponseBody response `shouldBe` "API request fields were rejected."]
          )

      it "lets a total context-aware declaration render typed field failures" $ do
        let fieldFailureDefinition =
              apiRouteDefinitionWithContextNeverFailingWithFieldFailure
                ApiGet
                (requiredField (queryField "query" apiTextValue))
                ApiNoRequestBody
                (textResponseEncoder :| [])
                (apiResponse . Text.pack . show)
                (\contextValue endpointRequest -> pure (apiResponse (contextValue <> ":" <> apiEndpointRequestFields endpointRequest)))
        response <- routeResponse fieldFailureDefinition Wai.defaultRequest (RouteRequest () "context")
        acceptedResponse <- routeResponse fieldFailureDefinition (Wai.defaultRequest {Wai.queryString = [("query", Just "accepted")]}) (RouteRequest () "context")
        expectAll
          ( (routeNavigationLabel fieldFailureDefinition `shouldBe` Nothing)
              :| [ routeMethods fieldFailureDefinition `shouldBe` [HarchWeb.RouteGet],
                   apiRouteResponseStatus response `shouldBe` HttpTypes.status400,
                   apiRouteResponseBody response `shouldBe` "[MissingApiField ApiQuerySource \"query\"]",
                   apiRouteResponseBody acceptedResponse `shouldBe` "context:accepted"
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
              protocolResponseLogEntries = [],
              protocolResponseDatabaseOperations = []
            }

      it "uses an empty strict body for a protocol result without an API body" $
        case protocolResponseBody (apiHttpResponseToProtocolResponse (ApiHttpResponse HttpTypes.status405 [("Allow", testHeaderValue "GET, HEAD, OPTIONS")] Nothing)) of
          ProtocolResponseBytes bodyBytes -> bodyBytes `shouldBe` ""
          ProtocolResponseStream _ -> expectationFailure "expected a strict protocol body"
