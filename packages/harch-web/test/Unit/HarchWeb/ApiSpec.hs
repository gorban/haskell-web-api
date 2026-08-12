{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWeb.ApiSpec (spec) where

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
import HarchWeb.Routing (RouteRequest (..))
import HarchWeb.Server (ProtocolResponse (..), ProtocolResponseBody (..), Response (..))
import HarchWeb.Site (RouteDefinition (..))
import Network.HTTP.Types qualified as HttpTypes
import Network.Wai qualified as Wai
import Network.Wai.Internal qualified as WaiInternal
import Test.Hspec
import TestCore.CustomAssertions (expectAll)

data TestTarget
  = ReadStatus
  | WriteStatus
  | ReadSecond
  deriving (Eq, Show)

testEndpoints :: [ApiEndpoint TestTarget]
testEndpoints =
  [ apiEndpoint ReadStatus ApiGet (at "/api/status"),
    apiEndpoint WriteStatus ApiPost (at "/api/status"),
    apiEndpoint ReadSecond ApiGet (at "/api/second")
  ]

allSampleMatchResults :: [ApiMatchResult TestTarget]
allSampleMatchResults =
  [ NoApiRouteMatch,
    ApiMethodNotAllowed (ApiGet :| [ApiPost]),
    ApiRouteMatched ReadStatus,
    ApiRouteMatchedHead ReadStatus,
    ApiRouteOptions (ApiGet :| [ApiPost])
  ]

-- | Invoke a WAI 'Wai.Application' and capture the 'Wai.Response' it
-- produces via the CPS-style 'Wai.Application' contract.
performWaiRequest :: Wai.Application -> Wai.Request -> IO Wai.Response
performWaiRequest webApplication request = do
  responseReference <- newIORef Nothing
  _ <- webApplication request (\response -> writeIORef responseReference (Just response) >> pure WaiInternal.ResponseReceived)
  maybeResponse <- readIORef responseReference
  pure (fromMaybe (error "expected WAI application to produce a response") maybeResponse)

readResponseBody :: Wai.Response -> IO Text
readResponseBody response = do
  let (_, _, withStreamingBody) = Wai.responseToStream response
  chunksReference <- newIORef []
  withStreamingBody $ \streamingBody ->
    streamingBody
      (\builder -> atomicModifyIORef' chunksReference (\chunks -> (chunks <> [Builder.toLazyByteString builder], ())))
      (pure ())
  chunks <- readIORef chunksReference
  pure (TextEncoding.decodeUtf8 (LazyByteString.toStrict (LazyByteString.concat chunks)))

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

runApiRoute :: ApiRouteEndpoint fields body domainFailure -> Wai.Request -> IO (Response () ())
runApiRoute endpoint request =
  routeResponse (apiRouteDefinition endpoint) request (RouteRequest () ())

spec :: Spec
spec =
  describe "HarchWeb.Api" $ do
    it "returns no route match for an undeclared path" $
      matchApiEndpoints "GET" "/api/unknown" testEndpoints `shouldBe` NoApiRouteMatch

    it "matches a declared method exactly" $
      expectAll
        ( (matchApiEndpoints "GET" "/api/status" testEndpoints `shouldBe` ApiRouteMatched ReadStatus)
            :| [ matchApiEndpoints "POST" "/api/status" testEndpoints `shouldBe` ApiRouteMatched WriteStatus,
                 matchApiEndpoints "GET" "/api/second" testEndpoints `shouldBe` ApiRouteMatched ReadSecond
               ]
        )

    it "reports method-not-allowed with every method declared at that path" $
      matchApiEndpoints "DELETE" "/api/status" testEndpoints
        `shouldBe` ApiMethodNotAllowed (ApiGet :| [ApiPost])

    it "synthesizes HEAD from a declared GET endpoint" $
      matchApiEndpoints "HEAD" "/api/status" testEndpoints
        `shouldBe` ApiRouteMatchedHead ReadStatus

    it "reports method-not-allowed for HEAD when no GET is declared at that path" $
      matchApiEndpoints "HEAD" "/api/write-only" [apiEndpoint WriteStatus ApiPost (at "/api/write-only")]
        `shouldBe` ApiMethodNotAllowed (ApiPost :| [])

    it "synthesizes OPTIONS from the declared method table without matching any declared endpoint" $
      matchApiEndpoints "OPTIONS" "/api/status" testEndpoints
        `shouldBe` ApiRouteOptions (ApiGet :| [ApiPost])

    it "reports no route match for OPTIONS on an undeclared path rather than synthesizing one" $
      matchApiEndpoints "OPTIONS" "/api/unknown" testEndpoints `shouldBe` NoApiRouteMatch

    it "renders the Allow header, synthesizing HEAD only alongside a declared GET, and always OPTIONS" $
      expectAll
        ( (apiAllowHeaderValue (ApiGet :| [ApiPost]) `shouldBe` "GET, POST, HEAD, OPTIONS")
            :| [ apiAllowHeaderValue (ApiPost :| []) `shouldBe` "POST, OPTIONS",
                 apiAllowHeaderValue (ApiDelete :| [ApiPut]) `shouldBe` "DELETE, PUT, OPTIONS"
               ]
        )

    it "renders every declared method to its RFC 9110 token" $
      expectAll
        ( (apiMethodText ApiGet `shouldBe` "GET")
            :| [ apiMethodText ApiPost `shouldBe` "POST",
                 apiMethodText ApiPut `shouldBe` "PUT",
                 apiMethodText ApiPatch `shouldBe` "PATCH",
                 apiMethodText ApiDelete `shouldBe` "DELETE"
               ]
        )

    it "derives comparable, printable representations for every declared type" $
      let methods = [ApiGet, ApiPost, ApiPut, ApiPatch, ApiDelete]
          paths = [at "/x", at "/y"]
       in expectAll
            ( (sum [fromEnum (left == right) | left <- methods, right <- methods] `shouldBe` length methods)
                :| [ sum [fromEnum (left /= right) | left <- methods, right <- methods] `shouldBe` length methods * (length methods - 1),
                     sum [length (show methodValue) + length (showList [methodValue] "") | methodValue <- methods] `shouldSatisfy` (> 0),
                     sum [fromEnum (left == right) | left <- paths, right <- paths] `shouldBe` length paths,
                     sum [fromEnum (left /= right) | left <- paths, right <- paths] `shouldBe` length paths * (length paths - 1),
                     sum [length (show pathValue) + length (showList [pathValue] "") | pathValue <- paths] `shouldSatisfy` (> 0),
                     sum [fromEnum (left == right) | left <- allSampleMatchResults, right <- allSampleMatchResults] `shouldBe` length allSampleMatchResults,
                     sum [fromEnum (left /= right) | left <- allSampleMatchResults, right <- allSampleMatchResults] `shouldBe` length allSampleMatchResults * (length allSampleMatchResults - 1),
                     sum [length (show matchResult) + length (showList [matchResult] "") | matchResult <- allSampleMatchResults] `shouldSatisfy` (> 0)
                   ]
            )

    describe "respondApiMatch" $ do
      it "renders a matched target's status and Content-Type, with its body" $
        respondApiMatch (const (apiTextResponse "hello")) (ApiRouteMatched ReadStatus)
          `shouldBe` ApiHttpResponse HttpTypes.status200 [("Content-Type", "text/plain; charset=utf-8")] (Just (apiTextResponse "hello"))

      it "renders a HEAD match with the same status and headers but no body" $
        respondApiMatch (const (apiTextResponse "hello")) (ApiRouteMatchedHead ReadStatus)
          `shouldBe` ApiHttpResponse HttpTypes.status200 [("Content-Type", "text/plain; charset=utf-8")] Nothing

      it "renders 404 with no headers or body for no route match" $
        respondApiMatch (const (apiTextResponse "unused")) NoApiRouteMatch
          `shouldBe` ApiHttpResponse HttpTypes.status404 [] Nothing

      it "renders 405 with an Allow header derived from the declared methods" $
        respondApiMatch (const (apiTextResponse "unused")) (ApiMethodNotAllowed (ApiGet :| [ApiPost]))
          `shouldBe` ApiHttpResponse HttpTypes.status405 [("Allow", "GET, POST, HEAD, OPTIONS")] Nothing

      it "renders 204 with an Allow header and no body for a synthesized OPTIONS match" $
        respondApiMatch (const (apiTextResponse "unused")) (ApiRouteOptions (ApiGet :| [ApiPost]))
          `shouldBe` ApiHttpResponse HttpTypes.status204 [("Allow", "GET, POST, HEAD, OPTIONS")] Nothing

      it "forwards a matched target's overridden status, e.g. 422 for semantically invalid input" $
        let invalidBody = (apiTextResponse "invalid") {apiResponseStatus = HttpTypes.status422}
         in respondApiMatch (const invalidBody) (ApiRouteMatched ReadStatus)
              `shouldBe` ApiHttpResponse HttpTypes.status422 [("Content-Type", "text/plain; charset=utf-8")] (Just invalidBody)

      it "derives comparable, printable representations for ApiHttpResponse" $
        let responses =
              [ respondApiMatch (const (apiTextResponse "hello")) (ApiRouteMatched ReadStatus),
                respondApiMatch (const (apiTextResponse "hello")) (ApiRouteMatchedHead ReadStatus),
                respondApiMatch (const (apiTextResponse "unused")) NoApiRouteMatch,
                respondApiMatch (const (apiTextResponse "unused")) (ApiMethodNotAllowed (ApiGet :| []))
              ]
         in expectAll
              ( (sum [fromEnum (left == right) | left <- responses, right <- responses] `shouldBe` length responses)
                  :| [ sum [fromEnum (left /= right) | left <- responses, right <- responses]
                         `shouldBe` length responses * (length responses - 1),
                       sum [length (show r) + length (showList [r] "") | r <- responses] `shouldSatisfy` (> 0)
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
                  apiRequestHeaders = [(apiHeaderName "x-custom", "value")]
                }

      it "decodes a flag-style query parameter with no value as empty rather than dropping it" $
        let request = Wai.defaultRequest {Wai.queryString = [("flag", Nothing)]}
         in apiRequestDataFromWaiRequest request
              `shouldBe` ApiRequestData {apiRequestQueryParameters = [("flag", "")], apiRequestHeaders = []}

      it "decodes invalid UTF-8 in a query value leniently rather than failing" $
        let request = Wai.defaultRequest {Wai.queryString = [("q", Just "bad\xFF")]}
         in apiRequestDataFromWaiRequest request
              `shouldBe` ApiRequestData {apiRequestQueryParameters = [("q", "bad\65533")], apiRequestHeaders = []}

    describe "apiRouteDefinition" $ do
      let requiredQuery = requiredField (queryField "q" apiTextValue)
          successfulEndpoint =
            apiRouteEndpoint
              ApiPost
              requiredQuery
              ApiNoRequestBody
              (pure . Right . apiTextResponse . apiEndpointRequestFields)
              (\() -> apiTextResponse "unreachable")
          domainFailureEndpoint =
            apiRouteEndpoint
              ApiGet
              (pure ())
              ApiNoRequestBody
              (const (pure (Left ())))
              (\() -> (apiTextResponse "domain failure") {apiResponseStatus = HttpTypes.status422})

      it "declares its one method in the shared route table" $
        expectAll
          ( (routeMethods (apiRouteDefinition successfulEndpoint) `shouldBe` [HarchWeb.RoutePost])
              :| [ routeMethods (apiRouteDefinition domainFailureEndpoint) `shouldBe` [HarchWeb.RouteGet],
                   routeMethods (apiRouteDefinition (apiRouteEndpoint ApiPut (pure ()) ApiNoRequestBody (const (pure (Right (apiTextResponse "")))) (\() -> apiTextResponse ""))) `shouldBe` [HarchWeb.RoutePut],
                   routeMethods (apiRouteDefinition (apiRouteEndpoint ApiPatch (pure ()) ApiNoRequestBody (const (pure (Right (apiTextResponse "")))) (\() -> apiTextResponse ""))) `shouldBe` [HarchWeb.RoutePatch],
                   routeMethods (apiRouteDefinition (apiRouteEndpoint ApiDelete (pure ()) ApiNoRequestBody (const (pure (Right (apiTextResponse "")))) (\() -> apiTextResponse ""))) `shouldBe` [HarchWeb.RouteDelete]
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
              :| [apiRouteResponseBody response `shouldBe` "accepted"]
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
                (pure . Right . apiTextResponse . apiEndpointRequestBody)
                (\() -> apiTextResponse "unreachable")
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
                   apiRouteResponseBody acceptedResponse `shouldBe` "ok"
                 ]
          )

    describe "apiHttpResponseToWaiResponse" $ do
      it "renders a matched response's status, headers, and body" $ do
        let waiResponse = apiHttpResponseToWaiResponse (ApiHttpResponse HttpTypes.status200 [("Content-Type", "text/plain")] (Just (apiTextResponse "hello")))
        body <- readResponseBody waiResponse
        expectAll
          ( (Wai.responseStatus waiResponse `shouldBe` HttpTypes.status200)
              :| [ Wai.responseHeaders waiResponse `shouldBe` [("Content-Type", "text/plain")],
                   body `shouldBe` "hello"
                 ]
          )

      it "renders 204, 400, 403, 404, 405, and 422 with their standard reason phrases" $
        expectAll
          ( (Wai.responseStatus (apiHttpResponseToWaiResponse (ApiHttpResponse HttpTypes.status204 [] Nothing)) `shouldBe` HttpTypes.status204)
              :| [ Wai.responseStatus (apiHttpResponseToWaiResponse (ApiHttpResponse HttpTypes.status400 [] Nothing)) `shouldBe` HttpTypes.status400,
                   Wai.responseStatus (apiHttpResponseToWaiResponse (ApiHttpResponse HttpTypes.status403 [] Nothing)) `shouldBe` HttpTypes.status403,
                   Wai.responseStatus (apiHttpResponseToWaiResponse (ApiHttpResponse HttpTypes.status404 [] Nothing)) `shouldBe` HttpTypes.status404,
                   Wai.responseStatus (apiHttpResponseToWaiResponse (ApiHttpResponse HttpTypes.status405 [] Nothing)) `shouldBe` HttpTypes.status405,
                   Wai.responseStatus (apiHttpResponseToWaiResponse (ApiHttpResponse HttpTypes.status422 [] Nothing)) `shouldBe` HttpTypes.status422
                 ]
          )

      it "renders an empty body when no body is present" $ do
        body <- readResponseBody (apiHttpResponseToWaiResponse (ApiHttpResponse HttpTypes.status404 [] Nothing))
        body `shouldBe` ""

      it "preserves the reason phrase of a supplied HTTP status" $
        HttpTypes.statusMessage (Wai.responseStatus (apiHttpResponseToWaiResponse (ApiHttpResponse HttpTypes.status500 [] Nothing))) `shouldBe` "Internal Server Error"

    describe "apiHttpResponseToProtocolResponse" $ do
      it "preserves a matched response's status, headers, and strict protocol bytes" $
        apiHttpResponseToProtocolResponse
          (ApiHttpResponse HttpTypes.status201 [("Content-Type", "application/example"), ("X-Example", "present")] (Just (apiBytesResponse (apiContentType (testMediaType "application/example")) "\NUL\SOH\STX")))
          `shouldBe` ProtocolResponse
            { protocolResponseStatus = HttpTypes.status201,
              protocolResponseHeaders = [("Content-Type", "application/example"), ("X-Example", "present")],
              protocolResponseBody = ProtocolResponseBytes "\NUL\SOH\STX",
              protocolResponseObservabilityAttributes = [],
              protocolResponseLogEntries = []
            }

      it "uses an empty strict body for a protocol result without an API body" $
        case protocolResponseBody (apiHttpResponseToProtocolResponse (ApiHttpResponse HttpTypes.status405 [("Allow", "GET, HEAD, OPTIONS")] Nothing)) of
          ProtocolResponseBytes bodyBytes -> bodyBytes `shouldBe` ""
          ProtocolResponseStream _ -> expectationFailure "expected a strict protocol body"

    describe "apiEndpointMiddleware" $ do
      let innerApplication :: Wai.Application
          innerApplication _ respond = respond (Wai.responseLBS HttpTypes.status200 [] "inner application")
          middleware = apiEndpointMiddleware testEndpoints (\_request _target -> pure (apiTextResponse "handled"))
          waiRequestFor requestMethod requestPath =
            Wai.defaultRequest {Wai.requestMethod = requestMethod, Wai.rawPathInfo = requestPath}

      it "dispatches a matched request through the endpoint table rather than the inner application" $ do
        response <- performWaiRequest (middleware innerApplication) (waiRequestFor "GET" "/api/status")
        body <- readResponseBody response
        expectAll
          ( (Wai.responseStatus response `shouldBe` HttpTypes.status200)
              :| [body `shouldBe` "handled"]
          )

      it "gives the matched target's handler the original request, not just the matched target" $ do
        let echoHeaderMiddleware =
              apiEndpointMiddleware
                testEndpoints
                (\request _target -> pure (apiTextResponse (TextEncoding.decodeUtf8 (fromMaybe "missing" (lookup "X-Probe" (Wai.requestHeaders request))))))
            probeRequest = (waiRequestFor "GET" "/api/status") {Wai.requestHeaders = [("X-Probe", "seen")]}
        response <- performWaiRequest (echoHeaderMiddleware innerApplication) probeRequest
        body <- readResponseBody response
        body `shouldBe` "seen"

      it "renders 405 with Allow for a declared path with the wrong method" $ do
        response <- performWaiRequest (middleware innerApplication) (waiRequestFor "DELETE" "/api/status")
        expectAll
          ( (Wai.responseStatus response `shouldBe` HttpTypes.status405)
              :| [Wai.responseHeaders response `shouldBe` [("Allow", "GET, POST, HEAD, OPTIONS")]]
          )

      it "omits the body for a HEAD match while keeping its status and headers" $ do
        response <- performWaiRequest (middleware innerApplication) (waiRequestFor "HEAD" "/api/status")
        body <- readResponseBody response
        expectAll
          ( (Wai.responseStatus response `shouldBe` HttpTypes.status200)
              :| [body `shouldBe` ""]
          )

      it "answers OPTIONS with 204, an Allow header, and no body without running any handler" $ do
        response <- performWaiRequest (middleware innerApplication) (waiRequestFor "OPTIONS" "/api/status")
        body <- readResponseBody response
        expectAll
          ( (Wai.responseStatus response `shouldBe` HttpTypes.status204)
              :| [ Wai.responseHeaders response `shouldBe` [("Allow", "GET, POST, HEAD, OPTIONS")],
                   body `shouldBe` ""
                 ]
          )

      it "falls through to the inner application for a path no endpoint declares" $ do
        response <- performWaiRequest (middleware innerApplication) (waiRequestFor "GET" "/unrelated")
        body <- readResponseBody response
        body `shouldBe` "inner application"

      it "handles malformed method and path bytes without crashing the middleware" $ do
        malformedMethodResponse <- performWaiRequest (middleware innerApplication) (waiRequestFor "\xFF" "/api/status")
        malformedPathResponse <- performWaiRequest (middleware innerApplication) (waiRequestFor "GET" "/api/\xFF")
        malformedPathBody <- readResponseBody malformedPathResponse
        expectAll
          ( (Wai.responseStatus malformedMethodResponse `shouldBe` HttpTypes.status405)
              :| [ Wai.responseStatus malformedPathResponse `shouldBe` HttpTypes.status200,
                   malformedPathBody `shouldBe` "inner application"
                 ]
          )

      it "forwards a target's overridden status through to the real WAI response" $ do
        let invalidMiddleware =
              apiEndpointMiddleware
                testEndpoints
                (\_request _target -> pure (apiTextResponse "invalid") {apiResponseStatus = HttpTypes.status422})
        response <- performWaiRequest (invalidMiddleware innerApplication) (waiRequestFor "GET" "/api/status")
        Wai.responseStatus response `shouldBe` HttpTypes.status422

    describe "RequestCodec" $ do
      let sampleRequestData =
            ApiRequestData
              { apiRequestQueryParameters = [("q", "hello"), ("dup", "one"), ("dup", "two")],
                apiRequestHeaders = [(apiHeaderName "X-Token", "secret"), (apiHeaderName "X-Bad", "")]
              }

      it "decodes a required, present field from its declared source" $
        runRequestCodec (requiredField (queryField "q" apiTextValue)) sampleRequestData
          `shouldBe` ([], Just "hello")

      it "decodes a required header field" $
        runRequestCodec (requiredField (headerField (apiHeaderName "X-Token") apiTextValue)) sampleRequestData
          `shouldBe` ([], Just "secret")

      it "canonicalizes header names for equality and diagnostics" $
        let declaredName = apiHeaderName "X-Token"
         in expectAll
              ( (declaredName `shouldBe` apiHeaderName "x-token")
                  :| [ declaredName `shouldNotBe` apiHeaderName "x-other",
                       apiHeaderNameText declaredName `shouldBe` "x-token",
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
        let sources = [ApiQuerySource, ApiHeaderSource]
            parseErrors =
              [ MissingApiField ApiQuerySource "q",
                DuplicateApiField ApiHeaderSource "h",
                InvalidApiField ApiQuerySource "q"
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
                   testMediaType "application/json" == testMediaType "application/json" `shouldBe` True,
                   testMediaType "application/json" /= testMediaType "text/plain" `shouldBe` True,
                   show (testMediaType "application/json") `shouldSatisfy` (not . null),
                   apiMediaType "not-a-media-type" `shouldBe` Nothing,
                   apiMediaType "text" `shouldBe` Nothing
                 ]
          )

      it "decodes a JSON body when Content-Type matches" $
        selectApiBodyDecoder RejectMissingContentType 1024 [jsonDecoder] (Just "application/json") "42"
          `shouldBe` ApiDecodedBody 42

      it "decodes a JSON body when Content-Type includes parameters" $
        selectApiBodyDecoder RejectMissingContentType 1024 [jsonDecoder] (Just "application/json; charset=utf-8") "7"
          `shouldBe` ApiDecodedBody 7

      it "matches Content-Type case-insensitively" $
        selectApiBodyDecoder RejectMissingContentType 1024 [jsonDecoder] (Just "APPLICATION/JSON") "1"
          `shouldBe` ApiDecodedBody 1

      it "reports unsupported media type for an undeclared Content-Type" $
        selectApiBodyDecoder RejectMissingContentType 1024 [jsonDecoder] (Just "text/plain") "3"
          `shouldBe` ApiUnsupportedMediaType [testMediaType "application/json"]

      it "reports unsupported media type for a malformed Content-Type header" $
        selectApiBodyDecoder RejectMissingContentType 1024 [jsonDecoder] (Just "garbage") "3"
          `shouldBe` ApiUnsupportedMediaType [testMediaType "application/json"]

      it "rejects a missing Content-Type when the policy requires one" $
        selectApiBodyDecoder RejectMissingContentType 1024 [jsonDecoder] Nothing "42"
          `shouldBe` ApiUnsupportedMediaType [testMediaType "application/json"]

      it "assumes a declared media type when Content-Type is missing and the policy allows it" $
        selectApiBodyDecoder (AssumeMediaType (testMediaType "application/json")) 1024 [jsonDecoder] Nothing "42"
          `shouldBe` ApiDecodedBody 42

      it "reports a malformed body when the selected decoder rejects the syntax" $
        selectApiBodyDecoder RejectMissingContentType 1024 [jsonDecoder] (Just "application/json") "not json"
          `shouldBe` ApiMalformedBody

      it "carries a non-empty error message when the JSON decoder itself rejects a body" $
        apiBodyDecoderParse jsonDecoder "not json" `shouldSatisfy` \case
          Left errorMessage -> not (Text.null errorMessage)
          Right (_ :: Int) -> False

      it "reports a body exceeding the declared byte limit as too large, without decoding it" $
        selectApiBodyDecoder RejectMissingContentType 2 [jsonDecoder] (Just "application/json") "12345"
          `shouldBe` ApiBodyTooLarge

      it "decodes a strict-UTF-8 text/plain body" $
        selectApiBodyDecoder RejectMissingContentType 1024 [textBodyDecoder] (Just "text/plain") "hello"
          `shouldBe` ApiDecodedBody "hello"

      it "reports a malformed body for invalid UTF-8 in a text/plain body" $
        selectApiBodyDecoder RejectMissingContentType 1024 [textBodyDecoder] (Just "text/plain") "bad\xFF"
          `shouldBe` ApiMalformedBody

      it "carries a fixed error message when the text decoder itself rejects invalid UTF-8" $
        apiBodyDecoderParse textBodyDecoder "bad\xFF" `shouldBe` Left "invalid UTF-8 body"

      it "passes a body through unparsed for a declared bytes media type" $
        selectApiBodyDecoder RejectMissingContentType 1024 [bytesBodyDecoder (testMediaType "application/octet-stream")] (Just "application/octet-stream") "\1\2\3"
          `shouldBe` ApiDecodedBody "\1\2\3"

      it "derives comparable, printable representations for MissingContentTypePolicy and ApiBodyOutcome" $
        let policies = [RejectMissingContentType, AssumeMediaType (testMediaType "application/json")]
            outcomes = [ApiUnsupportedMediaType [testMediaType "application/json"], ApiBodyTooLarge, ApiMalformedBody, ApiDecodedBody (1 :: Int)]
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

      it "defaults every built-in response body to status 200" $
        expectAll
          ( (apiResponseStatus (apiJsonResponse (42 :: Int)) `shouldBe` HttpTypes.status200)
              :| [ apiResponseStatus (apiTextResponse "hello") `shouldBe` HttpTypes.status200,
                   apiResponseStatus (apiBytesResponse (apiContentType (testMediaType "image/svg+xml")) "<svg/>") `shouldBe` HttpTypes.status200
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

      it "defaults an unparsable quality value to 1.0 and drops a malformed entry" $
        expectAll
          ( (map acceptedRangeQuality (parseAcceptHeader "text/plain;q=nope") `shouldBe` [1.0])
              :| [parseAcceptHeader "not-a-media-type, text/plain" `shouldBe` [AcceptedRange "text" "plain" [] 1.0]]
          )

      it "derives comparable, printable representations for negotiation types" $
        let ranges = parseAcceptHeader "text/plain;q=0.5;level=1, application/json"
            results = [NoAcceptableRepresentation, SelectedRepresentation (testMediaType "application/json")]
         in expectAll
              ( (sum [fromEnum (left == right) | left <- ranges, right <- ranges] `shouldBe` length ranges)
                  :| [ sum [fromEnum (left /= right) | left <- ranges, right <- ranges] `shouldBe` length ranges * (length ranges - 1),
                       sum [length (show rangeValue) + length (showList [rangeValue] "") | rangeValue <- ranges] `shouldSatisfy` (> 0),
                       sum [fromEnum (left == right) | left <- results, right <- results] `shouldBe` length results,
                       sum [fromEnum (left /= right) | left <- results, right <- results] `shouldBe` length results * (length results - 1),
                       sum [length (show resultValue) + length (showList [resultValue] "") | resultValue <- results] `shouldSatisfy` (> 0)
                     ]
              )
