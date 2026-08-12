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
import Data.Text.Encoding.Error qualified as TextEncodingError
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

apiRouteResponseHeaders :: Response route context -> HttpTypes.ResponseHeaders
apiRouteResponseHeaders response =
  case response of
    ProtocolResponseResult protocolResponse -> protocolResponseHeaders protocolResponse
    _ -> error "expected API route to render a protocol response"

runApiRoute :: ApiRouteEndpoint fields body domainFailure response -> Wai.Request -> IO (Response () ())
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
        let renderTarget target =
              case target of
                ReadStatus -> apiTextResponse "ReadStatus"
                WriteStatus -> apiTextResponse "WriteStatus"
                ReadSecond -> apiTextResponse "ReadSecond"
         in respondApiMatch renderTarget (ApiRouteMatched ReadStatus)
              `shouldBe` ApiHttpResponse HttpTypes.status200 [("Content-Type", "text/plain; charset=utf-8")] (Just (apiTextResponse "ReadStatus"))

      it "renders a HEAD match with the same status and headers but no body" $
        let renderTarget target =
              case target of
                ReadStatus -> apiTextResponse "hello"
                WriteStatus -> apiTextResponse "write"
                ReadSecond -> apiTextResponse "second"
         in respondApiMatch renderTarget (ApiRouteMatchedHead ReadStatus)
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
                   lookup "Vary" (apiRouteResponseHeaders response) `shouldBe` Nothing
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
                              { apiEndpointResponseHeaders = [("Cache-Control", "no-store")]
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
                              { apiEndpointResponseHeaders = [("X-Trace", "present"), ("vArY", "Origin"), ("Cache-Control", "no-store")]
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
                (\_ -> pure (Right ((apiResponse "hello") {apiEndpointResponseHeaders = [("Vary", "Accept")]})))
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
          innerApplication request respond = respond (Wai.responseLBS HttpTypes.status200 [] (LazyByteString.fromStrict (TextEncoding.encodeUtf8 (TextEncoding.decodeUtf8With TextEncodingError.lenientDecode (Wai.rawPathInfo request)))))
          middleware = apiEndpointMiddleware testEndpoints (\_request target -> pure (renderTarget target))
          waiRequestFor requestMethod requestPath =
            Wai.defaultRequest {Wai.requestMethod = requestMethod, Wai.rawPathInfo = requestPath}
          renderTarget target =
            case target of
              ReadStatus -> apiTextResponse "ReadStatus"
              WriteStatus -> apiTextResponse "WriteStatus"
              ReadSecond -> apiTextResponse "ReadSecond"

      it "dispatches a matched request through the endpoint table rather than the inner application" $ do
        response <- performWaiRequest (middleware innerApplication) (waiRequestFor "GET" "/api/status")
        body <- readResponseBody response
        expectAll
          ( (Wai.responseStatus response `shouldBe` HttpTypes.status200)
              :| [body `shouldBe` "ReadStatus"]
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
        body <- readResponseBody response
        expectAll
          ( (Wai.responseStatus response `shouldBe` HttpTypes.status405)
              :| [ Wai.responseHeaders response `shouldBe` [("Allow", "GET, POST, HEAD, OPTIONS")],
                   body `shouldBe` ""
                 ]
          )

      it "omits the body for a HEAD match while keeping its status and headers" $ do
        renderedTargets <- newIORef []
        let recordingMiddleware =
              apiEndpointMiddleware
                testEndpoints
                (\_request target -> atomicModifyIORef' renderedTargets (\targets -> (targets <> [target], headRenderTarget target)))
            headRenderTarget target =
              case target of
                ReadStatus -> (apiTextResponse "handled") {apiResponseStatus = HttpTypes.status201}
                WriteStatus -> (apiTextResponse "write") {apiResponseStatus = HttpTypes.status202}
                ReadSecond -> (apiTextResponse "second") {apiResponseStatus = HttpTypes.status203}
        response <- performWaiRequest (recordingMiddleware innerApplication) (waiRequestFor "HEAD" "/api/status")
        body <- readResponseBody response
        targets <- readIORef renderedTargets
        expectAll
          ( (Wai.responseStatus response `shouldBe` HttpTypes.status201)
              :| [body `shouldBe` "", targets `shouldBe` [ReadStatus]]
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
        body `shouldBe` "/unrelated"

      it "handles malformed method and path bytes without crashing the middleware" $ do
        malformedMethodResponse <- performWaiRequest (middleware innerApplication) (waiRequestFor "\xFF" "/api/status")
        malformedPathResponse <- performWaiRequest (middleware innerApplication) (waiRequestFor "GET" "/api/\xFF")
        malformedPathBody <- readResponseBody malformedPathResponse
        expectAll
          ( (Wai.responseStatus malformedMethodResponse `shouldBe` HttpTypes.status405)
              :| [ Wai.responseStatus malformedPathResponse `shouldBe` HttpTypes.status200,
                   malformedPathBody `shouldBe` "/api/\65533"
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
                       apiEndpointResponseValue responseValue `shouldBe` ("hello" :: Text),
                       apiResponseEncoderEncode jsonResponseEncoder ("hello" :: Text) `shouldBe` "\"hello\"",
                       apiResponseEncoderEncode textResponseEncoder "hello" `shouldBe` "hello",
                       apiResponseEncoderEncode (bytesResponseEncoder svgContentType) "<svg/>" `shouldBe` "<svg/>",
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
