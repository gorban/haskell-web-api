{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Exception (ErrorCall (..), evaluate)
import Control.Monad (forM_)
import Data.ByteString qualified as ByteString
import Data.IORef (newIORef, readIORef)
import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Api
import Network.HTTP.Types qualified as HttpTypes

testMediaType :: Text -> ApiMediaType
testMediaType value = fromMaybe (error "expected test media type to be valid") (apiMediaType value)

testHeaderValue :: Text -> ApiHeaderValue
testHeaderValue value = fromMaybe (error "expected test header value to be valid") (apiHeaderValue value)

testHeaderName :: Text -> ApiHeaderName
testHeaderName value = fromMaybe (error "expected test header name to be valid") (apiHeaderName value)

strictEncodedResponseBytes :: ApiEncodedResponseBody -> ByteString.ByteString
strictEncodedResponseBytes encodedResponse =
  case encodedResponse of
    ApiEncodedResponseBytes bodyBytes -> bodyBytes
    ApiEncodedResponseStream _ -> error "expected a strict encoded response"

spec =
  describe "HarchWeb.Api.Response" $ do
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
                   requireApiMediaType " Application/JSON " `shouldBe` testMediaType "application/json",
                   show (testMediaType "application/json") `shouldSatisfy` (not . null),
                   showList [testMediaType "application/json", testMediaType "text/plain"] "" `shouldSatisfy` (not . null),
                   apiMediaType "not-a-media-type" `shouldBe` Nothing,
                   apiMediaType "text" `shouldBe` Nothing
                 ]
          )

      it "fails clearly when an application declares an invalid required media type" $
        evaluate (requireApiMediaType "not-a-media-type") `shouldThrow` \case
          ErrorCall message -> "invalid declared media type: not-a-media-type" `isInfixOf` message

      -- Tabled per docs/design-guidance.md's CN decision record: one act
      -- (selectApiBodyDecoder against [jsonDecoder]), one comparison,
      -- differing only in the policy, Content-Type, body, and expected
      -- outcome. The text/plain and bytes decoder it blocks below use a
      -- different ApiBodyOutcome result type per decoder (Text,
      -- ByteString) and stay their own it blocks rather than widen this
      -- table to an existential.
      [ ("decodes a JSON body when Content-Type matches", RejectMissingContentType, Just "application/json", "42", ApiDecodedBody 42),
        ("decodes a JSON body when Content-Type includes parameters", RejectMissingContentType, Just "application/json; charset=utf-8", "7", ApiDecodedBody 7),
        ("matches Content-Type case-insensitively", RejectMissingContentType, Just "APPLICATION/JSON", "1", ApiDecodedBody 1),
        ("reports unsupported media type for an undeclared Content-Type", RejectMissingContentType, Just "text/plain", "3", ApiUnsupportedMediaType [testMediaType "application/json"]),
        ("reports unsupported media type for a malformed Content-Type header", RejectMissingContentType, Just "garbage", "3", ApiUnsupportedMediaType [testMediaType "application/json"]),
        ("rejects a missing Content-Type when the policy requires one", RejectMissingContentType, Nothing, "42", ApiUnsupportedMediaType [testMediaType "application/json"]),
        ("assumes a declared media type when Content-Type is missing and the policy allows it", AssumeMediaType (testMediaType "application/json"), Nothing, "42", ApiDecodedBody 42),
        ("reports a malformed body when the selected decoder rejects the syntax", RejectMissingContentType, Just "application/json", "not json", ApiMalformedBody)
        ]
        `forM_` \(label, policy, contentType, body, expected) ->
          it label $
            selectApiBodyDecoder policy [jsonDecoder] contentType body `shouldBe` expected

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
                       apiBodyDecoderParse formDecoder "one=1&two=2&three=%"
                         `shouldBe` Left "form contains more fields than declared",
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
                case ( runApiFormCodec (requiredField (formField "name" apiTextValue)) decodedForm,
                       runApiFormCodec (optionalField (queryField "q" apiTextValue)) decodedForm,
                       runApiFormCodec (optionalField (headerField (testHeaderName "X-Test") apiTextValue)) decodedForm,
                       runApiFormCodec (optionalField (cookieField "session" apiTextValue)) decodedForm
                     ) of
                  (ApiRequestDecoded name, ApiRequestDecoded queryValue, ApiRequestDecoded headerValue, ApiRequestDecoded cookieValue) ->
                    expectAll
                      ( (name `shouldBe` "Ada")
                          :| [ queryValue `shouldBe` Nothing,
                               headerValue `shouldBe` Nothing,
                               cookieValue `shouldBe` Nothing
                             ]
                      )
                  _ -> expectationFailure "expected every form codec to decode successfully"

      it "derives comparable, printable representations for MissingContentTypePolicy and ApiBodyOutcome" $
        let policies = [RejectMissingContentType, AssumeMediaType (testMediaType "application/json")]
            outcomes = [ApiUnsupportedMediaType [testMediaType "application/json"], ApiMalformedBody, ApiDecodedBody (1 :: Int)]
         in expectAll
              ( (sum [fromEnum (left == right) | left <- policies, right <- policies] `shouldBe` length policies)
                  :| [ sum [fromEnum (left /= right) | left <- policies, right <- policies]
                         `shouldBe` length policies
                         * (length policies - 1),
                       sum [length (show p) + length (showList [p] "") | p <- policies] `shouldSatisfy` (> 0),
                       sum [fromEnum (left == right) | left <- outcomes, right <- outcomes] `shouldBe` length outcomes,
                       sum [fromEnum (left /= right) | left <- outcomes, right <- outcomes]
                         `shouldBe` length outcomes
                         * (length outcomes - 1),
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
