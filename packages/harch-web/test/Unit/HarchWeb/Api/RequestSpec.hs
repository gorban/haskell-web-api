{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Api
import Network.Wai qualified as Wai

spec =
  describe "HarchWeb.Api.Request" $ do
    it "runs the named no-request-fields codec without errors" $
      expectDecoded (runRequestCodec noRequestFields (ApiRequestData [] [] [] [])) ()

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
                  apiRequestHeaders = [(validHeaderName "x-custom", "value")],
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

      it "drops malformed WAI header names before typed request decoding" $
        let request = Wai.defaultRequest {Wai.requestHeaders = [("Bad Name", "discarded"), ("X-Valid", "kept")]}
         in apiRequestDataFromWaiRequest request
              `shouldBe` ApiRequestData
                { apiRequestQueryParameters = [],
                  apiRequestHeaders = [(validHeaderName "x-valid", "kept")],
                  apiRequestCookies = [],
                  apiRequestFormFields = []
                }

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
                  apiRequestHeaders = [(validHeaderName "cookie", "session=first; theme=dark"), (validHeaderName "cookie", "session=second; malformed; bad name=ignored; Empty=")],
                  apiRequestCookies = [("session", "first"), ("theme", "dark"), ("session", "second"), ("Empty", "")],
                  apiRequestFormFields = []
                }

    describe "RequestCodec" $ do
      let sampleRequestData =
            ApiRequestData
              { apiRequestQueryParameters = [("q", "hello"), ("dup", "one"), ("dup", "two")],
                apiRequestHeaders = [(validHeaderName "X-Token", "secret"), (validHeaderName "X-Bad", "")],
                apiRequestCookies = [("session", "opaque"), ("repeat", "first"), ("repeat", "second")],
                apiRequestFormFields = [("name", "Ada"), ("repeat-form", "first"), ("repeat-form", "second")]
              }

      it "decodes a required, present field from its declared source" $
        expectDecoded (runRequestCodec (requiredField (queryField "q" apiTextValue)) sampleRequestData) "hello"

      it "decodes a required header field" $
        expectDecoded (runRequestCodec (requiredField (headerField (validHeaderName "X-Token") apiTextValue)) sampleRequestData) "secret"

      it "decodes a required case-sensitive cookie field" $
        expectAll
          ( expectDecoded (runRequestCodec (requiredField (cookieField "session" apiTextValue)) sampleRequestData) "opaque"
              :| [ expectRejected
                     (runRequestCodec (requiredField (cookieField "SESSION" apiTextValue)) sampleRequestData)
                     [MissingApiField ApiCookieSource "SESSION"],
                   expectRejected
                     (runRequestCodec (requiredField (cookieField "repeat" apiTextValue)) sampleRequestData)
                     [DuplicateApiField ApiCookieSource "repeat"]
                 ]
          )

      it "decodes a required form field and retains duplicate-form rejection" $
        expectAll
          ( expectDecoded (runRequestCodec (requiredField (formField "name" apiTextValue)) sampleRequestData) "Ada"
              :| [ expectRejected
                     (runRequestCodec (requiredField (formField "repeat-form" apiTextValue)) sampleRequestData)
                     [DuplicateApiField ApiFormSource "repeat-form"],
                   expectRejected
                     (runRequestCodec (requiredField (formField "missing-form" apiTextValue)) sampleRequestData)
                     [MissingApiField ApiFormSource "missing-form"]
                 ]
          )

      it "adds decoded form fields without discarding the original request sources" $
        case apiBodyDecoderParse (urlEncodedFormBodyDecoder 1) "name=Ada" of
          Left parseError -> expectationFailure (Text.unpack parseError)
          Right decodedForm ->
            expectDecoded
              ( runRequestCodec
                  ( (,)
                      <$> requiredField (queryField "q" apiTextValue)
                      <*> requiredField (formField "name" apiTextValue)
                  )
                  (apiRequestDataWithForm decodedForm sampleRequestData)
              )
              ("hello", "Ada")

      it "canonicalizes header names for equality and diagnostics" $
        let declaredName = validHeaderName "X-Token"
         in expectAll
              ( (Just declaredName `shouldBe` apiHeaderName "x-token")
                  :| [ declaredName `shouldNotBe` validHeaderName "x-other",
                       apiHeaderNameText declaredName `shouldBe` "x-token",
                       apiHeaderName "\8490" `shouldBe` Nothing,
                       apiHeaderName "k" `shouldBe` Just (validHeaderName "k"),
                       length (show declaredName) + length (showList [declaredName] "") `shouldSatisfy` (> 0)
                     ]
              )

      it "matches declared and extracted header names case-insensitively" $
        let request = Wai.defaultRequest {Wai.requestHeaders = [("X-Token", "secret")]}
         in expectDecoded
              (runRequestCodec (requiredField (headerField (validHeaderName "x-TOKEN") apiTextValue)) (apiRequestDataFromWaiRequest request))
              "secret"

      it "reports a missing required field" $
        expectRejected (runRequestCodec (requiredField (queryField "missing" apiTextValue)) sampleRequestData) [MissingApiField ApiQuerySource "missing"]

      it "reports a duplicate field" $
        expectRejected (runRequestCodec (requiredField (queryField "dup" apiTextValue)) sampleRequestData) [DuplicateApiField ApiQuerySource "dup"]

      it "reports an invalid field that fails its value parser" $
        expectRejected
          (runRequestCodec (requiredField (queryField "q" (parseApiField (const Nothing :: Text -> Maybe Text)))) sampleRequestData)
          [InvalidApiField ApiQuerySource "q"]

      it "resolves a present optional field to Just" $
        expectDecoded (runRequestCodec (optionalField (queryField "q" apiTextValue)) sampleRequestData) (Just "hello")

      it "resolves a missing optional field to Nothing without an error" $
        expectDecoded (runRequestCodec (optionalField (queryField "missing" apiTextValue)) sampleRequestData) Nothing

      it "keeps a duplicate optional field an error rather than silently defaulting" $
        expectRejected (runRequestCodec (optionalField (queryField "dup" apiTextValue)) sampleRequestData) [DuplicateApiField ApiQuerySource "dup"]

      it "resolves a present field-with-default to its value" $
        expectDecoded (runRequestCodec (fieldWithDefault "fallback" (queryField "q" apiTextValue)) sampleRequestData) "hello"

      it "resolves a missing field-with-default to its default" $
        expectDecoded (runRequestCodec (fieldWithDefault "fallback" (queryField "missing" apiTextValue)) sampleRequestData) "fallback"

      it "keeps a duplicate field-with-default an error rather than silently defaulting" $
        expectRejected (runRequestCodec (fieldWithDefault "fallback" (queryField "dup" apiTextValue)) sampleRequestData) [DuplicateApiField ApiQuerySource "dup"]

      it "accumulates independent errors from multiple required fields" $
        expectRejected
          ( runRequestCodec
              ( (,)
                  <$> requiredField (queryField "missing" apiTextValue)
                  <*> requiredField (headerField (validHeaderName "X-Missing") apiTextValue)
              )
              sampleRequestData
          )
          [MissingApiField ApiQuerySource "missing", MissingApiField ApiHeaderSource "x-missing"]

      it "keeps an explicit invalid codec result distinct from field errors" $
        case runRequestCodec (requestCodec (const ApiRequestCodecInvalid)) sampleRequestData of
          ApiRequestCodecInvalid -> pure ()
          ApiRequestDecoded _ -> expectationFailure "expected an explicit invalid codec result"
          ApiRequestRejected _ -> expectationFailure "expected an explicit invalid codec result"

      it "preserves one rejection when its independent applicative field succeeds" $
        let presentField = requiredField (queryField "q" apiTextValue)
            missingField = requiredField (queryField "missing" apiTextValue)
         in expectAll
              ( expectRejected
                  (runRequestCodec ((,) <$> missingField <*> presentField) sampleRequestData)
                  [MissingApiField ApiQuerySource "missing"]
                  :| [ expectRejected
                         (runRequestCodec ((,) <$> presentField <*> missingField) sampleRequestData)
                         [MissingApiField ApiQuerySource "missing"]
                     ]
              )

      it "sequences homogeneous field codecs through the standard Applicative interface" $
        let presentField = requiredField (queryField "q" apiTextValue)
         in expectDecoded
              (runRequestCodec (sequenceA [presentField, presentField]) sampleRequestData)
              ["hello", "hello"]

      it "maps and applies transformations through the standard codec interface" $
        let presentField = requiredField (queryField "q" apiTextValue)
         in expectAll
              ( expectDecoded
                  (runRequestCodec (Text.toUpper <$> presentField) sampleRequestData)
                  "HELLO"
                  :| [ expectDecoded
                         (runRequestCodec (pure "provided" :: RequestCodec Text) sampleRequestData)
                         "provided",
                       expectDecoded
                         (runRequestCodec (("answer=" <>) <$> presentField) sampleRequestData)
                         "answer=hello"
                     ]
              )

      it "keeps standard Functor and Applicative selection operations in the codec" $
        let presentField = requiredField (queryField "q" apiTextValue)
         in expectAll
              ( expectDecoded
                  (runRequestCodec (("constant" :: Text) <$ presentField) sampleRequestData)
                  "constant"
                  :| [ expectDecoded
                         (runRequestCodec (presentField <* presentField) sampleRequestData)
                         "hello",
                       expectDecoded
                         (runRequestCodec (presentField *> presentField) sampleRequestData)
                         "hello"
                     ]
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
                       (sampleRequestData /= sampleRequestData {apiRequestQueryParameters = []}) `shouldBe` True,
                       length (show sampleRequestData) + length (showList [sampleRequestData] "") `shouldSatisfy` (> 0)
                     ]
              )

    it "rejects non-token, empty, and non-ASCII header names" $
      map apiHeaderName ["", "Bad Name", "Bad:Name", "Bad\rName", "Bad\nName", "Bad\NULName", "\8490"]
        `shouldBe` replicate 7 Nothing

validHeaderName :: Text -> ApiHeaderName
validHeaderName name = fromMaybe (error "Expected a valid test header name") (apiHeaderName name)

expectDecoded :: (Eq value, Show value) => ApiRequestDecodeResult value -> value -> Expectation
expectDecoded result expectedValue =
  case result of
    ApiRequestDecoded actualValue -> actualValue `shouldBe` expectedValue
    ApiRequestRejected _ -> expectationFailure "expected a decoded API request value"
    ApiRequestCodecInvalid -> expectationFailure "expected a valid API request codec"

expectRejected :: ApiRequestDecodeResult value -> [ApiRequestParseError] -> Expectation
expectRejected result expectedErrors =
  case result of
    ApiRequestDecoded _ -> expectationFailure "expected API request field rejection"
    ApiRequestRejected actualErrors -> NonEmpty.toList actualErrors `shouldBe` expectedErrors
    ApiRequestCodecInvalid -> expectationFailure "expected API request field rejection"
