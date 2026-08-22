{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWeb.Api.RequestSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Api
import Network.Wai qualified as Wai
import Test.Hspec
import TestCore.CustomAssertions (expectAll)

spec :: Spec
spec =
  describe "HarchWeb.Api.Request" $ do
    it "runs the named no-request-fields codec without errors" $
      runRequestCodec noRequestFields (ApiRequestData [] [] [] []) `shouldBe` ([], Just ())

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
                       (sampleRequestData /= sampleRequestData {apiRequestQueryParameters = []}) `shouldBe` True,
                       length (show sampleRequestData) + length (showList [sampleRequestData] "") `shouldSatisfy` (> 0)
                     ]
              )
