{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWeb.ApiSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import HarchWeb.Api
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
    ApiRouteMatchedHead ReadStatus
  ]

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

    describe "RequestCodec" $ do
      let sampleRequestData =
            ApiRequestData
              { apiRequestQueryParameters = [("q", "hello"), ("dup", "one"), ("dup", "two")],
                apiRequestHeaders = [("X-Token", "secret"), ("X-Bad", "")]
              }

      it "decodes a required, present field from its declared source" $
        runRequestCodec (requiredField (queryField "q" apiTextValue)) sampleRequestData
          `shouldBe` ([], Just "hello")

      it "decodes a required header field" $
        runRequestCodec (requiredField (headerField "X-Token" apiTextValue)) sampleRequestData
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
              <*> requiredField (headerField "X-Missing" apiTextValue)
          )
          sampleRequestData
          `shouldBe` ( [MissingApiField ApiQuerySource "missing", MissingApiField ApiHeaderSource "X-Missing"],
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

    describe "ResponseCodec" $ do
      it "renders a JSON response with its content type" $
        expectAll
          ( (apiResponseContentType (apiJsonResponse (42 :: Int)) `shouldBe` "application/json; charset=utf-8")
              :| [apiResponseBodyBytes (apiJsonResponse (42 :: Int)) `shouldBe` "42"]
          )

      it "renders a text response with its content type" $
        expectAll
          ( (apiResponseContentType (apiTextResponse "hello") `shouldBe` "text/plain; charset=utf-8")
              :| [apiResponseBodyBytes (apiTextResponse "hello") `shouldBe` "hello"]
          )

      it "renders a bytes response with an explicit content type" $
        expectAll
          ( (apiResponseContentType (apiBytesResponse "image/svg+xml" "<svg/>") `shouldBe` "image/svg+xml")
              :| [apiResponseBodyBytes (apiBytesResponse "image/svg+xml" "<svg/>") `shouldBe` "<svg/>"]
          )

      it "derives comparable, printable representations for response bodies" $
        let bodies = [apiJsonResponse (1 :: Int), apiTextResponse "x", apiBytesResponse "image/svg+xml" "<svg/>"]
         in expectAll
              ( (sum [fromEnum (left == right) | left <- bodies, right <- bodies] `shouldBe` length bodies)
                  :| [ sum [fromEnum (left /= right) | left <- bodies, right <- bodies] `shouldBe` length bodies * (length bodies - 1),
                       sum [length (show bodyValue) + length (showList [bodyValue] "") | bodyValue <- bodies] `shouldSatisfy` (> 0)
                     ]
              )
