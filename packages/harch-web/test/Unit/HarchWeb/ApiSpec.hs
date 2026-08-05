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

    describe "Content negotiation" $ do
      let jsonAndText = "application/json" :| ["text/plain"]

      it "selects the first declared representation when Accept is absent" $
        selectRepresentation jsonAndText Nothing `shouldBe` SelectedRepresentation "application/json"

      it "selects an exact match" $
        selectRepresentation jsonAndText (Just "text/plain") `shouldBe` SelectedRepresentation "text/plain"

      it "prefers the higher client quality between two acceptable representations" $
        selectRepresentation jsonAndText (Just "application/json;q=0.2, text/plain;q=0.8")
          `shouldBe` SelectedRepresentation "text/plain"

      it "breaks a quality tie with server declaration order" $
        selectRepresentation jsonAndText (Just "application/json;q=0.5, text/plain;q=0.5")
          `shouldBe` SelectedRepresentation "application/json"

      it "matches a type wildcard" $
        selectRepresentation jsonAndText (Just "text/*") `shouldBe` SelectedRepresentation "text/plain"

      it "matches the full wildcard" $
        selectRepresentation jsonAndText (Just "*/*") `shouldBe` SelectedRepresentation "application/json"

      it "lets a more specific range's q=0 exclude a representation despite a permissive wildcard" $
        selectRepresentation jsonAndText (Just "*/*;q=1, application/json;q=0")
          `shouldBe` SelectedRepresentation "text/plain"

      it "returns 406 when every declared representation is excluded" $
        selectRepresentation jsonAndText (Just "text/html, application/xml")
          `shouldBe` NoAcceptableRepresentation

      it "returns 406 when the only match is explicitly q=0" $
        selectRepresentation jsonAndText (Just "*/*;q=0") `shouldBe` NoAcceptableRepresentation

      it "keeps the less specific match when a later range in the header is no more specific" $
        selectRepresentation jsonAndText (Just "application/json, */*")
          `shouldBe` SelectedRepresentation "application/json"

      it "lets a type wildcard's specificity win over its own higher quality against a more specific, lower-quality match" $
        selectRepresentation jsonAndText (Just "*/*;q=0.1, text/*;q=0.9, text/plain;q=0.5")
          `shouldBe` SelectedRepresentation "text/plain"

      it "never matches a declared representation that is not a well-formed media type" $
        expectAll
          ( (selectRepresentation ("not-a-media-type" :| ["text/plain"]) (Just "text/plain") `shouldBe` SelectedRepresentation "text/plain")
              :| [ selectRepresentation ("text" :| ["text/plain"]) (Just "text/plain")
                     `shouldBe` SelectedRepresentation "text/plain"
                 ]
          )

      it "drops an Accept parameter that has no '=' rather than failing the whole entry" $
        parseAcceptHeader "text/plain;malformed, application/json"
          `shouldBe` [AcceptedRange "text" "plain" [] 1.0, AcceptedRange "application" "json" [] 1.0]

      it "is case-insensitive for the declared media type" $
        selectRepresentation jsonAndText (Just "APPLICATION/JSON")
          `shouldBe` SelectedRepresentation "application/json"

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
            results = [NoAcceptableRepresentation, SelectedRepresentation "application/json"]
         in expectAll
              ( (sum [fromEnum (left == right) | left <- ranges, right <- ranges] `shouldBe` length ranges)
                  :| [ sum [fromEnum (left /= right) | left <- ranges, right <- ranges] `shouldBe` length ranges * (length ranges - 1),
                       sum [length (show rangeValue) + length (showList [rangeValue] "") | rangeValue <- ranges] `shouldSatisfy` (> 0),
                       sum [fromEnum (left == right) | left <- results, right <- results] `shouldBe` length results,
                       sum [fromEnum (left /= right) | left <- results, right <- results] `shouldBe` length results * (length results - 1),
                       sum [length (show resultValue) + length (showList [resultValue] "") | resultValue <- results] `shouldSatisfy` (> 0)
                     ]
              )
