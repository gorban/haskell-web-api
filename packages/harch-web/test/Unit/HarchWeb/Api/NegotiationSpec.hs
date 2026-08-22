{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWeb.Api.NegotiationSpec (spec) where

import Control.Monad (forM_)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import HarchWeb.Api
import Test.Hspec
import TestCore.CustomAssertions (expectAll)

testMediaType :: Text -> ApiMediaType
testMediaType value = fromMaybe (error "expected test media type to be valid") (apiMediaType value)

spec :: Spec
spec =
  describe "HarchWeb.Api.Negotiation" $ do
    describe "Content negotiation" $ do
      let jsonAndText = testMediaType "application/json" :| [testMediaType "text/plain"]

      -- Tabled per docs/design-guidance.md's CN decision record: one act
      -- (selectRepresentation jsonAndText), one comparison, differing only
      -- in the Accept header and expected negotiation result. The
      -- selectContentTypeRepresentation and parseAcceptHeader clusters below
      -- are separate acts and stay their own describe-local it blocks.
      [ ("selects the first declared representation when Accept is absent", Nothing, SelectedRepresentation (testMediaType "application/json")),
        ("selects an exact match", Just "text/plain", SelectedRepresentation (testMediaType "text/plain")),
        ("prefers the higher client quality between two acceptable representations", Just "application/json;q=0.2, text/plain;q=0.8", SelectedRepresentation (testMediaType "text/plain")),
        ("breaks a quality tie with server declaration order", Just "application/json;q=0.5, text/plain;q=0.5", SelectedRepresentation (testMediaType "application/json")),
        ("matches a type wildcard", Just "text/*", SelectedRepresentation (testMediaType "text/plain")),
        ("matches the full wildcard", Just "*/*", SelectedRepresentation (testMediaType "application/json")),
        ("does not claim a bare media type satisfies an Accept media parameter", Just "text/plain; charset=utf-8", NoAcceptableRepresentation),
        ("lets a more specific range's q=0 exclude a representation despite a permissive wildcard", Just "*/*;q=1, application/json;q=0", SelectedRepresentation (testMediaType "text/plain")),
        ("returns 406 when every declared representation is excluded", Just "text/html, application/xml", NoAcceptableRepresentation),
        ("returns 406 when the only match is explicitly q=0", Just "*/*;q=0", NoAcceptableRepresentation),
        ("keeps the less specific match when a later range in the header is no more specific", Just "application/json, */*", SelectedRepresentation (testMediaType "application/json")),
        ("lets a type wildcard's specificity win over its own higher quality against a more specific, lower-quality match", Just "*/*;q=0.1, text/*;q=0.9, text/plain;q=0.5", SelectedRepresentation (testMediaType "text/plain")),
        ("is case-insensitive for the declared media type", Just "APPLICATION/JSON", SelectedRepresentation (testMediaType "application/json"))
        ]
        `forM_` \(label, acceptHeader, expected) ->
          it label $
            selectRepresentation jsonAndText acceptHeader `shouldBe` expected

      -- Tabled per docs/design-guidance.md's CN decision record: one act
      -- (selectContentTypeRepresentation textContentTypes), one comparison,
      -- differing only in the Accept header and expected negotiation
      -- result. The parseAcceptHeader cluster below is a separate act and
      -- stays its own describe-local it blocks.
      let plainMediaType = testMediaType "text/plain"
          textContentTypes = apiContentType plainMediaType :| [apiUtf8ContentType plainMediaType]
          utf8Expected = SelectedContentTypeRepresentation (apiUtf8ContentType plainMediaType)
          plainExpected = SelectedContentTypeRepresentation (apiContentType plainMediaType)
       in [ ("matches an Accept media parameter against a declared response Content-Type", "text/plain; charset=\"UTF-8\"", utf8Expected),
            ("lets a more parameterized Accept range override an otherwise identical range", "text/plain;q=0.1, text/plain;charset=utf-8;q=0.9", utf8Expected),
            ("matches a full wildcard parameterized range against a declared Content-Type", "*/*;charset=utf-8;q=0.1, text/plain;charset=utf-8;q=0.9", utf8Expected),
            ("matches a type-wildcard parameterized range against a declared Content-Type", "text/*;charset=utf-8;q=0.1, text/plain;charset=utf-8;q=0.9", utf8Expected),
            ("matches a full wildcard range repeated with a redundant charset parameter", "*/*;charset=utf-8, */*;charset=utf-8;charset=utf-8", utf8Expected),
            ("matches a type-wildcard range repeated with a redundant charset parameter", "text/*;charset=utf-8, text/*;charset=utf-8;charset=utf-8", utf8Expected),
            ("does not let an Accept extension after q constrain Content-Type matching", "text/plain; q=0.5; charset=us-ascii", plainExpected)
          ]
            `forM_` \(label, acceptHeader, expected) ->
              it label $
                selectContentTypeRepresentation textContentTypes (Just acceptHeader) `shouldBe` expected

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

      -- Tabled per docs/design-guidance.md's CN decision record: one act
      -- (parseAcceptHeader), one comparison against the full parsed
      -- [AcceptedRange] result, differing only in the header text. The
      -- "parses quality, whitespace..." it above and the boundary-quality
      -- it below stay separate: they compare a projection of the result
      -- (acceptedRangeQuality, a tuple), not the full AcceptedRange list.
      [ ("drops an Accept parameter that has no '=' rather than failing the whole entry", "text/plain;malformed, application/json", [AcceptedRange "text" "plain" [] 1.0, AcceptedRange "application" "json" [] 1.0]),
        ("retains normalized media parameters before q and ignores extensions after it", "text/plain; charset=\"UTF-8\";q=0.5;level=1", [AcceptedRange "text" "plain" [("charset", "utf-8")] 0.5]),
        ("keeps quoted commas, semicolons, and escaped quotes inside one Accept parameter", "text/plain; note=\"first, second; \\\"quoted\\\"\";q=0.2, application/json", [AcceptedRange "text" "plain" [("note", "first, second; \\\"quoted\\\"")] 0.2, AcceptedRange "application" "json" [] 1.0]),
        ("drops a malformed quality value", "text/plain;q=nope", []),
        ("drops a malformed media range while keeping a valid one later in the header", "not-a-media-type, text/plain", [AcceptedRange "text" "plain" [] 1.0]),
        ("rejects a quality value above 1", "text/plain;q=1.001", []),
        ("rejects a quality value with more than three decimal places", "text/plain;q=0.1234", []),
        ("rejects a quality value with more than one digit before the decimal point", "text/plain;q=2", []),
        ("rejects a quality value with a non-numeric suffix", "text/plain;q=0.5suffix", [])
        ]
        `forM_` \(label, acceptHeader, expected) ->
          it label $
            parseAcceptHeader acceptHeader `shouldBe` expected

      it "parses valid quality values including a zero, a mid-range, and a trailing-zero boundary form" $
        map acceptedRangeQuality (parseAcceptHeader "text/plain;q=0, application/json;q=0.125, image/svg+xml;q=1.000")
          `shouldBe` [0.0, 0.125, 1.0]

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
