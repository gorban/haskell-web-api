{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWeb.MarkupSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as Text
import HarchWeb.Markup qualified as Markup
import HarchWeb.Markup.Unsafe qualified as Unsafe
import Test.Hspec
import TestCore.CustomAssertions (expectAll)

spec :: Spec
spec = do
  describe "HTML markup" $ do
    it "escapes ordinary text and attribute values while leaving trusted fragments explicit" $ do
      let renderedHtml =
            Markup.renderHtml
              ( Markup.fragment
                  [ Markup.element
                      Markup.paragraphTag
                      [Markup.href "https://example.test/?q=\"quoted\"&x=<value>"]
                      [ Markup.text "<script>alert('no')</script>",
                        Markup.trustedHtml (Unsafe.unsafeTrustHtml "<strong>reviewed</strong>")
                      ]
                  ]
              )
      expectAll
        ( (Text.isInfixOf "href=\"https://example.test/?q=&quot;quoted&quot;&amp;x=&lt;value&gt;\"" renderedHtml `shouldBe` True)
            :| [ Text.isInfixOf "&lt;script&gt;alert(&#39;no&#39;)&lt;/script&gt;" renderedHtml `shouldBe` True,
                 Text.isInfixOf "<strong>reviewed</strong>" renderedHtml `shouldBe` True
               ]
        )

    it "makes a region's SSR root and replacement root derive from the same identifier" $ do
      case Markup.mkElementId "subscription-result" of
        Nothing -> expectationFailure "expected the literal region ID to be valid"
        Just resultElementId -> do
          let resultRegion =
                Markup.region
                  (Markup.mkRegionId resultElementId)
                  Markup.paragraphTag
                  [Markup.role "status"]
                  [Markup.text "Ready"]
              renderedRegion = Markup.renderHtml (Markup.regionHtml resultRegion)
          renderedRegion
            `shouldBe` "<p id=\"subscription-result\" data-harch-region=\"true\" role=\"status\">Ready</p>"

    it "keeps framework-owned region attributes on repeated replacements" $ do
      case (Markup.mkElementId "subscription-result", Markup.mkElementId "attempted-override") of
        (Just regionElementId, Just overrideElementId) -> do
          let renderedPatch =
                Markup.regionPatchHtml
                  ( Markup.replaceRegion
                      ( Markup.region
                          (Markup.mkRegionId regionElementId)
                          Markup.paragraphTag
                          [Markup.elementId overrideElementId, Markup.dataAttribute "harch-region" "false", Markup.role "alert"]
                          [Markup.text "Try again"]
                      )
                  )
          renderedPatch
            `shouldBe` "<p id=\"subscription-result\" data-harch-region=\"true\" role=\"alert\">Try again</p>"
        _ -> expectationFailure "expected literal element IDs to be valid"
