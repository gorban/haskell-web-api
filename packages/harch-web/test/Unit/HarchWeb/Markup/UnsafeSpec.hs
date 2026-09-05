{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as Text
import HarchWeb.Markup qualified as Markup
import HarchWeb.Markup.Unsafe qualified as Unsafe

spec =
  describe "HarchWeb.Markup.Unsafe" $ do
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
