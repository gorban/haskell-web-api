{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-# SPEC #-}

import Data.List.NonEmpty (NonEmpty (..))
import HarchWeb
import HarchWeb.Markup qualified as Markup
import HarchWeb.Markup.Unsafe qualified as Unsafe

spec =
  describe "HarchWeb.Markup.Elements" $ do
    it "supports opaque markup equality and diagnostics without exposing its representation" $ do
      let firstHtml = element paragraphTag [dataAttribute "kind" "first"] [text "first"]
          sameHtml = element paragraphTag [dataAttribute "kind" "first"] [text "first"]
          otherHtml = element paragraphTag [dataAttribute "kind" "second"] [text "second"]
          firstAttribute = dataAttribute "kind" "first"
          otherAttribute = dataAttribute "kind" "second"
      expectAll
        ( (firstHtml == sameHtml `shouldBe` True)
            :| [ firstHtml /= otherHtml `shouldBe` True,
                 length (show firstHtml) `shouldSatisfy` (> 0),
                 length (show [firstHtml]) `shouldSatisfy` (> 0),
                 firstAttribute /= otherAttribute `shouldBe` True,
                 length (show firstAttribute) `shouldSatisfy` (> 0),
                 length (show [firstAttribute]) `shouldSatisfy` (> 0)
               ]
        )

    it "keeps normal and void tag values comparable and diagnosable" $
      expectAll
        ( (Markup.divTag /= Markup.sectionTag `shouldBe` True)
            :| [ show Markup.divTag `shouldBe` "NormalTag {normalTagText = \"div\"}",
                 show Markup.breakTag `shouldBe` "VoidTag {voidTagText = \"br\"}",
                 length (show [Markup.divTag]) `shouldSatisfy` (> 0),
                 length (show [Markup.breakTag]) `shouldSatisfy` (> 0)
               ]
        )

    it "keeps the complete internal markup AST comparable and printable" $ do
      let elementIdentifier = Markup.literalElementId "example"
          regionIdentifier = Markup.mkRegionId elementIdentifier
          attributeValue = Markup.dataAttribute "example" "value"
          booleanValue = Markup.dataFlag "example"
          trustedValue = Unsafe.unsafeTrustHtml "<em>trusted</em>"
          otherElementIdentifier = Markup.literalElementId "other-example"
          otherRegionIdentifier = Markup.mkRegionId otherElementIdentifier
          otherTrustedValue = Unsafe.unsafeTrustHtml "<strong>other</strong>"
          htmlValue =
            Markup.fragment
              [ Markup.element Markup.divTag [attributeValue] [Markup.text "child"],
                Markup.voidElement Markup.inputTag [booleanValue],
                Markup.text "plain",
                Markup.trustedHtml trustedValue
              ]
          regionValue = Markup.region regionIdentifier Markup.sectionTag [attributeValue] [htmlValue]
          patchValue = Markup.replaceRegion regionValue
          otherHtmlValue = Markup.fragment [Markup.text "other", Markup.trustedHtml otherTrustedValue]
          otherRegionValue = Markup.region otherRegionIdentifier Markup.divTag [booleanValue] [otherHtmlValue]
          otherPatchValue = Markup.replaceRegion otherRegionValue
          values =
            [ show attributeValue,
              show booleanValue,
              show elementIdentifier,
              show regionIdentifier,
              show trustedValue,
              show Markup.divTag,
              show htmlValue,
              show regionValue,
              show patchValue
            ]
      expectAll
        ( (attributeValue /= booleanValue `shouldBe` True)
            :| [ htmlValue /= otherHtmlValue `shouldBe` True,
                 regionValue /= otherRegionValue `shouldBe` True,
                 patchValue /= otherPatchValue `shouldBe` True,
                 show [attributeValue, booleanValue] `shouldSatisfy` (not . null),
                 showList [attributeValue, booleanValue] "" `shouldSatisfy` (not . null),
                 show [elementIdentifier, otherElementIdentifier] `shouldSatisfy` (not . null),
                 elementIdentifier /= otherElementIdentifier `shouldBe` True,
                 showList [elementIdentifier, otherElementIdentifier] "" `shouldSatisfy` (not . null),
                 show [regionIdentifier, otherRegionIdentifier] `shouldSatisfy` (not . null),
                 regionIdentifier /= otherRegionIdentifier `shouldBe` True,
                 showList [regionIdentifier, otherRegionIdentifier] "" `shouldSatisfy` (not . null),
                 show [trustedValue, otherTrustedValue] `shouldSatisfy` (not . null),
                 trustedValue /= otherTrustedValue `shouldBe` True,
                 -- 'deriving' only writes '=='; GHC's HPC instrumentation
                 -- attributes the same-value '==' path to its own box,
                 -- separate from the different-value path above. Comparing
                 -- two independently-constructed-but-equal values (rather
                 -- than a bare self-comparison) exercises it without
                 -- proving nothing.
                 trustedValue == Unsafe.unsafeTrustHtml "<em>trusted</em>" `shouldBe` True,
                 showList [trustedValue, otherTrustedValue] "" `shouldSatisfy` (not . null),
                 Markup.divTag /= Markup.sectionTag `shouldBe` True,
                 showList [Markup.divTag, Markup.sectionTag] "" `shouldSatisfy` (not . null),
                 show [htmlValue, otherHtmlValue] `shouldSatisfy` (not . null),
                 showList [htmlValue, otherHtmlValue] "" `shouldSatisfy` (not . null),
                 show [regionValue, otherRegionValue] `shouldSatisfy` (not . null),
                 showList [regionValue, otherRegionValue] "" `shouldSatisfy` (not . null),
                 show [patchValue, otherPatchValue] `shouldSatisfy` (not . null),
                 showList [patchValue, otherPatchValue] "" `shouldSatisfy` (not . null),
                 not (any null values) `shouldBe` True
               ]
        )

    it "renders the complete typed tag and attribute vocabulary" $ do
      Markup.mkElementId "" `shouldBe` Nothing
      case (Markup.mkElementId "email", Markup.mkElementId "form") of
        (Just emailElementId, Just formElementId) -> do
          let patch = Markup.replaceRegion (Markup.region (Markup.mkRegionId formElementId) Markup.divTag [] [])
              renderedVoidTags =
                Markup.renderHtml
                  ( Markup.fragment
                      [ Markup.voidElement Markup.breakTag [],
                        Markup.voidElement Markup.horizontalRuleTag [],
                        Markup.voidElement Markup.imageTag [],
                        Markup.voidElement Markup.metaTag []
                      ]
                  )
              quotedVoidTags = [harch|<br /><hr /><img /><meta />|]
              renderedHtml =
                Markup.renderHtml
                  ( Markup.fragment
                      [ Markup.element
                          Markup.formTag
                          [ Markup.elementId formElementId,
                            Markup.formAction "/register",
                            Markup.method "post",
                            Markup.enctype "multipart/form-data",
                            Markup.dataAttribute "harch-action" "true",
                            Markup.dataFlag "busy",
                            Markup.ariaLabel "Registration",
                            Markup.ariaLive "polite",
                            Markup.role "form"
                          ]
                          [ Markup.element Markup.headingOneTag [] [Markup.text "Register"],
                            Markup.element Markup.headingTwoTag [] [Markup.text "Details"],
                            Markup.element Markup.labelTag [Markup.labelFor emailElementId] [Markup.text "Email"],
                            Markup.voidElement
                              Markup.inputTag
                              [ Markup.elementId emailElementId,
                                Markup.className (ScopedCssClass (cssScope "account") "field"),
                                Markup.inputType "email",
                                Markup.inputMode "email",
                                Markup.autocomplete "email",
                                Markup.name "email",
                                Markup.value "ada@example.test",
                                Markup.minLength "3",
                                Markup.maxLength "255",
                                Markup.required
                              ],
                            Markup.element
                              Markup.selectTag
                              []
                              [Markup.element Markup.optionTag [Markup.value "en", Markup.selected] [Markup.text "English"]],
                            Markup.element
                              Markup.listTag
                              []
                              [Markup.element Markup.listItemTag [] [Markup.element Markup.codeTag [] [Markup.text "code"]]],
                            Markup.element Markup.paragraphTag [] [Markup.text "Paragraph"],
                            Markup.element Markup.sectionTag [] [Markup.text "Section"],
                            Markup.element Markup.anchorTag [Markup.href "/next"] [Markup.text "Next"],
                            Markup.element Markup.buttonTag [] [Markup.text "Submit"]
                          ]
                      ]
                  )
          Markup.regionPatchId patch `shouldBe` "form"
          Markup.regionPatchHtml patch `shouldBe` "<div id=\"form\" data-harch-region=\"true\"></div>"
          Markup.renderHtml (Markup.element Markup.divTag [Markup.elementId (Markup.literalElementId "literal")] []) `shouldBe` "<div id=\"literal\"></div>"
          renderedVoidTags `shouldBe` "<br><hr><img><meta>"
          Markup.renderHtml quotedVoidTags `shouldBe` renderedVoidTags
          renderedHtml
            `shouldBe` "<form id=\"form\" action=\"/register\" method=\"post\" enctype=\"multipart/form-data\" data-harch-action=\"true\" data-busy aria-label=\"Registration\" aria-live=\"polite\" role=\"form\"><h1>Register</h1><h2>Details</h2><label for=\"email\">Email</label><input id=\"email\" class=\"harch-account-field\" type=\"email\" inputmode=\"email\" autocomplete=\"email\" name=\"email\" value=\"ada@example.test\" minlength=\"3\" maxlength=\"255\" required><select><option value=\"en\" selected>English</option></select><ul><li><code>code</code></li></ul><p>Paragraph</p><section>Section</section><a href=\"/next\">Next</a><button>Submit</button></form>"
        _ -> expectationFailure "expected literal element IDs to be valid"
