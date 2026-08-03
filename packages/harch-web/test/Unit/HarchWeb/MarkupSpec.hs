{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Unit.HarchWeb.MarkupSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as Text
import HarchWeb
import HarchWeb.Markup qualified as Markup
import HarchWeb.Markup.Unsafe qualified as Unsafe
import Test.Hspec
import TestCore.CustomAssertions (expectAll)
import Unit.HarchWeb.MarkupComponents qualified as Account
import Unit.HarchWeb.MarkupRejection (rejectedMarkup)

duplicateNamedPropertyRejected :: Bool
duplicateNamedPropertyRejected =
  $(rejectedMarkup "<Account.HeroCard heroTitle=\"First\" heroTitle=\"Second\" />")

unknownNamedPropertyRejected :: Bool
unknownNamedPropertyRejected =
  $(rejectedMarkup "<Account.HeroCard heroTitle=\"First\" unknownProperty=\"Second\" />")

missingNamedPropertyRejected :: Bool
missingNamedPropertyRejected =
  $(rejectedMarkup "<Account.HeroCard />")

nonLiteralPropsRejected :: Bool
nonLiteralPropsRejected =
  $(rejectedMarkup "<Account.UserAvatar props={Account.AccountProfile \"Ada\"} />")

mixedPropsRejected :: Bool
mixedPropsRejected =
  $(rejectedMarkup "<Account.HeroCard props={[Account.HeroCardProps \"First\"]} heroTitle=\"First\" />")

duplicateChildrenRejected :: Bool
duplicateChildrenRejected =
  $(rejectedMarkup "<Account.HeroCard heroTitle=\"First\" children={[]} children={[]} />")

mixedChildrenRejected :: Bool
mixedChildrenRejected =
  $(rejectedMarkup "<Account.HeroCard heroTitle=\"First\" children={[]}><p>Nested</p></Account.HeroCard>")

literalChildrenRejected :: Bool
literalChildrenRejected =
  $(rejectedMarkup "<Account.HeroCard heroTitle=\"First\" children=\"not-a-list\" />")

newtype ControlRoute = ControlRoute Text.Text

newtype ControlContext = ControlContext Text.Text

controlRouteCodec :: RouteCodec ControlRoute ControlContext
controlRouteCodec =
  RouteCodec
    { parseRoute = \context path ->
        if path == "/control"
          then Just RouteRequest {requestRoute = ControlRoute "control", requestContext = context}
          else Nothing,
      renderRoute = \RouteRequest {requestRoute = ControlRoute target, requestContext = ControlContext prefix} -> prefix <> target,
      notFoundRequest = \context -> RouteRequest {requestRoute = ControlRoute "control", requestContext = context}
    }

spec :: Spec
spec = do
  describe "HTML markup" $ do
    it "lowers native tags, typed attributes, and void elements to the existing AST" $ do
      let emailId = literalElementId "subscription-email"
          quoted =
            [harch|
              <section data-page="home" class={ScopedCssClass (cssScope "home") "root"}>
                <h1>Home</h1>
                <label for={emailId}>Email address</label>
                <input id={emailId} name="email" type="email" autocomplete="email" required />
              </section>
            |]
          direct =
            element
              sectionTag
              [dataAttribute "page" "home", className (ScopedCssClass (cssScope "home") "root")]
              [ element headingOneTag [] [text "Home"],
                element labelTag [labelFor emailId] [text "Email address"],
                voidElement inputTag [elementId emailId, name "email", inputType "email", autocomplete "email", required]
              ]
      renderHtml quoted `shouldBe` renderHtml direct

    it "escapes literal and Text interpolation while composing Html interpolation safely" $ do
      let interpolatedText = "<reviewed>" :: Text.Text
          safeChild = element codeTag [] [text "safe"]
          quoted = [harch|<p>Literal &amp; unsafe &lt;literal&gt; {interpolatedText} {safeChild}</p>|]
      renderHtml quoted `shouldBe` "<p>Literal &amp; unsafe &lt;literal&gt; &lt;reviewed&gt; <code>safe</code></p>"

    it "composes a list of Html children in a root-level markup fragment" $ do
      let children = [element codeTag [] [text "safe"], element paragraphTag [] [text "after"]]
          quoted = [harch|<label for="email">Email address</label>{children}|]
      renderHtml quoted `shouldBe` "<label for=\"email\">Email address</label><code>safe</code><p>after</p>"

    it "lowers named, qualified, and self-closing components to typed Haskell functions" $ do
      let quoted =
            [harch|
              <Account.HeroCard heroTitle="Second page">
                <Account.ProfileCard profileCardTitle="Ada" />
              </Account.HeroCard>
            |]
      renderHtml quoted
        `shouldBe` "<section data-hero-card=\"true\"><h2>Second page</h2><p data-profile-card=\"true\">Ada</p></section>"

    it "passes computed children and heterogeneous positional props directly to components" $ do
      let computedChildren = [element paragraphTag [] [text "Computed child"]]
          computedChildrenQuoted =
            [harch|<Account.HeroCard heroTitle="Second page" children={computedChildren} />|]
          legacyProfileCardQuoted =
            [harch|<Account.ProfileCard props={[Account.ProfileCardProps "Legacy"]} />|]
          avatarQuoted =
            [harch|<Account.UserAvatar props={[Account.AccountProfile "Ada", Account.SmallAvatar]} />|]
      renderHtml computedChildrenQuoted
        `shouldBe` "<section data-hero-card=\"true\"><h2>Second page</h2><p>Computed child</p></section>"
      renderHtml legacyProfileCardQuoted `shouldBe` "<p data-profile-card=\"true\">Legacy</p>"
      renderHtml avatarQuoted `shouldBe` "<p data-user-avatar=\"small\">Ada</p>"

    it "lowers typed control component properties and renders framework-owned control attributes" $ do
      let quotedActionForm =
            [harch|
              <Account.TypedActionForm action="/actions/subscribe" aria-label="Subscription">
                <button type="submit">Subscribe</button>
              </Account.TypedActionForm>
            |]
          renderedLink = renderHtml (pageLink controlRouteCodec (ControlContext "/") (ControlRoute "control") [] [text "Continue"])
      renderHtml quotedActionForm
        `shouldBe` "<form aria-label=\"Subscription\" data-harch-control data-harch-action=\"true\" action=\"/actions/subscribe\" method=\"post\"><button type=\"submit\">Subscribe</button></form>"
      renderedLink `shouldBe` "<a href=\"/control\" data-page-link=\"true\">Continue</a>"

    it "rejects invalid named properties, positional props, and child forms while lowering" $
      expectAll
        ( (duplicateNamedPropertyRejected `shouldBe` True)
            :| [ unknownNamedPropertyRejected `shouldBe` True,
                 missingNamedPropertyRejected `shouldBe` True,
                 nonLiteralPropsRejected `shouldBe` True,
                 mixedPropsRejected `shouldBe` True,
                 duplicateChildrenRejected `shouldBe` True,
                 mixedChildrenRejected `shouldBe` True,
                 literalChildrenRejected `shouldBe` True
               ]
        )

    it "embeds patchable regions explicitly without changing their SSR root" $ do
      let statusRegion = region (mkRegionId (literalElementId "status")) paragraphTag [role "status"] [text "Ready"]
          quoted = [harch|<section><Region value={statusRegion} /></section>|]
      renderHtml quoted `shouldBe` "<section><p id=\"status\" data-harch-region=\"true\" role=\"status\">Ready</p></section>"

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

    it "renders the complete typed tag and attribute vocabulary" $ do
      Markup.mkElementId "" `shouldBe` Nothing
      case (Markup.mkElementId "email", Markup.mkElementId "form") of
        (Just emailElementId, Just formElementId) -> do
          let patch = Markup.replaceRegion (Markup.region (Markup.mkRegionId formElementId) Markup.divTag [] [])
              renderedHtml =
                Markup.renderHtml
                  ( Markup.fragment
                      [ Markup.element
                          Markup.formTag
                          [ Markup.elementId formElementId,
                            Markup.formAction "/register",
                            Markup.method "post",
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
                              [Markup.element Markup.optionTag [Markup.value "en"] [Markup.text "English"]],
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
          renderedHtml
            `shouldBe` "<form id=\"form\" action=\"/register\" method=\"post\" data-harch-action=\"true\" data-busy aria-label=\"Registration\" aria-live=\"polite\" role=\"form\"><h1>Register</h1><h2>Details</h2><label for=\"email\">Email</label><input id=\"email\" class=\"harch-account-field\" type=\"email\" inputmode=\"email\" autocomplete=\"email\" name=\"email\" value=\"ada@example.test\" minlength=\"3\" maxlength=\"255\" required><select><option value=\"en\">English</option></select><ul><li><code>code</code></li></ul><p>Paragraph</p><section>Section</section><a href=\"/next\">Next</a><button>Submit</button></form>"
        _ -> expectationFailure "expected literal element IDs to be valid"
