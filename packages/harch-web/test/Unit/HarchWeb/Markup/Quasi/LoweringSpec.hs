{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{-# SPEC #-}

import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import HarchWeb
import HarchWeb.Action qualified as Action
import Unit.HarchWeb.MarkupComponents qualified as Account
import Unit.HarchWeb.MarkupRejection (rejectedMarkup)

turkishInitialComponent :: Html
turkishInitialComponent = [harch|<Account.İtem itemLabel="Unicode component" />|]

primeSuffixedInterpolation :: Html
primeSuffixedInterpolation =
  let renderedValue' = text "prime-suffixed"
   in [harch|{renderedValue'}|]

characterLiteralInterpolation :: Html
characterLiteralInterpolation =
  [harch|{if '}' == '}' then text "character literal" else text "unreachable"}|]

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

-- | Template Haskell name quotes are not supported {...} expressions (see
-- 'parseExpression'\'s Haddock). Both direct and nested forms must fail the
-- splice cleanly instead of leaking haskell-src-meta's 'ErrorCall'.
thNameQuoteRejected :: Bool
thNameQuoteRejected =
  $(rejectedMarkup "{'Just}")

nestedThNameQuoteRejected :: Bool
nestedThNameQuoteRejected =
  $(rejectedMarkup "{f 'Just}")

newtype ControlRoute = ControlRoute Text.Text

controlRouteHref :: ControlRoute -> SafeUrl
controlRouteHref (ControlRoute target) = fromMaybe (error "expected a safe URL") (mkSafeUrl ("/" <> target))

controlActionCodec :: Action.ActionCodec Text.Text () Text.Text
controlActionCodec =
  case Action.actionCodec [Action.action "/actions/subscribe" (Action.post "/actions/subscribe") (pure "/actions/subscribe")] of
    Left codecError -> error (show codecError)
    Right codec -> codec

spec =
  describe "HarchWeb.Markup.Quasi.Lowering" $ do
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

    it "accepts prime-suffixed identifiers and character literals in expressions" $
      expectAll
        ( (renderHtml primeSuffixedInterpolation `shouldBe` "prime-suffixed")
            :| [renderHtml characterLiteralInterpolation `shouldBe` "character literal"]
        )

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

    it "uses Unicode case mapping when lowering a component's initial character" $
      renderHtml turkishInitialComponent `shouldBe` "Unicode component"

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
          renderedLink = renderHtml (pageLink controlRouteHref (ControlRoute "control") [text "Continue"])
          renderedActionForm = renderHtml quotedActionForm
          renderedEmptyActionForm = renderHtml (renderActionForm (actionForm controlActionCodec () "/actions/subscribe" defaultActionFormAttributes []))
      expectAll
        ( (Text.isInfixOf "aria-label=\"Subscription\"" renderedActionForm `shouldBe` True)
            :| [ Text.isInfixOf "data-harch-action-method=\"post\"" renderedActionForm `shouldBe` True,
                 Text.isInfixOf "method=\"dialog\"" renderedActionForm `shouldBe` True,
                 Text.isInfixOf "<button type=\"submit\">Subscribe</button>" renderedActionForm `shouldBe` True,
                 Text.isInfixOf "data-harch-action-status" renderedEmptyActionForm `shouldBe` True,
                 Text.isInfixOf "data-harch-action-cancel" renderedEmptyActionForm `shouldBe` True
               ]
        )
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
                 literalChildrenRejected `shouldBe` True,
                 thNameQuoteRejected `shouldBe` True,
                 nestedThNameQuoteRejected `shouldBe` True
               ]
        )
