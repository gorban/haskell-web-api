{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-local-binds #-}

-- | Isolated in its own module (rather than added to
-- 'Unit.HarchWeb.Markup.Quasi.LoweringSpec') because its whole point is to
-- shadow framework identifiers that module otherwise imports unqualified —
-- the module-wide 'Wno-name-shadowing'\/'Wno-unused-local-binds' suppression
-- this file needs would otherwise weaken warnings for genuinely unrelated
-- code in a much larger file.
module Unit.HarchWeb.Markup.Quasi.LoweringHygieneSpec (spec) where

import HarchWeb
import HarchWeb.Markup qualified as Markup
import Test.Hspec

spec :: Spec
spec = do
  describe "quasiquoter splice hygiene" $ do
    it "resolves framework identifiers hygienically even when a local binding shadows their names" $
      -- Regression for an mkName splice-capture bug: the quasiquoter used to
      -- resolve every framework identifier (text, value, name, method,
      -- divTag, ...) as an unqualified 'mkName', which GHC's renamer
      -- resolves by ordinary lexical scoping at the splice site — so a local
      -- binding of the same name (a component prop, say) would silently
      -- rebind the framework's own constructor instead of it. This module
      -- imports every one of those names unqualified via 'HarchWeb', so
      -- shadowing them right here, immediately before the splice, is the
      -- most direct possible reproduction of that failure mode: this test
      -- would not even compile under the old, unhygienic implementation.
      let text = 999 :: Int
          value = True
          name = ()
          method = []
          divTag = "not a tag" :: String
          quoted = [harch|<div name="ignored" method="ignored"><input type="text" value="filled" />hello</div>|]
          direct =
            element
              Markup.divTag
              [Markup.name "ignored", Markup.method "ignored"]
              [ voidElement inputTag [inputType "text", Markup.value "filled"],
                Markup.text "hello"
              ]
       in renderHtml quoted `shouldBe` renderHtml direct
