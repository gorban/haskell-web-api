{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Exception (ErrorCall (..), evaluate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, isJust)
import Data.Text qualified as Text
import HarchWeb.Markup qualified as Markup

spec =
  describe "HarchWeb.Markup.Attributes" $ do
    it "rejects a URL scheme that would execute script when followed, allowlisting relative and http(s) URLs" $
      expectAll
        ( (Markup.mkSafeUrl "javascript:alert(1)" `shouldBe` Nothing)
            :| [ Markup.mkSafeUrl "JavaScript:alert(1)" `shouldBe` Nothing,
                 Markup.mkSafeUrl "data:text/html,<script>alert(1)</script>" `shouldBe` Nothing,
                 Markup.mkSafeUrl "vbscript:msgbox(1)" `shouldBe` Nothing,
                 -- Browsers strip embedded tabs/newlines/carriage returns and
                 -- leading whitespace before reading a URL's scheme, so a
                 -- naive prefix check alone would miss these.
                 Markup.mkSafeUrl "java\tscript:alert(1)" `shouldBe` Nothing,
                 Markup.mkSafeUrl "java\nscript:alert(1)" `shouldBe` Nothing,
                 Markup.mkSafeUrl " javascript:alert(1)" `shouldBe` Nothing,
                 Markup.mkSafeUrl "/relative/path" `shouldSatisfy` isJust,
                 Markup.mkSafeUrl "relative.html" `shouldSatisfy` isJust,
                 Markup.mkSafeUrl "#fragment" `shouldSatisfy` isJust,
                 Markup.mkSafeUrl "?query=value" `shouldSatisfy` isJust,
                 Markup.mkSafeUrl "https://example.test/path?q=1" `shouldSatisfy` isJust,
                 Markup.mkSafeUrl "HTTP://example.test" `shouldSatisfy` isJust,
                 fmap Markup.safeUrlText (Markup.mkSafeUrl "/next") `shouldBe` Just "/next"
               ]
        )

    it "rejects a data-attribute suffix outside the [a-z0-9-]+ character set, closing an attribute-name injection" $
      expectAll
        ( (Markup.mkDataAttributeSuffix "" `shouldBe` Nothing)
            :| [ -- The finding this closes: an unvalidated suffix could close
                 -- the attribute early and inject an event handler, since
                 -- attribute *names* are written into markup with no escaping.
                 Markup.mkDataAttributeSuffix "x\" onmouseover=\"evil()" `shouldBe` Nothing,
                 Markup.mkDataAttributeSuffix "Upper" `shouldBe` Nothing,
                 Markup.mkDataAttributeSuffix "has space" `shouldBe` Nothing,
                 Markup.mkDataAttributeSuffix "under_score" `shouldBe` Nothing,
                 Markup.mkDataAttributeSuffix "harch-action" `shouldSatisfy` isJust,
                 Markup.mkDataAttributeSuffix "page123" `shouldSatisfy` isJust,
                 fmap Markup.dataAttributeSuffixText (Markup.mkDataAttributeSuffix "kind") `shouldBe` Just "kind"
               ]
        )

    it "raises the unsafe-URL construction error for a route renderer's rejected Maybe" $ do
      evaluate (Markup.requiredSafeUrlOrDie "test failure" Nothing `seq` ())
        `shouldThrow` \case
          ErrorCall message -> "test failure" `Text.isInfixOf` Text.pack message
      evaluate (Markup.requiredSafeUrl Nothing `seq` ())
        `shouldThrow` \case
          ErrorCall message -> "a required URL was unsafe" `Text.isInfixOf` Text.pack message

    it "exercises Eq and Show for DataAttributeSuffix and SafeUrl" $
      let suffixKind = fromMaybe (error "expected a valid suffix") (Markup.mkDataAttributeSuffix "kind")
          suffixAction = fromMaybe (error "expected a valid suffix") (Markup.mkDataAttributeSuffix "action")
          urlNext = fromMaybe (error "expected a valid url") (Markup.mkSafeUrl "/next")
          urlOther = fromMaybe (error "expected a valid url") (Markup.mkSafeUrl "/other")
       in expectAll
            ( (suffixKind /= suffixAction `shouldBe` True)
                -- 'deriving' only writes '=='; GHC's HPC instrumentation
                -- attributes the same-value '==' path to its own box,
                -- separate from the different-value path above. Comparing
                -- two independently-parsed-but-equal values (rather than a
                -- bare self-comparison) exercises it without proving
                -- nothing.
                :| [ Markup.mkDataAttributeSuffix "kind" == Just suffixKind `shouldBe` True,
                     show suffixKind `shouldBe` "DataAttributeSuffix \"kind\"",
                     show [suffixKind] `shouldBe` "[DataAttributeSuffix \"kind\"]",
                     urlNext /= urlOther `shouldBe` True,
                     Markup.mkSafeUrl "/next" == Just urlNext `shouldBe` True,
                     show urlNext `shouldBe` "SafeUrl \"/next\"",
                     show [urlNext] `shouldBe` "[SafeUrl \"/next\"]"
                   ]
            )

    it "raises the invalid data-attribute suffix literal error for a malformed OverloadedStrings literal" $
      evaluate (("bad suffix!" :: Markup.DataAttributeSuffix) `seq` ())
        `shouldThrow` \case
          ErrorCall message -> "invalid data-attribute suffix literal" `Text.isInfixOf` Text.pack message

    it "raises the invalid-or-unsafe URL literal error for a malformed OverloadedStrings literal" $
      evaluate (("javascript:alert(1)" :: Markup.SafeUrl) `seq` ())
        `shouldThrow` \case
          ErrorCall message -> "invalid or unsafe URL literal" `Text.isInfixOf` Text.pack message
