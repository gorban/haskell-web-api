{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWeb.UsernameSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import HarchWeb.Username (mkUsername, usernameText)
import Test.Hspec
import TestCore.CustomAssertions (expectAll)

spec :: Spec
spec =
  describe "Username" $ do
    it "accepts the source app's 3-20 character ASCII handle grammar" $ do
      expectAll
        ( (fmap usernameText (mkUsername "eve_42-dev") `shouldBe` Just "eve_42-dev")
            :| [ fmap usernameText (mkUsername "AbC") `shouldBe` Just "AbC",
                 fmap usernameText (mkUsername "abcdefghijklmnopqrst") `shouldBe` Just "abcdefghijklmnopqrst"
               ]
        )

    it "rejects short, long, non-ASCII, and punctuation-bearing handles" $ do
      expectAll
        ( (mkUsername "ab" `shouldBe` Nothing)
            :| [ mkUsername "abcdefghijklmnopqrstu" `shouldBe` Nothing,
                 mkUsername "eve@example.test" `shouldBe` Nothing,
                 mkUsername "José" `shouldBe` Nothing,
                 mkUsername "eve name" `shouldBe` Nothing
               ]
        )

    it "exercises the opaque username's derived instances" $
      case (mkUsername "eve_42-dev", mkUsername "other_user") of
        (Just username, Just otherUsername) ->
          expectAll
            ( ((username /= otherUsername) `shouldBe` True)
                -- 'deriving' only writes '=='; GHC's HPC instrumentation
                -- attributes the same-value '==' path to its own box,
                -- separate from the different-value path above. Comparing
                -- two independently-parsed-but-equal values (rather than a
                -- bare self-comparison) exercises it without proving
                -- nothing.
                :| [ mkUsername "eve_42-dev" == Just username `shouldBe` True,
                     show username `shouldBe` "Username {usernameText = \"eve_42-dev\"}",
                     show [username] `shouldBe` "[Username {usernameText = \"eve_42-dev\"}]"
                   ]
            )
        _ -> expectationFailure "known-valid usernames unexpectedly failed to parse"
