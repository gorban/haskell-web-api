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
      fmap usernameText (mkUsername "eve_42-dev") `shouldBe` Just "eve_42-dev"
      fmap usernameText (mkUsername "AbC") `shouldBe` Just "AbC"
      fmap usernameText (mkUsername "abcdefghijklmnopqrst") `shouldBe` Just "abcdefghijklmnopqrst"

    it "rejects short, long, non-ASCII, and punctuation-bearing handles" $ do
      mkUsername "ab" `shouldBe` Nothing
      mkUsername "abcdefghijklmnopqrstu" `shouldBe` Nothing
      mkUsername "eve@example.test" `shouldBe` Nothing
      mkUsername "José" `shouldBe` Nothing
      mkUsername "eve name" `shouldBe` Nothing

    it "exercises the opaque username's derived instances" $
      case (mkUsername "eve_42-dev", mkUsername "other_user") of
        (Just username, Just otherUsername) ->
          expectAll
            ( ((username == username) `shouldBe` True)
                :| [ (username /= otherUsername) `shouldBe` True,
                     show username `shouldBe` "Username {usernameText = \"eve_42-dev\"}",
                     show [username] `shouldBe` "[Username {usernameText = \"eve_42-dev\"}]"
                   ]
            )
        _ -> expectationFailure "known-valid usernames unexpectedly failed to parse"
