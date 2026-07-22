{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWeb.UsernameSpec (spec) where

import HarchWeb.Username (mkUsername, usernameText)
import Test.Hspec

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
