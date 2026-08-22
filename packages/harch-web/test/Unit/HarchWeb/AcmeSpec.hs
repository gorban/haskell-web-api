{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWeb.AcmeSpec (spec) where

import HarchWeb.Acme qualified as Acme
import Test.Hspec

spec :: Spec
spec =
  describe "the public HarchWeb.Acme boundary" $
    it "exports supported helpers directly" $
      Acme.validAcmeHttp01ChallengeToken "boundary-token" `shouldBe` Just "boundary-token"
