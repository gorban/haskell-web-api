{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import HarchWeb.Acme qualified as Acme

spec =
  describe "the public HarchWeb.Acme boundary" $
    it "exports supported helpers directly" $
      Acme.validAcmeHttp01ChallengeToken "boundary-token" `shouldBe` Just "boundary-token"
