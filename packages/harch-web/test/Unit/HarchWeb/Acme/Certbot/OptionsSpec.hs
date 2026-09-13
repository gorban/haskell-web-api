{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Data.List.NonEmpty (NonEmpty (..))
import HarchWeb

spec =
  describe "certbot option-list parsing helpers" $
    it "extracts option values, flags, and domain lists from a certbot argument list" $
      expectAll
        ( (certbotOptionValues "--cert-name" ["certonly", "--cert-name", "named-cert"] `shouldBe` ["named-cert"])
            :| [ certbotOptionValues "--domain" ["--domain=cli.example.com", "--domain", "other.example.com"] `shouldBe` ["other.example.com", "cli.example.com"],
                 certbotHasOption "--missing" ["certonly"] `shouldBe` False,
                 splitCertbotDomainValue " example.com , www.example.com ,, " `shouldBe` ["example.com", "www.example.com"],
                 firstCertbotDomain ["-d", "one.example.com", "--domains=two.example.com,three.example.com"] `shouldBe` Just "one.example.com"
               ]
        )
