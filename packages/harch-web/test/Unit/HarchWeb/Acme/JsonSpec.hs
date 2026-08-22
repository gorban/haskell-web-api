{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWeb.Acme.JsonSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import HarchWeb.Acme.Json (jsonArrayBytes, jsonObjectBytes, jsonStringBytes)
import Test.Hspec
import TestCore.CustomAssertions (expectAll)

spec :: Spec
spec =
  describe "ACME JSON encoding helpers" $
    it "encodes strings, arrays, and objects as JSON bytes" $ do
      expectAll
        ( ( jsonStringBytes "\"\\\b\f\n\r\tplain"
              `shouldBe` "\"\\\"\\\\\\u0008\\u000c\\n\\r\\tplain\""
          )
            :| [ jsonArrayBytes [] `shouldBe` "[]",
                 jsonArrayBytes ["1", "2"] `shouldBe` "[1,2]",
                 jsonObjectBytes [] `shouldBe` "{}",
                 jsonObjectBytes [("a", "1"), ("b", "2")] `shouldBe` "{\"a\":1,\"b\":2}"
               ]
        )
