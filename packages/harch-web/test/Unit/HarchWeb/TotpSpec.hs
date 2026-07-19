{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWeb.TotpSpec (spec) where

import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Text as Text
import HarchWeb.Totp
import Test.Hspec

spec :: Spec
spec = do
  describe "TotpSecret" $ do
    it "round-trips canonical Base32 enrollment secrets" $ do
      renderTotpSecret rfcSecret `shouldBe` "GEZDGNBVGY3TQOJQGEZDGNBVGY3TQOJQ"
      rfcSecret == rfcSecret `shouldBe` True
      rfcSecret /= alternateSecret `shouldBe` True
      fmap (== rfcSecret) (mkTotpSecret (renderTotpSecret rfcSecret)) `shouldBe` Just True
      fmap renderTotpSecret (mkTotpSecret (renderTotpSecret rfcSecret)) `shouldBe` Just "GEZDGNBVGY3TQOJQGEZDGNBVGY3TQOJQ"
      fmap renderTotpSecret (mkTotpSecret "gezdgnbvgy3tqojqgezdgnbvgy3tqojq") `shouldBe` Just "GEZDGNBVGY3TQOJQGEZDGNBVGY3TQOJQ"
      fmap renderTotpSecret (mkTotpSecret "AAAAAAAAAAAAAAAAAAAAAAAAAA") `shouldBe` Just "AAAAAAAAAAAAAAAAAAAAAAAAAA"

    it "accepts every canonical Base32 alphabet value" $ do
      map (fmap renderTotpSecret . mkTotpSecret . Text.replicate 32 . Text.singleton) "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"
        `shouldBe` map (Just . Text.replicate 32 . Text.singleton) "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"

    it "rejects empty, malformed, non-canonical, and undersized secrets" $ do
      isNothing (mkTotpSecret "") `shouldBe` True
      isNothing (mkTotpSecret "GEZDGNBVGY3TQOJQGEZDGNBVGY3TQOJ0") `shouldBe` True
      isNothing (mkTotpSecret "AB") `shouldBe` True
      isNothing (mkTotpSecret "GEZDGNBVGY3TQOJQGEZDGNBV") `shouldBe` True

    it "generates a 160-bit secret suitable for Base32 enrollment" $ do
      secret <- generateTotpSecret
      Text.length (renderTotpSecret secret) `shouldBe` 32
      fmap renderTotpSecret (mkTotpSecret (renderTotpSecret secret)) `shouldBe` Just (renderTotpSecret secret)

  describe "TotpCode" $ do
    it "uses RFC 6238 SHA-1 vectors with six-digit codes" $ do
      map (fmap totpCodeText . mkTotpCode) ["287082", "081804", "050471", "005924", "279037", "353130"]
        `shouldBe` map Just ["287082", "081804", "050471", "005924", "279037", "353130"]
      map (totpCodeText . (`totpCode` rfcSecret)) [59, 1111111109, 1111111111, 1234567890, 2000000000, 20000000000]
        `shouldBe` ["287082", "081804", "050471", "005924", "279037", "353130"]

    it "accepts exactly six ASCII digits and validates the current time step" $ do
      let correctCode = required (mkTotpCode "287082")
      mkTotpCode "287082" `shouldBe` Just correctCode
      correctCode == correctCode `shouldBe` True
      correctCode /= required (mkTotpCode "287083") `shouldBe` True
      show correctCode `shouldBe` "TotpCode \"287082\""
      show [correctCode] `shouldBe` "[TotpCode \"287082\"]"
      isNothing (mkTotpCode "28708") `shouldBe` True
      isNothing (mkTotpCode "2870820") `shouldBe` True
      isNothing (mkTotpCode "28708a") `shouldBe` True
      isNothing (mkTotpCode "28708\n") `shouldBe` True
      validateTotpCode 59 rfcSecret correctCode `shouldBe` True
      validateTotpCode 60 rfcSecret correctCode `shouldBe` False

rfcSecret :: TotpSecret
rfcSecret = required (mkTotpSecret "GEZDGNBVGY3TQOJQGEZDGNBVGY3TQOJQ")

alternateSecret :: TotpSecret
alternateSecret = required (mkTotpSecret "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")

required :: Maybe a -> a
required = fromMaybe (error "expected valid TOTP test value")
