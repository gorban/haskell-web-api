{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, isNothing)
import Data.Text qualified as Text
import HarchWeb.Time (unixTimeSeconds)
import HarchWeb.Totp

spec = do
  describe "TotpSecret" $ do
    it "round-trips canonical Base32 enrollment secrets" $ do
      expectAll
        ( (renderTotpSecret rfcSecret `shouldBe` "GEZDGNBVGY3TQOJQGEZDGNBVGY3TQOJQ")
            :| [ rfcSecret /= alternateSecret `shouldBe` True,
                 fmap (== rfcSecret) (mkTotpSecret (renderTotpSecret rfcSecret)) `shouldBe` Just True,
                 fmap renderTotpSecret (mkTotpSecret (renderTotpSecret rfcSecret)) `shouldBe` Just "GEZDGNBVGY3TQOJQGEZDGNBVGY3TQOJQ",
                 fmap renderTotpSecret (mkTotpSecret "gezdgnbvgy3tqojqgezdgnbvgy3tqojq") `shouldBe` Just "GEZDGNBVGY3TQOJQGEZDGNBVGY3TQOJQ",
                 fmap renderTotpSecret (mkTotpSecret "AAAAAAAAAAAAAAAAAAAAAAAAAA") `shouldBe` Just "AAAAAAAAAAAAAAAAAAAAAAAAAA"
               ]
        )

    it "accepts every canonical Base32 alphabet value" $ do
      map (fmap renderTotpSecret . mkTotpSecret . Text.replicate 32 . Text.singleton) "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"
        `shouldBe` map (Just . Text.replicate 32 . Text.singleton) "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567"

    it "rejects empty, malformed, non-canonical, and undersized secrets" $ do
      expectAll
        ( (isNothing (mkTotpSecret "") `shouldBe` True)
            :| [ isNothing (mkTotpSecret "GEZDGNBVGY3TQOJQGEZDGNBVGY3TQOJ0") `shouldBe` True,
                 isNothing (mkTotpSecret "AB") `shouldBe` True,
                 isNothing (mkTotpSecret "GEZDGNBVGY3TQOJQGEZDGNBV") `shouldBe` True
               ]
        )

    it "generates a 160-bit secret suitable for Base32 enrollment" $ do
      secret <- generateTotpSecret
      expectAll
        ( (Text.length (renderTotpSecret secret) `shouldBe` 32)
            :| [fmap renderTotpSecret (mkTotpSecret (renderTotpSecret secret)) `shouldBe` Just (renderTotpSecret secret)]
        )

  describe "TotpCode" $ do
    it "uses RFC 6238 SHA-1 vectors at known Unix instants with six-digit codes" $ do
      map (fmap totpCodeText . mkTotpCode) ["287082", "081804", "050471", "005924", "279037", "353130"]
        `shouldBe` map Just ["287082", "081804", "050471", "005924", "279037", "353130"]
      map (totpCodeText . (`totpCode` rfcSecret) . unixTimeSeconds) [59, 1111111109, 1111111111, 1234567890, 2000000000, 20000000000]
        `shouldBe` ["287082", "081804", "050471", "005924", "279037", "353130"]

    it "accepts exactly six ASCII digits and validates the current time step" $ do
      let correctCode = required (mkTotpCode "287082")
      expectAll
        ( (mkTotpCode "287082" `shouldBe` Just correctCode)
            :| [ correctCode /= required (mkTotpCode "287083") `shouldBe` True,
                 show correctCode `shouldBe` "TotpCode \"287082\"",
                 show [correctCode] `shouldBe` "[TotpCode \"287082\"]",
                 isNothing (mkTotpCode "28708") `shouldBe` True,
                 isNothing (mkTotpCode "2870820") `shouldBe` True,
                 isNothing (mkTotpCode "28708a") `shouldBe` True,
                 isNothing (mkTotpCode "28708\n") `shouldBe` True,
                 validateTotpCode 59 0 rfcSecret correctCode `shouldBe` True,
                 validateTotpCode 60 0 rfcSecret correctCode `shouldBe` False
               ]
        )

    it "accepts a caller-bounded adjacent-period clock skew" $ do
      let nowSeconds = 60
          previousPeriodCode = totpCode (nowSeconds - 30) rfcSecret
          currentPeriodCode = totpCode nowSeconds rfcSecret
          followingPeriodCode = totpCode (nowSeconds + 30) rfcSecret
          outsideWindowCode = totpCode (nowSeconds + 60) rfcSecret
      expectAll
        ( (validateTotpCode nowSeconds 1 rfcSecret previousPeriodCode `shouldBe` True)
            :| [ validateTotpCode nowSeconds 1 rfcSecret currentPeriodCode `shouldBe` True,
                 validateTotpCode nowSeconds 1 rfcSecret followingPeriodCode `shouldBe` True,
                 validateTotpCode nowSeconds 1 rfcSecret outsideWindowCode `shouldBe` False,
                 validateTotpCode 0 1 rfcSecret (totpCode 0 rfcSecret) `shouldBe` True,
                 validateTotpCode 0 1 rfcSecret (totpCode 30 rfcSecret) `shouldBe` True
               ]
        )

    it "reports the specific counter a code matched, for replay tracking" $ do
      let nowSeconds = 60
          previousPeriodCode = totpCode (nowSeconds - 30) rfcSecret
          currentPeriodCode = totpCode nowSeconds rfcSecret
          followingPeriodCode = totpCode (nowSeconds + 30) rfcSecret
          outsideWindowCode = totpCode (nowSeconds + 60) rfcSecret
      expectAll
        ( (validateTotpCodeCounter nowSeconds 1 rfcSecret previousPeriodCode `shouldBe` Just 1)
            :| [ validateTotpCodeCounter nowSeconds 1 rfcSecret currentPeriodCode `shouldBe` Just 2,
                 validateTotpCodeCounter nowSeconds 1 rfcSecret followingPeriodCode `shouldBe` Just 3,
                 validateTotpCodeCounter nowSeconds 1 rfcSecret outsideWindowCode `shouldBe` Nothing
               ]
        )

rfcSecret :: TotpSecret
rfcSecret = required (mkTotpSecret "GEZDGNBVGY3TQOJQGEZDGNBVGY3TQOJQ")

alternateSecret :: TotpSecret
alternateSecret = required (mkTotpSecret "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")

required :: Maybe a -> a
required = fromMaybe (error "expected valid TOTP test value")
