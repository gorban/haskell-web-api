{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWeb.AccessibilitySpec (spec) where

import HarchWeb (HtmlAttribute (..), LiveRegion (..), liveRegionAttributes)
import Test.Hspec

spec :: Spec
spec =
  describe "live-region accessibility helpers" $ do
    it "keeps polite status updates atomic" $
      liveRegionAttributes PoliteStatus
        `shouldBe` [HtmlAttribute "role" "status", HtmlAttribute "aria-live" "polite", HtmlAttribute "aria-atomic" "true"]

    it "uses assertive alerts for errors that require immediate attention" $
      liveRegionAttributes AssertiveAlert
        `shouldBe` [HtmlAttribute "role" "alert", HtmlAttribute "aria-live" "assertive", HtmlAttribute "aria-atomic" "true"]
