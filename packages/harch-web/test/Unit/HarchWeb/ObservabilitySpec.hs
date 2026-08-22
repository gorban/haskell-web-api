{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWeb.ObservabilitySpec (spec) where

import HarchWeb.Observability qualified as Observability
import Test.Hspec

spec :: Spec
spec =
  describe "newOtlpHttpManager" $
    it "constructs a usable HTTP manager, as an explicit prop rather than a global" $ do
      -- 'HttpClient.Manager' has no 'Eq'/'Show' to compare against, so the
      -- meaningful assertion available here is that construction succeeds;
      -- see the BZ decision record for why this is a caller-owned prop now,
      -- not a process-global CAF.
      manager <- Observability.newOtlpHttpManager
      manager `seq` pure ()
