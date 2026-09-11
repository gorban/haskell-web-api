{-# SPEC #-}

import TestCore.E2EPrelude qualified as E2EPrelude
import TestCore.SpecPreprocessor (runPure)

spec = do
  describe "TestCore.SpecPreprocessor" $
    it "retains the direct processor compatibility facade" $
      runPure "test" "/tmp/FacadeSpec.hs" "{-# SPEC #-}"
        `shouldContain'` "module FacadeSpec (spec) where"

  describe "TestCore.E2EPrelude" $ it "re-exports the standard test helpers" $ E2EPrelude.shouldBe True True
