-- | Compatibility facade for the dependency-light test spec preprocessor.
--
-- The implementation lives in @test-spec-preprocessor@ so every test package,
-- including @hspec-expectations-match@, can use its executable without a
-- package-level test dependency cycle.  Keep this facade for library users
-- that import the original @TestCore@ API.
module TestCore.SpecPreprocessor (run, runPure) where

import TestSpecPreprocessor (run, runPure)
