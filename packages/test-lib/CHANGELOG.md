# Revision history for test-lib

## 0.1.0.0 -- 2026-01-19

* Some initial testing utilities. For example TestLib.CustomAssertions (`shouldContain'`) for fluent substring assertions.
* SpecPreprocessor allows us to template test files with simple `{-# SPEC #-}`.
* The spec template also includes a TestLib.Prelude to import [hspec](https://hackage.haskell.org/package/hspec) and third party [hspec-expectations-match](https://hackage.haskell.org/package/hspec-expectations-match). It also defines the module export `spec :: Spec` for use with [hspec-discover](https://hackage.haskell.org/package/hspec-discover).
