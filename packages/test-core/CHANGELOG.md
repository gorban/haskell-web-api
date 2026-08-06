# Revision history for test-core

## 0.1.2.0

* Added `setInputFiles` to `TestCore.Browser.Scenario`: attaches a real file already on disk (e.g. via
  `System.IO.Temp.withSystemTempFile`) to a file input, so an E2E scenario can drive a native
  multipart-upload form without moving file bytes through the JSON command channel.

## 0.1.1.0

* Aligned changelog headings with the repository-wide version-first format used for the upcoming `0.1.1.0` work.
* Added `{-# E2E_SPEC #-}` preprocessing and `TestCore.E2EPrelude` so browser-facing specs can grow from the same Haskell test flow.
* Added a first browser-runner boundary in `TestCore.Browser`, with environment-driven runner/headless/keep-open options and a script protocol that keeps Haskell-authored specs on one side and a future official Node/Playwright runner on the other.

## 0.1.0.1

* Upgraded GHC to 9.14.1 and Base to 4.22.0.0

## 0.1.0.0

* Some initial testing utilities. For example TestCore.CustomAssertions (`shouldContain'`) for fluent substring assertions.
* SpecPreprocessor allows us to template test files with simple `{-# SPEC #-}`.
* The spec template also includes a TestCore.Prelude to import [hspec](https://hackage.haskell.org/package/hspec) and third party [hspec-expectations-match](https://hackage.haskell.org/package/hspec-expectations-match). It also defines the module export `spec :: Spec` for use with [hspec-discover](https://hackage.haskell.org/package/hspec-discover).
