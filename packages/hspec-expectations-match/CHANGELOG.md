# Local changes to upstream to third party package hspec-expectations-match (2026-01-19)

1. Source for third party package pulled from:
   <https://hackage.haskell.org/package/hspec-expectations-match-0.2.0.0/hspec-expectations-match-0.2.0.0.tar.gz>
2. It had a maximum Template Haskell below 2.1.3, but we're on 2.24.0, so it had to be upgraded to increase the maximum.

# 0.2.0.0 (November 17th, 2017)

- Added `assertDo`, which automatically instruments a `do` block with uses of `shouldReturnAndMatch`.

# 0.1.1.0 (November 15th, 2017)

- Added `shouldReturnAndMatch`, which combines the action-running behavior of `shouldReturn` with the pattern-matching behavior of `shouldMatch`.
- Added a missing `HasCallStack` constraint to properly provide source location information for failed expectations.

# 0.1.0.0 (November 15th, 2017)

- Initial release.
