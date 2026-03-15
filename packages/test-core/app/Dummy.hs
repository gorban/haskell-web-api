-- This dummy executable is required so we can have a "build-type: Custom" package,
-- and so other packages can depend on this package without getting errors like:
-- Unknown build target 'exe:test-core'.
-- There is no executable component 'test-core'.
module Main where

main :: IO ()
main = return ()
