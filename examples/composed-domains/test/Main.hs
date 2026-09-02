module Main (main) where

import E2E.ComposedSpec qualified
import Test.Hspec (hspec)
import Unit.App.ComposedSpec qualified

main :: IO ()
main = hspec $ do
  E2E.ComposedSpec.spec
  Unit.App.ComposedSpec.spec
