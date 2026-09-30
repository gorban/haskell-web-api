module Main (main) where

import Test.Hspec (hspec)
import Unit.Catalog.ApiSpec qualified
import Unit.Catalog.DomainSpec qualified

main :: IO ()
main = hspec $ do
  Unit.Catalog.ApiSpec.spec
  Unit.Catalog.DomainSpec.spec
