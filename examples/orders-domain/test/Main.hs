module Main (main) where

import Test.Hspec (hspec)
import Unit.Orders.ApiSpec qualified
import Unit.Orders.DomainSpec qualified

main :: IO ()
main = hspec $ do
  Unit.Orders.ApiSpec.spec
  Unit.Orders.DomainSpec.spec
