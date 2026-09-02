module Main (main) where

import Test.Hspec (hspec)
import Unit.Orders.DomainSpec qualified

main :: IO ()
main = hspec Unit.Orders.DomainSpec.spec
