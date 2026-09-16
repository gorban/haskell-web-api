module Main (main) where

import Test.Hspec (hspec)
import Unit.Catalog.DomainSpec qualified

main :: IO ()
main = hspec Unit.Catalog.DomainSpec.spec
