module Main (main) where

import Test.Hspec (hspec)
import qualified Unit.AppSpec

main :: IO ()
main = hspec Unit.AppSpec.spec
