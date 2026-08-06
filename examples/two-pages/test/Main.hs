module Main (main) where

import E2E.AppSpec qualified
import Test.Hspec (hspec)
import Unit.AppSpec qualified
import Unit.NativeUploadSpec qualified

main :: IO ()
main = hspec $ do
  E2E.AppSpec.spec
  Unit.AppSpec.spec
  Unit.NativeUploadSpec.spec
