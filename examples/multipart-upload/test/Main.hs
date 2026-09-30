module Main (main) where

import E2E.MultipartUploadSpec qualified
import Test.Hspec (hspec)
import Unit.MultipartUploadSpec qualified

main :: IO ()
main = hspec $ do
  E2E.MultipartUploadSpec.spec
  Unit.MultipartUploadSpec.spec
