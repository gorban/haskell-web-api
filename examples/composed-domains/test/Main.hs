module Main (main) where

import E2E.ComposedSpec qualified
import Test.Hspec (hspec)
import Unit.App.Composed.AdmissionProofSpec qualified
import Unit.App.Composed.AdmissionTypesSpec qualified
import Unit.App.Composed.CsrfSynchronizerSpec qualified
import Unit.App.ComposedSpec qualified

main :: IO ()
main = hspec $ do
  E2E.ComposedSpec.spec
  Unit.App.Composed.AdmissionProofSpec.spec
  Unit.App.Composed.AdmissionTypesSpec.spec
  Unit.App.Composed.CsrfSynchronizerSpec.spec
  Unit.App.ComposedSpec.spec
