module TestLib.CustomAssertions (shouldContain') where

import Data.List (isInfixOf)
import Test.Hspec (Expectation, expectationFailure)

shouldContain' :: String -> String -> Expectation
shouldContain' haystack needle =
  if needle `isInfixOf` haystack
    then pure ()
    else
      expectationFailure
        ( ("expected to contain: " ++ needle)
            ++ ("\n            but got: " ++ haystack)
        )
