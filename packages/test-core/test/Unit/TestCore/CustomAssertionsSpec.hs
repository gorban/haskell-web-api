{-# LANGUAGE TemplateHaskell #-}

{-# SPEC #-}

import Control.Exception (evaluate, try)
import Test.HUnit.Lang (FailureReason (Reason), HUnitFailure (HUnitFailure))

spec = describe "shouldContain'" $ do
  it "should match a substring" $ do
    result <- "Hello, World!" `shouldContain'` "World"
    {-
      Force evaluation to prevent "never executed" on the `()` here:
      shouldContain' haystack needle =
        if needle `isInfixOf` haystack
          then pure ()
    -}
    evaluate result

  it "should fail when the substring is not found" $ do
    result <- try $ "Hello, World!" `shouldContain'` "Hello!"
    msg <- $([|result|] `shouldMatch` [p|Left (HUnitFailure _ (Reason msg))|])
    msg `shouldBe` "expected to contain: Hello!\n            but got: Hello, World!"
