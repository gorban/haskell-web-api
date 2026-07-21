{-# LANGUAGE TemplateHaskell #-}

{-# SPEC #-}

import Control.Exception (IOException, evaluate, throwIO, try)
import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Test.HUnit.Lang (FailureReason (ExpectedButGot, Reason), HUnitFailure (HUnitFailure))

spec = do
  describe "shouldContain'" $ do
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

  describe "expectAll" $ do
    it "accepts independent expectations when they all pass" $
      expectAll ((1 `shouldBe` (1 :: Int)) :| ["ready" `shouldContain'` "read"])

    it "reports independent assertion failures in their original order" $ do
      result <- try $ expectAll ((1 `shouldBe` (2 :: Int)) :| ["alpha" `shouldContain'` "beta"])
      message <- $([|result|] `shouldMatch` [p|Left (HUnitFailure _ (Reason message))|])
      message `shouldSatisfy` ("1) expected: 2" `isInfixOf`)
      message `shouldSatisfy` ("2) expected to contain: beta" `isInfixOf`)

    it "preserves assertion prefaces while aggregating failures" $ do
      result <- try $ expectAll (throwIO (HUnitFailure Nothing (ExpectedButGot (Just "context") "expected" "actual")) :| [])
      message <- $([|result|] `shouldMatch` [p|Left (HUnitFailure _ (Reason message))|])
      message `shouldSatisfy` ("context\nexpected: expected\n but got: actual" `isInfixOf`)

    it "immediately rethrows unexpected exceptions" $ do
      result <- try $ expectAll (pure () :| [throwIO (userError "unexpected")])
      exception <- $([|result|] `shouldMatch` [p|Left exception|])
      show (exception :: IOException) `shouldContain'` "unexpected"
