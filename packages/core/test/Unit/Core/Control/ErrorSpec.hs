{-# SPEC #-}

import Control.Monad.Except (throwError)

spec = describe "handleError" $ do
  it "returns the value when computation succeeds" $ do
    result <- pure "success" `handleError` (\_ -> pure "handled")
    result `shouldBe` "success"

  it "calls handler when computation fails" $ do
    result <- throwError "error" `handleError` (\e -> pure $ "handled: " ++ e)
    result `shouldBe` "handled: error"

  it "passes the error to the handler" $ do
    let expectedError = "specific error message"
    result <- throwError expectedError `handleError` pure
    result `shouldBe` expectedError
