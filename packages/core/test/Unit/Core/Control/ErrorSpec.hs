{-# SPEC #-}

import Control.Monad.Except (runExceptT, throwError)

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

  it "lifts present optional values and explains missing ones" $ do
    runExceptT (fromMaybeError "missing" (Just "present")) `shouldReturn` Right "present"
    runExceptT (fromMaybeError "missing" (Nothing :: Maybe String)) `shouldReturn` Left "missing"

  it "continues only when a required condition holds" $ do
    runExceptT (guardError "rejected" True) `shouldReturn` Right ()
    runExceptT (guardError "rejected" False) `shouldReturn` Left "rejected"
