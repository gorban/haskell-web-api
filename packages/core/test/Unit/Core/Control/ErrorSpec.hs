{-# SPEC #-}

import Control.Monad.Except (runExceptT, throwError)
import Data.List.NonEmpty (NonEmpty (..))

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
    expectAll
      ( (runExceptT (fromMaybeError "missing" (Just "present")) `shouldReturn` Right "present")
          :| [runExceptT (fromMaybeError "missing" (Nothing :: Maybe String)) `shouldReturn` Left "missing"]
      )

  it "continues only when a required condition holds" $ do
    expectAll
      ( (runExceptT (guardError "rejected" True) `shouldReturn` Right ())
          :| [runExceptT (guardError "rejected" False) `shouldReturn` Left "rejected"]
      )

  it "lifts effectful Either values while mapping their errors" $ do
    expectAll
      ( (runExceptT (liftEitherWith length (pure (Right "value" :: Either String String))) `shouldReturn` Right "value")
          :| [runExceptT (liftEitherWith length (pure (Left "failed" :: Either String String))) `shouldReturn` Left 6]
      )
