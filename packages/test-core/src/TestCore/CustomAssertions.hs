module TestCore.CustomAssertions
  ( expectAll,
    shouldContain',
  )
where

import Control.Exception (SomeException, fromException, throwIO, try)
import Control.Monad (forM, unless)
import Data.List (intercalate, isInfixOf)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (catMaybes)
import Test.HUnit.Lang (FailureReason (..), HUnitFailure (..))
import Test.Hspec (Expectation, expectationFailure)

-- | Run independent expectations and report every assertion failure together.
-- Unexpected exceptions still stop the test immediately.
expectAll :: NonEmpty Expectation -> Expectation
expectAll expectations = do
  failures <- fmap catMaybes . forM (NonEmpty.toList expectations) $ \expectation -> do
    result <- try expectation
    case result of
      Right () -> pure Nothing
      Left exception ->
        case asHUnitFailure exception of
          Just failure -> pure (Just (renderHUnitFailure failure))
          Nothing -> throwIO exception
  unless (null failures) $
    expectationFailure ("independent expectations failed:\n" <> intercalate "\n" (zipWith renderFailure [(1 :: Int) ..] failures))
  where
    renderFailure index message = show index <> ") " <> message

asHUnitFailure :: SomeException -> Maybe HUnitFailure
asHUnitFailure = fromException

renderHUnitFailure :: HUnitFailure -> String
renderHUnitFailure (HUnitFailure _ reason) =
  case reason of
    Reason message -> message
    ExpectedButGot preface expected actual ->
      maybe "" (<> "\n") preface
        <> "expected: "
        <> expected
        <> "\n but got: "
        <> actual

shouldContain' :: String -> String -> Expectation
shouldContain' haystack needle =
  if needle `isInfixOf` haystack
    then pure ()
    else
      expectationFailure
        ( ("expected to contain: " ++ needle)
            ++ ("\n            but got: " ++ haystack)
        )
