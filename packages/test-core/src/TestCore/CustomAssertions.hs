module TestCore.CustomAssertions
  ( expectAll,
    shouldContain',
    equalValues,
    notEqualValues,
    renderedValue,
    renderedWithPrecedence,
    renderedValueList,
    minimumValue,
    maximumValue,
    successorValue,
    predecessorValue,
    valueToEnum,
    valueFromEnum,
    valuesFrom,
    valuesFromThen,
    valuesFromTo,
    valuesFromThenTo,
    exerciseClosedEnumeration,
  )
where

import Control.Exception (SomeException, fromException, throwIO, try)
import Control.Monad (forM, unless)
import Data.List (intercalate, isInfixOf)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (catMaybes)
import Test.HUnit.Lang (FailureReason (..), HUnitFailure (..))
import Test.Hspec (Expectation, expectationFailure, shouldBe, shouldSatisfy)

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

-- | Named wrapper around '(==)' for exercising a derived 'Eq' instance's
-- own 'HPC' box directly, rather than letting the optimizer inline a bare
-- '==' call site back into the class default. See @docs/design-guidance.md@
-- and this project's coverage-gate notes on derived-instance coverage.
equalValues :: (Eq value) => value -> value -> Bool
equalValues = (==)
{-# NOINLINE equalValues #-}

-- | Named wrapper around '(/=)', the '/=' companion to 'equalValues'.
notEqualValues :: (Eq value) => value -> value -> Bool
notEqualValues = (/=)
{-# NOINLINE notEqualValues #-}

-- | Named wrapper around 'show', the 'Show' companion to 'equalValues'.
renderedValue :: (Show value) => value -> String
renderedValue = show
{-# NOINLINE renderedValue #-}

-- | Named wrapper around 'showsPrec', for exercising a derived 'Show'
-- instance's parenthesization branch at a precedence above 10.
renderedWithPrecedence :: (Show value) => Int -> value -> ShowS
renderedWithPrecedence = showsPrec
{-# NOINLINE renderedWithPrecedence #-}

-- | Named wrapper around 'showList', the list-of-@value@ companion needed to
-- exercise a derived 'Show' instance's own 'showList' default method.
renderedValueList :: (Show value) => [value] -> ShowS
renderedValueList = showList
{-# NOINLINE renderedValueList #-}

-- | Named wrapper around 'minBound'.
minimumValue :: (Bounded value) => value
minimumValue = minBound
{-# NOINLINE minimumValue #-}

-- | Named wrapper around 'maxBound'.
maximumValue :: (Bounded value) => value
maximumValue = maxBound
{-# NOINLINE maximumValue #-}

-- | Named wrapper around 'succ'.
successorValue :: (Enum value) => value -> value
successorValue = succ
{-# NOINLINE successorValue #-}

-- | Named wrapper around 'pred'.
predecessorValue :: (Enum value) => value -> value
predecessorValue = pred
{-# NOINLINE predecessorValue #-}

-- | Named wrapper around 'toEnum'.
valueToEnum :: (Enum value) => Int -> value
valueToEnum = toEnum
{-# NOINLINE valueToEnum #-}

-- | Named wrapper around 'fromEnum'.
valueFromEnum :: (Enum value) => value -> Int
valueFromEnum = fromEnum
{-# NOINLINE valueFromEnum #-}

-- | Named wrapper around 'enumFrom'.
valuesFrom :: (Enum value) => value -> [value]
valuesFrom = enumFrom
{-# NOINLINE valuesFrom #-}

-- | Named wrapper around 'enumFromThen'.
valuesFromThen :: (Enum value) => value -> value -> [value]
valuesFromThen = enumFromThen
{-# NOINLINE valuesFromThen #-}

-- | Named wrapper around 'enumFromTo'.
valuesFromTo :: (Enum value) => value -> value -> [value]
valuesFromTo = enumFromTo
{-# NOINLINE valuesFromTo #-}

-- | Named wrapper around 'enumFromThenTo'.
valuesFromThenTo :: (Enum value) => value -> value -> value -> [value]
valuesFromThenTo = enumFromThenTo
{-# NOINLINE valuesFromThenTo #-}

-- | Exhaustively exercises a closed, contiguous 'Bounded'/'Enum' family
-- (every 'minBound'/'maxBound'/'succ'/'pred'/'toEnum'/'fromEnum'/'enumFrom*'
-- method, plus a same-value 'Eq' check, a different-value 'Eq' check, and a
-- 'Show'/'showsPrec'/'showList' exercise) in one call, so a derived instance
-- on a new closed sum type gets full coverage without hand-writing each
-- check. @values@ must be the family's full @[minBound .. maxBound]@ listed
-- in declaration order; @firstValue@/@secondValue@/@penultimateValue@/
-- @lastValue@ name its first two and last two constructors.
{-# ANN exerciseClosedEnumeration ("HLint: ignore Redundant $!" :: String) #-}
exerciseClosedEnumeration ::
  (Bounded value, Enum value, Eq value, Show value) =>
  [value] ->
  value ->
  value ->
  value ->
  value ->
  Expectation
exerciseClosedEnumeration values firstValue secondValue penultimateValue lastValue = do
  minimumValue `shouldBe` firstValue
  maximumValue `shouldBe` lastValue
  successorValue firstValue `shouldBe` secondValue
  predecessorValue lastValue `shouldBe` penultimateValue
  valueToEnum 0 `shouldBe` firstValue
  valueFromEnum firstValue `shouldBe` 0
  valuesFrom firstValue `shouldBe` values
  valuesFromThen firstValue secondValue `shouldBe` values
  valuesFromTo firstValue lastValue `shouldBe` values
  valuesFromThenTo firstValue secondValue lastValue `shouldBe` values
  equalValues firstValue firstValue `shouldBe` True
  notEqualValues firstValue lastValue `shouldBe` True
  renderedValue firstValue `shouldSatisfy` not . null
  -- \$!-forced: bare `11`/`""` literals here get CSE-shared with other
  -- occurrences elsewhere in this module under -O2, permanently leaving one
  -- of the two occurrences' HPC box unticked. See
  -- docs/design-guidance.md's never-mask-a-gate-finding section.
  ((renderedWithPrecedence $! 11) firstValue $! "") `shouldSatisfy` not . null
  (renderedValueList values $! "") `shouldSatisfy` not . null
{-# NOINLINE exerciseClosedEnumeration #-}
