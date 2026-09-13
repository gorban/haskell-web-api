{-# LANGUAGE DeriveFunctor #-}

-- | Small, application-owned validation rail for independent account-form
-- inputs.
--
-- Decision (AHI-9, 2026-08-31): registration and login both need to report
-- every independent field error in declaration order.  This is an
-- applicative validation, not a monad: a lawful fail-fast bind would discard
-- the remaining checks.  Effects begin only after 'validationResult' returns
-- a valid domain input, at which point the existing 'WebApi.AppEffect.AppM'
-- rail remains the single effectful workflow boundary.
module WebApi.AccountPages.Validation
  ( Validation,
    invalid,
    valid,
    validate3,
    validate4,
    validationResult,
  )
where

import Data.List.NonEmpty (NonEmpty (..))

data Validation error value
  = Invalid (NonEmpty error)
  | Valid value
  deriving (Functor)

instance Applicative (Validation error) where
  pure = Valid

  functionValidation <*> valueValidation =
    case (functionValidation, valueValidation) of
      (Valid transform, Valid value) -> Valid (transform value)
      (Invalid leftErrors, Invalid rightErrors) -> Invalid (leftErrors <> rightErrors)
      (Invalid errors, Valid _) -> Invalid errors
      (Valid _, Invalid errors) -> Invalid errors

invalid :: error -> Validation error value
invalid errorValue = Invalid (errorValue :| [])

-- | Accept one independently validated value.
valid :: value -> Validation error value
valid = pure

-- | Combine three independent checks, retaining every reported error in
-- declaration order.
validate3 :: (first -> second -> third -> result) -> Validation error first -> Validation error second -> Validation error third -> Validation error result
validate3 constructor first second third =
  constructor <$> first <*> second <*> third

-- | Combine four independent checks, retaining every reported error in
-- declaration order.
validate4 :: (first -> second -> third -> fourth -> result) -> Validation error first -> Validation error second -> Validation error third -> Validation error fourth -> Validation error result
validate4 constructor first second third fourth =
  constructor <$> first <*> second <*> third <*> fourth

validationResult :: Validation error value -> Either (NonEmpty error) value
validationResult validation =
  case validation of
    Invalid errors -> Left errors
    Valid value -> Right value
