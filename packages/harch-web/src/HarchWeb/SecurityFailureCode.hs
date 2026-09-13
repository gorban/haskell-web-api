{-# LANGUAGE OverloadedStrings #-}

-- | Bounded private security classifications shared by authentication and
-- security-event contracts. A value is never proof material, a claim,
-- account identity, or exception text.
module HarchWeb.SecurityFailureCode
  ( SecurityFailureCode,
    mkSecurityFailureCode,
  )
where

import Data.Char (isAsciiLower, isDigit)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.SecurityFailureCode.Internal (SecurityFailureCode, knownSecurityFailureCode)

mkSecurityFailureCode :: Text -> Either Text SecurityFailureCode
mkSecurityFailureCode value
  | Text.null value = Left "security failure code cannot be empty"
  | Text.length value > 80 = Left "security failure code is too long"
  | Text.all validCharacter value = Right (knownSecurityFailureCode value)
  | otherwise = Left "security failure code has invalid characters"
  where
    validCharacter character = isAsciiLower character || isDigit character || character == '.' || character == '-'
