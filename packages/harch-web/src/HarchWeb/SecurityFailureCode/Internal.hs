-- | Internal representation of validated security classifications.
--
-- 'knownSecurityFailureCode' is for fixed literals owned by Harch itself.
-- It is deliberately hidden from package users: application-supplied values
-- must enter through 'HarchWeb.SecurityFailureCode.mkSecurityFailureCode'.
module HarchWeb.SecurityFailureCode.Internal
  ( SecurityFailureCode (..),
    knownSecurityFailureCode,
  )
where

import Data.Text (Text)

newtype SecurityFailureCode = SecurityFailureCode Text
  deriving (Eq, Ord, Show)

knownSecurityFailureCode :: Text -> SecurityFailureCode
knownSecurityFailureCode = SecurityFailureCode
