module HarchWeb.Username
  ( Username,
    mkUsername,
    usernameText,
  )
where

import Data.Char (isAlphaNum, isAscii)
import Data.Text (Text)
import Data.Text qualified as Text

-- | A source-app compatible account handle. It deliberately preserves the
-- supplied spelling; persistence owns case-insensitive uniqueness policy.
newtype Username = Username
  { usernameText :: Text
  }
  deriving (Eq, Show)

mkUsername :: Text -> Maybe Username
mkUsername value
  | Text.length value < 3 || Text.length value > 20 = Nothing
  | Text.all isUsernameCharacter value = Just (Username value)
mkUsername _ = Nothing

isUsernameCharacter :: Char -> Bool
isUsernameCharacter character =
  (isAscii character && isAlphaNum character) || character == '_' || character == '-'
