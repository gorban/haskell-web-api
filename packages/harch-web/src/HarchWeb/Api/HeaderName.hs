-- | HTTP field-name validation shared by typed API request lookup and response
-- emission. This is an internal collaborator; the public API re-exports its
-- abstract type and checked constructor through 'HarchWeb.Api.Request'.
module HarchWeb.Api.HeaderName
  ( ApiHeaderName,
    apiHeaderName,
    apiHeaderNameLiteral,
    apiHeaderNameText,
  )
where

import Data.Char (isAlphaNum, isAscii, isAsciiUpper, toLower)
import Data.Text (Text)
import Data.Text qualified as Text

newtype ApiHeaderName = ApiHeaderName Text
  deriving (Eq, Show)

-- | Construct an HTTP field name only when every character is an ASCII RFC
-- 9110 token character. Names are stored in ASCII-lowercase form so request
-- lookup remains case-insensitive without Unicode case-folding.
apiHeaderName :: Text -> Maybe ApiHeaderName
apiHeaderName name
  | Text.null name || not (Text.all isFieldNameCharacter name) = Nothing
  | otherwise = Just (ApiHeaderName (Text.map asciiLower name))

-- | Internal constructor for fixed framework protocol names. Public callers
-- must use 'apiHeaderName' and handle malformed runtime input explicitly.
apiHeaderNameLiteral :: Text -> ApiHeaderName
apiHeaderNameLiteral = ApiHeaderName . Text.map asciiLower

apiHeaderNameText :: ApiHeaderName -> Text
apiHeaderNameText (ApiHeaderName name) = name

isFieldNameCharacter :: Char -> Bool
isFieldNameCharacter character =
  character == '!'
    || character == '#'
    || character == '$'
    || character == '%'
    || character == '&'
    || character == '\''
    || character == '*'
    || character == '+'
    || character == '-'
    || character == '.'
    || character == '^'
    || character == '_'
    || character == '`'
    || character == '|'
    || character == '~'
    || (isAscii character && isAlphaNum character)

asciiLower :: Char -> Char
asciiLower character
  | isAsciiUpper character = toLower character
  | otherwise = character
