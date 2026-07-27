{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Private JSON subset used by the ACME protocol implementation.
--
-- The facade keeps its compatibility exports while ACME protocol code shares
-- this parser and encoder without growing the framework boundary.
module HarchWeb.Acme.Json
  ( JsonValue (..),
    escapeJsonCharacter,
    jsonArrayBytes,
    jsonArrayItems,
    jsonBoolBytes,
    jsonObjectBytes,
    jsonObjectEntryParser,
    jsonObjectFields,
    jsonOptionalTextArrayField,
    jsonOptionalTextField,
    jsonRequiredField,
    jsonRequiredTextField,
    jsonStringCharacterParser,
    jsonStringBytes,
    jsonTextField,
    jsonValueParser,
    parseJsonValue,
    unicodeJsonCharacterParser,
  )
where

import Control.Monad (replicateM)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Text.ParserCombinators.ReadP (ReadP, char, choice, eof, get, manyTill, pfail, readP_to_S, sepBy, skipSpaces, string, (<++))
import Text.Read (readMaybe)

data JsonValue
  = JsonObject [(Text, JsonValue)]
  | JsonArray [JsonValue]
  | JsonString Text
  | JsonBool Bool
  | JsonNull

jsonObjectFields :: String -> JsonValue -> Either String [(Text, JsonValue)]
jsonObjectFields label value =
  case value of
    JsonObject fields -> Right fields
    _ -> Left (label <> " was not a JSON object")

jsonArrayItems :: String -> JsonValue -> Either String [JsonValue]
jsonArrayItems label value =
  case value of
    JsonArray items -> Right items
    _ -> Left (label <> " was not a JSON array")

jsonRequiredField :: Text -> [(Text, JsonValue)] -> Either String JsonValue
jsonRequiredField fieldName fields =
  maybe
    (Left ("missing required field " <> Text.unpack fieldName))
    Right
    (lookup fieldName fields)

jsonRequiredTextField :: Text -> [(Text, JsonValue)] -> Either String Text
jsonRequiredTextField fieldName fields =
  jsonTextField fieldName =<< jsonRequiredField fieldName fields

jsonOptionalTextField :: Text -> [(Text, JsonValue)] -> Either String (Maybe Text)
jsonOptionalTextField !fieldName fields =
  case lookup fieldName fields of
    Nothing -> Right Nothing
    Just JsonNull -> Right Nothing
    Just fieldValue -> Just <$> jsonTextField fieldName fieldValue

jsonOptionalTextArrayField :: Text -> [(Text, JsonValue)] -> Either String (Maybe [Text])
jsonOptionalTextArrayField !fieldName fields =
  case lookup fieldName fields of
    Nothing -> Right Nothing
    Just JsonNull -> Right Nothing
    Just fieldValue -> Just <$> (jsonArrayItems (Text.unpack fieldName) fieldValue >>= traverse (jsonTextField fieldName))

jsonTextField :: Text -> JsonValue -> Either String Text
jsonTextField fieldName fieldValue =
  case fieldValue of
    JsonString fieldText -> Right fieldText
    _ -> Left ("field " <> Text.unpack fieldName <> " was not a JSON string")

parseJsonValue :: LazyByteString.ByteString -> Either String JsonValue
parseJsonValue inputBytes =
  case [parsedValue | (parsedValue, _remainingInput) <- readP_to_S (jsonValueParser <* skipSpaces <* eof) inputText] of
    parsedValue : _ -> Right parsedValue
    [] -> Left "invalid JSON"
  where
    inputText = Text.unpack (TextEncoding.decodeUtf8 (LazyByteString.toStrict inputBytes))

jsonValueParser :: ReadP JsonValue
jsonValueParser =
  skipSpaces
    *> choice
      [ JsonObject <$> jsonObjectParser,
        JsonArray <$> jsonArrayParser,
        JsonString . Text.pack <$> jsonStringParser,
        JsonBool True <$ string "true",
        JsonBool False <$ string "false",
        JsonNull <$ string "null"
      ]
    <* skipSpaces

jsonObjectParser :: ReadP [(Text, JsonValue)]
jsonObjectParser = do
  _ <- char '{'
  skipSpaces
  (char '}' $> [])
    <++ do
      fields <- sepBy jsonObjectEntryParser (skipSpaces *> char ',' <* skipSpaces)
      skipSpaces
      _ <- char '}'
      pure fields

jsonObjectEntryParser :: ReadP (Text, JsonValue)
jsonObjectEntryParser = do
  fieldName <- Text.pack <$> jsonStringParser
  skipSpaces
  _ <- char ':'
  fieldValue <- jsonValueParser
  pure (fieldName, fieldValue)

jsonArrayParser :: ReadP [JsonValue]
jsonArrayParser = do
  _ <- char '['
  skipSpaces
  (char ']' $> [])
    <++ do
      items <- sepBy jsonValueParser (skipSpaces *> char ',' <* skipSpaces)
      skipSpaces
      _ <- char ']'
      pure items

jsonStringParser :: ReadP String
jsonStringParser =
  char '"' *> manyTill jsonStringCharacterParser (char '"')

jsonStringCharacterParser :: ReadP Char
jsonStringCharacterParser = do
  nextCharacter <- get
  if nextCharacter == '\\'
    then escapedJsonCharacterParser
    else pure nextCharacter

escapedJsonCharacterParser :: ReadP Char
escapedJsonCharacterParser = do
  choice
    [ '"' <$ char '"',
      '\\' <$ char '\\',
      '/' <$ char '/',
      '\b' <$ char 'b',
      '\f' <$ char 'f',
      '\n' <$ char 'n',
      '\r' <$ char 'r',
      '\t' <$ char 't',
      unicodeJsonCharacterParser
    ]

unicodeJsonCharacterParser :: ReadP Char
unicodeJsonCharacterParser = do
  _ <- char 'u'
  hexDigits <- replicateM 4 get
  maybe pfail (pure . toEnum) (readMaybe ("0x" <> hexDigits))

jsonStringBytes :: Text -> LazyByteString.ByteString
jsonStringBytes textValue =
  LazyByteString.fromStrict . TextEncoding.encodeUtf8 $
    "\""
      <> Text.concatMap escapeJsonCharacter textValue
      <> "\""

escapeJsonCharacter :: Char -> Text
escapeJsonCharacter character =
  case character of
    '"' -> "\\\""
    '\\' -> "\\\\"
    '\b' -> "\\b"
    '\f' -> "\\f"
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    _ -> Text.singleton character

jsonBoolBytes :: Bool -> LazyByteString.ByteString
jsonBoolBytes boolValue =
  if boolValue
    then "true"
    else "false"

jsonArrayBytes :: [LazyByteString.ByteString] -> LazyByteString.ByteString
jsonArrayBytes items =
  "[" <> LazyByteString.intercalate "," items <> "]"

jsonObjectBytes :: [(Text, LazyByteString.ByteString)] -> LazyByteString.ByteString
jsonObjectBytes fields =
  "{"
    <> LazyByteString.intercalate
      ","
      [ jsonStringBytes fieldName <> ":" <> fieldValue
      | (fieldName, fieldValue) <- fields
      ]
    <> "}"
