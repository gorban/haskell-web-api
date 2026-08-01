{-# LANGUAGE OverloadedStrings #-}

module Core.Config
  ( ConfigOverridesFileError (..),
    ConfigLayers (..),
    ConfigParseError (..),
    declaredIndices,
    indexedConfigKey,
    loadConfigOverridesFile,
    lookupConfigValue,
    parseConfigOverridesFile,
    parseBoolean,
    parseDelimitedTexts,
    parseDelimitedTextsUnsafe,
    parseHeadersUnsafe,
    parseNonNegativeInt,
    parsePositiveInt,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (IOException, displayException, evaluate, try)
import Data.Char (isDigit)
import Data.List (nub, sort)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import System.Directory (doesPathExist)
import Text.Read (readMaybe)

data ConfigParseError
  = MissingConfigValue Text
  | InvalidConfigValue Text Text
  deriving (Eq, Show)

data ConfigOverridesFileError
  = InvalidConfigOverridesLine Int Text
  | UnreadableConfigOverridesFile Text
  deriving (Eq, Show)

data ConfigLayers = ConfigLayers
  { configLayerCommittedDefaults :: [(Text, Text)],
    configLayerLocalOverrides :: [(Text, Text)],
    configLayerEnvironmentOverrides :: [(Text, Text)]
  }

loadConfigOverridesFile :: FilePath -> IO (Either ConfigOverridesFileError [(Text, Text)])
loadConfigOverridesFile overridesPath = do
  overridesPathExists <- doesPathExist overridesPath
  if overridesPathExists
    then loadExistingOverridesFile
    else pure (Right [])
  where
    loadExistingOverridesFile = do
      overridesReadResult <-
        ( try $ do
            fileContents <- TextIO.readFile overridesPath
            _ <- evaluate (Text.length fileContents)
            pure fileContents
        ) ::
          IO (Either IOException Text)
      pure $
        either
          (Left . UnreadableConfigOverridesFile . Text.pack . displayException)
          parseConfigOverridesFile
          overridesReadResult

parseConfigOverridesFile :: Text -> Either ConfigOverridesFileError [(Text, Text)]
parseConfigOverridesFile =
  fmap concat
    . traverse parseLine
    . zip [1 :: Int ..]
    . Text.lines
  where
    parseLine (lineNumber, rawLine) =
      let strippedLine = Text.strip rawLine
       in if Text.null strippedLine || Text.isPrefixOf "#" strippedLine
            then Right []
            else
              let (rawKey, rawValueWithSeparator) = Text.breakOn "=" strippedLine
                  strippedKey = Text.strip rawKey
               in if Text.null rawValueWithSeparator || Text.null strippedKey
                    then Left (InvalidConfigOverridesLine lineNumber rawLine)
                    else Right [(strippedKey, Text.strip (Text.drop 1 rawValueWithSeparator))]

lookupConfigValue :: Text -> ConfigLayers -> Maybe Text
lookupConfigValue
  key
  ConfigLayers
    { configLayerCommittedDefaults = committedDefaults,
      configLayerLocalOverrides = localOverrides,
      configLayerEnvironmentOverrides = environmentOverrides
    } =
    lookupInLayer environmentOverrides
      `orElse` lookupInLayer localOverrides
      `orElse` lookupInLayer committedDefaults
    where
      lookupInLayer = lookup key . reverse

orElse :: Maybe value -> Maybe value -> Maybe value
orElse maybeValue fallbackValue =
  maybeValue <|> fallbackValue

parsePositiveInt :: Text -> Text -> Either ConfigParseError Int
parsePositiveInt key value =
  case readMaybe (Text.unpack value) of
    Just parsedInt
      | parsedInt > 0 -> Right parsedInt
    _ -> Left (InvalidConfigValue key value)

parseNonNegativeInt :: Text -> Text -> Either ConfigParseError Int
parseNonNegativeInt key value =
  case readMaybe (Text.unpack value) of
    Just parsedInt
      | parsedInt >= 0 -> Right parsedInt
    _ -> Left (InvalidConfigValue key value)

parseBoolean :: Text -> Text -> Either ConfigParseError Bool
parseBoolean key value =
  case Text.toLower value of
    "true" -> Right True
    "false" -> Right False
    "1" -> Right True
    "0" -> Right False
    "yes" -> Right True
    "no" -> Right False
    _ -> Left (InvalidConfigValue key value)

parseDelimitedTexts :: Text -> Text -> Either ConfigParseError [Text]
parseDelimitedTexts key value =
  case parseDelimitedTextsUnsafe "," value of
    [] -> Left (InvalidConfigValue key value)
    parsedValues -> Right parsedValues

parseDelimitedTextsUnsafe :: Text -> Text -> [Text]
parseDelimitedTextsUnsafe delimiter =
  filter (not . Text.null)
    . map Text.strip
    . Text.splitOn delimiter

parseHeadersUnsafe :: Text -> [(Text, Text)]
parseHeadersUnsafe value =
  mapMaybe parseHeaderPair (parseDelimitedTextsUnsafe ";" value)
  where
    parseHeaderPair headerEntry =
      let (headerName, headerValueWithSeparator) = Text.breakOn "=" headerEntry
       in if Text.null headerName || Text.null headerValueWithSeparator
            then Nothing
            else Just (Text.strip headerName, Text.strip (Text.drop 1 headerValueWithSeparator))

declaredIndices :: Text -> [(Text, Text)] -> [Int]
declaredIndices entryPrefix =
  sort . nub . mapMaybe (extractIndexedKey entryPrefix . fst)

extractIndexedKey :: Text -> Text -> Maybe Int
extractIndexedKey entryPrefix entryKey =
  if Text.isPrefixOf entryPrefix entryKey
    then
      let indexedSuffix = Text.drop (Text.length entryPrefix) entryKey
          (indexDigits, remainder) = Text.span isDigit indexedSuffix
       in if Text.null indexDigits || not (Text.isPrefixOf "_" remainder)
            then Nothing
            else readMaybe (Text.unpack indexDigits)
    else Nothing

indexedConfigKey :: Text -> Int -> Text -> Text
indexedConfigKey prefix configIndex suffix =
  prefix <> "_" <> Text.pack (show configIndex) <> "_" <> suffix
