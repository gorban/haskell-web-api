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
    parseHeaders,
    parseNonNegativeInt,
    parsePositiveInt,
  )
where

import Control.Exception (IOException, displayException, evaluate, try)
import Data.Char (isDigit)
import Data.Foldable (asum)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
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

configLayersByPrecedence :: ConfigLayers -> [[(Text, Text)]]
configLayersByPrecedence
  ConfigLayers
    { configLayerCommittedDefaults = committedDefaults,
      configLayerLocalOverrides = localOverrides,
      configLayerEnvironmentOverrides = environmentOverrides
    } =
    [environmentOverrides, localOverrides, committedDefaults]

lookupConfigValue :: Text -> ConfigLayers -> Maybe Text
lookupConfigValue key =
  asum . map (lookup key . reverse) . configLayersByPrecedence

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

-- | Parse a comma-delimited configuration value into at least one non-empty,
-- trimmed entry.  The non-empty result is part of the contract: callers need
-- not recover from an empty list after this parser has accepted the input.
parseDelimitedTexts :: Text -> Text -> Either ConfigParseError (NonEmpty Text)
parseDelimitedTexts key value =
  maybe
    (Left (InvalidConfigValue key value))
    Right
    (nonEmpty (parseDelimitedTextsUnsafe "," value))

parseDelimitedTextsUnsafe :: Text -> Text -> [Text]
parseDelimitedTextsUnsafe delimiter =
  filter (not . Text.null)
    . map Text.strip
    . Text.splitOn delimiter

parseHeaders :: Text -> Text -> Either ConfigParseError [(Text, Text)]
parseHeaders key value =
  traverse parseHeaderPair (parseDelimitedTextsUnsafe ";" value)
  where
    parseHeaderPair headerEntry =
      let (rawHeaderName, headerValueWithSeparator) = Text.breakOn "=" headerEntry
          headerName = Text.strip rawHeaderName
       in if Text.null headerName || Text.null headerValueWithSeparator
            then Left (InvalidConfigValue key value)
            else Right (headerName, Text.strip (Text.drop 1 headerValueWithSeparator))

declaredIndices :: Text -> [(Text, Text)] -> [Int]
declaredIndices entryPrefix =
  Set.toAscList . Set.fromList . mapMaybe (extractIndexedKey entryPrefix . fst)

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
