{-# LANGUAGE OverloadedStrings #-}

-- | Pure parsing and selection for @Accept@-based representations.
module HarchWeb.Api.Negotiation
  ( AcceptedRange (..),
    ApiNegotiationResult (..),
    parseMediaRange,
    parseAcceptHeader,
    selectRepresentation,
  )
where

import Data.Char (isAscii, isDigit)
import Data.List (foldl1')
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Read qualified as TextRead
import HarchWeb.Api.MediaType (ApiMediaType, apiMediaTypeParts, parseMediaRange)

data AcceptedRange = AcceptedRange
  { acceptedRangeType :: Text,
    acceptedRangeSubtype :: Text,
    acceptedRangeParameters :: [(Text, Text)],
    acceptedRangeQuality :: Double
  }
  deriving (Eq, Show)

-- | Parse an @Accept@ header value into its declared media ranges. A
-- malformed entry is dropped rather than failing the whole header.
parseAcceptHeader :: Text -> [AcceptedRange]
parseAcceptHeader headerValue =
  Maybe.mapMaybe parseAcceptEntry (Text.splitOn "," headerValue)

parseAcceptEntry :: Text -> Maybe AcceptedRange
parseAcceptEntry entry =
  let (mediaRangeText, parameterSection) = Text.breakOn ";" (Text.strip entry)
      parameterTexts =
        if Text.null parameterSection
          then []
          else Text.splitOn ";" (Text.drop 1 parameterSection)
   in do
        (typeText, subtypeText) <- parseMediaRange (Text.strip mediaRangeText)
        let parameters = Maybe.mapMaybe parseAcceptParameter parameterTexts
        quality <- qualityFromParameters parameters
        pure
          AcceptedRange
            { acceptedRangeType = Text.toLower typeText,
              acceptedRangeSubtype = Text.toLower subtypeText,
              acceptedRangeParameters = filter ((/= "q") . fst) parameters,
              acceptedRangeQuality = quality
            }

parseAcceptParameter :: Text -> Maybe (Text, Text)
parseAcceptParameter parameterText =
  case Text.breakOn "=" (Text.strip parameterText) of
    (name, value)
      | not (Text.null name),
        Text.isPrefixOf "=" value ->
          Just (Text.toLower name, Text.strip (Text.drop 1 value))
    _ -> Nothing

qualityFromParameters :: [(Text, Text)] -> Maybe Double
qualityFromParameters parameters =
  case lookup "q" parameters of
    Nothing -> Just 1.0
    Just qualityText -> parseQuality qualityText

parseQuality :: Text -> Maybe Double
parseQuality qualityText =
  case Text.splitOn "." qualityText of
    ["0"] -> Just 0.0
    ["0", fraction] | validFraction fraction -> parseFraction fraction
    ["1"] -> Just 1.0
    ["1", fraction] | validFraction fraction, Text.all (== '0') fraction -> Just 1.0
    _ -> Nothing

validFraction :: Text -> Bool
validFraction fraction = Text.length fraction <= 3 && Text.all isAsciiDigit fraction

parseFraction :: Text -> Maybe Double
parseFraction fraction =
  case TextRead.double ("0." <> fraction) of
    Right (quality, "") -> Just quality
    _ -> Nothing

isAsciiDigit :: Char -> Bool
isAsciiDigit character = isAscii character && isDigit character

mediaRangeSpecificity :: AcceptedRange -> Int
mediaRangeSpecificity range
  | acceptedRangeType range == "*" = 0
  | acceptedRangeSubtype range == "*" = 1
  | otherwise = 2

rangeMatchesRepresentation :: (Text, Text) -> AcceptedRange -> Bool
rangeMatchesRepresentation (declaredType, declaredSubtype) range =
  (acceptedRangeType range == "*" || acceptedRangeType range == declaredType)
    && (acceptedRangeSubtype range == "*" || acceptedRangeSubtype range == declaredSubtype)

-- | The single most specific range that applies to a declared representation.
-- Per RFC 9110 section 12.5.1, when more than one range in the header
-- applies to a representation, the most specific one governs its quality
-- regardless of a less specific range's own quality.
bestMatchingRange :: ApiMediaType -> [AcceptedRange] -> Maybe AcceptedRange
bestMatchingRange declaredMediaType ranges =
  case filter (rangeMatchesRepresentation (apiMediaTypeParts declaredMediaType)) ranges of
    [] -> Nothing
    matches -> Just (foldl1' preferMoreSpecific matches)
  where
    preferMoreSpecific left right =
      if mediaRangeSpecificity right > mediaRangeSpecificity left then right else left

data ApiNegotiationResult
  = -- | The @Accept@ header explicitly excludes every declared representation:
    -- respond @406 Not Acceptable@.
    NoAcceptableRepresentation
  | -- | The selected declared representation. A caller that declares more
    -- than one representation must add @Vary: Accept@ to the response.
    SelectedRepresentation ApiMediaType
  deriving (Eq, Show)

-- | Negotiate a response representation from a declared,
-- server-preference-ordered list and an optional @Accept@ header. A missing
-- header selects the first declared representation. An explicit header that
-- excludes every declared representation is @406@; otherwise each
-- representation's most specific matching range determines its quality, the
-- highest quality is selected, and ties keep server declaration order.
selectRepresentation :: NonEmpty ApiMediaType -> Maybe Text -> ApiNegotiationResult
selectRepresentation declaredRepresentations maybeAcceptHeader =
  case maybeAcceptHeader of
    Nothing -> SelectedRepresentation (NonEmpty.head declaredRepresentations)
    Just headerValue ->
      case acceptableCandidates (parseAcceptHeader headerValue) of
        [] -> NoAcceptableRepresentation
        candidates -> SelectedRepresentation (fst (foldl1' preferHigherQuality candidates))
  where
    acceptableCandidates ranges =
      [ (representation, acceptedRangeQuality bestRange)
      | representation <- NonEmpty.toList declaredRepresentations,
        Just bestRange <- [bestMatchingRange representation ranges],
        acceptedRangeQuality bestRange > 0
      ]
    preferHigherQuality left right =
      if snd right > snd left then right else left
