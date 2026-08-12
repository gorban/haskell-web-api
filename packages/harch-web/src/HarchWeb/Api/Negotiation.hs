{-# LANGUAGE OverloadedStrings #-}

-- | Pure parsing and selection for @Accept@-based representations.
module HarchWeb.Api.Negotiation
  ( AcceptedRange (..),
    ApiNegotiationResult (..),
    ApiContentTypeNegotiationResult (..),
    parseMediaRange,
    parseAcceptHeader,
    selectRepresentation,
    selectContentTypeRepresentation,
  )
where

import Data.Char (digitToInt, isAscii, isDigit)
import Data.List (foldl1')
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Api.MediaType
  ( ApiContentType,
    ApiMediaType,
    apiContentTypeMediaType,
    apiContentTypeParameters,
    apiMediaTypeParts,
    parseMediaRange,
  )

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
      parameterTexts = maybe [] (Text.splitOn ";") (Text.stripPrefix ";" parameterSection)
   in do
        (typeText, subtypeText) <- parseMediaRange (Text.strip mediaRangeText)
        (mediaParameters, quality) <- acceptParameters parameterTexts
        pure
          AcceptedRange
            { acceptedRangeType = Text.toLower typeText,
              acceptedRangeSubtype = Text.toLower subtypeText,
              acceptedRangeParameters = mediaParameters,
              acceptedRangeQuality = quality
            }

-- | Parameters before @q@ describe the media range and constrain matching.
-- Parameters after it are RFC 9110 accept extensions, so they cannot make a
-- representation unacceptable. Malformed non-quality parameters are ignored,
-- preserving the established tolerant parsing behavior; a malformed quality
-- rejects just that comma-separated range.
acceptParameters :: [Text] -> Maybe ([(Text, Text)], Double)
acceptParameters = go []
  where
    go mediaParameters [] = Just (reverse mediaParameters, 1.0)
    go mediaParameters (parameterText : laterParameters) =
      case parseAcceptParameter parameterText of
        Nothing -> go mediaParameters laterParameters
        Just parameter@(name, value)
          | name == "q" ->
              case parseQuality value of
                Nothing -> Nothing
                Just quality -> Just (reverse mediaParameters, quality)
          | otherwise -> go (parameter : mediaParameters) laterParameters

parseAcceptParameter :: Text -> Maybe (Text, Text)
parseAcceptParameter parameterText =
  case Text.breakOn "=" (Text.strip parameterText) of
    (name, value)
      | not (Text.null name),
        Text.isPrefixOf "=" value ->
          Just (Text.toLower name, normalizedParameterValue (Text.drop 1 value))
    _ -> Nothing

normalizedParameterValue :: Text -> Text
normalizedParameterValue = Text.toLower . unquoteParameterValue . Text.strip

unquoteParameterValue :: Text -> Text
unquoteParameterValue value =
  Maybe.fromMaybe value (Text.stripPrefix "\"" value >>= Text.stripSuffix "\"")

parseQuality :: Text -> Maybe Double
parseQuality qualityText =
  case Text.splitOn "." qualityText of
    ["0"] -> Just 0.0
    ["0", fraction] | validFraction fraction -> Just (parseFraction fraction)
    ["1"] -> Just 1.0
    ["1", fraction] | validFraction fraction, Text.all (== '0') fraction -> Just 1.0
    _ -> Nothing

validFraction :: Text -> Bool
validFraction fraction = Text.length fraction <= 3 && Text.all isAsciiDigit fraction

-- The caller establishes that this is at most three ASCII digits, so this
-- conversion is total and does not need a partial numeric parser.
parseFraction :: Text -> Double
parseFraction fraction =
  let (numerator, width) =
        Text.foldl'
          (\(value, digitCount) digit -> (value * 10 + digitToInt digit, digitCount + 1))
          (0 :: Int, 0 :: Int)
          fraction
   in fromIntegral numerator / (10 ^ width)

isAsciiDigit :: Char -> Bool
isAsciiDigit character = isAscii character && isDigit character

mediaRangeSpecificity :: AcceptedRange -> (Int, Int)
mediaRangeSpecificity range =
  parameterCount `seq`
    ( if acceptedRangeType range == "*"
        then 0
        else
          if acceptedRangeSubtype range == "*"
            then 1
            else 2,
      parameterCount
    )
  where
    -- Evaluate the complete key when it is formed. A lazy tuple comparison can
    -- otherwise skip its parameter component whenever the wildcard component
    -- already decides the ordering.
    parameterCount = length (acceptedRangeParameters range)

rangeMatchesRepresentation :: (Text, Text) -> [(Text, Text)] -> AcceptedRange -> Bool
rangeMatchesRepresentation (declaredType, declaredSubtype) declaredParameters range =
  (acceptedRangeType range == "*" || acceptedRangeType range == declaredType)
    && (acceptedRangeSubtype range == "*" || acceptedRangeSubtype range == declaredSubtype)
    && all (`elem` declaredParameters) (acceptedRangeParameters range)

-- | The single most specific range that applies to a declared representation.
-- Per RFC 9110 section 12.5.1, when more than one range in the header
-- applies to a representation, the most specific one governs its quality
-- regardless of a less specific range's own quality.
bestMatchingRange :: (Text, Text) -> [(Text, Text)] -> [AcceptedRange] -> Maybe AcceptedRange
bestMatchingRange declaredMediaType declaredParameters ranges =
  case filter (rangeMatchesRepresentation declaredMediaType declaredParameters) ranges of
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

-- | Parameter-aware selection of one declared response content type.
data ApiContentTypeNegotiationResult
  = NoAcceptableContentTypeRepresentation
  | SelectedContentTypeRepresentation ApiContentType
  deriving (Eq, Show)

-- | Negotiate a response representation from a declared,
-- server-preference-ordered list and an optional @Accept@ header. A missing
-- header selects the first declared representation. An explicit header that
-- excludes every declared representation is @406@; otherwise each
-- representation's most specific matching range determines its quality, the
-- highest quality is selected, and ties keep server declaration order.
selectRepresentation :: NonEmpty ApiMediaType -> Maybe Text -> ApiNegotiationResult
selectRepresentation declaredRepresentations maybeAcceptHeader =
  toMediaTypeNegotiationResult $
    selectByRepresentation apiMediaTypeParts (const []) declaredRepresentations maybeAcceptHeader

-- | Negotiate a declared response content type, including its emitted media
-- parameters such as @charset=utf-8@. An @Accept@ media parameter before
-- @q@ must match the selected content type; extensions after @q@ do not.
selectContentTypeRepresentation :: NonEmpty ApiContentType -> Maybe Text -> ApiContentTypeNegotiationResult
selectContentTypeRepresentation declaredContentTypes maybeAcceptHeader =
  toContentTypeNegotiationResult $
    selectByRepresentation
      (apiMediaTypeParts . apiContentTypeMediaType)
      apiContentTypeParameters
      declaredContentTypes
      maybeAcceptHeader

selectByRepresentation ::
  (representation -> (Text, Text)) ->
  (representation -> [(Text, Text)]) ->
  NonEmpty representation ->
  Maybe Text ->
  ApiNegotiationResultFor representation
selectByRepresentation representationParts representationParameters declaredRepresentations maybeAcceptHeader =
  case maybeAcceptHeader of
    Nothing -> SelectedRepresentationFor (NonEmpty.head declaredRepresentations)
    Just headerValue ->
      case acceptableCandidates (parseAcceptHeader headerValue) of
        [] -> NoAcceptableRepresentationFor
        candidates -> SelectedRepresentationFor (fst (foldl1' preferHigherQuality candidates))
  where
    acceptableCandidates ranges =
      [ (representation, acceptedRangeQuality bestRange)
      | representation <- NonEmpty.toList declaredRepresentations,
        Just bestRange <- [bestMatchingRange (representationParts representation) (representationParameters representation) ranges],
        acceptedRangeQuality bestRange > 0
      ]
    preferHigherQuality left right =
      if snd right > snd left then right else left

data ApiNegotiationResultFor representation
  = NoAcceptableRepresentationFor
  | SelectedRepresentationFor representation

toMediaTypeNegotiationResult :: ApiNegotiationResultFor ApiMediaType -> ApiNegotiationResult
toMediaTypeNegotiationResult result =
  case result of
    NoAcceptableRepresentationFor -> NoAcceptableRepresentation
    SelectedRepresentationFor mediaType -> SelectedRepresentation mediaType

toContentTypeNegotiationResult :: ApiNegotiationResultFor ApiContentType -> ApiContentTypeNegotiationResult
toContentTypeNegotiationResult result =
  case result of
    NoAcceptableRepresentationFor -> NoAcceptableContentTypeRepresentation
    SelectedRepresentationFor contentType -> SelectedContentTypeRepresentation contentType
