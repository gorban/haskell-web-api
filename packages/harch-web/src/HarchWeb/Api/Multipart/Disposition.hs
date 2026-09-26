{-# LANGUAGE OverloadedStrings #-}

-- | Parsing for the per-part @Content-Disposition@ metadata in a multipart
-- body. This is deliberately pure: the upload driver decides what storage and
-- lifetime policy to apply after this module identifies the field.
module HarchWeb.Api.Multipart.Disposition
  ( MultipartDispositionParse (..),
    MultipartFieldDisposition (..),
    parseMultipartFieldDisposition,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.Encoding.Error qualified as TextEncodingError

-- | The @Content-Disposition@ @name@ and @filename@ parameters for a part,
-- extracted from the raw header block a multipart part-start event carries.
-- Extended (@filename*=@, RFC 5987/6266) and bare-token parameter values are
-- documented future extensions; only the common quoted-string form is
-- supported.
data MultipartFieldDisposition = MultipartFieldDisposition
  { multipartFieldName :: Maybe Text,
    multipartFieldFilename :: Maybe Text
  }
  deriving (Eq, Show)

-- | How a part's header block yields (or fails to yield) a
-- @Content-Disposition@. The line must occur exactly once: first-wins
-- parsing of a repeated line would let different multipart consumers
-- disagree about which untrusted value owns a part, while collapsing a
-- repeat into 'MultipartDispositionAbsent' would misreport ambiguity as a
-- missing header.
data MultipartDispositionParse
  = MultipartDispositionAbsent
  | MultipartDispositionRepeated
  | MultipartDispositionInvalid
  | MultipartDispositionParsed MultipartFieldDisposition
  deriving (Eq, Show)

-- | Parse a part's @Content-Disposition@ header, if present, from its raw
-- header block. RFC 7578 admits only @form-data@ dispositions. A parameter
-- must occur at most once: accepting an arbitrary duplicate would let
-- different multipart consumers disagree about which untrusted value owns a
-- part. Missing @name@ remains represented explicitly for the consumer to
-- report as its existing 'MultipartMissingDisposition' failure.
--
-- Decision record (GR-2): 'MultipartDispositionRepeated' is consumed at the
-- request boundary as 'MultipartMalformedBody' rather than a new error
-- constructor: a header block carrying the same untrusted field twice is a
-- malformed part-header block, which is exactly what that constructor
-- already names, and keeping the rail small preserves its stability.
parseMultipartFieldDisposition :: ByteString -> MultipartDispositionParse
parseMultipartFieldDisposition headerBlock =
  case filter ((== "content-disposition") . fst) (multipartHeaderFields headerBlock) of
    [] -> MultipartDispositionAbsent
    [(_, dispositionValue)] -> parseDispositionLine dispositionValue
    _ -> MultipartDispositionRepeated

parseDispositionLine :: Text -> MultipartDispositionParse
parseDispositionLine dispositionValue =
  let (dispositionType, parameters) = parseDispositionParameters dispositionValue
   in if Text.toCaseFold (Text.strip dispositionType) /= "form-data"
        then MultipartDispositionInvalid
        else case (atMostOneParameterValue "name" parameters, atMostOneParameterValue "filename" parameters) of
          (Just name, Just filename) ->
            MultipartDispositionParsed
              MultipartFieldDisposition
                { multipartFieldName = name,
                  multipartFieldFilename = filename
                }
          _ -> MultipartDispositionInvalid

multipartHeaderFields :: ByteString -> [(Text, Text)]
multipartHeaderFields headerBlock =
  Maybe.mapMaybe parseHeaderLine (splitOnCrlf headerBlock)

parseHeaderLine :: ByteString -> Maybe (Text, Text)
parseHeaderLine line =
  case ByteString.breakSubstring ":" line of
    (_, valueWithColon) | ByteString.null valueWithColon -> Nothing
    (nameBytes, valueWithColon) ->
      Just
        ( Text.toLower (Text.strip (decodeLeniently nameBytes)),
          Text.strip (decodeLeniently (ByteString.drop 1 valueWithColon))
        )

splitOnCrlf :: ByteString -> [ByteString]
splitOnCrlf bytes =
  case ByteString.breakSubstring "\r\n" bytes of
    (line, rest)
      | ByteString.null rest -> [line]
      | otherwise -> line : splitOnCrlf (ByteString.drop 2 rest)

decodeLeniently :: ByteString -> Text
decodeLeniently = TextEncoding.decodeUtf8With TextEncodingError.lenientDecode

-- | Split a @Content-Disposition@ value's @;@-separated parameters,
-- respecting quoted-string boundaries so a semicolon inside a quoted
-- @filename@ does not end the parameter early.
parseDispositionParameters :: Text -> (Text, [(Text, Text)])
parseDispositionParameters value =
  case splitParameters value of
    dispositionType :| parameterSegments ->
      (dispositionType, Maybe.mapMaybe parseParameter parameterSegments)

atMostOneParameterValue :: Text -> [(Text, Text)] -> Maybe (Maybe Text)
atMostOneParameterValue parameterName parameters =
  case [value | (candidateName, value) <- parameters, candidateName == parameterName] of
    [] -> Just Nothing
    [value] -> Just (Just value)
    _ -> Nothing

splitParameters :: Text -> NonEmpty Text
splitParameters = go False Text.empty
  where
    go !inQuotes current remaining =
      case Text.uncons remaining of
        Nothing -> current :| []
        Just ('"', rest) -> go (not inQuotes) (Text.snoc current '"') rest
        Just ('\\', rest) | inQuotes ->
          case Text.uncons rest of
            Just (escaped, rest') -> go inQuotes (Text.snoc (Text.snoc current '\\') escaped) rest'
            Nothing -> go inQuotes (Text.snoc current '\\') rest
        Just (';', rest) | not inQuotes ->
          case go False Text.empty rest of
            nextSegment :| remainingSegments -> current :| (nextSegment : remainingSegments)
        Just (nextChar, rest) -> go inQuotes (Text.snoc current nextChar) rest

parseParameter :: Text -> Maybe (Text, Text)
parseParameter segment =
  case Text.breakOn "=" (Text.strip segment) of
    (name, valueWithEquals)
      | Text.null name || not ("=" `Text.isPrefixOf` valueWithEquals) -> Nothing
      | otherwise -> Just (Text.toLower name, unquoteParameterValue (Text.strip (Text.drop 1 valueWithEquals)))

unquoteParameterValue :: Text -> Text
unquoteParameterValue value
  | Text.length value >= 2,
    Text.head value == '"',
    Text.last value == '"' =
      unescapeQuotedPairs (Text.init (Text.tail value))
  | otherwise = value

unescapeQuotedPairs :: Text -> Text
unescapeQuotedPairs = Text.pack . go . Text.unpack
  where
    go ('\\' : escaped : rest) = escaped : go rest
    go (character : rest) = character : go rest
    go [] = []
