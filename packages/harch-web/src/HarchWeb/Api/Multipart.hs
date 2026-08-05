{-# LANGUAGE OverloadedStrings #-}

-- | A bounded, incremental @multipart\/form-data@ boundary scanner (RFC 7578,
-- RFC 2046 section 5.1). This module only segments an arbitrarily-chunked
-- byte stream into per-part header blocks and body byte ranges; it does not
-- decode field names, filenames, or apply size limits itself, and it does
-- not yet integrate with a WAI request body. Those are documented future
-- extensions (see @TASKS.md@ item AB).
--
-- The scanner never buffers more of a part's body than the length of the
-- boundary delimiter: once a prefix of the buffered bytes is confirmed not to
-- be part of the next delimiter, it is emitted immediately as a
-- 'MultipartPartBodyChunk' and dropped from the retained state. This is the
-- property that lets a caller stream large file parts without buffering them
-- whole.
module HarchWeb.Api.Multipart
  ( MultipartEvent (..),
    MultipartScanner,
    newMultipartScanner,
    feedMultipartChunk,
    finishMultipartScanner,
    MultipartFieldDisposition (..),
    parseMultipartFieldDisposition,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.Encoding.Error qualified as TextEncodingError

data MultipartEvent
  = -- | A new part began. The payload is its raw header block (each header
    -- line separated by @\\r\\n@, without the trailing blank line);
    -- extracting typed fields such as @Content-Disposition@'s @name@ and
    -- @filename@ is a separate, non-streaming concern.
    MultipartPartStarted ByteString
  | -- | The next chunk of the current part's body. A part may produce any
    -- number of these, in order, with no size limit applied by this module.
    MultipartPartBodyChunk ByteString
  | -- | The current part's body is complete.
    MultipartPartEnded
  | -- | The closing boundary was reached; no further parts follow.
    MultipartFinished
  | -- | The stream did not follow the boundary grammar; scanning has
    -- stopped and no further events will be produced.
    MultipartMalformed
  deriving (Eq, Show)

data MultipartPhase
  = AwaitingFirstBoundary
  | StreamingBody
  | -- | The buffer starts with the just-matched delimiter's trailing bytes
    -- (everything after @--boundary@); classify them on the next step. A
    -- part's headers are always attempted from here (or from
    -- 'AwaitingFirstBoundary' for the very first part) in one go: if they
    -- are not yet fully buffered, this phase is retried unchanged on the
    -- next chunk rather than being tracked as a separate phase, since
    -- headers are small and re-scanning them is cheap.
    AtDelimiterTail
  | ScannerFinished

data MultipartScanner = MultipartScanner
  { scannerLeadingDelimiter :: !ByteString,
    scannerBodyDelimiter :: !ByteString,
    scannerBuffer :: !ByteString,
    scannerPhase :: !MultipartPhase
  }

-- | Start a scanner for the given multipart boundary token (the value of the
-- @Content-Type@ header's @boundary@ parameter, without the leading @--@).
newMultipartScanner :: ByteString -> MultipartScanner
newMultipartScanner boundary =
  MultipartScanner
    { scannerLeadingDelimiter = "--" <> boundary,
      scannerBodyDelimiter = "\r\n--" <> boundary,
      scannerBuffer = ByteString.empty,
      scannerPhase = AwaitingFirstBoundary
    }

-- | Feed the next chunk of request bytes, in order, producing every event
-- the new data makes determinable. Bytes that might still be an incomplete
-- prefix of the next boundary delimiter are retained internally rather than
-- emitted, so a chunk boundary landing mid-delimiter never loses data or
-- misidentifies a part.
feedMultipartChunk :: MultipartScanner -> ByteString -> ([MultipartEvent], MultipartScanner)
feedMultipartChunk scanner chunk =
  drainScanner scanner {scannerBuffer = scannerBuffer scanner <> chunk}

-- | Signal that the request body has ended. Any part still open when this is
-- called was truncated: the caller can detect that by checking whether the
-- last event from the whole scan was 'MultipartFinished'.
finishMultipartScanner :: MultipartScanner -> [MultipartEvent]
finishMultipartScanner scanner =
  case scannerPhase scanner of
    StreamingBody
      | not (ByteString.null (scannerBuffer scanner)) ->
          [MultipartPartBodyChunk (scannerBuffer scanner)]
    _ -> []

drainScanner :: MultipartScanner -> ([MultipartEvent], MultipartScanner)
drainScanner scanner =
  case stepScanner scanner of
    Nothing -> ([], scanner)
    Just (event, nextScanner) ->
      let (laterEvents, finalScanner) = drainScanner nextScanner
       in (event : laterEvents, finalScanner)

stepScanner :: MultipartScanner -> Maybe (MultipartEvent, MultipartScanner)
stepScanner scanner =
  case scannerPhase scanner of
    ScannerFinished -> Nothing
    AwaitingFirstBoundary -> stepAwaitingFirstBoundary scanner
    StreamingBody -> stepStreamingBody scanner
    AtDelimiterTail -> advancePastDelimiter scanner

stepAwaitingFirstBoundary :: MultipartScanner -> Maybe (MultipartEvent, MultipartScanner)
stepAwaitingFirstBoundary scanner =
  let marker = scannerLeadingDelimiter scanner
      (_preamble, atMarker) = ByteString.breakSubstring marker (scannerBuffer scanner)
   in if ByteString.null atMarker
        then Nothing
        else
          advancePastDelimiter
            scanner {scannerBuffer = ByteString.drop (ByteString.length marker) atMarker}

stepStreamingBody :: MultipartScanner -> Maybe (MultipartEvent, MultipartScanner)
stepStreamingBody scanner =
  let delimiter = scannerBodyDelimiter scanner
      buffer = scannerBuffer scanner
      (beforeDelimiter, atDelimiter) = ByteString.breakSubstring delimiter buffer
   in if not (ByteString.null atDelimiter)
        then
          if not (ByteString.null beforeDelimiter)
            then Just (MultipartPartBodyChunk beforeDelimiter, scanner {scannerBuffer = atDelimiter})
            else
              Just
                ( MultipartPartEnded,
                  scanner
                    { scannerBuffer = ByteString.drop (ByteString.length delimiter) atDelimiter,
                      scannerPhase = AtDelimiterTail
                    }
                )
        else
          let retainedLength = safeSuffixLength delimiter buffer
              safeLength = ByteString.length buffer - retainedLength
           in if safeLength > 0
                then
                  Just
                    ( MultipartPartBodyChunk (ByteString.take safeLength buffer),
                      scanner {scannerBuffer = ByteString.drop safeLength buffer}
                    )
                else Nothing

stepAwaitingHeaders :: MultipartScanner -> Maybe (MultipartEvent, MultipartScanner)
stepAwaitingHeaders scanner =
  let (headerBlock, atBlankLine) = ByteString.breakSubstring "\r\n\r\n" (scannerBuffer scanner)
   in if ByteString.null atBlankLine
        then Nothing
        else
          Just
            ( MultipartPartStarted headerBlock,
              scanner
                { scannerBuffer = ByteString.drop 4 atBlankLine,
                  scannerPhase = StreamingBody
                }
            )

-- | The buffer starts immediately after a consumed delimiter marker (leading
-- or mid-body). Classify what follows once enough bytes have arrived: @--@
-- closes the stream, a CRLF begins the next part's headers, and anything
-- else is a boundary-grammar violation.
advancePastDelimiter :: MultipartScanner -> Maybe (MultipartEvent, MultipartScanner)
advancePastDelimiter scanner
  | "--" `ByteString.isPrefixOf` afterMarker =
      Just (MultipartFinished, scanner {scannerPhase = ScannerFinished})
  | "\r\n" `ByteString.isPrefixOf` afterMarker =
      stepAwaitingHeaders scanner {scannerBuffer = ByteString.drop 2 afterMarker}
  | ByteString.length afterMarker < 2 = Nothing
  | otherwise =
      Just (MultipartMalformed, scanner {scannerPhase = ScannerFinished})
  where
    afterMarker = scannerBuffer scanner

-- | How many trailing bytes of @haystack@ must be retained because they
-- could be the start of @needle@ if more bytes arrive. Assumes @needle@ does
-- not occur as a complete substring of @haystack@.
safeSuffixLength :: ByteString -> ByteString -> Int
safeSuffixLength needle haystack =
  go (min (ByteString.length needle - 1) (ByteString.length haystack))
  where
    go 0 = 0
    go candidateLength
      | ByteString.drop (ByteString.length haystack - candidateLength) haystack
          == ByteString.take candidateLength needle =
          candidateLength
      | otherwise = go (candidateLength - 1)

-- | The @Content-Disposition@ @name@ and @filename@ parameters for a part,
-- extracted from the raw header block a 'MultipartPartStarted' event
-- carries. Extended (@filename*=@, RFC 5987/6266) and bare-token parameter
-- values are documented future extensions; only the common quoted-string
-- form is supported.
data MultipartFieldDisposition = MultipartFieldDisposition
  { multipartFieldName :: Maybe Text,
    multipartFieldFilename :: Maybe Text
  }
  deriving (Eq, Show)

-- | Parse a part's @Content-Disposition@ header, if present, from its raw
-- header block.
parseMultipartFieldDisposition :: ByteString -> Maybe MultipartFieldDisposition
parseMultipartFieldDisposition headerBlock = do
  dispositionValue <- lookup "content-disposition" (multipartHeaderFields headerBlock)
  let parameters = parseDispositionParameters dispositionValue
  pure
    MultipartFieldDisposition
      { multipartFieldName = lookup "name" parameters,
        multipartFieldFilename = lookup "filename" parameters
      }

multipartHeaderFields :: ByteString -> [(Text, Text)]
multipartHeaderFields headerBlock =
  Maybe.mapMaybe parseHeaderLine (splitOnCrlf headerBlock)

parseHeaderLine :: ByteString -> Maybe (Text, Text)
parseHeaderLine line =
  case ByteString.breakSubstring ":" line of
    (_, valueWithColon) | ByteString.null valueWithColon -> Nothing
    (nameBytes, valueWithColon) ->
      Just
        ( Text.toLower (Text.strip (decodeHeaderBytes nameBytes)),
          Text.strip (decodeHeaderBytes (ByteString.drop 1 valueWithColon))
        )

splitOnCrlf :: ByteString -> [ByteString]
splitOnCrlf bytes =
  case ByteString.breakSubstring "\r\n" bytes of
    (line, rest)
      | ByteString.null rest -> [line]
      | otherwise -> line : splitOnCrlf (ByteString.drop 2 rest)

-- Kept eta-expanded (not point-free) so HPC ticks the decode call on every
-- invocation rather than treating it as a once-shared CAF reference.
{-# ANN decodeHeaderBytes ("HLint: ignore Eta reduce" :: String) #-}
decodeHeaderBytes :: ByteString -> Text
decodeHeaderBytes bytes = TextEncoding.decodeUtf8With TextEncodingError.lenientDecode bytes

-- | Split a @Content-Disposition@ value's @;@-separated parameters,
-- respecting quoted-string boundaries so a semicolon inside a quoted
-- @filename@ does not end the parameter early.
parseDispositionParameters :: Text -> [(Text, Text)]
parseDispositionParameters value =
  Maybe.mapMaybe parseParameter (splitParameters value)

splitParameters :: Text -> [Text]
splitParameters = go False Text.empty
  where
    go !inQuotes current remaining =
      case Text.uncons remaining of
        Nothing -> [current]
        Just ('"', rest) -> go (not inQuotes) (Text.snoc current '"') rest
        Just ('\\', rest) | inQuotes ->
          case Text.uncons rest of
            Just (escaped, rest') -> go inQuotes (Text.snoc (Text.snoc current '\\') escaped) rest'
            Nothing -> go inQuotes (Text.snoc current '\\') rest
        Just (';', rest) | not inQuotes -> current : go False Text.empty rest
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
