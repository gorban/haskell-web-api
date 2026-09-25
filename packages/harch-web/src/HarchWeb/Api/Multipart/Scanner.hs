{-# LANGUAGE OverloadedStrings #-}

-- | Pure, incremental multipart boundary scanning. This module owns only
-- byte-level framing and its retained-byte limits; interpreting headers,
-- fields, and storage belongs to the consumer layer.
module HarchWeb.Api.Multipart.Scanner
  ( MultipartEvent (..),
    MultipartScanner,
    maxTransportPadding,
    newMultipartScanner,
    newBoundedMultipartScanner,
    feedMultipartChunk,
    finishMultipartScanner,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString

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
  | -- | The preamble before the first boundary exceeded its configured
    -- retained-byte limit. Scanning stops without retaining more request
    -- bytes.
    MultipartPreambleLimitExceeded
  | -- | A part's raw header block exceeded its configured retained-byte
    -- limit. Scanning stops without retaining more request bytes.
    MultipartPartHeaderLimitExceeded
  deriving (Eq, Show)

data MultipartPhase
  = AwaitingFirstBoundary
  | StreamingBody
  | AtDelimiterTail
  | ScannerFinished

data MultipartScanner = MultipartScanner
  { scannerLeadingDelimiter :: !ByteString,
    scannerBodyDelimiter :: !ByteString,
    scannerBuffer :: !ByteString,
    scannerPreambleBytes :: !Int,
    scannerPreambleLimit :: !(Maybe Int),
    scannerPartHeaderLimit :: !(Maybe Int),
    scannerPreambleLimitExceeded :: !Bool,
    scannerPhase :: !MultipartPhase
  }

-- | Start a scanner for the given multipart boundary token (the value of the
-- @Content-Type@ header's @boundary@ parameter, without the leading @--@).
newMultipartScanner :: ByteString -> MultipartScanner
newMultipartScanner = newMultipartScannerWithLimits Nothing Nothing

newBoundedMultipartScanner :: Int -> Int -> ByteString -> MultipartScanner
newBoundedMultipartScanner maximumPreambleBytes maximumPartHeaderBytes =
  newMultipartScannerWithLimits (Just maximumPreambleBytes) (Just maximumPartHeaderBytes)

newMultipartScannerWithLimits :: Maybe Int -> Maybe Int -> ByteString -> MultipartScanner
newMultipartScannerWithLimits maybeMaximumPreambleBytes maybeMaximumPartHeaderBytes boundary =
  MultipartScanner
    { scannerLeadingDelimiter = "--" <> boundary,
      scannerBodyDelimiter = "\r\n--" <> boundary,
      scannerBuffer = ByteString.empty,
      scannerPreambleBytes = 0,
      scannerPreambleLimit = maybeMaximumPreambleBytes,
      scannerPartHeaderLimit = maybeMaximumPartHeaderBytes,
      scannerPreambleLimitExceeded = False,
      scannerPhase = AwaitingFirstBoundary
    }

-- | Feed the next chunk of request bytes, in order, producing every event
-- the new data makes determinable. Bytes that might still be an incomplete
-- prefix of the next boundary delimiter are retained internally rather than
-- emitted, so a chunk boundary landing mid-delimiter never loses data or
-- misidentifies a part.
feedMultipartChunk :: MultipartScanner -> ByteString -> ([MultipartEvent], MultipartScanner)
feedMultipartChunk scanner chunk =
  drainScanner (discardSafePreamble scanner {scannerBuffer = scannerBuffer scanner <> chunk})

discardSafePreamble :: MultipartScanner -> MultipartScanner
discardSafePreamble scanner =
  case scannerPhase scanner of
    AwaitingFirstBoundary
      | ByteString.null atMarker ->
          scanner
            { scannerBuffer = ByteString.drop discardedLength buffer,
              scannerPreambleBytes = nextPreambleBytes,
              scannerPreambleLimitExceeded = exceedsScannerLimit (scannerPreambleLimit scanner) nextPreambleBytes
            }
    _ -> scanner
  where
    buffer = scannerBuffer scanner
    marker = scannerLeadingDelimiter scanner
    (_preamble, atMarker) = ByteString.breakSubstring marker buffer
    discardedLength = ByteString.length buffer - safeSuffixLength marker buffer
    nextPreambleBytes = scannerPreambleBytes scanner + discardedLength

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
  if scannerPreambleLimitExceeded scanner
    then
      Just
        ( MultipartPreambleLimitExceeded,
          scanner {scannerPreambleLimitExceeded = False, scannerPhase = ScannerFinished}
        )
    else case scannerPhase scanner of
      ScannerFinished -> Nothing
      AwaitingFirstBoundary -> stepAwaitingFirstBoundary scanner
      StreamingBody -> stepStreamingBody scanner
      AtDelimiterTail -> advancePastDelimiter scanner

stepAwaitingFirstBoundary :: MultipartScanner -> Maybe (MultipartEvent, MultipartScanner)
stepAwaitingFirstBoundary scanner =
  let marker = scannerLeadingDelimiter scanner
      (preamble, atMarker) = ByteString.breakSubstring marker (scannerBuffer scanner)
      nextPreambleBytes = scannerPreambleBytes scanner + ByteString.length preamble
   in if ByteString.null atMarker
        then Nothing
        else
          if exceedsScannerLimit (scannerPreambleLimit scanner) nextPreambleBytes
            then Just (MultipartPreambleLimitExceeded, scanner {scannerPhase = ScannerFinished})
            else
              advancePastDelimiter
                scanner
                  { scannerBuffer = ByteString.drop (ByteString.length marker) atMarker,
                    scannerPreambleBytes = nextPreambleBytes
                  }

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
        then rejectOversizedHeader scanner (ByteString.length (scannerBuffer scanner))
        else case rejectOversizedHeader scanner (ByteString.length headerBlock) of
          Just result -> Just result
          Nothing ->
            Just
              ( MultipartPartStarted headerBlock,
                scanner
                  { scannerBuffer = ByteString.drop 4 atBlankLine,
                    scannerPhase = StreamingBody
                  }
              )

rejectOversizedHeader :: MultipartScanner -> Int -> Maybe (MultipartEvent, MultipartScanner)
rejectOversizedHeader scanner headerBytes
  | exceedsScannerLimit (scannerPartHeaderLimit scanner) headerBytes =
      Just (MultipartPartHeaderLimitExceeded, scanner {scannerPhase = ScannerFinished})
  | otherwise = Nothing

-- | RFC 2046 transport padding: the grammar admits @transport-padding := *LWSP@
-- between a boundary token and its terminating CRLF (both for
-- @dash-boundary transport-padding CRLF@ and @close-delimiter transport-padding@),
-- so a delimiter line like @\\r\\n--BOUNDARY123 \\r\\n@ is valid. The accepted
-- run is capped so an endless stream of spaces cannot pin the scanner's
-- retained buffer; a longer run is a grammar violation.
maxTransportPadding :: Int
maxTransportPadding = 8

-- | The buffer starts immediately after a consumed delimiter marker (leading
-- or mid-body). Consume optional transport padding first, then classify what
-- follows once enough bytes have arrived: @--@ closes the stream (tolerating
-- trailing bytes after the close token, as before), a CRLF begins the next
-- part's headers, and anything else is a boundary-grammar violation. Partial
-- padding or one-byte trailers keep returning the need-more-bytes result
-- instead of deciding early.
advancePastDelimiter :: MultipartScanner -> Maybe (MultipartEvent, MultipartScanner)
advancePastDelimiter scanner =
  case ByteString.span isTransportPaddingByte afterMarker of
    (padding, rest)
      | ByteString.length padding > maxTransportPadding ->
          Just (MultipartMalformed, scanner {scannerPhase = ScannerFinished})
      | "--" `ByteString.isPrefixOf` rest ->
          Just (MultipartFinished, scanner {scannerPhase = ScannerFinished})
      | "\r\n" `ByteString.isPrefixOf` rest ->
          stepAwaitingHeaders scanner {scannerBuffer = ByteString.drop 2 rest}
      | ByteString.length rest < 2 -> Nothing
      | otherwise ->
          Just (MultipartMalformed, scanner {scannerPhase = ScannerFinished})
  where
    afterMarker = scannerBuffer scanner
    isTransportPaddingByte byte = byte == 32 || byte == 9

exceedsScannerLimit :: Maybe Int -> Int -> Bool
exceedsScannerLimit maybeMaximumBytes byteCount =
  maybe False (< byteCount) maybeMaximumBytes

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
