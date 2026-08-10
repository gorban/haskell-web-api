{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | A bounded, incremental @multipart\/form-data@ consumer (RFC 7578, RFC
-- 2046 section 5.1): a boundary scanner that segments an arbitrarily-chunked
-- byte stream into per-part header blocks and body byte ranges, plus a
-- driver that turns those events into complete parts against a WAI request
-- body, keeping small field values in memory and writing file uploads through
-- an application-selected storage adapter. RFC 5987\/6266 extended @filename*=@ parameters are not
-- supported, only the common quoted-string form.
--
-- The scanner never buffers more of a part's body than the length of the
-- boundary delimiter: once a prefix of the buffered bytes is confirmed not to
-- be part of the next delimiter, it is emitted immediately as a
-- 'MultipartPartBodyChunk' and dropped from the retained state. This is the
-- property that lets a caller stream large file parts without buffering them
-- whole.
module HarchWeb.Api.Multipart
  ( MultipartEvent (..),
    MultipartScannerLimit (..),
    MultipartScanner,
    newMultipartScanner,
    feedMultipartChunk,
    finishMultipartScanner,
    MultipartFieldDisposition (..),
    parseMultipartFieldDisposition,
    MultipartLimits (..),
    defaultMultipartLimits,
    MultipartPart,
    MultipartPartWith (..),
    MultipartStorage,
    MultipartStagedUpload,
    multipartStorage,
    multipartStagedUpload,
    InMemoryUpload,
    inMemoryMultipartStorage,
    inMemoryUploadBytes,
    MultipartConsumeError (..),
    consumeMultipartBody,
    consumeMultipartBodyWith,
    consumeMultipartRequestBody,
    consumeMultipartRequestBodyWith,
  )
where

import Control.Exception qualified as Exception
import Control.Monad (when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Foldable (for_, traverse_)
import Data.IORef qualified as IORef
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.Encoding.Error qualified as TextEncodingError
import HarchWeb.Api.Multipart.Storage
import HarchWeb.Api.Multipart.Storage.Internal qualified as MultipartStorage
import Network.Wai qualified as Wai

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
  | -- | Parser state exceeded a configured retained-byte limit. Scanning
    -- stops without retaining more request bytes.
    MultipartScannerLimitExceeded MultipartScannerLimit
  deriving (Eq, Show)

data MultipartScannerLimit
  = MultipartPreambleLimitExceeded
  | MultipartPartHeaderLimitExceeded
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
    scannerPreambleBytes :: !Int,
    scannerPreambleLimit :: !(Maybe Int),
    scannerPartHeaderLimit :: !(Maybe Int),
    scannerLimitFailure :: !(Maybe MultipartScannerLimit),
    scannerPhase :: !MultipartPhase
  }

-- | Start a scanner for the given multipart boundary token (the value of the
-- @Content-Type@ header's @boundary@ parameter, without the leading @--@).
newMultipartScanner :: ByteString -> MultipartScanner
newMultipartScanner = newMultipartScannerWithLimits Nothing Nothing

newBoundedMultipartScanner :: MultipartLimits -> ByteString -> MultipartScanner
newBoundedMultipartScanner limits =
  newMultipartScannerWithLimits
    (Just (multipartLimitsMaxPreambleBytes limits))
    (Just (multipartLimitsMaxPartHeaderBytes limits))

newMultipartScannerWithLimits :: Maybe Int -> Maybe Int -> ByteString -> MultipartScanner
newMultipartScannerWithLimits maybeMaximumPreambleBytes maybeMaximumPartHeaderBytes boundary =
  MultipartScanner
    { scannerLeadingDelimiter = "--" <> boundary,
      scannerBodyDelimiter = "\r\n--" <> boundary,
      scannerBuffer = ByteString.empty,
      scannerPreambleBytes = 0,
      scannerPreambleLimit = maybeMaximumPreambleBytes,
      scannerPartHeaderLimit = maybeMaximumPartHeaderBytes,
      scannerLimitFailure = Nothing,
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
              scannerLimitFailure =
                if exceedsScannerLimit (scannerPreambleLimit scanner) nextPreambleBytes
                  then Just MultipartPreambleLimitExceeded
                  else Nothing
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
  case scannerLimitFailure scanner of
    Just limitFailure ->
      Just
        ( MultipartScannerLimitExceeded limitFailure,
          scanner {scannerLimitFailure = Nothing, scannerPhase = ScannerFinished}
        )
    Nothing ->
      case scannerPhase scanner of
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
            then Just (MultipartScannerLimitExceeded MultipartPreambleLimitExceeded, scanner {scannerPhase = ScannerFinished})
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
      Just (MultipartScannerLimitExceeded MultipartPartHeaderLimitExceeded, scanner {scannerPhase = ScannerFinished})
  | otherwise = Nothing

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
        ( Text.toLower (Text.strip (decodeLeniently nameBytes)),
          Text.strip (decodeLeniently (ByteString.drop 1 valueWithColon))
        )

splitOnCrlf :: ByteString -> [ByteString]
splitOnCrlf bytes =
  case ByteString.breakSubstring "\r\n" bytes of
    (line, rest)
      | ByteString.null rest -> [line]
      | otherwise -> line : splitOnCrlf (ByteString.drop 2 rest)

-- Kept eta-expanded (not point-free), and reused for both header and field
-- body bytes, so HPC ticks the decode call on every invocation rather than
-- treating it as a once-shared CAF reference.
{-# ANN decodeLeniently ("HLint: ignore Eta reduce" :: String) #-}
decodeLeniently :: ByteString -> Text
decodeLeniently bytes = TextEncoding.decodeUtf8With TextEncodingError.lenientDecode bytes

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

-- | Bounds applied while consuming a multipart body: its total streamed byte
-- count, how much preamble and each part's headers the parser may retain, how
-- large a field value may grow before it is rejected, how large a staged file
-- upload may grow before it is rejected, and how many parts a single body may
-- declare.
data MultipartLimits = MultipartLimits
  { multipartLimitsMaxBodyBytes :: Int,
    multipartLimitsMaxPreambleBytes :: Int,
    multipartLimitsMaxPartHeaderBytes :: Int,
    multipartLimitsMaxFieldBytes :: Int,
    multipartLimitsMaxFileBytes :: Int,
    multipartLimitsMaxParts :: Int
  }
  deriving (Eq, Show)

-- | 32 MiB body, 8 KiB preamble, 16 KiB per-part headers, 1 MiB field
-- values, 25 MiB file uploads, 100 parts.
defaultMultipartLimits :: MultipartLimits
defaultMultipartLimits =
  MultipartLimits
    { multipartLimitsMaxBodyBytes = 32 * 1024 * 1024,
      multipartLimitsMaxPreambleBytes = 8 * 1024,
      multipartLimitsMaxPartHeaderBytes = 16 * 1024,
      multipartLimitsMaxFieldBytes = 1024 * 1024,
      multipartLimitsMaxFileBytes = 25 * 1024 * 1024,
      multipartLimitsMaxParts = 100
    }

-- | One fully-consumed part: either a plain field's decoded value, kept in
-- memory, or a file upload represented by the selected storage adapter's
-- completed value and byte count.
data MultipartPartWith stored
  = MultipartFieldPart Text Text
  | MultipartFilePart Text Text stored Int
  deriving (Eq, Show)

-- | The out-of-the-box multipart representation uses bounded in-memory file
-- storage. Applications selecting another adapter use 'MultipartPartWith'
-- at their explicit storage boundary.
type MultipartPart = MultipartPartWith InMemoryUpload

data MultipartConsumeError
  = -- | The streamed body exceeded 'multipartLimitsMaxBodyBytes'.
    MultipartBodyTooLarge
  | -- | The bytes before the first boundary exceeded
    -- 'multipartLimitsMaxPreambleBytes'.
    MultipartPreambleTooLarge
  | -- | A part's raw header block exceeded
    -- 'multipartLimitsMaxPartHeaderBytes'.
    MultipartPartHeadersTooLarge
  | -- | The body declared more parts than 'multipartLimitsMaxParts' allows.
    MultipartTooManyParts
  | -- | A part had no @Content-Disposition@ header, or one without a @name@
    -- parameter.
    MultipartMissingDisposition
  | -- | A field value exceeded 'multipartLimitsMaxFieldBytes'.
    MultipartFieldTooLarge Text
  | -- | A file upload exceeded 'multipartLimitsMaxFileBytes'.
    MultipartFileTooLarge Text
  | -- | The scanner reported a malformed delimiter, or a body event arrived
    -- without a part currently open.
    MultipartMalformedBody
  | -- | The body ended before the scanner reported 'MultipartFinished'.
    MultipartTruncatedBody
  deriving (Eq, Show)

data PartAccumulator stored
  = FieldAccumulator Text ByteString
  | FileAccumulator Text Text (MultipartStagedUpload stored) Int

discardActiveMultipartUpload :: IORef.IORef (Maybe (MultipartStagedUpload stored)) -> IO ()
discardActiveMultipartUpload activeUploadReference = do
  maybeActiveUpload <- IORef.atomicModifyIORef' activeUploadReference (Nothing,)
  for_ maybeActiveUpload MultipartStorage.discardMultipartUpload

-- | Incrementally consume a @multipart\/form-data@ body: drive
-- 'MultipartScanner' from repeated calls to @readChunk@ (an empty
-- 'ByteString' signals end of body, matching 'Network.Wai.getRequestBodyChunk'),
-- decode plain field values, and append file uploads (parts with a
-- @filename@ parameter) through an explicit storage adapter. Bounded by
-- @limits@; see 'MultipartConsumeError' for the ways this can fail.
consumeMultipartBody ::
  MultipartStorage stored ->
  MultipartLimits ->
  -- | The boundary token, without its leading @--@.
  ByteString ->
  -- | Reads the next body chunk; an empty result signals end of body.
  IO ByteString ->
  IO (Either MultipartConsumeError [MultipartPartWith stored])
consumeMultipartBody storage limits boundary readChunk = Exception.mask $ \restore -> do
  completedPartsReference <- IORef.newIORef []
  let discardCompletedUploads = do
        completedParts <- IORef.readIORef completedPartsReference
        traverse_ discardCompletedUpload (completedUploads completedParts)
  result <-
    restore
      ( consumeMultipartBodyWith storage limits boundary readChunk $ \part -> do
          IORef.modifyIORef' completedPartsReference (part :)
          pure (Right ())
      )
      `Exception.onException` discardCompletedUploads
  case result of
    Left consumeError -> do
      discardCompletedUploads
      pure (Left consumeError)
    Right () -> Right . reverse <$> IORef.readIORef completedPartsReference
  where
    completedUploads parts = [storedUpload | MultipartFilePart _ _ storedUpload _ <- parts]

    discardCompletedUpload storedUpload =
      for_ (MultipartStorage.discardCompletedMultipartUpload storage) ($ storedUpload)

-- | Like 'consumeMultipartBody', but @onPart@ runs as soon as each part
-- finishes, before any later part (including a later file part) is read.
-- Returning @Left@ from @onPart@ aborts the body with that error
-- immediately -- e.g. a caller can reject the whole body on an invalid CSRF
-- field without ever placing a later file part in storage, satisfying RFC
-- 7578 consumers that must not let a token arriving after a file part
-- authorize an already-committed write. Cleanup and promotion for an upload
-- completed before a later abort are the storage lifecycle work tracked by
-- AD, never an implicit raw-file-path policy.
consumeMultipartBodyWith ::
  MultipartStorage stored ->
  MultipartLimits ->
  ByteString ->
  IO ByteString ->
  (MultipartPartWith stored -> IO (Either MultipartConsumeError ())) ->
  IO (Either MultipartConsumeError ())
consumeMultipartBodyWith storage limits boundary readChunk onPart =
  Exception.mask $ \restore -> do
    activeUploadReference <- IORef.newIORef Nothing
    let discardActiveUpload = discardActiveMultipartUpload activeUploadReference
    result <-
      restore (consumeMultipartBodyWithActive activeUploadReference storage limits boundary readChunk onPart)
        `Exception.onException` discardActiveUpload
    case result of
      Left consumeError -> do
        discardActiveUpload
        pure (Left consumeError)
      Right () -> pure (Right ())

consumeMultipartBodyWithActive ::
  IORef.IORef (Maybe (MultipartStagedUpload stored)) ->
  MultipartStorage stored ->
  MultipartLimits ->
  ByteString ->
  IO ByteString ->
  (MultipartPartWith stored -> IO (Either MultipartConsumeError ())) ->
  IO (Either MultipartConsumeError ())
consumeMultipartBodyWithActive activeUploadReference storage limits boundary readChunk onPart =
  let consumer =
        MultipartConsumer
          { multipartConsumerStorage = storage,
            multipartConsumerLimits = limits,
            multipartConsumerActiveUploadReference = activeUploadReference,
            multipartConsumerReadChunk = readChunk,
            multipartConsumerOnPart = onPart
          }
   in runExceptT
        ( driveMultipartConsumer
            consumer
            (newBoundedMultipartScanner limits boundary)
            Nothing
            0
            0
        )

data MultipartConsumer stored = MultipartConsumer
  { multipartConsumerStorage :: MultipartStorage stored,
    multipartConsumerLimits :: MultipartLimits,
    multipartConsumerActiveUploadReference :: IORef.IORef (Maybe (MultipartStagedUpload stored)),
    multipartConsumerReadChunk :: IO ByteString,
    multipartConsumerOnPart :: MultipartPartWith stored -> IO (Either MultipartConsumeError ())
  }

data MultipartTransition stored
  = ContinueMultipartConsumption (Maybe (PartAccumulator stored)) Int
  | FinishMultipartConsumption

driveMultipartConsumer ::
  MultipartConsumer stored ->
  MultipartScanner ->
  Maybe (PartAccumulator stored) ->
  Int ->
  Int ->
  ExceptT MultipartConsumeError IO ()
driveMultipartConsumer consumer !scanner !currentPart !partCount !bodyBytesRead = do
  chunk <- liftIO (multipartConsumerReadChunk consumer)
  if ByteString.null chunk
    then case finishMultipartScanner scanner of
      [] -> throwError MultipartTruncatedBody
      finalEvents -> consumeMultipartEvents consumer finalEvents scanner currentPart partCount bodyBytesRead True
    else do
      let bodyBytesRead' = bodyBytesRead + ByteString.length chunk
      when (bodyBytesRead' > multipartLimitsMaxBodyBytes (multipartConsumerLimits consumer)) (throwError MultipartBodyTooLarge)
      let (events, scanner') = feedMultipartChunk scanner chunk
      consumeMultipartEvents consumer events scanner' currentPart partCount bodyBytesRead' False

consumeMultipartEvents ::
  MultipartConsumer stored ->
  [MultipartEvent] ->
  MultipartScanner ->
  Maybe (PartAccumulator stored) ->
  Int ->
  Int ->
  Bool ->
  ExceptT MultipartConsumeError IO ()
consumeMultipartEvents consumer events !scanner !currentPart !partCount !bodyBytesRead atEof =
  case events of
    []
      | atEof -> throwError MultipartTruncatedBody
      | otherwise -> driveMultipartConsumer consumer scanner currentPart partCount bodyBytesRead
    event : rest -> do
      transition <- applyMultipartEvent consumer event currentPart partCount
      case transition of
        FinishMultipartConsumption -> pure ()
        ContinueMultipartConsumption nextPart nextPartCount ->
          consumeMultipartEvents consumer rest scanner nextPart nextPartCount bodyBytesRead atEof

-- | Matched against the current accumulator, not just the event: the scanner
-- never emits a body event without an unmatched 'MultipartPartStarted' open,
-- so a body event with no open part (like an explicit 'MultipartMalformed')
-- can only mean the body doesn't follow the boundary grammar.
applyMultipartEvent ::
  MultipartConsumer stored ->
  MultipartEvent ->
  Maybe (PartAccumulator stored) ->
  Int ->
  ExceptT MultipartConsumeError IO (MultipartTransition stored)
applyMultipartEvent consumer event currentPart partCount =
  case (event, currentPart) of
    (MultipartPartStarted headerBlock, Nothing) -> do
      let limits = multipartConsumerLimits consumer
      when (partCount >= multipartLimitsMaxParts limits) (throwError MultipartTooManyParts)
      accumulator <- startMultipartPart consumer headerBlock
      pure (ContinueMultipartConsumption (Just accumulator) (partCount + 1))
    (MultipartPartBodyChunk bodyBytes, Just accumulator) -> do
      accumulator' <- appendMultipartPartBytes consumer accumulator bodyBytes
      pure (ContinueMultipartConsumption (Just accumulator') partCount)
    (MultipartPartEnded, Just accumulator) -> do
      part <- liftIO (finalizeMultipartPart consumer accumulator)
      acceptance <- liftIO (multipartConsumerOnPart consumer part)
      case acceptance of
        Left rejectionError -> throwError rejectionError
        Right () -> pure (ContinueMultipartConsumption Nothing partCount)
    (MultipartFinished, Nothing) -> pure FinishMultipartConsumption
    (MultipartScannerLimitExceeded MultipartPreambleLimitExceeded, _) -> throwError MultipartPreambleTooLarge
    (MultipartScannerLimitExceeded MultipartPartHeaderLimitExceeded, _) -> throwError MultipartPartHeadersTooLarge
    _ -> throwError MultipartMalformedBody

startMultipartPart :: MultipartConsumer stored -> ByteString -> ExceptT MultipartConsumeError IO (PartAccumulator stored)
startMultipartPart consumer headerBlock =
  case parseMultipartFieldDisposition headerBlock of
    Nothing -> throwError MultipartMissingDisposition
    Just disposition ->
      case multipartFieldName disposition of
        Nothing -> throwError MultipartMissingDisposition
        Just fieldName ->
          case multipartFieldFilename disposition of
            Nothing -> pure (FieldAccumulator fieldName ByteString.empty)
            Just filename -> do
              let storage = multipartConsumerStorage consumer
              stagedUpload <- liftIO (MultipartStorage.beginMultipartUpload storage $! filename)
              liftIO (IORef.writeIORef (multipartConsumerActiveUploadReference consumer) (Just stagedUpload))
              pure (FileAccumulator fieldName filename stagedUpload 0)

appendMultipartPartBytes ::
  MultipartConsumer stored ->
  PartAccumulator stored ->
  ByteString ->
  ExceptT MultipartConsumeError IO (PartAccumulator stored)
appendMultipartPartBytes consumer accumulator bodyBytes =
  case accumulator of
    FieldAccumulator fieldName buffered ->
      let grown = buffered <> bodyBytes
       in if ByteString.length grown > multipartLimitsMaxFieldBytes (multipartConsumerLimits consumer)
            then throwError (MultipartFieldTooLarge fieldName)
            else pure (FieldAccumulator fieldName grown)
    FileAccumulator fieldName filename stagedUpload bytesWritten ->
      let bytesWritten' = bytesWritten + ByteString.length bodyBytes
       in if bytesWritten' > multipartLimitsMaxFileBytes (multipartConsumerLimits consumer)
            then do
              liftIO (discardActiveMultipartUpload (multipartConsumerActiveUploadReference consumer))
              throwError (MultipartFileTooLarge fieldName)
            else do
              liftIO (MultipartStorage.appendMultipartUpload stagedUpload bodyBytes)
              pure (FileAccumulator fieldName filename stagedUpload bytesWritten')

finalizeMultipartPart :: MultipartConsumer stored -> PartAccumulator stored -> IO (MultipartPartWith stored)
finalizeMultipartPart consumer accumulator =
  case accumulator of
    FieldAccumulator fieldName buffered -> pure (MultipartFieldPart fieldName (decodeLeniently buffered))
    FileAccumulator fieldName filename stagedUpload bytesWritten -> do
      storedUpload <- MultipartStorage.completeMultipartUpload stagedUpload
      IORef.writeIORef (multipartConsumerActiveUploadReference consumer) Nothing
      pure (MultipartFilePart fieldName filename storedUpload bytesWritten)

-- | Consume a WAI request's body as @multipart\/form-data@, given the
-- boundary parameter extracted from its @Content-Type@ header. Field values
-- and files use the bounded in-memory adapter; applications that need durable
-- storage call 'consumeMultipartBody' with an explicit adapter.
consumeMultipartRequestBody ::
  MultipartLimits ->
  ByteString ->
  Wai.Request ->
  IO (Either MultipartConsumeError [MultipartPart])
consumeMultipartRequestBody limits boundary request =
  consumeMultipartBody inMemoryMultipartStorage limits boundary (Wai.getRequestBodyChunk request)

-- | Like 'consumeMultipartRequestBody', but see 'consumeMultipartBodyWith':
-- @onPart@ runs as soon as each part finishes, before any later part
-- (including a later file part) is read.
consumeMultipartRequestBodyWith ::
  MultipartLimits ->
  ByteString ->
  Wai.Request ->
  (MultipartPart -> IO (Either MultipartConsumeError ())) ->
  IO (Either MultipartConsumeError ())
consumeMultipartRequestBodyWith limits boundary request =
  consumeMultipartBodyWith inMemoryMultipartStorage limits boundary (Wai.getRequestBodyChunk request)
