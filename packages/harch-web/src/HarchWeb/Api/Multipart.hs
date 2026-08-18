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
-- Decision (2026-08-18): client filenames remain unvalidated metadata, not
-- paths. The parser carries them as opaque 'UntrustedFilename' values across
-- both the callback and adapter boundary; an adapter must explicitly reveal
-- text before applying its own storage naming policy. Byte and item budgets
-- likewise use opaque non-negative types so a malformed integer configuration
-- cannot turn a resource limit into an accidental unbounded admission.
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
    MultipartByteLimit,
    multipartByteLimit,
    multipartByteLimitFromInt,
    multipartByteLimitValue,
    MultipartItemLimit,
    multipartItemLimit,
    multipartItemLimitFromInt,
    multipartItemLimitValue,
    MultipartLimits (..),
    defaultMultipartLimits,
    MultipartScopedPart (..),
    MultipartUpload,
    promoteMultipartUpload,
    discardMultipartUpload,
    MultipartStorage,
    MultipartStagedUpload,
    UntrustedFilename,
    untrustedFilenameText,
    multipartStorage,
    multipartStagedUpload,
    InMemoryUpload,
    inMemoryMultipartStorage,
    inMemoryUploadBytes,
    MultipartConsumeError (..),
    rejectMultipartPart,
    multipartBoundaryFromContentType,
    withMultipartBodyWith,
    withMultipartRequestBodyWithStorage,
    withMultipartRequestBodyWith,
  )
where

import Control.Exception qualified as Exception
import Control.Monad (when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteStringChar8
import Data.Char (toLower)
import Data.Foldable (for_, traverse_)
import Data.IORef qualified as IORef
import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.Encoding.Error qualified as TextEncodingError
import Data.Word (Word8)
import HarchWeb.Api.Multipart.Disposition
import HarchWeb.Api.Multipart.Scanner
import HarchWeb.Api.Multipart.Storage
import HarchWeb.Api.Multipart.Storage.Internal qualified as MultipartStorage
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import Numeric.Natural (Natural)

-- Kept eta-expanded (not point-free), and reused for both header and field
-- body bytes, so HPC ticks the decode call on every invocation rather than
-- treating it as a once-shared CAF reference.
{-# ANN decodeLeniently ("HLint: ignore Eta reduce" :: String) #-}
decodeLeniently :: ByteString -> Text
decodeLeniently bytes = TextEncoding.decodeUtf8With TextEncodingError.lenientDecode bytes

-- | A non-negative byte budget for multipart parsing and retained request
-- data. A 'Natural' input makes a negative literal unrepresentable; use
-- 'multipartByteLimitFromInt' at an integer configuration boundary.
newtype MultipartByteLimit = MultipartByteLimit Natural
  deriving (Eq, Show)

multipartByteLimit :: Natural -> MultipartByteLimit
multipartByteLimit = MultipartByteLimit

multipartByteLimitFromInt :: Int -> Maybe MultipartByteLimit
multipartByteLimitFromInt byteCount
  | byteCount >= 0 = Just (MultipartByteLimit (fromIntegral byteCount))
  | otherwise = Nothing

multipartByteLimitValue :: MultipartByteLimit -> Natural
multipartByteLimitValue (MultipartByteLimit byteCount) = byteCount

-- | A non-negative multipart part, field, or file count budget. A count of
-- zero is useful: it expresses that the corresponding kind of part is not
-- accepted at this endpoint.
newtype MultipartItemLimit = MultipartItemLimit Natural
  deriving (Eq, Show)

multipartItemLimit :: Natural -> MultipartItemLimit
multipartItemLimit = MultipartItemLimit

multipartItemLimitFromInt :: Int -> Maybe MultipartItemLimit
multipartItemLimitFromInt itemCount
  | itemCount >= 0 = Just (MultipartItemLimit (fromIntegral itemCount))
  | otherwise = Nothing

multipartItemLimitValue :: MultipartItemLimit -> Natural
multipartItemLimitValue (MultipartItemLimit itemCount) = itemCount

-- | Bounds applied while consuming a multipart body: its total streamed byte
-- count, how much preamble and each part's headers the parser may retain, how
-- large a field value may grow before it is rejected, how large a staged file
-- upload may grow before it is rejected, and how many parts, fields, and
-- files a single body may declare. The field and file counts are each
-- independently bounded (not merely implied by the combined part count) so
-- an application can cap either kind of part on its own, e.g. a form with
-- many small fields but at most one attachment.
data MultipartLimits = MultipartLimits
  { multipartLimitsMaxBodyBytes :: MultipartByteLimit,
    multipartLimitsMaxPreambleBytes :: MultipartByteLimit,
    multipartLimitsMaxPartHeaderBytes :: MultipartByteLimit,
    multipartLimitsMaxFieldBytes :: MultipartByteLimit,
    multipartLimitsMaxFileBytes :: MultipartByteLimit,
    multipartLimitsMaxParts :: MultipartItemLimit,
    multipartLimitsMaxFieldCount :: MultipartItemLimit,
    multipartLimitsMaxFileCount :: MultipartItemLimit
  }
  deriving (Eq, Show)

-- | 32 MiB body, 8 KiB preamble, 16 KiB per-part headers, 1 MiB field
-- values, 25 MiB file uploads, 100 parts, 100 fields, 20 files.
defaultMultipartLimits :: MultipartLimits
defaultMultipartLimits =
  MultipartLimits
    { multipartLimitsMaxBodyBytes = multipartByteLimit (32 * 1024 * 1024),
      multipartLimitsMaxPreambleBytes = multipartByteLimit (8 * 1024),
      multipartLimitsMaxPartHeaderBytes = multipartByteLimit (16 * 1024),
      multipartLimitsMaxFieldBytes = multipartByteLimit (1024 * 1024),
      multipartLimitsMaxFileBytes = multipartByteLimit (25 * 1024 * 1024),
      multipartLimitsMaxParts = multipartItemLimit 100,
      multipartLimitsMaxFieldCount = multipartItemLimit 100,
      multipartLimitsMaxFileCount = multipartItemLimit 20
    }

-- | One fully-consumed part: either a plain field's decoded value, kept in
-- memory, or a file upload represented by the selected storage adapter's
-- completed value and byte count.
data MultipartPartWith stored
  = MultipartFieldPart Text Text
  | MultipartFilePart Text UntrustedFilename stored Int

-- | A file part visible only while a multipart callback is running. Its
-- upload must be deliberately promoted or is discarded when that callback
-- scope finishes.
data MultipartScopedPart stored
  = MultipartScopedFieldPart Text Text
  | MultipartScopedFilePart Text UntrustedFilename (MultipartUpload stored) Int

data MultipartUpload stored = MultipartUpload
  { multipartUploadStoredValue :: stored,
    multipartUploadDiscardAction :: Maybe (stored -> IO ()),
    multipartUploadClaimedReference :: IORef.IORef Bool
  }

promoteMultipartUpload :: MultipartUpload stored -> IO (Maybe stored)
promoteMultipartUpload upload =
  IORef.atomicModifyIORef' (multipartUploadClaimedReference upload) $ \claimed ->
    if claimed then (True, Nothing) else (True, Just (multipartUploadStoredValue upload))

discardMultipartUpload :: MultipartUpload stored -> IO ()
discardMultipartUpload upload = do
  shouldDiscard <- IORef.atomicModifyIORef' (multipartUploadClaimedReference upload) $ \claimed -> (True, not claimed)
  when shouldDiscard (for_ (multipartUploadDiscardAction upload) ($ multipartUploadStoredValue upload))

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
  | -- | The body declared more plain fields than 'multipartLimitsMaxFieldCount'
    -- allows.
    MultipartTooManyFields
  | -- | The body declared more file parts than 'multipartLimitsMaxFileCount'
    -- allows.
    MultipartTooManyFiles
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
  | -- | The request did not declare @multipart/form-data@ with one valid
    -- @boundary@ parameter.
    MultipartInvalidContentType
  | -- | A declared @Content-Length@ exceeded
    -- 'multipartLimitsMaxBodyBytes' before any body chunk was read.
    MultipartDeclaredBodyTooLarge
  deriving (Eq, Show)

-- | Reject the current part from an application callback when application
-- validation has determined that the multipart request cannot continue.
-- The parser owns the concrete protocol error; callers need not manufacture
-- a parser-failure constructor merely to stop later parts from being read.
rejectMultipartPart :: IO (Either MultipartConsumeError ())
rejectMultipartPart = pure (Left MultipartMalformedBody)

-- | 'FieldAccumulator' keeps a field's body as reversed chunks plus a
-- running byte count, concatenated once in 'finalizeMultipartPart', rather
-- than appending each chunk onto a growing strict 'ByteString' with '<>' —
-- an attacker who chunks the request body finely (the client controls
-- chunk size; see 'Wai.getRequestBodyChunk') would otherwise make each
-- appended chunk re-copy the whole accumulated value, costing O(n^2) time
-- for an n-byte field within the same declared field-size limit.
data PartAccumulator stored
  = FieldAccumulator Text [ByteString] Int
  | FileAccumulator Text UntrustedFilename (MultipartStagedUpload stored) Int

discardActiveMultipartUpload :: IORef.IORef (Maybe (MultipartStagedUpload stored)) -> IO ()
discardActiveMultipartUpload activeUploadReference = do
  maybeActiveUpload <- IORef.atomicModifyIORef' activeUploadReference (Nothing,)
  for_ maybeActiveUpload MultipartStorage.discardMultipartUpload

-- | Consume multipart parts through a scoped ownership API. A file's stored
-- value is hidden behind 'MultipartUpload'; every upload left unpromoted is
-- discarded after success, rejection, an exception, or cancellation. On the
-- first consume error or callback rejection, this function stops calling
-- @readChunk@ after cleanup rather than draining an unbounded unread body.
-- The WAI server's connection and request-body limits consequently remain
-- responsible for the unread remainder's transport policy.
withMultipartBodyWith ::
  MultipartStorage stored ->
  MultipartLimits ->
  ByteString ->
  IO ByteString ->
  (MultipartScopedPart stored -> IO (Either MultipartConsumeError ())) ->
  IO (Either MultipartConsumeError ())
withMultipartBodyWith storage limits boundary readChunk onPart = Exception.mask $ \restore -> do
  uploadsReference <- IORef.newIORef []
  activeUploadReference <- IORef.newIORef Nothing
  let discardUnclaimed = IORef.readIORef uploadsReference >>= traverse_ discardMultipartUpload
      discardUploads = discardActiveMultipartUpload activeUploadReference >> discardUnclaimed
      scopedPart = \case
        MultipartFieldPart fieldName value -> pure (MultipartScopedFieldPart fieldName value)
        MultipartFilePart fieldName filename storedUpload bytesWritten -> do
          claimedReference <- IORef.newIORef False
          let upload = MultipartUpload storedUpload (MultipartStorage.discardCompletedMultipartUpload storage) claimedReference
          IORef.modifyIORef' uploadsReference (upload :)
          pure (MultipartScopedFilePart fieldName filename upload bytesWritten)
      scopedCallback part = scopedPart part >>= onPart
  result <-
    restore (consumeMultipartBodyWithActive activeUploadReference storage limits boundary readChunk scopedCallback)
      `Exception.onException` discardUploads
  case result of
    Left consumeError -> discardUploads >> pure (Left consumeError)
    Right () -> discardUnclaimed >> pure (Right ())

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
            ( newBoundedMultipartScanner
                (multipartByteLimitAsInt (multipartLimitsMaxPreambleBytes limits))
                (multipartByteLimitAsInt (multipartLimitsMaxPartHeaderBytes limits))
                boundary
            )
            Nothing
            initialMultipartPartCounts
            0
        )

data MultipartConsumer stored = MultipartConsumer
  { multipartConsumerStorage :: MultipartStorage stored,
    multipartConsumerLimits :: MultipartLimits,
    multipartConsumerActiveUploadReference :: IORef.IORef (Maybe (MultipartStagedUpload stored)),
    multipartConsumerReadChunk :: IO ByteString,
    multipartConsumerOnPart :: MultipartPartWith stored -> IO (Either MultipartConsumeError ())
  }

-- | How many parts, and how many of each kind, have completed so far. Kept
-- as one record threaded through the driver rather than three separate
-- accumulator parameters.
data MultipartPartCounts = MultipartPartCounts
  { multipartPartCountsTotal :: Int,
    multipartPartCountsFields :: Int,
    multipartPartCountsFiles :: Int
  }

initialMultipartPartCounts :: MultipartPartCounts
initialMultipartPartCounts = MultipartPartCounts 0 0 0

incrementMultipartPartCounts :: PartAccumulator stored -> MultipartPartCounts -> MultipartPartCounts
incrementMultipartPartCounts accumulator counts =
  case accumulator of
    FieldAccumulator {} -> counts {multipartPartCountsTotal = nextTotal, multipartPartCountsFields = multipartPartCountsFields counts + 1}
    FileAccumulator {} -> counts {multipartPartCountsTotal = nextTotal, multipartPartCountsFiles = multipartPartCountsFiles counts + 1}
  where
    nextTotal = multipartPartCountsTotal counts + 1

data MultipartTransition stored
  = ContinueMultipartConsumption (Maybe (PartAccumulator stored)) MultipartPartCounts
  | FinishMultipartConsumption

driveMultipartConsumer ::
  MultipartConsumer stored ->
  MultipartScanner ->
  Maybe (PartAccumulator stored) ->
  MultipartPartCounts ->
  Int ->
  ExceptT MultipartConsumeError IO ()
driveMultipartConsumer consumer !scanner !currentPart !partCounts !bodyBytesRead = do
  chunk <- liftIO (multipartConsumerReadChunk consumer)
  if ByteString.null chunk
    then case finishMultipartScanner scanner of
      [] -> throwError MultipartTruncatedBody
      finalEvents -> consumeMultipartEvents consumer finalEvents scanner currentPart partCounts bodyBytesRead True
    else do
      let chunkBytes = ByteString.length chunk
      when
        ( exceedsMultipartLimit
            (multipartLimitsMaxBodyBytes (multipartConsumerLimits consumer))
            bodyBytesRead
            chunkBytes
        )
        (throwError MultipartBodyTooLarge)
      let (events, scanner') = feedMultipartChunk scanner chunk
      consumeMultipartEvents consumer events scanner' currentPart partCounts (bodyBytesRead + chunkBytes) False

consumeMultipartEvents ::
  MultipartConsumer stored ->
  [MultipartEvent] ->
  MultipartScanner ->
  Maybe (PartAccumulator stored) ->
  MultipartPartCounts ->
  Int ->
  Bool ->
  ExceptT MultipartConsumeError IO ()
consumeMultipartEvents consumer events !scanner !currentPart !partCounts !bodyBytesRead atEof =
  case events of
    []
      | atEof -> throwError MultipartTruncatedBody
      | otherwise -> driveMultipartConsumer consumer scanner currentPart partCounts bodyBytesRead
    event : rest -> do
      transition <- applyMultipartEvent consumer event currentPart partCounts
      case transition of
        FinishMultipartConsumption -> pure ()
        ContinueMultipartConsumption nextPart nextPartCounts ->
          consumeMultipartEvents consumer rest scanner nextPart nextPartCounts bodyBytesRead atEof

-- | Matched against the current accumulator, not just the event: the scanner
-- never emits a body event without an unmatched 'MultipartPartStarted' open,
-- so a body event with no open part (like an explicit 'MultipartMalformed')
-- can only mean the body doesn't follow the boundary grammar.
applyMultipartEvent ::
  MultipartConsumer stored ->
  MultipartEvent ->
  Maybe (PartAccumulator stored) ->
  MultipartPartCounts ->
  ExceptT MultipartConsumeError IO (MultipartTransition stored)
applyMultipartEvent consumer event currentPart partCounts =
  case (event, currentPart) of
    (MultipartPartStarted headerBlock, Nothing) -> do
      let limits = multipartConsumerLimits consumer
      when (reachedMultipartItemLimit (multipartPartCountsTotal partCounts) (multipartLimitsMaxParts limits)) (throwError MultipartTooManyParts)
      accumulator <- startMultipartPart consumer partCounts headerBlock
      pure (ContinueMultipartConsumption (Just accumulator) (incrementMultipartPartCounts accumulator partCounts))
    (MultipartPartBodyChunk bodyBytes, Just accumulator) -> do
      accumulator' <- appendMultipartPartBytes consumer accumulator bodyBytes
      pure (ContinueMultipartConsumption (Just accumulator') partCounts)
    (MultipartPartEnded, Just accumulator) -> do
      part <- liftIO (finalizeMultipartPart consumer accumulator)
      acceptance <- liftIO (multipartConsumerOnPart consumer part)
      case acceptance of
        Left rejectionError -> throwError rejectionError
        Right () -> pure (ContinueMultipartConsumption Nothing partCounts)
    (MultipartFinished, Nothing) -> pure FinishMultipartConsumption
    (MultipartPreambleLimitExceeded, _) -> throwError MultipartPreambleTooLarge
    (MultipartPartHeaderLimitExceeded, _) -> throwError MultipartPartHeadersTooLarge
    _ -> throwError MultipartMalformedBody

startMultipartPart :: MultipartConsumer stored -> MultipartPartCounts -> ByteString -> ExceptT MultipartConsumeError IO (PartAccumulator stored)
startMultipartPart consumer partCounts headerBlock =
  case parseMultipartFieldDisposition headerBlock of
    Nothing -> throwError MultipartMissingDisposition
    Just disposition ->
      case multipartFieldName disposition of
        Nothing -> throwError MultipartMissingDisposition
        Just fieldName ->
          case multipartFieldFilename disposition of
            Nothing -> do
              when
                (reachedMultipartItemLimit (multipartPartCountsFields partCounts) (multipartLimitsMaxFieldCount (multipartConsumerLimits consumer)))
                (throwError MultipartTooManyFields)
              pure (FieldAccumulator fieldName [] 0)
            Just filename -> do
              when
                (reachedMultipartItemLimit (multipartPartCountsFiles partCounts) (multipartLimitsMaxFileCount (multipartConsumerLimits consumer)))
                (throwError MultipartTooManyFiles)
              let storage = multipartConsumerStorage consumer
                  untrustedFilename = MultipartStorage.untrustedFilenameFromText filename
              -- An async exception landing between the upload starting and its
              -- reference being recorded would leave it visible to neither
              -- 'discardActiveMultipartUpload' nor 'discardUnclaimed', orphaning
              -- it in the storage adapter; mask_ makes the two one atomic step.
              stagedUpload <-
                liftIO . Exception.mask_ $ do
                  stagedUpload <- MultipartStorage.beginMultipartUpload storage untrustedFilename
                  IORef.writeIORef (multipartConsumerActiveUploadReference consumer) (Just stagedUpload)
                  pure stagedUpload
              pure (FileAccumulator fieldName untrustedFilename stagedUpload 0)

appendMultipartPartBytes ::
  MultipartConsumer stored ->
  PartAccumulator stored ->
  ByteString ->
  ExceptT MultipartConsumeError IO (PartAccumulator stored)
appendMultipartPartBytes consumer accumulator bodyBytes =
  case accumulator of
    FieldAccumulator fieldName chunks buffered ->
      if exceedsMultipartLimit
        (multipartLimitsMaxFieldBytes (multipartConsumerLimits consumer))
        buffered
        (ByteString.length bodyBytes)
        then throwError (MultipartFieldTooLarge fieldName)
        else pure (FieldAccumulator fieldName (bodyBytes : chunks) (buffered + ByteString.length bodyBytes))
    FileAccumulator fieldName filename stagedUpload bytesWritten ->
      if exceedsMultipartLimit
        (multipartLimitsMaxFileBytes (multipartConsumerLimits consumer))
        bytesWritten
        (ByteString.length bodyBytes)
        then do
          liftIO (discardActiveMultipartUpload (multipartConsumerActiveUploadReference consumer))
          throwError (MultipartFileTooLarge fieldName)
        else do
          liftIO (MultipartStorage.appendMultipartUpload stagedUpload bodyBytes)
          pure (FileAccumulator fieldName filename stagedUpload (bytesWritten + ByteString.length bodyBytes))

exceedsMultipartLimit :: MultipartByteLimit -> Int -> Int -> Bool
exceedsMultipartLimit maximumBytes current increment =
  toInteger current + toInteger increment > toInteger (multipartByteLimitValue maximumBytes)

multipartByteLimitAsInt :: MultipartByteLimit -> Int
multipartByteLimitAsInt maximumBytes =
  fromInteger (min (toInteger (maxBound :: Int)) (toInteger (multipartByteLimitValue maximumBytes)))

reachedMultipartItemLimit :: Int -> MultipartItemLimit -> Bool
reachedMultipartItemLimit current maximumItems =
  toInteger current >= toInteger (multipartItemLimitValue maximumItems)

finalizeMultipartPart :: MultipartConsumer stored -> PartAccumulator stored -> IO (MultipartPartWith stored)
finalizeMultipartPart consumer accumulator =
  case accumulator of
    FieldAccumulator fieldName chunks _buffered -> pure (MultipartFieldPart fieldName (decodeLeniently (ByteString.concat (reverse chunks))))
    FileAccumulator fieldName filename stagedUpload bytesWritten -> do
      storedUpload <- MultipartStorage.completeMultipartUpload stagedUpload
      IORef.writeIORef (multipartConsumerActiveUploadReference consumer) Nothing
      pure (MultipartFilePart fieldName filename storedUpload bytesWritten)

-- | Parse a strict @multipart\/form-data@ media type and its single boundary
-- parameter. Parameter names and the media type are ASCII case-insensitive;
-- malformed, repeated, empty, quoted-incorrectly, or RFC 2046-invalid
-- boundaries are rejected before a body reader is called.
multipartBoundaryFromContentType :: ByteString -> Either MultipartConsumeError ByteString
multipartBoundaryFromContentType contentType =
  do
    (mediaType, parameters) <- splitMultipartContentType contentType
    if asciiLower mediaType == "multipart/form-data"
      then do
        parsedParameters <- traverse parseContentTypeParameter parameters
        boundary <- requireSingleBoundary parsedParameters
        requireValidBoundary boundary
      else Left MultipartInvalidContentType

splitMultipartContentType :: ByteString -> Either MultipartConsumeError (ByteString, [ByteString])
splitMultipartContentType contentType =
  case map stripOptionalWhitespace (ByteString.split 59 contentType) of
    mediaType : parameters -> Right (mediaType, parameters)
    [] -> Left MultipartInvalidContentType

parseContentTypeParameter :: ByteString -> Either MultipartConsumeError (ByteString, ByteString)
parseContentTypeParameter parameter =
  case ByteString.break (== 61) parameter of
    (name, valueWithEquals)
      | ByteString.null name || ByteString.null valueWithEquals -> Left MultipartInvalidContentType
      | otherwise -> do
          value <- unquoteBoundaryValue (stripOptionalWhitespace (ByteString.drop 1 valueWithEquals))
          if ByteString.null value then Left MultipartInvalidContentType else Right (stripOptionalWhitespace name, value)

unquoteBoundaryValue :: ByteString -> Either MultipartConsumeError ByteString
unquoteBoundaryValue value
  | ByteString.length value >= 2 && ByteString.head value == 34 && ByteString.last value == 34 =
      let unquoted = ByteString.init (ByteString.tail value)
       in if ByteString.elem 34 unquoted || ByteString.elem 92 unquoted then Left MultipartInvalidContentType else Right unquoted
  | ByteString.elem 34 value || ByteString.elem 92 value = Left MultipartInvalidContentType
  | otherwise = Right value

requireSingleBoundary :: [(ByteString, ByteString)] -> Either MultipartConsumeError ByteString
requireSingleBoundary parameters =
  case [value | (name, value) <- parameters, asciiLower name == "boundary"] of
    [boundary] -> Right boundary
    _ -> Left MultipartInvalidContentType

requireValidBoundary :: ByteString -> Either MultipartConsumeError ByteString
requireValidBoundary boundary
  | ByteString.length boundary <= 70 && ByteString.all isBoundaryCharacter boundary = Right boundary
  | otherwise = Left MultipartInvalidContentType

isBoundaryCharacter :: Word8 -> Bool
isBoundaryCharacter byte =
  ByteString.elem byte "'()+_,-./:=?" || (byte >= 48 && byte <= 57) || (byte >= 65 && byte <= 90) || (byte >= 97 && byte <= 122)

stripOptionalWhitespace :: ByteString -> ByteString
stripOptionalWhitespace = ByteString.dropWhileEnd isOptionalWhitespace . ByteString.dropWhile isOptionalWhitespace

isOptionalWhitespace :: Word8 -> Bool
isOptionalWhitespace byte = byte == 32 || byte == 9

asciiLower :: ByteString -> ByteString
asciiLower = ByteStringChar8.map toLower

multipartRequestBoundary :: MultipartLimits -> Wai.Request -> Either MultipartConsumeError ByteString
multipartRequestBoundary limits request = do
  contentType <- maybe (Left MultipartInvalidContentType) Right (lookup Http.hContentType (Wai.requestHeaders request))
  boundary <- multipartBoundaryFromContentType contentType
  case lookup Http.hContentLength (Wai.requestHeaders request) >>= parseContentLength of
    Just declaredBytes | toInteger declaredBytes > toInteger (multipartByteLimitValue (multipartLimitsMaxBodyBytes limits)) -> Left MultipartDeclaredBodyTooLarge
    _ -> Right boundary

parseContentLength :: ByteString -> Maybe Integer
parseContentLength value = do
  (bytes, remaining) <- ByteStringChar8.readInteger value
  if bytes >= 0 && ByteString.null remaining then Just bytes else Nothing

-- | WAI request variant of 'withMultipartBodyWith'. The application selects
-- the storage adapter explicitly; every upload left unpromoted is discarded
-- at scope exit.
withMultipartRequestBodyWithStorage ::
  MultipartStorage stored ->
  MultipartLimits ->
  Wai.Request ->
  (MultipartScopedPart stored -> IO (Either MultipartConsumeError ())) ->
  IO (Either MultipartConsumeError ())
withMultipartRequestBodyWithStorage storage limits request onPart =
  case multipartRequestBoundary limits request of
    Left requestError -> pure (Left requestError)
    Right boundary -> withMultipartBodyWith storage limits boundary (Wai.getRequestBodyChunk request) onPart

-- | Bounded in-memory convenience variant of
-- 'withMultipartRequestBodyWithStorage'. Applications needing disk, object
-- storage, a scanning quarantine, or another durable backend must select
-- that adapter explicitly with the storage-selected variant.
withMultipartRequestBodyWith ::
  MultipartLimits ->
  Wai.Request ->
  (MultipartScopedPart InMemoryUpload -> IO (Either MultipartConsumeError ())) ->
  IO (Either MultipartConsumeError ())
withMultipartRequestBodyWith = withMultipartRequestBodyWithStorage inMemoryMultipartStorage
