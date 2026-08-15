{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Unit.HarchWeb.Api.MultipartSpec (spec) where

import Control.Concurrent (forkIO, myThreadId)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception qualified as Exception
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteStringChar8
import Data.IORef qualified as IORef
import Data.List (mapAccumL)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import HarchWeb.Api.Multipart
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import System.Directory (doesFileExist, removeFile)
import System.IO (Handle, hClose)
import System.IO.Temp qualified as Temp
import Test.Hspec
import TestCore.CustomAssertions (expectAll)

boundaryToken :: ByteString
boundaryToken = "BOUNDARY123"

fieldPartHeaders :: ByteString
fieldPartHeaders = "Content-Disposition: form-data; name=\"field1\""

filePartHeaders :: ByteString
filePartHeaders =
  "Content-Disposition: form-data; name=\"file1\"; filename=\"a.txt\"\r\nContent-Type: text/plain"

twoPartBody :: ByteString
twoPartBody =
  "--"
    <> boundaryToken
    <> "\r\n"
    <> fieldPartHeaders
    <> "\r\n\r\n"
    <> "value1"
    <> "\r\n--"
    <> boundaryToken
    <> "\r\n"
    <> filePartHeaders
    <> "\r\n\r\n"
    <> "file content here"
    <> "\r\n--"
    <> boundaryToken
    <> "--\r\n"

-- | A file part first, then a field part -- the reverse of 'twoPartBody' --
-- so a rejection on the second (field) part exercises the current
-- completed-upload lifecycle before AD adds final cleanup semantics.
fileTheRejectsSecondFieldBody :: ByteString
fileTheRejectsSecondFieldBody =
  "--"
    <> boundaryToken
    <> "\r\n"
    <> filePartHeaders
    <> "\r\n\r\n"
    <> "file content here"
    <> "\r\n--"
    <> boundaryToken
    <> "\r\n"
    <> fieldPartHeaders
    <> "\r\n\r\n"
    <> "value1"
    <> "\r\n--"
    <> boundaryToken
    <> "--\r\n"

fileThenMalformedFieldBody :: ByteString
fileThenMalformedFieldBody =
  "--"
    <> boundaryToken
    <> "\r\n"
    <> filePartHeaders
    <> "\r\n\r\n"
    <> "file content here"
    <> "\r\n--"
    <> boundaryToken
    <> "\r\nContent-Type: text/plain\r\n\r\nvalue\r\n--"
    <> boundaryToken
    <> "--\r\n"

fieldThenMalformedFieldBody :: ByteString
fieldThenMalformedFieldBody =
  "--"
    <> boundaryToken
    <> "\r\n"
    <> fieldPartHeaders
    <> "\r\n\r\n"
    <> "value1"
    <> "\r\n--"
    <> boundaryToken
    <> "\r\nContent-Type: text/plain\r\n\r\nvalue\r\n--"
    <> boundaryToken
    <> "--\r\n"

expectedTwoPartEvents :: [MultipartEvent]
expectedTwoPartEvents =
  [ MultipartPartStarted fieldPartHeaders,
    MultipartPartBodyChunk "value1",
    MultipartPartEnded,
    MultipartPartStarted filePartHeaders,
    MultipartPartBodyChunk "file content here",
    MultipartPartEnded,
    MultipartFinished
  ]

-- | Merge consecutive body-chunk events so scans that split a body across a
-- different number of chunks remain comparable to one expected event list.
normalizeEvents :: [MultipartEvent] -> [MultipartEvent]
normalizeEvents (MultipartPartBodyChunk first : MultipartPartBodyChunk second : rest) =
  normalizeEvents (MultipartPartBodyChunk (first <> second) : rest)
normalizeEvents (event : rest) = event : normalizeEvents rest
normalizeEvents [] = []

runScanner :: ByteString -> [ByteString] -> [MultipartEvent]
runScanner boundary chunks =
  concat eventLists <> finishMultipartScanner finalScanner
  where
    (finalScanner, eventLists) = mapAccumL feedStep (newMultipartScanner boundary) chunks
    feedStep scanner chunk = let (events, next) = feedMultipartChunk scanner chunk in (next, events)

splitAt2 :: Int -> ByteString -> [ByteString]
splitAt2 splitPoint body = [ByteString.take splitPoint body, ByteString.drop splitPoint body]

allByteChunks :: ByteString -> [ByteString]
allByteChunks body = map ByteString.singleton (ByteString.unpack body)

-- | An 'IO' action that yields each listed chunk in order, then an empty
-- 'ByteString' forever after, matching 'Network.Wai.getRequestBodyChunk'.
chunkReader :: [ByteString] -> IO (IO ByteString)
chunkReader chunks = do
  remaining <- IORef.newIORef chunks
  pure $ do
    queued <- IORef.readIORef remaining
    case queued of
      [] -> pure ByteString.empty
      next : rest -> do
        IORef.writeIORef remaining rest
        pure next

-- | Opens every file part into a fresh file within a temporary directory
-- that is removed once the action completes.
withTestUploadOpener :: ((Text -> IO (FilePath, Handle)) -> IO a) -> IO a
withTestUploadOpener action =
  Temp.withSystemTempDirectory "harch-web-multipart-test" $ \tempDirectory ->
    action (\_filenameHint -> Temp.openBinaryTempFile tempDirectory "upload.tmp")

storageFromOpener :: (Text -> IO (FilePath, Handle)) -> MultipartStorage FilePath
storageFromOpener openUploadFile =
  multipartStorage
    ( \filename -> do
        (path, handle) <- openUploadFile filename
        pure $
          multipartStagedUpload
            (ByteString.hPut handle)
            (hClose handle >> pure path)
            (hClose handle >> removeFile path)
    )
    (Just removeFile)

data ObservedPart
  = ObservedField Text Text
  | ObservedFile Text Text ByteString Int
  deriving (Eq, Show)

runConsume :: MultipartLimits -> [ByteString] -> IO (Either MultipartConsumeError [ObservedPart])
runConsume limits chunks = do
  observedPartsReference <- IORef.newIORef []
  readChunk <- chunkReader chunks
  result <-
    withMultipartBodyWith inMemoryMultipartStorage limits boundaryToken readChunk $ \case
      MultipartScopedFieldPart fieldName value -> do
        IORef.modifyIORef' observedPartsReference (ObservedField fieldName value :)
        pure (Right ())
      MultipartScopedFilePart fieldName filename upload byteCount -> do
        maybeUpload <- promoteMultipartUpload upload
        case maybeUpload of
          Nothing -> pure (Left MultipartMalformedBody)
          Just inMemoryUpload -> do
            IORef.modifyIORef' observedPartsReference (ObservedFile fieldName filename (inMemoryUploadBytes inMemoryUpload) byteCount :)
            pure (Right ())
  case result of
    Left consumeError -> pure (Left consumeError)
    Right () -> Right . reverse <$> IORef.readIORef observedPartsReference

runRequestConsume :: MultipartLimits -> Wai.Request -> IO (Either MultipartConsumeError [ObservedPart])
runRequestConsume limits request = do
  observedPartsReference <- IORef.newIORef []
  result <-
    withMultipartRequestBodyWith limits request $ \case
      MultipartScopedFieldPart fieldName value -> do
        IORef.modifyIORef' observedPartsReference (ObservedField fieldName value :)
        pure (Right ())
      MultipartScopedFilePart fieldName filename upload byteCount -> do
        maybeUpload <- promoteMultipartUpload upload
        case maybeUpload of
          Nothing -> pure (Left MultipartMalformedBody)
          Just inMemoryUpload -> do
            IORef.modifyIORef' observedPartsReference (ObservedFile fieldName filename (inMemoryUploadBytes inMemoryUpload) byteCount :)
            pure (Right ())
  case result of
    Left consumeError -> pure (Left consumeError)
    Right () -> Right . reverse <$> IORef.readIORef observedPartsReference

shouldReject :: IO (Either MultipartConsumeError [ObservedPart]) -> MultipartConsumeError -> Expectation
shouldReject action expectedError = do
  result <- action
  case result of
    Left actualError -> actualError `shouldBe` expectedError
    Right parts -> expectationFailure ("expected rejection, received: " <> show parts)

testLimits :: MultipartLimits
testLimits =
  defaultMultipartLimits
    { multipartLimitsMaxBodyBytes = 4096,
      multipartLimitsMaxFieldBytes = 1024,
      multipartLimitsMaxFileBytes = 1024,
      multipartLimitsMaxParts = 3
    }

singleFieldBody :: ByteString
singleFieldBody =
  "--" <> boundaryToken <> "\r\n" <> fieldPartHeaders <> "\r\n\r\n" <> "value1" <> "\r\n--" <> boundaryToken <> "--\r\n"

spec :: Spec
spec =
  describe "HarchWeb.Api.Multipart" $ do
    it "scans a two-part body delivered as a single chunk" $
      normalizeEvents (runScanner boundaryToken [twoPartBody]) `shouldBe` expectedTwoPartEvents

    it "scans a two-part body split at every possible two-way chunk boundary" $
      expectAll
        ( (normalizeEvents (runScanner boundaryToken (splitAt2 0 twoPartBody)) `shouldBe` expectedTwoPartEvents)
            :| [ normalizeEvents (runScanner boundaryToken (splitAt2 splitPoint twoPartBody)) `shouldBe` expectedTwoPartEvents
               | splitPoint <- [1 .. ByteString.length twoPartBody]
               ]
        )

    it "scans a two-part body delivered one byte at a time" $
      normalizeEvents (runScanner boundaryToken (allByteChunks twoPartBody)) `shouldBe` expectedTwoPartEvents

    it "ignores preamble content before the first boundary" $
      normalizeEvents (runScanner boundaryToken ["ignored preamble\r\n" <> twoPartBody]) `shouldBe` expectedTwoPartEvents

    it "reports a truncated stream by never reaching MultipartFinished" $
      let events =
            runScanner
              boundaryToken
              ["--" <> boundaryToken <> "\r\n" <> fieldPartHeaders <> "\r\n\r\n" <> "partial"]
       in expectAll
            ( ((MultipartFinished `notElem` events) `shouldBe` True)
                :| [normalizeEvents events `shouldBe` [MultipartPartStarted fieldPartHeaders, MultipartPartBodyChunk "partial"]]
            )

    it "flags a delimiter followed by neither -- nor CRLF as malformed" $
      runScanner boundaryToken ["--" <> boundaryToken <> "XYZ"]
        `shouldBe` [MultipartMalformed]

    it "retains an unresolved partial-boundary tail when the stream ends before it can be resolved" $
      let truncatingDelimiter = "\r\n--BOU"
       in normalizeEvents
            ( runScanner
                boundaryToken
                ["--" <> boundaryToken <> "\r\n" <> fieldPartHeaders <> "\r\n\r\n" <> "value" <> truncatingDelimiter]
            )
            `shouldBe` [MultipartPartStarted fieldPartHeaders, MultipartPartBodyChunk ("value" <> truncatingDelimiter)]

    it "completes a part's headers that arrive split across two chunks" $
      normalizeEvents
        ( runScanner
            boundaryToken
            [ "--" <> boundaryToken <> "\r\nContent-Disposition: form-data; nam",
              "e=\"field1\"\r\n\r\nvalue1\r\n--" <> boundaryToken <> "--\r\n"
            ]
        )
        `shouldBe` [MultipartPartStarted fieldPartHeaders, MultipartPartBodyChunk "value1", MultipartPartEnded, MultipartFinished]

    it "completes a second (mid-stream) part's headers that arrive split across two chunks" $
      normalizeEvents
        ( runScanner
            boundaryToken
            [ "--" <> boundaryToken <> "\r\n" <> fieldPartHeaders <> "\r\n\r\nvalue1\r\n--" <> boundaryToken <> "\r\nContent-Disposition: form-data; nam",
              "e=\"file1\"\r\n\r\nfile content\r\n--" <> boundaryToken <> "--\r\n"
            ]
        )
        `shouldBe` [ MultipartPartStarted fieldPartHeaders,
                     MultipartPartBodyChunk "value1",
                     MultipartPartEnded,
                     MultipartPartStarted "Content-Disposition: form-data; name=\"file1\"",
                     MultipartPartBodyChunk "file content",
                     MultipartPartEnded,
                     MultipartFinished
                   ]

    it "reports a part with an empty body" $
      normalizeEvents
        ( runScanner
            boundaryToken
            ["--" <> boundaryToken <> "\r\n" <> fieldPartHeaders <> "\r\n\r\n" <> "\r\n--" <> boundaryToken <> "--\r\n"]
        )
        `shouldBe` [MultipartPartStarted fieldPartHeaders, MultipartPartEnded, MultipartFinished]

    it "derives comparable, printable representations for every event constructor" $
      let events =
            [ MultipartPartStarted "h",
              MultipartPartBodyChunk "b",
              MultipartPartEnded,
              MultipartFinished,
              MultipartMalformed,
              MultipartPreambleLimitExceeded,
              MultipartPartHeaderLimitExceeded
            ]
       in expectAll
            ( (sum [fromEnum (left == right) | left <- events, right <- events] `shouldBe` length events)
                :| [ sum [fromEnum (left /= right) | left <- events, right <- events] `shouldBe` length events * (length events - 1),
                     sum [length (show event) + length (showList [event] "") | event <- events] `shouldSatisfy` (> 0)
                   ]
            )

    describe "parseMultipartFieldDisposition" $ do
      it "extracts a plain field's name" $
        parseMultipartFieldDisposition fieldPartHeaders
          `shouldBe` Just (MultipartFieldDisposition (Just "field1") Nothing)

      it "extracts a file field's name and filename" $
        parseMultipartFieldDisposition filePartHeaders
          `shouldBe` Just (MultipartFieldDisposition (Just "file1") (Just "a.txt"))

      it "returns Nothing when there is no Content-Disposition header" $
        parseMultipartFieldDisposition "Content-Type: text/plain" `shouldBe` Nothing

      it "is case-insensitive for the header name" $
        parseMultipartFieldDisposition "content-DISPOSITION: form-data; name=\"x\""
          `shouldBe` Just (MultipartFieldDisposition (Just "x") Nothing)

      it "keeps a semicolon inside a quoted filename from ending the parameter early" $
        parseMultipartFieldDisposition "Content-Disposition: form-data; name=\"f\"; filename=\"a;b.txt\""
          `shouldBe` Just (MultipartFieldDisposition (Just "f") (Just "a;b.txt"))

      it "unescapes a backslash-escaped quote inside a quoted value" $
        parseMultipartFieldDisposition "Content-Disposition: form-data; name=\"f\"; filename=\"a\\\"b.txt\""
          `shouldBe` Just (MultipartFieldDisposition (Just "f") (Just "a\"b.txt"))

      it "ignores a malformed parameter without '=' while keeping the well-formed ones" $
        parseMultipartFieldDisposition "Content-Disposition: form-data; malformed; name=\"f\""
          `shouldBe` Just (MultipartFieldDisposition (Just "f") Nothing)

      it "decodes non-ASCII header bytes leniently rather than failing" $
        parseMultipartFieldDisposition "Content-Disposition: form-data; name=\"f\"; filename=\"caf\xC3\xA9.txt\""
          `shouldBe` Just (MultipartFieldDisposition (Just "f") (Just "caf\233.txt"))

      it "substitutes the Unicode replacement character for invalid UTF-8 header bytes" $
        parseMultipartFieldDisposition "Content-Disposition: form-data; name=\"f\"; filename=\"bad\xFF.txt\""
          `shouldBe` Just (MultipartFieldDisposition (Just "f") (Just "bad\65533.txt"))

      it "skips a header line without a colon rather than failing the whole block" $
        parseMultipartFieldDisposition "garbage line\r\nContent-Disposition: form-data; name=\"f\""
          `shouldBe` Just (MultipartFieldDisposition (Just "f") Nothing)

      it "finds Content-Disposition even when it is not the first header" $
        parseMultipartFieldDisposition "Content-Type: text/plain\r\nContent-Disposition: form-data; name=\"f\""
          `shouldBe` Just (MultipartFieldDisposition (Just "f") Nothing)

      it "keeps an unquoted parameter value as-is" $
        parseMultipartFieldDisposition "Content-Disposition: form-data; name=f"
          `shouldBe` Just (MultipartFieldDisposition (Just "f") Nothing)

      it "does not crash on an unterminated quoted value ending in a backslash" $
        parseMultipartFieldDisposition "Content-Disposition: form-data; name=\"f\"; filename=\"a\\"
          `shouldSatisfy` \case
            Just (MultipartFieldDisposition (Just "f") (Just _)) -> True
            _ -> False

      it "derives comparable, printable representations for MultipartFieldDisposition" $
        let dispositions =
              [ MultipartFieldDisposition Nothing Nothing,
                MultipartFieldDisposition (Just "f") Nothing,
                MultipartFieldDisposition (Just "f") (Just "a.txt")
              ]
         in expectAll
              ( (sum [fromEnum (left == right) | left <- dispositions, right <- dispositions] `shouldBe` length dispositions)
                  :| [ sum [fromEnum (left /= right) | left <- dispositions, right <- dispositions]
                         `shouldBe` length dispositions * (length dispositions - 1),
                       sum [length (show d) + length (showList [d] "") | d <- dispositions] `shouldSatisfy` (> 0)
                     ]
              )

    describe "withMultipartBodyWith" $ do
      it "keeps the built-in storage adapter in memory within the file budget" $ do
        result <- runConsume testLimits [twoPartBody]
        equivalentResult <- runConsume testLimits [twoPartBody]
        case (result, equivalentResult) of
          ( Right [ObservedField "field1" "value1", ObservedFile "file1" "a.txt" upload byteCount],
            Right [ObservedField "field1" "value1", ObservedFile "file1" "a.txt" equivalentUpload _]
            ) ->
              expectAll
                ( (upload `shouldBe` "file content here")
                    :| [ byteCount `shouldBe` ByteString.length "file content here",
                         upload `shouldBe` equivalentUpload,
                         show upload `shouldSatisfy` (not . null),
                         showList [upload] "" `shouldSatisfy` (not . null)
                       ]
                )
          results -> expectationFailure ("unexpected results: " <> show results)

      it "consumes a single field part through the scoped callback" $ do
        result <- runConsume testLimits [singleFieldBody]
        case result of
          Right [ObservedField "field1" "value1"] -> pure ()
          other -> expectationFailure ("unexpected result: " <> show other)

      it "decodes invalid UTF-8 field bytes leniently" $ do
        let invalidUtf8FieldBody =
              "--"
                <> boundaryToken
                <> "\r\nContent-Disposition: form-data; name=\"field1\"\r\n\r\nbad\xFF\r\n--"
                <> boundaryToken
                <> "--\r\n"
        result <- runConsume testLimits [invalidUtf8FieldBody]
        result `shouldBe` Right [ObservedField "field1" "bad\65533"]

      it "uses an application-supplied storage adapter for a file part" $
        withTestUploadOpener $ \openUploadFile -> do
          readChunk <- chunkReader [twoPartBody]
          promotedReference <- IORef.newIORef Nothing
          result <-
            withMultipartBodyWith (storageFromOpener openUploadFile) testLimits boundaryToken readChunk $ \case
              MultipartScopedFieldPart "field1" "value1" -> pure (Right ())
              MultipartScopedFilePart "file1" "a.txt" upload byteCount -> do
                promoted <- promoteMultipartUpload upload
                IORef.writeIORef promotedReference ((,byteCount) <$> promoted)
                pure (Right ())
              _ -> pure (Left MultipartMalformedBody)
          promoted <- IORef.readIORef promotedReference
          case (result, promoted) of
            (Right (), Just (tempPath, byteCount)) -> do
              spooledContent <- ByteString.readFile tempPath
              expectAll
                ( (spooledContent `shouldBe` "file content here")
                    :| [byteCount `shouldBe` ByteString.length "file content here"]
                )
            other -> expectationFailure ("unexpected result: " <> show other)

      it "discards an earlier completed file when a later part is malformed" $
        withTestUploadOpener $ \openUploadFile -> do
          spooledPathReference <- IORef.newIORef Nothing
          let trackedOpener filename = do
                (path, handle) <- openUploadFile filename
                IORef.writeIORef spooledPathReference (Just path)
                pure (path, handle)
          readChunk <- chunkReader [fileThenMalformedFieldBody]
          result <- withMultipartBodyWith (storageFromOpener trackedOpener) testLimits boundaryToken readChunk (const (pure (Right ())))
          maybeSpooledPath <- IORef.readIORef spooledPathReference
          case (result, maybeSpooledPath) of
            (Left MultipartMissingDisposition, Just spooledPath) -> do
              doesFileExist spooledPath `shouldReturn` False
            other -> expectationFailure ("unexpected result: " <> show other)

      it "discards an earlier completed in-memory file when a later part is malformed" $
        shouldReject (runConsume testLimits [fileThenMalformedFieldBody]) MultipartMissingDisposition

      it "does not retain an earlier field when a later part is malformed" $
        shouldReject (runConsume testLimits [fieldThenMalformedFieldBody]) MultipartMissingDisposition

      it "discards a file that is still staged when the body is truncated" $
        withTestUploadOpener $ \openUploadFile -> do
          spooledPathReference <- IORef.newIORef Nothing
          let trackedOpener filename = do
                (path, handle) <- openUploadFile filename
                IORef.writeIORef spooledPathReference (Just path)
                pure (path, handle)
              truncatedFileBody =
                "--" <> boundaryToken <> "\r\n" <> filePartHeaders <> "\r\n\r\npartial file contents"
          readChunk <- chunkReader [truncatedFileBody]
          result <- withMultipartBodyWith (storageFromOpener trackedOpener) testLimits boundaryToken readChunk (const (pure (Right ())))
          maybeSpooledPath <- IORef.readIORef spooledPathReference
          case (result, maybeSpooledPath) of
            (Left MultipartTruncatedBody, Just spooledPath) ->
              doesFileExist spooledPath `shouldReturn` False
            other -> expectationFailure ("unexpected result: " <> show other)

      it "discards a file that is still staged when reading the body throws" $
        withTestUploadOpener $ \openUploadFile -> do
          spooledPathReference <- IORef.newIORef Nothing
          nextChunkReference <- IORef.newIORef (Just ("--" <> boundaryToken <> "\r\n" <> filePartHeaders <> "\r\n\r\npartial file contents"))
          let trackedOpener filename = do
                (path, handle) <- openUploadFile filename
                IORef.writeIORef spooledPathReference (Just path)
                pure (path, handle)
              failingReadChunk = do
                maybeChunk <- IORef.atomicModifyIORef' nextChunkReference (Nothing,)
                case maybeChunk of
                  Just chunk -> pure chunk
                  Nothing -> Exception.throwIO (userError "request body read failed")
          attempt :: Either Exception.SomeException (Either MultipartConsumeError ()) <-
            Exception.try (withMultipartBodyWith (storageFromOpener trackedOpener) testLimits boundaryToken failingReadChunk (const (pure (Right ()))))
          maybeSpooledPath <- IORef.readIORef spooledPathReference
          case (attempt, maybeSpooledPath) of
            (Left _, Just spooledPath) -> doesFileExist spooledPath `shouldReturn` False
            other -> expectationFailure ("unexpected result: " <> show other)

      it "discards a file that is still staged when the body reader is cancelled" $
        withTestUploadOpener $ \openUploadFile -> do
          spooledPathReference <- IORef.newIORef Nothing
          nextChunkReference <- IORef.newIORef (Just ("--" <> boundaryToken <> "\r\n" <> filePartHeaders <> "\r\n\r\npartial file contents"))
          let trackedOpener filename = do
                (path, handle) <- openUploadFile filename
                IORef.writeIORef spooledPathReference (Just path)
                pure (path, handle)
              cancelledReadChunk = do
                maybeChunk <- IORef.atomicModifyIORef' nextChunkReference (Nothing,)
                case maybeChunk of
                  Just chunk -> pure chunk
                  Nothing -> Exception.throwIO Exception.ThreadKilled
          attempt :: Either Exception.SomeException (Either MultipartConsumeError ()) <-
            Exception.try (withMultipartBodyWith (storageFromOpener trackedOpener) testLimits boundaryToken cancelledReadChunk (const (pure (Right ()))))
          maybeSpooledPath <- IORef.readIORef spooledPathReference
          case (attempt, maybeSpooledPath) of
            (Left exception, Just spooledPath) -> do
              Exception.fromException exception `shouldBe` Just Exception.ThreadKilled
              doesFileExist spooledPath `shouldReturn` False
            other -> expectationFailure ("unexpected result: " <> show other)

      it "discards a file cancelled between its storage staging and its active-upload bookkeeping" $
        withTestUploadOpener $ \openUploadFile -> do
          spooledPathReference <- IORef.newIORef Nothing
          openedSignal <- newEmptyMVar
          callingThreadId <- myThreadId
          let signalingOpener filename = do
                (path, handle) <- openUploadFile filename
                IORef.writeIORef spooledPathReference (Just path)
                putMVar openedSignal ()
                pure (path, handle)
              readChunk = pure ("--" <> boundaryToken <> "\r\n" <> filePartHeaders <> "\r\n\r\nfile contents")
          _ <- forkIO (takeMVar openedSignal >> Exception.throwTo callingThreadId Exception.ThreadKilled)
          attempt :: Either Exception.SomeException (Either MultipartConsumeError ()) <-
            Exception.try (withMultipartBodyWith (storageFromOpener signalingOpener) testLimits boundaryToken readChunk (const (pure (Right ()))))
          maybeSpooledPath <- IORef.readIORef spooledPathReference
          case (attempt, maybeSpooledPath) of
            (Left exception, Just spooledPath) -> do
              Exception.fromException exception `shouldBe` Just Exception.ThreadKilled
              doesFileExist spooledPath `shouldReturn` False
            other -> expectationFailure ("unexpected result: " <> show other)

      it "consumes a body delivered one byte at a time, exercising the multi-chunk driving loop" $ do
        result <- runConsume testLimits (allByteChunks singleFieldBody)
        case result of
          Right [ObservedField "field1" "value1"] -> pure ()
          other -> expectationFailure ("unexpected result: " <> show other)

      it "rejects a streamed body whose aggregate bytes exceed the configured limit" $
        shouldReject
          (runConsume (testLimits {multipartLimitsMaxBodyBytes = ByteString.length twoPartBody - 1}) [twoPartBody])
          MultipartBodyTooLarge

      it "rejects an oversized preamble while retaining only a boundary suffix" $
        shouldReject
          (runConsume (testLimits {multipartLimitsMaxPreambleBytes = 4}) ["too much preamble"])
          MultipartPreambleTooLarge

      it "rejects a preamble that exceeds its limit in the chunk containing the first boundary" $
        shouldReject
          ( runConsume
              (testLimits {multipartLimitsMaxPreambleBytes = 4})
              ["too much preamble--" <> boundaryToken <> "--\r\n"]
          )
          MultipartPreambleTooLarge

      it "rejects an incomplete part header block that exceeds its retained-byte limit" $
        shouldReject
          ( runConsume
              (testLimits {multipartLimitsMaxPartHeaderBytes = 4})
              ["--" <> boundaryToken <> "\r\n" <> ByteString.take 5 fieldPartHeaders]
          )
          MultipartPartHeadersTooLarge

      it "continues a retained part header block when the next body chunk arrives" $ do
        result <-
          runConsume
            testLimits
            [ "--" <> boundaryToken <> "\r\n" <> ByteString.take 5 fieldPartHeaders,
              ByteString.drop 5 fieldPartHeaders <> "\r\n\r\nvalue1\r\n--" <> boundaryToken <> "--\r\n"
            ]
        case result of
          Right [ObservedField "field1" "value1"] -> pure ()
          other -> expectationFailure ("unexpected result: " <> show other)

      it "rejects a complete part header block that exceeds its retained-byte limit" $
        shouldReject
          ( runConsume
              (testLimits {multipartLimitsMaxPartHeaderBytes = ByteString.length fieldPartHeaders - 1})
              ["--" <> boundaryToken <> "\r\n" <> fieldPartHeaders <> "\r\n\r\n"]
          )
          MultipartPartHeadersTooLarge

      it "discards an active file when a later chunk exceeds the aggregate limit" $
        withTestUploadOpener $ \openUploadFile -> do
          spooledPathReference <- IORef.newIORef Nothing
          let trackedOpener filename = do
                (path, handle) <- openUploadFile filename
                IORef.writeIORef spooledPathReference (Just path)
                pure (path, handle)
              firstChunk = "--" <> boundaryToken <> "\r\n" <> filePartHeaders <> "\r\n\r\npartial file contents"
              limits = testLimits {multipartLimitsMaxBodyBytes = ByteString.length firstChunk}
          readChunk <- chunkReader [firstChunk, " more bytes"]
          result <- withMultipartBodyWith (storageFromOpener trackedOpener) limits boundaryToken readChunk (const (pure (Right ())))
          maybeSpooledPath <- IORef.readIORef spooledPathReference
          case (result, maybeSpooledPath) of
            (Left MultipartBodyTooLarge, Just spooledPath) -> doesFileExist spooledPath `shouldReturn` False
            other -> expectationFailure ("unexpected result: " <> show other)

      it "rejects a body that declares more parts than the configured limit" $
        let fourFieldParts =
              ByteString.concat
                [ "--" <> boundaryToken <> "\r\nContent-Disposition: form-data; name=\"f" <> ByteString.singleton (toEnum (48 + n)) <> "\"\r\n\r\nv\r\n"
                | n <- [1 .. 4 :: Int]
                ]
                <> "--"
                <> boundaryToken
                <> "--\r\n"
         in shouldReject (runConsume testLimits [fourFieldParts]) MultipartTooManyParts

      it "rejects a body that declares more fields than the configured field-count limit, independent of the file count" $
        let twoFieldParts =
              ByteString.concat
                [ "--" <> boundaryToken <> "\r\nContent-Disposition: form-data; name=\"f" <> ByteString.singleton (toEnum (48 + n)) <> "\"\r\n\r\nv\r\n"
                | n <- [1 .. 2 :: Int]
                ]
                <> "--"
                <> boundaryToken
                <> "--\r\n"
         in shouldReject (runConsume (testLimits {multipartLimitsMaxFieldCount = 1}) [twoFieldParts]) MultipartTooManyFields

      it "rejects a body that declares more files than the configured file-count limit, independent of the field count" $
        let twoFileParts =
              ByteString.concat
                [ "--"
                    <> boundaryToken
                    <> "\r\nContent-Disposition: form-data; name=\"file"
                    <> ByteString.singleton (toEnum (48 + n))
                    <> "\"; filename=\"a"
                    <> ByteString.singleton (toEnum (48 + n))
                    <> ".txt\"\r\n\r\ncontent\r\n"
                | n <- [1 .. 2 :: Int]
                ]
                <> "--"
                <> boundaryToken
                <> "--\r\n"
         in shouldReject (runConsume (testLimits {multipartLimitsMaxFileCount = 1}) [twoFileParts]) MultipartTooManyFiles

      it "rejects a field value that grows past the configured limit" $
        let largeFieldBody =
              "--"
                <> boundaryToken
                <> "\r\n"
                <> fieldPartHeaders
                <> "\r\n\r\n"
                <> ByteString.replicate 2000 65
                <> "\r\n--"
                <> boundaryToken
                <> "--\r\n"
         in shouldReject (runConsume testLimits [largeFieldBody]) (MultipartFieldTooLarge "field1")

      it "rejects a file upload that grows past the configured limit" $
        let largeFileBody =
              "--"
                <> boundaryToken
                <> "\r\n"
                <> filePartHeaders
                <> "\r\n\r\n"
                <> ByteString.replicate 2000 65
                <> "\r\n--"
                <> boundaryToken
                <> "--\r\n"
         in shouldReject (runConsume testLimits [largeFileBody]) (MultipartFileTooLarge "file1")

      it "rejects a part with no Content-Disposition header" $
        let noDispositionBody =
              "--" <> boundaryToken <> "\r\nContent-Type: text/plain\r\n\r\nvalue\r\n--" <> boundaryToken <> "--\r\n"
         in shouldReject (runConsume testLimits [noDispositionBody]) MultipartMissingDisposition

      it "rejects a part whose Content-Disposition has no name parameter" $
        let noNameBody =
              "--" <> boundaryToken <> "\r\nContent-Disposition: form-data\r\n\r\nvalue\r\n--" <> boundaryToken <> "--\r\n"
         in shouldReject (runConsume testLimits [noNameBody]) MultipartMissingDisposition

      it "rejects a malformed body" $
        shouldReject (runConsume testLimits ["--" <> boundaryToken <> "XYZ"]) MultipartMalformedBody

      it "reports truncation when the body ends before any bytes of an open part's body arrive" $
        shouldReject
          (runConsume testLimits ["--" <> boundaryToken <> "\r\n" <> fieldPartHeaders <> "\r\n\r\n"])
          MultipartTruncatedBody

      it "reports truncation when the body ends mid-part, after some body bytes arrived" $
        shouldReject
          (runConsume testLimits ["--" <> boundaryToken <> "\r\n" <> fieldPartHeaders <> "\r\n\r\npartial"])
          MultipartTruncatedBody

      it "flushes a delimiter-sized body suffix before reporting truncation" $
        shouldReject
          (runConsume testLimits ["--" <> boundaryToken <> "\r\n" <> fieldPartHeaders <> "\r\n\r\n\r"])
          MultipartTruncatedBody

      it "derives comparable, printable representations for MultipartConsumeError" $
        let errors =
              [ MultipartBodyTooLarge,
                MultipartPreambleTooLarge,
                MultipartPartHeadersTooLarge,
                MultipartTooManyParts,
                MultipartTooManyFields,
                MultipartTooManyFiles,
                MultipartMissingDisposition,
                MultipartFieldTooLarge "f",
                MultipartFileTooLarge "f",
                MultipartMalformedBody,
                MultipartTruncatedBody,
                MultipartInvalidContentType,
                MultipartDeclaredBodyTooLarge
              ]
         in expectAll
              ( (sum [fromEnum (left == right) | left <- errors, right <- errors] `shouldBe` length errors)
                  :| [ sum [fromEnum (left /= right) | left <- errors, right <- errors]
                         `shouldBe` length errors * (length errors - 1),
                       sum [length (show e) + length (showList [e] "") | e <- errors] `shouldSatisfy` (> 0),
                       sum [fromEnum (left == right) | left <- errors, right <- errors] `shouldBe` length errors,
                       sum [fromEnum (left /= right) | left <- errors, right <- errors]
                         `shouldBe` length errors * (length errors - 1),
                       sum [length (show e) + length (showList [e] "") | e <- errors] `shouldSatisfy` (> 0)
                     ]
              )

      it "derives comparable, printable representations for MultipartLimits" $
        let limitsValues = [testLimits, defaultMultipartLimits]
         in expectAll
              ( (sum [fromEnum (left == right) | left <- limitsValues, right <- limitsValues] `shouldBe` length limitsValues)
                  :| [ sum [fromEnum (left /= right) | left <- limitsValues, right <- limitsValues]
                         `shouldBe` length limitsValues * (length limitsValues - 1),
                       sum [length (show l) + length (showList [l] "") | l <- limitsValues] `shouldSatisfy` (> 0)
                     ]
              )

    describe "withMultipartRequestBodyWith" $ do
      it "parses only a strict multipart media type with one valid boundary" $
        expectAll
          ( (multipartBoundaryFromContentType "MULTIPART/FORM-DATA ; charset=utf-8 ; BOUNDARY=\"Aa0'()+_,-./:=?Zz\"" `shouldBe` Right "Aa0'()+_,-./:=?Zz")
              :| [ multipartBoundaryFromContentType "" `shouldBe` Left MultipartInvalidContentType,
                   multipartBoundaryFromContentType "text/plain; boundary=valid" `shouldBe` Left MultipartInvalidContentType,
                   multipartBoundaryFromContentType "multipart/form-data" `shouldBe` Left MultipartInvalidContentType,
                   multipartBoundaryFromContentType "multipart/form-data; boundary" `shouldBe` Left MultipartInvalidContentType,
                   multipartBoundaryFromContentType "multipart/form-data; =valid" `shouldBe` Left MultipartInvalidContentType,
                   multipartBoundaryFromContentType "multipart/form-data; boundary=" `shouldBe` Left MultipartInvalidContentType,
                   multipartBoundaryFromContentType "multipart/form-data; boundary=valid; boundary=other" `shouldBe` Left MultipartInvalidContentType,
                   multipartBoundaryFromContentType "multipart/form-data; boundary=\"valid" `shouldBe` Left MultipartInvalidContentType,
                   multipartBoundaryFromContentType "multipart/form-data; boundary=va\"lid" `shouldBe` Left MultipartInvalidContentType,
                   multipartBoundaryFromContentType "multipart/form-data; boundary=va\\lid" `shouldBe` Left MultipartInvalidContentType,
                   multipartBoundaryFromContentType "multipart/form-data; boundary=\"va\"lid\"" `shouldBe` Left MultipartInvalidContentType,
                   multipartBoundaryFromContentType "multipart/form-data; boundary=\"va\\lid\"" `shouldBe` Left MultipartInvalidContentType,
                   multipartBoundaryFromContentType "multipart/form-data; boundary=contains space" `shouldBe` Left MultipartInvalidContentType,
                   multipartBoundaryFromContentType ("multipart/form-data; boundary=" <> ByteString.replicate 71 97) `shouldBe` Left MultipartInvalidContentType
                 ]
          )

      it "consumes a WAI request's body with the bounded in-memory storage adapter" $ do
        readChunk <- chunkReader [twoPartBody]
        let request = multipartRequest readChunk []
        result <- runRequestConsume defaultMultipartLimits request
        case result of
          Right [ObservedField "field1" "value1", ObservedFile "file1" "a.txt" upload byteCount] ->
            expectAll
              ( (upload `shouldBe` "file content here")
                  :| [byteCount `shouldBe` ByteString.length "file content here"]
              )
          other -> expectationFailure ("unexpected result: " <> show other)

      it "rejects an invalid request media type before reading its body" $ do
        bodyReadReference <- IORef.newIORef False
        let readChunk = IORef.writeIORef bodyReadReference True >> pure twoPartBody
            request = multipartRequest readChunk [(Http.hContentType, "text/plain")]
        result <- runRequestConsume defaultMultipartLimits request
        bodyRead <- IORef.readIORef bodyReadReference
        expectAll
          ( (result `shouldSatisfy` \case Left MultipartInvalidContentType -> True; _ -> False)
              :| [bodyRead `shouldBe` False]
          )

      it "rejects a missing media type before reading and treats a malformed declared length as unavailable" $ do
        missingTypeReadReference <- IORef.newIORef False
        malformedLengthReadReference <- IORef.newIORef False
        let missingTypeReader = IORef.writeIORef missingTypeReadReference True >> pure twoPartBody
            malformedLengthReader = IORef.writeIORef malformedLengthReadReference True >> pure twoPartBody
            missingTypeRequest = Wai.setRequestBodyChunks missingTypeReader Wai.defaultRequest
            malformedLengthRequest = multipartRequest malformedLengthReader [(Http.hContentLength, "11x")]
        missingTypeResult <- runRequestConsume defaultMultipartLimits missingTypeRequest
        malformedLengthResult <- runRequestConsume defaultMultipartLimits malformedLengthRequest
        missingTypeRead <- IORef.readIORef missingTypeReadReference
        malformedLengthRead <- IORef.readIORef malformedLengthReadReference
        expectAll
          ( (missingTypeResult `shouldSatisfy` \case Left MultipartInvalidContentType -> True; _ -> False)
              :| [ malformedLengthResult `shouldSatisfy` \case Right _ -> True; Left _ -> False,
                   missingTypeRead `shouldBe` False,
                   malformedLengthRead `shouldBe` True
                 ]
          )

      it "rejects declared oversized bodies, including values beyond machine integer range, before reading" $ do
        bodyReadReference <- IORef.newIORef False
        hugeBodyReadReference <- IORef.newIORef False
        let limits = defaultMultipartLimits {multipartLimitsMaxBodyBytes = 10}
            readChunk = IORef.writeIORef bodyReadReference True >> pure twoPartBody
            hugeReadChunk = IORef.writeIORef hugeBodyReadReference True >> pure twoPartBody
            request = multipartRequest readChunk [(Http.hContentLength, "11")]
            hugeRequest = multipartRequest hugeReadChunk [(Http.hContentLength, "999999999999999999999999999999999999999999999999999999999999")]
        result <- runRequestConsume limits request
        hugeResult <- runRequestConsume limits hugeRequest
        bodyRead <- IORef.readIORef bodyReadReference
        hugeBodyRead <- IORef.readIORef hugeBodyReadReference
        expectAll
          ( (result `shouldSatisfy` \case Left MultipartDeclaredBodyTooLarge -> True; _ -> False)
              :| [ hugeResult `shouldSatisfy` \case Left MultipartDeclaredBodyTooLarge -> True; _ -> False,
                   bodyRead `shouldBe` False,
                   hugeBodyRead `shouldBe` False
                 ]
          )

    describe "withMultipartBodyWith" $ do
      it "discards an unpromoted scoped upload after a successful callback" $
        withTestUploadOpener $ \openUploadFile -> do
          discardedReference <- IORef.newIORef False
          readChunk <- chunkReader [twoPartBody]
          let storage =
                multipartStorage
                  ( \filename -> do
                      (path, handle) <- openUploadFile filename
                      pure (multipartStagedUpload (ByteString.hPut handle) (hClose handle >> pure path) (hClose handle >> removeFile path))
                  )
                  (Just (\path -> IORef.writeIORef discardedReference True >> removeFile path))
          result <-
            withMultipartBodyWith storage defaultMultipartLimits boundaryToken readChunk $ \case
              MultipartScopedFieldPart _ _ -> pure (Right ())
              MultipartScopedFilePart {} -> pure (Right ())
          discarded <- IORef.readIORef discardedReference
          expectAll ((result `shouldBe` Right ()) :| [discarded `shouldBe` True])

      it "discards scoped uploads when a callback rejects" $
        withTestUploadOpener $ \openUploadFile -> do
          discardedReference <- IORef.newIORef False
          readChunk <- chunkReader [twoPartBody]
          let storage =
                multipartStorage
                  ( \filename -> do
                      (path, handle) <- openUploadFile filename
                      pure (multipartStagedUpload (ByteString.hPut handle) (hClose handle >> pure path) (hClose handle >> removeFile path))
                  )
                  (Just (\path -> IORef.writeIORef discardedReference True >> removeFile path))
          result <-
            withMultipartBodyWith storage defaultMultipartLimits boundaryToken readChunk $ \case
              MultipartScopedFieldPart _ _ -> pure (Right ())
              MultipartScopedFilePart {} -> pure (Left MultipartMalformedBody)
          discarded <- IORef.readIORef discardedReference
          expectAll ((result `shouldBe` Left MultipartMalformedBody) :| [discarded `shouldBe` True])

      it "keeps a promoted scoped upload under the application's ownership" $
        withTestUploadOpener $ \openUploadFile -> do
          promotedReference <- IORef.newIORef Nothing
          readChunk <- chunkReader [twoPartBody]
          result <-
            withMultipartBodyWith (storageFromOpener openUploadFile) defaultMultipartLimits boundaryToken readChunk $ \case
              MultipartScopedFieldPart fieldName value -> do
                fieldName `shouldBe` "field1"
                value `shouldBe` "value1"
                pure (Right ())
              MultipartScopedFilePart fieldName filename upload byteCount -> do
                fieldName `shouldBe` "file1"
                filename `shouldBe` "a.txt"
                promoted <- promoteMultipartUpload upload
                IORef.writeIORef promotedReference promoted
                byteCount `shouldBe` ByteString.length "file content here"
                promoteMultipartUpload upload `shouldReturn` Nothing
                pure (Right ())
          maybePath <- IORef.readIORef promotedReference
          case maybePath of
            Nothing -> expectationFailure "expected the callback to promote its upload"
            Just path -> do
              contents <- ByteString.readFile path
              removeFile path
              expectAll ((result `shouldBe` Right ()) :| [contents `shouldBe` "file content here"])

      it "discards scoped uploads when a callback throws" $
        withTestUploadOpener $ \openUploadFile -> do
          discardedReference <- IORef.newIORef False
          readChunk <- chunkReader [twoPartBody]
          let storage =
                multipartStorage
                  ( \filename -> do
                      (path, handle) <- openUploadFile filename
                      pure (multipartStagedUpload (ByteString.hPut handle) (hClose handle >> pure path) (hClose handle >> removeFile path))
                  )
                  (Just (\path -> IORef.writeIORef discardedReference True >> removeFile path))
          _ :: Either Exception.SomeException (Either MultipartConsumeError ()) <-
            Exception.try
              ( withMultipartBodyWith storage defaultMultipartLimits boundaryToken readChunk $ \case
                  MultipartScopedFieldPart _ _ -> pure (Right ())
                  MultipartScopedFilePart {} -> Exception.throwIO (userError "callback failure")
              )
          IORef.readIORef discardedReference `shouldReturn` True

      it "calls onPart for each completed part, in order, before the body finishes" $
        withTestUploadOpener $ \openUploadFile -> do
          seenPartsRef <- IORef.newIORef []
          readChunk <- chunkReader [twoPartBody]
          result <-
            withMultipartBodyWith (storageFromOpener openUploadFile) defaultMultipartLimits boundaryToken readChunk $ \case
              MultipartScopedFieldPart fieldName value -> do
                IORef.modifyIORef' seenPartsRef (ObservedField fieldName value :)
                pure (Right ())
              MultipartScopedFilePart fieldName filename _upload byteCount -> do
                IORef.modifyIORef' seenPartsRef (ObservedFile fieldName filename ByteString.empty byteCount :)
                pure (Right ())
          seenParts <- reverse <$> IORef.readIORef seenPartsRef
          expectAll
            ( (result `shouldSatisfy` \case Right () -> True; Left _ -> False)
                :| [ case seenParts of
                       [ObservedField "field1" "value1", ObservedFile "file1" "a.txt" _ _] -> pure ()
                       other -> expectationFailure ("unexpected parts: " <> show other)
                   ]
            )

      it "stops reading after a rejecting callback, leaving the unread body to the transport" $ do
        readCountReference <- IORef.newIORef (0 :: Int)
        chunksReference <-
          IORef.newIORef
            [ "--" <> boundaryToken <> "\r\n" <> fieldPartHeaders <> "\r\n\r\nvalue1\r\n--" <> boundaryToken <> "\r\n",
              filePartHeaders <> "\r\n\r\nfile content here\r\n--" <> boundaryToken <> "--\r\n"
            ]
        let readChunk = do
              IORef.modifyIORef' readCountReference (+ 1)
              IORef.atomicModifyIORef' chunksReference $ \case
                [] -> ([], ByteString.empty)
                chunk : rest -> (rest, chunk)
        result <-
          withMultipartBodyWith inMemoryMultipartStorage defaultMultipartLimits boundaryToken readChunk $ \case
            MultipartScopedFieldPart "field1" _ -> pure (Left MultipartMalformedBody)
            _ -> expectationFailure "did not expect another multipart part" >> pure (Right ())
        readCount <- IORef.readIORef readCountReference
        expectAll
          ( (result `shouldBe` Left MultipartMalformedBody)
              :| [readCount `shouldBe` 1]
          )

      it "retains an application-owned completed upload when a later part is rejected" $
        withTestUploadOpener $ \openUploadFile -> do
          spooledPathRef <- IORef.newIORef Nothing
          readChunk <- chunkReader [fileTheRejectsSecondFieldBody]
          result <-
            withMultipartBodyWith (storageFromOpener openUploadFile) defaultMultipartLimits boundaryToken readChunk $ \case
              MultipartScopedFilePart _ _ upload _ -> do
                spooledPath <- promoteMultipartUpload upload
                IORef.writeIORef spooledPathRef spooledPath
                pure (Right ())
              MultipartScopedFieldPart {} -> pure (Left MultipartMalformedBody)
          maybeSpooledPath <- IORef.readIORef spooledPathRef
          case maybeSpooledPath of
            Nothing -> expectationFailure "expected the file part to have been spooled before rejection"
            Just spooledPath -> do
              spooledContent <- ByteString.readFile spooledPath
              expectAll
                ( (result `shouldBe` Left MultipartMalformedBody)
                    :| [spooledContent `shouldBe` "file content here"]
                )

    describe "withMultipartRequestBodyWith" $ do
      it "uses scoped request ownership and rejects invalid metadata before its callback" $ do
        bodyReadReference <- IORef.newIORef False
        let readChunk = IORef.writeIORef bodyReadReference True >> pure twoPartBody
            request = Wai.setRequestBodyChunks readChunk Wai.defaultRequest
        result <- withMultipartRequestBodyWith defaultMultipartLimits request (const (pure (Right ())))
        bodyRead <- IORef.readIORef bodyReadReference
        expectAll ((result `shouldBe` Left MultipartInvalidContentType) :| [bodyRead `shouldBe` False])

      it "uses scoped request ownership for a valid in-memory upload" $ do
        readChunk <- chunkReader [twoPartBody]
        let request =
              multipartRequest
                readChunk
                [(Http.hContentLength, ByteStringChar8.pack (show (ByteString.length twoPartBody)))]
        result <-
          withMultipartRequestBodyWith defaultMultipartLimits request $ \case
            MultipartScopedFieldPart fieldName value -> do
              fieldName `shouldBe` "field1"
              value `shouldBe` "value1"
              pure (Right ())
            MultipartScopedFilePart fieldName filename upload byteCount -> do
              fieldName `shouldBe` "file1"
              filename `shouldBe` "a.txt"
              byteCount `shouldBe` ByteString.length "file content here"
              discardMultipartUpload upload
              pure (Right ())
        result `shouldBe` Right ()

      it "lets a WAI request select and promote an application storage adapter" $
        withTestUploadOpener $ \openUploadFile -> do
          promotedReference <- IORef.newIORef Nothing
          readChunk <- chunkReader [twoPartBody]
          let request = multipartRequest readChunk []
          result <-
            withMultipartRequestBodyWithStorage
              (storageFromOpener openUploadFile)
              defaultMultipartLimits
              request
              $ \case
                MultipartScopedFieldPart _ _ -> pure (Right ())
                MultipartScopedFilePart _ _ upload _ -> do
                  promoted <- promoteMultipartUpload upload
                  IORef.writeIORef promotedReference promoted
                  pure (Right ())
          maybePath <- IORef.readIORef promotedReference
          case maybePath of
            Nothing -> expectationFailure "expected the callback to promote its upload"
            Just path -> do
              contents <- ByteString.readFile path
              removeFile path
              expectAll ((result `shouldBe` Right ()) :| [contents `shouldBe` "file content here"])

      it "consumes a WAI request's body incrementally, calling onPart for each part" $
        withTestUploadOpener $ \_unusedOpener -> do
          seenPartsRef <- IORef.newIORef []
          readChunk <- chunkReader [twoPartBody]
          let request = multipartRequest readChunk []
          result <-
            withMultipartRequestBodyWith defaultMultipartLimits request $ \case
              MultipartScopedFieldPart fieldName value -> do
                IORef.modifyIORef' seenPartsRef (ObservedField fieldName value :)
                pure (Right ())
              MultipartScopedFilePart fieldName filename _upload byteCount -> do
                IORef.modifyIORef' seenPartsRef (ObservedFile fieldName filename ByteString.empty byteCount :)
                pure (Right ())
          seenParts <- reverse <$> IORef.readIORef seenPartsRef
          expectAll
            ( (result `shouldSatisfy` \case Right () -> True; Left _ -> False)
                :| [ case seenParts of
                       [ObservedField "field1" "value1", ObservedFile "file1" "a.txt" _ _] -> pure ()
                       other -> expectationFailure ("unexpected parts: " <> show other)
                   ]
            )

      it "rejects request metadata before calling a streaming callback or reading the body" $ do
        bodyReadReference <- IORef.newIORef False
        callbackCalledReference <- IORef.newIORef False
        let readChunk = IORef.writeIORef bodyReadReference True >> pure twoPartBody
            request = Wai.setRequestBodyChunks readChunk Wai.defaultRequest
        result <-
          withMultipartRequestBodyWith defaultMultipartLimits request $ \_part -> do
            IORef.writeIORef callbackCalledReference True
            pure (Right ())
        bodyRead <- IORef.readIORef bodyReadReference
        callbackCalled <- IORef.readIORef callbackCalledReference
        expectAll
          ( (result `shouldBe` Left MultipartInvalidContentType)
              :| [bodyRead `shouldBe` False, callbackCalled `shouldBe` False]
          )

      it "applies the declared-size limit before calling a streaming callback" $ do
        bodyReadReference <- IORef.newIORef False
        let limits = defaultMultipartLimits {multipartLimitsMaxBodyBytes = 10}
            readChunk = IORef.writeIORef bodyReadReference True >> pure twoPartBody
            request = multipartRequest readChunk [(Http.hContentLength, "11")]
        result <- withMultipartRequestBodyWith limits request (const (pure (Right ())))
        bodyRead <- IORef.readIORef bodyReadReference
        expectAll
          ( (result `shouldBe` Left MultipartDeclaredBodyTooLarge)
              :| [bodyRead `shouldBe` False]
          )

multipartRequest :: IO ByteString -> [(Http.HeaderName, ByteString)] -> Wai.Request
multipartRequest readChunk additionalHeaders =
  Wai.setRequestBodyChunks
    readChunk
    Wai.defaultRequest
      { Wai.requestHeaders = additionalHeaders <> [(Http.hContentType, "multipart/form-data; boundary=" <> boundaryToken)]
      }
