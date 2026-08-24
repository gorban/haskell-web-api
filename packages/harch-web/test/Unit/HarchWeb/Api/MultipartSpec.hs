{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{-# SPEC #-}

import Control.Concurrent (forkIO, myThreadId)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception qualified as Exception
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteStringChar8
import Data.IORef qualified as IORef
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Api.Multipart
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import System.Directory (doesFileExist, removeFile)
import System.IO (Handle, hClose)
import System.IO.Temp qualified as Temp

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
        (path, handle) <- openUploadFile (untrustedFilenameText filename)
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

testByteLimit :: Int -> MultipartByteLimit
testByteLimit = multipartByteLimit . fromIntegral

testItemLimit :: Int -> MultipartItemLimit
testItemLimit = multipartItemLimit . fromIntegral

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
            IORef.modifyIORef' observedPartsReference (ObservedFile fieldName (untrustedFilenameText filename) (inMemoryUploadBytes inMemoryUpload) byteCount :)
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
            IORef.modifyIORef' observedPartsReference (ObservedFile fieldName (untrustedFilenameText filename) (inMemoryUploadBytes inMemoryUpload) byteCount :)
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
    { multipartLimitsMaxBodyBytes = testByteLimit 4096,
      multipartLimitsMaxFieldBytes = testByteLimit 1024,
      multipartLimitsMaxFileBytes = testByteLimit 1024,
      multipartLimitsMaxParts = testItemLimit 3
    }

singleFieldBody :: ByteString
singleFieldBody =
  "--" <> boundaryToken <> "\r\n" <> fieldPartHeaders <> "\r\n\r\n" <> "value1" <> "\r\n--" <> boundaryToken <> "--\r\n"

allByteChunks :: ByteString -> [ByteString]
allByteChunks body = map ByteString.singleton (ByteString.unpack body)

spec =
  describe "HarchWeb.Api.Multipart" $ do
    it "rejects a callback part with the parser-owned generic rejection" $
      rejectMultipartPart `shouldReturn` Left MultipartMalformedBody

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
              MultipartScopedFilePart "file1" filename upload byteCount
                | untrustedFilenameText filename == "a.txt" -> do
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

      it "keeps client filenames opaque until a storage adapter explicitly reveals metadata" $ do
        filenameReference <- IORef.newIORef []
        revealedFilenameReference <- IORef.newIORef []
        callbackFilenameReference <- IORef.newIORef Nothing
        chunksReference <- IORef.newIORef []
        let untrustedFilename :: Text
            untrustedFilename = "../unsafe.txt"
            untrustedFilenameBody =
              "--"
                <> boundaryToken
                <> "\r\nContent-Disposition: form-data; name=\"file1\"; filename=\""
                <> TextEncoding.encodeUtf8 untrustedFilename
                <> "\"\r\n\r\nfile content here\r\n--"
                <> boundaryToken
                <> "--\r\n"
        readChunk <- chunkReader [untrustedFilenameBody]
        let storage =
              multipartStorage
                ( \filename -> do
                    IORef.modifyIORef' filenameReference (filename :)
                    IORef.modifyIORef' revealedFilenameReference (untrustedFilenameText filename :)
                    pure $
                      multipartStagedUpload
                        (\chunk -> IORef.modifyIORef' chunksReference (chunk :))
                        (pure ())
                        (IORef.modifyIORef' chunksReference (const []))
                )
                Nothing
        result <-
          withMultipartBodyWith storage defaultMultipartLimits boundaryToken readChunk $ \case
            MultipartScopedFieldPart {} -> pure (Right ())
            MultipartScopedFilePart _ filename _upload _ -> do
              IORef.writeIORef callbackFilenameReference (Just filename)
              pure (Right ())
        receivedFilenames <- IORef.readIORef filenameReference
        revealedFilenames <- IORef.readIORef revealedFilenameReference
        maybeCallbackFilename <- IORef.readIORef callbackFilenameReference
        case (receivedFilenames, maybeCallbackFilename) of
          ([storageFilename], Just callbackFilename) ->
            expectAll
              ( (result `shouldBe` Right ())
                  :| [ untrustedFilenameText storageFilename `shouldBe` untrustedFilename,
                       revealedFilenames `shouldBe` [untrustedFilename],
                       storageFilename == callbackFilename `shouldBe` True,
                       storageFilename /= callbackFilename `shouldBe` False
                     ]
              )
          other -> expectationFailure ("unexpected filename metadata: " <> show (fmap untrustedFilenameText (snd other), fmap untrustedFilenameText (fst other)))

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

      it "discards a file when cancellation is requested during staging before active-upload bookkeeping completes" $
        withTestUploadOpener $ \openUploadFile -> do
          spooledPathReference <- IORef.newIORef Nothing
          openedSignal <- newEmptyMVar
          nextChunkSignal <- newEmptyMVar
          nextChunkReference <- IORef.newIORef (Just ("--" <> boundaryToken <> "\r\n" <> filePartHeaders <> "\r\n\r\nfile contents"))
          callingThreadId <- myThreadId
          let signalingOpener filename = do
                (path, handle) <- openUploadFile filename
                IORef.writeIORef spooledPathReference (Just path)
                putMVar openedSignal ()
                pure (path, handle)
              readChunk = do
                maybeChunk <- IORef.atomicModifyIORef' nextChunkReference (Nothing,)
                case maybeChunk of
                  Just chunk -> pure chunk
                  -- Once this interruptible read wait begins, the cancellation
                  -- requested after staging must arrive only after the masked
                  -- staging/bookkeeping hand-off made cleanup own the upload.
                  Nothing -> takeMVar nextChunkSignal
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
          (runConsume (testLimits {multipartLimitsMaxBodyBytes = testByteLimit (ByteString.length twoPartBody - 1)}) [twoPartBody])
          MultipartBodyTooLarge

      it "rejects an oversized preamble while retaining only a boundary suffix" $
        shouldReject
          (runConsume (testLimits {multipartLimitsMaxPreambleBytes = testByteLimit 4}) ["too much preamble"])
          MultipartPreambleTooLarge

      it "rejects a preamble that exceeds its limit in the chunk containing the first boundary" $
        shouldReject
          ( runConsume
              (testLimits {multipartLimitsMaxPreambleBytes = testByteLimit 4})
              ["too much preamble--" <> boundaryToken <> "--\r\n"]
          )
          MultipartPreambleTooLarge

      it "rejects an incomplete part header block that exceeds its retained-byte limit" $
        shouldReject
          ( runConsume
              (testLimits {multipartLimitsMaxPartHeaderBytes = testByteLimit 4})
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
              (testLimits {multipartLimitsMaxPartHeaderBytes = testByteLimit (ByteString.length fieldPartHeaders - 1)})
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
              limits = testLimits {multipartLimitsMaxBodyBytes = testByteLimit (ByteString.length firstChunk)}
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
         in shouldReject (runConsume (testLimits {multipartLimitsMaxFieldCount = testItemLimit 1}) [twoFieldParts]) MultipartTooManyFields

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
         in shouldReject (runConsume (testLimits {multipartLimitsMaxFileCount = testItemLimit 1}) [twoFileParts]) MultipartTooManyFiles

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
                         `shouldBe` length errors
                         * (length errors - 1),
                       sum [length (show e) + length (showList [e] "") | e <- errors] `shouldSatisfy` (> 0),
                       sum [fromEnum (left == right) | left <- errors, right <- errors] `shouldBe` length errors,
                       sum [fromEnum (left /= right) | left <- errors, right <- errors]
                         `shouldBe` length errors
                         * (length errors - 1),
                       sum [length (show e) + length (showList [e] "") | e <- errors] `shouldSatisfy` (> 0)
                     ]
              )

      it "derives comparable, printable representations for MultipartLimits" $
        let limitsValues = [testLimits, defaultMultipartLimits]
         in expectAll
              ( (sum [fromEnum (left == right) | left <- limitsValues, right <- limitsValues] `shouldBe` length limitsValues)
                  :| [ sum [fromEnum (left /= right) | left <- limitsValues, right <- limitsValues]
                         `shouldBe` length limitsValues
                         * (length limitsValues - 1),
                       sum [length (show l) + length (showList [l] "") | l <- limitsValues] `shouldSatisfy` (> 0)
                     ]
              )

      it "keeps every multipart byte and item limit non-negative" $
        expectAll
          ( (multipartByteLimitFromInt (-1) `shouldBe` Nothing)
              :| [ multipartItemLimitFromInt (-1) `shouldBe` Nothing,
                   multipartByteLimitValue (multipartByteLimit 7) `shouldBe` 7,
                   multipartItemLimitValue (multipartItemLimit 3) `shouldBe` 3,
                   multipartByteLimitFromInt 0 `shouldBe` Just (multipartByteLimit 0),
                   multipartItemLimitFromInt 0 `shouldBe` Just (multipartItemLimit 0)
                 ]
          )

      it "keeps typed multipart limits comparable and printable" $
        let byteLimit = multipartByteLimit 7
            otherByteLimit = multipartByteLimit 8
            itemLimit = multipartItemLimit 3
            otherItemLimit = multipartItemLimit 4
         in expectAll
              ( (byteLimit /= otherByteLimit `shouldBe` True)
                  :| [ show byteLimit `shouldSatisfy` (not . null),
                       showList [byteLimit] "" `shouldSatisfy` (not . null),
                       itemLimit /= otherItemLimit `shouldBe` True,
                       show itemLimit `shouldSatisfy` (not . null),
                       showList [itemLimit] "" `shouldSatisfy` (not . null)
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
        let limits = defaultMultipartLimits {multipartLimitsMaxBodyBytes = testByteLimit 10}
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
                      (path, handle) <- openUploadFile (untrustedFilenameText filename)
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
                      (path, handle) <- openUploadFile (untrustedFilenameText filename)
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
                untrustedFilenameText filename `shouldBe` "a.txt"
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
                      (path, handle) <- openUploadFile (untrustedFilenameText filename)
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
                IORef.modifyIORef' seenPartsRef (ObservedFile fieldName (untrustedFilenameText filename) ByteString.empty byteCount :)
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
              untrustedFilenameText filename `shouldBe` "a.txt"
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
                IORef.modifyIORef' seenPartsRef (ObservedFile fieldName (untrustedFilenameText filename) ByteString.empty byteCount :)
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
        let limits = defaultMultipartLimits {multipartLimitsMaxBodyBytes = testByteLimit 10}
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
