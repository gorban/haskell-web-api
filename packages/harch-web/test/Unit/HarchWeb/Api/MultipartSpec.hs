{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWeb.Api.MultipartSpec (spec) where

import Control.Exception qualified as Exception
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.IORef qualified as IORef
import Data.List (mapAccumL)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import HarchWeb.Api.Multipart
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

runConsume :: MultipartLimits -> [ByteString] -> IO (Either MultipartConsumeError [MultipartPart])
runConsume limits chunks =
  do
    readChunk <- chunkReader chunks
    consumeMultipartBody inMemoryMultipartStorage limits boundaryToken readChunk

shouldReject :: IO (Either MultipartConsumeError [MultipartPart]) -> MultipartConsumeError -> Expectation
shouldReject action expectedError = do
  result <- action
  case result of
    Left actualError -> actualError `shouldBe` expectedError
    Right parts -> expectationFailure ("expected rejection, received: " <> show parts)

testLimits :: MultipartLimits
testLimits =
  defaultMultipartLimits
    { multipartLimitsMaxFieldBytes = 1024,
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
              MultipartMalformed
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

    describe "consumeMultipartBody" $ do
      it "keeps the built-in storage adapter in memory within the file budget" $ do
        result <- runConsume testLimits [twoPartBody]
        equivalentResult <- runConsume testLimits [twoPartBody]
        case (result, equivalentResult) of
          ( Right [MultipartFieldPart "field1" "value1", MultipartFilePart "file1" "a.txt" upload byteCount],
            Right [MultipartFieldPart "field1" "value1", MultipartFilePart "file1" "a.txt" equivalentUpload _]
            ) ->
              expectAll
                ( (inMemoryUploadBytes upload `shouldBe` "file content here")
                    :| [ byteCount `shouldBe` ByteString.length "file content here",
                         inMemoryUploadBytes upload `shouldBe` inMemoryUploadBytes equivalentUpload,
                         show upload `shouldSatisfy` (not . null),
                         showList [upload] "" `shouldSatisfy` (not . null)
                       ]
                )
          results -> expectationFailure ("unexpected results: " <> show results)

      it "consumes a single field part into a MultipartFieldPart" $ do
        result <- runConsume testLimits [singleFieldBody]
        case result of
          Right [MultipartFieldPart "field1" "value1"] -> pure ()
          other -> expectationFailure ("unexpected result: " <> show other)

      it "uses an application-supplied storage adapter for a file part" $
        withTestUploadOpener $ \openUploadFile -> do
          readChunk <- chunkReader [twoPartBody]
          result <- consumeMultipartBody (storageFromOpener openUploadFile) testLimits boundaryToken readChunk
          case result of
            Right [MultipartFieldPart "field1" "value1", MultipartFilePart "file1" "a.txt" tempPath byteCount] -> do
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
          result <- consumeMultipartBody (storageFromOpener trackedOpener) testLimits boundaryToken readChunk
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
          result <- consumeMultipartBody (storageFromOpener trackedOpener) testLimits boundaryToken readChunk
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
                maybeChunk <- IORef.atomicModifyIORef' nextChunkReference (\nextChunk -> (Nothing, nextChunk))
                case maybeChunk of
                  Just chunk -> pure chunk
                  Nothing -> Exception.throwIO (userError "request body read failed")
          attempt :: Either Exception.SomeException (Either MultipartConsumeError [MultipartPartWith FilePath]) <-
            Exception.try (consumeMultipartBody (storageFromOpener trackedOpener) testLimits boundaryToken failingReadChunk)
          maybeSpooledPath <- IORef.readIORef spooledPathReference
          case (attempt, maybeSpooledPath) of
            (Left _, Just spooledPath) -> doesFileExist spooledPath `shouldReturn` False
            other -> expectationFailure ("unexpected result: " <> show other)

      it "consumes a body delivered one byte at a time, exercising the multi-chunk driving loop" $ do
        result <- runConsume testLimits (allByteChunks singleFieldBody)
        case result of
          Right [MultipartFieldPart "field1" "value1"] -> pure ()
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

      it "derives comparable, printable representations for MultipartPart and MultipartConsumeError" $
        let parts :: [MultipartPartWith FilePath]
            parts = [MultipartFieldPart "f" "v", MultipartFilePart "f" "n" "/tmp/x" 3]
            errors =
              [ MultipartTooManyParts,
                MultipartMissingDisposition,
                MultipartFieldTooLarge "f",
                MultipartFileTooLarge "f",
                MultipartMalformedBody,
                MultipartTruncatedBody
              ]
         in expectAll
              ( (sum [fromEnum (left == right) | left <- parts, right <- parts] `shouldBe` length parts)
                  :| [ sum [fromEnum (left /= right) | left <- parts, right <- parts]
                         `shouldBe` length parts * (length parts - 1),
                       sum [length (show p) + length (showList [p] "") | p <- parts] `shouldSatisfy` (> 0),
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

    describe "consumeMultipartRequestBody" $
      it "consumes a WAI request's body with the bounded in-memory storage adapter" $ do
        readChunk <- chunkReader [twoPartBody]
        let request = Wai.setRequestBodyChunks readChunk Wai.defaultRequest
        result <- consumeMultipartRequestBody defaultMultipartLimits boundaryToken request
        case result of
          Right [MultipartFieldPart "field1" "value1", MultipartFilePart "file1" "a.txt" upload byteCount] ->
            expectAll
              ( (inMemoryUploadBytes upload `shouldBe` "file content here")
                  :| [byteCount `shouldBe` ByteString.length "file content here"]
              )
          other -> expectationFailure ("unexpected result: " <> show other)

    describe "consumeMultipartBodyWith" $ do
      it "calls onPart for each completed part, in order, before the body finishes" $
        withTestUploadOpener $ \openUploadFile -> do
          seenPartsRef <- IORef.newIORef []
          readChunk <- chunkReader [twoPartBody]
          result <-
            consumeMultipartBodyWith (storageFromOpener openUploadFile) defaultMultipartLimits boundaryToken readChunk $ \part -> do
              IORef.modifyIORef' seenPartsRef (part :)
              pure (Right ())
          seenParts <- reverse <$> IORef.readIORef seenPartsRef
          expectAll
            ( (result `shouldSatisfy` \case Right () -> True; Left _ -> False)
                :| [ case seenParts of
                       [MultipartFieldPart "field1" "value1", MultipartFilePart "file1" "a.txt" _ _] -> pure ()
                       other -> expectationFailure ("unexpected parts: " <> show other)
                   ]
            )

      it "aborts with a rejecting callback's error before a later file part is ever opened" $ do
        uploadOpenedRef <- IORef.newIORef False
        readChunk <- chunkReader [twoPartBody]
        result <-
          consumeMultipartBodyWith inMemoryMultipartStorage defaultMultipartLimits boundaryToken readChunk $ \case
            MultipartFieldPart "field1" _ -> pure (Left MultipartMalformedBody)
            part -> expectationFailure ("did not expect to see " <> show part) >> pure (Right ())
        uploadOpened <- IORef.readIORef uploadOpenedRef
        expectAll
          ( (result `shouldBe` Left MultipartMalformedBody)
              :| [uploadOpened `shouldBe` False]
          )

      it "retains an application-owned completed upload when a later part is rejected" $
        withTestUploadOpener $ \openUploadFile -> do
          spooledPathRef <- IORef.newIORef Nothing
          readChunk <- chunkReader [fileTheRejectsSecondFieldBody]
          result <-
            consumeMultipartBodyWith (storageFromOpener openUploadFile) defaultMultipartLimits boundaryToken readChunk $ \case
              MultipartFilePart _ _ spooledPath _ -> do
                IORef.writeIORef spooledPathRef (Just spooledPath)
                pure (Right ())
              MultipartFieldPart {} -> pure (Left MultipartMalformedBody)
          maybeSpooledPath <- IORef.readIORef spooledPathRef
          case maybeSpooledPath of
            Nothing -> expectationFailure "expected the file part to have been spooled before rejection"
            Just spooledPath -> do
              spooledContent <- ByteString.readFile spooledPath
              expectAll
                ( (result `shouldBe` Left MultipartMalformedBody)
                    :| [spooledContent `shouldBe` "file content here"]
                )

    describe "consumeMultipartRequestBodyWith" $
      it "consumes a WAI request's body incrementally, calling onPart for each part" $
        withTestUploadOpener $ \_unusedOpener -> do
          seenPartsRef <- IORef.newIORef []
          readChunk <- chunkReader [twoPartBody]
          let request = Wai.setRequestBodyChunks readChunk Wai.defaultRequest
          result <-
            consumeMultipartRequestBodyWith defaultMultipartLimits boundaryToken request $ \part -> do
              IORef.modifyIORef' seenPartsRef (part :)
              pure (Right ())
          seenParts <- reverse <$> IORef.readIORef seenPartsRef
          expectAll
            ( (result `shouldSatisfy` \case Right () -> True; Left _ -> False)
                :| [ case seenParts of
                       [MultipartFieldPart "field1" "value1", MultipartFilePart "file1" "a.txt" _ _] -> pure ()
                       other -> expectationFailure ("unexpected parts: " <> show other)
                   ]
            )
