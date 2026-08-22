{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWeb.Api.Multipart.ScannerSpec (spec) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.List (mapAccumL)
import Data.List.NonEmpty (NonEmpty (..))
import HarchWeb.Api.Multipart
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

spec :: Spec
spec =
  describe "HarchWeb.Api.Multipart.Scanner" $ do
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
