{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWeb.Api.MultipartSpec (spec) where

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
