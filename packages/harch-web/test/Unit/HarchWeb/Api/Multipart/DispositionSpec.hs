{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty (..))
import HarchWeb.Api.Multipart

fieldPartHeaders :: ByteString
fieldPartHeaders = "Content-Disposition: form-data; name=\"field1\""

filePartHeaders :: ByteString
filePartHeaders =
  "Content-Disposition: form-data; name=\"file1\"; filename=\"a.txt\"\r\nContent-Type: text/plain"

spec =
  describe "parseMultipartFieldDisposition" $ do
    -- Tabled per docs/design-guidance.md's CN decision record: one act,
    -- one comparison, differing only in the input header text and the
    -- expected disposition. The three duplicate-parameter cases were
    -- previously bundled in one 'it' via 'expectAll'; each now reports
    -- individually. The one case whose assertion is a pattern-match
    -- predicate, not an equality, stays its own 'it' below the table.
    [ ("extracts a plain field's name", fieldPartHeaders, Just (MultipartFieldDisposition (Just "field1") Nothing)),
      ("extracts a file field's name and filename", filePartHeaders, Just (MultipartFieldDisposition (Just "file1") (Just "a.txt"))),
      ("returns Nothing when there is no Content-Disposition header", "Content-Type: text/plain", Nothing),
      ("rejects a disposition that is not form-data", "Content-Disposition: attachment; name=\"field\"", Nothing),
      ("rejects a duplicate name parameter", "Content-Disposition: form-data; name=\"first\"; name=\"second\"", Nothing),
      ("rejects a duplicate filename parameter", "Content-Disposition: form-data; name=\"field\"; filename=\"first.txt\"; filename=\"second.txt\"", Nothing),
      ("is case-insensitive for the header name", "content-DISPOSITION: form-data; name=\"x\"", Just (MultipartFieldDisposition (Just "x") Nothing)),
      ("keeps a semicolon inside a quoted filename from ending the parameter early", "Content-Disposition: form-data; name=\"f\"; filename=\"a;b.txt\"", Just (MultipartFieldDisposition (Just "f") (Just "a;b.txt"))),
      ("unescapes a backslash-escaped quote inside a quoted value", "Content-Disposition: form-data; name=\"f\"; filename=\"a\\\"b.txt\"", Just (MultipartFieldDisposition (Just "f") (Just "a\"b.txt"))),
      ("ignores a malformed parameter without '=' while keeping the well-formed ones", "Content-Disposition: form-data; malformed; name=\"f\"", Just (MultipartFieldDisposition (Just "f") Nothing)),
      ("decodes non-ASCII header bytes leniently rather than failing", "Content-Disposition: form-data; name=\"f\"; filename=\"caf\xC3\xA9.txt\"", Just (MultipartFieldDisposition (Just "f") (Just "caf\233.txt"))),
      ("substitutes the Unicode replacement character for invalid UTF-8 header bytes", "Content-Disposition: form-data; name=\"f\"; filename=\"bad\xFF.txt\"", Just (MultipartFieldDisposition (Just "f") (Just "bad\65533.txt"))),
      ("skips a header line without a colon rather than failing the whole block", "garbage line\r\nContent-Disposition: form-data; name=\"f\"", Just (MultipartFieldDisposition (Just "f") Nothing)),
      ("finds Content-Disposition even when it is not the first header", "Content-Type: text/plain\r\nContent-Disposition: form-data; name=\"f\"", Just (MultipartFieldDisposition (Just "f") Nothing)),
      ("keeps an unquoted parameter value as-is", "Content-Disposition: form-data; name=f", Just (MultipartFieldDisposition (Just "f") Nothing))
      ]
      `forM_` \(label, input, expected) ->
        it label $
          parseMultipartFieldDisposition input `shouldBe` expected

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
                       `shouldBe` length dispositions
                       * (length dispositions - 1),
                     sum [length (show d) + length (showList [d] "") | d <- dispositions] `shouldSatisfy` (> 0)
                   ]
            )
