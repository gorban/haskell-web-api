{-# LANGUAGE OverloadedStrings #-}

-- | Framework-owned, opaque correlation identifiers for HTTP requests.
--
-- Decision record (AHI-5-RID, 2026-09-05): request correlation extends the
-- existing request-context ingress and response-finalization boundaries rather
-- than adding an application-local header convention.  This module owns only
-- the cryptographically random UUIDv4 representation and bounded validation;
-- the following AHI-5-RID integration slice attaches one value to each
-- accepted request, response, diagnostics, and observability record.  It does
-- not introduce account auditing, an idempotency key, a trace hierarchy, or a
-- trust decision for public inbound headers.
module HarchWeb.RequestId
  ( RequestId,
    mkRequestId,
    newRequestId,
    requestIdText,
  )
where

import Crypto.Random.Entropy (getEntropy)
import Data.Bits ((.&.), (.|.))
import Data.ByteString qualified as ByteString
import Data.ByteString.Base16 qualified as Base16
import Data.Char (isAscii, isDigit)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word8)

-- | A canonical lower-case UUIDv4.  Its constructor remains private so a
-- caller cannot use request correlation as an arbitrary text/header carrier.
newtype RequestId = RequestId Text
  deriving (Eq)

instance Show RequestId where
  showsPrec precedence requestId =
    showParen (precedence > 10) $
      showString "RequestId " . shows (requestIdText requestId)

requestIdText :: RequestId -> Text
requestIdText (RequestId value) = value

-- | Accept only the 36-byte canonical UUIDv4 text form.  In particular,
-- upper-case, non-ASCII, non-v4 and non-RFC-4122-variant values cannot become
-- correlation identifiers.
mkRequestId :: Text -> Maybe RequestId
mkRequestId value
  | Text.length value /= 36 = Nothing
  | Text.index value 14 /= '4' = Nothing
  | Text.index value 19 `notElem` ['8', '9', 'a', 'b'] = Nothing
  | all isCanonicalCharacter (zip [0 :: Int ..] (Text.unpack value)) = Just (RequestId value)
  | otherwise = Nothing
  where
    isCanonicalCharacter (index, character)
      | index `elem` [8, 13, 18, 23] = character == '-'
      | otherwise = isAsciiDigit character || character `elem` ['a' .. 'f']

    isAsciiDigit character = isAscii character && isDigit character

-- | Generate an unpredictable canonical UUIDv4 from the operating system's
-- CSPRNG.  The version and RFC 4122 variant bits are set after entropy is
-- obtained, before its canonical text representation is exposed.
newRequestId :: IO RequestId
newRequestId = requestIdFromEntropy <$> getEntropy 16

requestIdFromEntropy :: ByteString.ByteString -> RequestId
requestIdFromEntropy entropy =
  RequestId (renderUuid (setUuidVersionAndVariant entropy))

setUuidVersionAndVariant :: ByteString.ByteString -> ByteString.ByteString
setUuidVersionAndVariant entropy =
  ByteString.pack (zipWith applyUuidBits [0 :: Int ..] (ByteString.unpack entropy))
  where
    applyUuidBits :: Int -> Word8 -> Word8
    applyUuidBits index octet =
      case index of
        6 -> (octet .&. 0x0f) .|. 0x40
        8 -> (octet .&. 0x3f) .|. 0x80
        _ -> octet

renderUuid :: ByteString.ByteString -> Text
renderUuid bytes =
  let hex = TextEncoding.decodeLatin1 (Base16.encode bytes)
   in Text.intercalate "-" [Text.take 8 hex, Text.take 4 (Text.drop 8 hex), Text.take 4 (Text.drop 12 hex), Text.take 4 (Text.drop 16 hex), Text.drop 20 hex]
