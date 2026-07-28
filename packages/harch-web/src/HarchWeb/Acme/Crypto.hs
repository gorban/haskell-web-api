{-# LANGUAGE OverloadedStrings #-}

-- | Private byte encodings used by the ACME protocol and OpenSSL adapter.
module HarchWeb.Acme.Crypto
  ( acmeJwkThumbprintBytes,
    base64urlText,
    hexTextToByteString,
  )
where

import Data.ByteString qualified as ByteString
import Data.ByteString.Base64.URL qualified as Base64Url
import Data.Char (digitToInt, isDigit)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word8)
import HarchWeb.Acme.Protocol.Types (AcmeJwk (..))

acmeJwkThumbprintBytes :: AcmeJwk -> ByteString.ByteString
acmeJwkThumbprintBytes accountJwk =
  TextEncoding.encodeUtf8 $
    "{\"e\":\""
      <> acmeJwkExponent accountJwk
      <> "\",\"kty\":\"RSA\",\"n\":\""
      <> acmeJwkModulus accountJwk
      <> "\"}"

base64urlText :: ByteString.ByteString -> Text
base64urlText =
  TextEncoding.decodeUtf8 . Base64Url.encodeUnpadded

hexTextToByteString :: Text -> Either String ByteString.ByteString
hexTextToByteString hexText =
  ByteString.pack <$> decodeHexPairs cleanedHex
  where
    cleanedHex = filter (not . (`elem` [' ', '\n', '\r', '\t'])) (Text.unpack hexText)

decodeHexPairs :: String -> Either String [Word8]
decodeHexPairs [] = Right []
decodeHexPairs [_] = Left "hex string had an odd length"
decodeHexPairs (firstDigit : secondDigit : remainingDigits) =
  (:) <$> decodeHexByte firstDigit secondDigit <*> decodeHexPairs remainingDigits

decodeHexByte :: Char -> Char -> Either String Word8
decodeHexByte firstDigit secondDigit =
  case (hexDigitValue firstDigit, hexDigitValue secondDigit) of
    (Just highDigit, Just lowDigit) -> Right (fromIntegral (highDigit * 16 + lowDigit))
    _ -> Left ("invalid hex digit pair: " <> [firstDigit, secondDigit])

hexDigitValue :: Char -> Maybe Int
hexDigitValue hexDigit
  | isDigit hexDigit = Just (digitToInt hexDigit)
  | 'a' <= hexDigit && hexDigit <= 'f' = Just (digitToInt hexDigit)
  | 'A' <= hexDigit && hexDigit <= 'F' = Just (digitToInt hexDigit)
hexDigitValue _ = Nothing
