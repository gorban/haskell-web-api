{-# LANGUAGE OverloadedStrings #-}

module HarchWeb.Totp
  ( TotpCode,
    TotpSecret,
    generateTotpSecret,
    mkTotpCode,
    mkTotpSecret,
    renderTotpSecret,
    totpCode,
    totpCodeText,
    validateTotpCode,
  )
where

import Crypto.Hash.Algorithms (SHA1)
import Crypto.MAC.HMAC (HMAC, hmac)
import Crypto.Random.Entropy (getEntropy)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteArray (convert)
import Data.ByteString qualified as ByteString
import Data.Char (isAscii, isAsciiLower, isAsciiUpper, isDigit, ord)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word16, Word32, Word64, Word8)

newtype TotpSecret = TotpSecret ByteString.ByteString
  deriving (Eq)

newtype TotpCode = TotpCode Text
  deriving (Eq, Show)

generateTotpSecret :: IO TotpSecret
generateTotpSecret = TotpSecret <$> getEntropy 20

mkTotpSecret :: Text -> Maybe TotpSecret
mkTotpSecret encodedSecret = do
  secret <- decodeBase32 encodedSecret
  if ByteString.length secret < 16
    then Nothing
    else Just (TotpSecret secret)

mkTotpCode :: Text -> Maybe TotpCode
mkTotpCode code =
  if Text.length code == 6 && Text.all isAsciiDigit code
    then Just (TotpCode code)
    else Nothing

renderTotpSecret :: TotpSecret -> Text
renderTotpSecret (TotpSecret secret) = encodeBase32 secret

totpCodeText :: TotpCode -> Text
totpCodeText (TotpCode code) = code

totpCode :: Word64 -> TotpSecret -> TotpCode
totpCode nowSeconds (TotpSecret secret) =
  TotpCode (renderCode (dynamicTruncation (hmacSha1 secret (nowSeconds `div` 30))))

validateTotpCode :: Word64 -> TotpSecret -> TotpCode -> Bool
validateTotpCode nowSeconds secret suppliedCode = totpCode nowSeconds secret == suppliedCode

hmacSha1 :: ByteString.ByteString -> Word64 -> ByteString.ByteString
hmacSha1 secret counter =
  convert (hmac secret (counterBytes counter) :: HMAC SHA1)

counterBytes :: Word64 -> ByteString.ByteString
counterBytes counter =
  ByteString.pack
    [ fromIntegral (counter `shiftR` 56),
      fromIntegral (counter `shiftR` 48),
      fromIntegral (counter `shiftR` 40),
      fromIntegral (counter `shiftR` 32),
      fromIntegral (counter `shiftR` 24),
      fromIntegral (counter `shiftR` 16),
      fromIntegral (counter `shiftR` 8),
      fromIntegral counter
    ]

dynamicTruncation :: ByteString.ByteString -> Word32
dynamicTruncation digest =
  let offset = fromIntegral (ByteString.last digest .&. 0x0f)
      byteAt index = fromIntegral (ByteString.index digest (offset + index)) :: Word32
   in ( (byteAt 0 .&. 0x7f) `shiftL` 24
          .|. byteAt 1 `shiftL` 16
          .|. byteAt 2 `shiftL` 8
          .|. byteAt 3
      )
        `mod` 1000000

renderCode :: Word32 -> Text
renderCode value = Text.justifyRight 6 '0' (Text.pack (show value))

encodeBase32 :: ByteString.ByteString -> Text
encodeBase32 bytes = Text.pack (go (ByteString.unpack bytes) 0 0)
  where
    go :: [Word8] -> Word16 -> Int -> [Char]
    go remaining accumulator bitCount =
      case remaining of
        [] ->
          case bitCount of
            0 -> []
            _ -> [base32Character (fromIntegral ((accumulator `shiftL` (5 - bitCount)) .&. 0x1f))]
        byte : rest ->
          emitCharacters rest (accumulator `shiftL` 8 .|. fromIntegral byte) (bitCount + 8)

    emitCharacters :: [Word8] -> Word16 -> Int -> [Char]
    emitCharacters rest accumulator bitCount =
      if bitCount < 5
        then go rest accumulator bitCount
        else
          let nextBitCount = bitCount - 5
              nextCharacter = base32Character (fromIntegral ((accumulator `shiftR` nextBitCount) .&. 0x1f))
              remainingAccumulator = accumulator .&. ((1 `shiftL` nextBitCount) - 1)
           in nextCharacter : emitCharacters rest remainingAccumulator nextBitCount

decodeBase32 :: Text -> Maybe ByteString.ByteString
decodeBase32 encodedSecret = do
  values <- traverse base32Value (Text.unpack encodedSecret)
  decodedBytes <- go values 0 0 []
  if null decodedBytes
    then Nothing
    else Just (ByteString.pack (reverse decodedBytes))
  where
    go :: [Word8] -> Word16 -> Int -> [Word8] -> Maybe [Word8]
    go remaining accumulator bitCount decodedBytes =
      case remaining of
        [] ->
          if accumulator == 0
            then Just decodedBytes
            else Nothing
        value : rest ->
          let nextAccumulator = accumulator `shiftL` 5 .|. fromIntegral value
              nextBitCount = bitCount + 5
           in emitBytes rest nextAccumulator nextBitCount decodedBytes

    emitBytes :: [Word8] -> Word16 -> Int -> [Word8] -> Maybe [Word8]
    emitBytes rest accumulator bitCount decodedBytes =
      if bitCount < 8
        then go rest accumulator bitCount decodedBytes
        else
          let nextBitCount = bitCount - 8
              nextByte = fromIntegral (accumulator `shiftR` nextBitCount)
              remainingAccumulator = accumulator .&. ((1 `shiftL` nextBitCount) - 1)
           in emitBytes rest remainingAccumulator nextBitCount (nextByte : decodedBytes)

base32Character :: Word8 -> Char
base32Character value =
  Text.index "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567" (fromIntegral value)

base32Value :: Char -> Maybe Word8
base32Value character =
  case character of
    _ | isAsciiUpper character -> Just (fromIntegral (ord character - ord 'A'))
    _ | isAsciiLower character -> Just (fromIntegral (ord character - ord 'a'))
    '2' -> Just 26
    '3' -> Just 27
    '4' -> Just 28
    '5' -> Just 29
    '6' -> Just 30
    '7' -> Just 31
    _ -> Nothing

isAsciiDigit :: Char -> Bool
isAsciiDigit character = isAscii character && isDigit character
