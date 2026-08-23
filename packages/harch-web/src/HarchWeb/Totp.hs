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
    validateTotpCodeCounter,
  )
where

import Crypto.Hash.Algorithms (SHA1)
import Crypto.MAC.HMAC (HMAC, hmac)
import Crypto.Random.Entropy (getEntropy)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteArray (convert)
import Data.ByteString qualified as ByteString
import Data.Char (isAscii, isAsciiLower, isAsciiUpper, isDigit, ord)
import Data.List (find)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word16, Word32, Word64, Word8)
import HarchWeb.Security.ConstantTime (constantWorkEquals)
import HarchWeb.Time (UnixTimeSeconds, unixTimeSecondsValue)

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

totpCode :: UnixTimeSeconds -> TotpSecret -> TotpCode
totpCode nowSeconds (TotpSecret secret) = totpCodeForCounter secret (unixTimeSecondsValue nowSeconds `div` 30)

-- | Accepts a code from the current TOTP period and up to the requested
-- number of adjacent periods on either side. The caller chooses the bounded
-- skew window appropriate for its authentication policy. This does not
-- defend against replaying an observed code for the rest of its skew
-- window; a caller authenticating a login should use
-- 'validateTotpCodeCounter' instead and reject a counter it has already
-- accepted.
validateTotpCode :: UnixTimeSeconds -> Word8 -> TotpSecret -> TotpCode -> Bool
validateTotpCode nowSeconds maxSkewPeriods secret suppliedCode =
  isJust (validateTotpCodeCounter nowSeconds maxSkewPeriods secret suppliedCode)

-- | Like 'validateTotpCode', but returns the specific counter the supplied
-- code matched instead of only whether one did. A caller that persists the
-- highest counter it has already accepted (per account) and rejects any
-- counter at or below that value closes the replay window a bare boolean
-- result cannot: without it, an observed code stays valid for the rest of
-- its skew window against every future request.
validateTotpCodeCounter :: UnixTimeSeconds -> Word8 -> TotpSecret -> TotpCode -> Maybe Word64
validateTotpCodeCounter nowSeconds maxSkewPeriods secret suppliedCode =
  find (matchesCounter secret suppliedCode) (windowCounters (unixTimeSecondsValue nowSeconds `div` 30) maxSkewPeriods)

matchesCounter :: TotpSecret -> TotpCode -> Word64 -> Bool
matchesCounter (TotpSecret secret) suppliedCode counter =
  constantWorkEquals (totpCodeBytes (totpCodeForCounter secret counter)) (totpCodeBytes suppliedCode)

totpCodeBytes :: TotpCode -> ByteString.ByteString
totpCodeBytes (TotpCode code) = TextEncoding.encodeUtf8 code

totpCodeForCounter :: ByteString.ByteString -> Word64 -> TotpCode
totpCodeForCounter secret counter =
  TotpCode (renderCode (dynamicTruncation (hmacSha1 secret counter)))

windowCounters :: Word64 -> Word8 -> [Word64]
windowCounters currentCounter maxSkewPeriods =
  previousAndCurrent <> following
  where
    maxOffset = fromIntegral maxSkewPeriods :: Word64
    previousAndCurrent = [currentCounter - offset | offset <- [0 .. min maxOffset currentCounter]]
    -- A TOTP counter is derived by dividing a Word64 timestamp by the
    -- 30-second period, so even its largest possible value leaves ample room
    -- for the caller-bounded Word8 offset.
    following = [currentCounter + offset | offset <- [1 .. maxOffset]]

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
base32Value character
  | isAsciiUpper character = Just (fromIntegral (ord character - ord 'A'))
  | isAsciiLower character = Just (fromIntegral (ord character - ord 'a'))
  | character >= '2' && character <= '7' = Just (fromIntegral (ord character - ord '2' + 26))
base32Value _ = Nothing

isAsciiDigit :: Char -> Bool
isAsciiDigit character = isAscii character && isDigit character
