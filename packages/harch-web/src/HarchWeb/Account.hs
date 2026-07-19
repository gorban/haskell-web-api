module HarchWeb.Account
  ( AccountId,
    EmailVerificationToken,
    EmailVerificationTokenDigest,
    EmailVerificationValidation (..),
    StoredEmailVerification (..),
    accountIdText,
    emailVerificationTokenDigest,
    emailVerificationTokenText,
    generateEmailVerificationToken,
    mkAccountId,
    mkEmailVerificationToken,
    mkStoredEmailVerification,
    validateEmailVerificationToken,
  )
where

import Crypto.Hash (Digest, SHA256, hash)
import Crypto.Random.Entropy (getEntropy)
import Data.Bits (xor, (.|.))
import Data.ByteArray (convert)
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64.URL qualified as Base64Url
import Data.Char (isAscii, isAsciiLower, isAsciiUpper, isDigit)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word64)
import HarchWeb.Email (EmailAddress)

newtype AccountId = AccountId Text
  deriving (Eq, Show)

newtype EmailVerificationToken = EmailVerificationToken Text

newtype EmailVerificationTokenDigest = EmailVerificationTokenDigest Text
  deriving (Eq, Show)

data StoredEmailVerification = StoredEmailVerification
  { storedVerificationAccountId :: AccountId,
    storedVerificationEmail :: EmailAddress,
    storedVerificationTokenDigest :: EmailVerificationTokenDigest,
    storedVerificationExpiresAtNanoseconds :: Word64
  }
  deriving (Eq, Show)

data EmailVerificationValidation
  = EmailVerificationAccepted AccountId EmailAddress
  | EmailVerificationExpired
  | EmailVerificationRejected
  deriving (Eq, Show)

mkAccountId :: Text -> Maybe AccountId
mkAccountId value =
  if Text.null value || Text.any (not . isAccountIdCharacter) value
    then Nothing
    else Just (AccountId value)

accountIdText :: AccountId -> Text
accountIdText (AccountId value) = value

mkEmailVerificationToken :: Text -> Maybe EmailVerificationToken
mkEmailVerificationToken value =
  if Text.length value < 32 || Text.any (not . isOpaqueTokenCharacter) value
    then Nothing
    else Just (EmailVerificationToken value)

generateEmailVerificationToken :: IO EmailVerificationToken
generateEmailVerificationToken =
  EmailVerificationToken . TextEncoding.decodeUtf8 . Base64Url.encodeUnpadded <$> getEntropy 32

emailVerificationTokenText :: EmailVerificationToken -> Text
emailVerificationTokenText (EmailVerificationToken value) = value

emailVerificationTokenDigest :: EmailVerificationToken -> EmailVerificationTokenDigest
emailVerificationTokenDigest token =
  EmailVerificationTokenDigest
    ( TextEncoding.decodeUtf8
        ( Base64Url.encodeUnpadded
            (convert (hash (TextEncoding.encodeUtf8 (emailVerificationTokenText token)) :: Digest SHA256))
        )
    )

mkStoredEmailVerification :: AccountId -> EmailAddress -> Word64 -> EmailVerificationToken -> StoredEmailVerification
mkStoredEmailVerification accountId emailAddress expiresAt token =
  StoredEmailVerification
    { storedVerificationAccountId = accountId,
      storedVerificationEmail = emailAddress,
      storedVerificationTokenDigest = emailVerificationTokenDigest token,
      storedVerificationExpiresAtNanoseconds = expiresAt
    }

validateEmailVerificationToken :: Word64 -> EmailVerificationToken -> StoredEmailVerification -> EmailVerificationValidation
validateEmailVerificationToken now token storedVerification =
  case now >= storedVerificationExpiresAtNanoseconds storedVerification of
    True -> EmailVerificationExpired
    False ->
      case constantWorkEqual
        (digestText (emailVerificationTokenDigest token))
        (digestText (storedVerificationTokenDigest storedVerification)) of
        True -> EmailVerificationAccepted (storedVerificationAccountId storedVerification) (storedVerificationEmail storedVerification)
        False -> EmailVerificationRejected

digestText :: EmailVerificationTokenDigest -> Text
digestText (EmailVerificationTokenDigest value) = value

constantWorkEqual :: Text -> Text -> Bool
constantWorkEqual expected actual =
  let expectedBytes = TextEncoding.encodeUtf8 expected
      actualBytes = TextEncoding.encodeUtf8 actual
      byteDifference =
        foldl'
          (.|.)
          0
          (ByteString.zipWith (\left right -> fromIntegral (left `xor` right)) expectedBytes actualBytes)
      lengthDifference = ByteString.length expectedBytes `xor` ByteString.length actualBytes
   in (byteDifference .|. lengthDifference) == 0

isAccountIdCharacter :: Char -> Bool
isAccountIdCharacter character =
  isAscii character
    && (character == '-' || character == '_' || isAsciiLower character || isAsciiUpper character || isDigit character)

isOpaqueTokenCharacter :: Char -> Bool
isOpaqueTokenCharacter = isAccountIdCharacter
