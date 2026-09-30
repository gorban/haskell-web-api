module HarchWeb.Account
  ( AccountId,
    EmailVerificationToken,
    EmailVerificationTokenDigest,
    EmailVerificationValidation (..),
    StoredEmailVerification (..),
    accountIdText,
    emailVerificationTokenDigest,
    emailVerificationTokenDigestText,
    emailVerificationTokenText,
    generateAccountId,
    generateEmailVerificationToken,
    mkAccountId,
    mkEmailVerificationToken,
    mkStoredEmailVerification,
    validateEmailVerificationToken,
  )
where

import Crypto.Hash (Digest, SHA256, hash)
import Crypto.Random.Entropy (getEntropy)
import Data.ByteArray (convert)
import Data.ByteString.Base64.URL qualified as Base64Url
import Data.Char (isAscii, isAsciiLower, isAsciiUpper, isDigit)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Email (EmailAddress)
import HarchWeb.Security.ConstantTime (constantWorkEquals)
import HarchWeb.Time (UnixTimeNanoseconds)

newtype AccountId = AccountId Text
  deriving (Eq, Show)

newtype EmailVerificationToken = EmailVerificationToken Text

newtype EmailVerificationTokenDigest = EmailVerificationTokenDigest Text
  deriving (Eq, Show)

data StoredEmailVerification = StoredEmailVerification
  { storedVerificationAccountId :: AccountId,
    storedVerificationEmail :: EmailAddress,
    storedVerificationTokenDigest :: EmailVerificationTokenDigest,
    storedVerificationExpiresAtNanoseconds :: UnixTimeNanoseconds
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

generateAccountId :: IO AccountId
generateAccountId =
  AccountId . TextEncoding.decodeUtf8 . Base64Url.encodeUnpadded <$> getEntropy 16

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

emailVerificationTokenDigestText :: EmailVerificationTokenDigest -> Text
emailVerificationTokenDigestText (EmailVerificationTokenDigest value) = value

mkStoredEmailVerification :: AccountId -> EmailAddress -> UnixTimeNanoseconds -> EmailVerificationToken -> StoredEmailVerification
mkStoredEmailVerification accountId emailAddress expiresAt token =
  StoredEmailVerification
    { storedVerificationAccountId = accountId,
      storedVerificationEmail = emailAddress,
      storedVerificationTokenDigest = emailVerificationTokenDigest token,
      storedVerificationExpiresAtNanoseconds = expiresAt
    }

validateEmailVerificationToken :: UnixTimeNanoseconds -> EmailVerificationToken -> StoredEmailVerification -> EmailVerificationValidation
validateEmailVerificationToken now token storedVerification =
  case now >= storedVerificationExpiresAtNanoseconds storedVerification of
    True -> EmailVerificationExpired
    False ->
      case constantWorkEquals
        (TextEncoding.encodeUtf8 (digestText (emailVerificationTokenDigest token)))
        (TextEncoding.encodeUtf8 (digestText (storedVerificationTokenDigest storedVerification))) of
        True -> EmailVerificationAccepted (storedVerificationAccountId storedVerification) (storedVerificationEmail storedVerification)
        False -> EmailVerificationRejected

digestText :: EmailVerificationTokenDigest -> Text
digestText (EmailVerificationTokenDigest value) = value

isAccountIdCharacter :: Char -> Bool
isAccountIdCharacter character =
  isAscii character
    && (character == '-' || character == '_' || isAsciiLower character || isAsciiUpper character || isDigit character)

isOpaqueTokenCharacter :: Char -> Bool
isOpaqueTokenCharacter = isAccountIdCharacter
