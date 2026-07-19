{-# LANGUAGE OverloadedStrings #-}

module HarchWeb.Password
  ( Password,
    PasswordHash (..),
    PasswordHashingPolicy (..),
    defaultPasswordHashingPolicy,
    hashPassword,
    hashPasswordWithSalt,
    mkPassword,
    mkPasswordHashingPolicy,
    passwordHashText,
    readPasswordHash,
    verifyPassword,
  )
where

import Crypto.Error (CryptoFailable (..))
import Crypto.KDF.Argon2 qualified as Argon2
import Crypto.Random.Entropy (getEntropy)
import Data.Bits (xor, (.|.))
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64.URL qualified as Base64Url
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word32)
import Text.Read (readMaybe)

newtype Password = Password ByteString.ByteString

newtype PasswordHash = PasswordHash Text

data PasswordHashingPolicy = PasswordHashingPolicy
  { passwordHashIterations :: Word32,
    passwordHashMemoryKibibytes :: Word32,
    passwordHashParallelism :: Word32
  }
  deriving (Eq, Show)

defaultPasswordHashingPolicy :: PasswordHashingPolicy
defaultPasswordHashingPolicy =
  PasswordHashingPolicy
    { passwordHashIterations = 3,
      passwordHashMemoryKibibytes = 65536,
      passwordHashParallelism = 1
    }

mkPassword :: Text -> Password
mkPassword = Password . TextEncoding.encodeUtf8

mkPasswordHashingPolicy :: Word32 -> Word32 -> Word32 -> Maybe PasswordHashingPolicy
mkPasswordHashingPolicy iterations memoryKibibytes parallelism =
  case iterations == 0 || parallelism == 0 || memoryKibibytes < 8 * parallelism of
    True -> Nothing
    False -> Just (PasswordHashingPolicy iterations memoryKibibytes parallelism)

hashPassword :: PasswordHashingPolicy -> Password -> IO (Maybe PasswordHash)
hashPassword policy password = do
  salt <- getEntropy 16
  pure (hashPasswordWithSalt policy salt password)

hashPasswordWithSalt :: PasswordHashingPolicy -> ByteString.ByteString -> Password -> Maybe PasswordHash
hashPasswordWithSalt policy salt (Password password) =
  let hashValue = argon2Hash policy salt password
   in if ByteString.null hashValue
        then Nothing
        else Just
    ( PasswordHash
        ( "$argon2id$v=19$m="
            <> Text.pack (show (passwordHashMemoryKibibytes policy))
            <> ",t="
            <> Text.pack (show (passwordHashIterations policy))
            <> ",p="
            <> Text.pack (show (passwordHashParallelism policy))
            <> "$"
            <> encodeBase64Url (Base64Url.encodeUnpadded salt)
            <> "$"
            <> encodeBase64Url (Base64Url.encodeUnpadded hashValue)
        )
    )

passwordHashText :: PasswordHash -> Text
passwordHashText (PasswordHash hashValue) = hashValue

readPasswordHash :: Text -> Maybe PasswordHash
readPasswordHash hashValue =
  PasswordHash hashValue <$ parsePasswordHash hashValue

verifyPassword :: Password -> PasswordHash -> Bool
verifyPassword (Password password) (PasswordHash storedHash) =
  case parsePasswordHash storedHash of
    Nothing -> False
    Just (policy, salt, expectedHash) ->
      constantWorkEqual expectedHash (argon2Hash policy salt password)

argon2Hash :: PasswordHashingPolicy -> ByteString.ByteString -> ByteString.ByteString -> ByteString.ByteString
argon2Hash policy salt password =
  case Argon2.hash (argon2Options policy) password salt 32 of
    CryptoPassed hashValue -> hashValue
    CryptoFailed _ -> ByteString.empty

argon2Options :: PasswordHashingPolicy -> Argon2.Options
argon2Options policy =
  Argon2.defaultOptions
    { Argon2.iterations = passwordHashIterations policy,
      Argon2.memory = passwordHashMemoryKibibytes policy,
      Argon2.parallelism = passwordHashParallelism policy,
      Argon2.variant = Argon2.Argon2id,
      Argon2.version = Argon2.Version13
    }

parsePasswordHash :: Text -> Maybe (PasswordHashingPolicy, ByteString.ByteString, ByteString.ByteString)
parsePasswordHash storedHash = do
  ["", "argon2id", "v=19", parameters, encodedSalt, encodedHash] <- pure (Text.splitOn "$" storedHash)
  [memoryValue, iterationValue, parallelismValue] <- pure (Text.splitOn "," parameters)
  memoryKibibytes <- Text.stripPrefix "m=" memoryValue >>= readWord32
  iterations <- Text.stripPrefix "t=" iterationValue >>= readWord32
  parallelism <- Text.stripPrefix "p=" parallelismValue >>= readWord32
  policy <- mkPasswordHashingPolicy iterations memoryKibibytes parallelism
  salt <- either (const Nothing) Just (Base64Url.decodeUnpadded (TextEncoding.encodeUtf8 encodedSalt))
  hashValue <- either (const Nothing) Just (Base64Url.decodeUnpadded (TextEncoding.encodeUtf8 encodedHash))
  if ByteString.length salt < 16 || ByteString.length hashValue /= 32
    then Nothing
    else Just (policy, salt, hashValue)

readWord32 :: Text -> Maybe Word32
readWord32 = readMaybe . Text.unpack

encodeBase64Url :: ByteString.ByteString -> Text
encodeBase64Url = TextEncoding.decodeUtf8

constantWorkEqual :: ByteString.ByteString -> ByteString.ByteString -> Bool
constantWorkEqual expected actual =
  let byteDifference =
        foldl'
          (.|.)
          0
          (ByteString.zipWith (\left right -> fromIntegral (left `xor` right)) expected actual)
      lengthDifference = ByteString.length expected `xor` ByteString.length actual
   in (byteDifference .|. lengthDifference) == 0
