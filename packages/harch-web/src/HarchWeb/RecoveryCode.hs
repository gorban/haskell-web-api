{-# LANGUAGE OverloadedStrings #-}

module HarchWeb.RecoveryCode
  ( RecoveryCode,
    RecoveryCodeHash,
    generateRecoveryCode,
    hashRecoveryCode,
    hashRecoveryCodeWithSalt,
    mkRecoveryCode,
    readRecoveryCodeHash,
    recoveryCodeHashText,
    recoveryCodeHashWorkKibibytes,
    recoveryCodeText,
    verifyRecoveryCode,
  )
where

import Crypto.Random.Entropy (getEntropy)
import Data.ByteString qualified as ByteString
import Data.ByteString.Base16 qualified as Base16
import Data.Char (isDigit)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word32)
import HarchWeb.Password
  ( Password,
    PasswordHash,
    PasswordHashingPolicy,
    hashPassword,
    hashPasswordWithSalt,
    mkPassword,
    passwordHashMemoryKibibytes,
    passwordHashText,
    readPasswordHashWithWorkKibibytes,
    verifyPassword,
  )

newtype RecoveryCode = RecoveryCode Text
  deriving (Eq)

-- | A recovery-code verifier paired with the validated Argon2 memory it
-- needs. The constructor remains private, so a successfully read or produced
-- hash always has an admission cost; callers cannot reach native verification
-- with a malformed externally constructed password hash.
data RecoveryCodeHash = RecoveryCodeHash PasswordHash Word32

generateRecoveryCode :: IO RecoveryCode
generateRecoveryCode = RecoveryCode . renderCode <$> getEntropy recoveryCodeBytes

mkRecoveryCode :: Text -> Maybe RecoveryCode
mkRecoveryCode suppliedCode =
  let normalizedCode = Text.toUpper (Text.filter (/= '-') suppliedCode)
   in if Text.length normalizedCode == recoveryCodeHexCharacters && Text.all isHexDigit normalizedCode
        then Just (RecoveryCode normalizedCode)
        else Nothing

recoveryCodeText :: RecoveryCode -> Text
recoveryCodeText (RecoveryCode code) = Text.intercalate "-" (Text.chunksOf recoveryCodeGroupLength code)

hashRecoveryCode :: PasswordHashingPolicy -> RecoveryCode -> IO (Maybe RecoveryCodeHash)
hashRecoveryCode policy code =
  fmap (fmap (recoveryCodeHashWithPolicy policy)) (hashPassword policy (passwordFor code))

hashRecoveryCodeWithSalt :: PasswordHashingPolicy -> ByteString.ByteString -> RecoveryCode -> Maybe RecoveryCodeHash
hashRecoveryCodeWithSalt policy salt code = recoveryCodeHashWithPolicy policy <$> hashPasswordWithSalt policy salt (passwordFor code)

verifyRecoveryCode :: RecoveryCode -> RecoveryCodeHash -> Bool
verifyRecoveryCode code (RecoveryCodeHash storedHash _) = verifyPassword (passwordFor code) storedHash

recoveryCodeHashText :: RecoveryCodeHash -> Text
recoveryCodeHashText (RecoveryCodeHash storedHash _) = passwordHashText storedHash

recoveryCodeHashWorkKibibytes :: RecoveryCodeHash -> Word32
recoveryCodeHashWorkKibibytes (RecoveryCodeHash _ workKibibytes) = workKibibytes

readRecoveryCodeHash :: Text -> Maybe RecoveryCodeHash
readRecoveryCodeHash storedHash = do
  (passwordHash, workKibibytes) <- readPasswordHashWithWorkKibibytes storedHash
  pure (RecoveryCodeHash passwordHash workKibibytes)

recoveryCodeHashWithPolicy :: PasswordHashingPolicy -> PasswordHash -> RecoveryCodeHash
recoveryCodeHashWithPolicy policy passwordHash = RecoveryCodeHash passwordHash (passwordHashMemoryKibibytes policy)

passwordFor :: RecoveryCode -> Password
passwordFor (RecoveryCode code) = mkPassword code

renderCode :: ByteString.ByteString -> Text
renderCode = Text.toUpper . TextEncoding.decodeLatin1 . Base16.encode

isHexDigit :: Char -> Bool
isHexDigit character = isDigit character || ('A' <= character && character <= 'F')

recoveryCodeBytes :: Int
recoveryCodeBytes = 10

recoveryCodeHexCharacters :: Int
recoveryCodeHexCharacters = recoveryCodeBytes * 2

recoveryCodeGroupLength :: Int
recoveryCodeGroupLength = 5
