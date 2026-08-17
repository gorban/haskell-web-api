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
import HarchWeb.Password
  ( Password,
    PasswordHash,
    PasswordHashingPolicy,
    hashPassword,
    hashPasswordWithSalt,
    mkPassword,
    passwordHashText,
    readPasswordHash,
    verifyPassword,
  )

newtype RecoveryCode = RecoveryCode Text
  deriving (Eq)

newtype RecoveryCodeHash = RecoveryCodeHash PasswordHash

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
hashRecoveryCode policy code = fmap (fmap RecoveryCodeHash) (hashPassword policy (passwordFor code))

hashRecoveryCodeWithSalt :: PasswordHashingPolicy -> ByteString.ByteString -> RecoveryCode -> Maybe RecoveryCodeHash
hashRecoveryCodeWithSalt policy salt code = RecoveryCodeHash <$> hashPasswordWithSalt policy salt (passwordFor code)

verifyRecoveryCode :: RecoveryCode -> RecoveryCodeHash -> Bool
verifyRecoveryCode code (RecoveryCodeHash storedHash) = verifyPassword (passwordFor code) storedHash

recoveryCodeHashText :: RecoveryCodeHash -> Text
recoveryCodeHashText (RecoveryCodeHash storedHash) = passwordHashText storedHash

readRecoveryCodeHash :: Text -> Maybe RecoveryCodeHash
readRecoveryCodeHash storedHash = RecoveryCodeHash <$> readPasswordHash storedHash

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
