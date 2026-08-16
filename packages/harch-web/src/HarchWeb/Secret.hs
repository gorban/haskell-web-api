{-# LANGUAGE OverloadedStrings #-}

module HarchWeb.Secret
  ( EncryptionNonce,
    SecretDecryptionError (..),
    SecretEncryptionKey,
    SecretEncryptionError (..),
    SecretPlaintext,
    decryptSecret,
    decryptSecretText,
    encryptSecret,
    encryptSecretWithNonce,
    mkEncryptionNonce,
    mkSecretEncryptionKey,
    mkSecretPlaintext,
  )
where

import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (AEADMode (AEAD_GCM), AuthTag (..), aeadInit, aeadSimpleDecrypt, aeadSimpleEncrypt, cipherInit)
import Crypto.Error (CryptoFailable (..))
import Crypto.Random.Entropy (getEntropy)
import Data.Bifunctor (first)
import Data.ByteArray (convert)
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64.URL qualified as Base64Url
import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word8)

newtype SecretEncryptionKey = SecretEncryptionKey ByteString.ByteString
  deriving (Eq)

mkSecretEncryptionKey :: Text -> Maybe SecretEncryptionKey
mkSecretEncryptionKey encodedKey = do
  key <- either (const Nothing) Just (Base64Url.decodeUnpadded (TextEncoding.encodeUtf8 encodedKey))
  if ByteString.length key == 32
    then Just (SecretEncryptionKey key)
    else Nothing

-- | A fresh (or caller-supplied, in 'encryptSecretWithNonce') AEAD nonce.
-- Opaque so it cannot be transposed with 'SecretPlaintext' at a call site.
newtype EncryptionNonce = EncryptionNonce ByteString.ByteString

mkEncryptionNonce :: ByteString.ByteString -> Maybe EncryptionNonce
mkEncryptionNonce nonce
  | ByteString.length nonce == nonceLength = Just (EncryptionNonce nonce)
  | otherwise = Nothing

-- | The plaintext bytes being encrypted. Opaque so it cannot be transposed
-- with 'EncryptionNonce' at a call site.
newtype SecretPlaintext = SecretPlaintext ByteString.ByteString

mkSecretPlaintext :: ByteString.ByteString -> SecretPlaintext
mkSecretPlaintext = SecretPlaintext

data SecretEncryptionError
  = SecretEncryptionGeneratedInvalidNonce
  | SecretEncryptionCipherInitializationFailed
  | SecretEncryptionAeadInitializationFailed
  deriving (Eq)

data SecretDecryptionError
  = SecretDecryptionMalformedEnvelope
  | SecretDecryptionUnsupportedEnvelopeVersion Word8
  | SecretDecryptionCipherInitializationFailed
  | SecretDecryptionAeadInitializationFailed
  | SecretDecryptionAuthenticationFailed
  | SecretDecryptionPlaintextIsNotUtf8
  deriving (Eq)

-- | Encrypts a secret with fresh OS entropy. The nonce remains validated at
-- the construction boundary even though 'getEntropy' is expected to return
-- the requested length, so a short read is an explicit result instead of an
-- exception or an invalid GCM invocation.
encryptSecret :: SecretEncryptionKey -> ByteString.ByteString -> IO (Either SecretEncryptionError Text)
encryptSecret encryptionKey plaintext = do
  nonce <- getEntropy nonceLength
  pure $
    case mkEncryptionNonce nonce of
      Nothing -> Left SecretEncryptionGeneratedInvalidNonce
      Just validatedNonce -> encryptSecretWithNonce encryptionKey validatedNonce (mkSecretPlaintext plaintext)

-- | Encrypts with a caller-supplied, already-validated nonce. The nonce and
-- plaintext roles are distinct, so a 12-byte plaintext cannot be accidentally
-- transposed into the nonce position.
encryptSecretWithNonce :: SecretEncryptionKey -> EncryptionNonce -> SecretPlaintext -> Either SecretEncryptionError Text
encryptSecretWithNonce (SecretEncryptionKey key) (EncryptionNonce nonce) (SecretPlaintext plaintext) =
  TextEncoding.decodeUtf8 . Base64Url.encodeUnpadded <$> encrypt key nonce plaintext

-- | Decrypts a versioned envelope. Authentication failures are distinct from
-- malformed or unsupported envelopes so callers can log private diagnostics
-- without exposing the cause in a public response.
decryptSecret :: SecretEncryptionKey -> Text -> Either SecretDecryptionError ByteString.ByteString
decryptSecret (SecretEncryptionKey key) encodedEnvelope = do
  envelope <- first (const SecretDecryptionMalformedEnvelope) (Base64Url.decodeUnpadded (TextEncoding.encodeUtf8 encodedEnvelope))
  (nonce, authenticationTag, ciphertext) <- splitEnvelope envelope
  decrypt key nonce authenticationTag ciphertext

decryptSecretText :: SecretEncryptionKey -> Text -> Either SecretDecryptionError Text
decryptSecretText encryptionKey encodedEnvelope = do
  plaintext <- decryptSecret encryptionKey encodedEnvelope
  first (const SecretDecryptionPlaintextIsNotUtf8) (TextEncoding.decodeUtf8' plaintext)

encrypt :: ByteString.ByteString -> ByteString.ByteString -> ByteString.ByteString -> Either SecretEncryptionError ByteString.ByteString
encrypt key nonce plaintext = do
  cipher <- cryptoFailableToEither SecretEncryptionCipherInitializationFailed (cipherInit key :: CryptoFailable AES256)
  aead <- cryptoFailableToEither SecretEncryptionAeadInitializationFailed (aeadInit AEAD_GCM cipher nonce)
  let (authenticationTag, ciphertext) = aeadSimpleEncrypt aead associatedData plaintext authenticationTagLength
  pure (envelopeVersion <> nonce <> convert authenticationTag <> ciphertext)

decrypt :: ByteString.ByteString -> ByteString.ByteString -> ByteString.ByteString -> ByteString.ByteString -> Either SecretDecryptionError ByteString.ByteString
decrypt key nonce authenticationTag ciphertext = do
  cipher <- cryptoFailableToEither SecretDecryptionCipherInitializationFailed (cipherInit key :: CryptoFailable AES256)
  aead <- cryptoFailableToEither SecretDecryptionAeadInitializationFailed (aeadInit AEAD_GCM cipher nonce)
  maybe (Left SecretDecryptionAuthenticationFailed) Right (aeadSimpleDecrypt aead associatedData ciphertext (AuthTag (convert authenticationTag)))

splitEnvelope :: ByteString.ByteString -> Either SecretDecryptionError (ByteString.ByteString, ByteString.ByteString, ByteString.ByteString)
splitEnvelope envelope =
  case ByteString.uncons envelope of
    Nothing -> Left SecretDecryptionMalformedEnvelope
    Just (version, remainder)
      | version /= 1 -> Left (SecretDecryptionUnsupportedEnvelopeVersion version)
      | ByteString.length nonce /= nonceLength || ByteString.length authenticationTag /= authenticationTagLength -> Left SecretDecryptionMalformedEnvelope
      | otherwise -> Right (nonce, authenticationTag, ciphertext)
      where
        (nonce, afterNonce) = ByteString.splitAt nonceLength remainder
        (authenticationTag, ciphertext) = ByteString.splitAt authenticationTagLength afterNonce

cryptoFailableToEither :: errorValue -> CryptoFailable value -> Either errorValue value
cryptoFailableToEither failure cryptoResult =
  case cryptoResult of
    CryptoPassed value -> Right value
    CryptoFailed _ -> Left failure

envelopeVersion :: ByteString.ByteString
envelopeVersion = "\x01"

associatedData :: ByteString.ByteString
associatedData = "harch-web.secret.v1"

nonceLength :: Int
nonceLength = 12

authenticationTagLength :: Int
authenticationTagLength = 16
