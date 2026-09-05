{-# LANGUAGE OverloadedStrings #-}

module HarchWeb.Secret
  ( EncryptionNonce,
    SecretDecryptionError (..),
    SecretEncryptionKey,
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
import Crypto.Error (CryptoFailable, maybeCryptoError)
import Crypto.Random.Entropy (getEntropy)
import Data.Bifunctor (first)
import Data.ByteArray (convert)
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64.URL qualified as Base64Url
import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word8)

-- | An AES-256 key whose successful construction also proves that Cryptonite
-- has initialized its cipher context. Cryptonite represents AEAD setup with
-- 'CryptoFailable', which our public operations preserve instead of throwing:
-- AES-GCM can then remain total for its explicit cryptographic error rail.
data SecretEncryptionKey = SecretEncryptionKey ByteString.ByteString AES256

instance Eq SecretEncryptionKey where
  SecretEncryptionKey firstKey _ == SecretEncryptionKey secondKey _ = firstKey == secondKey

mkSecretEncryptionKey :: Text -> Maybe SecretEncryptionKey
mkSecretEncryptionKey encodedKey = do
  key <- either (const Nothing) Just (Base64Url.decodeUnpadded (TextEncoding.encodeUtf8 encodedKey))
  SecretEncryptionKey key <$> maybeCryptoError (cipherInit key :: CryptoFailable AES256)

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

data SecretDecryptionError
  = SecretDecryptionMalformedEnvelope
  | SecretDecryptionUnsupportedEnvelopeVersion Word8
  | SecretDecryptionAuthenticationFailed
  | SecretDecryptionPlaintextIsNotUtf8
  deriving (Eq)

-- | Encrypts a secret with fresh OS entropy. 'getEntropy' returns exactly the
-- requested number of bytes, so the module can create the opaque nonce
-- directly. Cryptographic setup failures remain an explicit
-- 'CryptoFailable' result rather than a partial pure exception.
encryptSecret :: SecretEncryptionKey -> ByteString.ByteString -> IO (CryptoFailable Text)
encryptSecret encryptionKey plaintext = do
  nonce <- getEntropy nonceLength
  pure (encryptSecretWithNonce encryptionKey (EncryptionNonce nonce) (mkSecretPlaintext plaintext))

-- | Encrypts with a caller-supplied, already-validated nonce. The nonce and
-- plaintext roles are distinct, so a 12-byte plaintext cannot be accidentally
-- transposed into the nonce position.
encryptSecretWithNonce :: SecretEncryptionKey -> EncryptionNonce -> SecretPlaintext -> CryptoFailable Text
encryptSecretWithNonce (SecretEncryptionKey _ key) (EncryptionNonce nonce) (SecretPlaintext plaintext) =
  TextEncoding.decodeUtf8 . Base64Url.encodeUnpadded <$> encrypt key nonce plaintext

-- | Decrypts a versioned envelope. Authentication failures are distinct from
-- malformed or unsupported envelopes so callers can log private diagnostics
-- without exposing the cause in a public response.
decryptSecret :: SecretEncryptionKey -> Text -> CryptoFailable (Either SecretDecryptionError ByteString.ByteString)
decryptSecret (SecretEncryptionKey _ key) encodedEnvelope = do
  case first (const SecretDecryptionMalformedEnvelope) (Base64Url.decodeUnpadded (TextEncoding.encodeUtf8 encodedEnvelope)) >>= splitEnvelope of
    Left failure -> pure (Left failure)
    Right (nonce, authenticationTag, ciphertext) -> decrypt key nonce authenticationTag ciphertext

decryptSecretText :: SecretEncryptionKey -> Text -> CryptoFailable (Either SecretDecryptionError Text)
decryptSecretText encryptionKey encodedEnvelope = do
  plaintext <- decryptSecret encryptionKey encodedEnvelope
  pure (plaintext >>= first (const SecretDecryptionPlaintextIsNotUtf8) . TextEncoding.decodeUtf8')

encrypt :: AES256 -> ByteString.ByteString -> ByteString.ByteString -> CryptoFailable ByteString.ByteString
encrypt key nonce plaintext = do
  aead <- aeadInit AEAD_GCM key nonce
  let (authenticationTag, ciphertext) = aeadSimpleEncrypt aead associatedData plaintext authenticationTagLength
  pure (envelopeVersion <> nonce <> convert authenticationTag <> ciphertext)

decrypt :: AES256 -> ByteString.ByteString -> ByteString.ByteString -> ByteString.ByteString -> CryptoFailable (Either SecretDecryptionError ByteString.ByteString)
decrypt key nonce authenticationTag ciphertext = do
  aead <- aeadInit AEAD_GCM key nonce
  pure (maybe (Left SecretDecryptionAuthenticationFailed) Right (aeadSimpleDecrypt aead associatedData ciphertext (AuthTag (convert authenticationTag))))

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

envelopeVersion :: ByteString.ByteString
envelopeVersion = "\x01"

associatedData :: ByteString.ByteString
associatedData = "harch-web.secret.v1"

nonceLength :: Int
nonceLength = 12

authenticationTagLength :: Int
authenticationTagLength = 16
