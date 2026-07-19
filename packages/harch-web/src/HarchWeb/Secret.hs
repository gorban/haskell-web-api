{-# LANGUAGE OverloadedStrings #-}

module HarchWeb.Secret
  ( SecretEncryptionKey,
    decryptSecret,
    encryptSecret,
    encryptSecretWithNonce,
    mkSecretEncryptionKey,
  )
where

import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (AEADMode (AEAD_GCM), AuthTag (..), aeadInit, aeadSimpleDecrypt, aeadSimpleEncrypt, cipherInit)
import Crypto.Error (CryptoFailable, throwCryptoError)
import Crypto.Random.Entropy (getEntropy)
import Data.ByteArray (convert)
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64.URL qualified as Base64Url
import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding

newtype SecretEncryptionKey = SecretEncryptionKey ByteString.ByteString
  deriving (Eq)

mkSecretEncryptionKey :: Text -> Maybe SecretEncryptionKey
mkSecretEncryptionKey encodedKey = do
  key <- either (const Nothing) Just (Base64Url.decodeUnpadded (TextEncoding.encodeUtf8 encodedKey))
  if ByteString.length key == 32
    then Just (SecretEncryptionKey key)
    else Nothing

encryptSecret :: SecretEncryptionKey -> ByteString.ByteString -> IO (Maybe Text)
encryptSecret encryptionKey plaintext = do
  nonce <- getEntropy nonceLength
  pure (encryptSecretWithNonce encryptionKey nonce plaintext)

encryptSecretWithNonce :: SecretEncryptionKey -> ByteString.ByteString -> ByteString.ByteString -> Maybe Text
encryptSecretWithNonce (SecretEncryptionKey key) nonce plaintext =
  TextEncoding.decodeUtf8 . Base64Url.encodeUnpadded <$> encrypt key nonce plaintext

decryptSecret :: SecretEncryptionKey -> Text -> Maybe ByteString.ByteString
decryptSecret (SecretEncryptionKey key) encodedEnvelope = do
  envelope <- either (const Nothing) Just (Base64Url.decodeUnpadded (TextEncoding.encodeUtf8 encodedEnvelope))
  (nonce, authenticationTag, ciphertext) <- splitEnvelope envelope
  decrypt key nonce authenticationTag ciphertext

encrypt :: ByteString.ByteString -> ByteString.ByteString -> ByteString.ByteString -> Maybe ByteString.ByteString
encrypt key nonce plaintext =
  if ByteString.length nonce /= nonceLength
    then Nothing
    else
      let cipher = throwCryptoError (cipherInit key :: CryptoFailable AES256)
          aead = throwCryptoError (aeadInit AEAD_GCM cipher nonce)
          (authenticationTag, ciphertext) = aeadSimpleEncrypt aead associatedData plaintext authenticationTagLength
       in Just (envelopeVersion <> nonce <> convert authenticationTag <> ciphertext)

decrypt :: ByteString.ByteString -> ByteString.ByteString -> ByteString.ByteString -> ByteString.ByteString -> Maybe ByteString.ByteString
decrypt key nonce authenticationTag ciphertext =
  let cipher = throwCryptoError (cipherInit key :: CryptoFailable AES256)
      aead = throwCryptoError (aeadInit AEAD_GCM cipher nonce)
   in aeadSimpleDecrypt aead associatedData ciphertext (AuthTag (convert authenticationTag))

splitEnvelope :: ByteString.ByteString -> Maybe (ByteString.ByteString, ByteString.ByteString, ByteString.ByteString)
splitEnvelope envelope = do
  (version, remainder) <- ByteString.uncons envelope
  if version /= 1
    then Nothing
    else do
      let (nonce, afterNonce) = ByteString.splitAt nonceLength remainder
          (authenticationTag, ciphertext) = ByteString.splitAt authenticationTagLength afterNonce
      if ByteString.length nonce == nonceLength && ByteString.length authenticationTag == authenticationTagLength
        then Just (nonce, authenticationTag, ciphertext)
        else Nothing

envelopeVersion :: ByteString.ByteString
envelopeVersion = "\x01"

associatedData :: ByteString.ByteString
associatedData = "harch-web.secret.v1"

nonceLength :: Int
nonceLength = 12

authenticationTagLength :: Int
authenticationTagLength = 16
