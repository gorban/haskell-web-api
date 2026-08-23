{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Crypto.Error (CryptoFailable, maybeCryptoError)
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64.URL qualified as Base64Url
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Secret

spec = do
  describe "SecretEncryptionKey" $ do
    it "accepts exactly one 256-bit base64url key" $ do
      expectAll
        ( (isJust (mkSecretEncryptionKey encodedKey) `shouldBe` True)
            :| [ isNothing (mkSecretEncryptionKey "not-base64") `shouldBe` True,
                 isNothing (mkSecretEncryptionKey (TextEncoding.decodeUtf8 (Base64Url.encodeUnpadded "short"))) `shouldBe` True,
                 isNothing (mkSecretEncryptionKey (TextEncoding.decodeUtf8 (Base64Url.encodeUnpadded (ByteString.replicate 16 1)))) `shouldBe` True,
                 requiredKey /= otherKey `shouldBe` True
               ]
        )

  describe "AES-256-GCM secret envelopes" $ do
    it "round-trips a versioned encrypted value and rejects altered inputs" $ do
      let key = requiredKey
          nonce = requiredNonce "0123456789ab"
          plaintext = "authenticator-secret"
          envelope = requiredCrypto (encryptSecretWithNonce key nonce (mkSecretPlaintext plaintext))
      expectRight plaintext (requiredCrypto (decryptSecret key envelope))
      randomEnvelope <- requiredCrypto <$> encryptSecret key plaintext
      expectAll
        ( expectRight plaintext (requiredCrypto (decryptSecret key randomEnvelope))
            :| [ expectRight "authenticator-secret" (requiredCrypto (decryptSecretText key envelope)),
                 expectLeft SecretDecryptionPlaintextIsNotUtf8 (requiredCrypto (decryptSecretText key (requiredCrypto (encryptSecretWithNonce key nonce (mkSecretPlaintext "\255"))))),
                 expectLeft SecretDecryptionMalformedEnvelope (requiredCrypto (decryptSecret key "not-base64")),
                 expectLeft SecretDecryptionMalformedEnvelope (requiredCrypto (decryptSecret key (encodedEnvelope ""))),
                 expectLeft SecretDecryptionAuthenticationFailed (requiredCrypto (decryptSecret key (envelope <> "A"))),
                 expectLeft (SecretDecryptionUnsupportedEnvelopeVersion 2) (requiredCrypto (decryptSecret key (encodedEnvelope "\x02"))),
                 expectLeft SecretDecryptionMalformedEnvelope (requiredCrypto (decryptSecret key (encodedEnvelope "\x01"))),
                 expectLeft SecretDecryptionAuthenticationFailed (requiredCrypto (decryptSecret otherKey envelope)),
                 isNothing (mkEncryptionNonce "short") `shouldBe` True
               ]
        )

    it "keeps decryption error categories inspectable for private diagnostics" $
      expectAll
        ( (SecretDecryptionMalformedEnvelope /= SecretDecryptionAuthenticationFailed `shouldBe` True)
            :| [ SecretDecryptionUnsupportedEnvelopeVersion 1 /= SecretDecryptionUnsupportedEnvelopeVersion 2 `shouldBe` True,
                 SecretDecryptionPlaintextIsNotUtf8 /= SecretDecryptionAuthenticationFailed `shouldBe` True
               ]
        )

isJust :: Maybe value -> Bool
isJust maybeValue =
  case maybeValue of
    Just _ -> True
    Nothing -> False

isNothing :: Maybe value -> Bool
isNothing maybeValue = not (isJust maybeValue)

requiredKey :: SecretEncryptionKey
requiredKey =
  case mkSecretEncryptionKey encodedKey of
    Just key -> key
    Nothing -> error "expected a valid encryption key"

otherKey :: SecretEncryptionKey
otherKey =
  case mkSecretEncryptionKey otherEncodedKey of
    Just key -> key
    Nothing -> error "expected a valid encryption key"

requiredNonce :: ByteString.ByteString -> EncryptionNonce
requiredNonce bytes =
  case mkEncryptionNonce bytes of
    Just nonce -> nonce
    Nothing -> error "expected a valid encryption nonce"

expectRight :: (Eq value, Show value) => value -> Either errorValue value -> Expectation
expectRight expected result =
  case result of
    Right actual -> actual `shouldBe` expected
    Left _ -> expectationFailure "expected secret operation to succeed"

expectLeft :: (Eq errorValue) => errorValue -> Either errorValue value -> Expectation
expectLeft expected result =
  case result of
    Left actual
      | actual == expected -> pure ()
      | otherwise -> expectationFailure "secret operation returned the wrong failure category"
    Right _ -> expectationFailure "expected secret operation to fail"

requiredCrypto :: CryptoFailable value -> value
requiredCrypto = fromMaybe (error "expected cryptographic operation to succeed") . maybeCryptoError

encodedEnvelope :: ByteString.ByteString -> Text
encodedEnvelope = TextEncoding.decodeUtf8 . Base64Url.encodeUnpadded

encodedKey :: Text
encodedKey = TextEncoding.decodeUtf8 (Base64Url.encodeUnpadded (ByteString.replicate 32 1))

otherEncodedKey :: Text
otherEncodedKey = TextEncoding.decodeUtf8 (Base64Url.encodeUnpadded (ByteString.replicate 32 2))
