{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWeb.SecretSpec (spec) where

import Data.ByteString qualified as ByteString
import Data.ByteString.Base64.URL qualified as Base64Url
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Secret
import Test.Hspec
import TestCore.CustomAssertions (expectAll)

spec :: Spec
spec = do
  describe "SecretEncryptionKey" $ do
    it "accepts exactly one 256-bit base64url key" $ do
      expectAll
        ( (isJust (mkSecretEncryptionKey encodedKey) `shouldBe` True)
            :| [ isNothing (mkSecretEncryptionKey "not-base64") `shouldBe` True,
                 isNothing (mkSecretEncryptionKey (TextEncoding.decodeUtf8 (Base64Url.encodeUnpadded "short"))) `shouldBe` True,
                 requiredKey == requiredKey `shouldBe` True,
                 requiredKey /= otherKey `shouldBe` True
               ]
        )

  describe "AES-256-GCM secret envelopes" $ do
    it "round-trips a versioned encrypted value and rejects altered inputs" $ do
      let key = requiredKey
          nonce = requiredNonce "0123456789ab"
          plaintext = "authenticator-secret"
          envelope = requiredEnvelope (encryptSecretWithNonce key nonce (mkSecretPlaintext plaintext))
      expectRight plaintext (decryptSecret key envelope)
      randomEnvelope <- requiredEnvelope <$> encryptSecret key plaintext
      expectAll
        ( (expectRight plaintext (decryptSecret key randomEnvelope))
            :| [ expectRight "authenticator-secret" (decryptSecretText key envelope),
                 expectLeft SecretDecryptionPlaintextIsNotUtf8 (decryptSecretText key (requiredEnvelope (encryptSecretWithNonce key nonce (mkSecretPlaintext "\255")))),
                 expectLeft SecretDecryptionMalformedEnvelope (decryptSecret key "not-base64"),
                 expectLeft SecretDecryptionAuthenticationFailed (decryptSecret key (envelope <> "A")),
                 expectLeft (SecretDecryptionUnsupportedEnvelopeVersion 2) (decryptSecret key (encodedEnvelope "\x02")),
                 expectLeft SecretDecryptionMalformedEnvelope (decryptSecret key (encodedEnvelope "\x01")),
                 expectLeft SecretDecryptionAuthenticationFailed (decryptSecret otherKey envelope),
                 isNothing (mkEncryptionNonce "short") `shouldBe` True
               ]
        )

    it "keeps encryption and decryption error categories inspectable for private diagnostics" $
      expectAll
        ( (SecretEncryptionGeneratedInvalidNonce /= SecretEncryptionCipherInitializationFailed `shouldBe` True)
            :| [ SecretEncryptionAeadInitializationFailed /= SecretEncryptionGeneratedInvalidNonce `shouldBe` True,
                 SecretDecryptionMalformedEnvelope /= SecretDecryptionAuthenticationFailed `shouldBe` True,
                 SecretDecryptionUnsupportedEnvelopeVersion 1 /= SecretDecryptionUnsupportedEnvelopeVersion 2 `shouldBe` True,
                 SecretDecryptionCipherInitializationFailed /= SecretDecryptionAuthenticationFailed `shouldBe` True,
                 SecretDecryptionAeadInitializationFailed /= SecretDecryptionAuthenticationFailed `shouldBe` True
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

requiredEnvelope :: Either errorValue Text -> Text
requiredEnvelope encryptionResult =
  case encryptionResult of
    Right envelope -> envelope
    Left _ -> error "expected a valid secret envelope"

expectRight :: (Eq value, Show value) => value -> Either errorValue value -> Expectation
expectRight expected result =
  case result of
    Right actual -> actual `shouldBe` expected
    Left _ -> expectationFailure "expected secret operation to succeed"

expectLeft :: Eq errorValue => errorValue -> Either errorValue value -> Expectation
expectLeft expected result =
  case result of
    Left actual
      | actual == expected -> pure ()
      | otherwise -> expectationFailure "secret operation returned the wrong failure category"
    Right _ -> expectationFailure "expected secret operation to fail"

encodedEnvelope :: ByteString.ByteString -> Text
encodedEnvelope = TextEncoding.decodeUtf8 . Base64Url.encodeUnpadded

encodedKey :: Text
encodedKey = TextEncoding.decodeUtf8 (Base64Url.encodeUnpadded (ByteString.replicate 32 1))

otherEncodedKey :: Text
otherEncodedKey = TextEncoding.decodeUtf8 (Base64Url.encodeUnpadded (ByteString.replicate 32 2))
