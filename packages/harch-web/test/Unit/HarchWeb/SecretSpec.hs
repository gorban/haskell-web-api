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
          nonce = mkEncryptionNonce "0123456789ab"
          plaintext = "authenticator-secret"
          envelope = requiredEnvelope (encryptSecretWithNonce key nonce (mkSecretPlaintext plaintext))
      decryptSecret key envelope `shouldBe` Just plaintext
      maybeRandomEnvelope <- encryptSecret key plaintext
      expectAll
        ( ((maybeRandomEnvelope >>= decryptSecret key) `shouldBe` Just plaintext)
            :| [ decryptSecretText key envelope `shouldBe` Just "authenticator-secret",
                 decryptSecretText key (requiredEnvelope (encryptSecretWithNonce key nonce (mkSecretPlaintext "\255"))) `shouldBe` Nothing,
                 decryptSecret key "not-base64" `shouldBe` Nothing,
                 decryptSecret key (envelope <> "A") `shouldBe` Nothing,
                 decryptSecret key (encodedEnvelope "\x02") `shouldBe` Nothing,
                 decryptSecret key (encodedEnvelope "\x01") `shouldBe` Nothing,
                 decryptSecret otherKey envelope `shouldBe` Nothing,
                 encryptSecretWithNonce key (mkEncryptionNonce "short") (mkSecretPlaintext plaintext) `shouldBe` Nothing
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

requiredEnvelope :: Maybe Text -> Text
requiredEnvelope maybeEnvelope =
  case maybeEnvelope of
    Just envelope -> envelope
    Nothing -> error "expected a valid secret envelope"

encodedEnvelope :: ByteString.ByteString -> Text
encodedEnvelope = TextEncoding.decodeUtf8 . Base64Url.encodeUnpadded

encodedKey :: Text
encodedKey = TextEncoding.decodeUtf8 (Base64Url.encodeUnpadded (ByteString.replicate 32 1))

otherEncodedKey :: Text
otherEncodedKey = TextEncoding.decodeUtf8 (Base64Url.encodeUnpadded (ByteString.replicate 32 2))
