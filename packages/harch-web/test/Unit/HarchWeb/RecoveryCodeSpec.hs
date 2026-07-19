{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWeb.RecoveryCodeSpec (spec) where

import Data.ByteString qualified as ByteString
import Data.Maybe (isNothing)
import Data.Text qualified as Text
import HarchWeb.Password (defaultPasswordHashingPolicy)
import HarchWeb.RecoveryCode
import Test.Hspec

spec :: Spec
spec = do
  describe "RecoveryCode" $ do
    it "canonicalizes user-entered hexadecimal codes and generates 80-bit codes" $ do
      recoveryCodeText knownCode `shouldBe` "12345-6789A-BCDEF-01234"
      knownCode == knownCode `shouldBe` True
      knownCode /= otherCode `shouldBe` True
      fmap recoveryCodeText (mkRecoveryCode "12345-6789a-bcdef-01234") `shouldBe` Just (recoveryCodeText knownCode)
      fmap recoveryCodeText (mkRecoveryCode "123456789ABCDEF01234") `shouldBe` Just (recoveryCodeText knownCode)
      map (isNothing . mkRecoveryCode) ["", "12345-6789A-BCDEF-0123", "12345-6789A-BCDEF-012345", "12345-6789A-BCDEF-0123G"] `shouldBe` [True, True, True, True]
      generatedCode <- generateRecoveryCode
      fmap recoveryCodeText (mkRecoveryCode (recoveryCodeText generatedCode)) `shouldBe` Just (recoveryCodeText generatedCode)

  describe "RecoveryCodeHash" $ do
    it "stores an Argon2id verifier without retaining the recovery code" $ do
      let maybeHash = hashRecoveryCodeWithSalt defaultPasswordHashingPolicy (ByteString.replicate 16 1) knownCode
          hashValue = required maybeHash
      Text.isInfixOf "12345" (recoveryCodeHashText hashValue) `shouldBe` False
      fmap recoveryCodeHashText (readRecoveryCodeHash (recoveryCodeHashText hashValue)) `shouldBe` Just (recoveryCodeHashText hashValue)
      isNothing (readRecoveryCodeHash "not-an-argon2id-hash") `shouldBe` True
      verifyRecoveryCode knownCode hashValue `shouldBe` True
      verifyRecoveryCode otherCode hashValue `shouldBe` False
      maybeRandomHash <- hashRecoveryCode defaultPasswordHashingPolicy knownCode
      fmap (verifyRecoveryCode knownCode) maybeRandomHash `shouldBe` Just True

knownCode :: RecoveryCode
knownCode = required (mkRecoveryCode "12345-6789A-BCDEF-01234")

otherCode :: RecoveryCode
otherCode = required (mkRecoveryCode "12345-6789A-BCDEF-01235")

required :: Maybe value -> value
required maybeValue =
  case maybeValue of
    Just value -> value
    Nothing -> error "expected a valid recovery-code value"
