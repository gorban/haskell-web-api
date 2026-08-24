{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isNothing)
import Data.Text qualified as Text
import HarchWeb.Password (defaultPasswordHashingPolicy)
import HarchWeb.RecoveryCode

spec = do
  describe "RecoveryCode" $ do
    it "canonicalizes user-entered hexadecimal codes and generates 80-bit codes" $ do
      expectAll
        ( (recoveryCodeText knownCode `shouldBe` "12345-6789A-BCDEF-01234")
            :| [ knownCode /= otherCode `shouldBe` True,
                 -- 'deriving' only writes '=='; GHC's HPC instrumentation
                 -- attributes the same-value '==' path to its own box,
                 -- separate from the different-value path above. Comparing
                 -- two independently-parsed-but-equal values (rather than a
                 -- bare self-comparison) exercises it without proving
                 -- nothing.
                 mkRecoveryCode "12345-6789a-bcdef-01234" == Just knownCode `shouldBe` True,
                 fmap recoveryCodeText (mkRecoveryCode "12345-6789a-bcdef-01234") `shouldBe` Just (recoveryCodeText knownCode),
                 fmap recoveryCodeText (mkRecoveryCode "123456789ABCDEF01234") `shouldBe` Just (recoveryCodeText knownCode),
                 map (isNothing . mkRecoveryCode) ["", "12345-6789A-BCDEF-0123", "12345-6789A-BCDEF-012345", "12345-6789A-BCDEF-0123G"] `shouldBe` [True, True, True, True]
               ]
        )
      generatedCode <- generateRecoveryCode
      fmap recoveryCodeText (mkRecoveryCode (recoveryCodeText generatedCode)) `shouldBe` Just (recoveryCodeText generatedCode)

  describe "RecoveryCodeHash" $ do
    it "stores an Argon2id verifier without retaining the recovery code" $ do
      let maybeHash = hashRecoveryCodeWithSalt defaultPasswordHashingPolicy (ByteString.replicate 16 1) knownCode
          hashValue = required maybeHash
      expectAll
        ( (Text.isInfixOf "12345" (recoveryCodeHashText hashValue) `shouldBe` False)
            :| [ fmap recoveryCodeHashText (readRecoveryCodeHash (recoveryCodeHashText hashValue)) `shouldBe` Just (recoveryCodeHashText hashValue),
                 fmap recoveryCodeHashWorkKibibytes (readRecoveryCodeHash (recoveryCodeHashText hashValue)) `shouldBe` Just 65536,
                 recoveryCodeHashWorkKibibytes hashValue `shouldBe` 65536,
                 isNothing (readRecoveryCodeHash "not-an-argon2id-hash") `shouldBe` True,
                 verifyRecoveryCode knownCode hashValue `shouldBe` True,
                 verifyRecoveryCode otherCode hashValue `shouldBe` False
               ]
        )
      maybeRandomHash <- hashRecoveryCode defaultPasswordHashingPolicy knownCode
      fmap (verifyRecoveryCode knownCode) maybeRandomHash `shouldBe` Just True
      fmap recoveryCodeHashWorkKibibytes maybeRandomHash `shouldBe` Just 65536

knownCode :: RecoveryCode
knownCode = required (mkRecoveryCode "12345-6789A-BCDEF-01234")

otherCode :: RecoveryCode
otherCode = required (mkRecoveryCode "12345-6789A-BCDEF-01235")

required :: Maybe value -> value
required maybeValue =
  case maybeValue of
    Just value -> value
    Nothing -> error "expected a valid recovery-code value"
