{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Unit.App.Composed.AdmissionSetupSpec (spec) where

import App.Composed
import Crypto.Error (maybeCryptoError)
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import HarchWeb.Password qualified as Password
import HarchWeb.Secret (decryptSecretText, mkSecretEncryptionKey)
import Test.Hspec

spec :: Spec
spec = describe "Unit.App.Composed.AdmissionSetup" $ do
  it "accepts only the closed migration and operator-provisioning commands" $ do
    parseAdmissionSetupCommand ["migrate"] `shouldBe` Right MigrateAdmissionDatabase
    parseAdmissionSetupCommand ["seed-example-api-client"] `shouldBe` Right SeedComposedExampleApiClient
    parseAdmissionSetupCommand ["provision", "support-operator", "support_operator"]
      `shouldSatisfy` \case Right (ProvisionAdmissionCredential _ _) -> True; _ -> False
    parseAdmissionSetupCommand ["provision", "invalid principal", "support_operator"]
      `shouldBe` Left AdmissionSetupInvalidPrincipal
    parseAdmissionSetupCommand ["provision", "support-operator", "invalid login"]
      `shouldBe` Left AdmissionSetupInvalidLogin
    parseAdmissionSetupCommand ["provision", "support-operator", "support_operator", "JBSWY3DPEHPK3PXPJ"]
      `shouldBe` Left AdmissionSetupInvalidCommand

  it "encrypts a valid terminal TOTP secret without exposing it in setup diagnostics" $ do
    let encryptionKey = fromMaybe (error "expected test encryption key") (mkSecretEncryptionKey "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
    encryptedSecret <- encryptAdmissionTotpSecret encryptionKey "GEZDGNBVGY3TQOJQGEZDGNBVGY3TQOJQ"
    show encryptedSecret `shouldBe` "Right EncryptedAdmissionTotpSecret <redacted>"
    case fmap (maybeCryptoError . decryptSecretText encryptionKey . encryptedAdmissionTotpSecretText) encryptedSecret of
      Right (Just (Right plaintext)) -> plaintext `shouldBe` "GEZDGNBVGY3TQOJQGEZDGNBVGY3TQOJQ"
      _ -> expectationFailure "expected the provisioned envelope to decrypt to the canonical TOTP secret"
    encryptAdmissionTotpSecret encryptionKey "not-a-totp-secret" `shouldReturn` Left AdmissionSetupInvalidTotpSecret

  it "hashes the prompted example client secret with the unknown-client Argon2id cost" $ do
    hashed <- hashComposedExampleApiClientSecret "one-time-example-secret"
    case hashed of
      Left failure -> expectationFailure (renderAdmissionSetupError failure)
      Right passwordHash -> do
        Password.passwordHashWorkKibibytes passwordHash `shouldBe` Just 65536
        Text.isPrefixOf "$argon2id$v=19$m=65536,t=3,p=1$" (Password.passwordHashText passwordHash) `shouldBe` True
    fmap isInvalidApiClientSecret (hashComposedExampleApiClientSecret "") `shouldReturn` True
    fmap isInvalidApiClientSecret (hashComposedExampleApiClientSecret (Text.replicate 3001 "x")) `shouldReturn` True

  it "renders only stable setup errors" $ do
    map renderAdmissionSetupError [AdmissionSetupInvalidCommand, AdmissionSetupInvalidPrincipal, AdmissionSetupInvalidLogin, AdmissionSetupInvalidTotpSecret, AdmissionSetupSecretInputMustBeTerminal, AdmissionSetupConfigUnavailable, AdmissionSetupMigrationFailed, AdmissionSetupCredentialProvisionFailed, AdmissionSetupApiClientSecretInputMustBeTerminal, AdmissionSetupApiClientSecretInvalid, AdmissionSetupApiClientSecretHashFailed, AdmissionSetupApiClientProvisionFailed]
      `shouldBe` [ "Expected: migrate, provision <principal-id> <login-name>, or seed-example-api-client.",
                   "The admission principal identifier is invalid.",
                   "The admission login name is invalid.",
                   "The supplied TOTP secret is invalid.",
                   "TOTP secret input requires an interactive terminal.",
                   "The composed admission deployment configuration is unavailable.",
                   "Unable to apply composed admission database changes.",
                   "Unable to provision the admission credential.",
                   "OAuth client secret input requires an interactive terminal.",
                   "The supplied OAuth client secret is invalid.",
                   "Unable to prepare the OAuth client credential.",
                   "Unable to provision the OAuth client."
                 ]

isInvalidApiClientSecret :: Either AdmissionSetupError value -> Bool
isInvalidApiClientSecret result =
  case result of
    Left AdmissionSetupApiClientSecretInvalid -> True
    _ -> False
