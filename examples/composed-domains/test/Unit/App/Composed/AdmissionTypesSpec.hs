{-# LANGUAGE OverloadedStrings #-}

module Unit.App.Composed.AdmissionTypesSpec (spec) where

import App.Composed
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text qualified as Text
import HarchWeb.Session (mkSessionId)
import HarchWeb.Time (unixTimeNanoseconds)
import Test.Hspec
import TestCore.CustomAssertions (expectAll)

spec :: Spec
spec = describe "Unit.App.Composed.AdmissionTypes" $ do
  it "validates and preserves bounded opaque admission values while redacting diagnostics" $ do
    loginName <- required "admission login name" (mkAdmissionLoginName "support_OPERATOR-01")
    principalId <- required "admission principal ID" (mkAdmissionPrincipalId "operator_01")
    encryptedSecret <- required "encrypted admission secret" (mkEncryptedAdmissionTotpSecret "v1:encrypted-envelope")
    sessionId <- required "admission session ID" (mkSessionId "0123456789abcdef0123456789abcdef")
    let admissionSessionId = mkAdmissionSessionId sessionId
        expiresAt = unixTimeNanoseconds 1234
        principal = mkAdmissionPrincipal principalId admissionSessionId expiresAt
    expectAll
      ( (admissionLoginNameText loginName `shouldBe` "support_OPERATOR-01")
          :| [ admissionPrincipalIdText principalId `shouldBe` "operator_01",
               encryptedAdmissionTotpSecretText encryptedSecret `shouldBe` "v1:encrypted-envelope",
               unAdmissionSessionId admissionSessionId `shouldBe` sessionId,
               admissionPrincipalId principal `shouldBe` principalId,
               admissionPrincipalSessionId principal `shouldBe` admissionSessionId,
               admissionPrincipalSessionExpiresAt principal `shouldBe` expiresAt,
               show loginName `shouldBe` "AdmissionLoginName <redacted>",
               show principalId `shouldBe` "AdmissionPrincipalId <redacted>",
               show encryptedSecret `shouldBe` "EncryptedAdmissionTotpSecret <redacted>",
               show admissionSessionId `shouldBe` "AdmissionSessionId <redacted>",
               show principal `shouldBe` "AdmissionPrincipal <redacted>"
             ]
      )

  it "rejects malformed and overlong login, principal, and encrypted-secret values" $ do
    expectAll
      ( (mkAdmissionLoginName "" `shouldBe` Nothing)
          :| [ mkAdmissionLoginName "invalid login" `shouldBe` Nothing,
               mkAdmissionLoginName "invalid!" `shouldBe` Nothing,
               mkAdmissionLoginName (Text.replicate 129 "a") `shouldBe` Nothing,
               mkAdmissionPrincipalId "" `shouldBe` Nothing,
               mkAdmissionPrincipalId "invalid principal" `shouldBe` Nothing,
               mkAdmissionPrincipalId "invalid!" `shouldBe` Nothing,
               mkAdmissionPrincipalId (Text.replicate 129 "a") `shouldBe` Nothing,
               mkEncryptedAdmissionTotpSecret "" `shouldBe` Nothing,
               mkEncryptedAdmissionTotpSecret (Text.replicate 4097 "a") `shouldBe` Nothing
             ]
      )

required :: String -> Maybe value -> IO value
required label = maybe (expectationFailure ("expected " <> label) >> fail "unreachable") pure
