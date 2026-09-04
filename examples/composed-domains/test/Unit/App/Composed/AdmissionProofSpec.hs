{-# LANGUAGE OverloadedStrings #-}

module Unit.App.Composed.AdmissionProofSpec (spec) where

import App.Composed
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.LoginProtection (defaultLoginProtectionPolicy)
import HarchWeb.Secret (mkSecretEncryptionKey)
import HarchWeb.Security (defaultClientAddress)
import HarchWeb.Time (unixTimeNanoseconds)
import HarchWeb.Totp (mkTotpCode)
import Test.Hspec

spec :: Spec
spec = describe "Unit.App.Composed.AdmissionProof" $ do
  it "keeps every unavailable, throttled, unknown, and malformed-proof outcome on its explicit rail" $ do
    loginName <- required "admission login" (mkAdmissionLoginName "support_operator")
    principalId <- required "admission principal" (mkAdmissionPrincipalId "support-principal")
    invalidEnvelope <- required "invalid envelope wrapper" (mkEncryptedAdmissionTotpSecret "not-an-encrypted-envelope")
    encryptionKey <- required "encryption key" (mkSecretEncryptionKey "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
    code <- required "TOTP code" (mkTotpCode "123456")
    settlements <- newIORef []
    cancellations <- newIORef (0 :: Int)
    reservationScopes <- newIORef ([] :: [[Text]])
    let store reservationResult =
          AdmissionAttemptStore
            { reserveAdmissionAttempt = \budgets _ -> do
                modifyIORef'
                  reservationScopes
                  ( <>
                      [ map
                          (admissionAttemptScopeStorageKey . admissionAttemptScope)
                          (NonEmpty.toList (admissionAttemptBudgetsToList budgets))
                      ]
                  )
                pure reservationResult,
              settleAdmissionAttempt = \_ succeeded -> modifyIORef' settlements (<> [succeeded]) >> pure (Right ()),
              cancelAdmissionAttempt = \_ -> modifyIORef' cancellations (+ 1) >> pure (Right ())
            }
        credentials result =
          AdmissionCredentialStore
            { findAdmissionCredential = \_ -> pure result,
              markAdmissionTotpCounterUsed = \_ _ -> pure (Right True)
            }
        config credentialResult reservationResult clockResult =
          AdmissionProofConfig
            { admissionProofCredentials = credentials credentialResult,
              admissionProofAttempts = store reservationResult,
              admissionProofPolicy = defaultLoginProtectionPolicy,
              admissionProofEncryptionKey = encryptionKey,
              admissionProofReadClock = pure clockResult
            }
        reserved = Right (AdmissionAttemptReserved (AdmissionAttemptReservation "attempt-1"))
        malformedCredential = StoredAdmissionCredential principalId invalidEnvelope Nothing
    completeAdmissionProof (config (Left AdmissionCredentialStoreUnavailable) reserved (Right (unixTimeNanoseconds 100))) defaultClientAddress loginName code
      `shouldReturn` AdmissionProofUnavailable
    completeAdmissionProof (config (Right Nothing) reserved (Left AdmissionProofClockUnavailable)) defaultClientAddress loginName code
      `shouldReturn` AdmissionProofUnavailable
    completeAdmissionProof (config (Right Nothing) (Left AdmissionAttemptStoreUnavailable) (Right (unixTimeNanoseconds 100))) defaultClientAddress loginName code
      `shouldReturn` AdmissionProofUnavailable
    completeAdmissionProof (config (Right Nothing) (Right (AdmissionAttemptThrottled (unixTimeNanoseconds 300))) (Right (unixTimeNanoseconds 100))) defaultClientAddress loginName code
      `shouldReturn` AdmissionProofThrottled
    completeAdmissionProof (config (Right Nothing) reserved (Right (unixTimeNanoseconds 100))) defaultClientAddress loginName code
      `shouldReturn` AdmissionProofRejected
    completeAdmissionProof (config (Right (Just malformedCredential)) reserved (Right (unixTimeNanoseconds 100))) defaultClientAddress loginName code
      `shouldReturn` AdmissionProofUnavailable
    readIORef settlements `shouldReturn` [False]
    readIORef cancellations `shouldReturn` 1
    let expectedUnknownScopes = [Text.pack "admission-totp:unknown:support_operator", Text.pack "admission-peer:127.0.0.1"]
    recordedScopes <- readIORef reservationScopes
    recordedScopes `shouldSatisfy` elem expectedUnknownScopes

required :: String -> Maybe value -> IO value
required label = maybe (expectationFailure ("expected " <> label) >> fail "unreachable") pure
