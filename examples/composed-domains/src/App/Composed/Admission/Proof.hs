{-# LANGUAGE OverloadedStrings #-}

-- | Application-owned encrypted credential proof and rate-limit hand-off for
-- the composed admission flow.
--
-- This collaborator owns the TOTP proof and its admission-specific attempt
-- namespaces.  It deliberately exposes a typed, transport-neutral result:
-- page actions and native fallbacks interpret one rail, while Harch owns only
-- the generic cancellation-safe reservation lifecycle.
module App.Composed.Admission.Proof
  ( AdmissionAttemptAdmission (..),
    AdmissionAttemptBudget (..),
    AdmissionAttemptBudgets,
    AdmissionAttemptReservation (..),
    AdmissionAttemptScope (..),
    AdmissionAttemptStore (..),
    AdmissionAttemptStoreError (..),
    AdmissionCredentialStore (..),
    AdmissionCredentialStoreError (..),
    AdmissionPrincipalKey (..),
    AdmissionProofClockError (..),
    AdmissionProofConfig (..),
    AdmissionProofResult (..),
    StoredAdmissionCredential (..),
    admissionAttemptBudgetsToList,
    admissionAttemptScopeStorageKey,
    completeAdmissionProof,
  )
where

import App.Composed.Admission.Types
import Crypto.Error (maybeCryptoError)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Data.Word (Word64, Word8)
import HarchWeb.Authentication.Attempt qualified as Attempt
import HarchWeb.LoginProtection (LoginProtectionPolicy)
import HarchWeb.Secret (SecretEncryptionKey, decryptSecretText)
import HarchWeb.Security (ClientAddress, clientAddressText)
import HarchWeb.Time (UnixTimeNanoseconds)
import HarchWeb.Time qualified as Time
import HarchWeb.Totp (TotpCode, TotpSecret, mkTotpSecret, validateTotpCodeCounter)

data StoredAdmissionCredential = StoredAdmissionCredential
  { storedAdmissionPrincipalId :: AdmissionPrincipalId,
    storedAdmissionEncryptedTotpSecret :: EncryptedAdmissionTotpSecret,
    storedAdmissionLastUsedTotpCounter :: Maybe Word64
  }
  deriving (Eq)

instance Show StoredAdmissionCredential where
  show _ = "StoredAdmissionCredential <redacted>"

data AdmissionCredentialStore = AdmissionCredentialStore
  { findAdmissionCredential :: AdmissionLoginName -> IO (Either AdmissionCredentialStoreError (Maybe StoredAdmissionCredential)),
    markAdmissionTotpCounterUsed :: AdmissionPrincipalId -> Word64 -> IO (Either AdmissionCredentialStoreError Bool)
  }

data AdmissionCredentialStoreError
  = AdmissionCredentialStoreUnavailable
  | AdmissionCredentialStoreCorrupt
  deriving (Eq, Show)

-- | Admission namespaces cannot be confused with account password or MFA
-- protection. Unknown names remain validated, opaque inputs rather than
-- becoming an account or credential record.
data AdmissionAttemptScope
  = AdmissionPrincipalAttemptScope AdmissionPrincipalKey
  | AdmissionPeerAttemptScope ClientAddress
  deriving (Eq)

data AdmissionPrincipalKey
  = KnownAdmissionPrincipal AdmissionPrincipalId
  | UnknownAdmissionPrincipal AdmissionLoginName
  deriving (Eq)

data AdmissionAttemptBudget = AdmissionAttemptBudget
  { admissionAttemptScope :: AdmissionAttemptScope,
    admissionAttemptPolicy :: LoginProtectionPolicy
  }

newtype AdmissionAttemptBudgets = AdmissionAttemptBudgets (NonEmpty AdmissionAttemptBudget)

newtype AdmissionAttemptReservation = AdmissionAttemptReservation Text.Text
  deriving (Eq)

data AdmissionAttemptAdmission
  = AdmissionAttemptReserved AdmissionAttemptReservation
  | AdmissionAttemptThrottled UnixTimeNanoseconds
  deriving (Eq)

data AdmissionAttemptStore = AdmissionAttemptStore
  { reserveAdmissionAttempt :: AdmissionAttemptBudgets -> UnixTimeNanoseconds -> IO (Either AdmissionAttemptStoreError AdmissionAttemptAdmission),
    settleAdmissionAttempt :: AdmissionAttemptReservation -> Bool -> IO (Either AdmissionAttemptStoreError ()),
    cancelAdmissionAttempt :: AdmissionAttemptReservation -> IO (Either AdmissionAttemptStoreError ())
  }

data AdmissionAttemptStoreError
  = AdmissionAttemptStoreUnavailable
  | AdmissionAttemptStoreCorrupt
  deriving (Eq, Show)

data AdmissionProofConfig = AdmissionProofConfig
  { admissionProofCredentials :: AdmissionCredentialStore,
    admissionProofAttempts :: AdmissionAttemptStore,
    admissionProofPolicy :: LoginProtectionPolicy,
    admissionProofEncryptionKey :: SecretEncryptionKey,
    admissionProofReadClock :: IO (Either AdmissionProofClockError UnixTimeNanoseconds)
  }

instance Show AdmissionProofConfig where
  show _ = "AdmissionProofConfig <redacted>"

data AdmissionProofClockError
  = AdmissionProofClockUnavailable
  | AdmissionProofClockCorrupt
  deriving (Eq, Show)

data AdmissionProofResult
  = AdmissionProofAccepted AdmissionPrincipalId
  | AdmissionProofRejected
  | AdmissionProofReplayed
  | AdmissionProofThrottled
  | AdmissionProofUnavailable
  deriving (Eq, Show)

completeAdmissionProof :: AdmissionProofConfig -> ClientAddress -> AdmissionLoginName -> TotpCode -> IO AdmissionProofResult
completeAdmissionProof config clientAddress loginName suppliedCode = do
  credentialResult <- findAdmissionCredential (admissionProofCredentials config) loginName
  case credentialResult of
    Left _ -> pure AdmissionProofUnavailable
    Right maybeCredential -> do
      clockResult <- admissionProofReadClock config
      case clockResult of
        Left _ -> pure AdmissionProofUnavailable
        Right now ->
          runAdmittedAdmissionAttempt config (admissionAttemptBudgets config clientAddress loginName maybeCredential) now $
            verifyAdmissionProof config now maybeCredential suppliedCode

admissionAttemptBudgets :: AdmissionProofConfig -> ClientAddress -> AdmissionLoginName -> Maybe StoredAdmissionCredential -> AdmissionAttemptBudgets
admissionAttemptBudgets config clientAddress loginName maybeCredential =
  AdmissionAttemptBudgets
    ( AdmissionAttemptBudget principalScope policy
        NonEmpty.:| [AdmissionAttemptBudget (AdmissionPeerAttemptScope clientAddress) policy]
    )
  where
    policy = admissionProofPolicy config
    principalScope =
      AdmissionPrincipalAttemptScope $
        case maybeCredential of
          Just credential -> KnownAdmissionPrincipal (storedAdmissionPrincipalId credential)
          Nothing -> UnknownAdmissionPrincipal loginName

runAdmittedAdmissionAttempt :: AdmissionProofConfig -> AdmissionAttemptBudgets -> UnixTimeNanoseconds -> IO (AdmissionProofResult, Maybe Bool) -> IO AdmissionProofResult
runAdmittedAdmissionAttempt config budgets now =
  Attempt.runAdmittedAttempt attemptStore budgets (const AdmissionProofThrottled) (const AdmissionProofUnavailable)
  where
    store = admissionProofAttempts config
    attemptStore =
      Attempt.AttemptReservationStore
        { Attempt.reserveAttempt = \attemptBudgets -> fmap (fmap mapAdmission) (reserveAdmissionAttempt store attemptBudgets now),
          Attempt.settleAttempt = settleAdmissionAttempt store,
          Attempt.cancelAttempt = cancelAdmissionAttempt store
        }
    mapAdmission admission =
      case admission of
        AdmissionAttemptReserved reservation -> Attempt.AttemptReserved reservation
        AdmissionAttemptThrottled lockoutEndsAt -> Attempt.AttemptThrottled lockoutEndsAt

verifyAdmissionProof :: AdmissionProofConfig -> UnixTimeNanoseconds -> Maybe StoredAdmissionCredential -> TotpCode -> IO (AdmissionProofResult, Maybe Bool)
verifyAdmissionProof config now maybeCredential suppliedCode =
  case maybeCredential of
    Nothing -> pure (AdmissionProofRejected, Just False)
    Just credential ->
      case decodeAdmissionTotpSecret (admissionProofEncryptionKey config) (storedAdmissionEncryptedTotpSecret credential) of
        Nothing -> pure (AdmissionProofUnavailable, Nothing)
        Just secret ->
          case validateTotpCodeCounter (Time.unixTimeSecondsFromNanoseconds now) admissionTotpSkewPeriods secret suppliedCode of
            Nothing -> pure (AdmissionProofRejected, Just False)
            Just matchedCounter
              | maybe False (matchedCounter <=) (storedAdmissionLastUsedTotpCounter credential) -> pure (AdmissionProofReplayed, Just False)
              | otherwise -> do
                  marked <- markAdmissionTotpCounterUsed (admissionProofCredentials config) (storedAdmissionPrincipalId credential) matchedCounter
                  pure $
                    case marked of
                      Left _ -> (AdmissionProofUnavailable, Nothing)
                      Right True -> (AdmissionProofAccepted (storedAdmissionPrincipalId credential), Just True)
                      Right False -> (AdmissionProofReplayed, Just False)

admissionTotpSkewPeriods :: Word8
admissionTotpSkewPeriods = 1

decodeAdmissionTotpSecret :: SecretEncryptionKey -> EncryptedAdmissionTotpSecret -> Maybe TotpSecret
decodeAdmissionTotpSecret encryptionKey encryptedSecret =
  maybeCryptoError (decryptSecretText encryptionKey (encryptedAdmissionTotpSecretText encryptedSecret)) >>= either (const Nothing) mkTotpSecret

admissionAttemptScopeStorageKey :: AdmissionAttemptScope -> Text.Text
admissionAttemptScopeStorageKey scope =
  case scope of
    AdmissionPrincipalAttemptScope principal ->
      case principal of
        KnownAdmissionPrincipal principalId -> "admission-totp:known:" <> admissionPrincipalIdText principalId
        UnknownAdmissionPrincipal loginName -> "admission-totp:unknown:" <> Text.toLower (admissionLoginNameText loginName)
    AdmissionPeerAttemptScope clientAddress -> "admission-peer:" <> clientAddressText clientAddress

admissionAttemptBudgetsToList :: AdmissionAttemptBudgets -> NonEmpty AdmissionAttemptBudget
admissionAttemptBudgetsToList (AdmissionAttemptBudgets budgets) = budgets
