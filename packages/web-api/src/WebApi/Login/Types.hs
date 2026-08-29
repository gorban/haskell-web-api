-- | Public login capabilities and outcomes.
--
-- Decision (FQ6, 2026-08-29): the password and MFA stages share a stable
-- login environment rather than accepting a transposable sequence of stores,
-- clocks, policy, work gate, and rehasher.  The public 'WebApi.Login' facade
-- keeps the one transport-facing protocol while private modules own each
-- lifecycle stage.  This is a responsibility split, not a metric-only move:
-- reservation ownership remains one shared operation and only the inputs
-- that are invariant across an attempt live in these records.
module WebApi.Login.Types
  ( AccountCredential (..),
    AccountCredentialStore (..),
    AccountCredentialStoreError (..),
    LoginAttemptAdmission (..),
    LoginAttemptReservation (..),
    LoginAttemptStore (..),
    LoginAttemptStoreError (..),
    LoginIdentifier (..),
    LoginThrottleContext (..),
    MfaLoginProof (..),
    PasswordLoginEnvironment (..),
    PasswordRehasher (..),
    PasswordMfaLoginResult (..),
    PasswordLoginResult (..),
    SecondFactorContext (..),
    defaultPasswordRehasher,
  )
where

import Data.Text (Text)
import HarchWeb.Account (AccountId)
import HarchWeb.Email (EmailAddress)
import HarchWeb.LoginProtection (LoginProtectionPolicy)
import HarchWeb.Password (Password, PasswordHash, PasswordHashingPolicy, PasswordWorkGate, defaultPasswordHashingPolicy, hashPassword)
import HarchWeb.RecoveryCode (RecoveryCode)
import HarchWeb.Secret (SecretEncryptionKey)
import HarchWeb.Time (UnixTimeNanoseconds, UnixTimeSeconds)
import HarchWeb.Totp (TotpCode)
import HarchWeb.Username (Username)
import WebApi.Mfa (MfaStore, MfaStoreError)

data AccountCredential = AccountCredential
  { accountCredentialId :: AccountId,
    accountCredentialPasswordHash :: PasswordHash,
    accountCredentialEmailVerified :: Bool
  }

data AccountCredentialStoreError
  = AccountCredentialStoreUnavailable Text
  | AccountCredentialStoreCorruptData Text
  deriving (Eq)

data AccountCredentialStore = AccountCredentialStore
  { findAccountCredentialByEmail :: EmailAddress -> IO (Either AccountCredentialStoreError (Maybe AccountCredential)),
    findAccountCredentialByUsername :: Username -> IO (Either AccountCredentialStoreError (Maybe AccountCredential)),
    -- | Replace only the exact verified legacy hash.  @False@ means another
    -- successful login (or a password change) has already superseded it;
    -- that is a completed best-effort upgrade, not a login failure.
    replacePasswordHashIfCurrent :: AccountId -> PasswordHash -> PasswordHash -> IO (Either AccountCredentialStoreError Bool)
  }

data LoginIdentifier
  = LoginEmailAddress EmailAddress
  | LoginUsername Username

-- | The current-policy hash operation used only after an existing credential
-- has already verified. Keeping it injectable makes optional migration
-- failure explicit and testable without making persistence responsible for
-- Argon2 work.
data PasswordRehasher = PasswordRehasher
  { passwordRehashingPolicy :: PasswordHashingPolicy,
    rehashVerifiedPassword :: PasswordHashingPolicy -> Password -> IO (Maybe PasswordHash)
  }

defaultPasswordRehasher :: PasswordRehasher
defaultPasswordRehasher = PasswordRehasher defaultPasswordHashingPolicy hashPassword

-- | A new capability record, rather than an extension of either credential
-- or MFA storage: throttling must also cover identifiers without an account.
data LoginAttemptStoreError
  = LoginAttemptStoreUnavailable Text
  | LoginAttemptStoreCorruptData Text
  deriving (Eq)

newtype LoginAttemptReservation = LoginAttemptReservation Text
  deriving (Eq)

data LoginAttemptAdmission
  = LoginAttemptReserved LoginAttemptReservation
  | LoginAttemptThrottled UnixTimeNanoseconds
  deriving (Eq)

-- | A PostgreSQL implementation reserves and settles an admitted failure
-- atomically. The shared lifecycle owner makes cancellation-safe handoffs;
-- durable cleanup after process loss remains separate retention work.
data LoginAttemptStore = LoginAttemptStore
  { reserveLoginAttempt :: Text -> LoginProtectionPolicy -> UnixTimeNanoseconds -> IO (Either LoginAttemptStoreError LoginAttemptAdmission),
    settleLoginAttempt :: LoginAttemptReservation -> Bool -> IO (Either LoginAttemptStoreError ()),
    cancelLoginAttempt :: LoginAttemptReservation -> IO (Either LoginAttemptStoreError ())
  }

data LoginThrottleContext = LoginThrottleContext
  { loginThrottleStore :: LoginAttemptStore,
    loginThrottlePolicy :: LoginProtectionPolicy,
    loginThrottleNow :: UnixTimeNanoseconds
  }

-- | Invariants shared by one password-stage attempt. The caller chooses the
-- concrete rehasher at construction time; application code normally uses
-- 'defaultPasswordRehasher'.
data PasswordLoginEnvironment = PasswordLoginEnvironment
  { passwordLoginCredentialStore :: AccountCredentialStore,
    passwordLoginMfaStore :: MfaStore,
    passwordLoginThrottle :: LoginThrottleContext,
    passwordLoginWorkGate :: PasswordWorkGate,
    passwordLoginRehasher :: PasswordRehasher
  }

data PasswordLoginResult
  = PasswordLoginRejected
  | PasswordLoginThrottled UnixTimeNanoseconds
  | PasswordLoginEmailVerificationRequired AccountId
  | PasswordLoginMfaEnrollmentRequired AccountId
  | PasswordLoginMfaRequired AccountId
  | PasswordLoginCredentialStoreError AccountCredentialStoreError
  | PasswordLoginMfaStoreError MfaStoreError
  | PasswordLoginAttemptStoreError LoginAttemptStoreError
  | PasswordLoginPasswordWorkBudgetExhausted
  deriving (Eq)

data MfaLoginProof
  = TotpLoginProof TotpCode
  | RecoveryCodeLoginProof RecoveryCode
  deriving (Eq)

data PasswordMfaLoginResult
  = PasswordMfaLoginRejected
  | PasswordMfaLoginThrottled UnixTimeNanoseconds
  | PasswordMfaLoginEmailVerificationRequired AccountId
  | PasswordMfaLoginEnrollmentRequired AccountId
  | PasswordMfaLoginAccepted AccountId
  | PasswordMfaLoginCredentialStoreError AccountCredentialStoreError
  | PasswordMfaLoginMfaStoreError MfaStoreError
  | PasswordMfaLoginAttemptStoreError LoginAttemptStoreError
  | PasswordMfaLoginCorruptEnrollment
  | PasswordMfaLoginPasswordWorkBudgetExhausted
  deriving (Eq)

-- | Second-factor inputs extend the immutable password-stage environment
-- with proof-specific material. Nesting it prevents the MFA step from
-- silently using a differently configured store, throttle, or work budget.
data SecondFactorContext = SecondFactorContext
  { secondFactorPasswordLoginEnvironment :: PasswordLoginEnvironment,
    secondFactorEncryptionKey :: SecretEncryptionKey,
    secondFactorNowNanoseconds :: UnixTimeNanoseconds,
    secondFactorNowSeconds :: UnixTimeSeconds,
    secondFactorProof :: MfaLoginProof
  }
