{-# LANGUAGE OverloadedStrings #-}

module WebApi.Login
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
    PasswordMfaLoginResult (..),
    PasswordLoginResult (..),
    SecondFactorContext (..),
    beginPasswordLogin,
    beginPasswordLoginWithIdentifier,
    completePasswordLogin,
    completePasswordLoginWithIdentifier,
    requiredPasswordHashOrDie,
  )
where

import Control.Exception (evaluate, onException)
import Control.Monad (void)
import Control.Monad.Except (ExceptT, runExceptT)
import Core.Control.Error (fromMaybeError, liftEitherWith)
import Crypto.Error (maybeCryptoError)
import Data.ByteString qualified as ByteString
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Account (AccountId, accountIdText)
import HarchWeb.Email (EmailAddress, emailAddressText)
import HarchWeb.LoginProtection (LoginProtectionPolicy)
import HarchWeb.Password (Password, PasswordHash, defaultPasswordHashingPolicy, hashPasswordWithSalt, mkPassword, verifyPassword)
import HarchWeb.RecoveryCode (RecoveryCode, readRecoveryCodeHash, recoveryCodeHashText, verifyRecoveryCode)
import HarchWeb.Secret (SecretEncryptionKey, decryptSecretText)
import HarchWeb.Time (UnixTimeNanoseconds, UnixTimeSeconds)
import HarchWeb.Totp (TotpCode, TotpSecret, mkTotpSecret, validateTotpCodeCounter)
import HarchWeb.Username (Username, usernameText)
import WebApi.Mfa (MfaStore (..), MfaStoreError, StoredTotpEnrollment (..))

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
    findAccountCredentialByUsername :: Username -> IO (Either AccountCredentialStoreError (Maybe AccountCredential))
  }

data LoginIdentifier
  = LoginEmailAddress EmailAddress
  | LoginUsername Username

-- | Decision (AZ, 2026-08-19): a new capability record, not an extension of
-- 'AccountCredentialStore' or 'MfaStore' — tracking throttle history is a
-- genuinely disjoint concern from credential lookup or MFA enrollment state
-- (per @docs/design-guidance.md@'s extend-vs-new-abstraction rule), and both
-- existing stores are keyed by a resolved 'AccountId' or a known identity,
-- while login throttling must also cover identifiers with no matching
-- account at all (the same brute-force and enumeration surface this task
-- closes). Attempts are keyed by an opaque 'Text' (an identifier's own text
-- for the password step, an account id's text for the second-factor step)
-- rather than 'LoginIdentifier' itself, so callers never need to construct a
-- fake 'LoginIdentifier' just to key a second-factor attempt.
data LoginAttemptStoreError
  = LoginAttemptStoreUnavailable Text
  | LoginAttemptStoreCorruptData Text
  deriving (Eq)

-- | An opaque, store-issued handle for one admitted authentication attempt.
-- It is never a client credential; it only lets the server settle or cancel
-- the provisional failure that made admission atomic.
newtype LoginAttemptReservation = LoginAttemptReservation Text
  deriving (Eq)

data LoginAttemptAdmission
  = LoginAttemptReserved LoginAttemptReservation
  | LoginAttemptThrottled UnixTimeNanoseconds
  deriving (Eq)

-- | Decision (PR-S4, 2026-08-23): extend the existing attempt-store boundary
-- rather than adding a parallel throttle. A PostgreSQL implementation admits
-- one key through a database reservation function: it takes the per-key lock,
-- then evaluates and inserts the unsettled row in a post-lock query snapshot.
-- That row already counts as a failure. The workflow then settles the
-- reservation to its real result, or cancels it on typed infrastructure
-- failure, settlement failure, and asynchronous exception. A process crash
-- can still leave an unsettled row until its window expires; PR-S5 owns
-- durable retention and stale-reservation cleanup rather than introducing a
-- second lifecycle here.
data LoginAttemptStore = LoginAttemptStore
  { reserveLoginAttempt :: Text -> LoginProtectionPolicy -> UnixTimeNanoseconds -> IO (Either LoginAttemptStoreError LoginAttemptAdmission),
    settleLoginAttempt :: LoginAttemptReservation -> Bool -> IO (Either LoginAttemptStoreError ()),
    cancelLoginAttempt :: LoginAttemptReservation -> IO (Either LoginAttemptStoreError ())
  }

-- | The throttle dependencies threaded through both the password step
-- ('beginPasswordLoginWithIdentifier') and the second-factor step
-- (via 'SecondFactorContext'), so both stages share one clock reading and
-- one policy for a single login attempt.
data LoginThrottleContext = LoginThrottleContext
  { loginThrottleStore :: LoginAttemptStore,
    loginThrottlePolicy :: LoginProtectionPolicy,
    loginThrottleNow :: UnixTimeNanoseconds
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
  deriving (Eq)

data LoginInfrastructureError
  = LoginMfaStoreError MfaStoreError
  | LoginCorruptEnrollment

-- | Decision (CF, 2026-08-19): extend this existing second-factor context to
-- the entry points ('completePasswordLogin', 'completePasswordLoginWithIdentifier')
-- rather than leaving them threading its five fields positionally alongside
-- the credential store, identifier, and password. 'secondFactorMfaStore'
-- already carries the one dependency 'beginPasswordLoginWithIdentifier' needs,
-- so the entry points read it from the context instead of taking a separate,
-- easily-transposed 'MfaStore' argument. This took both entry points from 8
-- positional parameters to 4.
-- | Decision (CF, 2026-08-19): extend this existing second-factor context to
-- the entry points ('completePasswordLogin', 'completePasswordLoginWithIdentifier')
-- rather than leaving them threading its five fields positionally alongside
-- the credential store, identifier, and password. 'secondFactorMfaStore'
-- already carries the one dependency 'beginPasswordLoginWithIdentifier' needs,
-- so the entry points read it from the context instead of taking a separate,
-- easily-transposed 'MfaStore' argument. This took both entry points from 8
-- positional parameters to 4.
data SecondFactorContext = SecondFactorContext
  { secondFactorMfaStore :: MfaStore,
    secondFactorEncryptionKey :: SecretEncryptionKey,
    secondFactorNowNanoseconds :: UnixTimeNanoseconds,
    secondFactorNowSeconds :: UnixTimeSeconds,
    secondFactorProof :: MfaLoginProof,
    -- | Added for AZ, 2026-08-19: the second factor's own KDF-amplification
    -- surface ('completeRecoveryCode' hashes against up to eight stored
    -- recovery codes) needs the same throttle machinery as the password
    -- step, keyed by account id once one is known.
    secondFactorThrottle :: LoginThrottleContext
  }

-- | A fixed, always-computed Argon2id hash consumed to keep an unknown
-- identifier's rejection taking approximately as long as a known identifier
-- with a wrong password. Without this, 'beginPasswordLoginWithIdentifier'
-- returning immediately for an unknown identifier while a known one runs a
-- full KDF verification is a reliable existence oracle.
-- | Per @docs/design-guidance.md@'s never-mask-a-gate-finding rule: the @$!@
-- below on the diagnostic-message literal is a last resort, confirmed
-- directly rather than assumed. The literal is unique to this call site (not
-- duplicated anywhere else in this module), so there is no shared expression
-- to deduplicate; it remains permanently unforced without the @$!@ because
-- 'dummyPasswordHash' is a 'NOINLINE' CAF evaluated at most once across the
-- whole test run, and nothing else in this module's test path forces it a
-- second time to pick up a tick.
{-# ANN dummyPasswordHash ("HLint: ignore Redundant $!" :: String) #-}
dummyPasswordHash :: PasswordHash
dummyPasswordHash =
  (requiredPasswordHashOrDie $! "expected a valid dummy password hash for the login existence-oracle defense")
    (hashPasswordWithSalt defaultPasswordHashingPolicy dummySalt (mkPassword "login-existence-oracle-defense"))
  where
    dummySalt = ByteString.replicate 16 0
{-# NOINLINE dummyPasswordHash #-}

-- | Decision (AZ, 2026-08-19, per @docs/design-guidance.md@'s
-- missing-framework-capability protocol): 'HarchWeb.Password.hashPasswordWithSalt'
-- is necessarily 'Maybe'-returning (a native Argon2 resource failure is a real,
-- if practically unreachable, outcome for any policy/salt/password), so
-- 'dummyPasswordHash''s fixed, already-validated inputs leave an @error@
-- fallback no production test can force without corrupting the fixed
-- constant the timing defense depends on. Adding a total framework-level
-- variant would mean asserting a native library call can never fail, which
-- is not a claim this codebase can honestly make — not taken. Instead this
-- follows the same shape as 'WebApi.AccountPages.Actions.Contract''s
-- @buildActionCodecOrDie@: extracted into its own named, exported helper so
-- a dedicated test can force the failure path directly with a deliberately
-- invalid 'Maybe' value (see 'Unit.WebApi.LoginSpec'), leaving
-- 'dummyPasswordHash''s own real call site untouched.
requiredPasswordHashOrDie :: Text -> Maybe PasswordHash -> PasswordHash
requiredPasswordHashOrDie context = fromMaybe (error ("WebApi.Login: " <> Text.unpack context))

-- | Decision (PR-S3, 2026-08-23): extend the existing attempt-store key
-- derivation and settlement path rather than adding a second-factor-only
-- throttle. Account lookup compares usernames with PostgreSQL @lower@,
-- while 'Username' admits ASCII only, so 'Text.toLower' gives the same
-- canonical identity before untrusted spelling reaches throttle storage.
-- TOTP and recovery attempts share 'secondFactorAttemptKey' because they
-- are alternate proofs of one account's MFA boundary; a failed required
-- settlement is surfaced through the existing typed store-error result
-- instead of allowing authentication to fail open.
loginIdentifierKey :: LoginIdentifier -> Text
loginIdentifierKey identifier =
  case identifier of
    LoginEmailAddress emailAddress -> "email:" <> emailAddressText emailAddress
    LoginUsername username -> "username:" <> Text.toLower (usernameText username)

secondFactorAttemptKey :: AccountId -> Text
secondFactorAttemptKey accountId = "mfa:" <> accountIdText accountId

runAdmittedLoginAttempt ::
  LoginThrottleContext ->
  Text ->
  (UnixTimeNanoseconds -> result) ->
  (LoginAttemptStoreError -> result) ->
  IO (result, Maybe Bool) ->
  IO result
runAdmittedLoginAttempt throttle key throttled storeFailure work = do
  admissionResult <- reserveLoginAttempt store key policy now
  case admissionResult of
    Left storeError -> pure (storeFailure storeError)
    Right (LoginAttemptThrottled lockoutEndsAt) -> pure (throttled lockoutEndsAt)
    Right (LoginAttemptReserved reservation) -> do
      (result, settlement) <- work `onException` void (cancelLoginAttempt store reservation)
      case settlement of
        Nothing -> cancelOrFail reservation result
        Just succeeded -> settleOrFail reservation succeeded result
  where
    store = loginThrottleStore throttle
    policy = loginThrottlePolicy throttle
    now = loginThrottleNow throttle
    cancelOrFail reservation result = do
      cancelResult <- cancelLoginAttempt store reservation
      pure (either storeFailure (const result) cancelResult)
    settleOrFail reservation succeeded result = do
      settleResult <- settleLoginAttempt store reservation succeeded
      case settleResult of
        Right () -> pure result
        Left storeError -> do
          -- A settlement outcome can be ambiguous to the caller. Cancellation
          -- only removes still-unsettled rows, so it recovers the ordinary
          -- failed-write case without undoing a settlement that reached the DB.
          void (cancelLoginAttempt store reservation)
          pure (storeFailure storeError)

-- | Validates the password first, then requires a confirmed authenticator.
-- This function intentionally never creates a session: completing the second
-- factor is required before the application may authenticate the account.
beginPasswordLogin :: AccountCredentialStore -> MfaStore -> LoginThrottleContext -> EmailAddress -> Password -> IO PasswordLoginResult
beginPasswordLogin credentialStore mfaStore throttle emailAddress =
  beginPasswordLoginWithIdentifier credentialStore mfaStore throttle (LoginEmailAddress emailAddress)

beginPasswordLoginWithIdentifier :: AccountCredentialStore -> MfaStore -> LoginThrottleContext -> LoginIdentifier -> Password -> IO PasswordLoginResult
beginPasswordLoginWithIdentifier credentialStore mfaStore throttle identifier password =
  runAdmittedLoginAttempt throttle key PasswordLoginThrottled PasswordLoginAttemptStoreError work
  where
    work = do
      credentialResult <- lookupCredential credentialStore identifier
      outcome <- either (pure . CredentialCheckCredentialStoreError) (continueWithCredential mfaStore password) credentialResult
      pure (credentialCheckToPasswordLoginAdmission outcome)
    key = loginIdentifierKey identifier

-- | A credential check's own outcome, distinct from 'PasswordLoginResult':
-- neither the throttle-gate's own rejection ('PasswordLoginThrottled') nor a
-- failure to even reach the credential check ('PasswordLoginAttemptStoreError')
-- can occur here, since 'beginPasswordLoginWithIdentifier' only reaches
-- 'continueWithCredential' after the throttle check itself has already
-- succeeded and permitted the attempt. Keeping those two states out of this
-- type (rather than reusing 'PasswordLoginResult' and pattern-matching
-- defensively) makes its admission result total over every
-- constructor a genuine credential check can produce, instead of carrying
-- two alternatives no test can ever legitimately reach.
data CredentialCheckOutcome
  = CredentialCheckRejected
  | CredentialCheckEmailVerificationRequired AccountId
  | CredentialCheckMfaEnrollmentRequired AccountId
  | CredentialCheckMfaRequired AccountId
  | CredentialCheckCredentialStoreError AccountCredentialStoreError
  | CredentialCheckMfaStoreError MfaStoreError

credentialCheckToPasswordLoginAdmission :: CredentialCheckOutcome -> (PasswordLoginResult, Maybe Bool)
credentialCheckToPasswordLoginAdmission outcome =
  case outcome of
    CredentialCheckRejected -> (PasswordLoginRejected, Just False)
    CredentialCheckEmailVerificationRequired accountId -> (PasswordLoginEmailVerificationRequired accountId, Just True)
    CredentialCheckMfaEnrollmentRequired accountId -> (PasswordLoginMfaEnrollmentRequired accountId, Just True)
    CredentialCheckMfaRequired accountId -> (PasswordLoginMfaRequired accountId, Just True)
    CredentialCheckCredentialStoreError storeError -> (PasswordLoginCredentialStoreError storeError, Nothing)
    CredentialCheckMfaStoreError storeError -> (PasswordLoginMfaStoreError storeError, Just True)

lookupCredential :: AccountCredentialStore -> LoginIdentifier -> IO (Either AccountCredentialStoreError (Maybe AccountCredential))
lookupCredential credentialStore identifier =
  case identifier of
    LoginEmailAddress emailAddress -> findAccountCredentialByEmail credentialStore emailAddress
    LoginUsername username -> findAccountCredentialByUsername credentialStore username

continueWithCredential :: MfaStore -> Password -> Maybe AccountCredential -> IO CredentialCheckOutcome
continueWithCredential mfaStore password maybeCredential =
  case maybeCredential of
    Nothing -> do
      _ <- evaluate (verifyPassword password dummyPasswordHash)
      pure CredentialCheckRejected
    Just credential -> continueWithKnownCredential mfaStore password credential

continueWithKnownCredential :: MfaStore -> Password -> AccountCredential -> IO CredentialCheckOutcome
continueWithKnownCredential mfaStore password credential =
  case verifyPassword password (accountCredentialPasswordHash credential) of
    False -> pure CredentialCheckRejected
    True ->
      case accountCredentialEmailVerified credential of
        False -> pure (CredentialCheckEmailVerificationRequired accountId)
        True -> classifyMfaEnrollment accountId <$> loadTotpEnrollment mfaStore accountId
  where
    accountId = accountCredentialId credential

classifyMfaEnrollment :: AccountId -> Either MfaStoreError (Maybe StoredTotpEnrollment) -> CredentialCheckOutcome
classifyMfaEnrollment accountId enrollmentResult =
  case enrollmentResult of
    Left storeError -> CredentialCheckMfaStoreError storeError
    Right Nothing -> CredentialCheckMfaEnrollmentRequired accountId
    Right (Just StoredTotpEnrollment {storedTotpConfirmedAtNanoseconds = Nothing}) -> CredentialCheckMfaEnrollmentRequired accountId
    Right (Just StoredTotpEnrollment {storedTotpConfirmedAtNanoseconds = Just _}) -> CredentialCheckMfaRequired accountId

-- | Performs password validation before examining the supplied second factor.
-- A recovery code is marked used by its stored hash only after its Argon2id
-- verification succeeds, so its cleartext representation never reaches storage.
completePasswordLogin :: AccountCredentialStore -> SecondFactorContext -> EmailAddress -> Password -> IO PasswordMfaLoginResult
completePasswordLogin credentialStore context emailAddress =
  completePasswordLoginWithIdentifier credentialStore context (LoginEmailAddress emailAddress)

completePasswordLoginWithIdentifier :: AccountCredentialStore -> SecondFactorContext -> LoginIdentifier -> Password -> IO PasswordMfaLoginResult
completePasswordLoginWithIdentifier credentialStore context identifier password = do
  passwordResult <- beginPasswordLoginWithIdentifier credentialStore (secondFactorMfaStore context) (secondFactorThrottle context) identifier password
  continuePasswordLogin context passwordResult

continuePasswordLogin :: SecondFactorContext -> PasswordLoginResult -> IO PasswordMfaLoginResult
continuePasswordLogin context passwordResult =
  case passwordResult of
    PasswordLoginRejected -> pure PasswordMfaLoginRejected
    PasswordLoginThrottled lockoutEndsAt -> pure (PasswordMfaLoginThrottled lockoutEndsAt)
    PasswordLoginEmailVerificationRequired accountId -> pure (PasswordMfaLoginEmailVerificationRequired accountId)
    PasswordLoginMfaEnrollmentRequired accountId -> pure (PasswordMfaLoginEnrollmentRequired accountId)
    PasswordLoginCredentialStoreError storeError -> pure (PasswordMfaLoginCredentialStoreError storeError)
    PasswordLoginMfaStoreError storeError -> pure (PasswordMfaLoginMfaStoreError storeError)
    PasswordLoginAttemptStoreError storeError -> pure (PasswordMfaLoginAttemptStoreError storeError)
    PasswordLoginMfaRequired accountId -> completeConfirmedEnrollment context accountId

completeConfirmedEnrollment :: SecondFactorContext -> AccountId -> IO PasswordMfaLoginResult
completeConfirmedEnrollment context accountId =
  either
    (pure . PasswordMfaLoginMfaStoreError)
    (maybe (pure (PasswordMfaLoginEnrollmentRequired accountId)) (completeStoredEnrollment context accountId))
    =<< loadTotpEnrollment (secondFactorMfaStore context) accountId

completeStoredEnrollment :: SecondFactorContext -> AccountId -> StoredTotpEnrollment -> IO PasswordMfaLoginResult
completeStoredEnrollment context accountId enrollment =
  case storedTotpConfirmedAtNanoseconds enrollment of
    Nothing -> pure (PasswordMfaLoginEnrollmentRequired accountId)
    Just _ -> verifyProof context accountId enrollment

verifyProof :: SecondFactorContext -> AccountId -> StoredTotpEnrollment -> IO PasswordMfaLoginResult
verifyProof context accountId enrollment =
  case secondFactorProof context of
    TotpLoginProof suppliedCode ->
      verifyTotpProof context accountId enrollment suppliedCode
    RecoveryCodeLoginProof suppliedCode ->
      completeRecoveryCode context accountId suppliedCode

-- | Rejects a counter at or below 'storedTotpLastUsedCounter' before ever
-- consulting the store again, closing the replay window a bare
-- 'HarchWeb.Totp.validateTotpCode' boolean leaves open for the rest of its
-- skew window. 'markTotpCodeUsed' is itself an atomic conditional update
-- (mirroring 'consumeRecoveryCodeHash'), so a concurrent request racing to
-- accept the same or an older counter for this account also loses.
verifyTotpProof :: SecondFactorContext -> AccountId -> StoredTotpEnrollment -> TotpCode -> IO PasswordMfaLoginResult
verifyTotpProof context accountId enrollment suppliedCode =
  runAdmittedLoginAttempt
    (secondFactorThrottle context)
    (secondFactorAttemptKey accountId)
    PasswordMfaLoginThrottled
    PasswordMfaLoginAttemptStoreError
    (verifyPermittedTotpProof context accountId enrollment suppliedCode)

verifyPermittedTotpProof :: SecondFactorContext -> AccountId -> StoredTotpEnrollment -> TotpCode -> IO (PasswordMfaLoginResult, Maybe Bool)
verifyPermittedTotpProof context accountId enrollment suppliedCode =
  case decodeTotpSecret (secondFactorEncryptionKey context) (storedTotpEncryptedSecret enrollment) of
    Nothing -> pure (PasswordMfaLoginCorruptEnrollment, Nothing)
    Just secret ->
      case validateTotpCodeCounter (secondFactorNowSeconds context) 1 secret suppliedCode of
        Nothing -> pure (PasswordMfaLoginRejected, Just False)
        Just matchedCounter ->
          case storedTotpLastUsedCounter enrollment of
            Just lastUsedCounter
              | matchedCounter <= lastUsedCounter -> pure (PasswordMfaLoginRejected, Just False)
            _ -> do
              markResult <- markTotpCodeUsed (secondFactorMfaStore context) accountId matchedCounter
              case markResult of
                Left storeError -> pure (PasswordMfaLoginMfaStoreError storeError, Nothing)
                Right True -> pure (PasswordMfaLoginAccepted accountId, Just True)
                Right False -> pure (PasswordMfaLoginRejected, Just False)

-- | A recovery proof shares the per-account MFA budget with TOTP. Its
-- accepted or rejected result is settled before it becomes caller-visible;
-- a required attempt-store write failure is therefore fail-closed.
completeRecoveryCode :: SecondFactorContext -> AccountId -> RecoveryCode -> IO PasswordMfaLoginResult
completeRecoveryCode context accountId suppliedCode =
  runAdmittedLoginAttempt throttle key PasswordMfaLoginThrottled PasswordMfaLoginAttemptStoreError work
  where
    work = do
      recoveryResult <- runExceptT $ do
        recoveryHashValues <- liftMfaStore (loadUnusedRecoveryCodeHashes (secondFactorMfaStore context) accountId)
        recoveryHashes <-
          fromMaybeError LoginCorruptEnrollment (traverse readRecoveryCodeHash recoveryHashValues)
        maybe (pure False) consumeMatchingHash (find (verifyRecoveryCode suppliedCode) recoveryHashes)
      case recoveryResult of
        Left infrastructureError -> pure (infrastructureFailureResult infrastructureError, Nothing)
        Right accepted ->
          case accepted of
            False -> pure (PasswordMfaLoginRejected, Just False)
            True -> pure (PasswordMfaLoginAccepted accountId, Just True)
    throttle = secondFactorThrottle context
    key = secondFactorAttemptKey accountId
    consumeMatchingHash matchingHash =
      liftMfaStore
        (consumeRecoveryCodeHash (secondFactorMfaStore context) accountId (recoveryCodeHashText matchingHash) (secondFactorNowNanoseconds context))

liftMfaStore :: IO (Either MfaStoreError value) -> ExceptT LoginInfrastructureError IO value
liftMfaStore = liftEitherWith LoginMfaStoreError

infrastructureFailureResult :: LoginInfrastructureError -> PasswordMfaLoginResult
infrastructureFailureResult infrastructureError =
  case infrastructureError of
    LoginMfaStoreError storeError -> PasswordMfaLoginMfaStoreError storeError
    LoginCorruptEnrollment -> PasswordMfaLoginCorruptEnrollment

decodeTotpSecret :: SecretEncryptionKey -> Text -> Maybe TotpSecret
decodeTotpSecret encryptionKey encryptedSecret =
  maybeCryptoError (decryptSecretText encryptionKey encryptedSecret) >>= either (const Nothing) mkTotpSecret
