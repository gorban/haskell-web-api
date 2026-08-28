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
    PasswordRehasher (..),
    PasswordMfaLoginResult (..),
    PasswordLoginResult (..),
    SecondFactorContext (..),
    beginPasswordLogin,
    beginPasswordLoginWithIdentifier,
    beginPasswordLoginWithIdentifierAndRehasher,
    completePasswordLogin,
    completePasswordLoginWithIdentifier,
    requiredPasswordHashOrDie,
  )
where

import Control.Exception (evaluate, onException)
import Control.Monad (join, void, when)
import Control.Monad.Except (ExceptT, runExceptT)
import Core.Control.Error (fromMaybeError, liftEitherWith)
import Crypto.Error (maybeCryptoError)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Account (AccountId, accountIdText)
import HarchWeb.Email (EmailAddress, emailAddressText)
import HarchWeb.LoginProtection (LoginProtectionPolicy)
import HarchWeb.Password (Password, PasswordHash (..), PasswordHashingPolicy, PasswordWorkGate, defaultPasswordHashingPolicy, hashPassword, passwordHashMemoryKibibytes, passwordHashNeedsRehash, passwordHashWorkKibibytes, verifyPassword, withPasswordWork)
import HarchWeb.RecoveryCode (RecoveryCode, RecoveryCodeHash, readRecoveryCodeHash, recoveryCodeHashText, recoveryCodeHashWorkKibibytes, verifyRecoveryCode)
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
-- has already verified.  Keeping it injectable makes the optional migration's
-- native-failure behavior explicit and testable without making persistence
-- responsible for Argon2 work.
data PasswordRehasher = PasswordRehasher
  { passwordRehashingPolicy :: PasswordHashingPolicy,
    rehashVerifiedPassword :: PasswordHashingPolicy -> Password -> IO (Maybe PasswordHash)
  }

defaultPasswordRehasher :: PasswordRehasher
defaultPasswordRehasher = PasswordRehasher defaultPasswordHashingPolicy hashPassword

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
    secondFactorThrottle :: LoginThrottleContext,
    -- | EJ's application-owned password-work gate also covers the password
    -- step and each recovery-code verification.
    secondFactorPasswordWorkGate :: PasswordWorkGate
  }

-- | A fixed, always-computed Argon2id hash consumed to keep an unknown
-- identifier's rejection taking approximately as long as a known identifier
-- with a wrong password. Without this, 'beginPasswordLoginWithIdentifier'
-- returning immediately for an unknown identifier while a known one runs a
-- full KDF verification is a reliable existence oracle.
-- | The precomputed, valid default-policy encoding ensures an unknown-account
-- request cannot become the first caller to allocate an otherwise
-- unbudgeted native Argon2 hash.
dummyPasswordHash :: PasswordHash
dummyPasswordHash = PasswordHash "$argon2id$v=19$m=65536,t=3,p=1$MDAwMDAwMDAwMDAwMDAwMA$nTQzDQsyrnF98d3p5wV9nHhxGtnnTCDElTqAkW2qVkk"

-- | Decision (AZ, 2026-08-19, per @docs/design-guidance.md@'s
-- missing-framework-capability protocol): 'HarchWeb.Password.hashPasswordWithSalt'
-- is necessarily 'Maybe'-returning (a native Argon2 resource failure is a real,
-- if practically unreachable, outcome for any policy/salt/password), so
-- callers must still handle the native failure. Rather than assert it cannot
-- occur with a total framework-level variant, this follows the same shape as
-- 'WebApi.AccountPages.Actions.Contract''s
-- @buildActionCodecOrDie@: extracted into its own named, exported helper so
-- a dedicated test can force the failure path directly with a deliberately
-- invalid 'Maybe' value (see 'Unit.WebApi.LoginSpec').
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

-- | Known credentials use their resolved account identity, not the spelling
-- that found them.  This makes email and username login attempts share one
-- durable password-failure budget; the submitted-identifier key remains for
-- an absent credential, where there is no principal to resolve.
accountPasswordAttemptKey :: AccountId -> Text
accountPasswordAttemptKey accountId = "account:" <> accountIdText accountId

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
beginPasswordLogin :: AccountCredentialStore -> MfaStore -> LoginThrottleContext -> PasswordWorkGate -> EmailAddress -> Password -> IO PasswordLoginResult
beginPasswordLogin credentialStore mfaStore throttle passwordWorkGate emailAddress =
  beginPasswordLoginWithIdentifier credentialStore mfaStore throttle passwordWorkGate (LoginEmailAddress emailAddress)

beginPasswordLoginWithIdentifier :: AccountCredentialStore -> MfaStore -> LoginThrottleContext -> PasswordWorkGate -> LoginIdentifier -> Password -> IO PasswordLoginResult
beginPasswordLoginWithIdentifier = beginPasswordLoginWithIdentifierAndRehasher defaultPasswordRehasher

-- | Decision (PR-SEC2, 2026-08-28): extend the existing password admission
-- path with a resolved-principal key, rather than add an alias map or a
-- second throttle. Credential lookup is necessarily before admission because
-- only it can distinguish an account principal from an unknown identifier.
-- Both branches still run exactly one admitted password check: a known
-- credential uses its opaque account id so email and username share one
-- durable budget, while the unknown branch keeps the canonical submitted
-- identifier and verifies the fixed dummy hash to preserve rejection timing.
beginPasswordLoginWithIdentifierAndRehasher :: PasswordRehasher -> AccountCredentialStore -> MfaStore -> LoginThrottleContext -> PasswordWorkGate -> LoginIdentifier -> Password -> IO PasswordLoginResult
beginPasswordLoginWithIdentifierAndRehasher passwordRehasher credentialStore mfaStore throttle passwordWorkGate identifier password = do
  credentialResult <- lookupCredential credentialStore identifier
  case credentialResult of
    Left storeError -> pure (PasswordLoginCredentialStoreError storeError)
    Right maybeCredential ->
      runAdmittedLoginAttempt
        throttle
        (maybe (loginIdentifierKey identifier) (accountPasswordAttemptKey . accountCredentialId) maybeCredential)
        PasswordLoginThrottled
        PasswordLoginAttemptStoreError
        ( credentialCheckToPasswordLoginAdmission
            <$> continueWithCredential passwordRehasher credentialStore mfaStore passwordWorkGate password maybeCredential
        )

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
  | CredentialCheckMfaStoreError MfaStoreError
  | CredentialCheckPasswordWorkBudgetExhausted

credentialCheckToPasswordLoginAdmission :: CredentialCheckOutcome -> (PasswordLoginResult, Maybe Bool)
credentialCheckToPasswordLoginAdmission outcome =
  case outcome of
    CredentialCheckRejected -> (PasswordLoginRejected, Just False)
    CredentialCheckEmailVerificationRequired accountId -> (PasswordLoginEmailVerificationRequired accountId, Just True)
    CredentialCheckMfaEnrollmentRequired accountId -> (PasswordLoginMfaEnrollmentRequired accountId, Just True)
    CredentialCheckMfaRequired accountId -> (PasswordLoginMfaRequired accountId, Just True)
    CredentialCheckMfaStoreError storeError -> (PasswordLoginMfaStoreError storeError, Just True)
    CredentialCheckPasswordWorkBudgetExhausted -> (PasswordLoginPasswordWorkBudgetExhausted, Nothing)

lookupCredential :: AccountCredentialStore -> LoginIdentifier -> IO (Either AccountCredentialStoreError (Maybe AccountCredential))
lookupCredential credentialStore identifier =
  case identifier of
    LoginEmailAddress emailAddress -> findAccountCredentialByEmail credentialStore emailAddress
    LoginUsername username -> findAccountCredentialByUsername credentialStore username

continueWithCredential :: PasswordRehasher -> AccountCredentialStore -> MfaStore -> PasswordWorkGate -> Password -> Maybe AccountCredential -> IO CredentialCheckOutcome
continueWithCredential passwordRehasher credentialStore mfaStore passwordWorkGate password maybeCredential =
  case maybeCredential of
    Nothing -> credentialCheckFromPasswordWork passwordWorkGate password dummyPasswordHash (pure CredentialCheckRejected)
    Just credential -> continueWithKnownCredential passwordRehasher credentialStore mfaStore passwordWorkGate password credential

continueWithKnownCredential :: PasswordRehasher -> AccountCredentialStore -> MfaStore -> PasswordWorkGate -> Password -> AccountCredential -> IO CredentialCheckOutcome
continueWithKnownCredential passwordRehasher credentialStore mfaStore passwordWorkGate password credential =
  credentialCheckFromPasswordWork passwordWorkGate password (accountCredentialPasswordHash credential) acceptedCredential
  where
    accountId = accountCredentialId credential
    acceptedCredential = do
      opportunisticallyRehashPassword passwordRehasher credentialStore passwordWorkGate password credential
      case accountCredentialEmailVerified credential of
        False -> pure (CredentialCheckEmailVerificationRequired accountId)
        True -> classifyMfaEnrollment accountId <$> loadTotpEnrollment mfaStore accountId

credentialCheckFromPasswordWork :: PasswordWorkGate -> Password -> PasswordHash -> IO CredentialCheckOutcome -> IO CredentialCheckOutcome
credentialCheckFromPasswordWork passwordWorkGate password passwordHash accepted =
  case passwordHashWorkKibibytes passwordHash of
    Nothing -> pure CredentialCheckRejected
    Just cost -> do
      maybeVerified <- withPasswordWork passwordWorkGate cost (evaluate (verifyPassword password passwordHash))
      case maybeVerified of
        Nothing -> pure CredentialCheckPasswordWorkBudgetExhausted
        Just False -> pure CredentialCheckRejected
        Just True -> accepted

-- | Decision (DM, 2026-08-25): extend the existing credential-store boundary
-- with a compare-and-replace operation rather than introducing a password
-- migration table or a second authentication path.  After the old hash has
-- verified, a uniformly weaker policy is rehashed under the current bounded
-- policy and conditionally replaced by account id plus the old hash.  This
-- lets concurrent successful requests yield one update and one harmless
-- no-op, and prevents a concurrent password change from being overwritten.
-- Resource admission, native hash failure, and persistence failure are all
-- deliberately best-effort: a legacy hash remains a valid credential, so an
-- upgrade problem must not turn a successful login into an authentication
-- failure.  Mixed stronger/weaker policies are retained to avoid lowering a
-- stored Argon2 cost.
opportunisticallyRehashPassword :: PasswordRehasher -> AccountCredentialStore -> PasswordWorkGate -> Password -> AccountCredential -> IO ()
opportunisticallyRehashPassword passwordRehasher credentialStore passwordWorkGate password credential =
  when (passwordHashNeedsRehash rehashPolicy previousHash) $ do
    maybeReplacement <-
      withPasswordWork
        passwordWorkGate
        (passwordHashMemoryKibibytes rehashPolicy)
        (rehashVerifiedPassword passwordRehasher rehashPolicy password)
    for_ (join maybeReplacement) $ \replacementHash ->
      void (replacePasswordHashIfCurrent credentialStore accountId previousHash replacementHash)
  where
    accountId = accountCredentialId credential
    previousHash = accountCredentialPasswordHash credential
    rehashPolicy = passwordRehashingPolicy passwordRehasher

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
  passwordResult <- beginPasswordLoginWithIdentifier credentialStore (secondFactorMfaStore context) (secondFactorThrottle context) (secondFactorPasswordWorkGate context) identifier password
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
    PasswordLoginPasswordWorkBudgetExhausted -> pure PasswordMfaLoginPasswordWorkBudgetExhausted
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
        fromMaybeError LoginCorruptEnrollment (traverse readRecoveryCodeHash recoveryHashValues)
      case recoveryResult of
        Left infrastructureError -> pure (infrastructureFailureResult infrastructureError, Nothing)
        Right recoveryHashes -> do
          matchingHash <- findMatchingRecoveryHash (secondFactorPasswordWorkGate context) suppliedCode recoveryHashes
          case matchingHash of
            Nothing -> pure (PasswordMfaLoginPasswordWorkBudgetExhausted, Nothing)
            Just Nothing -> pure (PasswordMfaLoginRejected, Just False)
            Just (Just hashValue) -> do
              consumed <- runExceptT (consumeMatchingHash hashValue)
              case consumed of
                Left infrastructureError -> pure (infrastructureFailureResult infrastructureError, Nothing)
                Right True -> pure (PasswordMfaLoginAccepted accountId, Just True)
                Right False -> pure (PasswordMfaLoginRejected, Just False)
    throttle = secondFactorThrottle context
    key = secondFactorAttemptKey accountId
    consumeMatchingHash matchingHash =
      liftMfaStore
        (consumeRecoveryCodeHash (secondFactorMfaStore context) accountId (recoveryCodeHashText matchingHash) (secondFactorNowNanoseconds context))

findMatchingRecoveryHash :: PasswordWorkGate -> RecoveryCode -> [RecoveryCodeHash] -> IO (Maybe (Maybe RecoveryCodeHash))
findMatchingRecoveryHash passwordWorkGate suppliedCode = go
  where
    go hashes =
      case hashes of
        [] -> pure (Just Nothing)
        hashValue : remainingHashes -> do
          maybeMatches <- withPasswordWork passwordWorkGate (recoveryCodeHashWorkKibibytes hashValue) (evaluate (verifyRecoveryCode suppliedCode hashValue))
          case maybeMatches of
            Nothing -> pure Nothing
            Just True -> pure (Just (Just hashValue))
            Just False -> go remainingHashes

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
