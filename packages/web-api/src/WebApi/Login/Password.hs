{-# LANGUAGE OverloadedStrings #-}

module WebApi.Login.Password
  ( beginPasswordLogin,
    beginPasswordLoginWithIdentifier,
    requiredPasswordHashOrDie,
  )
where

import Control.Exception (evaluate)
import Control.Monad (join, void, when)
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Account (AccountId)
import HarchWeb.Email (EmailAddress)
import HarchWeb.Password (Password, PasswordHash (..), PasswordWorkGate, passwordHashMemoryKibibytes, passwordHashNeedsRehash, passwordHashWorkKibibytes, verifyPassword, withPasswordWork)
import WebApi.Login.Attempt (runAdmittedLoginAttempt)
import WebApi.Login.Types
  ( AccountCredential (..),
    AccountCredentialStore (..),
    AccountCredentialStoreError,
    LoginAttemptBudget (..),
    LoginAttemptBudgets,
    LoginAttemptScope (..),
    LoginIdentifier (..),
    LoginPrincipal (..),
    LoginStage (..),
    LoginThrottleContext (..),
    PasswordLoginEnvironment (..),
    PasswordLoginResult (..),
    PasswordRehasher (..),
    mkLoginAttemptBudgets,
  )
import WebApi.Mfa (MfaStore (..), MfaStoreError, StoredTotpEnrollment (..))

-- | A fixed hash makes unknown-identifier rejection take approximately the
-- same KDF work as a known identifier with a wrong password.
dummyPasswordHash :: PasswordHash
dummyPasswordHash = PasswordHash "$argon2id$v=19$m=65536,t=3,p=1$MDAwMDAwMDAwMDAwMDAwMA$nTQzDQsyrnF98d3p5wV9nHhxGtnnTCDElTqAkW2qVkk"

-- | A deliberately named boundary for the native Argon2 @Maybe@ result.
-- The caller is explicitly tested with an invalid value rather than assuming
-- native allocation failure is impossible.
requiredPasswordHashOrDie :: Text -> Maybe PasswordHash -> PasswordHash
requiredPasswordHashOrDie context = fromMaybe (error ("WebApi.Login: " <> Text.unpack context))

-- | Validates a password and then identifies its MFA state; it never creates
-- a session. One environment owns every invariant capability for this stage.
beginPasswordLogin :: PasswordLoginEnvironment -> EmailAddress -> Password -> IO PasswordLoginResult
beginPasswordLogin environment emailAddress =
  beginPasswordLoginWithIdentifier environment (LoginEmailAddress emailAddress)

-- | Credential lookup precedes admission only so email and username aliases
-- can share a resolved-account throttle key. Both known and unknown paths do
-- exactly one admitted password check.
beginPasswordLoginWithIdentifier :: PasswordLoginEnvironment -> LoginIdentifier -> Password -> IO PasswordLoginResult
beginPasswordLoginWithIdentifier environment identifier password = do
  credentialResult <- lookupCredential credentialStore identifier
  case credentialResult of
    Left storeError -> pure (PasswordLoginCredentialStoreError storeError)
    Right maybeCredential ->
      runAdmittedLoginAttempt
        throttle
        (passwordAttemptBudgets throttle identifier maybeCredential)
        PasswordLoginThrottled
        PasswordLoginAttemptStoreError
        (credentialCheckToPasswordLoginAdmission <$> continueWithCredential environment password maybeCredential)
  where
    credentialStore = passwordLoginCredentialStore environment
    throttle = passwordLoginThrottle environment

passwordAttemptBudgets :: LoginThrottleContext -> LoginIdentifier -> Maybe AccountCredential -> LoginAttemptBudgets
passwordAttemptBudgets throttle identifier maybeCredential =
  mkLoginAttemptBudgets
    ( LoginAttemptBudget principalScope policy
        :| [LoginAttemptBudget (LoginPeerScope clientAddress) policy]
    )
  where
    policy = loginThrottlePolicy throttle
    clientAddress = loginThrottleClientAddress throttle
    principalScope =
      LoginPrincipalScope
        ( maybe
            (UnknownIdentifierPrincipal identifier PasswordLoginStage)
            (\credential -> KnownAccountPrincipal (accountCredentialId credential) PasswordLoginStage)
            maybeCredential
        )

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

continueWithCredential :: PasswordLoginEnvironment -> Password -> Maybe AccountCredential -> IO CredentialCheckOutcome
continueWithCredential environment password maybeCredential =
  case maybeCredential of
    Nothing -> credentialCheckFromPasswordWork passwordWorkGate password dummyPasswordHash (pure CredentialCheckRejected)
    Just credential -> continueWithKnownCredential environment password credential
  where
    passwordWorkGate = passwordLoginWorkGate environment

continueWithKnownCredential :: PasswordLoginEnvironment -> Password -> AccountCredential -> IO CredentialCheckOutcome
continueWithKnownCredential environment password credential =
  credentialCheckFromPasswordWork passwordWorkGate password (accountCredentialPasswordHash credential) acceptedCredential
  where
    passwordWorkGate = passwordLoginWorkGate environment
    accountId = accountCredentialId credential
    acceptedCredential = do
      opportunisticallyRehashPassword environment password credential
      case accountCredentialEmailVerified credential of
        False -> pure (CredentialCheckEmailVerificationRequired accountId)
        True -> classifyMfaEnrollment accountId <$> loadTotpEnrollment (passwordLoginMfaStore environment) accountId

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

-- | A verified weaker hash is replaced only best-effort. An upgrade failure
-- must never make a valid login fail.
opportunisticallyRehashPassword :: PasswordLoginEnvironment -> Password -> AccountCredential -> IO ()
opportunisticallyRehashPassword environment password credential =
  when (passwordHashNeedsRehash rehashPolicy previousHash) $ do
    maybeReplacement <-
      withPasswordWork
        (passwordLoginWorkGate environment)
        (passwordHashMemoryKibibytes rehashPolicy)
        (rehashVerifiedPassword passwordRehasher rehashPolicy password)
    for_ (join maybeReplacement) $ \replacementHash ->
      void (replacePasswordHashIfCurrent credentialStore accountId previousHash replacementHash)
  where
    accountId = accountCredentialId credential
    previousHash = accountCredentialPasswordHash credential
    credentialStore = passwordLoginCredentialStore environment
    passwordRehasher = passwordLoginRehasher environment
    rehashPolicy = passwordRehashingPolicy passwordRehasher

classifyMfaEnrollment :: AccountId -> Either MfaStoreError (Maybe StoredTotpEnrollment) -> CredentialCheckOutcome
classifyMfaEnrollment accountId enrollmentResult =
  case enrollmentResult of
    Left storeError -> CredentialCheckMfaStoreError storeError
    Right Nothing -> CredentialCheckMfaEnrollmentRequired accountId
    Right (Just StoredTotpEnrollment {storedTotpConfirmedAtNanoseconds = Nothing}) -> CredentialCheckMfaEnrollmentRequired accountId
    Right (Just StoredTotpEnrollment {storedTotpConfirmedAtNanoseconds = Just _}) -> CredentialCheckMfaRequired accountId
