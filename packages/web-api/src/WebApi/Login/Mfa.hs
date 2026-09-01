module WebApi.Login.Mfa
  ( completePasswordLogin,
    completePasswordLoginWithIdentifier,
  )
where

import Control.Exception (evaluate)
import Control.Monad.Except (ExceptT, runExceptT)
import Core.Control.Error (fromMaybeError, liftEitherWith)
import Crypto.Error (maybeCryptoError)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import HarchWeb.Account (AccountId)
import HarchWeb.Email (EmailAddress)
import HarchWeb.Password (Password, PasswordWorkGate, withPasswordWork)
import HarchWeb.RecoveryCode (RecoveryCode, RecoveryCodeHash, readRecoveryCodeHash, recoveryCodeHashText, recoveryCodeHashWorkKibibytes, verifyRecoveryCode)
import HarchWeb.Secret (SecretEncryptionKey, decryptSecretText)
import HarchWeb.Totp (TotpCode, TotpSecret, mkTotpSecret, validateTotpCodeCounter)
import WebApi.Login.Attempt (runAdmittedLoginAttempt)
import WebApi.Login.Password (beginPasswordLoginWithIdentifier)
import WebApi.Login.Types
  ( LoginAttemptBudget (..),
    LoginAttemptBudgets,
    LoginAttemptScope (..),
    LoginIdentifier (..),
    LoginPrincipal (..),
    LoginStage (..),
    LoginThrottleContext (..),
    MfaLoginProof (..),
    PasswordLoginEnvironment (..),
    PasswordLoginResult (..),
    PasswordMfaLoginResult (..),
    SecondFactorContext (..),
    mkLoginAttemptBudgets,
  )
import WebApi.Mfa (MfaStore (..), MfaStoreError, StoredTotpEnrollment (..))

data LoginInfrastructureError
  = LoginMfaStoreError MfaStoreError
  | LoginCorruptEnrollment

-- | Performs password validation before looking at the supplied MFA proof.
-- The nested environment ensures both stages use the same store, throttle,
-- work budget, and rehashing policy.
completePasswordLogin :: SecondFactorContext -> EmailAddress -> Password -> IO PasswordMfaLoginResult
completePasswordLogin context emailAddress =
  completePasswordLoginWithIdentifier context (LoginEmailAddress emailAddress)

completePasswordLoginWithIdentifier :: SecondFactorContext -> LoginIdentifier -> Password -> IO PasswordMfaLoginResult
completePasswordLoginWithIdentifier context identifier password = do
  passwordResult <- beginPasswordLoginWithIdentifier (secondFactorPasswordLoginEnvironment context) identifier password
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
completeConfirmedEnrollment context accountId = do
  enrollmentResult <- loadTotpEnrollment (passwordLoginMfaStore (secondFactorPasswordLoginEnvironment context)) accountId
  case enrollmentResult of
    Left storeError -> pure (PasswordMfaLoginMfaStoreError storeError)
    Right Nothing -> pure (PasswordMfaLoginEnrollmentRequired accountId)
    Right (Just enrollment) -> completeStoredEnrollment context accountId enrollment

completeStoredEnrollment :: SecondFactorContext -> AccountId -> StoredTotpEnrollment -> IO PasswordMfaLoginResult
completeStoredEnrollment context accountId enrollment =
  case storedTotpConfirmedAtNanoseconds enrollment of
    Nothing -> pure (PasswordMfaLoginEnrollmentRequired accountId)
    Just _ -> verifyProof context accountId enrollment

verifyProof :: SecondFactorContext -> AccountId -> StoredTotpEnrollment -> IO PasswordMfaLoginResult
verifyProof context accountId enrollment =
  case secondFactorProof context of
    TotpLoginProof suppliedCode -> verifyTotpProof context accountId enrollment suppliedCode
    RecoveryCodeLoginProof suppliedCode -> completeRecoveryCode context accountId suppliedCode

-- | Counter replay is rejected before a store update, and the update is an
-- atomic conditional write to reject concurrent reuse as well.
verifyTotpProof :: SecondFactorContext -> AccountId -> StoredTotpEnrollment -> TotpCode -> IO PasswordMfaLoginResult
verifyTotpProof context accountId enrollment suppliedCode =
  runAdmittedLoginAttempt
    (passwordLoginThrottle (secondFactorPasswordLoginEnvironment context))
    (secondFactorAttemptBudgets (passwordLoginThrottle (secondFactorPasswordLoginEnvironment context)) accountId)
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
              markResult <- markTotpCodeUsed mfaStore accountId matchedCounter
              case markResult of
                Left storeError -> pure (PasswordMfaLoginMfaStoreError storeError, Nothing)
                Right True -> pure (PasswordMfaLoginAccepted accountId, Just True)
                Right False -> pure (PasswordMfaLoginRejected, Just False)
  where
    mfaStore = passwordLoginMfaStore (secondFactorPasswordLoginEnvironment context)

completeRecoveryCode :: SecondFactorContext -> AccountId -> RecoveryCode -> IO PasswordMfaLoginResult
completeRecoveryCode context accountId suppliedCode =
  runAdmittedLoginAttempt throttle (secondFactorAttemptBudgets throttle accountId) PasswordMfaLoginThrottled PasswordMfaLoginAttemptStoreError work
  where
    environment = secondFactorPasswordLoginEnvironment context
    mfaStore = passwordLoginMfaStore environment
    throttle = passwordLoginThrottle environment
    work = do
      recoveryResult <- runExceptT $ do
        recoveryHashValues <- liftMfaStore (loadUnusedRecoveryCodeHashes mfaStore accountId)
        fromMaybeError LoginCorruptEnrollment (traverse readRecoveryCodeHash recoveryHashValues)
      case recoveryResult of
        Left infrastructureError -> pure (infrastructureFailureResult infrastructureError, Nothing)
        Right recoveryHashes -> do
          matchingHash <- findMatchingRecoveryHash (passwordLoginWorkGate environment) suppliedCode recoveryHashes
          case matchingHash of
            Nothing -> pure (PasswordMfaLoginPasswordWorkBudgetExhausted, Nothing)
            Just Nothing -> pure (PasswordMfaLoginRejected, Just False)
            Just (Just hashValue) -> do
              consumed <- runExceptT (consumeMatchingHash hashValue)
              case consumed of
                Left infrastructureError -> pure (infrastructureFailureResult infrastructureError, Nothing)
                Right True -> pure (PasswordMfaLoginAccepted accountId, Just True)
                Right False -> pure (PasswordMfaLoginRejected, Just False)
    consumeMatchingHash matchingHash =
      liftMfaStore
        (consumeRecoveryCodeHash mfaStore accountId (recoveryCodeHashText matchingHash) (secondFactorNowNanoseconds context))

secondFactorAttemptBudgets :: LoginThrottleContext -> AccountId -> LoginAttemptBudgets
secondFactorAttemptBudgets throttle accountId =
  mkLoginAttemptBudgets
    ( LoginAttemptBudget (LoginPrincipalScope (KnownAccountPrincipal accountId SecondFactorLoginStage)) (loginThrottlePolicy throttle)
        :| [LoginAttemptBudget (LoginPeerScope clientAddress) (loginThrottlePolicy throttle)]
    )
  where
    clientAddress = loginThrottleClientAddress throttle

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
