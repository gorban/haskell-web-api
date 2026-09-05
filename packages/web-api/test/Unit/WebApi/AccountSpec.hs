{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Concurrent (forkIO, killThread, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Exception (AsyncException (UserInterrupt), SomeException, throwIO, try)
import Control.Monad (when)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import HarchWeb.Account qualified as Account
import HarchWeb.Email qualified as Email
import HarchWeb.Password qualified as Password
import HarchWeb.Username qualified as Username
import Unit.WebApi.TestSupport hiding (accountId, databaseConfig, emailAddress)
import WebApi.Account (AccountProfile (..), AccountStore (..), AccountStoreError (..), CreatePendingAccountOutcome (..), EmailVerificationEnvironment (..), PendingAccount (..), PendingRegistrationClaim (..), PendingRegistrationDeliveryStage (..), RegistrationEnvironment (..), RegistrationError (..), RegistrationRequest (..), RegistrationResult (..), ResendVerificationError (..), ResendVerificationResult (..), VerificationDeliveryEnvironment (..), VerificationDeliveryFailure (..), VerificationResendAdmission (..), VerificationResendClaim (..), VerificationResendClaimSettlement (..), VerificationResendSuppression (..), confirmEmailVerificationAt, defaultPendingRegistrationStoragePolicy, defaultRegistrationDeliveryTimeout, defaultVerificationResendPolicy, mkPendingRegistrationStoragePolicy, mkRegistrationDeliveryTimeout, mkVerificationResendPolicy, pendingRegistrationClaimLeaseNanoseconds, pendingRegistrationMaximumAccounts, registerAccount, resendEmailVerificationAt, verificationResendClaimLeaseNanoseconds, verificationResendMaximumDeliveries, verificationResendMaximumRecords, verificationResendWindowNanoseconds)

spec = do
  describe "WebApi.Account" $ do
    it "reports exhausted password work before hashing or persisting registration" $ do
      passwordWorkGate <- Password.newPasswordWorkGate (required "password-work budget" (Password.mkPasswordWorkBudget 1))
      let accountStore =
            AccountStore
              { createPendingAccount = \_ _ -> error "registration persistence must not run after password-work rejection",
                completePendingRegistrationDelivery = \_ -> error "unexpected registration delivery completion",
                releasePendingRegistrationDelivery = \_ -> error "unexpected registration delivery release",
                reserveVerificationResend = \_ _ _ -> error "unexpected verification resend",
                completeVerificationResend = \_ _ -> error "unexpected verification resend completion",
                releaseVerificationResend = \_ -> error "unexpected verification resend release",
                replaceEmailVerification = \_ -> error "unexpected verification replacement",
                findEmailVerification = \_ -> error "unexpected verification lookup",
                consumeEmailVerification = \_ _ -> error "unexpected verification consumption"
              }
          environment =
            RegistrationEnvironment
              { registrationPasswordHasher = \_ _ -> error "password hashing must not run after password-work rejection",
                registrationHashingPolicy = testPasswordHashingPolicy,
                registrationPasswordWorkGate = passwordWorkGate,
                registrationStoragePolicy = defaultPendingRegistrationStoragePolicy,
                registrationVerificationEnvironment =
                  EmailVerificationEnvironment
                    { verificationStore = accountStore,
                      verificationDeliveryEnvironment =
                        VerificationDeliveryEnvironment
                          { verificationDeliveryTimeout = defaultRegistrationDeliveryTimeout,
                            verificationDelivery = Email.EmailDelivery (\_ -> error "email delivery must not run after password-work rejection"),
                            verificationLocale = Email.EmailEnglish,
                            verificationUrl = const "https://account.example.test/verify"
                          },
                      verificationNow = 100,
                      verificationLifetime = 200
                    }
              }
      registerAccount environment (registrationRequestOf (requiredEmailAddress "person@example.test"))
        >>= \case
          Left RegistrationPasswordWorkBudgetExhausted -> pure ()
          _ -> expectationFailure "expected password-work budget exhaustion"

    it "persists only a password hash and verification digest before delivering a localized verification email" $ do
      pendingAccountsReference <- newIORef []
      deliveredMessagesReference <- newIORef []
      settledClaimsReference <- newIORef []
      let accountStore =
            AccountStore
              { createPendingAccount = \_ pendingAccount -> do
                  modifyIORef' pendingAccountsReference (<> [pendingAccount])
                  pure (Right PendingAccountCreated),
                completePendingRegistrationDelivery = \claim -> modifyIORef' settledClaimsReference (<> [claim]) >> pure (Right True),
                releasePendingRegistrationDelivery = \_ -> pure (Right True),
                reserveVerificationResend = \_ _ _ -> error "unexpected verification resend",
                completeVerificationResend = \_ _ -> error "unexpected verification resend completion",
                releaseVerificationResend = \_ -> error "unexpected verification resend release",
                replaceEmailVerification = \_ -> error "unexpected verification replacement",
                findEmailVerification = \_ -> error "unexpected verification lookup",
                consumeEmailVerification = \_ _ -> error "unexpected verification consumption"
              }
          emailDelivery = Email.EmailDelivery (\message -> modifyIORef' deliveredMessagesReference (<> [message]))
          emailAddress = requiredEmailAddress "person@example.test"
      registrationResult <-
        registerAccount
          RegistrationEnvironment
            { registrationPasswordHasher = Password.hashPassword,
              registrationHashingPolicy = testPasswordHashingPolicy,
              registrationPasswordWorkGate = testPasswordWorkGate,
              registrationStoragePolicy = defaultPendingRegistrationStoragePolicy,
              registrationVerificationEnvironment =
                EmailVerificationEnvironment
                  { verificationStore = accountStore,
                    verificationDeliveryEnvironment =
                      VerificationDeliveryEnvironment
                        { verificationDeliveryTimeout = defaultRegistrationDeliveryTimeout,
                          verificationDelivery = emailDelivery,
                          verificationLocale = Email.EmailSpanish,
                          verificationUrl = \token -> "https://account.example.test/es/verify?token=" <> Account.emailVerificationTokenText token
                        },
                    verificationNow = 100,
                    verificationLifetime = 200
                  }
            }
          RegistrationRequest
            { registrationEmail = emailAddress,
              registrationPassword = Password.mkPassword "correct horse battery staple",
              registrationUsername = Nothing,
              registrationDisplayName = Nothing
            }
      pendingAccounts <- readIORef pendingAccountsReference
      deliveredMessages <- readIORef deliveredMessagesReference
      settledClaims <- readIORef settledClaimsReference
      createdAccountId <-
        case registrationResult of
          Right (RegistrationCreated accountId) -> pure accountId
          _ -> expectationFailure "expected a created registration" >> pure (requiredAccountId "unreachable")
      case (pendingAccounts, deliveredMessages) of
        ([pendingAccount], [message]) -> do
          createdAccountId `shouldBe` pendingAccountId pendingAccount
          pendingAccountEmail pendingAccount `shouldBe` emailAddress
          pendingAccountCreatedAtNanoseconds pendingAccount `shouldBe` 100
          Account.storedVerificationAccountId (pendingAccountVerification pendingAccount) `shouldBe` pendingAccountId pendingAccount
          Account.storedVerificationEmail (pendingAccountVerification pendingAccount) `shouldBe` emailAddress
          Account.storedVerificationExpiresAtNanoseconds (pendingAccountVerification pendingAccount) `shouldBe` 300
          Account.emailVerificationTokenDigestText (Account.storedVerificationTokenDigest (pendingAccountVerification pendingAccount)) `shouldSatisfy` (not . Text.null)
          Password.verifyPassword (Password.mkPassword "correct horse battery staple") (pendingAccountPasswordHash pendingAccount) `shouldBe` True
          Email.emailMessageRecipient message `shouldBe` emailAddress
          Email.emailMessageSubject message `shouldBe` "Verifica tu correo electrónico"
          Email.emailMessageBody message `shouldSatisfy` Text.isPrefixOf "Abre este enlace para verificar tu correo electrónico:\nhttps://account.example.test/es/verify?token="
          case settledClaims of
            [claim] -> do
              pendingRegistrationClaimAccountId claim `shouldBe` pendingAccountId pendingAccount
              pendingRegistrationClaimTokenDigest claim `shouldBe` Account.storedVerificationTokenDigest (pendingAccountVerification pendingAccount)
              case pendingRegistrationClaimStage claim of
                PendingRegistrationCreated -> pure ()
                PendingRegistrationRetried -> expectationFailure "created registrations must settle a created claim"
            _ -> expectationFailure "expected the created delivery claim to be settled"
        _ -> expectationFailure "expected exactly one pending account and verification email"

    it "persists typed account identity without changing verification delivery" $ do
      pendingAccountsReference <- newIORef []
      let accountStore =
            AccountStore
              { createPendingAccount = \_ pendingAccount -> modifyIORef' pendingAccountsReference (<> [pendingAccount]) >> pure (Right PendingAccountCreated),
                completePendingRegistrationDelivery = \_ -> pure (Right True),
                releasePendingRegistrationDelivery = \_ -> pure (Right True),
                reserveVerificationResend = \_ _ _ -> error "unexpected verification resend",
                completeVerificationResend = \_ _ -> error "unexpected verification resend completion",
                releaseVerificationResend = \_ -> error "unexpected verification resend release",
                replaceEmailVerification = \_ -> error "unexpected verification replacement",
                findEmailVerification = \_ -> error "unexpected verification lookup",
                consumeEmailVerification = \_ _ -> error "unexpected verification consumption"
              }
          username = fromMaybe (error "expected username") (Username.mkUsername "person_01")
          emailAddress = requiredEmailAddress "person@example.test"
      assertRegistrationResult
        ( registerAccount
            RegistrationEnvironment
              { registrationPasswordHasher = Password.hashPassword,
                registrationHashingPolicy = testPasswordHashingPolicy,
                registrationPasswordWorkGate = testPasswordWorkGate,
                registrationStoragePolicy = defaultPendingRegistrationStoragePolicy,
                registrationVerificationEnvironment =
                  EmailVerificationEnvironment
                    { verificationStore = accountStore,
                      verificationDeliveryEnvironment =
                        VerificationDeliveryEnvironment
                          { verificationDeliveryTimeout = defaultRegistrationDeliveryTimeout,
                            verificationDelivery = Email.EmailDelivery (\_ -> pure ()),
                            verificationLocale = Email.EmailEnglish,
                            verificationUrl = const "https://account.example.test/verify"
                          },
                      verificationNow = 100,
                      verificationLifetime = 200
                    }
              }
            RegistrationRequest
              { registrationEmail = emailAddress,
                registrationPassword = Password.mkPassword "correct horse battery staple",
                registrationUsername = Just username,
                registrationDisplayName = Just "Person Example"
              }
        )
        (\case Right (RegistrationCreated _) -> True; _ -> False)
      pendingAccounts <- readIORef pendingAccountsReference
      case pendingAccounts of
        [pendingAccount] -> do
          pendingAccountUsername pendingAccount `shouldBe` Just username
          pendingAccountDisplayName pendingAccount `shouldBe` Just "Person Example"
        _ -> expectationFailure "expected exactly one pending account"

    it "covers password-hashing failures and account-workflow value representations" $ do
      let accountStore =
            AccountStore
              { createPendingAccount = \_ _ -> error "password hashing should stop before persistence",
                completePendingRegistrationDelivery = \_ -> error "unexpected registration delivery completion",
                releasePendingRegistrationDelivery = \_ -> error "unexpected registration delivery release",
                reserveVerificationResend = \_ _ _ -> error "unexpected verification resend",
                completeVerificationResend = \_ _ -> error "unexpected verification resend completion",
                releaseVerificationResend = \_ -> error "unexpected verification resend release",
                replaceEmailVerification = \_ -> error "unexpected verification replacement",
                findEmailVerification = \_ -> error "unexpected verification lookup",
                consumeEmailVerification = \_ _ -> error "unexpected verification consumption"
              }
          emailDelivery = Email.EmailDelivery (\_ -> error "password hashing should stop before delivery")
          emailAddress = requiredEmailAddress "person@example.test"
          accountId = requiredAccountId "account_01"
      assertRegistrationResult
        (registerAccount (registrationEnvironmentAt (\_ _ -> pure Nothing) accountStore emailDelivery 100 200) (registrationRequestOf emailAddress))
        (\case Left RegistrationPasswordHashingFailed -> True; _ -> False)
      pendingAccountsReference <- newIORef []
      let successfulStore =
            AccountStore
              { createPendingAccount = \_ pendingAccount -> modifyIORef' pendingAccountsReference (<> [pendingAccount]) >> pure (Right PendingAccountCreated),
                completePendingRegistrationDelivery = \_ -> pure (Right True),
                releasePendingRegistrationDelivery = \_ -> pure (Right True),
                reserveVerificationResend = \_ _ _ -> error "unexpected verification resend",
                completeVerificationResend = \_ _ -> error "unexpected verification resend completion",
                releaseVerificationResend = \_ -> error "unexpected verification resend release",
                replaceEmailVerification = \_ -> error "unexpected verification replacement",
                findEmailVerification = \_ -> error "unexpected verification lookup",
                consumeEmailVerification = \_ _ -> error "unexpected verification consumption"
              }
      assertRegistrationResult
        (registerAccount (registrationEnvironmentAt Password.hashPassword successfulStore (Email.EmailDelivery (\_ -> pure ())) 100 200) (registrationRequestOf emailAddress))
        (\case Right (RegistrationCreated _) -> True; _ -> False)
      readIORef pendingAccountsReference >>= \case
        [pendingAccount] -> do
          pendingAccountUsername pendingAccount `shouldBe` Nothing
          pendingAccountDisplayName pendingAccount `shouldBe` Nothing
        _ -> expectationFailure "expected one pending account"
      Account.accountIdText accountId `shouldBe` "account_01"
      AccountStoreUnavailable "database unavailable" `shouldNotBe` AccountStoreCorruptData "database unavailable"
      show (AccountStoreUnavailable "database unavailable") `shouldBe` "AccountStoreUnavailable \"database unavailable\""
      show (AccountStoreCorruptData "malformed account") `shouldBe` "AccountStoreCorruptData \"malformed account\""
      expectAll
        ( ((AccountStoreUnavailable "database unavailable" /= AccountStoreCorruptData "database unavailable") `shouldBe` True)
            :| [ show [AccountStoreUnavailable "database unavailable"] `shouldBe` "[AccountStoreUnavailable \"database unavailable\"]",
                 pure ()
               ]
        )

    it "does not send an email when registration is already present, the username is taken, or persistence fails" $ do
      deliveredMessagesReference <- newIORef []
      let emailDelivery = Email.EmailDelivery (\message -> modifyIORef' deliveredMessagesReference (<> [message]))
          emailAddress = requiredEmailAddress "person@example.test"
          existingStore =
            AccountStore
              { createPendingAccount = \_ _ -> pure (Right PendingAccountEmailTaken),
                completePendingRegistrationDelivery = \_ -> pure (Right True),
                releasePendingRegistrationDelivery = \_ -> pure (Right True),
                reserveVerificationResend = \_ _ _ -> error "unexpected verification resend",
                completeVerificationResend = \_ _ -> error "unexpected verification resend completion",
                releaseVerificationResend = \_ -> error "unexpected verification resend release",
                replaceEmailVerification = \_ -> error "unexpected verification replacement",
                findEmailVerification = \_ -> error "unexpected verification lookup",
                consumeEmailVerification = \_ _ -> error "unexpected verification consumption"
              }
          takenUsernameStore = existingStore {createPendingAccount = \_ _ -> pure (Right PendingAccountUsernameTaken)}
          unavailableStore = existingStore {createPendingAccount = \_ _ -> pure (Left (AccountStoreUnavailable "database unavailable"))}
      assertRegistrationResult
        (registerAccount (registrationEnvironmentAt Password.hashPassword existingStore emailDelivery 100 200) (registrationRequestOf emailAddress))
        (\case Right RegistrationAlreadyRegistered -> True; _ -> False)
      assertRegistrationResult
        (registerAccount (registrationEnvironmentAt Password.hashPassword takenUsernameStore emailDelivery 100 200) (registrationRequestOf emailAddress))
        (\case Right RegistrationUsernameTaken -> True; _ -> False)
      assertRegistrationResult
        (registerAccount (registrationEnvironmentAt Password.hashPassword unavailableStore emailDelivery 100 200) (registrationRequestOf emailAddress))
        (\case Left (RegistrationStoreError storeError) -> isUnavailable "database unavailable" storeError; _ -> False)
      readIORef deliveredMessagesReference `shouldReturn` []

    it "rotates a pending account's verification token before resending its localized email" $ do
      storedVerificationReference <- newIORef Nothing
      deliveredMessagesReference <- newIORef []
      let accountId = requiredAccountId "account_01"
          emailAddress = requiredEmailAddress "person@example.test"
          pendingProfile = AccountProfile accountId emailAddress Nothing Nothing False
          verifiedProfile = AccountProfile accountId emailAddress Nothing Nothing True
          successfulStore =
            AccountStore
              { createPendingAccount = \_ _ -> error "unexpected account creation",
                completePendingRegistrationDelivery = \_ -> error "unexpected registration delivery completion",
                releasePendingRegistrationDelivery = \_ -> error "unexpected registration delivery release",
                reserveVerificationResend = \policy verification now -> do
                  verificationResendMaximumDeliveries policy `shouldBe` verificationResendMaximumDeliveries defaultVerificationResendPolicy
                  now `shouldBe` 100
                  writeIORef storedVerificationReference (Just verification)
                  pure (Right (VerificationResendReserved (VerificationResendClaim (Account.storedVerificationAccountId verification) (Account.storedVerificationTokenDigest verification)))),
                completeVerificationResend = \claim now -> do
                  case claim of
                    VerificationResendClaim claimAccountId claimDigest -> do
                      Account.accountIdText claimAccountId `shouldBe` Account.accountIdText accountId
                      Account.emailVerificationTokenDigestText claimDigest `shouldSatisfy` (not . Text.null)
                  now `shouldBe` 100
                  pure (Right VerificationResendClaimSettled),
                releaseVerificationResend = \claim -> do
                  case claim of
                    VerificationResendClaim claimAccountId claimDigest -> do
                      Account.accountIdText claimAccountId `shouldBe` Account.accountIdText accountId
                      Account.emailVerificationTokenDigestText claimDigest `shouldSatisfy` (not . Text.null)
                  pure (Right VerificationResendClaimSettled),
                replaceEmailVerification = \_ -> error "unexpected verification replacement",
                findEmailVerification = \_ -> error "unexpected verification lookup",
                consumeEmailVerification = \_ _ -> error "unexpected verification consumption"
              }
          unavailableStore = successfulStore {reserveVerificationResend = \policy verification now -> verificationResendMaximumDeliveries policy `seq` Account.storedVerificationAccountId verification `seq` now `seq` pure (Left (AccountStoreUnavailable "database unavailable"))}
          noLongerPendingStore = successfulStore {reserveVerificationResend = \policy verification now -> verificationResendMaximumDeliveries policy `seq` Account.storedVerificationAccountId verification `seq` now `seq` pure (Right (VerificationResendAdmissionSuppressed VerificationResendNoLongerPending))}
          delivery = Email.EmailDelivery (\message -> modifyIORef' deliveredMessagesReference (<> [message]))
          failingDelivery = Email.EmailDelivery (\_ -> ioError (userError "SMTP unavailable"))
          resend store emailDelivery profile now lifetime =
            resendEmailVerificationAt
              EmailVerificationEnvironment
                { verificationStore = store,
                  verificationDeliveryEnvironment =
                    VerificationDeliveryEnvironment
                      { verificationDeliveryTimeout = defaultRegistrationDeliveryTimeout,
                        verificationDelivery = emailDelivery,
                        verificationLocale = Email.EmailSpanish,
                        verificationUrl = \token -> "https://account.example.test/es/verify?token=" <> Account.emailVerificationTokenText token
                      },
                  verificationNow = now,
                  verificationLifetime = lifetime
                }
              profile
      resend successfulStore delivery pendingProfile 100 200 >>= \case
        Right ResendVerificationDelivered -> pure ()
        _ -> expectationFailure "expected a delivered resend"
      storedVerification <- readIORef storedVerificationReference
      deliveredMessages <- readIORef deliveredMessagesReference
      case (storedVerification, deliveredMessages) of
        (Just verification, [message]) -> do
          Account.storedVerificationAccountId verification `shouldBe` accountId
          Account.storedVerificationEmail verification `shouldBe` emailAddress
          Account.storedVerificationExpiresAtNanoseconds verification `shouldBe` 300
          Email.emailMessageRecipient message `shouldBe` emailAddress
          Email.emailMessageSubject message `shouldBe` "Verifica tu correo electrónico"
          Email.emailMessageBody message `shouldSatisfy` Text.isPrefixOf "Abre este enlace para verificar tu correo electrónico:\nhttps://account.example.test/es/verify?token="
        _ -> expectationFailure "expected a rotated verification and one email"
      resend unavailableStore delivery pendingProfile 100 200 >>= \case
        Left (ResendVerificationStoreError storeError)
          | isUnavailable "database unavailable" storeError -> pure ()
        _ -> expectationFailure "expected an unavailable resend store"
      resend noLongerPendingStore delivery pendingProfile 100 200 >>= \case
        Right (ResendVerificationSuppressed VerificationResendNoLongerPending) -> pure ()
        _ -> expectationFailure "expected pending-state suppression"
      resend successfulStore failingDelivery pendingProfile 100 200 >>= \case
        Left (ResendVerificationDeliveryFailed VerificationDeliveryTransportFailed) -> pure ()
        _ -> expectationFailure "expected a transport delivery failure"
      resendEmailVerificationAt
        EmailVerificationEnvironment
          { verificationStore = successfulStore,
            verificationDeliveryEnvironment =
              VerificationDeliveryEnvironment
                { verificationDeliveryTimeout = required "delivery timeout" (mkRegistrationDeliveryTimeout 1),
                  verificationDelivery = Email.EmailDelivery (\_ -> threadDelay 50000),
                  verificationLocale = Email.EmailSpanish,
                  verificationUrl = \token -> "https://account.example.test/es/verify?token=" <> Account.emailVerificationTokenText token
                },
            verificationNow = 100,
            verificationLifetime = 200
          }
        pendingProfile
        >>= \case
          Left (ResendVerificationDeliveryFailed VerificationDeliveryTimedOut) -> pure ()
          _ -> expectationFailure "expected a timed-out delivery"
      resend successfulStore delivery pendingProfile maxBound 1 >>= \case
        Left ResendVerificationClockOverflow -> pure ()
        _ -> expectationFailure "expected resend clock overflow"
      resend noLongerPendingStore delivery verifiedProfile 100 200 >>= \case
        Right (ResendVerificationSuppressed VerificationResendNoLongerPending) -> pure ()
        _ -> expectationFailure "expected verified-state suppression"

    it "keeps resend policy and claim settlement outcomes on explicit rails" $ do
      let accountId = requiredAccountId "account_01"
          emailAddress = requiredEmailAddress "person@example.test"
          profile = AccountProfile accountId emailAddress Nothing Nothing False
          candidateClaim verification = VerificationResendClaim (Account.storedVerificationAccountId verification) (Account.storedVerificationTokenDigest verification)
          storeFor admission completion release =
            AccountStore
              { createPendingAccount = \_ _ -> error "unexpected account creation",
                completePendingRegistrationDelivery = \_ -> error "unexpected registration delivery completion",
                releasePendingRegistrationDelivery = \_ -> error "unexpected registration delivery release",
                reserveVerificationResend = \_ verification _ -> pure (Right (admission (candidateClaim verification))),
                completeVerificationResend = \_ _ -> pure completion,
                releaseVerificationResend = \_ -> pure release,
                replaceEmailVerification = \_ -> error "unexpected verification replacement",
                findEmailVerification = \_ -> error "unexpected verification lookup",
                consumeEmailVerification = \_ _ -> error "unexpected verification consumption"
              }
          environment store delivery =
            EmailVerificationEnvironment
              { verificationStore = store,
                verificationDeliveryEnvironment = VerificationDeliveryEnvironment defaultRegistrationDeliveryTimeout delivery Email.EmailEnglish (const "https://account.example.test/verify"),
                verificationNow = 100,
                verificationLifetime = 200
              }
          noDelivery = Email.EmailDelivery (\_ -> error "suppressed resend must not deliver")
          successfulDelivery = Email.EmailDelivery (\_ -> pure ())
          failingDelivery = Email.EmailDelivery (\_ -> ioError (userError "SMTP unavailable"))
          throttledStore = storeFor (const (VerificationResendAdmissionSuppressed VerificationResendThrottled)) (Right VerificationResendClaimSettled) (Right VerificationResendClaimSettled)
          lostCompletionStore = storeFor VerificationResendReserved (Right VerificationResendClaimLost) (Right VerificationResendClaimSettled)
          failedCompletionStore = storeFor VerificationResendReserved (Left (AccountStoreUnavailable "completion unavailable")) (Right VerificationResendClaimSettled)
          failedReleaseStore = storeFor VerificationResendReserved (Right VerificationResendClaimSettled) (Left (AccountStoreUnavailable "release unavailable"))
          lostReleaseStore = storeFor VerificationResendReserved (Right VerificationResendClaimSettled) (Right VerificationResendClaimLost)
      case (mkVerificationResendPolicy 0 1 1 1, mkVerificationResendPolicy 1 0 1 1, mkVerificationResendPolicy 1 1 0 1, mkVerificationResendPolicy 1 1 1 0, mkVerificationResendPolicy 1 2 3 4) of
        (Nothing, Nothing, Nothing, Nothing, Just policy) -> do
          verificationResendMaximumDeliveries policy `shouldBe` 1
          verificationResendWindowNanoseconds policy `shouldBe` 2
          verificationResendClaimLeaseNanoseconds policy `shouldBe` 3
          verificationResendMaximumRecords policy `shouldBe` 4
        _ -> expectationFailure "resend policy must reject every zero bound"
      verificationResendMaximumDeliveries defaultVerificationResendPolicy `shouldBe` 3
      resendEmailVerificationAt (environment throttledStore noDelivery) profile >>= \case
        Right (ResendVerificationSuppressed VerificationResendThrottled) -> pure ()
        _ -> expectationFailure "expected throttled resend suppression"
      resendEmailVerificationAt (environment lostCompletionStore successfulDelivery) profile >>= \case
        Right (ResendVerificationSuppressed VerificationResendNoLongerPending) -> pure ()
        _ -> expectationFailure "expected lost-claim suppression"
      resendEmailVerificationAt (environment failedCompletionStore successfulDelivery) profile >>= \case
        Left (ResendVerificationStoreError (AccountStoreUnavailable "completion unavailable")) -> pure ()
        _ -> expectationFailure "expected a completion-store failure"
      resendEmailVerificationAt (environment failedReleaseStore failingDelivery) profile >>= \case
        Left (ResendVerificationStoreError (AccountStoreUnavailable "release unavailable")) -> pure ()
        _ -> expectationFailure "expected a release-store failure"
      resendEmailVerificationAt (environment lostReleaseStore failingDelivery) profile >>= \case
        Left (ResendVerificationStoreError (AccountStoreCorruptData "verification resend claim was lost while releasing delivery")) -> pure ()
        _ -> expectationFailure "expected a lost-release claim failure"

      let interruptedDelivery = Email.EmailDelivery (\_ -> throwIO UserInterrupt)
      (try (resendEmailVerificationAt (environment (storeFor VerificationResendReserved (Right VerificationResendClaimSettled) (Right VerificationResendClaimSettled)) interruptedDelivery) profile) :: IO (Either AsyncException (Either ResendVerificationError ResendVerificationResult)))
        >>= \case
          Left UserInterrupt -> pure ()
          _ -> expectationFailure "expected resend delivery cancellation to be rethrown"

    it "releases a resend claim when cancellation interrupts SMTP or promotion" $ do
      let accountId = requiredAccountId "account_01"
          emailAddress = requiredEmailAddress "person@example.test"
          profile = AccountProfile accountId emailAddress Nothing Nothing False
          runInterrupted interruptDelivery = do
            claimedReference <- newIORef []
            interruptionPoint <- newEmptyMVar
            completed <- newEmptyMVar
            let store =
                  AccountStore
                    { createPendingAccount = \_ _ -> error "unexpected account creation",
                      completePendingRegistrationDelivery = \_ -> error "unexpected registration delivery completion",
                      releasePendingRegistrationDelivery = \_ -> error "unexpected registration delivery release",
                      reserveVerificationResend = \_ verification _ -> do
                        let claim = VerificationResendClaim (Account.storedVerificationAccountId verification) (Account.storedVerificationTokenDigest verification)
                        pure (Right (VerificationResendReserved claim)),
                      completeVerificationResend = \_ _ -> do
                        if interruptDelivery then pure (Right VerificationResendClaimSettled) else putMVar interruptionPoint () >> threadDelay 10000000 >> pure (Right VerificationResendClaimSettled),
                      releaseVerificationResend = \claim -> do
                        case claim of
                          VerificationResendClaim claimedAccountId claimedDigest -> do
                            Account.accountIdText claimedAccountId `shouldBe` Account.accountIdText accountId
                            Account.emailVerificationTokenDigestText claimedDigest `shouldSatisfy` (not . Text.null)
                        modifyIORef' claimedReference (<> [claim])
                        pure (Right VerificationResendClaimSettled),
                      replaceEmailVerification = \_ -> error "unexpected verification replacement",
                      findEmailVerification = \_ -> error "unexpected verification lookup",
                      consumeEmailVerification = \_ _ -> error "unexpected verification consumption"
                    }
                delivery =
                  Email.EmailDelivery $ \_ ->
                    when interruptDelivery (putMVar interruptionPoint () >> threadDelay 10000000)
                environment =
                  EmailVerificationEnvironment
                    { verificationStore = store,
                      verificationDeliveryEnvironment = VerificationDeliveryEnvironment defaultRegistrationDeliveryTimeout delivery Email.EmailEnglish (const "https://account.example.test/verify"),
                      verificationNow = 100,
                      verificationLifetime = 200
                    }
            worker <- forkIO $ try (resendEmailVerificationAt environment profile) >>= putMVar completed
            takeMVar interruptionPoint
            killThread worker
            _ <- takeMVar completed :: IO (Either SomeException (Either ResendVerificationError ResendVerificationResult))
            readIORef claimedReference
      deliveryClaims <- runInterrupted True
      completionClaims <- runInterrupted False
      length deliveryClaims `shouldBe` 1
      length completionClaims `shouldBe` 1

    it "reports delivery failures after the pending account has been stored and rejects overflowing expiry calculations" $ do
      pendingAccountsReference <- newIORef []
      let accountStore =
            AccountStore
              { createPendingAccount = \_ pendingAccount -> modifyIORef' pendingAccountsReference (<> [pendingAccount]) >> pure (Right PendingAccountCreated),
                completePendingRegistrationDelivery = \_ -> pure (Right True),
                releasePendingRegistrationDelivery = \_ -> pure (Right True),
                reserveVerificationResend = \_ _ _ -> error "unexpected verification resend",
                completeVerificationResend = \_ _ -> error "unexpected verification resend completion",
                releaseVerificationResend = \_ -> error "unexpected verification resend release",
                replaceEmailVerification = \_ -> error "unexpected verification replacement",
                findEmailVerification = \_ -> error "unexpected verification lookup",
                consumeEmailVerification = \_ _ -> error "unexpected verification consumption"
              }
          failingDelivery = Email.EmailDelivery (\_ -> ioError (userError "SMTP unavailable"))
          emailAddress = requiredEmailAddress "person@example.test"
      -- The adapter's arbitrary exception text must not cross the typed
      -- delivery boundary; page-action diagnostics use this constructor's
      -- stable classification instead.
      result <-
        registerAccount
          (registrationEnvironmentAt Password.hashPassword accountStore failingDelivery 100 200)
          (registrationRequestOf emailAddress)
      case result of
        Left (RegistrationDeliveryFailed VerificationDeliveryTransportFailed) -> pure ()
        _ -> expectationFailure "expected a generic registration delivery transport failure"
      length <$> readIORef pendingAccountsReference `shouldReturn` 1
      let releaseFailureStore = accountStore {releasePendingRegistrationDelivery = \_ -> pure (Left (AccountStoreUnavailable "release unavailable"))}
          completionFailureStore = accountStore {completePendingRegistrationDelivery = \_ -> pure (Left (AccountStoreUnavailable "completion unavailable"))}
          successfulDelivery = Email.EmailDelivery (\_ -> pure ())
      assertRegistrationResult
        (registerAccount (registrationEnvironmentAt Password.hashPassword releaseFailureStore failingDelivery 100 200) (registrationRequestOf emailAddress))
        (\case Left (RegistrationStoreError storeError) -> isUnavailable "release unavailable" storeError; _ -> False)
      assertRegistrationResult
        (registerAccount (registrationEnvironmentAt Password.hashPassword completionFailureStore successfulDelivery 100 200) (registrationRequestOf emailAddress))
        (\case Left (RegistrationStoreError storeError) -> isUnavailable "completion unavailable" storeError; _ -> False)
      assertRegistrationResult
        (registerAccount (registrationEnvironmentAt Password.hashPassword accountStore failingDelivery maxBound 1) (registrationRequestOf emailAddress))
        (\case Left RegistrationClockOverflow -> True; _ -> False)

    it "settles a retry claim after delivery and releases it after a failure or timeout" $ do
      settledClaimsReference <- newIORef []
      releasedClaimsReference <- newIORef []
      deliveredBodiesReference <- newIORef []
      let emailAddress = requiredEmailAddress "person@example.test"
          claimStore stage =
            AccountStore
              { createPendingAccount = \_ pendingAccount ->
                  pure
                    ( Right
                        ( PendingAccountDeliveryClaimed
                            ( PendingRegistrationClaim
                                (pendingAccountId pendingAccount)
                                (Account.storedVerificationTokenDigest (pendingAccountVerification pendingAccount))
                                stage
                            )
                        )
                    ),
                completePendingRegistrationDelivery = \claim -> modifyIORef' settledClaimsReference (<> [claim]) >> pure (Right True),
                releasePendingRegistrationDelivery = \claim -> modifyIORef' releasedClaimsReference (<> [claim]) >> pure (Right True),
                reserveVerificationResend = \_ _ _ -> error "unexpected verification resend",
                completeVerificationResend = \_ _ -> error "unexpected verification resend completion",
                releaseVerificationResend = \_ -> error "unexpected verification resend release",
                replaceEmailVerification = \_ -> error "unexpected verification replacement",
                findEmailVerification = \_ -> error "unexpected verification lookup",
                consumeEmailVerification = \_ _ -> error "unexpected verification consumption"
              }
          retryStore = claimStore PendingRegistrationRetried
          createdClaimStore = claimStore PendingRegistrationCreated
          successfulDelivery = Email.EmailDelivery (\message -> modifyIORef' deliveredBodiesReference (<> [Email.emailMessageBody message]))
          failingDelivery = Email.EmailDelivery (\_ -> ioError (userError "SMTP unavailable"))
          timedOutDelivery = Email.EmailDelivery (\_ -> threadDelay 1000000)
          shortTimeout = required "short delivery timeout" (mkRegistrationDeliveryTimeout 1)
          environmentFor accountStore delivery =
            (registrationEnvironmentAt Password.hashPassword accountStore delivery 100 200)
              { registrationVerificationEnvironment =
                  (registrationVerificationEnvironment (registrationEnvironmentAt Password.hashPassword accountStore delivery 100 200))
                    { verificationDeliveryEnvironment =
                        (verificationDeliveryEnvironment (registrationVerificationEnvironment (registrationEnvironmentAt Password.hashPassword accountStore delivery 100 200)))
                          { verificationUrl = \token -> "https://account.example.test/verify?token=" <> Account.emailVerificationTokenText token
                          }
                    }
              }
          retryEnvironment = environmentFor retryStore
      registeredAccountId <-
        registerAccount (retryEnvironment successfulDelivery) (registrationRequestOf emailAddress) >>= \case
          Right (RegistrationRetried accountId) -> pure accountId
          _ -> expectationFailure "expected a settled retry delivery" >> pure (error "unreachable")
      readIORef settledClaimsReference >>= \case
        [claim] -> do
          pendingRegistrationClaimAccountId claim `shouldBe` registeredAccountId
          case pendingRegistrationClaimStage claim of
            PendingRegistrationCreated -> expectationFailure "retried registrations must settle a retry claim"
            PendingRegistrationRetried -> pure ()
        _ -> expectationFailure "expected one completed registration delivery claim"
      registerAccount (environmentFor createdClaimStore successfulDelivery) (registrationRequestOf emailAddress) >>= \case
        Right (RegistrationCreated createdAccountId) -> createdAccountId `shouldSatisfy` (not . Text.null . Account.accountIdText)
        _ -> expectationFailure "expected a created claim to report a created registration"
      deliveredBodies <- readIORef deliveredBodiesReference
      deliveredBodies `shouldSatisfy` all (Text.isInfixOf "?token=")
      registerAccount (retryEnvironment failingDelivery) (registrationRequestOf emailAddress) >>= \case
        Left (RegistrationDeliveryFailed VerificationDeliveryTransportFailed) -> pure ()
        _ -> expectationFailure "expected failed delivery to release its claim"
      let timedOutEnvironment = retryEnvironment timedOutDelivery
      registerAccount (timedOutEnvironment {registrationVerificationEnvironment = (registrationVerificationEnvironment timedOutEnvironment) {verificationDeliveryEnvironment = (verificationDeliveryEnvironment (registrationVerificationEnvironment timedOutEnvironment)) {verificationDeliveryTimeout = shortTimeout}}}) (registrationRequestOf emailAddress) >>= \case
        Left (RegistrationDeliveryFailed VerificationDeliveryTimedOut) -> pure ()
        _ -> expectationFailure "expected timed-out delivery to release its claim"
      readIORef releasedClaimsReference >>= \case
        [failedClaim, timedOutClaim] -> do
          case (pendingRegistrationClaimStage failedClaim, pendingRegistrationClaimStage timedOutClaim) of
            (PendingRegistrationRetried, PendingRegistrationRetried) -> pure ()
            _ -> expectationFailure "failed and timed-out deliveries must release retry claims"
        _ -> expectationFailure "expected failed and timed-out deliveries to release their claims"

    it "keeps invalid registration bounds out of the lifecycle and reports typed staging failures" $ do
      let emailAddress = requiredEmailAddress "person@example.test"
          storageExhaustedStore =
            AccountStore
              { createPendingAccount = \storagePolicy _ -> do
                  pendingRegistrationMaximumAccounts storagePolicy `shouldBe` 100000
                  pendingRegistrationClaimLeaseNanoseconds storagePolicy `shouldBe` 300000000000
                  pure (Right PendingAccountStorageExhausted),
                completePendingRegistrationDelivery = \_ -> error "unexpected registration delivery completion",
                releasePendingRegistrationDelivery = \_ -> error "unexpected registration delivery release",
                reserveVerificationResend = \_ _ _ -> error "unexpected verification resend",
                completeVerificationResend = \_ _ -> error "unexpected verification resend completion",
                releaseVerificationResend = \_ -> error "unexpected verification resend release",
                replaceEmailVerification = \_ -> error "unexpected verification replacement",
                findEmailVerification = \_ -> error "unexpected verification lookup",
                consumeEmailVerification = \_ _ -> error "unexpected verification consumption"
              }
          lostClaimStore =
            storageExhaustedStore
              { createPendingAccount = \_ pendingAccount -> pure (Right (PendingAccountDeliveryClaimed (PendingRegistrationClaim (pendingAccountId pendingAccount) (Account.storedVerificationTokenDigest (pendingAccountVerification pendingAccount)) PendingRegistrationCreated))),
                completePendingRegistrationDelivery = \claim -> do
                  case pendingRegistrationClaimStage claim of
                    PendingRegistrationCreated -> pure ()
                    PendingRegistrationRetried -> expectationFailure "a created registration must complete a created claim"
                  pure (Right False)
              }
          delivery = Email.EmailDelivery (\_ -> pure ())
      case (mkPendingRegistrationStoragePolicy 0 1, mkPendingRegistrationStoragePolicy 1 0, mkPendingRegistrationStoragePolicy 1 1) of
        (Nothing, Nothing, Just storagePolicy) -> do
          pendingRegistrationMaximumAccounts storagePolicy `shouldBe` 1
          pendingRegistrationClaimLeaseNanoseconds storagePolicy `shouldBe` 1
        _ -> expectationFailure "expected only positive pending-registration bounds to be accepted"
      case (mkRegistrationDeliveryTimeout (-1), mkRegistrationDeliveryTimeout 0, mkRegistrationDeliveryTimeout 1) of
        (Nothing, Nothing, Just _) -> pure ()
        _ -> expectationFailure "expected only a positive delivery timeout to be accepted"
      assertRegistrationResult
        (registerAccount (registrationEnvironmentAt Password.hashPassword storageExhaustedStore delivery 100 200) (registrationRequestOf emailAddress))
        (\case Left RegistrationStorageExhausted -> True; _ -> False)
      assertRegistrationResult
        (registerAccount (registrationEnvironmentAt Password.hashPassword lostClaimStore delivery 100 200) (registrationRequestOf emailAddress))
        (\case Left RegistrationDeliveryClaimLost -> True; _ -> False)

    it "validates and atomically consumes a matching verification token" $ do
      let accountId = requiredAccountId "account_01"
          emailAddress = requiredEmailAddress "person@example.test"
          token = requiredVerificationToken (Text.replicate 43 "a")
          storedVerification = Account.mkStoredEmailVerification accountId emailAddress 500 token
          accountStore =
            AccountStore
              { createPendingAccount = \_ _ -> error "unexpected account creation",
                completePendingRegistrationDelivery = \_ -> error "unexpected registration delivery completion",
                releasePendingRegistrationDelivery = \_ -> error "unexpected registration delivery release",
                reserveVerificationResend = \_ _ _ -> error "unexpected verification resend",
                completeVerificationResend = \_ _ -> error "unexpected verification resend completion",
                releaseVerificationResend = \_ -> error "unexpected verification resend release",
                replaceEmailVerification = \_ -> error "unexpected verification replacement",
                findEmailVerification = \digest ->
                  if digest == Account.emailVerificationTokenDigest token
                    then pure (Right (Just storedVerification))
                    else error "unexpected token digest",
                consumeEmailVerification = \digest now ->
                  if digest == Account.emailVerificationTokenDigest token && now == 499
                    then pure (Right (Just accountId))
                    else error "unexpected verification consumption"
              }
      confirmationResult <- confirmEmailVerificationAt accountStore 499 token
      case confirmationResult of
        Right (Account.EmailVerificationAccepted actualAccountId actualEmailAddress) -> do
          actualAccountId `shouldBe` accountId
          actualEmailAddress `shouldBe` emailAddress
        _ -> expectationFailure "expected accepted email verification"

    it "handles missing, expired, raced, corrupt, and unavailable verification records" $ do
      let accountId = requiredAccountId "account_01"
          otherAccountId = requiredAccountId "account_02"
          emailAddress = requiredEmailAddress "person@example.test"
          token = requiredVerificationToken (Text.replicate 43 "a")
          storedVerification = Account.mkStoredEmailVerification accountId emailAddress 500 token
          storeWith lookupResult consumptionResult =
            AccountStore
              { createPendingAccount = \_ _ -> error "unexpected account creation",
                completePendingRegistrationDelivery = \_ -> error "unexpected registration delivery completion",
                releasePendingRegistrationDelivery = \_ -> error "unexpected registration delivery release",
                reserveVerificationResend = \_ _ _ -> error "unexpected verification resend",
                completeVerificationResend = \_ _ -> error "unexpected verification resend completion",
                releaseVerificationResend = \_ -> error "unexpected verification resend release",
                replaceEmailVerification = \_ -> error "unexpected verification replacement",
                findEmailVerification = \_ -> pure lookupResult,
                consumeEmailVerification = \_ _ -> pure consumptionResult
              }
      assertEmailVerificationResult
        (confirmEmailVerificationAt (storeWith (Left (AccountStoreUnavailable "lookup unavailable")) (Right Nothing)) 499 token)
        (\case Left storeError -> isUnavailable "lookup unavailable" storeError; _ -> False)
      assertEmailVerificationResult
        (confirmEmailVerificationAt (storeWith (Right Nothing) (Right Nothing)) 499 token)
        (\case Right Account.EmailVerificationRejected -> True; _ -> False)
      assertEmailVerificationResult
        (confirmEmailVerificationAt (storeWith (Right (Just storedVerification)) (Right Nothing)) 500 token)
        (\case Right Account.EmailVerificationExpired -> True; _ -> False)
      assertEmailVerificationResult
        (confirmEmailVerificationAt (storeWith (Right (Just storedVerification)) (Left (AccountStoreUnavailable "consume unavailable"))) 499 token)
        (\case Left storeError -> isUnavailable "consume unavailable" storeError; _ -> False)
      assertEmailVerificationResult
        (confirmEmailVerificationAt (storeWith (Right (Just storedVerification)) (Right Nothing)) 499 token)
        (\case Right Account.EmailVerificationRejected -> True; _ -> False)
      assertEmailVerificationResult
        (confirmEmailVerificationAt (storeWith (Right (Just storedVerification)) (Right (Just otherAccountId))) 499 token)
        (\case Left storeError -> isCorrupt "email verification was consumed for a different account" storeError; _ -> False)
