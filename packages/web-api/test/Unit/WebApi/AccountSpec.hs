{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import HarchWeb.Account qualified as Account
import HarchWeb.Email qualified as Email
import HarchWeb.Password qualified as Password
import HarchWeb.Username qualified as Username
import Unit.WebApi.TestSupport hiding (accountId, databaseConfig, emailAddress)
import WebApi.Account (AccountProfile (..), AccountStore (..), AccountStoreError (..), CreatePendingAccountOutcome (..), PendingAccount (..), RegistrationEnvironment (..), RegistrationError (..), RegistrationRequest (..), RegistrationResult (..), ResendVerificationError (..), confirmEmailVerificationAt, registerAccount, resendEmailVerificationAt)

spec = do
  describe "WebApi.Account" $ do
    it "reports exhausted password work before hashing or persisting registration" $ do
      passwordWorkGate <- Password.newPasswordWorkGate (required "password-work budget" (Password.mkPasswordWorkBudget 1))
      let accountStore =
            AccountStore
              { createPendingAccount = \_ -> error "registration persistence must not run after password-work rejection",
                replaceEmailVerification = \_ -> error "unexpected verification replacement",
                findEmailVerification = \_ -> error "unexpected verification lookup",
                consumeEmailVerification = \_ _ -> error "unexpected verification consumption"
              }
          environment =
            RegistrationEnvironment
              { registrationPasswordHasher = \_ _ -> error "password hashing must not run after password-work rejection",
                registrationHashingPolicy = testPasswordHashingPolicy,
                registrationPasswordWorkGate = passwordWorkGate,
                registrationStore = accountStore,
                registrationDelivery = Email.EmailDelivery (\_ -> error "email delivery must not run after password-work rejection"),
                registrationLocale = Email.EmailEnglish,
                registrationVerificationUrl = const "https://account.example.test/verify",
                registrationNow = 100,
                registrationLifetime = 200
              }
      registerAccount environment (registrationRequestOf (requiredEmailAddress "person@example.test"))
        >>= \case
          Left RegistrationPasswordWorkBudgetExhausted -> pure ()
          _ -> expectationFailure "expected password-work budget exhaustion"

    it "persists only a password hash and verification digest before delivering a localized verification email" $ do
      pendingAccountsReference <- newIORef []
      deliveredMessagesReference <- newIORef []
      let accountStore =
            AccountStore
              { createPendingAccount = \pendingAccount -> do
                  modifyIORef' pendingAccountsReference (<> [pendingAccount])
                  pure (Right PendingAccountCreated),
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
              registrationStore = accountStore,
              registrationDelivery = emailDelivery,
              registrationLocale = Email.EmailSpanish,
              registrationVerificationUrl = \token -> "https://account.example.test/es/verify?token=" <> Account.emailVerificationTokenText token,
              registrationNow = 100,
              registrationLifetime = 200
            }
          RegistrationRequest
            { registrationEmail = emailAddress,
              registrationPassword = Password.mkPassword "correct horse battery staple",
              registrationUsername = Nothing,
              registrationDisplayName = Nothing
            }
      pendingAccounts <- readIORef pendingAccountsReference
      deliveredMessages <- readIORef deliveredMessagesReference
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
          Email.emailMessageSubject message `shouldBe` "Verifica tu correo electronico"
          Email.emailMessageBody message `shouldSatisfy` Text.isPrefixOf "Abre este enlace para verificar tu correo electronico:\nhttps://account.example.test/es/verify?token="
        _ -> expectationFailure "expected exactly one pending account and verification email"

    it "persists typed account identity without changing verification delivery" $ do
      pendingAccountsReference <- newIORef []
      let accountStore =
            AccountStore
              { createPendingAccount = \pendingAccount -> modifyIORef' pendingAccountsReference (<> [pendingAccount]) >> pure (Right PendingAccountCreated),
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
                registrationStore = accountStore,
                registrationDelivery = Email.EmailDelivery (\_ -> pure ()),
                registrationLocale = Email.EmailEnglish,
                registrationVerificationUrl = const "https://account.example.test/verify",
                registrationNow = 100,
                registrationLifetime = 200
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
              { createPendingAccount = \_ -> error "password hashing should stop before persistence",
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
              { createPendingAccount = \pendingAccount -> modifyIORef' pendingAccountsReference (<> [pendingAccount]) >> pure (Right PendingAccountCreated),
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
      equalValues (AccountStoreUnavailable "database unavailable") (AccountStoreUnavailable "database unavailable") `shouldBe` True
      equalValues (AccountStoreCorruptData "malformed account") (AccountStoreCorruptData "malformed account") `shouldBe` True
      equalValues (AccountStoreUnavailable "database unavailable") (AccountStoreCorruptData "database unavailable") `shouldBe` False
      renderedValue (AccountStoreUnavailable "database unavailable") `shouldBe` "AccountStoreUnavailable \"database unavailable\""
      renderedValue (AccountStoreCorruptData "malformed account") `shouldBe` "AccountStoreCorruptData \"malformed account\""
      equalValues ResendVerificationNoLongerPending ResendVerificationNoLongerPending `shouldBe` True
      equalValues (ResendVerificationStoreError (AccountStoreUnavailable "database unavailable")) (ResendVerificationStoreError (AccountStoreUnavailable "database unavailable")) `shouldBe` True
      equalValues (ResendVerificationDeliveryFailed "SMTP unavailable") (ResendVerificationDeliveryFailed "SMTP unavailable") `shouldBe` True
      equalValues ResendVerificationClockOverflow ResendVerificationClockOverflow `shouldBe` True
      equalValues (ResendVerificationStoreError (AccountStoreUnavailable "database unavailable")) (ResendVerificationDeliveryFailed "SMTP unavailable") `shouldBe` False
      equalValues ResendVerificationClockOverflow ResendVerificationNoLongerPending `shouldBe` False
      renderedValue (ResendVerificationStoreError (AccountStoreUnavailable "database unavailable")) `shouldBe` "ResendVerificationStoreError (AccountStoreUnavailable \"database unavailable\")"
      renderedValue (ResendVerificationDeliveryFailed "SMTP unavailable") `shouldBe` "ResendVerificationDeliveryFailed \"SMTP unavailable\""
      renderedValue ResendVerificationClockOverflow `shouldBe` "ResendVerificationClockOverflow"
      renderedValue ResendVerificationNoLongerPending `shouldBe` "ResendVerificationNoLongerPending"
      expectAll
        ( ((AccountStoreUnavailable "database unavailable" /= AccountStoreCorruptData "database unavailable") `shouldBe` True)
            :| [ show [AccountStoreUnavailable "database unavailable"] `shouldBe` "[AccountStoreUnavailable \"database unavailable\"]",
                 (ResendVerificationStoreError (AccountStoreUnavailable "database unavailable") /= ResendVerificationDeliveryFailed "database unavailable") `shouldBe` True,
                 show [ResendVerificationStoreError (AccountStoreUnavailable "database unavailable")] `shouldBe` "[ResendVerificationStoreError (AccountStoreUnavailable \"database unavailable\")]"
               ]
        )

    it "does not send an email when registration is already present, the username is taken, or persistence fails" $ do
      deliveredMessagesReference <- newIORef []
      let emailDelivery = Email.EmailDelivery (\message -> modifyIORef' deliveredMessagesReference (<> [message]))
          emailAddress = requiredEmailAddress "person@example.test"
          existingStore =
            AccountStore
              { createPendingAccount = \_ -> pure (Right PendingAccountEmailTaken),
                replaceEmailVerification = \_ -> error "unexpected verification replacement",
                findEmailVerification = \_ -> error "unexpected verification lookup",
                consumeEmailVerification = \_ _ -> error "unexpected verification consumption"
              }
          takenUsernameStore = existingStore {createPendingAccount = \_ -> pure (Right PendingAccountUsernameTaken)}
          unavailableStore = existingStore {createPendingAccount = \_ -> pure (Left (AccountStoreUnavailable "database unavailable"))}
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
              { createPendingAccount = \_ -> error "unexpected account creation",
                replaceEmailVerification = \verification -> writeIORef storedVerificationReference (Just verification) >> pure (Right True),
                findEmailVerification = \_ -> error "unexpected verification lookup",
                consumeEmailVerification = \_ _ -> error "unexpected verification consumption"
              }
          unavailableStore = successfulStore {replaceEmailVerification = \_ -> pure (Left (AccountStoreUnavailable "database unavailable"))}
          noLongerPendingStore = successfulStore {replaceEmailVerification = \_ -> pure (Right False)}
          delivery = Email.EmailDelivery (\message -> modifyIORef' deliveredMessagesReference (<> [message]))
          failingDelivery = Email.EmailDelivery (\_ -> ioError (userError "SMTP unavailable"))
          resend store emailDelivery profile now lifetime =
            resendEmailVerificationAt store emailDelivery Email.EmailSpanish (\token -> "https://account.example.test/es/verify?token=" <> Account.emailVerificationTokenText token) now lifetime profile
      resend successfulStore delivery pendingProfile 100 200 >>= (`shouldSatisfy` \case Right () -> True; _ -> False)
      storedVerification <- readIORef storedVerificationReference
      deliveredMessages <- readIORef deliveredMessagesReference
      case (storedVerification, deliveredMessages) of
        (Just verification, [message]) -> do
          Account.storedVerificationAccountId verification `shouldBe` accountId
          Account.storedVerificationEmail verification `shouldBe` emailAddress
          Account.storedVerificationExpiresAtNanoseconds verification `shouldBe` 300
          Email.emailMessageRecipient message `shouldBe` emailAddress
          Email.emailMessageSubject message `shouldBe` "Verifica tu correo electronico"
          Email.emailMessageBody message `shouldSatisfy` Text.isPrefixOf "Abre este enlace para verificar tu correo electronico:\nhttps://account.example.test/es/verify?token="
        _ -> expectationFailure "expected a rotated verification and one email"
      resend unavailableStore delivery pendingProfile 100 200 >>= (`shouldSatisfy` \case Left (ResendVerificationStoreError storeError) -> isUnavailable "database unavailable" storeError; _ -> False)
      resend noLongerPendingStore delivery pendingProfile 100 200 >>= (`shouldSatisfy` \case Left ResendVerificationNoLongerPending -> True; _ -> False)
      resend successfulStore failingDelivery pendingProfile 100 200 >>= (`shouldSatisfy` \case Left (ResendVerificationDeliveryFailed detail) -> "SMTP unavailable" `Text.isInfixOf` detail; _ -> False)
      resend successfulStore delivery pendingProfile maxBound 1 >>= (`shouldSatisfy` \case Left ResendVerificationClockOverflow -> True; _ -> False)
      resend successfulStore delivery verifiedProfile 100 200 >>= (`shouldSatisfy` \case Left ResendVerificationNoLongerPending -> True; _ -> False)

    it "reports delivery failures after the pending account has been stored and rejects overflowing expiry calculations" $ do
      pendingAccountsReference <- newIORef []
      let accountStore =
            AccountStore
              { createPendingAccount = \pendingAccount -> modifyIORef' pendingAccountsReference (<> [pendingAccount]) >> pure (Right PendingAccountCreated),
                replaceEmailVerification = \_ -> error "unexpected verification replacement",
                findEmailVerification = \_ -> error "unexpected verification lookup",
                consumeEmailVerification = \_ _ -> error "unexpected verification consumption"
              }
          failingDelivery = Email.EmailDelivery (\_ -> ioError (userError "SMTP unavailable"))
          emailAddress = requiredEmailAddress "person@example.test"
      assertRegistrationResult
        (registerAccount (registrationEnvironmentAt Password.hashPassword accountStore failingDelivery 100 200) (registrationRequestOf emailAddress))
        (\case Left (RegistrationDeliveryFailed message) -> "SMTP unavailable" `Text.isInfixOf` message; _ -> False)
      length <$> readIORef pendingAccountsReference `shouldReturn` 1
      assertRegistrationResult
        (registerAccount (registrationEnvironmentAt Password.hashPassword accountStore failingDelivery maxBound 1) (registrationRequestOf emailAddress))
        (\case Left RegistrationClockOverflow -> True; _ -> False)

    it "validates and atomically consumes a matching verification token" $ do
      let accountId = requiredAccountId "account_01"
          emailAddress = requiredEmailAddress "person@example.test"
          token = requiredVerificationToken (Text.replicate 43 "a")
          storedVerification = Account.mkStoredEmailVerification accountId emailAddress 500 token
          accountStore =
            AccountStore
              { createPendingAccount = \_ -> error "unexpected account creation",
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
              { createPendingAccount = \_ -> error "unexpected account creation",
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
