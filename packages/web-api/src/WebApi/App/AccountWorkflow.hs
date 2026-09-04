{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | FQ12's private account-workflow composition. Runtime and deliberately
-- unavailable workflows share the one process-wide password-work gate;
-- callers retain an explicit 'AccountWorkflow' value rather than acquiring an
-- ambient service.
module WebApi.App.AccountWorkflow
  ( buildRuntimeAccountWorkflow,
    buildRuntimeAccountWorkflowWithJwt,
    unavailableAccountWorkflow,
  )
where

import Data.Text qualified as Text
import HarchWeb qualified
import HarchWeb.Account qualified as HarchAccount
import HarchWeb.Email qualified as Email
import HarchWeb.Password qualified as Password
import HarchWeb.Time qualified as HarchWebTime
import System.IO.Unsafe (unsafePerformIO)
import WebApi.Account (AccountProfileStore (..), AccountStore (..), AccountStoreError (..), defaultRegistrationDeliveryTimeout)
import WebApi.AccountJwt (AccountJwtIssuer, unavailableAccountJwtIssuer)
import WebApi.AppEffect (AccountWorkflow (..))
import WebApi.Config (AppEnvironmentConfig (..), AppMode (..), SmtpDeliveryConfig (..), defaultAppEnvironmentConfig)
import WebApi.Login (AccountCredentialStore (..), AccountCredentialStoreError (..), LoginAttemptStore (..), LoginAttemptStoreError (..))
import WebApi.Mfa (MfaStore (..), MfaStoreError (..))
import WebApi.Postgres.AccountRepository
  ( buildRuntimePostgresAccountCredentialStore,
    buildRuntimePostgresAccountProfileStore,
    buildRuntimePostgresAccountStore,
  )
import WebApi.Postgres.LoginAttemptRepository (buildRuntimePostgresLoginAttemptStore)
import WebApi.Postgres.MfaEnrollmentSessionRepository (buildRuntimePostgresMfaEnrollmentSessionStore)
import WebApi.Postgres.MfaRepository (buildRuntimePostgresMfaStore)
import WebApi.Postgres.Pool (PostgresPool)
import WebApi.Postgres.SessionRepository (buildRuntimePostgresAccountSessionStore)
import WebApi.Route (AppRequestContext, AppRoute (EmailVerificationRoute), renderRoutePath)
import WebApi.Session (AccountSessionStore (..), AccountSessionStoreError (..), MfaEnrollmentSessionStore (..), MfaEnrollmentSessionStoreError (..))

buildRuntimeAccountWorkflow :: PostgresPool -> AppEnvironmentConfig -> AccountWorkflow
buildRuntimeAccountWorkflow pool environmentConfig =
  buildRuntimeAccountWorkflowWithJwt pool environmentConfig unavailableAccountJwtIssuer

-- | Runtime server construction supplies the startup-validated issuer. The
-- two-argument builder remains useful to storage/SMTP tests, where login
-- issuance is intentionally unavailable rather than silently generating a
-- development key.
buildRuntimeAccountWorkflowWithJwt :: PostgresPool -> AppEnvironmentConfig -> AccountJwtIssuer -> AccountWorkflow
buildRuntimeAccountWorkflowWithJwt pool !environmentConfig jwtIssuer =
  AccountWorkflow
    { accountWorkflowStore = buildRuntimePostgresAccountStore pool,
      accountWorkflowEmailDelivery = runtimeEmailDelivery (appMode environmentConfig) (smtpDeliveryConfig environmentConfig),
      accountWorkflowPasswordHasher = Password.hashPassword,
      accountWorkflowPasswordWorkGate = runtimePasswordWorkGate,
      accountWorkflowRegistrationDeliveryTimeout = defaultRegistrationDeliveryTimeout,
      accountWorkflowClock = HarchWebTime.currentUnixTimeNanoseconds,
      accountWorkflowMfaStore = buildRuntimePostgresMfaStore pool,
      accountWorkflowCredentialStore = buildRuntimePostgresAccountCredentialStore pool,
      accountWorkflowLoginAttemptStore = buildRuntimePostgresLoginAttemptStore pool,
      accountWorkflowSessionStore = buildRuntimePostgresAccountSessionStore pool,
      accountWorkflowMfaEnrollmentSessionStore = buildRuntimePostgresMfaEnrollmentSessionStore pool,
      accountWorkflowProfileStore = buildRuntimePostgresAccountProfileStore pool,
      accountWorkflowTotpEncryptionKey = totpEncryptionKey environmentConfig,
      accountWorkflowCsrfSigningKeyring = csrfSigningKeyring environmentConfig,
      accountWorkflowJwtIssuer = jwtIssuer,
      accountWorkflowTotpClock = HarchWebTime.unixTimeSecondsFromNanoseconds,
      accountWorkflowVerificationUrl = runtimeVerificationUrl (publicBaseUrl environmentConfig)
    }

runtimeEmailDelivery :: AppMode -> SmtpDeliveryConfig -> Email.EmailDelivery
runtimeEmailDelivery mode smtpConfig =
  case Email.mkEmailAddress (smtpDeliverySender smtpConfig) of
    Just sender ->
      case Email.mkSmtpConfig
        Email.SmtpConfigInput
          { Email.smtpInputHost = Email.smtpServerHost (smtpDeliveryHost smtpConfig),
            Email.smtpInputPort = fromIntegral (smtpDeliveryPort smtpConfig),
            Email.smtpInputHeloName = Email.smtpServerHeloName (smtpDeliveryHeloName smtpConfig),
            Email.smtpInputEnvelopeSender = sender,
            Email.smtpInputAuthentication =
              Just
                ( smtpAuthenticationForMode
                    mode
                    (Email.smtpLoginUsername (smtpDeliveryUsername smtpConfig))
                    (Email.smtpLoginPassword (smtpDeliveryPassword smtpConfig))
                )
          } of
        Just configuredSmtp -> Email.EmailDelivery (Email.deliverSmtpEmail configuredSmtp)
        Nothing -> unavailableEmailDelivery
    Nothing -> unavailableEmailDelivery
  where
    unavailableEmailDelivery = Email.EmailDelivery (\_ -> ioError (userError "SMTP delivery configuration is invalid"))

smtpAuthenticationForMode :: AppMode -> Email.SmtpUsername -> Email.SmtpPassword -> Email.SmtpAuthentication
smtpAuthenticationForMode mode =
  case mode of
    Production -> Email.smtpAuthentication
    Development -> Email.smtpAuthenticationForLocalDevelopment
    Test -> Email.smtpAuthenticationForLocalDevelopment

runtimeVerificationUrl :: Text.Text -> AppRequestContext -> HarchAccount.EmailVerificationToken -> Text.Text
runtimeVerificationUrl baseUrl requestContext verificationToken =
  trimTrailingSlash baseUrl
    <> renderRoutePath (HarchWeb.RouteRequest EmailVerificationRoute requestContext)
    <> "?token="
    <> HarchAccount.emailVerificationTokenText verificationToken

runtimePasswordWorkGate :: Password.PasswordWorkGate
runtimePasswordWorkGate = unsafePerformIO (Password.newPasswordWorkGate Password.defaultPasswordWorkBudget)
{-# NOINLINE runtimePasswordWorkGate #-}

trimTrailingSlash :: Text.Text -> Text.Text
trimTrailingSlash value =
  case Text.unsnoc value of
    Just (prefix, '/') -> prefix
    _ -> value

unavailableAccountWorkflow :: AccountWorkflow
unavailableAccountWorkflow =
  AccountWorkflow
    { accountWorkflowStore = unavailableAccountStore,
      accountWorkflowEmailDelivery = Email.EmailDelivery (\_ -> ioError (userError "email delivery is not configured")),
      accountWorkflowPasswordHasher = Password.hashPassword,
      accountWorkflowPasswordWorkGate = runtimePasswordWorkGate,
      accountWorkflowRegistrationDeliveryTimeout = defaultRegistrationDeliveryTimeout,
      accountWorkflowClock = pure (HarchWebTime.unixTimeNanoseconds 0),
      accountWorkflowMfaStore = unavailableMfaStore,
      accountWorkflowCredentialStore = unavailableAccountCredentialStore,
      accountWorkflowLoginAttemptStore = unavailableLoginAttemptStore,
      accountWorkflowSessionStore = unavailableAccountSessionStore,
      accountWorkflowMfaEnrollmentSessionStore = unavailableMfaEnrollmentSessionStore,
      accountWorkflowProfileStore = unavailableAccountProfileStore,
      accountWorkflowTotpEncryptionKey = totpEncryptionKey defaultAppEnvironmentConfig,
      accountWorkflowCsrfSigningKeyring = csrfSigningKeyring defaultAppEnvironmentConfig,
      accountWorkflowJwtIssuer = unavailableAccountJwtIssuer,
      accountWorkflowTotpClock = const 0,
      accountWorkflowVerificationUrl = \_ _ -> "https://invalid.example.test/verify"
    }

unavailableAccountStore :: AccountStore
unavailableAccountStore =
  AccountStore
    { createPendingAccount = \_ _ -> unavailableResult accountPersistenceUnavailable,
      completePendingRegistrationDelivery = const (unavailableResult accountPersistenceUnavailable),
      releasePendingRegistrationDelivery = const (unavailableResult accountPersistenceUnavailable),
      reserveVerificationResend = \_ _ _ -> unavailableResult accountPersistenceUnavailable,
      completeVerificationResend = \_ _ -> unavailableResult accountPersistenceUnavailable,
      releaseVerificationResend = const (unavailableResult accountPersistenceUnavailable),
      replaceEmailVerification = const (unavailableResult accountPersistenceUnavailable),
      findEmailVerification = const (unavailableResult accountPersistenceUnavailable),
      consumeEmailVerification = \_ _ -> unavailableResult accountPersistenceUnavailable
    }

unavailableMfaStore :: MfaStore
unavailableMfaStore =
  MfaStore
    { saveUnconfirmedTotpEnrollment = \_ _ _ -> unavailableResult mfaPersistenceUnavailable,
      loadTotpEnrollment = const (unavailableResult mfaPersistenceUnavailable),
      confirmTotpEnrollment = \_ _ _ -> unavailableResult mfaPersistenceUnavailable,
      loadUnusedRecoveryCodeHashes = const (unavailableResult mfaPersistenceUnavailable),
      consumeRecoveryCodeHash = \_ _ _ -> unavailableResult mfaPersistenceUnavailable,
      markTotpCodeUsed = \_ _ -> unavailableResult mfaPersistenceUnavailable
    }

unavailableAccountCredentialStore :: AccountCredentialStore
unavailableAccountCredentialStore =
  AccountCredentialStore
    { findAccountCredentialByEmail = const (unavailableResult accountCredentialsUnavailable),
      findAccountCredentialByUsername = const (unavailableResult accountCredentialsUnavailable),
      replacePasswordHashIfCurrent = \_ _ _ -> unavailableResult accountCredentialsUnavailable
    }

unavailableLoginAttemptStore :: LoginAttemptStore
unavailableLoginAttemptStore =
  LoginAttemptStore
    { reserveLoginAttempt = \_ _ -> unavailableResult loginAttemptsUnavailable,
      settleLoginAttempt = \_ _ -> unavailableResult loginAttemptsUnavailable,
      cancelLoginAttempt = \_ -> unavailableResult loginAttemptsUnavailable
    }

unavailableAccountSessionStore :: AccountSessionStore
unavailableAccountSessionStore =
  AccountSessionStore
    { saveAccountSession = const (unavailableResult AccountSessionStoreUnavailable),
      loadAccountSession = const (unavailableResult AccountSessionStoreUnavailable),
      invalidateAccountSession = const (const (unavailableResult AccountSessionStoreUnavailable))
    }

unavailableMfaEnrollmentSessionStore :: MfaEnrollmentSessionStore
unavailableMfaEnrollmentSessionStore =
  MfaEnrollmentSessionStore
    { saveMfaEnrollmentSession = const (unavailableResult MfaEnrollmentSessionStoreUnavailable),
      loadMfaEnrollmentSession = const (unavailableResult MfaEnrollmentSessionStoreUnavailable),
      invalidateMfaEnrollmentSession = const (const (unavailableResult MfaEnrollmentSessionStoreUnavailable))
    }

unavailableAccountProfileStore :: AccountProfileStore
unavailableAccountProfileStore =
  AccountProfileStore {findAccountProfile = const (unavailableResult accountProfilesUnavailable)}

accountPersistenceUnavailable :: AccountStoreError
accountPersistenceUnavailable = AccountStoreUnavailable "account persistence is not configured"

mfaPersistenceUnavailable :: MfaStoreError
mfaPersistenceUnavailable = MfaStoreUnavailable "MFA persistence is not configured"

accountCredentialsUnavailable :: AccountCredentialStoreError
accountCredentialsUnavailable = AccountCredentialStoreUnavailable "account credentials are not configured"

loginAttemptsUnavailable :: LoginAttemptStoreError
loginAttemptsUnavailable = LoginAttemptStoreUnavailable "login-attempt persistence is not configured"

accountProfilesUnavailable :: AccountStoreError
accountProfilesUnavailable = AccountStoreUnavailable "account profiles are not configured"

unavailableResult :: error -> IO (Either error value)
unavailableResult = pure . Left
