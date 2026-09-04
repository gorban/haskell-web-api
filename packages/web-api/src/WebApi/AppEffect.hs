{-# LANGUAGE OverloadedStrings #-}

module WebApi.AppEffect
  ( AccountWorkflow (..),
    AppFailure (..),
    AppM,
    AppServices (..),
    FailureCode (..),
    FailureDiagnostics (..),
    askAppServices,
    renderFailureCode,
    runAppM,
    throwAppFailure,
  )
where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.Reader (MonadReader (ask), ReaderT, runReaderT)
import Data.Text (Text)
import HarchWeb.Account (EmailVerificationToken)
import HarchWeb.Csrf (SignedCsrfKeyring)
import HarchWeb.Email qualified as Email
import HarchWeb.Password qualified as Password
import HarchWeb.Secret (SecretEncryptionKey)
import HarchWeb.Time (UnixTimeNanoseconds, UnixTimeSeconds)
import WebApi.Account (AccountProfileStore, AccountStore, RegistrationDeliveryTimeout)
import WebApi.AccountJwt (AccountJwtIssuer)
import WebApi.Login (AccountCredentialStore, LoginAttemptStore)
import WebApi.Mfa (MfaStore)
import WebApi.Route (AppRequestContext)
import WebApi.Session (AccountSessionStore, MfaEnrollmentSessionStore)

data AccountWorkflow = AccountWorkflow
  { accountWorkflowStore :: AccountStore,
    accountWorkflowEmailDelivery :: Email.EmailDelivery,
    accountWorkflowPasswordHasher :: Password.PasswordHashingPolicy -> Password.Password -> IO (Maybe Password.PasswordHash),
    accountWorkflowPasswordWorkGate :: Password.PasswordWorkGate,
    -- | One bounded SMTP deadline shared by registration and authenticated
    -- verification resends.  Keeping it here makes the operational timeout
    -- injectable in workflow tests while the application selects the safe
    -- reference default at startup.
    accountWorkflowRegistrationDeliveryTimeout :: RegistrationDeliveryTimeout,
    accountWorkflowClock :: IO UnixTimeNanoseconds,
    accountWorkflowMfaStore :: MfaStore,
    accountWorkflowCredentialStore :: AccountCredentialStore,
    accountWorkflowLoginAttemptStore :: LoginAttemptStore,
    accountWorkflowSessionStore :: AccountSessionStore,
    accountWorkflowMfaEnrollmentSessionStore :: MfaEnrollmentSessionStore,
    accountWorkflowProfileStore :: AccountProfileStore,
    accountWorkflowTotpEncryptionKey :: SecretEncryptionKey,
    -- | Immutable process-lifetime key ring for the storage-neutral signed
    -- CSRF backend.  Rotation is a reviewed configuration/deployment change;
    -- session stores remain the application-owned source of grant bindings.
    accountWorkflowCsrfSigningKeyring :: SignedCsrfKeyring,
    -- | The only credential-issuance capability available to login. The
    -- workflow receives neither JWK files nor verification keys, so it cannot
    -- accidentally grow a second authentication implementation.
    accountWorkflowJwtIssuer :: AccountJwtIssuer,
    -- | Derives the RFC TOTP Unix-second counter from the same durable
    -- instant read for this account operation.  This is pure deliberately:
    -- a second clock read could cross an epoch boundary independently.
    accountWorkflowTotpClock :: UnixTimeNanoseconds -> UnixTimeSeconds,
    accountWorkflowVerificationUrl :: AppRequestContext -> EmailVerificationToken -> Text
  }

newtype AppServices = AppServices
  { appAccountWorkflow :: AccountWorkflow
  }

-- | Stable low-cardinality application failure codes emitted to telemetry.
data FailureCode
  = RegistrationDeliveryFailure
  | RegistrationDeliveryTimeoutFailure
  | RegistrationStoreFailure
  | RegistrationPasswordHashFailure
  | RegistrationPasswordWorkBudgetFailure
  | RegistrationStorageCapacityFailure
  | RegistrationDeliveryClaimFailure
  | RegistrationClockFailure
  | VerificationStoreFailure
  | MfaEnrollmentSessionFailure
  | MfaEnrollmentStartFailure
  | MfaEnrollmentConfirmFailure
  | LoginCredentialStoreFailure
  | LoginMfaStoreFailure
  | LoginAttemptStoreFailure
  | LoginPasswordWorkBudgetFailure
  | LoginCorruptEnrollmentFailure
  | LoginSessionFailure
  | LoginJwtIssueFailure
  | LogoutSessionFailure
  | ProfileLoadFailure
  | ProfileResendDeliveryFailure
  | ProfileResendStoreFailure
  | ProfileResendClockFailure
  deriving (Eq, Show)

renderFailureCode :: FailureCode -> Text
renderFailureCode failureCodeValue =
  case failureCodeValue of
    RegistrationDeliveryFailure -> "account.registration.delivery"
    RegistrationDeliveryTimeoutFailure -> "account.registration.delivery-timeout"
    RegistrationStoreFailure -> "account.registration.store"
    RegistrationPasswordHashFailure -> "account.registration.password-hash"
    RegistrationPasswordWorkBudgetFailure -> "account.registration.password-work-budget"
    RegistrationStorageCapacityFailure -> "account.registration.storage-capacity"
    RegistrationDeliveryClaimFailure -> "account.registration.delivery-claim"
    RegistrationClockFailure -> "account.registration.clock"
    VerificationStoreFailure -> "account.verification.store"
    MfaEnrollmentSessionFailure -> "account.mfa.enrollment-session"
    MfaEnrollmentStartFailure -> "account.mfa.start"
    MfaEnrollmentConfirmFailure -> "account.mfa.confirm"
    LoginCredentialStoreFailure -> "account.login.credential-store"
    LoginMfaStoreFailure -> "account.login.mfa-store"
    LoginAttemptStoreFailure -> "account.login.attempt-store"
    LoginPasswordWorkBudgetFailure -> "account.login.password-work-budget"
    LoginCorruptEnrollmentFailure -> "account.login.corrupt-enrollment"
    LoginSessionFailure -> "account.login.session"
    LoginJwtIssueFailure -> "account.login.jwt-issue"
    LogoutSessionFailure -> "account.logout.session"
    ProfileLoadFailure -> "account.profile.load"
    ProfileResendDeliveryFailure -> "account.profile.resend.delivery"
    ProfileResendStoreFailure -> "account.profile.resend.store"
    ProfileResendClockFailure -> "account.profile.resend.clock"

data FailureDiagnostics = FailureDiagnostics
  { failureCode :: FailureCode,
    failureType :: Text,
    failureLogEntries :: [Text]
  }

data AppFailure publicFailure = AppFailure
  { appFailurePublic :: publicFailure,
    appFailureDiagnostics :: FailureDiagnostics
  }

type AppM publicFailure = ReaderT AppServices (ExceptT (AppFailure publicFailure) IO)

askAppServices :: AppM publicFailure AppServices
askAppServices = ask

throwAppFailure :: AppFailure publicFailure -> AppM publicFailure value
throwAppFailure = throwError

runAppM :: AppServices -> AppM publicFailure value -> IO (Either (AppFailure publicFailure) value)
runAppM services action = runExceptT (runReaderT action services)
