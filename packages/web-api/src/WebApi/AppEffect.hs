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
import Data.Word (Word64)
import HarchWeb.Account (EmailVerificationToken)
import HarchWeb.Email qualified as Email
import HarchWeb.Password qualified as Password
import HarchWeb.Secret (SecretEncryptionKey)
import WebApi.Account (AccountProfileStore, AccountStore)
import WebApi.Login (AccountCredentialStore)
import WebApi.Mfa (MfaStore)
import WebApi.Route (AppRequestContext)
import WebApi.Session (AccountSessionStore, MfaEnrollmentSessionStore)

data AccountWorkflow = AccountWorkflow
  { accountWorkflowStore :: AccountStore,
    accountWorkflowEmailDelivery :: Email.EmailDelivery,
    accountWorkflowPasswordHasher :: Password.PasswordHashingPolicy -> Password.Password -> IO (Maybe Password.PasswordHash),
    accountWorkflowClock :: IO Word64,
    accountWorkflowMfaStore :: MfaStore,
    accountWorkflowCredentialStore :: AccountCredentialStore,
    accountWorkflowSessionStore :: AccountSessionStore,
    accountWorkflowMfaEnrollmentSessionStore :: MfaEnrollmentSessionStore,
    accountWorkflowProfileStore :: AccountProfileStore,
    accountWorkflowTotpEncryptionKey :: SecretEncryptionKey,
    accountWorkflowTotpClock :: IO Word64,
    accountWorkflowVerificationUrl :: AppRequestContext -> EmailVerificationToken -> Text
  }

newtype AppServices = AppServices
  { appAccountWorkflow :: AccountWorkflow
  }

-- | Stable low-cardinality application failure codes emitted to telemetry.
data FailureCode
  = RegistrationDeliveryFailure
  | RegistrationStoreFailure
  | RegistrationPasswordHashFailure
  | RegistrationClockFailure
  | VerificationStoreFailure
  | MfaEnrollmentSessionFailure
  | MfaEnrollmentStartFailure
  | MfaEnrollmentConfirmFailure
  | LoginCredentialStoreFailure
  | LoginMfaStoreFailure
  | LoginCorruptEnrollmentFailure
  | LoginSessionFailure
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
    RegistrationStoreFailure -> "account.registration.store"
    RegistrationPasswordHashFailure -> "account.registration.password-hash"
    RegistrationClockFailure -> "account.registration.clock"
    VerificationStoreFailure -> "account.verification.store"
    MfaEnrollmentSessionFailure -> "account.mfa.enrollment-session"
    MfaEnrollmentStartFailure -> "account.mfa.start"
    MfaEnrollmentConfirmFailure -> "account.mfa.confirm"
    LoginCredentialStoreFailure -> "account.login.credential-store"
    LoginMfaStoreFailure -> "account.login.mfa-store"
    LoginCorruptEnrollmentFailure -> "account.login.corrupt-enrollment"
    LoginSessionFailure -> "account.login.session"
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
