module WebApi.AppEffect
  ( AccountWorkflow (..),
    AppFailure (..),
    AppM,
    AppServices (..),
    FailureDiagnostics (..),
    askAppServices,
    liftAppIO,
    runAppM,
    throwAppFailure,
  )
where

import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask), ReaderT, runReaderT)
import Data.Text (Text)
import Data.Word (Word64)
import HarchWeb.Account (EmailVerificationToken)
import HarchWeb.Email qualified as Email
import HarchWeb.Password qualified as Password
import HarchWeb.Secret (SecretEncryptionKey)
import WebApi.Account (AccountStore)
import WebApi.Login (AccountCredentialStore)
import WebApi.Mfa (MfaStore)
import WebApi.Route (AppRequestContext)
import WebApi.Session (AccountSessionStore)

data AccountWorkflow = AccountWorkflow
  { accountWorkflowStore :: AccountStore,
    accountWorkflowEmailDelivery :: Email.EmailDelivery,
    accountWorkflowPasswordHasher :: Password.PasswordHashingPolicy -> Password.Password -> IO (Maybe Password.PasswordHash),
    accountWorkflowClock :: IO Word64,
    accountWorkflowMfaStore :: MfaStore,
    accountWorkflowCredentialStore :: AccountCredentialStore,
    accountWorkflowSessionStore :: AccountSessionStore,
    accountWorkflowTotpEncryptionKey :: SecretEncryptionKey,
    accountWorkflowTotpClock :: IO Word64,
    accountWorkflowVerificationUrl :: AppRequestContext -> EmailVerificationToken -> Text
  }

newtype AppServices = AppServices
  { appAccountWorkflow :: AccountWorkflow
  }

data FailureDiagnostics = FailureDiagnostics
  { failureCode :: Text,
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

liftAppIO :: IO value -> AppM publicFailure value
liftAppIO = liftIO

throwAppFailure :: AppFailure publicFailure -> AppM publicFailure value
throwAppFailure = throwError

runAppM :: AppServices -> AppM publicFailure value -> IO (Either (AppFailure publicFailure) value)
runAppM services action = runExceptT (runReaderT action services)
