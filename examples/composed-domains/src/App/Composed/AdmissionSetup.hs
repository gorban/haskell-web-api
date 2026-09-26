{-# LANGUAGE OverloadedStrings #-}

-- | Operator-only migration and credential provisioning for composed admission.
--
-- Decision record (AHI-4C, 2026-09-21): admission credentials are created by
-- this separate setup executable instead of by a web route, process runtime
-- configuration, or a seed list.  The operator supplies the TOTP secret on a
-- terminal with echo disabled; the command validates and encrypts its
-- canonical form before the parameterized PostgreSQL insert.  It never puts a
-- raw secret, login name, principal identifier, connection string, or
-- encrypted envelope in output or an error.  Keeping migrations and
-- provisioning as closed commands also makes runtime startup unable to create
-- or change admission identities.
module App.Composed.AdmissionSetup
  ( AdmissionSetupCommand (..),
    AdmissionSetupError (..),
    encryptAdmissionTotpSecret,
    parseAdmissionSetupCommand,
    renderAdmissionSetupError,
  )
where

import App.Composed.Admission.Types
  ( AdmissionLoginName,
    AdmissionPrincipalId,
    EncryptedAdmissionTotpSecret,
    mkAdmissionLoginName,
    mkAdmissionPrincipalId,
    mkEncryptedAdmissionTotpSecret,
  )
import Crypto.Error (maybeCryptoError)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Secret (SecretEncryptionKey, encryptSecret)
import HarchWeb.Totp (mkTotpSecret, renderTotpSecret)

-- | The only database-mutating commands accepted by the setup executable.
-- Raw TOTP material is intentionally absent from this value so it can never
-- be rendered from command-line arguments or retained in shell history.
data AdmissionSetupCommand
  = MigrateAdmissionDatabase
  | ProvisionAdmissionCredential AdmissionPrincipalId AdmissionLoginName
  deriving (Eq, Show)

-- | Safe public failures for an operator invocation.  Each constructor is
-- deliberately free of untrusted text and deployed secret material.
data AdmissionSetupError
  = AdmissionSetupInvalidCommand
  | AdmissionSetupInvalidPrincipal
  | AdmissionSetupInvalidLogin
  | AdmissionSetupInvalidTotpSecret
  | AdmissionSetupSecretInputMustBeTerminal
  | AdmissionSetupConfigUnavailable
  | AdmissionSetupMigrationFailed
  | AdmissionSetupCredentialProvisionFailed
  deriving (Eq, Show)

parseAdmissionSetupCommand :: [String] -> Either AdmissionSetupError AdmissionSetupCommand
parseAdmissionSetupCommand arguments =
  case arguments of
    ["migrate"] -> Right MigrateAdmissionDatabase
    ["provision", principalText, loginText] -> do
      principalId <- maybe (Left AdmissionSetupInvalidPrincipal) Right (mkAdmissionPrincipalId (Text.pack principalText))
      loginName <- maybe (Left AdmissionSetupInvalidLogin) Right (mkAdmissionLoginName (Text.pack loginText))
      Right (ProvisionAdmissionCredential principalId loginName)
    _ -> Left AdmissionSetupInvalidCommand

-- | Validate and encrypt a canonical TOTP secret before it can reach the
-- credential adapter.  The result remains opaque; no caller needs the raw
-- value after this boundary.
encryptAdmissionTotpSecret :: SecretEncryptionKey -> Text.Text -> IO (Either AdmissionSetupError EncryptedAdmissionTotpSecret)
encryptAdmissionTotpSecret encryptionKey rawSecret =
  case mkTotpSecret rawSecret of
    Nothing -> pure (Left AdmissionSetupInvalidTotpSecret)
    Just secret -> do
      maybeEnvelope <- maybeCryptoError <$> encryptSecret encryptionKey (TextEncoding.encodeUtf8 (renderTotpSecret secret))
      pure $
        maybe
          (Left AdmissionSetupCredentialProvisionFailed)
          (maybe (Left AdmissionSetupCredentialProvisionFailed) Right . mkEncryptedAdmissionTotpSecret)
          maybeEnvelope

renderAdmissionSetupError :: AdmissionSetupError -> String
renderAdmissionSetupError setupError =
  case setupError of
    AdmissionSetupInvalidCommand -> "Expected: migrate or provision <principal-id> <login-name>."
    AdmissionSetupInvalidPrincipal -> "The admission principal identifier is invalid."
    AdmissionSetupInvalidLogin -> "The admission login name is invalid."
    AdmissionSetupInvalidTotpSecret -> "The supplied TOTP secret is invalid."
    AdmissionSetupSecretInputMustBeTerminal -> "TOTP secret input requires an interactive terminal."
    AdmissionSetupConfigUnavailable -> "The composed admission deployment configuration is unavailable."
    AdmissionSetupMigrationFailed -> "Unable to apply composed admission database changes."
    AdmissionSetupCredentialProvisionFailed -> "Unable to provision the admission credential."
