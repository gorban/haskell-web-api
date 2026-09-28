{-# LANGUAGE OverloadedStrings #-}

-- | Operator-only migration and credential provisioning for the composed
-- example's admission and OAuth clients.
--
-- Decision record (secure login and admission, 2026-09-21): admission credentials are created by
-- this separate setup executable instead of by a web route, process runtime
-- configuration, or a seed list.  The operator supplies the TOTP secret on a
-- terminal with echo disabled; the command validates and encrypts its
-- canonical form before the parameterized PostgreSQL insert.  It never puts a
-- raw secret, login name, principal identifier, connection string, or
-- encrypted envelope in output or an error.  Keeping migrations and
-- provisioning as closed commands also makes runtime startup unable to create
-- or change admission identities.
--
-- Decision record (AHI-4E-OAUTH, 2026-09-28): the reference API client secret
-- is supplied through a separate hidden-terminal command, hashed under the
-- same default 64 MiB Argon2id policy as the unknown-client dummy check, and
-- written only as a parameterized hash. The durable client row has one active
-- hash, while allowed/default scopes are separate ordered rows; the setup
-- command inserts the example client and both scope rows in one SQL statement.
-- It never prints the plaintext secret or hash. Issued tokens appear only in
-- the intended success `access_token` field; secrets, hashes, and tokens stay
-- out of error responses, logs, and telemetry.
module App.Composed.AdmissionSetup
  ( AdmissionSetupCommand (..),
    AdmissionSetupError (..),
    encryptAdmissionTotpSecret,
    hashComposedExampleApiClientSecret,
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
import Data.ByteString qualified as ByteString
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Password (PasswordHash, defaultPasswordHashingPolicy, hashPassword, mkPassword)
import HarchWeb.Secret (SecretEncryptionKey, encryptSecret)
import HarchWeb.Totp (mkTotpSecret, renderTotpSecret)

-- | The closed set of database-mutating commands accepted by the setup
-- executable. Raw TOTP and OAuth client secrets are intentionally absent from
-- this value so they cannot be rendered from command-line arguments or
-- retained in shell history.
data AdmissionSetupCommand
  = MigrateAdmissionDatabase
  | ProvisionAdmissionCredential AdmissionPrincipalId AdmissionLoginName
  | SeedComposedExampleApiClient
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
  | AdmissionSetupApiClientSecretInputMustBeTerminal
  | AdmissionSetupApiClientSecretInvalid
  | AdmissionSetupApiClientSecretHashFailed
  | AdmissionSetupApiClientProvisionFailed
  deriving (Eq, Show)

parseAdmissionSetupCommand :: [String] -> Either AdmissionSetupError AdmissionSetupCommand
parseAdmissionSetupCommand arguments =
  case arguments of
    ["migrate"] -> Right MigrateAdmissionDatabase
    ["seed-example-api-client"] -> Right SeedComposedExampleApiClient
    ["provision", principalText, loginText] -> do
      principalId <- maybe (Left AdmissionSetupInvalidPrincipal) Right (mkAdmissionPrincipalId (Text.pack principalText))
      loginName <- maybe (Left AdmissionSetupInvalidLogin) Right (mkAdmissionLoginName (Text.pack loginText))
      Right (ProvisionAdmissionCredential principalId loginName)
    _ -> Left AdmissionSetupInvalidCommand

-- | Hash the one-time setup input with the same 64 MiB Argon2id policy used
-- by the unknown-client dummy check. The input has a fixed upper byte bound
-- so the stored client can always authenticate through the declared Basic
-- header ceiling.
hashComposedExampleApiClientSecret :: Text.Text -> IO (Either AdmissionSetupError PasswordHash)
hashComposedExampleApiClientSecret rawSecret
  | Text.null rawSecret || utf8Length rawSecret > 3000 = pure (Left AdmissionSetupApiClientSecretInvalid)
  | otherwise =
      fmap
        (maybe (Left AdmissionSetupApiClientSecretHashFailed) Right)
        (hashPassword defaultPasswordHashingPolicy (mkPassword rawSecret))

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
    AdmissionSetupInvalidCommand -> "Expected: migrate, provision <principal-id> <login-name>, or seed-example-api-client."
    AdmissionSetupInvalidPrincipal -> "The admission principal identifier is invalid."
    AdmissionSetupInvalidLogin -> "The admission login name is invalid."
    AdmissionSetupInvalidTotpSecret -> "The supplied TOTP secret is invalid."
    AdmissionSetupSecretInputMustBeTerminal -> "TOTP secret input requires an interactive terminal."
    AdmissionSetupConfigUnavailable -> "The composed admission deployment configuration is unavailable."
    AdmissionSetupMigrationFailed -> "Unable to apply composed admission database changes."
    AdmissionSetupCredentialProvisionFailed -> "Unable to provision the admission credential."
    AdmissionSetupApiClientSecretInputMustBeTerminal -> "OAuth client secret input requires an interactive terminal."
    AdmissionSetupApiClientSecretInvalid -> "The supplied OAuth client secret is invalid."
    AdmissionSetupApiClientSecretHashFailed -> "Unable to prepare the OAuth client credential."
    AdmissionSetupApiClientProvisionFailed -> "Unable to provision the OAuth client."

utf8Length :: Text.Text -> Int
utf8Length = ByteString.length . TextEncoding.encodeUtf8
