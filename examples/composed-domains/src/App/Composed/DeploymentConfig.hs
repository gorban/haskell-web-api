{-# LANGUAGE OverloadedStrings #-}

-- | Deployment-only inputs for the durable composed-admission assembly.
--
-- The database connection and TOTP encryption material are supplied through
-- the deployment's layered configuration.  Their values never appear in this
-- module's errors or 'Show' output.  Operator credentials are deliberately
-- absent: provisioning receives an already encrypted envelope through the
-- separate setup boundary, rather than a plaintext account list in runtime
-- configuration.
module App.Composed.DeploymentConfig
  ( ComposedDeploymentConfig,
    composedDeploymentAdmissionEncryptionKey,
    composedDeploymentDatabase,
    parseComposedDeploymentConfig,
  )
where

import App.Composed.Postgres (ComposedDatabaseConnectionString (..))
import Core.Config (ConfigLayers (..), ConfigParseError (..), lookupConfigValue)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Secret (SecretEncryptionKey, mkSecretEncryptionKey)

data ComposedDeploymentConfig = ComposedDeploymentConfig
  { composedDeploymentDatabase :: ComposedDatabaseConnectionString,
    composedDeploymentAdmissionEncryptionKey :: SecretEncryptionKey
  }

instance Show ComposedDeploymentConfig where
  show _ = "ComposedDeploymentConfig <redacted>"

parseComposedDeploymentConfig :: [(Text, Text)] -> [(Text, Text)] -> [(Text, Text)] -> Either ConfigParseError ComposedDeploymentConfig
parseComposedDeploymentConfig committedDefaults localOverrides environmentOverrides = do
  connectionString <- requiredNonEmpty "COMPOSED_DATABASE_CONNECTION_STRING"
  encryptionKey <- requiredAdmissionEncryptionKey
  pure
    ComposedDeploymentConfig
      { composedDeploymentDatabase = ComposedDatabaseConnectionString (TextEncoding.encodeUtf8 connectionString),
        composedDeploymentAdmissionEncryptionKey = encryptionKey
      }
  where
    layers = ConfigLayers committedDefaults localOverrides environmentOverrides
    requiredNonEmpty key =
      case lookupConfigValue key layers of
        Nothing -> Left (MissingConfigValue key)
        Just value
          | Text.null value || Text.any (== '\NUL') value -> Left (InvalidConfigValue key "<redacted>")
          | otherwise -> Right value
    requiredAdmissionEncryptionKey = do
      value <- requiredNonEmpty "COMPOSED_ADMISSION_TOTP_ENCRYPTION_KEY"
      maybe
        (Left (InvalidConfigValue "COMPOSED_ADMISSION_TOTP_ENCRYPTION_KEY" "<redacted>"))
        Right
        (mkSecretEncryptionKey value)
