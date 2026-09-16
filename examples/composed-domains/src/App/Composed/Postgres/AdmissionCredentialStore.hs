{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | PostgreSQL adapter for operator-provisioned admission credentials.
module App.Composed.Postgres.AdmissionCredentialStore
  ( buildPostgresAdmissionCredentialStoreWithRunner,
  )
where

import App.Composed.Admission
  ( AdmissionCredentialStore (..),
    AdmissionCredentialStoreError (..),
    StoredAdmissionCredential (..),
  )
import App.Composed.Admission.Types
  ( admissionLoginNameText,
    admissionPrincipalIdText,
    mkAdmissionPrincipalId,
    mkEncryptedAdmissionTotpSecret,
  )
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import Text.Read (readMaybe)

buildPostgresAdmissionCredentialStoreWithRunner ::
  (source -> Text -> [Text] -> IO (Either Text [[Text]])) -> source -> AdmissionCredentialStore
buildPostgresAdmissionCredentialStoreWithRunner runQuery source =
  AdmissionCredentialStore
    { findAdmissionCredential = \loginName -> decodeCredential <$> runQuery source findQuery [admissionLoginNameText loginName],
      markAdmissionTotpCounterUsed = \principalId counter -> decodeWritten <$> runQuery source markUsedQuery [admissionPrincipalIdText principalId, Text.pack (show counter)]
    }

decodeCredential :: Either Text [[Text]] -> Either AdmissionCredentialStoreError (Maybe StoredAdmissionCredential)
decodeCredential result =
  case result of
    Left _ -> Left AdmissionCredentialStoreUnavailable
    Right [] -> Right Nothing
    Right [[principalText, encryptedSecretText, counterText]] -> do
      principalId <- maybe (Left AdmissionCredentialStoreCorrupt) Right (mkAdmissionPrincipalId principalText)
      encryptedSecret <- maybe (Left AdmissionCredentialStoreCorrupt) Right (mkEncryptedAdmissionTotpSecret encryptedSecretText)
      counter <- if Text.null counterText then Right Nothing else maybe (Left AdmissionCredentialStoreCorrupt) (Right . Just) (readMaybe (Text.unpack counterText) :: Maybe Word64)
      Right (Just (StoredAdmissionCredential principalId encryptedSecret counter))
    Right _ -> Left AdmissionCredentialStoreCorrupt

decodeWritten :: Either Text [[Text]] -> Either AdmissionCredentialStoreError Bool
decodeWritten = either (const (Left AdmissionCredentialStoreUnavailable)) $ \case
  [] -> Right False
  [[_]] -> Right True
  _ -> Left AdmissionCredentialStoreCorrupt

findQuery, markUsedQuery :: Text
findQuery = "SELECT admission_principal_id, encrypted_totp_secret, COALESCE(last_used_totp_counter::TEXT, '') FROM composed.admission_credentials WHERE admission_login_name = $1;"
markUsedQuery = "UPDATE composed.admission_credentials SET last_used_totp_counter = $2::BIGINT WHERE admission_principal_id = $1 AND (last_used_totp_counter IS NULL OR last_used_totp_counter < $2::BIGINT) RETURNING admission_principal_id;"
