{-# LANGUAGE OverloadedStrings #-}

-- | Opaque application-owned identifiers for the composed admission domain.
-- They intentionally do not reuse an account ID: admission and account
-- identity have independent provisioning, session tables, cookies, and
-- revocation lifecycles.
module App.Composed.Admission.Types
  ( AdmissionPrincipal,
    AdmissionLoginName,
    EncryptedAdmissionTotpSecret,
    AdmissionPrincipalId,
    AdmissionSessionId,
    admissionPrincipalId,
    admissionPrincipalIdText,
    admissionPrincipalSessionExpiresAt,
    admissionPrincipalSessionId,
    mkAdmissionPrincipal,
    mkAdmissionLoginName,
    mkEncryptedAdmissionTotpSecret,
    mkAdmissionPrincipalId,
    mkAdmissionSessionId,
    unAdmissionSessionId,
    admissionLoginNameText,
    encryptedAdmissionTotpSecretText,
  )
where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Session (SessionId)
import HarchWeb.Time (UnixTimeNanoseconds)

newtype AdmissionPrincipalId = AdmissionPrincipalId Text
  deriving (Eq, Ord)

instance Show AdmissionPrincipalId where
  show _ = "AdmissionPrincipalId <redacted>"

-- | A bounded operator-provisioned admission login name. It is distinct from
-- account usernames and does not enter a route, telemetry, or public error.
newtype AdmissionLoginName = AdmissionLoginName Text
  deriving (Eq, Ord)

instance Show AdmissionLoginName where
  show _ = "AdmissionLoginName <redacted>"

mkAdmissionLoginName :: Text -> Maybe AdmissionLoginName
mkAdmissionLoginName value
  | Text.length value < 1 || Text.length value > 128 = Nothing
  | Text.all validIdentifierCharacter value = Just (AdmissionLoginName value)
  | otherwise = Nothing

admissionLoginNameText :: AdmissionLoginName -> Text
admissionLoginNameText (AdmissionLoginName value) = value

-- | AES-GCM envelope bytes rendered by Harch's secret capability. The store
-- never receives a TOTP code or plaintext secret.
newtype EncryptedAdmissionTotpSecret = EncryptedAdmissionTotpSecret Text
  deriving (Eq)

instance Show EncryptedAdmissionTotpSecret where
  show _ = "EncryptedAdmissionTotpSecret <redacted>"

mkEncryptedAdmissionTotpSecret :: Text -> Maybe EncryptedAdmissionTotpSecret
mkEncryptedAdmissionTotpSecret value
  | Text.length value < 1 || Text.length value > 4096 = Nothing
  | otherwise = Just (EncryptedAdmissionTotpSecret value)

encryptedAdmissionTotpSecretText :: EncryptedAdmissionTotpSecret -> Text
encryptedAdmissionTotpSecretText (EncryptedAdmissionTotpSecret value) = value

newtype AdmissionSessionId = AdmissionSessionId SessionId
  deriving (Eq)

instance Show AdmissionSessionId where
  show _ = "AdmissionSessionId <redacted>"

-- | The context value established by the pre-auth guard only after it has
-- loaded an active durable admission session.  It carries no admission name,
-- TOTP secret/code, cookie text, or account authority.
data AdmissionPrincipal = AdmissionPrincipal
  { admissionPrincipalId :: AdmissionPrincipalId,
    admissionPrincipalSessionId :: AdmissionSessionId,
    admissionPrincipalSessionExpiresAt :: UnixTimeNanoseconds
  }
  deriving (Eq)

instance Show AdmissionPrincipal where
  show _ = "AdmissionPrincipal <redacted>"

mkAdmissionPrincipalId :: Text -> Maybe AdmissionPrincipalId
mkAdmissionPrincipalId value
  | Text.length value < 1 || Text.length value > 128 = Nothing
  | Text.all validIdentifierCharacter value = Just (AdmissionPrincipalId value)
  | otherwise = Nothing

validIdentifierCharacter :: Char -> Bool
validIdentifierCharacter character =
  character == '-'
    || character == '_'
    || isAsciiLower character
    || isAsciiUpper character
    || isDigit character

admissionPrincipalIdText :: AdmissionPrincipalId -> Text
admissionPrincipalIdText (AdmissionPrincipalId value) = value

mkAdmissionSessionId :: SessionId -> AdmissionSessionId
mkAdmissionSessionId = AdmissionSessionId

unAdmissionSessionId :: AdmissionSessionId -> SessionId
unAdmissionSessionId (AdmissionSessionId value) = value

mkAdmissionPrincipal :: AdmissionPrincipalId -> AdmissionSessionId -> UnixTimeNanoseconds -> AdmissionPrincipal
mkAdmissionPrincipal = AdmissionPrincipal
