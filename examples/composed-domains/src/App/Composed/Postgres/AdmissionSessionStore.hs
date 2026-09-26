{-# LANGUAGE OverloadedStrings #-}

-- | PostgreSQL persistence for opaque composed-admission sessions.
module App.Composed.Postgres.AdmissionSessionStore
  ( buildPostgresAdmissionSessionStoreWithRunner,
  )
where

import App.Composed.Admission (AdmissionSessionStore (..), AdmissionSessionStoreError (..))
import App.Composed.Admission.Types
  ( AdmissionPrincipalId,
    admissionPrincipalIdText,
    mkAdmissionPrincipalId,
    unAdmissionSessionId,
  )
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Session (OpaqueSession (..), SessionId, sessionIdText)
import HarchWeb.Time (UnixTimeNanoseconds, unixTimeNanoseconds, unixTimeNanosecondsValue)
import Text.Read (readMaybe)

buildPostgresAdmissionSessionStoreWithRunner ::
  (source -> Text -> [Text] -> IO (Either Text [[Text]])) ->
  source ->
  AdmissionSessionStore
buildPostgresAdmissionSessionStoreWithRunner runQuery source =
  AdmissionSessionStore
    { saveAdmissionSession = \session ->
        decodeWrittenSession
          <$> runQuery
            source
            saveQuery
            [ sessionIdText (sessionId session),
              admissionPrincipalIdText (sessionPrincipal session),
              renderTime (sessionIssuedAtNanoseconds session),
              renderTime (sessionExpiresAtNanoseconds session)
            ],
      loadAdmissionSession = \admissionSessionId ->
        decodeLoadedSession (unAdmissionSessionId admissionSessionId)
          <$> runQuery source loadQuery [sessionIdText (unAdmissionSessionId admissionSessionId)],
      invalidateAdmissionSession = \admissionSessionId invalidatedAt ->
        decodeWrittenSession
          <$> runQuery source invalidateQuery [sessionIdText (unAdmissionSessionId admissionSessionId), renderTime invalidatedAt]
    }

decodeWrittenSession :: Either Text [[Text]] -> Either AdmissionSessionStoreError Bool
decodeWrittenSession result =
  case result of
    Left _ -> Left AdmissionSessionStoreUnavailable
    Right [] -> Right False
    Right [[_]] -> Right True
    Right _ -> Left AdmissionSessionStoreCorrupt

decodeLoadedSession :: SessionId -> Either Text [[Text]] -> Either AdmissionSessionStoreError (Maybe (OpaqueSession AdmissionPrincipalId))
decodeLoadedSession requestedSessionId result =
  case result of
    Left _ -> Left AdmissionSessionStoreUnavailable
    Right [] -> Right Nothing
    Right [[principalText, issuedText, expiresText]] -> do
      principalId <- maybe (Left AdmissionSessionStoreCorrupt) Right (mkAdmissionPrincipalId principalText)
      issuedAt <- parseTime issuedText
      expiresAt <- parseTime expiresText
      Right (Just (OpaqueSession requestedSessionId principalId issuedAt expiresAt))
    Right _ -> Left AdmissionSessionStoreCorrupt

parseTime :: Text -> Either AdmissionSessionStoreError UnixTimeNanoseconds
parseTime value =
  maybe (Left AdmissionSessionStoreCorrupt) (Right . unixTimeNanoseconds) (readMaybe (Text.unpack value))

renderTime :: UnixTimeNanoseconds -> Text
renderTime = Text.pack . show . unixTimeNanosecondsValue

saveQuery, loadQuery, invalidateQuery :: Text
saveQuery = "INSERT INTO composed.admission_sessions (session_id, admission_principal_id, issued_at_nanoseconds, expires_at_nanoseconds) VALUES ($1, $2, $3::BIGINT, $4::BIGINT) ON CONFLICT (session_id) DO NOTHING RETURNING session_id;"
loadQuery = "SELECT admission_principal_id, issued_at_nanoseconds::TEXT, expires_at_nanoseconds::TEXT FROM composed.admission_sessions WHERE session_id = $1 AND invalidated_at_nanoseconds IS NULL;"
invalidateQuery = "UPDATE composed.admission_sessions SET invalidated_at_nanoseconds = $2::BIGINT WHERE session_id = $1 AND invalidated_at_nanoseconds IS NULL RETURNING session_id;"
