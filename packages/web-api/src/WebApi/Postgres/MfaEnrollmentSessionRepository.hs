{-# LANGUAGE OverloadedStrings #-}

module WebApi.Postgres.MfaEnrollmentSessionRepository
  ( buildRuntimePostgresMfaEnrollmentSessionStore,
    buildRuntimePostgresMfaEnrollmentSessionStoreWithRunner,
  )
where

import Control.Monad.Except (liftEither, runExceptT)
import Core.Control.Error (liftEitherWith)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Account (AccountId, accountIdText, mkAccountId)
import HarchWeb.Session
  ( OpaqueSession (..),
    SessionId,
    csrfTokenText,
    mkCsrfToken,
    sessionIdText,
  )
import HarchWeb.Time (unixTimeNanoseconds, unixTimeNanosecondsValue)
import Text.Read (readMaybe)
import WebApi.Postgres.Pool (PostgresPool)
import WebApi.Postgres.Runtime (runPooledParameterizedRowsQuery)
import WebApi.Session
  ( MfaEnrollmentSessionStore (..),
    MfaEnrollmentSessionStoreError (..),
  )

buildRuntimePostgresMfaEnrollmentSessionStore :: PostgresPool -> MfaEnrollmentSessionStore
buildRuntimePostgresMfaEnrollmentSessionStore !pool =
  buildRuntimePostgresMfaEnrollmentSessionStoreWithRunner runPooledParameterizedRowsQuery pool

buildRuntimePostgresMfaEnrollmentSessionStoreWithRunner ::
  (source -> Text -> [Text] -> IO (Either Text [[Text]])) ->
  source ->
  MfaEnrollmentSessionStore
buildRuntimePostgresMfaEnrollmentSessionStoreWithRunner runQuery source =
  MfaEnrollmentSessionStore
    { saveMfaEnrollmentSession = saveSession,
      loadMfaEnrollmentSession = loadSession,
      invalidateMfaEnrollmentSession = invalidateSession
    }
  where
    saveSession session =
      runSessionStoreQuery
        ( runQuery
            source
            saveMfaEnrollmentSessionQuery
            [ sessionIdText (sessionId session),
              accountIdText (sessionPrincipal session),
              csrfTokenText (sessionCsrfToken session),
              Text.pack (show (unixTimeNanosecondsValue (sessionIssuedAtNanoseconds session))),
              Text.pack (show (unixTimeNanosecondsValue (sessionExpiresAtNanoseconds session)))
            ]
        )
        (decodeMatchingSessionId (sessionIdText (sessionId session)))

    loadSession sessionToken =
      runSessionStoreQuery
        (runQuery source loadMfaEnrollmentSessionQuery [sessionIdText sessionToken])
        (decodeStoredSession sessionToken)

    invalidateSession sessionToken invalidatedAtNanoseconds =
      runSessionStoreQuery
        (runQuery source invalidateMfaEnrollmentSessionQuery [sessionIdText sessionToken, Text.pack (show (unixTimeNanosecondsValue invalidatedAtNanoseconds))])
        (decodeMatchingSessionId (sessionIdText sessionToken))

runSessionStoreQuery :: IO (Either Text [[Text]]) -> ([[Text]] -> Either MfaEnrollmentSessionStoreError value) -> IO (Either MfaEnrollmentSessionStoreError value)
runSessionStoreQuery query decodeRows =
  runExceptT $ do
    rows <- liftEitherWith (const MfaEnrollmentSessionStoreUnavailable) query
    liftEither (decodeRows rows)

decodeMatchingSessionId :: Text -> [[Text]] -> Either MfaEnrollmentSessionStoreError Bool
decodeMatchingSessionId expectedSessionId rows =
  case rows of
    [] -> Right False
    [[returnedSessionId]]
      | returnedSessionId == expectedSessionId -> Right True
    _ -> Left MfaEnrollmentSessionStoreCorruptData

decodeStoredSession :: SessionId -> [[Text]] -> Either MfaEnrollmentSessionStoreError (Maybe (OpaqueSession AccountId))
decodeStoredSession sessionToken rows =
  case rows of
    [] -> Right Nothing
    [[accountIdValue, csrfTokenValue, issuedAtValue, expiresAtValue]] ->
      case (mkAccountId accountIdValue, mkCsrfToken csrfTokenValue, readMaybe (Text.unpack issuedAtValue), readMaybe (Text.unpack expiresAtValue)) of
        (Just accountId, Just csrfToken, Just issuedAt, Just expiresAt) ->
          Right
            ( Just
                OpaqueSession
                  { sessionId = sessionToken,
                    sessionPrincipal = accountId,
                    sessionCsrfToken = csrfToken,
                    sessionIssuedAtNanoseconds = unixTimeNanoseconds issuedAt,
                    sessionExpiresAtNanoseconds = unixTimeNanoseconds expiresAt
                  }
            )
        _ -> Left MfaEnrollmentSessionStoreCorruptData
    _ -> Left MfaEnrollmentSessionStoreCorruptData

saveMfaEnrollmentSessionQuery, loadMfaEnrollmentSessionQuery, invalidateMfaEnrollmentSessionQuery :: Text
saveMfaEnrollmentSessionQuery = "INSERT INTO web_api.mfa_enrollment_sessions (session_id, account_id, csrf_token, issued_at_nanoseconds, expires_at_nanoseconds) VALUES ($1, $2, $3, $4, $5) ON CONFLICT (session_id) DO NOTHING RETURNING session_id;"
loadMfaEnrollmentSessionQuery = "SELECT account_id, csrf_token, issued_at_nanoseconds::TEXT, expires_at_nanoseconds::TEXT FROM web_api.mfa_enrollment_sessions WHERE session_id = $1 AND invalidated_at_nanoseconds IS NULL;"
invalidateMfaEnrollmentSessionQuery = "UPDATE web_api.mfa_enrollment_sessions SET invalidated_at_nanoseconds = $2 WHERE session_id = $1 AND invalidated_at_nanoseconds IS NULL RETURNING session_id;"
