{-# LANGUAGE OverloadedStrings #-}

module WebApi.Postgres.SessionRepository
  ( buildRuntimePostgresAccountSessionStore,
    buildRuntimePostgresAccountSessionStoreWithRunner,
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
import Text.Read (readMaybe)
import WebApi.Postgres.Pool (PostgresPool)
import WebApi.Postgres.Runtime (runPooledParameterizedRowsQuery)
import WebApi.Session
  ( AccountSessionStore (..),
    AccountSessionStoreError (..),
  )

buildRuntimePostgresAccountSessionStore :: PostgresPool -> AccountSessionStore
buildRuntimePostgresAccountSessionStore !pool =
  buildRuntimePostgresAccountSessionStoreWithRunner runPooledParameterizedRowsQuery pool

buildRuntimePostgresAccountSessionStoreWithRunner ::
  (source -> Text -> [Text] -> IO (Either Text [[Text]])) ->
  source ->
  AccountSessionStore
buildRuntimePostgresAccountSessionStoreWithRunner runQuery source =
  AccountSessionStore
    { saveAccountSession = saveSession,
      loadAccountSession = loadSession,
      invalidateAccountSession = invalidateSession
    }
  where
    saveSession session =
      runSessionStoreQuery
        ( runQuery
            source
            saveAccountSessionQuery
            [ sessionIdText (sessionId session),
              accountIdText (sessionPrincipal session),
              csrfTokenText (sessionCsrfToken session),
              Text.pack (show (sessionIssuedAtNanoseconds session)),
              Text.pack (show (sessionExpiresAtNanoseconds session))
            ]
        )
        (decodeMatchingSessionId (sessionIdText (sessionId session)))

    loadSession sessionToken =
      runSessionStoreQuery
        (runQuery source loadAccountSessionQuery [sessionIdText sessionToken])
        (decodeStoredSession sessionToken)

    invalidateSession sessionToken invalidatedAtNanoseconds =
      runSessionStoreQuery
        (runQuery source invalidateAccountSessionQuery [sessionIdText sessionToken, Text.pack (show invalidatedAtNanoseconds)])
        (decodeMatchingSessionId (sessionIdText sessionToken))

runSessionStoreQuery :: IO (Either Text [[Text]]) -> ([[Text]] -> Either AccountSessionStoreError value) -> IO (Either AccountSessionStoreError value)
runSessionStoreQuery query decodeRows =
  runExceptT $ do
    rows <- liftEitherWith (const AccountSessionStoreUnavailable) query
    liftEither (decodeRows rows)

decodeMatchingSessionId :: Text -> [[Text]] -> Either AccountSessionStoreError Bool
decodeMatchingSessionId expectedSessionId rows =
  case rows of
    [] -> Right False
    [[returnedSessionId]]
      | returnedSessionId == expectedSessionId -> Right True
    _ -> Left AccountSessionStoreCorruptData

decodeStoredSession :: SessionId -> [[Text]] -> Either AccountSessionStoreError (Maybe (OpaqueSession AccountId))
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
                    sessionIssuedAtNanoseconds = issuedAt,
                    sessionExpiresAtNanoseconds = expiresAt
                  }
            )
        _ -> Left AccountSessionStoreCorruptData
    _ -> Left AccountSessionStoreCorruptData

saveAccountSessionQuery, loadAccountSessionQuery, invalidateAccountSessionQuery :: Text
saveAccountSessionQuery = "INSERT INTO web_api.account_sessions (session_id, account_id, csrf_token, issued_at_nanoseconds, expires_at_nanoseconds) VALUES ($1, $2, $3, $4, $5) ON CONFLICT (session_id) DO NOTHING RETURNING session_id;"
loadAccountSessionQuery = "SELECT account_id, csrf_token, issued_at_nanoseconds::TEXT, expires_at_nanoseconds::TEXT FROM web_api.account_sessions WHERE session_id = $1 AND invalidated_at_nanoseconds IS NULL;"
invalidateAccountSessionQuery = "UPDATE web_api.account_sessions SET invalidated_at_nanoseconds = $2 WHERE session_id = $1 AND invalidated_at_nanoseconds IS NULL RETURNING session_id;"
