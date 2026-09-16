{-# LANGUAGE OverloadedStrings #-}

-- | PostgreSQL adapter for the one application-owned atomic session/audit
-- operation.  It invokes one controlled database function; it never composes
-- independently committed session and audit queries in Haskell.
module WebApi.Postgres.AccountSessionAuditRepository
  ( buildRuntimePostgresAccountSessionAuditStore,
    buildRuntimePostgresAccountSessionAuditStoreWithRunner,
  )
where

import Control.Monad.Except (liftEither, runExceptT)
import Core.Control.Error (liftEitherWith)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Account (AccountId, accountIdText)
import HarchWeb.RequestId (requestIdText)
import HarchWeb.Session (OpaqueSession (..), sessionIdText)
import HarchWeb.Time (unixTimeNanosecondsValue)
import WebApi.AccountSessionAudit
  ( AccountSessionAuditStore (..),
    AccountSessionAuditStoreError (..),
  )
import WebApi.ActivityAudit
  ( AccountActivity (..),
    AccountAuditPayload (..),
    AuditRouteObservation,
    accountAuditEventPayload,
    auditRouteEndpointName,
    auditRouteLocale,
    auditRouteMountChain,
    auditRouteTemplate,
  )
import WebApi.Postgres.Pool (PostgresPool)
import WebApi.Postgres.Runtime (runPooledNullableParameterizedRowsQuery)

buildRuntimePostgresAccountSessionAuditStore :: PostgresPool -> AccountSessionAuditStore
buildRuntimePostgresAccountSessionAuditStore =
  buildRuntimePostgresAccountSessionAuditStoreWithRunner runPooledNullableParameterizedRowsQuery

buildRuntimePostgresAccountSessionAuditStoreWithRunner ::
  (source -> Text -> [Maybe Text] -> IO (Either Text [[Text]])) ->
  source ->
  AccountSessionAuditStore
buildRuntimePostgresAccountSessionAuditStoreWithRunner runQuery source =
  AccountSessionAuditStore $ \session activity ->
    runExceptT $ do
      rows <-
        liftEitherWith sessionAuditStoreError $
          runQuery source issueSessionWithAuditQuery (sessionAuditParameters session activity)
      liftEither (decodeIssuedSession (sessionIdText (sessionId session)) rows)

sessionAuditStoreError :: Text -> AccountSessionAuditStoreError
sessionAuditStoreError databaseError
  | "account audit partition capacity is exhausted" `Text.isInfixOf` databaseError = AccountSessionAuditCapacityExceeded
  | otherwise = AccountSessionAuditStoreUnavailable

decodeIssuedSession :: Text -> [[Text]] -> Either AccountSessionAuditStoreError Bool
decodeIssuedSession expectedSessionId rows =
  case rows of
    [[returnedSessionId]]
      | returnedSessionId == expectedSessionId -> Right True
    [] -> Right False
    _ -> Left AccountSessionAuditStoreCorruptData

sessionAuditParameters :: OpaqueSession AccountId -> AccountActivity -> [Maybe Text]
sessionAuditParameters session activity =
  [ Just (sessionIdText (sessionId session)),
    Just (accountIdText (sessionPrincipal session)),
    Just (Text.pack (show (unixTimeNanosecondsValue (sessionIssuedAtNanoseconds session)))),
    Just (Text.pack (show (unixTimeNanosecondsValue (sessionExpiresAtNanoseconds session)))),
    Just (accountIdText (activitySubject activity)),
    Just (requestIdText (activityRequestId activity)),
    Just (accountAuditEventCode payload),
    Just (Text.pack (show (accountAuditPayloadVersion payload))),
    accountAuditPayloadDetail payload
  ]
    <> routeParameters (activityRoute activity)
  where
    payload = accountAuditEventPayload (activityEvent activity)

routeParameters :: Maybe AuditRouteObservation -> [Maybe Text]
routeParameters maybeRoute =
  case maybeRoute of
    Nothing -> [Nothing, Nothing, Nothing, Nothing]
    Just auditRoute ->
      [ Just (auditRouteEndpointName auditRoute),
        Just (auditRouteMountChain auditRoute),
        Just (auditRouteTemplate auditRoute),
        Just (auditRouteLocale auditRoute)
      ]

issueSessionWithAuditQuery :: Text
issueSessionWithAuditQuery =
  "SELECT session_id FROM account_audit.issue_account_session_with_activity($1, $2, $3::BIGINT, $4::BIGINT, $5, $6, $7, $8::SMALLINT, $9, $10, $11, $12, $13);"
