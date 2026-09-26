{-# LANGUAGE OverloadedStrings #-}

-- | PostgreSQL implementation of the application-owned audit append port.
--
-- Decision record (AHI-5, 2026-09-07): extend 'ActivityAuditStore' through
-- the existing pooled, parameterized PostgreSQL runtime boundary.  The audit
-- function is already the one owner of scope selection, catalog validation,
-- partition capacity, and the append, so adding an application SQL builder or
-- a second database abstraction would duplicate that authority.  Nullable
-- payload and route facts use the runtime boundary's nullable parameter form,
-- rather than collapsing absence into a sentinel text value.  This repository
-- deliberately does /not/ make an audit append atomic with an account-state
-- mutation: AHI-5's account-workflow integration must introduce that one
-- transaction boundary before a caller can claim the two effects committed
-- together.
module WebApi.Postgres.ActivityAuditRepository
  ( buildRuntimePostgresActivityAuditStore,
    buildRuntimePostgresActivityAuditStoreWithRunner,
  )
where

import Control.Monad.Except (liftEither, runExceptT)
import Core.Control.Error (liftEitherWith)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import HarchWeb.Account (accountIdText)
import HarchWeb.RequestId (requestIdText)
import Text.Read (readMaybe)
import WebApi.ActivityAudit
  ( AccountActivity (..),
    AccountAuditPayload (..),
    ActivityAuditStore (..),
    ActivityAuditStoreError (..),
    ActivityId,
    AuditRouteObservation,
    accountAuditEventPayload,
    activityIdFromDatabase,
    auditRouteEndpointName,
    auditRouteLocale,
    auditRouteMountChain,
    auditRouteTemplate,
  )
import WebApi.Postgres.Pool (PostgresPool)
import WebApi.Postgres.Runtime
  ( runPooledNullableParameterizedRowsQuery,
  )

buildRuntimePostgresActivityAuditStore :: PostgresPool -> ActivityAuditStore
buildRuntimePostgresActivityAuditStore =
  buildRuntimePostgresActivityAuditStoreWithRunner runPooledNullableParameterizedRowsQuery

buildRuntimePostgresActivityAuditStoreWithRunner ::
  (source -> Text -> [Maybe Text] -> IO (Either Text [[Text]])) ->
  source ->
  ActivityAuditStore
buildRuntimePostgresActivityAuditStoreWithRunner runQuery source =
  ActivityAuditStore appendActivity
  where
    appendActivity activity =
      runExceptT $ do
        rows <-
          liftEitherWith activityAuditStoreError $
            runQuery source appendActivityQuery (appendActivityParameters activity)
        liftEither (decodeActivityId rows)

activityAuditStoreError :: Text -> ActivityAuditStoreError
activityAuditStoreError databaseError
  | "account audit partition capacity is exhausted" `Text.isInfixOf` databaseError = ActivityAuditCapacityExceeded
  | otherwise = ActivityAuditUnavailable

decodeActivityId :: [[Text]] -> Either ActivityAuditStoreError ActivityId
decodeActivityId rows =
  case rows of
    [[activityIdText]] ->
      maybe (Left ActivityAuditCorruptResult) (Right . activityIdFromDatabase) (readMaybe (Text.unpack activityIdText) :: Maybe Word64)
    _ -> Left ActivityAuditCorruptResult

appendActivityParameters :: AccountActivity -> [Maybe Text]
appendActivityParameters activity =
  [ Just (accountIdText (activitySubject activity)),
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

appendActivityQuery :: Text
appendActivityQuery =
  "SELECT activity_id::TEXT FROM account_audit.append_activity($1, $2, $3, $4::SMALLINT, $5, $6, $7, $8, $9);"
