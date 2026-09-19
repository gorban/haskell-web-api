{-# LANGUAGE OverloadedStrings #-}

-- | PostgreSQL adapter for the one application-owned atomic registration
-- delivery/audit operation.  It invokes one controlled database function;
-- independent completion and append queries would make a durable audit gap
-- possible after SMTP has already accepted the message.
module WebApi.Postgres.PendingRegistrationAuditRepository
  ( buildRuntimePostgresPendingRegistrationAuditStore,
    buildRuntimePostgresPendingRegistrationAuditStoreWithRunner,
  )
where

import Control.Monad.Except (liftEither, runExceptT)
import Core.Control.Error (liftEitherWith)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Account (accountIdText, emailVerificationTokenDigestText)
import HarchWeb.RequestId (requestIdText)
import WebApi.Account (PendingRegistrationClaim (..))
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
import WebApi.PendingRegistrationAudit
  ( PendingRegistrationAuditStore (..),
    PendingRegistrationAuditStoreError (..),
  )
import WebApi.Postgres.Pool (PostgresPool)
import WebApi.Postgres.Runtime (runPooledNullableParameterizedRowsQuery)

buildRuntimePostgresPendingRegistrationAuditStore :: PostgresPool -> PendingRegistrationAuditStore
buildRuntimePostgresPendingRegistrationAuditStore =
  buildRuntimePostgresPendingRegistrationAuditStoreWithRunner runPooledNullableParameterizedRowsQuery

buildRuntimePostgresPendingRegistrationAuditStoreWithRunner ::
  (source -> Text -> [Maybe Text] -> IO (Either Text [[Text]])) ->
  source ->
  PendingRegistrationAuditStore
buildRuntimePostgresPendingRegistrationAuditStoreWithRunner runQuery source =
  PendingRegistrationAuditStore $ \claim activity ->
    runExceptT $ do
      rows <-
        liftEitherWith pendingRegistrationAuditStoreError $
          runQuery source completePendingRegistrationDeliveryWithAuditQuery (pendingRegistrationAuditParameters claim activity)
      liftEither (decodeCompletedPendingRegistrationDelivery claim rows)

pendingRegistrationAuditStoreError :: Text -> PendingRegistrationAuditStoreError
pendingRegistrationAuditStoreError databaseError
  | "account audit partition capacity is exhausted" `Text.isInfixOf` databaseError = PendingRegistrationAuditCapacityExceeded
  | otherwise = PendingRegistrationAuditStoreUnavailable

decodeCompletedPendingRegistrationDelivery :: PendingRegistrationClaim -> [[Text]] -> Either PendingRegistrationAuditStoreError Bool
decodeCompletedPendingRegistrationDelivery claim rows =
  case rows of
    [[returnedAccountId]]
      | returnedAccountId == accountIdText (pendingRegistrationClaimAccountId claim) -> Right True
    [] -> Right False
    _ -> Left PendingRegistrationAuditStoreCorruptData

pendingRegistrationAuditParameters :: PendingRegistrationClaim -> AccountActivity -> [Maybe Text]
pendingRegistrationAuditParameters claim activity =
  [ Just (accountIdText (pendingRegistrationClaimAccountId claim)),
    Just (emailVerificationTokenDigestText (pendingRegistrationClaimTokenDigest claim)),
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

completePendingRegistrationDeliveryWithAuditQuery :: Text
completePendingRegistrationDeliveryWithAuditQuery =
  "SELECT account_id FROM account_audit.complete_pending_registration_delivery_with_activity($1, $2, $3, $4, $5, $6::SMALLINT, $7, $8, $9, $10, $11);"
