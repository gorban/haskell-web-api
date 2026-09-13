{-# LANGUAGE OverloadedStrings #-}

-- | PostgreSQL adapter for the one application-owned atomic verification
-- resend settlement/audit operation. It invokes one controlled function; a
-- separate generic completion followed by append could otherwise leave a
-- delivered token and rolling delivery record without its required audit
-- evidence.
module WebApi.Postgres.VerificationResendAuditRepository
  ( buildRuntimePostgresVerificationResendAuditStore,
    buildRuntimePostgresVerificationResendAuditStoreWithRunner,
  )
where

import Control.Monad.Except (liftEither, runExceptT)
import Core.Control.Error (liftEitherWith)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Account (accountIdText, emailVerificationTokenDigestText)
import HarchWeb.RequestId (requestIdText)
import HarchWeb.Time (UnixTimeNanoseconds, unixTimeNanosecondsValue)
import WebApi.Account
  ( VerificationResendClaim (..),
    VerificationResendClaimSettlement (..),
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
import WebApi.VerificationResendAudit
  ( VerificationResendAuditStore (..),
    VerificationResendAuditStoreError (..),
  )

buildRuntimePostgresVerificationResendAuditStore :: PostgresPool -> VerificationResendAuditStore
buildRuntimePostgresVerificationResendAuditStore =
  buildRuntimePostgresVerificationResendAuditStoreWithRunner runPooledNullableParameterizedRowsQuery

buildRuntimePostgresVerificationResendAuditStoreWithRunner ::
  (source -> Text -> [Maybe Text] -> IO (Either Text [[Text]])) ->
  source ->
  VerificationResendAuditStore
buildRuntimePostgresVerificationResendAuditStoreWithRunner runQuery source =
  VerificationResendAuditStore $ \claim now activity ->
    runExceptT $ do
      rows <-
        liftEitherWith verificationResendAuditStoreError $
          runQuery source completeVerificationResendWithAuditQuery (verificationResendAuditParameters claim now activity)
      liftEither (decodeVerificationResendSettlement claim rows)

verificationResendAuditStoreError :: Text -> VerificationResendAuditStoreError
verificationResendAuditStoreError databaseError
  | "account audit partition capacity is exhausted" `Text.isInfixOf` databaseError = VerificationResendAuditCapacityExceeded
  | otherwise = VerificationResendAuditStoreUnavailable

decodeVerificationResendSettlement :: VerificationResendClaim -> [[Text]] -> Either VerificationResendAuditStoreError VerificationResendClaimSettlement
decodeVerificationResendSettlement claim rows =
  case rows of
    [["settled", returnedAccountId]]
      | returnedAccountId == accountIdText (verificationResendClaimAccountId claim) -> Right VerificationResendClaimSettled
    [["lost", ""]] -> Right VerificationResendClaimLost
    _ -> Left VerificationResendAuditStoreCorruptData

verificationResendAuditParameters :: VerificationResendClaim -> UnixTimeNanoseconds -> AccountActivity -> [Maybe Text]
verificationResendAuditParameters claim now activity =
  [ Just (accountIdText (verificationResendClaimAccountId claim)),
    Just (emailVerificationTokenDigestText (verificationResendClaimTokenDigest claim)),
    Just (Text.pack (show (unixTimeNanosecondsValue now))),
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

completeVerificationResendWithAuditQuery :: Text
completeVerificationResendWithAuditQuery =
  "SELECT outcome, value FROM account_audit.complete_verification_resend_with_activity($1, $2, $3, $4, $5, $6, $7::SMALLINT, $8, $9, $10, $11, $12);"
