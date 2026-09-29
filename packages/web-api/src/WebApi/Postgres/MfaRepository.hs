module WebApi.Postgres.MfaRepository
  ( buildRuntimePostgresMfaStore,
    buildRuntimePostgresMfaStoreWithRunner,
  )
where

import Control.Monad.Except (liftEither, runExceptT)
import Core.Control.Error (liftEitherWith)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import HarchWeb.Account (AccountId, accountIdText)
import HarchWeb.RequestId (requestIdText)
import HarchWeb.Time (UnixTimeNanoseconds, unixTimeNanoseconds, unixTimeNanosecondsValue)
import Text.Read (readMaybe)
import WebApi.ActivityAudit
  ( ActivityAuditStoreError (..),
    AuditRouteObservation,
    auditRouteEndpointName,
    auditRouteLocale,
    auditRouteMountChain,
    auditRouteTemplate,
  )
import WebApi.Mfa
  ( MfaConfirmationAuditContext (..),
    MfaStore (..),
    MfaStoreError (..),
    StoredTotpEnrollment (..),
  )
import WebApi.Postgres.Pool (PostgresPool)
import WebApi.Postgres.Runtime (renderUnexpectedResultShape, runPooledNullableParameterizedRowsQuery)

buildRuntimePostgresMfaStore :: PostgresPool -> MfaStore
buildRuntimePostgresMfaStore !pool =
  buildRuntimePostgresMfaStoreWithRunner runPooledNullableParameterizedRowsQuery pool

buildRuntimePostgresMfaStoreWithRunner ::
  (source -> Text -> [Maybe Text] -> IO (Either Text [[Text]])) ->
  source ->
  MfaStore
buildRuntimePostgresMfaStoreWithRunner runQuery source =
  MfaStore
    { saveUnconfirmedTotpEnrollment = saveEnrollment,
      loadTotpEnrollment = loadEnrollment,
      confirmTotpEnrollment = confirmEnrollment,
      loadUnusedRecoveryCodeHashes = loadRecoveryCodeHashes,
      consumeRecoveryCodeHash = consumeRecoveryCode,
      markTotpCodeUsed = markCodeUsed
    }
  where
    saveEnrollment accountId encryptedSecret now =
      runMfaStoreQuery
        (runQuery source saveUnconfirmedTotpEnrollmentQuery [Just (accountIdText accountId), Just encryptedSecret, Just (Text.pack (show (unixTimeNanosecondsValue now)))])
        (decodeMatchingAccount "unexpected TOTP enrollment result: " accountId)

    loadEnrollment accountId =
      runMfaStoreQuery
        (runQuery source loadTotpEnrollmentQuery [Just (accountIdText accountId)])
        decodeTotpEnrollment

    confirmEnrollment accountId recoveryCodeHashes now auditContext =
      runExceptT $ do
        rows <-
          liftEitherWith
            confirmationAuditStoreError
            (runQuery source (confirmTotpEnrollmentQuery (length (NonEmpty.toList recoveryCodeHashes))) (confirmationParameters accountId recoveryCodeHashes now auditContext))
        liftEither (decodeMatchingAccount "unexpected TOTP confirmation result: " accountId rows)

    loadRecoveryCodeHashes accountId =
      runMfaStoreQuery
        (runQuery source loadUnusedRecoveryCodeHashesQuery [Just (accountIdText accountId)])
        decodeRecoveryCodeHashes

    consumeRecoveryCode accountId recoveryCodeHash now =
      runMfaStoreQuery
        (runQuery source consumeRecoveryCodeHashQuery [Just (accountIdText accountId), Just recoveryCodeHash, Just (Text.pack (show (unixTimeNanosecondsValue now)))])
        (decodeMatchingAccount "unexpected recovery-code consumption result: " accountId)

    markCodeUsed accountId counter =
      runMfaStoreQuery
        (runQuery source markTotpCodeUsedQuery [Just (accountIdText accountId), Just (Text.pack (show counter))])
        (decodeMatchingAccount "unexpected TOTP counter update result: " accountId)

runMfaStoreQuery :: IO (Either Text [[Text]]) -> ([[Text]] -> Either MfaStoreError value) -> IO (Either MfaStoreError value)
runMfaStoreQuery query decodeRows =
  runExceptT $ do
    rows <- liftEitherWith MfaStoreUnavailable query
    liftEither (decodeRows rows)

decodeMatchingAccount :: Text -> AccountId -> [[Text]] -> Either MfaStoreError Bool
decodeMatchingAccount errorPrefix accountId rows =
  case rows of
    [] -> Right False
    [[returnedAccountId]]
      | returnedAccountId == accountIdText accountId -> Right True
    _ -> Left (MfaStoreCorruptData (errorPrefix <> renderUnexpectedResultShape rows))

decodeTotpEnrollment :: [[Text]] -> Either MfaStoreError (Maybe StoredTotpEnrollment)
decodeTotpEnrollment rows =
  case rows of
    [] -> Right Nothing
    [[encryptedSecret, confirmedAtValue, lastUsedCounterValue]] ->
      Just
        <$> ( StoredTotpEnrollment encryptedSecret
                <$> decodeOptionalUnixTimeNanoseconds "confirmation timestamp" confirmedAtValue
                <*> decodeOptionalWord64 "last-used counter" lastUsedCounterValue
            )
    _ -> Left (MfaStoreCorruptData ("unexpected TOTP enrollment lookup result: " <> renderUnexpectedResultShape rows))

decodeOptionalWord64 :: Text -> Text -> Either MfaStoreError (Maybe Word64)
decodeOptionalWord64 _ "" = Right Nothing
decodeOptionalWord64 label value =
  maybe
    (Left (MfaStoreCorruptData ("TOTP enrollment has an invalid " <> label)))
    (Right . Just)
    (readMaybe (Text.unpack value))

decodeOptionalUnixTimeNanoseconds :: Text -> Text -> Either MfaStoreError (Maybe UnixTimeNanoseconds)
decodeOptionalUnixTimeNanoseconds label value =
  fmap unixTimeNanoseconds <$> decodeOptionalWord64 label value

decodeRecoveryCodeHashes :: [[Text]] -> Either MfaStoreError [Text]
decodeRecoveryCodeHashes rows =
  maybe
    (Left (MfaStoreCorruptData ("unexpected recovery-code lookup result: " <> renderUnexpectedResultShape rows)))
    Right
    (traverse decodeSingleColumn rows)

decodeSingleColumn :: [value] -> Maybe value
decodeSingleColumn row =
  case row of
    [value] -> Just value
    _ -> Nothing

-- | Starting an enrollment must not silently destroy an already-confirmed
-- authenticator. The original @WHERE EXISTS@ guard checked only that the
-- account's email was verified, so re-running enrollment start against a
-- confirmed account reset 'confirmed_at_nanoseconds' to @NULL@ and replaced
-- the secret via the @ON CONFLICT@ upsert with no eligibility check of its
-- own. Extended the same guard with a second @AND NOT EXISTS@ clause (option
-- 1: small, general, squarely within this query's own existing eligibility
-- check) instead of adding a separate pre-check query, since
-- 'startMfaEnrollmentWith' already treats a declined save
-- (@guardError MfaEnrollmentAccountIsNotEligible@) as the correct outcome for
-- "not eligible to start" — the same error a confirmed account should now
-- also receive, reusing the existing interpretation rather than adding a new
-- one.
saveUnconfirmedTotpEnrollmentQuery, loadTotpEnrollmentQuery, loadUnusedRecoveryCodeHashesQuery, consumeRecoveryCodeHashQuery, markTotpCodeUsedQuery :: Text
saveUnconfirmedTotpEnrollmentQuery = "INSERT INTO web_api.account_totp (account_id, encrypted_secret, created_at_nanoseconds) SELECT $1, convert_to($2, 'UTF8'), $3 WHERE EXISTS (SELECT 1 FROM web_api.accounts WHERE account_id = $1 AND email_verified_at_nanoseconds IS NOT NULL) AND NOT EXISTS (SELECT 1 FROM web_api.account_totp WHERE account_id = $1 AND confirmed_at_nanoseconds IS NOT NULL) ON CONFLICT (account_id) DO UPDATE SET encrypted_secret = EXCLUDED.encrypted_secret, confirmed_at_nanoseconds = NULL, created_at_nanoseconds = EXCLUDED.created_at_nanoseconds, last_used_totp_counter = NULL RETURNING account_id;"
loadTotpEnrollmentQuery = "SELECT convert_from(encrypted_secret, 'UTF8'), COALESCE(confirmed_at_nanoseconds::TEXT, ''), COALESCE(last_used_totp_counter::TEXT, '') FROM web_api.account_totp WHERE account_id = $1;"
loadUnusedRecoveryCodeHashesQuery = "SELECT code_hash FROM web_api.account_recovery_codes WHERE account_id = $1 AND used_at_nanoseconds IS NULL ORDER BY code_hash ASC;"
consumeRecoveryCodeHashQuery = "UPDATE web_api.account_recovery_codes SET used_at_nanoseconds = $3 WHERE account_id = $1 AND code_hash = $2 AND used_at_nanoseconds IS NULL RETURNING account_id;"
markTotpCodeUsedQuery = "UPDATE web_api.account_totp SET last_used_totp_counter = $2 WHERE account_id = $1 AND (last_used_totp_counter IS NULL OR last_used_totp_counter < $2) RETURNING account_id;"

confirmationAuditStoreError :: Text -> MfaStoreError
confirmationAuditStoreError databaseError
  | "account audit partition capacity is exhausted" `Text.isInfixOf` databaseError =
      MfaStoreAuditAppendFailed ActivityAuditCapacityExceeded
  | otherwise = MfaStoreAuditAppendFailed ActivityAuditUnavailable

confirmationParameters :: AccountId -> NonEmpty.NonEmpty Text -> UnixTimeNanoseconds -> MfaConfirmationAuditContext -> [Maybe Text]
confirmationParameters accountId recoveryCodeHashes now MfaConfirmationAuditContext {mfaConfirmationRequestId, mfaConfirmationRoute} =
  [ Just (accountIdText accountId),
    Just (Text.pack (show (unixTimeNanosecondsValue now))),
    Just (requestIdText mfaConfirmationRequestId)
  ]
    <> routeParameters mfaConfirmationRoute
    <> fmap Just (NonEmpty.toList recoveryCodeHashes)

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

confirmTotpEnrollmentQuery :: Int -> Text
confirmTotpEnrollmentQuery recoveryCodeCount =
  "SELECT account_id FROM account_audit.confirm_mfa_enrollment_with_activity($1, $2::BIGINT, $3, $4, $5, $6, $7, "
    <> Text.intercalate ", " ["$" <> Text.pack (show parameterIndex) | parameterIndex <- [8 .. 7 + recoveryCodeCount]]
    <> ");"
