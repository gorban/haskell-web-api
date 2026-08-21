{-# LANGUAGE OverloadedStrings #-}

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
import Text.Read (readMaybe)
import WebApi.Config (DatabaseConfig)
import WebApi.Mfa
  ( MfaStore (..),
    MfaStoreError (..),
    StoredTotpEnrollment (..),
  )
import WebApi.Postgres.Runtime (runRuntimeParameterizedRowsQuery)

buildRuntimePostgresMfaStore :: DatabaseConfig -> MfaStore
buildRuntimePostgresMfaStore !databaseConfig =
  buildRuntimePostgresMfaStoreWithRunner runRuntimeParameterizedRowsQuery databaseConfig

buildRuntimePostgresMfaStoreWithRunner ::
  (DatabaseConfig -> Text -> [Text] -> IO (Either Text [[Text]])) ->
  DatabaseConfig ->
  MfaStore
buildRuntimePostgresMfaStoreWithRunner runQuery databaseConfig =
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
        (runQuery databaseConfig saveUnconfirmedTotpEnrollmentQuery [accountIdText accountId, encryptedSecret, Text.pack (show now)])
        (decodeMatchingAccount "unexpected TOTP enrollment result: " accountId)

    loadEnrollment accountId =
      runMfaStoreQuery
        (runQuery databaseConfig loadTotpEnrollmentQuery [accountIdText accountId])
        decodeTotpEnrollment

    confirmEnrollment accountId recoveryCodeHashes now =
      runMfaStoreQuery
        (runQuery databaseConfig (confirmTotpEnrollmentQuery recoveryCodeHashes) (accountIdText accountId : Text.pack (show now) : NonEmpty.toList recoveryCodeHashes))
        (decodeMatchingAccount "unexpected TOTP confirmation result: " accountId)

    loadRecoveryCodeHashes accountId =
      runMfaStoreQuery
        (runQuery databaseConfig loadUnusedRecoveryCodeHashesQuery [accountIdText accountId])
        decodeRecoveryCodeHashes

    consumeRecoveryCode accountId recoveryCodeHash now =
      runMfaStoreQuery
        (runQuery databaseConfig consumeRecoveryCodeHashQuery [accountIdText accountId, recoveryCodeHash, Text.pack (show now)])
        (decodeMatchingAccount "unexpected recovery-code consumption result: " accountId)

    markCodeUsed accountId counter =
      runMfaStoreQuery
        (runQuery databaseConfig markTotpCodeUsedQuery [accountIdText accountId, Text.pack (show counter)])
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
    _ -> Left (MfaStoreCorruptData (errorPrefix <> Text.pack (show rows)))

-- | Per @docs/design-guidance.md@'s never-mask-a-gate-finding rule: the @$!@
-- below on the second field label is a last resort, confirmed directly
-- rather than assumed. @"last-used counter"@ is a unique literal, not
-- written anywhere else in this module, so there is no duplicate expression
-- to deduplicate.
{-# ANN decodeTotpEnrollment ("HLint: ignore Redundant $!" :: String) #-}
decodeTotpEnrollment :: [[Text]] -> Either MfaStoreError (Maybe StoredTotpEnrollment)
decodeTotpEnrollment rows =
  case rows of
    [] -> Right Nothing
    [[encryptedSecret, confirmedAtValue, lastUsedCounterValue]] ->
      Just
        <$> ( StoredTotpEnrollment encryptedSecret
                <$> decodeOptionalWord64 "confirmation timestamp" confirmedAtValue
                <*> (decodeOptionalWord64 $! "last-used counter") lastUsedCounterValue
            )
    _ -> Left (MfaStoreCorruptData ("unexpected TOTP enrollment lookup result: " <> Text.pack (show rows)))

decodeOptionalWord64 :: Text -> Text -> Either MfaStoreError (Maybe Word64)
decodeOptionalWord64 _ "" = Right Nothing
decodeOptionalWord64 label value =
  maybe
    (Left (MfaStoreCorruptData ("TOTP enrollment has an invalid " <> label)))
    (Right . Just)
    (readMaybe (Text.unpack value))

decodeRecoveryCodeHashes :: [[Text]] -> Either MfaStoreError [Text]
decodeRecoveryCodeHashes rows =
  maybe
    (Left (MfaStoreCorruptData ("unexpected recovery-code lookup result: " <> Text.pack (show rows))))
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

confirmTotpEnrollmentQuery :: NonEmpty.NonEmpty Text -> Text
confirmTotpEnrollmentQuery recoveryCodeHashes =
  "WITH confirmed AS (UPDATE web_api.account_totp SET confirmed_at_nanoseconds = $2 WHERE account_id = $1 AND confirmed_at_nanoseconds IS NULL RETURNING account_id), removed_codes AS (DELETE FROM web_api.account_recovery_codes WHERE account_id IN (SELECT account_id FROM confirmed)), issued_codes AS (INSERT INTO web_api.account_recovery_codes (account_id, code_hash, created_at_nanoseconds) SELECT confirmed.account_id, recovery_codes.code_hash, $2 FROM confirmed CROSS JOIN (VALUES "
    <> Text.intercalate ", " ["($" <> Text.pack (show parameterIndex) <> ")" | parameterIndex <- [3 .. 2 + length (NonEmpty.toList recoveryCodeHashes)]]
    <> ") AS recovery_codes(code_hash)) SELECT account_id FROM confirmed;"
