{-# LANGUAGE OverloadedStrings #-}

-- | PostgreSQL adapter for the composed application's grouped admission
-- attempt lifecycle.  It consumes only the application's closed budget
-- algebra; the generic reservation hand-off remains in Harch.
module App.Composed.Postgres.AdmissionAttemptStore
  ( AdmissionAttemptStoragePolicy,
    buildPostgresAdmissionAttemptStoreWithRunner,
    defaultAdmissionAttemptStoragePolicy,
    mkAdmissionAttemptStoragePolicy,
  )
where

import App.Composed.Admission
  ( AdmissionAttemptAdmission (..),
    AdmissionAttemptBudget (..),
    AdmissionAttemptBudgets,
    AdmissionAttemptReservation (..),
    AdmissionAttemptStore (..),
    AdmissionAttemptStoreError (..),
    admissionAttemptBudgetsToList,
    admissionAttemptScopeStorageKey,
  )
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word64)
import HarchWeb.LoginProtection (LoginProtectionPolicy (..), defaultLoginProtectionPolicy)
import HarchWeb.Time (UnixTimeNanoseconds, unixTimeNanoseconds, unixTimeNanosecondsValue)
import Text.Read (readMaybe)

-- | Bounded application storage is separate from a per-key throttling policy.
-- The reference keeps up to ten thousand retained failed/provisional groups,
-- enough for normal use while keeping anonymous-input growth finite.
data AdmissionAttemptStoragePolicy = AdmissionAttemptStoragePolicy
  { admissionAttemptStorageMaximumGroups :: Word64,
    admissionAttemptStorageRetentionNanoseconds :: Word64
  }

defaultAdmissionAttemptStoragePolicy :: AdmissionAttemptStoragePolicy
defaultAdmissionAttemptStoragePolicy =
  AdmissionAttemptStoragePolicy
    { admissionAttemptStorageMaximumGroups = 10000,
      admissionAttemptStorageRetentionNanoseconds = loginProtectionWindowNanoseconds defaultLoginProtectionPolicy
    }

mkAdmissionAttemptStoragePolicy :: Word64 -> Word64 -> Maybe AdmissionAttemptStoragePolicy
mkAdmissionAttemptStoragePolicy maximumGroups retentionNanoseconds
  | maximumGroups == 0 = Nothing
  | retentionNanoseconds == 0 = Nothing
  | otherwise = Just (AdmissionAttemptStoragePolicy maximumGroups retentionNanoseconds)

buildPostgresAdmissionAttemptStoreWithRunner ::
  AdmissionAttemptStoragePolicy ->
  (source -> Text -> [Text] -> IO (Either Text [[Text]])) ->
  source ->
  AdmissionAttemptStore
buildPostgresAdmissionAttemptStoreWithRunner storagePolicy runQuery source =
  AdmissionAttemptStore
    { reserveAdmissionAttempt = reserveAttempt,
      settleAdmissionAttempt = settleAttempt,
      cancelAdmissionAttempt = cancelAttempt
    }
  where
    reserveAttempt budgets now =
      decodeStoreResult
        <$> runQuery
          source
          reserveAdmissionAttemptQuery
          [ encodeAdmissionAttemptBudgets budgets,
            renderTime (retentionStart storagePolicy now),
            renderTime now,
            Text.pack (show (admissionAttemptStorageMaximumGroups storagePolicy))
          ]

    settleAttempt (AdmissionAttemptReservation reservationId) succeeded =
      decodeWritten
        <$> runQuery source (settleAdmissionAttemptQuery succeeded) [reservationId]

    cancelAttempt (AdmissionAttemptReservation reservationId) =
      decodeCancelled <$> runQuery source cancelAdmissionAttemptQuery [reservationId]

retentionStart :: AdmissionAttemptStoragePolicy -> UnixTimeNanoseconds -> UnixTimeNanoseconds
retentionStart storagePolicy now =
  unixTimeNanoseconds
    ( if unixTimeNanosecondsValue now >= admissionAttemptStorageRetentionNanoseconds storagePolicy
        then unixTimeNanosecondsValue now - admissionAttemptStorageRetentionNanoseconds storagePolicy
        else 0
    )

renderTime :: UnixTimeNanoseconds -> Text
renderTime = Text.pack . show . unixTimeNanosecondsValue

decodeStoreResult :: Either Text [[Text]] -> Either AdmissionAttemptStoreError AdmissionAttemptAdmission
decodeStoreResult result =
  case result of
    Left _ -> Left AdmissionAttemptStoreUnavailable
    Right [["reserved", reservationId]] -> Right (AdmissionAttemptReserved (AdmissionAttemptReservation reservationId))
    Right [["throttled", lockoutText]] ->
      maybe (Left AdmissionAttemptStoreCorrupt) (Right . AdmissionAttemptThrottled . unixTimeNanoseconds) (readMaybe (Text.unpack lockoutText))
    Right [["storage-exhausted", ""]] -> Left AdmissionAttemptStoreUnavailable
    Right _ -> Left AdmissionAttemptStoreCorrupt

decodeWritten :: Either Text [[Text]] -> Either AdmissionAttemptStoreError ()
decodeWritten result =
  case result of
    Left _ -> Left AdmissionAttemptStoreUnavailable
    Right [[_]] -> Right ()
    Right _ -> Left AdmissionAttemptStoreCorrupt

decodeCancelled :: Either Text [[Text]] -> Either AdmissionAttemptStoreError ()
decodeCancelled = either (const (Left AdmissionAttemptStoreUnavailable)) (const (Right ()))

encodeAdmissionAttemptBudgets :: AdmissionAttemptBudgets -> Text
encodeAdmissionAttemptBudgets budgets =
  TextEncoding.decodeUtf8
    ( LazyByteString.toStrict
        (Aeson.encode (map encodeBudget (NonEmpty.toList (admissionAttemptBudgetsToList budgets))))
    )

encodeBudget :: AdmissionAttemptBudget -> Aeson.Value
encodeBudget budget =
  Aeson.object
    [ "key" Aeson..= admissionAttemptScopeStorageKey (admissionAttemptScope budget),
      "maximum" Aeson..= loginProtectionMaximumFailures policy,
      "window" Aeson..= loginProtectionWindowNanoseconds policy,
      "lockout" Aeson..= loginProtectionLockoutNanoseconds policy
    ]
  where
    policy = admissionAttemptPolicy budget

reserveAdmissionAttemptQuery, cancelAdmissionAttemptQuery :: Text
reserveAdmissionAttemptQuery = "SELECT outcome, value FROM composed.reserve_admission_attempt_group($1::JSONB, $2::BIGINT, $3::BIGINT, $4::BIGINT);"
cancelAdmissionAttemptQuery = "DELETE FROM composed.admission_attempt_groups WHERE attempt_group_id = $1::BIGINT AND settled = false RETURNING attempt_group_id::TEXT;"

settleAdmissionAttemptQuery :: Bool -> Text
settleAdmissionAttemptQuery succeeded =
  if succeeded
    then cancelAdmissionAttemptQuery
    else "UPDATE composed.admission_attempt_groups SET succeeded = false, settled = true WHERE attempt_group_id = $1::BIGINT AND settled = false RETURNING attempt_group_id::TEXT;"
