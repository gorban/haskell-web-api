{-# LANGUAGE OverloadedStrings #-}

module WebApi.Postgres.LoginAttemptRepository
  ( buildRuntimePostgresLoginAttemptStore,
    buildRuntimePostgresLoginAttemptStoreWithStoragePolicy,
    buildRuntimePostgresLoginAttemptStoreWithRunner,
    buildRuntimePostgresLoginAttemptStoreWithRunnerAndStoragePolicy,
    defaultLoginAttemptStoragePolicy,
    LoginAttemptStoragePolicy,
    mkLoginAttemptStoragePolicy,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import HarchWeb.LoginProtection (LoginProtectionPolicy (..), defaultLoginProtectionPolicy)
import HarchWeb.Time (UnixTimeNanoseconds, unixTimeNanoseconds, unixTimeNanosecondsValue)
import Text.Read (readMaybe)
import WebApi.Login
  ( LoginAttemptAdmission (..),
    LoginAttemptReservation (..),
    LoginAttemptStore (..),
    LoginAttemptStoreError (..),
  )
import WebApi.Postgres.Pool (PostgresPool)
import WebApi.Postgres.Runtime (runPooledParameterizedRowsQuery)

-- | Storage policy for the app-owned login-attempt reservation table.  It is
-- deliberately separate from the security policy: the latter determines a
-- key's admission window, while this policy limits the durable resource that
-- unauthenticated keys can consume across the whole application.
data LoginAttemptStoragePolicy = LoginAttemptStoragePolicy
  { loginAttemptStorageMaximumRows :: Word64,
    loginAttemptStorageRetentionNanoseconds :: Word64
  }

-- | The reference application retains one normal throttle window and permits
-- at most 100,000 unsettled or failed rows across all keys.  At five failures
-- per key this leaves ample ordinary headroom while giving the database a
-- hard, application-owned upper bound.
defaultLoginAttemptStoragePolicy :: LoginAttemptStoragePolicy
defaultLoginAttemptStoragePolicy =
  LoginAttemptStoragePolicy
    { loginAttemptStorageMaximumRows = 100000,
      loginAttemptStorageRetentionNanoseconds = loginProtectionWindowNanoseconds defaultLoginProtectionPolicy
    }

mkLoginAttemptStoragePolicy :: Word64 -> Word64 -> Maybe LoginAttemptStoragePolicy
mkLoginAttemptStoragePolicy maximumRows retentionNanoseconds
  | maximumRows == 0 || retentionNanoseconds == 0 = Nothing
  | otherwise = Just (LoginAttemptStoragePolicy maximumRows retentionNanoseconds)

buildRuntimePostgresLoginAttemptStore :: PostgresPool -> LoginAttemptStore
buildRuntimePostgresLoginAttemptStore !pool =
  buildRuntimePostgresLoginAttemptStoreWithStoragePolicy defaultLoginAttemptStoragePolicy pool

buildRuntimePostgresLoginAttemptStoreWithStoragePolicy :: LoginAttemptStoragePolicy -> PostgresPool -> LoginAttemptStore
buildRuntimePostgresLoginAttemptStoreWithStoragePolicy storagePolicy !pool =
  buildRuntimePostgresLoginAttemptStoreWithRunnerAndStoragePolicy storagePolicy runPooledParameterizedRowsQuery pool

buildRuntimePostgresLoginAttemptStoreWithRunner ::
  (source -> Text -> [Text] -> IO (Either Text [[Text]])) ->
  source ->
  LoginAttemptStore
buildRuntimePostgresLoginAttemptStoreWithRunner =
  buildRuntimePostgresLoginAttemptStoreWithRunnerAndStoragePolicy defaultLoginAttemptStoragePolicy

-- | Decision (PR-S5, 2026-08-24): extend PR-S4's reservation lifecycle,
-- rather than adding a separate cleanup service or a second throttle table.
-- Admission takes one transaction-wide capacity lock, prunes every expired
-- row, checks the global bound, and creates the provisional reservation.
-- Successful settlement and cancellation delete their row; failed or
-- abandoned reservations remain only until a later admission prunes the
-- storage-retention window.  The application validates email length and this
-- adapter refuses a key over 'maximumLoginAttemptKeyCharacters' before any
-- query, so untrusted identifiers cannot allocate an unbounded durable key.
buildRuntimePostgresLoginAttemptStoreWithRunnerAndStoragePolicy ::
  LoginAttemptStoragePolicy ->
  (source -> Text -> [Text] -> IO (Either Text [[Text]])) ->
  source ->
  LoginAttemptStore
buildRuntimePostgresLoginAttemptStoreWithRunnerAndStoragePolicy storagePolicy runQuery source =
  LoginAttemptStore
    { reserveLoginAttempt = reserveAttempt,
      settleLoginAttempt = settleAttempt,
      cancelLoginAttempt = cancelAttempt
    }
  where
    reserveAttempt key policy now =
      if Text.length key > maximumLoginAttemptKeyCharacters
        then pure (Left (LoginAttemptStoreUnavailable "login-attempt key exceeds storage limit"))
        else
          runLoginAttemptStoreQuery
            ( runQuery
                source
                reserveLoginAttemptQuery
                [ key,
                  Text.pack (show (windowStartNanoseconds policy now)),
                  Text.pack (show (storageRetentionStartNanoseconds storagePolicy now)),
                  Text.pack (show (unixTimeNanosecondsValue now)),
                  Text.pack (show (loginProtectionMaximumFailures policy)),
                  Text.pack (show (loginProtectionLockoutNanoseconds policy)),
                  Text.pack (show (loginAttemptStorageMaximumRows storagePolicy))
                ]
            )
            decodeAdmission

    settleAttempt (LoginAttemptReservation reservationId) succeeded =
      runLoginAttemptStoreQuery
        (runQuery source (settleLoginAttemptQuery succeeded) [reservationId])
        requireOneRow

    cancelAttempt (LoginAttemptReservation reservationId) =
      runLoginAttemptStoreQuery
        (runQuery source cancelLoginAttemptQuery [reservationId])
        (const (Right ()))

windowStartNanoseconds :: LoginProtectionPolicy -> UnixTimeNanoseconds -> Word64
windowStartNanoseconds policy =
  startNanoseconds (loginProtectionWindowNanoseconds policy)

storageRetentionStartNanoseconds :: LoginAttemptStoragePolicy -> UnixTimeNanoseconds -> Word64
storageRetentionStartNanoseconds storagePolicy =
  startNanoseconds (loginAttemptStorageRetentionNanoseconds storagePolicy)

startNanoseconds :: Word64 -> UnixTimeNanoseconds -> Word64
startNanoseconds duration now =
  if unixTimeNanosecondsValue now >= duration
    then unixTimeNanosecondsValue now - duration
    else 0

runLoginAttemptStoreQuery :: IO (Either Text [[Text]]) -> ([[Text]] -> Either LoginAttemptStoreError value) -> IO (Either LoginAttemptStoreError value)
runLoginAttemptStoreQuery query decodeRows =
  either (Left . LoginAttemptStoreUnavailable) decodeRows <$> query

decodeAdmission :: [[Text]] -> Either LoginAttemptStoreError LoginAttemptAdmission
decodeAdmission rows =
  case rows of
    [["reserved", reservationId]] -> Right (LoginAttemptReserved (LoginAttemptReservation reservationId))
    [["throttled", lockoutEndsAtValue]] ->
      maybe malformed (Right . LoginAttemptThrottled . unixTimeNanoseconds) (readMaybe (Text.unpack lockoutEndsAtValue))
    [["storage-exhausted", ""]] -> Left (LoginAttemptStoreUnavailable "login-attempt storage capacity exhausted")
    [["key-too-long", ""]] -> Left (LoginAttemptStoreUnavailable "login-attempt key exceeds storage limit")
    _ -> malformed
  where
    malformed = Left (LoginAttemptStoreCorruptData ("unexpected login-attempt admission result: " <> Text.pack (show rows)))

requireOneRow :: [[Text]] -> Either LoginAttemptStoreError ()
requireOneRow rows =
  case rows of
    [[_]] -> Right ()
    _ -> Left (LoginAttemptStoreCorruptData ("unexpected login-attempt settlement result: " <> Text.pack (show rows)))

-- | 'RETURNING' is required even though the returned value is discarded:
-- without it, a plain 'INSERT' reports @CommandOk@ rather than @TuplesOk@,
-- which both 'WebApi.Postgres.Runtime.runRuntimeParameterizedRowsQuery' and
-- its pooled counterpart treat as a query failure (they only read rows out
-- of a tuples-returning result). Every other INSERT in this package follows
-- the same convention.
reserveLoginAttemptQuery :: Text
reserveLoginAttemptQuery = "SELECT outcome, value FROM web_api.reserve_login_attempt($1::TEXT, $2::BIGINT, $3::BIGINT, $4::BIGINT, $5::BIGINT, $6::BIGINT, $7::BIGINT);"

settleLoginAttemptQuery :: Bool -> Text
settleLoginAttemptQuery succeeded =
  if succeeded
    then cancelLoginAttemptQuery
    else "UPDATE web_api.login_attempts SET succeeded = 'false', settled = true WHERE attempt_id = $1::BIGINT AND settled = false RETURNING attempt_id::TEXT;"

cancelLoginAttemptQuery :: Text
cancelLoginAttemptQuery = "DELETE FROM web_api.login_attempts WHERE attempt_id = $1::BIGINT AND settled = false RETURNING attempt_id::TEXT;"

maximumLoginAttemptKeyCharacters :: Int
maximumLoginAttemptKeyCharacters = 260
