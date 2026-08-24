{-# LANGUAGE OverloadedStrings #-}

module WebApi.Postgres.LoginAttemptRepository
  ( buildRuntimePostgresLoginAttemptStore,
    buildRuntimePostgresLoginAttemptStoreWithRunner,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import HarchWeb.LoginProtection (LoginProtectionPolicy (..))
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

buildRuntimePostgresLoginAttemptStore :: PostgresPool -> LoginAttemptStore
buildRuntimePostgresLoginAttemptStore !pool =
  buildRuntimePostgresLoginAttemptStoreWithRunner runPooledParameterizedRowsQuery pool

buildRuntimePostgresLoginAttemptStoreWithRunner ::
  (source -> Text -> [Text] -> IO (Either Text [[Text]])) ->
  source ->
  LoginAttemptStore
buildRuntimePostgresLoginAttemptStoreWithRunner runQuery source =
  LoginAttemptStore
    { reserveLoginAttempt = reserveAttempt,
      settleLoginAttempt = settleAttempt,
      cancelLoginAttempt = cancelAttempt
    }
  where
    reserveAttempt key policy now =
      runLoginAttemptStoreQuery
        ( runQuery
            source
            reserveLoginAttemptQuery
            [ key,
              Text.pack (show (windowStartNanoseconds policy now)),
              Text.pack (show (unixTimeNanosecondsValue now)),
              Text.pack (show (loginProtectionMaximumFailures policy)),
              Text.pack (show (loginProtectionLockoutNanoseconds policy))
            ]
        )
        decodeAdmission

    settleAttempt (LoginAttemptReservation reservationId) succeeded =
      runLoginAttemptStoreQuery
        (runQuery source settleLoginAttemptQuery [reservationId, succeededText succeeded])
        requireOneRow

    cancelAttempt (LoginAttemptReservation reservationId) =
      runLoginAttemptStoreQuery
        (runQuery source cancelLoginAttemptQuery [reservationId])
        (const (Right ()))

windowStartNanoseconds :: LoginProtectionPolicy -> UnixTimeNanoseconds -> Word64
windowStartNanoseconds policy now =
  if unixTimeNanosecondsValue now >= loginProtectionWindowNanoseconds policy
    then unixTimeNanosecondsValue now - loginProtectionWindowNanoseconds policy
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
    _ -> malformed
  where
    malformed = Left (LoginAttemptStoreCorruptData ("unexpected login-attempt admission result: " <> Text.pack (show rows)))

requireOneRow :: [[Text]] -> Either LoginAttemptStoreError ()
requireOneRow rows =
  case rows of
    [[_]] -> Right ()
    _ -> Left (LoginAttemptStoreCorruptData ("unexpected login-attempt settlement result: " <> Text.pack (show rows)))

succeededText :: Bool -> Text
succeededText succeeded = if succeeded then "true" else "false"

-- | 'RETURNING' is required even though the returned value is discarded:
-- without it, a plain 'INSERT' reports @CommandOk@ rather than @TuplesOk@,
-- which both 'WebApi.Postgres.Runtime.runRuntimeParameterizedRowsQuery' and
-- its pooled counterpart treat as a query failure (they only read rows out
-- of a tuples-returning result). Every other INSERT in this package follows
-- the same convention.
reserveLoginAttemptQuery :: Text
reserveLoginAttemptQuery = "SELECT outcome, value FROM web_api.reserve_login_attempt($1::TEXT, $2::BIGINT, $3::BIGINT, $4::BIGINT, $5::BIGINT);"

settleLoginAttemptQuery :: Text
settleLoginAttemptQuery = "UPDATE web_api.login_attempts SET succeeded = $2, settled = true WHERE attempt_id = $1::BIGINT AND settled = false RETURNING attempt_id::TEXT;"

cancelLoginAttemptQuery :: Text
cancelLoginAttemptQuery = "DELETE FROM web_api.login_attempts WHERE attempt_id = $1::BIGINT AND settled = false RETURNING attempt_id::TEXT;"
