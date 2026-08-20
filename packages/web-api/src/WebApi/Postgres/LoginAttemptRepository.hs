{-# LANGUAGE OverloadedStrings #-}

module WebApi.Postgres.LoginAttemptRepository
  ( buildRuntimePostgresLoginAttemptStore,
    buildRuntimePostgresLoginAttemptStoreWithRunner,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.LoginProtection (LoginAttempt (..))
import Text.Read (readMaybe)
import WebApi.Config (DatabaseConfig)
import WebApi.Login
  ( LoginAttemptStore (..),
    LoginAttemptStoreError (..),
  )
import WebApi.Postgres.Runtime (runRuntimeParameterizedRowsQuery)

buildRuntimePostgresLoginAttemptStore :: DatabaseConfig -> LoginAttemptStore
buildRuntimePostgresLoginAttemptStore !databaseConfig =
  buildRuntimePostgresLoginAttemptStoreWithRunner runRuntimeParameterizedRowsQuery databaseConfig

buildRuntimePostgresLoginAttemptStoreWithRunner ::
  (DatabaseConfig -> Text -> [Text] -> IO (Either Text [[Text]])) ->
  DatabaseConfig ->
  LoginAttemptStore
buildRuntimePostgresLoginAttemptStoreWithRunner runQuery databaseConfig =
  LoginAttemptStore
    { recordLoginAttempt = recordAttempt,
      loadRecentLoginAttempts = loadRecent
    }
  where
    recordAttempt key attempt =
      runLoginAttemptStoreQuery
        ( runQuery
            databaseConfig
            recordLoginAttemptQuery
            [key, Text.pack (show (loginAttemptAtNanoseconds attempt)), succeededText (loginAttemptSucceeded attempt)]
        )
        (const (Right ()))

    loadRecent key sinceNanoseconds =
      runLoginAttemptStoreQuery
        (runQuery databaseConfig loadRecentLoginAttemptsQuery [key, Text.pack (show sinceNanoseconds)])
        decodeLoginAttempts

runLoginAttemptStoreQuery :: IO (Either Text [[Text]]) -> ([[Text]] -> Either LoginAttemptStoreError value) -> IO (Either LoginAttemptStoreError value)
runLoginAttemptStoreQuery query decodeRows =
  either (Left . LoginAttemptStoreUnavailable) decodeRows <$> query

decodeLoginAttempts :: [[Text]] -> Either LoginAttemptStoreError [LoginAttempt]
decodeLoginAttempts rows =
  maybe
    (Left (LoginAttemptStoreCorruptData ("unexpected login-attempt lookup result: " <> Text.pack (show rows))))
    Right
    (traverse decodeLoginAttempt rows)

decodeLoginAttempt :: [Text] -> Maybe LoginAttempt
decodeLoginAttempt row =
  case row of
    [attemptedAtValue, succeededValue] ->
      LoginAttempt
        <$> readMaybe (Text.unpack attemptedAtValue)
        <*> decodeSucceeded succeededValue
    _ -> Nothing

decodeSucceeded :: Text -> Maybe Bool
decodeSucceeded value =
  case value of
    "true" -> Just True
    "false" -> Just False
    _ -> Nothing

succeededText :: Bool -> Text
succeededText succeeded = if succeeded then "true" else "false"

-- | 'RETURNING' is required even though the returned value is discarded:
-- without it, a plain 'INSERT' reports @CommandOk@ rather than @TuplesOk@,
-- which 'runRuntimeParameterizedRowsQuery' treats as a query failure (it
-- only reads rows out of a tuples-returning result). Every other INSERT in
-- this package follows the same convention.
recordLoginAttemptQuery :: Text
recordLoginAttemptQuery = "INSERT INTO web_api.login_attempts (attempt_key, attempted_at_nanoseconds, succeeded) VALUES ($1, $2, $3) RETURNING attempt_key;"

loadRecentLoginAttemptsQuery :: Text
loadRecentLoginAttemptsQuery = "SELECT attempted_at_nanoseconds, succeeded FROM web_api.login_attempts WHERE attempt_key = $1 AND attempted_at_nanoseconds >= $2 ORDER BY attempted_at_nanoseconds ASC;"
