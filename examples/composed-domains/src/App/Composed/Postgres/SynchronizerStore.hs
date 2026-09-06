{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | PostgreSQL adapter for the composed synchronizer-token capability.
module App.Composed.Postgres.SynchronizerStore
  ( SynchronizerStoragePolicy,
    buildPostgresSynchronizerTokenStoreWithRunner,
    defaultSynchronizerStoragePolicy,
    mkSynchronizerStoragePolicy,
  )
where

import App.Composed.CsrfSynchronizer
  ( SynchronizerTokenCapacityPolicy (..),
    SynchronizerTokenStore (..),
    SynchronizerTokenStoreError (..),
    synchronizerTokenDigestText,
  )
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import HarchWeb.Csrf (csrfBindingDigestText)
import HarchWeb.Time (unixTimeNanosecondsValue)

newtype SynchronizerStoragePolicy = SynchronizerStoragePolicy Word64
  deriving (Eq, Show)

defaultSynchronizerStoragePolicy :: SynchronizerStoragePolicy
defaultSynchronizerStoragePolicy = SynchronizerStoragePolicy 16

mkSynchronizerStoragePolicy :: Word64 -> Maybe SynchronizerStoragePolicy
mkSynchronizerStoragePolicy capacity
  | capacity == 0 || capacity > fromIntegral (maxBound :: Int64) = Nothing
  | otherwise = Just (SynchronizerStoragePolicy capacity)

-- | The runner is supplied by the composed deployment's own pooled libpq
-- adapter. Each query uses values only as parameters; the owned SQL has no
-- raw token, principal, session, or binding interpolation.
buildPostgresSynchronizerTokenStoreWithRunner ::
  SynchronizerStoragePolicy ->
  (source -> Text -> [Text] -> IO (Either Text [[Text]])) ->
  source ->
  SynchronizerTokenStore
buildPostgresSynchronizerTokenStoreWithRunner (SynchronizerStoragePolicy capacity) runQuery source =
  SynchronizerTokenStore
    { saveSynchronizerToken = \capacityPolicy tokenDigest bindingDigest issuedAt expiresAt ->
        queryBoolean (saveQuery capacityPolicy) [synchronizerTokenDigestText tokenDigest, csrfBindingDigestText bindingDigest, Text.pack (show (unixTimeNanosecondsValue issuedAt)), Text.pack (show (unixTimeNanosecondsValue expiresAt)), Text.pack (show capacity)],
      verifySynchronizerToken = \tokenDigest bindingDigest now ->
        queryBoolean verifyQuery [synchronizerTokenDigestText tokenDigest, csrfBindingDigestText bindingDigest, Text.pack (show (unixTimeNanosecondsValue now))],
      cleanupSynchronizerTokens = \now ->
        queryUnit cleanupQuery [Text.pack (show (unixTimeNanosecondsValue now))]
    }
  where
    queryBoolean sql parameters =
      fmap decodeBoolean (runQuery source sql parameters)
    queryUnit sql parameters =
      fmap (either (const (Left SynchronizerTokenStoreUnavailable)) (const (Right ()))) (runQuery source sql parameters)

decodeBoolean :: Either Text [[Text]] -> Either SynchronizerTokenStoreError Bool
decodeBoolean result =
  case result of
    Left _ -> Left SynchronizerTokenStoreUnavailable
    Right [["true"]] -> Right True
    Right [["false"]] -> Right False
    Right _ -> Left SynchronizerTokenStoreCorrupt

saveQuery :: SynchronizerTokenCapacityPolicy -> Text
saveQuery = \case
  PreserveSynchronizerTokens ->
    "WITH capacity_lock AS (SELECT pg_advisory_xact_lock(hashtextextended('composed.csrf_synchronizer.binding.v1:' || $2::TEXT, 0))), inserted AS (INSERT INTO composed.csrf_synchronizer_tokens (token_digest, binding_digest, issued_at_nanoseconds, expires_at_nanoseconds) SELECT $1, $2, $3::BIGINT, $4::BIGINT WHERE EXISTS (SELECT 1 FROM capacity_lock) AND (SELECT count(*) FROM composed.csrf_synchronizer_tokens WHERE binding_digest = $2 AND expires_at_nanoseconds > $3::BIGINT AND revoked_at_nanoseconds IS NULL) < $5::BIGINT ON CONFLICT (token_digest) DO NOTHING RETURNING token_digest) SELECT CASE WHEN EXISTS (SELECT 1 FROM inserted) THEN 'true' ELSE 'false' END;"
  ReplaceOldestSynchronizerToken ->
    "WITH capacity_lock AS (SELECT pg_advisory_xact_lock(hashtextextended('composed.csrf_synchronizer.binding.v1:' || $2::TEXT, 0))), ranked AS MATERIALIZED (SELECT token_digest, row_number() OVER (ORDER BY issued_at_nanoseconds ASC, token_digest ASC) AS position, count(*) OVER () AS active_count FROM composed.csrf_synchronizer_tokens WHERE binding_digest = $2 AND expires_at_nanoseconds > $3::BIGINT AND revoked_at_nanoseconds IS NULL AND EXISTS (SELECT 1 FROM capacity_lock)), evicted AS (DELETE FROM composed.csrf_synchronizer_tokens WHERE token_digest IN (SELECT token_digest FROM ranked WHERE position <= active_count - $5::BIGINT + 1) RETURNING token_digest), eviction_finished AS (SELECT count(*) FROM evicted), inserted AS (INSERT INTO composed.csrf_synchronizer_tokens (token_digest, binding_digest, issued_at_nanoseconds, expires_at_nanoseconds) SELECT $1, $2, $3::BIGINT, $4::BIGINT WHERE EXISTS (SELECT 1 FROM capacity_lock) AND (SELECT count(*) FROM eviction_finished) >= 0 ON CONFLICT (token_digest) DO NOTHING RETURNING token_digest) SELECT CASE WHEN EXISTS (SELECT 1 FROM inserted) THEN 'true' ELSE 'false' END;"

verifyQuery, cleanupQuery :: Text
verifyQuery =
  "SELECT CASE WHEN EXISTS (SELECT 1 FROM composed.csrf_synchronizer_tokens WHERE token_digest = $1 AND binding_digest = $2 AND expires_at_nanoseconds > $3::BIGINT AND revoked_at_nanoseconds IS NULL) THEN 'true' ELSE 'false' END;"
cleanupQuery =
  "DELETE FROM composed.csrf_synchronizer_tokens WHERE expires_at_nanoseconds <= $1::BIGINT OR revoked_at_nanoseconds IS NOT NULL RETURNING token_digest;"
