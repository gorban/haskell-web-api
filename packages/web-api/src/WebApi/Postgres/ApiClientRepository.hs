{-# LANGUAGE OverloadedStrings #-}

-- | PostgreSQL adapter for the durable OAuth API-client store.
--
-- Decision record (AHI-4D slice 4, 2026-09-16): each lookup reads its active
-- client marker, secret hashes, and scope policy in one SQL statement.  The
-- one PostgreSQL snapshot means a disabled client or removed scope is never
-- hidden by values stitched together from independent reads.  The runtime
-- role receives only @SELECT@ on this data; provisioning, rotation, and
-- disablement stay owner-side migration operations.  Corrupt durable rows are
-- treated as the same private dependency failure as an unavailable database,
-- so the public OAuth boundary cannot enumerate client state.
module WebApi.Postgres.ApiClientRepository
  ( buildRuntimePostgresApiClientStore,
    buildRuntimePostgresApiClientStoreWithRunner,
  )
where

import Control.Monad.Except (liftEither, runExceptT)
import Core.Control.Error (liftEitherWith)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Authentication
  ( ApiClientStore (..),
    ApiClientStoreError (..),
    OAuth2Scope,
    mkAuthenticationDependency,
    mkOAuth2Scope,
    requiredSecurityFailureCodeOrDie,
  )
import HarchWeb.Password (readPasswordHash)
import Text.Read (readMaybe)
import WebApi.ApiClient
  ( ApiClient,
    ApiClientId,
    EstablishedApiClient,
    apiClientIdText,
    mkApiClient,
    mkApiClientId,
    mkEstablishedApiClient,
  )
import WebApi.Postgres.Pool (PostgresPool)
import WebApi.Postgres.Runtime (runPooledParameterizedRowsQuery)

buildRuntimePostgresApiClientStore :: PostgresPool -> ApiClientStore ApiClientId ApiClient EstablishedApiClient
buildRuntimePostgresApiClientStore !pool =
  buildRuntimePostgresApiClientStoreWithRunner runPooledParameterizedRowsQuery pool

buildRuntimePostgresApiClientStoreWithRunner ::
  (source -> Text -> [Text] -> IO (Either Text [[Text]])) ->
  source ->
  ApiClientStore ApiClientId ApiClient EstablishedApiClient
buildRuntimePostgresApiClientStoreWithRunner runQuery source =
  ApiClientStore
    { findApiClient = findClient,
      establishApiClient = establishClient
    }
  where
    findClient clientId =
      runApiClientQuery
        (runQuery source findApiClientQuery [apiClientIdText clientId])
        decodeIssuanceRows
    establishClient clientId =
      runApiClientQuery
        (runQuery source establishApiClientQuery [apiClientIdText clientId])
        decodeEstablishedRows

runApiClientQuery :: IO (Either Text [[Text]]) -> ([[Text]] -> Either ApiClientStoreError value) -> IO (Either ApiClientStoreError value)
runApiClientQuery query decode =
  runExceptT $ do
    rows <- liftEitherWith (const apiClientStoreUnavailable) query
    liftEither (decode rows)

decodeIssuanceRows :: [[Text]] -> Either ApiClientStoreError (Maybe ApiClient)
decodeIssuanceRows rows = do
  decoded <- decodeRows rows
  case decoded of
    Nothing -> Right Nothing
    Just clientRows -> do
      secretHashes <- traverse (maybe (Left apiClientStoreUnavailable) Right . readPasswordHash) (decodedSecretHashes clientRows)
      case NonEmpty.nonEmpty secretHashes of
        Nothing -> Right Nothing
        Just nonEmptySecretHashes ->
          Just <$> mapConfigurationFailure (mkApiClient (decodedClientId clientRows) nonEmptySecretHashes (decodedAllowedScopes clientRows) (decodedDefaultScopes clientRows))

decodeEstablishedRows :: [[Text]] -> Either ApiClientStoreError (Maybe EstablishedApiClient)
decodeEstablishedRows rows = do
  decoded <- decodeRows rows
  case decoded of
    Nothing -> Right Nothing
    Just clientRows ->
      Just <$> mapConfigurationFailure (mkEstablishedApiClient (decodedClientId clientRows) (decodedAllowedScopes clientRows))

data DecodedClientRows = DecodedClientRows
  { decodedClientId :: ApiClientId,
    decodedSecretHashes :: [Text],
    decodedAllowedScopes :: [OAuth2Scope],
    decodedDefaultScopes :: [OAuth2Scope]
  }

decodeRows :: [[Text]] -> Either ApiClientStoreError (Maybe DecodedClientRows)
decodeRows [] = Right Nothing
decodeRows rows = do
  (clientId, secretHashes, scopes) <- foldl collect (Right (Nothing, [], [])) rows
  case clientId of
    Nothing -> Left apiClientStoreUnavailable
    Just identifier -> do
      (allowedScopes, defaultScopes) <- traverseScopeRows scopes
      Right
        ( Just
            DecodedClientRows
              { decodedClientId = identifier,
                decodedSecretHashes = secretHashes,
                decodedAllowedScopes = allowedScopes,
                decodedDefaultScopes = defaultScopes
              }
        )
  where
    collect accumulated row = do
      (currentClientId, secretHashes, scopes) <- accumulated
      case row of
        ["client", clientIdValue, "", "", ""] -> do
          clientId <- either (const (Left apiClientStoreUnavailable)) Right (mkApiClientId clientIdValue)
          case currentClientId of
            Nothing -> Right (Just clientId, secretHashes, scopes)
            Just _ -> Left apiClientStoreUnavailable
        ["secret", clientIdValue, secretHash, "", ""] -> do
          clientId <- either (const (Left apiClientStoreUnavailable)) Right (mkApiClientId clientIdValue)
          requireMatchingClient currentClientId clientId
          Right (currentClientId, secretHashes <> [secretHash], scopes)
        ["scope", clientIdValue, scopeValue, defaultValue, positionValue] -> do
          clientId <- either (const (Left apiClientStoreUnavailable)) Right (mkApiClientId clientIdValue)
          requireMatchingClient currentClientId clientId
          scope <- either (const (Left apiClientStoreUnavailable)) Right (mkOAuth2Scope scopeValue)
          defaultScope <- parseDefaultScope defaultValue
          position <- parseScopePosition positionValue
          Right (currentClientId, secretHashes, scopes <> [(position, scope, defaultScope)])
        _ -> Left apiClientStoreUnavailable

    requireMatchingClient Nothing _ = Left apiClientStoreUnavailable
    requireMatchingClient (Just expectedClientId) actualClientId
      | apiClientIdText expectedClientId == apiClientIdText actualClientId = Right ()
      | otherwise = Left apiClientStoreUnavailable

traverseScopeRows :: [(Int, OAuth2Scope, Bool)] -> Either ApiClientStoreError ([OAuth2Scope], [OAuth2Scope])
traverseScopeRows scopeRows =
  case scopeRows of
    [] -> Right ([], [])
    _ -> do
      requireSequentialPositions (fmap firstOfThree scopeRows)
      let scopes = fmap secondOfThree scopeRows
      Right (scopes, [scope | (_, scope, isDefault) <- scopeRows, isDefault])
  where
    firstOfThree (position, _, _) = position
    secondOfThree (_, scope, _) = scope

requireSequentialPositions :: [Int] -> Either ApiClientStoreError ()
requireSequentialPositions positions
  | positions == [0 .. length positions - 1] = Right ()
  | otherwise = Left apiClientStoreUnavailable

parseDefaultScope :: Text -> Either ApiClientStoreError Bool
parseDefaultScope "true" = Right True
parseDefaultScope "false" = Right False
parseDefaultScope _ = Left apiClientStoreUnavailable

parseScopePosition :: Text -> Either ApiClientStoreError Int
parseScopePosition value =
  case readMaybe (Text.unpack value) of
    Just position | position >= 0 -> Right position
    _ -> Left apiClientStoreUnavailable

mapConfigurationFailure :: Either configuration value -> Either ApiClientStoreError value
mapConfigurationFailure = either (const (Left apiClientStoreUnavailable)) Right

apiClientStoreUnavailable :: ApiClientStoreError
apiClientStoreUnavailable =
  ApiClientStoreUnavailable
    (mkAuthenticationDependency (requiredSecurityFailureCodeOrDie "api-client.store-unavailable"))

findApiClientQuery, establishApiClientQuery :: Text
findApiClientQuery =
  "WITH active_client AS (SELECT client_id FROM web_api.api_clients WHERE client_id = $1 AND disabled_at_nanoseconds IS NULL) SELECT row_kind, client_id, value, default_scope, scope_position FROM (SELECT 0 AS sort_order, 0 AS scope_order, 'client'::TEXT AS row_kind, client_id, ''::TEXT AS value, ''::TEXT AS default_scope, ''::TEXT AS scope_position FROM active_client UNION ALL SELECT 1 AS sort_order, 0 AS scope_order, 'secret'::TEXT AS row_kind, secret.client_id, secret.secret_hash AS value, ''::TEXT AS default_scope, ''::TEXT AS scope_position FROM web_api.api_client_secret_hashes AS secret JOIN active_client USING (client_id) WHERE secret.disabled_at_nanoseconds IS NULL UNION ALL SELECT 2 AS sort_order, scope.scope_position AS scope_order, 'scope'::TEXT AS row_kind, scope.client_id, scope.scope_text AS value, CASE WHEN scope.is_default THEN 'true' ELSE 'false' END AS default_scope, scope.scope_position::TEXT FROM web_api.api_client_scopes AS scope JOIN active_client USING (client_id)) AS client_rows ORDER BY sort_order, scope_order;"
establishApiClientQuery =
  "WITH active_client AS (SELECT client_id FROM web_api.api_clients WHERE client_id = $1 AND disabled_at_nanoseconds IS NULL) SELECT row_kind, client_id, value, default_scope, scope_position FROM (SELECT 0 AS sort_order, 0 AS scope_order, 'client'::TEXT AS row_kind, client_id, ''::TEXT AS value, ''::TEXT AS default_scope, ''::TEXT AS scope_position FROM active_client UNION ALL SELECT 1 AS sort_order, scope.scope_position AS scope_order, 'scope'::TEXT AS row_kind, scope.client_id, scope.scope_text AS value, CASE WHEN scope.is_default THEN 'true' ELSE 'false' END AS default_scope, scope.scope_position::TEXT FROM web_api.api_client_scopes AS scope JOIN active_client USING (client_id)) AS client_rows ORDER BY sort_order, scope_order;"
