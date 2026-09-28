{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | PostgreSQL adapter for the composed example's durable OAuth clients.
--
-- One SQL snapshot reads the enabled-client bit, its one current secret hash,
-- and the ordered allowed/default-scope policy. Runtime credentials receive
-- read-only grants; schema changes and the explicit example-client setup
-- command own writes. Database details and corrupt rows share one stable,
-- private dependency failure so the token protocol cannot disclose client
-- state.
module App.Composed.Postgres.ApiClientStore
  ( buildPostgresComposedApiClientStoreWithRunner,
    provisionComposedExampleApiClientWithRunner,
  )
where

import App.Composed.ApiClient
  ( ComposedApiClient,
    ComposedApiClientId,
    EstablishedComposedApiClient,
    composedApiClientIdText,
    composedExampleApiClientId,
    composedExampleApiClientScopeTexts,
    mkComposedApiClient,
    mkComposedApiClientId,
    mkEstablishedComposedApiClient,
  )
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
import HarchWeb.Password (PasswordHash (..), readPasswordHash)
import Text.Read (readMaybe)

buildPostgresComposedApiClientStoreWithRunner ::
  (source -> Text -> [Text] -> IO (Either Text [[Text]])) ->
  source ->
  ApiClientStore ComposedApiClientId ComposedApiClient EstablishedComposedApiClient
buildPostgresComposedApiClientStoreWithRunner runQuery source =
  ApiClientStore
    { findApiClient = \clientId ->
        runStoreQuery (runQuery source findApiClientQuery [composedApiClientIdText clientId]) (decodeIssuanceRows clientId),
      establishApiClient = \clientId ->
        runStoreQuery (runQuery source establishApiClientQuery [composedApiClientIdText clientId]) (decodeEstablishedRows clientId)
    }

-- | Insert the reference example client only when the operator explicitly
-- supplies its secret to the setup program. The caller passes only the
-- Argon2id hash; this fixed record grants both documented API operations by
-- default and never writes or returns plaintext credential material.
provisionComposedExampleApiClientWithRunner ::
  (source -> Text -> [Text] -> IO (Either Text [[Text]])) ->
  source ->
  PasswordHash ->
  IO (Either ApiClientStoreError Bool)
provisionComposedExampleApiClientWithRunner runQuery source secretHash =
  decodeProvisionResult
    <$> runQuery
      source
      provisionExampleClientQuery
      ( [ composedApiClientIdText composedExampleApiClientId,
          case secretHash of PasswordHash value -> value
        ]
          <> NonEmpty.toList composedExampleApiClientScopeTexts
      )

runStoreQuery :: IO (Either Text [[Text]]) -> ([[Text]] -> Either ApiClientStoreError value) -> IO (Either ApiClientStoreError value)
runStoreQuery query decode =
  runExceptT $ do
    rows <- liftEitherWith (const apiClientStoreUnavailable) query
    liftEither (decode rows)

decodeIssuanceRows :: ComposedApiClientId -> [[Text]] -> Either ApiClientStoreError (Maybe ComposedApiClient)
decodeIssuanceRows expectedClientId rows = do
  decoded <- decodeClientRows expectedClientId rows
  case decoded of
    Nothing -> Right Nothing
    Just clientRows -> do
      secretHash <- maybe (Left apiClientStoreUnavailable) Right (readPasswordHash (decodedSecretHash clientRows))
      scopes <- traverse decodeScopeRow (decodedScopeRows clientRows)
      validateScopeOrder scopes
      let allowedScopes = fmap scopeValue scopes
          defaultScopes = [scope | (scope, isDefault, _) <- scopes, isDefault]
      case NonEmpty.nonEmpty allowedScopes of
        Nothing -> Left apiClientStoreUnavailable
        Just _ -> mapConfigurationFailure (mkComposedApiClient expectedClientId secretHash allowedScopes defaultScopes) >>= Right . Just

decodeEstablishedRows :: ComposedApiClientId -> [[Text]] -> Either ApiClientStoreError (Maybe EstablishedComposedApiClient)
decodeEstablishedRows expectedClientId rows = do
  decoded <- decodeClientRows expectedClientId rows
  case decoded of
    Nothing -> Right Nothing
    Just clientRows -> do
      scopes <- traverse decodeScopeRow (decodedScopeRows clientRows)
      validateScopeOrder scopes
      let allowedScopes = fmap scopeValue scopes
      case NonEmpty.nonEmpty allowedScopes of
        Nothing -> Left apiClientStoreUnavailable
        Just _ -> mapConfigurationFailure (mkEstablishedComposedApiClient expectedClientId allowedScopes) >>= Right . Just

data DecodedClientRows = DecodedClientRows
  { decodedSecretHash :: Text,
    decodedScopeRows :: [(Text, Text, Text)]
  }

decodeClientRows :: ComposedApiClientId -> [[Text]] -> Either ApiClientStoreError (Maybe DecodedClientRows)
decodeClientRows expectedClientId rows =
  case rows of
    [] -> Right Nothing
    firstRow : remainingRows -> do
      (clientText, secretHash) <- decodeClientRow expectedClientId firstRow
      scopeRows <- traverse (decodeMatchingScopeRow clientText secretHash) remainingRows
      firstScopeRow <- decodeScopeColumns firstRow
      let everyScopeRow = firstScopeRow : scopeRows
          actualScopes = filter (not . Text.null . firstOfThree) everyScopeRow
      Right (Just (DecodedClientRows secretHash actualScopes))
  where
    decodeMatchingScopeRow expectedText expectedHash row = do
      (clientText, secretHash) <- decodeClientRow expectedClientId row
      if clientText == expectedText && secretHash == expectedHash
        then decodeScopeColumns row
        else Left apiClientStoreUnavailable

decodeClientRow :: ComposedApiClientId -> [Text] -> Either ApiClientStoreError (Text, Text)
decodeClientRow expectedClientId row =
  case row of
    [clientText, secretHash, _, _, _] -> do
      actualClientId <- either (const (Left apiClientStoreUnavailable)) Right (mkComposedApiClientId clientText)
      if composedApiClientIdText actualClientId == composedApiClientIdText expectedClientId && not (Text.null secretHash)
        then Right (clientText, secretHash)
        else Left apiClientStoreUnavailable
    _ -> Left apiClientStoreUnavailable

decodeScopeColumns :: [Text] -> Either ApiClientStoreError (Text, Text, Text)
decodeScopeColumns row =
  case row of
    [_, _, scopeText, defaultText, positionText] -> Right (scopeText, defaultText, positionText)
    _ -> Left apiClientStoreUnavailable

decodeScopeRow :: (Text, Text, Text) -> Either ApiClientStoreError (OAuth2Scope, Bool, Int)
decodeScopeRow (scopeText, defaultText, positionText) = do
  scope <- either (const (Left apiClientStoreUnavailable)) Right (mkOAuth2Scope scopeText)
  isDefault <-
    case defaultText of
      "true" -> Right True
      "false" -> Right False
      _ -> Left apiClientStoreUnavailable
  position <-
    case readMaybe (Text.unpack positionText) of
      Just value | value >= 0 -> Right value
      _ -> Left apiClientStoreUnavailable
  Right (scope, isDefault, position)

scopeValue :: (OAuth2Scope, Bool, Int) -> OAuth2Scope
scopeValue (scope, _, _) = scope

validateScopeOrder :: [(OAuth2Scope, Bool, Int)] -> Either ApiClientStoreError ()
validateScopeOrder scopes
  | fmap thirdOfThree scopes == [0 .. length scopes - 1] = Right ()
  | otherwise = Left apiClientStoreUnavailable

thirdOfThree :: (value, other, third) -> third
thirdOfThree (_, _, value) = value

firstOfThree :: (Text, Text, Text) -> Text
firstOfThree (firstValue, _, _) = firstValue

-- | Scope order is policy: it determines both the default OAuth response and
-- the stable order in the signed scope claim, so gaps or duplicate positions
-- mean the durable record is corrupt.
mapConfigurationFailure :: Either failure value -> Either ApiClientStoreError value
mapConfigurationFailure = either (const (Left apiClientStoreUnavailable)) Right

decodeProvisionResult :: Either Text [[Text]] -> Either ApiClientStoreError Bool
decodeProvisionResult = either (const (Left apiClientStoreUnavailable)) $ \case
  [] -> Right False
  [[clientText]] | clientText == composedApiClientIdText composedExampleApiClientId -> Right True
  _ -> Left apiClientStoreUnavailable

findApiClientQuery, establishApiClientQuery, provisionExampleClientQuery :: Text
findApiClientQuery =
  "SELECT client.client_id, client.active_secret_hash, COALESCE(scope.scope_text, ''), COALESCE(scope.is_default::TEXT, ''), COALESCE(scope.scope_position::TEXT, '') FROM composed.api_clients AS client LEFT JOIN composed.api_client_scopes AS scope USING (client_id) WHERE client.client_id = $1 AND client.is_enabled ORDER BY scope.scope_position;"
establishApiClientQuery =
  "SELECT client.client_id, 'established'::TEXT, COALESCE(scope.scope_text, ''), COALESCE(scope.is_default::TEXT, ''), COALESCE(scope.scope_position::TEXT, '') FROM composed.api_clients AS client LEFT JOIN composed.api_client_scopes AS scope USING (client_id) WHERE client.client_id = $1 AND client.is_enabled ORDER BY scope.scope_position;"
provisionExampleClientQuery =
  "WITH inserted_client AS (INSERT INTO composed.api_clients (client_id, active_secret_hash) VALUES ($1, $2) ON CONFLICT (client_id) DO NOTHING RETURNING client_id), inserted_scopes AS (INSERT INTO composed.api_client_scopes (client_id, scope_text, is_default, scope_position) SELECT inserted_client.client_id, seeded.scope_text, true, seeded.scope_position FROM inserted_client CROSS JOIN (VALUES ($3::TEXT, 0), ($4::TEXT, 1)) AS seeded(scope_text, scope_position) RETURNING client_id) SELECT client_id FROM inserted_client WHERE (SELECT count(*) FROM inserted_scopes) = 2;"

apiClientStoreUnavailable :: ApiClientStoreError
apiClientStoreUnavailable =
  ApiClientStoreUnavailable
    (mkAuthenticationDependency (requiredSecurityFailureCodeOrDie "composed.api-client.store-unavailable"))
