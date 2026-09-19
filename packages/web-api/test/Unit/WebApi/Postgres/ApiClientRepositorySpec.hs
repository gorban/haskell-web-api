{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Exception (bracket_)
import Control.Monad (unless)
import Data.Either (isLeft)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as Text
import HarchWeb.Authentication (ApiClientStore (..), oauth2ScopeText)
import HarchWeb.Password (defaultPasswordHashingPolicy, hashPasswordWithSalt, mkPassword, passwordHashText)
import TestSupport.RealPostgres (defaultMigrationPostgresConfig, defaultRealPostgresConfig, ensureDefaultPostgresAvailable)
import Unit.WebApi.TestSupport (databaseConfig, required)
import WebApi.ApiClient (ApiClient, ApiClientId, EstablishedApiClient, apiClientAllowedScopes, apiClientDefaultScopes, apiClientSecretHashes, establishedApiClientAllowedScopes, mkApiClientId)
import WebApi.Config (DatabaseConfig (..))
import WebApi.Postgres.Testing (buildRuntimePostgresApiClientStore, buildRuntimePostgresApiClientStoreWithRunner, newPostgresPool, runPostgresMigrationsForRuntime, runRuntimeParameterizedRowsQuery)

spec =
  describe "runtime PostgreSQL API-client persistence" $ do
    it "uses one current-state query for issuance and bearer establishment" $ do
      queriesReference <- newIORef []
      let runner runnerDatabaseConfig query parameters = do
            _ <- evaluateDatabaseConfig runnerDatabaseConfig
            modifyIORef' queriesReference ((query, parameters) :)
            pure $
              if "'secret'::TEXT" `Text.isInfixOf` query
                then Right issuanceRows
                else Right establishedRows
          store = buildRuntimePostgresApiClientStoreWithRunner runner databaseConfig
      issued <- findApiClient store testClientId
      established <- establishApiClient store testClientId
      expectAll
        ( (issuanceMatches ["resource:read", "profile:read:self"] ["resource:read"] 2 issued `shouldBe` True)
            :| [ establishmentMatches ["resource:read", "profile:read:self"] established `shouldBe` True,
                 (not . isLeft) issued `shouldBe` True,
                 (not . isLeft) established `shouldBe` True
               ]
        )
      recordedQueries <- reverse <$> readIORef queriesReference
      recordedQueries
        `shouldSatisfy` \case
          [issuanceQuery, establishmentQuery] ->
            all ((== ["automation-client"]) . snd) [issuanceQuery, establishmentQuery]
              && all (Text.isPrefixOf "WITH active_client AS" . fst) [issuanceQuery, establishmentQuery]
              && all (Text.isInfixOf "ORDER BY sort_order, scope_order;" . fst) [issuanceQuery, establishmentQuery]
              && Text.isInfixOf "'secret'::TEXT" (fst issuanceQuery)
              && not (Text.isInfixOf "'secret'::TEXT" (fst establishmentQuery))
          _ -> False

    it "rejects unavailable or malformed durable rows without exposing their contents" $ do
      let storeFor result = buildRuntimePostgresApiClientStoreWithRunner (\_ _ _ -> pure result) databaseConfig
      findApiClient (storeFor (Left "database unavailable")) testClientId `shouldSatisfyEqual` isLeft
      findApiClient (storeFor (Right [])) testClientId `shouldSatisfyEqual` isMissing
      findApiClient (storeFor (Right clientMarkerOnly)) testClientId `shouldSatisfyEqual` isMissing
      findApiClient (storeFor (Right [["client", "automation-client", "", "", ""], ["scope", "automation-client", "resource:read", "unexpected", "0"]])) testClientId `shouldSatisfyEqual` isLeft
      establishApiClient (storeFor (Right clientMarkerOnly)) testClientId `shouldSatisfyEqual` isLeft
      establishApiClient (storeFor (Right [["client", "automation-client", "", "", ""], ["scope", "automation-client", "bad scope", "false", "0"]])) testClientId `shouldSatisfyEqual` isLeft

    it "reads rotation, disablement, and scope removal through the runtime role" $ do
      ensureDefaultPostgresAvailable
      runPostgresMigrationsForRuntime defaultMigrationPostgresConfig defaultRealPostgresConfig `shouldReturn` Right ()
      let ownerQuery = runRuntimeParameterizedRowsQuery defaultMigrationPostgresConfig
          cleanup = do
            ownerQuery "DELETE FROM web_api.api_clients WHERE client_id = $1 RETURNING client_id;" ["api-client-repository-spec"] `shouldSatisfyEqual` (not . isLeft)
            pure ()
          setup = do
            cleanup
            ownerQuery "INSERT INTO web_api.api_clients (client_id) VALUES ($1) RETURNING client_id;" ["api-client-repository-spec"] `shouldReturn` Right [["api-client-repository-spec"]]
            ownerQuery "INSERT INTO web_api.api_client_secret_hashes (client_id, secret_hash, created_at_nanoseconds) VALUES ($1, $2, 1), ($1, $3, 2) RETURNING secret_hash;" ["api-client-repository-spec", encodedPasswordHash, rotatingPasswordHash] `shouldReturn` Right [[encodedPasswordHash], [rotatingPasswordHash]]
            ownerQuery "INSERT INTO web_api.api_client_scopes (client_id, scope_text, scope_position, is_default) VALUES ($1, $2, 0, true), ($1, $3, 1, false) RETURNING scope_text;" ["api-client-repository-spec", "resource:read", "profile:read:self"] `shouldReturn` Right [["resource:read"], ["profile:read:self"]]
            ownerQuery "INSERT INTO web_api.api_client_secret_hashes (client_id, secret_hash, created_at_nanoseconds) VALUES ($1, $2, 3) RETURNING secret_hash;" ["api-client-repository-spec", "not-an-argon2id-hash"] `shouldSatisfyEqual` isLeft
      bracket_ setup cleanup $ do
        pool <- newPostgresPool (databasePoolCapacity defaultRealPostgresConfig) defaultRealPostgresConfig
        let store = buildRuntimePostgresApiClientStore pool
            clientId = requiredEither "client id" (mkApiClientId "api-client-repository-spec")
        runRuntimeParameterizedRowsQuery defaultRealPostgresConfig "UPDATE web_api.api_clients SET disabled_at_nanoseconds = 99 WHERE client_id = $1 RETURNING client_id;" ["api-client-repository-spec"] `shouldSatisfyEqual` isLeft
        findApiClient store clientId `shouldSatisfyEqual` issuanceMatches ["resource:read", "profile:read:self"] ["resource:read"] 2
        establishApiClient store clientId `shouldSatisfyEqual` establishmentMatches ["resource:read", "profile:read:self"]
        ownerQuery "UPDATE web_api.api_client_secret_hashes SET disabled_at_nanoseconds = 3 WHERE client_id = $1 AND secret_hash = $2 RETURNING secret_hash;" ["api-client-repository-spec", rotatingPasswordHash] `shouldReturn` Right [[rotatingPasswordHash]]
        findApiClient store clientId `shouldSatisfyEqual` issuanceMatches ["resource:read", "profile:read:self"] ["resource:read"] 1
        ownerQuery "DELETE FROM web_api.api_client_scopes WHERE client_id = $1 AND scope_text = $2 RETURNING scope_text;" ["api-client-repository-spec", "profile:read:self"] `shouldReturn` Right [["profile:read:self"]]
        establishApiClient store clientId `shouldSatisfyEqual` establishmentMatches ["resource:read"]
        ownerQuery "UPDATE web_api.api_clients SET disabled_at_nanoseconds = 4 WHERE client_id = $1 RETURNING client_id;" ["api-client-repository-spec"] `shouldReturn` Right [["api-client-repository-spec"]]
        findApiClient store clientId `shouldSatisfyEqual` isMissing
        establishApiClient store clientId `shouldSatisfyEqual` isMissing

testClientId :: ApiClientId
testClientId = requiredEither "client id" (mkApiClientId "automation-client")

clientMarkerOnly, issuanceRows, establishedRows :: [[Text.Text]]
clientMarkerOnly = [["client", "automation-client", "", "", ""]]
issuanceRows =
  clientMarkerOnly
    <> [ ["secret", "automation-client", encodedPasswordHash, "", ""],
         ["secret", "automation-client", rotatingPasswordHash, "", ""],
         ["scope", "automation-client", "resource:read", "true", "0"],
         ["scope", "automation-client", "profile:read:self", "false", "1"]
       ]
establishedRows =
  clientMarkerOnly
    <> [ ["scope", "automation-client", "resource:read", "true", "0"],
         ["scope", "automation-client", "profile:read:self", "false", "1"]
       ]

encodedPasswordHash, rotatingPasswordHash :: Text.Text
encodedPasswordHash = passwordHashText (required "password hash" (hashPasswordWithSalt defaultPasswordHashingPolicy "0123456789abcdef" (mkPassword "client secret")))
rotatingPasswordHash = passwordHashText (required "rotating password hash" (hashPasswordWithSalt defaultPasswordHashingPolicy "fedcba9876543210" (mkPassword "rotating client secret")))

issuanceMatches :: [Text.Text] -> [Text.Text] -> Int -> Either error (Maybe ApiClient) -> Bool
issuanceMatches expectedAllowedScopes expectedDefaultScopes expectedHashCount result =
  case result of
    Right (Just client) ->
      fmap oauth2ScopeText (apiClientAllowedScopes client) == expectedAllowedScopes
        && fmap oauth2ScopeText (apiClientDefaultScopes client) == expectedDefaultScopes
        && length (apiClientSecretHashes client) == expectedHashCount
    _ -> False

establishmentMatches :: [Text.Text] -> Either error (Maybe EstablishedApiClient) -> Bool
establishmentMatches expectedScopes result =
  case result of
    Right (Just client) -> fmap oauth2ScopeText (establishedApiClientAllowedScopes client) == expectedScopes
    _ -> False

evaluateDatabaseConfig :: DatabaseConfig -> IO ()
evaluateDatabaseConfig config = databaseHost config `seq` pure ()

shouldSatisfyEqual :: IO value -> (value -> Bool) -> Expectation
shouldSatisfyEqual action predicate = do
  actual <- action
  unless (predicate actual) (expectationFailure "unexpected result")

isMissing :: Either error (Maybe value) -> Bool
isMissing result =
  case result of
    Right Nothing -> True
    _ -> False

requiredEither :: String -> Either error value -> value
requiredEither label result =
  case result of
    Right value -> value
    Left _ -> error (label <> " must be valid")
