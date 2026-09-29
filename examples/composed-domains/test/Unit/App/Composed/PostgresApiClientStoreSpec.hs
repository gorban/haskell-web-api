module Unit.App.Composed.PostgresApiClientStoreSpec (spec) where

import App.Composed
  ( buildPostgresComposedApiClientStoreWithRunner,
    provisionComposedExampleApiClientWithRunner,
  )
import App.Composed.ApiClient
import Control.Monad (forM_)
import Data.ByteString qualified as ByteString
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Authentication
  ( ApiClientStore (..),
    ApiClientStoreError,
    oauth2ScopeText,
  )
import HarchWeb.Password
  ( PasswordHash,
    argon2Iterations,
    argon2MemoryKib,
    argon2Parallelism,
    hashPasswordWithSalt,
    mkPassword,
    mkPasswordHashingPolicy,
    passwordHashText,
  )
import Test.Hspec

spec :: Spec
spec = describe "Unit.App.Composed.Postgres.ApiClientStore" $ do
  it "reads one issuance snapshot and a secret-free current-principal view" $ do
    calls <- newIORef []
    let clientId = requiredClientId "composed-example"
        queryRunner _ sql parameters = do
          modifyIORef' calls ((sql, parameters) :)
          pure (Right (validClientRows testHash))
        store = buildPostgresComposedApiClientStoreWithRunner queryRunner ()
    found <- findApiClient store clientId
    established <- establishApiClient store clientId
    case found of
      Right (Just client) -> do
        composedApiClientIdText (composedApiClientId client) `shouldBe` "composed-example"
        fmap oauth2ScopeText (composedApiClientAllowedScopes client) `shouldBe` ["catalog:read", "orders:write"]
        fmap oauth2ScopeText (composedApiClientDefaultScopes client) `shouldBe` ["catalog:read", "orders:write"]
      _ -> expectationFailure "the durable client snapshot should decode"
    case established of
      Right (Just client) -> do
        composedApiClientIdText (establishedComposedApiClientId client) `shouldBe` "composed-example"
        fmap oauth2ScopeText (establishedComposedApiClientAllowedScopes client) `shouldBe` ["catalog:read", "orders:write"]
      _ -> expectationFailure "the current bearer principal should decode"
    observed <- readIORef calls
    fmap snd observed `shouldBe` [["composed-example"], ["composed-example"]]
    fmap (Text.isInfixOf "client.is_enabled" . fst) observed `shouldBe` [True, True]
    fmap (Text.isInfixOf "active_secret_hash" . fst) observed `shouldBe` [False, True]

  it "collapses query failures and corrupt durable rows to one opaque store failure" $ do
    let clientId = requiredClientId "composed-example"
        hashText = passwordHashText testHash
        badRows =
          [ ["composed-example", "secret-store-error-sentinel", "catalog:read", "true", "0"],
            ["composed-example", hashText, "not a scope", "true", "0"],
            ["composed-example", hashText, "catalog:read", "default?", "0"],
            ["composed-example", hashText, "catalog:read", "true", "not-a-position"],
            ["composed-example", hashText, "catalog:read", "true", "1"],
            ["composed-example", hashText, "catalog:read", "true", "0", "extra"],
            ["different-client", hashText, "catalog:read", "true", "0"],
            ["composed-example", hashText, "catalog:read", "false", "0", "ignored"]
          ]
    forM_ badRows $ \rows -> do
      result <- findApiClient (storeReturning (Right [rows])) clientId
      isStoreFailure result `shouldBe` True
    failedQuery <- findApiClient (storeReturning (Left "secret-query-sentinel")) clientId
    case failedQuery of
      Left failure ->
        let publicDiagnostic = show failure
         in not ("secret-query-sentinel" `Text.isInfixOf` Text.pack publicDiagnostic) `shouldBe` True
      Right _ -> expectationFailure "a query error must stay on the private unavailable rail"
    noRows <- findApiClient (storeReturning (Right [])) clientId
    isMissingClient noRows `shouldBe` True

  it "seeds only the fixed example scopes and sends only the Argon2 hash to PostgreSQL" $ do
    calls <- newIORef []
    let queryRunner _ sql parameters = do
          modifyIORef' calls ((sql, parameters) :)
          pure (Right [["composed-example"]])
    result <- provisionComposedExampleApiClientWithRunner queryRunner () testHash
    result `shouldBe` Right True
    observed <- readIORef calls
    case observed of
      [(sql, parameters)] -> do
        Text.isInfixOf "INSERT INTO composed.api_clients" sql `shouldBe` True
        (parameters == ["composed-example", passwordHashText testHash, "catalog:read", "orders:write"]) `shouldBe` True
        ("one-time-example-secret" `elem` parameters) `shouldBe` False
      _ -> expectationFailure "seeding should use one atomic parameterized statement"
    provisionComposedExampleApiClientWithRunner (\_ _ _ -> pure (Right [])) () testHash `shouldReturn` Right False
    failedProvision <- provisionComposedExampleApiClientWithRunner (\_ _ _ -> pure (Left "seed-store-sentinel")) () testHash
    isStoreFailure failedProvision `shouldBe` True
    malformedProvision <- provisionComposedExampleApiClientWithRunner (\_ _ _ -> pure (Right [["different-client"]])) () testHash
    isStoreFailure malformedProvision `shouldBe` True

storeReturning :: Either Text [[Text]] -> ApiClientStore ComposedApiClientId ComposedApiClient EstablishedComposedApiClient
storeReturning result = buildPostgresComposedApiClientStoreWithRunner (\_ _ _ -> pure result) ()

validClientRows :: PasswordHash -> [[Text]]
validClientRows secretHash =
  [ ["composed-example", passwordHashText secretHash, "catalog:read", "true", "0"],
    ["composed-example", passwordHashText secretHash, "orders:write", "true", "1"]
  ]

isStoreFailure :: Either ApiClientStoreError value -> Bool
isStoreFailure result =
  case result of
    Left _ -> True
    Right _ -> False

isMissingClient :: Either ApiClientStoreError (Maybe value) -> Bool
isMissingClient result =
  case result of
    Right Nothing -> True
    _ -> False

requiredClientId :: Text -> ComposedApiClientId
requiredClientId value =
  case mkComposedApiClientId value of
    Right identifier -> identifier
    Left _ -> error "test client identifier must be valid"

testHash :: PasswordHash
testHash =
  case mkPasswordHashingPolicy (argon2Iterations 1) (argon2MemoryKib 8) (argon2Parallelism 1) of
    Nothing -> error "test hashing policy must be valid"
    Just policy ->
      case hashPasswordWithSalt policy (ByteString.pack [0 .. 15]) (mkPassword "one-time-example-secret") of
        Just hashValue -> hashValue
        Nothing -> error "test Argon2id hash should be constructible"
