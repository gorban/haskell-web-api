{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Unit.WebApi.SessionSpec (spec) where

import Control.Exception (evaluate)
import Control.Monad (unless)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as Text
import Data.Word (Word64)
import HarchWeb.Account (AccountId, mkAccountId)
import HarchWeb.Session
  ( OpaqueSession (..),
    SessionId,
    generateSessionId,
    mkCsrfToken,
    mkSessionId,
  )
import Test.Hspec
import TestCore.CustomAssertions (expectAll)
import TestSupport.RealPostgres (defaultMigrationPostgresConfig, defaultRealPostgresConfig, ensureDefaultPostgresAvailable)
import WebApi.Config (DatabaseConfig (..))
import WebApi.Postgres.Migration (runPostgresMigrationsForRuntime)
import WebApi.Postgres.SessionRepository (buildRuntimePostgresAccountSessionStore, buildRuntimePostgresAccountSessionStoreWithRunner)
import WebApi.Session
  ( AccountSessionStore (..),
    AccountSessionStoreError (..),
    issueAccountSession,
  )

spec :: Spec
spec = do
  describe "runtime PostgreSQL account-session persistence" $ do
    it "uses bound parameters to save, load, and invalidate an opaque session" $ do
      queriesReference <- newIORef []
      let runner runnerDatabaseConfig query parameters = do
            _ <- evaluate (databaseHost runnerDatabaseConfig)
            modifyIORef' queriesReference ((query, parameters) :)
            pure $
              if "INSERT INTO web_api.account_sessions" `Text.isInfixOf` query
                then Right [[sessionIdValue]]
                else
                  if "SELECT account_id, csrf_token" `Text.isInfixOf` query
                    then Right [["account_01", csrfTokenValue, "100", "200"]]
                    else
                      if "UPDATE web_api.account_sessions" `Text.isInfixOf` query
                        then Right [[sessionIdValue]]
                        else Left "unexpected query"
          store = buildRuntimePostgresAccountSessionStoreWithRunner runner databaseConfig
      saveAccountSession store opaqueSession `shouldReturnEqual` Right True
      loadAccountSession store testSessionId `shouldReturnEqual` Right (Just opaqueSession)
      invalidateAccountSession store testSessionId `shouldReturnEqual` Right True
      recordedQueries <- reverse <$> readIORef queriesReference
      recordedQueries
        `shouldBe` [ ( "INSERT INTO web_api.account_sessions (session_id, account_id, csrf_token, issued_at_nanoseconds, expires_at_nanoseconds) VALUES ($1, $2, $3, $4, $5) ON CONFLICT (session_id) DO NOTHING RETURNING session_id;",
                       [sessionIdValue, "account_01", csrfTokenValue, "100", "200"]
                     ),
                     ( "SELECT account_id, csrf_token, issued_at_nanoseconds::TEXT, expires_at_nanoseconds::TEXT FROM web_api.account_sessions WHERE session_id = $1 AND invalidated_at_nanoseconds IS NULL;",
                       [sessionIdValue]
                     ),
                     ( "UPDATE web_api.account_sessions SET invalidated_at_nanoseconds = issued_at_nanoseconds WHERE session_id = $1 AND invalidated_at_nanoseconds IS NULL RETURNING session_id;",
                       [sessionIdValue]
                     )
                   ]

    it "preserves unavailable, declined, and corrupt database outcomes" $ do
      let unavailableStore = buildRuntimePostgresAccountSessionStoreWithRunner (\_ _ _ -> pure (Left "database unavailable")) databaseConfig
          declinedStore = buildRuntimePostgresAccountSessionStoreWithRunner (\_ _ _ -> pure (Right [])) databaseConfig
          malformedStore = buildRuntimePostgresAccountSessionStoreWithRunner (\_ _ _ -> pure (Right [["account_01", csrfTokenValue, "not-a-time", "200"]])) databaseConfig
          wrongSessionStore = buildRuntimePostgresAccountSessionStoreWithRunner (\_ _ _ -> pure (Right [["other-session"]])) databaseConfig
      saveAccountSession unavailableStore opaqueSession `shouldReturnEqual` Left AccountSessionStoreUnavailable
      saveAccountSession declinedStore opaqueSession `shouldReturnEqual` Right False
      saveAccountSession wrongSessionStore opaqueSession `shouldReturnEqual` Left AccountSessionStoreCorruptData
      loadAccountSession unavailableStore testSessionId `shouldReturnEqual` Left AccountSessionStoreUnavailable
      loadAccountSession declinedStore testSessionId `shouldReturnEqual` Right Nothing
      loadAccountSession malformedStore testSessionId `shouldReturnEqual` Left AccountSessionStoreCorruptData
      loadAccountSession (buildRuntimePostgresAccountSessionStoreWithRunner (\_ _ _ -> pure (Right [["invalid account", csrfTokenValue, "100", "200"]])) databaseConfig) testSessionId
        `shouldReturnEqual` Left AccountSessionStoreCorruptData
      loadAccountSession (buildRuntimePostgresAccountSessionStoreWithRunner (\_ _ _ -> pure (Right [["account_01", "invalid-csrf", "100", "200"]])) databaseConfig) testSessionId
        `shouldReturnEqual` Left AccountSessionStoreCorruptData
      loadAccountSession (buildRuntimePostgresAccountSessionStoreWithRunner (\_ _ _ -> pure (Right [["account_01", csrfTokenValue, "100"]])) databaseConfig) testSessionId
        `shouldReturnEqual` Left AccountSessionStoreCorruptData
      invalidateAccountSession unavailableStore testSessionId `shouldReturnEqual` Left AccountSessionStoreUnavailable
      invalidateAccountSession declinedStore testSessionId `shouldReturnEqual` Right False
      invalidateAccountSession wrongSessionStore testSessionId `shouldReturnEqual` Left AccountSessionStoreCorruptData
      loadAccountSession (buildRuntimePostgresAccountSessionStoreWithRunner (\runnerDatabaseConfig _ _ -> evaluate (databaseHost runnerDatabaseConfig) >> pure (Right [["wrong", "shape"]])) databaseConfig) testSessionId
        `shouldReturnEqual` Left AccountSessionStoreCorruptData

    it "covers invalidation-specific persistence outcomes" $ do
      let store result =
            buildRuntimePostgresAccountSessionStoreWithRunner
              ( \runnerDatabaseConfig query _ -> do
                  _ <- evaluate (databaseHost runnerDatabaseConfig)
                  if "UPDATE web_api.account_sessions" `Text.isInfixOf` query
                    then pure result
                    else pure (Left "unexpected query")
              )
              databaseConfig
      invalidateAccountSession (store (Left "database unavailable")) testSessionId `shouldReturnEqual` Left AccountSessionStoreUnavailable
      invalidateAccountSession (store (Right [])) testSessionId `shouldReturnEqual` Right False
      invalidateAccountSession (store (Right [["other-session"]])) testSessionId `shouldReturnEqual` Left AccountSessionStoreCorruptData

    it "executes the native libpq session adapter against a migrated PostgreSQL database" $ do
      ensureDefaultPostgresAvailable
      runPostgresMigrationsForRuntime defaultMigrationPostgresConfig defaultRealPostgresConfig `shouldReturn` Right ()
      unknownSessionId <- generateSessionId
      loadAccountSession (buildRuntimePostgresAccountSessionStore defaultRealPostgresConfig) unknownSessionId `shouldReturnEqual` Right Nothing

    it "keeps the account-session errors comparable without exposing persistence details" $ do
      expectAll
        ( (AccountSessionStoreUnavailable == AccountSessionStoreUnavailable `shouldBe` True)
            :| [AccountSessionStoreUnavailable /= AccountSessionStoreCorruptData `shouldBe` True]
        )

  describe "account-session issuance" $ do
    it "generates and persists an opaque session after authentication succeeds" $ do
      savedSessionReference <- newIORef Nothing
      let store =
            AccountSessionStore
              { saveAccountSession = \session -> writeIORef savedSessionReference (Just session) >> pure (Right True),
                loadAccountSession = \_ -> pure (Right Nothing),
                invalidateAccountSession = \_ -> pure (Right False)
              }
      issuedSessionResult <- issueAccountSession store accountId 100
      case issuedSessionResult of
        Left _ -> expectationFailure "expected session issuance to succeed"
        Right issuedSession -> do
          savedSession <- readIORef savedSessionReference
          expectAll
            ( (sessionPrincipal issuedSession `shouldBe` accountId)
                :| [ sessionIssuedAtNanoseconds issuedSession `shouldBe` 100,
                     sessionExpiresAtNanoseconds issuedSession `shouldBe` 28800000000100,
                     savedSession `shouldBe` Just issuedSession
                   ]
            )

    it "preserves storage failures and refuses collisions or overflowing expirations" $ do
      let store result =
            AccountSessionStore
              { saveAccountSession = \_ -> pure result,
                loadAccountSession = \_ -> pure (Right Nothing),
                invalidateAccountSession = \_ -> pure (Right False)
              }
      issueAccountSession (store (Left AccountSessionStoreUnavailable)) accountId 100 `shouldReturnEqual` Left AccountSessionStoreUnavailable
      issueAccountSession (store (Right False)) accountId 100 `shouldReturnEqual` Left AccountSessionStoreCorruptData
      issueAccountSession (store (Right True)) accountId (maxBound :: Word64) `shouldReturnEqual` Left AccountSessionStoreCorruptData

shouldReturnEqual :: (Eq value) => IO value -> value -> Expectation
shouldReturnEqual action expected = do
  actual <- action
  unless (actual == expected) (expectationFailure "unexpected result")

databaseConfig :: DatabaseConfig
databaseConfig =
  DatabaseConfig
    { databaseHost = "127.0.0.1",
      databasePort = 5432,
      databaseName = "web_api_test",
      databaseUser = "web_api_runtime",
      databasePassword = "password"
    }

accountId :: AccountId
accountId =
  case mkAccountId "account_01" of
    Just value -> value
    Nothing -> error "expected a valid account id"

testSessionId :: SessionId
testSessionId =
  case mkSessionId sessionIdValue of
    Just value -> value
    Nothing -> error "expected a valid session id"

csrfTokenValue :: Text.Text
csrfTokenValue = "abcdefghijklmnopqrstuvwxyz0123456789-_"

sessionIdValue :: Text.Text
sessionIdValue = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_"

opaqueSession :: OpaqueSession AccountId
opaqueSession =
  case mkCsrfToken csrfTokenValue of
    Just csrfToken ->
      OpaqueSession
        { sessionId = testSessionId,
          sessionPrincipal = accountId,
          sessionCsrfToken = csrfToken,
          sessionIssuedAtNanoseconds = 100,
          sessionExpiresAtNanoseconds = 200
        }
    Nothing -> error "expected a valid csrf token"
