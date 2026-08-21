{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Unit.WebApi.MfaEnrollmentSessionSpec (spec) where

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
    sessionCookieName,
    sessionCookieNameText,
  )
import Test.Hspec
import TestCore.CustomAssertions (expectAll)
import TestSupport.RealPostgres (defaultMigrationPostgresConfig, defaultRealPostgresConfig, ensureDefaultPostgresAvailable)
import WebApi.Config (DatabaseConfig (..))
import WebApi.Postgres.Testing (buildRuntimePostgresMfaEnrollmentSessionStore, buildRuntimePostgresMfaEnrollmentSessionStoreWithRunner, newPostgresPool, runPostgresMigrationsForRuntime)
import WebApi.Session
  ( MfaEnrollmentSessionStore (..),
    MfaEnrollmentSessionStoreError (..),
    issueMfaEnrollmentSession,
    mfaEnrollmentSessionCookiePolicy,
  )

spec :: Spec
spec = do
  describe "runtime PostgreSQL MFA-enrollment-session persistence" $ do
    it "uses bound parameters to save, load, and invalidate an opaque session" $ do
      queriesReference <- newIORef []
      let runner runnerDatabaseConfig query parameters = do
            _ <- evaluate (databaseHost runnerDatabaseConfig)
            modifyIORef' queriesReference ((query, parameters) :)
            pure $
              if "INSERT INTO web_api.mfa_enrollment_sessions" `Text.isInfixOf` query
                then Right [[sessionIdValue]]
                else
                  if "SELECT account_id, csrf_token" `Text.isInfixOf` query
                    then Right [["account_01", csrfTokenValue, "100", "200"]]
                    else
                      if "UPDATE web_api.mfa_enrollment_sessions" `Text.isInfixOf` query
                        then Right [[sessionIdValue]]
                        else Left "unexpected query"
          store = buildRuntimePostgresMfaEnrollmentSessionStoreWithRunner runner databaseConfig
      saveMfaEnrollmentSession store opaqueSession `shouldReturnEqual` Right True
      loadMfaEnrollmentSession store testSessionId `shouldReturnEqual` Right (Just opaqueSession)
      invalidateMfaEnrollmentSession store testSessionId 300 `shouldReturnEqual` Right True
      recordedQueries <- reverse <$> readIORef queriesReference
      recordedQueries
        `shouldBe` [ ( "INSERT INTO web_api.mfa_enrollment_sessions (session_id, account_id, csrf_token, issued_at_nanoseconds, expires_at_nanoseconds) VALUES ($1, $2, $3, $4, $5) ON CONFLICT (session_id) DO NOTHING RETURNING session_id;",
                       [sessionIdValue, "account_01", csrfTokenValue, "100", "200"]
                     ),
                     ( "SELECT account_id, csrf_token, issued_at_nanoseconds::TEXT, expires_at_nanoseconds::TEXT FROM web_api.mfa_enrollment_sessions WHERE session_id = $1 AND invalidated_at_nanoseconds IS NULL;",
                       [sessionIdValue]
                     ),
                     ( "UPDATE web_api.mfa_enrollment_sessions SET invalidated_at_nanoseconds = $2 WHERE session_id = $1 AND invalidated_at_nanoseconds IS NULL RETURNING session_id;",
                       [sessionIdValue, "300"]
                     )
                   ]

    it "preserves unavailable, declined, and corrupt database outcomes" $ do
      let unavailableStore = buildRuntimePostgresMfaEnrollmentSessionStoreWithRunner (\_ _ _ -> pure (Left "database unavailable")) databaseConfig
          declinedStore = buildRuntimePostgresMfaEnrollmentSessionStoreWithRunner (\_ _ _ -> pure (Right [])) databaseConfig
          malformedStore = buildRuntimePostgresMfaEnrollmentSessionStoreWithRunner (\_ _ _ -> pure (Right [["account_01", csrfTokenValue, "not-a-time", "200"]])) databaseConfig
          wrongSessionStore = buildRuntimePostgresMfaEnrollmentSessionStoreWithRunner (\_ _ _ -> pure (Right [["other-session"]])) databaseConfig
      saveMfaEnrollmentSession unavailableStore opaqueSession `shouldReturnEqual` Left MfaEnrollmentSessionStoreUnavailable
      saveMfaEnrollmentSession declinedStore opaqueSession `shouldReturnEqual` Right False
      saveMfaEnrollmentSession wrongSessionStore opaqueSession `shouldReturnEqual` Left MfaEnrollmentSessionStoreCorruptData
      loadMfaEnrollmentSession unavailableStore testSessionId `shouldReturnEqual` Left MfaEnrollmentSessionStoreUnavailable
      loadMfaEnrollmentSession declinedStore testSessionId `shouldReturnEqual` Right Nothing
      loadMfaEnrollmentSession malformedStore testSessionId `shouldReturnEqual` Left MfaEnrollmentSessionStoreCorruptData
      loadMfaEnrollmentSession (buildRuntimePostgresMfaEnrollmentSessionStoreWithRunner (\_ _ _ -> pure (Right [["invalid account", csrfTokenValue, "100", "200"]])) databaseConfig) testSessionId
        `shouldReturnEqual` Left MfaEnrollmentSessionStoreCorruptData
      loadMfaEnrollmentSession (buildRuntimePostgresMfaEnrollmentSessionStoreWithRunner (\_ _ _ -> pure (Right [["account_01", "invalid-csrf", "100", "200"]])) databaseConfig) testSessionId
        `shouldReturnEqual` Left MfaEnrollmentSessionStoreCorruptData
      loadMfaEnrollmentSession (buildRuntimePostgresMfaEnrollmentSessionStoreWithRunner (\_ _ _ -> pure (Right [["account_01", csrfTokenValue, "100"]])) databaseConfig) testSessionId
        `shouldReturnEqual` Left MfaEnrollmentSessionStoreCorruptData
      invalidateMfaEnrollmentSession unavailableStore testSessionId 300 `shouldReturnEqual` Left MfaEnrollmentSessionStoreUnavailable
      invalidateMfaEnrollmentSession declinedStore testSessionId 300 `shouldReturnEqual` Right False
      invalidateMfaEnrollmentSession wrongSessionStore testSessionId 300 `shouldReturnEqual` Left MfaEnrollmentSessionStoreCorruptData
      loadMfaEnrollmentSession (buildRuntimePostgresMfaEnrollmentSessionStoreWithRunner (\runnerDatabaseConfig _ _ -> evaluate (databaseHost runnerDatabaseConfig) >> pure (Right [["wrong", "shape"]])) databaseConfig) testSessionId
        `shouldReturnEqual` Left MfaEnrollmentSessionStoreCorruptData

    it "covers invalidation-specific persistence outcomes" $ do
      let store result =
            buildRuntimePostgresMfaEnrollmentSessionStoreWithRunner
              ( \runnerDatabaseConfig query _ -> do
                  _ <- evaluate (databaseHost runnerDatabaseConfig)
                  if "UPDATE web_api.mfa_enrollment_sessions" `Text.isInfixOf` query
                    then pure result
                    else pure (Left "unexpected query")
              )
              databaseConfig
      invalidateMfaEnrollmentSession (store (Left "database unavailable")) testSessionId 300 `shouldReturnEqual` Left MfaEnrollmentSessionStoreUnavailable
      invalidateMfaEnrollmentSession (store (Right [])) testSessionId 300 `shouldReturnEqual` Right False
      invalidateMfaEnrollmentSession (store (Right [["other-session"]])) testSessionId 300 `shouldReturnEqual` Left MfaEnrollmentSessionStoreCorruptData

    it "executes the native libpq MFA-enrollment-session adapter against a migrated PostgreSQL database" $ do
      ensureDefaultPostgresAvailable
      runPostgresMigrationsForRuntime defaultMigrationPostgresConfig defaultRealPostgresConfig `shouldReturn` Right ()
      unknownSessionId <- generateSessionId
      pool <- newPostgresPool (databasePoolCapacity defaultRealPostgresConfig) defaultRealPostgresConfig
      loadMfaEnrollmentSession (buildRuntimePostgresMfaEnrollmentSessionStore pool) unknownSessionId `shouldReturnEqual` Right Nothing

    it "keeps the MFA-enrollment-session errors comparable without exposing persistence details" $ do
      MfaEnrollmentSessionStoreUnavailable /= MfaEnrollmentSessionStoreCorruptData `shouldBe` True

    it "uses a distinct, short-lived cookie separate from the ordinary login session" $ do
      sessionCookieNameText (sessionCookieName mfaEnrollmentSessionCookiePolicy) `shouldBe` "__Host-harch-mfa-enrollment"

  describe "MFA-enrollment-session issuance" $ do
    it "generates and persists an opaque session after email or password verification succeeds" $ do
      savedSessionReference <- newIORef Nothing
      let store =
            MfaEnrollmentSessionStore
              { saveMfaEnrollmentSession = \session -> writeIORef savedSessionReference (Just session) >> pure (Right True),
                loadMfaEnrollmentSession = \_ -> pure (Right Nothing),
                invalidateMfaEnrollmentSession = \_ _ -> pure (Right False)
              }
      issuedSessionResult <- issueMfaEnrollmentSession store accountId 100
      case issuedSessionResult of
        Left _ -> expectationFailure "expected MFA-enrollment session issuance to succeed"
        Right issuedSession -> do
          savedSession <- readIORef savedSessionReference
          expectAll
            ( (sessionPrincipal issuedSession `shouldBe` accountId)
                :| [ sessionIssuedAtNanoseconds issuedSession `shouldBe` 100,
                     sessionExpiresAtNanoseconds issuedSession `shouldBe` 600000000100,
                     savedSession `shouldBe` Just issuedSession
                   ]
            )

    it "preserves storage failures and refuses collisions or overflowing expirations" $ do
      let store result =
            MfaEnrollmentSessionStore
              { saveMfaEnrollmentSession = \_ -> pure result,
                loadMfaEnrollmentSession = \_ -> pure (Right Nothing),
                invalidateMfaEnrollmentSession = \_ _ -> pure (Right False)
              }
      issueMfaEnrollmentSession (store (Left MfaEnrollmentSessionStoreUnavailable)) accountId 100 `shouldReturnEqual` Left MfaEnrollmentSessionStoreUnavailable
      issueMfaEnrollmentSession (store (Right False)) accountId 100 `shouldReturnEqual` Left MfaEnrollmentSessionStoreCorruptData
      issueMfaEnrollmentSession (store (Right True)) accountId (maxBound :: Word64) `shouldReturnEqual` Left MfaEnrollmentSessionStoreCorruptData

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
      databasePassword = "password",
      databaseConnectTimeoutSeconds = 10,
      databasePoolCapacity = 10
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
