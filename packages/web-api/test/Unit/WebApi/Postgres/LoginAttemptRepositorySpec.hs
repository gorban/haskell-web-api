{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (replicateM, replicateM_, unless)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text qualified as Text
import HarchWeb.Account (accountIdText, generateAccountId)
import HarchWeb.LoginProtection (LoginProtectionPolicy (..), defaultLoginProtectionPolicy, loginProtectionLockoutNanoseconds, loginProtectionWindowNanoseconds)
import HarchWeb.Time (unixTimeNanoseconds)
import TestSupport.RealPostgres (defaultMigrationPostgresConfig, defaultRealPostgresConfig, ensureDefaultPostgresAvailable)
import Unit.WebApi.TestSupport (requiredDatabasePoolCapacity)
import WebApi.Config (DatabaseConfig (..))
import WebApi.Login (LoginAttemptAdmission (..), LoginAttemptReservation (..), LoginAttemptStore (..), LoginAttemptStoreError (..))
import WebApi.Postgres.Testing (buildRuntimePostgresLoginAttemptStore, buildRuntimePostgresLoginAttemptStoreWithRunner, newPostgresPool, runPostgresMigrationsForRuntime)

spec = do
  describe "runtime PostgreSQL login-attempt persistence" $ do
    it "uses one bound-parameter query to reserve, then settles or cancels its opaque reservation" $ do
      queriesReference <- newIORef []
      let runner _databaseConfig query parameters = do
            modifyIORef' queriesReference ((query, parameters) :)
            pure $
              if "SELECT outcome, value FROM web_api.reserve_login_attempt" `Text.isInfixOf` query
                then Right [["reserved", "42"]]
                else
                  if "UPDATE web_api.login_attempts" `Text.isInfixOf` query || "DELETE FROM web_api.login_attempts" `Text.isInfixOf` query
                    then Right [["42"]]
                    else Left "unexpected query"
          store = buildRuntimePostgresLoginAttemptStoreWithRunner runner databaseConfig
          reservation = LoginAttemptReservation "42"
      reserveLoginAttempt store "email:person@example.test" defaultLoginProtectionPolicy 500
        `shouldReturnEqual` Right (LoginAttemptReserved reservation)
      reserveLoginAttempt store "email:person@example.test" defaultLoginProtectionPolicy (unixTimeNanoseconds (loginProtectionWindowNanoseconds defaultLoginProtectionPolicy + 500))
        `shouldReturnEqual` Right (LoginAttemptReserved reservation)
      settleLoginAttempt store reservation True `shouldReturnEqual` Right ()
      cancelLoginAttempt store reservation `shouldReturnEqual` Right ()
      recordedQueries <- reverse <$> readIORef queriesReference
      case recordedQueries of
        [(firstReserveQuery, firstReserveParameters), (secondReserveQuery, secondReserveParameters), (settleQuery, [settleReservation, "true"]), (cancelQuery, [cancelReservation])] -> do
          "SELECT outcome, value FROM web_api.reserve_login_attempt" `Text.isInfixOf` firstReserveQuery `shouldBe` True
          firstReserveParameters `shouldBe` ["email:person@example.test", "0", "500", "5", "900000000000"]
          "SELECT outcome, value FROM web_api.reserve_login_attempt" `Text.isInfixOf` secondReserveQuery `shouldBe` True
          secondReserveParameters `shouldBe` ["email:person@example.test", "500", "900000000500", "5", "900000000000"]
          "UPDATE web_api.login_attempts" `Text.isInfixOf` settleQuery `shouldBe` True
          settleReservation `shouldBe` "42"
          "DELETE FROM web_api.login_attempts" `Text.isInfixOf` cancelQuery `shouldBe` True
          cancelReservation `shouldBe` "42"
        _ -> expectationFailure "expected two reserve queries followed by settlement and cancellation"

    it "preserves unavailable and corrupt database outcomes" $ do
      let unavailableStore = buildRuntimePostgresLoginAttemptStoreWithRunner (\_ _ _ -> pure (Left "database unavailable")) databaseConfig
          malformedAdmissionStore = buildRuntimePostgresLoginAttemptStoreWithRunner (\_ _ _ -> pure (Right [["unexpected"]])) databaseConfig
          malformedLockoutStore = buildRuntimePostgresLoginAttemptStoreWithRunner (\_ _ _ -> pure (Right [["throttled", "not-a-nanosecond"]])) databaseConfig
          malformedSettlementStore = buildRuntimePostgresLoginAttemptStoreWithRunner (\_ _ _ -> pure (Right [])) databaseConfig
      reserveLoginAttempt unavailableStore "key" defaultLoginProtectionPolicy 500 `shouldReturnEqual` Left (LoginAttemptStoreUnavailable "database unavailable")
      reserveLoginAttempt malformedAdmissionStore "key" defaultLoginProtectionPolicy 500
        `shouldReturnEqual` Left (LoginAttemptStoreCorruptData "unexpected login-attempt admission result: [[\"unexpected\"]]")
      reserveLoginAttempt malformedLockoutStore "key" defaultLoginProtectionPolicy 500
        `shouldReturnEqual` Left (LoginAttemptStoreCorruptData "unexpected login-attempt admission result: [[\"throttled\",\"not-a-nanosecond\"]]")
      settleLoginAttempt malformedSettlementStore (LoginAttemptReservation "42") False
        `shouldReturnEqual` Left (LoginAttemptStoreCorruptData "unexpected login-attempt settlement result: []")

    it "keeps login-attempt errors comparable" $ do
      (LoginAttemptStoreUnavailable "same" == LoginAttemptStoreUnavailable "same") `shouldBe` True
      (LoginAttemptStoreUnavailable "same" /= LoginAttemptStoreCorruptData "same") `shouldBe` True

    it "executes the native libpq login-attempt adapter against a migrated PostgreSQL database" $ do
      ensureDefaultPostgresAvailable
      runPostgresMigrationsForRuntime defaultMigrationPostgresConfig defaultRealPostgresConfig
        `shouldReturn` Right ()
      attemptKey <- accountIdText <$> generateAccountId
      pool <- newPostgresPool (databasePoolCapacity defaultRealPostgresConfig) defaultRealPostgresConfig
      let store = buildRuntimePostgresLoginAttemptStore pool
      reserved <- reserveLoginAttempt store attemptKey defaultLoginProtectionPolicy 500
      case reserved of
        Right (LoginAttemptReserved reservation) -> settleLoginAttempt store reservation False `shouldReturnEqual` Right ()
        _ -> expectationFailure "expected a real PostgreSQL attempt reservation"
      cancelled <- reserveLoginAttempt store attemptKey defaultLoginProtectionPolicy 600
      case cancelled of
        Right (LoginAttemptReserved reservation) -> cancelLoginAttempt store reservation `shouldReturnEqual` Right ()
        _ -> expectationFailure "expected a second real PostgreSQL attempt reservation"

    it "serializes concurrent admission for one key before credential work" $ do
      ensureDefaultPostgresAvailable
      runPostgresMigrationsForRuntime defaultMigrationPostgresConfig defaultRealPostgresConfig
        `shouldReturn` Right ()
      attemptKey <- accountIdText <$> generateAccountId
      pool <- newPostgresPool (databasePoolCapacity defaultRealPostgresConfig) defaultRealPostgresConfig
      let store = buildRuntimePostgresLoginAttemptStore pool
          policy = LoginProtectionPolicy 1 (loginProtectionWindowNanoseconds defaultLoginProtectionPolicy) (loginProtectionLockoutNanoseconds defaultLoginProtectionPolicy)
          reserveResult = reserveLoginAttempt store attemptKey policy 500
      resultsReference <- newEmptyMVar
      replicateM_ 2 (forkIO (reserveResult >>= putMVar resultsReference))
      results <- replicateM 2 (takeMVar resultsReference)
      let reservations = [reservation | Right (LoginAttemptReserved reservation) <- results]
          throttleEnds = [lockoutEndsAt | Right (LoginAttemptThrottled lockoutEndsAt) <- results]
      length reservations `shouldBe` 1
      throttleEnds `shouldBe` [500 + fromIntegral (loginProtectionLockoutNanoseconds policy)]
      case reservations of
        [reservation] -> settleLoginAttempt store reservation False `shouldReturnEqual` Right ()
        _ -> expectationFailure "expected exactly one reservation"

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
      databasePoolCapacity = requiredDatabasePoolCapacity 10
    }
