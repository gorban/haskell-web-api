{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (replicateM, replicateM_, unless)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import HarchWeb.Account (accountIdText, generateAccountId)
import HarchWeb.LoginProtection (LoginProtectionPolicy (..), defaultLoginProtectionPolicy, loginProtectionLockoutNanoseconds, loginProtectionWindowNanoseconds)
import HarchWeb.Time (unixTimeNanoseconds)
import TestSupport.RealPostgres (defaultMigrationPostgresConfig, defaultRealPostgresConfig, ensureDefaultPostgresAvailable)
import Unit.WebApi.TestSupport (requiredDatabasePoolCapacity)
import WebApi.Config (DatabaseConfig (..), DatabaseTransportSecurity (..))
import WebApi.Login (LoginAttemptAdmission (..), LoginAttemptReservation (..), LoginAttemptStore (..), LoginAttemptStoreError (..))
import WebApi.Postgres.Testing (buildRuntimePostgresLoginAttemptStore, buildRuntimePostgresLoginAttemptStoreWithRunner, buildRuntimePostgresLoginAttemptStoreWithRunnerAndStoragePolicy, buildRuntimePostgresLoginAttemptStoreWithStoragePolicy, mkLoginAttemptStoragePolicy, newPostgresPool, runPostgresMigrationsForRuntime, runRuntimeRowsQuery)

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
        [(firstReserveQuery, firstReserveParameters), (secondReserveQuery, secondReserveParameters), (settleQuery, [settleReservation]), (cancelQuery, [cancelReservation])] -> do
          "SELECT outcome, value FROM web_api.reserve_login_attempt" `Text.isInfixOf` firstReserveQuery `shouldBe` True
          firstReserveParameters `shouldBe` ["email:person@example.test", "0", "0", "500", "5", "900000000000", "100000"]
          "SELECT outcome, value FROM web_api.reserve_login_attempt" `Text.isInfixOf` secondReserveQuery `shouldBe` True
          secondReserveParameters `shouldBe` ["email:person@example.test", "500", "500", "900000000500", "5", "900000000000", "100000"]
          "DELETE FROM web_api.login_attempts" `Text.isInfixOf` settleQuery `shouldBe` True
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

    it "rejects an oversized key before querying PostgreSQL and decodes a full bounded store" $ do
      queryCountReference <- newIORef (0 :: Int)
      let runner _ _ _ = modifyIORef' queryCountReference (+ 1) >> pure (Right [["storage-exhausted", ""]])
          store = buildRuntimePostgresLoginAttemptStoreWithRunner runner databaseConfig
          keyTooLongStore = buildRuntimePostgresLoginAttemptStoreWithRunner (\_ _ _ -> pure (Right [["key-too-long", ""]])) databaseConfig
      reserveLoginAttempt store (Text.replicate 261 "a") defaultLoginProtectionPolicy 500
        `shouldReturnEqual` Left (LoginAttemptStoreUnavailable "login-attempt key exceeds storage limit")
      readIORef queryCountReference `shouldReturn` 0
      reserveLoginAttempt store "key" defaultLoginProtectionPolicy 500
        `shouldReturnEqual` Left (LoginAttemptStoreUnavailable "login-attempt storage capacity exhausted")
      reserveLoginAttempt keyTooLongStore "key" defaultLoginProtectionPolicy 500
        `shouldReturnEqual` Left (LoginAttemptStoreUnavailable "login-attempt key exceeds storage limit")

    it "rejects invalid storage policies and passes a bounded policy to its reservation function" $ do
      expectNothing "zero storage capacity" (mkLoginAttemptStoragePolicy 0 1)
      expectNothing "zero retention" (mkLoginAttemptStoragePolicy 1 0)
      queryParametersReference <- newIORef []
      let storagePolicy = fromMaybe (error "expected valid storage policy") (mkLoginAttemptStoragePolicy 2 100)
          runner _ _ parameters = modifyIORef' queryParametersReference (parameters :) >> pure (Right [["reserved", "42"]])
          store = buildRuntimePostgresLoginAttemptStoreWithRunnerAndStoragePolicy storagePolicy runner databaseConfig
      reserveLoginAttempt store "key" defaultLoginProtectionPolicy 500
        `shouldReturnEqual` Right (LoginAttemptReserved (LoginAttemptReservation "42"))
      readIORef queryParametersReference `shouldReturn` [["key", "0", "400", "500", "5", "900000000000", "2"]]

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

    it "bounds all retained rows and reclaims an abandoned reservation after its retention window" $ do
      ensureDefaultPostgresAvailable
      runPostgresMigrationsForRuntime defaultMigrationPostgresConfig defaultRealPostgresConfig
        `shouldReturn` Right ()
      cleared <- runRuntimeRowsQuery defaultRealPostgresConfig "DELETE FROM web_api.login_attempts RETURNING attempt_id::TEXT;"
      cleared `shouldSatisfy` either (const False) (const True)
      pool <- newPostgresPool (databasePoolCapacity defaultRealPostgresConfig) defaultRealPostgresConfig
      let storagePolicy = fromMaybe (error "expected valid storage policy") (mkLoginAttemptStoragePolicy 1 100)
          store = buildRuntimePostgresLoginAttemptStoreWithStoragePolicy storagePolicy pool
      firstReservation <- reserveLoginAttempt store "first" defaultLoginProtectionPolicy 500
      case firstReservation of
        Right (LoginAttemptReserved reservation) -> settleLoginAttempt store reservation False `shouldReturnEqual` Right ()
        _ -> expectationFailure "expected a first reservation"
      reserveLoginAttempt store "second" defaultLoginProtectionPolicy 500
        `shouldReturnEqual` Left (LoginAttemptStoreUnavailable "login-attempt storage capacity exhausted")
      reclaimedReservation <- reserveLoginAttempt store "second" defaultLoginProtectionPolicy 601
      case reclaimedReservation of
        Right (LoginAttemptReserved reservation) -> cancelLoginAttempt store reservation `shouldReturnEqual` Right ()
        _ -> expectationFailure "expected expired retention to free the storage budget"

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

expectNothing :: String -> Maybe value -> Expectation
expectNothing label value =
  case value of
    Nothing -> pure ()
    Just _ -> expectationFailure (label <> " should be rejected")

databaseConfig :: DatabaseConfig
databaseConfig =
  DatabaseConfig
    { databaseHost = "127.0.0.1",
      databasePort = 5432,
      databaseName = "web_api_test",
      databaseUser = "web_api_runtime",
      databasePassword = "password",
      databaseConnectTimeoutSeconds = 10,
      databasePoolCapacity = requiredDatabasePoolCapacity 10,
      databaseTransportSecurity = DatabaseTransportLibpqDefault
    }
