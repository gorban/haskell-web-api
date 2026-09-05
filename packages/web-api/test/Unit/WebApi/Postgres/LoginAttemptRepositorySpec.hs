{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (replicateM, replicateM_, unless)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Word (Word8)
import HarchWeb qualified
import HarchWeb.Account (AccountId, generateAccountId)
import HarchWeb.LoginProtection (LoginProtectionPolicy (..), defaultLoginProtectionPolicy, loginProtectionLockoutNanoseconds, loginProtectionWindowNanoseconds)
import HarchWeb.Time (unixTimeNanoseconds)
import Network.Socket qualified as Socket
import Network.Wai qualified as Wai
import TestSupport.RealPostgres (defaultMigrationPostgresConfig, defaultRealPostgresConfig, ensureDefaultPostgresAvailable)
import Unit.WebApi.TestSupport (requiredDatabasePoolCapacity)
import WebApi.Config (AppConfig (..), DatabaseConfig (..), DatabaseTransportSecurity (..), defaultAppConfig)
import WebApi.Login (LoginAttemptAdmission (..), LoginAttemptBudget (..), LoginAttemptBudgets, LoginAttemptReservation (..), LoginAttemptScope (..), LoginAttemptStore (..), LoginAttemptStoreError (..), LoginPrincipal (..), LoginStage (..), mkLoginAttemptBudgets)
import WebApi.Postgres.Testing (buildRuntimePostgresLoginAttemptStore, buildRuntimePostgresLoginAttemptStoreWithRunner, buildRuntimePostgresLoginAttemptStoreWithRunnerAndStoragePolicy, buildRuntimePostgresLoginAttemptStoreWithStoragePolicy, mkLoginAttemptStoragePolicy, newPostgresPool, runPostgresMigrationsForRuntime, runRuntimeRowsQuery)

spec = do
  describe "runtime PostgreSQL login-attempt persistence" $ do
    it "uses one bound-parameter query to reserve, then settles or cancels its opaque reservation" $ do
      queriesReference <- newIORef []
      let runner _databaseConfig query parameters = do
            modifyIORef' queriesReference ((query, parameters) :)
            pure $
              if "SELECT outcome, value FROM web_api.reserve_login_attempt_group" `Text.isInfixOf` query
                then Right [["reserved", "42"]]
                else
                  if "UPDATE web_api.login_attempt_groups" `Text.isInfixOf` query || "DELETE FROM web_api.login_attempt_groups" `Text.isInfixOf` query
                    then Right [["42"]]
                    else Left "unexpected query"
          store = buildRuntimePostgresLoginAttemptStoreWithRunner runner databaseConfig
          reservation = LoginAttemptReservation "42"
      reserveLoginAttempt store (peerBudgets defaultLoginProtectionPolicy) 500
        `shouldReturnEqual` Right (LoginAttemptReserved reservation)
      reserveLoginAttempt store (peerBudgets defaultLoginProtectionPolicy) (unixTimeNanoseconds (loginProtectionWindowNanoseconds defaultLoginProtectionPolicy + 500))
        `shouldReturnEqual` Right (LoginAttemptReserved reservation)
      settleLoginAttempt store reservation True `shouldReturnEqual` Right ()
      cancelLoginAttempt store reservation `shouldReturnEqual` Right ()
      recordedQueries <- reverse <$> readIORef queriesReference
      case recordedQueries of
        [(firstReserveQuery, firstReserveParameters), (secondReserveQuery, secondReserveParameters), (settleQuery, [settleReservation]), (cancelQuery, [cancelReservation])] -> do
          "SELECT outcome, value FROM web_api.reserve_login_attempt_group" `Text.isInfixOf` firstReserveQuery `shouldBe` True
          firstReserveParameters `shouldBe` ["[{\"key\":\"peer:127.0.0.1\",\"lockout\":900000000000,\"maximum\":5,\"window\":900000000000}]", "0", "500", "100000"]
          "SELECT outcome, value FROM web_api.reserve_login_attempt_group" `Text.isInfixOf` secondReserveQuery `shouldBe` True
          secondReserveParameters `shouldBe` ["[{\"key\":\"peer:127.0.0.1\",\"lockout\":900000000000,\"maximum\":5,\"window\":900000000000}]", "500", "900000000500", "100000"]
          "DELETE FROM web_api.login_attempt_groups" `Text.isInfixOf` settleQuery `shouldBe` True
          settleReservation `shouldBe` "42"
          "DELETE FROM web_api.login_attempt_groups" `Text.isInfixOf` cancelQuery `shouldBe` True
          cancelReservation `shouldBe` "42"
        _ -> expectationFailure "expected two reserve queries followed by settlement and cancellation"

    it "encodes an opaque direct Unix peer as JSON data at the PostgreSQL boundary" $ do
      queryParametersReference <- newIORef []
      let unsafeUnixPeer =
            HarchWeb.requestClientAddress
              (requestPolicy defaultAppConfig)
              (Wai.defaultRequest {Wai.remoteHost = Socket.SockAddrUnix "peer\"\\"})
          runner _ _ parameters = modifyIORef' queryParametersReference (parameters :) >> pure (Right [["reserved", "42"]])
          store = buildRuntimePostgresLoginAttemptStoreWithRunner runner databaseConfig
      reserveLoginAttempt store (peerBudgetsWith unsafeUnixPeer defaultLoginProtectionPolicy) 500
        `shouldReturnEqual` Right (LoginAttemptReserved (LoginAttemptReservation "42"))
      recordedParameters <- readIORef queryParametersReference
      case recordedParameters of
        [[encodedBudgets, _, _, _]] -> do
          Text.isInfixOf "\\\"" encodedBudgets `shouldBe` True
          Text.isInfixOf "\\\\" encodedBudgets `shouldBe` True
        _ -> expectationFailure "expected one serialized reservation-group parameter"

    it "preserves unavailable and corrupt database outcomes" $ do
      let unavailableStore = buildRuntimePostgresLoginAttemptStoreWithRunner (\_ _ _ -> pure (Left "database unavailable")) databaseConfig
          malformedAdmissionStore = buildRuntimePostgresLoginAttemptStoreWithRunner (\_ _ _ -> pure (Right [["unexpected"]])) databaseConfig
          malformedLockoutStore = buildRuntimePostgresLoginAttemptStoreWithRunner (\_ _ _ -> pure (Right [["throttled", "not-a-nanosecond"]])) databaseConfig
          malformedSettlementStore = buildRuntimePostgresLoginAttemptStoreWithRunner (\_ _ _ -> pure (Right [])) databaseConfig
      reserveLoginAttempt unavailableStore (peerBudgets defaultLoginProtectionPolicy) 500 `shouldReturnEqual` Left (LoginAttemptStoreUnavailable "database unavailable")
      reserveLoginAttempt malformedAdmissionStore (peerBudgets defaultLoginProtectionPolicy) 500
        `shouldReturnEqual` Left (LoginAttemptStoreCorruptData "unexpected login-attempt admission result: row-count=1, column-counts=[1]")
      reserveLoginAttempt malformedLockoutStore (peerBudgets defaultLoginProtectionPolicy) 500
        `shouldReturnEqual` Left (LoginAttemptStoreCorruptData "unexpected login-attempt admission result: row-count=1, column-counts=[2]")
      settleLoginAttempt malformedSettlementStore (LoginAttemptReservation "42") False
        `shouldReturnEqual` Left (LoginAttemptStoreCorruptData "unexpected login-attempt settlement result: row-count=0, column-counts=[]")

    it "decodes bounded-store outcomes from the reservation-group function" $ do
      let runner _ _ _ = pure (Right [["storage-exhausted", ""]])
          store = buildRuntimePostgresLoginAttemptStoreWithRunner runner databaseConfig
          keyTooLongStore = buildRuntimePostgresLoginAttemptStoreWithRunner (\_ _ _ -> pure (Right [["key-too-long", ""]])) databaseConfig
      reserveLoginAttempt store (peerBudgets defaultLoginProtectionPolicy) 500
        `shouldReturnEqual` Left (LoginAttemptStoreUnavailable "login-attempt storage capacity exhausted")
      reserveLoginAttempt keyTooLongStore (peerBudgets defaultLoginProtectionPolicy) 500
        `shouldReturnEqual` Left (LoginAttemptStoreUnavailable "login-attempt key exceeds storage limit")

    it "rejects invalid storage policies and passes a bounded policy to its reservation function" $ do
      expectNothing "zero storage capacity" (mkLoginAttemptStoragePolicy 0 1)
      expectNothing "zero retention" (mkLoginAttemptStoragePolicy 1 0)
      queryParametersReference <- newIORef []
      let storagePolicy = fromMaybe (error "expected valid storage policy") (mkLoginAttemptStoragePolicy 2 100)
          runner _ _ parameters = modifyIORef' queryParametersReference (parameters :) >> pure (Right [["reserved", "42"]])
          store = buildRuntimePostgresLoginAttemptStoreWithRunnerAndStoragePolicy storagePolicy runner databaseConfig
      reserveLoginAttempt store (peerBudgets defaultLoginProtectionPolicy) 500
        `shouldReturnEqual` Right (LoginAttemptReserved (LoginAttemptReservation "42"))
      readIORef queryParametersReference `shouldReturn` [["[{\"key\":\"peer:127.0.0.1\",\"lockout\":900000000000,\"maximum\":5,\"window\":900000000000}]", "400", "500", "2"]]

    it "keeps login-attempt errors comparable" $ do
      (LoginAttemptStoreUnavailable "same" == LoginAttemptStoreUnavailable (Text.concat ["sa", "me"])) `shouldBe` True
      (LoginAttemptStoreUnavailable "same" /= LoginAttemptStoreCorruptData "same") `shouldBe` True

    it "executes the native libpq login-attempt adapter against a migrated PostgreSQL database" $ do
      ensureDefaultPostgresAvailable
      runPostgresMigrationsForRuntime defaultMigrationPostgresConfig defaultRealPostgresConfig
        `shouldReturn` Right ()
      accountId <- generateAccountId
      pool <- newPostgresPool (databasePoolCapacity defaultRealPostgresConfig) defaultRealPostgresConfig
      let store = buildRuntimePostgresLoginAttemptStore pool
      reserved <- reserveLoginAttempt store (accountBudgets accountId defaultLoginProtectionPolicy) 500
      case reserved of
        Right (LoginAttemptReserved reservation@(LoginAttemptReservation groupId)) -> do
          scopeRows <- runRuntimeRowsQuery defaultRealPostgresConfig ("SELECT count(*)::TEXT FROM web_api.login_attempts WHERE attempt_group_id = " <> groupId <> ";")
          scopeRows `shouldBe` Right ["2"]
          settleLoginAttempt store reservation False `shouldReturnEqual` Right ()
        _ -> expectationFailure "expected a real PostgreSQL attempt reservation"
      cancelled <- reserveLoginAttempt store (accountBudgets accountId defaultLoginProtectionPolicy) 600
      case cancelled of
        Right (LoginAttemptReserved reservation) -> cancelLoginAttempt store reservation `shouldReturnEqual` Right ()
        _ -> expectationFailure "expected a second real PostgreSQL attempt reservation"

    it "does not leave a principal-only reservation when the shared peer scope is throttled" $ do
      ensureDefaultPostgresAvailable
      runPostgresMigrationsForRuntime defaultMigrationPostgresConfig defaultRealPostgresConfig
        `shouldReturn` Right ()
      cleared <- runRuntimeRowsQuery defaultRealPostgresConfig "DELETE FROM web_api.login_attempt_groups RETURNING attempt_group_id::TEXT;"
      cleared `shouldSatisfy` either (const False) (const True)
      firstAccountId <- generateAccountId
      secondAccountId <- generateAccountId
      pool <- newPostgresPool (databasePoolCapacity defaultRealPostgresConfig) defaultRealPostgresConfig
      let policy = LoginProtectionPolicy 1 (loginProtectionWindowNanoseconds defaultLoginProtectionPolicy) (loginProtectionLockoutNanoseconds defaultLoginProtectionPolicy)
          store = buildRuntimePostgresLoginAttemptStore pool
      firstReservation <- reserveLoginAttempt store (accountBudgets firstAccountId policy) 500
      case firstReservation of
        Right (LoginAttemptReserved reservation) -> settleLoginAttempt store reservation False `shouldReturnEqual` Right ()
        _ -> expectationFailure "expected the first group reservation"
      reserveLoginAttempt store (accountBudgets secondAccountId policy) 500
        `shouldReturnEqual` Right (LoginAttemptThrottled (500 + fromIntegral (loginProtectionLockoutNanoseconds policy)))
      retainedGroups <- runRuntimeRowsQuery defaultRealPostgresConfig "SELECT count(*)::TEXT FROM web_api.login_attempt_groups;"
      retainedScopes <- runRuntimeRowsQuery defaultRealPostgresConfig "SELECT count(*)::TEXT FROM web_api.login_attempts;"
      retainedGroups `shouldBe` Right ["1"]
      retainedScopes `shouldBe` Right ["2"]

    it "does not leave a peer-only reservation when the known principal is throttled across peers" $ do
      ensureDefaultPostgresAvailable
      runPostgresMigrationsForRuntime defaultMigrationPostgresConfig defaultRealPostgresConfig
        `shouldReturn` Right ()
      cleared <- runRuntimeRowsQuery defaultRealPostgresConfig "DELETE FROM web_api.login_attempt_groups RETURNING attempt_group_id::TEXT;"
      cleared `shouldSatisfy` either (const False) (const True)
      accountId <- generateAccountId
      pool <- newPostgresPool (databasePoolCapacity defaultRealPostgresConfig) defaultRealPostgresConfig
      let policy = LoginProtectionPolicy 1 (loginProtectionWindowNanoseconds defaultLoginProtectionPolicy) (loginProtectionLockoutNanoseconds defaultLoginProtectionPolicy)
          store = buildRuntimePostgresLoginAttemptStore pool
      firstReservation <- reserveLoginAttempt store (accountBudgetsWithPeer accountId policy firstTestPeer) 500
      case firstReservation of
        Right (LoginAttemptReserved reservation) -> settleLoginAttempt store reservation False `shouldReturnEqual` Right ()
        _ -> expectationFailure "expected the first group reservation"
      reserveLoginAttempt store (accountBudgetsWithPeer accountId policy secondTestPeer) 500
        `shouldReturnEqual` Right (LoginAttemptThrottled (500 + fromIntegral (loginProtectionLockoutNanoseconds policy)))
      retainedGroups <- runRuntimeRowsQuery defaultRealPostgresConfig "SELECT count(*)::TEXT FROM web_api.login_attempt_groups;"
      retainedScopes <- runRuntimeRowsQuery defaultRealPostgresConfig "SELECT count(*)::TEXT FROM web_api.login_attempts;"
      retainedGroups `shouldBe` Right ["1"]
      retainedScopes `shouldBe` Right ["2"]

    it "rejects malformed reservation groups before they can create a child scope" $ do
      ensureDefaultPostgresAvailable
      runPostgresMigrationsForRuntime defaultMigrationPostgresConfig defaultRealPostgresConfig
        `shouldReturn` Right ()
      cleared <- runRuntimeRowsQuery defaultRealPostgresConfig "DELETE FROM web_api.login_attempt_groups RETURNING attempt_group_id::TEXT;"
      cleared `shouldSatisfy` either (const False) (const True)
      malformedOutcome <- runRuntimeRowsQuery defaultRealPostgresConfig "SELECT outcome || ':' || value FROM web_api.reserve_login_attempt_group('[]'::JSONB, 0, 500, 100000);"
      retainedScopes <- runRuntimeRowsQuery defaultRealPostgresConfig "SELECT count(*)::TEXT FROM web_api.login_attempts;"
      malformedOutcome `shouldBe` Right ["invalid-budget:"]
      retainedScopes `shouldBe` Right ["0"]

    it "bounds all retained rows and reclaims an abandoned reservation after its retention window" $ do
      ensureDefaultPostgresAvailable
      runPostgresMigrationsForRuntime defaultMigrationPostgresConfig defaultRealPostgresConfig
        `shouldReturn` Right ()
      cleared <- runRuntimeRowsQuery defaultRealPostgresConfig "DELETE FROM web_api.login_attempt_groups RETURNING attempt_group_id::TEXT;"
      cleared `shouldSatisfy` either (const False) (const True)
      pool <- newPostgresPool (databasePoolCapacity defaultRealPostgresConfig) defaultRealPostgresConfig
      let storagePolicy = fromMaybe (error "expected valid storage policy") (mkLoginAttemptStoragePolicy 1 100)
          store = buildRuntimePostgresLoginAttemptStoreWithStoragePolicy storagePolicy pool
      firstReservation <- reserveLoginAttempt store (peerBudgets defaultLoginProtectionPolicy) 500
      case firstReservation of
        Right (LoginAttemptReserved reservation) -> settleLoginAttempt store reservation False `shouldReturnEqual` Right ()
        _ -> expectationFailure "expected a first reservation"
      reserveLoginAttempt store (peerBudgets defaultLoginProtectionPolicy) 500
        `shouldReturnEqual` Left (LoginAttemptStoreUnavailable "login-attempt storage capacity exhausted")
      reclaimedReservation <- reserveLoginAttempt store (peerBudgets defaultLoginProtectionPolicy) 601
      case reclaimedReservation of
        Right (LoginAttemptReserved reservation) -> cancelLoginAttempt store reservation `shouldReturnEqual` Right ()
        _ -> expectationFailure "expected expired retention to free the storage budget"

    it "serializes concurrent admission for one key before credential work" $ do
      ensureDefaultPostgresAvailable
      runPostgresMigrationsForRuntime defaultMigrationPostgresConfig defaultRealPostgresConfig
        `shouldReturn` Right ()
      accountId <- generateAccountId
      pool <- newPostgresPool (databasePoolCapacity defaultRealPostgresConfig) defaultRealPostgresConfig
      let store = buildRuntimePostgresLoginAttemptStore pool
          policy = LoginProtectionPolicy 1 (loginProtectionWindowNanoseconds defaultLoginProtectionPolicy) (loginProtectionLockoutNanoseconds defaultLoginProtectionPolicy)
          reserveResult = reserveLoginAttempt store (accountBudgets accountId policy) 500
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

peerBudgets :: LoginProtectionPolicy -> LoginAttemptBudgets
peerBudgets = peerBudgetsWith HarchWeb.defaultClientAddress

peerBudgetsWith :: HarchWeb.ClientAddress -> LoginProtectionPolicy -> LoginAttemptBudgets
peerBudgetsWith clientAddress policy =
  mkLoginAttemptBudgets
    (LoginAttemptBudget (LoginPeerScope clientAddress) policy :| [])

accountBudgets :: AccountId -> LoginProtectionPolicy -> LoginAttemptBudgets
accountBudgets accountId policy =
  accountBudgetsWithPeer accountId policy HarchWeb.defaultClientAddress

accountBudgetsWithPeer :: AccountId -> LoginProtectionPolicy -> HarchWeb.ClientAddress -> LoginAttemptBudgets
accountBudgetsWithPeer accountId policy clientAddress =
  mkLoginAttemptBudgets
    ( LoginAttemptBudget (LoginPrincipalScope (KnownAccountPrincipal accountId PasswordLoginStage)) policy
        :| [LoginAttemptBudget (LoginPeerScope clientAddress) policy]
    )

firstTestPeer :: HarchWeb.ClientAddress
firstTestPeer = testClientAddress (203, 0, 113, 10)

secondTestPeer :: HarchWeb.ClientAddress
secondTestPeer = testClientAddress (203, 0, 113, 11)

testClientAddress :: (Word8, Word8, Word8, Word8) -> HarchWeb.ClientAddress
testClientAddress address =
  HarchWeb.requestClientAddress
    (requestPolicy defaultAppConfig)
    (Wai.defaultRequest {Wai.remoteHost = Socket.SockAddrInet 4123 (Socket.tupleToHostAddress address)})
