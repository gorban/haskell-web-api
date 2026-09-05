{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay, throwTo)
import Control.Exception (AsyncException (ThreadKilled), SomeException, displayException, throwIO, try)
import Data.Either (isLeft)
import Data.IORef (atomicModifyIORef', newIORef)
import Database.PostgreSQL.LibPQ qualified as LibPQ
import Network.Socket (Family (AF_INET), SockAddr (SockAddrInet), SocketType (Stream), bind, close, defaultProtocol, getSocketName, listen, socket, tupleToHostAddress)
import System.Timeout (timeout)
import TestSupport.RealPostgres (defaultRealPostgresConfig, ensureDefaultPostgresAvailable)
import Unit.WebApi.TestSupport (requiredDatabasePoolCapacity)
import WebApi.Config (DatabaseConfig (..))
import WebApi.Postgres.Testing (closePostgresPool, newPostgresPool, newPostgresPoolWithConnector, runPooledRowsQuery, runtimeConnectionString, withPooledConnection)

spec = describe "WebApi.Postgres.Pool" $ do
  it "reuses the same connection across sequential acquire/release rather than opening a fresh one each time" $ do
    ensureDefaultPostgresAvailable
    pool <- newPostgresPool (requiredDatabasePoolCapacity 5) defaultRealPostgresConfig
    firstProcessId <- withPooledConnection pool LibPQ.backendPID
    secondProcessId <- withPooledConnection pool LibPQ.backendPID
    secondProcessId `shouldBe` firstProcessId

  it "blocks a second acquirer at capacity and releases it only once the first connection is returned" $ do
    ensureDefaultPostgresAvailable
    pool <- newPostgresPool (requiredDatabasePoolCapacity 1) defaultRealPostgresConfig
    acquiredSecond <- newEmptyMVar
    releaseFirst <- newEmptyMVar
    _ <-
      forkIO $
        withPooledConnection pool $ \_firstConnection ->
          takeMVar releaseFirst
    threadDelay 20000
    _ <-
      forkIO $
        withPooledConnection pool $ \_secondConnection ->
          putMVar acquiredSecond ()
    threadDelay 20000
    stillBlocked <- timeout 20000 (takeMVar acquiredSecond)
    stillBlocked `shouldBe` Nothing
    putMVar releaseFirst ()
    releasedInTime <- timeout 2000000 (takeMVar acquiredSecond)
    releasedInTime `shouldBe` Just ()

  it "terminally closes idle connections and rejects later leases" $ do
    ensureDefaultPostgresAvailable
    pool <- newPostgresPool (requiredDatabasePoolCapacity 1) defaultRealPostgresConfig
    _ <- runPooledRowsQuery pool "SELECT 1;"
    closePostgresPool pool
    afterCloseResult <- try (withPooledConnection pool (const (pure ()))) :: IO (Either IOError ())
    assertPoolClosed afterCloseResult
    closePostgresPool pool

  it "still returns the connection to the pool when the wrapped action throws" $ do
    ensureDefaultPostgresAvailable
    pool <- newPostgresPool (requiredDatabasePoolCapacity 1) defaultRealPostgresConfig
    firstAttempt <- try (withPooledConnection pool (const (throwIO (userError "boom")))) :: IO (Either SomeException ())
    isLeft firstAttempt `shouldBe` True
    secondResult <- runPooledRowsQuery pool "SELECT 1;"
    secondResult `shouldBe` Right ["1"]

  it "surfaces a connection failure as a query error and discards the broken connection instead of pooling it" $
    withUnusedLoopbackPort $ \unreachablePort -> do
      pool <- newPostgresPool (requiredDatabasePoolCapacity 1) (defaultRealPostgresConfig {databasePort = unreachablePort})
      firstResult <- runPooledRowsQuery pool "SELECT 1;"
      secondResult <- runPooledRowsQuery pool "SELECT 1;"
      isLeft firstResult `shouldBe` True
      isLeft secondResult `shouldBe` True

  it "returns a reserved slot when the connector throws before making a connection" $ do
    ensureDefaultPostgresAvailable
    attempts <- newIORef (0 :: Int)
    let connector = do
          attempt <- atomicModifyIORef' attempts (\count -> (count + 1, count))
          if attempt == 0
            then throwIO (userError "connector failed")
            else LibPQ.connectdb (runtimeConnectionString defaultRealPostgresConfig)
    pool <- newPostgresPoolWithConnector (requiredDatabasePoolCapacity 1) connector
    firstAttempt <- try (withPooledConnection pool (const (pure ()))) :: IO (Either SomeException ())
    isLeft firstAttempt `shouldBe` True
    secondResult <- timeout 2000000 (runPooledRowsQuery pool "SELECT 1;")
    secondResult `shouldBe` Just (Right ["1"])

  it "returns a reserved slot when cancellation interrupts a connector" $ do
    ensureDefaultPostgresAvailable
    attempts <- newIORef (0 :: Int)
    connectorStarted <- newEmptyMVar
    waitForCancellation <- newEmptyMVar
    let connector = do
          attempt <- atomicModifyIORef' attempts (\count -> (count + 1, count))
          if attempt == 0
            then putMVar connectorStarted () >> takeMVar waitForCancellation
            else LibPQ.connectdb (runtimeConnectionString defaultRealPostgresConfig)
    pool <- newPostgresPoolWithConnector (requiredDatabasePoolCapacity 1) connector
    cancellationResult <- newEmptyMVar
    connectorThread <-
      forkIO $ do
        result <- try (withPooledConnection pool (const (pure ()))) :: IO (Either SomeException ())
        putMVar cancellationResult result
    takeMVar connectorStarted
    throwTo connectorThread ThreadKilled
    cancelled <- timeout 2000000 (takeMVar cancellationResult)
    maybe False isLeft cancelled `shouldBe` True
    secondResult <- timeout 2000000 (runPooledRowsQuery pool "SELECT 1;")
    secondResult `shouldBe` Just (Right ["1"])

  it "waits for an in-flight lease to finish before terminal shutdown returns" $ do
    ensureDefaultPostgresAvailable
    pool <- newPostgresPool (requiredDatabasePoolCapacity 1) defaultRealPostgresConfig
    leaseStarted <- newEmptyMVar
    releaseLease <- newEmptyMVar
    _ <-
      forkIO $
        withPooledConnection pool $ \_connection -> do
          putMVar leaseStarted ()
          takeMVar releaseLease
    takeMVar leaseStarted
    closeCompleted <- newEmptyMVar
    _ <- forkIO (closePostgresPool pool >> putMVar closeCompleted ())
    stillClosing <- timeout 20000 (takeMVar closeCompleted)
    stillClosing `shouldBe` Nothing
    putMVar releaseLease ()
    completed <- timeout 2000000 (takeMVar closeCompleted)
    completed `shouldBe` Just ()
    afterCloseResult <- try (withPooledConnection pool (const (pure ()))) :: IO (Either IOError ())
    assertPoolClosed afterCloseResult

assertPoolClosed :: Either IOError () -> Expectation
assertPoolClosed closeResult =
  case closeResult of
    Left closeError -> displayException closeError `shouldBe` "user error (PostgreSQL pool is closed)"
    Right () -> expectationFailure "expected a terminal pool-close error"

withUnusedLoopbackPort :: (Int -> IO value) -> IO value
withUnusedLoopbackPort action = do
  reservedSocket <- socket AF_INET Stream defaultProtocol
  bind reservedSocket (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  listen reservedSocket 1
  socketAddress <- getSocketName reservedSocket
  case socketAddress of
    SockAddrInet port _ -> do
      close reservedSocket
      action (fromIntegral port)
    _ ->
      close reservedSocket
        >> error "expected IPv4 loopback reservation socket"
