{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Exception (SomeException, throwIO, try)
import Data.Either (isLeft)
import Database.PostgreSQL.LibPQ qualified as LibPQ
import Network.Socket (Family (AF_INET), SockAddr (SockAddrInet), SocketType (Stream), bind, close, defaultProtocol, getSocketName, listen, socket, tupleToHostAddress)
import System.Timeout (timeout)
import TestSupport.RealPostgres (defaultRealPostgresConfig, ensureDefaultPostgresAvailable)
import WebApi.Config (DatabaseConfig (..))
import WebApi.Postgres.Testing (closePostgresPool, newPostgresPool, runPooledRowsQuery, withPooledConnection)

spec = describe "WebApi.Postgres.Pool" $ do
  it "reuses the same connection across sequential acquire/release rather than opening a fresh one each time" $ do
    ensureDefaultPostgresAvailable
    pool <- newPostgresPool 5 defaultRealPostgresConfig
    firstProcessId <- withPooledConnection pool LibPQ.backendPID
    secondProcessId <- withPooledConnection pool LibPQ.backendPID
    secondProcessId `shouldBe` firstProcessId

  it "blocks a second acquirer at capacity and releases it only once the first connection is returned" $ do
    ensureDefaultPostgresAvailable
    pool <- newPostgresPool 1 defaultRealPostgresConfig
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

  it "keeps the pool usable after closing its idle connections" $ do
    ensureDefaultPostgresAvailable
    pool <- newPostgresPool 1 defaultRealPostgresConfig
    _ <- runPooledRowsQuery pool "SELECT 1;"
    closePostgresPool pool
    afterCloseResult <- runPooledRowsQuery pool "SELECT 1;"
    afterCloseResult `shouldBe` Right ["1"]

  it "still returns the connection to the pool when the wrapped action throws" $ do
    ensureDefaultPostgresAvailable
    pool <- newPostgresPool 1 defaultRealPostgresConfig
    firstAttempt <- try (withPooledConnection pool (const (throwIO (userError "boom")))) :: IO (Either SomeException ())
    isLeft firstAttempt `shouldBe` True
    secondResult <- runPooledRowsQuery pool "SELECT 1;"
    secondResult `shouldBe` Right ["1"]

  it "surfaces a connection failure as a query error and discards the broken connection instead of pooling it" $
    withUnusedLoopbackPort $ \unreachablePort -> do
      pool <- newPostgresPool 1 (defaultRealPostgresConfig {databasePort = unreachablePort})
      firstResult <- runPooledRowsQuery pool "SELECT 1;"
      secondResult <- runPooledRowsQuery pool "SELECT 1;"
      isLeft firstResult `shouldBe` True
      isLeft secondResult `shouldBe` True

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
