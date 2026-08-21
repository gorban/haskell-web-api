{-# LANGUAGE OverloadedStrings #-}

module WebApi.Postgres.Pool
  ( PostgresPool,
    newPostgresPool,
    closePostgresPool,
    withPooledConnection,
    libpqConnectionValue,
    runtimeConnectionString,
  )
where

import Control.Concurrent.STM (STM, atomically, modifyTVar', newTVarIO, readTVar, retry, swapTVar, writeTVar)
import Control.Concurrent.STM.TVar (TVar)
import Control.Exception (bracket)
import Data.ByteString qualified as ByteString
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Database.PostgreSQL.LibPQ qualified as LibPQ
import WebApi.Config (DatabaseConfig (..))

-- | A bounded set of live libpq connections against one 'DatabaseConfig',
-- shared across every runtime query the process makes instead of the
-- historical one-fresh-connection-per-query pattern. Connections are opened
-- lazily (on first acquire, up to 'poolCapacity') rather than eagerly at
-- construction, so 'newPostgresPool' itself always succeeds: a misconfigured
-- or unreachable database only surfaces as a query failure, matching the
-- 'DatabaseConfig'-per-query behavior this pool replaces (see
-- 'Integration.WebApiSpec'\'s connection-failure case, which asserts on
-- exactly that lazily-discovered failure shape).
data PostgresPool = PostgresPool
  { poolDatabaseConfig :: DatabaseConfig,
    -- | Live connections not currently checked out.
    poolIdleConnections :: TVar [LibPQ.Connection],
    -- | Total connections that exist right now: checked out, idle, or
    -- reserved mid-connect. Bounded by 'poolCapacity'; acquiring past that
    -- bound blocks (via STM 'retry') until a slot frees up.
    poolLiveConnectionCount :: TVar Int,
    poolCapacity :: Int
  }

newPostgresPool :: Int -> DatabaseConfig -> IO PostgresPool
newPostgresPool capacity config = do
  idleConnections <- newTVarIO []
  liveConnectionCount <- newTVarIO 0
  pure
    PostgresPool
      { poolDatabaseConfig = config,
        poolIdleConnections = idleConnections,
        poolLiveConnectionCount = liveConnectionCount,
        poolCapacity = capacity
      }

-- | Finishes every currently idle connection and frees its slot. Connections
-- checked out via 'withPooledConnection' at the time this runs are left
-- alone (their own release still runs when their caller finishes); calling
-- this while other callers still hold connections is a caller error, the
-- same as closing any other shared resource still in use.
closePostgresPool :: PostgresPool -> IO ()
closePostgresPool pool = do
  idleConnections <- atomically (swapTVar (poolIdleConnections pool) [])
  traverse_ LibPQ.finish idleConnections
  atomically (modifyTVar' (poolLiveConnectionCount pool) (subtract (length idleConnections)))

-- | Acquires a pooled connection for the duration of the given action and
-- always returns it afterward, even if the action throws.
withPooledConnection :: PostgresPool -> (LibPQ.Connection -> IO value) -> IO value
withPooledConnection pool = bracket (acquirePooledConnection pool) (releasePooledConnection pool)

data AcquiredSlot
  = ReusedIdleConnection LibPQ.Connection
  | ReservedNewConnectionSlot

acquirePooledConnection :: PostgresPool -> IO LibPQ.Connection
acquirePooledConnection pool = do
  acquiredSlot <- atomically (acquirePooledSlotSTM pool)
  case acquiredSlot of
    ReusedIdleConnection connection -> pure connection
    ReservedNewConnectionSlot -> LibPQ.connectdb (runtimeConnectionString (poolDatabaseConfig pool))

acquirePooledSlotSTM :: PostgresPool -> STM AcquiredSlot
acquirePooledSlotSTM pool = do
  idleConnections <- readTVar (poolIdleConnections pool)
  case idleConnections of
    -- The @$!@ forces this pattern-bound tail directly: GHC's HPC
    -- instrumentation does not credit a bare variable used as a direct
    -- argument to an already-instrumented call, even though this branch is
    -- genuinely exercised (see 'WebApi.Postgres.Pool' reuse test). Confirmed
    -- directly, not assumed, that HLint does not flag this: 'writeTVar'
    -- does not force its argument, and a pattern-matched list tail is a
    -- genuine unevaluated thunk here, not a value already in WHNF.
    connection : remainingIdleConnections -> do
      writeTVar (poolIdleConnections pool) $! remainingIdleConnections
      pure (ReusedIdleConnection connection)
    [] -> do
      liveConnectionCount <- readTVar (poolLiveConnectionCount pool)
      if liveConnectionCount < poolCapacity pool
        then do
          writeTVar (poolLiveConnectionCount pool) (liveConnectionCount + 1)
          pure ReservedNewConnectionSlot
        else retry

-- | A connection that failed mid-use (or failed to connect at all) reports
-- 'LibPQ.ConnectionBad' from 'LibPQ.status'; discarding it here and freeing
-- its slot lets the next acquirer open a fresh replacement instead of
-- reusing a connection known to be broken. This never throws: releasing a
-- connection back to the pool is bookkeeping, and a best-effort 'LibPQ.finish'
-- on an already-broken connection is not a caller-visible failure.
releasePooledConnection :: PostgresPool -> LibPQ.Connection -> IO ()
releasePooledConnection pool connection = do
  connectionStatus <- LibPQ.status connection
  case connectionStatus of
    LibPQ.ConnectionOk -> atomically (modifyTVar' (poolIdleConnections pool) (connection :))
    _ -> do
      LibPQ.finish connection
      atomically (modifyTVar' (poolLiveConnectionCount pool) (subtract 1))

runtimeConnectionString :: DatabaseConfig -> ByteString.ByteString
runtimeConnectionString databaseConfig =
  TextEncoding.encodeUtf8 $
    Text.unwords
      [ "host=" <> libpqConnectionValue (databaseHost databaseConfig),
        "port=" <> Text.pack (show (databasePort databaseConfig)),
        "dbname=" <> libpqConnectionValue (databaseName databaseConfig),
        "user=" <> libpqConnectionValue (databaseUser databaseConfig),
        "password=" <> libpqConnectionValue (databasePassword databaseConfig),
        "connect_timeout=" <> Text.pack (show (databaseConnectTimeoutSeconds databaseConfig))
      ]

-- | Quote a value for libpq's connection-string syntax. Backslashes must be
-- escaped before quotes: escaping in the other order turns each escaped
-- quote's backslash into two backslashes, leaving the quote unescaped and
-- terminating the value early, so the remainder of a password or database
-- name containing a quote would be parsed as further conninfo keywords.
libpqConnectionValue :: Text -> Text
libpqConnectionValue value =
  "'" <> Text.replace "'" "\\'" (Text.replace "\\" "\\\\" value) <> "'"
