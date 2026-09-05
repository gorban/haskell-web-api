{-# LANGUAGE OverloadedStrings #-}

module WebApi.Postgres.Pool
  ( PostgresPool,
    newPostgresPool,
    newPostgresPoolWithConnector,
    closePostgresPool,
    withPooledConnection,
    databaseTransportEnvironment,
    libpqConnectionValue,
    runtimeConnectionString,
  )
where

import Control.Concurrent.STM (STM, atomically, check, modifyTVar', newTVarIO, readTVar, retry, swapTVar, throwSTM, writeTVar)
import Control.Concurrent.STM.TVar (TVar)
import Control.Exception (SomeException, bracket, mask, onException, try)
import Control.Monad (unless)
import Data.ByteString qualified as ByteString
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Database.PostgreSQL.LibPQ qualified as LibPQ
import WebApi.Config (DatabaseConfig (..), DatabasePoolCapacity, DatabaseSslMode (..), DatabaseTransportSecurity (..), databasePoolCapacityValue)

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
  { -- | Live connections not currently checked out.
    poolIdleConnections :: TVar [LibPQ.Connection],
    -- | Total connections that exist right now: checked out, idle, or
    -- reserved mid-connect. Bounded by 'poolCapacity'; acquiring past that
    -- bound blocks (via STM 'retry') until a slot frees up.
    poolLiveConnectionCount :: TVar Int,
    poolCapacity :: DatabasePoolCapacity,
    -- | Once terminal closure begins, no new lease may be created. Existing
    -- leases finish their connection on release, allowing the owner to wait
    -- for a complete shutdown without racing a new borrower.
    poolLifecycle :: TVar PoolLifecycle,
    poolConnector :: IO LibPQ.Connection
  }

data PoolLifecycle
  = PoolOpen
  | PoolClosing

newPostgresPool :: DatabasePoolCapacity -> DatabaseConfig -> IO PostgresPool
newPostgresPool capacity config =
  newPostgresPoolWithConnector capacity (LibPQ.connectdb (runtimeConnectionString config))

-- | The connector seam makes the reservation/connect handoff testable under
-- deterministic synchronous exceptions. Production uses 'LibPQ.connectdb';
-- tests can throw before a connection exists and prove that the reserved slot
-- is returned to STM before another borrower attempts to lease it.
newPostgresPoolWithConnector :: DatabasePoolCapacity -> IO LibPQ.Connection -> IO PostgresPool
newPostgresPoolWithConnector capacity connector = do
  idleConnections <- newTVarIO []
  liveConnectionCount <- newTVarIO 0
  lifecycle <- newTVarIO PoolOpen
  pure
    PostgresPool
      { poolIdleConnections = idleConnections,
        poolLiveConnectionCount = liveConnectionCount,
        poolCapacity = capacity,
        poolLifecycle = lifecycle,
        poolConnector = connector
      }

-- | Terminally closes a pool. It first excludes new borrowers, then finishes
-- idle connections and waits for outstanding leases to return. A returned
-- connection is finished rather than retained once closure has begun. It is
-- therefore safe for the composition root to bracket this operation around
-- the server lifetime; the pool cannot be reused after it returns.
closePostgresPool :: PostgresPool -> IO ()
closePostgresPool pool = mask $ \_ -> do
  idleConnections <- atomically (beginPoolClosure pool)
  traverse_ (finishAndReleaseSlot pool) idleConnections
  atomically (waitForPoolClosure pool)

-- | Acquires a pooled connection for the duration of the given action and
-- always returns it afterward, even if the action throws.
withPooledConnection :: PostgresPool -> (LibPQ.Connection -> IO value) -> IO value
withPooledConnection pool = bracket (acquirePooledConnection pool) (releasePooledConnection pool)

data AcquiredSlot
  = ReusedIdleConnection LibPQ.Connection
  | ReservedNewConnectionSlot

acquirePooledConnection :: PostgresPool -> IO LibPQ.Connection
acquirePooledConnection pool = mask $ \restore -> do
  acquiredSlot <- atomically (acquirePooledSlotSTM pool)
  case acquiredSlot of
    ReusedIdleConnection connection -> pure connection
    ReservedNewConnectionSlot ->
      restore (poolConnector pool)
        `onException` atomically (releaseReservedSlotSTM pool)

acquirePooledSlotSTM :: PostgresPool -> STM AcquiredSlot
acquirePooledSlotSTM pool = do
  lifecycle <- readTVar (poolLifecycle pool)
  case lifecycle of
    PoolClosing -> throwSTM (userError "PostgreSQL pool is closed")
    PoolOpen -> acquireOpenPoolSlotSTM pool

acquireOpenPoolSlotSTM :: PostgresPool -> STM AcquiredSlot
acquireOpenPoolSlotSTM pool = do
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
      if liveConnectionCount < databasePoolCapacityValue (poolCapacity pool)
        then do
          writeTVar (poolLiveConnectionCount pool) (liveConnectionCount + 1)
          pure ReservedNewConnectionSlot
        else retry

releaseReservedSlotSTM :: PostgresPool -> STM ()
releaseReservedSlotSTM pool =
  modifyTVar' (poolLiveConnectionCount pool) (subtract 1)

beginPoolClosure :: PostgresPool -> STM [LibPQ.Connection]
beginPoolClosure pool = do
  lifecycle <- readTVar (poolLifecycle pool)
  case lifecycle of
    PoolClosing -> readTVar (poolIdleConnections pool)
    PoolOpen -> do
      writeTVar (poolLifecycle pool) PoolClosing
      swapTVar (poolIdleConnections pool) []

waitForPoolClosure :: PostgresPool -> STM ()
waitForPoolClosure pool = do
  liveConnectionCount <- readTVar (poolLiveConnectionCount pool)
  check (liveConnectionCount == 0)

-- | A connection that failed mid-use (or failed to connect at all) reports
-- 'LibPQ.ConnectionBad' from 'LibPQ.status'; discarding it here and freeing
-- its slot lets the next acquirer open a fresh replacement instead of
-- reusing a connection known to be broken. The surrounding 'mask' protects
-- the status/check-in handoff from cancellation, while a terminal lifecycle
-- instead finishes and releases the checked-out slot.
releasePooledConnection :: PostgresPool -> LibPQ.Connection -> IO ()
releasePooledConnection pool connection = mask $ \_ -> do
  connectionStatus <- LibPQ.status connection
  releaseAfterStatus connectionStatus
  where
    releaseAfterStatus LibPQ.ConnectionOk = do
      returnedToOpenPool <- atomically (returnToOpenPoolSTM pool connection)
      unless returnedToOpenPool releaseClosedConnection
    releaseAfterStatus _ = releaseClosedConnection
    releaseClosedConnection = finishAndReleaseSlot pool connection

returnToOpenPoolSTM :: PostgresPool -> LibPQ.Connection -> STM Bool
returnToOpenPoolSTM pool connection = do
  lifecycle <- readTVar (poolLifecycle pool)
  case lifecycle of
    PoolOpen -> do
      modifyTVar' (poolIdleConnections pool) (connection :)
      pure True
    PoolClosing -> pure False

finishAndReleaseSlot :: PostgresPool -> LibPQ.Connection -> IO ()
finishAndReleaseSlot pool connection = do
  _ <- try (LibPQ.finish connection) :: IO (Either SomeException ())
  atomically (releaseReservedSlotSTM pool)

runtimeConnectionString :: DatabaseConfig -> ByteString.ByteString
runtimeConnectionString databaseConfig =
  TextEncoding.encodeUtf8 $
    Text.unwords
      ( [ "host=" <> libpqConnectionValue (databaseHost databaseConfig),
          "port=" <> Text.pack (show (databasePort databaseConfig)),
          "dbname=" <> libpqConnectionValue (databaseName databaseConfig),
          "user=" <> libpqConnectionValue (databaseUser databaseConfig),
          "password=" <> libpqConnectionValue (databasePassword databaseConfig),
          "connect_timeout=" <> Text.pack (show (databaseConnectTimeoutSeconds databaseConfig))
        ]
          <> databaseTransportConnectionParameters (databaseTransportSecurity databaseConfig)
      )

databaseTransportConnectionParameters :: DatabaseTransportSecurity -> [Text]
databaseTransportConnectionParameters transportSecurity =
  case transportSecurity of
    DatabaseTransportLibpqDefault -> []
    DatabaseTransportSsl sslMode rootCert ->
      ["sslmode=" <> databaseSslModeIdentifier sslMode]
        <> maybe [] (pure . ("sslrootcert=" <>) . libpqConnectionValue) rootCert

databaseSslModeIdentifier :: DatabaseSslMode -> Text
databaseSslModeIdentifier sslMode =
  case sslMode of
    DatabaseSslDisable -> "disable"
    DatabaseSslAllow -> "allow"
    DatabaseSslPrefer -> "prefer"
    DatabaseSslRequire -> "require"
    DatabaseSslVerifyCa -> "verify-ca"
    DatabaseSslVerifyFull -> "verify-full"

-- | The @psql@ command path is also backed by libpq, but takes its TLS
-- settings through libpq's documented environment names rather than a
-- conninfo string.  Keep it derived from the same closed policy so seed and
-- administrative commands cannot silently use a different transport mode.
databaseTransportEnvironment :: DatabaseTransportSecurity -> [(String, String)]
databaseTransportEnvironment transportSecurity =
  case transportSecurity of
    DatabaseTransportLibpqDefault -> []
    DatabaseTransportSsl sslMode rootCert ->
      [("PGSSLMODE", Text.unpack (databaseSslModeIdentifier sslMode))]
        <> maybe [] (pure . ("PGSSLROOTCERT",) . Text.unpack) rootCert

-- | Quote a value for libpq's connection-string syntax. Backslashes must be
-- escaped before quotes: escaping in the other order turns each escaped
-- quote's backslash into two backslashes, leaving the quote unescaped and
-- terminating the value early, so the remainder of a password or database
-- name containing a quote would be parsed as further conninfo keywords.
libpqConnectionValue :: Text -> Text
libpqConnectionValue value =
  "'" <> Text.replace "'" "\\'" (Text.replace "\\" "\\\\" value) <> "'"
