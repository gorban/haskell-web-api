{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Private socket, Warp, and TLS runtime mechanics.
--
-- The public entry points remain on the framework facade while ACME owns its
-- certificate-acquisition orchestration. Keeping the transport layer here
-- makes those concerns independently testable without coupling them to the
-- application/WAI request pipeline.
module HarchWeb.Server.Transport
  ( ReloadingTlsCredentials,
    RunningRuntimeServer,
    ensureRuntimeFileExists,
    listenerSchemeText,
    loadReloadingTlsCredentials,
    loadTlsCredentialSnapshotOrThrowWithLoader,
    openListenerSocket,
    openLoopbackSocket,
    reloadTlsCredentialsIfChanged,
    socketPort,
    startHttpRuntimeServers,
    startManualTlsRuntimeServer,
    startManualTlsRuntimeServerWithStarter,
    startManualTlsRuntimeServers,
    startWarpRuntimeServerOnSocket,
    startWarpServerOnSocket,
    stopRuntimeServer,
    stopRuntimeServers,
  )
where

import Control.Concurrent (MVar, ThreadId, forkFinally, forkIOWithUnmask, killThread, modifyMVar, modifyMVar_, myThreadId, newEmptyMVar, newMVar, putMVar, takeMVar, threadDelay, tryPutMVar)
import Control.Exception (SomeException, displayException, evaluate, finally, fromException, onException, throwIO)
import Control.Monad (unless, void)
import Data.Char (toLower)
import Data.Either (lefts)
import Data.Foldable (for_)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime)
import GHC.Clock (getMonotonicTimeNSec)
import HarchWeb.Observability qualified as Observability
import HarchWeb.Security (socketAddressText)
import HarchWeb.Server.Config
import Network.Socket qualified as Socket
import Network.TLS qualified as TLS
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WarpTLS qualified as WarpTLS
import System.Directory (doesFileExist, getModificationTime)

data RunningRuntimeServer = RunningRuntimeServer
  { runningRuntimeSocket :: Socket.Socket,
    runningRuntimeThreadId :: ThreadId
  }

data ActiveConnectionAddresses = ActiveConnectionAddresses
  { pendingConnectionAddresses :: MVar [Socket.SockAddr],
    activeConnectionAddresses :: IORef [(ThreadId, Socket.SockAddr)]
  }

data TlsCredentialSnapshot = TlsCredentialSnapshot
  { tlsCredentialModifiedTimes :: (UTCTime, UTCTime),
    tlsCredentialValues :: TLS.Credentials
  }

data ReloadingTlsCredentials = ReloadingTlsCredentials
  { tlsCredentialCertificatePath :: FilePath,
    tlsCredentialPrivateKeyPath :: FilePath,
    tlsCredentialSnapshotReference :: IORef TlsCredentialSnapshot
  }

startHttpRuntimeServers :: [ListenerEndpoint] -> Wai.Application -> IO [RunningRuntimeServer]
startHttpRuntimeServers endpoints waiApplication =
  go [] endpoints
  where
    go runningServers remainingEndpoints =
      case remainingEndpoints of
        [] -> pure (reverse runningServers)
        endpoint : remaining ->
          ( do
              runningServer <- startHttpRuntimeServer endpoint waiApplication
              go (runningServer : runningServers) remaining
                `onException` stopRuntimeServers (runningServer : runningServers)
          )
            `onException` stopRuntimeServers runningServers

startManualTlsRuntimeServers :: [ManualTlsBindPlan] -> Wai.Application -> (Observability.ConnectionObservability -> IO ()) -> IO [RunningRuntimeServer]
startManualTlsRuntimeServers manualTlsPlans waiApplication connectionReporter =
  connectionReporter `seq` go [] manualTlsPlans
  where
    go runningServers remainingPlans =
      case remainingPlans of
        [] -> pure (reverse runningServers)
        manualTlsPlan : remaining ->
          ( do
              runningServer <- startManualTlsRuntimeServer manualTlsPlan waiApplication connectionReporter
              go (runningServer : runningServers) remaining
                `onException` stopRuntimeServers (runningServer : runningServers)
          )
            `onException` stopRuntimeServers runningServers

startHttpRuntimeServer :: ListenerEndpoint -> Wai.Application -> IO RunningRuntimeServer
startHttpRuntimeServer endpoint waiApplication = do
  listeningSocket <- openListenerSocket endpoint
  serverThreadId <-
    startWarpServerOnSocket endpoint listeningSocket waiApplication
  endpoint `seq`
    pure
      RunningRuntimeServer
        { runningRuntimeSocket = listeningSocket,
          runningRuntimeThreadId = serverThreadId
        }

startManualTlsRuntimeServer :: ManualTlsBindPlan -> Wai.Application -> (Observability.ConnectionObservability -> IO ()) -> IO RunningRuntimeServer
startManualTlsRuntimeServer =
  startManualTlsRuntimeServerWithStarter startWarpTlsServerOnSocket

startManualTlsRuntimeServerWithStarter :: (ListenerEndpoint -> WarpTLS.TLSSettings -> Socket.Socket -> (Observability.ConnectionObservability -> IO ()) -> Wai.Application -> IO ThreadId) -> ManualTlsBindPlan -> Wai.Application -> (Observability.ConnectionObservability -> IO ()) -> IO RunningRuntimeServer
startManualTlsRuntimeServerWithStarter startTlsServer manualTlsPlan waiApplication connectionReporter = do
  let tlsLabel =
        case tlsCredentialSourceKind manualTlsPlan of
          ManualTlsCredentials -> "Manual TLS"
          SharedTlsCredentials -> "Shared TLS"
  reloadingTlsCredentials <-
    case tlsStartupMode manualTlsPlan of
      RequireCertificateFiles ->
        loadReloadingTlsCredentialsWithLabel
          tlsLabel
          (tlsCertificateFile manualTlsPlan)
          (tlsPrivateKeyFile manualTlsPlan)
      AwaitCertificateFiles waitTimeoutSeconds ->
        awaitReloadingTlsCredentials
          waitTimeoutSeconds
          (tlsCertificateFile manualTlsPlan)
          (tlsPrivateKeyFile manualTlsPlan)
  initialTlsCredentials <- reloadTlsCredentialsIfChanged reloadingTlsCredentials
  let endpoint = tlsEndpoint manualTlsPlan
      baseTlsSettings =
        WarpTLS.tlsSettings
          (tlsCertificateFile manualTlsPlan)
          (tlsPrivateKeyFile manualTlsPlan)
      tlsSettings =
        baseTlsSettings
          { WarpTLS.tlsCredentials = Just initialTlsCredentials,
            WarpTLS.tlsServerHooks =
              (WarpTLS.tlsServerHooks baseTlsSettings)
                { TLS.onServerNameIndication = const (reloadTlsCredentialsIfChanged reloadingTlsCredentials)
                }
          }
  listeningSocket <- openListenerSocket endpoint
  serverThreadId <-
    connectionReporter `seq`
      startTlsServer endpoint tlsSettings listeningSocket connectionReporter waiApplication
        `onException` Socket.close listeningSocket
  manualTlsPlan `seq`
    pure
      RunningRuntimeServer
        { runningRuntimeSocket = listeningSocket,
          runningRuntimeThreadId = serverThreadId
        }

stopRuntimeServers :: [RunningRuntimeServer] -> IO ()
stopRuntimeServers =
  mapM_ stopRuntimeServer

stopRuntimeServer :: RunningRuntimeServer -> IO ()
stopRuntimeServer runningServer = do
  Socket.close (runningRuntimeSocket runningServer)
  killThread (runningRuntimeThreadId runningServer)

openLoopbackSocket :: IO Socket.Socket
openLoopbackSocket =
  openListenerSocket ListenerEndpoint {endpointHost = "127.0.0.1", endpointPort = 0}

socketPort :: Socket.Socket -> IO Int
socketPort listeningSocket = do
  Socket.SockAddrInet portNumber _ <- Socket.getSocketName listeningSocket
  pure (fromIntegral portNumber)

openListenerSocket :: ListenerEndpoint -> IO Socket.Socket
openListenerSocket endpoint = do
  addressInfo :| _ <-
    ( Socket.getAddrInfo
        (Just listenerSocketHints)
        (Just (Text.unpack (endpointHost endpoint)))
        (Just (show (endpointPort endpoint))) ::
        IO (NonEmpty Socket.AddrInfo)
    )
  listeningSocket <- Socket.openSocket addressInfo
  Socket.setSocketOption listeningSocket Socket.ReuseAddr 1
  Socket.bind listeningSocket (Socket.addrAddress addressInfo)
  Socket.listen listeningSocket Socket.maxListenQueue
  pure listeningSocket

data RuntimeServerReady = RuntimeServerReady

startWarpServerOnSocket :: ListenerEndpoint -> Socket.Socket -> Wai.Application -> IO ThreadId
startWarpServerOnSocket endpoint listeningSocket waiApplication =
  startWarpRuntimeServerOnSocket $ \startupSignal ->
    let settings = runtimeHttpServerSettings endpoint startupSignal
     in settings `seq` Warp.runSettingsSocket settings listeningSocket waiApplication

startWarpTlsServerOnSocket :: ListenerEndpoint -> WarpTLS.TLSSettings -> Socket.Socket -> (Observability.ConnectionObservability -> IO ()) -> Wai.Application -> IO ThreadId
startWarpTlsServerOnSocket endpoint tlsSettings listeningSocket connectionReporter waiApplication = do
  activeConnectionAddresses <- newActiveConnectionAddresses
  let listenerScheme = Https
  listenerScheme `seq`
    connectionReporter `seq`
      startWarpRuntimeServerOnSocket $ \startupSignal ->
        let settings =
              runtimeServerSettings listenerScheme endpoint startupSignal activeConnectionAddresses connectionReporter
         in settings `seq` WarpTLS.runTLSSocket tlsSettings settings listeningSocket waiApplication

startWarpRuntimeServerOnSocket :: (MVar (Either SomeException RuntimeServerReady) -> IO ()) -> IO ThreadId
startWarpRuntimeServerOnSocket runServerOnSocket = do
  startupSignal <- newEmptyMVar
  threadId <-
    forkFinally
      (runServerOnSocket startupSignal)
      (reportRuntimeServerExit startupSignal)
  _ <- waitForRuntimeServerStartup startupSignal
  pure threadId

runtimeServerSettings :: ListenerScheme -> ListenerEndpoint -> MVar (Either SomeException RuntimeServerReady) -> ActiveConnectionAddresses -> (Observability.ConnectionObservability -> IO ()) -> Warp.Settings
runtimeServerSettings listenerScheme endpoint startupSignal activeConnectionAddresses connectionReporter =
  Warp.setPort (endpointPort endpoint)
    . Warp.setOnException (runtimeConnectionExceptionReporter listenerScheme endpoint activeConnectionAddresses connectionReporter (Warp.getOnException Warp.defaultSettings))
    . Warp.setFork (forkTrackedConnection activeConnectionAddresses)
    . Warp.setOnOpen (registerActiveConnection activeConnectionAddresses)
    . Warp.setOnClose (\_ -> unregisterActiveConnection activeConnectionAddresses)
    $ Warp.setBeforeMainLoop (putMVar startupSignal (Right RuntimeServerReady)) Warp.defaultSettings

runtimeHttpServerSettings :: ListenerEndpoint -> MVar (Either SomeException RuntimeServerReady) -> Warp.Settings
runtimeHttpServerSettings endpoint startupSignal =
  Warp.setPort (endpointPort endpoint) $
    Warp.setBeforeMainLoop (putMVar startupSignal (Right RuntimeServerReady)) Warp.defaultSettings

newActiveConnectionAddresses :: IO ActiveConnectionAddresses
newActiveConnectionAddresses =
  ActiveConnectionAddresses
    <$> newMVar []
    <*> newIORef []

registerActiveConnection :: ActiveConnectionAddresses -> Socket.SockAddr -> IO Bool
registerActiveConnection tracker socketAddress = do
  modifyMVar_ (pendingConnectionAddresses tracker) (\entries -> pure (entries ++ [socketAddress]))
  pure True

unregisterActiveConnection :: ActiveConnectionAddresses -> IO ()
unregisterActiveConnection tracker = do
  currentThreadId <- myThreadId
  untrackActiveConnection tracker currentThreadId

lookupActiveConnectionAddress :: ActiveConnectionAddresses -> IO (Maybe Socket.SockAddr)
lookupActiveConnectionAddress tracker = do
  currentThreadId <- myThreadId
  atomicModifyIORef' (activeConnectionAddresses tracker) (\entries -> (entries, lookup currentThreadId entries))

forkTrackedConnection :: ActiveConnectionAddresses -> (((forall a. IO a -> IO a) -> IO ()) -> IO ())
forkTrackedConnection tracker action = do
  maybeSocketAddress <- claimPendingConnectionAddress tracker
  void $
    forkIOWithUnmask $ \unmask -> do
      currentThreadId <- myThreadId
      for_ maybeSocketAddress (trackActiveConnection tracker currentThreadId)
      action unmask `finally` untrackActiveConnection tracker currentThreadId

claimPendingConnectionAddress :: ActiveConnectionAddresses -> IO (Maybe Socket.SockAddr)
claimPendingConnectionAddress tracker =
  modifyMVar
    (pendingConnectionAddresses tracker)
    ( \entries ->
        case entries of
          [] -> pure ([], Nothing)
          firstAddress : _ -> do
            (,) <$> evaluate (drop 1 entries) <*> pure (Just firstAddress)
    )

trackActiveConnection :: ActiveConnectionAddresses -> ThreadId -> Socket.SockAddr -> IO ()
trackActiveConnection tracker currentThreadId socketAddress =
  atomicModifyIORef'
    (activeConnectionAddresses tracker)
    (\entries -> ((currentThreadId, socketAddress) : entries, ()))

untrackActiveConnection :: ActiveConnectionAddresses -> ThreadId -> IO ()
untrackActiveConnection tracker currentThreadId =
  atomicModifyIORef'
    (activeConnectionAddresses tracker)
    (\entries -> (filter ((/= currentThreadId) . fst) entries, ()))

runtimeConnectionExceptionReporter :: ListenerScheme -> ListenerEndpoint -> ActiveConnectionAddresses -> (Observability.ConnectionObservability -> IO ()) -> (Maybe Wai.Request -> SomeException -> IO ()) -> Maybe Wai.Request -> SomeException -> IO ()
runtimeConnectionExceptionReporter listenerScheme endpoint activeConnectionAddresses connectionReporter defaultReporter maybeRequest exception = do
  maybeConnectionObservability <-
    buildConnectionExceptionObservability
      listenerScheme
      endpoint
      activeConnectionAddresses
      exception
  case maybeConnectionObservability of
    Just connectionObservability ->
      Observability.forceConnectionObservability connectionObservability `seq`
        connectionReporter connectionObservability
    Nothing ->
      defaultReporter maybeRequest exception

buildConnectionExceptionObservability :: ListenerScheme -> ListenerEndpoint -> ActiveConnectionAddresses -> SomeException -> IO (Maybe Observability.ConnectionObservability)
buildConnectionExceptionObservability listenerScheme endpoint activeConnectionAddresses exception =
  case fromException exception of
    Just warpTlsException ->
      case warpTlsException of
        WarpTLS.InsecureConnectionDenied ->
          buildConnectionObservabilityValue "insecure-connection-denied" "InsecureConnectionDenied"
        WarpTLS.ClientClosedConnectionPrematurely ->
          buildConnectionObservabilityValue "client-closed-connection-prematurely" "ClientClosedConnectionPrematurely"
    Nothing -> pure Nothing
  where
    buildConnectionObservabilityValue eventName exceptionType = do
      maybePeerAddress <-
        fmap (fmap socketAddressText) (lookupActiveConnectionAddress activeConnectionAddresses)
      let maybeClientAddress = maybePeerAddress
      pure . Just $
        Observability.buildConnectionObservability
          ("CONNECTION " <> eventName)
          ( catMaybes
              [ textObservabilityAttribute "client.address" <$> maybeClientAddress,
                textObservabilityAttribute "network.peer.address" <$> maybePeerAddress
              ]
              ++ [ textObservabilityAttribute "url.scheme" (listenerSchemeText listenerScheme),
                   textObservabilityAttribute "server.address" (endpointHost endpoint),
                   Observability.ObservabilityAttribute
                     { Observability.attributeName = "server.port",
                       Observability.attributeValue = Observability.IntAttribute (endpointPort endpoint)
                     },
                   textObservabilityAttribute "harch.connection.event" eventName,
                   textObservabilityAttribute "exception.type" exceptionType,
                   textObservabilityAttribute "exception.message" (Text.pack (displayException exception))
                 ]
          )

reportRuntimeServerExit :: MVar (Either SomeException RuntimeServerReady) -> Either SomeException () -> IO ()
reportRuntimeServerExit startupSignal exitResult =
  mapM_ (tryPutMVar startupSignal . Left) (lefts [exitResult])

waitForRuntimeServerStartup :: MVar (Either SomeException RuntimeServerReady) -> IO RuntimeServerReady
waitForRuntimeServerStartup startupSignal = do
  startupResult <- takeMVar startupSignal
  case startupResult of
    Left startupException -> throwIO startupException
    Right runtimeServerReady@RuntimeServerReady -> evaluate runtimeServerReady

ensureRuntimeFileExists :: String -> FilePath -> IO ()
ensureRuntimeFileExists errorPrefix filePath = do
  fileExists <- doesFileExist filePath
  unless fileExists (ioError (userError (errorPrefix <> filePath)))

listenerSchemeText :: ListenerScheme -> Text
listenerSchemeText listenerScheme =
  case listenerScheme of
    Http -> "http"
    Https -> "https"

textObservabilityAttribute :: Text -> Text -> Observability.ObservabilityAttribute
textObservabilityAttribute name value =
  Observability.ObservabilityAttribute
    { Observability.attributeName = name,
      Observability.attributeValue = Observability.TextAttribute value
    }

loadReloadingTlsCredentials :: FilePath -> FilePath -> IO ReloadingTlsCredentials
loadReloadingTlsCredentials certificatePath privateKeyPath = do
  snapshot <- loadTlsCredentialSnapshotOrThrow certificatePath privateKeyPath
  snapshotReference <- newIORef snapshot
  pure
    ReloadingTlsCredentials
      { tlsCredentialCertificatePath = certificatePath,
        tlsCredentialPrivateKeyPath = privateKeyPath,
        tlsCredentialSnapshotReference = snapshotReference
      }

loadReloadingTlsCredentialsWithLabel :: String -> FilePath -> FilePath -> IO ReloadingTlsCredentials
loadReloadingTlsCredentialsWithLabel tlsLabel certificatePath privateKeyPath = do
  snapshot <- loadTlsCredentialSnapshotOrThrowWithLabel tlsLabel certificatePath privateKeyPath
  snapshotReference <- newIORef snapshot
  pure
    ReloadingTlsCredentials
      { tlsCredentialCertificatePath = certificatePath,
        tlsCredentialPrivateKeyPath = privateKeyPath,
        tlsCredentialSnapshotReference = snapshotReference
      }

awaitReloadingTlsCredentials :: Maybe Int -> FilePath -> FilePath -> IO ReloadingTlsCredentials
awaitReloadingTlsCredentials waitTimeoutSeconds certificatePath privateKeyPath = do
  startedAt <- getMonotonicTimeNSec
  go startedAt
  where
    timeoutWindow =
      fmap
        (\seconds -> (seconds, fromIntegral seconds * 1000000000))
        waitTimeoutSeconds

    go !startedAt = do
      snapshotResult <- loadTlsCredentialSnapshotIfPresent certificatePath privateKeyPath
      case snapshotResult of
        Just (Right snapshot) -> do
          snapshotReference <- newIORef snapshot
          pure
            ReloadingTlsCredentials
              { tlsCredentialCertificatePath = certificatePath,
                tlsCredentialPrivateKeyPath = privateKeyPath,
                tlsCredentialSnapshotReference = snapshotReference
              }
        _ -> do
          currentTime <- getMonotonicTimeNSec
          case timeoutWindow of
            Just (waitSeconds, timeoutNs)
              | currentTime - startedAt >= timeoutNs ->
                  let timeoutSuffix = " after " <> show waitSeconds <> " seconds"
                   in ioError . userError $
                        case snapshotResult of
                          Just (Left loadError) ->
                            "Timed out waiting for shared TLS credentials at "
                              <> certificatePath
                              <> " and "
                              <> privateKeyPath
                              <> timeoutSuffix
                              <> ": "
                              <> loadError
                          _ ->
                            "Timed out waiting for shared TLS certificate files at "
                              <> certificatePath
                              <> " and "
                              <> privateKeyPath
                              <> timeoutSuffix
            _ -> threadDelay 100000 >> go startedAt

reloadTlsCredentialsIfChanged :: ReloadingTlsCredentials -> IO TLS.Credentials
reloadTlsCredentialsIfChanged reloadingTlsCredentials = do
  cachedSnapshot <-
    atomicModifyIORef'
      (tlsCredentialSnapshotReference reloadingTlsCredentials)
      (\snapshot -> (snapshot, snapshot))
  latestSnapshotResult <-
    loadTlsCredentialSnapshotIfPresent
      (tlsCredentialCertificatePath reloadingTlsCredentials)
      (tlsCredentialPrivateKeyPath reloadingTlsCredentials)
  case latestSnapshotResult of
    Just (Right latestSnapshot)
      | tlsCredentialModifiedTimes latestSnapshot /= tlsCredentialModifiedTimes cachedSnapshot ->
          latestSnapshot `seq`
            atomicModifyIORef'
              (tlsCredentialSnapshotReference reloadingTlsCredentials)
              (const (latestSnapshot, tlsCredentialValues latestSnapshot))
    _ ->
      pure (tlsCredentialValues cachedSnapshot)

loadTlsCredentialSnapshotOrThrow :: FilePath -> FilePath -> IO TlsCredentialSnapshot
loadTlsCredentialSnapshotOrThrow =
  loadTlsCredentialSnapshotOrThrowWithLabel "Manual TLS"

loadTlsCredentialSnapshotOrThrowWithLabel :: String -> FilePath -> FilePath -> IO TlsCredentialSnapshot
loadTlsCredentialSnapshotOrThrowWithLabel tlsLabel certificatePath privateKeyPath =
  loadTlsCredentialSnapshotOrThrowWithLoader
    tlsLabel
    certificatePath
    privateKeyPath
    (loadTlsCredentialSnapshotIfPresent certificatePath privateKeyPath)

loadTlsCredentialSnapshotOrThrowWithLoader :: String -> FilePath -> FilePath -> IO (Maybe (Either String TlsCredentialSnapshot)) -> IO TlsCredentialSnapshot
loadTlsCredentialSnapshotOrThrowWithLoader tlsLabel certificatePath privateKeyPath loadSnapshot = do
  ensureRuntimeFileExists (tlsLabel <> " certificate file does not exist: ") certificatePath
  ensureRuntimeFileExists (tlsLabel <> " private key file does not exist: ") privateKeyPath
  snapshotResult <- loadSnapshot
  case fromMaybe (Left "credential files disappeared while loading") snapshotResult of
    Right snapshot ->
      pure snapshot
    Left loadError ->
      ioError . userError $
        "Failed to load "
          <> lowerFirst tlsLabel
          <> " credentials from "
          <> certificatePath
          <> " and "
          <> privateKeyPath
          <> ": "
          <> loadError
  where
    lowerFirst [] = []
    lowerFirst (firstCharacter : remainingCharacters) =
      toLower firstCharacter : remainingCharacters

loadTlsCredentialSnapshotIfPresent :: FilePath -> FilePath -> IO (Maybe (Either String TlsCredentialSnapshot))
loadTlsCredentialSnapshotIfPresent certificatePath privateKeyPath = do
  certificateExists <- doesFileExist certificatePath
  privateKeyExists <- doesFileExist privateKeyPath
  if certificateExists && privateKeyExists
    then do
      certificateModifiedAt <- getModificationTime certificatePath
      privateKeyModifiedAt <- getModificationTime privateKeyPath
      credentialResult <- TLS.credentialLoadX509 certificatePath privateKeyPath
      pure
        ( Just
            ( fmap
                ( \credential ->
                    TlsCredentialSnapshot
                      { tlsCredentialModifiedTimes = (certificateModifiedAt, privateKeyModifiedAt),
                        tlsCredentialValues = TLS.Credentials [credential]
                      }
                )
                credentialResult
            )
        )
    else
      pure Nothing

listenerSocketHints :: Socket.AddrInfo
listenerSocketHints =
  Socket.defaultHints
    { Socket.addrFlags = [Socket.AI_NUMERICHOST, Socket.AI_NUMERICSERV],
      Socket.addrFamily = Socket.AF_INET,
      Socket.addrSocketType = Socket.Stream
    }
