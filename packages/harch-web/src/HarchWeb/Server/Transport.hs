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
    TlsCertificateFilePath,
    TlsPrivateKeyFilePath,
    ensureRuntimeFileExists,
    listenerSchemeText,
    loadReloadingTlsCredentials,
    loadTlsCredentialSnapshotOrThrowWithLoader,
    openListenerSocket,
    openLoopbackSocket,
    reloadTlsCredentialsIfChanged,
    socketPort,
    startHttpRuntimeServerWithStarter,
    startHttpRuntimeServersWithRequestTransportLimits,
    startManualTlsRuntimeServerWithRequestTransportLimits,
    startManualTlsRuntimeServerWithStarter,
    startManualTlsRuntimeServersWithRequestTransportLimits,
    startWarpRuntimeServerOnSocket,
    startWarpServerOnSocketWithRequestTransportLimits,
    stopRuntimeServer,
    stopRuntimeServers,
    tlsCertificateFilePath,
    tlsCertificateFilePathValue,
    tlsPrivateKeyFilePath,
    tlsPrivateKeyFilePathValue,
  )
where

import Control.Concurrent (MVar, ThreadId, forkFinally, forkIOWithUnmask, killThread, modifyMVar, modifyMVar_, myThreadId, newEmptyMVar, newMVar, putMVar, takeMVar, tryPutMVar)
import Control.Exception (SomeException, displayException, evaluate, finally, fromException, onException, throwIO)
import Control.Monad (void)
import Data.Either (lefts)
import Data.Foldable (for_)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Observability qualified as Observability
import HarchWeb.Security
  ( RequestHeadLimits (..),
    RequestTransportLimits (..),
    requestByteLimitValue,
    requestTimeoutSecondsValue,
    socketAddressText,
  )
import HarchWeb.Server.Config
import HarchWeb.Server.Transport.Tls
  ( ReloadingTlsCredentials,
    TlsCertificateFilePath,
    TlsPrivateKeyFilePath,
    awaitReloadingTlsCredentials,
    ensureRuntimeFileExists,
    loadReloadingTlsCredentials,
    loadReloadingTlsCredentialsWithLabel,
    loadTlsCredentialSnapshotOrThrowWithLoader,
    reloadTlsCredentialsIfChanged,
    tlsCertificateFilePath,
    tlsCertificateFilePathValue,
    tlsPrivateKeyFilePath,
    tlsPrivateKeyFilePathValue,
  )
import Network.Socket qualified as Socket
import Network.TLS qualified as TLS
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WarpTLS qualified as WarpTLS

data RunningRuntimeServer = RunningRuntimeServer
  { runningRuntimeSocket :: Socket.Socket,
    runningRuntimeThreadId :: ThreadId
  }

data ActiveConnectionAddresses = ActiveConnectionAddresses
  { pendingConnectionAddresses :: MVar [Socket.SockAddr],
    activeConnectionAddresses :: IORef [(ThreadId, Socket.SockAddr)]
  }

-- | Start HTTP listeners with all opt-in Warp request transport controls.
startHttpRuntimeServersWithRequestTransportLimits :: RequestHeadLimits -> RequestTransportLimits -> [ListenerEndpoint] -> Wai.Application -> IO [RunningRuntimeServer]
startHttpRuntimeServersWithRequestTransportLimits requestHeadLimits transportLimits endpoints waiApplication =
  go [] endpoints
  where
    go runningServers remainingEndpoints =
      case remainingEndpoints of
        [] -> pure (reverse runningServers)
        endpoint : remaining ->
          ( do
              runningServer <- startHttpRuntimeServer requestHeadLimits transportLimits endpoint waiApplication
              go (runningServer : runningServers) remaining
                `onException` stopRuntimeServers (runningServer : runningServers)
          )
            `onException` stopRuntimeServers runningServers

-- | Start manual-TLS listeners with all opt-in Warp request transport controls.
startManualTlsRuntimeServersWithRequestTransportLimits :: RequestHeadLimits -> RequestTransportLimits -> [ManualTlsBindPlan] -> Wai.Application -> (Observability.ConnectionObservability -> IO ()) -> IO [RunningRuntimeServer]
startManualTlsRuntimeServersWithRequestTransportLimits requestHeadLimits transportLimits manualTlsPlans waiApplication connectionReporter =
  connectionReporter `seq` go [] manualTlsPlans
  where
    go runningServers remainingPlans =
      case remainingPlans of
        [] -> pure (reverse runningServers)
        manualTlsPlan : remaining ->
          ( do
              runningServer <- startManualTlsRuntimeServerWithRequestTransportLimits requestHeadLimits transportLimits manualTlsPlan waiApplication connectionReporter
              go (runningServer : runningServers) remaining
                `onException` stopRuntimeServers (runningServer : runningServers)
          )
            `onException` stopRuntimeServers runningServers

startHttpRuntimeServer :: RequestHeadLimits -> RequestTransportLimits -> ListenerEndpoint -> Wai.Application -> IO RunningRuntimeServer
startHttpRuntimeServer requestHeadLimits transportLimits =
  startHttpRuntimeServerWithStarter
    (startWarpServerOnSocketWithRequestTransportLimits requestHeadLimits transportLimits)

-- | Start one HTTP listener with an injected server starter.  The production
-- path supplies Warp; the injection keeps the post-bind startup failure path
-- testable so a failed starter cannot leave its listener socket open.
startHttpRuntimeServerWithStarter :: (ListenerEndpoint -> Socket.Socket -> Wai.Application -> IO ThreadId) -> ListenerEndpoint -> Wai.Application -> IO RunningRuntimeServer
startHttpRuntimeServerWithStarter startHttpServer endpoint waiApplication = do
  listeningSocket <- openListenerSocket endpoint
  serverThreadId <-
    startHttpServer endpoint listeningSocket waiApplication
      `onException` Socket.close listeningSocket
  endpoint `seq`
    pure
      RunningRuntimeServer
        { runningRuntimeSocket = listeningSocket,
          runningRuntimeThreadId = serverThreadId
        }

startManualTlsRuntimeServerWithRequestTransportLimits :: RequestHeadLimits -> RequestTransportLimits -> ManualTlsBindPlan -> Wai.Application -> (Observability.ConnectionObservability -> IO ()) -> IO RunningRuntimeServer
startManualTlsRuntimeServerWithRequestTransportLimits requestHeadLimits transportLimits =
  startManualTlsRuntimeServerWithStarterAndRequestTransportLimits
    (startWarpTlsServerOnSocketWithRequestTransportLimits requestHeadLimits transportLimits)

startManualTlsRuntimeServerWithStarter :: (ListenerEndpoint -> WarpTLS.TLSSettings -> Socket.Socket -> (Observability.ConnectionObservability -> IO ()) -> Wai.Application -> IO ThreadId) -> ManualTlsBindPlan -> Wai.Application -> (Observability.ConnectionObservability -> IO ()) -> IO RunningRuntimeServer
startManualTlsRuntimeServerWithStarter =
  startManualTlsRuntimeServerWithStarterAndRequestTransportLimits

startManualTlsRuntimeServerWithStarterAndRequestTransportLimits :: (ListenerEndpoint -> WarpTLS.TLSSettings -> Socket.Socket -> (Observability.ConnectionObservability -> IO ()) -> Wai.Application -> IO ThreadId) -> ManualTlsBindPlan -> Wai.Application -> (Observability.ConnectionObservability -> IO ()) -> IO RunningRuntimeServer
startManualTlsRuntimeServerWithStarterAndRequestTransportLimits startTlsServer manualTlsPlan waiApplication connectionReporter = do
  let tlsLabel =
        case tlsCredentialSourceKind manualTlsPlan of
          ManualTlsCredentials -> "Manual TLS"
          SharedTlsCredentials -> "Shared TLS"
  reloadingTlsCredentials <-
    case tlsStartupMode manualTlsPlan of
      RequireCertificateFiles ->
        loadReloadingTlsCredentialsWithLabel
          tlsLabel
          (tlsCertificateFilePath (tlsCertificateFile manualTlsPlan))
          (tlsPrivateKeyFilePath (tlsPrivateKeyFile manualTlsPlan))
      AwaitCertificateFiles waitTimeoutSeconds ->
        awaitReloadingTlsCredentials
          waitTimeoutSeconds
          (tlsCertificateFilePath (tlsCertificateFile manualTlsPlan))
          (tlsPrivateKeyFilePath (tlsPrivateKeyFile manualTlsPlan))
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
socketPort listeningSocket =
  fromIntegral <$> Socket.socketPort listeningSocket

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

startWarpServerOnSocketWithRequestTransportLimits :: RequestHeadLimits -> RequestTransportLimits -> ListenerEndpoint -> Socket.Socket -> Wai.Application -> IO ThreadId
startWarpServerOnSocketWithRequestTransportLimits requestHeadLimits transportLimits endpoint listeningSocket waiApplication =
  startWarpRuntimeServerOnSocket $ \startupSignal ->
    let settings = runtimeHttpServerSettings requestHeadLimits transportLimits endpoint startupSignal
     in settings `seq` Warp.runSettingsSocket settings listeningSocket waiApplication

startWarpTlsServerOnSocketWithRequestTransportLimits :: RequestHeadLimits -> RequestTransportLimits -> ListenerEndpoint -> WarpTLS.TLSSettings -> Socket.Socket -> (Observability.ConnectionObservability -> IO ()) -> Wai.Application -> IO ThreadId
startWarpTlsServerOnSocketWithRequestTransportLimits requestHeadLimits transportLimits endpoint tlsSettings listeningSocket connectionReporter waiApplication = do
  activeConnectionAddresses <- newActiveConnectionAddresses
  let listenerScheme = Https
  listenerScheme `seq`
    connectionReporter `seq`
      startWarpRuntimeServerOnSocket $ \startupSignal ->
        let settings =
              runtimeServerSettings requestHeadLimits transportLimits listenerScheme endpoint startupSignal activeConnectionAddresses connectionReporter
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

runtimeServerSettings :: RequestHeadLimits -> RequestTransportLimits -> ListenerScheme -> ListenerEndpoint -> MVar (Either SomeException RuntimeServerReady) -> ActiveConnectionAddresses -> (Observability.ConnectionObservability -> IO ()) -> Warp.Settings
runtimeServerSettings requestHeadLimits transportLimits listenerScheme endpoint startupSignal activeConnectionAddresses connectionReporter =
  Warp.setPort (endpointPort endpoint)
    . Warp.setOnException (runtimeConnectionExceptionReporter listenerScheme endpoint activeConnectionAddresses connectionReporter (Warp.getOnException Warp.defaultSettings))
    . Warp.setFork (forkTrackedConnection activeConnectionAddresses)
    . Warp.setOnOpen (registerActiveConnection activeConnectionAddresses)
    . Warp.setOnClose (\_ -> unregisterActiveConnection activeConnectionAddresses)
    $ applyRequestTransportLimits requestHeadLimits transportLimits (Warp.setBeforeMainLoop (putMVar startupSignal (Right RuntimeServerReady)) Warp.defaultSettings)

runtimeHttpServerSettings :: RequestHeadLimits -> RequestTransportLimits -> ListenerEndpoint -> MVar (Either SomeException RuntimeServerReady) -> Warp.Settings
runtimeHttpServerSettings requestHeadLimits transportLimits endpoint startupSignal =
  Warp.setPort (endpointPort endpoint) $
    applyRequestTransportLimits requestHeadLimits transportLimits (Warp.setBeforeMainLoop (putMVar startupSignal (Right RuntimeServerReady)) Warp.defaultSettings)

-- | Warp rejects an oversized header block before it allocates a WAI
-- request.  The WAI request-head gate still checks count and individual
-- values, and is the portable backstop for locally supplied applications.
applyRequestTransportLimits :: RequestHeadLimits -> RequestTransportLimits -> Warp.Settings -> Warp.Settings
applyRequestTransportLimits requestHeadLimits transportLimits =
  maybe id (Warp.setMaxTotalHeaderLength . requestByteLimitValue) (requestHeaderByteLimit requestHeadLimits)
    . maybe id (Warp.setTimeout . requestTimeoutSecondsValue) (requestNetworkTimeout transportLimits)
    . maybe id (Warp.setSlowlorisSize . requestByteLimitValue) (requestSlowlorisByteLimit transportLimits)

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

listenerSocketHints :: Socket.AddrInfo
listenerSocketHints =
  Socket.defaultHints
    { Socket.addrFlags = [Socket.AI_NUMERICHOST, Socket.AI_NUMERICSERV],
      Socket.addrFamily = Socket.AF_INET,
      Socket.addrSocketType = Socket.Stream
    }
