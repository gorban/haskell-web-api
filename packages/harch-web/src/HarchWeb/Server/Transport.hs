{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Private socket, Warp, and TLS runtime mechanics.
--
-- The public entry points remain on the framework facade while ACME owns its
-- certificate-acquisition orchestration. Keeping the transport layer here
-- makes those concerns independently testable without coupling them to the
-- application/WAI request pipeline.
--
-- FQ8 groups the request limits and rendered application shared by every
-- listener in one runtime.  Per-listener endpoint, TLS plan, socket, startup
-- signal, and reporter values stay explicit, so distinct listeners cannot be
-- mistaken for one another while repeated runtime dependencies no longer
-- travel as positional arguments.
module HarchWeb.Server.Transport
  ( ActiveConnectionAddresses,
    ManualTlsServerStarter,
    ReloadingTlsCredentials,
    RuntimeTransportDependencies (..),
    RunningRuntimeServer,
    TlsCertificateFilePath,
    TlsPrivateKeyFilePath,
    ensureRuntimeFileExists,
    acceptTrackedConnection,
    clearPendingAddressForAcceptLoopFailure,
    forkTrackedConnection,
    listenerSchemeText,
    loadReloadingTlsCredentials,
    loadTlsCredentialSnapshotOrThrowWithLoader,
    openListenerSocket,
    openLoopbackSocket,
    newActiveConnectionAddresses,
    recordAcceptLoopThread,
    reloadTlsCredentialsIfChanged,
    socketPort,
    startHttpRuntimeServerWithStarter,
    startHttpRuntimeServers,
    startManualTlsRuntimeServer,
    startManualTlsRuntimeServerWithStarter,
    startManualTlsRuntimeServers,
    startWarpRuntimeServerOnSocket,
    startWarpServerOnSocket,
    stopRuntimeServer,
    stopRuntimeServers,
    tlsCertificateFilePath,
    tlsCertificateFilePathValue,
    tlsPrivateKeyFilePath,
    tlsPrivateKeyFilePathValue,
  )
where

import Control.Concurrent (MVar, ThreadId, forkFinally, forkIOWithUnmask, killThread, myThreadId, newEmptyMVar, putMVar, takeMVar, tryPutMVar, tryReadMVar, tryTakeMVar)
import Control.Exception (SomeException, displayException, evaluate, finally, fromException, onException, throwIO)
import Control.Monad (unless, void, when)
import Data.Either (lefts)
import Data.Foldable (toList)
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

-- | Dependencies shared by all HTTP, manual-TLS, and ACME-adjacent listener
-- starts in one runtime.  Endpoint plans and connection reporters remain
-- separate because they are listener-specific.
data RuntimeTransportDependencies = RuntimeTransportDependencies
  { runtimeTransportRequestHeadLimits :: RequestHeadLimits,
    runtimeTransportRequestLimits :: RequestTransportLimits,
    runtimeTransportApplication :: Wai.Application
  }

-- | Values fixed while one TLS listener is being started.  The shared
-- transport dependencies are distinct from the listener's endpoint,
-- credentials, socket, and connection reporter.
data RuntimeTlsServerStart = RuntimeTlsServerStart
  { runtimeTlsStartTransportDependencies :: RuntimeTransportDependencies,
    runtimeTlsStartEndpoint :: ListenerEndpoint,
    runtimeTlsStartSettings :: WarpTLS.TLSSettings,
    runtimeTlsStartSocket :: Socket.Socket,
    runtimeTlsStartConnectionReporter :: Observability.ConnectionObservability -> IO ()
  }

-- | The complete internal Warp settings environment after startup has
-- allocated the readiness signal and peer tracker for this listener.
data RuntimeTlsListenerDependencies = RuntimeTlsListenerDependencies
  { runtimeTlsListenerTransportDependencies :: RuntimeTransportDependencies,
    runtimeTlsListenerScheme :: ListenerScheme,
    runtimeTlsListenerEndpoint :: ListenerEndpoint,
    runtimeTlsListenerStartupSignal :: MVar (Either SomeException RuntimeServerReady),
    runtimeTlsListenerActiveConnectionAddresses :: ActiveConnectionAddresses,
    runtimeTlsListenerConnectionReporter :: Observability.ConnectionObservability -> IO ()
  }

-- | The narrowly scoped injected operation used to prove listener-socket
-- cleanup. It names one test seam rather than making TLS start dependencies
-- positional at the public helper.
type ManualTlsServerStarter = ListenerEndpoint -> WarpTLS.TLSSettings -> Socket.Socket -> (Observability.ConnectionObservability -> IO ()) -> Wai.Application -> IO ThreadId

data ActiveConnectionAddresses = ActiveConnectionAddresses
  { -- | A one-place handoff from Warp's accept-loop thread to its worker
    -- factory.  Warp 3.3.24+ calls @settingsAccept@ then @settingsFork@
    -- while the accept loop masks asynchronous exceptions, and does not
    -- accept another connection until that factory returns.  This records the
    -- kernel-provided TCP peer at the only public point where it is available
    -- before TLS negotiation, without reimplementing WarpTLS's connection
    -- maker.  A full or empty handoff is a Warp lifecycle-contract violation:
    -- fail closed rather than attach one connection's address to another.
    pendingConnectionAddress :: MVar Socket.SockAddr,
    acceptLoopThreadId :: MVar ThreadId,
    activeConnectionAddresses :: IORef [(ThreadId, Socket.SockAddr)]
  }

-- | Start HTTP listeners with the runtime's shared request transport policy.
startHttpRuntimeServers :: RuntimeTransportDependencies -> [ListenerEndpoint] -> IO [RunningRuntimeServer]
startHttpRuntimeServers dependencies =
  go []
  where
    go runningServers remainingEndpoints =
      case remainingEndpoints of
        [] -> pure (reverse runningServers)
        endpoint : remaining ->
          ( do
              runningServer <- startHttpRuntimeServer dependencies endpoint
              go (runningServer : runningServers) remaining
                `onException` stopRuntimeServers (runningServer : runningServers)
          )
            `onException` stopRuntimeServers runningServers

-- | Start manual-TLS listeners with the runtime's shared request transport
-- policy and an explicit connection reporter.
startManualTlsRuntimeServers :: RuntimeTransportDependencies -> [ManualTlsBindPlan] -> (Observability.ConnectionObservability -> IO ()) -> IO [RunningRuntimeServer]
startManualTlsRuntimeServers dependencies manualTlsPlans connectionReporter =
  connectionReporter `seq` go [] manualTlsPlans
  where
    go runningServers remainingPlans =
      case remainingPlans of
        [] -> pure (reverse runningServers)
        manualTlsPlan : remaining ->
          ( do
              runningServer <- startManualTlsRuntimeServer dependencies manualTlsPlan connectionReporter
              go (runningServer : runningServers) remaining
                `onException` stopRuntimeServers (runningServer : runningServers)
          )
            `onException` stopRuntimeServers runningServers

startHttpRuntimeServer :: RuntimeTransportDependencies -> ListenerEndpoint -> IO RunningRuntimeServer
startHttpRuntimeServer dependencies endpoint =
  startHttpRuntimeServerWithStarter
    ( \listenerEndpoint listeningSocket waiApplication ->
        startWarpServerOnSocket
          (dependencies {runtimeTransportApplication = waiApplication})
          listenerEndpoint
          listeningSocket
    )
    endpoint
    (runtimeTransportApplication dependencies)

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

startManualTlsRuntimeServer :: RuntimeTransportDependencies -> ManualTlsBindPlan -> (Observability.ConnectionObservability -> IO ()) -> IO RunningRuntimeServer
startManualTlsRuntimeServer dependencies manualTlsPlan =
  startManualTlsRuntimeServerWithStarter
    ( \endpoint tlsSettings listeningSocket reporter waiApplication ->
        startWarpTlsServerOnSocket
          RuntimeTlsServerStart
            { runtimeTlsStartTransportDependencies = dependencies {runtimeTransportApplication = waiApplication},
              runtimeTlsStartEndpoint = endpoint,
              runtimeTlsStartSettings = tlsSettings,
              runtimeTlsStartSocket = listeningSocket,
              runtimeTlsStartConnectionReporter = reporter
            }
    )
    manualTlsPlan
    (runtimeTransportApplication dependencies)

startManualTlsRuntimeServerWithStarter :: ManualTlsServerStarter -> ManualTlsBindPlan -> Wai.Application -> (Observability.ConnectionObservability -> IO ()) -> IO RunningRuntimeServer
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
          { WarpTLS.tlsAllowedVersions = map tlsProtocolVersionValue (toList (tlsAllowedVersions (tlsBindPolicy manualTlsPlan))),
            WarpTLS.tlsCiphers = map tlsCipherSuiteValue (toList (tlsCipherSuites (tlsBindPolicy manualTlsPlan))),
            WarpTLS.tlsCredentials = Just initialTlsCredentials,
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

startWarpServerOnSocket :: RuntimeTransportDependencies -> ListenerEndpoint -> Socket.Socket -> IO ThreadId
startWarpServerOnSocket dependencies endpoint listeningSocket =
  startWarpRuntimeServerOnSocket $ \startupSignal ->
    let settings = runtimeHttpServerSettings dependencies endpoint startupSignal
     in settings `seq` Warp.runSettingsSocket settings listeningSocket (runtimeTransportApplication dependencies)

startWarpTlsServerOnSocket :: RuntimeTlsServerStart -> IO ThreadId
startWarpTlsServerOnSocket tlsServerStart = do
  activeConnectionAddresses <- newActiveConnectionAddresses
  let listenerScheme = Https
      dependencies = runtimeTlsStartTransportDependencies tlsServerStart
      endpoint = runtimeTlsStartEndpoint tlsServerStart
      tlsSettings = runtimeTlsStartSettings tlsServerStart
      listeningSocket = runtimeTlsStartSocket tlsServerStart
      connectionReporter = runtimeTlsStartConnectionReporter tlsServerStart
  listenerScheme `seq`
    connectionReporter `seq`
      startWarpRuntimeServerOnSocket $ \startupSignal ->
        let listenerDependencies =
              RuntimeTlsListenerDependencies
                { runtimeTlsListenerTransportDependencies = dependencies,
                  runtimeTlsListenerScheme = listenerScheme,
                  runtimeTlsListenerEndpoint = endpoint,
                  runtimeTlsListenerStartupSignal = startupSignal,
                  runtimeTlsListenerActiveConnectionAddresses = activeConnectionAddresses,
                  runtimeTlsListenerConnectionReporter = connectionReporter
                }
            settings = runtimeServerSettings listenerDependencies
         in settings `seq` WarpTLS.runTLSSocket tlsSettings settings listeningSocket (runtimeTransportApplication dependencies)

startWarpRuntimeServerOnSocket :: (MVar (Either SomeException RuntimeServerReady) -> IO ()) -> IO ThreadId
startWarpRuntimeServerOnSocket runServerOnSocket = do
  startupSignal <- newEmptyMVar
  threadId <-
    forkFinally
      (runServerOnSocket startupSignal)
      (reportRuntimeServerExit startupSignal)
  _ <- waitForRuntimeServerStartup startupSignal
  pure threadId

runtimeServerSettings :: RuntimeTlsListenerDependencies -> Warp.Settings
runtimeServerSettings listenerDependencies =
  Warp.setPort (endpointPort endpoint)
    . Warp.setOnException (runtimeConnectionExceptionReporter listenerDependencies (Warp.getOnException Warp.defaultSettings))
    . Warp.setFork (forkTrackedConnection activeConnectionAddresses)
    . Warp.setAccept (acceptTrackedConnection activeConnectionAddresses)
    $ applyRequestTransportLimits
      (runtimeTransportRequestHeadLimits dependencies)
      (runtimeTransportRequestLimits dependencies)
      (Warp.setBeforeMainLoop (putMVar startupSignal (Right RuntimeServerReady)) Warp.defaultSettings)
  where
    dependencies = runtimeTlsListenerTransportDependencies listenerDependencies
    endpoint = runtimeTlsListenerEndpoint listenerDependencies
    startupSignal = runtimeTlsListenerStartupSignal listenerDependencies
    activeConnectionAddresses = runtimeTlsListenerActiveConnectionAddresses listenerDependencies

runtimeHttpServerSettings :: RuntimeTransportDependencies -> ListenerEndpoint -> MVar (Either SomeException RuntimeServerReady) -> Warp.Settings
runtimeHttpServerSettings dependencies endpoint startupSignal =
  Warp.setPort (endpointPort endpoint) $
    applyRequestTransportLimits
      (runtimeTransportRequestHeadLimits dependencies)
      (runtimeTransportRequestLimits dependencies)
      (Warp.setBeforeMainLoop (putMVar startupSignal (Right RuntimeServerReady)) Warp.defaultSettings)

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
    <$> newEmptyMVar
    <*> newEmptyMVar
    <*> newIORef []

acceptTrackedConnection :: ActiveConnectionAddresses -> Socket.Socket -> IO (Socket.Socket, Socket.SockAddr)
acceptTrackedConnection tracker listeningSocket = do
  recordAcceptLoopThread tracker
  acceptedConnection@(acceptedSocket, socketAddress) <- Socket.accept listeningSocket
  accepted <- tryPutMVar (pendingConnectionAddress tracker) socketAddress
  if accepted
    then pure acceptedConnection
    else do
      Socket.close acceptedSocket
      ioError (userError "Warp peer-address handoff was unexpectedly occupied after accepting a TCP connection")

recordAcceptLoopThread :: ActiveConnectionAddresses -> IO ()
recordAcceptLoopThread tracker = do
  currentThreadId <- myThreadId
  recorded <- tryPutMVar (acceptLoopThreadId tracker) currentThreadId
  unless recorded $ do
    maybeAcceptThreadId <- tryReadMVar (acceptLoopThreadId tracker)
    unless
      (maybeAcceptThreadId == Just currentThreadId)
      (ioError (userError "Warp invoked the peer-address accept hook from multiple accept-loop threads"))

lookupActiveConnectionAddress :: ActiveConnectionAddresses -> IO (Maybe Socket.SockAddr)
lookupActiveConnectionAddress tracker = do
  currentThreadId <- myThreadId
  atomicModifyIORef' (activeConnectionAddresses tracker) (\entries -> (entries, lookup currentThreadId entries))

forkTrackedConnection :: ActiveConnectionAddresses -> (((forall a. IO a -> IO a) -> IO ()) -> IO ())
forkTrackedConnection tracker action = do
  maybeSocketAddress <- tryTakeMVar (pendingConnectionAddress tracker)
  case maybeSocketAddress of
    Nothing ->
      ioError (userError "Warp peer-address handoff was unexpectedly empty while starting a connection worker")
    Just socketAddress ->
      void $
        forkIOWithUnmask $ \unmask -> do
          currentThreadId <- myThreadId
          trackActiveConnection tracker currentThreadId socketAddress
          action unmask `finally` untrackActiveConnection tracker currentThreadId

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

runtimeConnectionExceptionReporter :: RuntimeTlsListenerDependencies -> (Maybe Wai.Request -> SomeException -> IO ()) -> Maybe Wai.Request -> SomeException -> IO ()
runtimeConnectionExceptionReporter listenerDependencies defaultReporter maybeRequest exception = do
  let activeConnectionAddresses = runtimeTlsListenerActiveConnectionAddresses listenerDependencies
      connectionReporter = runtimeTlsListenerConnectionReporter listenerDependencies
  clearPendingAddressForAcceptLoopFailure activeConnectionAddresses
  maybeConnectionObservability <-
    buildConnectionExceptionObservability listenerDependencies exception
  case maybeConnectionObservability of
    Just connectionObservability ->
      Observability.forceConnectionObservability connectionObservability `seq`
        connectionReporter connectionObservability
    Nothing ->
      defaultReporter maybeRequest exception

-- | Warp delivers failures while preparing a just-accepted connection through
-- its normal exception callback.  Only the accept-loop thread may clear this
-- slot: a worker can fail after the loop has accepted a different connection,
-- and clearing then would recreate the cross-connection attribution bug.
clearPendingAddressForAcceptLoopFailure :: ActiveConnectionAddresses -> IO ()
clearPendingAddressForAcceptLoopFailure tracker = do
  currentThreadId <- myThreadId
  maybeAcceptThreadId <- tryReadMVar (acceptLoopThreadId tracker)
  when
    (maybeAcceptThreadId == Just currentThreadId)
    (void (tryTakeMVar (pendingConnectionAddress tracker)))

buildConnectionExceptionObservability :: RuntimeTlsListenerDependencies -> SomeException -> IO (Maybe Observability.ConnectionObservability)
buildConnectionExceptionObservability listenerDependencies exception =
  case fromException exception of
    Just warpTlsException ->
      case warpTlsException of
        WarpTLS.InsecureConnectionDenied ->
          buildConnectionObservabilityValue "insecure-connection-denied" "InsecureConnectionDenied"
        WarpTLS.ClientClosedConnectionPrematurely ->
          buildConnectionObservabilityValue "client-closed-connection-prematurely" "ClientClosedConnectionPrematurely"
    Nothing -> pure Nothing
  where
    listenerScheme = runtimeTlsListenerScheme listenerDependencies
    endpoint = runtimeTlsListenerEndpoint listenerDependencies
    activeConnectionAddresses = runtimeTlsListenerActiveConnectionAddresses listenerDependencies
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
