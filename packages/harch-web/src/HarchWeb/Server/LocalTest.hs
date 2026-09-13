{-# LANGUAGE OverloadedStrings #-}

-- | Private loopback-server lifecycle for framework tests and examples.
--
-- The public facade exposes 'LocalTestServer', 'withLocalTestServer', and
-- 'withLocalTestServerForApplication'; raw sockets and server threads stay
-- behind this module boundary. FQ8 constructs the same
-- 'RuntimeTransportDependencies' record as production startup, so loopback
-- tests retain transport-policy parity without rebuilding the listener
-- dependency list positionally.
module HarchWeb.Server.LocalTest
  ( LocalTestServer (..),
    withLocalTestServer,
    withLocalTestServerForApplication,
  )
where

import Control.Concurrent (ThreadId, killThread)
import Control.Exception (bracket, onException)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Security (RequestHeadLimits, RequestTransportLimits, requestHeadLimits, requestTransportLimits, unboundedRequestHeadLimits, warpDefaultRequestTransportLimits)
import HarchWeb.Server.Application (Application (..))
import HarchWeb.Server.Config (ListenerEndpoint (..), ListenerScheme (..))
import HarchWeb.Server.RequestExecution (toWaiApplication)
import HarchWeb.Server.Transport (RuntimeTransportDependencies (..), listenerSchemeText, openLoopbackSocket, socketPort, startWarpServerOnSocket)
import Network.Socket qualified as Socket
import Network.Wai qualified as Wai

data LocalTestServer = LocalTestServer
  { localServerHost :: Text,
    localServerPort :: Int,
    localServerBaseUrl :: Text
  }
  deriving (Eq, Show)

data RunningLocalTestServer = RunningLocalTestServer
  { runningLocalServerInfo :: LocalTestServer,
    runningLocalServerSocket :: Socket.Socket,
    runningLocalServerThreadId :: ThreadId
  }

-- | Serve a typed 'Application' over a real loopback HTTP listener for the
-- lifetime of the callback, honoring its 'RequestPolicyConfig' exactly as
-- 'HarchWeb.Server.Runtime' would — including its concurrency admission
-- gate, which 'toWaiApplication' now applies unconditionally — so a
-- real-socket test observes the same admission behaviour a deployed
-- runtime would, not a narrower test-only approximation of it.
withLocalTestServer :: (Eq route) => Application route action context authorization -> (LocalTestServer -> IO a) -> IO a
withLocalTestServer webApplication useLocalServer = do
  gatedWaiApplication <- toWaiApplication webApplication
  withLocalTestServerWithRequestHeadLimits
    (requestHeadLimits (applicationRequestPolicy webApplication))
    (requestTransportLimits (applicationRequestPolicy webApplication))
    gatedWaiApplication
    useLocalServer

-- | Serve an already-built 'Wai.Application' over a real loopback HTTP
-- listener for the lifetime of the callback, e.g. one an application built
-- directly with 'toWaiApplication' and its own 'HarchWeb.Site.Site'. Prefer
-- 'withLocalTestServer' when a typed application is available. This raw-WAI
-- helper deliberately uses transport defaults: an opaque function carries no
-- 'HarchWeb.RequestPolicyConfig' to apply. Do not use it as a substitute for
-- policy-parity testing.
withLocalTestServerForApplication :: Wai.Application -> (LocalTestServer -> IO a) -> IO a
withLocalTestServerForApplication =
  withLocalTestServerWithRequestHeadLimits unboundedRequestHeadLimits warpDefaultRequestTransportLimits

withLocalTestServerWithRequestHeadLimits :: RequestHeadLimits -> RequestTransportLimits -> Wai.Application -> (LocalTestServer -> IO a) -> IO a
withLocalTestServerWithRequestHeadLimits requestHeadLimits transportLimits waiApplication useLocalServer =
  bracket (startLocalTestServer requestHeadLimits transportLimits waiApplication) stopLocalTestServer $
    useLocalServer . runningLocalServerInfo

startLocalTestServer :: RequestHeadLimits -> RequestTransportLimits -> Wai.Application -> IO RunningLocalTestServer
startLocalTestServer requestHeadLimits transportLimits waiApplication = do
  listeningSocket <- openLoopbackSocket
  localPort <- socketPort listeningSocket
  let listenerScheme = Http
      endpoint = ListenerEndpoint {endpointHost = "127.0.0.1", endpointPort = localPort}
      transportDependencies =
        RuntimeTransportDependencies
          { runtimeTransportRequestHeadLimits = requestHeadLimits,
            runtimeTransportRequestLimits = transportLimits,
            runtimeTransportApplication = waiApplication
          }
  serverThreadId <-
    endpointHost endpoint `seq`
      startWarpServerOnSocket transportDependencies endpoint listeningSocket
        `onException` Socket.close listeningSocket
  localPort `seq`
    pure
      RunningLocalTestServer
        { runningLocalServerInfo =
            LocalTestServer
              { localServerHost = endpointHost endpoint,
                localServerPort = localPort,
                localServerBaseUrl = listenerSchemeText listenerScheme <> "://" <> endpointHost endpoint <> ":" <> Text.pack (show localPort)
              },
          runningLocalServerSocket = listeningSocket,
          runningLocalServerThreadId = serverThreadId
        }

stopLocalTestServer :: RunningLocalTestServer -> IO ()
stopLocalTestServer runningServer = do
  Socket.close (runningLocalServerSocket runningServer)
  killThread (runningLocalServerThreadId runningServer)
