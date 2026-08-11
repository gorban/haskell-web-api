{-# LANGUAGE OverloadedStrings #-}

-- | Private loopback-server lifecycle for framework tests and examples.
--
-- The public facade exposes 'LocalTestServer', 'withLocalTestServer', and
-- 'withLocalTestServerForApplication'; raw sockets and server threads stay
-- behind this module boundary.
module HarchWeb.Server.LocalTest
  ( LocalTestServer (..),
    withLocalTestServer,
    withLocalTestServerForApplication,
  )
where

import Control.Concurrent (ThreadId, killThread)
import Control.Exception (bracket)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Security (RequestHeadLimits, RequestTransportLimits, requestHeadLimits, requestTransportLimits, unboundedRequestHeadLimits, warpDefaultRequestTransportLimits)
import HarchWeb.Server.Application (Application (..))
import HarchWeb.Server.Config (ListenerEndpoint (..), ListenerScheme (..))
import HarchWeb.Server.RequestExecution (toWaiApplication)
import HarchWeb.Server.Transport (listenerSchemeText, openLoopbackSocket, socketPort, startWarpServerOnSocketWithRequestTransportLimits)
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

withLocalTestServer :: (Eq route) => Application route action context -> (LocalTestServer -> IO a) -> IO a
withLocalTestServer webApplication =
  withLocalTestServerWithRequestHeadLimits
    (requestHeadLimits (applicationRequestPolicy webApplication))
    (requestTransportLimits (applicationRequestPolicy webApplication))
    (toWaiApplication webApplication)

-- | Serve an already-built 'Wai.Application' over a real loopback HTTP
-- listener for the lifetime of the callback, e.g. one composed from
-- 'HarchWeb.Api.apiEndpointMiddleware' wrapping 'toWaiApplication'. Prefer
-- 'withLocalTestServer' when no such composition is needed.
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
  serverThreadId <-
    endpointHost endpoint `seq`
      startWarpServerOnSocketWithRequestTransportLimits requestHeadLimits transportLimits endpoint listeningSocket waiApplication
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
