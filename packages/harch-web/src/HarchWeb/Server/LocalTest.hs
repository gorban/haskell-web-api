{-# LANGUAGE OverloadedStrings #-}

-- | Private loopback-server lifecycle for framework tests and examples.
--
-- The public facade exposes 'LocalTestServer' and 'withLocalTestServer'; raw
-- sockets and server threads stay behind this module boundary.
module HarchWeb.Server.LocalTest
  ( LocalTestServer (..),
    withLocalTestServer,
  )
where

import Control.Concurrent (ThreadId, killThread)
import Control.Exception (bracket)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Server.Application (Application)
import HarchWeb.Server.Config (ListenerEndpoint (..), ListenerScheme (..))
import HarchWeb.Server.RequestExecution (toWaiApplication)
import HarchWeb.Server.Transport (listenerSchemeText, openLoopbackSocket, socketPort, startWarpServerOnSocket)
import Network.Socket qualified as Socket

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
withLocalTestServer webApplication useLocalServer =
  bracket (startLocalTestServer webApplication) stopLocalTestServer $
    useLocalServer . runningLocalServerInfo

startLocalTestServer :: (Eq route) => Application route action context -> IO RunningLocalTestServer
startLocalTestServer webApplication = do
  listeningSocket <- openLoopbackSocket
  localPort <- socketPort listeningSocket
  let listenerScheme = Http
      endpoint = ListenerEndpoint {endpointHost = "127.0.0.1", endpointPort = localPort}
  serverThreadId <-
    endpointHost endpoint `seq`
      startWarpServerOnSocket endpoint listeningSocket (toWaiApplication webApplication)
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
