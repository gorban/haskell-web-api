{-# SPEC #-}

import Control.Concurrent (newEmptyMVar, putMVar, threadDelay, tryTakeMVar)
import Control.Exception (bracket)
import Control.Monad (void)
import Network.HTTP2.Server (allocSimpleConfig', confTimeoutManager, freeSimpleConfig)
import Network.Socket qualified as Socket
import System.TimeManager qualified as TimeManager

-- | HTTP2 owns the timeout manager created by 'allocSimpleConfig''.  Its
-- released 5.4.4 implementation calls the now-no-op 'TimeManager.killManager',
-- leaving callbacks alive after 'freeSimpleConfig'.  The pinned 5.4.0/0.2.4
-- pair preserves the release-time lifecycle contract.  Keep this test at the
-- public HTTP2 boundary so a future upgrade cannot silently give that cleanup
-- guarantee up again (DT, 2026-09-12).
spec =
  describe "HTTP2 simple configuration lifecycle" $
    it "cancels timeout callbacks when freeSimpleConfig releases its manager" $
      withConnectedSockets $ \_clientSocket serverSocket -> do
        configuration <- allocSimpleConfig' serverSocket 4096 10000
        fired <- newEmptyMVar
        void $ TimeManager.register (confTimeoutManager configuration) (putMVar fired ())
        freeSimpleConfig configuration
        threadDelay 100000
        tryTakeMVar fired `shouldReturn` Nothing

withConnectedSockets :: (Socket.Socket -> Socket.Socket -> IO a) -> IO a
withConnectedSockets action =
  Socket.withSocketsDo $
    bracket open Socket.close $ \listeningSocket -> do
      Socket.listen listeningSocket Socket.maxListenQueue
      port <- Socket.socketPort listeningSocket
      bracket (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol) Socket.close $ \clientSocket -> do
        Socket.connect clientSocket (Socket.SockAddrInet port (Socket.tupleToHostAddress (127, 0, 0, 1)))
        bracket (fst <$> Socket.accept listeningSocket) Socket.close (action clientSocket)
  where
    open = do
      listeningSocket <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
      Socket.setSocketOption listeningSocket Socket.ReuseAddr 1
      Socket.bind listeningSocket (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1)))
      pure listeningSocket
