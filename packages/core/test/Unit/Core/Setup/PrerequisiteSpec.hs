{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Exception (finally)
import Core.Setup.Prerequisite qualified as Prerequisite
import Data.Text qualified as Text
import Network.Socket (Family (AF_INET), SockAddr (SockAddrInet), SocketType (Stream), bind, close, defaultProtocol, getSocketName, listen, socket, tupleToHostAddress)

withListeningTcpEndpoint :: (Prerequisite.TcpEndpoint -> IO a) -> IO a
withListeningTcpEndpoint action = do
  listeningSocket <- socket AF_INET Stream defaultProtocol
  bind listeningSocket (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  listen listeningSocket 1
  socketAddress <- getSocketName listeningSocket
  case socketAddress of
    SockAddrInet port _ ->
      action
        Prerequisite.TcpEndpoint
          { Prerequisite.tcpEndpointHost = "127.0.0.1",
            Prerequisite.tcpEndpointPort = fromIntegral port
          }
        `finally` close listeningSocket
    _ -> close listeningSocket >> error "expected IPv4 listening socket"

spec = do
  describe "parseTracingEndpoint" $ do
    it "parses supported tracing URLs into TCP endpoints" $ do
      Prerequisite.parseTracingEndpoint "http://collector:4318/v1/traces"
        `shouldBe` Right
          Prerequisite.TcpEndpoint
            { Prerequisite.tcpEndpointHost = "collector",
              Prerequisite.tcpEndpointPort = 4318
            }
      Prerequisite.parseTracingEndpoint "https://collector.example/v1/traces"
        `shouldBe` Right
          Prerequisite.TcpEndpoint
            { Prerequisite.tcpEndpointHost = "collector.example",
              Prerequisite.tcpEndpointPort = 443
            }
      Prerequisite.parseTracingEndpoint "http://[::1]:4318/v1/traces"
        `shouldBe` Right
          Prerequisite.TcpEndpoint
            { Prerequisite.tcpEndpointHost = "::1",
              Prerequisite.tcpEndpointPort = 4318
            }
      Prerequisite.parseTracingEndpoint "https://[::1]/v1/traces"
        `shouldBe` Right
          Prerequisite.TcpEndpoint
            { Prerequisite.tcpEndpointHost = "::1",
              Prerequisite.tcpEndpointPort = 443
            }
      Prerequisite.parseTracingEndpoint "https://collector/v1/traces"
        `shouldBe` Right
          Prerequisite.TcpEndpoint
            { Prerequisite.tcpEndpointHost = "collector",
              Prerequisite.tcpEndpointPort = 443
            }

    it "rejects malformed or unsupported tracing endpoints explicitly" $ do
      Prerequisite.parseTracingEndpoint "://collector:4318/v1/traces"
        `shouldBe` Left (Prerequisite.InvalidTracingEndpointFormat "://collector:4318/v1/traces")
      Prerequisite.parseTracingEndpoint "collector:4318/v1/traces"
        `shouldBe` Left (Prerequisite.InvalidTracingEndpointFormat "collector:4318/v1/traces")
      Prerequisite.parseTracingEndpoint "grpc://collector:4317"
        `shouldBe` Left (Prerequisite.UnsupportedTracingEndpointScheme "grpc")
      Prerequisite.parseTracingEndpoint "http:///v1/traces"
        `shouldBe` Left Prerequisite.MissingTracingEndpointHost
      Prerequisite.parseTracingEndpoint "http://:4318/v1/traces"
        `shouldBe` Left Prerequisite.MissingTracingEndpointHost
      Prerequisite.parseTracingEndpoint "http://collector:not-a-port/v1/traces"
        `shouldBe` Left (Prerequisite.InvalidTracingEndpointPort "not-a-port")
      Prerequisite.parseTracingEndpoint "http://collector:0/v1/traces"
        `shouldBe` Left (Prerequisite.InvalidTracingEndpointPort "0")
      Prerequisite.parseTracingEndpoint "http://[::1/v1/traces"
        `shouldBe` Left Prerequisite.MissingTracingEndpointHost
      Prerequisite.parseTracingEndpoint "http://[]:4318/v1/traces"
        `shouldBe` Left Prerequisite.MissingTracingEndpointHost
      Prerequisite.parseTracingEndpoint "http://[::1]suffix/v1/traces"
        `shouldBe` Left (Prerequisite.InvalidTracingEndpointFormat "suffix")

    it "keeps parse error equality and rendering deterministic" $ do
      let tcpEndpoint =
            Prerequisite.TcpEndpoint
              { Prerequisite.tcpEndpointHost = "collector",
                Prerequisite.tcpEndpointPort = 4318
              }
          parseError = Prerequisite.InvalidTracingEndpointPort "not-a-port"
      parseError `shouldBe` Prerequisite.InvalidTracingEndpointPort "not-a-port"
      parseError `shouldNotBe` Prerequisite.InvalidTracingEndpointPort "other-port"
      tcpEndpoint `shouldBe` tcpEndpoint
      tcpEndpoint
        `shouldNotBe` Prerequisite.TcpEndpoint
          { Prerequisite.tcpEndpointHost = "other",
            Prerequisite.tcpEndpointPort = 4318
          }
      show tcpEndpoint
        `shouldBe` "TcpEndpoint {tcpEndpointHost = \"collector\", tcpEndpointPort = 4318}"
      showsPrec 11 tcpEndpoint ""
        `shouldBe` "(TcpEndpoint {tcpEndpointHost = \"collector\", tcpEndpointPort = 4318})"
      show [tcpEndpoint]
        `shouldBe` "[TcpEndpoint {tcpEndpointHost = \"collector\", tcpEndpointPort = 4318}]"
      show parseError `shouldBe` "InvalidTracingEndpointPort \"not-a-port\""
      show [parseError] `shouldBe` "[InvalidTracingEndpointPort \"not-a-port\"]"
      show (Prerequisite.InvalidTracingEndpointFormat "bad-endpoint")
        `shouldBe` "InvalidTracingEndpointFormat \"bad-endpoint\""
      show (Prerequisite.UnsupportedTracingEndpointScheme "grpc")
        `shouldBe` "UnsupportedTracingEndpointScheme \"grpc\""
      show Prerequisite.MissingTracingEndpointHost
        `shouldBe` "MissingTracingEndpointHost"

  describe "checkTcpEndpointReachable" $ do
    it "reports True for a reachable local TCP listener" $
      withListeningTcpEndpoint $ \tcpEndpoint ->
        Prerequisite.checkTcpEndpointReachable tcpEndpoint
          `shouldReturn` True

    it "reports False once the TCP listener is gone" $ do
      closedEndpoint <- withListeningTcpEndpoint pure
      Prerequisite.checkTcpEndpointReachable closedEndpoint
        `shouldReturn` False

    it "reports False for invalid resolver inputs or immediate timeout cutoffs" $
      withListeningTcpEndpoint $ \tcpEndpoint -> do
        Prerequisite.checkTcpEndpointReachableWithTimeout
          1000000
          Prerequisite.TcpEndpoint
            { Prerequisite.tcpEndpointHost = Prerequisite.tcpEndpointHost tcpEndpoint,
              Prerequisite.tcpEndpointPort = -1
            }
          `shouldReturn` False
        Prerequisite.checkTcpEndpointReachableWithTimeout 0 tcpEndpoint
          `shouldReturn` False

  describe "checkTracingEndpointReachable" $ do
    it "checks supported tracing endpoints by their parsed TCP host and port" $
      withListeningTcpEndpoint $ \tcpEndpoint -> do
        let endpoint =
              "http://"
                <> Prerequisite.tcpEndpointHost tcpEndpoint
                <> ":"
                <> Text.pack (show (Prerequisite.tcpEndpointPort tcpEndpoint))
                <> "/v1/traces"
        Prerequisite.checkTracingEndpointReachable endpoint
          `shouldReturn` Right True

    it "returns parse errors instead of silently treating malformed tracing endpoints as unreachable" $
      Prerequisite.checkTracingEndpointReachable "collector:4318/v1/traces"
        `shouldReturn` Left (Prerequisite.InvalidTracingEndpointFormat "collector:4318/v1/traces")
