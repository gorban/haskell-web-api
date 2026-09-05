{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception ()
import Control.Monad ()
import Data.ByteString qualified as ByteString ()
import Data.ByteString.Builder qualified as Builder ()
import Data.ByteString.Char8 qualified as ByteStringChar8 ()
import Data.ByteString.Lazy qualified as LazyByteString ()
import Data.Char ()
import Data.Either ()
import Data.Functor.Compose ()
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (isInfixOf, isPrefixOf)
import Data.List.NonEmpty ()
import Data.Maybe ()
import Data.Text ()
import Data.Text qualified as Text ()
import Data.Text.Encoding qualified as TextEncoding ()
import HarchWeb (ListenerEndpoint (ListenerEndpoint, endpointHost, endpointPort), ManualTlsBindPlan (ManualTlsBindPlan, tlsBindPolicy, tlsCertificateFile, tlsCredentialSourceKind, tlsEndpoint, tlsPrivateKeyFile, tlsStartupMode), TlsCredentialSourceKind (ManualTlsCredentials, SharedTlsCredentials), TlsStartupMode (AwaitCertificateFiles, RequireCertificateFiles), defaultTlsPolicy, startHttpRuntimeServerWithStarter, startManualTlsRuntimeServerWithStarter, startWarpRuntimeServerOnSocket, toWaiApplication)
import HarchWeb.Action qualified as Action ()
import HarchWeb.Database qualified as Database ()
import HarchWeb.Markup.Unsafe qualified as MarkupUnsafe ()
import HarchWeb.Observability qualified as Observability ()
import HarchWeb.Security qualified as Security ()
import Network.HTTP.Client qualified as HttpClient ()
import Network.HTTP.Types qualified as Http ()
import Network.Socket qualified as Socket (Family (AF_INET), SockAddr (SockAddrInet), SocketType (Stream), bind, close, defaultProtocol, socket, tupleToHostAddress)
import Network.Socket.ByteString qualified as SocketByteString ()
import Network.TLS qualified as TLS
import Network.Wai qualified as Wai ()
import Network.Wai.Handler.Warp qualified as Warp ()
import Network.Wai.Handler.WarpTLS qualified as WarpTLS
import System.Directory ()
import System.Environment ()
import System.Exit ()
import System.FilePath ((</>))
import System.IO ()
import System.IO.Error ()
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Signals ()
import System.Process ()
import TestCore.CustomAssertions ()
import TestCore.Wai ()
import Text.Read ()
import Unit.HarchWeb.TestSupport (manualTlsCertificatePem, manualTlsPrivateKeyPem, sampleApplication, withManualTlsFiles, withUnusedLoopbackPort)

spec = do
  describe "startWarpRuntimeServerOnSocket" $ do
    it "surfaces startup exceptions that happen before the runtime server becomes ready" $
      startWarpRuntimeServerOnSocket (\_ -> ioError (userError "synthetic runtime startup failure"))
        `shouldThrow` (\exception -> show (exception :: IOError) == "user error (synthetic runtime startup failure)")

  describe "startManualTlsRuntimeServerWithStarter" $ do
    it "applies the validated modern TLS policy to WarpTLS settings" $
      withUnusedLoopbackPort $ \httpsPort ->
        withManualTlsFiles $ \certificatePath privateKeyPath -> do
          observedSettings <- newIORef Nothing
          let manualTlsPlan =
                ManualTlsBindPlan
                  { tlsEndpoint = ListenerEndpoint {endpointHost = "127.0.0.1", endpointPort = httpsPort},
                    tlsCertificateFile = certificatePath,
                    tlsPrivateKeyFile = privateKeyPath,
                    tlsCredentialSourceKind = ManualTlsCredentials,
                    tlsStartupMode = RequireCertificateFiles,
                    tlsBindPolicy = defaultTlsPolicy
                  }
          gatedApplication <- toWaiApplication sampleApplication
          _ <-
            startManualTlsRuntimeServerWithStarter
              ( \_ tlsSettings listeningSocket _ _ -> do
                  writeIORef observedSettings (Just tlsSettings)
                  Socket.close listeningSocket
                  forkIO (pure ())
              )
              manualTlsPlan
              gatedApplication
              (const (pure ()))
          maybeSettings <- readIORef observedSettings
          fmap WarpTLS.tlsAllowedVersions maybeSettings `shouldBe` Just [TLS.TLS12, TLS.TLS13]
          fmap (map show . WarpTLS.tlsCiphers) maybeSettings
            `shouldBe` Just
              [ "ECDHE-ECDSA-AES256GCM-SHA384",
                "ECDHE-ECDSA-CHACHA20POLY1305-SHA256",
                "ECDHE-ECDSA-AES128GCM-SHA256",
                "ECDHE-RSA-AES256GCM-SHA384",
                "ECDHE-RSA-CHACHA20POLY1305-SHA256",
                "ECDHE-RSA-AES128GCM-SHA256",
                "AES256GCM-SHA384",
                "CHACHA20POLY1305-SHA256",
                "AES128GCM-SHA256"
              ]

    it "closes the listener socket when TLS startup throws before the server thread starts" $
      withUnusedLoopbackPort $ \httpsPort ->
        withManualTlsFiles $ \certificatePath privateKeyPath -> do
          let manualTlsPlan =
                ManualTlsBindPlan
                  { tlsEndpoint = ListenerEndpoint {endpointHost = "127.0.0.1", endpointPort = httpsPort},
                    tlsCertificateFile = certificatePath,
                    tlsPrivateKeyFile = privateKeyPath,
                    tlsCredentialSourceKind = ManualTlsCredentials,
                    tlsStartupMode = RequireCertificateFiles,
                    tlsBindPolicy = defaultTlsPolicy
                  }
          gatedApplication <- toWaiApplication sampleApplication
          startManualTlsRuntimeServerWithStarter
            (\_ _ _ _ _ -> ioError (userError "synthetic tls startup failure"))
            manualTlsPlan
            gatedApplication
            (const (pure ()))
            `shouldThrow` (\exception -> show (exception :: IOError) == "user error (synthetic tls startup failure)")
          reboundSocket <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
          Socket.bind reboundSocket (Socket.SockAddrInet (fromIntegral httpsPort) (Socket.tupleToHostAddress (127, 0, 0, 1)))
          Socket.close reboundSocket

    it "fails explicitly when shared TLS is configured to fail fast and the certificate files are missing" $
      withUnusedLoopbackPort $ \httpsPort ->
        withSystemTempDirectory "harch-web-shared-fail-fast" $ \sharedDirectory -> do
          let (certificatePath, privateKeyPath) = (sharedDirectory </> "fullchain.pem", sharedDirectory </> "privkey.pem")
              manualTlsPlan =
                ManualTlsBindPlan
                  { tlsEndpoint = ListenerEndpoint {endpointHost = "127.0.0.1", endpointPort = httpsPort},
                    tlsCertificateFile = certificatePath,
                    tlsPrivateKeyFile = privateKeyPath,
                    tlsCredentialSourceKind = SharedTlsCredentials,
                    tlsStartupMode = RequireCertificateFiles,
                    tlsBindPolicy = defaultTlsPolicy
                  }
          gatedApplication <- toWaiApplication sampleApplication
          startManualTlsRuntimeServerWithStarter
            (\_ _ _ _ _ -> expectationFailure "unexpected TLS starter invocation" >> pure undefined)
            manualTlsPlan
            gatedApplication
            (const (pure ()))
            `shouldThrow` (\exception -> show (exception :: IOError) == "user error (Shared TLS certificate file does not exist: " <> certificatePath <> ")")

    it "fails explicitly when shared TLS wait mode reaches its configured timeout" $
      withUnusedLoopbackPort $ \httpsPort ->
        withSystemTempDirectory "harch-web-shared-timeout" $ \sharedDirectory -> do
          let (certificatePath, privateKeyPath) = (sharedDirectory </> "fullchain.pem", sharedDirectory </> "privkey.pem")
              manualTlsPlan =
                ManualTlsBindPlan
                  { tlsEndpoint = ListenerEndpoint {endpointHost = "127.0.0.1", endpointPort = httpsPort},
                    tlsCertificateFile = certificatePath,
                    tlsPrivateKeyFile = privateKeyPath,
                    tlsCredentialSourceKind = SharedTlsCredentials,
                    tlsStartupMode = AwaitCertificateFiles (Just 0),
                    tlsBindPolicy = defaultTlsPolicy
                  }
          gatedApplication <- toWaiApplication sampleApplication
          startManualTlsRuntimeServerWithStarter
            (\_ _ _ _ _ -> expectationFailure "unexpected TLS starter invocation" >> pure undefined)
            manualTlsPlan
            gatedApplication
            (const (pure ()))
            `shouldThrow` (\exception -> show (exception :: IOError) == "user error (Timed out waiting for shared TLS certificate files at " <> certificatePath <> " and " <> privateKeyPath <> " after 0 seconds)")

    it "includes shared TLS loader errors when wait mode times out on invalid certificate files" $
      withUnusedLoopbackPort $ \httpsPort ->
        withSystemTempDirectory "harch-web-shared-invalid-timeout" $ \sharedDirectory -> do
          let (certificatePath, privateKeyPath) = (sharedDirectory </> "fullchain.pem", sharedDirectory </> "privkey.pem")
              manualTlsPlan =
                ManualTlsBindPlan
                  { tlsEndpoint = ListenerEndpoint {endpointHost = "127.0.0.1", endpointPort = httpsPort},
                    tlsCertificateFile = certificatePath,
                    tlsPrivateKeyFile = privateKeyPath,
                    tlsCredentialSourceKind = SharedTlsCredentials,
                    tlsStartupMode = AwaitCertificateFiles (Just 0),
                    tlsBindPolicy = defaultTlsPolicy
                  }
          writeFile certificatePath "not a certificate"
          writeFile privateKeyPath "not a private key"
          gatedApplication <- toWaiApplication sampleApplication
          startManualTlsRuntimeServerWithStarter
            (\_ _ _ _ _ -> expectationFailure "unexpected TLS starter invocation" >> pure undefined)
            manualTlsPlan
            gatedApplication
            (const (pure ()))
            `shouldThrow` ( \exception ->
                              let renderedException = show (exception :: IOError)
                               in length renderedException `seq`
                                    ( "user error (Timed out waiting for shared TLS credentials at " `isPrefixOf` renderedException
                                        && certificatePath `isInfixOf` renderedException
                                        && privateKeyPath `isInfixOf` renderedException
                                        && " after 0 seconds: " `isInfixOf` renderedException
                                    )
                          )

    it "keeps retrying shared TLS wait mode until a nonzero timeout expires" $
      withUnusedLoopbackPort $ \httpsPort ->
        withSystemTempDirectory "harch-web-shared-retrying-timeout" $ \sharedDirectory -> do
          let (certificatePath, privateKeyPath) = (sharedDirectory </> "fullchain.pem", sharedDirectory </> "privkey.pem")
              manualTlsPlan =
                ManualTlsBindPlan
                  { tlsEndpoint = ListenerEndpoint {endpointHost = "127.0.0.1", endpointPort = httpsPort},
                    tlsCertificateFile = certificatePath,
                    tlsPrivateKeyFile = privateKeyPath,
                    tlsCredentialSourceKind = SharedTlsCredentials,
                    tlsStartupMode = AwaitCertificateFiles (Just 1),
                    tlsBindPolicy = defaultTlsPolicy
                  }
          writeFile certificatePath "not a certificate"
          writeFile privateKeyPath "not a private key"
          gatedApplication <- toWaiApplication sampleApplication
          startManualTlsRuntimeServerWithStarter
            (\_ _ _ _ _ -> expectationFailure "unexpected TLS starter invocation" >> pure undefined)
            manualTlsPlan
            gatedApplication
            (const (pure ()))
            `shouldThrow` ( \exception ->
                              let renderedException = show (exception :: IOError)
                               in length renderedException `seq`
                                    ( "user error (Timed out waiting for shared TLS credentials at " `isPrefixOf` renderedException
                                        && certificatePath `isInfixOf` renderedException
                                        && privateKeyPath `isInfixOf` renderedException
                                        && " after 1 seconds: " `isInfixOf` renderedException
                                    )
                          )

    it "waits for shared TLS certificate files before invoking the TLS starter" $
      withUnusedLoopbackPort $ \httpsPort ->
        withSystemTempDirectory "harch-web-shared-wait-starter" $ \sharedDirectory -> do
          starterInvoked <- newIORef False
          let (certificatePath, privateKeyPath) = (sharedDirectory </> "fullchain.pem", sharedDirectory </> "privkey.pem")
              manualTlsPlan =
                ManualTlsBindPlan
                  { tlsEndpoint = ListenerEndpoint {endpointHost = "127.0.0.1", endpointPort = httpsPort},
                    tlsCertificateFile = certificatePath,
                    tlsPrivateKeyFile = privateKeyPath,
                    tlsCredentialSourceKind = SharedTlsCredentials,
                    tlsStartupMode = AwaitCertificateFiles Nothing,
                    tlsBindPolicy = defaultTlsPolicy
                  }
          _ <- forkIO $ do
            threadDelay 100000
            writeFile certificatePath manualTlsCertificatePem
            writeFile privateKeyPath manualTlsPrivateKeyPem
          gatedApplication <- toWaiApplication sampleApplication
          _ <-
            startManualTlsRuntimeServerWithStarter
              (\_ _ socket _ _ -> writeIORef starterInvoked True >> Socket.close socket >> forkIO (pure ()))
              manualTlsPlan
              gatedApplication
              (const (pure ()))
          readIORef starterInvoked `shouldReturn` True

  describe "startHttpRuntimeServerWithStarter" $ do
    it "closes the listener socket when HTTP startup throws before the server thread starts" $
      withUnusedLoopbackPort $ \httpPort -> do
        let endpoint = ListenerEndpoint {endpointHost = "127.0.0.1", endpointPort = httpPort}
        gatedApplication <- toWaiApplication sampleApplication
        startHttpRuntimeServerWithStarter
          (\_ _ _ -> ioError (userError "synthetic HTTP startup failure"))
          endpoint
          gatedApplication
          `shouldThrow` (\exception -> show (exception :: IOError) == "user error (synthetic HTTP startup failure)")
        reboundSocket <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
        Socket.bind reboundSocket (Socket.SockAddrInet (fromIntegral httpPort) (Socket.tupleToHostAddress (127, 0, 0, 1)))
        Socket.close reboundSocket
