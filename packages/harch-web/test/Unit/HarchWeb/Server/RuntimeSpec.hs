{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Concurrent (forkIO, killThread, newEmptyMVar, newMVar, putMVar, readMVar, threadDelay)
import Control.Exception (Exception (displayException), SomeException, finally, try)
import Control.Monad ()
import Data.ByteString qualified as ByteString (empty, isInfixOf)
import Data.ByteString.Builder qualified as Builder ()
import Data.ByteString.Char8 qualified as ByteStringChar8 ()
import Data.ByteString.Lazy qualified as LazyByteString ()
import Data.Char ()
import Data.Either ()
import Data.Functor.Compose ()
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (find, isInfixOf, isPrefixOf, isSuffixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isNothing)
import Data.Text ()
import Data.Text qualified as Text (isInfixOf, isPrefixOf, pack)
import Data.Text.Encoding qualified as TextEncoding ()
import HarchWeb (AcmeChallengeStore (AcmeChallengeStore), AcmeConfig (AcmeConfig, acmeCertbotConfig, acmeCertificateDirectory, acmeContactEmails, acmeDirectoryUrl, acmeDomains, acmeHttp01Port), Application (applicationRequestPolicy, reportApplicationLog, reportConnectionObservability, reportRequestObservability, requestContextFromRequest), CertbotConfig (CertbotConfig, certbotArguments, certbotExecutable), ListenerConfig (ListenerConfig, listenerAcme, listenerHost, listenerPort, listenerScheme, listenerTls), ListenerScheme (Http, Https), ManualTlsBindPlan, ManualTlsCertificateFiles (ManualTlsCertificateFiles, certificateFile, privateKeyFile), RequestPolicyConfig (forwardedHeaderTrust), TlsCertificateSource (AcmeCertificateSource, ManualCertificateFiles), TlsConfig (TlsConfig, certificateSource), acmeChallengeResponseForRequest, newCertbotWebrootStore, prepareCertbotManualTlsBindPlan, runServer, runServerWithWaiMiddleware, validAcmeHttp01ChallengeToken)
import HarchWeb.Action qualified as Action ()
import HarchWeb.Database qualified as Database ()
import HarchWeb.Markup.Unsafe qualified as MarkupUnsafe ()
import HarchWeb.Observability qualified as Observability (ConnectionObservability (observabilityConnectionSpan), ObservabilityAttribute (ObservabilityAttribute, attributeName, attributeValue), ObservabilityAttributeValue (TextAttribute), RequestIdentity (RequestIdentity, requestIdentityMethod, requestIdentityPath, requestIdentityRoutePath, requestIdentityScheme), RequestSpan (requestSpanAttributes, requestSpanDisplayName), ResponseKind (BodyResponseKind), buildRequestObservability, mkSpanMethodLabel, mkSpanRoutePath)
import HarchWeb.Security qualified as Security ()
import Network.HTTP.Client qualified as HttpClient ()
import Network.HTTP.Types qualified as Http (status200)
import Network.Socket qualified as Socket (AddrInfo (addrAddress, addrFlags), AddrInfoFlag (AI_PASSIVE), SocketOption (ReuseAddr), bind, close, defaultHints, getAddrInfo, listen, maxListenQueue, openSocket, setSocketOption)
import Network.Socket.ByteString qualified as SocketByteString ()
import Network.Wai qualified as Wai (Request (rawPathInfo, requestHeaders), defaultRequest, responseLBS)
import Network.Wai.Handler.Warp qualified as Warp ()
import System.Directory (doesFileExist, removePathForcibly)
import System.Environment ()
import System.Exit ()
import System.FilePath ((</>))
import System.IO (hClose)
import System.IO.Error (isAlreadyInUseError)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import System.Posix.Signals (raiseSignal, sigINT, sigTERM)
import System.Process ()
import TestCore.CustomAssertions ()
import TestCore.Wai ()
import Text.Read ()
import Unit.HarchWeb.TestSupport (acmeHttpsListener, acmeHttpsListenerWithContacts, acmeHttpsListenerWithDomains, acmeHttpsListenerWithDomainsAndChallengePort, certbotHttp01Backend, certbotHttp01BackendWithExecutable, connectAndCloseLoopbackSocket, defaultRequestPolicy, expectLoopbackPortReusable, expectMeasuredRootRequestTiming, fakeCertbotScriptPreamble, hasTextAttribute, httpRuntimeListener, manualTlsCertificatePem, manualTlsPrivateKeyPem, readLoopbackHttpResponse, readLoopbackHttpResponseBytesWithHostAndHeadersResult, readLoopbackHttpResponseBytesWithHostResult, readLoopbackHttpsResponse, readLoopbackHttpsResponseResult, runtimeAcmePlanWithCertbotConfig, sampleApplication, sampleRequestContextFromRequest, serverConfigWithListeners, sharedHttpsListener, stripVolatileRequestTiming, testTrustedForwardedProxy, waitForConnectionObservability, waitForHttpsServerResponse, waitForServerExit, waitForServerResponse, withCustomFakeCertbotExecutable, withEmptyExecutablePath, withFailingFakeCertbotExecutable, withFakeCertbotExecutable, withManualTlsFiles, withOccupiedLoopbackPort, withUnusedLoopbackPort)

spec = do
  describe "runServer" $ do
    it "serves responses on the configured HTTP listener and stays running until signalled to stop" $
      withUnusedLoopbackPort $ \unusedPort ->
        withSystemTempFile "harch-web-output.txt" $ \outputPath outputHandle -> do
          completionReference <- newIORef Nothing
          let runtimeConfig =
                serverConfigWithListeners
                  [ ListenerConfig
                      { listenerHost = "127.0.0.1",
                        listenerPort = unusedPort,
                        listenerScheme = Http,
                        listenerTls = Nothing,
                        listenerAcme = Nothing
                      }
                  ]
          serverThreadId <- forkIO $ do
            result <- try (runServer outputHandle runtimeConfig sampleApplication) :: IO (Either SomeException ())
            writeIORef completionReference (Just result)
          firstResponseText <- waitForServerResponse completionReference unusedPort "/known"
          Text.isInfixOf "<h1>Known</h1>" firstResponseText `shouldBe` True
          threadDelay 50000
          secondResponseText <- readLoopbackHttpResponse unusedPort "/known"
          Text.isInfixOf "<h1>Known</h1>" secondResponseText `shouldBe` True
          completionResult <- readIORef completionReference
          completionResult `shouldSatisfy` isNothing
          killThread serverThreadId
          waitForServerExit completionReference
          hClose outputHandle
          readFile outputPath `shouldReturn` ("HTTP Server listening at http://127.0.0.1:" <> show unusedPort <> "\n")

    it "stops listeners and returns normally when it receives SIGTERM" $
      withUnusedLoopbackPort $ \unusedPort ->
        withSystemTempFile "harch-web-sigterm-output.txt" $ \_ outputHandle -> do
          completionReference <- newIORef Nothing
          let runtimeConfig =
                serverConfigWithListeners
                  [ ListenerConfig
                      { listenerHost = "127.0.0.1",
                        listenerPort = unusedPort,
                        listenerScheme = Http,
                        listenerTls = Nothing,
                        listenerAcme = Nothing
                      }
                  ]
          _ <- forkIO $ do
            result <- try (runServer outputHandle runtimeConfig sampleApplication) :: IO (Either SomeException ())
            writeIORef completionReference (Just result)
          _ <- waitForServerResponse completionReference unusedPort "/known"
          raiseSignal sigTERM
          waitForServerExit completionReference
          completionResult <- readIORef completionReference
          case completionResult of
            Just (Right ()) -> pure ()
            Just (Left exception) -> expectationFailure ("expected SIGTERM shutdown to succeed, but got: " <> displayException exception)
            Nothing -> expectationFailure "expected SIGTERM shutdown to complete"
          hClose outputHandle

    it "stops listeners and returns normally when it receives SIGINT" $
      withUnusedLoopbackPort $ \unusedPort ->
        withSystemTempFile "harch-web-sigint-output.txt" $ \_ outputHandle -> do
          completionReference <- newIORef Nothing
          let runtimeConfig =
                serverConfigWithListeners
                  [ ListenerConfig
                      { listenerHost = "127.0.0.1",
                        listenerPort = unusedPort,
                        listenerScheme = Http,
                        listenerTls = Nothing,
                        listenerAcme = Nothing
                      }
                  ]
          _ <- forkIO $ do
            result <- try (runServer outputHandle runtimeConfig sampleApplication) :: IO (Either SomeException ())
            writeIORef completionReference (Just result)
          _ <- waitForServerResponse completionReference unusedPort "/known"
          raiseSignal sigINT
          waitForServerExit completionReference
          completionResult <- readIORef completionReference
          case completionResult of
            Just (Right ()) -> pure ()
            Just (Left exception) -> expectationFailure ("expected SIGINT shutdown to succeed, but got: " <> displayException exception)
            Nothing -> expectationFailure "expected SIGINT shutdown to complete"
          hClose outputHandle

    it "fails before startup when the listener plan is invalid" $
      withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
        let invalidConfig =
              serverConfigWithListeners
                [ ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5001,
                      listenerScheme = Https,
                      listenerTls = Nothing,
                      listenerAcme = Nothing
                    }
                ]
        runServer outputHandle invalidConfig sampleApplication
          `shouldThrow` (\exception -> show (exception :: IOError) == "user error (Invalid listener startup plan: InvalidListenerTlsConfiguration (ListenerConfig {listenerHost = \"127.0.0.1\", listenerPort = 5001, listenerScheme = Https, listenerTls = Nothing}))")

    it "serves responses on the configured manual TLS listener and stays running until signalled to stop" $
      withUnusedLoopbackPort $ \unusedPort ->
        withManualTlsFiles $ \certificatePath privateKeyPath ->
          withSystemTempFile "harch-web-output.txt" $ \outputPath outputHandle -> do
            completionReference <- newIORef Nothing
            let manualTlsConfig =
                  serverConfigWithListeners
                    [ ListenerConfig
                        { listenerHost = "127.0.0.1",
                          listenerPort = unusedPort,
                          listenerScheme = Https,
                          listenerTls =
                            Just
                              TlsConfig
                                { certificateSource =
                                    ManualCertificateFiles
                                      ManualTlsCertificateFiles
                                        { certificateFile = certificatePath,
                                          privateKeyFile = privateKeyPath
                                        }
                                },
                          listenerAcme = Nothing
                        }
                    ]
            serverThreadId <- forkIO $ do
              result <- try (runServer outputHandle manualTlsConfig sampleApplication) :: IO (Either SomeException ())
              writeIORef completionReference (Just result)
            firstResponseText <- waitForHttpsServerResponse completionReference unusedPort "/known"
            Text.isInfixOf "<h1>Known</h1>" firstResponseText `shouldBe` True
            threadDelay 50000
            secondResponseText <- readLoopbackHttpsResponse unusedPort "/known"
            Text.isInfixOf "<h1>Known</h1>" secondResponseText `shouldBe` True
            completionResult <- readIORef completionReference
            completionResult `shouldSatisfy` isNothing
            killThread serverThreadId
            waitForServerExit completionReference
            hClose outputHandle
            readFile outputPath `shouldReturn` ("HTTPS Server listening at https://127.0.0.1:" <> show unusedPort <> "\n")

    it "reports plaintext connections to an HTTPS listener as connection observability with peer addresses" $
      withUnusedLoopbackPort $ \unusedPort ->
        withManualTlsFiles $ \certificatePath privateKeyPath ->
          withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
            completionReference <- newIORef Nothing
            connectionObservabilityReference <- newIORef []
            let observingApplication =
                  sampleApplication
                    { reportConnectionObservability = \connectionObservabilityValue ->
                        modifyIORef' connectionObservabilityReference (connectionObservabilityValue :)
                    }
                manualTlsConfig =
                  serverConfigWithListeners
                    [ ListenerConfig
                        { listenerHost = "127.0.0.1",
                          listenerPort = unusedPort,
                          listenerScheme = Https,
                          listenerTls =
                            Just
                              TlsConfig
                                { certificateSource =
                                    ManualCertificateFiles
                                      ManualTlsCertificateFiles
                                        { certificateFile = certificatePath,
                                          privateKeyFile = privateKeyPath
                                        }
                                },
                          listenerAcme = Nothing
                        }
                    ]
            serverThreadId <- forkIO $ do
              result <- try (runServer outputHandle manualTlsConfig observingApplication) :: IO (Either SomeException ())
              writeIORef completionReference (Just result)
            _ <- waitForHttpsServerResponse completionReference unusedPort "/known"
            _ <- readLoopbackHttpResponseBytesWithHostResult unusedPort "127.0.0.1" "/known"
            connectionObservability <-
              waitForConnectionObservability connectionObservabilityReference "insecure-connection-denied"
            let connectionSpan = Observability.observabilityConnectionSpan connectionObservability
            Observability.requestSpanDisplayName connectionSpan `shouldBe` "CONNECTION insecure-connection-denied"
            Observability.requestSpanAttributes connectionSpan
              `shouldSatisfy` hasTextAttribute "client.address" "127.0.0.1"
            Observability.requestSpanAttributes connectionSpan
              `shouldSatisfy` hasTextAttribute "network.peer.address" "127.0.0.1"
            Observability.requestSpanAttributes connectionSpan
              `shouldSatisfy` hasTextAttribute "url.scheme" "https"
            Observability.requestSpanAttributes connectionSpan
              `shouldSatisfy` hasTextAttribute "harch.connection.event" "insecure-connection-denied"
            Observability.requestSpanAttributes connectionSpan
              `shouldSatisfy` hasTextAttribute "exception.type" "InsecureConnectionDenied"
            killThread serverThreadId
            waitForServerExit completionReference

    it "reports prematurely closed HTTPS listener connections as connection observability with peer addresses" $
      withUnusedLoopbackPort $ \unusedPort ->
        withManualTlsFiles $ \certificatePath privateKeyPath ->
          withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
            completionReference <- newIORef Nothing
            connectionObservabilityReference <- newIORef []
            let observingApplication =
                  sampleApplication
                    { reportConnectionObservability = \connectionObservabilityValue ->
                        modifyIORef' connectionObservabilityReference (connectionObservabilityValue :)
                    }
                manualTlsConfig =
                  serverConfigWithListeners
                    [ ListenerConfig
                        { listenerHost = "127.0.0.1",
                          listenerPort = unusedPort,
                          listenerScheme = Https,
                          listenerTls =
                            Just
                              TlsConfig
                                { certificateSource =
                                    ManualCertificateFiles
                                      ManualTlsCertificateFiles
                                        { certificateFile = certificatePath,
                                          privateKeyFile = privateKeyPath
                                        }
                                },
                          listenerAcme = Nothing
                        }
                    ]
            serverThreadId <- forkIO $ do
              result <- try (runServer outputHandle manualTlsConfig observingApplication) :: IO (Either SomeException ())
              writeIORef completionReference (Just result)
            _ <- waitForHttpsServerResponse completionReference unusedPort "/known"
            connectAndCloseLoopbackSocket unusedPort
            connectionObservability <-
              waitForConnectionObservability connectionObservabilityReference "client-closed-connection-prematurely"
            let connectionSpan = Observability.observabilityConnectionSpan connectionObservability
            Observability.requestSpanDisplayName connectionSpan `shouldBe` "CONNECTION client-closed-connection-prematurely"
            Observability.requestSpanAttributes connectionSpan
              `shouldSatisfy` hasTextAttribute "client.address" "127.0.0.1"
            Observability.requestSpanAttributes connectionSpan
              `shouldSatisfy` hasTextAttribute "network.peer.address" "127.0.0.1"
            Observability.requestSpanAttributes connectionSpan
              `shouldSatisfy` hasTextAttribute "url.scheme" "https"
            Observability.requestSpanAttributes connectionSpan
              `shouldSatisfy` hasTextAttribute "harch.connection.event" "client-closed-connection-prematurely"
            Observability.requestSpanAttributes connectionSpan
              `shouldSatisfy` hasTextAttribute "exception.type" "ClientClosedConnectionPrematurely"
            killThread serverThreadId
            waitForServerExit completionReference

    it "fails explicitly when a manual TLS certificate file is missing" $
      withSystemTempFile "harch-web-output.txt" $ \_ outputHandle ->
        withManualTlsFiles $ \_ privateKeyPath -> do
          let manualTlsConfig =
                serverConfigWithListeners
                  [ ListenerConfig
                      { listenerHost = "127.0.0.1",
                        listenerPort = 5443,
                        listenerScheme = Https,
                        listenerTls =
                          Just
                            TlsConfig
                              { certificateSource =
                                  ManualCertificateFiles
                                    ManualTlsCertificateFiles
                                      { certificateFile = "missing-cert.pem",
                                        privateKeyFile = privateKeyPath
                                      }
                              },
                        listenerAcme = Nothing
                      }
                  ]
          runServer outputHandle manualTlsConfig sampleApplication
            `shouldThrow` (\exception -> show (exception :: IOError) == "user error (Manual TLS certificate file does not exist: missing-cert.pem)")

    it "fails explicitly when a manual TLS private key file is missing" $
      withSystemTempFile "harch-web-output.txt" $ \_ outputHandle ->
        withManualTlsFiles $ \certificatePath _ -> do
          let manualTlsConfig =
                serverConfigWithListeners
                  [ ListenerConfig
                      { listenerHost = "127.0.0.1",
                        listenerPort = 5443,
                        listenerScheme = Https,
                        listenerTls =
                          Just
                            TlsConfig
                              { certificateSource =
                                  ManualCertificateFiles
                                    ManualTlsCertificateFiles
                                      { certificateFile = certificatePath,
                                        privateKeyFile = "missing-key.pem"
                                      }
                              },
                        listenerAcme = Nothing
                      }
                  ]
          runServer outputHandle manualTlsConfig sampleApplication
            `shouldThrow` (\exception -> show (exception :: IOError) == "user error (Manual TLS private key file does not exist: missing-key.pem)")

    it "fails when manual TLS certificate contents cannot be loaded at runtime" $
      withSystemTempDirectory "harch-web-invalid-tls" $ \tempDirectory ->
        withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
          let certificatePath = tempDirectory </> "cert.pem"
              privateKeyPath = tempDirectory </> "key.pem"
              invalidTlsConfig =
                serverConfigWithListeners
                  [ ListenerConfig
                      { listenerHost = "127.0.0.1",
                        listenerPort = 5443,
                        listenerScheme = Https,
                        listenerTls =
                          Just
                            TlsConfig
                              { certificateSource =
                                  ManualCertificateFiles
                                    ManualTlsCertificateFiles
                                      { certificateFile = certificatePath,
                                        privateKeyFile = privateKeyPath
                                      }
                              },
                        listenerAcme = Nothing
                      }
                  ]
          writeFile certificatePath "not a certificate"
          writeFile privateKeyPath "not a private key"
          runServer outputHandle invalidTlsConfig sampleApplication
            `shouldThrow` anyException

    it "fails explicitly when ACME runtime listeners are missing any HTTP challenge listener" $
      withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
        let acmeTlsConfig =
              serverConfigWithListeners
                [acmeHttpsListener "127.0.0.1" 5443 (certbotHttp01Backend [])]
        runServer outputHandle acmeTlsConfig sampleApplication
          `shouldThrow` (\exception -> show (exception :: IOError) == "user error (Unsupported runtime listener startup plan: ACME listener on 127.0.0.1:5443 requires an HTTP listener on port 80 for http-01 challenges.)")

    it "fails explicitly when ACME runtime listeners do not have an HTTP port 80 challenge listener" $
      withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
        let acmeTlsConfig =
              serverConfigWithListeners
                [ httpRuntimeListener "0.0.0.0" 5001,
                  acmeHttpsListener "0.0.0.0" 5443 (certbotHttp01Backend [])
                ]
        runServer outputHandle acmeTlsConfig sampleApplication
          `shouldThrow` (\exception -> show (exception :: IOError) == "user error (Unsupported runtime listener startup plan: ACME listener on 0.0.0.0:5443 requires an HTTP listener on port 80 for http-01 challenges.)")

    it "fails explicitly when ACME runtime challenge listeners do not match the HTTPS host" $
      withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
        let acmeTlsConfig =
              serverConfigWithListeners
                [ httpRuntimeListener "127.0.0.1" 80,
                  acmeHttpsListener "0.0.0.0" 5443 (certbotHttp01Backend [])
                ]
        runServer outputHandle acmeTlsConfig sampleApplication
          `shouldThrow` (\exception -> show (exception :: IOError) == "user error (Unsupported runtime listener startup plan: ACME listener on 0.0.0.0:5443 requires an HTTP listener on port 80 for http-01 challenges.)")

    it "fails explicitly after resolving an exact-host ACME challenge listener without ACME domains" $
      withUnusedLoopbackPort $ \challengePort ->
        withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
          let acmeTlsConfig =
                serverConfigWithListeners
                  [ httpRuntimeListener "127.0.0.1" challengePort,
                    acmeHttpsListenerWithDomainsAndChallengePort challengePort "127.0.0.1" 5443 ["ops@example.com"] [] (certbotHttp01Backend [])
                  ]
          runServer outputHandle acmeTlsConfig sampleApplication
            `shouldThrow` (\exception -> show (exception :: IOError) == "user error (Unsupported runtime listener startup plan: ACME listener on 127.0.0.1:5443 requires ACME domains or certbot arguments to declare --cert-name or a domain via -d/--domain/--domains.)")

    it "starts certbot-backed ACME listeners on the configured http-01 port and stays running until signalled to stop" $
      withUnusedLoopbackPort $ \challengePort ->
        withUnusedLoopbackPort $ \httpsPort ->
          withManualTlsFiles $ \certificatePath privateKeyPath ->
            withFakeCertbotExecutable certificatePath privateKeyPath $
              \certbotPath ->
                withSystemTempFile "harch-web-output.txt" $ \outputPath outputHandle -> do
                  completionReference <- newIORef Nothing
                  logEntriesReference <- newIORef []
                  let acmeConfig =
                        AcmeConfig
                          { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                            acmeContactEmails = ["ops@example.com"],
                            acmeDomains = ["loopback.example"],
                            acmeHttp01Port = challengePort,
                            acmeCertificateDirectory = Nothing,
                            acmeCertbotConfig = certbotHttp01BackendWithExecutable certbotPath []
                          }
                      acmeTlsConfig =
                        serverConfigWithListeners
                          [ httpRuntimeListener "0.0.0.0" challengePort,
                            ListenerConfig
                              { listenerHost = "127.0.0.1",
                                listenerPort = httpsPort,
                                listenerScheme = Https,
                                listenerTls =
                                  Just
                                    TlsConfig
                                      { certificateSource = AcmeCertificateSource acmeConfig
                                      },
                                listenerAcme = Nothing
                              }
                          ]
                      observingApplication =
                        sampleApplication
                          { reportApplicationLog = \logEntry ->
                              modifyIORef' logEntriesReference (<> [logEntry])
                          }
                  serverThreadId <- forkIO $ do
                    result <- try (runServer outputHandle acmeTlsConfig observingApplication) :: IO (Either SomeException ())
                    writeIORef completionReference (Just result)
                  firstResponseText <- waitForHttpsServerResponse completionReference httpsPort "/known"
                  Text.isInfixOf "<h1>Known</h1>" firstResponseText `shouldBe` True
                  threadDelay 50000
                  secondResponseText <- readLoopbackHttpsResponse httpsPort "/known"
                  Text.isInfixOf "<h1>Known</h1>" secondResponseText `shouldBe` True
                  completionResult <- readIORef completionReference
                  completionResult `shouldSatisfy` isNothing
                  killThread serverThreadId
                  waitForServerExit completionReference
                  hClose outputHandle
                  readFile outputPath
                    `shouldReturn` unlines
                      [ "HTTP Server listening at http://0.0.0.0:" <> show challengePort,
                        "HTTPS Server listening at https://127.0.0.1:" <> show httpsPort
                      ]
                  readIORef logEntriesReference
                    `shouldReturn` [ "ACME certbot webroot registered for listener 127.0.0.1:" <> Text.pack (show httpsPort),
                                     "Launching certbot for ACME listener on 127.0.0.1:" <> Text.pack (show httpsPort),
                                     "ACME certbot webroot unregistered for listener 127.0.0.1:" <> Text.pack (show httpsPort)
                                   ]

    it "lets shared HTTPS listeners reuse certificates issued by the certbot-backed ACME backend" $
      withUnusedLoopbackPort $ \challengePort ->
        withUnusedLoopbackPort $ \sharedHttpsPort ->
          withManualTlsFiles $ \certificatePath privateKeyPath ->
            withFakeCertbotExecutable certificatePath privateKeyPath $
              \certbotPath ->
                withSystemTempDirectory "harch-web-shared-certs" $ \sharedDirectory ->
                  withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
                    completionReference <- newIORef Nothing
                    logEntriesReference <- newIORef []
                    let acmeConfig =
                          AcmeConfig
                            { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                              acmeContactEmails = ["ops@example.com"],
                              acmeDomains = ["loopback.example"],
                              acmeHttp01Port = challengePort,
                              acmeCertificateDirectory = Just sharedDirectory,
                              acmeCertbotConfig = certbotHttp01BackendWithExecutable certbotPath []
                            }
                        runtimeConfig =
                          serverConfigWithListeners
                            [ ListenerConfig
                                { listenerHost = "127.0.0.1",
                                  listenerPort = challengePort,
                                  listenerScheme = Http,
                                  listenerTls = Nothing,
                                  listenerAcme = Just acmeConfig
                                },
                              sharedHttpsListener "127.0.0.1" sharedHttpsPort sharedDirectory
                            ]
                        observingApplication =
                          sampleApplication
                            { reportApplicationLog = \logEntry ->
                                modifyIORef' logEntriesReference (<> [logEntry])
                            }
                    serverThreadId <- forkIO $ do
                      result <- try (runServer outputHandle runtimeConfig observingApplication) :: IO (Either SomeException ())
                      writeIORef completionReference (Just result)
                    sharedResponseText <- waitForHttpsServerResponse completionReference sharedHttpsPort "/known"
                    Text.isInfixOf "<h1>Known</h1>" sharedResponseText `shouldBe` True
                    readFile (sharedDirectory </> "fullchain.pem") `shouldReturn` manualTlsCertificatePem
                    readFile (sharedDirectory </> "privkey.pem") `shouldReturn` manualTlsPrivateKeyPem
                    killThread serverThreadId
                    waitForServerExit completionReference
                    readIORef logEntriesReference
                      `shouldReturn` [ "ACME certbot webroot registered for listener 127.0.0.1:" <> Text.pack (show challengePort),
                                       "Launching certbot for ACME listener on 127.0.0.1:" <> Text.pack (show challengePort),
                                       "ACME certbot webroot unregistered for listener 127.0.0.1:" <> Text.pack (show challengePort),
                                       "Published ACME certificate files to shared directory " <> Text.pack sharedDirectory
                                     ]

    it "shuts down HTTP ACME producers that only publish certificates without starting HTTPS" $
      withUnusedLoopbackPort $ \challengePort ->
        withManualTlsFiles $ \certificatePath privateKeyPath ->
          withFakeCertbotExecutable certificatePath privateKeyPath $
            \certbotPath ->
              withSystemTempDirectory "harch-web-published-certs" $ \sharedDirectory ->
                withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
                  completionReference <- newIORef Nothing
                  let acmeConfig =
                        AcmeConfig
                          { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                            acmeContactEmails = ["ops@example.com"],
                            acmeDomains = ["loopback.example"],
                            acmeHttp01Port = challengePort,
                            acmeCertificateDirectory = Just sharedDirectory,
                            acmeCertbotConfig = certbotHttp01BackendWithExecutable certbotPath []
                          }
                      runtimeConfig =
                        serverConfigWithListeners
                          [ ListenerConfig
                              { listenerHost = "127.0.0.1",
                                listenerPort = challengePort,
                                listenerScheme = Http,
                                listenerTls = Nothing,
                                listenerAcme = Just acmeConfig
                              }
                          ]
                  serverThreadId <- forkIO $ do
                    result <- try (runServer outputHandle runtimeConfig sampleApplication) :: IO (Either SomeException ())
                    writeIORef completionReference (Just result)
                  responseText <- waitForServerResponse completionReference challengePort "/known"
                  Text.isInfixOf "<h1>Known</h1>" responseText `shouldBe` True
                  killThread serverThreadId
                  waitForServerExit completionReference

    it "fails explicitly when ACME listeners cannot launch certbot" $
      withUnusedLoopbackPort $ \challengePort ->
        withSystemTempFile "harch-web-output.txt" $ \_ outputHandle ->
          withEmptyExecutablePath $ do
            logEntriesReference <- newIORef []
            let acmeConfig =
                  AcmeConfig
                    { acmeDirectoryUrl = "http://127.0.0.1:14000/directory",
                      acmeContactEmails = ["ops@example.com"],
                      acmeDomains = ["loopback.example"],
                      acmeHttp01Port = challengePort,
                      acmeCertificateDirectory = Nothing,
                      acmeCertbotConfig = certbotHttp01Backend []
                    }
                acmeTlsConfig =
                  serverConfigWithListeners
                    [ httpRuntimeListener "127.0.0.1" challengePort,
                      ListenerConfig
                        { listenerHost = "127.0.0.1",
                          listenerPort = 5443,
                          listenerScheme = Https,
                          listenerTls =
                            Just
                              TlsConfig
                                { certificateSource = AcmeCertificateSource acmeConfig
                                },
                          listenerAcme = Nothing
                        }
                    ]
                observingApplication =
                  sampleApplication
                    { reportApplicationLog = \logEntry ->
                        modifyIORef' logEntriesReference (<> [logEntry])
                    }
            runServer outputHandle acmeTlsConfig observingApplication
              `shouldThrow` (\exception -> "user error (Failed to launch certbot for ACME listener on 127.0.0.1:5443:" `isPrefixOf` show (exception :: IOError))
            logEntries <- readIORef logEntriesReference
            case logEntries of
              [registeredLog, launchLog, failureLog, unregisterLog] ->
                expectAll
                  ( (registeredLog `shouldBe` "ACME certbot webroot registered for listener 127.0.0.1:5443")
                      :| [ launchLog `shouldBe` "Launching certbot for ACME listener on 127.0.0.1:5443",
                           failureLog `shouldSatisfy` Text.isPrefixOf "Failed to launch certbot for ACME listener on 127.0.0.1:5443: ",
                           unregisterLog `shouldBe` "ACME certbot webroot unregistered for listener 127.0.0.1:5443"
                         ]
                  )
              _ ->
                expectationFailure ("Expected four ACME certbot lifecycle logs, got: " <> show logEntries)

    it "fails explicitly when certbot-backed ACME listeners do not have the declared http-01 port listener" $
      withUnusedLoopbackPort $ \challengePort ->
        withUnusedLoopbackPort $ \otherPort ->
          withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
            let declaredPort = challengePort
                certbotBackend =
                  certbotHttp01Backend
                    ["certonly", "--http-01-port", Text.pack (show declaredPort), "--cert-name", "loopback.example"]
                acmeTlsConfig =
                  serverConfigWithListeners
                    [ httpRuntimeListener "127.0.0.1" otherPort,
                      acmeHttpsListener "127.0.0.1" 5443 certbotBackend
                    ]
            runServer outputHandle acmeTlsConfig sampleApplication
              `shouldThrow` (\exception -> show (exception :: IOError) == "user error (Unsupported runtime listener startup plan: ACME listener on 127.0.0.1:5443 requires an HTTP listener on port " <> show declaredPort <> " for http-01 challenges.)")

    it "fails explicitly when HTTP ACME producers declare a mismatched http-01 port" $
      withUnusedLoopbackPort $ \httpPort ->
        withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
          let acmeConfig =
                AcmeConfig
                  { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                    acmeContactEmails = ["ops@example.com"],
                    acmeDomains = ["example.com"],
                    acmeHttp01Port = httpPort + 1,
                    acmeCertificateDirectory = Just ".tls/example.com",
                    acmeCertbotConfig = certbotHttp01Backend []
                  }
              runtimeConfig =
                serverConfigWithListeners
                  [ ListenerConfig
                      { listenerHost = "127.0.0.1",
                        listenerPort = httpPort,
                        listenerScheme = Http,
                        listenerTls = Nothing,
                        listenerAcme = Just acmeConfig
                      }
                  ]
          runServer outputHandle runtimeConfig sampleApplication
            `shouldThrow` (\exception -> show (exception :: IOError) == "user error (Unsupported runtime listener startup plan: ACME listener on 127.0.0.1:" <> show httpPort <> " requires the configured http-01 port to match its HTTP listener port " <> show httpPort <> ".)")

    it "fails explicitly when HTTP ACME producers do not publish a certificate directory" $
      withUnusedLoopbackPort $ \httpPort ->
        withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
          let acmeConfig =
                AcmeConfig
                  { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                    acmeContactEmails = ["ops@example.com"],
                    acmeDomains = ["example.com"],
                    acmeHttp01Port = httpPort,
                    acmeCertificateDirectory = Nothing,
                    acmeCertbotConfig = certbotHttp01Backend []
                  }
              runtimeConfig =
                serverConfigWithListeners
                  [ ListenerConfig
                      { listenerHost = "127.0.0.1",
                        listenerPort = httpPort,
                        listenerScheme = Http,
                        listenerTls = Nothing,
                        listenerAcme = Just acmeConfig
                      }
                  ]
          runServer outputHandle runtimeConfig sampleApplication
            `shouldThrow` (\exception -> show (exception :: IOError) == "user error (Unsupported runtime listener startup plan: ACME listener on 127.0.0.1:" <> show httpPort <> " requires an ACME certificate directory so HTTPS listeners can consume published certificates.)")

    it "derives certonly plus non-interactive certbot defaults when certbot args are omitted" $
      withManualTlsFiles $ \certificatePath privateKeyPath ->
        withCustomFakeCertbotExecutable
          [ "#!/bin/sh",
            "set -eu",
            "config_dir=''",
            "cert_name=''",
            "domain=''",
            "command=''",
            "has_non_interactive=0",
            "has_agree_tos=0",
            "has_webroot=0",
            "webroot_path=''",
            "http_port=''",
            "server_url=''",
            "email=''",
            "while [ \"$#\" -gt 0 ]; do",
            "  case \"$1\" in",
            "    certonly) command='certonly'; shift ;;",
            "    --non-interactive|-n) has_non_interactive=1; shift ;;",
            "    --agree-tos) has_agree_tos=1; shift ;;",
            "    --webroot) has_webroot=1; shift ;;",
            "    -w|--webroot-path) webroot_path=\"$2\"; shift 2 ;;",
            "    --config-dir) config_dir=\"$2\"; shift 2 ;;",
            "    --cert-name) cert_name=\"$2\"; shift 2 ;;",
            "    --cert-name=*) cert_name=\"${1#--cert-name=}\"; shift ;;",
            "    --http-01-port) http_port=\"$2\"; shift 2 ;;",
            "    --server) server_url=\"$2\"; shift 2 ;;",
            "    --email|-m) email=\"$2\"; shift 2 ;;",
            "    -d|--domain|--domains) domain=\"$2\"; shift 2 ;;",
            "    --domains=*) domain=\"${1#--domains=}\"; shift ;;",
            "    *) shift ;;",
            "  esac",
            "done",
            "test \"$command\" = certonly",
            "test \"$has_non_interactive\" = 1",
            "test \"$has_agree_tos\" = 1",
            "test \"$has_webroot\" = 1",
            "test -n \"$webroot_path\"",
            "test -z \"$http_port\"",
            "test \"$server_url\" = https://acme-v02.api.letsencrypt.org/directory",
            "test \"$email\" = ops@example.com",
            "test \"$domain\" = example.com,www.example.com",
            "if [ -z \"$cert_name\" ]; then",
            "  cert_name=\"${domain%%,*}\"",
            "fi",
            "mkdir -p \"$webroot_path/.well-known/acme-challenge\"",
            "mkdir -p \"$config_dir/live/$cert_name\"",
            "cp " <> show certificatePath <> " \"$config_dir/live/$cert_name/fullchain.pem\"",
            "cp " <> show privateKeyPath <> " \"$config_dir/live/$cert_name/privkey.pem\""
          ]
          $ \certbotExecutable -> do
            let certbotConfig = CertbotConfig {certbotExecutable = certbotExecutable, certbotArguments = []}
            webrootStore <- newCertbotWebrootStore
            (manualTlsBindPlan, stateDirectory) <-
              prepareCertbotManualTlsBindPlan
                webrootStore
                (runtimeAcmePlanWithCertbotConfig certbotConfig)
                certbotConfig
            removePathForcibly stateDirectory
            manualTlsBindPlan `shouldSatisfy` (/= Nothing)

    it "does not duplicate explicit certbot command and agreement flags when already configured" $
      withManualTlsFiles $ \certificatePath privateKeyPath ->
        withCustomFakeCertbotExecutable
          [ "#!/bin/sh",
            "set -eu",
            "config_dir=''",
            "cert_name=''",
            "domain=''",
            "certonly_count=0",
            "non_interactive_count=0",
            "agree_tos_count=0",
            "webroot_count=0",
            "webroot_path=''",
            "http_port=''",
            "server_url=''",
            "email=''",
            "while [ \"$#\" -gt 0 ]; do",
            "  case \"$1\" in",
            "    certonly) certonly_count=$((certonly_count + 1)); shift ;;",
            "    --non-interactive|-n) non_interactive_count=$((non_interactive_count + 1)); shift ;;",
            "    --agree-tos) agree_tos_count=$((agree_tos_count + 1)); shift ;;",
            "    --webroot) webroot_count=$((webroot_count + 1)); shift ;;",
            "    -w|--webroot-path) webroot_path=\"$2\"; shift 2 ;;",
            "    --config-dir) config_dir=\"$2\"; shift 2 ;;",
            "    --cert-name) cert_name=\"$2\"; shift 2 ;;",
            "    --cert-name=*) cert_name=\"${1#--cert-name=}\"; shift ;;",
            "    --http-01-port) http_port=\"$2\"; shift 2 ;;",
            "    --server) server_url=\"$2\"; shift 2 ;;",
            "    --email|-m) email=\"$2\"; shift 2 ;;",
            "    -d|--domain|--domains) domain=\"$2\"; shift 2 ;;",
            "    --domains=*) domain=\"${1#--domains=}\"; shift ;;",
            "    *) shift ;;",
            "  esac",
            "done",
            "test \"$certonly_count\" = 1",
            "test \"$non_interactive_count\" = 1",
            "test \"$agree_tos_count\" = 1",
            "test \"$webroot_count\" = 1",
            "test -n \"$webroot_path\"",
            "test \"$http_port\" = 8080",
            "test \"$server_url\" = https://acme-staging.example/directory",
            "test \"$email\" = already-set@example.com",
            "test \"$domain\" = configured.example",
            "if [ -z \"$cert_name\" ]; then",
            "  cert_name=\"${domain%%,*}\"",
            "fi",
            "mkdir -p \"$webroot_path/.well-known/acme-challenge\"",
            "mkdir -p \"$config_dir/live/$cert_name\"",
            "cp " <> show certificatePath <> " \"$config_dir/live/$cert_name/fullchain.pem\"",
            "cp " <> show privateKeyPath <> " \"$config_dir/live/$cert_name/privkey.pem\""
          ]
          $ \certbotExecutable -> do
            let certbotConfig =
                  CertbotConfig
                    { certbotExecutable = certbotExecutable,
                      certbotArguments =
                        [ "certonly",
                          "--non-interactive",
                          "--agree-tos",
                          "--webroot",
                          "--http-01-port",
                          "8080",
                          "--server",
                          "https://acme-staging.example/directory",
                          "--email",
                          "already-set@example.com",
                          "--domains",
                          "configured.example"
                        ]
                    }
            webrootStore <- newCertbotWebrootStore
            (manualTlsBindPlan, stateDirectory) <-
              prepareCertbotManualTlsBindPlan
                webrootStore
                (runtimeAcmePlanWithCertbotConfig certbotConfig)
                certbotConfig
            removePathForcibly stateDirectory
            manualTlsBindPlan `shouldSatisfy` (/= Nothing)

    it "derives the webroot authenticator when only a certbot webroot path is preconfigured" $
      withManualTlsFiles $ \certificatePath privateKeyPath ->
        withSystemTempDirectory "harch-web-configured-webroot" $ \configuredWebrootPath ->
          withCustomFakeCertbotExecutable
            [ "#!/bin/sh",
              "set -eu",
              "config_dir=''",
              "cert_name=''",
              "domain=''",
              "webroot_count=0",
              "webroot_path=''",
              "while [ \"$#\" -gt 0 ]; do",
              "  case \"$1\" in",
              "    --webroot) webroot_count=$((webroot_count + 1)); shift ;;",
              "    -w|--webroot-path) webroot_path=\"$2\"; shift 2 ;;",
              "    --config-dir) config_dir=\"$2\"; shift 2 ;;",
              "    --cert-name) cert_name=\"$2\"; shift 2 ;;",
              "    --cert-name=*) cert_name=\"${1#--cert-name=}\"; shift ;;",
              "    -d|--domain|--domains) domain=\"$2\"; shift 2 ;;",
              "    --domains=*) domain=\"${1#--domains=}\"; shift ;;",
              "    *) shift ;;",
              "  esac",
              "done",
              "test \"$webroot_count\" = 1",
              "test \"$webroot_path\" = " <> show configuredWebrootPath,
              "if [ -z \"$cert_name\" ]; then",
              "  cert_name=\"${domain%%,*}\"",
              "fi",
              "mkdir -p \"$config_dir/live/$cert_name\"",
              "cp " <> show certificatePath <> " \"$config_dir/live/$cert_name/fullchain.pem\"",
              "cp " <> show privateKeyPath <> " \"$config_dir/live/$cert_name/privkey.pem\""
            ]
            $ \certbotExecutable -> do
              let certbotConfig =
                    CertbotConfig
                      { certbotExecutable = certbotExecutable,
                        certbotArguments =
                          [ "--webroot-path",
                            Text.pack configuredWebrootPath,
                            "--cert-name",
                            "configured-webroot-cert"
                          ]
                      }
              webrootStore <- newCertbotWebrootStore
              (_, stateDirectory) <-
                prepareCertbotManualTlsBindPlan
                  webrootStore
                  (runtimeAcmePlanWithCertbotConfig certbotConfig)
                  certbotConfig
              removePathForcibly stateDirectory

    it "keeps explicit non-webroot certbot authenticators from deriving webroot flags" $
      withManualTlsFiles $ \certificatePath privateKeyPath ->
        withCustomFakeCertbotExecutable
          [ "#!/bin/sh",
            "set -eu",
            "config_dir=''",
            "cert_name=''",
            "domain=''",
            "webroot_count=0",
            "webroot_path_count=0",
            "while [ \"$#\" -gt 0 ]; do",
            "  case \"$1\" in",
            "    --webroot) webroot_count=$((webroot_count + 1)); shift ;;",
            "    -w|--webroot-path) webroot_path_count=$((webroot_path_count + 1)); shift 2 ;;",
            "    --config-dir) config_dir=\"$2\"; shift 2 ;;",
            "    --cert-name) cert_name=\"$2\"; shift 2 ;;",
            "    --cert-name=*) cert_name=\"${1#--cert-name=}\"; shift ;;",
            "    -d|--domain|--domains) domain=\"$2\"; shift 2 ;;",
            "    --domains=*) domain=\"${1#--domains=}\"; shift ;;",
            "    *) shift ;;",
            "  esac",
            "done",
            "test \"$webroot_count\" = 0",
            "test \"$webroot_path_count\" = 0",
            "if [ -z \"$cert_name\" ]; then",
            "  cert_name=\"${domain%%,*}\"",
            "fi",
            "mkdir -p \"$config_dir/live/$cert_name\"",
            "cp " <> show certificatePath <> " \"$config_dir/live/$cert_name/fullchain.pem\"",
            "cp " <> show privateKeyPath <> " \"$config_dir/live/$cert_name/privkey.pem\""
          ]
          $ \certbotExecutable -> do
            let standaloneConfig =
                  CertbotConfig
                    { certbotExecutable = certbotExecutable,
                      certbotArguments =
                        [ "--authenticator",
                          "standalone",
                          "--cert-name",
                          "standalone-cert"
                        ]
                    }
                dnsConfig =
                  CertbotConfig
                    { certbotExecutable = certbotExecutable,
                      certbotArguments =
                        [ "--authenticator",
                          "dns-route53",
                          "--cert-name",
                          "dns-cert"
                        ]
                    }
            webrootStore <- newCertbotWebrootStore
            (_, standaloneStateDirectory) <-
              prepareCertbotManualTlsBindPlan
                webrootStore
                (runtimeAcmePlanWithCertbotConfig standaloneConfig)
                standaloneConfig
            removePathForcibly standaloneStateDirectory
            (_, dnsStateDirectory) <-
              prepareCertbotManualTlsBindPlan
                webrootStore
                (runtimeAcmePlanWithCertbotConfig dnsConfig)
                dnsConfig
            removePathForcibly dnsStateDirectory

    it "rejects empty and path-traversal certbot challenge tokens" $ do
      validAcmeHttp01ChallengeToken "" `shouldBe` Nothing
      validAcmeHttp01ChallengeToken "nested/token" `shouldBe` Nothing
      validAcmeHttp01ChallengeToken ".." `shouldBe` Nothing
      validAcmeHttp01ChallengeToken "token..suffix" `shouldBe` Nothing
      validAcmeHttp01ChallengeToken "loopback-token" `shouldBe` Just "loopback-token"

    it "checks registered certbot webroots for challenge files before they exist" $
      withManualTlsFiles $ \certificatePath privateKeyPath ->
        withSystemTempDirectory "harch-web-certbot-marker" $ \markerDirectory ->
          withCustomFakeCertbotExecutable
            [ "#!/bin/sh",
              "set -eu",
              "config_dir=''",
              "cert_name=''",
              "domain=''",
              "while [ \"$#\" -gt 0 ]; do",
              "  case \"$1\" in",
              "    --config-dir) config_dir=\"$2\"; shift 2 ;;",
              "    --cert-name) cert_name=\"$2\"; shift 2 ;;",
              "    --cert-name=*) cert_name=\"${1#--cert-name=}\"; shift ;;",
              "    -d|--domain|--domains) domain=\"$2\"; shift 2 ;;",
              "    --domains=*) domain=\"${1#--domains=}\"; shift ;;",
              "    *) shift ;;",
              "  esac",
              "done",
              "printf '%s' 'started' > " <> show (markerDirectory </> "started"),
              "sleep 1",
              "if [ -z \"$cert_name\" ]; then",
              "  cert_name=\"${domain%%,*}\"",
              "fi",
              "mkdir -p \"$config_dir/live/$cert_name\"",
              "cp " <> show certificatePath <> " \"$config_dir/live/$cert_name/fullchain.pem\"",
              "cp " <> show privateKeyPath <> " \"$config_dir/live/$cert_name/privkey.pem\""
            ]
            $ \certbotExecutable -> do
              prepareResultReference <- newEmptyMVar
              webrootStore <- newCertbotWebrootStore
              let certbotConfig = CertbotConfig {certbotExecutable = certbotExecutable, certbotArguments = []}
                  markerPath = markerDirectory </> "started"
                  challengeStore = AcmeChallengeStore <$> newMVar []
                  challengeRequest =
                    Wai.defaultRequest
                      { Wai.rawPathInfo = "/.well-known/acme-challenge/loopback-token",
                        Wai.requestHeaders = [("Host", "loopback.example")]
                      }
                  waitForMarker remainingAttempts = do
                    markerExists <- doesFileExist markerPath
                    if markerExists
                      then pure ()
                      else
                        if remainingAttempts > 0
                          then threadDelay 10000 >> waitForMarker (remainingAttempts - 1)
                          else expectationFailure "expected fake certbot to start before checking the registered webroot"
              _ <- forkIO $ do
                result <-
                  try
                    ( prepareCertbotManualTlsBindPlan
                        webrootStore
                        (runtimeAcmePlanWithCertbotConfig certbotConfig)
                        certbotConfig
                    ) ::
                    IO (Either SomeException (Maybe ManualTlsBindPlan, FilePath))
                putMVar prepareResultReference result
              waitForMarker (500 :: Int)
              challengeStoreValue <- challengeStore
              challengeResponse <- acmeChallengeResponseForRequest defaultRequestPolicy challengeStoreValue webrootStore challengeRequest
              isNothing challengeResponse `shouldBe` True
              prepareResult <- readMVar prepareResultReference
              case prepareResult of
                Right (_, cleanupDirectory) -> removePathForcibly cleanupDirectory
                Left exception -> expectationFailure ("expected fake certbot prepare to succeed: " <> displayException exception)

    it "serves certbot webroot challenge files from the running HTTP listener while certificate acquisition is in progress" $
      withUnusedLoopbackPort $ \challengePort ->
        withUnusedLoopbackPort $ \httpsPort ->
          withManualTlsFiles $ \certificatePath privateKeyPath ->
            withCustomFakeCertbotExecutable
              [ "#!/bin/sh",
                "set -eu",
                "config_dir=''",
                "cert_name=''",
                "domain=''",
                "webroot_path=''",
                "while [ \"$#\" -gt 0 ]; do",
                "  case \"$1\" in",
                "    --config-dir) config_dir=\"$2\"; shift 2 ;;",
                "    --cert-name) cert_name=\"$2\"; shift 2 ;;",
                "    --cert-name=*) cert_name=\"${1#--cert-name=}\"; shift ;;",
                "    -d|--domain|--domains) domain=\"$2\"; shift 2 ;;",
                "    --domains=*) domain=\"${1#--domains=}\"; shift ;;",
                "    -w|--webroot-path) webroot_path=\"$2\"; shift 2 ;;",
                "    *) shift ;;",
                "  esac",
                "done",
                "if [ -z \"$cert_name\" ]; then",
                "  cert_name=\"${domain%%,*}\"",
                "fi",
                "mkdir -p \"$webroot_path/.well-known/acme-challenge\"",
                "printf '%s' 'loopback-token-response' > \"$webroot_path/.well-known/acme-challenge/loopback-token\"",
                "sleep 1",
                "mkdir -p \"$config_dir/live/$cert_name\"",
                "cp " <> show certificatePath <> " \"$config_dir/live/$cert_name/fullchain.pem\"",
                "cp " <> show privateKeyPath <> " \"$config_dir/live/$cert_name/privkey.pem\""
              ]
              $ \certbotExecutable ->
                withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
                  completionReference <- newIORef Nothing
                  requestObservabilityReference <- newIORef []
                  let certbotBackend =
                        certbotHttp01BackendWithExecutable
                          certbotExecutable
                          []
                      clientAddressAttribute =
                        Observability.ObservabilityAttribute
                          { Observability.attributeName = "client.address",
                            Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
                          }
                      peerAddressAttribute =
                        Observability.ObservabilityAttribute
                          { Observability.attributeName = "network.peer.address",
                            Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
                          }
                      forwardedPrefixAttribute =
                        Observability.ObservabilityAttribute
                          { Observability.attributeName = "http.request.header.x_forwarded_prefix",
                            Observability.attributeValue = Observability.TextAttribute "/app"
                          }
                      acmeTlsConfig =
                        serverConfigWithListeners
                          [ httpRuntimeListener "127.0.0.1" challengePort,
                            acmeHttpsListenerWithDomainsAndChallengePort challengePort "127.0.0.1" httpsPort ["ops@example.com"] ["loopback.example", "alt.example"] certbotBackend
                          ]
                      waitForChallengeResponse remainingAttempts = do
                        completionResult <- readIORef completionReference
                        case completionResult of
                          Just (Left exception) ->
                            expectationFailure ("expected runServer to remain running, but it failed early: " <> displayException exception)
                              >> pure ByteString.empty
                          Just (Right ()) ->
                            expectationFailure "expected runServer to remain running, but it exited early"
                              >> pure ByteString.empty
                          Nothing -> do
                            responseResult <-
                              readLoopbackHttpResponseBytesWithHostResult
                                challengePort
                                "loopback.example"
                                "/.well-known/acme-challenge/loopback-token"
                            case responseResult of
                              Right responseBytes
                                | "loopback-token-response" `ByteString.isInfixOf` responseBytes ->
                                    pure responseBytes
                              Right _
                                | remainingAttempts > 0 -> do
                                    threadDelay 10000
                                    waitForChallengeResponse (remainingAttempts - 1)
                              Left _
                                | remainingAttempts > 0 -> do
                                    threadDelay 10000
                                    waitForChallengeResponse (remainingAttempts - 1)
                              _ ->
                                expectationFailure "expected runServer to serve certbot webroot challenge files on the HTTP listener"
                                  >> pure ByteString.empty
                      waitForPrefixedChallengeResponse remainingAttempts = do
                        completionResult <- readIORef completionReference
                        case completionResult of
                          Just (Left exception) ->
                            expectationFailure ("expected runServer to remain running, but it failed early: " <> displayException exception)
                              >> pure ByteString.empty
                          Just (Right ()) ->
                            expectationFailure "expected runServer to remain running, but it exited early"
                              >> pure ByteString.empty
                          Nothing -> do
                            responseResult <-
                              readLoopbackHttpResponseBytesWithHostAndHeadersResult
                                challengePort
                                "loopback.example"
                                "/app/.well-known/acme-challenge/loopback-token"
                                [("X-Forwarded-Prefix", "/app")]
                            case responseResult of
                              Right responseBytes
                                | "loopback-token-response" `ByteString.isInfixOf` responseBytes ->
                                    pure responseBytes
                              Right _
                                | remainingAttempts > 0 -> do
                                    threadDelay 10000
                                    waitForPrefixedChallengeResponse (remainingAttempts - 1)
                              Left _
                                | remainingAttempts > 0 -> do
                                    threadDelay 10000
                                    waitForPrefixedChallengeResponse (remainingAttempts - 1)
                              _ ->
                                expectationFailure "expected runServer to serve prefixed certbot webroot challenge files on the HTTP listener"
                                  >> pure ByteString.empty
                      waitForRequestObservability expectedObservability failureMessage remainingAttempts = do
                        observedValues <- readIORef requestObservabilityReference
                        case find ((== expectedObservability) . stripVolatileRequestTiming) observedValues of
                          Just requestObservabilityValue ->
                            pure requestObservabilityValue
                          Nothing ->
                            if remainingAttempts > 0
                              then threadDelay 10000 >> waitForRequestObservability expectedObservability failureMessage (remainingAttempts - 1)
                              else expectationFailure failureMessage >> pure expectedObservability
                  serverThreadId <- forkIO $ do
                    result <-
                      try
                        ( runServer
                            outputHandle
                            acmeTlsConfig
                            sampleApplication
                              { applicationRequestPolicy = defaultRequestPolicy {forwardedHeaderTrust = testTrustedForwardedProxy},
                                requestContextFromRequest = sampleRequestContextFromRequest testTrustedForwardedProxy,
                                reportRequestObservability = \requestObservabilityValue ->
                                  modifyIORef' requestObservabilityReference (<> [requestObservabilityValue])
                              }
                        ) ::
                        IO (Either SomeException ())
                    writeIORef completionReference (Just result)
                  challengeResponseBytes <- waitForChallengeResponse (500 :: Int)
                  challengeResponseBytes `shouldSatisfy` ByteString.isInfixOf "loopback-token-response"
                  prefixedChallengeResponseBytes <- waitForPrefixedChallengeResponse (500 :: Int)
                  prefixedChallengeResponseBytes `shouldSatisfy` ByteString.isInfixOf "loopback-token-response"
                  let expectedChallengeRequestObservability =
                        Observability.buildRequestObservability
                          Observability.RequestIdentity
                            { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "GET",
                              Observability.requestIdentityScheme = "http",
                              Observability.requestIdentityPath = "/.well-known/acme-challenge/loopback-token",
                              Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/.well-known/acme-challenge/*"
                            }
                          200
                          Observability.BodyResponseKind
                          [clientAddressAttribute, peerAddressAttribute]
                      expectedPrefixedChallengeRequestObservability =
                        Observability.buildRequestObservability
                          Observability.RequestIdentity
                            { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "GET",
                              Observability.requestIdentityScheme = "http",
                              Observability.requestIdentityPath = "/.well-known/acme-challenge/loopback-token",
                              Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/app/.well-known/acme-challenge/*"
                            }
                          200
                          Observability.BodyResponseKind
                          [clientAddressAttribute, peerAddressAttribute, forwardedPrefixAttribute]
                  challengeRequestObservability <-
                    waitForRequestObservability
                      expectedChallengeRequestObservability
                      "expected certbot webroot challenge response to report request observability"
                      (500 :: Int)
                  stripVolatileRequestTiming challengeRequestObservability
                    `shouldBe` expectedChallengeRequestObservability
                  expectMeasuredRootRequestTiming challengeRequestObservability
                  prefixedChallengeRequestObservability <-
                    waitForRequestObservability
                      expectedPrefixedChallengeRequestObservability
                      "expected prefixed certbot webroot challenge response to report request observability"
                      (500 :: Int)
                  stripVolatileRequestTiming prefixedChallengeRequestObservability
                    `shouldBe` expectedPrefixedChallengeRequestObservability
                  expectMeasuredRootRequestTiming prefixedChallengeRequestObservability
                  firstResponseText <- waitForHttpsServerResponse completionReference httpsPort "/known"
                  Text.isInfixOf "<h1>Known</h1>" firstResponseText `shouldBe` True
                  killThread serverThreadId
                  waitForServerExit completionReference

    it "starts certbot-backed ACME listeners on the declared http-01 port and stays running until signalled to stop" $
      withUnusedLoopbackPort $ \challengePort ->
        withUnusedLoopbackPort $ \httpsPort ->
          withManualTlsFiles $ \certificatePath privateKeyPath ->
            withFakeCertbotExecutable certificatePath privateKeyPath $
              \certbotExecutable ->
                withSystemTempFile "harch-web-output.txt" $ \outputPath outputHandle -> do
                  completionReference <- newIORef Nothing
                  let certbotBackend =
                        certbotHttp01BackendWithExecutable
                          certbotExecutable
                          ["certonly", "--http-01-port", Text.pack (show challengePort)]
                      acmeTlsConfig =
                        serverConfigWithListeners
                          [ httpRuntimeListener "127.0.0.1" challengePort,
                            acmeHttpsListenerWithDomains "127.0.0.1" httpsPort ["ops@example.com"] ["loopback.example", "alt.example"] certbotBackend
                          ]
                  serverThreadId <- forkIO $ do
                    result <- try (runServer outputHandle acmeTlsConfig sampleApplication) :: IO (Either SomeException ())
                    writeIORef completionReference (Just result)
                  firstResponseText <- waitForHttpsServerResponse completionReference httpsPort "/known"
                  Text.isInfixOf "<h1>Known</h1>" firstResponseText `shouldBe` True
                  threadDelay 50000
                  secondResponseText <- readLoopbackHttpsResponse httpsPort "/known"
                  Text.isInfixOf "<h1>Known</h1>" secondResponseText `shouldBe` True
                  completionResult <- readIORef completionReference
                  completionResult `shouldSatisfy` isNothing
                  killThread serverThreadId
                  waitForServerExit completionReference
                  hClose outputHandle
                  readFile outputPath
                    `shouldReturn` unlines
                      [ "HTTP Server listening at http://127.0.0.1:" <> show challengePort,
                        "HTTPS Server listening at https://127.0.0.1:" <> show httpsPort
                      ]

    it "lets other HTTPS listeners reuse ACME certificates from a shared directory" $
      withUnusedLoopbackPort $ \challengePort ->
        withUnusedLoopbackPort $ \acmeHttpsPort ->
          withUnusedLoopbackPort $ \sharedHttpsPort ->
            withManualTlsFiles $ \certificatePath privateKeyPath ->
              withFakeCertbotExecutable certificatePath privateKeyPath $
                \certbotExecutable ->
                  withSystemTempDirectory "harch-web-shared-certs" $ \sharedDirectory ->
                    withSystemTempFile
                      "harch-web-output.txt"
                      ( \_ outputHandle -> do
                          completionReference <- newIORef Nothing
                          let certbotBackend =
                                certbotHttp01BackendWithExecutable
                                  certbotExecutable
                                  ["certonly", "--http-01-port", Text.pack (show challengePort)]
                              acmeConfig =
                                AcmeConfig
                                  { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                                    acmeContactEmails = ["ops@example.com"],
                                    acmeDomains = ["loopback.example", "alt.example"],
                                    acmeHttp01Port = challengePort,
                                    acmeCertificateDirectory = Just sharedDirectory,
                                    acmeCertbotConfig = certbotBackend
                                  }
                              acmeListener =
                                ListenerConfig
                                  { listenerHost = "127.0.0.1",
                                    listenerPort = acmeHttpsPort,
                                    listenerScheme = Https,
                                    listenerTls =
                                      Just
                                        TlsConfig
                                          { certificateSource = AcmeCertificateSource acmeConfig
                                          },
                                    listenerAcme = Nothing
                                  }
                              runtimeConfig =
                                serverConfigWithListeners
                                  [ httpRuntimeListener "127.0.0.1" challengePort,
                                    acmeListener,
                                    sharedHttpsListener "127.0.0.1" sharedHttpsPort sharedDirectory
                                  ]
                          serverThreadId <- forkIO $ do
                            result <- try (runServer outputHandle runtimeConfig sampleApplication) :: IO (Either SomeException ())
                            writeIORef completionReference (Just result)
                          acmeResponseText <- waitForHttpsServerResponse completionReference acmeHttpsPort "/known"
                          Text.isInfixOf "<h1>Known</h1>" acmeResponseText `shouldBe` True
                          sharedResponseText <- waitForHttpsServerResponse completionReference sharedHttpsPort "/known"
                          Text.isInfixOf "<h1>Known</h1>" sharedResponseText `shouldBe` True
                          readFile (sharedDirectory </> "fullchain.pem") `shouldReturn` manualTlsCertificatePem
                          readFile (sharedDirectory </> "privkey.pem") `shouldReturn` manualTlsPrivateKeyPem
                          killThread serverThreadId
                          waitForServerExit completionReference
                      )

    it "waits for shared TLS certificate files to appear before starting the HTTPS listener" $
      withUnusedLoopbackPort $ \httpsPort ->
        withSystemTempDirectory "harch-web-shared-certs" $ \sharedDirectory ->
          withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
            completionReference <- newIORef Nothing
            let runtimeConfig =
                  serverConfigWithListeners
                    [sharedHttpsListener "127.0.0.1" httpsPort sharedDirectory]
            serverThreadId <- forkIO $ do
              result <- try (runServer outputHandle runtimeConfig sampleApplication) :: IO (Either SomeException ())
              writeIORef completionReference (Just result)
            threadDelay 100000
            readIORef completionReference >>= (`shouldSatisfy` isNothing)
            writeFile (sharedDirectory </> "fullchain.pem") manualTlsCertificatePem
            writeFile (sharedDirectory </> "privkey.pem") manualTlsPrivateKeyPem
            responseText <- waitForHttpsServerResponse completionReference httpsPort "/known"
            Text.isInfixOf "<h1>Known</h1>" responseText `shouldBe` True
            killThread serverThreadId
            waitForServerExit completionReference

    it "fails explicitly when certbot-backed ACME listeners do not have the default http-01 port listener" $
      withUnusedLoopbackPort $ \otherPort ->
        withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
          let certbotBackend =
                certbotHttp01Backend
                  ["certonly", "--cert-name", "loopback.example"]
              acmeTlsConfig =
                serverConfigWithListeners
                  [ httpRuntimeListener "127.0.0.1" otherPort,
                    acmeHttpsListener "127.0.0.1" 5443 certbotBackend
                  ]
          runServer outputHandle acmeTlsConfig sampleApplication
            `shouldThrow` (\exception -> show (exception :: IOError) == "user error (Unsupported runtime listener startup plan: ACME listener on 127.0.0.1:5443 requires an HTTP listener on port 80 for http-01 challenges.)")

    it "fails explicitly when certbot-backed ACME listeners do not declare a cert name or domain" $
      withUnusedLoopbackPort $ \challengePort ->
        withManualTlsFiles $ \certificatePath privateKeyPath ->
          withFakeCertbotExecutable certificatePath privateKeyPath $
            \certbotExecutable ->
              withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
                let certbotBackend =
                      certbotHttp01BackendWithExecutable
                        certbotExecutable
                        ["certonly", "--http-01-port", Text.pack (show challengePort)]
                    acmeTlsConfig =
                      serverConfigWithListeners
                        [ httpRuntimeListener "127.0.0.1" challengePort,
                          acmeHttpsListener "127.0.0.1" 5443 certbotBackend
                        ]
                runServer outputHandle acmeTlsConfig sampleApplication
                  `shouldThrow` (\exception -> show (exception :: IOError) == "user error (Unsupported runtime listener startup plan: ACME listener on 127.0.0.1:5443 requires ACME domains or certbot arguments to declare --cert-name or a domain via -d/--domain/--domains.)")

    it "fails explicitly when certbot-backed ACME listeners propagate certbot runtime failures" $
      withUnusedLoopbackPort $ \challengePort ->
        withFailingFakeCertbotExecutable $ \certbotExecutable ->
          withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
            logEntriesReference <- newIORef []
            let certbotBackend =
                  certbotHttp01BackendWithExecutable
                    certbotExecutable
                    [ "certonly",
                      "--http-01-port",
                      Text.pack (show challengePort),
                      "--cert-name",
                      "loopback.example",
                      "--email",
                      "already-set@example.com",
                      "--server",
                      "https://acme-staging.example/directory"
                    ]
                acmeTlsConfig =
                  serverConfigWithListeners
                    [ httpRuntimeListener "127.0.0.1" challengePort,
                      acmeHttpsListener "127.0.0.1" 5443 certbotBackend
                    ]
                observingApplication =
                  sampleApplication
                    { reportApplicationLog = \logEntry ->
                        modifyIORef' logEntriesReference (<> [logEntry])
                    }
            runServer outputHandle acmeTlsConfig observingApplication
              `shouldThrow` ( \exception ->
                                let rendered = show (exception :: IOError)
                                 in "user error (Certbot failed for ACME listener on 127.0.0.1:5443 with exit code ExitFailure 42.\nstdout:\n\nstderr:\nfake certbot failure\n" `isPrefixOf` rendered
                                      && "Certbot state directory was preserved for inspection: " `isInfixOf` rendered
                                      && "letsencrypt.log tail:\nfake letsencrypt detail\n" `isInfixOf` rendered
                            )
            readIORef logEntriesReference
              `shouldReturn` [ "ACME certbot webroot registered for listener 127.0.0.1:5443",
                               "Launching certbot for ACME listener on 127.0.0.1:5443",
                               "Certbot failed for ACME listener on 127.0.0.1:5443 with exit code ExitFailure 42",
                               "ACME certbot webroot unregistered for listener 127.0.0.1:5443"
                             ]

    it "keeps certbot failure diagnostics useful when certbot exits without a logfile" $
      withUnusedLoopbackPort $ \challengePort ->
        withCustomFakeCertbotExecutable ["#!/bin/sh", "echo fake certbot failure without log >&2", "exit 42"] $ \certbotExecutable ->
          withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
            let certbotBackend =
                  certbotHttp01BackendWithExecutable
                    certbotExecutable
                    [ "certonly",
                      "--http-01-port",
                      Text.pack (show challengePort),
                      "--cert-name",
                      "loopback.example"
                    ]
                acmeTlsConfig =
                  serverConfigWithListeners
                    [ httpRuntimeListener "127.0.0.1" challengePort,
                      acmeHttpsListener "127.0.0.1" 5443 certbotBackend
                    ]
            runServer outputHandle acmeTlsConfig sampleApplication
              `shouldThrow` ( \exception ->
                                let rendered = show (exception :: IOError)
                                 in "fake certbot failure without log" `isInfixOf` rendered
                                      && "No certbot logfile was found at " `isInfixOf` rendered
                                      && ".\n)" `isSuffixOf` rendered
                            )

    it "fails explicitly when certbot-backed ACME listeners cannot launch the certbot executable" $
      withUnusedLoopbackPort $ \challengePort ->
        withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
          let certbotBackend =
                certbotHttp01BackendWithExecutable
                  "/definitely/missing/certbot"
                  ["certonly", "--http-01-port", Text.pack (show challengePort), "--domains=loopback.example,alt.example"]
              acmeTlsConfig =
                serverConfigWithListeners
                  [ httpRuntimeListener "127.0.0.1" challengePort,
                    acmeHttpsListenerWithContacts "127.0.0.1" 5443 [] certbotBackend
                  ]
          runServer outputHandle acmeTlsConfig sampleApplication
            `shouldThrow` ( \exception ->
                              let rendered = show (exception :: IOError)
                               in "user error (Failed to launch certbot for ACME listener on 127.0.0.1:5443:" `isPrefixOf` rendered
                                    && "/definitely/missing/certbot" `isInfixOf` rendered
                          )

    it "fails explicitly when certbot-backed ACME listeners do not produce a certificate file" $
      withUnusedLoopbackPort $ \challengePort ->
        withCustomFakeCertbotExecutable
          ( fakeCertbotScriptPreamble
              <> ["mkdir -p \"$config_dir/live/$cert_name\""]
          )
          ( \certbotExecutable ->
              withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
                let certbotBackend =
                      certbotHttp01BackendWithExecutable
                        certbotExecutable
                        ["certonly", "--http-01-port", Text.pack (show challengePort), "--domains=loopback.example,alt.example"]
                    acmeTlsConfig =
                      serverConfigWithListeners
                        [ httpRuntimeListener "127.0.0.1" challengePort,
                          acmeHttpsListenerWithContacts "127.0.0.1" 5443 [] certbotBackend
                        ]
                runServer outputHandle acmeTlsConfig sampleApplication
                  `shouldThrow` (\exception -> "user error (Certbot ACME certificate file does not exist: " `isPrefixOf` show (exception :: IOError))
          )

    it "fails explicitly when certbot-backed ACME listeners do not produce a private key file" $
      withUnusedLoopbackPort $ \challengePort ->
        withManualTlsFiles $ \certificatePath _ ->
          withCustomFakeCertbotExecutable
            ( fakeCertbotScriptPreamble
                <> [ "mkdir -p \"$config_dir/live/$cert_name\"",
                     "cp " <> show certificatePath <> " \"$config_dir/live/$cert_name/fullchain.pem\""
                   ]
            )
            ( \certbotExecutable ->
                withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
                  let certbotBackend =
                        certbotHttp01BackendWithExecutable
                          certbotExecutable
                          ["certonly", "--http-01-port", Text.pack (show challengePort), "--cert-name=loopback.example"]
                      acmeTlsConfig =
                        serverConfigWithListeners
                          [ httpRuntimeListener "127.0.0.1" challengePort,
                            acmeHttpsListener "127.0.0.1" 5443 certbotBackend
                          ]
                  runServer outputHandle acmeTlsConfig sampleApplication
                    `shouldThrow` (\exception -> "user error (Certbot ACME private key file does not exist: " `isPrefixOf` show (exception :: IOError))
            )

    it "cleans up already-started ACME listeners when a later ACME bind fails" $
      withUnusedLoopbackPort $ \firstChallengePort ->
        withUnusedLoopbackPort $ \secondChallengePort ->
          withUnusedLoopbackPort $ \firstHttpsPort ->
            withUnusedLoopbackPort $ \blockedHttpsPort ->
              withManualTlsFiles $ \certificatePath privateKeyPath ->
                withFakeCertbotExecutable certificatePath privateKeyPath $
                  \certbotExecutable ->
                    withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
                      addressInfo : _ <-
                        Socket.getAddrInfo
                          (Just (Socket.defaultHints {Socket.addrFlags = [Socket.AI_PASSIVE]}))
                          (Just "127.0.0.1")
                          (Just (show blockedHttpsPort))
                      blockingSocket <- Socket.openSocket addressInfo
                      Socket.setSocketOption blockingSocket Socket.ReuseAddr 1
                      Socket.bind blockingSocket (Socket.addrAddress addressInfo)
                      Socket.listen blockingSocket Socket.maxListenQueue
                      let certbotBackend =
                            certbotHttp01BackendWithExecutable
                              certbotExecutable
                              ["certonly", "--http-01-port", Text.pack (show firstChallengePort), "--cert-name", "loopback.example"]
                          secondCertbotBackend =
                            certbotHttp01BackendWithExecutable
                              certbotExecutable
                              ["certonly", "--http-01-port", Text.pack (show secondChallengePort), "--domains=second.example"]
                          acmeTlsConfig =
                            serverConfigWithListeners
                              [ httpRuntimeListener "127.0.0.1" firstChallengePort,
                                httpRuntimeListener "127.0.0.1" secondChallengePort,
                                acmeHttpsListener "127.0.0.1" firstHttpsPort certbotBackend,
                                acmeHttpsListenerWithContacts "127.0.0.1" blockedHttpsPort [] secondCertbotBackend
                              ]
                      (runServer outputHandle acmeTlsConfig sampleApplication `shouldThrow` anyException)
                        `finally` Socket.close blockingSocket
                      threadDelay 50000
                      readLoopbackHttpsResponseResult firstHttpsPort "/known"
                        >>= (`shouldSatisfy` either (const True) (const False))

    it "fails explicitly when certbot-backed ACME listeners declare an invalid http-01 port" $
      withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
        let certbotBackend =
              certbotHttp01Backend
                ["certonly", "--http-01-port", "not-a-port"]
            acmeTlsConfig =
              serverConfigWithListeners
                [ httpRuntimeListener "127.0.0.1" 80,
                  acmeHttpsListener "127.0.0.1" 5443 certbotBackend
                ]
        runServer outputHandle acmeTlsConfig sampleApplication
          `shouldThrow` (\exception -> show (exception :: IOError) == "user error (Unsupported runtime listener startup plan: ACME listener on 127.0.0.1:5443 has an invalid certbot http-01 port: not-a-port)")

    it "fails explicitly when no supported runtime listeners are configured" $
      withSystemTempFile "harch-web-output.txt" $ \_ outputHandle ->
        runServer outputHandle (serverConfigWithListeners []) sampleApplication
          `shouldThrow` (\exception -> show (exception :: IOError) == "user error (Unsupported runtime listener startup plan: no runtime listeners are configured.)")

    it "fails gracefully when the configured HTTP port is already in use" $
      withOccupiedLoopbackPort $ \occupiedPort ->
        withSystemTempFile "harch-web-output.txt" $ \_ outputHandle ->
          let runtimeConfig =
                serverConfigWithListeners
                  [ ListenerConfig
                      { listenerHost = "127.0.0.1",
                        listenerPort = occupiedPort,
                        listenerScheme = Http,
                        listenerTls = Nothing,
                        listenerAcme = Nothing
                      }
                  ]
           in runServer outputHandle runtimeConfig sampleApplication
                `shouldThrow` isAlreadyInUseError

    it "cleans up already-started HTTP listeners when a later bind fails" $
      withUnusedLoopbackPort $ \firstPort ->
        withOccupiedLoopbackPort $ \occupiedPort ->
          withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
            let multiListenerConfig =
                  serverConfigWithListeners
                    [ ListenerConfig
                        { listenerHost = "127.0.0.1",
                          listenerPort = firstPort,
                          listenerScheme = Http,
                          listenerTls = Nothing,
                          listenerAcme = Nothing
                        },
                      ListenerConfig
                        { listenerHost = "127.0.0.1",
                          listenerPort = occupiedPort,
                          listenerScheme = Http,
                          listenerTls = Nothing,
                          listenerAcme = Nothing
                        }
                    ]
            runServer outputHandle multiListenerConfig sampleApplication
              `shouldThrow` isAlreadyInUseError
            expectLoopbackPortReusable firstPort

    it "cleans up already-started HTTP listeners when a later manual TLS bind fails" $
      withUnusedLoopbackPort $ \firstPort ->
        withOccupiedLoopbackPort $ \occupiedTlsPort ->
          withManualTlsFiles $ \certificatePath privateKeyPath ->
            withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
              let multiListenerConfig =
                    serverConfigWithListeners
                      [ ListenerConfig
                          { listenerHost = "127.0.0.1",
                            listenerPort = firstPort,
                            listenerScheme = Http,
                            listenerTls = Nothing,
                            listenerAcme = Nothing
                          },
                        ListenerConfig
                          { listenerHost = "127.0.0.1",
                            listenerPort = occupiedTlsPort,
                            listenerScheme = Https,
                            listenerTls =
                              Just
                                TlsConfig
                                  { certificateSource =
                                      ManualCertificateFiles
                                        ManualTlsCertificateFiles
                                          { certificateFile = certificatePath,
                                            privateKeyFile = privateKeyPath
                                          }
                                  },
                            listenerAcme = Nothing
                          }
                      ]
              runServer outputHandle multiListenerConfig sampleApplication
                `shouldThrow` isAlreadyInUseError
              expectLoopbackPortReusable firstPort

    it "cleans up already-started manual TLS listeners when a later manual TLS bind fails" $
      withUnusedLoopbackPort $ \firstTlsPort ->
        withOccupiedLoopbackPort $ \occupiedTlsPort ->
          withManualTlsFiles $ \certificatePath privateKeyPath ->
            withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
              let multiListenerConfig =
                    serverConfigWithListeners
                      [ ListenerConfig
                          { listenerHost = "127.0.0.1",
                            listenerPort = firstTlsPort,
                            listenerScheme = Https,
                            listenerTls =
                              Just
                                TlsConfig
                                  { certificateSource =
                                      ManualCertificateFiles
                                        ManualTlsCertificateFiles
                                          { certificateFile = certificatePath,
                                            privateKeyFile = privateKeyPath
                                          }
                                  },
                            listenerAcme = Nothing
                          },
                        ListenerConfig
                          { listenerHost = "127.0.0.1",
                            listenerPort = occupiedTlsPort,
                            listenerScheme = Https,
                            listenerTls =
                              Just
                                TlsConfig
                                  { certificateSource =
                                      ManualCertificateFiles
                                        ManualTlsCertificateFiles
                                          { certificateFile = certificatePath,
                                            privateKeyFile = privateKeyPath
                                          }
                                  },
                            listenerAcme = Nothing
                          }
                      ]
              runServer outputHandle multiListenerConfig sampleApplication
                `shouldThrow` isAlreadyInUseError
              expectLoopbackPortReusable firstTlsPort

  describe "runServerWithWaiMiddleware" $ do
    it "composes the given Wai.Middleware in front of the rendered application" $
      withUnusedLoopbackPort $ \unusedPort ->
        withSystemTempFile "harch-web-middleware-output.txt" $ \_ outputHandle -> do
          completionReference <- newIORef Nothing
          let runtimeConfig =
                serverConfigWithListeners
                  [ ListenerConfig
                      { listenerHost = "127.0.0.1",
                        listenerPort = unusedPort,
                        listenerScheme = Http,
                        listenerTls = Nothing,
                        listenerAcme = Nothing
                      }
                  ]
              markerMiddleware innerApplication request respond =
                if Wai.rawPathInfo request == "/middleware-marker"
                  then respond (Wai.responseLBS Http.status200 [] "handled by middleware")
                  else innerApplication request respond
          serverThreadId <- forkIO $ do
            result <- try (runServerWithWaiMiddleware markerMiddleware outputHandle runtimeConfig sampleApplication) :: IO (Either SomeException ())
            writeIORef completionReference (Just result)
          firstResponseText <- waitForServerResponse completionReference unusedPort "/known"
          markerResponseText <- readLoopbackHttpResponse unusedPort "/middleware-marker"
          expectAll
            ( (Text.isInfixOf "<h1>Known</h1>" firstResponseText `shouldBe` True)
                :| [Text.isInfixOf "handled by middleware" markerResponseText `shouldBe` True]
            )
          completionResult <- readIORef completionReference
          completionResult `shouldSatisfy` isNothing
          killThread serverThreadId
          waitForServerExit completionReference
