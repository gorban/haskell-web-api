{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteStringChar8
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Network.Socket (Family (AF_INET), SockAddr (SockAddrInet), SocketType (Stream), bind, close, defaultProtocol, getSocketName, socket, tupleToHostAddress)
import qualified Network.Socket as NetworkSocket
import qualified Network.Socket.ByteString as SocketByteString
import Numeric (readHex)
import System.Environment (getEnvironment, lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.IO (hClose)
import System.IO.Error (tryIOError)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import System.Process (ProcessHandle, StdStream (UseHandle), createProcess, cwd, env, getProcessExitCode, proc, readCreateProcessWithExitCode, readProcessWithExitCode, std_out, terminateProcess, waitForProcess)
import TestSupport.RealPostgres (databaseSetupEnvironment, defaultRealPostgresConfig, ensureDefaultPostgresAvailable, supportedPostgresMajorVersions, withContainerizedPsqlOnPath)
import WebApi.Config (DatabaseConfig (..))
import WebApi.Database (DatabaseEffect (..), DatabaseError (..), HomePageData (..), SecondPageData (..))
import WebApi.Postgres (buildPostgresDatabaseEffect, buildRuntimePostgresDatabaseEffect)
import WebApi.Route (AppLocale (Spanish), AppRequestContext (..), defaultRequestContext)

spec = do
  describe "main" $ do
    it "stays running while idle, serves real HTTP traffic, and only stops when terminated" $ do
      withUnusedLoopbackPort $ \unusedPort ->
        withSystemTempDirectory "haskell-web-api-run" $ \workingDirectory -> do
          writeFile (workingDirectory <> "/.env") ("LISTENER_0_PORT=" <> show unusedPort <> "\n")
          withSystemTempFile "haskell-web-api-stdout.txt" $ \outputPath outputHandle -> do
            (_, _, _, processHandle) <-
              createProcess
                ( (proc "haskell-web-api" [])
                    { cwd = Just workingDirectory,
                      std_out = UseHandle outputHandle
                    }
                )
            (responseText, runningExitCode) <-
              ( do
                  threadDelay 1500000
                  idleExitCode <- getProcessExitCode processHandle
                  readyResponse <- waitForProcessResponse processHandle unusedPort "/api/status"
                  stillRunningExitCode <- getProcessExitCode processHandle
                  idleExitCode `shouldBe` Nothing
                  pure (readyResponse, stillRunningExitCode)
              )
                `finally` do
                  terminateProcess processHandle
                  _ <- waitForProcess processHandle
                  hClose outputHandle
            responseText `shouldBe` "{\"status\":\"ok\",\"locale\":\"en\"}"
            runningExitCode `shouldBe` Nothing
            readFile outputPath
              `shouldReturn` unlines
                [ "Loaded config file: ./.env",
                  "Config file missing: ./.env.local",
                  "Parsed listener config: http://127.0.0.1:" <> show unusedPort,
                  "HTTP Server listening at http://127.0.0.1:" <> show unusedPort
                ]

    it "defaults plain HTTP traffic to HTTPS redirects when both HTTP and manual TLS listeners are configured" $
      withUnusedLoopbackPort $ \httpPort ->
        withUnusedLoopbackPort $ \httpsPort ->
          withManualTlsFiles $ \certificatePath privateKeyPath ->
            withSystemTempDirectory "haskell-web-api-https-redirect" $ \workingDirectory -> do
              writeFile
                (workingDirectory <> "/.env")
                ( unlines
                    [ "LISTENER_0_HOST=127.0.0.1",
                      "LISTENER_0_PORT=" <> show httpPort,
                      "LISTENER_0_SCHEME=http",
                      "LISTENER_1_HOST=127.0.0.1",
                      "LISTENER_1_PORT=" <> show httpsPort,
                      "LISTENER_1_SCHEME=https",
                      "LISTENER_1_TLS_SOURCE=manual",
                      "LISTENER_1_TLS_CERTIFICATE_FILE=" <> certificatePath,
                      "LISTENER_1_TLS_PRIVATE_KEY_FILE=" <> privateKeyPath
                    ]
                )
              withSystemTempFile "haskell-web-api-stdout.txt" $ \outputPath outputHandle -> do
                (_, _, _, processHandle) <-
                  createProcess
                    ( (proc "haskell-web-api" [])
                        { cwd = Just workingDirectory,
                          std_out = UseHandle outputHandle
                        }
                    )
                (redirectHeaders, httpsResponseText, runningExitCode) <-
                  ( do
                      readyRedirectHeaders <- waitForProcessHttpHeaders processHandle httpPort "/api/status"
                      readyHttpsResponse <- waitForProcessTrustedHttpsResponse processHandle certificatePath httpsPort "/api/status"
                      stillRunningExitCode <- getProcessExitCode processHandle
                      pure (readyRedirectHeaders, readyHttpsResponse, stillRunningExitCode)
                  )
                    `finally` do
                      terminateProcess processHandle
                      _ <- waitForProcess processHandle
                      hClose outputHandle
                redirectHeaders `shouldContain` "308 Permanent Redirect"
                redirectHeaders `shouldContain` ("Location: https://127.0.0.1:" <> show httpsPort <> "/api/status")
                httpsResponseText `shouldBe` "{\"status\":\"ok\",\"locale\":\"en\"}"
                runningExitCode `shouldBe` Nothing
                readFile outputPath
                  `shouldReturn` unlines
                    [ "Loaded config file: ./.env",
                      "Config file missing: ./.env.local",
                      "Parsed listener config: http://127.0.0.1:" <> show httpPort,
                      "Parsed listener config: https://127.0.0.1:" <> show httpsPort,
                      "HTTP Server listening at http://127.0.0.1:" <> show httpPort,
                      "HTTPS Server listening at https://127.0.0.1:" <> show httpsPort
                    ]

    it "lets REDIRECT_HTTP_TO_HTTPS=false keep both HTTP and HTTPS listeners serving traffic" $
      withUnusedLoopbackPort $ \httpPort ->
        withUnusedLoopbackPort $ \httpsPort ->
          withManualTlsFiles $ \certificatePath privateKeyPath ->
            withSystemTempDirectory "haskell-web-api-dual-listener" $ \workingDirectory -> do
              writeFile
                (workingDirectory <> "/.env")
                ( unlines
                    [ "LISTENER_0_HOST=127.0.0.1",
                      "LISTENER_0_PORT=" <> show httpPort,
                      "LISTENER_0_SCHEME=http",
                      "LISTENER_1_HOST=127.0.0.1",
                      "LISTENER_1_PORT=" <> show httpsPort,
                      "LISTENER_1_SCHEME=https",
                      "LISTENER_1_TLS_SOURCE=manual",
                      "LISTENER_1_TLS_CERTIFICATE_FILE=" <> certificatePath,
                      "LISTENER_1_TLS_PRIVATE_KEY_FILE=" <> privateKeyPath,
                      "REDIRECT_HTTP_TO_HTTPS=false"
                    ]
                )
              withSystemTempFile "haskell-web-api-stdout.txt" $ \outputPath outputHandle -> do
                (_, _, _, processHandle) <-
                  createProcess
                    ( (proc "haskell-web-api" [])
                        { cwd = Just workingDirectory,
                          std_out = UseHandle outputHandle
                        }
                    )
                (httpResponseText, httpsResponseText, runningExitCode) <-
                  ( do
                      readyHttpResponse <- waitForProcessResponse processHandle httpPort "/api/status"
                      readyHttpsResponse <- waitForProcessTrustedHttpsResponse processHandle certificatePath httpsPort "/api/status"
                      stillRunningExitCode <- getProcessExitCode processHandle
                      pure (readyHttpResponse, readyHttpsResponse, stillRunningExitCode)
                  )
                    `finally` do
                      terminateProcess processHandle
                      _ <- waitForProcess processHandle
                      hClose outputHandle
                httpResponseText `shouldBe` "{\"status\":\"ok\",\"locale\":\"en\"}"
                httpsResponseText `shouldBe` "{\"status\":\"ok\",\"locale\":\"en\"}"
                runningExitCode `shouldBe` Nothing
                readFile outputPath
                  `shouldReturn` unlines
                    [ "Loaded config file: ./.env",
                      "Config file missing: ./.env.local",
                      "Parsed listener config: http://127.0.0.1:" <> show httpPort,
                      "Parsed listener config: https://127.0.0.1:" <> show httpsPort,
                      "HTTP Server listening at http://127.0.0.1:" <> show httpPort,
                      "HTTPS Server listening at https://127.0.0.1:" <> show httpsPort
                    ]

  describe "database integration" $ do
    it
      "runs migrate-and-seed, verifies the supported PostgreSQL major version, loads seeded page data, and enforces runtime-role privileges against real PostgreSQL"
      ( withContainerizedPsqlOnPath $ do
          ensureDefaultPostgresAvailable
          inheritedEnvironment <- getEnvironment
          exitCode <-
            withSystemTempDirectory "haskell-web-api-db" $ \workingDirectory ->
              withSystemTempFile "haskell-web-api-db-stdout.txt" $ \outputPath outputHandle -> do
                (_, _, _, processHandle) <-
                  createProcess
                    ( (proc "haskell-web-api-db" ["migrate-and-seed"])
                        { cwd = Just workingDirectory,
                          env = Just (databaseSetupEnvironment inheritedEnvironment),
                          std_out = UseHandle outputHandle
                        }
                    )
                result <- waitForProcess processHandle
                hClose outputHandle
                readFile outputPath `shouldReturn` "Applied database migrations and seed data.\n"
                pure result
          exitCode `shouldBe` ExitSuccess

          supportedVersionResult <-
            readCreateProcessWithExitCode
              ( (proc "psql" ["--host", "127.0.0.1", "--port", "5432", "--dbname", "web_api_dev", "--username", "web_api_owner", "--no-password", "--set", "ON_ERROR_STOP=1", "--tuples-only", "--no-align", "--quiet", "--command", "SELECT current_setting('server_version_num')::integer / 10000;"])
                  { env = Just (("PGPASSWORD", "web_api_owner") : inheritedEnvironment)
                  }
              )
              ""
          supportedVersionResult
            `shouldSatisfy` (`elem` fmap (\majorVersion -> (ExitSuccess, show majorVersion <> "\n", "")) supportedPostgresMajorVersions)

          let postgresEffect = buildPostgresDatabaseEffect defaultRealPostgresConfig
              spanishRequestContext = defaultRequestContext {requestLocale = Spanish}
          loadHomePageData postgresEffect defaultRequestContext
            `shouldReturn` Right
              HomePageData
                { homePageDataSummary = "Server-rendered home page with stubbed content."
                }
          loadSecondPageData postgresEffect defaultRequestContext
            `shouldReturn` Right
              SecondPageData
                { secondPageDataSummary = "Second page content with stubbed data ready for future loaders.",
                  secondPageDataHighlights = []
                }
          loadHomePageData postgresEffect spanishRequestContext
            `shouldReturn` Right
              HomePageData
                { homePageDataSummary = "Inicio renderizado en el servidor con datos de desarrollo preconfigurados."
                }
          loadSecondPageData postgresEffect spanishRequestContext
            `shouldReturn` Right
              SecondPageData
                { secondPageDataSummary = "Second page content with stubbed data ready for future loaders.",
                  secondPageDataHighlights = []
                }

          withTemporaryEnvironment "PATH" (Just "") $ do
            let runtimePostgresEffect = buildRuntimePostgresDatabaseEffect defaultRealPostgresConfig
            loadHomePageData runtimePostgresEffect defaultRequestContext
              `shouldReturn` Right
                HomePageData
                  { homePageDataSummary = "Server-rendered home page with stubbed content."
                  }
            loadSecondPageData runtimePostgresEffect spanishRequestContext
              `shouldReturn` Right
                SecondPageData
                  { secondPageDataSummary = "Second page content with stubbed data ready for future loaders.",
                    secondPageDataHighlights = []
                  }

          allowedSelect <-
            readCreateProcessWithExitCode
              ( (proc "psql" ["--host", "127.0.0.1", "--port", "5432", "--dbname", "web_api_dev", "--username", "web_api_runtime", "--no-password", "--set", "ON_ERROR_STOP=1", "--tuples-only", "--no-align", "--quiet", "--command", "SELECT summary FROM web_api.page_content WHERE route_slug = 'home' AND locale = 'en';"])
                  { env = Just (("PGPASSWORD", "web_api") : inheritedEnvironment)
                  }
              )
              ""
          allowedSelect `shouldBe` (ExitSuccess, "Server-rendered home page with stubbed content.\n", "")

          forbiddenInsert <-
            readCreateProcessWithExitCode
              ( (proc "psql" ["--host", "127.0.0.1", "--port", "5432", "--dbname", "web_api_dev", "--username", "web_api_runtime", "--no-password", "--set", "ON_ERROR_STOP=1", "--command", "INSERT INTO web_api.page_content (route_slug, locale, summary) VALUES ('forbidden', 'en', 'nope');"])
                  { env = Just (("PGPASSWORD", "web_api") : inheritedEnvironment)
                  }
              )
              ""
          fst3 forbiddenInsert `shouldNotBe` ExitSuccess
          thd3 forbiddenInsert `shouldContain` "permission denied"

          forbiddenSchemaChange <-
            readCreateProcessWithExitCode
              ( (proc "psql" ["--host", "127.0.0.1", "--port", "5432", "--dbname", "web_api_dev", "--username", "web_api_runtime", "--no-password", "--set", "ON_ERROR_STOP=1", "--command", "DO $$ BEGIN EXECUTE format('CREATE TABLE web_api.forbidden_runtime_table_%s (id INTEGER);', pg_backend_pid()); END $$;"])
                  { env = Just (("PGPASSWORD", "web_api") : inheritedEnvironment)
                  }
              )
              ""
          fst3 forbiddenSchemaChange `shouldNotBe` ExitSuccess
          thd3 forbiddenSchemaChange `shouldContain` "permission denied"

          forbiddenRoleCreate <-
            readCreateProcessWithExitCode
              ( (proc "psql" ["--host", "127.0.0.1", "--port", "5432", "--dbname", "web_api_dev", "--username", "web_api_runtime", "--no-password", "--set", "ON_ERROR_STOP=1", "--command", "CREATE ROLE forbidden_runtime_role LOGIN PASSWORD 'forbidden_runtime_role';"])
                  { env = Just (("PGPASSWORD", "web_api") : inheritedEnvironment)
                  }
              )
              ""
          fst3 forbiddenRoleCreate `shouldNotBe` ExitSuccess
          thd3 forbiddenRoleCreate `shouldContain` "permission denied"
      )

    it "maps runtime PostgreSQL connection failures into database errors without shelling out to psql" $
      withUnusedLoopbackPort $ \unusedPort ->
        withTemporaryEnvironment "PATH" (Just "") $ do
          let runtimePostgresEffect =
                buildRuntimePostgresDatabaseEffect
                  defaultRealPostgresConfig
                    { databasePort = unusedPort
                    }
          loadHomePageData runtimePostgresEffect defaultRequestContext
            >>= \case
              Left (HomePageDataError errorMessage) -> do
                errorMessage `shouldSatisfy` (not . Text.null)
                errorMessage `shouldSatisfy` (not . Text.isInfixOf "posix_spawnp")
              Left otherError ->
                expectationFailure ("expected HomePageDataError, got " <> show otherError)
              Right homePageData ->
                expectationFailure ("expected runtime connection failure, got " <> show homePageData)
  where
    fst3 (firstValue, _, _) = firstValue
    thd3 (_, _, thirdValue) = thirdValue

withUnusedLoopbackPort :: (Int -> IO a) -> IO a
withUnusedLoopbackPort action = do
  reservedSocket <- socket AF_INET Stream defaultProtocol
  bind reservedSocket (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  socketAddress <- getSocketName reservedSocket
  case socketAddress of
    SockAddrInet port _ -> do
      close reservedSocket
      action (fromIntegral port)
    _ ->
      close reservedSocket
        >> error "expected IPv4 loopback reservation socket"

waitForProcessResponse :: ProcessHandle -> Int -> Text.Text -> IO Text.Text
waitForProcessResponse processHandle port path =
  waitForProcessReadiness
    processHandle
    "expected haskell-web-api to accept loopback HTTP requests"
    (readLoopbackHttpResponse port path)

waitForProcessTrustedHttpsResponse :: ProcessHandle -> FilePath -> Int -> Text.Text -> IO Text.Text
waitForProcessTrustedHttpsResponse processHandle certificatePath port path =
  waitForProcessReadiness
    processHandle
    "expected haskell-web-api to accept loopback HTTPS requests"
    (readTrustedLoopbackHttpsResponse certificatePath port path)

waitForProcessHttpHeaders :: ProcessHandle -> Int -> Text.Text -> IO String
waitForProcessHttpHeaders processHandle port path =
  waitForProcessReadiness
    processHandle
    "expected haskell-web-api to accept loopback HTTP requests"
    (readLoopbackHttpResponseHeaders port path)

waitForProcessReadiness :: ProcessHandle -> String -> IO response -> IO response
waitForProcessReadiness processHandle failureMessage readResponse =
  waitForResponseAttempts (500 :: Int)
  where
    waitForResponseAttempts remainingAttempts = do
      exitCode <- getProcessExitCode processHandle
      case exitCode of
        Just completedExitCode ->
          expectationFailure ("expected haskell-web-api to keep running, but it exited early with " <> show completedExitCode)
            >> readResponse
        Nothing -> do
          responseResult <- tryIOError readResponse
          case responseResult of
            Right responseValue -> pure responseValue
            Left _
              | remainingAttempts > 0 -> do
                  threadDelay 10000
                  waitForResponseAttempts (remainingAttempts - 1)
              | otherwise ->
                  expectationFailure failureMessage
                    >> readResponse

withTemporaryEnvironment :: String -> Maybe String -> IO a -> IO a
withTemporaryEnvironment key maybeValue action = do
  originalValue <- lookupEnv key
  let restoreEnvironment =
        case originalValue of
          Just value -> setEnv key value
          Nothing -> unsetEnv key
      setTemporaryEnvironment =
        case maybeValue of
          Just value -> setEnv key value
          Nothing -> unsetEnv key
  setTemporaryEnvironment
  action `finally` restoreEnvironment

readLoopbackHttpResponse :: Int -> Text.Text -> IO Text.Text
readLoopbackHttpResponse port path = do
  responseBytes <- readLoopbackHttpResponseBytes port path
  pure (TextEncoding.decodeUtf8 responseBytes)

readLoopbackHttpResponseHeaders :: Int -> Text.Text -> IO String
readLoopbackHttpResponseHeaders port path = do
  let url = "http://127.0.0.1:" <> show port <> Text.unpack path
  (exitCode, stdoutText, stderrText) <-
    readProcessWithExitCode
      "curl"
      ["--silent", "--show-error", "--noproxy", "*", "--dump-header", "-", "--output", "/dev/null", url]
      ""
  case exitCode of
    ExitSuccess -> pure stdoutText
    _ -> ioError (userError stderrText)

readTrustedLoopbackHttpsResponse :: FilePath -> Int -> Text.Text -> IO Text.Text
readTrustedLoopbackHttpsResponse certificatePath port path = do
  let url = "https://127.0.0.1:" <> show port <> Text.unpack path
  (exitCode, stdoutText, stderrText) <-
    readProcessWithExitCode
      "curl"
      ["--silent", "--show-error", "--fail", "--noproxy", "*", "--cacert", certificatePath, url]
      ""
  case exitCode of
    ExitSuccess -> pure (Text.pack stdoutText)
    _ -> ioError (userError stderrText)

readLoopbackHttpResponseBytes :: Int -> Text.Text -> IO ByteString.ByteString
readLoopbackHttpResponseBytes port path = do
  clientSocket <- socket AF_INET Stream defaultProtocol
  NetworkSocket.connect clientSocket (SockAddrInet (fromIntegral port) (tupleToHostAddress (127, 0, 0, 1)))
  SocketByteString.sendAll clientSocket (buildHttpRequest path)
  responseBytes <- readAllSocketChunks clientSocket
  close clientSocket
  pure (extractHttpBody responseBytes)

buildHttpRequest :: Text.Text -> ByteString.ByteString
buildHttpRequest path =
  ByteStringChar8.pack $
    "GET "
      <> Text.unpack path
      <> " HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n"

readAllSocketChunks :: NetworkSocket.Socket -> IO ByteString.ByteString
readAllSocketChunks clientSocket = do
  chunk <- SocketByteString.recv clientSocket 4096
  if ByteString.null chunk
    then pure ByteString.empty
    else fmap (chunk <>) (readAllSocketChunks clientSocket)

extractHttpBody :: ByteString.ByteString -> ByteString.ByteString
extractHttpBody responseBytes =
  let (headers, withSeparator) = ByteStringChar8.breakSubstring "\r\n\r\n" responseBytes
      responseBody = ByteString.drop 4 withSeparator
   in if ByteStringChar8.isInfixOf "Transfer-Encoding: chunked" headers
        then decodeChunkedBody responseBody
        else responseBody

decodeChunkedBody :: ByteString.ByteString -> ByteString.ByteString
decodeChunkedBody chunkedBytes =
  case ByteStringChar8.breakSubstring "\r\n" chunkedBytes of
    (chunkSizeHex, withSizeSeparator)
      | ByteString.null withSizeSeparator ->
          chunkedBytes
      | otherwise ->
          case readHex (ByteStringChar8.unpack chunkSizeHex) of
            [(chunkSize, "")]
              | chunkSize == (0 :: Int) ->
                  ByteString.empty
              | otherwise ->
                  let chunkPayload = ByteString.drop 2 withSizeSeparator
                      (chunk, withChunkSuffix) = ByteString.splitAt chunkSize chunkPayload
                   in chunk <> decodeChunkedBody (ByteString.drop 2 withChunkSuffix)
            _ ->
              chunkedBytes

withManualTlsFiles :: (FilePath -> FilePath -> IO a) -> IO a
withManualTlsFiles action =
  withSystemTempDirectory "web-api-integration-tls" $ \tempDirectory -> do
    let certificatePath = tempDirectory </> "cert.pem"
        privateKeyPath = tempDirectory </> "key.pem"
    writeFile certificatePath manualTlsCertificatePem
    writeFile privateKeyPath manualTlsPrivateKeyPem
    action certificatePath privateKeyPath

manualTlsCertificatePem :: String
manualTlsCertificatePem =
  unlines
    [ "-----BEGIN CERTIFICATE-----",
      "MIICMzCCAdmgAwIBAgIUAliSVDIFHNHzI1q+e3P+1Ah1kbkwCgYIKoZIzj0EAwIw",
      "QDEXMBUGA1UECgwOdHJ1c3RtZSB2MS4yLjExJTAjBgNVBAsMHFRlc3RpbmcgQ0Eg",
      "I2JZeVBlbjVhVnQ0MHlLaXAwIBcNMDAwMTAxMDAwMDAwWhgPMzAwMDAxMDEwMDAw",
      "MDBaMEIxFzAVBgNVBAoMDnRydXN0bWUgdjEuMi4xMScwJQYDVQQLDB5UZXN0aW5n",
      "IGNlcnQgI3JHR1p2N1VLMVQyd1hjeG8wWTATBgcqhkjOPQIBBggqhkjOPQMBBwNC",
      "AARK6NEQhfcGYBt2TRWkrktWpYdmCvYo76sciH70kYBcihzjqaKEw5dD/KbdJjmU",
      "v4pqTQEMnb8hVwKMfSYqOmqwo4GsMIGpMB0GA1UdDgQWBBR8NRVz81tKH8nCWLNI",
      "Pn7zdlXakTAMBgNVHRMBAf8EAjAAMB8GA1UdIwQYMBaAFCVFUSwlXOOm5JvKD5o1",
      "fvsmUu2bMB0GA1UdEQEB/wQTMBGHBH8AAAGCCWxvY2FsaG9zdDAOBgNVHQ8BAf8E",
      "BAMCBaAwKgYDVR0lAQH/BCAwHgYIKwYBBQUHAwIGCCsGAQUFBwMBBggrBgEFBQcD",
      "AzAKBggqhkjOPQQDAgNIADBFAiEAujBETz7z5tWMOpwL/NQFEX9LcbcuHA3+T2oa",
      "6z0Y87gCIDvX/o0KT31LKZM9LklDE11u1S63AYjY0948jEd4Jnrx",
      "-----END CERTIFICATE-----"
    ]

manualTlsPrivateKeyPem :: String
manualTlsPrivateKeyPem =
  unlines
    [ "-----BEGIN EC PRIVATE KEY-----",
      "MHcCAQEEIJ9itNr2Vm4XTUo74d26GQWuZNdRfEjN6cZqWK418T5LoAoGCCqGSM49",
      "AwEHoUQDQgAESujREIX3BmAbdk0VpK5LVqWHZgr2KO+rHIh+9JGAXIoc46mihMOX",
      "Q/ym3SY5lL+Kak0BDJ2/IVcCjH0mKjpqsA==",
      "-----END EC PRIVATE KEY-----"
    ]
