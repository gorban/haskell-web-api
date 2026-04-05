{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Concurrent (threadDelay)
import Control.Exception (finally, try)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteStringChar8
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Network.Socket (Family (AF_INET), SockAddr (SockAddrInet), SocketType (Stream), bind, close, defaultProtocol, getSocketName, socket, tupleToHostAddress)
import qualified Network.Socket as NetworkSocket
import qualified Network.Socket.ByteString as SocketByteString
import Numeric (readHex)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (hClose)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import System.Process (ProcessHandle, StdStream (UseHandle), createProcess, cwd, env, getProcessExitCode, proc, readCreateProcessWithExitCode, std_out, terminateProcess, waitForProcess)
import TestSupport.RealPostgres (databaseSetupEnvironment, defaultRealPostgresConfig, ensureDefaultPostgresAvailable, supportedPostgresMajorVersions, withContainerizedPsqlOnPath)
import WebApi.Database (DatabaseEffect (..), HomePageData (..), SecondPageData (..))
import WebApi.Postgres (buildPostgresDatabaseEffect)
import WebApi.Route (AppLocale (French), AppRequestContext (..), defaultRequestContext)

spec = do
  describe "main" $
    it "stays running, serves real HTTP traffic, and only stops when terminated" $ do
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
                  readyResponse <- waitForProcessResponse processHandle unusedPort "/api/status"
                  stillRunningExitCode <- getProcessExitCode processHandle
                  pure (readyResponse, stillRunningExitCode)
              )
                `finally` do
                  terminateProcess processHandle
                  _ <- waitForProcess processHandle
                  hClose outputHandle
            responseText `shouldBe` "{\"status\":\"ok\",\"locale\":\"en\"}"
            runningExitCode `shouldBe` Nothing
            readFile outputPath `shouldReturn` ("HTTP Server listening at http://127.0.0.1:" <> show unusedPort <> "\n")

  describe "database integration" $
    it "runs migrate-and-seed, verifies the supported PostgreSQL major version, loads seeded page data, and enforces runtime-role privileges against real PostgreSQL" $
      withContainerizedPsqlOnPath $ do
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
            frenchRequestContext = defaultRequestContext {requestLocale = French}
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
        loadHomePageData postgresEffect frenchRequestContext
          `shouldReturn` Right
            HomePageData
              { homePageDataSummary = "Accueil cote serveur avec des donnees de developpement preconfigurees."
              }
        loadSecondPageData postgresEffect frenchRequestContext
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
  waitForResponseAttempts (500 :: Int)
  where
    waitForResponseAttempts remainingAttempts = do
      exitCode <- getProcessExitCode processHandle
      case exitCode of
        Just completedExitCode ->
          expectationFailure ("expected haskell-web-api to keep running, but it exited early with " <> show completedExitCode)
            >> pure Text.empty
        Nothing -> do
          responseResult <- try (readLoopbackHttpResponse port path) :: IO (Either IOError Text.Text)
          case responseResult of
            Right responseText -> pure responseText
            Left _
              | remainingAttempts > 0 -> do
                  threadDelay 10000
                  waitForResponseAttempts (remainingAttempts - 1)
              | otherwise ->
                  expectationFailure "expected haskell-web-api to accept loopback HTTP requests"
                    >> pure Text.empty

readLoopbackHttpResponse :: Int -> Text.Text -> IO Text.Text
readLoopbackHttpResponse port path = do
  responseBytes <- readLoopbackHttpResponseBytes port path
  pure (TextEncoding.decodeUtf8 responseBytes)

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
