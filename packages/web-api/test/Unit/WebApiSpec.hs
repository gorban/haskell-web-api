{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{-# SPEC #-}

import Control.Concurrent (MVar, forkIO, killThread, newEmptyMVar, putMVar, readMVar, threadDelay)
import Control.Exception (IOException, SomeException, displayException, finally, try)
import qualified Core.Setup.PrerequisiteConfig as PrerequisiteConfig
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as ByteStringChar8
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Char (toLower)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified HarchWeb
import qualified HarchWeb.Observability as Observability
import qualified Network.HTTP.Types as Http
import Network.Socket (Family (AF_INET), SockAddr (SockAddrInet), SocketType (Stream), bind, close, defaultProtocol, getSocketName, listen, socket, tupleToHostAddress)
import qualified Network.Socket as NetworkSocket
import qualified Network.Socket.ByteString as SocketByteString
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as WaiInternal
import Numeric (readHex)
import System.Directory (createDirectory, getCurrentDirectory, removePathForcibly, setCurrentDirectory)
import System.Environment (getEnv, getEnvironment, lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.IO (hClose)
import System.IO.Error (isAlreadyInUseError)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import System.Process (callProcess)
import TestSupport.RealPostgres (containerizedPsqlScriptContents, defaultMigrationPostgresConfig, defaultRealPostgresConfig, ensureDefaultPostgresAvailable, ensureDefaultPostgresAvailableScript, withContainerizedPsqlOnPath)
import Text.Read (readMaybe)
import WebApi (buildApp, run)
import WebApi.App (buildAppWithDatabase, buildRuntimeAppWithDatabaseBuilder, runWithConfig)
import WebApi.App.Enhancements (pageEnhancementHooks)
import WebApi.App.Shell (buildAppPageShell)
import WebApi.Config (AcmeConfig (..), AppConfig (..), AppEnvironmentConfig (..), AppEnvironmentConfigLoadError (..), AppMode (..), AppStartupConfig (..), AppStartupConfigLoadError (..), CertbotConfig (..), CorsPolicyConfig (..), DatabaseConfig (..), ListenerConfig (..), ListenerScheme (..), ObservabilityConfig (..), OtlpExporter (..), RequestPolicyConfig (..), ResponseSecurityHeadersConfig (..), StaticAssetRoot (..), StaticAssetsConfig (..), StrictTransportSecurityConfig (..), TlsCertificateSource (..), TlsConfig (..), TlsStartupMode (..), committedEnvDefaults, committedRuntimeDefaults, defaultAppConfig, defaultAppEnvironmentConfig, defaultAppStartupConfig, defaultCorsPolicyConfig, defaultResponseSecurityHeadersConfig, defaultStaticAssetContentTypes, loadAppEnvironmentConfig, loadAppEnvironmentConfigWithFiles, loadAppStartupConfig, loadAppStartupConfigWithFiles, parseAppEnvironmentConfig, parseAppStartupConfig, parseRuntimeAppConfig)
import WebApi.Database (DatabaseEffect (..), DatabaseError (..), DatabaseOperation (..), DatabaseResult (..), DatabaseSeed (..), HomePageData (..), SecondPageData (..), buildSeededDatabaseEffect, defaultDatabaseEffect, defaultDatabaseSeed)
import WebApi.DatabaseSetup (DatabaseSetupCommand (..), DatabaseSetupError (..), loadDatabaseSetupConfig, parseDatabaseSetupCommand, parseDatabaseSetupConfig, renderDatabaseSetupError, runDatabaseSetupArgs, runDatabaseSetupArgsWith, runDatabaseSetupCommand, runDatabaseSetupCommandWith)
import WebApi.Page (AppPageModel (..), CallToAction (..), HomePageModel (..), NotFoundPageModel (..), SecondPageModel (..), buildPageModel, buildPageModelFromRouteData, buildPageModelWithDatabase, renderPage, renderPageBody, renderPageFromRouteData, renderPageWithDatabase)
import qualified WebApi.PageShell as LegacyPageShell
import WebApi.Postgres (PostgresCommand (..), PostgresCommandResult (..), PostgresRunnerError (..), buildPostgresDatabaseEffect, buildPostgresDatabaseEffectWithRunner, buildRuntimePostgresDatabaseEffectWithRunner, decodeRuntimeQueryValue, migrationStatementsFor, renderRuntimeConnectionErrorMessage, renderRuntimeResultErrorMessage, runPostgresMigrations, runPostgresMigrationsForRuntime, runPostgresMigrationsWithRunner, runPostgresMigrationsWithRunnerForRuntime, runPostgresSeed, runPostgresSeedWithRunner, runRuntimeRowsQuery, runRuntimeScalarQuery, seedStatements)
import WebApi.Response (renderApiResponseFromRouteData, selectResponse, selectResponseWithDatabase)
import WebApi.Route (AppLocale (..), AppRequestContext (..), AppRoute (..), RequestSurface (..), RouteSelectionError (..), defaultRequestContext, parseRoute, renderRoutePath, selectRoute)
import qualified WebApi.Route
import WebApi.RouteData (HomeRouteData (..), RouteDataResult (..), RouteDataSelection (..), SecondRouteData (..), StatusApiData (..), selectRouteData, selectRouteDataSelectionWithDatabase, selectRouteDataWithDatabase)
import WebApi.SetupConfig (AppSetupConfig (..), AppSetupConfigLoadError (..), SetupAutostartConfig (..), committedSetupDefaults, defaultAppSetupConfig, defaultSetupAutostartConfig, loadAppSetupConfig, loadAppSetupConfigWithFiles, parseAppSetupConfig)
import WebApi.SetupPlan (AppPrerequisitePlan (..), ContainerAutostartPlan (..), ContainerRuntime (..), DatabasePrerequisitePlan (..), TcpEndpoint (..), TracingEndpointParseError (..), TracingPrerequisitePlan (..), checkTcpEndpointReachable, checkTcpEndpointReachableWithTimeout, checkTracingEndpointReachable, defaultContainerAutostartPlan, parseTracingEndpoint, planAppPrerequisites, toSetupPrerequisiteConfig)

pureApplication :: HarchWeb.Application AppRoute AppRequestContext
pureApplication = buildApp defaultAppConfig

navigationAppConfig :: AppConfig
navigationAppConfig =
  defaultAppConfig
    { staticAssets =
        StaticAssetsConfig
          { staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = "/assets", staticDirectory = "public"}],
            staticAssetContentTypes = defaultStaticAssetContentTypes,
            staticCacheControlSeconds = Nothing
          }
    }

homeRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
homeRequest = HarchWeb.RouteRequest {HarchWeb.requestRoute = HomeRoute, HarchWeb.requestContext = defaultRequestContext}

secondRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
secondRequest = HarchWeb.RouteRequest {HarchWeb.requestRoute = SecondRoute, HarchWeb.requestContext = defaultRequestContext}

frenchRequestContext :: AppRequestContext
frenchRequestContext = defaultRequestContext {requestLocale = French}

prefixedRequestContext :: AppRequestContext
prefixedRequestContext = defaultRequestContext {requestPathPrefix = "/app"}

prefixedFrenchRequestContext :: AppRequestContext
prefixedFrenchRequestContext = frenchRequestContext {requestPathPrefix = "/app"}

frenchHomeRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
frenchHomeRequest = HarchWeb.RouteRequest {HarchWeb.requestRoute = HomeRoute, HarchWeb.requestContext = frenchRequestContext}

frenchSecondRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
frenchSecondRequest = HarchWeb.RouteRequest {HarchWeb.requestRoute = SecondRoute, HarchWeb.requestContext = frenchRequestContext}

prefixedHomeRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
prefixedHomeRequest = HarchWeb.RouteRequest {HarchWeb.requestRoute = HomeRoute, HarchWeb.requestContext = prefixedRequestContext}

prefixedSecondRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
prefixedSecondRequest = HarchWeb.RouteRequest {HarchWeb.requestRoute = SecondRoute, HarchWeb.requestContext = prefixedRequestContext}

prefixedFrenchSecondRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
prefixedFrenchSecondRequest = HarchWeb.RouteRequest {HarchWeb.requestRoute = SecondRoute, HarchWeb.requestContext = prefixedFrenchRequestContext}

prefixedApiStatusRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
prefixedApiStatusRequest =
  HarchWeb.RouteRequest
    { HarchWeb.requestRoute = StatusApiRoute,
      HarchWeb.requestContext = prefixedRequestContext {requestSurface = ApiSurface}
    }

frenchApiStatusRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
frenchApiStatusRequest =
  HarchWeb.RouteRequest
    { HarchWeb.requestRoute = StatusApiRoute,
      HarchWeb.requestContext = frenchRequestContext {requestSurface = ApiSurface}
    }

frenchApiSecondRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
frenchApiSecondRequest =
  HarchWeb.RouteRequest
    { HarchWeb.requestRoute = SecondRoute,
      HarchWeb.requestContext = frenchRequestContext {requestSurface = ApiSurface}
    }

notFoundRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
notFoundRequest = HarchWeb.RouteRequest {HarchWeb.requestRoute = NotFoundRoute, HarchWeb.requestContext = defaultRequestContext}

apiStatusRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
apiStatusRequest =
  HarchWeb.RouteRequest
    { HarchWeb.requestRoute = StatusApiRoute,
      HarchWeb.requestContext = defaultRequestContext {requestSurface = ApiSurface}
    }

apiSecondRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
apiSecondRequest =
  HarchWeb.RouteRequest
    { HarchWeb.requestRoute = SecondRoute,
      HarchWeb.requestContext = defaultRequestContext {requestSurface = ApiSurface}
    }

apiNotFoundRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
apiNotFoundRequest =
  HarchWeb.RouteRequest
    { HarchWeb.requestRoute = NotFoundRoute,
      HarchWeb.requestContext = defaultRequestContext {requestSurface = ApiSurface}
    }

pureRouteMatcher :: Text -> HarchWeb.RouteRequest AppRoute AppRequestContext
pureRouteMatcher = WebApi.Route.matchRoute WebApi.Route.defaultRequestContext

renderedShell :: AppConfig -> AppRoute -> IO Text
renderedShell config route = do
  renderedShellForRequest
    config
    HarchWeb.RouteRequest
      { HarchWeb.requestRoute = route,
        HarchWeb.requestContext = defaultRequestContext
      }

renderedShellForRequest :: AppConfig -> HarchWeb.RouteRequest AppRoute AppRequestContext -> IO Text
renderedShellForRequest config routeRequest = do
  let application = buildApp config
  page <- renderPage config routeRequest
  pure (HarchWeb.pageShell application page)

data CapturedOtlpRequest = CapturedOtlpRequest
  { capturedOtlpMethod :: ByteString.ByteString,
    capturedOtlpPath :: ByteString.ByteString,
    capturedOtlpHeaders :: [(ByteString.ByteString, ByteString.ByteString)],
    capturedOtlpBody :: ByteString.ByteString
  }

performWaiRequest :: Wai.Application -> Wai.Request -> IO Wai.Response
performWaiRequest webApplication request = do
  responseReference <- newIORef Nothing
  _ <- webApplication request (\response -> writeIORef responseReference (Just response) >> pure WaiInternal.ResponseReceived)
  maybeResponse <- readIORef responseReference
  pure (fromMaybe (error "expected WAI application to produce a response") maybeResponse)

readResponseBody :: Wai.Response -> IO Text
readResponseBody response = do
  let (_, _, withStreamingBody) = Wai.responseToStream response
  chunksReference <- newIORef []
  withStreamingBody $ \streamingBody ->
    streamingBody
      (\builder -> modifyIORef' chunksReference (<> [Builder.toLazyByteString builder]))
      (pure ())
  chunks <- readIORef chunksReference
  pure (TextEncoding.decodeUtf8 (LazyByteString.toStrict (mconcat chunks)))

waiRequest :: [Text] -> Wai.Request
waiRequest segments =
  Wai.defaultRequest
    { Wai.rawPathInfo = TextEncoding.encodeUtf8 renderedPath,
      Wai.pathInfo = segments
    }
  where
    renderedPath =
      case segments of
        [] -> "/"
        _ -> "/" <> Text.intercalate "/" segments

postgresTestConfig :: DatabaseConfig
postgresTestConfig =
  DatabaseConfig
    { databaseHost = "db.internal",
      databasePort = 6543,
      databaseName = "web_api_prod",
      databaseUser = "web_api_app",
      databasePassword = "super-secret"
    }

migrationPostgresTestConfig :: DatabaseConfig
migrationPostgresTestConfig =
  postgresTestConfig
    { databaseUser = "web_api_owner",
      databasePassword = "owner-secret"
    }

setupMigrationPostgresTestConfig :: DatabaseConfig
setupMigrationPostgresTestConfig =
  DatabaseConfig
    { databaseHost = "127.0.0.1",
      databasePort = 5432,
      databaseName = "web_api_dev",
      databaseUser = "web_api_owner",
      databasePassword = "owner-secret"
    }

runtimeSetupPostgresTestConfig :: DatabaseConfig
runtimeSetupPostgresTestConfig =
  DatabaseConfig
    { databaseHost = "127.0.0.1",
      databasePort = 5432,
      databaseName = "web_api_dev",
      databaseUser = "web_api_runtime",
      databasePassword = "runtime-secret"
    }

successfulPostgresResult :: Text -> PostgresCommandResult
successfulPostgresResult stdoutText =
  PostgresCommandResult
    { postgresExitCode = ExitSuccess,
      postgresStdout = stdoutText,
      postgresStderr = Text.empty
    }

failingPostgresResult :: Text -> PostgresCommandResult
failingPostgresResult stderrText =
  PostgresCommandResult
    { postgresExitCode = ExitFailure 1,
      postgresStdout = Text.empty,
      postgresStderr = stderrText
    }

commandSql :: PostgresCommand -> Text
commandSql command =
  case reverse (postgresArguments command) of
    sqlArgument : _ -> Text.pack sqlArgument
    [] -> Text.empty

withTemporaryEnvironment :: String -> Maybe String -> IO a -> IO a
withTemporaryEnvironment key maybeValue action = do
  previousValue <- lookupEnv key
  case maybeValue of
    Just value -> setEnv key value
    Nothing -> unsetEnv key
  let restore =
        case previousValue of
          Just value -> setEnv key value
          Nothing -> unsetEnv key
  action `finally` restore

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory directory action = do
  previousDirectory <- getCurrentDirectory
  setCurrentDirectory directory
  action `finally` setCurrentDirectory previousDirectory

withUnreadableFile :: FilePath -> String -> IO a -> IO a
withUnreadableFile filePath _fileContents action = do
  createDirectory filePath
  action `finally` removePathForcibly filePath

withClearedAppEnvironment :: IO a -> IO a
withClearedAppEnvironment =
  withTemporaryEnvironment "APP_MODE" Nothing
    . withTemporaryEnvironment "DATABASE_HOST" Nothing
    . withTemporaryEnvironment "DATABASE_PORT" Nothing
    . withTemporaryEnvironment "DATABASE_NAME" Nothing
    . withTemporaryEnvironment "DATABASE_USER" Nothing
    . withTemporaryEnvironment "DATABASE_PASSWORD" Nothing

withClearedEnvironmentPrefixes :: [String] -> IO a -> IO a
withClearedEnvironmentPrefixes prefixes action = do
  environment <- getEnvironment
  let matchingKeys =
        [ key
        | (key, _) <- environment,
          any (`isPrefixOf` key) prefixes
        ]
  foldr (`withTemporaryEnvironment` Nothing) action matchingKeys

withClearedRuntimeEnvironment :: IO a -> IO a
withClearedRuntimeEnvironment =
  withClearedEnvironmentPrefixes
    [ "APP_TITLE_PREFIX",
      "LISTENER_",
      "STATIC_ASSET_CONTENT_TYPE_",
      "STATIC_ASSET_ROOT_",
      "STATIC_CACHE_CONTROL_SECONDS",
      "REDIRECT_HTTP_TO_HTTPS",
      "HTTPS_REDIRECT_PORT",
      "HSTS_",
      "CORS_",
      "CONTENT_SECURITY_POLICY",
      "X_CONTENT_TYPE_OPTIONS_NOSNIFF",
      "X_XSS_PROTECTION",
      "REFERRER_POLICY",
      "PERMISSIONS_POLICY",
      "X_FRAME_OPTIONS",
      "OTLP_TRACING_",
      "OTLP_METRICS_"
    ]

withClearedSetupEnvironment :: IO a -> IO a
withClearedSetupEnvironment =
  withClearedEnvironmentPrefixes
    [ "SETUP_AUTOSTART_",
      "WEB_API_MIGRATION_DATABASE_"
    ]

withFakePsqlScriptResults :: [(Text, PostgresCommandResult)] -> (FilePath -> IO a) -> IO a
withFakePsqlScriptResults commandResults action =
  withSystemTempDirectory "fake-psql" $ \tempDirectory -> do
    originalPath <- getEnv "PATH"
    let scriptPath = tempDirectory <> "/psql"
        argsLogPath = tempDirectory <> "/psql-args.log"
        scriptBody =
          unlines
            ( [ "#!/usr/bin/env bash",
                "set -euo pipefail",
                "printf '%s\\n' \"$*\" >> \"$PSQL_ARGS_LOG\"",
                "sql=''",
                "while [ \"$#\" -gt 0 ]; do",
                "  case \"$1\" in",
                "    --command)",
                "      sql=\"$2\"",
                "      shift 2",
                "      ;;",
                "    *)",
                "      shift",
                "      ;;",
                "  esac",
                "done",
                "case \"$sql\" in"
              ]
                ++ concatMap renderCase commandResults
                ++ [ "  *)",
                     "    exit 0",
                     "    ;;",
                     "esac"
                   ]
            )
    writeFile scriptPath scriptBody
    callProcess "chmod" ["+x", scriptPath]
    withTemporaryEnvironment "PSQL_ARGS_LOG" (Just argsLogPath) $
      withTemporaryEnvironment "PATH" (Just (tempDirectory <> ":" <> originalPath)) $
        action argsLogPath
  where
    renderCase (sqlText, commandResult) =
      [ "  " <> show (Text.unpack sqlText) <> ")"
      ]
        ++ renderStdoutLines (postgresStdout commandResult)
        ++ renderStderrLines (postgresStderr commandResult)
        ++ [ "    exit " <> renderExitCode (postgresExitCode commandResult),
             "    ;;"
           ]

    renderStdoutLines stdoutText =
      case Text.unpack stdoutText of
        "" -> []
        stdoutValue -> ["    printf %s\\\\n " <> show stdoutValue]

    renderStderrLines stderrText =
      case Text.unpack stderrText of
        "" -> []
        stderrValue -> ["    printf %s\\\\n " <> show stderrValue <> " >&2"]

    renderExitCode exitCode =
      case exitCode of
        ExitSuccess -> "0"
        ExitFailure code -> show code

withFakePsqlScript :: [(Text, Text)] -> (FilePath -> IO a) -> IO a
withFakePsqlScript commandOutputs =
  withFakePsqlScriptResults
    (map toSuccessfulCommandResult commandOutputs)
  where
    toSuccessfulCommandResult (sqlText, stdoutText) =
      (sqlText, successfulPostgresResult stdoutText)

withListeningTcpEndpoint :: (TcpEndpoint -> IO a) -> IO a
withListeningTcpEndpoint action = do
  listenerSocket <- socket AF_INET Stream defaultProtocol
  bind listenerSocket (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  listen listenerSocket 1
  socketAddress <- getSocketName listenerSocket
  case socketAddress of
    SockAddrInet port _ ->
      action
        TcpEndpoint
          { tcpEndpointHost = "127.0.0.1",
            tcpEndpointPort = fromIntegral port
          }
        `finally` close listenerSocket
    _ ->
      close listenerSocket
        >> error "expected IPv4 loopback test socket"

withUnusedTcpEndpoint :: (TcpEndpoint -> IO a) -> IO a
withUnusedTcpEndpoint action = do
  reservedSocket <- socket AF_INET Stream defaultProtocol
  bind reservedSocket (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  socketAddress <- getSocketName reservedSocket
  case socketAddress of
    SockAddrInet port _ -> do
      close reservedSocket
      action
        TcpEndpoint
          { tcpEndpointHost = "127.0.0.1",
            tcpEndpointPort = fromIntegral port
          }
    _ ->
      close reservedSocket
        >> error "expected IPv4 loopback reservation socket"

withDefaultRuntimePortUnavailable :: IO a -> IO a
withDefaultRuntimePortUnavailable action = do
  reservedSocketResult <- try (socket AF_INET Stream defaultProtocol >>= reserveDefaultRuntimePort) :: IO (Either IOError NetworkSocket.Socket)
  case reservedSocketResult of
    Left bindError
      | isAlreadyInUseError bindError -> action
      | otherwise -> ioError bindError
    Right reservedSocket ->
      action `finally` close reservedSocket
  where
    reserveDefaultRuntimePort reservedSocket = do
      bind reservedSocket (SockAddrInet 5001 (tupleToHostAddress (127, 0, 0, 1)))
      listen reservedSocket 1
      pure reservedSocket

withOtlpCaptureServer ::
  Http.Status ->
  ByteString.ByteString ->
  (Text -> MVar CapturedOtlpRequest -> IO a) ->
  IO a
withOtlpCaptureServer responseStatus responseBody action = do
  listenerSocket <- socket AF_INET Stream defaultProtocol
  bind listenerSocket (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  listen listenerSocket 1
  socketAddress <- getSocketName listenerSocket
  case socketAddress of
    SockAddrInet port _ -> do
      capturedRequestReference <- newEmptyMVar
      let collectorUrl = Text.pack ("http://127.0.0.1:" <> show (fromIntegral port :: Int) <> "/v1/traces")
      serverThreadId <-
        forkIO $
          ( do
              (clientSocket, _) <- NetworkSocket.accept listenerSocket
              capturedRequest <- readCapturedHttpRequest clientSocket
              putMVar capturedRequestReference capturedRequest
              SocketByteString.sendAll clientSocket (buildHttpResponse responseStatus responseBody)
              close clientSocket
          )
            `finally` close listenerSocket
      action collectorUrl capturedRequestReference `finally` killThread serverThreadId
    _ ->
      close listenerSocket
        >> error "expected IPv4 loopback OTLP capture socket"

readCapturedHttpRequest :: NetworkSocket.Socket -> IO CapturedOtlpRequest
readCapturedHttpRequest clientSocket = do
  requestBytes <- readHttpRequestBytes clientSocket
  let (headerBytes, withSeparator) = ByteStringChar8.breakSubstring "\r\n\r\n" requestBytes
      requestBody = ByteString.drop 4 withSeparator
      headerLines = map stripHeaderLineEnd (ByteStringChar8.split '\n' headerBytes)
      requestLine =
        case headerLines of
          line : _ -> line
          [] -> ByteString.empty
      (requestMethod, requestPath) =
        case ByteStringChar8.words requestLine of
          method : path : _ -> (method, path)
          _ -> (ByteString.empty, ByteString.empty)
  pure
    CapturedOtlpRequest
      { capturedOtlpMethod = requestMethod,
        capturedOtlpPath = requestPath,
        capturedOtlpHeaders = mapMaybe parseCapturedHeader (drop 1 headerLines),
        capturedOtlpBody = requestBody
      }

readHttpRequestBytes :: NetworkSocket.Socket -> IO ByteString.ByteString
readHttpRequestBytes clientSocket =
  readRequestChunks ByteString.empty Nothing
  where
    readRequestChunks accumulatedRequest knownContentLength = do
      chunk <- SocketByteString.recv clientSocket 4096
      let accumulatedRequest' = accumulatedRequest <> chunk
          contentLength =
            case knownContentLength of
              Just value -> Just value
              Nothing -> parseHttpContentLength accumulatedRequest'
      case contentLength of
        Just bodyLength
          | ByteString.length (extractHttpBody accumulatedRequest') >= bodyLength ->
              pure accumulatedRequest'
        _ ->
          if ByteString.null chunk
            then pure accumulatedRequest'
            else readRequestChunks accumulatedRequest' contentLength

parseHttpContentLength :: ByteString.ByteString -> Maybe Int
parseHttpContentLength requestBytes =
  case ByteStringChar8.breakSubstring "\r\n\r\n" requestBytes of
    (_, withSeparator)
      | ByteString.null withSeparator -> Nothing
    (headerBytes, _) ->
      lookup "content-length" (mapMaybe parseCapturedHeader (drop 1 headerLines)) >>= readMaybe . ByteStringChar8.unpack
      where
        headerLines = map stripHeaderLineEnd (ByteStringChar8.split '\n' headerBytes)

parseCapturedHeader :: ByteString.ByteString -> Maybe (ByteString.ByteString, ByteString.ByteString)
parseCapturedHeader headerLine =
  case ByteStringChar8.break (== ':') headerLine of
    (headerName, withSeparator)
      | ByteString.null withSeparator -> Nothing
      | otherwise ->
          Just
            ( ByteStringChar8.map toLower headerName,
              ByteStringChar8.dropWhile (== ' ') (stripHeaderLineEnd (ByteString.drop 1 withSeparator))
            )

stripHeaderLineEnd :: ByteString.ByteString -> ByteString.ByteString
stripHeaderLineEnd =
  ByteStringChar8.filter (/= '\r')

buildHttpResponse :: Http.Status -> ByteString.ByteString -> ByteString.ByteString
buildHttpResponse responseStatus responseBody =
  ByteStringChar8.pack $
    "HTTP/1.1 "
      <> show (Http.statusCode responseStatus)
      <> " "
      <> ByteStringChar8.unpack (Http.statusMessage responseStatus)
      <> "\r\nContent-Type: application/json\r\nContent-Length: "
      <> show (ByteString.length responseBody)
      <> "\r\nConnection: close\r\n\r\n"
      <> ByteStringChar8.unpack responseBody

readLoopbackHttpResponse :: Int -> Text -> IO Text
readLoopbackHttpResponse port path = do
  responseBytes <- readLoopbackHttpResponseBytes port path
  pure (TextEncoding.decodeUtf8 responseBytes)

readLoopbackHttpResponseBytes :: Int -> Text -> IO ByteString.ByteString
readLoopbackHttpResponseBytes port path = do
  clientSocket <- socket AF_INET Stream defaultProtocol
  connect clientSocket
  SocketByteString.sendAll clientSocket (buildHttpRequest path)
  responseBytes <- readAllSocketChunks clientSocket
  close clientSocket
  pure (extractHttpBody responseBytes)
  where
    connect clientSocket =
      NetworkSocket.connect clientSocket (SockAddrInet (fromIntegral port) (tupleToHostAddress (127, 0, 0, 1)))

waitForRuntimeServerResponse :: IORef (Maybe (Either SomeException ())) -> Int -> Text -> IO Text
waitForRuntimeServerResponse completionReference port path =
  waitForResponseAttempts (500 :: Int)
  where
    waitForResponseAttempts remainingAttempts = do
      completionResult <- readIORef completionReference
      case completionResult of
        Just (Left exception) ->
          expectationFailure ("expected runtime server to remain running, but it failed early: " <> displayException exception)
            >> pure Text.empty
        Just (Right ()) ->
          expectationFailure "expected runtime server to remain running, but it exited early"
            >> pure Text.empty
        Nothing -> do
          responseResult <- try (readLoopbackHttpResponse port path) :: IO (Either IOError Text)
          case responseResult of
            Right responseText -> pure responseText
            Left _
              | remainingAttempts > 0 -> do
                  threadDelay 10000
                  waitForResponseAttempts (remainingAttempts - 1)
              | otherwise ->
                  expectationFailure "expected runtime server to accept loopback HTTP requests"
                    >> pure Text.empty

waitForRuntimeServerExit :: IORef (Maybe (Either SomeException ())) -> IO ()
waitForRuntimeServerExit completionReference =
  waitForExitAttempts (500 :: Int)
  where
    waitForExitAttempts remainingAttempts = do
      completionResult <- readIORef completionReference
      case completionResult of
        Just _ -> pure ()
        Nothing
          | remainingAttempts > 0 -> do
              threadDelay 10000
              waitForExitAttempts (remainingAttempts - 1)
          | otherwise ->
              expectationFailure "expected runtime server to stop after being signalled"

buildHttpRequest :: Text -> ByteString.ByteString
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

spec = do
  describe "defaultAppConfig" $ do
    it "reserves structured listener, static asset, and observability settings" $ do
      defaultAppConfig
        `shouldBe` AppConfig
          { appTitlePrefix = "web-api",
            listenerConfigs =
              [ ListenerConfig
                  { listenerHost = "127.0.0.1",
                    listenerPort = 5001,
                    listenerScheme = Http,
                    listenerTls = Nothing,
                    listenerAcme = Nothing
                  }
              ],
            staticAssets =
              StaticAssetsConfig
                { staticAssetRoots = [],
                  staticAssetContentTypes = defaultStaticAssetContentTypes,
                  staticCacheControlSeconds = Nothing
                },
            requestPolicy =
              RequestPolicyConfig
                { redirectHttpToHttps = False,
                  httpsRedirectPort = Nothing,
                  strictTransportSecurity = Nothing,
                  corsPolicy = defaultCorsPolicyConfig,
                  responseSecurityHeaders = defaultResponseSecurityHeadersConfig
                },
            observability =
              ObservabilityConfig
                { tracingExporter = Nothing,
                  metricsExporter = Nothing
                }
          }
      let serverConfig = HarchWeb.toServerConfig defaultAppConfig
      HarchWeb.listenerConfigs serverConfig `shouldBe` listenerConfigs defaultAppConfig
      HarchWeb.staticAssets serverConfig `shouldBe` staticAssets defaultAppConfig
      HarchWeb.requestPolicy serverConfig `shouldBe` requestPolicy defaultAppConfig
      HarchWeb.observability serverConfig `shouldBe` observability defaultAppConfig

  describe "parseRuntimeAppConfig" $ do
    it "parses committed runtime defaults into the expected app config" $
      parseRuntimeAppConfig committedRuntimeDefaults [] []
        `shouldBe` Right defaultAppConfig

    it "fails when no listeners are configured" $
      parseRuntimeAppConfig
        [("APP_TITLE_PREFIX", "runtime-test")]
        []
        []
        `shouldBe` Left (MissingConfigValue "LISTENER_0_HOST")

    it "parses multiple listeners in deterministic index order" $ do
      let committedDefaults =
            [ ("APP_TITLE_PREFIX", "runtime-test"),
              ("LISTENER_2_SCHEME", "http"),
              ("LISTENER_1_PORT", "5002"),
              ("LISTENER_2_PORT", "5003"),
              ("LISTENER_1_HOST", "127.0.0.2"),
              ("LISTENER_2_HOST", "127.0.0.3"),
              ("LISTENER_1_SCHEME", "http")
            ]
      parseRuntimeAppConfig committedDefaults [] []
        `shouldBe` Right
          AppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = "127.0.0.2",
                      listenerPort = 5002,
                      listenerScheme = Http,
                      listenerTls = Nothing,
                      listenerAcme = Nothing
                    },
                  ListenerConfig
                    { listenerHost = "127.0.0.3",
                      listenerPort = 5003,
                      listenerScheme = Http,
                      listenerTls = Nothing,
                      listenerAcme = Nothing
                    }
                ],
              staticAssets =
                StaticAssetsConfig
                  { staticAssetRoots = [],
                    staticAssetContentTypes = defaultStaticAssetContentTypes,
                    staticCacheControlSeconds = Nothing
                  },
              requestPolicy = requestPolicy defaultAppConfig,
              observability =
                ObservabilityConfig
                  { tracingExporter = Nothing,
                    metricsExporter = Nothing
                  }
            }

    it "requires HTTPS listeners to specify a TLS source" $
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "0.0.0.0"),
          ("LISTENER_0_PORT", "5443"),
          ("LISTENER_0_SCHEME", "https")
        ]
        []
        []
        `shouldBe` Left (MissingConfigValue "LISTENER_0_TLS_SOURCE")

    it "defaults redirects on and records the HTTPS port when one HTTP and one manual HTTPS listener are configured" $
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5001"),
          ("LISTENER_0_SCHEME", "http"),
          ("LISTENER_1_HOST", "127.0.0.1"),
          ("LISTENER_1_PORT", "5443"),
          ("LISTENER_1_SCHEME", "https"),
          ("LISTENER_1_TLS_SOURCE", "manual"),
          ("LISTENER_1_TLS_CERTIFICATE_FILE", "cert.pem"),
          ("LISTENER_1_TLS_PRIVATE_KEY_FILE", "key.pem")
        ]
        []
        []
        `shouldBe` Right
          defaultAppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5001,
                      listenerScheme = Http,
                      listenerTls = Nothing,
                      listenerAcme = Nothing
                    },
                  ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5443,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                ManualCertificateFiles
                                  { certificateFile = "cert.pem",
                                    privateKeyFile = "key.pem"
                                  }
                            },
                      listenerAcme = Nothing
                    }
                ],
              requestPolicy =
                RequestPolicyConfig
                  { redirectHttpToHttps = True,
                    httpsRedirectPort = Just 5443,
                    strictTransportSecurity = Nothing,
                    corsPolicy = defaultCorsPolicyConfig,
                    responseSecurityHeaders = defaultResponseSecurityHeadersConfig
                  }
            }

    it "parses shared HTTPS directories and ACME certificate publishing directories" $
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5443"),
          ("LISTENER_0_SCHEME", "https"),
          ("LISTENER_0_TLS_SOURCE", "shared"),
          ("LISTENER_0_TLS_CERTIFICATE_DIRECTORY", "/var/lib/web-api/shared-certs"),
          ("LISTENER_1_HOST", "127.0.0.1"),
          ("LISTENER_1_PORT", "5444"),
          ("LISTENER_1_SCHEME", "https"),
          ("LISTENER_1_TLS_SOURCE", "acme"),
          ("LISTENER_1_ACME_DIRECTORY_URL", "https://acme-staging-v02.api.letsencrypt.org/directory"),
          ("LISTENER_1_ACME_CONTACT_EMAILS", "ops@example.com"),
          ("LISTENER_1_ACME_DOMAINS", "example.com,www.example.com"),
          ("LISTENER_1_ACME_CERTIFICATE_DIRECTORY", "/var/lib/web-api/shared-certs")
        ]
        []
        []
        `shouldBe` Right
          defaultAppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5443,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                SharedCertificateFiles
                                  { certificateDirectory = "/var/lib/web-api/shared-certs",
                                    sharedCertificateStartupMode = AwaitCertificateFiles Nothing
                                  }
                            },
                      listenerAcme = Nothing
                    },
                  ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5444,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                AcmeCertificateSource
                                  AcmeConfig
                                    { acmeDirectoryUrl = "https://acme-staging-v02.api.letsencrypt.org/directory",
                                      acmeContactEmails = ["ops@example.com"],
                                      acmeDomains = ["example.com", "www.example.com"],
                                      acmeHttp01Port = 80,
                                      acmeCertificateDirectory = Just "/var/lib/web-api/shared-certs",
                                      acmeCertbotConfig =
                                        CertbotConfig
                                          { certbotExecutable = "certbot",
                                            certbotArguments = []
                                          }
                                    }
                            },
                      listenerAcme = Nothing
                    }
                ],
              requestPolicy = requestPolicy defaultAppConfig
            }

    it "defaults ACME publish directories and shared TLS reuse directories to .tls paths" $ do
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5443"),
          ("LISTENER_0_SCHEME", "https"),
          ("LISTENER_0_TLS_SOURCE", "shared"),
          ("LISTENER_1_HOST", "127.0.0.1"),
          ("LISTENER_1_PORT", "5444"),
          ("LISTENER_1_SCHEME", "https"),
          ("LISTENER_1_TLS_SOURCE", "acme"),
          ("LISTENER_1_ACME_DIRECTORY_URL", "https://acme-staging-v02.api.letsencrypt.org/directory"),
          ("LISTENER_1_ACME_CONTACT_EMAILS", "ops@example.com"),
          ("LISTENER_1_ACME_DOMAINS", "example.com,www.example.com")
        ]
        []
        []
        `shouldBe` Right
          defaultAppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5443,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                SharedCertificateFiles
                                  { certificateDirectory = ".tls/example.com",
                                    sharedCertificateStartupMode = AwaitCertificateFiles Nothing
                                  }
                            },
                      listenerAcme = Nothing
                    },
                  ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5444,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                AcmeCertificateSource
                                  AcmeConfig
                                    { acmeDirectoryUrl = "https://acme-staging-v02.api.letsencrypt.org/directory",
                                      acmeContactEmails = ["ops@example.com"],
                                      acmeDomains = ["example.com", "www.example.com"],
                                      acmeHttp01Port = 80,
                                      acmeCertificateDirectory = Just ".tls/example.com",
                                      acmeCertbotConfig =
                                        CertbotConfig
                                          { certbotExecutable = "certbot",
                                            certbotArguments = []
                                          }
                                    }
                            },
                      listenerAcme = Nothing
                    }
                ],
              requestPolicy = requestPolicy defaultAppConfig
            }
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5443"),
          ("LISTENER_0_SCHEME", "https"),
          ("LISTENER_0_TLS_SOURCE", "acme"),
          ("LISTENER_0_ACME_DIRECTORY_URL", "https://acme-v02.api.letsencrypt.org/directory"),
          ("LISTENER_0_ACME_CONTACT_EMAILS", "ops@example.com"),
          ("LISTENER_0_ACME_CERTBOT_ARGUMENTS", "certonly,--webroot,--cert-name,prod/example")
        ]
        []
        []
        `shouldBe` Right
          defaultAppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5443,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                AcmeCertificateSource
                                  AcmeConfig
                                    { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                                      acmeContactEmails = ["ops@example.com"],
                                      acmeDomains = [],
                                      acmeHttp01Port = 80,
                                      acmeCertificateDirectory = Just ".tls/prod/example",
                                      acmeCertbotConfig =
                                        CertbotConfig
                                          { certbotExecutable = "certbot",
                                            certbotArguments = ["certonly", "--webroot", "--cert-name", "prod/example"]
                                          }
                                    }
                            },
                      listenerAcme = Nothing
                    }
                ],
              requestPolicy = requestPolicy defaultAppConfig
            }
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5443"),
          ("LISTENER_0_SCHEME", "https"),
          ("LISTENER_0_TLS_SOURCE", "acme"),
          ("LISTENER_0_ACME_DIRECTORY_URL", "https://acme-v02.api.letsencrypt.org/directory"),
          ("LISTENER_0_ACME_CONTACT_EMAILS", "ops@example.com"),
          ("LISTENER_0_ACME_DOMAINS", "one.example.com"),
          ("LISTENER_1_HOST", "127.0.0.1"),
          ("LISTENER_1_PORT", "5444"),
          ("LISTENER_1_SCHEME", "https"),
          ("LISTENER_1_TLS_SOURCE", "acme"),
          ("LISTENER_1_ACME_DIRECTORY_URL", "https://acme-v02.api.letsencrypt.org/directory"),
          ("LISTENER_1_ACME_CONTACT_EMAILS", "ops@example.com"),
          ("LISTENER_1_ACME_DOMAINS", "two.example.com"),
          ("LISTENER_2_HOST", "127.0.0.1"),
          ("LISTENER_2_PORT", "5445"),
          ("LISTENER_2_SCHEME", "https"),
          ("LISTENER_2_TLS_SOURCE", "shared")
        ]
        []
        []
        `shouldBe` Left (MissingConfigValue "LISTENER_2_TLS_CERTIFICATE_DIRECTORY")

    it "parses explicit shared TLS wait and fail-fast startup modes" $ do
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5443"),
          ("LISTENER_0_SCHEME", "https"),
          ("LISTENER_0_TLS_SOURCE", "shared-wait"),
          ("LISTENER_0_TLS_CERTIFICATE_DIRECTORY", "/var/lib/web-api/shared-certs"),
          ("LISTENER_0_TLS_SHARED_WAIT_SECONDS", "15"),
          ("LISTENER_1_HOST", "127.0.0.1"),
          ("LISTENER_1_PORT", "5444"),
          ("LISTENER_1_SCHEME", "https"),
          ("LISTENER_1_TLS_SOURCE", "shared-fail-fast"),
          ("LISTENER_1_TLS_CERTIFICATE_DIRECTORY", "/var/lib/web-api/preprovisioned-certs")
        ]
        []
        []
        `shouldBe` Right
          defaultAppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5443,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                SharedCertificateFiles
                                  { certificateDirectory = "/var/lib/web-api/shared-certs",
                                    sharedCertificateStartupMode = AwaitCertificateFiles (Just 15)
                                  }
                            },
                      listenerAcme = Nothing
                    },
                  ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5444,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                SharedCertificateFiles
                                  { certificateDirectory = "/var/lib/web-api/preprovisioned-certs",
                                    sharedCertificateStartupMode = RequireCertificateFiles
                                  }
                            },
                      listenerAcme = Nothing
                    }
                ],
              requestPolicy = requestPolicy defaultAppConfig
            }
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5443"),
          ("LISTENER_0_SCHEME", "https"),
          ("LISTENER_0_TLS_SOURCE", "shared-fail-fast"),
          ("LISTENER_0_TLS_CERTIFICATE_DIRECTORY", "/var/lib/web-api/shared-certs"),
          ("LISTENER_0_TLS_SHARED_WAIT_SECONDS", "15")
        ]
        []
        []
        `shouldBe` Left (InvalidConfigValue "LISTENER_0_TLS_SHARED_WAIT_SECONDS" "15")
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5443"),
          ("LISTENER_0_SCHEME", "https"),
          ("LISTENER_0_TLS_SOURCE", "shared-wait"),
          ("LISTENER_0_TLS_CERTIFICATE_DIRECTORY", "/var/lib/web-api/shared-certs"),
          ("LISTENER_0_TLS_SHARED_WAIT_SECONDS", "-1")
        ]
        []
        []
        `shouldBe` Left (InvalidConfigValue "LISTENER_0_TLS_SHARED_WAIT_SECONDS" "-1")

    it "defaults production ACME directory URLs and redirects on for HTTP ACME producers plus shared HTTPS listener plans" $
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "8080"),
          ("LISTENER_0_SCHEME", "http"),
          ("LISTENER_0_ACME_CONTACT_EMAILS", "ops@example.com"),
          ("LISTENER_0_ACME_DOMAINS", "example.com,www.example.com"),
          ("LISTENER_1_HOST", "127.0.0.1"),
          ("LISTENER_1_PORT", "5443"),
          ("LISTENER_1_SCHEME", "https"),
          ("LISTENER_1_TLS_SOURCE", "shared-wait"),
          ("LISTENER_1_TLS_SHARED_WAIT_SECONDS", "120")
        ]
        []
        []
        `shouldBe` Right
          defaultAppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 8080,
                      listenerScheme = Http,
                      listenerTls = Nothing,
                      listenerAcme =
                        Just
                          AcmeConfig
                            { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                              acmeContactEmails = ["ops@example.com"],
                              acmeDomains = ["example.com", "www.example.com"],
                              acmeHttp01Port = 8080,
                              acmeCertificateDirectory = Just ".tls/example.com",
                              acmeCertbotConfig =
                                CertbotConfig
                                  { certbotExecutable = "certbot",
                                    certbotArguments = []
                                  }
                            }
                    },
                  ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5443,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                SharedCertificateFiles
                                  { certificateDirectory = ".tls/example.com",
                                    sharedCertificateStartupMode = AwaitCertificateFiles (Just 120)
                                  }
                            },
                      listenerAcme = Nothing
                    }
                ],
              requestPolicy =
                RequestPolicyConfig
                  { redirectHttpToHttps = True,
                    httpsRedirectPort = Just 5443,
                    strictTransportSecurity = Nothing,
                    corsPolicy = defaultCorsPolicyConfig,
                    responseSecurityHeaders = defaultResponseSecurityHeadersConfig
                  }
            }

    it "parses manual and ACME-backed HTTPS listeners distinctly" $ do
      let committedDefaults =
            [ ("APP_TITLE_PREFIX", "runtime-test"),
              ("LISTENER_BAD_HOST", "ignored-host"),
              ("LISTENER_0_HOST", "0.0.0.0"),
              ("LISTENER_0_PORT", "5443"),
              ("LISTENER_0_SCHEME", "https"),
              ("LISTENER_0_TLS_SOURCE", "manual"),
              ("LISTENER_0_TLS_CERTIFICATE_FILE", "cert.pem"),
              ("LISTENER_0_TLS_PRIVATE_KEY_FILE", "key.pem"),
              ("LISTENER_1_HOST", "0.0.0.0"),
              ("LISTENER_1_PORT", "5444"),
              ("LISTENER_1_SCHEME", "https"),
              ("LISTENER_1_TLS_SOURCE", "acme"),
              ("LISTENER_1_ACME_DIRECTORY_URL", "https://acme-staging-v02.api.letsencrypt.org/directory"),
              ("LISTENER_1_ACME_CONTACT_EMAILS", "ops@example.com,alerts@example.com"),
              ("LISTENER_1_ACME_DOMAINS", "example.com,www.example.com"),
              ("LISTENER_2_HOST", "0.0.0.0"),
              ("LISTENER_2_PORT", "5445"),
              ("LISTENER_2_SCHEME", "https"),
              ("LISTENER_2_TLS_SOURCE", "acme"),
              ("LISTENER_2_ACME_DIRECTORY_URL", "https://acme-v02.api.letsencrypt.org/directory"),
              ("LISTENER_2_ACME_CONTACT_EMAILS", "ops@example.com"),
              ("LISTENER_2_ACME_DOMAINS", "example.com"),
              ("LISTENER_2_ACME_CERTBOT_ARGUMENTS", "certonly,--webroot,--agree-tos")
            ]
      parseRuntimeAppConfig committedDefaults [] []
        `shouldBe` Right
          AppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = "0.0.0.0",
                      listenerPort = 5443,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                ManualCertificateFiles
                                  { certificateFile = "cert.pem",
                                    privateKeyFile = "key.pem"
                                  }
                            },
                      listenerAcme = Nothing
                    },
                  ListenerConfig
                    { listenerHost = "0.0.0.0",
                      listenerPort = 5444,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                AcmeCertificateSource
                                  AcmeConfig
                                    { acmeDirectoryUrl = "https://acme-staging-v02.api.letsencrypt.org/directory",
                                      acmeContactEmails = ["ops@example.com", "alerts@example.com"],
                                      acmeDomains = ["example.com", "www.example.com"],
                                      acmeHttp01Port = 80,
                                      acmeCertificateDirectory = Just ".tls/example.com",
                                      acmeCertbotConfig =
                                        CertbotConfig
                                          { certbotExecutable = "certbot",
                                            certbotArguments = []
                                          }
                                    }
                            },
                      listenerAcme = Nothing
                    },
                  ListenerConfig
                    { listenerHost = "0.0.0.0",
                      listenerPort = 5445,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                AcmeCertificateSource
                                  AcmeConfig
                                    { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                                      acmeContactEmails = ["ops@example.com"],
                                      acmeDomains = ["example.com"],
                                      acmeHttp01Port = 80,
                                      acmeCertificateDirectory = Just ".tls/example.com",
                                      acmeCertbotConfig =
                                        CertbotConfig
                                          { certbotExecutable = "certbot",
                                            certbotArguments = ["certonly", "--webroot", "--agree-tos"]
                                          }
                                    }
                            },
                      listenerAcme = Nothing
                    }
                ],
              staticAssets =
                StaticAssetsConfig
                  { staticAssetRoots = [],
                    staticAssetContentTypes = defaultStaticAssetContentTypes,
                    staticCacheControlSeconds = Nothing
                  },
              requestPolicy = requestPolicy defaultAppConfig,
              observability =
                ObservabilityConfig
                  { tracingExporter = Nothing,
                    metricsExporter = Nothing
                  }
            }

    it "rejects invalid listener scheme and TLS source values" $ do
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "0.0.0.0"),
          ("LISTENER_0_PORT", "5443"),
          ("LISTENER_0_SCHEME", "tcp")
        ]
        []
        []
        `shouldBe` Left (InvalidConfigValue "LISTENER_0_SCHEME" "tcp")
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "0.0.0.0"),
          ("LISTENER_0_PORT", "5443"),
          ("LISTENER_0_SCHEME", "https"),
          ("LISTENER_0_TLS_SOURCE", "vault")
        ]
        []
        []
        `shouldBe` Left (InvalidConfigValue "LISTENER_0_TLS_SOURCE" "vault")

    it "parses static asset roots and cache policy into the expected config" $ do
      let committedDefaults =
            [ ("APP_TITLE_PREFIX", "runtime-test"),
              ("LISTENER_0_HOST", "127.0.0.1"),
              ("LISTENER_0_PORT", "5001"),
              ("LISTENER_0_SCHEME", "http"),
              ("STATIC_ASSET_ROOT_2_DIRECTORY", "vendor/public"),
              ("STATIC_ASSET_ROOT_1_URL_PREFIX", "/assets"),
              ("STATIC_ASSET_ROOT_2_URL_PREFIX", "/vendor"),
              ("STATIC_ASSET_ROOT_1_DIRECTORY", "public"),
              ("STATIC_CACHE_CONTROL_SECONDS", "3600")
            ]
      parseRuntimeAppConfig committedDefaults [] []
        `shouldBe` Right
          AppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5001,
                      listenerScheme = Http,
                      listenerTls = Nothing,
                      listenerAcme = Nothing
                    }
                ],
              staticAssets =
                StaticAssetsConfig
                  { staticAssetRoots =
                      [ StaticAssetRoot
                          { staticUrlPrefix = "/assets",
                            staticDirectory = "public"
                          },
                        StaticAssetRoot
                          { staticUrlPrefix = "/vendor",
                            staticDirectory = "vendor/public"
                          }
                      ],
                    staticAssetContentTypes = defaultStaticAssetContentTypes,
                    staticCacheControlSeconds = Just 3600
                  },
              requestPolicy = requestPolicy defaultAppConfig,
              observability =
                ObservabilityConfig
                  { tracingExporter = Nothing,
                    metricsExporter = Nothing
                  }
            }

    it "parses numbered static asset content type entries including extensionless opt-in" $ do
      let committedDefaults =
            [ ("APP_TITLE_PREFIX", "runtime-test"),
              ("LISTENER_0_HOST", "127.0.0.1"),
              ("LISTENER_0_PORT", "5001"),
              ("LISTENER_0_SCHEME", "http"),
              ("STATIC_ASSET_CONTENT_TYPE_1_EXTENSION", ".wasm"),
              ("STATIC_ASSET_CONTENT_TYPE_1_MIME_TYPE", "application/wasm"),
              ("STATIC_ASSET_CONTENT_TYPE_2_EXTENSION", ""),
              ("STATIC_ASSET_CONTENT_TYPE_2_MIME_TYPE", "application/octet-stream")
            ]
      fmap (staticAssetContentTypes . staticAssets) (parseRuntimeAppConfig committedDefaults [] [])
        `shouldBe` Right
          [ (".wasm", "application/wasm"),
            ("", "application/octet-stream")
          ]

    it "parses redirect and HSTS request policy values for TLS-offload deployments" $
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [ ("REDIRECT_HTTP_TO_HTTPS", "true"),
          ("HSTS_MAX_AGE_SECONDS", "31536000"),
          ("HSTS_INCLUDE_SUBDOMAINS", "true"),
          ("HSTS_PRELOAD", "true")
        ]
        `shouldBe` Right
          defaultAppConfig
            { requestPolicy =
                RequestPolicyConfig
                  { redirectHttpToHttps = True,
                    httpsRedirectPort = Nothing,
                    strictTransportSecurity =
                      Just
                        StrictTransportSecurityConfig
                          { strictTransportSecurityMaxAgeSeconds = 31536000,
                            strictTransportSecurityIncludeSubDomains = True,
                            strictTransportSecurityPreload = True
                          },
                    corsPolicy = defaultCorsPolicyConfig,
                    responseSecurityHeaders = defaultResponseSecurityHeadersConfig
                  }
            }

    it "parses explicit false redirect and HSTS flags without changing the default policy shape" $
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [ ("REDIRECT_HTTP_TO_HTTPS", "false"),
          ("HSTS_MAX_AGE_SECONDS", "86400"),
          ("HSTS_INCLUDE_SUBDOMAINS", "false"),
          ("HSTS_PRELOAD", "false")
        ]
        `shouldBe` Right
          defaultAppConfig
            { requestPolicy =
                RequestPolicyConfig
                  { redirectHttpToHttps = False,
                    httpsRedirectPort = Nothing,
                    strictTransportSecurity =
                      Just
                        StrictTransportSecurityConfig
                          { strictTransportSecurityMaxAgeSeconds = 86400,
                            strictTransportSecurityIncludeSubDomains = False,
                            strictTransportSecurityPreload = False
                          },
                    corsPolicy = defaultCorsPolicyConfig,
                    responseSecurityHeaders = defaultResponseSecurityHeadersConfig
                  }
            }

    it "defaults optional HSTS booleans to false when only max-age is configured" $
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [("HSTS_MAX_AGE_SECONDS", "86400")]
        `shouldBe` Right
          defaultAppConfig
            { requestPolicy =
                RequestPolicyConfig
                  { redirectHttpToHttps = False,
                    httpsRedirectPort = Nothing,
                    strictTransportSecurity =
                      Just
                        StrictTransportSecurityConfig
                          { strictTransportSecurityMaxAgeSeconds = 86400,
                            strictTransportSecurityIncludeSubDomains = False,
                            strictTransportSecurityPreload = False
                          },
                    corsPolicy = defaultCorsPolicyConfig,
                    responseSecurityHeaders = defaultResponseSecurityHeadersConfig
                  }
            }

    it "parses CORS and response security policy overrides" $
      fmap
        requestPolicy
        ( parseRuntimeAppConfig
            committedRuntimeDefaults
            []
            [ ("CORS_ALLOWED_ORIGINS", "https://app.example.com, https://admin.example.com"),
              ("CORS_ALLOWED_METHODS", "GET, HEAD"),
              ("CORS_ALLOWED_HEADERS", "Content-Type, X-Requested-With"),
              ("CORS_MAX_AGE_SECONDS", "600"),
              ("CONTENT_SECURITY_POLICY", "default-src 'self'; connect-src 'self' https://collector.example.com"),
              ("X_CONTENT_TYPE_OPTIONS_NOSNIFF", "false"),
              ("X_XSS_PROTECTION", "0"),
              ("REFERRER_POLICY", "no-referrer"),
              ("PERMISSIONS_POLICY", "camera=()"),
              ("X_FRAME_OPTIONS", "SAMEORIGIN")
            ]
        )
        `shouldBe` Right
          ( (requestPolicy defaultAppConfig)
              { corsPolicy =
                  CorsPolicyConfig
                    { corsAllowedOrigins = ["https://app.example.com", "https://admin.example.com"],
                      corsAllowedMethods = ["GET", "HEAD"],
                      corsAllowedHeaders = ["Content-Type", "X-Requested-With"],
                      corsMaxAgeSeconds = Just 600
                    },
                responseSecurityHeaders =
                  ResponseSecurityHeadersConfig
                    { contentSecurityPolicy = Just "default-src 'self'; connect-src 'self' https://collector.example.com",
                      contentTypeOptionsNoSniff = False,
                      xssProtection = Just "0",
                      referrerPolicy = Just "no-referrer",
                      permissionsPolicy = Just "camera=()",
                      frameOptions = Just "SAMEORIGIN"
                    }
              }
          )

    it "lets REDIRECT_HTTP_TO_HTTPS=false disable the listener-aware default for dual listeners" $
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5001"),
          ("LISTENER_0_SCHEME", "http"),
          ("LISTENER_1_HOST", "127.0.0.1"),
          ("LISTENER_1_PORT", "5443"),
          ("LISTENER_1_SCHEME", "https"),
          ("LISTENER_1_TLS_SOURCE", "manual"),
          ("LISTENER_1_TLS_CERTIFICATE_FILE", "cert.pem"),
          ("LISTENER_1_TLS_PRIVATE_KEY_FILE", "key.pem")
        ]
        []
        [("REDIRECT_HTTP_TO_HTTPS", "false")]
        `shouldBe` Right
          defaultAppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5001,
                      listenerScheme = Http,
                      listenerTls = Nothing,
                      listenerAcme = Nothing
                    },
                  ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5443,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                ManualCertificateFiles
                                  { certificateFile = "cert.pem",
                                    privateKeyFile = "key.pem"
                                  }
                            },
                      listenerAcme = Nothing
                    }
                ],
              requestPolicy =
                RequestPolicyConfig
                  { redirectHttpToHttps = False,
                    httpsRedirectPort = Just 5443,
                    strictTransportSecurity = Nothing,
                    corsPolicy = defaultCorsPolicyConfig,
                    responseSecurityHeaders = defaultResponseSecurityHeadersConfig
                  }
            }

    it "keeps redirects on but leaves the redirect port implicit when multiple HTTPS ports exist" $
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5001"),
          ("LISTENER_0_SCHEME", "http"),
          ("LISTENER_1_HOST", "127.0.0.1"),
          ("LISTENER_1_PORT", "443"),
          ("LISTENER_1_SCHEME", "https"),
          ("LISTENER_1_TLS_SOURCE", "manual"),
          ("LISTENER_1_TLS_CERTIFICATE_FILE", "https-443-cert.pem"),
          ("LISTENER_1_TLS_PRIVATE_KEY_FILE", "https-443-key.pem"),
          ("LISTENER_2_HOST", "127.0.0.1"),
          ("LISTENER_2_PORT", "5443"),
          ("LISTENER_2_SCHEME", "https"),
          ("LISTENER_2_TLS_SOURCE", "manual"),
          ("LISTENER_2_TLS_CERTIFICATE_FILE", "https-5443-cert.pem"),
          ("LISTENER_2_TLS_PRIVATE_KEY_FILE", "https-5443-key.pem")
        ]
        []
        []
        `shouldBe` Right
          defaultAppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5001,
                      listenerScheme = Http,
                      listenerTls = Nothing,
                      listenerAcme = Nothing
                    },
                  ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 443,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                ManualCertificateFiles
                                  { certificateFile = "https-443-cert.pem",
                                    privateKeyFile = "https-443-key.pem"
                                  }
                            },
                      listenerAcme = Nothing
                    },
                  ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5443,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                ManualCertificateFiles
                                  { certificateFile = "https-5443-cert.pem",
                                    privateKeyFile = "https-5443-key.pem"
                                  }
                            },
                      listenerAcme = Nothing
                    }
                ],
              requestPolicy =
                RequestPolicyConfig
                  { redirectHttpToHttps = True,
                    httpsRedirectPort = Nothing,
                    strictTransportSecurity = Nothing,
                    corsPolicy = defaultCorsPolicyConfig,
                    responseSecurityHeaders = defaultResponseSecurityHeadersConfig
                  }
            }

    it "parses tracing and metrics exporters independently while preserving header order" $ do
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [ ("OTLP_TRACING_ENDPOINT", "http://collector:4318/v1/traces"),
          ("OTLP_TRACING_HEADERS", "authorization=Bearer token;x-api-key=secret")
        ]
        `shouldBe` Right
          defaultAppConfig
            { observability =
                ObservabilityConfig
                  { tracingExporter =
                      Just
                        OtlpExporter
                          { otlpEndpoint = "http://collector:4318/v1/traces",
                            otlpHeaders =
                              [ ("authorization", "Bearer token"),
                                ("x-api-key", "secret")
                              ]
                          },
                    metricsExporter = Nothing
                  }
            }
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [("OTLP_TRACING_ENDPOINT", "http://collector:4318/v1/traces")]
        `shouldBe` Right
          defaultAppConfig
            { observability =
                ObservabilityConfig
                  { tracingExporter =
                      Just
                        OtlpExporter
                          { otlpEndpoint = "http://collector:4318/v1/traces",
                            otlpHeaders = []
                          },
                    metricsExporter = Nothing
                  }
            }
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [ ("OTLP_TRACING_ENABLED", "true"),
          ("OTLP_TRACING_HEADERS", "authorization=Bearer token")
        ]
        `shouldBe` Right
          defaultAppConfig
            { observability =
                ObservabilityConfig
                  { tracingExporter =
                      Just
                        OtlpExporter
                          { otlpEndpoint = "http://127.0.0.1:4318/v1/traces",
                            otlpHeaders = [("authorization", "Bearer token")]
                          },
                    metricsExporter = Nothing
                  }
            }
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [("OTLP_TRACING_ENABLED", "true")]
        `shouldBe` Right
          defaultAppConfig
            { observability =
                ObservabilityConfig
                  { tracingExporter =
                      Just
                        OtlpExporter
                          { otlpEndpoint = "http://127.0.0.1:4318/v1/traces",
                            otlpHeaders = []
                          },
                    metricsExporter = Nothing
                  }
            }
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [ ("OTLP_TRACING_ENABLED", "false"),
          ("OTLP_TRACING_ENDPOINT", "http://collector:4318/v1/traces"),
          ("OTLP_TRACING_HEADERS", "authorization=Bearer token")
        ]
        `shouldBe` Right
          defaultAppConfig
            { observability =
                ObservabilityConfig
                  { tracingExporter = Nothing,
                    metricsExporter = Nothing
                  }
            }
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [ ("OTLP_METRICS_ENDPOINT", "http://collector:4318/v1/metrics"),
          ("OTLP_METRICS_HEADERS", "x-scope=metrics;broken-entry")
        ]
        `shouldBe` Right
          defaultAppConfig
            { observability =
                ObservabilityConfig
                  { tracingExporter = Nothing,
                    metricsExporter =
                      Just
                        OtlpExporter
                          { otlpEndpoint = "http://collector:4318/v1/metrics",
                            otlpHeaders = [("x-scope", "metrics")]
                          }
                  }
            }

    it "fails invalid runtime values with explicit errors" $ do
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "0"),
          ("LISTENER_0_SCHEME", "http")
        ]
        []
        []
        `shouldBe` Left (InvalidConfigValue "LISTENER_0_PORT" "0")
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5001"),
          ("LISTENER_0_SCHEME", "https"),
          ("LISTENER_0_TLS_SOURCE", "acme"),
          ("LISTENER_0_ACME_DIRECTORY_URL", "https://acme-v02.api.letsencrypt.org/directory"),
          ("LISTENER_0_ACME_CONTACT_EMAILS", ""),
          ("LISTENER_0_ACME_DOMAINS", "")
        ]
        []
        []
        `shouldBe` Left (InvalidConfigValue "LISTENER_0_ACME_CONTACT_EMAILS" "")
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5001"),
          ("LISTENER_0_SCHEME", "https"),
          ("LISTENER_0_TLS_SOURCE", "acme"),
          ("LISTENER_0_ACME_DIRECTORY_URL", "https://acme-v02.api.letsencrypt.org/directory"),
          ("LISTENER_0_ACME_CONTACT_EMAILS", "ops@example.com"),
          ("LISTENER_0_ACME_DOMAINS", "")
        ]
        []
        []
        `shouldBe` Left (InvalidConfigValue "LISTENER_0_ACME_DOMAINS" "")
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5001"),
          ("LISTENER_0_SCHEME", "https"),
          ("LISTENER_0_TLS_SOURCE", "acme"),
          ("LISTENER_0_ACME_DIRECTORY_URL", "https://acme-v02.api.letsencrypt.org/directory"),
          ("LISTENER_0_ACME_CONTACT_EMAILS", "ops@example.com"),
          ("LISTENER_0_ACME_CHALLENGE_BACKEND", "shell-script")
        ]
        []
        []
        `shouldBe` Left (InvalidConfigValue "LISTENER_0_ACME_CHALLENGE_BACKEND" "shell-script")
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5001"),
          ("LISTENER_0_SCHEME", "https"),
          ("LISTENER_0_TLS_SOURCE", "acme"),
          ("LISTENER_0_ACME_CONTACT_EMAILS", "ops@example.com"),
          ("LISTENER_0_ACME_DOMAINS", "example.com,www.example.com"),
          ("LISTENER_0_ACME_CERTBOT_EXECUTABLE", "certbot")
        ]
        []
        []
        `shouldBe` Right
          AppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5001,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                AcmeCertificateSource
                                  AcmeConfig
                                    { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                                      acmeContactEmails = ["ops@example.com"],
                                      acmeDomains = ["example.com", "www.example.com"],
                                      acmeHttp01Port = 80,
                                      acmeCertificateDirectory = Just ".tls/example.com",
                                      acmeCertbotConfig =
                                        CertbotConfig
                                          { certbotExecutable = "certbot",
                                            certbotArguments = []
                                          }
                                    }
                            },
                      listenerAcme = Nothing
                    }
                ],
              staticAssets =
                StaticAssetsConfig
                  { staticAssetRoots = [],
                    staticAssetContentTypes = defaultStaticAssetContentTypes,
                    staticCacheControlSeconds = Nothing
                  },
              requestPolicy = requestPolicy defaultAppConfig,
              observability =
                ObservabilityConfig
                  { tracingExporter = Nothing,
                    metricsExporter = Nothing
                  }
            }
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5001"),
          ("LISTENER_0_SCHEME", "https"),
          ("LISTENER_0_TLS_SOURCE", "acme"),
          ("LISTENER_0_ACME_CONTACT_EMAILS", "ops@example.com"),
          ("LISTENER_0_ACME_DOMAINS", "example.com,www.example.com")
        ]
        []
        []
        `shouldBe` Right
          AppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5001,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                AcmeCertificateSource
                                  AcmeConfig
                                    { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                                      acmeContactEmails = ["ops@example.com"],
                                      acmeDomains = ["example.com", "www.example.com"],
                                      acmeHttp01Port = 80,
                                      acmeCertificateDirectory = Just ".tls/example.com",
                                      acmeCertbotConfig =
                                        CertbotConfig
                                          { certbotExecutable = "certbot",
                                            certbotArguments = []
                                          }
                                    }
                            },
                      listenerAcme = Nothing
                    }
                ],
              staticAssets =
                StaticAssetsConfig
                  { staticAssetRoots = [],
                    staticAssetContentTypes = defaultStaticAssetContentTypes,
                    staticCacheControlSeconds = Nothing
                  },
              requestPolicy = requestPolicy defaultAppConfig,
              observability =
                ObservabilityConfig
                  { tracingExporter = Nothing,
                    metricsExporter = Nothing
                  }
            }
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5001"),
          ("LISTENER_0_SCHEME", "https"),
          ("LISTENER_0_TLS_SOURCE", "acme"),
          ("LISTENER_0_ACME_CONTACT_EMAILS", "ops@example.com")
        ]
        []
        []
        `shouldBe` Right
          AppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5001,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                AcmeCertificateSource
                                  AcmeConfig
                                    { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                                      acmeContactEmails = ["ops@example.com"],
                                      acmeDomains = [],
                                      acmeHttp01Port = 80,
                                      acmeCertificateDirectory = Just ".tls/listener-0",
                                      acmeCertbotConfig =
                                        CertbotConfig
                                          { certbotExecutable = "certbot",
                                            certbotArguments = []
                                          }
                                    }
                            },
                      listenerAcme = Nothing
                    }
                ],
              staticAssets =
                StaticAssetsConfig
                  { staticAssetRoots = [],
                    staticAssetContentTypes = defaultStaticAssetContentTypes,
                    staticCacheControlSeconds = Nothing
                  },
              requestPolicy = requestPolicy defaultAppConfig,
              observability =
                ObservabilityConfig
                  { tracingExporter = Nothing,
                    metricsExporter = Nothing
                  }
            }
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5001"),
          ("LISTENER_0_SCHEME", "http"),
          ("STATIC_CACHE_CONTROL_SECONDS", "-1")
        ]
        []
        []
        `shouldBe` Left (InvalidConfigValue "STATIC_CACHE_CONTROL_SECONDS" "-1")
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [ ("STATIC_ASSET_CONTENT_TYPE_1_EXTENSION", "wasm"),
          ("STATIC_ASSET_CONTENT_TYPE_1_MIME_TYPE", "application/wasm")
        ]
        `shouldBe` Left (InvalidConfigValue "STATIC_ASSET_CONTENT_TYPE_1_EXTENSION" "wasm")
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [ ("STATIC_ASSET_CONTENT_TYPE_1_EXTENSION", ".wasm"),
          ("STATIC_ASSET_CONTENT_TYPE_1_MIME_TYPE", "")
        ]
        `shouldBe` Left (InvalidConfigValue "STATIC_ASSET_CONTENT_TYPE_1_MIME_TYPE" "")
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [("OTLP_TRACING_HEADERS", "authorization=Bearer token")]
        `shouldBe` Left (MissingConfigValue "OTLP_TRACING_ENDPOINT")
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [("OTLP_TRACING_ENABLED", "maybe")]
        `shouldBe` Left (InvalidConfigValue "OTLP_TRACING_ENABLED" "maybe")
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [("REDIRECT_HTTP_TO_HTTPS", "maybe")]
        `shouldBe` Left (InvalidConfigValue "REDIRECT_HTTP_TO_HTTPS" "maybe")
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [("HSTS_INCLUDE_SUBDOMAINS", "true")]
        `shouldBe` Left (MissingConfigValue "HSTS_MAX_AGE_SECONDS")
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [ ("HSTS_MAX_AGE_SECONDS", "31536000"),
          ("HSTS_PRELOAD", "sometimes")
        ]
        `shouldBe` Left (InvalidConfigValue "HSTS_PRELOAD" "sometimes")
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [("HSTS_MAX_AGE_SECONDS", "-1")]
        `shouldBe` Left (InvalidConfigValue "HSTS_MAX_AGE_SECONDS" "-1")
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [("CORS_ALLOWED_ORIGINS", " , ")]
        `shouldBe` Left (InvalidConfigValue "CORS_ALLOWED_ORIGINS" " , ")
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [("CORS_MAX_AGE_SECONDS", "-1")]
        `shouldBe` Left (InvalidConfigValue "CORS_MAX_AGE_SECONDS" "-1")
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [("CONTENT_SECURITY_POLICY", "")]
        `shouldBe` Left (InvalidConfigValue "CONTENT_SECURITY_POLICY" "")
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [("X_CONTENT_TYPE_OPTIONS_NOSNIFF", "maybe")]
        `shouldBe` Left (InvalidConfigValue "X_CONTENT_TYPE_OPTIONS_NOSNIFF" "maybe")

  describe "defaultAppEnvironmentConfig" $ do
    it "keeps committed .env defaults aligned with the parsed development config" $ do
      committedEnvDefaults
        `shouldBe` [ ("APP_MODE", "development"),
                     ("DATABASE_HOST", "127.0.0.1"),
                     ("DATABASE_PORT", "5432"),
                     ("DATABASE_NAME", "web_api_dev"),
                     ("DATABASE_USER", "web_api_runtime"),
                     ("DATABASE_PASSWORD", "web_api")
                   ]
      defaultAppEnvironmentConfig
        `shouldBe` AppEnvironmentConfig
          { appMode = Development,
            databaseConfig =
              DatabaseConfig
                { databaseHost = "127.0.0.1",
                  databasePort = 5432,
                  databaseName = "web_api_dev",
                  databaseUser = "web_api_runtime",
                  databasePassword = "web_api"
                }
          }

    it "covers the new app/database config selectors and derived instances" $ do
      let productionDatabaseConfig =
            DatabaseConfig
              { databaseHost = "db.internal",
                databasePort = 6543,
                databaseName = "web_api_prod",
                databaseUser = "web_api_app",
                databasePassword = "super-secret"
              }
          productionEnvironmentConfig =
            AppEnvironmentConfig
              { appMode = Production,
                databaseConfig = productionDatabaseConfig
              }
      appMode productionEnvironmentConfig `shouldBe` Production
      databaseConfig productionEnvironmentConfig `shouldBe` productionDatabaseConfig
      databaseHost productionDatabaseConfig `shouldBe` "db.internal"
      databasePort productionDatabaseConfig `shouldBe` 6543
      databaseName productionDatabaseConfig `shouldBe` "web_api_prod"
      databaseUser productionDatabaseConfig `shouldBe` "web_api_app"
      databasePassword productionDatabaseConfig `shouldBe` "super-secret"
      Development `shouldNotBe` Test
      Test `shouldNotBe` Production
      productionDatabaseConfig `shouldBe` productionDatabaseConfig
      productionDatabaseConfig
        `shouldNotBe` productionDatabaseConfig
          { databasePassword = "different-secret"
          }
      productionEnvironmentConfig `shouldBe` productionEnvironmentConfig
      productionEnvironmentConfig
        `shouldNotBe` productionEnvironmentConfig
          { appMode = Test
          }
      MissingConfigValue "DATABASE_PASSWORD"
        `shouldNotBe` InvalidConfigValue "DATABASE_PASSWORD" "missing"
      show Development `shouldBe` "Development"
      show Test `shouldBe` "Test"
      show Production `shouldBe` "Production"
      show [Development, Test, Production] `shouldBe` "[Development,Test,Production]"
      show productionDatabaseConfig
        `shouldBe` "DatabaseConfig {databaseHost = \"db.internal\", databasePort = 6543, databaseName = \"web_api_prod\", databaseUser = \"web_api_app\", databasePassword = \"super-secret\"}"
      show [productionDatabaseConfig]
        `shouldBe` "[DatabaseConfig {databaseHost = \"db.internal\", databasePort = 6543, databaseName = \"web_api_prod\", databaseUser = \"web_api_app\", databasePassword = \"super-secret\"}]"
      show productionEnvironmentConfig
        `shouldBe` "AppEnvironmentConfig {appMode = Production, databaseConfig = DatabaseConfig {databaseHost = \"db.internal\", databasePort = 6543, databaseName = \"web_api_prod\", databaseUser = \"web_api_app\", databasePassword = \"super-secret\"}}"
      show [productionEnvironmentConfig]
        `shouldBe` "[AppEnvironmentConfig {appMode = Production, databaseConfig = DatabaseConfig {databaseHost = \"db.internal\", databasePort = 6543, databaseName = \"web_api_prod\", databaseUser = \"web_api_app\", databasePassword = \"super-secret\"}}]"
      show (MissingConfigValue "DATABASE_PASSWORD") `shouldBe` "MissingConfigValue \"DATABASE_PASSWORD\""
      show (InvalidConfigValue "APP_MODE" "staging") `shouldBe` "InvalidConfigValue \"APP_MODE\" \"staging\""
      show [MissingConfigValue "DATABASE_PASSWORD", InvalidConfigValue "APP_MODE" "staging"]
        `shouldBe` "[MissingConfigValue \"DATABASE_PASSWORD\",InvalidConfigValue \"APP_MODE\" \"staging\"]"

  describe "defaultDatabaseSeed" $ do
    it "defines deterministic page-facing seeded results for both locales" $
      defaultDatabaseSeed
        `shouldBe` DatabaseSeed
          { englishHomePageData =
              Right
                HomePageData
                  { homePageDataSummary = "Server-rendered home page with stubbed content."
                  },
            frenchHomePageData =
              Right
                HomePageData
                  { homePageDataSummary = "Accueil cote serveur avec des donnees de developpement preconfigurees."
                  },
            englishSecondPageData =
              Right
                SecondPageData
                  { secondPageDataSummary = "Second page content with stubbed data ready for future loaders.",
                    secondPageDataHighlights = []
                  },
            frenchSecondPageData =
              Right
                SecondPageData
                  { secondPageDataSummary = "Second page content with stubbed data ready for future loaders.",
                    secondPageDataHighlights = []
                  }
          }

    it "keeps seeded database data serializable and stable for tests" $ do
      let homePageData = HomePageData {homePageDataSummary = "Seeded home"}
          otherHomePageData = HomePageData {homePageDataSummary = "Different home"}
          secondPageData =
            SecondPageData
              { secondPageDataSummary = "Seeded second",
                secondPageDataHighlights = ["One"]
              }
          otherSecondPageData =
            SecondPageData
              { secondPageDataSummary = "Other second",
                secondPageDataHighlights = []
              }
          homeError = HomePageDataError "home unavailable"
          secondError = SecondPageDataError "second unavailable"
          databaseOperation =
            DatabaseOperation
              { databaseOperationName = "load-second-page-summary",
                databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;"
              }
          databaseResult =
            DatabaseResult
              { databaseResultValue = Right homePageData,
                databaseResultOperations = [databaseOperation]
              }
          seededDatabase =
            DatabaseSeed
              { englishHomePageData = Right homePageData,
                frenchHomePageData = Left homeError,
                englishSecondPageData = Right secondPageData,
                frenchSecondPageData = Left secondError
              }
      homePageData `shouldBe` homePageData
      homePageData `shouldNotBe` otherHomePageData
      secondPageData `shouldBe` secondPageData
      secondPageData `shouldNotBe` otherSecondPageData
      homeError `shouldBe` homeError
      homeError `shouldNotBe` secondError
      databaseOperation `shouldBe` databaseOperation
      databaseOperation `shouldNotBe` databaseOperation {databaseOperationName = "load-home-page-summary"}
      databaseResult `shouldBe` databaseResult
      databaseResult
        `shouldNotBe` databaseResult
          { databaseResultOperations = []
          }
      seededDatabase `shouldBe` seededDatabase
      seededDatabase
        `shouldNotBe` seededDatabase
          { frenchSecondPageData = Right otherSecondPageData
          }
      show (HomePageData {homePageDataSummary = "Seeded home"})
        `shouldBe` "HomePageData {homePageDataSummary = \"Seeded home\"}"
      show (SecondPageData {secondPageDataSummary = "Seeded second", secondPageDataHighlights = ["One"]})
        `shouldBe` "SecondPageData {secondPageDataSummary = \"Seeded second\", secondPageDataHighlights = [\"One\"]}"
      show (HomePageDataError "home unavailable")
        `shouldBe` "HomePageDataError \"home unavailable\""
      show (SecondPageDataError "second unavailable")
        `shouldBe` "SecondPageDataError \"second unavailable\""
      show databaseOperation
        `shouldBe` "DatabaseOperation {databaseOperationName = \"load-second-page-summary\", databaseQueryTemplate = \"SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;\"}"
      show databaseResult
        `shouldBe` "DatabaseResult {databaseResultValue = Right (HomePageData {homePageDataSummary = \"Seeded home\"}), databaseResultOperations = [DatabaseOperation {databaseOperationName = \"load-second-page-summary\", databaseQueryTemplate = \"SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;\"}]}"
      show seededDatabase
        `shouldBe` "DatabaseSeed {englishHomePageData = Right (HomePageData {homePageDataSummary = \"Seeded home\"}), frenchHomePageData = Left (HomePageDataError \"home unavailable\"), englishSecondPageData = Right (SecondPageData {secondPageDataSummary = \"Seeded second\", secondPageDataHighlights = [\"One\"]}), frenchSecondPageData = Left (SecondPageDataError \"second unavailable\")}"
      show [HomePageData {homePageDataSummary = "Seeded home"}]
        `shouldBe` "[HomePageData {homePageDataSummary = \"Seeded home\"}]"
      show [homeError, secondError]
        `shouldBe` "[HomePageDataError \"home unavailable\",SecondPageDataError \"second unavailable\"]"
      show [databaseOperation]
        `shouldBe` "[DatabaseOperation {databaseOperationName = \"load-second-page-summary\", databaseQueryTemplate = \"SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;\"}]"
      show [databaseResult]
        `shouldBe` "[DatabaseResult {databaseResultValue = Right (HomePageData {homePageDataSummary = \"Seeded home\"}), databaseResultOperations = [DatabaseOperation {databaseOperationName = \"load-second-page-summary\", databaseQueryTemplate = \"SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;\"}]}]"
      show
        [ SecondPageData
            { secondPageDataSummary = "Seeded second",
              secondPageDataHighlights = ["One"]
            }
        ]
        `shouldBe` "[SecondPageData {secondPageDataSummary = \"Seeded second\", secondPageDataHighlights = [\"One\"]}]"
      show [seededDatabase]
        `shouldBe` "[DatabaseSeed {englishHomePageData = Right (HomePageData {homePageDataSummary = \"Seeded home\"}), frenchHomePageData = Left (HomePageDataError \"home unavailable\"), englishSecondPageData = Right (SecondPageData {secondPageDataSummary = \"Seeded second\", secondPageDataHighlights = [\"One\"]}), frenchSecondPageData = Left (SecondPageDataError \"second unavailable\")}]"

  describe "buildSeededDatabaseEffect" $ do
    it "loads page-oriented seeded data for both English and French requests" $ do
      let englishEffect = buildSeededDatabaseEffect defaultDatabaseSeed
      loadHomePageDataWithObservability englishEffect defaultRequestContext
        `shouldReturn` DatabaseResult
          { databaseResultValue =
              Right
                HomePageData
                  { homePageDataSummary = "Server-rendered home page with stubbed content."
                  },
            databaseResultOperations = []
          }
      loadHomePageData englishEffect defaultRequestContext
        `shouldReturn` Right
          HomePageData
            { homePageDataSummary = "Server-rendered home page with stubbed content."
            }
      loadSecondPageData englishEffect defaultRequestContext
        `shouldReturn` Right
          SecondPageData
            { secondPageDataSummary = "Second page content with stubbed data ready for future loaders.",
              secondPageDataHighlights = []
            }
      loadHomePageData englishEffect frenchRequestContext
        `shouldReturn` Right
          HomePageData
            { homePageDataSummary = "Accueil cote serveur avec des donnees de developpement preconfigurees."
            }
      loadSecondPageData englishEffect frenchRequestContext
        `shouldReturn` Right
          SecondPageData
            { secondPageDataSummary = "Second page content with stubbed data ready for future loaders.",
              secondPageDataHighlights = []
            }
      loadSecondPageDataWithObservability englishEffect frenchRequestContext
        `shouldReturn` DatabaseResult
          { databaseResultValue =
              Right
                SecondPageData
                  { secondPageDataSummary = "Second page content with stubbed data ready for future loaders.",
                    secondPageDataHighlights = []
                  },
            databaseResultOperations = []
          }

    it "returns explicit seeded errors without collapsing page-specific failures" $ do
      let seededEffect =
            buildSeededDatabaseEffect
              DatabaseSeed
                { englishHomePageData = Left (HomePageDataError "home seed unavailable"),
                  frenchHomePageData =
                    Right
                      HomePageData
                        { homePageDataSummary = "Accueil seede"
                        },
                  englishSecondPageData =
                    Right
                      SecondPageData
                        { secondPageDataSummary = "Second seed",
                          secondPageDataHighlights = ["Known branch"]
                        },
                  frenchSecondPageData = Left (SecondPageDataError "second seed unavailable")
                }
      loadHomePageData seededEffect defaultRequestContext
        `shouldReturn` Left (HomePageDataError "home seed unavailable")
      loadSecondPageData seededEffect frenchRequestContext
        `shouldReturn` Left (SecondPageDataError "second seed unavailable")
      loadSecondPageDataWithObservability seededEffect frenchRequestContext
        `shouldReturn` DatabaseResult
          { databaseResultValue = Left (SecondPageDataError "second seed unavailable"),
            databaseResultOperations = []
          }

    it "keeps the default seeded interpreter deterministic for repeated requests" $ do
      firstHome <- loadHomePageData defaultDatabaseEffect defaultRequestContext
      secondHome <- loadHomePageData defaultDatabaseEffect defaultRequestContext
      firstHome `shouldBe` secondHome
      firstSecond <- loadSecondPageData defaultDatabaseEffect frenchRequestContext
      secondSecond <- loadSecondPageData defaultDatabaseEffect frenchRequestContext
      firstSecond `shouldBe` secondSecond

  describe "selectRouteData" $ do
    it "selects the same second-route domain data for page and API surfaces" $ do
      let seededDatabaseEffect =
            buildSeededDatabaseEffect
              DatabaseSeed
                { englishHomePageData = englishHomePageData defaultDatabaseSeed,
                  frenchHomePageData = frenchHomePageData defaultDatabaseSeed,
                  englishSecondPageData =
                    Right
                      SecondPageData
                        { secondPageDataSummary = "Shared domain summary.",
                          secondPageDataHighlights = ["Shared loader", "Shared renderer"]
                        },
                  frenchSecondPageData = frenchSecondPageData defaultDatabaseSeed
                }
      selectedRouteData <- selectRouteDataWithDatabase seededDatabaseEffect secondRequest
      selectedRouteData
        `shouldBe` SecondRouteDataResult
          ( Right
              SecondRouteData
                { secondRouteSummary = "Shared domain summary.",
                  secondRouteHighlights = ["Shared loader", "Shared renderer"]
                }
          )
      selectRouteDataWithDatabase seededDatabaseEffect apiSecondRequest `shouldReturn` selectedRouteData

    it "keeps route-data selections deterministic while exposing database operations separately" $ do
      let databaseOperation =
            DatabaseOperation
              { databaseOperationName = "load-second-page-summary",
                databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;"
              }
          routeDataSelection =
            RouteDataSelection
              { routeDataResult = SecondRouteDataResult (Right (SecondRouteData {secondRouteSummary = "Shared domain summary", secondRouteHighlights = []})),
                routeDataDatabaseOperations = [databaseOperation]
              }
      routeDataSelection `shouldBe` routeDataSelection
      routeDataSelection
        `shouldNotBe` routeDataSelection
          { routeDataDatabaseOperations = []
          }
      show routeDataSelection
        `shouldBe` "RouteDataSelection {routeDataResult = SecondRouteDataResult (Right (SecondRouteData {secondRouteSummary = \"Shared domain summary\", secondRouteHighlights = []})), routeDataDatabaseOperations = [DatabaseOperation {databaseOperationName = \"load-second-page-summary\", databaseQueryTemplate = \"SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;\"}]}"
      show [routeDataSelection]
        `shouldBe` "[RouteDataSelection {routeDataResult = SecondRouteDataResult (Right (SecondRouteData {secondRouteSummary = \"Shared domain summary\", secondRouteHighlights = []})), routeDataDatabaseOperations = [DatabaseOperation {databaseOperationName = \"load-second-page-summary\", databaseQueryTemplate = \"SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;\"}]}]"
      selectRouteDataSelectionWithDatabase (buildSeededDatabaseEffect defaultDatabaseSeed) secondRequest
        `shouldReturn` RouteDataSelection
          { routeDataResult =
              SecondRouteDataResult
                ( Right
                    SecondRouteData
                      { secondRouteSummary = "Second page content with stubbed data ready for future loaders.",
                        secondRouteHighlights = []
                      }
                ),
            routeDataDatabaseOperations = []
          }

    it "loads home-route data from the database effect and preserves explicit failures" $ do
      let seededDatabaseEffect =
            buildSeededDatabaseEffect
              DatabaseSeed
                { englishHomePageData =
                    Right
                      HomePageData
                        { homePageDataSummary = "Loaded from the seeded database effect."
                        },
                  frenchHomePageData = Left (HomePageDataError "home seed unavailable"),
                  englishSecondPageData = englishSecondPageData defaultDatabaseSeed,
                  frenchSecondPageData = frenchSecondPageData defaultDatabaseSeed
                }
      selectRouteDataWithDatabase seededDatabaseEffect homeRequest
        `shouldReturn` HomeRouteDataResult
          ( Right
              HomeRouteData
                { homeRouteSummary = "Loaded from the seeded database effect."
                }
          )
      selectRouteDataWithDatabase seededDatabaseEffect frenchHomeRequest
        `shouldReturn` HomeRouteDataResult
          (Left (HomePageDataError "home seed unavailable"))

    it "keeps route-data selectors and derived instances deterministic for tests" $ do
      let homeRouteData =
            HomeRouteData
              { homeRouteSummary = "Stubbed home summary"
              }
          otherHomeRouteData =
            HomeRouteData
              { homeRouteSummary = "Different home summary"
              }
          secondRouteData =
            SecondRouteData
              { secondRouteSummary = "Shared domain summary",
                secondRouteHighlights = ["Shared loader"]
              }
          statusApiData =
            StatusApiData
              { statusApiLocale = French
              }
          routeDataResult = HomeRouteDataResult (Right homeRouteData)
      homeRouteSummary homeRouteData `shouldBe` "Stubbed home summary"
      secondRouteSummary secondRouteData `shouldBe` "Shared domain summary"
      secondRouteHighlights secondRouteData `shouldBe` ["Shared loader"]
      statusApiLocale statusApiData `shouldBe` French
      homeRouteData `shouldBe` homeRouteData
      homeRouteData `shouldNotBe` otherHomeRouteData
      secondRouteData `shouldNotBe` secondRouteData {secondRouteHighlights = []}
      statusApiData `shouldBe` statusApiData
      statusApiData `shouldNotBe` StatusApiData {statusApiLocale = English}
      routeDataResult `shouldBe` routeDataResult
      routeDataResult `shouldNotBe` NotFoundRouteDataResult
      show homeRouteData `shouldBe` "HomeRouteData {homeRouteSummary = \"Stubbed home summary\"}"
      show secondRouteData
        `shouldBe` "SecondRouteData {secondRouteSummary = \"Shared domain summary\", secondRouteHighlights = [\"Shared loader\"]}"
      show statusApiData `shouldBe` "StatusApiData {statusApiLocale = French}"
      show routeDataResult
        `shouldBe` "HomeRouteDataResult (Right (HomeRouteData {homeRouteSummary = \"Stubbed home summary\"}))"
      show (SecondRouteDataResult (Right secondRouteData))
        `shouldBe` "SecondRouteDataResult (Right (SecondRouteData {secondRouteSummary = \"Shared domain summary\", secondRouteHighlights = [\"Shared loader\"]}))"
      show (StatusApiDataResult statusApiData)
        `shouldBe` "StatusApiDataResult (StatusApiData {statusApiLocale = French})"
      show [homeRouteData] `shouldBe` "[HomeRouteData {homeRouteSummary = \"Stubbed home summary\"}]"
      show [secondRouteData]
        `shouldBe` "[SecondRouteData {secondRouteSummary = \"Shared domain summary\", secondRouteHighlights = [\"Shared loader\"]}]"
      show [statusApiData] `shouldBe` "[StatusApiData {statusApiLocale = French}]"
      show [NotFoundRouteDataResult] `shouldBe` "[NotFoundRouteDataResult]"

    it "selects default stubbed and status route data without extra wiring" $ do
      selectRouteData homeRequest
        `shouldReturn` HomeRouteDataResult
          ( Right
              HomeRouteData
                { homeRouteSummary = "Server-rendered home page with stubbed content."
                }
          )
      selectRouteData secondRequest
        `shouldReturn` SecondRouteDataResult
          ( Right
              SecondRouteData
                { secondRouteSummary = "Second page content with stubbed data ready for future loaders.",
                  secondRouteHighlights = []
                }
          )
      selectRouteData frenchApiStatusRequest
        `shouldReturn` StatusApiDataResult
          StatusApiData
            { statusApiLocale = French
            }
      selectRouteData apiNotFoundRequest `shouldReturn` NotFoundRouteDataResult

  describe "WebApi.Postgres" $ do
    it "translates database config into psql commands for page queries" $ do
      recordedCommandsReference <- newIORef []
      let runner command = do
            modifyIORef' recordedCommandsReference (<> [command])
            pure $
              case commandSql command of
                sql
                  | Text.isInfixOf "route_slug = 'home'" sql ->
                      successfulPostgresResult $
                        if Text.isInfixOf "locale = 'fr'" sql
                          then "Accueil cote serveur avec des donnees de developpement preconfigurees."
                          else "Server-rendered home page with stubbed content."
                  | Text.isInfixOf "SELECT summary FROM web_api.page_content WHERE route_slug = 'second'" sql ->
                      successfulPostgresResult $
                        if Text.isInfixOf "locale = 'fr'" sql
                          then "Charge depuis PostgreSQL."
                          else "Loaded from PostgreSQL."
                  | Text.isInfixOf "SELECT highlight FROM web_api.page_highlights" sql ->
                      successfulPostgresResult $
                        if Text.isInfixOf "locale = 'fr'" sql
                          then "SSR rapide\nDonnees partagees"
                          else "Fast SSR\nShared route data"
                  | otherwise ->
                      failingPostgresResult "unexpected query"
          postgresEffect = buildPostgresDatabaseEffectWithRunner runner postgresTestConfig
      loadHomePageDataWithObservability postgresEffect defaultRequestContext
        `shouldReturn` DatabaseResult
          { databaseResultValue =
              Right
                HomePageData
                  { homePageDataSummary = "Server-rendered home page with stubbed content."
                  },
            databaseResultOperations =
              [ DatabaseOperation
                  { databaseOperationName = "load-home-page-summary",
                    databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;"
                  }
              ]
          }
      loadHomePageData postgresEffect defaultRequestContext
        `shouldReturn` Right
          HomePageData
            { homePageDataSummary = "Server-rendered home page with stubbed content."
            }
      loadSecondPageData postgresEffect defaultRequestContext
        `shouldReturn` Right
          SecondPageData
            { secondPageDataSummary = "Loaded from PostgreSQL.",
              secondPageDataHighlights = ["Fast SSR", "Shared route data"]
            }
      loadHomePageData postgresEffect frenchRequestContext
        `shouldReturn` Right
          HomePageData
            { homePageDataSummary = "Accueil cote serveur avec des donnees de developpement preconfigurees."
            }
      loadSecondPageData postgresEffect frenchRequestContext
        `shouldReturn` Right
          SecondPageData
            { secondPageDataSummary = "Charge depuis PostgreSQL.",
              secondPageDataHighlights = ["SSR rapide", "Donnees partagees"]
            }
      loadSecondPageDataWithObservability postgresEffect frenchRequestContext
        `shouldReturn` DatabaseResult
          { databaseResultValue =
              Right
                SecondPageData
                  { secondPageDataSummary = "Charge depuis PostgreSQL.",
                    secondPageDataHighlights = ["SSR rapide", "Donnees partagees"]
                  },
            databaseResultOperations =
              [ DatabaseOperation
                  { databaseOperationName = "load-second-page-summary",
                    databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;"
                  },
                DatabaseOperation
                  { databaseOperationName = "load-second-page-highlights",
                    databaseQueryTemplate = "SELECT highlight FROM web_api.page_highlights WHERE route_slug = ? AND locale = ? ORDER BY position ASC;"
                  }
              ]
          }
      recordedCommands <- readIORef recordedCommandsReference
      let expectedQueryCommand sql =
            PostgresCommand
              { postgresExecutable = "psql",
                postgresArguments =
                  [ "--host",
                    "db.internal",
                    "--port",
                    "6543",
                    "--dbname",
                    "web_api_prod",
                    "--username",
                    "web_api_app",
                    "--no-password",
                    "--set",
                    "ON_ERROR_STOP=1",
                    "--tuples-only",
                    "--no-align",
                    "--quiet",
                    "--command",
                    Text.unpack sql
                  ],
                postgresEnvironment = [("PGPASSWORD", "super-secret")]
              }
      recordedCommands
        `shouldBe` map
          expectedQueryCommand
          [ "SELECT summary FROM web_api.page_content WHERE route_slug = 'home' AND locale = 'en';",
            "SELECT summary FROM web_api.page_content WHERE route_slug = 'home' AND locale = 'en';",
            "SELECT summary FROM web_api.page_content WHERE route_slug = 'second' AND locale = 'en';",
            "SELECT highlight FROM web_api.page_highlights WHERE route_slug = 'second' AND locale = 'en' ORDER BY position ASC;",
            "SELECT summary FROM web_api.page_content WHERE route_slug = 'home' AND locale = 'fr';",
            "SELECT summary FROM web_api.page_content WHERE route_slug = 'second' AND locale = 'fr';",
            "SELECT highlight FROM web_api.page_highlights WHERE route_slug = 'second' AND locale = 'fr' ORDER BY position ASC;",
            "SELECT summary FROM web_api.page_content WHERE route_slug = 'second' AND locale = 'fr';",
            "SELECT highlight FROM web_api.page_highlights WHERE route_slug = 'second' AND locale = 'fr' ORDER BY position ASC;"
          ]

    it "maps missing rows and command failures into database errors" $ do
      let missingRunner command =
            pure $
              case commandSql command of
                sql
                  | Text.isInfixOf "route_slug = 'home'" sql ->
                      successfulPostgresResult Text.empty
                  | otherwise ->
                      failingPostgresResult "relation does not exist"
          postgresEffect = buildPostgresDatabaseEffectWithRunner missingRunner postgresTestConfig
      loadHomePageData postgresEffect defaultRequestContext
        `shouldReturn` Left (HomePageDataError "expected exactly one row: ")
      loadSecondPageData postgresEffect defaultRequestContext
        `shouldReturn` Left (SecondPageDataError "relation does not exist")
      loadSecondPageDataWithObservability postgresEffect defaultRequestContext
        `shouldReturn` DatabaseResult
          { databaseResultValue = Left (SecondPageDataError "relation does not exist"),
            databaseResultOperations =
              [ DatabaseOperation
                  { databaseOperationName = "load-second-page-summary",
                    databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;"
                  }
              ]
          }

    it "maps scalar query failures, malformed rows, and highlight query failures into explicit errors" $ do
      let homeFailureRunner command =
            pure $
              if Text.isInfixOf "route_slug = 'home'" (commandSql command)
                then
                  PostgresCommandResult
                    { postgresExitCode = ExitFailure 2,
                      postgresStdout = Text.empty,
                      postgresStderr = Text.empty
                    }
                else successfulPostgresResult Text.empty
          malformedScalarRunner command =
            pure $
              if Text.isInfixOf "route_slug = 'home'" (commandSql command)
                then successfulPostgresResult "first\nsecond"
                else successfulPostgresResult Text.empty
          highlightFailureRunner command =
            pure $
              case commandSql command of
                sql
                  | Text.isInfixOf "SELECT summary FROM web_api.page_content WHERE route_slug = 'second'" sql ->
                      successfulPostgresResult "Loaded from PostgreSQL."
                  | Text.isInfixOf "SELECT highlight FROM web_api.page_highlights" sql ->
                      failingPostgresResult "highlights unavailable"
                  | otherwise ->
                      successfulPostgresResult Text.empty
      loadHomePageData (buildPostgresDatabaseEffectWithRunner homeFailureRunner postgresTestConfig) defaultRequestContext
        `shouldReturn` Left (HomePageDataError "psql command failed")
      loadHomePageData (buildPostgresDatabaseEffectWithRunner malformedScalarRunner postgresTestConfig) defaultRequestContext
        `shouldReturn` Left (HomePageDataError "expected exactly one row: first, second")
      loadSecondPageData (buildPostgresDatabaseEffectWithRunner highlightFailureRunner postgresTestConfig) defaultRequestContext
        `shouldReturn` Left (SecondPageDataError "highlights unavailable")
      loadSecondPageDataWithObservability (buildPostgresDatabaseEffectWithRunner highlightFailureRunner postgresTestConfig) defaultRequestContext
        `shouldReturn` DatabaseResult
          { databaseResultValue = Left (SecondPageDataError "highlights unavailable"),
            databaseResultOperations =
              [ DatabaseOperation
                  { databaseOperationName = "load-second-page-summary",
                    databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;"
                  },
                DatabaseOperation
                  { databaseOperationName = "load-second-page-highlights",
                    databaseQueryTemplate = "SELECT highlight FROM web_api.page_highlights WHERE route_slug = ? AND locale = ? ORDER BY position ASC;"
                  }
              ]
          }

    it "translates database config into runtime SQL queries for page queries" $ do
      recordedScalarQueriesReference <- newIORef []
      recordedRowsQueriesReference <- newIORef []
      let scalarRunner databaseConfig sql = do
            databaseConfig `shouldBe` postgresTestConfig
            modifyIORef' recordedScalarQueriesReference (<> [sql])
            pure $
              case sql of
                queryText
                  | Text.isInfixOf "route_slug = 'home'" queryText ->
                      Right $
                        if Text.isInfixOf "locale = 'fr'" queryText
                          then "Accueil cote serveur avec des donnees de developpement preconfigurees."
                          else "Server-rendered home page with stubbed content."
                  | Text.isInfixOf "SELECT summary FROM web_api.page_content WHERE route_slug = 'second'" queryText ->
                      Right $
                        if Text.isInfixOf "locale = 'fr'" queryText
                          then "Charge depuis PostgreSQL."
                          else "Loaded from PostgreSQL."
                  | otherwise ->
                      Left "unexpected query"
          rowsRunner databaseConfig sql = do
            databaseConfig `shouldBe` postgresTestConfig
            modifyIORef' recordedRowsQueriesReference (<> [sql])
            pure $
              if Text.isInfixOf "locale = 'fr'" sql
                then Right ["SSR rapide", "Donnees partagees"]
                else Right ["Fast SSR", "Shared route data"]
          postgresEffect =
            buildRuntimePostgresDatabaseEffectWithRunner
              scalarRunner
              rowsRunner
              postgresTestConfig
      loadHomePageDataWithObservability postgresEffect defaultRequestContext
        `shouldReturn` DatabaseResult
          { databaseResultValue =
              Right
                HomePageData
                  { homePageDataSummary = "Server-rendered home page with stubbed content."
                  },
            databaseResultOperations =
              [ DatabaseOperation
                  { databaseOperationName = "load-home-page-summary",
                    databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;"
                  }
              ]
          }
      loadSecondPageData postgresEffect defaultRequestContext
        `shouldReturn` Right
          SecondPageData
            { secondPageDataSummary = "Loaded from PostgreSQL.",
              secondPageDataHighlights = ["Fast SSR", "Shared route data"]
            }
      loadHomePageData postgresEffect frenchRequestContext
        `shouldReturn` Right
          HomePageData
            { homePageDataSummary = "Accueil cote serveur avec des donnees de developpement preconfigurees."
            }
      loadSecondPageDataWithObservability postgresEffect frenchRequestContext
        `shouldReturn` DatabaseResult
          { databaseResultValue =
              Right
                SecondPageData
                  { secondPageDataSummary = "Charge depuis PostgreSQL.",
                    secondPageDataHighlights = ["SSR rapide", "Donnees partagees"]
                  },
            databaseResultOperations =
              [ DatabaseOperation
                  { databaseOperationName = "load-second-page-summary",
                    databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;"
                  },
                DatabaseOperation
                  { databaseOperationName = "load-second-page-highlights",
                    databaseQueryTemplate = "SELECT highlight FROM web_api.page_highlights WHERE route_slug = ? AND locale = ? ORDER BY position ASC;"
                  }
              ]
          }
      readIORef recordedScalarQueriesReference
        `shouldReturn` [ "SELECT summary FROM web_api.page_content WHERE route_slug = 'home' AND locale = 'en';",
                         "SELECT summary FROM web_api.page_content WHERE route_slug = 'second' AND locale = 'en';",
                         "SELECT summary FROM web_api.page_content WHERE route_slug = 'home' AND locale = 'fr';",
                         "SELECT summary FROM web_api.page_content WHERE route_slug = 'second' AND locale = 'fr';"
                       ]
      readIORef recordedRowsQueriesReference
        `shouldReturn` [ "SELECT highlight FROM web_api.page_highlights WHERE route_slug = 'second' AND locale = 'en' ORDER BY position ASC;",
                         "SELECT highlight FROM web_api.page_highlights WHERE route_slug = 'second' AND locale = 'fr' ORDER BY position ASC;"
                       ]

    it "maps runtime query failures into explicit database errors" $ do
      let scalarRunner _ sql =
            pure $
              if Text.isInfixOf "route_slug = 'home'" sql
                then Left "connection refused"
                else Right "Loaded from PostgreSQL."
          rowsRunner _ _ =
            pure (Left "highlights unavailable")
          postgresEffect =
            buildRuntimePostgresDatabaseEffectWithRunner
              scalarRunner
              rowsRunner
              postgresTestConfig
      loadHomePageData postgresEffect defaultRequestContext
        `shouldReturn` Left (HomePageDataError "connection refused")
      loadSecondPageData postgresEffect defaultRequestContext
        `shouldReturn` Left (SecondPageDataError "highlights unavailable")
      loadSecondPageDataWithObservability postgresEffect defaultRequestContext
        `shouldReturn` DatabaseResult
          { databaseResultValue = Left (SecondPageDataError "highlights unavailable"),
            databaseResultOperations =
              [ DatabaseOperation
                  { databaseOperationName = "load-second-page-summary",
                    databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;"
                  },
                DatabaseOperation
                  { databaseOperationName = "load-second-page-highlights",
                    databaseQueryTemplate = "SELECT highlight FROM web_api.page_highlights WHERE route_slug = ? AND locale = ? ORDER BY position ASC;"
                  }
              ]
          }

    it "maps runtime second-page summary failures without attempting highlight queries" $ do
      let scalarRunner _ sql =
            pure $
              if Text.isInfixOf "route_slug = 'second'" sql
                then Left "summary unavailable"
                else Right "Server-rendered home page with stubbed content."
          rowsRunner _ _ =
            error "expected runtime highlight query to be skipped when the second-page summary fails"
          postgresEffect =
            buildRuntimePostgresDatabaseEffectWithRunner
              scalarRunner
              rowsRunner
              postgresTestConfig
      loadSecondPageDataWithObservability postgresEffect defaultRequestContext
        `shouldReturn` DatabaseResult
          { databaseResultValue = Left (SecondPageDataError "summary unavailable"),
            databaseResultOperations =
              [ DatabaseOperation
                  { databaseOperationName = "load-second-page-summary",
                    databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;"
                  }
              ]
          }

    it "covers runtime libpq helper decoding branches" $ do
      decodeRuntimeQueryValue Nothing
        `shouldBe` Left "unexpected NULL column value"
      decodeRuntimeQueryValue (Just (ByteString.pack [115, 115, 114, 255]))
        `shouldBe` Right (Text.pack ['s', 's', 'r', '\xfffd'])
      renderRuntimeConnectionErrorMessage Nothing
        `shouldBe` "libpq connection failed"
      renderRuntimeConnectionErrorMessage (Just (ByteString.pack [32, 114, 117, 110, 255, 10]))
        `shouldBe` Text.pack ['r', 'u', 'n', '\xfffd']
      renderRuntimeResultErrorMessage Nothing
        `shouldBe` "libpq query failed"
      renderRuntimeResultErrorMessage (Just (ByteString.pack [32, 113, 117, 101, 114, 121, 255, 10]))
        `shouldBe` Text.pack ['q', 'u', 'e', 'r', 'y', '\xfffd']

    it "runs direct runtime libpq queries and surfaces malformed-row, syntax, and connection failures explicitly" $ do
      ensureDefaultPostgresAvailable
      runPostgresMigrationsForRuntime defaultMigrationPostgresConfig defaultRealPostgresConfig
        `shouldReturn` Right ()

      runRuntimeScalarQuery defaultRealPostgresConfig "SELECT 'Loaded from PostgreSQL.'::text;"
        `shouldReturn` Right "Loaded from PostgreSQL."
      runRuntimeRowsQuery defaultRealPostgresConfig "SELECT value FROM (VALUES ('Fast SSR'::text), ('Shared route data'::text)) AS runtime_rows(value);"
        `shouldReturn` Right ["Fast SSR", "Shared route data"]
      runRuntimeScalarQuery defaultRealPostgresConfig "SELECT value FROM (VALUES ('first'::text), ('second'::text)) AS runtime_rows(value);"
        `shouldReturn` Left "expected exactly one row: first, second"
      runRuntimeRowsQuery defaultRealPostgresConfig "SELECT NULL::text;"
        `shouldReturn` Left "unexpected NULL column value"

      syntaxResult <- runRuntimeRowsQuery defaultRealPostgresConfig "SELECT FROM"
      syntaxResult
        `shouldSatisfy` \case
          Left runtimeError ->
            Text.isInfixOf "syntax error" runtimeError
          Right rows ->
            error ("expected syntax failure, got rows: " <> show rows)

      withUnusedTcpEndpoint $ \unusedEndpoint -> do
        refusedResult <-
          runRuntimeScalarQuery
            defaultRealPostgresConfig
              { databasePort = tcpEndpointPort unusedEndpoint
              }
            "SELECT 1::text;"
        refusedResult
          `shouldSatisfy` \case
            Left runtimeError ->
              not (Text.null runtimeError)
                && not (Text.isInfixOf "posix_spawnp" runtimeError)
            Right value ->
              error ("expected connection failure, got value: " <> show value)

    it "runs migrations and seed statements in order through the provided runner" $ do
      recordedCommandsReference <- newIORef []
      let runner command = modifyIORef' recordedCommandsReference (<> [command]) >> pure (successfulPostgresResult Text.empty)
      runPostgresMigrationsWithRunnerForRuntime runner migrationPostgresTestConfig postgresTestConfig `shouldReturn` Right ()
      runPostgresSeedWithRunner runner postgresTestConfig `shouldReturn` Right ()
      recordedCommands <- readIORef recordedCommandsReference
      map commandSql recordedCommands `shouldBe` migrationStatementsFor migrationPostgresTestConfig postgresTestConfig <> seedStatements

    it "keeps the legacy same-config migration wrappers on the runtime-config path"
      $ withFakePsqlScript
        (fmap (,Text.empty) (migrationStatementsFor postgresTestConfig postgresTestConfig))
      $ \argsLogPath -> do
        recordedCommandsReference <- newIORef []
        let runner command = modifyIORef' recordedCommandsReference (<> [command]) >> pure (successfulPostgresResult Text.empty)
        runPostgresMigrationsWithRunner runner postgresTestConfig `shouldReturn` Right ()
        map commandSql
          <$> readIORef recordedCommandsReference
            `shouldReturn` migrationStatementsFor postgresTestConfig postgresTestConfig
        runPostgresMigrations postgresTestConfig `shouldReturn` Right ()
        let renderMutationLogEntry databaseConfig sql =
              "--host "
                <> Text.unpack (databaseHost databaseConfig)
                <> " --port "
                <> show (databasePort databaseConfig)
                <> " --dbname "
                <> Text.unpack (databaseName databaseConfig)
                <> " --username "
                <> Text.unpack (databaseUser databaseConfig)
                <> " --no-password --set ON_ERROR_STOP=1 --command "
                <> Text.unpack sql
        readFile argsLogPath
          `shouldReturn` unlines
            (fmap (renderMutationLogEntry postgresTestConfig) (migrationStatementsFor postgresTestConfig postgresTestConfig))

    it "stops database setup when a migration or seed command fails" $ do
      case seedStatements of
        failingSeedStatement : _ -> do
          let runner command =
                pure $
                  if commandSql command == failingSeedStatement
                    then failingPostgresResult "seed failed"
                    else successfulPostgresResult Text.empty
          runPostgresSeedWithRunner runner postgresTestConfig
            `shouldReturn` Left
              ( PostgresCommandFailed
                  PostgresCommand
                    { postgresExecutable = "psql",
                      postgresArguments =
                        [ "--host",
                          "db.internal",
                          "--port",
                          "6543",
                          "--dbname",
                          "web_api_prod",
                          "--username",
                          "web_api_app",
                          "--no-password",
                          "--set",
                          "ON_ERROR_STOP=1",
                          "--command",
                          "DELETE FROM web_api.page_highlights;"
                        ],
                      postgresEnvironment = [("PGPASSWORD", "super-secret")]
                    }
                  PostgresCommandResult
                    { postgresExitCode = ExitFailure 1,
                      postgresStdout = Text.empty,
                      postgresStderr = "seed failed"
                    }
              )
        [] -> expectationFailure "expected at least one seed statement"

    it "keeps postgres command, result, and error values serializable and stable" $ do
      let command =
            PostgresCommand
              { postgresExecutable = "psql",
                postgresArguments = ["--command", "SELECT 1;"],
                postgresEnvironment = [("PGPASSWORD", "secret")]
              }
          commandResult =
            PostgresCommandResult
              { postgresExitCode = ExitSuccess,
                postgresStdout = "1",
                postgresStderr = Text.empty
              }
          failedCommandResult =
            PostgresCommandResult
              { postgresExitCode = ExitFailure 3,
                postgresStdout = Text.empty,
                postgresStderr = "boom"
              }
          runnerError = PostgresCommandFailed command commandResult
          unexpectedRowsError = UnexpectedQueryRows "expected exactly one row" ["first", "second"]
      command `shouldBe` command
      command `shouldNotBe` command {postgresArguments = ["--command", "SELECT 2;"]}
      commandResult `shouldBe` commandResult
      commandResult `shouldNotBe` commandResult {postgresStdout = "2"}
      runnerError `shouldBe` runnerError
      runnerError `shouldNotBe` PostgresCommandFailed command failedCommandResult
      unexpectedRowsError `shouldBe` unexpectedRowsError
      unexpectedRowsError `shouldNotBe` UnexpectedQueryRows "expected exactly one row" ["first"]
      show command
        `shouldBe` "PostgresCommand {postgresExecutable = \"psql\", postgresArguments = [\"--command\",\"SELECT 1;\"], postgresEnvironment = [(\"PGPASSWORD\",\"secret\")]}"
      show commandResult
        `shouldBe` "PostgresCommandResult {postgresExitCode = ExitSuccess, postgresStdout = \"1\", postgresStderr = \"\"}"
      show failedCommandResult
        `shouldBe` "PostgresCommandResult {postgresExitCode = ExitFailure 3, postgresStdout = \"\", postgresStderr = \"boom\"}"
      show runnerError
        `shouldBe` "PostgresCommandFailed (PostgresCommand {postgresExecutable = \"psql\", postgresArguments = [\"--command\",\"SELECT 1;\"], postgresEnvironment = [(\"PGPASSWORD\",\"secret\")]}) (PostgresCommandResult {postgresExitCode = ExitSuccess, postgresStdout = \"1\", postgresStderr = \"\"})"
      show unexpectedRowsError
        `shouldBe` "UnexpectedQueryRows \"expected exactly one row\" [\"first\",\"second\"]"
      show [command]
        `shouldBe` "[PostgresCommand {postgresExecutable = \"psql\", postgresArguments = [\"--command\",\"SELECT 1;\"], postgresEnvironment = [(\"PGPASSWORD\",\"secret\")]}]"
      show [commandResult]
        `shouldBe` "[PostgresCommandResult {postgresExitCode = ExitSuccess, postgresStdout = \"1\", postgresStderr = \"\"}]"
      show [runnerError]
        `shouldBe` "[PostgresCommandFailed (PostgresCommand {postgresExecutable = \"psql\", postgresArguments = [\"--command\",\"SELECT 1;\"], postgresEnvironment = [(\"PGPASSWORD\",\"secret\")]}) (PostgresCommandResult {postgresExitCode = ExitSuccess, postgresStdout = \"1\", postgresStderr = \"\"})]"

    it "uses the default psql runner for effect loading and database setup when psql is on PATH"
      $ withFakePsqlScript
        ( [ ("SELECT summary FROM web_api.page_content WHERE route_slug = 'home' AND locale = 'en';", "Server-rendered home page with stubbed content."),
            ("SELECT summary FROM web_api.page_content WHERE route_slug = 'second' AND locale = 'en';", "Second page content with stubbed data ready for future loaders."),
            ("SELECT highlight FROM web_api.page_highlights WHERE route_slug = 'second' AND locale = 'en' ORDER BY position ASC;", Text.empty)
          ]
            <> fmap (,Text.empty) (migrationStatementsFor migrationPostgresTestConfig postgresTestConfig <> seedStatements)
        )
      $ \argsLogPath -> do
        let application = buildAppWithDatabase defaultAppConfig (buildPostgresDatabaseEffect postgresTestConfig)
        HarchWeb.renderResponse application secondRequest
          `shouldReturn` HarchWeb.PageResponseWithMetadata
            HarchWeb.ResponseBody
              { HarchWeb.responseStatus = 200,
                HarchWeb.responseContentType = "text/html; charset=utf-8",
                HarchWeb.responseBody = "",
                HarchWeb.responseObservabilityAttributes =
                  [ Observability.ObservabilityAttribute
                      { Observability.attributeName = "db.system",
                        Observability.attributeValue = Observability.TextAttribute "postgresql"
                      },
                    Observability.ObservabilityAttribute
                      { Observability.attributeName = "db.operation.name",
                        Observability.attributeValue = Observability.TextAttribute "load-second-page-summary"
                      },
                    Observability.ObservabilityAttribute
                      { Observability.attributeName = "db.query.template",
                        Observability.attributeValue = Observability.TextAttribute "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;"
                      },
                    Observability.ObservabilityAttribute
                      { Observability.attributeName = "db.system",
                        Observability.attributeValue = Observability.TextAttribute "postgresql"
                      },
                    Observability.ObservabilityAttribute
                      { Observability.attributeName = "db.operation.name",
                        Observability.attributeValue = Observability.TextAttribute "load-second-page-highlights"
                      },
                    Observability.ObservabilityAttribute
                      { Observability.attributeName = "db.query.template",
                        Observability.attributeValue = Observability.TextAttribute "SELECT highlight FROM web_api.page_highlights WHERE route_slug = ? AND locale = ? ORDER BY position ASC;"
                      }
                  ],
                HarchWeb.responseLogEntries = []
              }
            ( HarchWeb.Page
                { HarchWeb.pageTitle = "web-api: Second",
                  HarchWeb.pageRoute = SecondRoute,
                  HarchWeb.pageContext = defaultRequestContext,
                  HarchWeb.pageBody = "<section data-page=\"second\"><h1 data-page-title=\"true\">Second</h1><p>Second page content with stubbed data ready for future loaders.</p><p data-empty-state=\"true\">No highlights yet.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section>",
                  HarchWeb.pageBootstrapHooks = ["second-page"]
                }
            )
        runPostgresMigrationsForRuntime migrationPostgresTestConfig postgresTestConfig `shouldReturn` Right ()
        runPostgresSeed postgresTestConfig `shouldReturn` Right ()
        let renderQueryLogEntry sql =
              "--host db.internal --port 6543 --dbname web_api_prod --username web_api_app --no-password --set ON_ERROR_STOP=1 --tuples-only --no-align --quiet --command "
                <> Text.unpack sql
            renderMutationLogEntry databaseConfig sql =
              "--host "
                <> Text.unpack (databaseHost databaseConfig)
                <> " --port "
                <> show (databasePort databaseConfig)
                <> " --dbname "
                <> Text.unpack (databaseName databaseConfig)
                <> " --username "
                <> Text.unpack (databaseUser databaseConfig)
                <> " --no-password --set ON_ERROR_STOP=1 --command "
                <> Text.unpack sql
        readFile argsLogPath
          `shouldReturn` unlines
            ( [ renderQueryLogEntry "SELECT summary FROM web_api.page_content WHERE route_slug = 'second' AND locale = 'en';",
                renderQueryLogEntry "SELECT highlight FROM web_api.page_highlights WHERE route_slug = 'second' AND locale = 'en' ORDER BY position ASC;"
              ]
                <> fmap (renderMutationLogEntry migrationPostgresTestConfig) (migrationStatementsFor migrationPostgresTestConfig postgresTestConfig)
                <> fmap (renderMutationLogEntry postgresTestConfig) seedStatements
            )

    it "uses stderr from the default psql runner when a command fails"
      $ withFakePsqlScriptResults
        [ ( "SELECT summary FROM web_api.page_content WHERE route_slug = 'home' AND locale = 'en';",
            PostgresCommandResult
              { postgresExitCode = ExitFailure 4,
                postgresStdout = Text.empty,
                postgresStderr = "default runner failed"
              }
          )
        ]
      $ \_ ->
        loadHomePageData (buildPostgresDatabaseEffect postgresTestConfig) defaultRequestContext
          `shouldReturn` Left (HomePageDataError "default runner failed")

    it "prefers a runtime that is already running the named postgres container in the containerized psql wrapper" $ do
      containerizedPsqlScriptContents `shouldContain'` "database_endpoint_is_reachable()"
      containerizedPsqlScriptContents `shouldContain'` "host_psql_path=\"${WEB_API_REAL_PSQL_PATH:-}\""
      containerizedPsqlScriptContents `shouldContain'` "if [ -n \"$host_psql_path\" ] && [ -x \"$host_psql_path\" ] && database_endpoint_is_reachable; then"
      containerizedPsqlScriptContents `shouldContain'` "runtime_with_running_container()"
      containerizedPsqlScriptContents `shouldContain'` "for candidate in docker podman; do"
      containerizedPsqlScriptContents `shouldContain'` "elif runtime=$(runtime_with_existing_container); then"
      containerizedPsqlScriptContents `shouldContain'` "exec \"$runtime\" exec -e PGPASSWORD=\"${PGPASSWORD:-}\" web-api-postgres psql \"$@\""

    it "prefers a runtime that is already running the named postgres container before trying to start or create one" $ do
      ensureDefaultPostgresAvailableScript `shouldContain'` "database_endpoint_is_reachable()"
      ensureDefaultPostgresAvailableScript `shouldContain'` "host_psql_is_available()"
      ensureDefaultPostgresAvailableScript `shouldContain'` "owner_is_superuser_via_host_psql()"
      ensureDefaultPostgresAvailableScript `shouldContain'` "ensure_owner_superuser_via_host_psql()"
      ensureDefaultPostgresAvailableScript `shouldContain'` "if database_endpoint_is_reachable && host_psql_is_available; then"
      ensureDefaultPostgresAvailableScript `shouldContain'` "runtime_with_running_container()"
      ensureDefaultPostgresAvailableScript `shouldContain'` "for candidate in docker podman; do"
      ensureDefaultPostgresAvailableScript `shouldContain'` "elif runtime=$(runtime_with_existing_container); then"
      ensureDefaultPostgresAvailableScript `shouldContain'` "\"$runtime\" start web-api-postgres >/dev/null 2>&1 && return 0"

    it "loads seeded page data through the concrete postgres adapter against real PostgreSQL" $
      withContainerizedPsqlOnPath $ do
        ensureDefaultPostgresAvailable
        runPostgresMigrationsForRuntime defaultMigrationPostgresConfig defaultRealPostgresConfig `shouldReturn` Right ()
        runPostgresSeed defaultMigrationPostgresConfig `shouldReturn` Right ()
        let postgresEffect = buildPostgresDatabaseEffect defaultRealPostgresConfig
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

  describe "parseAppEnvironmentConfig" $ do
    it "parses committed development defaults into the expected config" $
      parseAppEnvironmentConfig committedEnvDefaults [] []
        `shouldBe` Right defaultAppEnvironmentConfig

    it "lets .env.local override committed .env defaults" $ do
      let localOverrides =
            [ ("APP_MODE", "production"),
              ("DATABASE_HOST", "localhost"),
              ("DATABASE_PORT", "6432"),
              ("DATABASE_NAME", "web_api_local"),
              ("DATABASE_USER", "local_user"),
              ("DATABASE_PASSWORD", "local_password")
            ]
      parseAppEnvironmentConfig committedEnvDefaults localOverrides []
        `shouldBe` Right
          AppEnvironmentConfig
            { appMode = Production,
              databaseConfig =
                DatabaseConfig
                  { databaseHost = "localhost",
                    databasePort = 6432,
                    databaseName = "web_api_local",
                    databaseUser = "local_user",
                    databasePassword = "local_password"
                  }
            }

    it "lets environment variables override .env.local values" $ do
      let localOverrides =
            [ ("APP_MODE", "production"),
              ("DATABASE_HOST", "localhost"),
              ("DATABASE_PORT", "6432"),
              ("DATABASE_NAME", "web_api_local"),
              ("DATABASE_USER", "local_user"),
              ("DATABASE_PASSWORD", "local_password")
            ]
          environmentOverrides =
            [ ("APP_MODE", "test"),
              ("DATABASE_PORT", "7432"),
              ("DATABASE_PASSWORD", "runtime_password")
            ]
      parseAppEnvironmentConfig committedEnvDefaults localOverrides environmentOverrides
        `shouldBe` Right
          AppEnvironmentConfig
            { appMode = Test,
              databaseConfig =
                DatabaseConfig
                  { databaseHost = "localhost",
                    databasePort = 7432,
                    databaseName = "web_api_local",
                    databaseUser = "local_user",
                    databasePassword = "runtime_password"
                  }
            }

    it "fails missing required values with explicit errors" $
      parseAppEnvironmentConfig
        [ ("APP_MODE", "development"),
          ("DATABASE_HOST", "127.0.0.1"),
          ("DATABASE_PORT", "5432"),
          ("DATABASE_NAME", "web_api_dev"),
          ("DATABASE_USER", "web_api_runtime")
        ]
        []
        []
        `shouldBe` Left (MissingConfigValue "DATABASE_PASSWORD")

    it "fails invalid port or mode values with precise errors" $ do
      parseAppEnvironmentConfig committedEnvDefaults [] [("APP_MODE", "staging")]
        `shouldBe` Left (InvalidConfigValue "APP_MODE" "staging")
      parseAppEnvironmentConfig committedEnvDefaults [] [("DATABASE_PORT", "0")]
        `shouldBe` Left (InvalidConfigValue "DATABASE_PORT" "0")

  describe "loadAppEnvironmentConfigWithFiles" $ do
    it "loads the documented .env then .env.local layers" $
      withSystemTempDirectory "app-environment-config" $ \tempDirectory ->
        withClearedAppEnvironment $ do
          let envPath = tempDirectory <> "/.env"
              envLocalPath = tempDirectory <> "/.env.local"
          writeFile envPath "APP_MODE=production\nDATABASE_HOST=db.shared\nDATABASE_PORT=6432\nDATABASE_NAME=shared_db\nDATABASE_USER=shared_user\nDATABASE_PASSWORD=shared_password\n"
          writeFile envLocalPath "APP_MODE=test\nDATABASE_PORT=7432\nDATABASE_PASSWORD=local_password\n"
          loadAppEnvironmentConfigWithFiles envPath envLocalPath
            `shouldReturn` Right
              AppEnvironmentConfig
                { appMode = Test,
                  databaseConfig =
                    DatabaseConfig
                      { databaseHost = "db.shared",
                        databasePort = 7432,
                        databaseName = "shared_db",
                        databaseUser = "shared_user",
                        databasePassword = "local_password"
                      }
                }

    it "lets process environment override .env.local values" $
      withSystemTempDirectory "app-environment-config-env" $ \tempDirectory ->
        withClearedAppEnvironment $
          withTemporaryEnvironment "APP_MODE" (Just "production") $
            withTemporaryEnvironment "DATABASE_PORT" (Just "8432") $
              withTemporaryEnvironment "DATABASE_PASSWORD" (Just "runtime_password") $ do
                let envPath = tempDirectory <> "/.env"
                    envLocalPath = tempDirectory <> "/.env.local"
                writeFile envPath "APP_MODE=development\nDATABASE_HOST=db.shared\nDATABASE_PORT=6432\nDATABASE_NAME=shared_db\nDATABASE_USER=shared_user\nDATABASE_PASSWORD=shared_password\n"
                writeFile envLocalPath "APP_MODE=test\nDATABASE_PORT=7432\nDATABASE_PASSWORD=local_password\n"
                loadAppEnvironmentConfigWithFiles envPath envLocalPath
                  `shouldReturn` Right
                    AppEnvironmentConfig
                      { appMode = Production,
                        databaseConfig =
                          DatabaseConfig
                            { databaseHost = "db.shared",
                              databasePort = 8432,
                              databaseName = "shared_db",
                              databaseUser = "shared_user",
                              databasePassword = "runtime_password"
                            }
                      }

    it "reports invalid override files with the failing path" $
      withSystemTempDirectory "app-environment-config-error" $ \tempDirectory ->
        withClearedAppEnvironment $ do
          let envPath = tempDirectory <> "/.env"
              envLocalPath = tempDirectory <> "/.env.local"
          writeFile envPath "DATABASE_HOST\n"
          loadAppEnvironmentConfigWithFiles envPath envLocalPath
            `shouldReturn` Left
              (AppEnvironmentOverridesFileError envPath (InvalidConfigOverridesLine 1 "DATABASE_HOST"))

    it "reports unreadable override files with the failing path" $
      withSystemTempDirectory "app-environment-config-unreadable" $ \tempDirectory ->
        withClearedAppEnvironment $ do
          let envPath = tempDirectory <> "/.env"
              envLocalPath = tempDirectory <> "/.env.local"
          writeFile envPath "APP_MODE=production\nDATABASE_HOST=db.shared\nDATABASE_PORT=6432\nDATABASE_NAME=shared_db\nDATABASE_USER=shared_user\nDATABASE_PASSWORD=shared_password\n"
          withUnreadableFile envLocalPath "APP_MODE=test\n" $ do
            result <- loadAppEnvironmentConfigWithFiles envPath envLocalPath
            result `shouldSatisfy` \case
              Left
                (AppEnvironmentOverridesFileError failingPath (UnreadableConfigOverridesFile errorMessage))
                  | failingPath == envLocalPath ->
                      not (Text.null errorMessage)
              _ -> False

    it "reports parse errors after both files load successfully" $
      withSystemTempDirectory "app-environment-config-parse-error" $ \tempDirectory ->
        withClearedAppEnvironment $ do
          let envPath = tempDirectory <> "/.env"
              envLocalPath = tempDirectory <> "/.env.local"
          writeFile envPath "DATABASE_PORT=0\n"
          loadAppEnvironmentConfigWithFiles envPath envLocalPath
            `shouldReturn` Left
              (AppEnvironmentConfigParseError (InvalidConfigValue "DATABASE_PORT" "0"))

  describe "loadAppEnvironmentConfig" $
    it "loads the default .env file names from the current directory" $
      withSystemTempDirectory "app-environment-config-current-directory" $ \tempDirectory ->
        withClearedAppEnvironment $ do
          writeFile (tempDirectory <> "/.env") "APP_MODE=production\nDATABASE_HOST=db.shared\nDATABASE_PORT=6432\nDATABASE_NAME=shared_db\nDATABASE_USER=shared_user\nDATABASE_PASSWORD=shared_password\n"
          writeFile (tempDirectory <> "/.env.local") "APP_MODE=test\nDATABASE_PASSWORD=local_password\n"
          withCurrentDirectory tempDirectory $
            loadAppEnvironmentConfig
              `shouldReturn` Right
                AppEnvironmentConfig
                  { appMode = Test,
                    databaseConfig =
                      DatabaseConfig
                        { databaseHost = "db.shared",
                          databasePort = 6432,
                          databaseName = "shared_db",
                          databaseUser = "shared_user",
                          databasePassword = "local_password"
                        }
                  }

  describe "AppEnvironmentConfigLoadError" $
    it "keeps load-error equality and rendering deterministic" $ do
      let fileLoadError = AppEnvironmentOverridesFileError ".env" (InvalidConfigOverridesLine 1 "BROKEN")
          parseLoadError = AppEnvironmentConfigParseError (InvalidConfigValue "DATABASE_PORT" "0")
      fileLoadError `shouldBe` fileLoadError
      fileLoadError `shouldNotBe` parseLoadError
      show fileLoadError
        `shouldBe` "AppEnvironmentOverridesFileError \".env\" (InvalidConfigOverridesLine 1 \"BROKEN\")"
      show parseLoadError
        `shouldBe` "AppEnvironmentConfigParseError (InvalidConfigValue \"DATABASE_PORT\" \"0\")"
      show [fileLoadError, parseLoadError]
        `shouldBe` "[AppEnvironmentOverridesFileError \".env\" (InvalidConfigOverridesLine 1 \"BROKEN\"),AppEnvironmentConfigParseError (InvalidConfigValue \"DATABASE_PORT\" \"0\")]"

  describe "parseAppStartupConfig" $
    it "parses committed environment and runtime defaults into the expected startup config" $ do
      defaultAppStartupConfig
        `shouldBe` AppStartupConfig
          { startupEnvironmentConfig = defaultAppEnvironmentConfig,
            startupAppConfig = defaultAppConfig
          }
      parseAppStartupConfig (committedEnvDefaults <> committedRuntimeDefaults) [] []
        `shouldBe` Right defaultAppStartupConfig

  describe "loadAppStartupConfigWithFiles" $ do
    it "loads the documented .env then .env.local layers for runtime startup" $
      withSystemTempDirectory "app-startup-config" $ \tempDirectory ->
        withClearedAppEnvironment $
          withClearedRuntimeEnvironment $ do
            let envPath = tempDirectory <> "/.env"
                envLocalPath = tempDirectory <> "/.env.local"
            writeFile envPath "APP_MODE=production\nDATABASE_HOST=db.shared\nDATABASE_PORT=6432\nAPP_TITLE_PREFIX=web-api-shared\nLISTENER_0_PORT=5443\n"
            writeFile envLocalPath "DATABASE_PASSWORD=local_password\nAPP_TITLE_PREFIX=web-api-local\nLISTENER_0_PORT=7443\n"
            loadAppStartupConfigWithFiles envPath envLocalPath
              `shouldReturn` Right
                AppStartupConfig
                  { startupEnvironmentConfig =
                      AppEnvironmentConfig
                        { appMode = Production,
                          databaseConfig =
                            DatabaseConfig
                              { databaseHost = "db.shared",
                                databasePort = 6432,
                                databaseName = "web_api_dev",
                                databaseUser = "web_api_runtime",
                                databasePassword = "local_password"
                              }
                        },
                    startupAppConfig =
                      defaultAppConfig
                        { appTitlePrefix = "web-api-local",
                          listenerConfigs =
                            [ ListenerConfig
                                { listenerHost = "127.0.0.1",
                                  listenerPort = 7443,
                                  listenerScheme = Http,
                                  listenerTls = Nothing,
                                  listenerAcme = Nothing
                                }
                            ]
                        }
                  }

    it "lets process environment override .env.local values for runtime startup" $
      withSystemTempDirectory "app-startup-config-env" $ \tempDirectory ->
        withClearedAppEnvironment $
          withClearedRuntimeEnvironment $
            withTemporaryEnvironment "APP_TITLE_PREFIX" (Just "web-api-runtime") $
              withTemporaryEnvironment "LISTENER_0_HOST" (Just "0.0.0.0") $
                withTemporaryEnvironment "LISTENER_0_PORT" (Just "80") $ do
                  let envPath = tempDirectory <> "/.env"
                      envLocalPath = tempDirectory <> "/.env.local"
                  writeFile envPath "APP_MODE=production\nDATABASE_HOST=db.shared\nDATABASE_PORT=6432\nAPP_TITLE_PREFIX=web-api-shared\nLISTENER_0_HOST=127.0.0.1\nLISTENER_0_PORT=5443\n"
                  writeFile envLocalPath "DATABASE_PASSWORD=local_password\nAPP_TITLE_PREFIX=web-api-local\nLISTENER_0_PORT=7443\n"
                  loadAppStartupConfigWithFiles envPath envLocalPath
                    `shouldReturn` Right
                      AppStartupConfig
                        { startupEnvironmentConfig =
                            AppEnvironmentConfig
                              { appMode = Production,
                                databaseConfig =
                                  DatabaseConfig
                                    { databaseHost = "db.shared",
                                      databasePort = 6432,
                                      databaseName = "web_api_dev",
                                      databaseUser = "web_api_runtime",
                                      databasePassword = "local_password"
                                    }
                              },
                          startupAppConfig =
                            defaultAppConfig
                              { appTitlePrefix = "web-api-runtime",
                                listenerConfigs =
                                  [ ListenerConfig
                                      { listenerHost = "0.0.0.0",
                                        listenerPort = 80,
                                        listenerScheme = Http,
                                        listenerTls = Nothing,
                                        listenerAcme = Nothing
                                      }
                                  ]
                              }
                        }

    it "reports invalid override files or parse failures with explicit errors" $
      withSystemTempDirectory "app-startup-config-errors" $ \tempDirectory ->
        withClearedAppEnvironment $
          withClearedRuntimeEnvironment $ do
            let brokenEnvPath = tempDirectory <> "/broken.env"
                envLocalPath = tempDirectory <> "/.env.local"
                invalidEnvPath = tempDirectory <> "/invalid.env"
            writeFile brokenEnvPath "APP_TITLE_PREFIX\n"
            loadAppStartupConfigWithFiles brokenEnvPath envLocalPath
              `shouldReturn` Left
                (AppStartupOverridesFileError brokenEnvPath (InvalidConfigOverridesLine 1 "APP_TITLE_PREFIX"))
            writeFile invalidEnvPath "LISTENER_0_PORT=0\n"
            loadAppStartupConfigWithFiles invalidEnvPath envLocalPath
              `shouldReturn` Left
                (AppStartupConfigParseError (InvalidConfigValue "LISTENER_0_PORT" "0"))

    it "reports unreadable override files with the failing path" $
      withSystemTempDirectory "app-startup-config-unreadable" $ \tempDirectory ->
        withClearedAppEnvironment $
          withClearedRuntimeEnvironment $ do
            let envPath = tempDirectory <> "/.env"
                envLocalPath = tempDirectory <> "/.env.local"
            writeFile envPath "APP_MODE=production\nDATABASE_HOST=db.shared\nDATABASE_PORT=6432\nAPP_TITLE_PREFIX=web-api-shared\nLISTENER_0_PORT=5443\n"
            withUnreadableFile envLocalPath "DATABASE_PASSWORD=local_password\nAPP_TITLE_PREFIX=web-api-local\n" $ do
              result <- loadAppStartupConfigWithFiles envPath envLocalPath
              result `shouldSatisfy` \case
                Left
                  (AppStartupOverridesFileError failingPath (UnreadableConfigOverridesFile errorMessage))
                    | failingPath == envLocalPath ->
                        not (Text.null errorMessage)
                _ -> False

  describe "loadAppStartupConfig" $
    it "loads the default .env file names for runtime startup from the current directory" $
      withSystemTempDirectory "app-startup-config-current-directory" $ \tempDirectory ->
        withClearedAppEnvironment $
          withClearedRuntimeEnvironment $ do
            writeFile (tempDirectory <> "/.env") "APP_MODE=production\nAPP_TITLE_PREFIX=web-api-shared\n"
            writeFile (tempDirectory <> "/.env.local") "APP_MODE=test\nLISTENER_0_PORT=6001\n"
            withCurrentDirectory tempDirectory $
              loadAppStartupConfig
                `shouldReturn` Right
                  defaultAppStartupConfig
                    { startupEnvironmentConfig =
                        defaultAppEnvironmentConfig
                          { appMode = Test
                          },
                      startupAppConfig =
                        defaultAppConfig
                          { appTitlePrefix = "web-api-shared",
                            listenerConfigs =
                              [ ListenerConfig
                                  { listenerHost = "127.0.0.1",
                                    listenerPort = 6001,
                                    listenerScheme = Http,
                                    listenerTls = Nothing,
                                    listenerAcme = Nothing
                                  }
                              ]
                          }
                    }

  describe "AppStartupConfig and AppStartupConfigLoadError" $
    it "keep equality and rendering deterministic" $ do
      let startupConfig =
            AppStartupConfig
              { startupEnvironmentConfig = defaultAppEnvironmentConfig {appMode = Test},
                startupAppConfig = defaultAppConfig {appTitlePrefix = "web-api-test"}
              }
          differentStartupConfig =
            AppStartupConfig
              { startupEnvironmentConfig = defaultAppEnvironmentConfig,
                startupAppConfig = defaultAppConfig
              }
          fileLoadError = AppStartupOverridesFileError ".env" (InvalidConfigOverridesLine 1 "BROKEN")
          parseLoadError = AppStartupConfigParseError (InvalidConfigValue "LISTENER_0_PORT" "0")
      startupConfig `shouldBe` startupConfig
      startupConfig `shouldNotBe` differentStartupConfig
      show startupConfig
        `shouldContain` ("staticAssetContentTypes = " <> show defaultStaticAssetContentTypes)
      show [startupConfig]
        `shouldContain` ("staticAssetContentTypes = " <> show defaultStaticAssetContentTypes)
      fileLoadError `shouldBe` fileLoadError
      fileLoadError `shouldNotBe` parseLoadError
      show fileLoadError
        `shouldBe` "AppStartupOverridesFileError \".env\" (InvalidConfigOverridesLine 1 \"BROKEN\")"
      show parseLoadError
        `shouldBe` "AppStartupConfigParseError (InvalidConfigValue \"LISTENER_0_PORT\" \"0\")"
      show [fileLoadError, parseLoadError]
        `shouldBe` "[AppStartupOverridesFileError \".env\" (InvalidConfigOverridesLine 1 \"BROKEN\"),AppStartupConfigParseError (InvalidConfigValue \"LISTENER_0_PORT\" \"0\")]"

  describe "parseAppSetupConfig" $ do
    it "parses committed runtime and setup defaults into the expected setup config" $ do
      committedSetupDefaults
        `shouldBe` [ ("SETUP_AUTOSTART_DATABASE", "true"),
                     ("SETUP_AUTOSTART_JAEGER", "false")
                   ]
      defaultSetupAutostartConfig
        `shouldBe` SetupAutostartConfig
          { setupAutostartDatabase = True,
            setupAutostartJaeger = False
          }
      defaultAppSetupConfig
        `shouldBe` AppSetupConfig
          { setupEnvironmentConfig = defaultAppEnvironmentConfig,
            setupAppConfig = defaultAppConfig,
            setupMigrationDatabaseConfig = Nothing,
            setupAutostartConfig = defaultSetupAutostartConfig
          }
      parseAppSetupConfig (committedEnvDefaults <> committedRuntimeDefaults <> committedSetupDefaults) [] []
        `shouldBe` Right defaultAppSetupConfig
      parseAppSetupConfig (committedEnvDefaults <> committedRuntimeDefaults <> committedSetupDefaults) [] []
        `shouldBe` Right defaultAppSetupConfig
      parseAppSetupConfig (committedEnvDefaults <> committedRuntimeDefaults) [] []
        `shouldBe` Right defaultAppSetupConfig

    it "lets setup booleans follow the same layered precedence as runtime config" $
      parseAppSetupConfig
        (committedEnvDefaults <> committedRuntimeDefaults <> committedSetupDefaults)
        [ ("APP_TITLE_PREFIX", "setup-local"),
          ("SETUP_AUTOSTART_DATABASE", "yes")
        ]
        [("SETUP_AUTOSTART_JAEGER", "1")]
        `shouldBe` Right
          AppSetupConfig
            { setupEnvironmentConfig = defaultAppEnvironmentConfig,
              setupAppConfig =
                defaultAppConfig
                  { appTitlePrefix = "setup-local"
                  },
              setupMigrationDatabaseConfig = Nothing,
              setupAutostartConfig =
                SetupAutostartConfig
                  { setupAutostartDatabase = True,
                    setupAutostartJaeger = True
                  }
            }

    it "lets OTLP_TRACING_ENABLED use the default local endpoint while still flowing into setup config" $
      parseAppSetupConfig
        (committedEnvDefaults <> committedRuntimeDefaults <> committedSetupDefaults)
        []
        [ ("OTLP_TRACING_ENABLED", "true"),
          ("OTLP_TRACING_HEADERS", "authorization=Bearer token")
        ]
        `shouldBe` Right
          AppSetupConfig
            { setupEnvironmentConfig = defaultAppEnvironmentConfig,
              setupAppConfig =
                defaultAppConfig
                  { observability =
                      ObservabilityConfig
                        { tracingExporter =
                            Just
                              OtlpExporter
                                { otlpEndpoint = "http://127.0.0.1:4318/v1/traces",
                                  otlpHeaders = [("authorization", "Bearer token")]
                                },
                          metricsExporter = Nothing
                        }
                  },
              setupMigrationDatabaseConfig = Nothing,
              setupAutostartConfig = defaultSetupAutostartConfig
            }

    it "parses optional migration-owner credentials separately from the runtime database config" $
      parseAppSetupConfig
        (committedEnvDefaults <> committedRuntimeDefaults <> committedSetupDefaults)
        [ ("DATABASE_USER", "web_api_runtime"),
          ("WEB_API_MIGRATION_DATABASE_HOST", "127.0.0.1"),
          ("WEB_API_MIGRATION_DATABASE_PORT", "5432"),
          ("WEB_API_MIGRATION_DATABASE_NAME", "web_api_dev"),
          ("WEB_API_MIGRATION_DATABASE_USER", "web_api_owner")
        ]
        [("WEB_API_MIGRATION_DATABASE_PASSWORD", "owner-secret")]
        `shouldBe` Right
          AppSetupConfig
            { setupEnvironmentConfig =
                defaultAppEnvironmentConfig
                  { databaseConfig =
                      DatabaseConfig
                        { databaseHost = "127.0.0.1",
                          databasePort = 5432,
                          databaseName = "web_api_dev",
                          databaseUser = "web_api_runtime",
                          databasePassword = "web_api"
                        }
                  },
              setupAppConfig = defaultAppConfig,
              setupMigrationDatabaseConfig =
                Just
                  DatabaseConfig
                    { databaseHost = "127.0.0.1",
                      databasePort = 5432,
                      databaseName = "web_api_dev",
                      databaseUser = "web_api_owner",
                      databasePassword = "owner-secret"
                    },
              setupAutostartConfig = defaultSetupAutostartConfig
            }

    it "fails invalid runtime, setup, or partial migration config values explicitly" $ do
      parseAppSetupConfig
        (committedEnvDefaults <> committedRuntimeDefaults <> committedSetupDefaults)
        []
        [("LISTENER_0_PORT", "0")]
        `shouldBe` Left (InvalidConfigValue "LISTENER_0_PORT" "0")
      parseAppSetupConfig
        (committedEnvDefaults <> committedRuntimeDefaults <> committedSetupDefaults)
        []
        [("SETUP_AUTOSTART_DATABASE", "sometimes")]
        `shouldBe` Left (InvalidConfigValue "SETUP_AUTOSTART_DATABASE" "sometimes")
      parseAppSetupConfig
        (committedEnvDefaults <> committedRuntimeDefaults <> committedSetupDefaults)
        []
        [("WEB_API_MIGRATION_DATABASE_HOST", "127.0.0.1")]
        `shouldBe` Left (MissingConfigValue "WEB_API_MIGRATION_DATABASE_PORT")
      parseAppSetupConfig
        (committedEnvDefaults <> committedRuntimeDefaults <> committedSetupDefaults)
        []
        [ ("WEB_API_MIGRATION_DATABASE_HOST", "127.0.0.1"),
          ("WEB_API_MIGRATION_DATABASE_PORT", "0"),
          ("WEB_API_MIGRATION_DATABASE_NAME", "web_api_dev"),
          ("WEB_API_MIGRATION_DATABASE_USER", "web_api_owner"),
          ("WEB_API_MIGRATION_DATABASE_PASSWORD", "owner-secret")
        ]
        `shouldBe` Left (InvalidConfigValue "WEB_API_MIGRATION_DATABASE_PORT" "0")

  describe "loadAppSetupConfigWithFiles" $ do
    it "loads the documented .env then .env.local layers for setup config" $
      withSystemTempDirectory "app-setup-config" $ \tempDirectory ->
        withClearedAppEnvironment $
          withClearedRuntimeEnvironment $
            withClearedSetupEnvironment $ do
              let envPath = tempDirectory <> "/.env"
                  envLocalPath = tempDirectory <> "/.env.local"
              writeFile envPath "APP_TITLE_PREFIX=web-api-shared\nSETUP_AUTOSTART_DATABASE=true\n"
              writeFile envLocalPath "APP_TITLE_PREFIX=web-api-local\nSETUP_AUTOSTART_JAEGER=yes\n"
              loadAppSetupConfigWithFiles envPath envLocalPath
                `shouldReturn` Right
                  AppSetupConfig
                    { setupEnvironmentConfig = defaultAppEnvironmentConfig,
                      setupAppConfig =
                        defaultAppConfig
                          { appTitlePrefix = "web-api-local"
                          },
                      setupMigrationDatabaseConfig = Nothing,
                      setupAutostartConfig =
                        SetupAutostartConfig
                          { setupAutostartDatabase = True,
                            setupAutostartJaeger = True
                          }
                    }

    it "lets process environment override .env.local values for setup config" $
      withSystemTempDirectory "app-setup-config-env" $ \tempDirectory ->
        withClearedAppEnvironment $
          withClearedRuntimeEnvironment $
            withClearedSetupEnvironment $
              withTemporaryEnvironment "APP_TITLE_PREFIX" (Just "web-api-runtime") $
                withTemporaryEnvironment "SETUP_AUTOSTART_DATABASE" (Just "false") $
                  withTemporaryEnvironment "SETUP_AUTOSTART_JAEGER" (Just "true") $ do
                    let envPath = tempDirectory <> "/.env"
                        envLocalPath = tempDirectory <> "/.env.local"
                    writeFile envPath "APP_TITLE_PREFIX=web-api-shared\nSETUP_AUTOSTART_DATABASE=true\n"
                    writeFile envLocalPath "APP_TITLE_PREFIX=web-api-local\nSETUP_AUTOSTART_JAEGER=no\n"
                    loadAppSetupConfigWithFiles envPath envLocalPath
                      `shouldReturn` Right
                        AppSetupConfig
                          { setupEnvironmentConfig = defaultAppEnvironmentConfig,
                            setupAppConfig =
                              defaultAppConfig
                                { appTitlePrefix = "web-api-runtime"
                                },
                            setupMigrationDatabaseConfig = Nothing,
                            setupAutostartConfig =
                              SetupAutostartConfig
                                { setupAutostartDatabase = False,
                                  setupAutostartJaeger = True
                                }
                          }

    it "loads optional migration-owner credentials from the same file layers without replacing runtime credentials" $
      withSystemTempDirectory "app-setup-config-migration" $ \tempDirectory ->
        withClearedAppEnvironment $
          withClearedRuntimeEnvironment $
            withClearedSetupEnvironment $ do
              let envPath = tempDirectory <> "/.env"
                  envLocalPath = tempDirectory <> "/.env.local"
              writeFile
                envPath
                ( unlines
                    [ "DATABASE_USER=web_api_runtime",
                      "WEB_API_MIGRATION_DATABASE_HOST=127.0.0.1",
                      "WEB_API_MIGRATION_DATABASE_PORT=5432",
                      "WEB_API_MIGRATION_DATABASE_NAME=web_api_dev",
                      "WEB_API_MIGRATION_DATABASE_USER=web_api_owner"
                    ]
                )
              writeFile envLocalPath "WEB_API_MIGRATION_DATABASE_PASSWORD=owner-secret\n"
              loadAppSetupConfigWithFiles envPath envLocalPath
                `shouldReturn` Right
                  AppSetupConfig
                    { setupEnvironmentConfig =
                        defaultAppEnvironmentConfig
                          { databaseConfig =
                              DatabaseConfig
                                { databaseHost = "127.0.0.1",
                                  databasePort = 5432,
                                  databaseName = "web_api_dev",
                                  databaseUser = "web_api_runtime",
                                  databasePassword = "web_api"
                                }
                          },
                      setupAppConfig = defaultAppConfig,
                      setupMigrationDatabaseConfig =
                        Just
                          DatabaseConfig
                            { databaseHost = "127.0.0.1",
                              databasePort = 5432,
                              databaseName = "web_api_dev",
                              databaseUser = "web_api_owner",
                              databasePassword = "owner-secret"
                            },
                      setupAutostartConfig = defaultSetupAutostartConfig
                    }

    it "reports invalid override files or parse failures with explicit errors" $
      withSystemTempDirectory "app-setup-config-errors" $ \tempDirectory ->
        withClearedAppEnvironment $
          withClearedRuntimeEnvironment $
            withClearedSetupEnvironment $ do
              let brokenEnvPath = tempDirectory <> "/broken.env"
                  envLocalPath = tempDirectory <> "/.env.local"
                  invalidEnvPath = tempDirectory <> "/invalid.env"
              writeFile brokenEnvPath "SETUP_AUTOSTART_DATABASE\n"
              loadAppSetupConfigWithFiles brokenEnvPath envLocalPath
                `shouldReturn` Left
                  (AppSetupOverridesFileError brokenEnvPath (InvalidConfigOverridesLine 1 "SETUP_AUTOSTART_DATABASE"))
              writeFile invalidEnvPath "SETUP_AUTOSTART_JAEGER=maybe\n"
              loadAppSetupConfigWithFiles invalidEnvPath envLocalPath
                `shouldReturn` Left
                  (AppSetupConfigParseError (InvalidConfigValue "SETUP_AUTOSTART_JAEGER" "maybe"))

    it "reports unreadable override files with the failing path" $
      withSystemTempDirectory "app-setup-config-unreadable" $ \tempDirectory ->
        withClearedAppEnvironment $
          withClearedRuntimeEnvironment $
            withClearedSetupEnvironment $ do
              let envPath = tempDirectory <> "/.env"
                  envLocalPath = tempDirectory <> "/.env.local"
              writeFile envPath "APP_TITLE_PREFIX=web-api-shared\nSETUP_AUTOSTART_DATABASE=true\n"
              withUnreadableFile envLocalPath "APP_TITLE_PREFIX=web-api-local\nSETUP_AUTOSTART_JAEGER=yes\n" $ do
                result <- loadAppSetupConfigWithFiles envPath envLocalPath
                result `shouldSatisfy` \case
                  Left
                    (AppSetupOverridesFileError failingPath (UnreadableConfigOverridesFile errorMessage))
                      | failingPath == envLocalPath ->
                          not (Text.null errorMessage)
                  _ -> False

  describe "loadAppSetupConfig" $
    it "loads the default .env file names for setup config from the current directory" $
      withSystemTempDirectory "app-setup-config-current-directory" $ \tempDirectory ->
        withClearedAppEnvironment $
          withClearedRuntimeEnvironment $
            withClearedSetupEnvironment $ do
              writeFile (tempDirectory <> "/.env") "SETUP_AUTOSTART_DATABASE=true\n"
              writeFile (tempDirectory <> "/.env.local") "APP_TITLE_PREFIX=web-api-dev\nSETUP_AUTOSTART_JAEGER=true\n"
              withCurrentDirectory tempDirectory $
                loadAppSetupConfig
                  `shouldReturn` Right
                    AppSetupConfig
                      { setupEnvironmentConfig = defaultAppEnvironmentConfig,
                        setupAppConfig =
                          defaultAppConfig
                            { appTitlePrefix = "web-api-dev"
                            },
                        setupMigrationDatabaseConfig = Nothing,
                        setupAutostartConfig =
                          SetupAutostartConfig
                            { setupAutostartDatabase = True,
                              setupAutostartJaeger = True
                            }
                      }

  describe "AppSetupConfig and AppSetupConfigLoadError" $
    it "keep selectors, equality, and rendering deterministic" $ do
      let setupConfig =
            AppSetupConfig
              { setupEnvironmentConfig = defaultAppEnvironmentConfig {appMode = Test},
                setupAppConfig = defaultAppConfig {appTitlePrefix = "setup-app"},
                setupMigrationDatabaseConfig =
                  Just
                    DatabaseConfig
                      { databaseHost = "127.0.0.1",
                        databasePort = 5432,
                        databaseName = "web_api_dev",
                        databaseUser = "web_api_owner",
                        databasePassword = "owner-secret"
                      },
                setupAutostartConfig =
                  SetupAutostartConfig
                    { setupAutostartDatabase = True,
                      setupAutostartJaeger = False
                    }
              }
          fileLoadError = AppSetupOverridesFileError ".env" (InvalidConfigOverridesLine 1 "BROKEN")
          parseLoadError = AppSetupConfigParseError (InvalidConfigValue "SETUP_AUTOSTART_DATABASE" "maybe")
      setupEnvironmentConfig setupConfig `shouldBe` defaultAppEnvironmentConfig {appMode = Test}
      setupAppConfig setupConfig `shouldBe` defaultAppConfig {appTitlePrefix = "setup-app"}
      setupMigrationDatabaseConfig setupConfig
        `shouldBe` Just
          DatabaseConfig
            { databaseHost = "127.0.0.1",
              databasePort = 5432,
              databaseName = "web_api_dev",
              databaseUser = "web_api_owner",
              databasePassword = "owner-secret"
            }
      setupAutostartConfig setupConfig
        `shouldBe` SetupAutostartConfig
          { setupAutostartDatabase = True,
            setupAutostartJaeger = False
          }
      setupAutostartDatabase (setupAutostartConfig setupConfig) `shouldBe` True
      setupAutostartJaeger (setupAutostartConfig setupConfig) `shouldBe` False
      defaultSetupAutostartConfig `shouldBe` defaultSetupAutostartConfig
      defaultSetupAutostartConfig
        `shouldNotBe` SetupAutostartConfig
          { setupAutostartDatabase = False,
            setupAutostartJaeger = False
          }
      show defaultSetupAutostartConfig
        `shouldBe` "SetupAutostartConfig {setupAutostartDatabase = True, setupAutostartJaeger = False}"
      showsPrec 11 defaultSetupAutostartConfig ""
        `shouldBe` "(SetupAutostartConfig {setupAutostartDatabase = True, setupAutostartJaeger = False})"
      show [defaultSetupAutostartConfig]
        `shouldBe` "[SetupAutostartConfig {setupAutostartDatabase = True, setupAutostartJaeger = False}]"
      setupConfig `shouldBe` setupConfig
      setupConfig
        `shouldNotBe` setupConfig
          { setupAutostartConfig =
              SetupAutostartConfig
                { setupAutostartDatabase = False,
                  setupAutostartJaeger = False
                }
          }
      show setupConfig
        `shouldContain` ("staticAssetContentTypes = " <> show defaultStaticAssetContentTypes)
      showsPrec 11 setupConfig ""
        `shouldContain` ("staticAssetContentTypes = " <> show defaultStaticAssetContentTypes)
      show [setupConfig]
        `shouldContain` ("staticAssetContentTypes = " <> show defaultStaticAssetContentTypes)
      fileLoadError `shouldBe` fileLoadError
      fileLoadError `shouldNotBe` parseLoadError
      show fileLoadError
        `shouldBe` "AppSetupOverridesFileError \".env\" (InvalidConfigOverridesLine 1 \"BROKEN\")"
      show parseLoadError
        `shouldBe` "AppSetupConfigParseError (InvalidConfigValue \"SETUP_AUTOSTART_DATABASE\" \"maybe\")"
      show [fileLoadError, parseLoadError]
        `shouldBe` "[AppSetupOverridesFileError \".env\" (InvalidConfigOverridesLine 1 \"BROKEN\"),AppSetupConfigParseError (InvalidConfigValue \"SETUP_AUTOSTART_DATABASE\" \"maybe\")]"

  describe "planAppPrerequisites" $ do
    it "preserves runtime database identity fields in the shared setup prerequisite config" $ do
      let setupConfig =
            defaultAppSetupConfig
              { setupEnvironmentConfig =
                  defaultAppEnvironmentConfig
                    { databaseConfig =
                        DatabaseConfig
                          { databaseHost = "db.internal",
                            databasePort = 6543,
                            databaseName = "web_api_build",
                            databaseUser = "web_api_runtime",
                            databasePassword = "secret"
                          }
                    }
              }
          prerequisiteConfig = toSetupPrerequisiteConfig setupConfig
      PrerequisiteConfig.setupDatabaseEndpoint prerequisiteConfig
        `shouldBe` TcpEndpoint
          { tcpEndpointHost = "db.internal",
            tcpEndpointPort = 6543
          }
      PrerequisiteConfig.setupDatabaseName prerequisiteConfig `shouldBe` "web_api_build"
      PrerequisiteConfig.setupDatabaseUser prerequisiteConfig `shouldBe` "web_api_runtime"
      PrerequisiteConfig.setupDatabasePassword prerequisiteConfig `shouldBe` "secret"

    it "always plans the configured database reachability check and skips disabled autostarts" $ do
      let setupConfig =
            defaultAppSetupConfig
              { setupEnvironmentConfig =
                  defaultAppEnvironmentConfig
                    { databaseConfig =
                        DatabaseConfig
                          { databaseHost = "db.internal",
                            databasePort = 6543,
                            databaseName = "web_api_build",
                            databaseUser = "web_api_runtime",
                            databasePassword = "secret"
                          }
                    },
                setupAutostartConfig =
                  defaultSetupAutostartConfig
                    { setupAutostartDatabase = False
                    }
              }
      planAppPrerequisites setupConfig
        `shouldBe` AppPrerequisitePlan
          { databasePrerequisitePlan =
              DatabasePrerequisitePlan
                { databaseCheckEndpoint =
                    TcpEndpoint
                      { tcpEndpointHost = "db.internal",
                        tcpEndpointPort = 6543
                      },
                  databaseAutostartPlan = Nothing
                },
            tracingPrerequisitePlan = Nothing
          }

    it "still plans tracing reachability when tracing is configured but Jaeger autostart stays disabled" $ do
      let tracing =
            OtlpExporter
              { otlpEndpoint = "http://127.0.0.1:4318",
                otlpHeaders = []
              }
          setupConfig =
            defaultAppSetupConfig
              { setupAppConfig =
                  defaultAppConfig
                    { observability =
                        ObservabilityConfig
                          { tracingExporter = Just tracing,
                            metricsExporter = Nothing
                          }
                    },
                setupAutostartConfig =
                  defaultSetupAutostartConfig
                    { setupAutostartDatabase = True
                    }
              }
      planAppPrerequisites setupConfig
        `shouldBe` AppPrerequisitePlan
          { databasePrerequisitePlan =
              DatabasePrerequisitePlan
                { databaseCheckEndpoint =
                    TcpEndpoint
                      { tcpEndpointHost = "127.0.0.1",
                        tcpEndpointPort = 5432
                      },
                  databaseAutostartPlan = Just defaultContainerAutostartPlan
                },
            tracingPrerequisitePlan =
              Just
                TracingPrerequisitePlan
                  { tracingCheckEndpoint = "http://127.0.0.1:4318",
                    tracingAutostartPlan = Nothing
                  }
          }

    it "plans podman-then-docker autostart for database and tracing when enabled" $ do
      let tracing =
            OtlpExporter
              { otlpEndpoint = "http://127.0.0.1:4318",
                otlpHeaders = [("authorization", "Bearer token")]
              }
          setupConfig =
            defaultAppSetupConfig
              { setupAppConfig =
                  defaultAppConfig
                    { observability =
                        ObservabilityConfig
                          { tracingExporter = Just tracing,
                            metricsExporter = Nothing
                          }
                    },
                setupAutostartConfig =
                  SetupAutostartConfig
                    { setupAutostartDatabase = True,
                      setupAutostartJaeger = True
                    }
              }
      planAppPrerequisites setupConfig
        `shouldBe` AppPrerequisitePlan
          { databasePrerequisitePlan =
              DatabasePrerequisitePlan
                { databaseCheckEndpoint =
                    TcpEndpoint
                      { tcpEndpointHost = "127.0.0.1",
                        tcpEndpointPort = 5432
                      },
                  databaseAutostartPlan = Just defaultContainerAutostartPlan
                },
            tracingPrerequisitePlan =
              Just
                TracingPrerequisitePlan
                  { tracingCheckEndpoint = "http://127.0.0.1:4318",
                    tracingAutostartPlan = Just defaultContainerAutostartPlan
                  }
          }

    it "keeps planner model selectors, equality, and rendering deterministic" $ do
      let databaseEndpoint =
            TcpEndpoint
              { tcpEndpointHost = "db.internal",
                tcpEndpointPort = 6543
              }
          databasePlan =
            DatabasePrerequisitePlan
              { databaseCheckEndpoint = databaseEndpoint,
                databaseAutostartPlan = Just defaultContainerAutostartPlan
              }
          tracingPlan =
            TracingPrerequisitePlan
              { tracingCheckEndpoint = "http://127.0.0.1:4318",
                tracingAutostartPlan = Nothing
              }
          appPlan =
            AppPrerequisitePlan
              { databasePrerequisitePlan = databasePlan,
                tracingPrerequisitePlan = Just tracingPlan
              }
      PodmanRuntime `shouldBe` PodmanRuntime
      PodmanRuntime `shouldNotBe` DockerRuntime
      show PodmanRuntime `shouldBe` "PodmanRuntime"
      show [PodmanRuntime, DockerRuntime] `shouldBe` "[PodmanRuntime,DockerRuntime]"
      autostartRuntimes defaultContainerAutostartPlan
        `shouldBe` [PodmanRuntime, DockerRuntime]
      defaultContainerAutostartPlan `shouldBe` defaultContainerAutostartPlan
      defaultContainerAutostartPlan
        `shouldNotBe` ContainerAutostartPlan {autostartRuntimes = [DockerRuntime]}
      show defaultContainerAutostartPlan
        `shouldBe` "ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]}"
      databaseEndpoint `shouldBe` databaseEndpoint
      databaseEndpoint
        `shouldNotBe` TcpEndpoint
          { tcpEndpointHost = "db.other",
            tcpEndpointPort = 6543
          }
      show databaseEndpoint
        `shouldBe` "TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543}"
      showsPrec 11 databaseEndpoint ""
        `shouldBe` "(TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543})"
      show [databaseEndpoint]
        `shouldBe` "[TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543}]"
      databaseCheckEndpoint databasePlan
        `shouldBe` TcpEndpoint
          { tcpEndpointHost = "db.internal",
            tcpEndpointPort = 6543
          }
      databasePlan `shouldBe` databasePlan
      databasePlan
        `shouldNotBe` databasePlan
          { databaseAutostartPlan = Nothing
          }
      show databasePlan
        `shouldBe` "DatabasePrerequisitePlan {databaseCheckEndpoint = TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543}, databaseAutostartPlan = Just (ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]})}"
      showsPrec 11 databasePlan ""
        `shouldBe` "(DatabasePrerequisitePlan {databaseCheckEndpoint = TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543}, databaseAutostartPlan = Just (ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]})})"
      databaseAutostartPlan databasePlan `shouldBe` Just defaultContainerAutostartPlan
      tracingPlan `shouldBe` tracingPlan
      tracingPlan
        `shouldNotBe` tracingPlan
          { tracingCheckEndpoint = "http://127.0.0.1:9999"
          }
      tracingCheckEndpoint tracingPlan `shouldBe` "http://127.0.0.1:4318"
      tracingAutostartPlan tracingPlan `shouldBe` Nothing
      show tracingPlan
        `shouldBe` "TracingPrerequisitePlan {tracingCheckEndpoint = \"http://127.0.0.1:4318\", tracingAutostartPlan = Nothing}"
      showsPrec 11 tracingPlan ""
        `shouldBe` "(TracingPrerequisitePlan {tracingCheckEndpoint = \"http://127.0.0.1:4318\", tracingAutostartPlan = Nothing})"
      databasePrerequisitePlan appPlan `shouldBe` databasePlan
      tracingPrerequisitePlan appPlan `shouldBe` Just tracingPlan
      appPlan `shouldBe` appPlan
      appPlan
        `shouldNotBe` appPlan
          { tracingPrerequisitePlan = Nothing
          }
      show appPlan
        `shouldBe` "AppPrerequisitePlan {databasePrerequisitePlan = DatabasePrerequisitePlan {databaseCheckEndpoint = TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543}, databaseAutostartPlan = Just (ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]})}, tracingPrerequisitePlan = Just (TracingPrerequisitePlan {tracingCheckEndpoint = \"http://127.0.0.1:4318\", tracingAutostartPlan = Nothing})}"
      showsPrec 11 appPlan ""
        `shouldBe` "(AppPrerequisitePlan {databasePrerequisitePlan = DatabasePrerequisitePlan {databaseCheckEndpoint = TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543}, databaseAutostartPlan = Just (ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]})}, tracingPrerequisitePlan = Just (TracingPrerequisitePlan {tracingCheckEndpoint = \"http://127.0.0.1:4318\", tracingAutostartPlan = Nothing})})"
      show [defaultContainerAutostartPlan]
        `shouldBe` "[ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]}]"
      show [databasePlan]
        `shouldBe` "[DatabasePrerequisitePlan {databaseCheckEndpoint = TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543}, databaseAutostartPlan = Just (ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]})}]"
      show [tracingPlan]
        `shouldBe` "[TracingPrerequisitePlan {tracingCheckEndpoint = \"http://127.0.0.1:4318\", tracingAutostartPlan = Nothing}]"
      show [appPlan]
        `shouldBe` "[AppPrerequisitePlan {databasePrerequisitePlan = DatabasePrerequisitePlan {databaseCheckEndpoint = TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543}, databaseAutostartPlan = Just (ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]})}, tracingPrerequisitePlan = Just (TracingPrerequisitePlan {tracingCheckEndpoint = \"http://127.0.0.1:4318\", tracingAutostartPlan = Nothing})}]"

  describe "parseTracingEndpoint" $ do
    it "parses supported tracing URLs into TCP endpoints" $ do
      parseTracingEndpoint "http://collector:4318/v1/traces"
        `shouldBe` Right
          TcpEndpoint
            { tcpEndpointHost = "collector",
              tcpEndpointPort = 4318
            }
      parseTracingEndpoint "https://collector.example/v1/traces"
        `shouldBe` Right
          TcpEndpoint
            { tcpEndpointHost = "collector.example",
              tcpEndpointPort = 443
            }
      parseTracingEndpoint "http://[::1]:4318/v1/traces"
        `shouldBe` Right
          TcpEndpoint
            { tcpEndpointHost = "::1",
              tcpEndpointPort = 4318
            }
      parseTracingEndpoint "https://[::1]/v1/traces"
        `shouldBe` Right
          TcpEndpoint
            { tcpEndpointHost = "::1",
              tcpEndpointPort = 443
            }
      parseTracingEndpoint "https://collector/v1/traces"
        `shouldBe` Right
          TcpEndpoint
            { tcpEndpointHost = "collector",
              tcpEndpointPort = 443
            }

    it "rejects malformed or unsupported tracing endpoints explicitly" $ do
      parseTracingEndpoint "://collector:4318/v1/traces"
        `shouldBe` Left (InvalidTracingEndpointFormat "://collector:4318/v1/traces")
      parseTracingEndpoint "collector:4318/v1/traces"
        `shouldBe` Left (InvalidTracingEndpointFormat "collector:4318/v1/traces")
      parseTracingEndpoint "grpc://collector:4317"
        `shouldBe` Left (UnsupportedTracingEndpointScheme "grpc")
      parseTracingEndpoint "http:///v1/traces"
        `shouldBe` Left MissingTracingEndpointHost
      parseTracingEndpoint "http://:4318/v1/traces"
        `shouldBe` Left MissingTracingEndpointHost
      parseTracingEndpoint "http://collector:not-a-port/v1/traces"
        `shouldBe` Left (InvalidTracingEndpointPort "not-a-port")
      parseTracingEndpoint "http://collector:0/v1/traces"
        `shouldBe` Left (InvalidTracingEndpointPort "0")
      parseTracingEndpoint "http://[::1/v1/traces"
        `shouldBe` Left MissingTracingEndpointHost
      parseTracingEndpoint "http://[]:4318/v1/traces"
        `shouldBe` Left MissingTracingEndpointHost
      parseTracingEndpoint "http://[::1]suffix/v1/traces"
        `shouldBe` Left (InvalidTracingEndpointFormat "suffix")

    it "keeps parse error equality and rendering deterministic" $ do
      let parseError = InvalidTracingEndpointPort "not-a-port"
      parseError `shouldBe` InvalidTracingEndpointPort "not-a-port"
      parseError `shouldNotBe` MissingTracingEndpointHost
      show parseError `shouldBe` "InvalidTracingEndpointPort \"not-a-port\""
      show [parseError] `shouldBe` "[InvalidTracingEndpointPort \"not-a-port\"]"

  describe "checkTcpEndpointReachable" $ do
    it "reports True for a reachable local TCP listener" $
      withListeningTcpEndpoint $ \tcpEndpoint ->
        checkTcpEndpointReachable tcpEndpoint
          `shouldReturn` True

    it "reports False once the TCP listener is gone" $ do
      closedEndpoint <- withListeningTcpEndpoint pure
      checkTcpEndpointReachable closedEndpoint
        `shouldReturn` False

    it "reports False for invalid resolver inputs or immediate timeout cutoffs" $
      withListeningTcpEndpoint $ \tcpEndpoint -> do
        checkTcpEndpointReachableWithTimeout
          1000000
          TcpEndpoint
            { tcpEndpointHost = tcpEndpointHost tcpEndpoint,
              tcpEndpointPort = -1
            }
          `shouldReturn` False
        checkTcpEndpointReachableWithTimeout 0 tcpEndpoint
          `shouldReturn` False

  describe "checkTracingEndpointReachable" $ do
    it "checks supported tracing endpoints by their parsed TCP host and port" $
      withListeningTcpEndpoint $ \tcpEndpoint -> do
        let endpoint =
              "http://"
                <> tcpEndpointHost tcpEndpoint
                <> ":"
                <> Text.pack (show (tcpEndpointPort tcpEndpoint))
                <> "/v1/traces"
        checkTracingEndpointReachable endpoint
          `shouldReturn` Right True

    it "returns parse errors instead of silently treating malformed tracing endpoints as unreachable" $
      checkTracingEndpointReachable "collector:4318/v1/traces"
        `shouldReturn` Left (InvalidTracingEndpointFormat "collector:4318/v1/traces")

  describe "parseDatabaseSetupCommand" $ do
    it "accepts migrate, seed, and migrate-and-seed" $ do
      parseDatabaseSetupCommand ["migrate"] `shouldBe` Right MigrateDatabase
      parseDatabaseSetupCommand ["seed"] `shouldBe` Right SeedDatabase
      parseDatabaseSetupCommand ["migrate-and-seed"] `shouldBe` Right MigrateAndSeedDatabase

    it "rejects unsupported command lines with explicit guidance" $ do
      parseDatabaseSetupCommand ["deploy"]
        `shouldBe` Left (InvalidDatabaseSetupCommand ["deploy"])
      renderDatabaseSetupError (InvalidDatabaseSetupCommand ["deploy"])
        `shouldBe` "Unsupported database setup command: deploy\nExpected one of: migrate, seed, migrate-and-seed"

    it "keeps command and error values stable" $ do
      let loadError = InvalidConfigValue "WEB_API_MIGRATION_DATABASE_PORT" "0"
          runtimeLoadError = MissingConfigValue "DATABASE_PASSWORD"
          configSetupError = DatabaseSetupConfigLoadError loadError
          runtimeConfigSetupError = DatabaseSetupRuntimeConfigLoadError runtimeLoadError
          migrationSetupError = DatabaseSetupMigrationError (UnexpectedQueryRows "expected exactly one row" ["first", "second"])
          seedSetupError = DatabaseSetupSeedError (UnexpectedQueryRows "expected exactly one row" ["seed"])
      MigrateDatabase `shouldBe` MigrateDatabase
      MigrateDatabase `shouldNotBe` SeedDatabase
      show MigrateDatabase `shouldBe` "MigrateDatabase"
      show SeedDatabase `shouldBe` "SeedDatabase"
      show MigrateAndSeedDatabase `shouldBe` "MigrateAndSeedDatabase"
      show [MigrateDatabase, SeedDatabase, MigrateAndSeedDatabase]
        `shouldBe` "[MigrateDatabase,SeedDatabase,MigrateAndSeedDatabase]"
      configSetupError `shouldBe` configSetupError
      configSetupError `shouldNotBe` migrationSetupError
      runtimeConfigSetupError `shouldBe` runtimeConfigSetupError
      runtimeConfigSetupError `shouldNotBe` configSetupError
      seedSetupError `shouldBe` seedSetupError
      show configSetupError
        `shouldBe` "DatabaseSetupConfigLoadError (InvalidConfigValue \"WEB_API_MIGRATION_DATABASE_PORT\" \"0\")"
      show runtimeConfigSetupError
        `shouldBe` "DatabaseSetupRuntimeConfigLoadError (MissingConfigValue \"DATABASE_PASSWORD\")"
      show migrationSetupError
        `shouldBe` "DatabaseSetupMigrationError (UnexpectedQueryRows \"expected exactly one row\" [\"first\",\"second\"])"
      show seedSetupError
        `shouldBe` "DatabaseSetupSeedError (UnexpectedQueryRows \"expected exactly one row\" [\"seed\"])"
      show [configSetupError]
        `shouldBe` "[DatabaseSetupConfigLoadError (InvalidConfigValue \"WEB_API_MIGRATION_DATABASE_PORT\" \"0\")]"

    it "renders load, migration, and seed failures explicitly" $ do
      let loadError = InvalidConfigValue "WEB_API_MIGRATION_DATABASE_PORT" "0"
          runtimeLoadError = MissingConfigValue "DATABASE_PASSWORD"
          migrationRunnerError = UnexpectedQueryRows "expected exactly one row" ["first", "second"]
          seedRunnerError = UnexpectedQueryRows "expected exactly one row" ["seed"]
      renderDatabaseSetupError (DatabaseSetupConfigLoadError loadError)
        `shouldBe` "Failed to load database setup config: InvalidConfigValue \"WEB_API_MIGRATION_DATABASE_PORT\" \"0\""
      renderDatabaseSetupError (DatabaseSetupRuntimeConfigLoadError runtimeLoadError)
        `shouldBe` "Failed to load runtime database config: MissingConfigValue \"DATABASE_PASSWORD\""
      renderDatabaseSetupError (DatabaseSetupMigrationError migrationRunnerError)
        `shouldBe` "Failed to apply database migrations: UnexpectedQueryRows \"expected exactly one row\" [\"first\",\"second\"]"
      renderDatabaseSetupError (DatabaseSetupSeedError seedRunnerError)
        `shouldBe` "Failed to apply database seed data: UnexpectedQueryRows \"expected exactly one row\" [\"seed\"]"

  describe "parseDatabaseSetupConfig" $ do
    it "reads owner-level migration credentials from dedicated environment variables" $
      parseDatabaseSetupConfig
        [ ("WEB_API_MIGRATION_DATABASE_HOST", "127.0.0.1"),
          ("WEB_API_MIGRATION_DATABASE_PORT", "5432"),
          ("WEB_API_MIGRATION_DATABASE_NAME", "web_api_dev"),
          ("WEB_API_MIGRATION_DATABASE_USER", "web_api_owner"),
          ("WEB_API_MIGRATION_DATABASE_PASSWORD", "owner-secret")
        ]
        `shouldBe` Right
          DatabaseConfig
            { databaseHost = "127.0.0.1",
              databasePort = 5432,
              databaseName = "web_api_dev",
              databaseUser = "web_api_owner",
              databasePassword = "owner-secret"
            }

    it "fails missing or invalid migration environment values explicitly" $ do
      parseDatabaseSetupConfig
        [ ("WEB_API_MIGRATION_DATABASE_HOST", "127.0.0.1"),
          ("WEB_API_MIGRATION_DATABASE_PORT", "5432"),
          ("WEB_API_MIGRATION_DATABASE_NAME", "web_api_dev"),
          ("WEB_API_MIGRATION_DATABASE_USER", "web_api_owner")
        ]
        `shouldBe` Left (MissingConfigValue "WEB_API_MIGRATION_DATABASE_PASSWORD")
      parseDatabaseSetupConfig
        [ ("WEB_API_MIGRATION_DATABASE_HOST", "127.0.0.1"),
          ("WEB_API_MIGRATION_DATABASE_PORT", "0"),
          ("WEB_API_MIGRATION_DATABASE_NAME", "web_api_dev"),
          ("WEB_API_MIGRATION_DATABASE_USER", "web_api_owner"),
          ("WEB_API_MIGRATION_DATABASE_PASSWORD", "owner-secret")
        ]
        `shouldBe` Left (InvalidConfigValue "WEB_API_MIGRATION_DATABASE_PORT" "0")

  describe "loadDatabaseSetupConfig" $
    it "reads dedicated migration credentials from the process environment" $
      withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_HOST" (Just "127.0.0.1") $
        withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_PORT" (Just "5432") $
          withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_NAME" (Just "web_api_dev") $
            withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_USER" (Just "web_api_owner") $
              withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_PASSWORD" (Just "owner-secret") $
                loadDatabaseSetupConfig
                  `shouldReturn` Right
                    DatabaseConfig
                      { databaseHost = "127.0.0.1",
                        databasePort = 5432,
                        databaseName = "web_api_dev",
                        databaseUser = "web_api_owner",
                        databasePassword = "owner-secret"
                      }

  describe "runDatabaseSetupCommand"
    $ it "uses the default migration environment loader and postgres runners for single-command setup"
    $ withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_HOST" (Just "127.0.0.1")
    $ withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_PORT" (Just "5432")
    $ withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_NAME" (Just "web_api_dev")
    $ withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_USER" (Just "web_api_owner")
    $ withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_PASSWORD" (Just "owner-secret")
    $ withTemporaryEnvironment "DATABASE_HOST" (Just "127.0.0.1")
    $ withTemporaryEnvironment "DATABASE_PORT" (Just "5432")
    $ withTemporaryEnvironment "DATABASE_NAME" (Just "web_api_dev")
    $ withTemporaryEnvironment "DATABASE_USER" (Just "web_api_runtime")
    $ withTemporaryEnvironment "DATABASE_PASSWORD" (Just "runtime-secret")
    $ withFakePsqlScript
      (fmap (,Text.empty) (migrationStatementsFor setupMigrationPostgresTestConfig runtimeSetupPostgresTestConfig <> seedStatements))
    $ \argsLogPath -> do
      runDatabaseSetupCommand MigrateDatabase `shouldReturn` Right ()
      runDatabaseSetupCommand SeedDatabase `shouldReturn` Right ()
      let renderMutationLogEntry databaseConfig sql =
            "--host "
              <> Text.unpack (databaseHost databaseConfig)
              <> " --port "
              <> show (databasePort databaseConfig)
              <> " --dbname "
              <> Text.unpack (databaseName databaseConfig)
              <> " --username "
              <> Text.unpack (databaseUser databaseConfig)
              <> " --no-password --set ON_ERROR_STOP=1 --command "
              <> Text.unpack sql
      readFile argsLogPath
        `shouldReturn` unlines
          ( fmap (renderMutationLogEntry setupMigrationPostgresTestConfig) (migrationStatementsFor setupMigrationPostgresTestConfig runtimeSetupPostgresTestConfig)
              <> fmap (renderMutationLogEntry setupMigrationPostgresTestConfig) seedStatements
          )

  describe "runDatabaseSetupCommandWith" $ do
    it "returns configuration load errors before running any commands" $ do
      recordedStepsReference <- newIORef ([] :: [Text])
      let loadError = InvalidConfigValue "WEB_API_MIGRATION_DATABASE_PORT" "0"
          unexpectedRuntimeLoader =
            modifyIORef' recordedStepsReference (<> ["runtime-loader"])
              >> pure (Right postgresTestConfig)
          unexpectedMigrationRunner _ _ =
            modifyIORef' recordedStepsReference (<> ["runner"])
              >> pure (Right ())
      runDatabaseSetupCommandWith
        (pure (Left loadError))
        unexpectedRuntimeLoader
        unexpectedMigrationRunner
        (\_ -> pure (Right ()))
        MigrateDatabase
        `shouldReturn` Left (DatabaseSetupConfigLoadError loadError)
      readIORef recordedStepsReference `shouldReturn` []

    it "returns runtime configuration load errors before running database commands" $ do
      recordedStepsReference <- newIORef ([] :: [Text])
      let loadError = MissingConfigValue "DATABASE_PASSWORD"
          unexpectedMigrationRunner _ _ =
            modifyIORef' recordedStepsReference (<> ["migrate"])
              >> pure (Right ())
          unexpectedSeedRunner _ =
            modifyIORef' recordedStepsReference (<> ["seed"])
              >> pure (Right ())
      runDatabaseSetupCommandWith
        (pure (Right migrationPostgresTestConfig))
        (pure (Left loadError))
        unexpectedMigrationRunner
        unexpectedSeedRunner
        MigrateDatabase
        `shouldReturn` Left (DatabaseSetupRuntimeConfigLoadError loadError)
      readIORef recordedStepsReference `shouldReturn` []

    it "runs migrations and seed data in order with the loaded database config" $ do
      recordedStepsReference <- newIORef ([] :: [Text])
      let recordMigrationStep migrationDatabaseConfig runtimeDatabaseConfig =
            modifyIORef'
              recordedStepsReference
              (<> ["migrate:" <> databaseUser migrationDatabaseConfig <> "->" <> databaseUser runtimeDatabaseConfig <> ":" <> databaseName runtimeDatabaseConfig])
              >> pure (Right ())
          recordSeedStep databaseRuntimeConfig =
            modifyIORef' recordedStepsReference (<> ["seed:" <> databaseUser databaseRuntimeConfig <> ":" <> databaseName databaseRuntimeConfig])
              >> pure (Right ())
      runDatabaseSetupCommandWith
        (pure (Right migrationPostgresTestConfig))
        (pure (Right postgresTestConfig))
        recordMigrationStep
        recordSeedStep
        MigrateAndSeedDatabase
        `shouldReturn` Right ()
      readIORef recordedStepsReference
        `shouldReturn` ["migrate:web_api_owner->web_api_app:web_api_prod", "seed:web_api_owner:web_api_prod"]

    it "maps single-command migration failures explicitly" $ do
      let migrationError =
            PostgresCommandFailed
              (PostgresCommand {postgresExecutable = "psql", postgresArguments = ["--command", "broken"], postgresEnvironment = []})
              (failingPostgresResult "migration failed")
      runDatabaseSetupCommandWith
        (pure (Right migrationPostgresTestConfig))
        (pure (Right postgresTestConfig))
        (\_ _ -> pure (Left migrationError))
        (\_ -> pure (Right ()))
        MigrateDatabase
        `shouldReturn` Left (DatabaseSetupMigrationError migrationError)

    it "maps single-command seed failures explicitly" $ do
      let seedError =
            PostgresCommandFailed
              (PostgresCommand {postgresExecutable = "psql", postgresArguments = ["--command", "broken-seed"], postgresEnvironment = []})
              (failingPostgresResult "seed failed")
      runDatabaseSetupCommandWith
        (pure (Right migrationPostgresTestConfig))
        (pure (Right postgresTestConfig))
        (\_ _ -> pure (Right ()))
        (\_ -> pure (Left seedError))
        SeedDatabase
        `shouldReturn` Left (DatabaseSetupSeedError seedError)

    it "stops after the first migration failure and preserves the runner error" $ do
      recordedStepsReference <- newIORef ([] :: [Text])
      let migrationError =
            PostgresCommandFailed
              (PostgresCommand {postgresExecutable = "psql", postgresArguments = ["--command", "broken"], postgresEnvironment = []})
              (failingPostgresResult "migration failed")
          failingMigrations _ _ =
            modifyIORef' recordedStepsReference (<> ["migrate"])
              >> pure (Left migrationError)
          unexpectedSeed _ =
            modifyIORef' recordedStepsReference (<> ["seed"])
              >> pure (Right ())
      runDatabaseSetupCommandWith
        (pure (Right migrationPostgresTestConfig))
        (pure (Right postgresTestConfig))
        failingMigrations
        unexpectedSeed
        MigrateAndSeedDatabase
        `shouldReturn` Left (DatabaseSetupMigrationError migrationError)
      readIORef recordedStepsReference `shouldReturn` ["migrate"]

    it "maps migrate-and-seed seed failures explicitly after successful migrations" $ do
      let seedError =
            PostgresCommandFailed
              (PostgresCommand {postgresExecutable = "psql", postgresArguments = ["--command", "broken-seed"], postgresEnvironment = []})
              (failingPostgresResult "seed failed")
      runDatabaseSetupCommandWith
        (pure (Right migrationPostgresTestConfig))
        (pure (Right postgresTestConfig))
        (\_ _ -> pure (Right ()))
        (\_ -> pure (Left seedError))
        MigrateAndSeedDatabase
        `shouldReturn` Left (DatabaseSetupSeedError seedError)

  describe "runDatabaseSetupArgsWith" $ do
    it "prints a success message for completed setup commands" $
      withSystemTempFile "database-setup-stdout.txt" $ \outputPath outputHandle -> do
        runDatabaseSetupArgsWith
          (pure (Right migrationPostgresTestConfig))
          (pure (Right postgresTestConfig))
          (\_ _ -> pure (Right ()))
          (\_ -> pure (Right ()))
          outputHandle
          ["seed"]
        hClose outputHandle
        readFile outputPath `shouldReturn` "Applied database seed data.\n"

    it "throws an explicit user error for unsupported command lines" $
      withSystemTempFile "database-setup-invalid-stdout.txt" $ \_ outputHandle -> do
        result <-
          try
            ( runDatabaseSetupArgsWith
                (pure (Right migrationPostgresTestConfig))
                (pure (Right postgresTestConfig))
                (\_ _ -> pure (Right ()))
                (\_ -> pure (Right ()))
                outputHandle
                ["deploy"]
            ) ::
            IO (Either IOException ())
        hClose outputHandle
        case result of
          Left exception ->
            displayException exception
              `shouldContain` "Unsupported database setup command: deploy"
          Right () ->
            expectationFailure "expected invalid database setup command to raise an exception"

    it "throws an explicit user error when setup returns a failure" $
      withSystemTempFile "database-setup-error-stdout.txt" $ \_ outputHandle -> do
        let loadError = InvalidConfigValue "WEB_API_MIGRATION_DATABASE_PORT" "0"
        result <-
          try
            ( runDatabaseSetupArgsWith
                (pure (Left loadError))
                (pure (Right postgresTestConfig))
                (\_ _ -> pure (Right ()))
                (\_ -> pure (Right ()))
                outputHandle
                ["migrate"]
            ) ::
            IO (Either IOException ())
        hClose outputHandle
        case result of
          Left exception ->
            displayException exception
              `shouldContain` "Failed to load database setup config"
          Right () ->
            expectationFailure "expected database setup failure to raise an exception"

  describe "runDatabaseSetupArgs"
    $ it "uses the default migration environment loader and postgres runners for migrate and migrate-and-seed output"
    $ withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_HOST" (Just "127.0.0.1")
    $ withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_PORT" (Just "5432")
    $ withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_NAME" (Just "web_api_dev")
    $ withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_USER" (Just "web_api_owner")
    $ withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_PASSWORD" (Just "owner-secret")
    $ withTemporaryEnvironment "DATABASE_HOST" (Just "127.0.0.1")
    $ withTemporaryEnvironment "DATABASE_PORT" (Just "5432")
    $ withTemporaryEnvironment "DATABASE_NAME" (Just "web_api_dev")
    $ withTemporaryEnvironment "DATABASE_USER" (Just "web_api_runtime")
    $ withTemporaryEnvironment "DATABASE_PASSWORD" (Just "runtime-secret")
    $ withFakePsqlScript
      (fmap (,Text.empty) (migrationStatementsFor setupMigrationPostgresTestConfig runtimeSetupPostgresTestConfig <> seedStatements))
    $ \_ ->
      withSystemTempFile "database-setup-args-migrate.txt" $ \migrateOutputPath migrateOutputHandle -> do
        runDatabaseSetupArgs migrateOutputHandle ["migrate"]
        hClose migrateOutputHandle
        readFile migrateOutputPath `shouldReturn` "Applied database migrations.\n"
        withSystemTempFile "database-setup-args-migrate-and-seed.txt" $ \migrateAndSeedOutputPath migrateAndSeedOutputHandle -> do
          runDatabaseSetupArgs migrateAndSeedOutputHandle ["migrate-and-seed"]
          hClose migrateAndSeedOutputHandle
          readFile migrateAndSeedOutputPath `shouldReturn` "Applied database migrations and seed data.\n"

  describe "config model values" $ do
    it "can represent manual, shared, and certbot-backed ACME certificates plus exporter endpoints" $ do
      let certbotConfig =
            CertbotConfig
              { certbotExecutable = "certbot",
                certbotArguments = ["certonly", "--webroot"]
              }
          sharedCertificateSource =
            SharedCertificateFiles
              { certificateDirectory = "/var/lib/web-api/shared-certs",
                sharedCertificateStartupMode = AwaitCertificateFiles Nothing
              }
          tlsSource =
            AcmeCertificateSource
              AcmeConfig
                { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                  acmeContactEmails = ["ops@example.com"],
                  acmeDomains = ["example.com", "www.example.com"],
                  acmeHttp01Port = 80,
                  acmeCertificateDirectory = Nothing,
                  acmeCertbotConfig = certbotConfig
                }
          exporter =
            OtlpExporter
              { otlpEndpoint = "http://otel-collector:4318",
                otlpHeaders = [("x-api-key", "secret")]
              }
      TlsConfig {certificateSource = ManualCertificateFiles {certificateFile = "cert.pem", privateKeyFile = "key.pem"}}
        `shouldBe` TlsConfig {certificateSource = ManualCertificateFiles {certificateFile = "cert.pem", privateKeyFile = "key.pem"}}
      show sharedCertificateSource
        `shouldBe` "SharedCertificateFiles {certificateDirectory = \"/var/lib/web-api/shared-certs\", sharedCertificateStartupMode = AwaitCertificateFiles {certificateWaitTimeoutSeconds = Nothing}}"
      show tlsSource
        `shouldBe` "AcmeCertificateSource (AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeDomains = [\"example.com\",\"www.example.com\"], acmeHttp01Port = 80, acmeCertificateDirectory = Nothing, acmeCertbotConfig = CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]}})"
      show exporter
        `shouldBe` "OtlpExporter {otlpEndpoint = \"http://otel-collector:4318\", otlpHeaders = [(\"x-api-key\",\"secret\")]}"

    it "reads exported selectors from the remaining public config and page-model types" $ do
      let manualCertificateSource =
            ManualCertificateFiles
              { certificateFile = "cert.pem",
                privateKeyFile = "key.pem"
              }
          inProcessAcmeConfig =
            AcmeConfig
              { acmeDirectoryUrl = "https://acme-staging-v02.api.letsencrypt.org/directory",
                acmeContactEmails = ["ops@example.com", "alerts@example.com"],
                acmeDomains = ["example.com", "www.example.com"],
                acmeHttp01Port = 80,
                acmeCertificateDirectory = Nothing,
                acmeCertbotConfig =
                  CertbotConfig
                    { certbotExecutable = "certbot",
                      certbotArguments = []
                    }
              }
          sharedCertificateSource =
            SharedCertificateFiles
              { certificateDirectory = "/var/lib/web-api/shared-certs",
                sharedCertificateStartupMode = AwaitCertificateFiles Nothing
              }
          tlsConfig = TlsConfig {certificateSource = manualCertificateSource}
          listenerConfig =
            ListenerConfig
              { listenerHost = "0.0.0.0",
                listenerPort = 5443,
                listenerScheme = Https,
                listenerTls = Just tlsConfig,
                listenerAcme = Nothing
              }
          staticRoot =
            StaticAssetRoot
              { staticUrlPrefix = "/assets",
                staticDirectory = "public"
              }
          staticConfig =
            StaticAssetsConfig
              { staticAssetRoots = [staticRoot],
                staticAssetContentTypes = defaultStaticAssetContentTypes,
                staticCacheControlSeconds = Just 3600
              }
          exporter =
            OtlpExporter
              { otlpEndpoint = "http://otel-collector:4318",
                otlpHeaders = [("authorization", "Bearer token")]
              }
          observabilityConfig =
            ObservabilityConfig
              { tracingExporter = Just exporter,
                metricsExporter = Just exporter
              }
          appConfig =
            AppConfig
              { appTitlePrefix = "test-app",
                listenerConfigs = [listenerConfig],
                staticAssets = staticConfig,
                requestPolicy = requestPolicy defaultAppConfig,
                observability = observabilityConfig
              }
          requestContext =
            AppRequestContext
              { requestLocale = French,
                requestCorrelationId = Just "req-456",
                requestSurface = PageSurface,
                requestPathPrefix = ""
              }
          callToAction =
            CallToAction
              { callToActionLabel = "Return home",
                callToActionRoute = HomeRoute,
                callToActionHref = "/fr"
              }
          notFoundPageModel =
            NotFoundPageModel
              { notFoundHeading = "Not Found",
                notFoundSummary = "The requested page could not be found.",
                notFoundPrimaryAction = callToAction
              }
          homePageModel =
            HomePageModel
              { homeHeading = "Home",
                homeSummary = "Server-rendered home page with stubbed content.",
                homeErrorMessage = Nothing,
                homePrimaryAction =
                  CallToAction
                    { callToActionLabel = "Browse the second page",
                      callToActionRoute = SecondRoute,
                      callToActionHref = "/fr/second"
                    }
              }
          secondPageModel =
            SecondPageModel
              { secondHeading = "Second",
                secondSummary = "Second page content with stubbed data ready for future loaders.",
                secondHighlights = ["Fast SSR", "Progressive enhancement"],
                secondErrorMessage = Nothing,
                secondPrimaryAction = callToAction
              }
      case manualCertificateSource of
        source@ManualCertificateFiles {} -> do
          certificateFile source `shouldBe` "cert.pem"
          privateKeyFile source `shouldBe` "key.pem"
        AcmeCertificateSource _ -> expectationFailure "expected manual certificate files"
        SharedCertificateFiles {} -> expectationFailure "expected manual certificate files"
      acmeDirectoryUrl inProcessAcmeConfig `shouldBe` "https://acme-staging-v02.api.letsencrypt.org/directory"
      acmeContactEmails inProcessAcmeConfig `shouldBe` ["ops@example.com", "alerts@example.com"]
      acmeDomains inProcessAcmeConfig `shouldBe` ["example.com", "www.example.com"]
      acmeHttp01Port inProcessAcmeConfig `shouldBe` 80
      acmeCertificateDirectory inProcessAcmeConfig `shouldBe` Nothing
      acmeCertbotConfig inProcessAcmeConfig
        `shouldBe` CertbotConfig
          { certbotExecutable = "certbot",
            certbotArguments = []
          }
      case sharedCertificateSource of
        SharedCertificateFiles {certificateDirectory = sharedDirectory, sharedCertificateStartupMode = startupMode} -> do
          sharedDirectory `shouldBe` "/var/lib/web-api/shared-certs"
          startupMode `shouldBe` AwaitCertificateFiles Nothing
        _ ->
          expectationFailure "expected shared certificate files"
      certificateSource tlsConfig `shouldBe` manualCertificateSource
      listenerHost listenerConfig `shouldBe` "0.0.0.0"
      listenerPort listenerConfig `shouldBe` 5443
      listenerScheme listenerConfig `shouldBe` Https
      listenerTls listenerConfig `shouldBe` Just tlsConfig
      staticUrlPrefix staticRoot `shouldBe` "/assets"
      staticDirectory staticRoot `shouldBe` "public"
      staticAssetRoots staticConfig `shouldBe` [staticRoot]
      staticCacheControlSeconds staticConfig `shouldBe` Just 3600
      otlpEndpoint exporter `shouldBe` "http://otel-collector:4318"
      otlpHeaders exporter `shouldBe` [("authorization", "Bearer token")]
      tracingExporter observabilityConfig `shouldBe` Just exporter
      metricsExporter observabilityConfig `shouldBe` Just exporter
      appTitlePrefix appConfig `shouldBe` "test-app"
      listenerConfigs appConfig `shouldBe` [listenerConfig]
      staticAssets appConfig `shouldBe` staticConfig
      observability appConfig `shouldBe` observabilityConfig
      requestLocale requestContext `shouldBe` French
      requestCorrelationId requestContext `shouldBe` Just "req-456"
      callToActionLabel callToAction `shouldBe` "Return home"
      callToActionRoute callToAction `shouldBe` HomeRoute
      callToActionHref callToAction `shouldBe` "/fr"
      homeHeading homePageModel `shouldBe` "Home"
      homeSummary homePageModel `shouldBe` "Server-rendered home page with stubbed content."
      homePrimaryAction homePageModel
        `shouldBe` CallToAction
          { callToActionLabel = "Browse the second page",
            callToActionRoute = SecondRoute,
            callToActionHref = "/fr/second"
          }
      secondHeading secondPageModel `shouldBe` "Second"
      secondSummary secondPageModel `shouldBe` "Second page content with stubbed data ready for future loaders."
      secondHighlights secondPageModel `shouldBe` ["Fast SSR", "Progressive enhancement"]
      secondPrimaryAction secondPageModel `shouldBe` callToAction
      notFoundHeading notFoundPageModel `shouldBe` "Not Found"
      notFoundSummary notFoundPageModel `shouldBe` "The requested page could not be found."
      notFoundPrimaryAction notFoundPageModel `shouldBe` callToAction

    it "directly exercises the remaining derived eq and show instances" $ do
      let certbotConfig =
            CertbotConfig
              { certbotExecutable = "certbot",
                certbotArguments = ["certonly", "--webroot"]
              }
          manualCertificateSource =
            ManualCertificateFiles
              { certificateFile = "cert.pem",
                privateKeyFile = "key.pem"
              }
          acmeCertificateSource =
            AcmeCertificateSource
              AcmeConfig
                { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                  acmeContactEmails = ["ops@example.com"],
                  acmeDomains = ["example.com", "www.example.com"],
                  acmeHttp01Port = 80,
                  acmeCertificateDirectory = Nothing,
                  acmeCertbotConfig = certbotConfig
                }
          sharedCertificateSource =
            SharedCertificateFiles
              { certificateDirectory = "/var/lib/web-api/shared-certs",
                sharedCertificateStartupMode = AwaitCertificateFiles Nothing
              }
          staticRoot =
            StaticAssetRoot
              { staticUrlPrefix = "/assets",
                staticDirectory = "public"
              }
          secondPageModel =
            SecondPageModel
              { secondHeading = "Second",
                secondSummary = "Second page content with stubbed data ready for future loaders.",
                secondHighlights = ["Fast SSR"],
                secondErrorMessage = Nothing,
                secondPrimaryAction =
                  CallToAction
                    { callToActionLabel = "Return home",
                      callToActionRoute = HomeRoute,
                      callToActionHref = "/"
                    }
              }
          notFoundPageModel =
            NotFoundPageModel
              { notFoundHeading = "Not Found",
                notFoundSummary = "The requested page could not be found.",
                notFoundPrimaryAction =
                  CallToAction
                    { callToActionLabel = "Return home",
                      callToActionRoute = HomeRoute,
                      callToActionHref = "/"
                    }
              }
          homePageModel =
            HomePageModel
              { homeHeading = "Home",
                homeSummary = "Server-rendered home page with stubbed content.",
                homeErrorMessage = Nothing,
                homePrimaryAction =
                  CallToAction
                    { callToActionLabel = "Browse the second page",
                      callToActionRoute = SecondRoute,
                      callToActionHref = "/second"
                    }
              }
      Http `shouldBe` Http
      Https `shouldBe` Https
      certbotConfig `shouldBe` certbotConfig
      certbotConfig `shouldBe` certbotConfig
      TlsConfig {certificateSource = manualCertificateSource}
        `shouldBe` TlsConfig {certificateSource = manualCertificateSource}
      sharedCertificateSource `shouldBe` sharedCertificateSource
      acmeCertificateSource `shouldBe` acmeCertificateSource
      staticRoot `shouldBe` staticRoot
      English `shouldBe` English
      French `shouldBe` French
      PageSurface `shouldBe` PageSurface
      ApiSurface `shouldBe` ApiSurface
      HomeRoute `shouldBe` HomeRoute
      SecondRoute `shouldBe` SecondRoute
      StatusApiRoute `shouldBe` StatusApiRoute
      NotFoundRoute `shouldBe` NotFoundRoute
      UnsupportedLocalePrefix "de" `shouldBe` UnsupportedLocalePrefix "de"
      UnsupportedPath "/missing" `shouldBe` UnsupportedPath "/missing"
      HomePage homePageModel `shouldBe` HomePage homePageModel
      SecondPage secondPageModel `shouldBe` SecondPage secondPageModel
      NotFoundPage notFoundPageModel `shouldBe` NotFoundPage notFoundPageModel
      show certbotConfig
        `shouldBe` "CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]}"
      show certbotConfig
        `shouldBe` "CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]}"
      show
        AcmeConfig
          { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
            acmeContactEmails = ["ops@example.com"],
            acmeDomains = ["example.com", "www.example.com"],
            acmeHttp01Port = 80,
            acmeCertificateDirectory = Nothing,
            acmeCertbotConfig = certbotConfig
          }
        `shouldBe` "AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeDomains = [\"example.com\",\"www.example.com\"], acmeHttp01Port = 80, acmeCertificateDirectory = Nothing, acmeCertbotConfig = CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]}}"
      show acmeCertificateSource
        `shouldBe` "AcmeCertificateSource (AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeDomains = [\"example.com\",\"www.example.com\"], acmeHttp01Port = 80, acmeCertificateDirectory = Nothing, acmeCertbotConfig = CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]}})"
      show (TlsConfig {certificateSource = manualCertificateSource})
        `shouldBe` "TlsConfig {certificateSource = ManualCertificateFiles {certificateFile = \"cert.pem\", privateKeyFile = \"key.pem\"}}"
      show sharedCertificateSource
        `shouldBe` "SharedCertificateFiles {certificateDirectory = \"/var/lib/web-api/shared-certs\", sharedCertificateStartupMode = AwaitCertificateFiles {certificateWaitTimeoutSeconds = Nothing}}"
      show manualCertificateSource
        `shouldBe` "ManualCertificateFiles {certificateFile = \"cert.pem\", privateKeyFile = \"key.pem\"}"
      show (ListenerConfig {listenerHost = "127.0.0.1", listenerPort = 5001, listenerScheme = Http, listenerTls = Nothing, listenerAcme = Nothing})
        `shouldBe` "ListenerConfig {listenerHost = \"127.0.0.1\", listenerPort = 5001, listenerScheme = Http, listenerTls = Nothing}"
      show staticRoot `shouldBe` "StaticAssetRoot {staticUrlPrefix = \"/assets\", staticDirectory = \"public\"}"
      show
        ( StaticAssetsConfig
            { staticAssetRoots = [staticRoot],
              staticAssetContentTypes = defaultStaticAssetContentTypes,
              staticCacheControlSeconds = Just 3600
            }
        )
        `shouldBe` ( "StaticAssetsConfig {staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = \"/assets\", staticDirectory = \"public\"}], staticAssetContentTypes = "
                       <> show defaultStaticAssetContentTypes
                       <> ", staticCacheControlSeconds = Just 3600}"
                   )
      show
        ( ObservabilityConfig
            { tracingExporter =
                Just
                  OtlpExporter
                    { otlpEndpoint = "http://otel-collector:4318",
                      otlpHeaders = [("x-api-key", "secret")]
                    },
              metricsExporter = Nothing
            }
        )
        `shouldBe` "ObservabilityConfig {tracingExporter = Just (OtlpExporter {otlpEndpoint = \"http://otel-collector:4318\", otlpHeaders = [(\"x-api-key\",\"secret\")]}), metricsExporter = Nothing}"
      show
        ( AppRequestContext
            { requestLocale = French,
              requestCorrelationId = Just "req-789",
              requestSurface = PageSurface,
              requestPathPrefix = ""
            }
        )
        `shouldBe` "AppRequestContext {requestLocale = French, requestCorrelationId = Just \"req-789\", requestSurface = PageSurface, requestPathPrefix = \"\"}"
      show
        ( CallToAction
            { callToActionLabel = "Return home",
              callToActionRoute = HomeRoute,
              callToActionHref = "/"
            }
        )
        `shouldBe` "CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = \"/\"}"
      show English `shouldBe` "English"
      show French `shouldBe` "French"
      show PageSurface `shouldBe` "PageSurface"
      show ApiSurface `shouldBe` "ApiSurface"
      show (UnsupportedLocalePrefix "de") `shouldBe` "UnsupportedLocalePrefix \"de\""
      show (UnsupportedPath "/missing") `shouldBe` "UnsupportedPath \"/missing\""
      show homePageModel
        `shouldBe` "HomePageModel {homeHeading = \"Home\", homeSummary = \"Server-rendered home page with stubbed content.\", homeErrorMessage = Nothing, homePrimaryAction = CallToAction {callToActionLabel = \"Browse the second page\", callToActionRoute = SecondRoute, callToActionHref = \"/second\"}}"
      show secondPageModel
        `shouldBe` "SecondPageModel {secondHeading = \"Second\", secondSummary = \"Second page content with stubbed data ready for future loaders.\", secondHighlights = [\"Fast SSR\"], secondErrorMessage = Nothing, secondPrimaryAction = CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = \"/\"}}"
      show (HomePage homePageModel)
        `shouldBe` "HomePage (HomePageModel {homeHeading = \"Home\", homeSummary = \"Server-rendered home page with stubbed content.\", homeErrorMessage = Nothing, homePrimaryAction = CallToAction {callToActionLabel = \"Browse the second page\", callToActionRoute = SecondRoute, callToActionHref = \"/second\"}})"
      show (SecondPage secondPageModel)
        `shouldBe` "SecondPage (SecondPageModel {secondHeading = \"Second\", secondSummary = \"Second page content with stubbed data ready for future loaders.\", secondHighlights = [\"Fast SSR\"], secondErrorMessage = Nothing, secondPrimaryAction = CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = \"/\"}})"
      show notFoundPageModel
        `shouldBe` "NotFoundPageModel {notFoundHeading = \"Not Found\", notFoundSummary = \"The requested page could not be found.\", notFoundPrimaryAction = CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = \"/\"}}"
      show (NotFoundPage notFoundPageModel)
        `shouldBe` "NotFoundPage (NotFoundPageModel {notFoundHeading = \"Not Found\", notFoundSummary = \"The requested page could not be found.\", notFoundPrimaryAction = CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = \"/\"}})"
      show
        ( AppConfig
            { appTitlePrefix = "test-app",
              listenerConfigs = [ListenerConfig {listenerHost = "127.0.0.1", listenerPort = 5001, listenerScheme = Http, listenerTls = Nothing, listenerAcme = Nothing}],
              staticAssets =
                StaticAssetsConfig
                  { staticAssetRoots = [staticRoot],
                    staticAssetContentTypes = defaultStaticAssetContentTypes,
                    staticCacheControlSeconds = Just 3600
                  },
              requestPolicy = requestPolicy defaultAppConfig,
              observability = ObservabilityConfig {tracingExporter = Nothing, metricsExporter = Nothing}
            }
        )
        `shouldContain` ("staticAssetContentTypes = " <> show defaultStaticAssetContentTypes)

    it "covers direct equality branches across the remaining public config and page types" $ do
      let certbotConfig =
            CertbotConfig
              { certbotExecutable = "certbot",
                certbotArguments = ["certonly", "--webroot"]
              }
          otherCertbotConfig =
            CertbotConfig
              { certbotExecutable = "certbot",
                certbotArguments = ["renew"]
              }
          acmeConfig =
            AcmeConfig
              { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                acmeContactEmails = ["ops@example.com"],
                acmeDomains = ["example.com", "www.example.com"],
                acmeHttp01Port = 80,
                acmeCertificateDirectory = Nothing,
                acmeCertbotConfig = certbotConfig
              }
          otherAcmeConfig =
            AcmeConfig
              { acmeDirectoryUrl = "https://acme-staging-v02.api.letsencrypt.org/directory",
                acmeContactEmails = ["ops@example.com"],
                acmeDomains = ["staging.example.com"],
                acmeHttp01Port = 80,
                acmeCertificateDirectory = Just "/var/lib/web-api/staging-certs",
                acmeCertbotConfig = otherCertbotConfig
              }
          manualCertificateSource =
            ManualCertificateFiles
              { certificateFile = "cert.pem",
                privateKeyFile = "key.pem"
              }
          acmeCertificateSource = AcmeCertificateSource acmeConfig
          tlsConfig = TlsConfig {certificateSource = manualCertificateSource}
          listenerConfig =
            ListenerConfig
              { listenerHost = "127.0.0.1",
                listenerPort = 5001,
                listenerScheme = Http,
                listenerTls = Nothing,
                listenerAcme = Nothing
              }
          secureListenerConfig =
            ListenerConfig
              { listenerHost = "0.0.0.0",
                listenerPort = 5443,
                listenerScheme = Https,
                listenerTls = Just tlsConfig,
                listenerAcme = Nothing
              }
          staticRoot =
            StaticAssetRoot
              { staticUrlPrefix = "/assets",
                staticDirectory = "public"
              }
          staticAssetsConfig =
            StaticAssetsConfig
              { staticAssetRoots = [staticRoot],
                staticAssetContentTypes = defaultStaticAssetContentTypes,
                staticCacheControlSeconds = Just 3600
              }
          exporter =
            OtlpExporter
              { otlpEndpoint = "http://otel-collector:4318",
                otlpHeaders = [("authorization", "Bearer token")]
              }
          observabilityConfig =
            ObservabilityConfig
              { tracingExporter = Just exporter,
                metricsExporter = Nothing
              }
          appConfig =
            AppConfig
              { appTitlePrefix = "test-app",
                listenerConfigs = [listenerConfig, secureListenerConfig],
                staticAssets = staticAssetsConfig,
                requestPolicy = requestPolicy defaultAppConfig,
                observability = observabilityConfig
              }
          requestContext =
            AppRequestContext
              { requestLocale = French,
                requestCorrelationId = Just "req-123",
                requestSurface = PageSurface,
                requestPathPrefix = ""
              }
          callToAction =
            CallToAction
              { callToActionLabel = "Return home",
                callToActionRoute = HomeRoute,
                callToActionHref = "/"
              }
          homePageModel =
            HomePageModel
              { homeHeading = "Home",
                homeSummary = "Server-rendered home page with stubbed content.",
                homeErrorMessage = Nothing,
                homePrimaryAction = callToAction
              }
          secondPageModel =
            SecondPageModel
              { secondHeading = "Second",
                secondSummary = "Second page content with stubbed data ready for future loaders.",
                secondHighlights = ["Fast SSR"],
                secondErrorMessage = Nothing,
                secondPrimaryAction = callToAction
              }
          notFoundPageModel =
            NotFoundPageModel
              { notFoundHeading = "Not Found",
                notFoundSummary = "The requested page could not be found.",
                notFoundPrimaryAction = callToAction
              }
      certbotExecutable certbotConfig `shouldBe` "certbot"
      certbotArguments certbotConfig `shouldBe` ["certonly", "--webroot"]
      certbotConfig `shouldBe` certbotConfig
      certbotConfig `shouldNotBe` otherCertbotConfig
      acmeConfig `shouldBe` acmeConfig
      acmeConfig `shouldNotBe` otherAcmeConfig
      manualCertificateSource `shouldBe` manualCertificateSource
      manualCertificateSource `shouldNotBe` acmeCertificateSource
      acmeCertificateSource `shouldBe` acmeCertificateSource
      acmeCertificateSource `shouldNotBe` AcmeCertificateSource otherAcmeConfig
      tlsConfig `shouldBe` tlsConfig
      tlsConfig `shouldNotBe` TlsConfig {certificateSource = acmeCertificateSource}
      listenerConfig `shouldBe` listenerConfig
      listenerConfig `shouldNotBe` secureListenerConfig
      staticRoot `shouldBe` staticRoot
      staticRoot `shouldNotBe` StaticAssetRoot {staticUrlPrefix = "/static", staticDirectory = "public"}
      staticAssetsConfig `shouldBe` staticAssetsConfig
      staticAssetsConfig
        `shouldNotBe` StaticAssetsConfig
          { staticAssetRoots = [],
            staticAssetContentTypes = defaultStaticAssetContentTypes,
            staticCacheControlSeconds = Nothing
          }
      exporter `shouldBe` exporter
      exporter `shouldNotBe` OtlpExporter {otlpEndpoint = "http://other-collector:4318", otlpHeaders = []}
      observabilityConfig `shouldBe` observabilityConfig
      observabilityConfig `shouldNotBe` ObservabilityConfig {tracingExporter = Nothing, metricsExporter = Nothing}
      appConfig `shouldBe` appConfig
      appConfig `shouldNotBe` appConfig {listenerConfigs = [listenerConfig]}
      English `shouldNotBe` French
      requestContext `shouldBe` requestContext
      requestContext `shouldNotBe` defaultRequestContext
      callToAction `shouldBe` callToAction
      callToAction `shouldNotBe` callToAction {callToActionHref = "/fr"}
      homePageModel `shouldBe` homePageModel
      homePageModel `shouldNotBe` homePageModel {homeHeading = "Accueil"}
      secondPageModel `shouldBe` secondPageModel
      secondPageModel `shouldNotBe` secondPageModel {secondHighlights = ["Different"]}
      notFoundPageModel `shouldBe` notFoundPageModel
      notFoundPageModel `shouldNotBe` notFoundPageModel {notFoundSummary = "Missing"}
      HomePage homePageModel `shouldNotBe` SecondPage secondPageModel
      SecondPage secondPageModel `shouldNotBe` NotFoundPage notFoundPageModel
      UnsupportedLocalePrefix "de" `shouldNotBe` UnsupportedPath "/de"
      PageSurface `shouldNotBe` ApiSurface
      HomeRoute `shouldNotBe` SecondRoute
      SecondRoute `shouldNotBe` NotFoundRoute

    it "covers high-precedence show rendering for the remaining public types" $ do
      let shouldBeParenthesized rendered = do
            case rendered of
              '(' : rest ->
                case reverse rest of
                  ')' : _ -> pure ()
                  _ -> expectationFailure "expected parenthesized rendering"
              _ -> expectationFailure "expected parenthesized rendering"
          certbotConfig =
            CertbotConfig
              { certbotExecutable = "certbot",
                certbotArguments = ["certonly", "--webroot"]
              }
          acmeConfig =
            AcmeConfig
              { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                acmeContactEmails = ["ops@example.com"],
                acmeDomains = ["example.com", "www.example.com"],
                acmeHttp01Port = 80,
                acmeCertificateDirectory = Nothing,
                acmeCertbotConfig = certbotConfig
              }
          manualCertificateSource =
            ManualCertificateFiles
              { certificateFile = "cert.pem",
                privateKeyFile = "key.pem"
              }
          acmeCertificateSource = AcmeCertificateSource acmeConfig
          tlsConfig = TlsConfig {certificateSource = acmeCertificateSource}
          listenerConfig =
            ListenerConfig
              { listenerHost = "0.0.0.0",
                listenerPort = 5443,
                listenerScheme = Https,
                listenerTls = Just tlsConfig,
                listenerAcme = Nothing
              }
          staticRoot =
            StaticAssetRoot
              { staticUrlPrefix = "/assets",
                staticDirectory = "public"
              }
          staticAssetsConfig =
            StaticAssetsConfig
              { staticAssetRoots = [staticRoot],
                staticAssetContentTypes = defaultStaticAssetContentTypes,
                staticCacheControlSeconds = Just 3600
              }
          exporter =
            OtlpExporter
              { otlpEndpoint = "http://otel-collector:4318",
                otlpHeaders = [("authorization", "Bearer token")]
              }
          observabilityConfig =
            ObservabilityConfig
              { tracingExporter = Just exporter,
                metricsExporter = Just exporter
              }
          appConfig =
            AppConfig
              { appTitlePrefix = "test-app",
                listenerConfigs = [listenerConfig],
                staticAssets = staticAssetsConfig,
                requestPolicy = requestPolicy defaultAppConfig,
                observability = observabilityConfig
              }
          requestContext =
            AppRequestContext
              { requestLocale = French,
                requestCorrelationId = Just "req-999",
                requestSurface = PageSurface,
                requestPathPrefix = ""
              }
          callToAction =
            CallToAction
              { callToActionLabel = "Return home",
                callToActionRoute = HomeRoute,
                callToActionHref = "/"
              }
          homePageModel =
            HomePageModel
              { homeHeading = "Home",
                homeSummary = "Server-rendered home page with stubbed content.",
                homeErrorMessage = Nothing,
                homePrimaryAction = callToAction
              }
          secondPageModel =
            SecondPageModel
              { secondHeading = "Second",
                secondSummary = "Second page content with stubbed data ready for future loaders.",
                secondHighlights = ["Fast SSR"],
                secondErrorMessage = Nothing,
                secondPrimaryAction = callToAction
              }
          notFoundPageModel =
            NotFoundPageModel
              { notFoundHeading = "Not Found",
                notFoundSummary = "The requested page could not be found.",
                notFoundPrimaryAction = callToAction
              }
      show Http `shouldBe` "Http"
      show Https `shouldBe` "Https"
      show HomeRoute `shouldBe` "HomeRoute"
      show SecondRoute `shouldBe` "SecondRoute"
      show StatusApiRoute `shouldBe` "StatusApiRoute"
      show NotFoundRoute `shouldBe` "NotFoundRoute"
      shouldBeParenthesized (showsPrec 11 certbotConfig "")
      shouldBeParenthesized (showsPrec 11 certbotConfig "")
      shouldBeParenthesized (showsPrec 11 acmeConfig "")
      shouldBeParenthesized (showsPrec 11 manualCertificateSource "")
      shouldBeParenthesized (showsPrec 11 acmeCertificateSource "")
      shouldBeParenthesized (showsPrec 11 tlsConfig "")
      shouldBeParenthesized (showsPrec 11 listenerConfig "")
      shouldBeParenthesized (showsPrec 11 staticRoot "")
      shouldBeParenthesized (showsPrec 11 staticAssetsConfig "")
      shouldBeParenthesized (showsPrec 11 exporter "")
      shouldBeParenthesized (showsPrec 11 observabilityConfig "")
      shouldBeParenthesized (showsPrec 11 appConfig "")
      shouldBeParenthesized (showsPrec 11 requestContext "")
      shouldBeParenthesized (showsPrec 11 callToAction "")
      shouldBeParenthesized (showsPrec 11 homePageModel "")
      shouldBeParenthesized (showsPrec 11 secondPageModel "")
      shouldBeParenthesized (showsPrec 11 notFoundPageModel "")
      shouldBeParenthesized (showsPrec 11 (HomePage homePageModel) "")
      shouldBeParenthesized (showsPrec 11 (SecondPage secondPageModel) "")
      shouldBeParenthesized (showsPrec 11 (NotFoundPage notFoundPageModel) "")
      shouldBeParenthesized (showsPrec 11 (UnsupportedLocalePrefix "de") "")
      shouldBeParenthesized (showsPrec 11 (UnsupportedPath "/missing") "")

    it "covers derived list-show rendering for the remaining public types" $ do
      let certbotConfig =
            CertbotConfig
              { certbotExecutable = "certbot",
                certbotArguments = ["certonly", "--webroot"]
              }
          acmeConfig =
            AcmeConfig
              { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                acmeContactEmails = ["ops@example.com"],
                acmeDomains = ["example.com", "www.example.com"],
                acmeHttp01Port = 80,
                acmeCertificateDirectory = Nothing,
                acmeCertbotConfig = certbotConfig
              }
          manualCertificateSource =
            ManualCertificateFiles
              { certificateFile = "cert.pem",
                privateKeyFile = "key.pem"
              }
          acmeCertificateSource = AcmeCertificateSource acmeConfig
          tlsConfig = TlsConfig {certificateSource = acmeCertificateSource}
          listenerConfig =
            ListenerConfig
              { listenerHost = "0.0.0.0",
                listenerPort = 5443,
                listenerScheme = Https,
                listenerTls = Just tlsConfig,
                listenerAcme = Nothing
              }
          staticRoot =
            StaticAssetRoot
              { staticUrlPrefix = "/assets",
                staticDirectory = "public"
              }
          staticAssetsConfig =
            StaticAssetsConfig
              { staticAssetRoots = [staticRoot],
                staticAssetContentTypes = defaultStaticAssetContentTypes,
                staticCacheControlSeconds = Just 3600
              }
          exporter =
            OtlpExporter
              { otlpEndpoint = "http://otel-collector:4318",
                otlpHeaders = [("authorization", "Bearer token")]
              }
          observabilityConfig =
            ObservabilityConfig
              { tracingExporter = Just exporter,
                metricsExporter = Just exporter
              }
          appConfig =
            AppConfig
              { appTitlePrefix = "test-app",
                listenerConfigs = [listenerConfig],
                staticAssets = staticAssetsConfig,
                requestPolicy = requestPolicy defaultAppConfig,
                observability = observabilityConfig
              }
          requestContext =
            AppRequestContext
              { requestLocale = French,
                requestCorrelationId = Just "req-list",
                requestSurface = PageSurface,
                requestPathPrefix = ""
              }
          callToAction =
            CallToAction
              { callToActionLabel = "Return home",
                callToActionRoute = HomeRoute,
                callToActionHref = "/"
              }
          homePageModel =
            HomePageModel
              { homeHeading = "Home",
                homeSummary = "Server-rendered home page with stubbed content.",
                homeErrorMessage = Nothing,
                homePrimaryAction = callToAction
              }
          secondPageModel =
            SecondPageModel
              { secondHeading = "Second",
                secondSummary = "Second page content with stubbed data ready for future loaders.",
                secondHighlights = ["Fast SSR"],
                secondErrorMessage = Nothing,
                secondPrimaryAction = callToAction
              }
          notFoundPageModel =
            NotFoundPageModel
              { notFoundHeading = "Not Found",
                notFoundSummary = "The requested page could not be found.",
                notFoundPrimaryAction = callToAction
              }
      Http `shouldNotBe` Https
      show [Http, Https] `shouldBe` "[Http,Https]"
      show [certbotConfig] `shouldBe` "[CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]}]"
      show [certbotConfig]
        `shouldBe` "[CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]}]"
      show [acmeConfig]
        `shouldBe` "[AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeDomains = [\"example.com\",\"www.example.com\"], acmeHttp01Port = 80, acmeCertificateDirectory = Nothing, acmeCertbotConfig = CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]}}]"
      show [manualCertificateSource, acmeCertificateSource]
        `shouldBe` "[ManualCertificateFiles {certificateFile = \"cert.pem\", privateKeyFile = \"key.pem\"},AcmeCertificateSource (AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeDomains = [\"example.com\",\"www.example.com\"], acmeHttp01Port = 80, acmeCertificateDirectory = Nothing, acmeCertbotConfig = CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]}})]"
      show [tlsConfig]
        `shouldBe` "[TlsConfig {certificateSource = AcmeCertificateSource (AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeDomains = [\"example.com\",\"www.example.com\"], acmeHttp01Port = 80, acmeCertificateDirectory = Nothing, acmeCertbotConfig = CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]}})}]"
      show [listenerConfig]
        `shouldBe` "[ListenerConfig {listenerHost = \"0.0.0.0\", listenerPort = 5443, listenerScheme = Https, listenerTls = Just (TlsConfig {certificateSource = AcmeCertificateSource (AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeDomains = [\"example.com\",\"www.example.com\"], acmeHttp01Port = 80, acmeCertificateDirectory = Nothing, acmeCertbotConfig = CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]}})})}]"
      show [staticRoot] `shouldBe` "[StaticAssetRoot {staticUrlPrefix = \"/assets\", staticDirectory = \"public\"}]"
      show [staticAssetsConfig]
        `shouldBe` ( "[StaticAssetsConfig {staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = \"/assets\", staticDirectory = \"public\"}], staticAssetContentTypes = "
                       <> show defaultStaticAssetContentTypes
                       <> ", staticCacheControlSeconds = Just 3600}]"
                   )
      show [exporter]
        `shouldBe` "[OtlpExporter {otlpEndpoint = \"http://otel-collector:4318\", otlpHeaders = [(\"authorization\",\"Bearer token\")]}]"
      show [observabilityConfig]
        `shouldBe` "[ObservabilityConfig {tracingExporter = Just (OtlpExporter {otlpEndpoint = \"http://otel-collector:4318\", otlpHeaders = [(\"authorization\",\"Bearer token\")]}), metricsExporter = Just (OtlpExporter {otlpEndpoint = \"http://otel-collector:4318\", otlpHeaders = [(\"authorization\",\"Bearer token\")]})}]"
      show [appConfig]
        `shouldContain` ("staticAssetContentTypes = " <> show defaultStaticAssetContentTypes)
      show [English, French] `shouldBe` "[English,French]"
      show [PageSurface, ApiSurface] `shouldBe` "[PageSurface,ApiSurface]"
      show [requestContext]
        `shouldBe` "[AppRequestContext {requestLocale = French, requestCorrelationId = Just \"req-list\", requestSurface = PageSurface, requestPathPrefix = \"\"}]"
      show [callToAction]
        `shouldBe` "[CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = \"/\"}]"
      show [homePageModel]
        `shouldBe` "[HomePageModel {homeHeading = \"Home\", homeSummary = \"Server-rendered home page with stubbed content.\", homeErrorMessage = Nothing, homePrimaryAction = CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = \"/\"}}]"
      show [secondPageModel]
        `shouldBe` "[SecondPageModel {secondHeading = \"Second\", secondSummary = \"Second page content with stubbed data ready for future loaders.\", secondHighlights = [\"Fast SSR\"], secondErrorMessage = Nothing, secondPrimaryAction = CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = \"/\"}}]"
      show [notFoundPageModel]
        `shouldBe` "[NotFoundPageModel {notFoundHeading = \"Not Found\", notFoundSummary = \"The requested page could not be found.\", notFoundPrimaryAction = CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = \"/\"}}]"
      show [HomePage homePageModel, SecondPage secondPageModel, NotFoundPage notFoundPageModel]
        `shouldBe` "[HomePage (HomePageModel {homeHeading = \"Home\", homeSummary = \"Server-rendered home page with stubbed content.\", homeErrorMessage = Nothing, homePrimaryAction = CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = \"/\"}}),SecondPage (SecondPageModel {secondHeading = \"Second\", secondSummary = \"Second page content with stubbed data ready for future loaders.\", secondHighlights = [\"Fast SSR\"], secondErrorMessage = Nothing, secondPrimaryAction = CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = \"/\"}}),NotFoundPage (NotFoundPageModel {notFoundHeading = \"Not Found\", notFoundSummary = \"The requested page could not be found.\", notFoundPrimaryAction = CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = \"/\"}})]"
      show [UnsupportedLocalePrefix "de", UnsupportedPath "/missing"]
        `shouldBe` "[UnsupportedLocalePrefix \"de\",UnsupportedPath \"/missing\"]"
      show [HomeRoute, SecondRoute, StatusApiRoute, NotFoundRoute] `shouldBe` "[HomeRoute,SecondRoute,StatusApiRoute,NotFoundRoute]"

  describe "parseRoute" $ do
    it "maps bare and default-locale paths to the same home route" $ do
      fmap HarchWeb.requestRoute (parseRoute defaultRequestContext "/") `shouldBe` Just HomeRoute
      fmap HarchWeb.requestRoute (parseRoute defaultRequestContext "/en") `shouldBe` Just HomeRoute
      fmap HarchWeb.requestRoute (parseRoute defaultRequestContext "/404") `shouldBe` Just NotFoundRoute

    it "parses API routes with the API response surface" $ do
      parseRoute defaultRequestContext "/api/status" `shouldBe` Just apiStatusRequest
      parseRoute defaultRequestContext "/api/second" `shouldBe` Just apiSecondRequest
      parseRoute defaultRequestContext "/api" `shouldBe` Just apiNotFoundRequest
      parseRoute defaultRequestContext "/api/404" `shouldBe` Just apiNotFoundRequest
      parseRoute defaultRequestContext "/api/missing" `shouldBe` Just apiNotFoundRequest
      parseRoute defaultRequestContext "/api/status/extra" `shouldBe` Just apiNotFoundRequest

    it "parses the second page path" $
      parseRoute defaultRequestContext "/second" `shouldBe` Just secondRequest

    it "lets explicit locale prefixes override the incoming request context" $ do
      parseRoute defaultRequestContext "/fr/second" `shouldBe` Just frenchSecondRequest
      parseRoute frenchRequestContext "/en/second" `shouldBe` Just secondRequest

    it "returns an unsupported-route representation for unknown paths" $
      parseRoute defaultRequestContext "/missing" `shouldBe` Nothing

    it "fails unsupported locale prefixes with a precise route-selection error" $ do
      selectRoute defaultRequestContext "/de" `shouldBe` Left (UnsupportedLocalePrefix "de")
      selectRoute defaultRequestContext "/de/second" `shouldBe` Left (UnsupportedLocalePrefix "de")

    it "rejects paths that do not start with a slash" $
      selectRoute defaultRequestContext "second" `shouldBe` Left (UnsupportedPath "second")

    it "rejects unsupported multi-segment paths" $
      selectRoute defaultRequestContext "/fr/second/extra" `shouldBe` Left (UnsupportedPath "/fr/second/extra")

    it "rejects unsupported single-segment non-locale paths" $
      selectRoute defaultRequestContext "/missing" `shouldBe` Left (UnsupportedPath "/missing")

    it "rejects locale-prefixed paths whose trailing segment is unsupported" $ do
      selectRoute defaultRequestContext "/fr/missing" `shouldBe` Left (UnsupportedPath "/fr/missing")
      selectRoute defaultRequestContext "/other/second" `shouldBe` Left (UnsupportedPath "/other/second")

    it "merges middleware-supplied and path-derived request inputs deterministically" $ do
      let middlewareContext =
            defaultRequestContext
              { requestLocale = English,
                requestCorrelationId = Just "req-123"
              }
      parseRoute middlewareContext "/fr"
        `shouldBe` Just (HarchWeb.RouteRequest {HarchWeb.requestRoute = HomeRoute, HarchWeb.requestContext = middlewareContext {requestLocale = French}})

    it "rejects invalid trailing slashes while keeping the root path valid" $ do
      parseRoute defaultRequestContext "/" `shouldBe` Just homeRequest
      parseRoute defaultRequestContext "/second/" `shouldBe` Nothing
      selectRoute defaultRequestContext "/second/" `shouldBe` Left (UnsupportedPath "/second/")

  describe "renderRoutePath" $ do
    it "round-trips known routes through the parser" $ do
      parseRoute defaultRequestContext (renderRoutePath homeRequest) `shouldBe` Just homeRequest
      parseRoute defaultRequestContext (renderRoutePath secondRequest) `shouldBe` Just secondRequest
      parseRoute defaultRequestContext (renderRoutePath frenchSecondRequest) `shouldBe` Just frenchSecondRequest
      parseRoute defaultRequestContext (renderRoutePath apiStatusRequest) `shouldBe` Just apiStatusRequest
      parseRoute defaultRequestContext (renderRoutePath apiSecondRequest) `shouldBe` Just apiSecondRequest
      parseRoute defaultRequestContext (renderRoutePath apiNotFoundRequest) `shouldBe` Just apiNotFoundRequest

    it "renders locale prefixes only for non-default locales" $ do
      renderRoutePath homeRequest `shouldBe` "/"
      renderRoutePath frenchHomeRequest `shouldBe` "/fr"
      renderRoutePath secondRequest `shouldBe` "/second"
      renderRoutePath frenchSecondRequest `shouldBe` "/fr/second"
      renderRoutePath (HarchWeb.RouteRequest {HarchWeb.requestRoute = StatusApiRoute, HarchWeb.requestContext = defaultRequestContext}) `shouldBe` "/404"
      renderRoutePath apiStatusRequest `shouldBe` "/api/status"
      renderRoutePath apiSecondRequest `shouldBe` "/api/second"
      renderRoutePath apiNotFoundRequest `shouldBe` "/api/404"
      renderRoutePath notFoundRequest `shouldBe` "/404"

    it "prepends the forwarded request path prefix to page and API routes" $ do
      renderRoutePath prefixedHomeRequest `shouldBe` "/app"
      renderRoutePath prefixedFrenchSecondRequest `shouldBe` "/app/fr/second"
      renderRoutePath prefixedApiStatusRequest `shouldBe` "/app/api/status"

  describe "matchRoute" $ do
    it "remains available separately from HarchWeb.matchRoute" $
      WebApi.Route.matchRoute WebApi.Route.defaultRequestContext "/second"
        `shouldBe` HarchWeb.matchRoute WebApi.Route.routeCodec WebApi.Route.defaultRequestContext "/second"

    it "matches the home path" $
      pureRouteMatcher "/" `shouldBe` homeRequest

    it "matches the second page path" $
      pureRouteMatcher "/second" `shouldBe` secondRequest

    it "matches locale-prefixed paths with the merged request context" $
      pureRouteMatcher "/fr" `shouldBe` frenchHomeRequest

    it "matches API paths with the API response surface" $ do
      pureRouteMatcher "/api/status" `shouldBe` apiStatusRequest
      pureRouteMatcher "/api/second" `shouldBe` apiSecondRequest
      pureRouteMatcher "/api/missing" `shouldBe` apiNotFoundRequest

    it "falls back to the stable not-found route for unknown paths" $
      pureRouteMatcher "/missing" `shouldBe` notFoundRequest

  describe "renderPage" $ do
    it "selects the expected home page model" $
      renderPage defaultAppConfig homeRequest
        `shouldReturn` HarchWeb.Page
          { HarchWeb.pageTitle = "web-api: Home",
            HarchWeb.pageRoute = HomeRoute,
            HarchWeb.pageContext = defaultRequestContext,
            HarchWeb.pageBody = "<section data-page=\"home\"><h1 data-page-title=\"true\">Home</h1><p>Server-rendered home page with stubbed content.</p><p><a href=\"/second\" data-page-link=\"true\">Browse the second page</a></p></section>",
            HarchWeb.pageBootstrapHooks = []
          }

    it "selects a distinct second page model" $
      renderPage defaultAppConfig secondRequest
        `shouldReturn` HarchWeb.Page
          { HarchWeb.pageTitle = "web-api: Second",
            HarchWeb.pageRoute = SecondRoute,
            HarchWeb.pageContext = defaultRequestContext,
            HarchWeb.pageBody = "<section data-page=\"second\"><h1 data-page-title=\"true\">Second</h1><p>Second page content with stubbed data ready for future loaders.</p><p data-empty-state=\"true\">No highlights yet.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section>",
            HarchWeb.pageBootstrapHooks = ["second-page"]
          }

    it "selects a stable not-found page model" $
      renderPage defaultAppConfig notFoundRequest
        `shouldReturn` HarchWeb.Page
          { HarchWeb.pageTitle = "web-api: Not Found",
            HarchWeb.pageRoute = NotFoundRoute,
            HarchWeb.pageContext = defaultRequestContext,
            HarchWeb.pageBody = "<section data-page=\"not-found\"><h1 data-page-title=\"true\">Not Found</h1><p>The requested page could not be found.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section>",
            HarchWeb.pageBootstrapHooks = []
          }

    it "renders selected route data without reloading it" $
      renderPageFromRouteData
        defaultAppConfig
        secondRequest
        ( SecondRouteDataResult
            ( Right
                SecondRouteData
                  { secondRouteSummary = "Shared domain summary.",
                    secondRouteHighlights = ["Shared loader"]
                  }
            )
        )
        `shouldBe` HarchWeb.Page
          { HarchWeb.pageTitle = "web-api: Second",
            HarchWeb.pageRoute = SecondRoute,
            HarchWeb.pageContext = defaultRequestContext,
            HarchWeb.pageBody = "<section data-page=\"second\"><h1 data-page-title=\"true\">Second</h1><p>Shared domain summary.</p><ul><li>Shared loader</li></ul><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section>",
            HarchWeb.pageBootstrapHooks = ["second-page"]
          }

    it "keeps shared layout data consistent across all routes" $ do
      let config =
            AppConfig
              { appTitlePrefix = "test-app",
                listenerConfigs = listenerConfigs defaultAppConfig,
                staticAssets = staticAssets defaultAppConfig,
                requestPolicy = requestPolicy defaultAppConfig,
                observability = observability defaultAppConfig
              }
      renderedShell config HomeRoute
        `shouldReturn` "<html><head><title>test-app: Home</title></head><body data-app=\"test-app\"><nav data-navigation-region=\"primary\"><a href=\"/\" aria-current=\"page\">Home</a><a href=\"/second\">Second</a></nav><main id=\"app-main\" data-navigation-content=\"true\"><section data-page=\"home\"><h1 data-page-title=\"true\">Home</h1><p>Server-rendered home page with stubbed content.</p><p><a href=\"/second\" data-page-link=\"true\">Browse the second page</a></p></section></main></body></html>"
      renderedShell config SecondRoute
        `shouldReturn` "<html><head><title>test-app: Second</title></head><body data-app=\"test-app\"><nav data-navigation-region=\"primary\"><a href=\"/\">Home</a><a href=\"/second\" aria-current=\"page\">Second</a></nav><main id=\"app-main\" data-navigation-content=\"true\" data-bootstrap-hooks=\"second-page\"><section data-page=\"second\"><h1 data-page-title=\"true\">Second</h1><p>Second page content with stubbed data ready for future loaders.</p><p data-empty-state=\"true\">No highlights yet.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section></main></body></html>"
      renderedShell config NotFoundRoute
        `shouldReturn` "<html><head><title>test-app: Not Found</title></head><body data-app=\"test-app\"><nav data-navigation-region=\"primary\"><a href=\"/\">Home</a><a href=\"/second\">Second</a></nav><main id=\"app-main\" data-navigation-content=\"true\"><section data-page=\"not-found\"><h1 data-page-title=\"true\">Not Found</h1><p>The requested page could not be found.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section></main></body></html>"

    it "keeps config, routes, and pages serializable and deterministic for tests" $ do
      let config =
            AppConfig
              { appTitlePrefix = "test-app",
                listenerConfigs = listenerConfigs defaultAppConfig,
                staticAssets = staticAssets defaultAppConfig,
                requestPolicy = requestPolicy defaultAppConfig,
                observability = observability defaultAppConfig
              }
      show config
        `shouldContain` ("staticAssetContentTypes = " <> show defaultStaticAssetContentTypes)
      show defaultRequestContext `shouldBe` "AppRequestContext {requestLocale = English, requestCorrelationId = Nothing, requestSurface = PageSurface, requestPathPrefix = \"\"}"
      show (renderPageFromRouteData config secondRequest (SecondRouteDataResult (Right (SecondRouteData {secondRouteSummary = "Second page content with stubbed data ready for future loaders.", secondRouteHighlights = []}))))
        `shouldBe` "Page {pageTitle = \"test-app: Second\", pageRoute = SecondRoute, pageContext = AppRequestContext {requestLocale = English, requestCorrelationId = Nothing, requestSurface = PageSurface, requestPathPrefix = \"\"}, pageBody = \"<section data-page=\\\"second\\\"><h1 data-page-title=\\\"true\\\">Second</h1><p>Second page content with stubbed data ready for future loaders.</p><p data-empty-state=\\\"true\\\">No highlights yet.</p><p><a href=\\\"/\\\" data-page-link=\\\"true\\\">Return home</a></p></section>\", pageBootstrapHooks = [\"second-page\"]}"
      renderPage config secondRequest `shouldReturn` renderPageFromRouteData config secondRequest (SecondRouteDataResult (Right (SecondRouteData {secondRouteSummary = "Second page content with stubbed data ready for future loaders.", secondRouteHighlights = []})))

  describe "selectResponse" $ do
    it "resolves page routes to page responses that still flow through the shared shell" $ do
      renderedPage <- renderPage defaultAppConfig secondRequest
      selectResponse defaultAppConfig secondRequest `shouldReturn` HarchWeb.PageResponse renderedPage

    it "resolves API-only routes to explicit status, content type, and body values" $ do
      selectResponse defaultAppConfig apiStatusRequest
        `shouldReturn` HarchWeb.BodyResponse
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = 200,
              HarchWeb.responseContentType = "application/json",
              HarchWeb.responseBody = "{\"status\":\"ok\",\"locale\":\"en\"}",
              HarchWeb.responseObservabilityAttributes = [],
              HarchWeb.responseLogEntries = []
            }
      selectResponse defaultAppConfig apiSecondRequest
        `shouldReturn` HarchWeb.BodyResponse
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = 200,
              HarchWeb.responseContentType = "application/json",
              HarchWeb.responseBody = "{\"summary\":\"Second page content with stubbed data ready for future loaders.\",\"highlights\":[]}",
              HarchWeb.responseObservabilityAttributes = [],
              HarchWeb.responseLogEntries = []
            }

    it "keeps API payload rendering locale-aware without touching page routing" $ do
      selectResponse defaultAppConfig frenchApiStatusRequest
        `shouldReturn` HarchWeb.BodyResponse
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = 200,
              HarchWeb.responseContentType = "application/json",
              HarchWeb.responseBody = "{\"status\":\"ok\",\"locale\":\"fr\"}",
              HarchWeb.responseObservabilityAttributes = [],
              HarchWeb.responseLogEntries = []
            }
      selectResponse defaultAppConfig frenchApiSecondRequest
        `shouldReturn` HarchWeb.BodyResponse
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = 200,
              HarchWeb.responseContentType = "application/json",
              HarchWeb.responseBody = "{\"summary\":\"Second page content with stubbed data ready for future loaders.\",\"highlights\":[]}",
              HarchWeb.responseObservabilityAttributes = [],
              HarchWeb.responseLogEntries = []
            }

    it "attaches safe database operation observability to postgres-backed page and API responses" $ do
      let postgresRunner command =
            pure $
              case commandSql command of
                sql
                  | Text.isInfixOf "SELECT summary FROM web_api.page_content WHERE route_slug = 'home'" sql ->
                      successfulPostgresResult "Loaded home summary."
                  | Text.isInfixOf "SELECT summary FROM web_api.page_content WHERE route_slug = 'second'" sql ->
                      successfulPostgresResult "Loaded second summary."
                  | Text.isInfixOf "SELECT highlight FROM web_api.page_highlights" sql ->
                      successfulPostgresResult "Fast SSR\nShared route data"
                  | otherwise ->
                      failingPostgresResult "unexpected query"
          postgresEffect = buildPostgresDatabaseEffectWithRunner postgresRunner postgresTestConfig
      let renderedSecondPage =
            renderPageFromRouteData
              defaultAppConfig
              secondRequest
              ( SecondRouteDataResult
                  ( Right
                      SecondRouteData
                        { secondRouteSummary = "Loaded second summary.",
                          secondRouteHighlights = ["Fast SSR", "Shared route data"]
                        }
                  )
              )
      selectResponseWithDatabase defaultAppConfig postgresEffect secondRequest
        `shouldReturn` HarchWeb.PageResponseWithMetadata
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = 200,
              HarchWeb.responseContentType = "text/html; charset=utf-8",
              HarchWeb.responseBody = "",
              HarchWeb.responseObservabilityAttributes =
                [ Observability.ObservabilityAttribute
                    { Observability.attributeName = "db.system",
                      Observability.attributeValue = Observability.TextAttribute "postgresql"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "db.operation.name",
                      Observability.attributeValue = Observability.TextAttribute "load-second-page-summary"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "db.query.template",
                      Observability.attributeValue = Observability.TextAttribute "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "db.system",
                      Observability.attributeValue = Observability.TextAttribute "postgresql"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "db.operation.name",
                      Observability.attributeValue = Observability.TextAttribute "load-second-page-highlights"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "db.query.template",
                      Observability.attributeValue = Observability.TextAttribute "SELECT highlight FROM web_api.page_highlights WHERE route_slug = ? AND locale = ? ORDER BY position ASC;"
                    }
                ],
              HarchWeb.responseLogEntries = []
            }
          renderedSecondPage
      selectResponseWithDatabase defaultAppConfig postgresEffect apiSecondRequest
        `shouldReturn` HarchWeb.BodyResponse
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = 200,
              HarchWeb.responseContentType = "application/json",
              HarchWeb.responseBody = "{\"summary\":\"Loaded second summary.\",\"highlights\":[\"Fast SSR\",\"Shared route data\"]}",
              HarchWeb.responseObservabilityAttributes =
                [ Observability.ObservabilityAttribute
                    { Observability.attributeName = "db.system",
                      Observability.attributeValue = Observability.TextAttribute "postgresql"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "db.operation.name",
                      Observability.attributeValue = Observability.TextAttribute "load-second-page-summary"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "db.query.template",
                      Observability.attributeValue = Observability.TextAttribute "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "db.system",
                      Observability.attributeValue = Observability.TextAttribute "postgresql"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "db.operation.name",
                      Observability.attributeValue = Observability.TextAttribute "load-second-page-highlights"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "db.query.template",
                      Observability.attributeValue = Observability.TextAttribute "SELECT highlight FROM web_api.page_highlights WHERE route_slug = ? AND locale = ? ORDER BY position ASC;"
                    }
                ],
              HarchWeb.responseLogEntries = []
            }

    it "keeps not-found handling consistent across page and non-page responses" $ do
      renderedPage <- renderPage defaultAppConfig notFoundRequest
      selectResponse defaultAppConfig notFoundRequest `shouldReturn` HarchWeb.PageResponse renderedPage
      selectResponse defaultAppConfig apiNotFoundRequest
        `shouldReturn` HarchWeb.BodyResponse
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = 404,
              HarchWeb.responseContentType = "application/json",
              HarchWeb.responseBody = "{\"error\":\"not-found\"}",
              HarchWeb.responseObservabilityAttributes = [],
              HarchWeb.responseLogEntries = []
            }

    it "maps shared second-page load failures into explicit API error responses" $
      selectResponseWithDatabase
        defaultAppConfig
        ( buildSeededDatabaseEffect
            DatabaseSeed
              { englishHomePageData = englishHomePageData defaultDatabaseSeed,
                frenchHomePageData = frenchHomePageData defaultDatabaseSeed,
                englishSecondPageData = Left (SecondPageDataError "seed unavailable"),
                frenchSecondPageData = frenchSecondPageData defaultDatabaseSeed
              }
        )
        apiSecondRequest
        `shouldReturn` HarchWeb.BodyResponse
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = 503,
              HarchWeb.responseContentType = "application/json",
              HarchWeb.responseBody = "{\"error\":\"second-page-unavailable\"}",
              HarchWeb.responseObservabilityAttributes =
                [ Observability.ObservabilityAttribute
                    { Observability.attributeName = "exception.type",
                      Observability.attributeValue = Observability.TextAttribute "SecondPageDataError"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "exception.message",
                      Observability.attributeValue = Observability.TextAttribute "seed unavailable"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "app.route",
                      Observability.attributeValue = Observability.TextAttribute "/second"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "app.surface",
                      Observability.attributeValue = Observability.TextAttribute "api"
                    }
                ],
              HarchWeb.responseLogEntries =
                ["Database failure while rendering required second-page api response: SecondPageDataError \"seed unavailable\""]
            }

    it "adds safe database operation details to postgres-backed failure diagnostics" $ do
      let failingRunner command =
            pure $
              case commandSql command of
                sql
                  | Text.isInfixOf "SELECT summary FROM web_api.page_content WHERE route_slug = 'second'" sql ->
                      successfulPostgresResult "Loaded second summary."
                  | Text.isInfixOf "SELECT highlight FROM web_api.page_highlights" sql ->
                      failingPostgresResult "highlights unavailable"
                  | otherwise ->
                      failingPostgresResult "unexpected query"
          postgresEffect = buildPostgresDatabaseEffectWithRunner failingRunner postgresTestConfig
      selectResponseWithDatabase defaultAppConfig postgresEffect apiSecondRequest
        `shouldReturn` HarchWeb.BodyResponse
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = 503,
              HarchWeb.responseContentType = "application/json",
              HarchWeb.responseBody = "{\"error\":\"second-page-unavailable\"}",
              HarchWeb.responseObservabilityAttributes =
                [ Observability.ObservabilityAttribute
                    { Observability.attributeName = "exception.type",
                      Observability.attributeValue = Observability.TextAttribute "SecondPageDataError"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "exception.message",
                      Observability.attributeValue = Observability.TextAttribute "highlights unavailable"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "app.route",
                      Observability.attributeValue = Observability.TextAttribute "/second"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "app.surface",
                      Observability.attributeValue = Observability.TextAttribute "api"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "db.system",
                      Observability.attributeValue = Observability.TextAttribute "postgresql"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "db.operation.name",
                      Observability.attributeValue = Observability.TextAttribute "load-second-page-summary"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "db.query.template",
                      Observability.attributeValue = Observability.TextAttribute "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "db.system",
                      Observability.attributeValue = Observability.TextAttribute "postgresql"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "db.operation.name",
                      Observability.attributeValue = Observability.TextAttribute "load-second-page-highlights"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "db.query.template",
                      Observability.attributeValue = Observability.TextAttribute "SELECT highlight FROM web_api.page_highlights WHERE route_slug = ? AND locale = ? ORDER BY position ASC;"
                    }
                ],
              HarchWeb.responseLogEntries =
                [ "Database failure while rendering required second-page api response after database operations [load-second-page-summary (SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;), load-second-page-highlights (SELECT highlight FROM web_api.page_highlights WHERE route_slug = ? AND locale = ? ORDER BY position ASC;)]: SecondPageDataError \"highlights unavailable\""
                ]
            }

    it "preserves unexpected database error constructors in API diagnostics" $
      renderApiResponseFromRouteData (SecondRouteDataResult (Left (HomePageDataError "wrong loader")))
        `shouldBe` HarchWeb.ResponseBody
          { HarchWeb.responseStatus = 503,
            HarchWeb.responseContentType = "application/json",
            HarchWeb.responseBody = "{\"error\":\"second-page-unavailable\"}",
            HarchWeb.responseObservabilityAttributes =
              [ Observability.ObservabilityAttribute
                  { Observability.attributeName = "exception.type",
                    Observability.attributeValue = Observability.TextAttribute "HomePageDataError"
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "exception.message",
                    Observability.attributeValue = Observability.TextAttribute "wrong loader"
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "app.route",
                    Observability.attributeValue = Observability.TextAttribute "/second"
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "app.surface",
                    Observability.attributeValue = Observability.TextAttribute "api"
                  }
              ],
            HarchWeb.responseLogEntries =
              ["Database failure while rendering required second-page api response: HomePageDataError \"wrong loader\""]
          }

    it "maps required second-page failures into explicit HTML 500 responses" $ do
      let failingDatabaseEffect =
            buildSeededDatabaseEffect
              DatabaseSeed
                { englishHomePageData = englishHomePageData defaultDatabaseSeed,
                  frenchHomePageData = frenchHomePageData defaultDatabaseSeed,
                  englishSecondPageData = Left (SecondPageDataError "seed unavailable"),
                  frenchSecondPageData = frenchSecondPageData defaultDatabaseSeed
                }
          renderedPage =
            renderPageFromRouteData
              defaultAppConfig
              secondRequest
              (SecondRouteDataResult (Left (SecondPageDataError "seed unavailable")))
      selectResponseWithDatabase defaultAppConfig failingDatabaseEffect secondRequest
        `shouldReturn` HarchWeb.PageResponseWithMetadata
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = 500,
              HarchWeb.responseContentType = "text/html; charset=utf-8",
              HarchWeb.responseBody = "",
              HarchWeb.responseObservabilityAttributes =
                [ Observability.ObservabilityAttribute
                    { Observability.attributeName = "exception.type",
                      Observability.attributeValue = Observability.TextAttribute "SecondPageDataError"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "exception.message",
                      Observability.attributeValue = Observability.TextAttribute "seed unavailable"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "app.route",
                      Observability.attributeValue = Observability.TextAttribute "/second"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "app.surface",
                      Observability.attributeValue = Observability.TextAttribute "page"
                    }
                ],
              HarchWeb.responseLogEntries =
                ["Database failure while rendering required second-page page response: SecondPageDataError \"seed unavailable\""]
            }
          renderedPage

    it "maps required home-page failures into explicit HTML 500 responses" $ do
      let failingDatabaseEffect =
            buildSeededDatabaseEffect
              DatabaseSeed
                { englishHomePageData = Left (HomePageDataError "home seed unavailable"),
                  frenchHomePageData = frenchHomePageData defaultDatabaseSeed,
                  englishSecondPageData = englishSecondPageData defaultDatabaseSeed,
                  frenchSecondPageData = frenchSecondPageData defaultDatabaseSeed
                }
          renderedPage =
            renderPageFromRouteData
              defaultAppConfig
              homeRequest
              (HomeRouteDataResult (Left (HomePageDataError "home seed unavailable")))
      selectResponseWithDatabase defaultAppConfig failingDatabaseEffect homeRequest
        `shouldReturn` HarchWeb.PageResponseWithMetadata
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = 500,
              HarchWeb.responseContentType = "text/html; charset=utf-8",
              HarchWeb.responseBody = "",
              HarchWeb.responseObservabilityAttributes =
                [ Observability.ObservabilityAttribute
                    { Observability.attributeName = "exception.type",
                      Observability.attributeValue = Observability.TextAttribute "HomePageDataError"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "exception.message",
                      Observability.attributeValue = Observability.TextAttribute "home seed unavailable"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "app.route",
                      Observability.attributeValue = Observability.TextAttribute "/"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "app.surface",
                      Observability.attributeValue = Observability.TextAttribute "page"
                    }
                ],
              HarchWeb.responseLogEntries =
                ["Database failure while rendering required home-page page response: HomePageDataError \"home seed unavailable\""]
            }
          renderedPage

    it "keeps routes without required database data on their existing responses" $ do
      let failingDatabaseEffect =
            buildSeededDatabaseEffect
              DatabaseSeed
                { englishHomePageData = englishHomePageData defaultDatabaseSeed,
                  frenchHomePageData = frenchHomePageData defaultDatabaseSeed,
                  englishSecondPageData = Left (SecondPageDataError "seed unavailable"),
                  frenchSecondPageData = Left (SecondPageDataError "seed unavailable")
                }
      renderedHomePage <- renderPage defaultAppConfig homeRequest
      selectResponseWithDatabase defaultAppConfig failingDatabaseEffect homeRequest
        `shouldReturn` HarchWeb.PageResponse renderedHomePage
      selectResponseWithDatabase defaultAppConfig failingDatabaseEffect apiStatusRequest
        `shouldReturn` HarchWeb.BodyResponse
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = 200,
              HarchWeb.responseContentType = "application/json",
              HarchWeb.responseBody = "{\"status\":\"ok\",\"locale\":\"en\"}",
              HarchWeb.responseObservabilityAttributes = [],
              HarchWeb.responseLogEntries = []
            }

    it "is deterministic for repeated requests" $ do
      firstResponse <- selectResponse defaultAppConfig apiStatusRequest
      secondResponse <- selectResponse defaultAppConfig apiStatusRequest
      firstResponse `shouldBe` secondResponse

  describe "buildPageModel" $ do
    it "builds stubbed home page data with a navigation affordance" $
      buildPageModel homeRequest
        `shouldReturn` HomePage
          HomePageModel
            { homeHeading = "Home",
              homeSummary = "Server-rendered home page with stubbed content.",
              homeErrorMessage = Nothing,
              homePrimaryAction =
                CallToAction
                  { callToActionLabel = "Browse the second page",
                    callToActionRoute = SecondRoute,
                    callToActionHref = "/second"
                  }
            }

    it "keeps locale-aware action paths in stubbed page data" $
      buildPageModel frenchHomeRequest
        `shouldReturn` HomePage
          HomePageModel
            { homeHeading = "Home",
              homeSummary = "Accueil cote serveur avec des donnees de developpement preconfigurees.",
              homeErrorMessage = Nothing,
              homePrimaryAction =
                CallToAction
                  { callToActionLabel = "Browse the second page",
                    callToActionRoute = SecondRoute,
                    callToActionHref = "/fr/second"
                  }
            }

    it "builds explicit home-page error state when the database effect fails" $
      buildPageModelWithDatabase
        ( buildSeededDatabaseEffect
            DatabaseSeed
              { englishHomePageData = Left (HomePageDataError "home seed unavailable"),
                frenchHomePageData = frenchHomePageData defaultDatabaseSeed,
                englishSecondPageData = englishSecondPageData defaultDatabaseSeed,
                frenchSecondPageData = frenchSecondPageData defaultDatabaseSeed
              }
        )
        homeRequest
        `shouldReturn` HomePage
          HomePageModel
            { homeHeading = "Home",
              homeSummary = "Home page content is temporarily unavailable.",
              homeErrorMessage = Just "Could not load home page data.",
              homePrimaryAction =
                CallToAction
                  { callToActionLabel = "Browse the second page",
                    callToActionRoute = SecondRoute,
                    callToActionHref = "/second"
                  }
            }

    it "renders selected route data into both page models and API responses" $ do
      let selectedRouteData =
            SecondRouteDataResult
              ( Right
                  SecondRouteData
                    { secondRouteSummary = "Shared domain summary.",
                      secondRouteHighlights = ["Shared loader", "Shared renderer"]
                    }
              )
      buildPageModelFromRouteData secondRequest selectedRouteData
        `shouldBe` SecondPage
          SecondPageModel
            { secondHeading = "Second",
              secondSummary = "Shared domain summary.",
              secondHighlights = ["Shared loader", "Shared renderer"],
              secondErrorMessage = Nothing,
              secondPrimaryAction =
                CallToAction
                  { callToActionLabel = "Return home",
                    callToActionRoute = HomeRoute,
                    callToActionHref = "/"
                  }
            }
      renderApiResponseFromRouteData selectedRouteData
        `shouldBe` HarchWeb.ResponseBody
          { HarchWeb.responseStatus = 200,
            HarchWeb.responseContentType = "application/json",
            HarchWeb.responseBody = "{\"summary\":\"Shared domain summary.\",\"highlights\":[\"Shared loader\",\"Shared renderer\"]}",
            HarchWeb.responseObservabilityAttributes = [],
            HarchWeb.responseLogEntries = []
          }

    it "loads second-page content from the database effect when provided" $
      buildPageModelWithDatabase
        ( buildSeededDatabaseEffect
            DatabaseSeed
              { englishHomePageData = englishHomePageData defaultDatabaseSeed,
                frenchHomePageData = frenchHomePageData defaultDatabaseSeed,
                englishSecondPageData =
                  Right
                    SecondPageData
                      { secondPageDataSummary = "Loaded from the seeded database effect.",
                        secondPageDataHighlights = ["Fast SSR", "Progressive enhancement"]
                      },
                frenchSecondPageData = frenchSecondPageData defaultDatabaseSeed
              }
        )
        secondRequest
        `shouldReturn` SecondPage
          SecondPageModel
            { secondHeading = "Second",
              secondSummary = "Loaded from the seeded database effect.",
              secondHighlights = ["Fast SSR", "Progressive enhancement"],
              secondErrorMessage = Nothing,
              secondPrimaryAction =
                CallToAction
                  { callToActionLabel = "Return home",
                    callToActionRoute = HomeRoute,
                    callToActionHref = "/"
                  }
            }

    it "builds an explicit error-state second page when the database effect fails" $
      buildPageModelWithDatabase
        ( buildSeededDatabaseEffect
            DatabaseSeed
              { englishHomePageData = englishHomePageData defaultDatabaseSeed,
                frenchHomePageData = frenchHomePageData defaultDatabaseSeed,
                englishSecondPageData = Left (SecondPageDataError "seed unavailable"),
                frenchSecondPageData = frenchSecondPageData defaultDatabaseSeed
              }
        )
        secondRequest
        `shouldReturn` SecondPage
          SecondPageModel
            { secondHeading = "Second",
              secondSummary = "Second page content is temporarily unavailable.",
              secondHighlights = [],
              secondErrorMessage = Just "Could not load second page data.",
              secondPrimaryAction =
                CallToAction
                  { callToActionLabel = "Return home",
                    callToActionRoute = HomeRoute,
                    callToActionHref = "/"
                  }
            }

  describe "renderPageBody" $ do
    it "renders the home page heading and navigation affordance" $ do
      homePageModel <- buildPageModel homeRequest
      renderPageBody homePageModel
        `shouldBe` "<section data-page=\"home\"><h1 data-page-title=\"true\">Home</h1><p>Server-rendered home page with stubbed content.</p><p><a href=\"/second\" data-page-link=\"true\">Browse the second page</a></p></section>"

    it "renders the second page with distinct content while the shared shell stays the same" $ do
      homeShell <- renderedShell defaultAppConfig HomeRoute
      secondShell <- renderedShell defaultAppConfig SecondRoute
      secondPageModel <- buildPageModel secondRequest
      renderPageBody secondPageModel
        `shouldBe` "<section data-page=\"second\"><h1 data-page-title=\"true\">Second</h1><p>Second page content with stubbed data ready for future loaders.</p><p data-empty-state=\"true\">No highlights yet.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section>"
      Text.isInfixOf "<nav data-navigation-region=\"primary\"><a href=\"/\" aria-current=\"page\">Home</a><a href=\"/second\">Second</a></nav><main id=\"app-main\" data-navigation-content=\"true\">" homeShell `shouldBe` True
      Text.isInfixOf "<nav data-navigation-region=\"primary\"><a href=\"/\">Home</a><a href=\"/second\" aria-current=\"page\">Second</a></nav><main id=\"app-main\" data-navigation-content=\"true\" data-bootstrap-hooks=\"second-page\">" secondShell `shouldBe` True

    it "preserves page-body HTML invariants needed for later navigation enhancement" $ do
      homePageModel <- buildPageModel homeRequest
      secondPageModel <- buildPageModel secondRequest
      let homeBody = renderPageBody homePageModel
          secondBody = renderPageBody secondPageModel
      Text.isInfixOf "<section data-page=\"home\">" homeBody `shouldBe` True
      Text.isInfixOf "<section data-page=\"second\">" secondBody `shouldBe` True
      Text.isInfixOf "data-page-title=\"true\"" homeBody `shouldBe` True
      Text.isInfixOf "data-page-link=\"true\"" secondBody `shouldBe` True
      Text.isInfixOf "<main" homeBody `shouldBe` False
      Text.isInfixOf "<body" secondBody `shouldBe` False

    it "covers empty and populated highlight rendering branches" $ do
      secondPageModel <- buildPageModel secondRequest
      Text.isInfixOf "<p data-empty-state=\"true\">No highlights yet.</p>" (renderPageBody secondPageModel) `shouldBe` True
      renderPageBody
        ( SecondPage
            SecondPageModel
              { secondHeading = "Second",
                secondSummary = "Second page content with stubbed data ready for future loaders.",
                secondHighlights = ["Fast SSR", "Stable routes"],
                secondErrorMessage = Nothing,
                secondPrimaryAction =
                  CallToAction
                    { callToActionLabel = "Return home",
                      callToActionRoute = HomeRoute,
                      callToActionHref = "/"
                    }
              }
        )
        `shouldBe` "<section data-page=\"second\"><h1 data-page-title=\"true\">Second</h1><p>Second page content with stubbed data ready for future loaders.</p><ul><li>Fast SSR</li><li>Stable routes</li></ul><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section>"

    it "renders an explicit error state when the second-page load fails" $
      renderPageWithDatabase
        defaultAppConfig
        ( buildSeededDatabaseEffect
            DatabaseSeed
              { englishHomePageData = englishHomePageData defaultDatabaseSeed,
                frenchHomePageData = frenchHomePageData defaultDatabaseSeed,
                englishSecondPageData = Left (SecondPageDataError "seed unavailable"),
                frenchSecondPageData = frenchSecondPageData defaultDatabaseSeed
              }
        )
        secondRequest
        `shouldReturn` HarchWeb.Page
          { HarchWeb.pageTitle = "web-api: Second",
            HarchWeb.pageRoute = SecondRoute,
            HarchWeb.pageContext = defaultRequestContext,
            HarchWeb.pageBody = "<section data-page=\"second\"><h1 data-page-title=\"true\">Second</h1><p data-error-state=\"true\">Could not load second page data.</p><p>Second page content is temporarily unavailable.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section>",
            HarchWeb.pageBootstrapHooks = ["second-page"]
          }

    it "renders an explicit error state when the home-page load fails" $
      renderPageWithDatabase
        defaultAppConfig
        ( buildSeededDatabaseEffect
            DatabaseSeed
              { englishHomePageData = Left (HomePageDataError "home seed unavailable"),
                frenchHomePageData = frenchHomePageData defaultDatabaseSeed,
                englishSecondPageData = englishSecondPageData defaultDatabaseSeed,
                frenchSecondPageData = frenchSecondPageData defaultDatabaseSeed
              }
        )
        homeRequest
        `shouldReturn` HarchWeb.Page
          { HarchWeb.pageTitle = "web-api: Home",
            HarchWeb.pageRoute = HomeRoute,
            HarchWeb.pageContext = defaultRequestContext,
            HarchWeb.pageBody = "<section data-page=\"home\"><h1 data-page-title=\"true\">Home</h1><p data-error-state=\"true\">Could not load home page data.</p><p>Home page content is temporarily unavailable.</p><p><a href=\"/second\" data-page-link=\"true\">Browse the second page</a></p></section>",
            HarchWeb.pageBootstrapHooks = []
          }

  describe "page shell integration" $ do
    it "keeps client-only enhancement hooks in the app seam instead of page rendering" $ do
      pageEnhancementHooks HomeRoute `shouldBe` []
      pageEnhancementHooks SecondRoute `shouldBe` ["second-page"]
      pageEnhancementHooks StatusApiRoute `shouldBe` []
      pageEnhancementHooks NotFoundRoute `shouldBe` []

    it "marks the active navigation item for each routed page" $ do
      homeShell <- renderedShell defaultAppConfig HomeRoute
      secondShell <- renderedShell defaultAppConfig SecondRoute
      notFoundShell <- renderedShell defaultAppConfig NotFoundRoute
      Text.isInfixOf "<a href=\"/\" aria-current=\"page\">Home</a><a href=\"/second\">Second</a>" homeShell `shouldBe` True
      Text.isInfixOf "<a href=\"/\">Home</a><a href=\"/second\" aria-current=\"page\">Second</a>" secondShell `shouldBe` True
      Text.isInfixOf "aria-current=\"page\"" notFoundShell `shouldBe` False

    it "emits deterministic navigation hooks and script references when assets are configured" $ do
      let rootMountedConfig =
            navigationAppConfig
              { staticAssets =
                  StaticAssetsConfig
                    { staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = "/", staticDirectory = "public"}],
                      staticAssetContentTypes = defaultStaticAssetContentTypes,
                      staticCacheControlSeconds = Nothing
                    }
              }
      homeShellWithoutAssets <- renderedShell defaultAppConfig HomeRoute
      homeShell <- renderedShell navigationAppConfig HomeRoute
      secondShell <- renderedShell navigationAppConfig SecondRoute
      rootMountedShell <- renderedShell rootMountedConfig HomeRoute
      Text.isInfixOf "<script src=\"/assets/navigation.js\" defer></script>" homeShellWithoutAssets `shouldBe` False
      Text.isInfixOf "<script src=\"/assets/navigation.js\" defer></script>" homeShell `shouldBe` True
      Text.isInfixOf "<script src=\"/navigation.js\" defer></script>" rootMountedShell `shouldBe` True
      Text.isInfixOf "<nav data-navigation-region=\"primary\">" homeShell `shouldBe` True
      Text.isInfixOf "<main id=\"app-main\" data-navigation-content=\"true\">" homeShell `shouldBe` True
      Text.isInfixOf "data-bootstrap-hooks" homeShell `shouldBe` False
      Text.isInfixOf "<main id=\"app-main\" data-navigation-content=\"true\" data-bootstrap-hooks=\"second-page\">" secondShell `shouldBe` True

    it "renders navigation and script hrefs under the forwarded request path prefix" $ do
      prefixedShell <- renderedShellForRequest navigationAppConfig prefixedSecondRequest
      Text.isInfixOf "<a href=\"/app\">Home</a><a href=\"/app/second\" aria-current=\"page\">Second</a>" prefixedShell `shouldBe` True
      Text.isInfixOf "<script src=\"/app/assets/navigation.js\" defer></script>" prefixedShell `shouldBe` True

    it "serves the bundled navigation asset through configured static roots" $ do
      response <- performWaiRequest (HarchWeb.toWaiApplication (buildApp navigationAppConfig)) (waiRequest ["assets", "navigation.js"])
      Wai.responseStatus response `shouldBe` Http.status200
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just "application/javascript; charset=utf-8"
      responseBody <- readResponseBody response
      Text.isInfixOf "data-page-link" responseBody `shouldBe` True
      Text.isInfixOf "popstate" responseBody `shouldBe` True

    it "serves bundled style, font, and resource assets through configured static roots" $ do
      stylesheetResponse <- performWaiRequest (HarchWeb.toWaiApplication (buildApp navigationAppConfig)) (waiRequest ["assets", "styles", "app.css"])
      Wai.responseStatus stylesheetResponse `shouldBe` Http.status200
      lookup Http.hContentType (Wai.responseHeaders stylesheetResponse) `shouldBe` Just "text/css; charset=utf-8"
      stylesheetBody <- readResponseBody stylesheetResponse
      Text.isInfixOf "font-family: system-ui, sans-serif;" stylesheetBody `shouldBe` True

      fontStylesheetResponse <- performWaiRequest (HarchWeb.toWaiApplication (buildApp navigationAppConfig)) (waiRequest ["assets", "fonts", "font-faces.css"])
      Wai.responseStatus fontStylesheetResponse `shouldBe` Http.status200
      lookup Http.hContentType (Wai.responseHeaders fontStylesheetResponse) `shouldBe` Just "text/css; charset=utf-8"
      fontStylesheetBody <- readResponseBody fontStylesheetResponse
      Text.isInfixOf "@font-face" fontStylesheetBody `shouldBe` True

      faviconResponse <- performWaiRequest (HarchWeb.toWaiApplication (buildApp navigationAppConfig)) (waiRequest ["assets", "resources", "favicon.svg"])
      Wai.responseStatus faviconResponse `shouldBe` Http.status200
      lookup Http.hContentType (Wai.responseHeaders faviconResponse) `shouldBe` Just "image/svg+xml"
      faviconBody <- readResponseBody faviconResponse
      Text.isInfixOf "<svg" faviconBody `shouldBe` True

    it "keeps shell output identical for repeated renders of the same page input" $ do
      let application = buildApp defaultAppConfig
      page <- renderPage defaultAppConfig frenchSecondRequest
      HarchWeb.pageShell application page `shouldBe` HarchWeb.pageShell application page

    it "keeps the legacy page-shell shim aligned with the app shell seam" $ do
      renderedPage <- renderPage defaultAppConfig secondRequest
      LegacyPageShell.buildAppPageShell defaultAppConfig renderedPage
        `shouldBe` buildAppPageShell defaultAppConfig renderedPage

    it "keeps not-found pages inside the shared shell" $
      renderedShell defaultAppConfig NotFoundRoute
        `shouldReturn` "<html><head><title>web-api: Not Found</title></head><body data-app=\"web-api\"><nav data-navigation-region=\"primary\"><a href=\"/\">Home</a><a href=\"/second\">Second</a></nav><main id=\"app-main\" data-navigation-content=\"true\"><section data-page=\"not-found\"><h1 data-page-title=\"true\">Not Found</h1><p>The requested page could not be found.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section></main></body></html>"

  describe "buildApp" $ do
    it "constructs the application description against the HarchWeb facade" $
      HarchWeb.appName pureApplication `shouldBe` "web-api"

    it "stores the default request context used by the WAI adapter" $
      HarchWeb.defaultRequestContext pureApplication `shouldBe` defaultRequestContext

    it "derives normalized forwarded path prefixes into the request context used by the WAI adapter" $ do
      let forwardedPrefixRequest =
            (waiRequest ["second"])
              { Wai.requestHeaders = [("X-Forwarded-Prefix", "app, /ignored")]
              }
          emptyForwardedPrefixRequest =
            (waiRequest ["second"])
              { Wai.requestHeaders = [("X-Forwarded-Prefix", ", ")]
              }
      HarchWeb.requestContextFromRequest pureApplication forwardedPrefixRequest defaultRequestContext
        `shouldBe` defaultRequestContext {requestPathPrefix = "/app"}
      HarchWeb.requestContextFromRequest pureApplication emptyForwardedPrefixRequest defaultRequestContext
        `shouldBe` defaultRequestContext

    it "stores the configured static assets used by the WAI adapter" $
      HarchWeb.applicationStaticAssets pureApplication `shouldBe` staticAssets defaultAppConfig

    it "keeps pure-app observability and log reporters as no-ops" $ do
      HarchWeb.reportRequestObservability
        pureApplication
        ( Observability.buildRequestObservability
            "GET"
            "http"
            "/"
            "/"
            200
            Observability.PageResponseKind
            []
        )
      HarchWeb.reportConnectionObservability
        pureApplication
        ( Observability.buildConnectionObservability
            "CONNECTION insecure-connection-denied"
            [ Observability.ObservabilityAttribute
                { Observability.attributeName = "network.peer.address",
                  Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
                }
            ]
        )
      HarchWeb.reportApplicationLog pureApplication "ignored"

    it "stores the same route codec behavior used by direct route tests" $ do
      let codec = HarchWeb.routeCodec pureApplication
      HarchWeb.parseRoute codec defaultRequestContext "/" `shouldBe` parseRoute defaultRequestContext "/"
      HarchWeb.parseRoute codec defaultRequestContext "/fr" `shouldBe` parseRoute defaultRequestContext "/fr"
      HarchWeb.parseRoute codec defaultRequestContext "/second" `shouldBe` parseRoute defaultRequestContext "/second"
      HarchWeb.parseRoute codec defaultRequestContext "/api/status" `shouldBe` parseRoute defaultRequestContext "/api/status"
      HarchWeb.parseRoute codec defaultRequestContext "/api/second" `shouldBe` parseRoute defaultRequestContext "/api/second"
      HarchWeb.parseRoute codec defaultRequestContext "/missing" `shouldBe` Nothing
      HarchWeb.renderRoute codec homeRequest `shouldBe` renderRoutePath homeRequest
      HarchWeb.renderRoute codec frenchSecondRequest `shouldBe` renderRoutePath frenchSecondRequest
      HarchWeb.renderRoute codec secondRequest `shouldBe` renderRoutePath secondRequest
      HarchWeb.renderRoute codec apiStatusRequest `shouldBe` renderRoutePath apiStatusRequest
      HarchWeb.renderRoute codec apiSecondRequest `shouldBe` renderRoutePath apiSecondRequest
      HarchWeb.renderRoute codec apiNotFoundRequest `shouldBe` renderRoutePath apiNotFoundRequest
      HarchWeb.renderRoute codec notFoundRequest `shouldBe` renderRoutePath notFoundRequest
      HarchWeb.notFoundRequest codec defaultRequestContext `shouldBe` notFoundRequest

    it "stores the same response-selection behavior used by direct response tests" $ do
      expectedHomeResponse <- selectResponse defaultAppConfig homeRequest
      expectedSecondResponse <- selectResponse defaultAppConfig secondRequest
      expectedApiStatusResponse <- selectResponse defaultAppConfig apiStatusRequest
      expectedApiSecondResponse <- selectResponse defaultAppConfig apiSecondRequest
      expectedNotFoundResponse <- selectResponse defaultAppConfig notFoundRequest
      expectedApiNotFoundResponse <- selectResponse defaultAppConfig apiNotFoundRequest
      HarchWeb.renderResponse pureApplication homeRequest `shouldReturn` expectedHomeResponse
      HarchWeb.renderResponse pureApplication secondRequest `shouldReturn` expectedSecondResponse
      HarchWeb.renderResponse pureApplication apiStatusRequest `shouldReturn` expectedApiStatusResponse
      HarchWeb.renderResponse pureApplication apiSecondRequest `shouldReturn` expectedApiSecondResponse
      HarchWeb.renderResponse pureApplication notFoundRequest `shouldReturn` expectedNotFoundResponse
      HarchWeb.renderResponse pureApplication apiNotFoundRequest `shouldReturn` expectedApiNotFoundResponse

    it "adapts the pure application to WAI without changing rendered pages" $ do
      secondResponse <- performWaiRequest (HarchWeb.toWaiApplication pureApplication) (waiRequest ["fr", "second"])
      Wai.responseStatus secondResponse `shouldBe` Http.status200
      lookup Http.hContentType (Wai.responseHeaders secondResponse) `shouldBe` Just (TextEncoding.encodeUtf8 "text/html; charset=utf-8")
      renderedPage <- renderPage defaultAppConfig frenchSecondRequest
      readResponseBody secondResponse
        `shouldReturn` HarchWeb.pageShell pureApplication renderedPage

      apiStatusResponse <- performWaiRequest (HarchWeb.toWaiApplication pureApplication) (waiRequest ["api", "status"])
      Wai.responseStatus apiStatusResponse `shouldBe` Http.status200
      lookup Http.hContentType (Wai.responseHeaders apiStatusResponse) `shouldBe` Just (TextEncoding.encodeUtf8 "application/json")
      readResponseBody apiStatusResponse
        `shouldReturn` "{\"status\":\"ok\",\"locale\":\"en\"}"

      apiSecondResponse <- performWaiRequest (HarchWeb.toWaiApplication pureApplication) (waiRequest ["api", "second"])
      Wai.responseStatus apiSecondResponse `shouldBe` Http.status200
      lookup Http.hContentType (Wai.responseHeaders apiSecondResponse) `shouldBe` Just (TextEncoding.encodeUtf8 "application/json")
      readResponseBody apiSecondResponse
        `shouldReturn` "{\"summary\":\"Second page content with stubbed data ready for future loaders.\",\"highlights\":[]}"

      missingResponse <- performWaiRequest (HarchWeb.toWaiApplication pureApplication) (waiRequest ["missing"])
      Wai.responseStatus missingResponse `shouldBe` Http.status404
      lookup Http.hContentType (Wai.responseHeaders missingResponse) `shouldBe` Just (TextEncoding.encodeUtf8 "text/html; charset=utf-8")
      notFoundPage <- renderPage defaultAppConfig notFoundRequest
      readResponseBody missingResponse
        `shouldReturn` HarchWeb.pageShell pureApplication notFoundPage

      apiMissingResponse <- performWaiRequest (HarchWeb.toWaiApplication pureApplication) (waiRequest ["api", "missing"])
      Wai.responseStatus apiMissingResponse `shouldBe` Http.status404
      lookup Http.hContentType (Wai.responseHeaders apiMissingResponse) `shouldBe` Just (TextEncoding.encodeUtf8 "application/json")
      readResponseBody apiMissingResponse
        `shouldReturn` "{\"error\":\"not-found\"}"

    it "adapts forwarded path prefixes through the WAI facade for pages and static assets" $ do
      let prefixedPageRequest =
            (waiRequest ["app", "second"])
              { Wai.requestHeaders = [("X-Forwarded-Prefix", "/app")]
              }
          prefixedAssetRequest =
            (waiRequest ["app", "assets", "navigation.js"])
              { Wai.requestHeaders = [("X-Forwarded-Prefix", "/app")]
              }
          prefixedApplication = buildApp navigationAppConfig
      pageResponse <- performWaiRequest (HarchWeb.toWaiApplication prefixedApplication) prefixedPageRequest
      Wai.responseStatus pageResponse `shouldBe` Http.status200
      pageBody <- readResponseBody pageResponse
      Text.isInfixOf "<a href=\"/app\">Home</a><a href=\"/app/second\" aria-current=\"page\">Second</a>" pageBody `shouldBe` True
      Text.isInfixOf "<script src=\"/app/assets/navigation.js\" defer></script>" pageBody `shouldBe` True

      assetResponse <- performWaiRequest (HarchWeb.toWaiApplication prefixedApplication) prefixedAssetRequest
      Wai.responseStatus assetResponse `shouldBe` Http.status200
      lookup Http.hContentType (Wai.responseHeaders assetResponse) `shouldBe` Just "application/javascript; charset=utf-8"

    it "returns HTTP 500 for required page failures while keeping unaffected routes unchanged" $ do
      let failingApplication =
            buildAppWithDatabase
              defaultAppConfig
              ( buildSeededDatabaseEffect
                  DatabaseSeed
                    { englishHomePageData = englishHomePageData defaultDatabaseSeed,
                      frenchHomePageData = frenchHomePageData defaultDatabaseSeed,
                      englishSecondPageData = Left (SecondPageDataError "seed unavailable"),
                      frenchSecondPageData = frenchSecondPageData defaultDatabaseSeed
                    }
              )
      secondResponse <- performWaiRequest (HarchWeb.toWaiApplication failingApplication) (waiRequest ["second"])
      Wai.responseStatus secondResponse `shouldBe` Http.internalServerError500
      lookup Http.hContentType (Wai.responseHeaders secondResponse) `shouldBe` Just (TextEncoding.encodeUtf8 "text/html; charset=utf-8")
      secondResponseBody <- readResponseBody secondResponse
      secondResponseBody `shouldSatisfy` Text.isInfixOf "Second page content is temporarily unavailable."

      homeResponse <- performWaiRequest (HarchWeb.toWaiApplication failingApplication) (waiRequest [])
      Wai.responseStatus homeResponse `shouldBe` Http.status200
      lookup Http.hContentType (Wai.responseHeaders homeResponse) `shouldBe` Just (TextEncoding.encodeUtf8 "text/html; charset=utf-8")

    it "is structurally complete enough to render supported and not-found shells" $ do
      homePage <- renderPage defaultAppConfig homeRequest
      secondPage <- renderPage defaultAppConfig secondRequest
      notFoundPage <- renderPage defaultAppConfig notFoundRequest
      HarchWeb.pageShell pureApplication homePage
        `shouldBe` "<html><head><title>web-api: Home</title></head><body data-app=\"web-api\"><nav data-navigation-region=\"primary\"><a href=\"/\" aria-current=\"page\">Home</a><a href=\"/second\">Second</a></nav><main id=\"app-main\" data-navigation-content=\"true\"><section data-page=\"home\"><h1 data-page-title=\"true\">Home</h1><p>Server-rendered home page with stubbed content.</p><p><a href=\"/second\" data-page-link=\"true\">Browse the second page</a></p></section></main></body></html>"
      HarchWeb.pageShell pureApplication secondPage
        `shouldBe` "<html><head><title>web-api: Second</title></head><body data-app=\"web-api\"><nav data-navigation-region=\"primary\"><a href=\"/\">Home</a><a href=\"/second\" aria-current=\"page\">Second</a></nav><main id=\"app-main\" data-navigation-content=\"true\" data-bootstrap-hooks=\"second-page\"><section data-page=\"second\"><h1 data-page-title=\"true\">Second</h1><p>Second page content with stubbed data ready for future loaders.</p><p data-empty-state=\"true\">No highlights yet.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section></main></body></html>"
      HarchWeb.pageShell pureApplication notFoundPage
        `shouldBe` "<html><head><title>web-api: Not Found</title></head><body data-app=\"web-api\"><nav data-navigation-region=\"primary\"><a href=\"/\">Home</a><a href=\"/second\">Second</a></nav><main id=\"app-main\" data-navigation-content=\"true\"><section data-page=\"not-found\"><h1 data-page-title=\"true\">Not Found</h1><p>The requested page could not be found.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section></main></body></html>"

    it "can grow from page responses to API responses without changing route matching" $ do
      renderedResponse <- HarchWeb.renderResponse pureApplication apiSecondRequest
      case renderedResponse of
        HarchWeb.BodyResponse body -> HarchWeb.responseBody body `shouldBe` "{\"summary\":\"Second page content with stubbed data ready for future loaders.\",\"highlights\":[]}"
        HarchWeb.PageResponse _ -> expectationFailure "expected body response"
        HarchWeb.PageResponseWithMetadata _ _ -> expectationFailure "expected body response"

  describe "buildRuntimeApp" $ do
    it "builds the runtime database effect from the environment config" $ do
      let runtimeEnvironmentConfig =
            defaultAppEnvironmentConfig
              { databaseConfig =
                  postgresTestConfig
                    { databaseName = "runtime_db",
                      databaseUser = "runtime_user"
                    }
              }
          runtimeApplication =
            buildRuntimeAppWithDatabaseBuilder
              defaultAppConfig
              ( \databaseRuntimeConfig ->
                  buildSeededDatabaseEffect
                    defaultDatabaseSeed
                      { englishSecondPageData =
                          Right
                            SecondPageData
                              { secondPageDataSummary =
                                  "runtime:" <> databaseName databaseRuntimeConfig <> ":" <> databaseUser databaseRuntimeConfig,
                                secondPageDataHighlights = ["configured-from-environment"]
                              }
                      }
              )
              runtimeEnvironmentConfig
      runtimeResponse <- HarchWeb.renderResponse runtimeApplication apiSecondRequest
      case runtimeResponse of
        HarchWeb.BodyResponse body ->
          HarchWeb.responseBody body
            `shouldBe` "{\"summary\":\"runtime:runtime_db:runtime_user\",\"highlights\":[\"configured-from-environment\"]}"
        HarchWeb.PageResponse _ -> expectationFailure "expected body response"
        HarchWeb.PageResponseWithMetadata _ _ -> expectationFailure "expected body response"
      HarchWeb.reportRequestObservability
        runtimeApplication
        ( Observability.buildRequestObservability
            "GET"
            "http"
            "/second"
            "/second"
            500
            Observability.BodyResponseKind
            [ Observability.ObservabilityAttribute
                { Observability.attributeName = "exception.type",
                  Observability.attributeValue = Observability.TextAttribute "SecondPageDataError"
                }
            ]
        )
      HarchWeb.reportConnectionObservability
        runtimeApplication
        ( Observability.buildConnectionObservability
            "CONNECTION insecure-connection-denied"
            [ Observability.ObservabilityAttribute
                { Observability.attributeName = "network.peer.address",
                  Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
                }
            ]
        )
      HarchWeb.reportApplicationLog runtimeApplication "runtime failure detail"

    it "exports runtime request observability to the configured OTLP tracing endpoint" $
      withOtlpCaptureServer Http.ok200 "{}" $ \collectorUrl capturedRequestReference -> do
        let runtimeAppConfig =
              defaultAppConfig
                { observability =
                    (observability defaultAppConfig)
                      { tracingExporter =
                          Just
                            OtlpExporter
                              { otlpEndpoint = collectorUrl,
                                otlpHeaders = [("x-runtime-trace", "enabled")]
                              }
                      }
                }
            runtimeApplication =
              buildRuntimeAppWithDatabaseBuilder
                runtimeAppConfig
                (const defaultDatabaseEffect)
                defaultAppEnvironmentConfig
        HarchWeb.reportRequestObservability
          runtimeApplication
          (Observability.buildRequestObservability "GET" "http" "/api/status" "/api/status" 200 Observability.BodyResponseKind [])
        CapturedOtlpRequest
          { capturedOtlpMethod = requestMethod,
            capturedOtlpPath = requestPath,
            capturedOtlpHeaders = requestHeaders,
            capturedOtlpBody = requestBody
          } <-
          readMVar capturedRequestReference
        let requestBodyText = TextEncoding.decodeUtf8 requestBody
        requestMethod `shouldBe` "POST"
        requestPath `shouldBe` "/v1/traces"
        lookup "content-type" requestHeaders `shouldBe` Just "application/json"
        lookup "x-runtime-trace" requestHeaders `shouldBe` Just "enabled"
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"service.name\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"web-api\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"name\":\"GET /api/status\""
        requestBodyText `shouldSatisfy` (not . Text.isInfixOf "\"STATUS_CODE_ERROR\"")

    it "keeps runtime request reporting alive when the OTLP collector rejects the export" $
      withOtlpCaptureServer Http.serviceUnavailable503 "{\"error\":\"collector unavailable\"}" $ \collectorUrl capturedRequestReference -> do
        let runtimeAppConfig =
              defaultAppConfig
                { observability =
                    (observability defaultAppConfig)
                      { tracingExporter =
                          Just
                            OtlpExporter
                              { otlpEndpoint = collectorUrl,
                                otlpHeaders = []
                              }
                      }
                }
            runtimeApplication =
              buildRuntimeAppWithDatabaseBuilder
                runtimeAppConfig
                (const defaultDatabaseEffect)
                defaultAppEnvironmentConfig
        HarchWeb.reportRequestObservability
          runtimeApplication
          (Observability.buildRequestObservability "GET" "http" "/api/second" "/api/second" 500 Observability.BodyResponseKind [])
        CapturedOtlpRequest
          { capturedOtlpMethod = requestMethod,
            capturedOtlpPath = requestPath
          } <-
          readMVar capturedRequestReference
        requestMethod `shouldBe` "POST"
        requestPath `shouldBe` "/v1/traces"

    it "exports runtime connection observability to the configured OTLP tracing endpoint" $
      withOtlpCaptureServer Http.ok200 "{}" $ \collectorUrl capturedRequestReference -> do
        let runtimeAppConfig =
              defaultAppConfig
                { observability =
                    (observability defaultAppConfig)
                      { tracingExporter =
                          Just
                            OtlpExporter
                              { otlpEndpoint = collectorUrl,
                                otlpHeaders = [("x-runtime-trace", "enabled")]
                              }
                      }
                }
            runtimeApplication =
              buildRuntimeAppWithDatabaseBuilder
                runtimeAppConfig
                (const defaultDatabaseEffect)
                defaultAppEnvironmentConfig
        HarchWeb.reportConnectionObservability
          runtimeApplication
          ( Observability.buildConnectionObservability
              "CONNECTION insecure-connection-denied"
              [ Observability.ObservabilityAttribute
                  { Observability.attributeName = "network.peer.address",
                    Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "exception.type",
                    Observability.attributeValue = Observability.TextAttribute "InsecureConnectionDenied"
                  }
              ]
          )
        CapturedOtlpRequest
          { capturedOtlpMethod = requestMethod,
            capturedOtlpPath = requestPath,
            capturedOtlpHeaders = requestHeaders,
            capturedOtlpBody = requestBody
          } <-
          readMVar capturedRequestReference
        let requestBodyText = TextEncoding.decodeUtf8 requestBody
        requestMethod `shouldBe` "POST"
        requestPath `shouldBe` "/v1/traces"
        lookup "content-type" requestHeaders `shouldBe` Just "application/json"
        lookup "x-runtime-trace" requestHeaders `shouldBe` Just "enabled"
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"name\":\"CONNECTION insecure-connection-denied\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"network.peer.address\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"InsecureConnectionDenied\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"STATUS_CODE_ERROR\""

    it "keeps runtime connection reporting alive when the OTLP collector rejects the export" $
      withOtlpCaptureServer Http.serviceUnavailable503 "{\"error\":\"collector unavailable\"}" $ \collectorUrl capturedRequestReference -> do
        let runtimeAppConfig =
              defaultAppConfig
                { observability =
                    (observability defaultAppConfig)
                      { tracingExporter =
                          Just
                            OtlpExporter
                              { otlpEndpoint = collectorUrl,
                                otlpHeaders = []
                              }
                      }
                }
            runtimeApplication =
              buildRuntimeAppWithDatabaseBuilder
                runtimeAppConfig
                (const defaultDatabaseEffect)
                defaultAppEnvironmentConfig
        HarchWeb.reportConnectionObservability
          runtimeApplication
          ( Observability.buildConnectionObservability
              "CONNECTION client-closed-connection-prematurely"
              [ Observability.ObservabilityAttribute
                  { Observability.attributeName = "network.peer.address",
                    Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
                  }
              ]
          )
        CapturedOtlpRequest
          { capturedOtlpMethod = requestMethod,
            capturedOtlpPath = requestPath
          } <-
          readMVar capturedRequestReference
        requestMethod `shouldBe` "POST"
        requestPath `shouldBe` "/v1/traces"

  describe "run" $ do
    it "starts the runtime server from an explicit environment and app config" $
      withUnusedTcpEndpoint $ \unusedEndpoint ->
        withSystemTempFile "web-api-runtime-output.txt" $ \outputPath outputHandle -> do
          completionReference <- newIORef Nothing
          let runtimeAppConfig =
                defaultAppConfig
                  { listenerConfigs =
                      [ ListenerConfig
                          { listenerHost = tcpEndpointHost unusedEndpoint,
                            listenerPort = tcpEndpointPort unusedEndpoint,
                            listenerScheme = Http,
                            listenerTls = Nothing,
                            listenerAcme = Nothing
                          }
                      ]
                  }
          serverThreadId <- forkIO $ do
            result <- try (runWithConfig outputHandle runtimeAppConfig defaultAppEnvironmentConfig) :: IO (Either SomeException ())
            writeIORef completionReference (Just result)
          responseText <- waitForRuntimeServerResponse completionReference (tcpEndpointPort unusedEndpoint) "/api/status"
          responseText `shouldBe` "{\"status\":\"ok\",\"locale\":\"en\"}"
          completionResult <- readIORef completionReference
          completionResult `shouldSatisfy` isNothing
          killThread serverThreadId
          waitForRuntimeServerExit completionReference
          hClose outputHandle
          readFile outputPath
            `shouldReturn` unlines
              [ "Parsed listener config: http://127.0.0.1:" <> show (tcpEndpointPort unusedEndpoint),
                "HTTP Server listening at http://127.0.0.1:" <> show (tcpEndpointPort unusedEndpoint)
              ]

    it "surfaces listener bind failures through the default app config" $
      withDefaultRuntimePortUnavailable $
        withSystemTempFile "web-api-runtime-output.txt" $ \_ outputHandle ->
          runWithConfig outputHandle defaultAppConfig defaultAppEnvironmentConfig
            `shouldThrow` isAlreadyInUseError

    it "serves database-backed runtime routes from the supplied environment config" $
      withContainerizedPsqlOnPath $ do
        ensureDefaultPostgresAvailable
        runPostgresMigrationsForRuntime defaultMigrationPostgresConfig defaultRealPostgresConfig `shouldReturn` Right ()
        runPostgresSeed defaultMigrationPostgresConfig `shouldReturn` Right ()
        withUnusedTcpEndpoint $ \unusedEndpoint ->
          withSystemTempFile "web-api-runtime-output.txt" $ \_ outputHandle -> do
            completionReference <- newIORef Nothing
            let runtimeAppConfig =
                  defaultAppConfig
                    { listenerConfigs =
                        [ ListenerConfig
                            { listenerHost = tcpEndpointHost unusedEndpoint,
                              listenerPort = tcpEndpointPort unusedEndpoint,
                              listenerScheme = Http,
                              listenerTls = Nothing,
                              listenerAcme = Nothing
                            }
                        ]
                    }
                runtimeEnvironmentConfig =
                  defaultAppEnvironmentConfig
                    { databaseConfig = defaultMigrationPostgresConfig
                    }
            serverThreadId <- forkIO $ do
              result <- try (runWithConfig outputHandle runtimeAppConfig runtimeEnvironmentConfig) :: IO (Either SomeException ())
              writeIORef completionReference (Just result)
            responseText <- waitForRuntimeServerResponse completionReference (tcpEndpointPort unusedEndpoint) "/api/second"
            responseText `shouldBe` "{\"summary\":\"Second page content with stubbed data ready for future loaders.\",\"highlights\":[]}"
            completionResult <- readIORef completionReference
            completionResult `shouldSatisfy` isNothing
            killThread serverThreadId
            waitForRuntimeServerExit completionReference
            hClose outputHandle

    it "announces parsed HTTPS listener configs before surfacing manual TLS startup failures" $
      withUnusedTcpEndpoint $ \unusedEndpoint ->
        withSystemTempFile "web-api-runtime-output.txt" $ \outputPath outputHandle -> do
          let runtimeAppConfig =
                defaultAppConfig
                  { listenerConfigs =
                      [ ListenerConfig
                          { listenerHost = tcpEndpointHost unusedEndpoint,
                            listenerPort = tcpEndpointPort unusedEndpoint,
                            listenerScheme = Https,
                            listenerTls =
                              Just
                                TlsConfig
                                  { certificateSource =
                                      ManualCertificateFiles
                                        { certificateFile = "/tmp/missing-cert.pem",
                                          privateKeyFile = "/tmp/missing-key.pem"
                                        }
                                  },
                            listenerAcme = Nothing
                          }
                      ]
                  }
          result <- try (runWithConfig outputHandle runtimeAppConfig defaultAppEnvironmentConfig) :: IO (Either IOException ())
          hClose outputHandle
          case result of
            Left exception ->
              displayException exception
                `shouldContain` "Manual TLS certificate file does not exist: /tmp/missing-cert.pem"
            Right () ->
              expectationFailure "expected runWithConfig to fail when manual TLS files are missing"
          readFile outputPath
            `shouldReturn` ("Parsed listener config: https://127.0.0.1:" <> show (tcpEndpointPort unusedEndpoint) <> "\n")

    it "writes startup output to the supplied handle for isolated tests and serves real requests" $
      withClearedAppEnvironment $
        withUnusedTcpEndpoint $ \unusedEndpoint ->
          withSystemTempDirectory "web-api-run" $ \tempDirectory ->
            withCurrentDirectory tempDirectory $ do
              writeFile ".env" ("LISTENER_0_PORT=" <> show (tcpEndpointPort unusedEndpoint) <> "\n")
              withSystemTempFile "web-api-output.txt" $ \outputPath outputHandle -> do
                completionReference <- newIORef Nothing
                serverThreadId <- forkIO $ do
                  result <- try (run outputHandle) :: IO (Either SomeException ())
                  writeIORef completionReference (Just result)
                responseText <- waitForRuntimeServerResponse completionReference (tcpEndpointPort unusedEndpoint) "/api/status"
                responseText `shouldBe` "{\"status\":\"ok\",\"locale\":\"en\"}"
                completionResult <- readIORef completionReference
                completionResult `shouldSatisfy` isNothing
                killThread serverThreadId
                waitForRuntimeServerExit completionReference
                hClose outputHandle
                readFile outputPath
                  `shouldReturn` unlines
                    [ "Loaded config file: ./.env",
                      "Config file missing: ./.env.local",
                      "Parsed listener config: http://127.0.0.1:" <> show (tcpEndpointPort unusedEndpoint),
                      "HTTP Server listening at http://127.0.0.1:" <> show (tcpEndpointPort unusedEndpoint)
                    ]

    it "fails explicitly when the layered runtime startup config is invalid" $
      withClearedAppEnvironment $
        withSystemTempDirectory "web-api-run-invalid" $ \tempDirectory ->
          withCurrentDirectory tempDirectory $ do
            writeFile ".env" "LISTENER_0_PORT=0\n"
            result <-
              ( try $
                  withSystemTempFile "web-api-output.txt" $ \_ outputHandle -> do
                    run outputHandle
                    hClose outputHandle
              ) ::
                IO (Either IOException ())
            case result of
              Left exception ->
                displayException exception
                  `shouldContain` "Failed to load app startup config: AppStartupConfigParseError (InvalidConfigValue \"LISTENER_0_PORT\" \"0\")"
              Right () ->
                expectationFailure "expected run to fail on invalid runtime startup config"
