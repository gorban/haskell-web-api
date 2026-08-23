{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWeb.TestSupport where

import Control.Concurrent (MVar, forkIO, killThread, newEmptyMVar, putMVar, threadDelay)
import Control.Exception (Exception (displayException), SomeException, finally, try)
import Control.Monad ()
import Data.ByteString qualified as ByteString (ByteString, drop, empty, null)
import Data.ByteString.Builder qualified as Builder ()
import Data.ByteString.Char8 qualified as ByteStringChar8 (breakSubstring, pack)
import Data.ByteString.Lazy qualified as LazyByteString (ByteString)
import Data.Char ()
import Data.Either ()
import Data.Functor.Compose ()
import Data.IORef (IORef, readIORef)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text (breakOn, drop, dropWhileEnd, empty, isPrefixOf, length, null, pack, splitOn, strip, stripPrefix, takeWhile, unpack)
import Data.Text.Encoding qualified as TextEncoding (decodeUtf8)
import HarchWeb
import HarchWeb.Action qualified as Action (ActionCodec, action, actionCodec, formField, getAt, postAt, required, textValue)
import HarchWeb.Database qualified as Database ()
import HarchWeb.Markup.Unsafe qualified as MarkupUnsafe (unsafeTrustHtml)
import HarchWeb.Observability qualified as Observability (ConnectionObservability (observabilityConnectionSpan), HttpServerMetrics (httpServerMetricAttributes), ObservabilityAttribute (attributeName, attributeValue), ObservabilityAttributeValue (IntAttribute, TextAttribute), RequestObservability (observabilityHttpServerMetrics, observabilityRequestSpan), RequestSpan (requestSpanAttributes), buildConnectionObservability)
import HarchWeb.Security qualified as Security ()
import Network.HTTP.Client qualified as HttpClient (Manager, defaultManagerSettings, newManager)
import Network.HTTP.Types qualified as Http (Header, RequestHeaders, Status, status200, status202, status501)
import Network.Socket qualified as Socket (Family (AF_INET), SockAddr (SockAddrInet), Socket, SocketType (Stream), bind, close, connect, defaultProtocol, getSocketName, listen, maxListenQueue, socket, tupleToHostAddress, withSocketsDo)
import Network.Socket.ByteString qualified as SocketByteString (recv, sendAll)
import Network.Wai qualified as Wai (Request (rawPathInfo, remoteHost, requestHeaders, requestMethod), responseLBS, strictRequestBody)
import Network.Wai.Handler.Warp qualified as Warp (run)
import System.Directory ()
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO ()
import System.IO.Error ()
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Signals ()
import System.Process (callProcess, readProcessWithExitCode)
import Test.Hspec (Expectation, expectationFailure, shouldBe, shouldSatisfy)
import TestCore.CustomAssertions (expectAll)
import TestCore.Wai (waiRequest)
import Text.Read (readMaybe)

data TestContext = TestContext
  { requestLanguage :: Text,
    testContextPathPrefix :: Text
  }
  deriving (Eq, Show)

data CapturedCollectorRequest = CapturedCollectorRequest
  { capturedCollectorMethod :: ByteString.ByteString,
    capturedCollectorPath :: ByteString.ByteString,
    capturedCollectorHeaders :: [Http.Header],
    capturedCollectorBody :: LazyByteString.ByteString
  }

data TestRoute
  = KnownRoute
  | QueryRoute Text
  | DataRoute
  | EventStreamRoute
  | MissingRoute
  deriving (Eq, Show)

trustedMarkup :: Text -> Html
trustedMarkup = trustedHtml . MarkupUnsafe.unsafeTrustHtml

renderDocument :: Document route -> Text
renderDocument = renderDocumentForTests

testRegionPatch :: Text -> Text -> RegionPatch
testRegionPatch identifier message =
  replaceRegion
    ( region
        (mkRegionId (fromMaybe (error "test region id must be non-empty") (mkElementId identifier)))
        paragraphTag
        []
        [text message]
    )

defaultContext :: TestContext
defaultContext = TestContext {requestLanguage = "en", testContextPathPrefix = ""}

spanishContext :: TestContext
spanishContext = TestContext {requestLanguage = "es", testContextPathPrefix = ""}

testActionCodec :: Action.ActionCodec Text TestContext Text
testActionCodec =
  case Action.actionCodec
    [ Action.action
        "save"
        (Action.postAt "/known" renderKnownActionPath)
        (("save:" <>) <$> Action.required (Action.formField "email" Action.textValue)),
      Action.action "read" (Action.getAt "/known" renderKnownActionPath) (pure "read")
    ] of
    Left codecError -> error (show codecError)
    Right codec -> codec

renderKnownActionPath :: TestContext -> Text
renderKnownActionPath requestContext = applyTestPathPrefix (testContextPathPrefix requestContext) "/known"

sampleCodec :: RouteCodec TestRoute TestContext
sampleCodec =
  RouteCodec
    { parseRoute = parseSampleRoute,
      renderRoute = renderSampleRoute,
      notFoundRequest = \routeContext -> routeContext `seq` RouteRequest {requestRoute = MissingRoute, requestContext = routeContext},
      routeMethods = routeMethodPolicy . sampleRouteMethods
    }

sampleRouteMethods :: TestRoute -> [RouteMethod]
sampleRouteMethods route =
  case route of
    MissingRoute -> []
    KnownRoute -> [RouteGet]
    QueryRoute _ -> [RouteGet]
    DataRoute -> [RouteGet, RoutePost]
    EventStreamRoute -> [RouteGet]

parseSampleRoute :: TestContext -> Text -> Maybe (RouteRequest TestRoute TestContext)
parseSampleRoute routeContext path
  | path == "/known" =
      Just RouteRequest {requestRoute = KnownRoute, requestContext = routeContext}
  | path == "/es/known" =
      Just RouteRequest {requestRoute = KnownRoute, requestContext = spanishContext}
  | Just queryString <- Text.stripPrefix "/query?" path =
      Just RouteRequest {requestRoute = QueryRoute queryString, requestContext = routeContext}
  | path == "/data" =
      Just RouteRequest {requestRoute = DataRoute, requestContext = routeContext}
  | path == "/events" =
      Just RouteRequest {requestRoute = EventStreamRoute, requestContext = routeContext}
  | otherwise = Nothing

renderSampleRoute :: RouteRequest TestRoute TestContext -> Text
renderSampleRoute request =
  applyTestPathPrefix
    (testContextPathPrefix (requestContext request))
    ( case (requestLanguage (requestContext request), requestRoute request) of
        (language, KnownRoute)
          | language == "es" -> "/es/known"
          | otherwise -> "/known"
        (_, QueryRoute queryString) -> "/query?" <> queryString
        (_, DataRoute) -> "/data"
        (_, EventStreamRoute) -> "/events"
        (_, MissingRoute) -> "/404"
    )

applyTestPathPrefix :: Text -> Text -> Text
applyTestPathPrefix pathPrefix path
  | Text.null pathPrefix = path
  | path == "/" = pathPrefix
  | otherwise = pathPrefix <> path

sampleRequestContextFromRequest :: ForwardedHeaderTrust -> Wai.Request -> TestContext -> TestContext
sampleRequestContextFromRequest forwardedHeaderTrust request requestContext =
  requestContext
    { testContextPathPrefix =
        if isTrustedForwardingPeer forwardedHeaderTrust (Wai.remoteHost request)
          then
            maybe
              ""
              normalizeTestPathPrefix
              ( lookup "X-Forwarded-Prefix" (Wai.requestHeaders request)
                  >>= firstTestHeaderValue
              )
          else ""
    }

normalizeTestPathPrefix :: Text -> Text
normalizeTestPathPrefix pathPrefix =
  let trimmedPrefix = Text.strip pathPrefix
      slashPrefixedPrefix =
        case (Text.null trimmedPrefix || trimmedPrefix == "/", Text.isPrefixOf "/" trimmedPrefix) of
          (True, _) -> ""
          (False, True) -> trimmedPrefix
          (False, False) -> "/" <> trimmedPrefix
      normalizedPrefix =
        Text.dropWhileEnd (== '/') slashPrefixedPrefix
   in normalizedPrefix

firstTestHeaderValue :: ByteString.ByteString -> Maybe Text
firstTestHeaderValue headerValue =
  case filter (not . Text.null) (map Text.strip (Text.splitOn "," (TextEncoding.decodeUtf8 headerValue))) of
    [] -> Nothing
    firstValue : _ -> Just firstValue

samplePage :: RouteRequest TestRoute TestContext -> Page TestRoute TestContext
samplePage request =
  Page
    { pageTitle = "Known",
      pageRoute = requestRoute request,
      pageContext = requestContext request,
      pageBody = trustedMarkup "<h1>Known</h1>",
      pageBootstrapHooks = []
    }

sampleMissingPage :: RouteRequest TestRoute TestContext -> Page TestRoute TestContext
sampleMissingPage request =
  Page
    { pageTitle = "Missing",
      pageRoute = requestRoute request,
      pageContext = requestContext request,
      pageBody = trustedMarkup "<h1>Missing</h1>",
      pageBootstrapHooks = []
    }

sampleShell :: PageShell TestRoute TestContext
sampleShell =
  PageShell
    { shellBodyAttributes =
        [ HtmlAttribute
            { attributeName = "data-app",
              attributeValue = "sample"
            }
        ],
      shellNavigationAttributes =
        [ HtmlAttribute
            { attributeName = "data-navigation-region",
              attributeValue = "primary"
            }
        ],
      shellNavigationItems =
        [ NavigationItem
            { navigationLabel = "Known",
              navigationRoute = KnownRoute
            },
          NavigationItem
            { navigationLabel = "Missing",
              navigationRoute = MissingRoute
            }
        ],
      shellMainId = "app-main",
      shellMainAttributes =
        [ HtmlAttribute
            { attributeName = "data-navigation-content",
              attributeValue = "true"
            }
        ],
      shellStylesheets = [],
      shellRuntimeDescriptors = [DeferredModule "navigation" "/assets/navigation.js"]
    }

emptyStaticAssets :: StaticAssetsConfig
emptyStaticAssets =
  StaticAssetsConfig
    { staticAssetRoots = [],
      staticAssetContentTypes = defaultStaticAssetContentTypes,
      staticCacheControlSeconds = Nothing
    }

defaultRequestPolicy :: RequestPolicyConfig
defaultRequestPolicy =
  RequestPolicyConfig
    { redirectHttpToHttps = False,
      httpsRedirectPort = Nothing,
      httpsRedirectAuthority = Just "app.example.com",
      strictTransportSecurity = Nothing,
      forwardedHeaderTrust = NeverTrustForwarded,
      requestHeadLimits = unboundedRequestHeadLimits,
      requestTransportLimits = warpDefaultRequestTransportLimits,
      requestConcurrencyLimit = Nothing,
      corsPolicy = defaultCorsPolicyConfig,
      responseSecurityHeaders = defaultResponseSecurityHeadersConfig
    }

-- | Covers both 'Wai.defaultRequest''s built-in peer (@0.0.0.0@) and the
-- explicit loopback peer ('waiRequestWithRemoteHostAndHeaders') this file's
-- fixtures use, so every existing "trust forwarded headers" test keeps its
-- request unchanged.
testTrustedForwardedProxy :: ForwardedHeaderTrust
testTrustedForwardedProxy =
  case parseCidrBlock "0.0.0.0/1" of
    Just cidrBlock -> TrustForwardedFrom (cidrBlock :| [])
    Nothing -> error "invalid test CIDR block"

sampleApplicationWithStaticAssets :: StaticAssetsConfig -> Application TestRoute Text TestContext
sampleApplicationWithStaticAssets staticAssetsConfig =
  sampleApplicationWithConfig staticAssetsConfig defaultRequestPolicy

sampleApplicationWithConfig ::
  StaticAssetsConfig ->
  RequestPolicyConfig ->
  Application TestRoute Text TestContext
sampleApplicationWithConfig staticAssetsConfig requestPolicyConfig =
  Application
    { appName = "sample",
      defaultRequestContext = defaultContext,
      requestContextFromRequest = sampleRequestContextFromRequest (forwardedHeaderTrust requestPolicyConfig),
      applicationNavigationRuntime = Nothing,
      applicationStaticAssets = staticAssetsConfig,
      applicationRequestPolicy = requestPolicyConfig,
      applicationRequestMiddleware = [],
      routeCodec = sampleCodec,
      renderRequestResponse = \_ -> pure . renderSampleResponse,
      decodeClientAction = DecodedClientAction . clientActionPath,
      handleClientAction = const (pure Nothing),
      pageShell = buildPageShell sampleCodec sampleShell,
      reportRequestObservability = const (pure ()),
      reportConnectionObservability = const (pure ()),
      reportApplicationLog = const (pure ())
    }

sampleApplication :: Application TestRoute Text TestContext
sampleApplication =
  sampleApplicationWithStaticAssets emptyStaticAssets

trustedForwardedApplication :: Application TestRoute Text TestContext
trustedForwardedApplication =
  sampleApplicationWithConfig
    emptyStaticAssets
    defaultRequestPolicy
      { forwardedHeaderTrust = testTrustedForwardedProxy
      }

sampleServerConfig :: ServerConfig
sampleServerConfig =
  ServerConfig
    { listenerConfigs =
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
      requestPolicy = defaultRequestPolicy,
      observability =
        ObservabilityConfig
          { tracingExporter = Nothing,
            metricsExporter = Nothing
          }
    }

serverConfigWithListeners :: [ListenerConfig] -> ServerConfig
serverConfigWithListeners listeners =
  sampleServerConfig
    { listenerConfigs = listeners
    }

httpRuntimeListener :: Text -> Int -> ListenerConfig
httpRuntimeListener host port =
  ListenerConfig
    { listenerHost = host,
      listenerPort = port,
      listenerScheme = Http,
      listenerTls = Nothing,
      listenerAcme = Nothing
    }

acmeHttpsListenerWithDomains :: Text -> Int -> [Text] -> [Text] -> CertbotConfig -> ListenerConfig
acmeHttpsListenerWithDomains =
  acmeHttpsListenerWithDomainsAndChallengePort 80

acmeHttpsListenerWithDomainsAndChallengePort :: Int -> Text -> Int -> [Text] -> [Text] -> CertbotConfig -> ListenerConfig
acmeHttpsListenerWithDomainsAndChallengePort challengePort host port contactEmails domains challengeBackend =
  ListenerConfig
    { listenerHost = host,
      listenerPort = port,
      listenerScheme = Https,
      listenerTls =
        Just
          TlsConfig
            { certificateSource =
                AcmeCertificateSource
                  AcmeConfig
                    { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                      acmeContactEmails = contactEmails,
                      acmeDomains = domains,
                      acmeHttp01Port = challengePort,
                      acmeCertificateDirectory = Nothing,
                      acmeCertbotConfig = challengeBackend
                    }
            },
      listenerAcme = Nothing
    }

sharedHttpsListener :: Text -> Int -> FilePath -> ListenerConfig
sharedHttpsListener host port certificateDirectory =
  sharedHttpsListenerWithStartupMode host port certificateDirectory (AwaitCertificateFiles Nothing)

sharedHttpsListenerWithStartupMode :: Text -> Int -> FilePath -> TlsStartupMode -> ListenerConfig
sharedHttpsListenerWithStartupMode host port certificateDirectory startupMode =
  ListenerConfig
    { listenerHost = host,
      listenerPort = port,
      listenerScheme = Https,
      listenerTls =
        Just
          TlsConfig
            { certificateSource =
                SharedCertificateFiles
                  SharedTlsCertificateFiles
                    { certificateDirectory = certificateDirectory,
                      sharedCertificateStartupMode = startupMode
                    }
            },
      listenerAcme = Nothing
    }

acmeHttpsListenerWithContacts :: Text -> Int -> [Text] -> CertbotConfig -> ListenerConfig
acmeHttpsListenerWithContacts host port contactEmails =
  acmeHttpsListenerWithDomains host port contactEmails []

acmeHttpsListener :: Text -> Int -> CertbotConfig -> ListenerConfig
acmeHttpsListener host port =
  acmeHttpsListenerWithContacts host port ["ops@example.com"]

certbotHttp01Backend :: [Text] -> CertbotConfig
certbotHttp01Backend =
  certbotHttp01BackendWithExecutable "certbot"

certbotHttp01BackendWithExecutable :: FilePath -> [Text] -> CertbotConfig
certbotHttp01BackendWithExecutable executablePath certbotArguments =
  CertbotConfig
    { certbotExecutable = executablePath,
      certbotArguments = certbotArguments
    }

runtimeAcmePlanWithCertbotConfig :: CertbotConfig -> RuntimeAcmeBindPlan
runtimeAcmePlanWithCertbotConfig certbotConfig =
  RuntimeAcmeBindPlan
    { runtimeAcmeEndpoint = ListenerEndpoint {endpointHost = "127.0.0.1", endpointPort = 5443},
      runtimeAcmeTlsEndpoint = Just ListenerEndpoint {endpointHost = "127.0.0.1", endpointPort = 5443},
      runtimeAcmeListenerConfig =
        AcmeConfig
          { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
            acmeContactEmails = ["ops@example.com"],
            acmeDomains = ["example.com", "www.example.com"],
            acmeHttp01Port = 80,
            acmeCertificateDirectory = Just ".tls/example.com",
            acmeCertbotConfig = certbotConfig
          }
    }

withCustomFakeCertbotExecutable :: [String] -> (FilePath -> IO a) -> IO a
withCustomFakeCertbotExecutable scriptLines action =
  withSystemTempDirectory "fake-certbot" $ \tempDirectory -> do
    let scriptPath = tempDirectory </> "certbot"
    writeFile scriptPath (unlines scriptLines)
    callProcess "chmod" ["+x", scriptPath]
    action scriptPath

withFakeCertbotExecutable :: FilePath -> FilePath -> (FilePath -> IO a) -> IO a
withFakeCertbotExecutable certificatePath privateKeyPath =
  withCustomFakeCertbotExecutable
    ( fakeCertbotScriptPreamble
        <> [ "mkdir -p \"$config_dir/live/$cert_name\"",
             "cp " <> show certificatePath <> " \"$config_dir/live/$cert_name/fullchain.pem\"",
             "cp " <> show privateKeyPath <> " \"$config_dir/live/$cert_name/privkey.pem\""
           ]
    )

withFailingFakeCertbotExecutable :: (FilePath -> IO a) -> IO a
withFailingFakeCertbotExecutable =
  withCustomFakeCertbotExecutable
    [ "#!/bin/sh",
      "logs_dir=''",
      "while [ \"$#\" -gt 0 ]; do",
      "  case \"$1\" in",
      "    --logs-dir) logs_dir=\"$2\"; shift 2 ;;",
      "    *) shift ;;",
      "  esac",
      "done",
      "if [ -n \"$logs_dir\" ]; then",
      "  mkdir -p \"$logs_dir\"",
      "  printf '%s\\n' 'fake letsencrypt detail' > \"$logs_dir/letsencrypt.log\"",
      "fi",
      "echo fake certbot failure >&2",
      "exit 42"
    ]

fakeCertbotScriptPreamble :: [String]
fakeCertbotScriptPreamble =
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
    "if [ -z \"$cert_name\" ]; then",
    "  cert_name=\"${domain%%,*}\"",
    "fi"
  ]

rootPathApplication :: Application TestRoute Text TestContext
rootPathApplication =
  Application
    { appName = "root-path",
      defaultRequestContext = defaultContext,
      requestContextFromRequest = sampleRequestContextFromRequest testTrustedForwardedProxy,
      applicationNavigationRuntime = Nothing,
      applicationStaticAssets = emptyStaticAssets,
      applicationRequestPolicy = defaultRequestPolicy {forwardedHeaderTrust = testTrustedForwardedProxy},
      applicationRequestMiddleware = [],
      routeCodec = rootPathCodec,
      renderRequestResponse = \_ -> pure . PageResponse . samplePage,
      decodeClientAction = DecodedClientAction . clientActionPath,
      handleClientAction = const (pure Nothing),
      pageShell = buildPageShell rootPathCodec sampleShell,
      reportRequestObservability = const (pure ()),
      reportConnectionObservability = const (pure ()),
      reportApplicationLog = const (pure ())
    }

rootPathCodec :: RouteCodec TestRoute TestContext
rootPathCodec =
  RouteCodec
    { parseRoute = \routeContext path ->
        if path == "/"
          then Just RouteRequest {requestRoute = KnownRoute, requestContext = routeContext}
          else Nothing,
      renderRoute = \request ->
        case requestRoute request of
          KnownRoute -> applyTestPathPrefix (testContextPathPrefix (requestContext request)) "/"
          QueryRoute queryString -> applyTestPathPrefix (testContextPathPrefix (requestContext request)) ("/query?" <> queryString)
          DataRoute -> applyTestPathPrefix (testContextPathPrefix (requestContext request)) "/data"
          EventStreamRoute -> applyTestPathPrefix (testContextPathPrefix (requestContext request)) "/events"
          MissingRoute -> applyTestPathPrefix (testContextPathPrefix (requestContext request)) "/404",
      notFoundRequest = \routeContext -> routeContext `seq` RouteRequest {requestRoute = MissingRoute, requestContext = routeContext},
      routeMethods = routeMethodPolicy . sampleRouteMethods
    }

renderSampleResponse :: RouteRequest TestRoute TestContext -> Response TestRoute TestContext
renderSampleResponse request =
  case requestRoute request of
    KnownRoute -> PageResponse (samplePage request)
    QueryRoute queryString ->
      BodyResponse
        ResponseBody
          { responseStatus = Http.status200,
            responseContentType = "text/plain; charset=utf-8",
            responseBody = queryString,
            responseObservabilityAttributes = [],
            responseLogEntries = [],
            responseDatabaseOperations = []
          }
    DataRoute ->
      BodyResponse
        ResponseBody
          { responseStatus = Http.status202,
            responseContentType = "application/json",
            responseBody = "{\"route\":\"data\"}",
            responseObservabilityAttributes = [],
            responseLogEntries = [],
            responseDatabaseOperations = []
          }
    EventStreamRoute ->
      BodyResponse
        ResponseBody
          { responseStatus = Http.status501,
            responseContentType = "text/plain; charset=utf-8",
            responseBody = "event stream not configured",
            responseObservabilityAttributes = [],
            responseLogEntries = [],
            responseDatabaseOperations = []
          }
    MissingRoute -> PageResponse (sampleMissingPage request)

waiRequestWithRemoteHostAndHeaders ::
  [Text] ->
  Socket.SockAddr ->
  Http.RequestHeaders ->
  Wai.Request
waiRequestWithRemoteHostAndHeaders segments remoteHost headers =
  (waiRequest segments)
    { Wai.remoteHost = remoteHost,
      Wai.requestHeaders = headers
    }

readLocalTestServerResponse :: LocalTestServer -> Text -> IO Text
readLocalTestServerResponse localTestServer path = do
  responseBytes <- readLocalTestServerResponseBytes localTestServer path
  pure (TextEncoding.decodeUtf8 responseBytes)

readLocalTestServerResponseBytes :: LocalTestServer -> Text -> IO ByteString.ByteString
readLocalTestServerResponseBytes localTestServer =
  readLoopbackHttpResponseBytes (localServerPort localTestServer)

readLoopbackHttpResponse :: Int -> Text -> IO Text
readLoopbackHttpResponse port path = do
  responseBytes <- readLoopbackHttpResponseBytes port path
  pure (TextEncoding.decodeUtf8 responseBytes)

readLoopbackHttpsResponse :: Int -> Text -> IO Text
readLoopbackHttpsResponse port path = do
  responseResult <- readLoopbackHttpsResponseResult port path
  case responseResult of
    Right responseText -> pure responseText
    Left responseError ->
      expectationFailure ("expected curl HTTPS request to succeed: " <> responseError)
        >> pure Text.empty

readLoopbackHttpResponseBytes :: Int -> Text -> IO ByteString.ByteString
readLoopbackHttpResponseBytes port =
  readLoopbackHttpResponseBytesWithHost port "127.0.0.1"

readLoopbackHttpResponseBytesWithHost :: Int -> Text -> Text -> IO ByteString.ByteString
readLoopbackHttpResponseBytesWithHost port hostHeader path = do
  responseResult <- readLoopbackHttpResponseBytesWithHostResult port hostHeader path
  case responseResult of
    Right responseBytes -> pure responseBytes
    Left responseError ->
      ioError (userError responseError)

readLoopbackHttpResponseBytesWithHostResult :: Int -> Text -> Text -> IO (Either String ByteString.ByteString)
readLoopbackHttpResponseBytesWithHostResult port hostHeader path =
  Socket.withSocketsDo $ do
    clientSocket <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
    responseResult <- try $ do
      Socket.connect clientSocket (Socket.SockAddrInet (fromIntegral port) (Socket.tupleToHostAddress (127, 0, 0, 1)))
      SocketByteString.sendAll clientSocket (buildHttpRequestWithHost hostHeader path)
      responseBytes <- readAllSocketChunks clientSocket
      pure (extractHttpBody responseBytes)
    Socket.close clientSocket
    pure $
      either
        (Left . displayException)
        Right
        (responseResult :: Either IOError ByteString.ByteString)

-- | Poll until an 'IORef' reaches an expected value, for synchronizing with
-- a concurrently forked request handler without a fixed 'threadDelay' guess.
waitUntilIORefEquals :: (Eq a) => IORef a -> a -> IO ()
waitUntilIORefEquals valueRef expectedValue = do
  currentValue <- readIORef valueRef
  if currentValue == expectedValue
    then pure ()
    else threadDelay 1000 >> waitUntilIORefEquals valueRef expectedValue

readRawLoopbackHttpResponse :: Int -> ByteString.ByteString -> IO ByteString.ByteString
readRawLoopbackHttpResponse port requestBytes =
  Socket.withSocketsDo $ do
    clientSocket <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
    responseResult <- try $ do
      Socket.connect clientSocket (Socket.SockAddrInet (fromIntegral port) (Socket.tupleToHostAddress (127, 0, 0, 1)))
      SocketByteString.sendAll clientSocket requestBytes
      readAllSocketChunks clientSocket
    Socket.close clientSocket
    either (ioError . userError . displayException) pure (responseResult :: Either IOError ByteString.ByteString)

readLoopbackHttpResponseBytesWithHostAndHeadersResult :: Int -> Text -> Text -> [(Text, Text)] -> IO (Either String ByteString.ByteString)
readLoopbackHttpResponseBytesWithHostAndHeadersResult port hostHeader path headers =
  Socket.withSocketsDo $ do
    clientSocket <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
    responseResult <- try $ do
      Socket.connect clientSocket (Socket.SockAddrInet (fromIntegral port) (Socket.tupleToHostAddress (127, 0, 0, 1)))
      SocketByteString.sendAll clientSocket (buildHttpRequestWithHostAndHeaders hostHeader path headers)
      responseBytes <- readAllSocketChunks clientSocket
      pure (extractHttpBody responseBytes)
    Socket.close clientSocket
    pure $
      either
        (Left . displayException)
        Right
        (responseResult :: Either IOError ByteString.ByteString)

readLoopbackHttpsResponseResult :: Int -> Text -> IO (Either String Text)
readLoopbackHttpsResponseResult port path = do
  let url = "https://127.0.0.1:" <> show port <> Text.unpack path
  (exitCode, stdoutText, stderrText) <-
    readProcessWithExitCode
      "curl"
      ["--silent", "--show-error", "--insecure", "--fail", "--noproxy", "*", url]
      ""
  pure $
    case exitCode of
      ExitSuccess -> Right (Text.pack stdoutText)
      ExitFailure _ -> Left stderrText

connectAndCloseLoopbackSocket :: Int -> IO ()
connectAndCloseLoopbackSocket port =
  Socket.withSocketsDo $ do
    clientSocket <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
    Socket.connect clientSocket (Socket.SockAddrInet (fromIntegral port) (Socket.tupleToHostAddress (127, 0, 0, 1)))
    Socket.close clientSocket

hasTextAttribute :: Text -> Text -> [Observability.ObservabilityAttribute] -> Bool
hasTextAttribute attributeName expectedValue =
  any
    ( \attribute ->
        Observability.attributeName attribute == attributeName
          && Observability.attributeValue attribute == Observability.TextAttribute expectedValue
    )

lookupIntAttribute :: Text -> [Observability.ObservabilityAttribute] -> Maybe Int
lookupIntAttribute expectedName =
  listToMaybe
    . mapMaybe
      ( \attribute ->
          case (Observability.attributeName attribute, Observability.attributeValue attribute) of
            (currentName, Observability.IntAttribute attributeValue)
              | currentName == expectedName -> Just attributeValue
            _ -> Nothing
      )

expectMeasuredRootRequestTiming :: Observability.RequestObservability -> Expectation
expectMeasuredRootRequestTiming requestObservabilityValue = do
  let spanAttributes =
        Observability.requestSpanAttributes
          (Observability.observabilityRequestSpan requestObservabilityValue)
      metricAttributes =
        Observability.httpServerMetricAttributes
          (Observability.observabilityHttpServerMetrics requestObservabilityValue)
      expectTimingAttribute attributeName = do
        lookupIntAttribute attributeName spanAttributes
          `shouldSatisfy` maybe False (>= 0)
        lookupIntAttribute attributeName metricAttributes
          `shouldBe` Nothing
  expectTimingAttribute "harch.request.start_monotonic_ns"
  expectTimingAttribute "harch.request.duration_ns"

expectMeasuredRequestTiming :: Observability.RequestObservability -> Expectation
expectMeasuredRequestTiming requestObservabilityValue = do
  expectMeasuredRootRequestTiming requestObservabilityValue
  let spanAttributes =
        Observability.requestSpanAttributes
          (Observability.observabilityRequestSpan requestObservabilityValue)
      metricAttributes =
        Observability.httpServerMetricAttributes
          (Observability.observabilityHttpServerMetrics requestObservabilityValue)
      expectTimingAttribute attributeName = do
        lookupIntAttribute attributeName spanAttributes
          `shouldSatisfy` maybe False (>= 0)
        lookupIntAttribute attributeName metricAttributes
          `shouldBe` Nothing
  expectTimingAttribute "harch.phase.request-policy.start_offset_ns"
  expectTimingAttribute "harch.phase.request-policy.duration_ns"
  expectTimingAttribute "harch.phase.route-match.start_offset_ns"
  expectTimingAttribute "harch.phase.route-match.duration_ns"
  expectTimingAttribute "harch.phase.render-response.start_offset_ns"
  expectTimingAttribute "harch.phase.render-response.duration_ns"

waitForConnectionObservability :: IORef [Observability.ConnectionObservability] -> Text -> IO Observability.ConnectionObservability
waitForConnectionObservability connectionObservabilityReference expectedEventName =
  waitForObservabilityAttempts (500 :: Int)
  where
    waitForObservabilityAttempts remainingAttempts = do
      connectionObservabilityValues <- readIORef connectionObservabilityReference
      case find matchesExpectedEvent connectionObservabilityValues of
        Just connectionObservabilityValue -> pure connectionObservabilityValue
        Nothing
          | remainingAttempts > 0 -> do
              threadDelay 10000
              waitForObservabilityAttempts (remainingAttempts - 1)
          | otherwise ->
              expectationFailure ("expected connection observability for " <> Text.unpack expectedEventName)
                >> pure
                  ( Observability.buildConnectionObservability
                      "missing"
                      []
                  )

    matchesExpectedEvent connectionObservabilityValue =
      hasTextAttribute
        "harch.connection.event"
        expectedEventName
        (Observability.requestSpanAttributes (Observability.observabilityConnectionSpan connectionObservabilityValue))

waitForServerResponse :: IORef (Maybe (Either SomeException ())) -> Int -> Text -> IO Text
waitForServerResponse completionReference port path =
  waitForResponseAttempts (500 :: Int)
  where
    waitForResponseAttempts remainingAttempts = do
      completionResult <- readIORef completionReference
      case completionResult of
        Just (Left exception) ->
          expectationFailure ("expected runServer to remain running, but it failed early: " <> displayException exception)
            >> pure Text.empty
        Just (Right ()) ->
          expectationFailure "expected runServer to remain running, but it exited early"
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
                  expectationFailure "expected runServer to accept loopback HTTP requests"
                    >> pure Text.empty

waitForHttpsServerResponse :: IORef (Maybe (Either SomeException ())) -> Int -> Text -> IO Text
waitForHttpsServerResponse completionReference port path =
  waitForResponseAttempts (500 :: Int)
  where
    waitForResponseAttempts remainingAttempts = do
      completionResult <- readIORef completionReference
      case completionResult of
        Just (Left exception) ->
          expectationFailure ("expected runServer to remain running, but it failed early: " <> displayException exception)
            >> pure Text.empty
        Just (Right ()) ->
          expectationFailure "expected runServer to remain running, but it exited early"
            >> pure Text.empty
        Nothing -> do
          responseResult <- readLoopbackHttpsResponseResult port path
          case responseResult of
            Right responseText -> pure responseText
            Left _
              | remainingAttempts > 0 -> do
                  threadDelay 10000
                  waitForResponseAttempts (remainingAttempts - 1)
              | otherwise ->
                  expectationFailure "expected runServer to accept loopback HTTPS requests"
                    >> pure Text.empty

waitForServerExit :: IORef (Maybe (Either SomeException ())) -> IO ()
waitForServerExit completionReference =
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
              expectationFailure "expected runServer to stop after being signalled"

withUnusedLoopbackPort :: (Int -> IO a) -> IO a
withUnusedLoopbackPort action = do
  reservedSocket <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
  Socket.bind reservedSocket (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1)))
  socketAddress <- Socket.getSocketName reservedSocket
  case socketAddress of
    Socket.SockAddrInet port _ -> do
      Socket.close reservedSocket
      action (fromIntegral port)
    _ -> do
      Socket.close reservedSocket
      error "expected IPv4 loopback reservation socket"

withOccupiedLoopbackPort :: (Int -> IO a) -> IO a
withOccupiedLoopbackPort action = do
  listeningSocket <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
  Socket.bind listeningSocket (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1)))
  Socket.listen listeningSocket Socket.maxListenQueue
  socketAddress <- Socket.getSocketName listeningSocket
  case socketAddress of
    Socket.SockAddrInet port _ ->
      action (fromIntegral port)
        `finally` Socket.close listeningSocket
    _ ->
      Socket.close listeningSocket
        >> error "expected IPv4 loopback listening socket"

withManualTlsFiles :: (FilePath -> FilePath -> IO a) -> IO a
withManualTlsFiles action =
  withSystemTempDirectory "harch-web-tls" $ \tempDirectory -> do
    let certificatePath = tempDirectory </> "cert.pem"
        privateKeyPath = tempDirectory </> "key.pem"
    writeFile certificatePath manualTlsCertificatePem
    writeFile privateKeyPath manualTlsPrivateKeyPem
    action certificatePath privateKeyPath

withEmptyExecutablePath :: IO a -> IO a
withEmptyExecutablePath action =
  withSystemTempDirectory "missing-executable-path" $ \tempDirectory -> do
    originalPath <- lookupEnv "PATH"
    setEnv "PATH" tempDirectory
    action `finally` maybe (unsetEnv "PATH") (setEnv "PATH") originalPath

stripVolatileRequestTiming :: Observability.RequestObservability -> Observability.RequestObservability
stripVolatileRequestTiming requestObservability =
  requestObservability
    { Observability.observabilityRequestSpan =
        stripVolatileRequestSpanTiming (Observability.observabilityRequestSpan requestObservability),
      Observability.observabilityHttpServerMetrics =
        (Observability.observabilityHttpServerMetrics requestObservability)
          { Observability.httpServerMetricAttributes =
              filter
                (not . isVolatileRequestTimingAttribute)
                (Observability.httpServerMetricAttributes (Observability.observabilityHttpServerMetrics requestObservability))
          }
    }

stripVolatileRequestSpanTiming :: Observability.RequestSpan -> Observability.RequestSpan
stripVolatileRequestSpanTiming requestSpan =
  requestSpan
    { Observability.requestSpanAttributes =
        filter
          (not . isVolatileRequestTimingAttribute)
          (Observability.requestSpanAttributes requestSpan)
    }

isVolatileRequestTimingAttribute :: Observability.ObservabilityAttribute -> Bool
isVolatileRequestTimingAttribute attribute =
  Observability.attributeName attribute
    `elem` [ "harch.request.start_monotonic_ns",
             "harch.request.duration_ns",
             "harch.phase.request-policy.start_offset_ns",
             "harch.phase.request-policy.duration_ns",
             "harch.phase.route-match.start_offset_ns",
             "harch.phase.route-match.duration_ns",
             "harch.phase.render-response.start_offset_ns",
             "harch.phase.render-response.duration_ns"
           ]

withOtlpCollector ::
  Http.Status ->
  LazyByteString.ByteString ->
  (HttpClient.Manager -> Text -> MVar CapturedCollectorRequest -> IO a) ->
  IO a
withOtlpCollector responseStatus responseBody action =
  withUnusedLoopbackPort $ \collectorPort -> do
    manager <- HttpClient.newManager HttpClient.defaultManagerSettings
    capturedRequestReference <- newEmptyMVar
    let collectorUrl = Text.pack ("http://127.0.0.1:" <> show collectorPort <> "/v1/traces")
        collectorApplication request respond = do
          requestBody <- Wai.strictRequestBody request
          putMVar
            capturedRequestReference
            CapturedCollectorRequest
              { capturedCollectorMethod = Wai.requestMethod request,
                capturedCollectorPath = Wai.rawPathInfo request,
                capturedCollectorHeaders = Wai.requestHeaders request,
                capturedCollectorBody = requestBody
              }
          respond (Wai.responseLBS responseStatus [("Content-Type", "application/json")] responseBody)
    serverThreadId <- forkIO (Warp.run collectorPort collectorApplication)
    threadDelay 50000
    action manager collectorUrl capturedRequestReference `finally` killThread serverThreadId

extractQuotedJsonField :: Text -> Text -> Maybe Text
extractQuotedJsonField fieldName bodyText =
  listToMaybe (extractQuotedJsonFields fieldName bodyText)

extractQuotedJsonFields :: Text -> Text -> [Text]
extractQuotedJsonFields fieldName bodyText =
  case Text.breakOn fieldPrefix bodyText of
    (_, withField)
      | Text.null withField -> []
      | otherwise ->
          let fieldValueStart = Text.drop (Text.length fieldPrefix) withField
              fieldValue = Text.takeWhile (/= '"') fieldValueStart
              remainingBody = Text.drop (Text.length fieldValue + 1) fieldValueStart
           in fieldValue : extractQuotedJsonFields fieldName remainingBody
  where
    fieldPrefix = "\"" <> fieldName <> "\":\""

extractQuotedJsonIntegerFields :: Text -> Text -> [Integer]
extractQuotedJsonIntegerFields fieldName bodyText =
  mapMaybe (readMaybe . Text.unpack) (extractQuotedJsonFields fieldName bodyText)

expectPlausibleEpochNanoTimestamps :: Text -> Expectation
expectPlausibleEpochNanoTimestamps bodyText = do
  let earliestPlausibleEpochNano = 1577836800000000000
      latestPlausibleEpochNano = 4102444800000000000
      startTimes = extractQuotedJsonIntegerFields "startTimeUnixNano" bodyText
      endTimes = extractQuotedJsonIntegerFields "endTimeUnixNano" bodyText
  startTimes `shouldSatisfy` (not . null)
  length startTimes `shouldBe` length endTimes
  mapM_
    ( \(startTimeUnixNano, endTimeUnixNano) ->
        expectAll
          ( (startTimeUnixNano `shouldSatisfy` (>= earliestPlausibleEpochNano))
              :| [ endTimeUnixNano `shouldSatisfy` (< latestPlausibleEpochNano),
                   startTimeUnixNano `shouldSatisfy` (< endTimeUnixNano),
                   (endTimeUnixNano - startTimeUnixNano) `shouldSatisfy` (>= 1000)
                 ]
          )
    )
    (zip startTimes endTimes)

expectLoopbackPortReusable :: Int -> IO ()
expectLoopbackPortReusable port = do
  bindResult <- try bindTemporaryListener :: IO (Either IOError ())
  case bindResult of
    Right () -> pure ()
    Left bindError ->
      expectationFailure ("expected loopback port " <> show port <> " to be reusable, but bind failed: " <> displayException bindError)
  where
    bindTemporaryListener = do
      temporarySocket <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
      Socket.bind temporarySocket (Socket.SockAddrInet (fromIntegral port) (Socket.tupleToHostAddress (127, 0, 0, 1)))
      Socket.listen temporarySocket Socket.maxListenQueue
      Socket.close temporarySocket

buildHttpRequestWithHost :: Text -> Text -> ByteString.ByteString
buildHttpRequestWithHost hostHeader path =
  buildHttpRequestWithHostAndHeaders hostHeader path []

buildHttpRequestWithHostAndHeaders :: Text -> Text -> [(Text, Text)] -> ByteString.ByteString
buildHttpRequestWithHostAndHeaders hostHeader path headers =
  ByteStringChar8.pack $
    "GET "
      <> Text.unpack path
      <> " HTTP/1.1\r\nHost: "
      <> Text.unpack hostHeader
      <> concatMap
        (\(headerName, headerValue) -> "\r\n" <> Text.unpack headerName <> ": " <> Text.unpack headerValue)
        headers
      <> "\r\nConnection: close\r\n\r\n"

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

readAllSocketChunks :: Socket.Socket -> IO ByteString.ByteString
readAllSocketChunks clientSocket = do
  chunk <- SocketByteString.recv clientSocket 4096
  if ByteString.null chunk
    then pure ByteString.empty
    else fmap (chunk <>) (readAllSocketChunks clientSocket)

extractHttpBody :: ByteString.ByteString -> ByteString.ByteString
extractHttpBody responseBytes =
  let (_, withSeparator) = ByteStringChar8.breakSubstring "\r\n\r\n" responseBytes
   in ByteString.drop 4 withSeparator
