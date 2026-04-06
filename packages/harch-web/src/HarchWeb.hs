{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module HarchWeb
  ( AcmeChallengeBackend (..),
    AcmeConfig (..),
    Application (..),
    CertbotConfig (..),
    Document (..),
    HasServerConfig (..),
    HtmlAttribute (..),
    HttpBindPlan (..),
    ListenerConfig (..),
    ListenerEndpoint (..),
    ListenerScheme (..),
    ListenerStartupError (..),
    LocalTestServer (..),
    ManualTlsBindPlan (..),
    NavigationItem (..),
    ObservabilityConfig (..),
    ObservabilityStartupPlan (..),
    OtlpExporter (..),
    OtlpExporterStartup (..),
    Page (..),
    PageShell (..),
    RequestPolicyConfig (..),
    Response (..),
    ResponseBody (..),
    ResolvedNavigationItem (..),
    RouteCodec (..),
    RouteRequest (..),
    ServerConfig (..),
    ServerStartupPlan (..),
    StaticAssetRoot (..),
    StaticAssetsConfig (..),
    StrictTransportSecurityConfig (..),
    TelemetrySignal (..),
    AcmeBindPlan (..),
    TlsCertificateSource (..),
    TlsConfig (..),
    application,
    buildDocument,
    buildNavigation,
    buildPageShell,
    matchRoute,
    planObservabilityStartup,
    planServerStartup,
    routeHref,
    renderDocument,
    runServer,
    staticAssetHref,
    staticAssetHrefWithPrefix,
    toWaiApplication,
    withLocalTestServer,
  )
where

import Control.Concurrent (MVar, ThreadId, forkFinally, killThread, newEmptyMVar, putMVar, takeMVar, tryPutMVar)
import Control.Exception (SomeException, bracket, evaluate, onException, throwIO)
import Control.Monad (unless)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteStringChar8
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Either (lefts)
import Data.List (maximumBy)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Observability qualified as Observability
import Network.HTTP.Types qualified as Http
import Network.Socket qualified as Socket
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WarpTLS qualified as WarpTLS
import System.Directory (doesFileExist)
import System.FilePath (splitDirectories, takeExtension, (</>))
import System.IO (Handle, hFlush, hPutStrLn)

data ListenerScheme
  = Http
  | Https
  deriving (Eq, Show)

data CertbotConfig = CertbotConfig
  { certbotExecutable :: FilePath,
    certbotArguments :: [Text]
  }
  deriving (Eq, Show)

data AcmeChallengeBackend
  = InProcessHttp01
  | CertbotHttp01 CertbotConfig
  deriving (Eq, Show)

data AcmeConfig = AcmeConfig
  { acmeDirectoryUrl :: Text,
    acmeContactEmails :: [Text],
    acmeChallengeBackend :: AcmeChallengeBackend
  }
  deriving (Eq, Show)

data TlsCertificateSource
  = ManualCertificateFiles
      { certificateFile :: FilePath,
        privateKeyFile :: FilePath
      }
  | AcmeCertificateSource AcmeConfig
  deriving (Eq, Show)

newtype TlsConfig = TlsConfig
  { certificateSource :: TlsCertificateSource
  }
  deriving (Eq, Show)

data ListenerConfig = ListenerConfig
  { listenerHost :: Text,
    listenerPort :: Int,
    listenerScheme :: ListenerScheme,
    listenerTls :: Maybe TlsConfig
  }
  deriving (Eq, Show)

data StaticAssetRoot = StaticAssetRoot
  { staticUrlPrefix :: Text,
    staticDirectory :: FilePath
  }
  deriving (Eq, Show)

data StaticAssetsConfig = StaticAssetsConfig
  { staticAssetRoots :: [StaticAssetRoot],
    staticCacheControlSeconds :: Maybe Int
  }
  deriving (Eq, Show)

data OtlpExporter = OtlpExporter
  { otlpEndpoint :: Text,
    otlpHeaders :: [(Text, Text)]
  }
  deriving (Eq, Show)

data ObservabilityConfig = ObservabilityConfig
  { tracingExporter :: Maybe OtlpExporter,
    metricsExporter :: Maybe OtlpExporter
  }
  deriving (Eq, Show)

data StrictTransportSecurityConfig = StrictTransportSecurityConfig
  { strictTransportSecurityMaxAgeSeconds :: Int,
    strictTransportSecurityIncludeSubDomains :: Bool,
    strictTransportSecurityPreload :: Bool
  }
  deriving (Eq, Show)

data RequestPolicyConfig = RequestPolicyConfig
  { redirectHttpToHttps :: Bool,
    strictTransportSecurity :: Maybe StrictTransportSecurityConfig
  }
  deriving (Eq, Show)

data TelemetrySignal
  = TracingSignal
  | MetricsSignal
  deriving (Eq, Show)

data OtlpExporterStartup = OtlpExporterStartup
  { startupSignal :: TelemetrySignal,
    startupEndpoint :: Text,
    startupHeaders :: [(Text, Text)]
  }
  deriving (Eq, Show)

newtype ObservabilityStartupPlan = ObservabilityStartupPlan
  { startupExporters :: [OtlpExporterStartup]
  }
  deriving (Eq, Show)

data ServerConfig = ServerConfig
  { listenerConfigs :: [ListenerConfig],
    staticAssets :: StaticAssetsConfig,
    requestPolicy :: RequestPolicyConfig,
    observability :: ObservabilityConfig
  }
  deriving (Eq, Show)

class HasServerConfig config where
  toServerConfig :: config -> ServerConfig

instance HasServerConfig ServerConfig where
  toServerConfig = id

data ListenerEndpoint = ListenerEndpoint
  { endpointHost :: Text,
    endpointPort :: Int
  }
  deriving (Eq, Show)

newtype HttpBindPlan = HttpBindPlan
  { httpEndpoints :: [ListenerEndpoint]
  }
  deriving (Eq, Show)

data ManualTlsBindPlan = ManualTlsBindPlan
  { tlsEndpoint :: ListenerEndpoint,
    tlsCertificateFile :: FilePath,
    tlsPrivateKeyFile :: FilePath
  }
  deriving (Eq, Show)

data AcmeBindPlan = AcmeBindPlan
  { acmeEndpoint :: ListenerEndpoint,
    acmeListenerConfig :: AcmeConfig
  }
  deriving (Eq, Show)

data ServerStartupPlan = ServerStartupPlan
  { httpBindPlan :: HttpBindPlan,
    manualTlsBindPlans :: [ManualTlsBindPlan],
    acmeBindPlans :: [AcmeBindPlan]
  }
  deriving (Eq, Show)

data ListenerStartupError
  = DuplicateListenerEndpoint ListenerEndpoint
  | InvalidListenerTlsConfiguration ListenerConfig
  deriving (Eq, Show)

data RouteRequest route context = RouteRequest
  { requestRoute :: route,
    requestContext :: context
  }
  deriving (Eq, Show)

data Page route context = Page
  { pageTitle :: Text,
    pageRoute :: route,
    pageContext :: context,
    pageBody :: Text,
    pageBootstrapHooks :: [Text]
  }
  deriving (Eq, Show)

data HtmlAttribute = HtmlAttribute
  { attributeName :: Text,
    attributeValue :: Text
  }
  deriving (Eq, Show)

data NavigationItem route = NavigationItem
  { navigationLabel :: Text,
    navigationRoute :: route
  }
  deriving (Eq, Show)

data ResolvedNavigationItem route = ResolvedNavigationItem
  { navigationLabel :: Text,
    navigationRoute :: route,
    navigationHref :: Text,
    navigationIsActive :: Bool
  }
  deriving (Eq, Show)

data Document route = Document
  { documentTitle :: Text,
    documentBodyAttributes :: [HtmlAttribute],
    documentNavigationAttributes :: [HtmlAttribute],
    documentNavigation :: [ResolvedNavigationItem route],
    documentMainId :: Text,
    documentMainAttributes :: [HtmlAttribute],
    documentMainContent :: Text,
    documentBootstrapHooks :: [Text],
    documentScriptSources :: [Text]
  }
  deriving (Eq, Show)

data PageShell route context = PageShell
  { shellBodyAttributes :: [HtmlAttribute],
    shellNavigationAttributes :: [HtmlAttribute],
    shellNavigationItems :: [NavigationItem route],
    shellMainId :: Text,
    shellMainAttributes :: [HtmlAttribute],
    shellScriptSources :: [Text]
  }
  deriving (Eq, Show)

data ResponseBody = ResponseBody
  { responseStatus :: Int,
    responseContentType :: Text,
    responseBody :: Text,
    responseObservabilityAttributes :: [Observability.ObservabilityAttribute],
    responseLogEntries :: [Text]
  }
  deriving (Eq, Show)

data Response route context
  = PageResponse (Page route context)
  | BodyResponse ResponseBody
  deriving (Eq, Show)

data RouteCodec route context = RouteCodec
  { parseRoute :: context -> Text -> Maybe (RouteRequest route context),
    renderRoute :: RouteRequest route context -> Text,
    notFoundRequest :: context -> RouteRequest route context
  }

data Application route context = Application
  { appName :: Text,
    defaultRequestContext :: context,
    requestContextFromRequest :: Wai.Request -> context -> context,
    applicationStaticAssets :: StaticAssetsConfig,
    applicationRequestPolicy :: RequestPolicyConfig,
    routeCodec :: RouteCodec route context,
    renderResponse :: RouteRequest route context -> IO (Response route context),
    pageShell :: Page route context -> Text,
    reportRequestObservability :: Observability.RequestObservability -> IO (),
    reportApplicationLog :: Text -> IO ()
  }

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

application :: Application route context -> Application route context
application = id

routeHref :: RouteCodec route context -> context -> route -> Text
routeHref codec context route =
  renderRoute codec RouteRequest {requestRoute = route, requestContext = context}

staticAssetHref :: StaticAssetRoot -> FilePath -> Text
staticAssetHref =
  staticAssetHrefWithPrefix Text.empty

staticAssetHrefWithPrefix :: Text -> StaticAssetRoot -> FilePath -> Text
staticAssetHrefWithPrefix pathPrefix staticRoot assetPath =
  let normalizedPrefix = normalizeStaticPrefix (staticUrlPrefix staticRoot)
      normalizedAssetPath = trimLeadingSlash (Text.pack assetPath)
      assetHref =
        if Text.null normalizedPrefix
          then "/" <> normalizedAssetPath
          else
            Text.concat
              [ normalizedPrefix,
                "/",
                normalizedAssetPath
              ]
   in applyRequestPathPrefix pathPrefix assetHref

buildNavigation :: (Eq route) => RouteCodec route context -> Page route context -> [NavigationItem route] -> [ResolvedNavigationItem route]
buildNavigation codec page =
  map
    ( \NavigationItem {navigationLabel = itemLabel, navigationRoute = itemRoute} ->
        ResolvedNavigationItem
          { navigationLabel = itemLabel,
            navigationRoute = itemRoute,
            navigationHref = routeHref codec (pageContext page) itemRoute,
            navigationIsActive = pageRoute page == itemRoute
          }
    )

buildDocument :: (Eq route) => RouteCodec route context -> PageShell route context -> Page route context -> Document route
buildDocument codec shell page =
  Document
    { documentTitle = pageTitle page,
      documentBodyAttributes = shellBodyAttributes shell,
      documentNavigationAttributes = shellNavigationAttributes shell,
      documentNavigation = buildNavigation codec page (shellNavigationItems shell),
      documentMainId = shellMainId shell,
      documentMainAttributes = shellMainAttributes shell,
      documentMainContent = pageBody page,
      documentBootstrapHooks = pageBootstrapHooks page,
      documentScriptSources = shellScriptSources shell
    }

renderDocument :: Document route -> Text
renderDocument document =
  Text.concat
    [ "<html><head><title>",
      documentTitle document,
      "</title>",
      renderScriptSources (documentScriptSources document),
      "</head><body",
      renderAttributes (documentBodyAttributes document),
      "><nav",
      renderAttributes (documentNavigationAttributes document),
      ">",
      Text.concat (map renderNavigationItem (documentNavigation document)),
      "</nav><main id=\"",
      documentMainId document,
      "\"",
      renderAttributes (documentMainAttributes document <> renderBootstrapHookAttributes (documentBootstrapHooks document)),
      ">",
      documentMainContent document,
      "</main></body></html>"
    ]

buildPageShell :: (Eq route) => RouteCodec route context -> PageShell route context -> Page route context -> Text
buildPageShell codec shell = renderDocument . buildDocument codec shell

matchRoute :: RouteCodec route context -> context -> Text -> RouteRequest route context
matchRoute codec context path = fromMaybe (notFoundRequest codec context) (parseRoute codec context path)

toWaiApplication :: (Eq route) => Application route context -> Wai.Application
toWaiApplication webApplication request respond =
  case requestRedirectLocation (applicationRequestPolicy webApplication) request of
    Just redirectLocation ->
      respond
        (httpsRedirectResponse redirectLocation)
    Nothing -> do
      maybeStaticResponse <- serveStaticAssetResponse (applicationStaticAssets webApplication) (waiRequestPath request)
      case maybeStaticResponse of
        Just staticResponse ->
          respond
            (applyResponseHeaders (requestPolicyResponseHeaders (applicationRequestPolicy webApplication) request) staticResponse)
        Nothing -> do
          let requestContext =
                requestContextFromRequest
                  webApplication
                  request
                  (defaultRequestContext webApplication)
              routeRequest =
                matchRoute
                  (routeCodec webApplication)
                  requestContext
                  (waiRequestPath request)
          response <- renderResponse webApplication routeRequest
          let requestContextAttributes = requestContextObservabilityAttributes request
              requestLogFields = requestLogContextFields request
              extraObservabilityAttributes =
                requestContextAttributes
                  <> case response of
                    PageResponse _ -> []
                    BodyResponse responseBodyValue -> responseObservabilityAttributes responseBodyValue
              contextualizedResponseLogEntries =
                case response of
                  PageResponse _ -> []
                  BodyResponse responseBodyValue ->
                    map
                      (prependRequestLogContext requestLogFields)
                      (responseLogEntries responseBodyValue)
              requestObservability =
                Observability.buildRequestObservability
                  (TextEncoding.decodeUtf8 (Wai.requestMethod request))
                  (requestScheme request)
                  (waiRequestPath request)
                  (renderRoute (routeCodec webApplication) routeRequest)
                  ( case response of
                      PageResponse page ->
                        if isNotFoundPage webApplication page
                          then 404
                          else 200
                      BodyResponse responseBodyValue -> responseStatus responseBodyValue
                  )
                  ( case response of
                      PageResponse _ -> Observability.PageResponseKind
                      BodyResponse _ -> Observability.BodyResponseKind
                  )
                  extraObservabilityAttributes
          Observability.forceRequestObservability requestObservability `seq`
            reportRequestObservability webApplication requestObservability
              >> mapM_ (reportApplicationLog webApplication) contextualizedResponseLogEntries
              >> respond
                ( applyResponseHeaders
                    (requestPolicyResponseHeaders (applicationRequestPolicy webApplication) request)
                    (toWaiResponse [] webApplication response)
                )

withLocalTestServer :: (Eq route) => Application route context -> (LocalTestServer -> IO a) -> IO a
withLocalTestServer webApplication useLocalServer =
  bracket (startLocalTestServer webApplication) stopLocalTestServer $
    useLocalServer . runningLocalServerInfo

runServer :: (Eq route, HasServerConfig config) => Handle -> config -> Application route context -> IO ()
runServer outputHandle config webApplication =
  case planServerStartup config of
    Left startupError -> ioError (userError ("Invalid listener startup plan: " <> show startupError))
    Right startupPlan -> do
      let observabilityPlan = planObservabilityStartup (observability (toServerConfig config))
      case runtimeStartupValidationError startupPlan of
        Just runtimeError ->
          ioError (userError runtimeError)
        Nothing ->
          observabilityPlan `seq`
            bracket
              (startHttpRuntimeServers (httpEndpoints (httpBindPlan startupPlan)) (toWaiApplication webApplication))
              stopRuntimeServers
              ( \httpServers ->
                  bracket
                    (startManualTlsRuntimeServers (manualTlsBindPlans startupPlan) (toWaiApplication webApplication))
                    stopRuntimeServers
                    ( \manualTlsServers ->
                        httpServers `seq`
                          manualTlsServers `seq`
                            announceRuntimeStartup outputHandle startupPlan
                              >> waitForShutdownSignal
                    )
              )

startLocalTestServer :: (Eq route) => Application route context -> IO RunningLocalTestServer
startLocalTestServer webApplication = do
  listeningSocket <- openLoopbackSocket
  localPort <- socketPort listeningSocket
  serverThreadId <-
    startWarpServerOnSocket localPort listeningSocket (toWaiApplication webApplication)
  localPort `seq`
    pure
      RunningLocalTestServer
        { runningLocalServerInfo =
            LocalTestServer
              { localServerHost = "127.0.0.1",
                localServerPort = localPort,
                localServerBaseUrl = Text.pack ("http://127.0.0.1:" <> show localPort)
              },
          runningLocalServerSocket = listeningSocket,
          runningLocalServerThreadId = serverThreadId
        }

stopLocalTestServer :: RunningLocalTestServer -> IO ()
stopLocalTestServer runningServer = do
  Socket.close (runningLocalServerSocket runningServer)
  killThread (runningLocalServerThreadId runningServer)

openLoopbackSocket :: IO Socket.Socket
openLoopbackSocket =
  openListenerSocket ListenerEndpoint {endpointHost = "127.0.0.1", endpointPort = 0}

socketPort :: Socket.Socket -> IO Int
socketPort listeningSocket = do
  Socket.SockAddrInet portNumber _ <- Socket.getSocketName listeningSocket
  pure (fromIntegral portNumber)

data RunningRuntimeServer = RunningRuntimeServer
  { runningRuntimeSocket :: Socket.Socket,
    runningRuntimeThreadId :: ThreadId
  }

startHttpRuntimeServers :: [ListenerEndpoint] -> Wai.Application -> IO [RunningRuntimeServer]
startHttpRuntimeServers endpoints waiApplication =
  go [] endpoints
  where
    go runningServers remainingEndpoints =
      case remainingEndpoints of
        [] -> pure (reverse runningServers)
        endpoint : remaining ->
          ( do
              runningServer <- startHttpRuntimeServer endpoint waiApplication
              go (runningServer : runningServers) remaining
                `onException` stopRuntimeServers (runningServer : runningServers)
          )
            `onException` stopRuntimeServers runningServers

startManualTlsRuntimeServers :: [ManualTlsBindPlan] -> Wai.Application -> IO [RunningRuntimeServer]
startManualTlsRuntimeServers manualTlsPlans waiApplication =
  go [] manualTlsPlans
  where
    go runningServers remainingPlans =
      case remainingPlans of
        [] -> pure (reverse runningServers)
        manualTlsPlan : remaining ->
          ( do
              runningServer <- startManualTlsRuntimeServer manualTlsPlan waiApplication
              go (runningServer : runningServers) remaining
                `onException` stopRuntimeServers (runningServer : runningServers)
          )
            `onException` stopRuntimeServers runningServers

startHttpRuntimeServer :: ListenerEndpoint -> Wai.Application -> IO RunningRuntimeServer
startHttpRuntimeServer endpoint waiApplication = do
  listeningSocket <- openListenerSocket endpoint
  serverThreadId <-
    startWarpServerOnSocket (endpointPort endpoint) listeningSocket waiApplication
  endpoint `seq`
    pure
      RunningRuntimeServer
        { runningRuntimeSocket = listeningSocket,
          runningRuntimeThreadId = serverThreadId
        }

startManualTlsRuntimeServer :: ManualTlsBindPlan -> Wai.Application -> IO RunningRuntimeServer
startManualTlsRuntimeServer manualTlsPlan waiApplication = do
  ensureRuntimeFileExists "Manual TLS certificate file does not exist: " (tlsCertificateFile manualTlsPlan)
  ensureRuntimeFileExists "Manual TLS private key file does not exist: " (tlsPrivateKeyFile manualTlsPlan)
  let endpoint = tlsEndpoint manualTlsPlan
      tlsSettings =
        WarpTLS.tlsSettings
          (tlsCertificateFile manualTlsPlan)
          (tlsPrivateKeyFile manualTlsPlan)
  listeningSocket <- openListenerSocket endpoint
  serverThreadId <-
    startWarpTlsServerOnSocket (endpointPort endpoint) tlsSettings listeningSocket waiApplication
      `onException` Socket.close listeningSocket
  manualTlsPlan `seq`
    pure
      RunningRuntimeServer
        { runningRuntimeSocket = listeningSocket,
          runningRuntimeThreadId = serverThreadId
        }

stopRuntimeServers :: [RunningRuntimeServer] -> IO ()
stopRuntimeServers =
  mapM_ stopRuntimeServer

stopRuntimeServer :: RunningRuntimeServer -> IO ()
stopRuntimeServer runningServer = do
  Socket.close (runningRuntimeSocket runningServer)
  killThread (runningRuntimeThreadId runningServer)

openListenerSocket :: ListenerEndpoint -> IO Socket.Socket
openListenerSocket endpoint = do
  addressInfo :| _ <-
    ( Socket.getAddrInfo
        (Just listenerSocketHints)
        (Just (Text.unpack (endpointHost endpoint)))
        (Just (show (endpointPort endpoint))) ::
        IO (NonEmpty Socket.AddrInfo)
    )
  listeningSocket <- Socket.openSocket addressInfo
  Socket.setSocketOption listeningSocket Socket.ReuseAddr 1
  Socket.bind listeningSocket (Socket.addrAddress addressInfo)
  Socket.listen listeningSocket Socket.maxListenQueue
  pure listeningSocket

data RuntimeServerReady = RuntimeServerReady

startWarpServerOnSocket :: Int -> Socket.Socket -> Wai.Application -> IO ThreadId
startWarpServerOnSocket portNumber listeningSocket waiApplication =
  startWarpRuntimeServerOnSocket $ \startupSignal ->
    Warp.runSettingsSocket
      (runtimeServerSettings portNumber startupSignal)
      listeningSocket
      waiApplication

startWarpTlsServerOnSocket :: Int -> WarpTLS.TLSSettings -> Socket.Socket -> Wai.Application -> IO ThreadId
startWarpTlsServerOnSocket portNumber tlsSettings listeningSocket waiApplication =
  startWarpRuntimeServerOnSocket $ \startupSignal ->
    WarpTLS.runTLSSocket
      tlsSettings
      (runtimeServerSettings portNumber startupSignal)
      listeningSocket
      waiApplication

startWarpRuntimeServerOnSocket :: (MVar (Either SomeException RuntimeServerReady) -> IO ()) -> IO ThreadId
startWarpRuntimeServerOnSocket runServerOnSocket = do
  startupSignal <- newEmptyMVar
  threadId <-
    forkFinally
      (runServerOnSocket startupSignal)
      (reportRuntimeServerExit startupSignal)
  _ <- waitForRuntimeServerStartup startupSignal
  pure threadId

runtimeServerSettings :: Int -> MVar (Either SomeException RuntimeServerReady) -> Warp.Settings
runtimeServerSettings portNumber startupSignal =
  Warp.setPort portNumber $
    Warp.setBeforeMainLoop (putMVar startupSignal (Right RuntimeServerReady)) Warp.defaultSettings

reportRuntimeServerExit :: MVar (Either SomeException RuntimeServerReady) -> Either SomeException () -> IO ()
reportRuntimeServerExit startupSignal exitResult =
  mapM_ (tryPutMVar startupSignal . Left) (lefts [exitResult])

waitForRuntimeServerStartup :: MVar (Either SomeException RuntimeServerReady) -> IO RuntimeServerReady
waitForRuntimeServerStartup startupSignal = do
  startupResult <- takeMVar startupSignal
  case startupResult of
    Left startupException -> throwIO startupException
    Right runtimeServerReady@RuntimeServerReady -> evaluate runtimeServerReady

ensureRuntimeFileExists :: String -> FilePath -> IO ()
ensureRuntimeFileExists errorPrefix filePath = do
  fileExists <- doesFileExist filePath
  unless fileExists (ioError (userError (errorPrefix <> filePath)))

announceRuntimeStartup :: Handle -> ServerStartupPlan -> IO ()
announceRuntimeStartup outputHandle startupPlan = do
  mapM_ (hPutStrLn outputHandle . uncurry listenerStartupMessage) (runtimeStartupListeners startupPlan)
  hFlush outputHandle

runtimeStartupListeners :: ServerStartupPlan -> [(ListenerScheme, ListenerEndpoint)]
runtimeStartupListeners startupPlan =
  map (Http,) (httpEndpoints (httpBindPlan startupPlan))
    <> map ((Https,) . tlsEndpoint) (manualTlsBindPlans startupPlan)

listenerStartupMessage :: ListenerScheme -> ListenerEndpoint -> String
listenerStartupMessage listenerScheme endpoint =
  listenerSchemePrefix listenerScheme
    <> Text.unpack (endpointHost endpoint)
    <> ":"
    <> show (endpointPort endpoint)

listenerSchemePrefix :: ListenerScheme -> String
listenerSchemePrefix listenerScheme =
  case listenerScheme of
    Http -> "HTTP Server listening at http://"
    Https -> "HTTPS Server listening at https://"

waitForShutdownSignal :: IO ()
waitForShutdownSignal = do
  shutdownSignal <- newEmptyMVar :: IO (MVar ())
  takeMVar shutdownSignal

runtimeStartupValidationError :: ServerStartupPlan -> Maybe String
runtimeStartupValidationError startupPlan =
  case ( null (acmeBindPlans startupPlan),
         null (httpEndpoints (httpBindPlan startupPlan)),
         null (manualTlsBindPlans startupPlan)
       ) of
    (False, _, _) ->
      Just "Unsupported runtime listener startup plan: ACME listeners are not implemented yet."
    (True, True, True) ->
      Just "Unsupported runtime listener startup plan: no runtime listeners are configured."
    (True, _, _) ->
      Nothing

listenerSocketHints :: Socket.AddrInfo
listenerSocketHints =
  Socket.defaultHints
    { Socket.addrFlags = [Socket.AI_NUMERICHOST, Socket.AI_NUMERICSERV],
      Socket.addrFamily = Socket.AF_INET,
      Socket.addrSocketType = Socket.Stream
    }

renderAttributes :: [HtmlAttribute] -> Text
renderAttributes = Text.concat . map renderAttribute

renderAttribute :: HtmlAttribute -> Text
renderAttribute attribute =
  Text.concat
    [ " ",
      attributeName attribute,
      "=\"",
      attributeValue attribute,
      "\""
    ]

renderBootstrapHookAttributes :: [Text] -> [HtmlAttribute]
renderBootstrapHookAttributes bootstrapHooks =
  case bootstrapHooks of
    [] -> []
    _ ->
      [ HtmlAttribute
          { attributeName = "data-bootstrap-hooks",
            attributeValue = Text.intercalate "," bootstrapHooks
          }
      ]

renderNavigationItem :: ResolvedNavigationItem route -> Text
renderNavigationItem ResolvedNavigationItem {navigationLabel = itemLabel, navigationHref = itemHref, navigationIsActive = itemIsActive} =
  Text.concat
    [ "<a href=\"",
      itemHref,
      "\"",
      if itemIsActive then " aria-current=\"page\"" else Text.empty,
      ">",
      itemLabel,
      "</a>"
    ]

renderScriptSources :: [Text] -> Text
renderScriptSources =
  Text.concat . map renderScriptSource

renderScriptSource :: Text -> Text
renderScriptSource scriptSource =
  Text.concat
    [ "<script src=\"",
      scriptSource,
      "\" defer></script>"
    ]

toWaiResponse :: (Eq route) => Http.ResponseHeaders -> Application route context -> Response route context -> Wai.Response
toWaiResponse additionalHeaders webApplication response =
  case response of
    PageResponse page ->
      Wai.responseLBS
        (if isNotFoundPage webApplication page then Http.status404 else Http.status200)
        (additionalHeaders <> [(Http.hContentType, TextEncoding.encodeUtf8 htmlContentType)])
        (LazyByteString.fromStrict (TextEncoding.encodeUtf8 (pageShell webApplication page)))
    BodyResponse responseBodyValue ->
      Wai.responseLBS
        (Http.mkStatus (responseStatus responseBodyValue) mempty)
        (additionalHeaders <> [(Http.hContentType, TextEncoding.encodeUtf8 (responseContentType responseBodyValue))])
        (LazyByteString.fromStrict (TextEncoding.encodeUtf8 (responseBody responseBodyValue)))

applyResponseHeaders :: Http.ResponseHeaders -> Wai.Response -> Wai.Response
applyResponseHeaders additionalHeaders =
  Wai.mapResponseHeaders (additionalHeaders <>)

httpsRedirectResponse :: ByteString.ByteString -> Wai.Response
httpsRedirectResponse redirectLocation =
  Wai.responseLBS
    Http.status308
    [ (Http.hLocation, redirectLocation),
      (Http.hContentType, TextEncoding.encodeUtf8 plainTextContentType)
    ]
    "Redirecting to HTTPS"

isNotFoundPage :: (Eq route) => Application route context -> Page route context -> Bool
isNotFoundPage webApplication page =
  let pageRequestContext = pageContext page
   in pageRequestContext `seq`
        pageRoute page == requestRoute (notFoundRequest (routeCodec webApplication) pageRequestContext)

waiRequestPath :: Wai.Request -> Text
waiRequestPath request =
  stripRequestPathPrefix
    (requestPathPrefix request)
    (rawRequestPath request)

requestRedirectLocation :: RequestPolicyConfig -> Wai.Request -> Maybe ByteString.ByteString
requestRedirectLocation requestPolicyConfig request =
  if redirectHttpToHttps requestPolicyConfig && requestScheme request == "http"
    then
      fmap
        ( \redirectAuthority ->
            "https://"
              <> redirectAuthority
              <> requestRedirectPathAndQuery request
        )
        (requestRedirectAuthority request)
    else Nothing

requestRedirectAuthority :: Wai.Request -> Maybe ByteString.ByteString
requestRedirectAuthority request =
  fmap
    (\hostHeader -> fromMaybe hostHeader (ByteStringChar8.stripSuffix ":80" hostHeader))
    (lookup "Host" (Wai.requestHeaders request))

requestRedirectPathAndQuery :: Wai.Request -> ByteString.ByteString
requestRedirectPathAndQuery request =
  TextEncoding.encodeUtf8 (externalRequestPath request) <> Wai.rawQueryString request

requestPolicyResponseHeaders :: RequestPolicyConfig -> Wai.Request -> Http.ResponseHeaders
requestPolicyResponseHeaders requestPolicyConfig request =
  case strictTransportSecurity requestPolicyConfig of
    Just strictTransportSecurityConfig
      | requestScheme request == "https" ->
          [ ( "Strict-Transport-Security",
              TextEncoding.encodeUtf8 (strictTransportSecurityHeaderValue strictTransportSecurityConfig)
            )
          ]
    _ -> []

strictTransportSecurityHeaderValue :: StrictTransportSecurityConfig -> Text
strictTransportSecurityHeaderValue strictTransportSecurityConfig =
  Text.intercalate
    "; "
    ( [ "max-age=" <> Text.pack (show (strictTransportSecurityMaxAgeSeconds strictTransportSecurityConfig))
      ]
        ++ [ "includeSubDomains"
           | strictTransportSecurityIncludeSubDomains strictTransportSecurityConfig
           ]
        ++ ["preload" | strictTransportSecurityPreload strictTransportSecurityConfig]
    )

requestContextObservabilityAttributes :: Wai.Request -> [Observability.ObservabilityAttribute]
requestContextObservabilityAttributes request =
  [ textObservabilityAttribute "client.address" (effectiveClientAddress request),
    textObservabilityAttribute "network.peer.address" (peerAddressText request)
  ]
    ++ maybe
      []
      (pure . textObservabilityAttribute "http.request.header.x_forwarded_for")
      (requestHeaderText "X-Forwarded-For" request)
    ++ maybe
      []
      (pure . textObservabilityAttribute "http.request.header.x_forwarded_proto")
      (requestHeaderText "X-Forwarded-Proto" request)
    ++ maybe
      []
      (pure . textObservabilityAttribute "http.request.header.x_forwarded_prefix")
      (requestHeaderText "X-Forwarded-Prefix" request)

requestLogContextFields :: Wai.Request -> [Text]
requestLogContextFields request =
  [ renderRequestLogField "client.address" (effectiveClientAddress request),
    renderRequestLogField "network.peer.address" (peerAddressText request)
  ]
    ++ optionalRequestLogField
      "http.request.header.x_forwarded_for"
      (requestHeaderText "X-Forwarded-For" request)
    ++ optionalRequestLogField
      "http.request.header.x_forwarded_proto"
      (requestHeaderText "X-Forwarded-Proto" request)
    ++ optionalRequestLogField
      "http.request.header.x_forwarded_prefix"
      (requestHeaderText "X-Forwarded-Prefix" request)
    ++ [renderRequestLogField "url.scheme" (requestScheme request)]

optionalRequestLogField :: Text -> Maybe Text -> [Text]
optionalRequestLogField fieldName maybeFieldValue =
  case maybeFieldValue of
    Just fieldValue -> [renderRequestLogField fieldName fieldValue]
    Nothing -> []

textObservabilityAttribute :: Text -> Text -> Observability.ObservabilityAttribute
textObservabilityAttribute name value =
  Observability.ObservabilityAttribute
    { Observability.attributeName = name,
      Observability.attributeValue = Observability.TextAttribute value
    }

requestScheme :: Wai.Request -> Text
requestScheme request =
  case fmap Text.toLower (requestHeaderToken "X-Forwarded-Proto" request) of
    Just "https" -> "https"
    Just "http" -> "http"
    _ ->
      if Wai.isSecure request
        then "https"
        else "http"

effectiveClientAddress :: Wai.Request -> Text
effectiveClientAddress request =
  fromMaybe
    (peerAddressText request)
    (requestHeaderToken "X-Forwarded-For" request)

peerAddressText :: Wai.Request -> Text
peerAddressText request =
  socketAddressText (Wai.remoteHost request)

socketAddressText :: Socket.SockAddr -> Text
socketAddressText socketAddress =
  case socketAddress of
    Socket.SockAddrInet _ hostAddress ->
      let (firstOctet, secondOctet, thirdOctet, fourthOctet) =
            Socket.hostAddressToTuple hostAddress
       in Text.intercalate
            "."
            (map (Text.pack . show) [firstOctet, secondOctet, thirdOctet, fourthOctet])
    _ -> Text.pack (show socketAddress)

requestHeaderToken :: Http.HeaderName -> Wai.Request -> Maybe Text
requestHeaderToken headerName request =
  requestHeaderText headerName request >>= firstCommaSeparatedValue

requestHeaderText :: Http.HeaderName -> Wai.Request -> Maybe Text
requestHeaderText headerName request =
  fmap
    (Text.strip . TextEncoding.decodeUtf8)
    (lookup headerName (Wai.requestHeaders request))

requestPathPrefix :: Wai.Request -> Text
requestPathPrefix request =
  maybe
    Text.empty
    normalizeRequestPathPrefix
    (requestHeaderToken "X-Forwarded-Prefix" request)

rawRequestPath :: Wai.Request -> Text
rawRequestPath request =
  if ByteString.null (Wai.rawPathInfo request)
    then "/"
    else TextEncoding.decodeUtf8 (Wai.rawPathInfo request)

externalRequestPath :: Wai.Request -> Text
externalRequestPath request =
  applyRequestPathPrefix
    (requestPathPrefix request)
    (waiRequestPath request)

normalizeRequestPathPrefix :: Text -> Text
normalizeRequestPathPrefix pathPrefix =
  let trimmedPrefix = Text.strip pathPrefix
      slashPrefixedPrefix =
        case (Text.null trimmedPrefix || trimmedPrefix == "/", Text.isPrefixOf "/" trimmedPrefix) of
          (True, _) -> Text.empty
          (False, True) -> trimmedPrefix
          (False, False) -> "/" <> trimmedPrefix
      normalizedPrefix =
        Text.dropWhileEnd (== '/') slashPrefixedPrefix
   in normalizedPrefix

applyRequestPathPrefix :: Text -> Text -> Text
applyRequestPathPrefix pathPrefix path =
  let normalizedPrefix = normalizeRequestPathPrefix pathPrefix
   in if Text.null normalizedPrefix
        then path
        else
          if path == "/"
            then normalizedPrefix
            else normalizedPrefix <> path

stripRequestPathPrefix :: Text -> Text -> Text
stripRequestPathPrefix pathPrefix path =
  let normalizedPrefix = normalizeRequestPathPrefix pathPrefix
   in if Text.null normalizedPrefix
        then path
        else
          if path == normalizedPrefix
            then "/"
            else maybe path ("/" <>) (Text.stripPrefix (normalizedPrefix <> "/") path)

firstCommaSeparatedValue :: Text -> Maybe Text
firstCommaSeparatedValue value =
  case filter (not . Text.null) (map Text.strip (Text.splitOn "," value)) of
    [] -> Nothing
    firstValue : _ -> Just firstValue

prependRequestLogContext :: [Text] -> Text -> Text
prependRequestLogContext fields logEntry =
  "[" <> Text.intercalate " " fields <> "] " <> logEntry

renderRequestLogField :: Text -> Text -> Text
renderRequestLogField fieldName fieldValue =
  fieldName <> "=" <> Text.pack (show fieldValue)

htmlContentType :: Text
htmlContentType = "text/html; charset=utf-8"

plainTextContentType :: Text
plainTextContentType = "text/plain; charset=utf-8"

serveStaticAssetResponse :: StaticAssetsConfig -> Text -> IO (Maybe Wai.Response)
serveStaticAssetResponse staticAssetsConfig requestPath =
  case matchStaticAssetRoot staticAssetsConfig requestPath of
    Nothing -> pure Nothing
    Just (matchedRoot, relativeAssetPath) ->
      case sanitizeStaticAssetPath relativeAssetPath of
        Nothing -> pure (Just (missingStaticAssetResponse staticAssetsConfig))
        Just safeAssetPath -> do
          let assetFilePath = staticDirectory matchedRoot </> safeAssetPath
          assetExists <- doesFileExist assetFilePath
          case assetExists of
            True -> do
              assetContents <- ByteString.readFile assetFilePath
              pure
                ( Just
                    ( Wai.responseLBS
                        Http.status200
                        (staticAssetHeaders staticAssetsConfig assetFilePath)
                        (LazyByteString.fromStrict assetContents)
                    )
                )
            False -> pure (Just (missingStaticAssetResponse staticAssetsConfig))

matchStaticAssetRoot :: StaticAssetsConfig -> Text -> Maybe (StaticAssetRoot, FilePath)
matchStaticAssetRoot staticAssetsConfig requestPath =
  case matchedRoots of
    [] -> Nothing
    _ -> Just (maximumBy compareStaticPrefixLength matchedRoots)
  where
    matchedRoots =
      [ (staticRoot, Text.unpack assetPath)
      | staticRoot <- staticAssetRoots staticAssetsConfig,
        Just assetPath <- [stripStaticPrefix (staticUrlPrefix staticRoot) requestPath]
      ]

    compareStaticPrefixLength (leftRoot, _) (rightRoot, _) =
      compare (Text.length (staticUrlPrefix leftRoot)) (Text.length (staticUrlPrefix rightRoot))

stripStaticPrefix :: Text -> Text -> Maybe Text
stripStaticPrefix configuredPrefix requestPath =
  let normalizedPrefix = normalizeStaticPrefix configuredPrefix
   in if Text.null normalizedPrefix
        then
          if requestPath == "/"
            then Just Text.empty
            else Text.stripPrefix "/" requestPath
        else
          if requestPath == normalizedPrefix
            then Just Text.empty
            else
              Text.stripPrefix
                (normalizedPrefix <> "/")
                requestPath

sanitizeStaticAssetPath :: FilePath -> Maybe FilePath
sanitizeStaticAssetPath assetPath =
  case splitDirectories assetPath of
    [] -> Nothing
    segments ->
      if all isSafeSegment segments
        then Just assetPath
        else Nothing
  where
    isSafeSegment segment =
      not (null segment)
        && segment /= "."
        && segment /= ".."

staticAssetHeaders :: StaticAssetsConfig -> FilePath -> Http.ResponseHeaders
staticAssetHeaders staticAssetsConfig assetFilePath =
  (Http.hContentType, TextEncoding.encodeUtf8 (staticAssetContentType assetFilePath))
    : maybe [] (\cacheHeader -> [(Http.hCacheControl, TextEncoding.encodeUtf8 cacheHeader)]) (staticCacheControlHeaderValue staticAssetsConfig)

staticCacheControlHeaderValue :: StaticAssetsConfig -> Maybe Text
staticCacheControlHeaderValue staticAssetsConfig =
  fmap
    (\seconds -> Text.pack ("public, max-age=" <> show seconds))
    (staticCacheControlSeconds staticAssetsConfig)

staticAssetContentType :: FilePath -> Text
staticAssetContentType assetFilePath =
  case takeExtension assetFilePath of
    ".css" -> "text/css; charset=utf-8"
    ".html" -> "text/html; charset=utf-8"
    ".js" -> "application/javascript; charset=utf-8"
    ".json" -> "application/json; charset=utf-8"
    ".svg" -> "image/svg+xml"
    ".txt" -> "text/plain; charset=utf-8"
    _ -> "application/octet-stream"

missingStaticAssetResponse :: StaticAssetsConfig -> Wai.Response
missingStaticAssetResponse staticAssetsConfig =
  Wai.responseLBS
    Http.status404
    ( (Http.hContentType, TextEncoding.encodeUtf8 "text/plain; charset=utf-8")
        : maybe [] (\cacheHeader -> [(Http.hCacheControl, TextEncoding.encodeUtf8 cacheHeader)]) (staticCacheControlHeaderValue staticAssetsConfig)
    )
    (LazyByteString.fromStrict (TextEncoding.encodeUtf8 "Not Found"))

normalizeStaticPrefix :: Text -> Text
normalizeStaticPrefix prefix =
  case Text.stripSuffix "/" prefix of
    Just trimmedPrefix ->
      if Text.null trimmedPrefix
        then Text.empty
        else trimmedPrefix
    Nothing -> prefix

trimLeadingSlash :: Text -> Text
trimLeadingSlash assetPath =
  fromMaybe assetPath (Text.stripPrefix "/" assetPath)

planServerStartup :: (HasServerConfig config) => config -> Either ListenerStartupError ServerStartupPlan
planServerStartup config = do
  plannedListeners <- traverse classifyListener (listenerConfigs (toServerConfig config))
  case firstDuplicate (map plannedEndpoint plannedListeners) of
    Just duplicateEndpoint -> Left (DuplicateListenerEndpoint duplicateEndpoint)
    Nothing ->
      Right
        ServerStartupPlan
          { httpBindPlan =
              HttpBindPlan
                { httpEndpoints =
                    [ endpoint
                    | PlannedHttp endpoint <- plannedListeners
                    ]
                },
            manualTlsBindPlans =
              [ manualTlsBindPlan
              | PlannedManualTls manualTlsBindPlan <- plannedListeners
              ],
            acmeBindPlans =
              [ acmeBindPlan
              | PlannedAcme acmeBindPlan <- plannedListeners
              ]
          }
  where
    classifyListener listenerConfig =
      case (listenerScheme listenerConfig, listenerTls listenerConfig) of
        (Http, Nothing) ->
          Right (PlannedHttp (listenerEndpoint listenerConfig))
        (Http, Just _) ->
          Left (InvalidListenerTlsConfiguration listenerConfig)
        (Https, Nothing) ->
          Left (InvalidListenerTlsConfiguration listenerConfig)
        (Https, Just TlsConfig {certificateSource = ManualCertificateFiles {certificateFile = certificatePath, privateKeyFile = privateKeyPath}}) ->
          Right
            ( PlannedManualTls
                ManualTlsBindPlan
                  { tlsEndpoint = listenerEndpoint listenerConfig,
                    tlsCertificateFile = certificatePath,
                    tlsPrivateKeyFile = privateKeyPath
                  }
            )
        (Https, Just TlsConfig {certificateSource = AcmeCertificateSource acmeConfig}) ->
          Right
            ( PlannedAcme
                AcmeBindPlan
                  { acmeEndpoint = listenerEndpoint listenerConfig,
                    acmeListenerConfig = acmeConfig
                  }
            )

planObservabilityStartup :: ObservabilityConfig -> ObservabilityStartupPlan
planObservabilityStartup observabilityConfig =
  ObservabilityStartupPlan
    { startupExporters =
        maybe [] (pure . buildStartup TracingSignal) (tracingExporter observabilityConfig)
          ++ maybe [] (pure . buildStartup MetricsSignal) (metricsExporter observabilityConfig)
    }
  where
    buildStartup signal exporter =
      OtlpExporterStartup
        { startupSignal = signal,
          startupEndpoint = otlpEndpoint exporter,
          startupHeaders = otlpHeaders exporter
        }

data PlannedListener
  = PlannedHttp ListenerEndpoint
  | PlannedManualTls ManualTlsBindPlan
  | PlannedAcme AcmeBindPlan

plannedEndpoint :: PlannedListener -> ListenerEndpoint
plannedEndpoint plannedListener =
  case plannedListener of
    PlannedHttp endpoint -> endpoint
    PlannedManualTls manualTlsBindPlan -> tlsEndpoint manualTlsBindPlan
    PlannedAcme acmeBindPlan -> acmeEndpoint acmeBindPlan

listenerEndpoint :: ListenerConfig -> ListenerEndpoint
listenerEndpoint listenerConfig =
  ListenerEndpoint
    { endpointHost = listenerHost listenerConfig,
      endpointPort = listenerPort listenerConfig
    }

firstDuplicate :: (Eq value) => [value] -> Maybe value
firstDuplicate values =
  case values of
    [] -> Nothing
    value : remainingValues ->
      if value `elem` remainingValues
        then Just value
        else firstDuplicate remainingValues
