{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

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
    OtlpExporter (..),
    Page (..),
    PageShell (..),
    Response (..),
    ResponseBody (..),
    ResolvedNavigationItem (..),
    RouteCodec (..),
    RouteRequest (..),
    ServerConfig (..),
    ServerStartupPlan (..),
    StaticAssetRoot (..),
    StaticAssetsConfig (..),
    AcmeBindPlan (..),
    TlsCertificateSource (..),
    TlsConfig (..),
    application,
    buildDocument,
    buildNavigation,
    buildPageShell,
    matchRoute,
    planServerStartup,
    routeHref,
    renderDocument,
    runServer,
    staticAssetHref,
    toWaiApplication,
    withLocalTestServer,
  )
where

import Control.Concurrent (ThreadId, forkIO, killThread, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List (maximumBy)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Network.HTTP.Types qualified as Http
import Network.Socket qualified as Socket
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import System.Directory (doesFileExist)
import System.FilePath (splitDirectories, takeExtension, (</>))
import System.IO (Handle, hPutStrLn)

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

data ServerConfig = ServerConfig
  { listenerConfigs :: [ListenerConfig],
    staticAssets :: StaticAssetsConfig,
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
    pageBody :: Text
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
    responseBody :: Text
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
    applicationStaticAssets :: StaticAssetsConfig,
    routeCodec :: RouteCodec route context,
    renderResponse :: RouteRequest route context -> IO (Response route context),
    pageShell :: Page route context -> Text
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
staticAssetHref staticRoot assetPath =
  let normalizedPrefix = normalizeStaticPrefix (staticUrlPrefix staticRoot)
      normalizedAssetPath = trimLeadingSlash (Text.pack assetPath)
   in if Text.null normalizedPrefix
        then "/" <> normalizedAssetPath
        else
          Text.concat
            [ normalizedPrefix,
              "/",
              normalizedAssetPath
            ]

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
      renderAttributes (documentMainAttributes document),
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
  do
    maybeStaticResponse <- serveStaticAssetResponse (applicationStaticAssets webApplication) (waiRequestPath request)
    case maybeStaticResponse of
      Just staticResponse -> respond staticResponse
      Nothing ->
        renderResponse
          webApplication
          ( matchRoute
              (routeCodec webApplication)
              (defaultRequestContext webApplication)
              (waiRequestPath request)
          )
          >>= respond . toWaiResponse webApplication

withLocalTestServer :: (Eq route) => Application route context -> (LocalTestServer -> IO a) -> IO a
withLocalTestServer webApplication useLocalServer =
  bracket (startLocalTestServer webApplication) stopLocalTestServer $
    useLocalServer . runningLocalServerInfo

runServer :: (Eq route, HasServerConfig config) => Handle -> config -> Application route context -> IO ()
runServer outputHandle config webApplication =
  case planServerStartup config of
    Left startupError -> ioError (userError ("Invalid listener startup plan: " <> show startupError))
    Right startupPlan -> do
      startupResponse <-
        fmap
          (toWaiResponse webApplication)
          ( renderResponse
              webApplication
              ( matchRoute
                  (routeCodec webApplication)
                  (defaultRequestContext webApplication)
                  "/"
              )
          )
      startupPlan `seq`
        Wai.responseStatus startupResponse `seq`
          hPutStrLn outputHandle "HTTP Server listening at http://localhost:5001"

startLocalTestServer :: (Eq route) => Application route context -> IO RunningLocalTestServer
startLocalTestServer webApplication = do
  listeningSocket <- openLoopbackSocket
  readySignal <- newEmptyMVar
  localPort <- socketPort listeningSocket
  let signalReady = putMVar readySignal localPort
  serverThreadId <-
    forkIO $
      Warp.runSettingsSocket
        ( Warp.setPort localPort $
            Warp.setBeforeMainLoop signalReady Warp.defaultSettings
        )
        listeningSocket
        (toWaiApplication webApplication)
  readyPort <- takeMVar readySignal
  readyPort `seq`
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
openLoopbackSocket = do
  addressInfo :| _ <-
    ( Socket.getAddrInfo
        (Just hints)
        (Just "127.0.0.1")
        (Just "0") ::
        IO (NonEmpty Socket.AddrInfo)
    )
  listeningSocket <- Socket.openSocket addressInfo
  Socket.setSocketOption listeningSocket Socket.ReuseAddr 1
  Socket.bind listeningSocket (Socket.addrAddress addressInfo)
  Socket.listen listeningSocket Socket.maxListenQueue
  pure listeningSocket
  where
    hints =
      Socket.defaultHints
        { Socket.addrFlags = [Socket.AI_NUMERICHOST, Socket.AI_NUMERICSERV],
          Socket.addrFamily = Socket.AF_INET,
          Socket.addrSocketType = Socket.Stream
        }

socketPort :: Socket.Socket -> IO Int
socketPort listeningSocket = do
  Socket.SockAddrInet portNumber _ <- Socket.getSocketName listeningSocket
  pure (fromIntegral portNumber)

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

toWaiResponse :: (Eq route) => Application route context -> Response route context -> Wai.Response
toWaiResponse webApplication response =
  case response of
    PageResponse page ->
      Wai.responseLBS
        (if isNotFoundPage webApplication page then Http.status404 else Http.status200)
        [(Http.hContentType, TextEncoding.encodeUtf8 htmlContentType)]
        (LazyByteString.fromStrict (TextEncoding.encodeUtf8 (pageShell webApplication page)))
    BodyResponse responseBodyValue ->
      Wai.responseLBS
        (Http.mkStatus (responseStatus responseBodyValue) mempty)
        [(Http.hContentType, TextEncoding.encodeUtf8 (responseContentType responseBodyValue))]
        (LazyByteString.fromStrict (TextEncoding.encodeUtf8 (responseBody responseBodyValue)))

isNotFoundPage :: (Eq route) => Application route context -> Page route context -> Bool
isNotFoundPage webApplication page =
  let pageRequestContext = pageContext page
   in pageRequestContext `seq`
        pageRoute page == requestRoute (notFoundRequest (routeCodec webApplication) pageRequestContext)

waiRequestPath :: Wai.Request -> Text
waiRequestPath request =
  if ByteString.null (Wai.rawPathInfo request)
    then "/"
    else TextEncoding.decodeUtf8 (Wai.rawPathInfo request)

htmlContentType :: Text
htmlContentType = "text/html; charset=utf-8"

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
