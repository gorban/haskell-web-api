{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module HarchWeb
  ( AcmeBindPlan (..),
    AcmeAuthorizationResponse (..),
    AcmeChallengeBackend (..),
    AcmeChallengeResponse (..),
    AcmeChallengeStore (..),
    AcmeConfig (..),
    AcmeDirectoryResponse (..),
    AcmeJwk (..),
    AcmeOrderIdentifier (..),
    AcmeOrderResponse (..),
    AcmeRequestAuth (..),
    ActiveAcmeChallenge (..),
    Application (..),
    CertbotConfig (..),
    Document (..),
    HasServerConfig (..),
    HtmlAttribute (..),
    HttpBindPlan (..),
    JsonValue (..),
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
    PreparedAcmeChallenge (..),
    RequestPolicyConfig (..),
    ReloadingTlsCredentials,
    Response (..),
    ResponseBody (..),
    ResolvedNavigationItem (..),
    RouteCodec (..),
    RouteRequest (..),
    RuntimeAcmeBindPlan (..),
    ServerConfig (..),
    ServerStartupPlan (..),
    StaticAssetRoot (..),
    StaticAssetsConfig (..),
    StrictTransportSecurityConfig (..),
    TelemetrySignal (..),
    TlsCertificateSource (..),
    TlsCredentialSourceKind (..),
    TlsStartupMode (..),
    TlsConfig (..),
    acmeCertificateRequestConfig,
    acmeChallengeResponseForRequest,
    acmeHttp01ChallengeToken,
    acmeJwkThumbprintBytes,
    application,
    base64urlText,
    buildAcmeJwsBody,
    buildAcmeKeyAuthorization,
    buildDocument,
    buildNavigation,
    buildPageShell,
    certbotCertificateName,
    certbotHasOption,
    certbotOptionValues,
    createAcmeAccount,
    createAcmeOrder,
    decodeAcmeJsonResponse,
    escapeJsonCharacter,
    exportRequestObservabilityToOtlp,
    fetchAcmeCertificate,
    fetchAcmeDirectory,
    fetchAcmeNonce,
    finalizeAcmeOrder,
    firstCertbotDomain,
    generateAcmeAccountKey,
    generateAcmeCertificateRequest,
    hexTextToByteString,
    jsonArrayBytes,
    jsonArrayItems,
    jsonBoolBytes,
    jsonObjectEntryParser,
    jsonObjectBytes,
    jsonObjectFields,
    jsonOptionalTextArrayField,
    jsonOptionalTextField,
    jsonRequiredField,
    jsonRequiredTextField,
    jsonStringCharacterParser,
    jsonStringBytes,
    jsonTextField,
    jsonValueParser,
    loadAcmeJwk,
    mailtoAcmeContact,
    matchRoute,
    matchesRuntimeAcmeChallenge,
    openSslSha256,
    parseAcmeAuthorizationResponse,
    parseAcmeChallengeResponse,
    parseAcmeDirectoryResponse,
    parseAcmeOrderIdentifier,
    parseAcmeOrderResponse,
    parseJsonValue,
    performAcmeJwsRequest,
    performAcmeRequest,
    planObservabilityStartup,
    planServerStartup,
    pollAcmeOrder,
    pollAcmeOrderWithRetries,
    prepareAcmeAuthorization,
    prepareCertbotManualTlsBindPlan,
    prepareInProcessManualTlsBindPlan,
    reloadTlsCredentialsIfChanged,
    registerAcmeChallenges,
    renderAcmeResponseBody,
    renderDocument,
    responseHeaderText,
    requestHostWithoutPort,
    routeHref,
    loadReloadingTlsCredentials,
    loadTlsCredentialSnapshotOrThrowWithLoader,
    startManualTlsRuntimeServerWithStarter,
    runInProcessAcmeChallenge,
    runOpenSslCommand,
    runOpenSslTextCommand,
    runServer,
    startWarpRuntimeServerOnSocket,
    runtimeCertbotArguments,
    signOpenSslRs256,
    splitCertbotDomainValue,
    staticAssetHref,
    staticAssetHrefWithPrefix,
    toWaiApplication,
    triggerAcmeChallenge,
    unicodeJsonCharacterParser,
    unregisterAcmeChallenges,
    withLocalTestServer,
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent (MVar, ThreadId, forkFinally, killThread, modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar, threadDelay, tryPutMVar)
import Control.Exception (IOException, SomeException, bracket, bracketOnError, bracket_, evaluate, onException, throwIO, try)
import Control.Monad (forever, replicateM, unless, void, when)
import Data.Bits (shiftR, xor)
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Base64.URL qualified as Base64Url
import Data.ByteString.Char8 qualified as ByteStringChar8
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Char (digitToInt, isDigit, toLower)
import Data.Either (lefts)
import Data.Functor (($>))
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.List (find, intercalate, maximumBy)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Time.Clock (UTCTime)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import HarchWeb.Observability qualified as Observability
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Client.TLS qualified as HttpClientTls
import Network.HTTP.Types qualified as Http
import Network.Socket qualified as Socket
import Network.TLS qualified as TLS
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WarpTLS qualified as WarpTLS
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, getModificationTime, removePathForcibly)
import System.Exit (ExitCode (..))
import System.FilePath (splitDirectories, takeExtension, (</>))
import System.IO (Handle, hFlush, hPutStrLn)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (proc, readCreateProcessWithExitCode)
import Text.ParserCombinators.ReadP (ReadP, char, choice, eof, get, manyTill, pfail, readP_to_S, sepBy, skipSpaces, string, (<++))
import Text.Read (readMaybe)

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
    acmeDomains :: [Text],
    acmeHttp01Port :: Int,
    acmeCertificateDirectory :: Maybe FilePath,
    acmeChallengeBackend :: AcmeChallengeBackend
  }
  deriving (Eq, Show)

data TlsCertificateSource
  = ManualCertificateFiles
      { certificateFile :: FilePath,
        privateKeyFile :: FilePath
      }
  | SharedCertificateFiles
      { certificateDirectory :: FilePath,
        sharedCertificateStartupMode :: TlsStartupMode
      }
  | AcmeCertificateSource AcmeConfig
  deriving (Eq, Show)

data TlsStartupMode
  = RequireCertificateFiles
  | AwaitCertificateFiles
      { certificateWaitTimeoutSeconds :: Maybe Int
      }
  deriving (Eq, Show)

data TlsCredentialSourceKind
  = ManualTlsCredentials
  | SharedTlsCredentials
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
    httpsRedirectPort :: Maybe Int,
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
    tlsPrivateKeyFile :: FilePath,
    tlsCredentialSourceKind :: TlsCredentialSourceKind,
    tlsStartupMode :: TlsStartupMode
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
  | PageResponseWithMetadata ResponseBody (Page route context)
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
                    PageResponseWithMetadata pageResponseBodyValue _ ->
                      responseObservabilityAttributes pageResponseBodyValue
                    BodyResponse responseBodyValue -> responseObservabilityAttributes responseBodyValue
              contextualizedResponseLogEntries =
                case response of
                  PageResponse _ -> []
                  PageResponseWithMetadata pageResponseBodyValue _ ->
                    map
                      (prependRequestLogContext requestLogFields)
                      (responseLogEntries pageResponseBodyValue)
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
                      PageResponseWithMetadata pageResponseBodyValue _ ->
                        responseStatus pageResponseBodyValue
                      BodyResponse responseBodyValue -> responseStatus responseBodyValue
                  )
                  ( case response of
                      PageResponse _ -> Observability.PageResponseKind
                      PageResponseWithMetadata _ _ -> Observability.PageResponseKind
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
      challengeStore <- AcmeChallengeStore <$> newMVar []
      let runtimeApplication = toRuntimeWaiApplication challengeStore webApplication
      case runtimeStartupValidationError startupPlan of
        Just runtimeError ->
          ioError (userError runtimeError)
        Nothing ->
          observabilityPlan `seq`
            bracket
              (startHttpRuntimeServers (httpEndpoints (httpBindPlan startupPlan)) runtimeApplication)
              stopRuntimeServers
              ( \httpServers ->
                  bracket
                    (startAcmeRuntimeServers (runtimeAcmeBindPlans startupPlan) runtimeApplication challengeStore)
                    stopAcmeRuntimeServers
                    ( \acmeServers ->
                        bracket
                          (startManualTlsRuntimeServers (manualTlsBindPlans startupPlan) runtimeApplication)
                          stopRuntimeServers
                          ( \manualTlsServers ->
                              httpServers `seq`
                                acmeServers `seq`
                                  manualTlsServers `seq`
                                    announceRuntimeStartup outputHandle startupPlan
                                      >> waitForShutdownSignal
                          )
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

data RunningAcmeRuntimeServer = RunningAcmeRuntimeServer
  { runningAcmeRuntimeServer :: RunningRuntimeServer,
    runningAcmeCleanupDirectory :: FilePath
  }

data TlsCredentialSnapshot = TlsCredentialSnapshot
  { tlsCredentialModifiedTimes :: (UTCTime, UTCTime),
    tlsCredentialValues :: TLS.Credentials
  }

data ReloadingTlsCredentials = ReloadingTlsCredentials
  { tlsCredentialCertificatePath :: FilePath,
    tlsCredentialPrivateKeyPath :: FilePath,
    tlsCredentialSnapshotReference :: IORef TlsCredentialSnapshot
  }

data RuntimeAcmeBindPlan = RuntimeAcmeBindPlan
  { runtimeAcmeEndpoint :: ListenerEndpoint,
    runtimeAcmeListenerConfig :: AcmeConfig
  }

runtimeAcmeBindPlans :: ServerStartupPlan -> [RuntimeAcmeBindPlan]
runtimeAcmeBindPlans startupPlan =
  [ RuntimeAcmeBindPlan
      { runtimeAcmeEndpoint = acmeEndpoint acmePlan,
        runtimeAcmeListenerConfig = acmeListenerConfig acmePlan
      }
  | acmePlan <- acmeBindPlans startupPlan
  ]

data ActiveAcmeChallenge = ActiveAcmeChallenge
  { activeAcmeChallengeDomain :: Text,
    activeAcmeChallengeToken :: Text,
    activeAcmeChallengeResponse :: Text
  }

newtype AcmeChallengeStore = AcmeChallengeStore (MVar [ActiveAcmeChallenge])

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

startAcmeRuntimeServers :: [RuntimeAcmeBindPlan] -> Wai.Application -> AcmeChallengeStore -> IO [RunningAcmeRuntimeServer]
startAcmeRuntimeServers acmePlans waiApplication challengeStore =
  go [] acmePlans
  where
    go runningServers remainingPlans =
      case remainingPlans of
        [] -> pure (reverse runningServers)
        acmePlan : remaining ->
          ( do
              runningServer <- startAcmeRuntimeServer acmePlan waiApplication challengeStore
              go (runningServer : runningServers) remaining
                `onException` stopAcmeRuntimeServers (runningServer : runningServers)
          )
            `onException` stopAcmeRuntimeServers runningServers

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
startManualTlsRuntimeServer =
  startManualTlsRuntimeServerWithStarter startWarpTlsServerOnSocket

startManualTlsRuntimeServerWithStarter :: (Int -> WarpTLS.TLSSettings -> Socket.Socket -> Wai.Application -> IO ThreadId) -> ManualTlsBindPlan -> Wai.Application -> IO RunningRuntimeServer
startManualTlsRuntimeServerWithStarter startTlsServer manualTlsPlan waiApplication = do
  let tlsLabel =
        case tlsCredentialSourceKind manualTlsPlan of
          ManualTlsCredentials -> "Manual TLS"
          SharedTlsCredentials -> "Shared TLS"
  reloadingTlsCredentials <-
    case tlsStartupMode manualTlsPlan of
      RequireCertificateFiles ->
        loadReloadingTlsCredentialsWithLabel
          tlsLabel
          (tlsCertificateFile manualTlsPlan)
          (tlsPrivateKeyFile manualTlsPlan)
      AwaitCertificateFiles waitTimeoutSeconds ->
        awaitReloadingTlsCredentials
          waitTimeoutSeconds
          (tlsCertificateFile manualTlsPlan)
          (tlsPrivateKeyFile manualTlsPlan)
  initialTlsCredentials <- reloadTlsCredentialsIfChanged reloadingTlsCredentials
  let endpoint = tlsEndpoint manualTlsPlan
      baseTlsSettings =
        WarpTLS.tlsSettings
          (tlsCertificateFile manualTlsPlan)
          (tlsPrivateKeyFile manualTlsPlan)
      tlsSettings =
        baseTlsSettings
          { WarpTLS.tlsCredentials = Just initialTlsCredentials,
            WarpTLS.tlsServerHooks =
              (WarpTLS.tlsServerHooks baseTlsSettings)
                { TLS.onServerNameIndication = const (reloadTlsCredentialsIfChanged reloadingTlsCredentials)
                }
          }
  listeningSocket <- openListenerSocket endpoint
  serverThreadId <-
    startTlsServer (endpointPort endpoint) tlsSettings listeningSocket waiApplication
      `onException` Socket.close listeningSocket
  manualTlsPlan `seq`
    pure
      RunningRuntimeServer
        { runningRuntimeSocket = listeningSocket,
          runningRuntimeThreadId = serverThreadId
        }

startAcmeRuntimeServer :: RuntimeAcmeBindPlan -> Wai.Application -> AcmeChallengeStore -> IO RunningAcmeRuntimeServer
startAcmeRuntimeServer runtimeAcmePlan waiApplication challengeStore = do
  (manualTlsPlan, cleanupDirectory) <-
    case acmeChallengeBackend (runtimeAcmeListenerConfig runtimeAcmePlan) of
      CertbotHttp01 certbotConfig ->
        prepareCertbotManualTlsBindPlan runtimeAcmePlan certbotConfig
      InProcessHttp01 ->
        prepareInProcessManualTlsBindPlan runtimeAcmePlan challengeStore
  runningServer <-
    startManualTlsRuntimeServer manualTlsPlan waiApplication
      `onException` removePathForcibly cleanupDirectory
  pure
    RunningAcmeRuntimeServer
      { runningAcmeRuntimeServer = runningServer,
        runningAcmeCleanupDirectory = cleanupDirectory
      }

prepareCertbotManualTlsBindPlan :: RuntimeAcmeBindPlan -> CertbotConfig -> IO (ManualTlsBindPlan, FilePath)
prepareCertbotManualTlsBindPlan runtimeAcmePlan certbotConfig = do
  tempDirectory <- getCanonicalTemporaryDirectory
  bracketOnError
    (createTempDirectory tempDirectory "harch-web-certbot")
    removePathForcibly
    $ \stateDirectory -> do
      let configDirectory = stateDirectory </> "config"
          workDirectory = stateDirectory </> "work"
          logsDirectory = stateDirectory </> "logs"
      mapM_ (createDirectoryIfMissing True) [configDirectory, workDirectory, logsDirectory]
      certificateName <-
        either
          (ioError . userError)
          pure
          (certbotCertificateName runtimeAcmePlan)
      runCertbotAcmeChallenge runtimeAcmePlan certbotConfig configDirectory workDirectory logsDirectory
      let certificateDirectory = configDirectory </> "live" </> Text.unpack certificateName
          certificatePath = certificateDirectory </> "fullchain.pem"
          privateKeyPath = certificateDirectory </> "privkey.pem"
      ensureRuntimeFileExists "Certbot ACME certificate file does not exist: " certificatePath
      ensureRuntimeFileExists "Certbot ACME private key file does not exist: " privateKeyPath
      (resolvedCertificatePath, resolvedPrivateKeyPath) <-
        case acmeCertificateDirectory (runtimeAcmeListenerConfig runtimeAcmePlan) of
          Nothing ->
            pure (certificatePath, privateKeyPath)
          Just sharedDirectory ->
            publishCertificateFiles sharedDirectory certificatePath privateKeyPath
      pure
        ( ManualTlsBindPlan
            { tlsEndpoint = runtimeAcmeEndpoint runtimeAcmePlan,
              tlsCertificateFile = resolvedCertificatePath,
              tlsPrivateKeyFile = resolvedPrivateKeyPath,
              tlsCredentialSourceKind = ManualTlsCredentials,
              tlsStartupMode = RequireCertificateFiles
            },
          stateDirectory
        )

runCertbotAcmeChallenge :: RuntimeAcmeBindPlan -> CertbotConfig -> FilePath -> FilePath -> FilePath -> IO ()
runCertbotAcmeChallenge runtimeAcmePlan certbotConfig configDirectory workDirectory logsDirectory = do
  let commandArguments =
        certbotRuntimeArguments runtimeAcmePlan certbotConfig configDirectory workDirectory logsDirectory
  processResult <-
    try (readCreateProcessWithExitCode (proc (certbotExecutable certbotConfig) commandArguments) "") ::
      IO (Either IOException (ExitCode, String, String))
  case processResult of
    Left launchError ->
      ioError . userError $
        "Failed to launch certbot for ACME listener on "
          <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
          <> ": "
          <> show launchError
    Right (ExitSuccess, stdoutText, stderrText) -> do
      void (evaluate (length stdoutText + length stderrText))
    Right (exitCode, stdoutText, stderrText) ->
      ioError . userError $
        "Certbot failed for ACME listener on "
          <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
          <> " with exit code "
          <> show exitCode
          <> ".\nstdout:\n"
          <> stdoutText
          <> "\nstderr:\n"
          <> stderrText

certbotRuntimeArguments :: RuntimeAcmeBindPlan -> CertbotConfig -> FilePath -> FilePath -> FilePath -> [String]
certbotRuntimeArguments runtimeAcmePlan certbotConfig configDirectory workDirectory logsDirectory =
  map Text.unpack (certbotArguments certbotConfig)
    <> ["--config-dir", configDirectory, "--work-dir", workDirectory, "--logs-dir", logsDirectory]
    <> certbotDirectoryUrlArguments runtimeAcmePlan
    <> certbotContactEmailArguments runtimeAcmePlan certbotConfig
    <> certbotDomainArguments runtimeAcmePlan certbotConfig

certbotDirectoryUrlArguments :: RuntimeAcmeBindPlan -> [String]
certbotDirectoryUrlArguments runtimeAcmePlan =
  if certbotHasOption "--server" (runtimeCertbotArguments runtimeAcmePlan)
    then []
    else ["--server", Text.unpack (acmeDirectoryUrl (runtimeAcmeListenerConfig runtimeAcmePlan))]

certbotContactEmailArguments :: RuntimeAcmeBindPlan -> CertbotConfig -> [String]
certbotContactEmailArguments runtimeAcmePlan certbotConfig =
  if certbotHasOption "--email" (certbotArguments certbotConfig)
    || certbotHasOption "-m" (certbotArguments certbotConfig)
    then []
    else case acmeContactEmails (runtimeAcmeListenerConfig runtimeAcmePlan) of
      firstContact : _ -> ["--email", Text.unpack firstContact]
      [] -> []

certbotDomainArguments :: RuntimeAcmeBindPlan -> CertbotConfig -> [String]
certbotDomainArguments runtimeAcmePlan certbotConfig =
  if any (`certbotHasOption` configuredArguments) ["-d", "--domain", "--domains"]
    then []
    else case acmeDomains (runtimeAcmeListenerConfig runtimeAcmePlan) of
      [] -> []
      domains -> ["--domains", Text.unpack (Text.intercalate "," domains)]
  where
    configuredArguments = certbotArguments certbotConfig

runtimeCertbotArguments :: RuntimeAcmeBindPlan -> [Text]
runtimeCertbotArguments runtimeAcmePlan =
  case acmeChallengeBackend (runtimeAcmeListenerConfig runtimeAcmePlan) of
    CertbotHttp01 certbotConfig -> certbotArguments certbotConfig
    InProcessHttp01 -> []

toRuntimeWaiApplication :: (Eq route) => AcmeChallengeStore -> Application route context -> Wai.Application
toRuntimeWaiApplication challengeStore webApplication request respond = do
  maybeChallengeResponse <- acmeChallengeResponseForRequest challengeStore request
  case maybeChallengeResponse of
    Just challengeResponse -> respond challengeResponse
    Nothing -> toWaiApplication webApplication request respond

acmeChallengeResponseForRequest :: AcmeChallengeStore -> Wai.Request -> IO (Maybe Wai.Response)
acmeChallengeResponseForRequest (AcmeChallengeStore challengeStore) request = do
  challenges <- readMVar challengeStore
  pure $
    fmap
      ( Wai.responseLBS
          Http.ok200
          [("Content-Type", "text/plain; charset=utf-8")]
          . LazyByteString.fromStrict
          . TextEncoding.encodeUtf8
          . activeAcmeChallengeResponse
      )
      (find (matchesRuntimeAcmeChallenge request) challenges)

matchesRuntimeAcmeChallenge :: Wai.Request -> ActiveAcmeChallenge -> Bool
matchesRuntimeAcmeChallenge request challenge =
  case acmeHttp01ChallengeToken request of
    Just challengeToken ->
      challengeToken == activeAcmeChallengeToken challenge
        && maybe True (== activeAcmeChallengeDomain challenge) (requestHostWithoutPort request)
    Nothing -> False

acmeHttp01ChallengeToken :: Wai.Request -> Maybe Text
acmeHttp01ChallengeToken request =
  Text.stripPrefix "/.well-known/acme-challenge/" (waiRequestPath request)

requestHostWithoutPort :: Wai.Request -> Maybe Text
requestHostWithoutPort request =
  fmap (Text.takeWhile (/= ':')) (requestHeaderToken "Host" request)

registerAcmeChallenges :: AcmeChallengeStore -> [ActiveAcmeChallenge] -> IO ()
registerAcmeChallenges (AcmeChallengeStore challengeStore) newChallenges =
  modifyMVar_ challengeStore (pure . (newChallenges <>))

unregisterAcmeChallenges :: AcmeChallengeStore -> [ActiveAcmeChallenge] -> IO ()
unregisterAcmeChallenges (AcmeChallengeStore challengeStore) completedChallenges =
  modifyMVar_ challengeStore (pure . filter (not . (`sameActiveAcmeChallengeAny` completedChallenges)))

sameActiveAcmeChallengeAny :: ActiveAcmeChallenge -> [ActiveAcmeChallenge] -> Bool
sameActiveAcmeChallengeAny candidate =
  any (sameActiveAcmeChallenge candidate)

sameActiveAcmeChallenge :: ActiveAcmeChallenge -> ActiveAcmeChallenge -> Bool
sameActiveAcmeChallenge left right =
  activeAcmeChallengeDomain left == activeAcmeChallengeDomain right
    && activeAcmeChallengeToken left == activeAcmeChallengeToken right
    && activeAcmeChallengeResponse left == activeAcmeChallengeResponse right

certbotCertificateName :: RuntimeAcmeBindPlan -> Either String Text
certbotCertificateName runtimeAcmePlan =
  maybe
    ( maybe
        ( Left $
            "Unsupported runtime listener startup plan: ACME listener on "
              <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
              <> " requires ACME domains or certbot arguments to declare --cert-name or a domain via -d/--domain/--domains."
        )
        Right
        ( firstCertbotDomain (runtimeCertbotArguments runtimeAcmePlan)
            <|> listToMaybe (acmeDomains (runtimeAcmeListenerConfig runtimeAcmePlan))
        )
    )
    Right
    (listToMaybe (certbotOptionValues "--cert-name" (runtimeCertbotArguments runtimeAcmePlan)))

firstCertbotDomain :: [Text] -> Maybe Text
firstCertbotDomain arguments =
  listToMaybe . concatMap splitCertbotDomainValue $
    certbotOptionValues "-d" arguments
      <> certbotOptionValues "--domain" arguments
      <> certbotOptionValues "--domains" arguments

splitCertbotDomainValue :: Text -> [Text]
splitCertbotDomainValue =
  filter (not . Text.null) . map Text.strip . Text.splitOn ","

certbotOptionValues :: Text -> [Text] -> [Text]
certbotOptionValues optionName arguments =
  [ optionValue
  | (argument, optionValue) <- zip arguments (drop 1 arguments),
    argument == optionName
  ]
    <> [ optionValue
       | argument <- arguments,
         Just optionValue <- [Text.stripPrefix (optionName <> "=") argument]
       ]

certbotHasOption :: Text -> [Text] -> Bool
certbotHasOption optionName =
  not . null . certbotOptionValues optionName

data AcmeDirectoryResponse = AcmeDirectoryResponse
  { acmeNewNonceUrl :: Text,
    acmeNewAccountUrl :: Text,
    acmeNewOrderUrl :: Text
  }

data AcmeOrderIdentifier = AcmeOrderIdentifier
  { acmeIdentifierKind :: Text,
    acmeIdentifierValue :: Text
  }

data AcmeChallengeResponse = AcmeChallengeResponse
  { acmeChallengeKind :: Text,
    acmeChallengeUrl :: Text,
    acmeChallengeTokenValue :: Text
  }

data AcmeAuthorizationResponse = AcmeAuthorizationResponse
  { acmeAuthorizationIdentifier :: AcmeOrderIdentifier,
    acmeAuthorizationChallenges :: [AcmeChallengeResponse]
  }

data AcmeOrderResponse = AcmeOrderResponse
  { acmeOrderStatus :: Text,
    acmeOrderAuthorizations :: Maybe [Text],
    acmeOrderFinalizeUrl :: Maybe Text,
    acmeOrderCertificateUrl :: Maybe Text
  }

data AcmeJwk = AcmeJwk
  { acmeJwkExponent :: Text,
    acmeJwkModulus :: Text
  }

data AcmeRequestAuth
  = AcmeRequestJwk AcmeJwk
  | AcmeRequestKid Text

data PreparedAcmeChallenge = PreparedAcmeChallenge
  { preparedAcmeChallengeRegistration :: ActiveAcmeChallenge,
    preparedAcmeChallengeUrl :: Text
  }

data JsonValue
  = JsonObject [(Text, JsonValue)]
  | JsonArray [JsonValue]
  | JsonString Text
  | JsonBool Bool
  | JsonNull

parseAcmeDirectoryResponse :: JsonValue -> Either String AcmeDirectoryResponse
parseAcmeDirectoryResponse value = do
  fields <- jsonObjectFields "AcmeDirectoryResponse" value
  AcmeDirectoryResponse
    <$> jsonRequiredTextField "newNonce" fields
    <*> jsonRequiredTextField "newAccount" fields
    <*> jsonRequiredTextField "newOrder" fields

parseAcmeOrderIdentifier :: JsonValue -> Either String AcmeOrderIdentifier
parseAcmeOrderIdentifier value = do
  fields <- jsonObjectFields "AcmeOrderIdentifier" value
  AcmeOrderIdentifier
    <$> jsonRequiredTextField "type" fields
    <*> jsonRequiredTextField "value" fields

parseAcmeChallengeResponse :: JsonValue -> Either String AcmeChallengeResponse
parseAcmeChallengeResponse value = do
  fields <- jsonObjectFields "AcmeChallengeResponse" value
  AcmeChallengeResponse
    <$> jsonRequiredTextField "type" fields
    <*> jsonRequiredTextField "url" fields
    <*> jsonRequiredTextField "token" fields

parseAcmeAuthorizationResponse :: JsonValue -> Either String AcmeAuthorizationResponse
parseAcmeAuthorizationResponse value = do
  fields <- jsonObjectFields "AcmeAuthorizationResponse" value
  AcmeAuthorizationResponse
    <$> (jsonRequiredField "identifier" fields >>= parseAcmeOrderIdentifier)
    <*> (jsonRequiredField "challenges" fields >>= jsonArrayItems "challenges" >>= traverse parseAcmeChallengeResponse)

parseAcmeOrderResponse :: JsonValue -> Either String AcmeOrderResponse
parseAcmeOrderResponse value = do
  fields <- jsonObjectFields "AcmeOrderResponse" value
  AcmeOrderResponse
    <$> jsonRequiredTextField "status" fields
    <*> jsonOptionalTextArrayField "authorizations" fields
    <*> jsonOptionalTextField "finalize" fields
    <*> jsonOptionalTextField "certificate" fields

jsonObjectFields :: String -> JsonValue -> Either String [(Text, JsonValue)]
jsonObjectFields label value =
  case value of
    JsonObject fields -> Right fields
    _ -> Left (label <> " was not a JSON object")

jsonArrayItems :: String -> JsonValue -> Either String [JsonValue]
jsonArrayItems label value =
  case value of
    JsonArray items -> Right items
    _ -> Left (label <> " was not a JSON array")

jsonRequiredField :: Text -> [(Text, JsonValue)] -> Either String JsonValue
jsonRequiredField fieldName fields =
  maybe
    (Left ("missing required field " <> Text.unpack fieldName))
    Right
    (lookup fieldName fields)

jsonRequiredTextField :: Text -> [(Text, JsonValue)] -> Either String Text
jsonRequiredTextField fieldName fields =
  jsonTextField fieldName =<< jsonRequiredField fieldName fields

jsonOptionalTextField :: Text -> [(Text, JsonValue)] -> Either String (Maybe Text)
jsonOptionalTextField !fieldName fields =
  case lookup fieldName fields of
    Nothing -> Right Nothing
    Just JsonNull -> Right Nothing
    Just fieldValue -> Just <$> jsonTextField fieldName fieldValue

jsonOptionalTextArrayField :: Text -> [(Text, JsonValue)] -> Either String (Maybe [Text])
jsonOptionalTextArrayField !fieldName fields =
  case lookup fieldName fields of
    Nothing -> Right Nothing
    Just JsonNull -> Right Nothing
    Just fieldValue -> Just <$> (jsonArrayItems (Text.unpack fieldName) fieldValue >>= traverse (jsonTextField fieldName))

jsonTextField :: Text -> JsonValue -> Either String Text
jsonTextField fieldName fieldValue =
  case fieldValue of
    JsonString fieldText -> Right fieldText
    _ -> Left ("field " <> Text.unpack fieldName <> " was not a JSON string")

parseJsonValue :: LazyByteString.ByteString -> Either String JsonValue
parseJsonValue inputBytes =
  case [parsedValue | (parsedValue, _remainingInput) <- readP_to_S (jsonValueParser <* skipSpaces <* eof) inputText] of
    parsedValue : _ -> Right parsedValue
    [] -> Left "invalid JSON"
  where
    inputText = Text.unpack (TextEncoding.decodeUtf8 (LazyByteString.toStrict inputBytes))

jsonValueParser :: ReadP JsonValue
jsonValueParser =
  skipSpaces
    *> choice
      [ JsonObject <$> jsonObjectParser,
        JsonArray <$> jsonArrayParser,
        JsonString . Text.pack <$> jsonStringParser,
        JsonBool True <$ string "true",
        JsonBool False <$ string "false",
        JsonNull <$ string "null"
      ]
    <* skipSpaces

jsonObjectParser :: ReadP [(Text, JsonValue)]
jsonObjectParser = do
  _ <- char '{'
  skipSpaces
  (char '}' $> [])
    <++ do
      fields <- sepBy jsonObjectEntryParser (skipSpaces *> char ',' <* skipSpaces)
      skipSpaces
      _ <- char '}'
      pure fields

jsonObjectEntryParser :: ReadP (Text, JsonValue)
jsonObjectEntryParser = do
  fieldName <- Text.pack <$> jsonStringParser
  skipSpaces
  _ <- char ':'
  fieldValue <- jsonValueParser
  pure (fieldName, fieldValue)

jsonArrayParser :: ReadP [JsonValue]
jsonArrayParser = do
  _ <- char '['
  skipSpaces
  (char ']' $> [])
    <++ do
      items <- sepBy jsonValueParser (skipSpaces *> char ',' <* skipSpaces)
      skipSpaces
      _ <- char ']'
      pure items

jsonStringParser :: ReadP String
jsonStringParser =
  char '"' *> manyTill jsonStringCharacterParser (char '"')

jsonStringCharacterParser :: ReadP Char
jsonStringCharacterParser = do
  nextCharacter <- get
  if nextCharacter == '\\'
    then escapedJsonCharacterParser
    else pure nextCharacter

escapedJsonCharacterParser :: ReadP Char
escapedJsonCharacterParser = do
  choice
    [ '"' <$ char '"',
      '\\' <$ char '\\',
      '/' <$ char '/',
      '\b' <$ char 'b',
      '\f' <$ char 'f',
      '\n' <$ char 'n',
      '\r' <$ char 'r',
      '\t' <$ char 't',
      unicodeJsonCharacterParser
    ]

unicodeJsonCharacterParser :: ReadP Char
unicodeJsonCharacterParser = do
  _ <- char 'u'
  hexDigits <- replicateM 4 get
  maybe pfail (pure . toEnum) (readMaybe ("0x" <> hexDigits))

jsonStringBytes :: Text -> LazyByteString.ByteString
jsonStringBytes textValue =
  LazyByteString.fromStrict . TextEncoding.encodeUtf8 $
    "\""
      <> Text.concatMap escapeJsonCharacter textValue
      <> "\""

escapeJsonCharacter :: Char -> Text
escapeJsonCharacter character =
  case character of
    '"' -> "\\\""
    '\\' -> "\\\\"
    '\b' -> "\\b"
    '\f' -> "\\f"
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    _ -> Text.singleton character

jsonBoolBytes :: Bool -> LazyByteString.ByteString
jsonBoolBytes boolValue =
  if boolValue
    then "true"
    else "false"

jsonArrayBytes :: [LazyByteString.ByteString] -> LazyByteString.ByteString
jsonArrayBytes items =
  "[" <> LazyByteString.intercalate "," items <> "]"

jsonObjectBytes :: [(Text, LazyByteString.ByteString)] -> LazyByteString.ByteString
jsonObjectBytes fields =
  "{"
    <> LazyByteString.intercalate
      ","
      [ jsonStringBytes fieldName <> ":" <> fieldValue
      | (fieldName, fieldValue) <- fields
      ]
    <> "}"

prepareInProcessManualTlsBindPlan :: RuntimeAcmeBindPlan -> AcmeChallengeStore -> IO (ManualTlsBindPlan, FilePath)
prepareInProcessManualTlsBindPlan !runtimeAcmePlan challengeStore = do
  tempDirectory <- getCanonicalTemporaryDirectory
  bracketOnError
    (createTempDirectory tempDirectory "harch-web-acme")
    removePathForcibly
    $ \stateDirectory -> do
      let privateKeyPath = stateDirectory </> "privkey.pem"
          certificatePath = stateDirectory </> "fullchain.pem"
      runInProcessAcmeChallenge runtimeAcmePlan challengeStore stateDirectory certificatePath privateKeyPath
      (resolvedCertificatePath, resolvedPrivateKeyPath) <-
        case acmeCertificateDirectory (runtimeAcmeListenerConfig runtimeAcmePlan) of
          Nothing ->
            pure (certificatePath, privateKeyPath)
          Just sharedDirectory ->
            publishCertificateFiles sharedDirectory certificatePath privateKeyPath
      pure
        ( ManualTlsBindPlan
            { tlsEndpoint = runtimeAcmeEndpoint runtimeAcmePlan,
              tlsCertificateFile = resolvedCertificatePath,
              tlsPrivateKeyFile = resolvedPrivateKeyPath,
              tlsCredentialSourceKind = ManualTlsCredentials,
              tlsStartupMode = RequireCertificateFiles
            },
          stateDirectory
        )

runInProcessAcmeChallenge :: RuntimeAcmeBindPlan -> AcmeChallengeStore -> FilePath -> FilePath -> FilePath -> IO ()
runInProcessAcmeChallenge !runtimeAcmePlan challengeStore stateDirectory certificatePath privateKeyPath = do
  let domains = acmeDomains (runtimeAcmeListenerConfig runtimeAcmePlan)
  when
    (null domains)
    ( ioError . userError $
        "Unsupported runtime listener startup plan: ACME listener on "
          <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
          <> " requires ACME domains for in-process http-01 runtime startup."
    )
  let accountKeyPath = stateDirectory </> "account-key.pem"
      csrConfigPath = stateDirectory </> "csr.cnf"
      csrPemPath = stateDirectory </> "request.csr"
      csrDerPath = stateDirectory </> "request.der"
  generateAcmeAccountKey runtimeAcmePlan accountKeyPath
  accountJwk <- loadAcmeJwk runtimeAcmePlan accountKeyPath
  generateAcmeCertificateRequest runtimeAcmePlan domains privateKeyPath csrConfigPath csrPemPath csrDerPath
  ensureRuntimeFileExists "In-process ACME private key file does not exist: " privateKeyPath
  csrDerBytes <- ByteString.readFile csrDerPath
  manager <- HttpClient.newManager HttpClientTls.tlsManagerSettings
  directory <- fetchAcmeDirectory runtimeAcmePlan manager
  accountKid <-
    createAcmeAccount
      runtimeAcmePlan
      manager
      directory
      accountKeyPath
      accountJwk
      (map mailtoAcmeContact (acmeContactEmails (runtimeAcmeListenerConfig runtimeAcmePlan)))
  (orderUrl, createdOrder) <-
    createAcmeOrder
      runtimeAcmePlan
      manager
      directory
      accountKeyPath
      accountKid
      domains
  authorizationUrls <-
    maybe
      ( ioError . userError $
          "In-process ACME new-order response for listener on "
            <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
            <> " did not include authorization URLs."
      )
      pure
      (acmeOrderAuthorizations createdOrder)
  preparedChallenges <-
    mapM
      (prepareAcmeAuthorization runtimeAcmePlan manager directory accountKeyPath accountKid accountJwk)
      authorizationUrls
  let activeChallenges = map preparedAcmeChallengeRegistration preparedChallenges
  bracket_
    (registerAcmeChallenges challengeStore activeChallenges)
    (unregisterAcmeChallenges challengeStore activeChallenges)
    $ do
      mapM_
        (triggerAcmeChallenge runtimeAcmePlan manager directory accountKeyPath accountKid . preparedAcmeChallengeUrl)
        preparedChallenges
      readyOrder <-
        pollAcmeOrder
          runtimeAcmePlan
          manager
          directory
          accountKeyPath
          accountKid
          orderUrl
          ["ready", "valid"]
      finalizedOrder <-
        if acmeOrderStatus readyOrder == "valid"
          then pure readyOrder
          else do
            finalizeUrl <-
              maybe
                ( ioError . userError $
                    "In-process ACME ready order for listener on "
                      <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
                      <> " did not include a finalize URL."
                )
                pure
                (acmeOrderFinalizeUrl readyOrder)
            finalizeAcmeOrder runtimeAcmePlan manager directory accountKeyPath accountKid finalizeUrl csrDerBytes
            pollAcmeOrder runtimeAcmePlan manager directory accountKeyPath accountKid orderUrl ["valid"]
      certificateUrl <-
        maybe
          ( ioError . userError $
              "In-process ACME valid order for listener on "
                <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
                <> " did not include a certificate URL."
          )
          pure
          (acmeOrderCertificateUrl finalizedOrder)
      certificatePem <- fetchAcmeCertificate runtimeAcmePlan manager directory accountKeyPath accountKid certificateUrl
      LazyByteString.writeFile certificatePath certificatePem

generateAcmeAccountKey :: RuntimeAcmeBindPlan -> FilePath -> IO ()
generateAcmeAccountKey !runtimeAcmePlan accountKeyPath =
  runOpenSslCommand runtimeAcmePlan ["genrsa", "-out", accountKeyPath, "4096"]

generateAcmeCertificateRequest :: RuntimeAcmeBindPlan -> [Text] -> FilePath -> FilePath -> FilePath -> FilePath -> IO ()
generateAcmeCertificateRequest !runtimeAcmePlan domains privateKeyPath csrConfigPath csrPemPath csrDerPath = do
  writeFile csrConfigPath (acmeCertificateRequestConfig domains)
  runOpenSslCommand
    runtimeAcmePlan
    [ "req",
      "-new",
      "-newkey",
      "rsa:2048",
      "-nodes",
      "-keyout",
      privateKeyPath,
      "-out",
      csrPemPath,
      "-config",
      csrConfigPath
    ]
  runOpenSslCommand
    runtimeAcmePlan
    ["req", "-in", csrPemPath, "-outform", "DER", "-out", csrDerPath]

acmeCertificateRequestConfig :: [Text] -> String
acmeCertificateRequestConfig domains =
  unlines
    [ "[req]",
      "distinguished_name = req_distinguished_name",
      "prompt = no",
      "req_extensions = req_ext",
      "",
      "[req_distinguished_name]",
      "CN = " <> Text.unpack firstDomain,
      "",
      "[req_ext]",
      "subjectAltName = " <> intercalate "," (map (("DNS:" <>) . Text.unpack) domains)
    ]
  where
    firstDomain =
      case domains of
        domain : _ -> domain
        [] -> "localhost"

loadAcmeJwk :: RuntimeAcmeBindPlan -> FilePath -> IO AcmeJwk
loadAcmeJwk !runtimeAcmePlan accountKeyPath = do
  modulusOutput <- runOpenSslTextCommand runtimeAcmePlan ["rsa", "-in", accountKeyPath, "-modulus", "-noout"]
  modulusText <-
    maybe
      ( ioError . userError $
          "OpenSSL did not return an RSA modulus for ACME listener on "
            <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
      )
      pure
      (Text.stripPrefix "Modulus=" (Text.strip (Text.pack modulusOutput)))
  modulusBytes <-
    either
      ( \decodeError ->
          ioError . userError $
            "OpenSSL returned an invalid RSA modulus for ACME listener on "
              <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
              <> ": "
              <> decodeError
      )
      pure
      (hexTextToByteString modulusText)
  pure
    AcmeJwk
      { acmeJwkExponent = "AQAB",
        acmeJwkModulus = base64urlText modulusBytes
      }

fetchAcmeDirectory :: RuntimeAcmeBindPlan -> HttpClient.Manager -> IO AcmeDirectoryResponse
fetchAcmeDirectory !runtimeAcmePlan manager = do
  request <- HttpClient.parseRequest (Text.unpack (acmeDirectoryUrl (runtimeAcmeListenerConfig runtimeAcmePlan)))
  response <- performAcmeRequest runtimeAcmePlan manager "directory fetch" request [200]
  decodeAcmeJsonResponse runtimeAcmePlan "directory fetch" parseAcmeDirectoryResponse response

createAcmeAccount :: RuntimeAcmeBindPlan -> HttpClient.Manager -> AcmeDirectoryResponse -> FilePath -> AcmeJwk -> [Text] -> IO Text
createAcmeAccount !runtimeAcmePlan manager directory accountKeyPath accountJwk contacts = do
  response <-
    performAcmeJwsRequest
      runtimeAcmePlan
      manager
      directory
      accountKeyPath
      "account creation"
      (AcmeRequestJwk accountJwk)
      (acmeNewAccountUrl directory)
      ( jsonObjectBytes
          [ ("termsOfServiceAgreed", jsonBoolBytes True),
            ("contact", jsonArrayBytes (map jsonStringBytes contacts))
          ]
      )
      Nothing
      [200, 201]
  maybe
    ( ioError . userError $
        "ACME account creation for listener on "
          <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
          <> " did not return an account location header."
    )
    pure
    (responseHeaderText "Location" response)

createAcmeOrder :: RuntimeAcmeBindPlan -> HttpClient.Manager -> AcmeDirectoryResponse -> FilePath -> Text -> [Text] -> IO (Text, AcmeOrderResponse)
createAcmeOrder !runtimeAcmePlan manager directory accountKeyPath accountKid domains = do
  response <-
    performAcmeJwsRequest
      runtimeAcmePlan
      manager
      directory
      accountKeyPath
      "new order"
      (AcmeRequestKid accountKid)
      (acmeNewOrderUrl directory)
      ( jsonObjectBytes
          [ ( "identifiers",
              jsonArrayBytes
                [ jsonObjectBytes
                    [ ("type", jsonStringBytes "dns"),
                      ("value", jsonStringBytes domain)
                    ]
                | domain <- domains
                ]
            )
          ]
      )
      Nothing
      [200, 201]
  orderUrl <-
    maybe
      ( ioError . userError $
          "ACME new-order response for listener on "
            <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
            <> " did not return an order location header."
      )
      pure
      (responseHeaderText "Location" response)
  createdOrder <- decodeAcmeJsonResponse runtimeAcmePlan "new order" parseAcmeOrderResponse response
  pure (orderUrl, createdOrder)

prepareAcmeAuthorization :: RuntimeAcmeBindPlan -> HttpClient.Manager -> AcmeDirectoryResponse -> FilePath -> Text -> AcmeJwk -> Text -> IO PreparedAcmeChallenge
prepareAcmeAuthorization !runtimeAcmePlan manager directory accountKeyPath accountKid accountJwk authorizationUrl = do
  response <-
    performAcmeJwsRequest
      runtimeAcmePlan
      manager
      directory
      accountKeyPath
      "authorization fetch"
      (AcmeRequestKid accountKid)
      authorizationUrl
      LazyByteString.empty
      Nothing
      [200]
  authorization <- decodeAcmeJsonResponse runtimeAcmePlan "authorization fetch" parseAcmeAuthorizationResponse response
  challenge <-
    maybe
      ( ioError . userError $
          "ACME authorization for listener on "
            <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
            <> " did not provide an http-01 challenge."
      )
      pure
      (find ((== "http-01") . acmeChallengeKind) (acmeAuthorizationChallenges authorization))
  keyAuthorization <- buildAcmeKeyAuthorization runtimeAcmePlan accountJwk (acmeChallengeTokenValue challenge)
  pure
    PreparedAcmeChallenge
      { preparedAcmeChallengeRegistration =
          ActiveAcmeChallenge
            { activeAcmeChallengeDomain = acmeIdentifierValue (acmeAuthorizationIdentifier authorization),
              activeAcmeChallengeToken = acmeChallengeTokenValue challenge,
              activeAcmeChallengeResponse = keyAuthorization
            },
        preparedAcmeChallengeUrl = acmeChallengeUrl challenge
      }

triggerAcmeChallenge :: RuntimeAcmeBindPlan -> HttpClient.Manager -> AcmeDirectoryResponse -> FilePath -> Text -> Text -> IO ()
triggerAcmeChallenge !runtimeAcmePlan manager directory accountKeyPath accountKid challengeUrl =
  void
    ( performAcmeJwsRequest
        runtimeAcmePlan
        manager
        directory
        accountKeyPath
        "challenge acknowledgement"
        (AcmeRequestKid accountKid)
        challengeUrl
        (jsonObjectBytes [])
        Nothing
        [200]
    )

pollAcmeOrder :: RuntimeAcmeBindPlan -> HttpClient.Manager -> AcmeDirectoryResponse -> FilePath -> Text -> Text -> [Text] -> IO AcmeOrderResponse
pollAcmeOrder !runtimeAcmePlan =
  pollAcmeOrderWithRetries 60 1000000 runtimeAcmePlan

pollAcmeOrderWithRetries ::
  Int ->
  Int ->
  RuntimeAcmeBindPlan ->
  HttpClient.Manager ->
  AcmeDirectoryResponse ->
  FilePath ->
  Text ->
  Text ->
  [Text] ->
  IO AcmeOrderResponse
pollAcmeOrderWithRetries !maxAttempts !retryDelayMicros !runtimeAcmePlan manager directory accountKeyPath accountKid orderUrl wantedStatuses =
  go maxAttempts
  where
    go !remainingAttempts = do
      response <-
        performAcmeJwsRequest
          runtimeAcmePlan
          manager
          directory
          accountKeyPath
          "order fetch"
          (AcmeRequestKid accountKid)
          orderUrl
          LazyByteString.empty
          Nothing
          [200]
      order <- decodeAcmeJsonResponse runtimeAcmePlan "order fetch" parseAcmeOrderResponse response
      if acmeOrderStatus order `elem` wantedStatuses
        then pure order
        else case acmeOrderStatus order of
          "pending"
            | remainingAttempts > 0 -> threadDelay retryDelayMicros >> go (remainingAttempts - 1)
          "processing"
            | remainingAttempts > 0 -> threadDelay retryDelayMicros >> go (remainingAttempts - 1)
          "invalid" ->
            ioError . userError $
              "ACME order for listener on "
                <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
                <> " became invalid."
          statusText ->
            if remainingAttempts > 0
              then threadDelay retryDelayMicros >> go (remainingAttempts - 1)
              else
                ioError . userError $
                  "ACME order for listener on "
                    <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
                    <> " did not reach the expected status. Last status: "
                    <> Text.unpack statusText

finalizeAcmeOrder :: RuntimeAcmeBindPlan -> HttpClient.Manager -> AcmeDirectoryResponse -> FilePath -> Text -> Text -> ByteString.ByteString -> IO ()
finalizeAcmeOrder !runtimeAcmePlan manager directory accountKeyPath accountKid finalizeUrl csrDerBytes =
  void
    ( performAcmeJwsRequest
        runtimeAcmePlan
        manager
        directory
        accountKeyPath
        "order finalization"
        (AcmeRequestKid accountKid)
        finalizeUrl
        (jsonObjectBytes [("csr", jsonStringBytes (base64urlText csrDerBytes))])
        Nothing
        [200]
    )

fetchAcmeCertificate :: RuntimeAcmeBindPlan -> HttpClient.Manager -> AcmeDirectoryResponse -> FilePath -> Text -> Text -> IO LazyByteString.ByteString
fetchAcmeCertificate !runtimeAcmePlan manager directory accountKeyPath accountKid certificateUrl = do
  response <-
    performAcmeJwsRequest
      runtimeAcmePlan
      manager
      directory
      accountKeyPath
      "certificate fetch"
      (AcmeRequestKid accountKid)
      certificateUrl
      LazyByteString.empty
      (Just "application/pem-certificate-chain")
      [200]
  pure (HttpClient.responseBody response)

performAcmeJwsRequest ::
  RuntimeAcmeBindPlan ->
  HttpClient.Manager ->
  AcmeDirectoryResponse ->
  FilePath ->
  String ->
  AcmeRequestAuth ->
  Text ->
  LazyByteString.ByteString ->
  Maybe ByteString.ByteString ->
  [Int] ->
  IO (HttpClient.Response LazyByteString.ByteString)
performAcmeJwsRequest !runtimeAcmePlan manager directory accountKeyPath !actionLabel requestAuth endpointUrl payload maybeAcceptHeader expectedStatusCodes = do
  nonce <- fetchAcmeNonce runtimeAcmePlan manager (acmeNewNonceUrl directory)
  requestBody <- buildAcmeJwsBody runtimeAcmePlan accountKeyPath requestAuth nonce endpointUrl payload
  baseRequest <- HttpClient.parseRequest (Text.unpack endpointUrl)
  let request =
        baseRequest
          { HttpClient.method = "POST",
            HttpClient.requestBody = HttpClient.RequestBodyLBS requestBody,
            HttpClient.requestHeaders =
              [("Content-Type", "application/jose+json")]
                <> maybe [] (\acceptHeader -> [("Accept", acceptHeader)]) maybeAcceptHeader
          }
  performAcmeRequest runtimeAcmePlan manager actionLabel request expectedStatusCodes

fetchAcmeNonce :: RuntimeAcmeBindPlan -> HttpClient.Manager -> Text -> IO Text
fetchAcmeNonce !runtimeAcmePlan manager nonceUrl = do
  request <- HttpClient.parseRequest (Text.unpack nonceUrl)
  response <-
    performAcmeRequest
      runtimeAcmePlan
      manager
      "nonce fetch"
      (request {HttpClient.method = "HEAD"})
      [200, 204]
  maybe
    ( ioError . userError $
        "ACME nonce response for listener on "
          <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
          <> " did not include a replay-nonce header."
    )
    pure
    (responseHeaderText "Replay-Nonce" response)

buildAcmeJwsBody :: RuntimeAcmeBindPlan -> FilePath -> AcmeRequestAuth -> Text -> Text -> LazyByteString.ByteString -> IO LazyByteString.ByteString
buildAcmeJwsBody !runtimeAcmePlan accountKeyPath requestAuth nonce endpointUrl payload = do
  let protectedBytes =
        LazyByteString.toStrict $
          jsonObjectBytes
            ( [ ("alg", jsonStringBytes "RS256"),
                ("nonce", jsonStringBytes nonce),
                ("url", jsonStringBytes endpointUrl)
              ]
                <> case requestAuth of
                  AcmeRequestJwk jwk ->
                    [ ( "jwk",
                        jsonObjectBytes
                          [ ("e", jsonStringBytes (acmeJwkExponent jwk)),
                            ("kty", jsonStringBytes "RSA"),
                            ("n", jsonStringBytes (acmeJwkModulus jwk))
                          ]
                      )
                    ]
                  AcmeRequestKid accountKid ->
                    [("kid", jsonStringBytes accountKid)]
            )
      protectedText = base64urlText protectedBytes
      payloadText = base64urlText (LazyByteString.toStrict payload)
      signingInput =
        LazyByteString.fromStrict
          (TextEncoding.encodeUtf8 protectedText <> "." <> TextEncoding.encodeUtf8 payloadText)
  signatureBytes <- signOpenSslRs256 runtimeAcmePlan accountKeyPath signingInput
  pure $
    jsonObjectBytes
      [ ("protected", jsonStringBytes protectedText),
        ("payload", jsonStringBytes payloadText),
        ("signature", jsonStringBytes (base64urlText signatureBytes))
      ]

performAcmeRequest ::
  RuntimeAcmeBindPlan ->
  HttpClient.Manager ->
  String ->
  HttpClient.Request ->
  [Int] ->
  IO (HttpClient.Response LazyByteString.ByteString)
performAcmeRequest !runtimeAcmePlan manager !actionLabel request expectedStatusCodes = do
  responseResult <- try (HttpClient.httpLbs request manager) :: IO (Either SomeException (HttpClient.Response LazyByteString.ByteString))
  response <-
    either
      ( \requestError ->
          ioError . userError $
            "Failed "
              <> actionLabel
              <> " for ACME listener on "
              <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
              <> ": "
              <> show requestError
      )
      pure
      responseResult
  let statusCode = Http.statusCode (HttpClient.responseStatus response)
  if statusCode `elem` expectedStatusCodes
    then pure response
    else
      ioError . userError $
        "ACME "
          <> actionLabel
          <> " for listener on "
          <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
          <> " failed with status "
          <> show statusCode
          <> ".\nbody:\n"
          <> renderAcmeResponseBody response

decodeAcmeJsonResponse ::
  RuntimeAcmeBindPlan ->
  String ->
  (JsonValue -> Either String a) ->
  HttpClient.Response LazyByteString.ByteString ->
  IO a
decodeAcmeJsonResponse !runtimeAcmePlan !actionLabel decodeJson response =
  either
    ( \decodeError ->
        ioError . userError $
          "Failed to decode ACME "
            <> actionLabel
            <> " response for listener on "
            <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
            <> ": "
            <> decodeError
            <> ".\nbody:\n"
            <> renderAcmeResponseBody response
    )
    pure
    (parseJsonValue (HttpClient.responseBody response) >>= decodeJson)

responseHeaderText :: Http.HeaderName -> HttpClient.Response body -> Maybe Text
responseHeaderText headerName response =
  fmap
    (Text.strip . TextEncoding.decodeUtf8)
    (lookup headerName (HttpClient.responseHeaders response))

renderAcmeResponseBody :: HttpClient.Response LazyByteString.ByteString -> String
renderAcmeResponseBody =
  Text.unpack . TextEncoding.decodeUtf8 . LazyByteString.toStrict . HttpClient.responseBody

buildAcmeKeyAuthorization :: RuntimeAcmeBindPlan -> AcmeJwk -> Text -> IO Text
buildAcmeKeyAuthorization !runtimeAcmePlan accountJwk challengeToken = do
  thumbprintDigest <- openSslSha256 runtimeAcmePlan (LazyByteString.fromStrict (acmeJwkThumbprintBytes accountJwk))
  pure (challengeToken <> "." <> base64urlText thumbprintDigest)

acmeJwkThumbprintBytes :: AcmeJwk -> ByteString.ByteString
acmeJwkThumbprintBytes accountJwk =
  TextEncoding.encodeUtf8 $
    "{\"e\":\""
      <> acmeJwkExponent accountJwk
      <> "\",\"kty\":\"RSA\",\"n\":\""
      <> acmeJwkModulus accountJwk
      <> "\"}"

mailtoAcmeContact :: Text -> Text
mailtoAcmeContact contactAddress =
  if "mailto:" `Text.isPrefixOf` contactAddress
    then contactAddress
    else "mailto:" <> contactAddress

base64urlText :: ByteString.ByteString -> Text
base64urlText =
  TextEncoding.decodeUtf8 . Base64Url.encodeUnpadded

hexTextToByteString :: Text -> Either String ByteString.ByteString
hexTextToByteString hexText =
  if odd (length cleanedHex)
    then Left "hex string had an odd length"
    else ByteString.pack <$> traverse hexPairToWord8 digitPairs
  where
    cleanedHex = filter (not . (`elem` [' ', '\n', '\r', '\t'])) (Text.unpack hexText)
    digitPairs =
      [ (cleanedHex !! pairIndex, cleanedHex !! (pairIndex + 1))
      | pairIndex <- [0, 2 .. length cleanedHex - 2]
      ]
    hexPairToWord8 (firstDigit, secondDigit) =
      if isHexDigitChar firstDigit && isHexDigitChar secondDigit
        then Right (fromIntegral (digitToInt firstDigit * 16 + digitToInt secondDigit))
        else Left ("invalid hex digit pair: " <> [firstDigit, secondDigit])
    isHexDigitChar hexDigit =
      isDigit hexDigit
        || ('a' <= hexDigit && hexDigit <= 'f')
        || ('A' <= hexDigit && hexDigit <= 'F')

runOpenSslTextCommand :: RuntimeAcmeBindPlan -> [String] -> IO String
runOpenSslTextCommand !runtimeAcmePlan arguments = do
  processResult <-
    try (readCreateProcessWithExitCode (proc "openssl" arguments) "") ::
      IO (Either IOException (ExitCode, String, String))
  case processResult of
    Left launchError ->
      ioError . userError $
        "Failed to launch openssl for ACME listener on "
          <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
          <> ": "
          <> show launchError
    Right (ExitSuccess, stdoutText, stderrText) -> do
      void (evaluate (length stderrText))
      pure stdoutText
    Right (exitCode, stdoutText, stderrText) ->
      ioError . userError $
        "OpenSSL failed for ACME listener on "
          <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
          <> " with exit code "
          <> show exitCode
          <> ".\nstdout:\n"
          <> stdoutText
          <> "\nstderr:\n"
          <> stderrText

runOpenSslCommand :: RuntimeAcmeBindPlan -> [String] -> IO ()
runOpenSslCommand !runtimeAcmePlan arguments =
  void (runOpenSslTextCommand runtimeAcmePlan arguments)

signOpenSslRs256 :: RuntimeAcmeBindPlan -> FilePath -> LazyByteString.ByteString -> IO ByteString.ByteString
signOpenSslRs256 !runtimeAcmePlan accountKeyPath signingInput = do
  temporaryDirectory <- getCanonicalTemporaryDirectory
  bracket
    (createTempDirectory temporaryDirectory "harch-web-acme-sign")
    removePathForcibly
    $ \signatureDirectory -> do
      let inputPath = signatureDirectory </> "signing-input.bin"
          outputPath = signatureDirectory </> "signature.bin"
      LazyByteString.writeFile inputPath signingInput
      runOpenSslCommand runtimeAcmePlan ["dgst", "-sha256", "-binary", "-sign", accountKeyPath, "-out", outputPath, inputPath]
      ByteString.readFile outputPath

openSslSha256 :: RuntimeAcmeBindPlan -> LazyByteString.ByteString -> IO ByteString.ByteString
openSslSha256 !runtimeAcmePlan inputBytes = do
  temporaryDirectory <- getCanonicalTemporaryDirectory
  bracket
    (createTempDirectory temporaryDirectory "harch-web-acme-sha256")
    removePathForcibly
    $ \hashDirectory -> do
      let inputPath = hashDirectory </> "hash-input.bin"
          outputPath = hashDirectory </> "hash-output.bin"
      LazyByteString.writeFile inputPath inputBytes
      runOpenSslCommand runtimeAcmePlan ["dgst", "-sha256", "-binary", "-out", outputPath, inputPath]
      ByteString.readFile outputPath

stopRuntimeServers :: [RunningRuntimeServer] -> IO ()
stopRuntimeServers =
  mapM_ stopRuntimeServer

stopRuntimeServer :: RunningRuntimeServer -> IO ()
stopRuntimeServer runningServer = do
  Socket.close (runningRuntimeSocket runningServer)
  killThread (runningRuntimeThreadId runningServer)

stopAcmeRuntimeServers :: [RunningAcmeRuntimeServer] -> IO ()
stopAcmeRuntimeServers =
  mapM_ stopAcmeRuntimeServer

stopAcmeRuntimeServer :: RunningAcmeRuntimeServer -> IO ()
stopAcmeRuntimeServer runningServer = do
  stopRuntimeServer (runningAcmeRuntimeServer runningServer)
  removePathForcibly (runningAcmeCleanupDirectory runningServer)

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
    <> map ((Https,) . acmeEndpoint) (acmeBindPlans startupPlan)

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
waitForShutdownSignal =
  forever (threadDelay maxBound)

runtimeStartupValidationError :: ServerStartupPlan -> Maybe String
runtimeStartupValidationError startupPlan =
  case ( null (acmeBindPlans startupPlan),
         null (httpEndpoints (httpBindPlan startupPlan)),
         null (manualTlsBindPlans startupPlan)
       ) of
    (True, True, True) ->
      Just "Unsupported runtime listener startup plan: no runtime listeners are configured."
    (False, _, _) ->
      firstAcmeRuntimeStartupError (httpEndpoints (httpBindPlan startupPlan)) (acmeBindPlans startupPlan)
    (True, _, _) ->
      Nothing

firstAcmeRuntimeStartupError :: [ListenerEndpoint] -> [AcmeBindPlan] -> Maybe String
firstAcmeRuntimeStartupError httpListenerEndpoints acmePlans =
  listToMaybe (mapMaybe (validateAcmeRuntimeBindPlan httpListenerEndpoints) acmePlans)

validateAcmeRuntimeBindPlan :: [ListenerEndpoint] -> AcmeBindPlan -> Maybe String
validateAcmeRuntimeBindPlan httpListenerEndpoints acmePlan =
  case acmeHttp01ChallengePort acmePlan of
    Left runtimeError ->
      Just runtimeError
    Right challengePort ->
      if hasMatchingAcmeHttp01ChallengeEndpoint challengePort httpListenerEndpoints acmePlan
        then validateAcmeRuntimeConfiguration acmePlan
        else
          Just $
            "Unsupported runtime listener startup plan: ACME listener on "
              <> renderListenerEndpoint (acmeEndpoint acmePlan)
              <> " requires an HTTP listener on port "
              <> show challengePort
              <> " for http-01 challenges."

validateAcmeRuntimeConfiguration :: AcmeBindPlan -> Maybe String
validateAcmeRuntimeConfiguration acmePlan =
  case acmeChallengeBackend (acmeListenerConfig acmePlan) of
    InProcessHttp01
      | null (acmeDomains (acmeListenerConfig acmePlan)) ->
          Just $
            "Unsupported runtime listener startup plan: ACME listener on "
              <> renderListenerEndpoint (acmeEndpoint acmePlan)
              <> " requires ACME domains for in-process http-01 runtime startup."
    _ ->
      Nothing

hasMatchingAcmeHttp01ChallengeEndpoint :: Int -> [ListenerEndpoint] -> AcmeBindPlan -> Bool
hasMatchingAcmeHttp01ChallengeEndpoint challengePort httpListenerEndpoints acmePlan =
  case find (isAcmeHttp01ChallengeEndpointFor challengePort (acmeEndpoint acmePlan)) httpListenerEndpoints of
    Just _ -> True
    Nothing -> False

acmeHttp01ChallengePort :: AcmeBindPlan -> Either String Int
acmeHttp01ChallengePort acmePlan =
  case acmeChallengeBackend (acmeListenerConfig acmePlan) of
    InProcessHttp01 ->
      Right (acmeHttp01Port (acmeListenerConfig acmePlan))
    CertbotHttp01 certbotConfig ->
      case certbotOptionValue "--http-01-port" (certbotArguments certbotConfig) of
        Nothing ->
          Right 80
        Just portText ->
          maybe
            ( Left $
                "Unsupported runtime listener startup plan: ACME listener on "
                  <> renderListenerEndpoint (acmeEndpoint acmePlan)
                  <> " has an invalid certbot http-01 port: "
                  <> Text.unpack portText
            )
            Right
            (readMaybe (Text.unpack portText))

certbotOptionValue :: Text -> [Text] -> Maybe Text
certbotOptionValue optionName arguments =
  listToMaybe (certbotOptionValues optionName arguments)

isAcmeHttp01ChallengeEndpointFor :: Int -> ListenerEndpoint -> ListenerEndpoint -> Bool
isAcmeHttp01ChallengeEndpointFor challengePort acmeListenerEndpoint httpListenerEndpoint =
  endpointPort httpListenerEndpoint == challengePort
    && ( endpointHost httpListenerEndpoint == "0.0.0.0"
           || endpointHost httpListenerEndpoint == endpointHost acmeListenerEndpoint
       )

renderListenerEndpoint :: ListenerEndpoint -> String
renderListenerEndpoint endpoint =
  Text.unpack (endpointHost endpoint) <> ":" <> show (endpointPort endpoint)

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
    PageResponseWithMetadata pageResponseBodyValue page ->
      let !pageStatusMessage = ByteString.empty
          !pageStatusMessageLength = ByteString.length pageStatusMessage
          !pageStatus = pageStatusMessageLength `seq` Http.Status (responseStatus pageResponseBodyValue) pageStatusMessage
       in Wai.responseLBS
            pageStatus
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
  if redirectHttpToHttps requestPolicyConfig
    && requestScheme request == "http"
    && not (isAcmeHttp01ChallengeRequest request)
    then
      fmap
        ( \redirectAuthority ->
            "https://"
              <> redirectAuthority
              <> requestRedirectPathAndQuery request
        )
        (requestRedirectAuthority requestPolicyConfig request)
    else Nothing

requestRedirectAuthority :: RequestPolicyConfig -> Wai.Request -> Maybe ByteString.ByteString
requestRedirectAuthority requestPolicyConfig request =
  fmap
    (applyHttpsRedirectPort (httpsRedirectPort requestPolicyConfig))
    (lookup "Host" (Wai.requestHeaders request))

requestRedirectPathAndQuery :: Wai.Request -> ByteString.ByteString
requestRedirectPathAndQuery request =
  TextEncoding.encodeUtf8 (externalRequestPath request) <> Wai.rawQueryString request

applyHttpsRedirectPort :: Maybe Int -> ByteString.ByteString -> ByteString.ByteString
applyHttpsRedirectPort maybeRedirectPort hostHeader =
  let normalizedDefaultHostHeader =
        fromMaybe hostHeader (ByteStringChar8.stripSuffix ":80" hostHeader)
      hostOnly = ByteStringChar8.takeWhile (/= ':') normalizedDefaultHostHeader
   in case maybeRedirectPort of
        Nothing -> normalizedDefaultHostHeader
        Just 443 -> hostOnly
        Just redirectPort ->
          hostOnly <> ":" <> ByteStringChar8.pack (show redirectPort)

isAcmeHttp01ChallengeRequest :: Wai.Request -> Bool
isAcmeHttp01ChallengeRequest request =
  Text.isPrefixOf "/.well-known/acme-challenge/" (waiRequestPath request)

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
                    tlsPrivateKeyFile = privateKeyPath,
                    tlsCredentialSourceKind = ManualTlsCredentials,
                    tlsStartupMode = RequireCertificateFiles
                  }
            )
        (Https, Just TlsConfig {certificateSource = SharedCertificateFiles {certificateDirectory = sharedDirectory, sharedCertificateStartupMode = startupMode}}) ->
          let (certificatePath, privateKeyPath) = sharedCertificatePaths sharedDirectory
           in Right
                ( PlannedManualTls
                    ManualTlsBindPlan
                      { tlsEndpoint = listenerEndpoint listenerConfig,
                        tlsCertificateFile = certificatePath,
                        tlsPrivateKeyFile = privateKeyPath,
                        tlsCredentialSourceKind = SharedTlsCredentials,
                        tlsStartupMode = startupMode
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

sharedCertificatePaths :: FilePath -> (FilePath, FilePath)
sharedCertificatePaths certificateDirectory =
  (certificateDirectory </> "fullchain.pem", certificateDirectory </> "privkey.pem")

publishCertificateFiles :: FilePath -> FilePath -> FilePath -> IO (FilePath, FilePath)
publishCertificateFiles certificateDirectory sourceCertificatePath sourcePrivateKeyPath = do
  createDirectoryIfMissing True certificateDirectory
  let (certificatePath, privateKeyPath) = sharedCertificatePaths certificateDirectory
  copyFile sourceCertificatePath certificatePath
  copyFile sourcePrivateKeyPath privateKeyPath
  pure (certificatePath, privateKeyPath)

loadReloadingTlsCredentials :: FilePath -> FilePath -> IO ReloadingTlsCredentials
loadReloadingTlsCredentials certificatePath privateKeyPath = do
  snapshot <- loadTlsCredentialSnapshotOrThrow certificatePath privateKeyPath
  snapshotReference <- newIORef snapshot
  pure
    ReloadingTlsCredentials
      { tlsCredentialCertificatePath = certificatePath,
        tlsCredentialPrivateKeyPath = privateKeyPath,
        tlsCredentialSnapshotReference = snapshotReference
      }

loadReloadingTlsCredentialsWithLabel :: String -> FilePath -> FilePath -> IO ReloadingTlsCredentials
loadReloadingTlsCredentialsWithLabel tlsLabel certificatePath privateKeyPath = do
  snapshot <- loadTlsCredentialSnapshotOrThrowWithLabel tlsLabel certificatePath privateKeyPath
  snapshotReference <- newIORef snapshot
  pure
    ReloadingTlsCredentials
      { tlsCredentialCertificatePath = certificatePath,
        tlsCredentialPrivateKeyPath = privateKeyPath,
        tlsCredentialSnapshotReference = snapshotReference
      }

awaitReloadingTlsCredentials :: Maybe Int -> FilePath -> FilePath -> IO ReloadingTlsCredentials
awaitReloadingTlsCredentials waitTimeoutSeconds certificatePath privateKeyPath = do
  startedAt <- getMonotonicTimeNSec
  go startedAt
  where
    timeoutWindow =
      fmap
        (\seconds -> (seconds, fromIntegral seconds * 1000000000))
        waitTimeoutSeconds

    go !startedAt = do
      snapshotResult <- loadTlsCredentialSnapshotIfPresent certificatePath privateKeyPath
      case snapshotResult of
        Just (Right snapshot) -> do
          snapshotReference <- newIORef snapshot
          pure
            ReloadingTlsCredentials
              { tlsCredentialCertificatePath = certificatePath,
                tlsCredentialPrivateKeyPath = privateKeyPath,
                tlsCredentialSnapshotReference = snapshotReference
              }
        _ -> do
          currentTime <- getMonotonicTimeNSec
          case timeoutWindow of
            Just (waitSeconds, timeoutNs)
              | currentTime - startedAt >= timeoutNs ->
                  let timeoutSuffix = " after " <> show waitSeconds <> " seconds"
                   in ioError . userError $
                        case snapshotResult of
                          Just (Left loadError) ->
                            "Timed out waiting for shared TLS credentials at "
                              <> certificatePath
                              <> " and "
                              <> privateKeyPath
                              <> timeoutSuffix
                              <> ": "
                              <> loadError
                          _ ->
                            "Timed out waiting for shared TLS certificate files at "
                              <> certificatePath
                              <> " and "
                              <> privateKeyPath
                              <> timeoutSuffix
            _ -> threadDelay 100000 >> go startedAt

reloadTlsCredentialsIfChanged :: ReloadingTlsCredentials -> IO TLS.Credentials
reloadTlsCredentialsIfChanged reloadingTlsCredentials = do
  cachedSnapshot <-
    atomicModifyIORef'
      (tlsCredentialSnapshotReference reloadingTlsCredentials)
      (\snapshot -> (snapshot, snapshot))
  latestSnapshotResult <-
    loadTlsCredentialSnapshotIfPresent
      (tlsCredentialCertificatePath reloadingTlsCredentials)
      (tlsCredentialPrivateKeyPath reloadingTlsCredentials)
  case latestSnapshotResult of
    Just (Right latestSnapshot)
      | tlsCredentialModifiedTimes latestSnapshot /= tlsCredentialModifiedTimes cachedSnapshot ->
          latestSnapshot `seq`
            atomicModifyIORef'
              (tlsCredentialSnapshotReference reloadingTlsCredentials)
              (const (latestSnapshot, tlsCredentialValues latestSnapshot))
    _ ->
      pure (tlsCredentialValues cachedSnapshot)

loadTlsCredentialSnapshotOrThrow :: FilePath -> FilePath -> IO TlsCredentialSnapshot
loadTlsCredentialSnapshotOrThrow =
  loadTlsCredentialSnapshotOrThrowWithLabel "Manual TLS"

loadTlsCredentialSnapshotOrThrowWithLabel :: String -> FilePath -> FilePath -> IO TlsCredentialSnapshot
loadTlsCredentialSnapshotOrThrowWithLabel tlsLabel certificatePath privateKeyPath =
  loadTlsCredentialSnapshotOrThrowWithLoader
    tlsLabel
    certificatePath
    privateKeyPath
    (loadTlsCredentialSnapshotIfPresent certificatePath privateKeyPath)

loadTlsCredentialSnapshotOrThrowWithLoader :: String -> FilePath -> FilePath -> IO (Maybe (Either String TlsCredentialSnapshot)) -> IO TlsCredentialSnapshot
loadTlsCredentialSnapshotOrThrowWithLoader tlsLabel certificatePath privateKeyPath loadSnapshot = do
  ensureRuntimeFileExists (tlsLabel <> " certificate file does not exist: ") certificatePath
  ensureRuntimeFileExists (tlsLabel <> " private key file does not exist: ") privateKeyPath
  snapshotResult <- loadSnapshot
  case fromMaybe (Left "credential files disappeared while loading") snapshotResult of
    Right snapshot ->
      pure snapshot
    Left loadError ->
      ioError . userError $
        "Failed to load "
          <> lowerFirst tlsLabel
          <> " credentials from "
          <> certificatePath
          <> " and "
          <> privateKeyPath
          <> ": "
          <> loadError
  where
    lowerFirst [] = []
    lowerFirst (firstCharacter : remainingCharacters) =
      toLower firstCharacter : remainingCharacters

loadTlsCredentialSnapshotIfPresent :: FilePath -> FilePath -> IO (Maybe (Either String TlsCredentialSnapshot))
loadTlsCredentialSnapshotIfPresent certificatePath privateKeyPath = do
  certificateExists <- doesFileExist certificatePath
  privateKeyExists <- doesFileExist privateKeyPath
  if certificateExists && privateKeyExists
    then do
      certificateModifiedAt <- getModificationTime certificatePath
      privateKeyModifiedAt <- getModificationTime privateKeyPath
      credentialResult <- TLS.credentialLoadX509 certificatePath privateKeyPath
      pure
        ( Just
            ( fmap
                ( \credential ->
                    TlsCredentialSnapshot
                      { tlsCredentialModifiedTimes = (certificateModifiedAt, privateKeyModifiedAt),
                        tlsCredentialValues = TLS.Credentials [credential]
                      }
                )
                credentialResult
            )
        )
    else
      pure Nothing

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

exportRequestObservabilityToOtlp ::
  Text ->
  OtlpExporter ->
  Observability.RequestObservability ->
  IO ()
exportRequestObservabilityToOtlp serviceName exporter requestObservability = do
  (traceId, spanId) <- nextOtlpSpanIdentifiers
  endTimeUnixNano <- getMonotonicTimeNSec
  let requestBody =
        otlpTraceRequestBody
          serviceName
          traceId
          spanId
          endTimeUnixNano
          endTimeUnixNano
          requestObservability
  baseRequest <- HttpClient.parseRequest (Text.unpack (otlpEndpoint exporter))
  response <-
    HttpClient.httpLbs
      baseRequest
        { HttpClient.method = "POST",
          HttpClient.requestHeaders =
            (Http.hContentType, "application/json")
              : map otlpHeader (otlpHeaders exporter),
          HttpClient.requestBody = HttpClient.RequestBodyLBS requestBody
        }
      otlpHttpManager
  let statusCode = Http.statusCode (HttpClient.responseStatus response)
  unless (statusCode >= 200 && statusCode < 300) $
    ioError . userError $
      "OTLP trace export failed with status "
        <> show statusCode
        <> ".\nbody:\n"
        <> renderAcmeResponseBody response

otlpTraceRequestBody ::
  Text ->
  Text ->
  Text ->
  Word64 ->
  Word64 ->
  Observability.RequestObservability ->
  LazyByteString.ByteString
otlpTraceRequestBody serviceName traceId spanId startTimeUnixNano endTimeUnixNano requestObservability =
  jsonObjectBytes
    [ ( "resourceSpans",
        jsonArrayBytes
          [ jsonObjectBytes
              [ ("resource", otlpResourceObject serviceName),
                ( "scopeSpans",
                  jsonArrayBytes
                    [ jsonObjectBytes
                        [ ( "scope",
                            jsonObjectBytes
                              [("name", jsonStringBytes "harch-web")]
                          ),
                          ( "spans",
                            jsonArrayBytes
                              [ otlpSpanObject
                                  traceId
                                  spanId
                                  startTimeUnixNano
                                  endTimeUnixNano
                                  requestObservability
                              ]
                          )
                        ]
                    ]
                )
              ]
          ]
      )
    ]

otlpResourceObject :: Text -> LazyByteString.ByteString
otlpResourceObject serviceName =
  jsonObjectBytes
    [ ( "attributes",
        jsonArrayBytes
          [ otlpAttribute
              Observability.ObservabilityAttribute
                { Observability.attributeName = "service.name",
                  Observability.attributeValue = Observability.TextAttribute serviceName
                },
            otlpAttribute
              Observability.ObservabilityAttribute
                { Observability.attributeName = "telemetry.sdk.language",
                  Observability.attributeValue = Observability.TextAttribute "haskell"
                },
            otlpAttribute
              Observability.ObservabilityAttribute
                { Observability.attributeName = "telemetry.sdk.name",
                  Observability.attributeValue = Observability.TextAttribute "harch-web"
                }
          ]
      )
    ]

otlpSpanObject ::
  Text ->
  Text ->
  Word64 ->
  Word64 ->
  Observability.RequestObservability ->
  LazyByteString.ByteString
otlpSpanObject traceId spanId startTimeUnixNano endTimeUnixNano requestObservability =
  jsonObjectBytes
    ( [ ("traceId", jsonStringBytes traceId),
        ("spanId", jsonStringBytes spanId),
        ("name", jsonStringBytes (requestSpanDisplayName requestObservability)),
        ("kind", jsonStringBytes "SPAN_KIND_SERVER"),
        ("startTimeUnixNano", jsonStringBytes (Text.pack (show startTimeUnixNano))),
        ("endTimeUnixNano", jsonStringBytes (Text.pack (show endTimeUnixNano))),
        ( "attributes",
          jsonArrayBytes
            ( map otlpAttribute $
                Observability.requestSpanAttributes
                  (Observability.observabilityRequestSpan requestObservability)
            )
        )
      ]
        ++ otlpSpanStatusFields requestObservability
    )

requestSpanDisplayName :: Observability.RequestObservability -> Text
requestSpanDisplayName =
  Observability.requestSpanDisplayName . Observability.observabilityRequestSpan

otlpSpanStatusFields :: Observability.RequestObservability -> [(Text, LazyByteString.ByteString)]
otlpSpanStatusFields requestObservability =
  case requestObservabilityStatusCode requestObservability of
    Just statusCode
      | statusCode >= 500 ->
          [ ( "status",
              jsonObjectBytes
                [("code", jsonStringBytes "STATUS_CODE_ERROR")]
            )
          ]
    _ -> []

requestObservabilityStatusCode :: Observability.RequestObservability -> Maybe Int
requestObservabilityStatusCode requestObservability =
  listToMaybe
    [ statusCode
    | Observability.ObservabilityAttribute
        { Observability.attributeName = "http.response.status_code",
          Observability.attributeValue = Observability.IntAttribute statusCode
        } <-
        Observability.requestSpanAttributes
          (Observability.observabilityRequestSpan requestObservability)
    ]

otlpAttribute :: Observability.ObservabilityAttribute -> LazyByteString.ByteString
otlpAttribute attribute =
  jsonObjectBytes
    [ ("key", jsonStringBytes (Observability.attributeName attribute)),
      ("value", otlpAttributeValue (Observability.attributeValue attribute))
    ]

otlpAttributeValue :: Observability.ObservabilityAttributeValue -> LazyByteString.ByteString
otlpAttributeValue attributeValue =
  jsonObjectBytes
    [ case attributeValue of
        Observability.TextAttribute textValue ->
          ("stringValue", jsonStringBytes textValue)
        Observability.IntAttribute intValue ->
          ("intValue", jsonStringBytes (Text.pack (show intValue)))
    ]

otlpHeader :: (Text, Text) -> Http.Header
otlpHeader (headerName, headerValue) =
  (fromString (Text.unpack headerName), TextEncoding.encodeUtf8 headerValue)

nextOtlpSpanIdentifiers :: IO (Text, Text)
nextOtlpSpanIdentifiers = do
  requestSeed <- atomicModifyIORef' otlpSpanSeed (\seed -> let nextSeed = seed + 1 in (nextSeed, nextSeed))
  monotonicTime <- getMonotonicTimeNSec
  let traceIdBytes = word64Bytes monotonicTime <> word64Bytes requestSeed
      spanIdBytes = word64Bytes (monotonicTime `xor` (requestSeed + 0x9e3779b97f4a7c15))
  pure (base64Text traceIdBytes, base64Text spanIdBytes)

base64Text :: ByteString.ByteString -> Text
base64Text =
  TextEncoding.decodeUtf8 . Base64.encode

word64Bytes :: Word64 -> ByteString.ByteString
word64Bytes word =
  ByteString.pack
    [ fromIntegral (word `shiftR` 56),
      fromIntegral (word `shiftR` 48),
      fromIntegral (word `shiftR` 40),
      fromIntegral (word `shiftR` 32),
      fromIntegral (word `shiftR` 24),
      fromIntegral (word `shiftR` 16),
      fromIntegral (word `shiftR` 8),
      fromIntegral word
    ]

otlpHttpManager :: HttpClient.Manager
{-# NOINLINE otlpHttpManager #-}
otlpHttpManager =
  unsafePerformIO (HttpClient.newManager HttpClientTls.tlsManagerSettings)

otlpSpanSeed :: IORef Word64
{-# NOINLINE otlpSpanSeed #-}
otlpSpanSeed =
  unsafePerformIO (newIORef 0)

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
