{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module HarchWeb
  ( AcmeBindPlan (..),
    AcmeAuthorizationResponse (..),
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
    LiveRegion (..),
    AssetPath (..),
    CertbotConfig (..),
    ClientActionRequest (..),
    ClientActionResponse (..),
    CorsPolicyConfig (..),
    CssClass (..),
    CssScope (..),
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
    NavigationRuntime (..),
    ObservabilityConfig (..),
    ObservabilityStartupPlan (..),
    OtlpExporter (..),
    OtlpExporterStartup (..),
    Page (..),
    PageShell (..),
    PreparedAcmeChallenge (..),
    RequestMiddleware (..),
    MiddlewareResult (..),
    RequestPolicyConfig (..),
    ReloadingTlsCredentials,
    Response (..),
    ResponseBody (..),
    ResponseDiagnostics (..),
    ResponseSecurityHeadersConfig (..),
    RegionPatch (..),
    ResolvedNavigationItem (..),
    RuntimeDescriptor (..),
    RuntimeNonce (..),
    ServerSentEvent (..),
    RouteCodec (..),
    RouteRequest (..),
    RuntimeAcmeBindPlan (..),
    ServerConfig (..),
    ServerStartupPlan (..),
    StaticAssetRoot (..),
    StaticAssetsConfig (..),
    Stylesheet (..),
    StrictTransportSecurityConfig (..),
    TelemetrySignal (..),
    TlsCertificateSource (..),
    TlsCredentialSourceKind (..),
    TlsStartupMode (..),
    TlsConfig (..),
    acmeCertificateRequestConfig,
    acmeChallengeResponseForRequest,
    acmeHttp01ChallengeToken,
    validAcmeHttp01ChallengeToken,
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
    cssClassText,
    cssScope,
    createAcmeAccount,
    createAcmeOrder,
    decodeAcmeJsonResponse,
    defaultContentSecurityPolicy,
    defaultCaptureKernel,
    defaultCaptureKernelScript,
    defaultCorsPolicyConfig,
    defaultNavigationRuntime,
    defaultNavigationRuntimeScript,
    defaultResponseSecurityHeadersConfig,
    defaultStaticAssetContentTypes,
    escapeJsonCharacter,
    exportConnectionObservabilityToOtlp,
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
    liveRegionAttributes,
    mailtoAcmeContact,
    matchRoute,
    navigationRuntimeResponse,
    navigationRuntimeScriptSource,
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
    reloadTlsCredentialsIfChanged,
    registerAcmeChallenges,
    renderAcmeResponseBody,
    renderDocument,
    renderDocumentWithNonce,
    renderServerSentEvent,
    runRequestMiddlewarePipeline,
    responseHeaderText,
    responseDiagnostics,
    redirectResponse,
    responseKind,
    responseStatusCode,
    serverSentEventContentType,
    requestHostWithoutPort,
    routeHref,
    loadReloadingTlsCredentials,
    loadTlsCredentialSnapshotOrThrowWithLoader,
    startManualTlsRuntimeServerWithStarter,
    runOpenSslCommand,
    runOpenSslTextCommand,
    runServer,
    startWarpRuntimeServerOnSocket,
    runtimeCertbotArguments,
    signOpenSslRs256,
    splitCertbotDomainValue,
    staticAssetHref,
    staticAssetHrefWithPrefix,
    stylesheet,
    toWaiApplication,
    triggerAcmeChallenge,
    unicodeJsonCharacterParser,
    unregisterAcmeChallenges,
    withLocalTestServer,
    generateRuntimeNonce,
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent (MVar, ThreadId, forkFinally, forkIOWithUnmask, killThread, modifyMVar, modifyMVar_, myThreadId, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar, threadDelay, tryPutMVar)
import Control.Exception (IOException, SomeException, bracket, bracket_, displayException, evaluate, finally, fromException, onException, throwIO, try)
import Control.Monad (replicateM, unless, void)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (bimap)
import Data.Bits (shiftR, xor)
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64.URL qualified as Base64Url
import Data.ByteString.Char8 qualified as ByteStringChar8
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Char (digitToInt, isDigit, isHexDigit, toLower)
import Data.Either (lefts)
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.List (find, intercalate, maximumBy)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybeToList)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.Encoding.Error qualified as TextEncodingError
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word64, Word8)
import GHC.Clock (getMonotonicTimeNSec)
import HarchWeb.Observability qualified as Observability
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Client.TLS qualified as HttpClientTls
import Network.HTTP.Types qualified as Http
import Network.HTTP.Types.URI qualified as HttpUri
import Network.Socket qualified as Socket
import Network.TLS qualified as TLS
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WarpTLS qualified as WarpTLS
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, getModificationTime, removePathForcibly)
import System.Exit (ExitCode (..))
import System.FilePath (splitDirectories, takeExtension, (</>))
import System.IO (Handle, IOMode (ReadMode), hFlush, hPutStrLn, withBinaryFile)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Signals (Handler (Catch), installHandler, sigINT, sigTERM)
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

data AcmeConfig = AcmeConfig
  { acmeDirectoryUrl :: Text,
    acmeContactEmails :: [Text],
    acmeDomains :: [Text],
    acmeHttp01Port :: Int,
    acmeCertificateDirectory :: Maybe FilePath,
    acmeCertbotConfig :: CertbotConfig
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
    listenerTls :: Maybe TlsConfig,
    listenerAcme :: Maybe AcmeConfig
  }
  deriving (Eq)

instance Show ListenerConfig where
  showsPrec precedence listenerConfig =
    showParen (precedence > 10) $
      showString "ListenerConfig {listenerHost = "
        . shows (listenerHost listenerConfig)
        . showString ", listenerPort = "
        . shows (listenerPort listenerConfig)
        . showString ", listenerScheme = "
        . shows (listenerScheme listenerConfig)
        . showString ", listenerTls = "
        . shows (listenerTls listenerConfig)
        . maybe
          id
          (\acmeConfig -> showString ", listenerAcme = " . shows acmeConfig)
          (listenerAcme listenerConfig)
        . showString "}"

data StaticAssetRoot = StaticAssetRoot
  { staticUrlPrefix :: Text,
    staticDirectory :: FilePath
  }
  deriving (Eq, Show)

data StaticAssetsConfig = StaticAssetsConfig
  { staticAssetRoots :: [StaticAssetRoot],
    staticAssetContentTypes :: [(Text, Text)],
    staticCacheControlSeconds :: Maybe Int
  }
  deriving (Eq, Show)

defaultStaticAssetContentTypes :: [(Text, Text)]
defaultStaticAssetContentTypes =
  [ (".css", "text/css; charset=utf-8"),
    (".html", "text/html; charset=utf-8"),
    (".js", "application/javascript; charset=utf-8"),
    (".json", "application/json; charset=utf-8"),
    (".svg", "image/svg+xml"),
    (".txt", "text/plain; charset=utf-8")
  ]

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

data CorsPolicyConfig = CorsPolicyConfig
  { corsAllowedOrigins :: [Text],
    corsAllowedMethods :: [Text],
    corsAllowedHeaders :: [Text],
    corsMaxAgeSeconds :: Maybe Int
  }
  deriving (Eq, Show)

defaultCorsPolicyConfig :: CorsPolicyConfig
defaultCorsPolicyConfig =
  CorsPolicyConfig
    { corsAllowedOrigins = [],
      corsAllowedMethods = ["GET", "HEAD", "OPTIONS"],
      corsAllowedHeaders = ["Content-Type", "X-Requested-With"],
      corsMaxAgeSeconds = Nothing
    }

data ResponseSecurityHeadersConfig = ResponseSecurityHeadersConfig
  { contentSecurityPolicy :: Maybe Text,
    contentTypeOptionsNoSniff :: Bool,
    xssProtection :: Maybe Text,
    referrerPolicy :: Maybe Text,
    permissionsPolicy :: Maybe Text,
    frameOptions :: Maybe Text
  }
  deriving (Eq, Show)

defaultContentSecurityPolicy :: Text
defaultContentSecurityPolicy =
  Text.intercalate
    "; "
    [ "default-src 'self'",
      "base-uri 'self'",
      "object-src 'none'",
      "frame-ancestors 'none'",
      "form-action 'self'",
      "script-src 'self'",
      "style-src 'self'",
      "img-src 'self' data:",
      "font-src 'self'",
      "connect-src 'self'"
    ]

defaultResponseSecurityHeadersConfig :: ResponseSecurityHeadersConfig
defaultResponseSecurityHeadersConfig =
  ResponseSecurityHeadersConfig
    { contentSecurityPolicy = Just defaultContentSecurityPolicy,
      contentTypeOptionsNoSniff = True,
      xssProtection = Just "1; mode=block",
      referrerPolicy = Just "strict-origin-when-cross-origin",
      permissionsPolicy = Just "accelerometer=(), camera=(), geolocation=(), gyroscope=(), magnetometer=(), microphone=(), payment=(), usb=()",
      frameOptions = Just "DENY"
    }

data RequestPolicyConfig = RequestPolicyConfig
  { redirectHttpToHttps :: Bool,
    httpsRedirectPort :: Maybe Int,
    strictTransportSecurity :: Maybe StrictTransportSecurityConfig,
    trustForwardedHeaders :: Bool,
    corsPolicy :: CorsPolicyConfig,
    responseSecurityHeaders :: ResponseSecurityHeadersConfig
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
    acmeTlsEndpoint :: Maybe ListenerEndpoint,
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
  | InvalidListenerAcmeConfiguration ListenerConfig
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

-- | The two live-region modes that are safe defaults for server-rendered region
-- patches. Keep the role and its announcement urgency together so action UIs do
-- not accidentally render a contradictory role/aria-live pair.
data LiveRegion
  = PoliteStatus
  | AssertiveAlert
  deriving (Eq, Show)

data NavigationItem route = NavigationItem
  { navigationLabel :: Text,
    navigationRoute :: route
  }
  deriving (Eq, Show)

data NavigationRuntime = NavigationRuntime
  { navigationRuntimePath :: Text,
    navigationRuntimeScript :: Text
  }
  deriving (Eq, Show)

-- | Runtime assets are declared before document rendering so the server can
-- apply the correct CSP policy without inspecting rendered HTML.
data RuntimeDescriptor
  = InlineBootstrap
      { runtimeDescriptorName :: Text,
        runtimeDescriptorSource :: Text
      }
  | DeferredModule
      { runtimeDescriptorName :: Text,
        runtimeDescriptorSource :: Text
      }
  deriving (Eq, Show)

newtype RuntimeNonce = RuntimeNonce
  { runtimeNonceValue :: Text
  }
  deriving (Eq, Show)

-- | A route-aware reference to an app-owned static file. This stays distinct
-- from filesystem paths so components only pass browser-visible asset URLs.
newtype AssetPath = AssetPath
  { assetPathText :: Text
  }
  deriving (Eq, Show)

-- | An external stylesheet declaration. Inline CSS remains intentionally
-- absent so the default CSP can keep @style-src 'self'@.
newtype Stylesheet = Stylesheet
  { stylesheetAsset :: AssetPath
  }
  deriving (Eq, Show)

-- | A stable namespace for styles authored by one page or component.
newtype CssScope = CssScope
  { cssScopeName :: Text
  }
  deriving (Eq, Show)

-- | A rendered CSS class can either be deliberately global or tied to a
-- component scope.
data CssClass
  = ScopedCssClass CssScope Text
  | GlobalCssClass Text
  deriving (Eq, Show)

stylesheet :: AssetPath -> Stylesheet
stylesheet = Stylesheet

cssScope :: Text -> CssScope
cssScope = CssScope

cssClassText :: CssClass -> Text
cssClassText cssClass =
  case cssClass of
    ScopedCssClass (CssScope scopeName) localName -> "harch-" <> scopeName <> "-" <> localName
    GlobalCssClass className -> className

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
    documentStylesheets :: [Stylesheet],
    documentRuntimeDescriptors :: [RuntimeDescriptor]
  }
  deriving (Eq, Show)

data PageShell route context = PageShell
  { shellBodyAttributes :: [HtmlAttribute],
    shellNavigationAttributes :: [HtmlAttribute],
    shellNavigationItems :: [NavigationItem route],
    shellMainId :: Text,
    shellMainAttributes :: [HtmlAttribute],
    shellStylesheets :: [Stylesheet],
    shellRuntimeDescriptors :: [RuntimeDescriptor]
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

-- | One event in a server-sent event stream. Event names and identifiers are
-- rendered as single protocol fields; embedded line breaks are discarded so a
-- value cannot inject another SSE field. Payload data may contain line breaks
-- and is rendered as one @data:@ field per line.
data ServerSentEvent = ServerSentEvent
  { serverSentEventName :: Maybe Text,
    serverSentEventId :: Maybe Text,
    serverSentEventData :: Text
  }
  deriving (Eq, Show)

data ResponseDiagnostics = ResponseDiagnostics
  { diagnosticObservabilityAttributes :: [Observability.ObservabilityAttribute],
    diagnosticLogEntries :: [Text]
  }

-- | A typed application-owned request middleware. Middleware runs after
-- framework redirect and CORS policy handling, and before routing or action
-- dispatch. Static assets remain public unless an app serves them as routes.
newtype RequestMiddleware context = RequestMiddleware
  { runRequestMiddleware :: Wai.Request -> context -> IO (MiddlewareResult context)
  }

-- | Middleware may carry an enriched request context onward or halt with a
-- framework response body. A halted response still receives framework
-- security headers, logging, and observability.
data MiddlewareResult context
  = ContinueMiddleware context
  | HaltMiddleware context ResponseBody
  deriving (Eq, Show)

-- | A same-origin form action captured before deferred behavior modules load.
-- Form fields preserve their authored order, including the successful submitter.
data ClientActionRequest context = ClientActionRequest
  { clientActionMethod :: Text,
    clientActionPath :: Text,
    clientActionFields :: [(Text, Text)],
    clientActionCsrfToken :: Maybe Text,
    clientActionContext :: context
  }
  deriving (Eq, Show)

-- | A named SSR region replacement returned by a client action. The replacement
-- must include the region element itself, preserving its id for later patches.
data RegionPatch = RegionPatch
  { regionPatchId :: Text,
    regionPatchHtml :: Text
  }
  deriving (Eq, Show)

data ClientActionResponse = ClientActionResponse
  { clientActionStatus :: Int,
    clientActionPatches :: [RegionPatch],
    clientActionFocusId :: Maybe Text,
    clientActionHeaders :: Http.ResponseHeaders,
    clientActionObservabilityAttributes :: [Observability.ObservabilityAttribute],
    clientActionLogEntries :: [Text]
  }
  deriving (Eq, Show)

data Response route context
  = PageResponse (Page route context)
  | PageResponseWithMetadata ResponseBody (Page route context)
  | BodyResponse ResponseBody
  | RedirectResponse ResponseBody Text
  | ClientActionBodyResponse ClientActionResponse
  deriving (Eq, Show)

redirectResponse :: Int -> Text -> Response route context
redirectResponse status =
  RedirectResponse
    ResponseBody
      { responseStatus = status,
        responseContentType = "text/plain; charset=utf-8",
        responseBody = "",
        responseObservabilityAttributes = [],
        responseLogEntries = []
      }

responseDiagnostics :: Response route context -> ResponseDiagnostics
responseDiagnostics response =
  case response of
    PageResponse _ -> ResponseDiagnostics [] []
    PageResponseWithMetadata responseBodyValue _ -> responseBodyDiagnostics responseBodyValue
    BodyResponse responseBodyValue -> responseBodyDiagnostics responseBodyValue
    RedirectResponse responseBodyValue _ -> responseBodyDiagnostics responseBodyValue
    ClientActionBodyResponse actionResponse -> responseBodyDiagnostics (clientActionResponseBody actionResponse)

responseBodyDiagnostics :: ResponseBody -> ResponseDiagnostics
responseBodyDiagnostics responseBodyValue =
  ResponseDiagnostics
    { diagnosticObservabilityAttributes = responseObservabilityAttributes responseBodyValue,
      diagnosticLogEntries = responseLogEntries responseBodyValue
    }

responseStatusCode :: (Eq route) => Application route context -> Response route context -> Int
responseStatusCode webApplication response =
  case response of
    PageResponse page ->
      if isNotFoundPage webApplication page
        then 404
        else 200
    PageResponseWithMetadata responseBodyValue _ -> responseStatus responseBodyValue
    BodyResponse responseBodyValue -> responseStatus responseBodyValue
    RedirectResponse responseBodyValue _ -> responseStatus responseBodyValue
    ClientActionBodyResponse actionResponse -> clientActionStatus actionResponse

responseKind :: Response route context -> Observability.ResponseKind
responseKind response =
  case response of
    PageResponse _ -> Observability.PageResponseKind
    PageResponseWithMetadata _ _ -> Observability.PageResponseKind
    BodyResponse _ -> Observability.BodyResponseKind
    RedirectResponse _ _ -> Observability.BodyResponseKind
    ClientActionBodyResponse _ -> Observability.BodyResponseKind

data RouteCodec route context = RouteCodec
  { parseRoute :: context -> Text -> Maybe (RouteRequest route context),
    renderRoute :: RouteRequest route context -> Text,
    notFoundRequest :: context -> RouteRequest route context
  }

data Application route context = Application
  { appName :: Text,
    defaultRequestContext :: context,
    requestContextFromRequest :: Wai.Request -> context -> context,
    applicationNavigationRuntime :: Maybe NavigationRuntime,
    applicationStaticAssets :: StaticAssetsConfig,
    applicationRequestPolicy :: RequestPolicyConfig,
    applicationRequestMiddleware :: [RequestMiddleware context],
    routeCodec :: RouteCodec route context,
    renderResponse :: RouteRequest route context -> IO (Response route context),
    handleClientAction :: ClientActionRequest context -> IO (Maybe ClientActionResponse),
    pageShell :: Page route context -> Document route,
    reportRequestObservability :: Observability.RequestObservability -> IO (),
    reportConnectionObservability :: Observability.ConnectionObservability -> IO (),
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

-- | Run middleware in declaration order. The first middleware sees the
-- request first; a halt short-circuits the remaining middleware.
runRequestMiddlewarePipeline :: [RequestMiddleware context] -> Wai.Request -> context -> IO (MiddlewareResult context)
runRequestMiddlewarePipeline middleware request = go middleware
  where
    go [] requestContext = pure (ContinueMiddleware requestContext)
    go (RequestMiddleware runMiddleware : remainingMiddleware) requestContext = do
      result <- runMiddleware request requestContext
      case result of
        ContinueMiddleware nextRequestContext -> go remainingMiddleware nextRequestContext
        HaltMiddleware haltedRequestContext responseBody -> pure (HaltMiddleware haltedRequestContext responseBody)

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

defaultNavigationRuntime :: NavigationRuntime
defaultNavigationRuntime =
  NavigationRuntime
    { navigationRuntimePath = "/assets/navigation.js",
      navigationRuntimeScript = defaultNavigationRuntimeScript
    }

-- | This tiny capture-phase kernel is deliberately inline in the head. It is
-- installed before any framework control in the body can become interactive;
-- larger behavior modules consume its queue after they load.
defaultCaptureKernel :: RuntimeDescriptor
defaultCaptureKernel =
  InlineBootstrap
    { runtimeDescriptorName = "harch-capture-kernel",
      runtimeDescriptorSource = defaultCaptureKernelScript
    }

defaultCaptureKernelScript :: Text
defaultCaptureKernelScript =
  Text.unlines
    [ "(() => {",
      "  const queuedEvents = [];",
      "  const controlSelector = '[data-harch-control]';",
      "  const actionSelector = 'form[data-harch-action=\"true\"]';",
      "  const capture = (event) => {",
      "    const target = event.target instanceof Element ? event.target.closest(controlSelector) : null;",
      "    if (target) {",
      "      if (event.type === 'submit' && target.matches(actionSelector)) {",
      "        const submitter = event.submitter instanceof HTMLElement ? event.submitter : undefined;",
      "        const fields = [];",
      "        new FormData(target, submitter).forEach((value, name) => {",
      "          if (typeof value === 'string') {",
      "            fields.push([name, value]);",
      "          }",
      "        });",
      "        queuedEvents.push({ type: 'submit', action: target.action, method: target.method, fields });",
      "        event.preventDefault();",
      "      } else {",
      "        queuedEvents.push({ event, target });",
      "      }",
      "      window.dispatchEvent(new Event('harch:capture'));",
      "    }",
      "  };",
      "  ['click', 'input', 'change', 'keydown', 'submit'].forEach((eventName) => {",
      "    document.addEventListener(eventName, capture, true);",
      "  });",
      "  window.__harchCaptureKernel = {",
      "    drain: () => queuedEvents.splice(0),",
      "  };",
      "})();"
    ]

generateRuntimeNonce :: IO RuntimeNonce
generateRuntimeNonce =
  withBinaryFile "/dev/urandom" ReadMode $ \randomHandle -> do
    randomBytes <- ByteString.hGet randomHandle 32
    pure
      ( RuntimeNonce
          (TextEncoding.decodeUtf8 (Base64Url.encode randomBytes))
      )

navigationRuntimeScriptSource :: Text -> NavigationRuntime -> Text
navigationRuntimeScriptSource pathPrefix runtime =
  applyRequestPathPrefix pathPrefix (navigationRuntimePath runtime)

navigationRuntimeResponse :: NavigationRuntime -> Text -> Maybe ResponseBody
navigationRuntimeResponse runtime requestPath =
  if requestPath == navigationRuntimePath runtime
    then
      Just
        ResponseBody
          { responseStatus = 200,
            responseContentType = "application/javascript; charset=utf-8",
            responseBody = navigationRuntimeScript runtime,
            responseObservabilityAttributes = [],
            responseLogEntries = []
          }
    else Nothing

defaultNavigationRuntimeScript :: Text
defaultNavigationRuntimeScript =
  Text.unlines
    [ "(() => {",
      "  const pageLinkSelector = 'a[data-page-link=\"true\"]';",
      "  const navigationRegionSelector = 'nav[data-navigation-region=\"primary\"]';",
      "  const navigationContentSelector = 'main[data-navigation-content=\"true\"]';",
      "  let navigationInFlight = false;",
      "",
      "  function applyActionResponse(actionResponse) {",
      "    (actionResponse.patches || []).forEach((patch) => {",
      "      const currentRegion = document.getElementById(patch.id);",
      "      if (!currentRegion || typeof patch.html !== 'string') {",
      "        return;",
      "      }",
      "      const replacementTemplate = document.createElement('template');",
      "      replacementTemplate.innerHTML = patch.html;",
      "      const replacementRegion = replacementTemplate.content.firstElementChild;",
      "      if (replacementRegion) {",
      "        currentRegion.replaceWith(replacementRegion);",
      "      }",
      "    });",
      "    if (actionResponse.focusId) {",
      "      document.getElementById(actionResponse.focusId)?.focus();",
      "    }",
      "  }",
      "",
      "  async function dispatchCapturedAction(capturedAction) {",
      "    const actionUrl = new URL(capturedAction.action, window.location.href);",
      "    if (actionUrl.origin !== window.location.origin) {",
      "      return;",
      "    }",
      "    const body = new URLSearchParams(capturedAction.fields || []).toString();",
      "    const response = await window.fetch(actionUrl, {",
      "      method: capturedAction.method || 'POST',",
      "      credentials: 'same-origin',",
      "      headers: {",
      "        'Accept': 'application/json',",
      "        'Content-Type': 'application/x-www-form-urlencoded;charset=UTF-8',",
      "        'X-Harch-Action': '1',",
      "      },",
      "      body,",
      "    });",
      "    if (!response.ok && response.status >= 500) {",
      "      return;",
      "    }",
      "    applyActionResponse(await response.json());",
      "  }",
      "",
      "  function drainCapturedActions() {",
      "    const captureKernel = window.__harchCaptureKernel;",
      "    if (!captureKernel) {",
      "      return;",
      "    }",
      "    captureKernel.drain().forEach((capturedEvent) => {",
      "      if (capturedEvent.type === 'submit') {",
      "        void dispatchCapturedAction(capturedEvent);",
      "      }",
      "    });",
      "  }",
      "",
      "  function isPlainLeftClick(event) {",
      "    return event.button === 0 && !event.metaKey && !event.ctrlKey && !event.shiftKey && !event.altKey;",
      "  }",
      "",
      "  function toAbsoluteUrl(targetUrl) {",
      "    return new URL(targetUrl, window.location.href);",
      "  }",
      "",
      "  function isSameOriginNavigationLink(anchor) {",
      "    if (!anchor || anchor.target || anchor.hasAttribute('download')) {",
      "      return false;",
      "    }",
      "",
      "    const absoluteUrl = toAbsoluteUrl(anchor.href);",
      "    return absoluteUrl.origin === window.location.origin;",
      "  }",
      "",
      "  function syncBodyAttributes(nextBody) {",
      "    const currentBody = document.body;",
      "    const nextAttributes = new Map(Array.from(nextBody.attributes, (attribute) => [attribute.name, attribute.value]));",
      "",
      "    Array.from(currentBody.attributes).forEach((attribute) => {",
      "      if (!nextAttributes.has(attribute.name)) {",
      "        currentBody.removeAttribute(attribute.name);",
      "      }",
      "    });",
      "",
      "    nextAttributes.forEach((value, name) => {",
      "      currentBody.setAttribute(name, value);",
      "    });",
      "  }",
      "",
      "  function applyFetchedDocument(responseText, targetUrl, shouldPushState) {",
      "    const parsedDocument = new DOMParser().parseFromString(responseText, 'text/html');",
      "    const nextTitle = parsedDocument.querySelector('title');",
      "    const nextNavigationRegion = parsedDocument.querySelector(navigationRegionSelector);",
      "    const nextNavigationContent = parsedDocument.querySelector(navigationContentSelector);",
      "    const currentNavigationRegion = document.querySelector(navigationRegionSelector);",
      "    const currentNavigationContent = document.querySelector(navigationContentSelector);",
      "",
      "    if (!nextTitle || !nextNavigationRegion || !nextNavigationContent || !currentNavigationRegion || !currentNavigationContent) {",
      "      return false;",
      "    }",
      "",
      "    document.title = nextTitle.textContent || document.title;",
      "    currentNavigationRegion.replaceWith(nextNavigationRegion);",
      "    currentNavigationContent.replaceWith(nextNavigationContent);",
      "    syncBodyAttributes(parsedDocument.body);",
      "",
      "    if (shouldPushState) {",
      "      window.history.pushState({ path: targetUrl }, '', targetUrl);",
      "    }",
      "",
      "    return true;",
      "  }",
      "",
      "  async function navigateTo(targetUrl, shouldPushState) {",
      "    if (navigationInFlight) {",
      "      return;",
      "    }",
      "",
      "    navigationInFlight = true;",
      "",
      "    try {",
      "      const response = await window.fetch(targetUrl, {",
      "        credentials: 'same-origin',",
      "        headers: {",
      "          'X-Requested-With': 'tiny-navigation',",
      "        },",
      "      });",
      "",
      "      if (!response.ok) {",
      "        window.location.assign(targetUrl);",
      "        return;",
      "      }",
      "",
      "      const responseText = await response.text();",
      "      if (!applyFetchedDocument(responseText, targetUrl, shouldPushState)) {",
      "        window.location.assign(targetUrl);",
      "      }",
      "    } catch (_error) {",
      "      window.location.assign(targetUrl);",
      "    } finally {",
      "      navigationInFlight = false;",
      "    }",
      "  }",
      "",
      "  function handleDocumentClick(event) {",
      "    if (event.defaultPrevented || !isPlainLeftClick(event)) {",
      "      return;",
      "    }",
      "",
      "    const anchor = event.target.closest(pageLinkSelector);",
      "    if (!isSameOriginNavigationLink(anchor)) {",
      "      return;",
      "    }",
      "",
      "    event.preventDefault();",
      "    void navigateTo(anchor.href, true);",
      "  }",
      "",
      "  function handlePopState() {",
      "    void navigateTo(window.location.href, false);",
      "  }",
      "",
      "  document.addEventListener('click', handleDocumentClick);",
      "  window.addEventListener('popstate', handlePopState);",
      "  window.addEventListener('harch:capture', drainCapturedActions);",
      "  drainCapturedActions();",
      "})();"
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
      documentBootstrapHooks = pageBootstrapHooks page,
      documentStylesheets = shellStylesheets shell,
      documentRuntimeDescriptors = shellRuntimeDescriptors shell
    }

renderDocument :: Document route -> Text
renderDocument = renderDocumentWithNonce (RuntimeNonce "development-render-nonce")

renderDocumentWithNonce :: RuntimeNonce -> Document route -> Text
renderDocumentWithNonce runtimeNonce document =
  Text.concat
    [ "<html><head><title>",
      documentTitle document,
      "</title>",
      renderStylesheets (documentStylesheets document),
      renderRuntimeDescriptors runtimeNonce (documentRuntimeDescriptors document),
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

buildPageShell :: (Eq route) => RouteCodec route context -> PageShell route context -> Page route context -> Document route
buildPageShell = buildDocument

renderStylesheets :: [Stylesheet] -> Text
renderStylesheets =
  Text.concat
    . map
      ( \Stylesheet {stylesheetAsset = AssetPath assetPath} ->
          "<link rel=\"stylesheet\" href=\"" <> assetPath <> "\">"
      )

matchRoute :: RouteCodec route context -> context -> Text -> RouteRequest route context
matchRoute codec context path = fromMaybe (notFoundRequest codec context) (parseRoute codec context path)

toWaiApplication :: (Eq route) => Application route context -> Wai.Application
toWaiApplication webApplication request respond = do
  requestStartedAt <- getMonotonicTimeNSec
  let requestPolicyConfig = applicationRequestPolicy webApplication
      policyResponseHeaders = requestPolicyResponseHeaders requestPolicyConfig request
      requestPath = waiRequestPath requestPolicyConfig request
  policyEvaluatedAt <- policyResponseHeaders `seq` getMonotonicTimeNSec
  earlyResult <- runExceptT (evaluateEarlyRequestStages webApplication requestPolicyConfig policyResponseHeaders request requestPath)
  case earlyResult of
    Left EarlyResponse {earlyResponsePath, earlyResponseValue} -> do
      responseReportedAt <- earlyResponseValue `seq` getMonotonicTimeNSec
      reportEarlyRequestObservability webApplication request requestStartedAt responseReportedAt earlyResponsePath earlyResponseValue
      respond earlyResponseValue
    Right () ->
      handleRoutedRequest webApplication request respond requestStartedAt policyEvaluatedAt requestPolicyConfig requestPath

data EarlyResponse = EarlyResponse
  { earlyResponsePath :: Text,
    earlyResponseValue :: Wai.Response
  }

evaluateEarlyRequestStages :: Application route context -> RequestPolicyConfig -> [Http.Header] -> Wai.Request -> Text -> ExceptT EarlyResponse IO ()
evaluateEarlyRequestStages webApplication requestPolicyConfig policyResponseHeaders request requestPath = do
  for_ (corsPreflightResponse requestPolicyConfig request) $ \response ->
    throwEarly (externalRequestPath requestPolicyConfig request) response
  for_ (requestRedirectLocation requestPolicyConfig request) $ \redirectLocation ->
    throwEarly (externalRequestPath requestPolicyConfig request) (httpsRedirectResponse redirectLocation)
  for_ (applicationNavigationRuntime webApplication >>= (`navigationRuntimeResponse` requestPath)) $ \runtimeResponseBody ->
    throwEarly requestPath (toWaiBodyResponse [] runtimeResponseBody)
  maybeStaticResponse <- liftIO (serveStaticAssetResponse (applicationStaticAssets webApplication) requestPath)
  for_ maybeStaticResponse $ \(staticRoutePath, staticResponse) ->
    throwEarly (applyRequestPathPrefix (requestPathPrefix requestPolicyConfig request) staticRoutePath) staticResponse
  where
    throwEarly path = throwError . EarlyResponse path . applyResponseHeaders policyResponseHeaders

handleRoutedRequest :: (Eq route) => Application route context -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> Word64 -> Word64 -> RequestPolicyConfig -> Text -> IO Wai.ResponseReceived
handleRoutedRequest webApplication request respond requestStartedAt policyEvaluatedAt requestPolicyConfig requestPath = do
  middlewareStartedAt <- getMonotonicTimeNSec
  middlewareResult <- runRequestMiddlewarePipeline (applicationRequestMiddleware webApplication) request (requestContextFromRequest webApplication request (defaultRequestContext webApplication))
  middlewareCompletedAt <- middlewareResult `seq` getMonotonicTimeNSec
  let requestContext = middlewareResultContext middlewareResult
      middlewareTiming = middlewareTimingEntry webApplication middlewareStartedAt middlewareCompletedAt
  routeMatchingStartedAt <- getMonotonicTimeNSec
  let routeRequest = matchRoute (routeCodec webApplication) requestContext (waiRequestRouteTarget requestPolicyConfig request)
  routeMatchedAt <- routeRequest `seq` getMonotonicTimeNSec
  renderStartedAt <- getMonotonicTimeNSec
  response <- dispatchRoutedRequest webApplication request requestPath routeRequest middlewareResult
  responseRenderedAt <- response `seq` getMonotonicTimeNSec
  runtimeNonce <- responseRuntimeNonce response
  finalizeRoutedResponse webApplication request respond requestStartedAt policyEvaluatedAt middlewareTiming routeMatchingStartedAt routeMatchedAt renderStartedAt responseRenderedAt requestPolicyConfig requestPath routeRequest runtimeNonce response

middlewareResultContext :: MiddlewareResult context -> context
middlewareResultContext middlewareResult =
  case middlewareResult of
    ContinueMiddleware requestContext -> requestContext
    HaltMiddleware requestContext _ -> requestContext

middlewareTimingEntry :: Application route context -> Word64 -> Word64 -> [(Text, Word64, Word64)]
middlewareTimingEntry webApplication startedAt completedAt =
  case applicationRequestMiddleware webApplication of
    [] -> []
    _ -> [("middleware", startedAt, completedAt)]

dispatchRoutedRequest :: Application route context -> Wai.Request -> Text -> RouteRequest route context -> MiddlewareResult context -> IO (Response route context)
dispatchRoutedRequest _ _ _ _ (HaltMiddleware _ responseBody) = pure (BodyResponse responseBody)
dispatchRoutedRequest webApplication request requestPath routeRequest@RouteRequest {requestContext = routedRequestContext} (ContinueMiddleware _) =
  if isClientActionRequest request
    then do
      requestBody <- Wai.strictRequestBody request
      let actionFields = parseClientActionFields requestBody
      maybeActionResponse <- handleClientAction webApplication ClientActionRequest {clientActionMethod = TextEncoding.decodeUtf8 (Wai.requestMethod request), clientActionPath = requestPath, clientActionFields = actionFields, clientActionCsrfToken = lookup "_csrf" actionFields, clientActionContext = routedRequestContext}
      maybe (renderResponse webApplication routeRequest) (pure . ClientActionBodyResponse) maybeActionResponse
    else renderResponse webApplication routeRequest

responseRuntimeNonce :: Response route context -> IO RuntimeNonce
responseRuntimeNonce response =
  case response of
    PageResponse _ -> generateRuntimeNonce
    PageResponseWithMetadata _ _ -> generateRuntimeNonce
    BodyResponse _ -> pure $! RuntimeNonce ""
    RedirectResponse _ _ -> pure $! RuntimeNonce ""
    ClientActionBodyResponse _ -> pure $! RuntimeNonce ""

finalizeRoutedResponse :: (Eq route) => Application route context -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> Word64 -> Word64 -> [(Text, Word64, Word64)] -> Word64 -> Word64 -> Word64 -> Word64 -> RequestPolicyConfig -> Text -> RouteRequest route context -> RuntimeNonce -> Response route context -> IO Wai.ResponseReceived
finalizeRoutedResponse webApplication request respond requestStartedAt policyEvaluatedAt middlewareTiming routeMatchingStartedAt routeMatchedAt renderStartedAt responseRenderedAt requestPolicyConfig requestPath routeRequest runtimeNonce response = do
  let requestLogFields = requestLogContextFields requestPolicyConfig request
      diagnosticValues = responseDiagnostics response
      contextualizedLogs = map (prependRequestLogContext requestLogFields) (diagnosticLogEntries diagnosticValues)
      observabilityValue = buildRoutedRequestObservability webApplication request requestStartedAt policyEvaluatedAt middlewareTiming routeMatchingStartedAt routeMatchedAt renderStartedAt responseRenderedAt requestPolicyConfig requestPath routeRequest response diagnosticValues
  Observability.forceRequestObservability observabilityValue `seq`
    reportRequestObservability webApplication observabilityValue
      >> mapM_ (reportApplicationLog webApplication) contextualizedLogs
      >> respond (applyResponseHeaders (responsePolicyHeaders requestPolicyConfig request runtimeNonce response) (toWaiResponse [] runtimeNonce webApplication response))

buildRoutedRequestObservability :: (Eq route) => Application route context -> Wai.Request -> Word64 -> Word64 -> [(Text, Word64, Word64)] -> Word64 -> Word64 -> Word64 -> Word64 -> RequestPolicyConfig -> Text -> RouteRequest route context -> Response route context -> ResponseDiagnostics -> Observability.RequestObservability
buildRoutedRequestObservability webApplication request requestStartedAt policyEvaluatedAt middlewareTiming routeMatchingStartedAt routeMatchedAt renderStartedAt responseRenderedAt requestPolicyConfig requestPath routeRequest response diagnosticValues =
  maybe id Observability.withRequestTraceContext (requestTraceContext request) $
    Observability.buildRequestObservability
      (TextEncoding.decodeUtf8 (Wai.requestMethod request))
      (requestScheme requestPolicyConfig request)
      requestPath
      (renderRoute (routeCodec webApplication) routeRequest)
      (responseStatusCode webApplication response)
      (responseKind response)
      ( requestContextObservabilityAttributes requestPolicyConfig request
          <> diagnosticObservabilityAttributes diagnosticValues
          <> requestTimingObservabilityAttributes requestStartedAt responseRenderedAt ([("request-policy", requestStartedAt, policyEvaluatedAt)] <> middlewareTiming <> [("route-match", routeMatchingStartedAt, routeMatchedAt), ("render-response", renderStartedAt, responseRenderedAt)])
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
          connectionReporter = reportConnectionObservability webApplication
      case runtimeStartupValidationError startupPlan of
        Just runtimeError ->
          ioError (userError runtimeError)
        Nothing ->
          connectionReporter `seq`
            observabilityPlan `seq`
              bracket
                (startHttpRuntimeServers (httpEndpoints (httpBindPlan startupPlan)) runtimeApplication)
                stopRuntimeServers
                ( \httpServers ->
                    bracket
                      (startAcmeRuntimeServers (runtimeAcmeBindPlans startupPlan) runtimeApplication connectionReporter (reportApplicationLog webApplication))
                      stopAcmeRuntimeServers
                      ( \acmeServers ->
                          bracket
                            (startManualTlsRuntimeServers (manualTlsBindPlans startupPlan) runtimeApplication connectionReporter)
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
  let listenerScheme = Http
      endpoint = ListenerEndpoint {endpointHost = "127.0.0.1", endpointPort = localPort}
  serverThreadId <-
    listenerSchemeText listenerScheme `seq`
      endpointHost endpoint `seq`
        startWarpServerOnSocket endpoint listeningSocket (toWaiApplication webApplication)
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

data ActiveConnectionAddresses = ActiveConnectionAddresses
  { pendingConnectionAddresses :: MVar [Socket.SockAddr],
    activeConnectionAddresses :: IORef [(ThreadId, Socket.SockAddr)]
  }

data RunningAcmeRuntimeServer = RunningAcmeRuntimeServer
  { runningAcmeRuntimeServer :: Maybe RunningRuntimeServer,
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
    runtimeAcmeTlsEndpoint :: Maybe ListenerEndpoint,
    runtimeAcmeListenerConfig :: AcmeConfig
  }

runtimeAcmeBindPlans :: ServerStartupPlan -> [RuntimeAcmeBindPlan]
runtimeAcmeBindPlans startupPlan =
  [ RuntimeAcmeBindPlan
      { runtimeAcmeEndpoint = acmeEndpoint acmePlan,
        runtimeAcmeTlsEndpoint = acmeTlsEndpoint acmePlan,
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

startManualTlsRuntimeServers :: [ManualTlsBindPlan] -> Wai.Application -> (Observability.ConnectionObservability -> IO ()) -> IO [RunningRuntimeServer]
startManualTlsRuntimeServers manualTlsPlans waiApplication connectionReporter =
  connectionReporter `seq` go [] manualTlsPlans
  where
    go runningServers remainingPlans =
      case remainingPlans of
        [] -> pure (reverse runningServers)
        manualTlsPlan : remaining ->
          ( do
              runningServer <- startManualTlsRuntimeServer manualTlsPlan waiApplication connectionReporter
              go (runningServer : runningServers) remaining
                `onException` stopRuntimeServers (runningServer : runningServers)
          )
            `onException` stopRuntimeServers runningServers

startAcmeRuntimeServers :: [RuntimeAcmeBindPlan] -> Wai.Application -> (Observability.ConnectionObservability -> IO ()) -> (Text -> IO ()) -> IO [RunningAcmeRuntimeServer]
startAcmeRuntimeServers acmePlans waiApplication connectionReporter applicationLogger =
  connectionReporter `seq` applicationLogger `seq` go [] acmePlans
  where
    go runningServers remainingPlans =
      case remainingPlans of
        [] -> pure (reverse runningServers)
        acmePlan : remaining ->
          ( do
              runningServer <- startAcmeRuntimeServer acmePlan waiApplication connectionReporter applicationLogger
              go (runningServer : runningServers) remaining
                `onException` stopAcmeRuntimeServers (runningServer : runningServers)
          )
            `onException` stopAcmeRuntimeServers runningServers

startHttpRuntimeServer :: ListenerEndpoint -> Wai.Application -> IO RunningRuntimeServer
startHttpRuntimeServer endpoint waiApplication = do
  listeningSocket <- openListenerSocket endpoint
  serverThreadId <-
    startWarpServerOnSocket endpoint listeningSocket waiApplication
  endpoint `seq`
    pure
      RunningRuntimeServer
        { runningRuntimeSocket = listeningSocket,
          runningRuntimeThreadId = serverThreadId
        }

startManualTlsRuntimeServer :: ManualTlsBindPlan -> Wai.Application -> (Observability.ConnectionObservability -> IO ()) -> IO RunningRuntimeServer
startManualTlsRuntimeServer =
  startManualTlsRuntimeServerWithStarter startWarpTlsServerOnSocket

startManualTlsRuntimeServerWithStarter :: (ListenerEndpoint -> WarpTLS.TLSSettings -> Socket.Socket -> (Observability.ConnectionObservability -> IO ()) -> Wai.Application -> IO ThreadId) -> ManualTlsBindPlan -> Wai.Application -> (Observability.ConnectionObservability -> IO ()) -> IO RunningRuntimeServer
startManualTlsRuntimeServerWithStarter startTlsServer manualTlsPlan waiApplication connectionReporter = do
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
    connectionReporter `seq`
      startTlsServer endpoint tlsSettings listeningSocket connectionReporter waiApplication
        `onException` Socket.close listeningSocket
  manualTlsPlan `seq`
    pure
      RunningRuntimeServer
        { runningRuntimeSocket = listeningSocket,
          runningRuntimeThreadId = serverThreadId
        }

startAcmeRuntimeServer :: RuntimeAcmeBindPlan -> Wai.Application -> (Observability.ConnectionObservability -> IO ()) -> (Text -> IO ()) -> IO RunningAcmeRuntimeServer
startAcmeRuntimeServer runtimeAcmePlan waiApplication connectionReporter applicationLogger = do
  let certbotConfig = acmeCertbotConfig (runtimeAcmeListenerConfig runtimeAcmePlan)
  (maybeManualTlsPlan, cleanupDirectory) <-
    prepareCertbotManualTlsBindPlanWithLogger applicationLogger runtimeAcmePlan certbotConfig
  maybeRunningServer <-
    connectionReporter `seq`
      traverse (\manualTlsPlan -> startManualTlsRuntimeServer manualTlsPlan waiApplication connectionReporter) maybeManualTlsPlan
        `onException` removePathForcibly cleanupDirectory
  pure
    RunningAcmeRuntimeServer
      { runningAcmeRuntimeServer = maybeRunningServer,
        runningAcmeCleanupDirectory = cleanupDirectory
      }

runtimeAcmeManualTlsBindPlan :: RuntimeAcmeBindPlan -> FilePath -> FilePath -> Maybe ManualTlsBindPlan
runtimeAcmeManualTlsBindPlan runtimeAcmePlan resolvedCertificatePath resolvedPrivateKeyPath =
  fmap
    ( \tlsListenerEndpoint ->
        ManualTlsBindPlan
          { tlsEndpoint = tlsListenerEndpoint,
            tlsCertificateFile = resolvedCertificatePath,
            tlsPrivateKeyFile = resolvedPrivateKeyPath,
            tlsCredentialSourceKind = ManualTlsCredentials,
            tlsStartupMode = RequireCertificateFiles
          }
    )
    (runtimeAcmeTlsEndpoint runtimeAcmePlan)

prepareCertbotManualTlsBindPlan :: RuntimeAcmeBindPlan -> CertbotConfig -> IO (Maybe ManualTlsBindPlan, FilePath)
prepareCertbotManualTlsBindPlan =
  prepareCertbotManualTlsBindPlanWithLogger ignoreTextLog

prepareCertbotManualTlsBindPlanWithLogger :: (Text -> IO ()) -> RuntimeAcmeBindPlan -> CertbotConfig -> IO (Maybe ManualTlsBindPlan, FilePath)
prepareCertbotManualTlsBindPlanWithLogger applicationLogger runtimeAcmePlan certbotConfig = do
  let endpointText = Text.pack (renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan))
  tempDirectory <- getCanonicalTemporaryDirectory
  stateDirectory <- createTempDirectory tempDirectory "harch-web-certbot"
  let configDirectory = stateDirectory </> "config"
      workDirectory = stateDirectory </> "work"
      logsDirectory = stateDirectory </> "logs"
      webrootDirectory = stateDirectory </> "webroot"
  mapM_
    (createDirectoryIfMissing True)
    [configDirectory, workDirectory, logsDirectory, webrootDirectory </> ".well-known" </> "acme-challenge"]
  certificateName <-
    either
      (ioError . userError)
      pure
      (certbotCertificateName runtimeAcmePlan)
  bracket_
    ( applicationLogger ("ACME certbot webroot registered for listener " <> endpointText)
        >> registerCertbotAcmeChallengeWebroot webrootDirectory
    )
    ( unregisterCertbotAcmeChallengeWebroot webrootDirectory
        >> applicationLogger ("ACME certbot webroot unregistered for listener " <> endpointText)
    )
    (runCertbotAcmeChallengeWithLogger applicationLogger runtimeAcmePlan certbotConfig stateDirectory configDirectory workDirectory logsDirectory webrootDirectory)
  let certificateDirectory = configDirectory </> "live" </> Text.unpack certificateName
      certificatePath = certificateDirectory </> "fullchain.pem"
      privateKeyPath = certificateDirectory </> "privkey.pem"
  ensureRuntimeFileExists "Certbot ACME certificate file does not exist: " certificatePath
  ensureRuntimeFileExists "Certbot ACME private key file does not exist: " privateKeyPath
  (resolvedCertificatePath, resolvedPrivateKeyPath) <-
    case acmeCertificateDirectory (runtimeAcmeListenerConfig runtimeAcmePlan) of
      Nothing ->
        pure (certificatePath, privateKeyPath)
      Just sharedDirectory -> do
        publishedPaths <- publishCertificateFiles sharedDirectory certificatePath privateKeyPath
        applicationLogger ("Published ACME certificate files to shared directory " <> Text.pack sharedDirectory)
        pure publishedPaths
  pure
    ( runtimeAcmeManualTlsBindPlan runtimeAcmePlan resolvedCertificatePath resolvedPrivateKeyPath,
      stateDirectory
    )

runCertbotAcmeChallengeWithLogger :: (Text -> IO ()) -> RuntimeAcmeBindPlan -> CertbotConfig -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> IO ()
runCertbotAcmeChallengeWithLogger applicationLogger runtimeAcmePlan certbotConfig stateDirectory configDirectory workDirectory logsDirectory webrootDirectory = do
  let endpointText = Text.pack (renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan))
  applicationLogger ("Launching certbot for ACME listener on " <> endpointText)
  let commandArguments =
        certbotRuntimeArguments runtimeAcmePlan certbotConfig configDirectory workDirectory logsDirectory webrootDirectory
  processResult <-
    try (readCreateProcessWithExitCode (proc (certbotExecutable certbotConfig) commandArguments) "") ::
      IO (Either IOException (ExitCode, String, String))
  case processResult of
    Left launchError -> do
      applicationLogger ("Failed to launch certbot for ACME listener on " <> endpointText <> ": " <> Text.pack (show launchError))
      ioError . userError $
        "Failed to launch certbot for ACME listener on "
          <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
          <> ": "
          <> show launchError
    Right (ExitSuccess, stdoutText, stderrText) -> do
      void (evaluate (length stdoutText + length stderrText))
    Right (exitCode, stdoutText, stderrText) -> do
      applicationLogger ("Certbot failed for ACME listener on " <> endpointText <> " with exit code " <> Text.pack (show exitCode))
      diagnostics <- certbotFailureDiagnostics stateDirectory logsDirectory
      ioError . userError $
        "Certbot failed for ACME listener on "
          <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
          <> " with exit code "
          <> show exitCode
          <> ".\nstdout:\n"
          <> stdoutText
          <> "\nstderr:\n"
          <> stderrText
          <> diagnostics

ignoreTextLog :: Text -> IO ()
ignoreTextLog textValue = void (evaluate (Text.length textValue))

certbotFailureDiagnostics :: FilePath -> FilePath -> IO String
certbotFailureDiagnostics stateDirectory logsDirectory = do
  let logPath = logsDirectory </> "letsencrypt.log"
  logExists <- doesFileExist logPath
  if logExists
    then do
      logText <- readFile logPath
      _ <- evaluate (length logText)
      pure $
        "\nCertbot state directory was preserved for inspection: "
          <> stateDirectory
          <> "\nletsencrypt.log tail:\n"
          <> tailTextLines 80 logText
    else
      pure $
        "\nCertbot state directory was preserved for inspection: "
          <> stateDirectory
          <> "\nNo certbot logfile was found at "
          <> logPath
          <> ".\n"

tailTextLines :: Int -> String -> String
tailTextLines lineCount textValue =
  unlines (drop (max 0 (length textLines - lineCount)) textLines)
  where
    textLines = lines textValue

certbotRuntimeArguments :: RuntimeAcmeBindPlan -> CertbotConfig -> FilePath -> FilePath -> FilePath -> FilePath -> [String]
certbotRuntimeArguments runtimeAcmePlan certbotConfig configDirectory workDirectory logsDirectory webrootDirectory =
  map Text.unpack (certbotCommandArguments certbotConfig)
    <> map Text.unpack (certbotArguments certbotConfig)
    <> certbotNonInteractiveArguments certbotConfig
    <> certbotAgreeTosArguments certbotConfig
    <> certbotAuthenticatorArguments certbotConfig
    <> certbotWebrootPathArguments certbotConfig webrootDirectory
    <> ["--config-dir", configDirectory, "--work-dir", workDirectory, "--logs-dir", logsDirectory]
    <> certbotHttp01PortArguments runtimeAcmePlan
    <> certbotDirectoryUrlArguments runtimeAcmePlan
    <> certbotContactEmailArguments runtimeAcmePlan certbotConfig
    <> certbotDomainArguments runtimeAcmePlan certbotConfig

certbotCommandArguments :: CertbotConfig -> [Text]
certbotCommandArguments certbotConfig =
  [ "certonly"
  | "certonly" `notElem` certbotArguments certbotConfig
  ]

certbotNonInteractiveArguments :: CertbotConfig -> [String]
certbotNonInteractiveArguments certbotConfig =
  [ "--non-interactive"
  | not (any (`certbotHasFlag` certbotArguments certbotConfig) ["--non-interactive", "-n"])
  ]

certbotAgreeTosArguments :: CertbotConfig -> [String]
certbotAgreeTosArguments certbotConfig =
  ["--agree-tos" | not (certbotHasFlag "--agree-tos" (certbotArguments certbotConfig))]

certbotAuthenticatorArguments :: CertbotConfig -> [String]
certbotAuthenticatorArguments certbotConfig =
  ["--webroot" | certbotNeedsDerivedWebrootAuthenticator (certbotArguments certbotConfig)]

certbotWebrootPathArguments :: CertbotConfig -> FilePath -> [String]
certbotWebrootPathArguments certbotConfig webrootDirectory =
  if certbotShouldUseWebroot (certbotArguments certbotConfig)
    && not (certbotHasOption "-w" (certbotArguments certbotConfig) || certbotHasOption "--webroot-path" (certbotArguments certbotConfig))
    then ["--webroot-path", webrootDirectory]
    else []

certbotHttp01PortArguments :: RuntimeAcmeBindPlan -> [String]
certbotHttp01PortArguments runtimeAcmePlan =
  if certbotHasOption "--http-01-port" (runtimeCertbotArguments runtimeAcmePlan)
    || certbotShouldUseWebroot (runtimeCertbotArguments runtimeAcmePlan)
    then []
    else ["--http-01-port", show (acmeHttp01Port (runtimeAcmeListenerConfig runtimeAcmePlan))]

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
  let certbotConfig = acmeCertbotConfig (runtimeAcmeListenerConfig runtimeAcmePlan)
   in certbotArguments certbotConfig

toRuntimeWaiApplication :: (Eq route) => AcmeChallengeStore -> Application route context -> Wai.Application
toRuntimeWaiApplication challengeStore webApplication request respond = do
  requestStartedAt <- getMonotonicTimeNSec
  let requestPolicyConfig = applicationRequestPolicy webApplication
  maybeChallengeResponse <- acmeChallengeResponseForRequest requestPolicyConfig challengeStore request
  case maybeChallengeResponse of
    Just challengeResponse -> do
      challengeResponseReportedAt <- challengeResponse `seq` getMonotonicTimeNSec
      reportEarlyRequestObservability
        webApplication
        request
        requestStartedAt
        challengeResponseReportedAt
        (acmeChallengeRoutePath requestPolicyConfig request)
        challengeResponse
      respond challengeResponse
    Nothing -> toWaiApplication webApplication request respond

acmeChallengeResponseForRequest :: RequestPolicyConfig -> AcmeChallengeStore -> Wai.Request -> IO (Maybe Wai.Response)
acmeChallengeResponseForRequest requestPolicyConfig (AcmeChallengeStore challengeStore) request = do
  challenges <- readMVar challengeStore
  case fmap
    ( Wai.responseLBS
        Http.ok200
        [("Content-Type", "text/plain; charset=utf-8")]
        . LazyByteString.fromStrict
        . TextEncoding.encodeUtf8
        . activeAcmeChallengeResponse
    )
    (find (matchesRuntimeAcmeChallenge requestPolicyConfig request) challenges) of
    Just challengeResponse ->
      pure (Just challengeResponse)
    Nothing ->
      certbotAcmeChallengeResponseForRequest requestPolicyConfig request

matchesRuntimeAcmeChallenge :: RequestPolicyConfig -> Wai.Request -> ActiveAcmeChallenge -> Bool
matchesRuntimeAcmeChallenge requestPolicyConfig request challenge =
  case acmeHttp01ChallengeToken requestPolicyConfig request of
    Just challengeToken ->
      challengeToken == activeAcmeChallengeToken challenge
        && maybe True (== activeAcmeChallengeDomain challenge) (requestHostWithoutPort request)
    Nothing -> False

acmeHttp01ChallengeToken :: RequestPolicyConfig -> Wai.Request -> Maybe Text
acmeHttp01ChallengeToken requestPolicyConfig request =
  Text.stripPrefix "/.well-known/acme-challenge/" (waiRequestPath requestPolicyConfig request)

acmeChallengeRoutePath :: RequestPolicyConfig -> Wai.Request -> Text
acmeChallengeRoutePath requestPolicyConfig request =
  applyRequestPathPrefix
    (requestPathPrefix requestPolicyConfig request)
    "/.well-known/acme-challenge/*"

requestHostWithoutPort :: Wai.Request -> Maybe Text
requestHostWithoutPort request =
  fmap (Text.takeWhile (/= ':')) (requestHeaderToken "Host" request)

registerAcmeChallenges :: AcmeChallengeStore -> [ActiveAcmeChallenge] -> IO ()
registerAcmeChallenges (AcmeChallengeStore challengeStore) newChallenges =
  modifyMVar_ challengeStore (pure . (newChallenges <>))

unregisterAcmeChallenges :: AcmeChallengeStore -> [ActiveAcmeChallenge] -> IO ()
unregisterAcmeChallenges (AcmeChallengeStore challengeStore) completedChallenges =
  modifyMVar_ challengeStore (pure . filter (not . (`sameActiveAcmeChallengeAny` completedChallenges)))

{-# NOINLINE certbotAcmeChallengeWebrootDirectories #-}
certbotAcmeChallengeWebrootDirectories :: MVar [FilePath]
certbotAcmeChallengeWebrootDirectories =
  unsafePerformIO (newMVar [])

registerCertbotAcmeChallengeWebroot :: FilePath -> IO ()
registerCertbotAcmeChallengeWebroot webrootDirectory =
  modifyMVar_ certbotAcmeChallengeWebrootDirectories (pure . (webrootDirectory :))

unregisterCertbotAcmeChallengeWebroot :: FilePath -> IO ()
unregisterCertbotAcmeChallengeWebroot webrootDirectory =
  modifyMVar_ certbotAcmeChallengeWebrootDirectories (pure . filter (/= webrootDirectory))

certbotAcmeChallengeResponseForRequest :: RequestPolicyConfig -> Wai.Request -> IO (Maybe Wai.Response)
certbotAcmeChallengeResponseForRequest requestPolicyConfig request =
  case acmeHttp01ChallengeToken requestPolicyConfig request >>= validAcmeHttp01ChallengeToken of
    Nothing ->
      pure Nothing
    Just challengeToken -> do
      webrootDirectories <- readMVar certbotAcmeChallengeWebrootDirectories
      maybeChallengeFile <-
        firstExistingFile
          [ webrootDirectory </> ".well-known" </> "acme-challenge" </> Text.unpack challengeToken
          | webrootDirectory <- webrootDirectories
          ]
      pure
        ( fmap
            (\challengeFile -> Wai.responseFile Http.ok200 [("Content-Type", "text/plain; charset=utf-8")] challengeFile Nothing)
            maybeChallengeFile
        )

validAcmeHttp01ChallengeToken :: Text -> Maybe Text
validAcmeHttp01ChallengeToken challengeToken
  | Text.null challengeToken = Nothing
  | Text.any (\character -> character == '/' || character == '\\') challengeToken = Nothing
  | challengeToken == "." || challengeToken == ".." = Nothing
  | Text.isInfixOf ".." challengeToken = Nothing
validAcmeHttp01ChallengeToken challengeToken = Just challengeToken

firstExistingFile :: [FilePath] -> IO (Maybe FilePath)
firstExistingFile candidatePaths =
  case candidatePaths of
    [] ->
      pure Nothing
    candidatePath : remainingPaths -> do
      candidateExists <- doesFileExist candidatePath
      if candidateExists
        then pure (Just candidatePath)
        else firstExistingFile remainingPaths

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

certbotHasFlag :: Text -> [Text] -> Bool
certbotHasFlag =
  elem

certbotNeedsDerivedWebrootAuthenticator :: [Text] -> Bool
certbotNeedsDerivedWebrootAuthenticator configuredArguments =
  not (certbotHasExplicitAuthenticator configuredArguments)
    || (certbotHasWebrootPathOption configuredArguments && not (certbotUsesWebrootFlagOrAuthenticator configuredArguments))

certbotShouldUseWebroot :: [Text] -> Bool
certbotShouldUseWebroot configuredArguments =
  certbotNeedsDerivedWebrootAuthenticator configuredArguments
    || certbotUsesWebroot configuredArguments

certbotHasExplicitAuthenticator :: [Text] -> Bool
certbotHasExplicitAuthenticator configuredArguments =
  certbotUsesWebroot configuredArguments
    || any (`certbotHasFlag` configuredArguments) ["--standalone", "--manual", "--apache", "--nginx"]
    || any ("--dns-" `Text.isPrefixOf`) configuredArguments
    || any isExplicitAuthenticator (certbotAuthenticatorValues configuredArguments)

certbotAuthenticatorValues :: [Text] -> [Text]
certbotAuthenticatorValues configuredArguments =
  certbotOptionValues "-a" configuredArguments
    <> certbotOptionValues "--authenticator" configuredArguments

isExplicitAuthenticator :: Text -> Bool
isExplicitAuthenticator authenticator =
  authenticator `elem` ["standalone", "manual", "apache", "nginx"]
    || "dns-" `Text.isPrefixOf` authenticator

certbotUsesWebroot :: [Text] -> Bool
certbotUsesWebroot configuredArguments =
  certbotUsesWebrootFlagOrAuthenticator configuredArguments
    || certbotHasWebrootPathOption configuredArguments

certbotUsesWebrootFlagOrAuthenticator :: [Text] -> Bool
certbotUsesWebrootFlagOrAuthenticator configuredArguments =
  certbotHasFlag "--webroot" configuredArguments
    || elem
      "webroot"
      (certbotOptionValues "-a" configuredArguments <> certbotOptionValues "--authenticator" configuredArguments)

certbotHasWebrootPathOption :: [Text] -> Bool
certbotHasWebrootPathOption configuredArguments =
  certbotHasOption "-w" configuredArguments
    || certbotHasOption "--webroot-path" configuredArguments

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
                <> " became invalid. Ensure the configured domains resolve publicly to this host and that TCP port 80 is reachable from the public internet for http-01 validation.\nbody:\n"
                <> renderAcmeResponseBody response
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
  ByteString.pack <$> decodeHexPairs cleanedHex
  where
    cleanedHex = filter (not . (`elem` [' ', '\n', '\r', '\t'])) (Text.unpack hexText)

decodeHexPairs :: String -> Either String [Word8]
decodeHexPairs [] = Right []
decodeHexPairs [_] = Left "hex string had an odd length"
decodeHexPairs (firstDigit : secondDigit : remainingDigits) =
  (:) <$> decodeHexByte firstDigit secondDigit <*> decodeHexPairs remainingDigits

decodeHexByte :: Char -> Char -> Either String Word8
decodeHexByte firstDigit secondDigit =
  case (hexDigitValue firstDigit, hexDigitValue secondDigit) of
    (Just highDigit, Just lowDigit) -> Right (fromIntegral (highDigit * 16 + lowDigit))
    _ -> Left ("invalid hex digit pair: " <> [firstDigit, secondDigit])

hexDigitValue :: Char -> Maybe Int
hexDigitValue hexDigit
  | isDigit hexDigit = Just (digitToInt hexDigit)
  | 'a' <= hexDigit && hexDigit <= 'f' = Just (digitToInt hexDigit)
  | 'A' <= hexDigit && hexDigit <= 'F' = Just (digitToInt hexDigit)
hexDigitValue _ = Nothing

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
  for_ (runningAcmeRuntimeServer runningServer) stopRuntimeServer
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

startWarpServerOnSocket :: ListenerEndpoint -> Socket.Socket -> Wai.Application -> IO ThreadId
startWarpServerOnSocket endpoint listeningSocket waiApplication =
  startWarpRuntimeServerOnSocket $ \startupSignal ->
    let settings = runtimeHttpServerSettings endpoint startupSignal
     in settings `seq` Warp.runSettingsSocket settings listeningSocket waiApplication

startWarpTlsServerOnSocket :: ListenerEndpoint -> WarpTLS.TLSSettings -> Socket.Socket -> (Observability.ConnectionObservability -> IO ()) -> Wai.Application -> IO ThreadId
startWarpTlsServerOnSocket endpoint tlsSettings listeningSocket connectionReporter waiApplication = do
  activeConnectionAddresses <- newActiveConnectionAddresses
  let listenerScheme = Https
  listenerScheme `seq`
    connectionReporter `seq`
      startWarpRuntimeServerOnSocket $ \startupSignal ->
        let settings =
              runtimeServerSettings listenerScheme endpoint startupSignal activeConnectionAddresses connectionReporter
         in settings `seq` WarpTLS.runTLSSocket tlsSettings settings listeningSocket waiApplication

startWarpRuntimeServerOnSocket :: (MVar (Either SomeException RuntimeServerReady) -> IO ()) -> IO ThreadId
startWarpRuntimeServerOnSocket runServerOnSocket = do
  startupSignal <- newEmptyMVar
  threadId <-
    forkFinally
      (runServerOnSocket startupSignal)
      (reportRuntimeServerExit startupSignal)
  _ <- waitForRuntimeServerStartup startupSignal
  pure threadId

runtimeServerSettings ::
  ListenerScheme ->
  ListenerEndpoint ->
  MVar (Either SomeException RuntimeServerReady) ->
  ActiveConnectionAddresses ->
  (Observability.ConnectionObservability -> IO ()) ->
  Warp.Settings
runtimeServerSettings listenerScheme endpoint startupSignal activeConnectionAddresses connectionReporter =
  Warp.setPort (endpointPort endpoint)
    . Warp.setOnException (runtimeConnectionExceptionReporter listenerScheme endpoint activeConnectionAddresses connectionReporter (Warp.getOnException Warp.defaultSettings))
    . Warp.setFork (forkTrackedConnection activeConnectionAddresses)
    . Warp.setOnOpen (registerActiveConnection activeConnectionAddresses)
    . Warp.setOnClose (\_ -> unregisterActiveConnection activeConnectionAddresses)
    $ Warp.setBeforeMainLoop (putMVar startupSignal (Right RuntimeServerReady)) Warp.defaultSettings

runtimeHttpServerSettings ::
  ListenerEndpoint ->
  MVar (Either SomeException RuntimeServerReady) ->
  Warp.Settings
runtimeHttpServerSettings endpoint startupSignal =
  Warp.setPort (endpointPort endpoint) $
    Warp.setBeforeMainLoop (putMVar startupSignal (Right RuntimeServerReady)) Warp.defaultSettings

newActiveConnectionAddresses :: IO ActiveConnectionAddresses
newActiveConnectionAddresses =
  ActiveConnectionAddresses
    <$> newMVar []
    <*> newIORef []

registerActiveConnection :: ActiveConnectionAddresses -> Socket.SockAddr -> IO Bool
registerActiveConnection tracker socketAddress = do
  modifyMVar_ (pendingConnectionAddresses tracker) (\entries -> pure (entries ++ [socketAddress]))
  pure True

unregisterActiveConnection :: ActiveConnectionAddresses -> IO ()
unregisterActiveConnection tracker = do
  currentThreadId <- myThreadId
  untrackActiveConnection tracker currentThreadId

lookupActiveConnectionAddress :: ActiveConnectionAddresses -> IO (Maybe Socket.SockAddr)
lookupActiveConnectionAddress tracker = do
  currentThreadId <- myThreadId
  atomicModifyIORef' (activeConnectionAddresses tracker) (\entries -> (entries, lookup currentThreadId entries))

forkTrackedConnection :: ActiveConnectionAddresses -> (((forall a. IO a -> IO a) -> IO ()) -> IO ())
forkTrackedConnection tracker action = do
  maybeSocketAddress <- claimPendingConnectionAddress tracker
  void $
    forkIOWithUnmask $ \unmask -> do
      currentThreadId <- myThreadId
      for_ maybeSocketAddress (trackActiveConnection tracker currentThreadId)
      action unmask `finally` untrackActiveConnection tracker currentThreadId

claimPendingConnectionAddress :: ActiveConnectionAddresses -> IO (Maybe Socket.SockAddr)
claimPendingConnectionAddress tracker =
  modifyMVar
    (pendingConnectionAddresses tracker)
    ( \entries ->
        case entries of
          [] -> pure ([], Nothing)
          firstAddress : _ -> do
            liftA2 (,) (evaluate (drop 1 entries)) (pure (Just firstAddress))
    )

trackActiveConnection :: ActiveConnectionAddresses -> ThreadId -> Socket.SockAddr -> IO ()
trackActiveConnection tracker currentThreadId socketAddress =
  atomicModifyIORef'
    (activeConnectionAddresses tracker)
    (\entries -> ((currentThreadId, socketAddress) : entries, ()))

untrackActiveConnection :: ActiveConnectionAddresses -> ThreadId -> IO ()
untrackActiveConnection tracker currentThreadId =
  atomicModifyIORef'
    (activeConnectionAddresses tracker)
    (\entries -> (filter ((/= currentThreadId) . fst) entries, ()))

runtimeConnectionExceptionReporter ::
  ListenerScheme ->
  ListenerEndpoint ->
  ActiveConnectionAddresses ->
  (Observability.ConnectionObservability -> IO ()) ->
  (Maybe Wai.Request -> SomeException -> IO ()) ->
  Maybe Wai.Request ->
  SomeException ->
  IO ()
runtimeConnectionExceptionReporter listenerScheme endpoint activeConnectionAddresses connectionReporter defaultReporter maybeRequest exception = do
  maybeConnectionObservability <-
    buildConnectionExceptionObservability
      listenerScheme
      endpoint
      activeConnectionAddresses
      exception
  case maybeConnectionObservability of
    Just connectionObservability ->
      Observability.forceConnectionObservability connectionObservability `seq`
        connectionReporter connectionObservability
    Nothing ->
      defaultReporter maybeRequest exception

buildConnectionExceptionObservability ::
  ListenerScheme ->
  ListenerEndpoint ->
  ActiveConnectionAddresses ->
  SomeException ->
  IO (Maybe Observability.ConnectionObservability)
buildConnectionExceptionObservability listenerScheme endpoint activeConnectionAddresses exception =
  case fromException exception of
    Just warpTlsException ->
      case warpTlsException of
        WarpTLS.InsecureConnectionDenied ->
          buildConnectionObservabilityValue "insecure-connection-denied" "InsecureConnectionDenied"
        WarpTLS.ClientClosedConnectionPrematurely ->
          buildConnectionObservabilityValue "client-closed-connection-prematurely" "ClientClosedConnectionPrematurely"
    Nothing -> pure Nothing
  where
    buildConnectionObservabilityValue eventName exceptionType = do
      maybePeerAddress <-
        fmap (fmap socketAddressText) (lookupActiveConnectionAddress activeConnectionAddresses)
      let maybeClientAddress = maybePeerAddress
      pure . Just $
        Observability.buildConnectionObservability
          ("CONNECTION " <> eventName)
          ( catMaybes
              [ textObservabilityAttribute "client.address" <$> maybeClientAddress,
                textObservabilityAttribute "network.peer.address" <$> maybePeerAddress
              ]
              ++ [ textObservabilityAttribute "url.scheme" (listenerSchemeText listenerScheme),
                   textObservabilityAttribute "server.address" (endpointHost endpoint),
                   Observability.ObservabilityAttribute
                     { Observability.attributeName = "server.port",
                       Observability.attributeValue = Observability.IntAttribute (endpointPort endpoint)
                     },
                   textObservabilityAttribute "harch.connection.event" eventName,
                   textObservabilityAttribute "exception.type" exceptionType,
                   textObservabilityAttribute "exception.message" (Text.pack (displayException exception))
                 ]
          )

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
    <> mapMaybe (fmap (Https,) . acmeTlsEndpoint) (acmeBindPlans startupPlan)

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
  shutdownSignal <- newEmptyMVar
  let noSignalMask = Nothing
      installShutdownHandler signal handler = noSignalMask `seq` installHandler signal handler $! noSignalMask
      requestShutdown = void (tryPutMVar shutdownSignal ())
  previousInterruptHandler <- installShutdownHandler sigINT (Catch requestShutdown)
  previousTerminationHandler <- installShutdownHandler sigTERM (Catch requestShutdown)
  takeMVar shutdownSignal
    `finally` do
      _ <- installShutdownHandler sigINT previousInterruptHandler
      installShutdownHandler sigTERM previousTerminationHandler

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
      case acmeTlsEndpoint acmePlan of
        Nothing ->
          if endpointPort (acmeEndpoint acmePlan) == challengePort
            then validateAcmeRuntimeConfiguration acmePlan
            else
              Just $
                "Unsupported runtime listener startup plan: ACME listener on "
                  <> renderListenerEndpoint (acmeEndpoint acmePlan)
                  <> " requires the configured http-01 port to match its HTTP listener port "
                  <> show (endpointPort (acmeEndpoint acmePlan))
                  <> "."
        Just _ ->
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
  if isNothing (acmeTlsEndpoint acmePlan)
    && isNothing (acmeCertificateDirectory (acmeListenerConfig acmePlan))
    then
      Just $
        "Unsupported runtime listener startup plan: ACME listener on "
          <> renderListenerEndpoint (acmeEndpoint acmePlan)
          <> " requires an ACME certificate directory so HTTPS listeners can consume published certificates."
    else Nothing

hasMatchingAcmeHttp01ChallengeEndpoint :: Int -> [ListenerEndpoint] -> AcmeBindPlan -> Bool
hasMatchingAcmeHttp01ChallengeEndpoint challengePort httpListenerEndpoints acmePlan =
  case find (isAcmeHttp01ChallengeEndpointFor challengePort (acmeEndpoint acmePlan)) httpListenerEndpoints of
    Just _ -> True
    Nothing -> False

acmeHttp01ChallengePort :: AcmeBindPlan -> Either String Int
acmeHttp01ChallengePort acmePlan =
  let certbotConfig = acmeCertbotConfig (acmeListenerConfig acmePlan)
   in case certbotOptionValue "--http-01-port" (certbotArguments certbotConfig) of
        Nothing ->
          Right (acmeHttp01Port (acmeListenerConfig acmePlan))
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

serverSentEventContentType :: Text
serverSentEventContentType = "text/event-stream; charset=utf-8"

renderServerSentEvent :: ServerSentEvent -> Text
renderServerSentEvent ServerSentEvent {serverSentEventName, serverSentEventId, serverSentEventData} =
  Text.concat
    ( maybeToList (renderSseField "event" <$> serverSentEventName)
        <> maybeToList (renderSseField "id" <$> serverSentEventId)
        <> map (renderSseDataLine . Text.filter (`notElem` ['\r', '\n'])) (nonEmptyLines serverSentEventData)
        <> ["\n"]
    )

renderSseField :: Text -> Text -> Text
renderSseField fieldName fieldValue =
  fieldName <> ": " <> Text.filter (`notElem` ['\r', '\n']) fieldValue <> "\n"

renderSseDataLine :: Text -> Text
renderSseDataLine line = "data: " <> line <> "\n"

nonEmptyLines :: Text -> [Text]
nonEmptyLines value =
  case Text.splitOn "\n" value of
    [] -> [Text.empty]
    linesValue -> linesValue

liveRegionAttributes :: LiveRegion -> [HtmlAttribute]
liveRegionAttributes liveRegion =
  case liveRegion of
    PoliteStatus -> [HtmlAttribute "role" "status", HtmlAttribute "aria-live" "polite", HtmlAttribute "aria-atomic" "true"]
    AssertiveAlert -> [HtmlAttribute "role" "alert", HtmlAttribute "aria-live" "assertive", HtmlAttribute "aria-atomic" "true"]

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
      " data-page-link=\"true\"",
      if itemIsActive then " aria-current=\"page\"" else Text.empty,
      ">",
      itemLabel,
      "</a>"
    ]

renderRuntimeDescriptors :: RuntimeNonce -> [RuntimeDescriptor] -> Text
renderRuntimeDescriptors runtimeNonce =
  Text.concat . map (renderRuntimeDescriptor runtimeNonce)

renderRuntimeDescriptor :: RuntimeNonce -> RuntimeDescriptor -> Text
renderRuntimeDescriptor runtimeNonce descriptor =
  case descriptor of
    InlineBootstrap {runtimeDescriptorSource = source} ->
      Text.concat
        [ "<script nonce=\"",
          runtimeNonceValue runtimeNonce,
          "\">",
          source,
          "</script>"
        ]
    DeferredModule {runtimeDescriptorSource = source} ->
      Text.concat
        [ "<script type=\"module\" src=\"",
          source,
          "\" defer></script>"
        ]

toWaiResponse :: (Eq route) => Http.ResponseHeaders -> RuntimeNonce -> Application route context -> Response route context -> Wai.Response
toWaiResponse additionalHeaders runtimeNonce webApplication response =
  case response of
    PageResponse page ->
      Wai.responseLBS
        (if isNotFoundPage webApplication page then Http.status404 else Http.status200)
        (additionalHeaders <> [(Http.hContentType, TextEncoding.encodeUtf8 htmlContentType)])
        (LazyByteString.fromStrict (TextEncoding.encodeUtf8 (renderDocumentWithNonce runtimeNonce (pageShell webApplication page))))
    PageResponseWithMetadata pageResponseBodyValue page ->
      let !pageStatusMessage = ByteString.empty
          !pageStatusMessageLength = ByteString.length pageStatusMessage
          !pageStatus = pageStatusMessageLength `seq` Http.Status (responseStatus pageResponseBodyValue) pageStatusMessage
       in Wai.responseLBS
            pageStatus
            (additionalHeaders <> [(Http.hContentType, TextEncoding.encodeUtf8 htmlContentType)])
            (LazyByteString.fromStrict (TextEncoding.encodeUtf8 (renderDocumentWithNonce runtimeNonce (pageShell webApplication page))))
    BodyResponse responseBodyValue ->
      toWaiBodyResponse additionalHeaders responseBodyValue
    RedirectResponse responseBodyValue location ->
      toWaiBodyResponse (additionalHeaders <> [(Http.hLocation, TextEncoding.encodeUtf8 location)]) responseBodyValue
    ClientActionBodyResponse actionResponse ->
      toWaiBodyResponse (additionalHeaders <> clientActionHeaders actionResponse) (clientActionResponseBody actionResponse)

toWaiBodyResponse :: Http.ResponseHeaders -> ResponseBody -> Wai.Response
toWaiBodyResponse additionalHeaders responseBodyValue =
  Wai.responseLBS
    (Http.mkStatus (responseStatus responseBodyValue) mempty)
    (additionalHeaders <> [(Http.hContentType, TextEncoding.encodeUtf8 (responseContentType responseBodyValue))])
    (LazyByteString.fromStrict (TextEncoding.encodeUtf8 (responseBody responseBodyValue)))

isClientActionRequest :: Wai.Request -> Bool
isClientActionRequest request =
  lookup "X-Harch-Action" (Wai.requestHeaders request) == Just "1"

parseClientActionFields :: LazyByteString.ByteString -> [(Text, Text)]
parseClientActionFields requestBody =
  map
    (bimap decodeActionField (maybe "" decodeActionField))
    (HttpUri.parseQuery (LazyByteString.toStrict requestBody))

decodeActionField :: ByteString.ByteString -> Text
decodeActionField =
  TextEncoding.decodeUtf8With TextEncodingError.lenientDecode

clientActionResponseBody :: ClientActionResponse -> ResponseBody
clientActionResponseBody actionResponse =
  ResponseBody
    { responseStatus = clientActionStatus actionResponse,
      responseContentType = "application/json; charset=utf-8",
      responseBody =
        TextEncoding.decodeUtf8
          ( LazyByteString.toStrict
              ( jsonObjectBytes
                  [ ( "patches",
                      jsonArrayBytes
                        ( map
                            ( \RegionPatch {regionPatchId = patchId, regionPatchHtml = patchHtml} ->
                                jsonObjectBytes
                                  [ ("id", jsonStringBytes patchId),
                                    ("html", jsonStringBytes patchHtml)
                                  ]
                            )
                            (clientActionPatches actionResponse)
                        )
                    ),
                    ("focusId", maybe "null" jsonStringBytes (clientActionFocusId actionResponse))
                  ]
              )
          ),
      responseObservabilityAttributes = clientActionObservabilityAttributes actionResponse,
      responseLogEntries = clientActionLogEntries actionResponse
    }

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

waiRequestPath :: RequestPolicyConfig -> Wai.Request -> Text
waiRequestPath requestPolicyConfig request =
  stripRequestPathPrefix
    (requestPathPrefix requestPolicyConfig request)
    (rawRequestPath request)

requestRedirectLocation :: RequestPolicyConfig -> Wai.Request -> Maybe ByteString.ByteString
requestRedirectLocation requestPolicyConfig request =
  if redirectHttpToHttps requestPolicyConfig
    && requestScheme requestPolicyConfig request == "http"
    && not (isAcmeHttp01ChallengeRequest requestPolicyConfig request)
    then
      fmap
        ( \redirectAuthority ->
            "https://"
              <> redirectAuthority
              <> requestRedirectPathAndQuery requestPolicyConfig request
        )
        (requestRedirectAuthority requestPolicyConfig request)
    else Nothing

requestRedirectAuthority :: RequestPolicyConfig -> Wai.Request -> Maybe ByteString.ByteString
requestRedirectAuthority requestPolicyConfig request =
  fmap
    (applyHttpsRedirectPort (httpsRedirectPort requestPolicyConfig))
    (lookup "Host" (Wai.requestHeaders request))

requestRedirectPathAndQuery :: RequestPolicyConfig -> Wai.Request -> ByteString.ByteString
requestRedirectPathAndQuery requestPolicyConfig request =
  TextEncoding.encodeUtf8 (externalRequestPath requestPolicyConfig request) <> Wai.rawQueryString request

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

isAcmeHttp01ChallengeRequest :: RequestPolicyConfig -> Wai.Request -> Bool
isAcmeHttp01ChallengeRequest requestPolicyConfig request =
  Text.isPrefixOf "/.well-known/acme-challenge/" (waiRequestPath requestPolicyConfig request)

requestPolicyResponseHeaders :: RequestPolicyConfig -> Wai.Request -> Http.ResponseHeaders
requestPolicyResponseHeaders requestPolicyConfig request =
  responseSecurityHeaderValues (responseSecurityHeaders requestPolicyConfig)
    <> strictTransportSecurityHeaders requestPolicyConfig request
    <> corsPolicyHeaders (corsPolicy requestPolicyConfig) request

responsePolicyHeaders :: RequestPolicyConfig -> Wai.Request -> RuntimeNonce -> Response route context -> Http.ResponseHeaders
responsePolicyHeaders requestPolicyConfig request runtimeNonce response =
  responseSecurityHeaderValuesWithNonce
    (responseSecurityHeaders requestPolicyConfig)
    ( case response of
        PageResponse _ -> Just runtimeNonce
        PageResponseWithMetadata _ _ -> Just runtimeNonce
        BodyResponse _ -> Nothing
        RedirectResponse _ _ -> Nothing
        ClientActionBodyResponse _ -> Nothing
    )
    <> strictTransportSecurityHeaders requestPolicyConfig request
    <> corsPolicyHeaders (corsPolicy requestPolicyConfig) request

responseSecurityHeaderValues :: ResponseSecurityHeadersConfig -> Http.ResponseHeaders
responseSecurityHeaderValues responseSecurityHeadersConfig =
  responseSecurityHeaderValuesWithNonce responseSecurityHeadersConfig Nothing

responseSecurityHeaderValuesWithNonce :: ResponseSecurityHeadersConfig -> Maybe RuntimeNonce -> Http.ResponseHeaders
responseSecurityHeaderValuesWithNonce responseSecurityHeadersConfig maybeRuntimeNonce =
  catMaybes
    [ ("Content-Security-Policy",) . TextEncoding.encodeUtf8
        <$> contentSecurityPolicyWithRuntimeNonce maybeRuntimeNonce (contentSecurityPolicy responseSecurityHeadersConfig),
      if contentTypeOptionsNoSniff responseSecurityHeadersConfig
        then Just ("X-Content-Type-Options", "nosniff")
        else Nothing,
      ("X-XSS-Protection",) . TextEncoding.encodeUtf8
        <$> xssProtection responseSecurityHeadersConfig,
      ("Referrer-Policy",) . TextEncoding.encodeUtf8
        <$> referrerPolicy responseSecurityHeadersConfig,
      ("Permissions-Policy",) . TextEncoding.encodeUtf8
        <$> permissionsPolicy responseSecurityHeadersConfig,
      ("X-Frame-Options",) . TextEncoding.encodeUtf8
        <$> frameOptions responseSecurityHeadersConfig
    ]

contentSecurityPolicyWithRuntimeNonce :: Maybe RuntimeNonce -> Maybe Text -> Maybe Text
contentSecurityPolicyWithRuntimeNonce maybeRuntimeNonce maybeContentSecurityPolicy =
  case (maybeRuntimeNonce, maybeContentSecurityPolicy) of
    (Just runtimeNonce, Just contentSecurityPolicy) -> Just (addRuntimeNonceToContentSecurityPolicy runtimeNonce contentSecurityPolicy)
    (_, contentSecurityPolicy) -> contentSecurityPolicy

addRuntimeNonceToContentSecurityPolicy :: RuntimeNonce -> Text -> Text
addRuntimeNonceToContentSecurityPolicy runtimeNonce policy =
  Text.intercalate
    "; "
    ( if any isScriptSourceDirective directives
        then map addNonceToDirective directives
        else directives <> ["script-src " <> nonceSource]
    )
  where
    nonceSource = "'nonce-" <> runtimeNonceValue runtimeNonce <> "'"
    directives = filter (not . Text.null) (map Text.strip (Text.splitOn ";" policy))
    isScriptSourceDirective directive =
      case Text.words directive of
        "script-src" : _ -> True
        _ -> False
    addNonceToDirective directive =
      case Text.words directive of
        "script-src" : sources ->
          Text.unwords
            ( "script-src"
                : if "'none'" `elem` sources
                  then [nonceSource]
                  else sources <> [nonceSource]
            )
        _ -> Text.strip directive

strictTransportSecurityHeaders :: RequestPolicyConfig -> Wai.Request -> Http.ResponseHeaders
strictTransportSecurityHeaders requestPolicyConfig request =
  case strictTransportSecurity requestPolicyConfig of
    Just strictTransportSecurityConfig
      | requestScheme requestPolicyConfig request == "https" ->
          [ ( "Strict-Transport-Security",
              TextEncoding.encodeUtf8 (strictTransportSecurityHeaderValue strictTransportSecurityConfig)
            )
          ]
    _ -> []

corsPolicyHeaders :: CorsPolicyConfig -> Wai.Request -> Http.ResponseHeaders
corsPolicyHeaders corsPolicyConfig request =
  case lookup "Origin" (Wai.requestHeaders request) of
    Just originHeader
      | originAllowed corsPolicyConfig originHeader ->
          [ ("Access-Control-Allow-Origin", originHeader),
            ("Vary", "Origin")
          ]
            <> corsPreflightHeaders corsPolicyConfig request
    _ -> []

corsPreflightHeaders :: CorsPolicyConfig -> Wai.Request -> Http.ResponseHeaders
corsPreflightHeaders corsPolicyConfig request =
  if corsPreflightRequestAllowed corsPolicyConfig request
    then
      [ ("Access-Control-Allow-Methods", corsHeaderValue (corsAllowedMethods corsPolicyConfig))
      ]
        <> [ ("Access-Control-Allow-Headers", corsHeaderValue (corsAllowedHeaders corsPolicyConfig))
           | not (null (corsAllowedHeaders corsPolicyConfig))
           ]
        <> [ ("Access-Control-Max-Age", ByteStringChar8.pack (show maxAgeSeconds))
           | Just maxAgeSeconds <- [corsMaxAgeSeconds corsPolicyConfig]
           ]
    else []

corsPreflightResponse :: RequestPolicyConfig -> Wai.Request -> Maybe Wai.Response
corsPreflightResponse requestPolicyConfig request =
  case lookup "Origin" (Wai.requestHeaders request) of
    Just originHeader
      | originAllowed (corsPolicy requestPolicyConfig) originHeader
          && corsPreflightRequestAllowed (corsPolicy requestPolicyConfig) request ->
          Just (Wai.responseLBS Http.status204 [] "")
    _ -> Nothing

isCorsPreflightRequest :: Wai.Request -> Bool
isCorsPreflightRequest request =
  Wai.requestMethod request == "OPTIONS"
    && isJust (lookup "Origin" (Wai.requestHeaders request))
    && isJust (lookup "Access-Control-Request-Method" (Wai.requestHeaders request))

corsPreflightRequestAllowed :: CorsPolicyConfig -> Wai.Request -> Bool
corsPreflightRequestAllowed corsPolicyConfig request =
  case lookup "Access-Control-Request-Method" (Wai.requestHeaders request) of
    Just requestedMethod ->
      isCorsPreflightRequest request
        && requestedMethodAllowed corsPolicyConfig requestedMethod
    Nothing -> False

originAllowed :: CorsPolicyConfig -> ByteString.ByteString -> Bool
originAllowed corsPolicyConfig originHeader =
  originHeader `elem` map TextEncoding.encodeUtf8 (corsAllowedOrigins corsPolicyConfig)

requestedMethodAllowed :: CorsPolicyConfig -> ByteString.ByteString -> Bool
requestedMethodAllowed corsPolicyConfig requestedMethod =
  requestedMethod `elem` map TextEncoding.encodeUtf8 (corsAllowedMethods corsPolicyConfig)

corsHeaderValue :: [Text] -> ByteString.ByteString
corsHeaderValue =
  TextEncoding.encodeUtf8 . Text.intercalate ", "

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

requestContextObservabilityAttributes :: RequestPolicyConfig -> Wai.Request -> [Observability.ObservabilityAttribute]
requestContextObservabilityAttributes requestPolicyConfig request =
  [ textObservabilityAttribute "client.address" (effectiveClientAddress requestPolicyConfig request),
    textObservabilityAttribute "network.peer.address" (peerAddressText request)
  ]
    ++ maybe
      []
      (pure . textObservabilityAttribute "harch.client.address.source")
      (effectiveClientAddressSource requestPolicyConfig request)
    ++ maybe
      []
      (pure . textObservabilityAttribute "http.request.header.x_forwarded_for")
      (trustedRequestHeaderText requestPolicyConfig "X-Forwarded-For" request)
    ++ maybe
      []
      (pure . textObservabilityAttribute "http.request.header.forwarded")
      (trustedRequestHeaderText requestPolicyConfig "Forwarded" request)
    ++ maybe
      []
      (pure . textObservabilityAttribute "http.request.header.x_forwarded_proto")
      (trustedRequestHeaderText requestPolicyConfig "X-Forwarded-Proto" request)
    ++ maybe
      []
      (pure . textObservabilityAttribute "http.request.header.x_forwarded_prefix")
      (trustedRequestHeaderText requestPolicyConfig "X-Forwarded-Prefix" request)
    ++ maybe
      []
      (pure . textObservabilityAttribute "user_agent.original")
      (requestHeaderText "User-Agent" request)
    ++ maybe
      []
      (pure . textObservabilityAttribute "http.request.header.referer")
      (sanitizedReferer request)
    ++ maybe
      []
      (pure . textObservabilityAttribute "http.request.header.x_requested_with")
      (requestHeaderText "X-Requested-With" request)
    ++ maybe
      []
      (pure . textObservabilityAttribute "harch.request.source")
      (requestSource request)

requestLogContextFields :: RequestPolicyConfig -> Wai.Request -> [Text]
requestLogContextFields requestPolicyConfig request =
  [ renderRequestLogField "client.address" (effectiveClientAddress requestPolicyConfig request),
    renderRequestLogField "network.peer.address" (peerAddressText request)
  ]
    ++ optionalRequestLogField
      "harch.client.address.source"
      (effectiveClientAddressSource requestPolicyConfig request)
    ++ optionalRequestLogField
      "http.request.header.x_forwarded_for"
      (trustedRequestHeaderText requestPolicyConfig "X-Forwarded-For" request)
    ++ optionalRequestLogField
      "http.request.header.forwarded"
      (trustedRequestHeaderText requestPolicyConfig "Forwarded" request)
    ++ optionalRequestLogField
      "http.request.header.x_forwarded_proto"
      (trustedRequestHeaderText requestPolicyConfig "X-Forwarded-Proto" request)
    ++ optionalRequestLogField
      "http.request.header.x_forwarded_prefix"
      (trustedRequestHeaderText requestPolicyConfig "X-Forwarded-Prefix" request)
    ++ optionalRequestLogField
      "user_agent.original"
      (requestHeaderText "User-Agent" request)
    ++ optionalRequestLogField
      "http.request.header.referer"
      (sanitizedReferer request)
    ++ optionalRequestLogField
      "http.request.header.x_requested_with"
      (requestHeaderText "X-Requested-With" request)
    ++ optionalRequestLogField
      "harch.request.source"
      (requestSource request)
    ++ [renderRequestLogField "url.scheme" (requestScheme requestPolicyConfig request)]

optionalRequestLogField :: Text -> Maybe Text -> [Text]
optionalRequestLogField fieldName maybeFieldValue =
  case maybeFieldValue of
    Just fieldValue -> [renderRequestLogField fieldName fieldValue]
    Nothing -> []

requestTimingObservabilityAttributes :: Word64 -> Word64 -> [(Text, Word64, Word64)] -> [Observability.ObservabilityAttribute]
requestTimingObservabilityAttributes requestStartedAt requestCompletedAt phaseTimings =
  intObservabilityAttribute "harch.request.start_monotonic_ns" (fromIntegral requestStartedAt)
    : intObservabilityAttribute "harch.request.duration_ns" (nanosecondsBetween requestStartedAt requestCompletedAt)
    : concatMap phaseTimingAttributes phaseTimings
  where
    phaseTimingAttributes (phaseName, phaseStartedAt, phaseEndedAt) =
      [ intObservabilityAttribute
          ("harch.phase." <> phaseName <> ".start_offset_ns")
          (nanosecondsBetween requestStartedAt phaseStartedAt),
        intObservabilityAttribute
          ("harch.phase." <> phaseName <> ".duration_ns")
          (nanosecondsBetween phaseStartedAt phaseEndedAt)
      ]

nanosecondsBetween :: Word64 -> Word64 -> Int
nanosecondsBetween start end =
  fromIntegral (end - min start end)

textObservabilityAttribute :: Text -> Text -> Observability.ObservabilityAttribute
textObservabilityAttribute name value =
  Observability.ObservabilityAttribute
    { Observability.attributeName = name,
      Observability.attributeValue = Observability.TextAttribute value
    }

intObservabilityAttribute :: Text -> Int -> Observability.ObservabilityAttribute
intObservabilityAttribute name value =
  Observability.ObservabilityAttribute
    { Observability.attributeName = name,
      Observability.attributeValue = Observability.IntAttribute value
    }

requestScheme :: RequestPolicyConfig -> Wai.Request -> Text
requestScheme requestPolicyConfig request =
  case fmap Text.toLower (trustedForwardedHeaderToken requestPolicyConfig "proto" request <|> trustedRequestHeaderToken requestPolicyConfig "X-Forwarded-Proto" request) of
    Just "https" -> "https"
    Just "http" -> "http"
    _ ->
      if Wai.isSecure request
        then "https"
        else "http"

listenerSchemeText :: ListenerScheme -> Text
listenerSchemeText listenerScheme =
  case listenerScheme of
    Http -> "http"
    Https -> "https"

effectiveClientAddress :: RequestPolicyConfig -> Wai.Request -> Text
effectiveClientAddress requestPolicyConfig request =
  fromMaybe
    (peerAddressText request)
    (trustedForwardedHeaderToken requestPolicyConfig "for" request <|> trustedRequestHeaderToken requestPolicyConfig "X-Forwarded-For" request)

effectiveClientAddressSource :: RequestPolicyConfig -> Wai.Request -> Maybe Text
effectiveClientAddressSource requestPolicyConfig request =
  case trustedForwardedHeaderToken requestPolicyConfig "for" request of
    Just _ -> Just "forwarded"
    Nothing ->
      case trustedRequestHeaderToken requestPolicyConfig "X-Forwarded-For" request of
        Just _ -> Just "x-forwarded-for"
        Nothing -> Nothing

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
    (limitObservabilityHeaderValue . Text.strip . TextEncoding.decodeUtf8)
    (lookup headerName (Wai.requestHeaders request))

forwardedHeaderToken :: Text -> Wai.Request -> Maybe Text
forwardedHeaderToken parameterName request =
  requestHeaderText "Forwarded" request >>= forwardedParameterValue parameterName

trustedRequestHeaderText :: RequestPolicyConfig -> Http.HeaderName -> Wai.Request -> Maybe Text
trustedRequestHeaderText requestPolicyConfig headerName request =
  if trustForwardedHeaders requestPolicyConfig
    then requestHeaderText headerName request
    else Nothing

trustedRequestHeaderToken :: RequestPolicyConfig -> Http.HeaderName -> Wai.Request -> Maybe Text
trustedRequestHeaderToken requestPolicyConfig headerName request =
  if trustForwardedHeaders requestPolicyConfig
    then requestHeaderToken headerName request
    else Nothing

trustedForwardedHeaderToken :: RequestPolicyConfig -> Text -> Wai.Request -> Maybe Text
trustedForwardedHeaderToken requestPolicyConfig parameterName request =
  if trustForwardedHeaders requestPolicyConfig
    then forwardedHeaderToken parameterName request
    else Nothing

forwardedParameterValue :: Text -> Text -> Maybe Text
forwardedParameterValue parameterName headerValue =
  case firstCommaSeparatedValue headerValue of
    Nothing -> Nothing
    Just forwardedElement ->
      listToMaybe
        [ cleanForwardedParameterValue parameterValue
        | parameter <- Text.splitOn ";" forwardedElement,
          let (parameterKey, parameterValueWithEquals) = Text.breakOn "=" (Text.strip parameter),
          Text.toLower (Text.strip parameterKey) == Text.toLower parameterName,
          Just parameterValue <- [Text.stripPrefix "=" parameterValueWithEquals],
          not (Text.null (cleanForwardedParameterValue parameterValue))
        ]

cleanForwardedParameterValue :: Text -> Text
cleanForwardedParameterValue =
  Text.strip . stripSurroundingQuotes . Text.strip

stripSurroundingQuotes :: Text -> Text
stripSurroundingQuotes value =
  fromMaybe value (Text.stripPrefix "\"" value >>= Text.stripSuffix "\"")

sanitizedReferer :: Wai.Request -> Maybe Text
sanitizedReferer request =
  sanitizeRefererValue <$> requestHeaderText "Referer" request

sanitizeRefererValue :: Text -> Text
sanitizeRefererValue =
  limitObservabilityHeaderValue
    . Text.takeWhile (\character -> character /= '?' && character /= '#')

requestSource :: Wai.Request -> Maybe Text
requestSource request =
  case fmap Text.toLower (requestHeaderText "X-Requested-With" request) of
    Just "tiny-navigation" -> Just "enhanced-navigation"
    Just "xmlhttprequest" -> Just "xml-http-request"
    Just _ -> Just "scripted-request"
    Nothing ->
      case (fmap Text.toLower (requestHeaderText "Accept" request), fmap Text.toLower (requestHeaderText "User-Agent" request)) of
        (Just acceptHeader, _) | "application/json" `Text.isInfixOf` acceptHeader -> Just "api-client"
        (_, Just userAgent) | "curl/" `Text.isPrefixOf` userAgent -> Just "manual-client"
        (_, Just _) -> Just "browser-or-client"
        _ -> Nothing

requestTraceContext :: Wai.Request -> Maybe Observability.RequestTraceContext
requestTraceContext request =
  parseTraceParentHeader =<< requestHeaderText "traceparent" request
  where
    parseTraceParentHeader traceParentHeader =
      case Text.splitOn "-" traceParentHeader of
        [version, traceId, parentSpanId, traceFlags]
          | isValidTraceParentVersion version
              && isValidTraceParentTraceId traceId
              && isValidTraceParentSpanId parentSpanId
              && isValidTraceParentFlags traceFlags ->
              Just
                Observability.RequestTraceContext
                  { Observability.traceContextTraceId = Text.toLower traceId,
                    Observability.traceContextParentSpanId = Text.toLower parentSpanId,
                    Observability.traceContextState = requestHeaderText "tracestate" request
                  }
        _ -> Nothing

isValidTraceParentVersion :: Text -> Bool
isValidTraceParentVersion version =
  Text.length version == 2
    && Text.all isAsciiHexText version
    && Text.toLower version /= "ff"

isValidTraceParentTraceId :: Text -> Bool
isValidTraceParentTraceId traceId =
  Text.length traceId == 32
    && Text.all isAsciiHexText traceId
    && traceId /= "00000000000000000000000000000000"

isValidTraceParentSpanId :: Text -> Bool
isValidTraceParentSpanId spanId =
  Text.length spanId == 16
    && Text.all isAsciiHexText spanId
    && spanId /= "0000000000000000"

isValidTraceParentFlags :: Text -> Bool
isValidTraceParentFlags traceFlags =
  Text.length traceFlags == 2
    && Text.all isAsciiHexText traceFlags

isAsciiHexText :: Char -> Bool
isAsciiHexText character =
  isHexDigit character && fromEnum character < 128

limitObservabilityHeaderValue :: Text -> Text
limitObservabilityHeaderValue =
  Text.take 256

requestPathPrefix :: RequestPolicyConfig -> Wai.Request -> Text
requestPathPrefix requestPolicyConfig request =
  maybe
    Text.empty
    normalizeRequestPathPrefix
    (trustedRequestHeaderToken requestPolicyConfig "X-Forwarded-Prefix" request)

rawRequestPath :: Wai.Request -> Text
rawRequestPath request =
  if ByteString.null (Wai.rawPathInfo request)
    then "/"
    else TextEncoding.decodeUtf8 (Wai.rawPathInfo request)

waiRequestRouteTarget :: RequestPolicyConfig -> Wai.Request -> Text
waiRequestRouteTarget requestPolicyConfig request =
  appendRawQueryString
    (waiRequestPath requestPolicyConfig request)
    (Wai.rawQueryString request)

appendRawQueryString :: Text -> ByteString.ByteString -> Text
appendRawQueryString path rawQueryString =
  if ByteString.null rawQueryString
    then path
    else path <> TextEncoding.decodeUtf8 rawQueryString

externalRequestPath :: RequestPolicyConfig -> Wai.Request -> Text
externalRequestPath requestPolicyConfig request =
  applyRequestPathPrefix
    (requestPathPrefix requestPolicyConfig request)
    (waiRequestPath requestPolicyConfig request)

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

reportEarlyRequestObservability ::
  (Eq route) =>
  Application route context ->
  Wai.Request ->
  Word64 ->
  Word64 ->
  Text ->
  Wai.Response ->
  IO ()
reportEarlyRequestObservability webApplication request requestStartedAt requestCompletedAt routePath response =
  let requestPolicyConfig = applicationRequestPolicy webApplication
      requestObservability =
        maybe
          id
          Observability.withRequestTraceContext
          (requestTraceContext request)
          ( Observability.buildRequestObservability
              (TextEncoding.decodeUtf8 (Wai.requestMethod request))
              (requestScheme requestPolicyConfig request)
              (waiRequestPath requestPolicyConfig request)
              routePath
              (Http.statusCode (Wai.responseStatus response))
              Observability.BodyResponseKind
              ( requestContextObservabilityAttributes requestPolicyConfig request
                  <> requestTimingObservabilityAttributes requestStartedAt requestCompletedAt []
              )
          )
   in Observability.forceRequestObservability requestObservability `seq`
        reportRequestObservability webApplication requestObservability

serveStaticAssetResponse :: StaticAssetsConfig -> Text -> IO (Maybe (Text, Wai.Response))
serveStaticAssetResponse staticAssetsConfig requestPath =
  case matchStaticAssetRoot staticAssetsConfig requestPath of
    Nothing -> pure Nothing
    Just (matchedRoot, relativeAssetPath) ->
      case sanitizeStaticAssetPath relativeAssetPath of
        Nothing -> pure (Just (staticAssetRoutePath matchedRoot, missingStaticAssetResponse staticAssetsConfig))
        Just safeAssetPath -> do
          case staticAssetContentType staticAssetsConfig safeAssetPath of
            Nothing -> pure (Just (staticAssetRoutePath matchedRoot, missingStaticAssetResponse staticAssetsConfig))
            Just assetContentType -> do
              let assetFilePath = staticDirectory matchedRoot </> safeAssetPath
              assetExists <- doesFileExist assetFilePath
              case assetExists of
                True -> do
                  assetContents <- ByteString.readFile assetFilePath
                  pure
                    ( Just
                        ( staticAssetRoutePath matchedRoot,
                          Wai.responseLBS
                            Http.status200
                            (staticAssetHeaders staticAssetsConfig assetContentType)
                            (LazyByteString.fromStrict assetContents)
                        )
                    )
                False -> pure (Just (staticAssetRoutePath matchedRoot, missingStaticAssetResponse staticAssetsConfig))

staticAssetRoutePath :: StaticAssetRoot -> Text
staticAssetRoutePath staticRoot =
  case staticUrlPrefix staticRoot of
    "/" -> "/*"
    staticPrefix -> staticPrefix <> "/*"

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
        && not (isHiddenStaticAssetSegment segment)

isHiddenStaticAssetSegment :: FilePath -> Bool
isHiddenStaticAssetSegment segment =
  case segment of
    '.' : _ -> True
    _ -> False

staticAssetHeaders :: StaticAssetsConfig -> Text -> Http.ResponseHeaders
staticAssetHeaders staticAssetsConfig assetContentType =
  (Http.hContentType, TextEncoding.encodeUtf8 assetContentType)
    : maybe [] (\cacheHeader -> [(Http.hCacheControl, TextEncoding.encodeUtf8 cacheHeader)]) (staticCacheControlHeaderValue staticAssetsConfig)

staticCacheControlHeaderValue :: StaticAssetsConfig -> Maybe Text
staticCacheControlHeaderValue staticAssetsConfig =
  fmap
    (\seconds -> Text.pack ("public, max-age=" <> show seconds))
    (staticCacheControlSeconds staticAssetsConfig)

staticAssetContentType :: StaticAssetsConfig -> FilePath -> Maybe Text
staticAssetContentType staticAssetsConfig assetFilePath =
  lookup (Text.pack (takeExtension assetFilePath)) (staticAssetContentTypes staticAssetsConfig)

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
  plannedListeners <- concat <$> traverse classifyListener (listenerConfigs (toServerConfig config))
  case firstDuplicate (concatMap plannedBindEndpoints plannedListeners) of
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

classifyListener :: ListenerConfig -> Either ListenerStartupError [PlannedListener]
classifyListener listenerConfig =
  case (listenerScheme listenerConfig, listenerTls listenerConfig, listenerAcme listenerConfig) of
    (Http, Nothing, Nothing) ->
      Right [PlannedHttp (listenerEndpoint listenerConfig)]
    (Http, Nothing, Just acmeConfig) ->
      Right
        [ PlannedHttp (listenerEndpoint listenerConfig),
          PlannedAcme
            AcmeBindPlan
              { acmeEndpoint = listenerEndpoint listenerConfig,
                acmeTlsEndpoint = Nothing,
                acmeListenerConfig = acmeConfig
              }
        ]
    (Http, Just _, _) ->
      Left (InvalidListenerTlsConfiguration listenerConfig)
    (Https, _, Just _) ->
      Left (InvalidListenerAcmeConfiguration listenerConfig)
    (Https, Nothing, Nothing) ->
      Left (InvalidListenerTlsConfiguration listenerConfig)
    (Https, Just TlsConfig {certificateSource = ManualCertificateFiles {certificateFile = certificatePath, privateKeyFile = privateKeyPath}}, Nothing) ->
      Right
        [ PlannedManualTls
            ManualTlsBindPlan
              { tlsEndpoint = listenerEndpoint listenerConfig,
                tlsCertificateFile = certificatePath,
                tlsPrivateKeyFile = privateKeyPath,
                tlsCredentialSourceKind = ManualTlsCredentials,
                tlsStartupMode = RequireCertificateFiles
              }
        ]
    (Https, Just TlsConfig {certificateSource = SharedCertificateFiles {certificateDirectory = sharedDirectory, sharedCertificateStartupMode = startupMode}}, Nothing) ->
      let (certificatePath, privateKeyPath) = sharedCertificatePaths sharedDirectory
       in Right
            [ PlannedManualTls
                ManualTlsBindPlan
                  { tlsEndpoint = listenerEndpoint listenerConfig,
                    tlsCertificateFile = certificatePath,
                    tlsPrivateKeyFile = privateKeyPath,
                    tlsCredentialSourceKind = SharedTlsCredentials,
                    tlsStartupMode = startupMode
                  }
            ]
    (Https, Just TlsConfig {certificateSource = AcmeCertificateSource acmeConfig}, Nothing) ->
      Right
        [ PlannedAcme
            AcmeBindPlan
              { acmeEndpoint = listenerEndpoint listenerConfig,
                acmeTlsEndpoint = Just (listenerEndpoint listenerConfig),
                acmeListenerConfig = acmeConfig
              }
        ]

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
  (generatedTraceId, spanId) <- nextOtlpSpanIdentifiers
  let childSpans =
        requestRuntimePhaseChildSpans requestObservability
          <> requestDatabaseChildSpans requestObservability
  childSpanIds <- mapM (const nextOtlpSpanId) childSpans
  endTimeUnixNano <- currentUnixTimeNSec
  let rootDurationNanoseconds =
        fromMaybe
          (requestFallbackDurationNanoseconds childSpans)
          (requestDurationNanoseconds requestObservability)
      startTimeUnixNano = nonNegativeStartTime endTimeUnixNano rootDurationNanoseconds
      traceId =
        maybe
          generatedTraceId
          Observability.traceContextTraceId
          (Observability.observabilityTraceContext requestObservability)
      parentSpanId =
        Observability.traceContextParentSpanId
          <$> Observability.observabilityTraceContext requestObservability
      traceState =
        Observability.traceContextState
          =<< Observability.observabilityTraceContext requestObservability
      timedChildSpans =
        zipWith
          (timedOtlpChildSpan startTimeUnixNano rootDurationNanoseconds)
          childSpanIds
          childSpans
      rootSpan =
        withoutDatabaseOperationAttributes
          (Observability.observabilityRequestSpan requestObservability)
  let requestBody =
        otlpTraceBodyFromSpan
          serviceName
          traceId
          spanId
          parentSpanId
          traceState
          startTimeUnixNano
          endTimeUnixNano
          rootSpan
          "SPAN_KIND_SERVER"
          (otlpRequestSpanStatusFields requestObservability)
          timedChildSpans
  sendOtlpTraceRequest exporter requestBody

exportConnectionObservabilityToOtlp ::
  Text ->
  OtlpExporter ->
  Observability.ConnectionObservability ->
  IO ()
exportConnectionObservabilityToOtlp serviceName exporter connectionObservability = do
  (traceId, spanId) <- nextOtlpSpanIdentifiers
  endTimeUnixNano <- currentUnixTimeNSec
  let startTimeUnixNano = nonNegativeStartTime endTimeUnixNano connectionFallbackDurationNanoseconds
  let requestBody =
        otlpTraceBodyFromSpan
          serviceName
          traceId
          spanId
          Nothing
          Nothing
          startTimeUnixNano
          endTimeUnixNano
          (Observability.observabilityConnectionSpan connectionObservability)
          "SPAN_KIND_INTERNAL"
          otlpErrorStatusFields
          []
  sendOtlpTraceRequest exporter requestBody

sendOtlpTraceRequest :: OtlpExporter -> LazyByteString.ByteString -> IO ()
sendOtlpTraceRequest exporter requestBody = do
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

currentUnixTimeNSec :: IO Word64
currentUnixTimeNSec =
  floor . (* 1000000000) <$> getPOSIXTime

otlpTraceBodyFromSpan ::
  Text ->
  Text ->
  Text ->
  Maybe Text ->
  Maybe Text ->
  Word64 ->
  Word64 ->
  Observability.RequestSpan ->
  Text ->
  [(Text, LazyByteString.ByteString)] ->
  [(Text, Text, Word64, Word64, Observability.RequestSpan)] ->
  LazyByteString.ByteString
otlpTraceBodyFromSpan serviceName traceId spanId maybeParentSpanId maybeTraceState startTimeUnixNano endTimeUnixNano requestSpan rootSpanKind statusFields childSpans =
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
                              ( otlpSpanObject
                                  traceId
                                  spanId
                                  maybeParentSpanId
                                  maybeTraceState
                                  rootSpanKind
                                  startTimeUnixNano
                                  endTimeUnixNano
                                  requestSpan
                                  statusFields
                                  : [ otlpSpanObject
                                        traceId
                                        childSpanId
                                        (Just spanId)
                                        maybeTraceState
                                        childSpanKind
                                        childStartTimeUnixNano
                                        childEndTimeUnixNano
                                        childSpan
                                        []
                                    | (childSpanId, childSpanKind, childStartTimeUnixNano, childEndTimeUnixNano, childSpan) <- childSpans
                                    ]
                              )
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
  Maybe Text ->
  Maybe Text ->
  Text ->
  Word64 ->
  Word64 ->
  Observability.RequestSpan ->
  [(Text, LazyByteString.ByteString)] ->
  LazyByteString.ByteString
otlpSpanObject traceId spanId maybeParentSpanId maybeTraceState spanKind startTimeUnixNano endTimeUnixNano requestSpan statusFields =
  jsonObjectBytes
    ( [ ("traceId", jsonStringBytes traceId),
        ("spanId", jsonStringBytes spanId),
        ("name", jsonStringBytes (Observability.requestSpanDisplayName requestSpan)),
        ("kind", jsonStringBytes spanKind),
        ("startTimeUnixNano", jsonStringBytes (Text.pack (show startTimeUnixNano))),
        ("endTimeUnixNano", jsonStringBytes (Text.pack (show endTimeUnixNano))),
        ( "attributes",
          jsonArrayBytes
            ( map otlpAttribute $
                filter shouldExportOtlpAttribute (Observability.requestSpanAttributes requestSpan)
            )
        )
      ]
        ++ maybe [] (\parentSpanId -> [("parentSpanId", jsonStringBytes parentSpanId)]) maybeParentSpanId
        ++ maybe [] (\traceState -> [("traceState", jsonStringBytes traceState)]) maybeTraceState
        ++ statusFields
    )

minimumOtlpSpanDurationNanoseconds :: Word64
minimumOtlpSpanDurationNanoseconds = 1000

requestFallbackDurationNanoseconds :: [(Text, Observability.RequestSpan)] -> Word64
requestFallbackDurationNanoseconds childSpans =
  minimumOtlpSpanDurationNanoseconds * fromIntegral (max 1 (length childSpans + 1))

connectionFallbackDurationNanoseconds :: Word64
connectionFallbackDurationNanoseconds = minimumOtlpSpanDurationNanoseconds

nonNegativeStartTime :: Word64 -> Word64 -> Word64
nonNegativeStartTime endTimeUnixNano durationNanos =
  endTimeUnixNano - min endTimeUnixNano durationNanos

timedOtlpChildSpan :: Word64 -> Word64 -> Text -> (Text, Observability.RequestSpan) -> (Text, Text, Word64, Word64, Observability.RequestSpan)
timedOtlpChildSpan rootStartTimeUnixNano rootDurationNanoseconds childSpanId (childSpanKind, childSpan) =
  ( childSpanId,
    childSpanKind,
    childStartTimeUnixNano,
    childStartTimeUnixNano + childDurationNanoseconds,
    childSpan
  )
  where
    childStartOffsetNanoseconds =
      fromMaybe 0 (requestSpanIntAttribute "harch.span.start_offset_ns" childSpan)
    childDurationNanoseconds =
      fromMaybe rootDurationNanoseconds (requestSpanIntAttribute "harch.span.duration_ns" childSpan)
    childStartTimeUnixNano =
      rootStartTimeUnixNano + min rootDurationNanoseconds childStartOffsetNanoseconds

requestDurationNanoseconds :: Observability.RequestObservability -> Maybe Word64
requestDurationNanoseconds requestObservability =
  requestSpanIntAttribute "harch.request.duration_ns" (Observability.observabilityRequestSpan requestObservability)

requestSpanIntAttribute :: Text -> Observability.RequestSpan -> Maybe Word64
requestSpanIntAttribute attributeName requestSpan =
  listToMaybe
    [ fromIntegral attributeValue
    | Observability.ObservabilityAttribute
        { Observability.attributeName = currentName,
          Observability.attributeValue = Observability.IntAttribute attributeValue
        } <-
        Observability.requestSpanAttributes requestSpan,
      currentName == attributeName,
      attributeValue >= 0
    ]

requestRuntimePhaseChildSpans :: Observability.RequestObservability -> [(Text, Observability.RequestSpan)]
requestRuntimePhaseChildSpans requestObservability =
  mapMaybe
    (\(displayName, phaseName, copiedAttributeNames) -> runtimePhaseChildSpan displayName phaseName copiedAttributeNames)
    [ ("HarchWeb request policy", "request-policy", ["http.request.method", "url.scheme"]),
      ("HarchWeb route match", "route-match", ["url.path", "http.route"]),
      ("HarchWeb render response", "render-response", ["http.response.status_code", "harch.response.kind"])
    ]
  where
    rootAttributes =
      Observability.requestSpanAttributes
        (Observability.observabilityRequestSpan requestObservability)

    runtimePhaseChildSpan displayName phaseName copiedAttributeNames =
      case phaseTimingAttributes phaseName of
        [] -> Nothing
        timingAttributes ->
          Just
            ( "SPAN_KIND_INTERNAL",
              Observability.RequestSpan
                { Observability.requestSpanDisplayName = displayName,
                  Observability.requestSpanAttributes =
                    [textObservabilityAttribute "harch.span.phase" phaseName]
                      <> timingAttributes
                      <> concatMap (`attributesNamed` rootAttributes) copiedAttributeNames
                }
            )

    phaseTimingAttributes phaseName =
      renamedIntAttribute
        "harch.span.start_offset_ns"
        ("harch.phase." <> phaseName <> ".start_offset_ns")
        <> renamedIntAttribute
          "harch.span.duration_ns"
          ("harch.phase." <> phaseName <> ".duration_ns")

    renamedIntAttribute childName rootName =
      [ Observability.ObservabilityAttribute
          { Observability.attributeName = childName,
            Observability.attributeValue = Observability.IntAttribute attributeValue
          }
      | Observability.ObservabilityAttribute
          { Observability.attributeName = currentName,
            Observability.attributeValue = Observability.IntAttribute attributeValue
          } <-
          rootAttributes,
        currentName == rootName
      ]

attributesNamed :: Text -> [Observability.ObservabilityAttribute] -> [Observability.ObservabilityAttribute]
attributesNamed expectedName attributes =
  [ attribute
  | attribute <- attributes,
    Observability.attributeName attribute == expectedName
  ]

requestDatabaseChildSpans :: Observability.RequestObservability -> [(Text, Observability.RequestSpan)]
requestDatabaseChildSpans requestObservability =
  databaseChildSpansFromAttributes requestStartMonotonicNanoseconds rootAttributes
  where
    rootSpan = Observability.observabilityRequestSpan requestObservability
    rootAttributes = Observability.requestSpanAttributes rootSpan
    requestStartMonotonicNanoseconds =
      requestSpanIntAttribute "harch.request.start_monotonic_ns" rootSpan

withoutDatabaseOperationAttributes :: Observability.RequestSpan -> Observability.RequestSpan
withoutDatabaseOperationAttributes requestSpan =
  requestSpan
    { Observability.requestSpanAttributes =
        filter
          (not . isDatabaseOperationAttribute)
          (Observability.requestSpanAttributes requestSpan)
    }

isDatabaseOperationAttribute :: Observability.ObservabilityAttribute -> Bool
isDatabaseOperationAttribute attribute =
  Observability.attributeName attribute
    `elem` [ "db.system",
             "db.operation.name",
             "db.query.template",
             "db.operation.start_monotonic_ns",
             "db.operation.duration_ns"
           ]

databaseChildSpansFromAttributes :: Maybe Word64 -> [Observability.ObservabilityAttribute] -> [(Text, Observability.RequestSpan)]
databaseChildSpansFromAttributes requestStartMonotonicNanoseconds =
  go
  where
    go currentAttributes =
      case currentAttributes of
        [] -> []
        systemAttribute@Observability.ObservabilityAttribute {Observability.attributeName = "db.system", Observability.attributeValue = Observability.TextAttribute _}
          : operationAttribute@Observability.ObservabilityAttribute {Observability.attributeName = "db.operation.name", Observability.attributeValue = Observability.TextAttribute operationName}
          : queryAttribute@Observability.ObservabilityAttribute {Observability.attributeName = "db.query.template", Observability.attributeValue = Observability.TextAttribute _}
          : startedAtAttribute@Observability.ObservabilityAttribute {Observability.attributeName = "db.operation.start_monotonic_ns", Observability.attributeValue = Observability.IntAttribute _}
          : durationAttribute@Observability.ObservabilityAttribute {Observability.attributeName = "db.operation.duration_ns", Observability.attributeValue = Observability.IntAttribute _}
          : remainingAttributes ->
            ( "SPAN_KIND_CLIENT",
              databaseOperationChildSpan
                requestStartMonotonicNanoseconds
                operationName
                systemAttribute
                operationAttribute
                queryAttribute
                [startedAtAttribute, durationAttribute]
            )
              : go remainingAttributes
        systemAttribute@Observability.ObservabilityAttribute {Observability.attributeName = "db.system", Observability.attributeValue = Observability.TextAttribute _}
          : operationAttribute@Observability.ObservabilityAttribute {Observability.attributeName = "db.operation.name", Observability.attributeValue = Observability.TextAttribute operationName}
          : queryAttribute@Observability.ObservabilityAttribute {Observability.attributeName = "db.query.template", Observability.attributeValue = Observability.TextAttribute _}
          : remainingAttributes ->
            ( "SPAN_KIND_CLIENT",
              databaseOperationChildSpan requestStartMonotonicNanoseconds operationName systemAttribute operationAttribute queryAttribute []
            )
              : go remainingAttributes
        _ : remainingAttributes ->
          go remainingAttributes

databaseOperationChildSpan ::
  Maybe Word64 ->
  Text ->
  Observability.ObservabilityAttribute ->
  Observability.ObservabilityAttribute ->
  Observability.ObservabilityAttribute ->
  [Observability.ObservabilityAttribute] ->
  Observability.RequestSpan
databaseOperationChildSpan requestStartMonotonicNanoseconds operationName systemAttribute operationAttribute queryAttribute timingAttributes =
  Observability.RequestSpan
    { Observability.requestSpanDisplayName =
        "DB " <> operationName,
      Observability.requestSpanAttributes =
        [systemAttribute, operationAttribute, queryAttribute]
          <> databaseOperationTimingAttributes requestStartMonotonicNanoseconds timingAttributes
    }

databaseOperationTimingAttributes :: Maybe Word64 -> [Observability.ObservabilityAttribute] -> [Observability.ObservabilityAttribute]
databaseOperationTimingAttributes requestStartMonotonicNanoseconds timingAttributes =
  case (requestStartMonotonicNanoseconds, attributeIntValue "db.operation.start_monotonic_ns" timingAttributes, attributeIntValue "db.operation.duration_ns" timingAttributes) of
    (Just requestStartedAt, Just operationStartedAt, Just operationDuration) ->
      [ intObservabilityAttribute
          "harch.span.start_offset_ns"
          (fromIntegral (operationStartedAt - min requestStartedAt operationStartedAt)),
        intObservabilityAttribute
          "harch.span.duration_ns"
          (fromIntegral operationDuration)
      ]
    _ -> []

attributeIntValue :: Text -> [Observability.ObservabilityAttribute] -> Maybe Word64
attributeIntValue expectedName attributes =
  listToMaybe
    [ fromIntegral attributeValue
    | Observability.ObservabilityAttribute
        { Observability.attributeName = currentName,
          Observability.attributeValue = Observability.IntAttribute attributeValue
        } <-
        attributes,
      currentName == expectedName,
      attributeValue >= 0
    ]

otlpRequestSpanStatusFields :: Observability.RequestObservability -> [(Text, LazyByteString.ByteString)]
otlpRequestSpanStatusFields requestObservability =
  case requestObservabilityStatusCode requestObservability of
    Just statusCode
      | statusCode >= 500 ->
          otlpErrorStatusFields
    _ -> []

otlpErrorStatusFields :: [(Text, LazyByteString.ByteString)]
otlpErrorStatusFields =
  [ ( "status",
      jsonObjectBytes
        [("code", jsonStringBytes "STATUS_CODE_ERROR")]
    )
  ]

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

shouldExportOtlpAttribute :: Observability.ObservabilityAttribute -> Bool
shouldExportOtlpAttribute attribute =
  not (isInternalTimingAttributeName (Observability.attributeName attribute))

isInternalTimingAttributeName :: Text -> Bool
isInternalTimingAttributeName attributeName =
  attributeName
    `elem` [ "harch.request.start_monotonic_ns",
             "harch.request.duration_ns",
             "harch.span.start_offset_ns",
             "harch.span.duration_ns",
             "db.operation.start_monotonic_ns",
             "db.operation.duration_ns"
           ]
    || ("harch.phase." `Text.isPrefixOf` attributeName && ".start_offset_ns" `Text.isSuffixOf` attributeName)
    || ("harch.phase." `Text.isPrefixOf` attributeName && ".duration_ns" `Text.isSuffixOf` attributeName)

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
  pure (otlpIdHexText traceIdBytes, otlpIdHexText spanIdBytes)

nextOtlpSpanId :: IO Text
nextOtlpSpanId = snd <$> nextOtlpSpanIdentifiers

otlpIdHexText :: ByteString.ByteString -> Text
otlpIdHexText =
  Text.concatMap renderHexByte . TextEncoding.decodeLatin1
  where
    renderHexByte byte =
      let byteValue = fromEnum byte
          highNibble = byteValue `div` 16
          lowNibble = byteValue `mod` 16
       in Text.pack [hexDigit highNibble, hexDigit lowNibble]

    hexDigit nibble =
      "0123456789abcdef" !! nibble

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

plannedBindEndpoints :: PlannedListener -> [ListenerEndpoint]
plannedBindEndpoints plannedListener =
  case plannedListener of
    PlannedHttp endpoint -> [endpoint]
    PlannedManualTls manualTlsBindPlan -> [tlsEndpoint manualTlsBindPlan]
    PlannedAcme acmeBindPlan -> maybeToList (acmeTlsEndpoint acmeBindPlan)

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
