{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module HarchWeb
  ( module HarchWeb.Acme,
    module HarchWeb.Document,
    module HarchWeb.Routing,
    module HarchWeb.Security,
    module HarchWeb.Server,
    module HarchWeb.StaticAssets,
    LocalTestServer (..),
    ObservabilityStartupPlan (..),
    ReloadingTlsCredentials,
    defaultCaptureKernel,
    defaultCaptureKernelScript,
    defaultNavigationRuntime,
    defaultNavigationRuntimeScript,
    exportConnectionObservabilityToOtlp,
    exportRequestObservabilityToOtlp,
    navigationRuntimeScriptSource,
    planObservabilityStartup,
    reloadTlsCredentialsIfChanged,
    loadReloadingTlsCredentials,
    loadTlsCredentialSnapshotOrThrowWithLoader,
    startManualTlsRuntimeServerWithStarter,
    runServer,
    startWarpRuntimeServerOnSocket,
    withLocalTestServer,
  )
where

import Control.Concurrent (ThreadId, killThread, newEmptyMVar, newMVar, takeMVar, tryPutMVar)
import Control.Exception (bracket, finally)
import Control.Monad (unless, void)
import Data.Bits (shiftR, xor)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.List (find)
import Data.Maybe (fromMaybe, isNothing, listToMaybe, mapMaybe)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import HarchWeb.Acme
import HarchWeb.Acme.Certbot.Runtime (runtimeAcmeBindPlans, startAcmeRuntimeServers, stopAcmeRuntimeServers)
import HarchWeb.Acme.Challenge (acmeChallengeRoutePath)
import HarchWeb.Document
  ( Document (..),
    HtmlAttribute (..),
    LiveRegion (..),
    NavigationItem (..),
    NavigationRuntime (..),
    Page (..),
    PageShell (..),
    ResolvedNavigationItem (..),
    RuntimeDescriptor (..),
    RuntimeNonce (..),
    buildDocument,
    buildNavigation,
    buildPageShell,
    generateRuntimeNonce,
    liveRegionAttributes,
    renderDocument,
    renderDocumentWithNonce,
  )
import HarchWeb.Observability qualified as Observability
import HarchWeb.Routing (RouteCodec (..), RouteRequest (..), matchRoute, routeHref)
import HarchWeb.Security
import HarchWeb.Server
import HarchWeb.Server.Transport
  ( ReloadingTlsCredentials,
    listenerSchemeText,
    loadReloadingTlsCredentials,
    loadTlsCredentialSnapshotOrThrowWithLoader,
    openLoopbackSocket,
    reloadTlsCredentialsIfChanged,
    socketPort,
    startHttpRuntimeServers,
    startManualTlsRuntimeServerWithStarter,
    startManualTlsRuntimeServers,
    startWarpRuntimeServerOnSocket,
    startWarpServerOnSocket,
    stopRuntimeServers,
  )
import HarchWeb.StaticAssets
  ( AssetPath (..),
    CssClass (..),
    CssScope (..),
    StaticAssetRoot (..),
    StaticAssetsConfig (..),
    Stylesheet (..),
    cssClassText,
    cssScope,
    defaultStaticAssetContentTypes,
    staticAssetHref,
    staticAssetHrefWithPrefix,
    stylesheet,
  )
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Client.TLS qualified as HttpClientTls
import Network.HTTP.Types qualified as Http
import Network.Socket qualified as Socket
import Network.Wai qualified as Wai
import System.IO (Handle, hFlush, hPutStrLn)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Signals (Handler (Catch), installHandler, sigINT, sigTERM)
import Text.Read (readMaybe)

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

navigationRuntimeScriptSource :: Text -> NavigationRuntime -> Text
navigationRuntimeScriptSource pathPrefix runtime =
  applyRequestPathPrefix pathPrefix (navigationRuntimePath runtime)

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
    endpointHost endpoint `seq`
      startWarpServerOnSocket endpoint listeningSocket (toWaiApplication webApplication)
  localPort `seq`
    pure
      RunningLocalTestServer
        { runningLocalServerInfo =
            LocalTestServer
              { localServerHost = "127.0.0.1",
                localServerPort = localPort,
                localServerBaseUrl = listenerSchemeText listenerScheme <> "://127.0.0.1:" <> Text.pack (show localPort)
              },
          runningLocalServerSocket = listeningSocket,
          runningLocalServerThreadId = serverThreadId
        }

stopLocalTestServer :: RunningLocalTestServer -> IO ()
stopLocalTestServer runningServer = do
  Socket.close (runningLocalServerSocket runningServer)
  killThread (runningLocalServerThreadId runningServer)

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
