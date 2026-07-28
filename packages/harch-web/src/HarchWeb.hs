{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module HarchWeb
  ( module HarchWeb.Acme,
    module HarchWeb.Document,
    ConnectionObservability (..),
    HttpServerMetrics (..),
    ObservabilityAttribute (ObservabilityAttribute),
    ObservabilityAttributeValue (..),
    RequestTraceContext (..),
    RequestObservability (..),
    RequestSpan (..),
    ResponseKind (..),
    buildConnectionObservability,
    buildRequestObservability,
    forceConnectionObservability,
    forceRequestObservability,
    requestObservabilityAttributes,
    requestSpanName,
    withRequestTraceContext,
    module HarchWeb.Routing,
    module HarchWeb.Security,
    module HarchWeb.Server,
    module HarchWeb.StaticAssets,
    LocalTestServer (..),
    ObservabilityStartupPlan (..),
    ReloadingTlsCredentials,
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
import Control.Monad (void)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List (find)
import Data.Maybe (fromMaybe, isNothing, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
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
    defaultCaptureKernel,
    defaultCaptureKernelScript,
    defaultNavigationRuntime,
    defaultNavigationRuntimeScript,
    generateRuntimeNonce,
    liveRegionAttributes,
    renderDocument,
    renderDocumentWithNonce,
  )
import HarchWeb.Observability
import HarchWeb.Observability qualified as Observability
import HarchWeb.Observability.Otlp qualified as Otlp
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
import Network.Socket qualified as Socket
import Network.Wai qualified as Wai
import System.IO (Handle, hFlush, hPutStrLn)
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

navigationRuntimeScriptSource :: Text -> NavigationRuntime -> Text
navigationRuntimeScriptSource pathPrefix runtime =
  applyRequestPathPrefix pathPrefix (navigationRuntimePath runtime)

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

exportRequestObservabilityToOtlp ::
  Text ->
  OtlpExporter ->
  Observability.RequestObservability ->
  IO ()
exportRequestObservabilityToOtlp serviceName exporter requestObservability = do
  (generatedTraceId, spanId) <- Otlp.nextOtlpSpanIdentifiers
  let childSpans =
        requestRuntimePhaseChildSpans requestObservability
          <> requestDatabaseChildSpans requestObservability
  childSpanIds <- mapM (const Otlp.nextOtlpSpanId) childSpans
  endTimeUnixNano <- Otlp.currentUnixTimeNSec
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
  Otlp.sendOtlpTraceRequest exporter requestBody

exportConnectionObservabilityToOtlp ::
  Text ->
  OtlpExporter ->
  Observability.ConnectionObservability ->
  IO ()
exportConnectionObservabilityToOtlp serviceName exporter connectionObservability = do
  (traceId, spanId) <- Otlp.nextOtlpSpanIdentifiers
  endTimeUnixNano <- Otlp.currentUnixTimeNSec
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
  Otlp.sendOtlpTraceRequest exporter requestBody

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
