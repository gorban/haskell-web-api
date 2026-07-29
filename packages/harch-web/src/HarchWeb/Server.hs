{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Typed application, request, response, and middleware contracts.
--
-- The framework facade re-exports this module. Request execution, WAI
-- rendering, and transport runtime implementation build on these acyclic types
-- in subsequent server-focused modules.
module HarchWeb.Server
  ( module HarchWeb.Server.Config,
    Application (..),
    ClientActionRequest (..),
    ClientActionResponse (..),
    MiddlewareResult (..),
    RegionPatch (..),
    RequestMiddleware (..),
    Response (..),
    ResponseBody (..),
    ResponseDiagnostics (..),
    ServerSentEvent (..),
    ServerSentEventSource (..),
    application,
    applyResponseHeaders,
    clientActionResponseBody,
    eventStreamResponse,
    isClientActionRequest,
    parseClientActionFields,
    redirectResponse,
    renderServerSentEvent,
    runEarlyRequestStages,
    responseDiagnostics,
    responseKind,
    responsePolicyHeaders,
    responseStatusCode,
    reportEarlyRequestObservability,
    serverSentEventContentType,
    runRequestMiddlewarePipeline,
    serverSentEventSourceFromList,
    navigationRuntimeResponse,
    planServerStartup,
    toWaiApplication,
    toWaiBodyResponse,
    toWaiResponse,
  )
where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (bimap)
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder qualified as ByteStringBuilder
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Foldable (for_)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.Encoding.Error qualified as TextEncodingError
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import HarchWeb.Document (Document, NavigationRuntime, Page)
import HarchWeb.Document qualified as Document
import HarchWeb.Observability qualified as Observability
import HarchWeb.Routing (RouteCodec (..), RouteRequest (..), matchRoute, renderRoute)
import HarchWeb.Security
  ( RequestPolicyConfig,
    applyRequestPathPrefix,
    corsPreflightResponse,
    externalRequestPath,
    httpsRedirectResponse,
    prependRequestLogContext,
    requestContextObservabilityAttributes,
    requestLogContextFields,
    requestPathPrefix,
    requestPolicyResponseHeaders,
    requestPolicyResponseHeadersWithNonce,
    requestRedirectLocation,
    requestScheme,
    requestTraceContext,
    waiRequestPath,
    waiRequestRouteTarget,
  )
import HarchWeb.Server.Config
import HarchWeb.Server.Response
import HarchWeb.Server.Sse
import HarchWeb.Server.StaticAssets (serveStaticAssetResponse)
import HarchWeb.StaticAssets (StaticAssetsConfig (..))
import Network.HTTP.Types qualified as Http
import Network.HTTP.Types.URI qualified as HttpUri
import Network.Wai qualified as Wai

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

data PlannedListener
  = PlannedHttp ListenerEndpoint
  | PlannedManualTls ManualTlsBindPlan
  | PlannedAcme AcmeBindPlan

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

application :: Application route context -> Application route context
application = id

applyResponseHeaders :: Http.ResponseHeaders -> Wai.Response -> Wai.Response
applyResponseHeaders additionalHeaders =
  Wai.mapResponseHeaders (additionalHeaders <>)

responsePolicyHeaders :: RequestPolicyConfig -> Wai.Request -> Document.RuntimeNonce -> Response route context -> Http.ResponseHeaders
responsePolicyHeaders requestPolicyConfig request runtimeNonce response =
  requestPolicyResponseHeadersWithNonce
    requestPolicyConfig
    request
    ( case response of
        PageResponse _ -> Just runtimeNonce
        PageResponseWithMetadata _ _ -> Just runtimeNonce
        BodyResponse _ -> Nothing
        RedirectResponse _ _ -> Nothing
        ClientActionBodyResponse _ -> Nothing
        EventStreamResponse _ _ -> Nothing
    )

navigationRuntimeResponse :: NavigationRuntime -> Text -> Maybe ResponseBody
navigationRuntimeResponse runtime requestPath =
  if requestPath == Document.navigationRuntimePath runtime
    then
      Just
        ResponseBody
          { responseStatus = 200,
            responseContentType = "application/javascript; charset=utf-8",
            responseBody = Document.navigationRuntimeScript runtime,
            responseObservabilityAttributes = [],
            responseLogEntries = []
          }
    else Nothing

-- | Handle framework-owned responses before routing or application middleware.
-- Each response receives the request policy headers exactly once.
runEarlyRequestStages :: Application route context -> Wai.Request -> Text -> Http.ResponseHeaders -> ExceptT (Text, Wai.Response) IO ()
runEarlyRequestStages webApplication request requestPath policyResponseHeaders = do
  let requestPolicyConfig = applicationRequestPolicy webApplication
      earlyResponse path = throwError . (path,) . applyResponseHeaders policyResponseHeaders
  for_ (corsPreflightResponse requestPolicyConfig request) $
    earlyResponse (externalRequestPath requestPolicyConfig request)
  for_ (requestRedirectLocation requestPolicyConfig request) $ \redirectLocation ->
    earlyResponse (externalRequestPath requestPolicyConfig request) (httpsRedirectResponse redirectLocation)
  for_ (applicationNavigationRuntime webApplication >>= (`navigationRuntimeResponse` requestPath)) $
    earlyResponse requestPath . toWaiBodyResponse []
  maybeStaticResponse <- liftIO (serveStaticAssetResponse (applicationStaticAssets webApplication) requestPath)
  for_ maybeStaticResponse $ \(staticRoutePath, staticResponse) ->
    earlyResponse
      (applyRequestPathPrefix (requestPathPrefix requestPolicyConfig request) staticRoutePath)
      staticResponse

data RequestExecutionTimings = RequestExecutionTimings
  { requestExecutionStartedAt :: Word64,
    requestPolicyEvaluatedAt :: Word64,
    requestMiddlewareTimings :: [(Text, Word64, Word64)],
    requestRouteMatchingStartedAt :: Word64,
    requestRouteMatchedAt :: Word64,
    requestRenderingStartedAt :: Word64,
    requestResponseRenderedAt :: Word64
  }

-- | Adapt a typed application to WAI. Framework-owned early responses,
-- middleware, route dispatch, and finalization all converge here.
toWaiApplication :: (Eq route) => Application route context -> Wai.Application
toWaiApplication webApplication request respond = do
  requestStartedAt <- getMonotonicTimeNSec
  let requestPolicyConfig = applicationRequestPolicy webApplication
      policyResponseHeaders = requestPolicyResponseHeaders requestPolicyConfig request
      requestPath = waiRequestPath requestPolicyConfig request
  policyEvaluatedAt <- policyResponseHeaders `seq` getMonotonicTimeNSec
  earlyResult <- runExceptT (runEarlyRequestStages webApplication request requestPath policyResponseHeaders)
  case earlyResult of
    Left (earlyResponsePath, earlyResponseValue) -> do
      responseReportedAt <- earlyResponseValue `seq` getMonotonicTimeNSec
      reportEarlyRequestObservability webApplication request requestStartedAt responseReportedAt earlyResponsePath earlyResponseValue
      respond earlyResponseValue
    Right () -> handleRoutedRequest webApplication request respond requestStartedAt policyEvaluatedAt requestPolicyConfig requestPath

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
  let executionTimings =
        RequestExecutionTimings
          { requestExecutionStartedAt = requestStartedAt,
            requestPolicyEvaluatedAt = policyEvaluatedAt,
            requestMiddlewareTimings = middlewareTiming,
            requestRouteMatchingStartedAt = routeMatchingStartedAt,
            requestRouteMatchedAt = routeMatchedAt,
            requestRenderingStartedAt = renderStartedAt,
            requestResponseRenderedAt = responseRenderedAt
          }
  finalizeRoutedResponse webApplication request respond executionTimings requestPolicyConfig requestPath routeRequest runtimeNonce response

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

responseRuntimeNonce :: Response route context -> IO Document.RuntimeNonce
responseRuntimeNonce response =
  case response of
    PageResponse _ -> Document.generateRuntimeNonce
    PageResponseWithMetadata _ _ -> Document.generateRuntimeNonce
    BodyResponse _ -> pure $! Document.RuntimeNonce ""
    RedirectResponse _ _ -> pure $! Document.RuntimeNonce ""
    ClientActionBodyResponse _ -> pure $! Document.RuntimeNonce ""
    EventStreamResponse _ _ -> pure $! Document.RuntimeNonce ""

finalizeRoutedResponse :: (Eq route) => Application route context -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> RequestExecutionTimings -> RequestPolicyConfig -> Text -> RouteRequest route context -> Document.RuntimeNonce -> Response route context -> IO Wai.ResponseReceived
finalizeRoutedResponse webApplication request respond executionTimings requestPolicyConfig requestPath routeRequest runtimeNonce response = do
  let requestLogFields = requestLogContextFields requestPolicyConfig request
      diagnosticValues = responseDiagnostics response
      contextualizedLogs = map (prependRequestLogContext requestLogFields) (diagnosticLogEntries diagnosticValues)
      observabilityValue = buildRoutedRequestObservability webApplication request executionTimings requestPolicyConfig requestPath routeRequest response diagnosticValues
  Observability.forceRequestObservability observabilityValue `seq`
    reportRequestObservability webApplication observabilityValue
      >> mapM_ (reportApplicationLog webApplication) contextualizedLogs
      >> respond (applyResponseHeaders (responsePolicyHeaders requestPolicyConfig request runtimeNonce response) (toWaiResponse [] runtimeNonce webApplication response))

buildRoutedRequestObservability :: (Eq route) => Application route context -> Wai.Request -> RequestExecutionTimings -> RequestPolicyConfig -> Text -> RouteRequest route context -> Response route context -> ResponseDiagnostics -> Observability.RequestObservability
buildRoutedRequestObservability webApplication request executionTimings requestPolicyConfig requestPath routeRequest response diagnosticValues =
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
          <> requestTimingObservabilityAttributes
            (requestExecutionStartedAt executionTimings)
            (requestResponseRenderedAt executionTimings)
            ( [("request-policy", requestExecutionStartedAt executionTimings, requestPolicyEvaluatedAt executionTimings)]
                <> requestMiddlewareTimings executionTimings
                <> [ ("route-match", requestRouteMatchingStartedAt executionTimings, requestRouteMatchedAt executionTimings),
                     ("render-response", requestRenderingStartedAt executionTimings, requestResponseRenderedAt executionTimings)
                   ]
            )
      )

requestTimingObservabilityAttributes :: Word64 -> Word64 -> [(Text, Word64, Word64)] -> [Observability.ObservabilityAttribute]
requestTimingObservabilityAttributes requestStartedAt requestCompletedAt phaseTimings =
  intObservabilityAttribute "harch.request.start_monotonic_ns" (fromIntegral requestStartedAt)
    : intObservabilityAttribute "harch.request.duration_ns" (nanosecondsBetween requestStartedAt requestCompletedAt)
    : concatMap phaseTimingAttributes phaseTimings
  where
    phaseTimingAttributes (phaseName, phaseStartedAt, phaseEndedAt) =
      [ intObservabilityAttribute ("harch.phase." <> phaseName <> ".start_offset_ns") (nanosecondsBetween requestStartedAt phaseStartedAt),
        intObservabilityAttribute ("harch.phase." <> phaseName <> ".duration_ns") (nanosecondsBetween phaseStartedAt phaseEndedAt)
      ]

nanosecondsBetween :: Word64 -> Word64 -> Int
nanosecondsBetween start end = fromIntegral (end - min start end)

intObservabilityAttribute :: Text -> Int -> Observability.ObservabilityAttribute
intObservabilityAttribute name value =
  Observability.ObservabilityAttribute
    { Observability.attributeName = name,
      Observability.attributeValue = Observability.IntAttribute value
    }

reportEarlyRequestObservability :: (Eq route) => Application route context -> Wai.Request -> Word64 -> Word64 -> Text -> Wai.Response -> IO ()
reportEarlyRequestObservability webApplication request requestStartedAt requestCompletedAt routePath response =
  let requestPolicyConfig = applicationRequestPolicy webApplication
      requestObservability =
        maybe id Observability.withRequestTraceContext (requestTraceContext request) $
          Observability.buildRequestObservability
            (TextEncoding.decodeUtf8 (Wai.requestMethod request))
            (requestScheme requestPolicyConfig request)
            (waiRequestPath requestPolicyConfig request)
            routePath
            (Http.statusCode (Wai.responseStatus response))
            Observability.BodyResponseKind
            (requestContextObservabilityAttributes requestPolicyConfig request <> requestTimingObservabilityAttributes requestStartedAt requestCompletedAt [])
   in Observability.forceRequestObservability requestObservability `seq`
        reportRequestObservability webApplication requestObservability

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
    ClientActionBodyResponse actionResponse ->
      ResponseDiagnostics
        (clientActionObservabilityAttributes actionResponse)
        (clientActionLogEntries actionResponse)
    EventStreamResponse responseBodyValue _ -> responseBodyDiagnostics responseBodyValue

responseBodyDiagnostics :: ResponseBody -> ResponseDiagnostics
responseBodyDiagnostics responseBodyValue =
  ResponseDiagnostics
    { diagnosticObservabilityAttributes = responseObservabilityAttributes responseBodyValue,
      diagnosticLogEntries = responseLogEntries responseBodyValue
    }

responseStatusCode :: (Eq route) => Application route context -> Response route context -> Int
responseStatusCode webApplication response =
  case response of
    PageResponse page -> if isNotFoundPage webApplication page then 404 else 200
    PageResponseWithMetadata responseBodyValue _ -> responseStatus responseBodyValue
    BodyResponse responseBodyValue -> responseStatus responseBodyValue
    RedirectResponse responseBodyValue _ -> responseStatus responseBodyValue
    ClientActionBodyResponse actionResponse -> clientActionStatus actionResponse
    EventStreamResponse responseBodyValue _ -> responseStatus responseBodyValue

responseKind :: Response route context -> Observability.ResponseKind
responseKind response =
  case response of
    PageResponse _ -> Observability.PageResponseKind
    PageResponseWithMetadata _ _ -> Observability.PageResponseKind
    BodyResponse _ -> Observability.BodyResponseKind
    RedirectResponse _ _ -> Observability.BodyResponseKind
    ClientActionBodyResponse _ -> Observability.BodyResponseKind
    EventStreamResponse _ _ -> Observability.BodyResponseKind

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
        HaltMiddleware haltedRequestContext responseBodyValue -> pure (HaltMiddleware haltedRequestContext responseBodyValue)

toWaiResponse :: (Eq route) => Http.ResponseHeaders -> Document.RuntimeNonce -> Application route context -> Response route context -> Wai.Response
toWaiResponse additionalHeaders runtimeNonce webApplication response =
  case response of
    PageResponse page ->
      Wai.responseLBS
        (if isNotFoundPage webApplication page then Http.status404 else Http.status200)
        (additionalHeaders <> [(Http.hContentType, TextEncoding.encodeUtf8 htmlContentType)])
        (LazyByteString.fromStrict (TextEncoding.encodeUtf8 (Document.renderDocumentWithNonce runtimeNonce (pageShell webApplication page))))
    PageResponseWithMetadata pageResponseBodyValue page ->
      let !pageStatusMessage = ByteString.empty
          !pageStatusMessageLength = ByteString.length pageStatusMessage
          !pageStatus = pageStatusMessageLength `seq` Http.Status (responseStatus pageResponseBodyValue) pageStatusMessage
       in Wai.responseLBS
            pageStatus
            (additionalHeaders <> [(Http.hContentType, TextEncoding.encodeUtf8 htmlContentType)])
            (LazyByteString.fromStrict (TextEncoding.encodeUtf8 (Document.renderDocumentWithNonce runtimeNonce (pageShell webApplication page))))
    BodyResponse responseBodyValue -> toWaiBodyResponse additionalHeaders responseBodyValue
    RedirectResponse responseBodyValue location -> toWaiBodyResponse (additionalHeaders <> [(Http.hLocation, TextEncoding.encodeUtf8 location)]) responseBodyValue
    ClientActionBodyResponse actionResponse -> toWaiBodyResponse (additionalHeaders <> clientActionHeaders actionResponse) (clientActionResponseBody actionResponse)
    EventStreamResponse responseBodyValue eventSource -> toWaiEventStreamResponse additionalHeaders responseBodyValue eventSource

toWaiBodyResponse :: Http.ResponseHeaders -> ResponseBody -> Wai.Response
toWaiBodyResponse additionalHeaders responseBodyValue =
  Wai.responseLBS
    (Http.mkStatus (responseStatus responseBodyValue) mempty)
    (additionalHeaders <> [(Http.hContentType, TextEncoding.encodeUtf8 (responseContentType responseBodyValue))])
    (LazyByteString.fromStrict (TextEncoding.encodeUtf8 (responseBody responseBodyValue)))

toWaiEventStreamResponse :: Http.ResponseHeaders -> ResponseBody -> ServerSentEventSource -> Wai.Response
toWaiEventStreamResponse additionalHeaders responseBodyValue eventSource =
  Wai.responseStream
    (Http.mkStatus (responseStatus responseBodyValue) mempty)
    ( additionalHeaders
        <> [ (Http.hContentType, TextEncoding.encodeUtf8 (responseContentType responseBodyValue)),
             ("Cache-Control", "no-cache"),
             ("X-Accel-Buffering", "no")
           ]
    )
    streamEvents
  where
    streamEvents write flush = do
      maybeEvent <- nextServerSentEvent eventSource
      for_ maybeEvent $ \event -> do
        write (ByteStringBuilder.byteString (TextEncoding.encodeUtf8 (renderServerSentEvent event)))
        flush
        streamEvents write flush

isClientActionRequest :: Wai.Request -> Bool
isClientActionRequest request = lookup "X-Harch-Action" (Wai.requestHeaders request) == Just "1"

parseClientActionFields :: LazyByteString.ByteString -> [(Text, Text)]
parseClientActionFields requestBody =
  map (bimap decodeActionField (maybe "" decodeActionField)) (HttpUri.parseQuery (LazyByteString.toStrict requestBody))

decodeActionField :: ByteString.ByteString -> Text
decodeActionField = TextEncoding.decodeUtf8With TextEncodingError.lenientDecode

clientActionResponseBody :: ClientActionResponse -> ResponseBody
clientActionResponseBody actionResponse =
  ResponseBody
    { responseStatus = clientActionStatus actionResponse,
      responseContentType = "application/json; charset=utf-8",
      responseBody = renderClientActionResponse actionResponse,
      responseObservabilityAttributes = clientActionObservabilityAttributes actionResponse,
      responseLogEntries = clientActionLogEntries actionResponse
    }

renderClientActionResponse :: ClientActionResponse -> Text
renderClientActionResponse actionResponse =
  "{\"patches\":["
    <> Text.intercalate "," (map renderPatch (clientActionPatches actionResponse))
    <> "],\"focusId\":"
    <> maybe "null" jsonString (clientActionFocusId actionResponse)
    <> "}"
  where
    renderPatch RegionPatch {regionPatchId, regionPatchHtml} =
      "{\"id\":" <> jsonString regionPatchId <> ",\"html\":" <> jsonString regionPatchHtml <> "}"

jsonString :: Text -> Text
jsonString textValue = "\"" <> Text.concatMap escapeJsonCharacter textValue <> "\""

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

isNotFoundPage :: (Eq route) => Application route context -> Page route context -> Bool
isNotFoundPage webApplication page =
  let pageRequestContext = Document.pageContext page
   in pageRequestContext `seq`
        Document.pageRoute page == requestRoute (notFoundRequest (routeCodec webApplication) pageRequestContext)

htmlContentType :: Text
htmlContentType = "text/html; charset=utf-8"
