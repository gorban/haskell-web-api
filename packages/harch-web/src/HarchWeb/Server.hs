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
import Data.Foldable (for_)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import HarchWeb.Document (NavigationRuntime)
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
    requestRedirectLocation,
    requestScheme,
    requestTraceContext,
    waiRequestPath,
    waiRequestRouteTarget,
  )
import HarchWeb.Server.Application
import HarchWeb.Server.ClientAction
import HarchWeb.Server.Config
import HarchWeb.Server.Response
import HarchWeb.Server.ResponseRendering
import HarchWeb.Server.Sse
import HarchWeb.Server.StaticAssets (serveStaticAssetResponse)
import Network.HTTP.Types qualified as Http
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
