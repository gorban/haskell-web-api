{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, readMVar)
import Control.Exception ()
import Control.Monad (forM_)
import Data.ByteString qualified as ByteString (drop, intercalate, isInfixOf, isPrefixOf, replicate, takeWhile)
import Data.ByteString.Builder qualified as Builder (byteString)
import Data.ByteString.Char8 qualified as ByteStringChar8 ()
import Data.ByteString.Lazy qualified as LazyByteString (fromStrict)
import Data.Char ()
import Data.Either (fromRight)
import Data.Functor.Compose (Compose (..))
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as Text (breakOn, drop, isInfixOf, pack, replace, stripPrefix)
import Data.Text.Encoding qualified as TextEncoding (decodeUtf8, encodeUtf8)
import HarchWeb (ActionNavigation (NavigateInternal, StayOnCurrentRoute), Application (applicationNavigationRuntime, applicationRequestMiddleware, applicationRequestPolicy, csrfProtection, decodeClientAction, handleClientAction, pageShell, renderRequestResponse, reportApplicationLog, reportRequestObservability, requestContextFromRequest, routeCodec, routeExecutionPolicy), ClientActionDecodeResult (DecodedClientAction, UnrecognizedClientAction), ClientActionPayload (clientActionCsrfToken, clientActionFields, clientActionIdempotencyKey, clientActionMethod), ClientActionRequest (ClientActionRequest, clientAction, clientActionContext, clientActionRequestIdempotencyKey), ClientActionResponse (ClientActionResponse, clientActionFocusId, clientActionHeaders, clientActionLogEntries, clientActionNavigation, clientActionObservabilityAttributes, clientActionPatches, clientActionStatus), CorsPolicyConfig (CorsPolicyConfig, corsAllowedHeaders, corsAllowedMethods, corsAllowedOrigins, corsMaxAgeSeconds), Document (documentRuntimeDescriptors), ForwardedHeaderTrust (NeverTrustForwarded), HistoryMode (ReplaceHistory), MiddlewareResult (ContinueMiddleware, HaltMiddleware), Page (pageRoute), ProtocolResponse (ProtocolResponse, protocolResponseBody, protocolResponseDatabaseOperations, protocolResponseHeaders, protocolResponseLogEntries, protocolResponseObservabilityAttributes, protocolResponseStatus), ProtocolResponseBody (ProtocolResponseBytes, ProtocolResponseStream, ProtocolResponseWai), RequestMiddleware (RequestMiddleware), RequestPolicyConfig (RequestPolicyConfig, corsPolicy, forwardedHeaderTrust, httpsRedirectAuthority, httpsRedirectPort, redirectHttpToHttps, requestConcurrencyLimit, requestHeadLimits, requestTransportLimits, responseSecurityHeaders, strictTransportSecurity), Response (BodyResponse, ClientActionBodyResponse, EventStreamResponse, PageResponse, PageResponseWithMetadata, ProtocolResponseResult), ResponseBody (ResponseBody, responseBody, responseContentType, responseDatabaseOperations, responseLogEntries, responseObservabilityAttributes, responseStatus), ResponseDiagnostics (diagnosticLogEntries, diagnosticObservabilityAttributes), ResponseSecurityHeadersConfig (ResponseSecurityHeadersConfig, contentSecurityPolicy, contentTypeOptionsNoSniff, frameOptions, permissionsPolicy, referrerPolicy, xssProtection), RouteExecutionPolicy (RouteExecutionPolicy), RouteRequest (RouteRequest, requestContext, requestRoute), RuntimeDescriptor (InlineBootstrap), ServerSentEvent (ServerSentEvent), StaticAssetRoot (StaticAssetRoot, staticDirectory, staticUrlPrefix), StaticAssetsConfig (StaticAssetsConfig, staticAssetContentTypes, staticAssetRoots, staticCacheControlSeconds), StrictTransportSecurityConfig (StrictTransportSecurityConfig, strictTransportSecurityIncludeSubDomains, strictTransportSecurityMaxAgeSeconds, strictTransportSecurityPreload), clientActionResponseBody, defaultContentSecurityPolicy, defaultCorsPolicyConfig, defaultNavigationRuntime, defaultResponseSecurityHeadersConfig, defaultStaticAssetContentTypes, eventStreamResponse, internalRedirectResponse, isClientActionRequest, literalElementId, mkRequestConcurrencyLimit, parseClientActionFields, redirectResponse, responseDiagnostics, responseKind, responseStatusCode, serverSentEventSourceFromList, toWaiApplication, toWaiResponse, unboundedRequestHeadLimits, unboundedRouteExecutionPolicy, warpDefaultRequestTransportLimits)
import HarchWeb qualified
import HarchWeb.Action qualified as Action (ActionDecoder, action, actionCodec, decodeAction, post)
import HarchWeb.Csrf (csrfTokenText)
import HarchWeb.Database qualified as Database (DatabaseOperation (DatabaseOperation, databaseOperationEndedAtNanoseconds, databaseOperationName, databaseOperationStartedAtNanoseconds, databaseOperationSystem, databaseQueryTemplate))
import HarchWeb.Markup.Unsafe qualified as MarkupUnsafe ()
import HarchWeb.Observability qualified as Observability (ObservabilityAttribute (ObservabilityAttribute, attributeName, attributeValue), ObservabilityAttributeValue (TextAttribute), RequestIdentity (RequestIdentity, requestIdentityMethod, requestIdentityPath, requestIdentityRoutePath, requestIdentityScheme), RequestObservability (observabilityRequestSpan), RequestSpan (requestSpanAttributes, requestSpanDisplayName), RequestTraceContext (RequestTraceContext, traceContextParentSpanId, traceContextState, traceContextTraceId), ResponseKind (BodyResponseKind, PageResponseKind), buildRequestObservability, mkSpanMethodLabel, mkSpanRoutePath, withDatabaseOperations, withRequestTraceContext)
import HarchWeb.Security qualified as Security ()
import Network.HTTP.Client qualified as HttpClient ()
import Network.HTTP.Types qualified as Http (ResponseHeaders, Status (statusCode, statusMessage), hAcceptRanges, hAllow, hCacheControl, hContentLength, hContentRange, hContentType, hETag, hIfModifiedSince, hIfNoneMatch, hLastModified, hLocation, hRange, status200, status201, status202, status204, status206, status302, status303, status304, status308, status400, status401, status403, status404, status405, status413, status415, status416, status422, status500, status503)
import Network.Socket qualified as Socket (SockAddr (SockAddrInet, SockAddrUnix), tupleToHostAddress)
import Network.Socket.ByteString qualified as SocketByteString ()
import Network.Wai qualified as Wai (Request (isSecure, pathInfo, rawPathInfo, rawQueryString, requestHeaders, requestMethod), defaultRequest, responseHeaders, responseLBS, responseStatus, setRequestBodyChunks)
import Network.Wai.Handler.Warp qualified as Warp ()
import System.Directory (createDirectoryIfMissing, createFileLink)
import System.Environment ()
import System.Exit ()
import System.FilePath ()
import System.IO ()
import System.IO.Error ()
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Signals ()
import System.Process ()
import TestCore.CustomAssertions ()
import TestCore.Wai (nextRequestBodyChunk, performWaiRequest, readResponseBody, waiRequest)
import Text.Read ()
import Unit.HarchWeb.TestSupport (TestContext (requestLanguage, testContextPathPrefix), TestRoute (DataRoute, EventStreamRoute, KnownRoute, MissingRoute), defaultContext, defaultRequestPolicy, emptyStaticAssets, expectMeasuredRequestTiming, expectMeasuredRootRequestTiming, hasTextAttribute, renderDocument, renderSampleResponse, rootPathApplication, sampleApplication, sampleApplicationWithConfig, sampleApplicationWithStaticAssets, samplePage, sampleRequestContextFromRequest, spanishContext, stripVolatileRequestTiming, testActionCodec, testPageSecurity, testRegionPatch, testTrustedForwardedProxy, trustedForwardedApplication, waiRequestWithRemoteHostAndHeaders, waitUntilIORefEquals)

spec = do
  describe "toWaiApplication" $ do
    it "fails closed if a page response reaches rendering without its CSP nonce" $ do
      let pageResponse = PageResponse testPageSecurity (samplePage (RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext}))
          response = toWaiResponse [] Nothing sampleApplication pageResponse
      Wai.responseStatus response `shouldBe` Http.status500
      Wai.responseHeaders response `shouldBe` [(Http.hContentType, "text/html; charset=utf-8")]
      readResponseBody response `shouldReturn` "A page response was missing its CSP nonce."

    it "attaches the matched root route facts to a guard security event" $ do
      emitted <- newIORef []
      let delivery =
            HarchWeb.SecurityEventDelivery $ \eventEnvelope -> do
              modifyIORef' emitted (<> [eventEnvelope])
              pure HarchWeb.SecurityEventDelivered
          eventRoot =
            HarchWeb.SecurityEventRoot
              { HarchWeb.securityEventRootModule = requiredModuleName "root.web",
                HarchWeb.securityEventRootLocale = HarchWeb.locale . requestLanguage,
                HarchWeb.securityEventRootDelivery = delivery,
                HarchWeb.securityEventRootUndelivered = const (pure ())
              }
          eventBody = HarchWeb.AuthenticationEvaluated (HarchWeb.AuthenticationEvent HarchWeb.AuthenticationMissing Nothing)
          guardedApplication =
            sampleApplication
              { HarchWeb.applicationSecurity =
                  HarchWeb.AuthenticationEnabled
                    []
                    ( HarchWeb.AuthenticationGuard $ \endpointRequest ->
                        case HarchWeb.endpointSecurityEventSink endpointRequest of
                          Nothing -> pure (HarchWeb.HaltEndpoint (HarchWeb.NonPageBodyResponse securityEventUnavailableResponse))
                          Just sink -> do
                            _ <- HarchWeb.emitSecurityEvent sink HarchWeb.TelemetryBestEffort eventBody
                            pure (HarchWeb.ContinueEndpoint (requestContext (HarchWeb.endpointRouteRequest endpointRequest)))
                    )
                    [],
                HarchWeb.applicationSecurityEventRoot = Just eventRoot,
                HarchWeb.applicationRouteModuleChain =
                  Just $ \case
                    KnownRoute -> requiredModuleName "root.web" :| [requiredModuleName "catalog"]
                    routeValue -> error ("unexpected test route: " <> show routeValue),
                HarchWeb.routeEndpointMetadata = \case
                  KnownRoute -> protectedEndpointMetadata
                  routeValue -> error ("unexpected test route: " <> show routeValue)
              }
          expectedEnvelope =
            HarchWeb.SecurityEventEnvelope
              { HarchWeb.securityEventRoute =
                  HarchWeb.RouteObservation
                    { HarchWeb.observedEndpointName = requiredEndpointName "test.protected",
                      HarchWeb.observedMountChain = requiredModuleName "root.web" :| [requiredModuleName "catalog"],
                      HarchWeb.observedRouteTemplate = requiredRouteTemplate "/known",
                      HarchWeb.observedLocale = HarchWeb.locale "en"
                    },
                HarchWeb.securityEventBody = eventBody,
                HarchWeb.securityEventRequirement = HarchWeb.TelemetryBestEffort
              }
      response <- performWaiRequest (toWaiApplication guardedApplication) (waiRequest ["known"])
      expectAll
        ( (Wai.responseStatus response `shouldBe` Http.status200)
            :| [readIORef emitted `shouldReturn` [expectedEnvelope]]
        )

    it "attaches observed route context to an unmounted root security event" $ do
      emitted <- newIORef []
      let delivery =
            HarchWeb.SecurityEventDelivery $ \eventEnvelope -> do
              modifyIORef' emitted (<> [eventEnvelope])
              pure HarchWeb.SecurityEventDelivered
          eventRoot =
            HarchWeb.SecurityEventRoot
              { HarchWeb.securityEventRootModule = requiredModuleName "root.web",
                HarchWeb.securityEventRootLocale = HarchWeb.locale . requestLanguage,
                HarchWeb.securityEventRootDelivery = delivery,
                HarchWeb.securityEventRootUndelivered = const (pure ())
              }
          eventBody = HarchWeb.AuthenticationEvaluated (HarchWeb.AuthenticationEvent HarchWeb.AuthenticationEstablished Nothing)
          guardedApplication =
            sampleApplication
              { HarchWeb.applicationSecurity =
                  HarchWeb.AuthenticationEnabled
                    []
                    ( HarchWeb.AuthenticationGuard $ \endpointRequest ->
                        case HarchWeb.endpointSecurityEventSink endpointRequest of
                          Nothing -> pure (HarchWeb.HaltEndpoint (HarchWeb.NonPageBodyResponse securityEventUnavailableResponse))
                          Just sink -> do
                            _ <- HarchWeb.emitSecurityEvent sink HarchWeb.TelemetryBestEffort eventBody
                            pure (HarchWeb.ContinueEndpoint (requestContext (HarchWeb.endpointRouteRequest endpointRequest)))
                    )
                    [],
                HarchWeb.applicationSecurityEventRoot = Just eventRoot,
                HarchWeb.applicationAttachRouteObservation = \routeValue metadata requestContextValue ->
                  if routeValue == KnownRoute && HarchWeb.endpointName metadata == requiredEndpointName "test.protected"
                    then requestContextValue {requestLanguage = "es"}
                    else requestContextValue,
                HarchWeb.routeEndpointMetadata = \case
                  KnownRoute -> protectedEndpointMetadata
                  routeValue -> error ("unexpected test route: " <> show routeValue)
              }
          expectedEnvelope =
            HarchWeb.SecurityEventEnvelope
              { HarchWeb.securityEventRoute =
                  HarchWeb.RouteObservation
                    { HarchWeb.observedEndpointName = requiredEndpointName "test.protected",
                      HarchWeb.observedMountChain = requiredModuleName "root.web" :| [],
                      HarchWeb.observedRouteTemplate = requiredRouteTemplate "/known",
                      HarchWeb.observedLocale = HarchWeb.locale "es"
                    },
                HarchWeb.securityEventBody = eventBody,
                HarchWeb.securityEventRequirement = HarchWeb.TelemetryBestEffort
              }
      response <- performWaiRequest (toWaiApplication guardedApplication) (waiRequest ["known"])
      expectAll
        ( (Wai.responseStatus response `shouldBe` Http.status200)
            :| [readIORef emitted `shouldReturn` [expectedEnvelope]]
        )

    it "fails closed before guards or handlers when a public root declares a protected endpoint" $ do
      guardRuns <- newIORef (0 :: Int)
      handlerRuns <- newIORef (0 :: Int)
      reportedLogs <- newIORef []
      let protectedApplication =
            sampleApplication
              { HarchWeb.applicationSecurity =
                  HarchWeb.AuthenticationDisabled
                    [ HarchWeb.EndpointGuard $ \_ -> do
                        modifyIORef' guardRuns (+ 1)
                        pure (HarchWeb.ContinueEndpoint defaultContext)
                    ],
                HarchWeb.routeEndpointMetadata = const protectedEndpointMetadata,
                renderRequestResponse = \_ routeRequest -> do
                  modifyIORef' handlerRuns (+ 1)
                  pure (renderSampleResponse routeRequest),
                HarchWeb.reportApplicationLog = \entry -> modifyIORef' reportedLogs (<> [entry])
              }
          headRequest = (waiRequest ["known"]) {Wai.requestMethod = "HEAD"}
          optionsRequest = (waiRequest ["known"]) {Wai.requestMethod = "OPTIONS"}
          methodMismatchRequest = (waiRequest ["known"]) {Wai.requestMethod = "DELETE"}
      normalResponse <- performWaiRequest (toWaiApplication protectedApplication) (waiRequest ["known"])
      headResponse <- performWaiRequest (toWaiApplication protectedApplication) headRequest
      optionsResponse <- performWaiRequest (toWaiApplication protectedApplication) optionsRequest
      methodMismatchResponse <- performWaiRequest (toWaiApplication protectedApplication) methodMismatchRequest
      expectAll
        ( ( map Wai.responseStatus [normalResponse, headResponse, optionsResponse, methodMismatchResponse]
              `shouldBe` replicate 4 Http.status503
          )
            :| [ readIORef guardRuns `shouldReturn` 0,
                 readIORef handlerRuns `shouldReturn` 0,
                 lookup Http.hContentType (Wai.responseHeaders normalResponse) `shouldBe` Just "text/plain; charset=utf-8",
                 (map (Text.isInfixOf "endpoint security configuration rejected a protected endpoint") <$> readIORef reportedLogs) `shouldReturn` replicate 4 True,
                 readResponseBody normalResponse `shouldReturn` "Authentication is unavailable."
               ]
        )

    it "passes every selected route protocol form to an enabled endpoint guard" $ do
      observedDispatchKinds <- newIORef []
      let guardedApplication =
            sampleApplication
              { HarchWeb.applicationSecurity =
                  HarchWeb.AuthenticationEnabled
                    []
                    ( HarchWeb.AuthenticationGuard $ \endpointRequest -> do
                        modifyIORef' observedDispatchKinds (<> [HarchWeb.endpointDispatchKind endpointRequest])
                        pure (HarchWeb.ContinueEndpoint (requestContext (HarchWeb.endpointRouteRequest endpointRequest)))
                    )
                    [],
                HarchWeb.routeEndpointMetadata = const protectedEndpointMetadata
              }
          headRequest = (waiRequest ["known"]) {Wai.requestMethod = "HEAD"}
          optionsRequest = (waiRequest ["known"]) {Wai.requestMethod = "OPTIONS"}
          methodMismatchRequest = (waiRequest ["known"]) {Wai.requestMethod = "DELETE"}
      waiApplication <- toWaiApplication guardedApplication
      _ <- performWaiRequest (pure waiApplication) (waiRequest ["known"])
      _ <- performWaiRequest (pure waiApplication) headRequest
      _ <- performWaiRequest (pure waiApplication) optionsRequest
      _ <- performWaiRequest (pure waiApplication) methodMismatchRequest
      readIORef observedDispatchKinds
        `shouldReturn` [ HarchWeb.EndpointMatched,
                         HarchWeb.EndpointMatchedHead,
                         HarchWeb.EndpointOptions,
                         HarchWeb.EndpointMethodNotAllowed
                       ]

    it "halts a protected client action before decoding its body or invoking its handler" $ do
      guardRuns <- newIORef (0 :: Int)
      handlerRuns <- newIORef (0 :: Int)
      let actionApplication =
            sampleApplication
              { HarchWeb.applicationSecurity =
                  HarchWeb.AuthenticationEnabled
                    []
                    ( HarchWeb.AuthenticationGuard $ \endpointRequest -> do
                        modifyIORef' guardRuns (+ 1)
                        HarchWeb.endpointDispatchKind endpointRequest `shouldBe` HarchWeb.EndpointClientAction
                        HarchWeb.requestRoute (HarchWeb.endpointRouteRequest endpointRequest) `shouldBe` KnownRoute
                        Wai.requestMethod (HarchWeb.endpointWaiRequest endpointRequest) `shouldBe` "POST"
                        pure (HarchWeb.HaltEndpoint (HarchWeb.NonPageBodyResponse securityEventUnavailableResponse))
                    )
                    [],
                HarchWeb.applicationRequestMiddleware =
                  [RequestMiddleware $ \_ requestContextValue -> pure (ContinueMiddleware requestContextValue {requestLanguage = "es"})],
                HarchWeb.clientActionEndpointMetadata = \methodValue pathValue requestContextValue ->
                  if methodValue == "POST" && pathValue == "/actions/protected" && requestLanguage requestContextValue == "es"
                    then Just protectedEndpointMetadata
                    else Nothing,
                HarchWeb.clientActionRoute = \methodValue pathValue requestContextValue ->
                  if methodValue == "POST" && pathValue == "/actions/protected" && requestLanguage requestContextValue == "es"
                    then Just KnownRoute
                    else Nothing,
                HarchWeb.decodeClientAction = \_ ->
                  error "client action decoder must not run after guard halt",
                HarchWeb.handleClientAction = \_ -> do
                  modifyIORef' handlerRuns (+ 1)
                  pure Nothing
              }
          actionRequest =
            (waiRequest ["actions", "protected"])
              { Wai.requestMethod = "POST",
                Wai.requestHeaders = [("X-Harch-Action", "1")]
              }
      response <- performWaiRequest (toWaiApplication actionApplication) actionRequest
      expectAll
        ( (Wai.responseStatus response `shouldBe` Http.status503)
            :| [ readIORef guardRuns `shouldReturn` 1,
                 readIORef handlerRuns `shouldReturn` 0
               ]
        )

    it "runs app middleware for dynamic routes while preserving transformed context" $ do
      let middlewareApplication =
            sampleApplication
              { applicationRequestMiddleware =
                  [ RequestMiddleware $ \request requestContext -> do
                      Wai.pathInfo request `shouldBe` ["known"]
                      pure (ContinueMiddleware requestContext {requestLanguage = "es"})
                  ]
              }
      response <- performWaiRequest (toWaiApplication middlewareApplication) (waiRequest ["known"])
      Wai.responseStatus response `shouldBe` Http.status200
      responseBody <- readResponseBody response
      Text.isInfixOf "<a href=\"/es/known\" data-page-link=\"true\" aria-current=\"page\">Known</a>" responseBody `shouldBe` True

    it "cannot bypass selected route admission through HEAD, OPTIONS, or a method mismatch" $ do
      releaseSignal <- newEmptyMVar
      admittedCount <- newIORef (0 :: Int)
      let baseApplication = sampleApplication
          limitedApplication =
            baseApplication
              { routeExecutionPolicy =
                  \case
                    KnownRoute -> RouteExecutionPolicy (mkRequestConcurrencyLimit 1)
                    _ -> unboundedRouteExecutionPolicy,
                renderRequestResponse = \request routeRequest ->
                  case requestRoute routeRequest of
                    KnownRoute -> do
                      atomicModifyIORef' admittedCount (\count -> (count + 1, ()))
                      readMVar releaseSignal
                      renderRequestResponse baseApplication request routeRequest
                    _ -> renderRequestResponse baseApplication request routeRequest
              }
      waiApplication <- toWaiApplication limitedApplication
      firstResponseSignal <- newEmptyMVar
      _ <- forkIO (performWaiRequest (pure waiApplication) (waiRequest ["known"]) >>= putMVar firstResponseSignal)
      waitUntilIORefEquals admittedCount 1
      headResponse <- performWaiRequest (pure waiApplication) ((waiRequest ["known"]) {Wai.requestMethod = "HEAD"})
      optionsResponse <- performWaiRequest (pure waiApplication) ((waiRequest ["known"]) {Wai.requestMethod = "OPTIONS"})
      methodMismatchResponse <- performWaiRequest (pure waiApplication) ((waiRequest ["known"]) {Wai.requestMethod = "DELETE"})
      putMVar releaseSignal ()
      firstResponse <- readMVar firstResponseSignal
      expectAll
        ( (Wai.responseStatus headResponse `shouldBe` Http.status503)
            :| [ Wai.responseStatus optionsResponse `shouldBe` Http.status503,
                 Wai.responseStatus methodMismatchResponse `shouldBe` Http.status503,
                 Wai.responseStatus firstResponse `shouldBe` Http.status200
               ]
        )

    it "halts dynamic requests without bypassing framework response headers" $ do
      let responseBodyValue = ResponseBody {responseStatus = Http.status401, responseContentType = "text/plain; charset=utf-8", responseBody = "Sign in required", responseObservabilityAttributes = [], responseLogEntries = [], responseDatabaseOperations = []}
          middlewareApplication =
            sampleApplication
              { applicationRequestMiddleware =
                  [RequestMiddleware $ \_ requestContext -> pure (HaltMiddleware requestContext responseBodyValue)]
              }
      response <- performWaiRequest (toWaiApplication middlewareApplication) (waiRequest ["data"])
      Wai.responseStatus response `shouldBe` Http.status401
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just "text/plain; charset=utf-8"
      lookup "Content-Security-Policy" (Wai.responseHeaders response) `shouldSatisfy` (/= Nothing)
      readResponseBody response `shouldReturn` "Sign in required"

    it "streams finite server-sent events through the normal response finalizer" $ do
      eventSource <-
        serverSentEventSourceFromList
          [ ServerSentEvent (Just "page-update") (Just "1") "first",
            ServerSentEvent Nothing (Just "2") "second"
          ]
      let eventApplication =
            sampleApplication
              { renderRequestResponse = \_ request ->
                  case requestRoute request of
                    EventStreamRoute -> pure (HarchWeb.nonPageResponse (eventStreamResponse eventSource))
                    _ -> pure (renderSampleResponse request)
              }
      response <- performWaiRequest (toWaiApplication eventApplication) (waiRequest ["events"])
      Wai.responseStatus response `shouldBe` Http.status200
      Http.statusMessage (Wai.responseStatus response) `shouldBe` "OK"
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just "text/event-stream; charset=utf-8"
      lookup "Cache-Control" (Wai.responseHeaders response) `shouldBe` Just "no-cache"
      lookup "X-Accel-Buffering" (Wai.responseHeaders response) `shouldBe` Just "no"
      lookup "Content-Security-Policy" (Wai.responseHeaders response) `shouldSatisfy` (/= Nothing)
      readResponseBody response
        `shouldReturn` "event: page-update\nid: 1\ndata: first\n\nid: 2\ndata: second\n\n"

      emptyEventSource <- serverSentEventSourceFromList []
      let emptyEventApplication = eventApplication {renderRequestResponse = \_ _ -> pure (HarchWeb.nonPageResponse (eventStreamResponse emptyEventSource))}
      emptyResponse <- performWaiRequest (toWaiApplication emptyEventApplication) (waiRequest ["events"])
      readResponseBody emptyResponse `shouldReturn` ""

    it "does not run app middleware for framework static responses" $ do
      middlewareRan <- newIORef False
      let middlewareApplication =
            sampleApplication
              { applicationNavigationRuntime = Just defaultNavigationRuntime,
                applicationRequestMiddleware =
                  [ RequestMiddleware $ \_ requestContext -> do
                      writeIORef middlewareRan True
                      pure (ContinueMiddleware requestContext)
                  ]
              }
      response <- performWaiRequest (toWaiApplication middlewareApplication) (waiRequest ["assets", "navigation.js"])
      Wai.responseStatus response `shouldBe` Http.status200
      readIORef middlewareRan `shouldReturn` False

    it "serves the configured navigation runtime before app route matching" $ do
      requestObservabilityReference <- newIORef Nothing
      let runtimeApplication =
            sampleApplication
              { applicationNavigationRuntime = Just defaultNavigationRuntime,
                reportRequestObservability = writeIORef requestObservabilityReference . Just
              }
      response <- performWaiRequest (toWaiApplication runtimeApplication) (waiRequest ["assets", "navigation.js"])
      Wai.responseStatus response `shouldBe` Http.status200
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just (TextEncoding.encodeUtf8 "application/javascript; charset=utf-8")
      responseBodyText <- readResponseBody response
      Text.isInfixOf "function navigateTo" responseBodyText `shouldBe` True
      maybeRequestObservability <- readIORef requestObservabilityReference
      fmap (Observability.requestSpanDisplayName . Observability.observabilityRequestSpan) maybeRequestObservability
        `shouldBe` Just "GET /assets/navigation.js"

    it "selects request paths through the stored route parser and returns HTML pages" $ do
      response <- performWaiRequest (toWaiApplication sampleApplication) (waiRequest ["es", "known"])
      Wai.responseStatus response `shouldBe` Http.status200
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just (TextEncoding.encodeUtf8 "text/html; charset=utf-8")
      lookup "Set-Cookie" (Wai.responseHeaders response) `shouldSatisfy` maybe False (ByteString.isPrefixOf "__Host-harch-csrf=")
      responseBody <- readResponseBody response
      Text.isInfixOf "<a href=\"/es/known\" data-page-link=\"true\" aria-current=\"page\">Known</a>" responseBody `shouldBe` True
      Text.isInfixOf "<script type=\"module\" src=\"/assets/navigation.js\" defer></script>" responseBody `shouldBe` True

    it "renders the preconstructed host-scoped CSRF token into action metadata independently from the page CSP nonce" $ do
      let csrfApplication =
            sampleApplication
              { renderRequestResponse = \_ request -> do
                  pageRoute (samplePage request) `shouldBe` KnownRoute
                  pure (PageResponse testPageSecurity (samplePage request))
              }
      response <- performWaiRequest (toWaiApplication csrfApplication) (waiRequest ["known"])
      nextResponse <- performWaiRequest (toWaiApplication csrfApplication) (waiRequest ["known"])
      responseBody <- readResponseBody response
      let responseHeaders = Wai.responseHeaders response
          csrfCookie = fromMaybe (error "page response did not issue CSRF cookie") (lookup "Set-Cookie" responseHeaders)
          nextCsrfCookie = fromMaybe (error "second page response did not issue CSRF cookie") (lookup "Set-Cookie" (Wai.responseHeaders nextResponse))
          csrfToken = ByteString.takeWhile (/= 59) (ByteString.drop 18 csrfCookie)
          contentSecurityPolicy = fromMaybe "" (lookup "Content-Security-Policy" responseHeaders)
      csrfCookie `shouldSatisfy` ByteString.isPrefixOf "__Host-harch-csrf="
      csrfCookie `shouldSatisfy` ByteString.isInfixOf "; Path=/; Max-Age=3600; Secure; HttpOnly; SameSite=Strict"
      csrfCookie `shouldSatisfy` (not . ByteString.isInfixOf "Domain=")
      csrfCookie `shouldSatisfy` ByteString.isInfixOf "HttpOnly"
      csrfCookie `shouldBe` nextCsrfCookie
      contentSecurityPolicy `shouldSatisfy` (not . ByteString.isInfixOf csrfToken)
      TextEncoding.encodeUtf8 responseBody `shouldSatisfy` ByteString.isInfixOf csrfToken
      responseBody `shouldSatisfy` Text.isInfixOf "data-harch-csrf-token=\"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\""

    it "uses the route-resolved context for captured actions and returns typed region patches" $ do
      actionRequestReference <- newIORef Nothing
      requestObservabilityReference <- newIORef Nothing
      logEntriesReference <- newIORef []
      let failureAttribute = Observability.ObservabilityAttribute "error.type" (Observability.TextAttribute "RegistrationStoreUnavailable")
          actionApplication =
            sampleApplication
              { handleClientAction = \actionRequest -> do
                  writeIORef actionRequestReference (Just actionRequest)
                  pure
                    ( Just
                        ClientActionResponse
                          { clientActionStatus = Http.status422,
                            clientActionPatches = [testRegionPatch "status-region" "Enter a valid email address."],
                            clientActionFocusId = Just (literalElementId "email"),
                            clientActionNavigation = StayOnCurrentRoute,
                            clientActionHeaders = [("Set-Cookie", "session=opaque")],
                            clientActionObservabilityAttributes = [failureAttribute],
                            clientActionLogEntries = ["private registration failure detail"]
                          }
                    ),
                reportRequestObservability = writeIORef requestObservabilityReference . Just,
                reportApplicationLog = \entry -> modifyIORef' logEntriesReference (<> [entry])
              }
      actionBodyChunks <- newIORef ["email=ada%40example.com&_csrf=csrf-token&_harch_csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA&intent=subscribe&blank"]
      let capturedActionRequest =
            Wai.setRequestBodyChunks
              (nextRequestBodyChunk actionBodyChunks)
              ( (waiRequest ["es", "known"])
                  { Wai.requestMethod = "POST",
                    Wai.requestHeaders = [("X-Harch-Action", "1"), ("Idempotency-Key", "retry-1"), (Http.hContentType, "application/x-www-form-urlencoded"), ("Host", "example.test"), ("Origin", "http://example.test"), ("Cookie", "__Host-harch-csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")]
                  }
              )
      response <- performWaiRequest (toWaiApplication actionApplication) capturedActionRequest
      maybeCapturedActionRequest <- readIORef actionRequestReference
      maybeCapturedActionRequest
        `shouldBe` Just
          ClientActionRequest
            { clientAction = "/es/known",
              clientActionRequestIdempotencyKey = Just "retry-1",
              clientActionContext = spanishContext
            }
      invalidIdempotencyBodyChunks <- newIORef ["email=ada%40example.com&_harch_csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"]
      let invalidIdempotencyRequest =
            Wai.setRequestBodyChunks
              (nextRequestBodyChunk invalidIdempotencyBodyChunks)
              ( capturedActionRequest
                  { Wai.requestHeaders = [("X-Harch-Action", "1"), ("Idempotency-Key", "\255"), (Http.hContentType, "application/x-www-form-urlencoded"), ("Host", "example.test"), ("Origin", "http://example.test"), ("Cookie", "__Host-harch-csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")]
                  }
              )
      writeIORef actionRequestReference Nothing
      _ <- performWaiRequest (toWaiApplication actionApplication) invalidIdempotencyRequest
      invalidIdempotencyActionRequest <- readIORef actionRequestReference
      invalidIdempotencyActionRequest
        `shouldBe` Just
          ClientActionRequest
            { clientAction = "/es/known",
              clientActionRequestIdempotencyKey = Nothing,
              clientActionContext = spanishContext
            }
      Http.statusCode (Wai.responseStatus response) `shouldBe` 422
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just "application/json; charset=utf-8"
      lookup "Set-Cookie" (Wai.responseHeaders response) `shouldBe` Just "session=opaque"
      readResponseBody response
        `shouldReturn` "{\"patches\":[{\"id\":\"status-region\",\"html\":\"<p id=\\\"status-region\\\" data-harch-region=\\\"true\\\">Enter a valid email address.</p>\"}],\"focusId\":\"email\",\"navigation\":null}"
      maybeRequestObservability <- readIORef requestObservabilityReference
      fmap (Observability.requestSpanAttributes . Observability.observabilityRequestSpan) maybeRequestObservability
        `shouldSatisfy` maybe False (hasTextAttribute "error.type" "RegistrationStoreUnavailable")
      capturedLogEntries <- readIORef logEntriesReference
      capturedLogEntries `shouldSatisfy` any (Text.isInfixOf "private registration failure detail")

    it "rejects malformed client-action form encoding before application dispatch" $ do
      actionBodyChunks <- newIORef ["email=%FF"]
      let actionRequest =
            Wai.setRequestBodyChunks
              (nextRequestBodyChunk actionBodyChunks)
              ( (waiRequest ["known"])
                  { Wai.requestMethod = "POST",
                    Wai.requestHeaders = [("X-Harch-Action", "1"), (Http.hContentType, "application/x-www-form-urlencoded"), ("Host", "example.test"), ("Origin", "http://example.test")]
                  }
              )
      response <- performWaiRequest (toWaiApplication sampleApplication) actionRequest
      Wai.responseStatus response `shouldBe` Http.status400
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just "application/json; charset=utf-8"
      readResponseBody response `shouldReturn` "{\"patches\":[],\"focusId\":null,\"navigation\":null}"

    it "bounds client-action form fields before decoding" $ do
      let fields fieldCount = LazyByteString.fromStrict (ByteString.intercalate "&" (replicate fieldCount "field=value"))
      expectAll
        ( (either (const (-1)) length (parseClientActionFields "") `shouldBe` 0)
            :| [ fromRight [] (parseClientActionFields "name=Ada+Lovelace&empty") `shouldBe` [("name", "Ada Lovelace"), ("empty", "")],
                 either (const (-1)) length (parseClientActionFields (fields 128)) `shouldBe` 128,
                 either (const (-1)) length (parseClientActionFields (fields 129)) `shouldBe` -1
               ]
        )

    it "rejects an oversized client-action body before URL decoding" $ do
      oversizedBodyReference <- newIORef (LazyByteString.fromStrict (ByteString.replicate 65537 97))
      oversizedBody <- readIORef oversizedBodyReference
      case parseClientActionFields oversizedBody of
        Left protocolError -> protocolError `seq` pure ()
        Right _fields -> expectationFailure "expected the client-action body limit to reject before decoding"

    it "recognizes only the explicit client-action protocol header" $ do
      let ordinaryRequest = waiRequest ["known"]
          disabledActionRequest = ordinaryRequest {Wai.requestHeaders = [("X-Harch-Action", "0")]}
          enabledActionRequest = ordinaryRequest {Wai.requestHeaders = [("X-Other", "value"), ("X-Harch-Action", "1")]}
      expectAll
        ( (isClientActionRequest ordinaryRequest `shouldBe` False)
            :| [ isClientActionRequest disabledActionRequest `shouldBe` False,
                 isClientActionRequest enabledActionRequest `shouldBe` True
               ]
        )

    it "rejects oversized or cross-origin client actions before application dispatch" $ do
      oversizedChunks <- newIORef [ByteString.replicate 65537 97]
      tooManyFieldsChunks <- newIORef [ByteString.intercalate "&" (replicate 129 "field")]
      crossOriginChunks <- newIORef ["email=ada%40example.test"]
      invalidContentTypeChunks <- newIORef ["email=ada%40example.test"]
      missingContentTypeChunks <- newIORef ["email=ada%40example.test"]
      missingCsrfChunks <- newIORef ["email=ada%40example.test"]
      mismatchedCsrfChunks <- newIORef ["email=ada%40example.test&_harch_csrf=csrf-token-mismatch"]
      invalidHostChunks <- newIORef ["email=ada%40example.test"]
      invalidOriginChunks <- newIORef ["email=ada%40example.test"]
      invalidCookieChunks <- newIORef ["email=ada%40example.test"]
      missingCookieChunks <- newIORef ["email=ada%40example.test"]
      parameterizedContentTypeChunks <- newIORef ["email=ada%40example.test&_harch_csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"]
      missingOriginAndHostChunks <- newIORef ["email=ada%40example.test"]
      let requestWith bodyChunks headers =
            Wai.setRequestBodyChunks
              (nextRequestBodyChunk bodyChunks)
              ( (waiRequest ["known"])
                  { Wai.requestMethod = "POST",
                    Wai.requestHeaders = headers
                  }
              )
          validHeaders = [("X-Harch-Action", "1"), (Http.hContentType, "application/x-www-form-urlencoded"), ("Host", "example.test"), ("Origin", "http://example.test")]
      oversizedResponse <- performWaiRequest (toWaiApplication sampleApplication) (requestWith oversizedChunks validHeaders)
      tooManyFieldsResponse <- performWaiRequest (toWaiApplication sampleApplication) (requestWith tooManyFieldsChunks validHeaders)
      crossOriginResponse <- performWaiRequest (toWaiApplication sampleApplication) (requestWith crossOriginChunks (init validHeaders <> [("Origin", "https://evil.example")]))
      invalidContentTypeResponse <- performWaiRequest (toWaiApplication sampleApplication) (requestWith invalidContentTypeChunks [("X-Harch-Action", "1"), (Http.hContentType, "application/x-www-form-urlencoded-malformed"), ("Host", "example.test"), ("Origin", "http://example.test")])
      missingContentTypeResponse <- performWaiRequest (toWaiApplication sampleApplication) (requestWith missingContentTypeChunks [("X-Harch-Action", "1"), ("Host", "example.test"), ("Origin", "http://example.test")])
      missingCsrfResponse <- performWaiRequest (toWaiApplication sampleApplication) (requestWith missingCsrfChunks (validHeaders <> [("Cookie", "__Host-harch-csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")]))
      mismatchedCsrfResponse <- performWaiRequest (toWaiApplication sampleApplication) (requestWith mismatchedCsrfChunks (validHeaders <> [("Cookie", "__Host-harch-csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")]))
      invalidHostResponse <- performWaiRequest (toWaiApplication sampleApplication) (requestWith invalidHostChunks [("X-Harch-Action", "1"), (Http.hContentType, "application/x-www-form-urlencoded"), ("Host", "\255"), ("Origin", "http://example.test")])
      invalidOriginResponse <- performWaiRequest (toWaiApplication sampleApplication) (requestWith invalidOriginChunks [("X-Harch-Action", "1"), (Http.hContentType, "application/x-www-form-urlencoded"), ("Host", "example.test"), ("Origin", "\255")])
      invalidCookieResponse <- performWaiRequest (toWaiApplication sampleApplication) (requestWith invalidCookieChunks (validHeaders <> [("Cookie", "__Host-harch-csrf=\255")]))
      missingCookieResponse <- performWaiRequest (toWaiApplication sampleApplication) (requestWith missingCookieChunks validHeaders)
      parameterizedContentTypeResponse <- performWaiRequest (toWaiApplication sampleApplication) (requestWith parameterizedContentTypeChunks [("X-Harch-Action", "1"), (Http.hContentType, "application/x-www-form-urlencoded; charset=utf-8"), ("Host", "example.test"), ("Origin", "http://example.test"), ("Cookie", "__Host-harch-csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")])
      missingOriginAndHostResponse <- performWaiRequest (toWaiApplication sampleApplication) (requestWith missingOriginAndHostChunks [("X-Harch-Action", "1"), (Http.hContentType, "application/x-www-form-urlencoded")])
      rejectedBodies <-
        traverse
          readResponseBody
          [ oversizedResponse,
            tooManyFieldsResponse,
            crossOriginResponse,
            invalidContentTypeResponse,
            missingCsrfResponse,
            mismatchedCsrfResponse,
            invalidHostResponse,
            invalidOriginResponse,
            invalidCookieResponse,
            missingCookieResponse,
            missingOriginAndHostResponse
          ]
      Wai.responseStatus oversizedResponse `shouldBe` Http.status413
      Wai.responseStatus tooManyFieldsResponse `shouldBe` Http.status413
      Wai.responseStatus crossOriginResponse `shouldBe` Http.status403
      Wai.responseStatus invalidContentTypeResponse `shouldBe` Http.status415
      Wai.responseStatus missingContentTypeResponse `shouldBe` Http.status415
      Wai.responseStatus missingCsrfResponse `shouldBe` Http.status403
      Wai.responseStatus mismatchedCsrfResponse `shouldBe` Http.status403
      Wai.responseStatus invalidHostResponse `shouldBe` Http.status403
      Wai.responseStatus invalidOriginResponse `shouldBe` Http.status403
      Wai.responseStatus invalidCookieResponse `shouldBe` Http.status403
      Wai.responseStatus missingCookieResponse `shouldBe` Http.status403
      Wai.responseStatus parameterizedContentTypeResponse `shouldBe` Http.status404
      Wai.responseStatus missingOriginAndHostResponse `shouldBe` Http.status403
      rejectedBodies `shouldBe` replicate 11 "{\"patches\":[],\"focusId\":null,\"navigation\":null}"

    it "rejects malformed or duplicated strict CSRF transport before an action handler" $ do
      handlerCalled <- newIORef False
      let actionApplication =
            sampleApplication
              { handleClientAction = \_ -> do
                  writeIORef handlerCalled True
                  pure (Just (ClientActionResponse Http.status204 [] Nothing StayOnCurrentRoute [] [] []))
              }
          requestWith bodyChunks bodyHeaders =
            Wai.setRequestBodyChunks
              (nextRequestBodyChunk bodyChunks)
              ( (waiRequest ["known"])
                  { Wai.requestMethod = "POST",
                    Wai.requestHeaders =
                      [ ("X-Harch-Action", "1"),
                        (Http.hContentType, "application/x-www-form-urlencoded"),
                        ("Host", "example.test"),
                        ("Origin", "http://example.test")
                      ]
                        <> bodyHeaders
                  }
              )
      malformedChunks <- newIORef ["_harch_csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"]
      shortCookieChunks <- newIORef ["_harch_csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"]
      duplicateCookieChunks <- newIORef ["_harch_csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"]
      duplicateCookieValuesChunks <- newIORef ["_harch_csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"]
      malformed <- performWaiRequest (toWaiApplication actionApplication) (requestWith malformedChunks [("Cookie", "harch-csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")])
      shortCookie <- performWaiRequest (toWaiApplication actionApplication) (requestWith shortCookieChunks [("Cookie", "__Host-harch-csrf=short")])
      duplicateCookie <- performWaiRequest (toWaiApplication actionApplication) (requestWith duplicateCookieChunks [("Cookie", "__Host-harch-csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"), ("Cookie", "__Host-harch-csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")])
      duplicateCookieValues <- performWaiRequest (toWaiApplication actionApplication) (requestWith duplicateCookieValuesChunks [("Cookie", "__Host-harch-csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA; __Host-harch-csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")])
      duplicateFieldChunks <- newIORef ["_harch_csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA&_harch_csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"]
      let duplicateFieldRequest =
            Wai.setRequestBodyChunks
              (nextRequestBodyChunk duplicateFieldChunks)
              ( (waiRequest ["known"])
                  { Wai.requestMethod = "POST",
                    Wai.requestHeaders =
                      [ ("X-Harch-Action", "1"),
                        (Http.hContentType, "application/x-www-form-urlencoded"),
                        ("Host", "example.test"),
                        ("Origin", "http://example.test"),
                        ("Cookie", "__Host-harch-csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
                      ]
                  }
              )
      duplicateField <- performWaiRequest (toWaiApplication actionApplication) duplicateFieldRequest
      map Wai.responseStatus [malformed, shortCookie, duplicateCookie, duplicateCookieValues, duplicateField] `shouldBe` replicate 5 Http.status403
      readIORef handlerCalled `shouldReturn` False

    it "keeps HEAD and OPTIONS client-action-looking requests in route method dispatch" $ do
      handlerCalled <- newIORef False
      headBodyChunks <- newIORef ["_harch_csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"]
      optionsBodyChunks <- newIORef ["_harch_csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"]
      let actionApplication =
            sampleApplication
              { handleClientAction = \_ -> do
                  writeIORef handlerCalled True
                  pure (Just (ClientActionResponse Http.status204 [] Nothing StayOnCurrentRoute [] [] []))
              }
          requestWith requestMethodValue bodyChunks =
            Wai.setRequestBodyChunks
              (nextRequestBodyChunk bodyChunks)
              ( (waiRequest ["known"])
                  { Wai.requestMethod = requestMethodValue,
                    Wai.requestHeaders =
                      [ ("X-Harch-Action", "1"),
                        (Http.hContentType, "application/x-www-form-urlencoded"),
                        ("Host", "example.test"),
                        ("Origin", "http://example.test"),
                        ("Cookie", "__Host-harch-csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
                      ]
                  }
              )
      headResponse <- performWaiRequest (toWaiApplication actionApplication) (requestWith "HEAD" headBodyChunks)
      optionsResponse <- performWaiRequest (toWaiApplication actionApplication) (requestWith "OPTIONS" optionsBodyChunks)
      expectAll
        ( (Wai.responseStatus headResponse `shouldBe` Http.status200)
            :| [ readResponseBody headResponse `shouldReturn` "",
                 Wai.responseStatus optionsResponse `shouldBe` Http.status204,
                 lookup Http.hAllow (Wai.responseHeaders optionsResponse) `shouldBe` Just "GET, HEAD, OPTIONS",
                 readIORef handlerCalled `shouldReturn` False
               ]
        )

    it "runs CSRF verification before an action handler" $ do
      authorizationCalled <- newIORef False
      handlerCalled <- newIORef False
      actionBodyChunks <- newIORef ["_harch_csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"]
      let actionApplication =
            sampleApplication
              { csrfProtection =
                  HarchWeb.CsrfProtection
                    { HarchWeb.issueCsrfToken = const (pure HarchWeb.CsrfProtectionUnavailable),
                      HarchWeb.verifyCsrfToken = \requestContext csrfToken -> do
                        requestLanguage requestContext `shouldBe` "en"
                        csrfTokenText csrfToken `shouldBe` "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
                        writeIORef authorizationCalled True
                        pure HarchWeb.CsrfRejected
                    },
                handleClientAction = \_ -> writeIORef handlerCalled True >> pure (Just (ClientActionResponse Http.status204 [] Nothing StayOnCurrentRoute [] [] []))
              }
          actionRequest =
            Wai.setRequestBodyChunks
              (nextRequestBodyChunk actionBodyChunks)
              ( (waiRequest ["known"])
                  { Wai.requestMethod = "POST",
                    Wai.requestHeaders =
                      [ ("X-Harch-Action", "1"),
                        (Http.hContentType, "application/x-www-form-urlencoded"),
                        ("Host", "example.test"),
                        ("Origin", "http://example.test"),
                        ("Cookie", "__Host-harch-csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
                      ]
                  }
              )
      response <- performWaiRequest (toWaiApplication actionApplication) actionRequest
      Wai.responseStatus response `shouldBe` Http.status403
      readIORef authorizationCalled `shouldReturn` True
      readIORef handlerCalled `shouldReturn` False

    it "passes decoded client-action CSRF and idempotency metadata to the application" $ do
      receivedAction <- newIORef (Nothing :: Maybe (ClientActionRequest Text TestContext))
      actionBodyChunks <- newIORef ["intent=save&_harch_csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"]
      let metadataApplication =
            sampleApplication
              { decodeClientAction = \payload ->
                  ( case ( clientActionMethod payload,
                           clientActionFields payload,
                           clientActionCsrfToken payload,
                           clientActionIdempotencyKey payload
                         ) of
                      ("POST", [("intent", "save"), ("_harch_csrf", "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")], Just "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA", Just "idempotency-1") -> DecodedClientAction "save"
                      _ -> UnrecognizedClientAction
                  ),
                handleClientAction = \decodedActionRequest -> do
                  writeIORef receivedAction (Just decodedActionRequest)
                  pure (Just (ClientActionResponse Http.status204 [] Nothing StayOnCurrentRoute [] [] []))
              }
          actionRequest =
            Wai.setRequestBodyChunks
              (nextRequestBodyChunk actionBodyChunks)
              ( (waiRequest ["known"])
                  { Wai.requestMethod = "POST",
                    Wai.requestHeaders =
                      [ ("X-Harch-Action", "1"),
                        (Http.hContentType, "application/x-www-form-urlencoded"),
                        ("Host", "example.test"),
                        ("Origin", "http://example.test"),
                        ("Cookie", "__Host-harch-csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"),
                        ("Idempotency-Key", "idempotency-1")
                      ]
                  }
              )
      response <- performWaiRequest (toWaiApplication metadataApplication) actionRequest
      capturedAction <- readIORef receivedAction
      expectAll
        ( (Wai.responseStatus response `shouldBe` Http.status204)
            :| [ fmap clientAction capturedAction `shouldBe` Just "save",
                 fmap clientActionRequestIdempotencyKey capturedAction `shouldBe` Just (Just "idempotency-1")
               ]
        )

    it "preserves WAI transport input for not-found and synthesized HEAD route dispatch" $ do
      routedRequests <- newIORef []
      let recordingApplication =
            sampleApplication
              { renderRequestResponse = \request routeRequest -> do
                  atomicModifyIORef' routedRequests (\requests -> (requests <> [(Wai.requestMethod request, requestRoute routeRequest)], ()))
                  pure (BodyResponse (ResponseBody Http.status200 "text/plain" "recorded" [] [] []))
              }
          requestFor requestMethodValue path = Wai.defaultRequest {Wai.requestMethod = requestMethodValue, Wai.rawPathInfo = path}
      notFoundResponse <- performWaiRequest (toWaiApplication recordingApplication) (requestFor "GET" "/missing")
      headResponse <- performWaiRequest (toWaiApplication recordingApplication) (requestFor "HEAD" "/known")
      routed <- readIORef routedRequests
      expectAll
        ( (Wai.responseStatus notFoundResponse `shouldBe` Http.status200)
            :| [ Wai.responseStatus headResponse `shouldBe` Http.status200,
                 routed `shouldBe` [("GET", MissingRoute), ("HEAD", KnownRoute)]
               ]
        )

    it "renders a typed internal redirect through the root route codec" $ do
      let redirect = internalRedirectResponse Http.status303 (RouteRequest KnownRoute defaultContext)
          rendered = toWaiResponse [] Nothing sampleApplication redirect
      expectAll
        ( (Wai.responseStatus rendered `shouldBe` Http.status303)
            :| [lookup Http.hLocation (Wai.responseHeaders rendered) `shouldBe` Just "/known"]
        )

    it "keeps an internal redirect target typed while retaining issued-cookie headers" $ do
      let redirect =
            HarchWeb.nonPageResponse
              ( HarchWeb.nonPageInternalRedirectResponseWithHeaders
                  Http.status303
                  [("Set-Cookie", "__Host-session=opaque"), (Http.hLocation, "https://attacker.invalid")]
                  (RouteRequest KnownRoute defaultContext)
              )
          rendered = toWaiResponse [] Nothing sampleApplication redirect
      expectAll
        ( (Wai.responseStatus rendered `shouldBe` Http.status303)
            :| [ lookup "Set-Cookie" (Wai.responseHeaders rendered) `shouldBe` Just "__Host-session=opaque",
                 lookup Http.hLocation (Wai.responseHeaders rendered) `shouldBe` Just "/known"
               ]
        )

    it "serializes client-action metadata, multiple patches, and every JSON escape" $ do
      let escapedText = "quote\" slash\\ backspace\b formfeed\f newline\n carriage\r tab\t unicode ☃"
          observabilityAttribute = Observability.ObservabilityAttribute "action.outcome" (Observability.TextAttribute "rejected")
          responseBodyValue =
            clientActionResponseBody
              (routeCodec sampleApplication)
              ClientActionResponse
                { clientActionStatus = Http.status422,
                  clientActionPatches = [testRegionPatch ("first " <> escapedText) escapedText, testRegionPatch "second" escapedText],
                  clientActionFocusId = Just (literalElementId escapedText),
                  clientActionNavigation = NavigateInternal ReplaceHistory (RouteRequest KnownRoute defaultContext),
                  clientActionHeaders = [],
                  clientActionObservabilityAttributes = [observabilityAttribute],
                  clientActionLogEntries = ["private action diagnostic"]
                }
          encodedResponse = responseBody responseBodyValue
      expectAll
        ( (responseStatus responseBodyValue `shouldBe` Http.status422)
            :| [ responseContentType responseBodyValue `shouldBe` "application/json; charset=utf-8",
                 responseObservabilityAttributes responseBodyValue `shouldBe` [observabilityAttribute],
                 responseLogEntries responseBodyValue `shouldBe` ["private action diagnostic"],
                 responseDatabaseOperations responseBodyValue `shouldBe` [],
                 encodedResponse `shouldSatisfy` Text.isInfixOf "},{",
                 encodedResponse `shouldSatisfy` Text.isInfixOf "\\\"",
                 encodedResponse `shouldSatisfy` Text.isInfixOf "\\\\",
                 encodedResponse `shouldSatisfy` Text.isInfixOf "\\u0008",
                 encodedResponse `shouldSatisfy` Text.isInfixOf "\\u000c",
                 encodedResponse `shouldSatisfy` Text.isInfixOf "\\n",
                 encodedResponse `shouldSatisfy` Text.isInfixOf "\\r",
                 encodedResponse `shouldSatisfy` Text.isInfixOf "\\t",
                 encodedResponse `shouldSatisfy` Text.isInfixOf "☃",
                 encodedResponse `shouldSatisfy` Text.isInfixOf "\"navigation\":{\"historyMode\":\"replace\",\"href\":\"/known\"}"
               ]
        )

    it "rejects marked actions without a matching handler instead of falling back to SSR" $ do
      actionBodyChunks <- newIORef ["intent=unknown&_harch_csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"]
      let actionRequest =
            Wai.setRequestBodyChunks
              (nextRequestBodyChunk actionBodyChunks)
              ( (waiRequest ["known"])
                  { Wai.requestMethod = "POST",
                    Wai.requestHeaders = [("X-Harch-Action", "1"), (Http.hContentType, "application/x-www-form-urlencoded"), ("Host", "example.test"), ("Origin", "http://example.test"), ("Cookie", "__Host-harch-csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")]
                  }
              )
      response <- performWaiRequest (toWaiApplication sampleApplication) actionRequest
      Wai.responseStatus response `shouldBe` Http.status404
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just (TextEncoding.encodeUtf8 "application/json; charset=utf-8")

    it "maps codec unknown, method, malformed, invalid-decoder, and domain action outcomes to safe protocol responses" $ do
      loggedActionFailures <- newIORef []
      let actionApplication =
            sampleApplication
              { decodeClientAction = Action.decodeAction testActionCodec,
                handleClientAction =
                  const
                    ( pure
                        ( Just
                            ClientActionResponse
                              { clientActionStatus = Http.status422,
                                clientActionPatches = [],
                                clientActionFocusId = Nothing,
                                clientActionNavigation = StayOnCurrentRoute,
                                clientActionHeaders = [],
                                clientActionObservabilityAttributes = [],
                                clientActionLogEntries = []
                              }
                        )
                    ),
                reportApplicationLog = \entry -> modifyIORef' loggedActionFailures (<> [entry])
              }
          invalidDecoder :: Action.ActionDecoder Text
          invalidDecoder = Compose (const (Compose ([], Nothing)))
          invalidDecoderCodec =
            fromRight (error "invalid test action codec") $
              Action.actionCodec [Action.action () (Action.post "/invalid") invalidDecoder]
          invalidDecoderApplication =
            actionApplication {decodeClientAction = Action.decodeAction invalidDecoderCodec}
          requestFor methodValue path bodyChunks =
            Wai.setRequestBodyChunks
              (nextRequestBodyChunk bodyChunks)
              ( (waiRequest path)
                  { Wai.requestMethod = methodValue,
                    Wai.requestHeaders =
                      [ ("X-Harch-Action", "1"),
                        (Http.hContentType, "application/x-www-form-urlencoded"),
                        ("Host", "example.test"),
                        ("Origin", "http://example.test"),
                        ("Cookie", "__Host-harch-csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
                      ]
                  }
              )
      unknownChunks <- newIORef ["_harch_csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"]
      wrongMethodChunks <- newIORef ["_harch_csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"]
      malformedChunks <- newIORef ["_harch_csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"]
      invalidDecoderChunks <- newIORef ["_harch_csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"]
      domainChunks <- newIORef ["email=ada%40example.test&_harch_csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"]
      unknownResponse <- performWaiRequest (toWaiApplication actionApplication) (requestFor "POST" ["missing"] unknownChunks)
      wrongMethodResponse <- performWaiRequest (toWaiApplication actionApplication) (requestFor "PUT" ["known"] wrongMethodChunks)
      malformedResponse <- performWaiRequest (toWaiApplication actionApplication) (requestFor "POST" ["known"] malformedChunks)
      invalidDecoderResponse <- performWaiRequest (toWaiApplication invalidDecoderApplication) (requestFor "POST" ["invalid"] invalidDecoderChunks)
      domainResponse <- performWaiRequest (toWaiApplication actionApplication) (requestFor "POST" ["known"] domainChunks)
      Wai.responseStatus unknownResponse `shouldBe` Http.status404
      Wai.responseStatus wrongMethodResponse `shouldBe` Http.status405
      lookup "Allow" (Wai.responseHeaders wrongMethodResponse) `shouldBe` Just "POST, GET"
      readResponseBody wrongMethodResponse `shouldReturn` "{\"patches\":[],\"focusId\":null,\"navigation\":null}"
      Wai.responseStatus malformedResponse `shouldBe` Http.status400
      readResponseBody malformedResponse `shouldReturn` "{\"patches\":[],\"focusId\":null,\"navigation\":null}"
      Wai.responseStatus invalidDecoderResponse `shouldBe` Http.status500
      readResponseBody invalidDecoderResponse `shouldReturn` "{\"patches\":[],\"focusId\":null,\"navigation\":null}"
      fmap (any (Text.isInfixOf "client action decode failure: malformed")) (readIORef loggedActionFailures) `shouldReturn` True
      fmap (any (Text.isInfixOf "client action decode failure: invalid decoder")) (readIORef loggedActionFailures) `shouldReturn` True
      Wai.responseStatus domainResponse `shouldBe` Http.status422

    it "renders typed redirects with the location header and standard response metadata" $ do
      let typedRedirect = redirectResponse Http.status302 "/spaces" :: Response TestRoute TestContext
          redirectApplication = sampleApplication {renderRequestResponse = \_ _ -> pure typedRedirect}
          diagnostics = responseDiagnostics typedRedirect
      diagnosticObservabilityAttributes diagnostics `shouldBe` []
      diagnosticLogEntries diagnostics `shouldBe` []
      responseStatusCode redirectApplication typedRedirect `shouldBe` 302
      responseKind typedRedirect `shouldBe` Observability.BodyResponseKind
      response <- performWaiRequest (toWaiApplication redirectApplication) (waiRequest ["known"])
      Wai.responseStatus response `shouldBe` Http.status302
      lookup Http.hLocation (Wai.responseHeaders response) `shouldBe` Just "/spaces"
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just "text/plain; charset=utf-8"
      readResponseBody response `shouldReturn` ""

    it "renders strict protocol bytes through the shared response interpreter" $ do
      let protocolResponse =
            ProtocolResponse
              { protocolResponseStatus = Http.status201,
                protocolResponseHeaders = [(Http.hContentType, "application/example"), ("X-Example", "present")],
                protocolResponseBody = ProtocolResponseBytes "\NUL\SOH\STX",
                protocolResponseObservabilityAttributes = [Observability.ObservabilityAttribute "example.outcome" (Observability.TextAttribute "created")],
                protocolResponseLogEntries = ["example response"],
                protocolResponseDatabaseOperations = []
              }
          renderedResponse = ProtocolResponseResult protocolResponse :: Response TestRoute TestContext
          protocolApplication = sampleApplication {renderRequestResponse = \_ _ -> pure renderedResponse}
          diagnostics = responseDiagnostics renderedResponse
          changedProtocolResponse =
            ProtocolResponse
              { protocolResponseStatus = Http.status200,
                protocolResponseHeaders = [(Http.hContentType, "application/example")],
                protocolResponseBody = ProtocolResponseBytes "changed",
                protocolResponseObservabilityAttributes = [],
                protocolResponseLogEntries = [],
                protocolResponseDatabaseOperations = []
              }
      response <- performWaiRequest (toWaiApplication protocolApplication) (waiRequest ["known"])
      body <- readResponseBody response
      expectAll
        ( (Wai.responseStatus response `shouldBe` Http.status201)
            :| [ lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just "application/example",
                 lookup "X-Example" (Wai.responseHeaders response) `shouldBe` Just "present",
                 body `shouldBe` "\NUL\SOH\STX",
                 diagnosticObservabilityAttributes diagnostics `shouldBe` [Observability.ObservabilityAttribute "example.outcome" (Observability.TextAttribute "created")],
                 diagnosticLogEntries diagnostics `shouldBe` ["example response"],
                 responseStatusCode protocolApplication renderedResponse `shouldBe` 201,
                 responseKind renderedResponse `shouldBe` Observability.BodyResponseKind,
                 protocolResponse /= changedProtocolResponse `shouldBe` True,
                 show renderedResponse `shouldSatisfy` isInfixOf "ProtocolResponseResult (ProtocolResponse",
                 showsPrec 11 renderedResponse "" `shouldSatisfy` isInfixOf "(ProtocolResponseResult (ProtocolResponse",
                 show protocolResponse `shouldSatisfy` isInfixOf "ProtocolResponseBytes",
                 showsPrec 11 protocolResponse "" `shouldSatisfy` isInfixOf "(ProtocolResponse",
                 show protocolResponse `shouldSatisfy` isInfixOf "application/example",
                 length (show protocolResponse) `shouldSatisfy` (> 0),
                 length (showList [protocolResponse] "") `shouldSatisfy` (> 0)
               ]
        )

    it "streams protocol bytes without materializing them as response text" $ do
      let streamBody write flush = do
            _ <- write (Builder.byteString "first-")
            _ <- flush
            _ <- write (Builder.byteString "second")
            pure ()
          renderedResponse =
            ProtocolResponseResult
              ProtocolResponse
                { protocolResponseStatus = Http.status200,
                  protocolResponseHeaders = [(Http.hContentType, "application/octet-stream")],
                  protocolResponseBody = ProtocolResponseStream streamBody,
                  protocolResponseObservabilityAttributes = [],
                  protocolResponseLogEntries = [],
                  protocolResponseDatabaseOperations = []
                } ::
              Response TestRoute TestContext
          protocolApplication = sampleApplication {renderRequestResponse = \_ _ -> pure renderedResponse}
          strictResponse =
            ProtocolResponseResult
              ProtocolResponse
                { protocolResponseStatus = Http.status200,
                  protocolResponseHeaders = [(Http.hContentType, "application/octet-stream")],
                  protocolResponseBody = ProtocolResponseBytes "first-second",
                  protocolResponseObservabilityAttributes = [],
                  protocolResponseLogEntries = [],
                  protocolResponseDatabaseOperations = []
                } ::
              Response TestRoute TestContext
          sameMetadataDifferentStream =
            ProtocolResponseResult
              ProtocolResponse
                { protocolResponseStatus = Http.status200,
                  protocolResponseHeaders = [(Http.hContentType, "application/octet-stream")],
                  protocolResponseBody = ProtocolResponseStream (\write _ -> write (Builder.byteString "other stream")),
                  protocolResponseObservabilityAttributes = [],
                  protocolResponseLogEntries = [],
                  protocolResponseDatabaseOperations = []
                } ::
              Response TestRoute TestContext
      response <- performWaiRequest (toWaiApplication protocolApplication) (waiRequest ["known"])
      expectAll
        ( (lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just "application/octet-stream")
            :| [ Wai.responseStatus response `shouldBe` Http.status200,
                 readResponseBody response `shouldReturn` "first-second",
                 renderedResponse `shouldBe` sameMetadataDifferentStream,
                 renderedResponse `shouldNotBe` strictResponse,
                 show renderedResponse `shouldSatisfy` isInfixOf "ProtocolResponseStream <stream>"
               ]
        )

    it "preserves a framework-owned WAI protocol response while applying root headers" $ do
      let frameworkResponse = Wai.responseLBS Http.status200 [(Http.hContentType, "application/octet-stream"), ("X-Asset", "present")] "asset"
          protocolResponse =
            ProtocolResponse
              { protocolResponseStatus = Http.status200,
                protocolResponseHeaders = [("X-Protocol", "present")],
                protocolResponseBody = ProtocolResponseWai frameworkResponse,
                protocolResponseObservabilityAttributes = [],
                protocolResponseLogEntries = [],
                protocolResponseDatabaseOperations = []
              }
          sameResponse = protocolResponse {protocolResponseBody = ProtocolResponseWai (Wai.responseLBS Http.status200 [] "different")}
          rendered = toWaiResponse [("X-Policy", "present")] Nothing sampleApplication (ProtocolResponseResult protocolResponse)
      expectAll
        ( (protocolResponse `shouldBe` sameResponse)
            :| [ show protocolResponse `shouldSatisfy` isInfixOf "ProtocolResponseWai <framework-response>",
                 Wai.responseStatus rendered `shouldBe` Http.status200,
                 lookup Http.hContentType (Wai.responseHeaders rendered) `shouldBe` Just "application/octet-stream",
                 lookup "X-Asset" (Wai.responseHeaders rendered) `shouldBe` Just "present",
                 lookup "X-Protocol" (Wai.responseHeaders rendered) `shouldBe` Just "present",
                 lookup "X-Policy" (Wai.responseHeaders rendered) `shouldBe` Just "present",
                 readResponseBody rendered `shouldReturn` "asset"
               ]
        )

    it "compares and prints every non-page response form without observing stream identity" $ do
      eventSource <- serverSentEventSourceFromList []
      sameEventSource <- serverSentEventSourceFromList [ServerSentEvent Nothing Nothing "later"]
      otherEventSource <- serverSentEventSourceFromList []
      let responseBodyValue = ResponseBody Http.status200 "text/plain" "ok" [] [] []
          otherResponseBodyValue = ResponseBody Http.status500 "text/plain" "failed" [] [] []
          actionResponse = ClientActionResponse Http.status200 [] Nothing StayOnCurrentRoute [] [] []
          otherActionResponse = ClientActionResponse Http.status422 [] (Just (literalElementId "email")) StayOnCurrentRoute [] [] []
          eventResponse = EventStreamResponse responseBodyValue eventSource :: Response TestRoute TestContext
          sameEventResponse = EventStreamResponse responseBodyValue sameEventSource
          otherEventResponse = EventStreamResponse otherResponseBodyValue otherEventSource
          actionBodyResponse = ClientActionBodyResponse actionResponse :: Response TestRoute TestContext
          otherActionBodyResponse = ClientActionBodyResponse otherActionResponse
          protocolResponse = ProtocolResponseResult (ProtocolResponse Http.status200 [] (ProtocolResponseBytes "ok") [] [] []) :: Response TestRoute TestContext
          otherProtocolResponse = ProtocolResponseResult (ProtocolResponse Http.status500 [] (ProtocolResponseBytes "failed") [] [] [])
      expectAll
        ( (actionBodyResponse /= otherActionBodyResponse `shouldBe` True)
            :| [ eventResponse == sameEventResponse `shouldBe` True,
                 eventResponse /= otherEventResponse `shouldBe` True,
                 protocolResponse /= otherProtocolResponse `shouldBe` True,
                 show [actionBodyResponse, eventResponse, protocolResponse] `shouldSatisfy` isInfixOf "EventStreamResponse",
                 show eventResponse `shouldSatisfy` isInfixOf "<event-source>",
                 show protocolResponse `shouldSatisfy` isInfixOf "ProtocolResponseResult",
                 show (ProtocolResponse Http.status200 [(Http.hContentType, "application/example")] (ProtocolResponseBytes "ok") [] [] [])
                   `shouldBe` "ProtocolResponse (Status {statusCode = 200, statusMessage = \"OK\"}) [(\"Content-Type\",\"application/example\")] \"ProtocolResponseBytes \\\"ok\\\"\""
               ]
        )

    it "serializes action responses with no patches or focus target" $ do
      let actionApplication =
            sampleApplication
              { handleClientAction = const (pure (Just ClientActionResponse {clientActionStatus = Http.status204, clientActionPatches = [], clientActionFocusId = Nothing, clientActionNavigation = StayOnCurrentRoute, clientActionHeaders = [], clientActionObservabilityAttributes = [], clientActionLogEntries = []}))
              }
      actionBodyChunks <- newIORef ["_harch_csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"]
      let actionRequest =
            Wai.setRequestBodyChunks
              (nextRequestBodyChunk actionBodyChunks)
              ( (waiRequest ["actions", "empty"])
                  { Wai.requestMethod = "POST",
                    Wai.requestHeaders = [("X-Harch-Action", "1"), (Http.hContentType, "application/x-www-form-urlencoded"), ("Host", "example.test"), ("Origin", "http://example.test"), ("Cookie", "__Host-harch-csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")]
                  }
              )
      response <- performWaiRequest (toWaiApplication actionApplication) actionRequest
      Wai.responseStatus response `shouldBe` Http.status204
      readResponseBody response `shouldReturn` "{\"patches\":[],\"focusId\":null,\"navigation\":null}"

    it "adds the page nonce to custom CSP script sources, including policies without script-src" $ do
      let applicationWithPolicy policy =
            sampleApplicationWithConfig
              emptyStaticAssets
              (defaultRequestPolicy {responseSecurityHeaders = defaultResponseSecurityHeadersConfig {contentSecurityPolicy = Just policy}})
      missingScriptSourceResponse <- performWaiRequest (toWaiApplication (applicationWithPolicy "default-src 'self'")) (waiRequest ["known"])
      noneScriptSourceResponse <- performWaiRequest (toWaiApplication (applicationWithPolicy "script-src 'none'")) (waiRequest ["known"])
      let missingScriptSourcePolicy = TextEncoding.decodeUtf8 (fromMaybe "" (lookup "Content-Security-Policy" (Wai.responseHeaders missingScriptSourceResponse)))
          noneScriptSourcePolicy = TextEncoding.decodeUtf8 (fromMaybe "" (lookup "Content-Security-Policy" (Wai.responseHeaders noneScriptSourceResponse)))
      Text.isInfixOf "; script-src 'nonce-" missingScriptSourcePolicy `shouldBe` True
      Text.isInfixOf "script-src 'nonce-" noneScriptSourcePolicy `shouldBe` True
      Text.isInfixOf "'none'" noneScriptSourcePolicy `shouldBe` False

    it "uses the page nonce for page responses with metadata" $ do
      let metadata =
            ResponseBody
              { responseStatus = Http.status422,
                responseContentType = "text/html; charset=utf-8",
                responseBody = "",
                responseObservabilityAttributes = [],
                responseLogEntries = [],
                responseDatabaseOperations = []
              }
          metadataApplication =
            sampleApplication
              { renderRequestResponse = \_ request -> pure (PageResponseWithMetadata testPageSecurity metadata (samplePage request)),
                pageShell =
                  \page ->
                    (pageShell sampleApplication page)
                      { documentRuntimeDescriptors = [InlineBootstrap "capture" "window.capture = true;"]
                      }
              }
      response <- performWaiRequest (toWaiApplication metadataApplication) (waiRequest ["known"])
      let policy = TextEncoding.decodeUtf8 (fromMaybe "" (lookup "Content-Security-Policy" (Wai.responseHeaders response)))
      Http.statusCode (Wai.responseStatus response) `shouldBe` 422
      Text.isInfixOf "script-src 'self' 'nonce-" policy `shouldBe` True
      lookup "Set-Cookie" (Wai.responseHeaders response) `shouldSatisfy` maybe False (ByteString.isPrefixOf "__Host-harch-csrf=")
      responseBody <- readResponseBody response
      Text.isInfixOf "<script nonce=\"" responseBody `shouldBe` True

    it "passes raw query strings to the stored route parser while keeping request paths path-only" $ do
      requestObservabilityReference <- newIORef Nothing
      let queryApplication =
            sampleApplication
              { reportRequestObservability = writeIORef requestObservabilityReference . Just
              }
          queryRequest =
            (waiRequest ["query"])
              { Wai.rawQueryString = "?q=server%20rendering"
              }
      response <- performWaiRequest (toWaiApplication queryApplication) queryRequest
      Wai.responseStatus response `shouldBe` Http.status200
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just (TextEncoding.encodeUtf8 "text/plain; charset=utf-8")
      readResponseBody response `shouldReturn` "q=server%20rendering"
      maybeRequestObservability <- readIORef requestObservabilityReference
      fmap
        ( filter ((== "url.path") . Observability.attributeName)
            . Observability.requestSpanAttributes
            . Observability.observabilityRequestSpan
        )
        maybeRequestObservability
        `shouldBe` Just
          [ Observability.ObservabilityAttribute
              { Observability.attributeName = "url.path",
                Observability.attributeValue = Observability.TextAttribute "/query"
              }
          ]

    it "treats an empty raw path as the root path" $ do
      response <- performWaiRequest (toWaiApplication rootPathApplication) Wai.defaultRequest
      Wai.responseStatus response `shouldBe` Http.status200
      responseBody <- readResponseBody response
      Text.isInfixOf "<a href=\"/\" data-page-link=\"true\" aria-current=\"page\">Known</a>" responseBody `shouldBe` True

    it "normalizes forwarded root prefixes for route matching and rendered root links" $ do
      let prefixedRootRequest =
            Wai.defaultRequest
              { Wai.rawPathInfo = "/app",
                Wai.requestHeaders = [("X-Forwarded-Prefix", "app")]
              }
      response <- performWaiRequest (toWaiApplication rootPathApplication) prefixedRootRequest
      Wai.responseStatus response `shouldBe` Http.status200
      responseBody <- readResponseBody response
      Text.isInfixOf "<a href=\"/app\" data-page-link=\"true\" aria-current=\"page\">Known</a>" responseBody `shouldBe` True

    it "uses forwarded path prefixes for route matching and rendered navigation links" $ do
      let prefixedRequest =
            Wai.defaultRequest
              { Wai.rawPathInfo = "/app/known",
                Wai.requestHeaders = [("X-Forwarded-Prefix", "/app")]
              }
      response <- performWaiRequest (toWaiApplication trustedForwardedApplication) prefixedRequest
      Wai.responseStatus response `shouldBe` Http.status200
      responseBody <- readResponseBody response
      Text.isInfixOf "<a href=\"/app/known\" data-page-link=\"true\" aria-current=\"page\">Known</a>" responseBody `shouldBe` True

    it "renders the not-found page through the shared shell with a 404 status" $ do
      response <- performWaiRequest (toWaiApplication sampleApplication) (waiRequest ["missing"])
      Wai.responseStatus response `shouldBe` Http.status404
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just (TextEncoding.encodeUtf8 "text/html; charset=utf-8")
      responseBody <- readResponseBody response
      Text.isInfixOf "<h1>Missing</h1>" responseBody `shouldBe` True

    it "keeps an unknown method-path pair as a 404" $ do
      let missingDeleteRequest = (waiRequest ["missing"]) {Wai.requestMethod = "DELETE"}
      response <- performWaiRequest (toWaiApplication sampleApplication) missingDeleteRequest
      expectAll
        ( (Wai.responseStatus response `shouldBe` Http.status404)
            :| [lookup Http.hAllow (Wai.responseHeaders response) `shouldBe` Nothing]
        )

    it "decodes an invalid request method leniently before route matching" $ do
      let malformedMethodRequest = (waiRequest ["known"]) {Wai.requestMethod = "\xFF"}
      response <- performWaiRequest (toWaiApplication sampleApplication) malformedMethodRequest
      Wai.responseStatus response `shouldBe` Http.status405

    it "returns 405 with a derived Allow header when a known route rejects the method" $ do
      let knownDeleteRequest = (waiRequest ["known"]) {Wai.requestMethod = "DELETE"}
      response <- performWaiRequest (toWaiApplication sampleApplication) knownDeleteRequest
      expectAll
        ( (Wai.responseStatus response `shouldBe` Http.status405)
            :| [ lookup Http.hAllow (Wai.responseHeaders response) `shouldBe` Just "GET, HEAD, OPTIONS",
                 readResponseBody response `shouldReturn` ""
               ]
        )

    it "derives HEAD from GET while preserving the ordinary response headers" $ do
      let knownHeadRequest = (waiRequest ["known"]) {Wai.requestMethod = "HEAD"}
      response <- performWaiRequest (toWaiApplication sampleApplication) knownHeadRequest
      expectAll
        ( (Wai.responseStatus response `shouldBe` Http.status200)
            :| [ lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just "text/html; charset=utf-8",
                 readResponseBody response `shouldReturn` ""
               ]
        )

    it "synthesizes OPTIONS from a matched route declaration" $ do
      let dataOptionsRequest = (waiRequest ["data"]) {Wai.requestMethod = "OPTIONS"}
      response <- performWaiRequest (toWaiApplication sampleApplication) dataOptionsRequest
      expectAll
        ( (Wai.responseStatus response `shouldBe` Http.status204)
            :| [ lookup Http.hAllow (Wai.responseHeaders response) `shouldBe` Just "GET, POST, HEAD, OPTIONS",
                 readResponseBody response `shouldReturn` ""
               ]
        )

    it "preserves body-response status, content type, and body" $ do
      response <- performWaiRequest (toWaiApplication sampleApplication) (waiRequest ["data"])
      Http.statusCode (Wai.responseStatus response) `shouldBe` 202
      Http.statusMessage (Wai.responseStatus response) `shouldBe` "Accepted"
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just (TextEncoding.encodeUtf8 "application/json")
      readResponseBody response `shouldReturn` "{\"route\":\"data\"}"

    it "handles secure requests through the same response-selection path" $ do
      let secureRequest =
            (waiRequest ["data"])
              { Wai.isSecure = True,
                Wai.requestMethod = "POST"
              }
      response <- performWaiRequest (toWaiApplication sampleApplication) secureRequest
      Http.statusCode (Wai.responseStatus response) `shouldBe` 202
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just (TextEncoding.encodeUtf8 "application/json")
      readResponseBody response `shouldReturn` "{\"route\":\"data\"}"

    it "redirects insecure requests to HTTPS before rendering the application response" $ do
      let redirectingApplication =
            (sampleApplicationWithConfig emptyStaticAssets (defaultRequestPolicy {redirectHttpToHttps = True}))
              { renderRequestResponse = \_ _ -> expectationFailure "expected HTTPS redirect before application rendering" >> pure (renderSampleResponse (RouteRequest {requestRoute = DataRoute, requestContext = defaultContext}))
              }
          redirectRequest =
            (waiRequest ["data"])
              { Wai.rawQueryString = "?from=plain-http",
                Wai.requestHeaders = [("Host", "app.example.com:80")]
              }
      response <- performWaiRequest (toWaiApplication redirectingApplication) redirectRequest
      Wai.responseStatus response `shouldBe` Http.status308
      lookup Http.hLocation (Wai.responseHeaders response) `shouldBe` Just "https://app.example.com/data?from=plain-http"
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just (TextEncoding.encodeUtf8 "text/plain; charset=utf-8")
      readResponseBody response `shouldReturn` "Redirecting to HTTPS"

    -- Tabled per docs/design-guidance.md's CN decision record: one act
    -- (perform a request against toWaiApplication under a given request
    -- policy, check only its status and Location header), differing in
    -- the request and the request-policy override. The prior it above
    -- stays separate: it overrides renderRequestResponse and asserts on
    -- Content-Type and body too, not just status/Location. The it's
    -- below (HSTS headers, more assertions) are separate acts too.
    [ ( "redirects to the configured canonical authority instead of an untrusted Host header",
        (waiRequest ["data"]) {Wai.rawQueryString = "?from=plain-http", Wai.requestHeaders = [("Host", "evil.example")]},
        defaultRequestPolicy {redirectHttpToHttps = True},
        Http.status308,
        Just "https://app.example.com/data?from=plain-http"
      ),
      ( "does not redirect at all when no canonical authority is configured",
        (waiRequest ["data"]) {Wai.requestHeaders = [("Host", "evil.example")]},
        defaultRequestPolicy {redirectHttpToHttps = True, httpsRedirectAuthority = Nothing},
        Http.status202,
        Nothing
      ),
      ( "rewrites redirects to the configured HTTPS listener port",
        (waiRequest ["data"]) {Wai.rawQueryString = "?from=runtime-http", Wai.requestHeaders = [("Host", "app.example.com:5001")]},
        defaultRequestPolicy {redirectHttpToHttps = True, httpsRedirectPort = Just 5443},
        Http.status308,
        Just "https://app.example.com:5443/data?from=runtime-http"
      ),
      ( "drops the default HTTPS port from redirect locations when configured explicitly",
        (waiRequest ["data"]) {Wai.requestHeaders = [("Host", "app.example.com:80")]},
        defaultRequestPolicy {redirectHttpToHttps = True, httpsRedirectPort = Just 443},
        Http.status308,
        Just "https://app.example.com/data"
      ),
      ( "keeps forwarded path prefixes in HTTPS redirect locations",
        Wai.defaultRequest {Wai.rawPathInfo = "/second", Wai.rawQueryString = "?from=plain", Wai.requestHeaders = [("Host", "app.example.com"), ("X-Forwarded-Prefix", "/app")]},
        defaultRequestPolicy {redirectHttpToHttps = True, forwardedHeaderTrust = testTrustedForwardedProxy},
        Http.status308,
        Just "https://app.example.com/app/second?from=plain"
      ),
      ( "keeps forwarded path prefixes in HTTPS redirects for the root route",
        Wai.defaultRequest {Wai.rawPathInfo = "/app", Wai.requestHeaders = [("Host", "app.example.com"), ("X-Forwarded-Prefix", "app")]},
        defaultRequestPolicy {redirectHttpToHttps = True, forwardedHeaderTrust = testTrustedForwardedProxy},
        Http.status308,
        Just "https://app.example.com/app"
      ),
      ( "rejects a hostile trusted forwarded prefix in HTTPS redirects",
        Wai.defaultRequest {Wai.rawPathInfo = "/second", Wai.requestHeaders = [("Host", "app.example.com"), ("X-Forwarded-Prefix", "//attacker.example")]},
        defaultRequestPolicy {redirectHttpToHttps = True, forwardedHeaderTrust = testTrustedForwardedProxy},
        Http.status308,
        Just "https://app.example.com/second"
      ),
      ( "does not redirect ACME http-01 challenge paths",
        Wai.defaultRequest {Wai.rawPathInfo = "/.well-known/acme-challenge/token", Wai.requestHeaders = [("Host", "app.example.com:5001")]},
        defaultRequestPolicy {redirectHttpToHttps = True, httpsRedirectPort = Just 5443},
        Http.status404,
        Nothing
      ),
      ( "redirects the root path without requiring an explicit :80 host suffix",
        Wai.defaultRequest {Wai.requestHeaders = [("Host", "app.example.com")]},
        defaultRequestPolicy {redirectHttpToHttps = True},
        Http.status308,
        Just "https://app.example.com/"
      )
      ]
      `forM_` \(label, redirectRequest, requestPolicyConfig, expectedStatus, expectedLocation) ->
        it label $ do
          response <- performWaiRequest (toWaiApplication (sampleApplicationWithConfig emptyStaticAssets requestPolicyConfig)) redirectRequest
          Wai.responseStatus response `shouldBe` expectedStatus
          lookup Http.hLocation (Wai.responseHeaders response) `shouldBe` expectedLocation

    it "uses forwarded HTTPS context to skip redirects and emit HSTS headers" $ do
      let requestPolicyConfig =
            RequestPolicyConfig
              { redirectHttpToHttps = True,
                httpsRedirectPort = Nothing,
                httpsRedirectAuthority = Just "app.example.com",
                strictTransportSecurity =
                  Just
                    StrictTransportSecurityConfig
                      { strictTransportSecurityMaxAgeSeconds = 31536000,
                        strictTransportSecurityIncludeSubDomains = True,
                        strictTransportSecurityPreload = True
                      },
                forwardedHeaderTrust = testTrustedForwardedProxy,
                requestHeadLimits = unboundedRequestHeadLimits,
                requestTransportLimits = warpDefaultRequestTransportLimits,
                requestConcurrencyLimit = Nothing,
                corsPolicy = defaultCorsPolicyConfig,
                responseSecurityHeaders = defaultResponseSecurityHeadersConfig
              }
          proxiedHttpsRequest =
            waiRequestWithRemoteHostAndHeaders
              ["data"]
              (Socket.SockAddrInet 4123 (Socket.tupleToHostAddress (127, 0, 0, 1)))
              [ ("Host", "app.example.com"),
                ("X-Forwarded-Proto", "https")
              ]
      response <- performWaiRequest (toWaiApplication (sampleApplicationWithConfig emptyStaticAssets requestPolicyConfig)) proxiedHttpsRequest
      Http.statusCode (Wai.responseStatus response) `shouldBe` 202
      lookup Http.hLocation (Wai.responseHeaders response) `shouldBe` Nothing
      lookup "Strict-Transport-Security" (Wai.responseHeaders response)
        `shouldBe` Just "max-age=31536000; includeSubDomains; preload"
      readResponseBody response `shouldReturn` "{\"route\":\"data\"}"

    it "ignores forwarded HTTPS context from a peer outside every trusted CIDR block" $ do
      let requestPolicyConfig =
            RequestPolicyConfig
              { redirectHttpToHttps = True,
                httpsRedirectPort = Nothing,
                httpsRedirectAuthority = Just "app.example.com",
                strictTransportSecurity =
                  Just
                    StrictTransportSecurityConfig
                      { strictTransportSecurityMaxAgeSeconds = 31536000,
                        strictTransportSecurityIncludeSubDomains = True,
                        strictTransportSecurityPreload = True
                      },
                forwardedHeaderTrust = testTrustedForwardedProxy,
                requestHeadLimits = unboundedRequestHeadLimits,
                requestTransportLimits = warpDefaultRequestTransportLimits,
                requestConcurrencyLimit = Nothing,
                corsPolicy = defaultCorsPolicyConfig,
                responseSecurityHeaders = defaultResponseSecurityHeadersConfig
              }
          untrustedPeerRequest =
            waiRequestWithRemoteHostAndHeaders
              ["data"]
              (Socket.SockAddrInet 4123 (Socket.tupleToHostAddress (203, 0, 113, 1)))
              [ ("Host", "app.example.com"),
                ("X-Forwarded-Proto", "https")
              ]
      response <- performWaiRequest (toWaiApplication (sampleApplicationWithConfig emptyStaticAssets requestPolicyConfig)) untrustedPeerRequest
      Wai.responseStatus response `shouldBe` Http.status308
      lookup "Strict-Transport-Security" (Wai.responseHeaders response) `shouldBe` Nothing

    it "does not emit HSTS headers for requests whose effective scheme stays HTTP" $ do
      let requestPolicyConfig =
            RequestPolicyConfig
              { redirectHttpToHttps = False,
                httpsRedirectPort = Nothing,
                httpsRedirectAuthority = Nothing,
                strictTransportSecurity =
                  Just
                    StrictTransportSecurityConfig
                      { strictTransportSecurityMaxAgeSeconds = 31536000,
                        strictTransportSecurityIncludeSubDomains = True,
                        strictTransportSecurityPreload = False
                      },
                forwardedHeaderTrust = NeverTrustForwarded,
                requestHeadLimits = unboundedRequestHeadLimits,
                requestTransportLimits = warpDefaultRequestTransportLimits,
                requestConcurrencyLimit = Nothing,
                corsPolicy = defaultCorsPolicyConfig,
                responseSecurityHeaders = defaultResponseSecurityHeadersConfig
              }
      response <- performWaiRequest (toWaiApplication (sampleApplicationWithConfig emptyStaticAssets requestPolicyConfig)) (waiRequest ["data"])
      Http.statusCode (Wai.responseStatus response) `shouldBe` 202
      lookup "Strict-Transport-Security" (Wai.responseHeaders response) `shouldBe` Nothing

    it "emits strict default response security headers without enabling cross-origin reads" $ do
      response <- performWaiRequest (toWaiApplication (sampleApplicationWithConfig emptyStaticAssets defaultRequestPolicy)) (waiRequest ["data"])
      let headers = Wai.responseHeaders response
      lookup "Content-Security-Policy" headers `shouldBe` Just (TextEncoding.encodeUtf8 defaultContentSecurityPolicy)
      lookup "X-Content-Type-Options" headers `shouldBe` Just "nosniff"
      lookup "X-XSS-Protection" headers `shouldBe` Just "1; mode=block"
      lookup "Referrer-Policy" headers `shouldBe` Just "strict-origin-when-cross-origin"
      lookup "Permissions-Policy" headers `shouldBe` Just "accelerometer=(), camera=(), geolocation=(), gyroscope=(), magnetometer=(), microphone=(), payment=(), usb=()"
      lookup "X-Frame-Options" headers `shouldBe` Just "DENY"
      lookup "Access-Control-Allow-Origin" headers `shouldBe` Nothing

    it "allows response security headers to be disabled explicitly" $ do
      let requestPolicyConfig =
            defaultRequestPolicy
              { responseSecurityHeaders =
                  ResponseSecurityHeadersConfig
                    { contentSecurityPolicy = Nothing,
                      contentTypeOptionsNoSniff = False,
                      xssProtection = Nothing,
                      referrerPolicy = Nothing,
                      permissionsPolicy = Nothing,
                      frameOptions = Nothing
                    }
              }
      response <- performWaiRequest (toWaiApplication (sampleApplicationWithConfig emptyStaticAssets requestPolicyConfig)) (waiRequest ["data"])
      let headers = Wai.responseHeaders response
      lookup "Content-Security-Policy" headers `shouldBe` Nothing
      lookup "X-Content-Type-Options" headers `shouldBe` Nothing
      lookup "X-XSS-Protection" headers `shouldBe` Nothing
      lookup "Referrer-Policy" headers `shouldBe` Nothing
      lookup "Permissions-Policy" headers `shouldBe` Nothing
      lookup "X-Frame-Options" headers `shouldBe` Nothing

    it "applies response security headers to HTTPS redirects" $ do
      let requestPolicyConfig =
            defaultRequestPolicy
              { redirectHttpToHttps = True
              }
          redirectRequest =
            (waiRequest [])
              { Wai.requestHeaders = [("Host", "app.example.com")]
              }
      response <- performWaiRequest (toWaiApplication (sampleApplicationWithConfig emptyStaticAssets requestPolicyConfig)) redirectRequest
      Wai.responseStatus response `shouldBe` Http.status308
      lookup "Content-Security-Policy" (Wai.responseHeaders response) `shouldBe` Just (TextEncoding.encodeUtf8 defaultContentSecurityPolicy)

    it "reports request observability for HTTPS redirects with the externally visible request path" $ do
      requestObservabilityReference <- newIORef []
      let directRemoteHost =
            Socket.SockAddrInet 4123 (Socket.tupleToHostAddress (127, 0, 0, 1))
          clientAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "client.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          peerAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "network.peer.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          forwardedPrefixAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "http.request.header.x_forwarded_prefix",
                Observability.attributeValue = Observability.TextAttribute "/app"
              }
          redirectingApplication =
            (sampleApplicationWithConfig emptyStaticAssets (defaultRequestPolicy {redirectHttpToHttps = True, forwardedHeaderTrust = testTrustedForwardedProxy}))
              { renderRequestResponse = \_ _ -> expectationFailure "expected HTTPS redirect before application rendering" >> pure (renderSampleResponse (RouteRequest {requestRoute = DataRoute, requestContext = defaultContext})),
                reportRequestObservability = \requestObservabilityValue ->
                  modifyIORef' requestObservabilityReference (<> [requestObservabilityValue])
              }
          redirectRequest =
            waiRequestWithRemoteHostAndHeaders
              ["second"]
              directRemoteHost
              [ ("Host", "app.example.com"),
                ("X-Forwarded-Prefix", "/app")
              ]
      response <- performWaiRequest (toWaiApplication redirectingApplication) redirectRequest
      Wai.responseStatus response `shouldBe` Http.status308
      capturedRequestObservability <- readIORef requestObservabilityReference
      map stripVolatileRequestTiming capturedRequestObservability
        `shouldBe` [ Observability.buildRequestObservability
                       Observability.RequestIdentity
                         { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "GET",
                           Observability.requestIdentityScheme = "http",
                           Observability.requestIdentityPath = "/second",
                           Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/app/second"
                         }
                       308
                       Observability.BodyResponseKind
                       [clientAddressAttribute, peerAddressAttribute, forwardedPrefixAttribute]
                   ]
      mapM_ expectMeasuredRootRequestTiming capturedRequestObservability

    it "propagates incoming W3C trace context for HTTPS redirect observability" $ do
      requestObservabilityReference <- newIORef []
      let directRemoteHost =
            Socket.SockAddrInet 4123 (Socket.tupleToHostAddress (127, 0, 0, 1))
          traceContext =
            Observability.RequestTraceContext
              { Observability.traceContextTraceId = "4bf92f3577b34da6a3ce929d0e0e4736",
                Observability.traceContextParentSpanId = "00f067aa0ba902b7",
                Observability.traceContextState = Just "vendor=value"
              }
          clientAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "client.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          peerAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "network.peer.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          redirectingApplication =
            (sampleApplicationWithConfig emptyStaticAssets (defaultRequestPolicy {redirectHttpToHttps = True}))
              { renderRequestResponse = \_ _ -> expectationFailure "expected HTTPS redirect before application rendering" >> pure (renderSampleResponse (RouteRequest {requestRoute = DataRoute, requestContext = defaultContext})),
                reportRequestObservability = \requestObservabilityValue ->
                  modifyIORef' requestObservabilityReference (<> [stripVolatileRequestTiming requestObservabilityValue])
              }
          redirectRequest =
            waiRequestWithRemoteHostAndHeaders
              ["second"]
              directRemoteHost
              [ ("Host", "app.example.com"),
                ("traceparent", "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"),
                ("tracestate", "vendor=value")
              ]
      response <- performWaiRequest (toWaiApplication redirectingApplication) redirectRequest
      Wai.responseStatus response `shouldBe` Http.status308
      readIORef requestObservabilityReference
        `shouldReturn` [ Observability.withRequestTraceContext
                           traceContext
                           ( Observability.buildRequestObservability
                               Observability.RequestIdentity
                                 { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "GET",
                                   Observability.requestIdentityScheme = "http",
                                   Observability.requestIdentityPath = "/second",
                                   Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/second"
                                 }
                               308
                               Observability.BodyResponseKind
                               [clientAddressAttribute, peerAddressAttribute]
                           )
                       ]

    it "adds CORS headers only for explicitly allowed origins" $ do
      let requestPolicyConfig =
            defaultRequestPolicy
              { corsPolicy =
                  defaultCorsPolicyConfig
                    { corsAllowedOrigins = ["https://client.example.com"]
                    }
              }
          allowedRequest =
            (waiRequest ["data"])
              { Wai.requestHeaders = [("Origin", "https://client.example.com")]
              }
          blockedRequest =
            (waiRequest ["data"])
              { Wai.requestHeaders = [("Origin", "https://evil.example.com")]
              }
      allowedResponse <- performWaiRequest (toWaiApplication (sampleApplicationWithConfig emptyStaticAssets requestPolicyConfig)) allowedRequest
      blockedResponse <- performWaiRequest (toWaiApplication (sampleApplicationWithConfig emptyStaticAssets requestPolicyConfig)) blockedRequest
      lookup "Access-Control-Allow-Origin" (Wai.responseHeaders allowedResponse) `shouldBe` Just "https://client.example.com"
      lookup "Vary" (Wai.responseHeaders allowedResponse) `shouldBe` Just "Origin"
      lookup "Access-Control-Allow-Methods" (Wai.responseHeaders allowedResponse) `shouldBe` Nothing
      lookup "Access-Control-Allow-Origin" (Wai.responseHeaders blockedResponse) `shouldBe` Nothing

    it "answers allowed CORS preflight requests with constrained methods and headers" $ do
      let requestPolicyConfig =
            defaultRequestPolicy
              { corsPolicy =
                  CorsPolicyConfig
                    { corsAllowedOrigins = ["https://client.example.com"],
                      corsAllowedMethods = ["GET", "HEAD"],
                      corsAllowedHeaders = ["Content-Type", "X-Requested-With"],
                      corsMaxAgeSeconds = Just 600
                    }
              }
          preflightRequest =
            (waiRequest ["data"])
              { Wai.requestMethod = "OPTIONS",
                Wai.requestHeaders =
                  [ ("Origin", "https://client.example.com"),
                    ("Access-Control-Request-Method", "GET")
                  ]
              }
      response <- performWaiRequest (toWaiApplication (sampleApplicationWithConfig emptyStaticAssets requestPolicyConfig)) preflightRequest
      Wai.responseStatus response `shouldBe` Http.status204
      lookup "Access-Control-Allow-Origin" (Wai.responseHeaders response) `shouldBe` Just "https://client.example.com"
      lookup "Access-Control-Allow-Methods" (Wai.responseHeaders response) `shouldBe` Just "GET, HEAD"
      lookup "Access-Control-Allow-Headers" (Wai.responseHeaders response) `shouldBe` Just "Content-Type, X-Requested-With"
      lookup "Access-Control-Max-Age" (Wai.responseHeaders response) `shouldBe` Just "600"
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Nothing
      readResponseBody response `shouldReturn` ""

    it "does not answer CORS preflight requests for disallowed methods" $ do
      let requestPolicyConfig =
            defaultRequestPolicy
              { corsPolicy =
                  CorsPolicyConfig
                    { corsAllowedOrigins = ["https://client.example.com"],
                      corsAllowedMethods = ["GET"],
                      corsAllowedHeaders = [],
                      corsMaxAgeSeconds = Nothing
                    }
              }
          preflightRequest =
            (waiRequest ["data"])
              { Wai.requestMethod = "OPTIONS",
                Wai.requestHeaders =
                  [ ("Origin", "https://client.example.com"),
                    ("Access-Control-Request-Method", "DELETE")
                  ]
              }
      response <- performWaiRequest (toWaiApplication (sampleApplicationWithConfig emptyStaticAssets requestPolicyConfig)) preflightRequest
      Wai.responseStatus response `shouldBe` Http.status204
      lookup "Access-Control-Allow-Origin" (Wai.responseHeaders response) `shouldBe` Just "https://client.example.com"
      lookup "Access-Control-Allow-Methods" (Wai.responseHeaders response) `shouldBe` Nothing
      lookup "Access-Control-Allow-Headers" (Wai.responseHeaders response) `shouldBe` Nothing

    it "omits optional CORS preflight headers when they are not configured" $ do
      let requestPolicyConfig =
            defaultRequestPolicy
              { corsPolicy =
                  CorsPolicyConfig
                    { corsAllowedOrigins = ["https://client.example.com"],
                      corsAllowedMethods = ["GET"],
                      corsAllowedHeaders = [],
                      corsMaxAgeSeconds = Nothing
                    }
              }
          preflightRequest =
            (waiRequest ["data"])
              { Wai.requestMethod = "OPTIONS",
                Wai.requestHeaders =
                  [ ("Origin", "https://client.example.com"),
                    ("Access-Control-Request-Method", "GET")
                  ]
              }
      response <- performWaiRequest (toWaiApplication (sampleApplicationWithConfig emptyStaticAssets requestPolicyConfig)) preflightRequest
      Wai.responseStatus response `shouldBe` Http.status204
      lookup "Access-Control-Allow-Methods" (Wai.responseHeaders response) `shouldBe` Just "GET"
      lookup "Access-Control-Allow-Headers" (Wai.responseHeaders response) `shouldBe` Nothing
      lookup "Access-Control-Max-Age" (Wai.responseHeaders response) `shouldBe` Nothing

    it "reports request observability for allowed CORS preflight responses" $ do
      requestObservabilityReference <- newIORef []
      let directRemoteHost =
            Socket.SockAddrInet 4123 (Socket.tupleToHostAddress (127, 0, 0, 1))
          clientAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "client.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          peerAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "network.peer.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          requestPolicyConfig =
            defaultRequestPolicy
              { corsPolicy =
                  CorsPolicyConfig
                    { corsAllowedOrigins = ["https://client.example.com"],
                      corsAllowedMethods = ["GET"],
                      corsAllowedHeaders = [],
                      corsMaxAgeSeconds = Nothing
                    }
              }
          preflightRequest =
            (waiRequestWithRemoteHostAndHeaders ["data"] directRemoteHost [])
              { Wai.requestMethod = "OPTIONS",
                Wai.requestHeaders =
                  [ ("Origin", "https://client.example.com"),
                    ("Access-Control-Request-Method", "GET")
                  ]
              }
          applicationWithObservability =
            (sampleApplicationWithConfig emptyStaticAssets requestPolicyConfig)
              { reportRequestObservability = \requestObservabilityValue ->
                  modifyIORef' requestObservabilityReference (<> [requestObservabilityValue])
              }
      response <- performWaiRequest (toWaiApplication applicationWithObservability) preflightRequest
      Wai.responseStatus response `shouldBe` Http.status204
      capturedRequestObservability <- readIORef requestObservabilityReference
      map stripVolatileRequestTiming capturedRequestObservability
        `shouldBe` [ Observability.buildRequestObservability
                       Observability.RequestIdentity
                         { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "OPTIONS",
                           Observability.requestIdentityScheme = "http",
                           Observability.requestIdentityPath = "/data",
                           Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/data"
                         }
                       204
                       Observability.BodyResponseKind
                       [clientAddressAttribute, peerAddressAttribute]
                   ]
      mapM_ expectMeasuredRootRequestTiming capturedRequestObservability

    it "extracts incoming W3C trace context into request observability without changing local request attributes" $ do
      requestObservabilityReference <- newIORef []
      let directRemoteHost =
            Socket.SockAddrInet 4123 (Socket.tupleToHostAddress (127, 0, 0, 1))
          traceContext =
            Observability.RequestTraceContext
              { Observability.traceContextTraceId = "4bf92f3577b34da6a3ce929d0e0e4736",
                Observability.traceContextParentSpanId = "00f067aa0ba902b7",
                Observability.traceContextState = Just "vendor=value"
              }
          clientAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "client.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          peerAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "network.peer.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          tracedRequest =
            waiRequestWithRemoteHostAndHeaders
              ["data"]
              directRemoteHost
              [ ("traceparent", "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"),
                ("tracestate", "vendor=value")
              ]
          diagnosticApplication =
            sampleApplication
              { reportRequestObservability = \requestObservabilityValue ->
                  modifyIORef' requestObservabilityReference (<> [requestObservabilityValue])
              }
      response <- performWaiRequest (toWaiApplication diagnosticApplication) tracedRequest
      Http.statusCode (Wai.responseStatus response) `shouldBe` 202
      capturedRequestObservability <- readIORef requestObservabilityReference
      map stripVolatileRequestTiming capturedRequestObservability
        `shouldBe` [ Observability.withRequestTraceContext
                       traceContext
                       ( Observability.buildRequestObservability
                           Observability.RequestIdentity
                             { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "GET",
                               Observability.requestIdentityScheme = "http",
                               Observability.requestIdentityPath = "/data",
                               Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/data"
                             }
                           202
                           Observability.BodyResponseKind
                           [clientAddressAttribute, peerAddressAttribute]
                       )
                   ]
      mapM_ expectMeasuredRequestTiming capturedRequestObservability

    it "ignores malformed W3C traceparent headers" $ do
      requestObservabilityReference <- newIORef []
      let directRemoteHost =
            Socket.SockAddrInet 4123 (Socket.tupleToHostAddress (127, 0, 0, 1))
          clientAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "client.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          peerAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "network.peer.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          tracedRequest =
            waiRequestWithRemoteHostAndHeaders
              ["data"]
              directRemoteHost
              [("traceparent", "00-00000000000000000000000000000000-00f067aa0ba902b7-01")]
          diagnosticApplication =
            sampleApplication
              { reportRequestObservability = \requestObservabilityValue ->
                  modifyIORef' requestObservabilityReference (<> [requestObservabilityValue])
              }
      response <- performWaiRequest (toWaiApplication diagnosticApplication) tracedRequest
      Http.statusCode (Wai.responseStatus response) `shouldBe` 202
      capturedRequestObservability <- readIORef requestObservabilityReference
      map stripVolatileRequestTiming capturedRequestObservability
        `shouldBe` [ Observability.buildRequestObservability
                       Observability.RequestIdentity
                         { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "GET",
                           Observability.requestIdentityScheme = "http",
                           Observability.requestIdentityPath = "/data",
                           Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/data"
                         }
                       202
                       Observability.BodyResponseKind
                       [clientAddressAttribute, peerAddressAttribute]
                   ]
      mapM_ expectMeasuredRequestTiming capturedRequestObservability

    it "ignores forwarded client, scheme, and prefix headers unless trust is enabled" $ do
      requestObservabilityReference <- newIORef []
      let directRemoteHost =
            Socket.SockAddrInet 4123 (Socket.tupleToHostAddress (127, 0, 0, 1))
          forwardedRequest =
            waiRequestWithRemoteHostAndHeaders
              ["data"]
              directRemoteHost
              [ ("X-Forwarded-For", "203.0.113.10, 10.0.0.1"),
                ("X-Forwarded-Proto", "https"),
                ("X-Forwarded-Prefix", "/app")
              ]
          clientAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "client.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          peerAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "network.peer.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          diagnosticApplication =
            sampleApplication
              { reportRequestObservability = \requestObservabilityValue ->
                  modifyIORef' requestObservabilityReference (<> [stripVolatileRequestTiming requestObservabilityValue])
              }
      response <- performWaiRequest (toWaiApplication diagnosticApplication) forwardedRequest
      Http.statusCode (Wai.responseStatus response) `shouldBe` 202
      readIORef requestObservabilityReference
        `shouldReturn` [ Observability.buildRequestObservability
                           Observability.RequestIdentity
                             { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "GET",
                               Observability.requestIdentityScheme = "http",
                               Observability.requestIdentityPath = "/data",
                               Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/data"
                             }
                           202
                           Observability.BodyResponseKind
                           [clientAddressAttribute, peerAddressAttribute]
                       ]

    it "reports body-response observability attributes and logs through the application hooks" $ do
      requestObservabilityReference <- newIORef []
      logEntriesReference <- newIORef []
      let failureAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "exception.type",
                Observability.attributeValue = Observability.TextAttribute "SampleError"
              }
          proxiedRemoteHost =
            Socket.SockAddrInet 4123 (Socket.tupleToHostAddress (127, 0, 0, 1))
          proxiedRequest =
            waiRequestWithRemoteHostAndHeaders
              ["data"]
              proxiedRemoteHost
              [ ("X-Forwarded-For", "203.0.113.10, 10.0.0.1"),
                ("X-Forwarded-Proto", "https"),
                ("X-Forwarded-Prefix", "/app")
              ]
          clientAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "client.address",
                Observability.attributeValue = Observability.TextAttribute "203.0.113.10"
              }
          peerAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "network.peer.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          clientAddressSourceAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "harch.client.address.source",
                Observability.attributeValue = Observability.TextAttribute "x-forwarded-for"
              }
          forwardedForAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "http.request.header.x_forwarded_for",
                Observability.attributeValue = Observability.TextAttribute "203.0.113.10, 10.0.0.1"
              }
          forwardedProtoAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "http.request.header.x_forwarded_proto",
                Observability.attributeValue = Observability.TextAttribute "https"
              }
          forwardedPrefixAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "http.request.header.x_forwarded_prefix",
                Observability.attributeValue = Observability.TextAttribute "/app"
              }
          databaseOperation =
            Database.DatabaseOperation
              { Database.databaseOperationSystem = "postgresql",
                Database.databaseOperationName = "load-data",
                Database.databaseQueryTemplate = "SELECT payload FROM data WHERE id = ?;",
                Database.databaseOperationStartedAtNanoseconds = Nothing,
                Database.databaseOperationEndedAtNanoseconds = Nothing
              }
          diagnosticApplication =
            trustedForwardedApplication
              { renderRequestResponse =
                  \_ _ ->
                    pure $
                      BodyResponse
                        ResponseBody
                          { responseStatus = Http.status503,
                            responseContentType = "application/json",
                            responseBody = "{\"error\":\"data-unavailable\"}",
                            responseObservabilityAttributes = [failureAttribute],
                            responseLogEntries = ["Sample failure log"],
                            responseDatabaseOperations = [databaseOperation]
                          },
                reportRequestObservability = \requestObservabilityValue ->
                  modifyIORef' requestObservabilityReference (<> [stripVolatileRequestTiming requestObservabilityValue]),
                reportApplicationLog = \logEntry ->
                  modifyIORef' logEntriesReference (<> [logEntry])
              }
      response <- performWaiRequest (toWaiApplication diagnosticApplication) proxiedRequest
      Http.statusCode (Wai.responseStatus response) `shouldBe` 503
      readResponseBody response `shouldReturn` "{\"error\":\"data-unavailable\"}"
      readIORef requestObservabilityReference
        `shouldReturn` [ Observability.withDatabaseOperations [databaseOperation] $
                           Observability.buildRequestObservability
                             Observability.RequestIdentity
                               { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "GET",
                                 Observability.requestIdentityScheme = "https",
                                 Observability.requestIdentityPath = "/data",
                                 Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/app/data"
                               }
                             503
                             Observability.BodyResponseKind
                             [ clientAddressAttribute,
                               peerAddressAttribute,
                               clientAddressSourceAttribute,
                               forwardedForAttribute,
                               forwardedProtoAttribute,
                               forwardedPrefixAttribute,
                               failureAttribute
                             ]
                       ]
      readIORef logEntriesReference
        >>= mapM_ (\logEntry -> stripRequestIdLogPrefix logEntry `shouldBe` "[client.address=\"203.0.113.10\" network.peer.address=\"127.0.0.1\" harch.client.address.source=\"x-forwarded-for\" http.request.header.x_forwarded_for=\"203.0.113.10, 10.0.0.1\" http.request.header.x_forwarded_proto=\"https\" http.request.header.x_forwarded_prefix=\"/app\" url.scheme=\"https\"] Sample failure log")

    it "enriches request observability with safe forwarded, user-agent, referrer, and request-source attributes" $ do
      requestObservabilityReference <- newIORef []
      logEntriesReference <- newIORef []
      let directRemoteHost =
            Socket.SockAddrInet 4123 (Socket.tupleToHostAddress (127, 0, 0, 1))
          enrichedRequest =
            waiRequestWithRemoteHostAndHeaders
              ["data"]
              directRemoteHost
              [ ("Forwarded", "for=\"198.51.100.7\";proto=\"https\""),
                ("User-Agent", "curl/8.7.1"),
                ("Referer", "https://client.example.com/path?secret=token#fragment"),
                ("X-Requested-With", "tiny-navigation")
              ]
          clientAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "client.address",
                Observability.attributeValue = Observability.TextAttribute "198.51.100.7"
              }
          peerAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "network.peer.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          clientAddressSourceAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "harch.client.address.source",
                Observability.attributeValue = Observability.TextAttribute "forwarded"
              }
          forwardedAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "http.request.header.forwarded",
                Observability.attributeValue = Observability.TextAttribute "for=\"198.51.100.7\";proto=\"https\""
              }
          userAgentAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "user_agent.original",
                Observability.attributeValue = Observability.TextAttribute "curl/8.7.1"
              }
          refererAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "http.request.header.referer",
                Observability.attributeValue = Observability.TextAttribute "https://client.example.com/path"
              }
          requestedWithAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "http.request.header.x_requested_with",
                Observability.attributeValue = Observability.TextAttribute "tiny-navigation"
              }
          requestSourceAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "harch.request.source",
                Observability.attributeValue = Observability.TextAttribute "enhanced-navigation"
              }
          diagnosticApplication =
            trustedForwardedApplication
              { renderRequestResponse =
                  \_ _ ->
                    pure $
                      BodyResponse
                        ResponseBody
                          { responseStatus = Http.status202,
                            responseContentType = "application/json",
                            responseBody = "{\"route\":\"data\"}",
                            responseObservabilityAttributes = [],
                            responseLogEntries = ["Enriched source log"],
                            responseDatabaseOperations = []
                          },
                reportRequestObservability = \requestObservabilityValue ->
                  modifyIORef' requestObservabilityReference (<> [stripVolatileRequestTiming requestObservabilityValue]),
                reportApplicationLog = \logEntry ->
                  modifyIORef' logEntriesReference (<> [logEntry])
              }
      response <- performWaiRequest (toWaiApplication diagnosticApplication) enrichedRequest
      Http.statusCode (Wai.responseStatus response) `shouldBe` 202
      readIORef requestObservabilityReference
        `shouldReturn` [ Observability.buildRequestObservability
                           Observability.RequestIdentity
                             { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "GET",
                               Observability.requestIdentityScheme = "https",
                               Observability.requestIdentityPath = "/data",
                               Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/data"
                             }
                           202
                           Observability.BodyResponseKind
                           [ clientAddressAttribute,
                             peerAddressAttribute,
                             clientAddressSourceAttribute,
                             forwardedAttribute,
                             userAgentAttribute,
                             refererAttribute,
                             requestedWithAttribute,
                             requestSourceAttribute
                           ]
                       ]
      readIORef logEntriesReference
        >>= mapM_ (\logEntry -> stripRequestIdLogPrefix logEntry `shouldBe` "[client.address=\"198.51.100.7\" network.peer.address=\"127.0.0.1\" harch.client.address.source=\"forwarded\" http.request.header.forwarded=\"for=\\\"198.51.100.7\\\";proto=\\\"https\\\"\" user_agent.original=\"curl/8.7.1\" http.request.header.referer=\"https://client.example.com/path\" http.request.header.x_requested_with=\"tiny-navigation\" harch.request.source=\"enhanced-navigation\" url.scheme=\"https\"] Enriched source log")

    it "parses unquoted Forwarded values and ignores empty trusted forwarded elements" $ do
      requestObservabilityReference <- newIORef []
      let directRemoteHost =
            Socket.SockAddrInet 4123 (Socket.tupleToHostAddress (127, 0, 0, 1))
          emptyForwardedRequest =
            waiRequestWithRemoteHostAndHeaders
              ["data"]
              directRemoteHost
              [("Forwarded", " , ")]
          emptyForwardedForRequest =
            waiRequestWithRemoteHostAndHeaders
              ["data"]
              directRemoteHost
              [("Forwarded", "for=\"\";proto=http")]
          unquotedForwardedRequest =
            waiRequestWithRemoteHostAndHeaders
              ["data"]
              directRemoteHost
              [("Forwarded", "for=203.0.113.8;proto=http")]
          peerAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "network.peer.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          emptyForwardedClientAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "client.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          emptyForwardedAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "http.request.header.forwarded",
                Observability.attributeValue = Observability.TextAttribute ","
              }
          emptyForwardedForAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "http.request.header.forwarded",
                Observability.attributeValue = Observability.TextAttribute "for=\"\";proto=http"
              }
          unquotedForwardedClientAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "client.address",
                Observability.attributeValue = Observability.TextAttribute "203.0.113.8"
              }
          unquotedForwardedSourceAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "harch.client.address.source",
                Observability.attributeValue = Observability.TextAttribute "forwarded"
              }
          unquotedForwardedAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "http.request.header.forwarded",
                Observability.attributeValue = Observability.TextAttribute "for=203.0.113.8;proto=http"
              }
          diagnosticApplication =
            trustedForwardedApplication
              { reportRequestObservability = \requestObservabilityValue ->
                  modifyIORef' requestObservabilityReference (<> [stripVolatileRequestTiming requestObservabilityValue])
              }
      Http.statusCode . Wai.responseStatus
        <$> performWaiRequest (toWaiApplication diagnosticApplication) emptyForwardedRequest
          `shouldReturn` 202
      Http.statusCode . Wai.responseStatus
        <$> performWaiRequest (toWaiApplication diagnosticApplication) emptyForwardedForRequest
          `shouldReturn` 202
      Http.statusCode . Wai.responseStatus
        <$> performWaiRequest (toWaiApplication diagnosticApplication) unquotedForwardedRequest
          `shouldReturn` 202
      readIORef requestObservabilityReference
        `shouldReturn` [ Observability.buildRequestObservability
                           Observability.RequestIdentity
                             { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "GET",
                               Observability.requestIdentityScheme = "http",
                               Observability.requestIdentityPath = "/data",
                               Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/data"
                             }
                           202
                           Observability.BodyResponseKind
                           [ emptyForwardedClientAddressAttribute,
                             peerAddressAttribute,
                             emptyForwardedAttribute
                           ],
                         Observability.buildRequestObservability
                           Observability.RequestIdentity
                             { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "GET",
                               Observability.requestIdentityScheme = "http",
                               Observability.requestIdentityPath = "/data",
                               Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/data"
                             }
                           202
                           Observability.BodyResponseKind
                           [ emptyForwardedClientAddressAttribute,
                             peerAddressAttribute,
                             emptyForwardedForAttribute
                           ],
                         Observability.buildRequestObservability
                           Observability.RequestIdentity
                             { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "GET",
                               Observability.requestIdentityScheme = "http",
                               Observability.requestIdentityPath = "/data",
                               Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/data"
                             }
                           202
                           Observability.BodyResponseKind
                           [ unquotedForwardedClientAddressAttribute,
                             peerAddressAttribute,
                             unquotedForwardedSourceAttribute,
                             unquotedForwardedAttribute
                           ]
                       ]

    it "classifies scripted, API, manual, and browser-like request sources" $ do
      requestObservabilityReference <- newIORef []
      let directRemoteHost =
            Socket.SockAddrInet 4123 (Socket.tupleToHostAddress (127, 0, 0, 1))
          diagnosticApplication =
            sampleApplication
              { reportRequestObservability = \requestObservabilityValue ->
                  modifyIORef' requestObservabilityReference (<> [stripVolatileRequestTiming requestObservabilityValue])
              }
          requestWithSource headers =
            performWaiRequest
              (toWaiApplication diagnosticApplication)
              (waiRequestWithRemoteHostAndHeaders ["data"] directRemoteHost headers)
          requestSourceValues requestObservabilityValue =
            [ sourceValue
            | Observability.ObservabilityAttribute
                { Observability.attributeName = "harch.request.source",
                  Observability.attributeValue = Observability.TextAttribute sourceValue
                } <-
                Observability.requestSpanAttributes
                  (Observability.observabilityRequestSpan requestObservabilityValue)
            ]
      _ <- requestWithSource [("X-Requested-With", "XMLHttpRequest")]
      _ <- requestWithSource [("X-Requested-With", "custom-script")]
      _ <- requestWithSource [("Accept", "application/json")]
      _ <- requestWithSource [("User-Agent", "curl/8.7.1")]
      _ <- requestWithSource [("User-Agent", "Mozilla/5.0")]
      _ <- requestWithSource [("Forwarded", " , ")]
      _ <- requestWithSource [("Forwarded", "for=\"\";proto=http")]
      _ <- requestWithSource [("Forwarded", "for=203.0.113.8;proto=http")]
      fmap (map requestSourceValues) (readIORef requestObservabilityReference)
        `shouldReturn` [ ["xml-http-request"],
                         ["scripted-request"],
                         ["api-client"],
                         ["manual-client"],
                         ["browser-or-client"],
                         [],
                         [],
                         []
                       ]

    it "falls back to the direct peer address and request security when forwarding headers are absent" $ do
      requestObservabilityReference <- newIORef []
      logEntriesReference <- newIORef []
      let directRemoteHost =
            Socket.SockAddrInet 4123 (Socket.tupleToHostAddress (127, 0, 0, 1))
          directRequest =
            (waiRequestWithRemoteHostAndHeaders ["data"] directRemoteHost [])
              { Wai.isSecure = True
              }
          clientAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "client.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          peerAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "network.peer.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          diagnosticApplication =
            trustedForwardedApplication
              { renderRequestResponse =
                  \_ _ ->
                    pure $
                      BodyResponse
                        ResponseBody
                          { responseStatus = Http.status202,
                            responseContentType = "application/json",
                            responseBody = "{\"route\":\"data\"}",
                            responseObservabilityAttributes = [],
                            responseLogEntries = ["Direct peer log"],
                            responseDatabaseOperations = []
                          },
                reportRequestObservability = \requestObservabilityValue ->
                  modifyIORef' requestObservabilityReference (<> [stripVolatileRequestTiming requestObservabilityValue]),
                reportApplicationLog = \logEntry ->
                  modifyIORef' logEntriesReference (<> [logEntry])
              }
      response <- performWaiRequest (toWaiApplication diagnosticApplication) directRequest
      Http.statusCode (Wai.responseStatus response) `shouldBe` 202
      readIORef requestObservabilityReference
        `shouldReturn` [ Observability.buildRequestObservability
                           Observability.RequestIdentity
                             { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "GET",
                               Observability.requestIdentityScheme = "https",
                               Observability.requestIdentityPath = "/data",
                               Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/data"
                             }
                           202
                           Observability.BodyResponseKind
                           [clientAddressAttribute, peerAddressAttribute]
                       ]
      readIORef logEntriesReference
        >>= mapM_ (\logEntry -> stripRequestIdLogPrefix logEntry `shouldBe` "[client.address=\"127.0.0.1\" network.peer.address=\"127.0.0.1\" url.scheme=\"https\"] Direct peer log")

    it "preserves page response semantics while surfacing page-level failure status, observability, and logs" $ do
      requestObservabilityReference <- newIORef []
      logEntriesReference <- newIORef []
      let directRemoteHost =
            Socket.SockAddrInet 4123 (Socket.tupleToHostAddress (127, 0, 0, 1))
          pageRequest =
            waiRequestWithRemoteHostAndHeaders ["known"] directRemoteHost []
          clientAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "client.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          peerAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "network.peer.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          failureAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "exception.type",
                Observability.attributeValue = Observability.TextAttribute "SampleError"
              }
          diagnosticApplication =
            sampleApplication
              { renderRequestResponse =
                  \_ ->
                    pure
                      . PageResponseWithMetadata
                        testPageSecurity
                        ResponseBody
                          { responseStatus = Http.status500,
                            responseContentType = "text/html; charset=utf-8",
                            responseBody = "",
                            responseObservabilityAttributes = [failureAttribute],
                            responseLogEntries = ["Sample page failure log"],
                            responseDatabaseOperations = []
                          }
                      . samplePage,
                reportRequestObservability = \requestObservabilityValue ->
                  modifyIORef' requestObservabilityReference (<> [stripVolatileRequestTiming requestObservabilityValue]),
                reportApplicationLog = \logEntry ->
                  modifyIORef' logEntriesReference (<> [logEntry])
              }
      response <- performWaiRequest (toWaiApplication diagnosticApplication) pageRequest
      Http.statusCode (Wai.responseStatus response) `shouldBe` 500
      Http.statusMessage (Wai.responseStatus response) `shouldBe` "Internal Server Error"
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just "text/html; charset=utf-8"
      readResponseBody response
        `shouldReturn` Text.replace
          "<body data-app=\"sample\">"
          "<body data-app=\"sample\" data-harch-csrf-token=\"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\">"
          (renderDocument (pageShell diagnosticApplication (samplePage (RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext}))))
      readIORef requestObservabilityReference
        `shouldReturn` [ Observability.buildRequestObservability
                           Observability.RequestIdentity
                             { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "GET",
                               Observability.requestIdentityScheme = "http",
                               Observability.requestIdentityPath = "/known",
                               Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/known"
                             }
                           500
                           Observability.PageResponseKind
                           [clientAddressAttribute, peerAddressAttribute, failureAttribute]
                       ]
      readIORef logEntriesReference
        >>= mapM_ (\logEntry -> stripRequestIdLogPrefix logEntry `shouldBe` "[client.address=\"127.0.0.1\" network.peer.address=\"127.0.0.1\" url.scheme=\"http\"] Sample page failure log")

    it "retains measured request timing across page and body response variants" $ do
      requestObservabilityReference <- newIORef []
      let directRemoteHost =
            Socket.SockAddrInet 4123 (Socket.tupleToHostAddress (127, 0, 0, 1))
          clientAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "client.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          peerAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "network.peer.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          forwardedPrefixAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "http.request.header.x_forwarded_prefix",
                Observability.attributeValue = Observability.TextAttribute "/app"
              }
          pageFailureAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "exception.type",
                Observability.attributeValue = Observability.TextAttribute "PageFailure"
              }
          bodyFailureAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "exception.type",
                Observability.attributeValue = Observability.TextAttribute "BodyFailure"
              }
          diagnosticApplication =
            sampleApplication
              { applicationRequestPolicy = defaultRequestPolicy {forwardedHeaderTrust = testTrustedForwardedProxy},
                requestContextFromRequest = sampleRequestContextFromRequest (defaultRequestPolicy {forwardedHeaderTrust = testTrustedForwardedProxy}),
                renderRequestResponse =
                  \_ request ->
                    pure $
                      case (requestRoute request, requestLanguage (requestContext request), testContextPathPrefix (requestContext request)) of
                        (KnownRoute, "es", _) ->
                          PageResponseWithMetadata
                            testPageSecurity
                            ResponseBody
                              { responseStatus = Http.status500,
                                responseContentType = "text/html; charset=utf-8",
                                responseBody = "",
                                responseObservabilityAttributes = [pageFailureAttribute],
                                responseLogEntries = [],
                                responseDatabaseOperations = []
                              }
                            (samplePage request)
                        (KnownRoute, _, _) ->
                          PageResponse testPageSecurity (samplePage request)
                        (DataRoute, _, "/app") ->
                          BodyResponse
                            ResponseBody
                              { responseStatus = Http.status503,
                                responseContentType = "application/json",
                                responseBody = "{\"error\":\"body-failure\"}",
                                responseObservabilityAttributes = [bodyFailureAttribute],
                                responseLogEntries = [],
                                responseDatabaseOperations = []
                              }
                        _ ->
                          renderSampleResponse request,
                reportRequestObservability = \requestObservabilityValue ->
                  modifyIORef' requestObservabilityReference (<> [requestObservabilityValue])
              }
          pageSuccessRequest =
            waiRequestWithRemoteHostAndHeaders
              ["known"]
              directRemoteHost
              []
          pageFailureRequest =
            waiRequestWithRemoteHostAndHeaders
              ["es", "known"]
              directRemoteHost
              []
          bodySuccessRequest =
            waiRequestWithRemoteHostAndHeaders
              ["data"]
              directRemoteHost
              []
          bodyFailureRequest =
            waiRequestWithRemoteHostAndHeaders
              ["data"]
              directRemoteHost
              [("X-Forwarded-Prefix", "/app")]
      Http.statusCode . Wai.responseStatus
        <$> performWaiRequest (toWaiApplication diagnosticApplication) pageSuccessRequest
          `shouldReturn` 200
      Http.statusCode . Wai.responseStatus
        <$> performWaiRequest (toWaiApplication diagnosticApplication) pageFailureRequest
          `shouldReturn` 500
      Http.statusCode . Wai.responseStatus
        <$> performWaiRequest (toWaiApplication diagnosticApplication) bodySuccessRequest
          `shouldReturn` 202
      Http.statusCode . Wai.responseStatus
        <$> performWaiRequest (toWaiApplication diagnosticApplication) bodyFailureRequest
          `shouldReturn` 503
      capturedRequestObservability <- readIORef requestObservabilityReference
      map stripVolatileRequestTiming capturedRequestObservability
        `shouldBe` [ Observability.buildRequestObservability
                       Observability.RequestIdentity
                         { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "GET",
                           Observability.requestIdentityScheme = "http",
                           Observability.requestIdentityPath = "/known",
                           Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/known"
                         }
                       200
                       Observability.PageResponseKind
                       [clientAddressAttribute, peerAddressAttribute],
                     Observability.buildRequestObservability
                       Observability.RequestIdentity
                         { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "GET",
                           Observability.requestIdentityScheme = "http",
                           Observability.requestIdentityPath = "/es/known",
                           Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/es/known"
                         }
                       500
                       Observability.PageResponseKind
                       [clientAddressAttribute, peerAddressAttribute, pageFailureAttribute],
                     Observability.buildRequestObservability
                       Observability.RequestIdentity
                         { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "GET",
                           Observability.requestIdentityScheme = "http",
                           Observability.requestIdentityPath = "/data",
                           Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/data"
                         }
                       202
                       Observability.BodyResponseKind
                       [clientAddressAttribute, peerAddressAttribute],
                     Observability.buildRequestObservability
                       Observability.RequestIdentity
                         { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "GET",
                           Observability.requestIdentityScheme = "http",
                           Observability.requestIdentityPath = "/data",
                           Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/app/data"
                         }
                       503
                       Observability.BodyResponseKind
                       [clientAddressAttribute, peerAddressAttribute, forwardedPrefixAttribute, bodyFailureAttribute]
                   ]
      mapM_ expectMeasuredRequestTiming capturedRequestObservability

    it "ignores empty forwarded-for tokens while still honoring forwarded plain-http scheme" $ do
      requestObservabilityReference <- newIORef []
      let directRemoteHost =
            Socket.SockAddrInet 4123 (Socket.tupleToHostAddress (127, 0, 0, 1))
          forwardedRequest =
            ( waiRequestWithRemoteHostAndHeaders
                ["data"]
                directRemoteHost
                [ ("X-Forwarded-For", " , "),
                  ("X-Forwarded-Proto", "http")
                ]
            )
              { Wai.isSecure = True
              }
          clientAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "client.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          peerAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "network.peer.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          forwardedForAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "http.request.header.x_forwarded_for",
                Observability.attributeValue = Observability.TextAttribute ","
              }
          forwardedProtoAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "http.request.header.x_forwarded_proto",
                Observability.attributeValue = Observability.TextAttribute "http"
              }
          diagnosticApplication =
            trustedForwardedApplication
              { reportRequestObservability = \requestObservabilityValue ->
                  modifyIORef' requestObservabilityReference (<> [stripVolatileRequestTiming requestObservabilityValue])
              }
      response <- performWaiRequest (toWaiApplication diagnosticApplication) forwardedRequest
      Http.statusCode (Wai.responseStatus response) `shouldBe` 202
      readIORef requestObservabilityReference
        `shouldReturn` [ Observability.buildRequestObservability
                           Observability.RequestIdentity
                             { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "GET",
                               Observability.requestIdentityScheme = "http",
                               Observability.requestIdentityPath = "/data",
                               Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/data"
                             }
                           202
                           Observability.BodyResponseKind
                           [ clientAddressAttribute,
                             peerAddressAttribute,
                             forwardedForAttribute,
                             forwardedProtoAttribute
                           ]
                       ]

    it "renders non-inet peer addresses into forwarded diagnostics" $ do
      requestObservabilityReference <- newIORef []
      let unixSocketRequest =
            waiRequestWithRemoteHostAndHeaders
              ["data"]
              (Socket.SockAddrUnix "/tmp/harch-web.sock")
              [("X-Forwarded-For", "198.51.100.24")]
          clientAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "client.address",
                Observability.attributeValue = Observability.TextAttribute "198.51.100.24"
              }
          peerAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "network.peer.address",
                Observability.attributeValue = Observability.TextAttribute "/tmp/harch-web.sock"
              }
          forwardedForAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "http.request.header.x_forwarded_for",
                Observability.attributeValue = Observability.TextAttribute "198.51.100.24"
              }
          clientAddressSourceAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "harch.client.address.source",
                Observability.attributeValue = Observability.TextAttribute "x-forwarded-for"
              }
          diagnosticApplication =
            trustedForwardedApplication
              { reportRequestObservability = \requestObservabilityValue ->
                  modifyIORef' requestObservabilityReference (<> [stripVolatileRequestTiming requestObservabilityValue])
              }
      response <- performWaiRequest (toWaiApplication diagnosticApplication) unixSocketRequest
      Http.statusCode (Wai.responseStatus response) `shouldBe` 202
      readIORef requestObservabilityReference
        `shouldReturn` [ Observability.buildRequestObservability
                           Observability.RequestIdentity
                             { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "GET",
                               Observability.requestIdentityScheme = "http",
                               Observability.requestIdentityPath = "/data",
                               Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/data"
                             }
                           202
                           Observability.BodyResponseKind
                           [clientAddressAttribute, peerAddressAttribute, clientAddressSourceAttribute, forwardedForAttribute]
                       ]

    it "groups unmatched requests under a stable not-found span display name while keeping the concrete missing path in attributes" $ do
      requestObservabilityReference <- newIORef []
      let directRemoteHost =
            Socket.SockAddrInet 4123 (Socket.tupleToHostAddress (127, 0, 0, 1))
          missingRequest =
            waiRequestWithRemoteHostAndHeaders
              ["favicon.ico"]
              directRemoteHost
              []
          clientAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "client.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          peerAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "network.peer.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          diagnosticApplication =
            sampleApplication
              { reportRequestObservability = \requestObservabilityValue ->
                  modifyIORef' requestObservabilityReference (<> [requestObservabilityValue])
              }
      response <- performWaiRequest (toWaiApplication diagnosticApplication) missingRequest
      Http.statusCode (Wai.responseStatus response) `shouldBe` 404
      capturedRequestObservability <- readIORef requestObservabilityReference
      map stripVolatileRequestTiming capturedRequestObservability
        `shouldBe` [ Observability.buildRequestObservability
                       Observability.RequestIdentity
                         { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "GET",
                           Observability.requestIdentityScheme = "http",
                           Observability.requestIdentityPath = "/favicon.ico",
                           Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/404"
                         }
                       404
                       Observability.PageResponseKind
                       [clientAddressAttribute, peerAddressAttribute]
                   ]
      mapM_ expectMeasuredRequestTiming capturedRequestObservability

    it "reports request observability for root-prefixed static asset responses with a wildcard route" $
      withSystemTempDirectory "harch-web-static-observability-root" $ \tempDirectory -> do
        requestObservabilityReference <- newIORef []
        let directRemoteHost =
              Socket.SockAddrInet 4123 (Socket.tupleToHostAddress (127, 0, 0, 1))
            assetDirectory = tempDirectory <> "/public"
            assetConfig =
              StaticAssetsConfig
                { staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = "/", staticDirectory = assetDirectory}],
                  staticAssetContentTypes = defaultStaticAssetContentTypes,
                  staticCacheControlSeconds = Nothing
                }
            clientAddressAttribute =
              Observability.ObservabilityAttribute
                { Observability.attributeName = "client.address",
                  Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
                }
            peerAddressAttribute =
              Observability.ObservabilityAttribute
                { Observability.attributeName = "network.peer.address",
                  Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
                }
            staticApplication =
              (sampleApplicationWithStaticAssets assetConfig)
                { reportRequestObservability = \requestObservabilityValue ->
                    modifyIORef' requestObservabilityReference (<> [requestObservabilityValue])
                }
        createDirectoryIfMissing True assetDirectory
        writeFile (assetDirectory <> "/styles.css") "body{}"
        response <- performWaiRequest (toWaiApplication staticApplication) (waiRequestWithRemoteHostAndHeaders ["styles.css"] directRemoteHost [])
        Wai.responseStatus response `shouldBe` Http.status200
        capturedRequestObservability <- readIORef requestObservabilityReference
        map stripVolatileRequestTiming capturedRequestObservability
          `shouldBe` [ Observability.buildRequestObservability
                         Observability.RequestIdentity
                           { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "GET",
                             Observability.requestIdentityScheme = "http",
                             Observability.requestIdentityPath = "/styles.css",
                             Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/*"
                           }
                         200
                         Observability.BodyResponseKind
                         [clientAddressAttribute, peerAddressAttribute]
                     ]
        mapM_ expectMeasuredRootRequestTiming capturedRequestObservability

    it "reports request observability for matched static asset misses with the prefixed wildcard route" $
      withSystemTempDirectory "harch-web-static-observability-missing" $ \tempDirectory -> do
        requestObservabilityReference <- newIORef []
        let directRemoteHost =
              Socket.SockAddrInet 4123 (Socket.tupleToHostAddress (127, 0, 0, 1))
            assetDirectory = tempDirectory <> "/public"
            assetConfig =
              StaticAssetsConfig
                { staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = "/assets", staticDirectory = assetDirectory}],
                  staticAssetContentTypes = defaultStaticAssetContentTypes,
                  staticCacheControlSeconds = Nothing
                }
            forwardedPrefixAttribute =
              Observability.ObservabilityAttribute
                { Observability.attributeName = "http.request.header.x_forwarded_prefix",
                  Observability.attributeValue = Observability.TextAttribute "/app"
                }
            clientAddressAttribute =
              Observability.ObservabilityAttribute
                { Observability.attributeName = "client.address",
                  Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
                }
            peerAddressAttribute =
              Observability.ObservabilityAttribute
                { Observability.attributeName = "network.peer.address",
                  Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
                }
            missingRequest =
              waiRequestWithRemoteHostAndHeaders
                ["app", "assets", "missing.js"]
                directRemoteHost
                [("X-Forwarded-Prefix", "/app")]
            staticApplication =
              (sampleApplicationWithConfig assetConfig (defaultRequestPolicy {forwardedHeaderTrust = testTrustedForwardedProxy}))
                { reportRequestObservability = \requestObservabilityValue ->
                    modifyIORef' requestObservabilityReference (<> [requestObservabilityValue])
                }
        createDirectoryIfMissing True assetDirectory
        response <- performWaiRequest (toWaiApplication staticApplication) missingRequest
        Wai.responseStatus response `shouldBe` Http.status404
        capturedRequestObservability <- readIORef requestObservabilityReference
        map stripVolatileRequestTiming capturedRequestObservability
          `shouldBe` [ Observability.buildRequestObservability
                         Observability.RequestIdentity
                           { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "GET",
                             Observability.requestIdentityScheme = "http",
                             Observability.requestIdentityPath = "/assets/missing.js",
                             Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/app/assets/*"
                           }
                         404
                         Observability.BodyResponseKind
                         [clientAddressAttribute, peerAddressAttribute, forwardedPrefixAttribute]
                     ]
        mapM_ expectMeasuredRootRequestTiming capturedRequestObservability

    it "serves configured static assets with deterministic cache-control headers" $
      withSystemTempDirectory "harch-web-static" $ \tempDirectory -> do
        let assetDirectory = tempDirectory <> "/public"
            assetConfig =
              StaticAssetsConfig
                { staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = "/assets", staticDirectory = assetDirectory}],
                  staticAssetContentTypes = defaultStaticAssetContentTypes,
                  staticCacheControlSeconds = Just 3600
                }
            staticApplication = sampleApplicationWithStaticAssets assetConfig
        createDirectoryIfMissing True assetDirectory
        writeFile (assetDirectory <> "/app.js") "console.log('asset');"
        firstResponse <- performWaiRequest (toWaiApplication staticApplication) (waiRequest ["assets", "app.js"])
        secondResponse <- performWaiRequest (toWaiApplication staticApplication) (waiRequest ["assets", "app.js"])
        Wai.responseStatus firstResponse `shouldBe` Http.status200
        lookup Http.hContentType (Wai.responseHeaders firstResponse) `shouldBe` Just (TextEncoding.encodeUtf8 "application/javascript; charset=utf-8")
        lookup Http.hCacheControl (Wai.responseHeaders firstResponse) `shouldBe` Just (TextEncoding.encodeUtf8 "public, max-age=3600")
        let firstResponseHeaders = Wai.responseHeaders firstResponse
            secondResponseHeaders = Wai.responseHeaders secondResponse
        expectAll
          ( (responseHasCanonicalRequestId firstResponseHeaders `shouldBe` True)
              :| [ responseHasCanonicalRequestId secondResponseHeaders `shouldBe` True,
                   filter ((/= "X-Request-ID") . fst) secondResponseHeaders `shouldBe` filter ((/= "X-Request-ID") . fst) firstResponseHeaders
                 ]
          )
        readResponseBody firstResponse `shouldReturn` "console.log('asset');"
        readResponseBody secondResponse `shouldReturn` "console.log('asset');"

    it "applies HSTS headers to static asset responses when the effective request scheme is HTTPS" $
      withSystemTempDirectory "harch-web-static-hsts" $ \tempDirectory -> do
        let assetDirectory = tempDirectory <> "/public"
            assetConfig =
              StaticAssetsConfig
                { staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = "/assets", staticDirectory = assetDirectory}],
                  staticAssetContentTypes = defaultStaticAssetContentTypes,
                  staticCacheControlSeconds = Nothing
                }
            requestPolicyConfig =
              RequestPolicyConfig
                { redirectHttpToHttps = False,
                  httpsRedirectPort = Nothing,
                  httpsRedirectAuthority = Nothing,
                  strictTransportSecurity =
                    Just
                      StrictTransportSecurityConfig
                        { strictTransportSecurityMaxAgeSeconds = 86400,
                          strictTransportSecurityIncludeSubDomains = False,
                          strictTransportSecurityPreload = False
                        },
                  forwardedHeaderTrust = testTrustedForwardedProxy,
                  requestHeadLimits = unboundedRequestHeadLimits,
                  requestTransportLimits = warpDefaultRequestTransportLimits,
                  requestConcurrencyLimit = Nothing,
                  corsPolicy = defaultCorsPolicyConfig,
                  responseSecurityHeaders = defaultResponseSecurityHeadersConfig
                }
            staticApplication = sampleApplicationWithConfig assetConfig requestPolicyConfig
            proxiedHttpsRequest =
              waiRequestWithRemoteHostAndHeaders
                ["assets", "app.js"]
                (Socket.SockAddrInet 4123 (Socket.tupleToHostAddress (127, 0, 0, 1)))
                [("X-Forwarded-Proto", "https")]
        createDirectoryIfMissing True assetDirectory
        writeFile (assetDirectory <> "/app.js") "console.log('asset');"
        response <- performWaiRequest (toWaiApplication staticApplication) proxiedHttpsRequest
        Wai.responseStatus response `shouldBe` Http.status200
        lookup "Strict-Transport-Security" (Wai.responseHeaders response)
          `shouldBe` Just "max-age=86400"
        readResponseBody response `shouldReturn` "console.log('asset');"

    it "strips forwarded path prefixes before serving static assets" $
      withSystemTempDirectory "harch-web-static-prefix" $ \tempDirectory -> do
        let assetDirectory = tempDirectory <> "/public"
            assetConfig =
              StaticAssetsConfig
                { staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = "/assets", staticDirectory = assetDirectory}],
                  staticAssetContentTypes = defaultStaticAssetContentTypes,
                  staticCacheControlSeconds = Nothing
                }
            prefixedRequest =
              Wai.defaultRequest
                { Wai.rawPathInfo = "/app/assets/app.js",
                  Wai.requestHeaders = [("X-Forwarded-Prefix", "/app")]
                }
            staticApplication =
              sampleApplicationWithConfig
                assetConfig
                defaultRequestPolicy
                  { forwardedHeaderTrust = testTrustedForwardedProxy
                  }
        createDirectoryIfMissing True assetDirectory
        writeFile (assetDirectory <> "/app.js") "console.log('asset');"
        response <- performWaiRequest (toWaiApplication staticApplication) prefixedRequest
        Wai.responseStatus response `shouldBe` Http.status200
        readResponseBody response `shouldReturn` "console.log('asset');"

    it "normalizes a trailing slash in static asset route prefixes" $
      withSystemTempDirectory "harch-web-static-prefix-slash" $ \tempDirectory -> do
        let assetDirectory = tempDirectory <> "/public"
            assetConfig =
              StaticAssetsConfig
                { staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = "/assets/", staticDirectory = assetDirectory}],
                  staticAssetContentTypes = defaultStaticAssetContentTypes,
                  staticCacheControlSeconds = Nothing
                }
            staticApplication = sampleApplicationWithStaticAssets assetConfig
        createDirectoryIfMissing True assetDirectory
        writeFile (assetDirectory <> "/app.js") "console.log('asset');"
        response <- performWaiRequest (toWaiApplication staticApplication) (waiRequest ["assets", "app.js"])
        Wai.responseStatus response `shouldBe` Http.status200
        readResponseBody response `shouldReturn` "console.log('asset');"

    it "serves root-prefixed static assets with the expected content types and no cache header" $
      withSystemTempDirectory "harch-web-static-root" $ \tempDirectory -> do
        let assetDirectory = tempDirectory <> "/public"
            assetConfig =
              StaticAssetsConfig
                { staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = "/", staticDirectory = assetDirectory}],
                  staticAssetContentTypes = defaultStaticAssetContentTypes,
                  staticCacheControlSeconds = Nothing
                }
            staticApplication = sampleApplicationWithStaticAssets assetConfig
            expectedResponses =
              [ (["styles.css"], "body{}", "text/css; charset=utf-8"),
                (["index.html"], "<h1>Home</h1>", "text/html; charset=utf-8"),
                (["data.json"], "{\"ok\":true}", "application/json; charset=utf-8"),
                (["logo.svg"], "<svg></svg>", "image/svg+xml"),
                (["note.txt"], "hello", "text/plain; charset=utf-8")
              ]
        createDirectoryIfMissing True assetDirectory
        writeFile (assetDirectory <> "/styles.css") "body{}"
        writeFile (assetDirectory <> "/index.html") "<h1>Home</h1>"
        writeFile (assetDirectory <> "/data.json") "{\"ok\":true}"
        writeFile (assetDirectory <> "/logo.svg") "<svg></svg>"
        writeFile (assetDirectory <> "/note.txt") "hello"
        writeFile (assetDirectory <> "/blob.bin") "0101"
        mapM_
          ( \(segments, expectedBody, expectedContentType) -> do
              response <- performWaiRequest (toWaiApplication staticApplication) (waiRequest segments)
              Wai.responseStatus response `shouldBe` Http.status200
              lookup Http.hContentType (Wai.responseHeaders response)
                `shouldBe` Just (TextEncoding.encodeUtf8 expectedContentType)
              lookup Http.hCacheControl (Wai.responseHeaders response) `shouldBe` Nothing
              readResponseBody response `shouldReturn` expectedBody
          )
          expectedResponses
        rootResponse <- performWaiRequest (toWaiApplication staticApplication) Wai.defaultRequest
        Wai.responseStatus rootResponse `shouldBe` Http.status404
        lookup Http.hContentType (Wai.responseHeaders rootResponse)
          `shouldBe` Just (TextEncoding.encodeUtf8 "text/html; charset=utf-8")
        lookup Http.hCacheControl (Wai.responseHeaders rootResponse) `shouldBe` Nothing
        rootResponseBody <- readResponseBody rootResponse
        rootResponseBody `shouldNotBe` "Not Found"
        unsupportedExtensionResponse <- performWaiRequest (toWaiApplication staticApplication) (waiRequest ["blob.bin"])
        Wai.responseStatus unsupportedExtensionResponse `shouldBe` Http.status404
        lookup Http.hContentType (Wai.responseHeaders unsupportedExtensionResponse)
          `shouldBe` Just (TextEncoding.encodeUtf8 "text/html; charset=utf-8")

    it "serves configured extensionless static assets when the empty extension is explicitly allowlisted" $
      withSystemTempDirectory "harch-web-static-extensionless" $ \tempDirectory -> do
        let assetDirectory = tempDirectory <> "/public"
            assetConfig =
              StaticAssetsConfig
                { staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = "/assets", staticDirectory = assetDirectory}],
                  staticAssetContentTypes = defaultStaticAssetContentTypes <> [("", "application/octet-stream")],
                  staticCacheControlSeconds = Nothing
                }
            staticApplication = sampleApplicationWithStaticAssets assetConfig
        createDirectoryIfMissing True assetDirectory
        writeFile (assetDirectory <> "/download") "raw"
        response <- performWaiRequest (toWaiApplication staticApplication) (waiRequest ["assets", "download"])
        Wai.responseStatus response `shouldBe` Http.status200
        lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just "application/octet-stream"
        readResponseBody response `shouldReturn` "raw"

    it "serves visible nested static assets but rejects hidden files and hidden directories" $
      withSystemTempDirectory "harch-web-static-hidden" $ \tempDirectory -> do
        let assetDirectory = tempDirectory <> "/public"
            assetConfig =
              StaticAssetsConfig
                { staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = "/assets", staticDirectory = assetDirectory}],
                  staticAssetContentTypes = defaultStaticAssetContentTypes,
                  staticCacheControlSeconds = Nothing
                }
            staticApplication = sampleApplicationWithStaticAssets assetConfig
        createDirectoryIfMissing True (assetDirectory <> "/scripts")
        createDirectoryIfMissing True (assetDirectory <> "/.hidden")
        writeFile (assetDirectory <> "/scripts/app.js") "console.log('nested');"
        writeFile (assetDirectory <> "/.env") "SECRET=true"
        writeFile (assetDirectory <> "/.hidden/app.js") "console.log('hidden');"
        nestedResponse <- performWaiRequest (toWaiApplication staticApplication) (waiRequest ["assets", "scripts", "app.js"])
        Wai.responseStatus nestedResponse `shouldBe` Http.status200
        readResponseBody nestedResponse `shouldReturn` "console.log('nested');"
        hiddenFileResponse <- performWaiRequest (toWaiApplication staticApplication) (waiRequest ["assets", ".env"])
        Wai.responseStatus hiddenFileResponse `shouldBe` Http.status404
        readResponseBody hiddenFileResponse `shouldReturn` "Not Found"
        hiddenDirectoryResponse <- performWaiRequest (toWaiApplication staticApplication) (waiRequest ["assets", ".hidden", "app.js"])
        Wai.responseStatus hiddenDirectoryResponse `shouldBe` Http.status404
        readResponseBody hiddenDirectoryResponse `shouldReturn` "Not Found"

    it "uses the most specific matching static root when multiple prefixes overlap" $
      withSystemTempDirectory "harch-web-static-overlap" $ \tempDirectory -> do
        let publicDirectory = tempDirectory <> "/public"
            adminDirectory = tempDirectory <> "/admin"
            assetConfig =
              StaticAssetsConfig
                { staticAssetRoots =
                    [ StaticAssetRoot {staticUrlPrefix = "/assets", staticDirectory = publicDirectory},
                      StaticAssetRoot {staticUrlPrefix = "/assets/admin", staticDirectory = adminDirectory}
                    ],
                  staticAssetContentTypes = defaultStaticAssetContentTypes,
                  staticCacheControlSeconds = Nothing
                }
            staticApplication = sampleApplicationWithStaticAssets assetConfig
        createDirectoryIfMissing True (publicDirectory <> "/admin")
        createDirectoryIfMissing True adminDirectory
        writeFile (publicDirectory <> "/admin/panel.js") "console.log('general');"
        writeFile (adminDirectory <> "/panel.js") "console.log('admin');"
        response <- performWaiRequest (toWaiApplication staticApplication) (waiRequest ["assets", "admin", "panel.js"])
        Wai.responseStatus response `shouldBe` Http.status200
        readResponseBody response `shouldReturn` "console.log('admin');"

    it "keeps missing static assets as 404 while rejecting malformed route targets before asset matching" $
      withSystemTempDirectory "harch-web-static-missing" $ \tempDirectory -> do
        let assetConfig =
              StaticAssetsConfig
                { staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = "/assets", staticDirectory = tempDirectory <> "/public"}],
                  staticAssetContentTypes = defaultStaticAssetContentTypes,
                  staticCacheControlSeconds = Nothing
                }
            staticApplication = sampleApplicationWithStaticAssets assetConfig
        missingResponse <- performWaiRequest (toWaiApplication staticApplication) (waiRequest ["assets", "missing.js"])
        Wai.responseStatus missingResponse `shouldBe` Http.status404
        lookup Http.hContentType (Wai.responseHeaders missingResponse) `shouldBe` Just (TextEncoding.encodeUtf8 "text/plain; charset=utf-8")
        readResponseBody missingResponse `shouldReturn` "Not Found"
        invalidResponse <- performWaiRequest (toWaiApplication staticApplication) (waiRequest ["assets", "..", "secret.txt"])
        Wai.responseStatus invalidResponse `shouldBe` Http.status400
        readResponseBody invalidResponse `shouldReturn` "Request target was rejected."
        malformedDynamicResponse <-
          performWaiRequest
            (toWaiApplication sampleApplication)
            ((waiRequest []) {Wai.rawPathInfo = "/known%2Fextra"})
        Wai.responseStatus malformedDynamicResponse `shouldBe` Http.status400
        readResponseBody malformedDynamicResponse `shouldReturn` "Request target was rejected."
        rootResponse <- performWaiRequest (toWaiApplication staticApplication) (waiRequest ["assets"])
        Wai.responseStatus rootResponse `shouldBe` Http.status404
        readResponseBody rootResponse `shouldReturn` "Not Found"

    it "rejects a route codec's malformed result after structural request-target decoding" $ do
      let malformedCodecApplication =
            sampleApplication
              { HarchWeb.routeCodec =
                  (HarchWeb.routeCodec sampleApplication)
                    { HarchWeb.parseRoute = \_ _ -> HarchWeb.RouteMalformed HarchWeb.InvalidRouteTargetEncoding
                    }
              }
      malformedResponse <- performWaiRequest (toWaiApplication malformedCodecApplication) (waiRequest ["known"])
      Wai.responseStatus malformedResponse `shouldBe` Http.status400
      lookup Http.hContentType (Wai.responseHeaders malformedResponse) `shouldBe` Just "text/plain; charset=utf-8"
      readResponseBody malformedResponse `shouldReturn` "Request target was rejected."

    it "rejects an asset path reconstructed as an absolute filesystem path" $
      withSystemTempDirectory "harch-web-static-absolute-escape" $ \tempDirectory -> do
        let publicDirectory = tempDirectory <> "/public"
            secretFilePath = tempDirectory <> "/outside-secret.txt"
            assetConfig =
              StaticAssetsConfig
                { staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = "/assets", staticDirectory = publicDirectory}],
                  staticAssetContentTypes = defaultStaticAssetContentTypes,
                  staticCacheControlSeconds = Nothing
                }
            staticApplication = sampleApplicationWithStaticAssets assetConfig
            -- Concatenating the route prefix with an absolute filesystem
            -- path is exactly the doubled `/` a reverse proxy or a client
            -- can send. `System.FilePath.(</>)` previously discarded the
            -- configured root for such a path and resolved this literal
            -- absolute path instead, so this must reach neither the file
            -- nor its contents.
            escapeRequestPath = Text.pack ("/assets" <> secretFilePath)
            escapeRequest = (waiRequest []) {Wai.rawPathInfo = TextEncoding.encodeUtf8 escapeRequestPath}
        createDirectoryIfMissing True publicDirectory
        writeFile secretFilePath "SECRET=true"
        escapeResponse <- performWaiRequest (toWaiApplication staticApplication) escapeRequest
        Wai.responseStatus escapeResponse `shouldBe` Http.status404
        readResponseBody escapeResponse `shouldReturn` "Not Found"

    it "rejects a symlink inside the static root that points outside it" $
      withSystemTempDirectory "harch-web-static-symlink-escape" $ \tempDirectory -> do
        let publicDirectory = tempDirectory <> "/public"
            assetConfig =
              StaticAssetsConfig
                { staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = "/assets", staticDirectory = publicDirectory}],
                  staticAssetContentTypes = defaultStaticAssetContentTypes,
                  staticCacheControlSeconds = Nothing
                }
            staticApplication = sampleApplicationWithStaticAssets assetConfig
        createDirectoryIfMissing True publicDirectory
        writeFile (tempDirectory <> "/outside-secret.txt") "SECRET=true"
        createFileLink (tempDirectory <> "/outside-secret.txt") (publicDirectory <> "/linked.txt")
        symlinkResponse <- performWaiRequest (toWaiApplication staticApplication) (waiRequest ["assets", "linked.txt"])
        Wai.responseStatus symlinkResponse `shouldBe` Http.status404
        readResponseBody symlinkResponse `shouldReturn` "Not Found"

    it "serves static files with validators, byte ranges, and HEAD metadata" $
      withSystemTempDirectory "harch-web-static-validators" $ \tempDirectory -> do
        let assetDirectory = tempDirectory <> "/public"
            assetConfig =
              StaticAssetsConfig
                { staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = "/assets", staticDirectory = assetDirectory}],
                  staticAssetContentTypes = defaultStaticAssetContentTypes,
                  staticCacheControlSeconds = Just 60
                }
            staticApplication = sampleApplicationWithStaticAssets assetConfig
            assetRequest = waiRequest ["assets", "alphabet.js"]
        createDirectoryIfMissing True assetDirectory
        writeFile (assetDirectory <> "/alphabet.js") "abcdefghij"
        fullResponse <- performWaiRequest (toWaiApplication staticApplication) assetRequest
        let responseHeaders = Wai.responseHeaders fullResponse
            assetETag = fromMaybe (error "expected ETag") (lookup Http.hETag responseHeaders)
            assetLastModified = fromMaybe (error "expected Last-Modified") (lookup Http.hLastModified responseHeaders)
        Wai.responseStatus fullResponse `shouldBe` Http.status200
        lookup Http.hAcceptRanges responseHeaders `shouldBe` Just "bytes"
        lookup Http.hCacheControl responseHeaders `shouldBe` Just "public, max-age=60"
        lookup Http.hContentLength responseHeaders `shouldBe` Just "10"
        readResponseBody fullResponse `shouldReturn` "abcdefghij"
        matchingETagResponse <-
          performWaiRequest
            (toWaiApplication staticApplication)
            (assetRequest {Wai.requestHeaders = [(Http.hIfNoneMatch, assetETag)]})
        Wai.responseStatus matchingETagResponse `shouldBe` Http.status304
        lookup Http.hContentType (Wai.responseHeaders matchingETagResponse) `shouldBe` Just "application/javascript; charset=utf-8"
        lookup Http.hCacheControl (Wai.responseHeaders matchingETagResponse) `shouldBe` Just "public, max-age=60"
        lookup Http.hETag (Wai.responseHeaders matchingETagResponse) `shouldBe` Just assetETag
        readResponseBody matchingETagResponse `shouldReturn` ""
        strongETagResponse <-
          performWaiRequest
            (toWaiApplication staticApplication)
            (assetRequest {Wai.requestHeaders = [(Http.hIfNoneMatch, ByteString.drop 2 assetETag)]})
        Wai.responseStatus strongETagResponse `shouldBe` Http.status304
        matchingDateResponse <-
          performWaiRequest
            (toWaiApplication staticApplication)
            (assetRequest {Wai.requestHeaders = [(Http.hIfModifiedSince, assetLastModified)]})
        Wai.responseStatus matchingDateResponse `shouldBe` Http.status304
        nonMatchingETagResponse <-
          performWaiRequest
            (toWaiApplication staticApplication)
            (assetRequest {Wai.requestHeaders = [(Http.hIfNoneMatch, "W/\"other\""), (Http.hIfModifiedSince, assetLastModified)]})
        Wai.responseStatus nonMatchingETagResponse `shouldBe` Http.status200
        readResponseBody nonMatchingETagResponse `shouldReturn` "abcdefghij"
        rangeResponse <-
          performWaiRequest
            (toWaiApplication staticApplication)
            (assetRequest {Wai.requestHeaders = [(Http.hRange, "bytes=2-5")]})
        Wai.responseStatus rangeResponse `shouldBe` Http.status206
        lookup Http.hContentType (Wai.responseHeaders rangeResponse) `shouldBe` Just "application/javascript; charset=utf-8"
        lookup Http.hCacheControl (Wai.responseHeaders rangeResponse) `shouldBe` Just "public, max-age=60"
        lookup Http.hETag (Wai.responseHeaders rangeResponse) `shouldBe` Just assetETag
        lookup Http.hContentRange (Wai.responseHeaders rangeResponse) `shouldBe` Just "bytes 2-5/10"
        lookup Http.hContentLength (Wai.responseHeaders rangeResponse) `shouldBe` Just "4"
        readResponseBody rangeResponse `shouldReturn` "cdef"
        suffixRangeResponse <-
          performWaiRequest
            (toWaiApplication staticApplication)
            (assetRequest {Wai.requestHeaders = [(Http.hRange, "bytes=-3")]})
        Wai.responseStatus suffixRangeResponse `shouldBe` Http.status206
        lookup Http.hContentRange (Wai.responseHeaders suffixRangeResponse) `shouldBe` Just "bytes 7-9/10"
        readResponseBody suffixRangeResponse `shouldReturn` "hij"
        openRangeResponse <-
          performWaiRequest
            (toWaiApplication staticApplication)
            (assetRequest {Wai.requestHeaders = [(Http.hRange, "bytes=7-")]})
        Wai.responseStatus openRangeResponse `shouldBe` Http.status206
        lookup Http.hContentRange (Wai.responseHeaders openRangeResponse) `shouldBe` Just "bytes 7-9/10"
        readResponseBody openRangeResponse `shouldReturn` "hij"
        unsatisfiableRangeResponse <-
          performWaiRequest
            (toWaiApplication staticApplication)
            (assetRequest {Wai.requestHeaders = [(Http.hRange, "bytes=10-")]})
        Wai.responseStatus unsatisfiableRangeResponse `shouldBe` Http.status416
        lookup Http.hContentType (Wai.responseHeaders unsatisfiableRangeResponse) `shouldBe` Just "application/javascript; charset=utf-8"
        lookup Http.hCacheControl (Wai.responseHeaders unsatisfiableRangeResponse) `shouldBe` Just "public, max-age=60"
        lookup Http.hETag (Wai.responseHeaders unsatisfiableRangeResponse) `shouldBe` Just assetETag
        lookup Http.hContentRange (Wai.responseHeaders unsatisfiableRangeResponse) `shouldBe` Just "bytes */10"
        lookup Http.hContentLength (Wai.responseHeaders unsatisfiableRangeResponse) `shouldBe` Just "0"
        readResponseBody unsatisfiableRangeResponse `shouldReturn` ""
        forM_ ["bytes=0-1,2-3", "bytes=1", "bytes=-0", "bytes=5-3", "bytes=-", "bytes=x-y"] $ \invalidRange -> do
          invalidRangeResponse <-
            performWaiRequest
              (toWaiApplication staticApplication)
              (assetRequest {Wai.requestHeaders = [(Http.hRange, invalidRange)]})
          Wai.responseStatus invalidRangeResponse `shouldBe` Http.status416
        headResponse <-
          performWaiRequest
            (toWaiApplication staticApplication)
            (assetRequest {Wai.requestMethod = "HEAD"})
        Wai.responseStatus headResponse `shouldBe` Http.status200
        lookup Http.hContentLength (Wai.responseHeaders headResponse) `shouldBe` Just "10"
        readResponseBody headResponse `shouldReturn` ""

    it "does not cache missing static asset responses when configured" $
      withSystemTempDirectory "harch-web-static-missing-cache" $ \tempDirectory -> do
        let assetConfig =
              StaticAssetsConfig
                { staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = "/assets", staticDirectory = tempDirectory <> "/public"}],
                  staticAssetContentTypes = defaultStaticAssetContentTypes,
                  staticCacheControlSeconds = Just 60
                }
            staticApplication = sampleApplicationWithStaticAssets assetConfig
        missingResponse <- performWaiRequest (toWaiApplication staticApplication) (waiRequest ["assets", "missing.js"])
        lookup Http.hCacheControl (Wai.responseHeaders missingResponse) `shouldBe` Nothing
        invalidResponse <- performWaiRequest (toWaiApplication staticApplication) (waiRequest ["assets", "..", "secret.txt"])
        lookup Http.hCacheControl (Wai.responseHeaders invalidResponse) `shouldBe` Nothing
        rootResponse <- performWaiRequest (toWaiApplication staticApplication) (waiRequest ["assets"])
        lookup Http.hCacheControl (Wai.responseHeaders rootResponse) `shouldBe` Nothing
        unsupportedExtensionResponse <- performWaiRequest (toWaiApplication staticApplication) (waiRequest ["assets", "secret.bin"])
        lookup Http.hCacheControl (Wai.responseHeaders unsupportedExtensionResponse) `shouldBe` Nothing

-- | Preserve the ordinary structured-log expectation while proving that its
-- dynamic framework prefix is a canonical opaque identifier.
stripRequestIdLogPrefix :: Text -> Text
stripRequestIdLogPrefix message =
  case Text.stripPrefix "request.id=" message of
    Nothing -> message
    Just identifierAndMessage ->
      let (identifierText, messageWithSeparator) = Text.breakOn " " identifierAndMessage
       in case HarchWeb.mkRequestId identifierText of
            Nothing -> message
            Just _ -> Text.drop 1 messageWithSeparator

responseHasCanonicalRequestId :: Http.ResponseHeaders -> Bool
responseHasCanonicalRequestId responseHeaders =
  case lookup "X-Request-ID" responseHeaders of
    Nothing -> False
    Just headerValue ->
      isJust (HarchWeb.mkRequestId (TextEncoding.decodeUtf8 headerValue))

protectedEndpointMetadata :: HarchWeb.EndpointMetadata ()
protectedEndpointMetadata =
  HarchWeb.mkEndpointMetadata
    (requiredEndpointName "test.protected")
    (requiredRouteTemplate "/known")
    HarchWeb.HtmlEndpoint
    HarchWeb.RequireAuthenticated

securityEventUnavailableResponse :: ResponseBody
securityEventUnavailableResponse =
  ResponseBody
    { responseStatus = Http.status503,
      responseContentType = "text/plain; charset=utf-8",
      responseBody = "Security-event sink unavailable.",
      responseObservabilityAttributes = [],
      responseLogEntries = [],
      responseDatabaseOperations = []
    }

requiredEndpointName :: Text -> HarchWeb.EndpointName
requiredEndpointName value =
  case HarchWeb.mkEndpointName value of
    Right endpointName -> endpointName
    Left metadataError -> error ("invalid endpoint-name test literal: " <> show metadataError)

requiredRouteTemplate :: Text -> HarchWeb.RouteTemplate
requiredRouteTemplate value =
  case HarchWeb.mkRouteTemplate value of
    Right routeTemplate -> routeTemplate
    Left metadataError -> error ("invalid route-template test literal: " <> show metadataError)

requiredModuleName :: Text -> HarchWeb.ModuleName
requiredModuleName value =
  case HarchWeb.mkModuleName value of
    Right moduleName -> moduleName
    Left moduleNameError -> error ("invalid module-name test literal: " <> show moduleNameError)
