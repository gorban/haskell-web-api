{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Exception (ErrorCall, try)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LazyByteString
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb (ClientActionPayload (..), ClientActionRequest (..), HtmlAttribute (..), NonPageResponse (..), Page (..), PageShell (..), Response (..), ResponseBody (..), RouteCodec (..), RouteRequest (..), toWaiApplication)
import HarchWeb qualified
import HarchWeb.Markup.Unsafe qualified as MarkupUnsafe
import HarchWeb.Observability qualified as Observability
import HarchWeb.Site (RouteDefinition (..), SimpleSiteConfiguration (..), Site (..), apiOnlySite, buildSiteApplication, simpleSite)
import HarchWeb.Site qualified as Site
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import Network.Wai.Internal qualified as WaiInternal

data SampleRoute
  = HomeRoute
  | SecondRoute
  | StatusApiRoute
  | NotFoundRoute
  deriving (Eq, Show)

newtype SampleContext = SampleContext
  { pathPrefix :: Text
  }
  deriving (Eq, Show)

type SampleAuthorization = ()

siteRequestId :: HarchWeb.RequestId
siteRequestId =
  fromMaybe (error "invalid site test request identifier") (HarchWeb.mkRequestId "550e8400-e29b-41d4-a716-446655440000")

siteTestCsrfToken :: HarchWeb.CsrfToken
siteTestCsrfToken = fromMaybe (error "invalid site test CSRF token") (HarchWeb.mkCsrfToken "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")

siteTestCsrfProtection :: HarchWeb.CsrfProtection SampleContext
siteTestCsrfProtection =
  HarchWeb.CsrfProtection
    { HarchWeb.issueCsrfToken = const (pure (HarchWeb.CsrfTokenIssued siteTestCsrfToken HarchWeb.defaultCsrfCookieMaxAgeSeconds)),
      HarchWeb.verifyCsrfToken = \_ _ -> pure HarchWeb.CsrfVerified
    }

routeLocationForTest :: Text -> HarchWeb.RouteLocation
routeLocationForTest target =
  case HarchWeb.decodeRouteLocation (HarchWeb.requestTarget (TextEncoding.encodeUtf8 path) (TextEncoding.encodeUtf8 query)) of
    Left routeError -> error ("invalid test route target: " <> show routeError)
    Right location -> location
  where
    (path, query) = Text.breakOn "?" target

sampleSecurity :: HarchWeb.ApplicationSecurity SampleRoute SampleContext SampleAuthorization
sampleSecurity = HarchWeb.AuthenticationDisabled []

sampleMetadata :: HarchWeb.EndpointProtocol -> SampleRoute -> HarchWeb.EndpointMetadata SampleAuthorization
sampleMetadata endpointProtocolValue route =
  HarchWeb.mkEndpointMetadata
    (requiredSampleEndpointName route)
    (requiredSampleRouteTemplate route)
    endpointProtocolValue
    HarchWeb.AllowUnauthenticated

requiredSampleEndpointName :: SampleRoute -> HarchWeb.EndpointName
requiredSampleEndpointName route =
  case HarchWeb.mkEndpointName ("sample." <> Text.toLower (Text.pack (show route))) of
    Right endpointName -> endpointName
    Left metadataError -> error ("invalid sample endpoint name: " <> show metadataError)

requiredSampleRouteTemplate :: SampleRoute -> HarchWeb.RouteTemplate
requiredSampleRouteTemplate route =
  case HarchWeb.mkRouteTemplate ("/{sample}/" <> Text.toLower (Text.pack (show route))) of
    Right routeTemplate -> routeTemplate
    Left metadataError -> error ("invalid sample route template: " <> show metadataError)

spec = do
  describe "buildSiteApplication" $ do
    it "refuses direct page-route invocation before page security is prepared" $ do
      result <- try (Site.routeResponse homeRouteDefinition Wai.defaultRequest (RouteRequest HomeRoute (SampleContext ""))) :: IO (Either ErrorCall (Response SampleRoute SampleContext))
      result `shouldSatisfy` either (("pre-render PageSecurity" `Text.isInfixOf`) . Text.pack . show) (const False)

    it "keeps the simpleSite defaults available when the composition root does not override them" $ do
      let siteApplication = buildSiteApplication sampleSite
          requestObservability =
            Observability.buildRequestObservability
              Observability.RequestIdentity
                { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "GET",
                  Observability.requestIdentityScheme = "http",
                  Observability.requestIdentityPath = "/",
                  Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/"
                }
              200
              Observability.PageResponseKind
              []
          connectionObservability =
            Observability.buildConnectionObservability
              "CONNECTION sample"
              []
      expectAll
        ( (siteName sampleSite `shouldBe` "sample")
            :| [ HarchWeb.appName siteApplication `shouldBe` "sample",
                 HarchWeb.staticAssetRoots (siteStaticAssets sampleSite) `shouldBe` [],
                 HarchWeb.staticAssetContentTypes (siteStaticAssets sampleSite) `shouldBe` HarchWeb.defaultStaticAssetContentTypes,
                 HarchWeb.staticCacheControlSeconds (siteStaticAssets sampleSite) `shouldBe` Nothing,
                 siteRuntimeAssets sampleSite `shouldBe` [],
                 fmap HarchWeb.navigationRuntimePath (siteNavigationRuntime sampleSite) `shouldBe` Just "/assets/navigation.js",
                 siteNavigationRuntimePathPrefix sampleSite (SampleContext "/app") `shouldBe` HarchWeb.emptyPathPrefix,
                 HarchWeb.httpsRedirectPort (siteRequestPolicy sampleSite) `shouldBe` Nothing,
                 HarchWeb.httpsRedirectAuthority (siteRequestPolicy sampleSite) `shouldBe` Nothing,
                 HarchWeb.requestTransportLimits (siteRequestPolicy sampleSite) `shouldBe` HarchWeb.warpDefaultRequestTransportLimits,
                 HarchWeb.requestConcurrencyLimit (siteRequestPolicy sampleSite) `shouldBe` Nothing,
                 HarchWeb.corsPolicy (siteRequestPolicy sampleSite) `shouldBe` HarchWeb.defaultCorsPolicyConfig,
                 length (siteRequestMiddleware sampleSite) `shouldBe` 0
               ]
        )
      siteRequestContextFromRequest sampleSite (waiRequest ["second"]) siteRequestId (SampleContext "/app") `shouldBe` SampleContext "/app"
      siteHandleClientAction
        sampleSite
        ClientActionRequest
          { clientAction = (),
            clientActionRequestIdempotencyKey = Nothing,
            clientActionContext = SampleContext ""
          }
        `shouldReturn` Nothing
      let actionPayload =
            ClientActionPayload
              { clientActionMethod = "POST",
                clientActionPath = "/actions/sample",
                clientActionFields = [],
                clientActionCsrfToken = Nothing,
                clientActionIdempotencyKey = Nothing,
                clientActionPayloadContext = SampleContext ""
              }
      siteDecodeClientAction sampleSite actionPayload `shouldBe` HarchWeb.UnrecognizedClientAction
      HarchWeb.decodeClientAction siteApplication actionPayload `shouldBe` HarchWeb.UnrecognizedClientAction
      case siteSecurityEventRoot sampleSite of
        Nothing -> pure ()
        Just _ -> expectationFailure "expected simpleSite to have no security-event root by default"
      case HarchWeb.applicationSecurityEventRoot siteApplication of
        Nothing -> pure ()
        Just _ -> expectationFailure "expected built simpleSite application to have no security-event root by default"
      case siteRouteModuleChain sampleSite of
        Nothing -> pure ()
        Just _ -> expectationFailure "expected simpleSite to have no route-module chain by default"
      case HarchWeb.applicationRouteModuleChain siteApplication of
        Nothing -> pure ()
        Just _ -> expectationFailure "expected built simpleSite application to have no route-module chain by default"
      siteClientActionEndpointMetadata sampleSite "POST" "/actions/sample" (SampleContext "") `shouldBe` Nothing
      HarchWeb.clientActionEndpointMetadata siteApplication "POST" "/actions/sample" (SampleContext "") `shouldBe` Nothing
      HarchWeb.clientActionRoute siteApplication "POST" "/actions/sample" (SampleContext "") `shouldBe` Nothing
      HarchWeb.handleClientAction
        siteApplication
        ClientActionRequest
          { clientAction = (),
            clientActionRequestIdempotencyKey = Nothing,
            clientActionContext = SampleContext ""
          }
        `shouldReturn` Nothing
      HarchWeb.verifyCsrfToken (siteCsrfProtection sampleSite) (SampleContext "") siteTestCsrfToken `shouldReturn` HarchWeb.CsrfVerified
      HarchWeb.verifyCsrfToken (HarchWeb.csrfProtection siteApplication) (SampleContext "") siteTestCsrfToken `shouldReturn` HarchWeb.CsrfVerified
      length (HarchWeb.applicationRequestMiddleware siteApplication) `shouldBe` 0
      siteReportRequestObservability sampleSite requestObservability `shouldReturn` ()
      siteReportConnectionObservability sampleSite connectionObservability `shouldReturn` ()
      siteReportApplicationLog sampleSite "sample-log" `shouldReturn` ()
      HarchWeb.reportRequestObservability siteApplication requestObservability `shouldReturn` ()
      HarchWeb.reportConnectionObservability siteApplication connectionObservability `shouldReturn` ()
      HarchWeb.reportApplicationLog siteApplication "sample-log" `shouldReturn` ()

    it "gives API-only sites a complete SSR fallback without page navigation runtime" $ do
      let apiSite = apiOnlySite "sample-api" (SampleContext "") sampleRouteCodec sampleSecurity sampleRouteDefinition
          apiApplication = buildSiteApplication apiSite
          fallbackPage =
            Page
              { pageTitle = "Fallback",
                pageRoute = HomeRoute,
                pageContext = SampleContext "",
                pageBody = HarchWeb.text "fallback",
                pageBootstrapHooks = []
              }
          document = HarchWeb.pageShell apiApplication fallbackPage
      expectAll
        ( (siteName apiSite `shouldBe` "sample-api")
            :| [ siteDefaultRequestContext apiSite `shouldBe` SampleContext "",
                 siteNavigationRuntime apiSite `shouldBe` Nothing,
                 siteNavigationRoutes apiSite `shouldBe` [],
                 HarchWeb.appName apiApplication `shouldBe` "sample-api",
                 HarchWeb.defaultRequestContext apiApplication `shouldBe` SampleContext "",
                 parseRoute (HarchWeb.routeCodec apiApplication) (SampleContext "") (routeLocationForTest "/api/status")
                   `shouldBe` HarchWeb.RouteParsed (RouteRequest StatusApiRoute (SampleContext "")),
                 HarchWeb.renderDocumentForTests document
                   `shouldBe` "<!DOCTYPE html><html><head><title>Fallback</title></head><body><nav data-navigation-region=\"primary\"></nav><main id=\"main\" data-navigation-content=\"true\">fallback</main></body></html>"
               ]
        )
      HarchWeb.renderResponse apiApplication (RouteRequest StatusApiRoute (SampleContext "")) >>= \case
        BodyResponse response -> HarchWeb.responseBody response `shouldBe` "{\"status\":\"ok\"}"
        _ -> expectationFailure "expected API-only site to render its protocol route"
      HarchWeb.issueCsrfToken (siteCsrfProtection apiSite) (SampleContext "") `shouldReturn` HarchWeb.CsrfProtectionUnavailable
      waiResponse <- performWaiRequest (toWaiApplication apiApplication) (waiRequest ["api", "status"])
      Wai.responseStatus waiResponse `shouldBe` Http.status200

    it "passes prepared page security and the parsed route request to a page declaration" $ do
      seenPageSecurity <- newIORef Nothing
      seenRouteContext <- newIORef Nothing
      seenCsrfContext <- newIORef Nothing
      let contextRecordingProtection =
            HarchWeb.CsrfProtection
              { HarchWeb.issueCsrfToken = \requestContextValue -> do
                  writeIORef seenCsrfContext (Just requestContextValue)
                  HarchWeb.issueCsrfToken siteTestCsrfProtection requestContextValue,
                HarchWeb.verifyCsrfToken = HarchWeb.verifyCsrfToken siteTestCsrfProtection
              }
          securityAwareSite =
            sampleSite
              { siteCsrfProtection = contextRecordingProtection,
                siteRouteDefinition = \case
                  HomeRoute ->
                    Site.pageRoute (sampleMetadata HarchWeb.HtmlEndpoint HomeRoute) (Just "Home") $ \pageSecurity routeRequest -> do
                      writeIORef seenPageSecurity (Just (HarchWeb.runtimeNonceValue (HarchWeb.pageSecurityRuntimeNonce pageSecurity)))
                      writeIORef seenRouteContext (Just (HarchWeb.requestContext routeRequest))
                      pure
                        Page
                          { pageTitle = "Security-aware home",
                            pageRoute = HomeRoute,
                            pageContext = HarchWeb.requestContext routeRequest,
                            pageBody = HarchWeb.text "home",
                            pageBootstrapHooks = []
                          }
                  otherRoute -> sampleRouteDefinition otherRoute
              }
      response <- performWaiRequest (toWaiApplication (buildSiteApplication securityAwareSite)) (waiRequest [])
      expectAll
        ( (Wai.responseStatus response `shouldBe` Http.status200)
            :| [ readIORef seenPageSecurity >>= (`shouldSatisfy` maybe False (not . Text.null)),
                 readIORef seenRouteContext `shouldReturn` Just (SampleContext ""),
                 readIORef seenCsrfContext `shouldReturn` Just (SampleContext "")
               ]
        )

    it "derives navigation items from labeled site routes and keeps route rendering prefix-aware" $ do
      let siteApplication = buildSiteApplication sampleSite
          homeRequest = RouteRequest {requestRoute = HomeRoute, requestContext = SampleContext "/app"}
      response <- HarchWeb.renderResponse siteApplication homeRequest
      case response of
        PageResponse _ page -> do
          let document = HarchWeb.pageShell siteApplication page
          runtimeNonce <- HarchWeb.generateRuntimeNonce
          HarchWeb.documentRuntimeDescriptors document
            `shouldBe` [HarchWeb.defaultCaptureKernel, HarchWeb.DeferredModule "harch-navigation" "/assets/navigation.js"]
          Text.isInfixOf ("<script nonce=\"" <> HarchWeb.runtimeNonceValue runtimeNonce <> "\">") (HarchWeb.renderDocumentWithNonce runtimeNonce document)
            `shouldBe` True
        PageResponseWithMetadata {} ->
          expectationFailure "expected pageRoute to render a plain page response"
        BodyResponse _ ->
          expectationFailure "expected a page response for the home route"
        RedirectResponse _ _ ->
          expectationFailure "expected a page response for the home route"
        InternalRedirectResponse _ _ ->
          expectationFailure "expected a page response for the home route"
        InternalRedirectResponseWithHeaders {} ->
          expectationFailure "expected a page response for the home route"
        EventStreamResponse _ _ ->
          expectationFailure "expected a page response for the home route"
        ClientActionBodyResponse _ ->
          expectationFailure "expected a page response for the home route"
        ProtocolResponseResult _ ->
          expectationFailure "expected a page response for the home route"

    it "renders the configured not-found page through the shared shell with a 404 status" $ do
      response <- performWaiRequest (toWaiApplication (buildSiteApplication sampleSite)) (waiRequest ["missing"])
      Wai.responseStatus response `shouldBe` Http.status404
      responseBody <- readResponseBody response
      Text.isInfixOf "<h1>Not Found</h1>" responseBody `shouldBe` True
      Text.isInfixOf "<script nonce=\"" responseBody `shouldBe` True

    it "retains one verified CSRF cookie across complete page GETs" $ do
      let application = buildSiteApplication sampleSite
          csrfCookie = "__Host-harch-csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          requestWithCsrfCookie =
            (waiRequest [])
              { Wai.requestHeaders = [(Http.hCookie, csrfCookie)]
              }
      firstResponse <- performWaiRequest (toWaiApplication application) (waiRequest [])
      retainedResponse <- performWaiRequest (toWaiApplication application) requestWithCsrfCookie
      retainedBody <- readResponseBody retainedResponse
      expectAll
        ( (lookup "Set-Cookie" (Wai.responseHeaders firstResponse) `shouldBe` Just "__Host-harch-csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA; Path=/; Max-Age=3600; Secure; HttpOnly; SameSite=Strict")
            :| [ lookup "Set-Cookie" (Wai.responseHeaders retainedResponse) `shouldBe` Nothing,
                 Text.isInfixOf "data-harch-csrf-token=\"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\"" retainedBody `shouldBe` True
               ]
        )

    it "returns a safe 503 instead of rendering a page when page CSRF issuance is unavailable" $ do
      let unavailableSite = sampleSite {siteCsrfProtection = HarchWeb.csrfProtectionUnavailable}
      response <- performWaiRequest (toWaiApplication (buildSiteApplication unavailableSite)) (waiRequest [])
      expectAll
        ( (Wai.responseStatus response `shouldBe` Http.status503)
            :| [ lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just "text/plain; charset=utf-8",
                 readResponseBody response `shouldReturn` "CSRF protection is unavailable."
               ]
        )

    it "passes page security and response metadata through the typed page route boundary" $ do
      let metadataResponse =
            ResponseBody
              { responseStatus = Http.status202,
                responseContentType = "text/plain; charset=utf-8",
                responseBody = "accepted",
                responseObservabilityAttributes = [],
                responseLogEntries = [],
                responseDatabaseOperations = []
              }
          metadataRoute =
            homeRouteDefinition
              { routeHandler =
                  Site.PageRouteHandler $ \pageSecurity routeRequest -> do
                    HarchWeb.pageCsrfValue (HarchWeb.pageSecurityCsrf pageSecurity) `shouldBe` siteTestCsrfToken
                    requestContext routeRequest `shouldBe` SampleContext "/context"
                    pure
                      ( HarchWeb.RenderedPageWithMetadata
                          metadataResponse
                          Page
                            { pageTitle = "Metadata",
                              pageRoute = HomeRoute,
                              pageContext = requestContext routeRequest,
                              pageBody = HarchWeb.text "metadata",
                              pageBootstrapHooks = []
                            }
                      )
              }
          siteWithMetadata = sampleSite {siteRouteDefinition = \case HomeRoute -> metadataRoute; route -> sampleRouteDefinition route}
      HarchWeb.renderResponse (buildSiteApplication siteWithMetadata) (RouteRequest HomeRoute (SampleContext "/context")) >>= \case
        PageResponseWithMetadata pageSecurity responseBodyValue page ->
          expectAll
            ( (HarchWeb.pageCsrfValue (HarchWeb.pageSecurityCsrf pageSecurity) `shouldBe` siteTestCsrfToken)
                :| [ responseBodyValue `shouldBe` metadataResponse,
                     pageTitle page `shouldBe` "Metadata"
                   ]
            )
        _ -> expectationFailure "expected page response metadata to remain distinct from its SSR page"

    it "takes method ownership from route definitions rather than a codec shadow table" $ do
      let conflictingCodec =
            (siteRouteCodec sampleSite)
              { HarchWeb.routeMethods = const (HarchWeb.routeMethodPolicy [HarchWeb.RouteDelete])
              }
          application = buildSiteApplication (sampleSite {siteRouteCodec = conflictingCodec})
          deleteHomeRequest = (waiRequest []) {Wai.requestMethod = "DELETE"}
      getResponse <- performWaiRequest (toWaiApplication application) (waiRequest [])
      deleteResponse <- performWaiRequest (toWaiApplication application) deleteHomeRequest
      expectAll
        ( (Wai.responseStatus getResponse `shouldBe` Http.status200)
            :| [ Wai.responseStatus deleteResponse `shouldBe` Http.status405,
                 lookup Http.hAllow (Wai.responseHeaders deleteResponse) `shouldBe` Just "GET, HEAD, OPTIONS"
               ]
        )

    it "carries each route's execution policy through the shared dispatcher" $ do
      let boundedStatusRoute =
            apiRouteDefinition
              { routeExecutionPolicy = HarchWeb.RouteExecutionPolicy (HarchWeb.mkRequestConcurrencyLimit 1)
              }
          siteWithBoundedStatusRoute =
            sampleSite
              { siteRouteDefinition = \case
                  StatusApiRoute -> boundedStatusRoute
                  route -> sampleRouteDefinition route
              }
          siteApplication = buildSiteApplication siteWithBoundedStatusRoute
      expectAll
        ( ( HarchWeb.routeExecutionPolicy siteApplication StatusApiRoute
              `shouldBe` HarchWeb.RouteExecutionPolicy (HarchWeb.mkRequestConcurrencyLimit 1)
          )
            :| [ HarchWeb.routeExecutionPolicy siteApplication HomeRoute
                   `shouldBe` HarchWeb.unboundedRouteExecutionPolicy
               ]
        )

    it "binds each full HTML response to a fresh CSP nonce before body controls parse" $ do
      let application = buildSiteApplication sampleSite
      firstResponse <- performWaiRequest (toWaiApplication application) (waiRequest [])
      secondResponse <- performWaiRequest (toWaiApplication application) (waiRequest [])
      firstBody <- readResponseBody firstResponse
      secondBody <- readResponseBody secondResponse
      let firstPolicy = TextEncoding.decodeUtf8 (fromMaybe "" (lookup "Content-Security-Policy" (Wai.responseHeaders firstResponse)))
          secondPolicy = TextEncoding.decodeUtf8 (fromMaybe "" (lookup "Content-Security-Policy" (Wai.responseHeaders secondResponse)))
          firstNonce = nonceFromHtml firstBody
          secondNonce = nonceFromHtml secondBody
      firstNonce `shouldSatisfy` (/= Nothing)
      secondNonce `shouldSatisfy` (/= Nothing)
      Text.isInfixOf ("'nonce-" <> fromMaybe "" firstNonce) firstPolicy `shouldBe` True
      Text.isInfixOf ("'nonce-" <> fromMaybe "" secondNonce) secondPolicy `shouldBe` True
      Text.isInfixOf "unsafe-inline" firstPolicy `shouldBe` False
      Text.isInfixOf "unsafe-inline" secondPolicy `shouldBe` False
      firstNonce `shouldNotBe` secondNonce

    it "preserves body responses for unlabeled non-page routes" $ do
      response <- performWaiRequest (toWaiApplication (buildSiteApplication sampleSite)) (waiRequest ["api", "status"])
      Wai.responseStatus response `shouldBe` Http.status200
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just "application/json"
      readResponseBody response `shouldReturn` "{\"status\":\"ok\"}"

    it "passes the selected route's original WAI request and parsed route request to its response declaration" $ do
      seenHeaderReference <- newIORef Nothing
      seenRouteReference <- newIORef Nothing
      let requestAwareSite =
            sampleSite
              { siteRouteDefinition = \case
                  StatusApiRoute ->
                    apiRouteDefinition
                      { routeHandler = Site.ProtocolRouteHandler $ \endpointRequest routeRequest -> do
                          writeIORef seenHeaderReference (lookup "X-Endpoint-Probe" (Wai.requestHeaders endpointRequest))
                          writeIORef seenRouteReference (Just (HarchWeb.requestRoute routeRequest))
                          pure
                            ( NonPageBodyResponse
                                ResponseBody
                                  { responseStatus = Http.status200,
                                    responseContentType = "application/json",
                                    responseBody = "{\"status\":\"ok\"}",
                                    responseObservabilityAttributes = [],
                                    responseLogEntries = [],
                                    responseDatabaseOperations = []
                                  }
                            )
                      }
                  otherRoute -> sampleRouteDefinition otherRoute
              }
          incomingRequest = (waiRequest ["api", "status"]) {Wai.requestHeaders = [("X-Endpoint-Probe", "received")]}
      response <- performWaiRequest (toWaiApplication (buildSiteApplication requestAwareSite)) incomingRequest
      expectAll
        ( (Wai.responseStatus response `shouldBe` Http.status200)
            :| [ readIORef seenHeaderReference `shouldReturn` Just "received",
                 readIORef seenRouteReference `shouldReturn` Just StatusApiRoute
               ]
        )

    it "adds missing navigation shell markers and can disable the built-in runtime" $ do
      let bareShellSite =
            sampleSite
              { siteNavigationRuntime = Nothing,
                sitePageShell =
                  const
                    PageShell
                      { shellBodyAttributes = [],
                        shellNavigationAttributes = [],
                        shellNavigationItems = [],
                        shellMainId = HarchWeb.literalElementId "app-main",
                        shellMainAttributes = [],
                        shellNavigationLifecycle = Nothing,
                        shellStylesheets = [],
                        shellRuntimeDescriptors = []
                      }
              }
          siteApplication = buildSiteApplication bareShellSite
          request = RouteRequest {requestRoute = HomeRoute, requestContext = SampleContext ""}
      PageResponse _ page <- HarchWeb.renderResponse siteApplication request
      HarchWeb.renderDocumentForTests (HarchWeb.pageShell siteApplication page)
        `shouldBe` "<!DOCTYPE html><html><head><title>Home</title></head><body><nav data-navigation-region=\"primary\"><a href=\"/\" data-page-link=\"true\" aria-current=\"page\">Home</a><a href=\"/second\" data-page-link=\"true\">Second</a></nav><main id=\"app-main\" data-navigation-content=\"true\"><h1>Home</h1><p><a href=\"/second\">Browse second</a></p></main></body></html>"

    it "does not duplicate a runtime module already supplied by the app shell" $ do
      let duplicatedRuntimeSite =
            sampleSite
              { sitePageShell =
                  \page ->
                    (samplePageShell page)
                      { shellRuntimeDescriptors = [HarchWeb.DeferredModule "harch-navigation" "/assets/navigation.js"]
                      }
              }
          siteApplication = buildSiteApplication duplicatedRuntimeSite
          request = RouteRequest {requestRoute = HomeRoute, requestContext = SampleContext ""}
      PageResponse _ page <- HarchWeb.renderResponse siteApplication request
      HarchWeb.documentRuntimeDescriptors (HarchWeb.pageShell siteApplication page)
        `shouldBe` [HarchWeb.defaultCaptureKernel, HarchWeb.DeferredModule "harch-navigation" "/assets/navigation.js"]

    it "does not duplicate a capture kernel already supplied by the app shell" $ do
      let duplicatedKernelSite =
            sampleSite
              { sitePageShell =
                  \page ->
                    (samplePageShell page)
                      { shellRuntimeDescriptors = [HarchWeb.defaultCaptureKernel]
                      }
              }
          siteApplication = buildSiteApplication duplicatedKernelSite
          request = RouteRequest {requestRoute = HomeRoute, requestContext = SampleContext ""}
      PageResponse _ page <- HarchWeb.renderResponse siteApplication request
      HarchWeb.documentRuntimeDescriptors (HarchWeb.pageShell siteApplication page)
        `shouldBe` [HarchWeb.defaultCaptureKernel, HarchWeb.DeferredModule "harch-navigation" "/assets/navigation.js"]

    it "renders the framework runtime script source from page context" $ do
      let prefixedRuntimeSite =
            sampleSite
              { siteNavigationRuntimePathPrefix = sampleContextPathPrefix
              }
          siteApplication = buildSiteApplication prefixedRuntimeSite
          request = RouteRequest {requestRoute = HomeRoute, requestContext = SampleContext "/app"}
      PageResponse _ page <- HarchWeb.renderResponse siteApplication request
      HarchWeb.documentRuntimeDescriptors (HarchWeb.pageShell siteApplication page)
        `shouldBe` [HarchWeb.defaultCaptureKernel, HarchWeb.DeferredModule "harch-navigation" "/app/assets/navigation.js"]

    it "serves application-selected runtime adapters through the generic early boundary" $ do
      let firstAsset = HarchWeb.RuntimeAsset "custom-dialog" "/assets/custom-dialog.js" "window.customDialog = 'first';"
          shadowedAsset = HarchWeb.RuntimeAsset "shadowed-dialog" "/assets/custom-dialog.js" "window.customDialog = 'second';"
          runtimeSite = sampleSite {siteRuntimeAssets = [firstAsset, shadowedAsset]}
          siteApplication = buildSiteApplication runtimeSite
      siteRuntimeAssets runtimeSite `shouldBe` [firstAsset, shadowedAsset]
      HarchWeb.applicationRuntimeAssets siteApplication `shouldBe` [firstAsset, shadowedAsset]
      response <- performWaiRequest (toWaiApplication siteApplication) (waiRequest ["assets", "custom-dialog.js"])
      expectAll
        ( (Wai.responseStatus response `shouldBe` Http.status200)
            :| [ lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just "application/javascript; charset=utf-8",
                 readResponseBody response `shouldReturn` "window.customDialog = 'first';"
               ]
        )

sampleSite :: Site SampleRoute () SampleContext SampleAuthorization
sampleSite =
  simpleSite
    SimpleSiteConfiguration
      { simpleSiteName = "sample",
        simpleSiteDefaultRequestContext = SampleContext "",
        simpleSiteRouteCodec = sampleRouteCodec,
        simpleSiteSecurity = sampleSecurity,
        simpleSiteCsrfProtection = siteTestCsrfProtection,
        simpleSitePageShell = samplePageShell,
        simpleSiteNavigationRoutes = [HomeRoute, SecondRoute],
        simpleSiteRouteDefinition = sampleRouteDefinition
      }

sampleContextPathPrefix :: SampleContext -> HarchWeb.PathPrefix
sampleContextPathPrefix (SampleContext value) =
  case HarchWeb.parseRequestPathPrefix value of
    Left parseError -> error ("invalid sample path prefix: " <> show parseError)
    Right pathPrefix -> pathPrefix

sampleRouteDefinition :: SampleRoute -> RouteDefinition SampleRoute SampleContext SampleAuthorization
sampleRouteDefinition route =
  case route of
    HomeRoute -> homeRouteDefinition
    SecondRoute -> secondRouteDefinition
    StatusApiRoute -> apiRouteDefinition
    NotFoundRoute -> notFoundRouteDefinition

homeRouteDefinition :: RouteDefinition SampleRoute SampleContext SampleAuthorization
homeRouteDefinition =
  Site.pageRoute (sampleMetadata HarchWeb.HtmlEndpoint HomeRoute) (Just "Home") $ \_ routeRequest ->
    pure
      Page
        { pageTitle = "Home",
          pageRoute = HomeRoute,
          pageContext = requestContext routeRequest,
          pageBody = HarchWeb.trustedHtml (MarkupUnsafe.unsafeTrustHtml ("<h1>Home</h1><p><a href=\"" <> renderRouteHref (requestContext routeRequest) SecondRoute <> "\">Browse second</a></p>")),
          pageBootstrapHooks = []
        }

secondRouteDefinition :: RouteDefinition SampleRoute SampleContext SampleAuthorization
secondRouteDefinition =
  Site.pageRoute (sampleMetadata HarchWeb.HtmlEndpoint SecondRoute) (Just "Second") $ \_ routeRequest ->
    pure
      Page
        { pageTitle = "Second",
          pageRoute = SecondRoute,
          pageContext = requestContext routeRequest,
          pageBody = HarchWeb.trustedHtml (MarkupUnsafe.unsafeTrustHtml ("<h1>Second</h1><p><a href=\"" <> renderRouteHref (requestContext routeRequest) HomeRoute <> "\">Return home</a></p>")),
          pageBootstrapHooks = ["second-page"]
        }

apiRouteDefinition :: RouteDefinition SampleRoute SampleContext SampleAuthorization
apiRouteDefinition =
  RouteDefinition
    { routeNavigationLabel = Nothing,
      routeMetadata = sampleMetadata HarchWeb.ApiEndpoint StatusApiRoute,
      routeMethods = [HarchWeb.RouteGet],
      routeExecutionPolicy = HarchWeb.unboundedRouteExecutionPolicy,
      routeHandler = Site.ProtocolRouteHandler $ \_ _ ->
        pure
          ( NonPageBodyResponse
              ResponseBody
                { responseStatus = Http.status200,
                  responseContentType = "application/json",
                  responseBody = "{\"status\":\"ok\"}",
                  responseObservabilityAttributes = [],
                  responseLogEntries = [],
                  responseDatabaseOperations = []
                }
          )
    }

notFoundRouteDefinition :: RouteDefinition SampleRoute SampleContext SampleAuthorization
notFoundRouteDefinition =
  ( Site.pageRoute (sampleMetadata HarchWeb.HtmlEndpoint NotFoundRoute) Nothing $ \_ routeRequest ->
      pure
        Page
          { pageTitle = "Not Found",
            pageRoute = NotFoundRoute,
            pageContext = requestContext routeRequest,
            pageBody = HarchWeb.trustedHtml (MarkupUnsafe.unsafeTrustHtml ("<h1>Not Found</h1><p><a href=\"" <> renderRouteHref (requestContext routeRequest) HomeRoute <> "\">Return home</a></p>")),
            pageBootstrapHooks = []
          }
  )
    { Site.routeMethods = []
    }

samplePageShell :: Page SampleRoute SampleContext -> PageShell SampleRoute SampleContext
samplePageShell page =
  HarchWeb.pageTitle page `seq`
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
        shellNavigationItems = [],
        shellMainId = HarchWeb.literalElementId "app-main",
        shellMainAttributes =
          [ HtmlAttribute
              { attributeName = "data-navigation-content",
                attributeValue = "true"
              }
          ],
        shellNavigationLifecycle = Nothing,
        shellStylesheets = [],
        shellRuntimeDescriptors = []
      }

sampleRouteCodec :: RouteCodec SampleRoute SampleContext
sampleRouteCodec =
  RouteCodec
    { parseRoute = \requestContextValue location ->
        case HarchWeb.safeUrlText (HarchWeb.encodeRouteLocation location) of
          "/" -> HarchWeb.RouteParsed RouteRequest {requestRoute = HomeRoute, requestContext = requestContextValue}
          "/second" -> HarchWeb.RouteParsed RouteRequest {requestRoute = SecondRoute, requestContext = requestContextValue}
          "/api/status" -> HarchWeb.RouteParsed RouteRequest {requestRoute = StatusApiRoute, requestContext = requestContextValue}
          _ -> HarchWeb.RouteNotMatched,
      renderRoute = \routeRequest -> routeLocationForTest (renderRouteHref (requestContext routeRequest) (requestRoute routeRequest)),
      notFoundRequest = \requestContextValue -> RouteRequest {requestRoute = NotFoundRoute, requestContext = requestContextValue},
      routeMethods =
        HarchWeb.routeMethodPolicy . \case
          NotFoundRoute -> []
          _ -> [HarchWeb.RouteGet]
    }

renderRouteHref :: SampleContext -> SampleRoute -> Text
renderRouteHref requestContextValue route =
  applyPrefix (pathPrefix requestContextValue) $
    case route of
      HomeRoute -> "/"
      SecondRoute -> "/second"
      StatusApiRoute -> "/api/status"
      NotFoundRoute -> "/404"

applyPrefix :: Text -> Text -> Text
applyPrefix prefix routePath
  | prefix == "" = routePath
  | routePath == "/" = prefix
  | otherwise = prefix <> routePath

waiRequest :: [Text] -> Wai.Request
waiRequest segments =
  Wai.defaultRequest
    { Wai.rawPathInfo = TextEncoding.encodeUtf8 renderedPath,
      Wai.pathInfo = segments
    }
  where
    renderedPath =
      case segments of
        [] -> "/"
        _ -> "/" <> Text.intercalate "/" segments

performWaiRequest :: IO Wai.Application -> Wai.Request -> IO Wai.Response
performWaiRequest buildWebApplication request = do
  webApplication <- buildWebApplication
  responseReference <- newIORef Nothing
  _ <-
    webApplication
      request
      ( \response -> do
          writeIORef responseReference (Just response)
          pure WaiInternal.ResponseReceived
      )
  maybeResponse <- readIORef responseReference
  case maybeResponse of
    Just response -> pure response
    Nothing -> expectationFailure "expected the WAI application to respond" >> pure (Wai.responseLBS Http.status500 [] "")

readResponseBody :: Wai.Response -> IO Text
readResponseBody response = do
  let (_, _, withStreamingBody) = Wai.responseToStream response
  chunksReference <- newIORef []
  withStreamingBody $ \streamingBody ->
    streamingBody
      (\builder -> modifyIORef' chunksReference (<> [Builder.toLazyByteString builder]))
      (pure ())
  chunks <- readIORef chunksReference
  pure (TextEncoding.decodeUtf8 (LazyByteString.toStrict (mconcat chunks)))

nonceFromHtml :: Text -> Maybe Text
nonceFromHtml html =
  Text.stripPrefix "<script nonce=\"" (snd (Text.breakOn "<script nonce=\"" html))
    >>= Just . Text.takeWhile (/= '"')
