{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWeb.SiteSpec (spec) where

import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LazyByteString
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb
  ( ClientActionPayload (..),
    ClientActionRequest (..),
    HtmlAttribute (..),
    Page (..),
    PageShell (..),
    Response (..),
    ResponseBody (..),
    RouteCodec (..),
    RouteRequest (..),
    toWaiApplication,
  )
import HarchWeb qualified
import HarchWeb.Markup.Unsafe qualified as MarkupUnsafe
import HarchWeb.Observability qualified as Observability
import HarchWeb.Site
  ( RouteDefinition (..),
    Site (..),
    buildSiteApplication,
    simpleSite,
  )
import HarchWeb.Site qualified as Site
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import Network.Wai.Internal qualified as WaiInternal
import Test.Hspec
import TestCore.CustomAssertions (expectAll)

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

spec :: Spec
spec = do
  describe "buildSiteApplication" $ do
    it "keeps the simpleSite defaults available when the composition root does not override them" $ do
      let siteApplication = buildSiteApplication sampleSite
          requestObservability =
            Observability.buildRequestObservability
              "GET"
              "http"
              "/"
              "/"
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
                 fmap HarchWeb.navigationRuntimePath (siteNavigationRuntime sampleSite) `shouldBe` Just "/assets/navigation.js",
                 siteNavigationRuntimePathPrefix sampleSite (SampleContext "/app") `shouldBe` "",
                 HarchWeb.httpsRedirectPort (siteRequestPolicy sampleSite) `shouldBe` Nothing,
                 HarchWeb.requestTransportLimits (siteRequestPolicy sampleSite) `shouldBe` HarchWeb.warpDefaultRequestTransportLimits,
                 HarchWeb.corsPolicy (siteRequestPolicy sampleSite) `shouldBe` HarchWeb.defaultCorsPolicyConfig,
                 length (siteRequestMiddleware sampleSite) `shouldBe` 0
               ]
        )
      siteRequestContextFromRequest sampleSite (waiRequest ["second"]) (SampleContext "/app") `shouldBe` SampleContext "/app"
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
      HarchWeb.handleClientAction
        siteApplication
        ClientActionRequest
          { clientAction = (),
            clientActionRequestIdempotencyKey = Nothing,
            clientActionContext = SampleContext ""
          }
        `shouldReturn` Nothing
      length (HarchWeb.applicationRequestMiddleware siteApplication) `shouldBe` 0
      siteReportRequestObservability sampleSite requestObservability `shouldReturn` ()
      siteReportConnectionObservability sampleSite connectionObservability `shouldReturn` ()
      siteReportApplicationLog sampleSite "sample-log" `shouldReturn` ()
      HarchWeb.reportRequestObservability siteApplication requestObservability `shouldReturn` ()
      HarchWeb.reportConnectionObservability siteApplication connectionObservability `shouldReturn` ()
      HarchWeb.reportApplicationLog siteApplication "sample-log" `shouldReturn` ()

    it "derives navigation items from labeled site routes and keeps route rendering prefix-aware" $ do
      let siteApplication = buildSiteApplication sampleSite
          homeRequest = RouteRequest {requestRoute = HomeRoute, requestContext = SampleContext "/app"}
      response <- HarchWeb.renderResponse siteApplication homeRequest
      case response of
        PageResponse page -> do
          let document = HarchWeb.pageShell siteApplication page
          HarchWeb.documentRuntimeDescriptors document
            `shouldBe` [HarchWeb.defaultCaptureKernel, HarchWeb.DeferredModule "harch-navigation" "/assets/navigation.js"]
          Text.isInfixOf "<script nonce=\"test-nonce\">" (HarchWeb.renderDocumentWithNonce (HarchWeb.RuntimeNonce "test-nonce") document)
            `shouldBe` True
        PageResponseWithMetadata _ _ ->
          expectationFailure "expected pageRoute to render a plain page response"
        BodyResponse _ ->
          expectationFailure "expected a page response for the home route"
        RedirectResponse _ _ ->
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

    it "takes method ownership from route definitions rather than a codec shadow table" $ do
      let conflictingCodec =
            (siteRouteCodec sampleSite)
              { HarchWeb.routeMethods = const [HarchWeb.RouteDelete]
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
                        shellMainId = "app-main",
                        shellMainAttributes = [],
                        shellStylesheets = [],
                        shellRuntimeDescriptors = []
                      }
              }
          siteApplication = buildSiteApplication bareShellSite
          request = RouteRequest {requestRoute = HomeRoute, requestContext = SampleContext ""}
      PageResponse page <- HarchWeb.renderResponse siteApplication request
      HarchWeb.renderDocument (HarchWeb.pageShell siteApplication page)
        `shouldBe` "<html><head><title>Home</title></head><body><nav data-navigation-region=\"primary\"><a href=\"/\" data-page-link=\"true\" aria-current=\"page\">Home</a><a href=\"/second\" data-page-link=\"true\">Second</a></nav><main id=\"app-main\" data-navigation-content=\"true\"><h1>Home</h1><p><a href=\"/second\">Browse second</a></p></main></body></html>"

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
      PageResponse page <- HarchWeb.renderResponse siteApplication request
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
      PageResponse page <- HarchWeb.renderResponse siteApplication request
      HarchWeb.documentRuntimeDescriptors (HarchWeb.pageShell siteApplication page)
        `shouldBe` [HarchWeb.defaultCaptureKernel, HarchWeb.DeferredModule "harch-navigation" "/assets/navigation.js"]

    it "renders the framework runtime script source from page context" $ do
      let prefixedRuntimeSite =
            sampleSite
              { siteNavigationRuntimePathPrefix = pathPrefix
              }
          siteApplication = buildSiteApplication prefixedRuntimeSite
          request = RouteRequest {requestRoute = HomeRoute, requestContext = SampleContext "/app"}
      PageResponse page <- HarchWeb.renderResponse siteApplication request
      HarchWeb.documentRuntimeDescriptors (HarchWeb.pageShell siteApplication page)
        `shouldBe` [HarchWeb.defaultCaptureKernel, HarchWeb.DeferredModule "harch-navigation" "/app/assets/navigation.js"]

sampleSite :: Site SampleRoute () SampleContext
sampleSite =
  simpleSite
    "sample"
    (SampleContext "")
    sampleRouteCodec
    samplePageShell
    [HomeRoute, SecondRoute]
    sampleRouteDefinition

sampleRouteDefinition :: SampleRoute -> RouteDefinition SampleRoute SampleContext
sampleRouteDefinition route =
  case route of
    HomeRoute -> homeRouteDefinition
    SecondRoute -> secondRouteDefinition
    StatusApiRoute -> apiRouteDefinition
    NotFoundRoute -> notFoundRouteDefinition

homeRouteDefinition :: RouteDefinition SampleRoute SampleContext
homeRouteDefinition =
  Site.pageRoute (Just "Home") $ \routeRequest ->
    pure
      Page
        { pageTitle = "Home",
          pageRoute = HomeRoute,
          pageContext = requestContext routeRequest,
          pageBody = HarchWeb.trustedHtml (MarkupUnsafe.unsafeTrustHtml ("<h1>Home</h1><p><a href=\"" <> renderRouteHref (requestContext routeRequest) SecondRoute <> "\">Browse second</a></p>")),
          pageBootstrapHooks = []
        }

secondRouteDefinition :: RouteDefinition SampleRoute SampleContext
secondRouteDefinition =
  Site.pageRoute (Just "Second") $ \routeRequest ->
    pure
      Page
        { pageTitle = "Second",
          pageRoute = SecondRoute,
          pageContext = requestContext routeRequest,
          pageBody = HarchWeb.trustedHtml (MarkupUnsafe.unsafeTrustHtml ("<h1>Second</h1><p><a href=\"" <> renderRouteHref (requestContext routeRequest) HomeRoute <> "\">Return home</a></p>")),
          pageBootstrapHooks = ["second-page"]
        }

apiRouteDefinition :: RouteDefinition SampleRoute SampleContext
apiRouteDefinition =
  RouteDefinition
    { routeNavigationLabel = Nothing,
      routeMethods = [HarchWeb.RouteGet],
      routeResponse =
        \_ ->
          pure
            ( BodyResponse
                ResponseBody
                  { responseStatus = 200,
                    responseContentType = "application/json",
                    responseBody = "{\"status\":\"ok\"}",
                    responseObservabilityAttributes = [],
                    responseLogEntries = []
                  }
            )
    }

notFoundRouteDefinition :: RouteDefinition SampleRoute SampleContext
notFoundRouteDefinition =
  ( Site.pageRoute Nothing $ \routeRequest ->
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
        shellMainId = "app-main",
        shellMainAttributes =
          [ HtmlAttribute
              { attributeName = "data-navigation-content",
                attributeValue = "true"
              }
          ],
        shellStylesheets = [],
        shellRuntimeDescriptors = []
      }

sampleRouteCodec :: RouteCodec SampleRoute SampleContext
sampleRouteCodec =
  RouteCodec
    { parseRoute = \requestContextValue path ->
        case path of
          "/" -> Just RouteRequest {requestRoute = HomeRoute, requestContext = requestContextValue}
          "/second" -> Just RouteRequest {requestRoute = SecondRoute, requestContext = requestContextValue}
          "/api/status" -> Just RouteRequest {requestRoute = StatusApiRoute, requestContext = requestContextValue}
          _ -> Nothing,
      renderRoute = \routeRequest -> renderRouteHref (requestContext routeRequest) (requestRoute routeRequest),
      notFoundRequest = \requestContextValue -> RouteRequest {requestRoute = NotFoundRoute, requestContext = requestContextValue},
      routeMethods = \case
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

performWaiRequest :: Wai.Application -> Wai.Request -> IO Wai.Response
performWaiRequest webApplication request = do
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
