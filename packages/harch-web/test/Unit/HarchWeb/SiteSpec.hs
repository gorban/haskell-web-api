{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWeb.SiteSpec (spec) where

import Control.Exception (SomeException, displayException, try)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LazyByteString
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb
  ( HtmlAttribute (..),
    Page (..),
    PageShell (..),
    Response (..),
    ResponseBody (..),
    RouteCodec (..),
    RouteRequest (..),
    toWaiApplication,
  )
import HarchWeb qualified
import HarchWeb.Observability qualified as Observability
import HarchWeb.Site
  ( Site (..),
    SiteRoute (..),
    buildSiteApplication,
    pageSiteRoute,
    simpleSite,
  )
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import Network.Wai.Internal qualified as WaiInternal
import Test.Hspec

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
      siteName sampleSite `shouldBe` "sample"
      HarchWeb.appName siteApplication `shouldBe` "sample"
      HarchWeb.staticAssetRoots (siteStaticAssets sampleSite) `shouldBe` []
      HarchWeb.staticAssetContentTypes (siteStaticAssets sampleSite) `shouldBe` HarchWeb.defaultStaticAssetContentTypes
      HarchWeb.staticCacheControlSeconds (siteStaticAssets sampleSite) `shouldBe` Nothing
      HarchWeb.httpsRedirectPort (siteRequestPolicy sampleSite) `shouldBe` Nothing
      HarchWeb.corsPolicy (siteRequestPolicy sampleSite) `shouldBe` HarchWeb.defaultCorsPolicyConfig
      siteRequestContextFromRequest sampleSite (waiRequest ["second"]) (SampleContext "/app") `shouldBe` SampleContext "/app"
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
        PageResponse page ->
          HarchWeb.pageShell siteApplication page
            `shouldBe` "<html><head><title>Home</title></head><body data-app=\"sample\"><nav data-navigation-region=\"primary\"><a href=\"/app\" aria-current=\"page\">Home</a><a href=\"/app/second\">Second</a></nav><main id=\"app-main\" data-navigation-content=\"true\"><h1>Home</h1><p><a href=\"/app/second\">Browse second</a></p></main></body></html>"
        PageResponseWithMetadata _ _ ->
          expectationFailure "expected pageSiteRoute to render a plain page response"
        BodyResponse _ ->
          expectationFailure "expected a page response for the home route"

    it "renders the configured not-found page through the shared shell with a 404 status" $ do
      response <- performWaiRequest (toWaiApplication (buildSiteApplication sampleSite)) (waiRequest ["missing"])
      Wai.responseStatus response `shouldBe` Http.status404
      readResponseBody response
        `shouldReturn` "<html><head><title>Not Found</title></head><body data-app=\"sample\"><nav data-navigation-region=\"primary\"><a href=\"/\">Home</a><a href=\"/second\">Second</a></nav><main id=\"app-main\" data-navigation-content=\"true\"><h1>Not Found</h1><p><a href=\"/\">Return home</a></p></main></body></html>"

    it "preserves body responses for unlabeled non-page routes" $ do
      response <- performWaiRequest (toWaiApplication (buildSiteApplication sampleSite)) (waiRequest ["api", "status"])
      Wai.responseStatus response `shouldBe` Http.status200
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just "application/json"
      readResponseBody response `shouldReturn` "{\"status\":\"ok\"}"

    it "fails loudly when the matched not-found route has not been configured" $ do
      let brokenSite =
            sampleSite
              { siteRoutes =
                  [ homeSiteRoute,
                    secondSiteRoute,
                    apiSiteRoute
                  ]
              }
      result <- try (performWaiRequest (toWaiApplication (buildSiteApplication brokenSite)) (waiRequest ["missing"])) :: IO (Either SomeException Wai.Response)
      case result of
        Left failure ->
          displayException failure `shouldContain` "No site route configured for matched route: NotFoundRoute"
        Right _ ->
          expectationFailure "expected missing not-found route configuration to fail"

sampleSite :: Site SampleRoute SampleContext
sampleSite =
  simpleSite
    "sample"
    (SampleContext "")
    sampleRouteCodec
    samplePageShell
    [ homeSiteRoute,
      secondSiteRoute,
      apiSiteRoute,
      notFoundSiteRoute
    ]

homeSiteRoute :: SiteRoute SampleRoute SampleContext
homeSiteRoute =
  pageSiteRoute HomeRoute (Just "Home") $ \routeRequest ->
    pure
      Page
        { pageTitle = "Home",
          pageRoute = HomeRoute,
          pageContext = requestContext routeRequest,
          pageBody = "<h1>Home</h1><p><a href=\"" <> renderRouteHref (requestContext routeRequest) SecondRoute <> "\">Browse second</a></p>",
          pageBootstrapHooks = []
        }

secondSiteRoute :: SiteRoute SampleRoute SampleContext
secondSiteRoute =
  pageSiteRoute SecondRoute (Just "Second") $ \routeRequest ->
    pure
      Page
        { pageTitle = "Second",
          pageRoute = SecondRoute,
          pageContext = requestContext routeRequest,
          pageBody = "<h1>Second</h1><p><a href=\"" <> renderRouteHref (requestContext routeRequest) HomeRoute <> "\">Return home</a></p>",
          pageBootstrapHooks = ["second-page"]
        }

apiSiteRoute :: SiteRoute SampleRoute SampleContext
apiSiteRoute =
  SiteRoute
    { siteRouteValue = StatusApiRoute,
      siteRouteNavigationLabel = Nothing,
      siteRouteResponse =
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

notFoundSiteRoute :: SiteRoute SampleRoute SampleContext
notFoundSiteRoute =
  pageSiteRoute NotFoundRoute Nothing $ \routeRequest ->
    pure
      Page
        { pageTitle = "Not Found",
          pageRoute = NotFoundRoute,
          pageContext = requestContext routeRequest,
          pageBody = "<h1>Not Found</h1><p><a href=\"" <> renderRouteHref (requestContext routeRequest) HomeRoute <> "\">Return home</a></p>",
          pageBootstrapHooks = []
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
        shellScriptSources = []
      }

sampleRouteCodec :: RouteCodec SampleRoute SampleContext
sampleRouteCodec =
  RouteCodec
    { parseRoute = \context path ->
        case path of
          "/" -> Just RouteRequest {requestRoute = HomeRoute, requestContext = context}
          "/second" -> Just RouteRequest {requestRoute = SecondRoute, requestContext = context}
          "/api/status" -> Just RouteRequest {requestRoute = StatusApiRoute, requestContext = context}
          _ -> Nothing,
      renderRoute = \routeRequest -> renderRouteHref (requestContext routeRequest) (requestRoute routeRequest),
      notFoundRequest = \context -> RouteRequest {requestRoute = NotFoundRoute, requestContext = context}
    }

renderRouteHref :: SampleContext -> SampleRoute -> Text
renderRouteHref context route =
  applyPrefix (pathPrefix context) $
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
