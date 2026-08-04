{-# LANGUAGE OverloadedStrings #-}

module Unit.AppSpec (spec) where

import App.App (TwoPageAction (..), buildApplication, twoPageServerConfig, twoPageSite)
import App.Pages.Route.Generated
  ( PageRoute (..),
    allPageRoutes,
    pageRoutePath,
    parsePageRoute,
  )
import App.Routes (ApiRoute (..), CustomRoute (..), TwoPageNavigationTarget (..), TwoPageRoute (..), mkPreviewSlug, routeHref, twoPageNavigationPath)
import App.Routes qualified as ExampleRoutes
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LazyByteString
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb
  ( ClientActionPayload (..),
    ClientActionRequest (..),
    ListenerConfig (..),
    RouteRequest (..),
    appName,
    applicationStaticAssets,
    corsPolicy,
    defaultCorsPolicyConfig,
    defaultResponseSecurityHeadersConfig,
    defaultStaticAssetContentTypes,
    httpsRedirectPort,
    listenerConfigs,
    metricsExporter,
    notFoundRequest,
    observability,
    parseRoute,
    redirectHttpToHttps,
    renderRoute,
    requestPolicy,
    responseSecurityHeaders,
    staticAssetContentTypes,
    staticAssetRoots,
    staticAssets,
    staticCacheControlSeconds,
    strictTransportSecurity,
    toWaiApplication,
    tracingExporter,
    trustForwardedHeaders,
  )
import HarchWeb qualified
import HarchWeb.Site
  ( routeNavigationLabel,
    siteName,
    siteNavigationRoutes,
    siteRequestPolicy,
    siteRouteDefinition,
    siteStaticAssets,
  )
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import Network.Wai.Internal qualified as WaiInternal
import Test.Hspec
import TestCore.CustomAssertions (expectAll)

spec :: Spec
spec =
  describe "Unit.App" $ do
    describe "twoPageSite" $ do
      it "keeps the example site wiring small and explicit" $ do
        exerciseGeneratedPageRouteInstances
        expectAll
          ( (siteName twoPageSite `shouldBe` "two-pages-example")
              :| [ siteNavigationRoutes twoPageSite
                     `shouldBe` [Page HomePage, Page SecondPage, Page LiveDataPage],
                   allPageRoutes
                     `shouldBe` [HomePage, LiveDataPage, PageNotFound, SecondPage],
                   map showViaDictionary allPageRoutes
                     `shouldBe` ["HomePage", "LiveDataPage", "PageNotFound", "SecondPage"],
                   routeNavigationLabel (siteRouteDefinition twoPageSite (Page PageNotFound))
                     `shouldBe` Nothing,
                   routeNavigationLabel (siteRouteDefinition twoPageSite (Api LiveDataEvents))
                     `shouldBe` Nothing,
                   staticAssetRoots (siteStaticAssets twoPageSite)
                     `shouldBe` [HarchWeb.StaticAssetRoot {staticUrlPrefix = "/assets", staticDirectory = "public"}],
                   staticAssetContentTypes (siteStaticAssets twoPageSite) `shouldBe` defaultStaticAssetContentTypes,
                   staticCacheControlSeconds (siteStaticAssets twoPageSite) `shouldBe` Nothing,
                   redirectHttpToHttps (siteRequestPolicy twoPageSite) `shouldBe` False,
                   httpsRedirectPort (siteRequestPolicy twoPageSite) `shouldBe` Nothing,
                   strictTransportSecurity (siteRequestPolicy twoPageSite) `shouldBe` Nothing,
                   trustForwardedHeaders (siteRequestPolicy twoPageSite) `shouldBe` False,
                   corsPolicy (siteRequestPolicy twoPageSite) `shouldBe` defaultCorsPolicyConfig,
                   responseSecurityHeaders (siteRequestPolicy twoPageSite) `shouldBe` defaultResponseSecurityHeadersConfig
                 ]
          )

      it "uses a minimal local server config without extra deployment concerns" $ do
        expectAll
          ( ( listenerConfigs twoPageServerConfig
                `shouldBe` [ ListenerConfig
                               { listenerHost = "127.0.0.1",
                                 listenerPort = 8080,
                                 listenerScheme = HarchWeb.Http,
                                 listenerTls = Nothing,
                                 listenerAcme = Nothing
                               }
                           ]
            )
              :| [ staticAssetRoots (staticAssets twoPageServerConfig)
                     `shouldBe` [HarchWeb.StaticAssetRoot {staticUrlPrefix = "/assets", staticDirectory = "public"}],
                   staticAssetContentTypes (staticAssets twoPageServerConfig) `shouldBe` defaultStaticAssetContentTypes,
                   staticCacheControlSeconds (staticAssets twoPageServerConfig) `shouldBe` Nothing,
                   redirectHttpToHttps (requestPolicy twoPageServerConfig) `shouldBe` False,
                   httpsRedirectPort (requestPolicy twoPageServerConfig) `shouldBe` Nothing,
                   strictTransportSecurity (requestPolicy twoPageServerConfig) `shouldBe` Nothing,
                   trustForwardedHeaders (requestPolicy twoPageServerConfig) `shouldBe` False,
                   corsPolicy (requestPolicy twoPageServerConfig) `shouldBe` defaultCorsPolicyConfig,
                   responseSecurityHeaders (requestPolicy twoPageServerConfig) `shouldBe` defaultResponseSecurityHeadersConfig,
                   tracingExporter (observability twoPageServerConfig) `shouldBe` Nothing,
                   metricsExporter (observability twoPageServerConfig) `shouldBe` Nothing
                 ]
          )

    describe "routeCodec" $
      it "parses and renders the supported two-page routes" $ do
        let previewSlug =
              maybe (error "expected valid test preview slug") id (mkPreviewSlug "summer-release")
        expectAll
          ( (eqViaDictionary (Page HomePage) (Page HomePage) `shouldBe` True)
              :| [ eqViaDictionary (Page HomePage) (Page SecondPage) `shouldBe` False,
                   eqViaDictionary (Page LiveDataPage) (Api LiveDataEvents) `shouldBe` False,
                   neqViaDictionary (Page HomePage) (Page SecondPage) `shouldBe` True,
                   showViaDictionary (Page HomePage) `shouldBe` "Page HomePage",
                   showViaDictionary (Page SecondPage) `shouldBe` "Page SecondPage",
                   showViaDictionary (Page LiveDataPage) `shouldBe` "Page LiveDataPage",
                   showViaDictionary (Api LiveDataEvents) `shouldBe` "Api LiveDataEvents",
                   eqViaDictionary LiveDataEvents LiveDataEvents `shouldBe` True,
                   neqViaDictionary LiveDataEvents LiveDataEvents `shouldBe` False,
                   showViaDictionary LiveDataEvents `shouldBe` "LiveDataEvents",
                   showsPrecViaDictionary 11 LiveDataEvents "" `shouldSatisfy` not . null,
                   showListViaDictionary [LiveDataEvents] "" `shouldSatisfy` not . null,
                   eqViaDictionary previewSlug previewSlug `shouldBe` True,
                   neqViaDictionary previewSlug previewSlug `shouldBe` False,
                   showViaDictionary previewSlug `shouldBe` "PreviewSlug \"summer-release\"",
                   showsPrecViaDictionary 11 previewSlug "" `shouldSatisfy` not . null,
                   showListViaDictionary [previewSlug] "" `shouldSatisfy` not . null,
                   eqViaDictionary (PreviewPage previewSlug) (PreviewPage previewSlug) `shouldBe` True,
                   neqViaDictionary (PreviewPage previewSlug) (PreviewPage previewSlug)
                     `shouldBe` False,
                   showViaDictionary (PreviewPage previewSlug)
                     `shouldBe` "PreviewPage (PreviewSlug \"summer-release\")",
                   showsPrecViaDictionary 11 (PreviewPage previewSlug) ""
                     `shouldSatisfy` not . null,
                   showListViaDictionary [PreviewPage previewSlug] ""
                     `shouldSatisfy` not . null,
                   showViaDictionary (Page PageNotFound) `shouldBe` "Page PageNotFound",
                   showsPrecViaDictionary 0 (Page HomePage) "" `shouldBe` "Page HomePage",
                   showListViaDictionary ([] :: [TwoPageRoute]) "" `shouldBe` "[]",
                   showListViaDictionary [Page HomePage, Page SecondPage, Page LiveDataPage] "" `shouldBe` "[Page HomePage,Page SecondPage,Page LiveDataPage]",
                   parseRoute ExampleRoutes.routeCodec () "/" `shouldBe` Just RouteRequest {requestRoute = Page HomePage, requestContext = ()},
                   parseRoute ExampleRoutes.routeCodec () "/second" `shouldBe` Just RouteRequest {requestRoute = Page SecondPage, requestContext = ()},
                   parseRoute ExampleRoutes.routeCodec () "/second?utm=demo" `shouldBe` Just RouteRequest {requestRoute = Page SecondPage, requestContext = ()},
                   parseRoute ExampleRoutes.routeCodec () "/live-data" `shouldBe` Just RouteRequest {requestRoute = Page LiveDataPage, requestContext = ()},
                   parseRoute ExampleRoutes.routeCodec () "/live-data/events" `shouldBe` Just RouteRequest {requestRoute = Api LiveDataEvents, requestContext = ()},
                   parseRoute ExampleRoutes.routeCodec () "/preview/summer-release"
                     `shouldBe` (\slug -> RouteRequest {requestRoute = Custom (PreviewPage slug), requestContext = ()}) <$> mkPreviewSlug "summer-release",
                   parseRoute ExampleRoutes.routeCodec () "/preview/Invalid" `shouldBe` Nothing,
                   parseRoute ExampleRoutes.routeCodec () "/missing" `shouldBe` Nothing,
                   renderRoute ExampleRoutes.routeCodec RouteRequest {requestRoute = Page HomePage, requestContext = ()} `shouldBe` "/",
                   renderRoute ExampleRoutes.routeCodec RouteRequest {requestRoute = Page SecondPage, requestContext = ()} `shouldBe` "/second",
                   renderRoute ExampleRoutes.routeCodec RouteRequest {requestRoute = Page LiveDataPage, requestContext = ()} `shouldBe` "/live-data",
                   renderRoute ExampleRoutes.routeCodec RouteRequest {requestRoute = Api LiveDataEvents, requestContext = ()} `shouldBe` "/live-data/events",
                   renderRoute ExampleRoutes.routeCodec RouteRequest {requestRoute = Page PageNotFound, requestContext = ()} `shouldBe` "/404",
                   routeHref (Page HomePage) `shouldBe` "/",
                   routeHref (Page SecondPage) `shouldBe` "/second",
                   routeHref (Page LiveDataPage) `shouldBe` "/live-data",
                   routeHref (Api LiveDataEvents) `shouldBe` "/live-data/events",
                   routeHref (Page PageNotFound) `shouldBe` "/404",
                   twoPageNavigationPath (NavigationPage HomePage) `shouldBe` "/",
                   twoPageNavigationPath (NavigationPreview previewSlug) `shouldBe` "/preview/summer-release",
                   notFoundRequest ExampleRoutes.routeCodec () `shouldBe` RouteRequest {requestRoute = Page PageNotFound, requestContext = ()},
                   map (parsePageRoute . pageRoutePath) allPageRoutes
                     `shouldBe` map Just allPageRoutes,
                   parseRoute ExampleRoutes.routeCodec () "/assets/navigation.js" `shouldBe` Nothing
                 ]
          )

    describe "buildApplication" $ do
      it "decodes only the modeled subscription action and preserves an absent email as validation input" $ do
        let actionPayload method path fields =
              ClientActionPayload
                { clientActionMethod = method,
                  clientActionPath = path,
                  clientActionFields = fields,
                  clientActionCsrfToken = Nothing,
                  clientActionPayloadContext = ()
                }
        let absentEmailDecoded =
              case HarchWeb.decodeClientAction
                buildApplication
                (actionPayload "POST" "/actions/subscribe" []) of
                HarchWeb.DecodedClientAction (SubscribeAction emailAddress) -> Text.null emailAddress
                _ -> False
            unknownActionRejected =
              case HarchWeb.decodeClientAction
                buildApplication
                (actionPayload "POST" "/actions/missing" []) of
                HarchWeb.UnrecognizedClientAction -> True
                _ -> False
            duplicateEmailRejected =
              case HarchWeb.decodeClientAction
                buildApplication
                (actionPayload "POST" "/actions/subscribe" [("email", "first@example.com"), ("email", "second@example.com")]) of
                HarchWeb.MalformedClientAction _ -> True
                _ -> False
            wrongMethodRejected =
              case HarchWeb.decodeClientAction buildApplication (actionPayload "GET" "/actions/subscribe" []) of
                HarchWeb.MethodNotAllowedClientAction _ -> True
                _ -> False
        expectAll
          ( (absentEmailDecoded `shouldBe` True)
              :| [ unknownActionRejected `shouldBe` True,
                   duplicateEmailRejected `shouldBe` True,
                   wrongMethodRejected `shouldBe` True
                 ]
          )

      it "totally dispatches every generated page to a complete SSR page" $ do
        responses <-
          traverse
            ( \pageRoute ->
                HarchWeb.renderResponse
                  buildApplication
                  RouteRequest {requestRoute = Page pageRoute, requestContext = ()}
            )
            allPageRoutes
        responses `shouldSatisfy` all isCompletePageResponse

      it "renders the home page with shared navigation and the enhancement runtime" $ do
        let application = buildApplication
            authorComponents =
              Text.concat
                [ "<section data-page-example=\"about\"><h2>About this example</h2>",
                  "<section data-example-author-card=\"true\"><p>Harch Web team</p>",
                  "<p>SSR framework maintainers</p>",
                  "<p>The page and its controls are complete before optional JavaScript loads.</p>",
                  "</section><div data-example-author-avatar=\"compact\"><p>HW</p>",
                  "<p>Maintained as a small, runnable framework reference.</p></div></section>"
                ]
        response <- performWaiRequest (toWaiApplication application) (waiRequest [])
        responseBody <- readResponseBody response
        expectAll
          ( (appName application `shouldBe` "two-pages-example")
              :| [ staticAssetRoots (applicationStaticAssets application)
                     `shouldBe` [HarchWeb.StaticAssetRoot {staticUrlPrefix = "/assets", staticDirectory = "public"}],
                   Wai.responseStatus response `shouldBe` Http.status200,
                   Text.isInfixOf "<title>Home</title>" responseBody `shouldBe` True,
                   Text.isInfixOf "<link rel=\"stylesheet\" href=\"/assets/two-pages.css\">" responseBody `shouldBe` True,
                   Text.isInfixOf "<section data-page=\"home\" class=\"harch-home-root\">" responseBody `shouldBe` True,
                   Text.isInfixOf "<nav data-navigation-region=\"primary\"><a href=\"/\" data-page-link=\"true\" aria-current=\"page\">Home</a><a href=\"/second\" data-page-link=\"true\">Second</a><a href=\"/live-data\" data-page-link=\"true\">Live updates</a></nav>" responseBody `shouldBe` True,
                   Text.isInfixOf "<a href=\"/second\" data-page-link=\"true\">Go to the second page</a>" responseBody `shouldBe` True,
                   Text.isInfixOf "<a href=\"/live-data\" data-page-link=\"true\">See live updates</a>" responseBody `shouldBe` True,
                   Text.isInfixOf authorComponents responseBody `shouldBe` True,
                   Text.isInfixOf "data-harch-action-method=\"post\"" responseBody `shouldBe` True,
                   Text.isInfixOf "action=\"/actions/subscribe\" method=\"dialog\"" responseBody `shouldBe` True,
                   Text.isInfixOf "<p id=\"subscription-result\" data-harch-region=\"true\" role=\"status\"></p>" responseBody `shouldBe` True,
                   Text.isInfixOf "<script nonce=\"" responseBody `shouldBe` True,
                   Text.isInfixOf "new FormData(target, submitter)" responseBody `shouldBe` True,
                   Text.isInfixOf "event.preventDefault()" responseBody `shouldBe` True,
                   Text.isInfixOf "<script type=\"module\" src=\"/assets/navigation.js\" defer></script>" responseBody `shouldBe` True
                 ]
          )

      it "renders the second page as full SSR HTML with bootstrap hooks" $ do
        response <- performWaiRequest (toWaiApplication buildApplication) (waiRequest ["second"])
        responseBody <- readResponseBody response
        expectAll
          ( (Wai.responseStatus response `shouldBe` Http.status200)
              :| [ Text.isInfixOf "<title>Second</title>" responseBody `shouldBe` True,
                   Text.isInfixOf "data-bootstrap-hooks=\"second-page\"" responseBody `shouldBe` True,
                   Text.isInfixOf "<a href=\"/\" data-page-link=\"true\">Back home</a>" responseBody `shouldBe` True
                 ]
          )

      it "renders the live-data page before the optional EventSource enhancement starts" $ do
        response <- performWaiRequest (toWaiApplication buildApplication) (waiRequest ["live-data"])
        responseBody <- readResponseBody response
        expectAll
          ( (Wai.responseStatus response `shouldBe` Http.status200)
              :| [ Text.isInfixOf "<title>Live updates</title>" responseBody `shouldBe` True,
                   Text.isInfixOf "This complete status is rendered on the server before any live connection starts." responseBody `shouldBe` True,
                   Text.isInfixOf "<p id=\"live-data-status\" data-live-data-status role=\"status\">Waiting for an update.</p>" responseBody `shouldBe` True,
                   Text.isInfixOf "<script type=\"module\" src=\"/assets/live-data.js\" defer></script>" responseBody `shouldBe` True
                 ]
          )

      it "streams the live-data update with event-stream headers" $ do
        response <- performWaiRequest (toWaiApplication buildApplication) (waiRequest ["live-data", "events"])
        responseBody <- readResponseBody response
        expectAll
          ( (Wai.responseStatus response `shouldBe` Http.status200)
              :| [ lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just "text/event-stream; charset=utf-8",
                   lookup "Cache-Control" (Wai.responseHeaders response) `shouldBe` Just "no-cache",
                   responseBody `shouldBe` "event: update\nid: example-1\ndata: The live update arrived.\n\n"
                 ]
          )

      it "renders an explicit typed dynamic route as complete SSR HTML" $ do
        let previewSlug =
              maybe (error "expected valid test preview slug") id (mkPreviewSlug "summer-release")
            previewRoute = Custom (PreviewPage previewSlug)
        response <-
          performWaiRequest
            (toWaiApplication buildApplication)
            (waiRequest ["preview", "summer-release"])
        responseBody <- readResponseBody response
        renderedResponse <-
          HarchWeb.renderResponse
            buildApplication
            RouteRequest {requestRoute = previewRoute, requestContext = ()}
        expectAll
          ( (Wai.responseStatus response `shouldBe` Http.status200)
              :| [ Text.isInfixOf "<title>Preview: summer-release</title>" responseBody
                     `shouldBe` True,
                   Text.isInfixOf
                     "<section data-page=\"preview\"><h1>Preview</h1>"
                     responseBody
                     `shouldBe` True,
                   Text.isInfixOf "<p>summer-release</p>" responseBody `shouldBe` True,
                   routeNavigationLabel (siteRouteDefinition twoPageSite previewRoute)
                     `shouldBe` Nothing,
                   renderedResponse `shouldSatisfy` hasPageRoute previewRoute
                 ]
          )

      it "renders not-found responses through the same shell with a 404 status" $ do
        response <- performWaiRequest (toWaiApplication buildApplication) (waiRequest ["missing"])
        responseBody <- readResponseBody response
        expectAll
          ( (Wai.responseStatus response `shouldBe` Http.status404)
              :| [ Text.isInfixOf "<title>Not Found</title>" responseBody `shouldBe` True,
                   Text.isInfixOf "<section data-page=\"not-found\">" responseBody `shouldBe` True,
                   Text.isInfixOf "<a href=\"/\" data-page-link=\"true\">Home</a><a href=\"/second\" data-page-link=\"true\">Second</a>" responseBody `shouldBe` True
                 ]
          )

      it "serves the navigation runtime through an unlabeled site route" $ do
        response <- performWaiRequest (toWaiApplication buildApplication) (waiRequest ["assets", "navigation.js"])
        responseBody <- readResponseBody response
        expectAll
          ( (Wai.responseStatus response `shouldBe` Http.status200)
              :| [ lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just "application/javascript; charset=utf-8",
                   Text.isInfixOf "navigateTo" responseBody `shouldBe` True,
                   Text.isInfixOf "handlePopState" responseBody `shouldBe` True,
                   Text.isInfixOf "data-page-link=\"true\"" responseBody `shouldBe` True,
                   Text.isInfixOf "window.location.assign" responseBody `shouldBe` True,
                   Text.isInfixOf "X-Harch-Action" responseBody `shouldBe` True,
                   Text.isInfixOf "actionUrl.origin !== window.location.origin" responseBody `shouldBe` True,
                   Text.isInfixOf "captureKernel.register" responseBody `shouldBe` True
                 ]
          )

      it "serves the typed stylesheet through the configured static asset root" $ do
        response <- performWaiRequest (toWaiApplication buildApplication) (waiRequest ["assets", "two-pages.css"])
        responseBody <- readResponseBody response
        expectAll
          ( (Wai.responseStatus response `shouldBe` Http.status200)
              :| [ lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just "text/css; charset=utf-8",
                   Text.isInfixOf ".harch-home-root" responseBody `shouldBe` True
                 ]
          )

      it "serves the deferred live-data enhancement as a same-origin static asset" $ do
        response <- performWaiRequest (toWaiApplication buildApplication) (waiRequest ["assets", "live-data.js"])
        responseBody <- readResponseBody response
        expectAll
          ( (Wai.responseStatus response `shouldBe` Http.status200)
              :| [ lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just "application/javascript; charset=utf-8",
                   Text.isInfixOf "new EventSource" responseBody `shouldBe` True,
                   Text.isInfixOf "eventSource.close()" responseBody `shouldBe` True
                 ]
          )

      it "returns validation patches for captured subscription actions" $ do
        actionBodyChunks <- newIORef [TextEncoding.encodeUtf8 "email=ada%40example&_harch_csrf=csrf-token"]
        let actionRequest =
              Wai.setRequestBodyChunks
                (nextRequestBodyChunk actionBodyChunks)
                ( (waiRequest ["actions", "subscribe"])
                    { Wai.requestMethod = "POST",
                      Wai.requestHeaders = [("X-Harch-Action", "1"), (Http.hContentType, "application/x-www-form-urlencoded"), ("Host", "example.test"), ("Origin", "http://example.test"), ("Cookie", "harch-csrf=csrf-token")]
                    }
                )
        response <- performWaiRequest (toWaiApplication buildApplication) actionRequest
        responseBody <- readResponseBody response
        directResponse <-
          HarchWeb.handleClientAction
            buildApplication
            ClientActionRequest
              { clientAction = SubscribeAction "ada@example",
                clientActionContext = ()
              }
        expectAll
          ( (Wai.responseStatus response `shouldBe` Http.status422)
              :| [ Text.isInfixOf "Enter a valid email address." responseBody `shouldBe` True,
                   Text.isInfixOf "\"focusId\":\"subscription-email\"" responseBody `shouldBe` True,
                   fmap HarchWeb.clientActionHeaders directResponse `shouldBe` Just [],
                   fmap HarchWeb.clientActionObservabilityAttributes directResponse `shouldBe` Just [],
                   fmap HarchWeb.clientActionLogEntries directResponse `shouldBe` Just []
                 ]
          )

      it "returns a success patch for valid captured subscription actions" $ do
        actionBodyChunks <- newIORef [TextEncoding.encodeUtf8 "email=ada%40example.com&_harch_csrf=csrf-token"]
        let actionRequest =
              Wai.setRequestBodyChunks
                (nextRequestBodyChunk actionBodyChunks)
                ( (waiRequest ["actions", "subscribe"])
                    { Wai.requestMethod = "POST",
                      Wai.requestHeaders = [("X-Harch-Action", "1"), (Http.hContentType, "application/x-www-form-urlencoded"), ("Host", "example.test"), ("Origin", "http://example.test"), ("Cookie", "harch-csrf=csrf-token")]
                    }
                )
        response <- performWaiRequest (toWaiApplication buildApplication) actionRequest
        responseBody <- readResponseBody response
        directResponse <-
          HarchWeb.handleClientAction
            buildApplication
            ClientActionRequest
              { clientAction = SubscribeAction "ada@example.com",
                clientActionContext = ()
              }
        expectAll
          ( (Wai.responseStatus response `shouldBe` Http.status200)
              :| [ Text.isInfixOf "Thanks. Your subscription request is ready." responseBody `shouldBe` True,
                   Text.isInfixOf "\"focusId\":null" responseBody `shouldBe` True,
                   fmap HarchWeb.clientActionHeaders directResponse `shouldBe` Just [],
                   fmap HarchWeb.clientActionObservabilityAttributes directResponse `shouldBe` Just [],
                   fmap HarchWeb.clientActionLogEntries directResponse `shouldBe` Just []
                 ]
          )

      it "rejects an address that does not contain an at sign" $ do
        invalidAction <-
          HarchWeb.handleClientAction
            buildApplication
            ClientActionRequest
              { clientAction = SubscribeAction "invalid",
                clientActionContext = ()
              }
        fmap HarchWeb.clientActionStatus invalidAction `shouldBe` Just 422

isCompletePageResponse :: HarchWeb.Response TwoPageRoute () -> Bool
isCompletePageResponse response =
  case response of
    HarchWeb.PageResponse page ->
      not (Text.null (HarchWeb.renderHtml (HarchWeb.pageBody page)))
    _ -> False

waiRequest :: [Text.Text] -> Wai.Request
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
  _ <- webApplication request (\response -> writeIORef responseReference (Just response) >> pure WaiInternal.ResponseReceived)
  maybeResponse <- readIORef responseReference
  maybe
    (ioError (userError "expected the WAI application to produce a response"))
    pure
    maybeResponse

readResponseBody :: Wai.Response -> IO Text.Text
readResponseBody response =
  decodeUtf8Response <$> readResponseBytes response

nextRequestBodyChunk :: IORef [ByteString.ByteString] -> IO ByteString.ByteString
nextRequestBodyChunk chunksReference =
  atomicModifyIORef' chunksReference $ \chunks ->
    case chunks of
      [] -> ([], ByteString.empty)
      chunk : remainingChunks -> (remainingChunks, chunk)

readResponseBytes :: Wai.Response -> IO LazyByteString.ByteString
readResponseBytes response = do
  buildersReference <- newIORef []
  case response of
    WaiInternal.ResponseBuilder _ _ bodyBuilder ->
      pure (Builder.toLazyByteString bodyBuilder)
    WaiInternal.ResponseFile _ _ path _ ->
      LazyByteString.readFile path
    WaiInternal.ResponseStream _ _ streamingBody -> do
      streamingBody
        (\builder -> modifyIORef' buildersReference (builder :))
        (pure ())
      builders <- readIORef buildersReference
      pure (Builder.toLazyByteString (mconcat (reverse builders)))
    WaiInternal.ResponseRaw _ fallbackResponse ->
      readResponseBytes fallbackResponse

decodeUtf8Response :: LazyByteString.ByteString -> Text.Text
decodeUtf8Response =
  TextEncoding.decodeUtf8 . LazyByteString.toStrict

hasPageRoute :: TwoPageRoute -> HarchWeb.Response TwoPageRoute () -> Bool
hasPageRoute expectedRoute response =
  case response of
    HarchWeb.PageResponse page -> HarchWeb.pageRoute page == expectedRoute
    _ -> False

exerciseGeneratedPageRouteInstances :: Expectation
exerciseGeneratedPageRouteInstances = do
  let routes = [HomePage, LiveDataPage, PageNotFound, SecondPage]
  (minimumViaDictionary :: PageRoute) `shouldBe` HomePage
  (maximumViaDictionary :: PageRoute) `shouldBe` SecondPage
  successorViaDictionary HomePage `shouldBe` LiveDataPage
  predecessorViaDictionary SecondPage `shouldBe` PageNotFound
  (toEnumViaDictionary 0 :: PageRoute) `shouldBe` HomePage
  fromEnumViaDictionary HomePage `shouldBe` 0
  enumFromViaDictionary HomePage `shouldBe` routes
  enumFromThenViaDictionary HomePage LiveDataPage `shouldBe` routes
  enumFromToViaDictionary HomePage SecondPage `shouldBe` routes
  enumFromThenToViaDictionary HomePage LiveDataPage SecondPage `shouldBe` routes
  eqViaDictionary HomePage HomePage `shouldBe` True
  neqViaDictionary HomePage SecondPage `shouldBe` True
  showViaDictionary HomePage `shouldBe` "HomePage"
  showsPrecViaDictionary 11 HomePage "" `shouldBe` "HomePage"
  showListViaDictionary routes ""
    `shouldBe` "[HomePage,LiveDataPage,PageNotFound,SecondPage]"

eqViaDictionary :: (Eq a) => a -> a -> Bool
eqViaDictionary = (==)
{-# NOINLINE eqViaDictionary #-}

showViaDictionary :: (Show a) => a -> String
showViaDictionary = show
{-# NOINLINE showViaDictionary #-}

neqViaDictionary :: (Eq a) => a -> a -> Bool
neqViaDictionary = (/=)
{-# NOINLINE neqViaDictionary #-}

showsPrecViaDictionary :: (Show a) => Int -> a -> ShowS
showsPrecViaDictionary = showsPrec
{-# NOINLINE showsPrecViaDictionary #-}

showListViaDictionary :: (Show a) => [a] -> ShowS
showListViaDictionary = showList
{-# NOINLINE showListViaDictionary #-}

minimumViaDictionary :: (Bounded a) => a
minimumViaDictionary = minBound
{-# NOINLINE minimumViaDictionary #-}

maximumViaDictionary :: (Bounded a) => a
maximumViaDictionary = maxBound
{-# NOINLINE maximumViaDictionary #-}

successorViaDictionary :: (Enum a) => a -> a
successorViaDictionary = succ
{-# NOINLINE successorViaDictionary #-}

predecessorViaDictionary :: (Enum a) => a -> a
predecessorViaDictionary = pred
{-# NOINLINE predecessorViaDictionary #-}

toEnumViaDictionary :: (Enum a) => Int -> a
toEnumViaDictionary = toEnum
{-# NOINLINE toEnumViaDictionary #-}

fromEnumViaDictionary :: (Enum a) => a -> Int
fromEnumViaDictionary = fromEnum
{-# NOINLINE fromEnumViaDictionary #-}

enumFromViaDictionary :: (Enum a) => a -> [a]
enumFromViaDictionary = enumFrom
{-# NOINLINE enumFromViaDictionary #-}

enumFromThenViaDictionary :: (Enum a) => a -> a -> [a]
enumFromThenViaDictionary = enumFromThen
{-# NOINLINE enumFromThenViaDictionary #-}

enumFromToViaDictionary :: (Enum a) => a -> a -> [a]
enumFromToViaDictionary = enumFromTo
{-# NOINLINE enumFromToViaDictionary #-}

enumFromThenToViaDictionary :: (Enum a) => a -> a -> a -> [a]
enumFromThenToViaDictionary = enumFromThenTo
{-# NOINLINE enumFromThenToViaDictionary #-}
