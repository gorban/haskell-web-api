{-# LANGUAGE OverloadedStrings #-}

module Unit.AppSpec (spec) where

import App.App (buildApplication, twoPageServerConfig, twoPageSite)
import App.Routes (TwoPageRoute (..), routeHref)
import qualified App.Routes as ExampleRoutes
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import HarchWeb
  ( ListenerConfig (..),
    RouteRequest (..),
    applicationStaticAssets,
    appName,
    corsPolicy,
    defaultCorsPolicyConfig,
    defaultResponseSecurityHeadersConfig,
    defaultStaticAssetContentTypes,
    httpsRedirectPort,
    listenerConfigs,
    notFoundRequest,
    observability,
    parseRoute,
    redirectHttpToHttps,
    renderRoute,
    requestPolicy,
    responseSecurityHeaders,
    metricsExporter,
    staticAssetContentTypes,
    staticAssetRoots,
    staticAssets,
    staticCacheControlSeconds,
    strictTransportSecurity,
    toWaiApplication,
    tracingExporter,
    trustForwardedHeaders,
  )
import qualified HarchWeb
import HarchWeb.Site
  ( siteName,
    siteRequestPolicy,
    siteRoutes,
    siteStaticAssets,
  )
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as WaiInternal
import Test.Hspec

spec :: Spec
spec =
  describe "Unit.App" $ do
    describe "twoPageSite" $ do
      it "keeps the example site wiring small and explicit" $ do
        siteName twoPageSite `shouldBe` "two-pages-example"
        length (siteRoutes twoPageSite) `shouldBe` 4
        staticAssetRoots (siteStaticAssets twoPageSite) `shouldBe` []
        staticAssetContentTypes (siteStaticAssets twoPageSite) `shouldBe` defaultStaticAssetContentTypes
        staticCacheControlSeconds (siteStaticAssets twoPageSite) `shouldBe` Nothing
        redirectHttpToHttps (siteRequestPolicy twoPageSite) `shouldBe` False
        httpsRedirectPort (siteRequestPolicy twoPageSite) `shouldBe` Nothing
        strictTransportSecurity (siteRequestPolicy twoPageSite) `shouldBe` Nothing
        trustForwardedHeaders (siteRequestPolicy twoPageSite) `shouldBe` False
        corsPolicy (siteRequestPolicy twoPageSite) `shouldBe` defaultCorsPolicyConfig
        responseSecurityHeaders (siteRequestPolicy twoPageSite) `shouldBe` defaultResponseSecurityHeadersConfig

      it "uses a minimal local server config without extra deployment concerns" $ do
        listenerConfigs twoPageServerConfig
          `shouldBe` [ ListenerConfig
                         { listenerHost = "127.0.0.1",
                           listenerPort = 8080,
                           listenerScheme = HarchWeb.Http,
                           listenerTls = Nothing,
                           listenerAcme = Nothing
                         }
                     ]
        staticAssetRoots (staticAssets twoPageServerConfig) `shouldBe` []
        staticAssetContentTypes (staticAssets twoPageServerConfig) `shouldBe` defaultStaticAssetContentTypes
        staticCacheControlSeconds (staticAssets twoPageServerConfig) `shouldBe` Nothing
        redirectHttpToHttps (requestPolicy twoPageServerConfig) `shouldBe` False
        httpsRedirectPort (requestPolicy twoPageServerConfig) `shouldBe` Nothing
        strictTransportSecurity (requestPolicy twoPageServerConfig) `shouldBe` Nothing
        trustForwardedHeaders (requestPolicy twoPageServerConfig) `shouldBe` False
        corsPolicy (requestPolicy twoPageServerConfig) `shouldBe` defaultCorsPolicyConfig
        responseSecurityHeaders (requestPolicy twoPageServerConfig) `shouldBe` defaultResponseSecurityHeadersConfig
        tracingExporter (observability twoPageServerConfig) `shouldBe` Nothing
        metricsExporter (observability twoPageServerConfig) `shouldBe` Nothing

    describe "routeCodec" $
      it "parses and renders the supported two-page routes" $ do
        eqViaDictionary HomeRoute HomeRoute `shouldBe` True
        eqViaDictionary HomeRoute SecondRoute `shouldBe` False
        eqViaDictionary NavigationScriptRoute NavigationScriptRoute `shouldBe` True
        neqViaDictionary HomeRoute SecondRoute `shouldBe` True
        showViaDictionary HomeRoute `shouldBe` "HomeRoute"
        showViaDictionary SecondRoute `shouldBe` "SecondRoute"
        showViaDictionary NavigationScriptRoute `shouldBe` "NavigationScriptRoute"
        showViaDictionary NotFoundRoute `shouldBe` "NotFoundRoute"
        showsPrecViaDictionary 0 HomeRoute "" `shouldBe` "HomeRoute"
        showListViaDictionary ([] :: [TwoPageRoute]) "" `shouldBe` "[]"
        showListViaDictionary [HomeRoute, SecondRoute] "" `shouldBe` "[HomeRoute, SecondRoute]"
        parseRoute ExampleRoutes.routeCodec () "/" `shouldBe` Just RouteRequest {requestRoute = HomeRoute, requestContext = ()}
        parseRoute ExampleRoutes.routeCodec () "/second" `shouldBe` Just RouteRequest {requestRoute = SecondRoute, requestContext = ()}
        parseRoute ExampleRoutes.routeCodec () "/missing" `shouldBe` Nothing
        renderRoute ExampleRoutes.routeCodec RouteRequest {requestRoute = HomeRoute, requestContext = ()} `shouldBe` "/"
        renderRoute ExampleRoutes.routeCodec RouteRequest {requestRoute = SecondRoute, requestContext = ()} `shouldBe` "/second"
        renderRoute ExampleRoutes.routeCodec RouteRequest {requestRoute = NavigationScriptRoute, requestContext = ()} `shouldBe` "/assets/navigation.js"
        renderRoute ExampleRoutes.routeCodec RouteRequest {requestRoute = NotFoundRoute, requestContext = ()} `shouldBe` "/404"
        routeHref HomeRoute `shouldBe` "/"
        routeHref SecondRoute `shouldBe` "/second"
        routeHref NavigationScriptRoute `shouldBe` "/assets/navigation.js"
        routeHref NotFoundRoute `shouldBe` "/404"
        notFoundRequest ExampleRoutes.routeCodec () `shouldBe` RouteRequest {requestRoute = NotFoundRoute, requestContext = ()}
        parseRoute ExampleRoutes.routeCodec () "/assets/navigation.js" `shouldBe` Just RouteRequest {requestRoute = NavigationScriptRoute, requestContext = ()}

    describe "buildApplication" $ do
      it "renders the home page with shared navigation and the enhancement runtime" $ do
        let application = buildApplication
        appName application `shouldBe` "two-pages-example"
        staticAssetRoots (applicationStaticAssets application) `shouldBe` []
        response <- performWaiRequest (toWaiApplication application) (waiRequest [])
        Wai.responseStatus response `shouldBe` Http.status200
        responseBody <- readResponseBody response
        Text.isInfixOf "<title>Home</title>" responseBody `shouldBe` True
        Text.isInfixOf "<nav data-navigation-region=\"primary\"><a href=\"/\" aria-current=\"page\">Home</a><a href=\"/second\">Second</a></nav>" responseBody `shouldBe` True
        Text.isInfixOf "<a href=\"/second\" data-page-link=\"true\">Go to the second page</a>" responseBody `shouldBe` True
        Text.isInfixOf "<script src=\"/assets/navigation.js\" defer></script>" responseBody `shouldBe` True

      it "renders the second page as full SSR HTML with bootstrap hooks" $ do
        response <- performWaiRequest (toWaiApplication buildApplication) (waiRequest ["second"])
        Wai.responseStatus response `shouldBe` Http.status200
        responseBody <- readResponseBody response
        Text.isInfixOf "<title>Second</title>" responseBody `shouldBe` True
        Text.isInfixOf "data-bootstrap-hooks=\"second-page\"" responseBody `shouldBe` True
        Text.isInfixOf "<a href=\"/\" data-page-link=\"true\">Back home</a>" responseBody `shouldBe` True

      it "renders not-found responses through the same shell with a 404 status" $ do
        response <- performWaiRequest (toWaiApplication buildApplication) (waiRequest ["missing"])
        Wai.responseStatus response `shouldBe` Http.status404
        responseBody <- readResponseBody response
        Text.isInfixOf "<title>Not Found</title>" responseBody `shouldBe` True
        Text.isInfixOf "<section data-page=\"not-found\">" responseBody `shouldBe` True
        Text.isInfixOf "<a href=\"/\">Home</a><a href=\"/second\">Second</a>" responseBody `shouldBe` True

      it "serves the navigation runtime through an unlabeled site route" $ do
        response <- performWaiRequest (toWaiApplication buildApplication) (waiRequest ["assets", "navigation.js"])
        Wai.responseStatus response `shouldBe` Http.status200
        lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just "application/javascript; charset=utf-8"
        responseBody <- readResponseBody response
        Text.isInfixOf "navigateTo" responseBody `shouldBe` True
        Text.isInfixOf "data-page-link=\"true\"" responseBody `shouldBe` True

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

eqViaDictionary :: Eq a => a -> a -> Bool
eqViaDictionary = (==)

{-# NOINLINE eqViaDictionary #-}

showViaDictionary :: Show a => a -> String
showViaDictionary = show

{-# NOINLINE showViaDictionary #-}

neqViaDictionary :: Eq a => a -> a -> Bool
neqViaDictionary = (/=)

{-# NOINLINE neqViaDictionary #-}

showsPrecViaDictionary :: Show a => Int -> a -> ShowS
showsPrecViaDictionary = showsPrec

{-# NOINLINE showsPrecViaDictionary #-}

showListViaDictionary :: Show a => [a] -> ShowS
showListViaDictionary = showList

{-# NOINLINE showListViaDictionary #-}
