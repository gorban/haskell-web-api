{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Exception (ErrorCall (..))
import HarchWeb.EndpointMetadata (AccessRequirement (AllowUnauthenticated), EndpointMetadata, EndpointProtocol (AssetEndpoint), mkEndpointMetadata, requiredEndpointNameOrDie, requiredRouteTemplateOrDie)
import HarchWeb.Routing (RouteCodec (..), RouteLocation (..), RouteMethod (RouteGet), RouteParseResult (..), RouteRequest (..), requiredPathSegment, routeMethodPolicy)
import HarchWeb.Routing qualified as Routing
import HarchWeb.Server (ProtocolResponse (..), ProtocolResponseBody (ProtocolResponseWai), Response (ProtocolResponseResult), unboundedRouteExecutionPolicy)
import HarchWeb.Site (RouteDefinition (..))
import HarchWeb.Site qualified as Site
import HarchWeb.StaticAssets (StaticAssetRoot (..), StaticAssetsConfig (..), defaultStaticAssetContentTypes)
import HarchWeb.StaticAssets.Route
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import System.Directory (createDirectoryIfMissing)
import System.IO.Temp (withSystemTempDirectory)

spec = describe "Unit.HarchWeb.StaticAssets.Route" $ do
  it "owns configured structured asset paths and delegates their file response" $
    withSystemTempDirectory "harch-web-static-route" $ \temporaryDirectory -> do
      let assetDirectory = temporaryDirectory <> "/assets"
          staticAssets =
            StaticAssetsConfig
              { staticAssetRoots = [StaticAssetRoot "/assets" assetDirectory],
                staticAssetContentTypes = defaultStaticAssetContentTypes,
                staticCacheControlSeconds = Nothing
              }
          codec = staticAssetRouteCodec staticAssets
          location = RouteLocation [requiredPathSegment "assets", requiredPathSegment "app.css"] []
          metadata :: EndpointMetadata ()
          metadata =
            mkEndpointMetadata
              (requiredEndpointNameOrDie "assets.public")
              (requiredRouteTemplateOrDie "/assets/*")
              AssetEndpoint
              AllowUnauthenticated
      createDirectoryIfMissing True assetDirectory
      writeFile (assetDirectory <> "/app.css") "body{}"
      parseRoute codec () location `shouldBe` RouteParsed (RouteRequest (StaticAssetRoute (routePathSegments location)) ())
      parseRoute codec () (RouteLocation [requiredPathSegment "orders"] []) `shouldBe` RouteNotMatched
      renderRoute codec (RouteRequest (StaticAssetRoute (routePathSegments location)) ()) `shouldBe` location
      notFoundRequest codec () `shouldBe` RouteRequest (StaticAssetRoute []) ()
      Routing.routeMethods codec (StaticAssetRoute (routePathSegments location)) `shouldBe` routeMethodPolicy [RouteGet]
      let definition = staticAssetRouteDefinition staticAssets metadata
      routeNavigationLabel definition `shouldBe` Nothing
      routeMetadata definition `shouldBe` metadata
      Site.routeMethods definition `shouldBe` [RouteGet]
      routeExecutionPolicy definition `shouldBe` unboundedRouteExecutionPolicy
      response <- routeResponse definition Wai.defaultRequest (RouteRequest (StaticAssetRoute (routePathSegments location)) ())
      case response of
        ProtocolResponseResult protocolResponse -> do
          protocolResponseStatus protocolResponse `shouldBe` Http.status200
          case protocolResponseBody protocolResponse of
            ProtocolResponseWai waiResponse -> Wai.responseStatus waiResponse `shouldBe` Http.status200
            _ -> expectationFailure "expected the file-backed framework response"
        _ -> expectationFailure "expected a protocol response from the static route"
      directResponse <- staticAssetRouteResponse staticAssets Wai.defaultRequest (StaticAssetRoute (routePathSegments location))
      protocolResponseStatus directResponse `shouldBe` Http.status200
      protocolResponseHeaders directResponse `shouldBe` []
      protocolResponseObservabilityAttributes directResponse `shouldBe` []
      protocolResponseLogEntries directResponse `shouldBe` []
      protocolResponseDatabaseOperations directResponse `shouldBe` []
      StaticAssetRoute [requiredPathSegment "assets"] `shouldBe` StaticAssetRoute [requiredPathSegment "assets"]
      StaticAssetRoute [requiredPathSegment "assets"] `shouldNotBe` StaticAssetRoute [requiredPathSegment "orders"]
      show (StaticAssetRoute [requiredPathSegment "assets"])
        `shouldBe` "StaticAssetRoute {staticAssetPathSegments = [PathSegment \"assets\"]}"
      showsPrec 11 (StaticAssetRoute [requiredPathSegment "assets"]) ""
        `shouldSatisfy` (not . null)
      showList [StaticAssetRoute [requiredPathSegment "assets"]] ""
        `shouldSatisfy` (not . null)
      staticAssetRouteResponse (staticAssets {staticAssetRoots = []}) Wai.defaultRequest (StaticAssetRoute [requiredPathSegment "orders"])
        `shouldThrow` \case
          ErrorCall message -> message == "selected static route was not owned by its configured static assets"
