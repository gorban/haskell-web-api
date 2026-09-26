{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Data.ByteString.Lazy.Char8 qualified as LazyByteString
import Data.Map.Strict qualified as Map
import HarchWeb.EndpointMetadata (AccessRequirement (AllowUnauthenticated), EndpointMetadata, EndpointProtocol (ApiEndpoint), mkEndpointMetadata, requiredEndpointNameOrDie, requiredRouteTemplateOrDie)
import HarchWeb.OpenApi
import HarchWeb.Routing (RouteMethod (RouteGet), RouteRequest (..), routeMethodPolicy)
import HarchWeb.Server (NonPageResponse (NonPageProtocolResponse), ProtocolResponse (..), ProtocolResponseBody (ProtocolResponseBytes), unboundedRouteExecutionPolicy)
import HarchWeb.Site (RouteDefinition (..), RouteHandler (ProtocolRouteHandler))
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai

spec =
  describe "OpenAPI document route" $ do
    it "uses the shared GET route policy and serves provider bytes as OpenAPI JSON" $ do
      provider <- requireRight (mkCachedOpenApiDocumentProvider (OpenApiDocumentDetails "Catalog API" "1.0") Map.empty () [])
      let definition = openApiDocumentRouteDefinition documentMetadata provider :: RouteDefinition () () ()
      routeNavigationLabel definition `shouldBe` Nothing
      routeMetadata definition `shouldBe` documentMetadata
      routeMethods definition (RouteRequest () ()) `shouldBe` routeMethodPolicy [RouteGet]
      routeExecutionPolicy definition `shouldBe` unboundedRouteExecutionPolicy
      response <- runProtocolRoute definition ()
      case response of
        NonPageProtocolResponse protocolResponse@ProtocolResponse {protocolResponseStatus, protocolResponseHeaders, protocolResponseBody = ProtocolResponseBytes bytes} -> do
          protocolResponseStatus `shouldBe` Http.status200
          protocolResponseHeaders `shouldBe` [(Http.hContentType, "application/vnd.oai.openapi+json;version=3.0")]
          LazyByteString.unpack (LazyByteString.fromStrict bytes) `shouldContain` "\"openapi\":\"3.0.3\""
          protocolResponseObservabilityAttributes protocolResponse `shouldBe` []
          protocolResponseLogEntries protocolResponse `shouldBe` []
          protocolResponseDatabaseOperations protocolResponse `shouldBe` []
        _ -> expectationFailure "expected an OpenAPI byte response"

    it "does not expose a dynamic provider failure" $ do
      let provider = OpenApiDocumentProvider (const (pure (Left EmptyOpenApiDocumentTitle)))
          definition = openApiDocumentRouteDefinition documentMetadata provider :: RouteDefinition () () ()
      response <- runProtocolRoute definition ()
      case response of
        NonPageProtocolResponse protocolResponse@ProtocolResponse {protocolResponseStatus, protocolResponseHeaders, protocolResponseBody = ProtocolResponseBytes bytes} -> do
          protocolResponseStatus `shouldBe` Http.status503
          protocolResponseHeaders `shouldBe` [(Http.hContentType, "text/plain; charset=utf-8"), ("Cache-Control", "no-store")]
          bytes `shouldBe` "OpenAPI document unavailable\n"
          protocolResponseObservabilityAttributes protocolResponse `shouldBe` []
          protocolResponseLogEntries protocolResponse `shouldBe` []
          protocolResponseDatabaseOperations protocolResponse `shouldBe` []
        _ -> expectationFailure "expected a safe unavailable response"

documentMetadata :: EndpointMetadata ()
documentMetadata =
  mkEndpointMetadata
    (requiredEndpointNameOrDie "catalog.open-api-document")
    (requiredRouteTemplateOrDie "/docs/openapi.json")
    ApiEndpoint
    AllowUnauthenticated

runProtocolRoute :: RouteDefinition () context authorization -> context -> IO (NonPageResponse () context)
runProtocolRoute definition requestContextValue =
  case routeHandler definition of
    ProtocolRouteHandler renderProtocol -> renderProtocol Wai.defaultRequest (RouteRequest () requestContextValue)
    _ -> expectationFailure "expected a protocol route" >> fail "expected a protocol route"

requireRight :: Either failure value -> IO value
requireRight result =
  case result of
    Right value -> pure value
    Left _ -> expectationFailure "expected a valid provider" >> fail "expected a valid provider"
