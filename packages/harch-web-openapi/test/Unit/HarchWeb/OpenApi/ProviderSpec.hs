{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Data.ByteString.Lazy.Char8 qualified as ByteString
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import HarchWeb.Api qualified as Api
import HarchWeb.ApplicationModule (RouteMount (..))
import HarchWeb.EndpointMetadata (AccessRequirement (AllowUnauthenticated), EndpointMetadata, EndpointProtocol (ApiEndpoint), mkEndpointMetadata, requiredEndpointNameOrDie, requiredRouteTemplateOrDie)
import HarchWeb.OpenApi
import HarchWeb.Routing (requiredPathSegment)
import HarchWeb.SecurityEvent (requiredModuleNameOrDie)

spec =
  describe "OpenAPI document providers" $ do
    it "prepares validated snapshot bytes for explicit dynamic providers" $ do
      prepared <-
        requireRight
          ( prepareOpenApiDocumentFromSnapshot
              (OpenApiDocumentDetails "Catalog API" "1.0")
              Map.empty
              True
              [openApiMountedFamily catalogMount (family [endpoint "/items"]) anonymousEndpointMetadata (const [])]
          )
      ByteString.unpack (preparedOpenApiDocumentBytes prepared) `shouldContain` "\"/api/catalog/items\""

    it "returns one immutable document prepared from its construction snapshot" $ do
      provider <-
        requireRight
          ( mkCachedOpenApiDocumentProvider
              (OpenApiDocumentDetails "Catalog API" "1.0")
              Map.empty
              True
              [openApiMountedFamily catalogMount (family [availabilityEndpoint "/items"]) anonymousEndpointMetadata (const [])]
          )
      enabled <- prepareOpenApiDocument provider True
      disabled <- prepareOpenApiDocument provider False
      case (enabled, disabled) of
        (Right enabledDocument, Right disabledDocument) -> do
          let enabledBytes = preparedOpenApiDocumentBytes enabledDocument
          expectAll
            ( (enabledBytes `shouldBe` preparedOpenApiDocumentBytes disabledDocument)
                :| [ByteString.unpack enabledBytes `shouldContain` "\"/api/catalog/items\""]
            )
        (Left _, _) -> expectationFailure "expected the cached provider to prepare its document"
        (_, Left _) -> expectationFailure "expected the cached provider to prepare its document"

    it "fails construction before a malformed default provider can serve a document" $
      expectDocumentFailure
        (mkCachedOpenApiDocumentProvider (OpenApiDocumentDetails "" "1.0") Map.empty True [openApiMountedFamily catalogMount (family [endpoint "/items"]) anonymousEndpointMetadata (const [])])
        EmptyOpenApiDocumentTitle

    it "lets an application-owned provider report a typed runtime failure" $ do
      let provider = OpenApiDocumentProvider (const (pure (Left EmptyOpenApiDocumentVersion)))
      result <- prepareOpenApiDocument provider True
      expectDocumentFailure result EmptyOpenApiDocumentVersion

catalogMount :: RouteMount () ()
catalogMount =
  RouteMount
    { routeMountName = requiredModuleNameOrDie "root.catalog-api",
      routeMountPrefix = requiredPathSegment "api" :| [requiredPathSegment "catalog"],
      embedChildRoute = id,
      projectChildRoute = Just
    }

family :: [Api.ApiRouteEndpoint Bool OpenApiExtension fields body domainFailure response] -> Api.ApiEndpointFamily Bool OpenApiExtension
family endpoints = Api.requireApiEndpointFamily (map Api.SomeApiRouteEndpoint endpoints)

anonymousEndpointMetadata :: Api.ApiPath -> EndpointMetadata ()
anonymousEndpointMetadata _ =
  mkEndpointMetadata
    (requiredEndpointNameOrDie "catalog.item")
    (requiredRouteTemplateOrDie "/items")
    ApiEndpoint
    AllowUnauthenticated

endpoint :: Text -> Api.ApiRouteEndpoint Bool OpenApiExtension () () () Text
endpoint = availabilityEndpoint

availabilityEndpoint :: Text -> Api.ApiRouteEndpoint Bool OpenApiExtension () () () Text
availabilityEndpoint path =
  Api.withApiEndpointAvailabilityFromContext
    (\enabled -> if enabled then Api.ApiAvailable else Api.ApiHidden)
    ( Api.apiRouteEndpointNeverFailing
        (Api.ApiRouteEndpointDeclaration (Api.at path) (Api.ApiEndpointContract Api.ApiGet Api.noRequestFields Api.ApiNoRequestBody (Api.textResponseEncoder :| []) Api.ApiUseGenericFieldFailure emptyOpenApiExtension))
        (const (pure (Api.apiResponse "visible")))
    )

requireRight :: Either errorValue value -> IO value
requireRight result =
  case result of
    Left _ -> expectationFailure "expected Right" >> fail "expected Right"
    Right value -> pure value

expectDocumentFailure :: Either OpenApiDocumentFailure value -> OpenApiDocumentFailure -> Expectation
expectDocumentFailure result expectedFailure =
  case result of
    Left actualFailure -> renderOpenApiDocumentFailure actualFailure `shouldBe` renderOpenApiDocumentFailure expectedFailure
    Right _ -> expectationFailure "expected document construction to fail"
