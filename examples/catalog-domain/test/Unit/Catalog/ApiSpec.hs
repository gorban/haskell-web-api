module Unit.Catalog.ApiSpec (spec) where

import Catalog.Api
import Catalog.Domain (CatalogContext (..), CatalogPolicy (MayReadCatalog), CatalogQueries (..), catalogLocaleCode)
import Data.List.NonEmpty (NonEmpty ((:|)))
import HarchWeb.Action qualified as Action
import HarchWeb.Api
  ( ApiAvailability (ApiAvailable, ApiHidden),
    ApiEndpointContract (..),
    ApiEndpointRequest (..),
    ApiFieldFailurePolicy (ApiRenderFieldFailures, ApiRenderFieldFailuresWithStatus, ApiUseGenericFieldFailure),
    ApiMethod (ApiGet),
    ApiRequestBody (ApiNoRequestBody),
    ApiRequestData (..),
    ApiRequestDecodeResult (..),
    ApiResponse (..),
    ApiRouteEndpointDeclaration (..),
    NoApiExtension (..),
    SomeApiRouteEndpoint (..),
    apiContentType,
    apiPathText,
    apiResponseEncoderContentType,
    apiRouteEndpointAvailability,
    jsonMediaType,
    mapApiEndpointFamily,
    runRequestCodec,
    withApiRouteEndpointDeclaration,
  )
import HarchWeb.ApplicationModule (ApplicationModule (..))
import HarchWeb.EndpointMetadata (AccessRequirement (AllowUnauthenticated, RequireAuthorized), EndpointProtocol (ApiEndpoint), endpointAccess, endpointName, endpointNameText, endpointProtocol, endpointRouteTemplate, routeTemplateText)
import HarchWeb.Routing (RouteCodec (..), RouteLocation (..), RouteMethod (RouteGet), RouteParseResult (..), RouteRequest (..), requiredPathSegment, routeMethodPolicy)
import HarchWeb.Routing qualified as Routing
import HarchWeb.Server (NonPageResponse (..), ProtocolResponse (..), ProtocolResponseBody (..), unboundedRouteExecutionPolicy)
import HarchWeb.Site (RouteDefinition (..), RouteHandler (..))
import HarchWeb.Site qualified as Site
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import Test.Hspec

spec :: Spec
spec = describe "Unit.Catalog.Api" $ do
  it "declares its typed GET contract and summary-derived JSON response" $ do
    let contract = catalogItemsApiContract NoApiExtension
        queries = CatalogQueries (\domainContext -> pure (catalogLocaleCode domainContext <> " summary"))
        catalogContext = CatalogContext "en" (Just "catalog.read")
    case apiEndpointContractMethod contract of
      ApiGet -> pure ()
      _ -> expectationFailure "catalog items must be a GET"
    case apiEndpointContractBody contract of
      ApiNoRequestBody -> pure ()
      _ -> expectationFailure "catalog items must take no request body"
    case apiEndpointContractFieldFailurePolicy contract of
      ApiUseGenericFieldFailure -> pure ()
      ApiRenderFieldFailures _ -> expectationFailure "the contract must use the generic field failure"
      ApiRenderFieldFailuresWithStatus _ -> expectationFailure "the contract must use the generic field failure"
    case apiEndpointContractEncoders contract of
      responseEncoder :| _ ->
        apiResponseEncoderContentType responseEncoder `shouldBe` apiContentType jsonMediaType
    case runRequestCodec (apiEndpointContractFields contract) (ApiRequestData [] [] [] []) of
      ApiRequestDecoded () -> pure ()
      _ -> expectationFailure "catalog items take no request fields"
    response <- catalogItemsApiHandler NoApiExtension queries catalogContext (ApiEndpointRequest () ())
    apiEndpointResponseStatus response `shouldBe` Http.status200
    apiEndpointResponseValue response `shouldBe` "{\"summary\":\"en summary\"}"
    checkDerived CatalogItems

  it "builds its module and executes the items endpoint through the route definition" $ do
    let queries = CatalogQueries (\domainContext -> pure (catalogLocaleCode domainContext <> " summary"))
        moduleValue = buildCatalogApiModule NoApiExtension queries
        catalogContext = CatalogContext "en" Nothing
    show (moduleName moduleValue) `shouldBe` "ModuleName \"catalog.api\""
    moduleOwnsRoute moduleValue CatalogItems `shouldBe` True
    moduleRouteMountChain moduleValue CatalogItems `shouldBe` moduleName moduleValue :| []
    moduleDeclaredRoutes moduleValue `shouldBe` [CatalogItems, CatalogUnlistedPreview, CatalogApiNotFound]
    case moduleGuards moduleValue of
      [] -> pure ()
      _ -> expectationFailure "catalog api module must not install guards"
    case Action.declaredActionEndpointMetadata (moduleActionCodec moduleValue) of
      [] -> pure ()
      _ -> expectationFailure "catalog api module must declare no action endpoints"
    parseRoute (moduleRouteCodec moduleValue) catalogContext (RouteLocation [requiredPathSegment "items"] [])
      `shouldBe` RouteParsed (RouteRequest CatalogItems catalogContext)
    parseRoute (moduleRouteCodec moduleValue) catalogContext (RouteLocation [] [])
      `shouldBe` RouteParsed (RouteRequest CatalogApiNotFound catalogContext)
    parseRoute (moduleRouteCodec moduleValue) catalogContext (RouteLocation [requiredPathSegment "other"] [])
      `shouldBe` RouteParsed (RouteRequest CatalogApiNotFound catalogContext)
    parseRoute (moduleRouteCodec moduleValue) catalogContext (RouteLocation [requiredPathSegment "items", requiredPathSegment "extra"] [])
      `shouldBe` RouteParsed (RouteRequest CatalogApiNotFound catalogContext)
    parseRoute (moduleRouteCodec moduleValue) catalogContext (RouteLocation [requiredPathSegment "unlisted-preview"] [])
      `shouldBe` RouteParsed (RouteRequest CatalogUnlistedPreview catalogContext)
    moduleOwnsRoute moduleValue CatalogUnlistedPreview `shouldBe` True
    checkDerived CatalogUnlistedPreview
    checkDerived CatalogApiNotFound
    renderRoute (moduleRouteCodec moduleValue) (RouteRequest CatalogItems catalogContext)
      `shouldBe` RouteLocation [requiredPathSegment "items"] []
    renderRoute (moduleRouteCodec moduleValue) (RouteRequest CatalogUnlistedPreview catalogContext)
      `shouldBe` RouteLocation [requiredPathSegment "unlisted-preview"] []
    renderRoute (moduleRouteCodec moduleValue) (RouteRequest CatalogApiNotFound catalogContext)
      `shouldBe` RouteLocation [requiredPathSegment "404"] []
    notFoundRequest (moduleRouteCodec moduleValue) catalogContext
      `shouldBe` RouteRequest CatalogApiNotFound catalogContext
    Routing.routeMethods (moduleRouteCodec moduleValue) (RouteRequest CatalogItems catalogContext) `shouldBe` routeMethodPolicy [RouteGet]
    Routing.routeMethods (moduleRouteCodec moduleValue) (RouteRequest CatalogUnlistedPreview catalogContext) `shouldBe` routeMethodPolicy []
    Routing.routeMethods (moduleRouteCodec moduleValue) (RouteRequest CatalogApiNotFound catalogContext) `shouldBe` routeMethodPolicy []
    -- The API module's action algebra is uninhabited, so both projections are
    -- constant on values that cannot exist; the tests supply bottom stand-ins
    -- to pin that constant behavior.
    moduleActionRoute moduleValue catalogContext (error "CatalogApiActionTarget is uninhabited") `shouldBe` Nothing
    actionResult <- moduleHandleAction moduleValue (error "CatalogApiAction cannot be requested")
    case actionResult of
      Nothing -> pure ()
      Just _ -> expectationFailure "catalog api module must not handle actions"
    let definition = moduleEndpoints moduleValue CatalogItems
    routeNavigationLabel definition `shouldBe` Nothing
    endpointNameText (endpointName (routeMetadata definition)) `shouldBe` "catalog.items"
    routeTemplateText (endpointRouteTemplate (routeMetadata definition)) `shouldBe` "/items"
    endpointProtocol (routeMetadata definition) `shouldBe` ApiEndpoint
    endpointAccess (routeMetadata definition) `shouldBe` RequireAuthorized MayReadCatalog
    Site.routeMethods definition (RouteRequest CatalogItems catalogContext) `shouldBe` routeMethodPolicy [RouteGet]
    let hiddenDefinition = moduleEndpoints moduleValue CatalogUnlistedPreview
    endpointNameText (endpointName (routeMetadata hiddenDefinition)) `shouldBe` "catalog.unlisted-preview"
    routeTemplateText (endpointRouteTemplate (routeMetadata hiddenDefinition)) `shouldBe` "/unlisted-preview"
    endpointAccess (routeMetadata hiddenDefinition) `shouldBe` RequireAuthorized MayReadCatalog
    case routeHandler hiddenDefinition of
      ProtocolRouteHandler _ -> pure ()
      PageRouteHandler _ -> expectationFailure "the unlisted preview is a protocol endpoint"
    let notFoundDefinition = moduleEndpoints moduleValue CatalogApiNotFound
    routeNavigationLabel notFoundDefinition `shouldBe` Nothing
    routeExecutionPolicy notFoundDefinition `shouldBe` unboundedRouteExecutionPolicy
    endpointNameText (endpointName (routeMetadata notFoundDefinition)) `shouldBe` "catalog.not-found"
    routeTemplateText (endpointRouteTemplate (routeMetadata notFoundDefinition)) `shouldBe` "/404"
    endpointProtocol (routeMetadata notFoundDefinition) `shouldBe` ApiEndpoint
    endpointAccess (routeMetadata notFoundDefinition) `shouldBe` AllowUnauthenticated
    Site.routeMethods notFoundDefinition (RouteRequest CatalogApiNotFound catalogContext) `shouldBe` routeMethodPolicy []
    case routeHandler notFoundDefinition of
      ProtocolRouteHandler renderNotFound -> do
        notFoundResponse <- renderNotFound Wai.defaultRequest (RouteRequest CatalogApiNotFound catalogContext)
        case notFoundResponse of
          NonPageProtocolResponse protocolResponse -> do
            protocolResponseStatus protocolResponse `shouldBe` Http.status404
            protocolResponseHeaders protocolResponse `shouldBe` []
            case protocolResponseBody protocolResponse of
              ProtocolResponseBytes bodyBytes -> bodyBytes `shouldBe` ""
              _ -> expectationFailure "expected strict protocol bytes"
          _ -> expectationFailure "expected a protocol response"
      PageRouteHandler _ -> expectationFailure "the catalog api not-found route is a protocol endpoint"
    case routeHandler definition of
      ProtocolRouteHandler renderProtocol -> do
        nonPage <- renderProtocol Wai.defaultRequest (RouteRequest CatalogItems catalogContext)
        case nonPage of
          NonPageProtocolResponse protocolResponse -> do
            protocolResponseStatus protocolResponse `shouldBe` Http.status200
            case protocolResponseBody protocolResponse of
              ProtocolResponseBytes bodyBytes -> bodyBytes `shouldBe` "{\"summary\":\"en summary\"}"
              _ -> expectationFailure "expected strict protocol bytes"
          _ -> expectationFailure "expected a protocol response"
      PageRouteHandler _ -> expectationFailure "catalog items is a protocol endpoint"

  it "exposes its endpoint and documented family over the summary port" $ do
    let queries = CatalogQueries (\domainContext -> pure (catalogLocaleCode domainContext <> " summary"))
    case catalogItemsApiEndpoint NoApiExtension queries of
      SomeApiRouteEndpoint _ -> pure ()
    case catalogApiFamily NoApiExtension queries of
      Left _ -> expectationFailure "catalog api family must validate"
      Right family -> do
        mapApiEndpointFamily (\endpoint -> apiRouteEndpointAvailability endpoint (CatalogContext "en" Nothing)) family
          `shouldBe` [ApiAvailable, ApiHidden]
        mapApiEndpointFamily
          ( \endpoint ->
              withApiRouteEndpointDeclaration endpoint $ \declaration ->
                ( apiPathText (apiRouteEndpointDeclarationPath declaration),
                  apiEndpointContractMethod (apiRouteEndpointDeclarationContract declaration)
                )
          )
          family
          `shouldBe` [("/items", ApiGet), ("/unlisted-preview", ApiGet)]

checkDerived :: (Eq value, Show value) => value -> Expectation
checkDerived value = do
  value == value `shouldBe` True
  value /= value `shouldBe` False
  shows value "" `shouldBe` show value
  showsPrec 11 value "" `shouldSatisfy` (not . null)
  showList [value] "" `shouldSatisfy` (not . null)
