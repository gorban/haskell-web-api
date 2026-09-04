{-# LANGUAGE OverloadedStrings #-}

module Unit.Catalog.DomainSpec (spec) where

import Catalog.Domain
import Data.List.NonEmpty (NonEmpty ((:|)))
import HarchWeb.Action qualified as Action
import HarchWeb.ApplicationModule (ApplicationModule (..))
import HarchWeb.Csrf (PageSecurity, mkCsrfToken, mkPageCsrf, mkPageSecurity)
import HarchWeb.Document (Page (..), testRuntimeNonce)
import HarchWeb.EndpointMetadata (AccessRequirement (RequireAuthorized), EndpointProtocol (ActionEndpoint, HtmlEndpoint), endpointAccess, endpointName, endpointNameText, endpointProtocol, endpointRouteTemplate, routeTemplateText)
import HarchWeb.Routing (RouteCodec (..), RouteLocation (..), RouteMethod (RouteGet), RouteParseResult (..), RouteRequest (..), requiredPathSegment, routeMethodPolicy)
import HarchWeb.Routing qualified as Routing
import HarchWeb.Server (ActionNavigation (StayOnCurrentRoute), ClientActionRequest (..), ClientActionResponse (..), PageResult (..), unboundedRouteExecutionPolicy)
import HarchWeb.Site (RouteDefinition (..), RouteHandler (..))
import HarchWeb.Site qualified as Site
import Network.HTTP.Types qualified as Http
import Test.Hspec

spec :: Spec
spec = describe "Unit.Catalog.Domain" $ do
  it "builds and executes its local query and command without root dependencies" $ do
    let catalogContext = CatalogContext "es" (Just "catalog.read")
        queries = CatalogQueries (\domainContext -> pure (catalogLocaleCode domainContext <> " summary"))
        commands = CatalogCommands (\domainContext -> (domainContext `shouldBe` catalogContext) >> pure ("refreshed for " <> catalogLocaleCode domainContext))
        moduleValue = buildCatalogModule queries commands
    CatalogIndex `shouldBe` CatalogIndex
    checkDerived CatalogIndex
    checkDerived RefreshCatalogTarget
    checkDerived RefreshCatalog
    checkDerived MayReadCatalog
    checkDerived MayRefreshCatalog
    catalogContext `shouldBe` CatalogContext "es" (Just "catalog.read")
    checkDerived catalogContext
    catalogLocaleCode catalogContext `shouldBe` "es"
    catalogViewerScope catalogContext `shouldBe` Just "catalog.read"
    loadCatalogSummary queries catalogContext `shouldReturn` "es summary"
    refreshCatalog commands catalogContext `shouldReturn` "refreshed for es"
    [MayReadCatalog, MayRefreshCatalog] `shouldBe` [MayReadCatalog, MayRefreshCatalog]
    moduleOwnsRoute moduleValue CatalogIndex `shouldBe` True
    show (moduleName moduleValue) `shouldBe` "ModuleName \"catalog\""
    moduleRouteMountChain moduleValue CatalogIndex `shouldBe` moduleName moduleValue :| []
    moduleDeclaredRoutes moduleValue `shouldBe` [CatalogIndex]
    case moduleGuards moduleValue of
      [] -> pure ()
      _ -> expectationFailure "catalog module should not install root-owned guards"
    parseRoute (moduleRouteCodec moduleValue) catalogContext (RouteLocation [] [])
      `shouldBe` RouteParsed (RouteRequest CatalogIndex catalogContext)
    parseRoute (moduleRouteCodec moduleValue) catalogContext (RouteLocation [requiredPathSegment "not-catalog"] [])
      `shouldBe` RouteNotMatched
    renderRoute (moduleRouteCodec moduleValue) (RouteRequest CatalogIndex catalogContext)
      `shouldBe` RouteLocation [] []
    notFoundRequest (moduleRouteCodec moduleValue) catalogContext
      `shouldBe` RouteRequest CatalogIndex catalogContext
    Routing.routeMethods (moduleRouteCodec moduleValue) CatalogIndex `shouldBe` routeMethodPolicy [RouteGet]
    Action.decodeAction
      (moduleActionCodec moduleValue)
      Action.ClientActionPayload
        { Action.clientActionMethod = "POST",
          Action.clientActionPath = "/actions/refresh",
          Action.clientActionFields = [],
          Action.clientActionCsrfToken = Nothing,
          Action.clientActionIdempotencyKey = Nothing,
          Action.clientActionPayloadContext = catalogContext
        }
      `shouldBe` Action.DecodedClientAction RefreshCatalog
    Action.actionPath (moduleActionCodec moduleValue) catalogContext RefreshCatalogTarget
      `shouldBe` Just "/actions/refresh"
    Action.staticActionPath (moduleActionCodec moduleValue) RefreshCatalogTarget
      `shouldBe` Just "/actions/refresh"
    moduleActionRoute moduleValue catalogContext RefreshCatalogTarget
      `shouldBe` Just CatalogIndex
    let definition = moduleEndpoints moduleValue CatalogIndex
    routeNavigationLabel definition `shouldBe` Just "Catalog"
    endpointProtocol (routeMetadata definition) `shouldBe` HtmlEndpoint
    endpointAccess (routeMetadata definition) `shouldBe` RequireAuthorized MayReadCatalog
    endpointNameText (endpointName (routeMetadata definition)) `shouldBe` "catalog.index"
    routeTemplateText (endpointRouteTemplate (routeMetadata definition)) `shouldBe` "/"
    Site.routeMethods definition `shouldBe` [RouteGet]
    routeExecutionPolicy definition `shouldBe` unboundedRouteExecutionPolicy
    case routeHandler definition of
      PageRouteHandler renderPage -> do
        response <- renderPage testPageSecurity (RouteRequest CatalogIndex catalogContext)
        case response of
          RenderedPage page -> do
            pageTitle page `shouldBe` "Catalog"
            pageRoute page `shouldBe` CatalogIndex
            pageContext page `shouldBe` catalogContext
            pageBootstrapHooks page `shouldBe` []
            show (pageBody page) `shouldBe` "\"<h1>es summary</h1>\""
          _ -> expectationFailure "expected catalog page"
      _ -> expectationFailure "expected a page route handler"
    actionResult <- moduleHandleAction moduleValue (ClientActionRequest RefreshCatalog Nothing catalogContext)
    case actionResult of
      Nothing -> expectationFailure "catalog action must produce a response"
      Just actionResponse -> do
        clientActionStatus actionResponse `shouldBe` Http.status200
        clientActionPatches actionResponse `shouldBe` []
        clientActionFocusId actionResponse `shouldBe` Nothing
        clientActionNavigation actionResponse `shouldBe` StayOnCurrentRoute
        clientActionHeaders actionResponse `shouldBe` []
        clientActionObservabilityAttributes actionResponse `shouldBe` []
        clientActionLogEntries actionResponse `shouldBe` []
    case Action.declaredActionEndpointMetadata (moduleActionCodec moduleValue) of
      [actionMetadata] -> do
        endpointNameText (endpointName actionMetadata) `shouldBe` "catalog.refresh"
        routeTemplateText (endpointRouteTemplate actionMetadata) `shouldBe` "/actions/refresh"
        endpointProtocol actionMetadata `shouldBe` ActionEndpoint
        endpointAccess actionMetadata `shouldBe` RequireAuthorized MayRefreshCatalog
      _ -> expectationFailure "catalog module must declare exactly one action endpoint"

testPageSecurity :: PageSecurity
testPageSecurity =
  mkPageSecurity testRuntimeNonce (mkPageCsrf testCsrfToken "catalog")
  where
    testCsrfToken =
      case mkCsrfToken "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" of
        Just csrfToken -> csrfToken
        Nothing -> error "invalid test CSRF token"

checkDerived :: (Eq value, Show value) => value -> Expectation
checkDerived value = do
  value == value `shouldBe` True
  value /= value `shouldBe` False
  shows value "" `shouldBe` show value
  showsPrec 11 value "" `shouldSatisfy` (not . null)
  showList [value] "" `shouldSatisfy` (not . null)
