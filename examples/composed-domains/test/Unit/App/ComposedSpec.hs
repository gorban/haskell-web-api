{-# LANGUAGE OverloadedStrings #-}

module Unit.App.ComposedSpec (spec) where

import App.Composed
import Catalog.Domain
import Control.Exception (ErrorCall, evaluate, try)
import Data.ByteString qualified as ByteString
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Action qualified as Action
import HarchWeb.ApplicationModule (ApplicationModule (..), mountApplicationModule)
import HarchWeb.Document (NavigationItem (..), Page (..), PageShell (..))
import HarchWeb.EndpointMetadata
  ( AccessRequirement (AllowUnauthenticated, RequireAuthorized),
    EndpointName,
    EndpointProtocol (AssetEndpoint, HtmlEndpoint),
    endpointAccess,
    endpointName,
    endpointProtocol,
    endpointRouteTemplate,
    requiredEndpointNameOrDie,
    routeTemplateText,
  )
import HarchWeb.EndpointSecurity
  ( ApplicationSecurity (AuthenticationDisabled, AuthenticationEnabled),
    AuthenticationGuard (..),
    EndpointDispatchKind (EndpointMatched, EndpointOptions),
    EndpointGuard (..),
    EndpointGuardResult (..),
    EndpointRequest (..),
  )
import HarchWeb.Localization (locale)
import HarchWeb.Markup (literalElementId, renderHtml)
import HarchWeb.RequestContext
  ( CoreRequestContext (..),
    RequestContext (..),
    RequestIdentity (..),
    requestLocale,
  )
import HarchWeb.Routing
  ( RouteCodec (..),
    RouteDecodeError (InvalidRouteTargetEncoding),
    RouteLocation (..),
    RouteParseResult (..),
    RouteRequest (..),
    requiredPathSegment,
  )
import HarchWeb.Routing qualified as Routing
import HarchWeb.SecurityEvent (ModuleName, RouteObservation (..), mkModuleName)
import HarchWeb.Server
  ( ClientActionRequest (..),
    ClientActionResponse (..),
    ProtocolResponse (..),
    ProtocolResponseBody (..),
    Response (..),
    ResponseBody (..),
    toWaiApplication,
    unboundedRouteExecutionPolicy,
  )
import HarchWeb.Site (RouteDefinition (..))
import HarchWeb.Site qualified as Site
import HarchWeb.StaticAssets
  ( StaticAssetRoot (..),
    staticAssetContentTypes,
    staticAssetRoots,
    staticCacheControlSeconds,
  )
import HarchWeb.StaticAssets.Route (StaticAssetRoute (..))
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import Orders.Domain
import Test.Hspec
import TestCore.CustomAssertions (expectAll)
import TestCore.Wai (performWaiRequest, readResponseBody, waiRequest)

spec :: Spec
spec = describe "Unit.App.Composed" $ do
  it "uses explicit, durable, cookie, header, and default locale precedence" $ do
    let anonymous = AnonymousIdentity
        authenticated = AuthenticatedIdentity (RootPrincipal (Just (locale "es")) ["catalog.read"])
    resolveLocale defaultLocalePolicy (LocaleResolutionInput (Just (locale "en")) (Just "es") (Just "es-MX") authenticated)
      `shouldBe` locale "en"
    resolveLocale defaultLocalePolicy (LocaleResolutionInput Nothing (Just "en") (Just "en-US") authenticated)
      `shouldBe` locale "es"
    resolveLocale defaultLocalePolicy (LocaleResolutionInput Nothing (Just "es") (Just "en-US") anonymous)
      `shouldBe` locale "es"
    resolveLocale defaultLocalePolicy (LocaleResolutionInput Nothing Nothing (Just "es-MX,en;q=0.8") anonymous)
      `shouldBe` locale "es"
    resolveLocale defaultLocalePolicy (LocaleResolutionInput Nothing Nothing (Just "fr-CA") anonymous)
      `shouldBe` locale "en"
    supportedLocales defaultLocalePolicy `shouldBe` locale "en" :| [locale "es"]
    defaultLocale defaultLocalePolicy `shouldBe` locale "en"
    let equivalentPolicy = LocalePolicy (locale "en" :| [locale "es"]) (locale "en")
        resolutionInput = LocaleResolutionInput Nothing (Just "es") (Just "en-US") anonymous
    equivalentPolicy `shouldBe` defaultLocalePolicy
    show equivalentPolicy `shouldBe` show defaultLocalePolicy
    resolutionInput `shouldBe` resolutionInput
    show resolutionInput `shouldBe` "LocaleResolutionInput {localeExplicitPrefix = Nothing, localeCookieValue = Just \"es\", localeAcceptLanguage = Just \"en-US\", localeIdentity = AnonymousIdentity}"

  it "mounts independent local routes/actions under an allowlisted locale root" $ do
    rootModule <- requiredRootModule
    let rootContext = defaultComposedContext
        spanishCatalog = RouteLocation [requiredPathSegment "es", requiredPathSegment "catalog"] []
    ordersOnlyModule <-
      case mountApplicationModule ordersModuleMount (buildOrdersModule ordersQueries ordersCommands) of
        Left mountError -> expectationFailure (show mountError) >> fail "could not mount the Orders module"
        Right mountedModule -> pure mountedModule
    moduleHandleAction ordersOnlyModule (ClientActionRequest (CatalogAction RefreshCatalog) Nothing rootContext)
      `shouldReturn` Nothing
    case parseRoute (moduleRouteCodec rootModule) rootContext spanishCatalog of
      RouteParsed request -> do
        requestRoute request `shouldBe` Localized (locale "es") (Catalog CatalogIndex)
        requestLocale (requestCore (requestContext request)) `shouldBe` locale "es"
      RouteNotMatched -> expectationFailure "expected the Spanish catalog route"
      RouteMalformed routeError -> expectationFailure (show routeError)
    routePathSegments (renderRoute (moduleRouteCodec rootModule) (RouteRequest (Localized (locale "es") (Orders OrdersIndex)) rootContext))
      `shouldBe` [requiredPathSegment "es", requiredPathSegment "orders"]
    Action.decodeAction
      (moduleActionCodec rootModule)
      Action.ClientActionPayload
        { Action.clientActionMethod = "POST",
          Action.clientActionPath = "/es/catalog/actions/refresh",
          Action.clientActionFields = [],
          Action.clientActionCsrfToken = Nothing,
          Action.clientActionIdempotencyKey = Nothing,
          Action.clientActionPayloadContext = spanishContext rootContext
        }
      `shouldBe` Action.DecodedClientAction (CatalogAction RefreshCatalog)
    Action.decodeAction
      (moduleActionCodec rootModule)
      Action.ClientActionPayload
        { Action.clientActionMethod = "POST",
          Action.clientActionPath = "/es/orders/actions/submit",
          Action.clientActionFields = [],
          Action.clientActionCsrfToken = Nothing,
          Action.clientActionIdempotencyKey = Nothing,
          Action.clientActionPayloadContext = spanishContext rootContext
        }
      `shouldBe` Action.DecodedClientAction (OrdersAction SubmitOrder)
    Action.decodeAction
      (moduleActionCodec rootModule)
      Action.ClientActionPayload
        { Action.clientActionMethod = "POST",
          Action.clientActionPath = "/es/unknown/actions/missing",
          Action.clientActionFields = [],
          Action.clientActionCsrfToken = Nothing,
          Action.clientActionIdempotencyKey = Nothing,
          Action.clientActionPayloadContext = spanishContext rootContext
        }
      `shouldBe` Action.UnrecognizedClientAction
    case Action.actionEndpointMetadata (moduleActionCodec rootModule) (spanishContext rootContext) "POST" "/es/catalog/actions/refresh" of
      Nothing -> expectationFailure "expected localized catalog action metadata"
      Just metadata -> routeTemplateText (endpointRouteTemplate metadata) `shouldBe` "/{locale}/catalog/actions/refresh"
    Action.actionPath (moduleActionCodec rootModule) (spanishContext rootContext) (CatalogActionTarget RefreshCatalogTarget)
      `shouldBe` Just "/es/catalog/actions/refresh"
    Action.actionPath (moduleActionCodec rootModule) (spanishContext rootContext) (OrdersActionTarget SubmitOrderTarget)
      `shouldBe` Just "/es/orders/actions/submit"

  it "serves typed public assets through the locale root with public asset metadata" $ do
    rootModule <- requiredRootModule
    let rootContext = defaultComposedContext
        assetLocation = RouteLocation [requiredPathSegment "es", requiredPathSegment "public", requiredPathSegment "assets", requiredPathSegment "app.css"] []
    case parseRoute (moduleRouteCodec rootModule) rootContext assetLocation of
      RouteParsed request -> do
        requestRoute request
          `shouldBe` Localized (locale "es") (Public (PublicAsset (StaticAssetRoute [requiredPathSegment "public", requiredPathSegment "assets", requiredPathSegment "app.css"])))
        let definition = moduleEndpoints rootModule (requestRoute request)
            metadata = routeMetadata definition
        endpointProtocol metadata `shouldBe` AssetEndpoint
        endpointAccess metadata `shouldBe` AllowUnauthenticated
        routeTemplateText (endpointRouteTemplate metadata) `shouldBe` "/{locale}/public/assets/*"
        response <- routeResponse definition Wai.defaultRequest request
        case response of
          ProtocolResponseResult protocolResponse -> do
            protocolResponseStatus protocolResponse `shouldBe` Http.status200
            case protocolResponseBody protocolResponse of
              ProtocolResponseWai waiResponse -> Wai.responseStatus waiResponse `shouldBe` Http.status200
              _ -> expectationFailure "expected the static adapter to keep a raw WAI response"
          _ -> expectationFailure "expected the static adapter to return a protocol response"
      RouteNotMatched -> expectationFailure "expected the Spanish public asset route"
      RouteMalformed routeError -> expectationFailure (show routeError)

  it "attaches the trusted root/module observation before a child context is projected" $ do
    rootModule <- requiredRootModule
    composedSite <- requiredComposedSite
    let rootContext = defaultComposedContext
        rootRoute = Localized (locale "es") (Catalog CatalogIndex)
        metadata = routeMetadata (moduleEndpoints rootModule rootRoute)
        observedContext = Site.siteAttachRouteObservation composedSite rootRoute metadata (spanishContext rootContext)
    requestRouteObservation (requestCore observedContext)
      `shouldBe` Just
        RouteObservation
          { observedEndpointName = endpointName metadata,
            observedMountChain = requiredModuleName "root" :| [requiredModuleName "root.catalog", requiredModuleName "catalog"],
            observedRouteTemplate = endpointRouteTemplate metadata,
            observedLocale = locale "es"
          }

  it "runs the locale-rooted public page through the production WAI interpreter" $ do
    composedSite <- requiredComposedSite
    waiApplication <- toWaiApplication (Site.buildSiteApplication composedSite)
    englishResponse <- performWaiRequest (pure waiApplication) (waiRequest ["public", "login"])
    spanishResponse <- performWaiRequest (pure waiApplication) (waiRequest ["es", "public", "login"])
    englishBody <- readResponseBody englishResponse
    spanishBody <- readResponseBody spanishResponse
    expectAll
      ( (Wai.responseStatus englishResponse `shouldBe` Http.status200)
          :| [ Wai.responseStatus spanishResponse `shouldBe` Http.status200,
               Text.isInfixOf "href=\"/en/catalog\"" englishBody `shouldBe` True,
               Text.isInfixOf "href=\"/es/catalog\"" spanishBody `shouldBe` True,
               Text.isInfixOf "<h1>Login</h1>" englishBody `shouldBe` True,
               Text.isInfixOf "<h1>Login</h1>" spanishBody `shouldBe` True
             ]
      )

  it "executes mounted pages and static assets through the assembled site" $ do
    let authenticatedSecurity =
          AuthenticationEnabled
            []
            (AuthenticationGuard (pure . ContinueEndpoint . authenticatedRootContext . endpointRouteRequest))
            []
        authenticatedSite =
          buildComposedSiteWithSecurity
            defaultComposedStaticAssets
            defaultLocalePolicy
            authenticatedSecurity
            catalogQueries
            catalogCommands
            ordersQueries
            ordersCommands
    waiApplication <- toWaiApplication (Site.buildSiteApplication authenticatedSite)
    catalogResponse <- performWaiRequest (pure waiApplication) (waiRequest ["es", "catalog"])
    ordersResponse <- performWaiRequest (pure waiApplication) (waiRequest ["es", "orders"])
    assetResponse <- performWaiRequest (pure waiApplication) (waiRequest ["es", "public", "assets", "app.css"])
    catalogBody <- readResponseBody catalogResponse
    ordersBody <- readResponseBody ordersResponse
    assetBody <- readResponseBody assetResponse
    expectAll
      ( (Wai.responseStatus catalogResponse `shouldBe` Http.status200)
          :| [ Wai.responseStatus ordersResponse `shouldBe` Http.status200,
               Wai.responseStatus assetResponse `shouldBe` Http.status200,
               Text.isInfixOf "<h1>Catalog</h1>" catalogBody `shouldBe` True,
               Text.isInfixOf "<h1>Orders</h1>" ordersBody `shouldBe` True,
               Text.isInfixOf "max-inline-size" assetBody `shouldBe` True
             ]
      )

  it "declares the complete public and mounted route algebra with one locale prefix" $ do
    rootModule <- requiredRootModule
    let rootContext = defaultComposedContext
        assetRoute = StaticAssetRoute [requiredPathSegment "public", requiredPathSegment "assets", requiredPathSegment "app.css"]
        loginRoute = Localized (locale "en") (Public PublicLogin)
        notFoundRoute = Localized (locale "en") (Public PublicNotFound)
        catalogRoute = Localized (locale "en") (Catalog CatalogIndex)
        ordersRoute = Localized (locale "en") (Orders OrdersIndex)
    staticAssetRoots defaultComposedStaticAssets `shouldBe` [StaticAssetRoot "/public/assets" "public-assets"]
    staticAssetContentTypes defaultComposedStaticAssets `shouldSatisfy` (not . null)
    staticCacheControlSeconds defaultComposedStaticAssets `shouldBe` Just 300
    Public PublicLogin `shouldBe` Public PublicLogin
    Public (PublicAsset assetRoute) `shouldBe` Public (PublicAsset assetRoute)
    Public PublicNotFound `shouldBe` Public PublicNotFound
    Public PublicLogin `shouldNotBe` Public (PublicAsset assetRoute)
    Public (PublicAsset assetRoute) `shouldNotBe` Public (PublicAsset (StaticAssetRoute [requiredPathSegment "other"]))
    Public PublicNotFound `shouldNotBe` Public PublicLogin
    Catalog CatalogIndex `shouldBe` Catalog CatalogIndex
    Orders OrdersIndex `shouldBe` Orders OrdersIndex
    Catalog CatalogIndex `shouldNotBe` Orders OrdersIndex
    CatalogActionTarget RefreshCatalogTarget `shouldBe` CatalogActionTarget RefreshCatalogTarget
    OrdersActionTarget SubmitOrderTarget `shouldBe` OrdersActionTarget SubmitOrderTarget
    CatalogActionTarget RefreshCatalogTarget `shouldNotBe` OrdersActionTarget SubmitOrderTarget
    CatalogAction RefreshCatalog `shouldBe` CatalogAction RefreshCatalog
    OrdersAction SubmitOrder `shouldBe` OrdersAction SubmitOrder
    CatalogAction RefreshCatalog `shouldNotBe` OrdersAction SubmitOrder
    [RootMayReadCatalog, RootMayRefreshCatalog, RootMayReadOrders, RootMaySubmitOrders]
      `shouldBe` [RootMayReadCatalog, RootMayRefreshCatalog, RootMayReadOrders, RootMaySubmitOrders]
    RootMayReadCatalog `shouldNotBe` RootMayRefreshCatalog
    RootMayReadOrders `shouldNotBe` RootMaySubmitOrders
    BrowserClient `shouldBe` BrowserClient
    OtherClient `shouldBe` OtherClient
    BrowserClient `shouldNotBe` OtherClient
    RootLocal `shouldBe` RootLocal
    show PublicLogin `shouldBe` "PublicLogin"
    show (PublicAsset assetRoute) `shouldBe` "PublicAsset (StaticAssetRoute {staticAssetPathSegments = [PathSegment \"public\",PathSegment \"assets\",PathSegment \"app.css\"]})"
    show PublicNotFound `shouldBe` "PublicNotFound"
    show (Catalog CatalogIndex) `shouldBe` "Catalog CatalogIndex"
    show (Orders OrdersIndex) `shouldBe` "Orders OrdersIndex"
    show (Localized (locale "en") (Catalog CatalogIndex)) `shouldBe` "Localized (Locale \"en\") (Catalog CatalogIndex)"
    show (CatalogActionTarget RefreshCatalogTarget) `shouldBe` "CatalogActionTarget RefreshCatalogTarget"
    show (OrdersActionTarget SubmitOrderTarget) `shouldBe` "OrdersActionTarget SubmitOrderTarget"
    show (CatalogAction RefreshCatalog) `shouldBe` "CatalogAction RefreshCatalog"
    show (OrdersAction SubmitOrder) `shouldBe` "OrdersAction SubmitOrder"
    show RootMayReadCatalog `shouldBe` "RootMayReadCatalog"
    show RootMayRefreshCatalog `shouldBe` "RootMayRefreshCatalog"
    show RootMayReadOrders `shouldBe` "RootMayReadOrders"
    show RootMaySubmitOrders `shouldBe` "RootMaySubmitOrders"
    show BrowserClient `shouldBe` "BrowserClient"
    show OtherClient `shouldBe` "OtherClient"
    show RootLocal `shouldBe` "RootLocal"
    RootPrincipal Nothing [] `shouldNotBe` RootPrincipal (Just (locale "es")) []
    RootPrincipal (Just (locale "es")) [] `shouldNotBe` RootPrincipal (Just (locale "es")) ["catalog.read"]
    show (RootPrincipal (Just (locale "es")) ["catalog.read"]) `shouldBe` "RootPrincipal {rootPrincipalLocalePreference = Just (Locale \"es\"), rootPrincipalScopes = [\"catalog.read\"]}"
    moduleOwnsRoute rootModule loginRoute `shouldBe` True
    moduleOwnsRoute rootModule notFoundRoute `shouldBe` True
    moduleOwnsRoute rootModule catalogRoute `shouldBe` True
    moduleOwnsRoute rootModule ordersRoute `shouldBe` True
    moduleRouteMountChain rootModule loginRoute
      `shouldBe` requiredModuleName "root" :| [requiredModuleName "root.public", requiredModuleName "public"]
    moduleRouteMountChain rootModule ordersRoute
      `shouldBe` requiredModuleName "root" :| [requiredModuleName "root.orders", requiredModuleName "orders"]
    Routing.routeMethods (moduleRouteCodec rootModule) loginRoute `shouldBe` Routing.routeMethodPolicy [Routing.RouteGet]
    Routing.routeMethods (moduleRouteCodec rootModule) notFoundRoute `shouldBe` Routing.RouteHidden
    Routing.routeMethods (moduleRouteCodec rootModule) catalogRoute `shouldBe` Routing.routeMethodPolicy [Routing.RouteGet]
    Routing.routeMethods (moduleRouteCodec rootModule) ordersRoute `shouldBe` Routing.routeMethodPolicy [Routing.RouteGet]
    routePathSegments (renderRoute (moduleRouteCodec rootModule) (RouteRequest loginRoute rootContext))
      `shouldBe` [requiredPathSegment "en", requiredPathSegment "public", requiredPathSegment "login"]
    routePathSegments (renderRoute (moduleRouteCodec rootModule) (RouteRequest (Localized (locale "es") (Public (PublicAsset assetRoute))) rootContext))
      `shouldBe` requiredPathSegment "es" : staticAssetPathSegments assetRoute
    notFoundRequest (moduleRouteCodec rootModule) rootContext `shouldBe` RouteRequest notFoundRoute rootContext
    let spanishNotFoundContext = spanishContext rootContext
    notFoundRequest (moduleRouteCodec rootModule) spanishNotFoundContext
      `shouldBe` RouteRequest (Localized (locale "es") (Public PublicNotFound)) spanishNotFoundContext
    parseRoute (moduleRouteCodec rootModule) rootContext (RouteLocation [requiredPathSegment "en", requiredPathSegment "public", requiredPathSegment "login"] [])
      `shouldBe` RouteParsed (RouteRequest loginRoute (rootContext {requestCore = (requestCore rootContext) {requestLocaleFallbacks = [locale "en"]}}))
    parseRoute (moduleRouteCodec rootModule) rootContext (RouteLocation [requiredPathSegment "catalog"] [])
      `shouldBe` RouteParsed (RouteRequest catalogRoute (rootContext {requestCore = (requestCore rootContext) {requestLocaleFallbacks = [locale "en"]}}))
    parseRoute (moduleRouteCodec rootModule) rootContext (RouteLocation [requiredPathSegment "fr", requiredPathSegment "catalog"] []) `shouldBe` RouteNotMatched
    parseRoute (moduleRouteCodec rootModule) rootContext (RouteLocation [] []) `shouldBe` RouteNotMatched
    loginResponse <- routeResponse (moduleEndpoints rootModule loginRoute) Wai.defaultRequest (RouteRequest loginRoute rootContext)
    assertPageResponse "Login" loginRoute rootContext loginResponse
    notFoundResponse <- routeResponse (moduleEndpoints rootModule notFoundRoute) Wai.defaultRequest (RouteRequest notFoundRoute rootContext)
    assertPageResponse "Not Found" notFoundRoute rootContext notFoundResponse

  it "keeps every exported root algebra value distinct and printable" $ do
    let assetRoute = StaticAssetRoute [requiredPathSegment "public", requiredPathSegment "assets", requiredPathSegment "app.css"]
        publicRoutes = [PublicLogin, PublicAsset assetRoute, PublicNotFound]
        localizedRoutes = [Public PublicLogin, Public (PublicAsset assetRoute), Public PublicNotFound, Catalog CatalogIndex, Orders OrdersIndex]
        rootRoutes = [Localized (locale "en") localRoute | localRoute <- localizedRoutes] <> [Localized (locale "es") (Catalog CatalogIndex)]
        actionTargets = [CatalogActionTarget RefreshCatalogTarget, OrdersActionTarget SubmitOrderTarget]
        actions = [CatalogAction RefreshCatalog, OrdersAction SubmitOrder]
        authorizations = [RootMayReadCatalog, RootMayRefreshCatalog, RootMayReadOrders, RootMaySubmitOrders]
        principals = [RootPrincipal Nothing [], RootPrincipal (Just (locale "en")) [], RootPrincipal Nothing ["catalog.read"], RootPrincipal (Just (locale "es")) ["orders.read"]]
        clients = [BrowserClient, OtherClient]
        policies = [defaultLocalePolicy, LocalePolicy (locale "en" :| []) (locale "en"), LocalePolicy (locale "es" :| [locale "en"]) (locale "es")]
        resolutionInputs =
          [ LocaleResolutionInput Nothing Nothing Nothing AnonymousIdentity,
            LocaleResolutionInput (Just (locale "es")) Nothing Nothing AnonymousIdentity,
            LocaleResolutionInput Nothing (Just "es") Nothing (AuthenticatedIdentity (RootPrincipal Nothing [])),
            LocaleResolutionInput Nothing Nothing (Just "en-US") (AuthenticatedIdentity (RootPrincipal (Just (locale "en")) ["catalog.read"]))
          ]
    expectDistinctAndPrintable publicRoutes
    expectDistinctAndPrintable localizedRoutes
    expectDistinctAndPrintable rootRoutes
    expectDistinctAndPrintable actionTargets
    expectDistinctAndPrintable actions
    expectDistinctAndPrintable authorizations
    expectDistinctAndPrintable principals
    expectDistinctAndPrintable clients
    expectDistinctAndPrintable policies
    expectDistinctAndPrintable resolutionInputs
    assertRootLocalEqual (requestLocal defaultComposedContext) RootLocal
    rootLocalsDiffer (requestLocal defaultComposedContext) RootLocal `shouldBe` False
    showsPrec 11 RootLocal "" `shouldBe` "RootLocal"
    showList [RootLocal] "" `shouldBe` "[RootLocal]"

  it "installs one site with the supplied security choice and root module chain" $ do
    let suppliedSecurity = AuthenticationDisabled []
        rootRoute = Localized (locale "en") (Public PublicLogin)
    let composedSite = buildComposedSiteWithSecurity defaultComposedStaticAssets defaultLocalePolicy suppliedSecurity catalogQueries catalogCommands ordersQueries ordersCommands
    Site.siteName composedSite `shouldBe` "composed-domains"
    Site.siteDefaultRequestContext composedSite `shouldBe` defaultComposedContext
    case Site.siteRouteModuleChain composedSite of
      Nothing -> expectationFailure "expected a root module chain"
      Just routeChain -> routeChain rootRoute `shouldBe` requiredModuleName "root" :| [requiredModuleName "root.public", requiredModuleName "public"]
    Site.siteNavigationRoutes composedSite `shouldBe` []
    case Site.siteSecurity composedSite of
      AuthenticationDisabled [] -> pure ()
      AuthenticationDisabled _ -> expectationFailure "expected no additional public guards"
      AuthenticationEnabled {} -> expectationFailure "expected the supplied public security policy"

  it "keeps the default root declaration, shell, and mounted declarations complete" $ do
    rootModule <- requiredRootModule
    let rootRoute = Localized (locale "es") (Public PublicLogin)
        englishLoginRoute = Localized (locale "en") (Public PublicLogin)
        assetRoute = StaticAssetRoute [requiredPathSegment "public", requiredPathSegment "assets", requiredPathSegment "app.css"]
        englishAssetRoute = Localized (locale "en") (Public (PublicAsset assetRoute))
        defaultSite = buildComposedSite defaultComposedStaticAssets defaultLocalePolicy catalogQueries catalogCommands ordersQueries ordersCommands
        shell = Site.sitePageShell defaultSite (Page "Login" rootRoute (spanishContext defaultComposedContext) (error "page body is not inspected") [])
    moduleName rootModule `shouldBe` requiredModuleName "root"
    moduleDeclaredRoutes rootModule
      `shouldBe` [ Localized (locale "en") (Public PublicLogin),
                   Localized (locale "en") (Public (PublicAsset assetRoute)),
                   Localized (locale "en") (Public PublicNotFound),
                   Localized (locale "en") (Catalog CatalogIndex),
                   Localized (locale "en") (Orders OrdersIndex)
                 ]
    map (endpointName . routeMetadata . moduleEndpoints rootModule) (moduleDeclaredRoutes rootModule)
      `shouldBe` [ requiredEndpointName "root.public.login",
                   requiredEndpointName "root.public.assets",
                   requiredEndpointName "root.public.not-found",
                   requiredEndpointName "root.catalog.catalog.index",
                   requiredEndpointName "root.orders.orders.index"
                 ]
    map endpointName (Action.declaredActionEndpointMetadata (moduleActionCodec rootModule))
      `shouldBe` [requiredEndpointName "root.catalog.catalog.refresh", requiredEndpointName "root.orders.orders.submit"]
    shellBodyAttributes shell `shouldBe` []
    shellNavigationAttributes shell `shouldBe` []
    shellMainId shell `shouldBe` literalElementId "main"
    shellMainAttributes shell `shouldBe` []
    shellStylesheets shell `shouldBe` []
    shellRuntimeDescriptors shell `shouldBe` []
    case Site.siteSecurity defaultSite of
      AuthenticationDisabled guards -> null guards `shouldBe` True
      _ -> expectationFailure "expected the default public root security"
    case parseRoute (Site.siteRouteCodec defaultSite) defaultComposedContext (RouteLocation [requiredPathSegment "public", requiredPathSegment "login"] []) of
      RouteParsed request -> do
        requestRoute request `shouldBe` englishLoginRoute
        siteResponse <- routeResponse (Site.siteRouteDefinition defaultSite englishLoginRoute) Wai.defaultRequest request
        assertPageResponse "Login" englishLoginRoute (requestContext request) siteResponse
      routeResult -> expectationFailure ("expected the installed root codec to parse the public login route, got " <> show routeResult)
    case parseRoute (Site.siteRouteCodec defaultSite) defaultComposedContext (RouteLocation (staticAssetPathSegments assetRoute) []) of
      RouteParsed request -> do
        requestRoute request `shouldBe` englishAssetRoute
        assetResponse <- routeResponse (Site.siteRouteDefinition defaultSite englishAssetRoute) Wai.defaultRequest request
        case assetResponse of
          ProtocolResponseResult protocolResponse -> protocolResponseStatus protocolResponse `shouldBe` Http.status200
          unexpectedResponse -> expectationFailure ("expected the installed static adapter to return a protocol response, got " <> show unexpectedResponse)
      routeResult -> expectationFailure ("expected the installed root codec to parse the asset route, got " <> show routeResult)
    Site.siteDecodeClientAction
      defaultSite
      Action.ClientActionPayload
        { Action.clientActionMethod = "POST",
          Action.clientActionPath = "/en/catalog/actions/refresh",
          Action.clientActionFields = [],
          Action.clientActionCsrfToken = Nothing,
          Action.clientActionIdempotencyKey = Nothing,
          Action.clientActionPayloadContext = defaultComposedContext
        }
      `shouldBe` Action.DecodedClientAction (CatalogAction RefreshCatalog)
    Site.siteHandleClientAction defaultSite (ClientActionRequest (CatalogAction RefreshCatalog) Nothing defaultComposedContext)
      `shouldReturn` Just (clientActionResponse Http.status200)
    Site.siteHandleClientAction defaultSite (ClientActionRequest (OrdersAction SubmitOrder) Nothing defaultComposedContext)
      `shouldReturn` Just (clientActionResponse Http.status202)

  it "projects authenticated and anonymous root facts into each domain's query and action adapters" $ do
    catalogQueryContext <- newIORef Nothing
    catalogActionContext <- newIORef Nothing
    ordersQueryContext <- newIORef Nothing
    ordersActionContext <- newIORef Nothing
    let queries = CatalogQueries (\domainContext -> writeIORef catalogQueryContext (Just domainContext) >> pure "Catalog")
        commands = CatalogCommands (\domainContext -> writeIORef catalogActionContext (Just domainContext) >> pure "refreshed")
        orderQueries = OrdersQueries (\domainContext -> writeIORef ordersQueryContext (Just domainContext) >> pure "Orders")
        orderCommands = OrdersCommands (\domainContext -> writeIORef ordersActionContext (Just domainContext) >> pure (OrderId "order-2"))
        authenticatedContext =
          spanishContext
            defaultComposedContext
              { requestIdentity = AuthenticatedIdentity (RootPrincipal (Just (locale "en")) ["catalog.read", "orders.read"]),
                requestClient = OtherClient
              }
        anonymousContext = spanishContext defaultComposedContext
        catalogRoute = Localized (locale "es") (Catalog CatalogIndex)
        ordersRoute = Localized (locale "es") (Orders OrdersIndex)
    let configuredPolicy = LocalePolicy (locale "en" :| [locale "es"]) (locale "en")
    rootModule <- requiredRootModuleWithPolicy configuredPolicy queries commands orderQueries orderCommands
    endpointAccess (routeMetadata (moduleEndpoints rootModule catalogRoute)) `shouldBe` RequireAuthorized RootMayReadCatalog
    endpointAccess (routeMetadata (moduleEndpoints rootModule ordersRoute)) `shouldBe` RequireAuthorized RootMayReadOrders
    Action.actionEndpointMetadata (moduleActionCodec rootModule) authenticatedContext "POST" "/es/catalog/actions/refresh"
      `shouldSatisfy` maybe False ((== RequireAuthorized RootMayRefreshCatalog) . endpointAccess)
    Action.actionEndpointMetadata (moduleActionCodec rootModule) authenticatedContext "POST" "/es/orders/actions/submit"
      `shouldSatisfy` maybe False ((== RequireAuthorized RootMaySubmitOrders) . endpointAccess)
    catalogResponse <- routeResponse (moduleEndpoints rootModule catalogRoute) Wai.defaultRequest (RouteRequest catalogRoute authenticatedContext)
    assertPageResponse "Catalog" catalogRoute authenticatedContext catalogResponse
    ordersResponse <- routeResponse (moduleEndpoints rootModule ordersRoute) Wai.defaultRequest (RouteRequest ordersRoute authenticatedContext)
    assertPageResponse "Orders" ordersRoute authenticatedContext ordersResponse
    readIORef catalogQueryContext `shouldReturn` Just (CatalogContext "es" (Just "catalog.read"))
    readIORef ordersQueryContext `shouldReturn` Just (OrdersContext "es" (Just "catalog.read"))
    moduleHandleAction rootModule (ClientActionRequest (CatalogAction RefreshCatalog) Nothing authenticatedContext)
      `shouldReturn` Just (clientActionResponse Http.status200)
    moduleHandleAction rootModule (ClientActionRequest (OrdersAction SubmitOrder) Nothing authenticatedContext)
      `shouldReturn` Just (clientActionResponse Http.status202)
    readIORef catalogActionContext `shouldReturn` Just (CatalogContext "es" (Just "catalog.read"))
    readIORef ordersActionContext `shouldReturn` Just (OrdersContext "es" (Just "catalog.read"))
    _ <- routeResponse (moduleEndpoints rootModule catalogRoute) Wai.defaultRequest (RouteRequest catalogRoute anonymousContext)
    readIORef catalogQueryContext `shouldReturn` Just (CatalogContext "es" Nothing)

  it "uses the shell locale and parses only bounded locale candidates from WAI" $ do
    let customPolicy = LocalePolicy (locale "es" :| [locale "en"]) (locale "en")
        composedSite = buildComposedSite defaultComposedStaticAssets customPolicy catalogQueries catalogCommands ordersQueries ordersCommands
        requestFor path headers = Wai.defaultRequest {Wai.pathInfo = path, Wai.requestHeaders = headers}
        requestContext = Site.siteRequestContextFromRequest composedSite
        shell = Site.sitePageShell composedSite (Page "Catalog" (Localized (locale "es") (Catalog CatalogIndex)) (spanishContext defaultComposedContext) (error "page body is not inspected") [])
    shellNavigationItems shell
      `shouldBe` [ NavigationItem "Sign in" (Localized (locale "es") (Public PublicLogin)),
                   NavigationItem "Catalog" (Localized (locale "es") (Catalog CatalogIndex)),
                   NavigationItem "Orders" (Localized (locale "es") (Orders OrdersIndex))
                 ]
    shellNavigationLifecycle shell `shouldBe` Nothing
    Site.siteNavigationRuntime composedSite `shouldSatisfy` isJust
    let prefixedContext = requestContext (requestFor ["es"] [(Http.hCookie, "locale=en"), (Http.hAcceptLanguage, "en-US")]) defaultComposedContext
    requestLocale (requestCore prefixedContext) `shouldBe` locale "es"
    requestLocaleFallbacks (requestCore prefixedContext) `shouldBe` [locale "es", locale "en"]
    requestLocale (requestCore (requestContext (requestFor [] [(Http.hCookie, "locale=es; theme=dark")]) defaultComposedContext)) `shouldBe` locale "es"
    requestLocale (requestCore (requestContext (requestFor [] [(Http.hAcceptLanguage, "es-MX,en;q=0.8")]) defaultComposedContext)) `shouldBe` locale "es"
    requestLocale (requestCore (requestContext (requestFor [] [(Http.hCookie, ByteString.pack [108, 111, 99, 97, 108, 101, 61, 255]), (Http.hAcceptLanguage, ByteString.pack [255])]) defaultComposedContext)) `shouldBe` locale "en"

  it "makes the public local module independently composable and rejects a sibling route" $ do
    let publicModule = buildPublicModule defaultComposedStaticAssets
        publicContext = defaultComposedContext
        loginLocation = RouteLocation [requiredPathSegment "public", requiredPathSegment "login"] []
        assetLocation = RouteLocation [requiredPathSegment "public", requiredPathSegment "assets", requiredPathSegment "app.css"] []
    moduleOwnsRoute publicModule (Public PublicLogin) `shouldBe` True
    moduleOwnsRoute publicModule (Catalog CatalogIndex) `shouldBe` False
    moduleRouteMountChain publicModule (Public PublicLogin)
      `shouldBe` requiredModuleName "root.public" :| [requiredModuleName "public"]
    parseRoute (moduleRouteCodec publicModule) publicContext loginLocation
      `shouldBe` RouteParsed (RouteRequest (Public PublicLogin) publicContext)
    parseRoute (moduleRouteCodec publicModule) publicContext (RouteLocation [requiredPathSegment "public", requiredPathSegment "missing"] []) `shouldBe` RouteNotMatched
    case parseRoute (moduleRouteCodec publicModule) publicContext assetLocation of
      RouteParsed request -> do
        requestRoute request `shouldBe` Public (PublicAsset (StaticAssetRoute (routePathSegments assetLocation)))
        requestContext request `shouldBe` publicContext
      routeResult -> expectationFailure ("expected public asset route, got " <> show routeResult)
    parseRoute (moduleRouteCodec publicModule) publicContext (RouteLocation [requiredPathSegment "other"] []) `shouldBe` RouteNotMatched
    Routing.routeMethods (moduleRouteCodec publicModule) (Public (PublicAsset (StaticAssetRoute (routePathSegments assetLocation)))) `shouldBe` Routing.routeMethodPolicy [Routing.RouteGet]
    Routing.routeMethods (moduleRouteCodec publicModule) (Catalog CatalogIndex) `shouldBe` Routing.RouteHidden
    notFoundRequest (moduleRouteCodec publicModule) publicContext `shouldBe` RouteRequest (Public PublicNotFound) publicContext
    moduleHandleAction publicModule (ClientActionRequest (CatalogAction RefreshCatalog) Nothing publicContext) `shouldReturn` Nothing
    let loginDefinition = moduleEndpoints publicModule (Public PublicLogin)
        assetDefinition = moduleEndpoints publicModule (Public (PublicAsset (StaticAssetRoute (routePathSegments assetLocation))))
        missingDefinition = moduleEndpoints publicModule (Public PublicNotFound)
    routeNavigationLabel loginDefinition `shouldBe` Just "Login"
    endpointName (routeMetadata loginDefinition) `shouldBe` requiredEndpointName "root.public.login"
    routeTemplateText (endpointRouteTemplate (routeMetadata loginDefinition)) `shouldBe` "/public/login"
    endpointProtocol (routeMetadata loginDefinition) `shouldBe` HtmlEndpoint
    endpointAccess (routeMetadata loginDefinition) `shouldBe` AllowUnauthenticated
    Site.routeMethods loginDefinition `shouldBe` [Routing.RouteGet]
    routeExecutionPolicy loginDefinition `shouldBe` unboundedRouteExecutionPolicy
    directLoginResponse <- routeResponse loginDefinition Wai.defaultRequest (RouteRequest (Public PublicLogin) publicContext)
    case directLoginResponse of
      PageResponse page -> do
        pageTitle page `shouldBe` "Login"
        pageRoute page `shouldBe` Public PublicLogin
        pageContext page `shouldBe` publicContext
        renderHtml (pageBody page) `shouldBe` "<h1>Login</h1>"
        pageBootstrapHooks page `shouldBe` []
      _ -> expectationFailure "expected the public login definition to return a page"
    routeNavigationLabel assetDefinition `shouldBe` Nothing
    endpointName (routeMetadata assetDefinition) `shouldBe` requiredEndpointName "root.public.assets"
    routeTemplateText (endpointRouteTemplate (routeMetadata assetDefinition)) `shouldBe` "/public/assets/*"
    endpointProtocol (routeMetadata assetDefinition) `shouldBe` AssetEndpoint
    endpointAccess (routeMetadata assetDefinition) `shouldBe` AllowUnauthenticated
    Site.routeMethods assetDefinition `shouldBe` [Routing.RouteGet]
    routeExecutionPolicy assetDefinition `shouldBe` unboundedRouteExecutionPolicy
    routeNavigationLabel missingDefinition `shouldBe` Nothing
    endpointName (routeMetadata missingDefinition) `shouldBe` requiredEndpointName "root.public.not-found"
    routeTemplateText (endpointRouteTemplate (routeMetadata missingDefinition)) `shouldBe` "/public/404"
    endpointProtocol (routeMetadata missingDefinition) `shouldBe` HtmlEndpoint
    endpointAccess (routeMetadata missingDefinition) `shouldBe` AllowUnauthenticated
    Site.routeMethods missingDefinition `shouldBe` []
    routeExecutionPolicy missingDefinition `shouldBe` unboundedRouteExecutionPolicy
    directMissingResponse <- routeResponse missingDefinition Wai.defaultRequest (RouteRequest (Public PublicNotFound) publicContext)
    case directMissingResponse of
      PageResponse page -> do
        pageTitle page `shouldBe` "Not Found"
        pageRoute page `shouldBe` Public PublicNotFound
        pageContext page `shouldBe` publicContext
        renderHtml (pageBody page) `shouldBe` "<h1>Not Found</h1>"
        pageBootstrapHooks page `shouldBe` []
      _ -> expectationFailure "expected the public not-found definition to return a page"
    renderFailure <- try (evaluate (renderRoute (moduleRouteCodec publicModule) (RouteRequest (Catalog CatalogIndex) publicContext))) :: IO (Either ErrorCall RouteLocation)
    case renderFailure of
      Left failure -> show failure `shouldBe` "attempted to render a non-public route through the public module"
      Right _ -> expectationFailure "expected public codec to reject its sibling route"
    definitionFailure <- try (evaluate (moduleEndpoints publicModule (Catalog CatalogIndex))) :: IO (Either ErrorCall (Site.RouteDefinition LocalizedRoute ComposedContext RootAuthorization))
    case definitionFailure of
      Left failure -> show failure `shouldBe` "attempted to select a non-public route through the public module"
      Right _ -> expectationFailure "expected public definition selection to reject its sibling route"

  it "adapts local guards under the selected locale without changing the root response algebra" $ do
    observedGuardInput <- newIORef Nothing
    let publicModule =
          (buildPublicModule defaultComposedStaticAssets)
            { moduleGuards =
                [ EndpointGuard $ \endpointRequest -> do
                    let localRequest = endpointRouteRequest endpointRequest
                        hasSecurityEventSink = case endpointSecurityEventSink endpointRequest of
                          Nothing -> False
                          Just _ -> True
                    writeIORef
                      observedGuardInput
                      ( Just
                          ( Wai.rawPathInfo (endpointWaiRequest endpointRequest),
                            requestRoute localRequest,
                            requestLocale (requestCore (requestContext localRequest)),
                            endpointName (endpointMetadata endpointRequest),
                            hasSecurityEventSink
                          )
                      )
                    case endpointDispatchKind endpointRequest of
                      EndpointMatched -> pure (ContinueEndpoint (requestContext localRequest))
                      EndpointOptions -> pure (HaltEndpoint (BodyResponse guardResponseBody))
                      _ -> pure (ContinueEndpoint (requestContext localRequest))
                ]
            }
        rootContext = spanishContext defaultComposedContext
        localizedContext =
          rootContext
            { requestCore =
                (requestCore rootContext)
                  { requestLocaleFallbacks = [locale "es", locale "en"]
                  }
            }
        rootRoute = Localized (locale "es") (Public PublicLogin)
    localizedModule <-
      case localizeApplicationModule defaultLocalePolicy publicModule of
        Left codecError -> expectationFailure (show codecError) >> fail "could not localize the public module"
        Right moduleValue -> pure moduleValue
    let metadata = routeMetadata (moduleEndpoints localizedModule rootRoute)
        endpointRequest dispatchKind =
          EndpointRequest
            { endpointWaiRequest = Wai.defaultRequest,
              endpointRouteRequest = RouteRequest rootRoute rootContext,
              endpointMetadata = metadata,
              endpointSecurityEventSink = Nothing,
              endpointDispatchKind = dispatchKind
            }
    case moduleGuards localizedModule of
      [localizedGuard] -> do
        runEndpointGuard localizedGuard (endpointRequest EndpointMatched) `shouldReturn` ContinueEndpoint localizedContext
        readIORef observedGuardInput
          `shouldReturn` Just ("", Public PublicLogin, locale "es", requiredEndpointName "root.public.login", False)
        runEndpointGuard localizedGuard (endpointRequest EndpointOptions) `shouldReturn` HaltEndpoint (BodyResponse guardResponseBody)
      _ -> expectationFailure "expected exactly one localized guard"

  it "keeps a malformed child route distinct from an ordinary root miss" $ do
    let publicModule = buildPublicModule defaultComposedStaticAssets
        malformedModule =
          publicModule
            { moduleRouteCodec =
                (moduleRouteCodec publicModule)
                  { parseRoute = \_ _ -> RouteMalformed InvalidRouteTargetEncoding
                  }
            }
        location = RouteLocation [requiredPathSegment "es", requiredPathSegment "public", requiredPathSegment "login"] []
    localizedModule <-
      case localizeApplicationModule defaultLocalePolicy malformedModule of
        Left codecError -> expectationFailure (show codecError) >> fail "could not localize the malformed module"
        Right moduleValue -> pure moduleValue
    parseRoute (moduleRouteCodec localizedModule) defaultComposedContext location
      `shouldBe` RouteMalformed InvalidRouteTargetEncoding

  it "passes the selected locale into child parsing, rendering, definitions, and not-found selection" $ do
    let publicModule = buildPublicModule defaultComposedStaticAssets
        contextAwareCodec =
          (moduleRouteCodec publicModule)
            { parseRoute = \childContext location ->
                case routePathSegments location of
                  [] -> RouteParsed (RouteRequest (Public PublicLogin) childContext)
                  _ -> parseRoute (moduleRouteCodec publicModule) childContext location,
              renderRoute = \childRequest ->
                RouteLocation [requiredPathSegment (if usesSpanishContext (requestContext childRequest) then "spanish" else "english")] [],
              notFoundRequest = \childContext ->
                RouteRequest
                  (if usesSpanishContext childContext then Public PublicLogin else Public PublicNotFound)
                  childContext
            }
        contextAwareDefinitions localRoute =
          (moduleEndpoints publicModule localRoute)
            { routeResponse = \_ localRequest ->
                pure
                  ( PageResponse
                      Page
                        { pageTitle =
                            case requestRoute localRequest of
                              Public PublicLogin
                                | usesSpanishContext (requestContext localRequest) -> "Spanish child"
                              _ -> "English child",
                          pageRoute = localRoute,
                          pageContext = requestContext localRequest,
                          pageBody = error "page body is not inspected",
                          pageBootstrapHooks = []
                        }
                  )
            }
        contextAwareModule =
          publicModule
            { moduleRouteCodec = contextAwareCodec,
              moduleEndpoints = contextAwareDefinitions
            }
        contextPolicy = LocalePolicy (locale "es" :| [locale "en"]) (locale "en")
        rootContext = spanishContext defaultComposedContext
        rootRoute = Localized (locale "es") (Public PublicLogin)
        usesSpanishContext childContext =
          requestLocale (requestCore childContext) == locale "es"
            && requestLocaleFallbacks (requestCore childContext) == [locale "es", locale "en"]
    localizedModule <-
      case localizeApplicationModule contextPolicy contextAwareModule of
        Left codecError -> expectationFailure (show codecError) >> fail "could not localize the context-aware module"
        Right moduleValue -> pure moduleValue
    parseRoute (moduleRouteCodec localizedModule) rootContext (RouteLocation [] [])
      `shouldBe` RouteParsed (RouteRequest rootRoute (rootContext {requestCore = (requestCore rootContext) {requestLocaleFallbacks = [locale "es", locale "en"]}}))
    notFoundRequest (moduleRouteCodec localizedModule) rootContext
      `shouldBe` RouteRequest rootRoute rootContext
    routePathSegments (renderRoute (moduleRouteCodec localizedModule) (RouteRequest rootRoute defaultComposedContext))
      `shouldBe` [requiredPathSegment "es", requiredPathSegment "spanish"]
    response <- routeResponse (moduleEndpoints localizedModule rootRoute) Wai.defaultRequest (RouteRequest rootRoute defaultComposedContext)
    case response of
      PageResponse page -> do
        pageTitle page `shouldBe` "Spanish child"
        pageRoute page `shouldBe` rootRoute
        pageContext page `shouldBe` defaultComposedContext
      _ -> expectationFailure "expected the localized context-aware page"

  it "localizes a page returned by a halting local guard while retaining root context" $ do
    let rootContext = spanishContext defaultComposedContext
        rootRoute = Localized (locale "es") (Public PublicLogin)
        pageGuard =
          EndpointGuard $ \endpointRequest ->
            pure
              ( HaltEndpoint
                  ( PageResponse
                      Page
                        { pageTitle = "Blocked",
                          pageRoute = Public PublicLogin,
                          pageContext = requestContext (endpointRouteRequest endpointRequest),
                          pageBody = error "page body is not inspected",
                          pageBootstrapHooks = []
                        }
                  )
              )
        publicModule = (buildPublicModule defaultComposedStaticAssets) {moduleGuards = [pageGuard]}
    localizedModule <-
      case localizeApplicationModule defaultLocalePolicy publicModule of
        Left codecError -> expectationFailure (show codecError) >> fail "could not localize the guarded module"
        Right moduleValue -> pure moduleValue
    let endpointRequest =
          EndpointRequest
            { endpointWaiRequest = Wai.defaultRequest,
              endpointRouteRequest = RouteRequest rootRoute rootContext,
              endpointMetadata = routeMetadata (moduleEndpoints localizedModule rootRoute),
              endpointSecurityEventSink = Nothing,
              endpointDispatchKind = EndpointMatched
            }
    case moduleGuards localizedModule of
      [localizedGuard] -> do
        result <- runEndpointGuard localizedGuard endpointRequest
        case result of
          HaltEndpoint (PageResponse page) -> do
            pageTitle page `shouldBe` "Blocked"
            pageRoute page `shouldBe` rootRoute
            pageContext page `shouldBe` rootContext
          unexpectedResult -> expectationFailure ("expected a localized guarded page, got " <> show unexpectedResult)
      _ -> expectationFailure "expected exactly one localized guard"

requiredRootModule :: IO (ApplicationModule RootRoute RootActionTarget RootAction ComposedContext RootAuthorization)
requiredRootModule =
  requiredRootModuleWith catalogQueries catalogCommands ordersQueries ordersCommands

requiredRootModuleWith :: CatalogQueries -> CatalogCommands -> OrdersQueries -> OrdersCommands -> IO (ApplicationModule RootRoute RootActionTarget RootAction ComposedContext RootAuthorization)
requiredRootModuleWith = requiredRootModuleWithPolicy defaultLocalePolicy

requiredRootModuleWithPolicy :: LocalePolicy -> CatalogQueries -> CatalogCommands -> OrdersQueries -> OrdersCommands -> IO (ApplicationModule RootRoute RootActionTarget RootAction ComposedContext RootAuthorization)
requiredRootModuleWithPolicy localePolicy queryDependencies commandDependencies orderQueryDependencies orderCommandDependencies =
  pure (buildComposedModule defaultComposedStaticAssets localePolicy queryDependencies commandDependencies orderQueryDependencies orderCommandDependencies)

requiredComposedSite :: IO (Site.Site RootRoute RootAction ComposedContext RootAuthorization)
requiredComposedSite =
  pure (buildComposedSite defaultComposedStaticAssets defaultLocalePolicy catalogQueries catalogCommands ordersQueries ordersCommands)

authenticatedRootContext :: RouteRequest RootRoute ComposedContext -> ComposedContext
authenticatedRootContext routeRequest =
  (requestContext routeRequest)
    { requestIdentity = AuthenticatedIdentity (RootPrincipal (Just (locale "es")) ["catalog.read", "orders.read"])
    }

requiredModuleName :: Text -> ModuleName
requiredModuleName value =
  case mkModuleName value of
    Left moduleNameError -> error (show moduleNameError)
    Right moduleName -> moduleName

requiredEndpointName :: Text -> EndpointName
requiredEndpointName = requiredEndpointNameOrDie

catalogQueries :: CatalogQueries
catalogQueries = CatalogQueries (const (pure "Catalog"))

catalogCommands :: CatalogCommands
catalogCommands = CatalogCommands (const (pure "refreshed"))

ordersQueries :: OrdersQueries
ordersQueries = OrdersQueries (const (pure "Orders"))

ordersCommands :: OrdersCommands
ordersCommands = OrdersCommands (const (pure (OrderId "order-1")))

spanishContext :: ComposedContext -> ComposedContext
spanishContext rootContext =
  rootContext
    { requestCore =
        (requestCore rootContext)
          { requestLocale = locale "es"
          }
    }

expectDistinctAndPrintable :: (Eq value, Show value) => [value] -> Expectation
expectDistinctAndPrintable values = do
  mapM_ (uncurry shouldNotBe) distinctPairs
  map (length . show) values `shouldSatisfy` all (> 0)
  map (\value -> showsPrec 11 value "") values `shouldSatisfy` not . any null
  showList values "" `shouldSatisfy` not . null
  where
    distinctPairs =
      [ (leftValue, rightValue)
      | (leftIndex, leftValue) <- zip [0 :: Int ..] values,
        (rightIndex, rightValue) <- zip [0 :: Int ..] values,
        leftIndex < rightIndex
      ]

assertRootLocalEqual :: RootLocal -> RootLocal -> Expectation
assertRootLocalEqual leftRootLocal rightRootLocal = leftRootLocal `shouldBe` rightRootLocal

rootLocalsDiffer :: RootLocal -> RootLocal -> Bool
rootLocalsDiffer leftRootLocal rightRootLocal = leftRootLocal /= rightRootLocal

assertPageResponse :: Text -> RootRoute -> ComposedContext -> Response RootRoute ComposedContext -> Expectation
assertPageResponse expectedTitle expectedRoute expectedContext response =
  case response of
    PageResponse page -> do
      pageTitle page `shouldBe` expectedTitle
      pageRoute page `shouldBe` expectedRoute
      pageContext page `shouldBe` expectedContext
    _ -> expectationFailure "expected a page response"

clientActionResponse :: Http.Status -> ClientActionResponse
clientActionResponse status =
  ClientActionResponse
    { clientActionStatus = status,
      clientActionPatches = [],
      clientActionFocusId = Nothing,
      clientActionHeaders = [],
      clientActionObservabilityAttributes = [],
      clientActionLogEntries = []
    }

guardResponseBody :: ResponseBody
guardResponseBody =
  ResponseBody
    { responseStatus = Http.status403,
      responseContentType = "text/plain; charset=utf-8",
      responseBody = "guarded",
      responseObservabilityAttributes = [],
      responseLogEntries = [],
      responseDatabaseOperations = []
    }
