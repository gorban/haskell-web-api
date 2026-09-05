{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module Unit.App.ComposedSpec (spec) where

import App.Composed
import Catalog.Domain
import Control.Exception (ErrorCall, evaluate, try)
import Control.Monad (when)
import Crypto.Error (maybeCryptoError)
import Data.ByteString qualified as ByteString
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word64)
import HarchWeb.Action qualified as Action
import HarchWeb.ApplicationModule (ApplicationModule (..), mountApplicationModule)
import HarchWeb.Csrf (PageSecurity, mkCsrfToken, mkPageCsrf, mkPageSecurity)
import HarchWeb.Csrf qualified as Csrf
import HarchWeb.Document (NavigationItem (..), Page (..), PageShell (..), testRuntimeNonce)
import HarchWeb.EndpointMetadata
  ( AccessRequirement (AllowUnauthenticated, RequireAuthorized),
    EndpointName,
    EndpointProtocol (ApiEndpoint, AssetEndpoint, HtmlEndpoint),
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
import HarchWeb.LoginProtection (defaultLoginProtectionPolicy)
import HarchWeb.Markup (literalElementId, renderHtml)
import HarchWeb.RequestContext
  ( CoreRequestContext (..),
    RequestContext (..),
    RequestIdentity (..),
    correlationRequestId,
    requestLocale,
  )
import HarchWeb.RequestId (RequestId, mkRequestId)
import HarchWeb.Routing
  ( RouteCodec (..),
    RouteDecodeError (InvalidRouteTargetEncoding),
    RouteLocation (..),
    RouteParseResult (..),
    RouteRequest (..),
    requiredPathSegment,
  )
import HarchWeb.Routing qualified as Routing
import HarchWeb.Secret (encryptSecretWithNonce, mkEncryptionNonce, mkSecretEncryptionKey, mkSecretPlaintext)
import HarchWeb.Security (clientAddressText, defaultClientAddress)
import HarchWeb.SecurityEvent (ModuleName, RouteObservation (..), mkModuleName)
import HarchWeb.Server
  ( ActionNavigation (NavigateInternal, StayOnCurrentRoute),
    ClientActionRequest (..),
    ClientActionResponse (..),
    HistoryMode (ReplaceHistory),
    NonPageResponse (NonPageBodyResponse),
    PageResult (RenderedPage, RenderedPageWithMetadata),
    ProtocolResponse (..),
    ProtocolResponseBody (..),
    Response (..),
    ResponseBody (..),
    nonPageResponse,
    toWaiApplication,
    unboundedRouteExecutionPolicy,
  )
import HarchWeb.Session (OpaqueSession (..), SessionCookiePolicy (..), mkSessionCookieName, mkSessionId)
import HarchWeb.Site (RouteDefinition (..), RouteHandler (PageRouteHandler, ProtocolRouteHandler))
import HarchWeb.Site qualified as Site
import HarchWeb.StaticAssets
  ( StaticAssetRoot (..),
    staticAssetContentTypes,
    staticAssetRoots,
    staticCacheControlSeconds,
  )
import HarchWeb.StaticAssets.Route (StaticAssetRoute (..))
import HarchWeb.Time (unixTimeNanoseconds, unixTimeNanosecondsValue, unixTimeSeconds)
import HarchWeb.Totp (mkTotpCode, mkTotpSecret, renderTotpSecret, totpCode, totpCodeText)
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import Orders.Domain
import Test.Hspec
import TestCore.CustomAssertions (expectAll)
import TestCore.Wai (nextRequestBodyChunk, performWaiRequest, readResponseBody, waiRequest)

testRequestId :: RequestId
testRequestId =
  fromMaybe (error "invalid composed test request identifier") (mkRequestId "550e8400-e29b-41d4-a716-446655440000")

spec :: Spec
spec = describe "Unit.App.Composed" $ do
  it "keeps admission login names and encrypted TOTP envelopes distinct and redacted" $ do
    let loginName = requiredCsrf "admission login" (mkAdmissionLoginName "support_operator")
        encryptedSecret = requiredCsrf "admission encrypted secret" (mkEncryptedAdmissionTotpSecret "v1-encrypted-envelope")
        principalId = requiredCsrf "admission principal" (mkAdmissionPrincipalId "support-principal")
        sessionId = requiredCsrf "admission session" (mkSessionId "0123456789abcdef0123456789abcdef")
        admissionPrincipal = mkAdmissionPrincipal principalId (mkAdmissionSessionId sessionId) 1234
    expectAll
      ( (show loginName `shouldBe` "AdmissionLoginName <redacted>")
          :| [ show encryptedSecret `shouldBe` "EncryptedAdmissionTotpSecret <redacted>",
               show principalId `shouldBe` "AdmissionPrincipalId <redacted>",
               show (mkAdmissionSessionId sessionId) `shouldBe` "AdmissionSessionId <redacted>",
               show admissionPrincipal `shouldBe` "AdmissionPrincipal <redacted>",
               mkAdmissionLoginName "invalid login" `shouldBe` Nothing,
               mkAdmissionLoginName "" `shouldBe` Nothing,
               mkAdmissionLoginName (Text.replicate 129 "a") `shouldBe` Nothing,
               mkAdmissionPrincipalId "invalid principal" `shouldBe` Nothing,
               mkAdmissionPrincipalId "" `shouldBe` Nothing,
               mkAdmissionPrincipalId (Text.replicate 129 "a") `shouldBe` Nothing,
               mkAdmissionReturnTarget "login" `shouldBe` Just ReturnToAccountLogin,
               mkAdmissionReturnTarget "https://attacker.invalid" `shouldBe` Nothing,
               mkEncryptedAdmissionTotpSecret "" `shouldBe` Nothing,
               mkEncryptedAdmissionTotpSecret (Text.replicate 4097 "a") `shouldBe` Nothing
             ]
      )
    expectDistinctAndPrintable
      [ AdmissionCredentialStoreUnavailable,
        AdmissionCredentialStoreCorrupt
      ]
    expectDistinctAndPrintable
      [ AdmissionAttemptStoreUnavailable,
        AdmissionAttemptStoreCorrupt
      ]
    expectDistinctAndPrintable
      [ AdmissionProofClockUnavailable,
        AdmissionProofClockCorrupt
      ]
    expectDistinctAndPrintable
      [ AdmissionProofRejected,
        AdmissionProofReplayed,
        AdmissionProofThrottled,
        AdmissionProofUnavailable
      ]
    expectDistinctAndPrintable
      [ AdmissionSessionStoreUnavailable,
        AdmissionSessionStoreCorrupt
      ]
    expectDistinctAndPrintable
      [ AdmissionSessionClockUnavailable,
        AdmissionSessionClockCorrupt
      ]
    expectDistinctAndPrintable
      [ AdmissionSessionStoreIssue AdmissionSessionStoreUnavailable,
        AdmissionSessionClockIssue AdmissionSessionClockUnavailable
      ]
    expectDistinctAndPrintable
      [ AdmissionCookieMustUseHostPrefix,
        AdmissionCookieLifetimeMustBeOneDay
      ]
    expectDistinctAndPrintable [AdmissionRequiresConfiguredAuthentication]
    show AdmissionDisabled `shouldBe` "AdmissionDisabled"

  it "uses opaque admission values only for equality and redacted diagnostics" $ do
    let loginName = requiredCsrf "admission login" (mkAdmissionLoginName "support_operator")
        otherLoginName = requiredCsrf "second admission login" (mkAdmissionLoginName "other_operator")
        principalId = requiredCsrf "admission principal" (mkAdmissionPrincipalId "support-principal")
        otherPrincipalId = requiredCsrf "second admission principal" (mkAdmissionPrincipalId "other-principal")
        encryptedSecret = requiredCsrf "admission encrypted secret" (mkEncryptedAdmissionTotpSecret "v1-encrypted-envelope")
        sessionId = requiredCsrf "admission session" (mkSessionId "0123456789abcdef0123456789abcdef")
        principal = mkAdmissionPrincipal principalId (mkAdmissionSessionId sessionId) 1234
        credential = StoredAdmissionCredential principalId encryptedSecret Nothing
    evaluate (loginName == loginName) `shouldReturn` True
    evaluate (loginName == otherLoginName) `shouldReturn` False
    evaluate (compare principalId otherPrincipalId) `shouldReturn` GT
    evaluate (show loginName) `shouldReturn` "AdmissionLoginName <redacted>"
    evaluate (show principalId) `shouldReturn` "AdmissionPrincipalId <redacted>"
    evaluate (show (mkAdmissionSessionId sessionId)) `shouldReturn` "AdmissionSessionId <redacted>"
    evaluate (show encryptedSecret) `shouldReturn` "EncryptedAdmissionTotpSecret <redacted>"
    evaluate (show principal) `shouldReturn` "AdmissionPrincipal <redacted>"
    evaluate (show credential) `shouldReturn` "StoredAdmissionCredential <redacted>"
    map renderDiagnosticValue [DiagnosticValue loginName, DiagnosticValue principalId, DiagnosticValue (mkAdmissionSessionId sessionId), DiagnosticValue encryptedSecret, DiagnosticValue principal, DiagnosticValue credential]
      `shouldBe` [ "AdmissionLoginName <redacted>",
                   "AdmissionPrincipalId <redacted>",
                   "AdmissionSessionId <redacted>",
                   "EncryptedAdmissionTotpSecret <redacted>",
                   "AdmissionPrincipal <redacted>",
                   "StoredAdmissionCredential <redacted>"
                 ]

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

  it "keeps default public/admission helpers inert until the application enables admission" $ do
    let publicModule = buildPublicModule defaultComposedStaticAssets
        trustedClient = TrustedNetworkClient BrowserClient defaultClientAddress
    moduleActionRoute publicModule defaultComposedContext AdmissionActionTarget `shouldBe` Nothing
    moduleDeclaredRoutes publicModule
      `shouldBe` [ Public PublicAdmission,
                   Public PublicLogin,
                   Public (PublicAsset (StaticAssetRoute [requiredPathSegment "public", requiredPathSegment "assets", requiredPathSegment "app.css"])),
                   Public PublicNotFound
                 ]
    show defaultSynchronizerStoragePolicy `shouldBe` "SynchronizerStoragePolicy 16"
    show trustedClient `shouldBe` "TrustedNetworkClient BrowserClient <redacted>"

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
        response <- runRouteDefinition definition Wai.defaultRequest request
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
            testCsrfProtection
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
    loginResponse <- runRouteDefinition (moduleEndpoints rootModule loginRoute) Wai.defaultRequest (RouteRequest loginRoute rootContext)
    assertPageResponse "Login" loginRoute rootContext loginResponse
    notFoundResponse <- runRouteDefinition (moduleEndpoints rootModule notFoundRoute) Wai.defaultRequest (RouteRequest notFoundRoute rootContext)
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
    let composedSite = buildComposedSiteWithSecurity defaultComposedStaticAssets defaultLocalePolicy testCsrfProtection suppliedSecurity catalogQueries catalogCommands ordersQueries ordersCommands
    Site.siteName composedSite `shouldBe` "composed-domains"
    Site.siteDefaultRequestContext composedSite `shouldBe` defaultComposedContext
    case Site.siteRouteModuleChain composedSite of
      Nothing -> expectationFailure "expected a root module chain"
      Just routeChain -> routeChain rootRoute `shouldBe` requiredModuleName "root" :| [requiredModuleName "root.public", requiredModuleName "public"]
    Site.siteNavigationRoutes composedSite `shouldBe` []
    Site.siteHandleClientAction composedSite (ClientActionRequest (CatalogAction RefreshCatalog) Nothing defaultComposedContext)
      `shouldReturn` Just (clientActionResponse Http.status200)
    disabledAdmissionSite <-
      requiredAdmission
        "disabled admission retains configured root security"
        ( buildComposedSiteWithAdmissionSecurity
            defaultComposedStaticAssets
            defaultLocalePolicy
            testCsrfProtection
            AdmissionDisabled
            suppliedSecurity
            catalogQueries
            catalogCommands
            ordersQueries
            ordersCommands
        )
    Site.siteHandleClientAction disabledAdmissionSite (ClientActionRequest (OrdersAction SubmitOrder) Nothing defaultComposedContext)
      `shouldReturn` Just (clientActionResponse Http.status202)
    Site.siteHandleClientAction disabledAdmissionSite (ClientActionRequest (CatalogAction RefreshCatalog) Nothing defaultComposedContext)
      `shouldReturn` Just (clientActionResponse Http.status200)
    case Site.siteSecurity composedSite of
      AuthenticationDisabled [] -> pure ()
      AuthenticationDisabled _ -> expectationFailure "expected no additional public guards"
      AuthenticationEnabled {} -> expectationFailure "expected the supplied public security policy"

  it "places durable admission before account authentication without weakening the public route matrix" $ do
    let admissionPrincipalId = requiredCsrf "admission principal id" (mkAdmissionPrincipalId "beta-operator")
        admissionSessionId = requiredCsrf "admission session id" (mkSessionId "0123456789abcdef0123456789abcdef")
        activeSession = OpaqueSession admissionSessionId admissionPrincipalId 1 2000000000
        activeStore =
          AdmissionSessionStore
            { saveAdmissionSession = \_ -> pure (Right True),
              loadAdmissionSession = \receivedSessionId ->
                pure (Right (if receivedSessionId == mkAdmissionSessionId admissionSessionId then Just activeSession else Nothing)),
              invalidateAdmissionSession = \_ _ -> pure (Right True)
            }
        unavailableStore = activeStore {loadAdmissionSession = \_ -> pure (Left AdmissionSessionStoreUnavailable)}
        expiredStore =
          activeStore
            { loadAdmissionSession = \receivedSessionId ->
                pure
                  ( Right
                      ( if receivedSessionId == mkAdmissionSessionId admissionSessionId
                          then Just (OpaqueSession admissionSessionId admissionPrincipalId 1 500)
                          else Nothing
                      )
                  )
            }
        authenticatedSecurity =
          AuthenticationEnabled
            []
            (AuthenticationGuard (pure . ContinueEndpoint . authenticatedRootContext . endpointRouteRequest))
            []
    activeConfig <- requiredAdmission "admission configuration" (mkAdmissionConfig defaultAdmissionSessionCookiePolicy activeStore (pure (Right 500)))
    unavailableConfig <- requiredAdmission "unavailable admission configuration" (mkAdmissionConfig defaultAdmissionSessionCookiePolicy unavailableStore (pure (Right 500)))
    expiredConfig <- requiredAdmission "expired admission configuration" (mkAdmissionConfig defaultAdmissionSessionCookiePolicy expiredStore (pure (Right 500)))
    unavailableClockConfig <- requiredAdmission "admission clock configuration" (mkAdmissionConfig defaultAdmissionSessionCookiePolicy activeStore (pure (Left AdmissionSessionClockUnavailable)))
    activeSite <-
      requiredAdmission
        "admission-enabled root"
        ( buildComposedSiteWithAdmissionSecurity
            defaultComposedStaticAssets
            defaultLocalePolicy
            admissionCsrfProtection
            (AdmissionEnabled activeConfig unavailableAdmissionProofConfig)
            authenticatedSecurity
            catalogQueries
            catalogCommands
            ordersQueries
            ordersCommands
        )
    unavailableSite <-
      requiredAdmission
        "unavailable admission-enabled root"
        ( buildComposedSiteWithAdmissionSecurity
            defaultComposedStaticAssets
            defaultLocalePolicy
            admissionCsrfProtection
            (AdmissionEnabled unavailableConfig unavailableAdmissionProofConfig)
            authenticatedSecurity
            catalogQueries
            catalogCommands
            ordersQueries
            ordersCommands
        )
    expiredSite <-
      requiredAdmission
        "expired admission-enabled root"
        ( buildComposedSiteWithAdmissionSecurity
            defaultComposedStaticAssets
            defaultLocalePolicy
            admissionCsrfProtection
            (AdmissionEnabled expiredConfig unavailableAdmissionProofConfig)
            authenticatedSecurity
            catalogQueries
            catalogCommands
            ordersQueries
            ordersCommands
        )
    unavailableClockSite <-
      requiredAdmission
        "clock-unavailable admission-enabled root"
        ( buildComposedSiteWithAdmissionSecurity
            defaultComposedStaticAssets
            defaultLocalePolicy
            admissionCsrfProtection
            (AdmissionEnabled unavailableClockConfig unavailableAdmissionProofConfig)
            authenticatedSecurity
            catalogQueries
            catalogCommands
            ordersQueries
            ordersCommands
        )
    case buildComposedSiteWithAdmissionSecurity
      defaultComposedStaticAssets
      defaultLocalePolicy
      admissionCsrfProtection
      (AdmissionEnabled activeConfig unavailableAdmissionProofConfig)
      (AuthenticationDisabled [])
      catalogQueries
      catalogCommands
      ordersQueries
      ordersCommands of
      Left AdmissionRequiresConfiguredAuthentication -> pure ()
      Right _ -> expectationFailure "enabled admission must require configured authentication"
    activeApplication <- toWaiApplication (Site.buildSiteApplication activeSite)
    unavailableApplication <- toWaiApplication (Site.buildSiteApplication unavailableSite)
    expiredApplication <- toWaiApplication (Site.buildSiteApplication expiredSite)
    unavailableClockApplication <- toWaiApplication (Site.buildSiteApplication unavailableClockSite)
    let requestAdaptedContext = Site.siteRequestContextFromRequest activeSite (waiRequest ["es", "catalog"]) testRequestId defaultComposedContext
        requestAdaptedClientAddress =
          case requestClient requestAdaptedContext of
            TrustedNetworkClient _ clientAddress -> Just (clientAddressText clientAddress)
            BrowserClient -> Nothing
            OtherClient -> Nothing
    admissionResponse <- performWaiRequest (pure activeApplication) (waiRequest ["es", "public", "admission"])
    assetResponse <- performWaiRequest (pure activeApplication) (waiRequest ["es", "public", "assets", "app.css"])
    loginChallenge <- performWaiRequest (pure activeApplication) (waiRequest ["es", "public", "login"])
    catalogChallenge <- performWaiRequest (pure activeApplication) (waiRequest ["es", "catalog"])
    ordersChallenge <- performWaiRequest (pure activeApplication) (waiRequest ["es", "orders"])
    notFoundResponse <- performWaiRequest (pure activeApplication) (waiRequest ["es", "public", "404"])
    actionChallenge <- performWaiRequest (pure activeApplication) ((waiRequest ["es", "catalog", "actions", "refresh"]) {Wai.requestMethod = "POST", Wai.requestHeaders = [("X-Harch-Action", "1")]})
    admittedLogin <- performWaiRequest (pure activeApplication) ((waiRequest ["es", "public", "login"]) {Wai.requestHeaders = [(Http.hCookie, "__Host-composed-admission=0123456789abcdef0123456789abcdef")]})
    admittedCatalog <- performWaiRequest (pure activeApplication) ((waiRequest ["es", "catalog"]) {Wai.requestHeaders = [(Http.hCookie, "__Host-composed-admission=0123456789abcdef0123456789abcdef")]})
    admittedOrders <- performWaiRequest (pure activeApplication) ((waiRequest ["es", "orders"]) {Wai.requestHeaders = [(Http.hCookie, "__Host-composed-admission=0123456789abcdef0123456789abcdef")]})
    unavailableResponse <- performWaiRequest (pure unavailableApplication) ((waiRequest ["es", "catalog"]) {Wai.requestHeaders = [(Http.hCookie, "__Host-composed-admission=0123456789abcdef0123456789abcdef")]})
    expiredResponse <- performWaiRequest (pure expiredApplication) ((waiRequest ["es", "catalog"]) {Wai.requestHeaders = [(Http.hCookie, "__Host-composed-admission=0123456789abcdef0123456789abcdef")]})
    unavailableClockResponse <- performWaiRequest (pure unavailableClockApplication) ((waiRequest ["es", "catalog"]) {Wai.requestHeaders = [(Http.hCookie, "__Host-composed-admission=0123456789abcdef0123456789abcdef")]})
    malformedCookieResponse <- performWaiRequest (pure activeApplication) ((waiRequest ["es", "catalog"]) {Wai.requestHeaders = [(Http.hCookie, "__Host-composed-admission=short")]})
    ambiguousCookieResponse <- performWaiRequest (pure activeApplication) ((waiRequest ["es", "catalog"]) {Wai.requestHeaders = [(Http.hCookie, "__Host-composed-admission=0123456789abcdef0123456789abcdef; __Host-composed-admission=0123456789abcdef0123456789abcdef")]})
    actionChallengeBody <- readResponseBody actionChallenge
    expectAll
      ( (Wai.responseStatus admissionResponse `shouldBe` Http.status200)
          :| [ Wai.responseStatus assetResponse `shouldBe` Http.status200,
               Wai.responseStatus loginChallenge `shouldBe` Http.status303,
               Wai.responseStatus catalogChallenge `shouldBe` Http.status303,
               Wai.responseStatus ordersChallenge `shouldBe` Http.status303,
               Wai.responseStatus notFoundResponse `shouldBe` Http.status404,
               Wai.responseStatus actionChallenge `shouldBe` Http.status401,
               lookup Http.hLocation (Wai.responseHeaders loginChallenge) `shouldBe` Just "/es/public/admission",
               lookup Http.hLocation (Wai.responseHeaders catalogChallenge) `shouldBe` Just "/es/public/admission",
               Text.isInfixOf "\"href\":\"/es/public/admission\"" actionChallengeBody `shouldBe` True,
               Wai.responseStatus admittedLogin `shouldBe` Http.status200,
               Wai.responseStatus admittedCatalog `shouldBe` Http.status200,
               Wai.responseStatus admittedOrders `shouldBe` Http.status200,
               Wai.responseStatus unavailableResponse `shouldBe` Http.status503,
               Wai.responseStatus expiredResponse `shouldBe` Http.status303,
               Wai.responseStatus unavailableClockResponse `shouldBe` Http.status503,
               Wai.responseStatus malformedCookieResponse `shouldBe` Http.status303,
               Wai.responseStatus ambiguousCookieResponse `shouldBe` Http.status303,
               requestLocale (requestCore requestAdaptedContext) `shouldBe` locale "es",
               correlationRequestId (requestCorrelation (requestCore requestAdaptedContext)) `shouldBe` Just testRequestId,
               requestAdaptedClientAddress `shouldSatisfy` maybe False (not . Text.null)
             ]
      )

  it "binds composed CSRF to the established admission session and its absolute expiry" $ do
    let principalId = requiredCsrf "admission principal" (mkAdmissionPrincipalId "beta-operator")
        sessionId = requiredCsrf "admission session" (mkSessionId "0123456789abcdef0123456789abcdef")
        admittedContext =
          defaultComposedContext
            { requestLocal = AdmissionEstablished (mkAdmissionPrincipal principalId (mkAdmissionSessionId sessionId) 1234)
            }
    anonymousBinding <- resolveAdmissionCsrfBinding defaultComposedContext
    admittedBinding <- resolveAdmissionCsrfBinding admittedContext
    case (anonymousBinding, admittedBinding) of
      (Csrf.AnonymousCsrfBinding, Csrf.BoundCsrfBinding _ expiresAt) -> expiresAt `shouldBe` 1234
      _ -> expectationFailure "expected anonymous and admission-bound CSRF resolutions"

  it "issues a distinct durable 24-hour admission session only after a confirmed write" $ do
    let principalId = requiredCsrf "admission principal" (mkAdmissionPrincipalId "beta-operator")
    saved <- newIORef Nothing
    let durableStore =
          AdmissionSessionStore
            { saveAdmissionSession = \session -> writeIORef saved (Just session) >> pure (Right True),
              loadAdmissionSession = \_ -> pure (Right Nothing),
              invalidateAdmissionSession = \_ _ -> pure (Right True)
            }
        rejectedStore = durableStore {saveAdmissionSession = \_ -> pure (Right False)}
    durableConfig <- requiredAdmission "durable session configuration" (mkAdmissionConfig defaultAdmissionSessionCookiePolicy durableStore (pure (Right 100)))
    rejectedConfig <- requiredAdmission "rejected session configuration" (mkAdmissionConfig defaultAdmissionSessionCookiePolicy rejectedStore (pure (Right 100)))
    unavailableClockConfig <- requiredAdmission "unavailable-clock admission configuration" (mkAdmissionConfig defaultAdmissionSessionCookiePolicy durableStore (pure (Left AdmissionSessionClockUnavailable)))
    issued <- issueAdmissionSession durableConfig principalId
    case issued of
      Left storeError -> expectationFailure (show storeError)
      Right session -> do
        readIORef saved `shouldReturn` Just session
        unixTimeNanosecondsValue (sessionExpiresAtNanoseconds session) `shouldBe` 86400000000100
        show (mkAdmissionSessionId (sessionId session)) `shouldBe` "AdmissionSessionId <redacted>"
    issueAdmissionSession rejectedConfig principalId `shouldReturn` Left (AdmissionSessionStoreIssue AdmissionSessionStoreCorrupt)
    issueAdmissionSession unavailableClockConfig principalId `shouldReturn` Left (AdmissionSessionClockIssue AdmissionSessionClockUnavailable)
    overflowConfig <- requiredAdmission "overflowing admission session configuration" (mkAdmissionConfig defaultAdmissionSessionCookiePolicy durableStore (pure (Right (unixTimeNanoseconds (maxBound :: Word64)))))
    issueAdmissionSession overflowConfig principalId `shouldReturn` Left (AdmissionSessionStoreIssue AdmissionSessionStoreCorrupt)
    evaluate (show durableConfig) `shouldReturn` "AdmissionConfig <redacted>"
    evaluate (show unavailableAdmissionProofConfig) `shouldReturn` "AdmissionProofConfig <redacted>"
    evaluate (show (AdmissionEnabled durableConfig unavailableAdmissionProofConfig)) `shouldReturn` "AdmissionEnabled <redacted>"

  it "rejects unsafe admission cookie configuration before session issuance" $ do
    let sessionStore =
          AdmissionSessionStore
            { saveAdmissionSession = \_ -> pure (Right True),
              loadAdmissionSession = \_ -> pure (Right Nothing),
              invalidateAdmissionSession = \_ _ -> pure (Right True)
            }
        insecureCookie =
          defaultAdmissionSessionCookiePolicy
            { sessionCookieName = requiredCsrf "insecure cookie name" (mkSessionCookieName "composed-admission")
            }
        shortCookie = defaultAdmissionSessionCookiePolicy {sessionCookieMaxAgeSeconds = 60}
    case mkAdmissionConfig insecureCookie sessionStore (pure (Right 100)) of
      Left AdmissionCookieMustUseHostPrefix -> pure ()
      Left _ -> expectationFailure "expected a host-only cookie-name rejection"
      Right _ -> expectationFailure "expected unsafe cookie configuration to be rejected"
    case mkAdmissionConfig shortCookie sessionStore (pure (Right 100)) of
      Left AdmissionCookieLifetimeMustBeOneDay -> pure ()
      Left _ -> expectationFailure "expected a 24-hour cookie-lifetime rejection"
      Right _ -> expectationFailure "expected unsafe cookie configuration to be rejected"

  it "adapts admission session persistence through parameterized composed-schema queries" $ do
    let principalId = requiredCsrf "admission principal" (mkAdmissionPrincipalId "beta-operator")
        sessionId = requiredCsrf "admission session" (mkSessionId "0123456789abcdef0123456789abcdef")
        session = OpaqueSession sessionId principalId 100 200
    calls <- newIORef ([] :: [(Text, [Text])])
    let runner _ sql parameters = do
          modifyIORef' calls (<> [(sql, parameters)])
          pure
            ( Right
                ( if "SELECT admission_principal_id" `Text.isInfixOf` sql
                    then [["beta-operator", "100", "200"]]
                    else [["0123456789abcdef0123456789abcdef"] | not ("UPDATE composed.admission_sessions" `Text.isInfixOf` sql)]
                )
            )
        store = buildPostgresAdmissionSessionStoreWithRunner runner ()
    saveAdmissionSession store session `shouldReturn` Right True
    loadAdmissionSession store (mkAdmissionSessionId sessionId) `shouldReturn` Right (Just session)
    invalidateAdmissionSession store (mkAdmissionSessionId sessionId) 300 `shouldReturn` Right False
    recordedCalls <- readIORef calls
    map snd recordedCalls
      `shouldBe` [ ["0123456789abcdef0123456789abcdef", "beta-operator", "100", "200"],
                   ["0123456789abcdef0123456789abcdef"],
                   ["0123456789abcdef0123456789abcdef", "300"]
                 ]

  it "loads encrypted admission credentials and atomically rejects replayed TOTP counters" $ do
    let loginName = requiredCsrf "admission login" (mkAdmissionLoginName "support_operator")
        principalId = requiredCsrf "admission principal" (mkAdmissionPrincipalId "beta-operator")
        encryptedSecret = requiredCsrf "encrypted admission secret" (mkEncryptedAdmissionTotpSecret "v1-envelope")
        credential = StoredAdmissionCredential principalId encryptedSecret (Just 4)
    calls <- newIORef ([] :: [(Text, [Text])])
    let runner _ sql parameters = do
          modifyIORef' calls (<> [(sql, parameters)])
          pure (Right [["beta-operator", "v1-envelope", "4"] | "SELECT admission_principal_id" `Text.isInfixOf` sql])
        store = buildPostgresAdmissionCredentialStoreWithRunner runner ()
    loadedCredential <- findAdmissionCredential store loginName
    loadedCredential `shouldBe` Right (Just credential)
    case loadedCredential of
      Right (Just storedCredential) -> show storedCredential `shouldBe` "StoredAdmissionCredential <redacted>"
      _ -> expectationFailure "expected the parameterized credential adapter to load one credential"
    markAdmissionTotpCounterUsed store principalId 4 `shouldReturn` Right False
    readIORef calls
      `shouldReturn` [ ( "SELECT admission_principal_id, encrypted_totp_secret, COALESCE(last_used_totp_counter::TEXT, '') FROM composed.admission_credentials WHERE admission_login_name = $1;",
                         ["support_operator"]
                       ),
                       ( "UPDATE composed.admission_credentials SET last_used_totp_counter = $2::BIGINT WHERE admission_principal_id = $1 AND (last_used_totp_counter IS NULL OR last_used_totp_counter < $2::BIGINT) RETURNING admission_principal_id;",
                         ["beta-operator", "4"]
                       )
                     ]

  it "fails closed for malformed and unavailable admission PostgreSQL rows" $ do
    let loginName = requiredCsrf "admission login" (mkAdmissionLoginName "support_operator")
        principalId = requiredCsrf "admission principal" (mkAdmissionPrincipalId "beta-operator")
        sessionId = requiredCsrf "admission session" (mkSessionId "0123456789abcdef0123456789abcdef")
        session = OpaqueSession sessionId principalId 100 200
        credentialRunner result _ _ _ = pure result
        sessionRunner result _ _ _ = pure result
        credentialStore result = buildPostgresAdmissionCredentialStoreWithRunner (credentialRunner result) ()
        sessionStore result = buildPostgresAdmissionSessionStoreWithRunner (sessionRunner result) ()
    findAdmissionCredential (credentialStore (Left "database unavailable")) loginName `shouldReturn` Left AdmissionCredentialStoreUnavailable
    findAdmissionCredential (credentialStore (Right [["bad id", "envelope", "4"]])) loginName `shouldReturn` Left AdmissionCredentialStoreCorrupt
    findAdmissionCredential (credentialStore (Right [["beta-operator", "", "not-a-counter"]])) loginName `shouldReturn` Left AdmissionCredentialStoreCorrupt
    markAdmissionTotpCounterUsed (credentialStore (Left "database unavailable")) principalId 4 `shouldReturn` Left AdmissionCredentialStoreUnavailable
    markAdmissionTotpCounterUsed (credentialStore (Right [["too", "many"]])) principalId 4 `shouldReturn` Left AdmissionCredentialStoreCorrupt
    saveAdmissionSession (sessionStore (Left "database unavailable")) session `shouldReturn` Left AdmissionSessionStoreUnavailable
    saveAdmissionSession (sessionStore (Right [["too", "many"]])) session `shouldReturn` Left AdmissionSessionStoreCorrupt
    loadAdmissionSession (sessionStore (Left "database unavailable")) (mkAdmissionSessionId sessionId) `shouldReturn` Left AdmissionSessionStoreUnavailable
    loadAdmissionSession (sessionStore (Right [["bad id", "100", "200"]])) (mkAdmissionSessionId sessionId) `shouldReturn` Left AdmissionSessionStoreCorrupt
    loadAdmissionSession (sessionStore (Right [["beta-operator", "not-a-time", "200"]])) (mkAdmissionSessionId sessionId) `shouldReturn` Left AdmissionSessionStoreCorrupt
    invalidateAdmissionSession (sessionStore (Left "database unavailable")) (mkAdmissionSessionId sessionId) 300 `shouldReturn` Left AdmissionSessionStoreUnavailable
    invalidateAdmissionSession (sessionStore (Right [["too", "many"]])) (mkAdmissionSessionId sessionId) 300 `shouldReturn` Left AdmissionSessionStoreCorrupt

  it "preserves empty, written, and conflict outcomes from admission PostgreSQL adapters" $ do
    let loginName = requiredCsrf "admission login" (mkAdmissionLoginName "support_operator")
        principalId = requiredCsrf "admission principal" (mkAdmissionPrincipalId "beta-operator")
        sessionId = requiredCsrf "admission session" (mkSessionId "0123456789abcdef0123456789abcdef")
        session = OpaqueSession sessionId principalId 100 200
        credentialStore result = buildPostgresAdmissionCredentialStoreWithRunner (\_ _ _ -> pure result) ()
        sessionStore result = buildPostgresAdmissionSessionStoreWithRunner (\_ _ _ -> pure result) ()
    findAdmissionCredential (credentialStore (Right [])) loginName `shouldReturn` Right Nothing
    findAdmissionCredential (credentialStore (Right [["beta-operator", "v1-envelope", ""]])) loginName
      `shouldReturn` Right (Just (StoredAdmissionCredential principalId (requiredCsrf "credential envelope" (mkEncryptedAdmissionTotpSecret "v1-envelope")) Nothing))
    markAdmissionTotpCounterUsed (credentialStore (Right [])) principalId 4 `shouldReturn` Right False
    markAdmissionTotpCounterUsed (credentialStore (Right [["beta-operator"]])) principalId 4 `shouldReturn` Right True
    saveAdmissionSession (sessionStore (Right [])) session `shouldReturn` Right False
    loadAdmissionSession (sessionStore (Right [])) (mkAdmissionSessionId sessionId) `shouldReturn` Right Nothing
    invalidateAdmissionSession (sessionStore (Right [["0123456789abcdef0123456789abcdef"]])) (mkAdmissionSessionId sessionId) 300 `shouldReturn` Right True

  it "runs encrypted admission TOTP proof through distinct principal and trusted-peer reservations" $ do
    let loginName = requiredCsrf "admission login" (mkAdmissionLoginName "support_operator")
        principalId = requiredCsrf "admission principal" (mkAdmissionPrincipalId "beta-operator")
        encryptionKey = requiredCsrf "admission encryption key" (mkSecretEncryptionKey "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        secret = requiredCsrf "admission TOTP secret" (mkTotpSecret "JBSWY3DPEHPK3PXPJBSWY3DPEHPK3PXP")
        now = unixTimeNanoseconds 123456000000000
        code = totpCode (unixTimeSeconds 123456) secret
        encryptedSecret =
          requiredCsrf
            "encrypted admission TOTP secret"
            ( mkEncryptedAdmissionTotpSecret
                =<< maybeCryptoError
                  ( encryptSecretWithNonce
                      encryptionKey
                      (requiredCsrf "admission encryption nonce" (mkEncryptionNonce (ByteString.replicate 12 7)))
                      (mkSecretPlaintext (TextEncoding.encodeUtf8 (renderTotpSecret secret)))
                  )
            )
        credential = StoredAdmissionCredential principalId encryptedSecret Nothing
    reservations <- newIORef ([] :: [AdmissionAttemptBudgets])
    settlements <- newIORef ([] :: [Bool])
    markedCounters <- newIORef ([] :: [Word64])
    let attemptStore =
          AdmissionAttemptStore
            { reserveAdmissionAttempt = \budgets _ -> modifyIORef' reservations (<> [budgets]) >> pure (Right (AdmissionAttemptReserved (AdmissionAttemptReservation "reservation-1"))),
              settleAdmissionAttempt = \_ succeeded -> modifyIORef' settlements (<> [succeeded]) >> pure (Right ()),
              cancelAdmissionAttempt = \_ -> pure (Right ())
            }
        credentialStore =
          AdmissionCredentialStore
            { findAdmissionCredential = \receivedLogin -> pure (Right (if receivedLogin == loginName then Just credential else Nothing)),
              markAdmissionTotpCounterUsed = \receivedPrincipal counter -> do
                when (receivedPrincipal == principalId) (modifyIORef' markedCounters (<> [counter]))
                pure (Right True)
            }
        config =
          AdmissionProofConfig
            { admissionProofCredentials = credentialStore,
              admissionProofAttempts = attemptStore,
              admissionProofPolicy = defaultLoginProtectionPolicy,
              admissionProofEncryptionKey = encryptionKey,
              admissionProofReadClock = pure (Right now)
            }
    completeAdmissionProof config defaultClientAddress loginName code `shouldReturn` AdmissionProofAccepted principalId
    recordedReservations <- readIORef reservations
    expectAll
      ( ( map (map (admissionAttemptScopeStorageKey . admissionAttemptScope) . NonEmpty.toList . admissionAttemptBudgetsToList) recordedReservations
            `shouldBe` [["admission-totp:known:beta-operator", "admission-peer:127.0.0.1"]]
        )
          :| [ readIORef settlements `shouldReturn` [True],
               readIORef markedCounters >>= (`shouldSatisfy` (not . null)),
               completeAdmissionProof config defaultClientAddress loginName (requiredCsrf "invalid TOTP code" (mkTotpCode "000000")) `shouldReturn` AdmissionProofRejected,
               readIORef settlements `shouldReturn` [True, False]
             ]
      )
    matchedCounters <- readIORef markedCounters
    matchedCounter <-
      case matchedCounters of
        counter : _ -> pure counter
        [] -> expectationFailure "expected the accepted proof to record a counter" >> fail "unreachable"
    let replayedCredential = credential {storedAdmissionLastUsedTotpCounter = Just matchedCounter}
        replayedConfig =
          config
            { admissionProofCredentials =
                credentialStore
                  { findAdmissionCredential = \_ -> pure (Right (Just replayedCredential))
                  }
            }
        markFalseConfig =
          config
            { admissionProofCredentials =
                credentialStore
                  { markAdmissionTotpCounterUsed = \_ _ -> pure (Right False)
                  }
            }
        markUnavailableConfig =
          config
            { admissionProofCredentials =
                credentialStore
                  { markAdmissionTotpCounterUsed = \_ _ -> pure (Left AdmissionCredentialStoreUnavailable)
                  }
            }
    completeAdmissionProof replayedConfig defaultClientAddress loginName code `shouldReturn` AdmissionProofReplayed
    completeAdmissionProof markFalseConfig defaultClientAddress loginName code `shouldReturn` AdmissionProofReplayed
    completeAdmissionProof markUnavailableConfig defaultClientAddress loginName code `shouldReturn` AdmissionProofUnavailable

  it "uses parameterized PostgreSQL group reservation and settlement for admission proof" $ do
    let loginName = requiredCsrf "admission login" (mkAdmissionLoginName "support_operator")
        principalId = requiredCsrf "admission principal" (mkAdmissionPrincipalId "beta-operator")
        encryptionKey = requiredCsrf "admission encryption key" (mkSecretEncryptionKey "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        secret = requiredCsrf "admission TOTP secret" (mkTotpSecret "JBSWY3DPEHPK3PXPJBSWY3DPEHPK3PXP")
        now = unixTimeNanoseconds 123456000000000
        code = totpCode (unixTimeSeconds 123456) secret
        encryptedSecret =
          requiredCsrf
            "encrypted admission TOTP secret"
            ( mkEncryptedAdmissionTotpSecret
                =<< maybeCryptoError
                  ( encryptSecretWithNonce
                      encryptionKey
                      (requiredCsrf "admission encryption nonce" (mkEncryptionNonce (ByteString.replicate 12 8)))
                      (mkSecretPlaintext (TextEncoding.encodeUtf8 (renderTotpSecret secret)))
                  )
            )
        credential = StoredAdmissionCredential principalId encryptedSecret Nothing
    calls <- newIORef ([] :: [(Text, [Text])])
    let runner _ sql parameters = do
          modifyIORef' calls (<> [(sql, parameters)])
          pure (Right (if "SELECT outcome, value" `Text.isInfixOf` sql then [["reserved", "1"]] else [["1"]]))
        config =
          AdmissionProofConfig
            { admissionProofCredentials =
                AdmissionCredentialStore
                  { findAdmissionCredential = \_ -> pure (Right (Just credential)),
                    markAdmissionTotpCounterUsed = \_ _ -> pure (Right True)
                  },
              admissionProofAttempts = buildPostgresAdmissionAttemptStoreWithRunner defaultAdmissionAttemptStoragePolicy runner (),
              admissionProofPolicy = defaultLoginProtectionPolicy,
              admissionProofEncryptionKey = encryptionKey,
              admissionProofReadClock = pure (Right now)
            }
    completeAdmissionProof config defaultClientAddress loginName code `shouldReturn` AdmissionProofAccepted principalId
    let epochConfig = config {admissionProofReadClock = pure (Right 0)}
        epochCode = totpCode (unixTimeSeconds 0) secret
    completeAdmissionProof epochConfig defaultClientAddress loginName epochCode `shouldReturn` AdmissionProofAccepted principalId
    recordedCalls <- readIORef calls
    case recordedCalls of
      firstCall : _ : epochReservationCall : _ ->
        expectAll
          ( (map (length . snd) recordedCalls `shouldBe` [4, 1, 4, 1])
              :| [ fst firstCall `shouldSatisfy` Text.isInfixOf "composed.reserve_admission_attempt_group",
                   Text.isInfixOf "support_operator" (Text.intercalate " " (concatMap snd recordedCalls)) `shouldBe` False,
                   drop 1 (snd epochReservationCall) `shouldBe` ["0", "0", "10000"]
                 ]
          )
      _ -> expectationFailure "expected reservation and settlement queries for both admission attempts"

  it "interprets every PostgreSQL admission-attempt reservation outcome on the proof rail" $ do
    let loginName = requiredCsrf "admission login" (mkAdmissionLoginName "support_operator")
        principalId = requiredCsrf "admission principal" (mkAdmissionPrincipalId "beta-operator")
        encryptionKey = requiredCsrf "admission encryption key" (mkSecretEncryptionKey "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        secret = requiredCsrf "admission TOTP secret" (mkTotpSecret "JBSWY3DPEHPK3PXPJBSWY3DPEHPK3PXP")
        now = unixTimeNanoseconds 123456000000000
        code = totpCode (unixTimeSeconds 123456) secret
        encryptedSecret =
          requiredCsrf
            "encrypted admission TOTP secret"
            ( mkEncryptedAdmissionTotpSecret
                =<< maybeCryptoError
                  ( encryptSecretWithNonce
                      encryptionKey
                      (requiredCsrf "admission encryption nonce" (mkEncryptionNonce (ByteString.replicate 12 10)))
                      (mkSecretPlaintext (TextEncoding.encodeUtf8 (renderTotpSecret secret)))
                  )
            )
        credential = StoredAdmissionCredential principalId encryptedSecret Nothing
        credentials =
          AdmissionCredentialStore
            { findAdmissionCredential = \_ -> pure (Right (Just credential)),
              markAdmissionTotpCounterUsed = \_ _ -> pure (Right True)
            }
        proofFor runner =
          AdmissionProofConfig
            { admissionProofCredentials = credentials,
              admissionProofAttempts = buildPostgresAdmissionAttemptStoreWithRunner defaultAdmissionAttemptStoragePolicy runner (),
              admissionProofPolicy = defaultLoginProtectionPolicy,
              admissionProofEncryptionKey = encryptionKey,
              admissionProofReadClock = pure (Right now)
            }
        resultRunner result _ _ _ = pure result
    completeAdmissionProof (proofFor (resultRunner (Right [["throttled", "123456000000001"]]))) defaultClientAddress loginName code
      `shouldReturn` AdmissionProofThrottled
    completeAdmissionProof (proofFor (resultRunner (Right [["storage-exhausted", ""]]))) defaultClientAddress loginName code
      `shouldReturn` AdmissionProofUnavailable
    completeAdmissionProof (proofFor (resultRunner (Right [["unexpected", "row"]]))) defaultClientAddress loginName code
      `shouldReturn` AdmissionProofUnavailable
    completeAdmissionProof (proofFor (resultRunner (Left "database unavailable"))) defaultClientAddress loginName code
      `shouldReturn` AdmissionProofUnavailable
    completeAdmissionProof
      ( proofFor
          (\_ sql _ -> pure (Right [["reserved", "1"] | "SELECT outcome" `Text.isInfixOf` sql]))
      )
      defaultClientAddress
      loginName
      (requiredCsrf "invalid TOTP code" (mkTotpCode "000000"))
      `shouldReturn` AdmissionProofUnavailable
    let cancelledReservation = AdmissionAttemptReservation "41"
        cancellationStore result =
          buildPostgresAdmissionAttemptStoreWithRunner
            defaultAdmissionAttemptStoragePolicy
            (\_ _ _ -> pure result)
            ()
    cancelAdmissionAttempt (cancellationStore (Right [])) cancelledReservation
      `shouldReturn` Right ()
    cancelAdmissionAttempt (cancellationStore (Left "database unavailable")) cancelledReservation
      `shouldReturn` Left AdmissionAttemptStoreUnavailable
    let settlementStore result =
          buildPostgresAdmissionAttemptStoreWithRunner
            defaultAdmissionAttemptStoragePolicy
            (\_ _ _ -> pure result)
            ()
    settleAdmissionAttempt (settlementStore (Right [["41"]])) cancelledReservation True
      `shouldReturn` Right ()
    settleAdmissionAttempt (settlementStore (Left "database unavailable")) cancelledReservation False
      `shouldReturn` Left AdmissionAttemptStoreUnavailable
    settleAdmissionAttempt (settlementStore (Right [])) cancelledReservation False
      `shouldReturn` Left AdmissionAttemptStoreCorrupt
    case mkAdmissionAttemptStoragePolicy 0 1 of
      Nothing -> pure ()
      Just _ -> expectationFailure "zero storage capacity must be rejected"
    case mkAdmissionAttemptStoragePolicy 1 0 of
      Nothing -> pure ()
      Just _ -> expectationFailure "zero storage retention must be rejected"
    case mkAdmissionAttemptStoragePolicy 1 1 of
      Nothing -> expectationFailure "positive storage policy must be accepted"
      Just _ -> pure ()

  it "keeps the default root declaration, shell, and mounted declarations complete" $ do
    rootModule <- requiredRootModule
    let rootRoute = Localized (locale "es") (Public PublicLogin)
        englishLoginRoute = Localized (locale "en") (Public PublicLogin)
        assetRoute = StaticAssetRoute [requiredPathSegment "public", requiredPathSegment "assets", requiredPathSegment "app.css"]
        englishAssetRoute = Localized (locale "en") (Public (PublicAsset assetRoute))
        defaultSite = buildComposedSite defaultComposedStaticAssets defaultLocalePolicy testCsrfProtection catalogQueries catalogCommands ordersQueries ordersCommands
        shell = Site.sitePageShell defaultSite (Page "Login" rootRoute (spanishContext defaultComposedContext) (error "page body is not inspected") [])
    moduleName rootModule `shouldBe` requiredModuleName "root"
    moduleDeclaredRoutes rootModule
      `shouldBe` [ Localized (locale "en") (Public PublicAdmission),
                   Localized (locale "en") (Public PublicLogin),
                   Localized (locale "en") (Public (PublicAsset assetRoute)),
                   Localized (locale "en") (Public PublicNotFound),
                   Localized (locale "en") (Catalog CatalogIndex),
                   Localized (locale "en") (Orders OrdersIndex)
                 ]
    map (endpointName . routeMetadata . moduleEndpoints rootModule) (moduleDeclaredRoutes rootModule)
      `shouldBe` [ requiredEndpointName "root.public.admission",
                   requiredEndpointName "root.public.login",
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
        siteResponse <- runRouteDefinition (Site.siteRouteDefinition defaultSite englishLoginRoute) Wai.defaultRequest request
        assertPageResponse "Login" englishLoginRoute (requestContext request) siteResponse
      routeResult -> expectationFailure ("expected the installed root codec to parse the public login route, got " <> show routeResult)
    case parseRoute (Site.siteRouteCodec defaultSite) defaultComposedContext (RouteLocation (staticAssetPathSegments assetRoute) []) of
      RouteParsed request -> do
        requestRoute request `shouldBe` englishAssetRoute
        assetResponse <- runRouteDefinition (Site.siteRouteDefinition defaultSite englishAssetRoute) Wai.defaultRequest request
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
    catalogResponse <- runRouteDefinition (moduleEndpoints rootModule catalogRoute) Wai.defaultRequest (RouteRequest catalogRoute authenticatedContext)
    assertPageResponse "Catalog" catalogRoute authenticatedContext catalogResponse
    ordersResponse <- runRouteDefinition (moduleEndpoints rootModule ordersRoute) Wai.defaultRequest (RouteRequest ordersRoute authenticatedContext)
    assertPageResponse "Orders" ordersRoute authenticatedContext ordersResponse
    readIORef catalogQueryContext `shouldReturn` Just (CatalogContext "es" (Just "catalog.read"))
    readIORef ordersQueryContext `shouldReturn` Just (OrdersContext "es" (Just "catalog.read"))
    moduleHandleAction rootModule (ClientActionRequest (CatalogAction RefreshCatalog) Nothing authenticatedContext)
      `shouldReturn` Just (clientActionResponse Http.status200)
    moduleHandleAction rootModule (ClientActionRequest (OrdersAction SubmitOrder) Nothing authenticatedContext)
      `shouldReturn` Just (clientActionResponse Http.status202)
    readIORef catalogActionContext `shouldReturn` Just (CatalogContext "es" (Just "catalog.read"))
    readIORef ordersActionContext `shouldReturn` Just (OrdersContext "es" (Just "catalog.read"))
    _ <- runRouteDefinition (moduleEndpoints rootModule catalogRoute) Wai.defaultRequest (RouteRequest catalogRoute anonymousContext)
    readIORef catalogQueryContext `shouldReturn` Just (CatalogContext "es" Nothing)

  it "uses the shell locale and parses only bounded locale candidates from WAI" $ do
    let customPolicy = LocalePolicy (locale "es" :| [locale "en"]) (locale "en")
        composedSite = buildComposedSite defaultComposedStaticAssets customPolicy testCsrfProtection catalogQueries catalogCommands ordersQueries ordersCommands
        requestFor path headers = Wai.defaultRequest {Wai.pathInfo = path, Wai.requestHeaders = headers}
        requestContext request = Site.siteRequestContextFromRequest composedSite request testRequestId defaultComposedContext
        shell = Site.sitePageShell composedSite (Page "Catalog" (Localized (locale "es") (Catalog CatalogIndex)) (spanishContext defaultComposedContext) (error "page body is not inspected") [])
    shellNavigationItems shell
      `shouldBe` [ NavigationItem "Sign in" (Localized (locale "es") (Public PublicLogin)),
                   NavigationItem "Catalog" (Localized (locale "es") (Catalog CatalogIndex)),
                   NavigationItem "Orders" (Localized (locale "es") (Orders OrdersIndex))
                 ]
    shellNavigationLifecycle shell `shouldBe` Nothing
    Site.siteNavigationRuntime composedSite `shouldSatisfy` isJust
    let prefixedContext = requestContext (requestFor ["es"] [(Http.hCookie, "locale=en"), (Http.hAcceptLanguage, "en-US")])
    requestLocale (requestCore prefixedContext) `shouldBe` locale "es"
    requestLocaleFallbacks (requestCore prefixedContext) `shouldBe` [locale "es", locale "en"]
    requestLocale (requestCore (requestContext (requestFor [] [(Http.hCookie, "locale=es; theme=dark")]))) `shouldBe` locale "es"
    requestLocale (requestCore (requestContext (requestFor [] [(Http.hAcceptLanguage, "es-MX,en;q=0.8")]))) `shouldBe` locale "es"
    requestLocale (requestCore (requestContext (requestFor [] [(Http.hCookie, ByteString.pack [108, 111, 99, 97, 108, 101, 61, 255]), (Http.hAcceptLanguage, ByteString.pack [255])]))) `shouldBe` locale "en"

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
    moduleActionRoute publicModule publicContext AdmissionActionTarget `shouldBe` Nothing
    moduleHandleAction publicModule (ClientActionRequest (CatalogAction RefreshCatalog) Nothing publicContext) `shouldReturn` Nothing
    let admissionDefinition = moduleEndpoints publicModule (Public PublicAdmission)
        loginDefinition = moduleEndpoints publicModule (Public PublicLogin)
        assetDefinition = moduleEndpoints publicModule (Public (PublicAsset (StaticAssetRoute (routePathSegments assetLocation))))
        missingDefinition = moduleEndpoints publicModule (Public PublicNotFound)
    Routing.routeMethods (moduleRouteCodec publicModule) (Public PublicAdmission) `shouldBe` Routing.routeMethodPolicy [Routing.RouteGet]
    directAdmissionResponse <- runRouteDefinition admissionDefinition Wai.defaultRequest (RouteRequest (Public PublicAdmission) publicContext)
    case directAdmissionResponse of
      PageResponse _ page -> do
        pageTitle page `shouldBe` "Admission"
        renderHtml (pageBody page) `shouldBe` "<section><h1>Admission</h1><p>Admission is not enabled.</p></section>"
      _ -> expectationFailure "expected disabled admission page"
    routeNavigationLabel loginDefinition `shouldBe` Just "Login"
    endpointName (routeMetadata loginDefinition) `shouldBe` requiredEndpointName "root.public.login"
    routeTemplateText (endpointRouteTemplate (routeMetadata loginDefinition)) `shouldBe` "/public/login"
    endpointProtocol (routeMetadata loginDefinition) `shouldBe` HtmlEndpoint
    endpointAccess (routeMetadata loginDefinition) `shouldBe` AllowUnauthenticated
    Site.routeMethods loginDefinition `shouldBe` [Routing.RouteGet]
    routeExecutionPolicy loginDefinition `shouldBe` unboundedRouteExecutionPolicy
    directLoginResponse <- runRouteDefinition loginDefinition Wai.defaultRequest (RouteRequest (Public PublicLogin) publicContext)
    case directLoginResponse of
      PageResponse _ page -> do
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
    directMissingResponse <- runRouteDefinition missingDefinition Wai.defaultRequest (RouteRequest (Public PublicNotFound) publicContext)
    case directMissingResponse of
      PageResponse _ page -> do
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

  it "declares admission form, action, and native fallback only with the enabled admission workflow" $ do
    let admissionSessionStore =
          AdmissionSessionStore
            { saveAdmissionSession = \_ -> pure (Right True),
              loadAdmissionSession = \_ -> pure (Right Nothing),
              invalidateAdmissionSession = \_ _ -> pure (Right True)
            }
        publicContext = defaultComposedContext
        nativeLocation = RouteLocation [requiredPathSegment "public", requiredPathSegment "admission", requiredPathSegment "native"] []
        authenticatedSecurity =
          AuthenticationEnabled
            []
            (AuthenticationGuard (pure . ContinueEndpoint . requestContext . endpointRouteRequest))
            []
    sessionConfig <- requiredAdmission "admission session configuration" (mkAdmissionConfig defaultAdmissionSessionCookiePolicy admissionSessionStore (pure (Right 100)))
    enabledSite <-
      requiredAdmission
        "admission-enabled public routes"
        ( buildComposedSiteWithAdmissionSecurity
            defaultComposedStaticAssets
            defaultLocalePolicy
            admissionCsrfProtection
            (AdmissionEnabled sessionConfig unavailableAdmissionProofConfig)
            authenticatedSecurity
            catalogQueries
            catalogCommands
            ordersQueries
            ordersCommands
        )
    let enabledNativeRoute = Localized (locale "en") (Public PublicAdmissionNativeFallback)
        enabledAdmissionRoute = Localized (locale "en") (Public PublicAdmission)
        disabledSite = buildComposedSite defaultComposedStaticAssets defaultLocalePolicy testCsrfProtection catalogQueries catalogCommands ordersQueries ordersCommands
        admissionDefinition = Site.siteRouteDefinition enabledSite enabledAdmissionRoute
        nativeDefinition = Site.siteRouteDefinition enabledSite enabledNativeRoute
        localizedNativeLocation = RouteLocation (requiredPathSegment "en" : routePathSegments nativeLocation) []
    parseRoute (Site.siteRouteCodec enabledSite) publicContext localizedNativeLocation
      `shouldBe` RouteParsed (RouteRequest enabledNativeRoute publicContext)
    parseRoute (Site.siteRouteCodec disabledSite) publicContext localizedNativeLocation `shouldBe` RouteNotMatched
    endpointName (routeMetadata nativeDefinition) `shouldBe` requiredEndpointName "root.public.admission.native"
    endpointProtocol (routeMetadata nativeDefinition) `shouldBe` ApiEndpoint
    Site.routeMethods nativeDefinition `shouldBe` [Routing.RoutePost]
    renderedAdmission <- runRouteDefinition admissionDefinition Wai.defaultRequest (RouteRequest enabledAdmissionRoute publicContext)
    case renderedAdmission of
      PageResponse _ page ->
        expectAll
          ( (Text.isInfixOf "action=\"/en/public/admission/native\"" (renderHtml (pageBody page)) `shouldBe` True)
              :| [ Text.isInfixOf "name=\"login\"" (renderHtml (pageBody page)) `shouldBe` True,
                   Text.isInfixOf "name=\"code\"" (renderHtml (pageBody page)) `shouldBe` True,
                   Text.isInfixOf "name=\"return\" value=\"login\"" (renderHtml (pageBody page)) `shouldBe` True,
                   Text.isInfixOf "name=\"_harch_csrf\"" (renderHtml (pageBody page)) `shouldBe` True,
                   Text.isInfixOf "autocomplete=\"username\"" (renderHtml (pageBody page)) `shouldBe` True,
                   Text.isInfixOf "autocomplete=\"one-time-code\"" (renderHtml (pageBody page)) `shouldBe` True,
                   Text.isInfixOf "inputmode=\"numeric\"" (renderHtml (pageBody page)) `shouldBe` True
                 ]
          )
      _ -> expectationFailure "expected an admission page"
    Site.siteHandleClientAction enabledSite (ClientActionRequest (SubmitAdmission (requiredCsrf "admission login" (mkAdmissionLoginName "operator")) (requiredCsrf "admission code" (mkTotpCode "123456")) ReturnToAccountLogin) Nothing publicContext)
      `shouldReturn` Just (clientActionResponse Http.status503)

  it "uses the same admission proof rail for a CSRF-verified native fallback and typed cookie redirect" $ do
    let loginName = requiredCsrf "admission login" (mkAdmissionLoginName "support_operator")
        principalId = requiredCsrf "admission principal" (mkAdmissionPrincipalId "beta-operator")
        encryptionKey = requiredCsrf "admission encryption key" (mkSecretEncryptionKey "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
        secret = requiredCsrf "admission TOTP secret" (mkTotpSecret "JBSWY3DPEHPK3PXPJBSWY3DPEHPK3PXP")
        now = unixTimeNanoseconds 123456000000000
        code = totpCode (unixTimeSeconds 123456) secret
        encryptedSecret =
          requiredCsrf
            "encrypted admission TOTP secret"
            ( mkEncryptedAdmissionTotpSecret
                =<< maybeCryptoError
                  ( encryptSecretWithNonce
                      encryptionKey
                      (requiredCsrf "admission encryption nonce" (mkEncryptionNonce (ByteString.replicate 12 9)))
                      (mkSecretPlaintext (TextEncoding.encodeUtf8 (renderTotpSecret secret)))
                  )
            )
        credential = StoredAdmissionCredential principalId encryptedSecret Nothing
        attemptStore =
          AdmissionAttemptStore
            { reserveAdmissionAttempt = \_ _ -> pure (Right (AdmissionAttemptReserved (AdmissionAttemptReservation "reservation-native"))),
              settleAdmissionAttempt = \_ _ -> pure (Right ()),
              cancelAdmissionAttempt = \_ -> pure (Right ())
            }
        proofConfig =
          AdmissionProofConfig
            { admissionProofCredentials =
                AdmissionCredentialStore
                  { findAdmissionCredential = \receivedLogin -> pure (Right (if receivedLogin == loginName then Just credential else Nothing)),
                    markAdmissionTotpCounterUsed = \_ _ -> pure (Right True)
                  },
              admissionProofAttempts = attemptStore,
              admissionProofPolicy = defaultLoginProtectionPolicy,
              admissionProofEncryptionKey = encryptionKey,
              admissionProofReadClock = pure (Right now)
            }
        rejectedProofConfig =
          proofConfig
            { admissionProofCredentials =
                AdmissionCredentialStore
                  { findAdmissionCredential = \_ -> pure (Right Nothing),
                    markAdmissionTotpCounterUsed = \_ _ -> pure (Right True)
                  }
            }
        sessionStore =
          AdmissionSessionStore
            { saveAdmissionSession = \_ -> pure (Right True),
              loadAdmissionSession = \_ -> pure (Right Nothing),
              invalidateAdmissionSession = \_ _ -> pure (Right True)
            }
        authenticatedSecurity =
          AuthenticationEnabled
            []
            (AuthenticationGuard (pure . ContinueEndpoint . requestContext . endpointRouteRequest))
            []
        publicContext = defaultComposedContext
    sessionConfig <- requiredAdmission "admission session configuration" (mkAdmissionConfig defaultAdmissionSessionCookiePolicy sessionStore (pure (Right now)))
    enabledSite <-
      requiredAdmission
        "admission-enabled native fallback"
        ( buildComposedSiteWithAdmissionSecurity
            defaultComposedStaticAssets
            defaultLocalePolicy
            testCsrfProtection
            (AdmissionEnabled sessionConfig proofConfig)
            authenticatedSecurity
            catalogQueries
            catalogCommands
            ordersQueries
            ordersCommands
        )
    rejectedSite <-
      requiredAdmission
        "admission rejection native fallback"
        ( buildComposedSiteWithAdmissionSecurity
            defaultComposedStaticAssets
            defaultLocalePolicy
            testCsrfProtection
            (AdmissionEnabled sessionConfig rejectedProofConfig)
            authenticatedSecurity
            catalogQueries
            catalogCommands
            ordersQueries
            ordersCommands
        )
    unavailableSite <-
      requiredAdmission
        "admission unavailable native fallback"
        ( buildComposedSiteWithAdmissionSecurity
            defaultComposedStaticAssets
            defaultLocalePolicy
            testCsrfProtection
            (AdmissionEnabled sessionConfig unavailableAdmissionProofConfig)
            authenticatedSecurity
            catalogQueries
            catalogCommands
            ordersQueries
            ordersCommands
        )
    csrfRejectedSite <-
      requiredAdmission
        "admission CSRF rejection native fallback"
        ( buildComposedSiteWithAdmissionSecurity
            defaultComposedStaticAssets
            defaultLocalePolicy
            (testCsrfProtection {Csrf.verifyCsrfToken = \_ _ -> pure Csrf.CsrfRejected})
            (AdmissionEnabled sessionConfig proofConfig)
            authenticatedSecurity
            catalogQueries
            catalogCommands
            ordersQueries
            ordersCommands
        )
    csrfUnavailableSite <-
      requiredAdmission
        "admission CSRF unavailable native fallback"
        ( buildComposedSiteWithAdmissionSecurity
            defaultComposedStaticAssets
            defaultLocalePolicy
            (testCsrfProtection {Csrf.verifyCsrfToken = \_ _ -> pure Csrf.CsrfVerificationUnavailable})
            (AdmissionEnabled sessionConfig proofConfig)
            authenticatedSecurity
            catalogQueries
            catalogCommands
            ordersQueries
            ordersCommands
        )
    issuance <- Csrf.issueCsrfToken testCsrfProtection publicContext
    csrfToken <-
      case issuance of
        Csrf.CsrfTokenIssued token _ -> pure token
        Csrf.CsrfProtectionUnavailable -> expectationFailure "expected test CSRF token" >> fail "unreachable"
    let nativeRequestWith body headers = do
          bodyChunks <- newIORef [TextEncoding.encodeUtf8 body]
          pure
            ( Wai.setRequestBodyChunks
                (nextRequestBodyChunk bodyChunks)
                (waiRequest ["en", "public", "admission", "native"])
                  { Wai.requestMethod = "POST",
                    Wai.requestHeaders = (Http.hContentType, "application/x-www-form-urlencoded") : headers
                  }
            )
        csrfHeader = (Http.hCookie, TextEncoding.encodeUtf8 ("__Host-harch-csrf=" <> Csrf.csrfTokenText csrfToken))
        acceptedBody = "login=support_operator&code=" <> totpCodeText code <> "&return=login&_harch_csrf=" <> Csrf.csrfTokenText csrfToken
    nativeRequest <- nativeRequestWith acceptedBody [csrfHeader]
    nativeApplication <- toWaiApplication (Site.buildSiteApplication enabledSite)
    rejectedApplication <- toWaiApplication (Site.buildSiteApplication rejectedSite)
    unavailableApplication <- toWaiApplication (Site.buildSiteApplication unavailableSite)
    csrfRejectedApplication <- toWaiApplication (Site.buildSiteApplication csrfRejectedSite)
    csrfUnavailableApplication <- toWaiApplication (Site.buildSiteApplication csrfUnavailableSite)
    nativeResponse <- performWaiRequest (pure nativeApplication) nativeRequest
    let setCookies = [TextEncoding.decodeUtf8 headerValue | (headerName, headerValue) <- Wai.responseHeaders nativeResponse, headerName == "Set-Cookie"]
    expectAll
      ( (Wai.responseStatus nativeResponse `shouldBe` Http.status303)
          :| [ lookup Http.hLocation (Wai.responseHeaders nativeResponse) `shouldBe` Just "/en/public/login",
               any (Text.isInfixOf "__Host-composed-admission=") setCookies `shouldBe` True,
               any (Text.isInfixOf "__Host-harch-csrf=") setCookies `shouldBe` True
             ]
      )
    rejectedRequest <- nativeRequestWith acceptedBody [csrfHeader]
    rejectedResponse <- performWaiRequest (pure rejectedApplication) rejectedRequest
    unavailableRequest <- nativeRequestWith acceptedBody [csrfHeader]
    unavailableResponse <- performWaiRequest (pure unavailableApplication) unavailableRequest
    csrfRejectedRequest <- nativeRequestWith acceptedBody [csrfHeader]
    csrfRejectedResponse <- performWaiRequest (pure csrfRejectedApplication) csrfRejectedRequest
    csrfUnavailableRequest <- nativeRequestWith acceptedBody [csrfHeader]
    csrfUnavailableResponse <- performWaiRequest (pure csrfUnavailableApplication) csrfUnavailableRequest
    missingCsrfRequest <- nativeRequestWith "login=support_operator&code=123456&return=login" []
    missingCsrfResponse <- performWaiRequest (pure nativeApplication) missingCsrfRequest
    invalidActionRequest <- nativeRequestWith ("login=invalid%20name&code=123456&return=login&_harch_csrf=" <> Csrf.csrfTokenText csrfToken) [csrfHeader]
    invalidActionResponse <- performWaiRequest (pure nativeApplication) invalidActionRequest
    malformedBodyChunks <- newIORef [ByteString.pack [255]]
    let malformedFieldsRequest =
          Wai.setRequestBodyChunks
            (nextRequestBodyChunk malformedBodyChunks)
            ( (waiRequest ["en", "public", "admission", "native"])
                { Wai.requestMethod = "POST",
                  Wai.requestHeaders = [(Http.hContentType, "application/x-www-form-urlencoded")]
                }
            )
    malformedFieldsResponse <- performWaiRequest (pure nativeApplication) malformedFieldsRequest
    tooManyFieldsRequest <- nativeRequestWith (Text.intercalate "&" (replicate 9 "noise=value")) []
    tooManyFieldsResponse <- performWaiRequest (pure nativeApplication) tooManyFieldsRequest
    tooLargeRequest <- nativeRequestWith (Text.replicate 9000 "x") []
    tooLargeResponse <- performWaiRequest (pure nativeApplication) tooLargeRequest
    emptyBodyRequest <- nativeRequestWith "" []
    emptyBodyResponse <- performWaiRequest (pure nativeApplication) emptyBodyRequest
    acceptedAction <- Site.siteHandleClientAction enabledSite (ClientActionRequest (SubmitAdmission loginName code ReturnToAccountLogin) Nothing publicContext)
    rejectedAction <- Site.siteHandleClientAction rejectedSite (ClientActionRequest (SubmitAdmission loginName code ReturnToAccountLogin) Nothing publicContext)
    unavailableAction <- Site.siteHandleClientAction unavailableSite (ClientActionRequest (SubmitAdmission loginName code ReturnToAccountLogin) Nothing publicContext)
    expectAll
      ( (Wai.responseStatus rejectedResponse `shouldBe` Http.status422)
          :| [ Wai.responseStatus unavailableResponse `shouldBe` Http.status503,
               Wai.responseStatus csrfRejectedResponse `shouldBe` Http.status403,
               Wai.responseStatus csrfUnavailableResponse `shouldBe` Http.status503,
               Wai.responseStatus missingCsrfResponse `shouldBe` Http.status403,
               Wai.responseStatus invalidActionResponse `shouldBe` Http.status422,
               Wai.responseStatus malformedFieldsResponse `shouldBe` Http.status422,
               Wai.responseStatus tooManyFieldsResponse `shouldBe` Http.status413,
               Wai.responseStatus tooLargeResponse `shouldBe` Http.status413,
               Wai.responseStatus emptyBodyResponse `shouldBe` Http.status403
             ]
      )
    case acceptedAction of
      Just actionResponse ->
        expectAll
          ( (clientActionStatus actionResponse `shouldBe` Http.status200)
              :| [ clientActionNavigation actionResponse `shouldBe` NavigateInternal ReplaceHistory (RouteRequest (Localized (locale "en") (Public PublicLogin)) publicContext),
                   length (clientActionHeaders actionResponse) `shouldBe` 2
                 ]
          )
      Nothing -> expectationFailure "expected enabled admission action response"
    case rejectedAction of
      Just actionResponse ->
        expectAll
          ( (clientActionStatus actionResponse `shouldBe` Http.status422)
              :| [ clientActionNavigation actionResponse `shouldBe` StayOnCurrentRoute,
                   clientActionHeaders actionResponse `shouldBe` []
                 ]
          )
      Nothing -> expectationFailure "expected rejected admission action response"
    case unavailableAction of
      Just actionResponse ->
        expectAll
          ( (clientActionStatus actionResponse `shouldBe` Http.status503)
              :| [ clientActionNavigation actionResponse `shouldBe` StayOnCurrentRoute,
                   clientActionHeaders actionResponse `shouldBe` []
                 ]
          )
      Nothing -> expectationFailure "expected unavailable admission action response"

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
                      EndpointOptions -> pure (HaltEndpoint (NonPageBodyResponse guardResponseBody))
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
        runEndpointGuard localizedGuard (endpointRequest EndpointOptions) `shouldReturn` HaltEndpoint (NonPageBodyResponse guardResponseBody)
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
            { routeHandler = PageRouteHandler $ \_ localRequest ->
                pure
                  ( RenderedPage
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
    response <- runRouteDefinition (moduleEndpoints localizedModule rootRoute) Wai.defaultRequest (RouteRequest rootRoute defaultComposedContext)
    case response of
      PageResponse _ page -> do
        pageTitle page `shouldBe` "Spanish child"
        pageRoute page `shouldBe` rootRoute
        pageContext page `shouldBe` defaultComposedContext
      _ -> expectationFailure "expected the localized context-aware page"

  it "preserves a non-page halt from a localized local guard" $ do
    let rootContext = spanishContext defaultComposedContext
        rootRoute = Localized (locale "es") (Public PublicLogin)
        guard =
          EndpointGuard $ \_ ->
            pure
              ( HaltEndpoint
                  (NonPageBodyResponse guardResponseBody)
              )
        publicModule = (buildPublicModule defaultComposedStaticAssets) {moduleGuards = [guard]}
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
          HaltEndpoint (NonPageBodyResponse responseBodyValue) -> responseBodyValue `shouldBe` guardResponseBody
          unexpectedResult -> expectationFailure ("expected a localized guarded response, got " <> show unexpectedResult)
      _ -> expectationFailure "expected exactly one localized guard"

-- | Deterministic security is used only to inspect how a halted page response
-- is remapped by module composition.  Production page security is constructed
-- by the site after the request has reached its page route.
testPageSecurity :: PageSecurity
testPageSecurity =
  mkPageSecurity testRuntimeNonce (mkPageCsrf testCsrfToken "composed-test")
  where
    testCsrfToken =
      case mkCsrfToken "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" of
        Just csrfToken -> csrfToken
        Nothing -> error "invalid test CSRF token"

-- | Exercise a route definition's declared handler without bypassing the
-- page-security argument.  Production requests use 'Site' to construct this
-- value from the selected CSRF backend; these composition tests use the fixed
-- redacted fixture solely to inspect route/context mapping.
runRouteDefinition :: RouteDefinition route context authorization -> Wai.Request -> RouteRequest route context -> IO (Response route context)
runRouteDefinition definition request routeRequest =
  case routeHandler definition of
    PageRouteHandler renderPage -> do
      pageResult <- renderPage testPageSecurity routeRequest
      pure $
        case pageResult of
          RenderedPage page -> PageResponse testPageSecurity page
          RenderedPageWithMetadata responseBodyValue page -> PageResponseWithMetadata testPageSecurity responseBodyValue page
    ProtocolRouteHandler renderProtocol -> nonPageResponse <$> renderProtocol request routeRequest

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
  pure (buildComposedSite defaultComposedStaticAssets defaultLocalePolicy testCsrfProtection catalogQueries catalogCommands ordersQueries ordersCommands)

testCsrfProtection :: Csrf.CsrfProtection ComposedContext
testCsrfProtection =
  Csrf.signedCsrfProtection
    keyring
    Csrf.defaultSignedCsrfPolicy
    (pure 1000000000)
    (const (pure Csrf.AnonymousCsrfBinding))
  where
    keyId = requiredCsrf "test CSRF key id" (Csrf.mkCsrfKeyId "composed-test-v1")
    signingKey = requiredCsrf "test CSRF signing key" (Csrf.mkCsrfSigningKey "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
    keyring = requiredCsrf "test CSRF keyring" (Csrf.mkSignedCsrfKeyring keyId ((keyId, signingKey) :| []))

admissionCsrfProtection :: Csrf.CsrfProtection ComposedContext
admissionCsrfProtection =
  Csrf.signedCsrfProtection
    keyring
    Csrf.defaultSignedCsrfPolicy
    (pure 1000000000)
    resolveAdmissionCsrfBinding
  where
    keyId = requiredCsrf "admission CSRF key id" (Csrf.mkCsrfKeyId "composed-test-v1")
    signingKey = requiredCsrf "admission CSRF signing key" (Csrf.mkCsrfSigningKey "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
    keyring = requiredCsrf "admission CSRF keyring" (Csrf.mkSignedCsrfKeyring keyId ((keyId, signingKey) :| []))

requiredCsrf :: String -> Maybe value -> value
requiredCsrf label = fromMaybe (error ("expected " <> label))

requiredAdmission :: String -> Either failure value -> IO value
requiredAdmission label = either (\_ -> expectationFailure ("expected " <> label) >> fail "unreachable") pure

unavailableAdmissionProofConfig :: AdmissionProofConfig
unavailableAdmissionProofConfig =
  AdmissionProofConfig
    { admissionProofCredentials =
        AdmissionCredentialStore
          { findAdmissionCredential = \_ -> pure (Left AdmissionCredentialStoreUnavailable),
            markAdmissionTotpCounterUsed = \_ _ -> pure (Left AdmissionCredentialStoreUnavailable)
          },
      admissionProofAttempts =
        AdmissionAttemptStore
          { reserveAdmissionAttempt = \_ _ -> pure (Left AdmissionAttemptStoreUnavailable),
            settleAdmissionAttempt = \_ _ -> pure (Left AdmissionAttemptStoreUnavailable),
            cancelAdmissionAttempt = \_ -> pure (Left AdmissionAttemptStoreUnavailable)
          },
      admissionProofPolicy = defaultLoginProtectionPolicy,
      admissionProofEncryptionKey = requiredCsrf "test admission encryption key" (mkSecretEncryptionKey "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"),
      admissionProofReadClock = pure (Left AdmissionProofClockUnavailable)
    }

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

data DiagnosticValue = forall value. (Show value) => DiagnosticValue value

renderDiagnosticValue :: DiagnosticValue -> String
renderDiagnosticValue (DiagnosticValue value) = show value

assertRootLocalEqual :: RootLocal -> RootLocal -> Expectation
assertRootLocalEqual leftRootLocal rightRootLocal = leftRootLocal `shouldBe` rightRootLocal

rootLocalsDiffer :: RootLocal -> RootLocal -> Bool
rootLocalsDiffer leftRootLocal rightRootLocal = leftRootLocal /= rightRootLocal

assertPageResponse :: Text -> RootRoute -> ComposedContext -> Response RootRoute ComposedContext -> Expectation
assertPageResponse expectedTitle expectedRoute expectedContext response =
  case response of
    PageResponse _ page -> do
      pageTitle page `shouldBe` expectedTitle
      pageRoute page `shouldBe` expectedRoute
      pageContext page `shouldBe` expectedContext
    _ -> expectationFailure "expected a page response"

clientActionResponse :: Http.Status -> ClientActionResponse RootRoute ComposedContext
clientActionResponse status =
  ClientActionResponse
    { clientActionStatus = status,
      clientActionPatches = [],
      clientActionFocusId = Nothing,
      clientActionNavigation = StayOnCurrentRoute,
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
