{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Exception (ErrorCall (..), evaluate, try)
import Control.Monad (forM_)
import Data.ByteString.Char8 qualified as ByteString
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Action qualified as Action
import HarchWeb.ApplicationModule
import HarchWeb.Document (Page (..))
import HarchWeb.Document qualified as Document
import HarchWeb.EndpointMetadata qualified as EndpointMetadata
import HarchWeb.EndpointSecurity
import HarchWeb.Markup (safeUrlText, text)
import HarchWeb.Routing
import HarchWeb.Routing qualified as Routing
import HarchWeb.SecurityEvent (ModuleName, mkModuleName)
import HarchWeb.Server (ClientActionRequest (..), ClientActionResponse (..), ProtocolResponse (..), ProtocolResponseBody (..), Response (..), ResponseBody (..), ServerSentEventSource (..), unboundedRouteExecutionPolicy)
import HarchWeb.Site (RouteDefinition (..))
import HarchWeb.Site qualified as Site
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai

data ParentRoute
  = CatalogRoute ChildRoute
  | ParentOtherRoute
  | ParentUnownedRoute
  deriving (Eq, Show)

data ChildRoute = ChildItemRoute
  deriving (Eq, Show)

data ParentAction
  = CatalogAction ChildAction
  | ParentOtherAction
  deriving (Eq, Show)

data ChildAction = SaveChildItem
  deriving (Eq, Show)

data ParentActionTarget = ParentSaveTarget
  deriving (Eq, Show)

data ChildActionTarget = ChildSaveTarget
  deriving (Eq, Show)

data ParentAuthorization = ParentCanSave
  deriving (Eq, Show)

data ChildAuthorization = ChildCanSave
  deriving (Eq, Show)

data ChildResponseKind
  = ChildPageResponse
  | ChildPageResponseWithMetadata
  | ChildBodyResponse
  | ChildRedirectResponse
  | ChildActionBodyResponse
  | ChildEventStreamResponse
  | ChildProtocolResponse
  deriving (Bounded, Enum, Eq, Show)

spec =
  describe "HarchWeb.ApplicationModule" $ do
    it "fails fast only for an invalid static module declaration" $ do
      requiredModuleConfiguration (Right 42 :: Either Text Int) `shouldBe` 42
      evaluate (requiredModuleConfiguration (Left "duplicate route" :: Either Text Int))
        `shouldThrow` \case
          ErrorCall message -> message == "invalid application-module declaration: \"duplicate route\""

    it "keeps mount and composition construction errors comparable and printable" $ do
      let mountMetadataError = InvalidMountedEndpointMetadata EndpointMetadata.InvalidRouteTemplate
          mountActionError = InvalidMountedActionCodec (Action.InvalidActionEndpointMetadata EndpointMetadata.InvalidRouteTemplate)
          duplicateModuleError = DuplicateModuleName (requiredModuleName "root.catalog")
          duplicateEndpointError = DuplicateModuleEndpointName (EndpointMetadata.requiredEndpointNameOrDie "root.catalog.item")
      expectAll
        ( (mountMetadataError /= mountActionError `shouldBe` True)
            :| [ show mountMetadataError `shouldSatisfy` not . null,
                 showsPrec 11 mountActionError "" `shouldSatisfy` not . null,
                 showList [mountMetadataError, mountActionError] "" `shouldSatisfy` not . null,
                 duplicateModuleError /= duplicateEndpointError `shouldBe` True,
                 showsPrec 11 duplicateModuleError "" `shouldSatisfy` not . null,
                 showList [duplicateModuleError, duplicateEndpointError] "" `shouldSatisfy` not . null
               ]
        )

    it "adapts child routes, endpoint metadata, actions, and guards to the parent algebra" $ do
      actionContext <- newIORef Nothing
      guardContext <- newIORef Nothing
      let moduleMount = testModuleMount
          childModule = buildChildModule actionContext guardContext
      mountedModule <-
        case mountApplicationModule moduleMount childModule of
          Left mountError -> expectationFailure (show mountError) >> fail "could not mount test module"
          Right mounted -> pure mounted
      let parentRequest = RouteRequest (CatalogRoute ChildItemRoute) 42
          mountedDefinition = moduleEndpoints mountedModule (CatalogRoute ChildItemRoute)
          metadata = routeMetadata mountedDefinition
      moduleOwnsRoute mountedModule (CatalogRoute ChildItemRoute) `shouldBe` True
      moduleOwnsRoute mountedModule ParentOtherRoute `shouldBe` False
      moduleRouteMountChain mountedModule (CatalogRoute ChildItemRoute)
        `shouldBe` requiredModuleName "root.catalog"
        :| [requiredModuleName "catalog"]
      routeLocationText (renderRoute (moduleRouteCodec mountedModule) parentRequest) `shouldBe` "/catalog/item"
      parseRoute (moduleRouteCodec mountedModule) 42 (testRouteLocation "/catalog/item")
        `shouldBe` RouteParsed parentRequest
      EndpointMetadata.endpointNameText (EndpointMetadata.endpointName metadata) `shouldBe` "root.catalog.item"
      EndpointMetadata.routeTemplateText (EndpointMetadata.endpointRouteTemplate metadata) `shouldBe` "/catalog/item"
      EndpointMetadata.endpointProtocol metadata `shouldBe` EndpointMetadata.HtmlEndpoint
      EndpointMetadata.endpointAccess metadata `shouldBe` EndpointMetadata.RequireAuthorized ParentCanSave
      Action.actionPath (moduleActionCodec mountedModule) 42 ParentSaveTarget `shouldBe` Just "/catalog/tenant-42/save"
      Action.actionEndpointMetadata (moduleActionCodec mountedModule) 42 "POST" "/catalog/tenant-42/save"
        `shouldBe` Just mountedChildActionMetadata
      Site.routeNavigationLabel mountedDefinition `shouldBe` Just "Catalog"
      Site.routeMethods mountedDefinition `shouldBe` [RouteGet]
      Site.routeExecutionPolicy mountedDefinition `shouldBe` unboundedRouteExecutionPolicy
      let mountedNotFoundRequest = notFoundRequest (moduleRouteCodec mountedModule) 42
      mountedNotFoundRequest `shouldBe` RouteRequest (CatalogRoute ChildItemRoute) 42
      requestRoute mountedNotFoundRequest `shouldBe` CatalogRoute ChildItemRoute
      requestContext mountedNotFoundRequest `shouldBe` 42
      response <- routeResponse mountedDefinition Wai.defaultRequest parentRequest
      case response of
        PageResponse page -> do
          Document.pageRoute page `shouldBe` CatalogRoute ChildItemRoute
          pageContext page `shouldBe` 42
        _ -> expectationFailure "expected mounted page response"
      actionResult <-
        moduleHandleAction
          mountedModule
          ClientActionRequest
            { clientAction = CatalogAction SaveChildItem,
              clientActionRequestIdempotencyKey = Nothing,
              clientActionContext = 42
            }
      actionResult `shouldBe` Nothing
      readIORef actionContext `shouldReturn` Just "tenant-42"
      moduleHandleAction
        mountedModule
        ClientActionRequest
          { clientAction = CatalogAction SaveChildItem,
            clientActionRequestIdempotencyKey = Just "retry-1",
            clientActionContext = 42
          }
        `shouldReturn` Nothing
      readIORef actionContext `shouldReturn` Just "tenant-42-retry-1"
      let endpointRequest =
            EndpointRequest
              { endpointWaiRequest = Wai.defaultRequest,
                endpointRouteRequest = parentRequest,
                endpointMetadata = metadata,
                endpointSecurityEventSink = Nothing,
                endpointDispatchKind = EndpointMatched
              }
      case moduleGuards mountedModule of
        [mountedGuard] -> do
          guardResult <- runEndpointGuard mountedGuard endpointRequest
          guardResult `shouldBe` ContinueEndpoint 42
          readIORef guardContext `shouldReturn` Just "tenant-42"
        _ -> expectationFailure "expected exactly one mounted guard"

    it "rejects endpoint identities made invalid by a mount namespace" $ do
      actionContext <- newIORef Nothing
      guardContext <- newIORef Nothing
      let oversizedMount =
            testModuleMount
              { mountedRoutes =
                  (mountedRoutes testModuleMount)
                    { routeMountName = requiredModuleName (Text.replicate 128 "a")
                    }
              }
      case mountApplicationModule oversizedMount (buildChildModule actionContext guardContext) of
        Left mountError -> mountError `shouldBe` InvalidMountedEndpointMetadata EndpointMetadata.EndpointNameTooLong
        Right _ -> expectationFailure "expected the oversized mounted endpoint name to be rejected"

    it "rejects mounted route metadata that would exceed the declared template budget" $ do
      actionContext <- newIORef Nothing
      guardContext <- newIORef Nothing
      let longChildTemplate = "/" <> Text.replicate 254 "x"
          childModule =
            (buildChildModule actionContext guardContext)
              { moduleEndpoints = const (childDefinitionWithAccess EndpointMetadata.AllowUnauthenticated longChildTemplate)
              }
      case mountApplicationModule testModuleMount childModule of
        Left mountError -> mountError `shouldBe` InvalidMountedEndpointMetadata EndpointMetadata.RouteTemplateTooLong
        Right _ -> expectationFailure "expected the mounted route template to be rejected"

    it "fails when a dynamically selected child definition violates already-validated mount metadata" $ do
      actionContext <- newIORef Nothing
      guardContext <- newIORef Nothing
      let longChildTemplate = "/" <> Text.replicate 254 "x"
          undeclaredDynamicModule =
            (buildChildModule actionContext guardContext)
              { moduleDeclaredRoutes = [],
                moduleEndpoints = const (childDefinitionWithAccess EndpointMetadata.AllowUnauthenticated longChildTemplate)
              }
      mountedModule <- requireMountedModule testModuleMount undeclaredDynamicModule
      metadataFailure <-
        try (evaluate (routeMetadata (moduleEndpoints mountedModule (CatalogRoute ChildItemRoute)))) ::
          IO (Either ErrorCall (EndpointMetadata.EndpointMetadata ParentAuthorization))
      assertErrorCall metadataFailure "undeclared endpoint metadata used by an application module: InvalidMountedEndpointMetadata RouteTemplateTooLong"

    it "keeps route and action mount construction failures distinguishable" $ do
      actionContext <- newIORef Nothing
      guardContext <- newIORef Nothing
      let actionOnlyModule =
            (buildChildModule actionContext guardContext)
              { moduleDeclaredRoutes = [],
                moduleActionCodec =
                  Action.singleActionCodecWithMetadata
                    ChildSaveTarget
                    (Action.post "/save")
                    childMetadata
                    (pure SaveChildItem),
                moduleEndpoints = \_ -> error "an action-only module must not select a route definition"
              }
          oversizedMount =
            testModuleMount
              { mountedRoutes =
                  (mountedRoutes testModuleMount)
                    { routeMountName = requiredModuleName (Text.replicate 128 "a")
                    }
              }
      case mountApplicationModule oversizedMount actionOnlyModule of
        Left mountError -> mountError `shouldBe` InvalidMountedActionCodec (Action.InvalidActionEndpointMetadata EndpointMetadata.EndpointNameTooLong)
        Right _ -> expectationFailure "expected an invalid mounted action declaration"

    it "maps every response shape without allowing a child route or context to escape its mount" $ do
      forM_ [minBound .. maxBound] $ \responseKind -> do
        actionContext <- newIORef Nothing
        guardContext <- newIORef Nothing
        mountedModule <-
          requireMountedModule
            testModuleMount
            ( (buildChildModule actionContext guardContext)
                { moduleEndpoints = const (childDefinitionWithResponse responseKind)
                }
            )
        let parentRequest = RouteRequest (CatalogRoute ChildItemRoute) 7
        response <- routeResponse (moduleEndpoints mountedModule (CatalogRoute ChildItemRoute)) Wai.defaultRequest parentRequest
        assertMountedResponse responseKind response

    it "preserves all declared access requirements and prefixes a child root endpoint exactly once" $ do
      actionContext <- newIORef Nothing
      guardContext <- newIORef Nothing
      let childModule =
            (buildChildModule actionContext guardContext)
              { moduleEndpoints = childDefinitionForAccess,
                moduleDeclaredRoutes = [ChildItemRoute]
              }
      mountedModule <- requireMountedModule testModuleMount childModule
      let metadata = routeMetadata (moduleEndpoints mountedModule (CatalogRoute ChildItemRoute))
      EndpointMetadata.endpointAccess metadata `shouldBe` EndpointMetadata.AllowUnauthenticated
      EndpointMetadata.routeTemplateText (EndpointMetadata.endpointRouteTemplate metadata) `shouldBe` "/catalog"

      authenticatedModule <-
        requireMountedModule
          testModuleMount
          (childModule {moduleEndpoints = const (childDefinitionWithAccess EndpointMetadata.RequireAuthenticated "/item")})
      EndpointMetadata.endpointAccess (routeMetadata (moduleEndpoints authenticatedModule (CatalogRoute ChildItemRoute)))
        `shouldBe` EndpointMetadata.RequireAuthenticated

    it "keeps mount misses, malformed child input, hidden methods, and incorrect ownership explicit" $ do
      let mountedCodec =
            mountRouteCodec
              (mountedRoutes testModuleMount)
              (mountedContext testModuleMount)
              (RouteRequest ParentOtherRoute)
              malformedChildCodec
      parseRoute mountedCodec 42 (testRouteLocation "/other") `shouldBe` RouteNotMatched
      parseRoute mountedCodec 42 (testRouteLocation "/catalog/missing") `shouldBe` RouteNotMatched
      parseRoute mountedCodec 42 (testRouteLocation "/catalog/malformed") `shouldBe` RouteMalformed InvalidRouteTargetEncoding
      notFoundRequest mountedCodec 42 `shouldBe` RouteRequest ParentOtherRoute 42
      Routing.routeMethods mountedCodec ParentOtherRoute `shouldBe` RouteHidden
      Routing.routeMethods mountedCodec (CatalogRoute ChildItemRoute) `shouldBe` routeMethodPolicy [RouteGet]
      let nestedPrefixMount =
            (mountedRoutes testModuleMount)
              { routeMountPrefix = requiredPathSegment "catalog" :| [requiredPathSegment "item"]
              }
          nestedPrefixCodec =
            mountRouteCodec
              nestedPrefixMount
              (mountedContext testModuleMount)
              (RouteRequest ParentOtherRoute)
              malformedChildCodec
      parseRoute nestedPrefixCodec 42 (testRouteLocation "/catalog") `shouldBe` RouteNotMatched
      renderFailure <- try (evaluate (renderRoute mountedCodec (RouteRequest ParentOtherRoute 42))) :: IO (Either ErrorCall RouteLocation)
      case renderFailure of
        Left failure -> show failure `shouldBe` "attempted to render a route through a mount that does not own it"
        Right _ -> expectationFailure "expected rendering another module's route to fail"

      actionContext <- newIORef Nothing
      guardContext <- newIORef Nothing
      mountedModule <- requireMountedModule testModuleMount (buildChildModule actionContext guardContext)
      definitionFailure <- try (evaluate (moduleEndpoints mountedModule ParentOtherRoute)) :: IO (Either ErrorCall (RouteDefinition ParentRoute Int ParentAuthorization))
      case definitionFailure of
        Left failure -> show failure `shouldBe` "attempted to select a route definition through a mount that does not own it"
        Right _ -> expectationFailure "expected selecting another module's definition to fail"
      routeObservationFailure <- try (evaluate (moduleRouteMountChain mountedModule ParentOtherRoute)) :: IO (Either ErrorCall (NonEmpty ModuleName))
      case routeObservationFailure of
        Left failure -> show failure `shouldBe` "attempted to select a route observation through a mount that does not own it"
        Right _ -> expectationFailure "expected selecting another module's route observation to fail"

    it "uses the projected context for child parse and render behavior" $ do
      actionContext <- newIORef Nothing
      guardContext <- newIORef Nothing
      let contextualCodec =
            childCodec
              { parseRoute = \childContext location ->
                  if childContext == "tenant-7" && routeLocationText location == "/item"
                    then RouteParsed (RouteRequest ChildItemRoute childContext)
                    else RouteNotMatched,
                renderRoute = \request ->
                  if requestContext request == "tenant-7"
                    then testRouteLocation "/item"
                    else testRouteLocation "/wrong-context",
                notFoundRequest = \childContext ->
                  if childContext == "tenant-7"
                    then RouteRequest ChildItemRoute childContext
                    else error "unexpected projected context for child not-found route"
              }
          contextualModule =
            (buildChildModule actionContext guardContext)
              { moduleRouteCodec = contextualCodec
              }
      mountedModule <- requireMountedModule testModuleMount contextualModule
      let mountedCodec = moduleRouteCodec mountedModule
      parseRoute mountedCodec 7 (testRouteLocation "/catalog/item")
        `shouldBe` RouteParsed (RouteRequest (CatalogRoute ChildItemRoute) 7)
      parseRoute mountedCodec 8 (testRouteLocation "/catalog/item")
        `shouldBe` RouteNotMatched
      renderRoute mountedCodec (RouteRequest (CatalogRoute ChildItemRoute) 7)
        `shouldBe` testRouteLocation "/catalog/item"
      notFoundRequest mountedCodec 7 `shouldBe` RouteRequest (CatalogRoute ChildItemRoute) 7

    it "forwards the original WAI request and projected context to a child route response" $ do
      actionContext <- newIORef Nothing
      guardContext <- newIORef Nothing
      let requestAwareDefinition =
            (childDefinition ChildItemRoute)
              { routeResponse = \waiRequest childRequest ->
                  pure
                    ( PageResponse
                        Page
                          { pageTitle = Text.pack (ByteString.unpack (Wai.requestMethod waiRequest)) <> ":" <> requestContext childRequest,
                            pageRoute = requestRoute childRequest,
                            pageContext = requestContext childRequest,
                            pageBody = text "Item",
                            pageBootstrapHooks = []
                          }
                    )
              }
          childModule =
            (buildChildModule actionContext guardContext)
              { moduleEndpoints = \case
                  ChildItemRoute -> requestAwareDefinition
              }
      mountedModule <- requireMountedModule testModuleMount childModule
      response <-
        routeResponse
          (moduleEndpoints mountedModule (CatalogRoute ChildItemRoute))
          (Wai.defaultRequest {Wai.requestMethod = "PATCH"})
          (RouteRequest (CatalogRoute ChildItemRoute) 7)
      case response of
        PageResponse page -> do
          pageTitle page `shouldBe` "PATCH:tenant-7"
          Document.pageRoute page `shouldBe` CatalogRoute ChildItemRoute
          pageContext page `shouldBe` 7
        _ -> expectationFailure "expected mounted page response"

    it "scopes child guards to their mount and maps a child halt through the parent response algebra" $ do
      actionContext <- newIORef Nothing
      guardContext <- newIORef Nothing
      guardFacts <- newIORef Nothing
      let childModule =
            (buildChildModule actionContext guardContext)
              { moduleGuards =
                  [ EndpointGuard
                      ( \request -> do
                          let securitySinkPresent = case endpointSecurityEventSink request of
                                Nothing -> False
                                Just _ -> True
                          writeIORef
                            guardFacts
                            ( Just
                                ( Wai.requestMethod (endpointWaiRequest request),
                                  EndpointMetadata.endpointNameText (EndpointMetadata.endpointName (endpointMetadata request)),
                                  securitySinkPresent,
                                  endpointDispatchKind request
                                )
                            )
                          pure
                            ( HaltEndpoint
                                ( PageResponse
                                    Page
                                      { pageTitle = "Blocked",
                                        pageRoute = ChildItemRoute,
                                        pageContext = "tenant-42",
                                        pageBody = text "Blocked",
                                        pageBootstrapHooks = []
                                      }
                                )
                            )
                      )
                  ]
              }
      mountedModule <- requireMountedModule testModuleMount childModule
      let metadata = routeMetadata (moduleEndpoints mountedModule (CatalogRoute ChildItemRoute))
          endpointRequest routeValue =
            EndpointRequest
              { endpointWaiRequest = Wai.defaultRequest,
                endpointRouteRequest = RouteRequest routeValue 42,
                endpointMetadata = metadata,
                endpointSecurityEventSink = Nothing,
                endpointDispatchKind = EndpointMatched
              }
      case moduleGuards mountedModule of
        [mountedGuard] -> do
          runEndpointGuard mountedGuard (endpointRequest ParentOtherRoute)
            `shouldReturn` ContinueEndpoint 42
          haltResult <- runEndpointGuard mountedGuard (endpointRequest (CatalogRoute ChildItemRoute))
          case haltResult of
            HaltEndpoint (PageResponse page) -> do
              pageRoute page `shouldBe` CatalogRoute ChildItemRoute
              pageContext page `shouldBe` 42
            _ -> expectationFailure "expected the child guard page to be mapped to the parent algebra"
          readIORef guardFacts `shouldReturn` Just ("GET", "item", False, EndpointMatched)
        _ -> expectationFailure "expected exactly one mounted guard"

    it "appends child guards after every root security phase without giving the child a disable path" $ do
      guardOrder <- newIORef ([] :: [Text])
      actionContext <- newIORef Nothing
      childGuardContext <- newIORef Nothing
      let recordGuard :: Text -> EndpointGuard ChildRoute Text ChildAuthorization
          recordGuard label =
            EndpointGuard $ \request -> do
              modifyIORef' guardOrder (<> [label])
              pure (ContinueEndpoint (requestContext (endpointRouteRequest request)))
          childModule =
            (buildChildModule actionContext childGuardContext)
              { moduleGuards = [recordGuard "child"]
              }
          endpointRequest =
            EndpointRequest
              { endpointWaiRequest = Wai.defaultRequest,
                endpointRouteRequest = RouteRequest ChildItemRoute "tenant",
                endpointMetadata = childMetadata,
                endpointSecurityEventSink = Nothing,
                endpointDispatchKind = EndpointMatched
              }
          authentication =
            AuthenticationGuard $ \request -> do
              modifyIORef' guardOrder (<> ["authentication"])
              pure (ContinueEndpoint (requestContext (endpointRouteRequest request)))
      case inheritApplicationModuleGuards (AuthenticationDisabled [recordGuard "public-root"]) childModule of
        AuthenticationDisabled guards -> do
          runEndpointGuardPipeline guards endpointRequest `shouldReturn` ContinueEndpoint "tenant"
          readIORef guardOrder `shouldReturn` ["public-root", "child"]
        AuthenticationEnabled {} -> expectationFailure "expected public root security"
      writeIORef guardOrder []
      case inheritApplicationModuleGuards (AuthenticationEnabled [recordGuard "before"] authentication [recordGuard "after"]) childModule of
        AuthenticationEnabled beforeGuards configuredAuthentication afterGuards -> do
          let enabledGuards = beforeGuards <> [EndpointGuard (runAuthenticationGuard configuredAuthentication)] <> afterGuards
          runEndpointGuardPipeline enabledGuards endpointRequest `shouldReturn` ContinueEndpoint "tenant"
          readIORef guardOrder `shouldReturn` ["before", "authentication", "after", "child"]
        AuthenticationDisabled {} -> expectationFailure "expected enabled root security"

    it "keeps one module executable through root composition and rejects duplicate module identities" $ do
      actionContext <- newIORef Nothing
      guardContext <- newIORef Nothing
      mountedModule <-
        case mountApplicationModule testModuleMount (buildChildModule actionContext guardContext) of
          Left mountError -> expectationFailure (show mountError) >> fail "could not mount test module"
          Right mounted -> pure mounted
      case combineApplicationModules (mountedModule :| []) of
        Left compositionError -> expectationFailure (show compositionError)
        Right rootModule -> do
          parseRoute (moduleRouteCodec rootModule) 42 (testRouteLocation "/catalog/item")
            `shouldBe` RouteParsed (RouteRequest (CatalogRoute ChildItemRoute) 42)
          routeLocationText (renderRoute (moduleRouteCodec rootModule) (RouteRequest (CatalogRoute ChildItemRoute) 42))
            `shouldBe` "/catalog/item"
      case combineApplicationModules (mountedModule :| [mountedModule]) of
        Left compositionError -> compositionError `shouldBe` DuplicateModuleName (requiredModuleName "root.catalog")
        Right _ -> expectationFailure "expected duplicate module identity to be rejected"

    it "combines sibling ownership, parsing, methods, actions, and declared identity checks" $ do
      actionContext <- newIORef Nothing
      guardContext <- newIORef Nothing
      catalogModule <- requireMountedModule testModuleMount (buildChildModule actionContext guardContext)
      rootModule <-
        case combineApplicationModules (catalogModule :| [otherModule]) of
          Left compositionError -> expectationFailure (show compositionError) >> fail "could not combine test modules"
          Right combinedModule -> pure combinedModule
      let rootCodec = moduleRouteCodec rootModule
      moduleName rootModule `shouldBe` requiredModuleName "root.catalog"
      moduleOwnsRoute rootModule (CatalogRoute ChildItemRoute) `shouldBe` True
      moduleOwnsRoute rootModule ParentOtherRoute `shouldBe` True
      moduleOwnsRoute rootModule ParentUnownedRoute `shouldBe` False
      moduleDeclaredRoutes rootModule `shouldBe` [CatalogRoute ChildItemRoute, ParentOtherRoute]
      length (moduleGuards rootModule) `shouldBe` 1
      parseRoute rootCodec 42 (testRouteLocation "/catalog/missing") `shouldBe` RouteNotMatched
      parseRoute rootCodec 42 (testRouteLocation "/other") `shouldBe` RouteParsed (RouteRequest ParentOtherRoute 42)
      renderRoute rootCodec (RouteRequest ParentOtherRoute 42) `shouldBe` testRouteLocation "/other"
      notFoundRequest rootCodec 42 `shouldBe` RouteRequest (CatalogRoute ChildItemRoute) 42
      Routing.routeMethods rootCodec (CatalogRoute ChildItemRoute) `shouldBe` routeMethodPolicy [RouteGet]
      Routing.routeMethods rootCodec ParentOtherRoute `shouldBe` routeMethodPolicy [RouteGet]
      EndpointMetadata.endpointNameText (EndpointMetadata.endpointName (routeMetadata (moduleEndpoints rootModule ParentOtherRoute)))
        `shouldBe` "root.other"
      let installedSite =
            installApplicationModule
              rootModule
              (Site.apiOnlySite "composed" 42 rootCodec (AuthenticationDisabled []) (moduleEndpoints rootModule) :: Site.Site ParentRoute ParentAction Int ParentAuthorization)
          directModuleSite =
            applicationModuleSite
              "composed"
              42
              (AuthenticationDisabled [])
              rootModule
      parseRoute (Site.siteRouteCodec installedSite) 42 (testRouteLocation "/other")
        `shouldBe` RouteParsed (RouteRequest ParentOtherRoute 42)
      EndpointMetadata.endpointNameText (EndpointMetadata.endpointName (routeMetadata (Site.siteRouteDefinition installedSite ParentOtherRoute)))
        `shouldBe` "root.other"
      parseRoute (Site.siteRouteCodec directModuleSite) 42 (testRouteLocation "/other")
        `shouldBe` RouteParsed (RouteRequest ParentOtherRoute 42)
      EndpointMetadata.endpointNameText (EndpointMetadata.endpointName (routeMetadata (Site.siteRouteDefinition directModuleSite ParentOtherRoute)))
        `shouldBe` "root.other"
      Site.siteName directModuleSite `shouldBe` "composed"
      Site.siteDefaultRequestContext directModuleSite `shouldBe` 42
      case Site.siteRouteModuleChain directModuleSite of
        Nothing -> expectationFailure "expected direct module site to retain a mount chain"
        Just routeModuleChain -> routeModuleChain (CatalogRoute ChildItemRoute) `shouldBe` requiredModuleName "root.catalog" :| [requiredModuleName "catalog"]
      case Site.siteSecurity directModuleSite of
        AuthenticationDisabled guards -> length guards `shouldBe` 1
        AuthenticationEnabled {} -> expectationFailure "expected the direct public root security to stay public"
      case Site.siteSecurity installedSite of
        AuthenticationDisabled guards -> length guards `shouldBe` 1
        AuthenticationEnabled {} -> expectationFailure "expected the public root security to stay public"
      actionResult <-
        moduleHandleAction
          rootModule
          ClientActionRequest
            { clientAction = CatalogAction SaveChildItem,
              clientActionRequestIdempotencyKey = Nothing,
              clientActionContext = 42
            }
      actionResult `shouldBe` Just testClientActionResponse
      readIORef actionContext `shouldReturn` Just "tenant-42"
      Site.siteHandleClientAction
        installedSite
        ClientActionRequest
          { clientAction = CatalogAction SaveChildItem,
            clientActionRequestIdempotencyKey = Nothing,
            clientActionContext = 42
          }
        `shouldReturn` Just testClientActionResponse
      Site.siteHandleClientAction
        directModuleSite
        ClientActionRequest
          { clientAction = CatalogAction SaveChildItem,
            clientActionRequestIdempotencyKey = Nothing,
            clientActionContext = 42
          }
        `shouldReturn` Just testClientActionResponse
      Action.decodeAction
        (moduleActionCodec rootModule)
        Action.ClientActionPayload
          { Action.clientActionMethod = "POST",
            Action.clientActionPath = "/catalog/tenant-42/save",
            Action.clientActionFields = [],
            Action.clientActionCsrfToken = Nothing,
            Action.clientActionIdempotencyKey = Nothing,
            Action.clientActionPayloadContext = 42
          }
        `shouldBe` Action.DecodedClientAction (CatalogAction SaveChildItem)
      Site.siteDecodeClientAction
        installedSite
        Action.ClientActionPayload
          { Action.clientActionMethod = "POST",
            Action.clientActionPath = "/catalog/tenant-42/save",
            Action.clientActionFields = [],
            Action.clientActionCsrfToken = Nothing,
            Action.clientActionIdempotencyKey = Nothing,
            Action.clientActionPayloadContext = 42
          }
        `shouldBe` Action.DecodedClientAction (CatalogAction SaveChildItem)
      Site.siteDecodeClientAction
        directModuleSite
        Action.ClientActionPayload
          { Action.clientActionMethod = "POST",
            Action.clientActionPath = "/catalog/tenant-42/save",
            Action.clientActionFields = [],
            Action.clientActionCsrfToken = Nothing,
            Action.clientActionIdempotencyKey = Nothing,
            Action.clientActionPayloadContext = 42
          }
        `shouldBe` Action.DecodedClientAction (CatalogAction SaveChildItem)
      Site.siteClientActionEndpointMetadata installedSite "POST" "/catalog/tenant-42/save" 42
        `shouldSatisfy` (/= Nothing)

      mountedOtherAction <-
        moduleHandleAction
          catalogModule
          ClientActionRequest
            { clientAction = ParentOtherAction,
              clientActionRequestIdempotencyKey = Nothing,
              clientActionContext = 42
            }
      mountedOtherAction `shouldBe` Nothing

      moduleHandleAction
        rootModule
        ClientActionRequest
          { clientAction = ParentOtherAction,
            clientActionRequestIdempotencyKey = Nothing,
            clientActionContext = 42
          }
        `shouldReturn` Just testClientActionResponse

      let noHandlerCatalogModule = catalogModule {moduleHandleAction = const (pure Nothing)}
          noHandlerOtherModule = otherModule {moduleHandleAction = const (pure Nothing)}
      noHandlerRoot <-
        case combineApplicationModules (noHandlerCatalogModule :| [noHandlerOtherModule]) of
          Left compositionError -> expectationFailure (show compositionError) >> fail "could not compose terminal handler-miss test"
          Right combinedModule -> pure combinedModule
      moduleHandleAction
        noHandlerRoot
        ClientActionRequest
          { clientAction = ParentOtherAction,
            clientActionRequestIdempotencyKey = Nothing,
            clientActionContext = 42
          }
        `shouldReturn` Nothing

      let duplicateEndpointModule =
            otherModule
              { moduleName = requiredModuleName "root.catalog-copy",
                moduleEndpoints = const duplicateEndpointDefinition
              }
          unownedDeclaration = catalogModule {moduleName = requiredModuleName "root.unowned", moduleOwnsRoute = const False}
          overlappingOwner = otherModule {moduleName = requiredModuleName "root.overlap", moduleOwnsRoute = const True}
          duplicateActionModule = otherModule {moduleActionCodec = duplicateActionCodec}
      assertCompositionFailure
        (combineApplicationModules (catalogModule :| [duplicateEndpointModule]))
        (DuplicateModuleEndpointName (EndpointMetadata.requiredEndpointNameOrDie "root.catalog.item"))
      assertCompositionFailure
        (combineApplicationModules (unownedDeclaration :| []))
        (ModuleDoesNotOwnDeclaredRoute (requiredModuleName "root.unowned"))
      assertCompositionFailure
        (combineApplicationModules (catalogModule :| [overlappingOwner]))
        (OverlappingModuleRoute (requiredModuleName "root.catalog") (requiredModuleName "root.overlap"))
      assertCompositionFailure
        (combineApplicationModules (catalogModule :| [duplicateActionModule]))
        (InvalidComposedActionCodec (Action.DuplicateActionEndpoint Action.ActionPost "/catalog/save"))
      map
        (length . show)
        [ DuplicateModuleName (requiredModuleName "root.catalog"),
          DuplicateModuleEndpointName (EndpointMetadata.requiredEndpointNameOrDie "root.catalog.item"),
          ModuleDoesNotOwnDeclaredRoute (requiredModuleName "root.unowned"),
          OverlappingModuleRoute (requiredModuleName "root.catalog") (requiredModuleName "root.overlap"),
          InvalidComposedActionCodec (Action.DuplicateActionEndpoint Action.ActionPost "/catalog/save")
        ]
        `shouldSatisfy` all (> 0)

    it "diagnoses undeclared route selection rather than assigning it accidental module precedence" $ do
      actionContext <- newIORef Nothing
      guardContext <- newIORef Nothing
      catalogModule <- requireMountedModule testModuleMount (buildChildModule actionContext guardContext)
      rootModule <-
        case combineApplicationModules (catalogModule :| [otherModule]) of
          Left compositionError -> expectationFailure (show compositionError) >> fail "could not combine test modules"
          Right combinedModule -> pure combinedModule
      noOwnerFailure <- try (evaluate (moduleEndpoints rootModule ParentUnownedRoute)) :: IO (Either ErrorCall (RouteDefinition ParentRoute Int ParentAuthorization))
      assertErrorCall noOwnerFailure "no application module owns the selected route"

      let catalogClaimsUnowned = catalogModule {moduleOwnsRoute = \routeValue -> routeValue == CatalogRoute ChildItemRoute || routeValue == ParentUnownedRoute}
          otherClaimsUnowned = otherModule {moduleOwnsRoute = \routeValue -> routeValue == ParentOtherRoute || routeValue == ParentUnownedRoute}
      ambiguousRoot <-
        case combineApplicationModules (catalogClaimsUnowned :| [otherClaimsUnowned]) of
          Left compositionError -> expectationFailure (show compositionError) >> fail "could not compose non-declared ownership test"
          Right combinedModule -> pure combinedModule
      multipleOwnerFailure <- try (evaluate (moduleEndpoints ambiguousRoot ParentUnownedRoute)) :: IO (Either ErrorCall (RouteDefinition ParentRoute Int ParentAuthorization))
      assertErrorCall multipleOwnerFailure "multiple application modules own the selected route"

otherModule :: ApplicationModule ParentRoute ParentActionTarget ParentAction Int ParentAuthorization
otherModule =
  ApplicationModule
    { moduleName = requiredModuleName "root.other",
      moduleOwnsRoute = (== ParentOtherRoute),
      moduleRouteMountChain = const (requiredModuleName "root.other" :| []),
      moduleRouteCodec = otherCodec,
      moduleDeclaredRoutes = [ParentOtherRoute],
      moduleEndpoints = \case
        ParentOtherRoute -> otherDefinition
        routeValue -> error ("root.other does not own " <> show routeValue),
      moduleActionCodec = Action.emptyActionCodec,
      moduleHandleAction = const (pure (Just testClientActionResponse)),
      moduleGuards = []
    }

otherCodec :: RouteCodec ParentRoute Int
otherCodec =
  RouteCodec
    { parseRoute = \requestContext location ->
        if routeLocationText location == "/other"
          then RouteParsed (RouteRequest ParentOtherRoute requestContext)
          else RouteNotMatched,
      renderRoute = \_ -> testRouteLocation "/other",
      notFoundRequest = RouteRequest ParentOtherRoute,
      routeMethods = const (routeMethodPolicy [RouteGet])
    }

otherDefinition :: RouteDefinition ParentRoute Int ParentAuthorization
otherDefinition = otherDefinitionWithMetadata otherEndpointMetadata

duplicateEndpointDefinition :: RouteDefinition ParentRoute Int ParentAuthorization
duplicateEndpointDefinition =
  otherDefinitionWithMetadata
    ( EndpointMetadata.mkEndpointMetadata
        (EndpointMetadata.requiredEndpointNameOrDie "root.catalog.item")
        (EndpointMetadata.requiredRouteTemplateOrDie "/other")
        EndpointMetadata.HtmlEndpoint
        EndpointMetadata.AllowUnauthenticated
    )

otherDefinitionWithMetadata :: EndpointMetadata.EndpointMetadata ParentAuthorization -> RouteDefinition ParentRoute Int ParentAuthorization
otherDefinitionWithMetadata metadata =
  Site.pageRoute
    metadata
    Nothing
    (\request -> pure Page {pageTitle = "Other", pageRoute = requestRoute request, pageContext = requestContext request, pageBody = text "Other", pageBootstrapHooks = []})

otherEndpointMetadata :: EndpointMetadata.EndpointMetadata ParentAuthorization
otherEndpointMetadata =
  EndpointMetadata.mkEndpointMetadata
    (EndpointMetadata.requiredEndpointNameOrDie "root.other")
    (EndpointMetadata.requiredRouteTemplateOrDie "/other")
    EndpointMetadata.HtmlEndpoint
    EndpointMetadata.AllowUnauthenticated

duplicateActionCodec :: Action.ActionCodec ParentActionTarget Int ParentAuthorization ParentAction
duplicateActionCodec =
  Action.singleActionCodecWithMetadata
    ParentSaveTarget
    (Action.postAt "/catalog/save" (const "/catalog/save"))
    ( EndpointMetadata.mkEndpointMetadata
        (EndpointMetadata.requiredEndpointNameOrDie "root.other-save")
        (EndpointMetadata.requiredRouteTemplateOrDie "/other/save")
        EndpointMetadata.ActionEndpoint
        EndpointMetadata.AllowUnauthenticated
    )
    (pure (CatalogAction SaveChildItem))

assertCompositionFailure :: Either ModuleCompositionError (ApplicationModule ParentRoute ParentActionTarget ParentAction Int ParentAuthorization) -> ModuleCompositionError -> Expectation
assertCompositionFailure result expectedError =
  case result of
    Left actualError -> actualError `shouldBe` expectedError
    Right _ -> expectationFailure "expected module composition to fail"

assertErrorCall :: Either ErrorCall value -> String -> Expectation
assertErrorCall result expectedMessage =
  case result of
    Left failure -> show failure `shouldBe` expectedMessage
    Right _ -> expectationFailure "expected an authored composition misuse to fail"

testModuleMount :: ModuleMount ParentRoute ParentActionTarget ParentAction Int ParentAuthorization ChildRoute ChildActionTarget ChildAction Text ChildAuthorization
testModuleMount =
  ModuleMount
    { mountedRoutes =
        RouteMount
          { routeMountName = requiredModuleName "root.catalog",
            routeMountPrefix = requiredPathSegment "catalog" :| [],
            embedChildRoute = CatalogRoute,
            projectChildRoute = \case
              CatalogRoute childRoute -> Just childRoute
              ParentOtherRoute -> Nothing
              ParentUnownedRoute -> Nothing
          },
      mountedActions =
        ActionMount
          { embedChildActionTarget = const ParentSaveTarget,
            embedChildAction = CatalogAction,
            projectChildAction = \case
              CatalogAction childAction -> Just childAction
              ParentOtherAction -> Nothing
          },
      mountedContext = ContextProjection (\parentContext -> "tenant-" <> showText parentContext),
      mountedAuthorization = AuthorizationProjection (\ChildCanSave -> ParentCanSave)
    }

buildChildModule :: IORef (Maybe Text) -> IORef (Maybe Text) -> ApplicationModule ChildRoute ChildActionTarget ChildAction Text ChildAuthorization
buildChildModule actionContext guardContext =
  ApplicationModule
    { moduleName = requiredModuleName "catalog",
      moduleOwnsRoute = const True,
      moduleRouteMountChain = \case
        ChildItemRoute -> requiredModuleName "catalog" :| [],
      moduleRouteCodec = childCodec,
      moduleDeclaredRoutes = [ChildItemRoute],
      moduleEndpoints = childDefinition,
      moduleActionCodec = childActionCodec,
      moduleHandleAction = \request -> do
        let actionSuffix = case clientAction request of
              SaveChildItem -> Text.empty
            idempotencySuffix = case clientActionRequestIdempotencyKey request of
              Nothing -> Text.empty
              Just key -> "-" <> key
        writeIORef actionContext (Just (clientActionContext request <> actionSuffix <> idempotencySuffix))
        pure Nothing,
      moduleGuards =
        [ EndpointGuard $ \request -> do
            case requestRoute (endpointRouteRequest request) of
              ChildItemRoute -> pure ()
            writeIORef guardContext (Just (requestContext (endpointRouteRequest request)))
            pure (ContinueEndpoint (requestContext (endpointRouteRequest request)))
        ]
    }

requireMountedModule :: ModuleMount ParentRoute ParentActionTarget ParentAction Int ParentAuthorization ChildRoute ChildActionTarget ChildAction Text ChildAuthorization -> ApplicationModule ChildRoute ChildActionTarget ChildAction Text ChildAuthorization -> IO (ApplicationModule ParentRoute ParentActionTarget ParentAction Int ParentAuthorization)
requireMountedModule moduleMount childModule =
  case mountApplicationModule moduleMount childModule of
    Left mountError -> expectationFailure (show mountError) >> fail "could not mount test module"
    Right mountedModule -> pure mountedModule

childCodec :: RouteCodec ChildRoute Text
childCodec =
  RouteCodec
    { parseRoute = \requestContext location ->
        if routeLocationText location == "/item"
          then RouteParsed (RouteRequest ChildItemRoute requestContext)
          else RouteNotMatched,
      renderRoute = \case
        RouteRequest ChildItemRoute _ -> testRouteLocation "/item",
      notFoundRequest = RouteRequest ChildItemRoute,
      routeMethods = \case
        ChildItemRoute -> routeMethodPolicy [RouteGet]
    }

malformedChildCodec :: RouteCodec ChildRoute Text
malformedChildCodec =
  childCodec
    { parseRoute = \requestContext location ->
        case routeLocationText location of
          "/malformed" -> RouteMalformed InvalidRouteTargetEncoding
          "/item" -> RouteParsed (RouteRequest ChildItemRoute requestContext)
          _ -> RouteNotMatched
    }

childDefinition :: ChildRoute -> RouteDefinition ChildRoute Text ChildAuthorization
childDefinition ChildItemRoute =
  Site.pageRoute
    childMetadata
    (Just "Catalog")
    (\request -> pure Page {pageTitle = "Catalog", pageRoute = requestRoute request, pageContext = requestContext request, pageBody = text "Item", pageBootstrapHooks = []})

childDefinitionWithResponse :: ChildResponseKind -> RouteDefinition ChildRoute Text ChildAuthorization
childDefinitionWithResponse responseKind =
  (childDefinition ChildItemRoute)
    { routeResponse = \_ request -> pure (childResponseFor responseKind request)
    }

childDefinitionForAccess :: ChildRoute -> RouteDefinition ChildRoute Text ChildAuthorization
childDefinitionForAccess ChildItemRoute =
  childDefinitionWithAccess EndpointMetadata.AllowUnauthenticated "/"

childDefinitionWithAccess :: EndpointMetadata.AccessRequirement ChildAuthorization -> Text -> RouteDefinition ChildRoute Text ChildAuthorization
childDefinitionWithAccess accessRequirement template =
  Site.pageRoute
    ( EndpointMetadata.mkEndpointMetadata
        (EndpointMetadata.requiredEndpointNameOrDie "item")
        (EndpointMetadata.requiredRouteTemplateOrDie template)
        EndpointMetadata.HtmlEndpoint
        accessRequirement
    )
    (Just "Catalog")
    (\request -> pure Page {pageTitle = "Catalog", pageRoute = requestRoute request, pageContext = requestContext request, pageBody = text "Item", pageBootstrapHooks = []})

childResponseFor :: ChildResponseKind -> RouteRequest ChildRoute Text -> Response ChildRoute Text
childResponseFor responseKind request =
  case responseKind of
    ChildPageResponse -> PageResponse (childPage request)
    ChildPageResponseWithMetadata -> PageResponseWithMetadata testResponseBody (childPage request)
    ChildBodyResponse -> BodyResponse testResponseBody
    ChildRedirectResponse -> RedirectResponse testResponseBody "/next"
    ChildActionBodyResponse -> ClientActionBodyResponse testClientActionResponse
    ChildEventStreamResponse -> EventStreamResponse testResponseBody (ServerSentEventSource (pure Nothing))
    ChildProtocolResponse -> ProtocolResponseResult testProtocolResponse

childPage :: RouteRequest ChildRoute Text -> Page ChildRoute Text
childPage request =
  Page
    { pageTitle = "Catalog",
      pageRoute = requestRoute request,
      pageContext = requestContext request,
      pageBody = text "Item",
      pageBootstrapHooks = []
    }

assertMountedResponse :: ChildResponseKind -> Response ParentRoute Int -> Expectation
assertMountedResponse responseKind response =
  case (responseKind, response) of
    (ChildPageResponse, PageResponse page) -> assertMountedPage page
    (ChildPageResponseWithMetadata, PageResponseWithMetadata responseBodyValue page) -> do
      responseBodyValue `shouldBe` testResponseBody
      assertMountedPage page
    (ChildBodyResponse, BodyResponse responseBodyValue) -> responseBodyValue `shouldBe` testResponseBody
    (ChildRedirectResponse, RedirectResponse responseBodyValue location) -> do
      responseBodyValue `shouldBe` testResponseBody
      location `shouldBe` "/next"
    (ChildActionBodyResponse, ClientActionBodyResponse actionResponse) -> actionResponse `shouldBe` testClientActionResponse
    (ChildEventStreamResponse, EventStreamResponse responseBodyValue source) -> do
      responseBodyValue `shouldBe` testResponseBody
      nextServerSentEvent source `shouldReturn` Nothing
    (ChildProtocolResponse, ProtocolResponseResult protocolResponse) -> protocolResponse `shouldBe` testProtocolResponse
    _ -> expectationFailure ("response kind was not preserved: " <> show responseKind <> " rendered as " <> show response)

assertMountedPage :: Page ParentRoute Int -> Expectation
assertMountedPage page = do
  Document.pageRoute page `shouldBe` CatalogRoute ChildItemRoute
  pageContext page `shouldBe` 7

testResponseBody :: ResponseBody
testResponseBody =
  ResponseBody
    { responseStatus = Http.status200,
      responseContentType = "text/plain; charset=utf-8",
      responseBody = "ok",
      responseObservabilityAttributes = [],
      responseLogEntries = [],
      responseDatabaseOperations = []
    }

testClientActionResponse :: ClientActionResponse
testClientActionResponse =
  ClientActionResponse
    { clientActionStatus = Http.status200,
      clientActionPatches = [],
      clientActionFocusId = Nothing,
      clientActionHeaders = [],
      clientActionObservabilityAttributes = [],
      clientActionLogEntries = []
    }

testProtocolResponse :: ProtocolResponse
testProtocolResponse =
  ProtocolResponse
    { protocolResponseStatus = Http.status200,
      protocolResponseHeaders = [],
      protocolResponseBody = ProtocolResponseBytes (ByteString.pack "ok"),
      protocolResponseObservabilityAttributes = [],
      protocolResponseLogEntries = [],
      protocolResponseDatabaseOperations = []
    }

childMetadata :: EndpointMetadata.EndpointMetadata ChildAuthorization
childMetadata =
  EndpointMetadata.mkEndpointMetadata
    (EndpointMetadata.requiredEndpointNameOrDie "item")
    (EndpointMetadata.requiredRouteTemplateOrDie "/item")
    EndpointMetadata.HtmlEndpoint
    (EndpointMetadata.RequireAuthorized ChildCanSave)

childActionCodec :: Action.ActionCodec ChildActionTarget Text ChildAuthorization ChildAction
childActionCodec =
  Action.singleActionCodecWithMetadata
    ChildSaveTarget
    (Action.postAt "/save" (\childContext -> "/" <> childContext <> "/save"))
    childActionMetadata
    (pure SaveChildItem)

childActionMetadata :: EndpointMetadata.EndpointMetadata ChildAuthorization
childActionMetadata =
  EndpointMetadata.mkEndpointMetadata
    (EndpointMetadata.requiredEndpointNameOrDie "save")
    (EndpointMetadata.requiredRouteTemplateOrDie "/save")
    EndpointMetadata.ActionEndpoint
    (EndpointMetadata.RequireAuthorized ChildCanSave)

mountedChildActionMetadata :: EndpointMetadata.EndpointMetadata ParentAuthorization
mountedChildActionMetadata =
  EndpointMetadata.mkEndpointMetadata
    (EndpointMetadata.requiredEndpointNameOrDie "root.catalog.save")
    (EndpointMetadata.requiredRouteTemplateOrDie "/catalog/save")
    EndpointMetadata.ActionEndpoint
    (EndpointMetadata.RequireAuthorized ParentCanSave)

requiredModuleName :: Text -> ModuleName
requiredModuleName value =
  case mkModuleName value of
    Left moduleNameError -> error (show moduleNameError)
    Right moduleName -> moduleName

testRouteLocation :: Text -> RouteLocation
testRouteLocation target =
  case decodeRouteLocation (requestTarget (TextEncoding.encodeUtf8 path) (TextEncoding.encodeUtf8 query)) of
    Left routeError -> error (show routeError)
    Right location -> location
  where
    (path, query) = Text.breakOn "?" target

routeLocationText :: RouteLocation -> Text
routeLocationText = safeUrlText . encodeRouteLocation

showText :: (Show value) => value -> Text
showText = Text.pack . show
