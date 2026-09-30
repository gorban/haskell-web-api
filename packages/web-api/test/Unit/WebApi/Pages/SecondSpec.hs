{-# SPEC #-}

import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text qualified as Text
import HarchWeb qualified
import HarchWeb.Database qualified as Database
import HarchWeb.Site
  ( NavigationOrder (..),
    RouteDefinition (..),
    RouteHandler (..),
    RouteNavigation (..),
  )
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import TestCore.Wai (performWaiRequest, readResponseBody, waiRequest)
import Unit.WebApi.TestSupport
  ( navigationAppConfig,
    prefixedSpanishSecondRequest,
    testPageSecurity,
    testTrustedForwardedProxy,
  )
import WebApi.App (buildAppWithDatabase)
import WebApi.Config (AppConfig (..), RequestPolicyConfig (..), defaultAppConfig)
import WebApi.Database
  ( DatabaseError (..),
    DatabaseOperation (..),
    DatabaseResult (..),
    PageRepository (..),
    SecondPageData (..),
    defaultPageRepository,
  )
import WebApi.PageModule
  ( PageDefinitionContext (..),
    PageModule (..),
    PageRequest (..),
    pageModuleDefinition,
  )
import WebApi.Pages.Second qualified as Second
import WebApi.Route
  ( AppAuthorization,
    AppLocale (..),
    AppRequestContext,
    AppRoute (SecondRoute),
    appNavigationItems,
    appNavigationRoutes,
    defaultRequestContext,
    endpointMetadata,
    routeNavigationDeclaration,
  )

spec =
  describe "WebApi.Pages.Second" $ do
    it "loads once for the matched locale and keeps successful database timings" $ do
      loadedLocales <- newIORef []
      let pageRepository =
            PageRepository $ \locale -> do
              modifyIORef' loadedLocales (<> [locale])
              pure
                DatabaseResult
                  { databaseResultValue = Right (SecondPageData "Contenido" []),
                    databaseResultOperations = [secondPageOperation]
                  }
      pageResult <- runSecondPage pageRepository prefixedSpanishSecondRequest
      readIORef loadedLocales `shouldReturn` [Spanish]
      case pageResult of
        HarchWeb.RenderedPageWithMetadata responseMetadata page -> do
          let renderedBody = HarchWeb.renderHtml (HarchWeb.pageBody page)
              operations = HarchWeb.responseDatabaseOperations responseMetadata
          expectAll
            ( (HarchWeb.responseStatus responseMetadata `shouldBe` Http.status200)
                :| [ HarchWeb.pageTitle page `shouldBe` "web-api: Segunda",
                     HarchWeb.pageRoute page `shouldBe` SecondRoute,
                     Text.isInfixOf "Contenido" renderedBody `shouldBe` True,
                     Text.isInfixOf "Aún no hay destacados." renderedBody `shouldBe` True,
                     Text.isInfixOf "No highlights yet." renderedBody `shouldBe` False,
                     Text.isInfixOf "href=\"/app/es\" data-page-link=\"true\">Volver al inicio" renderedBody `shouldBe` True
                   ]
            )
          case operations of
            [operation] ->
              expectAll
                ( (Database.databaseOperationName operation `shouldBe` "load-second-page-summary")
                    :| [ Database.databaseOperationStartedAtNanoseconds operation `shouldBe` Just 10,
                         Database.databaseOperationEndedAtNanoseconds operation `shouldBe` Just 20
                       ]
                )
            _ -> expectationFailure "expected one recorded database operation"
        HarchWeb.RenderedPage _ -> expectationFailure "expected successful database metadata"
        HarchWeb.RenderedPageWithHeaders _ _ -> expectationFailure "expected a page response without custom headers"

    it "renders localized safe failure copy while retaining private diagnostics and operations" $ do
      let privateFailure = "private database detail"
          pageRepository =
            PageRepository $ \_ ->
              pure
                DatabaseResult
                  { databaseResultValue = Left (SecondPageDataError privateFailure),
                    databaseResultOperations = [secondPageOperation]
                  }
      pageResult <- runSecondPage pageRepository prefixedSpanishSecondRequest
      case pageResult of
        HarchWeb.RenderedPageWithMetadata responseMetadata page -> do
          let renderedBody = HarchWeb.renderHtml (HarchWeb.pageBody page)
              responseLog = Text.unlines (HarchWeb.responseLogEntries responseMetadata)
              operations = HarchWeb.responseDatabaseOperations responseMetadata
          expectAll
            ( (HarchWeb.responseStatus responseMetadata `shouldBe` Http.status500)
                :| [ Text.isInfixOf "El contenido de la segunda pagina no esta disponible temporalmente." renderedBody `shouldBe` True,
                     Text.isInfixOf "No se pudieron cargar los datos de la segunda pagina." renderedBody `shouldBe` True,
                     Text.isInfixOf privateFailure renderedBody `shouldBe` False,
                     Text.isInfixOf privateFailure responseLog `shouldBe` True,
                     map Database.databaseOperationName operations `shouldBe` ["load-second-page-summary"]
                   ]
            )
        HarchWeb.RenderedPage _ -> expectationFailure "expected failure response metadata"
        HarchWeb.RenderedPageWithHeaders _ _ -> expectationFailure "expected a page response without custom headers"

    it "sends a live /second load failure as a complete HTTP 500 page" $ do
      let privateFailure = "private live database detail"
          pageRepository =
            PageRepository $ \_ ->
              pure
                DatabaseResult
                  { databaseResultValue = Left (SecondPageDataError privateFailure),
                    databaseResultOperations = [secondPageOperation]
                  }
          application = buildAppWithDatabase defaultAppConfig pageRepository
      response <- performWaiRequest (HarchWeb.toWaiApplication application) (waiRequest ["es", "second"])
      renderedDocument <- readResponseBody response
      expectAll
        ( (Wai.responseStatus response `shouldBe` Http.status500)
            :| [ Text.isInfixOf "<!DOCTYPE html><html lang=\"es\">" renderedDocument `shouldBe` True,
                 Text.isInfixOf "El contenido de la segunda pagina no esta disponible temporalmente." renderedDocument `shouldBe` True,
                 Text.isInfixOf "No se pudieron cargar los datos de la segunda pagina." renderedDocument `shouldBe` True,
                 Text.isInfixOf "data-harch-dialog-control" renderedDocument `shouldBe` True,
                 Text.isInfixOf "data-help-fab" renderedDocument `shouldBe` True,
                 Text.isInfixOf privateFailure renderedDocument `shouldBe` False
               ]
        )

    it "serves the public generated route once with locale and typed links under a path prefix" $ do
      loadedLocales <- newIORef []
      let pageRepository =
            PageRepository $ \locale -> do
              modifyIORef' loadedLocales (<> [locale])
              pure
                DatabaseResult
                  { databaseResultValue = Right (SecondPageData "Contenido español" ["Destacado"]),
                    databaseResultOperations = []
                  }
          appConfig =
            navigationAppConfig
              { requestPolicy =
                  (requestPolicy navigationAppConfig)
                    { forwardedHeaderTrust = testTrustedForwardedProxy
                    }
              }
          application = buildAppWithDatabase appConfig pageRepository
          request =
            (waiRequest ["app", "es", "second"])
              { Wai.requestHeaders = [("X-Forwarded-Prefix", "/app")]
              }
      response <- performWaiRequest (HarchWeb.toWaiApplication application) request
      renderedDocument <- readResponseBody response
      readIORef loadedLocales `shouldReturn` [Spanish]
      expectAll
        ( (Wai.responseStatus response `shouldBe` Http.status200)
            :| [ Text.isInfixOf "<!DOCTYPE html><html lang=\"es\">" renderedDocument `shouldBe` True,
                 Text.isInfixOf "Contenido español" renderedDocument `shouldBe` True,
                 Text.isInfixOf "Destacado" renderedDocument `shouldBe` True,
                 Text.isInfixOf "data-harch-dialog-control" renderedDocument `shouldBe` True,
                 Text.isInfixOf "data-help-fab" renderedDocument `shouldBe` True,
                 Text.isInfixOf "href=\"/app/es/second\" data-page-link=\"true\" aria-current=\"page\">Segunda" renderedDocument `shouldBe` True,
                 Text.isInfixOf "href=\"/app/es\" data-page-link=\"true\">Volver al inicio" renderedDocument `shouldBe` True
               ]
        )

    it "declares localized navigation with its page and preserves the prepared request security" $ do
      let pageDefinition = Second.pageDefinition (PageDefinitionContext defaultAppConfig defaultPageRepository)
          englishNavigation = routeNavigation pageDefinition defaultRequestContext
          spanishNavigation = routeNavigation pageDefinition (HarchWeb.requestContext prefixedSpanishSecondRequest)
          spanishContext = HarchWeb.requestContext prefixedSpanishSecondRequest
          standaloneSpanishNavigation = routeNavigationDeclaration SecondRoute spanishContext
          standaloneSpanishItem =
            any
              (\(HarchWeb.NavigationItem label route) -> route == SecondRoute && label == "Segunda")
              (appNavigationItems spanishContext)
          request = prefixedSpanishSecondRequest
          securityCheckingModule =
            PageModule
              { pageModuleEndpointMetadata = endpointMetadata SecondRoute,
                pageModulePresentation = Second.pagePresentation,
                pageModuleMethods = HarchWeb.routeMethodPolicy [HarchWeb.RouteGet],
                pageModuleLoad = \() receivedRequest ->
                  pure
                    ( pageRequestRoute receivedRequest == request
                        && HarchWeb.samePageSecurity (pageRequestSecurity receivedRequest) testPageSecurity
                    ),
                pageModuleRespond = \receivedRequest preserved ->
                  HarchWeb.RenderedPage $
                    HarchWeb.Page
                      { HarchWeb.pageTitle = if preserved then "request preserved" else "request changed",
                        HarchWeb.pageRoute = HarchWeb.requestRoute (pageRequestRoute receivedRequest),
                        HarchWeb.pageContext = HarchWeb.requestContext (pageRequestRoute receivedRequest),
                        HarchWeb.pageBody = HarchWeb.text (if preserved then "security preserved" else "security changed"),
                        HarchWeb.pageBootstrapHooks = [],
                        HarchWeb.pageStylesheets = []
                      }
              }
      expectAll
        ( ((englishNavigation == Just (RouteNavigation (NavigationOrder 10) "Second")) `shouldBe` True)
            :| [ (spanishNavigation == Just (RouteNavigation (NavigationOrder 10) "Segunda")) `shouldBe` True,
                 (standaloneSpanishNavigation == Just (RouteNavigation (NavigationOrder 10) "Segunda")) `shouldBe` True,
                 shouldBe standaloneSpanishItem True,
                 shouldContain appNavigationRoutes [SecondRoute],
                 HarchWeb.endpointAccess (endpointMetadata SecondRoute) `shouldBe` HarchWeb.AllowUnauthenticated
               ]
        )
      result <- runDefinition (pageModuleDefinition securityCheckingModule ()) request
      case result of
        HarchWeb.RenderedPage page ->
          expectAll
            ( (HarchWeb.pageTitle page `shouldBe` "request preserved")
                :| [ HarchWeb.pageRoute page `shouldBe` SecondRoute,
                     HarchWeb.renderHtml (HarchWeb.pageBody page) `shouldBe` "security preserved"
                   ]
            )
        _ -> expectationFailure "expected the page module to return a rendered page"

runSecondPage :: PageRepository -> HarchWeb.RouteRequest AppRoute AppRequestContext -> IO (HarchWeb.PageResult AppRoute AppRequestContext)
runSecondPage pageRepository =
  runDefinition
    ( Second.pageDefinition
        PageDefinitionContext
          { pageDefinitionConfig = defaultAppConfig,
            pageDefinitionPageRepository = pageRepository
          }
    )

runDefinition :: RouteDefinition AppRoute AppRequestContext AppAuthorization -> HarchWeb.RouteRequest AppRoute AppRequestContext -> IO (HarchWeb.PageResult AppRoute AppRequestContext)
runDefinition definition request =
  case routeHandler definition of
    PageRouteHandler handlePage -> handlePage testPageSecurity request
    ProtocolRouteHandler _ -> fail "expected a page route handler"

secondPageOperation :: DatabaseOperation
secondPageOperation =
  DatabaseOperation
    { databaseOperationName = "load-second-page-summary",
      databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;",
      databaseOperationStartedAtNanoseconds = Just 10,
      databaseOperationEndedAtNanoseconds = Just 20
    }
