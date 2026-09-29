-- | The database-backed @\/second@ page. Its loader retains the complete
-- 'DatabaseResult' so both successful query timing and private failure
-- diagnostics reach the existing response metadata boundary. Public failure
-- copy is localized from the matched request context. The P2 decision is to
-- keep loading local to this page and interpret its complete database outcome
-- once into 'HarchWeb.PageResult'; projecting only the value would discard
-- database timing and private diagnostics. Route metadata and the standalone
-- renderer's navigation mirror remain in 'WebApi.Route' until the P3
-- declarations-and-discovery cleanup. The older public
-- 'WebApi.Response.selectResponseWithDatabase' compatibility path still has
-- its central @RouteData@ projection for @/second@; retiring that and the
-- corresponding page-wide renderer is the named P5 cleanup.
module WebApi.Pages.Second (pageDefinition) where

import Data.Text (Text)
import HarchWeb
  ( Html,
    Page (..),
    RouteMethod (RouteGet),
    dataAttribute,
    element,
    fragment,
    href,
    listItemTag,
    listTag,
    paragraphTag,
    routeMethodPolicy,
    text,
  )
import HarchWeb qualified
import HarchWeb.Site (NavigationOrder (..), RouteDefinition, RouteNavigation (..))
import WebApi.Components.AppControls (appControls)
import WebApi.Components.PageFrame (PageFrameProps (..), PageKind (SecondPageFrame), pageFrame)
import WebApi.Config (AppConfig, appTitlePrefix)
import WebApi.Database (DatabaseResult (..), PageRepository, SecondPageData (..), loadSecondPage)
import WebApi.Localization (AppMessage (ReturnHome, Second, SecondPageLoadFailed, SecondPageNoHighlights, SecondPageUnavailable), localizedMessage)
import WebApi.PageModule
  ( PageDefinitionContext (..),
    PageModule (..),
    PageRequest (..),
    pageModuleDefinition,
  )
import WebApi.Response
  ( FailureSurface (PageFailureSurface),
    pageErrorResponseMetadata,
    pageFailureDiagnostics,
    pageSuccessResponseMetadata,
  )
import WebApi.Route
  ( AppAuthorization,
    AppLocale,
    AppRequestContext,
    AppRoute (HomeRoute, SecondRoute),
    endpointMetadata,
    renderRouteUrl,
    requestLocale,
  )

secondPageModule :: AppConfig -> PageModule PageRepository (DatabaseResult SecondPageData)
secondPageModule config =
  PageModule
    { pageModuleEndpointMetadata = endpointMetadata SecondRoute,
      pageModuleNavigation = secondPageNavigation,
      pageModuleMethods = routeMethodPolicy [RouteGet],
      pageModuleLoad = loadSecondPageForRequest,
      pageModuleRespond = respondToSecondPage config
    }

pageDefinition :: PageDefinitionContext -> RouteDefinition AppRoute AppRequestContext AppAuthorization
pageDefinition context =
  pageModuleDefinition
    (secondPageModule (pageDefinitionConfig context))
    (pageDefinitionPageRepository context)

loadSecondPageForRequest ::
  PageRepository ->
  PageRequest ->
  IO (DatabaseResult SecondPageData)
loadSecondPageForRequest pageRepository pageRequest =
  loadSecondPage
    pageRepository
    (requestLocale (HarchWeb.requestContext (pageRequestRoute pageRequest)))

secondPageNavigation :: AppRequestContext -> Maybe RouteNavigation
secondPageNavigation requestContext =
  Just
    ( RouteNavigation
        (NavigationOrder 10)
        (localizedMessage (requestLocale requestContext) Second)
    )

respondToSecondPage ::
  AppConfig ->
  PageRequest ->
  DatabaseResult SecondPageData ->
  HarchWeb.PageResult AppRoute AppRequestContext
respondToSecondPage config pageRequest databaseResult =
  case databaseResultValue databaseResult of
    Left databaseError ->
      HarchWeb.RenderedPageWithMetadata
        ( pageErrorResponseMetadata
            ( pageFailureDiagnostics
                PageFailureSurface
                "/second"
                "second-page"
                (databaseResultOperations databaseResult)
                databaseError
            )
        )
        (renderSecondPage config pageRequest databaseResult)
    Right _ ->
      let renderedPage = renderSecondPage config pageRequest databaseResult
       in case databaseResultOperations databaseResult of
            [] -> HarchWeb.RenderedPage renderedPage
            operations -> HarchWeb.RenderedPageWithMetadata (pageSuccessResponseMetadata operations) renderedPage

renderSecondPage :: AppConfig -> PageRequest -> DatabaseResult SecondPageData -> Page AppRoute AppRequestContext
renderSecondPage config pageRequest databaseResult =
  Page
    { pageTitle = appTitlePrefix config <> ": " <> heading,
      pageRoute = HarchWeb.requestRoute routeRequest,
      pageContext = requestContext,
      pageBody =
        fragment
          [ pageFrame
              PageFrameProps
                { pageFrameKind = SecondPageFrame,
                  pageFrameHeading = heading,
                  pageFrameSummary = Just summary,
                  pageFrameContent = content
                },
            appControls requestContext (HarchWeb.requestRoute routeRequest)
          ],
      pageBootstrapHooks = ["second-page"],
      pageStylesheets = []
    }
  where
    routeRequest = pageRequestRoute pageRequest
    requestContext = HarchWeb.requestContext routeRequest
    locale = requestLocale requestContext
    heading = localizedMessage locale Second
    (summary, content) =
      case databaseResultValue databaseResult of
        Right pageData ->
          ( secondPageDataSummary pageData,
            [ renderHighlights locale (secondPageDataHighlights pageData),
              renderReturnHome routeRequest
            ]
          )
        Left _ ->
          ( localizedMessage locale SecondPageUnavailable,
            [ renderError (localizedMessage locale SecondPageLoadFailed),
              renderReturnHome routeRequest
            ]
          )

renderError :: Text -> Html
renderError message =
  element paragraphTag [dataAttribute "error-state" "true"] [text message]

renderHighlights :: AppLocale -> [Text] -> Html
renderHighlights locale highlights =
  case highlights of
    [] -> element paragraphTag [dataAttribute "empty-state" "true"] [text (localizedMessage locale SecondPageNoHighlights)]
    _ -> element listTag [] (map renderHighlight highlights)

renderHighlight :: Text -> Html
renderHighlight highlight =
  element listItemTag [] [text highlight]

renderReturnHome :: HarchWeb.RouteRequest AppRoute AppRequestContext -> Html
renderReturnHome routeRequest =
  let label = localizedMessage (requestLocale (HarchWeb.requestContext routeRequest)) ReturnHome
      renderedUrl = renderRouteUrl (routeRequest {HarchWeb.requestRoute = HomeRoute})
   in element
        paragraphTag
        []
        [element HarchWeb.anchorTag [href renderedUrl, dataAttribute "page-link" "true"] [text label]]
