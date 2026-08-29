{-# LANGUAGE OverloadedStrings #-}

module WebApi.Page
  ( AppPageModel (..),
    AuthenticatedProfilePageDetails (..),
    CallToAction (..),
    NotFoundPageModel (..),
    PendingProfilePageDetails (..),
    ProfilePageModel (..),
    SecondPageModel (..),
    SignedOutProfilePageDetails (..),
    SpacesPageModel (..),
    UnavailableProfilePageDetails (..),
    buildPageModelFromRouteData,
    buildPageModelWithDatabase,
    buildPageModel,
    buildCallToActionHref,
    renderPageFromRouteData,
    renderUnavailableProfilePage,
    renderProfilePageWithState,
    renderPageWithDatabase,
    renderPage,
    renderPageBody,
  )
where

import Data.Text qualified as Text
import HarchWeb qualified
import WebApi.App.Enhancements (pageEnhancementHooks)
import WebApi.Config (AppConfig (..))
import WebApi.Database (PageRepository, defaultPageRepository)
import WebApi.Page.Building
  ( buildCallToActionHref,
    buildPageModel,
    buildPageModelFromRouteData,
    buildPageModelWithDatabase,
    buildProfilePageModel,
    buildUnavailableProfilePageModel,
  )
import WebApi.Page.Model
import WebApi.Page.Rendering (renderPageBody, renderPageBodyForLocale)
import WebApi.Profile (ProfileState)
import WebApi.Route
  ( AppRequestContext (..),
    AppRoute,
    RouteMetadata (routePageTitle),
    routeMetadata,
  )
import WebApi.RouteData (RouteDataResult, selectRouteDataWithDatabase)

renderPage :: AppConfig -> HarchWeb.RouteRequest AppRoute AppRequestContext -> IO (HarchWeb.Page AppRoute AppRequestContext)
renderPage config =
  renderPageWithDatabase config defaultPageRepository

renderPageWithDatabase :: AppConfig -> PageRepository -> HarchWeb.RouteRequest AppRoute AppRequestContext -> IO (HarchWeb.Page AppRoute AppRequestContext)
renderPageWithDatabase config pageRepository routeRequest =
  fmap
    (renderPageFromRouteData config routeRequest)
    (selectRouteDataWithDatabase pageRepository routeRequest)

renderPageFromRouteData :: AppConfig -> HarchWeb.RouteRequest AppRoute AppRequestContext -> RouteDataResult -> HarchWeb.Page AppRoute AppRequestContext
renderPageFromRouteData config routeRequest routeData =
  renderPageModel config routeRequest (buildPageModelFromRouteData routeRequest routeData)

renderProfilePageWithState :: AppConfig -> HarchWeb.RouteRequest AppRoute AppRequestContext -> ProfileState -> HarchWeb.Page AppRoute AppRequestContext
renderProfilePageWithState config routeRequest profileState =
  renderPageModel config routeRequest (ProfilePage (buildProfilePageModel routeRequest profileState))

renderUnavailableProfilePage :: AppConfig -> HarchWeb.RouteRequest AppRoute AppRequestContext -> HarchWeb.Page AppRoute AppRequestContext
renderUnavailableProfilePage config routeRequest =
  renderPageModel config routeRequest (buildUnavailableProfilePageModel routeRequest)

renderPageModel :: AppConfig -> HarchWeb.RouteRequest AppRoute AppRequestContext -> AppPageModel -> HarchWeb.Page AppRoute AppRequestContext
renderPageModel config routeRequest pageModel =
  HarchWeb.Page
    { HarchWeb.pageTitle = Text.concat [appTitlePrefix config, ": ", routeTitle (HarchWeb.requestRoute routeRequest)],
      HarchWeb.pageRoute = HarchWeb.requestRoute routeRequest,
      HarchWeb.pageContext = HarchWeb.requestContext routeRequest,
      HarchWeb.pageBody = renderPageBodyForLocale (HarchWeb.requestContext routeRequest) (requestLocale (HarchWeb.requestContext routeRequest)) pageModel,
      HarchWeb.pageBootstrapHooks = pageEnhancementHooks (HarchWeb.requestRoute routeRequest)
    }

routeTitle :: AppRoute -> Text.Text
routeTitle = routePageTitle . routeMetadata
