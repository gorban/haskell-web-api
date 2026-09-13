{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module App.Pages.NotFound (pageDefinition, notFoundPage) where

import App.Components.Controls (pageLink)
import App.Pages.Route.Generated (PageRoute (..))
import App.Routes (TwoPageNavigationTarget (NavigationPage), TwoPageRoute, twoPageEndpointMetadata)
import App.Routes qualified as Routes
import HarchWeb
  ( EndpointProtocol (HtmlEndpoint),
    Page (..),
    PageSecurity,
    RouteRequest (..),
    harch,
  )
import HarchWeb.Site (RouteDefinition)
import HarchWeb.Site qualified as Site

pageDefinition :: RouteDefinition TwoPageRoute () ()
pageDefinition =
  Site.pageRoute (twoPageEndpointMetadata HtmlEndpoint (Routes.Page PageNotFound)) Nothing notFoundPage

notFoundPage :: PageSecurity -> RouteRequest TwoPageRoute () -> IO (Page TwoPageRoute ())
notFoundPage _ routeRequest =
  pure
    Page
      { pageTitle = "Not Found",
        pageRoute = Routes.Page PageNotFound,
        pageContext = requestContext routeRequest,
        pageBody =
          [harch|
            <section data-page="not-found">
              <h1>Not Found</h1>
              <p>The requested page could not be found.</p>
              <p><PageLink to={NavigationPage HomePage}>Return home</PageLink></p>
            </section>
          |],
        pageBootstrapHooks = []
      }
