{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module App.Pages.LiveData (pageDefinition, liveDataPage) where

import App.Pages.Route.Generated (PageRoute (..))
import App.Routes (TwoPageRoute, twoPageEndpointMetadata)
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
  Site.pageRoute (twoPageEndpointMetadata HtmlEndpoint (Routes.Page LiveDataPage)) (Just "Live updates") liveDataPage

liveDataPage :: PageSecurity -> RouteRequest TwoPageRoute () -> IO (Page TwoPageRoute ())
liveDataPage _ routeRequest =
  pure
    Page
      { pageTitle = "Live updates",
        pageRoute = Routes.Page LiveDataPage,
        pageContext = requestContext routeRequest,
        pageBody =
          [harch|
            <section data-page="live-data" data-live-data-source="/live-data/events">
              <h1>Live updates</h1>
              <p>This complete status is rendered on the server before any live connection starts.</p>
              <p id="live-data-status" data-live-data-status role="status">Waiting for an update.</p>
            </section>
          |],
        pageBootstrapHooks = []
      }
