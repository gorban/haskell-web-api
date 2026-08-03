{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module App.Pages.NotFound (pageDefinition, notFoundPage) where

import App.Pages.Route.Generated (PageRoute (..))
import App.Routes (TwoPageRoute)
import App.Routes qualified as Routes
import HarchWeb
  ( Page (..),
    RouteRequest (..),
    anchorTag,
    dataAttribute,
    element,
    fragment,
    harch,
    headingOneTag,
    href,
    paragraphTag,
    sectionTag,
    text,
  )
import HarchWeb.Site (RouteDefinition)
import HarchWeb.Site qualified as Site

pageDefinition :: RouteDefinition TwoPageRoute ()
pageDefinition =
  Site.pageRoute Nothing notFoundPage

notFoundPage :: RouteRequest TwoPageRoute () -> IO (Page TwoPageRoute ())
notFoundPage routeRequest =
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
              <p><a href={Routes.routeHref (Routes.Page HomePage)} data-page-link="true">Return home</a></p>
            </section>
          |],
        pageBootstrapHooks = []
      }
