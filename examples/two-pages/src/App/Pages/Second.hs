{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module App.Pages.Second (pageDefinition, secondPage) where

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
  Site.pageRoute (Just "Second") secondPage

secondPage :: RouteRequest TwoPageRoute () -> IO (Page TwoPageRoute ())
secondPage routeRequest =
  pure
    Page
      { pageTitle = "Second",
        pageRoute = Routes.Page SecondPage,
        pageContext = requestContext routeRequest,
        pageBody =
          [harch|
            <section data-page="second">
              <h1>Second</h1>
              <p>This page also returns full HTML when loaded directly.</p>
              <p><a href={Routes.routeHref (Routes.Page HomePage)} data-page-link="true">Back home</a></p>
            </section>
          |],
        pageBootstrapHooks = ["second-page"]
      }
