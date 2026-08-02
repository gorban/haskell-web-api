{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module App.Pages.Second (secondPage) where

import App.Routes (TwoPageRoute (..), routeHref)
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

secondPage :: RouteRequest TwoPageRoute () -> IO (Page TwoPageRoute ())
secondPage routeRequest =
  pure
    Page
      { pageTitle = "Second",
        pageRoute = SecondRoute,
        pageContext = requestContext routeRequest,
        pageBody =
          [harch|
            <section data-page="second">
              <h1>Second</h1>
              <p>This page also returns full HTML when loaded directly.</p>
              <p><a href={routeHref HomeRoute} data-page-link="true">Back home</a></p>
            </section>
          |],
        pageBootstrapHooks = ["second-page"]
      }
