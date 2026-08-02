{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module App.Pages.NotFound (notFoundPage) where

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

notFoundPage :: RouteRequest TwoPageRoute () -> IO (Page TwoPageRoute ())
notFoundPage routeRequest =
  pure
    Page
      { pageTitle = "Not Found",
        pageRoute = NotFoundRoute,
        pageContext = requestContext routeRequest,
        pageBody =
          [harch|
            <section data-page="not-found">
              <h1>Not Found</h1>
              <p>The requested page could not be found.</p>
              <p><a href={routeHref HomeRoute} data-page-link="true">Return home</a></p>
            </section>
          |],
        pageBootstrapHooks = []
      }
