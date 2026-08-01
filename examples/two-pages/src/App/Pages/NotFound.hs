{-# LANGUAGE OverloadedStrings #-}

module App.Pages.NotFound (notFoundPage) where

import App.Routes (TwoPageRoute (..), routeHref)
import HarchWeb
  ( Page (..),
    RouteRequest (..),
    anchorTag,
    dataAttribute,
    element,
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
          element
            sectionTag
            [dataAttribute "page" "not-found"]
            [ element headingOneTag [] [text "Not Found"],
              element paragraphTag [] [text "The requested page could not be found."],
              element paragraphTag [] [element anchorTag [href (routeHref HomeRoute), dataAttribute "page-link" "true"] [text "Return home"]]
            ],
        pageBootstrapHooks = []
      }
