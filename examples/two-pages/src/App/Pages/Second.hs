{-# LANGUAGE OverloadedStrings #-}

module App.Pages.Second (secondPage) where

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

secondPage :: RouteRequest TwoPageRoute () -> IO (Page TwoPageRoute ())
secondPage routeRequest =
  pure
    Page
      { pageTitle = "Second",
        pageRoute = SecondRoute,
        pageContext = requestContext routeRequest,
        pageBody =
          element
            sectionTag
            [dataAttribute "page" "second"]
            [ element headingOneTag [] [text "Second"],
              element paragraphTag [] [text "This page also returns full HTML when loaded directly."],
              element paragraphTag [] [element anchorTag [href (routeHref HomeRoute), dataAttribute "page-link" "true"] [text "Back home"]]
            ],
        pageBootstrapHooks = ["second-page"]
      }
