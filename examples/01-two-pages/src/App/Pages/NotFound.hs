{-# LANGUAGE OverloadedStrings #-}

module App.Pages.NotFound (notFoundPage) where

import App.Routes (TwoPageRoute (..), routeHref)
import qualified Data.Text as Text
import HarchWeb
  ( Page (..),
    RouteRequest (..),
  )

notFoundPage :: RouteRequest TwoPageRoute () -> IO (Page TwoPageRoute ())
notFoundPage routeRequest =
  pure
    Page
      { pageTitle = "Not Found",
        pageRoute = NotFoundRoute,
        pageContext = requestContext routeRequest,
        pageBody =
          Text.concat
            [ "<section data-page=\"not-found\">",
              "<h1>Not Found</h1>",
              "<p>The requested page could not be found.</p>",
              "<p><a href=\"",
              routeHref HomeRoute,
              "\" data-page-link=\"true\">Return home</a></p>",
              "</section>"
            ],
        pageBootstrapHooks = []
      }
