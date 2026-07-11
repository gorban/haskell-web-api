{-# LANGUAGE OverloadedStrings #-}

module App.Pages.Home (homePage) where

import App.Routes (TwoPageRoute (..), routeHref)
import qualified Data.Text as Text
import HarchWeb
  ( Page (..),
    RouteRequest (..),
  )

homePage :: RouteRequest TwoPageRoute () -> IO (Page TwoPageRoute ())
homePage routeRequest =
  pure
    Page
      { pageTitle = "Home",
        pageRoute = HomeRoute,
        pageContext = requestContext routeRequest,
        pageBody =
          Text.concat
            [ "<section data-page=\"home\">",
              "<h1>Home</h1>",
              "<p>This page is fully server-rendered on direct load and reload.</p>",
              "<p><a href=\"",
              routeHref SecondRoute,
              "\" data-page-link=\"true\">Go to the second page</a></p>",
              "</section>"
            ],
        pageBootstrapHooks = []
      }
