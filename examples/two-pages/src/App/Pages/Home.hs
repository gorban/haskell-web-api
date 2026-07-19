{-# LANGUAGE OverloadedStrings #-}

module App.Pages.Home (homePage) where

import App.Routes (TwoPageRoute (..), routeHref)
import qualified Data.Text as Text
import HarchWeb
  ( CssClass (..),
    Page (..),
    RouteRequest (..),
    cssClassText,
    cssScope,
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
            [ "<section data-page=\"home\" class=\"",
              cssClassText (ScopedCssClass (cssScope "home") "root"),
              "\">",
              "<h1>Home</h1>",
              "<p>This page is fully server-rendered on direct load and reload.</p>",
              "<p><a href=\"",
              routeHref SecondRoute,
              "\" data-page-link=\"true\">Go to the second page</a></p>",
              "<form aria-label=\"Subscription\" data-harch-control data-harch-action=\"true\" action=\"/actions/subscribe\" method=\"post\">",
              "<label for=\"subscription-email\">Email address</label>",
              "<input id=\"subscription-email\" name=\"email\" type=\"email\" autocomplete=\"email\" required>",
              "<button name=\"intent\" value=\"subscribe\" type=\"submit\">Subscribe</button>",
              "</form>",
              "<p id=\"subscription-result\" data-harch-region=\"true\" role=\"status\"></p>",
              "</section>"
            ],
        pageBootstrapHooks = []
      }
