{-# LANGUAGE OverloadedStrings #-}

module App.Pages.LiveData (liveDataPage) where

import App.Routes (TwoPageRoute (..))
import Data.Text qualified as Text
import HarchWeb (Page (..), RouteRequest (..))

liveDataPage :: RouteRequest TwoPageRoute () -> IO (Page TwoPageRoute ())
liveDataPage routeRequest =
  pure
    Page
      { pageTitle = "Live updates",
        pageRoute = LiveDataRoute,
        pageContext = requestContext routeRequest,
        pageBody =
          Text.concat
            [ "<section data-page=\"live-data\" data-live-data-source=\"/live-data/events\">",
              "<h1>Live updates</h1>",
              "<p>This complete status is rendered on the server before any live connection starts.</p>",
              "<p id=\"live-data-status\" data-live-data-status role=\"status\">Waiting for an update.</p>",
              "</section>"
            ],
        pageBootstrapHooks = []
      }
