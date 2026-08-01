{-# LANGUAGE OverloadedStrings #-}

module App.Pages.LiveData (liveDataPage) where

import App.Routes (TwoPageRoute (..))
import HarchWeb (Page (..), RouteRequest (..), dataAttribute, dataFlag, element, elementId, headingOneTag, mkElementId, paragraphTag, role, sectionTag, text)

liveDataPage :: RouteRequest TwoPageRoute () -> IO (Page TwoPageRoute ())
liveDataPage routeRequest =
  pure
    Page
      { pageTitle = "Live updates",
        pageRoute = LiveDataRoute,
        pageContext = requestContext routeRequest,
        pageBody =
          case mkElementId "live-data-status" of
            Nothing -> text ""
            Just statusId ->
              element
                sectionTag
                [dataAttribute "page" "live-data", dataAttribute "live-data-source" "/live-data/events"]
                [ element headingOneTag [] [text "Live updates"],
                  element paragraphTag [] [text "This complete status is rendered on the server before any live connection starts."],
                  element paragraphTag [elementId statusId, dataFlag "live-data-status", role "status"] [text "Waiting for an update."]
                ],
        pageBootstrapHooks = []
      }
