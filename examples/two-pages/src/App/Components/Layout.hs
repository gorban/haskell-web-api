{-# LANGUAGE OverloadedStrings #-}

module App.Components.Layout
  ( twoPageShell,
  )
where

import App.Pages.Route.Generated (PageRoute (LiveDataPage))
import App.Routes (TwoPageRoute)
import App.Routes qualified as Routes
import HarchWeb
  ( AssetPath (..),
    HtmlAttribute (..),
    Page (..),
    PageShell (..),
    RuntimeDescriptor (..),
    stylesheet,
  )

twoPageShell :: Page TwoPageRoute () -> PageShell TwoPageRoute ()
twoPageShell page =
  PageShell
    { shellBodyAttributes =
        [ HtmlAttribute
            { attributeName = "data-app",
              attributeValue = "two-pages-example"
            }
        ],
      shellNavigationAttributes =
        [ HtmlAttribute
            { attributeName = "data-navigation-region",
              attributeValue = "primary"
            }
        ],
      shellNavigationItems = [],
      shellMainId = "app-main",
      shellMainAttributes =
        [ HtmlAttribute
            { attributeName = "data-navigation-content",
              attributeValue = "true"
            }
        ],
      shellStylesheets = [stylesheet (AssetPath "/assets/two-pages.css")],
      shellRuntimeDescriptors =
        case pageRoute page of
          Routes.Page LiveDataPage ->
            [DeferredModule "two-pages-live-data" "/assets/live-data.js"]
          _ -> []
    }
