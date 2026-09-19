{-# LANGUAGE OverloadedStrings #-}

module App.Components.Layout
  ( twoPageShell,
  )
where

import App.Pages.Route.Generated (PageRoute (HomePage, LiveDataPage))
import App.Routes (TwoPageRoute)
import App.Routes qualified as Routes
import HarchWeb
  ( AssetPath (..),
    HtmlAttribute (..),
    Page (..),
    PageShell (..),
    RuntimeDescriptor (..),
    Stylesheet,
    literalElementId,
    locale,
    stylesheet,
  )

twoPageShell :: Page TwoPageRoute () -> PageShell TwoPageRoute ()
twoPageShell page =
  PageShell
    { shellDocumentLanguage = locale "en",
      shellBodyAttributes =
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
      shellMainId = literalElementId "app-main",
      shellMainAttributes =
        [ HtmlAttribute
            { attributeName = "data-navigation-content",
              attributeValue = "true"
            }
        ],
      shellNavigationLifecycle = Nothing,
      shellStylesheets = stylesheet (AssetPath "/assets/two-pages.css") : pageStylesheets (pageRoute page),
      shellRuntimeDescriptors = pageEnhancements (pageRoute page)
    }

pageStylesheets :: TwoPageRoute -> [Stylesheet]
pageStylesheets route =
  case route of
    Routes.Page HomePage -> [stylesheet (AssetPath "/assets/home-enhancement.css")]
    _ -> []

pageEnhancements :: TwoPageRoute -> [RuntimeDescriptor]
pageEnhancements route =
  case route of
    Routes.Page HomePage -> [PageEnhancementModule "two-pages-home" "/assets/home-enhancement.js"]
    Routes.Page LiveDataPage -> [PageEnhancementModule "two-pages-live-data" "/assets/live-data.js"]
    _ -> []
