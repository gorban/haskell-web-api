{-# LANGUAGE OverloadedStrings #-}

module App.Components.Layout
  ( twoPageShell,
  )
where

import App.Routes (TwoPageRoute)
import HarchWeb
  ( HtmlAttribute (..),
    Page,
    PageShell (..),
  )

twoPageShell :: Page TwoPageRoute () -> PageShell TwoPageRoute ()
twoPageShell _ =
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
      shellRuntimeDescriptors = []
    }
