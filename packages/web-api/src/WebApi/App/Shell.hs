{-# LANGUAGE OverloadedStrings #-}

module WebApi.App.Shell
  ( buildAppPageShell,
  )
where

import Data.Text (Text)
import HarchWeb qualified
import WebApi.App.Assets (navigationScriptSources)
import WebApi.Config (AppConfig (..))
import WebApi.Route
  ( AppRequestContext,
    AppRoute (..),
    routeCodec,
  )

buildAppPageShell :: AppConfig -> HarchWeb.Page AppRoute AppRequestContext -> Text
buildAppPageShell config page =
  HarchWeb.buildPageShell routeCodec (appPageShellConfig config page) page

appPageShellConfig :: AppConfig -> HarchWeb.Page AppRoute AppRequestContext -> HarchWeb.PageShell AppRoute AppRequestContext
appPageShellConfig config page =
  HarchWeb.PageShell
    { HarchWeb.shellBodyAttributes =
        [ HarchWeb.HtmlAttribute
            { HarchWeb.attributeName = "data-app",
              HarchWeb.attributeValue = appTitlePrefix config
            }
        ],
      HarchWeb.shellNavigationAttributes =
        [ HarchWeb.HtmlAttribute
            { HarchWeb.attributeName = "data-navigation-region",
              HarchWeb.attributeValue = "primary"
            }
        ],
      HarchWeb.shellNavigationItems =
        [ HarchWeb.NavigationItem
            { HarchWeb.navigationLabel = "Home",
              HarchWeb.navigationRoute = HomeRoute
            },
          HarchWeb.NavigationItem
            { HarchWeb.navigationLabel = "Second",
              HarchWeb.navigationRoute = SecondRoute
            }
        ],
      HarchWeb.shellMainId = "app-main",
      HarchWeb.shellMainAttributes =
        [ HarchWeb.HtmlAttribute
            { HarchWeb.attributeName = "data-navigation-content",
              HarchWeb.attributeValue = "true"
            }
        ],
      HarchWeb.shellScriptSources = navigationScriptSources config (HarchWeb.pageContext page)
    }
