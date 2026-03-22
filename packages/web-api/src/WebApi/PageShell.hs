{-# LANGUAGE OverloadedStrings #-}

module WebApi.PageShell
  ( buildAppPageShell,
  )
where

import Data.Text (Text)
import HarchWeb qualified
import WebApi.Config (AppConfig (..))
import WebApi.Route
  ( AppRequestContext,
    AppRoute (..),
    routeCodec,
  )

buildAppPageShell :: AppConfig -> HarchWeb.Page AppRoute AppRequestContext -> Text
buildAppPageShell config =
  HarchWeb.buildPageShell routeCodec (appPageShellConfig config)

appPageShellConfig :: AppConfig -> HarchWeb.PageShell AppRoute AppRequestContext
appPageShellConfig config =
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
      HarchWeb.shellScriptSources = navigationScriptSources config
    }

navigationScriptSources :: AppConfig -> [Text]
navigationScriptSources config =
  case HarchWeb.staticAssetRoots (staticAssets config) of
    primaryRoot : _ -> [HarchWeb.staticAssetHref primaryRoot "navigation.js"]
    [] -> []
