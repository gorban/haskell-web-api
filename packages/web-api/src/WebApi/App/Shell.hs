{-# LANGUAGE OverloadedStrings #-}

module WebApi.App.Shell
  ( buildAppPageShell,
    buildAppPageShellConfig,
  )
where

import HarchWeb qualified
import WebApi.Config (AppConfig (..))
import WebApi.Route
  ( AppRequestContext,
    AppRoute (..),
    routeCodec,
  )

buildAppPageShell :: AppConfig -> HarchWeb.Page AppRoute AppRequestContext -> HarchWeb.Document AppRoute
buildAppPageShell config page =
  HarchWeb.pageTitle page `seq`
    HarchWeb.buildPageShell routeCodec (addAppNavigationItems (buildAppPageShellConfig config page)) page

buildAppPageShellConfig :: AppConfig -> HarchWeb.Page AppRoute AppRequestContext -> HarchWeb.PageShell AppRoute AppRequestContext
buildAppPageShellConfig config page =
  HarchWeb.pageRoute page `seq`
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
        HarchWeb.shellNavigationItems = [],
        HarchWeb.shellMainId = "app-main",
        HarchWeb.shellMainAttributes =
          [ HarchWeb.HtmlAttribute
              { HarchWeb.attributeName = "data-navigation-content",
                HarchWeb.attributeValue = "true"
              }
          ],
        HarchWeb.shellStylesheets = [],
        HarchWeb.shellRuntimeDescriptors = []
      }

addAppNavigationItems :: HarchWeb.PageShell AppRoute AppRequestContext -> HarchWeb.PageShell AppRoute AppRequestContext
addAppNavigationItems shell =
  shell
    { HarchWeb.shellNavigationItems =
        [ HarchWeb.NavigationItem
            { HarchWeb.navigationLabel = "Home",
              HarchWeb.navigationRoute = HomeRoute
            },
          HarchWeb.NavigationItem
            { HarchWeb.navigationLabel = "Second",
              HarchWeb.navigationRoute = SecondRoute
            },
          HarchWeb.NavigationItem
            { HarchWeb.navigationLabel = "Spaces",
              HarchWeb.navigationRoute = SpacesRoute
            }
        ]
    }
