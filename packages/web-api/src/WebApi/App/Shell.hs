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

-- | Per @docs/design-guidance.md@'s never-mask-a-gate-finding rule: the @$!@
-- below on 'page'\'s first reference is a last resort, confirmed directly
-- rather than assumed. 'page' is already a single, correctly-factored
-- function argument, referenced once each by 'buildAppPageShellConfig' and
-- 'HarchWeb.buildPageShell' — the correct shape for this code, not
-- duplication to remove. GHC shares the two references to this one thunk,
-- and only the second (the trailing bare argument) earns its own HPC tick
-- when forced; the first does not.
{-# ANN buildAppPageShell ("HLint: ignore Redundant $!" :: String) #-}
buildAppPageShell :: AppConfig -> HarchWeb.Page AppRoute AppRequestContext -> HarchWeb.Document AppRoute
buildAppPageShell config page =
  HarchWeb.buildPageShell routeCodec (addAppNavigationItems (buildAppPageShellConfig config $! page)) page

-- | The page argument is part of the shell-config callback shape
-- 'HarchWeb.Site.simpleSite' expects, even though this application's shell
-- does not currently vary by page.
buildAppPageShellConfig :: AppConfig -> HarchWeb.Page AppRoute AppRequestContext -> HarchWeb.PageShell AppRoute AppRequestContext
buildAppPageShellConfig config _page =
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
