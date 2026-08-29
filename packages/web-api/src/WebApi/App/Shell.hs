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
buildAppPageShell config =
  HarchWeb.buildPageShell routeCodec (standalonePageShell (buildAppPageShellConfig config))

-- | The compatibility renderer is a complete standalone document builder, so
-- it supplies the same declared navigation that 'HarchWeb.Site' supplies for
-- the normal application path.  'buildAppPageShellConfig' intentionally does
-- not include these items: adding them there would duplicate Site-owned
-- navigation in the running application.
standalonePageShell :: HarchWeb.PageShell AppRoute AppRequestContext -> HarchWeb.PageShell AppRoute AppRequestContext
standalonePageShell shell =
  shell {HarchWeb.shellNavigationItems = appNavigationItems}

appNavigationItems :: [HarchWeb.NavigationItem AppRoute]
appNavigationItems =
  [ HarchWeb.NavigationItem "Home" HomeRoute,
    HarchWeb.NavigationItem "Second" SecondRoute,
    HarchWeb.NavigationItem "Spaces" SpacesRoute,
    HarchWeb.NavigationItem "Create account" RegistrationRoute,
    HarchWeb.NavigationItem "Sign in" LoginRoute,
    HarchWeb.NavigationItem "Profile" ProfileRoute
  ]

-- | Application shell configuration is independent of the rendered page.
-- 'WebApi.App' adapts this value with 'const' for 'HarchWeb.Site.simpleSite'.
buildAppPageShellConfig :: AppConfig -> HarchWeb.PageShell AppRoute AppRequestContext
buildAppPageShellConfig config =
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
      HarchWeb.shellNavigationItems = noAppShellNavigationItems,
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

-- | Route navigation belongs to 'HarchWeb.Site' through the application's
-- declared navigation routes.  The app shell deliberately contributes no
-- duplicate static entries.
noAppShellNavigationItems :: [HarchWeb.NavigationItem AppRoute]
noAppShellNavigationItems = []
