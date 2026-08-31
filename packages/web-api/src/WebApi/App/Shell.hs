{-# LANGUAGE OverloadedStrings #-}

module WebApi.App.Shell
  ( buildAppPageShell,
    buildAppPageShellConfig,
  )
where

import HarchWeb qualified
import WebApi.Components.Shell (AppShellProps (..), appPageShell)
import WebApi.Config (AppConfig (..))
import WebApi.Route
  ( AppRequestContext (..),
    AppRoute (..),
    routeCodec,
  )

buildAppPageShell :: AppConfig -> HarchWeb.Page AppRoute AppRequestContext -> HarchWeb.Document AppRoute
buildAppPageShell config page =
  HarchWeb.buildPageShell
    routeCodec
    (standalonePageShell (buildAppPageShellConfig config (HarchWeb.pageContext page)))
    page

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

-- | AHI-1 keeps application styling and shell composition in app-owned typed
-- functions.  The shell consumes the context's already-validated path prefix
-- so the declared stylesheet follows the same mount point as routes and
-- runtime assets, without introducing another proxy-header parser.
buildAppPageShellConfig :: AppConfig -> AppRequestContext -> HarchWeb.PageShell AppRoute AppRequestContext
buildAppPageShellConfig config context =
  appPageShell
    AppShellProps
      { appShellTitlePrefix = appTitlePrefix config,
        appShellPathPrefix = requestPathPrefix context,
        appShellStylesheet = HarchWeb.stylesheet (HarchWeb.AssetPath "/assets/styles/app.css"),
        appShellNavigationItems = noAppShellNavigationItems
      }

-- | Route navigation belongs to 'HarchWeb.Site' through the application's
-- declared navigation routes.  The app shell deliberately contributes no
-- duplicate static entries.
noAppShellNavigationItems :: [HarchWeb.NavigationItem AppRoute]
noAppShellNavigationItems = []
