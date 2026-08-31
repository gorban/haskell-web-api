{-# LANGUAGE OverloadedStrings #-}

module WebApi.App.Shell
  ( buildAppPageShell,
    buildAppPageShellConfig,
    appRuntimeAssets,
  )
where

import HarchWeb qualified
import WebApi.Components.Shell (AppShellProps (..), appPageShell)
import WebApi.Config (AppConfig (..))
import WebApi.Localization (AppMessage (SkipToMainContent), localizedMessage)
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
        appShellNavigationItems = noAppShellNavigationItems,
        appShellNavigationLifecycle = Just (appNavigationLifecycle context),
        appShellRuntimeAssets = appRuntimeAssets
      }

appRuntimeAssets :: [HarchWeb.RuntimeAsset]
appRuntimeAssets = [HarchWeb.defaultDialogRuntime]

-- | The application localizes and styles the declarative lifecycle adapter;
-- Harch owns its stable main target, polite semantics, and runtime ordering.
appNavigationLifecycle :: AppRequestContext -> HarchWeb.NavigationLifecycle
appNavigationLifecycle context =
  let lifecycle = HarchWeb.mainNavigationLifecycle (localizedMessage (requestLocale context) SkipToMainContent)
   in lifecycle
        { HarchWeb.navigationSkipLink =
            addSkipLinkClass <$> HarchWeb.navigationSkipLink lifecycle,
          HarchWeb.navigationStatusClass = Just (HarchWeb.ScopedCssClass appShellScope "route-status")
        }

addSkipLinkClass :: HarchWeb.NavigationSkipLink -> HarchWeb.NavigationSkipLink
addSkipLinkClass skipLink =
  skipLink {HarchWeb.skipLinkClass = Just (HarchWeb.ScopedCssClass appShellScope "skip-link")}

appShellScope :: HarchWeb.CssScope
appShellScope = HarchWeb.cssScope "app-shell"

-- | Route navigation belongs to 'HarchWeb.Site' through the application's
-- declared navigation routes.  The app shell deliberately contributes no
-- duplicate static entries.
noAppShellNavigationItems :: [HarchWeb.NavigationItem AppRoute]
noAppShellNavigationItems = []
