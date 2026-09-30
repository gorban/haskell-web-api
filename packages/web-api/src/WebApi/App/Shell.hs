module WebApi.App.Shell
  ( appPageShellForPage,
    buildAppPageShell,
    buildAppPageShellConfig,
    appRuntimeAssets,
  )
where

import HarchWeb qualified
import WebApi.App.Reauthentication (reauthenticationRuntimeAsset)
import WebApi.Components.Shell (AppShellProps (..), appPageShell)
import WebApi.Config (AppConfig (..))
import WebApi.Localization (AppMessage (SkipToMainContent), localizedMessage)
import WebApi.Route
  ( AppLocale (..),
    AppRequestContext (..),
    AppRoute (..),
    appNavigationItems,
    routeCodec,
  )

-- | The application's shared shell configuration for one rendered page.
-- Page-owned runtime requirements stay on the 'HarchWeb.Page' value and are
-- appended by 'HarchWeb.buildPageShell' after these application-wide assets.
appPageShellForPage :: AppConfig -> HarchWeb.Page AppRoute AppRequestContext -> HarchWeb.PageShell AppRoute AppRequestContext
appPageShellForPage config page =
  buildAppPageShellConfig config (HarchWeb.pageContext page)

buildAppPageShell :: AppConfig -> HarchWeb.Page AppRoute AppRequestContext -> HarchWeb.Document AppRoute
buildAppPageShell config page =
  HarchWeb.buildPageShell
    routeCodec
    (standalonePageShell (HarchWeb.pageContext page) (appPageShellForPage config page))
    page

-- | The compatibility renderer is a complete standalone document builder, so
-- it supplies the same declared navigation that 'HarchWeb.Site' supplies for
-- the normal application path.  'buildAppPageShellConfig' intentionally does
-- not include these items: adding them there would duplicate Site-owned
-- navigation in the running application.
standalonePageShell :: AppRequestContext -> HarchWeb.PageShell AppRoute AppRequestContext -> HarchWeb.PageShell AppRoute AppRequestContext
standalonePageShell context shell =
  shell {HarchWeb.shellNavigationItems = appNavigationItems context}

-- | The component and styling architecture design keeps application styling and shell composition in app-owned typed
-- functions.  The shell consumes the context's already-validated path prefix
-- so the declared stylesheet follows the same mount point as routes and
-- runtime assets, without introducing another proxy-header parser.
buildAppPageShellConfig :: AppConfig -> AppRequestContext -> HarchWeb.PageShell AppRoute AppRequestContext
buildAppPageShellConfig config context =
  appPageShell
    AppShellProps
      { appShellTitlePrefix = appTitlePrefix config,
        appShellDocumentLanguage = documentLanguage (requestLocale context),
        appShellPathPrefix = requestPathPrefix context,
        appShellStylesheet = HarchWeb.stylesheet (HarchWeb.AssetPath "/assets/styles/app.css"),
        appShellNavigationItems = noAppShellNavigationItems,
        appShellNavigationLifecycle = Just (appNavigationLifecycle context),
        appShellRuntimeAssets = appRuntimeAssets
      }

documentLanguage :: AppLocale -> HarchWeb.Locale
documentLanguage selectedLocale =
  HarchWeb.locale
    ( case selectedLocale of
        English -> "en"
        Spanish -> "es"
    )

appRuntimeAssets :: [HarchWeb.RuntimeAsset]
appRuntimeAssets = [HarchWeb.defaultDialogRuntime, reauthenticationRuntimeAsset]

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
