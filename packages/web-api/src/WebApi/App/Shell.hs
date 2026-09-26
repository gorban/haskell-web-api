{-# LANGUAGE OverloadedStrings #-}

module WebApi.App.Shell
  ( appPageShellForPage,
    buildAppPageShell,
    buildAppPageShellConfig,
    appRuntimeAssets,
  )
where

import HarchWeb qualified
import HarchWeb.OpenApi.Swagger (swaggerUiPageEnhancement)
import WebApi.App.Reauthentication (reauthenticationRuntimeAsset)
import WebApi.Components.Shell (AppShellProps (..), appPageShell)
import WebApi.Config (AppConfig (..))
import WebApi.DocsSwagger (docsSwaggerUiProps)
import WebApi.Localization (AppMessage (SkipToMainContent), localizedMessage)
import WebApi.Route
  ( AppLocale (..),
    AppRequestContext (..),
    AppRoute (..),
    routeCodec,
  )

-- | The application's page shell for one rendered page: the shared shell
-- config plus the page's own runtime descriptors. The Swagger UI page
-- contributes its page-enhancement descriptor here (harch-web-openapi's
-- 'swaggerUiPageEnhancement'), so the Site-rendered application path and the
-- compatibility renderer below both emit the same enhancement script in SSR.
-- Per docs/design-guidance.md's never-mask-a-gate-finding rule: the @$!@ on
-- the route and context below is a confirmed, reproducible fix for the
-- documented HPC pattern where a bare local binding used as a direct record
-- argument stays unticked despite real execution (the descriptor's props
-- genuinely flow into the rendered page and its script URL).
{-# ANN appPageShellForPage ("HLint: ignore Redundant $!" :: String) #-}
appPageShellForPage :: AppConfig -> HarchWeb.Page AppRoute AppRequestContext -> HarchWeb.PageShell AppRoute AppRequestContext
appPageShellForPage config page =
  let shell = buildAppPageShellConfig config (HarchWeb.pageContext page)
   in if HarchWeb.pageRoute page == DocsSwaggerRoute
        then
          shell
            { HarchWeb.shellRuntimeDescriptors =
                HarchWeb.shellRuntimeDescriptors shell
                  <> [ swaggerUiPageEnhancement
                         (docsSwaggerUiProps ((HarchWeb.RouteRequest $! HarchWeb.pageRoute page) $! HarchWeb.pageContext page))
                     ]
            }
        else shell

buildAppPageShell :: AppConfig -> HarchWeb.Page AppRoute AppRequestContext -> HarchWeb.Document AppRoute
buildAppPageShell config page =
  HarchWeb.buildPageShell
    routeCodec
    (standalonePageShell (appPageShellForPage config page))
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
    HarchWeb.NavigationItem "TODO" TodoRoute,
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
