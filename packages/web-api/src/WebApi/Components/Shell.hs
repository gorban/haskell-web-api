{-# LANGUAGE OverloadedStrings #-}

-- | App-owned shell configuration.
--
-- AHI-1 composes the existing Harch shell, stylesheet, and validated path
-- prefix types here.  The request context has already established trust for
-- the prefix; this component only applies that typed value and never reparses
-- proxy input.
module WebApi.Components.Shell
  ( AppShellProps (..),
    appPageShell,
  )
where

import Data.Text (Text)
import HarchWeb qualified
import WebApi.Route (AppRequestContext, AppRoute)

data AppShellProps = AppShellProps
  { appShellTitlePrefix :: Text,
    appShellPathPrefix :: HarchWeb.PathPrefix,
    appShellStylesheet :: HarchWeb.Stylesheet,
    appShellNavigationItems :: [HarchWeb.NavigationItem AppRoute],
    appShellNavigationLifecycle :: Maybe HarchWeb.NavigationLifecycle
  }

appPageShell :: AppShellProps -> HarchWeb.PageShell AppRoute AppRequestContext
appPageShell AppShellProps {appShellTitlePrefix, appShellPathPrefix, appShellStylesheet, appShellNavigationItems, appShellNavigationLifecycle} =
  HarchWeb.PageShell
    { HarchWeb.shellBodyAttributes =
        [ HarchWeb.HtmlAttribute "data-app" appShellTitlePrefix,
          scopedClassAttribute "app-shell" "body"
        ],
      HarchWeb.shellNavigationAttributes =
        [ HarchWeb.HtmlAttribute "data-navigation-region" "primary",
          scopedClassAttribute "app-shell" "navigation"
        ],
      HarchWeb.shellNavigationItems = appShellNavigationItems,
      HarchWeb.shellMainId = HarchWeb.literalElementId "app-main",
      HarchWeb.shellMainAttributes =
        [ HarchWeb.HtmlAttribute "data-navigation-content" "true",
          scopedClassAttribute "app-shell" "main"
        ],
      HarchWeb.shellNavigationLifecycle = appShellNavigationLifecycle,
      HarchWeb.shellStylesheets = [stylesheetWithPrefix appShellPathPrefix appShellStylesheet],
      HarchWeb.shellRuntimeDescriptors = []
    }

stylesheetWithPrefix :: HarchWeb.PathPrefix -> HarchWeb.Stylesheet -> HarchWeb.Stylesheet
stylesheetWithPrefix pathPrefix (HarchWeb.Stylesheet (HarchWeb.AssetPath assetPath)) =
  HarchWeb.stylesheet
    ( HarchWeb.AssetPath
        ( HarchWeb.urlPathText
            (HarchWeb.applyRequestPathPrefix pathPrefix (HarchWeb.mkUrlPath assetPath))
        )
    )

scopedClassAttribute :: Text -> Text -> HarchWeb.HtmlAttribute
scopedClassAttribute scopeName localName =
  HarchWeb.HtmlAttribute
    "class"
    (HarchWeb.cssClassText (HarchWeb.ScopedCssClass (HarchWeb.cssScope scopeName) localName))
