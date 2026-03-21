{-# LANGUAGE OverloadedStrings #-}

module WebApi.App
  ( buildAppWithDatabase,
    buildApp,
    run,
  )
where

import Data.Text (Text)
import HarchWeb qualified
import System.IO (Handle)
import WebApi.Config (AppConfig (..), defaultAppConfig)
import WebApi.Database (DatabaseEffect, defaultDatabaseEffect)
import WebApi.Response (selectResponseWithDatabase)
import WebApi.Route
  ( AppRequestContext,
    AppRoute (..),
    defaultRequestContext,
    routeCodec,
  )

appShell :: AppConfig -> HarchWeb.Page AppRoute AppRequestContext -> Text
appShell config = HarchWeb.buildPageShell routeCodec (appShellConfig config)

appShellConfig :: AppConfig -> HarchWeb.PageShell AppRoute AppRequestContext
appShellConfig config =
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

buildAppWithDatabase :: AppConfig -> DatabaseEffect -> HarchWeb.Application AppRoute AppRequestContext
buildAppWithDatabase config databaseEffect =
  config `seq`
    HarchWeb.application
      HarchWeb.Application
        { HarchWeb.appName = "web-api",
          HarchWeb.defaultRequestContext = defaultRequestContext,
          HarchWeb.applicationStaticAssets = staticAssets config,
          HarchWeb.routeCodec = routeCodec,
          HarchWeb.renderResponse = selectResponseWithDatabase config databaseEffect,
          HarchWeb.pageShell = appShell config
        }

buildApp :: AppConfig -> HarchWeb.Application AppRoute AppRequestContext
buildApp config =
  buildAppWithDatabase config defaultDatabaseEffect

run :: Handle -> IO ()
run outputHandle =
  HarchWeb.runServer outputHandle defaultAppConfig (buildApp defaultAppConfig)
