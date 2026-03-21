module WebApi.App
  ( buildApp,
    run,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb qualified
import System.IO (Handle)
import WebApi.Config (AppConfig (..), defaultAppConfig)
import WebApi.Page (renderPage)
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
            { HarchWeb.attributeName = Text.pack "data-app",
              HarchWeb.attributeValue = appTitlePrefix config
            }
        ],
      HarchWeb.shellNavigationItems =
        [ HarchWeb.NavigationItem
            { HarchWeb.navigationLabel = Text.pack "Home",
              HarchWeb.navigationRoute = HomeRoute
            },
          HarchWeb.NavigationItem
            { HarchWeb.navigationLabel = Text.pack "Second",
              HarchWeb.navigationRoute = SecondRoute
            }
        ],
      HarchWeb.shellMainId = Text.pack "app-main"
    }

renderResponse :: AppConfig -> HarchWeb.RouteRequest AppRoute AppRequestContext -> HarchWeb.Response AppRoute AppRequestContext
renderResponse config routeRequest = HarchWeb.PageResponse (renderPage config routeRequest)

buildApp :: AppConfig -> HarchWeb.Application AppRoute AppRequestContext
buildApp config =
  config `seq`
    HarchWeb.application
      HarchWeb.Application
        { HarchWeb.appName = Text.pack "web-api",
          HarchWeb.defaultRequestContext = defaultRequestContext,
          HarchWeb.routeCodec = routeCodec,
          HarchWeb.renderResponse = renderResponse config,
          HarchWeb.pageShell = appShell config
        }

run :: Handle -> IO ()
run outputHandle =
  HarchWeb.runServer outputHandle defaultAppConfig (buildApp defaultAppConfig)
