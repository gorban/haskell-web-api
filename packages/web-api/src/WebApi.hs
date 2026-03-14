module WebApi
  ( AppConfig (..)
  , AppRoute (..)
  , buildApp
  , defaultAppConfig
  , matchRoute
  , renderRoute
  , run
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified HarchWeb

data AppConfig = AppConfig
  { appTitlePrefix :: Text
  }
  deriving (Eq, Show)

data AppRoute
  = HomeRoute
  | SecondRoute
  | NotFoundRoute
  deriving (Eq, Show)

defaultAppConfig :: AppConfig
defaultAppConfig = AppConfig {appTitlePrefix = Text.pack "web-api"}

routeCodec :: HarchWeb.RouteCodec AppRoute
routeCodec =
  HarchWeb.RouteCodec
    { HarchWeb.parseRoute = parseAppRoute
    , HarchWeb.renderRoute = renderAppRoute
    , HarchWeb.notFoundRoute = NotFoundRoute
    }

parseAppRoute :: Text -> Maybe AppRoute
parseAppRoute path
  | path == Text.pack "/" = Just HomeRoute
  | path == Text.pack "/second" = Just SecondRoute
  | otherwise = Nothing

renderAppRoute :: AppRoute -> Text
renderAppRoute route =
  case route of
    HomeRoute -> Text.pack "/"
    SecondRoute -> Text.pack "/second"
    NotFoundRoute -> Text.pack "/404"

matchRoute :: Text -> AppRoute
matchRoute = HarchWeb.matchRoute routeCodec

renderRoute :: AppConfig -> AppRoute -> HarchWeb.Page AppRoute
renderRoute config route =
  HarchWeb.Page
    { HarchWeb.pageTitle = Text.concat [appTitlePrefix config, Text.pack ": ", routeTitle route]
    , HarchWeb.pageRoute = route
    , HarchWeb.pageBody = routeBody route
    }

routeTitle :: AppRoute -> Text
routeTitle route =
  case route of
    HomeRoute -> Text.pack "Home"
    SecondRoute -> Text.pack "Second"
    NotFoundRoute -> Text.pack "Not Found"

routeBody :: AppRoute -> Text
routeBody route =
  case route of
    HomeRoute -> Text.pack "<h1>Home</h1>"
    SecondRoute -> Text.pack "<h1>Second</h1>"
    NotFoundRoute -> Text.pack "<h1>Not Found</h1>"

appShell :: AppConfig -> HarchWeb.Page AppRoute -> Text
appShell config page =
  Text.concat
    [ Text.pack "<html><head><title>"
    , HarchWeb.pageTitle page
    , Text.pack "</title></head><body data-app=\""
    , appTitlePrefix config
    , Text.pack "\"><main>"
    , HarchWeb.pageBody page
    , Text.pack "</main></body></html>"
    ]

buildApp :: AppConfig -> HarchWeb.Application AppRoute
buildApp config =
  HarchWeb.application
    HarchWeb.Application
      { HarchWeb.appName = Text.pack "web-api"
      , HarchWeb.routeCodec = routeCodec
      , HarchWeb.renderPage = renderRoute config
      , HarchWeb.notFoundPage = renderRoute config NotFoundRoute
      , HarchWeb.pageShell = appShell config
      }

run :: IO ()
run = HarchWeb.runServer defaultAppConfig (buildApp defaultAppConfig)
