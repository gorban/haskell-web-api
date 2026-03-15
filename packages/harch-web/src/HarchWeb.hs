module HarchWeb
  ( Application (..),
    Page (..),
    RouteCodec (..),
    application,
    matchRoute,
    runServer,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)

data Page route = Page
  { pageTitle :: Text,
    pageRoute :: route,
    pageBody :: Text
  }
  deriving (Eq, Show)

data RouteCodec route = RouteCodec
  { parseRoute :: Text -> Maybe route,
    renderRoute :: route -> Text,
    notFoundRoute :: route
  }

data Application route = Application
  { appName :: Text,
    routeCodec :: RouteCodec route,
    renderPage :: route -> Page route,
    notFoundPage :: Page route,
    pageShell :: Page route -> Text
  }

application :: Application route -> Application route
application = id

matchRoute :: RouteCodec route -> Text -> route
matchRoute codec path = fromMaybe (notFoundRoute codec) (parseRoute codec path)

runServer :: config -> Application route -> IO ()
runServer _config _application = pure ()
