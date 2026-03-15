module HarchWeb
  ( Application (..),
    Page (..),
    Response (..),
    ResponseBody (..),
    RouteCodec (..),
    RouteRequest (..),
    application,
    matchRoute,
    runServer,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import System.IO (Handle, hPutStrLn)

data RouteRequest route context = RouteRequest
  { requestRoute :: route,
    requestContext :: context
  }
  deriving (Eq, Show)

data Page route context = Page
  { pageTitle :: Text,
    pageRoute :: route,
    pageContext :: context,
    pageBody :: Text
  }
  deriving (Eq, Show)

data ResponseBody = ResponseBody
  { responseStatus :: Int,
    responseContentType :: Text,
    responseBody :: Text
  }
  deriving (Eq, Show)

data Response route context
  = PageResponse (Page route context)
  | BodyResponse ResponseBody
  deriving (Eq, Show)

data RouteCodec route context = RouteCodec
  { parseRoute :: context -> Text -> Maybe (RouteRequest route context),
    renderRoute :: RouteRequest route context -> Text,
    notFoundRequest :: context -> RouteRequest route context
  }

data Application route context = Application
  { appName :: Text,
    routeCodec :: RouteCodec route context,
    renderResponse :: RouteRequest route context -> Response route context,
    pageShell :: Page route context -> Text
  }

application :: Application route context -> Application route context
application = id

matchRoute :: RouteCodec route context -> context -> Text -> RouteRequest route context
matchRoute codec context path = fromMaybe (notFoundRequest codec context) (parseRoute codec context path)

runServer :: Handle -> config -> Application route context -> IO ()
runServer outputHandle _config _application =
  hPutStrLn outputHandle "HTTP Server listening at http://localhost:5001"
