-- | Typed route matching and rendering for HarchWeb applications.
--
-- The framework facade re-exports this module. Applications can keep using
-- 'HarchWeb' for the convenience API, while this focused module owns the
-- route codec boundary without depending on server or document machinery.
module HarchWeb.Routing
  ( RouteCodec (..),
    RouteRequest (..),
    matchRoute,
    routeHref,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)

data RouteRequest route context = RouteRequest
  { requestRoute :: route,
    requestContext :: context
  }
  deriving (Eq, Show)

data RouteCodec route context = RouteCodec
  { parseRoute :: context -> Text -> Maybe (RouteRequest route context),
    renderRoute :: RouteRequest route context -> Text,
    notFoundRequest :: context -> RouteRequest route context
  }

routeHref :: RouteCodec route context -> context -> route -> Text
routeHref codec context route =
  renderRoute codec RouteRequest {requestRoute = route, requestContext = context}

matchRoute :: RouteCodec route context -> context -> Text -> RouteRequest route context
matchRoute codec context path =
  fromMaybe (notFoundRequest codec context) (parseRoute codec context path)
