{-# LANGUAGE OverloadedStrings #-}

module App.Routes
  ( TwoPageRoute (..),
    routeCodec,
    routeHref,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb
  ( RouteCodec (..),
    RouteRequest (..),
  )

data TwoPageRoute
  = HomeRoute
  | SecondRoute
  | LiveDataRoute
  | LiveDataEventsRoute
  | NotFoundRoute

instance Eq TwoPageRoute where
  HomeRoute == HomeRoute = True
  SecondRoute == SecondRoute = True
  LiveDataRoute == LiveDataRoute = True
  LiveDataEventsRoute == LiveDataEventsRoute = True
  NotFoundRoute == NotFoundRoute = True
  _ == _ = False

  left /= right = not (left == right)

instance Show TwoPageRoute where
  showsPrec _ route =
    showString $
      case route of
        HomeRoute -> "HomeRoute"
        SecondRoute -> "SecondRoute"
        LiveDataRoute -> "LiveDataRoute"
        LiveDataEventsRoute -> "LiveDataEventsRoute"
        NotFoundRoute -> "NotFoundRoute"

  showList routes =
    showChar '[' . renderRoutes routes
    where
      renderRoutes remainingRoutes =
        case remainingRoutes of
          [] -> showChar ']'
          [route] -> shows route . showChar ']'
          route : nextRoutes -> shows route . showString ", " . renderRoutes nextRoutes

routeCodec :: RouteCodec TwoPageRoute ()
routeCodec =
  RouteCodec
    { parseRoute = \() path ->
        case routePath path of
          "/" -> Just RouteRequest {requestRoute = HomeRoute, requestContext = ()}
          "/second" -> Just RouteRequest {requestRoute = SecondRoute, requestContext = ()}
          "/live-data" -> Just RouteRequest {requestRoute = LiveDataRoute, requestContext = ()}
          "/live-data/events" -> Just RouteRequest {requestRoute = LiveDataEventsRoute, requestContext = ()}
          _ -> Nothing,
      renderRoute = \routeRequest -> routeHref (requestRoute routeRequest),
      notFoundRequest = \() -> RouteRequest {requestRoute = NotFoundRoute, requestContext = ()}
    }

routeHref :: TwoPageRoute -> Text
routeHref route =
  case route of
    HomeRoute -> "/"
    SecondRoute -> "/second"
    LiveDataRoute -> "/live-data"
    LiveDataEventsRoute -> "/live-data/events"
    NotFoundRoute -> "/404"

routePath :: Text -> Text
routePath =
  Text.takeWhile (/= '?')
