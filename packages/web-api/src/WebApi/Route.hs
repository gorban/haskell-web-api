module WebApi.Route
  ( AppLocale (..),
    AppRequestContext (..),
    AppRoute (..),
    RouteSelectionError (..),
    defaultRequestContext,
    matchRoute,
    parseRoute,
    renderRoutePath,
    selectRoute,
    routeCodec,
  )
where

import Data.Char (isAsciiLower)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb qualified

data AppLocale
  = English
  | French
  deriving (Eq, Show)

data AppRequestContext = AppRequestContext
  { requestLocale :: AppLocale,
    requestCorrelationId :: Maybe Text
  }
  deriving (Eq, Show)

data RouteSelectionError
  = UnsupportedLocalePrefix Text
  | UnsupportedPath Text
  deriving (Eq, Show)

data AppRoute
  = HomeRoute
  | SecondRoute
  | NotFoundRoute
  deriving (Eq, Show)

defaultRequestContext :: AppRequestContext
defaultRequestContext =
  AppRequestContext
    { requestLocale = English,
      requestCorrelationId = Nothing
    }

routeCodec :: HarchWeb.RouteCodec AppRoute AppRequestContext
routeCodec =
  HarchWeb.RouteCodec
    { HarchWeb.parseRoute = parseRoute,
      HarchWeb.renderRoute = renderRoutePath,
      HarchWeb.notFoundRequest = \requestContext ->
        HarchWeb.RouteRequest {HarchWeb.requestRoute = NotFoundRoute, HarchWeb.requestContext = requestContext}
    }

parseRoute :: AppRequestContext -> Text -> Maybe (HarchWeb.RouteRequest AppRoute AppRequestContext)
parseRoute requestContext path =
  either (const Nothing) Just (selectRoute requestContext path)

selectRoute :: AppRequestContext -> Text -> Either RouteSelectionError (HarchWeb.RouteRequest AppRoute AppRequestContext)
selectRoute requestContext path = do
  (pathLocale, route) <- parseRoutePath path
  pure
    HarchWeb.RouteRequest
      { HarchWeb.requestRoute = route,
        HarchWeb.requestContext = mergeRequestContext requestContext pathLocale
      }

renderRoutePath :: HarchWeb.RouteRequest AppRoute AppRequestContext -> Text
renderRoutePath routeRequest =
  let renderedPath = Text.concat [renderLocalePrefix (requestLocale (HarchWeb.requestContext routeRequest)), renderRouteSuffix (HarchWeb.requestRoute routeRequest)]
   in if Text.null renderedPath then Text.pack "/" else renderedPath

matchRoute :: AppRequestContext -> Text -> HarchWeb.RouteRequest AppRoute AppRequestContext
matchRoute = HarchWeb.matchRoute routeCodec

mergeRequestContext :: AppRequestContext -> Maybe AppLocale -> AppRequestContext
mergeRequestContext requestContext maybeLocale =
  requestContext
    { requestLocale =
        case maybeLocale of
          Just locale -> locale
          Nothing -> requestLocale requestContext
    }

parseRoutePath :: Text -> Either RouteSelectionError (Maybe AppLocale, AppRoute)
parseRoutePath path
  | not (Text.isPrefixOf (Text.pack "/") path) = Left (UnsupportedPath path)
  | path /= Text.pack "/" && Text.isSuffixOf (Text.pack "/") path = Left (UnsupportedPath path)
parseRoutePath path =
  case drop 1 (Text.splitOn (Text.pack "/") path) of
    [segment]
      | Text.null segment -> Right (Nothing, HomeRoute)
    [segment] -> parseSingleSegmentPath path segment
    [prefix, segment] -> parsePrefixedPath path prefix segment
    _ -> Left (UnsupportedPath path)

parseSingleSegmentPath :: Text -> Text -> Either RouteSelectionError (Maybe AppLocale, AppRoute)
parseSingleSegmentPath fullPath segment =
  case routeFromSegment segment of
    Just route -> Right (Nothing, route)
    Nothing ->
      case localeFromPrefix segment of
        Just locale -> Right (Just locale, HomeRoute)
        Nothing ->
          if looksLikeLocalePrefix segment
            then Left (UnsupportedLocalePrefix segment)
            else Left (UnsupportedPath fullPath)

parsePrefixedPath :: Text -> Text -> Text -> Either RouteSelectionError (Maybe AppLocale, AppRoute)
parsePrefixedPath fullPath prefix segment =
  case localeFromPrefix prefix of
    Just locale ->
      case routeFromSegment segment of
        Just route -> Right (Just locale, route)
        Nothing -> Left (UnsupportedPath fullPath)
    Nothing ->
      if looksLikeLocalePrefix prefix
        then Left (UnsupportedLocalePrefix prefix)
        else Left (UnsupportedPath fullPath)

routeFromSegment :: Text -> Maybe AppRoute
routeFromSegment segment
  | segment == Text.pack "second" = Just SecondRoute
  | segment == Text.pack "404" = Just NotFoundRoute
routeFromSegment _ = Nothing

localeFromPrefix :: Text -> Maybe AppLocale
localeFromPrefix prefix
  | prefix == Text.pack "en" = Just English
  | prefix == Text.pack "fr" = Just French
localeFromPrefix _ = Nothing

looksLikeLocalePrefix :: Text -> Bool
looksLikeLocalePrefix prefix =
  Text.length prefix == 2 && Text.all isAsciiLower prefix

renderLocalePrefix :: AppLocale -> Text
renderLocalePrefix locale =
  case locale of
    English -> Text.empty
    French -> Text.pack "/fr"

renderRouteSuffix :: AppRoute -> Text
renderRouteSuffix route =
  case route of
    HomeRoute -> Text.empty
    SecondRoute -> Text.pack "/second"
    NotFoundRoute -> Text.pack "/404"
