module WebApi.Route
  ( AppLocale (..),
    AppRequestContext (..),
    AppRoute (..),
    RequestSurface (..),
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

data RequestSurface
  = PageSurface
  | ApiSurface
  deriving (Eq, Show)

data AppRequestContext = AppRequestContext
  { requestLocale :: AppLocale,
    requestCorrelationId :: Maybe Text,
    requestSurface :: RequestSurface
  }
  deriving (Eq, Show)

data RouteSelectionError
  = UnsupportedLocalePrefix Text
  | UnsupportedPath Text
  deriving (Eq, Show)

data AppRoute
  = HomeRoute
  | SecondRoute
  | StatusApiRoute
  | NotFoundRoute
  deriving (Eq, Show)

defaultRequestContext :: AppRequestContext
defaultRequestContext =
  AppRequestContext
    { requestLocale = English,
      requestCorrelationId = Nothing,
      requestSurface = PageSurface
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
  (pathLocale, pathSurface, route) <- parseRoutePath path
  pure
    HarchWeb.RouteRequest
      { HarchWeb.requestRoute = route,
        HarchWeb.requestContext = mergeRequestContext requestContext pathLocale pathSurface
      }

renderRoutePath :: HarchWeb.RouteRequest AppRoute AppRequestContext -> Text
renderRoutePath routeRequest =
  case requestSurface (HarchWeb.requestContext routeRequest) of
    ApiSurface -> renderApiRoutePath (HarchWeb.requestRoute routeRequest)
    PageSurface ->
      let renderedPath = Text.concat [renderLocalePrefix (requestLocale (HarchWeb.requestContext routeRequest)), renderPageRouteSuffix (HarchWeb.requestRoute routeRequest)]
       in if Text.null renderedPath then Text.pack "/" else renderedPath

matchRoute :: AppRequestContext -> Text -> HarchWeb.RouteRequest AppRoute AppRequestContext
matchRoute = HarchWeb.matchRoute routeCodec

mergeRequestContext :: AppRequestContext -> Maybe AppLocale -> RequestSurface -> AppRequestContext
mergeRequestContext requestContext maybeLocale pathSurface =
  requestContext
    { requestLocale =
        case maybeLocale of
          Just locale -> locale
          Nothing -> requestLocale requestContext,
      requestSurface = pathSurface
    }

parseRoutePath :: Text -> Either RouteSelectionError (Maybe AppLocale, RequestSurface, AppRoute)
parseRoutePath path
  | not (Text.isPrefixOf (Text.pack "/") path) = Left (UnsupportedPath path)
  | path /= Text.pack "/" && Text.isSuffixOf (Text.pack "/") path = Left (UnsupportedPath path)
parseRoutePath path =
  case drop 1 (Text.splitOn (Text.pack "/") path) of
    [segment]
      | Text.null segment -> Right (Nothing, PageSurface, HomeRoute)
    [segment]
      | segment == Text.pack "api" -> Right (Nothing, ApiSurface, NotFoundRoute)
    [segment] -> parseSingleSegmentPath path segment
    [prefix, segment]
      | prefix == Text.pack "api" -> parseApiPath segment
    [prefix, segment] -> parsePrefixedPath path prefix segment
    apiPrefix : _
      | apiPrefix == Text.pack "api" -> Right (Nothing, ApiSurface, NotFoundRoute)
    _ -> Left (UnsupportedPath path)

parseSingleSegmentPath :: Text -> Text -> Either RouteSelectionError (Maybe AppLocale, RequestSurface, AppRoute)
parseSingleSegmentPath fullPath segment =
  case routeFromSegment segment of
    Just route -> Right (Nothing, PageSurface, route)
    Nothing ->
      case localeFromPrefix segment of
        Just locale -> Right (Just locale, PageSurface, HomeRoute)
        Nothing ->
          if looksLikeLocalePrefix segment
            then Left (UnsupportedLocalePrefix segment)
            else Left (UnsupportedPath fullPath)

parsePrefixedPath :: Text -> Text -> Text -> Either RouteSelectionError (Maybe AppLocale, RequestSurface, AppRoute)
parsePrefixedPath fullPath prefix segment =
  case localeFromPrefix prefix of
    Just locale ->
      case routeFromSegment segment of
        Just route -> Right (Just locale, PageSurface, route)
        Nothing -> Left (UnsupportedPath fullPath)
    Nothing ->
      if looksLikeLocalePrefix prefix
        then Left (UnsupportedLocalePrefix prefix)
        else Left (UnsupportedPath fullPath)

parseApiPath :: Text -> Either RouteSelectionError (Maybe AppLocale, RequestSurface, AppRoute)
parseApiPath segment
  | segment == Text.pack "status" = Right (Nothing, ApiSurface, StatusApiRoute)
  | segment == Text.pack "404" = Right (Nothing, ApiSurface, NotFoundRoute)
parseApiPath _ = Right (Nothing, ApiSurface, NotFoundRoute)

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

renderPageRouteSuffix :: AppRoute -> Text
renderPageRouteSuffix route =
  case route of
    HomeRoute -> Text.empty
    SecondRoute -> Text.pack "/second"
    NotFoundRoute -> Text.pack "/404"
    StatusApiRoute -> Text.pack "/404"

renderApiRoutePath :: AppRoute -> Text
renderApiRoutePath route =
  case route of
    StatusApiRoute -> Text.pack "/api/status"
    _ -> Text.pack "/api/404"
