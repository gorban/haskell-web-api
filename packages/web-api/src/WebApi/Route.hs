{-# LANGUAGE OverloadedStrings #-}

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
    requestContextFromWaiRequest,
    selectRoute,
    routeCodec,
  )
where

import Data.Char (isAsciiLower)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb qualified
import Network.Wai qualified as Wai

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
    requestSurface :: RequestSurface,
    requestPathPrefix :: Text
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
      requestSurface = PageSurface,
      requestPathPrefix = Text.empty
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
parseRoute requestContext target =
  either (const Nothing) Just (selectRoute requestContext target)

selectRoute :: AppRequestContext -> Text -> Either RouteSelectionError (HarchWeb.RouteRequest AppRoute AppRequestContext)
selectRoute requestContext target = do
  let path = routePath target
  (pathLocale, pathSurface, route) <- parseRoutePath path
  pure
    HarchWeb.RouteRequest
      { HarchWeb.requestRoute = route,
        HarchWeb.requestContext = mergeRequestContext requestContext pathLocale pathSurface
      }

renderRoutePath :: HarchWeb.RouteRequest AppRoute AppRequestContext -> Text
renderRoutePath routeRequest =
  applyRequestPathPrefix
    (requestPathPrefix (HarchWeb.requestContext routeRequest))
    ( case requestSurface (HarchWeb.requestContext routeRequest) of
        ApiSurface -> renderApiRoutePath (HarchWeb.requestRoute routeRequest)
        PageSurface ->
          let renderedPath = Text.concat [renderLocalePrefix (requestLocale (HarchWeb.requestContext routeRequest)), renderPageRouteSuffix (HarchWeb.requestRoute routeRequest)]
           in if Text.null renderedPath then "/" else renderedPath
    )

matchRoute :: AppRequestContext -> Text -> HarchWeb.RouteRequest AppRoute AppRequestContext
matchRoute = HarchWeb.matchRoute routeCodec

routePath :: Text -> Text
routePath =
  Text.takeWhile (/= '?')

mergeRequestContext :: AppRequestContext -> Maybe AppLocale -> RequestSurface -> AppRequestContext
mergeRequestContext requestContext maybeLocale pathSurface =
  requestContext
    { requestLocale =
        case maybeLocale of
          Just locale -> locale
          Nothing -> requestLocale requestContext,
      requestSurface = pathSurface
    }

requestContextFromWaiRequest :: Bool -> Wai.Request -> AppRequestContext -> AppRequestContext
requestContextFromWaiRequest trustProxyHeaders request requestContext =
  requestContext
    { requestPathPrefix =
        if trustProxyHeaders
          then
            maybe
              Text.empty
              normalizeRequestPathPrefix
              ( lookup "X-Forwarded-Prefix" (Wai.requestHeaders request)
                  >>= firstCommaSeparatedValue . Text.strip . TextEncoding.decodeUtf8
              )
          else Text.empty
    }

parseRoutePath :: Text -> Either RouteSelectionError (Maybe AppLocale, RequestSurface, AppRoute)
parseRoutePath path
  | not (Text.isPrefixOf "/" path) = Left (UnsupportedPath path)
  | path /= "/" && Text.isSuffixOf "/" path = Left (UnsupportedPath path)
parseRoutePath path =
  case drop 1 (Text.splitOn "/" path) of
    [segment]
      | Text.null segment -> Right (Nothing, PageSurface, HomeRoute)
    [segment]
      | segment == "api" -> Right (Nothing, ApiSurface, NotFoundRoute)
    [segment] -> parseSingleSegmentPath path segment
    [prefix, segment]
      | prefix == "api" -> parseApiPath segment
    [prefix, segment] -> parsePrefixedPath path prefix segment
    apiPrefix : _
      | apiPrefix == "api" -> Right (Nothing, ApiSurface, NotFoundRoute)
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
  | segment == "status" = Right (Nothing, ApiSurface, StatusApiRoute)
  | segment == "second" = Right (Nothing, ApiSurface, SecondRoute)
  | segment == "404" = Right (Nothing, ApiSurface, NotFoundRoute)
parseApiPath _ = Right (Nothing, ApiSurface, NotFoundRoute)

routeFromSegment :: Text -> Maybe AppRoute
routeFromSegment segment
  | segment == "second" = Just SecondRoute
  | segment == "404" = Just NotFoundRoute
routeFromSegment _ = Nothing

localeFromPrefix :: Text -> Maybe AppLocale
localeFromPrefix prefix
  | prefix == "en" = Just English
  | prefix == "fr" = Just French
localeFromPrefix _ = Nothing

looksLikeLocalePrefix :: Text -> Bool
looksLikeLocalePrefix prefix =
  Text.length prefix == 2 && Text.all isAsciiLower prefix

renderLocalePrefix :: AppLocale -> Text
renderLocalePrefix locale =
  case locale of
    English -> Text.empty
    French -> "/fr"

renderPageRouteSuffix :: AppRoute -> Text
renderPageRouteSuffix route =
  case route of
    HomeRoute -> Text.empty
    SecondRoute -> "/second"
    NotFoundRoute -> "/404"
    StatusApiRoute -> "/404"

renderApiRoutePath :: AppRoute -> Text
renderApiRoutePath route =
  case route of
    StatusApiRoute -> "/api/status"
    SecondRoute -> "/api/second"
    _ -> "/api/404"

firstCommaSeparatedValue :: Text -> Maybe Text
firstCommaSeparatedValue value =
  case filter (not . Text.null) (map Text.strip (Text.splitOn "," value)) of
    [] -> Nothing
    firstValue : _ -> Just firstValue

normalizeRequestPathPrefix :: Text -> Text
normalizeRequestPathPrefix pathPrefix =
  let trimmedPrefix = Text.strip pathPrefix
      slashPrefixedPrefix =
        case (Text.null trimmedPrefix || trimmedPrefix == "/", Text.isPrefixOf "/" trimmedPrefix) of
          (True, _) -> Text.empty
          (False, True) -> trimmedPrefix
          (False, False) -> "/" <> trimmedPrefix
      normalizedPrefix =
        Text.dropWhileEnd (== '/') slashPrefixedPrefix
   in normalizedPrefix

applyRequestPathPrefix :: Text -> Text -> Text
applyRequestPathPrefix pathPrefix path =
  let normalizedPrefix = normalizeRequestPathPrefix pathPrefix
   in if Text.null normalizedPrefix
        then path
        else
          if path == "/"
            then normalizedPrefix
            else normalizedPrefix <> path
