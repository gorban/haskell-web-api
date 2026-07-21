{-# LANGUAGE OverloadedStrings #-}

module WebApi.Route
  ( AppLocale (..),
    AppRequestContext (..),
    AppRoute (..),
    RequestSurface (..),
    RouteMetadata (..),
    RouteSelectionError (..),
    defaultRequestContext,
    matchRoute,
    parseRoute,
    renderRoutePath,
    requestContextFromWaiRequest,
    routeMetadata,
    selectRoute,
    routeCodec,
  )
where

import Data.Char (isAsciiLower)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb qualified
import HarchWeb.Session
  ( SessionId,
    defaultSessionCookiePolicy,
    mkSessionId,
    sessionCookieName,
    sessionCookieNameText,
  )
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai

data AppLocale
  = English
  | Spanish
  deriving (Eq, Show)

data RequestSurface
  = PageSurface
  | ApiSurface
  deriving (Eq, Show)

data AppRequestContext = AppRequestContext
  { requestLocale :: AppLocale,
    requestLocaleIsExplicit :: Bool,
    requestCorrelationId :: Maybe Text,
    requestSurface :: RequestSurface,
    requestPathPrefix :: Text,
    requestQueryParameters :: [(Text, Text)],
    requestSessionId :: Maybe SessionId
  }
  deriving (Eq, Show)

data RouteSelectionError
  = UnsupportedLocalePrefix Text
  | UnsupportedPath Text
  deriving (Eq, Show)

data AppRoute
  = HomeRoute
  | SecondRoute
  | SpacesRoute
  | RegistrationRoute
  | EmailVerificationRoute
  | MfaEnrollmentRoute
  | LoginRoute
  | LogoutRoute
  | StatusApiRoute
  | NotFoundRoute
  deriving (Eq, Show)

data RouteMetadata = RouteMetadata
  { routePageSegment :: Maybe Text,
    routePageSuffix :: Text,
    routePageTitle :: Text,
    routeEnhancementHooks :: [Text]
  }

defaultRequestContext :: AppRequestContext
defaultRequestContext =
  AppRequestContext
    { requestLocale = English,
      requestLocaleIsExplicit = False,
      requestCorrelationId = Nothing,
      requestSurface = PageSurface,
      requestPathPrefix = Text.empty,
      requestQueryParameters = [],
      requestSessionId = Nothing
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
        HarchWeb.requestContext = (mergeRequestContext requestContext pathLocale pathSurface) {requestQueryParameters = routeQuery target}
      }

renderRoutePath :: HarchWeb.RouteRequest AppRoute AppRequestContext -> Text
renderRoutePath routeRequest =
  applyRequestPathPrefix
    (requestPathPrefix (HarchWeb.requestContext routeRequest))
    ( case requestSurface (HarchWeb.requestContext routeRequest) of
        ApiSurface -> renderApiRoutePath (HarchWeb.requestRoute routeRequest)
        PageSurface ->
          let requestContext = HarchWeb.requestContext routeRequest
              renderedPath = Text.concat [renderLocalePrefix (requestLocale requestContext) (requestLocaleIsExplicit requestContext), renderPageRouteSuffix (HarchWeb.requestRoute routeRequest)]
           in if Text.null renderedPath then "/" else renderedPath
    )

matchRoute :: AppRequestContext -> Text -> HarchWeb.RouteRequest AppRoute AppRequestContext
matchRoute = HarchWeb.matchRoute routeCodec

routePath :: Text -> Text
routePath =
  Text.takeWhile (/= '?')

routeQuery :: Text -> [(Text, Text)]
routeQuery target =
  case Text.breakOn "?" target of
    (_, queryText) ->
      case Text.uncons queryText of
        Nothing -> []
        Just _ -> mapMaybe parsePair (Text.splitOn "&" (Text.drop 1 queryText))
  where
    parsePair pair =
      case Text.breakOn "=" pair of
        (key, value) ->
          case Text.uncons key of
            Nothing -> Nothing
            Just _ -> Just (key, Text.drop 1 value)

mergeRequestContext :: AppRequestContext -> Maybe AppLocale -> RequestSurface -> AppRequestContext
mergeRequestContext requestContext maybeLocale pathSurface =
  requestContext
    { requestLocale =
        case maybeLocale of
          Just locale -> locale
          Nothing -> requestLocale requestContext,
      requestLocaleIsExplicit =
        case maybeLocale of
          Just _ -> True
          Nothing -> requestLocaleIsExplicit requestContext,
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
          else Text.empty,
      requestSessionId = requestSessionIdFromHeaders (Wai.requestHeaders request)
    }

parseRoutePath :: Text -> Either RouteSelectionError (Maybe AppLocale, RequestSurface, AppRoute)
parseRoutePath path = parseRouteSegments path =<< tokenizeRoutePath path

tokenizeRoutePath :: Text -> Either RouteSelectionError [Text]
tokenizeRoutePath path
  | not (Text.isPrefixOf "/" path) = Left (UnsupportedPath path)
  | path /= "/" && Text.isSuffixOf "/" path = Left (UnsupportedPath path)
tokenizeRoutePath path = Right (drop 1 (Text.splitOn "/" path))

parseRouteSegments :: Text -> [Text] -> Either RouteSelectionError (Maybe AppLocale, RequestSurface, AppRoute)
parseRouteSegments path segments =
  case segments of
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
routeFromSegment segment =
  lookup
    segment
    [ (configuredSegment, route)
    | route <- pageRoutes,
      Just configuredSegment <- [routePageSegment (routeMetadata route)]
    ]

pageRoutes :: [AppRoute]
pageRoutes =
  [ HomeRoute,
    SecondRoute,
    SpacesRoute,
    RegistrationRoute,
    EmailVerificationRoute,
    MfaEnrollmentRoute,
    LoginRoute,
    LogoutRoute,
    NotFoundRoute
  ]

localeFromPrefix :: Text -> Maybe AppLocale
localeFromPrefix prefix
  | prefix == "en" = Just English
  | prefix == "es" = Just Spanish
localeFromPrefix _ = Nothing

looksLikeLocalePrefix :: Text -> Bool
looksLikeLocalePrefix prefix =
  Text.length prefix == 2 && Text.all isAsciiLower prefix

renderLocalePrefix :: AppLocale -> Bool -> Text
renderLocalePrefix locale isExplicit =
  case locale of
    English -> if isExplicit then "/en" else Text.empty
    Spanish -> "/es"

renderPageRouteSuffix :: AppRoute -> Text
renderPageRouteSuffix = routePageSuffix . routeMetadata

routeMetadata :: AppRoute -> RouteMetadata
routeMetadata route =
  case route of
    HomeRoute -> RouteMetadata Nothing Text.empty "Home" []
    SecondRoute -> RouteMetadata (Just "second") "/second" "Second" ["second-page"]
    SpacesRoute -> RouteMetadata (Just "spaces") "/spaces" "Spaces" []
    RegistrationRoute -> RouteMetadata (Just "register") "/register" "Create account" []
    EmailVerificationRoute -> RouteMetadata (Just "verify") "/verify" "Verify email" []
    MfaEnrollmentRoute -> RouteMetadata (Just "mfa") "/mfa" "Set up authenticator" []
    LoginRoute -> RouteMetadata (Just "login") "/login" "Sign in" []
    LogoutRoute -> RouteMetadata (Just "logout") "/logout" "Sign out" []
    NotFoundRoute -> RouteMetadata (Just "404") "/404" "Not Found" []
    StatusApiRoute -> RouteMetadata Nothing "/404" "Not Found" []

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

requestSessionIdFromHeaders :: Http.RequestHeaders -> Maybe SessionId
requestSessionIdFromHeaders headers = do
  cookieHeader <- lookup "Cookie" headers
  cookieText <- either (const Nothing) Just (TextEncoding.decodeUtf8' cookieHeader)
  cookieValue <- lookup (sessionCookieNameText (sessionCookieName defaultSessionCookiePolicy)) (map parseCookiePair (Text.splitOn ";" cookieText))
  mkSessionId cookieValue
  where
    parseCookiePair value =
      let (name, rawValue) = Text.breakOn "=" (Text.strip value)
       in (name, Text.drop 1 rawValue)

applyRequestPathPrefix :: Text -> Text -> Text
applyRequestPathPrefix pathPrefix path =
  let normalizedPrefix = normalizeRequestPathPrefix pathPrefix
   in if Text.null normalizedPrefix
        then path
        else
          if path == "/"
            then normalizedPrefix
            else normalizedPrefix <> path
