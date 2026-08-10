{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module WebApi.Route
  ( AppLocale (..),
    AppRequestContext (..),
    AppRoute
      ( Page,
        Api,
        HomeRoute,
        SecondRoute,
        SpacesRoute,
        RegistrationRoute,
        EmailVerificationRoute,
        MfaEnrollmentRoute,
        LoginRoute,
        LogoutRoute,
        ProfileRoute,
        StatusApiRoute,
        SecondApiRoute,
        NotFoundRoute,
        ApiNotFoundRoute
      ),
    ApiRoute (..),
    PageRoute (..),
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

data AppRequestContext = AppRequestContext
  { requestLocale :: AppLocale,
    requestLocaleIsExplicit :: Bool,
    requestCorrelationId :: Maybe Text,
    requestPathPrefix :: Text,
    requestQueryParameters :: [(Text, Text)],
    requestSessionId :: Maybe SessionId
  }
  deriving (Eq, Show)

data RouteSelectionError
  = UnsupportedLocalePrefix Text
  | UnsupportedPath Text
  deriving (Eq, Show)

data PageRoute
  = HomePage
  | SecondPage
  | SpacesPage
  | RegistrationPage
  | EmailVerificationPage
  | MfaEnrollmentPage
  | LoginPage
  | LogoutPage
  | ProfilePage
  | PageNotFound
  deriving (Bounded, Enum, Eq, Show)

data ApiRoute
  = StatusApi
  | SecondApi
  | ApiNotFound
  deriving (Bounded, Enum, Eq, Show)

data AppRoute
  = Page PageRoute
  | Api ApiRoute
  deriving (Eq)

pattern HomeRoute :: AppRoute
pattern HomeRoute = Page HomePage

pattern SecondRoute :: AppRoute
pattern SecondRoute = Page SecondPage

pattern SpacesRoute :: AppRoute
pattern SpacesRoute = Page SpacesPage

pattern RegistrationRoute :: AppRoute
pattern RegistrationRoute = Page RegistrationPage

pattern EmailVerificationRoute :: AppRoute
pattern EmailVerificationRoute = Page EmailVerificationPage

pattern MfaEnrollmentRoute :: AppRoute
pattern MfaEnrollmentRoute = Page MfaEnrollmentPage

pattern LoginRoute :: AppRoute
pattern LoginRoute = Page LoginPage

pattern LogoutRoute :: AppRoute
pattern LogoutRoute = Page LogoutPage

pattern ProfileRoute :: AppRoute
pattern ProfileRoute = Page ProfilePage

pattern StatusApiRoute :: AppRoute
pattern StatusApiRoute = Api StatusApi

pattern SecondApiRoute :: AppRoute
pattern SecondApiRoute = Api SecondApi

pattern NotFoundRoute :: AppRoute
pattern NotFoundRoute = Page PageNotFound

pattern ApiNotFoundRoute :: AppRoute
pattern ApiNotFoundRoute = Api ApiNotFound

{-# COMPLETE
  HomeRoute,
  SecondRoute,
  SpacesRoute,
  RegistrationRoute,
  EmailVerificationRoute,
  MfaEnrollmentRoute,
  LoginRoute,
  LogoutRoute,
  ProfileRoute,
  NotFoundRoute,
  StatusApiRoute,
  SecondApiRoute,
  ApiNotFoundRoute
  #-}

instance Show AppRoute where
  show route =
    case route of
      HomeRoute -> "HomeRoute"
      SecondRoute -> "SecondRoute"
      SpacesRoute -> "SpacesRoute"
      RegistrationRoute -> "RegistrationRoute"
      EmailVerificationRoute -> "EmailVerificationRoute"
      MfaEnrollmentRoute -> "MfaEnrollmentRoute"
      LoginRoute -> "LoginRoute"
      LogoutRoute -> "LogoutRoute"
      ProfileRoute -> "ProfileRoute"
      StatusApiRoute -> "StatusApiRoute"
      SecondApiRoute -> "SecondApiRoute"
      NotFoundRoute -> "NotFoundRoute"
      ApiNotFoundRoute -> "ApiNotFoundRoute"

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
        HarchWeb.RouteRequest
          { HarchWeb.requestRoute = NotFoundRoute,
            HarchWeb.requestContext = requestContext
          }
    }

parseRoute :: AppRequestContext -> Text -> Maybe (HarchWeb.RouteRequest AppRoute AppRequestContext)
parseRoute requestContext target =
  either (const Nothing) Just (selectRoute requestContext target)

selectRoute ::
  AppRequestContext ->
  Text ->
  Either RouteSelectionError (HarchWeb.RouteRequest AppRoute AppRequestContext)
selectRoute requestContext target = do
  let path = routePath target
  (pathLocale, route) <- parseRoutePath path
  pure
    HarchWeb.RouteRequest
      { HarchWeb.requestRoute = route,
        HarchWeb.requestContext =
          (mergeRequestContext requestContext pathLocale)
            { requestQueryParameters = routeQuery target
            }
      }

renderRoutePath :: HarchWeb.RouteRequest AppRoute AppRequestContext -> Text
renderRoutePath routeRequest =
  applyRequestPathPrefix
    (requestPathPrefix requestContext)
    ( case HarchWeb.requestRoute routeRequest of
        Api apiRoute -> renderApiRoutePath apiRoute
        Page pageRoute ->
          let renderedPath =
                Text.concat
                  [ renderLocalePrefix
                      (requestLocale requestContext)
                      (requestLocaleIsExplicit requestContext),
                    routePageSuffix (pageRouteMetadata pageRoute)
                  ]
           in if Text.null renderedPath then "/" else renderedPath
    )
  where
    requestContext = HarchWeb.requestContext routeRequest

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

mergeRequestContext :: AppRequestContext -> Maybe AppLocale -> AppRequestContext
mergeRequestContext requestContext maybeLocale =
  requestContext
    { requestLocale =
        case maybeLocale of
          Just locale -> locale
          Nothing -> requestLocale requestContext,
      requestLocaleIsExplicit =
        case maybeLocale of
          Just _ -> True
          Nothing -> requestLocaleIsExplicit requestContext
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
                  >>= either (const Nothing) (firstCommaSeparatedValue . Text.strip) . TextEncoding.decodeUtf8'
              )
          else Text.empty,
      requestSessionId = requestSessionIdFromHeaders (Wai.requestHeaders request)
    }

parseRoutePath :: Text -> Either RouteSelectionError (Maybe AppLocale, AppRoute)
parseRoutePath path = parseRouteSegments path =<< tokenizeRoutePath path

tokenizeRoutePath :: Text -> Either RouteSelectionError [Text]
tokenizeRoutePath path
  | not (Text.isPrefixOf "/" path) = Left (UnsupportedPath path)
  | path /= "/" && Text.isSuffixOf "/" path = Left (UnsupportedPath path)
tokenizeRoutePath path = Right (drop 1 (Text.splitOn "/" path))

parseRouteSegments :: Text -> [Text] -> Either RouteSelectionError (Maybe AppLocale, AppRoute)
parseRouteSegments path segments =
  case segments of
    [segment]
      | Text.null segment -> Right (Nothing, HomeRoute)
    [segment]
      | segment == "api" -> Right (Nothing, ApiNotFoundRoute)
    [segment] -> parseSingleSegmentPath path segment
    [prefix, segment]
      | prefix == "api" -> parseApiPath segment
    [prefix, segment] -> parsePrefixedPath path prefix segment
    apiPrefix : _
      | apiPrefix == "api" -> Right (Nothing, ApiNotFoundRoute)
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

parsePrefixedPath ::
  Text ->
  Text ->
  Text ->
  Either RouteSelectionError (Maybe AppLocale, AppRoute)
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

parseApiPath :: Text -> Either RouteSelectionError (Maybe AppLocale, AppRoute)
parseApiPath segment
  | segment == "status" = Right (Nothing, StatusApiRoute)
  | segment == "second" = Right (Nothing, SecondApiRoute)
  | segment == "404" = Right (Nothing, ApiNotFoundRoute)
parseApiPath _ = Right (Nothing, ApiNotFoundRoute)

routeFromSegment :: Text -> Maybe AppRoute
routeFromSegment segment =
  lookup
    segment
    [ (configuredSegment, Page pageRoute)
    | pageRoute <- [minBound .. maxBound],
      Just configuredSegment <- [routePageSegment (pageRouteMetadata pageRoute)]
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

routeMetadata :: AppRoute -> RouteMetadata
routeMetadata route =
  case route of
    Page pageRoute -> pageRouteMetadata pageRoute
    Api _ -> RouteMetadata Nothing "/api/404" "Not Found" []

pageRouteMetadata :: PageRoute -> RouteMetadata
pageRouteMetadata pageRoute =
  case pageRoute of
    HomePage -> RouteMetadata Nothing Text.empty "Home" []
    SecondPage -> RouteMetadata (Just "second") "/second" "Second" ["second-page"]
    SpacesPage -> RouteMetadata (Just "spaces") "/spaces" "Spaces" []
    RegistrationPage -> RouteMetadata (Just "register") "/register" "Create account" []
    EmailVerificationPage -> RouteMetadata (Just "verify") "/verify" "Verify email" []
    MfaEnrollmentPage -> RouteMetadata (Just "mfa") "/mfa" "Set up authenticator" []
    LoginPage -> RouteMetadata (Just "login") "/login" "Sign in" []
    LogoutPage -> RouteMetadata (Just "logout") "/logout" "Sign out" []
    ProfilePage -> RouteMetadata (Just "profile") "/profile" "Profile" []
    PageNotFound -> RouteMetadata (Just "404") "/404" "Not Found" []

renderApiRoutePath :: ApiRoute -> Text
renderApiRoutePath apiRoute =
  case apiRoute of
    StatusApi -> "/api/status"
    SecondApi -> "/api/second"
    ApiNotFound -> "/api/404"

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
      normalizedPrefix = Text.dropWhileEnd (== '/') slashPrefixedPrefix
   in normalizedPrefix

requestSessionIdFromHeaders :: Http.RequestHeaders -> Maybe SessionId
requestSessionIdFromHeaders headers = do
  cookieHeader <- lookup "Cookie" headers
  cookieText <- either (const Nothing) Just (TextEncoding.decodeUtf8' cookieHeader)
  cookieValue <-
    lookup
      (sessionCookieNameText (sessionCookieName defaultSessionCookiePolicy))
      (map parseCookiePair (Text.splitOn ";" cookieText))
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
