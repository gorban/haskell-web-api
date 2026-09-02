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
        LanguageRoute,
        HelpRoute,
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
    endpointMetadata,
    matchRoute,
    parseRoute,
    renderRoutePath,
    renderRouteUrl,
    requiredRouteUrl,
    requestContextFromWaiRequest,
    routeMetadata,
    selectRoute,
    routeCodec,
  )
where

import Data.Char (isAsciiLower)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb qualified
import HarchWeb.EndpointSecurity
  ( AccessRequirement (AllowUnauthenticated),
    EndpointMetadata,
    EndpointProtocol (ApiEndpoint, HtmlEndpoint),
    mkEndpointMetadata,
    requiredEndpointNameOrDie,
    requiredRouteTemplateOrDie,
  )
import HarchWeb.Session
  ( SessionId,
    defaultSessionCookiePolicy,
    mkSessionId,
    sessionCookieName,
    sessionCookieNameText,
  )
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import WebApi.Session (mfaEnrollmentSessionCookiePolicy)

data AppLocale
  = English
  | Spanish
  deriving (Eq, Show)

data AppRequestContext = AppRequestContext
  { requestLocale :: AppLocale,
    requestLocaleIsExplicit :: Bool,
    requestCorrelationId :: Maybe Text,
    requestClientAddress :: HarchWeb.ClientAddress,
    requestPathPrefix :: HarchWeb.PathPrefix,
    requestQueryParameters :: [(Text, Text)],
    requestSessionId :: Maybe SessionId,
    requestMfaEnrollmentSessionId :: Maybe SessionId
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
  | LanguagePage
  | HelpPage
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

pattern LanguageRoute :: AppRoute
pattern LanguageRoute = Page LanguagePage

pattern HelpRoute :: AppRoute
pattern HelpRoute = Page HelpPage

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
  LanguageRoute,
  HelpRoute,
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
      LanguageRoute -> "LanguageRoute"
      HelpRoute -> "HelpRoute"
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
      requestClientAddress = HarchWeb.defaultClientAddress,
      requestPathPrefix = HarchWeb.emptyPathPrefix,
      requestQueryParameters = [],
      requestSessionId = Nothing,
      requestMfaEnrollmentSessionId = Nothing
    }

routeCodec :: HarchWeb.RouteCodec AppRoute AppRequestContext
routeCodec =
  HarchWeb.RouteCodec
    { HarchWeb.parseRoute = parseRoute,
      HarchWeb.renderRoute = renderRouteLocation,
      HarchWeb.notFoundRequest = \requestContext ->
        HarchWeb.RouteRequest
          { HarchWeb.requestRoute = NotFoundRoute,
            HarchWeb.requestContext = requestContext
          },
      HarchWeb.routeMethods = HarchWeb.routeMethodPolicy . appRouteMethods
    }

appRouteMethods :: AppRoute -> [HarchWeb.RouteMethod]
appRouteMethods route =
  case route of
    Page PageNotFound -> []
    Page _ -> [HarchWeb.RouteGet]
    Api ApiNotFound -> []
    Api _ -> [HarchWeb.RouteGet]

parseRoute :: AppRequestContext -> HarchWeb.RouteLocation -> HarchWeb.RouteParseResult AppRoute AppRequestContext
parseRoute requestContext location =
  case selectRoute requestContext location of
    Left _ -> HarchWeb.RouteNotMatched
    Right routeRequest -> HarchWeb.RouteParsed routeRequest

selectRoute ::
  AppRequestContext ->
  HarchWeb.RouteLocation ->
  Either RouteSelectionError (HarchWeb.RouteRequest AppRoute AppRequestContext)
selectRoute requestContext location = do
  let segments = map HarchWeb.pathSegmentText (HarchWeb.routePathSegments location)
      path = HarchWeb.safeUrlText (HarchWeb.encodeRouteLocation (location {HarchWeb.routeQueryFields = []}))
  (pathLocale, route) <- parseRouteSegments path segments
  pure
    HarchWeb.RouteRequest
      { HarchWeb.requestRoute = route,
        HarchWeb.requestContext =
          (mergeRequestContext requestContext pathLocale)
            { requestQueryParameters = queryParameters (HarchWeb.routeQueryFields location)
            }
      }
  where
    queryFieldText (name, value) = (HarchWeb.queryNameText name, HarchWeb.queryValueText value)
    -- The framework retains every syntactically valid query field.  This
    -- application has always ignored unnamed fields, so keep that local
    -- semantic policy at the typed application boundary rather than teaching
    -- the shared request-target decoder to discard input facts.
    queryParameters = filter (not . Text.null . fst) . map queryFieldText

renderRoutePath :: HarchWeb.RouteRequest AppRoute AppRequestContext -> Text
renderRoutePath = HarchWeb.safeUrlText . HarchWeb.encodeRouteLocation . renderRouteLocation

renderRouteLocation :: HarchWeb.RouteRequest AppRoute AppRequestContext -> HarchWeb.RouteLocation
renderRouteLocation routeRequest =
  HarchWeb.prefixRouteLocation
    (requestPathPrefix requestContext)
    HarchWeb.RouteLocation
      { HarchWeb.routePathSegments = renderedSegments,
        HarchWeb.routeQueryFields = []
      }
  where
    requestContext = HarchWeb.requestContext routeRequest
    renderedSegments =
      case HarchWeb.requestRoute routeRequest of
        Api apiRoute -> NonEmpty.toList (apiRouteSegments apiRoute)
        Page pageRoute -> localeSegments <> maybe [] (pure . HarchWeb.requiredPathSegment) (routePageSegment (pageRouteMetadata pageRoute))
    localeSegments =
      case (requestLocale requestContext, requestLocaleIsExplicit requestContext) of
        (English, False) -> []
        (English, True) -> [HarchWeb.requiredPathSegment "en"]
        (Spanish, _) -> [HarchWeb.requiredPathSegment "es"]

apiRouteSegments :: ApiRoute -> NonEmpty.NonEmpty HarchWeb.PathSegment
apiRouteSegments apiRoute =
  case apiRoute of
    StatusApi -> pathSegment "api" NonEmpty.:| [pathSegment "status"]
    SecondApi -> pathSegment "api" NonEmpty.:| [pathSegment "second"]
    ApiNotFound -> pathSegment "api" NonEmpty.:| [pathSegment "404"]
  where
    pathSegment = HarchWeb.requiredPathSegment

-- | Turn the closed application's typed route rendering into a safe link
-- target. A rejection is an application route-table defect, not a request
-- outcome; 'requiredRouteUrl' keeps that invariant directly testable.
renderRouteUrl :: HarchWeb.RouteRequest AppRoute AppRequestContext -> HarchWeb.SafeUrl
renderRouteUrl = HarchWeb.encodeRouteLocation . renderRouteLocation

requiredRouteUrl :: Text -> HarchWeb.SafeUrl
requiredRouteUrl renderedPath =
  HarchWeb.requiredSafeUrlOrDie
    ("WebApi.Route rendered an unsafe URL: " <> renderedPath)
    (HarchWeb.mkSafeUrl renderedPath)

matchRoute :: AppRequestContext -> HarchWeb.RouteLocation -> HarchWeb.RouteParseResult AppRoute AppRequestContext
matchRoute = HarchWeb.matchRoute routeCodec

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

requestContextFromWaiRequest :: HarchWeb.RequestPolicyConfig -> Wai.Request -> AppRequestContext -> AppRequestContext
requestContextFromWaiRequest requestPolicyConfig request requestContext =
  requestContext
    { requestPathPrefix =
        HarchWeb.requestPathPrefix requestPolicyConfig request,
      requestClientAddress = HarchWeb.requestClientAddress requestPolicyConfig request,
      requestSessionId = sessionIdFromCookieHeaders (sessionCookieNameText (sessionCookieName defaultSessionCookiePolicy)) (Wai.requestHeaders request),
      requestMfaEnrollmentSessionId = sessionIdFromCookieHeaders (sessionCookieNameText (sessionCookieName mfaEnrollmentSessionCookiePolicy)) (Wai.requestHeaders request)
    }

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

routeMetadata :: AppRoute -> RouteMetadata
routeMetadata route =
  case route of
    Page pageRoute -> pageRouteMetadata pageRoute
    Api _ -> RouteMetadata Nothing "/api/404" "Not Found" []

-- | Stable, application-authored endpoint identities for the existing route
-- table. The reference app deliberately chooses the explicitly public root
-- configuration until AHI-4C supplies its account-backed authentication
-- guard, so that choice is visible in every route declaration.
endpointMetadata :: AppRoute -> EndpointMetadata ()
endpointMetadata route =
  case route of
    HomeRoute -> html "web.home" "/{locale}"
    SecondRoute -> html "web.second" "/{locale}/second"
    SpacesRoute -> html "web.spaces" "/{locale}/spaces"
    RegistrationRoute -> html "account.registration" "/{locale}/register"
    EmailVerificationRoute -> html "account.email-verification" "/{locale}/verify"
    MfaEnrollmentRoute -> html "account.mfa-enrollment" "/{locale}/mfa"
    LoginRoute -> html "account.login" "/{locale}/login"
    LogoutRoute -> html "account.logout" "/{locale}/logout"
    ProfileRoute -> html "account.profile" "/{locale}/profile"
    LanguageRoute -> html "web.language" "/{locale}/language"
    HelpRoute -> html "web.help" "/{locale}/help"
    NotFoundRoute -> html "web.not-found" "/{locale}/404"
    StatusApiRoute -> api "api.status" "/api/status"
    SecondApiRoute -> api "api.second" "/api/second"
    ApiNotFoundRoute -> api "api.not-found" "/api/404"
  where
    html = declaredMetadata HtmlEndpoint
    api = declaredMetadata ApiEndpoint

declaredMetadata :: EndpointProtocol -> Text -> Text -> EndpointMetadata ()
declaredMetadata protocol name template =
  mkEndpointMetadata
    (requiredEndpointNameOrDie name)
    (requiredRouteTemplateOrDie template)
    protocol
    AllowUnauthenticated

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
    LanguagePage -> RouteMetadata (Just "language") "/language" "Language" []
    HelpPage -> RouteMetadata (Just "help") "/help" "Help and support" []
    PageNotFound -> RouteMetadata (Just "404") "/404" "Not Found" []

sessionIdFromCookieHeaders :: Text -> Http.RequestHeaders -> Maybe SessionId
sessionIdFromCookieHeaders cookieName headers = do
  cookieHeader <- lookup "Cookie" headers
  cookieText <- either (const Nothing) Just (TextEncoding.decodeUtf8' cookieHeader)
  cookieValue <- lookup cookieName (map parseCookiePair (Text.splitOn ";" cookieText))
  mkSessionId cookieValue
  where
    parseCookiePair value =
      let (name, rawValue) = Text.breakOn "=" (Text.strip value)
       in (name, Text.drop 1 rawValue)
