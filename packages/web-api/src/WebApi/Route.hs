{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module WebApi.Route
  ( AppAuthorization,
    AppLocale (..),
    AppRequestContext (..),
    RequestAuthenticationTransport (..),
    AppRoute
      ( Page,
        Api,
        GeneratedPages,
        HomeRoute,
        SecondRoute,
        TodoRoute,
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
        MeApiRoute,
        TokenApiRoute,
        DocsOpenApiSpecRoute,
        NotFoundRoute,
        ApiNotFoundRoute,
        DocsSwaggerRoute,
        ShowcaseRoute,
        ShowcaseAlternateRoute
      ),
    ApiRoute (..),
    PageRoute (..),
    RouteMetadata (..),
    RouteSelectionError (..),
    accountAuthenticationProfileName,
    resourceAuthenticationProfileName,
    resourceReadScope,
    requiredOAuth2ScopeOrDie,
    defaultRequestContext,
    endpointMetadata,
    html,
    matchRoute,
    parseRoute,
    appRouteMethods,
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
import HarchWeb qualified
import HarchWeb.EndpointSecurity
  ( AccessRequirement (AllowUnauthenticated, RequireAuthenticated),
    EndpointMetadata,
    EndpointProtocol (ApiEndpoint, HtmlEndpoint),
    mkEndpointMetadata,
    requiredEndpointNameOrDie,
    requiredRouteTemplateOrDie,
  )
import WebApi.Pages.Route.Generated qualified as Generated
import WebApi.Route.Context

data RouteSelectionError
  = UnsupportedLocalePrefix Text
  | UnsupportedPath Text
  deriving (Eq, Show)

data PageRoute
  = HomePage
  | SecondPage
  | TodoPage
  | RegistrationPage
  | EmailVerificationPage
  | MfaEnrollmentPage
  | LoginPage
  | LogoutPage
  | ProfilePage
  | LanguagePage
  | HelpPage
  | DocsSwaggerPage
  | PageNotFound
  deriving (Bounded, Enum, Eq, Show)

data ApiRoute
  = StatusApi
  | SecondApi
  | MeApi
  | TokenApi
  | -- | AHI-4E: the prepared OpenAPI document itself, served as an ordinary
    -- unauthenticated typed GET endpoint at @\/docs\/openapi.json@. It is a
    -- protocol route like every other 'ApiRoute' (a complete SSR page and
    -- asset routes remain the later Swagger-UI slice), but its path lives
    -- outside the @\/api@ prefix the other constructors render — see
    -- 'apiRouteSegments' and 'parseRouteSegments'.
    DocsOpenApiSpec
  | ApiNotFound
  deriving (Bounded, Enum, Eq, Show)

data AppRoute
  = Page PageRoute
  | Api ApiRoute
  | GeneratedPages Generated.PageRoute
  deriving (Eq)

pattern HomeRoute :: AppRoute
pattern HomeRoute = Page HomePage

-- | The generated page family mounts beside the hand-owned routes: the
-- generator derives these from the 'WebApi.Pages' file names.
pattern DocsSwaggerRoute :: AppRoute
pattern DocsSwaggerRoute = Page DocsSwaggerPage

pattern ShowcaseRoute :: AppRoute
pattern ShowcaseRoute = GeneratedPages Generated.ShowcasePage

pattern ShowcaseAlternateRoute :: AppRoute
pattern ShowcaseAlternateRoute = GeneratedPages Generated.ShowcaseAlternatePage

pattern SecondRoute :: AppRoute
pattern SecondRoute = Page SecondPage

pattern TodoRoute :: AppRoute
pattern TodoRoute = Page TodoPage

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

pattern MeApiRoute :: AppRoute
pattern MeApiRoute = Api MeApi

pattern TokenApiRoute :: AppRoute
pattern TokenApiRoute = Api TokenApi

pattern DocsOpenApiSpecRoute :: AppRoute
pattern DocsOpenApiSpecRoute = Api DocsOpenApiSpec

pattern NotFoundRoute :: AppRoute
pattern NotFoundRoute = Page PageNotFound

pattern ApiNotFoundRoute :: AppRoute
pattern ApiNotFoundRoute = Api ApiNotFound

-- Completeness is declared over both views: the constructor triple is the
-- exact covering set (GHC misattributes pattern-synonym coverage when only
-- synonym names are listed), while the synonym set keeps the hand-authored
-- name tables complete on their own terms.
{-# COMPLETE Page, Api, GeneratedPages #-}

{-# COMPLETE HomeRoute, SecondRoute, TodoRoute, RegistrationRoute, EmailVerificationRoute, MfaEnrollmentRoute, LoginRoute, LogoutRoute, ProfileRoute, LanguageRoute, HelpRoute, NotFoundRoute, StatusApiRoute, SecondApiRoute, MeApiRoute, TokenApiRoute, DocsOpenApiSpecRoute, ApiNotFoundRoute, GeneratedPages #-}

instance Show AppRoute where
  show route =
    case route of
      HomeRoute -> "HomeRoute"
      SecondRoute -> "SecondRoute"
      TodoRoute -> "TodoRoute"
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
      MeApiRoute -> "MeApiRoute"
      TokenApiRoute -> "TokenApiRoute"
      DocsOpenApiSpecRoute -> "DocsOpenApiSpecRoute"
      NotFoundRoute -> "NotFoundRoute"
      ApiNotFoundRoute -> "ApiNotFoundRoute"
      DocsSwaggerRoute -> "DocsSwaggerRoute"
      GeneratedPages generatedPage -> "GeneratedPages " <> show generatedPage

data RouteMetadata = RouteMetadata
  { routePageSegment :: Maybe Text,
    routePageSuffix :: Text,
    routePageTitle :: Text,
    routeEnhancementHooks :: [Text]
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
      HarchWeb.routeMethods = HarchWeb.routeMethodPolicy . appRouteMethods . HarchWeb.requestRoute
    }

appRouteMethods :: AppRoute -> [HarchWeb.RouteMethod]
appRouteMethods route =
  case route of
    Page PageNotFound -> []
    Page _ -> [HarchWeb.RouteGet]
    Api ApiNotFound -> []
    Api TokenApi -> [HarchWeb.RoutePost]
    Api _ -> [HarchWeb.RouteGet]
    GeneratedPages _ -> [HarchWeb.RouteGet]

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
        GeneratedPages generatedPage ->
          localeSegments <> map HarchWeb.requiredPathSegment (filter (not . Text.null) (Text.splitOn "/" (Text.dropWhile (== '/') (Generated.pageRoutePath generatedPage))))
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
    MeApi -> pathSegment "api" NonEmpty.:| [pathSegment "me"]
    TokenApi -> pathSegment "api" NonEmpty.:| [pathSegment "oauth", pathSegment "token"]
    -- The documentation specification deliberately lives at the task file's
    -- default @\/docs@ path rather than under @\/api@: it is a support
    -- surface, not part of the documented API it describes.
    DocsOpenApiSpec -> pathSegment "docs" NonEmpty.:| [pathSegment "openapi.json"]
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
    -- The AHI-4E specification endpoint is locale-independent and exact:
    -- only @\/docs\/openapi.json@ matches, while any other @\/docs@ path
    -- keeps falling through to the ordinary unsupported-path rejection
    -- below rather than growing a second docs-specific 404 family.
    ["docs", "openapi.json"] -> Right (Nothing, DocsOpenApiSpecRoute)
    [prefix, segment] -> parsePrefixedPath path prefix segment
    ["api", "oauth", "token"] -> Right (Nothing, TokenApiRoute)
    apiPrefix : _
      | apiPrefix == "api" -> Right (Nothing, ApiNotFoundRoute)
    _ -> Left (UnsupportedPath path)

parseSingleSegmentPath :: Text -> Text -> Either RouteSelectionError (Maybe AppLocale, AppRoute)
parseSingleSegmentPath fullPath segment =
  case routeFromSegment segment of
    Just route -> Right (Nothing, route)
    Nothing ->
      case Generated.parsePageRoute ("/" <> segment) of
        Just generatedPage -> Right (Nothing, GeneratedPages generatedPage)
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
        Nothing ->
          case Generated.parsePageRoute ("/" <> segment) of
            Just generatedPage -> Right (Just locale, GeneratedPages generatedPage)
            Nothing -> Left (UnsupportedPath fullPath)
    Nothing ->
      if looksLikeLocalePrefix prefix
        then Left (UnsupportedLocalePrefix prefix)
        else Left (UnsupportedPath fullPath)

parseApiPath :: Text -> Either RouteSelectionError (Maybe AppLocale, AppRoute)
parseApiPath segment
  | segment == "status" = Right (Nothing, StatusApiRoute)
  | segment == "second" = Right (Nothing, SecondApiRoute)
  | segment == "me" = Right (Nothing, MeApiRoute)
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
    -- Generated pages keep their route presentation in this same table; their
    -- page modules read these values for titles and hooks while owning their
    -- model, body, and scoped styles.
    GeneratedPages Generated.ShowcasePage ->
      RouteMetadata (Just "showcase") "" "Showcase" ["web-api-showcase"]
    GeneratedPages Generated.ShowcaseAlternatePage ->
      RouteMetadata (Just "showcase-alternate") "" "Showcase alternate" ["web-api-showcase-alternate"]

-- | Stable, application-authored endpoint identities for the existing route
-- table. AHI-4C's configured root guard establishes a principal before the
-- protected profile/logout handlers run; public routes remain explicit.
endpointMetadata :: AppRoute -> EndpointMetadata AppAuthorization
endpointMetadata route =
  case route of
    HomeRoute -> html "web.home" "/{locale}"
    SecondRoute -> html "web.second" "/{locale}/second"
    TodoRoute -> html "web.todo" "/{locale}/todo"
    RegistrationRoute -> html "account.registration" "/{locale}/register"
    EmailVerificationRoute -> html "account.email-verification" "/{locale}/verify"
    MfaEnrollmentRoute -> html "account.mfa-enrollment" "/{locale}/mfa"
    LoginRoute -> html "account.login" "/{locale}/login"
    LogoutRoute -> protectedHtml "account.logout" "/{locale}/logout"
    ProfileRoute -> protectedHtml "account.profile" "/{locale}/profile"
    LanguageRoute -> html "web.language" "/{locale}/language"
    HelpRoute -> html "web.help" "/{locale}/help"
    DocsSwaggerRoute -> html "web.docs" "/{locale}/docs"
    NotFoundRoute -> html "web.not-found" "/{locale}/404"
    StatusApiRoute -> api "api.status" "/api/status"
    -- AHI-4D slice 5: an account cookie/bearer session is authorized
    -- unconditionally; an API-client bearer token must carry the
    -- 'resourceReadScope' scope (directly, or via its current durable
    -- allowance). See 'WebApi.ResourceAuthentication' and the AHI-4D
    -- decision record in @docs/design-guidance.md@.
    SecondApiRoute ->
      HarchWeb.withAuthenticationProfile
        resourceAuthenticationProfileName
        (declaredMetadata ApiEndpoint (HarchWeb.RequireAuthorized (HarchWeb.RequireAnyScope (resourceReadScope NonEmpty.:| []))) "api.second" "/api/second")
    -- Reuses the account profile's existing 'RequireAuthenticated' guard
    -- rather than a new authorization payload: an API-client bearer JWT has
    -- no session ID ('jti') claim, so it already fails this profile's claims
    -- parse and can never reach this handler merely by presenting a
    -- similarly named scope. See the AHI-4D decision record in
    -- @docs/design-guidance.md@.
    MeApiRoute -> protectedApi "api.me" "/api/me"
    -- The token endpoint authenticates its OAuth client itself (HTTP Basic
    -- client-credentials, verified against the durable API-client store), so
    -- it declares 'AllowUnauthenticated' like every other API route here: no
    -- account session or bearer JWT establishes the caller before this
    -- handler runs. See the AHI-4D decision record in
    -- @docs/design-guidance.md@.
    TokenApiRoute -> api "api.oauth-token" "/api/oauth/token"
    -- AHI-4E: the specification is an ordinary unauthenticated typed API
    -- endpoint; its security choice stays this application's, exactly like
    -- every other route's metadata here.
    DocsOpenApiSpecRoute -> api "api.openapi-spec" "/docs/openapi.json"
    ApiNotFoundRoute -> api "api.not-found" "/api/404"
    -- Generated pages keep their route presentation here (the single source
    -- every web-api route uses); their page modules read these values for
    -- titles and hooks while owning their model, body, and scoped styles.
    GeneratedPages Generated.ShowcasePage -> html "web.showcase" "/{locale}/showcase"
    GeneratedPages Generated.ShowcaseAlternatePage -> html "web.showcase-alternate" "/{locale}/showcase-alternate"

-- Per docs/design-guidance.md's never-mask-a-gate-finding rule: the @$!@ on
-- the name and template below is a confirmed, reproducible fix for the
-- documented HPC pattern where a binding used as a direct argument to an
-- instrumented call stays unticked despite real execution (every table row's
-- literals are validated end to end by the route-table tests).
{-# ANN declaredMetadata ("HLint: ignore Redundant $!" :: String) #-}
declaredMetadata :: EndpointProtocol -> AccessRequirement AppAuthorization -> Text -> Text -> EndpointMetadata AppAuthorization
declaredMetadata protocol accessRequirement name template =
  mkEndpointMetadata
    (requiredEndpointNameOrDie $! name)
    (requiredRouteTemplateOrDie $! template)
    protocol
    accessRequirement

-- | Endpoint-metadata builders shared by the route table and the generated
-- page family ('WebApi.Pages'): plain declarations for public surfaces and
-- profile-protected variants for authenticated ones.
html :: Text -> Text -> EndpointMetadata AppAuthorization
html = declaredMetadata HtmlEndpoint AllowUnauthenticated

protectedHtml :: Text -> Text -> EndpointMetadata AppAuthorization
protectedHtml name template =
  HarchWeb.withAuthenticationProfile
    accountAuthenticationProfileName
    (declaredMetadata HtmlEndpoint RequireAuthenticated name template)

api :: Text -> Text -> EndpointMetadata AppAuthorization
api = declaredMetadata ApiEndpoint AllowUnauthenticated

protectedApi :: Text -> Text -> EndpointMetadata AppAuthorization
protectedApi name template =
  HarchWeb.withAuthenticationProfile
    accountAuthenticationProfileName
    (declaredMetadata ApiEndpoint RequireAuthenticated name template)

pageRouteMetadata :: PageRoute -> RouteMetadata
pageRouteMetadata pageRoute =
  case pageRoute of
    HomePage -> RouteMetadata Nothing Text.empty "Home" []
    SecondPage -> RouteMetadata (Just "second") "/second" "Second" ["second-page"]
    TodoPage -> RouteMetadata (Just "todo") "/todo" "TODO" []
    RegistrationPage -> RouteMetadata (Just "register") "/register" "Create account" []
    EmailVerificationPage -> RouteMetadata (Just "verify") "/verify" "Verify email" []
    MfaEnrollmentPage -> RouteMetadata (Just "mfa") "/mfa" "Set up authenticator" []
    LoginPage -> RouteMetadata (Just "login") "/login" "Sign in" []
    LogoutPage -> RouteMetadata (Just "logout") "/logout" "Sign out" []
    ProfilePage -> RouteMetadata (Just "profile") "/profile" "Profile" []
    LanguagePage -> RouteMetadata (Just "language") "/language" "Language" []
    HelpPage -> RouteMetadata (Just "help") "/help" "Help and support" []
    DocsSwaggerPage -> RouteMetadata (Just "docs") "/docs" "Documentation" ["web-api-docs"]
    PageNotFound -> RouteMetadata (Just "404") "/404" "Not Found" []
