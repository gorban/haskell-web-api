-- | The reference application's route-codec facade. Route identities and
-- generated page presentations live in the cycle-free 'WebApi.Route.Types'
-- leaf; endpoint admission stays in 'WebApi.Route.Endpoint'. The single route
-- codec remains here. Static legacy page presentation stays explicit until
-- those routes migrate in P5; discovered pages use their generated module
-- declarations for title, hooks, and navigation in both Site and standalone
-- document rendering.
-- Discovered paths begin with "/" and contain a validated page segment, so
-- generated metadata strips that prefix directly.
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
    PagePresentation (..),
    RouteMetadata (..),
    RouteSelectionError (..),
    accountAuthenticationProfileName,
    resourceAuthenticationProfileName,
    resourceReadScope,
    requiredOAuth2ScopeOrDie,
    appNavigationItems,
    appNavigationRoutes,
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
    routeNavigationDeclaration,
    selectRoute,
    routeCodec,
  )
where

import Data.Char (isAsciiLower)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb qualified
import HarchWeb.Site qualified as Site
import WebApi.Localization (AppMessage (CreateAccount, HomeNavigationLabel, Profile, SignIn, TodoPageHeading), localizedMessage)
import WebApi.Pages.Generated qualified as PagesGenerated
import WebApi.Pages.Route.Generated qualified as Generated
import WebApi.Route.Context
  ( accountAuthenticationProfileName,
    defaultRequestContext,
    requestContextFromWaiRequest,
    requiredOAuth2ScopeOrDie,
    resourceAuthenticationProfileName,
    resourceReadScope,
  )
import WebApi.Route.Endpoint (endpointMetadata, html)
import WebApi.Route.Types
  ( ApiRoute (..),
    AppAuthorization,
    AppLocale (..),
    AppRequestContext (..),
    AppRoute (..),
    PagePresentation (..),
    PageRoute (..),
    RequestAuthenticationTransport (..),
    RouteMetadata (..),
    pageRouteMetadata,
  )
import WebApi.Route.Url (renderRouteLocation, renderRoutePath, renderRouteUrl, requiredRouteUrl)

data RouteSelectionError
  = UnsupportedLocalePrefix Text
  | UnsupportedPath Text
  deriving (Eq, Show)

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
    -- The OpenAPI specification endpoint is locale-independent and exact:
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

-- | Candidate inventory passed to Site. Route declarations below decide which
-- candidates appear and provide their context-specific labels and positions.
appNavigationRoutes :: [AppRoute]
appNavigationRoutes =
  [HomeRoute, TodoRoute, RegistrationRoute, LoginRoute, ProfileRoute]
    <> map GeneratedPages Generated.allPageRoutes

-- | Resolve the application's current page navigation through Harch's shared
-- stable route-declaration resolver. Complete-document compatibility paths use
-- this same resolver as Site.
appNavigationItems :: AppRequestContext -> [HarchWeb.NavigationItem AppRoute]
appNavigationItems =
  Site.resolveRouteNavigationItems appNavigationRoutes routeNavigationDeclaration

-- | Page-route navigation declarations. Labels follow the active application
-- locale, and each route owns a nonnegative position. Other route families do
-- not participate in the page navigation.
routeNavigationDeclaration :: AppRoute -> AppRequestContext -> Maybe Site.RouteNavigation
routeNavigationDeclaration route requestContext =
  case route of
    Page HomePage -> declare 0 HomeNavigationLabel
    Page TodoPage -> declare 20 TodoPageHeading
    Page RegistrationPage -> declare 30 CreateAccount
    Page LoginPage -> declare 40 SignIn
    Page ProfilePage -> declare 50 Profile
    GeneratedPages generatedPage ->
      pagePresentationNavigation
        (PagesGenerated.pageRoutePresentation generatedPage)
        requestContext
    _ -> Nothing
  where
    declare order message =
      Just
        ( Site.RouteNavigation
            (Site.NavigationOrder order)
            (localizedMessage (requestLocale requestContext) message)
        )

routeMetadata :: AppRoute -> RouteMetadata
routeMetadata route =
  case route of
    Page pageRoute -> pageRouteMetadata pageRoute
    Api _ -> RouteMetadata Nothing "/api/404" "Not Found" []
    GeneratedPages generatedPage -> generatedPageMetadata generatedPage

generatedPageMetadata :: Generated.PageRoute -> RouteMetadata
generatedPageMetadata generatedPage =
  let presentation = PagesGenerated.pageRoutePresentation generatedPage
      pagePath = Generated.pageRoutePath generatedPage
      pageSegment = Just (Text.drop 1 pagePath)
   in RouteMetadata
        { routePageSegment = pageSegment,
          routePageSuffix = pagePath,
          routePageTitle = pagePresentationTitle presentation defaultRequestContext,
          routeEnhancementHooks = pagePresentationEnhancementHooks presentation
        }
