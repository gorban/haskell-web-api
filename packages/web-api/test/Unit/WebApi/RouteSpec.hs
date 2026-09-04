{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Monad (forM_)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb qualified
import HarchWeb.Session (mkSessionId)
import Network.Wai qualified as Wai
import TestCore.Wai (waiRequest)
import Unit.WebApi.TestSupport hiding (databaseConfig)
import WebApi.Config (AppConfig (..), defaultAppConfig)
import WebApi.Route (ApiRoute (..), AppLocale (..), AppRequestContext (..), AppRoute (..), PageRoute, RouteSelectionError (..), defaultRequestContext, renderRoutePath, renderRouteUrl)
import WebApi.Route qualified

-- | Tests enter the public route codec through the same raw-target decoder
-- as WAI, while preserving concise table-driven assertions below.
parseRoute :: AppRequestContext -> Text.Text -> Maybe (HarchWeb.RouteRequest AppRoute AppRequestContext)
parseRoute requestContext target = do
  location <- either (const Nothing) Just (routeLocationFromText target)
  case WebApi.Route.parseRoute requestContext location of
    HarchWeb.RouteParsed routeRequest -> Just routeRequest
    HarchWeb.RouteNotMatched -> Nothing
    HarchWeb.RouteMalformed _ -> Nothing

selectRoute :: AppRequestContext -> Text.Text -> Either RouteSelectionError (HarchWeb.RouteRequest AppRoute AppRequestContext)
selectRoute requestContext target =
  case routeLocationFromText target of
    Left _ -> Left (UnsupportedPath target)
    Right location -> WebApi.Route.selectRoute requestContext location

routeLocationFromText :: Text.Text -> Either HarchWeb.RouteDecodeError HarchWeb.RouteLocation
routeLocationFromText target =
  HarchWeb.decodeRouteLocation
    (HarchWeb.requestTarget (TextEncoding.encodeUtf8 path) (TextEncoding.encodeUtf8 query))
  where
    (path, query) = Text.breakOn "?" target

requiredRouteLocation :: Text.Text -> HarchWeb.RouteLocation
requiredRouteLocation target =
  case routeLocationFromText target of
    Left routeError -> error ("invalid test route target: " <> show routeError)
    Right location -> location

spec = do
  describe "closed route families" $ do
    it "keeps every page and API constructor enumerable, comparable, and inspectable" $ do
      let pageRoutes = [minBound .. maxBound] :: [PageRoute]
          apiRoutes = [minBound .. maxBound] :: [ApiRoute]
      pageRoutes
        `shouldBe` [ WebApi.Route.HomePage,
                     WebApi.Route.SecondPage,
                     WebApi.Route.SpacesPage,
                     WebApi.Route.RegistrationPage,
                     WebApi.Route.EmailVerificationPage,
                     WebApi.Route.MfaEnrollmentPage,
                     WebApi.Route.LoginPage,
                     WebApi.Route.LogoutPage,
                     WebApi.Route.ProfilePage,
                     WebApi.Route.LanguagePage,
                     WebApi.Route.HelpPage,
                     WebApi.Route.PageNotFound
                   ]
      apiRoutes `shouldBe` [StatusApi, SecondApi, ApiNotFound]
      minBound `shouldBe` WebApi.Route.HomePage
      maxBound `shouldBe` WebApi.Route.PageNotFound
      succ WebApi.Route.HomePage `shouldBe` WebApi.Route.SecondPage
      pred WebApi.Route.PageNotFound `shouldBe` WebApi.Route.HelpPage
      WebApi.Route.HomePage `shouldNotBe` WebApi.Route.SecondPage
      enumFrom WebApi.Route.HomePage `shouldBe` pageRoutes
      enumFromThen WebApi.Route.HomePage WebApi.Route.SecondPage `shouldBe` pageRoutes
      enumFromThenTo WebApi.Route.HomePage WebApi.Route.SecondPage WebApi.Route.PageNotFound `shouldBe` pageRoutes
      map show pageRoutes
        `shouldBe` [ "HomePage",
                     "SecondPage",
                     "SpacesPage",
                     "RegistrationPage",
                     "EmailVerificationPage",
                     "MfaEnrollmentPage",
                     "LoginPage",
                     "LogoutPage",
                     "ProfilePage",
                     "LanguagePage",
                     "HelpPage",
                     "PageNotFound"
                   ]
      showList pageRoutes ""
        `shouldBe` "[HomePage,SecondPage,SpacesPage,RegistrationPage,EmailVerificationPage,MfaEnrollmentPage,LoginPage,LogoutPage,ProfilePage,LanguagePage,HelpPage,PageNotFound]"
      minBound `shouldBe` StatusApi
      maxBound `shouldBe` ApiNotFound
      succ StatusApi `shouldBe` SecondApi
      pred ApiNotFound `shouldBe` SecondApi
      StatusApi `shouldNotBe` SecondApi
      enumFrom StatusApi `shouldBe` apiRoutes
      enumFromThen StatusApi SecondApi `shouldBe` apiRoutes
      enumFromThenTo StatusApi SecondApi ApiNotFound `shouldBe` apiRoutes
      map show apiRoutes `shouldBe` ["StatusApi", "SecondApi", "ApiNotFound"]
      showList apiRoutes "" `shouldBe` "[StatusApi,SecondApi,ApiNotFound]"
      show SecondApiRoute `shouldBe` "SecondApiRoute"
      show ApiNotFoundRoute `shouldBe` "ApiNotFoundRoute"
      show LanguageRoute `shouldBe` "LanguageRoute"
      show HelpRoute `shouldBe` "HelpRoute"
      Page WebApi.Route.HomePage `shouldNotBe` Api ApiNotFound

  describe "requestContextFromWaiRequest" $ do
    it "accepts only a valid MFA-enrollment cookie while preserving the supplied context" $ do
      let validSession = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_"
          validRequest =
            (waiRequest ["mfa"])
              { Wai.requestHeaders =
                  [("Cookie", TextEncoding.encodeUtf8 ("__Host-harch-mfa-enrollment=" <> validSession))]
              }
          malformedUtf8Request =
            (waiRequest ["mfa"])
              { Wai.requestHeaders = [("Cookie", "__Host-harch-mfa-enrollment=\255")]
              }
          malformedValueRequest =
            (waiRequest ["mfa"])
              { Wai.requestHeaders = [("Cookie", "__Host-harch-mfa-enrollment=short")]
              }
          expectedSession = mkSessionId validSession
          requestContextFromRequest = WebApi.Route.requestContextFromWaiRequest (requestPolicy defaultAppConfig)
      expectAll
        ( (requestMfaEnrollmentSessionId (requestContextFromRequest validRequest defaultRequestContext) `shouldBe` expectedSession)
            :| [ requestMfaEnrollmentSessionId (requestContextFromRequest malformedUtf8Request defaultRequestContext) `shouldBe` Nothing,
                 requestMfaEnrollmentSessionId (requestContextFromRequest malformedValueRequest defaultRequestContext) `shouldBe` Nothing,
                 requestMfaEnrollmentSessionId (requestContextFromRequest (waiRequest ["mfa"]) defaultRequestContext) `shouldBe` Nothing
               ]
        )

  describe "parseRoute" $ do
    it "maps bare and default-locale paths to the same home route" $ do
      fmap HarchWeb.requestRoute (parseRoute defaultRequestContext "/") `shouldBe` Just HomeRoute
      fmap HarchWeb.requestRoute (parseRoute defaultRequestContext "/en") `shouldBe` Just HomeRoute
      fmap HarchWeb.requestRoute (parseRoute defaultRequestContext "/404") `shouldBe` Just NotFoundRoute

    it "parses API paths directly into the API route family" $ do
      parseRoute defaultRequestContext "/api/status" `shouldBe` Just apiStatusRequest
      parseRoute defaultRequestContext "/api/status?fresh=1"
        `shouldBe` Just apiStatusRequest {HarchWeb.requestContext = defaultRequestContext {requestQueryParameters = [("fresh", "1")]}}
      parseRoute defaultRequestContext "/api/second" `shouldBe` Just apiSecondRequest
      parseRoute defaultRequestContext "/api" `shouldBe` Just apiNotFoundRequest
      parseRoute defaultRequestContext "/api/404" `shouldBe` Just apiNotFoundRequest
      parseRoute defaultRequestContext "/api/missing" `shouldBe` Just apiNotFoundRequest
      parseRoute defaultRequestContext "/api/status/extra" `shouldBe` Just apiNotFoundRequest

    it "parses the second page path" $
      parseRoute defaultRequestContext "/second" `shouldBe` Just secondRequest

    it "parses the app-home spaces path with its typed locale" $ do
      parseRoute defaultRequestContext "/spaces" `shouldBe` Just spacesRequest
      parseRoute defaultRequestContext "/es/spaces" `shouldBe` Just spanishSpacesRequest

    it "parses SSR account routes and preserves email-verification query values" $ do
      fmap HarchWeb.requestRoute (parseRoute defaultRequestContext "/register") `shouldBe` Just RegistrationRoute
      fmap HarchWeb.requestRoute (parseRoute defaultRequestContext "/mfa") `shouldBe` Just MfaEnrollmentRoute
      fmap HarchWeb.requestRoute (parseRoute defaultRequestContext "/login") `shouldBe` Just LoginRoute
      fmap HarchWeb.requestRoute (parseRoute defaultRequestContext "/logout") `shouldBe` Just LogoutRoute
      fmap HarchWeb.requestRoute (parseRoute defaultRequestContext "/profile") `shouldBe` Just ProfileRoute
      parseRoute defaultRequestContext "/verify?token=opaque-token"
        `shouldBe` Just
          HarchWeb.RouteRequest
            { HarchWeb.requestRoute = EmailVerificationRoute,
              HarchWeb.requestContext = defaultRequestContext {requestQueryParameters = [("token", "opaque-token")]}
            }
      parseRoute defaultRequestContext "/verify?=ignored&token=opaque-token&flag"
        `shouldBe` Just
          HarchWeb.RouteRequest
            { HarchWeb.requestRoute = EmailVerificationRoute,
              HarchWeb.requestContext = defaultRequestContext {requestQueryParameters = [("token", "opaque-token"), ("flag", "")]}
            }
      parseRoute defaultRequestContext "/verify?to%6Ben=opaque%2Dtoken%2Btwo&flag&token=%FF&to%FFken=ignored"
        `shouldBe` Nothing

    it "keeps supported path routes available when raw query strings are present" $
      parseRoute defaultRequestContext "/second?utm=demo"
        `shouldBe` Just secondRequest {HarchWeb.requestContext = defaultRequestContext {requestQueryParameters = [("utm", "demo")]}}

    it "lets explicit locale prefixes override the incoming request context" $ do
      parseRoute defaultRequestContext "/es/second" `shouldBe` Just spanishSecondRequest
      parseRoute spanishRequestContext "/en/second" `shouldBe` Just (HarchWeb.RouteRequest SecondRoute explicitEnglishRequestContext)

    it "returns an unsupported-route representation for unknown paths" $
      parseRoute defaultRequestContext "/missing" `shouldBe` Nothing

    it "fails unsupported locale prefixes with a precise route-selection error" $ do
      selectRoute defaultRequestContext "/de" `shouldBe` Left (UnsupportedLocalePrefix "de")
      selectRoute defaultRequestContext "/de/second" `shouldBe` Left (UnsupportedLocalePrefix "de")

    it "rejects paths that do not start with a slash" $
      selectRoute defaultRequestContext "second" `shouldBe` Left (UnsupportedPath "second")

    it "rejects unsupported multi-segment paths" $
      selectRoute defaultRequestContext "/es/second/extra" `shouldBe` Left (UnsupportedPath "/es/second/extra")

    it "rejects unsupported single-segment non-locale paths" $
      selectRoute defaultRequestContext "/missing" `shouldBe` Left (UnsupportedPath "/missing")

    it "rejects unsupported query-bearing paths after separating the route path" $
      selectRoute defaultRequestContext "/missing?utm=demo" `shouldBe` Left (UnsupportedPath "/missing")

    it "rejects locale-prefixed paths whose trailing segment is unsupported" $ do
      selectRoute defaultRequestContext "/es/missing" `shouldBe` Left (UnsupportedPath "/es/missing")
      selectRoute defaultRequestContext "/other/second" `shouldBe` Left (UnsupportedPath "/other/second")

    it "merges middleware-supplied and path-derived request inputs deterministically" $ do
      let middlewareContext =
            defaultRequestContext
              { requestLocale = English,
                requestCorrelationId = Just "req-123"
              }
      parseRoute middlewareContext "/es"
        `shouldBe` Just (HarchWeb.RouteRequest {HarchWeb.requestRoute = HomeRoute, HarchWeb.requestContext = middlewareContext {requestLocale = Spanish, requestLocaleIsExplicit = True}})

    it "rejects invalid trailing slashes while keeping the root path valid" $ do
      parseRoute defaultRequestContext "/" `shouldBe` Just homeRequest
      parseRoute defaultRequestContext "/second/" `shouldBe` Nothing
      selectRoute defaultRequestContext "/second/" `shouldBe` Left (UnsupportedPath "/second/")

  describe "renderRoutePath" $ do
    it "round-trips known routes through the parser" $ do
      parseRoute defaultRequestContext (renderRoutePath homeRequest) `shouldBe` Just homeRequest
      parseRoute defaultRequestContext (renderRoutePath secondRequest) `shouldBe` Just secondRequest
      parseRoute defaultRequestContext (renderRoutePath spanishSecondRequest) `shouldBe` Just spanishSecondRequest
      parseRoute defaultRequestContext (renderRoutePath spacesRequest) `shouldBe` Just spacesRequest
      parseRoute defaultRequestContext (renderRoutePath profileRequest) `shouldBe` Just profileRequest
      parseRoute defaultRequestContext (renderRoutePath spanishSpacesRequest) `shouldBe` Just spanishSpacesRequest
      parseRoute defaultRequestContext (renderRoutePath (HarchWeb.RouteRequest SecondRoute explicitEnglishRequestContext)) `shouldBe` Just (HarchWeb.RouteRequest SecondRoute explicitEnglishRequestContext)
      parseRoute defaultRequestContext (renderRoutePath apiStatusRequest) `shouldBe` Just apiStatusRequest
      parseRoute defaultRequestContext (renderRoutePath apiSecondRequest) `shouldBe` Just apiSecondRequest
      parseRoute defaultRequestContext (renderRoutePath apiNotFoundRequest) `shouldBe` Just apiNotFoundRequest

    it "renders default and explicit locale prefixes" $ do
      renderRoutePath homeRequest `shouldBe` "/"
      renderRoutePath spanishHomeRequest `shouldBe` "/es"
      renderRoutePath secondRequest `shouldBe` "/second"
      renderRoutePath spanishSecondRequest `shouldBe` "/es/second"
      renderRoutePath spacesRequest `shouldBe` "/spaces"
      renderRoutePath spanishSpacesRequest `shouldBe` "/es/spaces"
      renderRoutePath (HarchWeb.RouteRequest HomeRoute explicitEnglishRequestContext) `shouldBe` "/en"
      renderRoutePath (HarchWeb.RouteRequest SecondRoute explicitEnglishRequestContext) `shouldBe` "/en/second"
      renderRoutePath (HarchWeb.RouteRequest RegistrationRoute defaultRequestContext) `shouldBe` "/register"
      renderRoutePath (HarchWeb.RouteRequest EmailVerificationRoute spanishRequestContext) `shouldBe` "/es/verify"
      renderRoutePath (HarchWeb.RouteRequest LoginRoute defaultRequestContext) `shouldBe` "/login"
      renderRoutePath (HarchWeb.RouteRequest LogoutRoute spanishRequestContext) `shouldBe` "/es/logout"
      renderRoutePath (HarchWeb.RouteRequest ProfileRoute spanishRequestContext) `shouldBe` "/es/profile"
      renderRoutePath apiStatusRequest `shouldBe` "/api/status"
      renderRoutePath apiSecondRequest `shouldBe` "/api/second"
      renderRoutePath apiNotFoundRequest `shouldBe` "/api/404"
      renderRoutePath notFoundRequest `shouldBe` "/404"
      HarchWeb.safeUrlText (renderRouteUrl spanishSpacesRequest) `shouldBe` "/es/spaces"

    it "prepends the forwarded request path prefix to page and API routes" $ do
      renderRoutePath prefixedHomeRequest `shouldBe` "/app"
      renderRoutePath prefixedSpanishSecondRequest `shouldBe` "/app/es/second"
      renderRoutePath prefixedApiStatusRequest `shouldBe` "/app/api/status"

  describe "endpointMetadata" $ do
    it "gives every closed route a stable public endpoint identity" $ do
      let endpointDeclarationFields endpointMetadataValue =
            ( HarchWeb.endpointNameText (HarchWeb.endpointName endpointMetadataValue),
              HarchWeb.routeTemplateText (HarchWeb.endpointRouteTemplate endpointMetadataValue),
              HarchWeb.endpointProtocol endpointMetadataValue,
              HarchWeb.endpointAccess endpointMetadataValue
            )
          expectedMetadata =
            [ (HomeRoute, "web.home", "/{locale}", HarchWeb.HtmlEndpoint),
              (SecondRoute, "web.second", "/{locale}/second", HarchWeb.HtmlEndpoint),
              (SpacesRoute, "web.spaces", "/{locale}/spaces", HarchWeb.HtmlEndpoint),
              (RegistrationRoute, "account.registration", "/{locale}/register", HarchWeb.HtmlEndpoint),
              (EmailVerificationRoute, "account.email-verification", "/{locale}/verify", HarchWeb.HtmlEndpoint),
              (MfaEnrollmentRoute, "account.mfa-enrollment", "/{locale}/mfa", HarchWeb.HtmlEndpoint),
              (LoginRoute, "account.login", "/{locale}/login", HarchWeb.HtmlEndpoint),
              (LogoutRoute, "account.logout", "/{locale}/logout", HarchWeb.HtmlEndpoint),
              (ProfileRoute, "account.profile", "/{locale}/profile", HarchWeb.HtmlEndpoint),
              (LanguageRoute, "web.language", "/{locale}/language", HarchWeb.HtmlEndpoint),
              (HelpRoute, "web.help", "/{locale}/help", HarchWeb.HtmlEndpoint),
              (NotFoundRoute, "web.not-found", "/{locale}/404", HarchWeb.HtmlEndpoint),
              (StatusApiRoute, "api.status", "/api/status", HarchWeb.ApiEndpoint),
              (SecondApiRoute, "api.second", "/api/second", HarchWeb.ApiEndpoint),
              (ApiNotFoundRoute, "api.not-found", "/api/404", HarchWeb.ApiEndpoint)
            ]
          expectedAccess route
            | route `elem` [LogoutRoute, ProfileRoute] = HarchWeb.RequireAuthenticated
            | otherwise = HarchWeb.AllowUnauthenticated
      forM_ expectedMetadata $ \(route, expectedName, template, protocol) ->
        endpointDeclarationFields (WebApi.Route.endpointMetadata route)
          `shouldBe` (expectedName, template, protocol, expectedAccess route)

  describe "matchRoute" $ do
    it "remains available separately from HarchWeb.matchRoute" $
      WebApi.Route.matchRoute WebApi.Route.defaultRequestContext (requiredRouteLocation "/second")
        `shouldBe` HarchWeb.matchRoute WebApi.Route.routeCodec WebApi.Route.defaultRequestContext (requiredRouteLocation "/second")

    -- Tabled per docs/design-guidance.md's CN decision record: one act
    -- ('pureRouteMatcher'), one comparison, differing only in the path and
    -- expected route request. The API-path cases were previously bundled
    -- three-per-'it'; each now reports individually.
    [ ("matches the home path", "/", homeRequest),
      ("matches the second page path", "/second", secondRequest),
      ("matches the app-home spaces path", "/spaces", spacesRequest),
      ("matches locale-prefixed paths with the merged request context", "/es", spanishHomeRequest),
      ("matches an API status path into the API route family", "/api/status", apiStatusRequest),
      ("matches an API second path into the API route family", "/api/second", apiSecondRequest),
      ("matches an unknown API path into the API route family's not-found outcome", "/api/missing", apiNotFoundRequest),
      ("falls back to the stable not-found route for unknown paths", "/missing", notFoundRequest)
      ]
      `forM_` \(label, path, expected) ->
        it label $
          pureRouteMatcher path `shouldBe` expected
