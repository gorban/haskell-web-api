{-# LANGUAGE PatternSynonyms #-}

-- | Cycle-free application route identities and generated page declarations.
-- This leaf imports only the generated route algebra and request-context
-- types, never a page implementation. Discovered page modules can therefore
-- own their presentation, and the generated aggregator can collect it without
-- importing the higher-level route codec back into those pages. Admission
-- policy remains on 'HarchWeb.Site.RouteDefinition' through the separate
-- 'WebApi.Route.Endpoint' declarations.
module WebApi.Route.Types
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
    pageRouteMetadata,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Site (RouteNavigation)
import WebApi.Pages.Route.Generated qualified as Generated
import WebApi.Route.Context
  ( AppAuthorization,
    AppLocale (..),
    AppRequestContext (..),
    RequestAuthenticationTransport (..),
  )

data PageRoute
  = HomePage
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
  | -- | OpenAPI documentation and Swagger UI: the prepared OpenAPI document itself, served as an ordinary
    -- unauthenticated typed GET endpoint at @\/docs\/openapi.json@. It is a
    -- protocol route like every other 'ApiRoute', but its path lives outside
    -- the @\/api@ prefix the other constructors render.
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

pattern DocsSwaggerRoute :: AppRoute
pattern DocsSwaggerRoute = Page DocsSwaggerPage

pattern ShowcaseRoute :: AppRoute
pattern ShowcaseRoute = GeneratedPages Generated.ShowcasePage

pattern ShowcaseAlternateRoute :: AppRoute
pattern ShowcaseAlternateRoute = GeneratedPages Generated.ShowcaseAlternatePage

pattern SecondRoute :: AppRoute
pattern SecondRoute = GeneratedPages Generated.SecondPage

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

-- | User-facing properties a discovered page owns beside its body and styles.
-- Title and navigation are pure functions of the matched request context, so
-- Site dispatch and the standalone document renderer use identical localized
-- values. Route paths still come from page-file discovery; endpoint admission
-- metadata remains a separate application-owned declaration.
data PagePresentation = PagePresentation
  { pagePresentationTitle :: AppRequestContext -> Text,
    pagePresentationNavigation :: AppRequestContext -> Maybe RouteNavigation,
    pagePresentationEnhancementHooks :: [Text]
  }

data RouteMetadata = RouteMetadata
  { routePageSegment :: Maybe Text,
    routePageSuffix :: Text,
    routePageTitle :: Text,
    routeEnhancementHooks :: [Text]
  }

-- | Compatibility presentation for routes that have not moved into the
-- discovered page family yet. P5 retires this table as those legacy route
-- definitions migrate; generated pages project their local 'PagePresentation'
-- instead.
pageRouteMetadata :: PageRoute -> RouteMetadata
pageRouteMetadata pageRoute =
  case pageRoute of
    HomePage -> RouteMetadata Nothing Text.empty "Home" []
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
