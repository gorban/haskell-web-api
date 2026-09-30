-- | Application-owned endpoint identities and admission requirements.
-- Discovered page titles, hooks, and navigation live in their page
-- presentations; this module keeps the separate security declaration used by
-- the existing Site dispatcher and API documentation.
module WebApi.Route.Endpoint
  ( endpointMetadata,
    html,
    protectedHtml,
    api,
    protectedApi,
  )
where

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
  ( AppAuthorization,
    accountAuthenticationProfileName,
    resourceAuthenticationProfileName,
    resourceReadScope,
  )
import WebApi.Route.Types
  ( ApiRoute (..),
    AppRoute (..),
    PageRoute (..),
  )

-- | Stable endpoint names and admission requirements for the closed
-- application route algebra. Page presentation is projected from generated
-- module declarations by 'WebApi.Route.routeMetadata'.
endpointMetadata :: AppRoute -> EndpointMetadata AppAuthorization
endpointMetadata route =
  case route of
    Page HomePage -> html "web.home" "/{locale}"
    Page TodoPage -> html "web.todo" "/{locale}/todo"
    Page RegistrationPage -> html "account.registration" "/{locale}/register"
    Page EmailVerificationPage -> html "account.email-verification" "/{locale}/verify"
    Page MfaEnrollmentPage -> html "account.mfa-enrollment" "/{locale}/mfa"
    Page LoginPage -> html "account.login" "/{locale}/login"
    Page LogoutPage -> protectedHtml "account.logout" "/{locale}/logout"
    Page ProfilePage -> protectedHtml "account.profile" "/{locale}/profile"
    Page LanguagePage -> html "web.language" "/{locale}/language"
    Page HelpPage -> html "web.help" "/{locale}/help"
    Page DocsSwaggerPage -> html "web.docs" "/{locale}/docs"
    Page PageNotFound -> html "web.not-found" "/{locale}/404"
    Api StatusApi -> api "api.status" "/api/status"
    -- Scoped API authentication: an account cookie/bearer session is authorized
    -- unconditionally; an API-client bearer token must carry the
    -- 'resourceReadScope' scope (directly, or via its current durable
    -- allowance). See 'WebApi.ResourceAuthentication' and the scoped
    -- API-authentication design's decision record in @docs/design-guidance.md@.
    Api SecondApi ->
      HarchWeb.withAuthenticationProfile
        resourceAuthenticationProfileName
        (declaredMetadata ApiEndpoint (HarchWeb.RequireAuthorized (HarchWeb.RequireAnyScope (resourceReadScope NonEmpty.:| []))) "api.second" "/api/second")
    -- Reuses the account profile's existing 'RequireAuthenticated' guard
    -- rather than a new authorization payload: an API-client bearer JWT has
    -- no session ID ('jti') claim, so it already fails this profile's claims
    -- parse and can never reach this handler merely by presenting a
    -- similarly named scope. See the scoped API-authentication design's
    -- decision record in @docs/design-guidance.md@.
    Api MeApi -> protectedApi "api.me" "/api/me"
    -- The token endpoint authenticates its OAuth client itself (HTTP Basic
    -- client-credentials, verified against the durable API-client store), so
    -- it declares 'AllowUnauthenticated' like every other API route here: no
    -- account session or bearer JWT establishes the caller before this
    -- handler runs. See the scoped API-authentication design's decision record in
    -- @docs/design-guidance.md@.
    Api TokenApi -> api "api.oauth-token" "/api/oauth/token"
    -- OpenAPI documentation and Swagger UI: the specification is an ordinary unauthenticated typed API
    -- endpoint; its security choice stays this application's, exactly like
    -- every other route's metadata here.
    Api DocsOpenApiSpec -> api "api.openapi-spec" "/docs/openapi.json"
    Api ApiNotFound -> api "api.not-found" "/api/404"
    -- Endpoint names remain explicit and stable, while each route template is
    -- derived from the same path emitted by page-file discovery.
    GeneratedPages Generated.ShowcasePage -> html "web.showcase" (generatedPageTemplate Generated.ShowcasePage)
    GeneratedPages Generated.ShowcaseAlternatePage -> html "web.showcase-alternate" (generatedPageTemplate Generated.ShowcaseAlternatePage)
    GeneratedPages Generated.SecondPage -> html "web.second" (generatedPageTemplate Generated.SecondPage)

generatedPageTemplate :: Generated.PageRoute -> Text
generatedPageTemplate pageRoute =
  "/{locale}" <> Text.dropWhileEnd (== '/') (Generated.pageRoutePath pageRoute)

-- Per docs/design-guidance.md's never-mask-a-gate-finding rule: the @$!@ on
-- the name and template below is a confirmed, reproducible fix for the
-- documented HPC pattern where a binding used as a direct argument to an
-- instrumented call stays unticked (every table row's literals are validated
-- end to end by the route-table tests).
{-# ANN declaredMetadata ("HLint: ignore Redundant $!" :: String) #-}
declaredMetadata :: EndpointProtocol -> AccessRequirement AppAuthorization -> Text -> Text -> EndpointMetadata AppAuthorization
declaredMetadata protocol accessRequirement name template =
  mkEndpointMetadata
    (requiredEndpointNameOrDie $! name)
    (requiredRouteTemplateOrDie $! template)
    protocol
    accessRequirement

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
