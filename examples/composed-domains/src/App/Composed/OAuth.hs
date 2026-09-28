{-# LANGUAGE OverloadedStrings #-}

-- | The composed application's OAuth token and discovery protocol.
--
-- This extends the root's typed route algebra and Harch's bounded API
-- endpoint declaration; it does not create a second dispatcher. The issuer
-- and API audience are validated as HTTPS resource identifiers here, then
-- the same runtime supplies the issuer, audience and public-only JWKS used
-- for signing and verification. RFC well-known paths preserve issuer and
-- resource path components. Token body failures use the API endpoint's closed
-- body-failure hook so size, media-type and form errors share the OAuth error
-- shape and @Cache-Control: private, no-store@ response.
-- A guard also requires WAI to mark the token transport secure before the
-- body reader or client store can run. It does not infer security from
-- request-supplied forwarded headers; deployments must arrange for the HTTPS
-- listener or trusted transport adapter to set that WAI bit.
--
-- Decision record (AHI-4E-OAUTH, 2026-09-27): compose the existing
-- 'HarchWeb.ApiClientStore', bounded password-work workflow and startup
-- validated RS256 runtime so credentials, scopes, protocol errors and
-- metadata remain application-owned. Harch's typed route and body reader
-- remain the sole dispatch and bounded-input owners. The body reader
-- previously generated 400/413/415 responses before an application could
-- attach OAuth's required no-store headers. Taking over the body as a stream
-- would duplicate form parsing; extending the existing body declaration with
-- a closed failure reason keeps byte/field enforcement before decoding,
-- exposes no request detail, and permits this endpoint's safe interpretation.
-- Harch's normal typed field-failure renderer deliberately fixes status 400,
-- so OAuth opts into its status-preserving policy to return the required 401
-- Basic challenge for failed Authorization-header decoding. The authorization
-- metadata includes RFC 8414's required empty response-type list because this
-- server supports only the client-credentials grant, and omits an
-- unregistered protected-resources property. See @docs/design-guidance.md@.
-- Ownership decision for AHI-4E-OMH (2026-09-28): retain this module as the
-- stable facade and sole route-composition owner. Put validated configuration,
-- token protocol handling, and discovery/JWKS rendering in private
-- @Configuration@, @Token@, and @Discovery@ modules. This splits by application
-- responsibility while preserving the typed route dispatcher, endpoint paths,
-- secure-transport guard, bounded request-body declaration, protocol failure
-- mappings, and existing behavior tests. These internal owners add no second
-- router and no framework capability. The 2026-09-27 quality report measured
-- the original module at 530 lines and 24 imports; the concrete extraction was
-- tracked by @TASKS/ahi-4e-oauth-module-health.md@.
module App.Composed.OAuth
  ( ComposedOAuthConfigurationError (..),
    ComposedOAuthDependencies,
    ComposedOAuthTokenFailure (..),
    buildComposedOAuthModule,
    composedOAuthTokenOutcomeResponse,
    mkComposedOAuthDependencies,
  )
where

import App.Composed.Model (ComposedContext, OAuthRoute (..), RootAction, RootActionTarget, RootAuthorization, RootRoute (..))
import App.Composed.OAuth.Configuration
  ( ComposedOAuthConfigurationError (..),
    ComposedOAuthDependencies (..),
    ComposedOAuthUrl (..),
    mkComposedOAuthDependencies,
  )
import App.Composed.OAuth.Discovery
  ( authorizationServerMetadataRouteDefinition,
    protectedResourceMetadataRouteDefinition,
    publicJwksRouteDefinition,
  )
import App.Composed.OAuth.Token
  ( ComposedOAuthTokenFailure (..),
    composedOAuthInvalidRequestBody,
    composedOAuthTokenOutcomeResponse,
    tokenRouteDefinition,
  )
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import HarchWeb.Action (emptyActionCodec)
import HarchWeb.Api (apiResponseBodyToProtocolResponse)
import HarchWeb.ApplicationModule (ApplicationModule (..))
import HarchWeb.EndpointMetadata
  ( AccessRequirement (AllowUnauthenticated),
    EndpointMetadata,
    EndpointProtocol (ApiEndpoint),
    mkEndpointMetadata,
    requiredEndpointNameOrDie,
    requiredRouteTemplateOrDie,
  )
import HarchWeb.EndpointSecurity
  ( EndpointGuard (..),
    EndpointGuardResult (..),
    EndpointRequest (..),
  )
import HarchWeb.Routing
  ( RouteCodec (..),
    RouteLocation (..),
    RouteMethod (RouteGet, RoutePost),
    RouteParseResult (RouteNotMatched, RouteParsed),
    RouteRequest (..),
    pathSegmentText,
    requiredPathSegment,
    routeMethodPolicy,
    routePathSegments,
  )
import HarchWeb.SecurityEvent (requiredModuleNameOrDie)
import HarchWeb.Server (NonPageResponse (NonPageProtocolResponse))
import HarchWeb.Site (RouteDefinition)
import Network.Wai qualified as Wai

buildComposedOAuthModule :: ComposedOAuthDependencies -> ApplicationModule RootRoute RootActionTarget RootAction ComposedContext RootAuthorization
buildComposedOAuthModule dependencies =
  ApplicationModule
    { moduleName = requiredModuleNameOrDie "root.oauth",
      moduleOwnsRoute = isOAuthRoute,
      moduleRouteMountChain = const (requiredModuleNameOrDie "root.oauth" :| []),
      moduleRouteCodec = composedOAuthRouteCodec dependencies,
      moduleDeclaredRoutes = UnlocalizedOAuth <$> oauthRoutes,
      moduleEndpoints = oauthEndpointDefinition dependencies,
      moduleActionCodec = emptyActionCodec,
      moduleActionRoute = \_ _ -> Nothing,
      moduleHandleAction = \_ -> pure Nothing,
      moduleGuards = [requireSecureTokenTransport]
    }
  where
    oauthRoutes = [OAuthToken, OAuthJwks, OAuthAuthorizationServerMetadata, OAuthProtectedResourceMetadata]

isOAuthRoute :: RootRoute -> Bool
isOAuthRoute rootRoute =
  case rootRoute of
    UnlocalizedOAuth _ -> True
    _ -> False

requireSecureTokenTransport :: EndpointGuard RootRoute ComposedContext RootAuthorization
requireSecureTokenTransport =
  EndpointGuard $ \endpointRequest ->
    case requestRoute (endpointRouteRequest endpointRequest) of
      UnlocalizedOAuth OAuthToken
        | not (Wai.isSecure (endpointWaiRequest endpointRequest)) ->
            pure (HaltEndpoint (NonPageProtocolResponse (apiResponseBodyToProtocolResponse composedOAuthInvalidRequestBody)))
      _ -> pure (ContinueEndpoint (requestContext (endpointRouteRequest endpointRequest)))

composedOAuthRouteCodec :: ComposedOAuthDependencies -> RouteCodec RootRoute ComposedContext
composedOAuthRouteCodec dependencies =
  RouteCodec
    { parseRoute = \requestContext location ->
        let segments = pathSegmentText <$> routePathSegments location
         in case lookup segments routeLocations of
              Just route -> RouteParsed (RouteRequest (UnlocalizedOAuth route) requestContext)
              Nothing -> RouteNotMatched,
      renderRoute = \request ->
        case requestRoute request of
          UnlocalizedOAuth selectedRoute -> RouteLocation (requiredPathSegment <$> routeSegments selectedRoute) []
          _ -> RouteLocation [] [],
      notFoundRequest = RouteRequest (UnlocalizedOAuth OAuthToken),
      routeMethods = \request ->
        case requestRoute request of
          UnlocalizedOAuth OAuthToken -> routeMethodPolicy [RoutePost]
          UnlocalizedOAuth _ -> routeMethodPolicy [RouteGet]
          _ -> routeMethodPolicy []
    }
  where
    oauthRoutes = [OAuthToken, OAuthJwks, OAuthAuthorizationServerMetadata, OAuthProtectedResourceMetadata]
    routeLocations = [(routeSegments route, route) | route <- oauthRoutes]
    routeSegments route =
      case route of
        OAuthToken -> composedOAuthBaseSegments (composedOAuthIssuerUrl dependencies) <> ["oauth", "token"]
        OAuthJwks -> composedOAuthBaseSegments (composedOAuthIssuerUrl dependencies) <> ["oauth", "jwks.json"]
        OAuthAuthorizationServerMetadata -> [".well-known", "oauth-authorization-server"] <> composedOAuthBaseSegments (composedOAuthIssuerUrl dependencies)
        OAuthProtectedResourceMetadata -> [".well-known", "oauth-protected-resource"] <> composedOAuthBaseSegments (composedOAuthResourceUrl dependencies)

oauthEndpointDefinition :: ComposedOAuthDependencies -> RootRoute -> RouteDefinition RootRoute ComposedContext RootAuthorization
oauthEndpointDefinition dependencies rootRoute =
  case rootRoute of
    UnlocalizedOAuth OAuthToken -> tokenRouteDefinition dependencies (oauthEndpointMetadata dependencies OAuthToken)
    UnlocalizedOAuth OAuthJwks -> publicJwksRouteDefinition dependencies (oauthEndpointMetadata dependencies OAuthJwks)
    UnlocalizedOAuth OAuthAuthorizationServerMetadata -> authorizationServerMetadataRouteDefinition dependencies (oauthEndpointMetadata dependencies OAuthAuthorizationServerMetadata)
    UnlocalizedOAuth OAuthProtectedResourceMetadata -> protectedResourceMetadataRouteDefinition dependencies (oauthEndpointMetadata dependencies OAuthProtectedResourceMetadata)
    _ -> error "composed-domains: OAuth definitions come from their root protocol module"

oauthEndpointMetadata :: ComposedOAuthDependencies -> OAuthRoute -> EndpointMetadata RootAuthorization
oauthEndpointMetadata dependencies route =
  mkEndpointMetadata
    (requiredEndpointNameOrDie (endpointNameFor route))
    (requiredRouteTemplateOrDie (endpointPathFor dependencies route))
    ApiEndpoint
    AllowUnauthenticated

endpointNameFor :: OAuthRoute -> Text
endpointNameFor route =
  case route of
    OAuthToken -> "root.oauth.token"
    OAuthJwks -> "root.oauth.jwks"
    OAuthAuthorizationServerMetadata -> "root.oauth.authorization-server-metadata"
    OAuthProtectedResourceMetadata -> "root.oauth.protected-resource-metadata"

endpointPathFor :: ComposedOAuthDependencies -> OAuthRoute -> Text
endpointPathFor dependencies route =
  case route of
    OAuthToken -> endpointPath (composedOAuthIssuerUrl dependencies) "/oauth/token"
    OAuthJwks -> endpointPath (composedOAuthIssuerUrl dependencies) "/oauth/jwks.json"
    OAuthAuthorizationServerMetadata -> wellKnownPath "oauth-authorization-server" (composedOAuthIssuerUrl dependencies)
    OAuthProtectedResourceMetadata -> wellKnownPath "oauth-protected-resource" (composedOAuthResourceUrl dependencies)

endpointPath :: ComposedOAuthUrl -> Text -> Text
endpointPath url suffix = composedOAuthBasePath url <> suffix

wellKnownPath :: Text -> ComposedOAuthUrl -> Text
wellKnownPath suffix url =
  "/.well-known/" <> suffix <> composedOAuthBasePath url
