{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- Deprecated per upstream jose: reading 'Crypto.JWT.unregisteredClaims' back
-- off an already-verified 'Crypto.JWT.ClaimsSet'. See this module's Haddock
-- for the framework-capability-gap decision this pragma records.
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | The AHI-4D combined account-or-API-client-bearer authentication profile.
--
-- Decision record (AHI-4D slice 5, 2026-09-17): @GET \/api\/second@ must admit
-- either an already-authenticated account (cookie or bearer, exactly like the
-- existing account profile) or an OAuth 2.0 API-client bearer token carrying
-- a sufficient scope. Neither the account profile nor the client-credentials
-- token workflow can express this alone: the account profile has one fixed
-- principal type, and scope-gating only matters for the API-client case. This
-- module is therefore its own authentication profile with a principal sum
-- type and a claims-shape-discriminating JWT projection, reusing
-- 'WebApi.AccountJwt.accountJwtRuntimeProofVerifier' and
-- 'WebApi.AccountJwt.accountJwtRuntimeProofExtractor' for the shared RS256
-- verification key and cookie policy rather than proving a second key pair.
--
-- An account-shaped claim (has @jti@) and an API-client-shaped claim (has
-- @scope@, never @jti@; see 'WebApi.ApiClientToken.claimsForApiClient') are
-- structurally disjoint, exactly as the existing @\/api\/me@ decision already
-- established. Discrimination therefore only needs an ordered try: account
-- shape first, API-client shape on its failure.
--
-- Framework-capability-gap decision (design-guidance.md protocol, option 2 —
-- application-layer workaround): 'HarchWeb.jwtProofVerifier' (and the
-- @jose@ 'Crypto.JWT.verifyClaims' it wraps) always projects a verified proof
-- into bare 'Crypto.JWT.ClaimsSet', not a caller-chosen @'Crypto.JWT.HasClaimsSet'@
-- subtype — confirmed directly against the pinned @jose@ version's actual
-- 'Crypto.JWT.verifyClaims' signature, which fixes its result to 'ClaimsSet'
-- via a 'Crypto.JWT.VerificationKeyStore' constraint naming 'ClaimsSet'
-- explicitly, not a free type variable. Generalizing Harch's verifier to a
-- real claims subtype would mean reimplementing @jose@'s own claims
-- validation (@iss@\/@aud@\/@exp@\/@nbf@) outside 'Crypto.JWT.verifyClaims',
-- which is not a small, general framework primitive — the option-1 test this
-- protocol requires before falling back to an application workaround. The
-- @scope@ claim is therefore read back from the already-verified
-- 'Crypto.JWT.ClaimsSet' via the upstream-deprecated
-- 'Crypto.JWT.unregisteredClaims' lens, the only accessor @jose@ exposes for
-- an application-defined claim once decoded as bare 'ClaimsSet'. This does
-- not weaken any security property: the claim is read only after the JWT's
-- signature and standard claims are already verified by the unmodified
-- 'HarchWeb.jwtProofVerifier' pipeline. This module's
-- @{-\# OPTIONS_GHC -Wno-deprecations \#-}@ is scoped to this one file for
-- exactly this read.
--
-- The combined principal is intentionally not attached to
-- 'WebApi.Route.AppRequestContext': no current @\/api\/second@ handler reads
-- caller identity, so adding a context field for it now would be speculative.
-- A future need to know which principal kind served a request is real, later
-- work, not built preemptively here.
module WebApi.ResourceAuthentication
  ( resourceAuthenticationPipeline,
  )
where

import Control.Lens (preview, (^.))
import Crypto.JWT qualified as Jwt
import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import HarchWeb qualified
import HarchWeb.Authentication (ApiClientStore (..), ApiClientStoreError (..))
import HarchWeb.Time (UnixTimeNanoseconds)
import Network.HTTP.Types qualified as Http
import WebApi.AccountJwt
  ( AccountJwtClaims,
    AccountJwtRuntime,
    accountJwtRuntimeProofExtractor,
    accountJwtRuntimeProofVerifier,
    authenticationErrorResponse,
    establishAccountPrincipal,
    parseAccountJwtClaims,
  )
import WebApi.AccountPrincipal (AccountPrincipal)
import WebApi.ApiClient
  ( ApiClient,
    ApiClientId,
    EstablishedApiClient,
    intersectEstablishedApiClientScopes,
    mkApiClientId,
  )
import WebApi.Route (AppAuthorization, AppRequestContext, AppRoute (LoginRoute))
import WebApi.Session (AccountSessionStore)

-- | The claims-shape-discriminated JWT projection: an already-established
-- account session claim, or an API-client identifier with the scopes its
-- bearer token was issued with (before intersection with that client's
-- current durable allowance, which principal establishment applies).
data ResourceJwtClaims
  = ResourceAccountJwtClaims AccountJwtClaims
  | ResourceApiClientJwtClaims ApiClientId [HarchWeb.OAuth2Scope]

-- | The established combined principal. An API-client's scopes here are
-- already intersected with its current durable allowance. Carries no client
-- identifier: nothing in this module (or 'authenticationAttachPrincipal's
-- deliberate no-op, see this module's own Haddock) reads API-client
-- identity once its scopes are resolved, so keeping one here would be dead
-- data no test could honestly force.
data ResourcePrincipal
  = ResourceAccountPrincipal AccountPrincipal
  | ResourceApiClientPrincipal [HarchWeb.OAuth2Scope]

-- | Construct the combined authentication rail. Both principal kinds share
-- this runtime's already-startup-proven RS256 key and cookie policy; the
-- durable API-client store is the same one the client-credentials token
-- workflow already uses, so a revoked or rescoped client takes effect on its
-- very next resource request exactly as it does on its next token request.
resourceAuthenticationPipeline ::
  AccountSessionStore ->
  IO UnixTimeNanoseconds ->
  AccountJwtRuntime ->
  ApiClientStore ApiClientId ApiClient EstablishedApiClient ->
  HarchWeb.AuthenticationPipeline AppRoute AppRequestContext AppAuthorization HarchWeb.JwtProof ResourceJwtClaims ResourcePrincipal HarchWeb.ScopeAuthorizationDenial
resourceAuthenticationPipeline sessionStore readClock jwtRuntime apiClientStore =
  HarchWeb.AuthenticationPipeline
    { HarchWeb.authenticationProofExtractor = accountJwtRuntimeProofExtractor jwtRuntime,
      HarchWeb.authenticationProofVerifier =
        HarchWeb.AuthenticationProofVerifier $ \proof ->
          HarchWeb.verifyAuthenticationProof (accountJwtRuntimeProofVerifier jwtRuntime parseResourceJwtClaims) proof,
      HarchWeb.authenticationPrincipalEstablisher = resourcePrincipalEstablisher sessionStore readClock apiClientStore,
      HarchWeb.authenticationAuthorization =
        HarchWeb.AuthenticationWithAuthorization
          resourceAuthorizationInterpreter
          (const knownResourceScopeDenied)
          (\endpointRequest _denial -> resourceLoginRedirect endpointRequest),
      HarchWeb.authenticationAttachPrincipal = \_ context -> context,
      HarchWeb.authenticationChallenge = \endpointRequest _failure -> resourceLoginRedirect endpointRequest,
      HarchWeb.authenticationUnavailable = \endpointRequest _ ->
        authenticationErrorResponse (HarchWeb.requestContext (HarchWeb.endpointRouteRequest endpointRequest)) Http.status503 "Authentication is temporarily unavailable."
    }

-- | The same 303-to-login challenge every other account-protected route
-- gives, reused rather than re-derived (an ignored second argument covers
-- both a failed authentication and a denied authorization). Consequence
-- named rather than hidden, exactly as for @\/api\/me@: an API-client bearer
-- request that fails authentication or authorization also receives this
-- redirect response, not a JSON 401\/403 body — a machine client cannot
-- follow it, but distinguishing API-shaped challenges by
-- 'HarchWeb.EndpointProtocol' is the same 'HarchWeb.EndpointSecurity'-wide
-- gap already named for @\/api\/me@, not a new one this route introduces.
resourceLoginRedirect :: HarchWeb.EndpointRequest AppRoute AppRequestContext AppAuthorization -> HarchWeb.NonPageResponse AppRoute AppRequestContext
resourceLoginRedirect endpointRequest =
  HarchWeb.authenticationChallengeForAction endpointRequest ordinaryChallenge
  where
    requestContext = HarchWeb.requestContext (HarchWeb.endpointRouteRequest endpointRequest)
    ordinaryChallenge =
      HarchWeb.nonPageInternalRedirectResponse
        Http.status303
        HarchWeb.RouteRequest
          { HarchWeb.requestRoute = LoginRoute,
            HarchWeb.requestContext = requestContext
          }

parseResourceJwtClaims :: Jwt.ClaimsSet -> Either HarchWeb.JwtClaimsError ResourceJwtClaims
parseResourceJwtClaims claims =
  case parseAccountJwtClaims claims of
    Right accountClaims -> Right (ResourceAccountJwtClaims accountClaims)
    Left _ -> ResourceApiClientJwtClaims <$> parseApiClientId <*> parseGrantedScopes
  where
    parseApiClientId = do
      subject <- maybe (Left invalidResourceJwtClaims) Right (claims ^. Jwt.claimSub)
      clientIdText <- maybe (Left invalidResourceJwtClaims) Right (preview Jwt.string subject)
      either (const (Left invalidResourceJwtClaims)) Right (mkApiClientId clientIdText)
    parseGrantedScopes =
      case Map.lookup "scope" (claims ^. Jwt.unregisteredClaims) of
        Just (Aeson.String scopeText) -> traverse (either (const (Left invalidResourceJwtClaims)) Right . HarchWeb.mkOAuth2Scope) (Text.words scopeText)
        _ -> Left invalidResourceJwtClaims

invalidResourceJwtClaims :: HarchWeb.JwtClaimsError
invalidResourceJwtClaims =
  HarchWeb.mkJwtClaimsError (HarchWeb.requiredSecurityFailureCodeOrDie "resource.jwt.claims-rejected")

-- | Establish the combined principal. An account claim reuses the exact
-- durable-session establishment the account profile already uses. An
-- API-client claim re-resolves the client's current durable view on every
-- request — never a cross-request acceptance cache — so a disabled client or
-- a scope removed since token issuance takes effect immediately.
resourcePrincipalEstablisher ::
  AccountSessionStore ->
  IO UnixTimeNanoseconds ->
  ApiClientStore ApiClientId ApiClient EstablishedApiClient ->
  HarchWeb.PrincipalEstablisher ResourceJwtClaims ResourcePrincipal
resourcePrincipalEstablisher sessionStore readClock apiClientStore =
  HarchWeb.PrincipalEstablisher $ \case
    ResourceAccountJwtClaims accountClaims -> do
      established <- HarchWeb.establishPrincipal (establishAccountPrincipal sessionStore readClock) accountClaims
      pure (ResourceAccountPrincipal <$> established)
    ResourceApiClientJwtClaims clientId grantedScopes -> do
      lookupResult <- establishApiClient apiClientStore clientId
      pure $ case lookupResult of
        Left (ApiClientStoreUnavailable dependency) -> Left (HarchWeb.PrincipalEstablishmentUnavailable dependency)
        Right Nothing -> Left (HarchWeb.PrincipalRejected apiClientPrincipalRejected)
        Right (Just established) -> Right (ResourceApiClientPrincipal (intersectEstablishedApiClientScopes established grantedScopes))

apiClientPrincipalRejected :: HarchWeb.PrincipalRejection
apiClientPrincipalRejected = HarchWeb.mkPrincipalRejection (HarchWeb.requiredSecurityFailureCodeOrDie "resource.api-client.rejected")

-- | An account principal is authorized unconditionally: account access to
-- @\/api\/second@ does not depend on OAuth scopes at all. An API-client
-- principal reuses 'HarchWeb.scopeAuthorizationInterpreter''s exact
-- 'HarchWeb.RequireAllScopes'\/'HarchWeb.RequireAnyScope' matching against its
-- already-intersected effective scopes, rather than re-deriving that match.
resourceAuthorizationInterpreter :: HarchWeb.AuthorizationInterpreter ResourcePrincipal AppAuthorization HarchWeb.ScopeAuthorizationDenial
resourceAuthorizationInterpreter = HarchWeb.AuthorizationInterpreter $ \principal requirement ->
  case principal of
    ResourceAccountPrincipal _ -> HarchWeb.Authorized
    ResourceApiClientPrincipal effectiveScopes ->
      -- Treats the already-resolved scope list itself as
      -- 'scopeAuthorizationInterpreter''s "principal" (projected by 'id'),
      -- rather than re-passing the outer 'ResourcePrincipal' through a
      -- 'const'-ignored projection: the outer principal has nothing left to
      -- contribute once its effective scopes are already in hand, so a
      -- placeholder argument here would be dead weight no test could ever
      -- force honestly.
      HarchWeb.authorizePrincipal (HarchWeb.scopeAuthorizationInterpreter id) effectiveScopes requirement

knownResourceScopeDenied :: HarchWeb.SecurityFailureCode
knownResourceScopeDenied = HarchWeb.requiredSecurityFailureCodeOrDie "resource.scope-denied"
