-- | Stable application facade for the composed example's two-audience JWT
-- runtime. 'App.Composed.Auth.Runtime' owns startup issuer, audience, and
-- RS256/JWK validation; 'App.Composed.Auth.Token' owns token issuance,
-- audience-specific verification, and claim parsing.
--
-- Decision (configurable registered JWT claims, 2026-09-30): keep the
-- reference profile on Harch's single JOSE verifier and resolve independent
-- web/API presence settings and minute skew in 'ComposedJwtConfiguration'.
-- Issuer and the distinct web/API audiences remain explicit protocol values;
-- both profiles emit expiry, with a configured one-hour default for web
-- tokens and the client lifetime for API tokens. @nbf@ generation defaults
-- on. Embedders supply policy values at their configuration boundary rather
-- than relying on a second JWT validation path.
--
-- Decision record (AHI-4E-CMH, 2026-09-28): retain this module as the stable
-- caller boundary, with one private runtime owner and one private token
-- owner. Issuance and verification stay together because they consume the
-- same immutable runtime and jointly define the two-audience failure
-- contract. This adds no second auth dispatcher, changes no Harch capability,
-- and creates no durable store for untrusted request data. Keep signature
-- and issuer validation in jose and the stable audience mismatch in the
-- application's claim parser.
--
-- Decision record (OpenAPI documentation and Swagger UI, 2026-09-26): this
-- extends the framework's existing JWT boundary
-- ('HarchWeb.Authentication.Jwt''s signer/verifier pipeline pieces, exactly
-- as @WebApi.AccountJwt.Runtime@ does for web-api) rather than adding a
-- parallel authentication layer; composed owns only its policy values
-- (issuer, the two audiences, claims shapes). No plaintext or token material
-- is ever logged or rendered by this module.
module App.Composed.Auth
  ( ComposedApiClaims (..),
    ComposedJwtConfiguration,
    ComposedJwtConfigurationError (..),
    ComposedJwtPolicySettings (..),
    ComposedJwtIssueError (..),
    ComposedJwtRuntime,
    ComposedWebClaims (..),
    composedApiProofVerifier,
    composedApiProofVerifierWithAcceptance,
    composedApiProofVerifierWithClock,
    composedJwtApiAudienceText,
    composedJwtIssuerText,
    composedJwtPublicJwkSet,
    composedAudienceMismatch,
    composedWebProofVerifier,
    composedWebProofVerifierWithAcceptance,
    composedWebProofVerifierWithClock,
    defaultComposedJwtPolicySettings,
    issueComposedApiToken,
    issueComposedWebToken,
    issueComposedWebTokenWithClock,
    loadComposedJwtRuntime,
    mkComposedJwtConfiguration,
    mkComposedJwtConfigurationWithPolicy,
    parseComposedApiJwtClaims,
    parseComposedWebJwtClaims,
  )
where

import App.Composed.Auth.Runtime
  ( ComposedJwtConfiguration,
    ComposedJwtConfigurationError (..),
    ComposedJwtPolicySettings (..),
    ComposedJwtRuntime,
    composedJwtApiAudienceText,
    composedJwtIssuerText,
    composedJwtPublicJwkSet,
    defaultComposedJwtPolicySettings,
    loadComposedJwtRuntime,
    mkComposedJwtConfiguration,
    mkComposedJwtConfigurationWithPolicy,
  )
import App.Composed.Auth.Token
  ( ComposedApiClaims (..),
    ComposedJwtIssueError (..),
    ComposedWebClaims (..),
    composedApiProofVerifier,
    composedApiProofVerifierWithAcceptance,
    composedApiProofVerifierWithClock,
    composedAudienceMismatch,
    composedWebProofVerifier,
    composedWebProofVerifierWithAcceptance,
    composedWebProofVerifierWithClock,
    issueComposedApiToken,
    issueComposedWebToken,
    issueComposedWebTokenWithClock,
    parseComposedApiJwtClaims,
    parseComposedWebJwtClaims,
  )
