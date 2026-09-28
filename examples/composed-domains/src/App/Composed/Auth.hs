-- | Stable application facade for the composed example's two-audience JWT
-- runtime. 'App.Composed.Auth.Runtime' owns startup issuer, audience, and
-- RS256/JWK validation; 'App.Composed.Auth.Token' owns token issuance,
-- audience-specific verification, and claim parsing.
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
    ComposedJwtIssueError (..),
    ComposedJwtRuntime,
    ComposedWebClaims (..),
    composedApiProofVerifier,
    composedJwtApiAudienceText,
    composedJwtIssuerText,
    composedJwtPublicJwkSet,
    composedAudienceMismatch,
    composedWebProofVerifier,
    issueComposedApiToken,
    issueComposedWebToken,
    loadComposedJwtRuntime,
    mkComposedJwtConfiguration,
    parseComposedApiJwtClaims,
    parseComposedWebJwtClaims,
  )
where

import App.Composed.Auth.Runtime
  ( ComposedJwtConfiguration,
    ComposedJwtConfigurationError (..),
    ComposedJwtRuntime,
    composedJwtApiAudienceText,
    composedJwtIssuerText,
    composedJwtPublicJwkSet,
    loadComposedJwtRuntime,
    mkComposedJwtConfiguration,
  )
import App.Composed.Auth.Token
  ( ComposedApiClaims (..),
    ComposedJwtIssueError (..),
    ComposedWebClaims (..),
    composedApiProofVerifier,
    composedAudienceMismatch,
    composedWebProofVerifier,
    issueComposedApiToken,
    issueComposedWebToken,
    parseComposedApiJwtClaims,
    parseComposedWebJwtClaims,
  )
