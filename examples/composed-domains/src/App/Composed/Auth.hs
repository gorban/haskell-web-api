{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | The composed example's self-contained two-audience JWT runtime (AHI-4E).
-- One issuer and one RS256 signing/JWKS key set mint and verify two distinct
-- audiences: the account-web audience for cookie-authenticated web routes and
-- the API audience for explicit bearer tokens over the documented API
-- subtree. A correctly signed web token therefore fails the API on audience
-- exactly — never on signature or issuer — and the reverse holds for an API
-- token presented to web routes.
--
-- Decision record (AHI-4E, 2026-09-26): this extends the framework's existing
-- JWT boundary ('HarchWeb.Authentication.Jwt''s signer/verifier pipeline
-- pieces, exactly as @WebApi.AccountJwt.Runtime@ does for web-api) rather
-- than adding a parallel authentication layer; composed owns only its policy
-- values (issuer, the two audiences, claims shapes). The audience check lives
-- in the claims parser, not in @jose@'s validation-settings predicate: only
-- the parser can attach the stable, audience-named failure code the
-- wrong-audience acceptance requires ('composedAudienceMismatch'), while
-- signature and issuer remain the validation settings' job. No plaintext or
-- token material is ever logged or rendered by this module.
--
-- Deprecated per upstream jose: writing and reading the @scope@ claim goes
-- through 'Crypto.JWT.unregisteredClaims', mirroring
-- 'WebApi.ResourceAuthentication''s recorded framework-capability-gap
-- decision (option 2): @jose@'s 'Crypto.JWT.verifyClaims' fixes its result
-- to bare 'Crypto.JWT.ClaimsSet', so no caller-supplied subtype can carry the
-- OAuth scope claim and this lens is the only accessor for it.
module App.Composed.Auth
  ( ComposedApiClaims (..),
    ComposedJwtConfiguration,
    ComposedJwtConfigurationError (..),
    ComposedJwtIssueError (..),
    ComposedJwtRuntime,
    ComposedWebClaims (..),
    composedApiProofVerifier,
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

import Control.Lens (matching, preview, (#), (&), (.~), (?~), (^.))
import Crypto.JOSE.Header (HeaderParam (..), RequiredProtection (..))
import Crypto.JOSE.JWA.JWS qualified as JwaJws
import Crypto.JOSE.JWK (JWK, JWKSet (..))
import Crypto.JOSE.JWK qualified as JoseJwk
import Crypto.JOSE.JWS qualified as JoseJws
import Crypto.JWT qualified as Jwt
import Data.Aeson (ToJSON, Value (String))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb
  ( AuthenticationProofVerifier (AuthenticationProofVerifier),
    EncodedJwt,
    JWSHeader,
    JwtAlgorithm (JwtRs256),
    JwtClaimsError,
    JwtProof,
    JwtSigner (JwtSigner),
    joseJwtSigner,
    jwtProofEncodedJwt,
    jwtProofVerifier,
    mapJwtSignerError,
    mkJwtAllowedAlgorithms,
    mkJwtClaimsError,
    requiredSecurityFailureCodeOrDie,
    signJwt,
    verifyAuthenticationProof,
  )

-- | The composed deployment's immutable issuer identity: one issuer name and
-- two deliberately distinct audience values over one key set.
data ComposedJwtConfiguration = ComposedJwtConfiguration
  { composedJwtIssuer :: Jwt.StringOrURI,
    composedWebAudience :: Jwt.StringOrURI,
    composedApiAudience :: Jwt.StringOrURI,
    composedJwtActiveKeyId :: Text
  }
  deriving (Eq, Show)

-- | Parsed at startup: an empty issuer/audience/key-id value or audiences
-- that are not distinct is a deployment error, never a runtime fallback.
data ComposedJwtConfigurationError
  = ComposedJwtIssuerEmpty
  | ComposedJwtWebAudienceEmpty
  | ComposedJwtApiAudienceEmpty
  | ComposedJwtAudiencesNotDistinct
  | ComposedJwtActiveKeyIdEmpty
  | ComposedJwtVerificationKeyMismatch
  | ComposedJwtIssuerUnparseable
  | ComposedJwtWebAudienceUnparseable
  | ComposedJwtApiAudienceUnparseable
  deriving (Eq, Show)

mkComposedJwtConfiguration :: Text -> Text -> Text -> Text -> Either ComposedJwtConfigurationError ComposedJwtConfiguration
mkComposedJwtConfiguration issuerText webAudienceText apiAudienceText activeKeyId
  | Text.null issuerText = Left ComposedJwtIssuerEmpty
  | Text.null webAudienceText = Left ComposedJwtWebAudienceEmpty
  | Text.null apiAudienceText = Left ComposedJwtApiAudienceEmpty
  | webAudienceText == apiAudienceText = Left ComposedJwtAudiencesNotDistinct
  | Text.null activeKeyId = Left ComposedJwtActiveKeyIdEmpty
  | otherwise =
      case (normalizedStringOrUri issuerText, normalizedStringOrUri webAudienceText, normalizedStringOrUri apiAudienceText) of
        (Just issuer, Just webAudience, Just apiAudience) ->
          Right
            ComposedJwtConfiguration
              { composedJwtIssuer = issuer,
                composedWebAudience = webAudience,
                composedApiAudience = apiAudience,
                composedJwtActiveKeyId = activeKeyId
              }
        (Nothing, _, _) -> Left ComposedJwtIssuerUnparseable
        (_, Nothing, _) -> Left ComposedJwtWebAudienceUnparseable
        (_, _, Nothing) -> Left ComposedJwtApiAudienceUnparseable

-- | Mint and compare through jose's own @stringOrUri@ normalization (the
-- exact construction @jose@'s @FromJSON@ produces on verification), so a
-- URI-shaped issuer or audience round-trips to a value equal to the one the
-- claims carried: raw @String@-form construction would compare unequal to
-- the parsed @URI@ form and reject its own tokens.
normalizedStringOrUri :: Text -> Maybe Jwt.StringOrURI
normalizedStringOrUri value =
  case matching Jwt.stringOrUri (Text.unpack value) of
    Left _ -> Nothing
    Right parsedValue -> Just parsedValue

-- | The startup-validated runtime: one RS256 signing key and its verification
-- set, over which both audiences are minted and verified.
data ComposedJwtRuntime = ComposedJwtRuntime
  { composedRuntimeConfiguration :: ComposedJwtConfiguration,
    composedRuntimeSigningKey :: JWK,
    composedRuntimeVerificationKeys :: JWKSet
  }

data ComposedJwtIssueError = ComposedJwtIssueFailed
  deriving (Eq, Show)

-- | Startup validation fails closed: the verification set must contain the
-- signing key's own material under the active key id, so every minted token
-- is verifiable by this runtime's own set and a verification key that does
-- not match the signing key is a deployment error.
loadComposedJwtRuntime :: ComposedJwtConfiguration -> JWK -> JWKSet -> Either ComposedJwtConfigurationError ComposedJwtRuntime
loadComposedJwtRuntime configuration signingKey verificationKeys@(JWKSet candidates)
  | any verificationKeyMatches candidates =
      Right
        ComposedJwtRuntime
          { composedRuntimeConfiguration = configuration,
            composedRuntimeSigningKey = signingKey,
            composedRuntimeVerificationKeys = verificationKeys
          }
  | otherwise = Left ComposedJwtVerificationKeyMismatch
  where
    activeKeyId = composedJwtActiveKeyId configuration
    verificationKeyMatches candidate =
      candidate ^. JoseJwk.jwkKid == Just activeKeyId && candidate == signingKey

-- | The web token's verified shape: its subject is the account identity.
newtype ComposedWebClaims = ComposedWebClaims {composedWebSubject :: Text}
  deriving (Eq, Show)

-- | The API token's verified shape: its subject names the OAuth client and
-- its space-delimited @scope@ claim carries the granted scopes.
data ComposedApiClaims = ComposedApiClaims
  { composedApiSubject :: Text,
    composedApiScopes :: [Text]
  }
  deriving (Eq, Show)

-- | Mint one account-web-audience token over the shared key set.
issueComposedWebToken :: ComposedJwtRuntime -> Text -> IO (Either ComposedJwtIssueError EncodedJwt)
issueComposedWebToken runtime subject =
  signJwt signer header claims
  where
    configuration = composedRuntimeConfiguration runtime
    signer = composedSigner runtime
    header = composedJwsHeader configuration
    claims =
      Jwt.emptyClaimsSet
        & Jwt.claimIss ?~ composedJwtIssuer configuration
        & Jwt.claimAud ?~ Jwt.Audience [composedWebAudience configuration]
        & Jwt.claimSub ?~ (Jwt.string # subject)

-- | Mint one API-audience token over the shared key set.
issueComposedApiToken :: ComposedJwtRuntime -> Text -> [Text] -> IO (Either ComposedJwtIssueError EncodedJwt)
issueComposedApiToken runtime subject scopes =
  signJwt signer header claims
  where
    configuration = composedRuntimeConfiguration runtime
    signer = composedSigner runtime
    header = composedJwsHeader configuration
    claims =
      Jwt.emptyClaimsSet
        & Jwt.claimIss ?~ composedJwtIssuer configuration
        & Jwt.claimAud ?~ Jwt.Audience [composedApiAudience configuration]
        & Jwt.claimSub ?~ (Jwt.string # subject)
        & Jwt.unregisteredClaims .~ scopeClaim scopes

scopeClaim :: [Text] -> Map.Map Text Value
scopeClaim scopes = Map.singleton "scope" (String (Text.unwords scopes))

composedSigner :: (ToJSON claims) => ComposedJwtRuntime -> JwtSigner ComposedJwtIssueError claims
composedSigner runtime =
  JwtSigner $ \header claims ->
    signJwt (mapJwtSignerError (const ComposedJwtIssueFailed) (joseJwtSigner (composedRuntimeSigningKey runtime))) header claims

composedJwsHeader :: ComposedJwtConfiguration -> JWSHeader RequiredProtection
composedJwsHeader configuration =
  JoseJws.newJWSHeaderProtected JwaJws.RS256
    & JoseJws.kid ?~ HeaderParam RequiredProtection (composedJwtActiveKeyId configuration)

-- | The stable, audience-named rejection for a correctly signed token whose
-- audience belongs to the other profile. This is the failure the
-- wrong-audience acceptance pins: an API request carrying a valid web token
-- is rejected exactly here, not at signature or issuer verification.
composedAudienceMismatch :: JwtClaimsError
composedAudienceMismatch =
  mkJwtClaimsError (requiredSecurityFailureCodeOrDie "composed.audience-mismatch")

-- | Verify a web-audience token against this runtime's startup-proven keys
-- and issuer, then require the account-web audience by name.
composedWebProofVerifier :: ComposedJwtRuntime -> AuthenticationProofVerifier JwtProof ComposedWebClaims
composedWebProofVerifier runtime =
  AuthenticationProofVerifier $ \proof ->
    verifyAuthenticationProof
      ( jwtProofVerifier
          (composedValidationSettings runtime)
          (mkJwtAllowedAlgorithms (JwtRs256 :| []))
          (composedRuntimeVerificationKeys runtime)
          (parseComposedWebJwtClaims (composedRuntimeConfiguration runtime))
      )
      (jwtProofEncodedJwt proof)

-- | Verify an API-audience token the same way, then require the API audience
-- by name — so a correctly signed web token fails exactly at the audience.
composedApiProofVerifier :: ComposedJwtRuntime -> AuthenticationProofVerifier JwtProof ComposedApiClaims
composedApiProofVerifier runtime =
  AuthenticationProofVerifier $ \proof ->
    verifyAuthenticationProof
      ( jwtProofVerifier
          (composedValidationSettings runtime)
          (mkJwtAllowedAlgorithms (JwtRs256 :| []))
          (composedRuntimeVerificationKeys runtime)
          (parseComposedApiJwtClaims (composedRuntimeConfiguration runtime))
      )
      (jwtProofEncodedJwt proof)

composedValidationSettings :: ComposedJwtRuntime -> Jwt.JWTValidationSettings
composedValidationSettings runtime =
  Jwt.defaultJWTValidationSettings (const True)
    & Jwt.jwtValidationSettingsIssuerPredicate .~ (== composedJwtIssuer (composedRuntimeConfiguration runtime))

-- | A web token must carry the account-web audience and a subject.
parseComposedWebJwtClaims :: ComposedJwtConfiguration -> Jwt.ClaimsSet -> Either JwtClaimsError ComposedWebClaims
parseComposedWebJwtClaims configuration claims
  | not (claimsCarryAudience (composedWebAudience configuration) claims) = Left composedAudienceMismatch
  | otherwise =
      case previewSubject claims of
        Nothing -> Left composedWebClaimsRejected
        Just subject -> Right (ComposedWebClaims subject)

-- | An API token must carry the API audience, a subject, and a scope claim.
parseComposedApiJwtClaims :: ComposedJwtConfiguration -> Jwt.ClaimsSet -> Either JwtClaimsError ComposedApiClaims
parseComposedApiJwtClaims configuration claims
  | not (claimsCarryAudience (composedApiAudience configuration) claims) = Left composedAudienceMismatch
  | otherwise =
      case previewSubject claims of
        Nothing -> Left composedApiClaimsRejected
        Just subject -> Right (ComposedApiClaims subject (scopeWords claims))

claimsCarryAudience :: Jwt.StringOrURI -> Jwt.ClaimsSet -> Bool
claimsCarryAudience expected claims =
  case claims ^. Jwt.claimAud of
    Nothing -> False
    Just (Jwt.Audience audiences) -> expected `elem` audiences

previewSubject :: Jwt.ClaimsSet -> Maybe Text
previewSubject claims =
  case claims ^. Jwt.claimSub of
    Nothing -> Nothing
    Just subjectOrUri -> preview Jwt.string subjectOrUri

scopeWords :: Jwt.ClaimsSet -> [Text]
scopeWords claims =
  case Map.lookup "scope" (claims ^. Jwt.unregisteredClaims) of
    Just (String scopeText) -> Text.words scopeText
    _ -> []

composedWebClaimsRejected :: JwtClaimsError
composedWebClaimsRejected =
  mkJwtClaimsError (requiredSecurityFailureCodeOrDie "composed.web.claims-rejected")

composedApiClaimsRejected :: JwtClaimsError
composedApiClaimsRejected =
  mkJwtClaimsError (requiredSecurityFailureCodeOrDie "composed.api.claims-rejected")
