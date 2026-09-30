-- `verifyClaims` returns jose's fixed 'Crypto.JWT.ClaimsSet', so a caller-
-- supplied custom-claims subtype cannot carry the OAuth scope claim. The
-- supported fallback is 'Crypto.JWT.unregisteredClaims'; another local claims
-- parser would duplicate signature and issuer validation instead of removing
-- the deprecated accessor. Keep this suppression on the token owner only.
{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Issue and verify the composed application's web/API JWTs over one
-- startup-validated runtime, including their audience-specific claim shapes.
module App.Composed.Auth.Token
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
where

import App.Composed.Auth.Runtime
  ( ComposedJwtConfiguration (..),
    ComposedJwtRuntime (..),
  )
import Control.Lens (preview, (#), (&), (.~), (?~), (^.))
import Crypto.JOSE.Header (HeaderParam (..), RequiredProtection (..))
import Crypto.JOSE.JWA.JWS qualified as JwaJws
import Crypto.JOSE.JWS qualified as JoseJws
import Crypto.JWT qualified as Jwt
import Data.Aeson (ToJSON, Value (String))
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
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
    jwtProofVerifierWithClock,
    jwtValidationSettingsWithClockSkew,
    mapJwtSignerError,
    mkJwtAllowedAlgorithms,
    mkJwtClaimsError,
    requiredSecurityFailureCodeOrDie,
    signJwt,
    verifyAuthenticationProof,
  )
import HarchWeb.Authentication qualified as OAuth2
import HarchWeb.Time (UnixTimeNanoseconds, unixTimeNanosecondsValue)

data ComposedJwtIssueError = ComposedJwtIssueFailed
  deriving (Eq, Show)

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
issueComposedWebToken = issueComposedWebTokenWithClock getCurrentTime

-- | Clock-injected web-profile issuer. Its configured lifetime supplies the
-- real expiry instant; skew is added only at verification.
issueComposedWebTokenWithClock :: IO UTCTime -> ComposedJwtRuntime -> Text -> IO (Either ComposedJwtIssueError EncodedJwt)
issueComposedWebTokenWithClock readClock runtime subject = do
  now <- readClock
  let configuration = composedRuntimeConfiguration runtime
      expiresAt = addUTCTime (fromIntegral (composedJwtWebTokenLifetimeSeconds configuration)) now
      signer = composedSigner runtime
      header = composedJwsHeader configuration
      baseClaims =
        Jwt.emptyClaimsSet
          & Jwt.claimIss ?~ composedJwtIssuer configuration
          & Jwt.claimAud ?~ Jwt.Audience [composedWebAudience configuration]
          & Jwt.claimSub ?~ (Jwt.string # subject)
          & Jwt.claimIat ?~ Jwt.NumericDate now
          & Jwt.claimExp ?~ Jwt.NumericDate expiresAt
      claims =
        if composedJwtProvideNotBefore configuration
          then baseClaims & Jwt.claimNbf ?~ Jwt.NumericDate now
          else baseClaims
  signJwt signer header claims

-- | Mint one expiring API-audience token over the shared key set. The caller
-- supplies the durable issuance instants so the client-credentials workflow
-- owns its lifetime policy and can fail closed if addition overflows.
issueComposedApiToken :: ComposedJwtRuntime -> Text -> NonEmpty OAuth2.OAuth2Scope -> UnixTimeNanoseconds -> UnixTimeNanoseconds -> IO (Either ComposedJwtIssueError EncodedJwt)
issueComposedApiToken runtime subject scopes issuedAt expiresAt
  | expiresAt <= issuedAt = pure (Left ComposedJwtIssueFailed)
  | otherwise = signJwt signer header claims
  where
    configuration = composedRuntimeConfiguration runtime
    signer = composedSigner runtime
    header = composedJwsHeader configuration
    baseClaims =
      Jwt.emptyClaimsSet
        & Jwt.claimIss ?~ composedJwtIssuer configuration
        & Jwt.claimAud ?~ Jwt.Audience [composedApiAudience configuration]
        & Jwt.claimSub ?~ (Jwt.string # subject)
        & Jwt.claimIat ?~ numericDate issuedAt
        & Jwt.claimExp ?~ numericDate expiresAt
        & Jwt.unregisteredClaims .~ scopeClaim scopes
    claims =
      if composedJwtProvideNotBefore configuration
        then baseClaims & Jwt.claimNbf ?~ numericDate issuedAt
        else baseClaims

numericDate :: UnixTimeNanoseconds -> Jwt.NumericDate
numericDate instant =
  Jwt.NumericDate
    ( posixSecondsToUTCTime
        (fromIntegral (unixTimeNanosecondsValue instant) / 1000000000)
    )

scopeClaim :: NonEmpty OAuth2.OAuth2Scope -> Map.Map Text Value
scopeClaim scopes = Map.singleton "scope" (String (Text.unwords (OAuth2.oauth2ScopeText <$> NonEmpty.toList scopes)))

composedSigner :: (ToJSON claims) => ComposedJwtRuntime -> JwtSigner ComposedJwtIssueError claims
composedSigner runtime =
  JwtSigner $ \header claims ->
    signJwt (mapJwtSignerError (const ComposedJwtIssueFailed) (joseJwtSigner (composedRuntimeSigningKey runtime))) header claims

composedJwsHeader :: ComposedJwtConfiguration -> JWSHeader RequiredProtection
composedJwsHeader configuration =
  JoseJws.newJWSHeaderProtected JwaJws.RS256
    & JoseJws.kid ?~ HeaderParam RequiredProtection (composedJwtActiveKeyId configuration)

-- | The stable, audience-named rejection for a correctly signed token whose
-- audience belongs to the other profile. The wrong-audience acceptance pins
-- that failure at the claims parser, after jose has verified signature and
-- issuer.
composedAudienceMismatch :: JwtClaimsError
composedAudienceMismatch =
  mkJwtClaimsError (requiredSecurityFailureCodeOrDie "composed.audience-mismatch")

-- | Verify a web-audience token against this runtime's startup-proven keys
-- and issuer, then require the account-web audience by name.
composedWebProofVerifier :: ComposedJwtRuntime -> AuthenticationProofVerifier JwtProof ComposedWebClaims
composedWebProofVerifier runtime =
  composedWebProofVerifierWithAcceptance runtime Nothing (Just (composedReferenceAudienceAcceptance configuration))
  where
    configuration = composedRuntimeConfiguration runtime

-- | Supply pure accepted-value predicates for this runtime. Each predicate
-- replaces its default singleton predicate. The public reference verifier
-- supplies an explicit audience predicate accepting both configured profile
-- audiences, so a correctly signed cross-profile token reaches the stable
-- application-level audience mismatch parser.
composedWebProofVerifierWithAcceptance :: ComposedJwtRuntime -> Maybe (Jwt.StringOrURI -> Bool) -> Maybe (Jwt.StringOrURI -> Bool) -> AuthenticationProofVerifier JwtProof ComposedWebClaims
composedWebProofVerifierWithAcceptance = composedWebProofVerifierWithClock getCurrentTime

-- | Clock-injected form used to prove the composed profile's exact temporal
-- boundaries without sleeps.
composedWebProofVerifierWithClock :: IO UTCTime -> ComposedJwtRuntime -> Maybe (Jwt.StringOrURI -> Bool) -> Maybe (Jwt.StringOrURI -> Bool) -> AuthenticationProofVerifier JwtProof ComposedWebClaims
composedWebProofVerifierWithClock readClock runtime acceptedIssuers acceptedAudiences =
  AuthenticationProofVerifier $ \proof ->
    verifyAuthenticationProof
      ( jwtProofVerifierWithClock
          readClock
          (composedValidationSettings runtime (composedWebAudience configuration) acceptedIssuers acceptedAudiences)
          (composedJwtWebRequiredClaims (composedRuntimeConfiguration runtime))
          (mkJwtAllowedAlgorithms (JwtRs256 :| []))
          (composedRuntimeVerificationKeys runtime)
          (parseComposedWebJwtClaims (composedRuntimeConfiguration runtime))
      )
      (jwtProofEncodedJwt proof)
  where
    configuration = composedRuntimeConfiguration runtime

-- | Verify an API-audience token the same way, then require the API audience
-- by name — so a correctly signed web token fails exactly at the audience.
composedApiProofVerifier :: ComposedJwtRuntime -> AuthenticationProofVerifier JwtProof ComposedApiClaims
composedApiProofVerifier runtime =
  composedApiProofVerifierWithAcceptance runtime Nothing (Just (composedReferenceAudienceAcceptance configuration))
  where
    configuration = composedRuntimeConfiguration runtime

composedApiProofVerifierWithAcceptance :: ComposedJwtRuntime -> Maybe (Jwt.StringOrURI -> Bool) -> Maybe (Jwt.StringOrURI -> Bool) -> AuthenticationProofVerifier JwtProof ComposedApiClaims
composedApiProofVerifierWithAcceptance = composedApiProofVerifierWithClock getCurrentTime

composedApiProofVerifierWithClock :: IO UTCTime -> ComposedJwtRuntime -> Maybe (Jwt.StringOrURI -> Bool) -> Maybe (Jwt.StringOrURI -> Bool) -> AuthenticationProofVerifier JwtProof ComposedApiClaims
composedApiProofVerifierWithClock readClock runtime acceptedIssuers acceptedAudiences =
  AuthenticationProofVerifier $ \proof ->
    verifyAuthenticationProof
      ( jwtProofVerifierWithClock
          readClock
          (composedValidationSettings runtime (composedApiAudience configuration) acceptedIssuers acceptedAudiences)
          (composedJwtApiRequiredClaims (composedRuntimeConfiguration runtime))
          (mkJwtAllowedAlgorithms (JwtRs256 :| []))
          (composedRuntimeVerificationKeys runtime)
          (parseComposedApiJwtClaims (composedRuntimeConfiguration runtime))
      )
      (jwtProofEncodedJwt proof)
  where
    configuration = composedRuntimeConfiguration runtime

composedValidationSettings :: ComposedJwtRuntime -> Jwt.StringOrURI -> Maybe (Jwt.StringOrURI -> Bool) -> Maybe (Jwt.StringOrURI -> Bool) -> Jwt.JWTValidationSettings
composedValidationSettings runtime defaultAudience acceptedIssuers acceptedAudiences =
  jwtValidationSettingsWithClockSkew
    (composedJwtClockSkew configuration)
    ( Jwt.defaultJWTValidationSettings (fromMaybe (== defaultAudience) acceptedAudiences)
        & Jwt.jwtValidationSettingsIssuerPredicate .~ fromMaybe defaultIssuerAcceptance acceptedIssuers
    )
  where
    configuration = composedRuntimeConfiguration runtime
    defaultIssuerAcceptance = (== composedJwtIssuer configuration)

-- | The composed reference app explicitly accepts the two audience values it
-- itself issues at the common JOSE boundary. The profile-specific claim
-- parser then preserves the stable web/API mismatch classification. Custom
-- verifier predicates replace this application predicate entirely.
composedReferenceAudienceAcceptance :: ComposedJwtConfiguration -> Jwt.StringOrURI -> Bool
composedReferenceAudienceAcceptance configuration candidate =
  candidate == composedWebAudience configuration || candidate == composedApiAudience configuration

-- | A web token must carry the account-web audience and a subject.
parseComposedWebJwtClaims :: ComposedJwtConfiguration -> Jwt.ClaimsSet -> Either JwtClaimsError ComposedWebClaims
parseComposedWebJwtClaims configuration claims
  | not (claimsCarryAudience (composedWebAudience configuration) claims) = Left composedAudienceMismatch
  | otherwise =
      case previewSubject claims of
        Nothing -> Left composedWebClaimsRejected
        Just subject -> Right (ComposedWebClaims subject)

-- | An API token must carry the API audience, a subject, and a nonempty,
-- duplicate-free OAuth scope claim. Invalid claims fail closed instead of
-- being normalized into a different authorization value.
parseComposedApiJwtClaims :: ComposedJwtConfiguration -> Jwt.ClaimsSet -> Either JwtClaimsError ComposedApiClaims
parseComposedApiJwtClaims configuration claims
  | not (claimsCarryAudience (composedApiAudience configuration) claims) = Left composedAudienceMismatch
  | otherwise =
      case (previewSubject claims, parseScopeClaim claims) of
        (Just subject, Just scopes) -> Right (ComposedApiClaims subject scopes)
        _ -> Left composedApiClaimsRejected

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

parseScopeClaim :: Jwt.ClaimsSet -> Maybe [Text]
parseScopeClaim claims =
  case Map.lookup "scope" (claims ^. Jwt.unregisteredClaims) of
    Just (String scopeText) ->
      let scopeTexts = Text.splitOn " " scopeText
       in if null scopeTexts || any Text.null scopeTexts || length scopeTexts /= length (nub scopeTexts)
            then Nothing
            else traverse parseScope scopeTexts
    _ -> Nothing
  where
    parseScope value =
      case OAuth2.mkOAuth2Scope value of
        Left _ -> Nothing
        Right scope -> Just (OAuth2.oauth2ScopeText scope)

composedWebClaimsRejected :: JwtClaimsError
composedWebClaimsRejected =
  mkJwtClaimsError (requiredSecurityFailureCodeOrDie "composed.web.claims-rejected")

composedApiClaimsRejected :: JwtClaimsError
composedApiClaimsRejected =
  mkJwtClaimsError (requiredSecurityFailureCodeOrDie "composed.api.claims-rejected")
