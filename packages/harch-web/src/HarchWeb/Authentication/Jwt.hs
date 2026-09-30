-- | The deliberately narrow @jose-0.13@ adapter used by Harch's pluggable
-- authentication pipeline.  JOSE owns compact parsing, signatures, selected
-- key lookup, and standard claim validation; this module owns the explicit
-- four-algorithm allow-list and maps library failures into a safe framework
-- rejection. It also resolves registered-claim presence defaults and checks
-- required presence after the JOSE verifier has validated the signature and
-- every present standard claim. It never widens @jose@'s default algorithm
-- set.
--
-- Decision (configurable registered JWT claims, 2026-09-30): extend this
-- verifier with presence requirements, a validated minutes-based clock skew,
-- and an injectable verification clock instead of adding a second decoder or
-- verifier. Applications still own generation strings and token lifetimes;
-- they resolve their issuance-dependent defaults once and pass the resulting
-- requirements here. The reference web-api and composed profiles use this
-- same verifier; see @docs/design-guidance.md@ and the R3 records in
-- @TASKS/pr-review-2026-09-30.md@.
module HarchWeb.Authentication.Jwt
  ( JWK,
    JWKSet,
    JWSHeader,
    JWTError,
    JWTValidationSettings,
    JwtAlgorithm (..),
    JwtAllowedAlgorithms,
    JwtClaimGeneration (..),
    JwtClaimPresencePolicy (..),
    JwtClaimPresenceRequirement (..),
    JwtClaimsError,
    JwtClockSkew,
    JwtConfigurationError (..),
    JwtRequiredClaims,
    JwtSigner (..),
    RequiredProtection,
    defaultJwtClaimPresencePolicy,
    defaultJwtClockSkew,
    issueJwt,
    jwtClockSkewMinutes,
    joseJwtSigner,
    jwtProofVerifier,
    jwtProofVerifierWithRequiredClaims,
    jwtProofVerifierWithClock,
    jwtRequiredAudience,
    jwtRequiredExpiration,
    jwtRequiredIssuer,
    jwtRequiredNotBefore,
    jwtValidationSettingsWithClockSkew,
    mapJwtSignerError,
    mkJwtAllowedAlgorithms,
    mkJwtClockSkewMinutes,
    mkJwtClaimsError,
    mkJwtStringOrUriMembership,
    resolveJwtRequiredClaims,
  )
where

import Control.Lens (matching, (&), (.~), (^.))
import Crypto.JOSE.Compact qualified as Compact
import Crypto.JOSE.Error qualified as Jose
import Crypto.JOSE.Header (RequiredProtection)
import Crypto.JOSE.JWA.JWS qualified as Jws
import Crypto.JOSE.JWK (JWK, JWKSet)
import Crypto.JOSE.JWS (JWSHeader)
import Crypto.JOSE.JWS qualified as JWS
import Crypto.JWT (ClaimsSet, JWTError, JWTValidationSettings)
import Crypto.JWT qualified as Jwt
import Data.Aeson (ToJSON)
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime, getCurrentTime)
import HarchWeb.Authentication
  ( AuthenticationProofVerifier (..),
    EncodedJwt,
    ProofRejection,
    ProofVerificationFailure (ProofRejected),
    SecurityFailureCode,
    encodedJwtBytes,
    encodedJwtFromBytes,
    mkProofRejection,
  )
import HarchWeb.SecurityFailureCode.Internal (knownSecurityFailureCode)

data JwtAlgorithm
  = JwtHs256
  | JwtHs512
  | JwtRs256
  | JwtRs512
  deriving (Eq, Ord, Show)

newtype JwtAllowedAlgorithms = JwtAllowedAlgorithms (Set.Set Jws.Alg)

-- | A configuration failure for a registered-claim policy value. No rejected
-- value is retained, so callers can log the stable class without logging a
-- configured issuer, audience, or token.
data JwtConfigurationError
  = JwtClockSkewNegative
  | JwtStringOrUriInvalid
  deriving (Eq, Show)

-- | Store an arbitrary-precision, nonnegative minute duration and convert it
-- exactly at the JOSE boundary. This avoids accidental narrowing through
-- 'Int' or a wrapped unsigned type.
newtype JwtClockSkew = JwtClockSkew Integer

instance Eq JwtClockSkew where
  JwtClockSkew leftMinutes == JwtClockSkew rightMinutes = leftMinutes == rightMinutes

instance Show JwtClockSkew where
  showsPrec precedence (JwtClockSkew minutes) =
    showParen (precedence > 10) $
      showString "JwtClockSkew " . showsPrec 11 minutes
  show value = shows value ""

-- | The default uses zero clock tolerance. Deployments may configure an
-- explicit nonnegative minute value.
defaultJwtClockSkew :: JwtClockSkew
defaultJwtClockSkew = JwtClockSkew 0

-- | Read the validated duration in minutes without exposing the internal
-- JOSE representation.
jwtClockSkewMinutes :: JwtClockSkew -> Integer
jwtClockSkewMinutes (JwtClockSkew minutes) = minutes

-- | Construct a skew from its external minute unit. Zero is valid and all
-- nonnegative 'Integer' values remain exact; a negative value fails instead
-- of being absolutized or reinterpreted as an unsigned duration.
mkJwtClockSkewMinutes :: Integer -> Either JwtConfigurationError JwtClockSkew
mkJwtClockSkewMinutes minutes
  | minutes < 0 = Left JwtClockSkewNegative
  | otherwise = Right (JwtClockSkew minutes)

-- | Apply the already-validated skew to JOSE's exact validation-settings
-- value. jose performs both exp and nbf comparison with this one allowance.
jwtValidationSettingsWithClockSkew :: JwtClockSkew -> JWTValidationSettings -> JWTValidationSettings
jwtValidationSettingsWithClockSkew (JwtClockSkew minutes) =
  Jwt.allowedSkew .~ fromInteger (minutes * 60)

-- | Parse a configured set once and build its pure membership predicate.
-- URI-shaped values use jose's own @stringOrUri@ parser, so they compare equal
-- to the decoded claim representation. An empty set is valid and rejects all
-- present values; callers can still allow the corresponding claim to be
-- absent through 'JwtClaimPresencePolicy'.
mkJwtStringOrUriMembership :: (Foldable collection) => collection Text -> Either JwtConfigurationError (Jwt.StringOrURI -> Bool)
mkJwtStringOrUriMembership configuredValues = do
  acceptedValues <- traverse parseConfiguredValue (toList configuredValues)
  pure (`elem` acceptedValues)
  where
    parseConfiguredValue value
      | Text.null value = Left JwtStringOrUriInvalid
      | otherwise =
          case matching Jwt.stringOrUri (Text.unpack value) of
            Left _ -> Left JwtStringOrUriInvalid
            Right parsedValue -> Right parsedValue

-- | Whether a profile emits each registered claim. Expiration does not use
-- this record because its default presence requirement is unconditional.
data JwtClaimGeneration = JwtClaimGeneration
  { jwtGenerationEmitsNotBefore :: Bool,
    jwtGenerationConfiguresIssuer :: Bool,
    jwtGenerationConfiguresAudience :: Bool
  }

instance Eq JwtClaimGeneration where
  leftGeneration == rightGeneration =
    ( jwtGenerationEmitsNotBefore leftGeneration,
      jwtGenerationConfiguresIssuer leftGeneration,
      jwtGenerationConfiguresAudience leftGeneration
    )
      == ( jwtGenerationEmitsNotBefore rightGeneration,
           jwtGenerationConfiguresIssuer rightGeneration,
           jwtGenerationConfiguresAudience rightGeneration
         )

instance Show JwtClaimGeneration where
  showsPrec precedence generation =
    showParen (precedence > 10) $
      showString "JwtClaimGeneration {jwtGenerationEmitsNotBefore = "
        . shows (jwtGenerationEmitsNotBefore generation)
        . showString ", jwtGenerationConfiguresIssuer = "
        . shows (jwtGenerationConfiguresIssuer generation)
        . showString ", jwtGenerationConfiguresAudience = "
        . shows (jwtGenerationConfiguresAudience generation)
        . showString "}"
  show generation = shows generation ""

-- | The unresolved claim-presence setting. The default follows the profile's
-- actual generation choice; explicit constructors remain independent per
-- claim.
data JwtClaimPresenceRequirement
  = UseIssuanceDefault
  | RequirePresence
  | AllowAbsence

instance Eq JwtClaimPresenceRequirement where
  UseIssuanceDefault == UseIssuanceDefault = True
  RequirePresence == RequirePresence = True
  AllowAbsence == AllowAbsence = True
  _ == _ = False

instance Show JwtClaimPresenceRequirement where
  showsPrec _ UseIssuanceDefault = showString "UseIssuanceDefault"
  showsPrec _ RequirePresence = showString "RequirePresence"
  showsPrec _ AllowAbsence = showString "AllowAbsence"
  show UseIssuanceDefault = "UseIssuanceDefault"
  show RequirePresence = "RequirePresence"
  show AllowAbsence = "AllowAbsence"

-- | Independent presence settings for the four registered claims. Choosing
-- 'AllowAbsence' skips only the missing-value rejection: JOSE continues to
-- validate a present claim.
data JwtClaimPresencePolicy = JwtClaimPresencePolicy
  { jwtExpirationPresence :: JwtClaimPresenceRequirement,
    jwtNotBeforePresence :: JwtClaimPresenceRequirement,
    jwtIssuerPresence :: JwtClaimPresenceRequirement,
    jwtAudiencePresence :: JwtClaimPresenceRequirement
  }

instance Eq JwtClaimPresencePolicy where
  leftPolicy == rightPolicy =
    ( jwtExpirationPresence leftPolicy,
      jwtNotBeforePresence leftPolicy,
      jwtIssuerPresence leftPolicy,
      jwtAudiencePresence leftPolicy
    )
      == ( jwtExpirationPresence rightPolicy,
           jwtNotBeforePresence rightPolicy,
           jwtIssuerPresence rightPolicy,
           jwtAudiencePresence rightPolicy
         )

instance Show JwtClaimPresencePolicy where
  showsPrec precedence policy =
    showParen (precedence > 10) $
      showString "JwtClaimPresencePolicy {jwtExpirationPresence = "
        . shows (jwtExpirationPresence policy)
        . showString ", jwtNotBeforePresence = "
        . shows (jwtNotBeforePresence policy)
        . showString ", jwtIssuerPresence = "
        . shows (jwtIssuerPresence policy)
        . showString ", jwtAudiencePresence = "
        . shows (jwtAudiencePresence policy)
        . showString "}"
  show policy = shows policy ""

-- | Resolved requirements consumed by the verifier. Construct these with
-- 'resolveJwtRequiredClaims' so issuance-dependent defaults cannot be
-- confused with explicit overrides.
data JwtRequiredClaims = JwtRequiredClaims
  { jwtRequiredExpiration :: Bool,
    jwtRequiredNotBefore :: Bool,
    jwtRequiredIssuer :: Bool,
    jwtRequiredAudience :: Bool
  }

instance Eq JwtRequiredClaims where
  leftRequirements == rightRequirements =
    ( jwtRequiredExpiration leftRequirements,
      jwtRequiredNotBefore leftRequirements,
      jwtRequiredIssuer leftRequirements,
      jwtRequiredAudience leftRequirements
    )
      == ( jwtRequiredExpiration rightRequirements,
           jwtRequiredNotBefore rightRequirements,
           jwtRequiredIssuer rightRequirements,
           jwtRequiredAudience rightRequirements
         )

instance Show JwtRequiredClaims where
  showsPrec precedence requirements =
    showParen (precedence > 10) $
      showString "JwtRequiredClaims {jwtRequiredExpiration = "
        . shows (jwtRequiredExpiration requirements)
        . showString ", jwtRequiredNotBefore = "
        . shows (jwtRequiredNotBefore requirements)
        . showString ", jwtRequiredIssuer = "
        . shows (jwtRequiredIssuer requirements)
        . showString ", jwtRequiredAudience = "
        . shows (jwtRequiredAudience requirements)
        . showString "}"
  show requirements = shows requirements ""

-- | Expiration is required for every profile by default. The other defaults
-- follow emitted/configured claims and are resolved by
-- 'resolveJwtRequiredClaims'.
defaultJwtClaimPresencePolicy :: JwtClaimPresencePolicy
defaultJwtClaimPresencePolicy =
  JwtClaimPresencePolicy
    { jwtExpirationPresence = UseIssuanceDefault,
      jwtNotBeforePresence = UseIssuanceDefault,
      jwtIssuerPresence = UseIssuanceDefault,
      jwtAudiencePresence = UseIssuanceDefault
    }

-- | Resolve the four independent defaults from this profile's generation
-- contract. Expiration defaults to required even when a profile emits no
-- expiry; such a profile must explicitly select 'AllowAbsence' or start
-- generating exp. The remaining claims default to the corresponding issuance
-- choice.
resolveJwtRequiredClaims :: JwtClaimGeneration -> JwtClaimPresencePolicy -> JwtRequiredClaims
resolveJwtRequiredClaims generation policy =
  JwtRequiredClaims
    { jwtRequiredExpiration = resolvePresence True (jwtExpirationPresence policy),
      jwtRequiredNotBefore = resolvePresence (jwtGenerationEmitsNotBefore generation) (jwtNotBeforePresence policy),
      jwtRequiredIssuer = resolvePresence (jwtGenerationConfiguresIssuer generation) (jwtIssuerPresence policy),
      jwtRequiredAudience = resolvePresence (jwtGenerationConfiguresAudience generation) (jwtAudiencePresence policy)
    }

resolvePresence :: Bool -> JwtClaimPresenceRequirement -> Bool
resolvePresence issuanceDefault requirement =
  case requirement of
    UseIssuanceDefault -> issuanceDefault
    RequirePresence -> True
    AllowAbsence -> False

newtype JwtClaimsError = JwtClaimsError SecurityFailureCode
  deriving (Eq, Show)

-- | A pluggable compact-JWT signing capability. The error type stays chosen
-- by the application boundary, while Harch's default adapter retains JOSE's
-- precise error type.
newtype JwtSigner signingError claims = JwtSigner
  { signJwt :: JWSHeader RequiredProtection -> claims -> IO (Either signingError EncodedJwt)
  }

-- | Adapt only a signer's private failure value without changing its headers,
-- claims, or issued proof.
mapJwtSignerError :: (sourceError -> targetError) -> JwtSigner sourceError claims -> JwtSigner targetError claims
mapJwtSignerError mapError signer =
  JwtSigner $ \header claims ->
    first mapError <$> signJwt signer header claims

-- | Harch's default JOSE-backed signer for a deployment-owned key.
joseJwtSigner :: (ToJSON claims) => JWK -> JwtSigner JWTError claims
joseJwtSigner key = JwtSigner (issueJwt key)

mkJwtClaimsError :: SecurityFailureCode -> JwtClaimsError
mkJwtClaimsError = JwtClaimsError

-- | Construct the verifier allow-list from the only algorithms Harch's
-- regression matrix supports.  The @None@ algorithm, every unselected
-- algorithm, and the library's broad default are therefore impossible here.
mkJwtAllowedAlgorithms :: NonEmpty JwtAlgorithm -> JwtAllowedAlgorithms
mkJwtAllowedAlgorithms = JwtAllowedAlgorithms . Set.fromList . map toJoseAlgorithm . NonEmpty.toList

toJoseAlgorithm :: JwtAlgorithm -> Jws.Alg
toJoseAlgorithm algorithm =
  case algorithm of
    JwtHs256 -> Jws.HS256
    JwtHs512 -> Jws.HS512
    JwtRs256 -> Jws.RS256
    JwtRs512 -> Jws.RS512

-- | Verify a compact JWT with Harch's generic defaults: expiration is
-- required, while not-before, issuer, and audience presence remain optional
-- until an application supplies its issuance policy. Signature, algorithm,
-- present standard-claim, and required-presence failures use Harch's fixed
-- rejection code.
jwtProofVerifier :: JWTValidationSettings -> JwtAllowedAlgorithms -> JWKSet -> (ClaimsSet -> Either JwtClaimsError verified) -> AuthenticationProofVerifier EncodedJwt verified
jwtProofVerifier validationSettings =
  jwtProofVerifierWithClock getCurrentTime validationSettings genericDefaultRequiredClaims

-- | Verify a compact JWT with the application's already-resolved presence
-- policy and the system clock. A projection failure retains its validated
-- application failure code so observability can distinguish a malformed
-- subject from a malformed session without retaining rejected claim values.
jwtProofVerifierWithRequiredClaims :: JWTValidationSettings -> JwtRequiredClaims -> JwtAllowedAlgorithms -> JWKSet -> (ClaimsSet -> Either JwtClaimsError verified) -> AuthenticationProofVerifier EncodedJwt verified
jwtProofVerifierWithRequiredClaims = jwtProofVerifierWithClock getCurrentTime

-- | The same single verifier with its validation clock supplied by the
-- existing runtime/dependency boundary. This lets tests prove exact exp/nbf
-- edges deterministically while production delegates to 'getCurrentTime'.
jwtProofVerifierWithClock :: IO UTCTime -> JWTValidationSettings -> JwtRequiredClaims -> JwtAllowedAlgorithms -> JWKSet -> (ClaimsSet -> Either JwtClaimsError verified) -> AuthenticationProofVerifier EncodedJwt verified
jwtProofVerifierWithClock readClock validationSettings requiredClaims allowedAlgorithms keySet claimsProjection =
  AuthenticationProofVerifier $ \encodedJwt -> do
    verificationTime <- readClock
    verificationResult <- Jose.runJOSE $ do
      signedJwt <- Compact.decodeCompact (LazyByteString.fromStrict (encodedJwtBytes encodedJwt)) :: Jose.JOSE JWTError IO Jwt.SignedJWT
      Jwt.verifyClaimsAt (withAllowedAlgorithms allowedAlgorithms validationSettings) keySet verificationTime signedJwt
    pure $
      case verificationResult of
        Left _ -> Left (ProofRejected rejectedJwtProof)
        Right claimsSet
          | not (requiredClaimsArePresent requiredClaims claimsSet) -> Left (ProofRejected rejectedJwtProof)
          | otherwise ->
              case claimsProjection claimsSet of
                Left (JwtClaimsError failureCode) -> Left (ProofRejected (mkProofRejection failureCode))
                Right verified -> Right verified

genericDefaultRequiredClaims :: JwtRequiredClaims
genericDefaultRequiredClaims =
  resolveJwtRequiredClaims
    (JwtClaimGeneration False False False)
    defaultJwtClaimPresencePolicy

requiredClaimsArePresent :: JwtRequiredClaims -> ClaimsSet -> Bool
requiredClaimsArePresent requiredClaims claimsSet =
  (not (jwtRequiredExpiration requiredClaims) || isPresent (claimsSet ^. Jwt.claimExp))
    && (not (jwtRequiredNotBefore requiredClaims) || isPresent (claimsSet ^. Jwt.claimNbf))
    && (not (jwtRequiredIssuer requiredClaims) || isPresent (claimsSet ^. Jwt.claimIss))
    && (not (jwtRequiredAudience requiredClaims) || isPresent (claimsSet ^. Jwt.claimAud))
  where
    isPresent maybeValue =
      case maybeValue of
        Nothing -> False
        Just _ -> True

-- | Issue exactly the claims the application supplies. In particular this
-- adapter does not manufacture @iat@, @nbf@, @exp@, issuer, or audience.
issueJwt :: (ToJSON claims) => JWK -> JWSHeader RequiredProtection -> claims -> IO (Either JWTError EncodedJwt)
issueJwt key header claims = do
  signedJwtResult <- Jose.runJOSE (Jwt.signJWT key header claims :: Jose.JOSE JWTError IO Jwt.SignedJWT)
  pure (fmap (encodedJwtFromBytes . LazyByteString.toStrict . Compact.encodeCompact) signedJwtResult)

withAllowedAlgorithms :: JwtAllowedAlgorithms -> JWTValidationSettings -> JWTValidationSettings
withAllowedAlgorithms (JwtAllowedAlgorithms allowedAlgorithms) validationSettings =
  validationSettings
    & Jwt.jwtValidationSettingsValidationSettings
      . JWS.algorithms
      .~ allowedAlgorithms

rejectedJwtProof :: ProofRejection
rejectedJwtProof = mkProofRejection (knownSecurityFailureCode "jwt.rejected")
