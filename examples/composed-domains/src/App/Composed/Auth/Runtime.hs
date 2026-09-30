-- | Startup-validated issuer, audience, registered-claim policy, and RS256
-- key-set configuration for the composed example's two-audience JWT runtime.
-- Web and API presence policies resolve independently. Both token profiles
-- generate expiry; the web profile's one-hour lifetime and the minute-based
-- verification skew are explicit settings, and default-on @nbf@ generation
-- feeds the unresolved policy defaults.
module App.Composed.Auth.Runtime
  ( ComposedJwtConfiguration (..),
    ComposedJwtConfigurationError (..),
    ComposedJwtPolicySettings (..),
    ComposedJwtRuntime (..),
    composedJwtApiAudienceText,
    composedJwtIssuerText,
    composedJwtPublicJwkSet,
    defaultComposedJwtPolicySettings,
    loadComposedJwtRuntime,
    mkComposedJwtConfiguration,
    mkComposedJwtConfigurationWithPolicy,
  )
where

import Control.Lens (matching, (&), (?~), (^.))
import Crypto.JOSE.JWA.JWK qualified as JwaJwk
import Crypto.JOSE.JWA.JWS qualified as JwaJws
import Crypto.JOSE.JWK (JWK, JWKSet (..))
import Crypto.JOSE.JWK qualified as JoseJwk
import Crypto.JWT qualified as Jwt
import Data.List (nub)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import HarchWeb qualified

-- | The composed deployment's immutable issuer identity: one explicit issuer
-- and two deliberately distinct audience values over one key set. It also
-- retains separately resolved presence requirements for the web and API
-- profiles; each profile's expiry requirement matches its generated lifetime.
data ComposedJwtConfiguration = ComposedJwtConfiguration
  { composedJwtIssuer :: Jwt.StringOrURI,
    composedJwtIssuerTextValue :: Text,
    composedWebAudience :: Jwt.StringOrURI,
    composedApiAudience :: Jwt.StringOrURI,
    composedJwtApiAudienceTextValue :: Text,
    composedJwtActiveKeyId :: Text,
    composedJwtWebTokenLifetimeSeconds :: Word64,
    composedJwtProvideNotBefore :: Bool,
    composedJwtWebRequiredClaims :: HarchWeb.JwtRequiredClaims,
    composedJwtApiRequiredClaims :: HarchWeb.JwtRequiredClaims,
    composedJwtClockSkew :: HarchWeb.JwtClockSkew
  }
  deriving (Eq, Show)

-- | The reference profile keeps separate presence overrides for the web and
-- API token contracts. Both profiles emit @exp@: web tokens use their
-- explicitly configured lifetime, API tokens use the client-credentials
-- lifetime. Both profiles inherit default-on @nbf@.
data ComposedJwtPolicySettings = ComposedJwtPolicySettings
  { composedPolicyProvideNotBefore :: Bool,
    composedPolicyWebTokenLifetimeSeconds :: Word64,
    composedWebPresencePolicy :: HarchWeb.JwtClaimPresencePolicy,
    composedApiPresencePolicy :: HarchWeb.JwtClaimPresencePolicy,
    composedPolicyClockSkewMinutes :: Integer
  }
  deriving (Eq, Show)

defaultComposedJwtPolicySettings :: ComposedJwtPolicySettings
defaultComposedJwtPolicySettings =
  ComposedJwtPolicySettings
    { composedPolicyProvideNotBefore = True,
      composedPolicyWebTokenLifetimeSeconds = 3600,
      composedWebPresencePolicy = HarchWeb.defaultJwtClaimPresencePolicy,
      composedApiPresencePolicy = HarchWeb.defaultJwtClaimPresencePolicy,
      composedPolicyClockSkewMinutes = 0
    }

-- | Deployment settings and verification keys that failed startup validation.
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
  | ComposedJwtClockSkewInvalid
  | ComposedJwtWebTokenLifetimeInvalid
  deriving (Eq, Show)

mkComposedJwtConfiguration :: Text -> Text -> Text -> Text -> Either ComposedJwtConfigurationError ComposedJwtConfiguration
mkComposedJwtConfiguration = mkComposedJwtConfigurationWithPolicy defaultComposedJwtPolicySettings

-- | Validate explicit discovery/resource identities and resolve both token
-- profiles' generation-dependent defaults in one startup configuration.
-- Caller-supplied acceptance predicates are supplied to the verifier and
-- replace the default functions; they are not stored or unioned here.
mkComposedJwtConfigurationWithPolicy :: ComposedJwtPolicySettings -> Text -> Text -> Text -> Text -> Either ComposedJwtConfigurationError ComposedJwtConfiguration
mkComposedJwtConfigurationWithPolicy policySettings issuerText webAudienceText apiAudienceText activeKeyId = do
  issuer <- requireConfiguredIdentity ComposedJwtIssuerEmpty ComposedJwtIssuerUnparseable issuerText
  webAudience <- requireConfiguredIdentity ComposedJwtWebAudienceEmpty ComposedJwtWebAudienceUnparseable webAudienceText
  apiAudience <- requireConfiguredIdentity ComposedJwtApiAudienceEmpty ComposedJwtApiAudienceUnparseable apiAudienceText
  if webAudience == apiAudience
    then Left ComposedJwtAudiencesNotDistinct
    else Right ()
  requireNonEmptyText ComposedJwtActiveKeyIdEmpty activeKeyId
  if composedPolicyWebTokenLifetimeSeconds policySettings == 0
    then Left ComposedJwtWebTokenLifetimeInvalid
    else Right ()
  clockSkew <-
    case HarchWeb.mkJwtClockSkewMinutes (composedPolicyClockSkewMinutes policySettings) of
      Left _ -> Left ComposedJwtClockSkewInvalid
      Right value -> Right value
  let profileGeneration =
        HarchWeb.JwtClaimGeneration
          { HarchWeb.jwtGenerationEmitsNotBefore = composedPolicyProvideNotBefore policySettings,
            HarchWeb.jwtGenerationConfiguresIssuer = True,
            HarchWeb.jwtGenerationConfiguresAudience = True
          }
      webRequiredClaims = HarchWeb.resolveJwtRequiredClaims profileGeneration (composedWebPresencePolicy policySettings)
      apiRequiredClaims = HarchWeb.resolveJwtRequiredClaims profileGeneration (composedApiPresencePolicy policySettings)
  Right
    ComposedJwtConfiguration
      { composedJwtIssuer = issuer,
        composedJwtIssuerTextValue = issuerText,
        composedWebAudience = webAudience,
        composedApiAudience = apiAudience,
        composedJwtApiAudienceTextValue = apiAudienceText,
        composedJwtActiveKeyId = activeKeyId,
        composedJwtWebTokenLifetimeSeconds = composedPolicyWebTokenLifetimeSeconds policySettings,
        composedJwtProvideNotBefore = composedPolicyProvideNotBefore policySettings,
        composedJwtWebRequiredClaims = webRequiredClaims,
        composedJwtApiRequiredClaims = apiRequiredClaims,
        composedJwtClockSkew = clockSkew
      }

requireConfiguredIdentity :: ComposedJwtConfigurationError -> ComposedJwtConfigurationError -> Text -> Either ComposedJwtConfigurationError Jwt.StringOrURI
requireConfiguredIdentity emptyError parseError value
  | Text.null value = Left emptyError
  | otherwise = maybe (Left parseError) Right (normalizedStringOrUri value)

requireNonEmptyText :: ComposedJwtConfigurationError -> Text -> Either ComposedJwtConfigurationError ()
requireNonEmptyText errorValue value
  | Text.null value = Left errorValue
  | otherwise = Right ()

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

instance Show ComposedJwtRuntime where
  showsPrec depth _ = showParen (depth > 10) (showString "ComposedJwtRuntime <redacted>")

-- | Startup validation fails closed: the verification set must contain the
-- signing key's own material under the active key id, so every minted token
-- is verifiable by this runtime's own set and a verification key that does
-- not match the signing key is a deployment error.
loadComposedJwtRuntime :: ComposedJwtConfiguration -> JWK -> JWKSet -> Either ComposedJwtConfigurationError ComposedJwtRuntime
loadComposedJwtRuntime configuration signingKey (JWKSet candidates) = do
  publicKeys <- maybe (Left ComposedJwtVerificationKeyMismatch) Right (traverse publicVerificationKey candidates)
  let keyIds = fmap viewKeyId publicKeys
      publicVerificationKeys = JWKSet publicKeys
  if length keyIds /= length (nub keyIds)
    then Left ComposedJwtVerificationKeyMismatch
    else
      if any verificationKeyMatches candidates
        then
          Right
            ComposedJwtRuntime
              { composedRuntimeConfiguration = configuration,
                composedRuntimeSigningKey = signingKey,
                composedRuntimeVerificationKeys = publicVerificationKeys
              }
        else Left ComposedJwtVerificationKeyMismatch
  where
    activeKeyId = composedJwtActiveKeyId configuration
    verificationKeyMatches candidate =
      candidate ^. JoseJwk.jwkKid == Just activeKeyId && sameRsaPublicMaterial candidate signingKey
    viewKeyId key = key ^. JoseJwk.jwkKid

-- | Metadata uses the exact issuer and API audience that token issuance and
-- verification use. The key set returned here has already been reduced to
-- public RSA material during 'loadComposedJwtRuntime'.
composedJwtIssuerText :: ComposedJwtRuntime -> Text
composedJwtIssuerText = composedJwtIssuerTextValue . composedRuntimeConfiguration

composedJwtApiAudienceText :: ComposedJwtRuntime -> Text
composedJwtApiAudienceText = composedJwtApiAudienceTextValue . composedRuntimeConfiguration

composedJwtPublicJwkSet :: ComposedJwtRuntime -> JWKSet
composedJwtPublicJwkSet = composedRuntimeVerificationKeys

publicVerificationKey :: JWK -> Maybe JWK
publicVerificationKey key = do
  keyId <- key ^. JoseJwk.jwkKid
  if Text.null keyId
    then Nothing
    else case key ^. JoseJwk.jwkMaterial of
      JwaJwk.RSAKeyMaterial rsaParameters ->
        Just
          ( JoseJwk.fromRSAPublic (JwaJwk.rsaPublicKey rsaParameters)
              & JoseJwk.jwkKid ?~ keyId
              & JoseJwk.jwkUse ?~ JoseJwk.Sig
              & JoseJwk.jwkAlg ?~ JoseJwk.JWSAlg JwaJws.RS256
          )
      _ -> Nothing

sameRsaPublicMaterial :: JWK -> JWK -> Bool
sameRsaPublicMaterial left right =
  case (left ^. JoseJwk.jwkMaterial, right ^. JoseJwk.jwkMaterial) of
    (JwaJwk.RSAKeyMaterial leftRsa, JwaJwk.RSAKeyMaterial rightRsa) ->
      leftRsa ^. JwaJwk.rsaN == rightRsa ^. JwaJwk.rsaN
        && leftRsa ^. JwaJwk.rsaE == rightRsa ^. JwaJwk.rsaE
    _ -> False
