-- | Startup-validated issuer, audience, and RS256 key-set configuration for
-- the composed example's two-audience JWT runtime.
module App.Composed.Auth.Runtime
  ( ComposedJwtConfiguration (..),
    ComposedJwtConfigurationError (..),
    ComposedJwtRuntime (..),
    composedJwtApiAudienceText,
    composedJwtIssuerText,
    composedJwtPublicJwkSet,
    loadComposedJwtRuntime,
    mkComposedJwtConfiguration,
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

-- | The composed deployment's immutable issuer identity: one issuer name and
-- two deliberately distinct audience values over one key set.
data ComposedJwtConfiguration = ComposedJwtConfiguration
  { composedJwtIssuer :: Jwt.StringOrURI,
    composedJwtIssuerTextValue :: Text,
    composedWebAudience :: Jwt.StringOrURI,
    composedApiAudience :: Jwt.StringOrURI,
    composedJwtApiAudienceTextValue :: Text,
    composedJwtActiveKeyId :: Text
  }
  deriving (Eq, Show)

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
                composedJwtIssuerTextValue = issuerText,
                composedWebAudience = webAudience,
                composedApiAudience = apiAudience,
                composedJwtApiAudienceTextValue = apiAudienceText,
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
