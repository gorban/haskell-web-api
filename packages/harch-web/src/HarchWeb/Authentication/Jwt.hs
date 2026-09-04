{-# LANGUAGE OverloadedStrings #-}

-- | The deliberately narrow @jose-0.12@ adapter used by Harch's pluggable
-- authentication pipeline.  JOSE owns compact parsing, signatures, selected
-- key lookup, and standard claim validation; this module owns the explicit
-- four-algorithm allow-list and maps library failures into a safe framework
-- rejection.  It never widens @jose@'s default algorithm set.
module HarchWeb.Authentication.Jwt
  ( JWK,
    JWKSet,
    JWSHeader,
    JWTError,
    JWTValidationSettings,
    JwtAlgorithm (..),
    JwtAllowedAlgorithms,
    JwtClaimsError,
    RequiredProtection,
    issueJwt,
    jwtProofVerifier,
    mkJwtAllowedAlgorithms,
    mkJwtClaimsError,
  )
where

import Control.Lens ((&), (.~))
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
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set qualified as Set
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

newtype JwtClaimsError = JwtClaimsError SecurityFailureCode
  deriving (Eq, Show)

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

-- | Verify a compact JWT and project the application-owned claims. Signature,
-- algorithm, and standard-claim failures use Harch's fixed rejection code;
-- a projection failure retains its validated application failure code so
-- observability can distinguish a malformed subject from a malformed session
-- without retaining the rejected claim values.
jwtProofVerifier :: JWTValidationSettings -> JwtAllowedAlgorithms -> JWKSet -> (ClaimsSet -> Either JwtClaimsError verified) -> AuthenticationProofVerifier EncodedJwt verified
jwtProofVerifier validationSettings allowedAlgorithms keySet claimsProjection =
  AuthenticationProofVerifier $ \encodedJwt -> do
    verificationResult <- Jose.runJOSE $ do
      signedJwt <- Compact.decodeCompact (LazyByteString.fromStrict (encodedJwtBytes encodedJwt)) :: Jose.JOSE JWTError IO Jwt.SignedJWT
      Jwt.verifyClaims (withAllowedAlgorithms allowedAlgorithms validationSettings) keySet signedJwt
    pure $
      case verificationResult of
        Left _ -> Left (ProofRejected rejectedJwtProof)
        Right claimsSet ->
          case claimsProjection claimsSet of
            Left (JwtClaimsError failureCode) -> Left (ProofRejected (mkProofRejection failureCode))
            Right verified -> Right verified

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
