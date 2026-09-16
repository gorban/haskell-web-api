{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Crypto.JOSE.JWA.JWK qualified as JwaJwk
import Crypto.JOSE.JWA.JWS qualified as JwaJws
import Crypto.JOSE.JWK qualified as JoseJwk
import Crypto.JOSE.JWS qualified as JoseJws
import Crypto.JWT qualified as Jwt
import Data.Either (fromRight)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb

spec =
  describe "HarchWeb.Authentication.Jwt" $ do
    it "verifies HS256 and HS512 only when their exact allow-list admits them" $ do
      hs256Key <- JoseJwk.genJWK (JwaJwk.OctGenParam 64)
      hs512Key <- JoseJwk.genJWK (JwaJwk.OctGenParam 64)
      assertAcceptedAndRejected JwtHs256 JwaJws.HS256 hs256Key
      assertAcceptedAndRejected JwtHs512 JwaJws.HS512 hs512Key

    it "verifies RS256 and RS512 only when their exact allow-list admits them" $ do
      rsaKey <- JoseJwk.genJWK (JwaJwk.RSAGenParam 2048)
      assertAcceptedAndRejected JwtRs256 JwaJws.RS256 rsaKey
      assertAcceptedAndRejected JwtRs512 JwaJws.RS512 rsaKey

    it "rejects wrong keys, none-style compact input, and failed claim projection" $ do
      signingKey <- JoseJwk.genJWK (JwaJwk.OctGenParam 64)
      wrongKey <- JoseJwk.genJWK (JwaJwk.OctGenParam 64)
      token <- issueTestJwt JwaJws.HS256 signingKey
      let allowed = mkJwtAllowedAlgorithms (JwtHs256 :| [])
          verifier = jwtProofVerifier validationSettings allowed (JoseJwk.JWKSet [wrongKey]) (Right . show)
          rejectedProjection = jwtProofVerifier validationSettings allowed (JoseJwk.JWKSet [signingKey]) (const (Left (mkJwtClaimsError (requiredFailureCode "jwt.claims-rejected")) :: Either JwtClaimsError ()))
          noneToken = encodedJwtFromBytes "eyJhbGciOiJub25lIn0.eyJzdWIiOiJhZGEifQ."
      expectAll
        ( (verifyAuthenticationProof verifier token `shouldReturn` Left (ProofRejected (mkProofRejection (requiredFailureCode "jwt.rejected"))))
            :| [ verifyAuthenticationProof verifier noneToken `shouldReturn` Left (ProofRejected (mkProofRejection (requiredFailureCode "jwt.rejected"))),
                 verifyAuthenticationProof rejectedProjection token `shouldReturn` Left (ProofRejected (mkProofRejection (requiredFailureCode "jwt.claims-rejected")))
               ]
        )

    it "keeps the supported algorithm and claim-error vocabulary closed" $ do
      let algorithms = [JwtHs256, JwtHs512, JwtRs256, JwtRs512]
          claimError = mkJwtClaimsError (requiredFailureCode "jwt.claims-rejected")
      expectAll
        ( (hasDerivedContract algorithms `shouldBe` True)
            :| [ compare JwtHs256 JwtHs256 `shouldBe` EQ,
                 compare JwtHs256 JwtHs512 `shouldBe` LT,
                 compare JwtRs512 JwtRs256 `shouldBe` GT,
                 JwtHs256 < JwtHs512 `shouldBe` True,
                 JwtHs256 <= JwtHs256 `shouldBe` True,
                 JwtRs512 > JwtRs256 `shouldBe` True,
                 JwtRs512 >= JwtRs512 `shouldBe` True,
                 max JwtHs256 JwtHs512 `shouldBe` JwtHs512,
                 min JwtHs256 JwtHs512 `shouldBe` JwtHs256,
                 claimError `shouldBe` claimError,
                 hasDerivedContract [claimError] `shouldBe` True
               ]
        )

    it "adapts signer failures without changing an issued compact proof" $ do
      let header = JoseJws.newJWSHeaderProtected JwaJws.HS256
          signingFailure = ("signing failed" :: Text)
          rejectedSigner = JwtSigner (\_ _ -> pure (Left ("kms unavailable" :: Text)))
      rejected <- signJwt (mapJwtSignerError (const signingFailure) rejectedSigner) header Jwt.emptyClaimsSet
      case rejected of
        Left failure -> failure `shouldBe` signingFailure
        Right _ -> expectationFailure "expected the adapted signer to retain its failure"
      signingKey <- JoseJwk.genJWK (JwaJwk.OctGenParam 64)
      issued <- signJwt (mapJwtSignerError (const signingFailure) (joseJwtSigner signingKey)) header Jwt.emptyClaimsSet
      case issued of
        Right token ->
          verifyAuthenticationProof
            (jwtProofVerifier validationSettings (mkJwtAllowedAlgorithms (JwtHs256 :| [])) (JoseJwk.JWKSet [signingKey]) (Right . show))
            token
            `shouldReturn` Right (show Jwt.emptyClaimsSet)
        Left failure -> expectationFailure ("expected the adapted JOSE signer to retain its proof: " <> Text.unpack failure)

assertAcceptedAndRejected :: JwtAlgorithm -> JwaJws.Alg -> JWK -> IO ()
assertAcceptedAndRejected algorithm joseAlgorithm key = do
  token <- issueTestJwt joseAlgorithm key
  let accepted = jwtProofVerifier validationSettings (mkJwtAllowedAlgorithms (algorithm :| [])) (JoseJwk.JWKSet [key]) (Right . show)
      rejected = jwtProofVerifier validationSettings (mkJwtAllowedAlgorithms (otherAlgorithm algorithm :| [])) (JoseJwk.JWKSet [key]) (Right . show)
  expectAll
    ( (verifyAuthenticationProof accepted token `shouldReturn` Right (show Jwt.emptyClaimsSet))
        :| [ verifyAuthenticationProof rejected token `shouldReturn` Left (ProofRejected (mkProofRejection (requiredFailureCode "jwt.rejected")))
           ]
    )

issueTestJwt :: JwaJws.Alg -> JWK -> IO EncodedJwt
issueTestJwt joseAlgorithm key = do
  issued <- issueJwt key (JoseJws.newJWSHeaderProtected joseAlgorithm) Jwt.emptyClaimsSet
  case issued of
    Right token -> pure token
    Left jwtError -> expectationFailure ("test JWT issuance failed: " <> show jwtError) >> error "unreachable"

validationSettings :: JWTValidationSettings
validationSettings = Jwt.defaultJWTValidationSettings (const True)

otherAlgorithm :: JwtAlgorithm -> JwtAlgorithm
otherAlgorithm algorithm =
  case algorithm of
    JwtHs256 -> JwtHs512
    JwtHs512 -> JwtHs256
    JwtRs256 -> JwtRs512
    JwtRs512 -> JwtRs256

requiredFailureCode :: Text -> SecurityFailureCode
requiredFailureCode failureCodeValue = fromRight (error "invalid failure code") (mkSecurityFailureCode failureCodeValue)

hasDerivedContract :: (Eq value, Show value) => [value] -> Bool
hasDerivedContract values =
  sum [fromEnum (left == right) | left <- values, right <- values] == length values
    && sum [fromEnum (left /= right) | left <- values, right <- values]
      == length values * (length values - 1)
    && sum [length (show item) + length (showList [item] "") | item <- values] > 0
