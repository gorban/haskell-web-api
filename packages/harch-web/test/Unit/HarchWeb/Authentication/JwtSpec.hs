{-# SPEC #-}

import Control.Lens (matching, view, (&), (.~), (?~))
import Crypto.JOSE.JWA.JWK qualified as JwaJwk
import Crypto.JOSE.JWA.JWS qualified as JwaJws
import Crypto.JOSE.JWK qualified as JoseJwk
import Crypto.JOSE.JWS qualified as JoseJws
import Crypto.JWT qualified as Jwt
import Data.Array (listArray)
import Data.Either (fromRight)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
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
      futureExpiration <- addUTCTime 3600 <$> getCurrentTime
      expOnlyToken <- issueTestClaimsJwt JwaJws.HS256 signingKey (Jwt.emptyClaimsSet & Jwt.claimExp ?~ Jwt.NumericDate futureExpiration)
      let allowed = mkJwtAllowedAlgorithms (JwtHs256 :| [])
          verifier = jwtProofVerifierWithRequiredClaims validationSettings noRequiredClaims allowed (JoseJwk.JWKSet [wrongKey]) (Right . show)
          rejectedProjection = jwtProofVerifierWithRequiredClaims validationSettings noRequiredClaims allowed (JoseJwk.JWKSet [signingKey]) (const (Left (mkJwtClaimsError (requiredFailureCode "jwt.claims-rejected")) :: Either JwtClaimsError ()))
          defaultPolicyVerifier = jwtProofVerifier validationSettings allowed (JoseJwk.JWKSet [signingKey]) (Right . show)
          noneToken = encodedJwtFromBytes "eyJhbGciOiJub25lIn0.eyJzdWIiOiJhZGEifQ."
      expectAll
        ( (verifyAuthenticationProof verifier token `shouldReturn` Left (ProofRejected (mkProofRejection (requiredFailureCode "jwt.rejected"))))
            :| [ verifyAuthenticationProof defaultPolicyVerifier token `shouldReturn` Left (ProofRejected (mkProofRejection (requiredFailureCode "jwt.rejected"))),
                 verifyAuthenticationProof defaultPolicyVerifier expOnlyToken >>= \result -> isAcceptedResult result `shouldBe` True,
                 verifyAuthenticationProof verifier noneToken `shouldReturn` Left (ProofRejected (mkProofRejection (requiredFailureCode "jwt.rejected"))),
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

    it "resolves claim-presence defaults and independent true/false overrides" $ do
      let nothingGenerated = JwtClaimGeneration False False False
          everythingConfigured = JwtClaimGeneration True True True
          defaultsWithoutGeneration = resolveJwtRequiredClaims nothingGenerated defaultJwtClaimPresencePolicy
          defaultsWithGeneration = resolveJwtRequiredClaims everythingConfigured defaultJwtClaimPresencePolicy
          explicitOverrides =
            resolveJwtRequiredClaims
              nothingGenerated
              JwtClaimPresencePolicy
                { jwtExpirationPresence = AllowAbsence,
                  jwtNotBeforePresence = RequirePresence,
                  jwtIssuerPresence = RequirePresence,
                  jwtAudiencePresence = AllowAbsence
                }
      expectAll
        ( (requiredClaimsTuple defaultsWithoutGeneration `shouldBe` (True, False, False, False))
            :| [ requiredClaimsTuple defaultsWithGeneration `shouldBe` (True, True, True, True),
                 requiredClaimsTuple explicitOverrides `shouldBe` (False, True, True, False),
                 resolveJwtRequiredClaims everythingConfigured defaultJwtClaimPresencePolicy == defaultsWithGeneration `shouldBe` True,
                 show defaultsWithGeneration `shouldContain` "JwtRequiredClaims",
                 show defaultJwtClaimPresencePolicy `shouldContain` "UseIssuanceDefault",
                 show UseIssuanceDefault `shouldBe` "UseIssuanceDefault",
                 show RequirePresence `shouldBe` "RequirePresence",
                 show AllowAbsence `shouldBe` "AllowAbsence",
                 show everythingConfigured `shouldContain` "jwtGenerationEmitsNotBefore",
                 JwtClaimPresencePolicy
                   { jwtExpirationPresence = RequirePresence,
                     jwtNotBeforePresence = AllowAbsence,
                     jwtIssuerPresence = UseIssuanceDefault,
                     jwtAudiencePresence = RequirePresence
                   }
                   /= defaultJwtClaimPresencePolicy
                     `shouldBe` True
               ]
        )

    it "compares and renders each registered-claim policy value directly" $ do
      oneMinute <- requiredRight "one minute skew" (mkJwtClockSkewMinutes 1)
      twoMinutes <- requiredRight "two minute skew" (mkJwtClockSkewMinutes 2)
      let nothingGenerated = JwtClaimGeneration False False False
          everythingGeneratedValue = JwtClaimGeneration True True True
          changedPolicy =
            JwtClaimPresencePolicy
              { jwtExpirationPresence = RequirePresence,
                jwtNotBeforePresence = AllowAbsence,
                jwtIssuerPresence = UseIssuanceDefault,
                jwtAudiencePresence = RequirePresence
              }
          defaultPolicy = defaultJwtClaimPresencePolicy
          requirementsToRender =
            [ jwtExpirationPresence defaultPolicy,
              jwtExpirationPresence changedPolicy,
              jwtNotBeforePresence changedPolicy
            ]
          generatedRequirements = resolveJwtRequiredClaims everythingGeneratedValue defaultPolicy
          noGenerationRequirements = resolveJwtRequiredClaims nothingGenerated defaultPolicy
          generationText =
            "JwtClaimGeneration {jwtGenerationEmitsNotBefore = True, jwtGenerationConfiguresIssuer = True, jwtGenerationConfiguresAudience = True}"
          policyText =
            "JwtClaimPresencePolicy {jwtExpirationPresence = UseIssuanceDefault, jwtNotBeforePresence = UseIssuanceDefault, jwtIssuerPresence = UseIssuanceDefault, jwtAudiencePresence = UseIssuanceDefault}"
          requirementsText =
            "JwtRequiredClaims {jwtRequiredExpiration = True, jwtRequiredNotBefore = True, jwtRequiredIssuer = True, jwtRequiredAudience = True}"
      expectAll
        ( ((oneMinute == oneMinute) `shouldBe` True)
            :| [ (oneMinute /= twoMinutes) `shouldBe` True,
                 show oneMinute `shouldBe` "JwtClockSkew 1",
                 showsPrec 11 oneMinute "" `shouldBe` "(JwtClockSkew 1)",
                 showList [oneMinute] "" `shouldBe` "[JwtClockSkew 1]",
                 (everythingGeneratedValue == everythingGeneratedValue) `shouldBe` True,
                 (everythingGeneratedValue /= nothingGenerated) `shouldBe` True,
                 show everythingGeneratedValue `shouldBe` generationText,
                 showsPrec 11 everythingGeneratedValue "" `shouldBe` ("(" <> generationText <> ")"),
                 showList [everythingGeneratedValue] "" `shouldBe` ("[" <> generationText <> "]"),
                 (UseIssuanceDefault == UseIssuanceDefault) `shouldBe` True,
                 (RequirePresence == RequirePresence) `shouldBe` True,
                 (AllowAbsence == AllowAbsence) `shouldBe` True,
                 (UseIssuanceDefault /= RequirePresence) `shouldBe` True,
                 show UseIssuanceDefault `shouldBe` "UseIssuanceDefault",
                 showsPrec 11 AllowAbsence "" `shouldBe` "AllowAbsence",
                 showList [RequirePresence, AllowAbsence] "" `shouldBe` "[RequirePresence,AllowAbsence]",
                 map show requirementsToRender `shouldBe` ["UseIssuanceDefault", "RequirePresence", "AllowAbsence"],
                 (defaultPolicy == defaultJwtClaimPresencePolicy) `shouldBe` True,
                 (defaultPolicy /= changedPolicy) `shouldBe` True,
                 show defaultPolicy `shouldBe` policyText,
                 showsPrec 11 defaultPolicy "" `shouldBe` ("(" <> policyText <> ")"),
                 showList [defaultPolicy] "" `shouldBe` ("[" <> policyText <> "]"),
                 (generatedRequirements == generatedRequirements) `shouldBe` True,
                 (generatedRequirements /= noGenerationRequirements) `shouldBe` True,
                 show generatedRequirements `shouldBe` requirementsText,
                 showsPrec 11 generatedRequirements "" `shouldBe` ("(" <> requirementsText <> ")"),
                 showList [generatedRequirements] "" `shouldBe` ("[" <> requirementsText <> "]")
               ]
        )

    it "accepts zero and arbitrary-precision nonnegative clock skew minutes exactly" $ do
      let largeMinutes = 2 ^ (80 :: Int)
      oneMinute <- requiredRight "one minute skew" (mkJwtClockSkewMinutes 1)
      largeSkew <- requiredRight "large skew" (mkJwtClockSkewMinutes largeMinutes)
      let negativeSkew = mkJwtClockSkewMinutes (-1)
          minuteSettings = jwtValidationSettingsWithClockSkew oneMinute validationSettings
          largeSettings = jwtValidationSettingsWithClockSkew largeSkew validationSettings
      expectAll
        ( (mkJwtClockSkewMinutes 0 `shouldBe` Right defaultJwtClockSkew)
            :| [ jwtClockSkewMinutes defaultJwtClockSkew `shouldBe` 0,
                 jwtClockSkewMinutes oneMinute `shouldBe` 1,
                 jwtClockSkewMinutes largeSkew `shouldBe` largeMinutes,
                 view Jwt.allowedSkew minuteSettings `shouldBe` 60,
                 view Jwt.allowedSkew largeSettings `shouldBe` fromInteger (largeMinutes * 60),
                 negativeSkew `shouldBe` Left JwtClockSkewNegative,
                 hasDerivedContract [JwtClockSkewNegative, JwtStringOrUriInvalid] `shouldBe` True,
                 show JwtClockSkewNegative `shouldBe` "JwtClockSkewNegative",
                 show JwtStringOrUriInvalid `shouldBe` "JwtStringOrUriInvalid",
                 show oneMinute `shouldContain` "JwtClockSkew"
               ]
        )

    it "builds empty, list, and array string-or-URI membership predicates" $ do
      listMembership <- requiredRight "list issuer membership" (mkJwtStringOrUriMembership ["https://issuer.example.test", "issuer-alias"])
      arrayMembership <- requiredRight "array audience membership" (mkJwtStringOrUriMembership (listArray (0 :: Int, 1) ["https://resource.example.test/api", "resource-alias"]))
      emptyMembership <- requiredRight "empty membership" (mkJwtStringOrUriMembership ([] :: [Text]))
      let issuer = requiredStringOrUri "https://issuer.example.test"
          issuerAlias = requiredStringOrUri "issuer-alias"
          rejectedIssuer = requiredStringOrUri "https://other.example.test"
          audience = requiredStringOrUri "https://resource.example.test/api"
          audienceAlias = requiredStringOrUri "resource-alias"
      expectAll
        ( (listMembership issuer `shouldBe` True)
            :| [ listMembership issuerAlias `shouldBe` True,
                 listMembership rejectedIssuer `shouldBe` False,
                 arrayMembership audience `shouldBe` True,
                 arrayMembership audienceAlias `shouldBe` True,
                 arrayMembership rejectedIssuer `shouldBe` False,
                 emptyMembership issuer `shouldBe` False,
                 isStringUriConfigurationError (mkJwtStringOrUriMembership [""]) `shouldBe` True,
                 isStringUriConfigurationError (mkJwtStringOrUriMembership ["https://bad.example.test\NUL"]) `shouldBe` True
               ]
        )

    it "requires each configured registered claim after JOSE verification" $ do
      signingKey <- JoseJwk.genJWK (JwaJwk.RSAGenParam 2048)
      let validClaims = claimsValidAt testVerificationTime
          claimsMissingExpiration = validClaims & Jwt.claimExp .~ Nothing
          claimsMissingNotBefore = validClaims & Jwt.claimNbf .~ Nothing
          claimsMissingIssuer = validClaims & Jwt.claimIss .~ Nothing
          claimsMissingAudience = validClaims & Jwt.claimAud .~ Nothing
          requiredClaims = resolveJwtRequiredClaims everythingGenerated defaultJwtClaimPresencePolicy
          verifier =
            jwtProofVerifierWithClock
              (pure testVerificationTime)
              (settingsForPredicates (const True) (const True))
              requiredClaims
              (mkJwtAllowedAlgorithms (JwtRs256 :| []))
              (JoseJwk.JWKSet [signingKey])
              (const (Right ("verified" :: Text)))
      validToken <- issueTestClaimsJwt JwaJws.RS256 signingKey validClaims
      missingExpirationToken <- issueTestClaimsJwt JwaJws.RS256 signingKey claimsMissingExpiration
      missingNotBeforeToken <- issueTestClaimsJwt JwaJws.RS256 signingKey claimsMissingNotBefore
      missingIssuerToken <- issueTestClaimsJwt JwaJws.RS256 signingKey claimsMissingIssuer
      missingAudienceToken <- issueTestClaimsJwt JwaJws.RS256 signingKey claimsMissingAudience
      results <-
        traverse
          (verifyAuthenticationProof verifier)
          [ validToken,
            missingExpirationToken,
            missingNotBeforeToken,
            missingIssuerToken,
            missingAudienceToken
          ]
      map isAcceptedResult results `shouldBe` [True, False, False, False, False]

    it "uses Foldable predicates in JOSE and accepts any accepted audience member" $ do
      signingKey <- JoseJwk.genJWK (JwaJwk.RSAGenParam 2048)
      acceptedIssuer <- requiredRight "accepted issuer predicate" (mkJwtStringOrUriMembership ["https://issuer.example.test"])
      acceptedAudiences <- requiredRight "accepted audience predicate" (mkJwtStringOrUriMembership (listArray (1 :: Int, 2) ["https://resource.example.test/api", "resource-alias"]))
      let goodClaims =
            claimsValidAt testVerificationTime
              & Jwt.claimAud ?~ Jwt.Audience (fmap requiredStringOrUri ["https://unmatched.example.test", "https://resource.example.test/api"])
          -- A supplied predicate replaces the generation value rather than
          -- silently broadening itself with that local value.
          excludedGenerationClaims = goodClaims & Jwt.claimIss ?~ requiredStringOrUri "https://local.example.test"
          acceptedClaims = goodClaims & Jwt.claimIss ?~ requiredStringOrUri "https://issuer.example.test"
          missingIdentityClaims = goodClaims & Jwt.claimIss .~ Nothing & Jwt.claimAud .~ Nothing
          noIdentityGeneration = JwtClaimGeneration True False False
          requiredClaims = resolveJwtRequiredClaims everythingGenerated defaultJwtClaimPresencePolicy
          optionalIdentityRequirements = resolveJwtRequiredClaims noIdentityGeneration defaultJwtClaimPresencePolicy
          explicitOptionalIdentityRequirements =
            resolveJwtRequiredClaims
              noIdentityGeneration
              defaultJwtClaimPresencePolicy
                { jwtIssuerPresence = AllowAbsence,
                  jwtAudiencePresence = AllowAbsence
                }
          requiredIdentityRequirements =
            resolveJwtRequiredClaims
              noIdentityGeneration
              defaultJwtClaimPresencePolicy
                { jwtIssuerPresence = RequirePresence,
                  jwtAudiencePresence = RequirePresence
                }
          verifier =
            jwtProofVerifierWithClock
              (pure testVerificationTime)
              (settingsForPredicates acceptedAudiences acceptedIssuer)
              requiredClaims
              (mkJwtAllowedAlgorithms (JwtRs256 :| []))
              (JoseJwk.JWKSet [signingKey])
              (const (Right ("verified" :: Text)))
          optionalIdentityVerifier =
            jwtProofVerifierWithClock
              (pure testVerificationTime)
              (settingsForPredicates acceptedAudiences acceptedIssuer)
              optionalIdentityRequirements
              (mkJwtAllowedAlgorithms (JwtRs256 :| []))
              (JoseJwk.JWKSet [signingKey])
              (const (Right ("verified" :: Text)))
          requiredIdentityVerifier =
            jwtProofVerifierWithClock
              (pure testVerificationTime)
              (settingsForPredicates acceptedAudiences acceptedIssuer)
              requiredIdentityRequirements
              (mkJwtAllowedAlgorithms (JwtRs256 :| []))
              (JoseJwk.JWKSet [signingKey])
              (const (Right ("verified" :: Text)))
      acceptedToken <- issueTestClaimsJwt JwaJws.RS256 signingKey acceptedClaims
      excludedGenerationToken <- issueTestClaimsJwt JwaJws.RS256 signingKey excludedGenerationClaims
      missingIdentityToken <- issueTestClaimsJwt JwaJws.RS256 signingKey missingIdentityClaims
      acceptedResult <- verifyAuthenticationProof verifier acceptedToken
      excludedGenerationResult <- verifyAuthenticationProof verifier excludedGenerationToken
      acceptedWithoutIdentityGeneration <- verifyAuthenticationProof optionalIdentityVerifier acceptedToken
      missingOptionalIdentity <- verifyAuthenticationProof optionalIdentityVerifier missingIdentityToken
      missingRequiredIdentity <- verifyAuthenticationProof requiredIdentityVerifier missingIdentityToken
      expectAll
        ( (isAcceptedResult acceptedResult `shouldBe` True)
            :| [ isAcceptedResult excludedGenerationResult `shouldBe` False,
                 isAcceptedResult acceptedWithoutIdentityGeneration `shouldBe` True,
                 isAcceptedResult missingOptionalIdentity `shouldBe` True,
                 isAcceptedResult missingRequiredIdentity `shouldBe` False,
                 optionalIdentityRequirements `shouldBe` explicitOptionalIdentityRequirements
               ]
        )

    it "applies positive skew once with inclusive nbf and exclusive exp boundaries" $ do
      signingKey <- JoseJwk.genJWK (JwaJwk.RSAGenParam 2048)
      skew <- requiredRight "two minute skew" (mkJwtClockSkewMinutes 2)
      zeroSkew <- requiredRight "zero minute skew" (mkJwtClockSkewMinutes 0)
      let nowSecondsValue = 100000
          boundaryCases =
            [ (nowSecondsValue + 120 - 1 / 1000, nowSecondsValue + 3600, True),
              (nowSecondsValue + 120, nowSecondsValue + 3600, True),
              (nowSecondsValue + 120 + 1 / 1000, nowSecondsValue + 3600, False),
              (nowSecondsValue, nowSecondsValue - 120 - 1 / 1000, False),
              (nowSecondsValue, nowSecondsValue - 120, False),
              (nowSecondsValue, nowSecondsValue - 120 + 1 / 1000, True)
            ]
          presencePolicy =
            JwtClaimPresencePolicy
              { jwtExpirationPresence = RequirePresence,
                jwtNotBeforePresence = RequirePresence,
                jwtIssuerPresence = AllowAbsence,
                jwtAudiencePresence = AllowAbsence
              }
          requiredClaims =
            resolveJwtRequiredClaims (JwtClaimGeneration True False False) presencePolicy
          settings =
            jwtValidationSettingsWithClockSkew
              skew
              (Jwt.defaultJWTValidationSettings (const True))
          verifier =
            jwtProofVerifierWithClock
              (pure testVerificationTime)
              settings
              requiredClaims
              (mkJwtAllowedAlgorithms (JwtRs256 :| []))
              (JoseJwk.JWKSet [signingKey])
              (const (Right ("verified" :: Text)))
          zeroSkewVerifier =
            jwtProofVerifierWithClock
              (pure testVerificationTime)
              (jwtValidationSettingsWithClockSkew zeroSkew (Jwt.defaultJWTValidationSettings (const True)))
              requiredClaims
              (mkJwtAllowedAlgorithms (JwtRs256 :| []))
              (JoseJwk.JWKSet [signingKey])
              (const (Right ("verified" :: Text)))
          optionalTemporalVerifier =
            jwtProofVerifierWithClock
              (pure testVerificationTime)
              settings
              noRequiredClaims
              (mkJwtAllowedAlgorithms (JwtRs256 :| []))
              (JoseJwk.JWKSet [signingKey])
              (const (Right ("verified" :: Text)))
          zeroBoundaryCases =
            [ (nowSecondsValue, nowSecondsValue + 3600, True),
              (nowSecondsValue + 1 / 1000, nowSecondsValue + 3600, False),
              (nowSecondsValue, nowSecondsValue, False)
            ]
      tokens <-
        traverse
          (\(notBefore, expiration, _) -> issueTestClaimsJwt JwaJws.RS256 signingKey (claimsAtSeconds notBefore expiration))
          boundaryCases
      results <- traverse (verifyAuthenticationProof verifier) tokens
      optionalTemporalResults <- traverse (verifyAuthenticationProof optionalTemporalVerifier) tokens
      zeroSkewTokens <-
        traverse
          (\(notBefore, expiration, _) -> issueTestClaimsJwt JwaJws.RS256 signingKey (claimsAtSeconds notBefore expiration))
          zeroBoundaryCases
      zeroSkewResults <- traverse (verifyAuthenticationProof zeroSkewVerifier) zeroSkewTokens
      expectAll
        ( (map isAcceptedResult results `shouldBe` fmap third boundaryCases)
            :| [ map isAcceptedResult optionalTemporalResults `shouldBe` fmap third boundaryCases,
                 map isAcceptedResult zeroSkewResults `shouldBe` fmap third zeroBoundaryCases
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
            (jwtProofVerifierWithRequiredClaims validationSettings noRequiredClaims (mkJwtAllowedAlgorithms (JwtHs256 :| [])) (JoseJwk.JWKSet [signingKey]) (Right . show))
            token
            `shouldReturn` Right (show Jwt.emptyClaimsSet)
        Left failure -> expectationFailure ("expected the adapted JOSE signer to retain its proof: " <> Text.unpack failure)

assertAcceptedAndRejected :: JwtAlgorithm -> JwaJws.Alg -> JWK -> IO ()
assertAcceptedAndRejected algorithm joseAlgorithm key = do
  token <- issueTestJwt joseAlgorithm key
  let accepted = jwtProofVerifierWithRequiredClaims validationSettings noRequiredClaims (mkJwtAllowedAlgorithms (algorithm :| [])) (JoseJwk.JWKSet [key]) (Right . show)
      rejected = jwtProofVerifierWithRequiredClaims validationSettings noRequiredClaims (mkJwtAllowedAlgorithms (otherAlgorithm algorithm :| [])) (JoseJwk.JWKSet [key]) (Right . show)
  expectAll
    ( (verifyAuthenticationProof accepted token `shouldReturn` Right (show Jwt.emptyClaimsSet))
        :| [ verifyAuthenticationProof rejected token `shouldReturn` Left (ProofRejected (mkProofRejection (requiredFailureCode "jwt.rejected")))
           ]
    )

issueTestJwt :: JwaJws.Alg -> JWK -> IO EncodedJwt
issueTestJwt joseAlgorithm key = do
  issueTestClaimsJwt joseAlgorithm key Jwt.emptyClaimsSet

issueTestClaimsJwt :: JwaJws.Alg -> JWK -> Jwt.ClaimsSet -> IO EncodedJwt
issueTestClaimsJwt joseAlgorithm key claims = do
  issued <- issueJwt key (JoseJws.newJWSHeaderProtected joseAlgorithm) claims
  case issued of
    Right token -> pure token
    Left jwtError -> expectationFailure ("test JWT issuance failed: " <> show jwtError) >> error "unreachable"

noRequiredClaims :: JwtRequiredClaims
noRequiredClaims =
  resolveJwtRequiredClaims
    (JwtClaimGeneration False False False)
    JwtClaimPresencePolicy
      { jwtExpirationPresence = AllowAbsence,
        jwtNotBeforePresence = AllowAbsence,
        jwtIssuerPresence = AllowAbsence,
        jwtAudiencePresence = AllowAbsence
      }

everythingGenerated :: JwtClaimGeneration
everythingGenerated = JwtClaimGeneration True True True

requiredClaimsTuple :: JwtRequiredClaims -> (Bool, Bool, Bool, Bool)
requiredClaimsTuple requiredClaims =
  ( jwtRequiredExpiration requiredClaims,
    jwtRequiredNotBefore requiredClaims,
    jwtRequiredIssuer requiredClaims,
    jwtRequiredAudience requiredClaims
  )

testVerificationTime :: UTCTime
testVerificationTime = posixSecondsToUTCTime 100000

claimsValidAt :: UTCTime -> Jwt.ClaimsSet
claimsValidAt now =
  Jwt.emptyClaimsSet
    & Jwt.claimIss ?~ requiredStringOrUri "https://issuer.example.test"
    & Jwt.claimAud ?~ Jwt.Audience [requiredStringOrUri "https://resource.example.test/api"]
    & Jwt.claimNbf ?~ Jwt.NumericDate now
    & Jwt.claimExp ?~ Jwt.NumericDate (addUTCTime 3600 now)

claimsAtSeconds :: Rational -> Rational -> Jwt.ClaimsSet
claimsAtSeconds notBeforeSeconds expirationSeconds =
  Jwt.emptyClaimsSet
    & Jwt.claimNbf ?~ numericDateFromSeconds notBeforeSeconds
    & Jwt.claimExp ?~ numericDateFromSeconds expirationSeconds

numericDateFromSeconds :: Rational -> Jwt.NumericDate
numericDateFromSeconds = Jwt.NumericDate . posixSecondsToUTCTime . fromRational

third :: (first, second, result) -> result
third (_, _, result) = result

settingsForPredicates :: (Jwt.StringOrURI -> Bool) -> (Jwt.StringOrURI -> Bool) -> JWTValidationSettings
settingsForPredicates acceptedAudience acceptedIssuer =
  Jwt.defaultJWTValidationSettings acceptedAudience
    & Jwt.jwtValidationSettingsIssuerPredicate .~ acceptedIssuer

requiredStringOrUri :: Text -> Jwt.StringOrURI
requiredStringOrUri inputText =
  case matching Jwt.stringOrUri (Text.unpack inputText) of
    Left parseError -> error ("expected test StringOrURI to parse: " <> show parseError)
    Right parsed -> parsed

requiredRight :: (Show errorValue) => Text -> Either errorValue value -> IO value
requiredRight description result =
  case result of
    Left failure -> expectationFailure (Text.unpack description <> " failed: " <> show failure) >> error "unreachable"
    Right resultValue -> pure resultValue

isAcceptedResult :: Either value ignored -> Bool
isAcceptedResult result =
  case result of
    Left _ -> False
    Right _ -> True

isStringUriConfigurationError :: Either JwtConfigurationError (Jwt.StringOrURI -> Bool) -> Bool
isStringUriConfigurationError result =
  case result of
    Left JwtStringOrUriInvalid -> True
    Left JwtClockSkewNegative -> False
    Right _ -> False

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
