{-# LANGUAGE OverloadedStrings #-}

module Unit.App.Composed.AuthSpec (spec) where

import App.Composed.Auth
import Control.Lens ((#), (&), (.~), (?~), (^.))
import Crypto.JOSE.JWA.JWK qualified as JwaJwk
import Crypto.JOSE.JWK qualified as JoseJwk
import Crypto.JOSE.Types (Base64Integer (..))
import Crypto.JWT qualified as Jwt
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import HarchWeb
  ( AuthenticationProofVerifier (AuthenticationProofVerifier),
    ProofRejection,
    ProofVerificationFailure (ProofRejected),
    jwtProofFromCookie,
    mkJwtClaimsError,
    mkProofRejection,
    requiredSecurityFailureCodeOrDie,
  )
import Test.Hspec
import TestCore.CustomAssertions (expectAll)

spec :: Spec
spec = describe "Unit.App.Composed.Auth" $ do
  it "mints and verifies two distinct audiences over one key set" $ do
    signingKey <- JoseJwk.genJWK (JwaJwk.RSAGenParam 1024)
    let namedSigningKey = signingKey & JoseJwk.jwkKid ?~ "composed-key-v1"
        verificationKeys = JoseJwk.JWKSet [namedSigningKey]
    configuration <- orFail "composed JWT configuration" (mkComposedJwtConfiguration "https://composed.test" "account-web" "composed-api" "composed-key-v1")
    runtime <- orFail "composed JWT runtime" (loadComposedJwtRuntime configuration namedSigningKey verificationKeys)
    webToken <- orMint =<< issueComposedWebToken runtime "account-1"
    apiToken <- orMint =<< issueComposedApiToken runtime "client-1" ["catalog:read", "orders:write"]
    let AuthenticationProofVerifier verifyWeb = composedWebProofVerifier runtime
        AuthenticationProofVerifier verifyApi = composedApiProofVerifier runtime
        audienceRejection :: Either ProofVerificationFailure value
        audienceRejection = Left (ProofRejected (mkProofRejection (requiredSecurityFailureCodeOrDie "composed.audience-mismatch")))
    webVerified <- verifyWeb (jwtProofFromCookie webToken)
    apiVerified <- verifyApi (jwtProofFromCookie apiToken)
    webTokenAtApi <- verifyApi (jwtProofFromCookie webToken)
    apiTokenAtWeb <- verifyWeb (jwtProofFromCookie apiToken)
    expectAll
      ( (webVerified `shouldBe` Right (ComposedWebClaims "account-1"))
          :| [ apiVerified `shouldBe` Right (ComposedApiClaims "client-1" ["catalog:read", "orders:write"]),
               webTokenAtApi `shouldBe` audienceRejection,
               apiTokenAtWeb `shouldBe` audienceRejection
             ]
      )

  it "rejects a foreign-signed token as a signature failure, not an audience failure" $ do
    honestKey <- JoseJwk.genJWK (JwaJwk.RSAGenParam 1024)
    foreignKey <- JoseJwk.genJWK (JwaJwk.RSAGenParam 1024)
    let namedHonestKey = honestKey & JoseJwk.jwkKid ?~ "composed-key-v1"
        namedForeignKey = foreignKey & JoseJwk.jwkKid ?~ "composed-key-v1"
    configuration <- orFail "composed JWT configuration" (mkComposedJwtConfiguration "https://composed.test" "account-web" "composed-api" "composed-key-v1")
    honestRuntime <- orFail "honest runtime" (loadComposedJwtRuntime configuration namedHonestKey (JoseJwk.JWKSet [namedHonestKey]))
    foreignRuntime <- orFail "foreign runtime" (loadComposedJwtRuntime configuration namedForeignKey (JoseJwk.JWKSet [namedForeignKey]))
    foreignSigned <- orMint =<< issueComposedApiToken foreignRuntime "client-1" ["catalog:read"]
    let AuthenticationProofVerifier verifyApi = composedApiProofVerifier honestRuntime
        audienceRejectionCode = mkProofRejection (requiredSecurityFailureCodeOrDie "composed.audience-mismatch")
    foreignResult <- verifyApi (jwtProofFromCookie foreignSigned)
    isRejectedNotAudience foreignResult audienceRejectionCode `shouldBe` True

  it "rejects claims that omit the audience, subject, or scope and maps minting failures to the typed issue error" $ do
    configuration <- orFail "composed JWT configuration" (mkComposedJwtConfiguration "https://composed.test" "account-web" "composed-api" "composed-key-v1")
    signingKey <- JoseJwk.genJWK (JwaJwk.RSAGenParam 1024)
    let namedSigningKey = signingKey & JoseJwk.jwkKid ?~ "composed-key-v1"
        publicOnlyKey =
          case namedSigningKey ^. JoseJwk.jwkMaterial of
            JwaJwk.RSAKeyMaterial rsaParameters ->
              namedSigningKey & JoseJwk.jwkMaterial .~ JwaJwk.RSAKeyMaterial (rsaParameters & JwaJwk.rsaN .~ Base64Integer 1)
            _ -> namedSigningKey
        webAudienceClaims = Jwt.emptyClaimsSet & Jwt.claimAud ?~ Jwt.Audience [Jwt.string # ("account-web" :: Text)]
        apiAudienceClaims = webAudienceClaims & Jwt.claimAud ?~ Jwt.Audience [Jwt.string # ("composed-api" :: Text)]
        withSubject claims = claims & Jwt.claimSub ?~ (Jwt.string # ("client-1" :: Text))
        expectedWebRejection = Left (HarchWeb.mkJwtClaimsError (HarchWeb.requiredSecurityFailureCodeOrDie "composed.web.claims-rejected"))
        expectedApiRejection = Left (HarchWeb.mkJwtClaimsError (HarchWeb.requiredSecurityFailureCodeOrDie "composed.api.claims-rejected"))
        audienceRejection = Left composedAudienceMismatch
    publicOnlyRuntime <- orFail "public-only runtime" (loadComposedJwtRuntime configuration publicOnlyKey (JoseJwk.JWKSet [publicOnlyKey]))
    failedMint <- issueComposedApiToken publicOnlyRuntime "client-1" ["catalog:read"]
    expectAll
      ( (parseComposedWebJwtClaims configuration Jwt.emptyClaimsSet `shouldBe` audienceRejection)
          :| [ parseComposedApiJwtClaims configuration Jwt.emptyClaimsSet `shouldBe` audienceRejection,
               parseComposedWebJwtClaims configuration webAudienceClaims `shouldBe` expectedWebRejection,
               parseComposedApiJwtClaims configuration apiAudienceClaims `shouldBe` expectedApiRejection,
               parseComposedApiJwtClaims configuration (withSubject apiAudienceClaims)
                 `shouldBe` Right (ComposedApiClaims "client-1" []),
               parseComposedWebJwtClaims configuration (withSubject webAudienceClaims)
                 `shouldBe` Right (ComposedWebClaims "client-1"),
               fmap composedWebSubject (parseComposedWebJwtClaims configuration (withSubject webAudienceClaims)) `shouldBe` Right "client-1",
               fmap composedApiSubject (parseComposedApiJwtClaims configuration (withSubject apiAudienceClaims)) `shouldBe` Right "client-1",
               fmap composedApiScopes (parseComposedApiJwtClaims configuration (withSubject apiAudienceClaims)) `shouldBe` Right [],
               mintedWithUnusableKey failedMint,
               show configuration `shouldContain` "account-web",
               show ComposedJwtIssueFailed `shouldBe` "ComposedJwtIssueFailed"
             ]
      )

  it "rejects values jose cannot normalize and pins every derived representation" $ do
    configuration <- orFail "composed JWT configuration" (mkComposedJwtConfiguration "https://composed.test" "account-web" "composed-api" "composed-key-v1")
    expectAll
      ( (mkComposedJwtConfiguration "http://[invalid" "account-web" "composed-api" "k" `shouldSatisfy` isConfigurationError ComposedJwtIssuerUnparseable)
          :| [ mkComposedJwtConfiguration "https://ok.test" "http://[invalid" "composed-api" "k" `shouldSatisfy` isConfigurationError ComposedJwtWebAudienceUnparseable,
               mkComposedJwtConfiguration "https://ok.test" "account-web" "http://[invalid" "k" `shouldSatisfy` isConfigurationError ComposedJwtApiAudienceUnparseable
             ]
      )
    checkDerived configuration
    checkDerived ComposedJwtIssuerEmpty
    checkDerived ComposedJwtVerificationKeyMismatch
    checkDerived ComposedJwtIssueFailed
    checkDerived (ComposedWebClaims "account-1")
    checkDerived (ComposedApiClaims "client-1" ["catalog:read"])

  it "fails closed on non-distinct audiences, empty values, and a mismatched verification set" $ do
    signingKey <- JoseJwk.genJWK (JwaJwk.RSAGenParam 1024)
    let namedSigningKey = signingKey & JoseJwk.jwkKid ?~ "composed-key-v1"
        differentKey = (signingKey & JoseJwk.jwkKid ?~ "different-id") :: JoseJwk.JWK
    expectAll
      ( (mkComposedJwtConfiguration "" "account-web" "composed-api" "k" `shouldSatisfy` isConfigurationError ComposedJwtIssuerEmpty)
          :| [ mkComposedJwtConfiguration "iss" "" "composed-api" "k" `shouldSatisfy` isConfigurationError ComposedJwtWebAudienceEmpty,
               mkComposedJwtConfiguration "iss" "account-web" "" "k" `shouldSatisfy` isConfigurationError ComposedJwtApiAudienceEmpty,
               mkComposedJwtConfiguration "iss" "same" "same" "k" `shouldSatisfy` isConfigurationError ComposedJwtAudiencesNotDistinct,
               mkComposedJwtConfiguration "iss" "account-web" "composed-api" "" `shouldSatisfy` isConfigurationError ComposedJwtActiveKeyIdEmpty,
               isConfigurationError
                 ComposedJwtVerificationKeyMismatch
                 (loadComposedJwtRuntime (configurationOrDie "composed-key-v1") namedSigningKey (JoseJwk.JWKSet [differentKey]))
                 `shouldBe` True
             ]
      )
  where
    configurationOrDie keyId =
      case mkComposedJwtConfiguration "https://composed.test" "account-web" "composed-api" keyId of
        Right configuration -> configuration
        Left failure -> error ("test configuration must parse: " <> show failure)

orFail :: (Show failure) => String -> Either failure value -> IO value
orFail label result =
  case result of
    Left failure -> expectationFailure (label <> " failed: " <> show failure) >> error "unreachable"
    Right value -> pure value

orMint :: Either ComposedJwtIssueError value -> IO value
orMint = orFail "token minting"

isConfigurationError :: (Eq failure) => failure -> Either failure value -> Bool
isConfigurationError expected result =
  case result of
    Left failure -> failure == expected
    Right _ -> False

mintedWithUnusableKey :: Either ComposedJwtIssueError value -> Expectation
mintedWithUnusableKey result =
  case result of
    Left issueError -> issueError `shouldBe` ComposedJwtIssueFailed
    Right _ -> expectationFailure "minting with an unusable signing key must fail"

checkDerived :: (Eq value, Show value) => value -> Expectation
checkDerived value = do
  value == value `shouldBe` True
  value /= value `shouldBe` False
  shows value "" `shouldBe` show value
  showsPrec 11 value "" `shouldSatisfy` (not . null)
  showList [value] "" `shouldSatisfy` (not . null)

isRejectedNotAudience :: Either ProofVerificationFailure value -> ProofRejection -> Bool
isRejectedNotAudience result audienceRejection =
  case result of
    Left (ProofRejected rejection) -> rejection /= audienceRejection
    _ -> False
