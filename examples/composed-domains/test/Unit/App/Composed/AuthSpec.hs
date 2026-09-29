module Unit.App.Composed.AuthSpec (spec) where

import App.Composed.Auth
import Control.Lens ((#), (&), (.~), (?~), (^.))
import Crypto.JOSE.JWA.JWK qualified as JwaJwk
import Crypto.JOSE.JWK qualified as JoseJwk
import Crypto.JOSE.Types (Base64Integer (..))
import Crypto.JWT qualified as Jwt
import Data.Aeson qualified as Aeson
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb
  ( AuthenticationProofVerifier (AuthenticationProofVerifier),
    EncodedJwt,
    ProofRejection,
    ProofVerificationFailure (ProofRejected),
    jwtProofFromCookie,
    mkJwtClaimsError,
    mkProofRejection,
    requiredSecurityFailureCodeOrDie,
  )
import HarchWeb.Authentication qualified as OAuth2
import HarchWeb.Time (addUnixTimeNanoseconds, currentUnixTimeNanoseconds)
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
    apiToken <- orMint =<< issueTestApiToken runtime "client-1" ["catalog:read", "orders:write"]
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
               apiTokenAtWeb `shouldBe` audienceRejection,
               show runtime `shouldBe` "ComposedJwtRuntime <redacted>",
               showList [runtime] "" `shouldBe` "[ComposedJwtRuntime <redacted>]"
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
    foreignSigned <- orMint =<< issueTestApiToken foreignRuntime "client-1" ["catalog:read"]
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
    failedMint <- issueTestApiToken publicOnlyRuntime "client-1" ["catalog:read"]
    expectAll
      ( (parseComposedWebJwtClaims configuration Jwt.emptyClaimsSet `shouldBe` audienceRejection)
          :| [ parseComposedApiJwtClaims configuration Jwt.emptyClaimsSet `shouldBe` audienceRejection,
               parseComposedWebJwtClaims configuration webAudienceClaims `shouldBe` expectedWebRejection,
               parseComposedApiJwtClaims configuration apiAudienceClaims `shouldBe` expectedApiRejection,
               parseComposedApiJwtClaims configuration (withSubject apiAudienceClaims) `shouldBe` expectedApiRejection,
               parseComposedApiJwtClaims configuration (jwtClaimsFromJson "{\"aud\":[\"composed-api\"],\"sub\":\"client-1\",\"scope\":\"catalog:read  orders:write\"}") `shouldBe` expectedApiRejection,
               parseComposedApiJwtClaims configuration (jwtClaimsFromJson "{\"aud\":[\"composed-api\"],\"sub\":\"client-1\",\"scope\":\"catalog:read catalog:read\"}") `shouldBe` expectedApiRejection,
               parseComposedApiJwtClaims configuration (jwtClaimsFromJson "{\"aud\":[\"composed-api\"],\"sub\":\"client-1\",\"scope\":\"catalog:bad\\n\"}") `shouldBe` expectedApiRejection,
               parseComposedApiJwtClaims configuration (jwtClaimsFromJson "{\"aud\":[\"composed-api\"],\"sub\":\"client-1\",\"scope\":7}") `shouldBe` expectedApiRejection,
               parseComposedWebJwtClaims configuration (withSubject webAudienceClaims)
                 `shouldBe` Right (ComposedWebClaims "client-1"),
               fmap composedWebSubject (parseComposedWebJwtClaims configuration (withSubject webAudienceClaims)) `shouldBe` Right "client-1",
               mintedWithUnusableKey failedMint,
               show configuration `shouldContain` "account-web",
               show ComposedJwtIssueFailed `shouldBe` "ComposedJwtIssueFailed"
             ]
      )

  it "rejects an API token whose lifetime is empty or reversed" $ do
    signingKey <- JoseJwk.genJWK (JwaJwk.RSAGenParam 1024)
    let namedSigningKey = signingKey & JoseJwk.jwkKid ?~ "composed-key-v1"
    configuration <- orFail "composed JWT configuration" (mkComposedJwtConfiguration "https://composed.test" "account-web" "composed-api" "composed-key-v1")
    runtime <- orFail "composed JWT runtime" (loadComposedJwtRuntime configuration namedSigningKey (JoseJwk.JWKSet [namedSigningKey]))
    emptyLifetime <- issueComposedApiToken runtime "client-1" (requiredTestScopes ["catalog:read"]) 100 100
    reversedLifetime <- issueComposedApiToken runtime "client-1" (requiredTestScopes ["catalog:read"]) 101 100
    expectAll
      ( (isIssueFailed emptyLifetime `shouldBe` True)
          :| [isIssueFailed reversedLifetime `shouldBe` True]
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

  it "rejects malformed or ambiguous verification keys and redacts runtime display" $ do
    signingKey <- JoseJwk.genJWK (JwaJwk.RSAGenParam 1024)
    differentSigningKey <- JoseJwk.genJWK (JwaJwk.RSAGenParam 1024)
    nonRsaKey <- JoseJwk.genJWK (JwaJwk.OctGenParam 64)
    let activeKeyId = "composed-key-v1"
        namedSigningKey = signingKey & JoseJwk.jwkKid ?~ activeKeyId
        namedDifferentKey = differentSigningKey & JoseJwk.jwkKid ?~ activeKeyId
        namedNonRsaKey = nonRsaKey & JoseJwk.jwkKid ?~ activeKeyId
        emptyKeyId = signingKey & JoseJwk.jwkKid ?~ ""
    configuration <- orFail "composed JWT configuration" (mkComposedJwtConfiguration "https://composed.test" "account-web" "composed-api" activeKeyId)
    let rejectsKeySet signingMaterial candidates =
          isConfigurationError
            ComposedJwtVerificationKeyMismatch
            (loadComposedJwtRuntime configuration signingMaterial (JoseJwk.JWKSet candidates))
    expectAll
      ( (rejectsKeySet namedSigningKey [signingKey] `shouldBe` True)
          :| [ rejectsKeySet namedSigningKey [emptyKeyId] `shouldBe` True,
               rejectsKeySet namedSigningKey [namedNonRsaKey] `shouldBe` True,
               rejectsKeySet namedSigningKey [namedSigningKey, namedSigningKey] `shouldBe` True,
               rejectsKeySet namedSigningKey [namedDifferentKey] `shouldBe` True,
               rejectsKeySet namedNonRsaKey [namedSigningKey] `shouldBe` True
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

isIssueFailed :: Either ComposedJwtIssueError value -> Bool
isIssueFailed result =
  case result of
    Left ComposedJwtIssueFailed -> True
    Right _ -> False

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

issueTestApiToken :: ComposedJwtRuntime -> Text -> [Text] -> IO (Either ComposedJwtIssueError EncodedJwt)
issueTestApiToken runtime subject scopes = do
  now <- currentUnixTimeNanoseconds
  case addUnixTimeNanoseconds now (3600 * 1000000000) of
    Nothing -> issueComposedApiToken runtime subject (requiredTestScopes scopes) now now
    Just expiresAt -> issueComposedApiToken runtime subject (requiredTestScopes scopes) now expiresAt

jwtClaimsFromJson :: Text -> Jwt.ClaimsSet
jwtClaimsFromJson jsonText =
  case Aeson.eitherDecodeStrict' (TextEncoding.encodeUtf8 jsonText) of
    Right claims -> claims
    Left message -> error ("expected valid test JWT claims JSON: " <> message)

requiredTestScopes :: [Text] -> NonEmpty OAuth2.OAuth2Scope
requiredTestScopes scopeTexts =
  case NonEmpty.nonEmpty (fmap requiredTestScope scopeTexts) of
    Just scopes -> scopes
    Nothing -> error "test API token scopes must be nonempty"

requiredTestScope :: Text -> OAuth2.OAuth2Scope
requiredTestScope scopeText =
  case OAuth2.mkOAuth2Scope scopeText of
    Right scope -> scope
    Left failure -> error ("expected a valid OAuth scope: " <> show failure)
