module Unit.App.Composed.AuthSpec (spec) where

import App.Composed.Auth
import Control.Lens ((#), (&), (.~), (?~), (^.))
import Crypto.JOSE.Header (HeaderParam (..), RequiredProtection (..))
import Crypto.JOSE.JWA.JWK qualified as JwaJwk
import Crypto.JOSE.JWA.JWS qualified as JwaJws
import Crypto.JOSE.JWK qualified as JoseJwk
import Crypto.JOSE.JWS qualified as JoseJws
import Crypto.JOSE.Types (Base64Integer (..))
import Crypto.JWT qualified as Jwt
import Data.Aeson qualified as Aeson
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
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
import HarchWeb qualified
import HarchWeb.Authentication qualified as OAuth2
import HarchWeb.Time (addUnixTimeNanoseconds, currentUnixTimeNanoseconds, unixTimeNanoseconds)
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
    fixedNow <- getCurrentTime
    webToken <- orMint =<< issueComposedWebTokenWithClock (pure fixedNow) runtime "account-1"
    apiToken <- orMint =<< issueTestApiToken runtime "client-1" ["catalog:read", "orders:write"]
    let AuthenticationProofVerifier verifyWeb = composedWebProofVerifier runtime
        AuthenticationProofVerifier verifyApi = composedApiProofVerifier runtime
        AuthenticationProofVerifier verifyWebAtFixed = composedWebProofVerifierWithClock (pure fixedNow) runtime Nothing Nothing
        AuthenticationProofVerifier verifyWebBeforeNbf = composedWebProofVerifierWithClock (pure (addUTCTime (-1) fixedNow)) runtime Nothing Nothing
        AuthenticationProofVerifier verifyApiWithRejectedIssuer = composedApiProofVerifierWithAcceptance runtime (Just (const False)) Nothing
        AuthenticationProofVerifier verifyApiWithReplacementAudience =
          composedApiProofVerifierWithAcceptance runtime Nothing (Just (== (Jwt.string # ("other-api" :: Text))))
        audienceRejection :: Either ProofVerificationFailure value
        audienceRejection = Left (ProofRejected (mkProofRejection (requiredSecurityFailureCodeOrDie "composed.audience-mismatch")))
    webVerified <- verifyWeb (jwtProofFromCookie webToken)
    webVerifiedAtNbf <- verifyWebAtFixed (jwtProofFromCookie webToken)
    webBeforeNbf <- verifyWebBeforeNbf (jwtProofFromCookie webToken)
    apiVerified <- verifyApi (jwtProofFromCookie apiToken)
    apiRejectedByCustomIssuer <- verifyApiWithRejectedIssuer (jwtProofFromCookie apiToken)
    apiRejectedByReplacementAudience <- verifyApiWithReplacementAudience (jwtProofFromCookie apiToken)
    webTokenAtApi <- verifyApi (jwtProofFromCookie webToken)
    apiTokenAtWeb <- verifyWeb (jwtProofFromCookie apiToken)
    expectAll
      ( (webVerified `shouldBe` Right (ComposedWebClaims "account-1"))
          :| [ webVerifiedAtNbf `shouldBe` Right (ComposedWebClaims "account-1"),
               webBeforeNbf `shouldSatisfy` isRejected,
               apiVerified `shouldBe` Right (ComposedApiClaims "client-1" ["catalog:read", "orders:write"]),
               apiRejectedByCustomIssuer `shouldSatisfy` isRejected,
               apiRejectedByReplacementAudience `shouldSatisfy` isRejected,
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

  it "uses the configured minute skew once at exact nbf and exp boundaries" $ do
    signingKey <- JoseJwk.genJWK (JwaJwk.RSAGenParam 1024)
    let namedSigningKey = signingKey & JoseJwk.jwkKid ?~ "composed-key-v1"
        policySettings = defaultComposedJwtPolicySettings {composedPolicyClockSkewMinutes = 2}
        nowSeconds = 1735689600 :: Integer
        fixedNow = posixSecondsToUTCTime (fromInteger nowSeconds)
        atOffset offset = unixTimeNanoseconds (fromInteger ((nowSeconds + offset) * 1000000000))
    configuration <-
      orFail
        "composed JWT skew configuration"
        (mkComposedJwtConfigurationWithPolicy policySettings "https://composed.test" "account-web" "composed-api" "composed-key-v1")
    runtime <- orFail "composed JWT skew runtime" (loadComposedJwtRuntime configuration namedSigningKey (JoseJwk.JWKSet [namedSigningKey]))
    let AuthenticationProofVerifier verifyApi = composedApiProofVerifierWithClock (pure fixedNow) runtime Nothing Nothing
        issueAndVerify nbfOffset expOffset = do
          token <-
            orMint
              =<< issueComposedApiToken
                runtime
                "client-1"
                (requiredTestScopes ["catalog:read"])
                (atOffset nbfOffset)
                (atOffset expOffset)
          verifyApi (jwtProofFromCookie token)
    nbfAtInclusiveBoundary <- issueAndVerify 120 600
    nbfBeyondBoundary <- issueAndVerify 121 600
    expWithinSkew <- issueAndVerify (-300) (-119)
    expAtExclusiveBoundary <- issueAndVerify (-300) (-120)
    expBeyondSkew <- issueAndVerify (-300) (-121)
    expectAll
      ( (nbfAtInclusiveBoundary `shouldSatisfy` isRight)
          :| [ nbfBeyondBoundary `shouldSatisfy` isRejected,
               expWithinSkew `shouldSatisfy` isRight,
               expAtExclusiveBoundary `shouldSatisfy` isRejected,
               expBeyondSkew `shouldSatisfy` isRejected
             ]
      )

  it "resolves default-on nbf generation against independent per-profile overrides" $ do
    signingKey <- JoseJwk.genJWK (JwaJwk.RSAGenParam 1024)
    let namedSigningKey = signingKey & JoseJwk.jwkKid ?~ "composed-key-v1"
        verificationKeys = JoseJwk.JWKSet [namedSigningKey]
        nowSeconds = 1735689600 :: Integer
        fixedNow = posixSecondsToUTCTime (fromInteger nowSeconds)
        issuedAt = unixTimeNanoseconds (fromInteger (nowSeconds * 1000000000))
        expiresAt = unixTimeNanoseconds (fromInteger ((nowSeconds + 900) * 1000000000))
        basePolicy = defaultComposedJwtPolicySettings {composedPolicyProvideNotBefore = False}
        requireNbfPolicy =
          basePolicy
            { composedApiPresencePolicy =
                (composedApiPresencePolicy basePolicy)
                  { HarchWeb.jwtNotBeforePresence = HarchWeb.RequirePresence
                  }
            }
        allowNbfByDefaultPolicy = basePolicy
    requiredNbfConfiguration <-
      orFail
        "explicit required nbf configuration"
        (mkComposedJwtConfigurationWithPolicy requireNbfPolicy "https://composed.test" "account-web" "composed-api" "composed-key-v1")
    defaultNbfConfiguration <-
      orFail
        "emission-derived nbf configuration"
        (mkComposedJwtConfigurationWithPolicy allowNbfByDefaultPolicy "https://composed.test" "account-web" "composed-api" "composed-key-v1")
    requiredNbfRuntime <- orFail "required nbf runtime" (loadComposedJwtRuntime requiredNbfConfiguration namedSigningKey verificationKeys)
    defaultNbfRuntime <- orFail "default nbf runtime" (loadComposedJwtRuntime defaultNbfConfiguration namedSigningKey verificationKeys)
    tokenWithoutNbf <-
      orMint
        =<< issueComposedApiToken requiredNbfRuntime "client-1" (requiredTestScopes ["catalog:read"]) issuedAt expiresAt
    tokenWithNbfOptional <-
      orMint
        =<< issueComposedApiToken defaultNbfRuntime "client-1" (requiredTestScopes ["catalog:read"]) issuedAt expiresAt
    let AuthenticationProofVerifier verifyRequiredNbf = composedApiProofVerifierWithClock (pure fixedNow) requiredNbfRuntime Nothing Nothing
        AuthenticationProofVerifier verifyDerivedNbf = composedApiProofVerifierWithClock (pure fixedNow) defaultNbfRuntime Nothing Nothing
    explicitlyRequiredButMissing <- verifyRequiredNbf (jwtProofFromCookie tokenWithoutNbf)
    unsetFollowsDisabledEmission <- verifyDerivedNbf (jwtProofFromCookie tokenWithNbfOptional)
    expectAll
      ( (explicitlyRequiredButMissing `shouldSatisfy` isRejected)
          :| [unsetFollowsDisabledEmission `shouldSatisfy` isRight]
      )

  it "allows explicitly optional exp and nbf when absent but still validates a present exp" $ do
    signingKey <- JoseJwk.genJWK (JwaJwk.RSAGenParam 1024)
    let activeKeyId = "composed-key-v1"
        namedSigningKey = signingKey & JoseJwk.jwkKid ?~ activeKeyId
        verificationKeys = JoseJwk.JWKSet [namedSigningKey]
        fixedNow = posixSecondsToUTCTime 1735689600
        policySettings =
          defaultComposedJwtPolicySettings
            { composedPolicyProvideNotBefore = False,
              composedApiPresencePolicy =
                (composedApiPresencePolicy defaultComposedJwtPolicySettings)
                  { HarchWeb.jwtExpirationPresence = HarchWeb.AllowAbsence,
                    HarchWeb.jwtNotBeforePresence = HarchWeb.AllowAbsence
                  }
            }
    configuration <-
      orFail
        "optional temporal-claim configuration"
        (mkComposedJwtConfigurationWithPolicy policySettings "https://composed.test" "account-web" "composed-api" activeKeyId)
    runtime <- orFail "optional temporal-claim runtime" (loadComposedJwtRuntime configuration namedSigningKey verificationKeys)
    let header :: HarchWeb.JWSHeader RequiredProtection
        header =
          JoseJws.newJWSHeaderProtected JwaJws.RS256
            & JoseJws.kid ?~ HeaderParam RequiredProtection activeKeyId
        commonClaims =
          [ "iss" Aeson..= ("https://composed.test" :: Text),
            "aud" Aeson..= ["composed-api" :: Text],
            "sub" Aeson..= ("client-1" :: Text),
            "scope" Aeson..= ("catalog:read" :: Text)
          ]
        claimsWithExpiration maybeExpiration =
          Aeson.object (commonClaims <> maybe [] (\expiration -> ["exp" Aeson..= expiration]) maybeExpiration)
        signClaims = HarchWeb.signJwt (HarchWeb.joseJwtSigner namedSigningKey) header
        AuthenticationProofVerifier verifyApi = composedApiProofVerifierWithClock (pure fixedNow) runtime Nothing Nothing
    missingTemporalToken <-
      orMint . either (const (Left ComposedJwtIssueFailed)) Right
        =<< signClaims (claimsWithExpiration Nothing)
    expiredPresentToken <-
      orMint . either (const (Left ComposedJwtIssueFailed)) Right
        =<< signClaims (claimsWithExpiration (Just (Jwt.NumericDate (posixSecondsToUTCTime 1735689599))))
    missingTemporalResult <- verifyApi (jwtProofFromCookie missingTemporalToken)
    expiredPresentResult <- verifyApi (jwtProofFromCookie expiredPresentToken)
    expectAll
      ( (missingTemporalResult `shouldBe` Right (ComposedApiClaims "client-1" ["catalog:read"]))
          :| [expiredPresentResult `shouldSatisfy` isRejected]
      )

  it "rejects negative profile skew during startup validation" $ do
    let invalidPolicy = defaultComposedJwtPolicySettings {composedPolicyClockSkewMinutes = -1}
    mkComposedJwtConfigurationWithPolicy invalidPolicy "https://composed.test" "account-web" "composed-api" "composed-key-v1"
      `shouldSatisfy` isConfigurationError ComposedJwtClockSkewInvalid

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

isRight :: Either failure value -> Bool
isRight result =
  case result of
    Left _ -> False
    Right _ -> True

isRejected :: Either ProofVerificationFailure value -> Bool
isRejected = not . isRight

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
