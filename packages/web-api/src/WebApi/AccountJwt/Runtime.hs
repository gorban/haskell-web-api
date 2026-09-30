{-# LANGUAGE ScopedTypeVariables #-}

-- | Application-owned RS256 account-session JWT startup and issuance runtime.
--
-- Harch owns the generic compact-JWT signing and verification boundary. This
-- module owns the application-specific configuration-to-runtime lifecycle:
-- it validates deployment inputs, reads and proves the selected RS256 key
-- pair before traffic is accepted, then issues account-session tokens from
-- that already-proven runtime.
--
-- Decision (AccountJwt module split, 2026-09-19): retain 'WebApi.AccountJwt' as the public
-- admission facade and retain Harch's generic JWT verification boundary.
-- Grouping configuration, key proof, and issuance here gives the explicit
-- 'AccountJwtConfiguration' to 'AccountJwtRuntime' lifecycle one owner. The
-- facade keeps cookie-or-bearer source handling, claim interpretation, and
-- durable-session-to-principal admission, so this split neither adds a second
-- credential parser nor changes which source reaches a request context.
--
-- The startup proof remains application-owned because only this package selects
-- the active signing key and verification JWK set. It proves that the active
-- private key can issue an RS256 compact proof accepted by that set before a
-- listener accepts login traffic.
--
-- Decision (configurable registered JWT claims, 2026-09-30): extend this same
-- runtime with optional issuer/audience generation, default-on @nbf@, and one
-- startup-resolved presence/skew policy. The generic jose verifier remains the
-- only signature and present-claim validator. The web profile always emits
-- its durable session expiry. @ACCOUNT_JWT_REQUIRE_EXP/NBF/ISS/AUD@ preserve
-- unset versus explicit false, while @JWT_MAX_CLOCK_SKEW_MINUTES@ accepts any
-- nonnegative integer minute value. See @docs/design-guidance.md@ and the R3
-- records in @TASKS/pr-review-2026-09-30.md@. This runtime now crosses the
-- module-health line/import threshold; the focused boundary review in
-- @TASKS/follow-up-matches-pattern-quality.md@ owns any cohesive follow-up
-- split without changing this startup-proof contract.
module WebApi.AccountJwt.Runtime
  ( AccountJwtConfiguration,
    AccountJwtRawConfiguration (..),
    AccountJwtSignerBuilder,
    AccountJwtConfigurationError (..),
    AccountJwtIssueError (..),
    AccountJwtIssuer (..),
    AccountJwtLoadError (..),
    AccountJwtRuntime,
    SharedJwtIssuance (..),
    accountJwtIssuerFromRuntime,
    accountJwtRuntimeProofExtractor,
    accountJwtRuntimeProofVerifier,
    accountJwtRuntimeProofVerifierWithAcceptance,
    accountJwtRuntimeProofVerifierWithClock,
    accountJwtRuntimeSharedIssuance,
    loadAccountJwtRuntime,
    loadAccountJwtRuntimeWithSigner,
    mkAccountJwtConfiguration,
    unavailableAccountJwtIssuer,
  )
where

import Control.Exception (IOException, try)
import Control.Lens (matching, review, (&), (.~), (?~), (^.))
import Control.Monad.Except (ExceptT (..), runExceptT)
import Core.Control.Error (liftEitherWith)
import Crypto.JOSE.Header (HeaderParam (..), RequiredProtection (..))
import Crypto.JOSE.JWA.JWK qualified as JwaJwk
import Crypto.JOSE.JWA.JWS qualified as JwaJws
import Crypto.JOSE.JWK qualified as JoseJwk
import Crypto.JOSE.JWS qualified as JoseJws
import Crypto.JWT qualified as Jwt
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as ByteString
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word64)
import HarchWeb qualified
import HarchWeb.Account qualified as Account
import HarchWeb.Session (OpaqueSession (..))
import HarchWeb.Session qualified as Session
import HarchWeb.Time (UnixTimeNanoseconds, unixTimeNanosecondsValue)
import Text.Show (showListWith)

data AccountJwtConfiguration = AccountJwtConfiguration
  { accountJwtIssuer :: Maybe ValidatedStringOrUri,
    accountJwtAudience :: Maybe ValidatedStringOrUri,
    accountJwtProvideNotBefore :: Bool,
    accountJwtRequiredClaims :: HarchWeb.JwtRequiredClaims,
    accountJwtClockSkew :: HarchWeb.JwtClockSkew,
    accountJwtActiveKeyId :: Text,
    accountJwtSigningJwkFile :: FilePath,
    accountJwtVerificationJwkSetFile :: FilePath,
    accountJwtCookiePolicy :: HarchWeb.AuthenticationCookiePolicy
  }

-- | Deployment-authored inputs before validation.  Keeping each role named
-- prevents issuer/audience, signing/verification location, and cookie values
-- from being transposed at a call site while retaining one pure validation
-- rail into 'AccountJwtConfiguration'.
--
-- Decision (secure login and admission, 2026-09-05; configurable claims,
-- 2026-09-30): this is a cohesive application configuration value, not
-- ambient startup state. The account-JWT adapter owns validation; the config
-- loader preserves unset booleans before the pure presence resolver runs here.
data AccountJwtRawConfiguration = AccountJwtRawConfiguration
  { rawAccountJwtIssuer :: Maybe Text,
    rawAccountJwtAudience :: Maybe Text,
    rawAccountJwtProvideNotBefore :: Bool,
    rawAccountJwtClaimPresencePolicy :: HarchWeb.JwtClaimPresencePolicy,
    rawAccountJwtClockSkewMinutes :: Integer,
    rawAccountJwtActiveKeyId :: Text,
    rawAccountJwtSigningJwkFile :: FilePath,
    rawAccountJwtVerificationJwkSetFile :: FilePath,
    rawAccountJwtCookieName :: Text,
    rawAccountJwtCookieMaxAgeSeconds :: Word64
  }

instance Eq AccountJwtConfiguration where
  left == right =
    (validatedStringOrUriText <$> accountJwtIssuer left) == (validatedStringOrUriText <$> accountJwtIssuer right)
      && (validatedStringOrUriText <$> accountJwtAudience left) == (validatedStringOrUriText <$> accountJwtAudience right)
      && accountJwtProvideNotBefore left == accountJwtProvideNotBefore right
      && accountJwtRequiredClaims left == accountJwtRequiredClaims right
      && accountJwtClockSkew left == accountJwtClockSkew right
      && accountJwtActiveKeyId left == accountJwtActiveKeyId right
      && accountJwtSigningJwkFile left == accountJwtSigningJwkFile right
      && accountJwtVerificationJwkSetFile left == accountJwtVerificationJwkSetFile right
      && accountJwtCookiePolicy left == accountJwtCookiePolicy right

instance Show AccountJwtConfiguration where
  showsPrec depth configuration =
    showParen (depth > 10) $
      showString "AccountJwtConfiguration {accountJwtIssuerText = "
        . shows (validatedStringOrUriText <$> accountJwtIssuer configuration)
        . showString ", accountJwtAudienceText = "
        . shows (validatedStringOrUriText <$> accountJwtAudience configuration)
        . showString ", accountJwtProvideNotBefore = "
        . shows (accountJwtProvideNotBefore configuration)
        . showString ", accountJwtRequiredClaims = "
        . shows (accountJwtRequiredClaims configuration)
        . showString ", accountJwtClockSkewMinutes = "
        . shows (HarchWeb.jwtClockSkewMinutes (accountJwtClockSkew configuration))
        . showString ", accountJwtActiveKeyId = "
        . shows (accountJwtActiveKeyId configuration)
        . showString ", accountJwtSigningJwkFile = "
        . shows (accountJwtSigningJwkFile configuration)
        . showString ", accountJwtVerificationJwkSetFile = "
        . shows (accountJwtVerificationJwkSetFile configuration)
        . showString ", accountJwtCookiePolicy = "
        . shows (accountJwtCookiePolicy configuration)
        . showChar '}'

  showList = showListWith shows

data ValidatedStringOrUri = ValidatedStringOrUri
  { validatedStringOrUriText :: Text,
    validatedStringOrUriValue :: Jwt.StringOrURI
  }

data AccountJwtConfigurationError
  = AccountJwtIssuerInvalid
  | AccountJwtAudienceInvalid
  | AccountJwtActiveKeyIdInvalid
  | AccountJwtSigningJwkFileInvalid
  | AccountJwtVerificationJwkSetFileInvalid
  | AccountJwtCookiePolicyInvalid
  | AccountJwtClockSkewInvalid
  deriving (Eq, Show)

-- | Failure classes intentionally contain no JWK bytes, JWT text, or
-- filesystem exception detail. Startup reports a stable operator action while
-- the deployment-owned secret material stays out of diagnostics.
data AccountJwtLoadError
  = AccountJwtSigningJwkUnreadable
  | AccountJwtVerificationJwkSetUnreadable
  | AccountJwtSigningJwkMalformed
  | AccountJwtVerificationJwkSetMalformed
  | AccountJwtSigningKeyIdMismatch
  | AccountJwtVerificationKeyMissing
  | AccountJwtSigningKeyNotRsaPrivate
  | AccountJwtVerificationKeyNotRsa
  | AccountJwtSigningKeyUnusable
  | AccountJwtVerificationKeyDoesNotMatchSigningKey
  deriving (Eq, Show)

data AccountJwtIssueError = AccountJwtIssueFailed
  deriving (Eq, Show)

data AccountJwtRuntime = AccountJwtRuntime
  { runtimeAccountJwtConfiguration :: AccountJwtConfiguration,
    runtimeAccountJwtVerificationKeys :: HarchWeb.JWKSet,
    runtimeAccountJwtSigner :: HarchWeb.JwtSigner AccountJwtIssueError Jwt.ClaimsSet,
    -- | Retained only because the scoped API-authentication design's reference profile deliberately issues
    -- account and API-client bearer tokens from one already-startup-proven
    -- RS256 key pair (see 'SharedJwtIssuance'). It is never rendered by this
    -- type's redacted 'Show' instance.
    runtimeAccountJwtSigningKey :: HarchWeb.JWK
  }

type AccountJwtSignerBuilder = HarchWeb.JWK -> HarchWeb.JwtSigner AccountJwtIssueError Jwt.ClaimsSet

-- | Never show an in-memory signing key through a failed assertion or an
-- application startup exception.
instance Show AccountJwtRuntime where
  showsPrec depth _ = showParen (depth > 10) (showString "AccountJwtRuntime <redacted>")

  showList = showListWith shows

-- | The small account-workflow capability needed after durable session
-- creation. Keeping it distinct from 'AccountJwtRuntime' means a workflow
-- cannot inspect verification keys or reimplement the authentication guard.
data AccountJwtIssuer = AccountJwtIssuer
  { accountJwtCookie :: HarchWeb.AuthenticationCookiePolicy,
    issueAccountSessionJwt :: OpaqueSession Account.AccountId -> IO (Either AccountJwtIssueError HarchWeb.EncodedJwt)
  }

-- | Configuration parses and retains issuer/audience values without reading
-- secret files; the application reads both only during startup before
-- accepting traffic. Keeping the parsed values here avoids a later
-- request-path parse while account identifiers keep their separate domain
-- validation boundary. The fixed cookie policy itself validates its host-only
-- attributes in Harch.
mkAccountJwtConfiguration :: AccountJwtRawConfiguration -> Either AccountJwtConfigurationError AccountJwtConfiguration
mkAccountJwtConfiguration rawConfiguration = do
  validIssuer <- traverse (requireStringOrUri AccountJwtIssuerInvalid) (rawAccountJwtIssuer rawConfiguration)
  validAudience <- traverse (requireStringOrUri AccountJwtAudienceInvalid) (rawAccountJwtAudience rawConfiguration)
  clockSkew <-
    case HarchWeb.mkJwtClockSkewMinutes (rawAccountJwtClockSkewMinutes rawConfiguration) of
      Left _ -> Left AccountJwtClockSkewInvalid
      Right value -> Right value
  let requiredClaims =
        HarchWeb.resolveJwtRequiredClaims
          HarchWeb.JwtClaimGeneration
            { HarchWeb.jwtGenerationEmitsNotBefore = rawAccountJwtProvideNotBefore rawConfiguration,
              HarchWeb.jwtGenerationConfiguresIssuer = isJust validIssuer,
              HarchWeb.jwtGenerationConfiguresAudience = isJust validAudience
            }
          (rawAccountJwtClaimPresencePolicy rawConfiguration)
  validKeyId <- requireBounded AccountJwtActiveKeyIdInvalid (rawAccountJwtActiveKeyId rawConfiguration)
  validSigningFile <- requireFilePath AccountJwtSigningJwkFileInvalid (rawAccountJwtSigningJwkFile rawConfiguration)
  validVerificationFile <- requireFilePath AccountJwtVerificationJwkSetFileInvalid (rawAccountJwtVerificationJwkSetFile rawConfiguration)
  cookiePolicy <-
    case HarchWeb.mkAuthenticationCookiePolicy (rawAccountJwtCookieName rawConfiguration) (rawAccountJwtCookieMaxAgeSeconds rawConfiguration) of
      Left _ -> Left AccountJwtCookiePolicyInvalid
      Right value -> Right value
  pure
    AccountJwtConfiguration
      { accountJwtIssuer = validIssuer,
        accountJwtAudience = validAudience,
        accountJwtProvideNotBefore = rawAccountJwtProvideNotBefore rawConfiguration,
        accountJwtRequiredClaims = requiredClaims,
        accountJwtClockSkew = clockSkew,
        accountJwtActiveKeyId = validKeyId,
        accountJwtSigningJwkFile = validSigningFile,
        accountJwtVerificationJwkSetFile = validVerificationFile,
        accountJwtCookiePolicy = cookiePolicy
      }
  where
    requireStringOrUri errorValue value
      | Text.null value = Left errorValue
      | otherwise =
          case matching Jwt.stringOrUri (Text.unpack value) of
            Left _ -> Left errorValue
            Right parsedValue -> Right (ValidatedStringOrUri value parsedValue)
    requireBounded errorValue value
      | Text.null value || Text.length value > 128 = Left errorValue
      | otherwise = Right value
    requireFilePath errorValue value
      | null value = Left errorValue
      | otherwise = Right value

loadAccountJwtRuntime :: AccountJwtConfiguration -> IO (Either AccountJwtLoadError AccountJwtRuntime)
loadAccountJwtRuntime = loadAccountJwtRuntimeWithSigner defaultAccountJwtSigner

loadAccountJwtRuntimeWithSigner :: AccountJwtSignerBuilder -> AccountJwtConfiguration -> IO (Either AccountJwtLoadError AccountJwtRuntime)
loadAccountJwtRuntimeWithSigner signerBuilder configuration = runExceptT $ do
  signingKey <- ExceptT (readJwk (accountJwtSigningJwkFile configuration))
  verificationKeys <- ExceptT (readJwkSet (accountJwtVerificationJwkSetFile configuration))
  -- The structural rail runs before the cryptographic round trip, so malformed
  -- key shapes fail with their precise configuration classification.
  (structurallyValidSigningKey, structurallyValidVerificationKeys) <-
    ExceptT (pure (validateRuntimeKeys configuration signingKey verificationKeys))
  let signer = signerBuilder structurallyValidSigningKey
  validatedVerificationKeys <-
    liftEitherWith id (validateRuntimeKeyPair configuration signer structurallyValidVerificationKeys)
  pure
    AccountJwtRuntime
      { runtimeAccountJwtConfiguration = configuration,
        runtimeAccountJwtVerificationKeys = validatedVerificationKeys,
        runtimeAccountJwtSigner = signer,
        runtimeAccountJwtSigningKey = structurallyValidSigningKey
      }

defaultAccountJwtSigner :: AccountJwtSignerBuilder
defaultAccountJwtSigner signingKey =
  HarchWeb.mapJwtSignerError (const AccountJwtIssueFailed) (HarchWeb.joseJwtSigner signingKey)

readJwk :: FilePath -> IO (Either AccountJwtLoadError HarchWeb.JWK)
readJwk path = do
  fileResult <- (try (ByteString.readFile path) :: IO (Either IOException ByteString.ByteString))
  pure $
    case fileResult of
      Left _ -> Left AccountJwtSigningJwkUnreadable
      Right bytes ->
        case Aeson.eitherDecodeStrict' bytes of
          Left _ -> Left AccountJwtSigningJwkMalformed
          Right key -> Right key

readJwkSet :: FilePath -> IO (Either AccountJwtLoadError HarchWeb.JWKSet)
readJwkSet path = do
  fileResult <- (try (ByteString.readFile path) :: IO (Either IOException ByteString.ByteString))
  pure $
    case fileResult of
      Left _ -> Left AccountJwtVerificationJwkSetUnreadable
      Right bytes ->
        case Aeson.eitherDecodeStrict' bytes of
          Left _ -> Left AccountJwtVerificationJwkSetMalformed
          Right keys -> Right keys

validateRuntimeKeys :: AccountJwtConfiguration -> HarchWeb.JWK -> HarchWeb.JWKSet -> Either AccountJwtLoadError (HarchWeb.JWK, HarchWeb.JWKSet)
validateRuntimeKeys configuration signingKey verificationKeys@(JoseJwk.JWKSet keys)
  | signingKey ^. JoseJwk.jwkKid /= Just (accountJwtActiveKeyId configuration) = Left AccountJwtSigningKeyIdMismatch
  | not (rsaPrivateJwk signingKey) = Left AccountJwtSigningKeyNotRsaPrivate
  | otherwise =
      case find ((== Just (accountJwtActiveKeyId configuration)) . (^. JoseJwk.jwkKid)) keys of
        Nothing -> Left AccountJwtVerificationKeyMissing
        Just verificationKey
          | rsaJwk verificationKey -> Right (signingKey, verificationKeys)
          | otherwise -> Left AccountJwtVerificationKeyNotRsa

-- | Verify the concrete deployment-owned key pairing with Harch's selected
-- RS256 verifier before constructing a runtime. This is intentionally here,
-- not in Harch: only the application chooses the active signing key and the
-- compatible verification set.
validateRuntimeKeyPair :: AccountJwtConfiguration -> HarchWeb.JwtSigner AccountJwtIssueError Jwt.ClaimsSet -> HarchWeb.JWKSet -> IO (Either AccountJwtLoadError HarchWeb.JWKSet)
validateRuntimeKeyPair configuration signer verificationKeys = do
  now <- getCurrentTime
  issued <- HarchWeb.signJwt signer validationHeader (validationClaims now)
  case issued of
    Left AccountJwtIssueFailed -> pure (Left AccountJwtSigningKeyUnusable)
    Right proof -> do
      let HarchWeb.AuthenticationProofVerifier verifyProof =
            HarchWeb.jwtProofVerifier
              (Jwt.defaultJWTValidationSettings (const True))
              (HarchWeb.mkJwtAllowedAlgorithms (HarchWeb.JwtRs256 :| []))
              verificationKeys
              Right
      verified <- verifyProof proof
      pure $
        case verified of
          Left _ -> Left AccountJwtVerificationKeyDoesNotMatchSigningKey
          Right _ -> Right verificationKeys
  where
    validationHeader :: HarchWeb.JWSHeader HarchWeb.RequiredProtection
    validationHeader =
      JoseJws.newJWSHeaderProtected JwaJws.RS256
        & JoseJws.kid ?~ HeaderParam RequiredProtection (accountJwtActiveKeyId configuration)
    -- Include an ordinary registered claim so this startup proof exercises the
    -- verifier's claim-validation callback as well as its RS256 key pairing.
    -- The callback deliberately accepts the probe's application-independent
    -- audience; request authentication below applies the configured audience.
    validationClaims :: UTCTime -> Jwt.ClaimsSet
    validationClaims now =
      Jwt.emptyClaimsSet
        & Jwt.claimExp ?~ Jwt.NumericDate (addUTCTime 60 now)

rsaPrivateJwk :: HarchWeb.JWK -> Bool
rsaPrivateJwk key =
  case key ^. JoseJwk.jwkMaterial of
    JwaJwk.RSAKeyMaterial rsaParameters -> isJust (rsaParameters ^. JwaJwk.rsaPrivateKeyParameters)
    _ -> False

rsaJwk :: HarchWeb.JWK -> Bool
rsaJwk key =
  case key ^. JoseJwk.jwkMaterial of
    JwaJwk.RSAKeyMaterial _ -> True
    _ -> False

accountJwtIssuerFromRuntime :: AccountJwtRuntime -> AccountJwtIssuer
accountJwtIssuerFromRuntime runtime =
  AccountJwtIssuer
    { accountJwtCookie = accountJwtCookiePolicy configuration,
      issueAccountSessionJwt = issueJwtForSession runtime
    }
  where
    configuration = runtimeAccountJwtConfiguration runtime

-- | The startup-proven RS256 key pair and issuer/audience/key-ID, exposed so
-- another principal kind can issue its own claims subtype from the exact same
-- already-validated key instead of loading and re-proving a second key pair.
--
-- Decision record (scoped API authentication, 2026-09-16): @web-api@ uses one issuer,
-- one RS256 signing/JWKS key set, and one audience for both account and
-- API-client bearer tokens, while keeping each principal's claims distinct.
-- 'AccountJwtRuntime' already retains the one structurally- and
-- cryptographically-validated signing key; this accessor lets the
-- API-client token workflow build its own 'HarchWeb.joseJwtSigner' over a
-- 'Crypto.JWT.ClaimsSet' subtype carrying a scope claim, rather than widening
-- the account signer's fixed 'Crypto.JWT.ClaimsSet' claims type or using
-- @jose@'s deprecated 'Crypto.JWT.unregisteredClaims'/'Crypto.JWT.addClaim'.
-- This does not add a second JWK file, a second startup proof, or a second
-- authentication dispatcher.
data SharedJwtIssuance = SharedJwtIssuance
  { sharedJwtSigningKey :: HarchWeb.JWK,
    sharedJwtIssuer :: Maybe Jwt.StringOrURI,
    sharedJwtAudience :: Maybe Jwt.StringOrURI,
    sharedJwtProvideNotBefore :: Bool,
    sharedJwtActiveKeyId :: Text
  }

accountJwtRuntimeSharedIssuance :: AccountJwtRuntime -> SharedJwtIssuance
accountJwtRuntimeSharedIssuance runtime =
  SharedJwtIssuance
    { sharedJwtSigningKey = runtimeAccountJwtSigningKey runtime,
      sharedJwtIssuer = validatedStringOrUriValue <$> accountJwtIssuer configuration,
      sharedJwtAudience = validatedStringOrUriValue <$> accountJwtAudience configuration,
      sharedJwtProvideNotBefore = accountJwtProvideNotBefore configuration,
      sharedJwtActiveKeyId = accountJwtActiveKeyId configuration
    }
  where
    configuration = runtimeAccountJwtConfiguration runtime

issueJwtForSession :: AccountJwtRuntime -> OpaqueSession Account.AccountId -> IO (Either AccountJwtIssueError HarchWeb.EncodedJwt)
issueJwtForSession runtime session =
  HarchWeb.signJwt signer header (claimsForSession configuration session)
  where
    configuration = runtimeAccountJwtConfiguration runtime
    signer = runtimeAccountJwtSigner runtime
    header :: HarchWeb.JWSHeader HarchWeb.RequiredProtection
    header =
      JoseJws.newJWSHeaderProtected JwaJws.RS256
        & JoseJws.kid ?~ HeaderParam RequiredProtection (accountJwtActiveKeyId configuration)

claimsForSession :: AccountJwtConfiguration -> OpaqueSession Account.AccountId -> Jwt.ClaimsSet
claimsForSession configuration session =
  addAudience (addIssuer Jwt.emptyClaimsSet)
    & Jwt.claimSub ?~ accountIdStringOrUri (sessionPrincipal session)
    & Jwt.claimIat ?~ numericDate (sessionIssuedAtNanoseconds session)
    & setNotBefore
    & Jwt.claimExp ?~ numericDate (sessionExpiresAtNanoseconds session)
    & Jwt.claimJti ?~ Session.sessionIdText (sessionId session)
  where
    addIssuer claims = maybe claims (\issuer -> claims & Jwt.claimIss ?~ validatedStringOrUriValue issuer) (accountJwtIssuer configuration)
    addAudience claims = maybe claims (\audience -> claims & Jwt.claimAud ?~ Jwt.Audience [validatedStringOrUriValue audience]) (accountJwtAudience configuration)
    setNotBefore claims
      | accountJwtProvideNotBefore configuration = claims & Jwt.claimNbf ?~ numericDate (sessionIssuedAtNanoseconds session)
      | otherwise = claims

accountIdStringOrUri :: Account.AccountId -> Jwt.StringOrURI
accountIdStringOrUri = review Jwt.string . Account.accountIdText

numericDate :: UnixTimeNanoseconds -> Jwt.NumericDate
numericDate instant =
  Jwt.NumericDate
    ( posixSecondsToUTCTime
        (fromIntegral (unixTimeNanosecondsValue instant) / 1000000000)
    )

-- | The cookie-or-bearer JWT proof extractor built from this runtime's own
-- deployment-authored cookie policy. Exposed so the scoped API-authentication
-- design's combined
-- account-or-API-client-bearer profile (securing @\/api\/second@) accepts the
-- exact same session cookie as every account-protected page/action, instead
-- of a second cookie declaration.
accountJwtRuntimeProofExtractor :: AccountJwtRuntime -> HarchWeb.AuthenticationProofExtractor route context authorization HarchWeb.JwtProof
accountJwtRuntimeProofExtractor runtime =
  HarchWeb.cookieOrBearerJwtExtractor
    (HarchWeb.authenticationCookieName (accountJwtCookiePolicy (runtimeAccountJwtConfiguration runtime)))
    authenticationProofMaximumBytes

-- | Verify a compact JWT against this runtime's already-startup-proven RS256
-- verification keys and validated issuer/audience, parameterized only by the
-- caller's own claims projection. Mirrors 'accountJwtRuntimeSharedIssuance':
-- a second principal kind reuses this runtime's verification material
-- instead of loading and re-proving a second key set. The account pipeline
-- above is this accessor's own first caller, with 'parseAccountJwtClaims';
-- the scoped API-authentication combined profile is its second, with a claims-shape-
-- discriminating projection of its own.
accountJwtRuntimeProofVerifier :: AccountJwtRuntime -> (Jwt.ClaimsSet -> Either HarchWeb.JwtClaimsError claims) -> HarchWeb.AuthenticationProofVerifier HarchWeb.JwtProof claims
accountJwtRuntimeProofVerifier runtime = accountJwtRuntimeProofVerifierWithAcceptance runtime Nothing Nothing

-- | Use optional application-owned accepted-value predicates. A supplied
-- predicate replaces the configured-generation singleton default and never
-- widens it by union. Predicates are pure functions over jose's validated
-- 'Jwt.StringOrURI' representation.
accountJwtRuntimeProofVerifierWithAcceptance :: AccountJwtRuntime -> Maybe (Jwt.StringOrURI -> Bool) -> Maybe (Jwt.StringOrURI -> Bool) -> (Jwt.ClaimsSet -> Either HarchWeb.JwtClaimsError claims) -> HarchWeb.AuthenticationProofVerifier HarchWeb.JwtProof claims
accountJwtRuntimeProofVerifierWithAcceptance = accountJwtRuntimeProofVerifierWithClock getCurrentTime

-- | Clock-injected version of the same reference verifier for deterministic
-- config-to-JOSE acceptance checks. Production uses the system clock through
-- 'accountJwtRuntimeProofVerifierWithAcceptance'.
accountJwtRuntimeProofVerifierWithClock :: IO UTCTime -> AccountJwtRuntime -> Maybe (Jwt.StringOrURI -> Bool) -> Maybe (Jwt.StringOrURI -> Bool) -> (Jwt.ClaimsSet -> Either HarchWeb.JwtClaimsError claims) -> HarchWeb.AuthenticationProofVerifier HarchWeb.JwtProof claims
accountJwtRuntimeProofVerifierWithClock readClock runtime acceptedIssuers acceptedAudiences claimsProjection =
  HarchWeb.AuthenticationProofVerifier $ \proof ->
    HarchWeb.verifyAuthenticationProof
      ( HarchWeb.jwtProofVerifierWithClock
          readClock
          validationSettings
          (accountJwtRequiredClaims configuration)
          (HarchWeb.mkJwtAllowedAlgorithms (HarchWeb.JwtRs256 :| []))
          (runtimeAccountJwtVerificationKeys runtime)
          claimsProjection
      )
      (HarchWeb.jwtProofEncodedJwt proof)
  where
    configuration = runtimeAccountJwtConfiguration runtime
    defaultIssuerPredicate = maybe (const False) ((==) . validatedStringOrUriValue) (accountJwtIssuer configuration)
    defaultAudiencePredicate = maybe (const False) ((==) . validatedStringOrUriValue) (accountJwtAudience configuration)
    validationSettings =
      HarchWeb.jwtValidationSettingsWithClockSkew
        (accountJwtClockSkew configuration)
        ( Jwt.defaultJWTValidationSettings (fromMaybe defaultAudiencePredicate acceptedAudiences)
            & Jwt.jwtValidationSettingsIssuerPredicate .~ fromMaybe defaultIssuerPredicate acceptedIssuers
        )

authenticationProofMaximumBytes :: HarchWeb.AuthenticationProofMaximumBytes
authenticationProofMaximumBytes = HarchWeb.requiredAuthenticationProofMaximumBytesOrDie 8192

unavailableAccountJwtIssuer :: AccountJwtIssuer
unavailableAccountJwtIssuer =
  AccountJwtIssuer
    { accountJwtCookie = unavailableCookiePolicy,
      issueAccountSessionJwt = \_ -> pure (Left AccountJwtIssueFailed)
    }
  where
    unavailableCookiePolicy =
      HarchWeb.requiredAuthenticationCookiePolicyOrDie "__Host-harch-session" 28800
