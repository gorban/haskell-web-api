{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Application-owned RS256 account-session JWT configuration and admission.
--
-- Harch verifies the compact proof and standard claim validity, but this
-- module owns all application meaning: startup JWK files, issuer/audience,
-- minimal account/session claims, and the durable-session lookup which makes
-- logout revocation effective before token expiry.
--
-- Decision (AHI-4C follow-up, 2026-09-04): startup proves the configured
-- active private key can issue an RS256 compact proof that the configured
-- verification JWK set accepts. Harch owns the generic JOSE operations, while
-- this application-owned adapter alone knows the intended signing/verification
-- pairing. This catches mismatched or structurally valid but cryptographically
-- unusable deployment keys before a listener accepts login traffic; it does
-- not create a second authentication or JWT implementation.
--
-- Decision (AHI-4C follow-up, 2026-09-04): the selected signing capability is
-- constructed at startup from the validated active key, then retained by the
-- runtime behind Harch's 'HarchWeb.JwtSigner' boundary. A non-JOSE deployment
-- may supply that constructor, but it must pass the same startup proof and
-- retains its ordinary session-issuance error rail. This keeps signing
-- pluggable without giving application routes a second cryptographic path.
-- Account identifiers use Harch's ASCII opaque-token alphabet, which excludes
-- @:@; they therefore occupy the JWT subject's explicit string form rather
-- than being dynamically re-parsed as a possible URI.
module WebApi.AccountJwt
  ( AccountJwtConfiguration,
    AccountJwtSignerBuilder,
    AccountJwtConfigurationError (..),
    AccountJwtIssueError (..),
    AccountJwtIssuer (..),
    AccountJwtLoadError (..),
    AccountJwtRuntime,
    accountJwtAuthenticationPipeline,
    accountJwtIssuerFromRuntime,
    loadAccountJwtRuntime,
    loadAccountJwtRuntimeWithSigner,
    mkAccountJwtConfiguration,
    unavailableAccountJwtIssuer,
  )
where

import Control.Exception (IOException, try)
import Control.Lens (matching, preview, review, (&), (.~), (?~), (^.))
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
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word64)
import HarchWeb qualified
import HarchWeb.Account qualified as Account
import HarchWeb.Session (OpaqueSession (..), SessionId, mkSessionId)
import HarchWeb.Session qualified as Session
import HarchWeb.Time (UnixTimeNanoseconds, unixTimeNanosecondsValue)
import Network.HTTP.Types qualified as Http
import Text.Show (showListWith)
import WebApi.AccountPrincipal (AccountPrincipal, mkAccountPrincipal)
import WebApi.Route (AppRequestContext (..), AppRoute (LoginRoute))
import WebApi.Session (AccountSessionStore (..))

data AccountJwtConfiguration = AccountJwtConfiguration
  { accountJwtIssuer :: ValidatedStringOrUri,
    accountJwtAudience :: ValidatedStringOrUri,
    accountJwtActiveKeyId :: Text,
    accountJwtSigningJwkFile :: FilePath,
    accountJwtVerificationJwkSetFile :: FilePath,
    accountJwtCookiePolicy :: HarchWeb.AuthenticationCookiePolicy
  }

instance Eq AccountJwtConfiguration where
  left == right =
    validatedStringOrUriText (accountJwtIssuer left) == validatedStringOrUriText (accountJwtIssuer right)
      && validatedStringOrUriText (accountJwtAudience left) == validatedStringOrUriText (accountJwtAudience right)
      && accountJwtActiveKeyId left == accountJwtActiveKeyId right
      && accountJwtSigningJwkFile left == accountJwtSigningJwkFile right
      && accountJwtVerificationJwkSetFile left == accountJwtVerificationJwkSetFile right
      && accountJwtCookiePolicy left == accountJwtCookiePolicy right

instance Show AccountJwtConfiguration where
  showsPrec depth configuration =
    showParen (depth > 10) $
      showString "AccountJwtConfiguration {accountJwtIssuerText = "
        . shows (validatedStringOrUriText (accountJwtIssuer configuration))
        . showString ", accountJwtAudienceText = "
        . shows (validatedStringOrUriText (accountJwtAudience configuration))
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
    runtimeAccountJwtSigner :: HarchWeb.JwtSigner AccountJwtIssueError Jwt.ClaimsSet
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
mkAccountJwtConfiguration :: Text -> Text -> Text -> FilePath -> FilePath -> Text -> Word64 -> Either AccountJwtConfigurationError AccountJwtConfiguration
mkAccountJwtConfiguration issuer audience activeKeyId signingJwkFile verificationJwkSetFile cookieName cookieMaxAgeSeconds = do
  validIssuer <- requireStringOrUri AccountJwtIssuerInvalid issuer
  validAudience <- requireStringOrUri AccountJwtAudienceInvalid audience
  validKeyId <- requireBounded AccountJwtActiveKeyIdInvalid activeKeyId
  validSigningFile <- requireFilePath AccountJwtSigningJwkFileInvalid signingJwkFile
  validVerificationFile <- requireFilePath AccountJwtVerificationJwkSetFileInvalid verificationJwkSetFile
  cookiePolicy <-
    case HarchWeb.mkAuthenticationCookiePolicy cookieName cookieMaxAgeSeconds of
      Left _ -> Left AccountJwtCookiePolicyInvalid
      Right value -> Right value
  pure
    AccountJwtConfiguration
      { accountJwtIssuer = validIssuer,
        accountJwtAudience = validAudience,
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
        runtimeAccountJwtSigner = signer
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
  issued <- HarchWeb.signJwt signer validationHeader validationClaims
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
    validationClaims :: Jwt.ClaimsSet
    validationClaims =
      Jwt.emptyClaimsSet
        & Jwt.claimAud ?~ Jwt.Audience [validatedStringOrUriValue (accountJwtAudience configuration)]

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
  Jwt.emptyClaimsSet
    & Jwt.claimIss ?~ validatedStringOrUriValue (accountJwtIssuer configuration)
    & Jwt.claimAud ?~ Jwt.Audience [validatedStringOrUriValue (accountJwtAudience configuration)]
    & Jwt.claimSub ?~ accountIdStringOrUri (sessionPrincipal session)
    & Jwt.claimIat ?~ numericDate (sessionIssuedAtNanoseconds session)
    & Jwt.claimNbf ?~ numericDate (sessionIssuedAtNanoseconds session)
    & Jwt.claimExp ?~ numericDate (sessionExpiresAtNanoseconds session)
    & Jwt.claimJti ?~ Session.sessionIdText (sessionId session)

accountIdStringOrUri :: Account.AccountId -> Jwt.StringOrURI
accountIdStringOrUri = review Jwt.string . Account.accountIdText

numericDate :: UnixTimeNanoseconds -> Jwt.NumericDate
numericDate instant =
  Jwt.NumericDate
    ( posixSecondsToUTCTime
        (fromIntegral (unixTimeNanosecondsValue instant) / 1000000000)
    )

-- | Construct the root authentication rail from the immutable startup
-- runtime. A successful signature is only an intermediate fact: this
-- establishment step resolves the current durable session and checks both its
-- subject and expiration before a principal reaches the request context.
accountJwtAuthenticationPipeline :: AccountSessionStore -> IO UnixTimeNanoseconds -> AccountJwtRuntime -> HarchWeb.AuthenticationPipeline AppRoute AppRequestContext () HarchWeb.EncodedJwt AccountJwtClaims AccountPrincipal ()
accountJwtAuthenticationPipeline sessionStore readClock runtime =
  HarchWeb.AuthenticationPipeline
    { HarchWeb.authenticationProofExtractor =
        HarchWeb.cookieJwtExtractor
          (HarchWeb.authenticationCookieName cookiePolicy)
          authenticationProofMaximumBytes,
      HarchWeb.authenticationProofVerifier =
        HarchWeb.jwtProofVerifier
          validationSettings
          (HarchWeb.mkJwtAllowedAlgorithms (HarchWeb.JwtRs256 :| []))
          (runtimeAccountJwtVerificationKeys runtime)
          parseAccountJwtClaims,
      HarchWeb.authenticationPrincipalEstablisher = establishAccountPrincipal sessionStore readClock,
      HarchWeb.authenticationAuthorization =
        HarchWeb.AuthenticationWithoutAuthorization
          (\_ -> authenticationErrorResponse Http.status503 "Authorization is not configured for this application."),
      HarchWeb.authenticationAttachPrincipal = \principal context -> context {requestAccountPrincipal = Just principal},
      HarchWeb.authenticationChallenge = accountAuthenticationChallenge,
      HarchWeb.authenticationUnavailable = \_ _ -> authenticationErrorResponse Http.status503 "Authentication is temporarily unavailable."
    }
  where
    configuration = runtimeAccountJwtConfiguration runtime
    cookiePolicy = accountJwtCookiePolicy configuration
    validationSettings =
      Jwt.defaultJWTValidationSettings (== validatedStringOrUriValue (accountJwtAudience configuration))
        & Jwt.jwtValidationSettingsIssuerPredicate .~ (== validatedStringOrUriValue (accountJwtIssuer configuration))

accountAuthenticationChallenge :: HarchWeb.EndpointRequest AppRoute AppRequestContext () -> HarchWeb.AuthenticationFailure -> HarchWeb.NonPageResponse AppRoute AppRequestContext
accountAuthenticationChallenge endpointRequest _ =
  HarchWeb.authenticationChallengeForAction endpointRequest ordinaryChallenge
  where
    requestContext = HarchWeb.requestContext (HarchWeb.endpointRouteRequest endpointRequest)
    ordinaryChallenge =
      HarchWeb.nonPageInternalRedirectResponse
        Http.status303
        HarchWeb.RouteRequest
          { HarchWeb.requestRoute = LoginRoute,
            HarchWeb.requestContext = requestContext
          }

authenticationErrorResponse :: Http.Status -> Text -> HarchWeb.NonPageResponse AppRoute AppRequestContext
authenticationErrorResponse status message =
  HarchWeb.NonPageBodyResponse
    HarchWeb.ResponseBody
      { HarchWeb.responseStatus = status,
        HarchWeb.responseContentType = "text/plain; charset=utf-8",
        HarchWeb.responseBody = message,
        HarchWeb.responseObservabilityAttributes = [],
        HarchWeb.responseLogEntries = [],
        HarchWeb.responseDatabaseOperations = []
      }

data AccountJwtClaims = AccountJwtClaims
  { accountJwtClaimAccountId :: Account.AccountId,
    accountJwtClaimSessionId :: SessionId
  }

parseAccountJwtClaims :: Jwt.ClaimsSet -> Either HarchWeb.JwtClaimsError AccountJwtClaims
parseAccountJwtClaims claims = do
  subject <- maybe (Left invalidJwtClaims) Right (claims ^. Jwt.claimSub)
  accountIdText <- maybe (Left invalidJwtClaims) Right (preview Jwt.string subject)
  accountId <- maybe (Left invalidJwtClaims) Right (Account.mkAccountId accountIdText)
  sessionIdValue <- maybe (Left invalidJwtClaims) Right (claims ^. Jwt.claimJti)
  sessionId <- maybe (Left invalidJwtClaims) Right (mkSessionId sessionIdValue)
  pure (AccountJwtClaims accountId sessionId)

invalidJwtClaims :: HarchWeb.JwtClaimsError
invalidJwtClaims =
  HarchWeb.mkJwtClaimsError
    (HarchWeb.requiredSecurityFailureCodeOrDie "account.jwt.claims-rejected")

establishAccountPrincipal :: AccountSessionStore -> IO UnixTimeNanoseconds -> HarchWeb.PrincipalEstablisher AccountJwtClaims AccountPrincipal
establishAccountPrincipal sessionStore readClock =
  HarchWeb.PrincipalEstablisher $ \claims -> do
    now <- readClock
    loadedSession <- loadAccountSession sessionStore (accountJwtClaimSessionId claims)
    pure $
      case loadedSession of
        Left _ -> Left (HarchWeb.PrincipalEstablishmentUnavailable accountSessionUnavailable)
        Right maybeSession ->
          case Session.validateSession now maybeSession of
            Session.ActiveSession session
              | sessionPrincipal session == accountJwtClaimAccountId claims ->
                  Right
                    ( mkAccountPrincipal
                        (sessionPrincipal session)
                        (sessionId session)
                        (sessionExpiresAtNanoseconds session)
                    )
              | otherwise -> Left (HarchWeb.PrincipalRejected accountSessionRejected)
            Session.MissingSession -> Left (HarchWeb.PrincipalRejected accountSessionRejected)
            Session.ExpiredSession -> Left (HarchWeb.PrincipalRejected accountSessionRejected)

accountSessionRejected :: HarchWeb.PrincipalRejection
accountSessionRejected = HarchWeb.mkPrincipalRejection knownAccountSessionRejected

accountSessionUnavailable :: HarchWeb.AuthenticationDependency
accountSessionUnavailable = HarchWeb.mkAuthenticationDependency knownAccountSessionUnavailable

knownAccountSessionRejected :: HarchWeb.SecurityFailureCode
knownAccountSessionRejected = HarchWeb.requiredSecurityFailureCodeOrDie "account.jwt.session-rejected"

knownAccountSessionUnavailable :: HarchWeb.SecurityFailureCode
knownAccountSessionUnavailable = HarchWeb.requiredSecurityFailureCodeOrDie "account.jwt.session-unavailable"

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
