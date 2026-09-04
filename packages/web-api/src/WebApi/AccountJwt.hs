{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Application-owned RS256 account-session JWT configuration and admission.
--
-- Harch verifies the compact proof and standard claim validity, but this
-- module owns all application meaning: startup JWK files, issuer/audience,
-- minimal account/session claims, and the durable-session lookup which makes
-- logout revocation effective before token expiry.
module WebApi.AccountJwt
  ( AccountJwtConfiguration,
    AccountJwtConfigurationError (..),
    AccountJwtIssueError (..),
    AccountJwtIssuer (..),
    AccountJwtLoadError (..),
    AccountJwtRuntime,
    accountJwtAuthenticationPipeline,
    accountJwtIssuerFromRuntime,
    loadAccountJwtRuntime,
    mkAccountJwtConfiguration,
    unavailableAccountJwtIssuer,
  )
where

import Control.Exception (IOException, try)
import Control.Lens (matching, preview, (&), (.~), (?~), (^.))
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
import Data.Void (Void, absurd)
import Data.Word (Word64)
import HarchWeb qualified
import HarchWeb.Account qualified as Account
import HarchWeb.Session (OpaqueSession (..), SessionId, mkSessionId)
import HarchWeb.Session qualified as Session
import HarchWeb.Time (UnixTimeNanoseconds, unixTimeNanosecondsValue)
import Network.HTTP.Types qualified as Http
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
  show configuration =
    "AccountJwtConfiguration {accountJwtIssuerText = "
      <> show (validatedStringOrUriText (accountJwtIssuer configuration))
      <> ", accountJwtAudienceText = "
      <> show (validatedStringOrUriText (accountJwtAudience configuration))
      <> ", accountJwtActiveKeyId = "
      <> show (accountJwtActiveKeyId configuration)
      <> ", accountJwtSigningJwkFile = "
      <> show (accountJwtSigningJwkFile configuration)
      <> ", accountJwtVerificationJwkSetFile = "
      <> show (accountJwtVerificationJwkSetFile configuration)
      <> ", accountJwtCookiePolicy = "
      <> show (accountJwtCookiePolicy configuration)
      <> "}"

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
  deriving (Eq, Show)

data AccountJwtIssueError = AccountJwtIssueFailed
  deriving (Eq, Show)

data AccountJwtRuntime = AccountJwtRuntime
  { runtimeAccountJwtConfiguration :: AccountJwtConfiguration,
    runtimeAccountJwtSigningKey :: HarchWeb.JWK,
    runtimeAccountJwtVerificationKeys :: HarchWeb.JWKSet
  }

-- | Never show an in-memory signing key through a failed assertion or an
-- application startup exception.
instance Show AccountJwtRuntime where
  show _ = "AccountJwtRuntime <redacted>"

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
loadAccountJwtRuntime configuration = do
  signingKeyResult <- readJwk (accountJwtSigningJwkFile configuration)
  verificationKeysResult <- readJwkSet (accountJwtVerificationJwkSetFile configuration)
  -- Force the complete startup validation rail before constructing a runtime.
  -- Binding a successful @()@ result without this case would be lazy enough to
  -- defer key compatibility checks until an unrelated later use.
  pure $ do
    signingKey <- signingKeyResult
    verificationKeys <- verificationKeysResult
    case validateRuntimeKeys configuration signingKey verificationKeys of
      Left failure -> Left failure
      Right () ->
        Right
          AccountJwtRuntime
            { runtimeAccountJwtConfiguration = configuration,
              runtimeAccountJwtSigningKey = signingKey,
              runtimeAccountJwtVerificationKeys = verificationKeys
            }

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

validateRuntimeKeys :: AccountJwtConfiguration -> HarchWeb.JWK -> HarchWeb.JWKSet -> Either AccountJwtLoadError ()
validateRuntimeKeys configuration signingKey (JoseJwk.JWKSet verificationKeys) = do
  if signingKey ^. JoseJwk.jwkKid == Just (accountJwtActiveKeyId configuration)
    then Right ()
    else Left AccountJwtSigningKeyIdMismatch
  if rsaPrivateJwk signingKey
    then Right ()
    else Left AccountJwtSigningKeyNotRsaPrivate
  case find ((== Just (accountJwtActiveKeyId configuration)) . (^. JoseJwk.jwkKid)) verificationKeys of
    Nothing -> Left AccountJwtVerificationKeyMissing
    Just verificationKey ->
      if rsaJwk verificationKey
        then Right ()
        else Left AccountJwtVerificationKeyNotRsa

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
issueJwtForSession runtime session = do
  result <- HarchWeb.issueJwt signingKey header (claimsForSession configuration session)
  pure $
    case result of
      Left _ -> Left AccountJwtIssueFailed
      Right token -> Right token
  where
    configuration = runtimeAccountJwtConfiguration runtime
    signingKey = runtimeAccountJwtSigningKey runtime
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
accountIdStringOrUri accountId =
  case matching Jwt.stringOrUri (Text.unpack (Account.accountIdText accountId)) of
    Right parsedValue -> parsedValue
    Left _ -> error "validated account identifier cannot be represented as a JWT StringOrURI"

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
accountJwtAuthenticationPipeline :: AccountSessionStore -> IO UnixTimeNanoseconds -> AccountJwtRuntime -> HarchWeb.AuthenticationPipeline AppRoute AppRequestContext () HarchWeb.EncodedJwt AccountJwtClaims AccountPrincipal Void
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
      HarchWeb.authenticationAuthorizationInterpreter = HarchWeb.AuthorizationInterpreter (\_ () -> HarchWeb.Authorized),
      HarchWeb.authenticationDenialFailureCode = absurd,
      HarchWeb.authenticationAttachPrincipal = \principal context -> context {requestAccountPrincipal = Just principal},
      HarchWeb.authenticationChallenge = accountAuthenticationChallenge,
      HarchWeb.authenticationForbidden = \_ denial -> absurd denial,
      HarchWeb.authenticationUnavailable = \_ _ -> authenticationErrorResponse Http.status503 "Authentication is temporarily unavailable."
    }
  where
    configuration = runtimeAccountJwtConfiguration runtime
    cookiePolicy = accountJwtCookiePolicy configuration
    validationSettings =
      Jwt.defaultJWTValidationSettings (== validatedStringOrUriValue (accountJwtAudience configuration))
        & Jwt.jwtValidationSettingsIssuerPredicate .~ (== validatedStringOrUriValue (accountJwtIssuer configuration))
        & Jwt.jwtValidationSettingsAudiencePredicate .~ (== validatedStringOrUriValue (accountJwtAudience configuration))

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
  case HarchWeb.mkSecurityFailureCode "account.jwt.claims-rejected" of
    Right failureCode -> HarchWeb.mkJwtClaimsError failureCode
    Left _ -> error "account JWT claim failure code is invalid"

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
knownAccountSessionRejected =
  case HarchWeb.mkSecurityFailureCode "account.jwt.session-rejected" of
    Right failureCode -> failureCode
    Left _ -> error "account JWT session rejection failure code is invalid"

knownAccountSessionUnavailable :: HarchWeb.SecurityFailureCode
knownAccountSessionUnavailable =
  case HarchWeb.mkSecurityFailureCode "account.jwt.session-unavailable" of
    Right failureCode -> failureCode
    Left _ -> error "account JWT session unavailable failure code is invalid"

authenticationProofMaximumBytes :: HarchWeb.AuthenticationProofMaximumBytes
authenticationProofMaximumBytes =
  case HarchWeb.mkAuthenticationProofMaximumBytes 8192 of
    Right maximumBytes -> maximumBytes
    Left _ -> error "account JWT proof maximum is invalid"

unavailableAccountJwtIssuer :: AccountJwtIssuer
unavailableAccountJwtIssuer =
  AccountJwtIssuer
    { accountJwtCookie = unavailableCookiePolicy,
      issueAccountSessionJwt = \_ -> pure (Left AccountJwtIssueFailed)
    }
  where
    unavailableCookiePolicy =
      case HarchWeb.mkAuthenticationCookiePolicy "__Host-harch-session" 28800 of
        Right cookiePolicy -> cookiePolicy
        Left _ -> error "unavailable account JWT cookie policy is invalid"
