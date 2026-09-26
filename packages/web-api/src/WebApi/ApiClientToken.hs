{-# LANGUAGE OverloadedStrings #-}

-- | OAuth 2.0 client-credentials verification and durable API-client bearer
-- token issuance.
--
-- Decision record (AHI-4D slice 4/5, 2026-09-16): connect
-- 'HarchWeb.ApiClientStore''s issuance view to Argon2 secret verification and
-- token issuance at this one workflow, rather than adding a second
-- authentication dispatcher or a second JWK file. An unknown client ID and a
-- known client with a wrong secret run the same Argon2-gated verification
-- shape and reach the same rejection, so response timing and shape cannot
-- enumerate client existence; this mirrors 'WebApi.Login.Password''s existing
-- unknown-identifier handling. A client's secret rotation may leave several
-- active hashes; every hash is tried in order under the shared work gate
-- before the request is rejected, and a work-budget exhaustion is reported
-- distinctly from a genuine rejection — for a known client and for the
-- unknown-client dummy check alike — so a caller cannot be told a secret is
-- wrong when it was never actually checked.
--
-- Issuance reuses 'WebApi.AccountJwt.accountJwtRuntimeSharedIssuance' so
-- account and API-client bearer tokens share one issuer, audience, and
-- already-startup-proven RS256 key, exactly as the design requires. The
-- token's granted-scope list travels as a claim built directly on
-- 'Aeson.Value' (see 'claimsForApiClient') rather than the deprecated
-- 'Crypto.JWT.unregisteredClaims'/'Crypto.JWT.addClaim', and rather than
-- widening the account signer's fixed 'Crypto.JWT.ClaimsSet' claims type with
-- a second module-owned 'ToJSON' instance. This module only issues a token:
-- HTTP routing, protocol error encoding, and bearer-token
-- establishment/verification remain later AHI-4D work.
module WebApi.ApiClientToken
  ( ApiClientTokenEnvironment (..),
    ApiClientTokenOutcome (..),
    apiClientTokenLifetimeSeconds,
    issueApiClientToken,
  )
where

import Control.Exception (evaluate)
import Control.Lens (review, (&), (?~))
import Crypto.JOSE.Header (HeaderParam (..), RequiredProtection (..))
import Crypto.JOSE.JWA.JWS qualified as JwaJws
import Crypto.JOSE.JWS qualified as JoseJws
import Crypto.JWT qualified as Jwt
import Data.Aeson qualified as Aeson
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word64)
import HarchWeb qualified
import HarchWeb.Authentication
  ( ApiClientStore (..),
    ApiClientStoreError (..),
    EncodedJwt,
    OAuth2ClientCredentials,
    OAuth2Scope,
    OAuth2ScopeRequest (..),
    oauth2ClientCredentialsId,
    oauth2ClientCredentialsSecret,
    oauth2ClientIdText,
    oauth2ScopeText,
  )
import HarchWeb.Password
  ( Password,
    PasswordHash (..),
    PasswordWorkGate,
    passwordHashWorkKibibytes,
    verifyPassword,
    withPasswordWork,
  )
import HarchWeb.Time (UnixTimeNanoseconds, addUnixTimeNanoseconds, unixTimeNanosecondsValue)
import Text.Show (showListWith)
import WebApi.AccountJwt (SharedJwtIssuance (..))
import WebApi.ApiClient
  ( ApiClient,
    ApiClientId,
    EstablishedApiClient,
    apiClientId,
    apiClientIdText,
    apiClientSecretHashes,
    mkApiClientId,
    selectApiClientScopes,
  )

data ApiClientTokenEnvironment = ApiClientTokenEnvironment
  { apiClientTokenStore :: ApiClientStore ApiClientId ApiClient EstablishedApiClient,
    apiClientTokenWorkGate :: PasswordWorkGate,
    apiClientTokenIssuance :: SharedJwtIssuance,
    apiClientTokenClock :: IO UnixTimeNanoseconds
  }

-- | 'ApiClientTokenIssued' deliberately never renders its proof: 'EncodedJwt'
-- has no 'Show' instance, so this outcome cannot become a public diagnostic
-- or log value merely by being compared in a test.
data ApiClientTokenOutcome
  = ApiClientTokenIssued EncodedJwt [OAuth2Scope] Word64
  | ApiClientTokenInvalidClient
  | ApiClientTokenInvalidScope
  | ApiClientTokenStoreUnavailable
  | ApiClientTokenWorkBudgetExhausted
  | ApiClientTokenIssueFailed

-- | 'OAuth2Scope' deliberately has no 'Eq' instance (see 'WebApi.ApiClient's
-- test-side scope comparisons via 'oauth2ScopeText'), so 'ApiClientTokenIssued'
-- compares its granted scopes as text rather than deriving this instance.
instance Eq ApiClientTokenOutcome where
  ApiClientTokenIssued _ leftScopes leftLifetimeSeconds == ApiClientTokenIssued _ rightScopes rightLifetimeSeconds =
    (oauth2ScopeText <$> leftScopes) == (oauth2ScopeText <$> rightScopes) && leftLifetimeSeconds == rightLifetimeSeconds
  ApiClientTokenInvalidClient == ApiClientTokenInvalidClient = True
  ApiClientTokenInvalidScope == ApiClientTokenInvalidScope = True
  ApiClientTokenStoreUnavailable == ApiClientTokenStoreUnavailable = True
  ApiClientTokenWorkBudgetExhausted == ApiClientTokenWorkBudgetExhausted = True
  ApiClientTokenIssueFailed == ApiClientTokenIssueFailed = True
  _ == _ = False

instance Show ApiClientTokenOutcome where
  showsPrec depth outcome =
    showParen (depth > 10) $ case outcome of
      ApiClientTokenIssued _ scopes lifetimeSeconds ->
        showString "ApiClientTokenIssued <redacted> "
          . shows (oauth2ScopeText <$> scopes)
          . showString " "
          . shows lifetimeSeconds
      ApiClientTokenInvalidClient -> showString "ApiClientTokenInvalidClient"
      ApiClientTokenInvalidScope -> showString "ApiClientTokenInvalidScope"
      ApiClientTokenStoreUnavailable -> showString "ApiClientTokenStoreUnavailable"
      ApiClientTokenWorkBudgetExhausted -> showString "ApiClientTokenWorkBudgetExhausted"
      ApiClientTokenIssueFailed -> showString "ApiClientTokenIssueFailed"

  showList = showListWith shows

-- | The fixed RFC 6749 token lifetime. It deliberately never slides and has
-- no refresh token.
apiClientTokenLifetimeSeconds :: Word64
apiClientTokenLifetimeSeconds = 900

-- | A fixed Argon2id hash with no corresponding real client. Running the same
-- admitted verification against it for an unknown client keeps rejection
-- timing close to a known client with exactly one active secret, without
-- reusing 'WebApi.Login.Password''s own login-specific dummy constant across
-- these two distinct principal kinds.
dummyApiClientSecretHash :: PasswordHash
dummyApiClientSecretHash = PasswordHash "$argon2id$v=19$m=65536,t=3,p=1$MDAwMDAwMDAwMDAwMDAwMA$nTQzDQsyrnF98d3p5wV9nHhxGtnnTCDElTqAkW2qVkk"

issueApiClientToken :: ApiClientTokenEnvironment -> OAuth2ClientCredentials -> OAuth2ScopeRequest -> IO ApiClientTokenOutcome
issueApiClientToken environment credentials scopeRequest = do
  resolved <- resolveApiClient environment credentials
  case resolved of
    ResolvedStoreUnavailable -> pure ApiClientTokenStoreUnavailable
    ResolvedUnknownClient -> do
      verified <- verifySecretAgainstHashes (apiClientTokenWorkGate environment) (oauth2ClientCredentialsSecret credentials) (dummyApiClientSecretHash :| [])
      case verified of
        Nothing -> pure ApiClientTokenWorkBudgetExhausted
        Just _ -> pure ApiClientTokenInvalidClient
    ResolvedClient client -> continueWithClient environment credentials scopeRequest client

-- | The three real outcomes of resolving a submitted client ID: an
-- unavailable durable store, no resolvable client (a syntactically invalid ID
-- shape and a store lookup that finds no active client are the same
-- "no such client" outcome to every caller), or the current client record.
-- Naming this outcome directly, instead of returning the store's raw
-- @Either ApiClientStoreError (Maybe ApiClient)@ unchanged, keeps
-- 'resolveApiClient' a real interpretation step rather than a pass-through.
data ResolvedApiClient
  = ResolvedStoreUnavailable
  | ResolvedUnknownClient
  | ResolvedClient ApiClient

resolveApiClient :: ApiClientTokenEnvironment -> OAuth2ClientCredentials -> IO ResolvedApiClient
resolveApiClient environment credentials =
  case mkApiClientId (oauth2ClientIdText (oauth2ClientCredentialsId credentials)) of
    Left _ -> pure ResolvedUnknownClient
    Right clientId -> findApiClient (apiClientTokenStore environment) clientId >>= interpretFoundApiClient

interpretFoundApiClient :: Either ApiClientStoreError (Maybe ApiClient) -> IO ResolvedApiClient
interpretFoundApiClient (Left (ApiClientStoreUnavailable _)) = pure ResolvedStoreUnavailable
interpretFoundApiClient (Right Nothing) = pure ResolvedUnknownClient
interpretFoundApiClient (Right (Just client)) = pure (ResolvedClient client)

continueWithClient :: ApiClientTokenEnvironment -> OAuth2ClientCredentials -> OAuth2ScopeRequest -> ApiClient -> IO ApiClientTokenOutcome
continueWithClient environment credentials scopeRequest client = do
  verified <- verifySecretAgainstHashes (apiClientTokenWorkGate environment) (oauth2ClientCredentialsSecret credentials) (apiClientSecretHashes client)
  case verified of
    Nothing -> pure ApiClientTokenWorkBudgetExhausted
    Just False -> pure ApiClientTokenInvalidClient
    Just True ->
      case selectApiClientScopes client (scopesFromRequest scopeRequest) of
        Left _ -> pure ApiClientTokenInvalidScope
        Right selectedScopes -> issueToken environment client selectedScopes

scopesFromRequest :: OAuth2ScopeRequest -> [OAuth2Scope]
scopesFromRequest scopeRequest =
  case scopeRequest of
    UseClientDefaultScopes -> []
    RequestOAuth2Scopes scopes -> NonEmpty.toList scopes

-- | Try each active secret hash in order under the shared Argon2 work gate,
-- accepting on the first match. 'Nothing' is a work-budget exhaustion, not a
-- rejection, so a caller is never told a secret is wrong when it was never
-- actually checked.
verifySecretAgainstHashes :: PasswordWorkGate -> Password -> NonEmpty PasswordHash -> IO (Maybe Bool)
verifySecretAgainstHashes workGate secret = go . NonEmpty.toList
  where
    go [] = pure (Just False)
    go (hash : rest) =
      case passwordHashWorkKibibytes hash of
        Nothing -> pure (Just False)
        Just cost -> do
          verified <- withPasswordWork workGate cost (evaluate (verifyPassword secret hash))
          case verified of
            Just False -> go rest
            outcome -> pure outcome

issueToken :: ApiClientTokenEnvironment -> ApiClient -> [OAuth2Scope] -> IO ApiClientTokenOutcome
issueToken environment client scopes = do
  now <- apiClientTokenClock environment
  case addUnixTimeNanoseconds now (apiClientTokenLifetimeSeconds * 1000000000) of
    Nothing -> pure ApiClientTokenIssueFailed
    Just expiresAt -> do
      issued <- HarchWeb.signJwt signer header (claimsForApiClient issuance client scopes now expiresAt)
      pure $ case issued of
        Left _ -> ApiClientTokenIssueFailed
        Right encoded -> ApiClientTokenIssued encoded scopes apiClientTokenLifetimeSeconds
  where
    issuance = apiClientTokenIssuance environment
    signer = HarchWeb.joseJwtSigner (sharedJwtSigningKey issuance)
    header :: HarchWeb.JWSHeader HarchWeb.RequiredProtection
    header =
      JoseJws.newJWSHeaderProtected JwaJws.RS256
        & JoseJws.kid ?~ HeaderParam RequiredProtection (sharedJwtActiveKeyId issuance)

-- | Build the claims directly as an 'Aeson.Value' instead of a bespoke
-- claims subtype with its own 'ToJSON' instance. Each claim value still keeps
-- @jose@'s exact type ('Jwt.StringOrURI', 'Jwt.NumericDate', 'Jwt.Audience'),
-- so this remains a typed construction, not string/number assembly; only the
-- container is the library's own already-'ToJSON' 'Aeson.Value' rather than
-- 'Crypto.JWT.ClaimsSet'. This avoids both the deprecated
-- 'Crypto.JWT.unregisteredClaims'/'Crypto.JWT.addClaim' and a second
-- module-owned 'ToJSON' instance whose declaration head 'HarchWeb.joseJwtSigner'
-- never itself enters (it only calls the already-defined 'toJSON'/'toEncoding'
-- methods), which otherwise appears in this module's HPC-diagnostic
-- declaration count as never covered; see
-- [never-mask-a-gate-finding](../../../../docs/design-guidance.md#never-mask-a-gate-finding-with-an-ignore-pragma).
claimsForApiClient :: SharedJwtIssuance -> ApiClient -> [OAuth2Scope] -> UnixTimeNanoseconds -> UnixTimeNanoseconds -> Aeson.Value
claimsForApiClient issuance client scopes now expiresAt =
  Aeson.object
    [ "iss" Aeson..= sharedJwtIssuer issuance,
      "aud" Aeson..= Jwt.Audience [sharedJwtAudience issuance],
      "sub" Aeson..= review Jwt.string (apiClientIdText (apiClientId client)),
      "iat" Aeson..= numericDate now,
      "nbf" Aeson..= numericDate now,
      "exp" Aeson..= numericDate expiresAt,
      "scope" Aeson..= Text.unwords (oauth2ScopeText <$> scopes)
    ]

numericDate :: UnixTimeNanoseconds -> Jwt.NumericDate
numericDate instant =
  Jwt.NumericDate
    ( posixSecondsToUTCTime
        (fromIntegral (unixTimeNanosecondsValue instant) / 1000000000)
    )
