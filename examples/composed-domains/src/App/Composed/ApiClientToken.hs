{-# LANGUAGE OverloadedStrings #-}

-- | OAuth client-credentials verification and API-audience token issuance for
-- the composed example.
--
-- Decision record (OpenAPI documentation and Swagger UI, 2026-09-27): use Harch's existing
-- 'HarchWeb.ApiClientStore' capability and bounded Argon work gate, but keep
-- composed client identity and scope policy application-owned. An unknown
-- client performs a 64 MiB Argon2id dummy verification before returning the
-- shared invalid-client outcome. The client model has one active hash, so the
-- seeded client's hash must use this same 64 MiB cost to preserve the nominal
-- work shape for one verification. Secret rotation replaces the hash
-- atomically; overlapping hashes need a new work-parity decision.
-- Secret verification precedes requested-scope interpretation, and an
-- admitted client can issue only an API-audience token through
-- 'App.Composed.Auth.issueComposedApiToken'. The API workflow is separate
-- from account-web sessions although both use the one startup-validated
-- RS256 key set. This module remains protocol-neutral: the optional
-- 'App.Composed.OAuth' adapter maps its ordinary outcomes to bounded
-- no-store responses, and 'App.Composed.Postgres.ApiClientStore' supplies the
-- durable client snapshot. AHI-4E-OAUTH's route, metadata, storage, and
-- privacy proofs live at those outer boundaries; repository gates and
-- exact-commit PR CI remain required before that task is recorded complete.
module App.Composed.ApiClientToken
  ( ComposedApiClientTokenEnvironment (..),
    ComposedApiClientTokenOutcome (..),
    composedApiClientTokenLifetimeSeconds,
    issueComposedApiClientToken,
  )
where

import App.Composed.ApiClient
  ( ComposedApiClient,
    ComposedApiClientId,
    EstablishedComposedApiClient,
    composedApiClientId,
    composedApiClientIdText,
    composedApiClientSecretHash,
    mkComposedApiClientId,
    selectComposedApiClientScopes,
  )
import App.Composed.Auth (ComposedJwtRuntime, issueComposedApiToken)
import Control.Exception (evaluate)
import Control.Monad.Except (ExceptT (..), liftEither, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Core.Control.Error (liftEitherWith)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Word (Word64)
import HarchWeb.Authentication
  ( ApiClientStore (..),
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
  ( PasswordHash (..),
    PasswordWorkGate,
    passwordHashWorkKibibytes,
    verifyPassword,
    withPasswordWork,
  )
import HarchWeb.Time (UnixTimeNanoseconds, addUnixTimeNanoseconds)

data ComposedApiClientTokenEnvironment = ComposedApiClientTokenEnvironment
  { composedApiClientTokenStore :: ApiClientStore ComposedApiClientId ComposedApiClient EstablishedComposedApiClient,
    composedApiClientTokenWorkGate :: PasswordWorkGate,
    composedApiClientTokenJwtRuntime :: ComposedJwtRuntime,
    composedApiClientTokenClock :: IO UnixTimeNanoseconds
  }

-- | An issued compact JWT is always redacted by 'Show'. Every rejection and
-- dependency failure remains an ordinary, stable workflow outcome for the
-- protocol adapter to interpret once.
data ComposedApiClientTokenOutcome
  = ComposedApiClientTokenIssued EncodedJwt [OAuth2Scope] Word64
  | ComposedApiClientTokenInvalidClient
  | ComposedApiClientTokenInvalidScope
  | ComposedApiClientTokenStoreUnavailable
  | ComposedApiClientTokenWorkBudgetExhausted
  | ComposedApiClientTokenIssueFailed

instance Eq ComposedApiClientTokenOutcome where
  ComposedApiClientTokenIssued _ leftScopes leftLifetime == ComposedApiClientTokenIssued _ rightScopes rightLifetime =
    (oauth2ScopeText <$> leftScopes) == (oauth2ScopeText <$> rightScopes) && leftLifetime == rightLifetime
  ComposedApiClientTokenInvalidClient == ComposedApiClientTokenInvalidClient = True
  ComposedApiClientTokenInvalidScope == ComposedApiClientTokenInvalidScope = True
  ComposedApiClientTokenStoreUnavailable == ComposedApiClientTokenStoreUnavailable = True
  ComposedApiClientTokenWorkBudgetExhausted == ComposedApiClientTokenWorkBudgetExhausted = True
  ComposedApiClientTokenIssueFailed == ComposedApiClientTokenIssueFailed = True
  _ == _ = False

instance Show ComposedApiClientTokenOutcome where
  showsPrec depth outcome =
    showParen (depth > 10) $ case outcome of
      ComposedApiClientTokenIssued _ scopes lifetime ->
        showString "ComposedApiClientTokenIssued <redacted> "
          . shows (oauth2ScopeText <$> scopes)
          . showString " "
          . shows lifetime
      ComposedApiClientTokenInvalidClient -> showString "ComposedApiClientTokenInvalidClient"
      ComposedApiClientTokenInvalidScope -> showString "ComposedApiClientTokenInvalidScope"
      ComposedApiClientTokenStoreUnavailable -> showString "ComposedApiClientTokenStoreUnavailable"
      ComposedApiClientTokenWorkBudgetExhausted -> showString "ComposedApiClientTokenWorkBudgetExhausted"
      ComposedApiClientTokenIssueFailed -> showString "ComposedApiClientTokenIssueFailed"

composedApiClientTokenLifetimeSeconds :: Word64
composedApiClientTokenLifetimeSeconds = 900

issueComposedApiClientToken :: ComposedApiClientTokenEnvironment -> OAuth2ClientCredentials -> OAuth2ScopeRequest -> IO ComposedApiClientTokenOutcome
issueComposedApiClientToken environment credentials scopeRequest = do
  result <- runExceptT $ do
    client <- resolveClient environment credentials
    verifiedClient <- verifyClientSecret environment credentials client
    selectedScopes <-
      liftEither $ case selectComposedApiClientScopes verifiedClient (scopesFromRequest scopeRequest) of
        Left _ -> Left ComposedApiClientTokenInvalidScope
        Right selected -> Right selected
    scopes <-
      case NonEmpty.nonEmpty selectedScopes of
        Nothing -> throwError ComposedApiClientTokenInvalidScope
        Just nonEmptyScopes -> pure nonEmptyScopes
    issueToken environment verifiedClient scopes
  pure $ case result of
    Left failure -> failure
    Right (encoded, scopes) -> ComposedApiClientTokenIssued encoded (NonEmpty.toList scopes) composedApiClientTokenLifetimeSeconds

resolveClient :: ComposedApiClientTokenEnvironment -> OAuth2ClientCredentials -> ExceptT ComposedApiClientTokenOutcome IO ComposedApiClient
resolveClient environment credentials =
  case mkComposedApiClientId (oauth2ClientIdText (oauth2ClientCredentialsId credentials)) of
    Left _ -> unknownClient
    Right clientId -> do
      found <- liftEitherWith (const ComposedApiClientTokenStoreUnavailable) (findApiClient (composedApiClientTokenStore environment) clientId)
      maybe unknownClient pure found
  where
    unknownClient = do
      _ <- verifySecret environment credentials dummyComposedApiClientSecretHash
      throwError ComposedApiClientTokenInvalidClient

verifyClientSecret :: ComposedApiClientTokenEnvironment -> OAuth2ClientCredentials -> ComposedApiClient -> ExceptT ComposedApiClientTokenOutcome IO ComposedApiClient
verifyClientSecret environment credentials client = do
  verified <- verifySecret environment credentials (composedApiClientSecretHash client)
  if verified
    then pure client
    else throwError ComposedApiClientTokenInvalidClient

verifySecret :: ComposedApiClientTokenEnvironment -> OAuth2ClientCredentials -> PasswordHash -> ExceptT ComposedApiClientTokenOutcome IO Bool
verifySecret environment credentials secretHash =
  case passwordHashWorkKibibytes secretHash of
    Nothing -> do
      _ <- verifyWithCost dummyComposedApiClientSecretHash 65536
      pure False
    Just cost -> verifyWithCost secretHash cost
  where
    verifyWithCost hash cost =
      ExceptT $ do
        result <- withPasswordWork (composedApiClientTokenWorkGate environment) cost (evaluate (verifyPassword (oauth2ClientCredentialsSecret credentials) hash))
        pure $ case result of
          Nothing -> Left ComposedApiClientTokenWorkBudgetExhausted
          Just verified -> Right verified

scopesFromRequest :: OAuth2ScopeRequest -> [OAuth2Scope]
scopesFromRequest scopeRequest =
  case scopeRequest of
    UseClientDefaultScopes -> []
    RequestOAuth2Scopes scopes -> NonEmpty.toList scopes

issueToken :: ComposedApiClientTokenEnvironment -> ComposedApiClient -> NonEmpty OAuth2Scope -> ExceptT ComposedApiClientTokenOutcome IO (EncodedJwt, NonEmpty OAuth2Scope)
issueToken environment client scopes = do
  now <- liftIO (composedApiClientTokenClock environment)
  expiresAt <-
    liftEither $ case addUnixTimeNanoseconds now (composedApiClientTokenLifetimeSeconds * 1000000000) of
      Nothing -> Left ComposedApiClientTokenIssueFailed
      Just expiry -> Right expiry
  issued <-
    liftEitherWith
      (const ComposedApiClientTokenIssueFailed)
      (issueComposedApiToken (composedApiClientTokenJwtRuntime environment) (composedApiClientIdText (composedApiClientId client)) scopes now expiresAt)
  pure (issued, scopes)

dummyComposedApiClientSecretHash :: PasswordHash
dummyComposedApiClientSecretHash = PasswordHash "$argon2id$v=19$m=65536,t=3,p=1$MDAwMDAwMDAwMDAwMDAwMA$nTQzDQsyrnF98d3p5wV9nHhxGtnnTCDElTqAkW2qVkk"
