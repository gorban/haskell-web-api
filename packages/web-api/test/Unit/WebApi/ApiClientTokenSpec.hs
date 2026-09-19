{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Lens ((&), (?~))
import Crypto.JOSE.JWA.JWK qualified as JwaJwk
import Crypto.JOSE.JWK qualified as JoseJwk
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Base64.URL qualified as Base64Url
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word32)
import HarchWeb.Api (ApiRequestData (..), ApiRequestDecodeResult (..), apiHeaderName, runRequestCodec)
import HarchWeb.Authentication (ApiClientStore (..), ApiClientStoreError (..), EncodedJwt, OAuth2ClientCredentials, OAuth2Scope, OAuth2ScopeRequest, encodedJwtBytes, mkAuthenticationDependency, mkOAuth2Scope, oauth2ClientCredentialsRequestCodec, oauth2ClientCredentialsScopes, oauth2ClientSecretBasicCodec, requiredOAuth2ClientCredentialsMaximumBytesOrDie, requiredSecurityFailureCodeOrDie)
import HarchWeb.Password (PasswordHash (..), PasswordHashingPolicy, argon2Iterations, argon2MemoryKib, argon2Parallelism, hashPasswordWithSalt, mkPassword, mkPasswordHashingPolicy, mkPasswordWorkBudget, newPasswordWorkGate)
import HarchWeb.Time (unixTimeNanoseconds)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import WebApi.AccountJwt (AccountJwtRawConfiguration (..), SharedJwtIssuance (..), accountJwtRuntimeSharedIssuance, loadAccountJwtRuntime, mkAccountJwtConfiguration)
import WebApi.ApiClient (ApiClient, ApiClientId, EstablishedApiClient, apiClientIdText, mkApiClient, mkApiClientId)
import WebApi.ApiClientToken

spec =
  describe "WebApi.ApiClientToken" $ do
    it "issues a bearer token carrying the client's default scopes" $
      withTestEnvironment ampleWorkBudget $ \environment -> do
        outcome <- issueApiClientToken environment (credentialsFor "automation-client" "current-secret") (scopeRequestFor [])
        payload <- requiredPayload outcome
        expectAll
          ( (outcome `shouldSatisfy` isIssuedWithLifetime)
              :| [ KeyMap.lookup "sub" payload `shouldBe` Just (Aeson.String "automation-client"),
                   KeyMap.lookup "scope" payload `shouldBe` Just (Aeson.String "resource:read"),
                   show outcome `shouldBe` "ApiClientTokenIssued <redacted> [\"resource:read\"] 900"
                 ]
          )

    it "renders every non-issued outcome fully and redacts an issued proof" $ do
      expectAll
        ( (show ApiClientTokenInvalidClient `shouldBe` "ApiClientTokenInvalidClient")
            :| [ show ApiClientTokenInvalidScope `shouldBe` "ApiClientTokenInvalidScope",
                 show ApiClientTokenStoreUnavailable `shouldBe` "ApiClientTokenStoreUnavailable",
                 show ApiClientTokenWorkBudgetExhausted `shouldBe` "ApiClientTokenWorkBudgetExhausted",
                 show ApiClientTokenIssueFailed `shouldBe` "ApiClientTokenIssueFailed",
                 show [ApiClientTokenInvalidClient, ApiClientTokenInvalidScope] `shouldBe` "[ApiClientTokenInvalidClient,ApiClientTokenInvalidScope]"
               ]
        )

    it "issues a bearer token restricted to an explicitly requested scope subset" $
      withTestEnvironment ampleWorkBudget $ \environment -> do
        outcome <- issueApiClientToken environment (credentialsFor "automation-client" "current-secret") (scopeRequestFor ["profile:read:self"])
        payload <- requiredPayload outcome
        KeyMap.lookup "scope" payload `shouldBe` Just (Aeson.String "profile:read:self")

    it "accepts a still-active rotated secret without accepting an unrelated wrong secret" $
      withTestEnvironment ampleWorkBudget $ \environment -> do
        rotated <- issueApiClientToken environment (credentialsFor "automation-client" "old-secret") (scopeRequestFor [])
        wrong <- issueApiClientToken environment (credentialsFor "automation-client" "not-the-secret") (scopeRequestFor [])
        expectAll
          ( (rotated `shouldSatisfy` isIssuedWithLifetime)
              :| [wrong `shouldBe` ApiClientTokenInvalidClient]
          )

    it "rejects an unknown client the same way as a known client with a wrong secret" $
      withTestEnvironment ampleWorkBudget $ \environment -> do
        outcome <-
          issueApiClientToken
            environment {apiClientTokenStore = storeReturning "no-such-client" (Right Nothing)}
            (credentialsFor "no-such-client" "whatever")
            (scopeRequestFor [])
        outcome `shouldBe` ApiClientTokenInvalidClient

    it "rejects a syntactically invalid client ID before ever querying the store" $
      withTestEnvironment ampleWorkBudget $ \environment -> do
        outcome <-
          issueApiClientToken
            environment {apiClientTokenStore = explodingStore}
            (credentialsFor "bad client" "whatever")
            (scopeRequestFor [])
        outcome `shouldBe` ApiClientTokenInvalidClient

    it "reports store unavailability distinctly from an unknown client" $
      withTestEnvironment ampleWorkBudget $ \environment -> do
        outcome <-
          issueApiClientToken
            environment {apiClientTokenStore = storeReturning "automation-client" (Left storeUnavailable)}
            (credentialsFor "automation-client" "current-secret")
            (scopeRequestFor [])
        outcome `shouldBe` ApiClientTokenStoreUnavailable

    it "reports Argon2 work-budget exhaustion distinctly from a rejection" $
      withTestEnvironment tinyWorkBudget $ \environment -> do
        outcome <- issueApiClientToken environment (credentialsFor "automation-client" "current-secret") (scopeRequestFor [])
        outcome `shouldBe` ApiClientTokenWorkBudgetExhausted

    it "reports Argon2 work-budget exhaustion for an unknown client too, not just a rejection" $
      withTestEnvironment tinyWorkBudget $ \environment -> do
        outcome <-
          issueApiClientToken
            environment {apiClientTokenStore = storeReturning "no-such-client" (Right Nothing)}
            (credentialsFor "no-such-client" "whatever")
            (scopeRequestFor [])
        outcome `shouldBe` ApiClientTokenWorkBudgetExhausted

    it "rejects a requested scope outside the client's current allowance" $
      withTestEnvironment ampleWorkBudget $ \environment -> do
        outcome <- issueApiClientToken environment (credentialsFor "automation-client" "current-secret") (scopeRequestFor ["unallowed:scope"])
        outcome `shouldBe` ApiClientTokenInvalidScope

    it "rejects every secret for a client whose stored hash is malformed, even before a later valid hash" $
      withTestEnvironment ampleWorkBudget $ \environment -> do
        outcome <-
          issueApiClientToken
            environment {apiClientTokenStore = storeReturning "automation-client" (Right (Just clientWithMalformedFirstHash))}
            (credentialsFor "automation-client" "current-secret")
            (scopeRequestFor [])
        outcome `shouldBe` ApiClientTokenInvalidClient

    it "fails closed when the token would expire beyond the durable clock's representable range" $
      withTestEnvironment ampleWorkBudget $ \environment -> do
        outcome <-
          issueApiClientToken
            environment {apiClientTokenClock = pure (unixTimeNanoseconds maxBound)}
            (credentialsFor "automation-client" "current-secret")
            (scopeRequestFor [])
        outcome `shouldBe` ApiClientTokenIssueFailed

    it "fails closed when the shared signing key cannot produce the declared algorithm" $
      withTestEnvironment ampleWorkBudget $ \environment -> do
        symmetricKey <- JoseJwk.genJWK (JwaJwk.OctGenParam 32)
        let issuance = apiClientTokenIssuance environment
        outcome <-
          issueApiClientToken
            environment {apiClientTokenIssuance = issuance {sharedJwtSigningKey = symmetricKey}}
            (credentialsFor "automation-client" "current-secret")
            (scopeRequestFor [])
        outcome `shouldBe` ApiClientTokenIssueFailed

    it "keeps every distinct outcome unequal to every other, and equal to a matching issuance" $
      withTestEnvironment ampleWorkBudget $ \environment -> do
        first <- issueApiClientToken environment (credentialsFor "automation-client" "current-secret") (scopeRequestFor [])
        second <- issueApiClientToken environment (credentialsFor "automation-client" "old-secret") (scopeRequestFor [])
        expectAll
          ( (first `shouldBe` second)
              :| [ first `shouldNotBe` ApiClientTokenInvalidClient,
                   ApiClientTokenInvalidClient `shouldNotBe` ApiClientTokenInvalidScope,
                   ApiClientTokenInvalidScope `shouldNotBe` ApiClientTokenStoreUnavailable,
                   ApiClientTokenStoreUnavailable `shouldNotBe` ApiClientTokenWorkBudgetExhausted,
                   ApiClientTokenWorkBudgetExhausted `shouldNotBe` ApiClientTokenIssueFailed,
                   ApiClientTokenIssueFailed `shouldBe` ApiClientTokenIssueFailed
                 ]
          )

-- * Environment fixtures

-- | Large enough for both a cheap-policy test secret hash and the production
-- 'dummyApiClientSecretHash' constant's real 65536 KiB Argon2 memory cost.
ampleWorkBudget :: Word32
ampleWorkBudget = 65536

tinyWorkBudget :: Word32
tinyWorkBudget = 1

withTestEnvironment :: Word32 -> (ApiClientTokenEnvironment -> IO a) -> IO a
withTestEnvironment workBudgetKibibytes action =
  withSystemTempDirectory "web-api-api-client-token" $ \directory -> do
    signingKey <- JoseJwk.genJWK (JwaJwk.RSAGenParam 1024)
    let namedSigningKey = signingKey & JoseJwk.jwkKid ?~ "test-shared-key-v1"
        signingFile = directory </> "private.jwk"
        verificationFile = directory </> "verification.jwks"
        configuration =
          requiredEither
            "test shared JWT configuration"
            ( mkAccountJwtConfiguration
                AccountJwtRawConfiguration
                  { rawAccountJwtIssuer = "https://issuer.example.test",
                    rawAccountJwtAudience = "web-api",
                    rawAccountJwtActiveKeyId = "test-shared-key-v1",
                    rawAccountJwtSigningJwkFile = signingFile,
                    rawAccountJwtVerificationJwkSetFile = verificationFile,
                    rawAccountJwtCookieName = "__Host-harch-session",
                    rawAccountJwtCookieMaxAgeSeconds = 28800
                  }
            )
    ByteString.writeFile signingFile (LazyByteString.toStrict (Aeson.encode namedSigningKey))
    ByteString.writeFile verificationFile (LazyByteString.toStrict (Aeson.encode (JoseJwk.JWKSet [namedSigningKey])))
    loaded <- loadAccountJwtRuntime configuration
    runtime <- case loaded of
      Right value -> pure value
      Left loadError -> expectationFailure ("expected a valid test JWT runtime: " <> show loadError) >> error "unreachable"
    workGate <- newPasswordWorkGate (required "work budget" (mkPasswordWorkBudget workBudgetKibibytes))
    action
      ApiClientTokenEnvironment
        { apiClientTokenStore = defaultStore,
          apiClientTokenWorkGate = workGate,
          apiClientTokenIssuance = accountJwtRuntimeSharedIssuance runtime,
          apiClientTokenClock = pure (unixTimeNanoseconds 1000000000000)
        }

-- | A cheap Argon2id policy so real secret verification stays fast; the
-- work-budget exhaustion test relies on its 8 KiB memory cost exceeding
-- 'tinyWorkBudget'.
testHashingPolicy :: PasswordHashingPolicy
testHashingPolicy = required "test Argon2 policy" (mkPasswordHashingPolicy (argon2Iterations 1) (argon2MemoryKib 8) (argon2Parallelism 1))

testSecretHash :: ByteString.ByteString -> Text -> PasswordHash
testSecretHash salt secretValue = required "test secret hash" (hashPasswordWithSalt testHashingPolicy salt (mkPassword secretValue))

defaultClient :: ApiClient
defaultClient =
  case mkApiClient
    (requiredClientId "automation-client")
    (testSecretHash "old-secret-salt-000" "old-secret" :| [testSecretHash "current-secret-salt" "current-secret"])
    [requiredScope "resource:read", requiredScope "profile:read:self"]
    [requiredScope "resource:read"] of
    Right client -> client
    Left _ -> error "expected a valid test API client"

defaultStore :: ApiClientStore ApiClientId ApiClient EstablishedApiClient
defaultStore = storeReturning "automation-client" (Right (Just defaultClient))

-- | A stored hash with no parseable Argon2 parameters. It sits ahead of a
-- hash that genuinely matches "current-secret" to prove a malformed record
-- fails closed instead of falling through to a later valid one.
clientWithMalformedFirstHash :: ApiClient
clientWithMalformedFirstHash =
  case mkApiClient
    (requiredClientId "automation-client")
    (PasswordHash "not-a-parseable-argon2-hash" :| [testSecretHash "current-secret-salt" "current-secret"])
    [requiredScope "resource:read"]
    [requiredScope "resource:read"] of
    Right client -> client
    Left _ -> error "expected a valid test API client with a malformed first hash"

-- | Asserts the exact client ID text 'issueApiClientToken' passes through to
-- the store, rather than accepting any argument via 'const', so a regression
-- that queries the wrong client is caught by the test double itself.
storeReturning :: Text -> Either ApiClientStoreError (Maybe ApiClient) -> ApiClientStore ApiClientId ApiClient EstablishedApiClient
storeReturning expectedClientId findResult =
  ApiClientStore
    { findApiClient = \clientId ->
        if apiClientIdText clientId == expectedClientId
          then pure findResult
          else error "WebApi.ApiClientTokenSpec: findApiClient queried an unexpected client ID",
      establishApiClient = const (pure (Left storeUnavailable))
    }

-- | Proves the invalid-client-ID path in 'issueApiClientToken' never reaches
-- the store.
explodingStore :: ApiClientStore ApiClientId ApiClient EstablishedApiClient
explodingStore =
  ApiClientStore
    { findApiClient = const (error "must not query the store for a syntactically invalid client ID"),
      establishApiClient = const (error "unused by token issuance")
    }

storeUnavailable :: ApiClientStoreError
storeUnavailable = ApiClientStoreUnavailable (mkAuthenticationDependency (requiredSecurityFailureCodeOrDie "test.api-client.store-unavailable"))

-- * OAuth request-value fixtures

credentialsFor :: Text -> Text -> OAuth2ClientCredentials
credentialsFor clientIdValue secretValue =
  case runRequestCodec (oauth2ClientSecretBasicCodec maximumBytes) (ApiRequestData [] [(authorizationHeader, basicHeaderValue)] [] []) of
    ApiRequestDecoded credentials -> credentials
    _ -> error "expected valid OAuth client Basic credentials"
  where
    maximumBytes = requiredOAuth2ClientCredentialsMaximumBytesOrDie 256
    authorizationHeader = required "Authorization header name" (apiHeaderName "Authorization")
    basicHeaderValue = "Basic " <> TextEncoding.decodeUtf8 (Base64.encode (TextEncoding.encodeUtf8 (clientIdValue <> ":" <> secretValue)))

scopeRequestFor :: [Text] -> OAuth2ScopeRequest
scopeRequestFor scopes =
  case runRequestCodec oauth2ClientCredentialsRequestCodec (ApiRequestData [] [] [] formFields) of
    ApiRequestDecoded request -> oauth2ClientCredentialsScopes request
    _ -> error "expected a valid OAuth client-credentials token request"
  where
    formFields = ("grant_type", "client_credentials") : [("scope", Text.unwords scopes) | not (null scopes)]

requiredClientId :: Text -> ApiClientId
requiredClientId value =
  case mkApiClientId value of
    Right clientId -> clientId
    Left _ -> error "expected a valid API client ID"

requiredScope :: Text -> OAuth2Scope
requiredScope value =
  case mkOAuth2Scope value of
    Right scope -> scope
    Left scopeError -> error ("expected a valid OAuth scope: " <> show scopeError)

-- * Issued-token assertions

isIssuedWithLifetime :: ApiClientTokenOutcome -> Bool
isIssuedWithLifetime outcome =
  case outcome of
    ApiClientTokenIssued _ _ lifetimeSeconds -> lifetimeSeconds == apiClientTokenLifetimeSeconds
    _ -> False

requiredPayload :: ApiClientTokenOutcome -> IO Aeson.Object
requiredPayload outcome =
  case outcome of
    ApiClientTokenIssued token _ _ -> pure (decodedPayload token)
    _ -> expectationFailure ("expected an issued token, got: " <> show outcome) >> error "unreachable"

decodedPayload :: EncodedJwt -> Aeson.Object
decodedPayload token =
  case ByteString.split 46 (encodedJwtBytes token) of
    [_header, payload, _signature] ->
      case Base64Url.decode payload of
        Right decoded ->
          case Aeson.decodeStrict decoded of
            Just (Aeson.Object object) -> object
            _ -> error "expected a JSON object JWT payload"
        Left message -> error ("expected a valid base64url JWT payload: " <> message)
    _ -> error "expected a three-segment compact JWT"

-- * Generic fixtures

required :: String -> Maybe value -> value
required label = fromMaybe (error ("WebApi.ApiClientTokenSpec: " <> label))

requiredEither :: (Show error) => String -> Either error value -> value
requiredEither label result =
  case result of
    Right value -> value
    Left failure -> error ("WebApi.ApiClientTokenSpec: " <> label <> ": " <> show failure)
