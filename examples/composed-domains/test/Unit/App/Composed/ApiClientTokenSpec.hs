{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import App.Composed.ApiClient
import App.Composed.ApiClientToken
import App.Composed.Auth
import Control.Lens ((&), (.~), (?~), (^.))
import Crypto.JOSE.JWA.JWK qualified as JwaJwk
import Crypto.JOSE.JWK qualified as JoseJwk
import Crypto.JOSE.Types (Base64Integer (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Base64.URL qualified as Base64Url
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word32)
import HarchWeb (AuthenticationProofVerifier (AuthenticationProofVerifier), jwtProofFromCookie)
import HarchWeb.Api (ApiRequestData (..), ApiRequestDecodeResult (..), apiHeaderName, runRequestCodec)
import HarchWeb.Authentication (ApiClientStore (..), ApiClientStoreError (..), EncodedJwt, OAuth2ClientCredentials, OAuth2Scope, OAuth2ScopeRequest, encodedJwtBytes, mkAuthenticationDependency, mkOAuth2Scope, oauth2ClientCredentialsRequestCodec, oauth2ClientCredentialsScopes, oauth2ClientSecretBasicCodec, oauth2ScopeText, requiredOAuth2ClientCredentialsMaximumBytesOrDie, requiredSecurityFailureCodeOrDie)
import HarchWeb.Password (PasswordHash (..), PasswordHashingPolicy, argon2Iterations, argon2MemoryKib, argon2Parallelism, hashPasswordWithSalt, mkPassword, mkPasswordHashingPolicy, mkPasswordWorkBudget, newPasswordWorkGate)
import HarchWeb.Time (currentUnixTimeNanoseconds, unixTimeNanoseconds)

spec =
  describe "Unit.App.Composed.ApiClientToken" $ do
    it "issues a short-lived API-audience token with the client's default scopes" $
      withTestEnvironment ampleWorkBudget $ \environment -> do
        outcome <- issueComposedApiClientToken environment (credentialsFor "automation-client" "current-secret") (scopeRequestFor [])
        token <- requireIssuedToken outcome
        let AuthenticationProofVerifier verifyApi = composedApiProofVerifier (composedApiClientTokenJwtRuntime environment)
        verified <- verifyApi (jwtProofFromCookie token)
        payload <- requiredPayload token
        expectAll
          ( (verified `shouldBe` Right (ComposedApiClaims "automation-client" ["catalog:read", "orders:write"]))
              :| [ KeyMap.lookup "sub" payload `shouldBe` Just (Aeson.String "automation-client"),
                   KeyMap.lookup "scope" payload `shouldBe` Just (Aeson.String "catalog:read orders:write"),
                   payload `shouldSatisfy` KeyMap.member "iat",
                   payload `shouldSatisfy` KeyMap.member "nbf",
                   payload `shouldSatisfy` KeyMap.member "exp",
                   outcome `shouldSatisfy` isIssuedWithLifetime,
                   show outcome `shouldBe` "ComposedApiClientTokenIssued <redacted> [\"catalog:read\",\"orders:write\"] 900"
                 ]
          )

    it "issues only an explicitly requested allowed scope subset" $
      withTestEnvironment ampleWorkBudget $ \environment -> do
        outcome <- issueComposedApiClientToken environment (credentialsFor "automation-client" "current-secret") (scopeRequestFor ["orders:write"])
        token <- requireIssuedToken outcome
        let AuthenticationProofVerifier verifyApi = composedApiProofVerifier (composedApiClientTokenJwtRuntime environment)
        verifyApi (jwtProofFromCookie token) `shouldReturn` Right (ComposedApiClaims "automation-client" ["orders:write"])

    it "rejects an unknown client and a wrong secret with the same public outcome" $
      withTestEnvironment ampleWorkBudget $ \environment -> do
        unknown <-
          issueComposedApiClientToken
            environment {composedApiClientTokenStore = storeReturning "unknown-client" (Right Nothing)}
            (credentialsFor "unknown-client" "wrong-secret")
            (scopeRequestFor [])
        wrong <- issueComposedApiClientToken environment (credentialsFor "automation-client" "wrong-secret") (scopeRequestFor [])
        expectAll
          ( (unknown `shouldBe` ComposedApiClientTokenInvalidClient)
              :| [wrong `shouldBe` ComposedApiClientTokenInvalidClient]
          )

    it "runs the dummy secret work and skips storage for a syntactically invalid client ID" $
      withTestEnvironment ampleWorkBudget $ \environment -> do
        outcome <-
          issueComposedApiClientToken
            environment {composedApiClientTokenStore = explodingStore}
            (credentialsFor "invalid client" "wrong-secret")
            (scopeRequestFor [])
        outcome `shouldBe` ComposedApiClientTokenInvalidClient

    it "uses the same work-budget outcome for a known secret check and an unknown-client dummy check" $
      withTestEnvironment tinyWorkBudget $ \environment -> do
        known <- issueComposedApiClientToken environment (credentialsFor "automation-client" "current-secret") (scopeRequestFor [])
        unknown <-
          issueComposedApiClientToken
            environment {composedApiClientTokenStore = storeReturning "unknown-client" (Right Nothing)}
            (credentialsFor "unknown-client" "wrong-secret")
            (scopeRequestFor [])
        let malformedClient =
              clientOrDie
                (requiredClientId "automation-client")
                (PasswordHash "not-an-argon2-hash")
                [requiredScope "catalog:read"]
                [requiredScope "catalog:read"]
        malformed <-
          issueComposedApiClientToken
            environment {composedApiClientTokenStore = storeReturning "automation-client" (Right (Just malformedClient))}
            (credentialsFor "automation-client" "wrong-secret")
            (scopeRequestFor [])
        expectAll
          ( (known `shouldBe` ComposedApiClientTokenWorkBudgetExhausted)
              :| [ unknown `shouldBe` ComposedApiClientTokenWorkBudgetExhausted,
                   malformed `shouldBe` ComposedApiClientTokenWorkBudgetExhausted
                 ]
          )

    it "keeps database unavailability and disallowed scopes distinct from invalid credentials" $
      withTestEnvironment ampleWorkBudget $ \environment -> do
        unavailable <-
          issueComposedApiClientToken
            environment {composedApiClientTokenStore = storeReturning "automation-client" (Left storeUnavailable)}
            (credentialsFor "automation-client" "current-secret")
            (scopeRequestFor [])
        disallowed <- issueComposedApiClientToken environment (credentialsFor "automation-client" "current-secret") (scopeRequestFor ["catalog:admin"])
        duplicate <- issueComposedApiClientToken environment (credentialsFor "automation-client" "current-secret") (scopeRequestFor ["catalog:read", "catalog:read"])
        let noDefaultClient =
              clientOrDie
                (requiredClientId "automation-client")
                (composedApiClientSecretHash defaultClient)
                [requiredScope "catalog:read"]
                []
        noDefault <-
          issueComposedApiClientToken
            environment {composedApiClientTokenStore = storeReturning "automation-client" (Right (Just noDefaultClient))}
            (credentialsFor "automation-client" "current-secret")
            (scopeRequestFor [])
        expectAll
          ( (unavailable `shouldBe` ComposedApiClientTokenStoreUnavailable)
              :| [ disallowed `shouldBe` ComposedApiClientTokenInvalidScope,
                   duplicate `shouldBe` ComposedApiClientTokenInvalidScope,
                   noDefault `shouldBe` ComposedApiClientTokenInvalidScope
                 ]
          )

    it "redacts every rejection outcome and compares issued outcomes by public metadata only" $
      withTestEnvironment ampleWorkBudget $ \environment -> do
        first <- issueComposedApiClientToken environment (credentialsFor "automation-client" "current-secret") (scopeRequestFor [])
        second <- issueComposedApiClientToken environment (credentialsFor "automation-client" "current-secret") (scopeRequestFor [])
        expectAll
          ( (first `shouldBe` second)
              :| [ first `shouldNotBe` ComposedApiClientTokenInvalidClient,
                   show ComposedApiClientTokenInvalidClient `shouldBe` "ComposedApiClientTokenInvalidClient",
                   show ComposedApiClientTokenInvalidScope `shouldBe` "ComposedApiClientTokenInvalidScope",
                   show ComposedApiClientTokenStoreUnavailable `shouldBe` "ComposedApiClientTokenStoreUnavailable",
                   show ComposedApiClientTokenWorkBudgetExhausted `shouldBe` "ComposedApiClientTokenWorkBudgetExhausted",
                   show ComposedApiClientTokenIssueFailed `shouldBe` "ComposedApiClientTokenIssueFailed",
                   show [first] `shouldBe` "[ComposedApiClientTokenIssued <redacted> [\"catalog:read\",\"orders:write\"] 900]",
                   showsPrec 11 first "" `shouldBe` "(ComposedApiClientTokenIssued <redacted> [\"catalog:read\",\"orders:write\"] 900)"
                 ]
          )

    it "maps a signing failure to the stable issue-failed outcome" $ do
      signingKey <- JoseJwk.genJWK (JwaJwk.RSAGenParam 1024)
      let namedSigningKey = signingKey & JoseJwk.jwkKid ?~ "composed-test-key-v1"
          unusableSigningKey =
            case namedSigningKey ^. JoseJwk.jwkMaterial of
              JwaJwk.RSAKeyMaterial rsaParameters ->
                namedSigningKey & JoseJwk.jwkMaterial .~ JwaJwk.RSAKeyMaterial (rsaParameters & JwaJwk.rsaN .~ Base64Integer 1)
              _ -> namedSigningKey
      let configuration = requiredEither "composed JWT configuration" (mkComposedJwtConfiguration "https://issuer.example.test" "account-web" "composed-api" "composed-test-key-v1")
          runtime = requiredEither "composed JWT runtime" (loadComposedJwtRuntime configuration unusableSigningKey (JoseJwk.JWKSet [unusableSigningKey]))
      withTestEnvironment ampleWorkBudget $ \environment -> do
        outcome <-
          issueComposedApiClientToken
            environment {composedApiClientTokenJwtRuntime = runtime}
            (credentialsFor "automation-client" "current-secret")
            (scopeRequestFor [])
        outcome `shouldBe` ComposedApiClientTokenIssueFailed

    it "fails closed on malformed stored hashes and an unrepresentable expiry" $
      withTestEnvironment ampleWorkBudget $ \environment -> do
        let malformedClient =
              clientOrDie
                (requiredClientId "automation-client")
                (PasswordHash "not-an-argon2-hash")
                [requiredScope "catalog:read"]
                [requiredScope "catalog:read"]
        malformed <-
          issueComposedApiClientToken
            environment {composedApiClientTokenStore = storeReturning "automation-client" (Right (Just malformedClient))}
            (credentialsFor "automation-client" "current-secret")
            (scopeRequestFor [])
        expired <-
          issueComposedApiClientToken
            environment {composedApiClientTokenClock = pure (unixTimeNanoseconds maxBound)}
            (credentialsFor "automation-client" "current-secret")
            (scopeRequestFor [])
        expectAll
          ( (malformed `shouldBe` ComposedApiClientTokenInvalidClient)
              :| [expired `shouldBe` ComposedApiClientTokenIssueFailed]
          )

    it "validates client identifiers and scope configuration before storage" $ do
      let validClient = requiredClientId "automation-client"
          hashValue = testSecretHash "client-scope-salt" "client-secret"
          readScope = requiredScope "catalog:read"
          writeScope = requiredScope "orders:write"
          unknownScope = requiredScope "catalog:admin"
      let established = establishComposedApiClient defaultClient
          establishedAllowedScopes = oauth2ScopeText <$> establishedComposedApiClientAllowedScopes established
          effectiveScopes = oauth2ScopeText <$> intersectEstablishedComposedApiClientScopes established [readScope]
          durableEstablishedFields =
            case mkEstablishedComposedApiClient validClient [readScope, writeScope] of
              Left _ -> ("", [])
              Right client ->
                ( composedApiClientIdText (establishedComposedApiClientId client),
                  oauth2ScopeText <$> establishedComposedApiClientAllowedScopes client
                )
      expectAll
        ( (isIdError ComposedApiClientIdEmpty (mkComposedApiClientId "") `shouldBe` True)
            :| [ isIdError ComposedApiClientIdTooLong (mkComposedApiClientId (Text.replicate 129 "a")) `shouldBe` True,
                 isIdError ComposedApiClientIdInvalidCharacter (mkComposedApiClientId "bad client") `shouldBe` True,
                 isIdError ComposedApiClientIdInvalidCharacter (mkComposedApiClientId "automation-client-é") `shouldBe` True,
                 isConfigurationError ComposedApiClientAllowedScopesEmpty (mkComposedApiClient validClient hashValue [] []) `shouldBe` True,
                 isConfigurationError ComposedApiClientAllowedScopesDuplicate (mkComposedApiClient validClient hashValue [readScope, readScope] []) `shouldBe` True,
                 isConfigurationError ComposedApiClientDefaultScopesDuplicate (mkComposedApiClient validClient hashValue [readScope, writeScope] [readScope, readScope]) `shouldBe` True,
                 isConfigurationError ComposedApiClientDefaultScopeNotAllowed (mkComposedApiClient validClient hashValue [readScope] [writeScope]) `shouldBe` True
               ]
        )
      expectAll
        ( (composedApiClientIdText (establishedComposedApiClientId established) `shouldBe` "automation-client")
            :| [ establishedAllowedScopes `shouldBe` ["catalog:read", "orders:write"],
                 effectiveScopes `shouldBe` ["catalog:read"],
                 isConfigurationError ComposedApiClientAllowedScopesEmpty (mkEstablishedComposedApiClient validClient []) `shouldBe` True,
                 isConfigurationError ComposedApiClientAllowedScopesDuplicate (mkEstablishedComposedApiClient validClient [readScope, readScope]) `shouldBe` True,
                 isValidEstablishedClient (mkEstablishedComposedApiClient validClient [readScope, writeScope]) `shouldBe` True,
                 durableEstablishedFields `shouldBe` ("automation-client", ["catalog:read", "orders:write"]),
                 selectComposedApiClientScopes defaultClient [readScope, readScope]
                   `shouldBe` Left ComposedApiClientRequestedScopeDuplicate,
                 selectComposedApiClientScopes defaultClient [unknownScope]
                   `shouldBe` Left ComposedApiClientRequestedScopeNotAllowed,
                 showList [ComposedApiClientIdEmpty, ComposedApiClientIdTooLong, ComposedApiClientIdInvalidCharacter] ""
                   `shouldBe` "[ComposedApiClientIdEmpty,ComposedApiClientIdTooLong,ComposedApiClientIdInvalidCharacter]",
                 show ComposedApiClientIdEmpty `shouldBe` "ComposedApiClientIdEmpty",
                 ComposedApiClientIdEmpty /= ComposedApiClientIdTooLong `shouldBe` True,
                 showList
                   [ ComposedApiClientAllowedScopesEmpty,
                     ComposedApiClientAllowedScopesDuplicate,
                     ComposedApiClientDefaultScopesDuplicate,
                     ComposedApiClientDefaultScopeNotAllowed
                   ]
                   ""
                   `shouldBe` "[ComposedApiClientAllowedScopesEmpty,ComposedApiClientAllowedScopesDuplicate,ComposedApiClientDefaultScopesDuplicate,ComposedApiClientDefaultScopeNotAllowed]",
                 show ComposedApiClientAllowedScopesEmpty `shouldBe` "ComposedApiClientAllowedScopesEmpty",
                 ComposedApiClientAllowedScopesEmpty /= ComposedApiClientDefaultScopeNotAllowed `shouldBe` True,
                 showList [ComposedApiClientRequestedScopeDuplicate, ComposedApiClientRequestedScopeNotAllowed] ""
                   `shouldBe` "[ComposedApiClientRequestedScopeDuplicate,ComposedApiClientRequestedScopeNotAllowed]",
                 show ComposedApiClientRequestedScopeDuplicate `shouldBe` "ComposedApiClientRequestedScopeDuplicate",
                 ComposedApiClientRequestedScopeDuplicate /= ComposedApiClientRequestedScopeNotAllowed `shouldBe` True
               ]
        )

-- * Environment fixtures

ampleWorkBudget :: Word32
ampleWorkBudget = 65536

tinyWorkBudget :: Word32
tinyWorkBudget = 1

withTestEnvironment :: Word32 -> (ComposedApiClientTokenEnvironment -> IO a) -> IO a
withTestEnvironment workBudgetKibibytes action = do
  signingKey <- JoseJwk.genJWK (JwaJwk.RSAGenParam 1024)
  let namedSigningKey = signingKey & JoseJwk.jwkKid ?~ "composed-test-key-v1"
      configuration = requiredEither "composed JWT configuration" (mkComposedJwtConfiguration "https://issuer.example.test" "account-web" "composed-api" "composed-test-key-v1")
      runtime = requiredEither "composed JWT runtime" (loadComposedJwtRuntime configuration namedSigningKey (JoseJwk.JWKSet [namedSigningKey]))
  workGate <- newPasswordWorkGate (required "password work budget" (mkPasswordWorkBudget workBudgetKibibytes))
  now <- currentUnixTimeNanoseconds
  action
    ComposedApiClientTokenEnvironment
      { composedApiClientTokenStore = storeReturning "automation-client" (Right (Just defaultClient)),
        composedApiClientTokenWorkGate = workGate,
        composedApiClientTokenJwtRuntime = runtime,
        composedApiClientTokenClock = pure now
      }

testHashingPolicy :: PasswordHashingPolicy
testHashingPolicy = required "test password hashing policy" (mkPasswordHashingPolicy (argon2Iterations 1) (argon2MemoryKib 8) (argon2Parallelism 1))

testSecretHash :: ByteString.ByteString -> Text -> PasswordHash
testSecretHash salt secretValue = required "test API client secret hash" (hashPasswordWithSalt testHashingPolicy salt (mkPassword secretValue))

defaultClient :: ComposedApiClient
defaultClient =
  clientOrDie
    (requiredClientId "automation-client")
    (testSecretHash "current-secret-salt-01" "current-secret")
    [requiredScope "catalog:read", requiredScope "orders:write"]
    [requiredScope "catalog:read", requiredScope "orders:write"]

clientOrDie :: ComposedApiClientId -> PasswordHash -> [OAuth2Scope] -> [OAuth2Scope] -> ComposedApiClient
clientOrDie clientId secretHash allowedScopes defaultScopes =
  requiredEither "valid composed API client" (mkComposedApiClient clientId secretHash allowedScopes defaultScopes)

storeReturning :: Text -> Either ApiClientStoreError (Maybe ComposedApiClient) -> ApiClientStore ComposedApiClientId ComposedApiClient EstablishedComposedApiClient
storeReturning expectedClientId findResult =
  ApiClientStore
    { findApiClient = \clientId ->
        if composedApiClientIdText clientId == expectedClientId
          then pure findResult
          else error "App.Composed.ApiClientTokenSpec: lookup used an unexpected API client ID",
      establishApiClient = const (pure (Left storeUnavailable))
    }

explodingStore :: ApiClientStore ComposedApiClientId ComposedApiClient EstablishedComposedApiClient
explodingStore =
  ApiClientStore
    { findApiClient = const (error "must not query storage for an invalid composed API client ID"),
      establishApiClient = const (error "unused by token issuance")
    }

storeUnavailable :: ApiClientStoreError
storeUnavailable = ApiClientStoreUnavailable (mkAuthenticationDependency (requiredSecurityFailureCodeOrDie "composed.api-client.store-unavailable"))

-- * OAuth request-value fixtures

credentialsFor :: Text -> Text -> OAuth2ClientCredentials
credentialsFor clientIdValue secretValue =
  case runRequestCodec (oauth2ClientSecretBasicCodec maximumBytes) (ApiRequestData [] [(authorizationHeader, basicHeaderValue)] [] []) of
    ApiRequestDecoded credentials -> credentials
    _ -> error "expected valid composed OAuth client Basic credentials"
  where
    maximumBytes = requiredOAuth2ClientCredentialsMaximumBytesOrDie 256
    authorizationHeader = required "Authorization header name" (apiHeaderName "Authorization")
    basicHeaderValue = "Basic " <> TextEncoding.decodeUtf8 (Base64.encode (TextEncoding.encodeUtf8 (clientIdValue <> ":" <> secretValue)))

scopeRequestFor :: [Text] -> OAuth2ScopeRequest
scopeRequestFor scopes =
  case runRequestCodec oauth2ClientCredentialsRequestCodec (ApiRequestData [] [] [] formFields) of
    ApiRequestDecoded request -> oauth2ClientCredentialsScopes request
    _ -> error "expected valid composed OAuth client-credentials form"
  where
    formFields = ("grant_type", "client_credentials") : [("scope", Text.unwords scopes) | not (null scopes)]

requiredClientId :: Text -> ComposedApiClientId
requiredClientId value =
  case mkComposedApiClientId value of
    Right clientId -> clientId
    Left failure -> error ("expected valid composed API client ID: " <> show failure)

requiredScope :: Text -> OAuth2Scope
requiredScope value =
  case mkOAuth2Scope value of
    Right scope -> scope
    Left failure -> error ("expected valid OAuth scope: " <> show failure)

requireIssuedToken :: ComposedApiClientTokenOutcome -> IO EncodedJwt
requireIssuedToken outcome =
  case outcome of
    ComposedApiClientTokenIssued token _ _ -> pure token
    _ -> expectationFailure ("expected an API token, got: " <> show outcome) >> error "unreachable"

requiredPayload :: EncodedJwt -> IO Aeson.Object
requiredPayload token =
  case ByteString.split 46 (encodedJwtBytes token) of
    [_header, payload, _signature] ->
      case Base64Url.decode payload of
        Right decoded ->
          case Aeson.decodeStrict decoded of
            Just (Aeson.Object object) -> pure object
            _ -> expectationFailure "expected an object JWT payload" >> error "unreachable"
        Left message -> expectationFailure ("expected base64url JWT payload: " <> message) >> error "unreachable"
    _ -> expectationFailure "expected a three-part JWT" >> error "unreachable"

isIssuedWithLifetime :: ComposedApiClientTokenOutcome -> Bool
isIssuedWithLifetime outcome =
  case outcome of
    ComposedApiClientTokenIssued _ _ lifetime -> lifetime == composedApiClientTokenLifetimeSeconds
    _ -> False

isIdError :: ComposedApiClientIdError -> Either ComposedApiClientIdError value -> Bool
isIdError expected result =
  case result of
    Left failure -> failure == expected
    Right _ -> False

isConfigurationError :: ComposedApiClientConfigurationError -> Either ComposedApiClientConfigurationError value -> Bool
isConfigurationError expected result =
  case result of
    Left failure -> failure == expected
    Right _ -> False

isValidEstablishedClient :: Either ComposedApiClientConfigurationError EstablishedComposedApiClient -> Bool
isValidEstablishedClient result =
  case result of
    Left _ -> False
    Right _ -> True

required :: String -> Maybe value -> value
required label = fromMaybe (error ("App.Composed.ApiClientTokenSpec: " <> label))

requiredEither :: (Show failure) => String -> Either failure value -> value
requiredEither label result =
  case result of
    Right value -> value
    Left failure -> error ("App.Composed.ApiClientTokenSpec: " <> label <> ": " <> show failure)
