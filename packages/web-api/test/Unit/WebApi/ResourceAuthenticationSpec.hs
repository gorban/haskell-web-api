{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Lens (review, (&), (?~))
import Crypto.JOSE.Header (HeaderParam (..), RequiredProtection (..))
import Crypto.JOSE.JWA.JWK qualified as JwaJwk
import Crypto.JOSE.JWA.JWS qualified as JwaJws
import Crypto.JOSE.JWK qualified as JoseJwk
import Crypto.JOSE.JWS qualified as JoseJws
import Crypto.JWT qualified as Jwt
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Lazy qualified as LazyByteString
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import HarchWeb qualified
import HarchWeb.Account qualified as Account
import HarchWeb.Api (ApiRequestData (..), ApiRequestDecodeResult (..), apiHeaderName, runRequestCodec)
import HarchWeb.Authentication (ApiClientStore (..), ApiClientStoreError (..), EncodedJwt, OAuth2ClientCredentials, OAuth2Scope, OAuth2ScopeRequest, encodedJwtBytes, mkOAuth2Scope, oauth2ClientCredentialsRequestCodec, oauth2ClientCredentialsScopes, oauth2ClientSecretBasicCodec, oauth2ScopeText, requiredOAuth2ClientCredentialsMaximumBytesOrDie)
import HarchWeb.Password (PasswordHashingPolicy, argon2Iterations, argon2MemoryKib, argon2Parallelism, hashPasswordWithSalt, mkPassword, mkPasswordHashingPolicy, mkPasswordWorkBudget, newPasswordWorkGate)
import HarchWeb.Session (OpaqueSession (..), SessionId, mkSessionId)
import HarchWeb.Time (UnixTimeNanoseconds, addUnixTimeNanoseconds, currentUnixTimeNanoseconds, unixTimeNanosecondsValue)
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import TestCore.Wai (waiRequest)
import WebApi.AccountJwt
import WebApi.ApiClient (ApiClient, ApiClientId, EstablishedApiClient, apiClientIdText, mkApiClient, mkApiClientId, mkEstablishedApiClient)
import WebApi.ApiClientToken
import WebApi.ResourceAuthentication (resourceAuthenticationPipeline)
import WebApi.Route (AppAuthorization, AppRequestContext (..), AppRoute (LoginRoute, SecondApiRoute), defaultRequestContext, endpointMetadata, resourceReadScope)
import WebApi.Session (AccountSessionStore (..))

-- | Direct pipeline-level coverage for the AHI-4D combined
-- account-or-API-client-bearer profile, mirroring how
-- 'Unit.WebApi.AccountJwtSpec' tests 'accountJwtAuthenticationPipeline'
-- directly via 'HarchWeb.runAuthenticationPipeline'. This builds its own
-- throwaway RSA key/runtime (like 'Unit.WebApi.ApiClientTokenSpec'), so
-- neither Postgres nor the full application composition is required.
spec =
  describe "WebApi.ResourceAuthentication" $ do
    it "halts an anonymous request with the account login challenge" $
      withTestRuntime $ \runtime -> do
        result <- HarchWeb.runAuthenticationPipeline (resourceAuthenticationPipeline testSessionStore testClock runtime unreachableClientStore) (secondApiEndpointRequest Nothing [])
        expectLoginRedirect result

    it "admits an authenticated account principal regardless of the required scope" $
      withTestRuntime $ \runtime -> do
        issued <- issueAccountSessionJwt (accountJwtIssuerFromRuntime runtime) (validSession expectedTestSessionId)
        token <- either (\issueError -> expectationFailure ("expected account JWT issuance: " <> show issueError) >> error "unreachable") pure issued
        cookieText <- case HarchWeb.renderAuthenticationCookie (accountJwtCookie (accountJwtIssuerFromRuntime runtime)) token of
          Just value -> pure value
          Nothing -> expectationFailure "expected a renderable test cookie" >> error "unreachable"
        result <- HarchWeb.runAuthenticationPipeline (resourceAuthenticationPipeline testSessionStore testClock runtime unreachableClientStore) (secondApiEndpointRequest Nothing [("Cookie", TextEncoding.encodeUtf8 cookieText)])
        case result of
          HarchWeb.ContinueEndpoint admittedContext -> admittedContext `shouldBe` defaultRequestContext
          HarchWeb.HaltEndpoint response -> expectationFailure ("expected the account principal to be admitted, got " <> show response)

    it "admits an API-client bearer token carrying the required scope" $
      withTestRuntime $ \runtime ->
        withTestApiClientStore [resourceReadScope, otherScope] [resourceReadScope] $ \clientStore -> do
          token <- mintApiClientToken runtime clientStore [resourceReadScope]
          result <- HarchWeb.runAuthenticationPipeline (resourceAuthenticationPipeline testSessionStore testClock runtime clientStore) (secondApiEndpointRequest Nothing [("Authorization", "Bearer " <> encodedJwtBytes token)])
          case result of
            HarchWeb.ContinueEndpoint admittedContext -> admittedContext `shouldBe` defaultRequestContext
            HarchWeb.HaltEndpoint response -> expectationFailure ("expected the scoped API-client principal to be admitted, got " <> show response)

    it "rejects an API-client bearer token missing the required scope, attributing the resource.scope-denied failure code" $
      withTestRuntime $ \runtime ->
        withTestApiClientStore [otherScope] [otherScope] $ \clientStore -> do
          token <- mintApiClientToken runtime clientStore [otherScope]
          (sink, capturedEvents) <- newCapturingSecurityEventSink
          result <- HarchWeb.runAuthenticationPipeline (resourceAuthenticationPipeline testSessionStore testClock runtime clientStore) (secondApiEndpointRequest (Just sink) [("Authorization", "Bearer " <> encodedJwtBytes token)])
          expectLoginRedirect result
          events <- readIORef capturedEvents
          show events `shouldContain` "resource.scope-denied"

    it "rejects an API-client bearer token whose current allowance no longer intersects its granted scope" $
      withTestRuntime $ \runtime ->
        withTestApiClientStore [resourceReadScope, otherScope] [resourceReadScope] $ \issuanceClientStore -> do
          token <- mintApiClientToken runtime issuanceClientStore [resourceReadScope]
          -- The client's durable allowance narrowed since issuance (the
          -- scope the token was granted is no longer current), so
          -- establishment must intersect down to nothing rather than trust
          -- the token's embedded claim.
          withTestApiClientStore [otherScope] [otherScope] $ \narrowedClientStore -> do
            result <- HarchWeb.runAuthenticationPipeline (resourceAuthenticationPipeline testSessionStore testClock runtime narrowedClientStore) (secondApiEndpointRequest Nothing [("Authorization", "Bearer " <> encodedJwtBytes token)])
            expectLoginRedirect result

    it "rejects a bearer token for an unknown or disabled API client, attributing the resource.api-client.rejected failure code" $
      withTestRuntime $ \runtime ->
        withTestApiClientStore [resourceReadScope] [resourceReadScope] $ \issuanceClientStore -> do
          token <- mintApiClientToken runtime issuanceClientStore [resourceReadScope]
          (sink, capturedEvents) <- newCapturingSecurityEventSink
          result <- HarchWeb.runAuthenticationPipeline (resourceAuthenticationPipeline testSessionStore testClock runtime notFoundClientStore) (secondApiEndpointRequest (Just sink) [("Authorization", "Bearer " <> encodedJwtBytes token)])
          expectLoginRedirect result
          events <- readIORef capturedEvents
          show events `shouldContain` "resource.api-client.rejected"

    -- Each malformed shape below fails a *different* parse step
    -- ('WebApi.ResourceAuthentication.parseResourceJwtClaims' tries the
    -- account shape first, then an ordered chain of API-client checks): no
    -- subject at all, a URI-shaped subject (valid 'Crypto.JWT.StringOrURI',
    -- but not the plain-string form an API-client ID requires), a
    -- plain-string subject with characters 'WebApi.ApiClient.mkApiClientId'
    -- rejects, and a scope claim with a character 'HarchWeb.mkOAuth2Scope'
    -- rejects. Each is tabled here as one act (mint) and one comparison
    -- (verify), matching this codebase's established one-act-one-comparison
    -- convention.
    it "rejects API-client bearer tokens with a malformed subject or scope claim" $
      withTestRuntime $ \runtime ->
        withTestApiClientStore [resourceReadScope] [resourceReadScope] $ \clientStore -> do
          let pipelineVerifier = HarchWeb.authenticationProofVerifier (resourceAuthenticationPipeline testSessionStore testClock runtime clientStore)
              expectClaimsRejected description extraClaims = do
                token <- mintClaimsToken runtime extraClaims
                verified <- HarchWeb.verifyAuthenticationProof pipelineVerifier (HarchWeb.jwtProofFromCookie token)
                case verified of
                  Left proofFailure -> show proofFailure `shouldContain` "resource.jwt.claims-rejected"
                  Right _ -> expectationFailure (description <> ": expected the malformed claim to be rejected during verification")
          expectClaimsRejected
            "missing subject"
            ["scope" Aeson..= oauth2ScopeText resourceReadScope]
          expectClaimsRejected
            "URI-shaped subject"
            [ "sub" Aeson..= ("https://issuer.example.test/clients/one" :: Text.Text),
              "scope" Aeson..= oauth2ScopeText resourceReadScope
            ]
          expectClaimsRejected
            "subject with characters an API-client ID rejects"
            [ "sub" Aeson..= (" invalid client id" :: Text.Text),
              "scope" Aeson..= oauth2ScopeText resourceReadScope
            ]
          expectClaimsRejected
            "scope with a character an OAuth scope rejects"
            [ "sub" Aeson..= review Jwt.string testApiClientIdText,
              "scope" Aeson..= ("\"quoted\"" :: Text.Text)
            ]
          expectClaimsRejected
            "scope claim entirely absent, with an otherwise-valid subject"
            ["sub" Aeson..= review Jwt.string testApiClientIdText]

    it "reports the durable API-client store as unavailable rather than admitting or rejecting outright" $
      withTestRuntime $ \runtime ->
        withTestApiClientStore [resourceReadScope] [resourceReadScope] $ \issuanceClientStore -> do
          token <- mintApiClientToken runtime issuanceClientStore [resourceReadScope]
          (sink, capturedEvents) <- newCapturingSecurityEventSink
          result <- HarchWeb.runAuthenticationPipeline (resourceAuthenticationPipeline testSessionStore testClock runtime unavailableClientStore) (secondApiEndpointRequest (Just sink) [("Authorization", "Bearer " <> encodedJwtBytes token)])
          case result of
            HarchWeb.HaltEndpoint (HarchWeb.NonPageBodyResponse responseBody) ->
              expectAll
                ( (HarchWeb.responseStatus responseBody `shouldBe` Http.status503)
                    :| [Text.isInfixOf "Authentication is temporarily unavailable." (HarchWeb.responseBody responseBody) `shouldBe` True]
                )
            other -> expectationFailure ("expected an unavailable-dependency response, got " <> show other)
          events <- readIORef capturedEvents
          show events `shouldContain` "test.api-client-store-unavailable"

-- | The real wall clock, not a fixed instant: JOSE's standard-claims
-- validation checks @nbf@\/@exp@ against the actual current time
-- ('Control.Monad.Time.MonadTime'), not against whatever clock value an
-- application used to compute those embedded claims at issuance. A fixed
-- 1970-epoch-relative clock here would mint tokens that read as already
-- expired the moment they are verified.
testClock :: IO UnixTimeNanoseconds
testClock = currentUnixTimeNanoseconds

secondApiEndpointRequest :: Maybe HarchWeb.SecurityEventSink -> Http.RequestHeaders -> HarchWeb.EndpointRequest AppRoute AppRequestContext AppAuthorization
secondApiEndpointRequest sink headers =
  HarchWeb.EndpointRequest
    { HarchWeb.endpointWaiRequest = (waiRequest ["api", "second"]) {Wai.requestHeaders = headers},
      HarchWeb.endpointRouteRequest = HarchWeb.RouteRequest SecondApiRoute defaultRequestContext,
      HarchWeb.endpointMetadata = endpointMetadata SecondApiRoute,
      HarchWeb.endpointSecurityEventSink = sink,
      HarchWeb.endpointDispatchKind = HarchWeb.EndpointMatched
    }

-- | A real (in-memory) security-event sink, so a rejection's private
-- failure-code attribution — never surfaced in any public response body —
-- is still genuinely observable and forceable from a test, exactly as it
-- would be by a real deployment's telemetry.
newCapturingSecurityEventSink :: IO (HarchWeb.SecurityEventSink, IORef [HarchWeb.SecurityEvent])
newCapturingSecurityEventSink = do
  capturedEvents <- newIORef []
  let sink = HarchWeb.SecurityEventSink $ \_ eventBody -> do
        modifyIORef' capturedEvents (eventBody :)
        pure HarchWeb.SecurityEventDelivered
  pure (sink, capturedEvents)

expectLoginRedirect :: HarchWeb.EndpointGuardResult AppRoute AppRequestContext -> Expectation
expectLoginRedirect result =
  case result of
    HarchWeb.HaltEndpoint (HarchWeb.NonPageInternalRedirectResponse responseBody target) ->
      expectAll
        ( (HarchWeb.responseStatus responseBody `shouldBe` Http.status303)
            :| [ HarchWeb.requestRoute target `shouldBe` LoginRoute,
                 requestAccountPrincipal (HarchWeb.requestContext target) `shouldBe` Nothing
               ]
        )
    HarchWeb.HaltEndpoint response -> expectationFailure ("expected the account login challenge, got " <> show response)
    HarchWeb.ContinueEndpoint _ -> expectationFailure "expected authentication or authorization to halt"

-- | A single valid durable session, matching whichever 'SessionId' the
-- caller minted a cookie for.
validSession :: SessionId -> OpaqueSession Account.AccountId
validSession testSessionId =
  OpaqueSession
    { sessionId = testSessionId,
      sessionPrincipal = requiredAccountId "resource-test-account",
      sessionIssuedAtNanoseconds = 100,
      sessionExpiresAtNanoseconds = maxBound
    }

testSessionStore :: AccountSessionStore
testSessionStore =
  AccountSessionStore
    { saveAccountSession = \_ -> error "unexpected test session save",
      loadAccountSession = \requestedSessionId ->
        pure (Right (if requestedSessionId == expectedTestSessionId then Just (validSession expectedTestSessionId) else Nothing)),
      invalidateAccountSession = \_ _ -> error "unexpected test session invalidation"
    }

expectedTestSessionId :: SessionId
expectedTestSessionId = requiredSessionId "ACCOUNTSESSION0123456789ABCDEF0123456789AB"

requiredSessionId :: Text.Text -> SessionId
requiredSessionId value = fromMaybe (error "expected a valid test session id") (mkSessionId value)

requiredAccountId :: Text.Text -> Account.AccountId
requiredAccountId value = fromMaybe (error "expected a valid test account id") (Account.mkAccountId value)

otherScope :: OAuth2Scope
otherScope = either (\_ -> error "invalid test scope declaration") id (mkOAuth2Scope "unrelated:scope")

-- | Builds a fresh, throwaway RSA key pair and 'AccountJwtRuntime', exactly
-- like 'Unit.WebApi.ApiClientTokenSpec''s own fixture: this pipeline's tests
-- need real RS256 signing/verification, not a Postgres-backed one.
withTestRuntime :: (AccountJwtRuntime -> IO a) -> IO a
withTestRuntime action =
  withSystemTempDirectory "web-api-resource-authentication" $ \directory -> do
    signingKey <- JoseJwk.genJWK (JwaJwk.RSAGenParam 1024)
    let namedSigningKey = signingKey & JoseJwk.jwkKid ?~ "test-resource-key-v1"
        signingFile = directory </> "private.jwk"
        verificationFile = directory </> "verification.jwks"
        configuration =
          either
            (\configurationError -> error ("expected a valid test shared JWT configuration: " <> show configurationError))
            id
            ( mkAccountJwtConfiguration
                AccountJwtRawConfiguration
                  { rawAccountJwtIssuer = "https://issuer.example.test",
                    rawAccountJwtAudience = "web-api",
                    rawAccountJwtActiveKeyId = "test-resource-key-v1",
                    rawAccountJwtSigningJwkFile = signingFile,
                    rawAccountJwtVerificationJwkSetFile = verificationFile,
                    rawAccountJwtCookieName = "__Host-harch-session",
                    rawAccountJwtCookieMaxAgeSeconds = 28800
                  }
            )
    ByteString.writeFile signingFile (LazyByteString.toStrict (Aeson.encode namedSigningKey))
    ByteString.writeFile verificationFile (LazyByteString.toStrict (Aeson.encode (JoseJwk.JWKSet [namedSigningKey])))
    loaded <- loadAccountJwtRuntime configuration
    case loaded of
      Right runtime -> action runtime
      Left loadError -> expectationFailure ("expected a valid test JWT runtime: " <> show loadError) >> error "unreachable"

-- | A durable API-client store with exactly one enabled client, its allowed
-- scopes, and a real (cheap-policy) Argon2 secret hash so
-- 'issueApiClientToken' can mint a real bearer token against it.
withTestApiClientStore :: [OAuth2Scope] -> [OAuth2Scope] -> (ApiClientStore ApiClientId ApiClient EstablishedApiClient -> IO a) -> IO a
withTestApiClientStore allowedScopes defaultScopes action = do
  let clientId = either (\_ -> error "invalid test api client id") id (mkApiClientId testApiClientIdText)
      secretHash = fromMaybe (error "expected a valid test secret hash") (hashPasswordWithSalt testHashingPolicy "resource-test-client-salt" (mkPassword testApiClientSecretText))
      client = either (\_ -> error "expected a valid test api client") id (mkApiClient clientId (secretHash :| []) allowedScopes defaultScopes)
      established = either (\_ -> error "expected a valid established test api client") id (mkEstablishedApiClient clientId allowedScopes)
  action
    ApiClientStore
      { findApiClient = \requestedClientId -> pure (Right (if apiClientIdText requestedClientId == apiClientIdText clientId then Just client else Nothing)),
        establishApiClient = \requestedClientId -> pure (Right (if apiClientIdText requestedClientId == apiClientIdText clientId then Just established else Nothing))
      }

testApiClientIdText :: Text.Text
testApiClientIdText = "resource-test-client"

testApiClientSecretText :: Text.Text
testApiClientSecretText = "resource-test-client-secret"

-- | A durable API-client store that must never be consulted: the anonymous
-- and account-principal tests never present a bearer proof at all, so this
-- fails loudly if establishment is ever reached by mistake.
unreachableClientStore :: ApiClientStore ApiClientId ApiClient EstablishedApiClient
unreachableClientStore =
  ApiClientStore
    { findApiClient = \_ -> error "unexpected api client lookup",
      establishApiClient = \_ -> error "unexpected api client establishment"
    }

-- | A durable API-client store genuinely consulted, but with no matching
-- client: models an unknown client ID or one disabled since token issuance.
notFoundClientStore :: ApiClientStore ApiClientId ApiClient EstablishedApiClient
notFoundClientStore =
  ApiClientStore
    { findApiClient = \_ -> pure (Right Nothing),
      establishApiClient = \_ -> pure (Right Nothing)
    }

-- | A durable API-client store reporting itself unavailable, distinct from
-- an unknown client: this is the dependency-unavailable rail, not a
-- rejection.
unavailableClientStore :: ApiClientStore ApiClientId ApiClient EstablishedApiClient
unavailableClientStore =
  ApiClientStore
    { findApiClient = \_ -> pure (Left (ApiClientStoreUnavailable dependency)),
      establishApiClient = \_ -> pure (Left (ApiClientStoreUnavailable dependency))
    }
  where
    dependency = HarchWeb.mkAuthenticationDependency (HarchWeb.requiredSecurityFailureCodeOrDie "test.api-client-store-unavailable")

testHashingPolicy :: PasswordHashingPolicy
testHashingPolicy = fromMaybe (error "expected a valid test Argon2 policy") (mkPasswordHashingPolicy (argon2Iterations 1) (argon2MemoryKib 8) (argon2Parallelism 1))

mintApiClientToken :: AccountJwtRuntime -> ApiClientStore ApiClientId ApiClient EstablishedApiClient -> [OAuth2Scope] -> IO EncodedJwt
mintApiClientToken runtime clientStore requestedScopes = do
  workGate <- newPasswordWorkGate (fromMaybe (error "expected a valid test work budget") (mkPasswordWorkBudget 8192))
  outcome <-
    issueApiClientToken
      ApiClientTokenEnvironment
        { apiClientTokenStore = clientStore,
          apiClientTokenWorkGate = workGate,
          apiClientTokenIssuance = accountJwtRuntimeSharedIssuance runtime,
          apiClientTokenClock = testClock
        }
      testClientCredentials
      (testScopeRequestFor requestedScopes)
  case outcome of
    ApiClientTokenIssued token _ _ -> pure token
    other -> expectationFailure ("expected a real issued test bearer token, got " <> show other) >> error "unreachable"

testClientCredentials :: OAuth2ClientCredentials
testClientCredentials =
  case runRequestCodec (oauth2ClientSecretBasicCodec maximumBytes) (ApiRequestData [] [(authorizationHeader, basicHeaderValue)] [] []) of
    ApiRequestDecoded credentials -> credentials
    _ -> error "expected valid test OAuth client Basic credentials"
  where
    maximumBytes = requiredOAuth2ClientCredentialsMaximumBytesOrDie 256
    authorizationHeader = fromMaybe (error "expected a valid Authorization header name") (apiHeaderName "Authorization")
    basicHeaderValue = "Basic " <> TextEncoding.decodeUtf8 (Base64.encode (TextEncoding.encodeUtf8 (testApiClientIdText <> ":" <> testApiClientSecretText)))

testScopeRequestFor :: [OAuth2Scope] -> OAuth2ScopeRequest
testScopeRequestFor scopes =
  case runRequestCodec oauth2ClientCredentialsRequestCodec (ApiRequestData [] [] [] formFields) of
    ApiRequestDecoded request -> oauth2ClientCredentialsScopes request
    _ -> error "expected a valid test OAuth client-credentials token request"
  where
    formFields = ("grant_type", "client_credentials") : [("scope", Text.unwords (oauth2ScopeText <$> scopes)) | not (null scopes)]

-- | Signs a real bearer token against the runtime's own proven key, with
-- the same @iss@\/@aud@\/@iat@\/@nbf@\/@exp@ shape 'claimsForApiClient'
-- produces, but letting the caller supply (and override) the remaining
-- claims directly — so a test can mint an otherwise-well-formed token with
-- exactly one malformed field, independent of 'issueApiClientToken''s own
-- always-valid issuance path.
mintClaimsToken :: AccountJwtRuntime -> [(Aeson.Key, Aeson.Value)] -> IO EncodedJwt
mintClaimsToken runtime extraClaims = do
  now <- currentUnixTimeNanoseconds
  let issuance = accountJwtRuntimeSharedIssuance runtime
      claims =
        Aeson.object
          ( [ "iss" Aeson..= sharedJwtIssuer issuance,
              "aud" Aeson..= Jwt.Audience [sharedJwtAudience issuance],
              "iat" Aeson..= testNumericDate now,
              "nbf" Aeson..= testNumericDate now,
              "exp" Aeson..= testNumericDate (testAddSeconds 900 now)
            ]
              <> extraClaims
          )
      signer = HarchWeb.joseJwtSigner (sharedJwtSigningKey issuance)
      header :: HarchWeb.JWSHeader RequiredProtection
      header =
        JoseJws.newJWSHeaderProtected JwaJws.RS256
          & JoseJws.kid ?~ HeaderParam RequiredProtection (sharedJwtActiveKeyId issuance)
  issued <- HarchWeb.signJwt signer header claims
  case issued of
    Right token -> pure token
    Left issueError -> expectationFailure ("expected malformed test JWT issuance: " <> show issueError) >> error "unreachable"

testNumericDate :: UnixTimeNanoseconds -> Jwt.NumericDate
testNumericDate instant = Jwt.NumericDate (posixSecondsToUTCTime (fromIntegral (unixTimeNanosecondsValue instant) / 1000000000))

testAddSeconds :: Integer -> UnixTimeNanoseconds -> UnixTimeNanoseconds
testAddSeconds seconds instant =
  fromMaybe (error "expected a representable test expiry instant") (addUnixTimeNanoseconds instant (fromIntegral (seconds * 1000000000)))
