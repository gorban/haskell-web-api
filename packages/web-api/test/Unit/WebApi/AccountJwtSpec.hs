{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Lens (matching, (&), (?~))
import Crypto.JOSE.Header (HeaderParam (..), RequiredProtection (..))
import Crypto.JOSE.JWA.JWK qualified as JwaJwk
import Crypto.JOSE.JWA.JWS qualified as JwaJws
import Crypto.JOSE.JWK qualified as JoseJwk
import Crypto.JOSE.JWS qualified as JoseJws
import Crypto.JWT qualified as Jwt
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import HarchWeb qualified
import HarchWeb.Account qualified as Account
import HarchWeb.Session (OpaqueSession (..), SessionId, mkSessionId, sessionIdText)
import HarchWeb.Time (UnixTimeNanoseconds, unixTimeNanosecondsValue)
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import TestCore.Wai (waiRequest)
import WebApi.AccountJwt
import WebApi.AccountPrincipal (mkAccountPrincipal)
import WebApi.Route (AppRequestContext (..), AppRoute (LoginRoute, ProfileRoute), defaultRequestContext)
import WebApi.Session (AccountSessionStore (..), AccountSessionStoreError (AccountSessionStoreUnavailable))

spec =
  describe "WebApi.AccountJwt" $ do
    it "rejects incomplete account-JWT configuration before reading deployment key files" $ do
      let valid = mkAccountJwtConfiguration "https://accounts.example.test" "web-api-account" "account-key-v1" "private.jwk" "verification.jwks" "__Host-harch-session" 28800
      expectAll
        ( (valid `shouldSatisfy` isRight)
            :| [ mkAccountJwtConfiguration "" "web-api-account" "account-key-v1" "private.jwk" "verification.jwks" "__Host-harch-session" 28800 `shouldBe` Left AccountJwtIssuerInvalid,
                 mkAccountJwtConfiguration "https://accounts.example.test" "" "account-key-v1" "private.jwk" "verification.jwks" "__Host-harch-session" 28800 `shouldBe` Left AccountJwtAudienceInvalid,
                 mkAccountJwtConfiguration "https://accounts.example.test" "web-api-account" "" "private.jwk" "verification.jwks" "__Host-harch-session" 28800 `shouldBe` Left AccountJwtActiveKeyIdInvalid,
                 mkAccountJwtConfiguration "https://accounts.example.test" "web-api-account" (Text.replicate 129 "a") "private.jwk" "verification.jwks" "__Host-harch-session" 28800 `shouldBe` Left AccountJwtActiveKeyIdInvalid,
                 mkAccountJwtConfiguration "https://accounts.example.test" "web-api-account" "account-key-v1" "" "verification.jwks" "__Host-harch-session" 28800 `shouldBe` Left AccountJwtSigningJwkFileInvalid,
                 mkAccountJwtConfiguration "https://accounts.example.test" "web-api-account" "account-key-v1" "private.jwk" "" "__Host-harch-session" 28800 `shouldBe` Left AccountJwtVerificationJwkSetFileInvalid,
                 mkAccountJwtConfiguration "https://accounts.example.test" "web-api-account" "account-key-v1" "private.jwk" "verification.jwks" "session" 28800 `shouldBe` Left AccountJwtCookiePolicyInvalid,
                 AccountJwtIssuerInvalid == AccountJwtIssuerInvalid `shouldBe` True,
                 show AccountJwtIssuerInvalid `shouldBe` "AccountJwtIssuerInvalid",
                 show AccountJwtIssueFailed `shouldBe` "AccountJwtIssueFailed",
                 show AccountJwtVerificationKeyMissing `shouldBe` "AccountJwtVerificationKeyMissing"
               ]
        )

    it "keeps public JWT configuration and failure values comparable without exposing key material" $ do
      let configuration = requiredConfiguration "private.jwk" "verification.jwks"
          otherConfiguration = requiredConfiguration "other-private.jwk" "verification.jwks"
          configurationErrors =
            [ AccountJwtIssuerInvalid,
              AccountJwtAudienceInvalid,
              AccountJwtActiveKeyIdInvalid,
              AccountJwtSigningJwkFileInvalid,
              AccountJwtVerificationJwkSetFileInvalid,
              AccountJwtCookiePolicyInvalid
            ]
          loadErrors =
            [ AccountJwtSigningJwkUnreadable,
              AccountJwtVerificationJwkSetUnreadable,
              AccountJwtSigningJwkMalformed,
              AccountJwtVerificationJwkSetMalformed,
              AccountJwtSigningKeyIdMismatch,
              AccountJwtVerificationKeyMissing,
              AccountJwtSigningKeyNotRsaPrivate,
              AccountJwtVerificationKeyNotRsa
            ]
      expectAll
        ( (configuration == configuration `shouldBe` True)
            :| [ configuration /= otherConfiguration `shouldBe` True,
                 show configuration `shouldContain` "AccountJwtConfiguration",
                 all (\failure -> failure == failure) configurationErrors `shouldBe` True,
                 show configurationErrors `shouldContain` "AccountJwtCookiePolicyInvalid",
                 all (\failure -> failure == failure) loadErrors `shouldBe` True,
                 show loadErrors `shouldContain` "AccountJwtVerificationKeyNotRsa",
                 AccountJwtIssueFailed == AccountJwtIssueFailed `shouldBe` True,
                 show [AccountJwtIssueFailed] `shouldBe` "[AccountJwtIssueFailed]"
               ]
        )

    it "loads only a matching RS256 private key/JWK set and admits the current durable session" $
      withTestRuntime $ \runtime signingKey session -> do
        let issuer = accountJwtIssuerFromRuntime runtime
        show runtime `shouldBe` "AccountJwtRuntime <redacted>"
        issued <- issueAccountSessionJwt issuer session
        token <-
          case issued of
            Right value -> pure value
            Left issueError -> expectationFailure ("JWT issuance failed: " <> show issueError) >> error "unreachable"
        basicVerification <-
          HarchWeb.verifyAuthenticationProof
            ( HarchWeb.jwtProofVerifier
                (Jwt.defaultJWTValidationSettings (const True))
                (HarchWeb.mkJwtAllowedAlgorithms (HarchWeb.JwtRs256 :| []))
                (JoseJwk.JWKSet [signingKey])
                (Right . show)
            )
            token
        basicVerification `shouldSatisfy` isAccepted
        let cookie =
              case HarchWeb.renderAuthenticationCookie (accountJwtCookie issuer) token of
                Just value -> value
                Nothing -> error "issued JWT was not renderable as a cookie"
            endpointRequest = protectedEndpointRequest cookie
            expectedPrincipal = mkAccountPrincipal (sessionPrincipal session) (sessionId session) (sessionExpiresAtNanoseconds session)
            activeStore =
              AccountSessionStore
                { saveAccountSession = \_ -> pure (Right True),
                  loadAccountSession = \receivedSessionId -> pure (Right (if receivedSessionId == sessionId session then Just session else Nothing)),
                  invalidateAccountSession = \_ _ -> pure (Right True)
                }
            revokedStore = activeStore {loadAccountSession = \_ -> pure (Right Nothing)}
            pipeline = accountJwtAuthenticationPipeline activeStore (pure 150) runtime
            revokedPipeline = accountJwtAuthenticationPipeline revokedStore (pure 150) runtime
        differentAccountId <- requiredAccountId "account_02"
        differentSessionId <- requiredSessionId "abcdefghijklmnopqrstuvwxyz0123456789-_"
        let differentAccountPrincipal = mkAccountPrincipal differentAccountId (sessionId session) (sessionExpiresAtNanoseconds session)
            differentSessionPrincipal = mkAccountPrincipal (sessionPrincipal session) differentSessionId (sessionExpiresAtNanoseconds session)
            differentExpiryPrincipal = mkAccountPrincipal (sessionPrincipal session) (sessionId session) (sessionExpiresAtNanoseconds session + 1)
        expectAll
          ( (expectedPrincipal == expectedPrincipal `shouldBe` True)
              :| [ expectedPrincipal /= differentAccountPrincipal `shouldBe` True,
                   expectedPrincipal /= differentSessionPrincipal `shouldBe` True,
                   expectedPrincipal /= differentExpiryPrincipal `shouldBe` True,
                   show [expectedPrincipal, differentAccountPrincipal] `shouldBe` "[AccountPrincipal <redacted>,AccountPrincipal <redacted>]"
                 ]
          )
        admitted <- HarchWeb.runAuthenticationPipeline pipeline endpointRequest
        revoked <- HarchWeb.runAuthenticationPipeline revokedPipeline endpointRequest
        case admitted of
          HarchWeb.ContinueEndpoint requestContext -> do
            requestAccountPrincipal requestContext `shouldBe` Just expectedPrincipal
            show expectedPrincipal `shouldBe` "AccountPrincipal <redacted>"
          HarchWeb.HaltEndpoint response -> expectationFailure ("expected established principal, got " <> show response)
        case revoked of
          HarchWeb.HaltEndpoint _ -> pure ()
          HarchWeb.ContinueEndpoint _ -> expectationFailure "revoked durable session must halt a protected endpoint"

    it "fails closed for missing and mismatched startup key material" $
      withSystemTempDirectory "web-api-account-jwt" $ \directory -> do
        let missingConfiguration = requiredConfiguration (directory </> "missing-private.jwk") (directory </> "missing-verification.jwks")
        missingResult <- loadAccountJwtRuntime missingConfiguration
        case missingResult of
          Left AccountJwtSigningJwkUnreadable -> pure ()
          _ -> expectationFailure "expected an unreadable signing-key startup failure"
        signingKey <- JoseJwk.genJWK (JwaJwk.RSAGenParam 1024)
        let namedSigningKey = signingKey & JoseJwk.jwkKid ?~ "different-key"
            signingFile = directory </> "private.jwk"
            verificationFile = directory </> "verification.jwks"
            configuration = requiredConfiguration signingFile verificationFile
        ByteString.writeFile signingFile (LazyByteString.toStrict (Aeson.encode namedSigningKey))
        expectLoadFailure (loadAccountJwtRuntime configuration) AccountJwtVerificationJwkSetUnreadable
        ByteString.writeFile signingFile (LazyByteString.toStrict (Aeson.encode namedSigningKey))
        ByteString.writeFile verificationFile (LazyByteString.toStrict (Aeson.encode (JoseJwk.JWKSet [namedSigningKey])))
        mismatchedResult <- loadAccountJwtRuntime configuration
        case mismatchedResult of
          Left AccountJwtSigningKeyIdMismatch -> pure ()
          _ -> expectationFailure "expected a signing-key id startup failure"

    it "keeps malformed signing and verification JWK files on explicit startup rails" $
      withSystemTempDirectory "web-api-account-jwt" $ \directory -> do
        signingKey <- JoseJwk.genJWK (JwaJwk.RSAGenParam 1024)
        let namedSigningKey = signingKey & JoseJwk.jwkKid ?~ "account-key-v1"
            signingFile = directory </> "private.jwk"
            verificationFile = directory </> "verification.jwks"
            configuration = requiredConfiguration signingFile verificationFile
        ByteString.writeFile signingFile "not-json"
        ByteString.writeFile verificationFile "not-json"
        expectLoadFailure (loadAccountJwtRuntime configuration) AccountJwtSigningJwkMalformed
        ByteString.writeFile signingFile (LazyByteString.toStrict (Aeson.encode namedSigningKey))
        expectLoadFailure (loadAccountJwtRuntime configuration) AccountJwtVerificationJwkSetMalformed
        ByteString.writeFile verificationFile (LazyByteString.toStrict (Aeson.encode (JoseJwk.JWKSet [])))
        expectLoadFailure (loadAccountJwtRuntime configuration) AccountJwtVerificationKeyMissing

    it "rejects non-RSA signing and verification key material at startup" $
      withSystemTempDirectory "web-api-account-jwt" $ \directory -> do
        signingKey <- JoseJwk.genJWK (JwaJwk.RSAGenParam 1024)
        nonRsaKey <- JoseJwk.genJWK (JwaJwk.OctGenParam 64)
        let namedSigningKey = signingKey & JoseJwk.jwkKid ?~ "account-key-v1"
            namedNonRsaKey = nonRsaKey & JoseJwk.jwkKid ?~ "account-key-v1"
            signingFile = directory </> "private.jwk"
            verificationFile = directory </> "verification.jwks"
            configuration = requiredConfiguration signingFile verificationFile
        ByteString.writeFile signingFile (LazyByteString.toStrict (Aeson.encode namedNonRsaKey))
        ByteString.writeFile verificationFile (LazyByteString.toStrict (Aeson.encode (JoseJwk.JWKSet [namedSigningKey])))
        expectLoadFailure (loadAccountJwtRuntime configuration) AccountJwtSigningKeyNotRsaPrivate
        ByteString.writeFile signingFile (LazyByteString.toStrict (Aeson.encode namedSigningKey))
        ByteString.writeFile verificationFile (LazyByteString.toStrict (Aeson.encode (JoseJwk.JWKSet [namedNonRsaKey])))
        expectLoadFailure (loadAccountJwtRuntime configuration) AccountJwtVerificationKeyNotRsa

    it "rejects an expired, mismatched, unavailable, or absent durable account session" $
      withTestRuntime $ \runtime _ session -> do
        let issuer = accountJwtIssuerFromRuntime runtime
            activeStore =
              AccountSessionStore
                { saveAccountSession = \_ -> pure (Right True),
                  loadAccountSession = const (pure (Right (Just session))),
                  invalidateAccountSession = \_ _ -> pure (Right True)
                }
            accountId = sessionPrincipal session
            mismatchedSession = session {sessionPrincipal = requiredAccountIdPure "account_02"}
            expiredSession = session {sessionExpiresAtNanoseconds = 100}
            stores =
              [ activeStore {loadAccountSession = const (pure (Right (Just mismatchedSession)))},
                activeStore {loadAccountSession = const (pure (Right (Just expiredSession)))},
                activeStore {loadAccountSession = const (pure (Right Nothing))}
              ]
            unavailableStore = activeStore {loadAccountSession = const (pure (Left AccountSessionStoreUnavailable))}
        issued <- issueAccountSessionJwt issuer session
        token <-
          case issued of
            Left issueError -> expectationFailure ("JWT issuance failed: " <> show issueError) >> error "unreachable"
            Right value -> pure value
        cookie <-
          case HarchWeb.renderAuthenticationCookie (accountJwtCookie issuer) token of
            Nothing -> error "issued JWT was not renderable as a cookie"
            Just value -> pure value
        mapM_ (\store -> HarchWeb.runAuthenticationPipeline (accountJwtAuthenticationPipeline store (pure 150) runtime) (protectedEndpointRequest cookie) >>= expectHalted) stores
        HarchWeb.runAuthenticationPipeline (accountJwtAuthenticationPipeline unavailableStore (pure 150) runtime) (protectedEndpointRequest cookie) >>= expectUnavailable
        HarchWeb.runAuthenticationPipeline (accountJwtAuthenticationPipeline activeStore (pure 150) runtime) (protectedEndpointRequest "") >>= expectLoginRedirect
        accountId `shouldBe` requiredAccountIdPure "account_01"

    it "keeps durable-session rejection and unavailability classifications at the principal boundary" $
      withTestRuntime $ \runtime _ session -> do
        let issuer = accountJwtIssuerFromRuntime runtime
            activeStore =
              AccountSessionStore
                { saveAccountSession = \_ -> pure (Right True),
                  loadAccountSession = const (pure (Right (Just session))),
                  invalidateAccountSession = \_ _ -> pure (Right True)
                }
            mismatchedSession = session {sessionPrincipal = requiredAccountIdPure "account_02"}
            expiredSession = session {sessionExpiresAtNanoseconds = 100}
            mismatchedStore = activeStore {loadAccountSession = const (pure (Right (Just mismatchedSession)))}
            expiredStore = activeStore {loadAccountSession = const (pure (Right (Just expiredSession)))}
            rejectedStore = activeStore {loadAccountSession = const (pure (Right Nothing))}
            unavailableStore = activeStore {loadAccountSession = const (pure (Left AccountSessionStoreUnavailable))}
        issued <- issueAccountSessionJwt issuer session
        token <-
          case issued of
            Left issueError -> expectationFailure ("JWT issuance failed: " <> show issueError) >> error "unreachable"
            Right value -> pure value
        mismatched <- establishIssuedPrincipal (accountJwtAuthenticationPipeline mismatchedStore (pure 150) runtime) token
        expired <- establishIssuedPrincipal (accountJwtAuthenticationPipeline expiredStore (pure 150) runtime) token
        missing <- establishIssuedPrincipal (accountJwtAuthenticationPipeline rejectedStore (pure 150) runtime) token
        unavailable <- establishIssuedPrincipal (accountJwtAuthenticationPipeline unavailableStore (pure 150) runtime) token
        expectAll
          ( (show mismatched `shouldContain` "account.jwt.session-rejected")
              :| [ show expired `shouldContain` "account.jwt.session-rejected",
                   show missing `shouldContain` "account.jwt.session-rejected",
                   show unavailable `shouldContain` "account.jwt.session-unavailable"
                 ]
          )

    it "rejects signed JWTs whose application account or session claims are absent or invalid" $
      withTestRuntime $ \runtime signingKey session -> do
        let issuer = accountJwtIssuerFromRuntime runtime
            activeStore =
              AccountSessionStore
                { saveAccountSession = \_ -> pure (Right True),
                  loadAccountSession = const (pure (Right (Just session))),
                  invalidateAccountSession = \_ _ -> pure (Right True)
                }
            pipeline = accountJwtAuthenticationPipeline activeStore (pure 150) runtime
            issueAndRender claims = do
              issued <- HarchWeb.issueJwt signingKey accountJwtHeader claims
              case issued of
                Left _ -> expectationFailure "expected test JWT to be issued" >> error "unreachable"
                Right token ->
                  case HarchWeb.renderAuthenticationCookie (accountJwtCookie issuer) token of
                    Nothing -> expectationFailure "expected test JWT to render as a cookie" >> error "unreachable"
                    Just cookie -> pure (token, cookie)
        missingSubject <- issueAndRender (claimsForTestSession session Nothing (Just (sessionIdText (sessionId session))))
        uriSubject <- issueAndRender (claimsForTestSession session (Just "https://accounts.example.test/principals/one") (Just (sessionIdText (sessionId session))))
        invalidAccount <- issueAndRender (claimsForTestSession session (Just "not an account id") (Just (sessionIdText (sessionId session))))
        missingSession <- issueAndRender (claimsForTestSession session (Just "account_01") Nothing)
        invalidSession <- issueAndRender (claimsForTestSession session (Just "account_01") (Just "short"))
        let issuedTokens = [missingSubject, uriSubject, invalidAccount, missingSession, invalidSession]
            HarchWeb.AuthenticationProofVerifier verifier = HarchWeb.authenticationProofVerifier pipeline
        verificationResults <- traverse (verifier . fst) issuedTokens
        mapM_ expectClaimsRejection verificationResults
        results <- traverse (HarchWeb.runAuthenticationPipeline pipeline . protectedEndpointRequest . snd) issuedTokens
        mapM_ expectLoginRedirect results

    it "keeps the unavailable workflow issuer safe and redacted" $ do
      let clearedCookie = HarchWeb.clearAuthenticationCookie (accountJwtCookie unavailableAccountJwtIssuer)
      clearedCookie `shouldSatisfy` Text.isInfixOf "__Host-harch-session="
      unavailableResult <- issueAccountSessionJwt unavailableAccountJwtIssuer (OpaqueSession (requiredSessionIdPure "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_") (requiredAccountIdPure "account_01") 1 2)
      case unavailableResult of
        Left AccountJwtIssueFailed -> pure ()
        Right _ -> expectationFailure "unavailable issuer must not mint an account JWT"

withTestRuntime :: (AccountJwtRuntime -> HarchWeb.JWK -> OpaqueSession Account.AccountId -> IO value) -> IO value
withTestRuntime action =
  withSystemTempDirectory "web-api-account-jwt" $ \directory -> do
    signingKey <- JoseJwk.genJWK (JwaJwk.RSAGenParam 1024)
    accountId <- requiredAccountId "account_01"
    sessionIdValue <- requiredSessionId "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_"
    let namedSigningKey = signingKey & JoseJwk.jwkKid ?~ "account-key-v1"
        signingFile = directory </> "private.jwk"
        verificationFile = directory </> "verification.jwks"
        configuration = requiredConfiguration signingFile verificationFile
        -- JOSE validates @exp@ against wall time; the durable-store test clock
        -- remains 150, while this intentionally distant expiry keeps the
        -- cryptographic proof valid independently of when the suite runs.
        session = OpaqueSession sessionIdValue accountId 100 4102444800000000000
    ByteString.writeFile signingFile (LazyByteString.toStrict (Aeson.encode namedSigningKey))
    ByteString.writeFile verificationFile (LazyByteString.toStrict (Aeson.encode (JoseJwk.JWKSet [namedSigningKey])))
    loaded <- loadAccountJwtRuntime configuration
    case loaded of
      Left loadError -> expectationFailure ("JWT runtime load failed: " <> show loadError) >> error "unreachable"
      Right runtime -> action runtime namedSigningKey session

isAccepted :: Either HarchWeb.ProofVerificationFailure value -> Bool
isAccepted result =
  case result of
    Right _ -> True
    Left _ -> False

establishIssuedPrincipal :: HarchWeb.AuthenticationPipeline AppRoute AppRequestContext () HarchWeb.EncodedJwt verified principal denial -> HarchWeb.EncodedJwt -> IO (Either HarchWeb.PrincipalEstablishmentFailure principal)
establishIssuedPrincipal pipeline token = do
  let HarchWeb.AuthenticationProofVerifier verifyProof = HarchWeb.authenticationProofVerifier pipeline
      HarchWeb.PrincipalEstablisher establishPrincipal = HarchWeb.authenticationPrincipalEstablisher pipeline
  verification <- verifyProof token
  case verification of
    Left failure -> expectationFailure ("expected a valid JWT proof: " <> show failure) >> error "unreachable"
    Right claims -> establishPrincipal claims

isRight :: Either failure value -> Bool
isRight result =
  case result of
    Left _ -> False
    Right _ -> True

expectHalted :: HarchWeb.EndpointGuardResult AppRoute AppRequestContext -> Expectation
expectHalted result =
  case result of
    HarchWeb.HaltEndpoint response -> show response `shouldSatisfy` (not . null)
    HarchWeb.ContinueEndpoint _ -> expectationFailure "expected authentication to halt"

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
    HarchWeb.HaltEndpoint response -> expectationFailure ("expected login redirect, got " <> show response)
    HarchWeb.ContinueEndpoint _ -> expectationFailure "expected authentication to halt"

expectUnavailable :: HarchWeb.EndpointGuardResult AppRoute AppRequestContext -> Expectation
expectUnavailable result =
  case result of
    HarchWeb.HaltEndpoint (HarchWeb.NonPageBodyResponse responseBody) ->
      expectAll
        ( (HarchWeb.responseStatus responseBody `shouldBe` Http.status503)
            :| [ HarchWeb.responseContentType responseBody `shouldBe` "text/plain; charset=utf-8",
                 HarchWeb.responseBody responseBody `shouldBe` "Authentication is temporarily unavailable.",
                 HarchWeb.responseObservabilityAttributes responseBody `shouldBe` [],
                 HarchWeb.responseLogEntries responseBody `shouldBe` [],
                 HarchWeb.responseDatabaseOperations responseBody `shouldBe` []
               ]
        )
    HarchWeb.HaltEndpoint response -> expectationFailure ("expected unavailable response, got " <> show response)
    HarchWeb.ContinueEndpoint _ -> expectationFailure "expected authentication to halt"

expectClaimsRejection :: Either HarchWeb.ProofVerificationFailure value -> Expectation
expectClaimsRejection result =
  case result of
    Left (HarchWeb.ProofRejected rejection) -> show rejection `shouldContain` "account.jwt.claims-rejected"
    Left failure -> expectationFailure ("expected claim rejection, got " <> show failure)
    Right _ -> expectationFailure "expected invalid application claims to reject the JWT"

expectLoadFailure :: IO (Either AccountJwtLoadError value) -> AccountJwtLoadError -> Expectation
expectLoadFailure load expected = do
  result <- load
  case result of
    Left actual -> actual `shouldBe` expected
    Right _ -> expectationFailure "expected account JWT runtime startup to fail"

requiredConfiguration :: FilePath -> FilePath -> AccountJwtConfiguration
requiredConfiguration signingFile verificationFile =
  case mkAccountJwtConfiguration "https://accounts.example.test" "web-api-account" "account-key-v1" signingFile verificationFile "__Host-harch-session" 28800 of
    Right configuration -> configuration
    Left configurationError -> error ("test JWT configuration is invalid: " <> show configurationError)

protectedEndpointRequest :: Text.Text -> HarchWeb.EndpointRequest AppRoute AppRequestContext ()
protectedEndpointRequest cookie =
  HarchWeb.EndpointRequest
    { HarchWeb.endpointWaiRequest = (waiRequest ["profile"]) {Wai.requestHeaders = [("Cookie", TextEncoding.encodeUtf8 cookie)]},
      HarchWeb.endpointRouteRequest = HarchWeb.RouteRequest ProfileRoute defaultRequestContext,
      HarchWeb.endpointMetadata =
        HarchWeb.mkEndpointMetadata
          (HarchWeb.requiredEndpointNameOrDie "account.profile")
          (HarchWeb.requiredRouteTemplateOrDie "/profile")
          HarchWeb.HtmlEndpoint
          HarchWeb.RequireAuthenticated,
      HarchWeb.endpointSecurityEventSink = Nothing,
      HarchWeb.endpointDispatchKind = HarchWeb.EndpointMatched
    }

requiredAccountId :: Text.Text -> IO Account.AccountId
requiredAccountId value =
  pure (requiredAccountIdPure value)

requiredAccountIdPure :: Text.Text -> Account.AccountId
requiredAccountIdPure value =
  case Account.mkAccountId value of
    Just accountId -> accountId
    Nothing -> error "test account identifier is invalid"

requiredSessionId :: Text.Text -> IO SessionId
requiredSessionId value =
  pure (requiredSessionIdPure value)

requiredSessionIdPure :: Text.Text -> SessionId
requiredSessionIdPure value =
  case mkSessionId value of
    Just sessionIdValue -> sessionIdValue
    Nothing -> error "test session identifier is invalid"

accountJwtHeader :: HarchWeb.JWSHeader HarchWeb.RequiredProtection
accountJwtHeader =
  JoseJws.newJWSHeaderProtected JwaJws.RS256
    & JoseJws.kid ?~ HeaderParam RequiredProtection "account-key-v1"

claimsForTestSession :: OpaqueSession Account.AccountId -> Maybe Text.Text -> Maybe Text.Text -> Jwt.ClaimsSet
claimsForTestSession session maybeSubject maybeSessionId =
  withOptionalSubject maybeSubject (withOptionalSessionId maybeSessionId baseClaims)
  where
    baseClaims =
      Jwt.emptyClaimsSet
        & Jwt.claimIss ?~ requiredStringOrUri "https://accounts.example.test"
        & Jwt.claimAud ?~ Jwt.Audience [requiredStringOrUri "web-api-account"]
        & Jwt.claimIat ?~ numericDateForTest (sessionIssuedAtNanoseconds session)
        & Jwt.claimNbf ?~ numericDateForTest (sessionIssuedAtNanoseconds session)
        & Jwt.claimExp ?~ numericDateForTest (sessionExpiresAtNanoseconds session)

withOptionalSubject :: Maybe Text.Text -> Jwt.ClaimsSet -> Jwt.ClaimsSet
withOptionalSubject maybeSubject claims =
  case maybeSubject of
    Nothing -> claims
    Just subject -> claims & Jwt.claimSub ?~ requiredStringOrUri subject

withOptionalSessionId :: Maybe Text.Text -> Jwt.ClaimsSet -> Jwt.ClaimsSet
withOptionalSessionId maybeSessionId claims =
  case maybeSessionId of
    Nothing -> claims
    Just sessionIdValue -> claims & Jwt.claimJti ?~ sessionIdValue

requiredStringOrUri :: Text.Text -> Jwt.StringOrURI
requiredStringOrUri value =
  case matching Jwt.stringOrUri (Text.unpack value) of
    Right parsedValue -> parsedValue
    Left _ -> error "test JWT string or URI is invalid"

numericDateForTest :: UnixTimeNanoseconds -> Jwt.NumericDate
numericDateForTest instant =
  Jwt.NumericDate (posixSecondsToUTCTime (fromIntegral (unixTimeNanosecondsValue instant) / 1000000000))
