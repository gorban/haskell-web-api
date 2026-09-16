{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Exception (ErrorCall (..), evaluate)
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64 qualified as Base64
import Data.Either (fromRight)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb
import HarchWeb.Api (ApiRequestData (..), ApiRequestDecodeResult (..), ApiRequestParseError (..), ApiRequestSource (..), apiHeaderName, runRequestCodec)
import HarchWeb.Password (defaultPasswordHashingPolicy, hashPasswordWithSalt, mkPassword, verifyPassword)
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import Unit.HarchWeb.TestSupport (TestContext (requestLanguage), TestRoute (DataRoute), defaultContext)

spec = do
  describe "durable API-client storage" $
    it "keeps discovery and current-principal establishment application supplied" $ do
      dependency <- newIORef (mkAuthenticationDependency (requiredSecurityFailureCodeOrDie "api-client.store-unavailable")) >>= readIORef
      let store :: ApiClientStore Text Text
          store =
            ApiClientStore
              { findApiClient = \clientId -> pure $ Right $ if clientId == ("known" :: Text) then Just "configured-client" else Nothing,
                establishApiClient = \clientId -> pure $ Right $ if clientId == ("current" :: Text) then Just "current-client" else Nothing
              }
          unavailable = ApiClientStoreUnavailable dependency
      knownClient <- findApiClient store "known"
      unknownClient <- findApiClient store "unknown"
      currentClient <- establishApiClient store "current"
      unavailableClient <- establishApiClient store "disabled"
      expectAll
        ( (knownClient `shouldBe` Right (Just "configured-client"))
            :| [ unknownClient `shouldBe` Right Nothing,
                 currentClient `shouldBe` Right (Just "current-client"),
                 unavailableClient `shouldBe` Right Nothing,
                 unavailable `shouldBe` unavailable,
                 (unavailable /= unavailable) `shouldBe` False,
                 show unavailable `shouldBe` "ApiClientStoreUnavailable (AuthenticationDependency (SecurityFailureCode \"api-client.store-unavailable\"))",
                 showList [unavailable] "" `shouldSatisfy` (not . null)
               ]
        )

  describe "authentication-flow vocabulary" $
    it "keeps browser, OAuth grant, and client-method policy as separate typed axes" $ do
      browserConfiguration <- newIORef BrowserSessionFlowConfiguration >>= readIORef
      clientMethod <- newIORef ClientSecretBasic >>= readIORef
      let oauthConfiguration = OAuth2FlowConfiguration (ClientCredentialsGrant clientMethod :| [])
          browserFlow :: InteractiveAuthenticationFlow Text
          browserFlow = BrowserSessionFlow browserConfiguration
          oauthFlow :: InteractiveAuthenticationFlow Text
          oauthFlow = OAuth2Flow oauthConfiguration
          customFlow :: InteractiveAuthenticationFlow Text
          customFlow = CustomAuthenticationFlow "deployment-defined"
      expectAll
        ( (browserFlow `shouldBe` browserFlow)
            :| [ oauthFlow `shouldBe` oauthFlow,
                 customFlow `shouldBe` customFlow,
                 oauth2FlowGrants oauthConfiguration `shouldBe` (ClientCredentialsGrant clientMethod :| []),
                 browserFlow `shouldNotBe` oauthFlow,
                 browserConfiguration `shouldBe` browserConfiguration,
                 oauthConfiguration `shouldBe` OAuth2FlowConfiguration (ClientCredentialsGrant clientMethod :| []),
                 ClientCredentialsGrant clientMethod `shouldBe` ClientCredentialsGrant clientMethod,
                 clientMethod `shouldBe` clientMethod,
                 (browserConfiguration /= browserConfiguration) `shouldBe` False,
                 (oauthConfiguration /= oauthConfiguration) `shouldBe` False,
                 (ClientCredentialsGrant clientMethod /= ClientCredentialsGrant clientMethod) `shouldBe` False,
                 (clientMethod /= clientMethod) `shouldBe` False,
                 show browserFlow `shouldBe` "BrowserSessionFlow BrowserSessionFlowConfiguration",
                 show oauthFlow
                   `shouldBe` "OAuth2Flow (OAuth2FlowConfiguration {oauth2FlowGrants = ClientCredentialsGrant ClientSecretBasic :| []})",
                 show customFlow `shouldBe` "CustomAuthenticationFlow \"deployment-defined\"",
                 show BrowserSessionFlowConfiguration `shouldBe` "BrowserSessionFlowConfiguration",
                 show oauthConfiguration
                   `shouldBe` "OAuth2FlowConfiguration {oauth2FlowGrants = ClientCredentialsGrant ClientSecretBasic :| []}",
                 show (ClientCredentialsGrant ClientSecretBasic) `shouldBe` "ClientCredentialsGrant ClientSecretBasic",
                 show ClientSecretBasic `shouldBe` "ClientSecretBasic",
                 showList [browserFlow, oauthFlow, customFlow] "" `shouldSatisfy` (not . null),
                 showList [browserConfiguration] "" `shouldSatisfy` (not . null),
                 showList [oauthConfiguration] "" `shouldSatisfy` (not . null),
                 showList [ClientCredentialsGrant clientMethod] "" `shouldSatisfy` (not . null),
                 showList [clientMethod] "" `shouldSatisfy` (not . null)
               ]
        )

  describe "OAuth scope declarations" $ do
    it "accepts the complete RFC 6749 scope-token character range" $ do
      dynamicScope <- newIORef "api/read:second~v1" >>= readIORef
      let accepted = mkOAuth2Scope dynamicScope
          scopeError = either Just (const Nothing)
      expectAll
        ( (oauth2ScopeText <$> accepted `shouldBe` Right dynamicScope)
            :| [ oauth2ScopeText <$> mkOAuth2Scope "!#[].~" `shouldBe` Right "!#[].~",
                 scopeError (mkOAuth2Scope "") `shouldBe` Just OAuth2ScopeEmpty,
                 scopeError (mkOAuth2Scope "contains space") `shouldBe` Just OAuth2ScopeInvalidCharacter,
                 scopeError (mkOAuth2Scope "quote\"") `shouldBe` Just OAuth2ScopeInvalidCharacter,
                 scopeError (mkOAuth2Scope "backslash\\") `shouldBe` Just OAuth2ScopeInvalidCharacter,
                 scopeError (mkOAuth2Scope "line\nbreak") `shouldBe` Just OAuth2ScopeInvalidCharacter,
                 scopeError (mkOAuth2Scope "non-ascii-\233") `shouldBe` Just OAuth2ScopeInvalidCharacter,
                 OAuth2ScopeEmpty `shouldBe` OAuth2ScopeEmpty,
                 (OAuth2ScopeEmpty /= OAuth2ScopeInvalidCharacter) `shouldBe` True,
                 show <$> mkOAuth2Scope "api/read" `shouldBe` Right "OAuth2Scope {oauth2ScopeText = \"api/read\"}",
                 show OAuth2ScopeEmpty `shouldBe` "OAuth2ScopeEmpty",
                 show OAuth2ScopeInvalidCharacter `shouldBe` "OAuth2ScopeInvalidCharacter",
                 (showList <$> traverse mkOAuth2Scope ["api/read"] <*> pure "") `shouldSatisfy` either (const False) (not . null),
                 showList [OAuth2ScopeEmpty, OAuth2ScopeInvalidCharacter] "" `shouldSatisfy` (not . null)
               ]
        )

  describe "OAuth client-credentials form decoding" $
    it "uses the bounded API form boundary without normalizing ambiguous scope requests" $ do
      let decode fields = runRequestCodec oauth2ClientCredentialsRequestCodec (ApiRequestData [] [] [] fields)
          scopeTexts result =
            case result of
              ApiRequestDecoded request -> oauth2RequestedScopeTexts (oauth2ClientCredentialsScopes request)
              ApiRequestRejected _ -> []
              ApiRequestCodecInvalid -> []
          usesDefaultScopes result =
            case result of
              ApiRequestDecoded request ->
                case oauth2ClientCredentialsScopes request of
                  UseClientDefaultScopes -> True
                  RequestOAuth2Scopes _ -> False
              ApiRequestRejected _ -> False
              ApiRequestCodecInvalid -> False
          clientCredentialsGrant result =
            case result of
              ApiRequestDecoded request -> oauth2ClientCredentialsGrant request == ClientCredentialsGrant ClientSecretBasic
              ApiRequestRejected _ -> False
              ApiRequestCodecInvalid -> False
          requestErrors result =
            case result of
              ApiRequestDecoded _ -> Nothing
              ApiRequestRejected errors -> Just errors
              ApiRequestCodecInvalid -> Nothing
          requiredGrant = [("grant_type", "client_credentials")]
      expectAll
        ( (usesDefaultScopes (decode requiredGrant) `shouldBe` True)
            :| [ scopeTexts (decode requiredGrant) `shouldBe` [],
                 clientCredentialsGrant (decode requiredGrant) `shouldBe` True,
                 scopeTexts (decode [("grant_type", "client_credentials"), ("scope", "api/read profile:read")]) `shouldBe` ["api/read", "profile:read"],
                 requestErrors (decode []) `shouldBe` Just (MissingApiField ApiFormSource "grant_type" :| []),
                 requestErrors (decode [("grant_type", "client_credentials"), ("grant_type", "client_credentials")])
                   `shouldBe` Just (DuplicateApiField ApiFormSource "grant_type" :| []),
                 requestErrors (decode [("grant_type", "refresh_token")])
                   `shouldBe` Just (InvalidApiField ApiFormSource "grant_type" :| []),
                 requestErrors (decode [("grant_type", "client_credentials"), ("scope", "")])
                   `shouldBe` Just (InvalidApiField ApiFormSource "scope" :| []),
                 requestErrors (decode [("grant_type", "client_credentials"), ("scope", "api/read  profile:read")])
                   `shouldBe` Just (InvalidApiField ApiFormSource "scope" :| []),
                 requestErrors (decode [("grant_type", "client_credentials"), ("scope", " api/read")])
                   `shouldBe` Just (InvalidApiField ApiFormSource "scope" :| []),
                 requestErrors (decode [("grant_type", "client_credentials"), ("scope", "api/read ")])
                   `shouldBe` Just (InvalidApiField ApiFormSource "scope" :| []),
                 requestErrors (decode [("grant_type", "client_credentials"), ("scope", "api/read"), ("scope", "profile:read")])
                   `shouldBe` Just (DuplicateApiField ApiFormSource "scope" :| []),
                 requestErrors (decode [("grant_type", "client_credentials"), ("scope", "api/read\\scope")])
                   `shouldBe` Just (InvalidApiField ApiFormSource "scope" :| [])
               ]
        )

  describe "OAuth client-secret Basic decoding" $
    it "uses the bounded API header boundary and keeps credentials opaque" $ do
      configuredMaximumBytes <- newIORef (128 :: Int) >>= readIORef
      let maximumBytes = requiredOAuth2ClientCredentialsMaximumBytesOrDie configuredMaximumBytes
          decode headers = runRequestCodec (oauth2ClientSecretBasicCodec maximumBytes) (ApiRequestData [] headers [] [])
          clientId result =
            case result of
              ApiRequestDecoded credentials -> Just (oauth2ClientIdText (oauth2ClientCredentialsId credentials))
              ApiRequestRejected _ -> Nothing
              ApiRequestCodecInvalid -> Nothing
          secretMatches expected result =
            case result of
              ApiRequestDecoded credentials ->
                case hashPasswordWithSalt defaultPasswordHashingPolicy "oauth-client-secret" (mkPassword expected) of
                  Just expectedHash -> verifyPassword (oauth2ClientCredentialsSecret credentials) expectedHash
                  Nothing -> False
              ApiRequestRejected _ -> False
              ApiRequestCodecInvalid -> False
          requestErrors result =
            case result of
              ApiRequestDecoded _ -> Nothing
              ApiRequestRejected errors -> Just errors
              ApiRequestCodecInvalid -> Nothing
          authorization = fromMaybe (error "expected Authorization header") (apiHeaderName "Authorization")
          basic encodedCredentials = "Basic " <> TextEncoding.decodeUtf8 (Base64.encode encodedCredentials)
          lowercaseBasic encodedCredentials = "basic " <> TextEncoding.decodeUtf8 (Base64.encode encodedCredentials)
          maximumBytesError result =
            case result of
              Left message -> message == "OAuth client-credentials maximum bytes must be positive"
              Right _ -> False
          validHeader = [(authorization, basic "demo-client:demo-secret")]
      expectAll
        ( (clientId (decode validHeader) `shouldBe` Just "demo-client")
            :| [ secretMatches "demo-secret" (decode validHeader) `shouldBe` True,
                 clientId (decode [(authorization, lowercaseBasic "demo-client:demo-secret")]) `shouldBe` Just "demo-client",
                 clientId (decode [(authorization, basic "demo%2Dclient:demo+secret")]) `shouldBe` Just "demo-client",
                 secretMatches "demo secret" (decode [(authorization, basic "demo%2Dclient:demo+secret")]) `shouldBe` True,
                 requestErrors (decode []) `shouldBe` Just (MissingApiField ApiHeaderSource "authorization" :| []),
                 requestErrors (decode [(authorization, basic "demo-client:demo-secret"), (authorization, basic "another-client:another-secret")]) `shouldBe` Just (DuplicateApiField ApiHeaderSource "authorization" :| []),
                 requestErrors (decode [(authorization, "Bearer token")]) `shouldBe` Just (InvalidApiField ApiHeaderSource "authorization" :| []),
                 requestErrors (decode [(authorization, "Basic not-base64!")]) `shouldBe` Just (InvalidApiField ApiHeaderSource "authorization" :| []),
                 requestErrors (decode [(authorization, basic "demo%ZZclient:demo-secret")]) `shouldBe` Just (InvalidApiField ApiHeaderSource "authorization" :| []),
                 requestErrors (decode [(authorization, basic (ByteString.pack [255, 58, 115, 101, 99, 114, 101, 116]))]) `shouldBe` Just (InvalidApiField ApiHeaderSource "authorization" :| []),
                 requestErrors (decode [(authorization, basic "missing-secret:")]) `shouldBe` Just (InvalidApiField ApiHeaderSource "authorization" :| []),
                 requestErrors (decode [(authorization, basic "missing-client")]) `shouldBe` Just (InvalidApiField ApiHeaderSource "authorization" :| []),
                 requestErrors (runRequestCodec (oauth2ClientSecretBasicCodec (requiredOAuth2ClientCredentialsMaximumBytesOrDie 8)) (ApiRequestData [] validHeader [] [])) `shouldBe` Just (InvalidApiField ApiHeaderSource "authorization" :| []),
                 maximumBytesError (mkOAuth2ClientCredentialsMaximumBytes 0) `shouldBe` True
               ]
        )
      evaluate (requiredOAuth2ClientCredentialsMaximumBytesOrDie 0 `seq` ())
        `shouldThrow` \case
          ErrorCall message -> "invalid OAuth client-credentials maximum-byte declaration" `Text.isInfixOf` Text.pack message

  describe "authentication proof extractors" $ do
    it "distinguishes absent, duplicate, malformed, and oversized cookie proofs" $ do
      let extractor = cookieJwtExtractor (requiredCookieName "__Host-session") (requiredProofMaximumBytes 8)
          punctuationExtractor = cookieJwtExtractor (requiredCookieName "__Host-session!#$%&'*+-.^_`|~") (requiredProofMaximumBytes 8)
          extract headers = fmap (fmap encodedJwtBytes) (extractAuthenticationProof extractor (endpointRequest AllowUnauthenticated headers))
          extractPunctuation headers = fmap (fmap encodedJwtBytes) (extractAuthenticationProof punctuationExtractor (endpointRequest AllowUnauthenticated headers))
      expectAll
        ( (extract [] `shouldBe` Right Nothing)
            :| [ extract [("Cookie", "__Host-session=good")] `shouldBe` Right (Just "good"),
                 extractPunctuation [("Cookie", "__Host-session!#$%&'*+-.^_`|~=good")] `shouldBe` Right (Just "good"),
                 extract [("Cookie", "__Host-session=one; __Host-session=two")] `shouldBe` Left ProofAmbiguous,
                 extract [("Cookie", "__Host-session=toolong-token")] `shouldBe` Left ProofTooLarge,
                 extract [("Cookie", "other=value")] `shouldBe` Right Nothing,
                 extract [("X-Ignored", "__Host-session=good")] `shouldBe` Right Nothing,
                 extract [("Cookie", "__Host-session=")] `shouldBe` Left ProofMalformed,
                 extract [("Cookie", "__Host-session")] `shouldBe` Left ProofMalformed
               ]
        )

    it "treats a present authorization header as bearer-only input" $ do
      let extractor = bearerJwtExtractor (requiredProofMaximumBytes 8)
          extract headers = fmap (fmap encodedJwtBytes) (extractAuthenticationProof extractor (endpointRequest AllowUnauthenticated headers))
      unrelatedHeaderName <- newIORef "X-Unrelated" >>= readIORef
      expectAll
        ( (extract [] `shouldBe` Right Nothing)
            :| [ extract [("Authorization", "Bearer valid")] `shouldBe` Right (Just "valid"),
                 extract [("Authorization", "Basic valid")] `shouldBe` Left ProofMalformed,
                 extract [("Authorization", "Bearer ")] `shouldBe` Left ProofMalformed,
                 extract [("Authorization", "Bearer has space")] `shouldBe` Left ProofMalformed,
                 extract [("Authorization", "Bearer one"), ("Authorization", "Bearer two")] `shouldBe` Left ProofAmbiguous,
                 extract [("Authorization", "Bearer over-eight")] `shouldBe` Left ProofTooLarge,
                 extract [(unrelatedHeaderName, "Bearer valid")] `shouldBe` Right Nothing
               ]
        )

    it "extracts JWT cookie-or-bearer proofs without an ambient fallback" $ do
      let extractor = cookieOrBearerJwtExtractor (requiredCookieName "__Host-session") (requiredProofMaximumBytes 8)
          extract headers = extractAuthenticationProof extractor (endpointRequest AllowUnauthenticated headers)
          extractSource = fmap (fmap jwtProofSource) . extract
          extractToken = fmap (fmap (encodedJwtBytes . jwtProofEncodedJwt)) . extract
      expectAll
        ( (extractSource [] `shouldBe` Right Nothing)
            :| [ extractSource [("Cookie", "__Host-session=cookie")] `shouldBe` Right (Just JwtProofFromCookie),
                 extractSource [("Authorization", "Bearer bearer")] `shouldBe` Right (Just JwtProofFromBearer),
                 extractSource [("Cookie", "__Host-session=same"), ("Authorization", "Bearer same")]
                   `shouldBe` Right (Just JwtProofFromCookieAndBearer),
                 extract [("Cookie", "__Host-session=cookie"), ("Authorization", "Bearer bearer")]
                   `shouldBe` Left ProofConflicting,
                 extract [("Cookie", "__Host-session=cookie"), ("Authorization", "Basic bearer")]
                   `shouldBe` Left ProofMalformed,
                 extract [("Cookie", "__Host-session=cookie; __Host-session=other")]
                   `shouldBe` Left ProofAmbiguous,
                 extract [("Authorization", "Bearer one"), ("Authorization", "Bearer two")]
                   `shouldBe` Left ProofAmbiguous,
                 extract [("Cookie", "__Host-session=over-eight")]
                   `shouldBe` Left ProofTooLarge,
                 fmap (fmap show) (extract [("Authorization", "Bearer secret")]) `shouldBe` Right (Just "JwtProof JwtProofFromBearer <redacted>"),
                 extractToken [("Authorization", "Bearer bearer")] `shouldBe` Right (Just "bearer")
               ]
        )

    it "does not silently choose between multiple configured proof sources" $ do
      let present :: AuthenticationProofExtractor TestRoute TestContext (ScopeRequirement Text) Text
          present =
            AuthenticationProofExtractor $ \receivedRequest ->
              if endpointProtocol (endpointMetadata receivedRequest) == HtmlEndpoint
                then Right (Just "first")
                else Left ProofMalformed
          absent :: AuthenticationProofExtractor TestRoute TestContext (ScopeRequirement Text) Text
          absent = AuthenticationProofExtractor (const (Right Nothing))
          malformed :: AuthenticationProofExtractor TestRoute TestContext (ScopeRequirement Text) Text
          malformed = AuthenticationProofExtractor (const (Left ProofMalformed))
          combined = combineProofExtractors (present :| [absent])
          ambiguous = combineProofExtractors (present :| [AuthenticationProofExtractor (const (Right (Just "second")))])
      request <- newIORef (endpointRequest AllowUnauthenticated []) >>= readIORef
      let extract extractor = extractAuthenticationProof extractor request
      expectAll
        ( (extract combined `shouldBe` Right (Just "first"))
            :| [ extract ambiguous `shouldBe` Left ProofAmbiguous,
                 extract (combineProofExtractors (malformed :| [absent])) `shouldBe` Left ProofMalformed,
                 extract (combineProofExtractors (absent :| [absent])) `shouldBe` Right Nothing
               ]
        )

    it "rejects invalid bounded configuration values" $
      expectAll
        ( (mkAuthenticationCookieName "" `shouldBe` Left "authentication cookie name cannot be empty")
            :| [ mkAuthenticationCookieName "bad;name" `shouldBe` Left "authentication cookie name has invalid characters",
                 mkAuthenticationCookieName "bad:name" `shouldBe` Left "authentication cookie name has invalid characters",
                 mkAuthenticationCookieName "bad\"name" `shouldBe` Left "authentication cookie name has invalid characters",
                 mkAuthenticationCookieName "bad\\name" `shouldBe` Left "authentication cookie name has invalid characters",
                 mkAuthenticationCookieName "bad\DELname" `shouldBe` Left "authentication cookie name has invalid characters",
                 mkAuthenticationCookieName "b\233d-name" `shouldBe` Left "authentication cookie name has invalid characters",
                 mkAuthenticationCookieName "__Host-account!#$%&'*+-.^_`|~" `shouldSatisfy` either (const False) (const True),
                 mkAuthenticationCookieName (Text.replicate 129 "a") `shouldBe` Left "authentication cookie name is too long",
                 mkAuthenticationProofMaximumBytes 0 `shouldBe` Left "authentication proof maximum bytes must be positive",
                 mkSecurityFailureCode "" `shouldBe` Left "security failure code cannot be empty",
                 mkSecurityFailureCode (Text.replicate 81 "a") `shouldBe` Left "security failure code is too long",
                 mkSecurityFailureCode "Bad.Code" `shouldBe` Left "security failure code has invalid characters"
               ]
        )

    it "raises only for invalid static authentication declarations" $ do
      evaluate (requiredAuthenticationCookiePolicyOrDie "session" 28800 `seq` ())
        `shouldThrow` \case
          ErrorCall message -> "invalid authentication cookie declaration" `Text.isInfixOf` Text.pack message
      evaluate (requiredAuthenticationProofMaximumBytesOrDie 0 `seq` ())
        `shouldThrow` \case
          ErrorCall message -> "invalid authentication proof limit declaration" `Text.isInfixOf` Text.pack message
      evaluate (requiredSecurityFailureCodeOrDie "Bad.Code" `seq` ())
        `shouldThrow` \case
          ErrorCall message -> "invalid security failure-code declaration" `Text.isInfixOf` Text.pack message

    it "renders and clears only host-only safe JWT cookies" $ do
      let policy = fromRight (error "expected authentication cookie policy") (mkAuthenticationCookiePolicy "__Host-account-session" 28800)
          punctuationPolicy = fromRight (error "expected punctuation cookie policy") (mkAuthenticationCookiePolicy "__Host-account!#$%&'*+-.^_`|~" 28800)
      expectAll
        ( ( renderAuthenticationCookie policy (encodedJwtFromBytes "header.payload.signature")
              `shouldBe` Just "__Host-account-session=header.payload.signature; Path=/; Max-Age=28800; HttpOnly; Secure; SameSite=Strict"
          )
            :| [ clearAuthenticationCookie policy `shouldBe` "__Host-account-session=; Path=/; Max-Age=0; HttpOnly; Secure; SameSite=Strict",
                 hasDerivedContract [policy] `shouldBe` True,
                 mkAuthenticationCookiePolicy "account-session" 28800 `shouldBe` Left "authentication cookie name must use the __Host- prefix",
                 mkAuthenticationCookiePolicy "__Host-account-session" 0 `shouldBe` Left "authentication cookie max age must be positive",
                 renderAuthenticationCookie policy (encodedJwtFromBytes "header;payload") `shouldBe` Nothing,
                 renderAuthenticationCookie policy (encodedJwtFromBytes "header\npayload") `shouldBe` Nothing,
                 renderAuthenticationCookie policy (encodedJwtFromBytes (ByteString.singleton 255)) `shouldBe` Nothing,
                 renderAuthenticationCookie punctuationPolicy (encodedJwtFromBytes "header.payload.signature")
                   `shouldBe` Just "__Host-account!#$%&'*+-.^_`|~=header.payload.signature; Path=/; Max-Age=28800; HttpOnly; Secure; SameSite=Strict",
                 clearAuthenticationCookie punctuationPolicy
                   `shouldBe` "__Host-account!#$%&'*+-.^_`|~=; Path=/; Max-Age=0; HttpOnly; Secure; SameSite=Strict"
               ]
        )

    it "validates dynamically supplied security classifications" $ do
      invalidFailureCode <- newIORef "Bad.Code" >>= readIORef
      invalidCookieName <- newIORef "bad;name" >>= readIORef
      expectAll
        ( (mkSecurityFailureCode invalidFailureCode `shouldBe` Left "security failure code has invalid characters")
            :| [ mkAuthenticationCookieName invalidCookieName `shouldBe` Left "authentication cookie name has invalid characters"
               ]
        )

    it "keeps every classified authentication failure explicit and printable" $ do
      let proofRejection = mkProofRejection (requiredFailureCode "proof.rejected")
          principalRejection = mkPrincipalRejection (requiredFailureCode "principal.rejected")
          dependency = mkAuthenticationDependency (requiredFailureCode "identity.unavailable")
          extractionFailures = [ProofMalformed, ProofAmbiguous, ProofConflicting, ProofTooLarge]
          verificationFailures = [ProofRejected proofRejection, ProofVerificationUnavailable dependency]
          establishmentFailures = [PrincipalRejected principalRejection, PrincipalEstablishmentUnavailable dependency]
          authenticationFailures =
            [ ProofMissing,
              ProofExtractionRejected ProofMalformed,
              ProofVerificationRejected proofRejection,
              PrincipalEstablishmentRejected principalRejection,
              AuthenticationUnavailable dependency
            ]
          accessFailures =
            [ Unauthenticated ProofMissing,
              AccessForbidden MissingRequiredScopes,
              AccessUnavailable dependency
            ]
      expectAll
        ( (hasDerivedContract extractionFailures `shouldBe` True)
            :| [ hasDerivedContract verificationFailures `shouldBe` True,
                 hasDerivedContract establishmentFailures `shouldBe` True,
                 hasDerivedContract authenticationFailures `shouldBe` True,
                 hasDerivedContract accessFailures `shouldBe` True,
                 hasDerivedContract [proofRejection] `shouldBe` True,
                 hasDerivedContract [principalRejection] `shouldBe` True,
                 hasDerivedContract [dependency] `shouldBe` True
               ]
        )

    it "compares bounded proof configuration by its validated values" $
      expectAll
        ( (requiredCookieName "__Host-session-a" `shouldNotBe` requiredCookieName "__Host-session-b")
            :| [ hasDerivedContract [requiredCookieName "__Host-session-a", requiredCookieName "__Host-session-b"] `shouldBe` True,
                 requiredProofMaximumBytes 8 `shouldNotBe` requiredProofMaximumBytes 9,
                 hasDerivedContract [requiredProofMaximumBytes 8, requiredProofMaximumBytes 9] `shouldBe` True,
                 hasEqContract [encodedJwtFromBytes "first", encodedJwtFromBytes "second"] `shouldBe` True,
                 encodedJwtBytes (encodedJwtFromBytes "proof") `shouldBe` "proof",
                 RequireAllScopes ("reader" :| []) `shouldNotBe` (RequireAnyScope ("reader" :| []) :: ScopeRequirement Text),
                 RequireAllScopes ("reader" :| []) `shouldNotBe` (RequireAllScopes ("writer" :| []) :: ScopeRequirement Text),
                 hasDerivedContract ([RequireAllScopes ("reader" :| []), RequireAnyScope ("reader" :| ["writer"])] :: [ScopeRequirement Text]) `shouldBe` True,
                 hasDerivedContract [Authorized, Forbidden MissingRequiredScopes] `shouldBe` True,
                 hasDerivedContract [MissingRequiredScopes] `shouldBe` True
               ]
        )

  describe "authentication pipeline" $ do
    it "passes typed endpoint, proof, verified value, principal, authorization, and denial through configured collaborators" $ do
      observations <- newIORef ([] :: [Text])
      emitted <- newIORef []
      let sink = SecurityEventSink $ \requirement eventBody -> modifyIORef' emitted (<> [(requirement, eventBody)]) >> pure SecurityEventDelivered
          request = (endpointRequest (RequireAuthorized (RequireAllScopes ("reader" :| []))) []) {endpointSecurityEventSink = Just sink}
          record observation = modifyIORef' observations (<> [observation])
          pipeline =
            AuthenticationPipeline
              { authenticationProofExtractor =
                  AuthenticationProofExtractor $ \receivedRequest ->
                    if endpointNameText (endpointName (endpointMetadata receivedRequest)) == "test.authentication"
                      then Right (Just "proof")
                      else Left ProofMalformed,
                authenticationProofVerifier = AuthenticationProofVerifier $ \proof -> do
                  record proof
                  pure (Right "verified"),
                authenticationPrincipalEstablisher = PrincipalEstablisher $ \verified -> do
                  record verified
                  pure (Right "ada"),
                authenticationAuthorization =
                  AuthenticationWithAuthorization
                    ( AuthorizationInterpreter $ \principal requirement ->
                        if principal == "ada" && requirement == RequireAllScopes ("reader" :| [])
                          then Authorized
                          else Forbidden MissingRequiredScopes
                    )
                    (\MissingRequiredScopes -> requiredFailureCode "authorization.denied")
                    ( \receivedRequest denial ->
                        case (endpointAccess (endpointMetadata receivedRequest), denial) of
                          (RequireAuthorized _, MissingRequiredScopes) -> NonPageBodyResponse response403
                          _ -> NonPageBodyResponse response503
                    ),
                authenticationAttachPrincipal = \principal requestContextValue -> requestContextValue {requestLanguage = principal},
                authenticationChallenge = \receivedRequest failure ->
                  case (endpointAccess (endpointMetadata receivedRequest), failure) of
                    (RequireAuthenticated, ProofMissing) -> NonPageBodyResponse response401
                    _ -> NonPageBodyResponse response503,
                authenticationUnavailable = \receivedRequest dependency ->
                  if endpointAccess (endpointMetadata receivedRequest) == RequireAuthenticated
                    && dependency == mkAuthenticationDependency (requiredFailureCode "identity.unavailable")
                    then NonPageBodyResponse response503
                    else NonPageBodyResponse response401
              }
      runAuthenticationPipeline pipeline request `shouldReturn` ContinueEndpoint (defaultContext {requestLanguage = "ada"})
      runAuthenticationPipeline pipeline ((endpointRequest (RequireAuthorized (RequireAllScopes ("operator" :| []))) []) {endpointSecurityEventSink = Just sink})
        `shouldReturn` HaltEndpoint (NonPageBodyResponse response403)
      readIORef observations `shouldReturn` ["proof", "verified", "proof", "verified"]
      readIORef emitted
        `shouldReturn` [ (TelemetryBestEffort, AuthenticationEvaluated (AuthenticationEvent AuthenticationEstablished Nothing)),
                         (TelemetryBestEffort, AuthenticationEvaluated (AuthenticationEvent AuthenticationEstablished Nothing)),
                         (TelemetryBestEffort, AuthorizationDenied (AuthorizationEvent (requiredFailureCode "authorization.denied")))
                       ]

    it "enriches anonymous endpoints only after a valid current principal" $ do
      let pipeline = testPipeline (AuthenticationProofExtractor (const (Right (Just "proof")))) (const (pure (Right "verified"))) (const (pure (Right "ada")))
      request <- newIORef (endpointRequest AllowUnauthenticated []) >>= readIORef
      runAuthenticationPipeline pipeline request
        `shouldReturn` ContinueEndpoint (defaultContext {requestLanguage = "ada"})

    it "fails closed when an authentication-only pipeline is attached to a scoped endpoint" $ do
      let pipeline = authenticationOnlyPipeline (AuthenticationProofExtractor (const (Right (Just "proof")))) (const (pure (Right "verified"))) (const (pure (Right "ada")))
      runAuthenticationPipeline pipeline (authenticationOnlyEndpointRequest (RequireAuthorized ()) [])
        `shouldReturn` HaltEndpoint (NonPageBodyResponse (responseBodyWith Http.status503 "Authentication unavailable for en"))

    it "continues anonymous endpoints without leaking presented-proof failure" $ do
      let rejected = mkProofRejection (requiredFailureCode "proof.rejected")
          pipeline = testPipeline (AuthenticationProofExtractor (const (Right (Just "proof")))) (const (pure (Left (ProofRejected rejected)))) (const (pure (Right "ada")))
      request <- newIORef (endpointRequest AllowUnauthenticated []) >>= readIORef
      runAuthenticationPipeline pipeline request
        `shouldReturn` ContinueEndpoint defaultContext

    it "emits anonymous successful authentication through a root-attached sink" $ do
      emitted <- newIORef []
      let sink = SecurityEventSink $ \requirement eventBody -> modifyIORef' emitted (<> [(requirement, eventBody)]) >> pure SecurityEventDelivered
          pipeline =
            testPipeline
              ( AuthenticationProofExtractor $ \receivedRequest ->
                  if endpointProtocol (endpointMetadata receivedRequest) == HtmlEndpoint
                    then Right (Just "proof")
                    else Left ProofMalformed
              )
              (const (pure (Right "verified")))
              (const (pure (Right "ada")))
          request = (endpointRequest AllowUnauthenticated []) {endpointSecurityEventSink = Just sink}
      runAuthenticationPipeline pipeline request `shouldReturn` ContinueEndpoint (defaultContext {requestLanguage = "ada"})
      readIORef emitted `shouldReturn` [(TelemetryBestEffort, AuthenticationEvaluated (AuthenticationEvent AuthenticationEstablished Nothing))]

    it "challenges protected endpoints for missing or rejected proof, and returns 503 only for dependency loss" $ do
      let rejection = mkProofRejection (requiredFailureCode "proof.rejected")
          dependency = mkAuthenticationDependency (requiredFailureCode "identity.store-unavailable")
          missing = testPipeline (AuthenticationProofExtractor (const (Right Nothing))) (const (pure (Right "verified"))) (const (pure (Right "ada")))
          rejected = testPipeline (AuthenticationProofExtractor (const (Right (Just "proof")))) (const (pure (Left (ProofRejected rejection)))) (const (pure (Right "ada")))
          unavailable = testPipeline (AuthenticationProofExtractor (const (Right (Just "proof")))) (const (pure (Left (ProofVerificationUnavailable dependency)))) (const (pure (Right "ada")))
      request <- newIORef (endpointRequest RequireAuthenticated []) >>= readIORef
      expectAll
        ( ( runAuthenticationPipeline missing request
              `shouldReturn` HaltEndpoint (NonPageBodyResponse response401)
          )
            :| [ runAuthenticationPipeline rejected request
                   `shouldReturn` HaltEndpoint (NonPageBodyResponse response401),
                 runAuthenticationPipeline unavailable request
                   `shouldReturn` HaltEndpoint (NonPageBodyResponse response503)
               ]
        )

    it "marks only a pre-handler client-action challenge for retained reauthentication" $ do
      let ordinaryChallenge = NonPageBodyResponse response401
          actionRequest = (endpointRequest RequireAuthenticated []) {endpointDispatchKind = EndpointClientAction}
      expectAll
        ( (authenticationChallengeForAction actionRequest ordinaryChallenge `shouldBe` NonPageClientActionBodyResponse clientActionReauthenticationRequiredResponse)
            :| [ authenticationChallengeForAction (endpointRequest RequireAuthenticated []) ordinaryChallenge `shouldBe` ordinaryChallenge,
                 clientActionStatus clientActionReauthenticationRequiredResponse `shouldBe` Http.status401,
                 clientActionHeaders clientActionReauthenticationRequiredResponse `shouldBe` [("X-Harch-Action-Reauthenticate", "required")]
               ]
        )

    it "navigates an independent authentication challenge without retaining its action" $ do
      let destination = RouteRequest DataRoute defaultContext
          actionRequest = (endpointRequest RequireAuthenticated []) {endpointDispatchKind = EndpointClientAction}
          expectedActionResponse =
            ClientActionResponse
              { clientActionStatus = Http.status401,
                clientActionPatches = [],
                clientActionFocusId = Nothing,
                clientActionNavigation = NavigateInternal ReplaceHistory destination,
                clientActionStorageCleanup = noClientStorageCleanup,
                clientActionFailureDestinations = noClientActionFailureDestinations,
                clientActionHeaders = [("X-Harch-Action-Authentication", "navigate")],
                clientActionObservabilityAttributes = [],
                clientActionLogEntries = []
              }
      expectAll
        ( ( authenticationNavigationChallengeForAction actionRequest destination
              `shouldBe` NonPageClientActionBodyResponse expectedActionResponse
          )
            :| [ authenticationNavigationChallengeForAction (endpointRequest RequireAuthenticated []) destination
                   `shouldBe` nonPageInternalRedirectResponse Http.status303 destination,
                 clientActionStatus expectedActionResponse `shouldBe` Http.status401,
                 clientActionHeaders expectedActionResponse `shouldBe` [("X-Harch-Action-Authentication", "navigate")]
               ]
        )

    it "gives failure responders the selected endpoint and typed failure" $ do
      let dependency = mkAuthenticationDependency (requiredFailureCode "identity.store-unavailable")
          pipeline =
            ( testPipeline
                (AuthenticationProofExtractor (const (Right Nothing)))
                (const (pure (Right "verified")))
                (const (pure (Right "ada")))
            )
              { authenticationChallenge = \receivedRequest failure ->
                  case (endpointNameText (endpointName (endpointMetadata receivedRequest)), endpointDispatchKind receivedRequest, failure) of
                    ("test.authentication", EndpointMatched, ProofMissing) -> NonPageBodyResponse response401
                    _ -> NonPageBodyResponse response503,
                authenticationUnavailable = \receivedRequest receivedDependency ->
                  if endpointRouteTemplate (endpointMetadata receivedRequest) == requiredRouteTemplate "/authentication"
                    && receivedDependency == dependency
                    then NonPageBodyResponse response503
                    else NonPageBodyResponse response401
              }
          unavailablePipeline =
            pipeline
              { authenticationProofExtractor = AuthenticationProofExtractor (const (Right (Just ("proof" :: Text)))),
                authenticationProofVerifier = AuthenticationProofVerifier (const (pure (Left (ProofVerificationUnavailable dependency))))
              }
          principalUnavailablePipeline =
            pipeline
              { authenticationProofExtractor = AuthenticationProofExtractor (const (Right (Just ("proof" :: Text)))),
                authenticationProofVerifier = AuthenticationProofVerifier (const (pure (Right ("verified" :: Text)))),
                authenticationPrincipalEstablisher = PrincipalEstablisher (const (pure (Left (PrincipalEstablishmentUnavailable dependency))))
              }
          request = endpointRequest RequireAuthenticated []
      expectAll
        ( (runAuthenticationPipeline pipeline request `shouldReturn` HaltEndpoint (NonPageBodyResponse response401))
            :| [ runAuthenticationPipeline unavailablePipeline request `shouldReturn` HaltEndpoint (NonPageBodyResponse response503),
                 runAuthenticationPipeline principalUnavailablePipeline request `shouldReturn` HaltEndpoint (NonPageBodyResponse response503)
               ]
        )

    it "enforces authorization only after principal establishment" $ do
      let pipeline = testPipeline (AuthenticationProofExtractor (const (Right (Just "proof")))) (const (pure (Right "verified"))) (const (pure (Right "ada")))
          scopes = RequireAllScopes ("operator" :| [])
      runAuthenticationPipeline pipeline (endpointRequest (RequireAuthorized scopes) [])
        `shouldReturn` HaltEndpoint (NonPageBodyResponse response403)

    it "runs the guard adapter and preserves every protected authentication failure rail" $ do
      let rejection = mkProofRejection (requiredFailureCode "proof.rejected")
          principalRejection = mkPrincipalRejection (requiredFailureCode "principal.rejected")
          dependency = mkAuthenticationDependency (requiredFailureCode "identity.unavailable")
          extractedFailure = testPipeline (AuthenticationProofExtractor (const (Left ProofMalformed))) (const (pure (Right "verified"))) (const (pure (Right "ada")))
          principalFailure = testPipeline (AuthenticationProofExtractor (const (Right (Just "proof")))) (const (pure (Right "verified"))) (const (pure (Left (PrincipalRejected principalRejection))))
          principalUnavailable = testPipeline (AuthenticationProofExtractor (const (Right (Just "proof")))) (const (pure (Right "verified"))) (const (pure (Left (PrincipalEstablishmentUnavailable dependency))))
          authenticated = testPipeline (AuthenticationProofExtractor (const (Right (Just "proof")))) (const (pure (Right "verified"))) (const (pure (Right "ada")))
          authorized = testPipeline (AuthenticationProofExtractor (const (Right (Just "proof")))) (const (pure (Right "verified"))) (const (pure (Right "ada")))
      expectAll
        ( ( runAuthenticationGuard (authenticationGuardFromPipeline extractedFailure) (endpointRequest RequireAuthenticated [])
              `shouldReturn` HaltEndpoint (NonPageBodyResponse response401)
          )
            :| [ runAuthenticationPipeline principalFailure (endpointRequest RequireAuthenticated [])
                   `shouldReturn` HaltEndpoint (NonPageBodyResponse response401),
                 runAuthenticationPipeline principalUnavailable (endpointRequest RequireAuthenticated [])
                   `shouldReturn` HaltEndpoint (NonPageBodyResponse response503),
                 runAuthenticationPipeline authenticated (endpointRequest RequireAuthenticated [])
                   `shouldReturn` ContinueEndpoint (defaultContext {requestLanguage = "ada"}),
                 runAuthenticationPipeline authorized (endpointRequest (RequireAuthorized (RequireAllScopes ("reader" :| []))) [])
                   `shouldReturn` ContinueEndpoint (defaultContext {requestLanguage = "ada"}),
                 show rejection `shouldSatisfy` (not . null)
               ]
        )

    it "emits typed authentication and authorization facts through the root-attached sink" $ do
      emitted <- newIORef []
      let sink =
            SecurityEventSink $ \requirement eventBody -> do
              modifyIORef' emitted (<> [(requirement, eventBody)])
              pure SecurityEventDelivered
          pipeline = testPipeline (AuthenticationProofExtractor (const (Right (Just "proof")))) (const (pure (Right "verified"))) (const (pure (Right "ada")))
          scopes = RequireAllScopes ("operator" :| [])
          request = (endpointRequest (RequireAuthorized scopes) []) {endpointSecurityEventSink = Just sink}
      runAuthenticationPipeline pipeline request `shouldReturn` HaltEndpoint (NonPageBodyResponse response403)
      readIORef emitted
        `shouldReturn` [ (TelemetryBestEffort, AuthenticationEvaluated (AuthenticationEvent AuthenticationEstablished Nothing)),
                         (TelemetryBestEffort, AuthorizationDenied (AuthorizationEvent (requiredFailureCode "authorization.denied")))
                       ]

    it "classifies every failed proof rail before emitting through a root sink" $ do
      emitted <- newIORef []
      let sink = SecurityEventSink $ \_ eventBody -> modifyIORef' emitted (<> [eventBody]) >> pure SecurityEventDelivered
          rejected = mkProofRejection (requiredFailureCode "proof.rejected")
          principalRejection = mkPrincipalRejection (requiredFailureCode "principal.rejected")
          dependency = mkAuthenticationDependency (requiredFailureCode "identity.unavailable")
          request = (endpointRequest RequireAuthenticated []) {endpointSecurityEventSink = Just sink}
          missing = testPipeline (AuthenticationProofExtractor (const (Right Nothing))) (const (pure (Right "verified"))) (const (pure (Right "ada")))
          malformed = testPipeline (AuthenticationProofExtractor (const (Left ProofMalformed))) (const (pure (Right "verified"))) (const (pure (Right "ada")))
          verificationRejected = testPipeline (AuthenticationProofExtractor (const (Right (Just "proof")))) (const (pure (Left (ProofRejected rejected)))) (const (pure (Right "ada")))
          principalRejected = testPipeline (AuthenticationProofExtractor (const (Right (Just "proof")))) (const (pure (Right "verified"))) (const (pure (Left (PrincipalRejected principalRejection))))
          unavailable = testPipeline (AuthenticationProofExtractor (const (Right (Just "proof")))) (const (pure (Left (ProofVerificationUnavailable dependency)))) (const (pure (Right "ada")))
      runAuthenticationPipeline missing request `shouldReturn` HaltEndpoint (NonPageBodyResponse response401)
      runAuthenticationPipeline malformed request `shouldReturn` HaltEndpoint (NonPageBodyResponse response401)
      runAuthenticationPipeline verificationRejected request `shouldReturn` HaltEndpoint (NonPageBodyResponse response401)
      runAuthenticationPipeline principalRejected request `shouldReturn` HaltEndpoint (NonPageBodyResponse response401)
      runAuthenticationPipeline unavailable request `shouldReturn` HaltEndpoint (NonPageBodyResponse response503)
      readIORef emitted
        `shouldReturn` [ AuthenticationEvaluated (AuthenticationEvent AuthenticationMissing Nothing),
                         AuthenticationEvaluated (AuthenticationEvent AuthenticationRejected Nothing),
                         AuthenticationEvaluated (AuthenticationEvent AuthenticationRejected (Just (requiredFailureCode "proof.rejected"))),
                         AuthenticationEvaluated (AuthenticationEvent AuthenticationRejected (Just (requiredFailureCode "principal.rejected"))),
                         AuthenticationEvaluated (AuthenticationEvent AuthenticationDependencyUnavailable (Just (requiredFailureCode "identity.unavailable")))
                       ]

  describe "scope authorization" $
    it "requires every required scope or at least one alternative" $ do
      let authorize :: AuthorizationInterpreter [Text] (ScopeRequirement Text) ScopeAuthorizationDenial
          authorize = scopeAuthorizationInterpreter id
      expectAll
        ( (authorizePrincipal authorize ["read", "write"] (RequireAllScopes ("read" :| ["write"])) `shouldBe` Authorized)
            :| [ authorizePrincipal authorize ["read"] (RequireAllScopes ("read" :| ["write"])) `shouldBe` Forbidden MissingRequiredScopes,
                 authorizePrincipal authorize ["write"] (RequireAnyScope ("read" :| ["write"])) `shouldBe` Authorized,
                 authorizePrincipal authorize [] (RequireAnyScope ("read" :| ["write"])) `shouldBe` Forbidden MissingRequiredScopes
               ]
        )

  describe "pipeline configuration" $
    it "exposes each configured collaborator through its typed field" $ do
      let pipeline = testPipeline (AuthenticationProofExtractor (const (Right (Just "proof")))) (const (pure (Right "verified"))) (const (pure (Right "ada")))
          request = endpointRequest RequireAuthenticated []
          AuthenticationProofExtractor extractor = authenticationProofExtractor pipeline
          AuthenticationProofVerifier verifier = authenticationProofVerifier pipeline
          PrincipalEstablisher establisher = authenticationPrincipalEstablisher pipeline
      expectAll
        ( (extractor request `shouldBe` Right (Just "proof"))
            :| [ verifier "proof" `shouldReturn` Right "verified",
                 establisher "verified" `shouldReturn` Right "ada",
                 authenticationAttachPrincipal pipeline "ada" defaultContext `shouldBe` defaultContext {requestLanguage = "ada"},
                 authenticationChallenge pipeline request ProofMissing `shouldBe` NonPageBodyResponse response401,
                 authenticationUnavailable pipeline request (mkAuthenticationDependency (requiredFailureCode "identity.unavailable")) `shouldBe` NonPageBodyResponse response503
               ]
        )

  describe "security failure codes" $
    it "compares and renders only validated low-cardinality classifications" $ do
      let accepted = requiredFailureCode "identity.accepted"
          rejected = requiredFailureCode "identity.rejected"
      expectAll
        ( (accepted == accepted `shouldBe` True)
            :| [ hasDerivedContract [accepted, rejected] `shouldBe` True,
                 accepted == rejected `shouldBe` False,
                 compare accepted rejected `shouldBe` LT,
                 compare rejected accepted `shouldBe` GT,
                 accepted < rejected `shouldBe` True,
                 accepted <= accepted `shouldBe` True,
                 rejected > accepted `shouldBe` True,
                 rejected >= rejected `shouldBe` True,
                 max accepted rejected `shouldBe` rejected,
                 min accepted rejected `shouldBe` accepted,
                 show accepted `shouldSatisfy` (not . null),
                 show rejected `shouldSatisfy` (not . null)
               ]
        )

endpointRequest :: AccessRequirement (ScopeRequirement Text) -> Http.RequestHeaders -> EndpointRequest TestRoute TestContext (ScopeRequirement Text)
endpointRequest access headers =
  EndpointRequest
    { endpointWaiRequest = Wai.defaultRequest {Wai.requestHeaders = headers},
      endpointRouteRequest = RouteRequest DataRoute defaultContext,
      endpointMetadata = mkEndpointMetadata (requiredEndpointName "test.authentication") (requiredRouteTemplate "/authentication") HtmlEndpoint access,
      endpointSecurityEventSink = Nothing,
      endpointDispatchKind = EndpointMatched
    }

authenticationOnlyEndpointRequest :: AccessRequirement () -> Http.RequestHeaders -> EndpointRequest TestRoute TestContext ()
authenticationOnlyEndpointRequest access headers =
  EndpointRequest
    { endpointWaiRequest = Wai.defaultRequest {Wai.requestHeaders = headers},
      endpointRouteRequest = RouteRequest DataRoute defaultContext,
      endpointMetadata = mkEndpointMetadata (requiredEndpointName "test.authentication") (requiredRouteTemplate "/authentication") HtmlEndpoint access,
      endpointSecurityEventSink = Nothing,
      endpointDispatchKind = EndpointMatched
    }

testPipeline :: AuthenticationProofExtractor TestRoute TestContext (ScopeRequirement Text) Text -> (Text -> IO (Either ProofVerificationFailure Text)) -> (Text -> IO (Either PrincipalEstablishmentFailure Text)) -> AuthenticationPipeline TestRoute TestContext (ScopeRequirement Text) Text Text Text ScopeAuthorizationDenial
testPipeline extractor verifier establisher =
  AuthenticationPipeline
    { authenticationProofExtractor = extractor,
      authenticationProofVerifier = AuthenticationProofVerifier verifier,
      authenticationPrincipalEstablisher = PrincipalEstablisher establisher,
      authenticationAuthorization =
        AuthenticationWithAuthorization
          (scopeAuthorizationInterpreter (const ["reader"]))
          (const (requiredFailureCode "authorization.denied"))
          (\_ _ -> NonPageBodyResponse response403),
      authenticationAttachPrincipal = \principal requestContextValue -> requestContextValue {requestLanguage = principal},
      authenticationChallenge = \_ _ -> NonPageBodyResponse response401,
      authenticationUnavailable = \_ _ -> NonPageBodyResponse response503
    }

authenticationOnlyPipeline :: AuthenticationProofExtractor TestRoute TestContext () Text -> (Text -> IO (Either ProofVerificationFailure Text)) -> (Text -> IO (Either PrincipalEstablishmentFailure Text)) -> AuthenticationPipeline TestRoute TestContext () Text Text Text ()
authenticationOnlyPipeline extractor verifier establisher =
  AuthenticationPipeline
    { authenticationProofExtractor = extractor,
      authenticationProofVerifier = AuthenticationProofVerifier verifier,
      authenticationPrincipalEstablisher = PrincipalEstablisher establisher,
      authenticationAuthorization =
        AuthenticationWithoutAuthorization
          ( \request ->
              NonPageBodyResponse
                (responseBodyWith Http.status503 ("Authentication unavailable for " <> requestLanguage (requestContext (endpointRouteRequest request))))
          ),
      authenticationAttachPrincipal = \principal requestContextValue -> requestContextValue {requestLanguage = principal},
      authenticationChallenge = \_ _ -> NonPageBodyResponse response401,
      authenticationUnavailable = \_ _ -> NonPageBodyResponse response503
    }

response401 :: ResponseBody
response401 = responseBodyWith Http.status401 "Sign in required"

response403 :: ResponseBody
response403 = responseBodyWith Http.status403 "Forbidden"

response503 :: ResponseBody
response503 = responseBodyWith Http.status503 "Authentication unavailable"

responseBodyWith :: Http.Status -> Text -> ResponseBody
responseBodyWith status body =
  ResponseBody
    { responseStatus = status,
      responseContentType = "text/plain; charset=utf-8",
      responseBody = body,
      responseObservabilityAttributes = [],
      responseLogEntries = [],
      responseDatabaseOperations = []
    }

requiredCookieName :: Text -> AuthenticationCookieName
requiredCookieName cookieNameValue = fromRight (error "invalid cookie name") (mkAuthenticationCookieName cookieNameValue)

requiredProofMaximumBytes :: Int -> AuthenticationProofMaximumBytes
requiredProofMaximumBytes byteCount = fromRight (error "invalid proof maximum") (mkAuthenticationProofMaximumBytes byteCount)

requiredFailureCode :: Text -> SecurityFailureCode
requiredFailureCode failureCodeValue = fromRight (error "invalid failure code") (mkSecurityFailureCode failureCodeValue)

requiredEndpointName :: Text -> EndpointName
requiredEndpointName endpointNameValue = fromRight (error "invalid endpoint name") (mkEndpointName endpointNameValue)

requiredRouteTemplate :: Text -> RouteTemplate
requiredRouteTemplate routeTemplateValue = fromRight (error "invalid route template") (mkRouteTemplate routeTemplateValue)

hasEqContract :: (Eq value) => [value] -> Bool
hasEqContract values =
  sum [fromEnum (left == right) | left <- values, right <- values] == length values
    && sum [fromEnum (left /= right) | left <- values, right <- values]
      == length values * (length values - 1)

hasDerivedContract :: (Eq value, Show value) => [value] -> Bool
hasDerivedContract values =
  hasEqContract values
    && sum [length (show item) + length (showList [item] "") | item <- values] > 0
