{-# LANGUAGE OverloadedStrings #-}

-- | Pluggable post-match authentication and authorization.
--
-- Decision record (AHI-4A, 2026-09-01): this module builds the configured
-- 'AuthenticationGuard' already owned by the endpoint-security rail. It does
-- not parse routes, run WAI middleware, or receive a handler continuation.
-- Separating proof validity from principal establishment means a valid signed
-- token cannot bypass a revoked session or disabled-account check; a fully
-- stateless application may deliberately provide a pure establisher instead.
--
-- Decision record (AHI-4C follow-up, 2026-09-04): authorization is optional
-- application policy after authentication, not an impossible @Void@ callback
-- bundle. 'AuthenticationWithAuthorization' preserves scoped endpoints;
-- 'AuthenticationWithoutAuthorization' gives an authentication-only
-- application an explicit fail-closed response if a scoped route is later
-- attached. This refines the existing guard boundary instead of introducing a
-- second authentication pipeline.
--
-- Decision record (AHI-4C coverage follow-up, 2026-09-04): fixed declarations
-- such as an application-owned cookie policy, failure code, or proof limit use
-- the explicit @required...OrDie@ helpers below. Runtime configuration keeps
-- the corresponding @mk...@ 'Either' rails. This follows Harch's existing
-- declaration boundary rather than making a client input rejection appear
-- impossible merely to satisfy a coverage gate.
module HarchWeb.Authentication
  ( AccessFailure (..),
    AuthenticationDependency,
    AuthenticationFailure (..),
    AuthenticationAuthorization (..),
    AuthenticationPipeline (..),
    AuthenticationCookiePolicy,
    AuthenticationProofMaximumBytes,
    AuthenticationProofExtractor (..),
    AuthenticationProofVerifier (..),
    AuthenticationCookieName,
    AuthorizationDecision (..),
    AuthorizationInterpreter (..),
    EncodedJwt,
    PrincipalEstablisher (..),
    PrincipalEstablishmentFailure (..),
    PrincipalRejection,
    ProofExtractionFailure (..),
    ProofRejection,
    ProofVerificationFailure (..),
    ScopeRequirement (..),
    ScopeAuthorizationDenial (..),
    SecurityFailureCode,
    authenticationGuardFromPipeline,
    authenticationChallengeForAction,
    authenticationCookieName,
    bearerJwtExtractor,
    clearAuthenticationCookie,
    combineProofExtractors,
    cookieJwtExtractor,
    encodedJwtFromBytes,
    encodedJwtBytes,
    mkAuthenticationDependency,
    mkAuthenticationCookieName,
    mkAuthenticationCookiePolicy,
    mkAuthenticationProofMaximumBytes,
    mkPrincipalRejection,
    mkProofRejection,
    mkSecurityFailureCode,
    renderAuthenticationCookie,
    requiredAuthenticationCookiePolicyOrDie,
    requiredAuthenticationProofMaximumBytesOrDie,
    requiredSecurityFailureCodeOrDie,
    runAuthenticationPipeline,
    scopeAuthorizationInterpreter,
  )
where

import Control.Monad (void)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Either (fromRight)
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word64)
import HarchWeb.EndpointSecurity
  ( AccessRequirement (..),
    AuthenticationGuard (..),
    EndpointDispatchKind (EndpointClientAction),
    EndpointGuardResult (..),
    EndpointMetadata (..),
    EndpointRequest (..),
  )
import HarchWeb.Routing (RouteRequest (requestContext))
import HarchWeb.SecurityEvent
  ( AuthenticationEvent (..),
    AuthenticationEventOutcome (..),
    AuthorizationEvent (..),
    EventDeliveryRequirement (TelemetryBestEffort),
    SecurityEvent (AuthenticationEvaluated, AuthorizationDenied),
    SecurityEventSink (..),
  )
import HarchWeb.SecurityFailureCode
import HarchWeb.Server.ClientAction (clientActionReauthenticationRequiredResponse)
import HarchWeb.Server.Response (NonPageResponse (..))
import Network.Wai qualified as Wai

newtype ProofRejection = ProofRejection SecurityFailureCode
  deriving (Eq, Show)

newtype PrincipalRejection = PrincipalRejection SecurityFailureCode
  deriving (Eq, Show)

newtype AuthenticationDependency = AuthenticationDependency SecurityFailureCode
  deriving (Eq, Show)

mkProofRejection :: SecurityFailureCode -> ProofRejection
mkProofRejection = ProofRejection

mkPrincipalRejection :: SecurityFailureCode -> PrincipalRejection
mkPrincipalRejection = PrincipalRejection

mkAuthenticationDependency :: SecurityFailureCode -> AuthenticationDependency
mkAuthenticationDependency = AuthenticationDependency

-- | A validated cookie name selected by an application configuration. The
-- extractor never uses a raw header-derived name as a lookup key.
newtype AuthenticationCookieName = AuthenticationCookieName ByteString
  deriving (Eq, Show)

mkAuthenticationCookieName :: Text -> Either Text AuthenticationCookieName
mkAuthenticationCookieName value
  | Text.null value = Left "authentication cookie name cannot be empty"
  | Text.length value > 128 = Left "authentication cookie name is too long"
  | Text.all validCookieCharacter value = Right (AuthenticationCookieName (TextEncoding.encodeUtf8 value))
  | otherwise = Left "authentication cookie name has invalid characters"
  where
    validCookieCharacter character = character > ' ' && character /= ';' && character /= '=' && character /= ','

-- | The only browser-session cookie policy supplied by Harch's JWT boundary.
-- A policy is deliberately host-only: the @__Host-@ prefix plus the fixed
-- @Path=/@, @Secure@, @HttpOnly@, and @SameSite=Strict@ attributes make a
-- deployment-specific domain or broad path impossible to author here.
--
-- Decision (AHI-4C, 2026-09-03): JWT proof extraction already belonged to
-- this module, but issuing one required every application to reconstruct the
-- security-sensitive cookie string.  Extend that existing capability with a
-- validated policy and opaque-token renderer rather than adding a web-api
-- helper. Applications still own the chosen name and lifetime; Harch never
-- loads keys or issues a JWT itself.
data AuthenticationCookiePolicy = AuthenticationCookiePolicy
  { authenticationCookieName :: AuthenticationCookieName,
    authenticationCookieMaxAgeSeconds :: Word64
  }
  deriving (Eq, Show)

-- | Construct a non-sliding host-only JWT cookie policy. A zero lifetime is
-- reserved for 'clearAuthenticationCookie', so successful authentication
-- cannot accidentally issue an immediately expired credential.
mkAuthenticationCookiePolicy :: Text -> Word64 -> Either Text AuthenticationCookiePolicy
mkAuthenticationCookiePolicy name maxAgeSeconds
  | maxAgeSeconds == 0 = Left "authentication cookie max age must be positive"
  | not ("__Host-" `Text.isPrefixOf` name) = Left "authentication cookie name must use the __Host- prefix"
  | otherwise =
      AuthenticationCookiePolicy <$> mkAuthenticationCookieName name <*> pure maxAgeSeconds

-- | Require a host-only cookie declaration authored by application code to
-- remain valid. Request-derived names or durations must use
-- 'mkAuthenticationCookiePolicy' and handle its rejection rail instead.
--
-- This follows the established @required...OrDie@ declaration boundary: an
-- error identifies an invalid program declaration, never a client outcome.
requiredAuthenticationCookiePolicyOrDie :: Text -> Word64 -> AuthenticationCookiePolicy
requiredAuthenticationCookiePolicyOrDie name maxAgeSeconds =
  fromRight
    (error "invalid authentication cookie declaration")
    (mkAuthenticationCookiePolicy name maxAgeSeconds)

-- | Render an issued compact JWT only when its opaque bytes are valid cookie
-- octets. 'EncodedJwt' intentionally also represents untrusted received
-- proofs for verifier tests, so rendering is partial rather than assuming its
-- bytes came from 'issueJwt'.
renderAuthenticationCookie :: AuthenticationCookiePolicy -> EncodedJwt -> Maybe Text
renderAuthenticationCookie policy encodedJwt = do
  token <- either (const Nothing) Just (TextEncoding.decodeUtf8' (encodedJwtBytes encodedJwt))
  if Text.null token || Text.any (not . validCookieValueCharacter) token
    then Nothing
    else
      pure
        ( authenticationCookieNameText (authenticationCookieName policy)
            <> "="
            <> token
            <> cookieAttributes (authenticationCookieMaxAgeSeconds policy)
        )

-- | Expire the configured host-only cookie after durable session revocation.
-- It deliberately accepts no token value, so logout cannot reflect an
-- untrusted credential back into a response header.
clearAuthenticationCookie :: AuthenticationCookiePolicy -> Text
clearAuthenticationCookie policy =
  authenticationCookieNameText (authenticationCookieName policy)
    <> "="
    <> cookieAttributes 0

authenticationCookieNameText :: AuthenticationCookieName -> Text
authenticationCookieNameText (AuthenticationCookieName name) = TextEncoding.decodeUtf8 name

cookieAttributes :: Word64 -> Text
cookieAttributes maxAgeSeconds =
  "; Path=/; Max-Age="
    <> Text.pack (show maxAgeSeconds)
    <> "; HttpOnly; Secure; SameSite=Strict"

validCookieValueCharacter :: Char -> Bool
validCookieValueCharacter character =
  character > ' '
    && character <= '~'
    && character /= '"'
    && character /= ','
    && character /= ';'
    && character /= '\\'

-- | A positive, application-selected byte budget for one compact proof.
-- Keeping the bound validated makes a missing or non-positive extraction
-- limit impossible to pass into the request path.
newtype AuthenticationProofMaximumBytes = AuthenticationProofMaximumBytes Int
  deriving (Eq, Show)

mkAuthenticationProofMaximumBytes :: Int -> Either Text AuthenticationProofMaximumBytes
mkAuthenticationProofMaximumBytes value
  | value <= 0 = Left "authentication proof maximum bytes must be positive"
  | otherwise = Right (AuthenticationProofMaximumBytes value)

-- | Require an application-authored positive proof limit. Runtime-selected
-- limits must use 'mkAuthenticationProofMaximumBytes' so configuration errors
-- remain on the ordinary validation rail.
requiredAuthenticationProofMaximumBytesOrDie :: Int -> AuthenticationProofMaximumBytes
requiredAuthenticationProofMaximumBytesOrDie value =
  fromRight
    (error "invalid authentication proof limit declaration")
    (mkAuthenticationProofMaximumBytes value)

-- | Require a fixed, application-authored failure classification. Dynamic
-- classifications must use 'mkSecurityFailureCode' and keep rejection on the
-- normal input-validation rail.
requiredSecurityFailureCodeOrDie :: Text -> SecurityFailureCode
requiredSecurityFailureCodeOrDie value =
  fromRight
    (error "invalid security failure-code declaration")
    (mkSecurityFailureCode value)

-- | Opaque compact-JWT bytes. It intentionally has no 'Show' instance: a
-- proof must not reach assertion failures, logs, or telemetry by accident.
newtype EncodedJwt = EncodedJwt ByteString
  deriving (Eq)

encodedJwtBytes :: EncodedJwt -> ByteString
encodedJwtBytes (EncodedJwt value) = value

encodedJwtFromBytes :: ByteString -> EncodedJwt
encodedJwtFromBytes = EncodedJwt

data ProofExtractionFailure
  = ProofMalformed
  | ProofAmbiguous
  | ProofTooLarge
  deriving (Eq, Show)

data ProofVerificationFailure
  = ProofRejected ProofRejection
  | ProofVerificationUnavailable AuthenticationDependency
  deriving (Eq, Show)

data PrincipalEstablishmentFailure
  = PrincipalRejected PrincipalRejection
  | PrincipalEstablishmentUnavailable AuthenticationDependency
  deriving (Eq, Show)

data AuthenticationFailure
  = ProofMissing
  | ProofExtractionRejected ProofExtractionFailure
  | ProofVerificationRejected ProofRejection
  | PrincipalEstablishmentRejected PrincipalRejection
  | AuthenticationUnavailable AuthenticationDependency
  deriving (Eq, Show)

data AccessFailure denial
  = Unauthenticated AuthenticationFailure
  | AccessForbidden denial
  | AccessUnavailable AuthenticationDependency
  deriving (Eq, Show)

newtype AuthenticationProofExtractor route context authorization proof = AuthenticationProofExtractor
  { extractAuthenticationProof :: EndpointRequest route context authorization -> Either ProofExtractionFailure (Maybe proof)
  }

newtype AuthenticationProofVerifier proof verified = AuthenticationProofVerifier
  { verifyAuthenticationProof :: proof -> IO (Either ProofVerificationFailure verified)
  }

newtype PrincipalEstablisher verified principal = PrincipalEstablisher
  { establishPrincipal :: verified -> IO (Either PrincipalEstablishmentFailure principal)
  }

data AuthorizationDecision denial
  = Authorized
  | Forbidden denial
  deriving (Eq, Show)

newtype AuthorizationInterpreter principal authorization denial = AuthorizationInterpreter
  { authorizePrincipal :: principal -> authorization -> AuthorizationDecision denial
  }

data ScopeRequirement scope
  = RequireAllScopes (NonEmpty scope)
  | RequireAnyScope (NonEmpty scope)
  deriving (Eq, Show)

data ScopeAuthorizationDenial
  = MissingRequiredScopes
  deriving (Eq, Show)

scopeAuthorizationInterpreter :: (Eq scope) => (principal -> [scope]) -> AuthorizationInterpreter principal (ScopeRequirement scope) ScopeAuthorizationDenial
scopeAuthorizationInterpreter principalScopes =
  AuthorizationInterpreter $ \principal scopeRequirement ->
    case scopeRequirement of
      RequireAllScopes requiredScopes ->
        if all (`elem` principalScopes principal) (NonEmpty.toList requiredScopes)
          then Authorized
          else Forbidden MissingRequiredScopes
      RequireAnyScope requiredScopes ->
        if any (`elem` principalScopes principal) (NonEmpty.toList requiredScopes)
          then Authorized
          else Forbidden MissingRequiredScopes

-- | Authorization policy selected after a current principal is established.
--
-- 'AuthenticationWithAuthorization' retains the original scoped-authorization
-- contract. 'AuthenticationWithoutAuthorization' is for applications whose
-- endpoint algebra contains only anonymous and authenticated routes; if a
-- scoped route is later introduced without selecting an interpreter, it fails
-- closed through the application's explicit unavailable response rather than
-- manufacturing an impossible denial value.
data AuthenticationAuthorization route context authorization principal denial
  = AuthenticationWithAuthorization
      (AuthorizationInterpreter principal authorization denial)
      (denial -> SecurityFailureCode)
      (EndpointRequest route context authorization -> denial -> NonPageResponse route context)
  | AuthenticationWithoutAuthorization
      (EndpointRequest route context authorization -> NonPageResponse route context)

-- | The complete application-selected proof-to-principal pipeline. A
-- challenge renderer receives the classified failure but is responsible for
-- returning only the protocol-appropriate generic public response.
data AuthenticationPipeline route context authorization proof verified principal denial = AuthenticationPipeline
  { authenticationProofExtractor :: AuthenticationProofExtractor route context authorization proof,
    authenticationProofVerifier :: AuthenticationProofVerifier proof verified,
    authenticationPrincipalEstablisher :: PrincipalEstablisher verified principal,
    authenticationAuthorization :: AuthenticationAuthorization route context authorization principal denial,
    authenticationAttachPrincipal :: principal -> context -> context,
    authenticationChallenge :: EndpointRequest route context authorization -> AuthenticationFailure -> NonPageResponse route context,
    authenticationUnavailable :: EndpointRequest route context authorization -> AuthenticationDependency -> NonPageResponse route context
  }

-- | Assemble one configured authentication guard. The root's
-- 'AuthenticationEnabled' choice controls where this guard occurs relative
-- to application-specific before/after guards.
authenticationGuardFromPipeline :: AuthenticationPipeline route context authorization proof verified principal denial -> AuthenticationGuard route context authorization
authenticationGuardFromPipeline pipeline = AuthenticationGuard (runAuthenticationPipeline pipeline)

-- | Preserve an application's ordinary authentication challenge for a page or
-- protocol endpoint, but turn an action guard's pre-handler 401 into Harch's
-- one retained-action marker.  The browser runtime recognizes only this
-- framework-owned response, so authorization, CSRF, validation, and handler
-- failures cannot be mistaken for a reauthentication invitation.
authenticationChallengeForAction :: EndpointRequest route context authorization -> NonPageResponse route context -> NonPageResponse route context
authenticationChallengeForAction endpointRequest ordinaryChallenge =
  case endpointDispatchKind endpointRequest of
    EndpointClientAction -> NonPageClientActionBodyResponse clientActionReauthenticationRequiredResponse
    _ -> ordinaryChallenge

-- | Run the pipeline against the selected endpoint. Anonymous endpoints may
-- enrich their context when proof succeeds, but every presented-proof failure
-- continues anonymously; protected endpoints interpret the same failure once
-- at this boundary and halt before handler admission.
runAuthenticationPipeline :: AuthenticationPipeline route context authorization proof verified principal denial -> EndpointRequest route context authorization -> IO (EndpointGuardResult route context)
runAuthenticationPipeline pipeline endpointRequest =
  case endpointAccess (endpointMetadata endpointRequest) of
    AllowUnauthenticated -> runAnonymousPipeline pipeline endpointRequest
    RequireAuthenticated -> runProtectedPipeline pipeline endpointRequest Nothing
    RequireAuthorized authorization -> runProtectedPipeline pipeline endpointRequest (Just authorization)

runAnonymousPipeline :: AuthenticationPipeline route context authorization proof verified principal denial -> EndpointRequest route context authorization -> IO (EndpointGuardResult route context)
runAnonymousPipeline pipeline endpointRequest = do
  result <- runExceptT (establishPipelinePrincipal pipeline endpointRequest)
  emitAuthenticationEvaluation endpointRequest (authenticationEventFromResult result)
  pure $
    case result of
      Right principal -> ContinueEndpoint (authenticationAttachPrincipal pipeline principal (requestContext (endpointRouteRequest endpointRequest)))
      Left _ -> ContinueEndpoint (requestContext (endpointRouteRequest endpointRequest))

runProtectedPipeline :: AuthenticationPipeline route context authorization proof verified principal denial -> EndpointRequest route context authorization -> Maybe authorization -> IO (EndpointGuardResult route context)
runProtectedPipeline pipeline endpointRequest maybeAuthorization = do
  authenticationResult <- runExceptT (establishPipelinePrincipal pipeline endpointRequest)
  emitAuthenticationEvaluation endpointRequest (authenticationEventFromResult authenticationResult)
  case authenticationResult of
    Left authenticationFailure -> pure (haltAuthenticationFailure pipeline endpointRequest authenticationFailure)
    Right principal ->
      case maybeAuthorization of
        Nothing -> pure (ContinueEndpoint (authenticationAttachPrincipal pipeline principal (requestContext (endpointRouteRequest endpointRequest))))
        Just authorization ->
          case authenticationAuthorization pipeline of
            AuthenticationWithAuthorization interpreter denialFailureCode forbidden ->
              case authorizePrincipal interpreter principal authorization of
                Authorized -> pure (ContinueEndpoint (authenticationAttachPrincipal pipeline principal (requestContext (endpointRouteRequest endpointRequest))))
                Forbidden denial -> do
                  emitAuthorizationDenial endpointRequest (denialFailureCode denial)
                  pure (HaltEndpoint (forbidden endpointRequest denial))
            AuthenticationWithoutAuthorization unexpectedAuthorization ->
              pure (HaltEndpoint (unexpectedAuthorization endpointRequest))

authenticationEventFromResult :: Either AuthenticationFailure principal -> AuthenticationEvent
authenticationEventFromResult result =
  case result of
    Right _ -> AuthenticationEvent AuthenticationEstablished Nothing
    Left authenticationFailure -> authenticationEventFromFailure authenticationFailure

authenticationEventFromFailure :: AuthenticationFailure -> AuthenticationEvent
authenticationEventFromFailure authenticationFailure =
  case authenticationFailure of
    ProofMissing -> AuthenticationEvent AuthenticationMissing Nothing
    ProofExtractionRejected _ -> AuthenticationEvent AuthenticationRejected Nothing
    ProofVerificationRejected (ProofRejection failureCode) -> AuthenticationEvent AuthenticationRejected (Just failureCode)
    PrincipalEstablishmentRejected (PrincipalRejection failureCode) -> AuthenticationEvent AuthenticationRejected (Just failureCode)
    AuthenticationUnavailable (AuthenticationDependency failureCode) -> AuthenticationEvent AuthenticationDependencyUnavailable (Just failureCode)

emitAuthenticationEvaluation :: EndpointRequest route context authorization -> AuthenticationEvent -> IO ()
emitAuthenticationEvaluation endpointRequest authenticationEvent =
  emitSecurityEventBody endpointRequest (AuthenticationEvaluated authenticationEvent)

emitAuthorizationDenial :: EndpointRequest route context authorization -> SecurityFailureCode -> IO ()
emitAuthorizationDenial endpointRequest failureCode =
  emitSecurityEventBody endpointRequest (AuthorizationDenied (AuthorizationEvent failureCode))

emitSecurityEventBody :: EndpointRequest route context authorization -> SecurityEvent -> IO ()
emitSecurityEventBody endpointRequest securityEvent =
  traverse_
    (\securityEventSink -> void (emitSecurityEvent securityEventSink TelemetryBestEffort securityEvent))
    (endpointSecurityEventSink endpointRequest)

establishPipelinePrincipal :: AuthenticationPipeline route context authorization proof verified principal denial -> EndpointRequest route context authorization -> ExceptT AuthenticationFailure IO principal
establishPipelinePrincipal pipeline endpointRequest = do
  maybeProof <- liftAuthenticationEither ProofExtractionRejected (extractAuthenticationProof (authenticationProofExtractor pipeline) endpointRequest)
  proof <- maybe (throwError ProofMissing) pure maybeProof
  verified <- do
    verificationResult <- liftIO (verifyAuthenticationProof (authenticationProofVerifier pipeline) proof)
    liftAuthenticationEither verificationFailure verificationResult
  establishmentResult <- liftIO (establishPrincipal (authenticationPrincipalEstablisher pipeline) verified)
  liftAuthenticationEither establishmentFailure establishmentResult

verificationFailure :: ProofVerificationFailure -> AuthenticationFailure
verificationFailure verificationFailureValue =
  case verificationFailureValue of
    ProofRejected rejection -> ProofVerificationRejected rejection
    ProofVerificationUnavailable dependency -> AuthenticationUnavailable dependency

establishmentFailure :: PrincipalEstablishmentFailure -> AuthenticationFailure
establishmentFailure establishmentFailureValue =
  case establishmentFailureValue of
    PrincipalRejected rejection -> PrincipalEstablishmentRejected rejection
    PrincipalEstablishmentUnavailable dependency -> AuthenticationUnavailable dependency

liftAuthenticationEither :: (sourceError -> AuthenticationFailure) -> Either sourceError value -> ExceptT AuthenticationFailure IO value
liftAuthenticationEither mapError result =
  case result of
    Left sourceError -> throwError (mapError sourceError)
    Right value -> pure value

haltAuthenticationFailure :: AuthenticationPipeline route context authorization proof verified principal denial -> EndpointRequest route context authorization -> AuthenticationFailure -> EndpointGuardResult route context
haltAuthenticationFailure pipeline endpointRequest authenticationFailureValue =
  case authenticationFailureValue of
    AuthenticationUnavailable dependency -> HaltEndpoint (authenticationUnavailable pipeline endpointRequest dependency)
    _ -> HaltEndpoint (authenticationChallenge pipeline endpointRequest authenticationFailureValue)

combineProofExtractors :: NonEmpty (AuthenticationProofExtractor route context authorization proof) -> AuthenticationProofExtractor route context authorization proof
combineProofExtractors extractors =
  AuthenticationProofExtractor $ \endpointRequest -> do
    extractedProofs <- traverse (`extractAuthenticationProof` endpointRequest) (NonEmpty.toList extractors)
    case catMaybes extractedProofs of
      [] -> Right Nothing
      [proof] -> Right (Just proof)
      _ -> Left ProofAmbiguous

-- | Extract one bounded browser-session JWT from the configured cookie. A
-- duplicate cookie is ambiguous rather than precedence-selected.
cookieJwtExtractor :: AuthenticationCookieName -> AuthenticationProofMaximumBytes -> AuthenticationProofExtractor route context authorization EncodedJwt
cookieJwtExtractor (AuthenticationCookieName cookieName) maximumBytes =
  AuthenticationProofExtractor $ \endpointRequest ->
    do
      matchingValues <- concat . concat <$> traverse matchingCookieValues (cookieHeaders endpointRequest)
      extractBoundedJwt maximumBytes (: []) matchingValues
  where
    cookieHeaders request =
      [ rawValue
      | (headerName, rawValue) <- Wai.requestHeaders (endpointWaiRequest request),
        headerName == "Cookie"
      ]
    matchingCookieValues rawHeader = traverse matchingCookieValue (ByteString.split 59 rawHeader)
    matchingCookieValue rawCookie =
      let strippedCookie = ByteString.dropWhile (== 32) rawCookie
          (cookieKey, cookieValueWithSeparator) = ByteString.break (== 61) strippedCookie
       in if cookieKey /= cookieName
            then Right []
            else case ByteString.uncons cookieValueWithSeparator of
              Nothing -> Left ProofMalformed
              Just (_, cookieValue)
                | ByteString.null cookieValue -> Left ProofMalformed
                | otherwise -> Right [cookieValue]

-- | Extract one bounded bearer JWT. Any Authorization header that is present
-- but not exactly one well-formed @Bearer <token>@ value is a malformed proof;
-- API applications therefore never fall back to a browser cookie by accident.
bearerJwtExtractor :: AuthenticationProofMaximumBytes -> AuthenticationProofExtractor route context authorization EncodedJwt
bearerJwtExtractor maximumBytes =
  AuthenticationProofExtractor $ \endpointRequest ->
    let authorizationValues =
          [ rawValue
          | (headerName, rawValue) <- Wai.requestHeaders (endpointWaiRequest endpointRequest),
            headerName == "Authorization"
          ]
     in case authorizationValues of
          [] -> Right Nothing
          [authorizationValue] ->
            case ByteString.stripPrefix "Bearer " authorizationValue of
              Just rawJwt
                | not (ByteString.null rawJwt) && not (ByteString.elem 32 rawJwt) -> extractJwtValue maximumBytes rawJwt
              _ -> Left ProofMalformed
          _ -> Left ProofAmbiguous

extractBoundedJwt :: AuthenticationProofMaximumBytes -> (ByteString -> [ByteString]) -> [ByteString] -> Either ProofExtractionFailure (Maybe EncodedJwt)
extractBoundedJwt (AuthenticationProofMaximumBytes maximumBytes) selectValues headers =
  case concatMap selectValues headers of
    [] -> Right Nothing
    [rawJwt]
      | ByteString.length rawJwt > maximumBytes -> Left ProofTooLarge
      | otherwise -> Right (Just (EncodedJwt rawJwt))
    _ -> Left ProofAmbiguous

extractJwtValue :: AuthenticationProofMaximumBytes -> ByteString -> Either ProofExtractionFailure (Maybe EncodedJwt)
extractJwtValue maximumBytes rawJwt = extractBoundedJwt maximumBytes (: []) [rawJwt]
