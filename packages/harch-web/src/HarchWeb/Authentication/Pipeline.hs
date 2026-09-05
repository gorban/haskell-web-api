{-# LANGUAGE LambdaCase #-}

-- | Authentication proof-to-principal and authorization orchestration.
--
-- Decision record (PR-F6, 2026-09-05): this internal owner consumes the one
-- typed extractor from 'HarchWeb.Authentication.Transport' and retains the
-- existing 'ExceptT' railway for expected extraction, verification, and
-- principal-establishment outcomes. Transport policy deliberately stays out
-- of this module; applications retain authorization policy and challenge
-- rendering. The split preserves one post-match guard rather than creating a
-- parallel security pipeline.
module HarchWeb.Authentication.Pipeline
  ( AccessFailure (..),
    AuthenticationAuthorization (..),
    AuthenticationDependency,
    AuthenticationFailure (..),
    AuthenticationPipeline (..),
    AuthenticationProofVerifier (..),
    AuthorizationDecision (..),
    AuthorizationInterpreter (..),
    PrincipalEstablisher (..),
    PrincipalEstablishmentFailure (..),
    PrincipalRejection,
    ProofRejection,
    ProofVerificationFailure (..),
    ScopeAuthorizationDenial (..),
    ScopeRequirement (..),
    SecurityFailureCode,
    authenticationChallengeForAction,
    authenticationGuardFromPipeline,
    mkAuthenticationDependency,
    mkPrincipalRejection,
    mkProofRejection,
    mkSecurityFailureCode,
    requiredSecurityFailureCodeOrDie,
    runAuthenticationPipeline,
    scopeAuthorizationInterpreter,
  )
where

import Control.Monad (void)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Either (fromRight)
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import HarchWeb.Authentication.Transport (AuthenticationProofExtractor (..), ProofExtractionFailure)
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

newtype ProofRejection = ProofRejection SecurityFailureCode deriving (Eq, Show)

newtype PrincipalRejection = PrincipalRejection SecurityFailureCode deriving (Eq, Show)

newtype AuthenticationDependency = AuthenticationDependency SecurityFailureCode deriving (Eq, Show)

mkProofRejection :: SecurityFailureCode -> ProofRejection
mkProofRejection = ProofRejection

mkPrincipalRejection :: SecurityFailureCode -> PrincipalRejection
mkPrincipalRejection = PrincipalRejection

mkAuthenticationDependency :: SecurityFailureCode -> AuthenticationDependency
mkAuthenticationDependency = AuthenticationDependency

requiredSecurityFailureCodeOrDie :: Text -> SecurityFailureCode
requiredSecurityFailureCodeOrDie value = fromRight (error "invalid security failure-code declaration") (mkSecurityFailureCode value)

data ProofVerificationFailure = ProofRejected ProofRejection | ProofVerificationUnavailable AuthenticationDependency deriving (Eq, Show)

data PrincipalEstablishmentFailure = PrincipalRejected PrincipalRejection | PrincipalEstablishmentUnavailable AuthenticationDependency deriving (Eq, Show)

data AuthenticationFailure
  = ProofMissing
  | ProofExtractionRejected ProofExtractionFailure
  | ProofVerificationRejected ProofRejection
  | PrincipalEstablishmentRejected PrincipalRejection
  | AuthenticationUnavailable AuthenticationDependency
  deriving (Eq, Show)

data AccessFailure denial = Unauthenticated AuthenticationFailure | AccessForbidden denial | AccessUnavailable AuthenticationDependency deriving (Eq, Show)

newtype AuthenticationProofVerifier proof verified = AuthenticationProofVerifier
  {verifyAuthenticationProof :: proof -> IO (Either ProofVerificationFailure verified)}

newtype PrincipalEstablisher verified principal = PrincipalEstablisher
  {establishPrincipal :: verified -> IO (Either PrincipalEstablishmentFailure principal)}

data AuthorizationDecision denial = Authorized | Forbidden denial deriving (Eq, Show)

newtype AuthorizationInterpreter principal authorization denial = AuthorizationInterpreter
  {authorizePrincipal :: principal -> authorization -> AuthorizationDecision denial}

data ScopeRequirement scope = RequireAllScopes (NonEmpty scope) | RequireAnyScope (NonEmpty scope) deriving (Eq, Show)

data ScopeAuthorizationDenial = MissingRequiredScopes deriving (Eq, Show)

scopeAuthorizationInterpreter :: (Eq scope) => (principal -> [scope]) -> AuthorizationInterpreter principal (ScopeRequirement scope) ScopeAuthorizationDenial
scopeAuthorizationInterpreter principalScopes = AuthorizationInterpreter $ \principal requirement ->
  let hasScope = (`elem` principalScopes principal)
   in case requirement of
        RequireAllScopes scopes | all hasScope (NonEmpty.toList scopes) -> Authorized
        RequireAnyScope scopes | any hasScope (NonEmpty.toList scopes) -> Authorized
        _ -> Forbidden MissingRequiredScopes

data AuthenticationAuthorization route context authorization principal denial
  = AuthenticationWithAuthorization
      (AuthorizationInterpreter principal authorization denial)
      (denial -> SecurityFailureCode)
      (EndpointRequest route context authorization -> denial -> NonPageResponse route context)
  | AuthenticationWithoutAuthorization
      (EndpointRequest route context authorization -> NonPageResponse route context)

data AuthenticationPipeline route context authorization proof verified principal denial = AuthenticationPipeline
  { authenticationProofExtractor :: AuthenticationProofExtractor route context authorization proof,
    authenticationProofVerifier :: AuthenticationProofVerifier proof verified,
    authenticationPrincipalEstablisher :: PrincipalEstablisher verified principal,
    authenticationAuthorization :: AuthenticationAuthorization route context authorization principal denial,
    authenticationAttachPrincipal :: principal -> context -> context,
    authenticationChallenge :: EndpointRequest route context authorization -> AuthenticationFailure -> NonPageResponse route context,
    authenticationUnavailable :: EndpointRequest route context authorization -> AuthenticationDependency -> NonPageResponse route context
  }

authenticationGuardFromPipeline :: AuthenticationPipeline route context authorization proof verified principal denial -> AuthenticationGuard route context authorization
authenticationGuardFromPipeline = AuthenticationGuard . runAuthenticationPipeline

authenticationChallengeForAction :: EndpointRequest route context authorization -> NonPageResponse route context -> NonPageResponse route context
authenticationChallengeForAction request ordinary = case endpointDispatchKind request of
  EndpointClientAction -> NonPageClientActionBodyResponse clientActionReauthenticationRequiredResponse
  _ -> ordinary

runAuthenticationPipeline :: AuthenticationPipeline route context authorization proof verified principal denial -> EndpointRequest route context authorization -> IO (EndpointGuardResult route context)
runAuthenticationPipeline pipeline request = case endpointAccess (endpointMetadata request) of
  AllowUnauthenticated -> runAnonymousPipeline pipeline request
  RequireAuthenticated -> runProtectedPipeline pipeline request Nothing
  RequireAuthorized authorization -> runProtectedPipeline pipeline request (Just authorization)

runAnonymousPipeline :: AuthenticationPipeline route context authorization proof verified principal denial -> EndpointRequest route context authorization -> IO (EndpointGuardResult route context)
runAnonymousPipeline pipeline request = do
  result <- runExceptT (establishPipelinePrincipal pipeline request)
  emitAuthenticationEvaluation request (authenticationEventFromResult result)
  pure $ ContinueEndpoint $ either (const context) (\principal -> authenticationAttachPrincipal pipeline principal context) result
  where
    context = requestContext (endpointRouteRequest request)

runProtectedPipeline :: AuthenticationPipeline route context authorization proof verified principal denial -> EndpointRequest route context authorization -> Maybe authorization -> IO (EndpointGuardResult route context)
runProtectedPipeline pipeline request maybeAuthorization = do
  result <- runExceptT (establishPipelinePrincipal pipeline request)
  emitAuthenticationEvaluation request (authenticationEventFromResult result)
  case result of
    Left failure -> pure (haltAuthenticationFailure pipeline request failure)
    Right principal -> continuePrincipal principal
  where
    continue principal =
      ContinueEndpoint
        (authenticationAttachPrincipal pipeline principal (requestContext (endpointRouteRequest request)))
    continuePrincipal principal = case maybeAuthorization of
      Nothing -> pure (continue principal)
      Just authorization -> case authenticationAuthorization pipeline of
        AuthenticationWithAuthorization interpreter code forbidden -> case authorizePrincipal interpreter principal authorization of
          Authorized -> pure (continue principal)
          Forbidden denial -> do
            emitAuthorizationDenial request (code denial)
            pure (HaltEndpoint (forbidden request denial))
        AuthenticationWithoutAuthorization unexpected -> pure (HaltEndpoint (unexpected request))

authenticationEventFromResult :: Either AuthenticationFailure principal -> AuthenticationEvent
authenticationEventFromResult = either authenticationEventFromFailure (const (AuthenticationEvent AuthenticationEstablished Nothing))

authenticationEventFromFailure :: AuthenticationFailure -> AuthenticationEvent
authenticationEventFromFailure failure = case failure of
  ProofMissing -> AuthenticationEvent AuthenticationMissing Nothing
  ProofExtractionRejected _ -> AuthenticationEvent AuthenticationRejected Nothing
  ProofVerificationRejected (ProofRejection code) -> AuthenticationEvent AuthenticationRejected (Just code)
  PrincipalEstablishmentRejected (PrincipalRejection code) -> AuthenticationEvent AuthenticationRejected (Just code)
  AuthenticationUnavailable (AuthenticationDependency code) -> AuthenticationEvent AuthenticationDependencyUnavailable (Just code)

emitAuthenticationEvaluation :: EndpointRequest route context authorization -> AuthenticationEvent -> IO ()
emitAuthenticationEvaluation request = emitSecurityEventBody request . AuthenticationEvaluated

emitAuthorizationDenial :: EndpointRequest route context authorization -> SecurityFailureCode -> IO ()
emitAuthorizationDenial request = emitSecurityEventBody request . AuthorizationDenied . AuthorizationEvent

emitSecurityEventBody :: EndpointRequest route context authorization -> SecurityEvent -> IO ()
emitSecurityEventBody request event = traverse_ (\sink -> void (emitSecurityEvent sink TelemetryBestEffort event)) (endpointSecurityEventSink request)

establishPipelinePrincipal :: AuthenticationPipeline route context authorization proof verified principal denial -> EndpointRequest route context authorization -> ExceptT AuthenticationFailure IO principal
establishPipelinePrincipal pipeline request = do
  maybeProof <- liftAuthenticationEither ProofExtractionRejected (extractAuthenticationProof (authenticationProofExtractor pipeline) request)
  proof <- maybe (throwError ProofMissing) pure maybeProof
  verified <- liftIO (verifyAuthenticationProof (authenticationProofVerifier pipeline) proof) >>= liftAuthenticationEither verificationFailure
  liftIO (establishPrincipal (authenticationPrincipalEstablisher pipeline) verified) >>= liftAuthenticationEither establishmentFailure

verificationFailure :: ProofVerificationFailure -> AuthenticationFailure
verificationFailure = \case
  ProofRejected rejection -> ProofVerificationRejected rejection
  ProofVerificationUnavailable dependency -> AuthenticationUnavailable dependency

establishmentFailure :: PrincipalEstablishmentFailure -> AuthenticationFailure
establishmentFailure = \case
  PrincipalRejected rejection -> PrincipalEstablishmentRejected rejection
  PrincipalEstablishmentUnavailable dependency -> AuthenticationUnavailable dependency

liftAuthenticationEither :: (sourceError -> AuthenticationFailure) -> Either sourceError value -> ExceptT AuthenticationFailure IO value
liftAuthenticationEither mapError = either (throwError . mapError) pure

haltAuthenticationFailure :: AuthenticationPipeline route context authorization proof verified principal denial -> EndpointRequest route context authorization -> AuthenticationFailure -> EndpointGuardResult route context
haltAuthenticationFailure pipeline request = \case
  AuthenticationUnavailable dependency -> HaltEndpoint (authenticationUnavailable pipeline request dependency)
  failure -> HaltEndpoint (authenticationChallenge pipeline request failure)
