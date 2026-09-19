{-# LANGUAGE OverloadedStrings #-}

-- | Application-owned RS256 account-session JWT admission.
--
-- This public facade owns application meaning after a compact proof is
-- verified: it preserves the cookie-or-bearer source, parses minimal account
-- claims, and resolves the durable session before a principal reaches the
-- request context. 'WebApi.AccountJwt.Runtime' owns the explicit startup
-- configuration, key proof, and token-issuance lifecycle.
--
-- Decision (AHI-4D-MH1, 2026-09-19): keep the established public API at this
-- module and Harch's generic JWT verification boundary. Moving the cohesive
-- configuration-to-runtime concern behind explicit inputs and outputs keeps
-- source-to-context admission together and does not introduce another
-- credential parser.
module WebApi.AccountJwt
  ( AccountJwtClaims,
    AccountJwtConfiguration,
    AccountJwtRawConfiguration (..),
    AccountJwtSignerBuilder,
    AccountJwtConfigurationError (..),
    AccountJwtIssueError (..),
    AccountJwtIssuer (..),
    AccountJwtLoadError (..),
    AccountJwtRuntime,
    SharedJwtIssuance (..),
    accountAuthenticationChallenge,
    accountJwtAuthenticationPipeline,
    accountJwtIssuerFromRuntime,
    accountJwtRuntimeProofExtractor,
    accountJwtRuntimeProofVerifier,
    accountJwtRuntimeSharedIssuance,
    authenticationErrorResponse,
    establishAccountPrincipal,
    loadAccountJwtRuntime,
    loadAccountJwtRuntimeWithSigner,
    mkAccountJwtConfiguration,
    parseAccountJwtClaims,
    unavailableAccountJwtIssuer,
  )
where

import Control.Lens (preview, (^.))
import Crypto.JWT qualified as Jwt
import Data.Text (Text)
import HarchWeb qualified
import HarchWeb.Account qualified as Account
import HarchWeb.Session (OpaqueSession (..), SessionId, mkSessionId)
import HarchWeb.Session qualified as Session
import HarchWeb.Time (UnixTimeNanoseconds)
import Network.HTTP.Types qualified as Http
import WebApi.AccountJwt.Runtime
import WebApi.AccountPrincipal (AccountPrincipal, mkAccountPrincipal)
import WebApi.Route
  ( AppAuthorization,
    AppRequestContext (..),
    AppRoute (LoginRoute),
    RequestAuthenticationTransport (..),
  )
import WebApi.Session (AccountSessionStore (..))

-- | Construct the root authentication rail from the immutable startup
-- runtime. A successful signature is only an intermediate fact: this
-- establishment step resolves the current durable session and checks both its
-- subject and expiration before a principal reaches the request context.
accountJwtAuthenticationPipeline :: AccountSessionStore -> IO UnixTimeNanoseconds -> AccountJwtRuntime -> HarchWeb.AuthenticationPipeline AppRoute AppRequestContext AppAuthorization HarchWeb.JwtProof (HarchWeb.JwtProofSource, AccountJwtClaims) (HarchWeb.JwtProofSource, AccountPrincipal) ()
accountJwtAuthenticationPipeline sessionStore readClock runtime =
  HarchWeb.AuthenticationPipeline
    { HarchWeb.authenticationProofExtractor = accountJwtRuntimeProofExtractor runtime,
      HarchWeb.authenticationProofVerifier =
        HarchWeb.AuthenticationProofVerifier $ \proof -> do
          verified <- HarchWeb.verifyAuthenticationProof (accountJwtRuntimeProofVerifier runtime parseAccountJwtClaims) proof
          pure ((HarchWeb.jwtProofSource proof,) <$> verified),
      HarchWeb.authenticationPrincipalEstablisher =
        HarchWeb.PrincipalEstablisher $ \(source, claims) -> do
          established <- HarchWeb.establishPrincipal (establishAccountPrincipal sessionStore readClock) claims
          pure ((source,) <$> established),
      HarchWeb.authenticationAuthorization =
        HarchWeb.AuthenticationWithoutAuthorization
          (\endpointRequest -> authenticationErrorResponse (HarchWeb.requestContext (HarchWeb.endpointRouteRequest endpointRequest)) Http.status503 "Authorization is not configured for this application."),
      HarchWeb.authenticationAttachPrincipal = \(source, principal) context ->
        context
          { requestAccountPrincipal = Just principal,
            requestAuthenticationTransport = requestTransport source
          },
      HarchWeb.authenticationChallenge = accountAuthenticationChallenge,
      HarchWeb.authenticationUnavailable = \endpointRequest _ -> authenticationErrorResponse (HarchWeb.requestContext (HarchWeb.endpointRouteRequest endpointRequest)) Http.status503 "Authentication is temporarily unavailable."
    }

requestTransport :: HarchWeb.JwtProofSource -> RequestAuthenticationTransport
requestTransport source =
  case source of
    HarchWeb.JwtProofFromCookie -> AccountJwtFromCookie
    HarchWeb.JwtProofFromBearer -> AccountJwtFromBearer
    HarchWeb.JwtProofFromCookieAndBearer -> AccountJwtFromCookieAndBearer

accountAuthenticationChallenge :: HarchWeb.EndpointRequest AppRoute AppRequestContext AppAuthorization -> HarchWeb.AuthenticationFailure -> HarchWeb.NonPageResponse AppRoute AppRequestContext
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

-- | Preserve the application's text-body ownership while making its two
-- authentication infrastructure failures support-correlatable on the Harch
-- request rail. Pure pipeline tests may deliberately provide no correlation
-- value; WAI ingress always supplies one. Other application error surfaces and
-- audit joins remain AHI-5-RID follow-up work.
authenticationErrorResponse :: AppRequestContext -> Http.Status -> Text -> HarchWeb.NonPageResponse AppRoute AppRequestContext
authenticationErrorResponse requestContext status message =
  HarchWeb.NonPageBodyResponse
    HarchWeb.ResponseBody
      { HarchWeb.responseStatus = status,
        HarchWeb.responseContentType = "text/plain; charset=utf-8",
        HarchWeb.responseBody = messageWithRequestId requestContext message,
        HarchWeb.responseObservabilityAttributes = [],
        HarchWeb.responseLogEntries = [],
        HarchWeb.responseDatabaseOperations = []
      }

messageWithRequestId :: AppRequestContext -> Text -> Text
messageWithRequestId requestContext message =
  case requestCorrelationId requestContext of
    Nothing -> message
    Just requestId -> message <> " Request ID: " <> HarchWeb.requestIdText requestId <> "."

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
  HarchWeb.mkJwtClaimsError
    (HarchWeb.requiredSecurityFailureCodeOrDie "account.jwt.claims-rejected")

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
knownAccountSessionRejected = HarchWeb.requiredSecurityFailureCodeOrDie "account.jwt.session-rejected"

knownAccountSessionUnavailable :: HarchWeb.SecurityFailureCode
knownAccountSessionUnavailable = HarchWeb.requiredSecurityFailureCodeOrDie "account.jwt.session-unavailable"
