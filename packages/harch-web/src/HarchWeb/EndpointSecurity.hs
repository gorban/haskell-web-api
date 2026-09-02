-- | Typed endpoint security declarations.
--
-- Decision record (AHI-4A, 2026-09-01): endpoint access extends Harch's one
-- matched-route dispatcher.  'RequestMiddleware' stays pre-route because it
-- owns request-context enrichment before a route exists; this module owns
-- post-match endpoint metadata and may halt with the existing full
-- 'Response'.  Making either concern parse paths would create a competing
-- dispatcher.  The explicit 'ApplicationSecurity' choice also prevents an
-- empty middleware list from silently becoming an authentication policy.
module HarchWeb.EndpointSecurity
  ( AccessRequirement (..),
    ApplicationSecurity (..),
    AuthenticationGuard (..),
    EndpointDispatchKind (..),
    EndpointGuard (..),
    EndpointGuardResult (..),
    EndpointMetadata (..),
    EndpointMetadataError (..),
    EndpointName,
    EndpointProtocol (..),
    EndpointRequest (..),
    RouteTemplate,
    endpointNameText,
    unauthenticatedApplicationGuards,
    beforeAuthenticationGuards,
    authenticationGuard,
    afterAuthenticationGuards,
    mkEndpointMetadata,
    mkEndpointName,
    mkRouteTemplate,
    requiredEndpointNameOrDie,
    requiredRouteTemplateOrDie,
    runEndpointGuardPipeline,
    routeTemplateText,
  )
where

import HarchWeb.EndpointMetadata
import HarchWeb.Routing (RouteRequest (..))
import HarchWeb.SecurityEvent (SecurityEventSink)
import HarchWeb.Server.Response (Response)
import Network.Wai qualified as Wai

-- | The dispatcher-selected protocol form.  These variants retain the same
-- endpoint declaration for synthetic method outcomes, preventing a HEAD,
-- OPTIONS, or 405 request from bypassing a guard.
data EndpointDispatchKind
  = EndpointMatched
  | EndpointMatchedHead
  | EndpointMethodNotAllowed
  | EndpointOptions
  | EndpointClientAction
  deriving (Eq, Show)

data EndpointRequest route context authorization = EndpointRequest
  { endpointWaiRequest :: Wai.Request,
    endpointRouteRequest :: RouteRequest route context,
    endpointMetadata :: EndpointMetadata authorization,
    -- | The optional root-attached sink. A guard receives no route-attribution
    -- constructor, so it can publish only a closed event body for the route
    -- the shared matcher already selected.
    endpointSecurityEventSink :: Maybe SecurityEventSink,
    endpointDispatchKind :: EndpointDispatchKind
  }

-- | A guard may replace context only by continuing. A halted response is final:
-- the dispatcher never observes a replacement context after it, so the type
-- intentionally cannot suggest otherwise.
data EndpointGuardResult route context
  = ContinueEndpoint context
  | HaltEndpoint (Response route context)
  deriving (Eq, Show)

-- | A post-match guard can enrich context or halt but never receives a
-- handler continuation.  The single dispatcher alone invokes the selected
-- handler, exactly once, after every guard has continued.
newtype EndpointGuard route context authorization = EndpointGuard
  { runEndpointGuard :: EndpointRequest route context authorization -> IO (EndpointGuardResult route context)
  }

-- | Run endpoint guards in declaration order.  A continuation receives the
-- latest context; a halt is final and deliberately does not expose a handler
-- continuation to the guard.
runEndpointGuardPipeline :: [EndpointGuard route context authorization] -> EndpointRequest route context authorization -> IO (EndpointGuardResult route context)
runEndpointGuardPipeline = go
  where
    go [] endpointRequest = pure (ContinueEndpoint (requestContext (endpointRouteRequest endpointRequest)))
    go (EndpointGuard runGuard : remainingGuards) endpointRequest = do
      guardResult <- runGuard endpointRequest
      case guardResult of
        HaltEndpoint response -> pure (HaltEndpoint response)
        ContinueEndpoint requestContext ->
          go
            remainingGuards
            ( endpointRequest
                { endpointRouteRequest =
                    (endpointRouteRequest endpointRequest) {requestContext = requestContext}
                }
            )

-- | An authentication guard has the same deliberately restricted execution
-- capability as other endpoint guards but is named separately so a root
-- application's required security selection is visible at construction.
newtype AuthenticationGuard route context authorization = AuthenticationGuard
  { runAuthenticationGuard :: EndpointRequest route context authorization -> IO (EndpointGuardResult route context)
  }

-- | Every root application chooses one security configuration explicitly.
-- Mounted modules will inherit a parent selection in AHI-4B; they never gain
-- a way to replace it with 'AuthenticationDisabled'.
data ApplicationSecurity route context authorization
  = AuthenticationDisabled
      [EndpointGuard route context authorization]
  | AuthenticationEnabled
      [EndpointGuard route context authorization]
      (AuthenticationGuard route context authorization)
      [EndpointGuard route context authorization]

-- | Total accessor for the optional guard list of an explicitly public root.
-- Authentication-enabled roots do not have an unauthenticated-only guard
-- phase, so their value is the empty list rather than a partial selector.
unauthenticatedApplicationGuards :: ApplicationSecurity route context authorization -> [EndpointGuard route context authorization]
unauthenticatedApplicationGuards applicationSecurity =
  case applicationSecurity of
    AuthenticationDisabled guards -> guards
    AuthenticationEnabled {} -> []

-- | Total accessor for guards preceding a configured authentication guard.
beforeAuthenticationGuards :: ApplicationSecurity route context authorization -> [EndpointGuard route context authorization]
beforeAuthenticationGuards applicationSecurity =
  case applicationSecurity of
    AuthenticationDisabled _ -> []
    AuthenticationEnabled guards _ _ -> guards

-- | A configured authentication guard when one exists.  A root that chose
-- 'AuthenticationDisabled' intentionally has no authentication behavior.
authenticationGuard :: ApplicationSecurity route context authorization -> Maybe (AuthenticationGuard route context authorization)
authenticationGuard applicationSecurity =
  case applicationSecurity of
    AuthenticationDisabled _ -> Nothing
    AuthenticationEnabled _ guard _ -> Just guard

-- | Total accessor for guards following authentication.
afterAuthenticationGuards :: ApplicationSecurity route context authorization -> [EndpointGuard route context authorization]
afterAuthenticationGuards applicationSecurity =
  case applicationSecurity of
    AuthenticationDisabled _ -> []
    AuthenticationEnabled _ _ guards -> guards
