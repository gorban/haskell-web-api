-- | Typed endpoint security declarations.
--
-- Decision record (AHI-4A, 2026-09-01): endpoint access extends Harch's one
-- matched-route dispatcher.  'RequestMiddleware' stays pre-route because it
-- owns request-context enrichment before a route exists; this module owns
-- post-match endpoint metadata and may halt with the existing full
-- 'Response'.  Making either concern parse paths would create a competing
-- dispatcher.  The explicit 'ApplicationSecurity' choice also prevents an
-- empty middleware list from silently becoming an authentication policy.
--
-- Decision record (AHI-4D slice 1, 2026-09-12): scoped profile selection
-- extends this post-match rail. Endpoint metadata supplies the most-specific
-- validated profile name, a typed mount supplies a family default, and the
-- root owns the registry and guard implementations. An anonymous profile is
-- non-terminal: a nested endpoint may choose an enabled profile, while an
-- unresolved protected declaration fails construction and is also rejected
-- before any guard runs. This deliberately does not create WAI middleware or
-- a second dispatcher.
module HarchWeb.EndpointSecurity
  ( AccessRequirement (..),
    ApplicationSecurity (..),
    AuthenticationProfile (..),
    AuthenticationProfileConfigurationError (..),
    AuthenticationProfileResolutionError (..),
    AuthenticationProfileName,
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
    authenticationProfileNameText,
    endpointNameText,
    unauthenticatedApplicationGuards,
    beforeAuthenticationGuards,
    authenticationGuard,
    afterAuthenticationGuards,
    mkEndpointMetadata,
    mkAuthenticationProfile,
    mkAuthenticationProfiles,
    mkAuthenticationProfileName,
    mkEndpointName,
    mkRouteTemplate,
    requiredEndpointNameOrDie,
    requiredAuthenticationProfileNameOrDie,
    requiredRouteTemplateOrDie,
    resolveAuthenticationProfile,
    runEndpointGuardPipeline,
    routeTemplateText,
    validateAuthenticationProfileRequirements,
    withAuthenticationProfile,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import HarchWeb.EndpointMetadata
import HarchWeb.Routing (RouteRequest (..))
import HarchWeb.SecurityEvent (SecurityEventSink)
import HarchWeb.Server.Response (NonPageResponse)
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
  | HaltEndpoint (NonPageResponse route context)
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
  | AuthenticationProfiles
      [EndpointGuard route context authorization]
      (NonEmpty (AuthenticationProfile route context authorization))
      AuthenticationProfileName
      [EndpointGuard route context authorization]

-- | One root-installed profile. Mounted modules and endpoint declarations
-- can select this validated name, but they cannot receive its guard, keys, or
-- backing services. A profile without a guard deliberately establishes no
-- identity and is useful as an explicitly public default.
data AuthenticationProfile route context authorization = AuthenticationProfile
  { authenticationProfileName :: AuthenticationProfileName,
    authenticationProfileGuard :: Maybe (AuthenticationGuard route context authorization)
  }

-- | Rejected registry declarations are construction failures. They never
-- become anonymous fallback behavior during request processing.
data AuthenticationProfileConfigurationError
  = DuplicateAuthenticationProfile AuthenticationProfileName
  | MissingDefaultAuthenticationProfile AuthenticationProfileName
  deriving (Eq, Show)

-- | A selected profile cannot be resolved, or a protected endpoint resolves
-- to a profile which does not establish identity.
data AuthenticationProfileResolutionError
  = UnknownAuthenticationProfile AuthenticationProfileName
  | ProtectedEndpointWithoutAuthenticationProfile EndpointName
  deriving (Eq, Show)

mkAuthenticationProfile :: AuthenticationProfileName -> Maybe (AuthenticationGuard route context authorization) -> AuthenticationProfile route context authorization
mkAuthenticationProfile = AuthenticationProfile

-- | Validate a root-owned profile registry. The default belongs to this same
-- registry, so a public root can contain a more-specific enabled profile for
-- a mounted API without creating another dispatcher.
mkAuthenticationProfiles :: [EndpointGuard route context authorization] -> NonEmpty (AuthenticationProfile route context authorization) -> AuthenticationProfileName -> [EndpointGuard route context authorization] -> Either AuthenticationProfileConfigurationError (ApplicationSecurity route context authorization)
mkAuthenticationProfiles before profiles defaultProfile after
  | Just duplicate <- duplicateProfileName (fmap authenticationProfileName (NonEmpty.toList profiles)) = Left (DuplicateAuthenticationProfile duplicate)
  | defaultProfile `notElem` fmap authenticationProfileName (NonEmpty.toList profiles) = Left (MissingDefaultAuthenticationProfile defaultProfile)
  | otherwise = Right (AuthenticationProfiles before profiles defaultProfile after)

duplicateProfileName :: [AuthenticationProfileName] -> Maybe AuthenticationProfileName
duplicateProfileName [] = Nothing
duplicateProfileName (profileName : remaining)
  | profileName `elem` remaining = Just profileName
  | otherwise = duplicateProfileName remaining

-- | Total accessor for the optional guard list of an explicitly public root.
-- Authentication-enabled roots do not have an unauthenticated-only guard
-- phase, so their value is the empty list rather than a partial selector.
unauthenticatedApplicationGuards :: ApplicationSecurity route context authorization -> [EndpointGuard route context authorization]
unauthenticatedApplicationGuards applicationSecurity =
  case applicationSecurity of
    AuthenticationDisabled guards -> guards
    AuthenticationEnabled {} -> []
    AuthenticationProfiles {} -> []

-- | Total accessor for guards preceding a configured authentication guard.
beforeAuthenticationGuards :: ApplicationSecurity route context authorization -> [EndpointGuard route context authorization]
beforeAuthenticationGuards applicationSecurity =
  case applicationSecurity of
    AuthenticationDisabled _ -> []
    AuthenticationEnabled guards _ _ -> guards
    AuthenticationProfiles guards _ _ _ -> guards

-- | A configured authentication guard when one exists.  A root that chose
-- 'AuthenticationDisabled' intentionally has no authentication behavior.
authenticationGuard :: ApplicationSecurity route context authorization -> Maybe (AuthenticationGuard route context authorization)
authenticationGuard applicationSecurity =
  case applicationSecurity of
    AuthenticationDisabled _ -> Nothing
    AuthenticationEnabled _ guard _ -> Just guard
    AuthenticationProfiles {} -> Nothing

-- | Total accessor for guards following authentication.
afterAuthenticationGuards :: ApplicationSecurity route context authorization -> [EndpointGuard route context authorization]
afterAuthenticationGuards applicationSecurity =
  case applicationSecurity of
    AuthenticationDisabled _ -> []
    AuthenticationEnabled _ _ guards -> guards
    AuthenticationProfiles _ _ _ guards -> guards

-- | Resolve endpoint selection after route/mount composition. Legacy roots
-- retain their single existing behavior; only a profile registry accepts an
-- endpoint override. The caller remains the established post-match guard
-- rail, so this does not add a dispatcher.
resolveAuthenticationProfile :: ApplicationSecurity route context authorization -> EndpointMetadata authorization -> Either AuthenticationProfileResolutionError (Maybe (AuthenticationGuard route context authorization))
resolveAuthenticationProfile security metadata =
  case security of
    AuthenticationDisabled _ -> resolveLegacy Nothing
    AuthenticationEnabled _ guard _ -> resolveLegacy (Just guard)
    AuthenticationProfiles _ profiles defaultProfile _ -> do
      let selectedProfile = fromMaybe defaultProfile (endpointAuthenticationProfile metadata)
      profile <- maybe (Left (UnknownAuthenticationProfile selectedProfile)) Right (findProfile selectedProfile (NonEmpty.toList profiles))
      pure (authenticationProfileGuard profile)
  where
    resolveLegacy = Right

findProfile :: AuthenticationProfileName -> [AuthenticationProfile route context authorization] -> Maybe (AuthenticationProfile route context authorization)
findProfile _ [] = Nothing
findProfile name (profile : remaining)
  | authenticationProfileName profile == name = Just profile
  | otherwise = findProfile name remaining

-- | Check every declaration before a profile-enabled application starts.
-- Protected endpoints must resolve to an enabled profile. An anonymous
-- default remains non-terminal: a more-specific mounted or endpoint profile
-- may still resolve to an enabled one. The explicit recursion makes a
-- successful result inspect each preceding declaration before the next one,
-- so construction cannot retain a lazy, unchecked declaration tail.
validateAuthenticationProfileRequirements :: ApplicationSecurity route context authorization -> [EndpointMetadata authorization] -> Either AuthenticationProfileResolutionError ()
validateAuthenticationProfileRequirements security = go
  where
    go [] = Right ()
    go (metadata : remaining) =
      case validateEndpoint metadata of
        Left resolutionError -> Left resolutionError
        Right () -> go remaining
    validateEndpoint metadata = do
      resolvedGuard <- resolveAuthenticationProfile security metadata
      case endpointAccess metadata of
        AllowUnauthenticated -> pure ()
        RequireAuthenticated -> requireGuard metadata resolvedGuard
        RequireAuthorized _ -> requireGuard metadata resolvedGuard
    requireGuard metadata maybeGuard =
      case maybeGuard of
        Just _ -> pure ()
        Nothing -> Left (ProtectedEndpointWithoutAuthenticationProfile (endpointName metadata))
