{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Typed application-module mount adaptation and construction validation.
module HarchWeb.ApplicationModule.Mount
  ( ActionMount (..),
    AuthorizationProjection (..),
    ModuleMount (..),
    ModuleMountError (..),
    RouteMount (..),
    mountActionCodec,
    mountApplicationModule,
    mountRouteCodec,
  )
where

import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (isJust)
import Data.Text qualified as Text
import HarchWeb.Action (ActionCodec, ActionCodecError, mapActionCodec, mountActionCodecAtPrefix)
import HarchWeb.ApplicationModule.Core (ApplicationModule (..))
import HarchWeb.Document (Page (..))
import HarchWeb.EndpointMetadata
  ( AccessRequirement (..),
    EndpointMetadata (..),
    EndpointMetadataError,
    endpointNameText,
    mkEndpointMetadata,
    mkEndpointName,
    mkRouteTemplate,
    routeTemplateText,
  )
import HarchWeb.EndpointSecurity
  ( EndpointGuard (..),
    EndpointGuardResult (..),
    EndpointRequest (..),
  )
import HarchWeb.Markup (safeUrlText)
import HarchWeb.RequestContext (ContextProjection (..))
import HarchWeb.Routing
  ( PathSegment,
    RouteCodec (..),
    RouteLocation (..),
    RouteMethodPolicy (RouteHidden),
    RouteParseResult (..),
    RouteRequest (..),
    encodeRouteLocation,
  )
import HarchWeb.Routing qualified as Routing
import HarchWeb.SecurityEvent (ModuleName, moduleNameText)
import HarchWeb.Server.Response (ClientActionRequest (..), ClientActionResponse, Response, mapResponsePage)
import HarchWeb.Site (RouteDefinition (..))
import HarchWeb.Site qualified as Site

-- | The action-side analogue of the embedding half of 'RouteMount'.  The
-- root action decoder only maps a child-selected target into its closed
-- algebra; it never receives a parent target to project back.  Consequently
-- this declaration exposes only the two mappings its codec and handler can
-- actually consume, rather than a misleading reverse target projection.
data ActionMount parentTarget parentAction childTarget childAction = ActionMount
  { embedChildActionTarget :: childTarget -> parentTarget,
    embedChildAction :: childAction -> parentAction,
    projectChildAction :: parentAction -> Maybe childAction
  }

-- | An explicit, one-way mapping from a child's declared authorization value
-- into the root application's policy algebra.  Child code cannot select a
-- root authentication configuration or weaken the enclosing policy.
newtype AuthorizationProjection parent child = AuthorizationProjection
  { projectChildAuthorization :: child -> parent
  }

-- | The typed route branch owned by a mounted module. 'embedChildRoute' and
-- 'projectChildRoute' form the usual prism law:
--
-- @projectChildRoute (embedChildRoute child) == Just child@.
--
-- The non-empty structured prefix is never parsed by the child, and it is
-- never represented as a raw @Text@ route fragment.
data RouteMount parent child = RouteMount
  { routeMountName :: ModuleName,
    routeMountPrefix :: NonEmpty PathSegment,
    embedChildRoute :: child -> parent,
    projectChildRoute :: parent -> Maybe child
  }

-- | All typed mappings required to adapt a child application module to a
-- parent route/action/context/policy algebra.  This value intentionally has
-- no authentication or listener field: those capabilities remain root-owned.
data ModuleMount parentRoute parentTarget parentAction parentContext parentAuthorization childRoute childTarget childAction childContext childAuthorization = ModuleMount
  { mountedRoutes :: RouteMount parentRoute childRoute,
    mountedActions :: ActionMount parentTarget parentAction childTarget childAction,
    mountedContext :: ContextProjection parentContext childContext,
    mountedAuthorization :: AuthorizationProjection parentAuthorization childAuthorization
  }

-- | Construction failures while adapting a child module's declared endpoint
-- metadata to its parent mount.  These are authored configuration failures,
-- never request outcomes.
data ModuleMountError
  = InvalidMountedEndpointMetadata EndpointMetadataError
  | InvalidMountedActionCodec ActionCodecError
  deriving (Eq, Show)

-- | Adapt a reusable child module into the parent's closed route/action/policy
-- algebra.  This maps definitions, action handling, and child guards as well
-- as the codecs: the result is still consumed by the root's single route and
-- client-action interpreters.  It deliberately does not choose root security
-- or compose sibling modules; those remain root construction decisions.
mountApplicationModule ::
  ModuleMount parentRoute parentTarget parentAction parentContext parentAuthorization childRoute childTarget childAction childContext childAuthorization ->
  ApplicationModule childRoute childTarget childAction childContext childAuthorization ->
  Either ModuleMountError (ApplicationModule parentRoute parentTarget parentAction parentContext parentAuthorization)
mountApplicationModule moduleMount childModule = do
  let routeMount = mountedRoutes moduleMount
      contextProjection = mountedContext moduleMount
      authorizationProjection = mountedAuthorization moduleMount
      actionMount = mountedActions moduleMount
  let metadataMapper = mountedMetadataMapper routeMount authorizationProjection
  traverse_ (metadataMapper . routeMetadata . moduleEndpoints childModule) (moduleDeclaredRoutes childModule)
  mountedActionCodec <-
    either (Left . InvalidMountedActionCodec) Right $
      mountActionCodecAtPrefix
        (routeMountPrefix routeMount)
        (moduleNameText (routeMountName routeMount))
        (embedChildActionTarget actionMount)
        (projectRequestContext contextProjection)
        (projectChildAuthorization authorizationProjection)
        (embedChildAction actionMount)
        (moduleActionCodec childModule)
  pure
    ApplicationModule
      { moduleName = routeMountName routeMount,
        moduleOwnsRoute = isJust . projectChildRoute routeMount,
        moduleRouteMountChain = \parentRoute ->
          case projectChildRoute routeMount parentRoute of
            Nothing -> error "attempted to select a route observation through a mount that does not own it"
            Just childRoute -> routeMountName routeMount NonEmpty.:| NonEmpty.toList (moduleRouteMountChain childModule childRoute),
        moduleRouteCodec =
          mountRouteCodec
            routeMount
            contextProjection
            ( \parentContext ->
                RouteRequest
                  { requestRoute = embedChildRoute routeMount (requestRoute (notFoundRequest (moduleRouteCodec childModule) (projectRequestContext contextProjection parentContext))),
                    requestContext = parentContext
                  }
            )
            (moduleRouteCodec childModule),
        moduleDeclaredRoutes = map (embedChildRoute routeMount) (moduleDeclaredRoutes childModule),
        moduleEndpoints = mountRouteDefinition routeMount contextProjection metadataMapper (moduleEndpoints childModule),
        moduleActionCodec = mountedActionCodec,
        moduleHandleAction = mountActionHandler actionMount contextProjection (moduleHandleAction childModule),
        moduleGuards =
          map
            (mountEndpointGuard routeMount contextProjection (moduleEndpoints childModule))
            (moduleGuards childModule)
      }

-- | Build the metadata mapper once at module construction.  The mount prefix
-- becomes part of the low-cardinality declared route template, while the
-- mount name namespaces a child endpoint identity without accepting request
-- text as a metric label.
mountedMetadataMapper ::
  RouteMount parentRoute childRoute ->
  AuthorizationProjection parentAuthorization childAuthorization ->
  EndpointMetadata childAuthorization ->
  Either ModuleMountError (EndpointMetadata parentAuthorization)
mountedMetadataMapper routeMount (AuthorizationProjection projectAuthorization) metadata =
  let mountNamePrefix = moduleNameText (routeMountName routeMount)
      mountPath = safeUrlText (encodeRouteLocation (RouteLocation (NonEmpty.toList (routeMountPrefix routeMount)) []))
   in case (mkEndpointName (mountNamePrefix <> "." <> endpointNameText (endpointName metadata)), mkRouteTemplate (mountPath <> childSuffix (routeTemplateText (endpointRouteTemplate metadata)))) of
        (Right mountedName, Right mountedTemplate) ->
          Right
            ( mkEndpointMetadata
                mountedName
                mountedTemplate
                (endpointProtocol metadata)
                (mapAccessRequirement projectAuthorization (endpointAccess metadata))
            )
        (Left endpointError, _) -> Left (InvalidMountedEndpointMetadata endpointError)
        (_, Left routeError) -> Left (InvalidMountedEndpointMetadata routeError)
  where
    childSuffix childTemplate
      | childTemplate == "/" = Text.empty
      | otherwise = childTemplate

mapAccessRequirement ::
  (childAuthorization -> parentAuthorization) ->
  AccessRequirement childAuthorization ->
  AccessRequirement parentAuthorization
mapAccessRequirement projectAuthorization requirement =
  case requirement of
    AllowUnauthenticated -> AllowUnauthenticated
    RequireAuthenticated -> RequireAuthenticated
    RequireAuthorized authorization -> RequireAuthorized (projectAuthorization authorization)

mountRouteDefinition ::
  RouteMount parentRoute childRoute ->
  ContextProjection parentContext childContext ->
  (EndpointMetadata childAuthorization -> Either ModuleMountError (EndpointMetadata parentAuthorization)) ->
  (childRoute -> RouteDefinition childRoute childContext childAuthorization) ->
  parentRoute ->
  RouteDefinition parentRoute parentContext parentAuthorization
mountRouteDefinition routeMount contextProjection mapMetadata childDefinitions parentRoute =
  case projectChildRoute routeMount parentRoute of
    Nothing -> error "attempted to select a route definition through a mount that does not own it"
    Just childRoute ->
      let childDefinition = childDefinitions childRoute
       in RouteDefinition
            { routeNavigationLabel = routeNavigationLabel childDefinition,
              routeMetadata = requiredMountedMetadata (mapMetadata (routeMetadata childDefinition)),
              routeMethods = Site.routeMethods childDefinition,
              routeExecutionPolicy = routeExecutionPolicy childDefinition,
              routeResponse = \request parentRequest -> do
                childResponse <-
                  routeResponse
                    childDefinition
                    request
                    RouteRequest
                      { requestRoute = childRoute,
                        requestContext = projectRequestContext contextProjection (requestContext parentRequest)
                      }
                pure (mapChildResponse routeMount (requestContext parentRequest) childResponse)
            }

requiredMountedMetadata :: Either ModuleMountError metadata -> metadata
requiredMountedMetadata mountedMetadata =
  case mountedMetadata of
    Right metadata -> metadata
    Left mountError -> error ("undeclared endpoint metadata used by an application module: " <> show mountError)

mapChildResponse :: RouteMount parentRoute childRoute -> parentContext -> Response childRoute childContext -> Response parentRoute parentContext
mapChildResponse routeMount parentContext =
  mapResponsePage (mapChildPage routeMount parentContext)

mapChildPage :: RouteMount parentRoute childRoute -> parentContext -> Page childRoute childContext -> Page parentRoute parentContext
mapChildPage routeMount parentContext childPage =
  childPage
    { pageRoute = embedChildRoute routeMount (pageRoute childPage),
      pageContext = parentContext
    }

mountActionHandler ::
  ActionMount parentTarget parentAction childTarget childAction ->
  ContextProjection parentContext childContext ->
  (ClientActionRequest childAction childContext -> IO (Maybe ClientActionResponse)) ->
  ClientActionRequest parentAction parentContext ->
  IO (Maybe ClientActionResponse)
mountActionHandler actionMount contextProjection childHandler parentRequest =
  case projectChildAction actionMount (clientAction parentRequest) of
    Nothing -> pure Nothing
    Just childAction ->
      childHandler
        ClientActionRequest
          { clientAction = childAction,
            clientActionRequestIdempotencyKey = clientActionRequestIdempotencyKey parentRequest,
            clientActionContext = projectRequestContext contextProjection (clientActionContext parentRequest)
          }

mountEndpointGuard ::
  RouteMount parentRoute childRoute ->
  ContextProjection parentContext childContext ->
  (childRoute -> RouteDefinition childRoute childContext childAuthorization) ->
  EndpointGuard childRoute childContext childAuthorization ->
  EndpointGuard parentRoute parentContext parentAuthorization
mountEndpointGuard routeMount contextProjection childDefinitions (EndpointGuard childGuard) =
  EndpointGuard $ \parentRequest ->
    case projectChildRoute routeMount (requestRoute (endpointRouteRequest parentRequest)) of
      Nothing -> pure (ContinueEndpoint (requestContext (endpointRouteRequest parentRequest)))
      Just childRoute -> do
        childResult <-
          childGuard
            EndpointRequest
              { endpointWaiRequest = endpointWaiRequest parentRequest,
                endpointRouteRequest =
                  RouteRequest
                    { requestRoute = childRoute,
                      requestContext = projectRequestContext contextProjection (requestContext (endpointRouteRequest parentRequest))
                    },
                endpointMetadata = routeMetadata (childDefinitions childRoute),
                endpointSecurityEventSink = endpointSecurityEventSink parentRequest,
                endpointDispatchKind = endpointDispatchKind parentRequest
              }
        case childResult of
          ContinueEndpoint _ -> pure (ContinueEndpoint (requestContext (endpointRouteRequest parentRequest)))
          HaltEndpoint childResponse -> pure (HaltEndpoint (mapChildResponse routeMount (requestContext (endpointRouteRequest parentRequest)) childResponse))

-- | Adapt a validated child action codec to its parent algebra.  The mapped
-- codec remains in the root's one client-action interpreter: this function
-- does not parse a request body or invoke a handler.
mountActionCodec ::
  ActionMount parentTarget parentAction childTarget childAction ->
  ContextProjection parentContext childContext ->
  AuthorizationProjection parentAuthorization childAuthorization ->
  ActionCodec childTarget childContext childAuthorization childAction ->
  ActionCodec parentTarget parentContext parentAuthorization parentAction
mountActionCodec ActionMount {embedChildActionTarget, embedChildAction} (ContextProjection projectContext) (AuthorizationProjection projectAuthorization) =
  mapActionCodec embedChildActionTarget projectContext projectAuthorization embedChildAction

-- | Adapt one child's structured codec to a parent's route algebra. A parent
-- route outside this mount is an ordinary ownership miss during rendering and
-- must be rendered by the parent/family codec that owns it; attempting to use
-- this mount's renderer for it is an authored composition error. Parsing a
-- malformed child location remains 'RouteMalformed' and therefore cannot fall
-- through to a later sibling mount.
mountRouteCodec ::
  RouteMount parent child ->
  ContextProjection parentContext childContext ->
  (parentContext -> RouteRequest parent parentContext) ->
  RouteCodec child childContext ->
  RouteCodec parent parentContext
mountRouteCodec RouteMount {routeMountPrefix, embedChildRoute, projectChildRoute} (ContextProjection projectContext) parentNotFound childCodec =
  RouteCodec
    { parseRoute = parseMountedRoute,
      renderRoute = renderMountedRoute,
      notFoundRequest = parentNotFound,
      routeMethods = mountedRouteMethods
    }
  where
    mountPrefix = NonEmpty.toList routeMountPrefix

    parseMountedRoute parentContext location =
      case stripMountPrefix mountPrefix (routePathSegments location) of
        Nothing -> RouteNotMatched
        Just childPathSegments ->
          case parseRoute childCodec (projectContext parentContext) (location {routePathSegments = childPathSegments}) of
            RouteNotMatched -> RouteNotMatched
            RouteMalformed routeError -> RouteMalformed routeError
            RouteParsed childRequest ->
              RouteParsed
                RouteRequest
                  { requestRoute = embedChildRoute (requestRoute childRequest),
                    requestContext = parentContext
                  }

    renderMountedRoute parentRequest =
      case projectChildRoute (requestRoute parentRequest) of
        Nothing -> error "attempted to render a route through a mount that does not own it"
        Just childRoute ->
          let childLocation =
                renderRoute
                  childCodec
                  RouteRequest
                    { requestRoute = childRoute,
                      requestContext = projectContext (requestContext parentRequest)
                    }
           in childLocation {routePathSegments = mountPrefix <> routePathSegments childLocation}

    mountedRouteMethods parentRoute =
      case projectChildRoute parentRoute of
        Nothing -> RouteHidden
        Just childRoute -> Routing.routeMethods childCodec childRoute

stripMountPrefix :: [PathSegment] -> [PathSegment] -> Maybe [PathSegment]
stripMountPrefix prefix segments =
  if prefix `isPrefixOf` segments
    then Just (drop (length prefix) segments)
    else Nothing
  where
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (prefixSegment : remainingPrefix) (segment : remainingSegments) =
      prefixSegment == segment && isPrefixOf remainingPrefix remainingSegments
