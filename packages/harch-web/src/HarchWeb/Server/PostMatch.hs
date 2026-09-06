{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Private post-match endpoint selection and guard execution.
--
-- This is the one collaborator between route classification and the existing
-- dispatcher: it selects action-owner metadata where applicable, attaches
-- route observation, and runs the installed guard rail. It deliberately does
-- not route, read a body, invoke a handler, or render a response.
--
-- Decision (PR-F2, 2026-09-05): extend the existing dispatcher with this
-- private collaborator instead of adding another router or security pipeline.
-- Route matching, body admission, timing, finalization, and handler invocation
-- retain their established single owners; this module owns only the cohesive
-- post-match selection/observation/guard responsibility.
module HarchWeb.Server.PostMatch
  ( PostMatchGuardResult (..),
    runPostMatchGuards,
  )
where

import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.Encoding.Error qualified as TextEncodingError
import HarchWeb.EndpointSecurity
  ( AccessRequirement (..),
    ApplicationSecurity (..),
    AuthenticationGuard (..),
    EndpointDispatchKind (..),
    EndpointGuard (..),
    EndpointGuardResult (..),
    EndpointMetadata (endpointAccess, endpointName, endpointRouteTemplate),
    EndpointRequest (..),
    runEndpointGuardPipeline,
  )
import HarchWeb.Routing (RouteDispatch (..), RouteRequest (..))
import HarchWeb.SecurityEvent (rootSecurityEventSink, rootSecurityEventSinkWithMountChain)
import HarchWeb.Server.Application
import HarchWeb.Server.ClientAction (isClientActionRequest)
import HarchWeb.Server.Response (MiddlewareResult (..), NonPageResponse (NonPageBodyResponse), ResponseBody (..))
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai

-- | The one declared owner selected after matching, together with the guard
-- result.  A client action owns its declared action route even when that URL
-- is absent from the page codec; an unknown action owns no declaration.
-- Keeping this selection with post-match guards prevents the admission rail
-- from independently interpreting the request.
data PostMatchGuardResult route context
  = HaltPostMatch (NonPageResponse route context)
  | ContinuePostMatch context (Maybe route)

-- | Apply the explicit post-match security selection to every declared route
-- outcome. A 404 has no endpoint declaration; a pre-route halt retains its
-- older response-body contract and is not reinterpreted as endpoint policy.
runPostMatchGuards :: Application route action context authorization -> Wai.Request -> Text -> RouteDispatch route context -> MiddlewareResult context -> IO (PostMatchGuardResult route context)
runPostMatchGuards webApplication request requestPath routeDispatch middlewareResult =
  case middlewareResult of
    HaltMiddleware _ responseBodyValue -> pure (HaltPostMatch (NonPageBodyResponse responseBodyValue))
    ContinueMiddleware middlewareContext ->
      case routeDispatch of
        RouteNotFound _
          | isAction,
            Just _ <- actionRoute ->
              runSelectedEndpointGuards EndpointClientAction
          | otherwise -> pure (ContinuePostMatch middlewareContext selectedAdmissionRoute)
        RouteMethodNotAllowed _ _ -> runSelectedEndpointGuards EndpointMethodNotAllowed
        RouteMatched _ -> runSelectedEndpointGuards EndpointMatched
        RouteMatchedHead _ -> runSelectedEndpointGuards EndpointMatchedHead
        RouteOptions _ _ -> runSelectedEndpointGuards EndpointOptions
  where
    routeRequest = routeDispatchRequest routeDispatch
    isAction = isClientActionRequest request
    actionRoute =
      if isAction
        then clientActionRoute webApplication (requestMethodText request) requestPath (requestContext routeRequest)
        else Nothing
    selectedAdmissionRoute =
      if isAction
        then actionRoute
        else case routeDispatch of
          RouteNotFound _ -> Nothing
          _ -> Just (requestRoute routeRequest)
    guardRouteRequest = maybe routeRequest (\actionRouteValue -> routeRequest {requestRoute = actionRouteValue}) actionRoute
    selectedEndpointMetadata =
      if isAction
        then clientActionEndpointMetadata webApplication (requestMethodText request) requestPath (requestContext routeRequest)
        else Just (routeEndpointMetadata webApplication (requestRoute routeRequest))
    runSelectedEndpointGuards routeDispatchKind =
      case selectedEndpointMetadata of
        Nothing -> pure (ContinuePostMatch (requestContext routeRequest) selectedAdmissionRoute)
        Just selectedMetadata ->
          let observedRouteRequest =
                guardRouteRequest
                  { requestContext = applicationAttachRouteObservation webApplication (requestRoute guardRouteRequest) selectedMetadata (requestContext guardRouteRequest)
                  }
              endpointRequest =
                EndpointRequest
                  { endpointWaiRequest = request,
                    endpointRouteRequest = observedRouteRequest,
                    endpointMetadata = selectedMetadata,
                    endpointSecurityEventSink = fmap (securityEventSink selectedMetadata observedRouteRequest) (applicationSecurityEventRoot webApplication),
                    endpointDispatchKind = if isAction then EndpointClientAction else routeDispatchKind
                  }
           in case applicationSecurity webApplication of
                AuthenticationDisabled _ ->
                  case endpointAccess (endpointMetadata endpointRequest) of
                    AllowUnauthenticated -> runGuards endpointRequest
                    _ -> pure (HaltPostMatch (NonPageBodyResponse disabledSecurityResponse))
                _ -> runGuards endpointRequest
    runGuards endpointRequest =
      toPostMatchGuardResult selectedAdmissionRoute
        <$> runEndpointGuardPipeline (applicationEndpointGuards (applicationSecurity webApplication)) endpointRequest
    securityEventSink selectedMetadata observedRouteRequest eventRoot =
      case applicationRouteModuleChain webApplication of
        Nothing -> rootSecurityEventSink eventRoot (endpointName selectedMetadata) (endpointRouteTemplate selectedMetadata) (requestContext observedRouteRequest)
        Just routeModuleChain -> rootSecurityEventSinkWithMountChain eventRoot (routeModuleChain (requestRoute guardRouteRequest)) (endpointName selectedMetadata) (endpointRouteTemplate selectedMetadata) (requestContext observedRouteRequest)

toPostMatchGuardResult :: Maybe route -> EndpointGuardResult route context -> PostMatchGuardResult route context
toPostMatchGuardResult selectedAdmissionRoute = \case
  HaltEndpoint response -> HaltPostMatch response
  ContinueEndpoint context -> ContinuePostMatch context selectedAdmissionRoute

routeDispatchRequest :: RouteDispatch route context -> RouteRequest route context
routeDispatchRequest = \case
  RouteNotFound routeRequest -> routeRequest
  RouteMethodNotAllowed routeRequest _ -> routeRequest
  RouteMatched routeRequest -> routeRequest
  RouteMatchedHead routeRequest -> routeRequest
  RouteOptions routeRequest _ -> routeRequest

disabledSecurityResponse :: ResponseBody
disabledSecurityResponse =
  ResponseBody
    { responseStatus = Http.status503,
      responseContentType = "text/plain; charset=utf-8",
      responseBody = "Authentication is unavailable.",
      responseObservabilityAttributes = [],
      responseLogEntries = ["endpoint security configuration rejected a protected endpoint"],
      responseDatabaseOperations = []
    }

applicationEndpointGuards :: ApplicationSecurity route context authorization -> [EndpointGuard route context authorization]
applicationEndpointGuards = \case
  AuthenticationDisabled guards -> guards
  AuthenticationEnabled beforeGuards (AuthenticationGuard runAuthentication) afterGuards -> beforeGuards <> [EndpointGuard runAuthentication] <> afterGuards

requestMethodText :: Wai.Request -> Text
requestMethodText = TextEncoding.decodeUtf8With TextEncodingError.lenientDecode . Wai.requestMethod
