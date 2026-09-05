{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Trusted action-codec mount adaptation.
--
-- The adapter names exactly the values crossing a module boundary; it does
-- not parse requests or create another action dispatcher.
module HarchWeb.Action.Mount
  ( ActionCodecMountAdapter (..),
    mountActionCodecAtPrefix,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Action.Codec
  ( ActionCodec (..),
    ActionCodecError (..),
    ActionPath (..),
    ValidatedActionEndpoint (..),
  )
import HarchWeb.EndpointMetadata
  ( AccessRequirement (..),
    mkEndpointMetadata,
    mkEndpointName,
    mkRouteTemplate,
  )
import HarchWeb.EndpointMetadata qualified as EndpointMetadata
import HarchWeb.Markup (safeUrlText)
import HarchWeb.Routing (PathSegment, RouteLocation (..), encodeRouteLocation)

-- | The typed mappings required to adapt a child's validated action codec to
-- its parent algebra. Dynamic request context remains explicit at decoding;
-- this record contains only stable construction-owned transformations.
data ActionCodecMountAdapter parentTarget parentContext parentAuthorization parentAction childTarget childContext childAuthorization childAction = ActionCodecMountAdapter
  { actionMountEmbedTarget :: childTarget -> parentTarget,
    actionMountProjectContext :: parentContext -> childContext,
    actionMountProjectAuthorization :: childAuthorization -> parentAuthorization,
    actionMountEmbedAction :: childAction -> parentAction
  }

mountActionCodecAtPrefix ::
  NonEmpty PathSegment ->
  Text ->
  ActionCodecMountAdapter parentTarget parentContext parentAuthorization parentAction childTarget childContext childAuthorization childAction ->
  ActionCodec childTarget childContext childAuthorization childAction ->
  Either ActionCodecError (ActionCodec parentTarget parentContext parentAuthorization parentAction)
mountActionCodecAtPrefix pathSegments endpointNamespace ActionCodecMountAdapter {actionMountEmbedTarget, actionMountProjectContext, actionMountProjectAuthorization, actionMountEmbedAction} =
  mountCodec
  where
    mountCodec (ActionCodec endpoints) = ActionCodec <$> traverse mountEndpoint endpoints
    mountedPathPrefix = safeUrlText (encodeRouteLocation (RouteLocation (toList pathSegments) []))
    toList (firstPathSegment :| remainingPathSegments) = firstPathSegment : remainingPathSegments
    mountEndpoint (ValidatedActionEndpoint childTarget childPath childMetadata decoder) = do
      mountedMetadata <- mapMetadata childMetadata
      pure (ValidatedActionEndpoint (actionMountEmbedTarget childTarget) (mountPath childPath) mountedMetadata (fmap actionMountEmbedAction decoder))
    mountPath childPath =
      ActionPath
        { actionPathMethod = actionPathMethod childPath,
          actionPathIdentity = mountPathText (actionPathIdentity childPath),
          renderActionPath = mountPathText . renderActionPath childPath . actionMountProjectContext,
          actionStaticPath = fmap mountPathText (actionStaticPath childPath)
        }
    mapMetadata metadata = do
      mountedName <- firstMetadataError (mkEndpointName (endpointNamespace <> "." <> EndpointMetadata.endpointNameText (EndpointMetadata.endpointName metadata)))
      mountedTemplate <- firstMetadataError (mkRouteTemplate (mountPathText (EndpointMetadata.routeTemplateText (EndpointMetadata.endpointRouteTemplate metadata))))
      pure (mkEndpointMetadata mountedName mountedTemplate (EndpointMetadata.endpointProtocol metadata) (mapAccessRequirement (EndpointMetadata.endpointAccess metadata)))
    mapAccessRequirement requirement = case requirement of
      AllowUnauthenticated -> AllowUnauthenticated
      RequireAuthenticated -> RequireAuthenticated
      RequireAuthorized authorization -> RequireAuthorized (actionMountProjectAuthorization authorization)
    mountPathText childPath
      | childPath == "/" = mountedPathPrefix
      | otherwise = mountedPathPrefix <> "/" <> Text.dropWhile (== '/') childPath
    firstMetadataError = either (Left . InvalidActionEndpointMetadata) Right
