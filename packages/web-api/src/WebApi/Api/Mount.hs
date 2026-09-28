{-# LANGUAGE OverloadedStrings #-}

-- | The one shared mount prefix for web-api's endpoint declarations and
-- OpenAPI family. Keeping this path fact below both owners avoids a dependency
-- cycle: endpoint declarations need its rendered path, while OpenApiDocs
-- composes the mount around those same declarations.
module WebApi.Api.Mount
  ( webApiApiMountPrefix,
    webApiApiMountPrefixText,
  )
where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import HarchWeb.Routing (PathSegment, pathSegmentText, requiredPathSegment)

webApiApiMountPrefix :: NonEmpty PathSegment
webApiApiMountPrefix = requiredPathSegment "api" :| []

webApiApiMountPrefixText :: Text
webApiApiMountPrefixText = "/" <> pathSegmentText (NonEmpty.head webApiApiMountPrefix)
