-- | Shared construction helpers for web-api's typed endpoint declarations.
-- These helpers keep endpoint-local contracts explicit while centralizing
-- the few operations that must behave identically across endpoint modules.
module WebApi.Api.Endpoints.Support
  ( apiDocumentedDeclaration,
    requireOpenApiExtension,
    requiredApiHeaderNameOrDie,
    requiredApiHeaderValueOrDie,
    jsonBytes,
  )
where

import Data.Aeson.Encoding qualified as JsonEncoding
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import HarchWeb.Api
  ( ApiEndpointContract,
    ApiHeaderName,
    ApiHeaderValue,
    ApiRouteEndpointDeclaration (..),
    apiHeaderName,
    apiHeaderValue,
    at,
  )
import HarchWeb.EndpointSecurity (EndpointMetadata (endpointRouteTemplate), routeTemplateText)
import HarchWeb.OpenApi
  ( OpenApiExtension,
    OpenApiExtensionError,
  )
import WebApi.Api.Mount (webApiApiMountPrefixText)

-- | Build a documented endpoint declaration from the same real
-- 'EndpointMetadata' value runtime dispatch uses. The family-local path is
-- derived by removing the shared mount prefix, so endpoint declarations do
-- not maintain a second copy of their path.
apiDocumentedDeclaration :: EndpointMetadata authorization -> ApiEndpointContract extension fields body response -> ApiRouteEndpointDeclaration extension fields body response
apiDocumentedDeclaration metadata =
  ApiRouteEndpointDeclaration (at (Text.drop (Text.length webApiApiMountPrefixText) (routeTemplateText (endpointRouteTemplate metadata))))

-- | Unwrap one statically authored documentation extension. The endpoints
-- pass compile-time literals; the failure rail remains total and directly
-- testable at this shared construction boundary.
requireOpenApiExtension :: Either OpenApiExtensionError (OpenApiExtension fields body response) -> OpenApiExtension fields body response
requireOpenApiExtension =
  either
    (error . ("web-api authored an invalid OpenAPI extension: " <>) . show)
    id

requiredApiHeaderNameOrDie :: Text.Text -> ApiHeaderName
requiredApiHeaderNameOrDie value = fromMaybe (error ("invalid API header name literal: " <> Text.unpack value)) (apiHeaderName value)

requiredApiHeaderValueOrDie :: Text.Text -> ApiHeaderValue
requiredApiHeaderValueOrDie value = fromMaybe (error ("invalid API header value literal: " <> Text.unpack value)) (apiHeaderValue value)

-- | Encode the same pure JSON bytes used by the older response helpers,
-- without a text conversion on a request path.
jsonBytes :: JsonEncoding.Encoding -> ByteString.ByteString
jsonBytes = LazyByteString.toStrict . JsonEncoding.encodingToLazyByteString
