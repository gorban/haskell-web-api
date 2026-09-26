{-# LANGUAGE BangPatterns #-}

-- | Cached, application-selected OpenAPI document providers.
--
-- Decision record (AHI-4E, 2026-09-22): document generation is an optional
-- application-composition concern, separate from Harch's route dispatcher.
-- 'mkCachedOpenApiDocumentProvider' consumes one deliberate availability
-- snapshot during startup, validates and encodes the document once, then
-- returns those immutable bytes for every request context.  It therefore
-- cannot query a database or regenerate a document per request.  Applications
-- that need a dynamic visibility or cache policy can construct an
-- 'OpenApiDocumentProvider' explicitly, making that effectful policy visible
-- at their composition root.  The provider does not route or authorize a
-- request; a later Swagger-route slice adapts its typed result through the
-- existing route and response boundaries.
module HarchWeb.OpenApi.Provider
  ( OpenApiDocumentProvider (..),
    PreparedOpenApiDocument,
    preparedOpenApiDocumentBytes,
    prepareOpenApiDocumentFromSnapshot,
    mkCachedOpenApiDocumentProvider,
  )
where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as ByteString
import Data.Map.Strict (Map)
import HarchWeb.EndpointMetadata (AuthenticationProfileName)
import HarchWeb.OpenApi.Document
  ( OpenApiDocumentDetails,
    OpenApiDocumentFailure,
    OpenApiMountedFamily,
    buildOpenApiDocument,
    encodeOpenApiDocument,
  )
import HarchWeb.OpenApi.Security (OpenApiSecurityScheme)

-- | A documentation source selected by the application. A custom provider may
-- return a typed failure for a deliberately dynamic policy; its route adapter
-- must convert that failure to a safe unavailable response rather than expose
-- document internals.
newtype OpenApiDocumentProvider context = OpenApiDocumentProvider
  { prepareOpenApiDocument :: context -> IO (Either OpenApiDocumentFailure PreparedOpenApiDocument)
  }

-- | An already encoded, immutable document. Its constructor stays private so
-- every supplied value was produced by 'prepareOpenApiDocumentFromSnapshot'.
newtype PreparedOpenApiDocument = PreparedOpenApiDocument ByteString

-- | Read the bytes prepared by a provider. These bytes are safe to serve only
-- through a later typed documentation route, which still owns response
-- headers, security policy, and failure presentation.
preparedOpenApiDocumentBytes :: PreparedOpenApiDocument -> ByteString
preparedOpenApiDocumentBytes (PreparedOpenApiDocument bytes) = bytes

-- | Validate, build, and encode one document from an explicit availability
-- snapshot. Dynamic providers may call this at a deliberate cache boundary;
-- the supplied default calls it once during application startup.
prepareOpenApiDocumentFromSnapshot :: OpenApiDocumentDetails -> Map AuthenticationProfileName OpenApiSecurityScheme -> context -> [OpenApiMountedFamily context] -> Either OpenApiDocumentFailure PreparedOpenApiDocument
prepareOpenApiDocumentFromSnapshot details securitySchemes availabilitySnapshot mountedFamilies =
  prepareDocument <$> buildOpenApiDocument details securitySchemes availabilitySnapshot mountedFamilies
  where
    prepareDocument document =
      PreparedOpenApiDocument
        (ByteString.fromStrict (ByteString.toStrict (encodeOpenApiDocument document)))

-- | Create the supplied startup-cached provider. Failure happens before a
-- provider exists, so the default application composition treats malformed
-- documentation as startup failure instead of serving a stale or partially
-- generated document at runtime. The availability snapshot parameter is
-- demanded when this constructor is applied: the task contract makes the
-- snapshot one value consumed during startup, so realizing it here — at the
-- boundary that owns that contract — is what keeps it from being silently
-- discarded per endpoint (every default availability decision is
-- context-free) or re-derived after startup.
mkCachedOpenApiDocumentProvider :: OpenApiDocumentDetails -> Map AuthenticationProfileName OpenApiSecurityScheme -> context -> [OpenApiMountedFamily context] -> Either OpenApiDocumentFailure (OpenApiDocumentProvider context)
mkCachedOpenApiDocumentProvider details securitySchemes !availabilitySnapshot mountedFamilies = do
  preparedDocument <- prepareOpenApiDocumentFromSnapshot details securitySchemes availabilitySnapshot mountedFamilies
  pure (OpenApiDocumentProvider (const (pure (Right preparedDocument))))
