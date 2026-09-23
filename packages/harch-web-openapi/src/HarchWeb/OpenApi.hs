-- | The optional OpenAPI package's data-model boundary.
--
-- Decision record (AHI-4E, 2026-09-22): keep the selected public @openapi3@
-- model and typed endpoint metadata in this optional package rather than put
-- an OpenAPI dependency or a second API dispatcher into @harch-web@. The
-- metadata attaches through the existing generic endpoint-extension slot, so
-- it cannot alter codecs or runtime routing. Its document builder consumes
-- only explicit families paired with their structural runtime mounts; it does
-- not crawl a completed site.
--
-- Decision record (AHI-4E, 2026-09-23): security-scheme derivation now ships
-- (see @HarchWeb.OpenApi.Document@ and @HarchWeb.OpenApi.Security@). Schema
-- interpretation, caching, a documentation route, and wiring this into a real
-- application remain later AHI-4E work. See @docs/design-guidance.md@.
module HarchWeb.OpenApi
  ( OpenApi,
    OpenApiDocument,
    OpenApiDocumentDetails (..),
    OpenApiDocumentFailure (..),
    renderOpenApiDocumentFailure,
    OpenApiMountedFamily,
    openApiMountedFamily,
    buildOpenApiDocument,
    openApiDocumentModel,
    mapOpenApiDocumentModel,
    applyOpenApiOperationExtensions,
    applyOpenApiAnonymousSecurity,
    encodeOpenApiDocument,
    OpenApiDocumentProvider (..),
    PreparedOpenApiDocument,
    preparedOpenApiDocumentBytes,
    prepareOpenApiDocumentFromSnapshot,
    mkCachedOpenApiDocumentProvider,
    openApiDocumentRouteDefinition,
    OpenApiExtension,
    OpenApiExtensionError (..),
    OpenApiSpecificationExtension,
    emptyOpenApiExtension,
    withOpenApiExtension,
    mkOpenApiExtension,
    withOpenApiOperationId,
    withOpenApiResponseStatus,
    withOpenApiRequestSchema,
    withOpenApiResponseSchema,
    withOpenApiRequestExample,
    withOpenApiResponseExample,
    withOpenApiExternalDocs,
    mkOpenApiSpecificationExtension,
    OpenApiSecurityScheme,
    OpenApiSecuritySchemeError (..),
    mkOpenApiCookieSessionSecurityScheme,
    mkOpenApiOAuth2ClientCredentialsSecurityScheme,
  )
where

import Data.OpenApi (OpenApi)
import HarchWeb.OpenApi.Document
import HarchWeb.OpenApi.Metadata
import HarchWeb.OpenApi.Provider
import HarchWeb.OpenApi.Route
import HarchWeb.OpenApi.Security
