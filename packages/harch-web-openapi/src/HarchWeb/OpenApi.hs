-- | The optional OpenAPI package's data-model boundary.
--
-- Decision record (AHI-4E, 2026-09-22): keep the selected public @openapi3@
-- model and typed endpoint metadata in this optional package rather than put
-- an OpenAPI dependency or a second API dispatcher into @harch-web@. The
-- metadata attaches through the existing generic endpoint-extension slot, so
-- it cannot alter codecs or runtime routing. This package still has no family
-- interpreter, document aggregation, or provider: the next AHI-4E task must
-- consume explicitly supplied families and must not crawl a completed site.
-- See @docs/design-guidance.md@.
module HarchWeb.OpenApi
  ( OpenApi,
    OpenApiExtension,
    OpenApiExtensionError (..),
    OpenApiSpecificationExtension,
    emptyOpenApiExtension,
    withOpenApiExtension,
    mkOpenApiExtension,
    mkOpenApiSpecificationExtension,
  )
where

import Data.OpenApi (OpenApi)
import HarchWeb.OpenApi.Metadata
