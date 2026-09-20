-- | The optional OpenAPI package's data-model boundary.
--
-- Decision record (AHI-4E compatibility slice, 2026-09-20): re-export the
-- selected public @openapi3@ model from the optional package rather than put
-- OpenAPI dependencies or a second API dispatcher into @harch-web@. The
-- package currently proves that the frozen, released data model builds and
-- encodes under this repository's GHC/Aeson plan. The following AHI-4E slice
-- adds the generic endpoint extension and this package's explicit family
-- interpreter; it must preserve the existing route dispatcher rather than
-- crawl a completed site. See @docs/design-guidance.md@.
module HarchWeb.OpenApi
  ( OpenApi,
  )
where

import Data.OpenApi (OpenApi)
