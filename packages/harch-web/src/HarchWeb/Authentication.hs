-- | Stable authoring facade for Harch's pluggable authentication boundary.
--
-- Decision record (PR-F6, 2026-09-05): retain one public import while moving
-- transport policy and proof-to-principal orchestration to cohesive private
-- owners. This does not add another extractor, router, or authentication rail.
module HarchWeb.Authentication
  ( module HarchWeb.Authentication.Pipeline,
    module HarchWeb.Authentication.Transport,
  )
where

import HarchWeb.Authentication.Pipeline
import HarchWeb.Authentication.Transport
