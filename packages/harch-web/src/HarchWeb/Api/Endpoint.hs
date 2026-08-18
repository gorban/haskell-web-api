-- | Typed API endpoint declarations and their single shared route-family
-- dispatcher.
--
-- Decision record (2026-08-18): this module remains the one public API
-- facade. Its three cohesive private collaborators are split by ownership:
-- @.Internal@ owns representation that must stay abstract to callers,
-- @.Family@ owns the one method-aware route-family adapter, and @.Runtime@
-- owns request decoding and protocol-response interpretation. This is a
-- structural extension of the existing boundary, not a second dispatcher or
-- a newly exposed internal API: all callers continue to compose the same
-- 'apiRouteEndpointFamilyCodec'/'apiRouteEndpointFamilyDefinition' pair.
-- The split repairs the module-health signal while keeping the family
-- dispatcher's ownership and 'ApiPath' abstraction intact; see
-- @docs/design-guidance.md@ and the Endpoint module-health task in
-- @TASKS.md@.
module HarchWeb.Api.Endpoint
  ( ApiMethod (..),
    ApiPath,
    ApiRouteEndpoint,
    SomeApiRouteEndpoint (..),
    ApiEndpointRequest (..),
    ApiRequestBody (..),
    ApiStreamingRequest (..),
    RequestBodyReadFailure (..),
    ApiMultipartRequest,
    ApiMultipartRequestError (..),
    withApiMultipartRequest,
    apiMethodText,
    apiRouteEndpoint,
    apiRouteEndpointAt,
    apiRouteEndpointAtNeverFailing,
    apiRouteEndpointAtNeverFailingWithFieldFailure,
    apiRouteEndpointAtWithFieldFailure,
    apiRouteEndpointPath,
    apiRouteEndpointWithFieldFailure,
    apiRouteDefinition,
    apiRouteDefinitionWithContext,
    apiRouteDefinitionWithContextWithFieldFailure,
    apiRouteDefinitionWithContextNeverFailing,
    apiRouteDefinitionWithContextNeverFailingWithFieldFailure,
    apiRouteEndpointFamilyCodec,
    apiRouteEndpointFamilyDefinition,
    at,
    apiResponseBodyToProtocolResponse,
    ApiHttpResponse (..),
    apiHttpResponseToProtocolResponse,
  )
where

import HarchWeb.Api.Endpoint.Family
import HarchWeb.Api.Endpoint.Internal
  ( ApiEndpointRequest (..),
    ApiMethod (..),
    ApiMultipartRequest,
    ApiMultipartRequestError (..),
    ApiPath,
    ApiRequestBody (..),
    ApiRouteEndpoint,
    ApiStreamingRequest (..),
    RequestBodyReadFailure (..),
    SomeApiRouteEndpoint (..),
    apiMethodText,
    apiRouteEndpoint,
    apiRouteEndpointAt,
    apiRouteEndpointAtNeverFailing,
    apiRouteEndpointAtNeverFailingWithFieldFailure,
    apiRouteEndpointAtWithFieldFailure,
    apiRouteEndpointPath,
    apiRouteEndpointWithFieldFailure,
    at,
    withApiMultipartRequest,
  )
import HarchWeb.Api.Endpoint.Runtime
