-- | Low-level HTTP API matching, codecs, and response helpers.
--
-- This facade does /not/ itself define the application's route dispatcher.
-- The AC design decision extends the shared
-- 'HarchWeb.Routing.RouteCodec'/'HarchWeb.Site.RouteDefinition' boundary and
-- the server response interpreter so pages, actions, and APIs have one
-- method/path owner: 'apiEndpointFamily' validates one heterogeneous
-- 'SomeApiRouteEndpoint' table before 'apiRouteEndpointFamilyCodec' adapts it
-- into that shared 'HarchWeb.Routing.RouteCodec' family, for combination via
-- 'HarchWeb.Routing.combineRouteCodecs' with an application's other route
-- families, and 'apiRouteEndpointFamilyDefinition' supplies the matching
-- 'HarchWeb.Site.RouteDefinition'. See
-- @docs/design-guidance.md@. This route-family pair is the only supported
-- dispatch path: the legacy @ApiEndpoint@/@apiEndpointMiddleware@
-- compatibility table and the intermediate @apiRouteEndpointMiddleware@
-- typed-WAI-middleware composition were removed 2026-08-13 once every
-- application in this repository had migrated onto the family registry (see
-- the AK decision record). A typed endpoint may declare a scoped multipart
-- request capability through 'ApiMultipartRequestBody'; its storage adapter
-- remains supplied by 'HarchWeb.Api.Multipart'. A typed endpoint may instead
-- declare a bounded, incremental request stream through
-- 'ApiStreamingRequestBody': the handler pulls one chunk at a time from the
-- delivered 'ApiStreamingRequest' rather than the framework buffering the
-- whole body, so its own memory use stays bounded regardless of body size; a
-- chunk that would push the running total over the declared budget is
-- reported as 'RequestBodyReadFailure' instead of retained. Typed API
-- response encoders can return a request-scoped stream through
-- 'streamingResponseEncoder'.
module HarchWeb.Api
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
    apiRouteEndpointWithFieldFailure,
    apiRouteEndpointAt,
    apiRouteEndpointAtWithFieldFailure,
    apiRouteEndpointAtNeverFailing,
    apiRouteEndpointAtNeverFailingWithFieldFailure,
    apiRouteEndpointPath,
    apiRouteDefinition,
    apiRouteDefinitionWithContext,
    apiRouteDefinitionWithContextWithFieldFailure,
    apiRouteDefinitionWithContextNeverFailing,
    apiRouteDefinitionWithContextNeverFailingWithFieldFailure,
    ApiEndpointFamily,
    ApiEndpointFamilyError (..),
    apiEndpointFamily,
    requireApiEndpointFamily,
    apiRouteEndpointFamilyCodec,
    apiRouteEndpointFamilyDefinition,
    at,
    ApiHttpResponse (..),
    apiHttpResponseToProtocolResponse,
    apiResponseBodyToProtocolResponse,
    ApiRequestData (..),
    apiRequestDataFromWaiRequest,
    ApiRequestSource (..),
    ApiRequestParseError (..),
    ApiHeaderName,
    apiHeaderName,
    apiHeaderNameText,
    ApiFieldValue,
    RequestField,
    RequestCodec,
    noRequestFields,
    apiTextValue,
    parseApiField,
    queryField,
    headerField,
    cookieField,
    formField,
    requiredField,
    optionalField,
    fieldWithDefault,
    runRequestCodec,
    runApiFormCodec,
    apiRequestDataWithForm,
    ApiBodyDecoder (..),
    ApiMediaType,
    apiMediaType,
    requireApiMediaType,
    apiMediaTypeText,
    jsonMediaType,
    plainTextMediaType,
    urlEncodedFormMediaType,
    htmlMediaType,
    ApiContentType,
    apiContentType,
    apiContentTypeMediaType,
    apiContentTypeParameters,
    apiUtf8ContentType,
    apiContentTypeText,
    jsonContentType,
    plainTextContentType,
    MissingContentTypePolicy (..),
    ApiBodyOutcome (..),
    selectApiBodyDecoder,
    jsonBodyDecoder,
    textBodyDecoder,
    bytesBodyDecoder,
    ApiForm,
    apiFormFields,
    urlEncodedFormBodyDecoder,
    ApiResponse (..),
    apiResponse,
    ApiHeaderValue,
    apiHeaderValue,
    apiHeaderValueText,
    ApiEncodedResponseBody (..),
    ApiResponseEncoder (..),
    jsonResponseEncoder,
    textResponseEncoder,
    bytesResponseEncoder,
    streamingResponseEncoder,
    ApiResponseBody (..),
    apiJsonResponse,
    apiTextResponse,
    apiBytesResponse,
    AcceptedRange (..),
    ApiNegotiationResult (..),
    ApiContentTypeNegotiationResult (..),
    parseAcceptHeader,
    selectRepresentation,
    selectContentTypeRepresentation,
  )
where

import HarchWeb.Api.Endpoint
import HarchWeb.Api.MediaType
import HarchWeb.Api.Negotiation
import HarchWeb.Api.Request
import HarchWeb.Api.Response
