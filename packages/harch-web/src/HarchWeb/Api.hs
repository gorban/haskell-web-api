-- | Low-level HTTP API matching, codecs, and response helpers.
--
-- This facade does /not/ itself define the application's route dispatcher.
-- The AC design decision extends the shared
-- 'HarchWeb.Routing.RouteCodec'/'HarchWeb.Site.RouteDefinition' boundary and
-- the server response interpreter so pages, actions, and APIs have one
-- method/path owner: 'apiRouteEndpointFamilyCodec' adapts a
-- 'SomeApiRouteEndpoint' table into that shared 'HarchWeb.Routing.RouteCodec'
-- family, for combination via 'HarchWeb.Routing.combineRouteCodecs' with an
-- application's other route families, and 'apiRouteEndpointFamilyDefinition'
-- supplies the matching 'HarchWeb.Site.RouteDefinition'. See
-- @docs/design-guidance.md@. 'apiRouteEndpointMiddleware'/'apiEndpointMiddleware'
-- remain compatibility helpers for an application not yet migrated onto that
-- pair; wrapping one around a 'HarchWeb.Site.buildSiteApplication' output
-- shares no 404\/405\/Allow\/HEAD\/OPTIONS authority with the site's own
-- route table. A typed endpoint may declare a scoped multipart request
-- capability through 'ApiMultipartRequestBody'; its storage adapter remains
-- supplied by 'HarchWeb.Api.Multipart'. Typed API response encoders can
-- return a request-scoped stream through 'streamingResponseEncoder'.
module HarchWeb.Api
  ( ApiMethod (..),
    ApiPath,
    ApiEndpoint,
    ApiRouteEndpoint,
    SomeApiRouteEndpoint (..),
    ApiEndpointRequest (..),
    ApiRequestBody (..),
    ApiMultipartRequest,
    ApiMultipartRequestError (..),
    withApiMultipartRequest,
    ApiMatchResult (..),
    apiMethodText,
    apiEndpoint,
    apiRouteEndpoint,
    apiRouteEndpointAt,
    apiRouteEndpointPath,
    apiRouteDefinition,
    apiRouteEndpointFamilyCodec,
    apiRouteEndpointFamilyDefinition,
    matchedApiRouteEndpointOrDie,
    apiEndpointTarget,
    at,
    matchApiEndpoints,
    apiAllowHeaderValue,
    ApiHttpResponse (..),
    respondApiMatch,
    apiHttpResponseToProtocolResponse,
    apiResponseBodyToProtocolResponse,
    apiHttpResponseToWaiResponse,
    apiRouteEndpointMiddleware,
    apiEndpointMiddleware,
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
