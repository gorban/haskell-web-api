-- | Low-level HTTP API matching, codecs, and response helpers.
--
-- This compatibility facade intentionally does /not/ define the application's
-- route dispatcher. The AC design decision extends the shared
-- 'HarchWeb.Routing.RouteCodec'/'HarchWeb.Site.RouteDefinition' boundary and
-- the server response interpreter so pages, actions, and APIs have one
-- method/path owner. See @docs/design-guidance.md@. The existing middleware is
-- retained only as a compatibility helper while that boundary is completed.
-- Streaming bodies such as multipart remain separate; see
-- 'HarchWeb.Api.Multipart'.
module HarchWeb.Api
  ( ApiMethod (..),
    ApiPath,
    ApiEndpoint,
    ApiMatchResult (..),
    apiMethodText,
    apiEndpoint,
    apiEndpointTarget,
    at,
    matchApiEndpoints,
    apiAllowHeaderValue,
    ApiHttpResponse (..),
    respondApiMatch,
    apiHttpResponseToProtocolResponse,
    apiHttpResponseToWaiResponse,
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
    requiredField,
    optionalField,
    fieldWithDefault,
    runRequestCodec,
    ApiBodyDecoder (..),
    ApiMediaType,
    apiMediaType,
    apiMediaTypeText,
    jsonMediaType,
    plainTextMediaType,
    htmlMediaType,
    ApiContentType,
    apiContentType,
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
    ApiResponseBody (..),
    apiJsonResponse,
    apiTextResponse,
    apiBytesResponse,
    AcceptedRange (..),
    ApiNegotiationResult (..),
    parseAcceptHeader,
    selectRepresentation,
  )
where

import HarchWeb.Api.Endpoint
import HarchWeb.Api.MediaType
import HarchWeb.Api.Negotiation
import HarchWeb.Api.Request
import HarchWeb.Api.Response
