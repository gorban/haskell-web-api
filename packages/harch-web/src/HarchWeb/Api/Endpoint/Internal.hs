{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Private representation shared by the endpoint declaration, family, and
-- runtime modules.  Keeping the constructors here lets the route-family
-- interpreter inspect declarations without making 'ApiPath' or endpoint
-- constructors public API.
module HarchWeb.Api.Endpoint.Internal
  ( ApiMethod (..),
    apiMethodText,
    ApiPath (..),
    at,
    ApiRouteEndpoint (..),
    SomeApiRouteEndpoint (..),
    ApiEndpointRequest (..),
    ApiMultipartRequest (..),
    ApiMultipartRequestError (..),
    withApiMultipartRequest,
    ApiStreamingRequest (..),
    RequestBodyReadFailure (..),
    ApiRequestBody (..),
    apiRouteEndpoint,
    apiRouteEndpointWithFieldFailure,
    apiRouteEndpointAt,
    apiRouteEndpointAtWithFieldFailure,
    apiRouteEndpointAtNeverFailing,
    apiRouteEndpointAtNeverFailingWithFieldFailure,
    apiRouteEndpointPath,
    apiRouteEndpointMethod,
  )
where

import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Typeable (Typeable)
import HarchWeb.Api.Multipart
  ( MultipartConsumeError,
    MultipartLimits,
    MultipartScopedPart,
    MultipartStorage,
  )
import HarchWeb.Api.Request
import HarchWeb.Api.Response
import HarchWeb.Server.RequestBody
import Numeric.Natural (Natural)

-- | Methods an 'ApiRouteEndpoint' can declare. @HEAD@ is synthesized from
-- @GET@ and @OPTIONS@ from the declared method table for a matched path.
data ApiMethod
  = ApiGet
  | ApiPost
  | ApiPut
  | ApiPatch
  | ApiDelete
  deriving (Eq, Show)

apiMethodText :: ApiMethod -> Text
apiMethodText methodValue =
  case methodValue of
    ApiGet -> "GET"
    ApiPost -> "POST"
    ApiPut -> "PUT"
    ApiPatch -> "PATCH"
    ApiDelete -> "DELETE"

-- | A static, context-independent request path.
newtype ApiPath = ApiPath Text
  deriving (Eq, Show)

at :: Text -> ApiPath
at = ApiPath

-- | One typed endpoint declaration for use in the application's shared route
-- table. It owns its path, method, field decoding, exactly one declared body
-- consumer, domain-failure interpretation, and response representations.
data ApiRouteEndpoint fields body domainFailure response where
  ApiRouteEndpoint ::
    (Typeable response) =>
    ApiPath ->
    ApiMethod ->
    RequestCodec fields ->
    ApiRequestBody body ->
    NonEmpty (ApiResponseEncoder response) ->
    Maybe ([ApiRequestParseError] -> ApiResponse response) ->
    (ApiEndpointRequest fields body -> IO (Either domainFailure (ApiResponse response))) ->
    (domainFailure -> ApiResponse response) ->
    ApiRouteEndpoint fields body domainFailure response
  ApiRouteEndpointNeverFailing ::
    (Typeable response) =>
    ApiPath ->
    ApiMethod ->
    RequestCodec fields ->
    ApiRequestBody body ->
    NonEmpty (ApiResponseEncoder response) ->
    Maybe ([ApiRequestParseError] -> ApiResponse response) ->
    (ApiEndpointRequest fields body -> IO (ApiResponse response)) ->
    ApiRouteEndpoint fields body domainFailure response

-- | An endpoint table may contain declarations with different request, body,
-- failure, and response types without exposing those existentials to a
-- handler. Each declaration remains fully typed at its definition site.
data SomeApiRouteEndpoint where
  SomeApiRouteEndpoint :: ApiRouteEndpoint fields body domainFailure response -> SomeApiRouteEndpoint

-- | Cohesive decoded input supplied to an endpoint handler.
data ApiEndpointRequest fields body = ApiEndpointRequest
  { apiEndpointRequestFields :: fields,
    apiEndpointRequestBody :: body
  }

-- | A scoped, single-use multipart request consumer. Its callback is the
-- only place a completed file upload is visible, so an upload must be
-- promoted there to outlive the request. Calling it twice is rejected before
-- attempting a second WAI body read.
newtype ApiMultipartRequest stored = ApiMultipartRequest
  { consumeApiMultipartRequest ::
      (MultipartScopedPart stored -> IO (Either MultipartConsumeError ())) ->
      IO (Either ApiMultipartRequestError ())
  }

-- | Multipart consumption either has the parser's precise failure or was
-- already claimed by the endpoint handler.
data ApiMultipartRequestError
  = ApiMultipartRequestAlreadyConsumed
  | ApiMultipartRequestFailed MultipartConsumeError
  deriving (Eq, Show)

-- | Consume the endpoint's declared multipart body once. This keeps the
-- request-scoped storage lifecycle inside the existing multipart adapter;
-- callers map its typed result to their ordinary endpoint outcome.
withApiMultipartRequest ::
  ApiMultipartRequest stored ->
  (MultipartScopedPart stored -> IO (Either MultipartConsumeError ())) ->
  IO (Either ApiMultipartRequestError ())
withApiMultipartRequest = consumeApiMultipartRequest

-- | One chunk pulled from a declared streaming request body, bounded the
-- same way 'HarchWeb.Server.RequestBody.readRequestBodyUpTo' bounds a
-- buffered one: each pull enforces the running-total budget instead of the
-- framework retaining the body itself. An empty chunk marks the end of the
-- body. Calling it after the body ends keeps returning an empty chunk.
newtype ApiStreamingRequest = ApiStreamingRequest
  { pullApiStreamingRequestChunk :: IO (Either RequestBodyReadFailure ByteString.ByteString)
  }

-- | An endpoint either declares no body consumer, one bounded buffered
-- decoder, a bounded URL-encoded form whose fields join the request codec,
-- a bounded incremental stream, or a scoped multipart consumer. A multipart
-- body remains scoped: its callback owns each completed part, and must
-- deliberately promote any file that needs to outlive the request.
data ApiRequestBody body where
  ApiNoRequestBody :: ApiRequestBody ()
  ApiBufferedRequestBody ::
    MissingContentTypePolicy ->
    Int ->
    [ApiBodyDecoder body] ->
    ApiRequestBody body
  ApiUrlEncodedFormRequestBody ::
    MissingContentTypePolicy ->
    Int ->
    Natural ->
    ApiRequestBody ApiForm
  ApiStreamingRequestBody ::
    Int ->
    ApiRequestBody ApiStreamingRequest
  ApiMultipartRequestBody ::
    MultipartStorage stored ->
    MultipartLimits ->
    ApiRequestBody (ApiMultipartRequest stored)

apiRouteEndpoint ::
  (Typeable response) =>
  ApiMethod ->
  RequestCodec fields ->
  ApiRequestBody body ->
  NonEmpty (ApiResponseEncoder response) ->
  (ApiEndpointRequest fields body -> IO (Either domainFailure (ApiResponse response))) ->
  (domainFailure -> ApiResponse response) ->
  ApiRouteEndpoint fields body domainFailure response
apiRouteEndpoint method = apiRouteEndpointAt method (at "")

-- | Like 'apiRouteEndpoint', but the declaration renders accumulated field
-- errors instead of the legacy generic protocol response.  Keeping the
-- errors at the declaration boundary preserves their source and field names
-- without leaking a transport concern into the handler's domain-failure rail.
-- The runtime fixes this response's status at 400, since field decoding is a
-- client request failure rather than an application outcome.
apiRouteEndpointWithFieldFailure ::
  (Typeable response) =>
  ApiMethod ->
  RequestCodec fields ->
  ApiRequestBody body ->
  NonEmpty (ApiResponseEncoder response) ->
  ([ApiRequestParseError] -> ApiResponse response) ->
  (ApiEndpointRequest fields body -> IO (Either domainFailure (ApiResponse response))) ->
  (domainFailure -> ApiResponse response) ->
  ApiRouteEndpoint fields body domainFailure response
apiRouteEndpointWithFieldFailure method = apiRouteEndpointAtWithFieldFailure method (at "")

-- | Construct a typed endpoint that owns a concrete path. The route-table
-- adapter can use 'apiRouteEndpoint' while the application route codec
-- remains the authoritative path owner.
apiRouteEndpointAt ::
  (Typeable response) =>
  ApiMethod ->
  ApiPath ->
  RequestCodec fields ->
  ApiRequestBody body ->
  NonEmpty (ApiResponseEncoder response) ->
  (ApiEndpointRequest fields body -> IO (Either domainFailure (ApiResponse response))) ->
  (domainFailure -> ApiResponse response) ->
  ApiRouteEndpoint fields body domainFailure response
apiRouteEndpointAt method path fields body encoders =
  ApiRouteEndpoint path method fields body encoders Nothing

-- | Path-owning variant of 'apiRouteEndpointWithFieldFailure'.
apiRouteEndpointAtWithFieldFailure ::
  (Typeable response) =>
  ApiMethod ->
  ApiPath ->
  RequestCodec fields ->
  ApiRequestBody body ->
  NonEmpty (ApiResponseEncoder response) ->
  ([ApiRequestParseError] -> ApiResponse response) ->
  (ApiEndpointRequest fields body -> IO (Either domainFailure (ApiResponse response))) ->
  (domainFailure -> ApiResponse response) ->
  ApiRouteEndpoint fields body domainFailure response
apiRouteEndpointAtWithFieldFailure method path fields body encoders fieldFailure =
  ApiRouteEndpoint path method fields body encoders (Just fieldFailure)

-- | Construct an endpoint whose handler has no domain-failure rail. Prefer
-- this over inventing an unreachable error value and failure renderer: it
-- makes total behavior explicit and leaves no impossible branch.
apiRouteEndpointAtNeverFailing ::
  (Typeable response) =>
  ApiMethod ->
  ApiPath ->
  RequestCodec fields ->
  ApiRequestBody body ->
  NonEmpty (ApiResponseEncoder response) ->
  (ApiEndpointRequest fields body -> IO (ApiResponse response)) ->
  ApiRouteEndpoint fields body domainFailure response
apiRouteEndpointAtNeverFailing method path fields body encoders =
  ApiRouteEndpointNeverFailing path method fields body encoders Nothing

-- | Total-handler variant of 'apiRouteEndpointAtWithFieldFailure'.
apiRouteEndpointAtNeverFailingWithFieldFailure ::
  (Typeable response) =>
  ApiMethod ->
  ApiPath ->
  RequestCodec fields ->
  ApiRequestBody body ->
  NonEmpty (ApiResponseEncoder response) ->
  ([ApiRequestParseError] -> ApiResponse response) ->
  (ApiEndpointRequest fields body -> IO (ApiResponse response)) ->
  ApiRouteEndpoint fields body domainFailure response
apiRouteEndpointAtNeverFailingWithFieldFailure method path fields body encoders fieldFailure =
  ApiRouteEndpointNeverFailing path method fields body encoders (Just fieldFailure)

apiRouteEndpointPath :: ApiRouteEndpoint fields body domainFailure response -> ApiPath
apiRouteEndpointPath endpoint =
  case endpoint of
    ApiRouteEndpoint path _ _ _ _ _ _ _ -> path
    ApiRouteEndpointNeverFailing path _ _ _ _ _ _ -> path

apiRouteEndpointMethod :: ApiRouteEndpoint fields body domainFailure response -> ApiMethod
apiRouteEndpointMethod endpoint =
  case endpoint of
    ApiRouteEndpoint _ method _ _ _ _ _ _ -> method
    ApiRouteEndpointNeverFailing _ method _ _ _ _ _ -> method
