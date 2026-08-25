{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Private representation shared by the endpoint declaration, family, and
-- runtime modules.  Keeping the constructors here lets the route-family
-- interpreter inspect declarations without making 'ApiPath' or endpoint
-- constructors public API. Decision record (PR-F6, 2026-08-25): one
-- 'ApiEndpointContract' groups method, request codec/body, representations,
-- and field-failure policy; 'ApiRouteEndpointDeclaration' adds the path only
-- when a context-free endpoint owns one. This extends the existing endpoint
-- declaration boundary instead of retaining a Cartesian constructor-name
-- matrix: context-aware definitions reuse the contract, while the two
-- genuinely distinct handler rails remain explicit. No compatibility aliases
-- remain. See @docs/design-guidance.md@. Decision record (PR-F5, 2026-08-25): API body
-- declarations use the opaque 'ApiRequestBodyByteLimit', checked from a
-- 'Natural' against the private reader's 'Int' range. This extends the one
-- endpoint declaration boundary instead of adding runtime validation; the
-- three body readers convert it exactly once, so negative and overflowed
-- budgets cannot be authored. See @docs/design-guidance.md@.
module HarchWeb.Api.Endpoint.Internal
  ( ApiMethod (..),
    apiMethodText,
    ApiPath (..),
    at,
    ApiFieldFailurePolicy (..),
    ApiEndpointContract (..),
    ApiRouteEndpointDeclaration (..),
    ApiRouteEndpoint (..),
    SomeApiRouteEndpoint (..),
    ApiEndpointRequest (..),
    ApiMultipartRequest (..),
    ApiMultipartRequestError (..),
    withApiMultipartRequest,
    ApiStreamingRequest (..),
    RequestBodyReadFailure (..),
    ApiRequestBodyByteLimit,
    apiRequestBodyByteLimit,
    requireApiRequestBodyByteLimit,
    apiRequestBodyByteLimitValue,
    ApiRequestBody (..),
    apiRouteEndpoint,
    apiRouteEndpointNeverFailing,
    apiRouteEndpointPath,
    apiRouteEndpointMethod,
  )
where

import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
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

-- | A bounded API request body size that is non-negative and representable by
-- the private WAI reader's 'Int' limit. Construct it with
-- 'apiRequestBodyByteLimit'; the constructor stays private so endpoint
-- declarations cannot introduce a negative or out-of-range budget.
newtype ApiRequestBodyByteLimit = ApiRequestBodyByteLimit Natural

apiRequestBodyByteLimit :: Natural -> Maybe ApiRequestBodyByteLimit
apiRequestBodyByteLimit byteCount
  | byteCount <= fromIntegral (maxBound :: Int) = Just (ApiRequestBodyByteLimit byteCount)
  | otherwise = Nothing

-- | Require a literal or otherwise independently validated byte limit. Use
-- 'apiRequestBodyByteLimit' when untrusted configuration needs an ordinary
-- recoverable validation result.
requireApiRequestBodyByteLimit :: Natural -> ApiRequestBodyByteLimit
requireApiRequestBodyByteLimit byteCount =
  fromMaybe (error "API request body byte limit exceeds Int") (apiRequestBodyByteLimit byteCount)

apiRequestBodyByteLimitValue :: ApiRequestBodyByteLimit -> Int
apiRequestBodyByteLimitValue (ApiRequestBodyByteLimit byteCount) = fromIntegral byteCount

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

-- | How a typed endpoint declaration turns field decoding rejections into a
-- protocol response. The policy belongs beside the input and representation
-- declarations, not in a separate constructor-name suffix.
data ApiFieldFailurePolicy response
  = ApiUseGenericFieldFailure
  | ApiRenderFieldFailures ([ApiRequestParseError] -> ApiResponse response)

-- | The reusable typed contract for an API request: method, decoded fields,
-- exactly one body consumer, response representations, and field-error
-- policy. Context-aware route definitions consume this value directly.
data ApiEndpointContract fields body response = ApiEndpointContract
  { apiEndpointContractMethod :: ApiMethod,
    apiEndpointContractFields :: RequestCodec fields,
    apiEndpointContractBody :: ApiRequestBody body,
    apiEndpointContractEncoders :: NonEmpty (ApiResponseEncoder response),
    apiEndpointContractFieldFailurePolicy :: ApiFieldFailurePolicy response
  }

-- | A path-owning endpoint declaration. It combines a route path with one
-- 'ApiEndpointContract'; context-free route tables use this while
-- context-aware definitions reuse the contract without manufacturing a path.
data ApiRouteEndpointDeclaration fields body response = ApiRouteEndpointDeclaration
  { apiRouteEndpointDeclarationPath :: ApiPath,
    apiRouteEndpointDeclarationContract :: ApiEndpointContract fields body response
  }

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
    ApiRequestBodyByteLimit ->
    [ApiBodyDecoder body] ->
    ApiRequestBody body
  ApiUrlEncodedFormRequestBody ::
    MissingContentTypePolicy ->
    ApiRequestBodyByteLimit ->
    Natural ->
    ApiRequestBody ApiForm
  ApiStreamingRequestBody ::
    ApiRequestBodyByteLimit ->
    ApiRequestBody ApiStreamingRequest
  ApiMultipartRequestBody ::
    MultipartStorage stored ->
    MultipartLimits ->
    ApiRequestBody (ApiMultipartRequest stored)

-- | Construct an endpoint with an ordinary, typed domain-failure rail.
-- 'ApiRouteEndpointDeclaration' keeps route and request protocol choices
-- cohesive, leaving this function to state only the genuinely distinct
-- handler mode and its failure interpreter.
apiRouteEndpoint ::
  (Typeable response) =>
  ApiRouteEndpointDeclaration fields body response ->
  (ApiEndpointRequest fields body -> IO (Either domainFailure (ApiResponse response))) ->
  (domainFailure -> ApiResponse response) ->
  ApiRouteEndpoint fields body domainFailure response
apiRouteEndpoint declaration =
  ApiRouteEndpoint path method fields body encoders fieldFailure
  where
    ApiRouteEndpointDeclaration path contract = declaration
    ApiEndpointContract method fields body encoders failurePolicy = contract
    fieldFailure = case failurePolicy of
      ApiUseGenericFieldFailure -> Nothing
      ApiRenderFieldFailures renderFieldFailures -> Just renderFieldFailures

-- | Construct an endpoint with a total handler and no fabricated domain
-- failure branch.
apiRouteEndpointNeverFailing ::
  (Typeable response) =>
  ApiRouteEndpointDeclaration fields body response ->
  (ApiEndpointRequest fields body -> IO (ApiResponse response)) ->
  ApiRouteEndpoint fields body domainFailure response
apiRouteEndpointNeverFailing declaration =
  ApiRouteEndpointNeverFailing path method fields body encoders fieldFailure
  where
    ApiRouteEndpointDeclaration path contract = declaration
    ApiEndpointContract method fields body encoders failurePolicy = contract
    fieldFailure = case failurePolicy of
      ApiUseGenericFieldFailure -> Nothing
      ApiRenderFieldFailures renderFieldFailures -> Just renderFieldFailures

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
