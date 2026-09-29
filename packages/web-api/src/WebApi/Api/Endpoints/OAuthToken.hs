{-# LANGUAGE BangPatterns #-}

-- | The RFC 6749 client-credentials endpoint, including its bounded request
-- declaration, issuance adapter, protocol failures, and response rendering.
-- Keeping this protocol vertical slice separate leaves the shared endpoint
-- module focused on the application API while preserving one exported token
-- endpoint value for runtime routing and OpenAPI interpretation.
module WebApi.Api.Endpoints.OAuthToken
  ( tokenApiRouteDefinition,
    tokenApiEndpoint,
    TokenApiFailure (..),
    tokenApiOutcomeResponse,
    tokenApiFailureResponse,
    tokenApiMissingContentTypePolicy,
  )
where

import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Api
  ( ApiEndpointContract (..),
    ApiEndpointRequest (..),
    ApiFieldFailurePolicy (ApiRenderFieldFailures),
    ApiForm,
    ApiHeaderName,
    ApiHeaderValue,
    ApiMethod (ApiPost),
    ApiRequestBody (ApiUrlEncodedFormRequestBody),
    ApiRequestBodyByteLimit,
    ApiRequestParseError,
    ApiResponse (..),
    MissingContentTypePolicy (RejectMissingContentType),
    RequestCodec,
    SomeApiRouteEndpoint (..),
    apiContentType,
    apiResponse,
    apiRouteDefinition,
    apiRouteEndpointWithContext,
    bytesResponseEncoder,
    jsonMediaType,
    requireApiRequestBodyByteLimit,
  )
import HarchWeb.Authentication
  ( OAuth2ClientCredentials,
    OAuth2ClientCredentialsMaximumBytes,
    OAuth2ClientCredentialsRequest,
    encodedJwtBytes,
    oauth2ClientCredentialsRequestCodec,
    oauth2ClientCredentialsScopes,
    oauth2ClientSecretBasicCodec,
    oauth2ScopeText,
    requiredOAuth2ClientCredentialsMaximumBytesOrDie,
  )
import HarchWeb.OpenApi (OpenApiExtension, mkOpenApiExtension)
import HarchWeb.Site (RouteDefinition)
import Network.HTTP.Types qualified as HttpTypes
import Numeric.Natural (Natural)
import WebApi.Api.Endpoints.Support
  ( apiDocumentedDeclaration,
    jsonBytes,
    requireOpenApiExtension,
    requiredApiHeaderNameOrDie,
    requiredApiHeaderValueOrDie,
  )
import WebApi.ApiClientToken
  ( ApiClientTokenEnvironment,
    ApiClientTokenOutcome (..),
    issueApiClientToken,
  )
import WebApi.Response (jsonErrorBody, tokenApiSuccessBody)
import WebApi.Route (AppAuthorization, AppRequestContext, AppRoute (TokenApiRoute), endpointMetadata)

-- | The RFC 6749 client-credentials grant combines HTTP Basic client
-- authentication with a bounded URL-encoded form body. Both values use the
-- existing framework decoders, so the application owns neither raw parsing
-- nor a second request-body limit.
tokenApiRequestFields :: RequestCodec (OAuth2ClientCredentials, OAuth2ClientCredentialsRequest)
tokenApiRequestFields =
  (,)
    <$> oauth2ClientSecretBasicCodec tokenApiMaximumBasicBytes
    <*> oauth2ClientCredentialsRequestCodec

-- | Bounds for one small, fixed-shape token request: generous for realistic
-- client credentials and scope lists, while keeping malformed or hostile
-- inputs cheap to reject.
tokenApiMaximumBasicBytes :: OAuth2ClientCredentialsMaximumBytes
tokenApiMaximumBasicBytes = requiredOAuth2ClientCredentialsMaximumBytesOrDie 4096

tokenApiRequestBodyByteLimit :: ApiRequestBodyByteLimit
tokenApiRequestBodyByteLimit = requireApiRequestBodyByteLimit 2048

tokenApiRequestMaximumFields :: Natural
tokenApiRequestMaximumFields = 8

-- | RFC 6749 requires the fixed token request body to declare its URL-form
-- content type; a missing header is rejected rather than assumed. Harch only
-- reads this policy when the incoming request has no content type, so the
-- omission case is directly covered by the WAI integration test.
tokenApiMissingContentTypePolicy :: MissingContentTypePolicy
tokenApiMissingContentTypePolicy = RejectMissingContentType

tokenApiContract :: ApiEndpointContract OpenApiExtension (OAuth2ClientCredentials, OAuth2ClientCredentialsRequest) ApiForm ByteString.ByteString
tokenApiContract =
  ApiEndpointContract
    ApiPost
    tokenApiRequestFields
    ( ApiUrlEncodedFormRequestBody
        tokenApiMissingContentTypePolicy
        tokenApiRequestBodyByteLimit
        tokenApiRequestMaximumFields
    )
    (bytesResponseEncoder (apiContentType jsonMediaType) :| [])
    (ApiRenderFieldFailures tokenApiInvalidRequestResponse)
    tokenApiExtension

tokenApiExtension :: OpenApiExtension (OAuth2ClientCredentials, OAuth2ClientCredentialsRequest) ApiForm ByteString.ByteString
tokenApiExtension =
  requireOpenApiExtension
    (mkOpenApiExtension (Just "Issue an OAuth 2.0 access token for a registered API client (RFC 6749 client credentials).") Nothing [] False [])

-- | The anonymous runtime route authenticates an OAuth client through its
-- Basic credentials inside the bounded token request. This is the same value
-- exported through 'WebApi.Api.Endpoints' for the OpenAPI family.
tokenApiEndpoint :: ApiClientTokenEnvironment -> SomeApiRouteEndpoint AppRequestContext OpenApiExtension
tokenApiEndpoint !environment =
  SomeApiRouteEndpoint (apiRouteEndpointWithContext (apiDocumentedDeclaration (endpointMetadata TokenApiRoute) tokenApiContract) (tokenApiHandler environment) tokenApiFailureResponse)

tokenApiHandler :: ApiClientTokenEnvironment -> AppRequestContext -> ApiEndpointRequest (OAuth2ClientCredentials, OAuth2ClientCredentialsRequest) ApiForm -> IO (Either TokenApiFailure (ApiResponse ByteString.ByteString))
tokenApiHandler environment _requestContext endpointRequest =
  let (credentials, tokenRequest) = apiEndpointRequestFields endpointRequest
   in tokenApiOutcomeResponse <$> issueApiClientToken environment credentials (oauth2ClientCredentialsScopes tokenRequest)

tokenApiRouteDefinition :: ApiClientTokenEnvironment -> RouteDefinition AppRoute AppRequestContext AppAuthorization
tokenApiRouteDefinition environment =
  case tokenApiEndpoint environment of
    SomeApiRouteEndpoint endpoint -> apiRouteDefinition (endpointMetadata TokenApiRoute) endpoint

-- | Keep missing or malformed credential sources private while returning
-- RFC 6749's stable @invalid_request@ failure for every field parse error.
tokenApiInvalidRequestResponse :: [ApiRequestParseError] -> ApiResponse ByteString.ByteString
tokenApiInvalidRequestResponse _ =
  (tokenApiFailureResponse TokenApiInvalidScope)
    { apiEndpointResponseValue = jsonBytes (jsonErrorBody "invalid_request")
    }

-- | The three public shapes an OAuth client can be told. Every non-issuance
-- outcome collapses to the same unavailable result so storage, work-budget,
-- and signing details stay private.
data TokenApiFailure
  = TokenApiInvalidClient
  | TokenApiInvalidScope
  | TokenApiUnavailable

-- | A signed compact JWT is ASCII by construction, so the invalid UTF-8
-- branch is only reachable for a deliberately malformed typed test value.
-- Every durable-store, work-budget, or signing failure maps to the same
-- public unavailable outcome.
tokenApiOutcomeResponse :: ApiClientTokenOutcome -> Either TokenApiFailure (ApiResponse ByteString.ByteString)
tokenApiOutcomeResponse outcome =
  case outcome of
    ApiClientTokenIssued encodedJwt scopes lifetimeSeconds ->
      case TextEncoding.decodeUtf8' (encodedJwtBytes encodedJwt) of
        Left _ -> Left TokenApiUnavailable
        Right accessToken ->
          Right (apiResponse (jsonBytes (tokenApiSuccessBody accessToken (oauth2ScopeText <$> scopes) lifetimeSeconds)))
    ApiClientTokenInvalidClient -> Left TokenApiInvalidClient
    ApiClientTokenInvalidScope -> Left TokenApiInvalidScope
    ApiClientTokenStoreUnavailable -> Left TokenApiUnavailable
    ApiClientTokenWorkBudgetExhausted -> Left TokenApiUnavailable
    ApiClientTokenIssueFailed -> Left TokenApiUnavailable

tokenApiFailureResponse :: TokenApiFailure -> ApiResponse ByteString.ByteString
tokenApiFailureResponse failure =
  case failure of
    TokenApiInvalidClient ->
      (apiResponse (jsonBytes (jsonErrorBody "invalid_client")))
        { apiEndpointResponseStatus = HttpTypes.status401,
          apiEndpointResponseHeaders = [(wwwAuthenticateHeaderName, basicChallengeHeaderValue)]
        }
    TokenApiInvalidScope ->
      (apiResponse (jsonBytes (jsonErrorBody "invalid_scope")))
        { apiEndpointResponseStatus = HttpTypes.status400
        }
    TokenApiUnavailable ->
      (apiResponse (jsonBytes (jsonErrorBody "token-issuance-unavailable")))
        { apiEndpointResponseStatus = HttpTypes.status503
        }

-- | RFC 6749 requires this Basic challenge when client authentication fails;
-- the fixed realm and UTF-8 declaration match the credentials decoder.
wwwAuthenticateHeaderName :: ApiHeaderName
wwwAuthenticateHeaderName = requiredApiHeaderNameOrDie "WWW-Authenticate"

basicChallengeHeaderValue :: ApiHeaderValue
basicChallengeHeaderValue = requiredApiHeaderValueOrDie "Basic realm=\"oauth-token\", charset=\"UTF-8\""
