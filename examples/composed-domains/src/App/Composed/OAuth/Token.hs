-- | Client-credentials request decoding and OAuth token response semantics.
module App.Composed.OAuth.Token
  ( ComposedOAuthTokenFailure (..),
    composedOAuthInvalidRequestBody,
    composedOAuthTokenOutcomeResponse,
    tokenRouteDefinition,
  )
where

import App.Composed.ApiClientToken
  ( ComposedApiClientTokenOutcome (..),
    issueComposedApiClientToken,
  )
import App.Composed.Model (ComposedContext, RootAuthorization, RootRoute)
import App.Composed.OAuth.Configuration (ComposedOAuthDependencies (..))
import Data.Aeson (Value, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Api
  ( ApiEndpointContract (..),
    ApiEndpointRequest (..),
    ApiFieldFailurePolicy (ApiRenderFieldFailuresWithStatus),
    ApiForm,
    ApiHeaderName,
    ApiHeaderValue,
    ApiMethod (ApiPost),
    ApiRequestBody (ApiUrlEncodedFormRequestBodyWithFailure),
    ApiRequestBodyByteLimit,
    ApiRequestBodyFailure (..),
    ApiRequestParseError (..),
    ApiRequestSource (..),
    ApiResponse (..),
    ApiResponseBody (..),
    MissingContentTypePolicy (RejectMissingContentType),
    NoApiExtension (..),
    RequestCodec,
    apiContentType,
    apiEndpointRequestBody,
    apiEndpointRequestFields,
    apiFormFields,
    apiHeaderName,
    apiHeaderValue,
    apiResponse,
    apiRouteDefinitionWithContext,
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
import HarchWeb.EndpointMetadata (EndpointMetadata)
import HarchWeb.Site (RouteDefinition)
import Network.HTTP.Types qualified as Http
import Numeric.Natural (Natural)

data ComposedOAuthTokenFailure
  = ComposedOAuthInvalidClient
  | ComposedOAuthInvalidRequest
  | ComposedOAuthInvalidScope
  | ComposedOAuthUnsupportedGrantType
  | ComposedOAuthTemporarilyUnavailable
  deriving (Eq, Show)

tokenRouteDefinition :: ComposedOAuthDependencies -> EndpointMetadata RootAuthorization -> RouteDefinition RootRoute ComposedContext RootAuthorization
tokenRouteDefinition dependencies metadata =
  apiRouteDefinitionWithContext contract metadata (tokenHandler dependencies) composedOAuthTokenFailureResponse
  where
    contract =
      ApiEndpointContract
        ApiPost
        tokenRequestFields
        ( ApiUrlEncodedFormRequestBodyWithFailure
            RejectMissingContentType
            tokenBodyMaximumBytes
            tokenBodyMaximumFields
            composedOAuthBodyFailureResponse
        )
        (bytesResponseEncoder (apiContentType jsonMediaType) :| [])
        (ApiRenderFieldFailuresWithStatus composedOAuthFieldFailureResponse)
        NoApiExtension

tokenRequestFields :: RequestCodec (OAuth2ClientCredentials, OAuth2ClientCredentialsRequest)
tokenRequestFields =
  (,)
    <$> oauth2ClientSecretBasicCodec tokenMaximumBasicBytes
    <*> oauth2ClientCredentialsRequestCodec

tokenMaximumBasicBytes :: OAuth2ClientCredentialsMaximumBytes
tokenMaximumBasicBytes = requiredOAuth2ClientCredentialsMaximumBytesOrDie 4096

tokenBodyMaximumBytes :: ApiRequestBodyByteLimit
tokenBodyMaximumBytes = requireApiRequestBodyByteLimit 2048

tokenBodyMaximumFields :: Natural
tokenBodyMaximumFields = 8

tokenHandler :: ComposedOAuthDependencies -> ComposedContext -> ApiEndpointRequest (OAuth2ClientCredentials, OAuth2ClientCredentialsRequest) ApiForm -> IO (Either ComposedOAuthTokenFailure (ApiResponse ByteString))
tokenHandler dependencies _requestContext endpointRequest = do
  let (credentials, tokenRequest) = apiEndpointRequestFields endpointRequest
      suppliedClientAuthentication = any isBodyClientAuthenticationParameter (apiFormFields (apiEndpointRequestBody endpointRequest))
  if suppliedClientAuthentication
    then pure (Left ComposedOAuthInvalidRequest)
    else do
      outcome <- issueComposedApiClientToken (composedOAuthTokenEnvironment dependencies) credentials (oauth2ClientCredentialsScopes tokenRequest)
      pure (composedOAuthTokenOutcomeResponse outcome)

isBodyClientAuthenticationParameter :: (Text, Text) -> Bool
isBodyClientAuthenticationParameter (name, _) = name `elem` ["client_id", "client_secret", "client_assertion", "client_assertion_type"]

-- | Convert the application's ordinary workflow alternatives once at the
-- token endpoint boundary. An issued compact JWT is allowed only in the
-- successful @access_token@ member; every response carries no-store headers.
composedOAuthTokenOutcomeResponse :: ComposedApiClientTokenOutcome -> Either ComposedOAuthTokenFailure (ApiResponse ByteString)
composedOAuthTokenOutcomeResponse outcome =
  case outcome of
    ComposedApiClientTokenIssued encodedJwt scopes lifetime ->
      case TextEncoding.decodeUtf8' (encodedJwtBytes encodedJwt) of
        Left _ -> Left ComposedOAuthTemporarilyUnavailable
        Right accessToken ->
          Right
            (noStoreApiResponse (jsonBytes (Aeson.object ["access_token" .= accessToken, "token_type" .= ("Bearer" :: Text), "expires_in" .= lifetime, "scope" .= Text.unwords (oauth2ScopeText <$> scopes)])))
    ComposedApiClientTokenInvalidClient -> Left ComposedOAuthInvalidClient
    ComposedApiClientTokenInvalidScope -> Left ComposedOAuthInvalidScope
    ComposedApiClientTokenStoreUnavailable -> Left ComposedOAuthTemporarilyUnavailable
    ComposedApiClientTokenWorkBudgetExhausted -> Left ComposedOAuthTemporarilyUnavailable
    ComposedApiClientTokenIssueFailed -> Left ComposedOAuthTemporarilyUnavailable

composedOAuthTokenFailureResponse :: ComposedOAuthTokenFailure -> ApiResponse ByteString
composedOAuthTokenFailureResponse failure =
  case failure of
    ComposedOAuthInvalidClient ->
      (noStoreApiResponse (jsonBytes (oauthError "invalid_client")))
        { apiEndpointResponseStatus = Http.status401,
          apiEndpointResponseHeaders = [(requiredHeaderName "WWW-Authenticate", requiredHeaderValue "Basic realm=\"oauth-token\", charset=\"UTF-8\""), (requiredHeaderName "Cache-Control", requiredHeaderValue "private, no-store"), (requiredHeaderName "Pragma", requiredHeaderValue "no-cache")]
        }
    ComposedOAuthInvalidRequest ->
      (noStoreApiResponse (jsonBytes (oauthError "invalid_request")))
        { apiEndpointResponseStatus = Http.status400
        }
    ComposedOAuthInvalidScope ->
      (noStoreApiResponse (jsonBytes (oauthError "invalid_scope")))
        { apiEndpointResponseStatus = Http.status400
        }
    ComposedOAuthUnsupportedGrantType ->
      (noStoreApiResponse (jsonBytes (oauthError "unsupported_grant_type")))
        { apiEndpointResponseStatus = Http.status400
        }
    ComposedOAuthTemporarilyUnavailable ->
      (noStoreApiResponse (jsonBytes (oauthError "temporarily_unavailable")))
        { apiEndpointResponseStatus = Http.status503
        }

composedOAuthFieldFailureResponse :: [ApiRequestParseError] -> ApiResponse ByteString
composedOAuthFieldFailureResponse errors
  | any isDuplicateAuthorization errors = composedOAuthTokenFailureResponse ComposedOAuthInvalidRequest
  | any isAuthorizationFailure errors = composedOAuthTokenFailureResponse ComposedOAuthInvalidClient
  | any isUnsupportedGrantType errors = composedOAuthTokenFailureResponse ComposedOAuthUnsupportedGrantType
  | any isInvalidScope errors = composedOAuthTokenFailureResponse ComposedOAuthInvalidScope
  | otherwise = composedOAuthTokenFailureResponse ComposedOAuthInvalidRequest
  where
    isDuplicateAuthorization failure =
      case failure of
        DuplicateApiField ApiHeaderSource "authorization" -> True
        _ -> False
    isAuthorizationFailure failure =
      case failure of
        MissingApiField ApiHeaderSource "authorization" -> True
        InvalidApiField ApiHeaderSource "authorization" -> True
        _ -> False
    isUnsupportedGrantType failure =
      case failure of
        InvalidApiField ApiFormSource "grant_type" -> True
        _ -> False
    isInvalidScope failure =
      case failure of
        InvalidApiField ApiFormSource "scope" -> True
        _ -> False

composedOAuthBodyFailureResponse :: ApiRequestBodyFailure -> ApiResponseBody
composedOAuthBodyFailureResponse _ = composedOAuthInvalidRequestBody

composedOAuthInvalidRequestBody :: ApiResponseBody
composedOAuthInvalidRequestBody =
  (noStoreApiResponseBody (jsonBytes (oauthError "invalid_request")))
    { apiResponseStatus = Http.status400
    }

noStoreApiResponse :: ByteString -> ApiResponse ByteString
noStoreApiResponse responseBytes =
  (apiResponse responseBytes)
    { apiEndpointResponseHeaders =
        [ (requiredHeaderName "Cache-Control", requiredHeaderValue "private, no-store"),
          (requiredHeaderName "Pragma", requiredHeaderValue "no-cache")
        ]
    }

noStoreApiResponseBody :: ByteString -> ApiResponseBody
noStoreApiResponseBody responseBytes =
  ApiResponseBody
    { apiResponseStatus = Http.status400,
      apiResponseContentType = apiContentType jsonMediaType,
      apiResponseHeaders =
        [ ("Cache-Control", requiredHeaderValue "private, no-store"),
          ("Pragma", requiredHeaderValue "no-cache")
        ],
      apiResponseBodyBytes = responseBytes
    }

requiredHeaderName :: Text -> ApiHeaderName
requiredHeaderName value =
  case apiHeaderName value of
    Just headerName -> headerName
    Nothing -> error "composed-domains authored an invalid OAuth response header name"

requiredHeaderValue :: Text -> ApiHeaderValue
requiredHeaderValue value =
  case apiHeaderValue value of
    Just headerValue -> headerValue
    Nothing -> error "composed-domains authored an invalid OAuth response header value"

oauthError :: Text -> Value
oauthError errorCode = Aeson.object ["error" .= errorCode]

jsonBytes :: Value -> ByteString
jsonBytes = LazyByteString.toStrict . Aeson.encode
