{-# LANGUAGE OverloadedStrings #-}

-- | The composed application's OAuth token and discovery protocol.
--
-- This extends the root's typed route algebra and Harch's bounded API
-- endpoint declaration; it does not create a second dispatcher. The issuer
-- and API audience are validated as HTTPS resource identifiers here, then
-- the same runtime supplies the issuer, audience and public-only JWKS used
-- for signing and verification. RFC well-known paths preserve issuer and
-- resource path components. Token body failures use the API endpoint's closed
-- body-failure hook so size, media-type and form errors share the OAuth error
-- shape and @Cache-Control: private, no-store@ response.
-- A guard also requires WAI to mark the token transport secure before the
-- body reader or client store can run. It does not infer security from
-- request-supplied forwarded headers; deployments must arrange for the HTTPS
-- listener or trusted transport adapter to set that WAI bit.
--
-- Decision record (AHI-4E-OAUTH, 2026-09-27): compose the existing
-- 'HarchWeb.ApiClientStore', bounded password-work workflow and startup
-- validated RS256 runtime so credentials, scopes, protocol errors and
-- metadata remain application-owned. Harch's typed route and body reader
-- remain the sole dispatch and bounded-input owners. The body reader
-- previously generated 400/413/415 responses before an application could
-- attach OAuth's required no-store headers. Taking over the body as a stream
-- would duplicate form parsing; extending the existing body declaration with
-- a closed failure reason keeps byte/field enforcement before decoding,
-- exposes no request detail, and permits this endpoint's safe interpretation.
-- Harch's normal typed field-failure renderer deliberately fixes status 400,
-- so OAuth opts into its status-preserving policy to return the required 401
-- Basic challenge for failed Authorization-header decoding. The authorization
-- metadata includes RFC 8414's required empty response-type list because this
-- server supports only the client-credentials grant, and omits an
-- unregistered protected-resources property. See @docs/design-guidance.md@.
-- The 2026-09-27 quality report measured this module at 530 lines and 24
-- imports; the cohesive split between token handling and discovery/JWKS is a
-- named follow-up in @TASKS/ahi-4e-oauth-module-health.md@.
module App.Composed.OAuth
  ( ComposedOAuthConfigurationError (..),
    ComposedOAuthDependencies,
    ComposedOAuthTokenFailure (..),
    buildComposedOAuthModule,
    composedOAuthTokenOutcomeResponse,
    mkComposedOAuthDependencies,
  )
where

import App.Composed.ApiClient (composedExampleApiClientScopes)
import App.Composed.ApiClientToken
  ( ComposedApiClientTokenEnvironment (..),
    ComposedApiClientTokenOutcome (..),
    issueComposedApiClientToken,
  )
import App.Composed.Auth
  ( ComposedJwtRuntime,
    composedJwtApiAudienceText,
    composedJwtIssuerText,
    composedJwtPublicJwkSet,
  )
import App.Composed.Model (ComposedContext, OAuthRoute (..), RootAction, RootActionTarget, RootAuthorization, RootRoute (..))
import Data.Aeson (Value, (.=))
import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Action (emptyActionCodec)
import HarchWeb.Api
  ( ApiEndpointContract (..),
    ApiEndpointRequest (..),
    ApiFieldFailurePolicy (ApiRenderFieldFailuresWithStatus, ApiUseGenericFieldFailure),
    ApiForm,
    ApiHeaderName,
    ApiHeaderValue,
    ApiMethod (ApiGet, ApiPost),
    ApiRequestBody (ApiNoRequestBody, ApiUrlEncodedFormRequestBodyWithFailure),
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
    apiResponseBodyToProtocolResponse,
    apiRouteDefinitionWithContext,
    apiRouteDefinitionWithContextNeverFailing,
    bytesResponseEncoder,
    jsonMediaType,
    noRequestFields,
    requireApiRequestBodyByteLimit,
  )
import HarchWeb.ApplicationModule (ApplicationModule (..))
import HarchWeb.Authentication
  ( OAuth2ClientCredentials,
    OAuth2ClientCredentialsMaximumBytes,
    OAuth2ClientCredentialsRequest,
    OAuth2Scope,
    OAuth2ScopeError,
    encodedJwtBytes,
    oauth2ClientCredentialsRequestCodec,
    oauth2ClientCredentialsScopes,
    oauth2ClientSecretBasicCodec,
    oauth2ScopeText,
    requiredOAuth2ClientCredentialsMaximumBytesOrDie,
  )
import HarchWeb.EndpointMetadata
  ( AccessRequirement (AllowUnauthenticated),
    EndpointMetadata,
    EndpointProtocol (ApiEndpoint),
    mkEndpointMetadata,
    requiredEndpointNameOrDie,
    requiredRouteTemplateOrDie,
  )
import HarchWeb.EndpointSecurity
  ( EndpointGuard (..),
    EndpointGuardResult (..),
    EndpointRequest (..),
  )
import HarchWeb.Routing
  ( RouteCodec (..),
    RouteLocation (..),
    RouteMethod (RouteGet, RoutePost),
    RouteParseResult (RouteNotMatched, RouteParsed),
    RouteRequest (..),
    decodeRouteLocation,
    pathSegmentText,
    requestTarget,
    requiredPathSegment,
    routeMethodPolicy,
    routePathSegments,
  )
import HarchWeb.SecurityEvent (requiredModuleNameOrDie)
import HarchWeb.Server (NonPageResponse (NonPageProtocolResponse))
import HarchWeb.Site (RouteDefinition)
import Network.HTTP.Types qualified as Http
import Network.URI (URI (..), URIAuth (..), parseURI, uriToString)
import Network.Wai qualified as Wai
import Numeric.Natural (Natural)

data ComposedOAuthConfigurationError
  = ComposedOAuthIssuerMustBeHttpsUrl
  | ComposedOAuthResourceMustBeHttpsUrl
  | ComposedOAuthExampleScopesInvalid OAuth2ScopeError
  deriving (Eq, Show)

data ComposedOAuthUrl = ComposedOAuthUrl
  { composedOAuthParsedUrl :: URI,
    composedOAuthBasePath :: Text,
    composedOAuthBaseSegments :: [Text]
  }

data ComposedOAuthDependencies = ComposedOAuthDependencies
  { composedOAuthTokenEnvironment :: ComposedApiClientTokenEnvironment,
    composedOAuthRuntime :: ComposedJwtRuntime,
    composedOAuthIssuer :: Text,
    composedOAuthResource :: Text,
    composedOAuthIssuerUrl :: ComposedOAuthUrl,
    composedOAuthResourceUrl :: ComposedOAuthUrl,
    composedOAuthExampleScopes :: NonEmpty OAuth2Scope
  }

data ComposedOAuthTokenFailure
  = ComposedOAuthInvalidClient
  | ComposedOAuthInvalidRequest
  | ComposedOAuthInvalidScope
  | ComposedOAuthUnsupportedGrantType
  | ComposedOAuthTemporarilyUnavailable
  deriving (Eq, Show)

mkComposedOAuthDependencies :: ComposedApiClientTokenEnvironment -> Either ComposedOAuthConfigurationError ComposedOAuthDependencies
mkComposedOAuthDependencies tokenEnvironment = do
  let jwtRuntime = composedApiClientTokenJwtRuntime tokenEnvironment
      issuer = composedJwtIssuerText jwtRuntime
      resource = composedJwtApiAudienceText jwtRuntime
  issuerUrl <- maybe (Left ComposedOAuthIssuerMustBeHttpsUrl) Right (parseComposedOAuthUrl issuer)
  resourceUrl <- maybe (Left ComposedOAuthResourceMustBeHttpsUrl) Right (parseComposedOAuthUrl resource)
  exampleScopes <- first ComposedOAuthExampleScopesInvalid composedExampleApiClientScopes
  pure
    ComposedOAuthDependencies
      { composedOAuthTokenEnvironment = tokenEnvironment,
        composedOAuthRuntime = jwtRuntime,
        composedOAuthIssuer = issuer,
        composedOAuthResource = resource,
        composedOAuthIssuerUrl = issuerUrl,
        composedOAuthResourceUrl = resourceUrl,
        composedOAuthExampleScopes = exampleScopes
      }

parseComposedOAuthUrl :: Text -> Maybe ComposedOAuthUrl
parseComposedOAuthUrl value = do
  parsedUrl <- parseURI (Text.unpack value)
  authority <- uriAuthority parsedUrl
  if uriScheme parsedUrl /= "https:" || null (uriRegName authority) || not (null (uriUserInfo authority)) || not (null (uriQuery parsedUrl)) || not (null (uriFragment parsedUrl))
    then Nothing
    else do
      let rawBasePath = Text.pack (uriPath parsedUrl)
          basePath = stripOneTrailingSlash rawBasePath
          rawRoutePath = TextEncoding.encodeUtf8 (if Text.null basePath then "/" else basePath)
      decodedPath <- either (const Nothing) Just (decodeRouteLocation (requestTarget rawRoutePath ""))
      let allSegments = pathSegmentText <$> routePathSegments decodedPath
          baseSegments = trimTrailingEmptySegments allSegments
      if any Text.null baseSegments
        then Nothing
        else
          Just
            ComposedOAuthUrl
              { composedOAuthParsedUrl = parsedUrl,
                composedOAuthBasePath = basePath,
                composedOAuthBaseSegments = baseSegments
              }

stripOneTrailingSlash :: Text -> Text
stripOneTrailingSlash value =
  if Text.isSuffixOf "/" value
    then Text.dropEnd 1 value
    else value

trimTrailingEmptySegments :: [Text] -> [Text]
trimTrailingEmptySegments = reverse . dropWhile Text.null . reverse

buildComposedOAuthModule :: ComposedOAuthDependencies -> ApplicationModule RootRoute RootActionTarget RootAction ComposedContext RootAuthorization
buildComposedOAuthModule dependencies =
  ApplicationModule
    { moduleName = requiredModuleNameOrDie "root.oauth",
      moduleOwnsRoute = isOAuthRoute,
      moduleRouteMountChain = const (requiredModuleNameOrDie "root.oauth" :| []),
      moduleRouteCodec = composedOAuthRouteCodec dependencies,
      moduleDeclaredRoutes = UnlocalizedOAuth <$> oauthRoutes,
      moduleEndpoints = oauthEndpointDefinition dependencies,
      moduleActionCodec = emptyActionCodec,
      moduleActionRoute = \_ _ -> Nothing,
      moduleHandleAction = \_ -> pure Nothing,
      moduleGuards = [requireSecureTokenTransport]
    }
  where
    oauthRoutes = [OAuthToken, OAuthJwks, OAuthAuthorizationServerMetadata, OAuthProtectedResourceMetadata]

isOAuthRoute :: RootRoute -> Bool
isOAuthRoute rootRoute =
  case rootRoute of
    UnlocalizedOAuth _ -> True
    _ -> False

requireSecureTokenTransport :: EndpointGuard RootRoute ComposedContext RootAuthorization
requireSecureTokenTransport =
  EndpointGuard $ \endpointRequest ->
    case requestRoute (endpointRouteRequest endpointRequest) of
      UnlocalizedOAuth OAuthToken
        | not (Wai.isSecure (endpointWaiRequest endpointRequest)) ->
            pure (HaltEndpoint (NonPageProtocolResponse (apiResponseBodyToProtocolResponse composedOAuthInvalidRequestBody)))
      _ -> pure (ContinueEndpoint (requestContext (endpointRouteRequest endpointRequest)))

composedOAuthRouteCodec :: ComposedOAuthDependencies -> RouteCodec RootRoute ComposedContext
composedOAuthRouteCodec dependencies =
  RouteCodec
    { parseRoute = \requestContext location ->
        let segments = pathSegmentText <$> routePathSegments location
         in case lookup segments routeLocations of
              Just route -> RouteParsed (RouteRequest (UnlocalizedOAuth route) requestContext)
              Nothing -> RouteNotMatched,
      renderRoute = \request ->
        case requestRoute request of
          UnlocalizedOAuth selectedRoute -> RouteLocation (requiredPathSegment <$> routeSegments selectedRoute) []
          _ -> RouteLocation [] [],
      notFoundRequest = RouteRequest (UnlocalizedOAuth OAuthToken),
      routeMethods = \request ->
        case requestRoute request of
          UnlocalizedOAuth OAuthToken -> routeMethodPolicy [RoutePost]
          UnlocalizedOAuth _ -> routeMethodPolicy [RouteGet]
          _ -> routeMethodPolicy []
    }
  where
    oauthRoutes = [OAuthToken, OAuthJwks, OAuthAuthorizationServerMetadata, OAuthProtectedResourceMetadata]
    routeLocations = [(routeSegments route, route) | route <- oauthRoutes]
    routeSegments route =
      case route of
        OAuthToken -> composedOAuthBaseSegments (composedOAuthIssuerUrl dependencies) <> ["oauth", "token"]
        OAuthJwks -> composedOAuthBaseSegments (composedOAuthIssuerUrl dependencies) <> ["oauth", "jwks.json"]
        OAuthAuthorizationServerMetadata -> [".well-known", "oauth-authorization-server"] <> composedOAuthBaseSegments (composedOAuthIssuerUrl dependencies)
        OAuthProtectedResourceMetadata -> [".well-known", "oauth-protected-resource"] <> composedOAuthBaseSegments (composedOAuthResourceUrl dependencies)

oauthEndpointDefinition :: ComposedOAuthDependencies -> RootRoute -> RouteDefinition RootRoute ComposedContext RootAuthorization
oauthEndpointDefinition dependencies rootRoute =
  case rootRoute of
    UnlocalizedOAuth OAuthToken -> tokenRouteDefinition dependencies
    UnlocalizedOAuth OAuthJwks -> staticJsonRouteDefinition (oauthEndpointMetadata dependencies OAuthJwks) (Aeson.toJSON (composedJwtPublicJwkSet (composedOAuthRuntime dependencies)))
    UnlocalizedOAuth OAuthAuthorizationServerMetadata -> staticJsonRouteDefinition (oauthEndpointMetadata dependencies OAuthAuthorizationServerMetadata) (authorizationServerMetadata dependencies)
    UnlocalizedOAuth OAuthProtectedResourceMetadata -> staticJsonRouteDefinition (oauthEndpointMetadata dependencies OAuthProtectedResourceMetadata) (protectedResourceMetadata dependencies)
    _ -> error "composed-domains: OAuth definitions come from their root protocol module"

oauthEndpointMetadata :: ComposedOAuthDependencies -> OAuthRoute -> EndpointMetadata RootAuthorization
oauthEndpointMetadata dependencies route =
  mkEndpointMetadata
    (requiredEndpointNameOrDie (endpointNameFor route))
    (requiredRouteTemplateOrDie (endpointPathFor dependencies route))
    ApiEndpoint
    AllowUnauthenticated

endpointNameFor :: OAuthRoute -> Text
endpointNameFor route =
  case route of
    OAuthToken -> "root.oauth.token"
    OAuthJwks -> "root.oauth.jwks"
    OAuthAuthorizationServerMetadata -> "root.oauth.authorization-server-metadata"
    OAuthProtectedResourceMetadata -> "root.oauth.protected-resource-metadata"

endpointPathFor :: ComposedOAuthDependencies -> OAuthRoute -> Text
endpointPathFor dependencies route =
  case route of
    OAuthToken -> endpointPath (composedOAuthIssuerUrl dependencies) "/oauth/token"
    OAuthJwks -> endpointPath (composedOAuthIssuerUrl dependencies) "/oauth/jwks.json"
    OAuthAuthorizationServerMetadata -> wellKnownPath "oauth-authorization-server" (composedOAuthIssuerUrl dependencies)
    OAuthProtectedResourceMetadata -> wellKnownPath "oauth-protected-resource" (composedOAuthResourceUrl dependencies)

endpointPath :: ComposedOAuthUrl -> Text -> Text
endpointPath url suffix = composedOAuthBasePath url <> suffix

wellKnownPath :: Text -> ComposedOAuthUrl -> Text
wellKnownPath suffix url =
  "/.well-known/" <> suffix <> composedOAuthBasePath url

tokenRouteDefinition :: ComposedOAuthDependencies -> RouteDefinition RootRoute ComposedContext RootAuthorization
tokenRouteDefinition dependencies =
  apiRouteDefinitionWithContext contract metadata (tokenHandler dependencies) composedOAuthTokenFailureResponse
  where
    metadata = oauthEndpointMetadata dependencies OAuthToken
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

staticJsonRouteDefinition :: EndpointMetadata RootAuthorization -> Value -> RouteDefinition RootRoute ComposedContext RootAuthorization
staticJsonRouteDefinition metadata jsonValue =
  apiRouteDefinitionWithContextNeverFailing contract metadata (\_ _ -> pure (apiResponse responseBytes))
  where
    contract =
      ApiEndpointContract
        ApiGet
        noRequestFields
        ApiNoRequestBody
        (bytesResponseEncoder (apiContentType jsonMediaType) :| [])
        ApiUseGenericFieldFailure
        NoApiExtension
    responseBytes = jsonBytes jsonValue

authorizationServerMetadata :: ComposedOAuthDependencies -> Value
authorizationServerMetadata dependencies =
  Aeson.object
    [ "issuer" .= composedOAuthIssuer dependencies,
      "token_endpoint" .= endpointAbsoluteUrl (composedOAuthIssuerUrl dependencies) "/oauth/token",
      "jwks_uri" .= endpointAbsoluteUrl (composedOAuthIssuerUrl dependencies) "/oauth/jwks.json",
      "grant_types_supported" .= ["client_credentials" :: Text],
      "response_types_supported" .= ([] :: [Text]),
      "token_endpoint_auth_methods_supported" .= ["client_secret_basic" :: Text],
      "scopes_supported" .= (oauth2ScopeText <$> composedOAuthExampleScopes dependencies)
    ]

protectedResourceMetadata :: ComposedOAuthDependencies -> Value
protectedResourceMetadata dependencies =
  Aeson.object
    [ "resource" .= composedOAuthResource dependencies,
      "authorization_servers" .= [composedOAuthIssuer dependencies],
      "scopes_supported" .= (oauth2ScopeText <$> composedOAuthExampleScopes dependencies),
      "bearer_methods_supported" .= ["header" :: Text]
    ]

endpointAbsoluteUrl :: ComposedOAuthUrl -> Text -> Text
endpointAbsoluteUrl url suffix =
  Text.pack
    ( uriToString
        id
        ((composedOAuthParsedUrl url) {uriPath = Text.unpack (composedOAuthBasePath url <> suffix), uriQuery = "", uriFragment = ""})
        ""
    )

oauthError :: Text -> Value
oauthError errorCode = Aeson.object ["error" .= errorCode]

jsonBytes :: Value -> ByteString
jsonBytes = LazyByteString.toStrict . Aeson.encode
