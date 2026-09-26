{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | @\/api\/status@ and @\/api\/second@ composed through
-- "HarchWeb.Api.Endpoint"'s typed endpoint boundary rather than the
-- hand-rolled 'WebApi.Route.ApiRoute' dispatch in "WebApi.Response". Both
-- need the request's own resolved locale (derived from a URL prefix, not
-- from any query\/header\/cookie field a typed endpoint's own
-- 'HarchWeb.Api.RequestCodec' can decode), which is exactly the gap
-- 'HarchWeb.Api.apiRouteDefinitionWithContext' was added to close; see the
-- AC decision record in @docs\/design-guidance.md@.
--
-- @\/api\/status@ has no failure case, so it uses
-- 'HarchWeb.Api.apiRouteDefinitionWithContextNeverFailing' rather than
-- pairing 'HarchWeb.Api.apiRouteDefinitionWithContext' with
-- @Data.Void.Void@\/@Data.Void.absurd@: that combination looks precise but
-- traps this repository's 100%-coverage gate, since @either@ never forces a
-- failure-response argument on a @Right@, and no test can force a @Void@
-- one any other way — see 'HarchWeb.Api.Endpoint.apiRouteDefinitionWithContextNeverFailing's
-- own Haddock and the AC decision record for how this was found and why the
-- never-failing sibling primitive is the fix. @\/api\/second@ genuinely can
-- fail (its database call can), so its own domain failure carries the
-- database operations alongside the error so the failure-response mapping
-- can still attach the same query-timing diagnostics the equivalent page
-- route attaches.
--
-- @WebApi.Route@'s own path parsing\/rendering, method table, and
-- @\/api\/404@ handling are unchanged for @\/api\/status@, @\/api\/second@,
-- and @\/api\/me@: this module only supplies their response logic, wired in
-- by @WebApi.App@'s per-route dispatch. @\/api\/me@ reuses the existing
-- account profile's 'HarchWeb.RequireAuthenticated' guard rather than a new
-- authorization payload; see 'meApiRouteDefinition's own Haddock and the
-- AHI-4D decision record. @\/api\/oauth\/token@ additionally owns its request
-- decoding: an RFC 6749 client-credentials grant combines HTTP Basic client
-- authentication with a bounded URL-encoded form body, both already decoded
-- by 'HarchWeb.Authentication.oauth2ClientSecretBasicCodec' and
-- 'HarchWeb.Authentication.oauth2ClientCredentialsRequestCodec'
-- (AHI-4D slice 3). This endpoint only adapts those existing decoders and
-- 'WebApi.ApiClientToken.issueApiClientToken' (AHI-4D slice 4) to the typed
-- API boundary; see the AHI-4D decision record in @docs\/design-guidance.md@
-- for why its access requirement, error-body shape, and unavailable-outcome
-- collapsing were chosen this way.
--
-- AHI-4E (2026-09-23): the four API endpoints above are each built ONCE as a
-- 'SomeApiRouteEndpoint' carrying a real 'OpenApiExtension', so the same
-- value feeds both its runtime 'RouteDefinition' and the documented
-- 'webApiOpenApiMountedFamily' — no second, documentation-only declaration
-- exists. The family is interpreted into one cached document served through
-- 'docsOpenApiSpecRouteDefinition' at @GET \/docs\/openapi.json@; see the
-- AHI-4E decision records in @docs\/design-guidance.md@.
module WebApi.Api.Endpoints
  ( noApiRequestFields,
    meApiRouteDefinition,
    MeApiFailure (..),
    meApiOutcomeResponse,
    meApiFailureResponse,
    secondApiRouteDefinition,
    statusApiRouteDefinition,
    tokenApiRouteDefinition,
    TokenApiFailure (..),
    tokenApiOutcomeResponse,
    tokenApiFailureResponse,
    tokenApiMissingContentTypePolicy,
    requiredApiHeaderNameOrDie,
    requiredApiHeaderValueOrDie,
    requireOpenApiExtension,
    webApiOpenApiDocumentProvider,
    requireWebApiOpenApiDocumentProvider,
    webApiOpenApiSecuritySchemes,
    webApiOpenApiEndpointMetadataForPath,
    webApiApiRouteMount,
    appAuthorizationScopes,
    docsOpenApiSpecRouteDefinition,
  )
where

import Data.Aeson.Encoding qualified as JsonEncoding
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Api
  ( ApiEndpointContract (..),
    ApiEndpointRequest (..),
    ApiFieldFailurePolicy (ApiRenderFieldFailures, ApiUseGenericFieldFailure),
    ApiForm,
    ApiHeaderName,
    ApiHeaderValue,
    ApiMethod (ApiGet, ApiPost),
    ApiPath,
    ApiRequestBody (ApiNoRequestBody, ApiUrlEncodedFormRequestBody),
    ApiRequestBodyByteLimit,
    ApiRequestParseError,
    ApiResponse (..),
    ApiRouteEndpointDeclaration (..),
    MissingContentTypePolicy (RejectMissingContentType),
    RequestCodec,
    SomeApiRouteEndpoint (..),
    apiContentType,
    apiHeaderName,
    apiHeaderValue,
    apiPathText,
    apiResponse,
    apiRouteDefinition,
    apiRouteEndpointWithContext,
    apiRouteEndpointWithContextNeverFailing,
    at,
    bytesResponseEncoder,
    jsonMediaType,
    requireApiEndpointFamily,
    requireApiRequestBodyByteLimit,
  )
import HarchWeb.ApplicationModule (RouteMount (..))
import HarchWeb.Authentication
  ( OAuth2ClientCredentials,
    OAuth2ClientCredentialsMaximumBytes,
    OAuth2ClientCredentialsRequest,
    ScopeRequirement (RequireAllScopes, RequireAnyScope),
    encodedJwtBytes,
    oauth2ClientCredentialsRequestCodec,
    oauth2ClientCredentialsScopes,
    oauth2ClientSecretBasicCodec,
    oauth2ScopeText,
    requiredOAuth2ClientCredentialsMaximumBytesOrDie,
  )
import HarchWeb.EndpointSecurity (AuthenticationProfileName, EndpointMetadata (endpointRouteTemplate), routeTemplateText)
import HarchWeb.OpenApi
  ( OpenApiDocumentDetails (..),
    OpenApiDocumentFailure,
    OpenApiDocumentProvider,
    OpenApiExtension,
    OpenApiExtensionError,
    OpenApiMountedFamily,
    OpenApiSecurityScheme,
    mkCachedOpenApiDocumentProvider,
    mkOpenApiExtension,
    mkOpenApiHttpBearerSecurityScheme,
    openApiDocumentRouteDefinition,
    openApiMountedFamily,
    renderOpenApiDocumentFailure,
  )
import HarchWeb.Routing (PathSegment, pathSegmentText, requiredPathSegment)
import HarchWeb.SecurityEvent (requiredModuleNameOrDie)
import HarchWeb.Site (RouteDefinition)
import Network.HTTP.Types qualified as HttpTypes
import Numeric.Natural (Natural)
import WebApi.Account (AccountProfile, AccountProfileStore)
import WebApi.ApiClientToken
  ( ApiClientTokenEnvironment,
    ApiClientTokenOutcome (..),
    issueApiClientToken,
  )
import WebApi.Database
  ( DatabaseError,
    DatabaseOperation,
    PageRepository,
    databaseResultOperations,
    databaseResultValue,
    loadSecondPage,
    secondPageDataHighlights,
    secondPageDataSummary,
  )
import WebApi.Profile (ProfileLoadError (..), ProfileState (..), loadProfileForPrincipal)
import WebApi.Response
  ( FailureSurface (ApiFailureSurface),
    diagnosticsDatabaseOperations,
    diagnosticsLogEntries,
    diagnosticsObservabilityAttributes,
    jsonErrorBody,
    meApiSuccessBody,
    pageFailureDiagnostics,
    secondRouteApiBody,
    statusApiBody,
    toHarchDatabaseOperation,
    tokenApiSuccessBody,
  )
import WebApi.Route
  ( AppAuthorization,
    AppRequestContext (requestAccountPrincipal),
    AppRoute (DocsOpenApiSpecRoute, MeApiRoute, SecondApiRoute, StatusApiRoute, TokenApiRoute),
    accountAuthenticationProfileName,
    defaultRequestContext,
    endpointMetadata,
    requestLocale,
    resourceAuthenticationProfileName,
  )
import WebApi.RouteData (SecondRouteData (..))

-- | Neither @\/api\/status@ nor @\/api\/second@ decodes any query, header, or
-- cookie field, so both endpoints below share this one declaration rather
-- than each writing their own @pure ()@. Exported so a Unit test can decode
-- it directly with 'HarchWeb.Api.runRequestCodec' and compare the result:
-- neither endpoint's own handler reads its decoded fields (there are none
-- to read), so routing a request through the full endpoint only pattern
-- matches the 'ApiRequestDecoded' constructor, never forcing the @()@
-- payload itself. A Unit test therefore decodes this declaration directly
-- and demands that value. See the AC decision record in
-- @docs/design-guidance.md@.
noApiRequestFields :: RequestCodec ()
noApiRequestFields = pure ()

-- | The one structural prefix web-api's documented API family mounts under.
-- It mirrors the literal @api@ path segment @WebApi.Route@ already dispatches
-- on, but documentation observes it exactly once here: both the mounted
-- family's paths and every local declaration below are derived from this
-- value and each endpoint's own real @endpointRouteTemplate@, so a
-- documented path can never disagree with where the endpoint actually
-- lives. See the AHI-4E decision record in @docs\/design-guidance.md@.
webApiApiMountPrefix :: NonEmpty.NonEmpty PathSegment
webApiApiMountPrefix = requiredPathSegment "api" NonEmpty.:| []

-- | 'webApiApiMountPrefix' rendered as its path form (@\"\/api\"@), used to
-- project a full route template down to its family-local declaration path.
webApiApiMountPrefixText :: Text.Text
webApiApiMountPrefixText = "/" <> pathSegmentText (NonEmpty.head webApiApiMountPrefix)

-- | Build a documented endpoint declaration from the SAME real
-- 'EndpointMetadata' value @WebApi.Route.endpointMetadata@ hands to runtime
-- dispatch, deriving the family-local path by removing the mount prefix
-- rather than authoring a second copy of the path (see
-- 'webApiApiMountPrefix'). A template that does not sit under the mount
-- prefix still produces a total, invalid local path that
-- 'HarchWeb.OpenApi.mountedOperationPath' rejects as a typed construction
-- failure at provider startup — never a silently wrong documented path.
-- The declaration path is documentation-only: 'HarchWeb.Api.apiRouteDefinition'
-- reads the method and availability, while route selection itself remains
-- @WebApi.Route@'s codec. See the AHI-4E decision record in
-- @docs\/design-guidance.md@.
apiDocumentedDeclaration :: EndpointMetadata authorization -> ApiEndpointContract extension fields body response -> ApiRouteEndpointDeclaration extension fields body response
apiDocumentedDeclaration metadata =
  ApiRouteEndpointDeclaration (at (Text.drop (Text.length webApiApiMountPrefixText) (routeTemplateText (endpointRouteTemplate metadata))))

-- | Unwrap one statically authored documentation extension. Every call below
-- passes compile-time literals that only fail validation for a genuinely
-- malformed authored value (a duplicate specification-extension name), so
-- this keeps the ordinary success path direct while leaving the failure rail
-- a total, directly testable boundary — exported so @Unit.WebApi.Api.EndpointsSpec@
-- can exercise it against a real invalid value instead of it staying an
-- unreachable @error@ branch, matching 'requiredApiHeaderNameOrDie'.
requireOpenApiExtension :: Either OpenApiExtensionError (OpenApiExtension fields body response) -> OpenApiExtension fields body response
requireOpenApiExtension =
  either
    (error . ("web-api authored an invalid OpenAPI extension: " <>) . show)
    id

statusApiContract :: ApiEndpointContract OpenApiExtension () () ByteString.ByteString
statusApiContract =
  ApiEndpointContract
    ApiGet
    noApiRequestFields
    ApiNoRequestBody
    (bytesResponseEncoder (apiContentType jsonMediaType) :| [])
    ApiUseGenericFieldFailure
    statusApiExtension

statusApiExtension :: OpenApiExtension () () ByteString.ByteString
statusApiExtension = requireOpenApiExtension (mkOpenApiExtension (Just "Anonymous application status check.") Nothing [] False [])

statusApiEndpoint :: SomeApiRouteEndpoint AppRequestContext OpenApiExtension
statusApiEndpoint =
  SomeApiRouteEndpoint (apiRouteEndpointWithContextNeverFailing (apiDocumentedDeclaration (endpointMetadata StatusApiRoute) statusApiContract) statusApiHandler)

statusApiHandler :: AppRequestContext -> ApiEndpointRequest () () -> IO (ApiResponse ByteString.ByteString)
statusApiHandler requestContext _endpointRequest = pure (apiResponse (jsonBytes (statusApiBody (requestLocale requestContext))))

statusApiRouteDefinition :: RouteDefinition AppRoute AppRequestContext AppAuthorization
statusApiRouteDefinition =
  case statusApiEndpoint of
    SomeApiRouteEndpoint endpoint -> apiRouteDefinition (endpointMetadata StatusApiRoute) endpoint

-- | @\/api\/me@: the requesting account's own username and email.
-- 'WebApi.Route.endpointMetadata' declares this route through the existing
-- account profile's 'HarchWeb.RequireAuthenticated' guard, so
-- 'requestAccountPrincipal' is already established by the time this handler
-- runs; @loadProfileForPrincipal@ still takes the 'Maybe' honestly rather
-- than partially unwrapping it, since nothing here can re-prove the guard
-- ran. See the AHI-4D decision record in @docs\/design-guidance.md@ for why
-- this reuses the account profile instead of a new authorization payload.
meApiContract :: ApiEndpointContract OpenApiExtension () () ByteString.ByteString
meApiContract =
  ApiEndpointContract
    ApiGet
    noApiRequestFields
    ApiNoRequestBody
    (bytesResponseEncoder (apiContentType jsonMediaType) :| [])
    ApiUseGenericFieldFailure
    meApiExtension

-- | Real synthetic username/email values in the example response are the
-- endpoint's requested resource (see the task file's "web-api documentation"
-- section); the real response stays @private, no-store@ regardless.
meApiExtension :: OpenApiExtension () () ByteString.ByteString
meApiExtension = requireOpenApiExtension (mkOpenApiExtension (Just "The authenticated account's own profile.") Nothing [] False [])

-- | Composition-time dependency realization: the store record is demanded to
-- WHNF when this endpoint value is composed (in either the runtime route
-- definition or the documented family), mirroring the provider binding's
-- startup-failure posture — a malformed or unexpectedly lazy top-level
-- dependency fails at composition instead of first dispatch. The handler
-- still consumes the record's fields lazily.
meApiEndpoint :: AccountProfileStore -> SomeApiRouteEndpoint AppRequestContext OpenApiExtension
meApiEndpoint !profileStore =
  SomeApiRouteEndpoint (apiRouteEndpointWithContext (apiDocumentedDeclaration (endpointMetadata MeApiRoute) meApiContract) (meApiHandler profileStore) meApiFailureResponse)

meApiHandler :: AccountProfileStore -> AppRequestContext -> ApiEndpointRequest () () -> IO (Either MeApiFailure (ApiResponse ByteString.ByteString))
meApiHandler profileStore requestContext _endpointRequest =
  meApiOutcomeResponse <$> loadProfileForPrincipal profileStore (requestAccountPrincipal requestContext)

meApiRouteDefinition :: AccountProfileStore -> RouteDefinition AppRoute AppRequestContext AppAuthorization
meApiRouteDefinition profileStore =
  case meApiEndpoint profileStore of
    SomeApiRouteEndpoint endpoint -> apiRouteDefinition (endpointMetadata MeApiRoute) endpoint

-- | @\/api\/me@ has one public failure shape: the account-self resource is
-- momentarily unavailable. A genuinely unauthenticated caller never reaches
-- this handler (the account profile's guard already halted the request), so
-- 'ProfileUnauthenticated' here can only mean the durable profile lookup
-- disagreed with an already-established session — a store inconsistency,
-- not a public 401/403 outcome.
data MeApiFailure = MeApiUnavailable

-- | Exported (alongside 'MeApiFailure' and 'meApiFailureResponse') so a test
-- can exercise every 'ProfileState'\/'ProfileLoadError' outcome directly.
-- The real end-to-end route can only ever demonstrate the outcome its one
-- WAI-level fixture account happens to be in; a genuinely unauthenticated
-- caller and a durable profile-store failure are both guard-boundary or
-- infrastructure conditions this module cannot manufacture through a real
-- request, matching the same testing shape already used for
-- 'tokenApiOutcomeResponse'.
meApiOutcomeResponse :: Either ProfileLoadError ProfileState -> Either MeApiFailure (ApiResponse ByteString.ByteString)
meApiOutcomeResponse result =
  case result of
    Left (ProfileAccountStoreError _) -> Left MeApiUnavailable
    Right ProfileUnauthenticated -> Left MeApiUnavailable
    Right (ProfilePending profile) -> Right (meApiProfileResponse profile)
    Right (ProfileAuthenticated profile) -> Right (meApiProfileResponse profile)

meApiProfileResponse :: AccountProfile -> ApiResponse ByteString.ByteString
meApiProfileResponse profile =
  (apiResponse (jsonBytes (meApiSuccessBody profile)))
    { apiEndpointResponseHeaders = [(requiredApiHeaderNameOrDie "Cache-Control", requiredApiHeaderValueOrDie "private, no-store")]
    }

meApiFailureResponse :: MeApiFailure -> ApiResponse ByteString.ByteString
meApiFailureResponse MeApiUnavailable =
  (apiResponse (jsonBytes (jsonErrorBody "profile-unavailable")))
    { apiEndpointResponseStatus = HttpTypes.status503
    }

secondApiContract :: ApiEndpointContract OpenApiExtension () () ByteString.ByteString
secondApiContract =
  ApiEndpointContract
    ApiGet
    noApiRequestFields
    ApiNoRequestBody
    (bytesResponseEncoder (apiContentType jsonMediaType) :| [])
    ApiUseGenericFieldFailure
    secondApiExtension

secondApiExtension :: OpenApiExtension () () ByteString.ByteString
secondApiExtension = requireOpenApiExtension (mkOpenApiExtension (Just "Second-page resource data.") Nothing [] False [])

-- | See 'meApiEndpoint' for why the repository record is demanded here: the
-- same value feeds the runtime route and the documented family, so its
-- composition-time dependency is realized exactly once, at composition.
secondApiEndpoint :: PageRepository -> SomeApiRouteEndpoint AppRequestContext OpenApiExtension
secondApiEndpoint !pageRepository =
  SomeApiRouteEndpoint (apiRouteEndpointWithContext (apiDocumentedDeclaration (endpointMetadata SecondApiRoute) secondApiContract) (secondApiHandler pageRepository) secondApiFailureResponse)

secondApiHandler :: PageRepository -> AppRequestContext -> ApiEndpointRequest () () -> IO (Either SecondApiFailure (ApiResponse ByteString.ByteString))
secondApiHandler pageRepository requestContext _endpointRequest = do
  secondPageResult <- loadSecondPage pageRepository (requestLocale requestContext)
  let databaseOperations = databaseResultOperations secondPageResult
  pure $ case databaseResultValue secondPageResult of
    Right secondPageData ->
      Right
        ( (apiResponse (jsonBytes (secondRouteApiBody (toSecondRouteData secondPageData))))
            { apiEndpointResponseDatabaseOperations = map toHarchDatabaseOperation databaseOperations
            }
        )
    Left databaseError -> Left (SecondApiFailure databaseOperations databaseError)
  where
    toSecondRouteData secondPageData =
      SecondRouteData (secondPageDataSummary secondPageData) (secondPageDataHighlights secondPageData)

secondApiRouteDefinition :: PageRepository -> RouteDefinition AppRoute AppRequestContext AppAuthorization
secondApiRouteDefinition pageRepository =
  case secondApiEndpoint pageRepository of
    SomeApiRouteEndpoint endpoint -> apiRouteDefinition (endpointMetadata SecondApiRoute) endpoint

-- | The RFC 6749 client-credentials grant decoded as one endpoint request:
-- HTTP Basic client authentication (a header field) alongside the grant-type
-- and scope form fields. Both codecs are AHI-4D slice 3's existing decoders;
-- combining them with 'Control.Applicative.liftA2' here — rather than either
-- decoder growing a second concern — keeps each one focused on the RFC
-- component it already owns.
tokenApiRequestFields :: RequestCodec (OAuth2ClientCredentials, OAuth2ClientCredentialsRequest)
tokenApiRequestFields =
  (,)
    <$> oauth2ClientSecretBasicCodec tokenApiMaximumBasicBytes
    <*> oauth2ClientCredentialsRequestCodec

-- | Bounds chosen for one small, fixed-shape token request (a Basic header
-- plus @grant_type@ and an optional @scope@ field), not for an arbitrary API
-- payload: generous enough for any realistic client ID/secret pair and scope
-- list, small enough to keep a malformed or hostile request cheap to reject.
tokenApiMaximumBasicBytes :: OAuth2ClientCredentialsMaximumBytes
tokenApiMaximumBasicBytes = requiredOAuth2ClientCredentialsMaximumBytesOrDie 4096

tokenApiRequestBodyByteLimit :: ApiRequestBodyByteLimit
tokenApiRequestBodyByteLimit = requireApiRequestBodyByteLimit 2048

tokenApiRequestMaximumFields :: Natural
tokenApiRequestMaximumFields = 8

-- | RFC 6749 requires a client to send this grant's fixed body as
-- @application\/x-www-form-urlencoded@; a request with no @Content-Type@ at
-- all is therefore rejected rather than assumed, unlike an endpoint whose
-- body shape a missing header could reasonably default.
-- "HarchWeb.Api"'s 'HarchWeb.Api.selectApiBodyDecoder' only ever pattern
-- matches this value when the incoming request omits @Content-Type@
-- entirely (see its @Nothing -> case missingPolicy of ...@ branch): every
-- request that declares a content type never forces it at all, so this
-- declaration needs its own missing-@Content-Type@ test to be genuinely
-- exercised — see @Unit.WebApi.Api.EndpointsSpec@'s direct 'Eq' assertion
-- and @Unit.WebApi.AppSpec@'s "buildRuntimeApp" real end-to-end request that
-- omits the header and expects 415.
tokenApiMissingContentTypePolicy :: MissingContentTypePolicy
tokenApiMissingContentTypePolicy = RejectMissingContentType

-- | @\/api\/oauth\/token@: RFC 6749 section 4.4 client-credentials grant.
-- 'WebApi.Route.endpointMetadata' declares this route 'AllowUnauthenticated'
-- because the OAuth client authenticates itself inside this handler (HTTP
-- Basic verified by 'issueApiClientToken' against the durable API-client
-- store), not through the account session/bearer-JWT rail every other
-- protected route uses; see the AHI-4D decision record in
-- @docs\/design-guidance.md@.
-- | RFC 6749 @\/api\/oauth\/token@: documented alongside the surface it
-- serves, as the task's @web-api@ documentation section requires. The
-- operation itself stays anonymous ('AllowUnauthenticated' in
-- 'WebApi.Route.endpointMetadata'): the OAuth *client* authenticates inside
-- the grant, so the document's @security@ array truthfully stays empty
-- rather than inventing a flow the runtime does not enforce here.
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
tokenApiExtension = requireOpenApiExtension (mkOpenApiExtension (Just "Issue an OAuth 2.0 access token for a registered API client (RFC 6749 client credentials).") Nothing [] False [])

-- | See 'meApiEndpoint' for why the token-issuance environment is demanded
-- here.
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

-- | RFC 6749 clients need a stable @invalid_request@ body for a rejected
-- token request, but parse failures can include the missing or malformed
-- credential source.  Keep those details private and render every field
-- rejection as the same opaque 400 response.
tokenApiInvalidRequestResponse :: [ApiRequestParseError] -> ApiResponse ByteString.ByteString
tokenApiInvalidRequestResponse _ =
  (tokenApiFailureResponse TokenApiInvalidScope)
    { apiEndpointResponseValue = jsonBytes (jsonErrorBody "invalid_request")
    }

-- | The three public shapes an OAuth client can be told: a successful token,
-- an RFC 6749 section 5.2 protocol rejection ('TokenApiInvalidClient',
-- 'TokenApiInvalidScope'), or a generic unavailable outcome. Every
-- non-issuance 'ApiClientTokenOutcome' collapses to 'TokenApiUnavailable' on
-- purpose: 'WebApi.ApiClientToken' already documents why a work-budget
-- exhaustion must stay indistinguishable from a genuine rejection
-- internally, and the same property must hold at this public boundary, so a
-- durable-store outage, an exhausted Argon2 work budget, and a signing
-- failure all render the same generic 503 rather than leaking which one
-- occurred.
data TokenApiFailure
  = TokenApiInvalidClient
  | TokenApiInvalidScope
  | TokenApiUnavailable

-- | Exported (alongside 'TokenApiFailure') so a test can exercise this
-- mapping directly with a deliberately malformed
-- 'HarchWeb.Authentication.EncodedJwt' (built via the also-exported
-- 'HarchWeb.Authentication.encodedJwtFromBytes', matching how
-- @HarchWeb.AuthenticationSpec@ already tests
-- 'HarchWeb.Authentication.renderAuthenticationCookie's identical
-- UTF-8-decode boundary). A real RS256-signed compact JWT can never take the
-- 'Left' branch below — RFC 7515's compact serialization is always ASCII —
-- so there is no way to reach it end-to-end through the real signer; this
-- direct test is the only way to keep it genuinely exercised rather than
-- coverage-gated by an ignore pragma.
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

-- | RFC 6749 section 5.2 requires the @WWW-Authenticate@ challenge on a
-- rejected client that attempted HTTP Basic authentication; this endpoint
-- accepts no other client authentication method, so it always names Basic.
-- Built once from literals known valid at compile time, the same
-- \"required-or-die\" shape this codebase already uses for other
-- always-valid declared literals (see e.g.
-- 'HarchWeb.EndpointSecurity.requiredEndpointNameOrDie', whose own test
-- proves its @error@ branch by deliberately passing an invalid literal —
-- 'requiredApiHeaderNameOrDie'\/'requiredApiHeaderValueOrDie' below are
-- exported for @Unit.WebApi.Api.EndpointsSpec@ to test the same way, since
-- neither always-valid literal below can ever trigger its own @error@
-- branch).
wwwAuthenticateHeaderName :: ApiHeaderName
wwwAuthenticateHeaderName = requiredApiHeaderNameOrDie "WWW-Authenticate"

basicChallengeHeaderValue :: ApiHeaderValue
basicChallengeHeaderValue = requiredApiHeaderValueOrDie "Basic"

requiredApiHeaderNameOrDie :: Text.Text -> ApiHeaderName
requiredApiHeaderNameOrDie value = fromMaybe (error ("invalid API header name literal: " <> Text.unpack value)) (apiHeaderName value)

requiredApiHeaderValueOrDie :: Text.Text -> ApiHeaderValue
requiredApiHeaderValueOrDie value = fromMaybe (error ("invalid API header value literal: " <> Text.unpack value)) (apiHeaderValue value)

data SecondApiFailure = SecondApiFailure [DatabaseOperation] DatabaseError

secondApiFailureResponse :: SecondApiFailure -> ApiResponse ByteString.ByteString
secondApiFailureResponse (SecondApiFailure databaseOperations databaseError) =
  (apiResponse (jsonBytes (jsonErrorBody "second-page-unavailable")))
    { apiEndpointResponseStatus = HttpTypes.status503,
      apiEndpointResponseObservabilityAttributes = diagnosticsObservabilityAttributes diagnostics,
      apiEndpointResponseLogEntries = diagnosticsLogEntries diagnostics,
      apiEndpointResponseDatabaseOperations = diagnosticsDatabaseOperations diagnostics
    }
  where
    diagnostics = pageFailureDiagnostics ApiFailureSurface "/second" "second-page" databaseOperations databaseError

-- | Render a JSON body through the same pure encoders "WebApi.Response"
-- already uses for these two payloads, keeping the typed endpoint's bytes
-- identical to the pre-migration response. Goes straight from the encoding
-- to bytes rather than through 'WebApi.Response.jsonText' and back, since
-- that 'Text' detour served no purpose here and added a partial
-- 'TextEncoding.decodeUtf8' on a request path.
jsonBytes :: JsonEncoding.Encoding -> ByteString.ByteString
jsonBytes = LazyByteString.toStrict . JsonEncoding.encodingToLazyByteString

-- ---------------------------------------------------------------------------
-- AHI-4E: web-api's documented API surface
--
-- One 'OpenApiMountedFamily' aggregates the four documented endpoint values
-- above under the real @\/api@ mount, and every operation's OpenAPI
-- @security@ is derived from the same 'WebApi.Route.endpointMetadata' value
-- that governs real enforcement ('webApiOpenApiEndpointMetadataForPath' plus
-- 'appAuthorizationScopes') — there is no docs-only security vocabulary.
-- The resulting immutable document is served through the ordinary typed
-- route adapter at @\/docs\/openapi.json@ ('docsOpenApiSpecRouteDefinition').
-- The complete SSR Swagger page, self-hosted assets, and OAuth panel remain
-- the later AHI-4E slices. See the AHI-4E decision records in
-- @docs\/design-guidance.md@.
-- ---------------------------------------------------------------------------

-- | The closed set of routes whose endpoints are documented. Kept beside the
-- family so a newly documented endpoint that is forgotten here fails at
-- provider startup ('webApiOpenApiEndpointMetadataForPath's error rail)
-- rather than being silently dropped from the document.
webApiDocumentedRoutes :: [AppRoute]
webApiDocumentedRoutes = [StatusApiRoute, SecondApiRoute, MeApiRoute, TokenApiRoute]

-- | The structural mount web-api's documented family is recorded under. The
-- prism is the identity on this closed route type: web-api composes no child
-- module at runtime — 'WebApi.Route' dispatches each route directly — so
-- this value exists to record the one structural prefix already true of the
-- real paths, not to install a second dispatcher. The mount name namespaces
-- only module-chain attribution; documentation reads the prefix.
webApiApiRouteMount :: RouteMount AppRoute AppRoute
webApiApiRouteMount =
  RouteMount
    { routeMountName = requiredModuleNameOrDie "web-api",
      routeMountPrefix = webApiApiMountPrefix,
      embedChildRoute = id,
      projectChildRoute = Just
    }

-- | Resolve a family-local declaration path (e.g. @\/status@) back to the
-- ONE real 'EndpointMetadata' that route dispatch already uses, by
-- re-projecting the local path under the mount prefix and comparing full
-- route templates — no second path table and no partial stripping. An
-- unknown family path is an authored-table defect and fails document
-- construction at startup; exported so a Unit test exercises that rail
-- directly against a genuinely unknown path.
webApiOpenApiEndpointMetadataForPath :: ApiPath -> EndpointMetadata AppAuthorization
webApiOpenApiEndpointMetadataForPath apiPath =
  case [ metadata
       | route <- webApiDocumentedRoutes,
         let metadata = endpointMetadata route,
         routeTemplateText (endpointRouteTemplate metadata) == webApiApiMountPrefixText <> apiPathText apiPath
       ] of
    metadata : _ -> metadata
    [] ->
      error
        ( "web-api documents no API endpoint at family path "
            <> Text.unpack (webApiApiMountPrefixText <> apiPathText apiPath)
        )

-- | Project one endpoint's real authorization value into the OpenAPI scope
-- names it demands. Both 'RequireAllScopes' and 'RequireAnyScope' are
-- rendered as the same scope-name list because the document records WHICH
-- scopes the runtime requirement names; the all-versus-any enforcement
-- distinction stays entirely in the real guard, where it already lives.
appAuthorizationScopes :: AppAuthorization -> [Text.Text]
appAuthorizationScopes scopeRequirement =
  case scopeRequirement of
    RequireAllScopes scopes -> oauth2ScopeText <$> NonEmpty.toList scopes
    RequireAnyScope scopes -> oauth2ScopeText <$> NonEmpty.toList scopes

-- | The document-level profile-to-scheme map keyed by the exact
-- 'AuthenticationProfileName' real routing uses. Both real profiles admit
-- the same credential shape at one transport boundary — a signed JWT
-- presented as a bearer @Authorization@ header — so both map to the closed
-- HTTP-bearer scheme; the account profile *additionally* accepts its
-- @__Host-@ session cookie, a cookie-or-bearer union that lives at this
-- application's transport boundary and cannot be expressed by any single
-- OpenAPI scheme (see the follow-up decision record in
-- 'HarchWeb.OpenApi.Security'). An OAuth2 client-credentials scheme for the
-- token flow needs an application-supplied absolute @https@ token URL — the
-- public origin this example does not configure — and is deliberately left
-- to the Swagger UI OAuth-panel slice that actually requires it.
webApiOpenApiSecuritySchemes :: Map AuthenticationProfileName OpenApiSecurityScheme
webApiOpenApiSecuritySchemes =
  Map.fromList
    [ (accountAuthenticationProfileName, jwtBearerScheme),
      (resourceAuthenticationProfileName, jwtBearerScheme)
    ]
  where
    jwtBearerScheme = mkOpenApiHttpBearerSecurityScheme (Just "JWT")

-- | Document identity. The version tracks the @haskell-web-api@ package
-- version this example application ships, so a version bump that changes the
-- API surface moves the published document's version with it.
webApiOpenApiDocumentDetails :: OpenApiDocumentDetails
webApiOpenApiDocumentDetails =
  OpenApiDocumentDetails
    { openApiDocumentTitle = "Harch Web API",
      openApiDocumentVersion = "0.1.2.0"
    }

-- | Build the startup-cached provider for web-api's one documented family
-- from an explicit @defaultRequestContext@ availability snapshot: validation,
-- generation, and encoding all happen here (once), never per request, and a
-- construction failure is surfaced by 'requireWebApiOpenApiDocumentProvider'
-- as application startup failure rather than a stale or malformed document.
webApiOpenApiDocumentProvider :: PageRepository -> AccountProfileStore -> ApiClientTokenEnvironment -> Either OpenApiDocumentFailure (OpenApiDocumentProvider AppRequestContext)
webApiOpenApiDocumentProvider pageRepository profileStore tokenEnvironment =
  mkCachedOpenApiDocumentProvider
    webApiOpenApiDocumentDetails
    webApiOpenApiSecuritySchemes
    defaultRequestContext
    [webApiOpenApiMountedFamily pageRepository profileStore tokenEnvironment]

-- | The documented family itself: the same four endpoint values that feed
-- the real per-route 'RouteDefinition's above, aggregated once for
-- documentation. 'requireApiEndpointFamily' re-checks that the declaration
-- table is non-empty and duplicate-free; every operation's security is
-- derived from 'webApiOpenApiEndpointMetadataForPath' and
-- 'appAuthorizationScopes' over the mount recorded in 'webApiApiRouteMount'.
webApiOpenApiMountedFamily :: PageRepository -> AccountProfileStore -> ApiClientTokenEnvironment -> OpenApiMountedFamily AppRequestContext
webApiOpenApiMountedFamily pageRepository profileStore tokenEnvironment =
  openApiMountedFamily
    webApiApiRouteMount
    ( requireApiEndpointFamily
        [ statusApiEndpoint,
          secondApiEndpoint pageRepository,
          meApiEndpoint profileStore,
          tokenApiEndpoint tokenEnvironment
        ]
    )
    webApiOpenApiEndpointMetadataForPath
    appAuthorizationScopes

-- | Resolve the application's static documentation provider, naming the
-- typed construction failure if the authored document is invalid. Exported
-- so its failure rail is directly testable against a synthetic failure, and
-- applied exactly once at application composition (see the bang binding in
-- 'WebApi.App.buildAppWithDatabaseAndOptionalReportersAndSecurity') so a
-- malformed document is startup failure, never a first-request surprise.
requireWebApiOpenApiDocumentProvider :: Either OpenApiDocumentFailure (OpenApiDocumentProvider context) -> OpenApiDocumentProvider context
requireWebApiOpenApiDocumentProvider =
  either (error . Text.unpack . renderOpenApiDocumentFailure) id

-- | @GET \/docs\/openapi.json@: the ordinary typed route adapter from
-- 'HarchWeb.OpenApi.Route' over the application-selected provider, declared
-- with this route's own 'endpointMetadata' like every other route here.
-- Access stays this route's own 'AllowUnauthenticated' choice (the task's
-- reference-example default); HEAD and OPTIONS come from the shared
-- dispatcher, and a provider construction failure was already raised at
-- startup before any request could reach this handler.
docsOpenApiSpecRouteDefinition :: OpenApiDocumentProvider AppRequestContext -> RouteDefinition AppRoute AppRequestContext AppAuthorization
docsOpenApiSpecRouteDefinition =
  openApiDocumentRouteDefinition (endpointMetadata DocsOpenApiSpecRoute)
