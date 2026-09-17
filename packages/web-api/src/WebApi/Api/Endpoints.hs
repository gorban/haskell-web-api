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
-- @\/api\/404@ handling are unchanged for @\/api\/status@ and @\/api\/second@:
-- this module only supplies their response logic, wired in by @WebApi.App@'s
-- per-route dispatch. @\/api\/oauth\/token@ additionally owns its request
-- decoding: an RFC 6749 client-credentials grant combines HTTP Basic client
-- authentication with a bounded URL-encoded form body, both already decoded
-- by 'HarchWeb.Authentication.oauth2ClientSecretBasicCodec' and
-- 'HarchWeb.Authentication.oauth2ClientCredentialsRequestCodec'
-- (AHI-4D slice 3). This endpoint only adapts those existing decoders and
-- 'WebApi.ApiClientToken.issueApiClientToken' (AHI-4D slice 4) to the typed
-- API boundary; see the AHI-4D decision record in @docs\/design-guidance.md@
-- for why its access requirement, error-body shape, and unavailable-outcome
-- collapsing were chosen this way.
module WebApi.Api.Endpoints
  ( noApiRequestFields,
    secondApiRouteDefinition,
    statusApiRouteDefinition,
    tokenApiRouteDefinition,
    TokenApiFailure (..),
    tokenApiOutcomeResponse,
    tokenApiFailureResponse,
    tokenApiMissingContentTypePolicy,
    requiredApiHeaderNameOrDie,
    requiredApiHeaderValueOrDie,
  )
where

import Data.Aeson.Encoding qualified as JsonEncoding
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Api
  ( ApiEndpointContract (..),
    ApiEndpointRequest (..),
    ApiFieldFailurePolicy (ApiUseGenericFieldFailure),
    ApiHeaderName,
    ApiHeaderValue,
    ApiMethod (ApiGet, ApiPost),
    ApiRequestBody (ApiNoRequestBody, ApiUrlEncodedFormRequestBody),
    ApiRequestBodyByteLimit,
    ApiResponse (..),
    MissingContentTypePolicy (RejectMissingContentType),
    RequestCodec,
    apiContentType,
    apiHeaderName,
    apiHeaderValue,
    apiResponse,
    apiRouteDefinitionWithContext,
    apiRouteDefinitionWithContextNeverFailing,
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
import HarchWeb.Site (RouteDefinition)
import Network.HTTP.Types qualified as HttpTypes
import Numeric.Natural (Natural)
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
import WebApi.Response
  ( FailureSurface (ApiFailureSurface),
    diagnosticsDatabaseOperations,
    diagnosticsLogEntries,
    diagnosticsObservabilityAttributes,
    jsonErrorBody,
    pageFailureDiagnostics,
    secondRouteApiBody,
    statusApiBody,
    toHarchDatabaseOperation,
    tokenApiSuccessBody,
  )
import WebApi.Route (AppRequestContext, AppRoute (SecondApiRoute, StatusApiRoute, TokenApiRoute), endpointMetadata, requestLocale)
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

statusApiRouteDefinition :: RouteDefinition AppRoute AppRequestContext ()
statusApiRouteDefinition =
  apiRouteDefinitionWithContextNeverFailing
    ( ApiEndpointContract
        ApiGet
        noApiRequestFields
        ApiNoRequestBody
        (bytesResponseEncoder (apiContentType jsonMediaType) :| [])
        ApiUseGenericFieldFailure
    )
    (endpointMetadata StatusApiRoute)
    (\requestContext _endpointRequest -> pure (apiResponse (jsonBytes (statusApiBody (requestLocale requestContext)))))

secondApiRouteDefinition :: PageRepository -> RouteDefinition AppRoute AppRequestContext ()
secondApiRouteDefinition pageRepository =
  apiRouteDefinitionWithContext
    ( ApiEndpointContract
        ApiGet
        noApiRequestFields
        ApiNoRequestBody
        (bytesResponseEncoder (apiContentType jsonMediaType) :| [])
        ApiUseGenericFieldFailure
    )
    (endpointMetadata SecondApiRoute)
    ( \requestContext _endpointRequest -> do
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
    )
    secondApiFailureResponse
  where
    toSecondRouteData secondPageData =
      SecondRouteData (secondPageDataSummary secondPageData) (secondPageDataHighlights secondPageData)

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
tokenApiRouteDefinition :: ApiClientTokenEnvironment -> RouteDefinition AppRoute AppRequestContext ()
tokenApiRouteDefinition environment =
  apiRouteDefinitionWithContext
    ( ApiEndpointContract
        ApiPost
        tokenApiRequestFields
        ( ApiUrlEncodedFormRequestBody
            tokenApiMissingContentTypePolicy
            tokenApiRequestBodyByteLimit
            tokenApiRequestMaximumFields
        )
        (bytesResponseEncoder (apiContentType jsonMediaType) :| [])
        ApiUseGenericFieldFailure
    )
    (endpointMetadata TokenApiRoute)
    ( \_requestContext endpointRequest ->
        let (credentials, tokenRequest) = apiEndpointRequestFields endpointRequest
         in tokenApiOutcomeResponse <$> issueApiClientToken environment credentials (oauth2ClientCredentialsScopes tokenRequest)
    )
    tokenApiFailureResponse

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
