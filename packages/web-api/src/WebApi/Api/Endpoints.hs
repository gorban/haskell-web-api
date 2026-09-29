{-# LANGUAGE BangPatterns #-}

-- | @\/api\/status@ and @\/api\/second@ composed through
-- "HarchWeb.Api.Endpoint"'s typed endpoint boundary rather than the
-- hand-rolled 'WebApi.Route.ApiRoute' dispatch in "WebApi.Response". Both
-- need the request's own resolved locale (derived from a URL prefix, not
-- from any query\/header\/cookie field a typed endpoint's own
-- 'HarchWeb.Api.RequestCodec' can decode), which is exactly the gap
-- 'HarchWeb.Api.apiRouteDefinitionWithContext' was added to close; see the
-- typed declarative endpoint boundary decision record in @docs\/design-guidance.md@.
--
-- @\/api\/status@ has no failure case, so it uses
-- 'HarchWeb.Api.apiRouteDefinitionWithContextNeverFailing' rather than
-- pairing 'HarchWeb.Api.apiRouteDefinitionWithContext' with
-- @Data.Void.Void@\/@Data.Void.absurd@: that combination looks precise but
-- traps this repository's 100%-coverage gate, since @either@ never forces a
-- failure-response argument on a @Right@, and no test can force a @Void@
-- one any other way — see 'HarchWeb.Api.Endpoint.apiRouteDefinitionWithContextNeverFailing's
-- own Haddock and the typed declarative endpoint boundary decision record
-- for how this was found and why the
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
-- scoped API-authentication design's decision record. The RFC 6749
-- @\/api\/oauth\/token@ vertical slice lives in
-- "WebApi.Api.Endpoints.OAuthToken": that route combines Basic authentication,
-- a bounded URL-form body, token issuance, and protocol-specific failure
-- rendering. This module re-exports the exact token endpoint value alongside
-- the other application endpoints so runtime composition and OpenAPI
-- interpretation share it. Shared metadata, header-literal, OpenAPI-extension,
-- and JSON-byte construction lives in "WebApi.Api.Endpoints.Support".
--
-- OpenAPI documentation interprets the same endpoint values exported below;
-- 'WebApi.Api.OpenApiDocs' owns family mounting and the startup-cached
-- document, so there is no second documentation-only endpoint inventory.
module WebApi.Api.Endpoints
  ( noApiRequestFields,
    meApiRouteDefinition,
    MeApiFailure (..),
    meApiOutcomeResponse,
    meApiFailureResponse,
    meApiEndpoint,
    secondApiRouteDefinition,
    secondApiEndpoint,
    statusApiRouteDefinition,
    statusApiEndpoint,
    tokenApiRouteDefinition,
    tokenApiEndpoint,
    TokenApiFailure (..),
    tokenApiOutcomeResponse,
    tokenApiFailureResponse,
    tokenApiMissingContentTypePolicy,
    requiredApiHeaderNameOrDie,
    requiredApiHeaderValueOrDie,
    requireOpenApiExtension,
  )
where

import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty ((:|)))
import HarchWeb.Api
  ( ApiEndpointContract (..),
    ApiEndpointRequest (..),
    ApiFieldFailurePolicy (ApiUseGenericFieldFailure),
    ApiMethod (ApiGet),
    ApiRequestBody (ApiNoRequestBody),
    ApiResponse (..),
    RequestCodec,
    SomeApiRouteEndpoint (..),
    apiContentType,
    apiResponse,
    apiRouteDefinition,
    apiRouteEndpointWithContext,
    apiRouteEndpointWithContextNeverFailing,
    bytesResponseEncoder,
    jsonMediaType,
  )
import HarchWeb.OpenApi (OpenApiExtension, mkOpenApiExtension)
import HarchWeb.Site (RouteDefinition)
import Network.HTTP.Types qualified as HttpTypes
import WebApi.Account (AccountProfile, AccountProfileStore)
import WebApi.Api.Endpoints.OAuthToken
  ( TokenApiFailure (..),
    tokenApiEndpoint,
    tokenApiFailureResponse,
    tokenApiMissingContentTypePolicy,
    tokenApiOutcomeResponse,
    tokenApiRouteDefinition,
  )
import WebApi.Api.Endpoints.Support
  ( apiDocumentedDeclaration,
    jsonBytes,
    requireOpenApiExtension,
    requiredApiHeaderNameOrDie,
    requiredApiHeaderValueOrDie,
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
  )
import WebApi.Route
  ( AppAuthorization,
    AppRequestContext (requestAccountPrincipal),
    AppRoute (MeApiRoute, SecondApiRoute, StatusApiRoute),
    endpointMetadata,
    requestLocale,
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
-- and demands that value. See the typed declarative endpoint boundary
-- decision record in @docs/design-guidance.md@.
noApiRequestFields :: RequestCodec ()
noApiRequestFields = pure ()

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
-- ran. See the scoped API-authentication design's decision record in @docs\/design-guidance.md@ for why
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
-- endpoint's requested resource (the web-api documentation requires real
-- synthetic values there); the real response stays @private, no-store@ regardless.
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
