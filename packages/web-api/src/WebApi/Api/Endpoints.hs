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
-- @\/api\/404@ handling are unchanged: this module only supplies the two
-- routes' response logic, wired in by @WebApi.App@'s per-route dispatch.
module WebApi.Api.Endpoints
  ( noApiRequestFields,
    secondApiRouteDefinition,
    statusApiRouteDefinition,
  )
where

import Data.Aeson.Encoding qualified as JsonEncoding
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List.NonEmpty (NonEmpty ((:|)))
import HarchWeb.Api
  ( ApiMethod (ApiGet),
    ApiRequestBody (ApiNoRequestBody),
    ApiResponse (..),
    RequestCodec,
    apiContentType,
    apiResponse,
    apiRouteDefinitionWithContext,
    apiRouteDefinitionWithContextNeverFailing,
    bytesResponseEncoder,
    jsonMediaType,
  )
import HarchWeb.Site (RouteDefinition)
import Network.HTTP.Types qualified as HttpTypes
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
    databaseOperationObservabilityAttributes,
    diagnosticsLogEntries,
    diagnosticsObservabilityAttributes,
    jsonErrorBody,
    pageFailureDiagnostics,
    secondRouteApiBody,
    statusApiBody,
  )
import WebApi.Route (AppRequestContext, AppRoute, requestLocale)
import WebApi.RouteData (SecondRouteData (..), StatusApiData (..))

-- | Neither @\/api\/status@ nor @\/api\/second@ decodes any query, header, or
-- cookie field, so both endpoints below share this one declaration rather
-- than each writing their own @pure ()@. Exported so a Unit test can decode
-- it directly with 'HarchWeb.Api.runRequestCodec' and compare the result:
-- neither endpoint's own handler reads its decoded fields (there are none
-- to read), so routing a request through the full endpoint only pattern
-- matches the surrounding @Just@, never forcing the @()@ payload itself —
-- an 'HarchWeb.Api.RequestCodec' is a nested 'Data.Functor.Compose' over
-- @(->) ApiRequestData@, and HPC only ticks that @()@ when something
-- actually demands it, not merely when the endpoint runs. See the AC
-- decision record in @docs/design-guidance.md@.
noApiRequestFields :: RequestCodec ()
noApiRequestFields = pure ()

statusApiRouteDefinition :: RouteDefinition AppRoute AppRequestContext
statusApiRouteDefinition =
  apiRouteDefinitionWithContextNeverFailing
    ApiGet
    noApiRequestFields
    ApiNoRequestBody
    (bytesResponseEncoder (apiContentType jsonMediaType) :| [])
    (\requestContext _endpointRequest -> pure (apiResponse (jsonBytes (statusApiBody (StatusApiData (requestLocale requestContext))))))

secondApiRouteDefinition :: PageRepository -> RouteDefinition AppRoute AppRequestContext
secondApiRouteDefinition pageRepository =
  apiRouteDefinitionWithContext
    ApiGet
    noApiRequestFields
    ApiNoRequestBody
    (bytesResponseEncoder (apiContentType jsonMediaType) :| [])
    ( \requestContext _endpointRequest -> do
        secondPageResult <- loadSecondPage pageRepository (requestLocale requestContext)
        let databaseOperations = databaseResultOperations secondPageResult
        pure $ case databaseResultValue secondPageResult of
          Right secondPageData ->
            Right
              ( (apiResponse (jsonBytes (secondRouteApiBody (toSecondRouteData secondPageData))))
                  { apiEndpointResponseObservabilityAttributes = databaseOperationObservabilityAttributes databaseOperations
                  }
              )
          Left databaseError -> Left (SecondApiFailure databaseOperations databaseError)
    )
    secondApiFailureResponse
  where
    toSecondRouteData secondPageData =
      SecondRouteData (secondPageDataSummary secondPageData) (secondPageDataHighlights secondPageData)

data SecondApiFailure = SecondApiFailure [DatabaseOperation] DatabaseError

secondApiFailureResponse :: SecondApiFailure -> ApiResponse ByteString.ByteString
secondApiFailureResponse (SecondApiFailure databaseOperations databaseError) =
  (apiResponse (jsonBytes (jsonErrorBody "second-page-unavailable")))
    { apiEndpointResponseStatus = HttpTypes.status503,
      apiEndpointResponseObservabilityAttributes = diagnosticsObservabilityAttributes diagnostics,
      apiEndpointResponseLogEntries = diagnosticsLogEntries diagnostics
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
