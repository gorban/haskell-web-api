{-# LANGUAGE OverloadedStrings #-}

-- | Private framework-owned pre-routing response interpreter.
--
-- Runtime assets, policy preflight/redirects, static assets, and malformed
-- request failures are all resolved before application routing. This remains
-- a stage of the one WAI dispatcher, not a second static or routing pipeline.
module HarchWeb.Server.EarlyStages
  ( navigationRuntimeResponse,
    requestHeadLimitResponse,
    routeLocationDecodeResponse,
    runEarlyRequestStages,
    runtimeAssetResponse,
  )
where

import Control.Monad.Except (ExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Foldable (for_)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Document (NavigationRuntime, RuntimeAsset)
import HarchWeb.Document qualified as Document
import HarchWeb.RequestId (RequestId, requestIdText)
import HarchWeb.Security
  ( RequestHeadLimitFailure (..),
    applyRequestPathPrefix,
    corsPreflightResponse,
    externalRequestPath,
    httpsRedirectResponse,
    mkUrlPath,
    requestPathPrefix,
    requestRedirectLocation,
    urlPathText,
  )
import HarchWeb.Server.Application
import HarchWeb.Server.Response (ResponseBody (..))
import HarchWeb.Server.ResponseRendering (applyResponseHeaders, toWaiBodyResponse)
import HarchWeb.Server.StaticAssets (serveStaticAssetResponse)
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai

navigationRuntimeResponse :: NavigationRuntime -> Text -> Maybe ResponseBody
navigationRuntimeResponse runtime requestPath =
  if requestPath == Document.navigationRuntimePath runtime
    then Just (runtimeResponse (Document.navigationRuntimeScript runtime))
    else Nothing

runtimeAssetResponse :: RuntimeAsset -> Text -> Maybe ResponseBody
runtimeAssetResponse runtimeAsset requestPath =
  if requestPath == Document.runtimeAssetPath runtimeAsset
    then Just (runtimeResponse (Document.runtimeAssetScript runtimeAsset))
    else Nothing

runtimeResponse :: Text -> ResponseBody
runtimeResponse source =
  ResponseBody
    { responseStatus = Http.status200,
      responseContentType = "application/javascript; charset=utf-8",
      responseBody = source,
      responseObservabilityAttributes = [],
      responseLogEntries = [],
      responseDatabaseOperations = []
    }

runEarlyRequestStages :: Application route action context authorization -> Wai.Request -> Text -> Http.ResponseHeaders -> ExceptT (Text, Wai.Response) IO ()
runEarlyRequestStages webApplication request requestPath policyResponseHeaders = do
  let requestPolicyConfig = applicationRequestPolicy webApplication
      earlyResponse path = throwError . (path,) . applyResponseHeaders policyResponseHeaders
  for_ (corsPreflightResponse requestPolicyConfig request) (earlyResponse (externalRequestPath requestPolicyConfig request))
  for_ (requestRedirectLocation requestPolicyConfig request) $ \redirectLocation ->
    earlyResponse (externalRequestPath requestPolicyConfig request) (httpsRedirectResponse redirectLocation)
  for_ (applicationNavigationRuntime webApplication >>= (`navigationRuntimeResponse` requestPath)) (earlyResponse requestPath . toWaiBodyResponse [])
  for_ (listToMaybe (mapMaybe (`runtimeAssetResponse` requestPath) (applicationRuntimeAssets webApplication))) (earlyResponse requestPath . toWaiBodyResponse [])
  maybeStaticResponse <- liftIO (serveStaticAssetResponse (applicationStaticAssets webApplication) request requestPath)
  for_ maybeStaticResponse $ \(staticRoutePath, staticResponse) ->
    earlyResponse (urlPathText (applyRequestPathPrefix (requestPathPrefix requestPolicyConfig request) (mkUrlPath staticRoutePath))) staticResponse

-- | Render a framework-owned pre-routing failure with the opaque identifier
-- already fixed at HTTP ingress. Only these Harch-owned plain-text bodies get
-- a support-copyable value; arbitrary application protocol, streaming, and WAI
-- response bodies remain application-owned. The remaining AHI-5-RID work covers
-- application error presentations and audit joins.
routeLocationDecodeResponse :: RequestId -> Wai.Response
routeLocationDecodeResponse requestId = Wai.responseLBS Http.status400 [(Http.hContentType, "text/plain; charset=utf-8")] (requestRejectionBody "Request target was rejected." requestId)

-- | As 'routeLocationDecodeResponse', while preserving the status selected by
-- the request-head budget failure.
requestHeadLimitResponse :: RequestId -> RequestHeadLimitFailure -> Wai.Response
requestHeadLimitResponse requestId limitFailure = Wai.responseLBS status [(Http.hContentType, "text/plain; charset=utf-8")] (requestRejectionBody "Request metadata was rejected." requestId)
  where
    status =
      case limitFailure of
        InvalidRequestTargetEncoding -> Http.status400
        RequestTargetTooLarge -> Http.status414
        TooManyRequestHeaders -> Http.status431
        RequestHeadersTooLarge -> Http.status431
        RequestHeaderValueTooLarge -> Http.status431
        TooManyRequestCookies -> Http.status431
        RequestCookieNameTooLarge -> Http.status431
        RequestCookieValueTooLarge -> Http.status431
        TooManyPathSegments -> Http.status414
        RequestPathSegmentTooLarge -> Http.status414
        TooManyQueryFields -> Http.status414
        RequestQueryFieldTooLarge -> Http.status414

requestRejectionBody :: Text -> RequestId -> LazyByteString.ByteString
requestRejectionBody rejectionSummary requestId =
  LazyByteString.fromStrict
    (TextEncoding.encodeUtf8 (rejectionSummary <> " Request ID: " <> requestIdText requestId <> "."))
