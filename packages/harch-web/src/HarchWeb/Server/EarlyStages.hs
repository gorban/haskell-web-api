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
import Data.Foldable (for_)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import HarchWeb.Document (NavigationRuntime, RuntimeAsset)
import HarchWeb.Document qualified as Document
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

routeLocationDecodeResponse :: Wai.Response
routeLocationDecodeResponse = Wai.responseLBS Http.status400 [(Http.hContentType, "text/plain; charset=utf-8")] "Request target was rejected."

requestHeadLimitResponse :: RequestHeadLimitFailure -> Wai.Response
requestHeadLimitResponse limitFailure = Wai.responseLBS status [(Http.hContentType, "text/plain; charset=utf-8")] "Request metadata was rejected."
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
