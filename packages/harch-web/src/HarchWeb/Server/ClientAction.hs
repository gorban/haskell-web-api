{-# LANGUAGE OverloadedStrings #-}

-- | Private client-action protocol parsing and response encoding.
module HarchWeb.Server.ClientAction
  ( ClientActionProtocolError (..),
    clientActionProtocolErrorResponse,
    clientActionMethodNotAllowedResponse,
    clientActionReauthenticationRequiredResponse,
    clientActionResponseBody,
    isClientActionRequest,
    maxClientActionBodyBytes,
    maxClientActionFieldCount,
    parseClientActionFields,
    csrfCookieFromRequest,
    validateActionCsrfTransport,
    validateClientActionCsrf,
    validateClientActionRequest,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as JsonEncoding
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Action (ActionMethod, actionMethodText)
import HarchWeb.ClientStorage (BrowserStorageClass (..), ClientStorageCleanup, browserStorageKeyClass, browserStorageKeyText, clientStorageCleanupEntries, noClientStorageCleanup)
import HarchWeb.Csrf (CsrfToken, mkCsrfToken, validateCsrfToken)
import HarchWeb.Markup (ElementId, elementIdText, regionPatchHtml, regionPatchId, safeUrlText)
import HarchWeb.Observability qualified as Observability
import HarchWeb.RequestId (RequestId, requestIdText)
import HarchWeb.Routing (RouteCodec (..), encodeRouteLocation)
import HarchWeb.Server.Response
import Network.HTTP.Types qualified as Http
import Network.HTTP.Types.URI qualified as HttpUri
import Network.Wai qualified as Wai

data ClientActionProtocolError
  = InvalidClientActionEncoding
  | ClientActionBodyTooLarge
  | ClientActionFieldCountExceeded
  | ClientActionUnsupportedMediaType
  | ClientActionOriginRejected
  | ClientActionCsrfRejected
  | ClientActionCsrfUnavailable
  | ClientActionPayloadMalformed
  | ClientActionDecoderInvalid
  | ClientActionNotFound

maxClientActionBodyBytes :: Int
maxClientActionBodyBytes = 65536

-- | Bound the number of decoded URL-encoded fields independently from the
-- raw-body budget.  The count is checked before parsing so a small body made
-- of tiny fields cannot amplify into an unbounded list of decoded values.
maxClientActionFieldCount :: Int
maxClientActionFieldCount = 128

isClientActionRequest :: Wai.Request -> Bool
isClientActionRequest request = lookup "X-Harch-Action" (Wai.requestHeaders request) == Just "1"

validateClientActionRequest :: Maybe Text -> Wai.Request -> Either ClientActionProtocolError ()
validateClientActionRequest expectedOrigin request
  | not (formUrlEncodedRequest request) = Left ClientActionUnsupportedMediaType
  | Just decodedOrigin <- expectedOrigin,
    requestOrigin request == Just decodedOrigin =
      Right ()
  | otherwise = Left ClientActionOriginRejected

validateClientActionCsrf :: Wai.Request -> [(Text, Text)] -> Either ClientActionProtocolError CsrfToken
validateClientActionCsrf = validateActionCsrfTransport

-- | Validate the framework-owned double-submit transport proof for any
-- URL-encoded mutation, including an explicit native fallback. The selected
-- backend verification remains a separate operation because only the routed
-- application context can resolve current grants.
validateActionCsrfTransport :: Wai.Request -> [(Text, Text)] -> Either ClientActionProtocolError CsrfToken
validateActionCsrfTransport request actionFields
  | Just cookieToken <- csrfCookieFromRequest request,
    Just submittedToken <- submittedCsrfToken actionFields,
    validateCsrfToken cookieToken submittedToken =
      Right submittedToken
  | otherwise = Left ClientActionCsrfRejected

formUrlEncodedRequest :: Wai.Request -> Bool
formUrlEncodedRequest request =
  maybe False isFormUrlEncoded (lookup "Content-Type" (Wai.requestHeaders request))
  where
    isFormUrlEncoded contentType =
      case ByteString.break (== 59) contentType of
        (mediaType, parameters) ->
          mediaType == "application/x-www-form-urlencoded"
            && (ByteString.null parameters || ByteString.head parameters == 59)

requestOrigin :: Wai.Request -> Maybe Text
requestOrigin request = lookup "Origin" (Wai.requestHeaders request) >>= either (const Nothing) Just . TextEncoding.decodeUtf8'

-- | Extract exactly one syntactically valid host CSRF cookie. Both page
-- preparation and action verification use this framework-owned parser, so a
-- duplicate or malformed cookie is never silently selected on one path but
-- rejected on the other.
csrfCookieFromRequest :: Wai.Request -> Maybe CsrfToken
csrfCookieFromRequest request =
  case [value | (name, value) <- requestCookies request, name == "__Host-harch-csrf"] of
    [rawToken] -> either (const Nothing) mkCsrfToken (TextEncoding.decodeUtf8' rawToken)
    _ -> Nothing

submittedCsrfToken :: [(Text, Text)] -> Maybe CsrfToken
submittedCsrfToken actionFields =
  case [value | (name, value) <- actionFields, name == "_harch_csrf"] of
    [rawToken] -> mkCsrfToken rawToken
    _ -> Nothing

requestCookies :: Wai.Request -> [(ByteString.ByteString, ByteString.ByteString)]
requestCookies request =
  concatMap (map parseCookie . ByteString.split 59 . snd) (filter ((== Http.hCookie) . fst) (Wai.requestHeaders request))
  where
    parseCookie cookie =
      let (name, valueWithSeparator) = ByteString.break (== 61) (ByteString.dropWhile (== 32) cookie)
       in (name, ByteString.drop 1 valueWithSeparator)

parseClientActionFields :: LazyByteString.ByteString -> Either ClientActionProtocolError [(Text, Text)]
parseClientActionFields requestBody =
  case LazyByteString.length requestBody > fromIntegral maxClientActionBodyBytes of
    True -> Left ClientActionBodyTooLarge
    False
      | urlEncodedFieldCount requestBody > maxClientActionFieldCount -> Left ClientActionFieldCountExceeded
      | otherwise -> traverse decodeField (HttpUri.parseQuery (LazyByteString.toStrict requestBody))
  where
    decodeField (fieldName, maybeFieldValue) =
      (,) <$> decodeActionField fieldName <*> (fromMaybe Text.empty <$> traverse decodeActionField maybeFieldValue)

urlEncodedFieldCount :: LazyByteString.ByteString -> Int
urlEncodedFieldCount requestBody
  | LazyByteString.null requestBody = 0
  | otherwise = 1 + sum (map (ByteString.count 38) (LazyByteString.toChunks requestBody))

decodeActionField :: ByteString.ByteString -> Either ClientActionProtocolError Text
decodeActionField = either (const (Left InvalidClientActionEncoding)) Right . TextEncoding.decodeUtf8'

clientActionProtocolErrorResponse :: RequestId -> ClientActionProtocolError -> ResponseBody
clientActionProtocolErrorResponse requestId protocolError =
  let details = clientActionProtocolErrorDetails protocolError
   in ResponseBody
        { responseStatus = clientActionErrorStatus details,
          responseContentType = "application/json; charset=utf-8",
          responseBody = clientActionErrorResponseJson requestId,
          responseObservabilityAttributes = clientActionErrorObservabilityAttributes details,
          responseLogEntries = clientActionErrorLogEntries details,
          responseDatabaseOperations = []
        }

data ClientActionProtocolErrorDetails = ClientActionProtocolErrorDetails
  { clientActionErrorStatus :: Http.Status,
    clientActionErrorObservabilityAttributes :: [Observability.ObservabilityAttribute],
    clientActionErrorLogEntries :: [Text]
  }

clientActionProtocolErrorDetails :: ClientActionProtocolError -> ClientActionProtocolErrorDetails
clientActionProtocolErrorDetails protocolError =
  case protocolError of
    InvalidClientActionEncoding -> ordinaryClientActionError Http.status400
    ClientActionBodyTooLarge -> ordinaryClientActionError Http.status413
    ClientActionFieldCountExceeded -> ordinaryClientActionError Http.status413
    ClientActionUnsupportedMediaType -> ordinaryClientActionError Http.status415
    ClientActionOriginRejected -> ordinaryClientActionError Http.status403
    ClientActionCsrfRejected -> ordinaryClientActionError Http.status403
    ClientActionCsrfUnavailable -> ordinaryClientActionError Http.status503
    ClientActionPayloadMalformed ->
      ClientActionProtocolErrorDetails
        { clientActionErrorStatus = Http.status400,
          clientActionErrorObservabilityAttributes =
            [ Observability.ObservabilityAttribute
                { Observability.attributeName = "harch.client_action.decode_failure",
                  Observability.attributeValue = Observability.TextAttribute "malformed"
                }
            ],
          clientActionErrorLogEntries = ["client action decode failure: malformed"]
        }
    ClientActionDecoderInvalid ->
      ClientActionProtocolErrorDetails
        { clientActionErrorStatus = Http.status500,
          clientActionErrorObservabilityAttributes =
            [ Observability.ObservabilityAttribute
                { Observability.attributeName = "harch.client_action.decode_failure",
                  Observability.attributeValue = Observability.TextAttribute "invalid_decoder"
                }
            ],
          clientActionErrorLogEntries = ["client action decode failure: invalid decoder"]
        }
    ClientActionNotFound -> ordinaryClientActionError Http.status404

ordinaryClientActionError :: Http.Status -> ClientActionProtocolErrorDetails
ordinaryClientActionError status =
  ClientActionProtocolErrorDetails
    { clientActionErrorStatus = status,
      clientActionErrorObservabilityAttributes = [],
      clientActionErrorLogEntries = []
    }

clientActionMethodNotAllowedResponse :: NonEmpty.NonEmpty ActionMethod -> ClientActionResponse route context
clientActionMethodNotAllowedResponse allowedMethods =
  ClientActionResponse
    { clientActionStatus = Http.status405,
      clientActionPatches = [],
      clientActionFocusId = Nothing,
      clientActionNavigation = StayOnCurrentRoute,
      clientActionStorageCleanup = noClientStorageCleanup,
      clientActionHeaders =
        [ ("Allow", TextEncoding.encodeUtf8 (Text.intercalate ", " (map actionMethodText (NonEmpty.toList allowedMethods))))
        ],
      clientActionObservabilityAttributes = [],
      clientActionLogEntries = []
    }

-- | The only action response recognized by the deferred runtime as a
-- pre-handler authentication challenge.  Its constant marker is owned by
-- Harch rather than application-authored JSON, so a regular handler's 401
-- cannot accidentally retain and replay a mutation.
clientActionReauthenticationRequiredResponse :: ClientActionResponse route context
clientActionReauthenticationRequiredResponse =
  ClientActionResponse
    { clientActionStatus = Http.status401,
      clientActionPatches = [],
      clientActionFocusId = Nothing,
      clientActionNavigation = StayOnCurrentRoute,
      clientActionStorageCleanup = noClientStorageCleanup,
      clientActionHeaders = [("X-Harch-Action-Reauthenticate", "required")],
      clientActionObservabilityAttributes = [],
      clientActionLogEntries = []
    }

clientActionResponseBody :: RequestId -> RouteCodec route context -> ClientActionResponse route context -> ResponseBody
clientActionResponseBody requestId routeCodec actionResponse =
  ResponseBody
    { responseStatus = clientActionStatus actionResponse,
      responseContentType = "application/json; charset=utf-8",
      responseBody = renderClientActionResponse requestId routeCodec actionResponse,
      responseObservabilityAttributes = clientActionObservabilityAttributes actionResponse,
      responseLogEntries = clientActionLogEntries actionResponse,
      responseDatabaseOperations = []
    }

renderClientActionResponse :: RequestId -> RouteCodec route context -> ClientActionResponse route context -> Text
renderClientActionResponse requestId routeCodec actionResponse =
  clientActionResponseJson requestId routeCodec (clientActionPatches actionResponse) (clientActionFocusId actionResponse) (clientActionNavigation actionResponse) (clientActionStorageCleanup actionResponse)

clientActionResponseJson :: RequestId -> RouteCodec route context -> [RegionPatch] -> Maybe ElementId -> ActionNavigation route context -> ClientStorageCleanup -> Text
clientActionResponseJson requestId routeCodec patches maybeFocusId navigation storageCleanup =
  jsonText
    ( JsonEncoding.pairs
        ( JsonEncoding.pair "patches" (JsonEncoding.list renderPatch patches)
            <> JsonEncoding.pair "focusId" (Aeson.toEncoding (elementIdText <$> maybeFocusId))
            <> JsonEncoding.pair "navigation" (renderNavigation navigation)
            <> JsonEncoding.pair "storageCleanup" (JsonEncoding.list renderStorageEntry (clientStorageCleanupEntries storageCleanup))
            <> JsonEncoding.pair "requestId" (Aeson.toEncoding (requestIdText requestId))
        )
    )
  where
    renderStorageEntry storageKey =
      JsonEncoding.pairs
        ( JsonEncoding.pair "storage" (Aeson.toEncoding (storageClassText (browserStorageKeyClass storageKey)))
            <> JsonEncoding.pair "key" (Aeson.toEncoding (browserStorageKeyText storageKey))
        )
    storageClassText :: BrowserStorageClass -> Text
    storageClassText storageClass =
      case storageClass of
        LocalStorage -> "local"
        SessionStorage -> "session"
    renderPatch patch =
      JsonEncoding.pairs
        ( JsonEncoding.pair "id" (Aeson.toEncoding (regionPatchId patch))
            <> JsonEncoding.pair "html" (Aeson.toEncoding (regionPatchHtml patch))
        )
    renderNavigation navigationValue =
      case navigationValue of
        StayOnCurrentRoute -> JsonEncoding.null_
        NavigateInternal historyMode routeRequest ->
          JsonEncoding.pairs
            ( JsonEncoding.pair "historyMode" (Aeson.toEncoding (historyModeText historyMode))
                <> JsonEncoding.pair "href" (Aeson.toEncoding (safeUrlText (encodeRouteLocation (renderRoute routeCodec routeRequest))))
            )

clientActionErrorResponseJson :: RequestId -> Text
clientActionErrorResponseJson requestId =
  jsonText
    ( JsonEncoding.pairs
        ( JsonEncoding.pair "patches" (Aeson.toEncoding ([] :: [Text]))
            <> JsonEncoding.pair "focusId" JsonEncoding.null_
            <> JsonEncoding.pair "navigation" JsonEncoding.null_
            <> JsonEncoding.pair "requestId" (Aeson.toEncoding (requestIdText requestId))
        )
    )

historyModeText :: HistoryMode -> Text
historyModeText historyMode =
  case historyMode of
    PushHistory -> "push"
    ReplaceHistory -> "replace"

jsonText :: JsonEncoding.Encoding -> Text
jsonText = TextEncoding.decodeUtf8 . LazyByteString.toStrict . JsonEncoding.encodingToLazyByteString
