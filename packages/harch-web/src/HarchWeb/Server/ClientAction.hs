{-# LANGUAGE OverloadedStrings #-}

-- | Private client-action protocol parsing and response encoding.
module HarchWeb.Server.ClientAction
  ( ClientActionProtocolError (..),
    clientActionProtocolErrorResponse,
    clientActionMethodNotAllowedResponse,
    clientActionResponseBody,
    isClientActionRequest,
    maxClientActionBodyBytes,
    maxClientActionFieldCount,
    parseClientActionFields,
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
import HarchWeb.Markup (elementIdText, regionPatchHtml, regionPatchId)
import HarchWeb.Observability qualified as Observability
import HarchWeb.Server.Response
import HarchWeb.Session (CsrfToken, mkCsrfToken, validateCsrfToken)
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
validateClientActionCsrf request actionFields
  | Just cookieToken <- requestCsrfCookie request,
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

requestCsrfCookie :: Wai.Request -> Maybe CsrfToken
requestCsrfCookie request =
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

clientActionProtocolErrorResponse :: ClientActionProtocolError -> ResponseBody
clientActionProtocolErrorResponse protocolError =
  let details = clientActionProtocolErrorDetails protocolError
   in ResponseBody
        { responseStatus = clientActionErrorStatus details,
          responseContentType = "application/json; charset=utf-8",
          responseBody = "{\"patches\":[],\"focusId\":null}",
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

clientActionMethodNotAllowedResponse :: NonEmpty.NonEmpty ActionMethod -> ClientActionResponse
clientActionMethodNotAllowedResponse allowedMethods =
  ClientActionResponse
    { clientActionStatus = Http.status405,
      clientActionPatches = [],
      clientActionFocusId = Nothing,
      clientActionHeaders =
        [ ("Allow", TextEncoding.encodeUtf8 (Text.intercalate ", " (map actionMethodText (NonEmpty.toList allowedMethods))))
        ],
      clientActionObservabilityAttributes = [],
      clientActionLogEntries = []
    }

clientActionResponseBody :: ClientActionResponse -> ResponseBody
clientActionResponseBody actionResponse =
  ResponseBody
    { responseStatus = clientActionStatus actionResponse,
      responseContentType = "application/json; charset=utf-8",
      responseBody = renderClientActionResponse actionResponse,
      responseObservabilityAttributes = clientActionObservabilityAttributes actionResponse,
      responseLogEntries = clientActionLogEntries actionResponse,
      responseDatabaseOperations = []
    }

renderClientActionResponse :: ClientActionResponse -> Text
renderClientActionResponse actionResponse =
  jsonText
    ( JsonEncoding.pairs
        ( JsonEncoding.pair "patches" (JsonEncoding.list renderPatch (clientActionPatches actionResponse))
            <> JsonEncoding.pair "focusId" (Aeson.toEncoding (elementIdText <$> clientActionFocusId actionResponse))
        )
    )
  where
    renderPatch patch =
      JsonEncoding.pairs
        ( JsonEncoding.pair "id" (Aeson.toEncoding (regionPatchId patch))
            <> JsonEncoding.pair "html" (Aeson.toEncoding (regionPatchHtml patch))
        )

jsonText :: JsonEncoding.Encoding -> Text
jsonText = TextEncoding.decodeUtf8 . LazyByteString.toStrict . JsonEncoding.encodingToLazyByteString
