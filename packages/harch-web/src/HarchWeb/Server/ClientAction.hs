{-# LANGUAGE OverloadedStrings #-}

-- | Private client-action protocol parsing and response encoding.
module HarchWeb.Server.ClientAction
  ( ClientActionProtocolError (..),
    clientActionProtocolErrorResponse,
    clientActionResponseBody,
    isClientActionRequest,
    maxClientActionBodyBytes,
    parseClientActionFields,
    validateClientActionRequest,
  )
where

import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Markup (regionPatchHtml, regionPatchId)
import HarchWeb.Server.Response
import Network.HTTP.Types.URI qualified as HttpUri
import Network.Wai qualified as Wai

data ClientActionProtocolError
  = InvalidClientActionEncoding
  | ClientActionBodyTooLarge
  | ClientActionMethodNotAllowed
  | ClientActionUnsupportedMediaType
  | ClientActionOriginRejected
  | ClientActionNotFound
  deriving (Eq, Show)

maxClientActionBodyBytes :: Int
maxClientActionBodyBytes = 65536

isClientActionRequest :: Wai.Request -> Bool
isClientActionRequest request = lookup "X-Harch-Action" (Wai.requestHeaders request) == Just "1"

validateClientActionRequest :: Text -> Wai.Request -> Either ClientActionProtocolError ()
validateClientActionRequest expectedOrigin request
  | Wai.requestMethod request /= "POST" = Left ClientActionMethodNotAllowed
  | not (formUrlEncodedRequest request) = Left ClientActionUnsupportedMediaType
  | requestOrigin request /= Just expectedOrigin = Left ClientActionOriginRejected
  | otherwise = Right ()

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

parseClientActionFields :: LazyByteString.ByteString -> Either ClientActionProtocolError [(Text, Text)]
parseClientActionFields requestBody =
  case LazyByteString.length requestBody > fromIntegral maxClientActionBodyBytes of
    True -> Left ClientActionBodyTooLarge
    False -> traverse decodeField (HttpUri.parseQuery (LazyByteString.toStrict requestBody))
  where
    decodeField (fieldName, maybeFieldValue) =
      (,) <$> decodeActionField fieldName <*> (fromMaybe Text.empty <$> traverse decodeActionField maybeFieldValue)

decodeActionField :: ByteString.ByteString -> Either ClientActionProtocolError Text
decodeActionField = either (const (Left InvalidClientActionEncoding)) Right . TextEncoding.decodeUtf8'

clientActionProtocolErrorResponse :: ClientActionProtocolError -> ResponseBody
clientActionProtocolErrorResponse protocolError =
  ResponseBody
    { responseStatus =
        case protocolError of
          InvalidClientActionEncoding -> 400
          ClientActionBodyTooLarge -> 413
          ClientActionMethodNotAllowed -> 405
          ClientActionUnsupportedMediaType -> 415
          ClientActionOriginRejected -> 403
          ClientActionNotFound -> 404,
      responseContentType = "application/json; charset=utf-8",
      responseBody = "{\"patches\":[],\"focusId\":null}",
      responseObservabilityAttributes = [],
      responseLogEntries = []
    }

clientActionResponseBody :: ClientActionResponse -> ResponseBody
clientActionResponseBody actionResponse =
  ResponseBody
    { responseStatus = clientActionStatus actionResponse,
      responseContentType = "application/json; charset=utf-8",
      responseBody = renderClientActionResponse actionResponse,
      responseObservabilityAttributes = clientActionObservabilityAttributes actionResponse,
      responseLogEntries = clientActionLogEntries actionResponse
    }

renderClientActionResponse :: ClientActionResponse -> Text
renderClientActionResponse actionResponse =
  "{\"patches\":["
    <> Text.intercalate "," (map renderPatch (clientActionPatches actionResponse))
    <> "],\"focusId\":"
    <> maybe "null" jsonString (clientActionFocusId actionResponse)
    <> "}"
  where
    renderPatch patch =
      "{\"id\":"
        <> jsonString (regionPatchId patch)
        <> ",\"html\":"
        <> jsonString (regionPatchHtml patch)
        <> "}"

jsonString :: Text -> Text
jsonString textValue = "\"" <> Text.concatMap escapeJsonCharacter textValue <> "\""

escapeJsonCharacter :: Char -> Text
escapeJsonCharacter character =
  case character of
    '"' -> "\\\""
    '\\' -> "\\\\"
    '\b' -> "\\b"
    '\f' -> "\\f"
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    _ -> Text.singleton character
