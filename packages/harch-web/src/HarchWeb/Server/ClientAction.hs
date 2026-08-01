{-# LANGUAGE OverloadedStrings #-}

-- | Private client-action protocol parsing and response encoding.
module HarchWeb.Server.ClientAction
  ( clientActionResponseBody,
    isClientActionRequest,
    parseClientActionFields,
  )
where

import Data.Bifunctor (bimap)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.Encoding.Error qualified as TextEncodingError
import HarchWeb.Server.Response
import Network.HTTP.Types.URI qualified as HttpUri
import Network.Wai qualified as Wai

isClientActionRequest :: Wai.Request -> Bool
isClientActionRequest request = lookup "X-Harch-Action" (Wai.requestHeaders request) == Just "1"

parseClientActionFields :: LazyByteString.ByteString -> [(Text, Text)]
parseClientActionFields requestBody =
  map (bimap decodeActionField (maybe "" decodeActionField)) (HttpUri.parseQuery (LazyByteString.toStrict requestBody))

decodeActionField :: ByteString.ByteString -> Text
decodeActionField = TextEncoding.decodeUtf8With TextEncodingError.lenientDecode

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
    renderPatch
      RegionPatch
        { regionPatchId,
          regionPatchHtml
        } =
        "{\"id\":" <> jsonString regionPatchId <> ",\"html\":" <> jsonString regionPatchHtml <> "}"

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
