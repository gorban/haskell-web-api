{-# LANGUAGE OverloadedStrings #-}

-- | Validated media types shared by API request decoding and response
-- representation negotiation.
module HarchWeb.Api.MediaType
  ( ApiMediaType,
    apiMediaType,
    apiMediaTypeText,
    jsonMediaType,
    plainTextMediaType,
    htmlMediaType,
    ApiContentType,
    apiContentType,
    apiContentTypeMediaType,
    apiUtf8ContentType,
    apiContentTypeText,
    jsonContentType,
    plainTextContentType,
    apiMediaTypeParts,
    parseMediaRange,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text

-- | A validated bare @type\/subtype@ media type declared by application
-- configuration. Header values remain raw 'Text' until parsing succeeds, so
-- an invalid request header cannot become a declared representation.
newtype ApiMediaType = ApiMediaType Text
  deriving (Eq, Show)

-- | Validate and normalize an application-declared bare media type.
apiMediaType :: Text -> Maybe ApiMediaType
apiMediaType value =
  ApiMediaType <$> normalizeMediaType value

apiMediaTypeText :: ApiMediaType -> Text
apiMediaTypeText (ApiMediaType value) = value

-- | The media type used by JSON request bodies and representations.
jsonMediaType :: ApiMediaType
jsonMediaType = ApiMediaType "application/json"

-- | The bare media type used by UTF-8 plain-text request bodies.
plainTextMediaType :: ApiMediaType
plainTextMediaType = ApiMediaType "text/plain"

-- | The bare media type used by UTF-8 HTML responses.
htmlMediaType :: ApiMediaType
htmlMediaType = ApiMediaType "text/html"

-- | A response @Content-Type@ derived from a declared media type. The
-- constructor is private so a caller cannot accidentally put an unvalidated
-- header value on an API response.
data ApiContentType = ApiContentType ApiMediaType ApiContentTypeCharset
  deriving (Eq, Show)

data ApiContentTypeCharset
  = NoCharset
  | Utf8Charset
  deriving (Eq, Show)

-- | Use a declared media type without a charset parameter, appropriate for
-- bytes whose encoding is application-defined or not textual.
apiContentType :: ApiMediaType -> ApiContentType
apiContentType mediaType = ApiContentType mediaType NoCharset

-- | Recover the validated media type from a declared response content type.
apiContentTypeMediaType :: ApiContentType -> ApiMediaType
apiContentTypeMediaType (ApiContentType mediaType _) = mediaType

-- | Declare that a textual response is encoded as UTF-8.
apiUtf8ContentType :: ApiMediaType -> ApiContentType
apiUtf8ContentType mediaType = ApiContentType mediaType Utf8Charset

-- | Render a validated response @Content-Type@ header value.
apiContentTypeText :: ApiContentType -> Text
apiContentTypeText (ApiContentType mediaType charset) =
  apiMediaTypeText mediaType <> renderedCharset charset

renderedCharset :: ApiContentTypeCharset -> Text
renderedCharset charset =
  case charset of
    NoCharset -> ""
    Utf8Charset -> "; charset=utf-8"

-- | The @Content-Type@ emitted by JSON response helpers.
jsonContentType :: ApiContentType
jsonContentType = apiUtf8ContentType jsonMediaType

-- | The @Content-Type@ emitted by plain-text response helpers.
plainTextContentType :: ApiContentType
plainTextContentType = apiUtf8ContentType plainTextMediaType

apiMediaTypeParts :: ApiMediaType -> (Text, Text)
apiMediaTypeParts (ApiMediaType mediaType) =
  (Text.takeWhile (/= '/') mediaType, Text.drop 1 (Text.dropWhile (/= '/') mediaType))

normalizeMediaType :: Text -> Maybe Text
normalizeMediaType contentTypeValue = do
  (typeText, subtypeText) <- parseMediaRange (Text.strip (fst (Text.breakOn ";" contentTypeValue)))
  pure (Text.toLower typeText <> "/" <> Text.toLower subtypeText)

parseMediaRange :: Text -> Maybe (Text, Text)
parseMediaRange mediaRangeText =
  case Text.splitOn "/" mediaRangeText of
    [typeText, subtypeText] | not (Text.null typeText), not (Text.null subtypeText) -> Just (typeText, subtypeText)
    _ -> Nothing
