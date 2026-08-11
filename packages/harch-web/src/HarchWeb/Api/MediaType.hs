{-# LANGUAGE OverloadedStrings #-}

-- | Validated media types shared by API request decoding and response
-- representation negotiation.
module HarchWeb.Api.MediaType
  ( ApiMediaType,
    apiMediaType,
    apiMediaTypeText,
    jsonMediaType,
    plainTextMediaType,
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

normalizeMediaType :: Text -> Maybe Text
normalizeMediaType contentTypeValue = do
  (typeText, subtypeText) <- parseMediaRange (Text.strip (fst (Text.breakOn ";" contentTypeValue)))
  pure (Text.toLower typeText <> "/" <> Text.toLower subtypeText)

parseMediaRange :: Text -> Maybe (Text, Text)
parseMediaRange mediaRangeText =
  case Text.splitOn "/" mediaRangeText of
    [typeText, subtypeText] | not (Text.null typeText), not (Text.null subtypeText) -> Just (typeText, subtypeText)
    _ -> Nothing
