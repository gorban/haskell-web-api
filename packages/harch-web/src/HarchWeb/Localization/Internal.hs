-- | Internal byte-boundary helpers for 'HarchWeb.Localization'.
module HarchWeb.Localization.Internal
  ( MessageRenderError (..),
    decodeRenderedIcuUtf8,
  )
where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding

-- | An application catalog miss is distinct from malformed ICU source: callers
-- may use the former to layer a fallback catalog, but must not silently render
-- malformed native output as replacement text.
data MessageRenderError
  = MessageNotFound
  | MessageFormatRejected
  deriving (Eq, Show)

-- | ICU's native formatter promises UTF-8 output. Reject a broken native
-- boundary explicitly rather than allowing a locale-dependent decoder or
-- replacement text to change rendered copy.
decodeRenderedIcuUtf8 :: ByteString -> Either MessageRenderError Text
decodeRenderedIcuUtf8 renderedBytes =
  case TextEncoding.decodeUtf8' renderedBytes of
    Left _ -> Left MessageFormatRejected
    Right rendered -> Right rendered
