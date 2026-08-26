{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

-- | ICU-backed, application-extensible message lookup and rendering.
--
-- This is deliberately a new framework boundary rather than an extension of
-- routing: routes choose a locale, while a 'Localizer' maps an application key
-- and that locale to a message template.  The first delivered slice owns the
-- general lookup and ICU MessageFormat execution only.  'web-api' still needs
-- to migrate its two-language call sites onto an application message catalog,
-- and the localization example still needs to demonstrate a third locale;
-- both gaps remain tracked as EI rather than being presented as complete.
module HarchWeb.Localization
  ( Locale,
    locale,
    localeText,
    MessageArgument,
    messageText,
    messageNumber,
    MessageArguments,
    messageArguments,
    MessageTemplate,
    messageTemplate,
    Localizer,
    localizer,
    renderLocalizedMessage,
    MessageRenderError (..),
  )
where

import Control.Exception (bracket)
import Data.ByteString qualified as ByteString
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (withMany)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke)

newtype Locale = Locale Text
  deriving (Eq, Ord, Show)

locale :: Text -> Locale
locale = Locale

localeText :: Locale -> Text
localeText (Locale value) = value

data MessageArgument
  = MessageText Text
  | MessageNumber Int64
  deriving (Eq, Show)

messageText :: Text -> MessageArgument
messageText = MessageText

messageNumber :: Int64 -> MessageArgument
messageNumber = MessageNumber

newtype MessageArguments = MessageArguments (Map Text MessageArgument)

messageArguments :: [(Text, MessageArgument)] -> MessageArguments
messageArguments = MessageArguments . Map.fromList

newtype MessageTemplate = MessageTemplate Text
  deriving (Eq, Show)

messageTemplate :: Text -> MessageTemplate
messageTemplate = MessageTemplate

newtype Localizer messageKey = Localizer (messageKey -> Locale -> Maybe MessageTemplate)

localizer :: (messageKey -> Locale -> Maybe MessageTemplate) -> Localizer messageKey
localizer = Localizer

-- | An application catalog miss is distinct from malformed ICU source: callers
-- may use the former to layer a fallback catalog, but must not silently render
-- a malformed message template.
data MessageRenderError
  = MessageNotFound
  | MessageFormatRejected
  deriving (Eq, Show)

renderLocalizedMessage :: Localizer messageKey -> messageKey -> Locale -> MessageArguments -> IO (Either MessageRenderError Text)
renderLocalizedMessage (Localizer lookupTemplate) messageKey requestedLocale (MessageArguments arguments) =
  case lookupTemplate messageKey requestedLocale of
    Nothing -> pure (Left MessageNotFound)
    Just (MessageTemplate template) -> renderIcuMessage requestedLocale template (Map.toAscList arguments)

renderIcuMessage :: Locale -> Text -> [(Text, MessageArgument)] -> IO (Either MessageRenderError Text)
renderIcuMessage (Locale requestedLocale) template arguments =
  TextEncoding.encodeUtf8 requestedLocale `ByteString.useAsCString` \localeValue ->
    TextEncoding.encodeUtf8 template `ByteString.useAsCString` \templateValue ->
      withMany (ByteString.useAsCString . TextEncoding.encodeUtf8 . fst) arguments $ \argumentNames ->
        withMany (ByteString.useAsCString . TextEncoding.encodeUtf8 . argumentText . snd) arguments $ \argumentTexts ->
          withArray (map (argumentNumber . snd) arguments) $ \argumentNumbers ->
            withArray (map (argumentKind . snd) arguments) $ \argumentKinds ->
              withArray argumentNames $ \names ->
                withArray argumentTexts $ \texts ->
                  alloca $ \resultPointer -> do
                    poke resultPointer nullPtr
                    status <- c_formatMessage localeValue templateValue names texts argumentNumbers argumentKinds (fromIntegral (length arguments)) resultPointer
                    result <- peek resultPointer
                    if status /= 0 || result == nullPtr
                      then pure (Left MessageFormatRejected)
                      else bracket (pure result) free (fmap Right . (Text.pack <$>) . peekCString)
  where
    argumentText argument =
      case argument of
        MessageText value -> value
        MessageNumber _ -> ""
    argumentNumber argument =
      case argument of
        MessageText _ -> 0
        MessageNumber value -> value
    argumentKind argument =
      case argument of
        MessageText _ -> 0
        MessageNumber _ -> 1

foreign import ccall unsafe "harch_web_format_message"
  c_formatMessage :: CString -> CString -> Ptr CString -> Ptr CString -> Ptr Int64 -> Ptr CInt -> CInt -> Ptr CString -> IO CInt
