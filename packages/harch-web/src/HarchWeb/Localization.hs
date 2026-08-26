{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

-- | ICU-backed, application-extensible message lookup and rendering.
--
-- This is deliberately a new framework boundary rather than an extension of
-- routing: routes choose a locale, while a 'Localizer' maps an application key
-- and that locale to a message template. ICU rendering is pure at this public
-- boundary: the native implementation has no observable state, and all native
-- allocation and release remains inside the implementation. That keeps the
-- same catalog usable in pure SSR builders and effectful action handlers.
--
-- HarchWeb has no end-user copy today. 'DefaultMessageKey' retains one
-- unrenderable sentinel so the empty default catalog is observable and tested;
-- 'defaultMessage' and 'defaultLocalizer' establish the extend-not-replace
-- layer now, so future framework keys can be added without applications
-- replacing their own catalogs.
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
    DefaultMessageKey (..),
    defaultMessage,
    Localizer,
    localizer,
    defaultLocalizer,
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
import System.IO.Unsafe (unsafePerformIO)

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

-- | The framework's currently-empty built-in message-key enum. Its sole
-- sentinel has no corresponding template. Applications use their own closed
-- key type and may fall back to 'defaultMessage' when a future framework key
-- is embedded in it.
data DefaultMessageKey = NoDefaultMessage

defaultMessage :: DefaultMessageKey -> Locale -> Maybe MessageTemplate
defaultMessage NoDefaultMessage _ = Nothing

newtype Localizer messageKey = Localizer (messageKey -> Locale -> Maybe MessageTemplate)

localizer :: (messageKey -> Locale -> Maybe MessageTemplate) -> Localizer messageKey
localizer = Localizer

defaultLocalizer :: Localizer DefaultMessageKey
defaultLocalizer = localizer defaultMessage

-- | An application catalog miss is distinct from malformed ICU source: callers
-- may use the former to layer a fallback catalog, but must not silently render
-- a malformed message template.
data MessageRenderError
  = MessageNotFound
  | MessageFormatRejected
  deriving (Eq, Show)

renderLocalizedMessage :: Localizer messageKey -> messageKey -> Locale -> MessageArguments -> Either MessageRenderError Text
renderLocalizedMessage (Localizer lookupTemplate) messageKey requestedLocale (MessageArguments arguments) =
  case lookupTemplate messageKey requestedLocale of
    Nothing -> Left MessageNotFound
    Just (MessageTemplate template) -> unsafePerformIO (renderIcuMessage requestedLocale template (Map.toAscList arguments))
{-# NOINLINE renderLocalizedMessage #-}

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
