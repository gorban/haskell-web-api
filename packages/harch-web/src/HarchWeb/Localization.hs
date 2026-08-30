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
--
-- FQ10 extends this existing localization boundary instead of adding a second
-- renderer: every UTF-8 input crosses the ICU ABI with its byte length, and
-- ICU's returned bytes are decoded explicitly. Embedded NUL values therefore
-- retain their complete value, while malformed native output remains the
-- ordinary 'MessageFormatRejected' result rather than an exception or
-- replacement text.
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
import Data.Text.Encoding qualified as TextEncoding
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Marshal.Alloc (alloca, free)
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (withMany)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek, poke)
import HarchWeb.Localization.Internal (MessageRenderError (..), decodeRenderedIcuUtf8)
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

renderLocalizedMessage :: Localizer messageKey -> messageKey -> Locale -> MessageArguments -> Either MessageRenderError Text
renderLocalizedMessage (Localizer lookupTemplate) messageKey requestedLocale (MessageArguments arguments) =
  case lookupTemplate messageKey requestedLocale of
    Nothing -> Left MessageNotFound
    Just (MessageTemplate template) -> unsafePerformIO (renderIcuMessage requestedLocale template (Map.toAscList arguments))
{-# NOINLINE renderLocalizedMessage #-}

renderIcuMessage :: Locale -> Text -> [(Text, MessageArgument)] -> IO (Either MessageRenderError Text)
renderIcuMessage (Locale requestedLocale) template arguments =
  ByteString.useAsCStringLen localeBytes $ \(localeValue, localeLength) ->
    ByteString.useAsCStringLen templateBytes $ \(templateValue, templateLength) ->
      withUtf8CStrings argumentNameBytes $ \names nameLengths ->
        withUtf8CStrings argumentTextBytes $ \texts textLengths ->
          withArray (map (argumentNumber . snd) arguments) $ \argumentNumbers ->
            withArray (map (argumentKind . snd) arguments) $ \argumentKinds ->
              alloca $ \resultPointer ->
                alloca $ \resultLengthPointer -> do
                  poke resultPointer nullPtr
                  poke resultLengthPointer 0
                  status <- c_formatMessage localeValue (fromIntegral localeLength) templateValue (fromIntegral templateLength) names nameLengths texts textLengths argumentNumbers argumentKinds (fromIntegral (length arguments)) resultPointer resultLengthPointer
                  result <- peek resultPointer
                  resultLength <- peek resultLengthPointer
                  if status /= 0 || result == nullPtr || resultLength > fromIntegral (maxBound :: Int)
                    then pure (Left MessageFormatRejected)
                    else bracket (pure result) free $ \allocatedResult ->
                      decodeRenderedIcuUtf8 <$> ByteString.packCStringLen (allocatedResult, fromIntegral resultLength)
  where
    localeBytes = TextEncoding.encodeUtf8 requestedLocale
    templateBytes = TextEncoding.encodeUtf8 template
    argumentNameBytes = map (TextEncoding.encodeUtf8 . fst) arguments
    argumentTextBytes = map (TextEncoding.encodeUtf8 . argumentText . snd) arguments
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

withUtf8CStrings :: [ByteString.ByteString] -> (Ptr CString -> Ptr CSize -> IO result) -> IO result
withUtf8CStrings values use =
  withMany ByteString.useAsCStringLen values $ \encodedValues ->
    withArray (map fst encodedValues) $ \valuePointers ->
      withArray (map (fromIntegral . snd) encodedValues) $ \valueLengths ->
        use valuePointers valueLengths

foreign import ccall unsafe "harch_web_format_message"
  c_formatMessage :: CString -> CSize -> CString -> CSize -> Ptr CString -> Ptr CSize -> Ptr CString -> Ptr CSize -> Ptr Int64 -> Ptr CInt -> CSize -> Ptr CString -> Ptr CSize -> IO CInt
