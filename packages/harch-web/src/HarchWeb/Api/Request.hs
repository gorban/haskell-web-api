{-# LANGUAGE OverloadedStrings #-}

-- | Typed request-field declarations and their WAI extraction boundary.
module HarchWeb.Api.Request
  ( ApiRequestData (..),
    apiRequestDataFromWaiRequest,
    ApiRequestSource (..),
    ApiRequestParseError (..),
    ApiHeaderName,
    apiHeaderName,
    apiHeaderNameText,
    ApiFieldValue,
    RequestField,
    RequestCodec,
    apiTextValue,
    parseApiField,
    queryField,
    headerField,
    cookieField,
    formField,
    requiredField,
    optionalField,
    fieldWithDefault,
    runRequestCodec,
    runApiFormCodec,
    apiRequestDataWithForm,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.CaseInsensitive qualified as CaseInsensitive
import Data.Char (chr, isAsciiUpper, ord)
import Data.Functor.Compose (Compose (..), getCompose)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.Encoding.Error qualified as TextEncodingError
import Data.Word (Word8)
import HarchWeb.Api.Response (ApiForm, apiFormFields)
import Network.Wai qualified as Wai

data ApiRequestData = ApiRequestData
  { apiRequestQueryParameters :: [(Text, Text)],
    apiRequestHeaders :: [(ApiHeaderName, Text)],
    -- | Cookie names are case-sensitive, unlike HTTP header names.
    apiRequestCookies :: [(Text, Text)],
    apiRequestFormFields :: [(Text, Text)]
  }
  deriving (Eq, Show)

apiRequestDataFromWaiRequest :: Wai.Request -> ApiRequestData
apiRequestDataFromWaiRequest request =
  ApiRequestData
    { apiRequestQueryParameters =
        [ (decodeUtf8Leniently name, maybe "" decodeUtf8Leniently value)
        | (name, value) <- Wai.queryString request
        ],
      apiRequestHeaders =
        [ (apiHeaderName (decodeUtf8Leniently (CaseInsensitive.foldedCase name)), decodeUtf8Leniently value)
        | (name, value) <- Wai.requestHeaders request
        ],
      apiRequestCookies =
        concatMap requestCookies (Wai.requestHeaders request),
      apiRequestFormFields = []
    }

requestCookies :: (CaseInsensitive.CI ByteString, ByteString) -> [(Text, Text)]
requestCookies (headerName, headerValue)
  | CaseInsensitive.foldedCase headerName /= "cookie" = []
  | otherwise = concatMap mapMaybeCookie (ByteString.split 59 headerValue)
  where
    mapMaybeCookie cookie =
      case ByteString.break (== 61) (ByteString.dropWhile isCookieWhitespace cookie) of
        (name, valueWithSeparator)
          | ByteString.null name || ByteString.null valueWithSeparator || not (ByteString.all isCookieNameByte name) -> []
          | otherwise -> [(decodeUtf8Leniently name, decodeUtf8Leniently (ByteString.drop 1 valueWithSeparator))]

isCookieWhitespace :: Word8 -> Bool
isCookieWhitespace byte = byte == 32 || byte == 9

isCookieNameByte :: Word8 -> Bool
isCookieNameByte byte =
  byte == 33
    || (byte >= 35 && byte <= 39)
    || byte == 42
    || byte == 43
    || byte == 45
    || byte == 46
    || (byte >= 48 && byte <= 57)
    || (byte >= 65 && byte <= 90)
    || (byte >= 94 && byte <= 122)
    || byte == 124
    || byte == 126

{-# ANN decodeUtf8Leniently ("HLint: ignore Eta reduce" :: String) #-}
decodeUtf8Leniently :: ByteString -> Text
decodeUtf8Leniently bytes = TextEncoding.decodeUtf8With TextEncodingError.lenientDecode bytes

data ApiRequestSource
  = ApiQuerySource
  | ApiHeaderSource
  | ApiCookieSource
  | ApiFormSource
  deriving (Eq, Show)

data ApiRequestParseError
  = MissingApiField ApiRequestSource Text
  | DuplicateApiField ApiRequestSource Text
  | InvalidApiField ApiRequestSource Text
  deriving (Eq, Show)

newtype ApiHeaderName = ApiHeaderName Text
  deriving (Eq, Show)

apiHeaderName :: Text -> ApiHeaderName
apiHeaderName = ApiHeaderName . Text.map asciiLower

-- HTTP field names are ASCII protocol tokens. Unicode case folding would make
-- unrelated non-ASCII text (such as the Kelvin sign) collide with a valid
-- ASCII header name.
asciiLower :: Char -> Char
asciiLower character
  | isAsciiUpper character = chr (ord character + 32)
  | otherwise = character

apiHeaderNameText :: ApiHeaderName -> Text
apiHeaderNameText (ApiHeaderName name) = name

newtype ApiFieldValue value = ApiFieldValue
  { runApiFieldValue :: Text -> Maybe value
  }

apiTextValue :: ApiFieldValue Text
apiTextValue = ApiFieldValue Just

parseApiField :: (Text -> Maybe value) -> ApiFieldValue value
parseApiField = ApiFieldValue

newtype RequestField value = RequestField (ApiRequestData -> ([ApiRequestParseError], Maybe value))

type RequestCodec value = Compose ((->) ApiRequestData) (Compose ((,) [ApiRequestParseError]) Maybe) value

requestCodec :: (ApiRequestData -> ([ApiRequestParseError], Maybe value)) -> RequestCodec value
requestCodec decode = Compose (Compose . decode)

runRequestCodec :: RequestCodec value -> ApiRequestData -> ([ApiRequestParseError], Maybe value)
runRequestCodec codec requestData = getCompose (getCompose codec requestData)

-- | Run a field codec against the fields decoded from one bounded form body.
-- The caller owns that single body-consumption declaration.
runApiFormCodec :: RequestCodec value -> ApiForm -> ([ApiRequestParseError], Maybe value)
runApiFormCodec codec formValue =
  runRequestCodec codec (ApiRequestData [] [] [] (apiFormFields formValue))

-- | Add fields from a successfully decoded form to the original request data.
-- This preserves query, header, and cookie declarations when an endpoint
-- declares the form as its single body consumer.
apiRequestDataWithForm :: ApiForm -> ApiRequestData -> ApiRequestData
apiRequestDataWithForm formValue requestData =
  requestData {apiRequestFormFields = apiFormFields formValue}

sourceFields :: ApiRequestSource -> ApiRequestData -> [(Text, Text)]
sourceFields source requestData =
  case source of
    ApiQuerySource -> apiRequestQueryParameters requestData
    ApiHeaderSource -> [(apiHeaderNameText name, value) | (name, value) <- apiRequestHeaders requestData]
    ApiCookieSource -> apiRequestCookies requestData
    ApiFormSource -> apiRequestFormFields requestData

requestField :: ApiRequestSource -> Text -> ApiFieldValue value -> RequestField value
requestField source fieldName valueDecoder =
  RequestField $ \requestData ->
    case [fieldValue | (name, fieldValue) <- sourceFields source requestData, name == fieldName] of
      [fieldValue] ->
        maybe
          ([InvalidApiField source fieldName], Nothing)
          (\value -> ([], Just value))
          (runApiFieldValue valueDecoder fieldValue)
      [] -> ([MissingApiField source fieldName], Nothing)
      _ -> ([DuplicateApiField source fieldName], Nothing)

queryField :: Text -> ApiFieldValue value -> RequestField value
queryField = requestField ApiQuerySource

headerField :: ApiHeaderName -> ApiFieldValue value -> RequestField value
headerField name = requestField ApiHeaderSource (apiHeaderNameText name)

-- | Declare a case-sensitive request cookie. Repeated cookie names are a
-- parse error rather than selecting an arbitrary value.
cookieField :: Text -> ApiFieldValue value -> RequestField value
cookieField = requestField ApiCookieSource

-- | Declare a field from one decoded URL-encoded form body.
formField :: Text -> ApiFieldValue value -> RequestField value
formField = requestField ApiFormSource

requiredField :: RequestField value -> RequestCodec value
requiredField (RequestField decode) = requestCodec decode

optionalField :: RequestField value -> RequestCodec (Maybe value)
optionalField (RequestField decode) =
  requestCodec $ \requestData ->
    case decode requestData of
      ([], Just value) -> ([], Just (Just value))
      ([MissingApiField _ _], Nothing) -> ([], Just Nothing)
      (parseErrors, _) -> (parseErrors, Nothing)

fieldWithDefault :: value -> RequestField value -> RequestCodec value
fieldWithDefault defaultValue (RequestField decode) =
  requestCodec $ \requestData ->
    case decode requestData of
      ([], Just value) -> ([], Just value)
      ([MissingApiField _ _], Nothing) -> ([], Just defaultValue)
      parseErrors -> parseErrors
