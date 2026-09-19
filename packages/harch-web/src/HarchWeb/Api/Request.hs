{-# LANGUAGE OverloadedStrings #-}

-- | Typed request-field declarations and their WAI extraction boundary.
--
-- Decision record (PR-F4, 2026-08-24): 'RequestCodec' owns the complete
-- result invariant, rather than exposing its former nested @Compose@ encoding
-- for callers to construct directly.  The existing accumulating applicative
-- remains the only way to combine field declarations, and its public runner
-- now returns a decoded value, a non-empty ordered rejection, or an explicit
-- invalid-codec outcome. This extends the existing API-decoding boundary
-- instead of adding a second validator at endpoint runtime, so a
-- field-failure renderer can never receive an empty error list. See
-- @docs/design-guidance.md@.
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
    ApiRequestDecodeResult (..),
    noRequestFields,
    apiTextValue,
    parseApiField,
    queryField,
    headerField,
    cookieField,
    formField,
    requiredField,
    optionalField,
    fieldWithDefault,
    requestCodec,
    runRequestCodec,
    runApiFormCodec,
    apiRequestDataWithForm,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.CaseInsensitive qualified as CaseInsensitive
import Data.Functor.Compose (Compose (..), getCompose)
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.Encoding.Error qualified as TextEncodingError
import Data.Word (Word8)
import HarchWeb.Api.HeaderName
  ( ApiHeaderName,
    apiHeaderName,
    apiHeaderNameText,
  )
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
      apiRequestHeaders = mapMaybe requestHeader (Wai.requestHeaders request),
      apiRequestCookies =
        concatMap requestCookies (Wai.requestHeaders request),
      apiRequestFormFields = []
    }

requestHeader :: (CaseInsensitive.CI ByteString, ByteString) -> Maybe (ApiHeaderName, Text)
requestHeader (name, value) =
  (,decodeUtf8Leniently value) <$> apiHeaderName (decodeUtf8Leniently (CaseInsensitive.foldedCase name))

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

decodeUtf8Leniently :: ByteString -> Text
decodeUtf8Leniently = TextEncoding.decodeUtf8With TextEncodingError.lenientDecode

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

newtype ApiFieldValue value = ApiFieldValue
  { runApiFieldValue :: Text -> Maybe value
  }

apiTextValue :: ApiFieldValue Text
apiTextValue = ApiFieldValue Just

parseApiField :: (Text -> Maybe value) -> ApiFieldValue value
parseApiField = ApiFieldValue

newtype RequestField value = RequestField (ApiRequestData -> Either (NonEmpty ApiRequestParseError) value)

-- | The total outcome of decoding an API endpoint's declared request fields.
-- A rejection always carries at least one ordered, stable parse error; an
-- explicit invalid outcome remains a generic endpoint rejection.
data ApiRequestDecodeResult value
  = ApiRequestDecoded value
  | ApiRequestRejected (NonEmpty ApiRequestParseError)
  | ApiRequestCodecInvalid

-- | An opaque, accumulating API request decoder. Build one with the field
-- combinators below; its constructor stays private so invalid success/error
-- combinations cannot reach endpoint interpretation.
newtype RequestCodec value
  = RequestCodec (Compose ((->) ApiRequestData) (Compose ((,) [ApiRequestParseError]) Maybe) value)

instance Functor RequestCodec where
  fmap transform (RequestCodec codec) =
    RequestCodec (fmap transform codec)

instance Applicative RequestCodec where
  pure value = RequestCodec (pure value)
  RequestCodec functionCodec <*> RequestCodec valueCodec =
    RequestCodec (functionCodec <*> valueCodec)

-- | A named declaration for endpoints that intentionally decode no query,
-- header, cookie, or form fields. It communicates that the empty input is
-- part of the endpoint contract without repeating a raw unit codec at every
-- declaration site.
noRequestFields :: RequestCodec ()
noRequestFields = pure ()

requestCodec :: (ApiRequestData -> ApiRequestDecodeResult value) -> RequestCodec value
requestCodec decode =
  RequestCodec . Compose $ \requestData ->
    Compose (toCodecRepresentation (decode requestData))

runRequestCodec :: RequestCodec value -> ApiRequestData -> ApiRequestDecodeResult value
runRequestCodec (RequestCodec codec) requestData =
  fromCodecRepresentation (getCompose (getCompose codec requestData))

-- | Run a field codec against the fields decoded from one bounded form body.
-- The caller owns that single body-consumption declaration.
runApiFormCodec :: RequestCodec value -> ApiForm -> ApiRequestDecodeResult value
runApiFormCodec codec formValue =
  runRequestCodec codec (ApiRequestData [] [] [] (apiFormFields formValue))

toCodecRepresentation :: ApiRequestDecodeResult value -> ([ApiRequestParseError], Maybe value)
toCodecRepresentation result =
  case result of
    ApiRequestDecoded value -> ([], Just value)
    ApiRequestRejected parseErrors -> (toList parseErrors, Nothing)
    ApiRequestCodecInvalid -> ([], Nothing)

fromCodecRepresentation :: ([ApiRequestParseError], Maybe value) -> ApiRequestDecodeResult value
fromCodecRepresentation representation =
  case representation of
    ([], Nothing) -> ApiRequestCodecInvalid
    (firstError : remainingErrors, Nothing) -> ApiRequestRejected (firstError :| remainingErrors)
    (_, Just value) -> ApiRequestDecoded value

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
          (Left (InvalidApiField source fieldName :| []))
          Right
          (runApiFieldValue valueDecoder fieldValue)
      [] -> Left (MissingApiField source fieldName :| [])
      _ -> Left (DuplicateApiField source fieldName :| [])

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
requiredField (RequestField decode) =
  requestCodec $ \requestData ->
    either ApiRequestRejected ApiRequestDecoded (decode requestData)

optionalField :: RequestField value -> RequestCodec (Maybe value)
optionalField (RequestField decode) =
  requestCodec $ \requestData ->
    case decode requestData of
      Right value -> ApiRequestDecoded (Just value)
      Left (MissingApiField _ _ :| []) -> ApiRequestDecoded Nothing
      Left parseErrors -> ApiRequestRejected parseErrors

fieldWithDefault :: value -> RequestField value -> RequestCodec value
fieldWithDefault defaultValue (RequestField decode) =
  requestCodec $ \requestData ->
    case decode requestData of
      Right value -> ApiRequestDecoded value
      Left (MissingApiField _ _ :| []) -> ApiRequestDecoded defaultValue
      Left parseErrors -> ApiRequestRejected parseErrors
