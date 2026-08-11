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
    requiredField,
    optionalField,
    fieldWithDefault,
    runRequestCodec,
  )
where

import Data.ByteString (ByteString)
import Data.CaseInsensitive qualified as CaseInsensitive
import Data.Functor.Compose (Compose (..), getCompose)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.Encoding.Error qualified as TextEncodingError
import Network.Wai qualified as Wai

data ApiRequestData = ApiRequestData
  { apiRequestQueryParameters :: [(Text, Text)],
    apiRequestHeaders :: [(ApiHeaderName, Text)]
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
        ]
    }

{-# ANN decodeUtf8Leniently ("HLint: ignore Eta reduce" :: String) #-}
decodeUtf8Leniently :: ByteString -> Text
decodeUtf8Leniently bytes = TextEncoding.decodeUtf8With TextEncodingError.lenientDecode bytes

data ApiRequestSource
  = ApiQuerySource
  | ApiHeaderSource
  deriving (Eq, Show)

data ApiRequestParseError
  = MissingApiField ApiRequestSource Text
  | DuplicateApiField ApiRequestSource Text
  | InvalidApiField ApiRequestSource Text
  deriving (Eq, Show)

newtype ApiHeaderName = ApiHeaderName Text
  deriving (Eq, Show)

apiHeaderName :: Text -> ApiHeaderName
apiHeaderName = ApiHeaderName . Text.toCaseFold

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

sourceFields :: ApiRequestSource -> ApiRequestData -> [(Text, Text)]
sourceFields source requestData =
  case source of
    ApiQuerySource -> apiRequestQueryParameters requestData
    ApiHeaderSource -> [(apiHeaderNameText name, value) | (name, value) <- apiRequestHeaders requestData]

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
