{-# LANGUAGE OverloadedStrings #-}

-- | Buffered API request decoding and response rendering.
module HarchWeb.Api.Response
  ( ApiBodyDecoder (..),
    MissingContentTypePolicy (..),
    ApiBodyOutcome (..),
    selectApiBodyDecoder,
    jsonBodyDecoder,
    textBodyDecoder,
    bytesBodyDecoder,
    ApiForm,
    apiFormFields,
    urlEncodedFormBodyDecoder,
    ApiResponse (..),
    apiResponse,
    ApiEncodedResponseBody (..),
    ApiResponseEncoder (..),
    jsonResponseEncoder,
    textResponseEncoder,
    bytesResponseEncoder,
    streamingResponseEncoder,
    ApiResponseBody (..),
    apiJsonResponse,
    apiTextResponse,
    apiBytesResponse,
  )
where

import Control.Monad (unless)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word8)
import HarchWeb.Api.MediaType
import Network.HTTP.Types qualified as HttpTypes
import Network.HTTP.Types.URI qualified as HttpUri
import Network.Wai qualified as Wai
import Numeric.Natural (Natural)

-- | Decodes a fully-buffered request body declared for one @Content-Type@
-- media type (ignoring its parameters, e.g. @charset@). A streaming body
-- decoder, such as multipart, is a separate, non-buffered concern; see
-- 'HarchWeb.Api.Multipart'.
data ApiBodyDecoder request = ApiBodyDecoder
  { apiBodyDecoderMediaType :: ApiMediaType,
    apiBodyDecoderParse :: ByteString -> Either Text request
  }

-- | What a missing @Content-Type@ header means for a declared endpoint.
data MissingContentTypePolicy
  = RejectMissingContentType
  | AssumeMediaType ApiMediaType
  deriving (Eq, Show)

data ApiBodyOutcome request
  = ApiUnsupportedMediaType [ApiMediaType]
  | ApiMalformedBody
  | ApiDecodedBody request
  deriving (Eq, Show)

-- | A typed endpoint result before its representation is selected. Status and
-- headers are application data; encoding remains declared by the endpoint.
data ApiResponse response = ApiResponse
  { apiEndpointResponseStatus :: HttpTypes.Status,
    apiEndpointResponseHeaders :: [(Text, Text)],
    apiEndpointResponseValue :: response
  }

-- | Construct an ordinary successful API result without application headers.
apiResponse :: response -> ApiResponse response
apiResponse responseValue =
  ApiResponse
    { apiEndpointResponseStatus = HttpTypes.status200,
      apiEndpointResponseHeaders = [],
      apiEndpointResponseValue = responseValue
    }

-- | A response encoder either produces strict bytes or a request-scoped WAI
-- stream. The latter deliberately cannot be converted into an application
-- owned lazy value, so it stays inside the server's protocol interpreter.
data ApiEncodedResponseBody
  = ApiEncodedResponseBytes ByteString
  | ApiEncodedResponseStream Wai.StreamingBody

-- | One declared way to render a typed API result.
data ApiResponseEncoder response = ApiResponseEncoder
  { apiResponseEncoderContentType :: ApiContentType,
    apiResponseEncoderEncode :: response -> ApiEncodedResponseBody
  }

jsonResponseEncoder :: (ToJSON response) => ApiResponseEncoder response
jsonResponseEncoder =
  ApiResponseEncoder
    { apiResponseEncoderContentType = jsonContentType,
      apiResponseEncoderEncode = ApiEncodedResponseBytes . LazyByteString.toStrict . Aeson.encode
    }

textResponseEncoder :: ApiResponseEncoder Text
textResponseEncoder =
  ApiResponseEncoder
    { apiResponseEncoderContentType = plainTextContentType,
      apiResponseEncoderEncode = ApiEncodedResponseBytes . TextEncoding.encodeUtf8
    }

bytesResponseEncoder :: ApiContentType -> ApiResponseEncoder ByteString
bytesResponseEncoder contentType =
  ApiResponseEncoder
    { apiResponseEncoderContentType = contentType,
      apiResponseEncoderEncode = ApiEncodedResponseBytes
    }

-- | Declare a response encoder that writes only while WAI renders the active
-- request. This is suitable for downloads, progressive encodings, and finite
-- streams; use the separate SSE response capability for event subscriptions.
streamingResponseEncoder :: ApiContentType -> (response -> Wai.StreamingBody) -> ApiResponseEncoder response
streamingResponseEncoder contentType renderStream =
  ApiResponseEncoder
    { apiResponseEncoderContentType = contentType,
      apiResponseEncoderEncode = ApiEncodedResponseStream . renderStream
    }

-- | Select a declared decoder by the request's @Content-Type@ (ignoring its
-- parameters) and run it against an already-bounded body.  Byte-limit
-- enforcement belongs to the body reader at the endpoint boundary, so this
-- pure operation has just the media-type and parse failure alternatives.
selectApiBodyDecoder ::
  MissingContentTypePolicy ->
  [ApiBodyDecoder request] ->
  Maybe Text ->
  ByteString ->
  ApiBodyOutcome request
selectApiBodyDecoder missingPolicy decoders maybeContentType bodyBytes =
  case resolvedMediaType of
    Nothing -> ApiUnsupportedMediaType declaredMediaTypes
    Just mediaType ->
      case [decoder | decoder <- decoders, apiBodyDecoderMediaType decoder == mediaType] of
        [] -> ApiUnsupportedMediaType declaredMediaTypes
        decoder : _ ->
          either (const ApiMalformedBody) ApiDecodedBody (apiBodyDecoderParse decoder bodyBytes)
  where
    declaredMediaTypes = map apiBodyDecoderMediaType decoders
    resolvedMediaType =
      case maybeContentType of
        Just contentTypeValue -> apiMediaType contentTypeValue
        Nothing ->
          case missingPolicy of
            RejectMissingContentType -> Nothing
            AssumeMediaType mediaType -> Just mediaType

jsonBodyDecoder :: (FromJSON request) => ApiBodyDecoder request
jsonBodyDecoder =
  ApiBodyDecoder
    { apiBodyDecoderMediaType = jsonMediaType,
      apiBodyDecoderParse = \bodyBytes ->
        case Aeson.eitherDecodeStrict' bodyBytes of
          Left errorMessage -> Left (Text.pack errorMessage)
          Right decodedValue -> Right decodedValue
    }

-- | Decodes a strict-UTF-8 @text/plain@ body; invalid UTF-8 is malformed.
textBodyDecoder :: ApiBodyDecoder Text
textBodyDecoder =
  ApiBodyDecoder
    { apiBodyDecoderMediaType = plainTextMediaType,
      apiBodyDecoderParse = \bodyBytes ->
        case TextEncoding.decodeUtf8' bodyBytes of
          Left _decodeError -> Left "invalid UTF-8 body"
          Right decodedText -> Right decodedText
    }

-- | Passes the body through unparsed for the given media type.
bytesBodyDecoder :: ApiMediaType -> ApiBodyDecoder ByteString
bytesBodyDecoder mediaType =
  ApiBodyDecoder
    { apiBodyDecoderMediaType = mediaType,
      apiBodyDecoderParse = Right
    }

-- | A bounded, decoded @application/x-www-form-urlencoded@ body. The order
-- and duplicates are retained so a typed field codec can reject ambiguity.
newtype ApiForm = ApiForm [(Text, Text)]
  deriving (Eq, Show)

apiFormFields :: ApiForm -> [(Text, Text)]
apiFormFields (ApiForm fields) = fields

-- | Decode a small URL-encoded form with an explicit field-count bound.
-- Byte limits remain owned by the enclosing 'ApiRequestBody' declaration.
urlEncodedFormBodyDecoder :: Natural -> ApiBodyDecoder ApiForm
urlEncodedFormBodyDecoder maximumFields =
  ApiBodyDecoder
    { apiBodyDecoderMediaType = urlEncodedFormMediaType,
      apiBodyDecoderParse = parseForm
    }
  where
    parseForm bodyBytes = do
      validatePercentEscapes bodyBytes
      let fields = HttpUri.parseQuery bodyBytes
      if fromIntegral (length fields) > maximumFields
        then Left "form contains more fields than declared"
        else ApiForm <$> traverse decodeField fields
    decodeField (name, maybeValue) =
      (,) <$> decodeUtf8Field name <*> maybe (Right "") decodeUtf8Field maybeValue

validatePercentEscapes :: ByteString -> Either Text ()
validatePercentEscapes bytes =
  unless (all (validPercentEscapeAt bytes) (ByteString.elemIndices 37 bytes)) invalidPercentEscapes

invalidPercentEscapes :: Either Text ()
invalidPercentEscapes = Left "form contains invalid percent encoding"

validPercentEscapeAt :: ByteString -> Int -> Bool
validPercentEscapeAt bytes percentIndex =
  case ByteString.unpack (ByteString.take 2 (ByteString.drop (percentIndex + 1) bytes)) of
    [firstDigit, secondDigit] -> isHexDigit firstDigit && isHexDigit secondDigit
    _ -> False

isHexDigit :: Word8 -> Bool
isHexDigit byte =
  (byte >= 48 && byte <= 57)
    || (byte >= 65 && byte <= 70)
    || (byte >= 97 && byte <= 102)

decodeUtf8Field :: ByteString -> Either Text Text
decodeUtf8Field bytes =
  case TextEncoding.decodeUtf8' bytes of
    Left _decodeError -> Left "form contains invalid UTF-8"
    Right fieldValue -> Right fieldValue

-- | A rendered API response body. Every built-in constructor defaults its
-- status to @200@; callers can deliberately override it with a record update.
data ApiResponseBody = ApiResponseBody
  { apiResponseStatus :: HttpTypes.Status,
    apiResponseContentType :: ApiContentType,
    apiResponseHeaders :: [(Text, Text)],
    apiResponseBodyBytes :: ByteString
  }
  deriving (Eq, Show)

apiJsonResponse :: (ToJSON value) => value -> ApiResponseBody
apiJsonResponse value =
  ApiResponseBody
    { apiResponseStatus = HttpTypes.status200,
      apiResponseContentType = jsonContentType,
      apiResponseHeaders = [],
      apiResponseBodyBytes = LazyByteString.toStrict (Aeson.encode value)
    }

apiTextResponse :: Text -> ApiResponseBody
apiTextResponse bodyText =
  ApiResponseBody
    { apiResponseStatus = HttpTypes.status200,
      apiResponseContentType = plainTextContentType,
      apiResponseHeaders = [],
      apiResponseBodyBytes = TextEncoding.encodeUtf8 bodyText
    }

apiBytesResponse :: ApiContentType -> ByteString -> ApiResponseBody
apiBytesResponse contentType bodyBytes =
  ApiResponseBody
    { apiResponseStatus = HttpTypes.status200,
      apiResponseContentType = contentType,
      apiResponseHeaders = [],
      apiResponseBodyBytes = bodyBytes
    }
