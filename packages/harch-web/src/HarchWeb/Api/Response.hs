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
    ApiResponse (..),
    apiResponse,
    ApiResponseEncoder (..),
    jsonResponseEncoder,
    textResponseEncoder,
    bytesResponseEncoder,
    ApiResponseBody (..),
    apiJsonResponse,
    apiTextResponse,
    apiBytesResponse,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Api.MediaType
import Network.HTTP.Types qualified as HttpTypes

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
  | ApiBodyTooLarge
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

-- | One declared way to render a typed API result.
data ApiResponseEncoder response = ApiResponseEncoder
  { apiResponseEncoderContentType :: ApiContentType,
    apiResponseEncoderEncode :: response -> ByteString
  }

jsonResponseEncoder :: (ToJSON response) => ApiResponseEncoder response
jsonResponseEncoder =
  ApiResponseEncoder
    { apiResponseEncoderContentType = jsonContentType,
      apiResponseEncoderEncode = LazyByteString.toStrict . Aeson.encode
    }

textResponseEncoder :: ApiResponseEncoder Text
textResponseEncoder =
  ApiResponseEncoder
    { apiResponseEncoderContentType = plainTextContentType,
      apiResponseEncoderEncode = TextEncoding.encodeUtf8
    }

bytesResponseEncoder :: ApiContentType -> ApiResponseEncoder ByteString
bytesResponseEncoder contentType =
  ApiResponseEncoder
    { apiResponseEncoderContentType = contentType,
      apiResponseEncoderEncode = id
    }

-- | Select a declared decoder by the request's @Content-Type@ (ignoring its
-- parameters) and run it against an already-bounded body. Never reads more
-- of the body itself; the caller supplies @maxBodyBytes@ enforcement.
selectApiBodyDecoder ::
  MissingContentTypePolicy ->
  Int ->
  [ApiBodyDecoder request] ->
  Maybe Text ->
  ByteString ->
  ApiBodyOutcome request
selectApiBodyDecoder missingPolicy maxBodyBytes decoders maybeContentType bodyBytes
  | ByteString.length bodyBytes > maxBodyBytes = ApiBodyTooLarge
  | otherwise =
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
