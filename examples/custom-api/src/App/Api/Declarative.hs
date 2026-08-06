{-# LANGUAGE OverloadedStrings #-}

-- | A small, compiled demonstration of "HarchWeb.Api": a negotiated GET
-- response (JSON or an application-defined media type), a JSON request
-- body, and a bounded streaming multipart upload, all dispatched through
-- 'apiEndpointMiddleware'. See ../../README.md.
module App.Api.Declarative
  ( GreetingTarget (..),
    GreetingRequest (..),
    GreetingResponse (..),
    declarativeApiEndpoints,
    declarativeApiApplication,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson.Types
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Api
import HarchWeb.Api.Multipart
import Network.HTTP.Types qualified as HttpTypes
import Network.Wai qualified as Wai

data GreetingTarget
  = ReadGreeting
  | SubmitGreeting
  | UploadAvatar

declarativeApiEndpoints :: [ApiEndpoint GreetingTarget]
declarativeApiEndpoints =
  [ apiEndpoint ReadGreeting ApiGet (at "/api/greeting"),
    apiEndpoint SubmitGreeting ApiPost (at "/api/greeting"),
    apiEndpoint UploadAvatar ApiPost (at "/api/avatar")
  ]

-- | An application opts into 'HarchWeb.Api' dispatch for just these three
-- paths by wrapping its own 'Wai.Application' with this middleware; every
-- other request reaches the wrapped application unchanged.
declarativeApiApplication :: Wai.Application -> Wai.Application
declarativeApiApplication = apiEndpointMiddleware declarativeApiEndpoints handleGreetingTarget

newtype GreetingRequest = GreetingRequest {requestedName :: Text}

-- | A hand-written 'ApiBodyDecoder' rather than the 'jsonBodyDecoder'
-- built-in's 'Data.Aeson.FromJSON': this is the extension point an
-- application uses for a decoder shape 'HarchWeb.Api' does not build in.
greetingRequestBodyDecoder :: ApiBodyDecoder GreetingRequest
greetingRequestBodyDecoder =
  ApiBodyDecoder
    { apiBodyDecoderMediaType = "application/json",
      apiBodyDecoderParse = \bodyBytes ->
        case Aeson.eitherDecodeStrict bodyBytes >>= Aeson.Types.parseEither parseGreetingRequest of
          Left errorMessage -> Left $! Text.pack errorMessage
          Right greetingRequest -> Right greetingRequest
    }

parseGreetingRequest :: Aeson.Value -> Aeson.Types.Parser GreetingRequest
parseGreetingRequest = (Aeson.withObject $! "GreetingRequest") $ \fields ->
  GreetingRequest <$> fields Aeson.Types..: "requestedName"

newtype GreetingResponse = GreetingResponse {greetingText :: Text}

encodeGreetingResponse :: GreetingResponse -> Aeson.Value
encodeGreetingResponse greeting = Aeson.object ["greetingText" Aeson..= greetingText greeting]

greetingFor :: Text -> GreetingResponse
greetingFor name = GreetingResponse ("Hello, " <> name <> "!")

-- | Negotiate between JSON (the framework/application default) and the
-- application-defined @text/x-greeting@ media type, declared only for this
-- endpoint.
renderGreeting :: Wai.Request -> GreetingResponse -> ApiResponseBody
renderGreeting request greeting =
  case selectRepresentation ("application/json" :| ["text/x-greeting"]) acceptHeaderText of
    SelectedRepresentation "text/x-greeting" ->
      (apiBytesResponse $! "text/x-greeting") (TextEncoding.encodeUtf8 ("GREETING " <> greetingText greeting))
    _ -> (apiBytesResponse $! "application/json; charset=utf-8") (LazyByteString.toStrict (Aeson.encode (encodeGreetingResponse greeting)))
  where
    acceptHeaderText = TextEncoding.decodeUtf8 <$> lookup HttpTypes.hAccept (Wai.requestHeaders request)

handleGreetingTarget :: Wai.Request -> GreetingTarget -> IO ApiResponseBody
handleGreetingTarget request ReadGreeting =
  pure (renderGreeting request (greetingFor "World"))
handleGreetingTarget request SubmitGreeting = do
  bodyBytes <- readBoundedBody maxGreetingBodyBytes request
  pure $ case selectApiBodyDecoder RejectMissingContentType maxGreetingBodyBytes [greetingRequestBodyDecoder] contentTypeHeaderText bodyBytes of
    ApiDecodedBody greetingRequest -> renderGreeting request (greetingFor (requestedName greetingRequest))
    ApiUnsupportedMediaType _ -> apiTextResponse "unsupported media type; send application/json"
    ApiBodyTooLarge -> apiTextResponse "request body too large"
    ApiMalformedBody -> apiTextResponse "malformed JSON body"
  where
    contentTypeHeaderText = TextEncoding.decodeUtf8 <$> lookup HttpTypes.hContentType (Wai.requestHeaders request)
handleGreetingTarget request UploadAvatar =
  case lookup HttpTypes.hContentType (Wai.requestHeaders request) >>= multipartBoundary of
    Nothing -> pure (apiTextResponse "missing multipart boundary")
    Just boundary -> do
      result <- consumeMultipartRequestBody defaultMultipartLimits boundary request
      pure $ case result of
        Right parts -> apiTextResponse (Text.pack (show (length parts)) <> " part(s) received")
        Left _consumeError -> apiTextResponse "invalid multipart body"

maxGreetingBodyBytes :: Int
maxGreetingBodyBytes = 16 * 1024

-- | Reads at most @maxBytes + 1@ bytes so an oversized body is still
-- detectable by 'selectApiBodyDecoder' without buffering an unbounded body
-- first.
readBoundedBody :: Int -> Wai.Request -> IO ByteString
readBoundedBody maxBytes request = go ByteString.empty
  where
    go accumulated
      | ByteString.length accumulated > maxBytes = pure accumulated
      | otherwise = do
          chunk <- Wai.getRequestBodyChunk request
          if ByteString.null chunk
            then pure accumulated
            else go (accumulated <> chunk)

-- | Extracts the @boundary@ parameter from a @Content-Type@ header value,
-- unquoting it if quoted. A minimal parser for this example; a real
-- application should use a full media-type parameter parser.
multipartBoundary :: ByteString -> Maybe ByteString
multipartBoundary contentTypeValue =
  case ByteString.breakSubstring "boundary=" contentTypeValue of
    (_, suffix)
      | ByteString.null suffix -> Nothing
      | otherwise -> Just (unquote (ByteString.takeWhile (/= 59) (ByteString.drop 9 suffix)))
  where
    unquote value
      | ByteString.length value >= 2,
        ByteString.head value == 34,
        ByteString.last value == 34 =
          ByteString.init (ByteString.tail value)
      | otherwise = value
