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
import Data.ByteString.Lazy qualified as LazyByteString
import Data.IORef qualified as IORef
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Api
import HarchWeb.Api.Multipart
import HarchWeb.Server (RequestBodyReadFailure (..), readRequestBodyUpTo)
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
    { apiBodyDecoderMediaType = jsonMediaType,
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
    acceptHeaderText = requestHeaderText HttpTypes.hAccept request

handleGreetingTarget :: Wai.Request -> GreetingTarget -> IO ApiResponseBody
handleGreetingTarget request ReadGreeting =
  pure (renderGreeting request (greetingFor "World"))
handleGreetingTarget request SubmitGreeting = do
  bodyResult <- readRequestBodyUpTo maxGreetingBodyReadBytes request
  pure $
    case bodyResult of
      Left RequestBodyLimitExceeded -> apiTextResponse "request body too large"
      Right bodyBytes ->
        case selectApiBodyDecoder RejectMissingContentType maxGreetingBodyBytes [greetingRequestBodyDecoder] contentTypeHeaderText (LazyByteString.toStrict bodyBytes) of
          ApiDecodedBody greetingRequest -> renderGreeting request (greetingFor (requestedName greetingRequest))
          ApiUnsupportedMediaType _ -> apiTextResponse "unsupported media type; send application/json"
          ApiBodyTooLarge -> apiTextResponse "request body too large"
          ApiMalformedBody -> apiTextResponse "malformed JSON body"
  where
    contentTypeHeaderText = requestHeaderText HttpTypes.hContentType request
handleGreetingTarget request UploadAvatar = handleAvatarUpload request

handleAvatarUpload :: Wai.Request -> IO ApiResponseBody
handleAvatarUpload request = do
  partCountReference <- IORef.newIORef (0 :: Int)
  result <-
    withMultipartRequestBodyWith defaultMultipartLimits request $ \_part -> do
      IORef.modifyIORef' partCountReference (+ 1)
      pure (Right ())
  partCount <- IORef.readIORef partCountReference
  pure $ case result of
    Right () -> apiTextResponse (Text.pack (show partCount) <> " part(s) received")
    Left _consumeError -> apiTextResponse "invalid multipart body"

requestHeaderText :: HttpTypes.HeaderName -> Wai.Request -> Maybe Text
requestHeaderText headerName request =
  lookup headerName (Wai.requestHeaders request) >>= either (const Nothing) Just . TextEncoding.decodeUtf8'

maxGreetingBodyBytes :: Int
maxGreetingBodyBytes = 16 * 1024

-- | Keep one extra byte while reading so 'selectApiBodyDecoder' can return
-- its typed 'ApiBodyTooLarge' outcome at the declared boundary. Any larger
-- streamed body is still rejected by 'readRequestBodyUpTo' as it arrives.
maxGreetingBodyReadBytes :: Int
maxGreetingBodyReadBytes = maxGreetingBodyBytes + 1
