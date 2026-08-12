{-# LANGUAGE OverloadedStrings #-}

-- | A compiled demonstration of the legacy low-level 'HarchWeb.Api' helpers:
-- negotiated JSON/custom-media responses, a JSON body, and a bounded
-- multipart upload. It deliberately remains separate from the application's
-- shared route table until AC supplies the method-aware RouteCodec/
-- RouteDefinition endpoint boundary. See ../../README.md.
module App.Api.Declarative
  ( GreetingTarget (..),
    GreetingRequest (..),
    GreetingResponse (..),
    declarativeApiEndpoints,
    declarativeApiApplication,
    renderGreetingWith,
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

-- | Compatibility composition only. This owns just the explicitly declared
-- API paths and delegates every other request to the supplied application;
-- it cannot establish whole-application path/method ownership.
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

-- | Negotiates the two representations declared by this compatibility
-- example. The shared endpoint boundary will own this declaration alongside
-- path and method dispatch.
renderGreeting :: Wai.Request -> GreetingResponse -> ApiResponseBody
renderGreeting = renderGreetingWith (apiMediaType "text/x-greeting")

renderGreetingWith :: Maybe ApiMediaType -> Wai.Request -> GreetingResponse -> ApiResponseBody
renderGreetingWith Nothing _request _greeting =
  (apiTextResponse "API representation configuration is invalid") {apiResponseStatus = HttpTypes.status500}
renderGreetingWith (Just greetingMediaType) request greeting =
  case selectRepresentation (jsonMediaType :| [greetingMediaType]) acceptHeaderText of
    SelectedRepresentation selectedMediaType
      | selectedMediaType == greetingMediaType ->
          (apiBytesResponse $! apiContentType $! selectedMediaType) (TextEncoding.encodeUtf8 ("GREETING " <> greetingText greeting))
    _ -> (apiBytesResponse $! jsonContentType) (LazyByteString.toStrict (Aeson.encode (encodeGreetingResponse greeting)))
  where
    acceptHeaderText = requestHeaderText HttpTypes.hAccept request

handleGreetingTarget :: Wai.Request -> GreetingTarget -> IO ApiResponseBody
handleGreetingTarget request ReadGreeting =
  pure (renderGreeting request (greetingFor "World"))
handleGreetingTarget request SubmitGreeting = do
  bodyResult <- readRequestBodyUpTo maxGreetingBodyBytes request
  pure $
    case bodyResult of
      Left RequestBodyLimitExceeded -> apiTextResponse "request body too large"
      Right bodyBytes ->
        case selectApiBodyDecoder RejectMissingContentType [greetingRequestBodyDecoder] contentTypeHeaderText (LazyByteString.toStrict bodyBytes) of
          ApiDecodedBody greetingRequest -> renderGreeting request (greetingFor (requestedName greetingRequest))
          ApiUnsupportedMediaType _ -> apiTextResponse "unsupported media type; send application/json"
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
