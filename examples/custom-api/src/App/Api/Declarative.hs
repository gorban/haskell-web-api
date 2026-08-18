{-# LANGUAGE OverloadedStrings #-}

-- | A compiled demonstration of the typed 'HarchWeb.Api' endpoint boundary:
-- a hand-written request-body decoder and response encoders as the
-- extension points 'HarchWeb.Api' does not build in, negotiated JSON/custom
-- media responses, and a bounded multipart upload. AC closed the shared
-- method-aware 'HarchWeb.RouteCodec'/'HarchWeb.Site.RouteDefinition'
-- boundary this module used to wait on: 'declarativeApiEndpoints' is now
-- composed through 'HarchWeb.Api.apiRouteEndpointFamilyCodec'/
-- 'apiRouteEndpointFamilyDefinition' into an ordinary 'HarchWeb.Site.Site',
-- rather than the removed legacy @apiEndpoint@\/@apiEndpointMiddleware@
-- pair this module previously used. Migrating this example is the AC
-- follow-up named in @TASKS.md@; @web-api@'s own @\/api\/status@\/@\/api\/second@
-- routes remain a separate, larger follow-up (they additionally need
-- per-response observability attributes/log entries, a capability the typed
-- endpoint boundary does not yet expose). See ../../README.md and the AC
-- decision record in docs/design-guidance.md.
--
-- The endpoint table owns the whole application's routing, so this example
-- uses 'HarchWeb.Site.apiOnlySite'. That small extension of the shared site
-- boundary has no parallel API dispatcher and keeps a safe minimal SSR
-- fallback inside the framework should a future page route be added.
module App.Api.Declarative
  ( GreetingRequest (..),
    GreetingResponse (..),
    declarativeApiEndpoints,
    declarativeApiApplication,
    declarativeApiSite,
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
import HarchWeb qualified
import HarchWeb.Api
import HarchWeb.Api.Multipart (InMemoryUpload, defaultMultipartLimits, inMemoryMultipartStorage)
import HarchWeb.Site qualified as Site
import Network.Wai qualified as Wai

declarativeApiEndpoints :: [SomeApiRouteEndpoint]
declarativeApiEndpoints =
  [ SomeApiRouteEndpoint readGreetingEndpoint,
    SomeApiRouteEndpoint submitGreetingEndpoint,
    SomeApiRouteEndpoint uploadAvatarEndpoint
  ]

declarativeApiSite :: Site.Site ApiPath () ()
declarativeApiSite =
  Site.apiOnlySite
    "custom-api"
    ()
    (apiRouteEndpointFamilyCodec declarativeApiEndpoints)
    (apiRouteEndpointFamilyDefinition declarativeApiEndpoints)

declarativeApiApplication :: Wai.Application
declarativeApiApplication = HarchWeb.toWaiApplication (Site.buildSiteApplication declarativeApiSite)

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

-- | Negotiated response representations for both greeting endpoints. Listed
-- in server-preference order, so a missing or wildcard @Accept@ selects
-- JSON; 'HarchWeb.Api.Endpoint' negotiates and adds @Vary: Accept@ itself,
-- replacing this module's former hand-rolled 'selectRepresentation' call and
-- its accompanying defensive @Maybe ApiMediaType@ failure path, which cannot
-- arise now that each encoder's content type is a fixed, total value rather
-- than one parsed at request time.
greetingEncoders :: NonEmpty (ApiResponseEncoder GreetingResponse)
greetingEncoders = jsonGreetingEncoder :| [customGreetingEncoder]

jsonGreetingEncoder :: ApiResponseEncoder GreetingResponse
jsonGreetingEncoder =
  ApiResponseEncoder
    { apiResponseEncoderContentType = jsonContentType,
      apiResponseEncoderEncode = ApiEncodedResponseBytes . LazyByteString.toStrict . Aeson.encode . encodeGreetingResponse
    }

customGreetingEncoder :: ApiResponseEncoder GreetingResponse
customGreetingEncoder =
  ApiResponseEncoder
    { apiResponseEncoderContentType = apiContentType greetingMediaType,
      apiResponseEncoderEncode = ApiEncodedResponseBytes . TextEncoding.encodeUtf8 . ("GREETING " <>) . greetingText
    }

-- | @text\/x-greeting@ is a fixed, compile-time-valid bare media type
-- literal (one non-empty @type\/subtype@ pair), so 'apiMediaType' cannot
-- actually reject it here; the alternative is unreachable.
greetingMediaType :: ApiMediaType
greetingMediaType = requireApiMediaType "text/x-greeting"

readGreetingEndpoint :: ApiRouteEndpoint () () domainFailure GreetingResponse
readGreetingEndpoint =
  apiRouteEndpointAtNeverFailing
    ApiGet
    (at "/api/greeting")
    noRequestFields
    ApiNoRequestBody
    greetingEncoders
    (\_endpointRequest -> pure (apiResponse (greetingFor "World")))

submitGreetingEndpoint :: ApiRouteEndpoint () GreetingRequest domainFailure GreetingResponse
submitGreetingEndpoint =
  apiRouteEndpointAtNeverFailing
    ApiPost
    (at "/api/greeting")
    noRequestFields
    (ApiBufferedRequestBody RejectMissingContentType maxGreetingBodyBytes [greetingRequestBodyDecoder])
    greetingEncoders
    (pure . apiResponse . greetingFor . requestedName . apiEndpointRequestBody)

maxGreetingBodyBytes :: Int
maxGreetingBodyBytes = 16 * 1024

uploadAvatarEndpoint :: ApiRouteEndpoint () (ApiMultipartRequest InMemoryUpload) AvatarUploadFailure Text
uploadAvatarEndpoint =
  apiRouteEndpointAt
    ApiPost
    (at "/api/avatar")
    noRequestFields
    (ApiMultipartRequestBody inMemoryMultipartStorage defaultMultipartLimits)
    (textResponseEncoder :| [])
    handleAvatarUpload
    avatarUploadFailureResponse

data AvatarUploadFailure = InvalidAvatarUpload

avatarUploadFailureResponse :: AvatarUploadFailure -> ApiResponse Text
avatarUploadFailureResponse InvalidAvatarUpload = apiResponse "invalid multipart body"

-- | Counts parts and deliberately discards every upload: this compiled
-- demonstration never promotes a file, matching the AD storage-ownership
-- discipline (an application that wants to keep an upload must promote it
-- explicitly through its chosen adapter instead).
handleAvatarUpload :: ApiEndpointRequest () (ApiMultipartRequest InMemoryUpload) -> IO (Either AvatarUploadFailure (ApiResponse Text))
handleAvatarUpload endpointRequest = do
  partCountReference <- IORef.newIORef (0 :: Int)
  result <-
    withApiMultipartRequest (apiEndpointRequestBody endpointRequest) $ \_part -> do
      IORef.modifyIORef' partCountReference (+ 1)
      pure (Right ())
  partCount <- IORef.readIORef partCountReference
  pure $ case result of
    Right () -> Right (apiResponse (Text.pack (show partCount) <> " part(s) received"))
    Left _consumeError -> Left InvalidAvatarUpload
