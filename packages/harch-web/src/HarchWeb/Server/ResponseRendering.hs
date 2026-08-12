{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Response finalization and WAI rendering for typed applications.
module HarchWeb.Server.ResponseRendering
  ( applyResponseHeaders,
    redirectResponse,
    responseDiagnostics,
    responseKind,
    responsePolicyHeaders,
    responseRuntimeNonce,
    responseStatusCode,
    toWaiResponse,
    toWaiBodyResponse,
  )
where

import Data.ByteString qualified as ByteString
import Data.ByteString.Builder qualified as ByteStringBuilder
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Foldable (for_)
import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Document (Page)
import HarchWeb.Document qualified as Document
import HarchWeb.Observability qualified as Observability
import HarchWeb.Routing (RouteCodec (..), RouteRequest (..))
import HarchWeb.Security (RequestPolicyConfig, requestPolicyResponseHeadersWithNonce)
import HarchWeb.Server.Application (Application (..))
import HarchWeb.Server.ClientAction (clientActionResponseBody)
import HarchWeb.Server.Response
import HarchWeb.Server.Sse (renderServerSentEvent)
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai

applyResponseHeaders :: Http.ResponseHeaders -> Wai.Response -> Wai.Response
applyResponseHeaders additionalHeaders =
  Wai.mapResponseHeaders (additionalHeaders <>)

responsePolicyHeaders :: RequestPolicyConfig -> Wai.Request -> Document.RuntimeNonce -> Response route context -> Http.ResponseHeaders
responsePolicyHeaders requestPolicyConfig request runtimeNonce response =
  requestPolicyResponseHeadersWithNonce
    requestPolicyConfig
    request
    ( case response of
        PageResponse _ -> Just runtimeNonce
        PageResponseWithMetadata _ _ -> Just runtimeNonce
        BodyResponse _ -> Nothing
        RedirectResponse _ _ -> Nothing
        ClientActionBodyResponse _ -> Nothing
        EventStreamResponse _ _ -> Nothing
        ProtocolResponseResult _ -> Nothing
    )

responseRuntimeNonce :: Response route context -> IO Document.RuntimeNonce
responseRuntimeNonce response =
  case response of
    PageResponse _ -> Document.generateRuntimeNonce
    PageResponseWithMetadata _ _ -> Document.generateRuntimeNonce
    BodyResponse _ -> pure $! Document.RuntimeNonce ""
    RedirectResponse _ _ -> pure $! Document.RuntimeNonce ""
    ClientActionBodyResponse _ -> pure $! Document.RuntimeNonce ""
    EventStreamResponse _ _ -> pure $! Document.RuntimeNonce ""
    ProtocolResponseResult _ -> pure $! Document.RuntimeNonce ""

redirectResponse :: Int -> Text -> Response route context
redirectResponse status =
  RedirectResponse
    ResponseBody
      { responseStatus = status,
        responseContentType = "text/plain; charset=utf-8",
        responseBody = "",
        responseObservabilityAttributes = [],
        responseLogEntries = []
      }

responseDiagnostics :: Response route context -> ResponseDiagnostics
responseDiagnostics response =
  case response of
    PageResponse _ -> ResponseDiagnostics [] []
    PageResponseWithMetadata responseBodyValue _ -> responseBodyDiagnostics responseBodyValue
    BodyResponse responseBodyValue -> responseBodyDiagnostics responseBodyValue
    RedirectResponse responseBodyValue _ -> responseBodyDiagnostics responseBodyValue
    ClientActionBodyResponse actionResponse ->
      ResponseDiagnostics
        (clientActionObservabilityAttributes actionResponse)
        (clientActionLogEntries actionResponse)
    EventStreamResponse responseBodyValue _ -> responseBodyDiagnostics responseBodyValue
    ProtocolResponseResult protocolResponse ->
      ResponseDiagnostics
        (protocolResponseObservabilityAttributes protocolResponse)
        (protocolResponseLogEntries protocolResponse)

responseBodyDiagnostics :: ResponseBody -> ResponseDiagnostics
responseBodyDiagnostics responseBodyValue =
  ResponseDiagnostics
    { diagnosticObservabilityAttributes = responseObservabilityAttributes responseBodyValue,
      diagnosticLogEntries = responseLogEntries responseBodyValue
    }

responseStatusCode :: (Eq route) => Application route action context -> Response route context -> Int
responseStatusCode webApplication response =
  case response of
    PageResponse page -> if isNotFoundPage webApplication page then 404 else 200
    PageResponseWithMetadata responseBodyValue _ -> responseStatus responseBodyValue
    BodyResponse responseBodyValue -> responseStatus responseBodyValue
    RedirectResponse responseBodyValue _ -> responseStatus responseBodyValue
    ClientActionBodyResponse actionResponse -> clientActionStatus actionResponse
    EventStreamResponse responseBodyValue _ -> responseStatus responseBodyValue
    ProtocolResponseResult protocolResponse -> Http.statusCode (protocolResponseStatus protocolResponse)

responseKind :: Response route context -> Observability.ResponseKind
responseKind response =
  case response of
    PageResponse _ -> Observability.PageResponseKind
    PageResponseWithMetadata _ _ -> Observability.PageResponseKind
    BodyResponse _ -> Observability.BodyResponseKind
    RedirectResponse _ _ -> Observability.BodyResponseKind
    ClientActionBodyResponse _ -> Observability.BodyResponseKind
    EventStreamResponse _ _ -> Observability.BodyResponseKind
    ProtocolResponseResult _ -> Observability.BodyResponseKind

toWaiResponse ::
  (Eq route) =>
  Http.ResponseHeaders ->
  Document.RuntimeNonce ->
  Application route action context ->
  Response route context ->
  Wai.Response
toWaiResponse additionalHeaders runtimeNonce webApplication response =
  case response of
    PageResponse page ->
      Wai.responseLBS
        (if isNotFoundPage webApplication page then Http.status404 else Http.status200)
        (pageResponseHeaders additionalHeaders runtimeNonce)
        (LazyByteString.fromStrict (TextEncoding.encodeUtf8 (Document.renderDocumentWithNonce runtimeNonce (pageShell webApplication page))))
    PageResponseWithMetadata pageResponseBodyValue page ->
      let !pageStatusMessage = ByteString.empty
          !pageStatusMessageLength = ByteString.length pageStatusMessage
          !pageStatus = pageStatusMessageLength `seq` Http.Status (responseStatus pageResponseBodyValue) pageStatusMessage
       in Wai.responseLBS
            pageStatus
            (pageResponseHeaders additionalHeaders runtimeNonce)
            (LazyByteString.fromStrict (TextEncoding.encodeUtf8 (Document.renderDocumentWithNonce runtimeNonce (pageShell webApplication page))))
    BodyResponse responseBodyValue -> toWaiBodyResponse additionalHeaders responseBodyValue
    RedirectResponse responseBodyValue location -> toWaiBodyResponse (additionalHeaders <> [(Http.hLocation, TextEncoding.encodeUtf8 location)]) responseBodyValue
    ClientActionBodyResponse actionResponse -> toWaiBodyResponse (additionalHeaders <> clientActionHeaders actionResponse) (clientActionResponseBody actionResponse)
    EventStreamResponse responseBodyValue eventSource -> toWaiEventStreamResponse additionalHeaders responseBodyValue eventSource
    ProtocolResponseResult protocolResponse -> toWaiProtocolResponse additionalHeaders protocolResponse

pageResponseHeaders :: Http.ResponseHeaders -> Document.RuntimeNonce -> Http.ResponseHeaders
pageResponseHeaders additionalHeaders runtimeNonce =
  additionalHeaders
    <> [ (Http.hContentType, TextEncoding.encodeUtf8 htmlContentType),
         ("Set-Cookie", TextEncoding.encodeUtf8 ("harch-csrf=" <> Document.runtimeNonceValue runtimeNonce <> "; Path=/; SameSite=Strict"))
       ]

toWaiBodyResponse :: Http.ResponseHeaders -> ResponseBody -> Wai.Response
toWaiBodyResponse additionalHeaders responseBodyValue =
  Wai.responseLBS
    (Http.mkStatus (responseStatus responseBodyValue) mempty)
    (additionalHeaders <> [(Http.hContentType, TextEncoding.encodeUtf8 (responseContentType responseBodyValue))])
    (LazyByteString.fromStrict (TextEncoding.encodeUtf8 (responseBody responseBodyValue)))

toWaiProtocolResponse :: Http.ResponseHeaders -> ProtocolResponse -> Wai.Response
toWaiProtocolResponse additionalHeaders protocolResponse =
  case protocolResponseBody protocolResponse of
    ProtocolResponseBytes bodyBytes ->
      Wai.responseLBS
        (protocolResponseStatus protocolResponse)
        (additionalHeaders <> protocolResponseHeaders protocolResponse)
        (LazyByteString.fromStrict bodyBytes)
    ProtocolResponseStream streamBody ->
      Wai.responseStream
        (protocolResponseStatus protocolResponse)
        (additionalHeaders <> protocolResponseHeaders protocolResponse)
        streamBody

toWaiEventStreamResponse :: Http.ResponseHeaders -> ResponseBody -> ServerSentEventSource -> Wai.Response
toWaiEventStreamResponse additionalHeaders responseBodyValue eventSource =
  Wai.responseStream
    (Http.mkStatus (responseStatus responseBodyValue) mempty)
    ( additionalHeaders
        <> [ (Http.hContentType, TextEncoding.encodeUtf8 (responseContentType responseBodyValue)),
             ("Cache-Control", "no-cache"),
             ("X-Accel-Buffering", "no")
           ]
    )
    streamEvents
  where
    streamEvents write flush = do
      maybeEvent <- nextServerSentEvent eventSource
      for_ maybeEvent $ \event -> do
        write (ByteStringBuilder.byteString (TextEncoding.encodeUtf8 (renderServerSentEvent event)))
        flush
        streamEvents write flush

isNotFoundPage :: (Eq route) => Application route action context -> Page route context -> Bool
isNotFoundPage webApplication page =
  let pageRequestContext = Document.pageContext page
   in pageRequestContext `seq`
        Document.pageRoute page == requestRoute (notFoundRequest (routeCodec webApplication) pageRequestContext)

htmlContentType :: Text
htmlContentType = "text/html; charset=utf-8"
