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

responsePolicyHeaders :: RequestPolicyConfig -> Wai.Request -> Maybe Document.RuntimeNonce -> Http.ResponseHeaders
responsePolicyHeaders = requestPolicyResponseHeadersWithNonce

responseRuntimeNonce :: Response route context -> IO (Maybe Document.RuntimeNonce)
responseRuntimeNonce response =
  case response of
    PageResponse _ -> Just <$> Document.generateRuntimeNonce
    PageResponseWithMetadata _ _ -> Just <$> Document.generateRuntimeNonce
    BodyResponse _ -> pure Nothing
    RedirectResponse _ _ -> pure Nothing
    ClientActionBodyResponse _ -> pure Nothing
    EventStreamResponse _ _ -> pure Nothing
    ProtocolResponseResult _ -> pure Nothing

redirectResponse :: Http.Status -> Text -> Response route context
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
    PageResponse page -> Http.statusCode (if isNotFoundPage webApplication page then Http.status404 else Http.status200)
    PageResponseWithMetadata responseBodyValue _ -> Http.statusCode (responseStatus responseBodyValue)
    BodyResponse responseBodyValue -> Http.statusCode (responseStatus responseBodyValue)
    RedirectResponse responseBodyValue _ -> Http.statusCode (responseStatus responseBodyValue)
    ClientActionBodyResponse actionResponse -> Http.statusCode (clientActionStatus actionResponse)
    EventStreamResponse responseBodyValue _ -> Http.statusCode (responseStatus responseBodyValue)
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
  Maybe Document.RuntimeNonce ->
  Application route action context ->
  Response route context ->
  Wai.Response
toWaiResponse additionalHeaders maybeRuntimeNonce webApplication response =
  case response of
    PageResponse page ->
      renderPageResponse
        (if isNotFoundPage webApplication page then Http.status404 else Http.status200)
        page
    PageResponseWithMetadata pageResponseBodyValue page ->
      renderPageResponse (responseStatus pageResponseBodyValue) page
    BodyResponse responseBodyValue -> toWaiBodyResponse additionalHeaders responseBodyValue
    RedirectResponse responseBodyValue location -> toWaiBodyResponse (additionalHeaders <> [(Http.hLocation, TextEncoding.encodeUtf8 location)]) responseBodyValue
    ClientActionBodyResponse actionResponse -> toWaiBodyResponse (additionalHeaders <> clientActionHeaders actionResponse) (clientActionResponseBody actionResponse)
    EventStreamResponse responseBodyValue eventSource -> toWaiEventStreamResponse additionalHeaders responseBodyValue eventSource
    ProtocolResponseResult protocolResponse -> toWaiProtocolResponse additionalHeaders protocolResponse
  where
    renderPageResponse status page =
      case maybeRuntimeNonce of
        Just runtimeNonce ->
          Wai.responseLBS
            status
            (pageResponseHeaders additionalHeaders runtimeNonce)
            (LazyByteString.fromStrict (TextEncoding.encodeUtf8 (Document.renderDocumentWithNonce runtimeNonce (pageShell webApplication page))))
        Nothing ->
          Wai.responseLBS
            Http.internalServerError500
            [(Http.hContentType, TextEncoding.encodeUtf8 htmlContentType)]
            "A page response was missing its CSP nonce."

pageResponseHeaders :: Http.ResponseHeaders -> Document.RuntimeNonce -> Http.ResponseHeaders
pageResponseHeaders additionalHeaders runtimeNonce =
  additionalHeaders
    <> [ (Http.hContentType, TextEncoding.encodeUtf8 htmlContentType),
         ("Set-Cookie", TextEncoding.encodeUtf8 ("harch-csrf=" <> Document.runtimeNonceValue runtimeNonce <> "; Path=/; SameSite=Strict"))
       ]

toWaiBodyResponse :: Http.ResponseHeaders -> ResponseBody -> Wai.Response
toWaiBodyResponse additionalHeaders responseBodyValue =
  Wai.responseLBS
    (responseStatus responseBodyValue)
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
    (responseStatus responseBodyValue)
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
