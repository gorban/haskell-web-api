{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Typed application, request, response, and middleware contracts.
--
-- The framework facade re-exports this module. Request execution, WAI
-- rendering, and transport runtime implementation build on these acyclic types
-- in subsequent server-focused modules.
module HarchWeb.Server
  ( Application (..),
    ClientActionRequest (..),
    ClientActionResponse (..),
    MiddlewareResult (..),
    RegionPatch (..),
    RequestMiddleware (..),
    Response (..),
    ResponseBody (..),
    ResponseDiagnostics (..),
    ServerSentEvent (..),
    ServerSentEventSource (..),
    application,
    clientActionResponseBody,
    eventStreamResponse,
    isClientActionRequest,
    parseClientActionFields,
    redirectResponse,
    renderServerSentEvent,
    responseDiagnostics,
    responseKind,
    responseStatusCode,
    serverSentEventContentType,
    runRequestMiddlewarePipeline,
    serverSentEventSourceFromList,
    toWaiBodyResponse,
    toWaiResponse,
  )
where

import Data.Bifunctor (bimap)
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder qualified as ByteStringBuilder
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Foldable (for_)
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.Encoding.Error qualified as TextEncodingError
import HarchWeb.Document (Document, NavigationRuntime, Page)
import HarchWeb.Document qualified as Document
import HarchWeb.Observability qualified as Observability
import HarchWeb.Routing (RouteCodec (..), RouteRequest (..))
import HarchWeb.Security (RequestPolicyConfig)
import HarchWeb.StaticAssets (StaticAssetsConfig)
import Network.HTTP.Types qualified as Http
import Network.HTTP.Types.URI qualified as HttpUri
import Network.Wai qualified as Wai

data ResponseBody = ResponseBody
  { responseStatus :: Int,
    responseContentType :: Text,
    responseBody :: Text,
    responseObservabilityAttributes :: [Observability.ObservabilityAttribute],
    responseLogEntries :: [Text]
  }
  deriving (Eq, Show)

-- | One event in a server-sent event stream. Event names and identifiers are
-- rendered as single protocol fields; embedded line breaks are discarded so a
-- value cannot inject another SSE field. Payload data may contain line breaks
-- and is rendered as one @data:@ field per line.
data ServerSentEvent = ServerSentEvent
  { serverSentEventName :: Maybe Text,
    serverSentEventId :: Maybe Text,
    serverSentEventData :: Text
  }
  deriving (Eq, Show)

-- | A subscription supplies the next event, or 'Nothing' when the stream has
-- ended. A production source may block while it waits for data; a finite source
-- is equally useful for deterministic integration tests and one-shot updates.
newtype ServerSentEventSource = ServerSentEventSource
  { nextServerSentEvent :: IO (Maybe ServerSentEvent)
  }

data ResponseDiagnostics = ResponseDiagnostics
  { diagnosticObservabilityAttributes :: [Observability.ObservabilityAttribute],
    diagnosticLogEntries :: [Text]
  }

-- | A typed application-owned request middleware. Middleware runs after
-- framework redirect and CORS policy handling, and before routing or action
-- dispatch. Static assets remain public unless an app serves them as routes.
newtype RequestMiddleware context = RequestMiddleware
  { runRequestMiddleware :: Wai.Request -> context -> IO (MiddlewareResult context)
  }

-- | Middleware may carry an enriched request context onward or halt with a
-- framework response body. A halted response still receives framework
-- security headers, logging, and observability.
data MiddlewareResult context
  = ContinueMiddleware context
  | HaltMiddleware context ResponseBody
  deriving (Eq, Show)

-- | A same-origin form action captured before deferred behavior modules load.
-- Form fields preserve their authored order, including the successful submitter.
data ClientActionRequest context = ClientActionRequest
  { clientActionMethod :: Text,
    clientActionPath :: Text,
    clientActionFields :: [(Text, Text)],
    clientActionCsrfToken :: Maybe Text,
    clientActionContext :: context
  }
  deriving (Eq, Show)

-- | A named SSR region replacement returned by a client action. The replacement
-- must include the region element itself, preserving its id for later patches.
data RegionPatch = RegionPatch
  { regionPatchId :: Text,
    regionPatchHtml :: Text
  }
  deriving (Eq, Show)

data ClientActionResponse = ClientActionResponse
  { clientActionStatus :: Int,
    clientActionPatches :: [RegionPatch],
    clientActionFocusId :: Maybe Text,
    clientActionHeaders :: Http.ResponseHeaders,
    clientActionObservabilityAttributes :: [Observability.ObservabilityAttribute],
    clientActionLogEntries :: [Text]
  }
  deriving (Eq, Show)

data Response route context
  = PageResponse (Page route context)
  | PageResponseWithMetadata ResponseBody (Page route context)
  | BodyResponse ResponseBody
  | RedirectResponse ResponseBody Text
  | ClientActionBodyResponse ClientActionResponse
  | EventStreamResponse ResponseBody ServerSentEventSource

instance (Eq route, Eq context) => Eq (Response route context) where
  left == right =
    case (left, right) of
      (PageResponse leftPage, PageResponse rightPage) -> leftPage == rightPage
      (PageResponseWithMetadata leftBody leftPage, PageResponseWithMetadata rightBody rightPage) -> leftBody == rightBody && leftPage == rightPage
      (BodyResponse leftBody, BodyResponse rightBody) -> leftBody == rightBody
      (RedirectResponse leftBody leftLocation, RedirectResponse rightBody rightLocation) -> leftBody == rightBody && leftLocation == rightLocation
      (ClientActionBodyResponse leftAction, ClientActionBodyResponse rightAction) -> leftAction == rightAction
      (EventStreamResponse leftBody _, EventStreamResponse rightBody _) -> leftBody == rightBody
      _ -> False

instance (Show route, Show context) => Show (Response route context) where
  showsPrec precedence response =
    case response of
      PageResponse page -> showParen (precedence > 10) (showString "PageResponse " . showsPrec 11 page)
      PageResponseWithMetadata responseBodyValue page -> showParen (precedence > 10) (showString "PageResponseWithMetadata " . showsPrec 11 responseBodyValue . showChar ' ' . showsPrec 11 page)
      BodyResponse responseBodyValue -> showParen (precedence > 10) (showString "BodyResponse " . showsPrec 11 responseBodyValue)
      RedirectResponse responseBodyValue location -> showParen (precedence > 10) (showString "RedirectResponse " . showsPrec 11 responseBodyValue . showChar ' ' . shows location)
      ClientActionBodyResponse actionResponse -> showParen (precedence > 10) (showString "ClientActionBodyResponse " . showsPrec 11 actionResponse)
      EventStreamResponse responseBodyValue _ -> showParen (precedence > 10) (showString "EventStreamResponse " . showsPrec 11 responseBodyValue . showString " <event-source>")

data Application route context = Application
  { appName :: Text,
    defaultRequestContext :: context,
    requestContextFromRequest :: Wai.Request -> context -> context,
    applicationNavigationRuntime :: Maybe NavigationRuntime,
    applicationStaticAssets :: StaticAssetsConfig,
    applicationRequestPolicy :: RequestPolicyConfig,
    applicationRequestMiddleware :: [RequestMiddleware context],
    routeCodec :: RouteCodec route context,
    renderResponse :: RouteRequest route context -> IO (Response route context),
    handleClientAction :: ClientActionRequest context -> IO (Maybe ClientActionResponse),
    pageShell :: Page route context -> Document route,
    reportRequestObservability :: Observability.RequestObservability -> IO (),
    reportConnectionObservability :: Observability.ConnectionObservability -> IO (),
    reportApplicationLog :: Text -> IO ()
  }

application :: Application route context -> Application route context
application = id

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

eventStreamResponse :: ServerSentEventSource -> Response route context
eventStreamResponse =
  EventStreamResponse
    ResponseBody
      { responseStatus = 200,
        responseContentType = "text/event-stream; charset=utf-8",
        responseBody = Text.empty,
        responseObservabilityAttributes = [],
        responseLogEntries = []
      }

serverSentEventSourceFromList :: [ServerSentEvent] -> IO ServerSentEventSource
serverSentEventSourceFromList events = do
  eventsReference <- newIORef events
  pure $
    ServerSentEventSource $
      atomicModifyIORef' eventsReference $ \case
        [] -> ([], Nothing)
        event : remainingEvents -> (remainingEvents, Just event)

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

responseBodyDiagnostics :: ResponseBody -> ResponseDiagnostics
responseBodyDiagnostics responseBodyValue =
  ResponseDiagnostics
    { diagnosticObservabilityAttributes = responseObservabilityAttributes responseBodyValue,
      diagnosticLogEntries = responseLogEntries responseBodyValue
    }

responseStatusCode :: (Eq route) => Application route context -> Response route context -> Int
responseStatusCode webApplication response =
  case response of
    PageResponse page -> if isNotFoundPage webApplication page then 404 else 200
    PageResponseWithMetadata responseBodyValue _ -> responseStatus responseBodyValue
    BodyResponse responseBodyValue -> responseStatus responseBodyValue
    RedirectResponse responseBodyValue _ -> responseStatus responseBodyValue
    ClientActionBodyResponse actionResponse -> clientActionStatus actionResponse
    EventStreamResponse responseBodyValue _ -> responseStatus responseBodyValue

responseKind :: Response route context -> Observability.ResponseKind
responseKind response =
  case response of
    PageResponse _ -> Observability.PageResponseKind
    PageResponseWithMetadata _ _ -> Observability.PageResponseKind
    BodyResponse _ -> Observability.BodyResponseKind
    RedirectResponse _ _ -> Observability.BodyResponseKind
    ClientActionBodyResponse _ -> Observability.BodyResponseKind
    EventStreamResponse _ _ -> Observability.BodyResponseKind

-- | Run middleware in declaration order. The first middleware sees the
-- request first; a halt short-circuits the remaining middleware.
runRequestMiddlewarePipeline :: [RequestMiddleware context] -> Wai.Request -> context -> IO (MiddlewareResult context)
runRequestMiddlewarePipeline middleware request = go middleware
  where
    go [] requestContext = pure (ContinueMiddleware requestContext)
    go (RequestMiddleware runMiddleware : remainingMiddleware) requestContext = do
      result <- runMiddleware request requestContext
      case result of
        ContinueMiddleware nextRequestContext -> go remainingMiddleware nextRequestContext
        HaltMiddleware haltedRequestContext responseBodyValue -> pure (HaltMiddleware haltedRequestContext responseBodyValue)

renderServerSentEvent :: ServerSentEvent -> Text
renderServerSentEvent ServerSentEvent {serverSentEventName, serverSentEventId, serverSentEventData} =
  Text.concat
    ( maybeToList (renderSseField "event" <$> serverSentEventName)
        <> maybeToList (renderSseField "id" <$> serverSentEventId)
        <> map (renderSseDataLine . Text.filter (`notElem` ['\r', '\n'])) (Text.splitOn "\n" serverSentEventData)
        <> ["\n"]
    )

renderSseField :: Text -> Text -> Text
renderSseField fieldName fieldValue = fieldName <> ": " <> Text.filter (`notElem` ['\r', '\n']) fieldValue <> "\n"

renderSseDataLine :: Text -> Text
renderSseDataLine line = "data: " <> line <> "\n"

toWaiResponse :: (Eq route) => Http.ResponseHeaders -> Document.RuntimeNonce -> Application route context -> Response route context -> Wai.Response
toWaiResponse additionalHeaders runtimeNonce webApplication response =
  case response of
    PageResponse page ->
      Wai.responseLBS
        (if isNotFoundPage webApplication page then Http.status404 else Http.status200)
        (additionalHeaders <> [(Http.hContentType, TextEncoding.encodeUtf8 htmlContentType)])
        (LazyByteString.fromStrict (TextEncoding.encodeUtf8 (Document.renderDocumentWithNonce runtimeNonce (pageShell webApplication page))))
    PageResponseWithMetadata pageResponseBodyValue page ->
      let !pageStatusMessage = ByteString.empty
          !pageStatusMessageLength = ByteString.length pageStatusMessage
          !pageStatus = pageStatusMessageLength `seq` Http.Status (responseStatus pageResponseBodyValue) pageStatusMessage
       in Wai.responseLBS
            pageStatus
            (additionalHeaders <> [(Http.hContentType, TextEncoding.encodeUtf8 htmlContentType)])
            (LazyByteString.fromStrict (TextEncoding.encodeUtf8 (Document.renderDocumentWithNonce runtimeNonce (pageShell webApplication page))))
    BodyResponse responseBodyValue -> toWaiBodyResponse additionalHeaders responseBodyValue
    RedirectResponse responseBodyValue location -> toWaiBodyResponse (additionalHeaders <> [(Http.hLocation, TextEncoding.encodeUtf8 location)]) responseBodyValue
    ClientActionBodyResponse actionResponse -> toWaiBodyResponse (additionalHeaders <> clientActionHeaders actionResponse) (clientActionResponseBody actionResponse)
    EventStreamResponse responseBodyValue eventSource -> toWaiEventStreamResponse additionalHeaders responseBodyValue eventSource

toWaiBodyResponse :: Http.ResponseHeaders -> ResponseBody -> Wai.Response
toWaiBodyResponse additionalHeaders responseBodyValue =
  Wai.responseLBS
    (Http.mkStatus (responseStatus responseBodyValue) mempty)
    (additionalHeaders <> [(Http.hContentType, TextEncoding.encodeUtf8 (responseContentType responseBodyValue))])
    (LazyByteString.fromStrict (TextEncoding.encodeUtf8 (responseBody responseBodyValue)))

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

isClientActionRequest :: Wai.Request -> Bool
isClientActionRequest request = lookup "X-Harch-Action" (Wai.requestHeaders request) == Just "1"

parseClientActionFields :: LazyByteString.ByteString -> [(Text, Text)]
parseClientActionFields requestBody =
  map (bimap decodeActionField (maybe "" decodeActionField)) (HttpUri.parseQuery (LazyByteString.toStrict requestBody))

decodeActionField :: ByteString.ByteString -> Text
decodeActionField = TextEncoding.decodeUtf8With TextEncodingError.lenientDecode

clientActionResponseBody :: ClientActionResponse -> ResponseBody
clientActionResponseBody actionResponse =
  ResponseBody
    { responseStatus = clientActionStatus actionResponse,
      responseContentType = "application/json; charset=utf-8",
      responseBody = renderClientActionResponse actionResponse,
      responseObservabilityAttributes = clientActionObservabilityAttributes actionResponse,
      responseLogEntries = clientActionLogEntries actionResponse
    }

renderClientActionResponse :: ClientActionResponse -> Text
renderClientActionResponse actionResponse =
  "{\"patches\":["
    <> Text.intercalate "," (map renderPatch (clientActionPatches actionResponse))
    <> "],\"focusId\":"
    <> maybe "null" jsonString (clientActionFocusId actionResponse)
    <> "}"
  where
    renderPatch RegionPatch {regionPatchId, regionPatchHtml} =
      "{\"id\":" <> jsonString regionPatchId <> ",\"html\":" <> jsonString regionPatchHtml <> "}"

jsonString :: Text -> Text
jsonString textValue = "\"" <> Text.concatMap escapeJsonCharacter textValue <> "\""

escapeJsonCharacter :: Char -> Text
escapeJsonCharacter character =
  case character of
    '"' -> "\\\""
    '\\' -> "\\\\"
    '\b' -> "\\b"
    '\f' -> "\\f"
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    _ -> Text.singleton character

isNotFoundPage :: (Eq route) => Application route context -> Page route context -> Bool
isNotFoundPage webApplication page =
  let pageRequestContext = Document.pageContext page
   in pageRequestContext `seq`
        Document.pageRoute page == requestRoute (notFoundRequest (routeCodec webApplication) pageRequestContext)

htmlContentType :: Text
htmlContentType = "text/html; charset=utf-8"

serverSentEventContentType :: Text
serverSentEventContentType = "text/event-stream; charset=utf-8"
