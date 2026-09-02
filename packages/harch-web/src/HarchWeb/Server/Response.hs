{-# LANGUAGE DuplicateRecordFields #-}

-- | Private typed request and response contracts for the WAI server pipeline.
module HarchWeb.Server.Response
  ( ClientActionDecodeResult (..),
    ClientActionIdempotencyKey,
    ClientActionPayload (..),
    ClientActionRequest (..),
    ClientActionResponse (..),
    MiddlewareResult (..),
    RegionPatch,
    RequestMiddleware (..),
    Response (..),
    mapResponsePage,
    ResponseBody (..),
    ResponseDiagnostics (..),
    ProtocolResponse (..),
    ProtocolResponseBody (..),
    ServerSentEvent (..),
    ServerSentEventSource (..),
  )
where

import Data.ByteString (ByteString)
import Data.Text (Text)
import HarchWeb.Action
  ( ClientActionDecodeResult (..),
    ClientActionIdempotencyKey,
    ClientActionPayload (..),
  )
import HarchWeb.Database (DatabaseOperation)
import HarchWeb.Document (Page)
import HarchWeb.Markup (ElementId, RegionPatch)
import HarchWeb.Observability qualified as Observability
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai

-- | A response carries 'Http.Status' rather than a bare numeric code so an
-- application cannot construct an invalid HTTP status and WAI retains the
-- standard reason phrase.
--
-- Decision record (DV, 2026-08-17): extend the existing response boundary
-- with the 'http-types' status type instead of adding a framework wrapper.
-- It is the established representation at the WAI boundary; numeric status
-- codes are extracted only for observability attributes.
--
-- Decision record (BW, 2026-08-19): database work extends this existing
-- response boundary as typed 'DatabaseOperation' values. It is projected to
-- OTLP child spans only by the exporter; generic attributes remain generic
-- attributes and are never reparsed as a second, order-dependent protocol.
data ResponseBody = ResponseBody
  { responseStatus :: Http.Status,
    responseContentType :: Text,
    responseBody :: Text,
    responseObservabilityAttributes :: [Observability.ObservabilityAttribute],
    responseLogEntries :: [Text],
    responseDatabaseOperations :: [DatabaseOperation]
  }
  deriving (Eq, Show)

-- | An HTTP response whose payload is already protocol bytes rather than
-- document text. This is the shared server-boundary primitive for APIs,
-- downloads, and application-defined representations: it still travels
-- through response security headers, diagnostics, and observability.
--
-- Decision record (AC, 2026-08-12): this adds only the response half of the
-- shared endpoint boundary. It does not introduce another dispatcher; the
-- method-aware 'HarchWeb.Routing.RouteCodec'/'HarchWeb.Site.RouteDefinition'
-- boundary remains responsible for one application-wide path/method owner.
data ProtocolResponse = ProtocolResponse
  { protocolResponseStatus :: Http.Status,
    protocolResponseHeaders :: Http.ResponseHeaders,
    protocolResponseBody :: ProtocolResponseBody,
    protocolResponseObservabilityAttributes :: [Observability.ObservabilityAttribute],
    protocolResponseLogEntries :: [Text],
    protocolResponseDatabaseOperations :: [DatabaseOperation]
  }

-- | A protocol response may be strict bytes, a one-shot WAI stream, or a
-- framework-owned WAI response. The latter preserves a file-backed response
-- selected by the static-asset interpreter (including its conditional/range
-- semantics) while the outer renderer still attaches root policy headers.
data ProtocolResponseBody
  = ProtocolResponseBytes ByteString
  | ProtocolResponseStream Wai.StreamingBody
  | ProtocolResponseWai Wai.Response

instance Eq ProtocolResponse where
  left == right =
    protocolResponseStatus left == protocolResponseStatus right
      && protocolResponseHeaders left == protocolResponseHeaders right
      && equalProtocolBodies (protocolResponseBody left) (protocolResponseBody right)
      && protocolResponseObservabilityAttributes left == protocolResponseObservabilityAttributes right
      && protocolResponseLogEntries left == protocolResponseLogEntries right
      && protocolResponseDatabaseOperations left == protocolResponseDatabaseOperations right

equalProtocolBodies :: ProtocolResponseBody -> ProtocolResponseBody -> Bool
equalProtocolBodies left right =
  case (left, right) of
    (ProtocolResponseBytes leftBytes, ProtocolResponseBytes rightBytes) -> leftBytes == rightBytes
    (ProtocolResponseStream _, ProtocolResponseStream _) -> True
    (ProtocolResponseWai _, ProtocolResponseWai _) -> True
    _ -> False

instance Show ProtocolResponse where
  showsPrec precedence response =
    showParen (precedence > 10) $
      showString
        ( "ProtocolResponse "
            <> showsPrec 11 (protocolResponseStatus response) ""
            <> " "
            <> shows (protocolResponseHeaders response) ""
            <> " "
            <> shows (protocolResponseBodySummary (protocolResponseBody response)) ""
        )

protocolResponseBodySummary :: ProtocolResponseBody -> String
protocolResponseBodySummary responseBodyValue =
  case responseBodyValue of
    ProtocolResponseBytes bytes -> "ProtocolResponseBytes " <> show bytes
    ProtocolResponseStream _ -> "ProtocolResponseStream <stream>"
    ProtocolResponseWai _ -> "ProtocolResponseWai <framework-response>"

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
    diagnosticLogEntries :: [Text],
    diagnosticDatabaseOperations :: [DatabaseOperation]
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

-- | The application action after its codec has consumed the transport payload.
-- Handlers receive only this typed action, an optional retry identity, and
-- their request context. A non-'Nothing' identity is supplied only by an
-- explicitly idempotent control; the handler is its durable deduplication
-- boundary and must not log the key.
data ClientActionRequest action context = ClientActionRequest
  { clientAction :: action,
    clientActionRequestIdempotencyKey :: Maybe ClientActionIdempotencyKey,
    clientActionContext :: context
  }
  deriving (Eq, Show)

-- | Client-action status shares the ordinary response boundary's exact HTTP
-- status representation, including its reason phrase.
--
-- Decision (AHI-7, 2026-08-31): focus extends this existing response with
-- 'ElementId' rather than an application string or a parallel focus command.
-- The JSON encoder is the sole boundary that erases the ID to text, making a
-- mismatch with typed field renderers harder to author.
data ClientActionResponse = ClientActionResponse
  { clientActionStatus :: Http.Status,
    clientActionPatches :: [RegionPatch],
    clientActionFocusId :: Maybe ElementId,
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
  | ProtocolResponseResult ProtocolResponse

-- | Change the route and context carried by page responses while preserving
-- every response that has no page.  Route composition uses this one total
-- operation instead of each adapter repeating a partial-looking case split.
mapResponsePage :: (Page route context -> Page mappedRoute mappedContext) -> Response route context -> Response mappedRoute mappedContext
mapResponsePage mapPage response =
  case response of
    PageResponse page -> PageResponse (mapPage page)
    PageResponseWithMetadata responseBodyValue page -> PageResponseWithMetadata responseBodyValue (mapPage page)
    BodyResponse responseBodyValue -> BodyResponse responseBodyValue
    RedirectResponse responseBodyValue location -> RedirectResponse responseBodyValue location
    ClientActionBodyResponse actionResponse -> ClientActionBodyResponse actionResponse
    EventStreamResponse responseBodyValue source -> EventStreamResponse responseBodyValue source
    ProtocolResponseResult protocolResponse -> ProtocolResponseResult protocolResponse

instance (Eq route, Eq context) => Eq (Response route context) where
  left == right =
    case (left, right) of
      (PageResponse leftPage, PageResponse rightPage) -> leftPage == rightPage
      (PageResponseWithMetadata leftBody leftPage, PageResponseWithMetadata rightBody rightPage) -> leftBody == rightBody && leftPage == rightPage
      (BodyResponse leftBody, BodyResponse rightBody) -> leftBody == rightBody
      (RedirectResponse leftBody leftLocation, RedirectResponse rightBody rightLocation) -> leftBody == rightBody && leftLocation == rightLocation
      (ClientActionBodyResponse leftAction, ClientActionBodyResponse rightAction) -> leftAction == rightAction
      (EventStreamResponse leftBody _, EventStreamResponse rightBody _) -> leftBody == rightBody
      (ProtocolResponseResult leftResponse, ProtocolResponseResult rightResponse) -> leftResponse == rightResponse
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
      ProtocolResponseResult protocolResponse -> showParen (precedence > 10) (showString "ProtocolResponseResult " . showsPrec 11 protocolResponse)
