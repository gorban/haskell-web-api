{-# LANGUAGE DuplicateRecordFields #-}

-- | Private typed request and response contracts for the WAI server pipeline.
module HarchWeb.Server.Response
  ( ActionNavigation (..),
    ClientActionDecodeResult (..),
    ClientActionIdempotencyKey,
    ClientActionPayload (..),
    ClientActionRequest (..),
    ClientActionResponse (..),
    HistoryMode (..),
    MiddlewareResult (..),
    RegionPatch,
    RequestMiddleware (..),
    PageResult (..),
    mapPageResult,
    NonPageResponse (..),
    nonPageResponse,
    mapClientActionResponse,
    mapNonPageResponse,
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
import HarchWeb.Csrf (PageSecurity, samePageSecurity)
import HarchWeb.Database (DatabaseOperation)
import HarchWeb.Document (Page)
import HarchWeb.Markup (ElementId, RegionPatch)
import HarchWeb.Observability qualified as Observability
import HarchWeb.Routing (RouteRequest)
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

-- | History behavior for an action's typed internal destination. Rendering
-- through the application's one root route codec is the only conversion to a
-- browser URL.
data HistoryMode
  = PushHistory
  | ReplaceHistory
  deriving (Eq, Show)

-- | A client action either remains on its current document or navigates to a
-- typed internal route. Raw URL text is intentionally absent; an external
-- redirect needs a separate, conspicuously named validated capability.
data ActionNavigation route context
  = StayOnCurrentRoute
  | NavigateInternal HistoryMode (RouteRequest route context)
  deriving (Eq, Show)

-- | Client-action status shares the ordinary response boundary's exact HTTP
-- status representation, including its reason phrase.
--
-- Decision (AHI-7, 2026-08-31): focus extends this existing response with
-- 'ElementId' rather than an application string or a parallel focus command.
-- The JSON encoder is the sole boundary that erases the ID to text, making a
-- mismatch with typed field renderers harder to author.
--
-- Decision (AHI-4C, 2026-09-03): action navigation extends this same response
-- with a typed 'RouteRequest', rather than a URL callback or raw JSON URL.
-- The final encoder has the root 'RouteCodec', and mounted modules map their
-- child destination while retaining local patches and diagnostics. This keeps
-- routing first-class without giving actions a second URL authority.
data ClientActionResponse route context = ClientActionResponse
  { clientActionStatus :: Http.Status,
    clientActionPatches :: [RegionPatch],
    clientActionFocusId :: Maybe ElementId,
    clientActionNavigation :: ActionNavigation route context,
    clientActionHeaders :: Http.ResponseHeaders,
    clientActionObservabilityAttributes :: [Observability.ObservabilityAttribute],
    clientActionLogEntries :: [Text]
  }
  deriving (Eq, Show)

-- | Map only an action response's typed destination while adapting a mounted
-- child module. Patches, focus, headers, and diagnostics retain their local
-- meaning; the parent remains the sole owner of its route algebra.
mapClientActionResponse ::
  (RouteRequest route context -> RouteRequest mappedRoute mappedContext) ->
  ClientActionResponse route context ->
  ClientActionResponse mappedRoute mappedContext
mapClientActionResponse mapRoute actionResponse =
  actionResponse
    { clientActionNavigation =
        case clientActionNavigation actionResponse of
          StayOnCurrentRoute -> StayOnCurrentRoute
          NavigateInternal historyMode routeRequest -> NavigateInternal historyMode (mapRoute routeRequest)
    }

-- | The outcome of an SSR page handler before request execution attaches the
-- one pre-rendered 'PageSecurity' value.  Keeping this distinct from
-- 'Response' makes it impossible for a page handler to invent a different
-- nonce/CSRF pair after it has rendered its markup, while retaining the
-- existing status, diagnostics, and database-operation metadata surface.
data PageResult route context
  = RenderedPage (Page route context)
  | RenderedPageWithMetadata ResponseBody (Page route context)
  deriving (Eq, Show)

-- | Adapt only the page carried by a page-handler outcome.  Composition
-- adapters use this closed total fold before the renderer attaches page
-- security, rather than manufacturing a provisional 'Response'.
mapPageResult :: (Page route context -> Page mappedRoute mappedContext) -> PageResult route context -> PageResult mappedRoute mappedContext
mapPageResult mapPage pageResult =
  case pageResult of
    RenderedPage page -> RenderedPage (mapPage page)
    RenderedPageWithMetadata responseBodyValue page -> RenderedPageWithMetadata responseBodyValue (mapPage page)

-- | Every response form which cannot carry an SSR page. Route protocol
-- handlers and post-match guards use this narrower result, leaving 'Site' as
-- the sole interpreter which can combine a page-handler result with freshly
-- prepared 'PageSecurity'. This extends the existing response boundary with a
-- closed capability subset; it is not another request dispatcher.
--
-- AHI-4C relies on this distinction to prevent an API, asset, SSE, or guard
-- from manufacturing a page response outside the pre-render security path.
data NonPageResponse route context
  = NonPageBodyResponse ResponseBody
  | NonPageRedirectResponse ResponseBody Text
  | -- | An application-owned redirect remains typed until the root renderer
    -- writes its @Location@ header. This is the native counterpart of
    -- 'NavigateInternal', not a second URL-routing mechanism.
    NonPageInternalRedirectResponse ResponseBody (RouteRequest route context)
  | -- | A typed internal redirect may carry application response headers such
    -- as an issued host-only session cookie. The final renderer retains sole
    -- ownership of @Location@: any supplied @Location@ header is discarded.
    NonPageInternalRedirectResponseWithHeaders ResponseBody Http.ResponseHeaders (RouteRequest route context)
  | NonPageClientActionBodyResponse (ClientActionResponse route context)
  | NonPageEventStreamResponse ResponseBody ServerSentEventSource
  | NonPageProtocolResponse ProtocolResponse

instance (Eq route, Eq context) => Eq (NonPageResponse route context) where
  left == right =
    case (left, right) of
      (NonPageBodyResponse leftBody, NonPageBodyResponse rightBody) -> leftBody == rightBody
      (NonPageRedirectResponse leftBody leftLocation, NonPageRedirectResponse rightBody rightLocation) -> leftBody == rightBody && leftLocation == rightLocation
      (NonPageInternalRedirectResponse leftBody leftRequest, NonPageInternalRedirectResponse rightBody rightRequest) -> leftBody == rightBody && leftRequest == rightRequest
      (NonPageInternalRedirectResponseWithHeaders leftBody leftHeaders leftRequest, NonPageInternalRedirectResponseWithHeaders rightBody rightHeaders rightRequest) -> leftBody == rightBody && leftHeaders == rightHeaders && leftRequest == rightRequest
      (NonPageClientActionBodyResponse leftAction, NonPageClientActionBodyResponse rightAction) -> leftAction == rightAction
      (NonPageEventStreamResponse leftBody _, NonPageEventStreamResponse rightBody _) -> leftBody == rightBody
      (NonPageProtocolResponse leftResponse, NonPageProtocolResponse rightResponse) -> leftResponse == rightResponse
      _ -> False

instance (Show route, Show context) => Show (NonPageResponse route context) where
  showsPrec precedence response =
    case response of
      NonPageBodyResponse responseBodyValue -> showParen (precedence > 10) (showString "NonPageBodyResponse " . showsPrec 11 responseBodyValue)
      NonPageRedirectResponse responseBodyValue location -> showParen (precedence > 10) (showString "NonPageRedirectResponse " . showsPrec 11 responseBodyValue . showChar ' ' . shows location)
      NonPageInternalRedirectResponse responseBodyValue routeRequest -> showParen (precedence > 10) (showString "NonPageInternalRedirectResponse " . showsPrec 11 responseBodyValue . showChar ' ' . showsPrec 11 routeRequest)
      NonPageInternalRedirectResponseWithHeaders responseBodyValue headers routeRequest -> showParen (precedence > 10) (showString "NonPageInternalRedirectResponseWithHeaders " . showsPrec 11 responseBodyValue . showChar ' ' . shows headers . showChar ' ' . showsPrec 11 routeRequest)
      NonPageClientActionBodyResponse actionResponse -> showParen (precedence > 10) (showString "NonPageClientActionBodyResponse " . showsPrec 11 actionResponse)
      NonPageEventStreamResponse responseBodyValue _ -> showParen (precedence > 10) (showString "NonPageEventStreamResponse " . showsPrec 11 responseBodyValue . showString " <event-source>")
      NonPageProtocolResponse protocolResponse -> showParen (precedence > 10) (showString "NonPageProtocolResponse " . showsPrec 11 protocolResponse)

-- | Interpret the non-page subset through the existing final response
-- renderer. It is deliberately total because each non-page constructor has
-- exactly one established full-response equivalent.
nonPageResponse :: NonPageResponse route context -> Response route context
nonPageResponse response =
  case response of
    NonPageBodyResponse responseBodyValue -> BodyResponse responseBodyValue
    NonPageRedirectResponse responseBodyValue location -> RedirectResponse responseBodyValue location
    NonPageInternalRedirectResponse responseBodyValue routeRequest -> InternalRedirectResponse responseBodyValue routeRequest
    NonPageInternalRedirectResponseWithHeaders responseBodyValue headers routeRequest -> InternalRedirectResponseWithHeaders responseBodyValue headers routeRequest
    NonPageClientActionBodyResponse actionResponse -> ClientActionBodyResponse actionResponse
    NonPageEventStreamResponse responseBodyValue source -> EventStreamResponse responseBodyValue source
    NonPageProtocolResponse protocolResponse -> ProtocolResponseResult protocolResponse

-- | Adapt a non-page response across a mounted route/context algebra. The
-- closed fold has no page branch by construction, so mounting a guard cannot
-- accidentally regain the ability to manufacture a secured page.
mapNonPageResponse ::
  (RouteRequest route context -> RouteRequest mappedRoute mappedContext) ->
  NonPageResponse route context ->
  NonPageResponse mappedRoute mappedContext
mapNonPageResponse mapRoute response =
  case response of
    NonPageBodyResponse responseBodyValue -> NonPageBodyResponse responseBodyValue
    NonPageRedirectResponse responseBodyValue location -> NonPageRedirectResponse responseBodyValue location
    NonPageInternalRedirectResponse responseBodyValue routeRequest -> NonPageInternalRedirectResponse responseBodyValue (mapRoute routeRequest)
    NonPageInternalRedirectResponseWithHeaders responseBodyValue headers routeRequest -> NonPageInternalRedirectResponseWithHeaders responseBodyValue headers (mapRoute routeRequest)
    NonPageClientActionBodyResponse actionResponse -> NonPageClientActionBodyResponse (mapClientActionResponse mapRoute actionResponse)
    NonPageEventStreamResponse responseBodyValue source -> NonPageEventStreamResponse responseBodyValue source
    NonPageProtocolResponse protocolResponse -> NonPageProtocolResponse protocolResponse

data Response route context
  = PageResponse PageSecurity (Page route context)
  | PageResponseWithMetadata PageSecurity ResponseBody (Page route context)
  | BodyResponse ResponseBody
  | RedirectResponse ResponseBody Text
  | InternalRedirectResponse ResponseBody (RouteRequest route context)
  | InternalRedirectResponseWithHeaders ResponseBody Http.ResponseHeaders (RouteRequest route context)
  | ClientActionBodyResponse (ClientActionResponse route context)
  | EventStreamResponse ResponseBody ServerSentEventSource
  | ProtocolResponseResult ProtocolResponse

-- | Change the route and context carried by page and action-response
-- destinations. Route composition supplies both closed transformations so an
-- internal navigation cannot retain a child route after mounting.
mapResponsePage ::
  (Page route context -> Page mappedRoute mappedContext) ->
  (RouteRequest route context -> RouteRequest mappedRoute mappedContext) ->
  Response route context ->
  Response mappedRoute mappedContext
mapResponsePage mapPage mapRoute response =
  case response of
    PageResponse pageSecurity page -> PageResponse pageSecurity (mapPage page)
    PageResponseWithMetadata pageSecurity responseBodyValue page -> PageResponseWithMetadata pageSecurity responseBodyValue (mapPage page)
    BodyResponse responseBodyValue -> BodyResponse responseBodyValue
    RedirectResponse responseBodyValue location -> RedirectResponse responseBodyValue location
    InternalRedirectResponse responseBodyValue routeRequest -> InternalRedirectResponse responseBodyValue (mapRoute routeRequest)
    InternalRedirectResponseWithHeaders responseBodyValue headers routeRequest -> InternalRedirectResponseWithHeaders responseBodyValue headers (mapRoute routeRequest)
    ClientActionBodyResponse actionResponse -> ClientActionBodyResponse (mapClientActionResponse mapRoute actionResponse)
    EventStreamResponse responseBodyValue source -> EventStreamResponse responseBodyValue source
    ProtocolResponseResult protocolResponse -> ProtocolResponseResult protocolResponse

instance (Eq route, Eq context) => Eq (Response route context) where
  left == right =
    case (left, right) of
      (PageResponse leftSecurity leftPage, PageResponse rightSecurity rightPage) -> samePageSecurity leftSecurity rightSecurity && leftPage == rightPage
      (PageResponseWithMetadata leftSecurity leftBody leftPage, PageResponseWithMetadata rightSecurity rightBody rightPage) -> samePageSecurity leftSecurity rightSecurity && leftBody == rightBody && leftPage == rightPage
      (BodyResponse leftBody, BodyResponse rightBody) -> leftBody == rightBody
      (RedirectResponse leftBody leftLocation, RedirectResponse rightBody rightLocation) -> leftBody == rightBody && leftLocation == rightLocation
      (InternalRedirectResponse leftBody leftRequest, InternalRedirectResponse rightBody rightRequest) -> leftBody == rightBody && leftRequest == rightRequest
      (InternalRedirectResponseWithHeaders leftBody leftHeaders leftRequest, InternalRedirectResponseWithHeaders rightBody rightHeaders rightRequest) -> leftBody == rightBody && leftHeaders == rightHeaders && leftRequest == rightRequest
      (ClientActionBodyResponse leftAction, ClientActionBodyResponse rightAction) -> leftAction == rightAction
      (EventStreamResponse leftBody _, EventStreamResponse rightBody _) -> leftBody == rightBody
      (ProtocolResponseResult leftResponse, ProtocolResponseResult rightResponse) -> leftResponse == rightResponse
      _ -> False

instance (Show route, Show context) => Show (Response route context) where
  showsPrec precedence response =
    case response of
      PageResponse pageSecurity page -> showParen (precedence > 10) (showString "PageResponse " . showsPrec 11 pageSecurity . showChar ' ' . showsPrec 11 page)
      PageResponseWithMetadata pageSecurity responseBodyValue page -> showParen (precedence > 10) (showString "PageResponseWithMetadata " . showsPrec 11 pageSecurity . showChar ' ' . showsPrec 11 responseBodyValue . showChar ' ' . showsPrec 11 page)
      BodyResponse responseBodyValue -> showParen (precedence > 10) (showString "BodyResponse " . showsPrec 11 responseBodyValue)
      RedirectResponse responseBodyValue location -> showParen (precedence > 10) (showString "RedirectResponse " . showsPrec 11 responseBodyValue . showChar ' ' . shows location)
      InternalRedirectResponse responseBodyValue routeRequest -> showParen (precedence > 10) (showString "InternalRedirectResponse " . showsPrec 11 responseBodyValue . showChar ' ' . showsPrec 11 routeRequest)
      InternalRedirectResponseWithHeaders responseBodyValue headers routeRequest -> showParen (precedence > 10) (showString "InternalRedirectResponseWithHeaders " . showsPrec 11 responseBodyValue . showChar ' ' . shows headers . showChar ' ' . showsPrec 11 routeRequest)
      ClientActionBodyResponse actionResponse -> showParen (precedence > 10) (showString "ClientActionBodyResponse " . showsPrec 11 actionResponse)
      EventStreamResponse responseBodyValue _ -> showParen (precedence > 10) (showString "EventStreamResponse " . showsPrec 11 responseBodyValue . showString " <event-source>")
      ProtocolResponseResult protocolResponse -> showParen (precedence > 10) (showString "ProtocolResponseResult " . showsPrec 11 protocolResponse)
