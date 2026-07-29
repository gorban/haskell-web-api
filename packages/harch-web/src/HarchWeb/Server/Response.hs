{-# LANGUAGE DuplicateRecordFields #-}

-- | Private typed request and response contracts for the WAI server pipeline.
module HarchWeb.Server.Response
  ( ClientActionRequest (..),
    ClientActionResponse (..),
    MiddlewareResult (..),
    RegionPatch (..),
    RequestMiddleware (..),
    Response (..),
    ResponseBody (..),
    ResponseDiagnostics (..),
    ServerSentEvent (..),
    ServerSentEventSource (..),
  )
where

import Data.Text (Text)
import HarchWeb.Document (Page)
import HarchWeb.Observability qualified as Observability
import Network.HTTP.Types qualified as Http
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
