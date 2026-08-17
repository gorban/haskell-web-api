{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Private server-sent-event stream construction and rendering.
module HarchWeb.Server.Sse
  ( eventStreamResponse,
    renderServerSentEvent,
    serverSentEventContentType,
    serverSentEventSourceFromList,
  )
where

import Data.IORef (atomicModifyIORef', newIORef)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Server.Response
import Network.HTTP.Types qualified as Http

eventStreamResponse :: ServerSentEventSource -> Response route context
eventStreamResponse =
  EventStreamResponse
    ResponseBody
      { responseStatus = Http.status200,
        responseContentType = serverSentEventContentType,
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

renderServerSentEvent :: ServerSentEvent -> Text
renderServerSentEvent
  ServerSentEvent
    { serverSentEventName,
      serverSentEventId,
      serverSentEventData
    } =
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

serverSentEventContentType :: Text
serverSentEventContentType = "text/event-stream; charset=utf-8"
