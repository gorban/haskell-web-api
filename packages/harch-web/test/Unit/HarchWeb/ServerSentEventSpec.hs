{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWeb.ServerSentEventSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import HarchWeb
  ( ClientActionResponse (..),
    Response (..),
    ResponseBody (..),
    ServerSentEvent (..),
    eventStreamResponse,
    nextServerSentEvent,
    renderServerSentEvent,
    serverSentEventContentType,
    serverSentEventSourceFromList,
  )
import Network.HTTP.Types qualified as Http
import Test.Hspec
import TestCore.CustomAssertions (expectAll)

spec :: Spec
spec =
  describe "server-sent events" $ do
    it "uses the canonical UTF-8 event-stream media type" $
      serverSentEventContentType `shouldBe` "text/event-stream; charset=utf-8"

    it "renders named events with an identifier and an empty terminating line" $
      renderServerSentEvent (ServerSentEvent (Just "page-update") (Just "42") "Ready")
        `shouldBe` "event: page-update\nid: 42\ndata: Ready\n\n"

    it "renders every payload line as a data field and keeps an empty payload observable" $ do
      expectAll
        ( (renderServerSentEvent (ServerSentEvent Nothing Nothing "first\nsecond") `shouldBe` "data: first\ndata: second\n\n")
            :| [renderServerSentEvent (ServerSentEvent Nothing Nothing "") `shouldBe` "data: \n\n"]
        )

    it "removes line breaks from protocol fields without turning them into injected fields" $
      renderServerSentEvent (ServerSentEvent (Just "page\nupdate") (Just "4\r\n2") "ok\r\nnow")
        `shouldBe` "event: pageupdate\nid: 42\ndata: ok\ndata: now\n\n"

    it "provides finite sources for deterministic subscriptions" $ do
      source <- serverSentEventSourceFromList [ServerSentEvent Nothing (Just "1") "first"]
      nextServerSentEvent source `shouldReturn` Just (ServerSentEvent Nothing (Just "1") "first")
      nextServerSentEvent source `shouldReturn` Nothing

    it "uses an opaque source in response display output" $ do
      source <- serverSentEventSourceFromList []
      show (eventStreamResponse source :: Response () ())
        `shouldBe` "EventStreamResponse (ResponseBody {responseStatus = Status {statusCode = 200, statusMessage = \"OK\"}, responseContentType = \"text/event-stream; charset=utf-8\", responseBody = \"\", responseObservabilityAttributes = [], responseLogEntries = []}) <event-source>"

    it "compares opaque streams by their safe metadata and renders every response form" $ do
      firstSource <- serverSentEventSourceFromList []
      secondSource <- serverSentEventSourceFromList []
      let responseBodyValue = ResponseBody Http.status204 "text/plain; charset=utf-8" "Done" [] []
          actionResponse = ClientActionResponse Http.status200 [] Nothing [] [] []
          streamResponse = eventStreamResponse firstSource :: Response () ()
          clientActionResponse = ClientActionBodyResponse actionResponse :: Response () ()
      expectAll
        ( (streamResponse `shouldBe` eventStreamResponse secondSource)
            :| [ clientActionResponse `shouldBe` clientActionResponse,
                 streamResponse `shouldNotBe` BodyResponse responseBodyValue,
                 showsPrec 11 (RedirectResponse responseBodyValue "/next" :: Response () ()) "" `shouldBe` "(RedirectResponse (ResponseBody {responseStatus = Status {statusCode = 204, statusMessage = \"No Content\"}, responseContentType = \"text/plain; charset=utf-8\", responseBody = \"Done\", responseObservabilityAttributes = [], responseLogEntries = []}) \"/next\")",
                 showsPrec 11 clientActionResponse "" `shouldBe` "(ClientActionBodyResponse (ClientActionResponse {clientActionStatus = Status {statusCode = 200, statusMessage = \"OK\"}, clientActionPatches = [], clientActionFocusId = Nothing, clientActionHeaders = [], clientActionObservabilityAttributes = [], clientActionLogEntries = []}))"
               ]
        )
