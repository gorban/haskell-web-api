{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWeb.ServerSentEventSpec (spec) where

import HarchWeb (ServerSentEvent (..), renderServerSentEvent, serverSentEventContentType)
import Test.Hspec

spec :: Spec
spec =
  describe "server-sent events" $ do
    it "uses the canonical UTF-8 event-stream media type" $
      serverSentEventContentType `shouldBe` "text/event-stream; charset=utf-8"

    it "renders named events with an identifier and an empty terminating line" $
      renderServerSentEvent (ServerSentEvent (Just "page-update") (Just "42") "Ready")
        `shouldBe` "event: page-update\nid: 42\ndata: Ready\n\n"

    it "renders every payload line as a data field and keeps an empty payload observable" $ do
      renderServerSentEvent (ServerSentEvent Nothing Nothing "first\nsecond")
        `shouldBe` "data: first\ndata: second\n\n"
      renderServerSentEvent (ServerSentEvent Nothing Nothing "")
        `shouldBe` "data: \n\n"

    it "removes line breaks from protocol fields without turning them into injected fields" $
      renderServerSentEvent (ServerSentEvent (Just "page\nupdate") (Just "4\r\n2") "ok\r\nnow")
        `shouldBe` "event: pageupdate\nid: 42\ndata: ok\ndata: now\n\n"
