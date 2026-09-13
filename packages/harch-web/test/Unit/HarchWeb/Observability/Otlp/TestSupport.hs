{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWeb.Observability.Otlp.TestSupport
  ( CapturedCollectorRequest (..),
    withOtlpCollector,
    extractQuotedJsonField,
    extractQuotedJsonIntegerFields,
    expectPlausibleEpochNanoTimestamps,
  )
where

import Control.Concurrent (MVar, forkIO, killThread, newEmptyMVar, putMVar, threadDelay)
import Control.Exception (finally)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Test.Hspec (Expectation, shouldBe, shouldSatisfy)
import TestCore.CustomAssertions (expectAll)
import Text.Read (readMaybe)
import Unit.HarchWeb.TestSupport (withUnusedLoopbackPort)

data CapturedCollectorRequest = CapturedCollectorRequest
  { capturedCollectorMethod :: ByteString.ByteString,
    capturedCollectorPath :: ByteString.ByteString,
    capturedCollectorHeaders :: [Http.Header],
    capturedCollectorBody :: LazyByteString.ByteString
  }

withOtlpCollector ::
  Http.Status ->
  LazyByteString.ByteString ->
  (HttpClient.Manager -> Text -> MVar CapturedCollectorRequest -> IO a) ->
  IO a
withOtlpCollector responseStatus responseBody action =
  withUnusedLoopbackPort $ \collectorPort -> do
    manager <- HttpClient.newManager HttpClient.defaultManagerSettings
    capturedRequestReference <- newEmptyMVar
    let collectorUrl = Text.pack ("http://127.0.0.1:" <> show collectorPort <> "/v1/traces")
        collectorApplication request respond = do
          requestBody <- Wai.strictRequestBody request
          putMVar
            capturedRequestReference
            CapturedCollectorRequest
              { capturedCollectorMethod = Wai.requestMethod request,
                capturedCollectorPath = Wai.rawPathInfo request,
                capturedCollectorHeaders = Wai.requestHeaders request,
                capturedCollectorBody = requestBody
              }
          respond (Wai.responseLBS responseStatus [("Content-Type", "application/json")] responseBody)
    serverThreadId <- forkIO (Warp.run collectorPort collectorApplication)
    threadDelay 50000
    action manager collectorUrl capturedRequestReference `finally` killThread serverThreadId

extractQuotedJsonField :: Text -> Text -> Maybe Text
extractQuotedJsonField fieldName bodyText =
  listToMaybe (extractQuotedJsonFields fieldName bodyText)

extractQuotedJsonIntegerFields :: Text -> Text -> [Integer]
extractQuotedJsonIntegerFields fieldName bodyText =
  mapMaybe (readMaybe . Text.unpack) (extractQuotedJsonFields fieldName bodyText)

expectPlausibleEpochNanoTimestamps :: Text -> Expectation
expectPlausibleEpochNanoTimestamps bodyText = do
  let earliestPlausibleEpochNano = 1577836800000000000
      latestPlausibleEpochNano = 4102444800000000000
      startTimes = extractQuotedJsonIntegerFields "startTimeUnixNano" bodyText
      endTimes = extractQuotedJsonIntegerFields "endTimeUnixNano" bodyText
  startTimes `shouldSatisfy` (not . null)
  length startTimes `shouldBe` length endTimes
  mapM_
    ( \(startTimeUnixNano, endTimeUnixNano) ->
        expectAll
          ( (startTimeUnixNano `shouldSatisfy` (>= earliestPlausibleEpochNano))
              :| [ endTimeUnixNano `shouldSatisfy` (< latestPlausibleEpochNano),
                   startTimeUnixNano `shouldSatisfy` (< endTimeUnixNano),
                   (endTimeUnixNano - startTimeUnixNano) `shouldSatisfy` (>= 1000)
                 ]
          )
    )
    (zip startTimes endTimes)

extractQuotedJsonFields :: Text -> Text -> [Text]
extractQuotedJsonFields fieldName bodyText =
  case Text.breakOn fieldPrefix bodyText of
    (_, withField)
      | Text.null withField -> []
      | otherwise ->
          let fieldValueStart = Text.drop (Text.length fieldPrefix) withField
              fieldValue = Text.takeWhile (/= '"') fieldValueStart
              remainingBody = Text.drop (Text.length fieldValue + 1) fieldValueStart
           in fieldValue : extractQuotedJsonFields fieldName remainingBody
  where
    fieldPrefix = "\"" <> fieldName <> "\":\""
