{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWeb.GmailSpec (spec) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (SomeException, displayException, finally, try)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text qualified as Text
import HarchWeb.Email (EmailAddress, EmailMessage, EmailMessageInput (..), mkEmailAddress, mkEmailMessage)
import HarchWeb.Gmail
  ( GmailHttpRequest (..),
    GmailHttpResponse (..),
    deliverGmailApiEmailWithRunner,
    mkGmailApiConfig,
    runGmailHttpRequest,
  )
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Types qualified as Http
import Network.Socket qualified as Socket
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Test.Hspec

spec :: Spec
spec = do
  describe "deliverGmailApiEmailWithRunner" $ do
    it "posts a base64url RFC 2822 message with a delegated bearer token" $ do
      receivedRequest <- newIORef Nothing
      let sender = requiredEmailAddress "sender@example.com"
          message = requiredEmailMessage "recipient@example.com" "Welcome" "A line\n.second line"
          config = mkGmailApiConfig sender (pure "delegated-access-token")
          runner request = writeIORef receivedRequest (Just request) >> pure (GmailHttpResponse 200 "{\"id\":\"sent\"}")
      deliverGmailApiEmailWithRunner runner config message
      request <- readIORef receivedRequest
      case request of
        Nothing -> expectationFailure "Expected Gmail request"
        Just gmailRequest -> do
          gmailHttpMethod gmailRequest `shouldBe` "POST"
          gmailHttpUrl gmailRequest `shouldBe` "https://gmail.googleapis.com/gmail/v1/users/me/messages/send"
          gmailHttpHeaders gmailRequest `shouldContain` [("Authorization", "Bearer delegated-access-token")]
          gmailHttpHeaders gmailRequest `shouldContain` [("Content-Type", "application/json")]
          Text.isPrefixOf "{\"raw\":\"" (gmailHttpBody gmailRequest) `shouldBe` True
          Text.isInfixOf "=" (gmailHttpBody gmailRequest) `shouldBe` False

    it "rejects invalid delegated access tokens before making an HTTP request" $ do
      let sender = requiredEmailAddress "sender@example.com"
          message = requiredEmailMessage "recipient@example.com" "Welcome" "Body"
          config = mkGmailApiConfig sender (pure "bad\ntoken")
          runner _ = expectationFailure "HTTP runner must not be called" >> pure (GmailHttpResponse 200 "")
      result <- try (deliverGmailApiEmailWithRunner runner config message) :: IO (Either SomeException ())
      case result of
        Left failure -> displayException failure `shouldContain` "access-token provider returned an invalid token"
        Right () -> expectationFailure "Expected token validation failure"

    it "surfaces API failures without including the bearer token" $ do
      let sender = requiredEmailAddress "sender@example.com"
          message = requiredEmailMessage "recipient@example.com" "Welcome" "Body"
          config = mkGmailApiConfig sender (pure "private-token")
          runner _ = pure (GmailHttpResponse 403 "delegation is not configured")
      result <- try (deliverGmailApiEmailWithRunner runner config message) :: IO (Either SomeException ())
      case result of
        Left failure -> do
          displayException failure `shouldContain` "Gmail API send failed with status 403"
          displayException failure `shouldNotContain` "private-token"
        Right () -> expectationFailure "Expected Gmail API failure"

    it "uses URL-safe base64 replacements" $ do
      receivedRequest <- newIORef Nothing
      let sender = requiredEmailAddress "sender@example.com"
          message = requiredEmailMessage "recipient@example.com" "Welcome" "~~~???~~~???"
          config = mkGmailApiConfig sender (pure "delegated-access-token")
          runner request = writeIORef receivedRequest (Just request) >> pure (GmailHttpResponse 200 "")
      deliverGmailApiEmailWithRunner runner config message
      request <- readIORef receivedRequest
      case request of
        Nothing -> expectationFailure "Expected Gmail request"
        Just gmailRequest -> do
          Text.isInfixOf "+" (gmailHttpBody gmailRequest) `shouldBe` False
          Text.isInfixOf "/" (gmailHttpBody gmailRequest) `shouldBe` False

    it "executes configured HTTP requests and preserves the status and body" $
      withHttpServer $ \baseUrl -> do
        manager <- HttpClient.newManager HttpClient.defaultManagerSettings
        response <-
          runGmailHttpRequest
            manager
            GmailHttpRequest
              { gmailHttpMethod = "POST",
                gmailHttpUrl = baseUrl <> "/gmail-send",
                gmailHttpHeaders = [("Authorization", "Bearer test-token"), ("Content-Type", "application/json")],
                gmailHttpBody = "{\"raw\":\"abc\"}"
              }
        gmailHttpStatus response `shouldBe` 202
        gmailHttpResponseBody response `shouldBe` "{\"accepted\":true}"

requiredEmailAddress :: Text.Text -> EmailAddress
requiredEmailAddress value =
  case mkEmailAddress value of
    Just address -> address
    Nothing -> error "Expected valid email address"

requiredEmailMessage :: Text.Text -> Text.Text -> Text.Text -> EmailMessage
requiredEmailMessage recipient subject body =
  case mkEmailMessage
    EmailMessageInput
      { emailInputRecipient = requiredEmailAddress recipient,
        emailInputSubject = subject,
        emailInputBody = body
      } of
    Just message -> message
    Nothing -> error "Expected valid email message"

withHttpServer :: (Text.Text -> IO a) -> IO a
withHttpServer action =
  withUnusedLoopbackPort $ \port -> do
    let baseUrl = Text.pack ("http://127.0.0.1:" <> show port)
        application request respond = do
          requestBody <- Wai.strictRequestBody request
          case (Wai.requestMethod request, Wai.rawPathInfo request, lookup "Authorization" (Wai.requestHeaders request), requestBody) of
            ("POST", "/gmail-send", Just "Bearer test-token", "{\"raw\":\"abc\"}") ->
              respond (Wai.responseLBS Http.accepted202 [("Content-Type", "application/json")] "{\"accepted\":true}")
            _ -> respond (Wai.responseLBS Http.badRequest400 [] "unexpected request")
    serverThreadId <- forkIO (Warp.run port application)
    threadDelay 50000
    action baseUrl `finally` killThread serverThreadId

withUnusedLoopbackPort :: (Int -> IO a) -> IO a
withUnusedLoopbackPort action = do
  reservedSocket <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
  Socket.bind reservedSocket (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1)))
  socketAddress <- Socket.getSocketName reservedSocket
  Socket.close reservedSocket
  case socketAddress of
    Socket.SockAddrInet port _ -> action (fromIntegral port)
    _ -> error "Expected IPv4 loopback reservation socket"
