{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWeb.EmailSpec (spec) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, SomeException, bracket, displayException, throwIO, try)
import Control.Monad (unless)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word16)
import HarchWeb.Email
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as SocketByteString
import Test.Hspec
import TestCore.CustomAssertions (expectAll)

sampleRecipient :: EmailAddress
sampleRecipient = required "sample recipient" (mkEmailAddress "ada@example.test")

sampleSender :: EmailAddress
sampleSender = required "sample sender" (mkEmailAddress "noreply@example.test")

sampleMessage :: EmailMessage
sampleMessage = required "sample message" (mkEmailMessage sampleRecipient "Account verification" ".first\nsecond\r\n.third")

required :: String -> Maybe value -> value
required label = fromMaybe (error ("expected " <> label))

spec :: Spec
spec = do
  describe "EmailAddress" $ do
    it "accepts safe mailbox values and rejects ambiguous envelope addresses" $ do
      expectAll
        ( (emailAddressText sampleRecipient `shouldBe` "ada@example.test")
            :| [ mkEmailAddress "A1._+-@Example-1.test" `shouldSatisfy` (/= Nothing),
                 sampleRecipient /= sampleSender `shouldBe` True,
                 show sampleRecipient `shouldBe` "EmailAddress \"ada@example.test\"",
                 show [sampleRecipient] `shouldBe` "[EmailAddress \"ada@example.test\"]",
                 mkEmailAddress "@example.test" `shouldBe` Nothing,
                 mkEmailAddress "ada@" `shouldBe` Nothing,
                 mkEmailAddress "ada.example.test" `shouldBe` Nothing,
                 mkEmailAddress "ada@example@test" `shouldBe` Nothing,
                 mkEmailAddress "ada space@example.test" `shouldBe` Nothing,
                 mkEmailAddress "ada@example test" `shouldBe` Nothing
               ]
        )

  describe "EmailMessage" $ do
    it "keeps header injection out of application-authored messages" $ do
      expectAll
        ( (emailMessageRecipient sampleMessage `shouldBe` sampleRecipient)
            :| [ emailMessageSubject sampleMessage `shouldBe` "Account verification",
                 emailMessageBody sampleMessage `shouldBe` ".first\nsecond\r\n.third",
                 sampleMessage /= required "different message" (mkEmailMessage sampleRecipient "Different" "body") `shouldBe` True,
                 show sampleMessage `shouldBe` "EmailMessage {emailMessageRecipient = EmailAddress \"ada@example.test\", emailMessageSubject = \"Account verification\", emailMessageBody = \".first\\nsecond\\r\\n.third\"}",
                 show [sampleMessage] `shouldContain` "EmailMessage",
                 mkEmailMessage sampleRecipient "Bcc: attacker@example.test\r\n" "body" `shouldBe` Nothing,
                 mkEmailMessage sampleRecipient "Bcc: attacker@example.test\n" "body" `shouldBe` Nothing,
                 mkEmailMessage sampleRecipient "Sena\241" "body" `shouldBe` Nothing
               ]
        )

  describe "verificationEmail" $ do
    it "renders localized English and Spanish verification content" $ do
      let englishEmail = verificationEmail EmailEnglish sampleRecipient "https://account.example.test/verify/en"
          spanishEmail = verificationEmail EmailSpanish sampleRecipient "https://account.example.test/verify/es"
      expectAll
        ( (EmailEnglish /= EmailSpanish `shouldBe` True)
            :| [ show EmailEnglish `shouldBe` "EmailEnglish",
                 show [EmailEnglish, EmailSpanish] `shouldBe` "[EmailEnglish,EmailSpanish]",
                 emailMessageRecipient englishEmail `shouldBe` sampleRecipient,
                 emailMessageSubject englishEmail `shouldBe` "Verify your email address",
                 emailMessageBody englishEmail `shouldBe` "Open this link to verify your email address:\nhttps://account.example.test/verify/en",
                 emailMessageRecipient spanishEmail `shouldBe` sampleRecipient,
                 emailMessageSubject spanishEmail `shouldBe` "Verifica tu correo electronico",
                 emailMessageBody spanishEmail `shouldBe` "Abre este enlace para verificar tu correo electronico:\nhttps://account.example.test/verify/es"
               ]
        )

  describe "SmtpConfig" $ do
    it "requires a resolved host, nonzero port, and header-safe HELO name" $ do
      expectAll
        ( (isJust (mkSmtpConfig "127.0.0.1" 2525 "account.example.test" sampleSender) `shouldBe` True)
            :| [ isNothing (mkSmtpConfig "" 2525 "account.example.test" sampleSender) `shouldBe` True,
                 isNothing (mkSmtpConfig "127.0.0.1" 0 "account.example.test" sampleSender) `shouldBe` True,
                 isNothing (mkSmtpConfig "127.0.0.1" 2525 "account\r\n.example.test" sampleSender) `shouldBe` True,
                 isJust (mkAuthenticatedSmtpConfig "127.0.0.1" 2525 "account.example.test" sampleSender "smtp-user" "smtp-password") `shouldBe` True,
                 isNothing (mkAuthenticatedSmtpConfig "127.0.0.1" 2525 "account.example.test" sampleSender "" "smtp-password") `shouldBe` True,
                 isNothing (mkAuthenticatedSmtpConfig "127.0.0.1" 2525 "account.example.test" sampleSender "smtp-user\r" "smtp-password") `shouldBe` True,
                 isNothing (mkAuthenticatedSmtpConfig "127.0.0.1" 2525 "account.example.test" sampleSender "smtp-user" "smtp-password\NUL") `shouldBe` True
               ]
        )

  describe "deliverSmtpEmail" $ do
    it "fails clearly when its resolver has no usable SMTP address" $ do
      let config = required "SMTP config" (mkSmtpConfig "smtp.example.test" 2525 "account.example.test" sampleSender)
      result <- try (deliverSmtpEmailWithResolver (const (pure Nothing)) config sampleMessage) :: IO (Either IOException ())
      case result of
        Left failure -> displayException failure `shouldContain` "SMTP hostname did not resolve to a connectable address"
        Right () -> expectationFailure "expected an SMTP resolver failure"

    it "delivers a dot-stuffed UTF-8 transactional message to a loopback SMTP server" $
      withLoopbackSmtp
        acceptingServer
        ( \port -> do
            let config = required "loopback SMTP config" (mkSmtpConfig "127.0.0.1" port "account.example.test" sampleSender)
            deliverSmtpEmail config sampleMessage
        )
        >>= \(_, deliveredMessage) ->
          deliveredMessage
            `shouldBe` TextEncoding.encodeUtf8 "From: <noreply@example.test>\r\nTo: <ada@example.test>\r\nSubject: Account verification\r\nMIME-Version: 1.0\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Transfer-Encoding: 8bit\r\n\r\n..first\r\nsecond\r\n..third\r\n.\r\n"

    it "stops when the SMTP server rejects a command" $ do
      _ <- withLoopbackSmtp rejectingEhloServer $ \port -> do
        let config = required "rejecting SMTP config" (mkSmtpConfig "127.0.0.1" port "account.example.test" sampleSender)
        result <- try (deliverSmtpEmail config sampleMessage) :: IO (Either IOException ())
        case result of
          Left failure -> do
            expectAll
              ( (displayException failure `shouldContain` "Unexpected SMTP response:")
                  :| [length (displayException failure) `shouldSatisfy` (> 0)]
              )
          Right () -> expectationFailure "expected an SMTP command rejection"
      pure ()

    it "uses SMTP AUTH PLAIN when credentials are configured" $
      withLoopbackSmtp
        acceptingAuthenticatedServer
        ( \port -> do
            let config = required "authenticated SMTP config" (mkAuthenticatedSmtpConfig "127.0.0.1" port "account.example.test" sampleSender "smtp-user" "smtp-password")
            deliverSmtpEmail config sampleMessage
        )
        >>= \(_, deliveredMessage) ->
          deliveredMessage `shouldSatisfy` ByteString.isInfixOf "Subject: Account verification"

withLoopbackSmtp :: (Socket.Socket -> IO ByteString.ByteString) -> (Word16 -> IO value) -> IO (value, ByteString.ByteString)
withLoopbackSmtp server action =
  bracket start stop $ \(_, port, completed) -> do
    result <- action (fromIntegral port)
    serverResult <- takeMVar completed
    deliveredMessage <- either throwIO pure serverResult
    pure (result, deliveredMessage)
  where
    start = do
      listener <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
      Socket.setSocketOption listener Socket.ReuseAddr 1
      Socket.bind listener (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1)))
      Socket.listen listener 1
      address <- Socket.getSocketName listener
      port <-
        case address of
          Socket.SockAddrInet assignedPort _ -> pure assignedPort
          _ -> ioError (userError "expected an IPv4 loopback listener")
      completed <- newEmptyMVar
      _ <-
        forkIO $ do
          serverResult <- try (bracket (Socket.accept listener) (Socket.close . fst) (server . fst)) :: IO (Either SomeException ByteString.ByteString)
          putMVar completed serverResult
      pure (listener, port, completed)
    stop (listener, _, _) = Socket.close listener

acceptingServer :: Socket.Socket -> IO ByteString.ByteString
acceptingServer socket = do
  sendResponse socket "220 loopback ready\r\n"
  expectCommand socket "EHLO account.example.test\r\n" "250 loopback\r\n"
  expectCommand socket "MAIL FROM:<noreply@example.test>\r\n" "250 sender accepted\r\n"
  expectCommand socket "RCPT TO:<ada@example.test>\r\n" "250 recipient accepted\r\n"
  expectCommand socket "DATA\r\n" "354 send message\r\n"
  message <- receiveUntil socket "\r\n.\r\n"
  sendResponse socket "250 message accepted\r\n"
  expectCommand socket "QUIT\r\n" "221 goodbye\r\n"
  pure message

acceptingAuthenticatedServer :: Socket.Socket -> IO ByteString.ByteString
acceptingAuthenticatedServer socket = do
  sendResponse socket "220 loopback ready\r\n"
  expectCommand socket "EHLO account.example.test\r\n" "250 loopback\r\n"
  expectCommand socket "AUTH PLAIN AHNtdHAtdXNlcgBzbXRwLXBhc3N3b3Jk\r\n" "235 authenticated\r\n"
  expectCommand socket "MAIL FROM:<noreply@example.test>\r\n" "250 sender accepted\r\n"
  expectCommand socket "RCPT TO:<ada@example.test>\r\n" "250 recipient accepted\r\n"
  expectCommand socket "DATA\r\n" "354 send message\r\n"
  message <- receiveUntil socket "\r\n.\r\n"
  sendResponse socket "250 message accepted\r\n"
  expectCommand socket "QUIT\r\n" "221 goodbye\r\n"
  pure message

rejectingEhloServer :: Socket.Socket -> IO ByteString.ByteString
rejectingEhloServer socket = do
  sendResponse socket "220 loopback ready\r\n"
  command <- receiveChunk socket
  command `shouldBeServer` "EHLO account.example.test\r\n"
  sendResponse socket "500 rejected\r\n"
  pure ByteString.empty

expectCommand :: Socket.Socket -> ByteString.ByteString -> ByteString.ByteString -> IO ()
expectCommand socket expected response = do
  command <- receiveChunk socket
  command `shouldBeServer` expected
  sendResponse socket response

receiveChunk :: Socket.Socket -> IO ByteString.ByteString
receiveChunk socket = do
  bytes <- SocketByteString.recv socket 4096
  if ByteString.null bytes
    then ioError (userError "SMTP client disconnected before completing the command")
    else pure bytes

receiveUntil :: Socket.Socket -> ByteString.ByteString -> IO ByteString.ByteString
receiveUntil socket terminator = go ByteString.empty
  where
    go received = do
      next <- receiveChunk socket
      let combined = received <> next
      if terminator `ByteString.isSuffixOf` combined
        then pure combined
        else go combined

sendResponse :: Socket.Socket -> ByteString.ByteString -> IO ()
sendResponse = SocketByteString.sendAll

shouldBeServer :: ByteString.ByteString -> ByteString.ByteString -> IO ()
shouldBeServer actual expected =
  unless (actual == expected) $
    ioError
      ( userError
          ( "Unexpected SMTP command: "
              <> show actual
              <> "; expected "
              <> show expected
          )
      )
