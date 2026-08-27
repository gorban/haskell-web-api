{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, SomeException, bracket, displayException, throwIO, try)
import Control.Monad (unless)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Default (def)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word16)
import HarchWeb.Email
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as SocketByteString
import Network.TLS qualified as TLS
import Network.TLS.Extra.Cipher qualified as TLSCipher
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

sampleRecipient :: EmailAddress
sampleRecipient = required "sample recipient" (mkEmailAddress "ada@example.test")

sampleSender :: EmailAddress
sampleSender = required "sample sender" (mkEmailAddress "noreply@example.test")

sampleMessage :: EmailMessage
sampleMessage = required "sample message" (mkEmailMessage (EmailMessageInput sampleRecipient "Account verification" ".first\nsecond\r\n.third"))

-- | A long-lived, test-only localhost CA/server certificate. The transport
-- tests place the public certificate in the system-store override and run a
-- real TLS server with its matching key, exercising hostname and chain
-- validation without accepting an untrusted peer.
smtpTlsCertificatePem :: String
smtpTlsCertificatePem =
  unlines
    [ "-----BEGIN CERTIFICATE-----",
      "MIIDTDCCAjSgAwIBAgIUHA13DKO1JeFtr6x7cNzIa+EAn8UwDQYJKoZIhvcNAQEL",
      "BQAwFDESMBAGA1UEAwwJbG9jYWxob3N0MB4XDTI2MDgyNjIyMzg1MFoXDTQ2MDgy",
      "MTIyMzg1MFowFDESMBAGA1UEAwwJbG9jYWxob3N0MIIBIjANBgkqhkiG9w0BAQEF",
      "AAOCAQ8AMIIBCgKCAQEA4bgqkjAISnkPrB7NH25KPVzBU3O9ZemHSYURhBTNG0r0",
      "uqrHM5tESWkMdthUTkYb2aVjfT2ubNlXf4zZyMR1zkV/uSwMEfo4uxnW4CT80zyt",
      "DJnV83wyblv1/HkNYbdKe27HQP/ddaX0V0py5hM8Gy303ZS/pMoBqYRCJSUWAWxW",
      "ywodoWeaEmXBeRU6iwIMzC8owRx6HOg1pOsSjdNt3pkqfYdshe10lqQbGiqct5pA",
      "dQMCVOKDyxILKT9D2b0s6Sult+Ch3xXUIBRMqgkRP6Gb0iqQwIeWO0DCmwlrVHiY",
      "QnzU+4iYNppFE4bkIqrd5XOU7V3VOHdOPpJEB3lZ4QIDAQABo4GVMIGSMB0GA1Ud",
      "DgQWBBTlbDKxMavP2NXvcsYc6Kc7Y5flXjAfBgNVHSMEGDAWgBTlbDKxMavP2NXv",
      "csYc6Kc7Y5flXjAPBgNVHRMBAf8EBTADAQH/MA4GA1UdDwEB/wQEAwICpDATBgNV",
      "HSUEDDAKBggrBgEFBQcDATAaBgNVHREEEzARgglsb2NhbGhvc3SHBH8AAAEwDQYJ",
      "KoZIhvcNAQELBQADggEBABZcSneZEd8/4XIQYKkQ6fZ7kjv/wnQ3g+f55ovqInbz",
      "zIIQwEj1VZzO6M7kExza7S9IT7ylLZLTQ/hAOlozypbU3/7Gfsnk1+qkx+wydSWM",
      "/TLKNu+xp1f5EwBtE2VUb20FIKAhsS0qYTIReBVlWeUy8r1VP8n3FQRTan39ZVdu",
      "xbXIHuwp94NpFDe3zk1H8ihXPUFhjG2xEVZmmrUpS9ysuD8RIU9Cd9/UCKmxtCEW",
      "naqmcr8Rpc4CBVp/TlTrGCeFtwmSa4JhcycLqGti2zwqhm9SYXiYyyQ/OIQbeN9N",
      "5Z33K4rAoZ4L9d7cJX/2H/j/i2al6VfG7xq6eqKAPUE=",
      "-----END CERTIFICATE-----"
    ]

smtpTlsPrivateKeyPem :: String
smtpTlsPrivateKeyPem =
  unlines
    [ "-----BEGIN PRIVATE KEY-----",
      "MIIEvgIBADANBgkqhkiG9w0BAQEFAASCBKgwggSkAgEAAoIBAQDhuCqSMAhKeQ+s",
      "Hs0fbko9XMFTc71l6YdJhRGEFM0bSvS6qsczm0RJaQx22FRORhvZpWN9Pa5s2Vd/",
      "jNnIxHXORX+5LAwR+ji7GdbgJPzTPK0MmdXzfDJuW/X8eQ1ht0p7bsdA/911pfRX",
      "SnLmEzwbLfTdlL+kygGphEIlJRYBbFbLCh2hZ5oSZcF5FTqLAgzMLyjBHHoc6DWk",
      "6xKN023emSp9h2yF7XSWpBsaKpy3mkB1AwJU4oPLEgspP0PZvSzpK6W34KHfFdQg",
      "FEyqCRE/oZvSKpDAh5Y7QMKbCWtUeJhCfNT7iJg2mkUThuQiqt3lc5TtXdU4d04+",
      "kkQHeVnhAgMBAAECggEAB5XglQlDshBXnZLAanVn+HVw8sdsM+v76wArj/CCBGWJ",
      "tr83/WI9p9OTAr3E4YeGIT48tcVYTVn60IjGJd2CUamwv+M8zvAwLrYmLDdZ/sBR",
      "LB/8Wm0lUGOgmeON4VI0piluzUlRTYZDoUOu0Pq7AEp+/4ujlXELdWwCLAzkbIJB",
      "yBWUTn48xlBz98IdxrLjfMpSlPmrmhX8pHU5hwGWdmu2rjQrYKfKKeC+aPtBrQ0f",
      "qqZ+464wVe1V6wIwIbdnt9QciZC8WKGEv1GeaMI/io6gTXVI2rYiHhBckI9weG08",
      "WCkNXAtPqeWE7hHVI4kvFdZOmYoXXV97kkWt5fxudQKBgQD/s8MIDy/Z2FOsI5Vk",
      "XurhK3CjoFu63wYHgelozNs6WJp/zSf3//G2a+fhyWIHkMv9KLrkw4xU3Cqb2Td1",
      "4oF90eJ8+vwsmvZVPhXC2nGEZH6Q5mpFEpI4zjhV0rB+e93yx3fsLXjJGese46k9",
      "bX8lJ3g7kKdtkb6RXeRVtUm+VQKBgQDh+3cLVX+t89F7Qg8N/yVqrzpdawmAdEgD",
      "3zyshrtMGL3YAGPEDJMpRMD5t86tKEM4J+j4xKy/iWGPBj9AjnOrOl6/BSPbDk1Y",
      "cmxfyVXwW3OcSJWhOE9LGIgkw562LDPD9MTcbxi0uzzhXW3J4mXB+v050mr/jnIw",
      "ZXCvD01hXQKBgQCyJTBcvg/tauyogkYDnqlvZM9eAEvIPrc5pcXTEN4voSlKnskY",
      "3AUvva/Yu3ADq1qjLqw+0wpC2P0KhWRBSiRDX2W3AzmLbg7uxolsrrnlrgoLs6wY",
      "IV8kGnBfQqFaDbxM97FSJz/+g4MuixgOxumNHhmDDj3HdYHaTxrUp7AP3QKBgQDA",
      "EdEuiu7IVNEZaB4PkWql8GU0/ULIMQz8bYLOSH1swiKpMjuZZ+60RsGV/YnmVrA7",
      "TICQ3+jLquKGg5LDNdBOGoBo7t940gEfSbGfACrQ7YmnrXGv44JRm3ordTCyOYON",
      "675edPaUtgearKQh0REX/FG6EYPrzZB0XeOWhvUH6QKBgAOhiP/DXPR2MHkYPFV2",
      "Luvmvbjabp3CAfDOHnTFkyp0WFWkTOg//2mwOAfqIT4onnn1FxUKhAC0vaOKBjn9",
      "u5NdCXYgRIljOvfAKqt0qrat3X33B8SYX5d7y+pPIy0I7lXk6RnAwT3K/sJWr/xh",
      "xQrpzK+lX3Op80vAaLRGv2CM",
      "-----END PRIVATE KEY-----"
    ]

required :: String -> Maybe value -> value
required label = fromMaybe (error ("expected " <> label))

smtpConfigInput :: Text -> Word16 -> Text -> EmailAddress -> Maybe SmtpAuthentication -> SmtpConfigInput
smtpConfigInput host port heloName sender authentication =
  SmtpConfigInput
    { smtpInputHost = smtpServerHost host,
      smtpInputPort = port,
      smtpInputHeloName = smtpServerHeloName heloName,
      smtpInputEnvelopeSender = sender,
      smtpInputAuthentication = authentication
    }

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
                 mkEmailAddress "ada@example test" `shouldBe` Nothing,
                 mkEmailAddress (Text.replicate 64 "a" <> "@" <> Text.replicate 189 "b") `shouldSatisfy` (/= Nothing),
                 mkEmailAddress (Text.replicate 64 "a" <> "@" <> Text.replicate 190 "b") `shouldBe` Nothing
               ]
        )

  describe "EmailMessage" $ do
    it "keeps header injection out of application-authored messages" $ do
      expectAll
        ( (emailMessageRecipient sampleMessage `shouldBe` sampleRecipient)
            :| [ emailMessageSubject sampleMessage `shouldBe` "Account verification",
                 emailMessageBody sampleMessage `shouldBe` ".first\nsecond\r\n.third",
                 sampleMessage /= required "different message" (mkEmailMessage (EmailMessageInput sampleRecipient "Different" "body")) `shouldBe` True,
                 show sampleMessage `shouldBe` "EmailMessage {emailMessageRecipient = EmailAddress \"ada@example.test\", emailMessageSubject = \"Account verification\", emailMessageBody = \".first\\nsecond\\r\\n.third\"}",
                 show [sampleMessage] `shouldContain` "EmailMessage",
                 mkEmailMessage (EmailMessageInput sampleRecipient "Bcc: attacker@example.test\r\n" "body") `shouldBe` Nothing,
                 mkEmailMessage (EmailMessageInput sampleRecipient "Bcc: attacker@example.test\n" "body") `shouldBe` Nothing,
                 mkEmailMessage (EmailMessageInput sampleRecipient "subject\NUL" "body") `shouldBe` Nothing,
                 mkEmailMessage (EmailMessageInput sampleRecipient "Señal" "body") `shouldSatisfy` isJust
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
                 emailMessageSubject spanishEmail `shouldBe` "Verifica tu correo electrónico",
                 emailMessageBody spanishEmail `shouldBe` "Abre este enlace para verificar tu correo electrónico:\nhttps://account.example.test/verify/es",
                 renderEmailMessage sampleSender spanishEmail `shouldSatisfy` ByteString.isInfixOf "Subject: =?UTF-8?B?"
               ]
        )

  describe "SmtpConfig" $ do
    it "requires a resolved host, nonzero port, and header-safe HELO name" $ do
      expectAll
        ( (isJust (mkSmtpConfig (smtpConfigInput "127.0.0.1" 2525 "account.example.test" sampleSender Nothing)) `shouldBe` True)
            :| [ isNothing (mkSmtpConfig (smtpConfigInput "" 2525 "account.example.test" sampleSender Nothing)) `shouldBe` True,
                 isNothing (mkSmtpConfig (smtpConfigInput "127.0.0.1" 0 "account.example.test" sampleSender Nothing)) `shouldBe` True,
                 isNothing (mkSmtpConfig (smtpConfigInput "127.0.0.1" 2525 "account\r\n.example.test" sampleSender Nothing)) `shouldBe` True,
                 isNothing (mkSmtpConfig (smtpConfigInput "127.0.0.1" 2525 "account\NUL.example.test" sampleSender Nothing)) `shouldBe` True,
                 isJust (mkSmtpConfig (smtpConfigInput "127.0.0.1" 2525 "account.example.test" sampleSender (Just (smtpAuthentication (smtpLoginUsername "smtp-user") (smtpLoginPassword "smtp-password"))))) `shouldBe` True,
                 isNothing (mkSmtpConfig (smtpConfigInput "127.0.0.1" 2525 "account.example.test" sampleSender (Just (smtpAuthentication (smtpLoginUsername "") (smtpLoginPassword "smtp-password"))))) `shouldBe` True,
                 isNothing (mkSmtpConfig (smtpConfigInput "127.0.0.1" 2525 "account.example.test" sampleSender (Just (smtpAuthentication (smtpLoginUsername "smtp-user\r") (smtpLoginPassword "smtp-password"))))) `shouldBe` True,
                 isNothing (mkSmtpConfig (smtpConfigInput "127.0.0.1" 2525 "account.example.test" sampleSender (Just (smtpAuthentication (smtpLoginUsername "smtp-user") (smtpLoginPassword "smtp-password\NUL"))))) `shouldBe` True
               ]
        )

  describe "deliverSmtpEmail" $ do
    it "fails clearly when its resolver has no usable SMTP address" $ do
      let config = required "SMTP config" (mkSmtpConfig (smtpConfigInput "smtp.example.test" 2525 "account.example.test" sampleSender Nothing))
      result <- try (deliverSmtpEmailWithResolver (const (pure Nothing)) config sampleMessage) :: IO (Either IOException ())
      case result of
        Left failure -> displayException failure `shouldContain` "SMTP hostname did not resolve to a connectable address"
        Right () -> expectationFailure "expected an SMTP resolver failure"

    it "delivers a dot-stuffed UTF-8 transactional message to a loopback SMTP server" $
      withLoopbackSmtp
        acceptingServer
        ( \port -> do
            let config = required "loopback SMTP config" (mkSmtpConfig (smtpConfigInput "127.0.0.1" port "account.example.test" sampleSender Nothing))
            deliverSmtpEmail config sampleMessage
        )
        >>= \(_, deliveredMessage) ->
          deliveredMessage
            `shouldBe` TextEncoding.encodeUtf8 "From: <noreply@example.test>\r\nTo: <ada@example.test>\r\nSubject: Account verification\r\nMIME-Version: 1.0\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Transfer-Encoding: 7bit\r\n\r\n..first\r\nsecond\r\n..third\r\n.\r\n"

    it "uses base64 instead of unadvertised 8BITMIME for a Unicode message body" $
      withLoopbackSmtp
        acceptingServer
        ( \port -> do
            let unicodeMessage = required "Unicode message" (mkEmailMessage (EmailMessageInput sampleRecipient "Málaga" "café"))
                config = required "loopback SMTP config" (mkSmtpConfig (smtpConfigInput "127.0.0.1" port "account.example.test" sampleSender Nothing))
            deliverSmtpEmail config unicodeMessage
        )
        >>= \(_, deliveredMessage) ->
          deliveredMessage
            `shouldBe` TextEncoding.encodeUtf8 "From: <noreply@example.test>\r\nTo: <ada@example.test>\r\nSubject: =?UTF-8?B?TcOhbGFnYQ==?=\r\nMIME-Version: 1.0\r\nContent-Type: text/plain; charset=utf-8\r\nContent-Transfer-Encoding: base64\r\n\r\nY2Fmw6k=\r\n.\r\n"

    it "stops when the SMTP server rejects a command" $ do
      _ <- withLoopbackSmtp rejectingEhloServer $ \port -> do
        let config = required "rejecting SMTP config" (mkSmtpConfig (smtpConfigInput "127.0.0.1" port "account.example.test" sampleSender Nothing))
        result <- try (deliverSmtpEmail config sampleMessage) :: IO (Either IOException ())
        case result of
          Left failure -> do
            expectAll
              ( (displayException failure `shouldContain` "Unexpected SMTP response:")
                  :| [length (displayException failure) `shouldSatisfy` (> 0)]
              )
          Right () -> expectationFailure "expected an SMTP command rejection"
      pure ()

    it "rejects malformed, unfinished, and inconsistent SMTP replies" $ do
      let assertRejected server expectedMessage = do
            _ <- withLoopbackSmtp server $ \port -> do
              let loopbackConfig = required "loopback SMTP config" (mkSmtpConfig (smtpConfigInput "127.0.0.1" port "account.example.test" sampleSender Nothing))
              result <- try (deliverSmtpEmail loopbackConfig sampleMessage) :: IO (Either IOException ())
              case result of
                Left failure -> displayException failure `shouldContain` expectedMessage
                Right () -> expectationFailure "expected malformed SMTP reply to be rejected"
            pure ()
      assertRejected malformedGreetingServer "Malformed SMTP response"
      assertRejected malformedGreetingServer "not an SMTP response"
      assertRejected unfinishedReplyServer "closed the connection"
      assertRejected inconsistentMultilineReplyServer "changed status code"
      assertRejected invalidMultilineSeparatorServer "invalid continuation separator"
      assertRejected oversizedGreetingServer "exceeds 16384 bytes"

    it "consumes every line of a multiline SMTP capability reply" $
      withLoopbackSmtp
        acceptingThreeLineCapabilityServer
        ( \port -> do
            let config = required "loopback SMTP config" (mkSmtpConfig (smtpConfigInput "127.0.0.1" port "account.example.test" sampleSender Nothing))
            deliverSmtpEmail config sampleMessage
        )
        >>= \(_, deliveredMessage) ->
          deliveredMessage `shouldSatisfy` ByteString.isInfixOf "Subject: Account verification"

    it "uses SMTP AUTH PLAIN when credentials are configured" $
      withLoopbackSmtp
        acceptingAuthenticatedServer
        ( \port -> do
            let config = required "authenticated SMTP config" (mkSmtpConfig (smtpConfigInput "127.0.0.1" port "account.example.test" sampleSender (Just (smtpAuthenticationForLocalDevelopment (smtpLoginUsername "smtp-user") (smtpLoginPassword "smtp-password")))))
            deliverSmtpEmail config sampleMessage
        )
        >>= \(_, deliveredMessage) ->
          deliveredMessage `shouldSatisfy` ByteString.isInfixOf "Subject: Account verification"

    it "recognizes AUTH PLAIN when the capability has advertised parameters" $
      withLoopbackSmtp
        acceptingAuthenticatedServerWithParameters
        ( \port -> do
            let config = required "authenticated SMTP config" (mkSmtpConfig (smtpConfigInput "127.0.0.1" port "account.example.test" sampleSender (Just (smtpAuthenticationForLocalDevelopment (smtpLoginUsername "smtp-user") (smtpLoginPassword "smtp-password")))))
            deliverSmtpEmail config sampleMessage
        )
        >>= \(_, deliveredMessage) ->
          deliveredMessage `shouldSatisfy` ByteString.isInfixOf "Subject: Account verification"

    it "refuses ordinary credentials when the server does not advertise STARTTLS" $ do
      _ <- withLoopbackSmtp startTlsUnavailableServer $ \port -> do
        let config = required "authenticated SMTP config" (mkSmtpConfig (smtpConfigInput "127.0.0.1" port "account.example.test" sampleSender (Just (smtpAuthentication (smtpLoginUsername "smtp-user") (smtpLoginPassword "smtp-password")))))
        result <- try (deliverSmtpEmail config sampleMessage) :: IO (Either IOException ())
        case result of
          Left failure -> displayException failure `shouldContain` "STARTTLS"
          Right () -> expectationFailure "expected credentials without STARTTLS to be rejected"
      pure ()

    it "rejects plaintext bytes buffered after STARTTLS is accepted" $ do
      _ <- withLoopbackSmtp startTlsWithTrailingPlaintextServer $ \port -> do
        let config = required "authenticated SMTP config" (mkSmtpConfig (smtpConfigInput "127.0.0.1" port "account.example.test" sampleSender (Just (smtpAuthentication (smtpLoginUsername "smtp-user") (smtpLoginPassword "smtp-password")))))
        result <- try (deliverSmtpEmail config sampleMessage) :: IO (Either IOException ())
        case result of
          Left failure -> displayException failure `shouldContain` "plaintext bytes"
          Right () -> expectationFailure "expected trailing plaintext after STARTTLS to be rejected"
      pure ()

    it "upgrades to a hostname-validated TLS session before sending AUTH PLAIN" $
      withSystemTrustedLoopbackCertificate $ \certificatePath privateKeyPath ->
        withLoopbackSmtp
          (startTlsAuthenticatedServer certificatePath privateKeyPath)
          ( \port -> do
              let config = required "STARTTLS SMTP config" (mkSmtpConfig (smtpConfigInput "localhost" port "account.example.test" sampleSender (Just (smtpAuthentication (smtpLoginUsername "smtp-user") (smtpLoginPassword "smtp-password")))))
              deliverSmtpEmailWithResolver (loopbackSmtpResolver port) config sampleMessage
          )
          >>= \(_, deliveredMessage) ->
            deliveredMessage `shouldSatisfy` ByteString.isInfixOf "Subject: Account verification"

    it "uses a hostname-validated implicit TLS session before reading the SMTP greeting" $
      withSystemTrustedLoopbackCertificate $ \certificatePath privateKeyPath ->
        withLoopbackSmtp
          (implicitTlsAuthenticatedServer certificatePath privateKeyPath)
          ( \port -> do
              let config = required "implicit TLS SMTP config" (mkSmtpConfig (smtpConfigInput "localhost" port "account.example.test" sampleSender (Just (smtpAuthenticationOverImplicitTls (smtpLoginUsername "smtp-user") (smtpLoginPassword "smtp-password")))))
              deliverSmtpEmailWithResolver (loopbackSmtpResolver port) config sampleMessage
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
  expectCommand socket "EHLO account.example.test\r\n" "250-loopback ready\r\n250 PIPELINING\r\n"
  expectCommand socket "MAIL FROM:<noreply@example.test>\r\n" "250 sender accepted\r\n"
  expectCommand socket "RCPT TO:<ada@example.test>\r\n" "250 recipient accepted\r\n"
  expectCommand socket "DATA\r\n" "354 send message\r\n"
  message <- receiveUntil socket "\r\n.\r\n"
  sendResponse socket "250 message accepted\r\n"
  expectCommand socket "QUIT\r\n" "221 goodbye\r\n"
  pure message

acceptingThreeLineCapabilityServer :: Socket.Socket -> IO ByteString.ByteString
acceptingThreeLineCapabilityServer socket = do
  sendResponse socket "220 loopback ready\r\n"
  expectCommand socket "EHLO account.example.test\r\n" "250-loopback ready\r\n250-PIPELINING\r\n250 SIZE 100000\r\n"
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
  expectCommand socket "EHLO account.example.test\r\n" "250-loopback ready\r\n250 AUTH PLAIN\r\n"
  expectCommand socket "AUTH PLAIN AHNtdHAtdXNlcgBzbXRwLXBhc3N3b3Jk\r\n" "235 authenticated\r\n"
  expectCommand socket "MAIL FROM:<noreply@example.test>\r\n" "250 sender accepted\r\n"
  expectCommand socket "RCPT TO:<ada@example.test>\r\n" "250 recipient accepted\r\n"
  expectCommand socket "DATA\r\n" "354 send message\r\n"
  message <- receiveUntil socket "\r\n.\r\n"
  sendResponse socket "250 message accepted\r\n"
  expectCommand socket "QUIT\r\n" "221 goodbye\r\n"
  pure message

acceptingAuthenticatedServerWithParameters :: Socket.Socket -> IO ByteString.ByteString
acceptingAuthenticatedServerWithParameters socket = do
  sendResponse socket "220 loopback ready\r\n"
  expectCommand socket "EHLO account.example.test\r\n" "250 AUTH PLAIN LOGIN\r\n"
  expectCommand socket "AUTH PLAIN AHNtdHAtdXNlcgBzbXRwLXBhc3N3b3Jk\r\n" "235 authenticated\r\n"
  expectCommand socket "MAIL FROM:<noreply@example.test>\r\n" "250 sender accepted\r\n"
  expectCommand socket "RCPT TO:<ada@example.test>\r\n" "250 recipient accepted\r\n"
  expectCommand socket "DATA\r\n" "354 send message\r\n"
  message <- receiveUntil socket "\r\n.\r\n"
  sendResponse socket "250 message accepted\r\n"
  expectCommand socket "QUIT\r\n" "221 goodbye\r\n"
  pure message

startTlsUnavailableServer :: Socket.Socket -> IO ByteString.ByteString
startTlsUnavailableServer socket = do
  sendResponse socket "220 loopback ready\r\n"
  expectCommand socket "EHLO account.example.test\r\n" "250 AUTH PLAIN\r\n"
  pure ByteString.empty

startTlsWithTrailingPlaintextServer :: Socket.Socket -> IO ByteString.ByteString
startTlsWithTrailingPlaintextServer socket = do
  sendResponse socket "220 loopback ready\r\n"
  expectCommand socket "EHLO account.example.test\r\n" "250-STARTTLS\r\n250 AUTH PLAIN\r\n"
  expectCommand socket "STARTTLS\r\n" "220 begin TLS\r\nunexpected plaintext\r\n"
  pure ByteString.empty

data TlsTestConnection = TlsTestConnection
  { tlsTestContext :: TLS.Context,
    tlsTestBufferedBytes :: IORef ByteString.ByteString
  }

startTlsAuthenticatedServer :: FilePath -> FilePath -> Socket.Socket -> IO ByteString.ByteString
startTlsAuthenticatedServer certificatePath privateKeyPath socket = do
  sendResponse socket "220 loopback ready\r\n"
  expectCommand socket "EHLO account.example.test\r\n" "250-STARTTLS\r\n250 AUTH PLAIN\r\n"
  expectCommand socket "STARTTLS\r\n" "220 begin TLS\r\n"
  connection <- newTlsTestConnection certificatePath privateKeyPath socket
  serveAuthenticatedTls connection

implicitTlsAuthenticatedServer :: FilePath -> FilePath -> Socket.Socket -> IO ByteString.ByteString
implicitTlsAuthenticatedServer certificatePath privateKeyPath socket = do
  connection <- newTlsTestConnection certificatePath privateKeyPath socket
  writeTlsResponse connection "220 loopback ready\r\n"
  serveAuthenticatedTls connection

newTlsTestConnection :: FilePath -> FilePath -> Socket.Socket -> IO TlsTestConnection
newTlsTestConnection certificatePath privateKeyPath socket = do
  credentialResult <- TLS.credentialLoadX509 certificatePath privateKeyPath
  credential <- either (ioError . userError) pure credentialResult
  tlsContext <-
    TLS.contextNew
      (tlsTestBackend socket)
      ( (def :: TLS.ServerParams)
          { TLS.serverShared =
              (def :: TLS.Shared)
                { TLS.sharedCredentials = TLS.Credentials [credential]
                },
            TLS.serverSupported =
              (def :: TLS.Supported)
                { TLS.supportedCiphers = TLSCipher.ciphersuite_default
                }
          }
      )
  TLS.handshake tlsContext
  TlsTestConnection tlsContext <$> newIORef ByteString.empty

serveAuthenticatedTls :: TlsTestConnection -> IO ByteString.ByteString
serveAuthenticatedTls connection = do
  expectTlsCommand connection "EHLO account.example.test" "250 AUTH PLAIN\r\n"
  expectTlsCommand connection "AUTH PLAIN AHNtdHAtdXNlcgBzbXRwLXBhc3N3b3Jk" "235 authenticated\r\n"
  expectTlsCommand connection "MAIL FROM:<noreply@example.test>" "250 sender accepted\r\n"
  expectTlsCommand connection "RCPT TO:<ada@example.test>" "250 recipient accepted\r\n"
  expectTlsCommand connection "DATA" "354 send message\r\n"
  message <- receiveTlsMessage connection
  writeTlsResponse connection "250 message accepted\r\n"
  expectTlsCommand connection "QUIT" "221 goodbye\r\n"
  TLS.bye (tlsTestContext connection)
  pure message

tlsTestBackend :: Socket.Socket -> TLS.Backend
tlsTestBackend socket =
  TLS.Backend
    { TLS.backendFlush = pure (),
      TLS.backendClose = pure (),
      TLS.backendSend = SocketByteString.sendAll socket,
      TLS.backendRecv = SocketByteString.recv socket
    }

expectTlsCommand :: TlsTestConnection -> ByteString.ByteString -> ByteString.ByteString -> IO ()
expectTlsCommand connection expected response = do
  command <- readTlsLine connection
  command `shouldBeServer` expected
  writeTlsResponse connection response

writeTlsResponse :: TlsTestConnection -> ByteString.ByteString -> IO ()
writeTlsResponse connection response =
  TLS.sendData (tlsTestContext connection) (LazyByteString.fromStrict response)

readTlsLine :: TlsTestConnection -> IO ByteString.ByteString
readTlsLine connection = do
  buffered <- readIORef (tlsTestBufferedBytes connection)
  go buffered
  where
    go buffered =
      case ByteString.breakSubstring "\r\n" buffered of
        (line, remainder)
          | not (ByteString.null remainder) -> do
              writeIORef (tlsTestBufferedBytes connection) (ByteString.drop 2 remainder)
              pure line
          | otherwise -> do
              next <- TLS.recvData (tlsTestContext connection)
              if ByteString.null next
                then ioError (userError "SMTP client closed the TLS connection before completing a command")
                else go (buffered <> next)

receiveTlsMessage :: TlsTestConnection -> IO ByteString.ByteString
receiveTlsMessage connection = go []
  where
    go messageLines = do
      line <- readTlsLine connection
      if line == "."
        then pure (ByteString.intercalate "\r\n" (reverse messageLines))
        else go (line : messageLines)

withSystemTrustedLoopbackCertificate :: (FilePath -> FilePath -> IO value) -> IO value
withSystemTrustedLoopbackCertificate action =
  withSystemTempDirectory "smtp-tls-certificate" $ \tempDirectory -> do
    let certificatePath = tempDirectory </> "certificate.pem"
        privateKeyPath = tempDirectory </> "private-key.pem"
    writeFile certificatePath smtpTlsCertificatePem
    writeFile privateKeyPath smtpTlsPrivateKeyPem
    withSystemCertificatePath certificatePath (action certificatePath privateKeyPath)

withSystemCertificatePath :: FilePath -> IO value -> IO value
withSystemCertificatePath certificatePath action =
  bracket
    (lookupEnv "SYSTEM_CERTIFICATE_PATH")
    restoreSystemCertificatePath
    (\_ -> setEnv "SYSTEM_CERTIFICATE_PATH" certificatePath >> action)

restoreSystemCertificatePath :: Maybe String -> IO ()
restoreSystemCertificatePath =
  maybe (unsetEnv "SYSTEM_CERTIFICATE_PATH") (setEnv "SYSTEM_CERTIFICATE_PATH")

loopbackSmtpResolver :: Word16 -> SmtpAddressResolver
loopbackSmtpResolver port _ =
  listToMaybe
    <$> Socket.getAddrInfo
      (Just Socket.defaultHints {Socket.addrSocketType = Socket.Stream})
      (Just "127.0.0.1")
      (Just (show port))

rejectingEhloServer :: Socket.Socket -> IO ByteString.ByteString
rejectingEhloServer socket = do
  sendResponse socket "220 loopback ready\r\n"
  command <- receiveChunk socket
  command `shouldBeServer` "EHLO account.example.test\r\n"
  sendResponse socket "500 rejected\r\n"
  pure ByteString.empty

malformedGreetingServer :: Socket.Socket -> IO ByteString.ByteString
malformedGreetingServer socket = do
  sendResponse socket "not an SMTP response\r\n"
  pure ByteString.empty

unfinishedReplyServer :: Socket.Socket -> IO ByteString.ByteString
unfinishedReplyServer socket = do
  sendResponse socket "220-loopback keeps talking\r\n"
  pure ByteString.empty

inconsistentMultilineReplyServer :: Socket.Socket -> IO ByteString.ByteString
inconsistentMultilineReplyServer socket = do
  sendResponse socket "220 loopback ready\r\n"
  expectCommand socket "EHLO account.example.test\r\n" "250-first capability\r\n550 unexpected final code\r\n"
  pure ByteString.empty

invalidMultilineSeparatorServer :: Socket.Socket -> IO ByteString.ByteString
invalidMultilineSeparatorServer socket = do
  sendResponse socket "220?invalid continuation separator\r\n"
  pure ByteString.empty

oversizedGreetingServer :: Socket.Socket -> IO ByteString.ByteString
oversizedGreetingServer socket = do
  sendResponse socket ("220 " <> ByteString.replicate 16381 97)
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
